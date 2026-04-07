lapply(c("quantmod", "timeSeries", "moexer"), require, character.only = T) # lib

bar.cor.rus.commodities <- function(
    x, s=NULL, e=NULL, main=NULL, method="spearman", lg=F){
  
  redom = list(
    c("AGRO", "RAGR"), c("CIAN", "CNRU"), c("HHRU", "HEAD"), c("FIVE", "X5"),
    c("FIXP", "FIXR"), c("YNDX", "YDEX"))
  
  from = "2007-01-01"
  
  J <- NULL
  R <- NULL
  
  for (n in 1:length(x)){
    
    if (any(sapply(redom, function(redom_item) x[n] %in% redom_item))){
      
      f <- which(sapply(redom, function(redom_item) x[n] %in% redom_item))
      
      for (k in 1:length(redom[[f]])){
        
        a = as.data.frame(
          get_candles(redom[[f]][k], from=from, interval='daily')[,c(3,8)]
        )
        
        if (k == 2){ 
          
          message(
            sprintf(
              "%s is downloaded; %s from %s", x[n], which(x == x[n]), length(x)
            )
          )
        }
        
        a <- a[!duplicated(a),] # Remove duplicates
        
        a <- xts(a[, 1], order.by = as.Date(a[, 2]))
        
        if (x[n] == "AGRO") a <- a / 7.01
        
        colnames(a) <- redom[[f]][2]
        
        if (is.null(R)) R <- data.frame(a) else R <- rbind.data.frame(R, a)
      }
    } else {
      
      a = as.data.frame(get_candles(x[n], from=from, interval='daily')[,c(3,8)])
      
      message(
        sprintf(
          "%s is downloaded; %s from %s", 
          x[n], which(x == x[n]), length(x)
        )
      )
      
      a <- a[!duplicated(a),] # Remove duplicates
      
      a <- xts(a[, 1], order.by = as.Date(a[, 2]))
      
      colnames(a) <- x[n]
      
      R <- data.frame(a) 
    }
    
    R <- as.timeSeries(R) # Make it time series
    
    if (x[n] == "BELU"){ j <- which(rownames(R) == "2024-08-15")
    
    R[c(1:j),] <- R[c(1:j),]/8 } # Adjustments for Novabev stock
    
    if (is.null(J)) J <- list(R) else J[[n]] <- R 
    R <- NULL  # Reset R for next iteration
  }
  
  message("Stocks data has been downloaded successfully")
  
  y <- c(paste(c("BZ", "HG", "GC", "SB", "CT", "KC", "CC", "HE", "ZS",
                 "ZR"), "=F", sep = ""), "RUB=X") # tickers 
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
  for (A in y){ p <- cbind(p, getSymbols(A, src="yahoo", auto.assign=F)[,4]) 
  
  message(
    sprintf(
      "%s is downloaded; %s from %s", 
      A, which(y == A), length(y)
    )
  )
  }
  
  message("Commodities data has been downloaded successfully")
  
  if (isTRUE(grepl("-", y))){ y <- gsub("-", "", y) }
  if (isTRUE(grepl("=", y))){ y <- gsub("=", "", y) }
  
  colnames(p) <- c(
    "Brent", "Copper", "Gold", "Sugar", "Cotton", "Coffee", "Cocoa", "Hogs", 
    "Soybeans", "Rice", "Dollar"
    )
  
  a <- as.timeSeries(p) # Make it time series and display
  
  cir <- function(s, e){
    
    if (as.Date(s, format = "%d.%m.%Y") < "2013-09-17") s = "17.09.2013"
    
    L <- sprintf(
      paste(
        "https://www.cbr.ru/eng/hd_base/KeyRate/",
        "?UniDbQuery.Posted=",
        "True&UniDbQuery.From=%s&UniDbQuery.To=%s",
        sep = ""),
      s, e)
    
    B <- read_html(L) %>% html_nodes('table') %>% html_nodes('tr') %>%
      html_nodes('td') %>% html_text() 
    
    v <- data.frame(
      B[seq(from = 1, to = length(B), by = 2)],
      B[seq(from = 2, to = length(B), by = 2)]
    )
    
    colnames(v) <- c("Date", "Interest Rate")
    
    v$Date <- as.Date(v$Date, format = "%d.%m.%Y")
    
    v <- v[order(v$Date, decreasing = F), ]
    
    dates <- v[,1]
    
    v <- as.data.frame(v[,-1])
    
    rownames(v) <- dates
    colnames(v) <- "Rate"
    
    for (n in 1:ncol(v)){ v[,n] <- as.numeric(v[,n]) }
    
    as.timeSeries(v)
  }
  
  cbr = cir("17.09.2013", as.Date(Sys.Date())) # Interest Rate Data
  
  message("Interest Rate data has been downloaded successfully")
  
  a <- as.timeSeries(cbind(a, cbr)) # Make it time series and display
  
  a <- a[apply(a, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  M <- NULL
  
  for (n in 1:length(J)){ p <- cbind(J[[n]], a)
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    if (lg) p <- diff(log(p))[-1,]
    
    l <- sort(cor(p, method=method)[1,])

    l <- l[l < 1]

    C = c(
      "#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
      "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
      "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
      "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
      "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
      "#895c8b","#bd5975"
    ) # Add colour range

    par(mar = rep(5, 4)) # Define borders of the plot

    title = ifelse(
      is.null(main) == T, 
      sprintf("Commodity Correlations for %s", x[n]), main
    )

    # Create bar plot
    B <- barplot(
      l,
      horiz = F,
      col = C,
      main = title,
      ylab = "Correlation Level",
      ylim = c(-1, 1),
      las = 2,
      xpd = F
    )
    # Y axis
    p.seq <- seq(-0.95, 0.95, .1)
    axis(side = 2, at = p.seq, las = 1, labels = p.seq)
    axis(side = 4, at = seq(-1, 1, .1), las = 1, labels = seq(-1, 1, .1))

    for (h in p.seq){ abline(h = h, col ="grey", lty = 3) } # Horizontal lines
    abline(v = B, col = "grey", lty = 3) # Vertical lines

    abline(h = 0)
    
    box() # Add box

    m <- NULL # Write advices about securities according to correlations
                 
    for (k in 1:length(j)){ # Messages indicating correlation levels for stocks

      o = j[[k]][1]
      t = j[[k]][2]

      if (!identical(names(which(l > o & l < t)), character(0))){

        m = c(m, paste(j[[k]][3], toString(names(which(l > o & l < t))))) } }

    if (is.null(M)) M <- list(m) else M[[n]] <- m } # Display

  names(M) <- x

  M
}
bar.cor.rus.commodities(
  c(
    "RNFT", "WTCMP", "BISVP", "RENI", "DIOD", "MAGN","MRKV", "LSRG", "CNRU", 
    "RAGR", "HIMCP", "AQUA"
  )
)
