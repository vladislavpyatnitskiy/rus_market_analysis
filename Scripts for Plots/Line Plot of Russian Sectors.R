lapply(c("moexer", "xts", "timeSeries"), require, character.only = T) # Libs

line.plt.sector.rus <- function(data=T, s=NULL, e=NULL, start=F){
  
  x <- c(
    "MOEXTL", "MOEXOG", "MOEXFN", "MOEXEU", "MOEXTN", "MOEXIT", "MOEXCH",
    "MOEXMM", "MOEXRE", "MOEXCN"
  )
  
  y <- c(
    "Communications", "Energy", "Financials", "Utilities",  "Industrials",
    "IT", "Chemicals", "Metals & Mining", "Real Estate", "Consumer"
  )
  
  if (start){ x <- x[1:4]
    
    y <- y[1:4] }
  
  if (data){ R <- NULL # data off
    
    getData2 <- function(A, s, e) { 
      if (is.null(s) && is.null(e))
        return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
      if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
      if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
      return(get_candles(A, from = s, till = e, interval = 'daily')) 
    }
    for (A in x){ D <- as.data.frame(getData2(A, s, e)[,c(3,8)])
      
      message(
        sprintf(
          "%s is downloaded (%s / %s)", 
          A, which(x == A), length(x)
        )
      ) # Download message
      
      D <- D[!duplicated(D),] # Remove duplicates
      
      R <- cbind(R, xts(D[, 1], order.by = as.Date(D[, 2]))) } }
      
  R <- R[apply(R, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(R) <- y
  
  DF <- apply(diff(log(as.timeSeries(R)))[-1,], 2,
              function(col) exp(cumsum(col)) - 1) * 100
  
  par(mar = c(8, 2.5, 4, 2.5)) # Define borders of the plot
  
  C = c("pink","#60bf37","#953ada","yellow","blue","#a7b43d","black",
        "green","turquoise","red")
  
  plot(
    DF[,1],
    ylim = c(min(DF), max(DF)),
    lty = 1,
    type = "l",
    lwd = 2,
    las = 1,
    col = C[1],
    xlab = "Trading Days",
    ylab = "Returns (%)",
    main = "Performance of Russian Sectors"
  )
  
  axis(side = 4, las = 2) # Right Y-Axis Values
  
  grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
  
  abline(h = 0) # Add black horizontal line at break even point
  
  for (n in 2:(ncol(DF))){ lines(DF[,n], col = C[n], lwd = 2) } # Plot indices
  
  legend(
    x = "bottom",
    inset = c(0, -.2),
    legend = colnames(DF),
    xpd = T,
    col = C,
    lwd = 3,
    cex = .75,
    bty = "n",
    horiz = F,
    ncol = 6
  )
  
  on.exit(par(par(no.readonly = T))) # Show legend with names
}
line.plt.sector.rus(T)
