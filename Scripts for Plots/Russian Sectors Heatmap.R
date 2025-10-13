lapply(c("moexer", "timeSeries", "xts"), require, character.only = T) # Libs

sector.correlation.rus <- function(data=T, s=NULL, e=NULL, lg=T, size=2){
  
  x <- c(
    "MOEXTL", "MOEXCN", "MOEXOG", "MOEXFN", "MOEXTN", "MOEXIT", "MOEXCH",
    "MOEXEU", "MOEXMM", "MOEXRE"
  )
  
  y <- c(
    "Communications", "Consumer", "Energy", "Financials",  "Industrials",
    "IT", "Chemicals", "Utilities", "Metals & Mining", "Real Estate"
  )
  
  if (data){ R <- NULL # data off
    
    getData2 <- function(A, s, e) { 
      if (is.null(s) && is.null(e))
        return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
      if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
      if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
      return(get_candles(A, from = s, till = e, interval = 'daily')) 
    }
    for (A in x){ D <- as.data.frame(getData2(A, s, e)[,c(3,8)])
    
    D <- D[!duplicated(D),] # Remove duplicates
    
    R <- cbind(R, xts(D[, 1], order.by = as.Date(D[, 2]))) } }
  
  R <- R[apply(R, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(R) <- y
    
  if (lg | data) { R <- diff(log(as.timeSeries(R)))[-1,] }
  
  m.correlation = as.matrix(R) # Convert data into matrix
  
  c.correlation = ncol(m.correlation) # Get number of columns
  
  new_cor <- cor(m.correlation) # Calculate correlation coefficients
  
  # Create appropriate colour for each pair of correlation for heatmap
  k.c <- round((10 * length(unique(as.vector(new_cor))))/2)
  corrColorMatrix <- rgb(c(rep(0, k.c), seq(0, 1, length = k.c)),
                         c(rev(seq(0,1,length=k.c)), rep(0,k.c)), rep(0,2*k.c))
  # Display heatmap
  image(x = 1:c.correlation,y = 1:c.correlation,z = new_cor[, c.correlation:1],
        col = corrColorMatrix, axes = FALSE, main = "", xlab = "", ylab = "")
  
  # Add labels for both axis
  axis(2, at = c.correlation:1, labels = colnames(m.correlation), las = 2)
  axis(1, at = 1:c.correlation, labels = colnames(m.correlation), las = 2)
  
  title(main = "Heatmap of Russian Sector Correlations") # title
  
  box() # Box heatmap
  
  # Add correlation values as text strings to each heatmap cell
  x = y = 1:c.correlation
  n_x = n_y = length(y)
  xoy = cbind(rep(x, n_y), as.vector(matrix(y, n_x, n_y, byrow = TRUE)))
  corr.coord = matrix(xoy, n_x * n_y, 2, byrow = FALSE)
  X.corr = t(new_cor)
  for (i in 1:c.correlation ^ 2) {
    text(corr.coord[i, 1], corr.coord[c.correlation ^ 2 + 1 - i, 2],
         round(X.corr[corr.coord[i,1],corr.coord[i,2]],digits=2),col = "white",
         cex=size) }
  
  par(mar = rep(10, 4)) # Define borders of the plot
}
sector.correlation.rus(T)
