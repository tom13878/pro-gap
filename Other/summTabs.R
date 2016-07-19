summTab <- function(data, varNames, probs){
  mean <- sapply(data[, varNames], mean, na.rm=TRUE)
  sd <- sapply(data[, varNames], sd, na.rm=TRUE)
  f <- function(x) quantile(x, probs=probs, na.rm=TRUE)
  quantiles <- t(sapply(data[, varNames], f))
  out <- cbind(mean, sd, quantiles)
  as.data.frame(round(out, 2))
}

summTabSplit <- function(data, varNames, splitVar, splitVal, probs){
  M <- list()
  for (Val in splitVal) {
    bool <- data[, splitVar] == Val
    dat <- data[bool, ]
    M[[Val]] <- summTab(dat, varNames, probs=probs)
  }
  M
}


