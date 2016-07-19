# function removing rows of a dataframe, in
# which a variable contains outliers.
# The data can be split prior to removing
# outliers by any variable or a number of
# variables

# ARGUMENTS:
# data: data frame
# var: variable from which outliers
# splitvars: variables to split the data, NULL when no split
# n: number of observations to remove per split

# RETURN: dataframe with rows removed according
# to whether they are considered outliers or not

removeOutly <- function(data, var="N", splitvars=NULL, n=5){
  if(is.null(splitvars)){
    for (i in 1:n){
      x <- as.list(data)[[var]]
      loc <- which.max(x)
      data <- data[-loc,]
    }
  } else {
    for (i in 1:n){
      rows <- as.integer(row.names(data))
      x <- as.list(data)[[var]]
      splits <- data[, splitvars]
      split1 <- split(x, splits)
      split2 <- split(rows, splits)
      maxim <- sapply(split1, which.max)
      good <- sapply(maxim, function(x) !length(x) == 0)
      maxim <- maxim[good]
      split2 <- split2[good]
      loc <- sapply(1:length(maxim), function(x) split2[[x]][maxim[[x]]])
      data <- data[-loc,]
    }
  }
  data
}
