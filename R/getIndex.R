#' Return a dataframe of calculated indices for each given location
#' 
#' @param indexdf A dataframe of index information.
#' @param indexno A number signifying which index in the data frame to evaluate.
#' @return indexdata A dataframe of calculated indices by location
#' @export


getIndex <- function(indexdf, indexno){ 
  #Given a data frame of indices and a row number, calculate the specified index
  data <- getEventNormRefl()
  p1 <- data$normrefl[which(data$wavelength == indexdf$w1[indexno])]
  p2 <- data$normrefl[which(data$wavelength == indexdf$w2[indexno])]
  p3 <- data$normrefl[which(data$wavelength == indexdf$w3[indexno])]
  p4 <- data$normrefl[which(data$wavelength == indexdf$w4[indexno])]
  index <- eval(parse(text = indexdf$expression_form[indexno]))
  location <- unique(data$location)
  indexdata <- data.frame(location, index)
  return(indexdata)
}