#' Get timestamp from a spu file.
#' 
#' @param inputfiles A vector of UniSpec dataset filenames from user input.
#' @return A timestamp in POSIXct format.
#' @export

getTimestamp <- function(inputfiles){
  data <- read.table(as.character(inputfiles[1,4]),skip = 2, nrow = 1,
                     fill = FALSE, header = FALSE, 
                     stringsAsFactors = FALSE, strip.white = TRUE)
  timestamp <- sub(as.vector(data[1,1]), pattern = "Time:       ", replacement = "")
  timestamp <- as.POSIXct(timestamp, format = "%m/%d/%Y %I:%M:%S %p")
  return(timestamp)
}