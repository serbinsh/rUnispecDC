#' Get integration time from UniSpec
#' 
#' @param inputfiles A vector of UniSpec dataset filenames from user input.
#' @return inttime A string indicating the UniSpec software name and version.
#' @export

getIntTime <- function(inputfiles){
  line <- read.table(as.character(inputfiles[1,4]),skip = 7, nrow = 1,
                     fill = FALSE, header = FALSE, 
                     stringsAsFactors = FALSE, strip.white = TRUE)
  inttime <- as.integer(sub(as.vector(line[1,1]), pattern = "Integration:  ", 
                            replacement = ""))
  return(inttime)
}