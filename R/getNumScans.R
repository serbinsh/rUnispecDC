#' Get number of scans per file.
#' 
#' @param inputfiles A vector of UniSpec dataset filenames from user input.
#' @return scans An integer value indicating the number of scans done and aggregated per file.
#' @export

getNumScans <- function(inputfiles){
  line <- read.table(as.character(inputfiles[1,4]),skip = 8, nrow = 1,
                     fill = FALSE, header = FALSE, 
                     stringsAsFactors = FALSE, strip.white = TRUE)
  scans <- as.integer(sub(as.vector(line[1,1]), pattern = "Number Scans: ", replacement = ""))
  return(scans)
}