#' Get software version info.
#' 
#' @param inputfiles A vector of UniSpec dataset filenames from user input.
#' @return software A string indicating the UniSpec software name and version.
#' @export

getSoftVer <- function(inputfiles){
  line <- read.table(as.character(inputfiles[1,4]),skip = 1, nrow = 1,
                     fill = FALSE, header = FALSE, 
                     stringsAsFactors = FALSE, strip.white = TRUE)
  software <- sub(as.vector(line[1,1]), pattern = "Remarks:    SW=", replacement = "")
  return(software) 
}
