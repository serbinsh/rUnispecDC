#' Get data from an spu file
#' 
#' @param filepath The filepath for the given files.
#' @param chan The channel (A or B) used for the upward facing fiber optic.
#' @return rawdata A data frame with three columns: wavelength, radiance, and irradiance.
#'  If the upward channel is not A (the default), then the program will reverse the radiance and
#'  irradiance column names.
#' @export
#' 
getDataFromSPU <- function(filepath, chan){
  data <- read.delim(filepath, fill = TRUE,
                     col.names = c("wavelength","radiance","irradiance"),
                     stringsAsFactors = FALSE, strip.white = TRUE)
  data <- data[-grep(pattern = '[[:alpha:]]', x = data[,1]),]
  data[,1] <- as.numeric(data[,1])
  if(chan == "B"){names(data) <- c("wavelength","irradiance","radiance")}
  return(data)
}