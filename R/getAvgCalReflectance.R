#' Calculate the average calibration panel reflectance.
#' 
#' @param caldata A dataframe of the calibration data from all panel scans.
#' @return avgrefldata A dataframe with three columns: wavelength in nanometers, 
#' average reflectance, and standard deviation.
#' @export

getAvgCalReflectance <- function(caldata) {
  paneldata <- data.frame(wavelength = caldata[[1]]$wavelength)
  
  #For each of the white panel files, read in the file, calculate reflectance, and 
  #add a column to the results file
  for(i in 1:length(caldata)){
    pfile <- caldata[[i]]
    paneldata[,i+1] <- pfile$radiance/pfile$irradiance
  }
  
  #Calculate mean reflectance and the standard deviation of the reflectance at every 
  #wavelength measured in the raw data files
  if(length(caldata) == 1){
    paneldata$avg <- paneldata[,2]
    paneldata$sd <- rep(NA, nrow(paneldata))
  } else {
    paneldata$avg <- rowMeans(paneldata[,2:ncol(paneldata)])
    paneldata$avg[which(paneldata$avg > 1000)] <- NA
    paneldata$sd <- apply(paneldata[2:(ncol(paneldata)-1)],1,sd,na.rm=TRUE)
  }
  
  avgrefldata <- data.frame(paneldata$wavelength, paneldata$avg, paneldata$sd)
  names(avgrefldata) <-c("wavelength","avg","sd")
  return(avgrefldata)
}