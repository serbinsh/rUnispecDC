#' Interpolate radiance and irradiance values for a single event file.
#' 
#' @param eventdata A data frame of a single event data file.
#' @param calreflavgs A data frame of the calibration panel reflectance averages.
#' @param interpolation One of three interpolation types: linear, spline, or cubic. 'linear' 
#' will call the 'linear' method of the approx() function in the stats package. 'spline' or 
#' 'cubic' will respectively return the 'natural' or 'fmm' (Forsythe, Malcolm and Moler) 
#' method for the spline() function from the stats package.
#' @return interpdata A data frame of interpolated normalized reflectance values for 
#'  consecutive whole number wavelength values from the default start value (303 nm) 
#'  to end value (1147 nm). The data frame has four columns:wavelength, irradiance, 
#'  radiance, and normrefl (normal reflectance).
#'  
#' @seealso \code{\link[stats]{approx}}, \code{\link[stats]{spline}}
#' @export

getInterpNormRefl <- function(eventdata, calreflavgs, interpolation, limitNR){
  #Interpolate the radiance and irradiance data for a single event file.
  #Then calculate normalized reflectance for the event file.

  data <- eventdata
  calrefl <- calreflavgs
  interpolation <- interpolation
  allwaves <- seq(303, 1147, by = 1)
  
  #Linear
  if(interpolation == "linear"){
    InterCalIrr <- approx(x = data$wavelength, y = data$irradiance,
                          xout = allwaves,  method = "linear")
    InterCalRad <- approx(x = data$wavelength, y = data$radiance,
                          xout = allwaves,  method = "linear")
    InterWhite <- approx(x = data$wavelength, y = calrefl$avg,
                         xout = allwaves,  method = "linear")
  }
  
  #Spline 
  if (interpolation == "spline"){
    InterCalIrr <- spline(x = data$wavelength, y = data$irradiance,
                          xout = allwaves, method = "natural")
    InterCalRad <- spline(x = data$wavelength, y = data$radiance,
                          xout = allwaves, method = "natural")
    InterWhite <- spline(x = data$wavelength, y = calrefl$avg,
                         xout = allwaves, method = "natural")  
  }

  if (interpolation == "cubic"){
    InterCalIrr <- spline(x = data$wavelength, y = data$irradiance,
                          xout = allwaves, method = "fmm")
    InterCalRad <- spline(x = data$wavelength, y = data$radiance,
                          xout = allwaves, method = "fmm")
    InterWhite <- spline(x = data$wavelength, y = calrefl$avg,
                         xout = allwaves, method = "fmm")  
  }
  
  interpdata <- data.frame(wavelength = InterCalIrr$x, irradiance = InterCalIrr$y,
                           radiance = InterCalRad$y, calrefl = InterWhite$y)
  
  #Step 2: Calculate reflectance data by dividing the radiance by the irradiance
  #at each wavelength
  interpdata$refl <- (interpdata$radiance/interpdata$irradiance)
  
  #Step 3: Calculate the final reflectance by comparing the irradiance signal to
  #the mean white panel signal (that is, normalize the data)
  interpdata$normrefl <- (interpdata$refl/interpdata$calrefl)
  
  #Step 4: Remove values outside of the possible range of -1 to 1
  if(limitNR == "nl"){}
  if(limitNR == "lim1"){
    interpdata$normrefl[which(interpdata$normrefl > 1)] <- 1
    interpdata$normrefl[which(interpdata$normrefl < -1)] <- -1
  }
  if(limitNR == "limNA"){
    interpdata$normrefl[which(interpdata$normrefl > 1)] <- NA
    interpdata$normrefl[which(interpdata$normrefl < -1)] <- NA
  }
  return(interpdata)
}