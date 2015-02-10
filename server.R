#server.R
#For rHyperSpec_shinyapp

shinyServer(function(input, output) {
  
  #### Reactive Functions ####
  getCalTimestamp <- reactive({
    getTimestamp(input$calfiles)
  })
  
  getSoftwareVersion <- reactive({
    getSoftVer(input$calfiles)
  })
  
  getIntegrationTime <- reactive({
    getIntTime(input$calfiles)
  })
  
  getNumberScans <- reactive({
    getNumScans(input$calfiles)
  })
  
  getEventTimestamp <- reactive({
    getTimestamp(input$eventfiles)
  })
  
  getDate <- reactive({
    date <- substr(as.character(getCalTimestamp()), 1, 10)
    return(date)
  })
  
  getLimitNR <- reactive({
    limNR <- input$limitNR
    return(limNR)
  })

  
  getNumberCalFiles <- reactive({
    #Count the number of calibration files entered.
    nrow(input$calfiles)
  })
  
  getNumberEventFiles <- reactive({
    #Count the number of event files entered.
    nrow(input$eventfiles)
  })
  
  getCalDataList <- reactive({
    #For each calibration file entered, use the getDataFromSPU function to extract
    #the data, then put each data frame into a list. The result is a list of data
    #frames, each with three columns: wavelength, radiance, and irradiance
    calfiles <- input$calfiles 
    files <- lapply(X = calfiles[,4], getDataFromSPU, input$upchannel) 
    return(files)
  })
  
  getCalDataFrame <- reactive({
    #For each calibration file entered, use the getCalDataList function to get a list
    #of all the calibration data frames. Then bind them into a single data frame
    #with four columns: wavelength, radiance, irradiance, and filename.
    files <- getCalDataList()
    data <- do.call(rbind, files)
    data$filename <- as.factor(rep(input$calfiles[,1], each=256))
    return(data)
  })
  
  getEventDataList <- reactive({
    #For each event file entered, use the getDataFromSPU function to extract
    #the data, then put each data frame into a list. The result is a list of data
    #frames, each with three columns: wavelength, radiance, and irradiance
    eventfiles <- input$eventfiles 
    files <- lapply(X = eventfiles[,4], getDataFromSPU, input$upchannel) 
    for(i in 1:length(files)){files[[i]]$filename <- eventfiles[i,1]}
    if(input$direction == "backward"){files <- rev(files)} 
    return(files)
  })
  
  getEventDataFrame <- reactive({
    #For each event file entered, use the getEventDataList function to get a list
    #of all the event data frames. Then bind them into a single data frame
    #with four columns: wavelength, radiance, irradiance, and filename.
    files <- getEventDataList()
    data <- do.call(rbind, files)
    data$filename <- as.factor(rep(input$eventfiles[,1], each=256))
    return(data)
  })
  
  getAvgCalRefl <- reactive({
    #getAvgCalRefl: calculate the average reflectance for all of the white panel 
    #files, and return a data frame with three columns: wavelength, avg, sd
    caldata <- getCalDataList()
    avgrefldata <- getAvgCalReflectance(caldata)
    return(avgrefldata)
  })
  
  getEventNormRefl <- reactive({
    #getEventNormRefl: for a set of event files, calculate the normalized reflectance
    #for each file, using the average reflectance of the white panels. Return a 
    #data frame with three columns: location, wavelength, and normrefl (normal reflectance).
    calrefl <- getAvgCalRefl()
    eventdata <- getEventDataList()
    limitNR <- getLimitNR()
    interpdata <- lapply(eventdata, getInterpNormRefl, calrefl, input$interpolation, limitNR)
    normrefl <- do.call(rbind, interpdata)
    normrefl$location <- rep(1:length(eventdata),each = 845)
    normrefl <- normrefl[,c("location","wavelength","normrefl")]
    return(normrefl)
  })
  
  getEventNormReflWIrrRad <- reactive({
    #getEventNormRefl: for a set of event files, calculate the normalized reflectance
    #for each file, using the average reflectance of the white panels. Return a 
    #data frame with five columns: location, wavelength, and normrefl (normal reflectance).
    calrefl <- getAvgCalRefl()
    eventdata <- getEventDataList()
    limitNR <- getLimitNR()
    interpdata <- lapply(eventdata, getInterpNormRefl, calrefl, input$interpolation, limitNR)
    normrefl <- do.call(rbind, interpdata)
    normrefl$location <- rep(1:length(eventdata),each = 845)
    normrefl <- normrefl[,c("location","wavelength","irradiance","radiance","calrefl",
                            "refl","normrefl")]
    return(normrefl)
  })
  
  getEventAvgNormRefl <- reactive({
    #Take the data frame with all normalized reflectance values for all events (using
    #getEventNormRefl()), calculate a mean normal reflectance for each wavelength, 
    #and return a data frame with two columns: avgnormrefl and wavelength.
    data <- getEventNormRefl()
    newdata <- data.frame(tapply(X = data$normrefl, INDEX = list(data$wavelength), 
                                 mean, na.rm = TRUE))
    names(newdata) <- c("avgnormrefl")
    newdata$wavelength <- as.numeric(row.names(newdata))
    return(newdata)
  })
  
  getIndices <- reactive({
    #Calculate all indices in the index list and bind into a single data frame
    indexdata <- merge(getIndex(indexlist, 1), getIndex(indexlist, 2), by = "location") 
    indexdata$date <- rep(getDate(), nrow(indexdata))
    indexdata$event <- rep(input$eventno, nrow(indexdata))
    indexdata <- indexdata[,c(1,4,5,2,3)]
    for(i in 3:nrow(indexlist)){
      indexdata <- merge(indexdata, getIndex(indexlist,i), by = "location")
    }
    names(indexdata) <- c("location","date","event_no",indexlist[,2])
    for(i in 4:ncol(indexdata)){
      indexdata[,i] <- round(as.numeric(as.character(indexdata[,i])),5)
    }
    return(indexdata)
  })
  
  summarizeIndices <- reactive({
    #Calculate the minimum, mean, and maximum values of all of the indices
    indices <- getIndices()
    mins <- as.data.frame(apply(X = indices[,4:ncol(indices)], MARGIN = 2, FUN = min, na.rm = TRUE))
    means <- as.data.frame(apply(X = indices[,4:ncol(indices)], MARGIN = 2, FUN = mean, na.rm = TRUE))
    maxs <- as.data.frame(apply(X = indices[,4:ncol(indices)], MARGIN = 2, FUN = max, na.rm = TRUE))
    sds <- as.data.frame(apply(X = indices[,4:ncol(indices)], MARGIN = 2, FUN = sd, na.rm = TRUE))
    results <- cbind(mins, means, maxs, sds)
    names(results) <- c("minimum","mean","maximum","standard_deviation")
    results.2 <- data.frame(row.names(results), results)
    names(results.2)[1] <- "index"
    return(results.2)
  })
  
  projectMetadata <- reactive({
    df <- data.frame(variable = c(
      "Sampling path",
      "Unispec serial number",
      "Software and version",
      "Upward facing fiber optic channel", 
      "Integration time (ms)",
      "Number of files per integration",
      "Event number for the day",
      "Person who collected data (1)",
      "Person who collected data (2)",
      "Person who analyzed data",
      "Sky condition by class",
      "Wind",
      "Date data analyzed",
      "First timestamp for calibration files",
      "First timestamp for event files",
      "Calibration file names",
      "Event file names",
      "Number of calibration files",
      "Number of event files",
      "Direction of event along transect",
      "Wavelength interpolation method",
      "Track condition notes",
      "Instrumentation notes",
      "Weather notes",
      "Data file or collection notes"))
    
    df$value <- c(
      input$samplingpath,
      input$unispec,
      getSoftwareVersion(),
      input$upchannel,
      getIntegrationTime(),
      getNumberScans(),
      input$eventno,
      input$eventperson1,
      input$eventperson2,
      input$analysisperson,
      input$skyclasscondition,
      input$skycondition,
      input$wind,
      input$sunanglestart,
      input$sunangleend,
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
      format(getCalTimestamp(), "%Y-%m-%d %H:%M:%S"),
      format(getEventTimestamp(), "%Y-%m-%d %H:%M:%S"),
      paste(input$calfiles[,1], collapse = ", "),
      paste(input$eventfiles[,1], collapse = ", "),
      length(input$calfiles[,1]),
      length(input$eventfiles[,1]),
      input$direction, 
      input$interpolation,
      input$tracknotes,
      input$instrumentnotes,
      input$weathernotes,
      input$datanotes)
    
    return(df)
  })
  
  plotCalIrradiance <- reactive({
    #Plot all of the irradiance for a given set of panel files
    data <- getCalDataFrame()
    p <- ggplot(data, aes(x = wavelength, y = irradiance, colour = filename)) + 
      geom_line()+
      geom_point() +
      scale_x_continuous(limits = input$cal_waverange,
                         breaks = c(seq(400,1100,100))) +
      scale_y_continuous(limits=c(0, 70000)) +
      ylab("Irradiance") + 
      theme(legend.position="none",
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })
  
  plotCalRadiance <- reactive({
    #Plot all of the radiance for a given set of panel files
    data <- getCalDataFrame()
    p <- ggplot(data, aes(x = wavelength, y = radiance, colour = filename)) + 
      geom_line()+
      geom_point() +
      scale_x_continuous(limits = input$cal_waverange,
                         breaks = c(seq(400,1100,100))) +
      scale_y_continuous(limits=c(0, 70000)) +
      ylab("Radiance") + 
      theme(legend.position="none",
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })
  
  plotCalIrrRadSingle <- reactive({
    #Plot the irradiance and radiance for any given (using a slider) panel file.
    files <- getCalDataList()
    p <- ggplot(files[[input$calfileslider]], aes(x = wavelength)) + 
      geom_line(aes(y = irradiance, color = 'irradiance'))+
      geom_line(aes(y = radiance, color = 'radiance')) +
      geom_point(aes(y = irradiance, color = 'irradiance')) +
      geom_point(aes(y = radiance, color = 'radiance')) +
      scale_x_continuous(limits = input$cal_waverange,
                         breaks = c(seq(400,1100,100))) +
      scale_y_continuous(limits=c(0, 70000)) +
      ylab("") + 
      scale_colour_brewer(type = "qual", palette = 6, name = "") +
      theme(legend.position = "top",
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })
  
  plotEventIrradiance <- reactive({
    #Plot all of the irradiance for a given set of panel files
    data <- getEventDataFrame()
    p <- ggplot(data, aes(x = wavelength, y = irradiance, colour = filename)) + 
      geom_line()+
      geom_point() +
      scale_x_continuous(limits = input$event_waverange,
                         breaks = c(seq(400,1100,100))) +
      scale_y_continuous(limits=c(0, 30000)) +
      ylab("Irradiance") + 
      theme(legend.position="none",
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })
  
  plotEventRadiance <- reactive({
    #Plot all of the radiance for a given set of panel files
    data <- getEventDataFrame()
    p <- ggplot(data, aes(x = wavelength, y = radiance, colour = filename)) + 
      geom_line()+
      geom_point() +
      scale_x_continuous(limits = input$event_waverange,
                         breaks = c(seq(400,1100,100))) +
      scale_y_continuous(limits=c(0, 30000)) +
      ylab("Radiance") + 
      theme(legend.position="none",
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })
  
  plotEventIrrRadSingle <- reactive({
    #Plot the irradiance and radiance for any given (using a slider) panel file.
    files <- getEventDataList()
    p <- ggplot(files[[input$eventfileslider]], aes(x = wavelength)) + 
      geom_line(aes(y = irradiance, color = 'irradiance'))+
      geom_line(aes(y = radiance, color = 'radiance')) +
      geom_point(aes(y = irradiance, color = 'irradiance')) +
      geom_point(aes(y = radiance, color = 'radiance')) +
      scale_x_continuous(limits = input$event_waverange,
                         breaks = c(seq(400,1100,100))) +
      scale_y_continuous(limits=c(0, 30000)) +
      ylab("") + 
      scale_colour_brewer(type = "qual", palette = 6, name = "") +
      theme(legend.position = "top",
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })
  
  plotAvgCalRefl <- reactive({
    #Plot the average reflectance for the calibration panel
    calrefl <- getAvgCalRefl()
    p <- ggplot(calrefl, aes(x = wavelength, y = avg)) +
      geom_line(colour = 'darkblue') +
      geom_point(colour = 'darkblue') +
      scale_x_continuous(breaks = c(seq(400,1100,100))) +
      scale_y_continuous(limits=c(0, 4)) +
      ylab("Avg Panel Reflectance") +
      theme(legend.position = "top",
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })

  plotAvgNormRefl <- reactive({
    #Plot average normalized reflectance for the event files
    p <- ggplot(getEventAvgNormRefl(), aes(x = wavelength, y = avgnormrefl)) +
      geom_line(colour = 'darkblue') +
      geom_point(colour = 'darkblue') +
      geom_smooth() +                                 
      scale_x_continuous(breaks = c(seq(400,1100,100))) +
      scale_y_continuous(limits=c(0, 2)) +
      ylab("Avg. Normalized Reflectance") +
      theme(legend.position = "top",
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })
  
  plotReflHeatMap <- reactive({
    #Plot a color map of the normalized reflectance for any given event file 
    #(using the same slider)
    data <- getEventNormRefl()
    limitNR <- getLimitNR()
    if(limitNR == "nl"){lim <- 2}else{lim <- 1}
    p <- ggplot(data, aes(x = location, y = wavelength, fill = normrefl)) +
      geom_raster() + 
      scale_y_continuous(limits = c(400,1000)) +
      scale_fill_continuous(name="Normalized\nReflectance", limits = c(0,lim)) +
      theme(legend.key.height = unit(1, "cm"),
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })
  
  plotIndexByLocation <- reactive({
    data <- getIndices()
    data <- melt(data, id.vars = c("location","date","event_no"))
    data <- data[data$variable %in% input$indexnames,]
    p <- ggplot(data, aes(x = location, y = value, group = variable, colour = variable)) +
      geom_point() +
      geom_line(size = 0.5) +
      geom_smooth(method = input$smoothtype,   
                  formula = y ~ x, size = 1.5) +
      scale_x_continuous(breaks = c(seq(0,110,10))) +
      ylab("Index value") +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size = 14),
            legend.position = "top",
            legend.key.height = unit(1, "cm"),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })
  
  plotIndexComparison <- reactive({
    aes_mapping <- aes_string(x = input$xindex, y = input$yindex)
    data <- getIndices()
    p <- ggplot(data, mapping = aes_mapping) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x) +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size = 14),
            panel.grid.major.y = element_line(size = .5, colour = "gray80"),
            panel.grid.major.x = element_line(size = .5, colour = "gray80"),
            panel.background = element_rect(fill = "white"))
    p
  })
  
  #### Output simple timestamp and tables ####
  
  output$caltimestamp <- renderText({
    #output a timestamp taken from the first white panel calibration file uploaded.
    timestamp <- as.character(getCalTimestamp())
    print(timestamp)
  })
  
  output$eventtimestamp <- renderText({
    #output a timestamp taken from the first white panel calibration file uploaded.
    timestamp <- as.character(getEventTimestamp())
    timestamp
  })
  
  #### Metadata Table ####
  output$metadata <- renderDataTable({
    metadata <- projectMetadata()
    metadata
  }, options = list(bSortClasses = TRUE))
  
  #### Index List Table ####
  output$indexListTable <- renderDataTable(
    indexlist[,c(1,2,7,9)],
    options = list(orderClasses = TRUE, 
                   iDisplayLength = 10,
                   #columnDefs = list(list(width = c("15%","15%", "15%", "55%"))),
                   class="display compact",
                   pagingType = "full_numbers"))
  
  #### Locations Table ####
  output$locations <- renderDataTable({
    locations <- read.csv(input$location_info[1,4], header = TRUE, stringsAsFactors = FALSE)
    locations
  },
  options = list(bSortClasses = TRUE))
  
  #### All Calculated Indices Table ####  
  output$allIndexTable <- renderDataTable({
    indices <- getIndices()
    indices <- indices[,c(1,4:ncol(indices))]
    indices
  }, 
  options = list(bSortClasses = TRUE, sScrollX = "100%", 
                 bScrollCollapse = "true"))
  
  #### Summary Index Table ####
  output$summaryIndexTable <- renderDataTable({
    indices <- summarizeIndices()
    
    #   write.csv(indices, "tables/indices-calculated-summary.csv", 
    #              row.names = FALSE, append = FALSE)
    indices  
  }, options = list(bSortClasses = TRUE))
  
  #### Download Button content ####
  output$downloadMetadata <- downloadHandler(
    filename = function() {paste('metadata-',getDate(), '-event', input$eventno, '.csv', sep = '')},
    content = function(con) {write.csv(projectMetadata(), con, row.names = FALSE)}
  )
  
  output$downloadCalIrradiancePlot <- downloadHandler(
    filename = function() {paste('plot_cal-irradiance_',getDate(), '-event', 
                                 input$eventno, ".png", sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 2.5, units = "in", res = 300)
      print(plotCalIrradiance())   
      dev.off()
    })
  
  output$downloadCalRadiancePlot <- downloadHandler(
    filename = function() {paste('plot_cal-radiance_',getDate(), '-event', 
                                 input$eventno, ".png", sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 2.5, units = "in", res = 300)
      print(plotCalRadiance())   
      dev.off()
    })
  
  output$downloadCalSingleIrrRadPlot <- downloadHandler(
    filename = function() {paste('plot_cal-irr-rad_',getDate(), '-event', 
                                 input$eventno, '-file', input$calfileslider, ".png", 
                                 sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 2.5, units = "in", res = 300)
      print(plotCalIrrRadSingle())   
      dev.off()
    })
  
  output$downloadEventIrradiancePlot <- downloadHandler(
    filename = function() {paste('plot_event-irradiance_',getDate(), '-event', 
                                 input$eventno, ".png", sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 2.5, units = "in", res = 300)
      print(plotEventIrradiance())   
      dev.off()
    })
  
  output$downloadEventRadiancePlot <- downloadHandler(
    filename = function() {paste('plot_event-radiance_',getDate(), '-event', 
                                 input$eventno, ".png", sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 2.5, units = "in", res = 300)
      print(plotEventRadiance())   
      dev.off()
    })
  
  output$downloadEventSingleIrrRadPlot <- downloadHandler(
    filename = function() {paste('plot_event-irr-rad_',getDate(), '-event', 
                                 input$eventno, '-file', input$calfileslider, ".png", 
                                 sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 2.5, units = "in", res = 300)
      print(plotEventIrrRadSingle())   
      dev.off()
    })
  
  output$downloadAvgCalReflPlot <- downloadHandler(
    filename = function() {paste('plot_avg-cal-refl_',getDate(), '-event', 
                                 input$eventno, ".png", 
                                 sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 2.5, units = "in", res = 300)
      print(plotAvgCalRefl())   
      dev.off()
    })
  
  output$downloadAvgNormReflPlot <- downloadHandler(
    filename = function() {paste('plot_avg-norm-refl_',getDate(), '-event', 
                                 input$eventno, ".png", 
                                 sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 2.5, units = "in", res = 300)
      print(plotAvgNormRefl())   
      dev.off()
    })
  
  output$downloadReflHeatMap <- downloadHandler(
    filename = function() {paste('plot_refl-heatmap_',getDate(), '-event', 
                                 input$eventno, ".png", 
                                 sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 3, units = "in", res = 300)
      print(plotReflHeatMap())   
      dev.off()
    })
    
  output$downloadIndexByLocationPlot <- downloadHandler(
#    indexnames <- input$indexnames,
#    newname <- indexnames[1],
#    if(length(indexnames > 1)){
#      for(i in 2:length(indexnames)){
#        newname <- paste(newname, indexnames[i], sep = "-") 
#    }},
    filename = function() {paste('plot_index-by-location_', getDate(), '-event', 
                                 input$eventno, ".png", 
                                 sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 4, units = "in", res = 300)
      print(plotIndexByLocation())   
      dev.off()
    })

  output$downloadIndexCompPlot <- downloadHandler(
    filename = function() {paste('plot_index-comp-plot_',getDate(), '-event', 
                                 input$eventno, ".png", 
                                 sep = '')}, 
    content = function(file) {
      png(file, width = 6.5, height = 3, units = "in", res = 300)
      print(plotIndexComparison())   
      dev.off()
    })
  
  output$downloadEventData <- downloadHandler(
    filename = function() {paste('eventdata-', getDate(), '-event', input$eventno, '.csv', sep = '')},
    content = function(con) {write.csv(getEventNormReflWIrrRad(), con, row.names = FALSE)}
  )
  
  output$downloadIndexData <- downloadHandler(
    filename = function() {paste('indexdata-', getDate(), '-event', input$eventno, '.csv', sep = '')},
    content = function(con) {write.csv(getIndices(), con, row.names = FALSE)}
  )
  
  output$downloadIndexSummaries  <- downloadHandler(
    filename = function() {paste('indexdata-summary-', getDate(), '-event', input$eventno,'.csv', sep = '')},
    content = function(con) {write.csv(summarizeIndices(), con, row.names = FALSE)}
  )
  
  output$downloadPdfReport <- downloadHandler(filename = "rHyperSpec_pdf_report.pdf",
                                              content = function(file){
                                                # generate PDF
                                                knit2pdf("rHyperSpec_pdf_report.Rnw")
                                                
                                                # copy pdf to 'file'
                                                file.copy("rHyperSpec_pdf_report.pdf", file)
                                                
                                                # delete generated files
                                                file.remove("rHyperSpec_pdf_report.pdf", 
                                                            "rHyperSpec_pdf_report.tex",
                                                            "rHyperSpec_pdf_report.aux", 
                                                            "rHyperSpec_pdf_report.log")
                                              },
                                              contentType = "application/pdf"
  )
  
  #### Sliders ####
  output$eventFileSlider <- renderUI({
    #output a slider bar for the panel files, to be used with 'raweventplot'
    n <- nrow(as.data.frame(input$eventfiles))
    sliderInput('eventfileslider', "View irradiance and radiance by file", 
                min = 1,  max = n , value = 1, step = 1)   
  }) 
  
  output$eventWaveSlider <- renderUI({
    #output a slider bar to select the wavelength range to be used in plotting - 
    #allows a user to zoom in.
    sliderInput('event_waverange',label = 'Wavelength range for analysis', 
                min = 305, max = 1145, value = c(400,1000), step = 10)})

  #### Plots ####
  output$calFileSlider <- renderUI({
    #output a slider bar for the panel files, to be used with 'rawcalplot'
    n <- nrow(as.data.frame(input$calfiles))
    sliderInput('calfileslider', "View irradiance and radiance by file", 
                min = 1,  max = n , value = 1, step = 1)}) 
  
  output$calWaveSlider <- renderUI({
    #output a slider bar to select the wavelength range to be used in plotting - 
    #allows a user to zoom in.
    sliderInput('cal_waverange',label = 'Wavelength range for analysis', 
                min = 305, max = 1145, value = c(400,1000), step = 10)})
  
  output$rawcalirrplots <- renderPlot({
    print(plotCalIrradiance())
  })
  
  output$rawcalradplots <- renderPlot({
    print(plotCalRadiance())
  })
  
  output$rawcalplot <- renderPlot({
    print(plotCalIrrRadSingle())
  })
  
  output$raweventirrplots <- renderPlot({
    print(plotEventIrradiance())
  })
  
  output$raweventradplots <- renderPlot({
    print(plotEventRadiance())
  })
  
  output$raweventplot <- renderPlot({
    print(plotEventIrrRadSingle())
  })
  
  output$avgcalreflplot <- renderPlot({
    print(plotAvgCalRefl())
  })
  
  output$avgnormreflplot <- renderPlot({
    print(plotAvgNormRefl())
  })
  
  output$reflheatmap <- renderPlot({
    print(plotReflHeatMap())
  })
  
  output$indexbylocationplot <- renderPlot({
    print(plotIndexByLocation())
  })
  
  output$indexcompplot <- renderPlot({
    print(plotIndexComparison())
  })
  
})