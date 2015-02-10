#ui.R
#For rHyperSpec_shinyapp

#install and load required libraries
require(shiny)
require(ggplot2)
require(reshape2)
require(scales)
require(grid) #not available for 3.0.2
require(MASS) #for RML method of smoothing
require(mgcv) #for GAM method of smoothing
require(RCurl) #check internet connection for googleVis
require(knitr)
require(devtools)
require(Rcpp)
require(roxygen2)

source("sources/rHyperSpec_shinyapp.R")


#Import required .csv files, which can be manipulated by the user
#list of spectral indices
indexlist <- read.csv("sources/Indices.csv", header = TRUE, strip.white = TRUE, 
                      stringsAsFactors = FALSE)
#list of people involved with project
peoplelist <- read.csv("sources/people.csv", header = FALSE, stringsAsFactors = FALSE)
people <- as.character(peoplelist[,1])
#list of sampling paths (tramlines, transects, etc.)
samplingpathlist <- read.csv("sources/samplingpath.csv", header = FALSE, stringsAsFactors = FALSE)
samplingpath <- as.character(samplingpathlist[,1])



shinyUI(navbarPage("rHyperSpec", id = "nav",
                   
                   tabPanel("Start",

                            includeMarkdown("README.md"),
                            
                            # checkboxInput("showIndexList", "Show Index List", FALSE),
                            # conditionalPanel(
                            #          condition = "input.showIndexList == 'TRUE'",
                            #          dataTableOutput('indexListTable')),
                            
                            fluidPage(
                              fluidRow(
                                column(3, 
                                    fileInput('calfiles','Select calibration files', 
                                              multiple = TRUE,
                                              accept = NULL)),
                                column(3, 
                                    fileInput('eventfiles','Select event files', 
                                              multiple = TRUE,
                                              accept = '.spu')),
                                column(3, 
                                    fileInput('location_info',
                                              'Select file with location information',
                                              multiple = FALSE,
                                              accept = '.csv')))),                            
                            dataTableOutput('locations')  
                            
                   ),
                   
                   tabPanel('Index List',
                            includeMarkdown("sources/AboutIndices.Rmd"),
                            dataTableOutput('indexListTable')),
                   
                   tabPanel('Enter Metadata',
                            
                            includeMarkdown("sources/EnteringMetadata.Rmd"),
                            
                            downloadButton('downloadMetadata', 'Download Metadata'),
                            br(),
                            br(),
                            br(),
                            
                            fluidPage(
                              fluidRow(
                                column(3, 
                                    selectInput("samplingpath", 
                                                label = "Sampling Path",
                                                choices = samplingpath)),
                                column(3, 
                                    selectInput('unispec',
                                                label = "Unispec serial #\n ",
                                                choices = c("2011","2012","2013"),
                                                selected = c("2012"))),
                                column(3, 
                                    selectInput('upchannel', 
                                                label = 'Upward fiber optic channel',
                                                choices = c('Channel A' = 'A', 
                                                            'Channel B' = 'B')))),
                              fluidRow(  
                                column(3, 
                                    selectInput('eventno', 
                                                label = 'Event number for the day',
                                                choices = c('1' = '1', '2' = '2', 
                                                            '3' = '3', '4' = '4',
                                                            '5' = '5', '6' = '6', 
                                                            '7' = '7', '8' = '8',
                                                            '9' = '9', '10' = '10'))),
                                column(3, 
                                    selectInput('skyclasscondition',
                                                label = "Sky condition by class",
                                                choices = c("0% (No clouds)", 
                                                            "0-10% (Clear)", 
                                                            "10-25% (Isolated)",
                                                            "25-50% (Scattered)", 
                                                            "50-90% (Broken)", 
                                                            "90-100% (Overcast)"))),
                                column(3, 
                                    selectInput('wind', 
                                                label = 'Wind',
                                                choices = c('negligable' = 'negligible', 
                                                            'breezy' = 'breezy',
                                                            'windy' = 'windy', 
                                                            'extremely windy' = 'extremely windy')))),
                            
                              fluidRow(  
                                column(3, 
                                    selectInput('eventperson1', 
                                                label = 'Data collector #1',
                                                choices = people)),
                                column(3, 
                                    selectInput('eventperson2', 
                                                label = 'Data collector #2',
                                                choices = people)),
                                column(3, 
                                    selectInput('analysisperson', 
                                                label = 'Data analyzer',
                                                choices = people))),
                            
                              fluidRow(  
                                column(5,
                                    textInput('tracknotes',
                                              label = "Track condition notes",
                                              value = ""),
                                    tags$head(tags$style(type="text/css", 
                                                         "#tracknotes {width: 350px}"))),
                                  column(5,
                                    textInput('instrumentnotes',
                                              label = "Instrumentation notes",
                                              value = ""),
                                    tags$head(tags$style(type="text/css", 
                                                         "#instrumentnotes {width: 350px}")))),
                            
                              fluidRow(  
                                column(5,
                                    textInput('weathernotes',
                                              label = "Weather notes",
                                              value = ""),
                                    tags$head(tags$style(type="text/css", 
                                                         "#weathernotes {width: 350px}"))),
                                column(5,
                                    textInput('datanotes',
                                              label = "Data file or collection notes",
                                              value = ""),
                                    tags$head(tags$style(type="text/css", 
                                                         "#datanotes {width: 350px}")))),
                            
                            HTML('<hr style="background:#F87431; border:0; height:5px" />'),
                            
                            fluidRow(  
                              column(3,
                                    radioButtons("direction", "Direction files given:",
                                                 c("Forward" = "forward", 
                                                   "Backward" = "backward"))),
                              column(3,
                                    radioButtons("interpolation", "Interpolation type:",
                                                 c("Linear" = "linear", 
                                                   "Spline" = "spline", 
                                                   "Cubic" = "cubic"))),
                              column(3,
                                    radioButtons("limitNR", "Normalized Reflectance Calculation Options:",
                                                 c("Change values > 1 to 1 and < -1 to -1" = "lim1", 
                                                   "Change values > 1 and < -1 to NA" = "limNA",
                                                   "No Limits" = "nl")))),
                            
                            HTML('<hr style="background:#F87431; border:0; height:5px" />'),
                            
                            dataTableOutput('metadata')
                   )),
                   
                   
                   tabPanel('Calibration Plots',
                            includeMarkdown("sources/AboutCalPlots.Rmd"),
                            downloadButton('downloadCalIrradiancePlot', "Download irradiance plot"),
                            downloadButton('downloadCalRadiancePlot', "Download radiance plot"),
                            downloadButton('downloadCalSingleIrrRadPlot', "Download single irradiance/radiance plot"),
                            downloadButton('downloadAvgCalRefl', "Download average panel reflectance plot"),
                            downloadButton('downloadAvgNormRefl', "Download average normalized reflectance plot"),
                            br(),
                            hr(),
                            tags$head(tags$style(type="text/css", ".jslider {max-width: 500px; }")),
                            uiOutput('calWaveSlider'),
                            plotOutput('rawcalirrplots', height = '180px'),
                            hr(),
                            plotOutput('rawcalradplots', height = '180px'),
                            hr(),
                            uiOutput('calFileSlider'),
                            plotOutput('rawcalplot', height = '230px'),
                            hr(),
                            plotOutput('avgcalreflplot', height = '180px'),
                            hr(),
                            plotOutput('avgnormreflplot', height = '180px')),
                   
                   tabPanel('Event Plots',
                            includeMarkdown("sources/AboutEventPlots.Rmd"),
                            downloadButton('downloadEventData', 'Download Event Data'),
                            downloadButton('downloadEventIrradiancePlot', "Download irradiance plot"),
                            downloadButton('downloadEventRadiancePlot', "Download radiance plot"),
                            downloadButton('downloadEventSingleIrrRadPlot', "Download single irradiance/radiance plot"),
                            downloadButton('downloadReflHeatMap', "Download reflectance heat map"),
                            br(),
                            hr(),
                            tags$head(tags$style(type="text/css", ".jslider {max-width: 500px; }")),
                            uiOutput('eventWaveSlider'),
                            plotOutput('raweventirrplots', height = "180px"),
                            hr(),
                            plotOutput('raweventradplots', height = "180px"),
                            hr(),
                            uiOutput('eventFileSlider'),
                            plotOutput('raweventplot', height = '230px'),
                            hr(),
                            plotOutput('reflheatmap', height = '230px')),
                   
           
                   tabPanel('Table of Calculated Indices',
                            downloadButton('downloadIndexData', 'Download Index Data'),
                            hr(),
                            dataTableOutput('allIndexTable')), 
                   
                   tabPanel('Table of Average Index Values',
                            downloadButton('downloadIndexSummaries', 'Download Index Summary Data'),
                            hr(),
                            dataTableOutput('summaryIndexTable')), 
                   
                   tabPanel('Index by Location Plots',
                            fluidRow(
                                    selectInput(inputId = 'indexnames', label = 'Select indices of interest:', 
                                                choices = indexlist[,2], selected = 'ndvi1',
                                                multiple = TRUE),
                            selectInput('smoothtype', 'Smoothing Method:',
                                        choices = c('loess','lm','glm','rlm','gam'), 
                                        selected = 'loess')),
                            downloadButton('downloadIndexByLocationPlot', 'Download plot'),
                            plotOutput('indexbylocationplot', height = "400px")),

                   tabPanel('Index Comparison Plots',
                            selectInput('xindex', 'X-axis Index:',
                                        indexlist[,2], selected = 'green1'),
                            selectInput('yindex', 'Y-axis Index:',
                                        indexlist[,2], 'ndvi1'),
                            downloadButton('downloadIndexCompPlot', 'Download plot'),
                            plotOutput('indexcompplot'))           
))