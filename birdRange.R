## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "birdRange",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(birdRange = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "birdRange.Rmd"), ## same file
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>= 1.1.1)", "ggplot2", "raster", "rgdal", "sf", "data.table", "terra",
                  "googledrive"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".doSelectInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first selection event should occur."),
    defineParameter("selectInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between select events."),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    defineParameter("rasterToMatchLocation", "character", NA, NA, NA,
                    "the file location of the rasterToMatch"),
    defineParameter("rasterToMatchName", "character", NA, NA, NA,
                    "the name of the rasterToMatch file"),
    defineParameter("studyAreaLocation", "character", NA, NA, NA,
                    "the file location of the studyArea"),
    defineParameter("nameBCR", "character", NA, NA, NA,
                    "the BAM regional model BCR region that the studyArea is located in"),
    defineParameter("folderUrlBirdRaster", "character", NA, NA, NA,
                    "the location of the bird density rasters"),
    defineParameter("fromDrive", "logical", TRUE, NA, NA,
                    "Should the rasterToMatch, studyArea and bird Rasters be found on Google Drive or a similar online source? If false, they should already be on your local computer."),
    defineParameter("excludeList", "character", NA, NA, NA,
                    "a list of species codes, for species that should not be included, regardless of their densities"),
    defineParameter("areaThreshold", "numeric", 0.05, 0, 1,
                    "The minimum area over which the probability of occurrence threshold must be reached for species to be considered present"),
    defineParameter("probOfOccurrenceThreshold", "numeric", 0.5, 0, 1,
                    "The minimum probability of occurence that must be reached for species to be considered present"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.birdRange = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.doSelectInitialTime, "birdRange", "selectBirds")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "birdRange", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "birdRange", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "birdRange", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "birdRange", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    selectBirds = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      sim <- selectBirds(sim)
      # schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim) + P(sim)$selectInterval, 
                           moduleName = "birdRange", eventType = "selectBirds") 
      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "birdRange", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "birdRange", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
selectBirds <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test
  
  # Calculate the density threshold for the given probability of occurrence threshold 
  ps <- 6.25  # pixel size in hectares conversion rate
  densityThreshold <- -base::log(1 - P(sim)$probOfOccurrenceThreshold) / ps
  print(paste("with a probability of occurrence threshold of ", P(sim)$probOfOccurrenceThreshold, " a density threshold of ", densityThreshold, " is required", sep = "" ))
  
 
  sim$birdList <- lapply(X = sim$birdRasters, FUN = function(birdRaster){
   #tryCatch({
     
     
     print(birdRaster)
    ## take the values from the rasters and input 
    ## them to a data table called cellValues
     clearPlot()
     Plot(birdRaster, na.color = "grey")
     hist(birdRaster)
     
     nameBird <- names(birdRaster)
     densityValues <- terra::values(birdRaster)
     densityValues <- na.omit(densityValues)
     noCells <- length(densityValues)
     meetsDensityThreshold <- sum(densityValues > densityThreshold)
     propArea <- meetsDensityThreshold/noCells
     meetsThreshold <-  propArea >= P(sim)$areaThreshold
    
     if(meetsThreshold == TRUE){
       return(nameBird)
     } else {
       return(NA)
     }
     
    
    #}, error = function(e) return(NA))
  })

   sim$birdList <- unlist(sim$birdList, use.names = FALSE)
  sim$birdList <- na.omit(sim$birdList)
  sim$birdList <- unlist(lapply(sim$birdList, substr, 8, 11)) #works for strings of the form "mosaic-XXXX-run3.tif"
  sim$birdList <- setdiff(sim$birdList, P(sim)$excludeList)
   #sim$birdList <- unlist(lapply(sim$birdList, substr, 1, 4)) #works for strings of the form "XXXX-meanboot_60.tif"
   
   #get names of all birds looked at
   sim$allBirds <-  lapply(X = sim$birdRasters, FUN = function(birdRaster){
     nameRas <- names(birdRaster)
     nameRas <- substr(nameRas, 8, 11) #works for strings of the form "mosaic-XXXX-run3.tif"
     #nameRas <- substr(nameRas, 1, 4) #works for strings of the form "XXXX-meanboot_60.tif"
         return(nameRas)
   })
   sim$allBirds <- unlist(sim$allBirds, use.names = FALSE)
   #sim$allBirdRasters <- unlist(sim$birdRasters, use.names = FALSE)
   
   #find birds that did not meet threshold
   sim$abscentBirds <- setdiff(sim$allBirds, sim$birdList)
   
   
   # browser()
   # sim$allBirdNames <-  names(sim$birdRasters)
   # #get the species codes as names for the downloadedRasters object, rather than using the whole filepath
   # sim$allBirdNames <- lapply(sim$allBirdRasters, substr, 8, 11) #works for strings of the form "mosaic-XXXX-run3.tif"
   # #sim$allBirdNames <- lapply(allBirdRasters, substr,1, 4) #works for strings of the form "XXXX...tif"
   # print(sim$allBirdNames)
   # #names(sim$birdRasters) <- sim$allBirdNames
   
    print("all bird species examined")
    print(sim$allBirds)
    
    print("birds that meet Threshold:")
    print(sim$birdList)
    
    print("birds that did not meet threshold, or were in the excludeList:")
    print(sim$abscentBirds)
    
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  # if (P(sim)$fromDrive == TRUE) {
  #   
  #   ### GET FILES FROM GOOGLE DRIVE ###
  #   #get RasterToMatch
  #   print("download rasterToMatch")
  #   rasterToMatch <-  googledrive::drive_download(
  #     file = P(sim)$rasterToMatchLocation,
  #     overwrite = TRUE,
  #     verbose = TRUE,
  #     path = file.path(inputsDir, P(sim)$rasterToMatchName)) #download rasterTomatch from drive
  #   sim$rasterToMatch <- terra::rast(file.path(inputsDir, P(sim)$rasterToMatchName)) #load raster into R
  #   
  #   # get studyArea shapefile
  #   print("get studyArea shapefile from drive")
  #   sim$studyArea <- prepInputs(targetFile = P(sim)$.studyAreaName,
  #                               url = P(sim)$studyAreaLocation,
  #                               archive = P(sim)$archiveStudyArea,
  #                               alsoExtract = "similar", #Extract other files with similar names
  #                               destinationPath = downloadFolderArea, #folder to download to
  #                               #fun = "terra::vect", #use the function shapefile
  #                               useTerra = TRUE,
  #                               targetCRS = crs(sim$rasterToMatch), #make crs same as rasterToMatch
  #                               overwrite = TRUE,
  #                               verbose = TRUE)
  #   
  #   #crop and mask rasterTomatch to studyArea
  #   sim$rasterToMatch <- terra::mask(terra::crop(sim$rasterToMatch, sim$studyArea), sim$studyArea)
  #   names(sim$rasterToMatch) <- "rasterToMatch"
  #   # dev()
  #   # clearPlot()
  #   # Plot(sim$rasterToMatch, na.color= "grey")
  #  
  # 
  #   #get bird density rasters
  #   patternNameBirdRaster <- "mosaic-" #choose naming pattern to look for
  #   
  #   ## drive_ls function is used to list all the files it finds using the folder url with the given pattern
  #   sim$allBirdRasters <-
  #     googledrive::drive_ls(path = as_id(P(sim)$folderUrlBirdRaster),
  #                           pattern = patternNameBirdRaster)
  #   
  #   print(sim$allBirdRasters$name)
  #   
  #   
  #   
  #   # grepl function searches for all items in the allBirdRasters that are on birdList & stores their names in rastersforBirdList
  #   # sim$rastersForBirdList <-
  #   #   allBirdRasters$name[grepl(pattern = paste(P(sim)$birdList, collapse = "|"),
  #   #                              x = allBirdRasters$name)]
  #   
  #   ## for each item in turn from rastersForBirdlist the following function is applied:
  #   sim$birdList <-
  #     lapply(
  #       X = sim$rastersForBirdList,
  #       FUN = function(rasterFile) {
  #         
  #         nameBird <- substr(rasterFile, 8, 11) #works for strings of the form "mosaic-XXXX-run3.tif"
  #         nameBird <- paste(nameBird, ".tif", sep = "")
  #         
  #         ## if the item in rastersForBirdList is not already present at rastersPath, googledrive package downloads it
  #         if (!file.exists(file.path(downloadFolderBird, rasterFile))) {
  #           googledrive::drive_download(
  #             file = as_id(allBirdRasters[allBirdRasters$name %in% rasterFile,]$id),
  #             overwrite = TRUE,
  #             path = file.path(downloadFolderBird, nameBird)
  #             
  #           )
  #         }
  #         
  #         ## otherwise, if it is already present and downloaded, just get the name of the item
  #         return(terra::rast(file.path(downloadFolderBird, nameBird)))
  #       }
  #     )
  #   
  #   # #get the species codes as names for the downloadedRasters object, rather than using the whole filepath
  #   # X <- lapply(sim$rastersForBirdList, substr, 8, 11) #works for strings of the form "mosaic-XXXX-run3.tif"
  #   # names(sim$birdRasters) <- X
  #   
  #   sim$birdRasters <- lapply(X = sim$birdRasters, FUN = function(RasterLayer) {
  #     ## the function postProcesses the layer, cropping and masking it to a given study area and rasterToMatch, and saving it to a given destination path
  #     
  #     proRaster <- reproducible::postProcess(RasterLayer,
  #                                            #studyArea = sim$studyArea,
  #                                            rasterToMatch = sim$rasterToMatch,
  #                                            useTerra = TRUE,
  #                                            fun = "terra::rast",
  #                                            destinationPath = downloadFolderBird,
  #                                            filename2 = paste(downloadFolderBird, "/", names(RasterLayer), ".tif", sep = ""),
  #                                            overwrite = TRUE,
  #                                            verbose = TRUE)
  #     # clearPlot()
  #     # Plot(proRaster, na.color= "grey")
  #     return(proRaster)
  #   })
  #   
  #   
  # } else {
  
    ### GET FILES FROM LOCAL LOCATION ###
    
    #get rasterToMatch
    print("get rasterToMatch from local drive")
    sim$rasterToMatch <- terra::rast(file.path(P(sim)$rasterToMatchLocation, P(sim)$rasterToMatchName))
    
    
    #get StudyArea shapefile
    print("get studyArea shapefile from local drive")
    studyArea <- terra::vect(file.path(P(sim)$studyAreaLocation, P(sim)$.studyAreaName))
    
    #postProcess studyArea
    sim$studyArea <- reproducible::postProcess(studyArea,
                                               destinationPath = downloadFolderArea,
                                               filename2 = "studyArea", 
                                               useTerra = TRUE,
                                               fun = "terra::vect", #use the function vect
                                               targetCRS = crs(sim$rasterToMatch), #make crs same as rasterToMatch
                                               overwrite = TRUE,
                                               verbose = TRUE)
    
    #crop and mask rasterToMatch
    sim$rasterToMatch <- terra::mask(terra::crop(sim$rasterToMatch, sim$studyArea), sim$studyArea) 
    names(sim$rasterToMatch) <- "rasterToMatch"
    
   
     #get bird density rasters
    sim$allBirdRasters <- sort(list.files(downloadFolderBird, pattern = ".tif")) #list all bird raster files with a given naming pattern
    #sim$allBirdRasters <- sort(list.files(downloadFolderBird, pattern = "-meanBoot_60")) #list all bird raster files with a given naming pattern
    print(sim$allBirdRasters)
    ## for each item in turn from rastersForBirdlist the following function is applied:
    sim$birdRasters <-
      lapply(
        X = sim$allBirdRasters,
        FUN = function(birdRaster) {
           raster <- terra::rast(file.path(downloadFolderBird, birdRaster))
          return(raster)
        }
      )
    
    sim$birdRasters <- lapply(X = sim$birdRasters, FUN = function(RasterLayer) {
      ## the function postProcesses the layer, cropping and masking it to a given study area and rasterToMatch, and saving it to a given destination path
      print(RasterLayer)
      
      proRaster <- reproducible::postProcess(RasterLayer,
                                             #studyArea = sim$studyArea,
                                             rasterToMatch = sim$rasterToMatch,
                                             useTerra = TRUE,
                                             fun = "terra::rast",
                                             destinationPath = downloadFolderBird,
                                             filename2 = paste(downloadFolderBird, "/", names(RasterLayer), ".tif", sep = ""),
                                             overwrite = TRUE,
                                             verbose = TRUE)
      
     
      # clearPlot()
      # Plot(proRaster, na.color= "grey")
      return(proRaster)
    })
    

    
  #}

  print(sim$birdRasters)
  #sim$allBirdNames <- names(sim$birdRasters)
  #names(sim$birdRasters) <- sim$allBirdNames
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
