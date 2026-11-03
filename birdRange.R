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
  reqdPkgs = list(
    "PredictiveEcology/SpaDES.core@development (>= 1.1.1)", "ggplot2", "data.table", "terra", "googledrive"
  ),
  parameters = bindrows(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(
      ".plots", "character", "screen", NA, NA,
      "Used by Plots function, which can be optionally used here"
    ),
    defineParameter(
      ".doSelectInitialTime", "numeric", start(sim), NA, NA,
      "Describes the simulation time at which the first selection event should occur."
    ),
    defineParameter(
      "selectInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between select events."
    ),
    defineParameter(
      ".plotInitialTime", "numeric", start(sim), NA, NA,
      "Describes the simulation time at which the first plot event should occur."
    ),
    defineParameter(
      ".plotInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between plot events."
    ),
    defineParameter(
      ".saveInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first save event should occur."
    ),
    defineParameter(
      ".saveInterval", "numeric", NA, NA, NA,
      "This describes the simulation time interval between save events."
    ),
    defineParameter(
      ".studyAreaName", "character", NA, NA, NA,
      "Human-readable name for the study area used - e.g., a hash of the study",
      "area obtained using `reproducible::studyAreaName()`"
    ),
    defineParameter(
      "rasterToMatchLocation", "character", NA, NA, NA,
      "the file location of the rasterToMatch"
    ),
    defineParameter(
      "rasterToMatchName", "character", NA, NA, NA,
      "the name of the rasterToMatch file"
    ),
    defineParameter(
      "studyAreaLocation", "character", NA, NA, NA,
      "the file location of the studyArea"
    ),
    defineParameter(
      "locationSpRas", "character", NA, NA, NA,
      "the location of the sp density rasters"
    ),
    defineParameter(
      "namePattern", "character", NA, NA, NA,
      "a naming pattern that can be used to identify wanted species rasters at locationSpRas"
    ),
    defineParameter(
      "spNameSubstr", "numeric", NA, NA, NA,
      "Two numbers giving the location of the first and last characters of the species name within the species raster file name"
    ),
    defineParameter(
      "fromDrive", "logical", TRUE, NA, NA,
      "Should the rasterToMatch, studyArea and sp Rasters be found on Google Drive or a similar online source? If false, they should already be on your local computer."
    ),
    defineParameter(
      "excludeList", "character", NA, NA, NA,
      "a list of species codes, for species that should not be included, regardless of their densities"
    ),
    defineParameter(
      "areaThreshold", "numeric", 0.25, 0, 1,
      "The minimum area over which the probability of occurrence threshold must be reached for species to be considered present"
    ),
    defineParameter(
      "probOfOccurrenceThreshold", "numeric", 0.05, 0, 1,
      "The minimum probability of occurence that must be reached for species to be considered present"
    ),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(
      ".seed", "list", list(), NA, NA,
      "Named list of seeds to use for each event (names)."
    ),
    defineParameter(
      ".useCache", "logical", FALSE, NA, NA,
      "Should caching of events or module be used?"
    )
  ),
  inputObjects = bindrows(
    # expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    # createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "spList", objectClass = "character", 
                  desc = "A character vector of the species names/codes that meet the probOfOccurrenceThreshold over the areaThreshold and that are not on the excludeList")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.birdRange <- function(sim, eventTime, eventType) {
  switch(eventType,
         init = {
           ### check for more detailed object dependencies:
           ### (use `checkObject` or similar)
           
           # do stuff for this event
           sim <- Init(sim)
           #sim <- selectSps(sim)
           
           # schedule future event(s)
           sim <- scheduleEvent(sim, P(sim)$.doSelectInitialTime, "birdRange", "selectSps")
           sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "birdRange", "plot")
           sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "birdRange", "save")
         },
         plot = {
           # ! ----- EDIT BELOW ----- ! #
           # do stuff for this event
           
           plotFun(sim) # example of a plotting function
           # schedule future event(s)
           
           # e.g.,
           # sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "birdRange", "plot")
           
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
         selectSps = {
           # ! ----- EDIT BELOW ----- ! #
           # do stuff for this event
           
           # e.g., call your custom functions/methods here
           # you can define your own methods below this `doEvent` function
           sim <- selectSps(sim)
           # schedule future event(s)
           sim <- scheduleEvent(sim,
                                eventTime = time(sim) + P(sim)$selectInterval,
                                moduleName = "birdRange", eventType = "selectSps"
           )
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
                       "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
                       sep = ""
         ))
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
selectSps <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  # densityThreshold ----
  # Calculate the density threshold for the given probability of occurrence threshold 
  crsSpRas <- terra::crs(sim$spRasters[[1]], proj=TRUE)
  if(grepl("units=m", crsSpRas)){ #check sp raster units are in meters
    ps <- prod(terra::res(sim$spRasters[[1]])) / 10000 
  }else{
    stopifnot(grepl("units=m", crsSpRas))
  }
  sim$densityThreshold <- -base::log(1 - P(sim)$probOfOccurrenceThreshold) / ps
  print(paste("with a probability of occurrence threshold of ", P(sim)$probOfOccurrenceThreshold, " a density threshold of ", sim$densityThreshold, " is required", sep = ""))
  
  # inputSpRas ----
  sim$inputSpRas <- lapply(X = sim$spRasters, FUN = function(spRaster) {
    nameRas <- names(spRaster)
    return(nameRas)
  })
  sim$inputSpRas <- unlist(sim$inputSpRas, use.names = FALSE)
  print("inputSpRas:") 
  print(sim$inputSpRas)
  
  
 ## spList ----
  sim$spList <- lapply(X = sim$spList, FUN = function(sp) {
    tryCatch(
      {
    
        spRaster <- sim$spRasters[[sp]]
        hist(spRaster)

        ## Get raster density values 
        densityValues <- terra::values(spRaster)
        densityValues <- na.omit(densityValues)

        ## Calculate if meets threshold 
        noCells <- length(densityValues)
        meetsDensityThreshold <- sum(densityValues > sim$densityThreshold)
        propArea <- meetsDensityThreshold / noCells
        meetsThreshold <- propArea >= P(sim)$areaThreshold

        if (meetsThreshold == TRUE) {
          #print(paste(nameSp, " meets threshold", sep = ""))
          return(sp)
        } else {
          #print(paste(nameSp, " doesn't meet threshold", sep = ""))
          return(NA)
        }
        
      },
      error = function(e) {
        return(NA)
      }
    )
  })

  sim$spList <- unlist(sim$spList, use.names = FALSE)
  sim$spList <- na.omit(sim$spList)
  sim$spList <- setdiff(sim$spList, P(sim)$excludeList)

  print("spList: sps that meet Thresholds and were not on exclude list")
  print(sim$spList)

  
  # rejectSp ----
  sim$rejectSps <- setdiff(sim$inputSpRas, sim$spList)
  
  print("rejectSp: sps that did not meet threshold, or were on the excludeList")
  print(sim$rejectSps)

  # amend spRasters ----
  # remove all rasters not on spList
  
  
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
  
  # cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  # ! ----- EDIT BELOW ----- ! #

  # rasterToMatch ----
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    print("get rasterToMatch from local drive")
    sim$rasterToMatch <- terra::rast(file.path(P(sim)$rasterToMatchLocation, P(sim)$rasterToMatchName))
    terra::values(sim$rasterToMatch) <- 1
    names(sim$rasterToMatch) <- "rasterToMatch"
  }
  
  # studyArea ----
  if (!suppliedElsewhere("studyArea", sim)) {
    print("get studyArea shapefile from local drive")
    studyArea <- terra::vect(file.path(P(sim)$studyAreaLocation, P(sim)$.studyAreaName))
    
    # postProcess studyArea
    sim$studyArea <- reproducible::Cache(reproducible::postProcessTo,
                                         from = studyArea,
                                         to = sim$rasterToMatch,
                                         overwrite = FALSE,
                                         verbose = TRUE
    )
  }
 
  # crop and mask rasterToMatch to studyArea
  #sim$rasterToMatch <- reproducible::Cache(terra::crop, sim$rasterToMatch, sim$studyArea)
  #sim$rasterToMatch <- reproducible::Cache(terra::mask, sim$rasterToMatch, sim$studyArea)
  
  # spRasters ----
   if (!suppliedElsewhere("spRasters", sim)) { 
  
     #get names of all sp rasters
     if(is.na(P(sim)$namePattern)){ 
       ## list all sp raster files
       sim$namesAllSpRas <- sort(list.files(P(sim)$locationSpRas)) 
     } else {
       ## list all sp raster files with a given naming pattern
       sim$namesAllSpRas <- sort(list.files(P(sim)$locationSpRas, pattern = P(sim)$namePattern))
     }
     
  #input spRasters   
  sim$spRasters <- lapply(X = sim$namesAllSpRas, FUN = function(ras) {
    
    tryCatch({
    
    spRas <- terra::rast(file.path(P(sim)$locationSpRas, ras))
    
    #name the raster sp
    spName <- substr(ras, P(sim)$spNameSubst[1], P(sim)$spNameSubst[2]) 
    names(spRas) <- spName
    
   # postprocess raster - cropping, masking to the study area and reprojecting to the rasterToMatch
   spRas <- reproducible::postProcessTo(
     from = spRas,
     to = sim$rasterToMatch,
     cropTo = sim$studyArea,
     maskTo = sim$studyArea,
     overwrite = FALSE,
     verbose = TRUE
   )
   
   return(spRas)
   
      }, error = function(e) {return(NA)})
  
    })
  
  # Remove any NAs in the spRasters List ####
  naSp <- is.na(sim$spRasters)
  sim$spRasters <- sim$spRasters[!naSp]
  
  #Get vector of sp input from raster names
  sim$inputSpRas <- unlist(lapply(X = sim$spRasters, FUN = function(spRas) {
    sp <- names(spRas)
    return(sp)
  }), use.names = FALSE)
  
  #name spRasters
  names(sim$spRasters) <- sim$inputSpRas
  print(".inputObjects: inputSpRas") 
  print(sim$inputSpRas)
  
  #check for multiple rasters per sp
  stopifnot(!any(duplicated(sim$inputSpRas))) 

   }
   
    # spList ----
   if (!suppliedElsewhere("spList", sim)) {
          sim$spList <- sort(unique(sim$inputSpRas))
   }
  print(".inputObjects: spList")
  print(sim$spList)
  
    # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
