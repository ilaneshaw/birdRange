library("conflicted")
library("SpaDES.core")
#library("googledrive")

## make a list of directory paths
inputsDir <- checkPath("../../inputs", create = TRUE)
outputsDir <- checkPath("../../outputs", create = TRUE)
downloadFolderArea <- checkPath(file.path(inputsDir, "studyArea"), create = TRUE)
downloadFolderBird <- checkPath(file.path(inputsDir, "birdRasterFiles"), create = TRUE)
outputMeanBirdRasters <- checkPath(file.path(outputsDir, "birds"), create = TRUE)
setPaths(modulePath = file.path("../../modules"),
         cachePath = file.path("../../cache"),
         scratchPath = file.path("../../scratch"),
         inputPath = inputsDir,
         outputPath = outputsDir)


simPaths <- getPaths()

# #parameters from Drive
# folderUrlBirdRaster <- ""
# rasterToMatchLocation <- as_id("")
# rasterToMatchName <- "LCC2005_V1_4a.tif"
# nameBCR <- "60"
# 
# ### STUDY AREA AB
# studyAreaLocation <- file.path(")
# #specify file name
# .studyAreaName <- "studyAreaAB.shp" #specify file name
#  #specify folder url
# archiveStudyArea <- "studyAreaAB.zip" #give archive name

#parameters from local
rasterToMatchLocation <- inputsDir
rasterToMatchName <- "LCC2005_V1_4a.tif"
studyAreaLocation <- downloadFolderArea
nameBCR <- "60"
.studyAreaName <- "studyArea_AB_BCR6.shp"
folderUrlBirdRaster <- downloadFolderBird
excludeList <- c("EUST", "HOSP", "ROPI")



simModules <- list("birdRange")

## Set simulation and module parameters
simTimes <- list(start = 1, end = 1, timeunit = "year")
simParams <- list(
  birdRange = list( 
    .doSelectInitialTime = 1,
    fromDrive = FALSE,
    folderUrlBirdRaster = folderUrlBirdRaster,
    .studyAreaName = .studyAreaName,
    #archiveStudyArea = archiveStudyArea,
    rasterToMatchLocation = rasterToMatchLocation,
    rasterToMatchName = rasterToMatchName,
    studyAreaLocation = studyAreaLocation,
    nameBCR = nameBCR,
    excludeList = excludeList,
    areaThreshold = 0.05,
    probOfOccurrenceThreshold = 0.5
  )
)



## Simulation setup
mySim <- simInit(times = simTimes, params = simParams, 
                 modules = simModules, paths = simPaths)

test <- spades(mySim)
