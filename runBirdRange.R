
library("conflicted")
library("SpaDES.core")
# library("googledrive")


## make a list of directory paths
setPaths(
  modulePath = file.path("../../modules"),
  cachePath = file.path("../../cache"),
  scratchPath = file.path("../../scratch"),
  inputPath = file.path("../../inputs"),
  outputPath = file.path("../../outputs/birdRange")
)
simPaths <- getPaths()

# specify where inputs come from
locationRasterToMatch <- Paths$inputPath
rasterToMatchName <- "ALFL-meanBoot_BCR-60_studyArea_AB_BCR6"

studyAreaLocation <- checkPath(file.path(Paths$inputPath, "studyArea/studyArea_AB_BCR6"), create = TRUE)
.studyAreaName <- "studyArea_AB_BCR6.shp"

locationSpRas <- checkPath(file.path(Paths$inputPath, "meanSpRasters"), create = TRUE)

nameBCR <- "60"

excludeList <- c("EUST", "HOSP", "ROPI") # specify any species to exclude- here, non-native species


## Set simulation and module parameters
simModules <- list("birdRange")
simTimes <- list(start = 1, end = 1, timeunit = "year")
simParams <- list(
  birdRange = list(
    .doSelectInitialTime = 1,
    fromDrive = FALSE,
    locationSpRas = locationSpRas,
    .studyAreaName = .studyAreaName,
    # archiveStudyArea = archiveStudyArea,
    rasterToMatchLocation = locationRasterToMatch,
    rasterToMatchName = rasterToMatchName,
    studyAreaLocation = studyAreaLocation,
    nameBCR = nameBCR,
    excludeList = excludeList,
    areaThreshold = 0.01,
    probOfOccurrenceThreshold = 0.25
  )
)


## Simulation setup
mySim <- simInit(
  times = simTimes, params = simParams,
  modules = simModules, paths = simPaths
)

test <- spades(mySim)

