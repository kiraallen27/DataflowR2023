#' rraster
#'
#' @slot range numeric.
#'
#' @export
#' @importFrom methods setClass
#'
rraster <- methods::setClass("rraster", contains="RasterStack", slots=c(range="numeric"))


#' streaminterp
#'
#'@param dt input data frame
#'@param paramlist list of parameters (dt column names) to interpolate
#'@param yearmon a file path used to extract basename
#'@param trim_rstack logical trim the raster stack by the convex hull of training points?
#'@param trim_negative logical set negative values to zero?
#'@param tname file.path location to save training dataset
#'@param vname file.path location to save validation dataset
#'@param missprop numeric proportion of missing data allowed. Variables with a greater proportion of missing data will be dropped.
#'@param fdir character file path to local data directory
#'@param costrasname character file.path to cost raster
#'
#' @export
#' @importFrom raster raster writeRaster mask stack crs
#' @importFrom gdata resample
#' @importFrom sp SpatialPointsDataFrame coordinates CRS spTransform proj4string
#' @importFrom sf st_as_sf
#' @importFrom ipdw ipdwInterp pathdistGen
#' @import rgeos
#' @import methods
#'
#' @examples \dontrun{
#'dt <- streamget(yearmon = 201513, qa = TRUE)
#'streaminterp(dt, paramlist = c("sal"), yearmon = 201513)
#'}

streaminterp<- function (dt, paramlist, yearmon, trim_rstack = TRUE, trim_negative = TRUE,
                         costrasname = "CostRas_Optimize_Clip.tif", tname = NA, vname = NA,
                         missprop = 0.16, fdir = getOption("fdir"))
{
  projstr <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  latlonproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  costras <- raster::raster(file.path(fdir, "DF_Basefile", "CostRas_Optimize_Clip.tif",
                                      costrasname))
  raster::crs(costras)<- projstr
  paramlist <- tolower(paramlist)
  names(dt) <- tolower(names(dt))
  if (!all(paramlist %in% names(dt))) {
    stop("check to make sure paramlist entries matches column names")
  }
  naparam <- lapply(paramlist, function(x) length(which(is.na(data.frame(dt[,
                                                                            x])))))
  if (any(which(naparam > (nrow(dt)/(1/missprop))))) {
    paramlist <- paramlist[-which(naparam > (nrow(dt) *
                                               missprop))]
    warning(paste(-which(naparam > (nrow(dt) * missprop)),
                  "has many missing values."))
  }
  if (length(paramlist) == 0) {
    stop("too many missing values")
  }
  if (is.na(tname) & is.na(vname)) {
    tname <- file.path(fdir, "/DF_Subsets/", yearmon, "s.csv",
                       fsep = "")
    vname <- file.path(fdir, "/DF_Validation/", yearmon,
                       "s.csv", fsep = "")
  }
  if (!file.exists(tname) & !file.exists(vname)) {
    fulldataset.over <- dt
    gridlev <- unique(fulldataset.over$gridcode)
    for (i in 1:length(gridlev)) {
      activesub <- subset(fulldataset.over, fulldataset.over$gridcode ==
                            gridlev[i])
      selectnum <- gdata::resample(1:nrow(activesub),
                                   1)
      if (i == 1) {
        training <- activesub[selectnum, ]
      }
      else {
        training <- rbind(training, activesub[selectnum,
        ])
      }
    }
    validate <- fulldataset.over[!(row.names(fulldataset.over) %in%
                                     row.names(training)), ]
    xy <- cbind(validate$lon_dd, validate$lat_dd)
    validate <- sp::SpatialPointsDataFrame(xy, validate)
    training <- training[!is.na(training$lat_dd), ]
    xy <- cbind(training$lon_dd, training$lat_dd)
    training <- sp::SpatialPointsDataFrame(xy, training)
    write.csv(training, tname)
    write.csv(validate, vname)
  } else {
    warning("using previously defined subset")
    training <- read.csv(tname, sep = ",")
    sp::coordinates(training) <- c("lon_dd", "lat_dd")
  }
  sp::proj4string(training) <- sp::CRS(latlonproj)
  training <- sp::spTransform(training, sp::CRS(projstr))
  training <- sf::st_as_sf(training) ###Addedd to resolve training not being a class sf object
  rstack <- ipdw::pathdistGen(training, costras, 100000, yearmon = yearmon)
  if (trim_rstack == TRUE) {
    rstack <- raster::mask(rstack, rgeos::gConvexHull(coordinatize(streamget(yearmon),
                                                                   latname = "lat_dd", lonname = "lon_dd")), inverse = FALSE)
  }
  rstack <- raster::stack(rstack) ###Added because thought later code needed raster stack instead of raster brick
  dir.create(file.path(fdir, "/DF_Surfaces/", yearmon))
  for (j in 1:length(paramlist)) {
    #rraster <- methods::setClass("rraster", contains="RasterStack", slots=c(range="numeric")) ###This and two lines below added to add range slot to rstack
    rstack <- as(rstack, "rraster") ###Added
    rstack@range <- 100000 ###Added
    finalras <- ipdw::ipdwInterp(training, rstack, paramlist[j],
                                 overlapped = TRUE, yearmon = yearmon)
    if (trim_negative) {
      finalras[finalras < 0] <- 0
    }
    rf <- raster::writeRaster(finalras, filename = file.path(fdir,
                                                             "DF_Surfaces", yearmon, paste(paramlist[j], ".grd",
                                                                                           sep = "")), overwrite = T)
    rf <- raster::writeRaster(finalras, filename = file.path(fdir,
                                                             "DF_Surfaces", yearmon, paste(paramlist[j], ".tif",
                                                                                           sep = "")), overwrite = T, format = "GTiff")
  }
}
