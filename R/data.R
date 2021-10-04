#' Carolina wren counts from the Breeding Bird Survey
#'
#' A spatio-temporal dataset containing yearly counts of the Carolina wren from
#'   the North American Breeding Bird Survey, between the years 1967 and 2014.
#'
#' We obtained the data the "Spatio-Temporal Statistics with R" book by C. K.
#'   Wikle et al, available at \url{spatetimewithr.org}. The original citation
#'   for the dataset is given below.
#'
#' @format An sf object of point geometries containing 1,575 rows and 2 fields:
#' \describe{
#'  \item{cnt}{The number of bird sighting for one sampling unit.}
#'  \item{year}{The year of the sample.}
#' }
#' @source \url{ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/Archivefiles/Version2016v0/}
"bird_survey"

#' Haddock survey data from the Scotian shelf
#'
#' A spatio-temporal dataset containing observations of haddock abundance
#'   from a yearly scientific survey on the Scotian shelf from the years 1982 to 2017.
#'
#' The data were obtained from the Department of Fisheries and Oceans Canada.
#'
#' @format An sf object of point geometries containing 6,434 rows and 11 fields:
#' \describe{
#'   \item{TotalWeight}{Total biomass caught}
#'   \item{TotalCount}{Total number of indidivuals caught}
#'   \item{Presence}{Presence (1) or absence (0) of fish}
#'   \item{Year}{The year of the sample}
#'   \item{Duration}{The duration of the fishing set}
#'   \item{Distance}{The distance traveled during the fishing set}
#'   \item{Speed}{The average speed traveled during the fishing set}
#'   \item{SurfaceTemperature}{Sea surface temperature}
#'   \item{BottomTemperature}{Sea bottom temperature}
#'   \item{BottomSalinity}{Water salinity at the seafloor}
#'   \item{Area}{The area swept by the fishing set}
#' }
"haddock_survey"
