#' Carolina wren counts from the Breeding Bird Survey
#'
#' A spatio-temporal dataset containing yearly counts of the Carolina wren from
#'   the North American Breeding Bird Survey, between the years 1967 and 2014.
#'
#' We obtained the data the "Spatio-Temporal Statistics with R" book by C. K.
#'   Wikle et al, available at \url{spacetimewithr.org}. The original citation
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
#'   from a yearly scientific survey on the Scotian shelf from the years 1982 to
#'   2017.
#'
#' The data were obtained from the Department of Fisheries and Oceans Canada and
#'   are more thoroughly described in the following paper.
#'
#' Jubinville, I. / Lawler, E. / Tattrie, S. / Shackell, N. L. / Mills Flemming,
#'   J. / Worm, B. Distributions of Threatened Skates and Commercial Fisheries
#'   Inform Conservation Hotspots 2021. Mar Ecol Prog Ser , Vol. 679, p. 1-18
#'
#' @source \url{https://doi.org/10.3354/meps13938}
#'
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
