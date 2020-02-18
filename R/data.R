#' House finch counts from the Breeding Bird Survey
#'
#' A spatio-temporal dataset containing yearly counts of the house finch from
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