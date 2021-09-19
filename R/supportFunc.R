#' Function to calculate areas of classes within a categorical raster
#'
#' This function takes a categorical raster object and calculates the areas covered by each class within the raster.
#' @param x An object of class RasterLayer.
#' @return A data frame with one column representing the values of the classes of the raster and the other variable representing corresponding areas in Ha.
#' @author Faustin Gashakamba
#' @details
#' The raster package's "area()" function normally returns the same raster where cell values have been replaced by their areas in Square Km.
#' Using this function, the values of all cells of same class are aggregated and the raster is transformed into a data frame.
#' If the CRS of the input raster is projected, then the area is calculated by multiplying the resolution of the raster by the count of cells for each class.
#' @seealso \code{\link{area}}
#' @seealso \code{\link{aggregate}}
#' @seealso \code{\link{getValues}}
#' @importFrom raster area
#' @importFrom raster aggregate
#' @importFrom raster getValues
#' @importFrom raster isLonLat
#' @importFrom raster res
#' @importFrom raster crs
#' @importFrom raster as.data.frame
#' @examples
#' calc_areas(lulc_yanze) # calculate the area covered by each class in a categorical raster
#' @export
#'
calc_areas <- function(x){
  if(isLonLat(crs(x))){
    b <- getValues(area(x, weights=FALSE)) #assign each cell by its area
    b <- raster::aggregate(b, by=list(getValues(x)), sum, na.rm=TRUE)
    b <- raster::as.data.frame(b)
    names(b) <- c("CLASS","AREA")
    b$AREA <- b$AREA * 100 #Convert in Ha
    return(b)
  }else{
    b <- raster::as.data.frame(x) %>%
      group_by(getValues(x)) %>%
      tally() %>%
      mutate(AREA = (n * res(x)[1] * res(x)[2])/10000) #calculate area in Ha

    names(b) <- c("CLASS", "COUNT", "AREA")
    CLASS <- NULL #deal with the  R CMD check
    b <- b %>% filter(!is.na(CLASS)) #remove NA cells
    return(select(b,"CLASS", "AREA"))
  }
}

#' Test if user-supplied inputs match the expected arguments type and form
#'
#' This function tests whether the input supplied by the user are of the expected type (class),
#' and are in the right form (overlap of extents and same projection).
#'
#' @param x The input provided by the user for the x argument.
#' @param y The input provided by the user for the y argument.
#' @return There is no return value. If any error is found, the execution is just halted.
#' @importFrom raster extract
#' @importFrom raster extent
#' @importFrom raster compareCRS
#'
test_arguments <- function(x, y){
  if(!any(class(x) %in% c("SpatialPolygonsDataFrame", "sf"))){
    stop("X has to be a spatial object")
  }
  if(!any(class(y) %in% c("SpatRaster", "RasterLayer", "stars"))){
    stop("y has to be a raster object")
  }
  if(is.null(extract(y, x)) == TRUE){
    stop("the two inputs have to spatially overlap")
  }
  if(compareCRS(x, y) != TRUE){
    stop("the two inputs have to be of the same projection")
  }
}

#' Convert the input vector object into simple features if it's provided as a Spatial object
#'
#' This function tests whether the supplied vector input is a simple features (sf) object,
#' if not, the object is converted using the st_as-sf() function.
#'
#' @param x The input provided by the user for the x argument.
#' @return the converted simple features object
#' @importFrom sf st_as_sf
#'
Vector_conversion <- function(x){
  if(class(x)[1] != "sf"){
    x <- st_as_sf(x)
  }
  return(x)
}
