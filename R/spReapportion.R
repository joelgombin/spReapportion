#' Reapportion data from one geography to another
#'
#' This function allows to reapportion data from one geography to another, for example in the context of working with different administrative units.
#' @author Joel Gombin
#' @note Inspiration from http://stackoverflow.com/a/17703903 and http://rstudio-pubs-static.s3.amazonaws.com/6577_3b66f8d8f4984fb2807e91224defa854.html. All mistakes are mine, obviously.
#' @param old_geom a `SpatialPolygonsDataFrame` representing the initial geometry.
#' @param new_geom a `SpatialPolygonsDataFrame` representing the geometry you want to reapportion data to.
#' @param data a `data.frame` containing the data to reapportion, and an ID allowing to match it to the `old_geom`.
#' @param old_ID a string, the name of the ID variable in the `old_geom`.
#' @param new_ID a string, the name of the ID variable in the `new_geom`.
#' @param data_ID a string, the name of the ID variable in the `data`.
#' @param variables a character vector, representing the names of the variables in the `data` set to reapportion. By default, all data variables except for the ID.
#' @param mode either `"count"` or `"proportion"`. `"count"` is for absolute values, `"proportion"` is for, well, proportions (expressed between 0 and 1). If `"proportion"`, you need to provide a weights variable.
#' @param weights In case the variables are proportions, the name of the variable containing weights (i.e. the total number of observations per unit in the `old_geom`).
#' @param weight_matrix a SpatialPointsDataFrame indicating where are the observations (inhabitants, voters, etc.) (optional).
#' @param weight_matrix_var the name of the variable in \code{weight_matrix} containing the weights.
#' @export
#' @import sp maptools rgeos purrr
#'
spReapportion <- function(old_geom, new_geom, data, old_ID, new_ID, data_ID, variables = names(data)[-which(names(data) %in% data_ID)], mode = "count", weights = NULL, weight_matrix = NULL, weight_matrix_var = NULL) {

  if (!(old_ID %in% names(old_geom@data))) stop(paste(old_ID, "is not a variable from", deparse(substitute(old_geom)),"!", sep=" "))
  if (!(new_ID %in% names(new_geom@data))) stop(paste(new_ID, "is not a variable from", deparse(substitute(new_geom)),"!", sep=" "))
  if (sum(!(variables %in% names(data))) > 0) stop(paste(variables[!(variables %in% names(data))], "is not a variable from", deparse(substitute(data)),"!",sep=" "))
  if (mode %in% "proportion" & is.null(weights)) stop("When mode = 'proportion', you must provide weights.")
  if (mode %in% "proportion")
    if (!(weights %in% names(data)))
      stop(paste0(weights, " is not a variable from ", deparse(substitute(data)), "!"))
  if (!is.null(weight_matrix) & !inherits(weight_matrix, "SpatialPointsDataFrame", which = FALSE))
    stop("The weight_matrix argument is not a SpatialPointsDataFrame!")



  # if several polygons with the same ID, merge them
  if (length(old_geom@data[,old_ID]) > length(unique(old_geom@data[,old_ID]))) {
    df <- old_geom@data[match(unique(old_geom@data[,old_ID]),old_geom@data[,old_ID]),]
    old_geom <- unionSpatialPolygons(old_geom, old_geom@data[,old_ID])
    old_geom <- SpatialPolygonsDataFrame(old_geom, df, old_ID)
  }
  if (length(new_geom@data[,new_ID]) > length(unique(new_geom@data[,new_ID]))) {
    df <- new_geom@data[match(unique(new_geom@data[,new_ID]),new_geom@data[,new_ID]),]
    new_geom <- unionSpatialPolygons(new_geom, new_geom@data[,new_ID])
    new_geom <- SpatialPolygonsDataFrame(new_geom, df, new_ID)
  }

  # make sure SPDF IDs are OK
  old_geom <- spChFIDs(old_geom, as.character(old_geom@data[,old_ID]))
  new_geom <- spChFIDs(new_geom, as.character(new_geom@data[,new_ID]))

  names(data)[names(data) %in% data_ID] <- "old_ID"

  # make sure both SPDFs have the same projection
  if (!identicalCRS(old_geom, new_geom)) {
    message("Reprojecting new_geom to the same projection as old_geom...")
    new_geom <- spTransform(new_geom, old_geom@proj4string)
  }

  # use weight matrix if provided
  if (!is.null(weight_matrix)) {
    weight_matrix <- weight_matrix[colSums(gWithin(weight_matrix, old_geom, byid = TRUE)) > 0,]
    weight_matrix_total <- sum(weight_matrix@data[, weight_matrix_var], na.rm = TRUE)
    weight_matrix@data <- cbind(weight_matrix@data, over(weight_matrix, old_geom))
  }


  # start by trimming out areas that don't intersect

  old_geom_sub <- rgeos::gIntersects(old_geom, new_geom, byid = TRUE) # test for areas that don't intersect
  old_geom_sub2 <- apply(old_geom_sub, 2, function(x) {sum(x)}) # test across all polygons in the SpatialPolygon whether it intersects or not
  old_geom_sub3 <- old_geom[old_geom_sub2 > 0,] # keep only the ones that actually intersect

  # perform the intersection. This takes a while since it also calculates area and other things, which is why we trimmed out irrelevant areas first
  int <- rgeos::gIntersection(old_geom_sub3, new_geom, byid = TRUE, drop_lower_td = TRUE) # intersect the polygon and your administrative boundaries

  intdf <- data.frame(intname = names(int)) # make a data frame for the intersected SpatialPolygon, using names from the output list from int
  intdf$intname <- as.character(intdf$intname) # convert the name to character
  splitid <- strsplit(intdf$intname, " ", fixed=TRUE) # split the names
  splitid <- do.call("rbind", splitid) # rbind those back together
  colnames(splitid) <- c("old_ID", "new_ID") # now you have the administrative area ID and the polygonID as separate variables in a dataframe that correspond to the int SpatialPolygon.
  intdf <- data.frame(intdf, splitid) # make that into a dataframe
  intdf$old_ID <- as.character(intdf$old_ID) # convert to character
  intdf$new_ID <- as.character(intdf$new_ID) # convert to character.


  # now you have a dataframe corresponding to the intersected SpatialPolygon object

  if (!is.null(weight_matrix)) {
    # check in which intersected polygon each point stands
    weight_matrix_int <- over(weight_matrix, int)
    # use points weights to reapportion
    intdf$polyarea <- map_int(1:length(int), ~ sum(weight_matrix@data[weight_matrix_int %in% .x, "n"]))
    data$departarea <- map_int(old_geom@data[, old_ID], ~ sum(weight_matrix@data[weight_matrix@data[, old_ID] %in% .x, weight_matrix_var]))[match(data[[old_ID]], old_geom@data[[old_ID]])]
  } else {
    # if we don't have weights we just use areas
    intdf$polyarea <- gArea(int, byid = TRUE) # get area from the polygon SP object and put it in the df
    data$departarea <- gArea(old_geom, byid = TRUE)[match(data$old_ID, old_geom@data[, old_ID])]
  }

  intdf2 <- dplyr::left_join(intdf, data, by = c("old_ID" = old_ID)) # join together the two dataframes by the administrative ID
  if (mode %in% "count") {
    intdf2[,paste(variables,"inpoly",sep="")] <- plyr::numcolwise(function(x) {x * (intdf2$polyarea / intdf2$departarea)})(as.data.frame(intdf2[,variables]))
    intpop <- plyr::ddply(intdf2, "new_ID", function(x) {plyr::numcolwise(sum, na.rm = TRUE)(as.data.frame(x[,paste(variables,"inpoly",sep="")]))}) # sum population lying within each polygon
    names(intpop)[-1] <- variables
  } else {
    intdf2[,paste(variables,"inpoly",sep="")] <- plyr::numcolwise(function(x) {x * (intdf2$polyarea / intdf2$departarea)})(as.data.frame(intdf2[,variables]) * intdf2[,weights])
    intdf2$weights <- intdf2[,weights] * (intdf2$polyarea / intdf2$departarea)
    intpop <- plyr::ddply(intdf2, "new_ID", function(x) {plyr::numcolwise(sum, na.rm = TRUE)(as.data.frame(x[,c(paste(variables,"inpoly",sep=""), "weights")]))}) # sum population lying within each polygon
    intpop[,paste0(variables, "inpoly")] <- intpop[,paste0(variables, "inpoly")] / intpop$weights
    names(intpop)[-c(1, length(names(intpop)))] <- variables
    names(intpop)[length(names(intpop))] <- weights
  }



  names(intpop)[1] <- new_ID
  return(intpop) # done!
}
