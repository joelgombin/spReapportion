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
#' @export
#' @import sp
#'
spReapportion <- function(old_geom, new_geom, data, old_ID, new_ID, data_ID, variables = names(data)[-which(names(data) %in% data_ID)]) {

  if (!(old_ID %in% names(old_geom@data))) stop(paste(old_ID, "is not a variable from", deparse(substitute(old_geom)),"!", sep=" "))
  if (!(new_ID %in% names(new_geom@data))) stop(paste(new_ID, "is not a variable from", deparse(substitute(new_geom)),"!", sep=" "))
  if (sum(!(variables %in% names(data))) > 0) stop(paste(variables[!(variables %in% names(data))], "is not a variable from", deparse(substitute(data)),"!",sep=" "))


  # if several polygons with the same ID, merge them
  if (length(old_geom@data[,old_ID]) > length(unique(old_geom@data[,old_ID]))) {
    df <- old_geom@data[match(unique(old_geom@data[,old_ID]),old_geom@data[,old_ID]),]
    old_geom <- maptools::unionSpatialPolygons(old_geom, old_geom@data[,old_ID])
    old_geom <- SpatialPolygonsDataFrame(old_geom, df, old_ID)
  }
  if (length(new_geom@data[,new_ID]) > length(unique(new_geom@data[,new_ID]))) {
    df <- new_geom@data[match(unique(new_geom@data[,new_ID]),new_geom@data[,new_ID]),]
    new_geom <- maptools::unionSpatialPolygons(new_geom, new_geom@data[,new_ID])
    new_geom <- SpatialPolygonsDataFrame(new_geom, df, new_ID)
  }

  # make sure SPDF IDs are OK
  old_geom <- spChFIDs(old_geom, as.character(old_geom@data[,old_ID]))
  new_geom <- spChFIDs(new_geom, as.character(new_geom@data[,new_ID]))

  names(data)[names(data) %in% data_ID] <- "old_ID"

  # start by trimming out areas that don't intersect

  old_geom_sub <- rgeos::gIntersects(old_geom, new_geom, byid=TRUE) # test for areas that don't intersect
  old_geom_sub2 <- apply(old_geom_sub, 2, function(x) {sum(x)}) # test across all polygons in the SpatialPolygon whether it intersects or not
  old_geom_sub3 <- old_geom[old_geom_sub2 > 0,] # keep only the ones that actually intersect
  # perform the intersection. This takes a while since it also calculates area and other things, which is why we trimmed out irrelevant areas first

  int <- rgeos::gIntersection(old_geom_sub3, new_geom, byid=TRUE, drop_lower_td = TRUE) # intersect the polygon and your administrative boundaries

  intdf <- data.frame(intname = names(int)) # make a data frame for the intersected SpatialPolygon, using names from the output list from int
  intdf$intname <- as.character(intdf$intname) # convert the name to character
  splitid <- strsplit(intdf$intname, " ", fixed=TRUE) # split the names
  splitid <- do.call("rbind", splitid) # rbind those back together
  colnames(splitid) <- c("old_ID", "new_ID") # now you have the administrative area ID and the polygonID as separate variables in a dataframe that correspond to the int SpatialPolygon.
  intdf <- data.frame(intdf, splitid) # make that into a dataframe
  intdf$old_ID <- as.character(intdf$old_ID) # convert to character
  intdf$new_ID <- as.character(intdf$new_ID) # convert to character.


  # now you have a dataframe corresponding to the intersected SpatialPolygon object

  intdf$polyarea <- sapply(int@polygons, function(x) {x@area}) # get area from the polygon SP object and put it in the df
  data$departarea <- sapply(old_geom@polygons, function(x) {x@area})[match(data$old_ID, old_geom@data[, old_ID])]
  intdf2 <- plyr::join(intdf, data, by="old_ID") # join together the two dataframes by the administrative ID
  intdf2[,paste(variables,"inpoly",sep="")] <- plyr::numcolwise(function(x) {x * (intdf2$polyarea / intdf2$departarea)})(as.data.frame(intdf2[,variables]))

  intpop <- plyr::ddply(intdf2, "new_ID", function(x) {plyr::numcolwise(sum)(as.data.frame(x[,paste(variables,"inpoly",sep="")]))}) # sum population lying within each polygon

  names(intpop)[-1] <- variables
  names(intpop)[1] <- new_ID
  return(intpop) # done!
}
