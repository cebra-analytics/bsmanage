#' Manage Divisions class builder
#'
#' Builds a class to represent one or more divisions (parts, locations,
#' categories, etc.) specified for a management resource allocation design.
#' It may be defined via a raster layer with active (non-NA) cells, via a data
#' frame of locations or patches with spatial coordinates, or via a data frame
#' without coordinates for none-spatial divisions, such as disease categories.
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster} object
#'   representing a grid-based spatial region (template). The spatial division
#'   of locations may also be defined via a data frame (or matrix) of location
#'   coordinates in longitude and latitude (WGS84) with explicitly named
#'   columns "lon" and "lat". Alternatively, other none-spatial divisions, such
#'   as categories, may be defined via a data frame (or matrix) with one or
#'   more rows (one per part) but without coordinate columns.
#' @param ... Additional parameters.
#' @return A \code{ManageDivisions} class object (list) containing functions
#'   for accessing attributes, checking compatibility of objects with the
#'   divisions, and to maintain and calculate spatial data:
#'   \describe{
#'     \item{\code{get_type()}}{Get the type of representation: spatial "grid"
#'       (raster cells), "patch" (coordinates), or "other" (none-spatial).}
#'     \item{\code{get_parts()}}{Get the number of parts (locations,
#'       categories, management resource units, etc.) that are specified for
#'       the design.}
#'     \item{\code{is_compatible(y)}}{Check the compatibility of object
#'       \code{y} with the divisions defined by \code{x}.}
#'     \item{\code{get_template(empty = FALSE)}}{Get the spatial template when
#'      the \code{type} is "grid", with either zeros in non-NA locations
#'      (default), or with no values when \code{empty = TRUE}.}
#'     \item{\code{get_rast(values)}}{Get spatial \code{terra::SpatRaster} set
#'       with a single value or a vector of values matching the number of
#'       locations when \code{type} is "grid".}
#'     \item{\code{get_indices()}}{Get cell indices of grid or patch locations
#'       that are included in the design when the \code{type} is "grid".}
#'     \item{\code{get_res()}}{Get the spatial cell resolution (in m) of the
#'       region when the \code{type} is "grid".}
#'     \item{\code{is_included(indices)}}{Check if cell \code{indices} of grid
#'       locations are included (non-NA cells) in the design when the
#'       \code{type} is "grid", and return a logical vector indicating the
#'       inclusion of each index.}
#'     \item{\code{get_data()}}{Get the data frame specifying the division when
#'       \code{type} is "patch" or "other".}
#'     \item{\code{get_coords(extra_cols = FALSE)}}{Get a data frame of
#'       location coordinates when \code{type} is "patch" or "grid", as well as
#'       optional extra named columns from the original patch-based location
#'       data.}
#'     \item{\code{get_feat()}}{Get spatial \code{terra::SpatVector} features
#'       or points when \code{type} is "grid" or "patch".}
#'   }
#' @export
ManageDivisions <- function(x, ...) {
  UseMethod("ManageDivisions")
}

#' @name ManageDivisions
#' @export
ManageDivisions.Raster <- function(x, ...) {

  # Call the terra version of the function
  ManageDivisions(terra::rast(x), ...)
}

#' @name ManageDivisions
#' @export
ManageDivisions.SpatRaster <- function(x, ...) {

  # Non-NA cell indices and points (terra::SpatVector)
  indices <- which(!is.na(x[]))
  grid_pts <- terra::as.points(x, values = FALSE)

  # Create a class structure
  self <- structure(list(), class = "ManageDivisions")

  # Get the divisions type
  self$get_type <- function() {
    return("grid")
  }

  # Get the number of parts (cell locations) that are specified for the design
  self$get_parts <- function() {
    return(length(indices))
  }

  # Check compatibility of a spatial raster y with the divisions specified by x
  self$is_compatible <- function(y) {
    y <- terra::rast(y)
    return(terra::crs(y) == terra::crs(x) &&
             terra::ext(y) == terra::ext(x) &&
             all(terra::res(y) == terra::res(x)))
  }

  # Get spatial template with zero/NA or empty values
  self$get_template <- function(empty = FALSE) {
    if (empty) {
      template <- terra::rast(x)
    } else {
      template <- x*0
      names(template) <- "value"
    }
    return(template)
  }

  # Get spatial raster set with values
  self$get_rast <- function(values) {

    # Check values is vector of correct length
    if (!length(values) %in% c(1, self$get_parts())) {
      stop(paste("Values should be a single value or vector of length",
                 "matching the number of region locations."), call. = FALSE)
    }

    # Set values raster as appropriate type
    if (is.factor(values)) {
      value_rast <- terra::as.factor(self$get_template())
      terra::set.values(value_rast, indices, values)
      levels(value_rast) <- data.frame(ID = 1:length(levels(values)),
                                       category = levels(values))
    } else if (is.logical(values)) {
      value_rast <- terra::as.bool(self$get_template())
      terra::set.values(value_rast, indices, values)
    } else if (is.integer(values)) {
      value_rast <- terra::as.int(self$get_template())
      terra::set.values(value_rast, indices, values)
    } else {
      value_rast <- as.numeric(self$get_template())
      terra::set.values(value_rast, indices, values)
    }

    return(value_rast)
  }

  # Get cell indices
  self$get_indices <- function() {
    return(indices)
  }

  # Get the spatial cell resolution
  self$get_res <- function() {
    if (terra::is.lonlat(x)) { # EPSG:4326
      corners <- array(terra::ext(x), c(2, 2))
      diagonal <- terra::distance(corners[1,,drop = FALSE],
                                  corners[2,,drop = FALSE], lonlat = TRUE)
      return(diagonal/sqrt(terra::nrow(x)^2 + terra::ncol(x)^2))
    } else {
      return(mean(terra::res(x)[1]))
    }
  }

  # Get location coordinates (unused extra named columns for grid)
  self$get_coords <- function(extra_cols = FALSE) {
    coords <- terra::as.data.frame(terra::project(grid_pts, "EPSG:4326"),
                                   geom = "XY")
    colnames(coords) <- c("lon", "lat")
    return(coords)
  }

  # Check if cell indices are included (non-NA cells) in the design
  self$is_included <- function(indices) {
    return(!is.na(x[indices][,1]))
  }

  # Get spatial terra::SpatVector features or points
  self$get_feat <- function() {
    return(grid_pts)
  }

  return(self)
}

#' @name ManageDivisions
#' @export
ManageDivisions.matrix <- function(x, ...) {

  # Call the data frame version of the function
  ManageDivisions(as.data.frame(x), ...)
}

#' @name ManageDivisions
#' @export
ManageDivisions.data.frame <- function(x, ...) {

  # Check data frame
  if (all(c("lon", "lat") %in% names(x))) {

    # Set type
    type = "patch"

    # Patch points (terra::SpatVector)
    patch_pts <- terra::vect(x[, c("lon", "lat")], crs = "EPSG:4326")

  } else {
    type = "other"
  }

  # Create a class structure
  self <- structure(list(), class = "ManageDivisions")

  # Get the divisions type
  self$get_type <- function() {
    return(type)
  }

  # Get the number of parts (locations, categories, resource units, etc.)
  # that are specified for the design
  self$get_parts <- function() {
    return(nrow(x))
  }

  # Check compatibility of vector, matrix, or adjacency data frame y
  # with the divisions defined by x
  self$is_compatible <- function(y) {
    if (is.data.frame(y)) {
      return(ncol(y) == 3 && all(unique(unlist(y[,1:2])) %in% 1:nrow(x)))
    } else {
      y <- as.matrix(y)
      return(nrow(y) == nrow(x) && ncol(y) %in% c(1, nrow(x)))
    }
  }

  # Get the data frame specifying the division
  self$get_data <- function() {
    return(x)
  }

  # Functions for "patch" type only
  if (type == "patch") {

    # Get location coordinates plus optional extra named columns
    self$get_coords <- function(extra_cols = FALSE) {
      if (extra_cols) {
        extra_cols <- names(x)[which(!names(x) %in% c("lon", "lat"))]
        return(x[, c("lon", "lat", extra_cols)])
      } else {
        return(x[, c("lon", "lat")])
      }
    }

    # Get spatial terra::SpatVector features or points
    self$get_feat <- function() {
      return(patch_pts)
    }
  }

  return(self)
}
