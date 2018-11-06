# copied most of this function from vizlab:::readData.shp. can't directly reuse
# that function here because it requires viz syntax. Also, we'll use this function to filter out territories for which we don't have water use data
#' @param skip a list with the field name and the values to skip
read_shp_zip <- function(zipfile, skip = NULL) {

  # unzip the file into a temporary location
  shp_path <- file.path(tempdir(), 'tmp')
  if (!dir.exists(shp_path)){
    dir.create(shp_path)
  }
  unzip(zipfile, exdir = shp_path)

  # identify the layer (assumes there's exactly one)
  layer <- tools::file_path_sans_ext(list.files(shp_path, pattern='*.shp'))[1]

  # read the layer from the shapefile
  data_out <- rgdal::readOGR(shp_path, layer=layer, verbose=FALSE)

  if (!is.null(skip) & length(skip) != 1){
    stop('skip must be a list of length one if specified.')
  }

  if (!is.null(skip)){
    # filter out any field attribute matches we want to skip
    data_out <- data_out[!(data_out@data[[names(skip)[1]]] %in% skip[[1]]), ]
  }

  # clean up and return
  unlink(shp_path, recursive = TRUE)
  return(data_out)
}

#' @param sp the full spatial object to be altered, w/ STATEFP attribute
#' @param ... named character argument for fields in `sp` to be scaled
#' @param scale a scale factor to apply to fips
#' @return an `sp` similar to the input, but with the specified fips scaled according to `scale` parameter
mutate_sp_coords <- function(sp, ..., scale, shift_x, shift_y, rotate, ref = sp){
  args <- list(...)
  field <- names(args)
  if (length(field) != 1){
    stop(args, ' was not valid')
  }
  # we can specify a single "field" name that is an attribute of the spatial data (e.g., STATEFP)
  # that "field" can have multiple values (e.g, "02" and "71") that we'll apply "scale" to.
  # here we test that the length of "scale" is equal to the length of the values:
  values <- args[[1]]

  for (i in 1:length(values)){
    tomutate_sp <- sp[sp@data[[field]] %in% values[[i]], ]
    tomutate_ref <- ref[ref@data[[field]] %in% values[[i]], ]
    mutated_sp <- mutate_sp(tomutate_sp, scale = scale[i], shift = c(shift_x[i], shift_y[i]), rotate = rotate[i], ref = tomutate_ref)
    if (inherits(sp, 'SpatialPoints')){
      # had to do this to retain order...sp...shrug
      sp_points <- as(sp, "SpatialPoints")
      sp_data <- sp@data
      sp_points_mutated <- as(mutated_sp, "SpatialPoints")
      sp_data_mutated <- mutated_sp@data
      sp_out <- rbind(sp_points[!sp_data[[field]] %in% values[[i]], ], sp_points_mutated)
      sp_data <- rbind(sp_data[!sp_data[[field]] %in% values[[i]], ], sp_data_mutated)
      row.names(sp_data) <- seq(1:nrow(sp_data))

      sp_out <- SpatialPointsDataFrame(sp_out, data = sp_data)

    } else {
      sp_out <- rbind(sp[!(sp@data[[field]] %in% values[[i]]), ], mutated_sp)
    }

    sp <- sp_out
  }

  return(sp_out)
}

mutate_sp <- function(sp, scale = NULL, shift = NULL, rotate = 0, ref=sp, proj.string=NULL, row.names=NULL){

  if (is.null(scale) & is.null(shift) & rotate == 0){
    return(obj)
  }
  orig.cent <- colMeans(rgeos::gCentroid(ref, byid=TRUE)@coords)
  scale <- max(apply(bbox(ref), 1, diff)) * scale
  obj <- elide(sp, rotate=rotate, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, rotate=rotate, center=orig.cent, bb = bbox(ref))
  obj <- elide(obj, scale=scale, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, scale=scale, center=orig.cent, bb = bbox(ref))
  new.cent <- colMeans(rgeos::gCentroid(ref, byid=TRUE)@coords)
  obj <- elide(obj, shift=shift*10000+c(orig.cent-new.cent))

  if (is.null(proj.string)){
    proj4string(obj) <- proj4string(sp)
  } else {
    proj4string(obj) <- proj.string
  }

  if (!is.null(row.names)){
    row.names(obj) <- row.names
  }

  return(obj)
}
