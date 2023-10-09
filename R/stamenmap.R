#' This function is to create a stamen map image.
#' @references Reference page link <https://maps.stamen.com/#watercolor/11/40.6575/-73.9685>
#' @description generateStamenMap is to build a stamen map image when given an address, map type and zoom range.
#' @param address An address, e.g. New York City
#' @param maptype The map type that stamen supported.
#' @param zoom The zoom parameter is an integer between 0 (zoomed out) and 18 (zoomed in). 18 is normally the maximum.
#' @returns An image of stamen map and geocodes addresses (longitude and latitude)
#' @examples
#' maps <- generateStamenMap("ryd linkoping", "watercolor", 15)
#' magick::image_read(maps$map_image)
#' maps$lat_longs
#' @import httr
#' @import ggplot2
#' @import magick
#' @import opencage
#' @export generateStamenMap
# Define a function to generate a Stamen map image
generateStamenMap <- function(address, maptype, zoom) {
  # Checking address argument
  if ( !is.character(address) ) {
    stop("The argument address should be a string, please check!")
  }

  maptype_array <- c("terrain","terrain-background","terrain-labels",
                     "terrain-lines", "toner", "toner-background",
                     "toner-labels", "toner-lines", "toner-lite", "watercolor")
  # Checking maptype argument
  if ( !maptype %in% maptype_array ) {
    stop("The supported map type are: terrain,terrain-background,terrain-labels,
                     terrain-lines, toner, toner-2010, toner-2011, toner-background,
                     toner-hybrid, toner-labels, toner-lines, toner-lite, watercolor.")
  }

  # Checking zoom argument
  # https://github.com/dkahle/ggmap/blob/master/R/get_stamenmap.R
  if ( !(is.numeric(zoom) && length(zoom) == 1 && zoom == round(zoom) && zoom >=0 && zoom <=18) ) {
    stop("The argument zoom should be a positve integer ranging from 0-18")
  }

  # Set default opencage API keys for the first time
  env_vars <- Sys.getenv()
  if ( !"OPENCAGE_KEY" %in% names(env_vars)) {
    Sys.setenv("OPENCAGE_KEY" = "d77c46fce55647ec9ca422379173a3ec")
  }

  # Geocode the address using the opencage package
  tryCatch(
    {
      result_df <- oc_forward_df(address)
      lat = result_df$oc_lat
      lng = result_df$oc_lng
    },
    error = function(e) {
      message("Geocoding failed. Using default coordinates.")
      list(lat = 40.7128, lng = -74.006)  # Default coordinates (New York City)
    }
  )

  # Check if geocoding was successful
  if (is.na(lng) || is.na(lat)) {
    stop("Geocoding failed.")
  }
  # store latitue and longitude into a dataframe for further functions
  lat_longs <- data.frame(name=paste0(gsub("[,\\.]", "", gsub("\\s+", "_", address))), latitude=lat,longitude=lng)
  # Calculate xtile and ytile
  #https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Coordinates_to_tile_numbers_2
  lat_rad <- lat * pi /180
  n <- 2.0 ^ zoom
  xtile <- floor((lng + 180.0) / 360.0 * n)
  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)

  # Construct the base URL for the Stamen Maps API
  # https://docs.stadiamaps.com/guides/migrating-from-stamen-map-tiles/
  base_url <- "https://tiles.stadiamaps.com/tiles/"
  map_url <- paste0(base_url, "stamen_", maptype)

  if ( maptype == "watercolor" ) {
    filetype <- ".jpg"
  } else {
    filetype <- "@2x.png"
  }

  # Create the map image URL with the latitude and longitude
  complete_url <- sprintf(
    "%s/%d/%d/%d%s",
    map_url,
    zoom,     # Zoom level (adjust as needed)
    xtile,  # Latitude
    ytile,  # Longitude
    filetype # Filetype (png or jpg)
  )

  # Send a GET request to the Stamen Maps API and retrieve the map image
  response <- httr::GET(complete_url)

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Read the downloaded image
    map_content <- httr::content(response, "raw")
  } else {
    stop("Failed to retrieve the map image.")
  }
  return(list(map_image = map_content, lat_longs = lat_longs))
}
