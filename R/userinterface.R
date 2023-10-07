library(shiny)
library(opencage)

ui<-fluidPage(
  titlePanel("Stamen Map"),
  sidebarLayout(


    sidebarPanel(

      helpText("This is the app that can be visualize the map data what you searched location in the world."),

      selectInput("mapType",
                  label = "Choose a map type",
                  choices = list("terrain","terrain-background","terrain-labels",
                                 "terrain-lines", "toner", "toner-background",
                                 "toner-labels", "toner-lines", "toner-lite", "watercolor"),
                  selected = "toner"),
      textInput("address",
                label = "Type your city and country"
                ),
      sliderInput(inputId = "zoom",
                    label = "Zooming range:",
                    min = 1,
                    max = 18,
                    value = 10),
      actionButton("generateButton", "Get Stamen Image")

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      uiOutput("mapImage")
    )
  )
)

server <- function(input, output) {
  # Function to fetch and display the map image
  generateStamenMap <- function(address, maptype, zoom) {
    # Replace spaces with "+" in the address for the URL
    address <- gsub(" ", "+", address)

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


    lng <- -74.006  # Default longitude (New York City)
    lat <- 40.7128

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

    # Calculate xtile and ytile
    #https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Coordinates_to_tile_numbers_2
    lat_rad <- lat * pi /180
    n <- 2.0 ^ zoom
    xtile <- floor((lng + 180.0) / 360.0 * n)
    ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
    #return( c(xtile, ytile))
    #return(paste(paste("https://tile.openstreetmap.org", zoom, xtile, ytile, sep="/"),".png",sep=""))

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
    return(complete_url)
  }

  # Create a reactive value to store the image URL
  image_url <- reactiveVal(NULL)

  # Observe the "Generate Image" button click event
  observeEvent(input$generateButton, {
    # Get values from inputs
    mapType <- input$mapType
    address <- input$address
    zoom <- input$zoom

    # Call the generateStamenMap function to get the image URL
    img_url <- generateStamenMap(address,mapType, zoom)

    # Update the reactive value with the image URL
    image_url(img_url)
  })

  # Render the map image
  output$mapImage <- renderUI({
    img_url <- image_url()

    if (!is.null(img_url)) {
      tags$img(src = img_url, width = "100%")
    }
  })
}

shinyApp(ui = ui, server = server)

runApp("userinterface")

