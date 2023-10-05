library(shiny)

ui<-fluidPage(
  titlePanel("Stem Map"),
  sidebarLayout(


    sidebarPanel(

      helpText("This is the app that can be visualize the map data what you searched location in the world."),

      selectInput("var",
                  label = "Choose a map type",
                  choices = list("Water Colour",
                                 "Tonner",
                                 "Terrain"),
                  selected = "Tonner"),
      textInput("var2",
                label = "Tyepe your city and country"
                ),
      sliderInput(inputId = "bins",
                    label = "Zooming range:",
                    min = 1,
                    max = 100,
                    value = 15)

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      generateStamenMap("ryd linkoping", "watercolor", 15)

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

  })

}

shinyApp(ui = ui, server = server)

runApp("userinterface")

