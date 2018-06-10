library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

navbarPage("DIRKOIL LNG Case", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default",
        draggable = TRUE, top = 350, left = "auto", right = 1350, bottom = "auto",
        width = 330, height = "auto",

        h2("Variable explorer"),

        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "adultpop"),
        sliderInput("kms", "Truck distance:",min = 0, max = 900, value = 400),
        sliderInput("station", "Station distance:",min = 0, max = 900, value = 400),
        sliderInput("dot", "Dot radius (in m):",min = 1, max = 1000, value = 10)
      ),

      tags$div(id="cite",
        'DirkOil a LNG case, by Adriaens Matthias, Lauwers Diederick, Serrarens Simon & Van Essche Maarten'
      )
    ),tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
)
