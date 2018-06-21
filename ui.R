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
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
      
        h3("Data selection"),
        checkboxInput('petrol', 'Petrol',value = TRUE),
        checkboxInput('cng', 'CNG', value = TRUE),
        checkboxInput('lng', 'LNG', value = TRUE),
        h3("Variable input"),
        checkboxInput('comp', 'Competition',value = FALSE),
        selectInput("target", "Target:",
                    c("Cars and Trucks" = "all traffic",
                      "Trucks only" = "trucks",
                      "Cars only" = "cars")),
        sliderInput("numlng", "Number of LNGs:",min = 0, max = 10, value = 5),
        sliderInput("kms", "Truck distance:",min = 0, max = 900, value = 400),
        sliderInput("station", "Station distance:",min = 0, max = 40, value = 20),
        actionButton("solution", "GENERATE SOLUTION")
      ),

      tags$div(id="cite",
        'DirkOil a LNG case, by Adriaens Matthias, Lauwers Diederick, Serrarens Simon & Van Essche Maarten'
      )
    ),tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ),

  tabPanel("Basetable explorer",
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
  tabPanel("Help",
           fluidPage(
             titlePanel("DirkOil, a LNG case"),
             sidebarLayout(
               sidebarPanel(
                 h2("General"),
                 p("This application is solving a linear programming problem with hueristics and under certain constraints. It is solved under a set of parameters that can be set by the user of the application.The program returns a set of LNG stations under the given variables. Some options in order to provide discoverability are given."),
                 br(),
                 p("Navigate in your terminal to the app directory and run: "),
                 code("R -e shiny::runApp('lngapp')"),
                 br(),
                 br(),
                 img(src = "logo_full.png", height = 100, width = 270),
                 br(),
                 br(),
                 "App built by: Adriaens Matthias, Lauwers Diederick, Serrarens Simon & Van Essche Maarten"
               ),
               mainPanel(
                 h1("Introducing DirkOil"),
                 p("Shiny is a new package from RStudio that makes it ",
                   em("incredibly easy "),
                   "to build interactive web applications with R."),
                 br(),
                 p("For an introduction and live examples, visit the ",
                   a("Shiny homepage.",
                     href = "http://shiny.rstudio.com")),
                 br(),
                 h2("Interactive Map"),
                 h4("Data input"),
                 h4("Variable input"),
                 h4("Measure tool"),
                 p("This is a little tool that is provided. You can find in in the bottom left of the page.
                   It is a nifty tool to measure linear distances and areas.
                   Multiple measurements can be taken in order to compare areas and distances in order to enhace the decision making
                   process"),
                 h2("Basetable explorer")
               )
             )
           )









           ),
  conditionalPanel("false", icon("crosshair"))
)
