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

  tabPanel("Interactive station map",
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
        sliderInput("numlng", "Number of stations:",min = 0, max = 10, value = 5),
        sliderInput("kms", "Truck distance:",min = 0, max = 900, value = 400),
        sliderInput("station", "Station distance:",min = 0, max = 40, value = 20),
        actionButton("solution", "GENERATE SOLUTION")
      ),

      tags$div(id="cite",
        'DirkOil a LNG case, by Adriaens Matthias, Lauwers Diederick, Serrarens Simon & Van Essche Maarten'
      )
    ),tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ),

  tabPanel("Station explorer",
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
                 "App built by: Adriaens Matthias, Lauwers Diederik, Serrarens Simon & Van Essche Maarten"
               ),
               mainPanel(
                 h3("Application overview"),
                 h4("Data selection"),
                 p("It is here possible to set the type of station that the user wants to see on the map. The terminals are always
                   displayed, even if all the checkboxes are unchecked. It is possible to toggle LNG,CNG and petrol station. Each of 
                   the different type of stations are also clickable. A popup will show when the user clicks te map.
                   This makes the map more interactive and gives the user a richer experience when exploring the possibilities."),
                 h4("Competition"),
                 p("Competition is a true or false variable that takes the existing lng-station in mind when suggesting new LNG places."),
                 h4("Target"),
                 p("The target of the aloright is a checkbox where the user can decide to only incorporate trucks, cars or both of them.
                   The application suggests different solutions for different targets."),
                 h4("Number of stations"),
                 p("The number of stations is the number of stations that managment is looking to build. The application will suggest 
                   the here predefined number of stations on the map. It will also link those stations to the nearest terminal."),
                 h4("Truck distance"),
                 p("This is a contraint in our linear programm for which distance the trucks are limited in driving between the 
                   LNG-station and the terminal"),
                 h4("Station distance"),
                 p("This parameter allows the user to set the minimum distance between the newly generated stations. This 
                   distance is defined in bird's eye view."),
                 h4("Measure tool"),
                 p("This is a little tool that is provided. You can find in in the bottom left of the page.
                   It is a nifty tool to measure linear distances and areas.
                   Multiple measurements can be taken in order to compare areas and distances in order to enhace the decision making
                   process")
               )
             )
           )
           ),
  conditionalPanel("false", icon("crosshair"))
)
