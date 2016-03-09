
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library("shiny")

shinyUI(fluidPage(

  # Application title
  titlePanel("MedPlanet"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput("family_selector"),
      uiOutput("genus_selector"),
      uiOutput("species_selector"),
      
      
      # sliderInput("day", label = "Day of sampling",
      #             min = 1, max = 52, step=1,
      #             value = 1,
      #             animate=TRUE
      # ),
      sliderInput("week", label = "Week number",
                  min = min(d$weeks_since_start), max = max(d$weeks_since_start), step=1,
                  value = 1,
                  animate=animationOptions(interval = 500)
      ),
      # checkboxGroupInput(
      #   "sp",
      #   "Species",
      #   choices=species,
      #   selected=species[1:2]
      # ),
      div()
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # submitButton(),
      plotOutput("plot")
    )
  )
))

