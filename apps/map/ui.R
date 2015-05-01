
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library("shiny")
library("lubridate")

load("data.Rdata")
species <- as.character(catches$species)
names(species) <- str_c(catches$species, " (", round(catches$n), ")")
dates <- sort(unique(d$date))
julian <- range(d$julian)

shinyUI(fluidPage(

  # Application title
  titlePanel("MedPlanet"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("day", label = "Day of sampli",
                  min = 1, max = 52, step=1,
                  value = 1,
                  animate=TRUE
      ),
      # sliderInput("week", label = "Week of year",
      #             min = 1, max = 52, step=1,
      #             value = 1,
      #             animate=TRUE
      # ),
      checkboxGroupInput(
        "sp",
        "Species",
        choices=species,
        selected=species[1:2]
      ),
      div()
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # submitButton(),
      plotOutput("plot")
    )
  )
))

