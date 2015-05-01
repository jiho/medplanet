
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library("shiny")

shinyServer(function(input, output) {

  get.data <- reactive({
    load("data.Rdata")
    d$week <- week(d$date)
    
    ds <- d
    ds <- filter(ds, species %in% input$sp)
    ds <- filter(ds, week >= input$week[1], week <= input$week[2])
    ds
  })

  output$plot <- renderPlot({

    d <- get.data()
    p <- ggplot(d) + geom_line(aes(x=date, y=cpue)) + facet_grid(species~., scales="free_y")
    print(p)

  })

})

