
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library("ggplot2")
library("lubridate")

shinyServer(function(input, output) {

  output$family_selector <- renderUI({
    selectInput("fam", label="Family", choices=c("-", all_families), selected="-")
  })
  get_selected_family <- reactive({
    x <- input$fam
    if ( is.null(x) | x == "-" ) { x <- all_families }
    x
  })

  output$genus_selector <- renderUI({
    avail_fam <- get_selected_family()
    staxo <- filter(taxo, family %in% avail_fam)
    avail_gen <- sort(unique(staxo$genus))
    selectInput("gen", label="Genus", choices=c("-", avail_gen), selected="-")
  })
  get_selected_genus <- reactive({
    x <- input$gen
    if ( is.null(x) | x == "-" ) { x <- all_genera }
    x
  })

  output$species_selector <- renderUI({
    avail_fam <- get_selected_family()
    avail_gen <- get_selected_genus()
    staxo <- filter(taxo, family %in% avail_fam, genus %in% avail_gen)
    avail_sp <- sort(unique(staxo$species))
    selectInput("sp", label="Species", choices=c("-", avail_sp))
  })
  get_selected_species <- reactive({
    x <- input$sp
    if ( is.null(x) | x == "-" ) { x <- all_species }
    x
  })

  get_selected_data <- reactive({
    ds <- d
    
    ds <- filter(ds, weeks_since_start == input$week)
    
    # aggregate data at the correct taxonomic level
    if ( input$sp != "-") {
      ds <- filter(ds, species == input$sp)
    } else if ( input$gen != "-" ) {
      ds <- filter(ds, genus == input$gen)
      ds <- summarise(group_by(ds, place, lat, lon, weeks_since_start, genus), cpue=ifelse(all(is.na(cpue)), as.numeric(NA), sum(cpue, na.rm=T)))
    } else if ( input$fam != "-" ) {
      ds <- filter(ds, family == input$fam)
      ds <- summarise(group_by(ds, place, lat, lon, weeks_since_start, family), cpue=ifelse(all(is.na(cpue)), as.numeric(NA), sum(cpue, na.rm=T)))
    } else {
      ds <- summarise(group_by(ds, place, lat, lon, weeks_since_start), cpue=ifelse(all(is.na(cpue)), as.numeric(NA), sum(cpue, na.rm=T)))
    }
    ds
  })

  output$plot <- renderPlot({
    ds <- get_selected_data()
    ds <- rbind.fill(ds, data.frame(place="dummy", lat=40, lon=2, cpue=0))
    # to force legend
    
    p <- map +
      geom_point(aes(x=lon, y=lat), data=all_places, size=2, colour="grey20", shape=4) +
      geom_point(aes(x=lon, y=lat, size=cpue), data=ds, na.rm=TRUE) +
      scale_size_continuous("Nb per CARE", range=c(2,20), limits=c(0,10), trans="sqrt") +
      theme(legend.position="bottom") +
      coord_quickmap(ylim=c(41, 44), xlim=c(2.5, 10.5)) +
      annotate("text", x=3, y=41.1, label=(ymd("2012-01-01") + weeks(ds$weeks_since_start[1])))
    print(p)
  }, height=500)

})

