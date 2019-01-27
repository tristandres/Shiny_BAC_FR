#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
data <- read.csv(file = "Lycee_Data.csv")

function(input,output){
  dataInput1 <- reactive({
    melt(data[which(data$Departement == input$Departements),],id.vars = "Secteur", measure.vars = c("Taux_S","Taux_ES","Taux_L","Taux_Total"))
  })
  dataInput2 <- reactive({
    melt(data[which(data$Departement == input$Departements),],id.vars = "Total", measure.vars = c("Taux_S","Taux_ES","Taux_L","Taux_Total"))
    
  })
  dataInput3 <- reactive({
    melt(data[which(data$Departement == input$Departements),],id.vars = "Groupe", measure.vars = c("Taux_S","Taux_ES","Taux_L","Taux_Total"))
    
  })
  
  pal3 <- reactive({
    colorBin("RdYlGn", domain = c[[input$exam]], bins = bins)
  })
  dataInput4 <- reactive({
    data.frame("Section" = c("S","ES","L","total"), "Nb_of_Students" = c(sum(data[which(data$Departement == input$Departements),]$nb_S),
                                                                        sum(data[which(data$Departement == input$Departements),]$nb_ES),
                                                                        sum(data[which(data$Departement == input$Departements),]$nb_L),
                                                                       (sum(data[which(data$Departement == input$Departements),]$nb_Total) - sum(data[which(data$Departement == input$Departements),]$nb_Tech))))
  })
  output$map1 <- renderLeaflet(map_1)
  output$box1 <- renderPlot(
    ggplot(dataInput1()) + geom_boxplot(aes(x=Secteur,y=value,color=variable)) +
      scale_y_continuous(name = "Passing %",
                         breaks = seq(60, 100, 2),
                         limits=c(70, 100)) +
      labs(x = "", y = "", 
           title = "Passing Rates for Private & Public Schools")
  )
  
  output$box2 <- renderPlot(
    ggplot(dataInput2()) + geom_boxplot(aes(x=Total,y=value,color=variable)) +
    scale_y_continuous(name = "Passing %",
                       breaks = seq(60, 100, 2),
                       limits=c(60, 100)) +
    labs(x = "",y = "", 
         title = "Passing Rates for all Schools Confounded"))
  
  output$box3 <- renderPlot(
    ggplot(dataInput3()) + geom_boxplot(aes(x=Groupe,y=value,color=variable)) +
    scale_y_continuous(name = "Passing %",
                       breaks = seq(60, 100, 2),
                       limits=c(60, 100)) +
    labs(x = "",y = "", 
         title = "Passing Rates for Schools Offering a Tech Degree"))
  
    output$bar1 <- renderPlot(
    ggplot(dataInput4(), aes(x=Section, y=Nb_of_Students)) +
    geom_bar(stat="identity") + 
    scale_y_continuous(name = "Number of students") +
    labs(x = "", y = "", title = "Student Distribution"))
    
    output$table = DT::renderDataTable({
      table1
    })
    
    output$minBox <- renderUI({
     infoBox(paste(input$exam," Min. Passing Rate"),
      min(c[[input$exam]]),
      icon = icon("angle-double-down"),
      color = "red",
      width=3
      )
    })
    
    output$maxBox <- renderUI({
      infoBox(paste(input$exam," Max. Passing Rate"),
              max(c[[input$exam]]),
              icon = icon("angle-double-up"),
              color = "green",
              width=3
      )
    })
    
    output$avgBox <- renderUI({
      infoBox(paste(input$exam," Avg. Passing Rate"),
              round(mean(c[[input$exam]]),1),
              icon = icon("align-justify"),
              color = "yellow",
              width=3
      )
    })
    
    output$map2 <- renderLeaflet(
      spdf_departement %>% 
      subset(. , str_length(code_insee) == 2) %>% 
      leaflet() %>% 
      addProviderTiles("Esri.WorldStreetMap") %>%
      addPolygons(fillColor = ~pal1(c[[input$exam]]),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "5",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf(
                    "<strong>%s</strong><br/>%g %% passing rate",
                    cc, c[[input$exam]])
                  %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% addLegend(pal = pal1, values = ~density, opacity = 0.7, title = sprintf("%s passing rate %%",input$exam),
                                                       position = "bottomright"))
    
    
}

