library(shiny)
library(RColorBrewer)
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))

data <- read.csv("muntanya.csv")
i_col <- getPalette(length(levels(data$Region)))
names(i_col) <- levels(factor(data$Region))

ui <- navbarPage(
  
  title = "Sortides Muntanya",
  tabPanel(
    "Table",
    
    # *Input() functions
    
    #### Create a new Row in the UI for selectInputs
    fluidRow(
      column(4, 
             selectInput(
               inputId = "type",
               label = "Type:",
               choices = c("All",
                           unique(as.character(data$Type)))
             )),
      column(4,
             selectInput(
               inputId = "region",
               label = "Region:",
               choices = c("All",
                           unique(as.character(data$Region)))
             )),
      column(4,
             selectInput(
               inputId = "month",
               label = "Month:",
               choices = c("All",
                           "January", "February", "March", "April",
                           "May", "June", "July", "August",
                           "September", "October", "November", "December"
                           )))
    ),
    
    fluidRow(
      column(4,
             sliderInput(
               inputId = "distance",
               label = "Distance",
               min = min(data$Distance, na.rm = TRUE),
               max = max(data$Distance, na.rm = TRUE),
               value = c(min(data$Distance, na.rm = TRUE), 
                         max(data$Distance, na.rm = TRUE))
             )),
      column(4,
             sliderInput(
               inputId = "desnivell",
               label = "Desnivell",
               min = min(data$Denivell, na.rm = TRUE),
               max = max(data$Denivell, na.rm = TRUE),
               value = c(min(data$Denivell, na.rm = TRUE), 
                         max(data$Denivell, na.rm = TRUE))
             )),
      column(4,
             sliderInput(
               inputId = "altitudine",
               label = "Altitudine",
               min = min(data$Altitude, na.rm = TRUE),
               max = max(data$Altitude, na.rm = TRUE),
               value = c(min(data$Altitude, na.rm = TRUE), 
                         max(data$Altitude, na.rm = TRUE))
             ))
    ),
    
    # *Output() functions
    #### Create a new row for the table.
    DT::dataTableOutput("table")
  ), # close tabPanel "Table"
  
  tabPanel(
    "Graphs",
    plotOutput(
      outputId = "plot_regions"
    )
  ),
  
  tabPanel(
    "Done",
    checkboxInput("checkbox", label = "Done", value = TRUE),
    plotOutput(
      outputId = "plot_done"
    )
  )
)

server <- function(input, output){
  output$table <- DT::renderDataTable(DT::datatable({
    data <- read.csv("muntanya.csv")[,-1]
    if (input$type != "All") {
      data <- data[data$Type == input$type,]
    }
    if (input$region != "All") {
      data <- data[data$Region == input$region,]
    }
    if(input$month != "All"){
      data <- data[grep(substr(input$month, 1, 3), data$Period), ]
    }
    
    data <- data[(data$Distance > input$distance[1] &
                   data$Distance < input$distance[2]) |
                   is.na(data$Distance), ]
    data <- data[(data$Denivell > input$desnivell[1] &
                   data$Denivell < input$desnivell[2]) |
                   is.na(data$Denivell), ]
    data <- data[(data$Altitude > input$altitudine[1] &
                   data$Altitude < input$altitudine[2]) |
                   is.na(data$Altitude), ]
    data[,-c(7,10,11)]
  }, rownames= FALSE))
  
  output$plot_regions <- renderPlot({
    if (input$type != "All") {
      data <- data[data$Type == input$type,]
    }
    if (input$region != "All") {
      data <- data[data$Region == input$region,]
    }
    if(input$month != "All"){
      data <- data[grep(input$month, data$Period), ]
    }
    
    data <- data[(data$Distance > input$distance[1] &
                    data$Distance < input$distance[2]) |
                   is.na(data$Distance), ]
    data <- data[(data$Denivell > input$desnivell[1] &
                    data$Denivell < input$desnivell[2]) |
                   is.na(data$Denivell), ]
    data <- data[(data$Altitude > input$altitudine[1] &
                    data$Altitude < input$altitudine[2]) |
                   is.na(data$Altitude), ]
    tmp <- data.frame(table(data$Region))
    tmp <- tmp[order(-tmp$Freq),]
    data$Region <- factor(data$Region, 
                               levels = as.character(tmp$Var1))
    tmp$col <- NA
    for(i in 1:nrow(tmp)){
      tmp$col[i] <- i_col[names(i_col) == tmp$Var1[i]]
    }
    barplot(height = tmp$Freq, names = tmp$Var1, col = tmp$col, las = 2)
    })
  
  output$plot_done <- renderPlot({
    if(input$checkbox){
      data_done <- data[data$Done == TRUE, ]
    } else{
      data_done <- data[, ]
    }
    tmp <- data.frame(table(data_done$Region))
    tmp <- tmp[order(-tmp$Freq),]
    data_done$Region <- factor(data_done$Region, 
                               levels = as.character(tmp$Var1))
    tmp$col <- NA
    for(i in 1:nrow(tmp)){
      tmp$col[i] <- i_col[names(i_col) == tmp$Var1[i]]
    }
    barplot(height = tmp$Freq, names = tmp$Var1, col = tmp$col, las = 2)
  })
}

shinyApp(ui = ui, server = server)