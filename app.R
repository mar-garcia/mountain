library(shiny)
library(RColorBrewer)
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))

data <- read.csv("muntanya.csv")
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

i_col <- getPalette(length(levels(data$Region)))
names(i_col) <- levels(factor(data$Region))

ui <- navbarPage(
  
  title = "Sortides Muntanya",
  tabPanel(
    "Table",
    
    # *Input() functions
    
    #### Create a new Row in the UI for selectInputs
    fluidRow(
      column(3, 
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
      column(3,
             selectInput(
               inputId = "month",
               label = "Month:",
               choices = c("All",
                           "January", "February", "March", "April",
                           "May", "June", "July", "August",
                           "September", "October", "November", "December"
                           ))),
      column(2,
             checkboxGroupInput(
               inputId = "checkbox",
               label = "Done",
               choices = list("yes" = 1, "no" = 0, "standby" = 2),
               selected = c(0,1,2)))
    ),
    
    fluidRow(
      column(3,
             sliderInput(
               inputId = "distance",
               label = "Distance",
               min = min(data$Distance, na.rm = TRUE),
               max = max(data$Distance, na.rm = TRUE),
               value = c(min(data$Distance, na.rm = TRUE), 
                         max(data$Distance, na.rm = TRUE))
             )),
      column(3,
             sliderInput(
               inputId = "desnivell",
               label = "Desnivell",
               min = min(data$Denivell, na.rm = TRUE),
               max = max(data$Denivell, na.rm = TRUE),
               value = c(min(data$Denivell, na.rm = TRUE), 
                         max(data$Denivell, na.rm = TRUE))
             )),
      column(3,
             sliderInput(
               inputId = "altitudine",
               label = "Altitude Max.",
               min = min(data$Altitude, na.rm = TRUE),
               max = max(data$Altitude, na.rm = TRUE),
               value = c(min(data$Altitude, na.rm = TRUE), 
                         max(data$Altitude, na.rm = TRUE))
             )),
      column(3,
             sliderInput(
               inputId = "difficulty",
               label = "Difficulty",
               min = min(data$Difficulty),
               max = max(data$Difficulty), step = 0.5,
               value = c(min(data$Difficulty), max(data$Difficulty))
             ))
    ),
    
    # *Output() functions
    #### Create a new row for the table.
    DT::dataTableOutput("table")
    
  ), # close tabPanel "Table"
  
  ##################################################################
  
  tabPanel(
    "Graphs",
    fluidRow(plotOutput(outputId = "plot_regions")),
    fluidRow(plotOutput(outputId = "plot_type"))
  ),
  
  tabPanel(
    "Details",
    DT::dataTableOutput("table_web")
  ),
  
  ##################################################################
  
  tabPanel(
    "Done",
    fluidRow(
      column(4, 
             selectInput(
               inputId = "typed",
               label = "Type:",
               choices = c("All",
                           unique(as.character(data$Type)))
             )),
      column(4,
             selectInput(
               inputId = "regiond",
               label = "Region:",
               choices = c("All",
                           unique(as.character(data$Region)))
             )),
      column(4, 
             dateRangeInput("dates", label = "Date range", 
                            start = min(data$Date, na.rm = TRUE),
                            end = max(data$Date, na.rm = TRUE))
             )
    ),
    
    fluidRow(DT::dataTableOutput("table_done")),
    fluidRow(
      column(6, plotOutput(outputId = "plotd_regions")),
      column(6, plotOutput(outputId = "plotd_type"))
      )
  )
)


##################################################################
##################################################################
##################################################################


server <- function(input, output){
  
  datax <- reactive({
    data <- read.csv("muntanya.csv")[,-1]
    if(length(input$checkbox) == 1 & input$checkbox == 0){
      data <- data[data$Done == "FALSE", ]
    } else if(length(input$checkbox) == 1 & input$checkbox == 1){
      data <- data[data$Done == "TRUE", ]
    } else if(length(input$checkbox) == 1 & input$checkbox == 2){
      data <- data[data$Done == "STANDBY", ]
    } else if(length(input$checkbox) == 2 & input$checkbox == c("0", "1")){
      data <- data[data$Done == "FALSE" | data$Done == "TRUE", ]
    } else if(length(input$checkbox) == 2 & input$checkbox == c("1", "2")){
      data <- data[data$Done == "TRUE" | data$Done == "STANDBY", ]
    } else if(length(input$checkbox) == 2 & input$checkbox == c("0", "2")){
      data <- data[data$Done == "FALSE" | data$Done == "STANDBY", ]
    } else if(length(input$checkbox) == 3 & input$checkbox == c("0", "1", "2")){
      data <- data
    } 
    if (input$type != "All") {
      data <- data[data$Type == input$type,]
    }
    if (input$region != "All") {
      data <- data[data$Region == input$region,]
    }
    if(input$month != "All"){
      data <- data[grep(substr(input$month, 1, 3), data$Period), ]
    }
    
    data <- data[(data$Distance >= input$distance[1] &
                    data$Distance <= input$distance[2]) |
                   is.na(data$Distance), ]
    data <- data[(data$Denivell >= input$desnivell[1] &
                    data$Denivell <= input$desnivell[2]) |
                   is.na(data$Denivell), ]
    data <- data[(data$Altitude >= input$altitudine[1] &
                    data$Altitude <= input$altitudine[2]) |
                   is.na(data$Altitude), ]
    data <- data[(data$Difficulty >= input$difficulty[1] &
                    data$Difficulty <= input$difficulty[2]), ]
    data
  })
  
  datax_done <- reactive({
    data_done <- read.csv("muntanya.csv", stringsAsFactors = FALSE)[,-1]
    data_done <- data_done[data_done$Done == TRUE, ]
    if (input$typed != "All") {
      data_done <- data_done[data_done$Type == input$typed,]
    }
    if (input$regiond != "All") {
      data_done <- data_done[data_done$Region == input$regiond,]
    }
    data_done$Date <- as.Date(data_done$Date, format = "%d/%m/%Y")
    data_done <- data_done[data_done$Date >= input$dates[1] &
                             data_done$Date <= input$dates[2], ]
    
    data_done
  })
  
  ####################################################################
  
  output$table <- DT::renderDataTable(
    DT::datatable({
      datax()[,-c(7,8:ncol(datax()))]
    }, options = list(pageLength = nrow(datax()), dom = 'Bfrtip'
                      ), rownames= FALSE)
  )
  
  output$plot_regions <- renderPlot({
    tmp <- datax
    tmp <- data.frame(table(tmp()[,6]))
    tmp <- tmp[order(-tmp$Freq),]
    data$Region <- factor(data$Region, 
                          levels = as.character(tmp$Var1))
    tmp$col <- NA
    for(i in 1:nrow(tmp)){
      tmp$col[i] <- i_col[names(i_col) == tmp$Var1[i]]
    }
    barplot(height = tmp$Freq, names = tmp$Var1, col = tmp$col, las = 2)
  })
  
  output$plot_type <- renderPlot({
    tmp <- data.frame(table(datax()[,2]))
    slices <- tmp$Freq
    lbls <- paste0(tmp$Var1, "\n (n=", tmp$Freq, ")")
    pie(slices, lbls)
  })
  
  output$table_web <- DT::renderDataTable(
    DT::datatable({
      datax()[,c(1,8:9,12:14)]
    }, options = list(pageLength = nrow(datax()), dom = 't'), rownames= FALSE)
  )
  
  ####################################################################
  
  output$table_done <- DT::renderDataTable(
    DT::datatable({
      datax_done()[,-c(7:10)]
    }, rownames= FALSE, options = list(dom = 't'))
  )
  
  output$plotd_regions <- renderPlot({
    tmp <- datax_done
    tmp <- data.frame(table(tmp()[,6]))
    tmp <- tmp[order(-tmp$Freq),]
    data$Region <- factor(data$Region, 
                          levels = as.character(tmp$Var1))
    tmp$col <- NA
    for(i in 1:nrow(tmp)){
      tmp$col[i] <- i_col[names(i_col) == tmp$Var1[i]]
    }
    barplot(height = tmp$Freq, names = tmp$Var1, col = tmp$col, las = 2)
  })
  
  output$plotd_type <- renderPlot({
    tmp <- data.frame(table(datax_done()[,2]))
    slices <- tmp$Freq
    lbls <- paste0(tmp$Var1, "\n (n=", tmp$Freq, ")")
    pie(slices, lbls)
  })
}

shinyApp(ui = ui, server = server)