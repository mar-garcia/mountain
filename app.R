library(shiny)
library(RColorBrewer)
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))

data <- read.csv("muntanya.csv")
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

data$Region <- factor(data$Region)
i_col <- getPalette(length(levels(data$Region)))
names(i_col) <- levels(data$Region)

ui <- navbarPage(
  
  title = "Sortides Muntanya",
  tabPanel(
    "Table",
    
    # *Input() functions
    
    #### Create a new Row in the UI for selectInputs
    fluidRow(
      column(2, 
             selectInput(
               inputId = "type",
               label = "Type:",
               choices = c("All",
                           unique(as.character(data$Type)))
             )),
      
      column(2,
             selectInput(
               inputId = "region",
               label = "Region:",
               choices = c("All", "Val di Sole", "Val di Non", 
                           "Giudicarie", "Val di Fassa", "Val di Fiemme", 
                           "Primiero", "Valsugana", "Garda e Ledro", 
                           "Vallagarina", "Val d'Adige", "Oltradige",
                           "Val Venosta", "Merano", "Sciliar", 
                           "Valle Isarco", "Val Pusteria", "Belluno")
             )),
      
      column(2,
             selectInput(
               inputId = "gruppo",
               label = "Gruppo:",
               choices = c("All", unique(as.character(data$Gruppo)))
             )),
      
      column(4,
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
               selected = c(0,1,2))),

    fluidRow(
      column(2,
             sliderInput(
               inputId = "distance",
               label = "Distance",
               min = min(data$Distance, na.rm = TRUE),
               max = max(data$Distance, na.rm = TRUE),
               value = c(min(data$Distance, na.rm = TRUE), 
                         max(data$Distance, na.rm = TRUE))
             )),
      column(2,
             sliderInput(
               inputId = "desnivell",
               label = "Desnivell",
               min = min(data$Denivell, na.rm = TRUE),
               max = max(data$Denivell, na.rm = TRUE),
               value = c(min(data$Denivell, na.rm = TRUE), 
                         max(data$Denivell, na.rm = TRUE))
             )),
      column(2,
             sliderInput(
               inputId = "altitudine",
               label = "Altitude Max.",
               min = min(data$Altitude, na.rm = TRUE),
               max = max(data$Altitude, na.rm = TRUE),
               value = c(min(data$Altitude, na.rm = TRUE), 
                         max(data$Altitude, na.rm = TRUE))
             )),
      column(4,
             sliderInput(
               inputId = "difficulty",
               label = "Difficulty",
               min = min(data$Difficulty),
               max = max(data$Difficulty), step = 0.5,
               value = c(min(data$Difficulty), max(data$Difficulty))
             )),
      
      column(2,
             textInput("text", label = "Search", value = ""))
    )
    ),
    
    # *Output() functions
    #### Create a new row for the table.
    DT::dataTableOutput("table")
    
  ), # close tabPanel "Table"
  
  ##################################################################
  
  tabPanel(
    "Graphs",
      column(8, plotOutput(outputId = "plot_regions")),
      column(4, plotOutput(outputId = "plot_type"))
      ),
  
  tabPanel(
    "Details",
    DT::dataTableOutput("table_web")
  )
)


##################################################################
##################################################################
##################################################################


server <- function(input, output){
  
  datax <- reactive({
    data <- read.csv("muntanya.csv")[,-1]
    if(length(input$text)){
      data <- data[grep(tolower(input$text), tolower(data$Destination)), ]
    }
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
    if (input$gruppo != "All") {
      data <- data[data$Gruppo == input$gruppo,]
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
  
  ####################################################################
  
  output$table <- DT::renderDataTable(
    DT::datatable({
      datax()[,-c(8:ncol(datax()))]
    }, options = list(pageLength = nrow(datax()), dom = "t" #dom = 'Bfrtip'
                      ), rownames= FALSE)
  )
  
  output$plot_regions <- renderPlot({
    tmp <- datax
    tmp <- data.frame(table(factor(tmp()[,6], levels = names(i_col))))
    tmp <- tmp[order(-tmp$Freq),]
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
    par(mar=c(0,1,0,2))
    pie(slices, lbls)
  })
  
  output$table_web <- DT::renderDataTable(
    DT::datatable({
      datax()[,c(1,6:7,9,10,13:15)]
    }, options = list(pageLength = nrow(datax()), dom = 't'), rownames= FALSE)
  )
}

shinyApp(ui = ui, server = server)