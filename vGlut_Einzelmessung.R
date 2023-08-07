library(shiny)
library(shinythemes)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
      tags$style(HTML('#fluidrow1 {
               display: flex;
         align-items: center;
         }
                      #fluidrow2 {
               display: flex;
         align-items: center;
         }')),
      
    # Application title
    titlePanel("vGlut Auswertung"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          # Browse for file
          fileInput("file", "Choose .tif to analyze", multiple = FALSE, accept = NULL, width = NULL),
          selectInput("dropdown", "Select condition", "no file loaded yet"),
          fluidRow(column(8, textInput("new_condition", "Or enter new condition")),
                   column(4, actionButton("add_condition", "Add"))),
          tags$style(type='text/css', "#add_condition { width:100%; margin-top: 25px;}")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(id = "fluidrow1", 
                   column(2, h3("Status:")),
                   column(10, align = "left", textOutput("text_msg_oben"))),
          fluidRow(column(3, # column divided into quarters
                          plotOutput("plot1")),
                   column(3,
                          plotOutput("plot2")),
                   column(3, 
                          plotOutput("plot3")),
                   column(3,
                          plotOutput("plot4"))
          )
          )
        ),
    hr(),
    fluidRow(column(6, align = "center",
                    plotOutput("hist_red"),
                    sliderInput("range_red", "Select Range for red channel (MAP2)", min=0, max = 1, value = c(0.05,1), step = 1/256, width = "90%")),
             column(6, plotOutput("plot_red_range"))),
    fluidRow(column(6, align = "center",
                    plotOutput("hist_green"),
                    sliderInput("range_green", "Select Range for green channel (vGlut)", min=0, max = 1, value = c(0.05,1), step = 1/256, width = "90%")),
             column(6, plotOutput("plot_green_range"))),
    hr(),
    fluidRow(id = "fluidrow2",
             column(3, actionButton("button_save", "Save", width = "200px")),
             column(2, h3("Status:")),
             column(7, align = "left", textOutput("text_msg_unten")))
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
##############################################
  library(tidyverse)
  library(EBImage)
  
  #initialize status
  #output$text_status <- renderText("No file selected yet.")
  
  # Render rgb plot tiles upon file selection
  observeEvent(input$file, {
    #load image
    rawimage <<- readImage(input$file$datapath)
    current_filename <<- input$file$name
    # load conditions list
    old_conditions <<- NULL
    if (file.exists("condition_choices.rds")){
      old_conditions <<- readRDS(file = "condition_choices.rds")}
    updateSelectInput(getDefaultReactiveDomain(), "dropdown",
                      label = "Select condition",
                      choices = old_conditions)
    })
  output$plot1 <- renderPlot({
    input$file
    rgbr <<- rgbImage(red = channel(rawimage,"r"))
    plot(rgbr)
  })
  output$plot2 <- renderPlot({
    input$file
    rgbg <<- rgbImage(green = channel(rawimage,"g"))
    plot(rgbg)
  })
  output$plot3 <- renderPlot({
    input$file
    rgbb <- rgbImage(blue = channel(rawimage,"b"))
    plot(rgbb)
  })
  output$plot4 <- renderPlot({
    input$file
    plot(rawimage)
  })
  observeEvent(input$file, {status_msg$msg = paste("File", input$file$name, "selected - Choose analysis parameter and save.")})
  
  # Generate status messages
  status_msg <- reactiveValues(msg = "Choose a file to analyze.")
  output$text_msg_oben <- renderText(paste(status_msg$msg))
  output$text_msg_unten <- renderText(paste(status_msg$msg))
  
  # Add entered condition as choice upon button press
  observeEvent(input$add_condition, {
    new_condition <- input$new_condition
    condition_choices <<- append(old_conditions, new_condition)
    saveRDS(condition_choices, file = "condition_choices.rds")
    updateSelectInput(getDefaultReactiveDomain(), "dropdown",
                      label = "Select condition",
                      choices = condition_choices,
                      selected = new_condition)
  })
  
  # plot the histograms and black-and-"white" images
  output$hist_red <- renderPlot({
    input$file
    red <- imageData(rawimage)[,,1]
    red_data <- data.frame(red = as.numeric(paste(red)))
    red_breaks <- seq(from = 0, to = 1, length.out = 256)
    ggplot(red_data, aes(x = red)) + geom_histogram(data = red_data, breaks = red_breaks, color = "grey60", fill = "#ffcccb") +
      geom_histogram(data = subset(red_data, ((red >= input$range_red[1]) & (red <= input$range_red[2]))), breaks = red_breaks, color = "black", fill = "red") + 
      geom_vline(xintercept = input$range_red, color = "red", size = 1.5)
    })
  output$hist_green <- renderPlot({
    input$file
    green <- imageData(rawimage)[,,2]
    green_data <- data.frame(green = as.numeric(paste(green)))
    green_breaks <- seq(from = 0, to = 1, length.out = 256)
    ggplot(green_data, aes(x = green)) + geom_histogram(data = green_data, breaks = green_breaks, color = "grey60", fill = "#d0f0c0") +
      geom_histogram(data = subset(green_data, ((green > input$range_green[1]) & (green < input$range_green[2]))), breaks = green_breaks, color = "black", fill = "#266c35") + 
      geom_vline(xintercept = input$range_green, color = "#266c35", size = 1.5)
  }) 
  output$plot_red_range <- renderPlot({
    input$file
    imgdata <- imageData(rawimage)[,,1]
    imgdata[(imgdata < input$range_red[1]) | (imgdata > input$range_red[2])] <- 0
    imgdata[(imgdata >= input$range_red[1]) & (imgdata <= input$range_red[2])] <- 1
    red_in_range <<- rgbImage(red = imgdata)
    plot(red_in_range)
  })
  output$plot_green_range <- renderPlot({
    input$file
    imgdata <- imageData(rawimage)[,,2]
    imgdata[(imgdata < input$range_green[1]) | (imgdata > input$range_green[2])] <- 0
    imgdata[(imgdata >= input$range_green[1]) & (imgdata <= input$range_green[2])] <- 1
    green_in_range <<- rgbImage(green = imgdata)
    plot(green_in_range)
  })
  
  # Calculate and save results upon button press "Save"
  observeEvent(input$button_save, {
    # load conditions lookup-table ( if exists), add this file and save
    condition_lookup <<- NULL
    if (file.exists("./Results/condition_lookup.rds")){
      condition_lookup <<- readRDS(file = "./Results/condition_lookup.rds")
      if (any(str_detect(condition_lookup[,2], current_filename))) {
        # if the current filename is already in the list
        condition_lookup[str_which(condition_lookup[,2], current_filename),] <- c(input$dropdown, current_filename)
      } else {
        condition_lookup <<- rbind(condition_lookup, c(input$dropdown, current_filename))
      }
    } else {
      condition_lookup <<- rbind(condition_lookup, c(input$dropdown, current_filename))  
    }
    saveRDS(condition_lookup, file = "./Results/condition_lookup.rds")
    # calculate vesicle ration both ways
    reddata <- imageData(rawimage)[,,1]
    reddata[(reddata < input$range_red[1]) | (reddata > input$range_red[2])] <- 0
    redintensity <- sum(reddata)
    greendata <- imageData(rawimage)[,,2]
    greendata[(greendata < input$range_green[1]) | (reddata > input$range_green[2])] <- 0
    greenintensity <- sum(greendata)
    red_area <- sum(red_in_range)
    results <- list()
    results$green_norm_red_int <- greenintensity / redintensity
    results$green_norm_red_area <- greenintensity / red_area
    results$images$rawimage <- rawimage
    results$images$red_in_range <- red_in_range
    results$images$green_in_range <- green_in_range
    # collect meta data: range red, range green, analysis timestamp, condition, filename
    results$parameter$range_red <- input$range_red
    results$parameter$range_green <- input$range_green
    results$file$condition <- input$dropdown
    results$file$filename <- current_filename
    results$timestamp <- c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    # generate filename
    save_filename <- paste("./Results/", substr(current_filename, 1, (nchar(current_filename)-4)), ".rds", sep="")
    # save
    saveRDS(results, file = save_filename)
    # update status message
    observeEvent(input$button_save, {status_msg$msg = paste("File", save_filename, "saved. Choose the next file to analyze or close this window.")})
  })
  
  
##############################################  
    }

# Run the application 
shinyApp(ui = ui, server = server)
