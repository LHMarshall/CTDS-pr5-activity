library(activity)
library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(Distance)

linebreaks <- function(n){HTML(strrep(br(), n))}

gen.boot.vals <- function(rtime, reps){
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  # Set up message
  progress$set(message = "Running bootstrap", value = 0)
  
  #Create storage for values
  boot.vals <- rep(NA, reps)
  
  # Number of times we'll go through the loop
  for (i in 1:reps) {
    # Each time through the loop, add another row of data. This is
    # a stand-in for a long-running computation.
    boot.vals[i] <- unname(fitact(rtime, sample="data", reps=1, show = FALSE)@act[3])
    
    # Increment the progress bar, and update the detail text.
    progress$inc(1/reps, detail = paste("Calulating for rep ", i))
  }
  return(boot.vals)
}


ui <- fluidPage(
  #Application Title
  titlePanel("Temporal Availability Calculations for Analysis of Camera Trap Data"),
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script>
                ")),
  
  tabsetPanel(type = "tabs",
              tabPanel("Data",
                       fileInput("file",
                                 label = "Please select your data file.",
                                 multiple = FALSE,
                                 accept = c(".csv", ".txt")),
                       sidebarLayout(
                         sidebarPanel(
                           # Radio buttons to check data
                           radioButtons("display","Display: ",
                                        c(Summary = "summary", Head = "head")),
                           width = 2
                           
                         ),
                         mainPanel(
                           # Display the data
                           conditionalPanel(
                             condition = "(input.display == 'summary' || input.display == 'head')",
                             verbatimTextOutput("data")
                           ),
                           conditionalPanel(
                             condition = "input.display == 'full'",
                             dataTableOutput("data.full")
                           )
                         )
                       )
                       
              ),
              tabPanel("Pre-processing",
                       "We could provide users with a way to combine time and date columns on this tab. See the accompanying document for possible options. If this was done then the first two options on the Analysis tab would be invisible / disabled. "
              ),
              tabPanel("Anaysis", 
                       "[If there has been no data pre-processing] On this page you will need to identify the column in the dataset containing the date-time information and select the appropriate format from the dropdown menu.",
                       selectInput("time.of.day", "Time of day variable name:",
                                   c("Please select data in data tab" = "none")),
                       #checkboxInput("in.radians", "Already in radians", value = FALSE),
                       selectInput("time.format", "Date time format",
                                   c("%Y/%m/%d %H:%M" = "%Y/%m/%d %H:%M")),
                       "The distribution of the triggering events times now be smoothed, using a kernel smoother. In order to estimate uncertainty in the point estimate of activity proportion the data can be resampled. Please provide the number of replicates. [This could also be applied to the resample values to be downloaded - need to remove box on next tab.]. Once you have supplied the necessary information press 'Run Fit Activity'",
                       textInput("reps", "Replicates:", 100),
                       actionButton("run", "Run Fit Activity"),
                       verbatimTextOutput("results"),
                       plotOutput("hist")
                       ),
              tabPanel("Activity and Bootstrap Values",
                       p("We need to perform one final adjustment to obtain the activity multipliers we need to include in our distance sampling analyses. We need to convert the activity value to the proportion of the camera operation time animals were available to be detected"),
                       fluidRow(
                         column(3, "Operational camera hours out of 24 hours: "),
                         column(3,textInput("op.hours", "", value = 24, width = '50%'))
                       ),
                       br(),
                       "Proportion of day cameras were recording:",
                       textOutput("prop.op.hours", inline = TRUE),
                       linebreaks(2),
                       "Activity rate multiplier:",
                       textOutput("activity.rate", inline = TRUE),
                       linebreaks(2),
                       "Activity rate standard error (SE):",
                       textOutput("activity.rate.se", inline = TRUE),
                       linebreaks(2),
                       #actionButton("boot.gen", "Generate Bootstrap Replicates"),
                       strong("Bootstrap replicates summary"),
                       #textInput("boot.reps", "Replicates: (remove this and use previous page value!)", 100),
                       linebreaks(2),
                       verbatimTextOutput("boot.mult"),
                       linebreaks(1),
                       downloadButton("boot_mults", "Download Bootstrap Replicates")
              )
  )
)


server <- function(input, output){
  
  # DATA
  output$data <- renderPrint({
    file <- input$file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext %in% c("csv", "txt"), "Please upload a csv or txt file"))
    if(ext == "csv"){
      data <- read.csv(file$datapath, header = TRUE)
    }
    if(ext == "txt"){
      data <- read.table(file$datapath, header = TRUE)
    }
    switch(input$display,
           "summary" = summary(data),
           "head" = head(data))
  })
  
  # DATA.FULL
  output$data.full <- renderDataTable({
    file <- input$file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext %in% c("csv", "txt"), "Please upload a csv or txt file"))
    if(ext == "csv"){
      read.csv(file$datapath, header = TRUE)
    }
    if(ext == "txt"){
      read.table(file$datapath, header = TRUE)
    }
  })
  
  # Update calculation options
  observeEvent(input$file, {
    file <- input$file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext %in% c("csv", "txt"), "Please upload a csv or txt file"))
    if(ext == "csv"){
      actdata <<- read.csv(file$datapath, header = TRUE)
    }
    if(ext == "txt"){
      actdata <<- read.table(file$datapath, header = TRUE)
    }
    dnames <- names(actdata)
    names(dnames) <- dnames
    updateSelectInput(inputId = "time.of.day", 
                      choices = dnames)
  }) 
  
  # Run the analysis
  rtime <- reactive(gettime(actdata[[input$time.of.day]], input$time.format))
  act_result <- reactive(fitact(rtime(), sample="data", reps=1, show = FALSE))
  # Generate the bootstrap replicates
  boot.mults <- reactive(gen.boot.vals(rtime(), as.numeric(input$reps)))
  act_se <- reactive(sd(boot.mults()))
  
  # Get activity results
  output$results <- renderPrint({
    if(input$run){
      point.result <- try(act_result()@act)
      boot.results <- try(boot.mults())
    }
    if(class(point.result) != "try-error" && class(boot.results) != "try-error"){
      point.result$se <- act_se()
      point.result[3] <- quantile(boot.results, prob = 0.025)
      point.result[4] <- quantile(boot.results, prob = 0.975)
      print(point.result)
    }else{
      print('Not yet run / error')
    }
  })
  
  # Plot activity results
  output$hist <- renderPlot({
    if(input$run){
      plot(act_result(), cline = list(col = NULL))
    }
  })
  
  prop.camera <- reactive(as.numeric(input$op.hours)/24)

  output$prop.op.hours <- renderPrint({
    cat(prop.camera())
  })
  
  output$activity.rate <- renderPrint({
    act.rate <- try(act_result()@act[1]/prop.camera(), silent = TRUE)
    if(class(act.rate) == "try-error"){
      cat("Please run an analysis")
    }else{
      cat(act.rate)
    }
  })
  
  output$activity.rate.se <- renderPrint({
    act.rate.se <- try(act_se()/prop.camera(), silent = TRUE)
    if(class(act.rate.se) == "try-error"){
      cat("Please run an analysis")
    }else{
      cat(act.rate.se)
    }
  })
  
  # Get activity results
  output$boot.mult <- renderPrint({
    if(input$run){
      boot.vals <- try(boot.mults()/prop.camera(), silent = TRUE)
    }
    if(class(boot.vals) != "try-error"){
      print(summary(boot.vals))  
    }else{
      print("Please run an analysis")
    }
  })
  
  download.data <- reactive(data.frame(boot.reps = boot.mults()/prop.camera()))
  
  # Downloadable csv of selected dataset ----
  output$boot_mults <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(download.data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)


