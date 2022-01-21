library(shiny)

ui <- fluidPage(
  #Application Title
  titlePanel("Temporal Availability Calculations for Analysis of Camera Trap Data"),
  
  fileInput("file",
            label = "Select data",
            multiple = FALSE,
            accept = c(".csv", ".txt")),
  
  sidebarLayout(
    sidebarPanel(
      # Radio buttons to check data
      radioButtons("display","Display: ",
                   c(Summary = "summary", Head = "head", Full = "full")),
      
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
}

shinyApp(ui = ui, server = server)


