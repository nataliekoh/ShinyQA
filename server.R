library(shiny); library(DT); library(shinyjs)
library(dplyr); library(digest); library(knitr)

function(input, output, session) {
  
  observe({
    mandatoryFilled <- vapply(fieldsMandatory, function(x) {
      !is.null(input[[x]]) && input[[x]] != ""},
      logical(1))
    mandatoryFilled <- all(mandatoryFilled)
  })
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv", humanTime(), digest::digest(data))
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  observeEvent(input$submit, {saveData(formData())})
  
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit("/mnt/home/natkoh/shinyQA/parrec.Rmd", 
                                       quiet = TRUE)))
  })
  
  output$downloadData <- downloadHandler(filename = function() {
    sprintf("RS_fMRI_QA_logsheet.csv", humanTime())
  },
  content = function(file) {
    write.csv(loadData(), file, row.names = FALSE)
  })
  
  output$value <- renderPrint({ input$radio })
  # You can access the value of the widget with input$text, e.g.
  output$comment <- renderText({ paste("Your comments are:", input$Comments) })
  
}