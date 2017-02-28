library(shiny); library(DT); library(shinyjs)
#library(dplyr); library(digest)

subjects <- read.csv("all_subjects")

function(input, output, session) {
  
  observe({
    mandatoryFilled <- vapply(fieldsMandatory, function(x) {
      !is.null(input[[x]]) && input[[x]] != ""},
      logical(1))
    mandatoryFilled <- all(mandatoryFilleds)
  })
  
  output$value <- renderPrint({ input$radio })
  # You can access the value of the widget with input$text, e.g.
  output$comment <- renderText({ paste("Your comments are:", input$Comments) })
  
}