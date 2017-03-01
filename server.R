library(shiny); library(DT); library(shinyjs)
library(dplyr); library(digest); library(knitr)

setwd(getwd())

fieldsMandatory <- c("subid", "userid", "parrecQA", "rawmoviesQA", "tsnrQA", "meicaQA", "motionQA", "regQA", "Comments")
fieldsAll <- c("subid","userid", "parrecQA", "rawmoviesQA", "tsnrQA", "meicaQA", "motionQA", "regQA", "Comments")
responsesDir <- "output"
epochTime <- function(){as.integer(Sys.time())}
humanTime <- function(){format(Sys.time(), "%Y%m%d-%H%M%OS")}

saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv", humanTime(), digest::digest(data))
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = FALSE)
}

loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

function(input, output, session) {
  
  #subid
  subject <- reactive({ input$subid })
  output$subject <- renderText({ input$subid })
  output$subviewtext <- renderText({ paste("You are now viewing subject: ", input$subid) })
  
  #paths to all QA images
  output$rawe001x <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e001_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe001y <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e001_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe001z <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e001_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe002x <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e002_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe002y <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e002_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe002z <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e002_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe003x <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e003_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe003y <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e003_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe003z <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e003_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  
  output$meica.tsocx <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_tsoc_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rmeica.tsocy <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_tsoc_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.tsocz <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_tsoc_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.mednx <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_medn_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.medny <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_medn_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.mednz <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_medn_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.mefcx <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_mefc_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.mefcy <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_mefc_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.mefcz <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_mefc_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  
  output$tsocT1 <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_tsoc_reoriented_to_T1.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsocCT <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_tsoc_to_CT_epireg_ants.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsocMNI <- renderImage({ 
    filename <- normalizePath(file.path("./www", paste(input$subid, "/rest-on_e00213_tsoc_reoriented_to_mni_epireg_ants.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  
  #warnings
  output$warnings <- renderUI({
    HTML(markdown::markdownToHTML(knit(file.path("./www", paste(input$subid, "/rest-on_WARNING.Rmd", sep = "")), 
                                       quiet = TRUE)))})
  
  #quantitative measures
  output$quantmeasures <- renderUI({
    HTML(markdown::markdownToHTML(knit(file.path("./www", paste(input$subid, "/rest-on_motion.Rmd", sep = "")), 
                                       quiet = TRUE)))})
  
  #acquisition parameters
  output$acqpar <- renderUI({
    HTML(markdown::markdownToHTML(knit(file.path("./www", paste(input$subid, "/rest-on_parrec.Rmd", sep = "")), 
                                       quiet = TRUE)))})
  
  #form
  observe({
    mandatoryFilled <- vapply(fieldsMandatory, function(x) {
      !is.null(input[[x]]) && input[[x]] != ""},
      logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("submitted_msg")
    }, error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE)
    }, finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
    
    })
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("submitted_msg")
  })
  
  output$responsestable <- DT::renderDataTable(
    loadData(),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )
  #observeEvent(input$submit, {shinyjs::enable("submit")})
  
  #download data
  output$downloadData <- downloadHandler(#filename = "RS_fMRI_QA_logsheet.csv",
  function() {
    filename = sprintf("rest-on_QALogSheet_%.csv", humanTime())
  },
  content = function(file) {
    write.csv(loadData(), file, row.names = FALSE)
  })
  
}