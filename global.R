library(shiny)

subjects <- read.csv("all_subjects")

fieldsMandatory <- c("userid", "GoodorBad", "Comments")
fieldsAll <- c("userid", "GoodorBad", "Comments")
responsesDir <- file.path("/mnt/home/natkoh/ShinyQA/output")
epochTime <- function(){as.integer(Sys.time())}
humanTime <- function(){format(Sys.time(), "%Y%m%d-%H%M%OS")}

loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- dplyr::rbind_all(data)
  data
}