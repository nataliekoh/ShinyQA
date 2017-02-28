library(shiny)

subjects <- read.csv("all_subjects")

fieldsMandatory <- c("GoodorBad", "Comments")
fieldsAll <- c("GoodorBad", "Comments")
