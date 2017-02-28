
shinyUI(fluidPage(
  titlePanel("Resting State fMRI QA"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("subid", 
                  label = "Select Subject ID",
                  choices = list("100044")),
      selectInput("rs_var", 
                  label = "Select QA Measure",
                  choices = list("T1 Skullstrip",#this should be in structuralQA 
                                 "Acquisition Parameters",
                                 "Raw Data Movies",
                                 "TSNR Images",
                                 "MEICA Movies",
                                 "Motion Parameters",
                                 "Registrations")),
      div(
        id = "form",
        textInput("userid", label = h4("Your UW NetID"), value = ""),
        radioButtons("GoodorBad", label = h4("How does the data look?"),
                     choices = list("Good" = 1, 
                                    "Bad" = 2),
                     selected = 1),
        textInput("Comments", label = h4("Comments"), value = "Enter text..."),
        actionButton("submit", "Submit", class = "btn-primary")
      ),
      br(),
      downloadButton("downloadData", "Download QA Logsheet")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 p("This subject has been flagged for"),
                 uiOutput("markdown")),
        tabPanel("Acquisition Parameters",
        textOutput("comment"),
        p("This subject has been flagged for"), #VARNAME),
        h3("T1 Brain Skullstrip"),
        img(src="T1_brain.gif", height = 400, width = 400)),
        tabPanel("Raw Data Movies",
                 p("sometext")),
        tabPanel("MEICA Movies",
                 p("sometext"))
      )
  )
)))
