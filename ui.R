
shinyUI(fluidPage(
  titlePanel(h2("Resting State fMRI QA")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("subid", 
                  label = "Select Subject ID",
                  choices = list("100044", "100054")),
      #selectInput("rs_var", 
       #           label = "Select QA Measure",
        #          choices = list("T1 Skullstrip",#this should be in structuralQA 
         #                        "Acquisition Parameters",
          #                       "Raw Data Movies",
           #                      "TSNR Images",
            #                     "MEICA Movies",
             #                    "Motion Parameters",
              #                   "Registrations")),
      div(
        id = "form",
        textInput("userid", label = h4("Your UW NetID"), value = ""),
        radioButtons("parrecQA", label = h4("Acquisition Parameters"),
                     choices = list("Good" = 1, 
                                    "Bad" = 2)),
        radioButtons("rawmoviesQA", label = h4("Raw Movies"),
                     choices = list("Good" = 1, 
                                    "Bad" = 2)),
        radioButtons("tsnrQA", label = h4("TSNR Images"),
                     choices = list("Good" = 1, 
                                    "Bad" = 2)),
        radioButtons("meicaQA", label = h4("MEICA Output"),
                     choices = list("Good" = 1, 
                                    "Bad" = 2)),
        radioButtons("motionQA", label = h4("Motion Metrics"),
                     choices = list("Good" = 1, 
                                    "Bad" = 2)),
        radioButtons("regQA", label = h4("Registrations"),
                     choices = list("Good" = 1, 
                                    "Bad" = 2)),
        textInput("Comments", label = h4("Overall Comments"), value = "Enter text..."),
        actionButton("submit", "Submit", class = "btn-primary")
      ),
      br(),
      downloadButton("downloadData", "Download QA Logsheet")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 h3(textOutput("subviewtext", inline = TRUE)),
                 div("This subject has been flagged for", style = "color:red"),
                 uiOutput("markdown")),
        tabPanel("Acquisition Parameters",
        textOutput("comment"),
        h3("T1 Brain Skullstrip"),
        img(src="100044/T1_brain.gif", height = 400, width = 400)),
        tabPanel("Raw Data Movies",
                 p("sometext")),
        tabPanel("MEICA Movies",
                 p("sometext"))
      )
  )
)))
