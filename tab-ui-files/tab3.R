fluidPage(
    
    # rclipboardSetup(),
    
    # Application title
    titlePanel("Index of Coincidence"),
    
    # Sidebar with a slider for sample size and action button to get a new sample.
    fluidRow(
        # Put slider and button in the sidebar
        column(12, 
               numericInput("period","How many periods should we consider?",
                            min = 3, max = 21, value = 10)
               # checkboxInput("compare_letter","Show Corpus Table",value = F)
               ),
        
        column(12,
               h4(textOutput("enough_message")),
               plotOutput("iocplot")
               ),
        
        column(6,
               # h3("Message Letter Counts"),
               # tableOutput("letterdf")
               ),
        column(6,
               # h3("Corpus Letter Percents"),
               # tableOutput("corpusletter")
        ),
    )
)