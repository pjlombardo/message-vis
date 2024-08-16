fluidPage(
    
    # rclipboardSetup(),
    
    # Application title
    titlePanel("Gathering Letter Statistics"),
    
    # Sidebar with a slider for sample size and action button to get a new sample.
    fluidRow(
        # Put slider and button in the sidebar
        column(12, checkboxInput("order1","Order By Count",value = F),
               checkboxInput("compare_letter","Show Corpus Table",value = F)),
        
        column(12,
               plotOutput("letterplot")),
        
        column(12,
               h4(textOutput("letterscore")),
               p("(This measures how different our message letter statistics are from the Corpus. Values close to zero indicate similarity with the Corpus in terms of letter occurence.)")
               ),
        
        column(6,
               h3("Message Letter Counts"),
               tableOutput("letterdf")),
        column(6,
               h3("Corpus Letter Percents"),
               tableOutput("corpusletter")
        ),
    )
)