fluidPage(
    
    # rclipboardSetup(),
    
    # Application title
    titlePanel("Gathering Bigram Statistics"),
    
    # Sidebar with a slider for sample size and action button to get a new sample.
    fluidRow(
        # Put slider and button in the sidebar
        # column(12, 
        #        checkboxInput("order2","Order By Count",value = F),
        #        # checkboxInput("compare_letter","Show Corpus Table",value = F)
        #        ),
        
        column(12,
               h3("Heatmap of Bigram Percents"),
               plotOutput("bigramplot",
                          width = "800px",
                          height = "600px")),
        
        column(12,
               h4(textOutput("bigramscore")),
               p("(This measures how different our message bigram statistics are from the Corpus. Values close to zero indicate similarity with the Corpus in terms of bigram occurence.)")
        ),
        
        column(12,
               h3("Message Bigram Table"),
               checkboxInput("percents","Show Percents",value = F),
               tableOutput("bigramdf")),
        hr(),hr(),
        
        column(12,
               h3("Difference From Corpus Bigram Percents"),
               tableOutput("bigramdiff")
        ),
        # column(12,
        #        h3(" "),
        #        plotOutput("bigramdiff_hm",
        #                   width = "800px",
        #                   height = "600px")
        # )
    )
)