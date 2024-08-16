ui<-fluidPage(
    h1("Gathering Statistics for a Message"),br(),br(),
    textInput("message", "Place Message Below", "abcd",width = "90%"),br(),
    actionButton("submit","Run Analysis!"),br(),br(),hr(),hr(),
    tabsetPanel(type = "tabs",
                tabPanel("Letter Statistics",
                         br(),
                         source('tab-ui-files/tab1.R',local=TRUE)$value),
                tabPanel("Bigram Statistics",
                         br(),
                         source('tab-ui-files/tab2.R',local=TRUE)$value),
                tabPanel("Index of Coincidence (IOC)",
                         br(),
                         source('tab-ui-files/tab3.R',local=TRUE)$value)
    ),
    hr(),
    div(h4("Created by P. Lombardo"), style = "float:right")
)
