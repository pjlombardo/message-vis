library(tidyverse)
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(kableExtra)
# library(rclipboard)

source('functions.R')

server <- function(input, output) {
    
    # # Initial outputs:
    
    #tab 1
    output$letterdf<-renderTable(letter_tbl,
                                 digits = 3)
    
    output$corpusletter <-renderTable(NULL)
    
    output$letterplot<-renderPlot(
        letter_plot("abcd")
    )
    output$letterscore<-renderText(
        letter_score_txt("abcd")
    )
    
    #tab 2
    output$bigramplot<-renderPlot(
        make_heatmap("abcd")
    )
    
    output$bigramdf <- renderTable(
        bigram_table("abcd", frequency=T) %>% 
            rownames_to_column(var=" "),
        digits = 0
    )
    
    output$bigramdiff <-renderTable(
        (bigram_table("abcd",frequency = F) - bgtruth) %>%
            rownames_to_column(var = " "),
        digits = 4
    )
    output$bigramscore<-renderText(
        bigram_score_txt("abcd")
    )

    # 
    # output$bigramdiff_hm <-renderPlot(
    #     (bigram_table("abcd",frequency = F) - bgtruth) %>%
    #         make_heatmap()
    # )
    
    # tab3
    output$enough_message<-renderText("The message must be longer to have a meaningful plot.")
    output$iocplot<-renderPlot(
        ggplot()+theme_bw()
    )
    
   
    # # tab 1: single sample
    uletter_tbl<-reactiveValues(data = letter_tbl)
    umessage<-reactiveValues(data = "abcd")

    observeEvent(c(input$compare_letter,input$order1),{
        if (input$compare_letter){
            output$corpusletter <-renderTable(
                corpus_letter_table(input$order1)
            )
        } else {
            output$corpusletter <-renderTable(NULL)
        }
    })
    
    observeEvent(c(input$submit, input$order1, input$percents),{
        umessage<-reactiveValues(data = input$message)
        if (input$message == ""){
            #t1
            uletter_tbl$data <- NULL
            output$letterdf<-renderTable(uletter_tbl$data)
            output$letterplot<-renderPlot(
                ggplot()+theme_bw()
            )
            output$letterscore<-renderText(
                "Letter Score = ...."
            )
            #t2
            output$bigramplot<-renderPlot(
                ggplot()+theme_bw()
            )
            output$bigramdf <- renderTable(NULL)
            output$bigramdiff<-renderTable(NULL)
            output$bigramscore<-renderText(
                "Bigram Score = ...."
            )
            
            #t3
            output$iocplot<-renderPlot(
                ggplot()+theme_bw()
            )
            output$enough_message<-renderText("The message must be longer to have a meaningful plot.")
            
        } else {
            #t1
            uletter_tbl$data <- tally_letters(input$message, order = input$order1)
            output$letterdf<-renderTable(uletter_tbl$data,
                                         digits = 3)
            output$letterplot<-renderPlot(
                letter_plot(umessage$data, input$order1)
            )
            output$letterscore<-renderText(
                letter_score_txt(umessage$data)
            )
            #t2
            output$bigramplot<-renderPlot(
                make_heatmap(umessage$data)
            )
            output$bigramdf <- renderTable(
                bigram_table(umessage$data, !input$percents) %>%
                    rownames_to_column(var = " "),
                digits = digits_fxn(input$percents)
            )
            output$bigramdiff <-renderTable(
                (bigram_table(umessage$data,frequency = F) - bgtruth) %>%
                    rownames_to_column(var = " "),
                digits = 4
            )
            output$bigramscore<-renderText(
                bigram_score_txt(umessage$data)
            )
            #t3
            if (check_iocplot(umessage$data)>2*input$period){
                output$iocplot<-renderPlot(
                    IOC_by_period_plot(umessage$data, input$period)
                )
                output$enough_message<-renderText(NULL)
            } else {
                output$iocplot<-renderPlot(
                    ggplot()+theme_bw()
                )
                output$enough_message<-renderText("The message must be longer to have a meaningful plot.")
            }
            # output$bigramdiff_hm <-renderPlot(
            #     (bigram_table(umessage$data,frequency = F) - bgtruth) %>%
            #         make_heatmap()
            # )
            
        }
    })
    
    
    # observeEvent(input$num_sims1, {
    #     utbl$data<-data.frame(color=NA) %>% kable("html",col.names="Jelly Color",
    #                                               row.names =FALSE,
    #                                               align = "c")
    #     u_summ$data<-data.frame(summary=NA)
    #     
    # })
    # 
    # observeEvent(c(input$new_sim1), {
    #     # update reactive values
    #     udf$data<-get_sample(input$num_sims1)
    #     utbl$data<-get_sample_tbl(udf$data)
    #     u_summ$data <-get_sample_summ(udf$data)
    # 
    #     #update outputs
    #     output$jelly_sample <-function(){
    #         utbl$data
    #     }
    #     
    #     output$sample_sum <-renderTable(
    #         u_summ$data, digits = 3
    #     )
    #     
    # 
    # })
    # 
    # 
    # 
    # 
    # # tab 2: sampling dist
    # #
    # u_sim_summ<-reactiveValues(data = history)
    # counter<-reactiveValues(data = 0)
    # uhistory<-reactiveValues(data = NULL)
    # usim_summ<-reactiveValues(data = get_sim_summ(history))
    # 
    # observeEvent(input$num_sims2, {
    #     counter$data<-0
    #     uhistory$data<-NULL
    #     
    #     output$history<-renderTable(
    #         data.frame(history = NA)
    #     )
    #     
    #     output$sim_summ<-renderTable(
    #         data.frame(summaries=NA),
    #         digits=3
    #     )
    #     
    #     
    #     output$hist_percents<-renderPlot(
    #         ggplot()+theme_bw()
    #     )
    # })
    # 
    # observeEvent(input$new_sim2, {
    #     counter$data<-counter$data+1
    #     uhistory$data<-c(uhistory$data, mean(rbinom(input$num_sims2,1,0.2123)))
    #     
    #     output$history<-renderTable(
    #         data.frame(
    #             "Sample Number" = paste("Sample",1:counter$data),
    #             "Percent Green" = uhistory$data
    #         ),
    #         digits = 3
    #     )
    #     
    #     
    #     output$sim_summ<-renderTable(
    #         get_sim_summ(uhistory$data),
    #         digits=3
    #     )
    #     
    #     output$hist_percents<-renderPlot(
    #         generate_plot(uhistory$data, counter$data, input$binsize)
    #     )
    #     
    # })
    # 
    # 
    # observeEvent(input$new_sim2_100, {
    #     counter$data<-counter$data+100
    #     uhistory$data<-c(uhistory$data, rbinom(100,input$num_sims2,0.2123)/input$num_sims2)
    #     
    #     output$history<-renderTable(
    #         data.frame(
    #             "Sample Number" = paste("Sample",1:counter$data),
    #             "Percent Green" = uhistory$data
    #         ),
    #         digits = 3
    #     )
    #     
    #     
    #     output$sim_summ<-renderTable(
    #         get_sim_summ(uhistory$data),
    #         digits=3
    #     )
    #     
    #     output$hist_percents<-renderPlot(
    #         generate_plot(uhistory$data, counter$data, input$binsize)
    #     )
    # 
    # })
    # 
    # observeEvent(input$binsize, {
    #     if (counter$data !=0){
    #         output$hist_percents<-renderPlot(
    #             generate_plot(uhistory$data, counter$data, input$binsize)
    #         )
    #     }
    # })

    # 
}