

# library(tidyverse)
# library(RColorBrewer)
# read bigrams in 
bgtruth<-read.csv('data/bigram_percents.csv', header = T)
bgtruth <- bgtruth %>% column_to_rownames(var = "X")
colnames(bgtruth)[1]<-"-"
bgtruth %>% head()

digits_fxn<-function(x){
    if (x){
        return(3)
    } else {
        return(0)
    }
}
# read letter frequencies
lettertruth<-read.csv('data/letter_percents.csv',header = T)

message1 <- "She walks in beauty, like the night
Of cloudless climes and starry skies;
And all that's best of dark and bright
Meet in her aspect and her eyes;
Thus mellowed to that tender light
Which heaven to gaudy day denies.

One shade the more, one ray the less,
Had half impaired the nameless grace
Which waves in every raven tress,
Or softly lightens o'er her face;
Where thoughts serenely sweet express,
How pure, how dear their dwelling-place.

And on that cheek, and o'er that brow,
So soft, so calm, yet eloquent,
The smiles that win, the   tints that glow,
But tell of days in goodness spent,
A mind at peace with all below,
A heart whose love is innocent!"


clean_message<-function(x){
    temp <-tolower(x)
    temp <-gsub("\n"," ",temp)
    temp <-gsub(" ","A",temp)
    temp <- gsub("\\W","",temp)
    temp <-gsub("AAAAAA","A",temp)
    temp <-gsub("AAAAA","A",temp)
    temp <-gsub("AAAA","A",temp)
    temp <-gsub("AAA","A",temp)
    temp <-gsub("AA","A",temp)
    temp <-gsub("A","-",temp)
    return(paste("-",temp,"-",sep=""))
}

tally_letters<-function(message, order = F){
    message = clean_message(message)
    message = gsub("-","",message)
    split_message <- strsplit(message,split="")[[1]]
    # print(split_message)
    n<-length(split_message)
    df = data.frame(table(split_message))
    colnames(df) <- c("letters", "frequency")
    # return(df)
    opts<-strsplit("abcdefghijklmnopqrstuvwxyz","")[[1]]
    # 
    all<-sapply(opts, function(x){
        if (sum(df$letters==x)!=0){
            return(df %>% filter(letters == x) %>% pull(frequency))
        } else{
            return(0)
        }
    })
    df2<- data.frame(letters = opts,
                     frequency = all)
    df2$percents <- round(df2$frequency / sum(df$frequency),6)
    # df2 <- df2 %>% filter(letters !='-')
    if (order){
        return(df2 %>% arrange(desc(frequency)))
    } else {
        return(df2)
    }
}
letter_tbl<-tally_letters("abcd")

corpus_letter_table<-function(order = F){
    if (order){
        return(lettertruth %>% arrange(desc(percents)))
    } else{
        return(lettertruth)
    }
}


letter_plot<-function(message,order=F){
    if (order){
        t<-tally_letters(message, order = T)
        levs<-as.character(t$letters)
        # print(levs)
        t<-t %>% 
            mutate(letters = factor(letters, levels = levs))
        # print(t)
        # print(t$letters)
        l<- lettertruth %>% arrange(desc(percents))
        levs2 <- as.character(l$letters)
        l<-l %>% 
            mutate(letters = factor(letters, levels = levs2))
        ggplot()+
            geom_col(data = data.frame(x = 1:26-.2,
                                       y = l$percents), aes(x = x, y = y,fill = "mode1"),
                     position = "dodge", alpha = .7, width = .5)+
            geom_col(data = data.frame(x = t$letters,
                                       y = t$percents), aes(x = x, y = y,fill = "mode2"),
                     alpha = .7, width = .5)+
            scale_x_discrete(labels = levs)+
            scale_fill_manual(name = "",
                              labels = c("Corpus", "Message"),
                              values = c('gray','red'))+
            labs(x = "Letters in the Message",
                 y = "Occurence Percentage",
                 title = "Comparing Occurrence Percentage in a Message against a Corpus")+
            theme_bw()+
            theme(axis.text.x=element_text(size=18),
                  axis.title=element_text(size=14,face="bold"))
    } else {
        t<-tally_letters(message)
        l<- lettertruth
        ggplot()+
            geom_col(data = data.frame(x = 1:26-.2,
                                       y = l$percents), aes(x = x, y = y,fill = "mode1"),
                     position = "dodge", alpha = .7, width = .5)+
            geom_col(data = data.frame(x = t$letters,
                                       y = t$percents), aes(x = x, y = y,fill = "mode2"),
                     alpha = .7, width = .5)+
            scale_x_discrete(labels = l$letters)+
            scale_fill_manual(name = "",
                              labels = c("Corpus", "Message"),
                              values = c('gray','red'))+
            labs(x = "Letters in the Message",
                 y = "Occurence Percentage",
                 title = "Comparing Occurrence Percentage in a Message against a Corpus")+
            theme_bw()+
            theme(axis.text.x=element_text(size=18),
                  axis.title=element_text(size=14,face="bold"))
    }
    
}
# letter_plot(message1,order =F)

letter_comparison_score<-function(message){
    df = tally_letters(message)
    return(sum(abs(df$percents - lettertruth$percents)))
}

# letter_comparison_score(message1)
letter_score_txt<-function(message){
    return(
        paste("Letter Score = ",
              round(letter_comparison_score(message),4))
    )
}
# letter_score_txt(message1)
### Bigrams


tally_bigrams<-function(message){
    split_message <- strsplit(clean_message(message),split="")[[1]]
    n<-length(split_message)
    bigrams = c()
    for (i in 1:(n-1)){
        bigrams <- c(paste(split_message[i],split_message[i+1],sep=""),
                     bigrams)
    }
    return(data.frame(table(bigrams)))
}



get_bigram_col<-function(letter,tally){
    end_pattern <- paste0(letter,"$")
    searched <- subset(tally, grepl(end_pattern,tally$bigrams), drop=FALSE)
    # # this drop = FALSE is critical for getting the right counts!
    # # print(searched)
    vec <- sapply(strsplit("-abcdefghijklmnopqrstuvwxyz","")[[1]],
                  function(x){
                      s1<- paste("^",x,sep="")
                      # print(s1)
                      d<- subset(searched, grepl(s1,searched$bigrams),drop = F)
                      len<-dim(d)[1]
                      # print(l1)
                      if (len!=0){
                          return(d[,2])
                      } else {
                          return(0)
                      }
                  })
    return(as.numeric(vec))
}


bigram_table<-function(message,frequency = T){
    btbl<-tally_bigrams(message)
    # print(btbl)
    labs<-strsplit("-abcdefghijklmnopqrstuvwxyz","")[[1]]
    s1<-get_bigram_col("-",btbl)
    start<-cbind(get_bigram_col("-",btbl),
                 get_bigram_col("a",btbl))
    for (i in 3:27){
        start <- cbind(start,
                       get_bigram_col(labs[i],btbl))
    }
    df = data.frame(start)
    colnames(df)<-labs
    rownames(df)<-labs
    if (frequency){
        return(df)
    } else {
        return(round(df/sum(df),6))
    }
}

# bigram_table(message1) %>% rownames_to_column(var = " ")
make_heatmap<-function(message){
    df<-bigram_table(message, frequency = F)
    df %>% 
        rownames_to_column(var = "first_letter") %>%
        pivot_longer(cols = -first_letter,
                     names_to = "second_letter",
                     values_to = "percents") %>%
        ggplot(data = .,
               aes(y=first_letter,
                   x = second_letter,
                   fill = percents))+
        geom_tile()+
        scale_x_discrete(position = "top")+
        scale_y_discrete(limits =rev)+
        scale_fill_gradientn("Appearance\nPercentage\nIn Message",
                             colors = brewer.pal(9,name="Reds"),
                             limits = )+
        labs(x = "Second Letter", y="First Letter")+
        theme(axis.text.x=element_text(size=18),
              axis.text.y=element_text(size=18),
              axis.title=element_text(size=14,face="bold"))
}



# bigram_table("abcd")[1:8,1:8]
bigram_corpus_heatmap <- bgtruth %>% 
    rownames_to_column(var = "first_letter") %>%
    pivot_longer(cols = -first_letter,
                 names_to = "second_letter",
                 values_to = "percents") %>%
    ggplot(data = .,
           aes(y=first_letter,
               x = second_letter,
               fill = percents))+
    geom_tile()+
    scale_x_discrete(position = "top")+
    scale_y_discrete(limits =rev)+
    scale_fill_gradientn("Appearance\nPercentage\nIn Message",
                         colors = brewer.pal(9,name="Reds"))+
    labs(x = "Second Letter", y="First Letter")

# compare_hm<-function(bg1, bg2){
#     diff <- bg1 - bg2
#     diff %>% 
#         rownames_to_column(var = "first_letter") %>%
#         pivot_longer(cols = -first_letter,
#                      names_to = "second_letter",
#                      values_to = "percents") %>%
#         ggplot(data = .,
#                aes(y=first_letter,
#                    x = second_letter,
#                    fill = percents))+
#         geom_tile()+
#         scale_x_discrete(position = "top")+
#         scale_y_discrete(limits =rev)+
#         scale_fill_gradientn("Appearance\nPercentage\nIn Message",
#                              colors = brewer.pal(9,name="Spectral"))+
#         labs(x = "Second Letter", y="First Letter")
# }
# 
# compare_hm(bigram_table(message1,frequency = F),
#            bgtruth)

bigram_comparison_score<-function(message){
    tbl1<-as.matrix(bigram_table(message,frequency = F))
    tbl2<-as.matrix(bgtruth)
    diff<-abs(tbl1 - tbl2)
    # print(diff)
    return(sum(diff))
}
# bigram_comparison_score(message1)
bigram_score_txt<-function(message){
    return(
        paste("Bigram Score = ",
              round(bigram_comparison_score(message),4))
    )
}
# bigram_score_txt(message1)


#IOC stuff

break_by_period<-function(message, period =1){
    message <- clean_message(message)
    message <- gsub("-","",message)
    message<-strsplit(message,"")[[1]]
    # print(message)
    n<-length(message)
    r<- n%% period
    d<- (n-r)/period
    all <- list()
    for (i in 1:period){
        all[[i]] <- paste0(na.omit(message[i+period*(0:d)]),collapse="")
    }
    return(all)
}

index_of_coincidence<-function(message, period = 1){
    if (period ==1){
        df <- tally_letters(message)
        N <- sum(df$frequency)
        IOC <- sum(df$frequency*(df$frequency - 1))
        return(26*IOC/(N*(N-1)))
    } else {
        groups <- break_by_period(message, period)
        iocs<-sapply(1:period, function(x){
            # print(groups[[x]])
            dft<-tally_letters(groups[[x]][1])
            # print(dft)
            N <- sum(dft$frequency)
            IOC <- sum(dft$frequency*(dft$frequency - 1))
            return(26*IOC/(N*(N-1)))
        })
        # print(iocs)
        return(mean(iocs))
    }
}

IOC_by_period<-function(message, length = 5){
    df = data.frame(
        period = 1:length,
        avg_ioc = sapply(1:length,function(x){
            index_of_coincidence(message, period = x)
        })
    )
    return(df)
}

IOC_by_period_plot<-function(message, length=5){
    df = IOC_by_period(message, length)
    ggplot(data = df,
           aes(x = period,y = avg_ioc))+
        geom_col(fill='green4', color='white')+
        scale_x_continuous(breaks = 1:length)+
        theme_bw()+
        labs(x = "Period",
             y = "Average IOC",
             title = "Average Index of Coincidence (IOC) by Period")
}

IOC_by_period_plot("abcdefghijklmnopqrstuvwx")
check_iocplot<-function(message){
    message <- clean_message(message)
    message <- gsub("-","",message)
    sm <- strsplit(message,"")[[1]]
    return(length(sm))
}
check_iocplot("abcd")
# # testing
# tally_letters(message1, order = F)
# tally_letters(message1, order = T)
# index_of_coincidence(message1, period = 1)
# index_of_coincidence(message1, period = 2)
# index_of_coincidence(message1, period = 3)
# IOC_by_period(message1)
# IOC_by_period_plot(message1)



