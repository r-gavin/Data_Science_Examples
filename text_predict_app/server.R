#

library(shiny)
library(dplyr)
library(stringr)
library(ngram)

fin_1 <- tbl_df(read.table("fin_1.txt",header = TRUE,stringsAsFactors = FALSE))
fin_2 <- tbl_df(read.table("fin_2.txt",header = TRUE,stringsAsFactors = FALSE))
fin_3 <- tbl_df(read.table("fin_3.txt",header = TRUE,stringsAsFactors = FALSE))

one_num <- function(x1) {
    temp1 <- fin_1[grep(paste0("^",x1),fin_1$ngrams),] %>% select(-freq)
    one_return <- c()
    
    if (nrow(temp1) == 0) {
        one_return <- c()
    } else if (nrow(temp1) == 1) {
        temp1_2 <- fin_2[grep(paste0("^",x1),fin_2$ngrams),] %>% select(-freq)
        if (x1 %in% temp1$ngrams) {
            one_return <- c(temp1_2$ngrams[1:3])
        } else {
            one_return <- c(temp1$ngrams[1],temp1_2$ngrams[1:2])
        }
    } else if (nrow(temp1) == 2) {
        temp1_2 <- fin_2[grep(paste0("^",x1),fin_2$ngrams),] %>% select(-freq)
        if (x1 %in% temp1$ngrams) {
            one_return <- c(setdiff(temp1$ngrams,x1),temp1_2$ngrams[1:2])
        } else {
            one_return <- c(temp1$ngrams[1:2],temp1_2$ngrams[1])
        }
    } else if (nrow(temp1) == 3) {
        temp1_2 <- fin_2[grep(paste0("^",x1),fin_2$ngrams),] %>% select(-freq)
        if (x1 %in% temp1$ngrams) {
            one_return <- c(setdiff(temp1$ngrams,x1),temp1_2$ngrams[1])
        } else {
            one_return <- c(temp1$ngrams)
        }
    } else {
        if (x1 %in% temp1$ngrams[1:3]) {
            one_return <- c(setdiff(temp1$ngrams[1:4],x1))
        } else {
            one_return <- c(temp1$ngrams[1:3])
        }
    }
    
    if (anyNA(one_return)) one_return <- one_return[!is.na(one_return)]
    one_return
}

two_num <- function(x2) {
    end_space_x2 <- grepl(" $",x2)
    temp2 <- fin_2[grep(paste0("^",x2),fin_2$ngrams),] %>% select(-freq)
    two_return <- c()
    
    if (nrow(temp2) == 0) {
        two_return <- paste(
            word(x2,start = 1),one_num(word(x2,start = 2)),
            sep = " ")
    } else if (nrow(temp2) == 1) {
        temp2_3 <- fin_3[grep(paste0("^",x2),fin_3$ngrams),] %>% select(-freq)
        if (x2 %in% temp2$ngrams) {
            two_return <- c(temp2_3$ngrams[1:3])
        } else {
            two_return <- c(temp2$ngrams[1],temp2_3$ngrams[1:2])
        }
    } else if (nrow(temp2) == 2) {
        temp2_3 <- fin_3[grep(paste0("^",x2),fin_3$ngrams),] %>% select(-freq)
        if (x2 %in% temp2$ngrams) {
            two_return <- c(setdiff(temp2$ngrams,x2),temp2_3$ngrams[1:2])
        } else {
            two_return <- c(temp2$ngrams[1:2],temp2_3$ngrams[1])
        }
    } else if (nrow(temp2) == 3) {
        temp2_3 <- fin_3[grep(paste0("^",x2),fin_3$ngrams),] %>% select(-freq)
        if (x2 %in% temp2$ngrams) {
            two_return <- c(setdiff(temp2$ngrams,x2),temp2_3$ngrams[1])
        } else {
            two_return <- c(temp2$ngrams)
        }
    } else {
        if (x2 %in% temp2$ngrams[1:3]) {
            two_return <- c(setdiff(temp2$ngrams[1:4],x2))
        } else {
            two_return <- c(temp2$ngrams[1:3])
        }
    }
    
    if (anyNA(two_return)) two_return <- two_return[!is.na(two_return)]
    two_return
}

thr_num <- function(x3) {
    end_space_x3 <- grepl(" $",x3)
    temp3 <- fin_3[grep(paste0("^",x3),fin_3$ngrams),] %>% select(-freq)
    thr_return <- c()
    
    if (nrow(temp3) == 0) {
        thr_return <- paste(
            word(x3,start = 1),two_num(word(x3,start = 2,end = 3)),
            sep = " ")
    } else if (nrow(temp3) == 1) {
        if (x3 %in% temp3$ngrams) {
            thr_return <- paste(
                word(x3,start = 1),two_num(paste0(word(x3,start = 2,end = 3)," ")),
                sep = " ")
        } else {
            thr_return <- c(temp3$ngrams[1])
        }
    } else {
        if (x3 %in% temp3$ngrams[1:3]) {
            thr_return <- c(setdiff(temp3$ngrams[1:4],x3))
        } else {
            thr_return <- c(temp3$ngrams[1:3])
        }
    }
    
    if (anyNA(thr_return)) thr_return <- thr_return[!is.na(thr_return)]
    thr_return
}

nlpP_model <- function(usr_txt) {
    
    x <- usr_txt
    
    wd_sug <- c()
    end_in_space <- grepl(" $",x)
    
    x <- unlist(strsplit(x,split = " "))
    num_words <- length(x)
    if (num_words>3) x <- x[I(num_words-2):num_words]
    num_words <- length(x)
    
    if (num_words<1) return(wd_sug)
    
    end_punct_i <- 0
    comma_loc_i <- 0
    
    end_punct_i <- seq_len(num_words)[grepl("[.?!]$",x)]
    end_punct_i <- ifelse(length(end_punct_i)<1,0,max(end_punct_i))
    comma_loc_i <- seq_len(num_words)[grepl(",$",x)]
    comma_loc_i <- comma_loc_i[comma_loc_i>end_punct_i]
    if (length(comma_loc_i)<1) comma_loc_i <- 0
    
    if (end_punct_i==num_words) {
        wd_sug <- c(fin_1$ngrams[1:3])
        return(wd_sug)
    } else {
        x <- x[I(end_punct_i+1):num_words]
        if (comma_loc_i>0) comma_loc_i <- comma_loc_i-end_punct_i
    }
    
    x <- sapply(x,preprocess,case = "lower",remove.punct = TRUE,USE.NAMES = FALSE)
    num_words <- length(x)
    
    if (num_words == 1) {
        if (end_in_space | comma_loc_i==1) {
            wd_sug <- two_num(paste0(x," "))
        } else {
            wd_sug <- one_num(x)
        }
    }
    
    if (num_words == 2) {
        if (end_in_space | max(comma_loc_i)==2) {
            wd_sug <- thr_num(paste0(concatenate(x)," "))
        } else {
            wd_sug <- two_num(concatenate(x))
        }
    }
    
    if (num_words == 3) {
        if (end_in_space | max(comma_loc_i)==3) {
            wd_sug <- paste(x[1],thr_num(paste0(concatenate(x[2:3])," ")),sep = " ")
        } else {
            wd_sug <- thr_num(concatenate(x))
        }
    }
    
    if (length(wd_sug)>0 & comma_loc_i>0) {
        for (i in 1:length(wd_sug)) {
            ttt <- unlist(strsplit(wd_sug[i],split = " "))
            ttt <- replace(ttt,comma_loc_i,paste0(ttt[comma_loc_i],","))
            wd_sug[i] <- concatenate(ttt)
        }
    }
    
    if (length(wd_sug)>0) {
        for (i in 1:length(wd_sug)) {
            wd_sug[i] <- word(wd_sug[i],num_words,wordcount(wd_sug[i]))
        }
    }
    
    wd_sug
}

shinyServer(function(input, output, session) {
    
    temp_text <- reactive({input$usr_text})
    temp_sug <- reactive({nlpP_model(temp_text())})
    
    num_sug <- reactive({length(temp_sug())})
    output$thenum <- renderText({num_sug()})

    output$str_suggest1 <- renderText({
        if (num_sug()>0) {
            temp_sug()[1]
        }
    })
    output$str_suggest2 <- renderText({
        if (num_sug()>1) {
            temp_sug()[2]
        }
    })
    output$str_suggest3 <- renderText({
        if (num_sug()>2) {
            temp_sug()[3]
        }
    })
    
    observeEvent(input$choice1, {
        updateTextInput(session, "usr_text", 
                        value = concatenate(
                            word(input$usr_text,1,I(wordcount(input$usr_text)-1))
                            ,temp_sug()[1],""
                            )
                        )
    })
    observeEvent(input$choice2, {
        updateTextInput(session, "usr_text", 
                        value = concatenate(
                            word(input$usr_text,1,I(wordcount(input$usr_text)-1))
                            ,temp_sug()[2],""
                            )
                        )
    })
    observeEvent(input$choice3, {
        updateTextInput(session, "usr_text", 
                        value = concatenate(
                            word(input$usr_text,1,I(wordcount(input$usr_text)-1))
                            ,temp_sug()[3],""
                            )
                        )
    })
    output$fin_text <- renderText(input$usr_text)
})
