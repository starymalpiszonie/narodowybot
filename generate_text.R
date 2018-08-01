library(data.table)
library(stringi)
library(dplyr)
library(tidytext)
library(tidyr)

probs<-readRDS("probs_2.RDS")

probs_cum <- probs %>%
  group_by(term1) %>%
  arrange(term1,probs) %>%
  mutate(cum_prob=cumsum(prob)) %>%
  ungroup()

generate_one_sentence<-function(start_term="xxxstart1xxx"){
  
  probs<-runif(100,0,1.0)
  current_token<-start_term
  i<-1
  done<-F
  text<-ifelse(start_term=="xxxstart1xxx",c(),c(start_term))
  while(!done){
    next_probs<-probs_cum %>% filter(
      term1==current_token
    ) %>%
      arrange(cum_prob)
    
    #-- find next token
    rand<-probs[i]
    ind<-min(which(next_probs$cum_prob>rand))
    next_word<-next_probs[ind,]$term2
    
    if(next_word=="xxxendxxx" | i>=100)
      done<-T
    else{
      i<-i+1
      text<-c(text,next_word)
      current_token<-next_word
    }
  }
  if(length(text)<2)
    text<-c(text," ")
  
  res<-paste0(text[2:length(text)],collapse = " ")
  res<-paste0(stri_trans_totitle(text[1])," ",res,".")
  res
}