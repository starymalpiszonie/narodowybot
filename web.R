library(plumber)
library(httr)
library(data.table)
library(RCurl)
library(jsonlite)
library(dplyr)
library(stringi)
source("/home/lukasz/sentence_generator.R")
source("/home/lukasz/find_seed.R")
APP_TOKEN<-"EAAC9ATwJtPoBAMkpe8z2N79VB8y0MiNu0vke8I7eJIp4iiUtdaWqKpC1P94s4aQvqDPoQKdDLYiZAs7ZCTMhKjpYavLuiIlEdoeMmztHUlWHxf35p8ZCxjT9DIZAVN9qJK1xY1t2fFfFAy9sw5LptQjBRqSSF783ckZCnqdkoVwZDZD"

send_message<-function(recipient,message){
  body<-'{"recipient":{"id":"<ID>"},"message":{"text":"<MSG>"}}'
  body<-stri_replace_all(body,fixed = "<ID>",recipient)
  body<-stri_replace_all(body,fixed = "<MSG>",message)

  url<-"https://graph.facebook.com/v2.6/me/messages?access_token="
  full_url<-paste0(url,APP_TOKEN)
  res <- POST(full_url,body=body,content_type("application/json"))
  res
}
#* @serializer html
#* @post /
myFunc <- function(req,res){
  #message(req)
  #message(res)
  
  postBody<-req[["postBody"]]

  extractOneMessage<-function(postBody){
    inner<-function(msg){
      df<-msg$message
      df$sender<-msg$sender$id[1]
      df
    }
    
    tmp<-fromJSON(postBody)
    object<-tmp$object
    
    if(object=="page"){
      messages<-tmp$entry$messaging
      res<-lapply(messages,inner) %>% bind_rows()
    }
    else
      res<-data.frame(stringsAsFactors = F)
    res
  }
  
  m<-lapply(postBody,extractOneMessage) %>% bind_rows()

  for(i in nrow(m)){
    cr<-m[i,]
    to<-cr$sender[1]
    test_sentence<-cr$text
    seed<-find_seed(test_sentence)
    text<-generate_one_sentence(seed)
    
    send_message(to,text)
  }
    
  return ("OK")
}
