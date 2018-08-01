library(plumber)
library(httr)
library(data.table)
library(RCurl)
library(jsonlite)

#* @serializer html
#* @get /
myFunc <- function(){
  return ("Chuje zle")
}
