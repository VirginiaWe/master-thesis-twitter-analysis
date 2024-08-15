## Packages

if (!require("pacman", quietly=T)) install.packages("pacman")
pacman::p_load(rtweet, jsonlite, rjson, httr, RCurl, data.table, readr, dplyr, plyr, tidyverse, rjson, cld3,
               igraph, rtweet, tidygraph, ggraph, lubridate, graphlayouts,bbplot,kableExtra,
               quanteda, quanteda, quanteda.textstats, ggwordcloud, networkD3, 
               RColorBrewer, ggalt, readxl)



## Twitter Tokens
app = app
consumer_key = consumer_key
consumer_secret = consumer_secret
access_token = access_token
access_secret = access_secret

# Google Key
googlekey = googlekey

options(stringsAsFactors = FALSE)