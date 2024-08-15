#!/usr/local/bin/Rscript

## Search Twitter Stream
rm(list=ls())
source("Scripts/run_first.R")
options(stringsAsFactors = F)

## 1. Register API using Twitter account
#browseURL("https://apps.twitter.com")
token <- create_token(app = app,
                      consumer_key = consumer_key,
                      consumer_secret = consumer_secret,
                      access_token = access_token,
                      access_secret = access_secret)

## 2. Search twitter Hastag & Keywords
keywords <- c("Konzernverantwortungsinitiative OR kovi OR kovini OR KVI OR konzernverantwortung OR 
              MultinationalesResponsables OR #kvi OR #konzernverantwortung OR #KVI OR #Konzernverantwortungsinitiative
              OR #Kovini OR #kovi OR #KoVi OR #MultinationalesResponsables",
              "#UVI OR #Unternehmensverantwortung OR #JaKVI OR #UVINein OR #kvinein OR #KVIja")

tweetsdf <-  search_tweets(keywords, n=18000, retryonratelimit = T)

saveRDS(tweetsdf, paste0("kovi_tweets_", Sys.Date(),".rds"))


