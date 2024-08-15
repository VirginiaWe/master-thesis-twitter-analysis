# Cleaning twitter data
rm(list=ls())
source("Scripts/run_first.R")

# Import data & Combine -------------------------------------------------------------
## .Rds files
files = list.files(path = "./Data", pattern = "*.rds",  full.names = T)

dat1 <- files %>%
  map(readRDS) %>% 
  bind_rows()

## .csv file
files2 = list.files(path = "./Data", pattern = "*.csv", full.names = T)

dat2 <- read_csv(files2)
names(dat2) <- tolower(names(dat2))
names(dat2)[3] <- "Datum"

# .json files
files3 = list.files(path = "./Data/json", pattern = "*.json", full.names = T)

dat3 <- lapply(files3, function(x) rjson::fromJSON(file=x) %>% as.data.frame) %>%
  bind_rows()
names(dat3) <- tolower(names(dat3))
names(dat3)[3] <- "Datum"
names(dat3)[91] <- "Akteur"
names(dat3)[92] <- "Kürzel"
names(dat3)[93] <- "Quelle"

# Combine
dat <- rbind.fill(dat1, dat2) %>% rbind.fill(dat3)

save(dat,file="Data/data_raw.Rda")
# The raw df includes 630901 tweets from 22032 users


# Clean data -------------------------------------------------------------
# Language
## German: 248451 Tweets
dat %>%
  group_by(lang) %>%
  tally()

## detect Language using cld3:
dat$lang2 <- detect_language(dat$text)
## German: 240788 Tweets
dat %>%
  group_by(lang2) %>%
  tally()

dat$langdif <- ifelse(dat$lang==dat$lang2,1,0)

dat %>%
  group_by(lang) %>%
  filter(langdif == 0) %>%
  filter(lang == "de" | lang2 == "de") %>%
  select(text, lang, lang2)

# lang seems to be the better choice!
dat <- filter(dat, lang == "de") #248451 Tweets

# Only unique tweets
dat <- dat %>%
  distinct(user_id, text, is_quote, is_retweet, source, .keep_all = T)

save(dat,file="Data/data_clean.Rda")

# Information about dataset -----------------------------------------------

nrow(dat) # 40324 Tweets
length(unique(dat$user_id)) # 5827 Accounts 
length(unique(dat$text)) # 16962 Unique Tweets
dat %>%
  filter(is_retweet == TRUE) %>%
  tally() # 23768 Retweets
dat %>%
  filter(is_quote == 1) %>%
  tally()# 2098 Quotes

nrow(dat) - sum(is.na(dat$reply_to_status_id)) # 7233 Replies
