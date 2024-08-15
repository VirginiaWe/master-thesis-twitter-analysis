# Assignment of the categories to the communities
rm(list=ls())
source("Scripts/run_first.R")
load("Data/data_comm.Rda")


# Examine biographies -----------------------------------------------------
dat <- dat %>% 
  filter(community_name != "Others" & !is.na(community_name)) 
  
user <- unique(dat$user_id)
usrs <- lookup_users(user)


# Description by Community ------------------------------------------------
usrs$description_clean <- usrs$description %>% 
  str_remove_all("http[:graph:]+") %>% #Visible characters
  str_remove_all("[^[:alpha:]|[:blank:]]")  #Space and tab

usrs <- usrs %>% 
  filter(description_clean != "") 

# Combine with community
com <- dat %>% select(user_id, community_name) %>% distinct()
usrs <- usrs %>%
  left_join(com, by="user_id") 

corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  textstat_frequency(n = 20, groups = community_name) # häufigste 20 Wörter

f_count <- length(unique(usrs$community_name))
get.palette <- colorRampPalette(brewer.pal(7, "Accent"))

plot <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_group(groups = community_name) %>%
  dfm_trim(min_termfreq = 5, verbose = FALSE) %>%
  quanteda.textplots::textplot_wordcloud(comparison = TRUE, max_words = 300,
                                         color = get.palette(f_count)); plot

ggsave(plot, filename = "Plots/community_freq.pdf", width = 8, height = 5, device = cairo_pdf)



# Search Description by Words ---------------------------------------------

# Wirtschaft/Unternehmen & Wirtschaftsverbände
econ <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_select("wirtschaft*")   %>%
  textstat_frequency(n = 20, groups = community_name) %>%
  as.data.frame() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise("wirtschaft*" = sum(frequency));econ

# Wissenschaft/Experten
science <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_select("scientist")   %>%
  textstat_frequency(n = 20, groups = community_name) %>%
  as.data.frame() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise("scientist" = sum(frequency));science

# Religiöse Akteure
religion <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_select("relig*")   %>%
  textstat_frequency(n = 20, groups = community_name) %>%
  as.data.frame() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise("relig*" = sum(frequency));religion


# Foreign Actors
foreign <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_select("germany")   %>%
  textstat_frequency(n = 20, groups = community_name) %>%
  as.data.frame() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise("germany" = sum(frequency));foreign

# CVP
cvp <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_select("cvp")   %>%
  textstat_frequency(n = 20, groups = community_name) %>%
  as.data.frame() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(cvp = sum(frequency));cvp

# BDP
bdp <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_select("bdp")   %>%
  textstat_frequency(n = 20, groups = community_name) %>%
  as.data.frame() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(bdp = sum(frequency));bdp

# GLP
glp <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_select("glp")   %>%
  textstat_frequency(n = 20, groups = community_name) %>%
  as.data.frame() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(glp = sum(frequency));glp

# SP
sp <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_select("sp")   %>%
  textstat_frequency(n = 20, groups = community_name) %>%
  as.data.frame() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(sp = sum(frequency));sp

# FDP
fdp <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_select("fdp")   %>%
  textstat_frequency(n = 20, groups = community_name) %>%
  as.data.frame() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(fdp = sum(frequency));fdp

# SVP
svp <- corpus(usrs, text_field = "description_clean") %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  tokens_remove(.,c(stopwords('en'))) %>%
  dfm() %>%
  dfm_select("svp")   %>%
  textstat_frequency(n = 20, groups = community_name) %>%
  as.data.frame() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(svp = sum(frequency));svp

df.list <- list(econ, science, religion, foreign, bdp, cvp, fdp, glp, sp, svp)
dat_freq <- df.list %>% reduce(left_join, by = "group")
dat_freq[is.na(dat_freq)] <- 0
names(dat_freq)[1] <- ""

save(dat_freq, file="Data/categroy_communits.RData")



