# Content Analysis
rm(list=ls()) 
source("Scripts/run_first.R")
load("Data/data_comm.Rda")


# Communities -------------------------------------------------------------
dat <- dat %>%
  mutate(date = as.Date(created_at))

dat$supop <- ifelse(dat$community %in% c(1,2,4:7), "Supporter", 
                    ifelse(dat$community %in% c(3), "Opponent", "Neither"))

         
# . Timeline  ---------------------------------------------------
# Create dataset
dat_plot <- dat %>%
  filter(!is.na(community_name) & community_name != "Others") %>%
  mutate(date = as.Date(created_at),
         group = months(created_at, abbreviate = F)) %>%
  group_by(community_name, date, group) %>%
  tally() 


dat_plot$group <- factor(dat_plot$group, levels = unique(dat_plot$group))
dat_plot$community_name <- factor(dat_plot$community_name, levels = unique(dat_plot$community_name))

## Plot Total
plot <- ggplot(data = dat_plot, 
               aes(x = date, y = n, color=community_name)) +
  geom_line(size=0.7) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style()  +
  scale_y_continuous(limits = c(0,800)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_color_brewer(name = '', type = "qual", palette = 1) +
  theme(legend.title = element_blank(), 
        legend.position = c(0.2, 0.76), 
        legend.direction = "vertical",
        legend.text = element_text(size=rel(1)),
        plot.caption = element_text(size=rel(.9), face="italic", hjust = 0), 
        axis.text.y = element_text(size=rel(.8)),
        axis.text.x = element_text(size=rel(.8), angle = 45, hjust = 1),
        plot.title = element_text(size=rel(2)),
        plot.subtitle = element_text(size=rel(1.5))) +
  geom_point(shape=15, size=0) +
  guides(colour = guide_legend(override.aes = list(size=3,linetype=0))) +
  
  ## Add election date line
  geom_segment(aes(x = as.Date("2020-11-29"), y = 0, xend = as.Date("2020-11-29"), yend = 700), size=.5, linetype="dashed", colour = "#555555") +
  annotate("text", x=as.Date("2020-11-29"), y=700, label="Election Day\n29 November 2020", size = rel(4), hjust=.5, vjust=-.3, colour = "#555555" );plot 

ggsave(plot, filename = "Plots/community_activity_line.pdf", width = 8, height = 5, device = cairo_pdf)

## Peaks
dat_plot %>%
  group_by(date) %>%
  mutate(n_sum = sum(n)) %>%
  distinct(date, .keep_all = T) %>%
  select(date, n_sum) %>%
  arrange(desc(n_sum)) %>%
  head(n=6)


# # Groups:   date [6]
# date       n_sum
# 1 2020-11-29  2677 -> Election day
# 2 2020-11-20  1325 -> SRF Arena
# 3 2020-11-21  1237
# 4 2020-11-18  1184 -> Abstimmungsprognose Tamedia
# 5 2020-11-19  1072
# 6 2020-10-30  1036 -> SRF Arena



# . Content ---------------------------------------------------------------
# Create doc-id
dat <- dat %>%
  group_by(screen_name) %>%
  mutate(number=str_pad(row_number(),width=4,pad="0"),
         doc_id=paste0(user_id,number)) 


data <- filter(dat, !is.na(community_name) & community_name != "Others") #only for identified communities
## Create corpora 
corpus <- corpus(data, text_field = "text")

## Create tokens (& clean text)
tokens <- tokens(corpus, 
                     remove_punct = TRUE, remove_symbols = TRUE,
                     remove_url = TRUE, include_docvars = TRUE) %>% 
    tokens_tolower() %>%
    tokens_wordstem(language = ("de")) %>% #generate wordstems
    tokens_remove(.,c(stopwords('de'), "fur", "dass")) # remove stopwords

## Create dfm
dfm <- dfm(tokens)
topfeatures(dfm) 
# #kvi              #konzernverantwort                              ja 
# 23025                            8344                            7870 
# schweiz                        @konz_vi #konzernverantwortungsinitiativ 
# 6380                            5228                            4834 
# #uvi                           #kovi                            mehr 
# 4797                            4514                            3902 
# konzern 
# 3890 

## Compute e total frequency of words as well as the relative frequency of words by group
textstat_frequency(dfm, groups = community_name, n=3) 

## Compute Yes/Pro Share per Community
dict_yes <- dictionary(list(Yes = c("ja", "pro"), 
                            No = c("nein")))
dfm_yes <- dfm_lookup(dfm, dict_yes)

yes <- textstat_frequency(dfm_yes, group = community_name) %>%
  group_by(group) %>%
  mutate(yesshare = round((frequency/sum(frequency))*100)) %>%
  filter(feature == "Yes") %>%
  as.data.frame() %>%
  select(community_name = group, yesshare); yes
 
save(yes, file="Data/yesshare.RData")

## Compute UVI/KVI Share per Community
dict_hst <- dictionary(list(Pro = c("kvi"), 
                            Con = c("uvi")))
dfm_hst <- dfm_lookup(dfm, dict_hst)

hst <- textstat_frequency(dfm_hst, group = community_name) %>%
  group_by(group) %>%
  mutate(share = round((frequency/sum(frequency))*100)) %>%
  as.data.frame() %>%
  select(community_name = group, feature, share) %>%
  pivot_wider(names_from = "feature", values_from="share");hst
  
save(hst, file="Data/hstshare.RData")

# Supporters vs. Opponents ------------------------------------------------
# . Assign groups ------------------------------------------------
## Method 1: by community
dat$supop <- ifelse(dat$community %in% c(1,2,4:7), "Supporter",
                    ifelse(dat$community %in% c(3), "Opponent", "Neither"))

## Method 2: by dictionnary
pro <- c("kovi", "kvi", "jakvi", "kvija", "ja", "pro")
#
con <- c("uvi", "uvinein", "kvinein", "unternehmensverantwortungsinitiative",
         "unternehmensverantwortung", "nein", "gegen")

dict <- dictionary(list(Supporter = pro,
                             Opponent = con))
dfm_dict <- dfm_lookup(dfm, dict)

textstat_frequency(dfm_dict, group = community_name)
supop2 <- convert(dfm_dict, to = "data.frame")
dat <- left_join(dat, supop, by = "doc_id")

dat <- dat %>%
   group_by(screen_name) %>%
  mutate(supporter_sum = sum(Supporter),
          opponent_sum = sum(Opponent),
         support = (supporter_sum/(supporter_sum + opponent_sum)),
         fight = (opponent_sum/(supporter_sum + opponent_sum)),
         position = ifelse(support >= .8, "P",
                           ifelse(opponent >= .8, "C", "N")))


# . Content ----------------------------------------------------------------
## Compute e total frequency of words as well as the relative frequency of words by group

# Supporter vs. Opponents
tstat_freq <- textstat_frequency(dfm, groups = supop) %>%
  group_by(group) %>%
  filter(group != "Neither") %>%
  mutate(pct = frequency/sum(frequency)) %>%
  select(feature, group, frequency, pct) %>%
  pivot_wider(names_from = "group", values_from = c("frequency", "pct")) %>%
  mutate(score = pct_Supporter/pct_Opponent, lscore = log(score)) %>% # Log Relative frequency c1 / relative frequency c2
  na.omit() %>%
  mutate(freq_tot = frequency_Opponent + frequency_Supporter)

## Calculate the Chi-square for each word
tstat_freq$chi <- NA
for (i in 1:nrow(tstat_freq)) {
  chi <- chisq.test(tstat_freq[i,c(2,3)])
  tstat_freq$chi[i] <- as.numeric(chi$statistic)
}


## Select the 60 most frequently used words & 60 with the highest Chisquare values 
tstat_freq_60_freq <- tstat_freq %>%
  slice_max(freq_tot, n = 60) 
  
tstat_freq_60_chi <- tstat_freq %>%
  slice_max(chi, n = 60) 

#Unique words
tstat_freq_60_fin <- tstat_freq %>%
  filter(feature %in% tstat_freq_60_freq$feature | feature %in% tstat_freq_60_chi$feature) %>%
  filter(score < 15) %>%
  mutate(lscore_norm = lscore/max(abs(lscore))) %>%
  as.data.frame()

## Plot
plot <- tstat_freq_60_fin %>%
  ggplot(aes(
    x = lscore_norm, y = 0,
    label = feature, size = freq_tot, colour = lscore_norm)
  ) +
  geom_text_wordcloud() +
  xlim(-1,1) +
  # scale_size_area(max_size = 10) +
  # scale_colour_gradient(low = "red", high = "blue", breaks = scales::extended_breaks()) +
  scale_color_gradientn(colours = c("red", "blue"),
                       limits = c(-.5,.5), 
                       oob = scales::squish) + 
  bbc_style()  +
  theme_classic()  +
  xlab("Overrepresentation") +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=rel(.9)),
        plot.margin = margin(2, 2, 2, 2, "cm")) +
  
  # Add labels
  geom_text(aes(label = "Supporters"),
            x = 1.2,
            size = 5,
            color = "blue",
            angle = "90") +
  geom_text(aes(label = "Opponents"),
            x = -1.2,
            size = 5,
            color = "red",
            angle = "90") +
  coord_cartesian(clip = "off"); plot

ggsave(plot, filename = "Plots/overrep_words.pdf", width = 6.5, height = 5, device = cairo_pdf)


