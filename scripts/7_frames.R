# Frames
rm(list=ls())
source("Scripts/run_first.R")
load("Data/data_comm.Rda")


###########################################################################
# Online Frames -----------------------------------------------------------
###########################################################################

# Prepare data -------------------------------------------------------------
dat <- dat %>%
  mutate(date = as.Date(created_at))

dat$supop <- ifelse(dat$community %in% c(1,2,4:7), "Supporter", 
                    ifelse(dat$community %in% c(3), "Opponent", "Neither"))

# . Content ---------------------------------------------------------------
# Create doc-id
dat <- dat %>%
  group_by(screen_name) %>%
  mutate(number=str_pad(row_number(),width=4,pad="0"),
         doc_id=paste0(user_id,number)) 

# filter identified communities
data <- filter(dat, !is.na(community_name) & community_name != "Others") 

## Create corpora - tokens - dfm
dfm <- corpus(data, text_field = "text") %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
                 remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_tolower() %>%
  tokens_wordstem(language = ("de")) %>% #generate wordstems
  tokens_remove(.,c(stopwords('de'), "fur", "dass"))  %>% # remove stopwords
  dfm()


# . Generate Dictionnary --------------------------------------------------
dict_frames_small <- dictionary(list(economic = c("prämien", "praemien", "kmu", "unternehmen", "steuern", "wirtschaft", 
                                            "kosten", "beschäftigung", "beschaeftigung", "arbeit", "arbeitslos", "markt", "ausgaben", 
                                            "milliarden", "millionen", "geld", "firma", "unternehmen", "finanzierung", "franken",
                                            "regulierung", "leistungen", "arbeitsplätze", "arbeitsplaetze", "wirtschaftsstandort", "entwicklung"),
                               resource = c("ressourcen", "wohnen", "infrastruktur",  "national", "anbieten", "bereitstellen", "mittel", 
                                            "finanzierung", "natürlich", "natuerlich", "durchsetzung", "kapazität", "kapazitaet", "aufwand", 
                                            "kosten", "teuer", "mehraufwand", "bürokratie", "buerokratie"),
                               morality = c("moralisch", "religionen", "religiös", "religioes","ehrenhaft", "verantwortlich", "verantwortung",
                                            "illegal", "schützen", "schuetzen", "götter", "goetter", "heilig", "islam", "muslim", "christ", 
                                            "radikal", "gewalt", "opfer", "kirche"),
                               fair = c("fairness", "gleichheit", "ungleichheit", "gesetze", "rechte", "rasse", "geschlecht", "klasse", "zugang", 
                                        "arm", "bürgerlich", "buergerlich", "gerechtigkeit", "sozial", "frauen", "lgbt", "lgbtq", "diskriminierung", "entscheidungen"),
                               legal = c("rechte", "gesetze", "exekutive", "urteil", "verfassungsmässig", "verfassungsmaessig", "amnestie", 
                                         "entscheidungen", "reproduktiv", "legal", "legalität", "legalitaet", "gericht", "einwanderung", 
                                         "änderungen", "aenderungen", "richter", "autorität", "autoritaet", "präzedenzfall", "praezedenzfall",
                                         "gesetzgebung", "beweise", "beweislast", "beweislastumkehr", "widersprechen", "gesetz", "menschenrecht", 
                                         "knebelvertrag", "knebelverträge", "knebelvertraege", "erpressung", "haftungsrisiko", "generalverdacht", 
                                         "paralleljustiz", "klagen", "klageindustrie", "wortlaut"),
                               culture = c("identität", "identitaet", "sozial", "werte", "konservativ", "liberal", "nation", "schweiz", 
                                           "schweizerinnen", "schweizer", "gemeinschaft", "gemeinschaften", "land", "einwanderer", "flüchtlinge",
                                           "fluechtlinge", "asyl", "asylant", "asylanten", "geschichte", "historisch", "generalverdacht", "anständig",
                                           "anstaendig"),
                               # public = c("öffentlichkeit", "oeffentlichkeit", "stimmung", "meinung", "umfragen", "drehen", "umfrage", 
                               #            "unterstützung", "unterstuetzung", "schweiz", "reform", "aktion", "wollen", "brauchen", "abstimmung"),
                               # political = c("politik", "politisch", "haltung", "ansicht", "parteien", "lobby", "sp", "svp", "grüne", "gruene",
                               #               "gps", "glp", "bdp", "cvp", "links", "mitte", "rechts", "demokratie", "nationalrat", "ständerat", "staenderat", 
                               #               "bundesrat", "ausschuss", "partei", "verwaltung"),
                               policy = c("politik", "fixieren", "funktioniert", "arbeiten", "vorgeschlagen", "vorschlagen", "vorlage", "lösung", "loesung",
                                          "lösen", "loesen", "ergebnisse", "rechnung", "gesetz", "änderung", "aenderung", "plan", "unterstützen","unterstuetzen",  
                                          "aufheben", "reform", "extrem", "kontraproduktiv", "experiment", "dominoeffekt", "konstruktionsfehler", 
                                          "falsch", "widersprechen", "schaden", "unmöglich", "unmoeglich"),
                               external = c("regulierung", "schweiz", "beziehungen", "international", "national", "handel", "aussen", 
                                            "staat", "grenze", "visum", "verbündeter",  "verbuendeter", "verbündete", "verbuendete",
                                            "vereinigt", "flüchtlinge", "fluechtlinge", "führung", "fuehrung", "themen", "europa", "isoliert", 
                                            "weltpolizei", "alleingang", "paralleljustiz", "erpressbar", "überwachen", "ueberwachen", 
                                            "souveränität", "souverän","souveraenitaet", "souveraen"),
                               support = c("ja", "pro", "nein", "contra", "für", "gegen", "unterstuetzen", "sinnlos")))

# Apply
dfm_frames <- dfm_lookup(dfm, dict_frames_small)

dfm_frames %>%
  textstat_frequency()
dfm_frames %>%
  textstat_frequency(groups = supop)


# . combine with dataframe --------------------------------------------------
frame <- convert(dfm_frames, to = "data.frame")
dat <- left_join(dat, frame, by = "doc_id")


###########################################################################
# Offline Frames -----------------------------------------------------------
###########################################################################
dat_aps <- read_excel("Data/APS/APS-Datensatz_20201129_Konzernverantwortungsinitiative.xlsx",sheet = "DATEN", range = "A1:BZ1105")

dat_aps_clean <- dat_aps %>%
  filter(Farbe == 1) %>%
  select(Laufnummer, Position, Erscheinungsdatum, messagepro1:messagecon14) %>%
  pivot_longer(messagepro1:messagecon14, names_to = "argument", values_to = "value")

dat_aps_clean$frame1 <- ifelse(dat_aps_clean$argument %in% c("messagepro1", "messagepro3"), "Morality & Ethics",
                             ifelse(dat_aps_clean$argument %in% c("messagecon12"), "Capacity & Resources",
                                           ifelse(dat_aps_clean$argument %in% c("messagecon2", "messagecon3", "messagecon13"), "Policy Description & Evaluation",
                                                  ifelse(dat_aps_clean$argument %in% c("messagepro2", "messagepro5", "messagepro6", "messagecon10"), "Legality & Jurisdiction",
                                                                ifelse(dat_aps_clean$argument %in% c("messagepro7", "messagecon6", "messagecon9", "messagecon11"), "External Regulation and Reputation",
                                                                              ifelse(dat_aps_clean$argument == "messagecon5", "Cultural Identity",
                                                                                     ifelse(dat_aps_clean$argument %in% c("messagepro8", "messagecon1", "messagecon4", "messagecon14"), "Economic", 
                                                                                            ifelse(dat_aps_clean$argument %in% c("messagepro4", "messagecon8"), "General support & rejection", "NA"))))))))

dat_aps_clean$frame2 <- ifelse(dat_aps_clean$argument %in% c("messagepro5"), "Morality & Ethics",
                               ifelse(dat_aps_clean$argument %in% c("messagecon10"), "Capacity & Resources",
                                             ifelse(dat_aps_clean$argument %in% c("messagecon11", "messagecon13"), "Legality & Jurisdiction",
                                                                  ifelse(dat_aps_clean$argument %in% c("messagepro2"), "Fairness & Equality", "NA"))))

dat_aps_clean <- dat_aps_clean %>%
  pivot_longer(frame1:frame2, names_to="framenr", values_to="frame") %>%
  filter(value == 1) %>%
  filter(frame != "NA") %>%
  mutate(date = as.Date(Erscheinungsdatum),
         week = format(as.Date(Erscheinungsdatum), "%W")) 

dat_aps %>%
  group_by(Position) %>%
  tally()

# Comparison Twitter - Offline --------------------------------------------

# . online/offline -----------------------------------------------------

df.expanded$frame_lbl <- ifelse(df.expanded$frame == "morality", "Morality & Ethics",
                             ifelse(df.expanded$frame == "fair", "Fairness & Equality",
                                    ifelse(df.expanded$frame == "resource", "Capacity & Resources",
                                           ifelse(df.expanded$frame == "policy", "Policy Description & Evaluation",
                                                  ifelse(df.expanded$frame == "legal", "Legality & Jurisdiction",
                                                         ifelse(df.expanded$frame == "public", "Public Sentiment",
                                                                ifelse(df.expanded$frame == "external", "External Regulation and Reputation",
                                                                       ifelse(df.expanded$frame == "political", "Political Factors & Implications",
                                                                              ifelse(df.expanded$frame == "culture", "Cultural Identity",
                                                                                     ifelse(df.expanded$frame == "economic", "Economic", "General support & rejection"))))))))))


df.expanded$group <- "Tweets"
df.expanded <- select(df.expanded, supop, frame_lbl, group)

data <- dat_aps_clean %>%
  mutate(supop = ifelse(Position == "C", "Supporter", "Opponent"),
         group = "Advertisements") %>%
  select(supop, frame_lbl = frame, group) %>%
  rbind(df.expanded) %>%
  na.omit()

dat_plot <- data %>%
  group_by(group, frame_lbl) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  ungroup() %>%
  pivot_wider(-n, names_from = "group", values_from = "freq") %>%
  as.data.table()

dat_plot[is.na(dat_plot)] <- 0

## Plot Total
for(i in 1:nrow(dat_plot)){
  dat_plot$frame_lbl[i] <- paste(strwrap(dat_plot$frame_lbl[i], width = 25), collapse = "\n")
}

dat_plot$diff <- abs(dat_plot$Tweets-dat_plot$Advertisements)
dat_plot$frame_lbl <- factor(dat_plot$frame_lbl, levels = unique(dat_plot[order(diff)]$frame_lbl))

# dat_plot$frame_lbl <- factor(dat_plot$frame_lbl, levels = unique(dat_plot[order(Tweets)]$frame_lbl))


# . Dumbbell overall ------------------------------------------------------


plot <- ggplot(dat_plot, aes(x = Advertisements, xend = Tweets, y = frame_lbl, group = frame_lbl)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  bbc_style() + 
  scale_x_continuous(labels = c("0%", "10%", "20%", "30%")) +
  theme(axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=16)) +
  
  ### Add Labels
  geom_text(aes(x = Advertisements), label= paste0(round(dat_plot$Advertisements), "%"), color = "#717D8C", size = 3.75, vjust = 2.5) +
  geom_text(aes(x = Tweets), label= paste0(round(dat_plot$Tweets), "%"), color = "#717D8C", size = 3.75, vjust = 2.5, hjust = .75) +
  
  ### Add legend
  geom_text(data=filter(dat_plot, frame_lbl=="Cultural Identity"), aes(x = Advertisements, y = frame_lbl), label= "Advertisements", color = "#FAAB18", size = 4, fontface = "bold", vjust = -2, hjust = .8) +
  geom_text(data=filter(dat_plot, frame_lbl=="Cultural Identity"), aes(x = Tweets, y = frame_lbl), label= "Tweets", color = "#1380A1", size = 4, fontface = "bold", vjust = -2); plot

ggsave(plot, filename = "Plots/frames_db_vrgl.pdf", width = 8, height = 4.5, device = cairo_pdf)

# . Scatterplot by Channel and Position ------------------------------------------------------

## Data
channel <- data %>%
  group_by(group, frame_lbl) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  ungroup() %>%
  pivot_wider(-n, names_from = "group", values_from = "freq") %>%
  as.data.table() 

position <- data %>%
  group_by(supop, frame_lbl) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  ungroup() %>%
  pivot_wider(-n, names_from = "supop", values_from = "freq") %>%
  as.data.table() 

n <-   data %>%
  group_by(frame_lbl) %>%
  dplyr::summarise(n = n())  %>%
  select(n)

position <- cbind(position, n)


dat_plot <- merge(channel, position, by = "frame_lbl")
dat_plot[,diff_c := (Advertisements/(Tweets+Advertisements)*2)-1]
dat_plot[,diff_p := (Opponent/(Supporter+Opponent)*2)-1]


## Plot Total
for(i in 1:nrow(dat_plot)){
  dat_plot$frame_lbl[i] <- paste(strwrap(dat_plot$frame_lbl[i], width = 25), collapse = "\n")
}

summary(dat_plot$diff_c)
xlim <- c(-1,1)

summary(dat_plot$diff_p)
ylim <- c(-0.6,0.6) 

get.palette <- colorRampPalette(brewer.pal(8, "Set3"))
cols <- get.palette(length(unique(dat_plot$frame_lbl)))
fontsize <- 2.8

# .. Scatter --------------------------------------------------------------
plot <- ggplot(data = dat_plot, aes(x = diff_c, y = diff_p, color = frame_lbl)) +
  geom_point(aes(size = n), shape = 16) +
  geom_point(aes(size = n), color = "white", shape = 21, fill = NA, stroke = .25) +
  geom_text(data = dat_plot, hjust = 0, vjust = 1, aes(label =  frame_lbl, x = diff_c, y=diff_p), color = "grey45", size = fontsize) +
  bbc_style()  +
  labs(x = "<-- Tweets  Advertisement -->", y = "<-- Supporter  Opponent -->") + 
  scale_size_area(max_size = max(dat_plot$n)*0.001, guide = FALSE) +
  scale_color_manual(values = cols) +
  scale_x_continuous(limits = xlim, breaks = seq(xlim[1], xlim[2], 0.2), expand = c(0.01,0)) +
  scale_y_continuous(limits = ylim, breaks = seq(ylim[1], ylim[2], 0.2),labels = as.character(round(seq(ylim[1], ylim[2], 0.2), digits = 2)), expand = c(0.01,0)) +
  theme(panel.grid.major = element_line(size = 0.1), 
        panel.grid.minor.x = element_line(color = "#cbcbcb", size = 0.1), 
        legend.position = "none",
        axis.text = element_text(size = 8, colour = "grey60"), 
        axis.title = element_text(size = 6, colour = "grey60"), 
        legend.text = element_text(color = "grey45", size = 9)); plot

ggsave(plot, filename = "Plots/frames_scatter.pdf", width = 4.5, height = 4.5, device = cairo_pdf)



