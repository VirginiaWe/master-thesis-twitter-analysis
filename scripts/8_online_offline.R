# Online vs. Offline

rm(list=ls())
source("Scripts/run_first.R")
load("Data/data_comm.Rda")
data_kovi <- readRDS("Data/smd_data_kovi.RDS")



# Prepare Twitter data ----------------------------------------------------
dat <- dat %>%
  mutate(date = as.Date(created_at))

max <- max(dat$date)
min <- min(dat$date)

# Prepare Newspaper Data --------------------------------------------------

# Select dates & language
data_K <- data_kovi %>%
  mutate(date = as.Date(pubDateTime)) %>%
  filter(date %in% min:max) %>%
  filter(la == "de") %>%
  filter(selectsclass != "NotPolitical")

# Filter contiains relevant words
## Create corpora 
# Doc-id
data_K <- data_K %>%
  mutate(number=str_pad(row_number(),width=4,pad="0"),
         doc_id=paste0(so,number)) 

dfm <- corpus(data_K, text_field = "text_prep") %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, include_docvars = TRUE) %>% 
  tokens_tolower() %>%
  tokens_wordstem(language = ("de")) %>% 
  tokens_remove(.,c(stopwords('de'), "fur", "dass")) %>%
  dfm()

dict_kovi <- dictionary(list(kovi = c("konzern*", "unternehmens*")))
dfm_kovi <- dfm_lookup(dfm, dict_kovi)

data_kovi <- convert(dfm_kovi, to = "data.frame") #aus dfm data frame machen
data_K <- left_join(data_K, data_kovi, by = "doc_id") #gemäss doc_id mit Originaldaten kombinieren
data_K$kovi <- ifelse(data_K$kovi != 0, 1, 0) #Binäre Variable 1: Thema Wahl, 0 anderes Thema

data_K <- data_K %>%
  filter(kovi == 1)


# Prepare advertsiment data -----------------------------------------------

dat_aps <- read_excel("Data/APS/APS-Datensatz_20201129_Konzernverantwortungsinitiative.xlsx",sheet = "DATEN", range = "A1:BZ1105")


dataAPS <- dat_aps %>%
  filter(Farbe %in% c(1,0)) %>%
  mutate(date = as.Date(Erscheinungsdatum)) %>%
  filter(date %in% min:max) 

# Timeline ----------------------------------------------------------------

# Total
dat_plot1 <- dat %>%
  filter(!is.na(community_name) & community_name != "Others") %>%
  mutate(date = as.Date(created_at),
         group = months(created_at, abbreviate = F)) %>%
  group_by(date, group) %>%
  tally() %>%
  mutate(group = "Twitter")

dat_plot2 <- data_K %>%
  mutate(date = as.Date(pubDateTime),
         group = months(date, abbreviate = F)) %>%
  group_by(date, group) %>%
  tally() %>%
  mutate(group = "Media contributions")

dat_plot <- rbind(dat_plot1, dat_plot2)

## Plot Total
plot <- ggplot(data = dat_plot, 
               aes(x = date, y = ifelse(group == "Twitter", n, n*10), color=group)) +
  geom_line(size=0.7) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style()  +
  scale_y_continuous(name="Twitter", limits=c(0,3200),
    sec.axis = sec_axis( trans=~./10, name="Media Contributions")) +
  scale_x_date(name="", date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual(values = c("#FAAB18", "#1380A1")) +
  theme(legend.title = element_blank(), 
        legend.position = c(0.2, 0.8), 
        legend.direction = "vertical",
        legend.text = element_text(size=rel(1)),
        plot.caption = element_text(size=rel(.9), face="italic", hjust = 0), 
        axis.text.y = element_text(size=rel(.8)),
        axis.text.x = element_text(size=rel(.8), angle = 45, hjust = 1),
        axis.title = element_text(size = 8, angle = 0),
        plot.title = element_text(size=rel(2)),
        plot.subtitle = element_text(size=rel(1.5)),
        plot.margin = rep(grid::unit(0.1,"in"),4)) +
  geom_point(shape=15, size=0) +
  guides(colour = guide_legend(override.aes = list(size=3,linetype=0))) +
  
  ## Add election date line
  geom_segment(aes(x = as.Date("2020-11-29"), y = 0, xend = as.Date("2020-11-29"), yend = 2750), size=.5, linetype="dashed", colour = "#555555") +
  annotate("text", x=as.Date("2020-11-18"), y=2100, label="Election Day\n29 November 2020", size = rel(4), hjust=.5, vjust=-.3, colour = "#555555") +
  geom_curve(aes(x = as.Date("2020-11-29"), y = 2800, xend = as.Date("2020-11-18"), yend = 2500), 
             colour = "#555555", 
             size=0.1, 
             curvature = 0.3,
             arrow = arrow(length = unit(0.02, "npc")));plot 

ggsave(plot, filename = "Plots/online_offline_line.pdf", width = 8, height = 5, device = cairo_pdf)

# Anteil
dat_plot1 <- dat %>%
  filter(!is.na(community_name) & community_name != "Others") %>%
  mutate(date = as.Date(created_at)) %>%
  group_by(date) %>%
  dplyr::summarise(n = n()) %>%
  mutate(pct = n / sum(n)*100) %>%
  mutate(group = "Twitter")

dat_plot2 <- data_K %>%
  mutate(date = as.Date(pubDateTime),
         group = months(date, abbreviate = F)) %>%
  group_by(date) %>%
  dplyr::summarise(n = n()) %>%
  mutate(pct = n / sum(n)*100) %>%
  mutate(group = "Media contributions")

dat_plot3 <- dataAPS %>%
  mutate(group = months(date, abbreviate = F)) %>%
  group_by(date) %>%
  dplyr::summarise(n = n()) %>%
  mutate(pct = n / sum(n)*100) %>%
  mutate(group = "Advertisements")



dat_plot <- rbind(dat_plot1, dat_plot2)
dat_plot <- rbind(dat_plot, dat_plot3)

## Plot Total
plot <- ggplot(data = dat_plot, 
               aes(x = date, y = pct/100, color=group)) +
  geom_line(size=0.7) +
  annotate("rect", xmin=as.Date("2020-10-18"), xmax=as.Date("2020-11-15"), ymin=0, ymax=.08, alpha=0.1, fill="red") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style()  +
  scale_y_continuous(limits=c(0,0.1), labels=scales::percent_format(accuracy = 1L)) +
  scale_x_date(name="", date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual(values = c("#AD7A99", "#FAAB18", "#1380A1")) +
  theme(legend.title = element_blank(), 
        legend.position = c(0.2, 0.85), 
        legend.direction = "vertical",
        legend.text = element_text(size=rel(1)),
        plot.caption = element_text(size=rel(.9), face="italic", hjust = 0), 
        axis.text.y = element_text(size=rel(.8)),
        axis.text.x = element_text(size=rel(.8), angle = 45, hjust = 1),
        plot.title = element_text(size=rel(2)),
        plot.subtitle = element_text(size=rel(1.5)),
        plot.margin = rep(grid::unit(0.1,"in"),4)) +
  geom_point(shape=15, size=0) +
  guides(colour = guide_legend(override.aes = list(size=3,linetype=0))) +
  
  ## Add election date line

  geom_segment(aes(x = as.Date("2020-11-29"), y = 0, xend = as.Date("2020-11-29"), yend = 0.076), size=.5, linetype="dashed", colour = "#555555") +
  annotate("text", x=as.Date("2020-11-29"), y=0.078, label="Election Day\n29 November 2020", size = rel(4), hjust=.5, vjust=-.3, colour = "#555555");plot 

ggsave(plot, filename = "Plots/online_offline_line3.pdf", width = 8, height = 5, device = cairo_pdf)

# Anteil (Weeks before Election)
dat_plot1 <- dat %>%
  filter(!is.na(community_name) & community_name != "Others") %>%
  mutate(date = as.Date(created_at)) %>%
  mutate(week = week(date)) %>%
  group_by(week) %>%
  dplyr::summarise(n = n()) %>%
  mutate(pct = n / sum(n)*100) %>%
  mutate(group = "Twitter")

dat_plot2 <- data_K %>%
  mutate(date = as.Date(pubDateTime),
         group = months(date, abbreviate = F)) %>%
  mutate(week = week(date)) %>%
  group_by(week) %>%
  dplyr::summarise(n = n()) %>%
  mutate(pct = n / sum(n)*100) %>%
  mutate(group = "Media contributions")

dat_plot3 <- dataAPS %>%
  mutate(group = months(date, abbreviate = F)) %>%
  mutate(week = week(date)) %>%
  group_by(week) %>%
  dplyr::summarise(n = n()) %>%
  mutate(pct = n / sum(n)*100) %>%
  mutate(group = "Advertisements")

dat_plot <- rbind(dat_plot1, dat_plot2)
dat_plot <- rbind(dat_plot, dat_plot3)
dat_plot <- filter(dat_plot, week < 48)

## Plot Total
plot <- ggplot(data = dat_plot, 
               aes(x = week, y = pct/100, color=group, group=group)) +
  geom_line(size=0.7) +
  annotate("rect", xmin=41, xmax=45, ymin=0, ymax=.3, alpha=0.1, fill="red") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style()  +
  scale_y_continuous(limits=c(0,0.3), labels=scales::percent_format(accuracy = 1L)) +
  scale_x_continuous(breaks = c(38:47), labels = c(-9:0)) +
  scale_color_manual(values = c("#AD7A99", "#FAAB18", "#1380A1")) +
  theme(legend.title = element_blank(), 
        legend.position = c(0.2, 0.85), 
        legend.direction = "vertical",
        legend.text = element_text(size=rel(1)),
        plot.caption = element_text(size=rel(.9), face="italic", hjust = 0), 
        axis.text.y = element_text(size=rel(.8)),
        axis.text.x = element_text(size=rel(.8), angle = 45, hjust = 1),
        plot.title = element_text(size=rel(2)),
        plot.subtitle = element_text(size=rel(1.5)),
        plot.margin = rep(grid::unit(0.1,"in"),4)) +
  geom_point(shape=15, size=0) +
  guides(colour = guide_legend(override.aes = list(size=3,linetype=0))); plot
  
ggsave(plot, filename = "Plots/online_offline_line4.pdf", width = 8, height = 4, device = cairo_pdf)



#  Relevance actors ------------------------------------------------------

## Share of contributions per community

# Twitter
data <- dat %>%
  filter(!is.na(community_name) & community_name != "Others") %>%
  group_by(community_name) %>%
  dplyr::summarise(n = n()) %>%
  mutate(Twitter1 = round(n / sum(n)*100)) %>%
  mutate(Twitter = paste0(round(n / sum(n)*100), "%")) %>%
  arrange(-Twitter1) %>%
  select(Community = community_name, Twitter) 

# Media (fög data)
media <- read_excel("Data/resonance.xlsx") %>%
  select(Community, media = Resonance)

media$media <- paste0(round(media$media), "%")
media$media[1] <- "-"
data <- merge(data, media, by="Community") %>%
  arrange(-as.numeric(str_extract(Twitter, "[0-9]{1,2}")))

names(data)[3] <- "Media contributions"
save(data, file="Data/resonance.RData")

# Supporter vs. Opponents
dat$supop <- ifelse(dat$community %in% c(1,2,4:7), "Supporter", 
                    ifelse(dat$community %in% c(3), "Opponent", "Neither"))

data1 <- dat %>%
  filter(!is.na(community_name) & community_name != "Others") %>%
  group_by(supop) %>%
  dplyr::summarise(n = n()) %>%
  mutate(Twitter = paste0(round(n / sum(n)*100), "%")) %>%
  select(Position = supop, Twitter)

# Media (fög data)
media <- c("53%", "47%")
data1$media <- media
names(data1)[3] <- "Media contributions"

# Advertsiments (APS)
dat_aps <- read_excel("Data/APS/APS-Datensatz_20201129_Konzernverantwortungsinitiative.xlsx",sheet = "DATEN", range = "A1:BZ1105")

data3 <- dat_aps %>%
  filter(Farbe %in% c(1,0) & Position !="N") %>%
  mutate(Position = ifelse(Position == "C", "Opponent", "Supporter")) %>%
  select(Laufnummer, Position, Erscheinungsdatum, messagepro1:messagecon15) %>%
  pivot_longer(messagepro1:messagecon15, names_to = "argument", values_to = "value") %>%
  group_by(Position) %>%
  dplyr::summarise(n = n()) %>%
  mutate(Advertisments = paste0(round(n / sum(n)*100), "%")) %>%
  select(Position, Advertisments)

data1$Advertsiments <- data3$Advertisments

save(data1, file="Data/resonance_so.RData")
