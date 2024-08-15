# Interactions/Reply Network
rm(list=ls())
source("Scripts/run_first.R")
load("Data/data_comm.Rda")

# Construct reply network 
# (Which of the identified users replied to whom and with what frequency)


# Create df ---------------------------------------------------------------
reply_df<- dat %>%
  filter(!is.na(reply_to_user_id)) %>% #Filter NAs
  filter(user_id !=  reply_to_user_id) %>%
  filter(community_name != "Others" & !is.na(community_name)) %>%
  select(user_id, reply_to_user_id, community_name) %>%
  filter(reply_to_user_id %in% unique(user_id)) %>% #keep only people from network of interest
  as.data.table()


reply_df <-  reply_df[, list(Freq =.N), by=list(user_id,reply_to_user_id)]
names(reply_df) <- c("user", "reply_to", "freq")

# Metadata
metadata <- dat %>% 
  select(user_id, screen_name, community_name) %>% 
  filter(community_name != "Others" & !is.na(community_name)) %>%
  distinct()


# Create network ----------------------------------------------------------
netframe_directed <- graph_from_data_frame(reply_df[,1:2], directed = T)
adj_undir <- as_adjacency_matrix(netframe_directed, type  = "both")


# Add covariates
links <- as_data_frame(netframe_directed, what = "edges")
nodes <- as_data_frame(netframe_directed, what = "vertices")
nodes <- left_join(nodes, metadata, by = c("name" = "user_id")) %>% 
  distinct(name, .keep_all = TRUE)
links <- links %>% filter(from %in% unique(nodes$name)) %>%
  filter(to %in% unique(nodes$name))
netframe <- graph_from_data_frame(links, directed = T,
                                  vertices = nodes)
E(netframe)$weight <- reply_df$freq

# Plot
# Generate colors based on media type:
V(netframe)$color <- ifelse(V(netframe)$community_name == "Green-Left", brewer.pal(7,"Set1")[1],
                            ifelse(V(netframe)$community_name == "Mainstream", brewer.pal(7,"Set1")[2],
                                   ifelse(V(netframe)$community_name == "Conservative", brewer.pal(7,"Set1")[3],
                                          ifelse(V(netframe)$community_name == "Science/ Experts", brewer.pal(7,"Set1")[4],
                                                 ifelse(V(netframe)$community_name == "Center and religious actors", brewer.pal(7,"Set1")[5],
                                                        ifelse(V(netframe)$community_name == "Foreign actors", brewer.pal(7,"Set1")[6], brewer.pal(7,"Set1")[7]))))))


# The labels are currently node IDs. Setting them to NA will render no labels:
V(netframe)$label <- NA
# Set edge width based on weight:
E(netframe)$width <- E(netframe)$weight/6
#change arrow size and edge color:
E(netframe)$arrow.size <- .2
E(netframe)$edge.color <- "gray80"
E(netframe)$width <- 1+E(netframe)$weight/12
V(netframe)$size <- 8

#write.graph(netframe, file="netframe_interaction.graphml", format="graphml")


# Cross-Table (internal/external replies) ---------------------------------
reply_df<- dat %>%
  filter(!is.na(reply_to_user_id)) %>% 
  filter(user_id !=  reply_to_user_id) %>%
  filter(community_name != "Others" & !is.na(community_name)) %>%
  select(user_id, user_name = screen_name, community_user = community_name, reply_to_user_id) %>%
  filter(reply_to_user_id %in% unique(user_id)) %>% 
  left_join(metadata, by = c("reply_to_user_id" = "user_id")) %>%
  set_names("user_id",  "user_name", "community_user",
            "user_id_reply","reply_name" , "community_reply")

table <- reply_df %>%
  select(community_user, community_reply) %>%
  group_by(community_user, community_reply) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = paste0(round((n/sum(n)*100)), "%")) %>%
  select(-n) %>%
  pivot_wider(names_from = "community_user", values_from = c("freq")) %>%
  as.data.table()
table[is.na(table)] <- "0%"

# Row/Column sums
n_replies <-  reply_df %>%
  select(community_user, community_reply) %>%
  group_by(community_reply) %>%
  dplyr::summarise(n = n()) 

n_sent <-  reply_df %>%
  select(community_user, community_reply) %>%
  group_by(community_user) %>%
  dplyr::summarise(n = n()) 

table$n_replies_sent <-  n_sent$n

df = data.frame("Number of replies to posts from sender (n)", t(n_replies$n), "")
names(df) <- names(table)
table <- rbind(df, table)



names(table)[1] <- "Community posting a tweet (sender)"
names(table)[9] <- "Number of replies received"

table <- cbind(a = "Community receiving a reply", table)
names(table)[1] <- " "



table %>%
  knitr::kable(caption="Cross-tabulation of internal and external replies to tweets posted by each discourse community.", booktabs = TRUE, longtable = TRUE) %>%
  kable_styling(latex_options = c("repeat_header", "hold_position")) %>%
  add_header_above(c(" " = 2, "Community sending a reply" = 7, " "=1), bold=FALSE) %>%
  collapse_rows(columns = 1,
                valign = "middle") %>%
  footnote(general=paste0("The grey background represents internal replies within one community, while the white background represents external replies from other communities."), footnote_as_chunk = T) %>%
  row_spec(1:8, bold=F)
          
          
save(table, file="Data/table_replies.RData")

# Plot Sankey --------------------------------------------------------------------
plot_dat<- dat %>%
  filter(!is.na(reply_to_user_id)) %>% 
  filter(user_id !=  reply_to_user_id) %>%
  filter(community_name != "Others" & !is.na(community_name)) %>%
  select(user_id, user_name = screen_name, community_user = community_name, reply_to_user_id) %>%
  filter(reply_to_user_id %in% unique(user_id)) %>% 
  left_join(metadata, by = c("reply_to_user_id" = "user_id")) %>%
  set_names("user_id",  "user_name", "community_user",
            "user_id_reply","reply_name" , "community_reply") %>%
  select(community_user, community_reply) %>%
  group_by(community_user, community_reply) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = paste0(round((n/sum(n)*100)), "%")) %>%
  as.data.table()

data_long <- plot_dat %>% 
  select(-freq) 

colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

ColourScal ='d3.scaleOrdinal() .range(["#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F","#BF5B17"])'

# Make the Network
network <- sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, iterations = 0,
              colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20); network

require(htmlwidgets)
saveWidget(network, file="sankey.html")

require(webshot)
webshot("/Users/virginia/Dropbox (Privat)/Masterarbeit/R/Plots/sankey.html", "sankey.pdf")


# Plot Internal/External response -----------------------------------------
plot_dat2 <- dat %>%
  filter(!is.na(reply_to_user_id)) %>% 
  filter(user_id !=  reply_to_user_id) %>%
  filter(community_name != "Others" & !is.na(community_name)) %>%
  mutate(Date = as.Date(created_at)) %>%
  select(user_id, user_name = screen_name, community_user = community_name, reply_to_user_id, Date) %>%
  filter(reply_to_user_id %in% unique(user_id)) %>% 
  left_join(metadata, by = c("reply_to_user_id" = "user_id")) %>%
  select(user_id,  user_name, community_user,
            user_id_reply = reply_to_user_id, reply_name = screen_name , community_reply = community_name, Date) %>%
  select(community_user, community_reply, Date) %>%
  mutate(internal = ifelse(community_user == community_reply, 1, 0)) %>%
  group_by(internal, Date) %>%
  dplyr::summarise(n = n()) 

plot <- ggplot(data = plot_dat2, 
               aes(x = Date, y = n, color=as.factor(internal))) +
  geom_line(size=0.7) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style()  +
  scale_y_continuous(limits = c(0,200)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d.%b") +
  scale_color_manual(values = c("#FAAB18", "#1380A1"),
                     labels = c("External replies", "Internal replies"))+
  theme(legend.title = element_blank(), 
        legend.position = c(0.2, 0.85),
        legend.direction = "vertical",
        legend.text = element_text(size=rel(1)),
        plot.caption = element_text(size=rel(.9), face="italic", hjust = 0), 
        axis.text.y = element_text(size=rel(.8)),
        axis.text.x = element_text(size=rel(.8), angle = 45, hjust = 1),
        plot.title = element_text(size=rel(2)),
        plot.subtitle = element_text(size=rel(1.5))) +
  geom_point(shape=15, size=0) +
  guides(colour = guide_legend(override.aes = list(size=3,linetype=0)))  +
  ## Add election date line
  geom_segment(aes(x = as.Date("2020-11-29"), y = 0, xend = as.Date("2020-11-29"), yend = 150), size=.5, linetype="dashed", colour = "#555555") +
  annotate("text", x=as.Date("2020-11-29"), y=150, label="Election Day\n29 November 2020", size = rel(4), hjust=.5, vjust=-.6, colour = "#555555" );plot 

ggsave(plot, filename = "Plots/internal_external.pdf", width = 8, height = 5, device = cairo_pdf)

  
