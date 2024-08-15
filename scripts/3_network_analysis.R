# Identifying Twitter Communities
rm(list=ls())
source("Scripts/run_first.R")
load("Data/data_clean.Rda")

# Create Twitter Follower Network
token <- create_token(app = app,
                      consumer_key = consumer_key,
                      consumer_secret = consumer_secret,
                      access_token = access_token,
                      access_secret = access_secret)

# Gather follower data ----------------------------------------------------
# Look up user of interest
# user <- lookup_users(unique(dat$user_id))
# user <- data.frame(lapply(user,as.character),
#                    stringsAsFactors = F)
load("Data/user.RData")

# Get Follower Network of User
# follower <- vector(mode = 'list', length = length(user$screen_name))
# names(follower) <- user$screen_name

# for (i in seq_along(user$screen_name)) {
#   message("Getting followers for user #", i, "/130")
#   follower[[i]] <- get_followers(user$screen_name[i], 
#                                  n=5000,
#                               retryonratelimit = TRUE)
#   
#   if(i %% 5 == 0){
#     message("sleep for 5 minutes")
#     Sys.sleep(5*60)
#   } 
# }

load("Data/follower_clean.RData")
follower <- follower_clean

# convert list to dataframe
follower_df <- bind_rows(follower, .id = "screen_name")
active_fol_x <- user %>% select(user_id,screen_name)

# Metadata
metadata <- user %>% select(user_id, screen_name)

# left join to convert screen_name into its user id
follower_df <- left_join(follower_df, active_fol_x, by="screen_name") %>%
  select(user_id.x,user_id.y) %>%
  setNames(c("friend_name","user")) %>% 
  na.omit() %>% 
  filter(friend_name %in% unique(metadata$user_id)) #keep only users from original kovi-df

# Create network dataset --------------------------------------------------
netframe_directed <- graph_from_data_frame(follower_df, directed = TRUE)

# Add covariates
links <- as_data_frame(netframe_directed, what = "edges")
nodes <- as_data_frame(netframe_directed, what = "vertices")
nodes <- left_join(nodes, metadata, by = c("name" = "user_id")) %>% 
  distinct(name, .keep_all = TRUE)
links <- links %>% filter(from %in% unique(nodes$name)) %>%
  filter(to %in% unique(nodes$name))
netframe <- graph_from_data_frame(links, directed = TRUE,
                                  vertices = nodes)
# Save graph-file for gephi visualization
# write.graph(netframe, file="netframe.graphml", format="graphml")

# Detect Communities ------------------------------------------------------
set.seed(12345)
netframe2 <- simplify(netframe)
adjacency_matrix <- igraph::as_adjacency_matrix(netframe2)
clu <- leiden::leiden(adjacency_matrix)
V(netframe2)$community <- as.character(clu)
netframe2$community <- as.character(clu)
data <- igraph::as_data_frame(netframe2, what= c("vertices") ) %>%
  select(user_id = name, screen_name, community) 

# Add to original dataframe
dat <- dat %>%
  left_join(data, by = c("user_id", "screen_name"))

dat$community_name <- ifelse(
  dat$community == "1", "Green-Left",
  ifelse(dat$community == "2", "Mainstream",
         ifelse(dat$community == "3", "Conservative",
                ifelse(dat$community == "4", "Science/ Experts",
                       ifelse(dat$community == "5", "Center and religious actors",
                              ifelse(dat$community == "6", "Foreign actors",
                                     ifelse(dat$community == "7", "Basel Region", "Others"))))))
)

save(dat, file="Data/data_comm.Rda")

# Covariates & Output -----------------------------
# Covariates
density <- igraph::edge_density(netframe) #0.01306022
path.length <- igraph::mean_distance(netframe) #2.74692
# modularity <- igraph::modularity(netframe, membership(community)) #0.2230334

# Size & most important
communities <- data.frame() 
for (i in unique(netframe2$community)) { 
  # create subgraphs for each community 
  subgraph <- induced_subgraph(netframe2, v = which(netframe2$community == i)) 
  # get size of each subgraph 
  size <- igraph::gorder(subgraph) 
  # get betweenness centrality 
  btwn <- igraph::betweenness(subgraph) 
  # get density
  density <- igraph::edge_density(subgraph)
  # average path length
  path.length <- igraph::mean_distance(subgraph)
  # eigenvektor
  eigen <- round(mean(igraph::centr_eigen(subgraph)$vector),2)
  communities <- communities %>% 
    dplyr::bind_rows(data.frame(
      community = i, 
      n_characters = size, 
      density = density,
      path.length = path.length,
      eigen = eigen,
      user_id = names(which(btwn == max(btwn))) 
    ) 
    ) 
} 

# communities %>%
#   left_join(metadata, by="user_id")  %>%
#   # filter(community %in% c(10,4,2,8,9,5,7)) %>%
#   mutate(n.pct = round((n_characters/sum(n_characters))*100)) %>%
#   select(community, screen_name, n_characters, n.pct, density, path.length, eigen)


# Top five users per community (by degree)
top_five <- data.frame() 
for (i in unique(netframe2$community)) { 
  # create subgraphs for each community 
  subgraph <- induced_subgraph(netframe2, v =       which(netframe2$community == i)) 
  # for larger communities 
  if (igraph::gorder(subgraph) > 20) { 
    # get degree 
    degree <- igraph::degree(subgraph) 
    # get top five degrees 
    top <- names(head(sort(degree, decreasing = TRUE), 10)) 
    result <- data.frame(community = i, rank = 1:10, user_id = top) 
  } else { 
    result <- data.frame(community = NULL, rank = NULL, user_id = NULL) 
  } 
  top_five <- top_five %>% 
    dplyr::bind_rows(result) 
  
} 

top_five <- top_five %>% 
  left_join(metadata, by="user_id") 

# top_five %>%
#   select(community, rank, screen_name) %>%
#   tidyr::pivot_wider(names_from = community, values_from = screen_name) 

# write.csv(x, file="topfive.csv")

# Top five users per community (number of followers)
topfive_follower <- dat %>% 
  distinct(screen_name, .keep_all=T) %>%
  select(screen_name, followers_count, community_name) %>%
  group_by(community_name) %>%
  slice_max(followers_count, n = 5) %>%
  filter(!is.na(community_name))

# Create nice looking table
communities <- communities %>%
  mutate(nofall = round(n_characters/sum(n_characters)*100)) %>%
  mutate(n = paste0(n_characters, " ", "(", nofall, "%",")")) %>%
  mutate(path.length = round(path.length, 2),
         density = round(density, 3))

communities <- dat %>%
  mutate(date = as.Date(created_at)) %>%
  group_by(community, date) %>%
  tally() %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(pct =  n/sum(n)*100) %>%
  ungroup() %>%
  group_by(community) %>%
  mutate(sharedailyvolume = paste0(round(mean(pct)), "%")) %>%
  select(community, sharedailyvolume) %>%
  distinct() %>%
  right_join(communities, by="community")


communities$community_name <- ifelse(
  communities$community == "1", "Green-Left",
  ifelse(communities$community == "2", "Mainstream",
         ifelse(communities$community == "3", "Conservative",
                ifelse(communities$community == "4", "Science/ Experts",
                       ifelse(communities$community == "5", "Center and religious actors",
                              ifelse(communities$community == "6", "Foreign actors",
                                     ifelse(communities$community == "7", "Basel Region", "Others"))))))
)


communities$position <- ifelse(
  communities$community == "1", "Supporter",
  ifelse(communities$community == "2", "Supporter",
         ifelse(communities$community == "3", "Supporter & Opponents",
                ifelse(communities$community == "4", "No position",
                       ifelse(communities$community == "5", "Supporter",
                              ifelse(communities$community == "6", "Supporter",
                                     ifelse(communities$community == "7", "Supporter/Neutral", ""))))))
)


communities$top_user <- ifelse(
  communities$community == "1", list(top_five$screen_name[top_five$community == 1][1:3]),
  ifelse(communities$community == "2", list(top_five$screen_name[top_five$community == 2][1:3]),
         ifelse(communities$community == "3", list(top_five$screen_name[top_five$community == 3][1:3]),
                ifelse(communities$community == "4", list(top_five$screen_name[top_five$community == 4][1:3]),
                       ifelse(communities$community == "5", list(top_five$screen_name[top_five$community == 5][1:3]),
                              ifelse(communities$community == "6", list(top_five$screen_name[top_five$community == 6][1:3]),
                                     ifelse(communities$community == "7", list(top_five$screen_name[top_five$community == 7][1:3]), ""))))))
)


communities <- communities %>%
  filter(community %in% c(1:7)) %>%
  select(community_name, position, top_user, n, sharedailyvolume, density, path.length, eigen)

save(communities, file="Data/communities.RData")


# Compare Network to Random Network (Erdos–Renyi Random Network) ---------
comp <- data.frame(network = c("Twitter", "Random"),
                   diameter = c(0, 0),
                   transitivity = c(0,0),
                   path.length =c(0,0),
                   degree =c(0,0),
                   polarization = c(0,0))

comp$diameter[comp$network == "Twitter"] <- diameter(netframe) # 8
comp$transitivity[comp$network == "Twitter"] <- round(transitivity(netframe, type = "global"),3) #clustring coefficient = 0.1627383
comp$path.length[comp$network == "Twitter"] <- round(mean_distance(netframe),2) #Mean Paht Length = 2.75
comp$degree[comp$network == "Twitter"] <- round(mean(degree(netframe)),2) #degree 143.09
comp$polarization[comp$network == "Twitter"] <- round(((max(betweenness(netframe)) - mean(betweenness(netframe))) / mean(betweenness(netframe))),2)

# Simulate Erdos–Renyi random networks with the same numbers of nodes and edges (100 networks) 
c=numeric(100)
r=numeric(100)
p=numeric(100)
d=numeric(100)
pol=numeric(100)

for (i in 1:100) {
  random_network=erdos.renyi.game(vcount(netframe), ecount(netframe), type="gnm")
  r[i]=diameter(random_network)
  c[i]=transitivity(random_network, type="global")
  p[i]=mean_distance(random_network)
  d[i]=mean(degree(random_network))
  pol[i]=((max(betweenness(random_network)) - mean(betweenness(random_network))) / mean(betweenness(random_network)))
}
mean(r)
mean(c)
mean(p)
mean(d)
mean(pol)

comp$diameter[comp$network == "Random"] <- paste0("M=",round(mean(r),2),",SD=",round(sd(r),3))
comp$transitivity[comp$network == "Random"] <- paste0("M=",round(mean(c),3),",SD=",round(sd(c),3))
comp$path.length[comp$network == "Random"] <- paste0("M=",round(mean(p),2),",SD=",round(sd(p),3))
comp$degree[comp$network == "Random"] <- paste0("M=",round(mean(d),2),",SD=",round(sd(d),3))
comp$polarization[comp$network == "Random"] <- paste0("M=",round(mean(pol),2),",SD=",round(sd(pol),3))

save(comp, file="Data/network_comparision_output.RData")



