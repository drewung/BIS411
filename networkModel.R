

#itle, author information, and a table of contents (5pts)

---
  title 
author information
table of contents
---
  
  
  
  #narrative summary of the project (1 - 2 paragraphs explaining what you did) (10pts)
  
  
  #a code chunk showing your setup process, including the packages used and reading 
  #of the data sets and creating graph objects. This section should also include a 
  #brief description of the data (what are edges and vertices in the network, who collected the data, 
  #when was it collected, any other relevant info) and include a link to access (10pts)
  
  
  #at least one network plot for each data set used, making sure to visualize the 
  #plots in an aesthetically appealing way (and more plots if your primary topic 
  #is about visualization) (20pts)
  
  
  #Note: if your emphasis is on network visualization 
  #(e.g., creating a dashboard for user exploration),
  #you should include at least one static plot of the network you are visualizing 
  #as a way to introduce the audience to the network(s) you are exploring.
  
  #code chunks that illustrate any relevant analysis that you are doing, along 
  #with narrative explanations of what the measures are telling you about the network data (20pts)
  
  #Again, if your project is primarily centered on creating a dashboard(s), 
  #this is where you should include the code for your dashboards along 
  #with narrative explanations of the features you have created. Also include some 
  #examples of how to use the dashboard.
  #a final (1-2 paragraph) summary of what you want readers to take away from your work (10pts)


  # Install required packages if needed
  # install.packages(c("nbastatR", "igraph", "dplyr", "ggplot2"))
  
library(igraph)
library(dplyr)
library(ggplot2)



install.packages("remotes")
remotes::install_github("abresler/nbastatR", force = TRUE)

library(nbastatR)
library(future)

# Use multisession for Windows parallelism (recommended instead of multiprocess)
#plan(multisession)

# Now run your data download with parallel computing enabled
game_data <- game_logs(seasons = 2020:2024)

# Create player-to-player network based on shared teams
cat("Creating player teammate network...\n")

# Get all player-team-season combinations
player_teams <- game_data %>%
  select(namePlayer, slugTeam, slugSeason) %>%
  distinct()

# Create edges between players who were teammates
teammate_pairs <- player_teams %>%
  group_by(slugTeam, slugSeason) %>%
  filter(n() > 1) %>%  # Teams with multiple players
  do({
    players <- .$namePlayer
    expand.grid(
      player1 = players,
      player2 = players,
      stringsAsFactors = FALSE
    ) %>%
    filter(player1 != player2)
  }) %>%
  group_by(player1, player2) %>%
  summarise(
    shared_teams = n(),
    teams = paste(unique(paste(slugTeam, slugSeason)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(shared_teams >= 1)  # At least one shared team

cat("Teammate connections created:", nrow(teammate_pairs), "\n")

# Sample network for analysis (too many players for full network)
# Focus on players with most connections
player_connections <- teammate_pairs %>%
  group_by(player1) %>%
  summarise(total_teammates = n(), .groups = "drop") %>%
  arrange(desc(total_teammates))

# Take top connected players for network analysis
top_players <- head(player_connections$player1, 100)

# Filter network to top players
filtered_network <- teammate_pairs %>%
  filter(player1 %in% top_players & player2 %in% top_players)

# Create network
players_network <- graph_from_data_frame(filtered_network, directed = FALSE)

# Calculate betweenness centrality (broker players)
between <- betweenness(players_network, normalized = FALSE)
bet_normalized <- betweenness(players_network, normalized = TRUE)
bet.dat <- data.frame(between, bet_normalized)

# Community detection
communities <- cluster_louvain(players_network)

# Results
print("=== TOP BROKER PLAYERS (Betweenness Centrality) ===")
print(head(bet.dat[order(bet.dat$between, decreasing = TRUE),], 10))

print("\n=== COMMUNITY STRUCTURE ===")
cat("Number of communities:", max(membership(communities)), "\n")
cat("Modularity:", round(modularity(communities), 3), "\n")

# Show communities
for(i in 1:min(5, max(membership(communities)))) {
  community_players <- V(players_network)$name[membership(communities) == i]
  cat("Community", i, ":", paste(head(community_players, 5), collapse = ", "), 
      ifelse(length(community_players) > 5, "...", ""), "\n")
}

players_network <- simplify(players_network, remove.multiple = TRUE, remove.loops = TRUE)





# Plot network
plot(players_network,
     layout = layout_with_fr(players_network),
     vertex.label = ifelse(between > quantile(between, 0.9), 
                           V(players_network)$name, NA),
     vertex.color = membership(communities),
     vertex.size = between * 0.01 + 3,
     edge.width = 0.1,
     edge.color = "gray80",
     main = "NBA Player Communities\n(Colors = Communities, Size = Betweenness)")



