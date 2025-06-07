# Download nbastatR
install.packages("remotes")
remotes::install_github("abresler/nbastatR", force = TRUE)

# Load libraries
library(nbastatR)
library(future)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)

# Setup for large data set
Sys.setenv(VROOM_CONNECTION_SIZE = 131072 * 10)

# Set parallel plan to sequential
plan(sequential)

# Get game logs from 2018 to 2024 
logs <- game_logs(seasons = 2018:2024)

# Function to create NBA Draft Class Network Graphs
draft_class_network <- function(draft_years, logs) {
  
  # Get draft class players
  draft_data <- drafts(draft_years = draft_years, nest_data = FALSE, return_message = TRUE)
  
  # Filter logs for draft class players
  career_logs <- logs %>%
    filter(namePlayer %in% draft_data$namePlayer)
  
  # Create player-team-season dataframe
  player_team_season <- career_logs %>%
    select(namePlayer, slugTeam, yearSeason) %>%
    distinct()
  
  # Build edges, pair players on same team in same season
  edges_list <- list()
  
  # Get unique (team, season) combinations
  team_seasons <- unique(player_team_season[, c("slugTeam", "yearSeason")])
  
  # Loop over all team-season combinations
  for (i in 1:nrow(team_seasons)) {
    # Get current team and season
    team <- team_seasons$slugTeam[i]
    season <- team_seasons$yearSeason[i]
    
    # Get players on this team for this season
    players_in_team_season <- player_team_season %>%
      filter(slugTeam == team, yearSeason == season) %>%
      pull(namePlayer)
    
    # If 2 or more players, create edges (all possible pairs)
    if (length(players_in_team_season) >= 2) {
      player_combos <- combn(players_in_team_season, 2)
      
      # Store edges as dataframe
      edges_list[[length(edges_list) + 1]] <- data.frame(
        from = player_combos[1, ],
        to = player_combos[2, ],
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all edges into one data frame
  edges <- do.call(rbind, edges_list)
  
  # Build igraph object
  g <- graph_from_data_frame(edges, directed = FALSE)
  
  # Summarize player stats, average points, assists, total rebounds, minutes and games played
  career_stats <- career_logs %>%
    group_by(namePlayer, yearSeason) %>%
    summarize(
      pts = mean(pts, na.rm = TRUE),
      ast = mean(ast, na.rm = TRUE),
      treb = mean(treb, na.rm = TRUE),
      minutes = mean(minutes, na.rm = TRUE),
      games_played = n_distinct(idGame),
      .groups = 'drop'
    )
  
  # Aggregate stats across all season for each player
  node_stats <- career_stats %>%
    group_by(namePlayer) %>%
    summarize(
      avg_pts = mean(pts, na.rm = TRUE),
      total_games = sum(games_played),
      .groups = 'drop'
    )
  
  # Add node attributes
  V(g)$avg_pts <- node_stats$avg_pts[match(V(g)$name, node_stats$namePlayer)] # avg_pts = average points per game across career
  V(g)$total_games <- node_stats$total_games[match(V(g)$name, node_stats$namePlayer)] # total_games = total games played across career
  
  #Plot graph with ggraph
  ggraph(g, layout = "fr") + # Use Fruchterman-Reingold layout (spread out nicely)
    geom_edge_link(alpha = 0.4, color = "gray80") + # Draw edges (gray lines, semi-transparent)
    geom_node_point(aes(size = total_games, color = avg_pts), alpha = 0.9) + # Draw nodes (size = games played, color = avg pts)
    geom_node_text(aes(label = name), repel = TRUE, size = 3) + # Label nodes (with repel to avoid overlapping text!)
    scale_color_viridis_c(option = "C", direction = -1, name = "Avg PPG") + # Color scale = viridis color palette (C), reversed
    scale_size(range = c(3, 10), name = "Total Games Played") + # Node size scale
    theme_void() + # No background / axis
    ggtitle(paste(draft_years, "Draft Class Career Trajectory Network")) + # Title of the plot
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) # Title formatting (centered, bold)
}

# Run function
draft_class_network(2019, logs)




