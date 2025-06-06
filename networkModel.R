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
plan(sequential)

# Get game logs from 2018 to 2024
logs <- game_logs(seasons = 2018:2024)

# Get 2018 draft class players
draft_2018 <- drafts(draft_years = 2018, nest_data = FALSE, return_message = TRUE)

# Filter logs for draft class players
career_logs_2018 <- logs %>%
  filter(namePlayer %in% draft_2018$namePlayer)

# Create player-team-season dataframe
player_team_season <- career_logs_2018 %>%
  select(namePlayer, slugTeam, yearSeason) %>%
  distinct()

# Build edges (base R version): pair players on same team in same season
edges_list <- list()

team_seasons <- unique(player_team_season[, c("slugTeam", "yearSeason")])

for (i in 1:nrow(team_seasons)) {
  team <- team_seasons$slugTeam[i]
  season <- team_seasons$yearSeason[i]
  
  players_in_team_season <- player_team_season %>%
    filter(slugTeam == team, yearSeason == season) %>%
    pull(namePlayer)
  
  if (length(players_in_team_season) >= 2) {
    player_combos <- combn(players_in_team_season, 2)
    edges_list[[length(edges_list) + 1]] <- data.frame(
      from = player_combos[1, ],
      to   = player_combos[2, ],
      stringsAsFactors = FALSE
    )
  }
}

# Combine all edges into one data frame
edges <- do.call(rbind, edges_list)

# Build igraph object
g <- graph_from_data_frame(edges, directed = FALSE)

# Optional: add average PTS and Games played as node attributes
career_stats_2018 <- career_logs_2018 %>%
  group_by(namePlayer, yearSeason) %>%
  summarize(
    pts = mean(pts, na.rm = TRUE),
    ast = mean(ast, na.rm = TRUE),
    treb = mean(treb, na.rm = TRUE),
    minutes = mean(minutes, na.rm = TRUE),
    games_played = n_distinct(idGame),
    .groups = 'drop'
  )

node_stats <- career_stats_2018 %>%
  group_by(namePlayer) %>%
  summarize(
    avg_pts = mean(pts, na.rm = TRUE),
    total_games = sum(games_played),
    .groups = 'drop'
  )

# Add node attributes
V(g)$avg_pts <- node_stats$avg_pts[match(V(g)$name, node_stats$namePlayer)]
V(g)$total_games <- node_stats$total_games[match(V(g)$name, node_stats$namePlayer)]

layout_fr <- layout_with_fr(g)
plot(g,
     layout = layout_fr,
     main = "2018 Draft Class Career Trajectory (2018-2022)",
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.size = sqrt(V(g)$total_games) * 1.5,
     vertex.color = heat.colors(length(V(g)))[rank(V(g)$avg_pts)],
     edge.color = "gray80")

ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.4, color = "gray") +
  geom_node_point(aes(size = total_games, color = avg_pts)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +  # <-- repel = TRUE!
  scale_color_viridis_c(option = "C") +
  theme_void() +
  ggtitle("2018 Draft Class Career Trajectory Network (Spread and Readable)")





