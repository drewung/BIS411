---
title: "NBA Draft Network Analysis Project"
author: "Marcus Li and Andrew Ung"
date: "6/6/2025"
output:
  html_document:
    toc: true
    toc_depth: 2
    toc_float: true
    theme: readable
    highlight: tango
---

## Narrative Summary

This project analyzes NBA player career trajectories and teammate networks using network analysis. We focus on the \_\_\_ draft class and \_\_\_ season teammates to explore how players connect through shared teams, and use network metrics and community detection to identify key players and roles. We also will be exploring "broker" players, using Gould-Fernandez Brokerage with the RNA package.

The goal is to visualize and interpret player dynamics through graph structures.

Our main package for data is going to be `nbastatR` to access real NBA data, and `igraph`, `ggraph`, and `dplyr` for data wrangling, network creation, and visualization. This work could help illustrate team structures and player centrality to coaches, analysts, or fans.

## Setup Process

```{r setup}
#installing packages
install.packages("remotes")
remotes::install_github("abresler/nbastatR", force = TRUE)

```

### Packages Used

```{r packages}
#dataset package
library(nbastatR)

#packages needed
library(devtools)
library(dplyr)
library(future)
library(ggplot2)
library(igraph)
library(Rtools)
library(knitr)

# For brokerage analysis
library(sna)

```

### Reading in the Data Sets

```{r datasets}
#Adjust VROOM buffer size for large data!
Sys.setenv(VROOM_CONNECTION_SIZE = 131072 * 10)
#2022:2024) dennis
#change season to anything from (ex 2020:2024, from 2020 to 2024 seasons)
game_data <- game_logs(seasons = 2020:2024)
```

### Creating Graph Objects

```{r graphObjects}
# Create player-to-player network based on shared teams
cat("Creating player teammate network...\n")

# Unique player-team-season combos
player_teams <- game_data %>%
  select(namePlayer, slugTeam, slugSeason) %>%
  distinct()
  
# Create teammate pairs per team-season
teammate_pairs <- player_teams %>%
  group_by(slugTeam, slugSeason) %>%
  filter(n() > 1) %>%
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
  filter(shared_teams >= 1)

cat("Teammate connections created:", nrow(teammate_pairs), "\n")
```

### Filtering

```{r filtering}
cat("Filtering connections betwene players...\n")
# most connected players
player_connections <- teammate_pairs %>%
  group_by(player1) %>%
  summarise(total_teammates = n(), .groups = "drop") %>%
  arrange(desc(total_teammates))

# Filter top 100 players by connections
top_players <- head(player_connections$player1, 100)

filtered_network <- teammate_pairs %>%
  filter(player1 %in% top_players & player2 %in% top_players)

#Creates graph + simplfly (remove loops and multiple edges)
players_network <- graph_from_data_frame(filtered_network, directed = FALSE)
players_network <- simplify(players_network)


```

### Detection

```{r detection}
# Detect communities using Louvain algorithm
communities <- cluster_louvain(players_network)
V(players_network)$community <- membership(communities_louvain)

#Calculate betweenness centrality
between <- igraph::betweenness(players_network)

# Print top 10 players by betweenness
at("=== (Betweenness Centrality) ===\n")
print(head(bet.dat[order(bet.dat$between, decreasing = TRUE),], 10))

# Community summary
cat("\n=== COMMUNITY STRUCTURE ===\n"
cat("Modularity:", round(modularity(communities), 3), "\n")

# Members of community
for(i in 1:min(5, max(membership_vec))) {
  community_players <- V(players_network)$name[membership_vec == i]
  cat("Community", i, ":", paste(head(community_players, 5), collapse = ", "), 
      ifelse(length(community_players) > 5, "...", ""), "\n")
}
```

### Brokerage Analysis

Identifies players who connect others across different groups. Measures 5 broker types:

**Coordinator:** Connects players within same group (A→A→A)\
**Consultant:** Outsider connecting group members (A→B→A)\
**Gatekeeper:** Controls incoming connections (A→B→B)\
**Representative:** Controls outgoing connections (A→A→B)\
**Liaison:** Connects different groups (A→B→C)

Values are also normalized + rounded to two digits

Z-scores show statistical significance - values \>1.96 are unusually high brokers for that role. (sourced from ?brokerage command)

Here are the resources I used to help me,

<https://www.rdocumentation.org/packages/sna/versions/2.8/topics/brokerage>

<https://rstudio-pubs-static.s3.amazonaws.com/320424_f1e3875c90dd489983f92c7628164bdc.html#Brokerage_Roles>

```{r Brokerage}

#MAKE SURE TO RUN THIS LINE, aS IT WILL CONFUSE SAME FUNCTIONS FROM EARLIER PACKAGES
install.packages("intergraph", dependencies = TRUE)

#PACKAGES
library(intergraph)
library(statnet)
library(knitr)
library(dplyr)
# Convert igraph to sna network
snaNetwork <- asNetwork(players_network)

# Confirm the network size matches igraph vertex count
stopifnot(network.size(snaNetwork) == length(V(players_network)))

#Assign communities as vertex attributes
community_named <- membership(communities)
names(community_named) <- V(players_network)$name

#Assign community attribute to the network vertices by matching names
set.vertex.attribute(
  snaNetwork,
  "community",
  value = community_named[get.vertex.attribute(snaNetwork, "vertex.names")]
)

#  Check to see if community listed
print(table(get.vertex.attribute(snaNetwork, "community")))


# Create dataframe from brokerage anaylsis result, 2 DIGITS
brokerage_df <- round(brokerage(snaNetwork, cl=get.vertex.attribute(snaNetwork, "community"))$z.nli, 2) 

#Here is the un-rounded non-normalized version of above
#brokerage_dftest <- brokerage_results$raw.nli

#Collumn names readability
colnames(brokerage_df) <- c(
  "Coordinator",     # was w_I
  "Consultant",      # was w_O
  "Representative",  # was b_IO
  "Gatekeeper",      # was b_OI
  "Liaison",         # was b_O
  "Total"            # was t
)

# Show all data sorted by Total
print("=== ALL PLAYERS BY TOTAL BROKERAGE ===")
print(brokerage_df[order(brokerage_df[, "Total"], decreasing = TRUE), ])

# Show top 5 by Total
print("=== TOP 5 TOTAL BROKERS ===")
print(head(brokerage_df[order(brokerage_df[, "Total"], decreasing = TRUE), ], 5))

# Show top 3 players in each category
print("=== TOP 3 PLAYERS BY CATEGORY ===")
for(col in colnames(brokerage_df)) {
 print(paste("=== TOP 3", col, "==="))
 sorted <- brokerage_df[order(brokerage_df[, col], decreasing = TRUE), ]
 top_3 <- head(sorted[, col, drop = FALSE], 3)
 for(i in 1:nrow(top_3)) {
   print(paste(i, ".", rownames(top_3)[i], "(", top_3[i, 1], ")"))
 }
 writeLines("")  # blank line
}
```

### Description of the Data and Link

One example I tried was the seasons (2016:2024),

![](images/Screenshot%202025-06-06%20185157-01.png)

"...Because even though Jeff Green’s journeyman path through the NBA has been un-sexy, he’s been able to keep a job in the league for 15 seasons, playing for eleven teams, and collecting a lot of paychecks. Jeff Green is the opposite of a “star” and there’s actually something cool about that." - BasketBallxFeelings

<https://www.reddit.com/r/nbadiscussion/comments/15jadhd/jeff_green_is_kinda_an_nba_legend/>

As noted in a Reddit post, Jeff Green has carved out a long career as a journeyman playing for 11 teams over 15 seasons. Our brokerage analysis supports this narrative, showing he holds the highest total score across multiple connective roles in the league.

Another example I analyzed was from the seasons 2020 to 2024.

![](images/clipboard-3321849320.png)

This yielded interesting results. Most top ranked players in this window, like Wenyen Gabriel and Troy Brown Jr, fit the journeyman profile.

James Harden stood out. Despite being a star player, Harden appears with a high brokerage role total (2.74), indicating that even elite players can take on connector roles across multiple teams. His strong scores in coordinator and gatekeeper roles reflect his influence beyond just scoring.

![](images/Screenshot%202025-06-06%20185806-01.png)

<https://www.reddit.com/r/nba/comments/oyk95z/dennis_schroder_is_running_out_of_options_on/> <https://www.reddit.com/r/nba/comments/1hegyvq/slater_dennis_schroder_has_now_changed_teams/>

## Network Plot

```{r plotting}
# Find the top player in each community
community_tops <- by(1:length(V(players_network)), membership(communities), function(idx) {
  community_betweenness <- between[idx]
  idx[which.max(community_betweenness)]
})

# Convert to vector of indices
top_in_each_community <- unlist(community_tops)

# Create label color and font vectors
label_colors <- rep("black", length(V(players_network)))
label_fonts <- rep(1, length(V(players_network)))

# Top player in each community - red and bold
label_colors[top_in_each_community] <- "blue"
label_fonts[top_in_each_community] <- 2

# Plot network
plot(players_network,
     layout = layout_with_kk(players_network),
     vertex.label = ifelse(between > quantile(between, 0.1), 
                           V(players_network)$name, NA),
     vertex.label.color = label_colors,
     vertex.label.font = label_fonts,
     vertex.label.cex = 0.8,
     vertex.color = membership(communities),
     vertex.size = between * 0.01 + 3,
     edge.width = 0.1,
     edge.color = "gray80",
     main = "NBA Player Communities\n(Red Labels = Community Leaders, Colors = Communities)")

```

## Final Summary
