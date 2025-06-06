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
This project analyzes NBA player career trajectories and teammate networks using 
network analysis. We focus on the ____ draft class and ___ season teammates to 
explore how players connect through shared teams, and use network metrics and 
community detection to identify key players and roles. We also will be exploring 
"broker" players The goal is to visualize and interpret player dynamics 
through graph structures.

We use `nbastatR` to access real NBA data, and `igraph`, `ggraph`, and `dplyr` 
for data wrangling, network creation, and visualization. This work could help 
illustrate team structures and player centrality to coaches, analysts, or fans.
## Setup Process

```{r setup}
#installing packages
install.packages("remotes")
remotes::install_github("abresler/nbastatR", force = TRUE)

```

### Packages Used

```{r packages}
#packages needed
library(devtools)
library(dplyr)
library(future)
library(ggplot2)
library(igraph)
library(nbastatR)
library(Rtools)
```

### Reading the Data Sets

```{r datasets}
#Adjust VROOM buffer size for large data!
Sys.setenv(VROOM_CONNECTION_SIZE = 131072 * 10)

#change season to anything from (ex 2020:2024, from 2020 to 2024 seasons)
game_data <- game_logs(seasons = 2024)
```

### Creating Graph Objects

```{r graphObjects}


```

### Description of the Data and Link

## Network Plot

## Final Summary
