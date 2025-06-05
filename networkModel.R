install.packages("remotes")
remotes::install_github("abresler/nbastatR", force = TRUE)

library(nbastatR)
library(future)
Sys.setenv(VROOM_CONNECTION_SIZE = 131072 * 10)

# Use multisession for Windows parallelism (recommended instead of multiprocess)
plan(multisession)

# Now run your data download with parallel computing enabled
game_logs(seasons = 2022:2024)

# Check draft combine data for 2025 season (or upcoming)
combine_2024 <- draft_combines(years = 2024, return_message = TRUE, nest_data = FALSE)
print(head(combine_2024))

