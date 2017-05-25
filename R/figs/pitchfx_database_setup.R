# --------------------------------------------------------------
# FIGURE: Create and setup PITCHf/x database
#
# TITLE: Effects of Pitch Location and Count on 
# Professional Baseball Umpires' Ball/Strike Decisions
#
# APS 2017
# Aaron R. Baggett, Ph.D.
# May 27, 2017
# --------------------------------------------------------------

# Load package libraries
library(dplyr)
library(pitchRx)

# Set and create working directory
setwd("~/your/working/directory")

# Initialize SQLite database
# NOTE: You may replace `db` with year (e.g., pfx_16)
pfx_db <- src_sqlite("pfx_16.sqlite3", create = TRUE)

# Set XML files to collect
files <- c("inning/inning_all.xml", "players.xml", "miniscoreboard.xml")

# Scrape data
scrape(start = "YYY-MM-DD", end = "YYY-MM-DD", suffix = files, connect = pfx_db$con)

# Verify *pfx_db* tables
pfx_db

# Load *pfx_db* DB
pfx_db <- src_sqlite("~/your/working/directory/pfx_db.sqlite3")

# Convert *pfx_db* tables to standalone tables
action <- tbl(pfx_db, "action")
atbat <- tbl(pfx_db, "atbat")
coach <- tbl(pfx_db, "coach")
game <- tbl(pfx_db, "game")
media <- tbl(pfx_db, "media")
pitch <- tbl(pfx_db, "pitch")
player <- tbl(pfx_db, "player")
po <- tbl(pfx_db, "po")
runner <- tbl(pfx_db, "runner")
umpire <- tbl(pfx_db, "umpire")

# Filtering and manipulation operations
# *atbat* table
atbats <- atbat %>%
  select(num, stand, b_height, batter_name, gameday_link) %>%
  group_by(gameday_link)
atbats <- collect(atbats, n = Inf)

# *pitch* table
# All pitches recorded in 2016 (N = 726,789)
all_pitches <- pitch %>%
  select(call = des, sz_top, sz_bot, px, pz, zone, num, count, gameday_link) %>%
  group_by(gameday_link)
all_pitches <- collect(all_pitches, n = Inf)

# Only pitches requring umpire decision (n = 365,732)
pitch_decs <- pitch %>%
  select(call = des, sz_top, sz_bot, px, pz, zone, num, count, gameday_link) %>%
  filter(call == "Called Strike" | call == "Ball") %>%
  group_by(gameday_link)
pitch_decs <- collect(pitch_decs, n = Inf)

# *umpire* table
umpires <- umpire %>%
  select(position, umpire = name, gameday_link) %>%
  filter(position == "home") %>%
  group_by(gameday_link)
umpires <- collect(umpires, n = Inf)

# Join *atbat*, *pitch*, and *umpire* by *gameday_link*
ps_abs <- left_join(pitch_decs, atbats, by = c("num", "gameday_link"))
ps_abs_us <- left_join(ps_abs, umpires, by = "gameday_link", copy = TRUE)
pfx_16 <- tbl_df(as.data.frame(ps_abs_us, n = -1))

# Save standalone tables as .Rda files
all_pitches <- tbl_df(all_pitches)
save(pitch, file = "~/your/working/directory/all_pitches.Rda")

pitch_decs <- tbl_df(pitch_decs)
save(pitch, file = "~/your/working/directory/pitch_decs.Rda")

atbat <- tbl_df(atbat)
save(atbat, file = "~/your/working/directory/atbat.Rda")

umpire <- tbl_df(umpire)
save(umpire, file = "~/your/working/directory/umpire.Rda")

# Customize *pfx_16* for full analyses
# add *game_date* variable
pfx_16$game_date <- substr(pfx_16$gameday_link, start = 5, stop = 14)

# Function for including only complete cases by column
# Source: http://stackoverflow.com/a/11258247/1656111
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Omit missing elements of *px* and *pz*
pfx_16 <- completeFun(pfx_16, c("px", "pz"))

# Add player stike zone limits by *b_height*
pfx_16 <- pfx_16 %>%
  group_by(b_height) %>%
  mutate(player_sz_bot = mean(sz_bot)) %>%
  mutate(player_sz_top = mean(sz_top))

# Create *u_test* variable for umpire's decision [1 = correct]
# Width based on plate width (17")
pfx_16$u_test_17 <- with(pfx_16,
    ifelse(call == "Ball" & px < -0.7083333 | px > 0.7083333 | 
      pz < sz_bot | pz > sz_top, 1,
    ifelse(call == "Called Strike" & pz > sz_bot & pz < sz_top & 
      px >= -0.7083333 & px <= 0.7083333, 1,
    ifelse(call == "Ball" & pz > sz_bot & pz < sz_top & 
      px > -0.7083333 & px < 0.7083333, 0,
    ifelse(call == "Called Strike" & px < -0.7083333 | px > 0.7083333 | 
      pz < sz_bot | pz > sz_top, 0, 99)))))

# Create *u_test_adj* variable for correct/incorrect decision
# Width based on ball radius = ((1.57*2 + 17) / 12) / 2
pfx_16$u_test_adj <- with(pfx_16,
  ifelse(call == "Ball" & px < -0.8391667 | px > 0.8391667 | 
      pz < player_sz_bot | pz > player_sz_top, 1,
    ifelse(call == "Called Strike" & pz > player_sz_bot & pz < player_sz_top & 
        px >= -0.8391667 & px <= 0.8391667, 1,
      ifelse(call == "Ball" & pz > player_sz_bot & pz < player_sz_top & 
          px > -0.8391667 & px < 0.8391667, 0,
        ifelse(call == "Called Strike" & px < -0.8391667 | px > 0.8391667 |
            pz < player_sz_bot | pz > player_sz_top, 0, 99)))))

# Specify *count* advantages
# 1. Add *bs_count* variable to indicate who has the advantage (p vs. b)
# Based on Marchi & Albert, 2014
pfx_16$bs_count <- with(pfx_16,

  # 1.1 Neutral
  ifelse(count == "0-0" | count == "1-0" | 
         count == "1-1" | count == "2-1", "neutral",

  # 1.2 Batter
  ifelse(count == "2-0" | count == "3-0" | 
		 count == "3-1" | count == "3-2", "batter",

  # 1.3 Pitcher
  ifelse(count == "0-1" | count == "0-2" | 
         count == "1-2" | count == "2-2", "pitcher", 99)
)))

# 2. Convert *count* to factor
pfx_16$bs_count <- as.factor(pfx_16$bs_count)

# Respecify *zone* regions
pfx_16$zone_reg <- with(pfx_16,

  # 1. RHBs
  ifelse(stand == "R" & zone == "1" | 
		 stand == "R" & zone == "4" | 
         stand == "R" & zone == "7", "inner",

  ifelse(stand == "R" & zone == "2" | 
         stand == "R" & zone == "5" | 
         stand == "R" & zone == "8", "middle",

  ifelse(stand == "R" & zone == "3" | 
         stand == "R" & zone == "6" | 
         stand == "R" & zone == "9", "outer",

  # 2. LHBs
  ifelse(stand == "L" & zone == "1" | 
         stand == "L" & zone == "4" | 
         stand == "L" & zone == "7", "outer",

  ifelse(stand == "L" & zone == "2" | 
         stand == "L" & zone == "5" | 
         stand == "L" & zone == "8", "middle",

  ifelse(stand == "L" & zone == "3" | 
         stand == "L" & zone == "6" | 
         stand == "L" & zone == "9", "inner", "ball")))))))

# 3. Convert *zone* to factor
pfx_16$zone_reg <- as.factor(pfx_16$zone_reg)

# Relevel "zone_reg" to make "ball" reference group
pfx_16$zone_reg <- relevel(pfx_16$zone_reg, ref = "ball")

# Relevel "bs_count" to make "neutral" reference group
pfx_16 $bs_count <- relevel(pfx_16$bs_count, ref = "neutral")

# Rearrange variables
pfx_16 <- pfx_16 %>% 
  select(c(umpire, position, batter_name, stand, b_height, sz_top, sz_bot, 
    player_sz_top, player_sz_bot, count, bs_count, px, pz, zone, zone_reg,
    call, u_test_17, u_test_adj, num, game_date, gameday_link))

# Save final version of *pfx_16* as .Rda file
save(pfx_16, file = "~/your/working/directory/pfx_16.Rda")
