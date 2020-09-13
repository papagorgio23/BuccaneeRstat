
## Code: Jason Lee
## A.I. Sports  www.AISportsfirm.com



############################
###   Map NFL Schedule   ###
############################



# load libraries
library("gmt")
library("maps")
library("ggthemes")
library("glue")
library("nflfastR")
library("tidyverse")


# load nfl team data
team_logos <- nflfastR::teams_colors_logos
team_info <- read_csv("https://raw.githubusercontent.com/papagorgio23/BuccaneeRstat/master/data/NFL_Team_Info.csv")

head(team_info)
team_info <- team_info %>%
  mutate(team_code = ifelse(team_code == "OAK", "LV", team_code),
         team = ifelse(team_code == "LV", "Las Vegas Raiders", team),
         longitude = ifelse(team_code == "LV", -115.1833, longitude),
         latitude = ifelse(team_code == "LV", 36.0909, latitude),)

# join data
team_info <- team_info %>%
  left_join(team_logos, by = c("team_code" = "team_abbr"))

# create home and away team info
home_teams <- team_info
away_teams <- team_info


# rename data for home and away teams
names <- colnames(team_info)
home_names <- paste0("home_", names)
away_names <- paste0("away_", names)
colnames(home_teams) <- home_names
colnames(away_teams) <- away_names



##### Updated 2020 Maps
all_games <- nflfastR::fast_scraper_schedules(2020)

# filter to team's games
team_schedule <- all_games %>%
  filter(home_team == "TB" | away_team == "TB")

# join info with schedule
team_schedule <- team_schedule %>%
  left_join(home_teams, by = c("home_team" = "home_team_code")) %>%
  left_join(away_teams, by = c("away_team" = "away_team_code"))

# calculate distances
team_schedule <- team_schedule %>%
  mutate(
    travel = 2 * round(1.151 * geodist(
      Nfrom = away_latitude,
      Efrom = away_longitude,
      Nto = home_latitude,
      Eto = home_longitude,
      units = 'nm'
    )),
    travel = ifelse(home_team == "TB", 0, travel)
  )



# how many miles total will we travel? 18,242
total_miles <- scales::comma(sum(team_schedule$travel))


# get US map
us_states <- map_data("state")


# get all the teams that we want to include in the map (only the teams on our schedule)
teams <- unique(c(team_schedule$home_team, team_schedule$away_team))


# get just the needed logos for teams we play
team_info1 <- team_info %>%
  filter(team_code %in% teams)



######## Plot the Map


## The annotated text needs to be manually adjusted and it's really annoying...
# move the text Right = longitude + 1
# move the text Left = longitude - 1
# move the text Up = latitude + 1
# move the text Down = latitude - 1


teamcolor <- team_info %>%
  filter(team_code == "TB") %>%
  pull(team_color)
### Tampa Bay Bucs 2020:
ggplot() +
  geom_polygon(data = us_states,
               mapping = aes(x = long,
                             y = lat,
                             group = group,
                             fill = region),
               color = "white",
               size = 1) +
  scale_fill_manual(values = rep(c("grey90"), 49)) +
  labs(title = "Tampa Bay Buccaneers\n2020 Schedule",
       subtitle = glue("{total_miles} miles of travel"),
       caption = "Data from A.I. Sports") +
  geom_path(data = team_schedule, aes(x = home_longitude, y = home_latitude), color = teamcolor, size = 1) +
  geom_image(data = team_info1, aes(x = longitude, y = latitude, image = team_logo_wikipedia)) +
  annotate("text",
           x = team_schedule[team_schedule$week == 1, 'home_longitude'][[1]],
           y = team_schedule[team_schedule$week == 1, 'home_latitude'][[1]] - 0.83,
           label = "Saints - Week 1, 9 (H)",
           size = 1.75) #+
  annotate("text",
           x = team_schedule[team_schedule$week == 2, 'home_longitude'][[1]] + 1.3,
           y = team_schedule[team_schedule$week == 2, 'home_latitude'][[1]] - 1,
           label = "Week 2, 17 (H)",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 3, 'away_longitude'][[1]],
           y = team_schedule[team_schedule$week == 3, 'away_latitude'][[1]] + 1,
           label = "Week 3 (H)",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 4, 'home_longitude'][[1]],
           y = team_schedule[team_schedule$week == 4, 'home_latitude'][[1]] - 1,
           label = "Week 4",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 5, 'away_longitude'][[1]],
           y = team_schedule[team_schedule$week == 5, 'away_latitude'][[1]] + 1,
           label = "Week 5 (H)",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 6, 'home_longitude'][[1]] + 4,
           y = team_schedule[team_schedule$week == 6, 'home_latitude'][[1]] - 1.5,
           label = "Jets - Week 6",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 7, 'away_longitude'][[1]] + 5.3,
           y = team_schedule[team_schedule$week == 7, 'away_latitude'][[1]] - 1.75,
           label = "Eagles - Week 7 (H), 16",
           size = 1.75) +
  annotate("text",
           x = team_info[team_info$team_code == "DAL", 'longitude'][[1]],
           y = team_info[team_info$team_code == "DAL", 'latitude'][[1]] - 1.1,
           label = "Week 8 (Bye)",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 10, 'away_longitude'][[1]],
           y = team_schedule[team_schedule$week == 10, 'away_latitude'][[1]] + 1.1,
           label = "Week 10 (H)",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 11, 'home_longitude'][[1]],
           y = team_schedule[team_schedule$week == 11, 'home_latitude'][[1]] + 1.1,
           label = "Week 11",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 12, 'home_longitude'][[1]],
           y = team_schedule[team_schedule$week == 12, 'home_latitude'][[1]] + 1.1,
           label = "Week 12",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 13, 'away_longitude'][[1]],
           y = team_schedule[team_schedule$week == 13, 'away_latitude'][[1]] + 1.1,
           label = "Week 13 (H)",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 14, 'home_longitude'][[1]],
           y = team_schedule[team_schedule$week == 14, 'home_latitude'][[1]] + 1,
           label = "Week 14",
           size = 1.75) +
  annotate("text",
           x = team_schedule[team_schedule$week == 15, 'away_longitude'][[1]],
           y = team_schedule[team_schedule$week == 15, 'away_latitude'][[1]] + 1.1,
           label = "Week 15 (H)",
           size = 1.75) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE) +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5, color = teamcolor, size=22, face="bold.italic"),
        plot.caption = element_text(hjust = 0.1),
        plot.subtitle = element_text(hjust = 0.5, color = teamcolor, face = "italic"))
