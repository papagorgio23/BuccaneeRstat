

## Code: Jason Lee
## A.I. Sports   www.AISportsFirm.com




############################
###   Map NFL Schedule   ###
############################


# Load libraries
library(tidyverse)
library(ggimage)
library(ggthemes)
library(mapproj)



# load 2019 schedule
schedule <- read_csv("https://raw.githubusercontent.com/papagorgio23/BuccaneeRstat/master/data/nfl2019.csv")
# load nfl team data
team_info <- read_csv("https://raw.githubusercontent.com/papagorgio23/BuccaneeRstat/master/data/NFL_Team_Info.csv")


# filter to your team (Dallas in this case)
team_schedule <- schedule %>%
  filter(home_team == "DAL" | away_team == "DAL")


# change travel to 0 when we are at home
team_schedule$travel <- ifelse(team_schedule$home_team == "DAL", 0, team_schedule$travel)


# how many miles total will we travel? 18,242
sum(team_schedule$travel)


# get US map
library(maps)
us_states <- map_data("state")


# get all the teams that we want to include in the map (only the teams on our schedule)
teams <- unique(c(team_schedule$home_team, team_schedule$away_team))


# get just the needed logos for teams we play
team_info <- team_info %>%
  filter(team_code %in% teams)




######## Plot the Map


## The annotated text needs to be manually adjusted and it's really annoying...
# move the text Right = longitude + 1
# move the text Left = longitude - 1
# move the text Up = latitude + 1
# move the text Down = latitude - 1


teamcolor <- "#002244"
### Dallas Cowboys:
ggplot() + 
  geom_polygon(data = us_states, 
               mapping = aes(x = long, 
                             y = lat, 
                             group = group, 
                             fill = region), 
               color = "white", 
               size = 1) + 
  scale_fill_manual(values = rep(c("grey90"), 49)) + 
  labs(title = "Dallas Cowboys\n2019 Schedule",
       subtitle = "18,242 miles of travel",
       caption = "Data from A.I. Sports") + 
  geom_path(data = team_schedule, aes(x = home_longitude, y = home_latitude), color = teamcolor, size = 1) +
  geom_image(data = team_info, aes(x = longitude, y = latitude, image = url)) + 
  annotate("text", 
           x = team_schedule[team_schedule$week == 1, 'away_longitude'][[1]] + 5.7, 
           y = team_schedule[team_schedule$week == 1, 'away_latitude'][[1]] - 0.83, 
           label = "Giants - Week 1 (H), 9", 
           size = 1.75) +
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

