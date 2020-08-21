


## Code: Jason Lee
## A.I. Sports  www.AISportsfirm.com



#########################################
####   Animated QB EPA Game Visual   ####
#########################################


## Load libraries
library("tidyverse")
library("dplyr")
library("ggimage")
library("teamcolors")
library("nflscrapR")
library("gganimate")
library("RCurl")
library("png")
library("grid")



#########################################
####    Relive Birth of FitzMagic    ####
#########################################




## Week 1 2018 vs Drew Brees


## Load data
play_by_play2018 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))


## filter to specific game
game1 <- play_by_play2018 %>%
  filter(game_id == "2018090906")


## Fitz Plays
FitzMagic <- game1 %>%
  filter(str_detect(desc, "Fitz")) %>%
  filter(!is.na(epa)) %>%
  arrange(desc(game_seconds_remaining)) %>%
  select(epa, posteam) %>%
  mutate(Total_EPA = cumsum(epa),
         Play_Num = row_number(),
         QB = "FitzMagic",
         url = "https://raw.githubusercontent.com/papagorgio23/BuccaneeRstat/master/images/fitzmagic.png")

## Brees Plays
Brees <- game1 %>%
  filter(str_detect(desc, "Brees")) %>%
  filter(!is.na(epa)) %>%
  arrange(desc(game_seconds_remaining)) %>%
  select(epa, posteam) %>%
  mutate(Total_EPA = cumsum(epa),
         Play_Num = row_number(),
         QB = "Brees",
         url = "https://raw.githubusercontent.com/papagorgio23/BuccaneeRstat/master/images/brees.png")



# combine qb datasets
BothQB <- rbind(FitzMagic, Brees)


# get logo
logo_url <- "https://raw.githubusercontent.com/papagorgio23/BuccaneeRstat/master/images/Tampa.png"
logo <-  png::readPNG(getURLContent(logo_url))
rast <- grid::rasterGrob(logo, interpolate = T)

# plot game
ggplot(BothQB, aes(Play_Num, Total_EPA, group = QB)) +
  geom_line(aes(color = QB, size = 2), show.legend = FALSE) +
  scale_color_manual(values = c("#9f8958", "#d50a0a")) +
  geom_segment(aes(xend = 51, yend = Total_EPA), linetype = 2, colour = 'grey') +
  geom_point(size = 2) +
  geom_image(aes(image = url), size = 0.1) +
  geom_text(aes(x = 51.1, label = QB), hjust = 0) +
  coord_cartesian(clip = 'off') +
  labs(title = 'Week 1 - Bucs vs Saints (48 - 40)',
       subtitle =  'FitzMagic: 21/28 417yds 4 TD 0 INT \nBrees: 37/45 439yds 3 TD 0 INT',
       y = 'Total EPA',
       x = 'Play Number',
       caption = "Data = nflscrapR") +
  transition_reveal(Play_Num) +
  theme_minimal() +
  annotation_custom(rast, xmin=1, xmax=10, ymin=29, ymax=36)


anim_save("Week1 Fitz.gif", animation = last_animation())


