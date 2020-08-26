


#################################################

######       Weekly Matchup Graphics       ######

#################################################


library("tidyverse")
library("rvest")
library("cowplot")
library("reshape2")
library("readr")
library("glue")


###### GLOBAL VARIABLES
TEAM_1 <- "TB"
TEAM_2 <- "ATL"
WEEK_NUMBER <- 10
TEAM_1_LINE <- -6.5


## load data
nfl_team_colors <- read_csv("data/nfl_team_colors.csv")
NFL_Team_info <- read_csv("data/NFL_Team_info.csv")

## 538 Theme
theme_538 <- function(base_size = 12, font = "Impact") {

  # Text setting
  txt <- element_text(size = base_size + 2, colour = "black", face = "plain")
  bold_txt <- element_text(
    size = base_size + 2, colour = "black",
    family = "Impact", face = "bold"
  )
  large_txt <- element_text(size = base_size + 4, color = "black", face = "bold")
  big_txt <- element_text(size = base_size + 10, color = "black", face = "bold")


  theme_minimal(base_size = base_size, base_family = font) +
    theme(
      # Legend Settings
      legend.key = element_blank(),
      legend.background = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",

      # Backgrounds
      strip.background = element_blank(),
      strip.text = large_txt,
      plot.background = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),

      # Axis & Titles
      text = txt,
      axis.text = txt,
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title = bold_txt,
      plot.title = big_txt,

      # Panel
      panel.grid = element_line(colour = NULL),
      panel.grid.major = element_line(colour = "#D2D2D2"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()
    )
}


# function to get score
zscore <- function(value, valueMean, valueSD) {
  score <- (value - valueMean) / valueSD
  score <- round(pnorm(score, 0, 1) *100, 2)
  return(score)
}

## gets rid of percent sign in the dataframes
cleanPercent <- function(x) {gsub("\\%", "", x)}



## Setup
# 9 offensive and 7 defensive stats
#' 1. Offensive DVOA - rankings
#' 2. Pass DVOA - rankings
#' 3. Rush DVOA - rankings
#' 4. DSR (Successful Drives) - odrive1
#' 5. Avoid Turnovers - odrive1
#' 6. Redzone Points - odrive2
#' 7. Run Block O-line - oline
#' 8. Pass Block O-line - oline
#' 9. Pace - pace


# Get rankings
rankings_url <- "https://www.footballoutsiders.com/stats/teamoff/2019"
rankings <- read_html(rankings_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]]
rankings <- rankings[,c(2, 3, 7, 9)]
colnames(rankings) <- c("Team", "OFFENSEDVOA", "PASSOFF", "RUSHOFF")

## Get Offensive Drive Stats
odrive_url <- "https://www.footballoutsiders.com/stats/drivestatsoff/2019"
odrive1 <- read_html(odrive_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]]
odrive1 <- odrive1[,c(1, 7, 19)]

# get redzone
odrive2 <- read_html(odrive_url) %>%
  html_table(fill = TRUE) %>%
  .[[2]]
odrive2 <- odrive2[,c(1, 15)]

# get O line stats
oline_url <- "https://www.footballoutsiders.com/stats/ol/2019"
oline <- read_html(oline_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]]
oline1 <- oline[,c(2, 3)]
oline2 <- oline[,c(13, 16)]
oline1 <- oline1[-c(1, 34),]
oline2 <- oline2[-c(1, 34),]
oline <- oline1 %>%
  left_join(oline2, by = c("RUN BLOCKING" = "PASS PROTECTION"))
colnames(oline) <- c("Team", "RunBlock", "PassBlock")

# get Pace
pace_url <- "https://www.footballoutsiders.com/stats/pacestats/2019"
pace <- read_html(pace_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]]
pace <- pace[-33,c(1, 14)]
colnames(pace)[2] <- "Pace"


## Join data
Offensive <- rankings %>%
  left_join(odrive1, by = "Team") %>%
  left_join(odrive2, by = "Team") %>%
  left_join(oline, by = "Team") %>%
  left_join(pace, by = "Team")

# change to numeric data
Offensive[, 2:10] <- sapply(Offensive[, 2:10], cleanPercent)
Offensive[, 2:10] <- sapply(Offensive[, 2:10], as.numeric)
str(Offensive)


###### OFFENSIVE Rankings
# get this year averages
avg_offense <- Offensive %>%
  summarise(Offense = median(OFFENSEDVOA),
            Pass = median(PASSOFF),
            Rush = median(RUSHOFF),
            TO = median(`TO/Dr`),
            DSR = median(DSR),
            RedZone = median(`Pts/RZ`),
            RunBlock = median(RunBlock),
            PassBlock = median(PassBlock),
            Pace = median(Pace))

# get standard deviations
sd_offense <- Offensive %>%
  summarise(Offense = sd(OFFENSEDVOA),
            Pass = sd(PASSOFF),
            Rush = sd(RUSHOFF),
            TO = sd(`TO/Dr`),
            DSR = sd(DSR),
            RedZone = sd(`Pts/RZ`),
            RunBlock = sd(RunBlock),
            PassBlock = sd(PassBlock),
            Pace = sd(Pace))


# add z scores
Offensive$Offense <- zscore(Offensive$OFFENSEDVOA, avg_offense$Offense, sd_offense$Offense)
Offensive$Pass <- zscore(Offensive$PASSOFF, avg_offense$Pass, sd_offense$Pass)
Offensive$Rush <- zscore(Offensive$RUSHOFF, avg_offense$Rush, sd_offense$Rush)
Offensive$TO <- zscore(Offensive$`TO/Dr`, avg_offense$TO, sd_offense$TO)
Offensive$DSR <- zscore(Offensive$DSR, avg_offense$DSR, sd_offense$DSR)
Offensive$RedZone <- zscore(Offensive$`Pts/RZ`, avg_offense$RedZone, sd_offense$RedZone)
Offensive$RunBlock <- zscore(Offensive$RunBlock, avg_offense$RunBlock, sd_offense$RunBlock)
Offensive$PassBlock <- zscore(Offensive$PassBlock, avg_offense$PassBlock, sd_offense$PassBlock)
Offensive$Pace <- zscore(Offensive$Pace, avg_offense$Pace, sd_offense$Pace)

# Turnovers are in reverse
Offensive$TO <- 100 - Offensive$TO
Offensive$PassBlock <- 100 - Offensive$PassBlock


######## Get Global plotting variables
## get colors
TEAM_1_color <- nfl_team_colors %>%
  filter(team == TEAM_1) %>%
  pull(color)
TEAM_2_color <- nfl_team_colors %>%
  filter(team == TEAM_2) %>%
  pull(color2)

## full team name
TEAM_1_name <- NFL_Team_info %>%
  filter(team_code == TEAM_1) %>%
  pull(team)
TEAM_2_name <- NFL_Team_info %>%
  filter(team_code == TEAM_2) %>%
  pull(team)



######## Plot Offense
# OFFENSE
teams_odata <- Offensive %>%
  filter(Team %in% c(TEAM_1, TEAM_2)) %>%
  select(Team, Offense, Pace, Pass, PassBlock, Rush, RunBlock, DSR, RedZone, TO) %>%
  rename(`Successful Drives` = DSR,
         `Air Attack` = Pass,
         `Ground Attack` = Rush,
         `Red Zone Success` = RedZone,
         `Run Blocking` = RunBlock,
         `Pass Blocking` = PassBlock,
         `Avoid Turnovers` = TO) %>%
  melt(id.vars = "Team")

# Base Offensive Plot
off_plot <- teams_odata %>%
  ggplot(aes(x = Team,
             y = value,
             fill = Team,
             position = "dodge",
             group = variable)) +
  geom_col() +
  facet_grid(~variable) +
  geom_hline(yintercept = 0, color = "black", size = 2) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_538() +
  scale_fill_manual(values = c(TEAM_2_color, TEAM_1_color)) +
  labs(
    x = "",
    y = "Score",
    title = glue("Week {WEEK_NUMBER} Matchup - Offensive Stats"),
    subtitle = glue("\n{TEAM_1_name} ({TEAM_1_LINE}) vs. {TEAM_2_name}\n"),
    caption = "Data: A.I. Sports & Football Outsiders"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 30)
  )



### Plot
logo_file <- "https://raw.githubusercontent.com/papagorgio23/BuccaneeRstat/master/images/BucRstats1.png"
ggdraw() +
  draw_plot(off_plot) +
  draw_image(logo_file, x = 0.215, y = 0.985, hjust = 1, vjust = 1, width = 0.13, height = 0.17)



### DEFENSE
# Get rankings
d_rankings_url <- "https://www.footballoutsiders.com/stats/teamdef/2019"
d_rankings <- read_html(d_rankings_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]]
d_rankings <- d_rankings[,c(2, 3, 7, 9)]
colnames(d_rankings)[1] <- "Team"

## Get Defensive Drive Stats
ddrive_url <- "https://www.footballoutsiders.com/stats/drivestatsdef/2019"
ddrive1 <- read_html(ddrive_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]]
ddrive1 <- ddrive1[,c(1, 7, 19)]

# get redzone
ddrive2 <- read_html(ddrive_url) %>%
  html_table(fill = TRUE) %>%
  .[[2]]
ddrive2 <- ddrive2[,c(1, 9, 15)]

# get O line stats
dline_url <- "https://www.footballoutsiders.com/stats/dl/2019"
dline <- read_html(dline_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]]
dline1 <- dline[,c(2, 3)]
dline2 <- dline[,c(13, 16)]
dline1 <- dline1[-c(1, 34),]
dline2 <- dline2[-c(1, 34),]
dline <- dline1 %>%
  left_join(dline2, by = c("RUN BLOCKING" = "PASS PROTECTION"))
colnames(dline) <- c("Team", "RunStuff", "PassRush")


## Join data
Defensive <- d_rankings %>%
  left_join(ddrive1, by = "Team") %>%
  left_join(ddrive2, by = "Team") %>%
  left_join(dline, by = "Team")

# change to numeric data
Defensive[, 2:10] <- sapply(Defensive[, 2:10], cleanPercent)
Defensive[, 2:10] <- sapply(Defensive[, 2:10], as.numeric)
str(Defensive)


###### OFFENSIVE Rankings
# get this year averages
avg_defense <- Defensive %>%
  summarise(Defense = median(DEFENSEDVOA),
            Pass = median(PASSDEF),
            Rush = median(RUSHDEF),
            TO = median(`TO/Dr`),
            DSR = median(DSR),
            RedZone = median(`Pts/RZ`),
            RunStuff = median(RunStuff),
            PassRush = median(PassRush),
            Pace = median(`3Outs/Dr`))

# get standard deviations
sd_defense <- Defensive %>%
  summarise(Defense = sd(DEFENSEDVOA),
            Pass = sd(PASSDEF),
            Rush = sd(RUSHDEF),
            TO = sd(`TO/Dr`),
            DSR = sd(DSR),
            RedZone = sd(`Pts/RZ`),
            RunStuff = sd(RunStuff),
            PassRush = sd(PassRush),
            Pace = sd(`3Outs/Dr`))


# add z scores
Defensive$Defense <- zscore(Defensive$DEFENSEDVOA, avg_defense$Defense, sd_defense$Defense)
Defensive$Pass <- zscore(Defensive$PASSDEF, avg_defense$Pass, sd_defense$Pass)
Defensive$Rush <- zscore(Defensive$RUSHDEF, avg_defense$Rush, sd_defense$Rush)
Defensive$TO <- zscore(Defensive$`TO/Dr`, avg_defense$TO, sd_defense$TO)
Defensive$DSR <- zscore(Defensive$DSR, avg_defense$DSR, sd_defense$DSR)
Defensive$RedZone <- zscore(Defensive$`Pts/RZ`, avg_defense$RedZone, sd_defense$RedZone)
Defensive$RunStuff <- zscore(Defensive$RunStuff, avg_defense$RunStuff, sd_defense$RunStuff)
Defensive$PassRush <- zscore(Defensive$PassRush, avg_defense$PassRush, sd_defense$PassRush)
Defensive$Pace <- zscore(Defensive$`3Outs/Dr`, avg_defense$Pace, sd_defense$Pace)

# Turnovers are in reverse
Defensive$Defense <- 100 - Defensive$Defense
Defensive$Pass <- 100 - Defensive$Pass
Defensive$Rush <- 100 - Defensive$Rush
Defensive$DSR <- 100 - Defensive$DSR
Defensive$RedZone <- 100 - Defensive$RedZone
Defensive$RunStuff <- 100 - Defensive$RunStuff



######## Plot Defense
# Defense
teams_ddata <- Defensive %>%
  filter(Team %in% c(TEAM_1, TEAM_2)) %>%
  select(Team, Defense, Pace, Pass, Rush, PassRush, RunStuff, DSR, RedZone, TO) %>%
  rename(`Drive Stopper` = DSR,
         `3&Outs` = Pace,
         `Air Defense` = Pass,
         `Ground Defense` = Rush,
         `Red Zone D` = RedZone,
         `Run Stuffer` = RunStuff,
         `Pass Rush` = PassRush,
         `Force Turnovers` = TO) %>%
  melt(id.vars = "Team")

def_plot <- teams_ddata %>%
  ggplot(aes(x = Team, y = value, fill = Team, position = "dodge", group = variable)) +
  geom_col() +
  facet_grid(~variable) +
  geom_hline(yintercept = 0, color = "black", size = 2) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_538() +
  scale_fill_manual(values = c(TEAM_2_color, TEAM_1_color)) +
  labs(
    x = "",
    y = "Score",
    title = glue("Week {WEEK_NUMBER} Matchup - Defensive Stats"),
    subtitle = glue("\n{TEAM_1_name} ({TEAM_1_LINE}) vs. {TEAM_2_name}\n"),
    caption = "Data: A.I. Sports & Football Outsiders"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 30)
  )


### Plot
logo_file <- "https://raw.githubusercontent.com/papagorgio23/BuccaneeRstat/master/images/BucRstats1.png"
ggdraw() +
  draw_plot(def_plot) +
  draw_image(logo_file, x = 0.215, y = 0.985, hjust = 1, vjust = 1, width = 0.13, height = 0.17)

