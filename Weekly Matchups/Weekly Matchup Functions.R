
### Functions to get data

# load libraries
library("tidyverse")
library("rvest")
library("glue")



### OFFENSIVE RANKINGS
get_fo_off <- function(year = 2019){
  # Get rankings
  rankings_url <- glue("https://www.footballoutsiders.com/stats/teamoff/{year}")
  rankings <- read_html(rankings_url) %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  rankings <- rankings[,c(2, 3, 7, 9)]
  colnames(rankings) <- c("Team", "OFFENSEDVOA", "PASSOFF", "RUSHOFF")


  ## Get Offensive Drive Stats
  odrive_url <- glue("https://www.footballoutsiders.com/stats/drivestatsoff/{year}")
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
  oline_url <- glue("https://www.footballoutsiders.com/stats/ol/{year}")
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
  pace_url <- glue("https://www.footballoutsiders.com/stats/pacestats/{year}")
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
    left_join(pace, by = "Team") %>%
    mutate(Year = year)

  # change to numeric data
  Offensive[, 2:10] <- sapply(Offensive[, 2:10], cleanPercent)
  Offensive[, 2:10] <- sapply(Offensive[, 2:10], as.numeric)

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
  Offensive$PassBlock <- zscore(Offensive$PassBlock, avg_offense$PassBlock, sd_offense$PassBlock)
  Offensive$RunBlock <- zscore(Offensive$RunBlock, avg_offense$RunBlock, sd_offense$RunBlock)
  Offensive$Pace <- zscore(Offensive$Pace, avg_offense$Pace, sd_offense$Pace)

  # Turnovers are in reverse
  Offensive$TO <- 100 - Offensive$TO
  Offensive$PassBlock <- 100 - Offensive$PassBlock

  Offensive <- Offensive %>%
    select(Team, Year, Offense, Pass, Rush, TO, DSR,
           RedZone, PassBlock, RunBlock, Pace)

  return(Offensive)
}


### DEFENSIVE RANKINGS
get_fo_def <- function(year = 2019){
  # Get rankings
  d_rankings_url <- glue("https://www.footballoutsiders.com/stats/teamdef/{year}")
  d_rankings <- read_html(d_rankings_url) %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  d_rankings <- d_rankings[,c(2, 3, 7, 9)]
  colnames(d_rankings)[1] <- "Team"

  ## Get Defensive Drive Stats
  ddrive_url <- glue("https://www.footballoutsiders.com/stats/drivestatsdef/{year}")
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
  dline_url <- glue("https://www.footballoutsiders.com/stats/dl/{year}")
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
    left_join(dline, by = "Team") %>%
    mutate(Year = year)

  # change to numeric data
  Defensive[, 2:10] <- sapply(Defensive[, 2:10], cleanPercent)
  Defensive[, 2:10] <- sapply(Defensive[, 2:10], as.numeric)

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

  Defensive <- Defensive %>%
    select(Team, Year, Defense, Pass, Rush, TO, DSR,
           RedZone, PassRush, RunStuff, Pace)

  return(Defensive)
}

