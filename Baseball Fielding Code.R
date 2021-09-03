#David Schmerfeld
#The Effectiveness of Using Out Probabilities to Measure Fielding in Baseball
#R Code

##########################################
###Part 1 of 4 - Player ID Map Creation###
##########################################

#Read large player map
PlayerMap <- read.csv("http://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv")

#Filter out players from 2015 to present
PlayerMap2015 <- PlayerMap[!is.na(PlayerMap$mlb_played_last) & PlayerMap$mlb_played_last >= 2015, ]

#Write PlayerMap2015
write.csv(PlayerMap2015,"c:/Users/David/Desktop/PlayerMap.csv",
          row.names=FALSE)

##########################################
###Part 2 of 4 - Statcast Data Scraping###
##########################################

#This code takes several hours to run

#Statcast search on baseballsavant.com limits queries to about one week of games
#The following "Dates" .csv files contain start dates and end dates to include 4 days of games
#For example, the first entry of the 2020Dates.csv file is:
#year,start_date,end_date
#2020,2020-07-23,2020-07-26

#Read Dates Files for each year
Dates20 <- read.csv("2020Dates.csv")
Dates19 <- read.csv("2019Dates.csv")
Dates18 <- read.csv("2018Dates.csv")
Dates17 <- read.csv("2017Dates.csv")

#Create empty data frames for each year
data20 <- data.frame()
data19 <- data.frame()
data18 <- data.frame()
data17 <- data.frame()

#Create 2020 Statcast file
for(i in 1:nrow(Dates20)){
  new <- baseballr::scrape_statcast_savant(start_date=Dates20[i,2], 
                                           end_date=Dates20[i,3],
                                           player_type="batter")
  
  data20 <- rbind(data20, new)
}

#Create 2019 Statcast file
for(i in 1:nrow(Dates19)){
  new <- baseballr::scrape_statcast_savant(start_date=Dates19[i,2], 
                                           end_date=Dates19[i,3],
                                           player_type="batter")
  
  data19 <- rbind(data19, new)
}

#Create 2018 Statcast file
for(i in 1:nrow(Dates18)){
  new <- baseballr::scrape_statcast_savant(start_date=Dates18[i,2], 
                                           end_date=Dates18[i,3],
                                           player_type="batter")
  
  data18 <- rbind(data18, new)
}

#Create 2017 Statcast file
for(i in 1:nrow(Dates17)){
  new <- baseballr::scrape_statcast_savant(start_date=Dates17[i,2], 
                                           end_date=Dates17[i,3],
                                           player_type="batter")
  
  data17 <- rbind(data17, new)
}

#Write CSV files to USB Drive
write.csv(data20, "E:\\StatcastDFs\\Statcast20", row.names=FALSE)
write.csv(data19, "E:\\StatcastDFs\\Statcast19", row.names=FALSE)
write.csv(data18, "E:\\StatcastDFs\\Statcast18", row.names=FALSE)
write.csv(data17, "E:\\StatcastDFs\\Statcast17", row.names=FALSE)

#####################################
###Part 3 of 4 - Data Set Creation###
#####################################

#Column Labels for Retrosheet Game Logs
#http://www.retrosheet.org/gamelogs/glfields.txt
GLCols <- c("Date", "Games", "Day", "AwayTm", "AwayLg", "AwayGm", "HomeTm", 
            "HomeLg", "HomeGm", "AwayScore", "HomeScore", "TotalOuts", "DayNight",
            "CompletedDate", "Forfeit", "Protest", "ParkID", "Attendance",
            "Length", "AwayLineScore", "HomeLineScore", "AwayAB", "AwayH",
            "Away2B", "Away3B", "AwayHR", "AwayRBI", "AwaySAC", "AwaySF", "AwayHBP",
            "AwayBB", "AwayIW", "AwayK", "AwaySB", "AwayCS", "AwayGDP", "AwayCI",
            "AwayLOB", "AwayPitchers", "AwayIndER", "AwayTmER", "AwayWP", 
            "AwayBalk", "AwayPO", "AwayA", "AwayE", "AwayPB", "AwayDP", "AwayTP",
            "HomeAB", "HomeH", "Home2B", "Home3B", "HomeHR", "HomeRBI", "HomeSAC",
            "HomeSF", "HomeHBP", "HomeBB", "HomeIW", "HomeK", "HomeSB", "HomeCS", 
            "HomeGDP", "HomeCI", "HomeLOB", "HomePitchers", "HomeIndER", 
            "HomeTmER", "HomeWP", "HomeBalk", "HomePO", "HomeA", "HomeE", 
            "HomePB", "HomeDP", "HomeTP", "UmpHomeID", "UmpHomeName", "Ump1BID",
            "Ump1BName", "Ump2BID", "Ump2BName", "Ump3BID", "Ump3BName",
            "UmpLFID", "UmpLFName", "UmpRFID", "UmpRFName", "AwayMgrID", 
            "AwayMgrName", "HomeMgrID", "HomeMgrName", "WinPitcherID", 
            "WinPitcherName", "LosePitcherID", "LosePitcherName", "SavePitcherID",
            "SavePitcherName", "WinRBIBatterID", "WinRBIBatterName", 
            "AwaySPID", "AwaySPName", "HomeSPID", "HomeSPName", "AwayStart1ID",
            "AwayStart1Name", "AwayStart1Pos", "AwayStart2ID", "AwayStart2Name", 
            "AwayStart2Pos", "AwayStart3ID", "AwayStart3Name", "AwayStart3Pos",  
            "AwayStart4ID", "AwayStart4Name", "AwayStart4Pos", "AwayStart5ID", 
            "AwayStart5Name", "AwayStart5Pos", "AwayStart6ID", "AwayStart6Name", 
            "AwayStart6Pos", "AwayStart7ID", "AwayStart7Name", "AwayStart7Pos", 
            "AwayStart8ID", "AwayStart8Name", "AwayStart8Pos", "AwayStart9ID",  
            "AwayStart9Name", "AwayStart9Pos", "HomeStart1ID", "HomeStart1Name", 
            "HomeStart1Pos", "HomeStart2ID", "HomeStart2Name", "HomeStart2Pos", 
            "HomeStart3ID", "HomeStart3Name", "HomeStart3Pos", "HomeStart4ID",  
            "HomeStart4Name", "HomeStart4Pos", "HomeStart5ID", "HomeStart5Name", 
            "HomeStart5Pos", "HomeStart6ID", "HomeStart6Name", "HomeStart6Pos",  
            "HomeStart7ID", "HomeStart7Name", "HomeStart7Pos", "HomeStart8ID",  
            "HomeStart8Name", "HomeStart8Pos", "HomeStart9ID", "HomeStart9Name", 
            "HomeStart9Pos", "OtherInfo", "DataComplete")

#Load Game Logs from Retrosheet - http://www.retrosheet.org/gamelogs
#Ballpark codes are at http://www.retrosheet.org/parkcode.txt
GL2020 <- read.csv("GL2020.csv", header=FALSE)
GL2019 <- read.csv("GL2019.csv", header=FALSE)
GL2018 <- read.csv("GL2018.csv", header=FALSE)
GL2017 <- read.csv("GL2017.csv", header=FALSE)

#Add column labels to game logs
colnames(GL2020) <- GLCols
colnames(GL2019) <- GLCols
colnames(GL2018) <- GLCols
colnames(GL2017) <- GLCols

#Combine years 2017-2020 into single data frame
GL <- rbind(GL2017, GL2018, GL2019, GL2020)

#Filter to keep only relevant columns
GL <- GL[,c("Date", "Games", "AwayTm", "HomeTm", "AwayScore", "HomeScore", 
            "ParkID", "AwayHR", "AwayHBP", "AwayBB", "AwayIW", "AwayK", 
            "AwayLOB", "AwayE", "HomeHR",  "HomeHBP", "HomeBB", "HomeIW", "HomeK", 
            "HomeLOB", "HomeE", "HomeSPID")]

#Correct stadium for Texas Rangers 2020 home games
GL$ParkID[GL$Date >= 20200000 & GL$HomeTm=="TEX"] <- "ARL03"

#Create Retrosheet Game IDs
GL$RetroID <- paste(GL$Date, GL$AwayTm, GL$HomeTm, GL$Games,
                    sep="")

#Load standard team abbreviations for mapping b/n datasets
Teams <- read.csv("Teams.csv")

#Merge in standard away team abbreviations to Retrosheet
GL <- merge(GL,
            Teams[,c("AwayAbb", "Retrosheet")],
            by.x="AwayTm", by.y="Retrosheet", all.x=TRUE)

#Merge in standard home team abbreviations to Retrosheet
GL <- merge(GL,
            Teams[,c("HomeAbb", "Retrosheet")],
            by.x="HomeTm", by.y="Retrosheet", all.x=TRUE)

#Create standardized Game ID column
GL$GameID <- paste(GL$Date, GL$AwayAbb, GL$HomeAbb, GL$HomeSPID, sep="")

#Confirm that there are no duplicates
nrow(GL) - length(unique(GL$GameID))

#Confirm that there are no NAs for Game IDs
nrow(GL[is.na(GL$GameID),])

#Load Statcast pitch-by-pitch data
#Data scraped from baseballsavant.com
#See R code file 3 of 4 for code used to scrape Statcast data
Statcast20_raw <- read.csv("Statcast20.csv")
Statcast19_raw <- read.csv("Statcast19.csv")
Statcast18_raw <- read.csv("Statcast18.csv")
Statcast17_raw <- read.csv("Statcast17.csv")

#Combine years 2017-2019 into single data frame
Statcast_raw <- rbind(Statcast17_raw, Statcast18_raw, Statcast19_raw, 
                      Statcast20_raw)

#Add Expected Hit column (BA>0.500)
Statcast_raw$xHit <- ifelse(Statcast_raw$estimated_ba_using_speedangle<.5 | 
                              Statcast_raw$estimated_ba_using_speedangle=="null",0,1)

#Create vector of relevant Statcast columns
Statcast_vars <- c("game_date", "events", "game_type", "home_team",
                   "away_team", "type", "inning_topbot", "game_pk",
                   "pitcher_1", "estimated_ba_using_speedangle",
                   "launch_speed_angle", "at_bat_number",
                   "pitch_number", "barrel", "xHit")

#Filter Statcast data to only include relevant columns
Statcast <- Statcast_raw[,Statcast_vars]

#Remove NAs for game types, at bat numbers, and pitch numbers
Statcast <- Statcast[!is.na(Statcast$game_type) &
                       !is.na(Statcast$at_bat_number) & 
                       !is.na(Statcast$pitch_number),]

#Filter for all first pitches of game
Statcast_Pitch1 <- Statcast[Statcast$game_type=="R" &
                              Statcast$at_bat_number==1 & 
                              Statcast$pitch_number==1,
                            c("game_date", "home_team",
                              "away_team", "game_pk", "pitcher_1")]

#Add one game for which Statcast data is missing first pitch data
Statcast_Pitch1_Add <- Statcast[Statcast$game_type=="R" &
                                  Statcast$game_pk==531713 &
                                  Statcast$at_bat_number==1 & 
                                  Statcast$pitch_number==2,
                                c("game_date", "home_team",
                                  "away_team", "game_pk", "pitcher_1")]

#Combine data frame of first pitches
Statcast_Pitch1 <- rbind(Statcast_Pitch1, Statcast_Pitch1_Add)

#Confirm there are no NAs for Statcast game IDs
nrow(Statcast_Pitch1[is.na(Statcast_Pitch1$game_pk),])

#Load player map for mapping ID numbers b/n different data sources
#people.csv file at http://github.com/chadwickbureau/register/tree/master/data
#Filtered to only include players whose  last game was in or after 2015
#R code file 4 of 4 contains code for obtaining player map
PlayerMap <- read.csv("PlayerMap.csv")

#Merge Retrosheet player IDs into first pitch data
Statcast_Pitch1 <- merge(Statcast_Pitch1,
                         PlayerMap[,c("key_mlbam", "key_retro", "name_last", 
                                      "name_first")],
                         by.x="pitcher_1", by.y="key_mlbam", all.x=TRUE)

#Merge in standard away team abbreviations into first pitch data
Statcast_Pitch1 <- merge(Statcast_Pitch1,
                         Teams[,c("AwayAbb", "Statcast")],
                         by.x="away_team", by.y="Statcast", all.x=TRUE)

#Merge in standard home team abbreviations into first pitch data
Statcast_Pitch1 <- merge(Statcast_Pitch1,
                         Teams[,c("HomeAbb", "Statcast")],
                         by.x="home_team", by.y="Statcast", all.x=TRUE)

#Determine year, months, & days
Statcast_Pitch1$Year <- substr(Statcast_Pitch1$game_date, start=1, stop=4)
Statcast_Pitch1$Month <- substr(Statcast_Pitch1$game_date, start=6, stop=7)
Statcast_Pitch1$Day <- substr(Statcast_Pitch1$game_date, start=9, stop=10)

#Create standardized Game ID
Statcast_Pitch1$GameID <- paste(Statcast_Pitch1$Year, Statcast_Pitch1$Month,
                                Statcast_Pitch1$Day, Statcast_Pitch1$AwayAbb,
                                Statcast_Pitch1$HomeAbb,
                                Statcast_Pitch1$key_retro, sep="")

#Confirm no NAs for Game ID
nrow(Statcast_Pitch1[is.na(Statcast_Pitch1$GameID),])

#Remove duplicates of Statcast Game IDs
Statcast_Pitch1 <- Statcast_Pitch1[!duplicated(Statcast_Pitch1$game_pk),]

#Merge Statcast IDs with Retrosheet IDs
GL <- merge(GL, Statcast_Pitch1[,c("game_pk", "GameID")], all.x=TRUE)

#Confirm there are no NAs for Statcast Game ID
nrow(GL[is.na(GL$game_pk),])

#Confirm that all games have a unique ID
nrow(GL) - length(unique(GL$game_pk))

library(dplyr)

#Determine Home Team Barrels for each game
Statcast_HomeBarrels <- filter(Statcast, inning_topbot == "Bot") %>%
  group_by(game_pk) %>%
  summarize(HomeBarrels = sum(barrel)) %>%
  as.data.frame()

#Determine Away Team Barrels for each game
Statcast_AwayBarrels <- filter(Statcast, inning_topbot == "Top") %>%
  group_by(game_pk) %>%
  summarize(AwayBarrels = sum(barrel)) %>%
  as.data.frame()

#Add Home Team Barrels into Retrosheet Game Logs
GL <- merge(GL, Statcast_HomeBarrels, all.x=TRUE)

#Add Away Team Barrels into Retrosheet Game Logs
GL <- merge(GL, Statcast_AwayBarrels, all.x=TRUE)

#Determine Home Team Expected Hits for each game
Statcast_HomexHits <- filter(Statcast, inning_topbot == "Bot") %>%
  group_by(game_pk) %>%
  summarize(HomexHits = sum(xHit)) %>%
  as.data.frame()

#Determine Away Team Expected Hits for each game
Statcast_AwayxHits <- filter(Statcast, inning_topbot == "Top") %>%
  group_by(game_pk) %>%
  summarize(AwayxHits = sum(xHit)) %>%
  as.data.frame()

#Add Home Team Expected Hits into Retrosheet Game Logs
GL <- merge(GL, Statcast_HomexHits, all.x=TRUE)

#Add Away Team Expected Hits into Retrosheet Game Logs
GL <- merge(GL, Statcast_AwayxHits, all.x=TRUE)

#Filter Statcast data to only include batted ball events & include HRs
Statcast_BBE <- Statcast_raw[Statcast_raw$type=="X", Statcast_vars]

#Total home runs
length(which(Statcast_BBE$events=="home_run")) #20,769

#Total barrels
sum(Statcast_BBE$barrel) #28,897

#Total barrels and home runs
length(which(Statcast_BBE$events=="home_run" & Statcast_BBE$barrel==1)) #16,714

#Home Runs that are Barrels
16714/20769 #80.5%

#Barrels that are Home Runs
16714/28897 #57.8%

#Filter batted ball events to only include barrels
Statcast_barrels <- Statcast_BBE[Statcast_BBE$barrel==1,] 

#Denominator for Batting Average of Barrels
length(which(Statcast_barrels$events!="sac_fly" & 
               Statcast_barrels$events!="sac_fly_double_play")) #28,647

#Numerator for Batting Average of Barrels
length(which(Statcast_barrels$events=="home_run" | 
               Statcast_barrels$events=="triple" |
               Statcast_barrels$events=="double" |
               Statcast_barrels$events=="single")) #23,003

#Batting Average for Barrels
23003/28647 #0.803

#Filter Statcast data to only include balls hit in play & exclude HRs
Statcast_BIP <- Statcast_raw[Statcast_raw$type=="X" & Statcast_raw$events!="home_run",
                             Statcast_vars]

#Change hit probability to numeric
Statcast_BIP$estimated_ba_using_speedangle <- as.numeric(Statcast_BIP$estimated_ba_using_speedangle)

#Remove NA hit probabilities and hit probabilities greater than 1
Statcast_BIP <- Statcast_BIP[!is.na(Statcast_BIP$estimated_ba_using_speedangle) & 
                               Statcast_BIP$estimated_ba_using_speedangle <= 1, ]

#Calculate advanced fielding statistic for each batted ball in play
Statcast_BIP$Fld <- ifelse(Statcast_BIP$events == "triple" |
                             Statcast_BIP$events == "double" |
                             Statcast_BIP$events == "single" |
                             Statcast_BIP$events == "field_error",
                           -1*(1-Statcast_BIP$estimated_ba_using_speedangle),
                           Statcast_BIP$estimated_ba_using_speedangle)

#Determine Home Team advanced fielding statistic for each game
Statcast_HomeFld <- filter(Statcast_BIP, inning_topbot == "Top") %>%
  group_by(game_pk) %>%
  summarize(HomeFld = sum(Fld)) %>%
  as.data.frame()

#Determine Away Team advanced fielding statistic for each game
Statcast_AwayFld <- filter(Statcast_BIP, inning_topbot == "Bot") %>%
  group_by(game_pk) %>%
  summarize(AwayFld = sum(Fld)) %>%
  as.data.frame()

#Add Home Team fielding into Retrosheet Game Logs (& remove games w/out Fld stats)
GL <- merge(GL, Statcast_HomeFld)

#Add Away Team fielding into Retrosheet Game Logs (& remove games w/out Fld stats)
GL <- merge(GL, Statcast_AwayFld)

#Create vector of column labels for final data set
GLCols_Final <- c("SavantGameID", "Date", "Runs", "ParkID", "HR", "HBP", "BB",
                  "IW", "K", "LOB", "E", "Team", "Opponent", "Barrels", "xHits", 
                  "Fld", "HomeAway")

#Filter out away team statistics (e.g., runs allowed by away team)
GL_Away <- GL[,c("game_pk", "Date", "HomeScore", "ParkID", "HomeHR", "HomeHBP", 
                 "HomeBB", "HomeIW", "HomeK", "HomeLOB", "AwayE", "AwayAbb", "HomeAbb", 
                 "HomeBarrels", "HomexHits", "AwayFld")]

#Add Away indicator
GL_Away$HomeAway <- "Away"

#Rename Columns
colnames(GL_Away) <- GLCols_Final

#Filter out home team statistics (e.g., runs allowed by home team)
GL_Home <- GL[,c("game_pk", "Date", "AwayScore", "ParkID", "AwayHR", 
                 "AwayHBP", "AwayBB", "AwayIW", "AwayK", "AwayLOB", "HomeE", 
                 "HomeAbb", "AwayAbb", "AwayBarrels", "AwayxHits", "HomeFld")]

#Add Home indicator
GL_Home$HomeAway <- "Home"

#Rename Columns
colnames(GL_Home) <- GLCols_Final

#Combine statistics for home and away teams
GL_Final <- rbind(GL_Away, GL_Home)

#Check correlations
cor(GL_Final$HR, GL_Final$Barrels)
cor(GL_Final$E, GL_Final$Fld)
cor(GL_Final$Runs, GL_Final$xHits)

#Write data set
write.csv(GL_Final,"c:/Users/David/Desktop/Baseball.csv", row.names=FALSE)

########################################
###Part 4 of 4 - Statistical Modeling###
########################################

#Load data set (as created in R code part 3 of 4)
Baseball <- read.csv("Baseball.csv")

#Combine Hit-by-Pitch, Base-on-Balls, and Intentional Walks into same statistic
Baseball$Walks <- Baseball$HBP + Baseball$BB + Baseball$IW

#Mean, variance, min & max Fld
mean(Baseball$Fld) #0.16
min(Baseball$Fld) #-9.2
max(Baseball$Fld) #6.8
sd(Baseball$Fld) #1.9

#Histogram of Runs
par(mar=c(5,4.5,4,1)+0.1)
hist(Baseball$Runs, breaks=30, xlim=c(0,30), xlab=expression(bold(Runs)),
     ylab=expression(bold(Frequency)), main="", cex.lab=1.5, cex.axis=1.5,
     right=FALSE)

#Mean, variance, & max Runs
mean(Baseball$Runs) #4.64
var(Baseball$Runs) #10.6
max(Baseball$Runs) #29

#Correlation table
cor(Baseball[,c(3,5,9,11,14,16,18)])

#Load glmmTMB package for GLMMs
library(glmmTMB)

#Negative Binomial model (Fielding Errors)
Baseball_glmmNB_E <- glmmTMB(Runs ~ Walks + K + E + Barrels + (1 | ParkID),
                             ziformula=~0, family=nbinom2(link="log"),
                             data=Baseball)

summary(Baseball_glmmNB_E)

#Negative Binomial model (Advanced Fielding Statistic)
Baseball_glmmNB_Fld <- glmmTMB(Runs ~ Walks + K + Fld + Barrels + 
                                 (1 | ParkID), ziformula=~0, 
                               family=nbinom2(link="log"), data=Baseball)

summary(Baseball_glmmNB_Fld)

#BLUPs dataframe
BLUPs_df <- data.frame(ranef(Baseball_glmmNB_Fld)[[1]])
park_BLUPs <- data.frame(park = rownames(BLUPs_df),
                         exp_BLUP = exp(BLUPs_df))
rownames(park_BLUPs) <- NULL
colnames(park_BLUPs) <- c("park", "exp_BLUP")

#Read park IDs file to merge with team & stadium names into BLUPs dataframe
#CSV obtained from http://www.retrosheet.org/parkcode.txt
#Manually added team names and relabeled columns
ParkIDs <- read.csv("ParkIDs.csv")

park_BLUPs <- merge(ParkIDs[,c("ParkID", "Team", "Stadium")], park_BLUPs, 
                    by.x="ParkID", by.y="park", all.y=TRUE)

park_BLUPs$ParkID <- NULL

library(dplyr)
park_BLUPs %>% arrange(desc(park_BLUPs$exp_BLUP))

#Diagnostic plots

#Normality of residuals
qqnorm(residuals(Baseball_glmmNB_Fld), main="QQ plot of Residuals")
qqline(residuals(Baseball_glmmNB_Fld))

#Residuals vs. predicted
plot(fitted(Baseball_glmmNB_Fld), residuals(Baseball_glmmNB_Fld),
     xlab="Fitted", ylab="Residuals", main="Residuals vs. Fitted",
     col=Baseball$Runs+1, xlim=c(0,65), ylim=c(-30, 15), pch=16, xaxt="n", yaxt="n",
     abline(h=0))
abline(h=10, col="red", lty=2)
abline(h=-10, col="red", lty=2)
axis(1, at=seq(0,50,5), labels=seq(0,50,5))
axis(2, seq(-30,10,5))
legend(x="topright" ,legend=c(0:25,29), col=c(1:26,30), pch=16, cex=0.788, ncol=3, title="Runs")

#Normality of random effect
qqnorm(ranef(Baseball_glmmNB_Fld)[[1]]$ParkID[,1], main="Stadium Effects")
qqline(ranef(Baseball_glmmNB_Fld)[[1]]$ParkID[,1])
