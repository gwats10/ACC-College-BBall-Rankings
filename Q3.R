library(tidyverse)

#Normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Weights for each category
multiplier <- function(x) {
  return (norm_data[, x] * cor[x,])
}

q3.og <- read.csv('ACCGames1819.csv')
View(q3)

#Identify Game Winner
q3 <- q3.og %>%
  mutate(HomeWinner = ifelse(HomeScore > AwayScore, 1, 0), ScoreDiff = abs(HomeScore - AwayScore)) %>%
  mutate(Winner = ifelse(HomeWinner == 1, as.character(HomeTeam),  as.character(AwayTeam)))

teams <- unique(q3$AwayTeam)
mylist = list()
totals = list()
iter = 1

#Identify type of win/loss (home win, away loss etc)
for(i in teams){
  mylist[[i]] <- as.data.frame(q3[q3$HomeTeam == i | q3$AwayTeam == i,])
  df <- mylist[[i]] %>%
    mutate(HomeWin = ifelse(HomeWinner == 1 & Winner == i, 1, 0),
         AwayWin = ifelse(HomeWinner == 0 & Winner == i, 1, 0),
         HomeLoss = ifelse(HomeWinner == 0 & Winner != i, 1, 0),
         AwayLoss = ifelse(HomeWinner == 1 & Winner != i, 1, 0))
  
  #create season statistics averages/totals
  team_totals <- df %>%
    summarise(AwayScore = mean(AwayScore[AwayTeam == i]), AwayAST = mean(AwayAST[AwayTeam == i]), AwayTOV = mean(AwayTOV[AwayTeam == i]), AwaySTL = mean(AwaySTL[AwayTeam == i]),
              AwayBLK = mean(AwayBLK[AwayTeam == i]), AwayRebounds = mean(AwayRebounds[AwayTeam == i]), AwayORB = mean(AwayORB[AwayTeam == i]), AwayDRB = mean(AwayDRB[AwayTeam == i]),
              AwayFGAAvg = mean(AwayFGA[AwayTeam == i]), AwayFGMAvg = mean(AwayFGM[AwayTeam == i]), AwayFGATotal = sum(AwayFGA[AwayTeam == i]), AwayFGMTotal = sum(AwayFGM[AwayTeam == i]),
              Away3FGAAvg = mean(Away3FGA[AwayTeam == i]), Away3FGMAvg = mean(Away3FGM[AwayTeam == i]), Away3FGATotal = sum(Away3FGA[AwayTeam == i]), Away3FGMTotal = sum(Away3FGM[AwayTeam == i]),
              AwayFTAAvg = mean(AwayFTA[AwayTeam == i]), AwayFTMAvg = mean(AwayFTM[AwayTeam == i]), AwayFTATotal = sum(AwayFTA[AwayTeam == i]), AwayFTMTotal = sum(AwayFTM[AwayTeam == i]),
              AwayFouls = mean(AwayFouls[AwayTeam == i]),
              HomeScore = mean(HomeScore[HomeTeam == i]), HomeAST = mean(HomeAST[HomeTeam == i]), HomeTOV = mean(HomeTOV[HomeTeam == i]), HomeSTL = mean(HomeSTL[HomeTeam == i]),
              HomeBLK = mean(HomeBLK[HomeTeam == i]), HomeRebounds = mean(HomeRebounds[HomeTeam == i]), HomeORB = mean(HomeORB[HomeTeam == i]), HomeDRB = mean(HomeDRB[HomeTeam == i]),
              HomeFGAAvg = mean(HomeFGA[HomeTeam == i]), HomeFGMAvg = mean(HomeFGM[HomeTeam == i]), HomeFGATotal = sum(HomeFGA[HomeTeam == i]), HomeFGMTotal = sum(HomeFGM[HomeTeam == i]),
              Home3FGAAvg = mean(Home3FGA[HomeTeam == i]), Home3FGMAvg = mean(Home3FGM[HomeTeam == i]), Home3FGATotal = sum(Home3FGA[HomeTeam == i]), Home3FGMTotal = sum(Home3FGM[HomeTeam == i]),
              HomeFTAAvg = mean(HomeFTA[HomeTeam == i]), HomeFTMAvg = mean(HomeFTM[HomeTeam == i]), HomeFTATotal = sum(HomeFTA[HomeTeam == i]), HomeFTMTotal = sum(HomeFTM[HomeTeam == i]), 
              HomeFouls = mean(HomeFouls[HomeTeam == i]),
              HomeWin = sum(HomeWin), HomeLoss = sum(HomeLoss), AwayWin = sum(AwayWin), AwayLoss = sum(AwayLoss)) %>%
    mutate(Team = i)
  totals[[iter]] <- team_totals
  iter = iter + 1
}
totals <- bind_rows(totals)
totals <- totals %>%
  mutate(TotWins 
         = HomeWin + AwayWin, TotLoss = HomeLoss + AwayLoss, Games = TotWins + TotLoss, TotWinPercent = round(TotWins / Games * 100, digits = 2),
         HomeWinPercent = HomeWin / (HomeWin + HomeLoss) * 100, AwayWinPercent = AwayWin / (AwayWin + AwayLoss) * 100,
         HomeFGPPercent = HomeFGMTotal / HomeFGATotal, Home3FGPercent = Home3FGMTotal / Home3FGATotal, HomeFTPercent = HomeFTMTotal / HomeFTATotal,
         AwayFGPPercent = AwayFGMTotal / AwayFGATotal, Away3FGPercent = Away3FGMTotal / Away3FGATotal, AwayFTPercent = AwayFTMTotal / AwayFTATotal) %>%
  arrange(TotWinPercent)
          
#See stats highly correlated with total win percentage
cor <- as.data.frame(cor(select(totals, -Team, -HomeWin, -HomeLoss, -AwayWin, -AwayLoss, -TotWins, -TotLoss, -Games,         
                                -HomeWinPercent, -AwayWinPercent)))
cor$Stat<- rownames(cor)
cor <- cor %>%
  select(Stat, TotWinPercent) %>%
  mutate(TotWinPercent = abs(TotWinPercent)) %>%
  arrange(desc(TotWinPercent))

#normalize data
norm_data <- as.data.frame(sapply(select(totals, -Team, -HomeWin, -HomeLoss, -AwayWin, -AwayLoss, -TotWins, -TotLoss, -Games,
              -HomeWinPercent, -AwayWinPercent), normalize))

rownames(norm_data)<- totals$Team 


rownames(cor) <- cor$Stat
cor$Stat <- NULL

#create final dataframe with category weights
final <- as.data.frame(sapply(names(norm_data), multiplier))
final$Rank <- rowSums(final)
final$Team <- totals$Team 

#max score to calculate rating
max_score <- sum(cor$TotWinPercent) - 1

#final rankings and ratings
output <- final %>%
  select(Rank, Team) %>%
  mutate(Rating = Rank / max_score) %>%
  arrange(desc(Rating)) %>%
  mutate(Rank = seq(1,15))
  
#write.csv(output, 'ACCRankings1819.csv', row.names = F)  