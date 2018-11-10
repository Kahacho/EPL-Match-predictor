# EPL-Match-predictor
English Premier League Match predictor function using R

EPL_Predictor<-function(Home_Team,Away_Team){

##reading league data from online data bases at football-data.co.uk
data_1<-read.csv(url("http://www.football-data.co.uk/mmz4281/1718/E0.csv"))
data_2<-read.csv(url("http://www.football-data.co.uk/mmz4281/1819/E0.csv"))

##recording variables for relegated teams in the league
Westbrom<-c(which(data_1$HomeTeam=="West Brom"),which(data_1$AwayTeam=="West Brom"))
Stoke<-c(which(data_1$HomeTeam=="Stoke"),which(data_1$AwayTeam=="Stoke"))
Swansea<-c(which(data_1$HomeTeam=="Swansea"),which(data_1$AwayTeam=="Swansea"))

##recording variables for promoted teams in the league
Cardiff<-c(which(data_2$HomeTeam=="Cardiff"),which(data_2$AwayTeam=="Cardiff"))
Fulham<-c(which(data_2$HomeTeam=="Fulham"),which(data_2$AwayTeam=="Fulham"))
Wolves<-c(which(data_2$HomeTeam=="Wolves"),which(data_2$AwayTeam=="Wolves"))

##removing relegated and promoted teams from the league data to be used in the function
EPL_2017_2018<-data_1[c(-Westbrom,-Stoke,-Swansea),c(3:6)]
EPL_2018_2019<-data_2[c(-Cardiff,-Fulham,-Wolves),c(3:6)]

##creating the league database to use for the function
EPL_data<-rbind(EPL_2017_2018,EPL_2018_2019)

##League Goals Calculations
HomeGoals<-EPL_data$FTHG
Average_Home_Goals<-sum(HomeGoals)/length(HomeGoals)
AwayGoals<-EPL_data$FTAG
Average_Away_Goals<-sum(AwayGoals)/length(AwayGoals)
TotalGoals<-sum(HomeGoals,AwayGoals)
AverageGoals<-TotalGoals/nrow(EPL_data)
EPL_Home_Teams<-EPL_data$HomeTeam
EPL_Away_Teams<-EPL_data$AwayTeam

##creating specific data for a specific home side
for(i in 1:length(EPL_Home_Teams)){
if(EPL_Home_Teams[i]==Home_Team){
Home_Team_data<-EPL_data[EPL_data$HomeTeam==Home_Team,]
}
}

##manipulation of home team data
Average_Home_Team_Goals_Scored<-mean(Home_Team_data$FTHG)
Average_Home_Team_Goals_Conceded<-mean(Home_Team_data$FTAG)
Home_Team_Attacking_Strength<-Average_Home_Team_Goals_Scored/Average_Home_Goals
Home_Team_Defending_Strength<-Average_Home_Team_Goals_Conceded/Average_Away_Goals

##creating specific data for a specific away side
for(j in 1:length(EPL_Away_Teams)){
if(EPL_Away_Teams[j]==Away_Team){
Away_Team_data<-EPL_data[EPL_data$AwayTeam==Away_Team,]
}
}

##manipulation of away team data
Average_Away_Team_Goals_Scored<-mean(Away_Team_data$FTAG)
Average_Away_Team_Goals_Conceded<-mean(Away_Team_data$FTHG)
Away_Team_Attacking_Strength<-Average_Away_Team_Goals_Scored/Average_Away_Goals
Away_Team_Defending_Strength<-Average_Away_Team_Goals_Conceded/Average_Home_Goals

##Goal Expectancy for home and away team
Home_Team_Goal_Expectancy<-Home_Team_Attacking_Strength*Away_Team_Defending_Strength*Average_Home_Goals
Away_Team_Goal_Expectancy<-Away_Team_Attacking_Strength*Home_Team_Defending_Strength*Average_Away_Goals
Home_Team_Goals<-rpois(1000,Home_Team_Goal_Expectancy)
Away_Team_Goals<-rpois(1000,Away_Team_Goal_Expectancy)
Expected_Home_Team_Goals<-round(mean(Home_Team_Goals))
Expected_Away_Team_Goals<-round(mean(Away_Team_Goals))

##Match Expected Outcomes
Home_wins<-signif(sum(Home_Team_Goals>Away_Team_Goals)*0.1,digits=3)
Home_wins_Trueodds<-100/Home_wins
draws<-signif(sum(Home_Team_Goals==Away_Team_Goals)*0.1,digits=3)
draws_trueodds<-100/draws
Away_wins<-signif(sum(Home_Team_Goals<Away_Team_Goals)*0.1,digits=3)
Away_wins_Trueodds<-100/Away_wins
Over15goals<-signif(sum((Home_Team_Goals+Away_Team_Goals)>1)*0.1,digits=3)
Over15goals_Trueodds<-100/Over15goals
Over25goals<-signif(sum((Home_Team_Goals+Away_Team_Goals)>2)*0.1,digits=3)
Over25goals_Trueodds<-100/Over25goals
gg<-signif(sum(Home_Team_Goals>0 & Away_Team_Goals>0)*0.1,digits=3)
gg_Trueodds<-100/gg
ExpectedMatchGoals<-round(AverageGoals)

##Matrix output of the various outcomes expected in from the function
hda<-(matrix(c(Home_wins,draws,Away_wins,Home_wins_Trueodds,draws_trueodds,Away_wins_Trueodds),byrow=T,ncol=3,nrow=2,
dimnames=list(c("Match Outcome %","True Odds"),c(Home_Team,"Draw",Away_Team))))


goals<-(matrix(c(ExpectedMatchGoals,Over15goals,Over25goals,gg,ExpectedMatchGoals,Over15goals_Trueodds,Over25goals_Trueodds,gg_Trueodds),byrow=T,ncol=4,nrow=2,
dimnames=list(c("Other Outcomes%","True Odds"),c("Match Goals","Ov1.5","Ov2.5","GG"))))


correctscore<-(matrix(c(Expected_Home_Team_Goals,Expected_Away_Team_Goals),byrow=T,ncol=2,nrow=1,
dimnames=list(c("Expected Goals"),c(Home_Team,Away_Team))))

return(list(home_draw_away=hda,match_goals=goals,correct_score=correctscore))

}

##Input of home team and away team to get results from the function
EPL_Predictor("Southampton","Leicester")
