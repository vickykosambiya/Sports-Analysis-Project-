library(tidyverse)
library(skimr)
library(dplyr)
library(janitor)
library(ggplot2)

#importing required data sets

matches <- read_csv("matches.csv")
View(matches)

players <- read_csv("deliveries.csv")
View(players)

#finding successful teams 

str(matches)
head(matches)
colnames(matches)
summary(matches$winner)


#Removing Duplicates and changing names to short forms

matches[matches=="Delhi Daredevils"] <- "Delhi Capitals"
matches[matches=="Deccan Chargers"] <- "Sunrisers Hyderabad"
matches[matches=="Rising Pune Supergiant"] <- "Rising Pune Supergiants"
matches[matches=="Chennai Super Kings"] <- "CSK"
matches[matches=="Delhi Capitals"] <- "DC"
matches[matches=="Sunrisers Hyderabad"] <- "SRH"
matches[matches=="Rising Pune Supergiants"] <- "RPS"
matches[matches=="Gujarat Lions"] <- "GL"
matches[matches=="Kings XI Punjab"] <- "KXIP"
matches[matches=="Kochi Tuskers Kerala"] <- "KTK"
matches[matches=="Kolkata Knight Riders"] <- "KKR"
matches[matches=="Mumbai Indians"] <- "MI"
matches[matches=="Pune Warriors"] <- "PW"
matches[matches=="Rajasthan Royals"] <- "RR"
matches[matches=="Royal Challengers Bangalore"] <- "RCB"



#Plotting graph to analyze which team has highest wins


ggplot(matches, aes(x = factor(winner), fill = winner)) +
  geom_bar() + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  labs(x = "Team Names", y = "Wins", title = "No. of Wins By Each Team")



#What teams prefer after winning toss

ggplot(matches, aes(x = toss_decision, fill = toss_decision)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(y = "Number of wins and loss", title = "Teams Preferance after winning Toss")


#Making Mumbai Indians team's dataset

Mumbai <- filter(matches, team1 == 'MI' | team2 == 'MI')
View(Mumbai)

Mumbai1 <- Mumbai %>% 
  mutate(Results = if_else(winner == 'MI', 'Won', 'Lost'),
         Toss_Won = if_else(toss_winner == 'MI', 'Won', 'Lost'),
         Opponent = if_else(team1 == 'MI', team2, team1)) %>% 
  select(-c(16,17,18))

View(Mumbai1)
colnames(Mumbai1)

#Analyzing Result of match with regards to toss.

ggplot(Mumbai1, aes(x = Results, fill = Toss_Won)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(y = "Number of wins and loss", title = "Results depending on Toss")


#We can see from the given graph that Mumbai Indian have won approximately same matches when they have won or lost toss and there is also not much difference in losses with regards to toss. Hence we can say that toss is not much important factor for success of Mumbai Indians 


#Analyzing success depending in Which city match is been played


ggplot(Mumbai1, aes(x = city, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "City", y = "Number of wins and loss", title = "Results depending City in Which Match is been played") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

#Mumbai Indians have played most of their matches in Mumbai and have also won most matches there i.e 53 matches they have also lost most matches at Mumbai i.e 29 which means they have approx. 64% win while they play in Mumbai. They have their highest win percent in Bangalore 8/10 matches won i.e 80%. Their second highest win % is in Kolkata 10/13 i.e 77% approx. 

#Analyzing success of Mumbai Indian in front of different teams


ggplot(Mumbai1, aes(x = Opponent, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "City", y = "Number of wins and loss", title = "Results depending on  Opponents")

#Mumbai Indian have dominated every team except Rising Pune Supergiants they have highest win percent against 19/25 i.e 76%  against Kolkata Knight Riders. Second highest win percentage is against Royal Challengers Bangalore 64%(16/25).


#Analyzing Success depending on toss decision.



ggplot(Mumbai1, aes(x = toss_decision, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Decision", y = "Number of wins and loss", title = "Results depending on  Toss decision")

#Mumbai Indian have won more matches while fielding than batting in first  innings. Win percent while batting first is 60% approx and that while fielding first is 58%. Hence there is slightly more chances of winning while batting first but as there is not much difference so we cannot conclude which option must be opted by team. 






#Making Chennai Super Kings team's dataset

Chennai <- filter(matches, team1 == 'CSK' | team2 == 'CSK')
View(Chennai)

Chennai1 <- Chennai %>% 
  mutate(Results = if_else(winner == 'CSK', 'Won', 'Lost'),
         Toss_Won = if_else(toss_winner == 'CSK', 'Won', 'Lost'),
         Opponent = if_else(team1 == 'CSK', team2, team1)) %>% 
  select(-c(16,17,18))

View(Chennai1)
colnames(Chennai1)

#Analyzing Result of match with regards to toss.

ggplot(Chennai1, aes(x = Results, fill = Toss_Won)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(y = "Number of wins and loss", title = "Results depending on Toss")


#We can see from the given graph that Chennai Super Kings has no significant role of toss when they lose its 50-50. They have won more games when they have won the toss.



#Analyzing success depending in Which city match is been played


ggplot(Chennai1, aes(x = city, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "City", y = "Number of wins and loss", title = "Results depending City in Which Match is been played") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

#They have highest win percent in Chennai. They have dominated almost every ground they have played in except Kolkata. There are others too but they don't have significant numbers.


#Analyzing success of Chennai Super Kings in front of different teams


ggplot(Chennai1, aes(x = Opponent, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "City", y = "Number of wins and loss", title = "Results depending on  Opponents")

#Chennai Super Kings have good win ratio against every team except Mumbai Indians both have faced each other 28 times out of which Chennai Super Kings have won 11 times and lost 17 times. They have highest win ratio against Delhi Capitals i.e 71% approx. 


#Analyzing Success depending on toss decision.



ggplot(Chennai1, aes(x = toss_decision, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Decision", y = "Number of wins and loss", title = "Results depending on  Toss decision")

#Chennai Super Kings have good win percentage while batting first i.e. 65.41% while when fielding first it drops down to 58.53%.








#Making Royal Challengers Bangalore team's dataset

Bangalore <- filter(matches, team1 == 'RCB' | team2 == 'RCB')
View(Bangalore)

RCB <- Bangalore %>% 
  mutate(Results = if_else(winner == 'RCB', 'Won', 'Lost'),
         Toss_Won = if_else(toss_winner == 'RCB', 'Won', 'Lost'),
         Opponent = if_else(team1 == 'RCB', team2, team1)) %>% 
  select(-c(16,17,18))

View(RCB)
colnames(RCB)

#Analyzing Result of match with regards to toss.

ggplot(RCB, aes(x = Results, fill = Toss_Won)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(y = "Number of wins and loss", title = "Results depending on Toss")


#The chance of winning while winning toss is 48.84% while when they lose toss chances of winning game also drops to 41%.



#Analyzing success depending in Which city match is been played


ggplot(RCB, aes(x = city, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "City", y = "Number of wins and loss", title = "Results depending in Which city Match is been played") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

#They have their highest win ratio in Delhi which is 6/9. also their most wins and most loss comes from Bangalore.They have poor performances in Chennai and Hyderabad.


#Analyzing success of Royal Challengers Bangalore in front of different teams


ggplot(RCB, aes(x = Opponent, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Teams", y = "Number of wins and loss", title = "Results depending on  Opponents")

#They have dominated Pune Warriors and Delhi Capitals They need to improve their stats against CSK, MI and SRH.


#Analyzing Success depending on toss decision.



ggplot(RCB, aes(x = toss_decision, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Decision", y = "Number of wins and loss", title = "Results depending on  Toss decision")

#They have won matches while fielding first and also lost matches while fielding first.






#Making Delhi Capitals team's dataset

Delhi <- filter(matches, team1 == 'DC' | team2 == 'DC')
View(Delhi)

DC <- Delhi %>% 
  mutate(Results = if_else(winner == 'DC', 'Won', 'Lost'),
         Toss_Won = if_else(toss_winner == 'DC', 'Won', 'Lost'),
         Opponent = if_else(team1 == 'DC', team2, team1)) %>% 
  select(-c(16,17,18))

View(DC)
colnames(DC)

#Analyzing Result of match with regards to toss.

ggplot(DC, aes(x = Results, fill = Toss_Won)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(y = "Number of wins and loss", title = "Results depending on Toss")


#Although the number of wins is more while losing toss, but win ratio increases when they have won toss.



#Analyzing success depending in Which city match is been played


ggplot(DC, aes(x = city, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "City", y = "Number of wins and loss", title = "Results depending in Which City Match is been played") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

#They have good win ratio in Bangalore, Chandigarh, Chennai, Mohali, Jaipur, Kolkalta and Mumbai. Their most wins have come from Delhi but win percentage drops down significantly.


#Analyzing success of Delhi Capitals in front of different teams


ggplot(DC, aes(x = Opponent, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Teams", y = "Number of wins and loss", title = "Results depending on  Opponents")

#They have 50% win percent against Mumbai Indians and Sunrisers Hyderabad. DC should consider improving their stats against CSK , RCB and KXIP.


#Analyzing Success depending on toss decision.



ggplot(DC, aes(x = toss_decision, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Decision", y = "Number of wins and loss", title = "Results depending on  Toss decision")

#They have won more number of matches while fielding first and also lost matches while fielding first.









#Making Kolkata Knigh Riders team's dataset

Kolkata <- filter(matches, team1 == 'KKR' | team2 == 'KKR')
View(Kolkata)

KKR <- Kolkata %>% 
  mutate(Results = if_else(winner == 'KKR', 'Won', 'Lost'),
         Toss_Won = if_else(toss_winner == 'KKR', 'Won', 'Lost'),
         Opponent = if_else(team1 == 'KKR', team2, team1)) %>% 
  select(-c(16,17,18))

View(KKR)
colnames(KKR)

#Analyzing Result of match with regards to toss.

ggplot(KKR, aes(x = Results, fill = Toss_Won)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(y = "Number of wins and loss", title = "Results depending on Toss")


#Win percent while winn toss is better than win ratio while losing toss.



#Analyzing success depending in Which city match is been played


ggplot(KKR, aes(x = city, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "City", y = "Number of wins and loss", title = "Results depending in Which City Match is been played") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

#They have most number of wins in Kolkata, while have better win in Pune and Hyderabad.


#Analyzing success of Kolkata Knight Riders in front of different teams


ggplot(KKR, aes(x = Opponent, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Teams", y = "Number of wins and loss", title = "Results depending on  Opponents")

#KKR have dominated KXIP, SRH, RCB and DC. They should consider improving against CSK and MI


#Analyzing Success depending on toss decision.



ggplot(KKR, aes(x = toss_decision, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Decision", y = "Number of wins and loss", title = "Results depending on  Toss decision")

#Win percent is better while batting first but most wins have come while fielding first.





#Making Sunrisers Hyderabad team's dataset

Hyderabad <- filter(matches, team1 == 'SRH' | team2 == 'SRH')
View(Hyderabad)

SRH <- Hyderabad %>% 
  mutate(Results = if_else(winner == 'SRH', 'Won', 'Lost'),
         Toss_Won = if_else(toss_winner == 'SRH', 'Won', 'Lost'),
         Opponent = if_else(team1 == 'SRH', team2, team1)) %>% 
  select(-c(16,17,18))

View(SRH)
colnames(SRH)

#Analyzing Result of match with regards to toss.

ggplot(SRH, aes(x = Results, fill = Toss_Won)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(y = "Number of wins and loss", title = "Results depending on Toss")


#Win ratio is not much affected due to toss. They have won more matches while losing toss as compared to winning toss, but the numbers don't have much significant differences.



#Analyzing success depending in Which city match is been played


ggplot(SRH, aes(x = city, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "City", y = "Number of wins and loss", title = "Results depending in Which City Match is been played") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

#SRH have won most matches in Hyderabad. They have not performed well in Mumbai, Bangalore, Kolkata and Chennai.


#Analyzing success of Sunrisers Hyderabad in front of different teams


ggplot(SRH, aes(x = Opponent, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Teams", y = "Number of wins and loss", title = "Results depending on  Opponents")


#They have 50% win percent against Mumbai Indians and Sunrisers Hyderabad. DC should consider improving their stats against CSK , RCB and KXIP.


#Analyzing Success depending on toss decision.



ggplot(SRH, aes(x = toss_decision, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Decision", y = "Number of wins and loss", title = "Results depending on  Toss decision")

#They have won more number of matches while fielding first and also lost matches while fielding first. Win ratio is better while fielding first.







#Making Kings XI Punjab Team's dataset

Punjab <- filter(matches, team1 == 'KXIP' | team2 == 'KXIP')
View(Punjab)

KXIP <- Punjab %>% 
  mutate(Results = if_else(winner == 'KXIP', 'Won', 'Lost'),
         Toss_Won = if_else(toss_winner == 'KXIP', 'Won', 'Lost'),
         Opponent = if_else(team1 == 'KXIP', team2, team1)) %>% 
  select(-c(16,17,18))

View(KXIP)
colnames(KXIP)

#Analyzing Result of match with regards to toss.

ggplot(KXIP, aes(x = Results, fill = Toss_Won)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(y = "Number of wins and loss", title = "Results depending on Toss")


#They have a better win percentage while losing toss and also most wins have came when they lost toss.


#Analyzing success depending in Which city match is been played


ggplot(KXIP, aes(x = city, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "City", y = "Number of wins and loss", title = "Results depending in Which City Match is been played") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

#KXIP should improve their performance while playing in Mumbai, Jaipur, Chennai and Kolkata. They have good win percent in Dharamshala and Mohali.


#Analyzing success of Kings XI Punjab in front of different teams


ggplot(KXIP, aes(x = Opponent, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Teams", y = "Number of wins and loss", title = "Results depending on  Opponents")

#Except DC they have not much succeeded against any other teams.

#Analyzing Success depending on toss decision.



ggplot(KXIP, aes(x = toss_decision, fill = Results)) +
  geom_bar(position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", position = position_dodge(width = 0.8)) +
  labs(x = "Decision", y = "Number of wins and loss", title = "Results depending on  Toss decision")

#Results are better when they field first.

#Others teams data are not much significant as number of matches are low.




#Analyzing Players

View(players)
str(players)

#Player contributing most for his team


motm <- matches %>% 
  count(player_of_match)

motm <- motm %>% 
  arrange(desc(n)) %>% 
  slice(1:15)


View(motm)


ggplot(motm) +
  geom_bar(aes(y = reorder(player_of_match, n) , x = n), stat = "identity", fill = c(4)) +
  labs(x = "Players", y = "No.of MOM", title = "Top 15 Players With Most MOM in IPL")




