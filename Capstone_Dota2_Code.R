library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(xlsx)
library(readr)
library(corrplot)
library(broom)
library(Cairo)
library(cairoDevice)
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(party)
library(caret)
library(e1071)
library(arules)
library(arulesViz)
library(datasets)

# Input all required csv files 
match_csv <- read_csv("match.csv")
chat_csv <- read_csv("chat.csv")
cluster_regions_csv <- read_csv("cluster_regions.csv")
Detailed_Heroes <- read_csv("Detailed_Heroes.csv")
purchase_log_csv <- read_csv("purchase_log.csv")
players_csv <- read_csv("players.csv")
objectives_csv <- read_csv("objectives.csv")
player_ratings_csv <- read_csv("player_ratings.csv")
item_class_1 <- read_csv("item_class_1.csv")
item_ids_csv <- read_csv("item_ids.csv")

## Data Wrangling

# Run exploratory analysis for match_csv to identify columns with missing values, unneccessary columns and outliers
hist(match_csv$game_mode)
hist(match_csv$negative_votes)
hist(match_csv$positive_votes)

# Negative_votes and positive_votes have only 1 observation. Game_mode has 2 observations, but still not significant for our analysis. So, these columns can be removed
match_csv$game_mode <- NULL
match_csv$negative_votes <- NULL
match_csv$positive_votes <- NULL

# Mean of first_blood_time column in match_csv returns 93. So, is is inferred that unit of first_blood_time is seconds since 93 minutes is an impractical estimate of the average time to first blood. 
mean(match_csv$first_blood_time)

# 93 seconds for average first blood seems short. It usually takes a player atleast 1.5 minutes to setup their heroes and move towards center of map.
sum(match_csv$first_blood_time < 100) # 29437 observations in first_blood_time were below 100 seconds. This indicates some error in the way first blood is calculated.

# In match_csv, cluster region is given as number. Instead of using numbers, join cluster_regions with match_csv
match_csv <- left_join(match_csv,cluster_regions_csv,by = "cluster")

# In chat_csv, some time values are negative. Check how many are negative
chat_negative <- (chat_csv$time < 0) # 77905 values are negative

# Convert all negative time values in chat table to 0 since most of the negative values are low single digit values, implying that they are observations measured at game start
chat_csv$time[chat_negative] <- 0

# Do something similar for the time column of purchase_log_csv table
purchase_log_negative <- (purchase_log_csv$time<0)
purchase_log_csv$time[purchase_log_negative] <- 0

# In players_csv, convert all 'None' observations in stuns column to 0
none_players_stuns <- which(players_csv$stuns == "None")
players_csv$stuns[none_players_stuns] <- 0 

# Write function to compute sum of NA`s in different columns of a dataset (Only choose columns that we believe have many NA`s)
function_NA <- function(x){sum(is.na(x))}

Check_NA <- players_csv %>% summarise_at(c("xp_other","xp_roshan","gold_killing_roshan","gold_buyback","gold_abandon","gold_killing_couriers","unit_order_none","unit_order_cast_toggle","unit_order_cast_toggle_auto","unit_order_drop_item","unit_order_give_item","unit_order_disassemble_item", "unit_order_cast_toggle", "unit_order_stop", "unit_order_taunt", "unit_order_buyback", "unit_order_glyph", "unit_order_eject_item_from_stash", "unit_order_cast_rune", "unit_order_move_to_direction", "unit_order_patrol", "unit_order_vector_target_position", "unit_order_radar","unit_order_stop", "unit_order_set_item_combine_lock", "unit_order_continue"),function_NA) 

# High NA columns are all columns displayed in above line other than xp_other

High_NA_Columns <- c("xp_roshan","gold_killing_roshan","gold_buyback","gold_abandon","gold_killing_couriers","unit_order_none","unit_order_cast_toggle","unit_order_cast_toggle_auto","unit_order_drop_item","unit_order_give_item","unit_order_disassemble_item", "unit_order_cast_toggle", "unit_order_stop", "unit_order_taunt", "unit_order_buyback", "unit_order_glyph", "unit_order_eject_item_from_stash", "unit_order_cast_rune", "unit_order_move_to_direction", "unit_order_patrol", "unit_order_vector_target_position", "unit_order_radar","unit_order_stop", "unit_order_set_item_combine_lock", "unit_order_continue")

# Nullify all high NA columns so that the dataset is easier to work with

players_csv <- players_csv %>% select(-one_of(High_NA_Columns))

# Nullify Key column in objectives_csv due to high percentage of NA`s

objectives_csv <- objectives_csv %>% select(-key)

# Convert all negative account id`s in player_ratings_csv to 0

negative_account_id <- which(player_ratings_csv$account_id < 0)

player_ratings_csv$account_id[negative_account_id] <- 0

## Feature Engineering

# Download detailed heroes csv file from Louis Github, a former Springboard student who has developed a file with names of heroes, their types and other attributes. https://github.com/DMzMin/LMS_Dota2/blob/master/hero_names_mod.csv
# From Detailed_Heroes, subset 2 columns: hero_id and Class. Call this Heroes_Type 
# STR stands for strength, AGI for agility and INT for intelligence hero.

Heroes_Type <- Detailed_Heroes[ ,c(1,13)]

# Use left join to include the type of hero in players_csv table 

players_csv <- left_join(players_csv,Heroes_Type,by="hero_id")

# Reduce players_csv and players_ratings_csv file to only those rows that have non-zero account id`s
# Call them players_csv1 and players_ratings_csv1

players_csv1 <- players_csv %>% filter(account_id !=0)

player_ratings_csv1 <- player_ratings_csv %>% filter(account_id !=0)

# In players_csv1, 0 to 4 stands for Radiant team and 128-132 stands for Dire team

players_csv1 <- players_csv1 %>% mutate(player_slot = case_when(player_slot == 0|player_slot == 1|player_slot == 2|player_slot == 3|player_slot == 4 ~ "Radiant", player_slot == 128|player_slot == 129|player_slot == 130|player_slot == 131|player_slot == 132 ~ "Dire"))

# In players_csv2 item_0, item_1, item_2, item_3, item_4 and item_5 to be converted to item classifications. 
# Items to be classified as Strength (Str), Agility (Agi), Intelligence (Int), Mobility (Mob) etc. 
# Some items are combinations of above classifications. Items that combine 2 of above classes are 2 Class, that combine 3 of above classes are 3 Class etc.
# To perform this conversion, a new file called item_class_1 is created using item_ids_csv as base. 
# In item_class_1, a new column is created next to the item_name column. This column is item_class.
# The item_class observations are populated using information from the internet.

# Below code was reused to convert item_0, item_1, item_2, item_3, item_4 and item_5 columns from item_id to item_class
# This was done by first creating a new dataframe called players_csv2, identical to players_csv1. 
# Next, since the item_id column is only available in item_class_1 dataframe and not in players_csv2 dataframe, we rename item_0 to item_id, since they are both the same. (item_0 to item_5 refer to id`s inventory items in different boxes inside a hero`s inventory dashboard. In essence, they are the same as item_id)
# Now left_join is used to combine item_class and players_csv2 using the item_id column, that is now common in both dataframes
# After the join is completed, the item_class column is renamed to item_0
# item_id, item_name are nullified since they are not required
# This similar procedure is performed for item_0 to item_5 columns in players_csv2

players_csv2 <- players_csv1

colnames(players_csv2)[colnames(players_csv2)=="item_5"] <- "item_id"
players_csv2 <- left_join(players_csv2,item_class_1,by="item_id") 
colnames(players_csv2)[colnames(players_csv2)=="item_class"] <- "item_5"
players_csv2$item_id <- NULL
players_csv2$item_name <- NULL

#function_item_id_to_item_class <- function(a){
#colnames(players_csv2)[colnames(players_csv2)==as.character(a)] <- "item_id"
#players_csv2 <- left_join(players_csv2,item_class_1,by="item_id") 
#colnames(players_csv2)[colnames(players_csv2)=="item_class"] <- as.character(a)
#players_csv2$item_id <- NULL
#players_csv2$item_name <- NULL  
#}

#function_item_id_to_item_class(item_0)
# Also tried removing as.character(a) and inserting function argument as "item_0" but did not work

# Export players_csv2 for markdown
write.csv(players_csv2,file="players_csv2.csv")

## Exploratory Data Analysis and Data Visualization using ggplot2

# Calculate total stats by hero type

players_csv$stuns <- as.numeric(players_csv$stuns)

Stats_by_hero_type <- players_csv %>% group_by(Class) %>% summarise(Total_Damage = sum(hero_damage,na.rm=TRUE),Total_Healing = sum(hero_healing),Total_Kills = sum(kills),Total_Deaths = sum(deaths),Total_Assists = sum(assists),Total_Tower_Damage = sum(tower_damage),Total_Gold_Spent = sum(gold_spent,na.rm=TRUE),Last_Hits = sum(last_hits),Total_Stuns=sum(stuns),Mean_XP_Per_Min=mean(xp_per_min),Mean_Gold_Per_Min=mean(gold_per_min),Runes_Picked=sum(unit_order_pickup_rune,na.rm=TRUE))
  
Stats_by_hero_type$Total_Gold_Spent <- NULL

# Scale all observations so that sum of columns equals 1. This helps normalize values
# Class column is nullified because sapply function to make sum of column values to 1 will not be able to ignore character values.
# sapply applies observation/total observation function on all column values 
# Fourth row in Scaled_Stats_by_hero_type is NA. So, that row is removed
# Now Class column is added back as row name

Scaled_Stats_by_hero_type <- Stats_by_hero_type

Scaled_Stats_by_hero_type$Class <- NULL

Scaled_Stats_by_hero_type <- sapply(Scaled_Stats_by_hero_type,function(x) (x/sum(as.numeric(x))))

Scaled_Stats_by_hero_type <- Scaled_Stats_by_hero_type[-4,]

Class <- c("AGI","INT","STR")

row.names(Scaled_Stats_by_hero_type) <- Class

# Visualize scaled stats using heatmap so that reader gets complete picture of hero stats from single graph

Scaled_Stats_by_hero_type_m <- melt(Scaled_Stats_by_hero_type)
  
  # Export Scaled_State_by_hero_type_m for markdown
write.csv(Scaled_Stats_by_hero_type_m,file = "Scaled_Stats_by_hero_type_m.csv")

ggplot(Scaled_Stats_by_hero_type_m, aes(x=Var1,y=Var2,fill=value)) + geom_tile() + labs(x="Type of Hero",y="Stats") + scale_fill_gradient(low="white",high="red")

# Better to only look at players who have played a minimum of 5 games. So, player_ratings_csv2 only includes those players 

player_ratings_csv2 <- player_ratings_csv1 %>% filter(total_matches>4)

# Insert new column to find percentage of wins

player_ratings_csv2 <- player_ratings_csv2 %>% mutate(percentage_wins=total_wins/total_matches)

# Export player_ratings_csv2 for markdown

write.csv(player_ratings_csv2,file="player_ratings_csv2.csv")

# Histogram line plot of percentage_wins statistic by player account_id. Most players win half of the games they play.

ggplot(player_ratings_csv2,aes(percentage_wins)) +geom_histogram(aes(y=..density..)) + geom_density(aes(y=..density..))

# Scatter plot of trueskill_mu and percentage_wins in players_ratings_csv2. trueskill_sigma is assigned to color attribute. Both trueskill attributes are calculated by Dota fans and not by Valve corporation. While trueskill_mu is indicator of player`s ability, with higher value implying better player, trueskill_sigma is the uncertainty in the trueskill_mu measure. As expected, skill and win percentage are correlated. 

ggplot(player_ratings_csv2,aes(x=trueskill_mu,y=percentage_wins,col=trueskill_sigma)) +geom_point() + geom_jitter(shape=1)

# Scatter plot of radiant_win and game duration shows that radiant or dire winning is unaffected by game duration (radiant and dire are the names of 2 dota teams)

ggplot(match_csv,aes(x=radiant_win,y=duration)) +geom_point() + geom_jitter(shape=1)

# Boxplot of level values for each type of item in players_csv2. Level refers to the player/hero`s level during the end of the game. 

function_boxplot_level_itemclass <- function(a){ggplot(players_csv2,aes(x=a,y=level)) + geom_boxplot() + theme(axis.text.x=element_text(angle=-90)) + labs(x="Item types bought by Hero",y="Hero level at game end")}

function_boxplot_level_itemclass(players_csv2$item_0)
function_boxplot_level_itemclass(players_csv2$item_1)
function_boxplot_level_itemclass(players_csv2$item_2)
function_boxplot_level_itemclass(players_csv2$item_3)
function_boxplot_level_itemclass(players_csv2$item_4)
function_boxplot_level_itemclass(players_csv2$item_5)

# Barplot of item_0 to item_5 versus hero type

#function_item_and_hero <- function(data=players_csv2,c){
#item_and_hero <- data %>% count(Class,c) %>% group_by(Class) 
#item_and_hero <- item_and_hero %>% group_by_(Class) %>% mutate(percentage=n/sum(n)*100)
#item_and_hero_1 <- item_and_hero %>% filter(percentage > 5)
#ggplot(item_and_hero_1,aes(x=Class,y=percentage)) + geom_bar(stat='identity',position='dodge')
#}

#function_item_and_hero(data=players_csv2,"item_0")

  # Below code counts combination of Class and item_0, item_1 to item_5. 
  # Further, a new column called percentage_item_0 to percentage_item_5 is created which calculates percentage of class,specific item combination as total of class observations.
  # percentage_item_0 to percentage_item_5 columns are filtered such that only observations higher than 5% are included

item_and_hero_0 <- players_csv2 %>% count(Class,item_0) %>% group_by(Class) %>% mutate(percentage_item_0=n/sum(n)*100) %>% filter(percentage_item_0 > 5)
item_and_hero_0$n <- NULL

item_and_hero_1 <- players_csv2 %>% count(Class,item_1) %>% group_by(Class) %>% mutate(percentage_item_1=n/sum(n)*100) %>% filter(percentage_item_1 > 5)
item_and_hero_1$n <- NULL

item_and_hero_2 <- players_csv2 %>% count(Class,item_2) %>% group_by(Class) %>% mutate(percentage_item_2=n/sum(n)*100) %>% filter(percentage_item_2 > 5)
item_and_hero_2$n <- NULL

item_and_hero_3 <- players_csv2 %>% count(Class,item_3) %>% group_by(Class) %>% mutate(percentage_item_3=n/sum(n)*100) %>% filter(percentage_item_3 > 5)
item_and_hero_3$n <- NULL

item_and_hero_4 <- players_csv2 %>% count(Class,item_4) %>% group_by(Class) %>% mutate(percentage_item_4=n/sum(n)*100) %>% filter(percentage_item_4 > 5)
item_and_hero_4$n <- NULL

item_and_hero_5 <- players_csv2 %>% count(Class,item_5) %>% group_by(Class) %>% mutate(percentage_item_5=n/sum(n)*100) %>% filter(percentage_item_5 > 5)
item_and_hero_5$n <- NULL

#function_item_and_hero <- function(data,b){data[[b]] <- data[[item_class]] }

#function_item_and_hero(data=item_and_hero_0,b="item_0")

  # Change item_0 to item_5 in item_and_hero_0 to item_and_hero_5 to item_class
  # This is done to enable left_join of all columns in different dataframes by item_class
colnames(item_and_hero_0)[colnames(item_and_hero_0)=="item_0"] <- "item_class"
colnames(item_and_hero_1)[colnames(item_and_hero_1)=="item_1"] <- "item_class"
colnames(item_and_hero_2)[colnames(item_and_hero_2)=="item_2"] <- "item_class"
colnames(item_and_hero_3)[colnames(item_and_hero_3)=="item_3"] <- "item_class"
colnames(item_and_hero_4)[colnames(item_and_hero_4)=="item_4"] <- "item_class"
colnames(item_and_hero_5)[colnames(item_and_hero_5)=="item_5"] <- "item_class"

  # Data from above 6 dataframes are combined to single dataframe using nested left_join
item_and_hero <- left_join(item_and_hero_0,item_and_hero_1,by=c("Class","item_class")) %>% left_join(.,item_and_hero_2,by=c("Class","item_class")) %>% left_join(.,item_and_hero_3,by=c("Class","item_class"))%>% left_join(.,item_and_hero_4,by=c("Class","item_class")) %>% left_join(.,item_and_hero_5,by=c("Class","item_class"))

  # Class column consists of STR, AGI and INT. item_class consists of 2 Class, 3 Class, Heal, Misc and Mob.
  # By combining the Class and item_class columns, we get 17 factors. These 17 factors are used to plot the required barplot
  # Row 17 is NA and is removed
  # Data converted to long form using melt to enable plotting
item_and_hero <- item_and_hero %>% unite(new,Class,item_class)
item_and_hero <- item_and_hero[-18,]
item_and_hero_m <- melt(item_and_hero)

# Export item_and_hero_m for markdown file

write.csv(item_and_hero_m,file="item_and_hero_m.csv")

ggplot(item_and_hero_m,aes(x=new,y=value,colour=variable,group=1)) + geom_point() + geom_line(aes(group=variable)) + theme(axis.text.x=element_text(angle=-90)) + labs(x="Item class of the respective hero type",y="Percentage of observations") 

# Identify heroes that win most games
# New dataframe called players_csv3 is created where player_slot values are converted from numbers to radiant or dire

players_csv3 <- players_csv %>% mutate(player_slot = case_when(player_slot == 0|player_slot == 1|player_slot == 2|player_slot == 3|player_slot == 4 ~ "Radiant", player_slot == 128|player_slot == 129|player_slot == 130|player_slot == 131|player_slot == 132 ~ "Dire"))

  # match_csv contains logical column called radiant_win. TRUE implies that team radiant has won the game.
  # radiant_win column is needed to understand the heroes that win most games
players_csv3 <- left_join(players_csv3,match_csv,by="match_id")
players_csv3$radiant_win <- as.logical(players_csv3$radiant_win)
players_csv3 <- players_csv3 %>% select(player_slot,radiant_win,Class,match_id) %>% mutate(winning_heroes_radiant=case_when(radiant_win==TRUE & player_slot=="Radiant" & Class == "STR" ~ "STR",radiant_win==TRUE & player_slot=="Radiant" & Class == "AGI" ~ "AGI",radiant_win==TRUE & player_slot=="Radiant" & Class == "INT" ~ "INT"))
players_csv3 <- players_csv3 %>% mutate(winning_heroes_dire=case_when(radiant_win==FALSE & player_slot=="Dire" & Class == "STR" ~ "STR",radiant_win==FALSE & player_slot=="Dire" & Class == "AGI" ~ "AGI",radiant_win==FALSE & player_slot=="Dire" & Class == "INT" ~ "INT"))

players_csv3_winning_heroes_radiant <- players_csv3 %>% select(winning_heroes_radiant)
players_csv3_winning_heroes_dire <- players_csv3 %>% select(winning_heroes_dire)

ggplot(na.omit(players_csv3_winning_heroes_radiant),aes(x=winning_heroes_radiant)) + geom_histogram(stat="count")
ggplot(na.omit(players_csv3_winning_heroes_dire),aes(x=winning_heroes_dire)) + geom_histogram(stat="count")

# Export players_csv3_winning_heroes_radiant and players_csv3_winning_heroes_dire for markdown
write.csv(players_csv3_winning_heroes_radiant,file = "players_csv3_winning_heroes_radiant.csv")
write.csv(players_csv3_winning_heroes_dire,file = "players_csv3_winning_heroes_dire.csv")

# Identify heroes that have the highest probability of winning

Prob_Int_Winning <- sum((players_csv3$winning_heroes_radiant == "INT"),na.rm=TRUE)/sum(players_csv3$Class == "INT",na.rm=TRUE)
Prob_AGI_Winning <- sum((players_csv3$winning_heroes_radiant == "AGI"),na.rm=TRUE)/sum(players_csv3$Class == "AGI",na.rm=TRUE)
Prob_STR_Winning <- sum((players_csv3$winning_heroes_radiant == "STR"),na.rm=TRUE)/sum(players_csv3$Class == "STR",na.rm=TRUE)

Prob_Hero_Winning <- c(Prob_Int_Winning,Prob_AGI_Winning,Prob_STR_Winning)

barplot(Prob_Hero_Winning,main="Probability of winning for each hero type",names.arg = c("INT","AGI","STR"))

# Export Prob_Hero_Winning for markdown
write.csv(Prob_Hero_Winning,file="Prob_Hero_Winning.csv")

# Team hero combinations and winnability

## For sake of simplicity find out number of wins by teams that have >= 3 STR or >= 3 AGI or >= 3 INT heroes. 
## Find out which of these 3 combinations has highest winnability
## This is done by first performing mutate on players_csv3 using 3 new columns called STR_1, AGI_1 and INT_1
## For finding total matches where winning team had >= 3 STR heroes, a value of 1 is assigned to the STR_1 column. Similarly for AGI and INT heroes.
## This action is performed separately for winning_heroes_radiant and winning_heroes_dire separately
## Further, STR_1, AGI_1 and INT_1 are summarised and respective sums are calculated for radiant and dire separately
## After finding sum, filter function is used to exclude sums that are less than 3
## A new variable called sum_n_STR_1_winningradiant calculates total rowns of players_csv4 dataset. This is same as total number of games won by radiant team when STR >= 3
## Further, sum of hero type for dire and radiant are added together.

players_csv4 <- players_csv3 %>% mutate(STR_1 = case_when(winning_heroes_radiant=="STR" ~ 1)) %>% group_by(match_id,winning_heroes_radiant) %>% summarise(n_STR_1_winningradiant=sum(STR_1)) %>% filter(n_STR_1_winningradiant>=3)
sum_n_STR_1_winningradiant <- nrow(players_csv4)

players_csv5 <- players_csv3 %>% mutate(AGI_1 = case_when(winning_heroes_radiant=="AGI" ~ 1)) %>% group_by(match_id,winning_heroes_radiant) %>% summarise(n_AGI_1_winningradiant=sum(AGI_1)) %>% filter(n_AGI_1_winningradiant>=3)
sum_n_AGI_1_winningradiant <- nrow(players_csv5)

players_csv6 <- players_csv3 %>% mutate(INT_1 = case_when(winning_heroes_radiant=="INT" ~ 1)) %>% group_by(match_id,winning_heroes_radiant) %>% summarise(n_INT_1_winningradiant=sum(INT_1)) %>% filter(n_INT_1_winningradiant>=3)
sum_n_INT_1_winningradiant <- nrow(players_csv6)

players_csv7 <- players_csv3 %>% mutate(STR_1 = case_when(winning_heroes_dire=="STR" ~ 1)) %>% group_by(match_id,winning_heroes_dire) %>% summarise(n_STR_1_winningdire=sum(STR_1)) %>% filter(n_STR_1_winningdire>=3)
sum_n_STR_1_winningdire <- nrow(players_csv7)

players_csv8 <- players_csv3 %>% mutate(AGI_1 = case_when(winning_heroes_dire=="AGI" ~ 1)) %>% group_by(match_id,winning_heroes_dire) %>% summarise(n_AGI_1_winningdire=sum(AGI_1)) %>% filter(n_AGI_1_winningdire>=3)
sum_n_AGI_1_winningdire <- nrow(players_csv8)

players_csv9 <- players_csv3 %>% mutate(INT_1 = case_when(winning_heroes_dire=="INT" ~ 1)) %>% group_by(match_id,winning_heroes_dire) %>% summarise(n_INT_1_winningdire=sum(INT_1)) %>% filter(n_INT_1_winningdire>=3)
sum_n_INT_1_winningdire <- nrow(players_csv9)

sum_n_STR_winning <- sum_n_STR_1_winningradiant + sum_n_STR_1_winningdire
sum_n_AGI_winning <- sum_n_AGI_1_winningradiant + sum_n_AGI_1_winningdire
sum_n_INT_winning <- sum_n_INT_1_winningradiant + sum_n_INT_1_winningdire

Team_Hero_Winning <- c(sum_n_STR_winning,sum_n_AGI_winning,sum_n_INT_winning)

# Export Team_Hero_Winning for markdown
write.csv(Team_Hero_Winning,file="Team_Hero_Winning.csv")

barplot(Team_Hero_Winning,main="Total team wins where team had >=3 heros of same type",names.arg = c("STR","AGI","INT"))

# Market basket analysis or association rules

    # Setup dataframe for performing association rules
association_data <- players_csv2[,-c(1:44)]

    # Gather dataframe so that data is long
    # This is only for plotting item frequency and not for rule association
    # Value=B is used to keep column header short since x-axis labels
    # on frequency plot include column header
association_data <- gather(association_data,key="A",value = "B",item_0:item_5)
association_data$A <- NULL

    # association_data1 created only to generate frequency plot
    # We ensure that association_data1 does not have Class column
association_data1 <- association_data
association_data1$Class <- NULL

    # Convert characters to factors
association_data1 <- sapply(association_data1,as.factor)

    # Convert to itemMatrix after removing Class column 
    # since we do not want Class column in frequency plot
association_data1 <- as.data.frame(association_data1)
association_data1 <- as(association_data1,"transactions")

    # Plot item frequency
itemFrequencyPlot(association_data1,topN=15,type="absolute")

    # New itemMatrix which includes Class column since for 
    # rules, we want to include Class
association_data2 <- players_csv2[,-c(1:44)]
association_data2 <- as.data.frame(sapply(association_data2,as.factor))
association_data2 <- as(association_data2,"transactions")

  # Generate rules
rules <- apriori(association_data2,parameter = list(supp=0.001,confidence=0.8))

    # Sort and inspect rules
options(digits=2)
rules <- sort(rules,by="confidence",decreasing=TRUE)
inspect(rules[1:10])

    # Find rules that have Class=STR on right side. But use lower 
    # confidence values since there are no high confidence rules with STR
rules1 <- apriori(association_data2,parameter=list(supp=0.001,confidence=0.4),appearance=list(default="lhs",rhs="Class=STR"),control=list(verbose=F))
rules1 <- sort(rules1,by="confidence",decreasing=TRUE)
inspect(rules1[1:10])

## Machine Learning

# Run linear regression to find relation between trueskill_mu and other variables
  # Combine trueskill_mu variable with players_csv2 since players_csv2 has a lot of useful information about specific player
    
    # In players_csv2, there are many instances where same player has played multiple games. 
    # First remove columns that don`t add value to our analysis
players_csv2_VariablesToBeRemoved <- c("match_id","hero_id","stuns","gold_per_min","xp_per_min","leaver_status","gold_other","gold_death","gold_sell")
players_csv2_LR <- players_csv2 %>% select(-one_of(players_csv2_VariablesToBeRemoved))
players_csv2_LR <- players_csv2_LR[,-(20:35)]
players_csv2_LR <- players_csv2_LR[-(21:26)]

    # Develop two new columns. First column assigns 1 if player played as Radiant. Second column assigns 1 if player played as Dire
players_csv2_LR <- players_csv2_LR %>% mutate(player_slot_Radiant=case_when(player_slot=="Radiant"~1))
players_csv2_LR <- players_csv2_LR %>% mutate(player_slot_Dire=case_when(player_slot=="Dire"~1))    

    # Develop three new columns. First column assigns 1 if player chose STR hero. Second column assigns 1 if player chose AGI hero. Third column assigns 1 if players chose INT hero.
players_csv2_LR <- players_csv2_LR %>% mutate(Class_STR=case_when(Class=="STR"~1))
players_csv2_LR <- players_csv2_LR %>% mutate(Class_AGI=case_when(Class=="AGI"~1))
players_csv2_LR <- players_csv2_LR %>% mutate(Class_INT=case_when(Class=="INT"~1))

    # Remove player_slot and Class columns since they are redundant now
players_csv2_LR$player_slot <- NULL
players_csv2_LR$Class <- NULL

    # Convert all NA`s to 0
players_csv2_LR[is.na(players_csv2_LR)] <- 0

    # Aggregate all values so that any given account_id has only value of each respective column
players_csv2_LR_1 <- players_csv2_LR %>% group_by(account_id) %>% summarise(gold=mean(gold),gold_spent=mean(gold_spent),kills=mean(kills),deaths=mean(deaths),assists=mean(assists),denies=mean(denies),last_hits=mean(last_hits),hero_damage=mean(hero_damage),hero_healing=mean(hero_healing),tower_damage=mean(tower_damage),level=mean(level),xp_hero=mean(xp_hero),xp_creep=mean(xp_creep),xp_other=mean(xp_other),gold_destroying_structure=mean(gold_destroying_structure),gold_killing_heros=mean(gold_killing_heros),gold_killing_creeps=mean(gold_killing_creeps),player_slot_Radiant=sum(player_slot_Radiant),player_slot_Dire=sum(player_slot_Dire),Class_STR=sum(Class_STR),Class_AGI=sum(Class_AGI),Class_INT=sum(Class_INT))

    # Combine players_csv2_LR_1 with player_ratings_csv2 but leave out cases where account_id`s don`t match
Combined_LR_1 <- inner_join(player_ratings_csv2,players_csv2_LR_1,by="account_id")

    # Combined_LR_1 dataframe does not need account_id anymore
Combined_LR_1$account_id <- NULL    

    # Assuming that trueskill_mu is dependant variable, find if independant variables are correlated
Combined_LR_1_cor <- cor(Combined_LR_1[,-(23:27)])

    # Export Combined_LR_1 for markdown
write.csv(Combined_LR_1,"Combined_LR_1.csv")

    # Below code was taken from https://codedump.io/share/BSdmR40dKSWs/1/how-to-change-font-size-of-the-correlation-coefficient-in-corrplot since font size of corrplot without these changes was too big and not clear 
corrplot(Combined_LR_1_cor,method="color",insig="blank",addCoef.col = "grey",order="AOE",cl.cex=0.5,tl.cex = 0.5,addCoefasPercent = TRUE,number.cex = 0.5)

    # From corrplot (where correlation is given as percentage instead of -1 to +1)
    # we find that total_wins and total_matches have high negative correlation with trueskill_sigma.

    # total_wins and total_matches,trueskill_mu and percentage_wins,xp_hero and gold_killing_heroes, 
    # xp_hero and level,gold_killing_heroes and kills, kills and hero_damage, level and gold_spent,
    # level and xp_creep, gold_spent and last hits, gold_spent and gold_killing_creeps, xp_creep and 
    # last hits, xp_creep and gold_killing_creeps, last_hits and gold_killing_creeps have high positive 
    # correlation.

    # So, we remove columns total_wins,total_matches,percentage_wins,xp_hero,gold_killing_heroes,hero_damage,gold_spent,xp_creep and gold_killing_creeps.
Remove_Correlated_Columns <- c("total_wins","total_matches","percentage_wins","xp_hero","gold_killing_heros","hero_damage","gold_spent","xp_creep","gold_killing_creeps")

Combined_LR_2 <- Combined_LR_1 %>% select(-one_of(Remove_Correlated_Columns))

    # Export Combined_LR_2 for markdown
write.csv(Combined_LR_2,"Combined_LR_2.csv")

    # Linear regression where trueskill_mu is dependant variable
LR_model_1 <- lm(trueskill_mu ~ trueskill_sigma + gold + kills + deaths + assists + denies + last_hits + hero_healing + tower_damage + level + xp_other + gold_destroying_structure + player_slot_Radiant + player_slot_Dire + Class_STR + Class_AGI + Class_INT,data=Combined_LR_2)
summary(LR_model_1)
    # Summary of linear regression shows that denies, tower_damage, gold_destroying_structure, 
    # player_slot_Radiant, player_slot_dire, Class_STR, Class_AGI, Class_INT are not significant

    # Another linear regression after removing variables that were not significant
    # Also, level and kills were removed since the model with these 2 variables 
    # had Residuals-Fitted and Scale-Location plots where residuals were not random
    # (high degree of parabolic shape)
LR_model_2 <- lm(trueskill_mu ~ trueskill_sigma + gold + deaths + assists + last_hits + hero_healing  + xp_other,data=Combined_LR_2)
summary(LR_model_2)

confint(LR_model_2)
hist(residuals(LR_model_2))

par(mfrow=c(1,1))
plot(LR_model_2)
    # Export anova as dataframe using broom package
LR_model_2_anova <- anova(LR_model_2)
LR_model_2_anova_Export <- tidy(LR_model_2_anova)
write.csv(LR_model_2_anova_Export,"LR_model_2_anova_Expert.csv")

    # Export linear regression model as dataframe using broom package
LR_model_2_Export <- tidy(LR_model_2)
write.csv(LR_model_2_Export,"LR_model_2_Export.csv")

# Logistic regression to predict probability of winning of radiant or dire teams

    # From player_ratings_csv and players_csv, remove rows that have 0 account_id
player_ratings_csv_LogR <- player_ratings_csv %>% filter(account_id>0)
players_csv_LogR <- players_csv %>% filter(account_id>0)

    # In players_csv_LogR, convert player_slot to 1 (Radiant) and 0 (Dire)
players_csv_LogR <- players_csv_LogR %>% mutate(player_slot = case_when(player_slot == 0|player_slot == 1|player_slot == 2|player_slot == 3|player_slot == 4 ~ 1, player_slot == 128|player_slot == 129|player_slot == 130|player_slot == 131|player_slot == 132 ~ 0))

    # Create new dataframe by combining players_csv_LogR and player_ratings_csv_LogR
Combined_LogR <- inner_join(players_csv_LogR,player_ratings_csv_LogR,by="account_id")

    # In this new dataframe, generate new column called trueskill_var. 
    # We do this since we would eventually like to add variances. 
    # Standard deviation cannot be added 
Combined_LogR$trueskill_var <- Combined_LogR$trueskill_sigma^2

    # Remove trueskill_sigma
Combined_LogR$trueskill_sigma <- NULL

    # Remove correlated variables from Combined_LogR dataframe. Use the same set of variables from linear regression
    # Also remove other columns that don`t add value to analysis
Remove_Correlated_Columns_1 <- c("total_wins","total_matches","xp_hero","gold_killing_heros","hero_damage","gold_spent","xp_creep","gold_killing_creeps")
Combined_LogR <- Combined_LogR %>% select(-one_of(Remove_Correlated_Columns_1))

Combined_LogR <- Combined_LogR[,-c(3,6,7,16:21,23,26,27,29:44)]

    # Develop three new columns. First column assigns 1 if player chose STR hero. Second column assigns 1 if player chose AGI hero. Third column assigns 1 if players chose INT hero.
Combined_LogR <- Combined_LogR %>% mutate(Class_STR=case_when(Class=="STR"~1))
Combined_LogR <- Combined_LogR %>% mutate(Class_AGI=case_when(Class=="AGI"~1))
Combined_LogR <- Combined_LogR %>% mutate(Class_INT=case_when(Class=="INT"~1))

    # Now remove Class and account_id columns
Combined_LogR$Class <- NULL
Combined_LogR$account_id <- NULL
    
    # Covert all NA`s to 0
Combined_LogR[is.na(Combined_LogR)] <- 0

    # Group by match_id and player_slot and summarize by adding observations
Combined_LogR_1 <- Combined_LogR %>% group_by(match_id,player_slot) %>% summarise(gold=sum(gold),kills=sum(kills),deaths=sum(deaths),assists=sum(assists),denies=sum(denies),last_hits=sum(last_hits),stuns=sum(stuns),hero_healing=sum(hero_healing),tower_damage=sum(tower_damage),level=sum(level),xp_other=sum(xp_other),gold_other=sum(gold_other),gold_destroying_structure=sum(gold_destroying_structure),trueskill_mu=sum(trueskill_mu),trueskill_var=sum(trueskill_var),Class_STR=sum(Class_STR),Class_AGI=sum(Class_AGI),Class_INT=sum(Class_INT))

    # Now combine Combined_LogR_1 and match_csv
Combined_LogR_2 <- inner_join(Combined_LogR_1,match_csv,by="match_id")

    # Export Combined_LogR_2 for markdown
write.csv(Combined_LogR_2,"Combined_LogR_2.csv")

    # Find variables that are correlated
Combined_LogR_2_cor <- cor(Combined_LogR_2[,-c(1,2,18:21,28:30)])

        # Below code was taken from https://codedump.io/share/BSdmR40dKSWs/1/how-to-change-font-size-of-the-correlation-coefficient-in-corrplot 
        # since font size of corrplot without these changes was too big and not clear 

corrplot(Combined_LogR_2_cor,method="color",insig="blank",addCoef.col = "grey",order="AOE",cl.cex=0.5,tl.cex = 0.5,addCoefasPercent = TRUE,number.cex = 0.5)

    # Positively correlated pairs are (tower_status_radiant,barracks_status_radiant),
    # (level,trueskill_mu),(tower_damage,gold_destroying_structure),(level,kills),
    # (assists,kills),(last_hits,level) and (tower_status_dire,barracks_status_dire)

    # Negatively correlated pairs are (barracks_status_dire,barracks_status_radiant),
    # (barracks_status_radiant,tower_status_dire), (tower_status_radiant,barracks_status_dire)
    # and (tower_status_radiant,tower_status_dire)

    # Following columns are removed due to correlation: tower_status_radiant,barracks_status_radiant,
    # tower_status_dire, barracks_status_dire, level, assists, gold_destroying_structure

    # Remove correlated columns and some other columns 
Remove_Correlated_Columns_2 <- c("tower_status_radiant","barracks_status_radiant","tower_status_dire","barracks_status_dire", "level", "assists", "gold_destroying_structure")

Combined_LogR_3 <- Combined_LogR_2 %>% select(-one_of(Remove_Correlated_Columns_2))
Combined_LogR_3$start_time <- NULL
  
    # Change radiant_win == TRUE to 1 and FALSE to 0
Combined_LogR_3 <- Combined_LogR_3 %>% mutate(radiant_win = case_when(radiant_win == "True" ~ 1, radiant_win == "False" ~ 0))

    # Export Combined_LogR_3 for markdown
write.csv(Combined_LogR_3,"Combined_LogR_3.csv")

    # Logistic Regression
set.seed(100)

        # Split data
LogR_split <- sample.split(Combined_LogR_3$radiant_win,SplitRatio = 0.7)

        # Assign split data into training and test sets
LogR_Train <- subset(Combined_LogR_3,LogR_split == TRUE)
LogR_Test <- subset(Combined_LogR_3,LogR_split == FALSE)

        # Export LogR_Train for markdown
write.csv(LogR_Train,"LogR_Train.csv")

        # Perform Logistic regression
LogR_model <- glm(radiant_win ~ gold + kills + deaths + denies + last_hits + stuns + hero_healing + tower_damage + xp_other + gold_other + trueskill_mu + trueskill_var + Class_STR + Class_AGI + Class_INT + duration + first_blood_time + cluster,LogR_Train,family=binomial)

        # Perform Logistic regression after removing statistically insignificant variables
LogR_model_1 <- glm(radiant_win ~ gold + kills + deaths + last_hits + hero_healing + tower_damage + duration,LogR_Train,family=binomial)

        # Export logistic regression model as dataframe using broom model
LogR_model_1_Export <- tidy(LogR_model_1)
write.csv(LR_model_2_Export,"LogR_model_1_Export.csv")

        # Predict function to give us probability output of logistic regression model on training dataset
pred_LogR_model_Train <- predict(object=LogR_model_1,type="response")
summary(pred_LogR_model_Train)

        # tapply takes pred_LogR_model_Train probabilities and the corresponding values
        # of LogR_Train$radiant_win (which has 0 or 1) and finds the final mean of
        # probabilities for 0 and 1 respectively
tapply(pred_LogR_model_Train,LogR_Train$radiant_win,mean)

        # Generate confusion matrix 
table(LogR_Train$radiant_win,pred_LogR_model_Train>=0.5)

        # Generate ROC curve
LogR_ROCR_Predict <- prediction(pred_LogR_model_Train,LogR_Train$radiant_win)
LogR_ROCR_Perf <- performance(LogR_ROCR_Predict,"tpr","fpr")
plot(LogR_ROCR_Perf,colorize=TRUE)

# Decision tree to predict probability of winning of radiant or dire teams
set.seed(200)
     # Use same dataset used in logistic regression 
Combined_Tree <- Combined_LogR_3
Combined_Tree$match_id <- NULL
Combined_Tree$radiant_win <- as.factor(Combined_Tree$radiant_win)

     # Split dataset
Combined_Tree_split <- sample.split(Combined_Tree$radiant_win,SplitRatio = 0.7)

     # Assign split data into training and test datasets
Combined_Tree_Train <- subset(Combined_Tree,Combined_Tree_split == TRUE)
Combined_Tree_Test <- subset(Combined_Tree,Combined_Tree_split == FALSE)

     # Export train and test datasets for markdown
write.csv(Combined_Tree_Train,"Combined_Tree_Train.csv")
write.csv(Combined_Tree_Train,"Combined_Tree_Test.csv")

     # Build model
Combined_Tree_Model <- rpart(radiant_win ~  .,Combined_Tree_Train,method="class",control=rpart.control(minbucket = 25))

     # View tree
prp(Combined_Tree_Model)

# Cross validate tree
    # First find optimal cp value
fitControl <- trainControl(method="cv",number=10)
cartGrid <- expand.grid(.cp=(1:50)*0.01)
train(radiant_win ~ .,data=Combined_Tree_Train,method="rpart",trControl=fitControl,tuneGrid=cartGrid) 

    # Use this cp in new tree
Combined_Tree_Model_CP <- rpart(radiant_win ~ .,Combined_Tree_Train,method="class",control=rpart.control(cp=0.01))
Combined_Tree_Model_CP_Predict <- predict(object=Combined_Tree_Model_CP,newdata=Combined_Tree_Test,type="class")
table(Combined_Tree_Test$radiant_win,Combined_Tree_Model_CP_Predict)

