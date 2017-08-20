library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(xlsx)
library(readr)

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

ggplot(na.omit(players_csv3[,"winning_heroes_radiant"]),aes(x=winning_heroes_radiant)) + geom_histogram(stat="count")
ggplot(na.omit(players_csv3[,"winning_heroes_dire"]),aes(x=winning_heroes_dire)) + geom_histogram(stat="count")

# Identify heroes that have the highest probability of winning

Prob_Int_Winning <- sum((players_csv3$winning_heroes_radiant == "INT"),na.rm=TRUE)/sum(players_csv3$Class == "INT",na.rm=TRUE)
Prob_AGI_Winning <- sum((players_csv3$winning_heroes_radiant == "AGI"),na.rm=TRUE)/sum(players_csv3$Class == "AGI",na.rm=TRUE)
Prob_STR_Winning <- sum((players_csv3$winning_heroes_radiant == "STR"),na.rm=TRUE)/sum(players_csv3$Class == "STR",na.rm=TRUE)

Prob_Hero_Winning <- c(Prob_Int_Winning,Prob_AGI_Winning,Prob_STR_Winning)

barplot(Prob_Hero_Winning,main="Probability of winning for each hero type",names.arg = c("INT","AGI","STR"))

# Team hero combinations and winnability

## For sake of simplicity find out number of wins by teams that have >= 3 STR or >= 3 AGI or >= 3 INT heroes. 
## Find out which of these 3 combinations has highest winnability

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

barplot(Team_Hero_Winning,main="Total team wins where team had >=3 heros of same type",names.arg = c("STR","AGI","INT"))
