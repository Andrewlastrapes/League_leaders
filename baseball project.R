library("dplyr")
library("ggplot2")

install.packages("knitr")
library("knitr")

# load and store data:
setwd("/Users/andrewlastrapes")
batting <- read.csv("batting.csv")

# create "batting averages" variable: 

batting <- batting %>%
  mutate(round(ba = H/AB), digits = 3)


# create "on base percentage" variable: 

batting <- batting %>%
  mutate(round(obp = (H + BB + HBP)/(AB + BB + HBP + SF), digits = 3))


#create "on base percentage" for players before 1954 (no sacfrices or HBP were recorded until 1954.)

batting1871_1953 <- batting %>%
  filter(yearID %in% 1871:1953)


batting1871_1953 <- batting1871_1953 %>%
  mutate(round(obp = (H + BB)/(AB + BB), digits = 3))

# create "slugging percentage": 

#step 1: create "singles" variable

batting <- batting %>%
  mutate(singles = H - (HR + X2B + X3B))


# step 2: create "total bases" variable 

# funtion for total bases: x = Home runs, y = Triples, z = Doubles, r = Singles 

tb_function <- function(hr, t, d, s) {
  hr = hr * 4
  t = t * 3
  d = d * 2
  s = s * 1
  tb = hr + t + d + s
return(tb)
}


batting <- batting %>%
  mutate(totalbases = tb_function(HR, X3B, X2B, singles))

# slugging percentage 

batting <- batting %>%
  mutate(round(slg = totalbases/AB), digits = 3)


# calculate strike out ratio:

batting_strikeouts <- batting %>%
  filter(SO > 0) %>%
  mutate(round(soratio= AB/SO), digits = 3)

# Calculate league averages for Batting Average, Home Runs, RBI's, Slugging Percentage, On Base Percentage, and Runs. 

# Find league leaders--minimum 100 games 

batting100 <- batting %>%
  filter(G >= 100)

batting100 <- batting100 %>%
  mutate(so_ratio = AB/SO)

 leag_avg <- batting100 %>%
    group_by(yearID) %>%
    summarise(lgba_avg = round(sum(H, na.rm = T)/sum(AB, na.rm = T),digits = 3),
              lghr_avg = round(mean(HR, na.rm = T), digits = 2) ,
              lgrbi_avg = round(mean(RBI, na.rm = T),digits = 2),
              lgslg_avg = round(mean(slg, na.rm = T),digits = 3),
              lgobp_avg = round(mean(obp, na.rm = T),digits = 3),
              lgruns_avg = round(mean(R, na.rm = T),digits = 2),
              soratio = round(mean(so_ratio, na.rm = T), digits = 2))
              
  
# Find league leaders in batting average, home runs, RBI's, slugging percentage, on base percentage, runs, and strike out ratio. 

# Batting Average:

bestba <- batting100 %>%
  select(yearID, ba , playerID) %>%
  group_by(yearID) %>%
  summarise(highest_ba = max(ba))

bestba2 <- merge(bestba, batting100[, c("yearID", "ba", "playerID")], by.x = c("yearID", "highest_ba"), by.y = c("yearID", "ba"))

bestba2 <- bestba2[!duplicated(bestba2$yearID), ]

bestba3 <- bestba2[!duplicated(bestba2$yearID, fromLast = T), ]

w <- 1:20
max(w[1:10])



# Home Run:

bestHR <- batting100 %>%
  group_by(yearID) %>%
  summarise(highest_HR = max(HR))

# Merge datasets to include playerID  

bestHR2 <- merge(bestHR, batting100[, c("yearID", "HR", "playerID")], by.x = c("yearID", "highest_HR"), by.y = c("yearID", "HR"))


bestHR2 <- bestHR2[!duplicated(bestHR2$yearID), ]

bestHR3 <- bestHR2[!duplicated(bestHR2$yearID, fromLast = T), ]


# RBI:


bestRBI <- batting100 %>%
  select(yearID, RBI , playerID) %>%
  group_by(yearID) %>%
  summarise(highest_RBI = max(RBI))

bestRBI2 <- merge(bestRBI, batting100[, c("yearID", "RBI", "playerID")], by.x = c("yearID", "highest_RBI"), by.y = c("yearID", "RBI"))

bestRBI2 <- bestRBI2[!duplicated(bestRBI2$yearID), ]

bestRBI3<- bestRBI2[!duplicated(bestRBI2$yearID, fromLast = T), ]

# On Base Percentage

bestobp <- batting100 %>%
  group_by(yearID) %>%
  summarise(highest_obp = max(obp, na.rm= T))

bestobp2 <- merge(bestobp, batting100[, c("yearID", "obp", "playerID")], by.x = c("yearID", "highest_obp"), by.y = c("yearID", "obp"))

bestobp2 <- bestobp2[!duplicated(bestobp2$yearID), ]

bestobp3<- bestobp2[!duplicated(bestobp2$yearID, fromLast = T), ]


# Runs
            
 bestrun <- batting100 %>%
   group_by(yearID) %>%
   summarise(highest_runs = max(R))

 bestrun2 <- merge(bestrun, batting100[, c("yearID", "R", "playerID")], by.x = c("yearID", "highest_runs"), by.y = c("yearID", "R"))
 
 bestrun2 <- bestrun2[!duplicated(bestrun2$yearID), ]
 
 bestrun3 <- bestrun2[!duplicated(bestrun2$yearID, fromLast = T), ] 

 
 # Slugging Percentage 
 
 bestslg <- batting100 %>%
   select(yearID, slg, playerID) %>%
   group_by(yearID) %>%
   summarise(highest_slg = max(slg))
 
 bestslg2 <- merge(bestslg, batting100[, c("yearID", "slg", "playerID")], by.x = c("yearID", "highest_slg"), by.y = c("yearID", "slg"))
 
 bestslg2 <- bestslg2[!duplicated(bestslg2$yearID), ]
 
 bestslg3 <- bestslg2[!duplicated(bestslg2$yearID, fromLast = T), ]
 
 # Strike Out Ratio
 
 bestsoratio <- batting100 %>%
   select(yearID, so_ratio, playerID) %>%
   group_by(yearID) %>%
   summarise(highest_soratio = max(so_ratio))
 
 
 
 bestso2 <- merge(bestsoratio, batting100[, c("yearID", "so_ratio", "playerID")], by.x = c("yearID", "highest_soratio"), by.y = c("yearID", "so_ratio"))
 
 bestso2 <- bestso2[!duplicated(bestso2$yearID), ]
 
 bestso3 <- bestso2[!duplicated(bestso2$yearID, fromLast = T), ]
 
 
 # Greatest margins between leaders and league average
 
 # Create data frame with league averages and leaders:
 
test <- cbind(leag_avg, bestba2[, 2:3 ], bestHR2[, 2:3 ], bestobp2[, 2:3 ], bestrun2[, 2:3 ], bestRBI2[, 2:3 ], bestslg2[, 2:3 ], bestso2[, 2:3 ])
 

colnames(test)[10] <- "Player_ba"
colnames(test)[12] <- "Player_HR"
colnames(test)[14] <- "Player_obp"
colnames(test)[16] <- "Player_runs"
colnames(test)[18] <- "Player_RBI"
colnames(test)[20] <- "Player_slg"
colnames(test)[22] <- "Player_soratio"



test1 <- test %>%
        mutate(ba_difference = highest_ba-lgba_avg),
            (runs_diff = highest_runs- lgruns_avg),
            (HR_diff = highest_HR - lghr_avg),
            (obp_diff = highest_obp - lgobp_avg),
            (RBI_diff = highest_RBI - lgrbi_avg),
            (slg_diff = highest_slg - lgslg_avg),
            (sor_diff = highest_soratio - soratio)

leag_avg_leag_leaders <- test %>%
  mutate(ba_diff = highest_ba - lgba_avg)%>%
  mutate(runs_diff = highest_runs- lgruns_avg)%>%
  mutate(HR_diff = highest_HR - lghr_avg)%>%
  mutate (obp_diff = highest_obp - lgobp_avg)%>%
  mutate (RBI_diff = highest_RBI - lgrbi_avg)%>%
  mutate (slg_diff = highest_slg - lgslg_avg)%>%
  mutate (sor_diff = highest_soratio - soratio)

# make into a chart 

# Batting Average- 1887 -  Tip O'Neil- .435-  league average- .290       difference- .145
# Highest Runs - 1921-  Babe Ruth -    177-   77                        difference - 100
# HR          - 2001 - Barry Bonds -   73 -    17                         difference - 56
# Slugging P    1920   Babe Ruth -    .849     .402                              .447
# RBI's         1931   Lou Gehrig     184        68                             116
# OBP           2004   Barry Bonds    .609        .346            .263
 
 


Statistic <- c("Batting Average", "Runs", "HR", "Slugging Percentage", "RBI's", "OBP")
Year <- c("1887", "1921", "2001", "1920", "1931", "2004")
Player <- c("Tip Oneil", "Babe Ruth", "Barry Bonds", "Babe Ruth", "Lou Gehrig", "Barry Bonds")
League_High <- c(.435, 177, 73, .849, 184, .609) 
League_Average <- c(.290, 77, 17, .402, 68, .346) 
Difference <- c(.145, 100, 56, .447, 116, .263)


final <- data.frame(Statistic, Year, Player, League_High, League_Average, Difference)


rownames(final$Statistic[2]) <- "Runs" 


ggplot(final, aes(x = Statistic, y = League_High)) + geom_line()


?ggplot

# How could I summarise the difference between league leaders and league average with a function or for loop?
 
 leag_difference <- function(x, league_ave){
   difference <- x - league_ave[x]
   return(difference)
 }
 
leag_difference()

 





