###introduce the data
#"numot" - this indicates the number of overtime periods in the game, an integer 0 or higher.
#"wloc" - this identifies the "location" of the winning team. If the winning team was the home team, this value will be "H". If the winning team was the visiting team, this value will be "A". If it was played on a neutral court, then this value will be "N". Sometimes it is unclear whether the site should be considered neutral, since it is near one team's home court, or even on their court during a tournament, but for this determination we have simply used the Kenneth Massey data in its current state, where the "@" sign is either listed with the winning team, the losing team, or neither team.
#wfgm - field goals made
#wfga - field goals attempted
#wfgm3 - three pointers made
#wfga3 - three pointers attempted
#wftm - free throws made
#wfta - free throws attempted
#wor - offensive rebounds
#wdr - defensive rebounds
#wast - assists
#wto - turnovers
#wstl - steals
#wblk - blocks
#wpf - personal fouls
##import the data
ncaa_regular = read.csv2("data/regular_season_detailed_results.csv",sep=",",dec =",")
str(ncaa_regular)
ncaa_tourney = read.csv2("data/tourney_compact_results.csv",sep=",",dec =",")
teams = read.csv2("data/teams.csv",sep=",",dec =",")
seeds = read.csv2("data/tourney_seeds.csv",sep=",",dec =",")
slot = read.csv2("data/tourney_slots.csv",sep=",",dec =",")
##count the number of matches in 2003
count = 0
for (i in c(1:length(ncaa_regular$season))){
  while(ncaa_regular$season[i]==2003){count = count+1;break;}
}  
####we pick 2003 year data
ncaa_regular_2003 = ncaa_regular[1:count,]
View(ncaa_regular_2003)
# get the winer and loser subset of the list
colnames(ncaa_regular_2003)
wnamelist <- c(colnames(ncaa_regular_2003)[c(3,4,7:21)])
lnamelist <- c(colnames(ncaa_regular_2003)[c(5:8,22:34)])
wsubset = subset(ncaa_regular_2003,select=wnamelist)
lsubset = subset(ncaa_regular_2003,select=lnamelist)
#change the location of loser subset from wloc to lloc.
for(i in c(1:length(lsubset$wloc))){ 
  if(lsubset$wloc[i]=='A') {wlocnumber = 1}
  if(lsubset$wloc[i]=='H') {wlocnumber = 2}
  if(lsubset$wloc[i]=='N') {wlocnumber = 3}
  lsubset$wloc[i]<-switch(wlocnumber,'H','A','N')
}
##change the name of column in each subset
namelist = c('team','score','loc','numot','fgm','fga','fgm3','fga3','ftm','fta','or','dr','ast','to','stl','blk','pf')
colnames(lsubset) <- namelist
colnames(wsubset) <- namelist
#add result to lsubset and wsubset
lsubset$result <- c(rep(0,each=(length(lsubset$team))))
wsubset$result <- c(rep(1,each=(length(wsubset$team))))
#merge lsubset and wsubset
subset_2003 = rbind(wsubset,lsubset)

##transform the value to decimal after divided by the maximum value in one column 
transform_decimal <- function(x){
for(y in x){
max = 0
for(i in c(1:length(subset_2003[[y]]))){
 max <- ifelse(max>subset_2003[[y]][i], max,subset_2003[[y]][i])
}
 
for(i in c(1:length(subset_2003[[y]]))){
subset_2003[[y]][i] <- subset_2003[[y]][i]/max
}
}
return(subset_2003)
}
##get a new_subset_2003. Then we preprocess this set to a traning set
new_subset_2003 <- transform_decimal(c(5:17))
##delete the 1st, 2nd column in the new_subset_2003
#new_subset_2003[1] = NULL
#new_subset_2003[1] = NULL
###new_subset_2003 only contains the match detailed data and match results, and it 
##does not contain team, score data. This data set is used to build model to predict 
##match results from match detailed data.

#####logistics regression 1
library("MASS")
logr_vm <- glm(result ~ numot+fgm+fga+fgm3+fga3+ftm+fta+or+dr+ast+to+stl+blk+pf, data=new_subset_2003, family=binomial(logit))
summary(logr_vm)

###get the ROC curve compares the rank of prediction and result
prob=predict(logr_vm,type=c("response"))
new_subset_2003$prob=prob
library(pROC)
roc_2003 <- roc(result ~ prob, data = new_subset_2003)
plot(roc_2003)    
plot(logr_vm$fitted)
#####get the accuracy of logr_vm
yhat = (logr_vm$fit>=0.5)
for(i in c(1:9232)){yhat[i]=ifelse(yhat[i]==TRUE,1,0)}
correct = 0
for(i in c(1:9232)){if(yhat[i]==new_subset_2003$result[i]) {correct = correct+1} 
}
correctrate= correct/9232
print(correctrate)


##teamrelation(A,B) function can get the team A average data in 2003, average data 
##of matches that team A beats team B, average data of matches that team B beats 
##team A
teamrelation <- function(x,y){
  windata = 0
  losedata = 0
  averagedata = 0
  wincount = 0
  losecount = 0
  totalcount = 0
  ###this is team A won and team B lost  
  for(i in c(1:4616)) {
    if((grepl(x,new_subset_2003$team[i]))&&(grepl(y,new_subset_2003$team[i+4616]))){
      windata = windata + new_subset_2003[i,4:17]
      wincount = wincount+1
      print("find A win B")
    }
  }
  if(wincount!=0){
    windata = windata/wincount
  }
  else{
    windata = 0
  }
  ###this is team A lost and team B won   
  for(i in c(4617:9232)) {
    if((grepl(x,new_subset_2003$team[i]))&&(grepl(y,new_subset_2003$team[i-4616]))){
      losedata = losedata+new_subset_2003[i,4:17]
      losecount = losecount+1
    }
  }
  if(losecount!=0){
    losedata = losedata/losecount
  }
  else{
    losedata = 0
  }
  for(i in c(1:9232)){
    if(grepl(x,new_subset_2003$team[i])){
      averagedata = averagedata+ new_subset_2003[i,4:17]
      totalcount = totalcount+1
    }
  }
  averagedata = averagedata/totalcount
  return(list(windata,losedata,averagedata))
}

####teamwinpossibility(A,B) function can get the posibility that team A beat team B
teamwinpossibility = function(y,z){
  x = teamrelation(y,z)
count = 0
add = 0
for(i in c(1:3)){
  if(length(x[[i]]==14)){
    count = count+1
    add = add + x[[i]]
  }
}
add = add/count
return(predict(logr_vm,add,type="response"))
}
##test, we used team 1328 and team 1376
teamwinpossibility(1328,1376)
teamwinpossibility(1376,1328)
##result(A,B) function showed which team could win 
result = function(x,y){
  if(teamwinpossibility(x,y)>teamwinpossibility(y,x)){
    cat("team",x,"win")
    return(x)
  }
  else{
    cat("team",y,"win")
    return(y)
  }
}
##test, we used team 1328 and team 1376
result(1328,1376)
###test, 2003 tournament 4 Regions
tourteams = list(c(1328,1376),c(1143,1314),c(1139,1280),c(1257,1122),
                 c(1329,1335),c(1393,1264),c(1120,1363),c(1448,1190),
                 c(1400,1421),c(1345,1261),c(1163,1140),c(1390,1360),
                 c(1268,1423),c(1462,1407),c(1277,1160),c(1196,1358),
                 c(1246,1237),c(1332,1428),c(1458,1451),c(1409,1173),
                 c(1281,1356),c(1266,1221),c(1231,1104),c(1338,1447),
                 c(1112,1436),c(1211,1153),c(1323,1454),c(1228,1443),
                 c(1141,1166),c(1181,1161),c(1113,1272),c(1242,1429))
count = 0
for(i in c(1:length(tourteams))){
  if(result(tourteams[[i]][1],tourteams[[i]][2])==tourteams[[i]][1]){
    count = count+1
  }
}
### count/32 is the accuracy in first round 32 matches
count/32


####################Revised after Bryon suggested I should use comparison data
#I will also use 2003 year data as my example.
#I will compare each feature of two teams in each match by subtracting them and normalizing it
#I get a new set ncaa_regular_compare_2003
ncaa_regular_compare_2003 <- ncaa_regular_2003
for(i in c(1:4616)){
  for(j in c(9:21)){
  ncaa_regular_compare_2003[[j]][i]<-(ncaa_regular_compare_2003[[j]][i]-ncaa_regular_compare_2003[[j+13]][i])/(max(ncaa_regular_compare_2003[[j]],ncaa_regular_compare_2003[[j+13]]))
  ncaa_regular_compare_2003[[j+13]][i] <- -ncaa_regular_compare_2003[[j]][i]
}
}
##divide new_ncaa_regular_compare_2003 into two set and then build a new set new_subset_compare_2003
wnamelist <- c(colnames(ncaa_regular_compare_2003)[c(1:4,7:21)])
lnamelist <- c(colnames(ncaa_regular_compare_2003)[c(1:2,5:8,22:34)])
wsubset = subset(ncaa_regular_compare_2003,select=wnamelist)
lsubset = subset(ncaa_regular_compare_2003,select=lnamelist)
#change the location of loser subset from wloc to lloc.
for(i in c(1:length(lsubset$wloc))){ 
  if(lsubset$wloc[i]=='A') {wlocnumber = 1}
  if(lsubset$wloc[i]=='H') {wlocnumber = 2}
  if(lsubset$wloc[i]=='N') {wlocnumber = 3}
  lsubset$wloc[i]<-switch(wlocnumber,'H','A','N')
}
##change the name of column in each subset
namelist = c('season','daynum','team','score','loc','numot','fgm','fga','fgm3','fga3','ftm','fta','or','dr','ast','to','stl','blk','pf')
colnames(lsubset) <- namelist
colnames(wsubset) <- namelist
#add result to lsubset and wsubset
lsubset$result <- c(rep(0,each=(length(lsubset$team))))
wsubset$result <- c(rep(1,each=(length(wsubset$team))))
#merge lsubset and wsubset
new_ncaa_regular_compare_2003 = rbind(wsubset,lsubset)
#transform character value to number
new_ncaa_regular_compare_2003$loc <- as.integer(new_ncaa_regular_compare_2003$loc)
##logistics regression
logr_vm <- glm(result ~ loc+numot+fgm+fga+fgm3+fga3+ftm+fta+or+dr+ast+to+stl+blk+pf, data=new_ncaa_regular_compare_2003, family=binomial(logit))
summary(logr_vm)
###get the ROC curve compares the rank of prediction and result
prob=predict(logr_vm,type=c("response"))
new_ncaa_regular_compare_2003$prob=prob
library(pROC)
roc_2003 <- roc(result ~ prob, data = new_ncaa_regular_compare_2003)
plot(roc_2003)    
plot(logr_vm$fitted)
#####get the accuracy of logr_vm in training set
yhat = (logr_vm$fit>=0.5)
for(i in c(1:9232)){yhat[i]=ifelse(yhat[i]==TRUE,1,0)}
correct = 0
for(i in c(1:9232)){if(yhat[i]==new_ncaa_regular_compare_2003$result[i]) {correct = correct+1} 
}
correctrate= correct/9232
print(correctrate)

######### evaluate tournament matches

teamrelation <- function(x,y){
  windata = 0
  losedata = 0
  averagedata = 0
  wincount = 0
  losecount = 0
  totalcount = 0
  ###this is team A won and team B lost  
  for(i in c(1:4616)) {
    if((grepl(x,new_ncaa_regular_compare_2003$team[i]))&&(grepl(y,new_ncaa_regular_compare_2003$team[i+4616]))){
      windata = windata + new_ncaa_regular_compare_2003[i,5:19]
      wincount = wincount+1
      print("find A win B")
    }
  }
  if(wincount!=0){
    windata = windata/wincount
  }
  else{
    windata = 0
  }
  ###this is team A lost and team B won   
  for(i in c(4617:9232)) {
    if((grepl(x,new_ncaa_regular_compare_2003$team[i]))&&(grepl(y,new_ncaa_regular_compare_2003$team[i-4616]))){
      losedata = losedata+new_ncaa_regular_compare_2003[i,5:19]
      losecount = losecount+1
    }
  }
  if(losecount!=0){
    losedata = losedata/losecount
  }
  else{
    losedata = 0
  }
  for(i in c(1:9232)){
    if(grepl(x,new_ncaa_regular_compare_2003$team[i])){
      averagedata = averagedata+ new_ncaa_regular_compare_2003[i,5:19]
      totalcount = totalcount+1
    }
  }
  averagedata = averagedata/totalcount
  return(list(windata,losedata,averagedata))
}


####teamwinpossibility(A,B) function can get the posibility that team A beat team B
teamwinpossibility = function(y,z){
  x = teamrelation(y,z)
  count = 0
  add = 0
  for(i in c(1:3)){
    if(length(x[[i]]>=13)){
      count = count+1
      add = add + x[[i]]
    }
  }
  add = add/count   
  add$loc = 3
  return(predict(logr_vm,add,type="response"))
}
##test, we used team 1328 and team 1376
teamwinpossibility(1328,1376)
teamwinpossibility(1376,1328)
##result(A,B) function showed which team could win 
result = function(x,y){
  if(teamwinpossibility(x,y)>teamwinpossibility(y,x)){
    cat("team",x,"win")
    return(x)
  }
  else{
    cat("team",y,"win")
    return(y)
  }
}
##test, we used team 1328 and team 1376
result(1328,1376)
###test, 2003 tournament 4 Regions
tourteams = list(c(1328,1376),c(1143,1314),c(1139,1280),c(1257,1122),
                 c(1329,1335),c(1393,1264),c(1120,1363),c(1448,1190),
                 c(1400,1421),c(1345,1261),c(1163,1140),c(1390,1360),
                 c(1268,1423),c(1462,1407),c(1277,1160),c(1196,1358),
                 c(1246,1237),c(1332,1428),c(1458,1451),c(1409,1173),
                 c(1281,1356),c(1266,1221),c(1231,1104),c(1338,1447),
                 c(1112,1436),c(1211,1153),c(1323,1454),c(1228,1443),
                 c(1141,1166),c(1181,1161),c(1113,1272),c(1242,1429))
count = 0
for(i in c(1:length(tourteams))){
  if(result(tourteams[[i]][1],tourteams[[i]][2])==tourteams[[i]][1]){
    count = count+1
  }
}
### count/32 is the accuracy in first round 32 matches
count/32
count/32





#############
#take 2013-2014 as an example
#############

##count the number of matches in 2013
count = 0
for (i in c(1:length(ncaa_regular$season))){
  while(ncaa_regular$season[i]==2013){count = count+1;break;}
}  
first = 0
for (i in c(1:length(ncaa_regular$season))){
  if(ncaa_regular$season[i]==2013) {first=i;break}
}
####we pick 2013 year data
ncaa_regular_2013 = ncaa_regular[first:(first+count-1),]
View(ncaa_regular_2013)

#I will compare each feature of two teams in each match by subtracting them and normalizing it
#I get a new set ncaa_regular_compare_2013
ncaa_regular_compare_2013 <- ncaa_regular_2013
for(i in c(1:5320)){
  for(j in c(9:21)){
    ncaa_regular_compare_2013[[j]][i]<-(ncaa_regular_compare_2013[[j]][i]-ncaa_regular_compare_2013[[j+13]][i])/(max(ncaa_regular_compare_2013[[j]],ncaa_regular_compare_2013[[j+13]]))
    ncaa_regular_compare_2013[[j+13]][i] <- -ncaa_regular_compare_2013[[j]][i]
  }
}
##divide new_ncaa_regular_compare_2003 into two set and then build a new set new_subset_compare_2003
wnamelist <- c(colnames(ncaa_regular_compare_2013)[c(1:4,7:21)])
lnamelist <- c(colnames(ncaa_regular_compare_2013)[c(1:2,5:8,22:34)])
wsubset_2013 = subset(ncaa_regular_compare_2013,select=wnamelist)
lsubset_2013 = subset(ncaa_regular_compare_2013,select=lnamelist)
#change the location of loser subset from wloc to lloc.
for(i in c(1:length(lsubset_2013$wloc))){ 
  if(lsubset_2013$wloc[i]=='A') {wlocnumber = 1}
  if(lsubset_2013$wloc[i]=='H') {wlocnumber = 2}
  if(lsubset_2013$wloc[i]=='N') {wlocnumber = 3}
  lsubset_2013$wloc[i]<-switch(wlocnumber,'H','A','N')
}
##change the name of column in each subset
namelist = c('season','daynum','team','score','loc','numot','fgm','fga','fgm3','fga3','ftm','fta','or','dr','ast','to','stl','blk','pf')
colnames(lsubset_2013) <- namelist
colnames(wsubset_2013) <- namelist
#add result to lsubset and wsubset
lsubset_2013$result <- c(rep(0,each=(length(lsubset_2013$team))))
wsubset_2013$result <- c(rep(1,each=(length(wsubset_2013$team))))
#merge lsubset and wsubset
new_ncaa_regular_compare_2013 = rbind(wsubset_2013,lsubset_2013)
#transform character value to number
new_ncaa_regular_compare_2013$loc <- as.integer(new_ncaa_regular_compare_2013$loc)
##logistics regression
logr_vm_2013 <- glm(result ~ loc+numot+fgm+fga+fgm3+fga3+ftm+fta+or+dr+ast+to+stl+blk+pf, data=new_ncaa_regular_compare_2013, family=binomial(logit))
summary(logr_vm_2013)
###get the ROC curve compares the rank of prediction and result
prob=predict(logr_vm_2013,type=c("response"))
new_ncaa_regular_compare_2013$prob=prob
library(pROC)
roc_2013 <- roc(result ~ prob, data = new_ncaa_regular_compare_2013)
plot(roc_2013)    
plot(logr_vm_2013$fitted)
#####get the accuracy of logr_vm in training set
yhat = (logr_vm_2013$fit>=0.5)
for(i in c(1:10640)){yhat[i]=ifelse(yhat[i]==TRUE,1,0)}
correct = 0
for(i in c(1:10640)){if(yhat[i]==new_ncaa_regular_compare_2013$result[i]) {correct = correct+1} 
}
correctrate= correct/10640
print(correctrate)

######### evaluate tournament matches

teamrelation <- function(x,y){
  windata = 0
  losedata = 0
  averagedata = 0
  wincount = 0
  losecount = 0
  totalcount = 0
  ###this is team A won and team B lost  
  for(i in c(1:5320)) {
    if((grepl(x,new_ncaa_regular_compare_2013$team[i]))&&(grepl(y,new_ncaa_regular_compare_2013$team[i+5320]))){
      windata = windata + new_ncaa_regular_compare_2013[i,5:19]
      wincount = wincount+1
      print("find A win B")
    }
  }
  if(wincount!=0){
    windata = windata/wincount
  }
  else{
    windata = 0
  }
  ###this is team A lost and team B won   
  for(i in c(5320:10640)) {
    if((grepl(x,new_ncaa_regular_compare_2013$team[i]))&&(grepl(y,new_ncaa_regular_compare_2013$team[i-5320]))){
      losedata = losedata+new_ncaa_regular_compare_2013[i,5:19]
      losecount = losecount+1
    }
  }
  if(losecount!=0){
    losedata = losedata/losecount
  }
  else{
    losedata = 0
  }
  for(i in c(1:10640)){
    if(grepl(x,new_ncaa_regular_compare_2013$team[i])){
      averagedata = averagedata+ new_ncaa_regular_compare_2013[i,5:19]
      totalcount = totalcount+1
    }
  }
  averagedata = averagedata/totalcount
  return(list(windata,losedata,averagedata))
}


####teamwinpossibility(A,B) function can get the posibility that team A beat team B
teamwinpossibility = function(y,z){
  x = teamrelation(y,z)
  count = 0
  add = 0
  for(i in c(1:3)){
    if(length(x[[i]]>=13)){
      count = count+1
      add = add + x[[i]]
    }
  }
  add = add/count   
  add$loc = 3
  return(predict(logr_vm_2013,add,type="response"))
}
##test, we used team 1328 and team 1376
teamwinpossibility(1196,1107)
teamwinpossibility(1107,1196)
##result(A,B) function showed which team could win 
result = function(x,y){
  if(teamwinpossibility(x,y)>teamwinpossibility(y,x)){
    cat("team",x,"win")
    return(x)
  }
  else{
    cat("team",y,"win")
    return(y)
  }
}
##test, we used team 1328 and team 1376
result(1328,1376)
###test, 2003 tournament 4 Regions
tourteams = list(c(1328,1376),c(1143,1314),c(1139,1280),c(1257,1122),
                 c(1329,1335),c(1393,1264),c(1120,1363),c(1448,1190),
                 c(1400,1421),c(1345,1261),c(1163,1140),c(1390,1360),
                 c(1268,1423),c(1462,1407),c(1277,1160),c(1196,1358),
                 c(1246,1237),c(1332,1428),c(1458,1451),c(1409,1173),
                 c(1281,1356),c(1266,1221),c(1231,1104),c(1338,1447),
                 c(1112,1436),c(1211,1153),c(1323,1454),c(1228,1443),
                 c(1141,1166),c(1181,1161),c(1113,1272),c(1242,1429))
count = 0
for(i in c(1:length(tourteams))){
  if(result(tourteams[[i]][1],tourteams[[i]][2])==tourteams[[i]][1]){
    count = count+1
  }
}
### count/32 is the accuracy in first round 32 matches
count/32
count/32

install.packages("lsa")
