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
#create tables for 2013 & 2014
ncaa_regular_2013 <- subset(ncaa_regular,ncaa_regular$season==2013)
ncaa_regular_2014 <- subset(ncaa_regular,ncaa_regular$season==2014)

#Get the comparative dataset of 2013 and 2014 
#by subtracting them and normalizing it
compare_2013 <- ncaa_regular_2013
for(i in c(1:length(ncaa_regular_2013$season))){
  for(j in c(9:21)){
    compare_2013[[j]][i]<-(ncaa_regular_2013[[j]][i]-ncaa_regular_2013[[j+13]][i])/(max(ncaa_regular_2013[[j]],ncaa_regular_2013[[j+13]]))
    compare_2013[[j+13]][i] <- -(compare_2013[[j]][i])
  }
}

compare_2014 <- ncaa_regular_2014
for(i in c(1:length(ncaa_regular_2014$season))){
  for(j in c(9:21)){
    compare_2014[[j]][i]<-(ncaa_regular_2014[[j]][i]-ncaa_regular_2014[[j+13]][i])/(max(ncaa_regular_2014[[j]],ncaa_regular_2014[[j+13]]))
    compare_2014[[j+13]][i] <- -(compare_2014[[j]][i])
  }
}

## add more features:add a new feature 'wseed' 'lseed' in compare_2013 and compare_2014 and seeddistance
compare_2013$wseed = 0
compare_2013$lseed = 0
for(i in c(1:length(compare_2013$season))){
  for(j in c(1:length(seeds$season))){
    if(compare_2013$season[i]==seeds$season[j]){
      if(compare_2013$wteam[i]==seeds$team[j]){
        compare_2013$wseed[i] = seeds$seedpercent[j]
      }
      if(compare_2013$lteam[i]==seeds$team[j]){
        compare_2013$lseed[i] = seeds$seedpercent[j]
      } 
    }   
  } 
}
compare_2013$seeddistance <- (compare_2013$wseed - compare_2013$lseed)/16

compare_2014$wseed = 0
compare_2014$lseed = 0
for(i in c(1:length(compare_2014$season))){
  for(j in c(1:length(seeds$season))){
    if(compare_2014$season[i]==seeds$season[j]){
      if(compare_2014$wteam[i]==seeds$team[j]){
        compare_2014$wseed[i] = seeds$seedpercent[j]
      }
      if(compare_2014$lteam[i]==seeds$team[j]){
        compare_2014$lseed[i] = seeds$seedpercent[j]
      } 
    }   
  } 
}

compare_2014$seeddistance <- (compare_2014$wseed - compare_2014$lseed)/16

###read in vegas 2013 and 2014 data 
vegas2013 = read.csv2("data/vegasnew2013.csv",sep=",",dec =",")
vegas2014 = read.csv2("data/vegasnew2014.csv",sep=",",dec =",")
##delete all the null rows in vegas2013 and vegas2014
for(times in c(1:5)){ 
  for( i in c(1:length(vegas2013$statistics))){
      if(!is.na(as.numeric(vegas2013$statistics[i]))){
        if(((as.numeric(vegas2013$statistics[i]))==1)){
          if(!is.na(as.numeric(vegas2013$title[i]))){
            if(((as.numeric(vegas2013$title[i]))==1)){
               vegas2013 = vegas2013[-i,]
           }
        }
      }
    }
  }
}
for(times in c(1:5)){
  for( i in c(1:length(vegas2014$statistics))){
    if(!is.na(as.numeric(vegas2014$statistics[i]))){
      if(((as.numeric(vegas2014$statistics[i]))==1)){
        if(!is.na(as.numeric(vegas2014$title[i]))){
          if(((as.numeric(vegas2014$title[i]))==1)){
            vegas2014 = vegas2014[-i,]
          }
        }
      }
    }
  }
}
##process vegas2013 and vegas2014
#process the vagas2013$statistics
library(stringr)
newvegas2013 = vegas2013
newvegas2013$statistics <- as.character(newvegas2013$statistics)
newvegas2013$statistics = str_trim(newvegas2013$statistics,side = "both")
newvegas2013$website <- as.character(newvegas2013$website)
newvegas2013$website = str_trim(newvegas2013$website,side = "both")
newvegas2013$title <- as.character(newvegas2013$title)
newvegas2013$title = str_trim(newvegas2013$title,side = "both")

for( i in c(1:length(newvegas2013$statistics))){
  if(newvegas2013$statistics[[i]]!=""){
    if(!is.na(as.numeric(strsplit(newvegas2013$statistics[[i]],",")[[1]][1]))){
      newvegas2013$statistics[[i]] = as.numeric(strsplit(newvegas2013$statistics[[i]],",")[[1]][1])
    }
    else {
      newvegas2013$statistics[[i]] = 0
    }
  }
  else{
    next
  }
}

newvegas2014 = vegas2014
newvegas2014$statistics <- as.character(newvegas2014$statistics)
newvegas2014$statistics = str_trim(newvegas2014$statistics,side = "both")
newvegas2014$website <- as.character(newvegas2014$website)
newvegas2014$website = str_trim(newvegas2014$website,side = "both")
newvegas2014$title <- as.character(newvegas2014$title)
newvegas2014$title = str_trim(newvegas2014$title,side = "both")

for( i in c(1:length(newvegas2014$statistics))){
  if(newvegas2014$statistics[[i]]!=""){
    if(!is.na(as.numeric(strsplit(newvegas2014$statistics[[i]],",")[[1]][1]))){
      newvegas2014$statistics[[i]] = as.numeric(strsplit(newvegas2014$statistics[[i]],",")[[1]][1])
    }
    else {
      newvegas2014$statistics[[i]] = 0
    }
  }
  else{
    next
  }
}
#process the vagas2013/vagas2014$website and change its name to 'daynum'
for( i in c(1:length(newvegas2013$website))){
  newvegas2013$website[[i]] = str_split(newvegas2013$website[[i]],'/')[[1]][length(str_split(newvegas2013$website[[i]],'/')[[1]])]
}
colnames(newvegas2013)[1] = 'daynum'
for( i in c(1:length(newvegas2013$daynum))){
  if(ISOdate(str_split(newvegas2013$daynum[[i]],"-")[[1]][3],str_split(newvegas2013$daynum[[i]],"-")[[1]][1],str_split(newvegas2013$daynum[[i]],"-")[[1]][2]) > ISOdate(2012,11,5)){
    newvegas2013$daynum[[i]] = as.integer(ISOdate(str_split(newvegas2013$daynum[[i]],"-")[[1]][3],str_split(newvegas2013$daynum[[i]],"-")[[1]][1],str_split(newvegas2013$daynum[[i]],"-")[[1]][2]) - ISOdate(2012,11,5))
  }
  else{
    newvegas2013$daynum[[i]] = as.integer(ISOdate(str_split(newvegas2013$daynum[[i]],"-")[[1]][3],str_split(newvegas2013$daynum[[i]],"-")[[1]][2],str_split(newvegas2013$daynum[[i]],"-")[[1]][1]) - ISOdate(2012,11,5))
  }
}

for( i in c(1:length(newvegas2014$website))){
  newvegas2014$website[[i]] = str_split(newvegas2014$website[[i]],'/')[[1]][length(str_split(newvegas2014$website[[i]],'/')[[1]])]
}
colnames(newvegas2014)[1] = 'daynum'
for( i in c(1:length(newvegas2014$daynum))){
  if(ISOdate(str_split(newvegas2014$daynum[[i]],"-")[[1]][3],str_split(newvegas2014$daynum[[i]],"-")[[1]][1],str_split(newvegas2014$daynum[[i]],"-")[[1]][2]) > ISOdate(2013,11,4)){
    newvegas2014$daynum[[i]] = as.integer(ISOdate(str_split(newvegas2014$daynum[[i]],"-")[[1]][3],str_split(newvegas2014$daynum[[i]],"-")[[1]][1],str_split(newvegas2014$daynum[[i]],"-")[[1]][2]) - ISOdate(2013,11,4))
  }
  else{
    newvegas2014$daynum[[i]] = as.integer(ISOdate(str_split(newvegas2014$daynum[[i]],"-")[[1]][3],str_split(newvegas2014$daynum[[i]],"-")[[1]][2],str_split(newvegas2014$daynum[[i]],"-")[[1]][1]) - ISOdate(2013,11,4))
  }
}
#process the newvegas2013$title and newvegas2014$title
newvegas2013$title = tolower(newvegas2013$title)
titlevegas2013 = newvegas2013
for(i in c(1:length(titlevegas2013$title))){
  if(length(strsplit(titlevegas2013$title[i],",")[[1]])>1){
    if(substr(strsplit(titlevegas2013$title[i],",")[[1]][1],1,1) == substr(titlevegas2013$title[i+1],1,1)){
      titlevegas2013$title[i+1] = strsplit(titlevegas2013$title[i],",")[[1]][1]
    }
    if(substr(strsplit(titlevegas2013$title[i],",")[[1]][2],1,1)==substr(titlevegas2013$title[i+2],1,1)){
      titlevegas2013$title[i+2] = strsplit(titlevegas2013$title[i],",")[[1]][2]
    }
  
  }
}

#install.packages("RecordLinkage")
#library("RecordLinkage", lib.loc="~/Library/R/3.1/library")
#ClosestMatch2 = function(string, stringVector){
#  distance = levenshteinSim(string, stringVector);
#  stringVector[distance == max(distance)]
#}
titlevegas2013$match_name = 0
titlevegas2013$match_id = 0
#for(i in c(1:length(titlevegas2013$title))){
#  if(length(strsplit(titlevegas2013$title[i],",")[[1]])<2){
#    titlevegas2013$match_name[i] = ClosestMatch2(as.vector(titlevegas2013$title[i]),as.vector(teams$team_name))
#      for(j in c(1:length(teams$team_id))){
#        if(teams$team_name[j]==titlevegas2013$match_name[i]){
#          titlevegas2013$match_id[i] = teams$team_id[j]
#        }
#      }
#  }
#}
for(i in c(1:length(titlevegas2013$title))){
  if(length(strsplit(titlevegas2013$title[i],",")[[1]])<2){
    titlevegas2013$title[i] = simpleCap(titlevegas2013$title[i])
  } 
}
for(i in c(1:length(titlevegas2013$title))){
  if(length(strsplit(titlevegas2013$title[i],",")[[1]])<2){
    for(j in c(1:length(vegas2013oldtable$title))){
      if(vegas2013oldtable$title[j]==titlevegas2013$title[i]){
        titlevegas2013$match_name[i] = vegas2013oldtable$matchname[j]
        titlevegas2013$match_id[i] = vegas2013oldtable$matchteam[j]
      }
    }
  }
}


#########cut-off 2013 and 2014

newvegas2014$title = tolower(newvegas2014$title)
titlevegas2014 = newvegas2014
for(i in c(1:length(titlevegas2014$title))){
  if(length(strsplit(titlevegas2014$title[i],",")[[1]])>1){
    if(substr(strsplit(titlevegas2014$title[i],",")[[1]][1],1,1) == substr(titlevegas2014$title[i+1],1,1)){
      titlevegas2014$title[i+1] = strsplit(titlevegas2014$title[i],",")[[1]][1]
    }
    if(substr(strsplit(titlevegas2014$title[i],",")[[1]][2],1,1)==substr(titlevegas2014$title[i+2],1,1)){
      titlevegas2014$title[i+2] = strsplit(titlevegas2014$title[i],",")[[1]][2]
    }
    
  }
}

titlevegas2014$match_name = 0
titlevegas2014$match_id = 0
#for(i in c(1:length(titlevegas2014$title))){
#  if(length(strsplit(titlevegas2014$title[i],",")[[1]])<2){
#    titlevegas2014$match_name[i] = ClosestMatch2(as.vector(titlevegas2014$title[i]),as.vector(teams$team_name))
#    for(j in c(1:length(teams$team_id))){
#      if(teams$team_name[j]==titlevegas2014$match_name[i]){
#        titlevegas2014$match_id[i] = teams$team_id[j]
#      }
#    }
#  }
#}
for(i in c(1:length(titlevegas2014$title))){
  if(length(strsplit(titlevegas2014$title[i],",")[[1]])<2){
    titlevegas2014$title[i] = simpleCap(titlevegas2014$title[i])
  } 
}
for(i in c(1:length(titlevegas2014$title))){
  if(length(strsplit(titlevegas2014$title[i],",")[[1]])<2){
    for(j in c(1:length(vegas2014oldtable$title))){
      if(vegas2014oldtable$title[j]==titlevegas2014$title[i]){
        titlevegas2014$match_name[i] = vegas2014oldtable$matchname[j]
        titlevegas2014$match_id[i] = vegas2014oldtable$matchteam[j]
      }
    }
  }
}
##add the odds in titlevegas2013 into table comapre_2013
compare_2013$wodds = 0
compare_2013$lodds = 0
for(i in c(1:length(compare_2013$daynum))){
  sign = 0
  set1 = subset(titlevegas2013, titlevegas2013$daynum ==compare_2013$daynum[i] )
  for(j in c(1:length(set1$daynum))){
    if(compare_2013$wteam[i]==set1$match_id[j]&&sign ==0){
      for(k in c(j-2:j+2)){
        if(compare_2013$lteam[i]==set1$match_id[k]){
          compare_2013$wodds[i] = set1$statistics[j]
          compare_2013$lodds[i] = set1$statistics[k]
          sign = 1
          break
        }
      }  
    }
  }
}

for(i in c(1:length(compare_2013$daynum))){
  if(compare_2013$wodds[i]==0&&compare_2013$lodds[i]==0){
    set1 = subset(titlevegas2013, titlevegas2013$daynum ==compare_2013$daynum[i] )
    for(j in c(1:length(set1$daynum))){
      if(compare_2013$wteam[i]==set1$match_id[j]){
        compare_2013$wodds[i] = set1$statistics[j]
      }
      if(compare_2013$lteam[i]==set1$match_id[j]){
        compare_2013$lodds[i] = set1$statistics[j]
      }
    }  
  }
}
##add the odds in titlevegas2014 into table comapre_2014
compare_2014$wodds = 0
compare_2014$lodds = 0
for(i in c(1:length(compare_2014$daynum))){
  sign = 0
  set2 = subset(titlevegas2014, titlevegas2014$daynum ==compare_2014$daynum[i] )
  if(length(set2$daynum)>0){
    for(j in c(1:length(set2$daynum))){
      if((compare_2014$wteam[i]==set2$match_id[j])&&(sign==0)){
        for(k in c(j-2:j+2)){
          if(compare_2014$lteam[i]==set2$match_id[k]){
            compare_2014$wodds[i] = set2$statistics[j]
            compare_2014$lodds[i] = set2$statistics[k]
            sign = 1
            break
          }
        }  
      }
    }
  }
}

for(i in c(1:length(compare_2014$daynum))){
  if(compare_2014$wodds[i]==0&&compare_2014$lodds[i]==0){
    set2 = subset(titlevegas2014, titlevegas2014$daynum ==compare_2014$daynum[i] )
    if(length(set2$daynum)>0){
      for(j in c(1:length(set2$daynum))){
       if(compare_2014$wteam[i]==set2$match_id[j]){
          compare_2014$wodds[i] = set2$statistics[j]
        }
        if(compare_2014$lteam[i]==set2$match_id[j]){
          compare_2014$lodds[i] = set2$statistics[j]
        }
      }
    }  
  }
}


##transfer the wodds and lodds to winner's possiblility 
# first, we define some functions that ransfer money line, spread line 
# to the possibility of one team could win
point2moneyline <- function(x){
  if(x>=-1){
    return(-110) 
  }
  if(x<-1&x>=-2){
    return(-125)
  }
  if(x<-2&x>=-3){
    return(-135)
  }
  if(x<-3&x>=-4){
    return(-185)
  }
  if(x<-4&x>=-5){
    return(-215)
  }
  if(x<-5&x>=-6){
    return(-240)
  }
  if(x<-6&x>=-7){
    return(-270)
  }
  if(x<-7&x>=-8){
    return(-345)
  }
  if(x<-8&x>=-9){
    return(-395)
  }
  if(x<-9&x>=-10){
    return(-470)
  }
  if(x<-10&x>=-11){
    return(-535)
  }
  
  if(x<-11&x>=-12){
    return(-645)
  }
  if(x<-12&x>=-13){
    return(-730)
  }
  if(x<-13&x>=-14){
    return(-805)
  }
  if(x<-14&x>=-15){
    return(-1050)
  }
  if(x<-15&x>=-16){
    return(-1200)
  }
  if(x<-16&x>=-17){
    return(-1425)
  }
  if(x<-17&x>=-19){
    return(-1600)
  }
  if(x<-19){
    return(-2000)
  }
}
vegasmoneyline <- function(x){
  if(x==-110){
    return(0.2779783)
  }
  if(x==-125){
    return(0.5675676)
  }
  if(x==-135){
    return(0.6183206)
  }
  if(x==-185){
    return(0.7532387)
  }
  if(x==-215){
    return(0.7990959)
  }
  if(x==-240){
    return(0.8310811)
  }
  if(x==-270){
    return(0.8613037)
  }
  if(x==-345){
    return(0.9091322)
  }
  if(x==-395){
    return(0.9277326)
  }
  if(x==-470){
    return(0.9463087)
  }
  if(x==-535){
    return(0.9563985)
  }
  if(x==-645){
    return(0.9696302)
  }
  if(x==-730){
    return(0.9752598)
  }
  if(x==-805){
    return(0.979716)
  }
  if(x==-1050){
    return(0.9872082)
  }
  if(x==-1200){
    return(0.9902344)
  }
  if(x==-1425){
    return(0.9929966)
  }
  if(x==-1600){
    return(0.9948187)
  }
  if(x==-2000){
    return(0.9968847)
  }
}
vegaspredictor <- function(x,y){
  if(x<0&y==0){
    return(vegasmoneyline(point2moneyline(x)))
  }
  if(x==0&y<0){
    return(1-vegasmoneyline(point2moneyline(y)))
  }
  if(x<0&y>0){
    x = point2moneyline(x)
    return((x*y)/(x*y-10000))
  }
  if(x>0&y<0){
    y = point2moneyline(y)
    return(1-(x*y/(x*y-10000)))
  }
  if(x==0&y==0){
    return(0)
  }
  
}

#transfer the odds in compare_2013 and compare_2014
compare_2013$wodds = as.numeric(compare_2013$wodds)
compare_2013$lodds = as.numeric(compare_2013$lodds)

for(i in c(1:length(compare_2013$daynum))){
  if(compare_2013$wodds[i]!=0|compare_2013$lodds[i]!=0){
    if(compare_2013$wodds[i]<0){
      compare_2013$wodds[i] = point2moneyline(compare_2013$wodds[i])
    }
    if(compare_2013$lodds[i]<0){
      compare_2013$lodds[i] = point2moneyline(compare_2013$wodds[i])
    }
  }
}
compare_2013$vegas = 0
compare_2013$vegas = as.numeric(compare_2013$vegas)

for(i in c(1:length(compare_2013$daynum))){
  if(is.na(compare_2013$wodds[i])){
    compare_2013$wodds[i] = 0
  }
  if(is.na(compare_2013$lodds[i])){
    compare_2013$wodds[i] = 0
  }
  if(compare_2013$wodds[i]!=0&compare_2013$lodds[i]!=0){
    if(compare_2013$wodds[i]<0){
      compare_2013$vegas[i] = ((-compare_2013$wodds[i])*(compare_2013$lodds[i]))/(10000+((-compare_2013$wodds[i])*(compare_2013$lodds[i])))
    }
    else {
      compare_2013$vegas[i] = 10000/(10000-(compare_2013$wodds[i])*(compare_2013$lodds[i]))
    }
  }
  if(compare_2013$wodds[i]<0&compare_2013$lodds[i]==0){
      compare_2013$vegas[i] = vegasmoneyline(compare_2013$wodds[i])
  }
  if(compare_2013$wodds[i]==0&compare_2013$lodds[i]<0){
    compare_2013$vegas[i] = 1- vegasmoneyline(compare_2013$lodds[i])
  }
}  
compare_2013$lvegas = 0
for(i in c(1:length(compare_2013$vegas))){
  if(compare_2013$vegas[i]!=0){
    compare_2013$lvegas[i] = 1 -compare_2013$vegas[i]
  }
}

###########line between 2013 and 2014
compare_2014$wodds = as.numeric(compare_2014$wodds)
compare_2014$lodds = as.numeric(compare_2014$lodds)
for(i in c(1:length(compare_2014$daynum))){
  if(is.na(compare_2014$wodds[i])){
    compare_2014$wodds[i] = 0
  }
  if(is.na(compare_2014$lodds[i])){
    compare_2014$lodds[i] = 0
  }
}
for(i in c(1:length(compare_2014$daynum))){
    if(compare_2014$wodds[i]!=0|compare_2014$lodds[i]!=0){
      if(compare_2014$wodds[i]<0){
        compare_2014$wodds[i] = point2moneyline(compare_2014$wodds[i])
     }
      if(compare_2014$lodds[i]<0){
        compare_2014$lodds[i] = point2moneyline(compare_2014$wodds[i])
      }
    }
}
compare_2014$vegas = 0
compare_2014$vegas = as.numeric(compare_2014$vegas)

for(i in c(1:length(compare_2014$daynum))){
  if(compare_2014$wodds[i]!=0&compare_2014$lodds[i]!=0){
    if(compare_2014$wodds[i]<0){
      compare_2014$vegas[i] = ((-compare_2014$wodds[i])*(compare_2014$lodds[i]))/(10000+((-compare_2014$wodds[i])*(compare_2014$lodds[i])))
    }
    else {
      compare_2014$vegas[i] = 10000/(10000-(compare_2014$wodds[i])*(compare_2014$lodds[i]))
    }
  }
  if(compare_2014$wodds[i]<0&compare_2014$lodds[i]==0){
    compare_2014$vegas[i] = vegasmoneyline(compare_2014$wodds[i])
  }
  if(compare_2014$wodds[i]==0&compare_2014$lodds[i]<0){
    compare_2014$vegas[i] = 1- vegasmoneyline(compare_2014$lodds[i])
  }
}  
compare_2014$lvegas = 0
for(i in c(1:length(compare_2014$vegas))){
  if(compare_2014$vegas[i]!=0){
    compare_2014$lvegas[i] = 1 -compare_2014$vegas[i]
  }
}

###split the compare_2013 and compare_2014 to a table whose each row only contains one team
wnamelist <- c(colnames(compare_2013)[c(1:4,7:21,37,40)])
lnamelist <- c(colnames(compare_2013)[c(1,2,5:8,22:34,37,41)])
w2013 = subset(compare_2013,select=wnamelist)
l2013 = subset(compare_2013,select=lnamelist)
w2014 = subset(compare_2014,select=wnamelist)
l2014 = subset(compare_2014,select=lnamelist)
#change the location of loser subset from wloc to lloc.
for(i in c(1:length(l2013$wloc))){ 
  if(l2013$wloc[i]=='A') {wlocnumber = 1}
  if(l2013$wloc[i]=='H') {wlocnumber = 2}
  if(l2013$wloc[i]=='N') {wlocnumber = 3}
  l2013$wloc[i]<-switch(wlocnumber,'H','A','N')
}
for(i in c(1:length(l2014$wloc))){ 
  if(l2014$wloc[i]=='A') {wlocnumber = 1}
  if(l2014$wloc[i]=='H') {wlocnumber = 2}
  if(l2014$wloc[i]=='N') {wlocnumber = 3}
  l2014$wloc[i]<-switch(wlocnumber,'H','A','N')
}
#change the seeddistance in l2013 and l2014
l2013$seeddistance = -(l2013$seeddistance)
l2014$seeddistance = -(l2014$seeddistance)

##change the name of column in each subset
namelist = c('season','daynum','team','score','loc','numot','fgm','fga','fgm3','fga3','ftm','fta','or','dr','ast','to','stl','blk','pf','seeddistance','vegas')
colnames(l2013) <- namelist
colnames(w2013) <- namelist
colnames(l2014) <- namelist
colnames(w2014) <- namelist
#add result to lsubset and wsubset
l2013$result <- c(rep(0,each=(length(l2013$team))))
w2013$result <- c(rep(1,each=(length(w2013$team))))
l2014$result <- c(rep(0,each=(length(l2014$team))))
w2014$result <- c(rep(1,each=(length(w2014$team))))
#merge lsubset and wsubset
training2013 = rbind(w2013,l2013)
training2013 = training2013[with(training2013,order(training2013$daynum)),]
training2014 = rbind(w2014,l2014)
training2014 = training2014[with(training2014,order(training2014$daynum)),]
trainingset = rbind(training2013,training2014)
###build model 
##(1)logistics regression
#transform character value to number
training2013$loc <- as.integer(training2013$loc)
training2014$loc <- as.integer(training2014$loc)
trainingset$loc <- as.integer(trainingset$loc)

logr_vm <- glm(result ~ loc+fgm+fga+fgm3+fga3+ftm+fta+or+dr+ast+to+stl+blk+pf+seeddistance+vegas, data=trainingset, family=binomial(logit))
prob=predict(logr_vm,type=c("response"))
result2013 = training2013
result2013$prob=prob
str(table(trainingset$result,logr_vm$fitted.values))
library("pROC", lib.loc="~/Library/R/3.1/library")

###(2)random forest
#import the random forest library
library( "randomForest" )
dim(training2013)
rf <- randomForest(result~loc+fgm+fga+fgm3+fga3+ftm+fta+or+dr+ast+to+stl+blk+pf+seeddistance+vegas,trainingset,ntree=50,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE)
print(rf)
predict(rf,training2013[c(5,7:21)][1,])
table(predict(rf), training2013$result)
plot(rf)
importance(rf)

#evaluate the model
teamrelation <- function(x,y){
  xdata = 0
  ydata = 0
  xcount = 0
  ycount = 0
  ###this is team A's average data  
  for(i in c(1:length(trainingset$season))) {
    if(grepl(x,training2013$team[i])){
      xdata = xdata + training2013[i,c(7:19)]
      xcount = xcount+1
    }
  }
  if(xcount!=0){
    xdata = xdata/xcount
  }
  else{
    xdata = 0
  }
  ###this is team B's average data  
  for(i in c(1:length(training2013$season))) {
    if(grepl(y,training2013$team[i])){
      ydata = ydata + training2013[i,c(7:19)]
      ycount = ycount+1
    }
  }
  if(ycount!=0){
    ydata = ydata/ycount
  }
  else{
    ydata = 0
  }
  return(list(xdata,ydata))
}

####teamwinpossibility(A,B) function can get the posibility that team A beat team B
teamwinpossibility = function(y,z,vegasy,vegasz){
  x = teamrelation(y,z)
  yseed = 0
  zseed = 0
  add = 0
  add = x[[1]]-x[[2]]
  add$loc = 3
  subseeds = subset(seeds,seeds$season == "2013")
  for(i in c(1:length(subseeds$season))){
    if(y==subseeds$team[i]){
      yseed = subseeds$seedpercent[i]
    }
    if(z==subseeds$team[i]){
      zseed = subseeds$seedpercent[i]
    }
  }
  add$seeddistance = (yseed-zseed)/16
  add$vegas = vegaspredictor(as.numeric(vegasy),as.numeric(vegasz))
  return(predict(logr_vm,add,type="response"))
  #return(predict(rf,add,type="response"))
}

result = function(x,y,vegasx,vegasy){
  if(teamwinpossibility(x,y,vegasx,vegasy)>teamwinpossibility(y,x,vegasy,vegasx)){
    cat("team",x,"win")
    return(x)
  }
  else{
    cat("team",y,"win")
    return(y)
  }
}
## find the tournament team list by manually and automatically
tourteams2013 = list()
sub2013 = subset(ncaa_tournament,season == 2013)
for(i in c(1:length(sub2013$season))){
  tourteams2013[[i]] = c(sub2013$daynum[i],sub2013$wteam[i],sub2013$lteam[i])
}


subtitlevegas2013 = subset(titlevegas2013, as.numeric(titlevegas2013$daynum) >= 134)
for(i in c(1:length(tourteams2013))){
  for(j in c(1:length(subtitlevegas2013$daynum))){
    if((tourteams2013[[i]][2]==subtitlevegas2013$match_id[j])&(tourteams2013[[i]][1]==subtitlevegas2013$daynum[j])){
      tourteams2013[[i]][4] = subtitlevegas2013$statistics[j]
    }
    if((tourteams2013[[i]][3]==subtitlevegas2013$match_id[j])&(tourteams2013[[i]][1]==subtitlevegas2013$daynum[j])){
      tourteams2013[[i]][5] = subtitlevegas2013$statistics[j]
    }
  }
}
##manually adjust the tourteams2013
tourteams2013[[1]][4] = 0
tourteams2013[[1]][5] = 0
tourteams2013[[2]][4] = 0
tourteams2013[[2]][5] = 0
tourteams2013[[3]][5] = -1
tourteams2013[[6]][5] = 121.5
tourteams2013[[7]][4] = 130
tourteams2013[[7]][5] = -3.5
tourteams2013[[9]][5] = 127
tourteams2013[[11]][5] = 123.5
tourteams2013[[14]][4] = -11.5
tourteams2013[[14]][5] = 140.5
tourteams2013[[15]][4] = -11.5
tourteams2013[[15]][5] = 140.5
tourteams2013[[19]][4] = -8
tourteams2013[[20]][4] = 119.5
tourteams2013[[20]][5] = -4.5
tourteams2013[[28]][5] = 132
tourteams2013[[31]][5] = 134
tourteams2013[[33]][4] = -5
tourteams2013[[35]][4] = -2.5
tourteams2013[[35]][5] = 132
tourteams2013[[36]][5] = -4
tourteams2013[[40]][4] = -3.5
tourteams2013[[40]][5] = 144.5
tourteams2013[[41]][4] = -6
tourteams2013[[42]][5] = -5
tourteams2013[[44]][4] = 130.5
tourteams2013[[48]][4] = -12
tourteams2013[[49]][5] = 146
tourteams2013[[51]][5] = 128.5
tourteams2013[[55]][5] = -5
tourteams2013[[58]][5] = 137.5
tourteams2013[[62]][5] = -5
tourteams2013[[64]][4] = 135
tourteams2013[[65]][4] = -9.5
tourteams2013[[65]][5] = 133.5
tourteams2013[[66]][4] = -1.5
tourteams2013[[67]][4] = -4

for(i in c(1:length(tourteams2013))){
  tourteams2013[[i]][4] = as.numeric(tourteams2013[[i]][4])
  tourteams2013[[i]][5] = as.numeric(tourteams2013[[i]][5])
  
}
View(tourteams2013)
count = 0
for(i in c(5:67)){
  if(result(tourteams2013[[i]][2],tourteams2013[[i]][3],tourteams2013[[i]][4],tourteams2013[[i]][5])==tourteams2013[[i]][2]){
    count = count+1
  }
}
count/63
##test how much we could earn by it
account = 0
for(i in c(5:67)){
  if(result(tourteams2013[[i]][2],tourteams2013[[i]][3],tourteams2013[[i]][4],tourteams2013[[i]][5])==tourteams2013[[i]][2]){
    money = as.numeric(ifelse(tourteams2013[[i]][4]>0,tourteams2013[[i]][4],point2moneyline(as.numeric(tourteams2013[[i]][4]))))
      if(money>0){
        account = account + money
      }
      else{
        account = account - 10000/money
      }
  }
  else{
      account = account - 100    
  }
}
account/6300
###get the baseline : 
##(1) High seeds beat low seeds method
##(2) Vegas probility
#findseed function

###Baseline(1)
findseed <- function(x,year){
  subseeds = subset(seeds,season==year)
  for(i in c(1:length(subseeds$team))){
    if(x==subseeds$team[i]){
      return(subseeds$seedpercent[i])
    }
  }
}
result_highlow = function(x,y,year){
  if(findseed(x,year)>findseed(y,year)){
    cat("team",x,"win")
    return(x)
  }
  if(findseed(y,year)>findseed(x,year)){
    cat("team",y,"win")
    return(y)
  }
  if(findseed(y,year)==findseed(x,year)){
    return(ifelse(runif(1)>0.5,x,y))
  }
}
count_highlow = 0
for(time in c(1:100)){
  for(i in c(5:67)){
  if(result_highlow(tourteams2013[[i]][2],tourteams2013[[i]][3],2013)==tourteams2013[[i]][2]){
    count_highlow = count_highlow +1
  }
  }
}
count_highlow/6300

account_highlow = 0
for(time in c(1:100)){
  for(i in c(5:67)){
    if(result_highlow(tourteams2013[[i]][2],tourteams2013[[i]][3],2013)==tourteams2013[[i]][2]){
      money = as.numeric(ifelse(tourteams2013[[i]][4]>0,tourteams2013[[i]][4],point2moneyline(as.numeric(tourteams2013[[i]][4]))))
        if(money>0){
          account_highlow = account_highlow + money
        }
        else{
          account_highlow = account_highlow - 10000/money
        }
    }
    else{
      account_highlow = account_highlow - 100    
    }
  }
}
account_highlow/6300
###Baseline(2)
account_vegas = 0
for(i in c(5:67)){
  if(ifelse(tourteams2013[[i]][4]<0,tourteams2013[[i]][2],tourteams2013[[i]][3])==tourteams2013[[i]][2]){
    money = as.numeric(ifelse(tourteams2013[[i]][4]>0,tourteams2013[[i]][4],point2moneyline(as.numeric(tourteams2013[[i]][4]))))
    if(money>0){
      account_vegas = account_vegas + money
    }
    else{
      account_vegas = account_vegas - 10000/money
    }
  }
  else{
    account_vegas = account_vegas - 100    
  }
}
account_vegas/6300
count_vegas = 0
for(i in c(5:67)){
  if(ifelse(tourteams2013[[i]][4]<0,tourteams2013[[i]][2],tourteams2013[[i]][3])==tourteams2013[[i]][2]){
    count_vegas = count_vegas +1
  }
}
count_vegas/63