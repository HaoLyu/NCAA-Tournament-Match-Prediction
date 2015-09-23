######new idea
#consider finding the dark horse team 


## step 1:  transfer the format of seed in seeds and name it seedpercent.
## seedpercent ranges from 0 to 1. 
## seed 1 is represented by 16, seed 2 is 15,,, seed 16 is 1, none-seed is 0. 
for(i in c(1:length(seeds$season))){
  seeds$seedpercent[i] = 17 - as.integer(substr(seeds$seed[i],2,3))
}



##  step 2:add a new feature 'wseed' 'lseed' in ncaa_regular
ncaa_regular$wseed = 0
ncaa_regular$lseed = 0
for(i in c(1:length(ncaa_regular$season))){
  for(j in c(1:length(seeds$season))){
    if(ncaa_regular$season[i]==seeds$season[j]){
      if(ncaa_regular$wteam[i]==seeds$team[j]){
        ncaa_regular$wseed[i] = seeds$seedpercent[j]
      }
      if(ncaa_regular$lteam[i]==seeds$team[j]){
        ncaa_regular$lseed[i] = seeds$seedpercent[j]
      } 
    }   
  } 
}

##  step 3:add a new feature 'wseed' 'lseed' in new_ncaa_regular_compare_2003
new_ncaa_regular_compare_2003$wseed = 0
new_ncaa_regular_compare_2003$lseed = 0
for(i in c(1:4616)){
  for(j in c(1:length(seeds$season))){
    if(new_ncaa_regular_compare_2003$season[i]==seeds$season[j]){
      if(new_ncaa_regular_compare_2003$team[i]==seeds$team[j])
       
        { new_ncaa_regular_compare_2003$wseed[i] = seeds$seedpercent[j]
        }
      if(new_ncaa_regular_compare_2003$team[i+4616]==seeds$team[j])
        { new_ncaa_regular_compare_2003$lseed[i] = seeds$seedpercent[j]
        } 
    }   
  } 
  new_ncaa_regular_compare_2003$seeddistance[i] = new_ncaa_regular_compare_2003$wseed[i]-new_ncaa_regular_compare_2003$lseed[i]
}

for(i in c(4617:9232)){
  new_ncaa_regular_compare_2003$wseed[i] = new_ncaa_regular_compare_2003$wseed[i-4616]
  new_ncaa_regular_compare_2003$lseed[i] = new_ncaa_regular_compare_2003$lseed[i-4616]
  new_ncaa_regular_compare_2003$seeddistance[i] = -new_ncaa_regular_compare_2003$seeddistance[i-4616]
}

for(i in c(1:4616)){
  new_ncaa_regular_compare_2003$scoredistance[i] = new_ncaa_regular_compare_2003$score[i] - new_ncaa_regular_compare_2003$score[i+4616]
}
for(i in c(4617:9232)){
  new_ncaa_regular_compare_2003$scoredistance[i] = - new_ncaa_regular_compare_2003$scoredistance[i-4616]
}