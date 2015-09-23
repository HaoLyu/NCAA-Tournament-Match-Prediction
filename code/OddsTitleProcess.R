#function simpleCap is used to captalize every first letter of a word.
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
#function findinlist is used to find if x is in list y
findinlist <- function(x,list){
  for(i in c(1:length(list))){
    if(list[[i]]==x){
      return(TRUE)
    }
  }
  return(FALSE)
}
#we captalize every first letter in table teams and build it newteams
newteams = teams
for(i in c(1:length(newteams$team_id))){
  newteams$team_name[i] = simpleCap(tolower(newteams$team_name[i]))
}
#we copy a new set titlevegas2014_new
titlevegas2014_new = titlevegas2014
#we captalize each team's name tile
for(i in c(1:length(titlevegas2014_new$title))){
  if(length(strsplit(titlevegas2014_new$title[i],",")[[1]])<2){
    titlevegas2014_new$title[i] = simpleCap(titlevegas2014_new$title[i])
  } 
}
#we build a list vegas2014list to store all names of teams in 2014 game
vegas2014list = list(titlevegas2014_new$title[2])

for(i in c(1:length(titlevegas2014_new$title))){
  if(length(strsplit(titlevegas2014_new$title[i],",")[[1]])<2){
     if(!findinlist(titlevegas2014_new$title[i],vegas2014list)){
       vegas2014list = c(vegas2014list,titlevegas2014_new$title[i])
     }
  } 
}
#we define a function that could match charater between newteams and vegas2014list
#and return the team_id or 0.
#we also need function maxlist(x) and compare(x,y) to support our function namematch
#compare function could find how relevance are two team names x and y, then return the value.
compare <- function(x,y){
  y_new = paste(strsplit(y," ")[[1]][1],tolower(strsplit(y," ")[[1]][2]),tolower(strsplit(y," ")[[1]][3]),tolower(strsplit(y," ")[[1]][4]),sep='')
  score = 0
  if(length(strsplit(x," ")[[1]])>1){
    for(i in c(1:length(strsplit(x," ")[[1]]))){
      if(findinlist((substring(strsplit(x," ")[[1]][i],1,2)),(substring(strsplit(y," ")[[1]],1,2)))){
        score = score+0.4/length(strsplit(x," ")[[1]])
      }
    }
    for(j in c(1,length(strsplit(x," ")[[1]]))){
      if(findinlist(strsplit(x," ")[[1]][j],strsplit(y," ")[[1]])){
        score = score + 0.3
      }
    }
    return(score)
  }
  else{
    if(findinlist(x,strsplit(y," ")[[1]])&(length(strsplit(y," ")[[1]])==1)){
      score = 1
      return(score)
    }
    else{
      if(grepl(x,y_new)){
        score = 0.6
        return(score)
      }
    }
    return(0)
  }
}
#maxlist finds the max value in list x.
maxlist <- function(x){  
  max = x[[1]]
  for(i in c(1:length(x))){
    max = ifelse(max>x[[i]],max,x[[i]])
  }
  return(max)
}

namematch <- function(x){
  tmp = list()
  for(i in c(1:length(newteams$team_name))){
    if(substring(x,1,1)==substring(newteams$team_name[i],1,1)){
      tmp = c(tmp, compare(x,newteams$team_name[i])) 
      names(tmp)[length(tmp)] = newteams$team_id[i] 
    }
  }
  maxtmpkey = 0
  for(j in c(1:length(tmp))){
    if(maxlist(tmp)==tmp[j]){
      maxtmpkey = names(tmp)[j]
    }
  }
  if(maxlist(tmp)>=0.5&(maxtmpkey!=0)&(!is.na(maxtmpkey))){
    return(maxtmpkey)
  }
  else{
    return(0)
  }
}

#we build a new list vegas2014oldtable, and it can match all teams in titlevegas2014_new
#which are similar to team title in newteams table.
#vegas2014oldtable has three features: title(team name in titlevegas2014_new), 
#matchteam(team id in newteams table), matchname(team name in newteams table)
vegas2014oldtable = list(0)
for( i in c(1:length(vegas2014list))){
  vegas2014oldtable$title[i] = vegas2014list[[i]]
}
for(i in c(1:length(vegas2014oldtable$title))){
  vegas2014oldtable$matchteam[i] = namematch(vegas2014oldtable$title[i])
}
for(i in c(1:length(vegas2014oldtable$matchteam))){
  if(vegas2014oldtable$matchteam[i]!='0'){
    for(j in c(1:length(newteams$team_name))){
      if(newteams$team_id[j]==vegas2014oldtable$matchteam[i]){
        vegas2014oldtable$matchname[i] = newteams$team_name[j]  
      } 
    }
  }
  else {
    vegas2014oldtable$matchname[i] = '0'
  }
}
vegas2014oldtable = data.frame(title=c(vegas2014oldtable$title),matchteam=c(vegas2014oldtable$matchteam),matchname=c(vegas2014oldtable$matchname),stringsAsFactors=FALSE)
### Then we manually fix the list vegas2014oldtable
# we define a function fixvegas2014 that helps us fix the table manually
fixvegas2014 <- function(x,y){
  z=''
  for(i in c(1:length(newteams$team_id))){
    if(newteams$team_id[i]==y){
      z = newteams$team_name[i]
      break
    }
  }
  return(c(y,z))
}

## faillist2014 is the part in vegas2014oldtable we need to fix manually 
faillist2014 = c()
for(i in c(1:length(vegas2014oldtable$title))){
  if(vegas2014oldtable$matchname[i]=='0'){
    faillist2014 = c(faillist2014,i)
  }
}
#we find all the correct team_id and team_name in newteams and build a fixlist2014 which corresponds to faillist2014.
fixlist2014 = c()

#we finally fix the table vegas2014oldtable
for(i in c(1:length(faillist2014))){
  vegas2014oldtable[faillist2014[i],][2:3] = fixvegas2014(faillist2014[i],fixlist2014[i])
}

############################
##cut-off 2013 and 2014
#then we build the similar team list in 2013 which is named vegas2013oldtable

#we copy a new set titlevegas2013_new
titlevegas2013_new = titlevegas2013
#we captalize each team's name tile
for(i in c(1:length(titlevegas2013_new$title))){
  if(length(strsplit(titlevegas2013_new$title[i],",")[[1]])<2){
    titlevegas2013_new$title[i] = simpleCap(titlevegas2013_new$title[i])
  } 
}
#we build a list vegas2013list to store all names of teams in 2013 game
vegas2013list = list(titlevegas2013_new$title[2])

for(i in c(1:length(titlevegas2013_new$title))){
  if(length(strsplit(titlevegas2013_new$title[i],",")[[1]])<2){
    if(!findinlist(titlevegas2013_new$title[i],vegas2013list)){
      vegas2013list = c(vegas2013list,titlevegas2013_new$title[i])
    }
  } 
}

#we build a new list vegas2013oldtable, and it can match all teams in titlevegas2013_new
#which are similar to team title in newteams table.
#vegas2013oldtable has three features: title(team name in titlevegas2013_new), 
#matchteam(team id in newteams table), matchname(team name in newteams table)
vegas2013oldtable = list()
for( i in c(1:length(vegas2013list))){
  vegas2013oldtable$title[i] = vegas2013list[[i]]
}
for(i in c(1:length(vegas2013oldtable$title))){
  vegas2013oldtable$matchteam[i] = namematch(vegas2013oldtable$title[i])
}
for(i in c(1:length(vegas2013oldtable$matchteam))){
  if(vegas2013oldtable$matchteam[i]!='0'){
    for(j in c(1:length(newteams$team_name))){
      if(newteams$team_id[j]==vegas2013oldtable$matchteam[i]){
        vegas2013oldtable$matchname[i] = newteams$team_name[j]  
      } 
    }
  }
  else {
    vegas2013oldtable$matchname[i] = '0'
  }
}
vegas2013oldtable = data.frame(title=c(vegas2013oldtable$title),matchteam=c(vegas2013oldtable$matchteam),matchname=c(vegas2013oldtable$matchname),stringsAsFactors=FALSE)


### Then we manually fix the list vegas2013oldtable
# we define a function fixvegas2013 that helps us fix the table manually
fixvegas2013 <- function(x,y){
  z=''
  for(i in c(1:length(newteams$team_id))){
    if(newteams$team_id[i]==y){
      z = newteams$team_name[i]
      break
    }
  }
  return(c(y,z))
}

## faillist2013 is the part in vegas2013oldtable we need to fix manually 
faillist2013 = c()
for(i in c(1:length(vegas2013oldtable$title))){
  if(vegas2013oldtable$matchname[i]=='0'){
    faillist2013 = c(faillist2013,i)
  }
}
#we find all the correct team_id and team_name in newteams and build a fixlist2013 which corresponds to faillist2013.
fixlist2013 = c()

#we finally fix the table vegas2014oldtable
for(i in c(1:length(faillist2013))){
  vegas2013oldtable[faillist2013[i],][2:3] = fixvegas2013(faillist2013[i],fixlist2013[i])
}



###After we refine two talbes vegas2013oldtable and vegas2014oldtable, which are the dictionaries for 
###teams in titlevegas2013/2014 and newteams
### Then we refine all the team names in titlevegas2014_new and titlevegas2013_new
for(i in c(1:length(titlevegas2014_new$title))){
  if(length()<2)
}
###Finally we replace titlevegas2014 and titlevegas2013 with titlevegas2014_new and titlevegas2013_new