install.packages("remotes")
library(remotes)
install_github("TilburgNetworkGroup/remify")
install_github("TilburgNetworkGroup/remstats")
install.packages("hash")
install.packages("Rcpp")
#install.packages("imputeTS")
install.packages("lubridate")
#install.packages("tidyverse")
install.packages("relevent")
install.packages("abind")
install.packages("dplyr")

library(Rcpp)
library(remstats)
library(lubridate)
library(relevent)
library(remify)
library(hash)
#library(imputeTS)
#library(tidyverse)
library(abind)
library(dplyr)


relation <- read.csv("RelationshipsFromSurveys.csv")
music <- read.csv("MusicGenrePreference.csv")

genres <- unique(music$genre)


"%notin%" <- Negate("%in%")
no_one <- c()
for (i in 1:84){
  if (i %notin% relation$id.A){
    if (i %notin% relation$id.B){
      no_one <- c(no_one, i)
    }
  }
}

#########Relation##############

relation <- read.csv("RelationshipsFromSurveys.csv")
relation <- relation[relation$relationship == "CloseFriend" | relation$relationship == "SocializeTwicePerWeek" | relation$relationship == "PoliticalDiscussant", ]

for(i in 1:nrow(relation)){
  if (relation$survey.date[i] == "2008-09-09"){
    relation$survey.date[i] <- 1
  } else if (relation$survey.date[i] == "2008-10-19"){
    relation$survey.date[i] <- 2
  } else if (relation$survey.date[i] == "2009-03-05"){
    relation$survey.date[i] <- 3
  } else if (relation$survey.date[i] == "2009-05-18"){
    relation$survey.date[i] <- 4
  } else if (relation$survey.date[i] == "2008-12-13"){ 
    relation$survey.date[i] <- 5
  } else if (relation$survey.date[i] == "2009-04-17"){
    relation$survey.date[i] <- 6
  }
}  


relation_1 <- relation[relation$survey.date == 1,]
relation_2 <- relation[relation$survey.date == 2,]
relation_3 <- relation[relation$survey.date == 3,]
relation_4 <- relation[relation$survey.date == 4,]
relation_5 <- relation[relation$survey.date == 5,]
relation_6 <- relation[relation$survey.date == 6,]

######getting data from earlier surveys#########

lst_2 <- c()
for (i in 1:84){
  if (i %notin% relation_2$id.A){
    if (i %in% relation_1$id.A){
      lst_2 <- c(lst_2,i)
    }
  }
}
df_2 <- data.frame()
for (i in lst_2){
  df_2 <- rbind(df_2, relation_1[relation_1$id.A == i,])
}
relation_2 <- rbind(relation_2, df_2)

lst_3 <- c()
for (i in 1:84){
  if (i %notin% relation_3$id.A){
    if (i %in% relation_5$id.A | i %in% relation_2$id.A | i %in% relation_1$id.A) {
      lst_3 <- c(lst_3, i)
    }
  }
}  

df_3 <- data.frame()
for (i in lst_3){
  if (i %in% relation_5$id.A){
    df_3 <- rbind(df_3, relation_5[relation_5$id.A == i,])
  } else if (i %in% relation_2$id.A){
    df_3 <- rbind(df_3, relation_2[relation_2$id.A == i,])
  } else if (i %in% relation_1$id.A){
    df_3 <- rbind(df_3, relation_1[relation_1$id.A == i,])
  }
  
}
relation_3 <- rbind(relation_3, df_3)

lst_4 <- c()
for (i in 1:84){
  if (i %notin% relation_4$id.A){
    if (i %in% relation_6$id.A | i %in% relation_5$id.A | i %in% relation_3$id.A | i %in% relation_2$id.A | i %in% relation_1$id.A) {
      lst_4 <- c(lst_4, i)
    }
  }
}  

df_4 <- data.frame()
for (i in lst_4){
  if (i %in% relation_6$id.A){
    df_4 <- rbind(df_4, relation_6[relation_6$id.A == i,])
  } else if (i %in% relation_3$id.A){
    df_4 <- rbind(df_4, relation_3[relation_3$id.A == i,])
  } else if (i %in% relation_5$id.A){
    df_4 <- rbind(df_4, relation_5[relation_5$id.A == i,])
  } else if (i %in% relation_2$id.A){
    df_4 <- rbind(df_4, relation_2[relation_2$id.A == i,])
  } else if (i %in% relation_1$id.A){
    df_4 <- rbind(df_4, relation_1[relation_1$id.A == i,])
  }  
}
relation_4 <- rbind(relation_4, df_4)

####making matrixes###
friend_1 <- matrix(0, nrow = 84, ncol = 84)

for (i in 1:nrow(relation_1)){
  y <- relation_1$id.A[i]
  x <- relation_1$id.B[i]
  friend_1[x,y] <- friend_1[x,y] + 1
}
colnames(friend_1) <- c(1:84)
rownames(friend_1) <- c(1:84)

friend_2 <- matrix(0, nrow = 84, ncol = 84)

for (i in 1:nrow(relation_2)){
  y <- relation_2$id.A[i]
  x <- relation_2$id.B[i]
  friend_2[x,y] <- friend_2[x,y] + 1
}
colnames(friend_2) <- c(1:84)
rownames(friend_2) <- c(1:84)

friend_3 <- matrix(0, nrow = 84, ncol = 84)

for (i in 1:nrow(relation_3)){
  y <- relation_3$id.A[i]
  x <- relation_3$id.B[i]
  friend_3[x,y] <- friend_3[x,y] + 1
}
colnames(friend_3) <- c(1:84)
rownames(friend_3) <- c(1:84)

friend_4 <- matrix(0, nrow = 84, ncol = 84)

for (i in 1:nrow(relation_4)){
  y <- relation_4$id.A[i]
  x <- relation_4$id.B[i]
  friend_4[x,y] <- friend_4[x,y] + 1
}
colnames(friend_4) <- c(1:84)
rownames(friend_4) <- c(1:84)

for (i in 1:84){
  friend_1[i,i] <- 0
  friend_2[i,i] <- 0
  friend_3[i,i] <- 0
  friend_4[i,i] <- 0
  for (k in 1:84){
    if (friend_1[i,k] > 0 & friend_1[k,i] > 0){
      friend_1[i,k] <- 1
      friend_1[k,i] <- 1
    } else {
      friend_1[i,k] <- 0
      friend_1[k,i] <- 0
    }
    if (friend_2[i,k] > 0 & friend_2[k,i] > 0){
      friend_2[i,k] <- 1
      friend_2[k,i] <- 1
    } else {
      friend_2[i,k] <- 0
      friend_2[k,i] <- 0
    }
    if (friend_3[i,k] > 0 & friend_3[k,i] > 0){
      friend_3[i,k] <- 1
      friend_3[k,i] <- 1
    } else {
      friend_3[i,k] <- 0
      friend_3[k,i] <- 0
    }
    if (friend_4[i,k] > 0 & friend_4[k,i] > 0){
      friend_4[i,k] <- 1
      friend_4[k,i] <- 1
    } else {
      friend_4[i,k] <- 0
      friend_4[k,i] <- 0
    }
  }
}

isSymmetric(friend_4)
##############Music#########
music <- read.csv("MusicGenrePreference.csv")

music <- music[music$score == "3 High Interest"| music$score == "2 Moderate Interest" | music$score == "1 Slight Interest", ]

lst <- list()
for(i in 1:nrow(music)){
  if (music$date[i] == "2008-09-15" | music$date[i] == "2008-09-16" | music$date[i] == "2008-09-18" | music$date[i] == "2008-09-19" | music$date[i] == "2008-09-22"){
    music$date[i] <- 1
  } else if (music$date[i] == "2008-10-29" |
             music$date[i] == "2008-10-30" |
             music$date[i] == "2008-10-31" |
             music$date[i] == "2008-11-01" |
             music$date[i] == "2008-11-02" |
             music$date[i] == "2008-11-03" |
             music$date[i] == "2008-11-06" |
             music$date[i] == "2008-11-11"){
    music$date[i] <- 2
  } else if (music$date[i] == "2009-03-14" |music$date[i] == "2009-03-15"){
    music$date[i] <- 3
  } else if (music$date[i] == "2009-05-20" | music$date[i] == "2009-05-21" |music$date[i] == "2009-05-22" |music$date[i] == "2009-05-26" |music$date[i] == "2009-05-27" |music$date[i] == "2009-05-28" |music$date[i] == "2009-05-29" |music$date[i] == "2009-05-30" |music$date[i] == "2009-06-01" |music$date[i] == "2009-06-02" |music$date[i] == "2009-06-03" |music$date[i] == "2009-06-04" |music$date[i] == "2009-06-05"){
    music$date[i] <- 4
  } else{
    list <- append(list, i)
  }
}      

music_1 <- music[music$date == 1,]
music_2 <- music[music$date == 2,]
music_3 <- music[music$date == 3,]
music_4 <- music[music$date == 4,]

genres <- unique(music$genre)

#data from earlier surveys#

lst_2 <- c()
for (i in 1:84){
  if (i %notin% music_2$user.id){
    if (i %in% music_1$user.id){
      lst_2 <- c(lst_2,i)
    }
  }
}

df_2 <- data.frame()
for (i in lst_2){
  df_2 <- rbind(df_2, music_1[music_1$user.id == i,])
}
music_2 <- rbind(music_2, df_2)

lst_3 <- c()
for (i in 1:84){
  if (i %notin% music_3$user.id){
    if (i %in% music_1$user.id | i %in% music_2$user.id){
      lst_3 <- c(lst_3,i)
    }
  }
}

df_3 <- data.frame()
for (i in lst_3){
  if (i %in% music_2$user.id){
    df_3 <- rbind(df_3, music_2[music_2$user.id == i,])
  } else {
    df_3 <- rbind(df_3, music_1[music_1$user.id == i,])
  }
}
music_3 <- rbind(music_3, df_3)

lst_4 <- c()
for (i in 1:84){
  if (i %notin% music_3$user.id){
    if (i %in% music_1$user.id | i %in% music_2$user.id | i %in% music_3$user.id){
      lst_4 <- c(lst_4,i)
    }
  }
}

df_4 <- data.frame()
for (i in lst_4){
  if (i %in% music_3$user.id){
    df_4 <- rbind(df_4, music_3[music_3$user.id == i,])
  } else if (i %in% music_2$user.id){
    df_4 <- rbind(df_4, music_2[music_2$user.id == i,])
  } else{
    df_4 <- rbind(df_4, music_1[music_1$user.id == i,])
  }
}
music_4 <- rbind(music_4, df_4)

#prepping data#


musicmatrix1 <- matrix(0, nrow = 84, ncol = length(genres))
colnames(musicmatrix1) <- genres


for (i in 1:nrow(music_1)){
  x <- music_1$user.id[i]
  y <- music_1$genre[i]
  if (music_1$score[i] == "3 High Interest"){
    musicmatrix1[x,y] <- 1
  } else if ( music_1$score[i] == "2 Moderate Interest"){
    musicmatrix1[x,y] <- 1
  }
}

musicmatrix2 <- matrix(0, nrow = 84, ncol = length(genres))
colnames(musicmatrix2) <- genres

for (i in 1:nrow(music_2)){
  x <- music_2$user.id[i]
  y <- music_2$genre[i]
  if (music_2$score[i] == "3 High Interest"){
    musicmatrix2[x,y] <- 1
  } else if ( music_2$score[i] == "2 Moderate Interest"){
    musicmatrix2[x,y] <- 1
  } 
}


musicmatrix3 <- matrix(0, nrow = 84, ncol = length(genres))
colnames(musicmatrix3) <- genres

for (i in 1:nrow(music_3)){
  x <- music_3$user.id[i]
  y <- music_3$genre[i]
  if (music_3$score[i] == "3 High Interest"){
    musicmatrix3[x,y] <- 1
  } else if ( music_3$score[i] == "2 Moderate Interest"){
    musicmatrix3[x,y] <- 1
  }
}


musicmatrix4 <- matrix(0, nrow = 84, ncol = length(genres))
colnames(musicmatrix4) <- genres

for (i in 1:nrow(music_4)){
  x <- music_4$user.id[i]
  y <- music_4$genre[i]
  if (music_4$score[i] == "3 High Interest"){
    musicmatrix4[x,y] <- 1
  } else if ( music_4$score[i] == "2 Moderate Interest"){
    musicmatrix4[x,y] <- 1
  } 
}



#final music matrixes#

fmm1 <- matrix(0, nrow = 84, ncol = 84)
colnames(fmm1) <- c(1:84)

for (i in 1:ncol(musicmatrix1)){
  id_lst <- c()
  for (k in 1:84){
    if (musicmatrix1[k,i] == 1){
      id_lst <- c(id_lst, k)
    } 
  }
  fmm1[id_lst, id_lst] <- fmm1[id_lst, id_lst] + 1 
}

fmm2 <- matrix(0, nrow = 84, ncol = 84)
colnames(fmm2) <- c(1:84)

for (i in 1:ncol(musicmatrix2)){
  id_lst <- c()
  for (k in 1:84){
    if (musicmatrix2[k,i] == 1){
      id_lst <- c(id_lst, k)
    } 
  }
  fmm2[id_lst, id_lst] <- fmm2[id_lst, id_lst] + 1 
}

fmm3 <- matrix(0, nrow = 84, ncol = 84)
colnames(fmm3) <- c(1:84)
rownames(fmm3) <- c(1:84)

for (i in 1:ncol(musicmatrix3)){
  id_lst <- c()
  for (k in 1:84){
    if (musicmatrix3[k,i] == 1){
      id_lst <- c(id_lst, k)
    } 
  }
  fmm3[id_lst, id_lst] <- fmm3[id_lst, id_lst] + 1 
}

fmm4 <- matrix(0, nrow = 84, ncol = 84)
colnames(fmm4) <- c(1:84)

for (i in 1:ncol(musicmatrix4)){
  id_lst <- c()
  for (k in 1:84){
    if (musicmatrix4[k,i] == 1){
      id_lst <- c(id_lst, k)
    } 
  }
  fmm4[id_lst, id_lst] <- fmm4[id_lst, id_lst] + 1 
}

for (i in 1:84){
  fmm1[i,i] <- 0
  fmm2[i,i] <- 0
  fmm3[i,i] <- 0
  fmm4[i,i] <- 0
}





########politics##############
politics <- read.csv("Politics.csv") 
politics <- politics[,c(1,2,3,8)]

#creating DF's#
pol_2008_09 <- politics[politics$survey.month == 2008.09, ]
pol_2008_10 <- politics[politics$survey.month == 2008.10, ]
pol_2008_11 <- politics[politics$survey.month == 2008.11, ]
pol_2008_12 <- politics[politics$survey.month == 2008.12, ]
pol_2009_03 <- politics[politics$survey.month == 2009.03, ]
pol_2009_04 <- politics[politics$survey.month == 2009.04, ]
pol_2009_06 <- politics[politics$survey.month == 2009.06, ]


subjects <- read.csv("Subjects.csv")
######finetuning######


for (i in 1:nrow(subjects)){
  if(subjects$year_school[i] == "GRT / Other"){
    subjects$year_school[i] <- 5
  } else if(subjects$year_school[i] == "Senior"){
    subjects$year_school[i] <- 4
  } else if(subjects$year_school[i] == "Junior"){
    subjects$year_school[i] <- 3
  } else if(subjects$year_school[i] == "Sophomore"){
    subjects$year_school[i] <- 2
  } else if(subjects$year_school[i] == "Freshman"){
    subjects$year_school[i] <- 1
  }
}

subject_2008_09 <- subjects
subject_2008_12 <- subjects
subject_2009_03 <- subjects
subject_2009_06 <- subjects


#Adding all relevant data to pol df#

lst_2 <- c()
for (i in 1:84){
  if (i %notin% pol_2008_12$user_id){
    if (i %in% pol_2008_11$user_id | i %in% pol_2008_10$user_id | i %in% pol_2008_09$user_id){
      lst_2 <- c(lst_2,i)
    }
  }
}

df_2 <- data.frame()
for (i in lst_2){
  if (i %in% pol_2008_11$user_id){
    df_2 <- rbind(df_2, pol_2008_11[pol_2008_11$user_id == i,])
  } else if (i %in% pol_2008_10$user_id){
    df_2 <- rbind(df_2, pol_2008_10[pol_2008_10$user_id == i,])
  } else {
    df_2 <- rbind(df_2, pol_2008_09[pol_2008_09$user_id == i,])
  }  
}
pol_2008_12 <- rbind(pol_2008_12, df_2)

lst_21 <- c()
for (i in pol_2008_12$user_id) {
  if (pol_2008_12[pol_2008_12$user_id == i, 3] == ""){
    lst_21 <- c(lst_21,i)
  }
}

for (i in lst_21){
  if (i %in% pol_2008_11$user_id){
    pol_2008_12[pol_2008_12$user_id == i, 3] <- pol_2008_11[pol_2008_11$user_id == i, 3]
  } else if (i %in% pol_2008_10$user_id){
    pol_2008_12[pol_2008_12$user_id == i, 3] <- pol_2008_10[pol_2008_10$user_id == i, 3]
  } else {
    pol_2008_12[pol_2008_12$user_id == i, 3] <- pol_2008_09[pol_2008_09$user_id == i, 3]
  }  
}


lst_3 <- c()
for (i in 1:84){
  if (i %notin% pol_2009_03$user_id){
    if (i %in% pol_2008_12$user_id | i %in% pol_2008_11$user_id | i %in% pol_2008_10$user_id | i %in% pol_2008_09$user_id){
      lst_3 <- c(lst_3,i)
    }
  }
}

df_3 <- data.frame()
for (i in lst_3){
  if (i %in% pol_2008_12$user_id){
    df_3 <- rbind(df_3, pol_2008_12[pol_2008_12$user_id == i,])
  } else if (i %in% pol_2008_11$user_id){
    df_3 <- rbind(df_3, pol_2008_11[pol_2008_11$user_id == i,])
  } else if (i %in% pol_2008_10$user_id){
    df_3 <- rbind(df_3, pol_2008_10[pol_2008_10$user_id == i,])
  } else {
    df_3 <- rbind(df_3, pol_2008_09[pol_2008_09$user_id == i,])
  }  
}
pol_2009_03 <- rbind(pol_2009_03, df_3)

lst_22 <- c()
for (i in pol_2009_03$user_id) {
  if (pol_2009_03[pol_2009_03$user_id == i, 3] == ""){
    lst_22 <- c(lst_22,i)
  }
}

for (i in lst_22){
  if (i %in% pol_2008_12$user_id){
    pol_2009_03[pol_2009_03$user_id == i, 3] <- pol_2008_12[pol_2008_12$user_id == i, 3]
  } else if (i %in% pol_2008_11$user_id){
    pol_2009_03[pol_2009_03$user_id == i, 3] <- pol_2008_11[pol_2008_11$user_id == i, 3]
  } else if (i %in% pol_2008_10$user_id){
    pol_2009_03[pol_2009_03$user_id == i, 3] <- pol_2008_10[pol_2008_10$user_id == i, 3]
  } else if (i %in% pol_2008_09$user_id){
    pol_2009_03[pol_2009_03$user_id == i, 3] <- pol_2008_09[pol_2008_09$user_id == i, 3]
  } else {
    pol_2009_03[pol_2009_03$user_id == i, 3] <- toString(i)
  }
}


lst_4 <- c()
for (i in 1:84){
  if (i %notin% pol_2009_06$user_id){
    if (i %in% pol_2009_04$user_id | i %in% pol_2009_03$user_id | i %in% pol_2008_12$user_id | i %in% pol_2008_11$user_id | i %in% pol_2008_10$user_id | i %in% pol_2008_09$user_id){
      lst_4 <- c(lst_4,i)
    }
  }
}

df_4 <- data.frame()
for (i in lst_4){
  if (i %in% pol_2009_04$user_id){
    df_4 <- rbind(df_4, pol_2009_04[pol_2009_04$user_id == i,])
  } else if (i %in% pol_2009_03$user_id){
    df_4 <- rbind(df_4, pol_2009_03[pol_2009_03$user_id == i,])
  } else if (i %in% pol_2008_12$user_id){
    df_4 <- rbind(df_4, pol_2008_12[pol_2008_12$user_id == i,])
  } else if (i %in% pol_2008_11$user_id){
    df_4 <- rbind(df_4, pol_2008_11[pol_2008_11$user_id == i,])
  } else if (i %in% pol_2008_10$user_id){
    df_4 <- rbind(df_4, pol_2008_10[pol_2008_10$user_id == i,])
  } else {
    df_4 <- rbind(df_4, pol_2008_09[pol_2008_09$user_id == i,])
  }  
}
pol_2009_06 <- rbind(pol_2009_06, df_4)

lst_23 <- c()
for (i in pol_2009_06$user_id) {
  if (pol_2009_06[pol_2009_06$user_id == i, 3] == ""){
    lst_23 <- c(lst_23,i)
  }
}

for (i in lst_23){
  if (i %in% pol_2009_03$user_id){
    pol_2009_06[pol_2009_06$user_id == i, 3] <- pol_2009_03[pol_2009_03$user_id == i, 3]
  } else if (i %in% pol_2008_12$user_id){
    pol_2009_06[pol_2009_06$user_id == i, 3] <- pol_2008_12[pol_2008_12$user_id == i, 3]
  } else if (i %in% pol_2008_11$user_id){
    pol_2009_06[pol_2009_06$user_id == i, 3] <- pol_2008_11[pol_2008_11$user_id == i, 3]
  } else if (i %in% pol_2008_10$user_id){
    pol_2009_06[pol_2009_06$user_id == i, 3] <- pol_2008_10[pol_2008_10$user_id == i, 3]
  } else if (i %in% pol_2008_09$user_id){
    pol_2009_06[pol_2009_06$user_id == i, 3] <- pol_2008_09[pol_2008_09$user_id == i, 3]
  } else {
    pol_2009_06[pol_2009_06$user_id == i, 3] <- toString(i)
  } 
}

#creating subject column#

subject_2008_09$interested_pol <- ""
subject_2008_12$interested_pol <- ""
subject_2009_03$interested_pol <- ""
subject_2009_06$interested_pol <- ""
subject_2008_09$pol_party <- ""
subject_2008_12$pol_party <- ""
subject_2009_03$pol_party <- ""
subject_2009_06$pol_party <- ""
subject_2008_09$diet <- ""
subject_2008_12$diet <- ""
subject_2009_03$diet <- ""
subject_2009_06$diet <- ""
subject_2008_09$smoking <- ""
subject_2008_12$smoking <- ""
subject_2009_03$smoking <- ""
subject_2009_06$smoking <- ""
subject_2008_09$aerobic <- ""
subject_2008_12$aerobic <- ""
subject_2009_03$aerobic <- ""
subject_2009_06$aerobic <- ""
subject_2008_09$sport <- ""
subject_2008_12$sport <- ""
subject_2009_03$sport <- ""
subject_2009_06$sport <- ""

#Adding to subject df#

for (i in 1:nrow(pol_2008_09)){
  for (k in 1:nrow(subject_2008_09)){
    if (pol_2008_09$user_id[i] == subject_2008_09$user_id[k]){
      subject_2008_09$interested_pol[k] <- pol_2008_09$interested_in_politics[i]
      subject_2008_09$pol_party[k] <- pol_2008_09$preferred_party[i]
    }
  }
}

for (i in 1:nrow(pol_2008_12)){
  for (k in 1:nrow(subject_2008_12)){
    if (pol_2008_12$user_id[i] == subject_2008_12$user_id[k]){
      subject_2008_12$interested_pol[k] <- pol_2008_12$interested_in_politics[i]
      subject_2008_12$pol_party[k] <- pol_2008_12$preferred_party[i]
    }
  }
}

for (i in 1:nrow(pol_2009_03)){
  for (k in 1:nrow(subject_2009_03)){
    if (pol_2009_03$user_id[i] == subject_2009_03$user_id[k]){
      subject_2009_03$interested_pol[k] <- pol_2009_03$interested_in_politics[i]
      subject_2009_03$pol_party[k] <- pol_2009_03$preferred_party[i]
    }
  }
}

for (i in 1:nrow(pol_2009_06)){
  for (k in 1:nrow(subject_2009_06)){
    if (pol_2009_06$user_id[i] == subject_2009_06$user_id[k]){
      subject_2009_06$interested_pol[k] <- pol_2009_06$interested_in_politics[i]
      subject_2009_06$pol_party[k] <- pol_2009_06$preferred_party[i]
    }
  }
}

#politics##

for (i in 1:nrow(subject_2008_09)){
  if(subject_2008_09$interested_pol[i] == "Not at all interested"){
    subject_2008_09$interested_pol[i] <- 1
  } else if (subject_2008_09$interested_pol[i] == "Slightly interested"){
    subject_2008_09$interested_pol[i] <- 2
  } else if (subject_2008_09$interested_pol[i] == "No Answer"){
    subject_2008_09$interested_pol[i] <- 3
  } else if (subject_2008_09$interested_pol[i] == ""){
    subject_2008_09$interested_pol[i] <- 3
  } else if (subject_2008_09$interested_pol[i] == "Somewhat interested"){
    subject_2008_09$interested_pol[i] <- 4
  } else if (subject_2008_09$interested_pol[i] == "Very interested"){
    subject_2008_09$interested_pol[i] <- 5
  }
  if(subject_2008_12$interested_pol[i] == "Not at all interested"){
    subject_2008_12$interested_pol[i] <- 1
  } else if (subject_2008_12$interested_pol[i] == "Slightly interested"){
    subject_2008_12$interested_pol[i] <- 2
  } else if (subject_2008_12$interested_pol[i] == "No Answer"){
    subject_2008_12$interested_pol[i] <- 3
  } else if (subject_2008_12$interested_pol[i] == ""){
    subject_2008_12$interested_pol[i] <- 3
  } else if (subject_2008_12$interested_pol[i] == "Somewhat interested"){
    subject_2008_12$interested_pol[i] <- 4
  } else if (subject_2008_12$interested_pol[i] == "Very interested"){
    subject_2008_12$interested_pol[i] <- 5
  }
  if(subject_2009_03$interested_pol[i] == "Not at all interested"){
    subject_2009_03$interested_pol[i] <- 1
  } else if (subject_2009_03$interested_pol[i] == "Slightly interested"){
    subject_2009_03$interested_pol[i] <- 2
  } else if (subject_2009_03$interested_pol[i] == "No Answer"){
    subject_2009_03$interested_pol[i] <- 3
  } else if (subject_2009_03$interested_pol[i] == ""){
    subject_2009_03$interested_pol[i] <- 3
  } else if (subject_2009_03$interested_pol[i] == "Somewhat interested"){
    subject_2009_03$interested_pol[i] <- 4
  } else if (subject_2009_03$interested_pol[i] == "Very interested"){
    subject_2009_03$interested_pol[i] <- 5
  }
  if(subject_2009_06$interested_pol[i] == "Not at all interested"){
    subject_2009_06$interested_pol[i] <- 1
  } else if (subject_2009_06$interested_pol[i] == "Slightly interested"){
    subject_2009_06$interested_pol[i] <- 2
  } else if (subject_2009_06$interested_pol[i] == "No Answer"){
    subject_2009_06$interested_pol[i] <- 3
  } else if (subject_2009_06$interested_pol[i] == ""){
    subject_2009_06$interested_pol[i] <- 3
  } else if (subject_2009_06$interested_pol[i] == "Somewhat interested"){
    subject_2009_06$interested_pol[i] <- 4
  } else if (subject_2009_06$interested_pol[i] == "Very interested"){
    subject_2009_06$interested_pol[i] <- 5
  }
}


######HEALTH############

health <- read.csv("Health.csv")
health <- health[,c(1,6,7,8,9,10)]
health <- health[complete.cases(health),]


for(i in 1:nrow(health)){
  if (health$healthy_diet[i] == "Very unhealthy"){
    health$healthy_diet[i] <- 1
  } else if (health$healthy_diet[i] == "Unhealthy"){
    health$healthy_diet[i] <- 2
  } else if (health$healthy_diet[i] == "Below average"){
    health$healthy_diet[i] <- 3
  } else if (health$healthy_diet[i] == "Average"){
    health$healthy_diet[i] <- 4
  } else if (health$healthy_diet[i] == "Healthy"){
    health$healthy_diet[i] <- 5
  } else if (health$healthy_diet[i] == "Very healthy"){
    health$healthy_diet[i] <- 6
  } 
}

for(i in 1:nrow(health)){
  if(health$current_smoking[i] == "Never"){
    health$current_smoking[i] <- 0
  } else if(health$current_smoking[i] == "0"){
    health$current_smoking[i] <- 0
  } else if(health$current_smoking[i] == "2"){
    health$current_smoking[i] <- 2
  } else if(health$current_smoking[i] == "Once in a while"){
    health$current_smoking[i] <- 2
  } else if(health$current_smoking[i] == "Some days"){
    health$current_smoking[i] <- 3
  } else if(health$current_smoking[i] == "5"){
    health$current_smoking[i] <- 4
  }
}

health_2008_09 <- health[health$survey.month == 2008.09, ]
health_2008_10 <- health[health$survey.month == 2008.10, ]
health_2008_12 <- health[health$survey.month == 2008.12, ]
health_2009_03 <- health[health$survey.month == 2009.03, ]
health_2009_04 <- health[health$survey.month == 2009.04, ]
health_2009_06 <- health[health$survey.month == 2009.06, ]



#prepping data and inserting it into subjects
for (i in 1:nrow(health_2008_09)){
  for (k in 1:nrow(subject_2008_09)){
    if (health_2008_09$user_id[i] == subject_2008_09$user_id[k]){
      subject_2008_09$diet[k] <- health_2008_09$healthy_diet[i]
      subject_2008_09$smoking[k] <- health_2008_09$current_smoking[i]
      subject_2008_09$aerobic[k] <- health_2008_09$aerobic_per_week[i]
      subject_2008_09$sport[k] <- health_2008_09$sports_per_week[i]
    }
  }
}


lst_2 <- c()
for (i in 1:84){
  if (i %notin% health_2008_12$user_id){
    if (i %in% health_2008_10$user_id | i %in% health_2008_09$user_id){
      lst_2 <- c(lst_2,i)
    }
  }
}

df_2 <- data.frame()
for (i in lst_2){
  if (i %in% health_2008_10$user_id){
    df_2 <- rbind(df_2, health_2008_10[health_2008_10$user_id == i,])
  } else if (i %in% health_2008_09$user_id){
    df_2 <- rbind(df_2, health_2008_09[health_2008_09$user_id == i,])
  } 
}
health_2008_12 <- rbind(health_2008_12, df_2)


for (i in 1:nrow(health_2008_12)){
  for (k in 1:nrow(subject_2008_12)){
    if (health_2008_12$user_id[i] == subject_2008_12$user_id[k]){
      subject_2008_12$diet[k] <- health_2008_12$healthy_diet[i]
      subject_2008_12$smoking[k] <- health_2008_12$current_smoking[i]
      subject_2008_12$aerobic[k] <- health_2008_12$aerobic_per_week[i]
      subject_2008_12$sport[k] <- health_2008_12$sports_per_week[i]
    }
  }
}

lst_3 <- c()
for (i in 1:84){
  if (i %notin% health_2009_03$user_id){
    if (i %in% health_2008_12$user_id | i %in% health_2008_10$user_id | i %in% health_2008_09$user_id){
      lst_3 <- c(lst_3,i)
    }
  }
}

df_3 <- data.frame()
for (i in lst_3){
  if (i %in% health_2008_12$user_id){
    df_3 <- rbind(df_3, health_2008_12[health_2008_12$user_id == i,])
  } else if (i %in% health_2008_10$user_id){
    df_3 <- rbind(df_3, health_2008_10[health_2008_10$user_id == i,])
  } else if (i %in% health_2008_09$user_id){
    df_3 <- rbind(df_3, health_2008_09[health_2008_09$user_id == i,])
  } 
}
health_2009_03 <- rbind(health_2009_03, df_3)


for (i in 1:nrow(health_2009_03)){
  for (k in 1:nrow(subject_2009_03)){
    if (health_2009_03$user_id[i] == subject_2009_03$user_id[k]){
      subject_2009_03$diet[k] <- health_2009_03$healthy_diet[i]
      subject_2009_03$smoking[k] <- health_2009_03$current_smoking[i]
      subject_2009_03$aerobic[k] <- health_2009_03$aerobic_per_week[i]
      subject_2009_03$sport[k] <- health_2009_03$sports_per_week[i]
    }
  }
}

lst_4 <- c()
for (i in 1:84){
  if (i %notin% health_2009_06$user_id){
    if (i %notin% health_2009_04$user_id | i %notin% health_2009_03$user_id | i %in% health_2008_12$user_id | i %in% health_2008_10$user_id | i %in% health_2008_09$user_id){
      lst_4 <- c(lst_4,i)
    }
  }
}

df_4 <- data.frame()
for (i in lst_4){
  if (i %in% health_2009_04$user_id){
    df_4 <- rbind(df_4, health_2009_04[health_2009_04$user_id == i,])
  } else if (i %in% health_2009_03$user_id){
    df_4 <- rbind(df_4, health_2009_03[health_2009_03$user_id == i,])
  } else if (i %in% health_2008_12$user_id){
    df_4 <- rbind(df_4, health_2008_12[health_2008_12$user_id == i,])
  } else if (i %in% health_2008_10$user_id){
    df_4 <- rbind(df_4, health_2008_10[health_2008_10$user_id == i,])
  } else if (i %in% health_2008_09$user_id){
    df_4 <- rbind(df_4, health_2008_09[health_2008_09$user_id == i,])
  } 
}
health_2009_06 <- rbind(health_2009_06, df_4)

for (i in 1:nrow(health_2009_06)){
  for (k in 1:nrow(subject_2009_06)){
    if (health_2009_06$user_id[i] == subject_2009_06$user_id[k]){
      subject_2009_06$diet[k] <- health_2009_06$healthy_diet[i]
      subject_2009_06$smoking[k] <- health_2009_06$current_smoking[i]
      subject_2009_06$aerobic[k] <- health_2009_06$aerobic_per_week[i]
      subject_2009_06$sport[k] <- health_2009_06$sports_per_week[i]
    }
  }
}

for (i in 1:nrow(subject_2008_09)){
  if(subject_2008_09$diet[i] == ""){
    subject_2008_09$diet[i] <- 4
  } 
  if(subject_2008_12$diet[i] == ""){
    subject_2008_12$diet[i] <- 4
  } 
  if (subject_2009_03$diet[i] == ""){
    subject_2009_03$diet[i] <- 4
  } 
  if (subject_2009_06$diet[i] == ""){
    subject_2009_06$diet[i] <- 4
  }
}


for (i in 1:nrow(subject_2008_09)){
  if(subject_2008_09$smoking[i] == ""){
    subject_2008_09$smoking[i] <- 1
  } 
  if (subject_2008_12$smoking[i] == ""){
    subject_2008_12$smoking[i] <- 1
  } 
  if (subject_2009_03$smoking[i] == ""){
    subject_2009_03$smoking[i] <- 1
  } 
  if (subject_2009_06$smoking[i] == ""){
    subject_2009_06$smoking[i] <- 1
  }
}


#####activities###########

activities <- read.csv("Activities.csv")
activities <- unique(activities)

activities$campus.organization <- as.numeric(factor(activities$campus.organization))

act_2008_09 <- activities[activities$survey.month == 2008.09,]
act_2009_03 <- activities[activities$survey.month == 2009.03,]
act_2009_04 <- activities[activities$survey.month == 2009.04,]
act_2009_06 <- activities[activities$survey.month == 2009.06,]

organisations <- c(1:length(unique(activities$campus.organization)))

act_mat <- matrix(0, nrow = 84, ncol = length(organisations))
colnames(act_mat) <- organisations

for (i in 1:nrow(act_2008_09)){
  x <- act_2008_09$user.id[i]
  y <- act_2008_09$campus.organization[i]
  act_mat[x,y] <- 1
}

act_1 <- matrix(0, nrow = 84, ncol = 84)
colnames(act_1) <- c(1:84)

for (i in 1:ncol(act_mat)){
  id_lst <- c()
  for (k in 1:84){
    if (act_mat[k,i] == 1){
      id_lst <- c(id_lst, k)
    } 
  }
  act_1[id_lst, id_lst] <- act_1[id_lst, id_lst] + 1 
}

lst_3 <- c()
for (i in 1:84){
  if (i %notin% act_2009_03$user.id){
    if (i %in% act_2008_09$user.id & nrow(act_2008_09[act_2008_09$user.id == i,]) > 1){
      lst_3 <- c(lst_3,i)
    }
  }
}
df_3 <- data.frame()
for (i in lst_2){
  df_3 <- rbind(df_3, act_2008_09[act_2008_09$user.id == i,])
}
act_2009_03 <- rbind(act_2009_03, df_3)



organisations <- c(1:length(unique(activities$campus.organization)))

act_mat3 <- matrix(0, nrow = 84, ncol = length(organisations))
colnames(act_mat3) <- organisations

for (i in 1:nrow(act_2009_03)){
  x <- act_2009_03$user.id[i]
  y <- act_2009_03$campus.organization[i]
  act_mat3[x,y] <- 1
}

act_3 <- matrix(0, nrow = 84, ncol = 84)
colnames(act_3) <- c(1:84)
rownames(act_3) <- c(1:84)

for (i in 1:ncol(act_mat3)){
  id_lst <- c()
  for (k in 1:84){
    if (act_mat3[k,i] == 1){
      id_lst <- c(id_lst, k)
    } 
  }
  act_3[id_lst, id_lst] <- act_3[id_lst, id_lst] + 1 
}

#act_3 is 2009.03


lst_4 <- c()
for (i in 1:84){
  if (i %notin% act_2009_06$user.id){
    if (i %in% act_2009_04$user.id & nrow(act_2009_04[act_2009_04$user.id == i,]) > 1){
      lst_4 <- c(lst_4,i)
    }
    else if (i %in% act_2009_03$user.id & nrow(act_2009_03[act_2009_03$user.id == i,]) > 1){
      lst_4 <- c(lst_4,i)
    }
    else if(i %in% act_2008_09$user.id & nrow(act_2008_09[act_2008_09$user.id == i,]) > 1){
      lst_4 <- c(lst_4,i)
    }
  }
}

df_4 <- data.frame()
for (i in lst_4){
  if (i %in% act_2009_04$user.id){
    df_4 <- rbind(df_4, act_2009_04[act_2009_04$user.id == i,])  
  } else if (i %in% act_2009_03$user.id){
    df_4 <- rbind(df_4, act_2009_03[act_2009_03$user.id == i,])  
  } else if (i %in% act_2008_09$user.id){
    df_4 <- rbind(df_4, act_2008_09[act_2008_09$user.id == i,])  
  }  
}
act_2009_06 <- rbind(act_2009_06, df_4)

organisations <- c(1:length(unique(activities$campus.organization)))

act_mat4 <- matrix(0, nrow = 84, ncol = length(organisations))
colnames(act_mat4) <- organisations

for (i in 1:nrow(act_2009_06)){
  x <- act_2009_06$user.id[i]
  y <- act_2009_06$campus.organization[i]
  act_mat4[x,y] <- 1
}

act_4 <- matrix(0, nrow = 84, ncol = 84)
colnames(act_4) <- c(1:84)

for (i in 1:ncol(act_mat4)){
  id_lst <- c()
  for (k in 1:84){
    if (act_mat4[k,i] == 1){
      id_lst <- c(id_lst, k)
    } 
  }
  act_4[id_lst, id_lst] <- act_4[id_lst, id_lst] + 1 
}

act_2 <- act_1

for (i in 1:84){
  act_1[i,i] <- 0
  act_2[i,i] <- 0
  act_3[i,i] <- 0
  act_4[i,i] <- 0
}




rm(act_2008_09)
rm(act_2009_03)
rm(act_2009_06)
rm(act_mat)
rm(act_mat3)
rm(act_mat4)
rm(activities)
rm(health)
rm(health_2008_09)
rm(health_2008_12)
rm(health_2009_03)
rm(health_2009_06)
rm(list)
rm(lst)
rm(music)
rm(music_1)
rm(music_2)
rm(music_3)
rm(music_4)
rm(musicmatrix1)
rm(musicmatrix2)
rm(musicmatrix3)
rm(musicmatrix4)
rm(pol_2008_09)
rm(pol_2008_11)
rm(politics)
rm(relation)
rm(relation_1)
rm(relation_2)
rm(relation_3)
rm(relation_4)
rm(subjects)
rm(genres)
rm(i)
rm(organisations)
rm(k)
rm(id_lst)
rm(no_one)
rm(x)
rm(y)
rm(lst_2)
rm(lst_21)
rm(lst_22)
rm(lst_23)
rm(lst_3)
rm(lst_4)
rm(act_2009_04)
rm(df_2)
rm(df_3)
rm(df_4)
rm(relation_5)
rm(relation_6)
rm(health_2008_10)
rm(health_2009_04)
rm(pol_2008_10)
rm(pol_2008_12)
rm(pol_2009_03)
rm(pol_2009_04)
rm(pol_2009_06)

prox <- read.csv("Proximity.csv")
SMS <- read.csv("SMS.csv")
subjects <- read.csv("Subjects.csv")
calls <- read.csv("Calls.csv")

############data prep######
prox <- subset(prox, prob2 >= 0.9)
prox <- prox[,c(3,1,2)]
prox$type <- rep(0, nrow(prox))
names(prox) <- c("time", "actor1", "actor2", "type")

calls <- calls[,c(2,1,4)]
calls$type <- rep(1,nrow(calls))
names(calls) <- c("time", "actor1", "actor2", "type")

SMS <- SMS[,c(2,1,4)]
SMS$type <- rep(1, nrow(SMS))
names(SMS) <- c("time", "actor1", "actor2", "type")

directed <- rbind(calls, SMS)
all_events <- rbind(prox, directed)

###########data cleaning#####


all_events <- all_events[complete.cases(all_events),]

list_doubles <- c()
for (i in 1:nrow(all_events)) {
  if (all_events$actor1[i] == all_events$actor2[i]) {
    list_doubles <- append(list_doubles, i)
  }
}
all_events <- all_events[-list_doubles, ]


###data prep2###


feb_09<- as.numeric(ymd_hms("2009-02-28 23:59:59"))
apr_09<- as.numeric(ymd_hms("2009-04-01 00:00:00"))
all_events$time <- as.numeric(ymd_hms(all_events$time))
all_events <- unique(all_events)
all_events <- all_events[order(all_events$time),]

all_events <- all_events %>% filter(time > feb_09 & time < apr_09)
all_events$time <- (all_events$time - feb_09)

rownames(all_events) <- 1:nrow(all_events)


####deleting variables####

rm(act_1)
rm(act_2)
rm(act_4)
rm(calls)
rm(directed)
rm(fmm1)
rm(fmm2)
rm(fmm4)
rm(friend_1)
rm(friend_2)
rm(friend_4)
rm(prox)
rm(SMS)
rm(subject_2008_09)
rm(subject_2008_12)
rm(subject_2009_06)
rm(subjects)
rm(apr_09)
rm(feb_09)
rm(list_doubles)

############MODEL###########

attributesObject <- data.frame(id=subject_2009_03$user_id,
                               year = subject_2009_03$year_school,
                               floor = subject_2009_03$floor,
                               diet = subject_2009_03$diet,
                               smoking = subject_2009_03$smoking,
                               aerobic = subject_2009_03$aerobic,
                               sport = subject_2009_03$sport,
                               interested_pol = subject_2009_03$interested_pol,
                               pol_party = subject_2009_03$pol_party)
attributesObject$time <- 0
attributesObject <- attributesObject[,c(1,10,2,3,4,5,6,7,8,9)]
attributesObject$floor <- as.numeric(factor(attributesObject$floor))
attributesObject$year <- as.numeric(factor(attributesObject$year))
attributesObject$diet <- as.numeric(factor(attributesObject$diet))
attributesObject$smoking <- as.numeric(factor(attributesObject$smoking))
attributesObject$aerobic <- as.numeric(factor(attributesObject$aerobic))
attributesObject$sport <- as.numeric(factor(attributesObject$sport))
attributesObject$interested_pol <- as.numeric(factor(attributesObject$interested_pol))  
attributesObject$pol_party <- as.numeric(factor(attributesObject$pol_party))


effects.stats <- ~ 
  (remstats::inertia(consider_type = TRUE)) : (FEtype()) +
  remstats::same("floor") +
  remstats::difference("year") +
  difference("diet")+
  difference("smoking")+
  difference("aerobic")+
  difference("sport")+
  tie(friend_3, "Friends") +
  tie(fmm3, "Music") +
  tie(act_3, "Activities") +
  difference("interested_pol") +
  same("pol_party") 


#all_events <- all_events[1:200,]


fit1 <- fit2 <- fit3 <- fit4 <- fit5 <- fit6 <- list()
evlsList <- statsList <- list()

rehObject <- reh(all_events, actors = attributesObject$id, directed = FALSE, origin = 0)
# Call remstats to compute the statistics
out <- remstats(tie_effects = effects.stats, edgelist = rehObject,
                directed = FALSE, actors = attributesObject$id, attributes = attributesObject)

inertia.type <- out$statistics[,,"inertia.type"]

inertia.real <- inertia.type[,1:(ncol(inertia.type)/2)]
inertia.digital <- inertia.type[,(ncol(inertia.type)/2 + 1):ncol(inertia.type)]

inertia.real <- cbind(inertia.real, inertia.real)
inertia.digital <- cbind(inertia.digital, inertia.digital)

statistics <- out$statistics[,,4:14]
baseline <- out$statistics[,,1]
FEtype <- out$statistics[,, "FEtype"]
stats <- abind(baseline, inertia.real, inertia.digital, FEtype*inertia.real, FEtype*inertia.digital, along=3)

dimnames(stats)[[3]] <- c("baseline", "inertia.real.DV.real", "inertia.digital.DV.real",
                          "inertial.real.DV.digital", "inertia.digital.DV.digital")

stats <- abind(stats, statistics, along=3)
# Extract the relevant objects from the output
#THIS ISNT NECESSARY I BELIEVE statistics <- out$statistics
evls <- out$evls
# Induce a small time difference between the dyads interacting in
# groups

evls[,2] <- cumsum(rehObject$intereventTime)
# Get the parameter estimates for the five models


fit1 <- rem(evls, array(stats[,,1],
                             dim = c(dim(stats[,,1]), 1)),
                 timing = "interval", estimator = "MLE")

fit2 <- rem(evls, stats[,,1:5],
                 timing = "interval", estimator = "MLE")

fit3 <- rem(evls, stats[,,1:7],
                 timing = "interval", estimator = "MLE")

fit4 <- rem(evls, stats[,,1:11],
                 timing = "interval", estimator = "MLE")

fit5 <- rem(evls, stats[,,1:14],
                 timing = "interval", estimator = "MLE")

fit6 <- rem(evls, stats,
                 timing = "interval", estimator = "MLE")

#save
evlsList <- evls
statsList <- stats


save(fit1, fit2, fit3, fit4, fit5, fit6, file = "Type_fit.RData")
save(statsList, evlsList, file = "Type_stats.RData")

fit11 <- fit21 <- fit31 <- fit41 <- fit51 <- fit61 <- list()
evlsList1 <- statsList1 <- list()

####model with windows

windows <- data.frame(
  begin = c(0, seq(from = 259201, to = all_events$time[nrow(all_events)] - 518400, by = 259200)),
  end = c(seq(from = 518400, to = all_events$time[nrow(all_events)]-259200, by = 259200), all_events$time[nrow(all_events)]))

# Find the event indices for when the windows start and stop
windows$start <- apply(windows, 1, function(x) {
  start <- min(which(all_events$time > as.numeric(x[1])))
  ifelse(start == 0, 1, start)
})
windows$stop <- apply(windows, 1, function(x) {
  stop <- min(which(all_events$time > as.numeric(x[2])))-1
  ifelse(stop == Inf, nrow(all_events), stop)
})



for(i in 1:nrow(windows)){
  # Call remstats to compute the statistics
  print(i)
  rehObject <- reh(all_events[windows$start[i]:windows$stop[i],], actors = attributesObject$id, directed = FALSE, origin = windows$start[i])
  print("Reh")
  # Call remstats to compute the statistics
  out <- remstats(tie_effects = effects.stats, edgelist = all_events,
                  directed = FALSE, actors = attributesObject$id, attributes = attributesObject,
                  start = windows$start[i], stop= windows$stop[i])
  print("out")
  
  inertia.type <- out$statistics[,,"inertia.type"]
  
  print("inertia.type")
  
  inertia.real <- inertia.type[,1:(ncol(inertia.type)/2)]
  print("real")
  inertia.digital <- inertia.type[,(ncol(inertia.type)/2 + 1):ncol(inertia.type)]
  print("digital")
  
  inertia.real <- cbind(inertia.real, inertia.real)
  print("real bind")
  inertia.digital <- cbind(inertia.digital, inertia.digital)
  print("digital bind")
  
  statistics <- out$statistics[,,4:14]
  print("statistics")
  baseline <- out$statistics[,,1]
  print("baseline")
  FEtype <- out$statistics[,, "FEtype"]
  print("FEtype")
  stats <- abind(baseline, inertia.real, inertia.digital, FEtype*inertia.real, FEtype*inertia.digital, along=3)
  print("stats")
  
  dimnames(stats)[[3]] <- c("baseline", "inertia.real.DV.real", "inertia.digital.DV.real",
                            "inertial.real.DV.digital", "inertia.digital.DV.digital")
  print("dimnames")
  
  stats <- abind(stats, statistics, along=3)
  print("stats2")
  # Extract the relevant objects from the output
  #THIS ISNT NECESSARY I BELIEVE statistics <- out$statistics
  evls <- out$evls[windows$start[i]:windows$stop[i],]
  print("evls")
  # Induce a small time difference between the dyads interacting in
  # groups
  
  evls[,2] <- cumsum(rehObject$intereventTime)
  print("evls2")
  
  # Get the parameter estimates for the five models
  fit11[[i]] <- rem(evls, array(stats[,,1],
                               dim = c(dim(stats[,,1]), 1)),
                   timing = "interval", estimator = "MLE")
  print("after fit1")
  fit21[[i]] <- rem(evls, stats[,,1:5],
                   timing = "interval", estimator = "MLE")
  print("after fit2")
  fit31[[i]] <- rem(evls, stats[,,1:7],
                   timing = "interval", estimator = "MLE")
  print("after fit3")
  fit41[[i]] <- rem(evls, stats[,,1:11],
                   timing = "interval", estimator = "MLE")
  print("after fit4")
  fit51[[i]] <- rem(evls, stats[,,1:14],
                   timing = "interval", estimator = "MLE")
  print("after_fit5")
  fit61[[i]] <- rem(evls, stats,
                   timing = "interval", estimator = "MLE")
  print("after fit6")
  #save
  evlsList1[[i]] <- evls
  statsList1[[i]] <- stats
}


save(fit11, fit21, fit31, fit41, fit51, fit61, file = "Type_fit_windows.RData")
save(statsList1, evlsList1, file = "Type_stats_windows.RData")


cppFunction("
	
	arma::mat compute_lambda(arma::cube statistics, arma::colvec coef) {
	
		arma::mat lambda(statistics.n_rows, statistics.n_cols, arma::fill::zeros);
		
		for(arma::uword i = 0; i < coef.n_elem; ++i) {
			arma::mat statsslice = statistics.slice(i);
			arma::mat lambdaslice = coef(i)*statsslice;
			lambda += lambdaslice;
		}
		
		lambda = exp(lambda);
		return lambda;
	
	}", depends = "RcppArmadillo")


# Function to compute gof
compute_gof <- function(fit, statistics, evls, interactions) {
  # Stap 1: Compute lambda
  lambda <- compute_lambda(statistics, fit$coef)
  
  # Stap 2: Compute event probabilities
  eventprob <- t(apply(lambda, 1, function(x) {
    x/sum(x)
  }))
  
  # Stap 3: Inverse event ranks
  eventranks <- t(apply(eventprob, 1, function(x) {
    rank(x, ties.method = "max")/length(x)
  }))
  
  # Stap 4: Gof
  gof <- vector()
  for(j in 1:length(unique(interactions))) {
    top5 <- which(eventranks[which(interactions == unique(interactions)[j])[1],] >= 0.95)
    dyads <- evls[which(interactions == unique(interactions)[j]),1]
    gof[j] <- mean(dyads %in% top5)>0
  }
  
  mean(gof)
}

all_events2 <- all_events

gof1 <- lapply(1, function(x) {
  temp <- all_events2
  temp$interaction_id <- 1:nrow(temp)
  temp <- temp[!duplicated(temp$interaction_id),]
  temp$numofactors <- rep(2, nrow(temp))
  cr <- sapply(temp$numofactors, function(x) {
    1-(0.95^choose(x, 2))
  })
  mean(cr)
}) 

all_events2$interaction_id <- 1:nrow(all_events2)

gof2 <- compute_gof(fit2, statsList[,,1:5], evlsList, all_events2$interaction_id)
gof3 <- compute_gof(fit3, statsList[,,1:7], evlsList, all_events2$interaction_id)
gof4 <- compute_gof(fit4, statsList[,,1:11], evlsList, all_events2$interaction_id)
gof5 <- compute_gof(fit5, statsList[,,1:14], evlsList, all_events2$interaction_id)
gof6 <- compute_gof(fit6, statsList, evlsList, all_events2$interaction_id)


save(gof1, gof2, gof3, gof4, gof5, gof6, file = "Type_gof.Rdata")

# Compute gof

gof11 <- lapply(1:nrow(windows), function(x) {
  temp <- all_events[windows$start[x]:windows$stop[x],]
  temp$interaction_id <- 1:nrow(temp)
  temp <- temp[!duplicated(temp$interaction_id),]
  temp$numofactors <- rep(2, nrow(temp))
  cr <- sapply(temp$numofactors, function(x) {
    1-(0.95^choose(x, 2))
  })
  mean(cr)
}) 
all_events$interaction_id <- 1:nrow(all_events)

gof21 <- lapply(1:length(fit21), function(x) {
  compute_gof(fit21[[x]], statsList1[[x]], evlsList1[[x]], 
              all_events$interaction_id[windows$start[x]:windows$stop[x]])
})

gof31 <- lapply(1:length(fit31), function(x) {
  compute_gof(fit31[[x]], statsList1[[x]], evlsList1[[x]], 
              all_events$interaction_id[windows$start[x]:windows$stop[x]])
})

gof41 <- lapply(1:length(fit41), function(x) {
  compute_gof(fit41[[x]], statsList1[[x]], evlsList1[[x]], 
              all_events$interaction_id[windows$start[x]:windows$stop[x]])
})

gof51 <- lapply(1:length(fit51), function(x) {
  compute_gof(fit51[[x]], statsList1[[x]], evlsList1[[x]], 
              all_events$interaction_id[windows$start[x]:windows$stop[x]])
})
gof61 <- lapply(1:length(fit61), function(x) {
  compute_gof(fit61[[x]], statsList1[[x]], evlsList1[[x]], 
              all_events$interaction_id[windows$start[x]:windows$stop[x]])
})

save(gof11, gof21, gof31, gof41, gof51, gof61, file = "Type_gof_windows.Rdata")




