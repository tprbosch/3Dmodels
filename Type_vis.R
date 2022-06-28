getwd()
load("Type_fit_windows.Rdata")
load("Type_fit.Rdata")
load("Type_gof.Rdata")
load("Type_gof_windows.Rdata")

install.packages("plotly.io")
library(plotly)
library(plotly.io)

?write_html
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

calls <- calls[complete.cases(calls),]

list_doubles <- c()
for (i in 1:nrow(calls)) {
  if (calls$actor1[i] == calls$actor2[i]) {
    list_doubles <- append(list_doubles, i)
  }
}
calls <- calls[-list_doubles, ]


feb_09<- as.numeric(ymd_hms("2009-02-28 23:59:59"))
apr_09<- as.numeric(ymd_hms("2009-04-01 00:00:00"))
calls$time <- as.numeric(ymd_hms(calls$time))
calls <- unique(calls)
calls <- calls %>% filter(time > feb_09 & time < apr_09)

SMS <- SMS[complete.cases(SMS),]

list_doubles <- c()
for (i in 1:nrow(SMS)) {
  if (SMS$actor1[i] == SMS$actor2[i]) {
    list_doubles <- append(list_doubles, i)
  }
}
SMS <- SMS[-list_doubles, ]


SMS$time <- as.numeric(ymd_hms(SMS$time))
SMS <- unique(SMS)
SMS <- SMS %>% filter(time > feb_09 & time < apr_09)


prox <- prox[complete.cases(prox),]

list_doubles <- c()
for (i in 1:nrow(prox)) {
  if (prox$actor1[i] == prox$actor2[i]) {
    list_doubles <- append(list_doubles, i)
  }
}
prox <- prox[-list_doubles, ]


prox$time <- as.numeric(ymd_hms(prox$time))
prox <- unique(prox)
prox <- prox %>% filter(time > feb_09 & time < apr_09)




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


val_gof1 <- c()
val_gof2 <- c()
val_gof3 <- c()
val_gof4 <- c()
val_gof5 <- c()
val_gof6 <- c()
for(i in 1:length(gof11)){
  val_gof1 <- c(val_gof1, gof11[[i]]*100)
  val_gof2 <- c(val_gof2, gof21[[i]]*100)
  val_gof3 <- c(val_gof3, gof31[[i]]*100)
  val_gof4 <- c(val_gof4, gof41[[i]]*100)
  val_gof5 <- c(val_gof5, gof51[[i]]*100)
  val_gof6 <- c(val_gof6, gof61[[i]]*100)
}

avg_1 <- round(mean(val_gof1),2)
avg_2 <- round(mean(val_gof2),2)
avg_3 <- round(mean(val_gof3),2)
avg_4 <- round(mean(val_gof4),2)
avg_5 <- round(mean(val_gof5),2)
avg_6 <- round(mean(val_gof6),2)


# Graph cars using blue points overlayed by a line 
plot(val_gof1, type="o", col="gray", ylim =c(0,100), ann=FALSE, xaxt= "n", lty=1)
axis(1, at=1:nrow(windows), labels=c(1:nrow(windows)))

pal <- colorRampPalette(c("blue", "orange"))
x <- pal(9)

lines(val_gof2, type="o", pch=22, lty=2, col="blue")
lines(val_gof3, type="o", pch=22, lty=3, col=x[9])
lines(val_gof4, type="o", pch=22, lty=4, col="green")
lines(val_gof5, type="o", pch=22, lty=5, col="black")
lines(val_gof6, type="o", pch=22, lty=6, col=x[7])

# Create a title with a red, bold/italic font
title(main="Gof of the different event model with windows", col.main="red", font.main=4)

title(xlab="Windows of 6 days", col.lab=rgb(0,0,0))
title(ylab="GOF (%)", col.lab=rgb(0,0,0))

legend("right", legend=c(paste("Model 1,", "Average(",avg_1,"%)"), paste("Model 2,", "Average(",avg_2,"%)"), paste("Model 3,", "Average(",avg_3,"%)"), paste("Model 4,", "Average(",avg_4,"%)"), paste("Model 5,", "Average(",avg_5,"%)"), paste("Model 6,", "Average(",avg_6,"%)")),
       col=c("gray", "blue", x[9], "green", "black", x[7]), lty=c(1,2,3,4,5,6), cex=0.8)



#p values

lst_p_1 <- c()
lst_p_2 <- c()
lst_p_3 <- c()
lst_p_4 <- c()
lst_p_5 <- c()
lst_p_6 <- c()


for (i in 1:nrow(windows)){
  for(k in 1:length(fit11[[i]]$coef)){
    x <- fit11[[i]]$coef[[k]]/fit11[[i]]$se[[k]]
    p_value1 <- 2*(1-pnorm(abs(x)))
    if(p_value1 > 0.05){
      lst_p_1 <- c(lst_p_1, i, names(fit11[[i]]$coef)[k],p_value1)
    }
  }
  for(k in 1:length(fit21[[i]]$coef)){
    x <- fit21[[i]]$coef[[k]]/fit21[[i]]$se[[k]]
    p_value2 <- 2*(1-pnorm(abs(x)))
    if(p_value2 > 0.05){
      lst_p_2 <- c(lst_p_2, i, names(fit21[[i]]$coef)[k],p_value2)
    }
  }
  for(k in 1:length(fit31[[i]]$coef)){
    x <- fit31[[i]]$coef[[k]]/fit31[[i]]$se[[k]]
    p_value3 <- 2*(1-pnorm(abs(x)))
    if(p_value3 > 0.05){
      lst_p_3 <- c(lst_p_3, i, names(fit31[[i]]$coef)[k],p_value3)
    }
  }
  for(k in 1:length(fit41[[i]]$coef)){
    x <- fit41[[i]]$coef[[k]]/fit41[[i]]$se[[k]]
    p_value4 <- 2*(1-pnorm(abs(x)))
    if(p_value4 > 0.05){
      lst_p_4 <- c(lst_p_4, i, names(fit41[[i]]$coef)[k], p_value4)
    }
  }
  for(k in 1:length(fit51[[i]]$coef)){
    x <- fit51[[i]]$coef[[k]]/fit51[[i]]$se[[k]]
    p_value5 <- 2*(1-pnorm(abs(x)))
    if(p_value5 > 0.05){
      lst_p_5 <- c(lst_p_5, i, names(fit51[[i]]$coef)[k], p_value5)
    }
  }
  for(k in 1:length(fit61[[i]]$coef)){
    x <- fit61[[i]]$coef[[k]]/fit61[[i]]$se[[k]]
    p_value6 <- 2*(1-pnorm(abs(x)))
    if(p_value6 > 0.05){
      lst_p_6 <- c(lst_p_6, i, names(fit61[[i]]$coef)[k], p_value6)
    }
  }
}

#fit_1_P <- data.frame(lst_p_1[seq(1, length(lst_p_1), by = 3)], lst_p_1[seq(2, length(lst_p_1), by = 3)], lst_p_1[seq(3, length(lst_p_1), by = 3)])
fit_2_P <- data.frame(lst_p_2[seq(1, length(lst_p_2), by = 3)], lst_p_2[seq(2, length(lst_p_2), by = 3)], lst_p_2[seq(3, length(lst_p_2), by = 3)])
fit_3_P <- data.frame(lst_p_3[seq(1, length(lst_p_3), by = 3)], lst_p_3[seq(2, length(lst_p_3), by = 3)], lst_p_3[seq(3, length(lst_p_3), by = 3)])
fit_4_P <- data.frame(lst_p_4[seq(1, length(lst_p_4), by = 3)], lst_p_4[seq(2, length(lst_p_4), by = 3)], lst_p_4[seq(3, length(lst_p_4), by = 3)])
fit_5_P <- data.frame(lst_p_5[seq(1, length(lst_p_5), by = 3)], lst_p_5[seq(2, length(lst_p_5), by = 3)], lst_p_5[seq(3, length(lst_p_5), by = 3)])
fit_6_P <- data.frame(lst_p_6[seq(1, length(lst_p_6), by = 3)], lst_p_6[seq(2, length(lst_p_6), by = 3)], lst_p_6[seq(3, length(lst_p_6), by = 3)])

#names(fit_1_P) <- c("Window", "Predictor", "P-value")
names(fit_2_P) <- c("Window", "Predictor", "P-value")
names(fit_3_P) <- c("Window", "Predictor", "P-value")
names(fit_4_P) <- c("Window", "Predictor", "P-value")
names(fit_5_P) <- c("Window", "Predictor", "P-value")
names(fit_6_P) <- c("Window", "Predictor", "P-value")



#fit_1_P$Window <- as.numeric(fit_1_P$Window)
fit_2_P$Window <- as.numeric(fit_2_P$Window)
fit_3_P$Window <- as.numeric(fit_3_P$Window)
fit_4_P$Window <- as.numeric(fit_4_P$Window)
fit_5_P$Window <- as.numeric(fit_5_P$Window)
fit_6_P$Window <- as.numeric(fit_6_P$Window)


#count_1_1 <- 0
#count_1_2 <- 0
#count_1_3 <- 0
#count_1_4 <- 0
#count_1_5 <- 0
#count_1_6 <- 0
#count_1_7 <- 0
#count_1_8 <- 0
#count_1_9 <- 0

#for (i in 1:nrow(fit_1_P)){
#  for (k in 1:nrow(windows)){
#    if(fit_1_P$Window[i] == k){
#      if(k == 1){
#        count_1_1 <- count_1_1 +1   
#      }
#      if(k == 2){
#        count_1_2 <- count_1_2 +1   
#      }
#     if(k == 3){
#        count_1_3 <- count_1_3 +1   
#      }
#      if(k == 4){
#        count_1_4 <- count_1_4 +1   
#      }
#      if(k == 5){
#        count_1_5 <- count_1_5 +1   
#      }
#      if(k == 6){
#        count_1_6 <- count_1_6 +1   
#      } 
#      if(k == 7){
#        count_1_7 <- count_1_7 +1   
#      }
#      if(k == 8){
#        count_1_8 <- count_1_8 +1   
#      } 
#      if(k == 9){
#        count_1_9 <- count_1_9 +1   
#      }  
#    }
#  }
#}


count_2_1 <- 0
count_2_2 <- 0
count_2_3 <- 0
count_2_4 <- 0
count_2_5 <- 0
count_2_6 <- 0
count_2_7 <- 0
count_2_8 <- 0
count_2_9 <- 0

for (i in 1:nrow(fit_2_P)){
  for (k in 1:nrow(windows)){
    if(fit_2_P$Window[i] == k){
      if(k == 1){
        count_2_1 <- count_2_1 +1   
      }
      if(k == 2){
        count_2_2 <- count_2_2 +1   
      }
      if(k == 3){
        count_2_3 <- count_2_3 +1   
      }
      if(k == 4){
        count_2_4 <- count_2_4 +1   
      }
      if(k == 5){
        count_2_5 <- count_2_5 +1   
      }
      if(k == 6){
        count_2_6 <- count_2_6 +1   
      } 
      if(k == 7){
        count_2_7 <- count_2_7 +1   
      }
      if(k == 8){
        count_2_8 <- count_2_8 +1   
      } 
      if(k == 9){
        count_2_9 <- count_2_9 +1   
      }  
    }
  }
}

count_3_1 <- 0
count_3_2 <- 0
count_3_3 <- 0
count_3_4 <- 0
count_3_5 <- 0
count_3_6 <- 0
count_3_7 <- 0
count_3_8 <- 0
count_3_9 <- 0


for (i in 1:nrow(fit_3_P)){
  for (k in 1:nrow(windows)){
    if(fit_3_P$Window[i] == k){
      if(k == 1){
        count_3_1 <- count_3_1 +1   
      }
      if(k == 2){
        count_3_2 <- count_3_2 +1   
      }
      if(k == 3){
        count_3_3 <- count_3_3 +1   
      }
      if(k == 4){
        count_3_4 <- count_3_4 +1   
      }
      if(k == 5){
        count_3_5 <- count_3_5 +1   
      }
      if(k == 6){
        count_3_6 <- count_3_6 +1   
      } 
      if(k == 7){
        count_3_7 <- count_3_7 +1   
      }
      if(k == 8){
        count_3_8 <- count_3_8 +1   
      } 
      if(k == 9){
        count_3_9 <- count_3_9 +1   
      }
      
    }
  }
}

count_4_1 <- 0
count_4_2 <- 0
count_4_3 <- 0
count_4_4 <- 0
count_4_5 <- 0
count_4_6 <- 0
count_4_7 <- 0
count_4_8 <- 0
count_4_9 <- 0
count_4_10 <- 0
count_4_11 <- 0
count_4_12 <- 0
count_4_13 <- 0
count_4_14 <- 0
count_4_15 <- 0
count_4_16 <- 0
count_4_17 <- 0

for (i in 1:nrow(fit_4_P)){
  for (k in 1:nrow(windows)){
    if(fit_4_P$Window[i] == k){
      if(k == 1){
        count_4_1 <- count_4_1 +1   
      }
      if(k == 2){
        count_4_2 <- count_4_2 +1   
      }
      if(k == 3){
        count_4_3 <- count_4_3 +1   
      }
      if(k == 4){
        count_4_4 <- count_4_4 +1   
      }
      if(k == 5){
        count_4_5 <- count_4_5 +1   
      }
      if(k == 6){
        count_4_6 <- count_4_6 +1   
      } 
      if(k == 7){
        count_4_7 <- count_4_7 +1   
      }
      if(k == 8){
        count_4_8 <- count_4_8 +1   
      } 
      if(k == 9){
        count_4_9 <- count_4_9 +1   
      } 
      if(k == 10){
        count_4_10 <- count_4_10 +1   
      } 
      if(k == 11){
        count_4_11 <- count_4_11 +1   
      } 
      if(k == 12){
        count_4_12 <- count_4_12 +1   
      } 
      if(k == 13){
        count_4_13 <- count_4_13 +1   
      }
      if(k == 14){
        count_4_14 <- count_4_14 +1   
      } 
      if(k == 15){
        count_4_15 <- count_4_15 +1   
      } 
      if(k == 16){
        count_4_16 <- count_4_16 +1   
      } 
      if(k == 17){
        count_4_17 <- count_4_17 +1   
      } 
    }
  }
}

count_5_1 <- 0
count_5_2 <- 0
count_5_3 <- 0
count_5_4 <- 0
count_5_5 <- 0
count_5_6 <- 0
count_5_7 <- 0
count_5_8 <- 0
count_5_9 <- 0


for (i in 1:nrow(fit_5_P)){
  for (k in 1:nrow(windows)){
    if(fit_5_P$Window[i] == k){
      if(k == 1){
        count_5_1 <- count_5_1 +1   
      }
      if(k == 2){
        count_5_2 <- count_5_2 +1   
      }
      if(k == 3){
        count_5_3 <- count_5_3 +1   
      }
      if(k == 4){
        count_5_4 <- count_5_4 +1   
      }
      if(k == 5){
        count_5_5 <- count_5_5 +1   
      }
      if(k == 6){
        count_5_6 <- count_5_6 +1   
      } 
      if(k == 7){
        count_5_7 <- count_5_7 +1   
      }
      if(k == 8){
        count_5_8 <- count_5_8 +1   
      } 
      if(k == 9){
        count_5_9 <- count_5_9 +1   
      }
      if(k == 10){
        count_5_10 <- count_5_10 +1   
      } 
      if(k == 11){
        count_5_11 <- count_5_11 +1   
      } 
      if(k == 12){
        count_5_12 <- count_5_12 +1   
      } 
      if(k == 13){
        count_5_13 <- count_5_13 +1   
      }
      if(k == 14){
        count_5_14 <- count_5_14 +1   
      } 
      if(k == 15){
        count_5_15 <- count_5_15 +1   
      } 
      if(k == 16){
        count_5_16 <- count_5_16 +1   
      } 
      if(k == 17){
        count_5_17 <- count_5_17 +1   
      } 
    }
  }
}




count_6_1 <- 0
count_6_2 <- 0
count_6_3 <- 0
count_6_4 <- 0
count_6_5 <- 0
count_6_6 <- 0
count_6_7 <- 0
count_6_8 <- 0
count_6_9 <- 0
count_6_10 <- 0
count_6_11 <- 0
count_6_12 <- 0
count_6_13 <- 0
count_6_14 <- 0
count_6_15 <- 0
count_6_16 <- 0
count_6_17 <- 0


for (i in 1:nrow(fit_6_P)){
  for (k in 1:nrow(windows)){
    if(fit_6_P$Window[i] == k){
      if(k == 1){
        count_6_1 <- count_6_1 +1   
      }
      if(k == 2){
        count_6_2 <- count_6_2 +1   
      }
      if(k == 3){
        count_6_3 <- count_6_3 +1   
      }
      if(k == 4){
        count_6_4 <- count_6_4 +1   
      }
      if(k == 5){
        count_6_5 <- count_6_5 +1   
      }
      if(k == 6){
        count_6_6 <- count_6_6 +1   
      } 
      if(k == 7){
        count_6_7 <- count_6_7 +1   
      }
      if(k == 8){
        count_6_8 <- count_6_8 +1   
      } 
      if(k == 9){
        count_6_9 <- count_6_9 +1   
      }  
      if(k == 10){
        count_6_10 <- count_6_10 +1   
      } 
      if(k == 11){
        count_6_11 <- count_6_11 +1   
      } 
      if(k == 12){
        count_6_12 <- count_6_12 +1   
      } 
      if(k == 13){
        count_6_13 <- count_6_13 +1   
      }
      if(k == 14){
        count_6_14 <- count_6_14 +1   
      } 
      if(k == 15){
        count_6_15 <- count_6_15 +1   
      } 
      if(k == 16){
        count_6_16 <- count_6_16 +1   
      } 
      if(k == 17){
        count_6_17 <- count_6_17 +1   
      } 
    }
  }
}


#vec_fit1 <- c(count_1_1, count_1_2, count_1_3, count_1_4, count_1_5, count_1_6, count_1_7, count_1_8, count_1_9)
vec_fit2 <- c(count_2_1, count_2_2, count_2_3, count_2_4, count_2_5, count_2_6, count_2_7, count_2_8, count_2_9)
vec_fit3 <- c(count_3_1, count_3_2, count_3_3, count_3_4, count_3_5, count_3_6, count_3_7, count_3_8, count_3_9)
vec_fit4 <- c(count_4_1, count_4_2, count_4_3, count_4_4, count_4_5, count_4_6, count_4_7, count_4_8, count_4_9)
vec_fit5 <- c(count_5_1, count_5_2, count_5_3, count_5_4, count_5_5, count_5_6, count_5_7, count_5_8, count_5_9)
vec_fit6 <- c(count_6_1, count_6_2, count_6_3, count_6_4, count_6_5, count_6_6, count_6_7, count_6_8, count_6_9)


#pdf("P_value_plot.pdf")
x <- pal(9)

summary(fit6)

plot(vec_fit2, type="o", col="blue", ylim =c(0,17), ann=FALSE, xaxt= "n", lty=1)
axis(1, at=c(1:nrow(windows)), labels=1:nrow(windows))
lines(0:10, rep(5,11), type="l", lty= 3, col="blue")
lines(1:9, vec_fit3, type="o", pch=22, lty=3, col=x[9])
lines(0:10, rep(7,11), type="l", lty= 3, col=x[9])
lines(1:9,vec_fit4, type="o", pch=22, lty=1, col="green")
lines(0:10, rep(11,11), type="l", lty=3, col="green")
lines(1:9,vec_fit5, type="o", pch=22, lty=1, col="black")
lines(0:10, rep(14,11), type="l", lty=3, col="black")
lines(1:9,vec_fit6, type="o", pch=22, lty=1, col=x[7])
lines(0:10, rep(16,11), type="l", lty=3, col=x[7])


title(main="Models that have predictors with p value > 0.05 per window", col.main="red", font.main=4)

title(xlab="Window", col.lab=rgb(0,0,0))
title(ylab="Amount of predictors with p > 0.05", col.lab=rgb(0,0,0))

legend("topright", legend=c("Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
       col=c("blue", x[9], "green", "black", x[7]), lty=1, cex=0.8)

#dev.off()


#seee which predictors are not good

names <- c("Baseline", "Inertia real DV real" , "Inertia digital DV real", "Inertia real DV digital" , "Inertia digital DV digital","Same Floor", "School Year", "Diet", "Smoking", "Aerobic", "Sport", "Friends", "Music", "Activities", "Interested in Politics", "Political party interested")


lst_p_1 <- c()
lst_p_2 <- c()
lst_p_3 <- c()
lst_p_4 <- c()
lst_p_5 <- c()
lst_p_6 <- c()



for (i in 1:nrow(windows)){
  for(k in 1:length(fit11[[i]]$coef)){
    x <- fit11[[i]]$coef[[k]]/fit11[[i]]$se[[k]]
    p_value1 <- 2*(1-pnorm(abs(x)))
    if(p_value1 > 0.05){
      lst_p_1 <- c(lst_p_1, i, names(fit11[[i]]$coef)[k],p_value1)
    }
  }
  for(k in 1:length(fit21[[i]]$coef)){
    x <- fit21[[i]]$coef[[k]]/fit21[[i]]$se[[k]]
    p_value2 <- 2*(1-pnorm(abs(x)))
    if(p_value2 > 0.05){
      lst_p_2 <- c(lst_p_2, i, names(fit21[[i]]$coef)[k],p_value2)
    }
  }
  for(k in 1:length(fit31[[i]]$coef)){
    x <- fit31[[i]]$coef[[k]]/fit31[[i]]$se[[k]]
    p_value3 <- 2*(1-pnorm(abs(x)))
    if(p_value3 > 0.05){
      lst_p_3 <- c(lst_p_3, i, names(fit31[[i]]$coef)[k],p_value3)
    }
  }
  for(k in 1:length(fit41[[i]]$coef)){
    x <- fit41[[i]]$coef[[k]]/fit41[[i]]$se[[k]]
    p_value4 <- 2*(1-pnorm(abs(x)))
    if(p_value4 > 0.05){
      lst_p_4 <- c(lst_p_4, i, names(fit41[[i]]$coef)[k], p_value4)
    }
  }
  for(k in 1:length(fit51[[i]]$coef)){
    x <- fit51[[i]]$coef[[k]]/fit51[[i]]$se[[k]]
    p_value5 <- 2*(1-pnorm(abs(x)))
    if(p_value5 > 0.05){
      lst_p_5 <- c(lst_p_5, i, names(fit51[[i]]$coef)[k], p_value5)
    }
  }
  for(k in 1:length(fit61[[i]]$coef)){
    x <- fit61[[i]]$coef[[k]]/fit61[[i]]$se[[k]]
    p_value6 <- 2*(1-pnorm(abs(x)))
    if(p_value6 > 0.05){
      lst_p_6 <- c(lst_p_6, i, names(fit61[[i]]$coef)[k], p_value6)
    }
  }
}

#fit_1_P <- data.frame(lst_p_1[seq(1, length(lst_p_1), by = 3)], lst_p_1[seq(2, length(lst_p_1), by = 3)], lst_p_1[seq(3, length(lst_p_1), by = 3)])
fit_2_P <- data.frame(lst_p_2[seq(1, length(lst_p_2), by = 3)], lst_p_2[seq(2, length(lst_p_2), by = 3)], lst_p_2[seq(3, length(lst_p_2), by = 3)])
fit_3_P <- data.frame(lst_p_3[seq(1, length(lst_p_3), by = 3)], lst_p_3[seq(2, length(lst_p_3), by = 3)], lst_p_3[seq(3, length(lst_p_3), by = 3)])
fit_4_P <- data.frame(lst_p_4[seq(1, length(lst_p_4), by = 3)], lst_p_4[seq(2, length(lst_p_4), by = 3)], lst_p_4[seq(3, length(lst_p_4), by = 3)])
fit_5_P <- data.frame(lst_p_5[seq(1, length(lst_p_5), by = 3)], lst_p_5[seq(2, length(lst_p_5), by = 3)], lst_p_5[seq(3, length(lst_p_5), by = 3)])
fit_6_P <- data.frame(lst_p_6[seq(1, length(lst_p_6), by = 3)], lst_p_6[seq(2, length(lst_p_6), by = 3)], lst_p_6[seq(3, length(lst_p_6), by = 3)])

#names(fit_1_P) <- c("Window", "Predictor", "P-value")
names(fit_2_P) <- c("Window", "Predictor", "P-value")
names(fit_3_P) <- c("Window", "Predictor", "P-value")
names(fit_4_P) <- c("Window", "Predictor", "P-value")
names(fit_5_P) <- c("Window", "Predictor", "P-value")
names(fit_6_P) <- c("Window", "Predictor", "P-value")


#fit_1_P$Window <- as.numeric(fit_1_P$Window)
fit_2_P$Window <- as.numeric(fit_2_P$Window)
fit_3_P$Window <- as.numeric(fit_3_P$Window)
fit_4_P$Window <- as.numeric(fit_4_P$Window)
fit_5_P$Window <- as.numeric(fit_5_P$Window)
fit_6_P$Window <- as.numeric(fit_6_P$Window)

names <- c(unique(fit_6_P$Predictor), unique(fit_5_P$Predictor), unique(fit_4_P$Predictor))
names <- unique(names)

h2 <- hash()
h3 <- hash()
h4 <- hash()
h5 <- hash()
h6 <- hash()

for(i in 1:nrow(fit_2_P)){
  for(k in 1:length(names))
    h2[[names[k]]] <- sum(str_count(fit_2_P$Predictor, regex(names[k])))
}
for(i in 1:nrow(fit_3_P)){
  for(k in 1:length(names))
    h3[[names[k]]] <- sum(str_count(fit_3_P$Predictor, regex(names[k])))
}
for(i in 1:nrow(fit_4_P)){
  for(k in 1:length(names))
    h4[[names[k]]] <- sum(str_count(fit_4_P$Predictor, regex(names[k])))
}
for(i in 1:nrow(fit_5_P)){
  for(k in 1:length(names))
    h5[[names[k]]] <- sum(str_count(fit_5_P$Predictor, regex(names[k])))
}
for(i in 1:nrow(fit_6_P)){
  for(k in 1:length(names))
    h6[[names[k]]] <- sum(str_count(fit_6_P$Predictor, regex(names[k])))
}




test <- data.frame(1:length(names), 0, 0, 0, 0 )
colnames(test) <- c("Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
rownames(test) <- names
test$`Model 2` <- 0

for (i in 1:length(names)){
  test[names[i], ] <- c(h2[[names[i]]], h3[[names[i]]], h4[[names[i]]], h5[[names[i]]], h6[[names[i]]])
}

vec_1 <- c()
vec_2 <- c()
vec_3 <- c()
vec_4 <- c()
vec_5 <- c()
vec_6 <- c()
vec_7 <- c()
vec_8 <- c()
vec_9 <- c()
vec_10 <- c()

for (i in 1:5){
  vec_1 <- c(vec_1,test[1,i])
  vec_2 <- c(vec_2,test[2,i])
  vec_3 <- c(vec_3,test[3,i])
  vec_4 <- c(vec_4,test[4,i])
  vec_5 <- c(vec_5,test[5,i])
  vec_6 <- c(vec_6,test[6,i])
  vec_7 <- c(vec_7,test[7,i])
  vec_8 <- c(vec_8,test[8,i])
  vec_9 <- c(vec_9,test[9,i])
  vec_10 <- c(vec_10,test[10,i])
}

pal <- colorRampPalette(c("blue", "orange"))
x <- pal(9)
axis_labels <- c("Inertia (Di DV R)", "Floor", "Year", "Diet", "Smoking", "Aerobics", "Sports", "Music", "Activities", "Interested in Politics")
axis_labels <- setNames(axis_labels, 1:10)

barplot(c(vec_3, vec_4, vec_2, vec_6, vec_7, vec_5, vec_10, vec_8, vec_9, vec_1), main="How often each predictor has an insignificant p_value per model", 
        space = rep(c(1,0,0,0,0), 10),
        col=c("blue", x[9], "green", "black", x[7]), ylim = c(0,10),
        beside=TRUE)
abline(h=9, col="gray", lty=2)
legend("right", legend = colnames(test), fill=c("blue", x[9], "green", "black", x[7]))

axis(1, at=c(3.5,9.5,15.5,21.5,27.5,33.5,39.5,45.5,51.5,57.5), labels = axis_labels, las=2, cex.axis=0.6)

######t.tests######

#inertia Di DV R
t.test(vec_3)

#floor
vec_4 <- vec_4[2:5]
t.test(vec_4)

#year
vec_2 <- vec_2[2:5]
t.test(vec_2)

#diet
vec_6 <- vec_6[3:5]
t.test(vec_6)

#smoking
vec_7 <- vec_7[3:5]
t.test(vec_7)

#aerobics
vec_5 <- vec_5[3:5]
t.test(vec_5)

#sports
vec_10 <- vec_10[3:5]
t.test(vec_10)

#music
vec_8 <- vec_8[4:5]
t.test(vec_8)

#activities
vec_9 <- vec_9[4:5]
t.test(vec_9)

#interested in politics
vec_1 <- vec_1[5]
t.test(vec_1)
####3D#####

vector1 <- c()
vector2 <- c()
vector3 <- c()
vector4 <- c()
vector5 <- c()
vector6 <- c()


for (i in 1:nrow(windows)){
  for (k in 1:length(fit61[[1]][[4]])){
    if (is.na(fit11[[i]][[4]][k]) == FALSE){
      vector1 <- c(vector1, fit11[[i]][[4]][k])
    }
    if (is.na(fit21[[i]][[4]][k]) == FALSE){
      vector2 <- c(vector2, fit21[[i]][[4]][k])
    }
    if (is.na(fit31[[i]][[4]][k]) == FALSE){
      vector3 <- c(vector3, fit31[[i]][[4]][k])
    }
    if (is.na(fit41[[i]][[4]][k]) == FALSE){
      vector4 <- c(vector4, fit41[[i]][[4]][k])
    }
    if (is.na(fit51[[i]][[4]][k]) == FALSE){
      vector5 <- c(vector5, fit51[[i]][[4]][k])
    }
    if (is.na(fit61[[i]][[4]][k]) == FALSE){
      vector6 <- c(vector6, fit61[[i]][[4]][k])
    }
  }
}


which_pred_6 <- 1:length(vector6)
which_pred_5 <- 1:length(vector5)
which_pred_4 <- 1:length(vector4)
which_pred_3 <- 1:length(vector3)
which_pred_2 <- 1:length(vector2)
which_pred_1 <- 1:length(vector1)


df_6 <- data.frame(vector6, which_pred_6)
for(i in 1:length(vector6)-1){
  y <- (i%%length(fit61[[1]][[4]]))+1
  df_6$which_pred_6[i+1] <- names[y]
}
df_5 <- data.frame(vector5, which_pred_5)
for(i in 1:length(vector5)-1){
  y <- (i%%length(fit51[[1]][[4]]))+1
  df_5$which_pred_5[i+1] <- names[y]
}
df_4 <- data.frame(vector4, which_pred_4)
for(i in 1:length(vector4)-1){
  y <- (i%%length(fit41[[1]][[4]]))+1
  df_4$which_pred_4[i+1] <- names[y]
}
df_3 <- data.frame(vector3, which_pred_3)
for(i in 1:length(vector3)-1){
  y <- (i%%length(fit31[[1]][[4]]))+1
  df_3$which_pred_3[i+1] <- names[y]
}
df_2 <- data.frame(vector2, which_pred_2)
for(i in 1:length(vector2)-1){
  y <- (i%%length(fit21[[1]][[4]]))+1
  df_2$which_pred_2[i+1] <- names[y]
}
df_1 <- data.frame(vector1, which_pred_1)
for(i in 1:length(vector1)-1){
  y <- (i%%length(fit11[[1]][[4]]))+1
  df_1$which_pred_1[i+1] <- names[y]
}

for(i in 1:nrow(windows)){
  df_6$window[((i-1)*length(fit61[[1]][[4]])+1):(i*length(fit61[[1]][[4]]))] <- i
  df_5$window[((i-1)*length(fit51[[1]][[4]])+1):(i*length(fit51[[1]][[4]]))] <- i
  df_4$window[((i-1)*length(fit41[[1]][[4]])+1):(i*length(fit41[[1]][[4]]))] <- i
  df_3$window[((i-1)*length(fit31[[1]][[4]])+1):(i*length(fit31[[1]][[4]]))] <- i
  df_2$window[((i-1)*length(fit21[[1]][[4]])+1):(i*length(fit21[[1]][[4]]))] <- i
  df_1$window[((i-1)*length(fit11[[1]][[4]])+1):(i*length(fit11[[1]][[4]]))] <- i
}

names(df_6) <- c("Coefficient", "Predictor", "window")
names(df_5) <- c("Coefficient", "Predictor", "window")
names(df_4) <- c("Coefficient", "Predictor", "window")
names(df_3) <- c("Coefficient", "Predictor", "window")
names(df_2) <- c("Coefficient", "Predictor", "window")
names(df_1) <- c("Coefficient", "Predictor", "window")

df_1 <- df_1[order(df_1$Predictor),]
df_2 <- df_2[order(df_2$Predictor),]
df_3 <- df_3[order(df_3$Predictor),]
df_4 <- df_4[order(df_4$Predictor),]
df_5 <- df_5[order(df_5$Predictor),]
df_6 <- df_6[order(df_6$Predictor),]


df_1$model <- "Model 1"
df_2$model <- "Model 2"
df_3$model <- "Model 3"
df_4$model <- "Model 4"
df_5$model <- "Model 5"
df_6$model <- "Model 6"


all_events <- rbind(df_1,df_2,df_3,df_4,df_5,df_6)
all_events <- all_events[order(all_events$Predictor),]


names[2:16]
inter <- all_events[!(all_events$Predictor=="Baseline"),]

fig <- plot_ly(inter, 
               x= ~Predictor, 
               y = ~window, 
               z= ~Coefficient, 
               type="scatter3d", 
               mode="markers", 
               color=~model
)
axx <- list(
  ticketmode = 'array',
  ticktext = names[2:16],
  tickvals = 0:15,
  range = 0:15
)
axy <- list(
  ticketmode = "array",
  ticktext = 1:9,
  nticks = nrow(windows),
  range = 1:nrow(windows)
)
fig <- fig %>% layout(scene = list(xaxis=axx, yaxis=axy))
fig
write_html(fig, file = "test.html", auto_open=TRUE)
?write_html


#####AVERAGE COEFFICIENTS

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)


bas_1 <- 0
bas_2 <- 0
bas_3 <- 0
bas_4 <- 0
bas_5 <- 0
bas_6 <- 0

#model 2
in_RR2 <- 0
in_RR3 <- 0
in_RR4 <- 0
in_RR5 <- 0
in_RR6 <- 0

in_DR2 <- 0
in_DR3 <- 0
in_DR4 <- 0
in_DR5 <- 0
in_DR6 <- 0

in_RD2 <- 0
in_RD3 <- 0
in_RD4 <- 0
in_RD5 <- 0
in_RD6 <- 0

in_DD2 <- 0
in_DD3 <- 0
in_DD4 <- 0
in_DD5 <- 0
in_DD6 <- 0
#model 3
y3 <- 0
y4 <- 0
y5 <- 0
y6 <- 0

f3 <- 0
f4 <- 0
f5 <- 0
f6 <- 0

#model 4 

d4 <- 0
d5 <- 0
d6 <- 0
s4 <- 0
s5 <- 0
s6 <- 0
a4 <- 0
a5 <- 0
a6 <- 0
sp4 <- 0
sp5 <- 0
sp6 <- 0

#model 5

fr5 <- 0
fr6 <- 0

m5 <- 0
m6 <- 0

Ac5 <- 0
Ac6 <- 0


#model 6

ip6 <- 0
pp6 <- 0


for (i in 1:9){
  bas_1 <- bas_1 + fit11[[i]][[7]][[1]]
  bas_2 <- bas_2 + fit21[[i]][[7]][[1]]
  bas_3 <- bas_3 + fit31[[i]][[7]][[1]]
  bas_4 <- bas_4 + fit41[[i]][[7]][[1]]
  bas_5 <- bas_5 + fit51[[i]][[7]][[1]]
  bas_6 <- bas_6 + fit61[[i]][[7]][[1]]
  in_RR2 <- in_RR2 + fit21[[i]][[7]][[2]]
  in_RR3 <- in_RR3 + fit31[[i]][[7]][[2]]
  in_RR4 <- in_RR4 + fit41[[i]][[7]][[2]]
  in_RR5 <- in_RR5 + fit51[[i]][[7]][[2]]
  in_RR6 <- in_RR6 + fit61[[i]][[7]][[2]]
  
  in_DR2 <- in_DR2 + fit21[[i]][[7]][[3]]
  in_DR3 <- in_DR3 + fit31[[i]][[7]][[3]]
  in_DR4 <- in_DR4 + fit41[[i]][[7]][[3]]
  in_DR5 <- in_DR5 + fit51[[i]][[7]][[3]]
  in_DR6 <- in_DR6 + fit61[[i]][[7]][[3]]
  
  in_RD2 <- in_RD2 + fit21[[i]][[7]][[4]]
  in_RD3 <- in_RD3 + fit31[[i]][[7]][[4]]
  in_RD4 <- in_RD4 + fit41[[i]][[7]][[4]]
  in_RD5 <- in_RD5 + fit51[[i]][[7]][[4]]
  in_RD6 <- in_RD6 + fit61[[i]][[7]][[4]]
  
  in_DD2 <- in_DD2 + fit21[[i]][[7]][[5]]
  in_DD3 <- in_DD3 + fit31[[i]][[7]][[5]]
  in_DD4 <- in_DD4 + fit41[[i]][[7]][[5]]
  in_DD5 <- in_DD5 + fit51[[i]][[7]][[5]]
  in_DD6 <- in_DD6 + fit61[[i]][[7]][[5]]
  
  y3 <- y3 + fit31[[i]][[7]][[7]]
  y4 <- y4 + fit41[[i]][[7]][[7]]
  y5 <- y5 + fit51[[i]][[7]][[7]]
  y6 <- y6 + fit61[[i]][[7]][[7]]
  
  f3 <- f3 + fit31[[i]][[7]][[6]]
  f4 <- f4 + fit41[[i]][[7]][[6]]
  f5 <- f5 + fit51[[i]][[7]][[6]]
  f6 <- f6 + fit61[[i]][[7]][[6]]
  
  
  d4 <- d4 + fit41[[i]][[7]][[8]]
  d5 <- d5 + fit51[[i]][[7]][[8]]
  d6 <- d6 + fit61[[i]][[7]][[8]]
  
  s4 <- s4 + fit41[[i]][[7]][[9]]
  s5 <- s5 + fit51[[i]][[7]][[9]]
  s6 <- s6 + fit61[[i]][[7]][[9]]
  
  a4 <- a4 + fit41[[i]][[7]][[10]]
  a5 <- a5 + fit51[[i]][[7]][[10]]
  a6 <- a6 + fit61[[i]][[7]][[10]]
  
  sp4 <- sp4 + fit41[[i]][[7]][[11]]
  sp5 <- sp5 + fit51[[i]][[7]][[11]]
  sp6 <- sp6 + fit61[[i]][[7]][[11]]
  
  fr5 <- fr5 + fit51[[i]][[7]][[12]]
  fr6 <- fr6 + fit61[[i]][[7]][[12]]
  
  m5 <- m5 + fit51[[i]][[7]][[13]]
  m6 <- m6 + fit61[[i]][[7]][[13]]
  
  Ac5 <- Ac5 + fit51[[i]][[7]][[14]]
  Ac6 <- Ac6 + fit61[[i]][[7]][[14]]
  
  ip6 <- ip6 + fit61[[i]][[7]][[15]]
  pp6 <- pp6 + fit61[[i]][[7]][[16]]
  
  
}

bas_1 <- bas_1/i
bas_2 <- bas_2/i
bas_3 <- bas_3/i
bas_4 <- bas_4/i
bas_5 <- bas_5/i
bas_6 <- bas_6/i

in_RR2 <- in_RR2/i
in_RR3 <- in_RR3/i
in_RR4 <- in_RR4/i
in_RR5 <- in_RR5/i
in_RR6 <- in_RR6/i

in_DR2 <- in_DR2/i
in_DR3 <- in_DR3/i
in_DR4 <- in_DR4/i
in_DR5 <- in_DR5/i
in_DR6 <- in_DR6/i

in_RD2 <- in_RD2/i
in_RD3 <- in_RD3/i
in_RD4 <- in_RD4/i
in_RD5 <- in_RD5/i
in_RD6 <- in_RD6/i

in_DD2 <- in_DD2/i
in_DD3 <- in_DD3/i
in_DD4 <- in_DD4/i
in_DD5 <- in_DD5/i
in_DD6 <- in_DD6/i

y3 <- y3/i
y4 <- y4/i
y5 <- y5/i
y6 <- y6/i

f3 <- f3/i
f4 <- f4/i
f5 <- f5/i
f6 <- f6/i


d4 <- d4/i
d5 <- d5/i
d6 <- d6/i

s4 <- s4/i
s5 <- s5/i
s6 <- s6/i

a4 <- a4/i
a5 <- a5/i
a6 <- a6/i

sp4 <- sp4/i
sp5 <- sp5/i
sp6 <- sp6/i

fr5 <- fr5/i
fr6 <- fr6/i
m5 <- m5/i
m6 <- m6/i

Ac5 <- Ac5/i
Ac6 <- Ac6/i

ip6 <- ip6/i
pp6 <- pp6/i



#STANDARD ERROR

names(fit11[[1]])
fit11[[1]][[12]][[1]]

bas_1 <- 0
bas_2 <- 0
bas_3 <- 0
bas_4 <- 0
bas_5 <- 0
bas_6 <- 0

#model 2
in_RR2 <- 0
in_RR3 <- 0
in_RR4 <- 0
in_RR5 <- 0
in_RR6 <- 0

in_DR2 <- 0
in_DR3 <- 0
in_DR4 <- 0
in_DR5 <- 0
in_DR6 <- 0

in_RD2 <- 0
in_RD3 <- 0
in_RD4 <- 0
in_RD5 <- 0
in_RD6 <- 0

in_DD2 <- 0
in_DD3 <- 0
in_DD4 <- 0
in_DD5 <- 0
in_DD6 <- 0
#model 3
y3 <- 0
y4 <- 0
y5 <- 0
y6 <- 0

f3 <- 0
f4 <- 0
f5 <- 0
f6 <- 0

#model 4 

d4 <- 0
d5 <- 0
d6 <- 0
s4 <- 0
s5 <- 0
s6 <- 0
a4 <- 0
a5 <- 0
a6 <- 0
sp4 <- 0
sp5 <- 0
sp6 <- 0

#model 5

fr5 <- 0
fr6 <- 0

m5 <- 0
m6 <- 0

Ac5 <- 0
Ac6 <- 0


#model 6

ip6 <- 0
pp6 <- 0


for (i in 1:9){
  bas_1 <- bas_1 + fit11[[i]][[12]][[1]]
  bas_2 <- bas_2 + fit21[[i]][[12]][[1]]
  bas_3 <- bas_3 + fit31[[i]][[12]][[1]]
  bas_4 <- bas_4 + fit41[[i]][[12]][[1]]
  bas_5 <- bas_5 + fit51[[i]][[12]][[1]]
  bas_6 <- bas_6 + fit61[[i]][[12]][[1]]
  in_RR2 <- in_RR2 + fit21[[i]][[12]][[2]]
  in_RR3 <- in_RR3 + fit31[[i]][[12]][[2]]
  in_RR4 <- in_RR4 + fit41[[i]][[12]][[2]]
  in_RR5 <- in_RR5 + fit51[[i]][[12]][[2]]
  in_RR6 <- in_RR6 + fit61[[i]][[12]][[2]]
  
  in_DR2 <- in_DR2 + fit21[[i]][[12]][[3]]
  in_DR3 <- in_DR3 + fit31[[i]][[12]][[3]]
  in_DR4 <- in_DR4 + fit41[[i]][[12]][[3]]
  in_DR5 <- in_DR5 + fit51[[i]][[12]][[3]]
  in_DR6 <- in_DR6 + fit61[[i]][[12]][[3]]
  
  in_RD2 <- in_RD2 + fit21[[i]][[12]][[4]]
  in_RD3 <- in_RD3 + fit31[[i]][[12]][[4]]
  in_RD4 <- in_RD4 + fit41[[i]][[12]][[4]]
  in_RD5 <- in_RD5 + fit51[[i]][[12]][[4]]
  in_RD6 <- in_RD6 + fit61[[i]][[12]][[4]]
  
  in_DD2 <- in_DD2 + fit21[[i]][[12]][[5]]
  in_DD3 <- in_DD3 + fit31[[i]][[12]][[5]]
  in_DD4 <- in_DD4 + fit41[[i]][[12]][[5]]
  in_DD5 <- in_DD5 + fit51[[i]][[12]][[5]]
  in_DD6 <- in_DD6 + fit61[[i]][[12]][[5]]
  
  y3 <- y3 + fit31[[i]][[12]][[7]]
  y4 <- y4 + fit41[[i]][[12]][[7]]
  y5 <- y5 + fit51[[i]][[12]][[7]]
  y6 <- y6 + fit61[[i]][[12]][[7]]
  
  f3 <- f3 + fit31[[i]][[12]][[6]]
  f4 <- f4 + fit41[[i]][[12]][[6]]
  f5 <- f5 + fit51[[i]][[12]][[6]]
  f6 <- f6 + fit61[[i]][[12]][[6]]
  
  
  d4 <- d4 + fit41[[i]][[12]][[8]]
  d5 <- d5 + fit51[[i]][[12]][[8]]
  d6 <- d6 + fit61[[i]][[12]][[8]]
  
  s4 <- s4 + fit41[[i]][[12]][[9]]
  s5 <- s5 + fit51[[i]][[12]][[9]]
  s6 <- s6 + fit61[[i]][[12]][[9]]
  
  a4 <- a4 + fit41[[i]][[12]][[10]]
  a5 <- a5 + fit51[[i]][[12]][[10]]
  a6 <- a6 + fit61[[i]][[12]][[10]]
  
  sp4 <- sp4 + fit41[[i]][[12]][[11]]
  sp5 <- sp5 + fit51[[i]][[12]][[11]]
  sp6 <- sp6 + fit61[[i]][[12]][[11]]
  
  fr5 <- fr5 + fit51[[i]][[12]][[12]]
  fr6 <- fr6 + fit61[[i]][[12]][[12]]
  
  m5 <- m5 + fit51[[i]][[12]][[13]]
  m6 <- m6 + fit61[[i]][[12]][[13]]
  
  Ac5 <- Ac5 + fit51[[i]][[12]][[14]]
  Ac6 <- Ac6 + fit61[[i]][[12]][[14]]
  
  ip6 <- ip6 + fit61[[i]][[12]][[15]]
  pp6 <- pp6 + fit61[[i]][[12]][[16]]
  
  
}

bas_1 <- bas_1/i
bas_2 <- bas_2/i
bas_3 <- bas_3/i
bas_4 <- bas_4/i
bas_5 <- bas_5/i
bas_6 <- bas_6/i

in_RR2 <- in_RR2/i
in_RR3 <- in_RR3/i
in_RR4 <- in_RR4/i
in_RR5 <- in_RR5/i
in_RR6 <- in_RR6/i

in_DR2 <- in_DR2/i
in_DR3 <- in_DR3/i
in_DR4 <- in_DR4/i
in_DR5 <- in_DR5/i
in_DR6 <- in_DR6/i

in_RD2 <- in_RD2/i
in_RD3 <- in_RD3/i
in_RD4 <- in_RD4/i
in_RD5 <- in_RD5/i
in_RD6 <- in_RD6/i

in_DD2 <- in_DD2/i
in_DD3 <- in_DD3/i
in_DD4 <- in_DD4/i
in_DD5 <- in_DD5/i
in_DD6 <- in_DD6/i

y3 <- y3/i
y4 <- y4/i
y5 <- y5/i
y6 <- y6/i

f3 <- f3/i
f4 <- f4/i
f5 <- f5/i
f6 <- f6/i


d4 <- d4/i
d5 <- d5/i
d6 <- d6/i

s4 <- s4/i
s5 <- s5/i
s6 <- s6/i

a4 <- a4/i
a5 <- a5/i
a6 <- a6/i

sp4 <- sp4/i
sp5 <- sp5/i
sp6 <- sp6/i

fr5 <- fr5/i
fr6 <- fr6/i
m5 <- m5/i
m6 <- m6/i

Ac5 <- Ac5/i
Ac6 <- Ac6/i

ip6 <- ip6/i
pp6 <- pp6/i





for (i in 1:9){
  bas_1 <- bas_1 + fit11[[i]][[18]]
  bas_2 <- bas_2 + fit21[[i]][[18]]
  bas_3 <- bas_3 + fit31[[i]][[18]]
  bas_4 <- bas_4 + fit41[[i]][[18]]
  bas_5 <- bas_5 + fit51[[i]][[18]]
  bas_6 <- bas_6 + fit61[[i]][[18]]
  
}


bas_1 <- bas_1/i
bas_2 <- bas_2/i
bas_3 <- bas_3/i
bas_4 <- bas_4/i
bas_5 <- bas_5/i
bas_6 <- bas_6/i







