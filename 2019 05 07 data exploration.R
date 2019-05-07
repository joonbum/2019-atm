# 2019/04/17
# Matlab Simdata test

library(tidyverse)
source("~/Box/JB Work/R/Function/multiplot.R")
source("/Users/leej1/Box/JB Work/R/Function/multiplot.R")
# mac
data.condition<-read.csv("~/Box/_ATM/atmExp1/part108/file.csv")
data.frame<-read.csv("~/Box/_ATM/atmExp1/part108/file2.csv")
data.gantry<-read.csv("~/Box/_ATM/atmExp1/part108/file3.csv")

data.eye<-read.csv("~/Box/_NCHRP 03-124 - Guidance for ATM/Technical/Sample Data/ATM Experiement 1 Part 2/108/108_2. Recording 4112019 121225 PM_CsvData.csv")
data.aoi<-read.csv("~/Box/_NCHRP 03-124 - Guidance for ATM/Technical/Sample Data/ATM Experiement 1 Part 2/108/108_2. Recording 4112019 121225 PM_Glance Intervals On Aois.csv")



# window
data.condition<-read.csv("/Users/LeeJ1/Box/_ATM/atmExp1/part108/file.csv")
data.frame<-read.csv("/Users/LeeJ1/Box/_ATM/atmExp1/part108/file2.csv")
data.gantry<-read.csv("/Users/LeeJ1/Box/_ATM/atmExp1/part108/file3.csv")
data.speed<-read.csv("/Users/leej1/Box/_ATM/atmExp1/part108/speed.csv")
data.lane<-read.csv("/Users/leej1/Box/_ATM/atmExp1/part108/lane.csv")

data.atm1<-read.csv("/Users/leej1/Box/_ATM/atmExp1/part108/atm_sign1.csv")
data.atm2<-read.csv("/Users/leej1/Box/_ATM/atmExp1/part108/atm_sign2.csv")
data.atm3<-read.csv("/Users/leej1/Box/_ATM/atmExp1/part108/atm_sign3.csv")
data.atm4<-read.csv("/Users/leej1/Box/_ATM/atmExp1/part108/atm_sign4.csv")


data.eye<-read.csv("/Users/LeeJ1/Box/_NCHRP 03-124 - Guidance for ATM/Technical/Sample Data/ATM Experiement 1 Part 2/108/108_2. Recording 4112019 121225 PM_CsvData.csv")
data.aoi<-read.csv("/Users/LeeJ1/Box/_NCHRP 03-124 - Guidance for ATM/Technical/Sample Data/ATM Experiement 1 Part 2/108/108_2. Recording 4112019 121225 PM_Glance Intervals On Aois.csv")



names(data.condition)[1]<-"condition"
names(data.frame)[1]<-"frame"
names(data.gantry)[1]<-"gantry"
names(data.speed)[1]<-"speed"
names(data.lane)[1]<-"lane"

names(data.atm1)[1]<-"sign1"
names(data.atm2)[1]<-"sign2"
names(data.atm3)[1]<-"sign3"
names(data.atm4)[1]<-"sign4"

data.atm1$lane_sig[data.atm1$sign1==30]<-"close"
data.atm1$lane_sig[data.atm1$sign1==10]<-"open"
data.atm1$lane_sig[data.atm1$sign1==60]<-"merge left"
data.atm1$lane_sig[data.atm1$sign1==50]<-"merge right"


data.new <- cbind(data.condition,data.frame, data.gantry, data.speed, data.lane, data.atm1)


# combo 2: 1-4-2-3-5

# delete unneccesarry portion
point.start<-min(which(data.new$condition==1),which(data.new$condition==2)) # beginning of the first condition (=1 or 2)
point.end<-max(which(data.new$condition==5)) # end of the first condition (=5)

data.new<-data.new[point.start:point.end,]

ggplot(data.new, aes(x=frame,y=condition))+geom_point()
ggplot(data.new, aes(x=frame,y=gantry))+geom_point()
ggplot(data.new, aes(x=frame,y=factor(lane)))+geom_point()


p1<-ggplot(data.new, aes(x=frame,y=condition))+geom_point()
p2<-ggplot(data.new, aes(x=frame,y=gantry))+geom_point()
multiplot(p1,p2,cols=1)


# flag gantry
data.new$condition_next<-100 # random value
data.new$condition_next[1:length(data.new$condition)-1]<-data.new$condition[2:length(data.new$condition)]
data.new$flag_condition[data.new$condition!=data.new$condition_next]<-1

# this provides points of condition change
point.change<-subset(data.new,flag_condition==1)
data.new<-subset(data.new, frame<103010|frame>110550)

p1<-ggplot(data.new, aes(x=frame,y=condition))+geom_point()
p2<-ggplot(data.new, aes(x=frame,y=gantry))+geom_point()
p0<-ggplot(data.new, aes(x=frame,y=factor(lane)))+geom_point()
p00<-ggplot(data.new, aes(x=frame,y=lane_sig))+geom_point()
multiplot(p1,p2,cols=1)
multiplot(p0,p2,cols=1)
multiplot(p0,p3,p2,p00,cols=1)
multiplot(p3,p00,cols=1)

multiplot(p00,p2,cols=1)


ggplot(data.new, aes(x=frame,y=lane_sig,w=exp_condition))+geom_point()+geom_tile(aes(fill=factor(exp_condition)))
ggplot(subset(data.new,!(is.na(exp_condition))), aes(x=frame,y=lane_sig,w=exp_condition))+geom_point()+geom_line(aes(color=factor(exp_condition)),size=3)


data.block<-ddply(data.new,.(condition), summarise,
                  frame_start=min(frame),
                  frame_end=max(frame))

data.block<-data.new%>%group_by(condition)%>%summarise(frame_start=min(frame),frame_end=max(frame))

names(data.new)
data.new$exp_condition[data.new$fram>74996&data.new$frame<83950]<-1
data.new$exp_condition[data.new$fram>92958&data.new$frame<103010]<-2
data.new$exp_condition[data.new$fram>103010&data.new$frame<119480]<-3
data.new$exp_condition[data.new$fram>83950&data.new$frame<92958]<-4
data.new$exp_condition[data.new$fram>119480&data.new$frame<130640]<-5

p3<-ggplot(data.new,aes(x=frame,y=exp_condition))+geom_point()
p4<-ggplot(data.new,aes(x=frame,y=gantry))+geom_point()
multiplot(p3,p4,cols=1)

data.new%>%subset(!is.na(exp_condition))%>%subset(exp_condition!=5)%>%ggplot(aes(x=factor(exp_condition),y=speed))+stat_summary(fun.data="mean_cl_boot")

# eye data
# data.eye<-data.eye[c("rec_time","MiniSim_Frame")]
names(data.eye)[14]<-"frame"

data.eye$Time <- seq(0,455.1333,by=1/60) # add a new time variable

# aoi data
data.aoi$Start_Time<-data.aoi$Start_Time/1000
data.aoi$End_Time<-data.aoi$End_Time/1000
data.aoi$Duration<-data.aoi$Duration/1000


# add condition variable
data.eye$condition[data.eye$frame>=74997&data.eye$frame<=83950]<-1
data.eye$condition[data.eye$frame>=92959&data.eye$frame<=103000]<-2
data.eye$condition[data.eye$frame>=110560&data.eye$frame<=119480]<-3
data.eye$condition[data.eye$frame>=83951&data.eye$frame<=92958]<-4
data.eye$condition[data.eye$frame>=119480&data.eye$frame<=130640]<-5

data.block2<-ddply(data.eye,.(condition), summarise,
                  time_start=min(Time),
                  time_end=max(Time))


# AOI
data.aoi$subjectid<-108
ggplot(data.aoi,aes(y=AOI,x=Start_Time,group=subjectid))+geom_step()+xlab("Time (msec)")

data.aoi$condition[data.aoi$Start_Time>=0&data.aoi$End_Time<=65.73333]<-3
data.aoi$condition[data.aoi$Start_Time>=65.75&data.aoi$End_Time<=312.15]<-5

data.aoi<-subset(data.aoi,!(is.na(condition)))

ggplot(data.aoi,aes(y=AOI,x=Start_Time,group=subjectid))+geom_step(aes(color=factor(condition)))+xlab("Time (msec)")

#
ggplot(data.eye, aes(x=Dikablis.))


ggplot(data.new,aes(x=frame,y=speed))+geom_point()+geom_line()
ggplot(data.new,aes(x=frame,y=condition))+geom_point()+geom_line()
