# last update: 2019/05/23
# purpose: processing MiniSim and D-lab data
# author: Joonbum Lee





# 1. load----

# library
library(tidyverse)
library(readtext)


# custom functions
# source("~/Box/JB Work/R/Function/multiplot.R")
source("/Users/leej1/Box/JB Work/R/Function/multiplot.R") # for multiplot


# data

# mac
# data.condition<-read.csv("~/Box/_ATM/atmExp1/part108/file.csv")
# data.frame<-read.csv("~/Box/_ATM/atmExp1/part108/file2.csv")
# data.gantry<-read.csv("~/Box/_ATM/atmExp1/part108/file3.csv")
# 
# data.eye<-read.csv("~/Box/_NCHRP 03-124 - Guidance for ATM/Technical/Sample Data/ATM Experiement 1 Part 2/108/108_2. Recording 4112019 121225 PM_CsvData.csv")
# data.aoi<-read.csv("~/Box/_NCHRP 03-124 - Guidance for ATM/Technical/Sample Data/ATM Experiement 1 Part 2/108/108_2. Recording 4112019 121225 PM_Glance Intervals On Aois.csv")

# window
# data.condition<-read.csv("/Users/LeeJ1/Box/_ATM/atmExp1/part108/file.csv") # experimental condition
# data.frame<-read.csv("/Users/LeeJ1/Box/_ATM/atmExp1/part108/file2.csv") # frame
# data.gantry<-read.csv("/Users/LeeJ1/Box/_ATM/atmExp1/part108/file3.csv") # gantry positions
# data.speed<-read.csv("/Users/leej1/Box/_ATM/atmExp1/part108/speed.csv") # speed data
# data.lane<-read.csv("/Users/leej1/Box/_ATM/atmExp1/part108/lane.csv") # lane position

# this data set has been pre-processed by Matlab
data.new<-read.csv("/Users/leej1/Box/_NCHRP 03-124 - Guidance for ATM/Technical/Task 7/Data analysis/Data/s129.csv")

# this data was manually created
data.gantry.info<-read.csv("/Users/leej1/Box/_NCHRP 03-124 - Guidance for ATM/Technical/Task 7/Data analysis/Data/atm_gantry_information.csv")

# from D-Lab
# data.aoi<-read.table("/Users/leej1/Box/_NCHRP 03-124 - Guidance for ATM/Technical/Task 7/Data analysis/Data/108_2. Recording 4112019 121225 PM_CsvData.txt",sep="\t",header=TRUE)
data.aoi<-read.table("/Users/leej1/Box/_NCHRP 03-124 - Guidance for ATM/Technical/Task 7/Data analysis/Data/129_1. Recording 522019 24857 PM_CsvData.txt",sep="\t",header=TRUE)






# 2. clean----

# rename variables
names(data.aoi)[1]<-"time"
names(data.aoi)[2]<-"UTC"
names(data.aoi)[3]<-"frame"
names(data.aoi)[4]<-"fixation_road"
names(data.aoi)[5]<-"fixation_phone"


# rename variablebs
names(data.new)[1]<-"frame"
names(data.new)[2]<-"condition"
names(data.new)[3]<-"gantry"
names(data.new)[4]<-"speed"
names(data.new)[5]<-"lane"

# add subjectid (note: this will be deleted once we combine all subs data)
data.new$subjectid<-129

# add condition names
data.new$condition_name[data.new$condition==1]<-"Baseline 1 (smartphone only)"
data.new$condition_name[data.new$condition==2]<-"Baseline 2 (gantry only)"
data.new$condition_name[data.new$condition==3]<-"Just-in-time"
data.new$condition_name[data.new$condition==4]<-"Always-on"
data.new$condition_name[data.new$condition==5]<-"Extra"



# combo 2: 1-4-2-3-5

# delete unneccesarry portion (before the condition 1 or 2, and after the last condition--right before condition 5)
point.start<-min(which(data.new$condition==1),which(data.new$condition==2)) # beginning of the first condition (=1 or 2).
point.end<-min(which(data.new$condition==5))-1 # end of the first condition (=5)

# trim out unnecessary data
data.new<-data.new[point.start:point.end,]

# condition variable in this data set was misued by representing number of lanes when there is no condition defined.
# therefore, I had to distinguish real condition values from noise.

# flag gantry
data.new$condition_next<-100 # random value
data.new$condition_next[1:length(data.new$condition)-1]<-data.new$condition[2:length(data.new$condition)]
data.new$flag_condition[data.new$condition!=data.new$condition_next]<-1

# this provides points of condition change
point.change<-subset(data.new,flag_condition==1)


# create a block variable: this variable will assign unique numbers for each block
block<-1 # initial value

for (i in 1:nrow(data.new))
{
  if (is.na(data.new$flag_condition[i]))
  {
    data.new$block[i]<-block
  }
  else 
  {
    data.new$block[i]<-block
    block = block + 1
  }
}

# based on number of unique gantry values, I can filter noise
real_block<-data.new%>%group_by(block)%>%summarise(num_gantry=length(unique(gantry)))%>%filter(num_gantry>=4)

# data.new<-subset(data.new, gantry%in%c(11,12,13,14,21,22,23,24,31,32,33,34,41,42,43,44,51,52,53)) 
data.new<-subset(data.new, block%in%unique(real_block$block))
data.new<-inner_join(data.new,data.gantry.info,by="gantry")
# 
# 
# p1<-ggplot(data.new,aes(x=frame,y=condition))+geom_point()
# p2<-ggplot(data.new,aes(x=frame,y=block))+geom_point()
# p3<-ggplot(data.new,aes(x=frame,y=gantry))+geom_point()
# p4<-ggplot(data.new,aes(x=frame,y=factor(lane)))+geom_point()+scale_y_discrete(limits=c("0","1","2","3"))
# multiplot(p1,p2,p3,p4,cols=1)
# 
# ggplot(data.new, aes(x=factor(condition),y=speed))+stat_summary(fun.data="mean_cl_boot")
# ggplot(data.new,aes(x=frame,y=lane))+geom_point()
# unique(data.new$gantry)
# 
# 
# gantry.position<-data.new%>%group_by(gantry)%>%summarise(start.point=min(frame))

ggplot(data.new,aes(x=frame,y=factor(0)))+geom_point(aes(color=lane0),size=5,alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(1),color=lane1),size=5,alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(2),color=lane2),size=5,alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(3),color=lane3),size=5,alpha=I(.5), shape=0)+
  geom_line(data=data.new,aes(x=frame,y=factor(lane),group=subjectid),alpha=I(.5),size=2)+
  scale_colour_manual(values = c("red", "orange", "forestgreen"))+
  ylab("Lane number \n(0=right most lane, 3=left mist lane)")+xlab("Frame")+theme(legend.position = "none")

ggplot(data.new,aes(x=frame,y=factor(0)))+geom_line(aes(color=lane0,group=subjectid),alpha=I(.5),size=10)+
  geom_line(data=data.new,aes(x=frame,y=factor(1),color=lane1,group=subjectid),alpha=I(.5),size=10)+
  geom_line(data=data.new,aes(x=frame,y=factor(2),color=lane2,group=subjectid),alpha=I(.5),size=10)+
  geom_line(data=data.new,aes(x=frame,y=factor(3),color=lane3,group=subjectid),alpha=I(.5),size=10)+
  geom_line(data=data.new,aes(x=frame,y=factor(lane),group=subjectid),alpha=I(.5),size=2)+
  scale_colour_manual(values = c("red", "orange", "forestgreen"))+
  # geom_vline(data=gantry.position, aes(xintercept=start.point), color="black", size=1,alpha=I(.3), linetype=2)+
  ylab("Lane number \n(0=right most lane, 3=left mist lane)")+xlab("Frame")+theme(legend.position = "none")+
  facet_wrap(~condition_name,scales="free")

ggplot(data.new,aes(x=frame,y=factor(0)))+geom_point(aes(color=lane0),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(1),color=lane1),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(2),color=lane2),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(3),color=lane3),alpha=I(.5), shape=0)+
  geom_line(data=data.new,aes(x=frame,y=factor(lane),group=subjectid),alpha=I(.5),size=2)+
  scale_colour_manual(values = c("red", "orange", "forestgreen"))+
  # geom_vline(data=gantry.position, aes(xintercept=start.point), color="black", size=1,alpha=I(.3), linetype=2)+
  ylab("Lane number \n(0=right most lane, 3=left mist lane)")+xlab("Frame")+theme(legend.position = "none")+
  facet_wrap(~condition_name,scales="free")

ggplot(data.new,aes(x=frame,y=factor(0)))+geom_point(aes(color=lane0),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(1),color=lane1),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(2),color=lane2),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(3),color=lane3),alpha=I(.5), shape=0)+
  geom_line(data=data.new,aes(x=frame,y=factor(lane),group=subjectid),alpha=I(.5),size=2)+
  scale_colour_manual(values = c("red", "orange", "forestgreen"))+
  geom_vline(data=gantry.position, aes(xintercept=start.point), color="black", size=1,alpha=I(.3), linetype=2)+
  ylab("Lane number \n(0=right most lane, 3=left mist lane)")+xlab("Frame")+theme(legend.position = "none")

ggplot(data.new,aes(x=frame,y=factor(0)))+geom_point(aes(color=lane0),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(1),color=lane1),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(2),color=lane2),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(3),color=lane3),alpha=I(.5), shape=0)+
  geom_line(data=data.new,aes(x=frame,y=factor(lane),group=subjectid),alpha=I(.5),size=2)+
  scale_colour_manual(values = c("red", "orange", "forestgreen"))+
  # geom_vline(data=gantry.position, aes(xintercept=start.point), color="black", size=1,alpha=I(.3), linetype=2)+
  ylab("Lane number \n(0=right most lane, 3=left mist lane)")+xlab("Frame")+theme(legend.position = "none")+
  facet_wrap(~condition_name,scales="free_x",ncol=4)+theme(panel.spacing = unit(-1.25, "lines")) 

ggplot(data.new,aes(x=frame,y=factor(0)))+geom_point(aes(color=lane0),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(1),color=lane1),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(2),color=lane2),alpha=I(.5), shape=0)+
  geom_point(data=data.new,aes(x=frame,y=factor(3),color=lane3),alpha=I(.5), shape=0)+
  geom_line(data=data.new,aes(x=frame,y=factor(lane),group=subjectid),alpha=I(.5),size=2)+
  scale_colour_manual(values = c("red", "orange", "forestgreen"))+
  # geom_vline(data=gantry.position, aes(xintercept=start.point), color="black", size=1,alpha=I(.3), linetype=2)+
  ylab("Lane number \n(0=right most lane, 3=left mist lane)")+xlab("Frame")+theme(legend.position = "none")+
  facet_wrap(~block,scales="free_x",ncol=4)+theme(panel.spacing = unit(-1.25, "lines")) 



# task 1: assign exp condition based on the condition table
condition.table<-data.new%>%group_by(condition_name)%>%summarise(start.point=min(frame),end.point=max(frame))

data.aoi<-subset(data.aoi, !is.na(frame))


data.aoi$condition_name[(data.aoi$frame>condition.table[[1,"start.point"]])&(data.aoi$frame<condition.table[[1,"end.point"]])]<-condition.table[[1,"condition_name"]]
data.aoi$condition_name[(data.aoi$frame>condition.table[[2,"start.point"]])&(data.aoi$frame<condition.table[[2,"end.point"]])]<-condition.table[[2,"condition_name"]]
data.aoi$condition_name[(data.aoi$frame>condition.table[[3,"start.point"]])&(data.aoi$frame<condition.table[[3,"end.point"]])]<-condition.table[[3,"condition_name"]]
data.aoi$condition_name[(data.aoi$frame>condition.table[[4,"start.point"]])&(data.aoi$frame<condition.table[[4,"end.point"]])]<-condition.table[[4,"condition_name"]]


glance.table<-data.aoi%>%group_by(condition_name)%>%summarise(glance.smartphone=sum(fixation_phone)/60)


# task 2: calculate average intervarl between gantries and calculate end point of each experimental condition
# end of code
#
