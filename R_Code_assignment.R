############################# Load data ##################################

    setwd("F:/COURSERA/Reproducible Research/2° week")
    activity<-read.csv("activity.csv")

## Formatting dates ##
    activity$date<-as.Date(as.character(activity$date))
    
########## What is mean total number of steps taken per day? #############
    
    Total_steps<-sum(activity$steps,na.rm=T)
    hist(activity$steps, main = "Histogram of total steps", xlab = "Steps", col = "red")
    mean_step<-mean(activity$steps,na.rm=T)
    median_step<-median(activity$steps,na.rm=T)
    mean_step
    median_step
    
############ What is the average daily activity pattern? #################
    
    library(dplyr)
    media_5_int<-group_by(activity, interval)
    media_5_int_value<-summarize(media_5_int,avg_step=mean(steps,na.rm=T))
    plot(media_5_int_value$interval,media_5_int_value$avg_step,type="l", main="Average daily activity pattern",xlab = "Interval_5_minutes",ylab = "Average steps",col="red",lwd=1.5)
    max_5_int<-arrange(media_5_int_value,desc(avg_step))
    max_int<-max_5_int[1,1]
    max_int<-as.matrix(max_int)
    
############ Imputing missing values #######################
    
    mis_val<-sum(is.na(activity$steps))
    mis_val
    activity_no_NA<-merge(activity,media_5_int_value,by.x = "interval",by.y = "interval",all.x = T)
    activity_no_NA$steps<-as.double(activity_no_NA$steps)
    activity_no_NA<-mutate(activity_no_NA,no_NA=case_when(is.na(steps)==TRUE ~ avg_step,is.na(steps)==FALSE ~ steps))
    activity_no_NA<-select(activity_no_NA,c(interval,no_NA,date))
    hist(activity_no_NA$no_NA, main = "Histogram of total steps", xlab = "Steps", col = "red")
    mean_step_no_NA<-mean(activity_no_NA$no_NA,na.rm=T)
    median_step_no_NA<-median(activity_no_NA$no_NA,na.rm=T)
    Total_steps_no_NA<-sum(activity_no_NA$no_NA,na.rm=T)
    mean_step_no_NA
    median_step_no_NA
    Total_steps_no_NA
    
################# Are there differences in activity patterns between weekdays and weekends? ########
    
    library(ggplot2)
    activity_no_NA$day<-weekdays(activity_no_NA$date)  
    activity_no_NA<-mutate(activity_no_NA, day_type=case_when(day %in% c("lunedì","martedì","mercoledì","giovedì","venerdì") ~ "weekday",day %in% c("sabato","domenica") ~ "weekend"))
    activity_group_inter_day_type<-group_by(activity_no_NA,interval,day_type)
    activity_group_sum_inte_day_type<-summarize(activity_group_inter_day_type,avg_day_type=sum(no_NA))
    
    g<-ggplot(activity_group_sum_inte_day_type,aes(interval,avg_day_type))
    g+geom_line(lwd=1)+facet_grid(day_type ~ .)+labs(ylab("avg_step"))+ggtitle("Average Steps divided by type of day")
    