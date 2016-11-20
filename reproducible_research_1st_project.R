######################### IMPORTS #########################

library(ggplot2)
library(dplyr)
library(chron)

setwd("/home/rogelio/Desktop/datasciencecoursera/RepData_PeerAssessment1")

wearbl_data <- read.csv("./activity.csv")



###################### TRANSFORM DATA ######################

wearbl_data <- mutate(wearbl_data, date = as.Date(date))



##################### NUMBER OF STEPS #####################

num_o_steps <- aggregate(wearbl_data$steps, 
                         list(wearbl_data$date), 
                         function(x){sum(x, na.rm = TRUE)})

names(num_o_steps) <- c("date", "number_of_steps")

######################## HISTOGRAM #########################
###################### NAS SUPPRESSED ######################
# The following histogram shows the frequency of the daily
# number of steps. Lines in red and blue represent the 
# mean and median respectively

p <- ggplot(num_o_steps, aes(number_of_steps)) + 
        geom_histogram(color = "black", fill = NA, binwidth = 1250) + 
        geom_vline(aes(colour = "median", 
                       xintercept = median(num_o_steps$number_of_steps))) + 
        geom_vline(aes(colour = "mean", 
                       xintercept = mean(num_o_steps$number_of_steps))) + 
        scale_colour_manual("lines", values = c(median = "blue",mean = "red")) +
        labs(x = "number of steps per day", y = "frequency") +
        labs(title = "frequency of number of steps per day\n(NAs were suppressed)")

print(p)


########################## TABLE ##########################
##################### MEAN AND MEDIAN #####################
# table to display mean and median
tab <- data.frame(mean = mean(num_o_steps$number_of_steps), 
                  median = median(num_o_steps$number_of_steps))
        

################### AVERAGE IN INTERVAL ###################
# collapse by 5 minute lapse

av_in_interval <- aggregate(wearbl_data$steps, list(wearbl_data$interval), 
                         function(x){mean(x, na.rm = TRUE)})

names(av_in_interval) <- c("interval", "average_activity")
####################### TIME SERIES #######################
################### AVERAGE IN INTERVAL ###################
ggplot(av_in_interval, aes(av_in_interval$interval, 
                           av_in_interval$average_activity)) +
        geom_line() + 
        labs(x = "interval [minutes]", y = "average activity [steps]") +
        labs(title = "average activity through all dates\n(NAs were suppressed)")

######################## MAX VALUE ########################
####################### IN INTERVAL #######################
a <- which(av_in_interval$average_activity == max(av_in_interval$average_activity))
av_in_interval$interval[a]

 
####################### REPLACE NAS #######################
#################### WITH DAY AVERAGES ####################
# a copy of wearbl_data

wearbl_data1 <- wearbl_data

# I'll obtain the mean for each date, replicate each a number
# of times equal to the number of intervals in each date and
# replace wherever there's an NA in wearbl_data$steps
 
means_by_date <- aggregate(wearbl_data1$steps, list(wearbl_data1$date), 
           function(x){mean(x, na.rm = TRUE)})$x

# It seems that there are dates for which all intervals have
# an NA value and hence taking the mean with na.rm is not 
# enough to eliminate them. The following chunk will take 
# care of that...

means_by_date[is.nan(means_by_date)] <- 0

# replicate each mean a number of times equal to the number 
# of intervals in one day

means_by_date <- rep(means_by_date, each = 288)

# replace NAs

# a logical vector that contains the positions of NAs
# in wearbl_data$steps
pos_nas <- is.na(wearbl_data1$steps)

wearbl_data1$steps[pos_nas] <- means_by_date[pos_nas]

######################## HISTOGRAM #########################
####################### NAS REPLACED #######################

num_o_steps1 <- aggregate(wearbl_data1$steps, list(wearbl_data1$date), 
                         function(x){sum(x, na.rm = TRUE)})

p <- ggplot(num_o_steps1, aes(x)) + 
        geom_histogram(color = "black", fill = NA, binwidth = 1250) + 
        geom_vline(aes(colour = "median", xintercept = median(num_o_steps1$x))) + 
        geom_vline(aes(colour = "mean", xintercept = mean(num_o_steps1$x))) + 
        scale_colour_manual("lines", values = c(median = "blue",mean = "red")) +
        labs(x = "number of steps per day", y = "frequency") +
        labs(title = "average activity through all dates\n(NAs were replaced)")

print(p)

# The histogram doesn't show any changes with respect to 
# the histogram that excluded NAs altogether.
# It seems that the shift introduced by replacing NAs
# in the number of steps per day 
# doesn't affect the bins in which each measure falls into.
# Plus, there are no differences in the mean and median

tab1 <- data.frame(method = c("NAs replaced", "NAs suppressed"), 
                   mean = c(mean(num_o_steps1$x), 
                            mean(num_o_steps$x)), 
                  median = c(median(num_o_steps1$x), 
                             median(num_o_steps$x)))



################## ADD WEEKDAY OR WEEKEND ##################
# Now, an extra variable (factor) will be added to 
# wearbl_data. It will indicate whether the date
# corresponds to a weekday or not.
# It will control for the differences in the number
# of steps in weekdays and weekends

wkday <- wearbl_data1$date

is_it_weekday <- vector(length = length(wkday))

is_it_weekday[is.weekend(wkday)] <- "weekend"
is_it_weekday[!is.weekend(wkday)] <- "weekday"

wearbl_data1 <- cbind(wearbl_data1, is_it_weekday)


####################### TIME SERIES #######################
################### WEEKENDS & WEEKDAYS ###################
av_in_interval1 <- aggregate(steps ~ interval + is_it_weekday,
                            data = wearbl_data1,
                            function(x){mean(x, na.rm = TRUE)})

ggplot(av_in_interval1, aes(interval, steps)) +
        geom_line() + 
        facet_wrap(~is_it_weekday, ncol = 1)

a <- which(av_in_interval$x == max(av_in_interval$x))
av_in_interval$Group.1[a]