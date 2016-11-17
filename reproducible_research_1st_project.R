# Imports

library(ggplot2)
library(dplyr)

setwd("/home/rogelio/Desktop/datasciencecoursera/RepData_PeerAssessment1")

wearbl_data <- read.csv("./activity.csv")



# Transform data

# wearbl_data <- mutate(wearbl_data, date = as.Date(date))



# number of steps taken each day

num_o_steps <- aggregate(wearbl_data$steps, list(wearbl_data$date), 
                         function(x){sum(x, na.rm = TRUE)})

png("./hist.png")
p <- ggplot(num_o_steps, aes(x)) + 
        geom_histogram(color = "black", fill = NA, binwidth = 1250) + 
        geom_vline(aes(colour = "median", xintercept = median(num_o_steps$x))) + 
        geom_vline(aes(colour = "mean", xintercept = mean(num_o_steps$x))) + 
        scale_colour_manual("lines", values = c(median = "blue",mean = "red"))

print(p)
dev.off()
        

