# Imports

library(ggplot)
library(dplyr)

setwd("/home/rogelio/Desktop/datasciencecoursera/RepData_PeerAssessment1")

wearbl_data <- read.csv("./activity.csv")

# Transform data

wearbl_data <- mutate(wearbl_data, date = as.Date(date))