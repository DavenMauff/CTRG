#IMPORTS
library(tidyverse)
library(dplyr)
library(plyr)
cesm <- read.csv(file="CESM.csv",header=TRUE, sep=",")
qlfs <- read.csv(file="QLFS.csv",header=TRUE, sep=",")

#RENAMING COLUMNS
names(cesm) <- c("X", "category", "CESM.PROPORTION.2010", "CESM.PROPORTION.2011", "CESM.PROPORTION.2012", "CESM.PROPORTION.2013", "CESM.PROPORTION.2014", "CESM.PROPORTION.2015", "CESM.PROPORTION.2016", "CESM.PROPORTION.2017")
names(qlfs) <- c("Y", "category", "QLFS.PROPORTION.2010", "QLFS.PROPORTION.2011", "QLFS.PROPORTION.2012", "QLFS.PROPORTION.2013", "QLFS.PROPORTION.2014", "QLFS.PROPORTION.2015", "QLFS.PROPORTION.2016", "QLFS.PROPORTION.2017")

#MERGING ALL DATA
allData <- merge(cesm, qlfs, by = "category", all = T)

rm("cesm", "qlfs")

#DROPING KEY NUMBER COLUMN
allData <- allData %>%
  select(-X, -Y)

#TRANSFORMING AND CREATING GRAPH VARIABLES
cesmTransform <- allData %>%
  gather(Year, Freq, CESM.PROPORTION.2010, CESM.PROPORTION.2011, CESM.PROPORTION.2012, CESM.PROPORTION.2013, CESM.PROPORTION.2014, CESM.PROPORTION.2015, CESM.PROPORTION.2016, CESM.PROPORTION.2017) %>%
  select(category, Year, Freq)

qlfsTransform <- allData %>%
  gather(Year, Freq, QLFS.PROPORTION.2010, QLFS.PROPORTION.2011, QLFS.PROPORTION.2012, QLFS.PROPORTION.2013, QLFS.PROPORTION.2014, QLFS.PROPORTION.2015, QLFS.PROPORTION.2016, QLFS.PROPORTION.2017) %>%
  select(category, Year, Freq)

#COMBINING TRANSFORMED GRAPHS
allData <- cbind(cesmTransform, qlfsTransform)
names(allData) <- c("category", "Year", "CESM", "To_Drop1", "To_Drop2", "QLFS")

#SELECTING USEFUL COLUMNS
allData <- allData %>%
  select(category, Year, CESM, QLFS)

#REMOVING PREFIX FROM YEAR
allData$Year <- gsub("CESM.PROPORTION.", "", allData$Year)

allData <- allData %>%
  gather(Type, Proportion, CESM, QLFS)

#REMOVING DATASETS
rm("cesmTransform", "qlfsTransform")

#GRAPHING DATASET
ggplot(allData, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_line() +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~category)
