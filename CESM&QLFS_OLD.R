library(dplyr)
library(plyr)
library(tidyverse)

cesm <- read.csv(file="CESM WITH GROWTH RATES.csv", header=TRUE, sep=",")
qlfs <- read.csv(file="QLFS WITH GROWTH RATES.csv", header=TRUE, sep=",")

cesmProportions <- read.csv(file="CESM WITH PROPORTIONS.csv", header=TRUE, sep=",")
qlfsProportions <- read.csv(file="QLFS WITH PROPORTIONS.csv", header=TRUE, sep=",")

#MERGING AND FILTER CESM IMPORT

cesm <- cesm %>%
  filter(category == "Accounting" | category == "Agriculture" | category == "Architecture" | category == "Civil Engineers" | category == "Computing" | category == "Design" | category == "Education" | category == "Electrical Engineers" | category == "Human Resources" | category == "Insurance/Finance" | category == "Law" | category == "Management/Logistics" | category == "Medical" | category == "Nursing" | category == "Natural Science" | category == "Sales" | category == "Social Work" | category == "Theology")

cesm_and_qlfs <- merge(cesm, qlfs, by="category", all=T)

cesm_and_qlfs_GR <- cesm_and_qlfs %>%
  select(category, CESM.GR.2011, CESM.GR.2012, CESM.GR.2013, CESM.GR.2014, CESM.GR.2015, CESM.GR.2016, CESM.GR.2017, QLFS.GR.2011, QLFS.GR.2012, QLFS.GR.2013, QLFS.GR.2014, QLFS.GR.2015, QLFS.GR.2016, QLFS.GR.2017)

#MERGING AND FILTERING CESM AND QLFS PROPORTIONS IMPORT

cesmProportions <- cesmProportions %>%
  filter(category == "Accounting" | category == "Agriculture" | category == "Architecture" | category == "Civil Engineers" | category == "Computing" | category == "Design" | category == "Education" | category == "Electrical Engineers" | category == "Human Resources" | category == "Insurance/Finance" | category == "Law" | category == "Management/Logistics" | category == "Medical" | category == "Nursing" | category == "Natural Science" | category == "Sales" | category == "Social Work" | category == "Theology")
  

cesm_qlfs_proportions <- merge(cesmProportions, qlfsProportions, by="category", all=T)

cesm_qlfs_proportions <- cesm_qlfs_proportions %>%
  select(-X.x, -X.y)

#CREATING GRAPH VARIABLES FOR GROWTH RATES

cesm_graph <- cesm_and_qlfs_GR %>%
  gather(Year, Freq, CESM.GR.2011, CESM.GR.2012, CESM.GR.2013, CESM.GR.2014, CESM.GR.2015, CESM.GR.2016, CESM.GR.2017) %>%
  select(category, Year, Freq)

qlfs_graph <- cesm_and_qlfs_GR %>%
  gather(Year, Freq, QLFS.GR.2011, QLFS.GR.2012, QLFS.GR.2013, QLFS.GR.2014, QLFS.GR.2015, QLFS.GR.2016, QLFS.GR.2017) %>%
  select(category, Year, Freq)

#CREATING GRAPH VARIABLES FOR PROPORTIONS

cesm_prop_graph <- cesm_qlfs_proportions %>%
  gather(Year, Freq, PropCESM2010, PropCESM2011, PropCESM2012, PropCESM2013, PropCESM2014, PropCESM2015, PropCESM2016, PropCESM2017) %>%
  select(category, Year, Freq)

qlfs_prop_graph <- cesm_qlfs_proportions %>%
  gather(Year, Freq, propQLFS2010, propQLFS2011, propQLFS2012, propQLFS2013, propQLFS2014, propQLFS2015, propQLFS2016, propQLFS2017) %>%
  select(category, Year, Freq)

#GRAPH VARIABLE FOR PROP

all_graph_prop <- cbind(cesm_prop_graph, qlfs_prop_graph)
names(all_graph_prop) <- c("category", "Year", "CESM", "To_Drop1", "To_Drop2", "QLFS")

#Removing one year collumn because of redundcy

all_graph_prop <- all_graph_prop %>%
  select(category, Year, CESM, QLFS)

all_graph_prop$Year <- gsub("PropCESM", "", all_graph_prop$Year)

all_graph_prop <- all_graph_prop %>%
  gather(Type, Proportion, CESM, QLFS)

#GRAPH VARIABLE FOR GROWTH RATES
all_graph <- cbind(cesm_graph, qlfs_graph) 
names(all_graph) <- c("category", "Year", "CESM", "To_Drop1", "To_Drop2", "QLFS")

#Removing one year collumn because of redundcy

all_graph <- all_graph %>%
  select(category, Year, CESM, QLFS)

all_graph$Year <- gsub("CESM.GR.", "", all_graph$Year)

all_graph <- all_graph %>%
  gather(Type, Cumulative, CESM, QLFS)

#GRAPHING GROWTH RATES

ggplot(all_graph, aes(x = as.numeric(Year), y = Cumulative, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_line() +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  facet_wrap(~category)

#GRAPHING POPRTIONS

#GRAPHING 

ggplot(all_graph_prop, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_line() +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  facet_wrap(~category)

