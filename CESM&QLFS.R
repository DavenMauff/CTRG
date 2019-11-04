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

accounting <- allData %>% 
  filter(category == "Accounting")

computing <- allData %>% 
  filter(category == "Computing")

education <- allData %>% 
  filter(category == "Education")

human_resources <- allData %>% 
  filter(category == "Human Resources")

insurance_finanace <- allData %>% 
  filter(category == "Insurance/Finance")

law <- allData %>% 
  filter(category == "Law")

management_logistics <- allData %>% 
  filter(category == "Management/Logistics")

medical <- allData %>% 
  filter(category == "Medical")

nursing <- allData %>% 
  filter(category == "Nursing")

social_work <- allData %>% 
  filter(category == "Social Work")

oversupply <- allData %>% 
  filter(category == "Accounting" | category == "Computing" | category == "Law" | category == "Medical")

undersupply <- allData %>%
  filter(category == "Education" | category == "Insurance/Finance" | category == "Nursing")

match <- allData %>%
  filter(category == "Management/Logistics" | category == "Social Work" | category == "Human Resources")

#REMOVING DATASETS
rm("cesmTransform", "qlfsTransform")

#UNDERSUPPLY
ggplot(undersupply, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_line() +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~category) +
  ylim(0,50) +
  ggtitle("UNDERSUPPLY of Skills")

#OVERSUPPLY
ggplot(oversupply, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_line() +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~category) +
  ylim(0,50) +
  ggtitle("OVERSUPPLY of Skills")

#MATCH
ggplot(match, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_line() +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~category) +
  ylim(0,50) +
  ggtitle("EQUILIBRIUM of Skills")

#ACCOUNTING 
ggplot(accounting, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "Accounting") +
  ylab("Percentage") +
  ylim(0,50)

#COMPUTING
ggplot(computing, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "Computing") +
  ylab("Percentage") +
  ylim(0,50)

#EDUCATION
ggplot(education, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "Education") +
  ylab("Percentage") +
  ylim(0,50)

#HUMAN RESOURCES
ggplot(human_resources, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "Human Resources") +
  ylab("Percentage") +
  ylim(0,50)

#INSURANCE/FINANCE
ggplot(insurance_finanace, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "Insurnace/Finance") +
  ylab("Percentage") +
  ylim(0,50)

#LAW
ggplot(law, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "Law") +
  ylab("Percentage") +
  ylim(0,50)

#MANAGEMENT/LOGISTICS
ggplot(management_logistics, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "Management/Logistics") +
  ylab("Percentage") +
  ylim(0,50)

#MEDICAL
ggplot(medical, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "Medical") +
  ylab("Percentage") +
  ylim(0,50)

#NURSING
ggplot(nursing, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "Nursing") +
  ylab("Percentage") +
  ylim(0,50)

#SOCIAL WORK
ggplot(social_work, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "Social Work") +
  ylab("Percentage") +
  ylim(0,50)

#GRAPHING EVERYTHING
ggplot(allData, aes(x = as.numeric(Year), y = Proportion, color = Type)) + 
  geom_point(alpha = 0.5) + 
  geom_point() +
  geom_line(size=2) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  labs(x = "Year") +
  theme_gray() +
  theme(legend.position = "bottom",legend.key.width = unit(1, "cm"), plot.title = element_text(face = "bold")) +
  theme(axis.line.x = element_line(), axis.line.y = element_line(), plot.title = element_text( size = 15, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", hjust = 0.5), axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=12, face="bold")) +
  labs(title = "Skill Mismatch", subtitle = "All Occupations") +
  ylab("Percentage") +
  ylim(0,50) +
  facet_wrap(~category)
