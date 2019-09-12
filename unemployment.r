#EDIT 2: FINDING STATUS COUNTS#################################################################################################################
qlfsDataFOS <- read.csv(file="wf_08_17FOS.csv",header=TRUE, sep=",")
qlfsDataFOS <- qlfsDataFOS %>%
  select(occupation, field, edu, status, year)

qlfsDataFOS$field <- case_when(
  qlfsDataFOS$field == "Finance; economics and accounting" ~"Accounting", qlfsDataFOS$field == "Computer science" | qlfsDataFOS$field == "Information technology and computer" ~"Computing", qlfsDataFOS$field == "Education; training or development" | qlfsDataFOS$field == "Education and development" ~"Education", qlfsDataFOS$field == "Office administration" ~"Human Resources", qlfsDataFOS$field == "Business; commerce and management studies" ~"Insurance/Finance", qlfsDataFOS$field == "Law" ~"Law", qlfsDataFOS$field == "Management" ~"Management/Logistics", qlfsDataFOS$field == "Health care or health sciences" ~"Medical", qlfsDataFOS$field == "Public administration or social services" | qlfsDataFOS$field == "Social sciences or social studies" ~"Social Work")

qlfsDataFOS2 <- qlfsDataFOS %>%
  filter(edu == higherEducation) %>%
  filter(status == "Job Seeker" | status == "Want to work, stopped seeking") %>%
  filter(field == top$occupation)

qlfsDataFOS2 <- qlfsDataFOS2 %>%
  select(field, edu, status)

qlfsDataFOS2 <- data.frame(with(qlfsDataFOS2, table(field, status)))
qlfsDataFOS2 <- qlfsDataFOS2 %>%
  spread(status, Freq)

qlfsDataFOS2 <- qlfsDataFOS2 %>%
  select(field, `Job Seeker`, `Want to work, stopped seeking`)

#FILTERING INTO YEARS

years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
for (year in years) {
  name <- paste("year", year, sep = "")
  assign(name, qlfsDataFOS %>%
           filter(year == 2010))
}

for (year in years) {
  name <- paste("year", year, sep = "")
  assign(name, filter(status == "Job Seeker" | status == "Want to work, stopped seeking"))
}

year2010 <- qlfsDataFOS %>%
  filter(year == 2010)
year2011 <- qlfsDataFOS %>%
  filter(year == 2011)
year2012 <- qlfsDataFOS %>%
  filter(year == 2012)
year2013 <- qlfsData %>%
  filter(year == 2013)
year2014 <- qlfsData %>%
  filter(year == 2014)
year2015 <- qlfsData %>%
  filter(year == 2015)
year2016 <- qlfsData %>%
  filter(year == 2016)
year2017 <- qlfsData %>%
  filter(year == 2017)

#FILTERING BY EMPLOYEED PERSONS
year2010 <- year2010 %>%
  filter(status == "Employee" | status == "Business Owner")
year2011 <- year2011 %>%
  filter(status == "Employee" | status == "Business Owner")
year2012 <- year2012 %>%
  filter(status == "Employee" | status == "Business Owner")
year2013 <- year2013 %>%
  filter(status == "Employee" | status == "Business Owner")
year2014 <- year2014 %>%
  filter(status == "Employee" | status == "Business Owner")
year2015 <- year2015 %>%
  filter(status == "Employee" | status == "Business Owner")
year2016 <- year2016 %>%
  filter(status == "Employee" | status == "Business Owner")
year2017 <- year2017 %>%
  filter(status == "Employee" | status == "Business Owner")

#SUBSETTING YEARS BY TOP 
year2010 <- year2010 %>%
  filter(year2010$occupation == top$occupation)
year2011 <- year2011 %>%
  filter(year2011$occupation == top$occupation)
year2012 <- year2012 %>%
  filter(year2012$occupation == top$occupation)
year2013 <- year2013 %>%
  filter(year2013$occupation == top$occupation)
year2014 <- year2014 %>%
  filter(year2014$occupation == top$occupation)
year2015 <- year2015 %>%
  filter(year2015$occupation == top$occupation)
year2016 <- year2016 %>%
  filter(year2016$occupation == top$occupation)
year2017 <- year2017 %>%
  filter(year2017$occupation == top$occupation)

#COUNTING OCCURANCES PER YEAR 
year2010 <- data.frame(with(year2010, table(occupation, edu)))
year2010 <- year2010 %>%
  spread(edu, Freq)

year2011 <- data.frame(with(year2011, table(occupation, edu)))
year2011 <- year2011 %>%
  spread(edu, Freq)

year2012 <- data.frame(with(year2012, table(occupation, edu)))
year2012 <- year2012 %>%
  spread(edu, Freq)

year2013 <- data.frame(with(year2013, table(occupation, edu)))
year2013 <- year2013 %>%
  spread(edu, Freq)

year2014 <- data.frame(with(year2014, table(occupation, edu)))
year2014 <- year2014 %>%
  spread(edu, Freq)

year2015 <- data.frame(with(year2015, table(occupation, edu)))
year2015 <- year2015 %>%
  spread(edu, Freq)

year2016 <- data.frame(with(year2016, table(occupation, edu)))
year2016 <- year2016 %>%
  spread(edu, Freq)

year2017 <- data.frame(with(year2017, table(occupation, edu)))
year2017 <- year2017 %>%
  spread(edu, Freq)

#CALCULATING HIGHER EDUCATION TOTAL PER YEAR
year2010 <- year2010 %>%
  mutate(`Total Higher Education` = round((rowSums(year2010[higherEducation])), digits = 0))

year2011 <- year2011 %>%
  mutate(`Total Higher Education` = round((rowSums(year2011[higherEducation])), digits = 0))

year2012 <- year2012 %>%
  mutate(`Total Higher Education` = round((rowSums(year2012[higherEducation])), digits = 0))

year2013 <- year2013 %>%
  mutate(`Total Higher Education` = round((rowSums(year2013[higherEducation])), digits = 0))

year2014 <- year2014 %>%
  mutate(`Total Higher Education` = round((rowSums(year2014[higherEducation])), digits = 0))

year2015 <- year2015 %>%
  mutate(`Total Higher Education` = round((rowSums(year2015[higherEducation])), digits = 0))

year2016 <- year2016 %>%
  mutate(`Total Higher Education` = round((rowSums(year2016[higherEducation])), digits = 0))

year2017 <- year2017 %>%
  mutate(`Total Higher Education` = round((rowSums(year2017[higherEducation])), digits = 0))

#SELECTING USEFUL CATEGORIES FROM DATASET
year2010 <- year2010 %>%
  select(occupation, `Total Higher Education`)

year2011 <- year2011 %>%
  select(occupation, `Total Higher Education`)

year2012 <- year2012 %>%
  select(occupation, `Total Higher Education`)

year2013 <- year2013 %>%
  select(occupation, `Total Higher Education`)

year2014 <- year2014 %>%
  select(occupation, `Total Higher Education`)

year2015 <- year2015 %>%
  select(occupation, `Total Higher Education`)

year2016 <- year2016 %>%
  select(occupation, `Total Higher Education`)

year2017 <- year2017 %>%
  select(occupation, `Total Higher Education`)

#CALCULATING PROPORTIONS PER YEAR
year2010 <- year2010 %>%
  mutate(`2010 Proportion` = round(`Total Higher Education`/sum(`Total Higher Education`), digits = 2) * 100)

year2011 <- year2011 %>%
  mutate(`2011 Proportion` = round(`Total Higher Education`/sum(`Total Higher Education`), digits = 2) * 100)

year2012 <- year2012 %>%
  mutate(`2012 Proportion` = round(`Total Higher Education`/sum(`Total Higher Education`), digits = 2) * 100)

year2013 <- year2013 %>%
  mutate(`2013 Proportion` = round(`Total Higher Education`/sum(`Total Higher Education`), digits = 2) * 100)

year2014 <- year2014 %>%
  mutate(`2014 Proportion` = round(`Total Higher Education`/sum(`Total Higher Education`), digits = 2) * 100)

year2015 <- year2015 %>%
  mutate(`2015 Proportion` = round(`Total Higher Education`/sum(`Total Higher Education`), digits = 2) * 100)

year2016 <- year2016 %>%
  mutate(`2016 Proportion` = round(`Total Higher Education`/sum(`Total Higher Education`), digits = 2) * 100)

year2017 <- year2017 %>%
  mutate(`2017 Proportion` = round(`Total Higher Education`/sum(`Total Higher Education`), digits = 2) * 100)