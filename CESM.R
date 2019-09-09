#IMPORTS
library(tidyverse)
library(dplyr)
cesmData <- read.csv(file="second_order_CESM.csv",header=TRUE, sep=",")

#COLLAPSING INTO SMALLER CATEGORIES
cesmData$category <- case_when(
  cesmData$category == "Accounting And Related Services" | cesmData$category == "Taxation" ~"Accounting",
  cesmData$category == "Computer And Information Sciences" | cesmData$category == "Computer and Information Sciences, Other" | cesmData$category == "Computer Business Systems Analysis" | cesmData$category == "Computer Engineering" | cesmData$category == "Computer Programming" | cesmData$category == "Computer Science" | cesmData$category == "Computer Software And Media Applications" | cesmData$category == "Computer Systems Networking And Telecommunications" | cesmData$category == "Computer/Information Technology Administration And Management" | cesmData$category == "Data Entry/Microcomputer Applications" | cesmData$category == "Data Processing And Information Science" | cesmData$category == "Management Information Systems And Services" ~"Computing",
  cesmData$category == "Afrikaans Language And Literature" | cesmData$category == "Asian Languages And Literature" | cesmData$category == "Classics And Classical Languages And Literature" | cesmData$category == "Comparative African Languages And Literatures" | cesmData$category == "Counsellor Education And Guidance Services" | cesmData$category == "Curriculum And Instruction" | cesmData$category == "Dutch Language And Literature" | cesmData$category == "Education, General" | cesmData$category == "Education, Other" | cesmData$category == "Educational Assessment, Evaluation And Research" | cesmData$category == "Educational Management And Leadership" | cesmData$category == "Educational Psychology" | cesmData$category == "Educational/Instructional Media Design" | cesmData$category == "English Language And Literature" | cesmData$category == "European Languages And Literature (Excluding Dutch)" | cesmData$category == "Foundations of Education" | cesmData$category == "International And Comparative Education" | cesmData$category == "Isindebele Language And Literature" | cesmData$category == "Isixhosa Language And Literature" | cesmData$category == "Isizulu Language And Literature" | cesmData$category == "Languages, Linguistics and Literature, Other" | cesmData$category == "Linguistic, Comparative And Related Language Studies And Practices" | cesmData$category == "Middle/Near Eastern And Semitic Languages And Literature" | cesmData$category == "Other African Languages And Literature" | cesmData$category == "Other fields of study in education" | cesmData$category == "Public Administration" | cesmData$category == "Sepedi Language And Literature" | cesmData$category == "Sesotho Language And Literature" | cesmData$category == "Setswana Language And Literature" | cesmData$category == "Siswati Language And Literature" | cesmData$category == "Social And Philosophical Foundations Of Education" | cesmData$category =="South African Sign Language (Sasl)" | cesmData$category == "Special Needs Education" | cesmData$category == "Teacher Education And Professional Development, Specific Levels And Methods" | cesmData$category == "Teacher Education And Professional Development, Specific Subject Areas, Early" | cesmData$category == "Childhood Development (Ecd) And General Education And Training (Get)" | cesmData$category == "Teacher Education And Professional Development, Specific Subject Areas, Further" | cesmData$category == "Education And Training (Fet)" | cesmData$category == "Teaching and Learning Support" | cesmData$category == "Teaching, Leading and Researching in Community and Adult Education and Training Contexts" | cesmData$category == "Teaching, Leading and Researching in Early Childhood Education and Development Contexts" | cesmData$category == "Teaching, Leading and Researching in Higher Education" | cesmData$category == "Teaching; Leading and Researching in Schooling Contexts (Further Education and Training (FET) Phase)" | cesmData$category == "Teaching; Leading and Researching in Schooling Contexts (Grade R and Foundation Phase)" | cesmData$category == "Teaching; Leading and Researching in Schooling Contexts (Inter-mediate Phase)" | cesmData$category == "Teaching; Leading and Researching in Schooling Contexts (Senior Phase)" | cesmData$category == "Teaching; leading and researching in Technical and Vocational Education and Training (TVET) contexts" | cesmData$category == "Tshivenda Language And Literature" | cesmData$category == "Xitsonga Language And Literature" ~"Education",
  cesmData$category == "Human Resource Management and Services" ~"Human Resources",
  cesmData$category == "Finance and Financial Management Services" | cesmData$category == "Insurance" ~"Insurance/Finance",
  cesmData$category == "Public Law" | cesmData$category == "Private Law" | cesmData$category == "Perspectives On Law" | cesmData$category == "Mercantile Law" | cesmData$category == "Legal Profession" | cesmData$category == "Law, Other" | cesmData$category == "International Aspects Of Law" | cesmData$category == "Formal Law" | cesmData$category == "Criminal Justice And Corrections" ~ "Law",
  cesmData$category == "Systems Engineering" | cesmData$category == "Public Management and Services, Other" | cesmData$category == "Management Sciences And Quantitative Methods" | cesmData$category == "Entrepreneurial and Small Business Operations" | cesmData$category == "Business Administration, Management And Operations" | cesmData$category == "Business, Economics and Management Studies, Other" | cesmData$category == "Business/Corporate Communications" ~ "Management/Logistics",
  cesmData$category == "Alternative And Complementary Medicine And Medical Systems" | cesmData$category == "Bioethics/Medical Ethics" | cesmData$category == "Biomedical/Medical Engineering"| cesmData$category == "Chiropractic"| cesmData$category == "Clinical Psychology"| cesmData$category == "Cognitive Psychology And Psycholinguistics"| cesmData$category == "Counselling Psychology"| cesmData$category == "Developmental And Child Psychology"| cesmData$category == "Health And Medical Administrative Services"| cesmData$category == "Health Professions and Related Clinical Sciences, Other"| cesmData$category == "Health/Medical Psychology"| cesmData$category == "Medical Clinical Sciences"| cesmData$category == "Medical Illustration And Informatics"| cesmData$category == "Medical Radiologic Technology/Science (Radiography)"| cesmData$category == "Medicine"| cesmData$category == "Movement And Mind-Body Therapies And Education"| cesmData$category == "Optometry"| cesmData$category == "Personality Psychology"| cesmData$category == "Pharmacology And Toxicology"| cesmData$category == "Pharmacy, Pharmaceutical Sciences And Administration"| cesmData$category == "Physiology, Pathology And Related Sciences"| cesmData$category == "Podiatric Medicine/Podiatry"| cesmData$category == "Psychology, General"| cesmData$category == "Psychology, Other"| cesmData$category == "Psychometrics And Applied Psychological Assessment"| cesmData$category == "Rehabilitation And Therapeutic Professions"~"Medical",
  cesmData$category == "Nursing" ~ "Nursing",
  cesmData$category == "Social Work" | cesmData$category == "Human Services" | cesmData$category == "Community Psychology" | cesmData$category == "Community Organisation and Advocacy" ~ "Social Work")
  
#REMOVING NA ROWS
cesmData <- cesmData %>%
  na.omit(cesmData, cols="category")

#COLLAPSING ROWS
cesmDataCollapsed <- aggregate(cbind(cesmData$X2010, cesmData$X2011, cesmData$X2012, cesmData$X2013, cesmData$X2014, cesmData$X2015, cesmData$X2016, cesmData$X2017), by = list(category = cesmData$category), FUN = sum, na.rm = TRUE)

rm("cesmData")

#RENAMING COLUMNS
names(cesmDataCollapsed) <- c("category", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")

#CALCULATING PROPORTIONS PER YEAR
cesmDataCollapsed <- cesmDataCollapsed %>%
  mutate(`2010 Proportion` = round(`2010`/sum(`2010`), digits = 2) * 100)

cesmDataCollapsed <- cesmDataCollapsed %>%
  mutate(`2011 Proportion` = round(`2011`/sum(`2011`), digits = 2) * 100)

cesmDataCollapsed <- cesmDataCollapsed %>%
  mutate(`2012 Proportion` = round(`2012`/sum(`2012`), digits = 2) * 100)

cesmDataCollapsed <- cesmDataCollapsed %>%
  mutate(`2013 Proportion` = round(`2013`/sum(`2013`), digits = 2) * 100)

cesmDataCollapsed <- cesmDataCollapsed %>%
  mutate(`2014 Proportion` = round(`2014`/sum(`2014`), digits = 2) * 100)

cesmDataCollapsed <- cesmDataCollapsed %>%
  mutate(`2015 Proportion` = round(`2015`/sum(`2015`), digits = 2) * 100)

cesmDataCollapsed <- cesmDataCollapsed %>%
  mutate(`2016 Proportion` = round(`2016`/sum(`2016`), digits = 2) * 100)

cesmDataCollapsed <- cesmDataCollapsed %>%
  mutate(`2017 Proportion` = round(`2017`/sum(`2017`), digits = 2) * 100)

#SELECTING USEFUL ROWS FROM DATASET
cesmDataCollapsed <- cesmDataCollapsed %>%
  select(-`2010`, -`2011`, -`2012`, -`2013`, -`2014`, -`2015`, -`2016`,-`2017`)

#EXPORTING TO CSV
write.csv(cesmDataCollapsed, file = "CESM.csv")