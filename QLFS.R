#IMPORTS
library(tidyverse)
library(dplyr)
qlfsData <- read.csv(file="wf_08_17.csv",header=TRUE, sep=",")

#CHANGING DATA STRUCTURE
qlfsData$year <- as.numeric(qlfsData$year)
qlfsData$occupation <- as.factor(qlfsData$occupation)

#COLLAPSING INTO SMALLER CATEGORIES
qlfsData$occupation <- case_when(
  qlfsData$occupation == "Accountants and related accounting occupations, Accounting occupations not elsewhere classified, Auditors and related oc" | qlfsData$occupation == "Accounting and bookkeeping clerks" | qlfsData$occupation == "Bookkeepers" | qlfsData$occupation == "Accountants and related accounting occupations, Accounting o" | qlfsData$occupation == "Customs, tax and related government associate professionals not elsewhere classified" | qlfsData$occupation == "Government tax and excise officers" ~"Accounting",
  qlfsData$occupation == "Administrative associate professionals not elsewhere classified" | qlfsData$occupation == "Administrative secretaries and related associate professiona" | qlfsData$occupation == "Administrative secretaries and related associate professionals" | qlfsData$occupation == "Other office clerks and clerks not elsewhere classified (except customer services clerks)" | qlfsData$occupation == "Secretaries" ~"Assistants",
  qlfsData$occupation == "Advertising and public relations managers/department managers" ~"Marketing",
  qlfsData$occupation == "Advocates, attorneys and related occupations, Lawyers/attorn" | qlfsData$occupation == "Advocates, attorneys and related occupations, Lawyers/attorneys and related occupations, Advocates/barristers, Prosecuto" | qlfsData$occupation == "Judges and magistrates, Judges , and Magistrates" | qlfsData$occupation == "Legal and related business associate professionals, Legal bu" | qlfsData$occupation == "Legal and related business associate professionals, Legal business professions and Other business professions" | qlfsData$occupation == "Legal professionals not elsewhere classified" | qlfsData$occupation == "Legislators" ~"Law",
  qlfsData$occupation == "Agronomists, food scientists and related professionals, Agri" | qlfsData$occupation == "Agronomists, food scientists and related professionals, Agriculture, forestry and food scientists, Natural sciences tech" | qlfsData$occupation == "Agronomy and forestry technicians, Technicians, agronomy and forestry, Assistants, technical and agriculture" | qlfsData$occupation == "Apiarists and sericulturists (farm owners and skilled farm workers)" | qlfsData$occupation == "Dairy and livestock producers (farm owners and skilled farm workers)" | qlfsData$occupation == "Farming and forestry advisers/consultants" | qlfsData$occupation == "Fishery, hunting and trapping labourers" | qlfsData$occupation == "Fruit, vegetable and related product preservers (including apprentices/trainees)" | qlfsData$occupation == "General managers in agriculture, hunting, forestry and fishi" | qlfsData$occupation == "General managers in agriculture, hunting, forestry and fishing" | qlfsData$occupation == "Market-oriented animal producers and related workers not elsewhere classified (farm owners and skilled farm workers)" | qlfsData$occupation == "Market-oriented crop and animal producers (farm owners and s" | qlfsData$occupation == "Market-oriented crop and animal producers (farm owners and skilled farm workers)" | qlfsData$occupation == "Mixed crop growers (farm owners and skilled farm workers)" | qlfsData$occupation == "Poultry producers (farm owners and skilled farm workers)" | qlfsData$occupation == "Production and operations managers/department managers in ag" | qlfsData$occupation == "Production and operations managers/department managers in agriculture, hunting, forestry, fishing and mining" | qlfsData$occupation == "Tree and shrub crop growers (farm owners and skilled farm workers)" ~"Agriculture",
  qlfsData$occupation == "Appraisers, valuers and auctioneers" ~"Appraisal",
  qlfsData$occupation == "Architects, engineers and related professionals not elsewher" | qlfsData$occupation == "Architects, engineers and related professionals not elsewhere classified, Industrial/production engineers, Quantity surv" | qlfsData$occupation == "Architects, town and traffic planners" | qlfsData$occupation == "Draughtspersons" ~"Architecture",
  qlfsData$occupation == "Sculptors, painters and related artists" ~"Art",
  qlfsData$occupation == "Air traffic controllers" | qlfsData$occupation == "Air traffic safety technicians" | qlfsData$occupation == "Aircraft engine mechanics and fitters (including apprentices/trainees)" | qlfsData$occupation == "Aircraft pilots and related associate professionals, Air tra" | qlfsData$occupation == "Aircraft pilots and related associate professionals, Air transport supervisors, Aircraft pilots, Navigators and Flight e" ~"Aviation",
  qlfsData$occupation == "Aquatic life cultivation workers" | qlfsData$occupation == "Biologists, botanists, zoologists and related professionals" ~"Biologists",
  qlfsData$occupation == "Blacksmiths, hammersmiths and forging-press workers (including apprentices/trainees)" | qlfsData$occupation == "Metal drawers and extruders" | qlfsData$occupation == "Metal finishing, plating and coating machine operators" | qlfsData$occupation == "Metal moulders and coremakers (including apprentices/trainees)" | qlfsData$occupation == "Ore and metal furnace operators" | qlfsData$occupation == "Woodworking-machine setters and setter-operators (including apprentices/trainees)" | qlfsData$occupation == "Printing engravers and etchers (including apprentices/trainees)" ~"Blacksmiths/Metal Work/Woodwork",
  qlfsData$occupation == "Chemical engineering technicians" | qlfsData$occupation == "Chemical engineers" | qlfsData$occupation == "Chemical-processing plant operators not elsewhere classified" | qlfsData$occupation == "Chemical heat-treating plant operators" ~"Chemical Engineers",
  qlfsData$occupation == "Chemists" ~"Chemists",
  qlfsData$occupation == "Civil engineering technicians, Technicians, engineering, civ" | qlfsData$occupation == "Civil engineering technicians, Technicians, engineering, civil, Assistants, technical and civil engineering" | qlfsData$occupation == "Civil engineers" ~"Civil Engineers",
  qlfsData$occupation == "Computer assistants" | qlfsData$occupation == "Computer programmers" | qlfsData$occupation == "Computer systems designers and analysts" | qlfsData$occupation == "Computing professionals not elsewhere classified" | qlfsData$occupation == "Computing services managers/department managers" ~"Computing",
  qlfsData$occupation == "Building and fire inspectors" | qlfsData$occupation == "General managers in construction" | qlfsData$occupation == "Production and operations managers/department managers in building and construction" | qlfsData$occupation == "Safety, health and quality inspectors, Inspectors, safety and health" | qlfsData$occupation == "Underwater workers (including apprentices/trainees)" ~"Construction",
  qlfsData$occupation == "Dental assistants" | qlfsData$occupation == "Dentists (general), Dental specialists and Other dental occu" | qlfsData$occupation == "Dentists (general), Dental specialists and Other dental occupations" ~"Dental",
  qlfsData$occupation == "Decorators and commercial designers, Product, industrial des" | qlfsData$occupation == "Decorators and commercial designers, Product, industrial designers, Textile/ clothing/ fashion designers, Interior desig" ~"Design",
  qlfsData$occupation == "Dieticians and nutritionists" ~"Dietary",
  qlfsData$occupation == "Economists" ~"Economics",
  qlfsData$occupation == "Education methods specialists" | qlfsData$occupation == "Other teaching associate professionals" | qlfsData$occupation == "Pre-primary education teaching professionals" | qlfsData$occupation == "Primary education teaching associate professionals" | qlfsData$occupation == "Primary education teaching professionals" | qlfsData$occupation == "Rectors/principals of universities/colleges" | qlfsData$occupation == "School inspectors" | qlfsData$occupation == "Secondary education teaching professionals" | qlfsData$occupation == "Special education teaching associate professionals" | qlfsData$occupation == "Special education teaching professionals" | qlfsData$occupation == "Teaching associate professionals not elsewhere classified" | qlfsData$occupation == "Technikon, teacher training, technical and other colleges, u" | qlfsData$occupation == "Technikon, teacher training, technical and other colleges, university and other higher education institutions teaching p" ~"Education",
  qlfsData$occupation == "Electrical engineering technicians, Technicians, engineering" | qlfsData$occupation == "Electrical engineering technicians, Technicians, engineering, electrical, Assistants, technical, electrical engineering" | qlfsData$occupation == "Electrical engineers" | qlfsData$occupation == "Electronics and telecommunications engineering technicians, " | qlfsData$occupation == "Electronics and telecommunications engineering technicians, Assistants, technical and electronic engineering" | qlfsData$occupation == "Electronics and telecommunications engineers" ~"Electrical Engineers",
  qlfsData$occupation == "Art, entertainment and sport associate professionals not els" | qlfsData$occupation == "Art, entertainment and sport associate professionals not elsewhere classified" | qlfsData$occupation == "Athletes, sportspersons and related associate professionals" | qlfsData$occupation == "Choreographers and dancers" | qlfsData$occupation == "Clowns, magicians, acrobats and related associate professionals" | qlfsData$occupation == "Radio, television and other announcers" | qlfsData$occupation == "Street, nightclub and related musicians, singers and dancers" ~"Entertainment",
  qlfsData$occupation == "Broadcasting and telecommunications equipment operators" | qlfsData$occupation == "Film, stage and related actors and directors" | qlfsData$occupation == "Photographers and image recording equipment operators, Sound recording equipment operators" | qlfsData$occupation == "Photographic and related workers (including apprentices/trai" | qlfsData$occupation == "Photographic and related workers (including apprentices/trainees)" ~"Film/Photography",
  qlfsData$occupation == "Production and operations managers/department managers in hotels, restaurants and other catering and accommodation servi" | qlfsData$occupation == "Tea, coffee, and cocoa-processing machine operators" ~"Hospitality",
  qlfsData$occupation == "Employment agents and labour contractors" | qlfsData$occupation == "Personnel and careers professionals, Consultants: management" | qlfsData$occupation == "Personnel and careers professionals, Consultants: management/personnel" | qlfsData$occupation == "Personnel and industrial relations managers/department manag" | qlfsData$occupation == "Personnel and industrial relations managers/department managers" ~"Human Resources",
  qlfsData$occupation == "Clearing and forwarding agents" | qlfsData$occupation == "Customs and border inspectors" ~"Importers/Exporters",
  qlfsData$occupation == "Archivists and curators" | qlfsData$occupation == "Librarians and related information professionals" | qlfsData$occupation == "Research and development managers/department managers" | qlfsData$occupation == "Word-processor and related operators" ~"Information Science",
  qlfsData$occupation == "Business services agents and trade brokers not elsewhere cla" | qlfsData$occupation == "Business services agents and trade brokers not elsewhere classified" | qlfsData$occupation == "Finance and administration managers/department managers" | qlfsData$occupation == "Finance and sales associate professionals not elsewhere classified" | qlfsData$occupation == "Insurance representatives" | qlfsData$occupation == "Securities and finance dealers and brokers" | qlfsData$occupation == "Statistical finance clerks" | qlfsData$occupation == "Trade brokers" ~"Insurance/Finance",
  qlfsData$occupation == "Philologists, translators and interpreters" | qlfsData$occupation == "Scribes and related workers" | qlfsData$occupation == "Stenographers and typists" ~"Language",
  qlfsData$occupation == "Corporate managers not elsewhere classified" | qlfsData$occupation == "Directors and chief executives" | qlfsData$occupation == "General managers not elsewhere classified" | qlfsData$occupation == "General managers of business services" | qlfsData$occupation == "Other managers/department managers not elsewhere classified" | qlfsData$occupation == "Production and operations managers/department managers in business services" | qlfsData$occupation == "Production and operations managers/department managers in ma" | qlfsData$occupation == "Production and operations managers/department managers in manufacturing" | qlfsData$occupation == "Production and operations managers/department managers not e" | qlfsData$occupation == "Production and operations managers/department managers not elsewhere classified" | qlfsData$occupation == "Production and operations managers/department managers in bu" | qlfsData$occupation == "Production and operations managers/department managers in bu_duplicated_1227" | qlfsData$occupation == "Senior officers of employers', workers' and other economic-interest organisations" | qlfsData$occupation == "Government licensing officers" | qlfsData$occupation == "Government social benefits officers" | qlfsData$occupation == "Senior government officers" | qlfsData$occupation == "Senior officers of political party organisations" | qlfsData$occupation == "Business professionals not elsewhere classified, Consultants" | qlfsData$occupation == "Production and operations managers/department managers in tr" | qlfsData$occupation == "Production and operations managers/department managers in transport, storage and communications" | qlfsData$occupation == "Production clerks" | qlfsData$occupation == "Supply and distribution managers/department managers" ~"Management/Logistics",
  qlfsData$occupation == "Ships’ deck officers and pilots" | qlfsData$occupation == "Shipsâ€™ deck officers and pilots" ~"Maritime Studies",
  qlfsData$occupation == "Calculating-machine operators" | qlfsData$occupation == "Mathematicians and related professionals, Analysts and metho" | qlfsData$occupation == "Mathematicians and related professionals, Analysts and methodology research" | qlfsData$occupation == "Statistical, mathematical and related associate professional" | qlfsData$occupation == "Statistical, mathematical and related associate professionals" | qlfsData$occupation == "Statisticians" ~"Maths",
  qlfsData$occupation == "Mechanical engineering technicians, Technicians, engineering, mechanical, Assistants, technical and mechanical engineeri" | qlfsData$occupation == "Mechanical engineers" ~"Mechanical Engineers",
  qlfsData$occupation == "Institution-based personal care workers, Nursing aids, Ambulance men and first-aid attendants" | qlfsData$occupation == "Life science technicians, Biological science and Medical sci" | qlfsData$occupation == "Life science technicians, Biological science and Medical science" | qlfsData$occupation == "Medical assistants" | qlfsData$occupation == "Medical equipment operators" | qlfsData$occupation == "Medical equipment operators" | qlfsData$occupation == "Medical practitioners, physicians, Medical specialists and M" | qlfsData$occupation == "Medical practitioners, physicians, Medical specialists and Medical occupations not elsewhere classified" | qlfsData$occupation == "Modern health associate professionals (except nursing) not e" | qlfsData$occupation == "Modern health associate professionals (except nursing) not elsewhere classified, Homeopaths, Therapists, speech, Therapi" | qlfsData$occupation == "Optometrist? assistants" | qlfsData$occupation == "Optometristâ€™ assistants" | qlfsData$occupation == "Optometrists and opticians" | qlfsData$occupation == "Pharmaceutical assistants" | qlfsData$occupation == "Pharmacists" | qlfsData$occupation == "Physiotherapists and related associate professionals, Physio" | qlfsData$occupation == "Physiotherapists and related associate professionals, Physiotherapists, Masseurs, Therapists not elsewhere classified, R" | qlfsData$occupation == "Psychologists, Psychometricians and Psycho-technicians" ~"Medical",
  qlfsData$occupation == "Diamond diver" | qlfsData$occupation == "Mining and metallurgical technicians" | qlfsData$occupation == "Mining engineers, Metallurgists and related professionals" ~"Mining Engineer",
  qlfsData$occupation == "Composers, musicians and singers" ~"Music",
  qlfsData$occupation == "Geologists and geophysicists" | qlfsData$occupation == "Natural science technicians" ~"Natural Science",
  qlfsData$occupation == "Midwifery associate professionals" | qlfsData$occupation == "Nursing and midwifery professionals, Nursing services manage" | qlfsData$occupation == "Nursing and midwifery professionals, Nursing services managers and Professional nurses" | qlfsData$occupation == "Nursing associate professionals, Nurses, senior, student, pu" | qlfsData$occupation == "Nursing associate professionals, Nurses, senior, student, pupil, Nurses, not elsewhere classified (nursing assistants/ai" ~"Nursing",
  qlfsData$occupation == "Industrial robot controllers" | qlfsData$occupation == "Land surveyors, Cartographers and other surveyors" | qlfsData$occupation == "Physical and engineering science technicians not elsewhere c" | qlfsData$occupation == "Physical and engineering science technicians not elsewhere classified, Technicians, physical and engeneering science, As" | qlfsData$occupation == "Brewers, wine and other beverage machine operators" ~"Other Engineers",
  qlfsData$occupation == "Other associate professionals not elsewhere classified" | qlfsData$occupation == "Other professionals not elsewhere classified" ~"Other Professionals",
  qlfsData$occupation == "Police inspectors and detectives" ~"Public Health and Safety",
  qlfsData$occupation == "Estate agents" | qlfsData$occupation == "Production and operations managers/department managers in wholesale and retail trade" | qlfsData$occupation == "Sales and marketing managers/department managers" | qlfsData$occupation == "Technical and commercial sales representatives" ~"Sales",
  qlfsData$occupation == "Biological sciences, Chemical sciences, Medical sciences, Ph" | qlfsData$occupation == "Biological sciences, Chemical sciences, Medical sciences, Physical sciences and Veterinary sciences" | qlfsData$occupation == "Meteorologists" | qlfsData$occupation == "Physical sciences technologists" | qlfsData$occupation == "Physical, mathematical and engineering science professionals not elsewhere classified" | qlfsData$occupation == "Physicists and astronomers" | qlfsData$occupation == "Scientist" ~"Scientists",
  qlfsData$occupation == "Customer services clerks not elsewhere classified" ~"Service Clerks",
  qlfsData$occupation == "Philosophers, historians and political scientists" | qlfsData$occupation == "Sociologists, anthropologists and related professionals" ~"Social Science",
  qlfsData$occupation == "General managers in personal care, cleaning and related services" | qlfsData$occupation == "Production and operations managers/department managers in personal care, cleaning and related services" | qlfsData$occupation == "Senior officers of humanitarian and other special-interest organisations" | qlfsData$occupation == "Social work associate professionals" | qlfsData$occupation == "Social work professionals" | qlfsData$occupation == "Social work researcher" ~"Social Work",
  qlfsData$occupation == "Religious associate professionals" | qlfsData$occupation == "Religious professionals" ~"Theology",
  qlfsData$occupation == "Travel agency and related clerks" | qlfsData$occupation == "Travel attendants and travel stewards" | qlfsData$occupation == "Travel consultants and organisers" | qlfsData$occupation == "Travel guides" ~"Travel",
  qlfsData$occupation == "Ammunition and explosive products machine operators" | qlfsData$occupation == "Railway brakers, signallers, shunters and related workers" | qlfsData$occupation == "Riggers and cable splicers (including apprentices/trainees)" | qlfsData$occupation == "Steam-engine and boiler operators" | qlfsData$occupation == "Tellers and other counter clerks" | qlfsData$occupation == "Petroleum and natural gas refining plant operators" | qlfsData$occupation == "Electronic equipment assemblers" ~"Labourer",
  qlfsData$occupation == "Veterinarians" | qlfsData$occupation == "Veterinary assistants" ~"Vet",
  qlfsData$occupation == "Hunters and trappers" ~"Wildlife",
  qlfsData$occupation == "Authors, journalists and other writers, Editors, Reporters, " | qlfsData$occupation == "Authors, journalists and other writers, Editors, Reporters, journalists, Writers, poets, playwrights and Other writers, " ~"Writing/Journalism"
)

#SELECTING USEFUL CATEGORIES FROM DATASET
qlfsData <- qlfsData %>%
  select(occupation, edu, year, status)

#SPREADING DATA TO EDUCATION COLUMNS
qlfsDataSpread <- data.frame(with(qlfsData, table(occupation, edu)))
qlfsDataSpread <- qlfsDataSpread %>%
  spread(edu, Freq)

#CALULATING AVERAGE TOTALS PER OCCUPATION
qlfsDataSpread <- qlfsDataSpread %>%
  mutate(`Average Total/Year` = round((rowSums(qlfsDataSpread[2:40]))/7, digits = 0))

#HIGHER EDUCATION LIST
higherEducation <- c("Bachelors Degree and Post Graduate Diploma", "Bachelors Degree", "Bachelors Degree and Diploma", "Higher Degree (Masters, Doctorate)", "Higher Degree (Masters/Phd)", "Honours Degree", "Post Higher Diploma (Masters; Doctoral Diploma)", "Diploma with Grade 12/Std 10")

#CALULATING AVERAGE HIGHER EDUCATION TOTAL PER YEAR
qlfsDataSpread <- qlfsDataSpread %>%
  mutate(`Average Higher Education/Year` = round((rowSums(qlfsDataSpread[higherEducation])/7), digits = 0))

#CALCULATING HIGHER EDUCATION SCORE
qlfsDataSpread <- qlfsDataSpread %>%
  mutate(`Higher Education Score` = round((`Average Higher Education/Year`/`Average Total/Year` * 100), digits = 2))

#FILTERING OUT OCCUPATIONS < 30%
qlfsDataSpread <- qlfsDataSpread %>%
  filter(`Higher Education Score`>30)

#SELECTING TOP 20 BASED ON AVERAGE HIGHER EDUCATION TOTAL PER YEAR
top <- qlfsDataSpread %>%
  select(occupation, `Higher Education Score`, `Average Higher Education/Year`) %>%
  top_n(20)

#FILTERING OUT OCCUPATIONS < 100
top <- top %>%
  filter(`Average Higher Education/Year`>100)

#FILTERING INTO YEARS
year2010 <- qlfsData %>%
  filter(year == 2010)
year2011 <- qlfsData %>%
  filter(year == 2011)
year2012 <- qlfsData %>%
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

#MERGING ALL YEAR COLUMNS
qlfsProportions <- merge(merge(merge(merge(merge(merge(merge(year2010, year2011, by = "occupation", all = T), year2012, by = "occupation", all = T), year2013, by = "occupation", all = T), year2014, by = "occupation", all = T), year2015, by = "occupation", all =T), year2016, by = "occupation", all = T), year2017, by = "occupation", all = T)

rm("year2010", "year2011", "year2012", "year2013", "year2014", "year2015", "year2016", "year2017", "higherEducation")

#CHANGING THE NAMES OF PROPORTIONS DATASET TO ACCOUNT FOR DUPPLICATE COLUMNS
names(qlfsProportions) <- c("occupation", "2010", "2010 Proportion", "2011", "2011 Proportion", "2012", "2012 Proportion", "2013", "2013 Proportion", "2014", "2014 Proportion", "2015", "2015 Proportion", "2016", "2016 Proportion", "2017", "2017 Proportion")

#SELECTING USEFUL CATEGORIES FROM DATASET
qlfsProportions <- qlfsProportions %>%
  select(occupation, `2010 Proportion`, `2011 Proportion`, `2012 Proportion`, `2013 Proportion`, `2014 Proportion`, `2015 Proportion`, `2016 Proportion`, `2017 Proportion`)

#EXPORTING DATA TO CSV
write.csv(qlfsProportions, file = "QLFS.csv")
