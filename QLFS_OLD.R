library(dplyr)
library(tidyverse)

qlfsData <- read.csv(file="wf_08_17.csv",header=TRUE, sep=",")

qlfsData$year <- as.numeric(qlfsData$year)
qlfsData$occupation <- as.factor(qlfsData$occupation)

#HIGHER QUALIFICATION CATEGORIES USED

occupationHE <- c("Bachelors Degree and Post Graduate Diploma", "Bachelors Degree", "Bachelors Degree and Diploma", "Higher Degree (Masters, Doctorate)", "Higher Degree (Masters/Phd)", "Honours Degree", "Post Higher Diploma (Masters; Doctoral Diploma)", "Diploma with Grade 12/Std 10")


#CASE WHEN FUNCTION TO COLLAPSE COLUMNS INTO LARGER SIZE PARTICIPANTS PER OCCUPATION

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

#REMOVING UNEMPLOYEED INDIVIDUALS (LATER USE)

qlfsDataEmployed <- qlfsData %>%
  filter(paid_work == "Yes" | own_business == "Yes")

#SEPERATING EMPLOYEED INTO YEARS (LATER USE)

qlfsDataEmployed2017 <- qlfsDataEmployed %>%
  filter(year == 2017)

qlfsDataEmployed2016 <- qlfsDataEmployed %>%
  filter(year == 2016)

qlfsDataEmployed2015 <- qlfsDataEmployed %>%
  filter(year == 2015)

qlfsDataEmployed2014 <- qlfsDataEmployed %>%
  filter(year == 2014)

qlfsDataEmployed2013 <- qlfsDataEmployed %>%
  filter(year == 2013)

qlfsDataEmployed2012 <- qlfsDataEmployed %>%
  filter(year == 2012)

qlfsDataEmployed2011 <- qlfsDataEmployed %>%
  filter(year == 2011)

qlfsDataEmployed2010 <- qlfsDataEmployed %>%
  filter(year == 2010)

#TRANSFORMING ONLY EMPLOYEED DATA (LATER USE)

#2017
seperatedEmployedHigherEducation2017 <- data.frame(with(distinct(qlfsDataEmployed2017), table(occupation, edu)))

seperatedEmployedHigherEducation2017 <- seperatedEmployedHigherEducation2017 %>%
  spread(edu, Freq)

seperatedEmployedHigherEducation2017 <- seperatedEmployedHigherEducation2017 %>%
  mutate(completeTotal = rowSums(seperatedEmployedHigherEducation2017[2:40]), heTotal = rowSums(seperatedEmployedHigherEducation2017[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2016
seperatedEmployedHigherEducation2016 <- data.frame(with(distinct(qlfsDataEmployed2016), table(occupation, edu)))

seperatedEmployedHigherEducation2016 <- seperatedEmployedHigherEducation2016 %>%
  spread(edu, Freq)

seperatedEmployedHigherEducation2016 <- seperatedEmployedHigherEducation2016 %>%
  mutate(completeTotal = rowSums(seperatedEmployedHigherEducation2016[2:40]), heTotal = rowSums(seperatedEmployedHigherEducation2016[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2015
seperatedEmployedHigherEducation2015 <- data.frame(with(distinct(qlfsDataEmployed2015), table(occupation, edu)))

seperatedEmployedHigherEducation2015 <- seperatedEmployedHigherEducation2015 %>%
  spread(edu, Freq)

seperatedEmployedHigherEducation2015 <- seperatedEmployedHigherEducation2015 %>%
  mutate(completeTotal = rowSums(seperatedEmployedHigherEducation2015[2:40]), heTotal = rowSums(seperatedEmployedHigherEducation2015[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2014
seperatedEmployedHigherEducation2014 <- data.frame(with(distinct(qlfsDataEmployed2014), table(occupation, edu)))

seperatedEmployedHigherEducation2014 <- seperatedEmployedHigherEducation2014 %>%
  spread(edu, Freq)

seperatedEmployedHigherEducation2014 <- seperatedEmployedHigherEducation2014 %>%
  mutate(completeTotal = rowSums(seperatedEmployedHigherEducation2014[2:40]), heTotal = rowSums(seperatedEmployedHigherEducation2014[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2013
seperatedEmployedHigherEducation2013 <- data.frame(with(distinct(qlfsDataEmployed2013), table(occupation, edu)))

seperatedEmployedHigherEducation2013 <- seperatedEmployedHigherEducation2013 %>%
  spread(edu, Freq)

seperatedEmployedHigherEducation2013 <- seperatedEmployedHigherEducation2013 %>%
  mutate(completeTotal = rowSums(seperatedEmployedHigherEducation2013[2:40]), heTotal = rowSums(seperatedEmployedHigherEducation2013[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2012
seperatedEmployedHigherEducation2012 <- data.frame(with(distinct(qlfsDataEmployed2012), table(occupation, edu)))

seperatedEmployedHigherEducation2012 <- seperatedEmployedHigherEducation2012 %>%
  spread(edu, Freq)

seperatedEmployedHigherEducation2012 <- seperatedEmployedHigherEducation2012 %>%
  mutate(completeTotal = rowSums(seperatedEmployedHigherEducation2012[2:40]), heTotal = rowSums(seperatedEmployedHigherEducation2012[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2011
seperatedEmployedHigherEducation2011 <- data.frame(with(distinct(qlfsDataEmployed2011), table(occupation, edu)))

seperatedEmployedHigherEducation2011 <- seperatedEmployedHigherEducation2011 %>%
  spread(edu, Freq)

seperatedEmployedHigherEducation2011 <- seperatedEmployedHigherEducation2011 %>%
  mutate(completeTotal = rowSums(seperatedEmployedHigherEducation2011[2:40]), heTotal = rowSums(seperatedEmployedHigherEducation2011[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2010
seperatedEmployedHigherEducation2010 <- data.frame(with(distinct(qlfsDataEmployed2010), table(occupation, edu)))

seperatedEmployedHigherEducation2010 <- seperatedEmployedHigherEducation2010 %>%
  spread(edu, Freq)

seperatedEmployedHigherEducation2010 <- seperatedEmployedHigherEducation2010 %>%
  mutate(completeTotal = rowSums(seperatedEmployedHigherEducation2010[2:40]), heTotal = rowSums(seperatedEmployedHigherEducation2010[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#SEPARATING INTO YEARS

qlfsData2017 <- qlfsData %>%
  filter(year == 2017)

qlfsData2016 <- qlfsData %>%
  filter(year == 2016)

qlfsData2015 <- qlfsData %>%
  filter(year == 2015)

qlfsData2014 <- qlfsData %>%
  filter(year == 2014)

qlfsData2013 <- qlfsData %>%
  filter(year == 2013)

qlfsData2012 <- qlfsData %>%
  filter(year == 2012)

qlfsData2011 <- qlfsData %>%
  filter(year == 2011)

qlfsData2010 <- qlfsData %>%
  filter(year == 2010)

#TRANSFORMING DATA
#2017

seperatedHigherEducation2017 <- data.frame(with(distinct(qlfsData2017), table(occupation, edu)))

seperatedHigherEducation2017 <- seperatedHigherEducation2017 %>%
  spread(edu, Freq)

seperatedHigherEducation2017 <- seperatedHigherEducation2017 %>%
  mutate(completeTotal = rowSums(seperatedHigherEducation2017[2:40]), heTotal = rowSums(seperatedHigherEducation2017[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2016

seperatedHigherEducation2016 <- data.frame(with(distinct(qlfsData2016), table(occupation, edu)))

seperatedHigherEducation2016 <- seperatedHigherEducation2016 %>%
  spread(edu, Freq)

seperatedHigherEducation2016 <- seperatedHigherEducation2016 %>%
  mutate(completeTotal = rowSums(seperatedHigherEducation2016[2:40]), heTotal = rowSums(seperatedHigherEducation2016[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2015

seperatedHigherEducation2015 <- data.frame(with(distinct(qlfsData2015), table(occupation, edu)))

seperatedHigherEducation2015 <- seperatedHigherEducation2015 %>%
  spread(edu, Freq)

seperatedHigherEducation2015 <- seperatedHigherEducation2015 %>%
  mutate(completeTotal = rowSums(seperatedHigherEducation2015[2:40]), heTotal = rowSums(seperatedHigherEducation2015[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2014

seperatedHigherEducation2014 <- data.frame(with(distinct(qlfsData2014), table(occupation, edu)))

seperatedHigherEducation2014 <- seperatedHigherEducation2014 %>%
  spread(edu, Freq)

seperatedHigherEducation2014 <- seperatedHigherEducation2014 %>%
  mutate(completeTotal = rowSums(seperatedHigherEducation2014[2:40]), heTotal = rowSums(seperatedHigherEducation2014[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2013

seperatedHigherEducation2013 <- data.frame(with(distinct(qlfsData2013), table(occupation, edu)))

seperatedHigherEducation2013 <- seperatedHigherEducation2013 %>%
  spread(edu, Freq)

seperatedHigherEducation2013 <- seperatedHigherEducation2013 %>%
  mutate(completeTotal = rowSums(seperatedHigherEducation2013[2:40]), heTotal = rowSums(seperatedHigherEducation2013[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2012

seperatedHigherEducation2012 <- data.frame(with(distinct(qlfsData2012), table(occupation, edu)))

seperatedHigherEducation2012 <- seperatedHigherEducation2012 %>%
  spread(edu, Freq)

seperatedHigherEducation2012 <- seperatedHigherEducation2012 %>%
  mutate(completeTotal = rowSums(seperatedHigherEducation2012[2:40]), heTotal = rowSums(seperatedHigherEducation2012[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2011

seperatedHigherEducation2011 <- data.frame(with(distinct(qlfsData2011), table(occupation, edu)))

seperatedHigherEducation2011 <- seperatedHigherEducation2011 %>%
  spread(edu, Freq)

seperatedHigherEducation2011 <- seperatedHigherEducation2011 %>%
  mutate(completeTotal = rowSums(seperatedHigherEducation2011[2:40]), heTotal = rowSums(seperatedHigherEducation2011[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#2010

seperatedHigherEducation2010 <- data.frame(with(distinct(qlfsData2010), table(occupation, edu)))

seperatedHigherEducation2010 <- seperatedHigherEducation2010 %>%
  spread(edu, Freq)

seperatedHigherEducation2010 <- seperatedHigherEducation2010 %>%
  mutate(completeTotal = rowSums(seperatedHigherEducation2010[2:40]), heTotal = rowSums(seperatedHigherEducation2010[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2))

#COMPUTING DATA WITH HE TOTALS
#2017

cleaner2017 <- seperatedHigherEducation2017 %>%
  filter(percentageHE >= 30) %>%
  select(occupation, percentageHE, completeTotal)

#2016

cleaner2016 <- seperatedHigherEducation2016 %>%
  filter(percentageHE >= 30) %>%
  select(occupation, percentageHE, completeTotal)

#2015

cleaner2015 <- seperatedHigherEducation2015 %>%
  filter(percentageHE >= 30) %>%
  select(occupation, percentageHE, completeTotal)

#2014

cleaner2014 <- seperatedHigherEducation2014 %>%
  filter(percentageHE >= 30) %>%
  select(occupation, percentageHE, completeTotal)

#2013

cleaner2013 <- seperatedHigherEducation2013 %>%
  filter(percentageHE >= 30) %>%
  select(occupation, percentageHE, completeTotal)

#2012

cleaner2012 <- seperatedHigherEducation2012 %>%
  filter(percentageHE >= 30) %>%
  select(occupation, percentageHE, completeTotal)

#2011

cleaner2011 <- seperatedHigherEducation2011 %>%
  filter(percentageHE >= 30) %>%
  select(occupation, percentageHE, completeTotal)

#2010

cleaner2010 <- seperatedHigherEducation2010 %>%
  filter(percentageHE >= 30) %>%
  select(occupation, percentageHE, completeTotal)

#Average of All Years

cleanerAverage <- merge(merge(merge(merge(merge(merge(merge(cleaner2010, cleaner2011, by = "occupation", all=T), cleaner2012, by = "occupation", all = T), cleaner2013, by = "occupation", all= T), cleaner2014, by = "occupation", all = T), cleaner2015, by = "occupation", all = T), cleaner2016, by = "occupation", all = T), cleaner2017, by = "occupation", all = T)

columnNames <- c("occupation", "% 2010", "complete 2010", "% 2011", "complete 2011", "% 2012", "complete 2012", "% 2013", "complete 2013", "% 2014", "complete 2014", "% 2015", "complete 2015", "% 2016", "complete 2016", "% 2017", "complete 2017")

names(cleanerAverage) <- columnNames

cleanerAverage <- cleanerAverage %>%
  select(occupation, `complete 2010`, `complete 2011`, `complete 2012`, `complete 2013`, `complete 2014`, `complete 2015`, `complete 2016`, `complete 2017`, `% 2010`, `% 2011`, `% 2012`, `% 2013`, `% 2014`,`% 2015`,`% 2016`,`% 2017`)

cleanerAverage <- cleanerAverage %>%
  mutate(totals = rowSums(cleanerAverage[2:9], na.rm = T), average = round(rowSums((cleanerAverage[2:9]/7), na.rm=T), digits=0), averageHE = round(rowSums((cleanerAverage[10:17]/7), na.rm =T),digits = 2))

top20 <- cleanerAverage %>%
  select(occupation, averageHE, average) %>%
  top_n(20)

#DATA TO BE EXPORTED USING TOP 20 CATEGORIES TO SELECT BASED ON AN EARLIER DATASET 

export2010 <- seperatedEmployedHigherEducation2010 %>%
  filter(occupation == "Accounting" | occupation == "Agriculture" | occupation == "Architecture" | occupation == "Civil Engineers" | occupation == "Computing" | occupation == "Design" | occupation == "Education" | occupation == "Electrical Engineers" | occupation == "Human Resources" | occupation == "Insurance/Finance" | occupation == "Law" | occupation == "Management/Logistics" | occupation == "Medical" | occupation == "Nursing" | occupation == "Natural Science" | occupation == "Sales" | occupation == "Social Work" | occupation == "Theology") %>%
  select(occupation, completeTotal)

names(export2010) <- c("category", "QLFS Total 2010")

export2011 <- seperatedEmployedHigherEducation2011 %>%
  filter(occupation == "Accounting" | occupation == "Agriculture" | occupation == "Architecture" | occupation == "Civil Engineers" | occupation == "Computing" | occupation == "Design" | occupation == "Education" | occupation == "Electrical Engineers" | occupation == "Human Resources" | occupation == "Insurance/Finance" | occupation == "Law" | occupation == "Management/Logistics" | occupation == "Medical" | occupation == "Nursing" | occupation == "Natural Science" | occupation == "Sales" | occupation == "Social Work" | occupation == "Theology") %>%
  select(occupation, completeTotal)

names(export2011) <- c("category", "QLFS Total 2011")

export2012 <- seperatedEmployedHigherEducation2012 %>%
  filter(occupation == "Accounting" | occupation == "Agriculture" | occupation == "Architecture" | occupation == "Civil Engineers" | occupation == "Computing" | occupation == "Design" | occupation == "Education" | occupation == "Electrical Engineers" | occupation == "Human Resources" | occupation == "Insurance/Finance" | occupation == "Law" | occupation == "Management/Logistics" | occupation == "Medical" | occupation == "Nursing" | occupation == "Natural Science" | occupation == "Sales" | occupation == "Social Work" | occupation == "Theology") %>%
  select(occupation, completeTotal)

names(export2012) <- c("category", "QLFS Total 2012")

export2013 <- seperatedEmployedHigherEducation2013 %>%
  filter(occupation == "Accounting" | occupation == "Agriculture" | occupation == "Architecture" | occupation == "Civil Engineers" | occupation == "Computing" | occupation == "Design" | occupation == "Education" | occupation == "Electrical Engineers" | occupation == "Human Resources" | occupation == "Insurance/Finance" | occupation == "Law" | occupation == "Management/Logistics" | occupation == "Medical" | occupation == "Nursing" | occupation == "Natural Science" | occupation == "Sales" | occupation == "Social Work" | occupation == "Theology") %>%
  select(occupation, completeTotal)

names(export2013) <- c("category", "QLFS Total 2013")

export2014 <- seperatedEmployedHigherEducation2014 %>%
  filter(occupation == "Accounting" | occupation == "Agriculture" | occupation == "Architecture" | occupation == "Civil Engineers" | occupation == "Computing" | occupation == "Design" | occupation == "Education" | occupation == "Electrical Engineers" | occupation == "Human Resources" | occupation == "Insurance/Finance" | occupation == "Law" | occupation == "Management/Logistics" | occupation == "Medical" | occupation == "Nursing" | occupation == "Natural Science" | occupation == "Sales" | occupation == "Social Work" | occupation == "Theology") %>%
  select(occupation, completeTotal)


names(export2014) <- c("category", "QLFS Total 2014")

export2015 <- seperatedEmployedHigherEducation2015 %>%
  filter(occupation == "Accounting" | occupation == "Agriculture" | occupation == "Architecture" | occupation == "Civil Engineers" | occupation == "Computing" | occupation == "Design" | occupation == "Education" | occupation == "Electrical Engineers" | occupation == "Human Resources" | occupation == "Insurance/Finance" | occupation == "Law" | occupation == "Management/Logistics" | occupation == "Medical" | occupation == "Nursing" | occupation == "Natural Science" | occupation == "Sales" | occupation == "Social Work" | occupation == "Theology") %>%
  select(occupation, completeTotal)

names(export2015) <- c("category", "QLFS Total 2015")

export2016 <- seperatedEmployedHigherEducation2016 %>%
  filter(occupation == "Accounting" | occupation == "Agriculture" | occupation == "Architecture" | occupation == "Civil Engineers" | occupation == "Computing" | occupation == "Design" | occupation == "Education" | occupation == "Electrical Engineers" | occupation == "Human Resources" | occupation == "Insurance/Finance" | occupation == "Law" | occupation == "Management/Logistics" | occupation == "Medical" | occupation == "Nursing" | occupation == "Natural Science" | occupation == "Sales" | occupation == "Social Work" | occupation == "Theology") %>%
  select(occupation, completeTotal)

names(export2016) <- c("category", "QLFS Total 2016")

export2017 <- seperatedEmployedHigherEducation2017 %>%
  filter(occupation == "Accounting" | occupation == "Agriculture" | occupation == "Architecture" | occupation == "Civil Engineers" | occupation == "Computing" | occupation == "Design" | occupation == "Education" | occupation == "Electrical Engineers" | occupation == "Human Resources" | occupation == "Insurance/Finance" | occupation == "Law" | occupation == "Management/Logistics" | occupation == "Medical" | occupation == "Nursing" | occupation == "Natural Science" | occupation == "Sales" | occupation == "Social Work" | occupation == "Theology") %>%
  select(occupation, completeTotal)

names(export2017) <- c("category", "QLFS Total 2017")

exportAllYears <- merge(merge(merge(merge(merge(merge(merge(export2010, export2011, by = "category", all = T), export2012, by = "category", all = T), export2013, by = "category", all = T), export2014, by = "category", all = T), export2015, by = "category", all =T), export2016, by = "category", all = T), export2017, by = "category", all = T)

#CALULCATING GROWTH RATES WITH EXPORT DATA

exportAllYearsQLFS <- exportAllYears %>%
  mutate(`QLFS GR 2011` = round(((`QLFS Total 2011`/sum(`QLFS Total 2011`)) - (`QLFS Total 2010`/sum(`QLFS Total 2010`)))/(`QLFS Total 2010`/sum(`QLFS Total 2010`)) * 100, digits = 2), `QLFS GR 2012` = round(((`QLFS Total 2012`/sum(`QLFS Total 2012`)) - (`QLFS Total 2011`/sum(`QLFS Total 2011`)))/(`QLFS Total 2011`/sum(`QLFS Total 2011`)) * 100, digits = 2), `QLFS GR 2013` = round(((`QLFS Total 2013`/sum(`QLFS Total 2013`)) - (`QLFS Total 2012`/sum(`QLFS Total 2012`)))/(`QLFS Total 2012`/sum(`QLFS Total 2012`)) * 100, digits = 2), `QLFS GR 2014` = round(((`QLFS Total 2014`/sum(`QLFS Total 2014`)) - (`QLFS Total 2013`/sum(`QLFS Total 2013`)))/(`QLFS Total 2013`/sum(`QLFS Total 2013`)) * 100, digits = 2), `QLFS GR 2015` = round(((`QLFS Total 2015`/sum(`QLFS Total 2015`)) - (`QLFS Total 2014`/sum(`QLFS Total 2014`)))/(`QLFS Total 2014`/sum(`QLFS Total 2014`)) * 100, digits = 2), `QLFS GR 2016` = round(((`QLFS Total 2016`/sum(`QLFS Total 2016`)) - (`QLFS Total 2015`/sum(`QLFS Total 2015`)))/(`QLFS Total 2015`/sum(`QLFS Total 2015`)) * 100, digits = 2), `QLFS GR 2017` = round(((`QLFS Total 2017`/sum(`QLFS Total 2017`)) - (`QLFS Total 2016`/sum(`QLFS Total 2016`)))/(`QLFS Total 2016`/sum(`QLFS Total 2016`)) * 100, digits = 2), `QLFS Cumulative GR` = round((`QLFS GR 2011` + `QLFS GR 2012` + `QLFS GR 2013` + `QLFS GR 2014` + `QLFS GR 2015` + `QLFS GR 2016` + `QLFS GR 2017`)/7, digits = 2))

#EXPORTING TO CSV
write.csv(exportAllYearsQLFS, file = "QLFS WITH GROWTH RATES.csv")

#TABULATING PROPORTIONS
qlfsProportions <- exportAllYears %>%
  mutate(propQLFS2010 = round((`QLFS Total 2010`/ sum(`QLFS Total 2010`))*100, digits = 2),
         propQLFS2011 = round((`QLFS Total 2011`/ sum(`QLFS Total 2011`))*100, digits = 2),
         propQLFS2012 = round((`QLFS Total 2012`/ sum(`QLFS Total 2012`))*100,digits = 2),
         propQLFS2013 = round((`QLFS Total 2013`/ sum(`QLFS Total 2013`))*100,digits = 2),
         propQLFS2014 = round((`QLFS Total 2014`/ sum(`QLFS Total 2014`))*100,digits = 2),
         propQLFS2015 = round((`QLFS Total 2015`/ sum(`QLFS Total 2015`))*100,digits = 2),
         propQLFS2016 = round((`QLFS Total 2016`/ sum(`QLFS Total 2016`))*100,digits = 2),
         propQLFS2017 = round((`QLFS Total 2017`/ sum(`QLFS Total 2017`))*100, digits = 2) )%>%
  select(category, propQLFS2010, propQLFS2011, propQLFS2012,propQLFS2013, propQLFS2014, propQLFS2015, propQLFS2016, propQLFS2017)

#EXPORTING PROPORTIONS TO CV
write.csv(qlfsProportions, file = "QLFS WITH PROPORTIONS.csv")
