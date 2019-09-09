

qlfsData <- read.csv(file="wf_08_17.csv",header=TRUE, sep=",")

qlfsData$year <- as.numeric(qlfsData$year)
qlfsData$occupation <- as.factor(qlfsData$occupation)

qlfsData$occupation <- case_when(
  qlfsData$occupation == "Accountants and related accounting occupations, Accounting occupations not elsewhere classified, Auditors and related oc" | qlfsData$occupation == "Accounting and bookkeeping clerks" ~"fuck me twice"
)

qlfsData2017 <- qlfsData %>%
  filter(year == 2016)






occupationHE <- c("Bachelors Degree and Post Graduate Diploma", "Bachelors Degree", "Bachelors Degree and Diploma", "Higher Degree (Masters, Doctorate)", "Higher Degree (Masters/Phd)", "Honours Degree", "Post Higher Diploma (Masters; Doctoral Diploma)", "Diploma with Grade 12/Std 10")



myData3 <- data.frame(with(distinct(qlfsData2017), table(occupation, edu)))

myData3 <- myData3 %>%
  spread(edu, Freq)

myData3 <- myData3 %>%
  mutate(completeTotal = rowSums(myData3[2:40]), heTotal = rowSums(myData3[occupationHE]), percentageHE = round((heTotal/completeTotal) * 100, digits = 2)) 

  
cleanerRound1 <- myData3 %>%
  filter(percentageHE >= 25) %>%
  select(occupation, percentageHE)


write.csv(cleanerRound1, file = "cleanerRound2017.csv")

myData4 <- myData3 %>%
  select(occupation, percentageHE)

myData5 <- qlfsData %>%
  filter(edu == "Post Higher Diploma (Masters; Doctoral Diploma)")


levels(qlfsData$edu)
