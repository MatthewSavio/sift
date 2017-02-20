# Calculate Roll-up and Roll-down reports from Meridian Data

library(plyr)
library(dplyr)

#change it

# Part one: Import new datafile

filename <- readline(prompt="Enter a file name: ")

newdata <- read.csv(filename, header = TRUE)

#Add column with days since start date at job
newdata$DaysInJob <- (Sys.Date() - as.Date(as.character(newdata$`Date.in.Job`), format="%m/%d/%y"))# 

# Add column with 'Less than one year' and 'Greater than one year'
newdata$Tenure <- floor(newdata$DaysInJob / 365)

print("19")
#Add Mood Engagement Row
newdata$MoodEng[newdata$Mood.Score < 8]<-"Engaged"
newdata$MoodEng[newdata$Mood.Score < 6]<-"Neutral"
newdata$MoodEng[newdata$Mood.Score < 4]<-"Disengaged"

#Add Q1 Row
newdata$Q1[newdata$Category.Question.1.Score < 8]<-"Engaged"
newdata$Q1[newdata$Category.Question.1.Score < 6]<-"Neutral"
newdata$Q1[newdata$Category.Question.1.Score < 4]<-"Disengaged"

newdata$Q2[newdata$Category.Question.2.Score < 8]<-"Engaged"
newdata$Q2[newdata$Category.Question.2.Score < 6]<-"Neutral"
newdata$Q2[newdata$Category.Question.2.Score < 4]<-"Disengaged"

newdata$Q3[newdata$Category.Question.3.Score < 8]<-"Engaged"
newdata$Q3[newdata$Category.Question.3.Score < 6]<-"Neutral"
newdata$Q3[newdata$Category.Question.3.Score < 4]<-"Disengaged"



#Add QAvg Row
newdata$QAvg <- rowMeans(subset(newdata, select = c(Category.Question.1.Score, Category.Question.2.Score, Category.Question.3.Score)), na.rm = TRUE)

#Add QAvg Engagement Row
newdata$QEng[newdata$QAvg < 8]<-"Engaged"
newdata$QEng[newdata$QAvg < 6]<-"Neutral"
newdata$QEng[newdata$QAvg < 4]<-"Disengaged"


print("49")
# rounding function --> moodavg <- plyr::round_any(mean(newdata$`Mood.Score`), accuracy=0.01, f=floor)
### Part two: Generate company-wide summaries ###

# Company-wide Mood NPS
va <- sum(newdata$MoodEng == "Engaged", na.rm = TRUE)
vb <- sum(newdata$MoodEng == "Neutral", na.rm = TRUE)
vc <- sum(newdata$MoodEng == "Disengaged", na.rm = TRUE)

AllCompany.Mood <- plyr::round_any(100 * (va - vc)/(va + vb + vc), accuracy=1, f=floor)

# Company-wide Engagement NPS

va <- sum(newdata$QEng == "Engaged", na.rm = TRUE)
vb <- sum(newdata$QEng == "Neutral", na.rm = TRUE)
vc <- sum(newdata$QEng == "Disengaged", na.rm = TRUE)

AllCompany.Engagement <- plyr::round_any(100 * (va - vc)/(va + vb + vc), accuracy=1, f=floor)

# Company-wide Categories

#Add Category Engagement Rows
newdata$OnlyOwn[(newdata$QEng == "Engaged")&(newdata$Category == "Ownership")]<-"Engaged"
newdata$OnlyOwn[(newdata$QEng == "Neutral")&(newdata$Category == "Ownership")]<-"Neutral"
newdata$OnlyOwn[(newdata$QEng == "Disengaged")&(newdata$Category == "Ownership")]<-"Disengaged"

va <- sum(newdata$OnlyOwn == "Engaged", na.rm = TRUE)
vb <- sum(newdata$OnlyOwn == "Neutral", na.rm = TRUE)
vc <- sum(newdata$OnlyOwn == "Disengaged", na.rm = TRUE)

AllCompany.Ownership <- plyr::round_any(100 * (va - vc)/(va + vb + vc), accuracy=1, f=floor)


newdata$OnlyCD[(newdata$QEng == "Engaged")&(newdata$Category == "Career Development")]<-"Engaged"
newdata$OnlyCD[(newdata$QEng == "Neutral")&(newdata$Category == "Career Development")]<-"Neutral"
newdata$OnlyCD[(newdata$QEng == "Disengaged")&(newdata$Category == "Career Development")]<-"Disengaged"

va <- sum(newdata$OnlyCD == "Engaged", na.rm = TRUE)
vb <- sum(newdata$OnlyCD == "Neutral", na.rm = TRUE)
vc <- sum(newdata$OnlyCD == "Disengaged", na.rm = TRUE)

AllCompany.Career_Development <- plyr::round_any(100 * (va - vc)/(va + vb + vc), accuracy=1, f=floor)


newdata$OnlyLea[(newdata$QEng == "Engaged")&(newdata$Category == "Leadership")]<-"Engaged"
newdata$OnlyLea[(newdata$QEng == "Neutral")&(newdata$Category == "Leadership")]<-"Neutral"
newdata$OnlyLea[(newdata$QEng == "Disengaged")&(newdata$Category == "Leadership")]<-"Disengaged"

va <- sum(newdata$OnlyLea == "Engaged", na.rm = TRUE)
vb <- sum(newdata$OnlyLea == "Neutral", na.rm = TRUE)
vc <- sum(newdata$OnlyLea == "Disengaged", na.rm = TRUE)
print ("100")
AllCompany.Leadership <- plyr::round_any(100 * (va - vc)/(va + vb + vc), accuracy=1, f=floor)

newdata$OnlyMis[(newdata$QEng == "Engaged")&(newdata$Category == "Mission")]<-"Engaged"
newdata$OnlyMis[(newdata$QEng == "Neutral")&(newdata$Category == "Mission")]<-"Neutral"
newdata$OnlyMis[(newdata$QEng == "Disengaged")&(newdata$Category == "Mission")]<-"Disengaged"

va <- sum(newdata$OnlyMis == "Engaged", na.rm = TRUE)
vb <- sum(newdata$OnlyMis == "Neutral", na.rm = TRUE)
vc <- sum(newdata$OnlyMis == "Disengaged", na.rm = TRUE)

AllCompany.Mission <- plyr::round_any(100 * (va - vc)/(va + vb + vc), accuracy=1, f=floor)

newdata$OnlyRec[(newdata$QEng == "Engaged")&(newdata$Category == "Recognition")]<-"Engaged"
newdata$OnlyRec[(newdata$QEng == "Neutral")&(newdata$Category == "Recognition")]<-"Neutral"
newdata$OnlyRec[(newdata$QEng == "Disengaged")&(newdata$Category == "Recognition")]<-"Disengaged"

va <- sum(newdata$OnlyRec == "Engaged", na.rm = TRUE)
vb <- sum(newdata$OnlyRec == "Neutral", na.rm = TRUE)
vc <- sum(newdata$OnlyRec == "Disengaged", na.rm = TRUE)

AllCompany.Recognition <- plyr::round_any(100 * (va - vc)/(va + vb + vc), accuracy=1, f=floor)

newdata$OnlyTea[(newdata$QEng == "Engaged")&(newdata$Category == "Teamwork")]<-"Engaged"
newdata$OnlyTea[(newdata$QEng == "Neutral")&(newdata$Category == "Teamwork")]<-"Neutral"
newdata$OnlyTea[(newdata$QEng == "Disengaged")&(newdata$Category == "Teamwork")]<-"Disengaged"

va <- sum(newdata$OnlyTea == "Engaged", na.rm = TRUE)
vb <- sum(newdata$OnlyTea == "Neutral", na.rm = TRUE)
vc <- sum(newdata$OnlyTea == "Disengaged", na.rm = TRUE)

AllCompany.Teamwork <- plyr::round_any(100 * (va - vc)/(va + vb + vc), accuracy=1, f=floor)

AllCompanyColHead <- c('Mood', 'Leadership')
AllCompanyColBody <- c(AllCompany.Mood, AllCompany.Leadership)
# AllCompanyColHead <- c('Mood', 'Engagement', 'Career Development', 'Leadership', 'Mission', 'Ownership', 'Recognition', 'Teamwork')
# AllCompanyColBody <- c(AllCompany.Mood, AllCompany.Engagement, AllCompany.Career_Development, AllCompany.Leadership, AllCompany.Mission, AllCompany.Ownership, AllCompany.Recognition, AllCompany.Teamwork)

print ("135")
####### !!! #######
AllCompany <- data.frame(AllCompanyColHead, AllCompanyColBody)
####### !!! #######
print ("139")
### Part three:  Slice and dice ###

# Team Leader Chart (need a row of:
# A Person | B Person | C Person #
# A Mood
# A Engage
# A Career Dev...
# eg <- c(A Person, A Mood, A Engage)
# 

# Get Team Leaders with Mood Score Each. 

# First, get each Team Leader with number of engaged/disengaged:

TLmatrix.Mood <- as.data.frame.matrix(xtabs(~Team.Leader + MoodEng, data=newdata))

# Then, calculte Mood NPS for Each and append it as a rounded value:

TLmatrix.Mood <- within(TLmatrix.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Repeat for Engagemnent, Ownership, etc...

# TLmatrix.Engagement <- as.data.frame.matrix(xtabs(~Team.Leader + QEng, data=newdata))
# TLmatrix.Engagement <- within(TLmatrix.Engagement, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# TLmatrix.Ownership <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyOwn, data=newdata))
# TLmatrix.Ownership <- within(TLmatrix.Ownership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# TLmatrix.Career_Development <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyCD, data=newdata))
# TLmatrix.Career_Development <- within(TLmatrix.Career_Development, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

TLmatrix.Leadership <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyLea, data=newdata))
TLmatrix.Leadership <- within(TLmatrix.Leadership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# TLmatrix.Mission <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyMis, data=newdata))
# TLmatrix.Mission <- within(TLmatrix.Mission, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# TLmatrix.Recognition <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyRec, data=newdata))
# TLmatrix.Recognition <- within(TLmatrix.Recognition, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# TLmatrix.Teamwork <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyTea, data=newdata))
# TLmatrix.Teamwork <- within(TLmatrix.Teamwork, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print ("182")
TLmatrix <- data.frame(TLmatrix.Mood$NPS, TLmatrix.Leadership$NPS)
# TLmatrix <- data.frame(TLmatrix.Mood$NPS, TLmatrix.Engagement$NPS, TLmatrix.Career_Development$NPS, TLmatrix.Leadership$NPS, TLmatrix.Mission$NPS, TLmatrix.Ownership$NPS, TLmatrix.Recognition$NPS, TLmatrix.Teamwork$NPS)
row.names(TLmatrix) <- row.names(TLmatrix.Mood)
colnames(TLmatrix)[colnames(TLmatrix)=="TLmatrix.Mood.NPS"] <- "Mood"
# colnames(TLmatrix)[colnames(TLmatrix)=="TLmatrix.Engagement.NPS"] <- "Engagement"
# colnames(TLmatrix)[colnames(TLmatrix)=="TLmatrix.Career_Development.NPS"] <- "Career Development"
colnames(TLmatrix)[colnames(TLmatrix)=="TLmatrix.Leadership.NPS"] <- "Leadership"
# colnames(TLmatrix)[colnames(TLmatrix)=="TLmatrix.Mission.NPS"] <- "Mission"
# colnames(TLmatrix)[colnames(TLmatrix)=="TLmatrix.Ownership.NPS"] <- "Ownership"
# colnames(TLmatrix)[colnames(TLmatrix)=="TLmatrix.Recognition.NPS"] <- "Recognition"
# colnames(TLmatrix)[colnames(TLmatrix)=="TLmatrix.Teamwork.NPS"] <- "Teamwork"
print ("194")
TLmatrix <- data.frame(t(TLmatrix))
print ("196")
colnames(TLmatrix) <- row.names(TLmatrix.Mood)
TLmatrix$`Company Average` <- AllCompany$AllCompanyColBody
## Job Titles
print ("200")
JobTitleMatrix.Mood <- as.data.frame.matrix(xtabs(~Job.Title + MoodEng, data=newdata))
JobTitlematrix.Mood <- within(JobTitleMatrix.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print ("206")
# JobTitlematrix.Engagement <- as.data.frame.matrix(xtabs(~Job.Title + QEng, data=newdata))
# JobTitlematrix.Engagement <- within(JobTitlematrix.Engagement, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# JobTitlematrix.Ownership <- as.data.frame.matrix(xtabs(~Job.Title + OnlyOwn, data=newdata))
# JobTitlematrix.Ownership <- within(JobTitlematrix.Ownership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# JobTitlematrix.Career_Development <- as.data.frame.matrix(xtabs(~Job.Title + OnlyCD, data=newdata))
# JobTitlematrix.Career_Development <- within(JobTitlematrix.Career_Development, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print ("215")
JobTitlematrix.Leadership <- as.data.frame.matrix(xtabs(~Job.Title + OnlyLea, data=newdata))
JobTitlematrix.Leadership <- within(JobTitlematrix.Leadership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# JobTitlematrix.Mission <- as.data.frame.matrix(xtabs(~Job.Title + OnlyMis, data=newdata))
# JobTitlematrix.Mission <- within(JobTitlematrix.Mission, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# JobTitlematrix.Recognition <- as.data.frame.matrix(xtabs(~Job.Title + OnlyRec, data=newdata))
# JobTitlematrix.Recognition <- within(JobTitlematrix.Recognition, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# JobTitlematrix.Teamwork <- as.data.frame.matrix(xtabs(~Job.Title + OnlyTea, data=newdata))
# JobTitlematrix.Teamwork <- within(JobTitlematrix.Teamwork, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print ("227")
# JobTitlematrix <- data.frame(JobTitlematrix.Mood$NPS, JobTitlematrix.Engagement$NPS, JobTitlematrix.Career_Development$NPS, JobTitlematrix.Leadership$NPS, JobTitlematrix.Mission$NPS, JobTitlematrix.Ownership$NPS, JobTitlematrix.Recognition$NPS, JobTitlematrix.Teamwork$NPS)
JobTitlematrix <- data.frame(JobTitlematrix.Mood$NPS, JobTitlematrix.Leadership$NPS)
row.names(JobTitlematrix) <- row.names(JobTitlematrix.Mood)
colnames(JobTitlematrix)[colnames(JobTitlematrix)=="JobTitlematrix.Mood.NPS"] <- "Mood"
# colnames(JobTitlematrix)[colnames(JobTitlematrix)=="JobTitlematrix.Engagement.NPS"] <- "Engagement"
# colnames(JobTitlematrix)[colnames(JobTitlematrix)=="JobTitlematrix.Career_Development.NPS"] <- "Career Development"
colnames(JobTitlematrix)[colnames(JobTitlematrix)=="JobTitlematrix.Leadership.NPS"] <- "Leadership"
# colnames(JobTitlematrix)[colnames(JobTitlematrix)=="JobTitlematrix.Mission.NPS"] <- "Mission"
# colnames(JobTitlematrix)[colnames(JobTitlematrix)=="JobTitlematrix.Ownership.NPS"] <- "Ownership"
# colnames(JobTitlematrix)[colnames(JobTitlematrix)=="JobTitlematrix.Recognition.NPS"] <- "Recognition"
# colnames(JobTitlematrix)[colnames(JobTitlematrix)=="JobTitlematrix.Teamwork.NPS"] <- "Teamwork"
JobTitlematrix <- data.frame(t(JobTitlematrix))
colnames(JobTitlematrix) <- row.names(JobTitlematrix.Mood)
JobTitlematrix$`Company Average` <- AllCompany$AllCompanyColBody

## Location
print ("244")
LocMatrix.Mood <- as.data.frame.matrix(xtabs(~Location + MoodEng, data=newdata))
LocMatrix.Mood <- within(LocMatrix.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print ("247")
# LocMatrix.Engagement <- as.data.frame.matrix(xtabs(~Location + QEng, data=newdata))
# LocMatrix.Engagement <- within(LocMatrix.Engagement, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# LocMatrix.Ownership <- as.data.frame.matrix(xtabs(~Location + OnlyOwn, data=newdata))
# LocMatrix.Ownership <- within(LocMatrix.Ownership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# LocMatrix.Career_Development <- as.data.frame.matrix(xtabs(~Location + OnlyCD, data=newdata))
# LocMatrix.Career_Development <- within(LocMatrix.Career_Development, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print ("256")
LocMatrix.Leadership <- as.data.frame.matrix(xtabs(~Location + OnlyLea, data=newdata))
LocMatrix.Leadership <- within(LocMatrix.Leadership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# LocMatrix.Mission <- as.data.frame.matrix(xtabs(~Location + OnlyMis, data=newdata))
# LocMatrix.Mission <- within(LocMatrix.Mission, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# LocMatrix.Recognition <- as.data.frame.matrix(xtabs(~Location + OnlyRec, data=newdata))
# LocMatrix.Recognition <- within(LocMatrix.Recognition, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# LocMatrix.Teamwork <- as.data.frame.matrix(xtabs(~Location + OnlyTea, data=newdata))
# LocMatrix.Teamwork <- within(LocMatrix.Teamwork, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
#
# LocMatrix <- data.frame(LocMatrix.Mood$NPS, LocMatrix.Engagement$NPS, LocMatrix.Career_Development$NPS, LocMatrix.Leadership$NPS, LocMatrix.Mission$NPS, LocMatrix.Ownership$NPS, LocMatrix.Recognition$NPS, LocMatrix.Teamwork$NPS)
LocMatrix <- data.frame(LocMatrix.Mood$NPS, LocMatrix.Leadership$NPS)
row.names(LocMatrix) <- row.names(LocMatrix.Mood)
colnames(LocMatrix)[colnames(LocMatrix)=="LocMatrix.Mood.NPS"] <- "Mood"
# colnames(LocMatrix)[colnames(LocMatrix)=="LocMatrix.Engagement.NPS"] <- "Engagement"
# colnames(LocMatrix)[colnames(LocMatrix)=="LocMatrix.Career_Development.NPS"] <- "Career Development"
colnames(LocMatrix)[colnames(LocMatrix)=="LocMatrix.Leadership.NPS"] <- "Leadership"
# colnames(LocMatrix)[colnames(LocMatrix)=="LocMatrix.Mission.NPS"] <- "Mission"
# colnames(LocMatrix)[colnames(LocMatrix)=="LocMatrix.Ownership.NPS"] <- "Ownership"
# colnames(LocMatrix)[colnames(LocMatrix)=="LocMatrix.Recognition.NPS"] <- "Recognition"
# colnames(LocMatrix)[colnames(LocMatrix)=="LocMatrix.Teamwork.NPS"] <- "Teamwork"
LocMatrix <- data.frame(t(LocMatrix))
colnames(LocMatrix) <- row.names(LocMatrix.Mood)
LocMatrix$`Company Average` <- AllCompany$AllCompanyColBody

## Employee Level

ELevel.Mood <- as.data.frame.matrix(xtabs(~Employee.Level + MoodEng, data=newdata))
ELevel.Mood <- within(ELevel.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Engagement <- as.data.frame.matrix(xtabs(~Employee.Level + QEng, data=newdata))
# ELevel.Engagement <- within(ELevel.Engagement, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Ownership <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyOwn, data=newdata))
# ELevel.Ownership <- within(ELevel.Ownership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Career_Development <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyCD, data=newdata))
# ELevel.Career_Development <- within(ELevel.Career_Development, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

ELevel.Leadership <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyLea, data=newdata))
ELevel.Leadership <- within(ELevel.Leadership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Mission <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyMis, data=newdata))
# ELevel.Mission <- within(ELevel.Mission, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Recognition <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyRec, data=newdata))
# ELevel.Recognition <- within(ELevel.Recognition, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Teamwork <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyTea, data=newdata))
# ELevel.Teamwork <- within(ELevel.Teamwork, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print("304")
# ELevel <- data.frame(ELevel.Mood$NPS, ELevel.Engagement$NPS, ELevel.Career_Development$NPS, ELevel.Leadership$NPS, ELevel.Mission$NPS, ELevel.Ownership$NPS, ELevel.Recognition$NPS, ELevel.Teamwork$NPS)
ELevel <- data.frame(ELevel.Mood$NPS, ELevel.Leadership$NPS)
row.names(ELevel) <- row.names(ELevel.Mood)
colnames(ELevel)[colnames(ELevel)=="ELevel.Mood.NPS"] <- "Mood"
# colnames(ELevel)[colnames(ELevel)=="ELevel.Engagement.NPS"] <- "Engagement"
# colnames(ELevel)[colnames(ELevel)=="ELevel.Career_Development.NPS"] <- "Career Development"
colnames(ELevel)[colnames(ELevel)=="ELevel.Leadership.NPS"] <- "Leadership"
# colnames(ELevel)[colnames(ELevel)=="ELevel.Mission.NPS"] <- "Mission"
# colnames(ELevel)[colnames(ELevel)=="ELevel.Ownership.NPS"] <- "Ownership"
# colnames(ELevel)[colnames(ELevel)=="ELevel.Recognition.NPS"] <- "Recognition"
# colnames(ELevel)[colnames(ELevel)=="ELevel.Teamwork.NPS"] <- "Teamwork"
ELevel <- data.frame(t(ELevel))
colnames(ELevel) <- row.names(ELevel.Mood)
ELevel$`Company Average` <- AllCompany$AllCompanyColBody

## Employee Level

Tenure.Mood <- as.data.frame.matrix(xtabs(~Tenure + MoodEng, data=newdata))
Tenure.Mood <- within(Tenure.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Engagement <- as.data.frame.matrix(xtabs(~Tenure + QEng, data=newdata))
# Tenure.Engagement <- within(Tenure.Engagement, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Ownership <- as.data.frame.matrix(xtabs(~Tenure + OnlyOwn, data=newdata))
# Tenure.Ownership <- within(Tenure.Ownership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Career_Development <- as.data.frame.matrix(xtabs(~Tenure + OnlyCD, data=newdata))
# Tenure.Career_Development <- within(Tenure.Career_Development, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

Tenure.Leadership <- as.data.frame.matrix(xtabs(~Tenure + OnlyLea, data=newdata))
Tenure.Leadership <- within(Tenure.Leadership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Mission <- as.data.frame.matrix(xtabs(~Tenure + OnlyMis, data=newdata))
# Tenure.Mission <- within(Tenure.Mission, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Recognition <- as.data.frame.matrix(xtabs(~Tenure + OnlyRec, data=newdata))
# Tenure.Recognition <- within(Tenure.Recognition, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Teamwork <- as.data.frame.matrix(xtabs(~Tenure + OnlyTea, data=newdata))
# Tenure.Teamwork <- within(Tenure.Teamwork, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
#
# Tenure <- data.frame(Tenure.Mood$NPS, Tenure.Engagement$NPS, Tenure.Career_Development$NPS, Tenure.Leadership$NPS, Tenure.Mission$NPS, Tenure.Ownership$NPS, Tenure.Recognition$NPS, Tenure.Teamwork$NPS)
Tenure <- data.frame(Tenure.Mood$NPS, Tenure.Leadership$NPS)
row.names(Tenure) <- row.names(Tenure.Mood)
colnames(Tenure)[colnames(Tenure)=="Tenure.Mood.NPS"] <- "Mood"
# colnames(Tenure)[colnames(Tenure)=="Tenure.Engagement.NPS"] <- "Engagement"
# colnames(Tenure)[colnames(Tenure)=="Tenure.Career_Development.NPS"] <- "Career Development"
colnames(Tenure)[colnames(Tenure)=="Tenure.Leadership.NPS"] <- "Leadership"
# colnames(Tenure)[colnames(Tenure)=="Tenure.Mission.NPS"] <- "Mission"
# colnames(Tenure)[colnames(Tenure)=="Tenure.Ownership.NPS"] <- "Ownership"
# colnames(Tenure)[colnames(Tenure)=="Tenure.Recognition.NPS"] <- "Recognition"
# colnames(Tenure)[colnames(Tenure)=="Tenure.Teamwork.NPS"] <- "Teamwork"
Tenure <- data.frame(t(Tenure))
colnames(Tenure) <- row.names(Tenure.Mood)
Tenure$`Company Average` <- AllCompany$AllCompanyColBody

# Q1 Score
Q1Score <- count(newdata$Q1)
# Q2 Score
Q2Score <- count(newdata$Q2)
# Q3 Score
Q3Score <- count(newdata$Q3)

QScores <- data.frame(Q1Score$x, Q1Score$freq, Q2Score$freq, Q3Score$freq)

colnames(QScores)[colnames(QScores)=="Q1Score.freq"] <- "Q1"
colnames(QScores)[colnames(QScores)=="Q2Score.freq"] <- "Q2"
colnames(QScores)[colnames(QScores)=="Q3Score.freq"] <- "Q3"
colnames(QScores)[colnames(QScores)=="Q1Score.x"] <- "Category"




######  !!!  ######
View(TLmatrix)
View(JobTitlematrix)
View(LocMatrix)
View(ELevel)
View(Tenure)
View(QScores)
######  !!!  ######

# Part Four: 
# By Question 1, 2, 3:  

# Add row for Q1Eng, Q2Eng, and Q3Eng

# Aggregate Q1 Scores:



# by Job Title, by location, by team leader, by employee level, by employee status(???), by time in current job ()
# by 

# Part four:  Top Positive / Negative Comments

# Part five:  Slice comments by team leader
