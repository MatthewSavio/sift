# 2.21.17 sync

# Calculate Roll-up and Roll-down reports from Meridian Data

library(plyr)
library(dplyr)

#### Functions:  ######## 

##### Import Datafile Function given file name


##### Check Columns for common types, confirm with User

##### Calculate NPS from a column of numbers
# Sum up Disengaged answers, Neutral answers, Engaged answers and calculate Engagement NPS from those as different answers
# Inputs:
# Datatable "df"
# New Column Name  "MoodEng"
# Column with Scores "Mood.Score"
# Upper Bounds "8"
# Neutral Bounds "5.5"
# Lower Bounds "3.5"
# TLmatrix.Mood <- within(TLmatrix.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

########## Initialize Bounds ##########
LB <- 3.5
MB <- 5.5
UB <- 8


NewCol <- function(df, colname="NewCol") {
	df[,colname] <- NA
	return (df)
}

NPSCol <- function(df, ScoreColStr, EmptyColStr, LBound=LB, MBound=MB, UBound=UB) {
	df[[EmptyColStr]][df[[ScoreColStr]] < UBound] <- "Engaged"
	df[[EmptyColStr]][df[[ScoreColStr]] < MBound] <- "Neutral"
	df[[EmptyColStr]][df[[ScoreColStr]] < LBound] <- "Disengaged"
	return (df)
}

MakeCrossTable <- function(df, rows, columns) {
	df <- as.data.frame.matrix(xtabs(~rows + columns, data=df))
	return (df)
}

# Appends an NPS Column to a dataframe.  Accepts Col Name Strings are Arguments
# example:    df <- NewNPSCol(df, "NPS_Score", "NewNPSCol")
NewNPSCol <- function(df, ScoreColStr, colname="NewCol", LBound=LB, MBound=MB, UBound=UB) {
	df <- NewCol(df, colname)
	df <- NPSCol(df, ScoreColStr, colname, LBound, MBound, UBound)
	return (df)
}

# Accepts Columns as Arguments, makes a separate table
# example:   NPSTable <- MakeNPSTable(df, df$Team.Leader, df$NewNPSCol)
MakeNPSTable <- function(df, GroupCol, EngagementCol) {
	df <- MakeCrossTable(df, GroupCol, EngagementCol)
	df <- within(df, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
	return (df)
}

# Requests file name of CSV in folder and imports it to a var
# example:     df <- ImportFile()
ImportFile <- function() {
	filename <- readline(prompt="Enter a file name: ")
	df <- read.csv(filename, header = TRUE)
	return (df)
}

# Exports dataframe to CSV with datestamp
# example:    ExportTable(df, "finaltable")
ExportTable <- function(df, filename="csvexport") {
	filename <- paste(filename, "_", Sys.Date(), ".csv", sep="") 
	write.csv(df, file=filename)
}

# Asks for the Company to parse the datafile
AskCompName <- function() {
	CompName <- readline(prompt="Company Name? [FOC or Meridian]: ")
	return (CompName)
}

# Calculates vertical company-wide NPS on a column
# example:     CalcColNPS(df, "EngagementCol")
CalcColNPS <- function(df, colname) {
	a <- sum(df[[colname]] == "Engaged", na.rm = TRUE)
	b <- sum(df[[colname]] == "Neutral", na.rm = TRUE)
	c <- sum(df[[colname]] == "Disengaged", na.rm = TRUE)
	ColNPS <- plyr::round_any(100 * (a - c)/(a + b + c), accuracy=1, f=round)
	return (ColNPS)
}

# Adds a new column with only Engagement of a Category

NewCategoryCol <- function(df, categoryname, colname, catcol=category) {
	df[[colname]][(df$QAvg == "Engaged")&(df[[catcol]] == categoryname)]<-"Engaged"
	df[[colname]][(df$QAvg == "Neutral")&(df[[catcol]] == categoryname)]<-"Neutral"
	df[[colname]][(df$QAvg == "Disengaged")&(df[[catcol]] == categoryname)]<-"Disengaged"
	return(df)
}


##### Get CrossTable

# Get function for combining Meridian's CSV sheets on top of each other.


#####################


# Part one: Import new datafile
df <- ImportFile()

# Part two:  Parse File Headers
CompName <- AskCompName()

# Initialize Column Headers

if (CompName == "FOC") {
	TenureDaysCol <- "anniversaryDate"
	MoodScore <- "mood_score"
	Q1Score <- "category_question_1_score"
	Q2Score <- "category_question_2_score"
	Q3Score <- "category_question_3_score"
	dateformat <- "%m/%d/%Y"
	category <- "survey_category"
}

if (CompName == "Meridian") {
	dateformat <- "%m/%d/%y"
}


#Add column with days since start date at job
df$DaysInJob <- (Sys.Date() - as.Date(as.character(df[[TenureDaysCol]]), format=dateformat))# 

# Add column with 'Less than one year' and 'Greater than one year'
df$Tenure <- floor(df$DaysInJob / 365)

print("19")

#Add Mood Engagement Row
df <- NewNPSCol(df, MoodScore, "MoodEng")

#Add Q1 Row
df <- NewNPSCol(df, Q1Score , "Q1")
df <- NewNPSCol(df, Q2Score , "Q2")
df <- NewNPSCol(df, Q3Score , "Q3")


#Add QAvg Row
df$QAvgScore <- rowMeans(subset(df, select = c(Q1Score, Q2Score, Q3Score)), na.rm = TRUE)

#Add QAvg Engagement Row
df <- NewNPSCol(df, "QAvgScore" , "QAvg")

print("49")
# rounding function --> moodavg <- plyr::round_any(mean(df$`Mood.Score`), accuracy=0.01, f=round)
### Part two: Generate company-wide summaries ###

# Company-wide Mood NPS
CompanyMood <- CalcColNPS(df, "MoodEng")

# Company-wide Engagement NPS (This is only useful for single things, useless currently)
# CompanyQAvg <- CalcColNPS(df, "QAvg")


# Company-wide Categories
### NOTE:  Will have to make this able to work without all categories present
#Add Category Engagement Rows
df <- NewCategoryCol(df, "Ownership", "OnlyOwn")
CompanyOwnership <- CalcColNPS(df, "OnlyOwn")

df <- NewCategoryCol(df, "Career Development", "OnlyCD")
CompanyCareerDevelopment <- CalcColNPS(df, "OnlyCD")

df <- NewCategoryCol(df, "Leadership", "OnlyLead")
CompanyLeadership <- CalcColNPS(df, "OnlyLead")

df <- NewCategoryCol(df, "Mission", "OnlyMiss")
CompanyMission <- CalcColNPS(df, "OnlyMiss")

df <- NewCategoryCol(df, "Recognition", "OnlyRec")
CompanyRecognition <- CalcColNPS(df, "OnlyRec")

df <- NewCategoryCol(df, "Teamwork", "OnlyTeam")
CompanyTeamwork <- CalcColNPS(df, "OnlyTeam")

#### NOTE:  Need a way to combine these into a table.

#AllCompanyColHead <- c('Mood', 'Leadership')
#AllCompanyColBody <- c(AllCompany.Mood, AllCompany.Leadership)
# AllCompanyColHead <- c('Mood', 'Engagement', 'Career Development', 'Leadership', 'Mission', 'Ownership', 'Recognition', 'Teamwork')
# AllCompanyColBody <- c(AllCompany.Mood, AllCompany.Engagement, AllCompany.Career_Development, AllCompany.Leadership, AllCompany.Mission, AllCompany.Ownership, AllCompany.Recognition, AllCompany.Teamwork)

print ("135")
####### !!! #######
#AllCompany <- data.frame(AllCompanyColHead, AllCompanyColBody)
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

##### NOTE:  Individual Questions for each cycle Score

##### NOTE:  Team Leaders for each cycle score

##### NOTE:  Tenure by

##### NOTE:  Title by 

##### NOTE:  subTeam by

# Get Team Leaders with Mood Score Each. 

# First, get each Team Leader with number of engaged/disengaged:

TLmatrix.Mood <- as.data.frame.matrix(xtabs(~Team.Leader + MoodEng, data=df))

# Then, calculte Mood NPS for Each and append it as a rounded value:

TLmatrix.Mood <- within(TLmatrix.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Repeat for Engagemnent, Ownership, etc...

# TLmatrix.Engagement <- as.data.frame.matrix(xtabs(~Team.Leader + QEng, data=df))
# TLmatrix.Engagement <- within(TLmatrix.Engagement, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# TLmatrix.Ownership <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyOwn, data=df))
# TLmatrix.Ownership <- within(TLmatrix.Ownership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# TLmatrix.Career_Development <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyCD, data=df))
# TLmatrix.Career_Development <- within(TLmatrix.Career_Development, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

TLmatrix.Leadership <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyLea, data=df))
TLmatrix.Leadership <- within(TLmatrix.Leadership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# TLmatrix.Mission <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyMis, data=df))
# TLmatrix.Mission <- within(TLmatrix.Mission, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# TLmatrix.Recognition <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyRec, data=df))
# TLmatrix.Recognition <- within(TLmatrix.Recognition, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# TLmatrix.Teamwork <- as.data.frame.matrix(xtabs(~Team.Leader + OnlyTea, data=df))
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
JobTitleMatrix.Mood <- as.data.frame.matrix(xtabs(~Job.Title + MoodEng, data=df))
JobTitlematrix.Mood <- within(JobTitleMatrix.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print ("206")
# JobTitlematrix.Engagement <- as.data.frame.matrix(xtabs(~Job.Title + QEng, data=df))
# JobTitlematrix.Engagement <- within(JobTitlematrix.Engagement, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# JobTitlematrix.Ownership <- as.data.frame.matrix(xtabs(~Job.Title + OnlyOwn, data=df))
# JobTitlematrix.Ownership <- within(JobTitlematrix.Ownership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# JobTitlematrix.Career_Development <- as.data.frame.matrix(xtabs(~Job.Title + OnlyCD, data=df))
# JobTitlematrix.Career_Development <- within(JobTitlematrix.Career_Development, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print ("215")
JobTitlematrix.Leadership <- as.data.frame.matrix(xtabs(~Job.Title + OnlyLea, data=df))
JobTitlematrix.Leadership <- within(JobTitlematrix.Leadership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# JobTitlematrix.Mission <- as.data.frame.matrix(xtabs(~Job.Title + OnlyMis, data=df))
# JobTitlematrix.Mission <- within(JobTitlematrix.Mission, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# JobTitlematrix.Recognition <- as.data.frame.matrix(xtabs(~Job.Title + OnlyRec, data=df))
# JobTitlematrix.Recognition <- within(JobTitlematrix.Recognition, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# JobTitlematrix.Teamwork <- as.data.frame.matrix(xtabs(~Job.Title + OnlyTea, data=df))
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
LocMatrix.Mood <- as.data.frame.matrix(xtabs(~Location + MoodEng, data=df))
LocMatrix.Mood <- within(LocMatrix.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print ("247")
# LocMatrix.Engagement <- as.data.frame.matrix(xtabs(~Location + QEng, data=df))
# LocMatrix.Engagement <- within(LocMatrix.Engagement, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# LocMatrix.Ownership <- as.data.frame.matrix(xtabs(~Location + OnlyOwn, data=df))
# LocMatrix.Ownership <- within(LocMatrix.Ownership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# LocMatrix.Career_Development <- as.data.frame.matrix(xtabs(~Location + OnlyCD, data=df))
# LocMatrix.Career_Development <- within(LocMatrix.Career_Development, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
print ("256")
LocMatrix.Leadership <- as.data.frame.matrix(xtabs(~Location + OnlyLea, data=df))
LocMatrix.Leadership <- within(LocMatrix.Leadership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# LocMatrix.Mission <- as.data.frame.matrix(xtabs(~Location + OnlyMis, data=df))
# LocMatrix.Mission <- within(LocMatrix.Mission, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# LocMatrix.Recognition <- as.data.frame.matrix(xtabs(~Location + OnlyRec, data=df))
# LocMatrix.Recognition <- within(LocMatrix.Recognition, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# LocMatrix.Teamwork <- as.data.frame.matrix(xtabs(~Location + OnlyTea, data=df))
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

ELevel.Mood <- as.data.frame.matrix(xtabs(~Employee.Level + MoodEng, data=df))
ELevel.Mood <- within(ELevel.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Engagement <- as.data.frame.matrix(xtabs(~Employee.Level + QEng, data=df))
# ELevel.Engagement <- within(ELevel.Engagement, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Ownership <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyOwn, data=df))
# ELevel.Ownership <- within(ELevel.Ownership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Career_Development <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyCD, data=df))
# ELevel.Career_Development <- within(ELevel.Career_Development, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

ELevel.Leadership <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyLea, data=df))
ELevel.Leadership <- within(ELevel.Leadership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Mission <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyMis, data=df))
# ELevel.Mission <- within(ELevel.Mission, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Recognition <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyRec, data=df))
# ELevel.Recognition <- within(ELevel.Recognition, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# ELevel.Teamwork <- as.data.frame.matrix(xtabs(~Employee.Level + OnlyTea, data=df))
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

Tenure.Mood <- as.data.frame.matrix(xtabs(~Tenure + MoodEng, data=df))
Tenure.Mood <- within(Tenure.Mood, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Engagement <- as.data.frame.matrix(xtabs(~Tenure + QEng, data=df))
# Tenure.Engagement <- within(Tenure.Engagement, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Ownership <- as.data.frame.matrix(xtabs(~Tenure + OnlyOwn, data=df))
# Tenure.Ownership <- within(Tenure.Ownership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Career_Development <- as.data.frame.matrix(xtabs(~Tenure + OnlyCD, data=df))
# Tenure.Career_Development <- within(Tenure.Career_Development, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

Tenure.Leadership <- as.data.frame.matrix(xtabs(~Tenure + OnlyLea, data=df))
Tenure.Leadership <- within(Tenure.Leadership, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Mission <- as.data.frame.matrix(xtabs(~Tenure + OnlyMis, data=df))
# Tenure.Mission <- within(Tenure.Mission, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Recognition <- as.data.frame.matrix(xtabs(~Tenure + OnlyRec, data=df))
# Tenure.Recognition <- within(Tenure.Recognition, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))

# Tenure.Teamwork <- as.data.frame.matrix(xtabs(~Tenure + OnlyTea, data=df))
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
Q1Score <- count(df$Q1)
# Q2 Score
Q2Score <- count(df$Q2)
# Q3 Score
Q3Score <- count(df$Q3)

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
