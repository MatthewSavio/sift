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
LB <- 3.5 ######
MB <- 5.5 ######
UB <- 8   ######
################

NewCol <- function(df, colname="NewCol", fill=NA) {
	df[,colname] <- fill
	return (df)
}

NewRow <- function(df, rowname="NewRow", fill=NA) {
	df[rowname,] <- fill
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

# If a Column Header is missing, create a blank column with that header.
# example:  TestMissingCol(df, "Engaged")
TestMissingCol <- function(df, colname) {
	if (!(colname %in% colnames(df))) {
		df <- NewCol(df, colname, 0)
	}
	return (df)
}

TestMissingRow <- function(df, rowname) {
	if (!(rowname %in% row.names(df))) {
		df <- NewRow(df, rowname, 0)
	}
	return (df)
}

# Accepts Columns as Arguments, makes a separate table
# example:   NPSTable <- MakeNPSTable(df, df$Team.Leader, df$NewNPSCol)
MakeNPSTable <- function(df, GroupCol, EngagementCol) {
	df <- MakeCrossTable(df, GroupCol, EngagementCol)
	df <- TestMissingCol(df, "Engaged")
	df <- TestMissingCol(df, "Neutral")
	df <- TestMissingCol(df, "Disengaged")
	df <- within(df, NPS <- plyr::round_any(100 * (Engaged - Disengaged)/(Engaged + Neutral + Disengaged), accuracy=1, f=floor))
	return (df)
}

# Requests file name of CSV in folder and imports it to a var
# example:     df <- ImportFile()
ImportFile <- function(filename) {
	df <- read.csv(filename, header = TRUE)
	return (df)
}

# Exports dataframe to CSV with datestamp
# example:    ExportTable(df, "finaltable")
ExportTable <- function(df, filename="csvexport") {
	filename <- gsub("[?().,@~`^&*{}<>#;!¡¿%$·']", "", filename)
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

NewCategoryCol <- function(df, categoryname, colname, question="QAvg", catcol=category) {
	df[[colname]][(df[[question]] == "Engaged")&(df[[catcol]] == categoryname)]<-"Engaged"
	df[[colname]][(df[[question]] == "Neutral")&(df[[catcol]] == categoryname)]<-"Neutral"
	df[[colname]][(df[[question]] == "Disengaged")&(df[[catcol]] == categoryname)]<-"Disengaged"
	return(df)
}

TestTenureRows <- function(CategoryTable) {
	CategoryTable <- TestMissingRow(CategoryTable, "< 1 Year")
	CategoryTable <- TestMissingRow(CategoryTable, "1-2 Years")
	CategoryTable <- TestMissingRow(CategoryTable, "2-3 Years")
	CategoryTable <- TestMissingRow(CategoryTable, "3+ Years")
}


dfCategoryBy <- function(df, categoryby=teamleader, ColMood="MoodEng", ColCD="OnlyCD", ColLead="OnlyLead", ColMiss="OnlyMiss", ColOwn="OnlyOwn", ColRec="OnlyRec", ColTeam="OnlyTeam") {
	Mood <- MakeNPSTable(df, df[[categoryby]], df[[ColMood]])
	CareerDev <- MakeNPSTable(df, df[[categoryby]], df[[ColCD]])
	Leadership <- MakeNPSTable(df, df[[categoryby]], df[[ColLead]])
	Mission <- MakeNPSTable(df, df[[categoryby]], df[[ColMiss]])
	Ownership <- MakeNPSTable(df, df[[categoryby]], df[[ColOwn]])
	Recognition <- MakeNPSTable(df, df[[categoryby]], df[[ColRec]])
	Teamwork <- MakeNPSTable(df, df[[categoryby]], df[[ColTeam]])

	if (categoryby=="Tenure") {
		Mood <- TestTenureRows(Mood)
		CareerDev <- TestTenureRows(CareerDev)
		Leadership <- TestTenureRows(Leadership)
		Mission <- TestTenureRows(Mission)
		Ownership <- TestTenureRows(Ownership)
		Recognition <- TestTenureRows(Recognition)
		Teamwork <- TestTenureRows(Teamwork)
	}
	cattable <- data.frame(Mood$NPS, CareerDev$NPS, Leadership$NPS, Mission$NPS, Ownership$NPS, Recognition$NPS, Teamwork$NPS)
	colnames(cattable) <- c("Mood", "Career_Development", "Leadership", "Mission", "Ownership", "Recognition", "Teamwork")
	cattable <- data.frame(t(cattable))
	colnames(cattable) <- row.names(Mood)
	return(cattable)
}




##### Get CrossTable

# Get function for combining Meridian's CSV sheets on top of each other.


#####################


# Part one: Import new datafile
filename <- readline(prompt="Enter a file name: ")
df <- ImportFile(filename)

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
	teamleader <- "teamLeader"
	jobtitle <- "title"
	team <- "subTeam"
}

if (CompName == "Meridian") {
	dateformat <- "%m/%d/%y"
}


#Add column with days since start date at job
df$DaysInJob <- (Sys.Date() - as.Date(as.character(df[[TenureDaysCol]]), format=dateformat))# 

# Add column with 'Less than one year' and 'Greater than one year'
df$YearsInJob <- floor(df$DaysInJob / 365)

df[["Tenure"]][df$YearsInJob < 100] <- "3+ Years"
df[["Tenure"]][df$YearsInJob < 3] <- "2-3 Years"
df[["Tenure"]][df$YearsInJob < 2] <- "1-2 Years"
df[["Tenure"]][df$YearsInJob < 1] <- "< 1 Year"


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
df <- NewCategoryCol(df, "Ownership", "OnlyOwn", )
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

TL_Table <- dfCategoryBy(df, teamleader)
JobTitle_Table <- dfCategoryBy(df, jobtitle)
Team_Table <- dfCategoryBy(df, team)
# Tenure_Table <- dfCategoryBy(df, "Tenure")
### NOTE:  Tenure is Broken.

## NOTE:  Add Company Averages as a column to the beginning / end here.

### Now Get Team Leader Results Per Question in Each Category


# NOTE:  Then, calculte Mood NPS for Each and append it as a rounded value:

#### Last Thing, Questions per TL per Category.

### Create Q1/Q2/Q3 Cols for each category

# Breakdown Questions by TL.

# Category Question Rows

df <- NewCategoryCol(df, "Ownership", "Q1Own", "Q1")
df <- NewCategoryCol(df, "Career Development", "Q1CD", "Q1")
df <- NewCategoryCol(df, "Leadership", "Q1Lead", "Q1")
df <- NewCategoryCol(df, "Mission", "Q1Miss", "Q1")
df <- NewCategoryCol(df, "Recognition", "Q1Rec", "Q1")
df <- NewCategoryCol(df, "Teamwork", "Q1Team", "Q1")

df <- NewCategoryCol(df, "Ownership", "Q2Own", "Q2")
df <- NewCategoryCol(df, "Career Development", "Q2CD", "Q2")
df <- NewCategoryCol(df, "Leadership", "Q2Lead", "Q2")
df <- NewCategoryCol(df, "Mission", "Q2Miss", "Q2")
df <- NewCategoryCol(df, "Recognition", "Q2Rec", "Q2")
df <- NewCategoryCol(df, "Teamwork", "Q2Team", "Q2")

df <- NewCategoryCol(df, "Career Development", "Q3CD", "Q3")
df <- NewCategoryCol(df, "Leadership", "Q3Lead", "Q3")
df <- NewCategoryCol(df, "Mission", "Q3Miss", "Q3")
df <- NewCategoryCol(df, "Ownership", "Q3Own", "Q3")
df <- NewCategoryCol(df, "Recognition", "Q3Rec", "Q3")
df <- NewCategoryCol(df, "Teamwork", "Q3Team", "Q3")

Q1_Table <- dfCategoryBy(df, teamleader, "MoodEng", "Q1CD", "Q1Lead", "Q1Miss", "Q1Own", "Q1Rec", "Q1Team")
Q2_Table <- dfCategoryBy(df, teamleader, "MoodEng", "Q2CD", "Q2Lead", "Q2Miss", "Q2Own", "Q2Rec", "Q2Team")
Q3_Table <- dfCategoryBy(df, teamleader, "MoodEng", "Q3CD", "Q3Lead", "Q3Miss", "Q3Own", "Q3Rec", "Q3Team")

#####

df <- NewCategoryCol(df, "Ownership", "Mood-Wk3", "MoodEng")
df <- NewCategoryCol(df, "Career Development", "Mood-Wk5", "MoodEng")
df <- NewCategoryCol(df, "Leadership", "Mood-Wk1", "MoodEng")
df <- NewCategoryCol(df, "Mission", "Mood-Wk2", "MoodEng")
df <- NewCategoryCol(df, "Recognition", "Mood-Wk6", "MoodEng")
df <- NewCategoryCol(df, "Teamwork", "Mood-Wk4", "MoodEng")



dE <- "No"
dE <- readline(prompt="Export CSVs?: ")

if ((dE == "yes")|(dE == "Yes")|(dE == "YES")|(dE == "Y")|(dE == "y")|(dE == "TRUE")) {
	filenameTL <- paste(filename, "_", "TL_Table", sep="")
	ExportTable(TL_Table, filenameTL)

	filenameJobTitle <- paste(filename, "_", "JobTitle_Table", sep="")
	ExportTable(JobTitle_Table, filenameJobTitle)

	filenameTeam <- paste(filename, "_", "Team_Table", sep="")
	ExportTable(Team_Table, filenameTeam)

	# filenameTenure <- paste(filename, "_", "Tenure_Table", sep="")
	# ExportTable(Tenure_Table, filenameTenure)

	filenameQ1 <- paste(filename, "_", "Q1_Table", sep="")
	ExportTable(Q1_Table, filenameQ1)

	filenameQ2 <- paste(filename, "_", "Q2_Table", sep="")
	ExportTable(Q2_Table, filenameQ2)

	filenameQ3 <- paste(filename, "_", "Q3_Table", sep="")
	ExportTable(Q3_Table, filenameQ3)
}


######  !!!  ######

# Part Four: 
# By Question 1, 2, 3:  

# Add row for Q1Eng, Q2Eng, and Q3Eng

# Aggregate Q1 Scores:



# by Job Title, by location, by team leader, by employee level, by employee status(???), by time in current job ()
# by 

# Part four:  Top Positive / Negative Comments

# Part five:  Slice comments by team leader
