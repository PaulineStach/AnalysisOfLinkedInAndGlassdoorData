#Dataset 1: LinkedIn______________________________________________________________________________
setwd("C:/Users/pauli/Documents/NCI/Programming for Big Data/Project/Dataset1")

#loading data and assigning to variable LinkedIn1
LinkedIn1 <- read.csv(file="linkedin_data.csv", head=TRUE, sep=",")

#first counter column makes every row unique, therefore duplicated rows dont show up
#data frame without first column
LinkedIn1 <- LinkedIn1[,2:52]

#extracting unique rows
LinkedIn1 <- unique(LinkedIn1) #62516 rows

#selection relevant columns
LinkedIn1 <- LinkedIn1[, c("c_name",	"m_urn",	"n_pos",	"n_prev_tenures",	"tenure_len", "gender")]

#renaming columns
names(LinkedIn1) <- c("Company", "User_ID", "N_Pos", "N_Prev_Ten", "Ten_Len", "Gender")

#deleting unnecessary user id  information
LinkedIn1$User_ID <- substring(LinkedIn1$User_ID, 15) #first 15 characters will be cut and replacing Userr_ID column

#calculating tenure from days into months and replacing df column
LinkedIn1$Ten_Len <-round(LinkedIn1$Ten_Len/30.42,0)

#indentifying NA
#one row (21312) has NA for column Company, this is not possible to predict or calculate
#therefore, due to the size of the data set, remove
LinkedIn1[!complete.cases(LinkedIn1),] #checking for na in data frame, with one result
subset(LinkedIn1, User_ID == "3208061") #looking at user more specifically to dedice what to do
LinkedIn1 <- LinkedIn1[!is.na(LinkedIn1$Company),] #filtering row out

#saving cleaned file as csq LinkedIn1
write.csv(LinkedIn1, file = "LinkedIn1.csv") #saving in csv

#_______________________________________________________________________________
#analysing columns
#using str_replace() and regular exoression to find all microsoft-related jobs with "Microsoft" 
#replacing vector in "Company" column
LinkedIn1$Company <- str_replace(LinkedIn1$Company, '^.*icroso.*$', 'Microsoft')
#saving df only with Microsoft rows
Microsoft <- LinkedIn1[grep("icrosof", LinkedIn1$Company), ]#8980

#using str_replace() and regular exoression to find all google-related jobs with "Google" 
#replacing vector in "Company" column
LinkedIn1$Company <- str_replace(LinkedIn1$Company, '^.*oogle.*$', 'Google')
#saving df only with Google rows
Google <- LinkedIn1[grep("Google", LinkedIn1$Company), ]#261

#using str_replace() and regular exoression to find all amazon-related jobs with "Amazon" 
#replacing vector in "Company" column
LinkedIn1$Company <- str_replace(LinkedIn1$Company, '^.*mazon.*$', 'Amazon')
#saving df only with Google rows
Amazon <- LinkedIn1[grep("Amazon", LinkedIn1$Company), ]#366

#using str_replace() and regular exoression to find all facebook-related jobs with "Facebook" 
#replacing vector in "Company" column
LinkedIn1$Company <- str_replace(LinkedIn1$Company, '^.*acebook.*$', 'Facebook')
#saving df only with Google rows
Facebook <- LinkedIn1[grep("Facebook", LinkedIn1$Company), ]#61

#using str_replace() and regular exoression to find all apple-related jobs with "Apple" 
#replacing vector in "Company" column
LinkedIn1$Company <- str_replace(LinkedIn1$Company, '^.*pple.*$', 'Apple')
#saving df only with Google rows
Apple <- LinkedIn1[grep("Apple", LinkedIn1$Company), ]#134

#____________________________________________________________________
#extracting outliers
#Microsoft
#finding outlier values
outlier_values <- boxplot.stats(Microsoft$Ten_Len)$out
#finding in which row outliers are
Microsoft[which(Microsoft$Ten_Len %in% outlier_values),]
#removing the rows with outliers
Microsoft <- Microsoft[-which(Microsoft$Ten_Len %in% outlier_values),] #8477

#Amazon
#finding outlier values
outlier_values <- boxplot.stats(Amazon$Ten_Len)$out
#finding in which row outliers are
Amazon[which(Amazon$Ten_Len %in% outlier_values),]
#removing the rows with outliers
Amazon <- Amazon[-which(Amazon$Ten_Len %in% outlier_values),] #351

#Google
#finding outlier values
outlier_values <- boxplot.stats(Google$Ten_Len)$out
#finding in which row outliers are
Google[which(Google$Ten_Len %in% outlier_values),]
#removing the rows with outliers
Google <- Google[-which(Google$Ten_Len %in% outlier_values),] #249

#Apple
#finding outlier values
outlier_values <- boxplot.stats(Apple$Ten_Len)$out
#finding in which row outliers are
Apple[which(Apple$Ten_Len %in% outlier_values),]
#removing the rows with outliers
Apple<- Apple[-which(Apple$Ten_Len %in% outlier_values),] #126

#Facebook
#finding outlier values
outlier_values <- boxplot.stats(Facebook$Ten_Len)$out
#finding in which row outliers are
Facebook[which(Facebook$Ten_Len %in% outlier_values),]
#removing the rows with outliers
Facebook<- Facebook[-which(Facebook$Ten_Len %in% outlier_values),] #58

#subsetting data frame to extract only rows with relevant companies: MAGAF=Microsoft, Amazon, Google ...
MAGAF <- subset(LinkedIn1, Company=="Microsoft"|Company=="Amazon"|Company=="Google"|Company=="Apple"|Company=="Facebook")

#writing csv
write.csv(MAGAF, file = "MAGAF.csv") #saving in csv with 9802 rows
#--> Insight 1 and 2
#_________________________________________________________________________________________________
#editing for insight 3____________________________________________________________________________
#selecting relevant columns
MAGAF<- MAGAF[, c("Company", "User_ID", "N_Prev_Ten")]

#filtering out unique rows
MAGAF <- unique(MAGAF) #9686 rows
write.csv(MAGAF, file = "MAGAF_Reduced.csv")
#MAGAF df has to be cleaned
#I want to get all rows that occur more than once and keep them
#Editing file in Excel: 
#  .	marking all duplicate User_ID column which are those, where one person occurs twice, meaning, that he changed jobs between companies
#.	Filtering marked Columns/Rows out so that only they remain


#reading df MAGAF_Reduced
MAGAF_Reduced <- read.csv(file="MAGAF_Reduced.csv", head=TRUE, sep=",") #1731 rows
MAGAF_Reduced <- MAGAF_Reduced[2:4] #without first column which makes every row unique

#finding unique user_ids
users <- unique(MAGAF_Reduced$User_ID) #all User_IDs that occur more than once #814


#for loop that prints out all rows, where there are 2, 3, 4, 5 companies for each user_id
#there are 2/814 users with 5 companies
#there are 9/814 users with 4 companies
#there are 79/814 users with 3 companies
#there are 724/814users with 2 companies
#each row will be split up in excel, so that there are only rows illustrating a shift from one job to a second
#therefore, some user_ids will occur more often, max 4 times
#all job switches will be summarised in one excel file "2shift" #918 rows

for (user in users){
  if (length(user_companies <- as.vector(subset(MAGAF_Reduced$Company, MAGAF_Reduced$User_ID == user))) ==2)
    print(user_companies)
}

#installing library to read excel files______________________________________________________________________
setwd("C:/Users/pauli/Documents/NCI/Programming for Big Data/Project/Dataset1")
install.packages("readxl")
library("readxl")

#reading excel file
shift2 <- read_excel("2shift.xlsx")

#saving excel file as df
shift2 <- as.data.frame(shift2) #918 rows

#how many different unique rows are in df, which will be the categories for the diagram
shift2_unique <- unique(shift2) #21

#using magic library to save loop output
library(magicfor)
magic_for(print, silent = TRUE)
#magic_free() #disabling function

#for loop that counts frequency of each row(combination of companies) 
for(n in 1:21){
  Quantity <- nrow(subset(shift2, Companies == shift2_unique[n,]))
  print(Quantity)
}

#saving loop output as vector
Quantity <- magic_result_as_vector()

#attaching new vector as column to existing df
shift2 <- cbind(shift2_unique, Quantity)

#______________________________________________________________________________________
#creating another vector for the total amount of job switches that occured
Total <- rep(918, 21)

#creating a lopop to calculate percentage
for(i in shift2[2]){
  Percentage <- 100/918*i
  print(Percentage)
}

#saving output in vector
Percentage <- round(magic_result_as_vector(),2)

#attaching all columns together
Switch <- cbind(shift2, Total, Percentage)