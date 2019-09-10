setwd("C:/Users/pauli/Documents/NCI/Programming for Big Data/Project/Dataset2")
Dataset2 <- read.csv(file="employee_reviews.csv", head=TRUE, sep=",")

#selection relevant columns
Dataset2 <- Dataset2[, c("company", "overall.ratings",	"work.balance.stars",	"culture.values.stars",	"carrer.opportunities.stars", "comp.benefit.stars")]

#renaming columns
names(Dataset2) <- c("Company", "Overall_Ratings", "Work_Balance", "Culture_Values", "Career_Opportunities", "Benefits")

#indentifying NA
Dataset2[!complete.cases(Dataset2),] #no na in df

#df contains comapnies= microsoft, amazon, google, apple, facebook, netflix
#filtering company=netflix rows out (810 rows)
Dataset2 <- (subset(Dataset2, Company != "netflix"))
#netflix is still a level though
levels(Dataset2$Company)
#dropping level netflix and assigning the vector without level=netflix to variable
Drop_Netflix <- droplevels(Dataset2$Company)
#replacing Company vector with new vector without level=netflix
Dataset2$Company <-Drop_Netflix

#whole df without 'none' to be able to see variables in comparison #53047 rows
##deleting all rows where there is at least one 'none'
Dataset2_Reduced <- subset(Dataset2, Overall_Ratings != "none" & Work_Balance != "none" & Culture_Values != "none" &
                             Career_Opportunities != "none" & Benefits != "none")
##saving vectors as numeric to be able to plot them
Dataset2_Reduced$Overall_Ratings <- round(as.numeric(as.character(Dataset2_Reduced$Overall_Ratings)),0)
Dataset2_Reduced$Work_Balance <- round(as.numeric(as.character(Dataset2_Reduced$Work_Balance)),0)
Dataset2_Reduced$Culture_Values <- round(as.numeric(as.character(Dataset2_Reduced$Culture_Values)),0)
Dataset2_Reduced$Career_Opportunities <- round(as.numeric(as.character(Dataset2_Reduced$Career_Opportunities)),0)
Dataset2_Reduced$Benefits <- round(as.numeric(as.character(Dataset2_Reduced$Benefits)),0)


#Creating a df for each company
microsoft <- subset(Dataset2_Reduced, Company=="microsoft") #13413
amazon <- subset(Dataset2_Reduced, Company=="amazon") #22246
google <- subset(Dataset2_Reduced, Company=="google") #5641
apple <- subset(Dataset2_Reduced, Company=="apple") #10303
facebook <- subset(Dataset2_Reduced, Company=="facebook") #1444

#saving edited data as csv Dataset2
write.csv(Dataset2, file = "Dataset2.csv") #saving in csv
