#general information on companies__________________________________________________________________
#Gender Proportion
#All companies/general
#Calculate the percentage for each day, using one decimal place.
percentlabels_gen<- round(100*table(MAGAF$Gender)/sum(table(MAGAF$Gender)), 1)
#Add a '%' sign to each percentage value using the paste command.
pielabels_gen<- paste(percentlabels_gen, "%", sep="")
#Create pie chart
pie(table(MAGAF$Gender), main="General", col=c("yellow", "aquamarine1"), labels=pielabels_gen, cex=0.8)
#legend
legend("topleft", c("Female","Male"), col=c("yellow", "aquamarine1"), cex=1.5, pch=19)
#Microsoft
percentlabels_mic<- round(100*table(Microsoft$Gender)/sum(table(Microsoft$Gender)), 1)
pielabels_mic<- paste(percentlabels_mic, "%", sep="")
pie(table(Microsoft$Gender), main="Microsoft", col=c("yellow", "aquamarine1"), labels=pielabels_mic, cex=0.8)
#Amazon
percentlabels_ama<- round(100*table(Amazon$Gender)/sum(table(Amazon$Gender)), 1)
pielabels_ama<- paste(percentlabels_ama, "%", sep="")
pie(table(Amazon$Gender), main="Amazon", col=c("yellow", "aquamarine1"), labels=pielabels_ama, cex=0.8)
#Google
percentlabels_goo<- round(100*table(Google$Gender)/sum(table(Google$Gender)), 1)
pielabels_goo<- paste(percentlabels_goo, "%", sep="")
pie(table(Google$Gender), main="Google", col=c("yellow", "aquamarine1"), labels=pielabels_goo, cex=0.8)
#Apple
percentlabels_app<- round(100*table(Apple$Gender)/sum(table(Apple$Gender)), 1)
pielabels_app<- paste(percentlabels_app, "%", sep="")
pie(table(Apple$Gender), main="Apple", col=c("yellow", "aquamarine1"), labels=pielabels_app, cex=0.8)
#Amazon
percentlabels_fac<- round(100*table(Facebook$Gender)/sum(table(Facebook$Gender)), 1)
pielabels_fac<- paste(percentlabels_fac, "%", sep="")
pie(table(Facebook$Gender), main="Facebook", col=c("yellow", "aquamarine1"), labels=pielabels_fac, cex=0.8)


#insight 1
#__________________________________________________________________________________________________
#function that will plot 1 boxplot for the distribution of tenure in each company
Ten_Len_Company <- function(Companyx){
  #subsetting female and male employees
  Companyx_Female <- subset(Companyx, Gender=="Female")
  Companyx_Male <- subset(Companyx, Gender=="Male")
  #printing relevant statistical values
  print(summary(Companyx))
  print(summary(Companyx$Ten_Len))
  print(summary(Companyx_Female$Ten_Len))
  print(summary(Companyx_Male$Ten_Len))
  #conducting a t-test
  print(t.test(Companyx_Male$Ten_Len,Companyx_Female$Ten_Len, alternative="two.sided", paired=FALSE))
  #creating a boxplot
  boxplot(Companyx_Male$Ten_Len,Companyx_Female$Ten_Len, Companyx$Ten_Len, 
          names=c("Male", "Female", "General"), col=c("aquamarine1", "yellow", "magenta1"), 
          horizontal=TRUE)
  #creatine two vertical lines to illustrate the "nine-month and two-year mark"
  abline(v=c(9,24), col="red")
}
#calling function
Ten_Len_Company(Microsoft)

#_______________________________________________________________________________
#Comparing all companies with boxplots
boxplot(Microsoft$Ten_Len, Amazon$Ten_Len, Google$Ten_Len, Apple$Ten_Len, Facebook$Ten_Len, 
names=c("Microsoft", "Amazon", "Google", "Apple", "Facebook"), 
col=c("darkorange", "chartreuse3", "firebrick1", "darkorchid1", "dodgerblue3"), 
main="Tenure in Months for Each Company", xlab="Months", horizontal=TRUE)
#legend
legend("topright", c("Facebook", "Apple", "Google","Amazon","Microsoft"), 
col=c("dodgerblue3","darkorchid1",  "firebrick1", "chartreuse3", "darkorange"), pch=19)
abline(v=c(9,24), col="red")