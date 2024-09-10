
file_path<-"/Users/noorfathima/Documents/college/year 2/Semester 3/Applied Statistics and R/r package/caseStudy5.csv"
mydata <- read.csv(file = file_path, header = TRUE, sep = ",", dec = ".")

c1<-subset(mydata,Active=="1")
c2<-subset(mydata,Active=="2")

label<-c("Active","Placebo")
painrate<-c("0","1","2","3","4","5","6","7","8","9","10")

#PRE-TEST

#pretest boxplot
boxplot(mydata$Score_1~mydata$Active,data=mydata,main="Pre Test",xlab="Pain Rating",ylab="Devices",names=c("Active","Placebo"),col=c("orange","red"),horizontal = TRUE)

#frequency table
freqtable_preActive=table(cut(c1$Score_1,breaks<-c(0,1,2,3,4,5,6,7,8,9,10,11),right=FALSE))
print("The frequency table is ")
print(freqtable_preActive)

freqtable_prePlacebo=table(cut(c2$Score_1,breaks<-c(0,1,2,3,4,5,6,7,8,9,10,11),right=FALSE))
print("The frequency table is ")
print(freqtable_prePlacebo)


#frequency
freq_preActive<-c(rep(0,10))
x<-1
for (i in freqtable_preActive) {
  print(i)
  freq_preActive[x]=i
  x<-x+1
}
print(freq_preActive)

freq_prePlacebo<-c(rep(0,10))
x<-1
for (i in freqtable_prePlacebo) {
  print(i)
  freq_prePlacebo[x]=i
  x<-x+1
}
print(freq_prePlacebo)

#relative frequencies
relative_freq_preActive <- freq_preActive / sum(freq_preActive)
print(relative_freq_preActive)

relative_freq_prePlacebo <- freq_prePlacebo / sum(freq_prePlacebo)
print(relative_freq_prePlacebo)

#plotting bar graph
preValues<-matrix(c(relative_freq_preActive,relative_freq_prePlacebo),nrow = 2,ncol = 11,byrow = TRUE)
print(preValues)
barplot(preValues,main="Pre Test",xlab = "Pain Rating",ylab = "Relative Frequency",col=c("orange","red"),names.arg=painrate,ylim = c(0,1.0),beside = TRUE)
legend(legend=label,"topright",label , cex = 0.7, fill = c("orange","red"))

preActiveSummary<-summary(c1$Score_1)
print("Pre Test (Active Magnets) Summary:")
print(paste0("Minimum: ",preActiveSummary["Min."]))
print(paste0("Maximum: ",preActiveSummary["Max."]))
print(paste0("Range: ",preActiveSummary["Max."]-preActiveSummary["Min."]))
print(paste0("Mean ",preActiveSummary["Mean"]))
print(paste0("Median: ",preActiveSummary["Median"]))
print(paste0("1st Quartile: ",preActiveSummary["1st Qu."]))
print(paste0("3rd Quartile: ",preActiveSummary["3rd Qu."]))
print(paste0("Variance: ",var(c1$Score_1)))
print(paste0("Standard Deviation: ",sd(c1$Score_1)))

prePlaceboSummary<-summary(c2$Score_1)
print("Pre Test (Placebo Magnets) Summary:")
print(paste0("Minimum: ",prePlaceboSummary["Min."]))
print(paste0("Maximum: ",prePlaceboSummary["Max."]))
print(paste0("Range: ",prePlaceboSummary["Max."]-prePlaceboSummary["Min."]))
print(paste0("Mean ",prePlaceboSummary["Mean"]))
print(paste0("Median: ",prePlaceboSummary["Median"]))
print(paste0("1st Quartile: ",prePlaceboSummary["1st Qu."]))
print(paste0("3rd Quartile: ",prePlaceboSummary["3rd Qu."]))
print(paste0("Variance: ",var(c2$Score_1)))
print(paste0("Standard Deviation: ",sd(c2$Score_1)))

#POST TEST

#post box
boxplot(mydata$Score_2~mydata$Active,data=mydata,main="Post Test",xlab="Pain Rating",ylab="Devices",names=c("Active","Placebo"),col=c("purple","pink"),horizontal = TRUE)

#frequency table
freqtable_postActive=table(cut(c1$Score_2,breaks<-c(0,1,2,3,4,5,6,7,8,9,10,11),right=FALSE))
print("The frequency table is ")
print(freqtable_postActive)

freqtable_postPlacebo=table(cut(c2$Score_2,breaks<-c(0,1,2,3,4,5,6,7,8,9,10,11),right=FALSE))
print("The frequency table is ")
print(freqtable_postPlacebo)

#frequency
freq_postActive<-c(rep(0,10))
x<-1
for (i in freqtable_postActive) {
  print(i)
  freq_postActive[x]=i
  x<-x+1
  #print(x)
}
print(freq_postActive)

freq_postPlacebo<-c(rep(0,10))
x<-1
for (i in freqtable_postPlacebo) {
  print(i)
  freq_postPlacebo[x]=i
  x<-x+1
}
print(freq_postPlacebo)

#relative frequencies
relative_freq_postActive <- freq_postActive / sum(freq_postActive)
print(relative_freq_postActive)

relative_freq_postPlacebo <- freq_postPlacebo / sum(freq_postPlacebo)
print(relative_freq_postPlacebo)

#plotting bar graph
values<-matrix(c(relative_freq_postActive,relative_freq_postPlacebo),nrow = 2,ncol = 11,byrow = TRUE)
print(values)
barplot(values,main="Post Test",xlab = "Pain Rating",ylab = "Relative Frequency",col=c("purple","pink"),names.arg=painrate,ylim = c(0,0.6),beside = TRUE)
legend(legend=label,"topright",label , cex = 0.7, fill = c("purple","pink"))

postActiveSummary<-summary(c1$Score_2)
print("Post Test (Active Magnets) Summary:")
print(paste0("Minimum: ",postActiveSummary["Min."]))
print(paste0("Maximum: ",postActiveSummary["Max."]))
print(paste0("Range: ",postActiveSummary["Max."]-postActiveSummary["Min."]))
print(paste0("Mean ",postActiveSummary["Mean"]))
print(paste0("Median: ",postActiveSummary["Median"]))
print(paste0("1st Quartile: ",postActiveSummary["1st Qu."]))
print(paste0("3rd Quartile: ",postActiveSummary["3rd Qu."]))
print(paste0("Variance: ",var(c1$Score_2)))
print(paste0("Standard Deviation: ",sd(c1$Score_2)))

postPlaceboSummary<-summary(c2$Score_2)
print("Post Test (Placebo Magnets) Summary:")
print(paste0("Minimum: ",postPlaceboSummary["Min."]))
print(paste0("Maximum: ",postPlaceboSummary["Max."]))
print(paste0("Range: ",postPlaceboSummary["Max."]-postPlaceboSummary["Min."]))
print(paste0("Mean ",postPlaceboSummary["Mean"]))
print(paste0("Median: ",postPlaceboSummary["Median"]))
print(paste0("1st Quartile: ",postPlaceboSummary["1st Qu."]))
print(paste0("3rd Quartile: ",postPlaceboSummary["3rd Qu."]))
print(paste0("Variance: ",var(c2$Score_2)))
print(paste0("Standard Deviation: ",sd(c2$Score_2)))

#CHANGE TEST

#Change box
boxplot(mydata$Change~mydata$Active,data=mydata,main="Change in Pain",xlab="Change in Pain Rating",ylab="Devices",names=c("Active","Placebo"),col=c("green","darkgreen"),horizontal = TRUE)

#frequency table
freqtable_changeActive=table(cut(c1$Change,breaks<-c(0,1,2,3,4,5,6,7,8,9,10,11),right=FALSE))
print("The frequency table is ")
print(freqtable_changeActive)

freqtable_changePlacebo=table(cut(c2$Change,breaks<-c(0,1,2,3,4,5,6,7,8,9,10,11),right=FALSE))
print("The frequency table is ")
print(freqtable_changePlacebo)

#frequency
freq_changeActive<-c(rep(0,10))
x<-1
for (i in freqtable_changeActive) {
  print(i)
  freq_changeActive[x]=i
  x<-x+1
}
print(freq_changeActive)

freq_changePlacebo<-c(rep(0,10))
x<-1
for (i in freqtable_changePlacebo) {
  print(i)
  freq_changePlacebo[x]=i
  x<-x+1
}
print(freq_changePlacebo)

#relative frequencies
relative_freq_changeActive <- freq_changeActive / sum(freq_changeActive)
print(relative_freq_changeActive)

relative_freq_changePlacebo <- freq_changePlacebo / sum(freq_changePlacebo)
print(relative_freq_changePlacebo)

#plotting bar graph
values<-matrix(c(relative_freq_changeActive,relative_freq_changePlacebo),nrow = 2,ncol = 11,byrow = TRUE)
print(values)
barplot(values,main="Change in Pain",xlab = "Change in Pain Rating",ylab = "Relative Frequency",col=c("green","darkgreen"),names.arg=painrate,ylim = c(0,0.6),beside = TRUE)
legend(legend=label,"topright",label , cex = 0.7, fill = c("green","darkgreen"))

changeActiveSummary<-summary(c1$Change)
print("Change Test (Active Magnets) Summary:")
print(paste0("Minimum: ",changeActiveSummary["Min."]))
print(paste0("Maximum: ",changeActiveSummary["Max."]))
print(paste0("Range: ",changeActiveSummary["Max."]-changeActiveSummary["Min."]))
print(paste0("Mean ",changeActiveSummary["Mean"]))
print(paste0("Median: ",changeActiveSummary["Median"]))
print(paste0("1st Quartile: ",changeActiveSummary["1st Qu."]))
print(paste0("3rd Quartile: ",changeActiveSummary["3rd Qu."]))
print(paste0("Variance: ",var(c1$Change)))
print(paste0("Standard Deviation: ",sd(c1$Change)))

changePlaceboSummary<-summary(c2$Change)
print("Change Test (Placebo Magnets) Summary:")
print(paste0("Minimum: ",changePlaceboSummary["Min."]))
print(paste0("Maximum: ",changePlaceboSummary["Max."]))
print(paste0("Range: ",changePlaceboSummary["Max."]-changePlaceboSummary["Min."]))
print(paste0("Mean ",changePlaceboSummary["Mean"]))
print(paste0("Median: ",changePlaceboSummary["Median"]))
print(paste0("1st Quartile: ",changePlaceboSummary["1st Qu."]))
print(paste0("3rd Quartile: ",changePlaceboSummary["3rd Qu."]))
print(paste0("Variance: ",var(c2$Change)))
print(paste0("Standard Deviation: ",sd(c2$Change)))

#t-test
print("T Test")
active=c1$Change
placebo=c2$Change
mean_active<-mean(active)
mean_placebo<-mean(placebo)
var_active<-var(active)
var_placebo<-var(placebo)
n_active<-length(active)
n_placebo<-length(placebo)
t_test<-(mean_active-mean_placebo)/sqrt((var_active/n_active)+(var_placebo/n_placebo))
print(paste0("t-statistics: ",t_test))
df_active_placebo<-n_active+n_placebo-2
p_value<-2*(1-pt(abs(t_test),df=df_active_placebo))
print(paste0("p_value: ",p_value,"\n"))
if(p_value>0.05)
{
  print("means of active magnets and placebo magnets are equal");
}else
{
  print("means of active magnets and placebo magnets are NOT equal");
}

#linear regression
x1<-c(c1$Score_1)
x2<-c(c2$Score_1)

y1<-c(c1$Score_2)
y2<-c(c2$Score_2)

relation1<-lm(y1~x1)

i<-data.frame(x1=10)
result1<-predict(relation1,i)
result1

relation2<-lm(y2~x2)
j<-data.frame(x2=10)
result2<-predict(relation2,j)
result2





