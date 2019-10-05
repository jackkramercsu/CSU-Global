'''Hypothesis need to add up here'''


####################################################################### Necessary packages and Librarys and Data Start
ls()
df<-read.csv(file="~/Desktop/Credit_CardDebtMIS500.csv",
             head=TRUE,)

#head(df)


##for correlation tests, pearson, kenall, spearman
if(!require(devtools)) install.packages("devtools")
devtools::install_github ("kassambra/ggpubr")
install.packages("dplyr")
library(dplyr)

##for for graphs/plots 
library(ggplot2)
install.packages("ggplot2")

install.packages("ggiraph")
install.packages("ggiraphExtra")

install.packages("plyr")

install.packages("ggpubr")
library(ggpubr)

#install.packages("moonBook") this was tried for an interactive chart, need more practice w/

#rpart is for the regression tree
#library(rpart) this is below for a different graph



if(!require(devtools)) install.packages("devtools")
devtools::install_github ("kassambra/ggpubr")
library(rpart)


summary(df)

####################################################################### Necessary packages and Librarys END


####################################################################### Section 0 Start prior to summary
#show a random sample of 10 from data set total rows 10,000
set.seed(1234)
dplyr::sample_n(df, 10)
#show the levels
levels(df$student)

#reodering the levels
df$student <- ordered(df$student,
                      levels = c("ctrl", "trt1", "trt2"))



####################################################################### Section 0 Start prior to summary



####################################################################### Section 1 start
#This does a paired t test on income and balance and show a scatter plot with a regession line.
#paired 
##income and balance
t.test(df$income,
       df$balance,
       paired=TRUE,
       conf.level=0.95)


#################################################### Section 1.2
#have at after the paired t sest but prior to the scatter plot so that it provides context
#with for the plot

#correlation test  come back only works on numerical values
##pearson, kendall, and spearman
#income and balance
cor(df$income, df$balance, method = c("pearson", "kendall", "spearman"))
cor.test(df$income, df$balance, method=c("pearson", "kendall", "spearman"))
#confimrs cor of -0.1552 tested else where find that spot
#################################################### Section 1.2 end


'''This gets a constant error in consol not sure why it is reading the URL
#simple regression this porvides a -0.1522 corelation, this is the same as the pearsons cor test confirmed twice
#only works with numeric URL for reference
#http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/#examples-of-data-and-problem'''
theme_set(theme_pubr())

ggplot(df, aes(x = df$income, y = df$balance)) +
  geom_point() +
  stat_smooth()

#regession line
ggplot(df, aes(df$income, df$balance)) + 
  geom_point() + 
  stat_smooth(method = lm)

#the correlation between balance and income is week and does not provide much insight to the data.
cor(df$income, df$balance)

#Section 1 provides multiple ways of performing a correlation test for numeric veriables, and confirms the
#correlation between income and balance of -.1522 or if writen in reverse provides a positive correlation of 
#.1522

####################################################################### Section 1 end.





####################################################################### Section 2 start.
#this section does intipendent t.test and provides different combinations of boxplots
#provides insight the the summary stats for the two numerical values in the data set.
#Tindependt Ttest each column doesnt work for categorical values
t.test(df$balance) #numerical
t.test(df$income) #numerical
#good boxplot
##Income level of default
##This plot shows the median income level of those who default
##is relativily similar to those who dont, This may provide the 
##reasoning for why we only get a correlation of 0.1522 correlation
##between income and balance. 
boxplot(df$income~df$default, data=df, main="Default Data",
        xlab="Default", ylab="Income Level")

##Income level of Student
##Medium income level for students is much lower than for non students
##This makes sense and provides confirmation of the accuracy of the data
boxplot(df$income~df$student, data=df, main="Default Data",
        xlab="Student", ylab="Income Level")


##balance by default
#The median balance of those who default is close to $2,000
boxplot(df$balance~df$default, data=df, main="Default Data",
        xlab="Default", ylab="Outstanding Balance")

##Balance by Student
##Students have a slightly higher outstanding balance that non
##students, this makes since, do students have a higher chance of defaulting
##with a higher balance?
boxplot(df$balance~df$student, data=df, main="Default Data",
        xlab="Student", ylab="Outstanding Balance")

#######This is a good transistion point, go through the broad strokes
#######of what each categorical veriable looks like with each numerical value,
#######now show what the liekly hood of defaulting is.This could be a good ending point
#######use the correlation and other statistics on either the backend to show
#######overall data and how it all relates but it is not very interesting overall
##Adding a histogram to see the comparison of student defaul to non student default
##stacked bar chart doesnt provide detail I want
af <- data.frame(df$student, df$default)
student <-ggplot(af, aes(df$student, ..count..)) +geom_bar(aes(fill = df$default), possition = "dodge")
print(student + ggtitle("Count of Default by Student") 
      + labs(y="Count", x="Student") 
      + labs(fill="Default"))


#need an accompanying frequency table to show percent of defaults
#Create Frequency Table This good
groups.t1 <- table(af) #Creates frequency table
groups.t1 #prints table

#proportions and percentages for students and default
round(prop.table(groups.t1),3)

'''This shows that the likely hood of defaulting on a credit card 
whether a student or not is very unlikely, This is good for students
who are taking this into account when think of going/continuing with school'''


####################################################################### Section 2 end.



####################################################################### Section 3 Start
#Regression tree

git <- rpart(df$student~df$income + df$balance, 
             method="anova", data=df)

printcp(git) # display the results 
plotcp(git) # visualize cross-validation results 
summary(git) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(git) # visualize cross-validation results   

# plot tree 
plot(git, uniform=TRUE, 
     main="Regression Tree for Student Default ")
text(git, use.n=TRUE, all=TRUE, cex=.8)


####################################################################### Section 3 End








'''
#references

#simple regression this porvides a -0.1522 corelation, this is the same as the pearsons cor test confirmed twice
#only works with numeric URL for reference
http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/#examples-of-data-and-problem

#Anova: analysis of variance
#http://www.sthda.com/english/wiki/one-way-anova-test-in-r#import-your-data-into-r
#website for visualization


ggPredict:
https://www.rdocumentation.org/packages/ggiraphExtra/versions/0.2.9/topics/ggPredict

plot gallery
https://www.r-graph-gallery.com/262-basic-boxplot-with-ggplot2.html

#wilcoxon sum test
#http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r


#signtest


#Other websites
#website for making a cool plot
http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
#chi.sqrd test

install.packages("ggplot") #this is not avialable for r 3.6.1
find updated version or alt package'''
'''
