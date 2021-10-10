
nc = read.csv('new_york_city.csv')
wc = read.csv('washington.csv')
cc = read.csv('chicago.csv')

head(nc)


head(wc)

head(cc)

#New York City
#loading the dplyr package
library(dplyr)
nc<-read.csv('new_york_city.csv')
#Extracting a more manageable portion from our data
new_nc <- nc %>% select(X, Start.Time)
#Adding a months column
new_nc$Month <- format(as.Date(new_nc$Start.Time), "%m")
#converting the month data type to integer for better continuous manipulation
new_nc$Month <- as.integer(new_nc$Month)
#creating dataframe for months with their travel counts
month_counts <- new_nc %>% count(Month)
#printing out the month with the highest travels for new york city
month_counts[which.max(month_counts$n), ]
#Visualisation, installing ggplot package
library("ggplot2")
#Visualisation for travels in the different months for new york city
nc_plot<- ggplot(data=month_counts, aes(Month,y=n))+
geom_line(linetype="dashed")+
scale_x_continuous()+
ggtitle("A bar plot showing travels for different months in New York City")+
theme(plot.title=element_text(hjust=0.5))+
labs(x="Month", y="Total number of travels in a month")
nc_plot
#Looking at the summary
summary(month_counts)


#Washington City
#loading the dplyr package
library(dplyr)
wc<-read.csv('washington.csv')
#Extracting a more manageable portion from our data
new_wc <- wc %>% select(X, Start.Time)
#Adding a months column
new_wc$Month <- format(as.Date(new_wc$Start.Time), "%m")
#converting the month data type to integer for better continuous manipulation
new_wc$Month <- as.integer(new_wc$Month)
#creating dataframe for months with their travel counts
wcmonth_counts <- new_wc %>% count(Month)
#printing out the month with the highest travels for new york city
wcmonth_counts[which.max(wcmonth_counts$n), ]
#Visualisation, installing ggplot package
library("ggplot2")
#Visualisation for travels in the different months for new york city
wc_plot<- ggplot(data=subset(wcmonth_counts, !is.na(Month)), aes(Month,y=n))+
geom_line()+
scale_x_continuous()+
ggtitle("A bar plot showing travels for different months in Washington City")+
labs(x="Month", y="Total number of travels in a month")
wc_plot
#Looking at the summary
summary(wcmonth_counts)


#Chicago City
#loading the dplyr package
library(dplyr)
cc<-read.csv('chicago.csv')
#Extracting a more manageable portion from our data
new_cc <- cc %>% select(X, Start.Time)
#Adding a months column
new_cc$Month <- format(as.Date(new_cc$Start.Time), "%m")
#converting the month data type to integer for better continuous manipulation
new_cc$Month <- as.integer(new_cc$Month)
#creating dataframe for months with their travel counts
ccmonth_counts <- new_cc %>% count(Month)
#printing out the month with the highest travels for new york city
ccmonth_counts[which.max(ccmonth_counts$n), ]
#Visualisation, installing ggplot package
library("ggplot2")
#Visualisation for travels in the different months for new york city
cc_plot<- ggplot(data=subset(ccmonth_counts, !is.na(Month)), aes(Month,y=n))+
geom_line()+
scale_x_continuous()+
ggtitle("A bar plot showing travels for different months in Washington City")+
labs(x="Month", y="Total number of travels in a month")
cc_plot
#Looking at the summary
summary(ccmonth_counts)

#New york city
#loading the dolyr package
library(dplyr)
#import working file
nc<-read.csv('new_york_city.csv')
#Extracting a more manageable portion from our data
week_nc <- nc %>% select(X, Start.Time)
#adjust the data frame to place week and month
week_nc$Weekday <- format(as.Date(week_nc$Start.Time), "%u")
week_nc$Month<- format(as.Date(week_nc$Start.Time), "%Y-%m") 
#creating dataframe for weekdays and months with their travel counts
weekday_counts <- week_nc %>% count(Weekday, Month)
#calculating out the day of the week with the highest travels for new york city
weekday_counts[which.max(weekday_counts$n), ]
paste('The commonest week day in all months is the Thursday of the month of June')
#Visualization, but remember to adjust the data to remove na
qplot(data = weekday_counts, x = Weekday, y=n, col = Month)+
  scale_x_discrete(1:7)+
facet_wrap(~Month)
#Summary using boxplot to performance of the week day in all months
ggplot(weekday_counts, aes(x=Weekday, y=n))+
geom_boxplot()+
ggtitle('Summary of the week day counts for all Months')+
theme(plot.title=element_text(hjust=0.5))+
labs(x="Weekday for all Months", y="Travel counts for the week day in all months")
#summary statistic
summary(weekday_counts)
by(weekday_counts$n, weekday_counts$Weekday, summary)

#Washington city
#loading dplyr package
library(dplyr)
#importing working file
wc<-read.csv('washington.csv')
#Extracting a more manageable portion from our data
week_wc <- wc %>% select(X, Start.Time)
#adjust the data frame to place week and month
week_wc$Weekday <- format(as.Date(week_wc$Start.Time), "%u")
week_wc$Month<- format(as.Date(week_wc$Start.Time), "%Y-%m") 
#creating dataframe for weekdays and months with their travel counts
wcweekday_counts <- week_wc %>% count(Weekday, Month)
#calculating out the day of the week with the highest travels 
wcweekday_counts[which.max(wcweekday_counts$n), ]
#Visualization, but remember to adjust the data to remove na
qplot(data = subset(wcweekday_counts, !is.na(Weekday)), x = Weekday, y=n, col = Month)+
  scale_x_discrete(1:7)+
facet_wrap(~Month)
#Summary using boxplot to performance of the week day in all months
ggplot(data=subset(wcweekday_counts, !is.na(Weekday)), aes(x=Weekday, y=n))+
geom_boxplot()+
ggtitle('Summary of the week day counts for all Months')+
theme(plot.title=element_text(hjust=0.5))+
labs(x="Weekday for all Months", y="Travel counts for the week day in all months")
#summary statistic
summary(wcweekday_counts)
by(wcweekday_counts$n, wcweekday_counts$Weekday, summary)

#Chicago city
#loading dplyr package
library(dplyr)
#importing working file
wc<-read.csv('chicago.csv')
#Extracting a more manageable portion from our data
week_cc <- cc %>% select(X, Start.Time)
#adjust the data frame to place week and month
week_cc$Weekday <- format(as.Date(week_cc$Start.Time), "%u")
week_cc$Month<- format(as.Date(week_cc$Start.Time), "%Y-%m") 
#creating dataframe for weekdays and months with their travel counts
ccweekday_counts <- week_cc %>% count(Weekday, Month)
#calculating out the day of the week with the highest travels
ccweekday_counts[which.max(ccweekday_counts$n), ]
#Visualization, but remember to adjust the data to remove na
qplot(data = subset(ccweekday_counts, !is.na(Weekday)), x = Weekday, y=n, col = Month)+
  scale_x_discrete(1:7)+
facet_wrap(~Month)
#Summary using boxplot to performance of the week day in all months
ggplot(data=subset(ccweekday_counts, !is.na(Weekday)), aes(x=Weekday, y=n))+
geom_boxplot()+
ggtitle('Summary of the week day counts for all Months')+
theme(plot.title=element_text(hjust=0.5))+
labs(x="Weekday for all Months", y="Travel counts for the week day in all months")
#summary statistic
summary(wcweekday_counts)
by(ccweekday_counts$n, ccweekday_counts$Weekday, summary)

#New york city
#loading the dolyr package
library(dplyr)
#import working file
nc<-read.csv('new_york_city.csv')
#Extracting a more manageable portion from our data
day_nc <- nc %>% select(X, Start.Time)
#adjust the data frame to place day
day_nc$Day <- format(as.Date(day_nc$Start.Time), "%Y-%m-%d")
#creating dataframe for daily travel counts
ncday_counts <- day_nc %>% count(Day)
#adding months to the data frame
ncday_counts$Month<- format(as.Date(ncday_counts$Day), "%m")
ncday_counts$Day <- format(as.Date(ncday_counts$Day), "%d")
#calculating out the day with the highest travels for new york city
ncday_counts[which.max(ncday_counts$n), ]
#modifying day to integer for better plot manipulation
ncday_counts$Day<-as.integer(ncday_counts$Day)
#Visualization
ggplot(data=subset(ncday_counts, !is.na(Day)), aes(Day,y=n))+
geom_point()+
geom_line()+
scale_x_continuous()+
facet_wrap(~Month, ncol=3)+
ggtitle("Scatter plots with line added showing total travels for each day in New York City")+
labs(x="Day of the Month", y="Total travels in day")
#summary statistics
by(ncday_counts$n, ncday_counts$Month, summary)
summary(ncday_counts)

#Washington city
#loading the dolyr package
library(dplyr)
#import working file
wc<-read.csv('washington.csv')
#Extracting a more manageable portion from our data
day_wc <- wc %>% select(X, Start.Time)
#adjust the data frame to place day
day_wc$Day <- format(as.Date(day_wc$Start.Time), "%Y-%m-%d")
#creating dataframe for daily travel counts
wcday_counts <- day_wc %>% count(Day)
#adding months to the data frame
wcday_counts$Month<- format(as.Date(wcday_counts$Day), "%m")
wcday_counts$Day <- format(as.Date(wcday_counts$Day), "%d")
#calculating out the day with the highest travels for new york city
wcday_counts[which.max(wcday_counts$n), ]
#modifying day to integer for better plot manipulation
wcday_counts$Day<-as.integer(wcday_counts$Day)
#Visualization
ggplot(data=subset(wcday_counts, !is.na(Day)), aes(Day,y=n))+
geom_point()+
geom_line()+
scale_x_continuous()+
facet_wrap(~Month, ncol=3)+
ggtitle("Scatter plots with line added showing total travels for each day in Washington City")+
labs(x="Day of the Month", y="Total travels in day")
#summary statistics
by(wcday_counts$n, wcday_counts$Month, summary)
summary(wcday_counts)

#Chicago city
#loading the dolyr package
library(dplyr)
#import working file
cc<-read.csv('chicago.csv')
#Extracting a more manageable portion from our data
day_cc <- cc %>% select(X, Start.Time)
#adjust the data frame to place day
day_cc$Day <- format(as.Date(day_cc$Start.Time), "%Y-%m-%d")
#creating dataframe for daily travel counts
ccday_counts <- day_cc %>% count(Day)
#adding months to the data frame
ccday_counts$Month<- format(as.Date(ccday_counts$Day), "%m")
ccday_counts$Day <- format(as.Date(ccday_counts$Day), "%d")
#calculating out the day with the highest travels for new york city
ccday_counts[which.max(ccday_counts$n), ]
#modifying day to integer for better plot manipulation
ccday_counts$Day<-as.integer(ccday_counts$Day)
#Visualization
ggplot(data=subset(ccday_counts, !is.na(Day)), aes(Day,y=n))+
geom_point()+
geom_line()+
scale_x_continuous()+
facet_wrap(~Month, ncol=3)+
ggtitle("Scatter plots with line added showing total travels for each day in Chicago City")+
labs(x="Day of the Month", y="Total travels in day")
#summary statistics
by(ccday_counts$n, ccday_counts$Month, summary)
summary(ccday_counts)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
