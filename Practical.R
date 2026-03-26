#Q1
london_crime <- read.csv("C:/Users/nancy/Downloads/london-crime-data.csv")
str(london_crime)
View(london_crime)
Date<-paste(london_crime$day,london_crime$month,london_crime$year)
Date
#Q2
names(london_crime)
# Rename the variables
names(london_crime)[names(london_crime) == "borough"] <- "Borough"
names(london_crime)[names(london_crime) == "major_category"] <- "MajorCategory "
names(london_crime)[names(london_crime) == "minor_category"] <- "SubCategory "
names(london_crime)[names(london_crime) == "value"] <- "Value "
names(london_crime)[names(london_crime) == "Date"] <- "CrimeDate "
# Prove the names have changed
names(london_crime)


#Q3
new_date <- as.Date(london_crime$Date, format = "%m/%d/%Y %H:%M")

# Prove it is now a date variable
class(new_date)
str(new_date)
head(new_date)

#Q4
#summary of borough
Summary_boroght<-(london_crime$Borough)
Summary_boroght

help(table)

Sum_view <- table(Summary_boroght)
Sum_view

windows(16,10)
par(mfrow=c(2,1))

# Simple bar chart
barplot(Sum_view,
        main = "Simple Bar Plot",
        xlab = "Improvement", ylab = "Frequency")

# Create box around plot
box()

# Label the x and y axes with dark green text
title(main = "Summary of Borought", col.main = "blue", font.main = 4)
title(xlab = "Improvement", col.lab = rgb(0, 0.5, 0))
title(ylab = "Frequency", col.lab = rgb(0, 1, 0))


#Q5
help(table)

MajorCategory_Data <- table(london_crime$MajorCategory)
MajorCategory_Data
help(table)



# Pie charts
# 2 rows by 2 cols - for 4 charts
windows(16,10)
par(mfrow = c(2, 2))

# First chart - shows title with chart broken into
# slices vector
slices <- c(20,30,15,5,8,10,2,40,35)
# Theft and Handling is major category had the highest level of crimes.
# Sexual Offences is major category had the highest level of crimes.
lbls <- c('Burglary','Criminal Damage','Drugs','Fraud or Forgery','Other Notifiable Offences',
          'Robbery','Sexual Offences','Theft and Handling','Violence Against the Person')
pie(slices, labels = lbls,
    main = "Major Category Crimes")

#Q6

london_crime$Region[london_crime$Borough >= "Barking and Dagenham"] <- "East"
london_crime$Region[london_crime$Borough >= "Barnet"] <- "North"
london_crime$Region[london_crime$Borough >= "Bexley"] <- "East"
london_crime$Region[london_crime$Borough >= "Brent"] <- "West"
london_crime$Region[london_crime$Borough >= "Bromley"] <- "South"
london_crime$Region[london_crime$Borough >= "Camden"] <- "North"
london_crime$Region[london_crime$Borough >= "Croydon"] <- "South"
london_crime$Region[london_crime$Borough >= "Ealing"] <- "West"
london_crime$Region[london_crime$Borough >= "Enfield"] <- "North"
london_crime$Region[london_crime$Borough >= "Greenwich"] <- "East"
london_crime$Region[london_crime$Borough >= "Hackney"] <- "North"
london_crime$Region[london_crime$Borough >= "Hammersmith and Fulham"] <- "West"
london_crime$Region[london_crime$Borough >= "Haringey"] <- "North"
london_crime$Region[london_crime$Borough >= "Harrow"] <- "West"
london_crime$Region[london_crime$Borough >= "Havering"] <- "East"
london_crime$Region[london_crime$Borough >= "Hillingdon"] <- "West"
london_crime$Region[london_crime$Borough >= "Hounslow"] <- "West"
london_crime$Region[london_crime$Borough >= "Islington"] <- "Central"
london_crime$Region[london_crime$Borough >= "Kensington and Chelsea"] <- "Central"
london_crime$Region[london_crime$Borough >= "Kingston upon Thames"] <- "East"
london_crime$Region[london_crime$Borough >= "Lambeth"] <- "Central"
london_crime$Region[london_crime$Borough >= "Lewisham"] <- "Central"
london_crime$Region[london_crime$Borough >= "Merton"] <- "South"
london_crime$Region[london_crime$Borough >= "Newham"] <- "East"
london_crime$Region[london_crime$Borough >= "Redbridge"] <- "East"
london_crime$Region[london_crime$Borough >= "Richmond upon Thames"] <- "West"
london_crime$Region[london_crime$Borough >= "Southwark"] <- "Central"
london_crime$Region[london_crime$Borough >= "Sutton"] <- "South"
london_crime$Region[london_crime$Borough >= "Tower Hamlets"] <- "Central"
london_crime$Region[london_crime$Borough >= "Waltham Forest "] <- "Central"
london_crime$Region[london_crime$Borough >= "Wandsworth"] <- "East"
london_crime$Region[london_crime$Borough >= "Westminster"] <- "Central"
london_crime
View(london_crime)
# Load the required libraries
library(mice)
library(VIM)

# Display the missing data pattern using mice
md.pattern(london_crime)
#checking whether any Borough has null value
sum(is.na(london_crime$Borough))

#Q7
#which region in London has the highest recorded crime rate
london_crime_highRegion <- london_crime[order(london_crime$Region),]
#london_crime_highRegion

#Using the plot() function, show the number of reported crimes by region
london_crime_highRegion$Region <- as.factor(london_crime_highRegion$Region)
class(london_crime_highRegion$Region)
str(london_crime_highRegion)

plot(london_crime_highRegion$Region, type = "b", col = "green",main = "Highest Recorded Crime",
     xlab = "Region", ylab = "Crime Rate")

#Q8 
#Referring to your answer in Q7, extract out the subset of data that had the highest number of crimes. 
Sorted_data <- subset(london_crime_highRegion)
Sorted_data


#Q9
library(vcd)
?rainbow()

Table_Data <- table(london_crime_highRegion$Region,london_crime_highRegion$Region)
Table_Data
# Use table headers for chart labels
label <- paste(names(Table_Data), "\n", mytable, sep = "")
label
pie(Table_Data, labels = label,
    main = "Pie Chart from a Table\n (with sample sizes)")

windows(16,10)
#par(mfrow = c(2, 2))
hist(Table_Data)


#Q10
write.csv(london_crime, "ondon-crimemodified.csv", row.names = FALSE)
list.files()
