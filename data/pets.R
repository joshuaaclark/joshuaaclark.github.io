## ------------------------------------------------------------------------
asid <- read.csv('Animal_Services_Intake_Data.csv')
nrow(asid)
asid.type <- table(asid$Animal.Type, asid$Intake.Condition)
asid.type

## ------------------------------------------------------------------------
library(lubridate)
library(plyr)
library(scales)
library(ggplot2)
library(arules)

asid$Date <- parse_date_time(asid$Intake.Date, 'm/d/Y', tz='PST')
table(year(asid$Date))

asid.intake.date <- ddply(asid, c('Date', 'Animal.Type'), summarize, Count=length(Date))
ggplot(asid.intake.date, aes(x=Date, y=Count, color=Animal.Type))+geom_point(size=3, alpha=0.8)

## ------------------------------------------------------------------------
ggplot(asid.intake.date, aes(x=Date, y=Count, color=Animal.Type))+geom_smooth()

## ------------------------------------------------------------------------
asid.intake.shelter.type <- data.frame(prop.table(table(asid$Shelter, asid$Animal.Type), 1))
names(asid.intake.shelter.type) <- c('Shelter', 'Animal.Type', 'Freq')
ggplot(asid.intake.shelter.type, aes(y = Shelter, x = Animal.Type, fill=Freq, label=percent(Freq))) + geom_tile(color='black') + scale_fill_gradient(low='white', high='#3182bd') + geom_text() + xlab("Animal Type")

## ----fig.width = 10, fig.height = 20-------------------------------------
asid.dog <- subset(asid, Animal.Type == 'DOG')
asid.dog.tab <- subset(data.frame(table(asid.dog$Breed.1)), Freq > 100)
ggplot(asid.dog.tab, aes(x=sort(Var1, desc=TRUE), y= Freq, fill=Var1)) + geom_bar(stat='identity') + coord_flip() + xlab('Breed') + ylab('Number of Dogs') +  guides(fill=FALSE)

## ------------------------------------------------------------------------
asid.cat <- subset(asid, Animal.Type == 'CAT')
asid.cat.tab <- subset(data.frame(table(asid.cat$Breed.1)), Freq > 100)
ggplot(asid.cat.tab, aes(x=sort(Var1, desc=TRUE), y= Freq, fill=Var1)) + geom_bar(stat='identity') + coord_flip() + xlab('Breed') + ylab('Number of Cats') +  guides(fill=FALSE)

## ------------------------------------------------------------------------
asid$month <- factor(month(asid$Date))
pet.rules <- apriori(asid[,c('Shelter', 'Intake.Condition', 'Intake.Type', 'Animal.Type', 'month' )])
subrules <- pet.rules[round(quality(pet.rules)$lift, digits=1) != 1]

## ----results='asis'------------------------------------------------------
library(stargazer)
stargazer(as(subrules, 'data.frame'), type="html", summary=FALSE)

