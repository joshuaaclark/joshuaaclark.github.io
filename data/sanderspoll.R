ep<-read.csv('exit-polls.csv') #Read the data
head(ep) #Take a look see

ep$diff <- ep$Support.for.Clinton.in.Results - ep$Support.for.Clinton.in.Exit.Polls #Difference the two polling numbers
head(ep)
t.ep <- t.test(diff~Paper.Trail, data = ep) #Compare the means
t.ep #Overlap

library(ggplot2) #For pretty graphs! <3 u Hadley
ggplot(ep, aes(x = diff, fill = Paper.Trail)) + geom_histogram(color = 'black') + xlab('Difference') + ylab('Count') #Histogram
ggplot(ep, aes(y = diff, x = Paper.Trail, fill = Paper.Trail)) + geom_boxplot() #Boxplot


ep.no.az <- subset(ep, State != 'Arizona') #Leaving Arizona out
t.ep.2 <- t.test(diff ~ Paper.Trail, data = ep.no.az) #Redo
t.ep.2 #Nada

ggplot(ep.no.az, aes(x = diff, fill = Paper.Trail)) + geom_histogram(color = 'black') + xlab('Difference') + ylab('Count')
ggplot(ep.no.az, aes(y = diff, x = Paper.Trail, fill = Paper.Trail)) + geom_boxplot()

library(dplyr)
race.data <- read.csv('raw_data.csv', stringsAsFactors = FALSE) #Read the data
race.data[race.data == 'N/A'] <- NA #Turn missing data into a format R likes
race.data$Asian <- as.numeric(race.data$Asian) #Clean up
race.data$Two.Or.More.Races <- as.numeric(race.data$Two.Or.More.Races)
head(race.data)
race.data[,c('White','Black','Hispanic','Asian', 'Two.Or.More.Races')] <- race.data[,c('White','Black','Hispanic','Asian', 'Two.Or.More.Races')]*100 #Rescale so that the regression coefs are expressed as per one percentage point change
head(race.data)
combo.data <- left_join(ep, race.data, by=c('State'='Location')) #join
paper.only.mod<-lm(Support.for.Clinton.in.Results ~  Paper.Trail + Hispanic, data = combo.data) #OG model - blueness
summary(paper.only.mod) #Paper trail checks in 

fin.mod<-lm(Support.for.Clinton.in.Results ~  Paper.Trail + White + Black + Hispanic + Asian, data = combo.data, na.action = na.exclude) #refit
summary(fin.mod) #nada
combo.data$pred <- predict(fin.mod) 
ggplot(combo.data, aes(y = Support.for.Clinton.in.Results, x = Black, shape = Paper.Trail)) + geom_point(color = 'red') + geom_point(aes(y = pred, ), color = 'blue') + ggtitle("Predicted vs Actual Results, Red = Actual, Blue = Predicted") + xlab("% of Black Voters in State")

black.hispanic.mod<-lm(Support.for.Clinton.in.Results ~  Paper.Trail + White + Black + Hispanic, data = combo.data) #Another model
summary(black.hispanic.mod) #let's see
library(car)
vif(black.hispanic.mod)

black.hispanic.mod2<-lm(Support.for.Clinton.in.Results ~  Paper.Trail +  Black + Hispanic, data = combo.data) #Another model
summary(black.hispanic.mod2) #let's see
library(car)
vif(black.hispanic.mod2)


