---
layout: post
title: 'Investigating the LA Shelter Data'
author: Joshua Clark
date: December 3rd, 2015
---

Shortly after I moved to Los Angeles four years ago I met Macho, my
girlfriend's dog. Macho is a Chihuahua and despite not really liking
tiny dogs he charmed me rather quickly. However one of Macho's other
noteworthy traits is that the was adopted from an LA County shelter.

![Hero Dog](http://i.imgur.com/gFVTjLv.jpg)

Fast forward a few years and I was trying to find a home for a box of
new-born kittens that was left out on the street in Koreatown. While the
folks at the shelter were really supportive they did not have the space
and capacity to take care of four two day old kittens. Eventually we
were able to find a foster for them on the Westside but the entire
process got me thinking, how full are the LA Shelters?

Fortunately the [LA Open Data](https://data.lacity.org/) portal has information on all of the
intakes to the shelter over the past few years, so with a bit of coding
I could find my answer. After spending some time digging around I
figured if I was interested there should be at least a few other people
who might also find my examination useful and decided to whip up
this post.

Ok, so with the context out of the way let's load up the data. I'm using
a CSV downloaded from [this
page](https://data.lacity.org/A-Well-Run-City/Animal-Services-Intake-Data/8cmr-fbcu)
but JSON and other formats are available as well. The analysis is all done in [R](https://www.r-project.org/).

    asid <- read.csv('Animal_Services_Intake_Data.csv')
    nrow(asid)

    ## [1] 187593

    asid.type <- table(asid$Animal.Type, asid$Intake.Condition)
    asid.type

    ##            
    ##             < 8 WEEKS ALIVE  DEAD LITTER
    ##   BIRD            717  7490  1113      6
    ##   CAT           18916 40608  1551   2778
    ##   DOG            2313 94811  3408   1481
    ##   EQUINE            0    63     7      0
    ##   LIVESTOCK         0    54    15      0
    ##   OTHER          1388  9968   751    155

Looks like we have 187593 records from 6 categories of animals. That's a
lot of cats, dogs and other creatures! However the data range for this
data isn't 100% clear. Fortunately the lubridate package can help with
that.

    library(lubridate)
    library(plyr)
    library(scales)
    library(ggplot2)
    library(arules)
    library(stargazer)

    asid$Date <- parse_date_time(asid$Intake.Date, 'm/d/Y', tz='PST')
    table(year(asid$Date))

    ## 
    ##  2011  2012  2013 
    ## 65986 63496 58111

    asid.intake.date <- ddply(asid, c('Date', 'Animal.Type'), summarize, Count=length(Date))
    ggplot(asid.intake.date, aes(x=Date, y=Count, color=Animal.Type))+geom_point(size=3, alpha=0.8)

![](http://i.imgur.com/PQw3n0F.png)

There is some clear year over year cyclicality for cats whereas dogs
display a more consistent trend. Birds also appear to be the most prone
to extremely high outliers. We can clean up the data by fitting a
smoothed regression curve for each animal type.

    ggplot(asid.intake.date, aes(x=Date, y=Count, color=Animal.Type))+geom_smooth()

![](http://i.imgur.com/7C1KMJN.png)

Clearly the shelters get the most new residents in the summer months. 

The eight shelters in Los Angeles are in very different parts of the
city. We can view this breakdown in a grid chart with each row
showing the % composition of that shelter's population broken down by
animal type.

    asid.intake.shelter.type <- data.frame(prop.table(table(asid$Shelter, asid$Animal.Type), 1))
    names(asid.intake.shelter.type) <- c('Shelter', 'Animal.Type', 'Freq')
    ggplot(asid.intake.shelter.type, aes(y = Shelter, x = Animal.Type, fill=Freq, label=percent(Freq))) + geom_tile(color='black') + scale_fill_gradient(low='white', high='#3182bd') + geom_text() + xlab("Animal Type")

![](http://i.imgur.com/904habm.png)

Dogs clearly dominate in the N.East while the Annex, W. Valley and W LA
Shelters have a surprising amount of birds. 

For dogs we also have a lot information on the different breeds. Let's break out this data and see what breeds are the most common in LA's animal shelters.

    asid.dog <- subset(asid, Animal.Type == 'DOG')
    asid.dog.tab <- subset(data.frame(table(asid.dog$Breed.1)), Freq > 100)
    ggplot(asid.dog.tab, aes(x=sort(Var1, desc=TRUE), y= Freq, fill=Var1)) + geom_bar(stat='identity') + coord_flip() + xlab('Breed') + ylab('Number of Dogs') +  guides(fill=FALSE)

![](http://i.imgur.com/QkKWO63.png)

Chihuahuas are by far and away the most common dogs in shelters,
followed by Pit Bulls. Let's take a look at cats

    asid.cat <- subset(asid, Animal.Type == 'CAT')
    asid.cat.tab <- subset(data.frame(table(asid.cat$Breed.1)), Freq > 100)
    ggplot(asid.cat.tab, aes(x=sort(Var1, desc=TRUE), y= Freq, fill=Var1)) + geom_bar(stat='identity') + coord_flip() + xlab('Breed') + ylab('Number of Cats') +  guides(fill=FALSE)

![](http://i.imgur.com/KMmwVDJ.png)

A lot less variety here, with the big catch-all category of domestic
short hair being the most common donation.

Finally we can use the [apriori algorithm](https://en.wikipedia.org/wiki/Apriori_algorithm) to search through the various
combinations in the data frame. This tells us what permutations of
shelter/animal and other factor appear together most commonly, so we can
classify each shelter by it's most common patterns. We need to subset
the data down a little beforehand because otherwise we will get
uninformative rules like "Chihuahuas tend to be Dogs." 

    asid$month <- factor(month(asid$Date))
    pet.rules <- apriori(asid[,c('Shelter', 'Intake.Condition', 'Intake.Type', 'Animal.Type', 'month' )])

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport support minlen maxlen
    ##         0.8    0.1    1 none FALSE            TRUE     0.1      1     10
    ##  target   ext
    ##   rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 18759 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[38 item(s), 187593 transaction(s)] done [0.02s].
    ## sorting and recoding items ... [14 item(s)] done [0.01s].
    ## creating transaction tree ... done [0.05s].
    ## checking subsets of size 1 2 3 done [0.00s].
    ## writing ... [13 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.01s].

    subrules <- pet.rules[round(quality(pet.rules)$lift, digits=1) != 1]
    stargazer(as(subrules, 'data.frame'), type="html", summary=FALSE)

<table style="text-align:center">
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
rules
</td>
<td>
support
</td>
<td>
confidence
</td>
<td>
lift
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
2
</td>
<td>
Intake.Condition=\< 8 WEEKS =\> Animal.Type=CAT
</td>
<td>
0.101
</td>
<td>
0.811
</td>
<td>
2.382
</td>
</tr>
<tr>
<td style="text-align:left">
3
</td>
<td>
Intake.Condition=\< 8 WEEKS =\> Intake.Type=STRAY
</td>
<td>
0.112
</td>
<td>
0.899
</td>
<td>
1.422
</td>
</tr>
<tr>
<td style="text-align:left">
7
</td>
<td>
Intake.Type=OWNER SUR =\> Intake.Condition=ALIVE
</td>
<td>
0.178
</td>
<td>
0.867
</td>
<td>
1.063
</td>
</tr>
<tr>
<td style="text-align:left">
8
</td>
<td>
Animal.Type=CAT =\> Intake.Type=STRAY
</td>
<td>
0.280
</td>
<td>
0.824
</td>
<td>
1.303
</td>
</tr>
<tr>
<td style="text-align:left">
9
</td>
<td>
Animal.Type=DOG =\> Intake.Condition=ALIVE
</td>
<td>
0.505
</td>
<td>
0.929
</td>
<td>
1.140
</td>
</tr>
<tr>
<td style="text-align:left">
10
</td>
<td>
Shelter=S LA,Animal.Type=DOG =\> Intake.Condition=ALIVE
</td>
<td>
0.104
</td>
<td>
0.934
</td>
<td>
1.145
</td>
</tr>
<tr>
<td style="text-align:left">
11
</td>
<td>
Intake.Type=OWNER SUR,Animal.Type=DOG =\> Intake.Condition=ALIVE
</td>
<td>
0.123
</td>
<td>
0.886
</td>
<td>
1.087
</td>
</tr>
<tr>
<td style="text-align:left">
12
</td>
<td>
Shelter=E VALLEY,Animal.Type=DOG =\> Intake.Condition=ALIVE
</td>
<td>
0.125
</td>
<td>
0.926
</td>
<td>
1.135
</td>
</tr>
<tr>
<td style="text-align:left">
13
</td>
<td>
Intake.Type=STRAY,Animal.Type=DOG =\> Intake.Condition=ALIVE
</td>
<td>
0.296
</td>
<td>
0.936
</td>
<td>
1.148
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
</table>

Each rule gets' three criteria. Support is the proportion of cases that
fit the rule over all of cases. Confidence is the proportion of cases
that fit the rule over the number of cases that have one value that
matches the rule. Finally lift tells us whether knowing one half of the
rule allows us to make good predictions about the other half. So if we
know that an animal was under 8 weeks old when it was turned in the high
lift means that we can be pretty confident that it is a cat.

![From
<http://www.saedsayad.com/association_rules.htm>](http://i.imgur.com/1oocjfb.png)

Judging by these rules, extremely young animals also tend to be cats,
and there are more stray cats. Dogs are more likely to be taken in
alive, especially at the E. Valley and S. LA Shelters. The association
between cats being turned into shelters as strays is interesting given
the recent [Kitten Convict
project](http://explodingkittens.com/kittyconvict) which highlights the
fact that a lot of lost cats don't get returned as everyone thinks that
they are outdoor cats. I wonder if some of those lost cats end up in
shelters as well as strays.

So, the moral of the story is short haired cats, Chihuahuas, Pit Bulls
all end up in shelters in LA. Furthermore a lot of people drop off
extremely young cats. So if you've got room in your home consider going
to [Pet Harbor](http://petharbor.com) and adopting any animal!

