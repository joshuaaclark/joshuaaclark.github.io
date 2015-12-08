---
layout: post
title: 'Learning to play Hearthstone, with DATA!'
author: Joshua Clark
date: December 8th, 2015
---

I've recently started to play Hearthstone, and I'm absolutely awful. I
usually lose two or three matches for each one that I win, including
some really frustrating closes loses (usually my own fault). In an
attempt to get better I noticed that Blizzard posted [the deck
lists](http://us.battle.net/hearthstone/en/blog/19960161/decklists-of-champions-11-11-2015)
from the World Championships at BlizzCon. In the comments
[Yriii](https://www.reddit.com/user/Yriii) collected a data file on all of the
decks and made a really [cool Tableau dashboard](http://tabsoft.co/1SJpMlG) (go check it out, it is awesome). Inspired by his efforts
and wanting to up my own game I downloaded the data and decided to do
some quick network analysis and visualization with R, igraph and
ggplot2.

Basically I want to see how the pros play and how their decks relate
both to each other and their win rate.

First up is reading the data into R for analysis, which is easily done
with some base commands.

    hearth <- read.csv('Card_Scatterplot_data.csv', stringsAsFactors = FALSE)
    head(hearth)

    ##   Card.Rarity               Card Deck.Count          Card.Name Mana
    ## 1      Common Druid of the Saber         10 Druid of the Saber    2
    ## 2      Common Druid of the Saber         10 Druid of the Saber    2
    ## 3      Common        Leper Gnome         10        Leper Gnome    1
    ## 4      Common        Leper Gnome         10        Leper Gnome    1
    ## 5      Common Refreshment Vendor         40 Refreshment Vendor    4
    ## 6      Common  Druid of the Claw          4  Druid of the Claw    5
    ##   Deck.Class Deck.Id                            Event Player.Name Rarity
    ## 1      druid      10 2015 Blizzcon World Championship        nias Common
    ## 2      druid      10 2015 Blizzcon World Championship        nias Common
    ## 3      druid      10 2015 Blizzcon World Championship        nias Common
    ## 4      druid      10 2015 Blizzcon World Championship        nias Common
    ## 5      druid      40 2015 Blizzcon World Championship      lovecx Common
    ## 6      druid       4 2015 Blizzcon World Championship     hotform Common
    ##   Card.Type
    ## 1    Minion
    ## 2    Minion
    ## 3    Minion
    ## 4    Minion
    ## 5    Minion
    ## 6    Minion

We've got data about cards and players mixed together, so for clarity
let's break it out into two separate files. First up are the cards,
let's take a look at the mana curve and rarities for all of the
championship decks.

    library(reshape2)
    card.ref <- unique(hearth[,c('Card', 'Mana', 'Rarity', 'Card.Type')])
    library(ggplot2)
    ggplot(card.ref, aes(x = Mana, fill = Rarity)) + geom_bar(binwidth=1, origin = -0.5, color='black') + scale_x_continuous(breaks=0:20) 

![](http://i.imgur.com/6VkXNVv.png)

One and two mana cards are the most common with a fairly smooth taper
off towards 10. Let's also take a look at the relative mana
distributions across players and classes. For this we'll need a violin
plot, where the y-axis shows the distribution of mana in the deck and
the width of the figure represents how many cards in the deck have that
value.

    ggplot(hearth, aes(x = Deck.Class, y = Mana, fill= Deck.Class)) + geom_violin() + facet_grid(Player.Name ~ .) + xlab('Deck Class') + guides(fill=FALSE)

![](http://i.imgur.com/fALVEAX.png)

Looks like hunters tend to bring out a lot more low cost cards while
druids come into play a little later.

For players let's summarize what decks they have and their total mana
cost across all three decks. Additionally we can fold in data about
where each participant is from and their total wins from the [Team
Liquid
Wiki](http://wiki.teamliquid.net/hearthstone/2015_Hearthstone_World_Championship).

    library(plyr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'
    ## 
    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    hearth$player.combo <- paste(hearth$Player.Name, hearth$Deck.Class, sep=',')
    player.ref <- ddply(hearth, c('player.combo'), summarize, 
          total.mana = sum(Mana),
          player.name = max(Player.Name),
          decks = paste(unique(Deck.Class), collapse=','))


    player.details <- data.frame(player.name = unique(player.ref$player.name),
                                 wins= c(7, 13, 6, 9, 10, 4, 3, 5, 4, 3, 2, 15, 9, 5, 11, 8),
                                 location = c('CN', 'NA', 'NA', 'AP', 'AP', 'EU', 'CN', 'AP', 'EU', 'NA', 'CN', 'EU', 'AP', 'NA', 'EU', 'CN') )
    player.ref<-left_join(player.ref, player.details)

    ## Joining by: "player.name"

    ggplot(player.ref, aes(x=location, y=wins)) + geom_boxplot()

![](http://i.imgur.com/SpD2NCX.png)

An EU player won the tournament so their distribution as the highest ceiling, it is also interesting how clustered all the
players from the Asia-Pacific region are in performance.

The fun thing about decks is that each card doesn't operate
independently. Combos and other strategies often depend on chains of
cards being in the deck at the same time. Most statistical measures
consider each case as independent, so each card in a deck is a stand
alone case. However graph/network analysis focuses on the relationship
between different objects. This allows us to look at what pairings,
triads or larger grouping of cards are found together in a deck.

Network analysis needs two things, an edge list which lists all of the
relationships and a node list that lists all of the objects. This
network is going to be a bimodal network, it will have two types of
nodes, players and cards. If a player has used a card in their deck they
will be linked. Both edges (relationships) and nodes (cards or people)
have attributes which are attached to them. Cards will have their mana
count as an example. Let's build the network!

    library(igraph)

    ## 
    ## Attaching package: 'igraph'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     %>%, as_data_frame, groups, union
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum
    ## 
    ## The following object is masked from 'package:base':
    ## 
    ##     union

    hearth.edge <- hearth[,c('Card', 'player.combo')] #grab the data for the edges
    hearth.net <- graph.data.frame(hearth.edge) #make a basic network
    hearth.nl <- get.data.frame(hearth.net, what='vertices') #get a list of all the nodes
    library(dplyr)
    hearth.nl <- left_join(hearth.nl, card.ref, by=c('name' = 'Card')) #attach card info
    hearth.nl <- left_join(hearth.nl, player.ref, by=c('name' = 'player.combo')) #attach player info
    hearth.nl$type <- ifelse(is.na(hearth.nl$Rarity)==TRUE, TRUE, FALSE) #Flag if a node represents a player or card
    library(igraph)
    hearth.net <- graph.data.frame(hearth.edge, vertices=hearth.nl, directed=FALSE) #rebuild the network
    l <- layout_nicely(hearth.net)
    plot(hearth.net, vertex.color= V(hearth.net)$type, vertex.size = 4, vertex.label=ifelse(V(hearth.net)$type==TRUE, V(hearth.net)$name, NA), vertex.label.family='Helvetica', vertex.label.color='black', layout=l) #visualize

![](http://i.imgur.com/t9JHyXD.png)

Currently there are multiple edges connecting some nodes. So if there is
a card that appears two of a player's three decks the card edge will be
linked by two edges. For simplicity these muti-edges can be collapsed
into one with an attribute called weight. Therefore the two edges from
beforehand are collapsed into one with a weight value of 2.

    E(hearth.net)$weight <- 1
    hearth.w.net <- simplify(hearth.net, edge.attr.comb = 'sum')

    plot(hearth.w.net, vertex.color= V(hearth.w.net)$type, vertex.size = 3, vertex.label=NA, edge.color=ifelse(E(hearth.w.net)$weight>1, 'red', 'black'), layout=l)

![](http://i.imgur.com/b5e2eMV.png)

Let's transform the bipartite network into two other ones. The first
only consists of cards, each edge between two cards means that they are
in the same deck. The other is only players, with edges showing how many
cards their decks have in common.

    hearth.proj <- bipartite_projection(hearth.net)
    card.net <- hearth.proj[[1]]
    player.net <- hearth.proj[[2]]
    l.c <- layout_nicely(card.net)
    #Card Network
    plot(card.net, vertex.label=NA, vertex.color=factor(V(card.net)$Rarity), vertex.size=3, layout=l)

![](http://i.imgur.com/DeG2ddQ.png)

    l.p <- layout_nicely(player.net)
    plot(player.net, vertex.size=3, layout=l.p)
    #Player Network

![](http://i.imgur.com/q0WL2O3.png)

First up is the card deck. Let's compute some [network centrality measures](https://en.wikipedia.org/wiki/Centrality).
Centrality refers to the position of a given node (card) within the
network. There are a couple different centrality measures. Closeness
centrality is the "Kevin Bacon" card of the tournament. Like the seven
degrees of Kevin Bacon the card with the highest closeness centrality
can "hop" along edges to get to any of the other cards quickly. The card
with the highest closeness centrality is therefore at the intersection
of a lot of different decks.

Betweenness captures if a card is on the shortest path between two other
cards. So if a card is part of a lot of multi-card combos or tends to
appear with multiple other cards in a deck it should have high
betweenness.

Eigenvector centrality is a measure of the 'prestige' of a card. If card
is connected to a lot of other cards which are also well connected it
will have higher eigenvector centrality. Pagerank, Google's search
algorithm is a cousin of this measure. Google scores a page high if it
is linked too by other pages with a lot of links. Eigenvector centrality
will score a card high if it appears it decks alongside other popular
cards.

    card.close <- sort(closeness(card.net, normalized= TRUE))
    card.btwn <- sort(betweenness(card.net, normalized= TRUE))
    card.egn <- sort(evcent(card.net)$vector)

    tail(card.close)

    ##          Bear Trap  Mind Control Tech Refreshment Vendor 
    ##          0.3441227          0.3458904          0.3500867 
    ##        Alexstrasza            Loatheb           Dr. Boom 
    ##          0.3513043          0.3550088          0.4089069

    tail(card.btwn)

    ## Southsea Deckhand       Alexstrasza      Blood Knight      Cone of Cold 
    ##        0.06545342        0.07215199        0.08824178        0.09118004 
    ##           Loatheb          Dr. Boom 
    ##        0.16108166        0.38000917

    tail(card.egn)

    ##               Swipe           Innervate     Ancient of Lore 
    ##           0.8198673           0.8198673           0.8198673 
    ## Keeper of the Grove     Force of Nature    Piloted Shredder 
    ##           0.8198673           0.8198673           1.0000000

So for closeness, Dr. Boom, Loatheb and Alexstrasza are the most "Kevin
Bacon" like of the cards in the championship deck. They may not be the
most popular but they are connected to a lot of different parts of the
card network so you can get from one to any other part easily.

Betweenness sees Dr. Boom, Loatheb and the Cone of Cold as the cards
which bridge otherwise unconnected parts of the network. So if you had
two distinct decks these would be the most likely ones to overlap. This
makes sense as these are neutral cards that activate in conjunction with
others, so they can be paired with a large variety of decks.

Eigenvector shows the Piloted Shredder, Innvervate and Force of Nature,
these are the most "prestigious" cards, the ones that are most likely to
be picked alongside other really popular cards. Clearly these were
popular choices at the tournament. This is probably due to the high
number of druids in play.

Given that these players are some of the best in the world I was also
interested in how much their decks differed from each others. Do all of
the members of the tournament follow similar strategies or do some
innovate? Also does one path lead to more victories?

One way to count how similar the decks are is to see how many cards
overlap between two players. But this doesn't take into account that
cards are non independent. Two decks may share a card but use it totally
different ways. Given that we've already got a card network I looked at
some of the network literature and came across this paper by Brian Uzzi.
Basically it details a methods of how to determine how conventional or
unconventional a pairing is in a network.

As an example the Murloc Knight and Shielded Minibot cards appear
together six times. This may seem like a lot but it is also important to
consider their relative popularity. How can we say that their rate of
co-occurrence is more or less than what we'd expect by chance? One way
is to consider some alternate realities. Each card in the network has a
degree score, which is the total number of connections it has in the
network, in other words it's relative popularity. Let's look at the
Murloc Knight as an example.

    mk <- make_ego_graph(card.net, 1, nodes='Murloc Knight')[[1]]
    plot(mk, vertex.color=ifelse(V(mk)$name=='Murloc Knight', 'red', 'dodger blue'))

![](http://i.imgur.com/S9toQuT.png)

    head(get.data.frame(mk))

    ##          from               to weight
    ## 1 Zombie Chow     Ironbeak Owl      4
    ## 2 Zombie Chow Piloted Shredder     10
    ## 3 Zombie Chow  Antique Healbot      1
    ## 4 Zombie Chow Shielded Minibot      8
    ## 5 Zombie Chow    Murloc Knight      3
    ## 6 Zombie Chow  Big Game Hunter      2

    graph.strength(mk)['Murloc Knight']

    ## Murloc Knight 
    ##            85

    mk.el <- get.data.frame(mk)
    mk.el.sub<-subset(mk.el, from=='Murloc Knight' | to=='Murloc Knight')
    head(mk.el.sub)

    ##                 from              to weight
    ## 5        Zombie Chow   Murloc Knight      3
    ## 27      Ironbeak Owl   Murloc Knight      3
    ## 48  Piloted Shredder   Murloc Knight      6
    ## 68   Antique Healbot   Murloc Knight      2
    ## 85  Shielded Minibot   Murloc Knight      6
    ## 104    Murloc Knight Big Game Hunter      3

So our Knight has a total of 85 connections distributed throughout the
network, two ties to the Antique Healbot but six co-occurrences with the
Piloted Shredder. With this data we can imagine an alternative reality
where these weights are different, let's say that there are six ties
with the Healbot but only one with the Shredder. Or three and four.
Either scenario preserves the total of seven (in this case) but creates
different patterns of connection. By shuffling the connections within
the card network a few hundred times we can quickly create a bunch of
alternative universes which we can then compare reality to.

If a card's pairings are basically random then there shouldn't be much
difference between the random alternative realities and the actual data.
However if two cards are chosen together more often than chance the
actual weight that we see will be a lot higher than the average of the
weights across all of the alternatives we created. Similarly if two
cards tend *not* to be chosen together their co-occurrence will be
lower. These differences can be captured in a simple statistic called
the z-score, which basically tells us how much higher or lower the
scores from reality are then the average of the all the simulated
scores.

A positive z-score means that a pairing is more conventional, negative
more unconventional. By considering all of the card pairs that a player
has chosen across their decks it is possible to see who was more
conventional or different overall within the tournament.

    library(tnet)

    ## Loading required package: survival
    ## tnet: Analysis of Weighted, Two-mode, and Longitudinal networks.
    ## Type ?tnet for help.

    net <- cbind(get.edgelist(card.net, names=FALSE), E(card.net)$weight) #extract card pairing and weight
    net <- symmetrise_w(net) #get ready
    net <- as.tnet(net, type="weighted one-mode tnet")

    #SHUFFLE
    shuffle <- function(network, seed){
      net2 <- rg_reshuffling_w(network, option="weights.local", seed=seed, directed=FALSE) #create alternative realities
      ed <- net2$w #extract data
    }  
    set.seed(11)
    x1 <- runif(1000, 1, 100000) #do it 1,000 times
    graph.grid.d<-ldply(x1, function(x1) shuffle(net,x1)) #glom all the results together
    graph.grid.t<-as.data.frame(t(graph.grid.d)) #clean up
    names(graph.grid.t)<-paste('n',1:ncol(graph.grid.t), sep='') #name

    n0<-get.data.frame(card.net, what='edges') #grab data again
    head(n0)

    ##                 from                 to weight
    ## 1 Druid of the Saber        Leper Gnome      4
    ## 2 Druid of the Saber  Druid of the Claw      4
    ## 3 Druid of the Saber   Piloted Shredder      4
    ## 4 Druid of the Saber Shade of Naxxramas      4
    ## 5 Druid of the Saber    Ancient of Lore      4
    ## 6 Druid of the Saber Emperor Thaurissan      2

    graph.grid.top<-graph.grid.t[(1:nrow(n0)),] #grab the matching data (shuffle creates two entries for each pair but igraph just needs one.)
    gg.fin<-cbind(n0, graph.grid.top) #stick it all together

    gg.fin$simmean<-apply(gg.fin[4:ncol(gg.fin)],1,mean) #mean of simulations
    gg.fin$simsd<-apply(gg.fin[4:ncol(gg.fin)],1,sd) # SD of sims
    gg.fin$zs<-(gg.fin$weight-gg.fin$simmean)/gg.fin$simsd #Z-score


    gg.trim <- gg.fin[, c('to','from','weight','zs')] #put it all together neatly
    head(gg.trim)

    ##                   to               from weight         zs
    ## 1        Leper Gnome Druid of the Saber      4  0.4084864
    ## 2  Druid of the Claw Druid of the Saber      4  0.3692745
    ## 3   Piloted Shredder Druid of the Saber      4  0.3933630
    ## 4 Shade of Naxxramas Druid of the Saber      4  0.3831305
    ## 5    Ancient of Lore Druid of the Saber      4  0.4134733
    ## 6 Emperor Thaurissan Druid of the Saber      2 -2.5098347

    z.net <- graph.data.frame(gg.trim, directed=FALSE) #rebuild the network,
    combo.net <- hearth.net + z.net #add it back to og network, links players with cards
    player.ref$convention <- sapply(player.ref$player.combo, function(x) sum(E(make_ego_graph(combo.net, 1, x)[[1]])$zs, na.rm=TRUE)) #get convetnionality score for each deck.

    library(stargazer)

    ## 
    ## Please cite as: 
    ## 
    ##  Hlavac, Marek (2015). stargazer: Well-Formatted Regression and Summary Statistics Tables.
    ##  R package version 5.2. http://CRAN.R-project.org/package=stargazer

    stargazer(arrange(player.ref[,c('player.combo', 'total.mana', 'convention')], convention), type='html', summary=FALSE)

<table style="text-align:center">
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
player.combo
</td>
<td>
total.mana
</td>
<td>
convention
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
1
</td>
<td>
lifecoach,warlock
</td>
<td>
170
</td>
<td>
-74.285
</td>
</tr>
<tr>
<td style="text-align:left">
2
</td>
<td>
diemeng,shaman
</td>
<td>
63
</td>
<td>
-45.202
</td>
</tr>
<tr>
<td style="text-align:left">
3
</td>
<td>
thijs,priest
</td>
<td>
104
</td>
<td>
25.332
</td>
</tr>
<tr>
<td style="text-align:left">
4
</td>
<td>
pinpingho,shaman
</td>
<td>
100
</td>
<td>
129.144
</td>
</tr>
<tr>
<td style="text-align:left">
5
</td>
<td>
neilyo,warrior
</td>
<td>
120
</td>
<td>
139.214
</td>
</tr>
<tr>
<td style="text-align:left">
6
</td>
<td>
lifecoach,warrior
</td>
<td>
114
</td>
<td>
159.159
</td>
</tr>
<tr>
<td style="text-align:left">
7
</td>
<td>
hotform,mage
</td>
<td>
92
</td>
<td>
188.359
</td>
</tr>
<tr>
<td style="text-align:left">
8
</td>
<td>
jab,mage
</td>
<td>
87
</td>
<td>
315.386
</td>
</tr>
<tr>
<td style="text-align:left">
9
</td>
<td>
thijs,warrior
</td>
<td>
81
</td>
<td>
352.817
</td>
</tr>
<tr>
<td style="text-align:left">
10
</td>
<td>
diemeng,hunter
</td>
<td>
58
</td>
<td>
353.022
</td>
</tr>
<tr>
<td style="text-align:left">
11
</td>
<td>
kranich,warlock
</td>
<td>
95
</td>
<td>
367.533
</td>
</tr>
<tr>
<td style="text-align:left">
12
</td>
<td>
lovecx,warlock
</td>
<td>
88
</td>
<td>
369.054
</td>
</tr>
<tr>
<td style="text-align:left">
13
</td>
<td>
ostkaka,rogue
</td>
<td>
86
</td>
<td>
426.239
</td>
</tr>
<tr>
<td style="text-align:left">
14
</td>
<td>
hotform,rogue
</td>
<td>
86
</td>
<td>
440.618
</td>
</tr>
<tr>
<td style="text-align:left">
15
</td>
<td>
zoro,hunter
</td>
<td>
65
</td>
<td>
441.553
</td>
</tr>
<tr>
<td style="text-align:left">
16
</td>
<td>
purple,rogue
</td>
<td>
88
</td>
<td>
497.514
</td>
</tr>
<tr>
<td style="text-align:left">
17
</td>
<td>
ostkaka,warrior
</td>
<td>
88
</td>
<td>
513.919
</td>
</tr>
<tr>
<td style="text-align:left">
18
</td>
<td>
notomorrow,hunter
</td>
<td>
67
</td>
<td>
578.952
</td>
</tr>
<tr>
<td style="text-align:left">
19
</td>
<td>
kno,warlock
</td>
<td>
89
</td>
<td>
597.674
</td>
</tr>
<tr>
<td style="text-align:left">
20
</td>
<td>
lovecx,paladin
</td>
<td>
118
</td>
<td>
608.781
</td>
</tr>
<tr>
<td style="text-align:left">
21
</td>
<td>
kno,paladin
</td>
<td>
119
</td>
<td>
611.119
</td>
</tr>
<tr>
<td style="text-align:left">
22
</td>
<td>
nias,hunter
</td>
<td>
80
</td>
<td>
809.899
</td>
</tr>
<tr>
<td style="text-align:left">
23
</td>
<td>
diemeng,paladin
</td>
<td>
82
</td>
<td>
868.611
</td>
</tr>
<tr>
<td style="text-align:left">
24
</td>
<td>
thijs,mage
</td>
<td>
102
</td>
<td>
944.999
</td>
</tr>
<tr>
<td style="text-align:left">
25
</td>
<td>
ostkaka,mage
</td>
<td>
103
</td>
<td>
948.882
</td>
</tr>
<tr>
<td style="text-align:left">
26
</td>
<td>
purple,mage
</td>
<td>
108
</td>
<td>
948.882
</td>
</tr>
<tr>
<td style="text-align:left">
27
</td>
<td>
jab,hunter
</td>
<td>
88
</td>
<td>
968.730
</td>
</tr>
<tr>
<td style="text-align:left">
28
</td>
<td>
neirea,mage
</td>
<td>
111
</td>
<td>
970.598
</td>
</tr>
<tr>
<td style="text-align:left">
29
</td>
<td>
nias,mage
</td>
<td>
111
</td>
<td>
970.598
</td>
</tr>
<tr>
<td style="text-align:left">
30
</td>
<td>
pinpingho,hunter
</td>
<td>
90
</td>
<td>
972.104
</td>
</tr>
<tr>
<td style="text-align:left">
31
</td>
<td>
kranich,hunter
</td>
<td>
86
</td>
<td>
990.616
</td>
</tr>
<tr>
<td style="text-align:left">
32
</td>
<td>
neilyo,hunter
</td>
<td>
90
</td>
<td>
1,002.582
</td>
</tr>
<tr>
<td style="text-align:left">
33
</td>
<td>
zoro,paladin
</td>
<td>
84
</td>
<td>
1,035.753
</td>
</tr>
<tr>
<td style="text-align:left">
34
</td>
<td>
neilyo,paladin
</td>
<td>
92
</td>
<td>
1,040.213
</td>
</tr>
<tr>
<td style="text-align:left">
35
</td>
<td>
neirea,paladin
</td>
<td>
95
</td>
<td>
1,041.619
</td>
</tr>
<tr>
<td style="text-align:left">
36
</td>
<td>
notomorrow,paladin
</td>
<td>
86
</td>
<td>
1,045.557
</td>
</tr>
<tr>
<td style="text-align:left">
37
</td>
<td>
nias,druid
</td>
<td>
105
</td>
<td>
2,233.152
</td>
</tr>
<tr>
<td style="text-align:left">
38
</td>
<td>
neirea,druid
</td>
<td>
119
</td>
<td>
3,144.638
</td>
</tr>
<tr>
<td style="text-align:left">
39
</td>
<td>
purple,druid
</td>
<td>
119
</td>
<td>
3,144.638
</td>
</tr>
<tr>
<td style="text-align:left">
40
</td>
<td>
kranich,druid
</td>
<td>
117
</td>
<td>
3,288.334
</td>
</tr>
<tr>
<td style="text-align:left">
41
</td>
<td>
jab,druid
</td>
<td>
115
</td>
<td>
3,303.764
</td>
</tr>
<tr>
<td style="text-align:left">
42
</td>
<td>
hotform,druid
</td>
<td>
117
</td>
<td>
3,328.829
</td>
</tr>
<tr>
<td style="text-align:left">
43
</td>
<td>
kno,druid
</td>
<td>
118
</td>
<td>
3,335.305
</td>
</tr>
<tr>
<td style="text-align:left">
44
</td>
<td>
lifecoach,druid
</td>
<td>
116
</td>
<td>
3,487.367
</td>
</tr>
<tr>
<td style="text-align:left">
45
</td>
<td>
notomorrow,druid
</td>
<td>
119
</td>
<td>
3,573.480
</td>
</tr>
<tr>
<td style="text-align:left">
46
</td>
<td>
pinpingho,druid
</td>
<td>
114
</td>
<td>
3,616.308
</td>
</tr>
<tr>
<td style="text-align:left">
47
</td>
<td>
lovecx,druid
</td>
<td>
120
</td>
<td>
3,629.818
</td>
</tr>
<tr>
<td style="text-align:left">
48
</td>
<td>
zoro,druid
</td>
<td>
123
</td>
<td>
3,667.808
</td>
</tr>
<tr>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
</table>
So the most novel deck (within the tournament) is Lifecoach's Warlock
while the most conventional is Zoro's Druid. As a crude metric let's
compare the mana curve of Lifecoach's deck to the other Warlocks.

    wlock <- subset(hearth, Deck.Class=='warlock')
    ggplot(wlock, aes(x=Player.Name, y=Mana, fill=Player.Name)) + geom_violin()

![](http://i.imgur.com/jivFWju.png)

Very different with that investment in high mana cards. Let's also
compare the druids

    druid <- subset(hearth, Deck.Class=='druid')
    ggplot(druid, aes(x=Player.Name, y=Mana, fill=Player.Name)) + geom_violin()

![](http://i.imgur.com/CfAehKl.png)

Much more uniform, which is probably why we see a lot more druids towards the
top of the conventionality ratings.

Since these ratings have passed the initial sanity test let's see how
they relate to success within the tournament.

    player.ref <- cbind(player.ref, colsplit(player.ref$player.combo, ',', c('Player.Name', 'Deck')))
    player.ref<- left_join(player.ref, player.details)

    ## Joining by: c("player.name", "wins", "location")

    player.conv<-ddply(player.ref, 'Player.Name', summarize, 
          tot.convention=sum(convention),
          wins = max(wins),
          tot.mana = sum(total.mana),
          location = max(as.character(location)))

    ggplot(player.conv, aes(x=wins, y=tot.convention, color=tot.mana, label=Player.Name)) +  geom_text(size=4) + xlab('Wins') + ylab('Conventionality (all decks)') + scale_colour_continuous(name = "Total Mana (all decks)", low='dodger blue', high='red') + geom_smooth()

    ## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.

![](http://i.imgur.com/ogcGU6H.png)

There is a clear trend towards more novel decks also winning more.
HOWEVER it isn't statistically significant, so it may be
that these results are due to chance. This is in part due to the small
sample (16 players), so if anyone has a big database of decks and win
rates let me know! Might be fun to test this technique in a more robust
setting.

This analysis has a number of limitations. I didn't take the time to pull the head to head match ups to see if novel or conventional decks one at each round. Additionally a lot of the statistics are aggregated upwards to the player or deck level instead of looking at the micro-interactions between pairs or triads of cards. Still, I hope it has provided some insight and hopefully it will help me (and others) player a little better.

So there we have it. My Hearthstone win rate hasn't gotten much better
but this was a fun little exploration into the decks being used by the
best of the best. I highly doubt I'll ever be at that level but these
results are encouraging as my play style so far has been to mess with
odd or unusual deck compositions. Who knows maybe I'll find a killer
combo or build and start climbing the ladder. Until then I'll see you in
the 15-20 ranks.

[Josh](http://twitter.com/joshuaaclark)
