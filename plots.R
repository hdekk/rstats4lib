## Edit and uncomment the next line to point to the directory where 
## circsample.csv is located.

# test for >= ver.2.14 dependency



library(ggplot2)
library(gmodels)
library(plyr)
library(portfolio)

## Description
# The data file circsample.csv is a sample of 27,567 monograph titles purchased during
# a particular time period. There are 4 variables:
#  LC.number - Library of Congress classification of the title
#  circ      - number of times the title has circulated
#  lang      - language (3 letter code)
#  cntry     - country of publication (2 letter code)

## Be sure to set a working directory (where your data is located). Uncomment the 
#  next line and edit accordingly.

# setwd("/YOUR_WORK_DIRECTORY")
books <- read.csv("circsample.csv", col.names=c("LC.number", "circ", "lang", "ctry" ))

## Create some new variables by extracting parts of the LC number

books$LC.clean <- as.character(books$LC.number)
books$LC.clean <- sub("^\\s+", "", books$LC.clean)
books$LC.clean <- sub("^f+\\s+", "", books$LC.clean)  #strip out folio f's'
m <- gregexpr("^[a-zA-Z]+", books$LC.clean)
subclass.matches <- regmatches(books$LC.clean, m)
books$LC.subclass <- as.factor(toupper(as.character(subclass.matches)))
books$LC.class <- substr(as.character(books$LC.subclass), 1,1)
books$LC.class <- as.factor(substr(as.character(books$LC.subclass), 1,1))

## Do some workspace cleanup

rm(subclass.matches)
rm(books$LC.clean)
rm(m)

## Assign value labels to LC.class

class.code = c("A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","Z")
class.description <- c("General Works", "Philosophy, Psychology, Religion", "Auxiliary Sciences of History", "History - General & Eastern Hemisphere", "History - Western Hemisphere 1", "History - Western Hemisphere 2", "Geography, Anthropology, Recreation", "Social Sciences", "Political Science", "Law", "Education", "Music", "Fine Arts", "Language and Literature", "Sciences", "Medicine", "Agriculture", "Technology", "Military Science", "Naval Science", "History of Books, Library Science, Bibliography")
books$LC.class <- factor(books$LC.class, levels = class.code, labels = class.description)

## Create three new variables that we'll use later in various plots

## books$circ.any - a factor to indicate any circulation activity
## 0 = title hasn't circulated during review period
## 1 = title has circulated during review period

books$circ.any <- books$circ
books$circ.any[books$circ > 0] <- 1
books$circ.any <- factor(books$circ.any, levels = c(0, 1), labels = c("Not Circulated", "Circulated"))

## books$circ.multi - a factor to indicate two or more circs
##   0 = title has circulated less than two times during review period
##   1 = title has circulated two or more times during review period

books$circ.multi <- 0
books$circ.multi[books$circ > 1] <- 1
sum(books$circ.multi)

## create a dummy variable to indicate English language
## 0 = title has circulated less than two times during review period
## 1 = title has circulated two or more times during review period

books$english <- 0
books$english[books$lang == "eng"] <- 1
books$english <-factor(books$english, levels = c(0, 1), labels = c("Non-English", "English"))

sum(books$english)

lang.tbl <- table(books$english,books$circ.any)
barchart(lang.tbl, stack=FALSE, auto.key=TRUE)

circ.tbl <- table(books$LC.class, books$english, books$circ.any)
barchart(circ.tbl, stack=FALSE, auto.key=TRUE)
CrossTable(books$LC.class, books$circ.any, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, format="SPSS")
CrossTable(books$english, books$circ.any, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, format="SPSS")

circ3 <- ddply(books, .(LC.subclass, LC.class, english), summarize, circ=sum(circ.any), items=nrow(piece))
circ4 <- ddply(books, .(LC.class, lang), summarize, circ=sum(circ.any), items=nrow(piece))

## calculate a total circulation ratio that includes multiple circulations for a title

circ1$circ.rate = 100 * circ1$circ/circ1$items
circ2$circ.rate = 100 * circ2$circ/circ2$items

## top code at 150% to reduce the effect of outliers in the treemap visualization

circ1$circ.rate[circ1$circ.rate>150] <- 150
circ2$circ.rate[circ2$circ.rate>150] <- 150

## use the mapmarket function from the portfolio package to draw a treemap 

map.market(id=circ1$LC.subclass, group=circ1$LC.class, area=circ1$items, color=circ1$circ.rate, main="Total Circulation by LC Subclass")
map.market(id=circ2$LC.subclass, group=circ2$LC.class, area=circ2$items, color=circ2$circ.rate, main="Circulation by Title and LC Subclass")
