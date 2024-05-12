## Tabulating categorical or discrete data
## Main tools: table(), prop.table()

## Tables of counts
counts<-rpois(1000,0.6)
table(counts)

disease<-read.table("Datasets/disease.txt",header=T)
attach(disease)
head(disease)

table(status,gender) #table(rowvar, columnvar)
table(gender,status)

## Tables of proportions
disease_tab<-table(gender,status)
disease_tab

prop.table(disease_tab,margin=1) #row-wise proportions
prop.table(disease_tab,margin=2) #column-wise proportions
prop.table(disease_tab) # grand total proportions

## Tabulating summaries of numeric data
## Basics: summary(), mean(), median(), sd(),...
daphnia<-read.table("Datasets/Daphnia.txt",header = T)
attach(daphnia)
head(daphnia)
summary(daphnia)

## General summaries by group
#install.packages("psych")
library(psych)
describe(Growth.rate)
describe(Growth.rate,skew = F)

describeBy(Growth.rate,Water) # describeBy(Num, Groups, option)
describeBy(Growth.rate,Water,skew=F)

# Generating summary tables by two categorical variables
describeBy(Growth.rate,list(Water,Daphnia),skew=F)

## Bespoke summaries by group
tapply(Growth.rate,Detergent,mean)
tapply(Growth.rate,Water,mean)
tapply(Growth.rate,Daphnia,mean)

# Bidimensional table
tapply(Growth.rate,list(Daphnia,Detergent),mean)
tapply(Growth.rate,list(Daphnia,Detergent),median)

# tapplying my own function!
tapply(Growth.rate,list(Daphnia,Detergent),
       function(x) sqrt(var(x)/length(x))) # Standard errors

tapply(Growth.rate,list(Daphnia,Detergent,Water),mean)
# Splitted cubic table
# Use flat table!
ftable(tapply(Growth.rate,list(Daphnia,Detergent,Water),mean))

water<-factor(Water,levels=c("Wear","Tyne")) # Changing order
ftable(tapply(Growth.rate,list(Daphnia,Detergent,water),mean))

## Supplying functions with extra arguments!
tapply(Growth.rate,Detergent,mean,trim=0.1)
tapply(Growth.rate,Detergent,mean,na.rm=T)


### Converting between tables and dataframes
## From a table to a dataframe
daph_table<-tapply(Growth.rate,list(Detergent,Daphnia),mean)
daph_table_data<-as.data.frame.table(daph_table)
daph_table_data # Names have been changed

names(daph_table_data)<-c("detergents","daphnia","mean")
head(daph_table_data)                               
detach(daphnia)

# Table format data
setwd("Datasets")
tabledata<-read.table("tabledata.txt",header=T)
head(tabledata)

# From a contingency table to a raw spreadsheet
# By anonymous function
expanded_table<-lapply(tabledata,function(x) rep(x,tabledata$count))
expanded_table<-as.data.frame(expanded_table)
head(expanded_table)

expanded_table<-expanded_table[,-1]
head(expanded_table);tail(expanded_table)

# By repeated indexing
expanded_table2<-tabledata[rep(1:nrow(tabledata),
                               tabledata[['count']]),]
head(expanded_table2)
expanded_table2

## From a dataframe to a table
table(expanded_table)
as.data.frame(table(expanded_table))

contract_table<-as.data.frame(table(expanded_table))
names(contract_table)[4]<-"count"
contract_table
