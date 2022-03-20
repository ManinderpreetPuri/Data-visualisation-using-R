#Task-1 a.
#Importing libraries
library(dplyr)
library(ggplot2)
library(gghighlight)
library(reshape2)

#Reading data
covid <- read.csv(file = 'covid_au_state.csv')

#Formatting dates column
covid$date <- as.Date(covid$date, format = "%d/%m/%Y")

#Grouping confirmed column by dates column
a <- covid %>%
  mutate(date = as.Date(date,format="%d/%m/%Y")) %>% 
  group_by(date) %>%       
  summarize(confirmed = sum(confirmed))
  
#Plotting data
  ggplot(a,aes(x =date , y = confirmed)) +
  geom_line(aes(color = confirmed), size = 0.8, color = "red") +
  gghighlight(confirmed >= 610, label_key = confirmed, unhighlighted_params = list(colour="blue" ,size = .5)) +
  labs(x = 'Date', y = 'New Confirmed cases(every day)', 
       title = 'Total number of new confirmed cases daily throughout Australia')


#Task-1 b.
#Subsetting data  
TEST <- subset(covid,date>"2020-03-17",select=c(date,state_abbrev,confirmed))

#Reshaping columns to rows
Test <-  reshape(TEST,timevar = 'state_abbrev', idvar = 'date', direction = 'wide')

#Finding Growth factor
Test$ACT <- Test$confirmed.ACT/lag(Test$confirmed.ACT)
Test$NSW <- Test$confirmed.NSW/lag(Test$confirmed.NSW)
Test$NT <- Test$confirmed.NT/lag(Test$confirmed.NT)
Test$QLD <- Test$confirmed.QLD/lag(Test$confirmed.QLD)
Test$SA <- Test$confirmed.SA/lag(Test$confirmed.SA)
Test$TAS <- Test$confirmed.TAS/lag(Test$confirmed.TAS)
Test$VIC <- Test$confirmed.VIC/lag(Test$confirmed.VIC)
Test$WA <- Test$confirmed.WA/lag(Test$confirmed.WA)


#Removing NA,NAN,Infinite values
Test$ACT[is.infinite(Test$ACT)] <- 0
Test$ACT[is.na(Test$ACT)] <- 0
Test$NSW[is.infinite(Test$NSW)] <- 0
Test$NSW[is.na(Test$NSW)] <- 0
Test$NT[is.infinite(Test$NT)] <- 0
Test$NT[is.na(Test$NT)] <- 0
Test$QLD[is.infinite(Test$QLD)] <- 0
Test$QLD[is.na(Test$QLD)] <- 0
Test$SA[is.infinite(Test$SA)] <- 0
Test$SA[is.na(Test$SA)] <- 0
Test$TAS[is.infinite(Test$TAS)] <- 0
Test$TAS[is.na(Test$TAS)] <- 0
Test$VIC[is.infinite(Test$VIC)] <- 0
Test$VIC[is.na(Test$VIC)] <- 0
Test$WA[is.infinite(Test$WA)] <- 0
Test$WA[is.na(Test$WA)] <- 0


#subsetting data for plotting
Gf1 <- subset(Test, select = c(date,ACT,NSW,NT,QLD,SA,TAS,VIC,WA))
act <- ggplot(Gf1, aes(x = date, y=ACT)) + geom_line()
nsw <- ggplot(Gf1, aes(x = date, y=NSW))+ geom_line()
nt <- ggplot(Gf1, aes(x = date, y=NT)) + geom_line()
qld <- ggplot(Gf1, aes(x = date, y=QLD))+ geom_line()
sa <- ggplot(Gf1, aes(x = date, y=SA)) + geom_line()
tas <- ggplot(Gf1, aes(x = date, y=TAS))+ geom_line()
vic <- ggplot(Gf1, aes(x = date, y=VIC)) + geom_line()
wa <- ggplot(Gf1, aes(x = date, y=WA))+ geom_line()


#reshaping data
Gf1 <- melt(Gf1, id.vars = 'date')

#Creating line chart
ggplot(Gf1, aes(x= date, y= value, color= variable)) +
geom_smooth(alpha= 0)  + 
labs(title= "Daily growth factor across all states in Australia", x = "Months", y= "Growth factor") 


