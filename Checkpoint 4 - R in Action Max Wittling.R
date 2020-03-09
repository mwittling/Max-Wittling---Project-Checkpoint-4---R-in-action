# Data Transformations
library(tidyr)
library(tidyverse)
library(skimr)
library(Hmisc)
library(ggplot2)

PPPs = read.csv(choose.files(), header = TRUE)
View(PPPs)
# all of the years are in their own columns, lets rearrange using gather and make a column with years and a column with GDP/capita,PPP

PPPsClean <- PPPs %>%
  gather(`X1990`, `X1991`, `X1992`, `X1993`, `X1994`, `X1995`, `X1996`, `X1997`, `X1998`,`X1999`, `X2000`, `X2001`, `X2002`, `X2003`, `X2004`, `X2005`, `X2006`, `X2007`, `X2008`, `X2009`, `X2010`, `X2011`, `X2012`, `X2013`, `X2014`, `X2015`, `X2016`, `X2017`, `X2018`,  key = "year", value = "GDP per capita,PPP")

View(PPPsClean)
#much better


# Exploratory Data Analysis

  # lets get a look at our data
summary(PPPsClean)
 
 #overall PPPs:
  summary(PPPsClean$`GDP per capita,PPP`)
 
   #filtered to North America
  summary(filter(PPPs, Country.Name=="North America"))
 
   #filtered to Europe and Central Asia:
  EuropeCenAsia <- select(filter(PPPs, Country.Name=="Europe & Central Asia"), -(2:4))
 
   #gather the data so we can graph it with ggplot2
  "Eur&CenAsia" <- EuropeCenAsia %>%
    gather(`X1990`, `X1991`, `X1992`, `X1993`, `X1994`, `X1995`, `X1996`, `X1997`, `X1998`,`X1999`, `X2000`, `X2001`, `X2002`, `X2003`, `X2004`, `X2005`, `X2006`, `X2007`, `X2008`, `X2009`, `X2010`, `X2011`, `X2012`, `X2013`, `X2014`, `X2015`, `X2016`, `X2017`, `X2018`,  key = "year", value = "GDP per capita,PPP")
  
  #lets take alook at Eur & central asias growth in PPP over time
  ggplot(`Eur&CenAsia`, aes(year,`GDP per capita,PPP`)) +  geom_line(aes(group = Country.Name)) + ggtitle("Europe and Central Asia") + scale_x_discrete(limits=c("X1990","X1995","X2000","x2005","X2010", "X2015","X2018"))
  View(EuropeCenAsia)
  
  #this is great, but now lets compare all the regions:
ggplot(PPPsClean, aes(year,`GDP per capita,PPP`)) +  geom_line(aes(group = Country.Name), color = "grey50") + geom_point(aes(color = Country.Name)) + scale_x_discrete(limits=c("X1990","X1995","X2000","x2005","X2010", "X2015","X2018"))



# Models - Regression Model
names(PPPsClean)
PPPslmregres1 <- lm(`GDP per capita,PPP` ~ year, data = PPPsClean)
summary(PPPslmregres1) #only .1397 of the variance in the data is explained by year alone

PPPslmregre2<- lm(`GDP per capita,PPP` ~ Country.Name, data = PPPsClean)
summary(PPPslmregre2) # impressive, region explains .8019 of the data

PPPslmregre3<- lm(`GDP per capita,PPP` ~ Country.Name + year, data = PPPsClean)
summary(PPPslmregre3) 

# wow, these two variables - region and year - explains 94.16% of the data, also,
#notice the negative t value for Sub-Saharan Africa and the crazy high t value for North America compared to the rest. 

#Notice how the years predict the PPP more and more accurately as time goes on, this allows us to inference that globaliztion has been responsible for the increasing world PPP's with 99.999% certainty from 2006 on

#Ordinary least squares regression relies on several assumptions, including that the residuals are normally distributed and homoscedastic, the errors are independent and the relationships are linear.
# we investigate these assumptions by visually by plotting our model:
plot(PPPslmregre3, which = c(1,2))

##

