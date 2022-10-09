# Name: Christiaan Lombard

# Student Number:u21439487

#WST212 Project

#If below packages are not installed comment out install code
#install.packages("package_name")
#install.packages("sqldf")
#install.packages("readr")
#install.packages("RH2")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("gapminder")
#install.packages("stringr")
#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("ggthemes")
#install.packages("corrr")

#Packages:
library(sqldf)
library(readr)
library(RH2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gapminder)
library(stringr)
library(tidyverse)
library(janitor)
library(ggthemes)
library(corrr)


#Ensure the CSV file are in the working directory
#To get Working Directory: getwd()

Provinces <- read_csv("ProvincePopulation.csv")
CrimeStats <- read_csv("SouthAfricaCrimeStats_v2.csv")

#Generate Datasets:


#===============================================================================
#Overall Crime Committed in South Africa
#===============================================================================

#Extract the number of crimes committed in every province for every year
#-------------------------------------------------------------------------------
yearly_crime_per_prov <- sqldf('SELECT Province, SUM("2005-2006") AS "2005", SUM("2006-2007") AS "2006", SUM("2007-2008") AS "2007",
                            SUM("2008-2009") AS "2008", SUM("2009-2010") AS "2009", SUM("2010-2011") AS "2010", SUM("2011-2012") AS "2011",
                            SUM("2012-2013") AS "2012", SUM("2013-2014") AS "2013", SUM("2014-2015") AS "2014", SUM("2015-2016") AS "2015"
                            FROM CrimeStats
                            GROUP BY Province')


#Determine the average number of crimes committed in each province over the time period
#-------------------------------------------------------------------------------
total_period_crime_per_prov<- sqldf('SELECT Province, (`2005` + `2006` +
                            `2007` + `2008` + `2009` + `2010` + `2011` +
                            `2012` + `2013` + `2014` + `2015`)/11 AS average_crime
                            FROM yearly_crime_per_prov')

#Determine the total crime committed every year in the whole country
#-------------------------------------------------------------------------------
yearly_crime_country<-yearly_crime_per_prov %>%
  transmute(`2005`=sum(`2005`), `2006`=sum(`2006`), `2007`=sum(`2007`),
            `2008`=sum(`2008`), `2009`=sum(`2009`), `2010`=sum(`2010`), `2011`=sum(`2011`),
            `2012`=sum(`2012`), `2013`=sum(`2013`), `2014`=sum(`2014`), `2015`=sum(`2015`)) %>% slice(1:1) 

#Transpose the data -> add the years to the data -> rename incorrectly named columns
yearly_crime_country<-as.data.frame(t(yearly_crime_country))
yearly_crime_country<-mutate(yearly_crime_country, Year = c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"))
yearly_crime_country<-rename_all(yearly_crime_country, recode, V1 = "Crimes")

#Plot the total number of crimes committed every year in South Africa
#-------------------------------------------------------------------------------
ggplot(yearly_crime_country, aes(Year, Crimes, group = 1)) +
  geom_point(colour="darkgreen", size=4) +
  geom_line(colour="darkgreen", size=1)+
  labs(y="Total Crimes Commited\n",x='\nYear', title = "Crimes Commited Every Year in South Africa")+
  scale_y_continuous(labels=scales::comma)+
  theme_economist()



#===============================================================================
#Crime per Province
#===============================================================================


#Plot average crimes committed every year per province
#-------------------------------------------------------------------------------
ggplot(data=total_period_crime_per_prov, aes(x = reorder(Province, -average_crime), y=`average_crime`))+
  geom_bar(stat='identity', width=0.8, fill='darkgreen')+
  labs(y='Number of Crimes Commited\n', x='\nProvinces', title='Average Number of Crimes Commited from 2005-2015')+
  scale_y_continuous(labels=scales::comma) +
  theme_economist()

#Plot average crimes committed every year per province
#-------------------------------------------------------------------------------
ggplot(data=Provinces, aes(x = reorder(Province, -Density), y=`Density`))+
  geom_bar(stat='identity', width=0.8, fill='darkgreen')+
  labs(y='Population Density\n', x='\nProvinces', title='Population Density per Province')+
  scale_y_continuous(labels=scales::comma) +
  theme_economist()


#Extract the average number of crimes committed per capita per province
#-------------------------------------------------------------------------------
province_crime_per_capita <-sqldf('SELECT Provinces.Province, (total_period_crime_per_prov.average_crime/Provinces.population) AS Crime_per_capita
                            FROM total_period_crime_per_prov, Provinces
                            WHERE Provinces.Province = total_period_crime_per_prov.Province
                            ORDER BY Crime_per_capita DESC')

#Plot the average number of crimes committed per capita per province
#-------------------------------------------------------------------------------
ggplot(data=province_crime_per_capita, aes(x = reorder(Province, -Crime_per_capita), y=Crime_per_capita))+
  geom_bar(stat='identity', width=0.8, fill='darkgreen')+
  labs(x='\nCrimes per Capita', y='Province\n', title="Average Crimes Per Capita for 2005-2015")+
  scale_y_continuous(labels=scales::comma) +
  theme_economist()



#===============================================================================
#Correlation between Crimes Committed and Population Density
#===============================================================================


#Determine the average crimes committed per density over the time period
#-------------------------------------------------------------------------------
densityCrimeCorr<-sqldf('SELECT Provinces.Density, total_period_crime_per_prov.average_crime
                        FROM Provinces, total_period_crime_per_prov
                        WHERE Provinces.province = total_period_crime_per_prov.province')

#Calculate the correlation coefficient
#-------------------------------------------------------------------------------
densityCrimeCorrMatrix <- densityCrimeCorr %>% 
  select(Density, average_crime) %>% 
  correlate(use = "pairwise.complete.obs", method = "pearson")


#Scatter plot for density and crime rate 
#-------------------------------------------------------------------------------
ggplot(data = densityCrimeCorr, aes(x = Density, y = average_crime)) + 
  geom_point(size=2)+
  geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x')+
  labs(y='Average Crimes Commited\n', x='\nDensity', title='Crimes Commited vs Population Density')+
  scale_y_continuous(labels=scales::comma) +
  theme_economist()



#===============================================================================
#Growth Rate for Specific Crimes
#===============================================================================


#Determine the total crime committed every year per type of offence
#-------------------------------------------------------------------------------
crime_per_type<- sqldf('SELECT Category, SUM("2005-2006") AS "2005", SUM("2006-2007") AS "2006", SUM("2007-2008") AS "2007",
                        SUM("2008-2009") AS "2008", SUM("2009-2010") AS "2009", SUM("2010-2011") AS "2010", SUM("2011-2012") AS "2011",
                        SUM("2012-2013") AS "2012", SUM("2013-2014") AS "2013", SUM("2014-2015") AS "2014", SUM("2015-2016") AS "2015"
                        FROM CrimeStats
                        GROUP BY Category')

#Pivot the crime_per_type table -> take the top row of values as new column names -> take the row-index names and make it a Year column
#-------------------------------------------------------------------------------
crime_per_type <- as.data.frame(t(crime_per_type))%>%
  row_to_names(row_number = 1)%>%
  tibble::rownames_to_column("Year")

#Declare a function to calculate the growth rate per crime, use it in a mutate function -> remove 2005, since growth will be NaN
#-------------------------------------------------------------------------------
calcGrowth <- function(x, na.rm = FALSE) ((as.numeric(x) - as.numeric(lag(x)))/as.numeric(lag(x))*100)
growth_per_crime<-crime_per_type %>% mutate_at(vars(-("Year")), calcGrowth) %>% filter(Year!="2005")

#Plot the growth rate per crime 
#-------------------------------------------------------------------------------
ggplot(data=growth_per_crime, aes(x=Year,group = 1))+
  geom_line(aes(y=Carjacking, color="Carjacking"), size=1.5)+
  geom_line(aes(y=Murder, color="Murder"), size=1.5)+
  geom_line(aes(y=`Sexual Offences`,color="Sexual Offences"), size=1.5)+
  geom_line(aes(y=`Drug-related crime`,color="Drug-related crime"), size=1.5)+
  geom_point(aes(y=Carjacking, color="Carjacking"), size=3)+
  geom_point(aes(y=Murder, color="Murder"), size=3)+
  geom_point(aes(y=`Sexual Offences`,color="Sexual Offences"), size=3)+
  geom_point(aes(y=`Drug-related crime`,color="Drug-related crime"), size=3)+
  labs(y='Growth Rate (%)\n', x='\nYear', title='Yearly Growth Rate for Different Crimes',color = "Legend")+
  scale_y_continuous(breaks = seq(-50, 100, 10)) +
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5), legend.direction = "vertical",
        legend.position = c(0.85, 0.18), legend.background = element_rect(fill = "white"),
        legend.key.size = unit(0.35, "cm"), legend.key.width = unit(0.4,"cm") )

