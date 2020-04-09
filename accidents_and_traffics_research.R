# clean enviroment
rm(list = ls())
options(scipen=999)

### preparation ###

#install.packages("data.table")
library(data.table)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("magrittr")
library(magrittr)
#install.packages("qwraps2")
library(qwraps2)
#install.packages("car")
library(car)
#install.packages("AER")
library(AER)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)

setwd("C:/Users/user/Desktop/לימודים/שנה ד סמסטר א/סמינר/data")

data = read.csv("data.csv")

# Loading all the files
mursha2011 = read.csv("מורשים לנהוג 2011.csv")
mursha2012 = read.csv("מורשים לנהוג 2012.csv")
mursha2013 = read.csv("מורשים לנהוג 2013.csv")
mursha2014 = read.csv("מורשים לנהוג 2014.csv")
mursha2016 = read.csv("מורשים לנהוג 2016.csv")
mursha2017 = read.csv("מורשים לנהוג 2017.csv")
mursha2018 = read.csv("מורשים לנהוג 2018.csv")

acc2011 = read.csv("Acc 2011.csv")
acc2012 = read.csv("Acc 2012.csv")
acc2013 = read.csv("Acc 2013.csv")
acc2014 = read.csv("Acc 2014.csv")
acc2015 = read.csv("Acc 2015.csv")
acc2016 = read.csv("Acc 2016.csv")
acc2017 = read.csv("Acc 2017.csv")
acc2018 = read.csv("Acc 2018.csv")

academy = read.csv("school_grat.csv")


# Those are the serial number for each city. You might find it usefull
# 1.	beer_y
# 2.	or_yehuda
# 3.	ramat_gan
# 4.	rishon
# 5.	ashdod
# 6.	or_akiva
# 7.	beer_sheva
# 8.	bet_shaan
# 9.	dimona
# 10.	herzelia
# 11.	tveria
# 12.	tira
# 13.	kfar_saba
# 14.	migdal_haemek
# 15.	nof_hagalil
# 16.	nesher
# 17.	natanya
# 18.	petach_tikva
# 19.	motzkin
# 20.	kiryat_gat
# 21.	rosh_haain
# 22.	sfaram
# 23. ramla
# 24. lod



#### SECTION A: Prepare the data ####

### A.1 Daily traffic data ###


# Using only the years we need
years_we_need = c(2011,2012, 2013,2014,2015,2016,2017,2018)
data = data %>% filter(year %in% years_we_need)

### A.2 Driving licensees data - "Mursha" ####

## A.2.1 Loading and preparing the "Mursha" data ##


# Cleaning & Saving only observations
mursha2011 = mursha2011[-(1:15),c(1,12)]
mursha2012 = mursha2012[-(1:16),c(1,11)]
mursha2013 = mursha2013[-(1:15),c(1,11)]
mursha2014 = mursha2014[-(1:16),c(1,11)]
mursha2016 = mursha2016[-(1:17),c(1,11)]
mursha2017 = mursha2017[-(1:17),c(1,11)]
mursha2018 = mursha2018[-(1:17),c(1,11)]


# Adding year variable to each file
mursha2011$year = 2011
mursha2012$year = 2012
mursha2013$year = 2013
mursha2014$year = 2014
mursha2016$year = 2016
mursha2017$year = 2017
mursha2018$year = 2018

# Make the file to a dataframe
mursha2011 = data.frame(mursha2011)
mursha2012 = data.frame(mursha2012)
mursha2013 = data.frame(mursha2013)
mursha2014 = data.frame(mursha2014)
mursha2016 = data.frame(mursha2016)
mursha2017 = data.frame(mursha2017)
mursha2018 = data.frame(mursha2018)



# Merging the data frames
the_mursha = rbindlist(list(mursha2011,mursha2012,
                            mursha2013,mursha2014,mursha2016,
                            mursha2017,mursha2018), use.names=FALSE)






## A.2.2 Assigning licensed drivers to data by city ##

# 1. beer_y
data$mursha[(data$city =="beer_y") &(data$year == "2011")] = "14,500"
data$mursha[(data$city =="beer_y") &(data$year == "2012")] = "16,300"
data$mursha[(data$city =="beer_y") &(data$year == "2013")] = "16,316"
data$mursha[(data$city =="beer_y") &(data$year == "2014")] = "17,521"
data$mursha[(data$city =="beer_y") &(data$year == "2016")] = "18,401"
data$mursha[(data$city =="beer_y") &(data$year == "2017")] = "20,378"
data$mursha[(data$city =="beer_y") &(data$year == "2018")] = "22,851"

# 2. or_yehuda
data$mursha[(data$city =="or_yehuda") &(data$year == "2011")] = "35,000"
data$mursha[(data$city =="or_yehuda") &(data$year == "2012")] = "35,300"
data$mursha[(data$city =="or_yehuda") &(data$year == "2013")] = "35,262"
data$mursha[(data$city =="or_yehuda") &(data$year == "2014")] = "35,526"
data$mursha[(data$city =="or_yehuda") &(data$year == "2016")] = "36,163"
data$mursha[(data$city =="or_yehuda") &(data$year == "2017")] = "36,536"
data$mursha[(data$city =="or_yehuda") &(data$year == "2018")] = "36,706"



# 3. ramat_gan
data$mursha[(data$city =="ramat_gan") &(data$year == "2011")] = "148,000"
data$mursha[(data$city =="ramat_gan") &(data$year == "2012")] = "148,400"
data$mursha[(data$city =="ramat_gan") &(data$year == "2013")] = "148,396"
data$mursha[(data$city =="ramat_gan") &(data$year == "2014")] = "149,594"
data$mursha[(data$city =="ramat_gan") &(data$year == "2016")] = "152,596"
data$mursha[(data$city =="ramat_gan") &(data$year == "2017")] = "153,674"
data$mursha[(data$city =="ramat_gan") &(data$year == "2018")] = "156,277"

# 4. rishon
data$mursha[(data$city =="rishon") &(data$year == "2011")] = "232,400"
data$mursha[(data$city =="rishon") &(data$year == "2012")] = "235,100"
data$mursha[(data$city =="rishon") &(data$year == "2013")] = "235,123"
data$mursha[(data$city =="rishon") &(data$year == "2014")] = "237,639"
data$mursha[(data$city =="rishon") &(data$year == "2016")] = "243,972"
data$mursha[(data$city =="rishon") &(data$year == "2017")] = "247,323"
data$mursha[(data$city =="rishon") &(data$year == "2018")] = "249,860"


# 5. ashdod
data$mursha[(data$city =="ashdod") &(data$year == "2011")] = "212,300"
data$mursha[(data$city =="ashdod") &(data$year == "2012")] = "214,900"
data$mursha[(data$city =="ashdod") &(data$year == "2013")] = "214,886"
data$mursha[(data$city =="ashdod") &(data$year == "2014")] = "216,511"
data$mursha[(data$city =="ashdod") &(data$year == "2016")] = "220,173"
data$mursha[(data$city =="ashdod") &(data$year == "2017")] = "221,591"
data$mursha[(data$city =="ashdod") &(data$year == "2018")] = "222,883"

# 6. or_akiva
data$mursha[(data$city =="or_akiva") &(data$year == "2011")] = "15,900"
data$mursha[(data$city =="or_akiva") &(data$year == "2012")] = "16,200"
data$mursha[(data$city =="or_akiva") &(data$year == "2013")] = "16,206"
data$mursha[(data$city =="or_akiva") &(data$year == "2014")] = "16,560"
data$mursha[(data$city =="or_yehuda") &(data$year == "2016")] = "17,083"
data$mursha[(data$city =="or_akiva") &(data$year == "2017")] = "17,568"
data$mursha[(data$city =="or_akiva") &(data$year == "2018")] = "17,759"

# 7. beer_sheva
data$mursha[(data$city =="beer_sheva") &(data$year == "2011")] = "196,300"
data$mursha[(data$city =="beer_sheva") &(data$year == "2012")] = "197,300"
data$mursha[(data$city =="beer_sheva") &(data$year == "2013")] = "197,269"
data$mursha[(data$city =="beer_sheva") &(data$year == "2014")] = "199,334"
data$mursha[(data$city =="beer_sheva") &(data$year == "2016")] = NA
data$mursha[(data$city =="beer_sheva") &(data$year == "2017")] = NA
data$mursha[(data$city =="beer_sheva") &(data$year == "2018")] = NA


# 8. bet_shaan
data$mursha[(data$city =="bet_shaan") &(data$year == "2011")] = "16,900"
data$mursha[(data$city =="bet_shaan") &(data$year == "2012")] = "17,200"
data$mursha[(data$city =="bet_shaan") &(data$year == "2013")] = "17,184"
data$mursha[(data$city =="bet_shaan") &(data$year == "2014")] = "17,263"
data$mursha[(data$city =="bet_shaan") &(data$year == "2016")] = "11,425"
data$mursha[(data$city =="bet_shaan") &(data$year == "2017")] = "11,568"
data$mursha[(data$city =="bet_shaan") &(data$year == "2018")] = "11,714"


# 9. dimona
data$mursha[(data$city =="dimona") &(data$year == "2011")] = "32,400"
data$mursha[(data$city =="dimona") &(data$year == "2012")] = "33,100"
data$mursha[(data$city =="dimona") &(data$year == "2013")] = "33,056"
data$mursha[(data$city =="dimona") &(data$year == "2014")] = "33,188"
data$mursha[(data$city =="dimona") &(data$year == "2016")] = "33,257"
data$mursha[(data$city =="dimona") &(data$year == "2017")] = "33,452"
data$mursha[(data$city =="dimona") &(data$year == "2018")] = "33,666"

# 10. herzelia
data$mursha[(data$city =="herzelia") &(data$year == "2011")] = "88,700"
data$mursha[(data$city =="herzelia") &(data$year == "2012")] = "89,200"
data$mursha[(data$city =="herzelia") &(data$year == "2013")] = "89,232"
data$mursha[(data$city =="herzelia") &(data$year == "2014")] = "89,813"
data$mursha[(data$city =="herzelia") &(data$year == "2016")] = "91,925"
data$mursha[(data$city =="herzelia") &(data$year == "2017")] = "93,116"
data$mursha[(data$city =="herzelia") &(data$year == "2018")] = "93,989"


# 11. tveria
data$mursha[(data$city =="tveria") &(data$year == "2011")] = "41,700"
data$mursha[(data$city =="tveria") &(data$year == "2012")] = "41,700"
data$mursha[(data$city =="tveria") &(data$year == "2013")] = "41,741"
data$mursha[(data$city =="tveria") &(data$year == "2014")] = "41,984"
data$mursha[(data$city =="tveria") &(data$year == "2016")] = "42,609"
data$mursha[(data$city =="tveria") &(data$year == "2017")] = "43,148"
data$mursha[(data$city =="tveria") &(data$year == "2018")] = "43,664"


# 12. tira
data$mursha[(data$city =="tira") &(data$year == "2011")] = "23,300"
data$mursha[(data$city =="tira") &(data$year == "2012")] = "23,900"
data$mursha[(data$city =="tira") &(data$year == "2013")] = "23,857"
data$mursha[(data$city =="tira") &(data$year == "2014")] = "24,046"
data$mursha[(data$city =="tira") &(data$year == "2016")] = "24,872"
data$mursha[(data$city =="tira") &(data$year == "2017")] = "25,268"
data$mursha[(data$city =="tira") &(data$year == "2018")] = "25,721"


# 13. kfar_saba
data$mursha[(data$city =="kfar_saba") &(data$year == "2011")] = "87,300"
data$mursha[(data$city =="kfar_saba") &(data$year == "2012")] = "89,200"
data$mursha[(data$city =="kfar_saba") &(data$year == "2013")] = "89,220"
data$mursha[(data$city =="kfar_saba") &(data$year == "2014")] = "91,664"
data$mursha[(data$city =="kfar_saba") &(data$year == "2016")] = "96,922"
data$mursha[(data$city =="kfar_saba") &(data$year == "2017")] = "98,981"
data$mursha[(data$city =="kfar_saba") &(data$year == "2018")] = "100,039"


# 14. migdal_haemek
data$mursha[(data$city =="migdal_haemek") &(data$year == "2011")] = "24,100"
data$mursha[(data$city =="migdal_haemek") &(data$year == "2012")] = "24,400"
data$mursha[(data$city =="migdal_haemek") &(data$year == "2013")] = "24,447"
data$mursha[(data$city =="migdal_haemek") &(data$year == "2014")] = "24,810"
data$mursha[(data$city =="migdal_haemek") &(data$year == "2016")] = "24,959"
data$mursha[(data$city =="migdal_haemek") &(data$year == "2017")] = "25,084"
data$mursha[(data$city =="migdal_haemek") &(data$year == "2018")] = "25,371"


# 15. nof_hagalil
data$mursha[(data$city =="nof_hagalil") &(data$year == "2011")] = "40,600"
data$mursha[(data$city =="nof_hagalil") &(data$year == "2012")] = "40,800"
data$mursha[(data$city =="nof_hagalil") &(data$year == "2013")] = "40,829"
data$mursha[(data$city =="nof_hagalil") &(data$year == "2014")] = "40,460"
data$mursha[(data$city =="nof_hagalil") &(data$year == "2016")] = "40,197"
data$mursha[(data$city =="nof_hagalil") &(data$year == "2017")] = "40,244"
data$mursha[(data$city =="nof_hagalil") &(data$year == "2018")] = "40,596"


# 16. nesher
data$mursha[(data$city =="nesher") &(data$year == "2011")] = "24,000"
data$mursha[(data$city =="nesher") &(data$year == "2012")] = "23,200"
data$mursha[(data$city =="nesher") &(data$year == "2013")] = "23,237"
data$mursha[(data$city =="nesher") &(data$year == "2014")] = "23,319"
data$mursha[(data$city =="nesher") &(data$year == "2016")] = "23,419"
data$mursha[(data$city =="nesher") &(data$year == "2017")] = "23,684"
data$mursha[(data$city =="nesher") &(data$year == "2018")] = "23,749"


# 17. natanya
data$mursha[(data$city =="natanya") &(data$year == "2011")] = "189,700"
data$mursha[(data$city =="natanya") &(data$year == "2012")] = "192,200"
data$mursha[(data$city =="natanya") &(data$year == "2013")] = "192,159"
data$mursha[(data$city =="natanya") &(data$year == "2014")] = "196,978"
data$mursha[(data$city =="natanya") &(data$year == "2016")] = "207,945"
data$mursha[(data$city =="natanya") &(data$year == "2017")] = "210,834"
data$mursha[(data$city =="natanya") &(data$year == "2018")] = "214,101"


# 18. petach_tikva
data$mursha[(data$city =="petach_tikva") &(data$year == "2011")] = "210,400"
data$mursha[(data$city =="petach_tikva") &(data$year == "2012")] = "213,900"
data$mursha[(data$city =="petach_tikva") &(data$year == "2013")] = "213,898"
data$mursha[(data$city =="petach_tikva") &(data$year == "2014")] = "219,303"
data$mursha[(data$city =="petach_tikva") &(data$year == "2016")] = "230,984"
data$mursha[(data$city =="petach_tikva") &(data$year == "2017")] = "236,169"
data$mursha[(data$city =="petach_tikva") &(data$year == "2018")] = "240,357"


# 19. motzkin
data$mursha[(data$city =="motzkin") &(data$year == "2011")] = "38,300"
data$mursha[(data$city =="motzkin") &(data$year == "2012")] = "38,900"
data$mursha[(data$city =="motzkin") &(data$year == "2013")] = "38,915"
data$mursha[(data$city =="motzkin") &(data$year == "2014")] = "39,228"
data$mursha[(data$city =="motzkin") &(data$year == "2016")] = "40,159"
data$mursha[(data$city =="motzkin") &(data$year == "2017")] = "40,750"
data$mursha[(data$city =="motzkin") &(data$year == "2018")] = "41,440"


# 20. kiryat_gat
data$mursha[(data$city =="kiryat_gat") &(data$year == "2011")] = "47,800"
data$mursha[(data$city =="kiryat_gat") &(data$year == "2012")] = "48,300"
data$mursha[(data$city =="kiryat_gat") &(data$year == "2013")] = "48,275"
data$mursha[(data$city =="kiryat_gat") &(data$year == "2014")] = "49,352"
data$mursha[(data$city =="kiryat_gat") &(data$year == "2016")] = "51,482"
data$mursha[(data$city =="kiryat_gat") &(data$year == "2017")] = "52,585"
data$mursha[(data$city =="kiryat_gat") &(data$year == "2018")] = "53,487"


# 21. rosh_haain
data$mursha[(data$city =="rosh_haain") &(data$year == "2011")] = "39,900"
data$mursha[(data$city =="rosh_haain") &(data$year == "2012")] = "40,600"
data$mursha[(data$city =="rosh_haain") &(data$year == "2013")] = "40,608"
data$mursha[(data$city =="rosh_haain") &(data$year == "2014")] = "41,451"
data$mursha[(data$city =="rosh_haain") &(data$year == "2016")] = "42,938"
data$mursha[(data$city =="rosh_haain") &(data$year == "2017")] = "45,487"
data$mursha[(data$city =="rosh_haain") &(data$year == "2018")] = "50,453"


# 22. sfaram
data$mursha[(data$city =="sfaram") &(data$year == "2011")] = "37,700"
data$mursha[(data$city =="sfaram") &(data$year == "2012")] = "38,300"
data$mursha[(data$city =="sfaram") &(data$year == "2013")] = "38,343"
data$mursha[(data$city =="sfaram") &(data$year == "2014")] = "38,717"
data$mursha[(data$city =="sfaram") &(data$year == "2016")] = "40,017"
data$mursha[(data$city =="sfaram") &(data$year == "2017")] = "40,535"
data$mursha[(data$city =="sfaram") &(data$year == "2018")] = "41,024"

# 23. ramla
data$mursha[(data$city =="ramla") &(data$year == "2011")] = "67,900"
data$mursha[(data$city =="ramla") &(data$year == "2012")] = "68,000"
data$mursha[(data$city =="ramla") &(data$year == "2013")] = "68,017"
data$mursha[(data$city =="ramla") &(data$year == "2014")] = "69,034"
data$mursha[(data$city =="ramla") &(data$year == "2016")] = "73,685"
data$mursha[(data$city =="ramla") &(data$year == "2017")] = "74,964"
data$mursha[(data$city =="ramla") &(data$year == "2018")] = "75,668"

# 24. lod
data$mursha[(data$city =="lod") &(data$year == "2011")] = "70,300"
data$mursha[(data$city =="lod") &(data$year == "2012")] = "71,100"
data$mursha[(data$city =="lod") &(data$year == "2013")] = "71,060"
data$mursha[(data$city =="lod") &(data$year == "2014")] = "71,642"
data$mursha[(data$city =="lod") &(data$year == "2016")] = "72,818"
data$mursha[(data$city =="lod") &(data$year == "2017")] = "73,608"
data$mursha[(data$city =="lod") &(data$year == "2018")] = "74,604"



## A.2.3 Adapting "Mursha" variable to the data ##

## Fill NA in "Mursha" data ##

# Erase Comma from Mursha variable, so we can calculate mean 
data$mursha <-gsub(",","",data$mursha)


# Calculating average by city
avg = by(as.numeric(data$mursha), data$city, mean, na.rm = T)
avg

# Average by city 

# 1.
beer_y_avg = "18064"

# 2. 
or_yehuda_avg = "35784.71"

# 3.
ramat_gan_avg = "150584.4"

# 4. 
rishon_avg = "240202.4"

# 5.
ashdod_avg = "217606.3"

# 6.
or_akiva_avg = "16771.45"

# 7.
beer_sheva_avg = "197550.8"

# 8.
bet_shaan_avg = "14750.57"

# 9.
dimona_avg = '33137.38'

# 10.
herzelia_avg = "90476.5"

# 11. 
tveria_Avg = "42392.92"

# 12.
tira_avg = "24388.92"

# 13.
kfar_saba_avg = "92734"

# 14.
migdal_haemek_avg = "24747.12"

# 15.
nof_hagalil_avg = "40554.92"

# 16.
nesher_avg = "23555"

# 17.
natanya_avg = "199991.5"

# 18.
petach_tikva_avg = "223573"

# 19.
motzkin_avg = "39375.33"

# 20.
kiryat_gat_avg = "50183"

# 21.
rosh_haain_avg = "43062.43"

# 22.
sfaram_avg = "39233.71"

# 23. 
ramla_avg = "71038.29"

# 24.
lod_avg = "72161.71"


# Filling the values with the city average
#  1. 
data$mursha[(is.na(data$mursha)) & (data$city== "beer_y")] = beer_y_avg 

# 2.
data$mursha[(is.na(data$mursha)) & (data$city== "or_yehuda")] = or_yehuda_avg 

# 3. 
data$mursha[(is.na(data$mursha)) & (data$city== "ramat_gan")] = ramat_gan_avg

# 4. 
data$mursha[(is.na(data$mursha)) & (data$city== "rishon")] = rishon_avg 

# 5.
data$mursha[(is.na(data$mursha)) & (data$city== "ashdod")] = ashdod_avg

# 6. 
data$mursha[(is.na(data$mursha)) & (data$city== "or_akiva")] = or_akiva_avg

# 7.
data$mursha[(is.na(data$mursha)) & (data$city== "beer_sheva")] = beer_sheva_avg

# 8.
data$mursha[(is.na(data$mursha)) & (data$city== "bet_shaan")] = bet_shaan_avg

# 9.
data$mursha[(is.na(data$mursha)) & (data$city== "dimona")] = dimona_avg

# 10. 
data$mursha[(is.na(data$mursha)) & (data$city== "herzelia")] = herzelia_avg

# 11.
data$mursha[(is.na(data$mursha)) & (data$city== "tveria")] = tveria_Avg

# 12.
data$mursha[(is.na(data$mursha)) & (data$city== "tira")] = tira_avg

# 13.
data$mursha[(is.na(data$mursha)) & (data$city== "kfar_saba")] = kfar_saba_avg

# 14.
data$mursha[(is.na(data$mursha)) & (data$city== "migdal_haemek")] = migdal_haemek_avg

# 15.
data$mursha[(is.na(data$mursha)) & (data$city== "nof_hagalil")] = nof_hagalil_avg

# 16.
data$mursha[(is.na(data$mursha)) & (data$city== "nesher")] = nesher_avg

# 17.
data$mursha[(is.na(data$mursha)) & (data$city== "natanya")] = natanya_avg

# 18.
data$mursha[(is.na(data$mursha)) & (data$city== "petach_tikva")] = petach_tikva_avg

# 19.
data$mursha[(is.na(data$mursha)) & (data$city== "motzkin")] = motzkin_avg

# 20. 
data$mursha[(is.na(data$mursha)) & (data$city== "kiryat_gat")] = kiryat_gat_avg

# 21. 
data$mursha[(is.na(data$mursha)) & (data$city== "rosh_haain")] = rosh_haain_avg

# 22.
data$mursha[(is.na(data$mursha)) & (data$city== "sfaram")] = sfaram_avg


# 23.
data$mursha[(is.na(data$mursha)) & (data$city== "ramla")] = ramla_avg

# 24.
data$mursha[(is.na(data$mursha)) & (data$city== "lod")] = lod_avg

data$mursha = data$mursha/1000

# Checking there are no mistakes
table(is.na(data$mursha))

## ALL FALSE? GREAT ##

### A.3 Accident data ###

## A.3.1 Cleaning accident data ##
# Columns we might want

to_keep = c("SEMEL_YISHUV","SHNAT_TEUNA","SHAA",
            "YOM_LAYLA","YOM_BASHAVUA","HUMRAT_TEUNA"	,"MEHIRUT_MUTERET",	"MEZEG_AVIR")

acc2011 = acc2011[to_keep]
acc2012 = acc2012[to_keep]
acc2013 = acc2013[to_keep]
acc2014 = acc2014[to_keep]
acc2015 = acc2015[to_keep]
acc2016 = acc2016[to_keep]
acc2017 = acc2017[to_keep]
acc2018 = acc2018[to_keep]


# Merge data using Rbind

the_acc = rbindlist(list(acc2011,acc2012,
                         acc2013,acc2014,acc2015,acc2016,
                         acc2017,acc2018), use.names=FALSE)

# cleaning Yishuvim (Cities)

yishuv_num = c("70","1020", "2400" ,"8300","2530","8600","9000",
               "9200" ,"2200","6400","6700","2720","6900","874",
               "1061","2500","7400","7900",
               "8200","2630","2640","8800","8500","7000")


# Saving only cities we need
the_acc = the_acc[which(the_acc$SEMEL_YISHUV %in% yishuv_num),]

# Changing city symbol to city name
# 1. beer_y
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="2530"] = "beer_y"
# 2. or_yehuda
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="2400"] = "or_yehuda"
# 3. ramat_gan
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="8600"] = "ramat_gan"
# 4. rishon
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="8300"] = "rishon"
# 5. ashdod
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="70"] = "ashdod"
# 6. or_akiva
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="1020"] = "or_akiva"
# 7. beer_sheva
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="9000"] = "beer_sheva"
# 8. bet_shaan
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="9200"] = "bet_shaan"
# 9. dimona
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="2200"] = "dimona"
# 10.herzelia
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="6400"] = "herzelia"
# 11. tveria
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="6700"] = "tveria"
# 12. tira
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="2720"] = "tira"
# 13. kfar_saba
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="6900"] = "kfar_saba"
# 14. migdal_haemek
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="874"] = "migdal_haemek"
# 15. nof_hagalil
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="1061"] = "nof_hagalil"
# 16. nesher
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="2500"] = "nesher"
# 17. natanya
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="7400"] = "natanya"
# 18. petach_tikva
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="7900"] = "petach_tikva"
# 19. motzkin
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="8200"] = "motzkin"
# 20. kiryat_gat
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="2630"] = "kiryat_gat"
# 21. rosh_haain
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="2640"] = "rosh_haain"
# 22. sfaram
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="8800"] = "sfaram"
# 23. ramla
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="8500"] = "ramla"
# 22. lod
the_acc$SEMEL_YISHUV[the_acc$SEMEL_YISHUV=="7000"] = "lod"


# Changing column name
colnames(the_acc)[colnames(the_acc)=="SEMEL_YISHUV"] = "city"
colnames(the_acc)[colnames(the_acc)=="SHNAT_TEUNA"] = "year"

## A.3.2 Merging accident data with the original data ##

# Change variable to factor for the merge
the_acc$city = factor(the_acc$city)
data$city = factor(data$city)

# MAke sure they have same level
data$city
the_acc$city

# MERGE!
data = inner_join(the_acc, data, by = c("city" = "city" , "year" = "year"))

##A.3.3  IMPORTANT! ##
## BEcause the original daily traffic data has multiple line for each year
## (e.g. 2011/2012/2013.. might come up more than once)
## We must the the average for every year. 
## That is what we are doing here.

daily_avg_table = data[,c("city", "year", "daily_avg")]


daily_avg_table = daily_avg_table %>% 
  group_by(city,year) %>% summarise(mean(daily_avg))


names(daily_avg_table) [1] = "city"
names(daily_avg_table) [2] = "year"
names(daily_avg_table) [3] = "daily_avg"


daily_avg_table$daily_avg = format(round(daily_avg_table$daily_avg,2))

data$daily_avg = NULL


data = inner_join(data,daily_avg_table, by = c("city" = "city", "year" = "year"))

## A.3.4 Adjusting varibales form accidents data ##

data$YOM_LAYLA = as.character(data$YOM_LAYLA)
data$YOM_LAYLA[data$YOM_LAYLA =="5"] = "Night"
data$YOM_LAYLA[data$YOM_LAYLA =="1"] = "Day"


data$YOM_BASHAVUA[data$YOM_BASHAVUA =="1"] = "Sunday"
data$YOM_BASHAVUA[data$YOM_BASHAVUA =="2"] = "Monday"
data$YOM_BASHAVUA[data$YOM_BASHAVUA =="3"] = "Tuesday"
data$YOM_BASHAVUA[data$YOM_BASHAVUA =="4"] = "Wednsday"
data$YOM_BASHAVUA[data$YOM_BASHAVUA =="5"] = "Thursday"
data$YOM_BASHAVUA[data$YOM_BASHAVUA =="6"] = "Firday"
data$YOM_BASHAVUA[data$YOM_BASHAVUA =="7"] = "Saturday"

data$HUMRAT_TEUNA[data$HUMRAT_TEUNA =="1"] = "Deadly injury"
data$HUMRAT_TEUNA[data$HUMRAT_TEUNA =="2"] = "Serious injury"
data$HUMRAT_TEUNA[data$HUMRAT_TEUNA =="3"] = "Easy injury"


data$MEHIRUT_MUTERET[data$MEHIRUT_MUTERET =="0"] = "Unknown"
data$MEHIRUT_MUTERET[data$MEHIRUT_MUTERET =="1"] = "50 KPH"
data$MEHIRUT_MUTERET[data$MEHIRUT_MUTERET =="2"] = "60 KPH"
data$MEHIRUT_MUTERET[data$MEHIRUT_MUTERET =="3"] = "70 KPH"
data$MEHIRUT_MUTERET[data$MEHIRUT_MUTERET =="4"] = "80 KPH"
data$MEHIRUT_MUTERET[data$MEHIRUT_MUTERET =="5"] = "90 KPH"
data$MEHIRUT_MUTERET[data$MEHIRUT_MUTERET =="6"] = "100 KPH"
data$MEHIRUT_MUTERET[data$MEHIRUT_MUTERET == "7"] = "Unknown"

data$MEZEG_AVIR[data$MEZEG_AVIR =="1"] = "Bright"
data$MEZEG_AVIR[data$MEZEG_AVIR =="2"] = "Rainy"
data$MEZEG_AVIR[data$MEZEG_AVIR =="3"] = "Very hot"
data$MEZEG_AVIR[data$MEZEG_AVIR =="4"] = "Misty"
data$MEZEG_AVIR[data$MEZEG_AVIR =="5"] = "Other"
data$MEZEG_AVIR[data$MEZEG_AVIR =="9"] = "Unknown"

data$direction = NULL


### A.4 Academy data ##$

##  A.4.1 creating "academy" variable in "data"


data$academic = NA


## A.4.2 Assigning academy data to original data
# 1.
data$academic[which(data$city == "beer_y" & data$year =="2018")] =
  academy$percent[which(academy$city == "beer_y" & academy$year =="2018")]


data$academic[which(data$city == "beer_y" & data$year =="2017")] =
  academy$percent[which(academy$city == "beer_y" & academy$year =="2017")]


data$academic[which(data$city == "beer_y" & data$year =="2016")] =
  academy$percent[which(academy$city == "beer_y" & academy$year =="2016")]


data$academic[which(data$city == "beer_y" & data$year =="2015")] =
  academy$percent[which(academy$city == "beer_y" & academy$year =="2015")]


data$academic[which(data$city == "beer_y" & data$year =="2014")] =
  academy$percent[which(academy$city == "beer_y" & academy$year =="2014")]


data$academic[which(data$city == "beer_y" & data$year =="2013")] =
  academy$percent[which(academy$city == "beer_y" & academy$year =="2013")]


data$academic[which(data$city == "beer_y" & data$year =="2012")] =
  academy$percent[which(academy$city == "beer_y" & academy$year =="2012")]


data$academic[which(data$city == "beer_y" & data$year =="2011")] =
  academy$percent[which(academy$city == "beer_y" & academy$year =="2011")]


# 2. 
data$academic[which(data$city == "or_yehuda" & data$year =="2018")] =
  academy$percent[which(academy$city == "or_yehuda" & academy$year =="2018")]


data$academic[which(data$city == "or_yehuda" & data$year =="2017")] =
  academy$percent[which(academy$city == "or_yehuda" & academy$year =="2017")]


data$academic[which(data$city == "or_yehuda" & data$year =="2016")] =
  academy$percent[which(academy$city == "or_yehuda" & academy$year =="2016")]


data$academic[which(data$city == "or_yehuda" & data$year =="2015")] =
  academy$percent[which(academy$city == "or_yehuda" & academy$year =="2015")]


data$academic[which(data$city == "or_yehuda" & data$year =="2014")] =
  academy$percent[which(academy$city == "or_yehuda" & academy$year =="2014")]


data$academic[which(data$city == "or_yehuda" & data$year =="2013")] =
  academy$percent[which(academy$city == "or_yehuda" & academy$year =="2013")]


data$academic[which(data$city == "or_yehuda" & data$year =="2012")] =
  academy$percent[which(academy$city == "or_yehuda" & academy$year =="2012")]


data$academic[which(data$city == "or_yehuda" & data$year =="2011")] =
  academy$percent[which(academy$city == "or_yehuda" & academy$year =="2011")]

# 3.
data$academic[which(data$city == "ramat_gan" & data$year == "2018")] =
  academy$percent[which(academy$city == "ramat_gan" & academy$year =="2018")]


data$academic[which(data$city == "ramat_gan" & data$year == "2017")] =
  academy$percent[which(academy$city == "ramat_gan" & academy$year =="2017")]


data$academic[which(data$city == "ramat_gan" & data$year == "2016")] =
  academy$percent[which(academy$city == "ramat_gan" & academy$year =="2016")]


data$academic[which(data$city == "ramat_gan" & data$year == "2015")] =
  academy$percent[which(academy$city == "ramat_gan" & academy$year =="2015")]


data$academic[which(data$city == "ramat_gan" & data$year == "2014")] =
  academy$percent[which(academy$city == "ramat_gan" & academy$year =="2014")]


data$academic[which(data$city == "ramat_gan" & data$year == "2013")] =
  academy$percent[which(academy$city == "ramat_gan" & academy$year =="2013")]


data$academic[which(data$city == "ramat_gan" & data$year == "2012")] =
  academy$percent[which(academy$city == "ramat_gan" & academy$year =="2012")]


data$academic[which(data$city == "ramat_gan" & data$year == "2011")] =
  academy$percent[which(academy$city == "ramat_gan" & academy$year =="2011")]

# 4.
data$academic[which(data$city == "rishon" & data$year =="2018")] = 
  academy$percent[which(academy$city == "rishon" & academy$year =="2018")]


data$academic[which(data$city == "rishon" & data$year =="2017")] = 
  academy$percent[which(academy$city == "rishon" & academy$year =="2017")]


data$academic[which(data$city == "rishon" & data$year =="2016")] = 
  academy$percent[which(academy$city == "rishon" & academy$year =="2016")]


data$academic[which(data$city == "rishon" & data$year =="2015")] = 
  academy$percent[which(academy$city == "rishon" & academy$year =="2015")]


data$academic[which(data$city == "rishon" & data$year =="2014")] = 
  academy$percent[which(academy$city == "rishon" & academy$year =="2014")]


data$academic[which(data$city == "rishon" & data$year =="2013")] = 
  academy$percent[which(academy$city == "rishon" & academy$year =="2013")]


data$academic[which(data$city == "rishon" & data$year =="2012")] = 
  academy$percent[which(academy$city == "rishon" & academy$year =="2012")]


data$academic[which(data$city == "rishon" & data$year =="2011")] = 
  academy$percent[which(academy$city == "rishon" & academy$year =="2011")]

# 5.
data$academic[which(data$city == "ashdod" & data$year == "2018")] = 
  academy$percent[which(academy$city == "ashdod" & academy$year =="2018")]


data$academic[which(data$city == "ashdod" & data$year == "2017")] = 
  academy$percent[which(academy$city == "ashdod" & academy$year =="2017")]


data$academic[which(data$city == "ashdod" & data$year == "2016")] = 
  academy$percent[which(academy$city == "ashdod" & academy$year =="2016")]


data$academic[which(data$city == "ashdod" & data$year == "2015")] = 
  academy$percent[which(academy$city == "ashdod" & academy$year =="2015")]


data$academic[which(data$city == "ashdod" & data$year == "2014")] = 
  academy$percent[which(academy$city == "ashdod" & academy$year =="2014")]


data$academic[which(data$city == "ashdod" & data$year == "2013")] = 
  academy$percent[which(academy$city == "ashdod" & academy$year =="2013")]


data$academic[which(data$city == "ashdod" & data$year == "2012")] = 
  academy$percent[which(academy$city == "ashdod" & academy$year =="2012")]


data$academic[which(data$city == "ashdod" & data$year == "2011")] = 
  academy$percent[which(academy$city == "ashdod" & academy$year =="2011")]

# 6.
data$academic[which(data$city == "or_akiva" & data$year =="2018")] =
  academy$percent[which(academy$city == "or_akiva" & academy$year =="2018")]


data$academic[which(data$city == "or_akiva" & data$year =="2017")] =
  academy$percent[which(academy$city == "or_akiva" & academy$year =="2017")]


data$academic[which(data$city == "or_akiva" & data$year =="2016")] =
  academy$percent[which(academy$city == "or_akiva" & academy$year =="2016")]


data$academic[which(data$city == "or_akiva" & data$year =="2015")] =
  academy$percent[which(academy$city == "or_akiva" & academy$year =="2015")]


data$academic[which(data$city == "or_akiva" & data$year =="2014")] =
  academy$percent[which(academy$city == "or_akiva" & academy$year =="2014")]


data$academic[which(data$city == "or_akiva" & data$year =="2013")] =
  academy$percent[which(academy$city == "or_akiva" & academy$year =="2013")]


data$academic[which(data$city == "or_akiva" & data$year =="2012")] =
  academy$percent[which(academy$city == "or_akiva" & academy$year =="2012")]


data$academic[which(data$city == "or_akiva" & data$year =="2011")] =
  academy$percent[which(academy$city == "or_akiva" & academy$year =="2011")]

# 7.
data$academic[which(data$city == "beer_sheva" & data$year =="2018")] =
  academy$percent[which(academy$city == "beer_sheva" & academy$year =="2018")]


data$academic[which(data$city == "beer_sheva" & data$year =="2017")] =
  academy$percent[which(academy$city == "beer_sheva" & academy$year =="2017")]


data$academic[which(data$city == "beer_sheva" & data$year =="2016")] =
  academy$percent[which(academy$city == "beer_sheva" & academy$year =="2016")]


data$academic[which(data$city == "beer_sheva" & data$year =="2015")] =
  academy$percent[which(academy$city == "beer_sheva" & academy$year =="2015")]


data$academic[which(data$city == "beer_sheva" & data$year =="2014")] =
  academy$percent[which(academy$city == "beer_sheva" & academy$year =="2014")]


data$academic[which(data$city == "beer_sheva" & data$year =="2013")] =
  academy$percent[which(academy$city == "beer_sheva" & academy$year =="2013")]


data$academic[which(data$city == "beer_sheva" & data$year =="2012")] =
  academy$percent[which(academy$city == "beer_sheva" & academy$year =="2012")]


data$academic[which(data$city == "beer_sheva" & data$year =="2011")] =
  academy$percent[which(academy$city == "beer_sheva" & academy$year =="2011")]

# 8.
data$academic[which(data$city == "bet_shaan" & data$year == "2018")] =
  academy$percent[which(academy$city == "bet_shaan" & academy$year=="2018")]


data$academic[which(data$city == "bet_shaan" & data$year == "2017")] =
  academy$percent[which(academy$city == "bet_shaan" & academy$year=="2017")]


data$academic[which(data$city == "bet_shaan" & data$year == "2016")] =
  academy$percent[which(academy$city == "bet_shaan" & academy$year=="2016")]


data$academic[which(data$city == "bet_shaan" & data$year == "2015")] =
  academy$percent[which(academy$city == "bet_shaan" & academy$year=="2015")]


data$academic[which(data$city == "bet_shaan" & data$year == "2014")] =
  academy$percent[which(academy$city == "bet_shaan" & academy$year=="2014")]


data$academic[which(data$city == "bet_shaan" & data$year == "2013")] =
  academy$percent[which(academy$city == "bet_shaan" & academy$year=="2013")]


data$academic[which(data$city == "bet_shaan" & data$year == "2012")] =
  academy$percent[which(academy$city == "bet_shaan" & academy$year=="2012")]


data$academic[which(data$city == "bet_shaan" & data$year == "2011")] =
  academy$percent[which(academy$city == "bet_shaan" & academy$year=="2011")]

# 9.
data$academic[which(data$city == "dimona" & data$year == "2018")] =
  academy$percent[which(academy$city =="dimona" & academy$year =="2018")]


data$academic[which(data$city == "dimona" & data$year == "2017")] =
  academy$percent[which(academy$city =="dimona" & academy$year =="2017")]


data$academic[which(data$city == "dimona" & data$year == "2016")] =
  academy$percent[which(academy$city =="dimona" & academy$year =="2016")]


data$academic[which(data$city == "dimona" & data$year == "2015")] =
  academy$percent[which(academy$city =="dimona" & academy$year =="2015")]


data$academic[which(data$city == "dimona" & data$year == "2014")] =
  academy$percent[which(academy$city =="dimona" & academy$year =="2014")]


data$academic[which(data$city == "dimona" & data$year == "2013")] =
  academy$percent[which(academy$city =="dimona" & academy$year =="2013")]


data$academic[which(data$city == "dimona" & data$year == "2012")] =
  academy$percent[which(academy$city =="dimona" & academy$year =="2012")]


data$academic[which(data$city == "dimona" & data$year == "2011")] =
  academy$percent[which(academy$city =="dimona" & academy$year =="2011")]


# 10.
data$academic[which(data$city == "herzelia" & data$year == "2018")] =
  academy$percent[which(academy$city == "herzelia" &academy$year =="2018")]


data$academic[which(data$city == "herzelia" & data$year == "2017")] =
  academy$percent[which(academy$city == "herzelia" &academy$year =="2017")]


data$academic[which(data$city == "herzelia" & data$year == "2016")] =
  academy$percent[which(academy$city == "herzelia" &academy$year =="2016")]


data$academic[which(data$city == "herzelia" & data$year == "2015")] =
  academy$percent[which(academy$city == "herzelia" &academy$year =="2015")]


data$academic[which(data$city == "herzelia" & data$year == "2014")] =
  academy$percent[which(academy$city == "herzelia" &academy$year =="2014")]


data$academic[which(data$city == "herzelia" & data$year == "2013")] =
  academy$percent[which(academy$city == "herzelia" &academy$year =="2013")]


data$academic[which(data$city == "herzelia" & data$year == "2012")] =
  academy$percent[which(academy$city == "herzelia" &academy$year =="2012")]


data$academic[which(data$city == "herzelia" & data$year == "2011")] =
  academy$percent[which(academy$city == "herzelia" &academy$year =="2011")]

# 11.
data$academic[which(data$city == "tveria" & data$year =="2018")] = 
  academy$percent[which(academy$city == "tveria" & academy$year =="2018")]


data$academic[which(data$city == "tveria" & data$year =="2017")] = 
  academy$percent[which(academy$city == "tveria" & academy$year =="2017")]

data$academic[which(data$city == "tveria" & data$year =="2016")] = 
  academy$percent[which(academy$city == "tveria" & academy$year =="2016")]


data$academic[which(data$city == "tveria" & data$year =="2015")] = 
  academy$percent[which(academy$city == "tveria" & academy$year =="2015")]


data$academic[which(data$city == "tveria" & data$year =="2014")] = 
  academy$percent[which(academy$city == "tveria" & academy$year =="2014")]


data$academic[which(data$city == "tveria" & data$year =="2013")] = 
  academy$percent[which(academy$city == "tveria" & academy$year =="2013")]


data$academic[which(data$city == "tveria" & data$year =="2012")] = 
  academy$percent[which(academy$city == "tveria" & academy$year =="2012")]


data$academic[which(data$city == "tveria" & data$year =="2011")] = 
  academy$percent[which(academy$city == "tveria" & academy$year =="2011")]

# 12.
data$academic[which(data$city == "tira" & data$year == "2018")] =
  academy$percent[which(academy$city == "tira" & academy$year =="2018")]


data$academic[which(data$city == "tira" & data$year == "2017")] =
  academy$percent[which(academy$city == "tira" & academy$year =="2017")]


data$academic[which(data$city == "tira" & data$year == "2016")] =
  academy$percent[which(academy$city == "tira" & academy$year =="2016")]


data$academic[which(data$city == "tira" & data$year == "2015")] =
  academy$percent[which(academy$city == "tira" & academy$year =="2015")]


data$academic[which(data$city == "tira" & data$year == "2014")] =
  academy$percent[which(academy$city == "tira" & academy$year =="2014")]


data$academic[which(data$city == "tira" & data$year == "2013")] =
  academy$percent[which(academy$city == "tira" & academy$year =="2013")]


data$academic[which(data$city == "tira" & data$year == "2012")] =
  academy$percent[which(academy$city == "tira" & academy$year =="2012")]


data$academic[which(data$city == "tira" & data$year == "2011")] =
  academy$percent[which(academy$city == "tira" & academy$year =="2011")]


# 13.
data$academic[which(data$city == "kfar_saba" & data$year =="2018")] =
  academy$percent[which(academy$city == "kfar_saba" & academy$year =="2018")]


data$academic[which(data$city == "kfar_saba" & data$year =="2017")] =
  academy$percent[which(academy$city == "kfar_saba" & academy$year =="2017")]


data$academic[which(data$city == "kfar_saba" & data$year =="2016")] =
  academy$percent[which(academy$city == "kfar_saba" & academy$year =="2016")]


data$academic[which(data$city == "kfar_saba" & data$year =="2015")] =
  academy$percent[which(academy$city == "kfar_saba" & academy$year =="2015")]



data$academic[which(data$city == "kfar_saba" & data$year =="2014")] =
  academy$percent[which(academy$city == "kfar_saba" & academy$year =="2014")]


data$academic[which(data$city == "kfar_saba" & data$year =="2013")] =
  academy$percent[which(academy$city == "kfar_saba" & academy$year =="2013")]



data$academic[which(data$city == "kfar_saba" & data$year =="2012")] =
  academy$percent[which(academy$city == "kfar_saba" & academy$year =="2012")]



data$academic[which(data$city == "kfar_saba" & data$year =="2011")] =
  academy$percent[which(academy$city == "kfar_saba" & academy$year =="2011")]

# 14.
data$academic[which(data$city == "migdal_haemek" & data$year =="2018")] =
  academy$percent[which(academy$city == "migdal_haemek" & academy$year =="2018")]


data$academic[which(data$city == "migdal_haemek" & data$year =="2017")] =
  academy$percent[which(academy$city == "migdal_haemek" & academy$year =="2017")]


data$academic[which(data$city == "migdal_haemek" & data$year =="2016")] =
  academy$percent[which(academy$city == "migdal_haemek" & academy$year =="2016")]

data$academic[which(data$city == "migdal_haemek" & data$year =="2015")] =
  academy$percent[which(academy$city == "migdal_haemek" & academy$year =="2015")]

data$academic[which(data$city == "migdal_haemek" & data$year =="2014")] =
  academy$percent[which(academy$city == "migdal_haemek" & academy$year =="2014")]


data$academic[which(data$city == "migdal_haemek" & data$year =="2013")] =
  academy$percent[which(academy$city == "migdal_haemek" & academy$year =="2013")]


data$academic[which(data$city == "migdal_haemek" & data$year =="2012")] =
  academy$percent[which(academy$city == "migdal_haemek" & academy$year =="2012")]


data$academic[which(data$city == "migdal_haemek" & data$year =="2011")] =
  academy$percent[which(academy$city == "migdal_haemek" & academy$year =="2011")]


# 15.
data$academic[which(data$city == "nof_hagalil" & data$year == "2018")] =
  academy$percent[which(academy$city == "nof_hagalil" & academy$year =="2018")]


data$academic[which(data$city == "nof_hagalil" & data$year == "2017")] =
  academy$percent[which(academy$city == "nof_hagalil" & academy$year =="2017")]


data$academic[which(data$city == "nof_hagalil" & data$year == "2016")] =
  academy$percent[which(academy$city == "nof_hagalil" & academy$year =="2016")]


data$academic[which(data$city == "nof_hagalil" & data$year == "2015")] =
  academy$percent[which(academy$city == "nof_hagalil" & academy$year =="2015")]



data$academic[which(data$city == "nof_hagalil" & data$year == "2014")] =
  academy$percent[which(academy$city == "nof_hagalil" & academy$year =="2014")]



data$academic[which(data$city == "nof_hagalil" & data$year == "2013")] =
  academy$percent[which(academy$city == "nof_hagalil" & academy$year =="2013")]



data$academic[which(data$city == "nof_hagalil" & data$year == "2012")] =
  academy$percent[which(academy$city == "nof_hagalil" & academy$year =="2012")]


data$academic[which(data$city == "nof_hagalil" & data$year == "2011")] =
  academy$percent[which(academy$city == "nof_hagalil" & academy$year =="2011")]


# 16.
data$academic[which(data$city == "nesher" & data$year =="2018")] =
  academy$percent[which(academy$city == "nesher" & academy$year =="2018")]


data$academic[which(data$city == "nesher" & data$year =="2017")] =
  academy$percent[which(academy$city == "nesher" & academy$year =="2017")]


data$academic[which(data$city == "nesher" & data$year =="2016")] =
  academy$percent[which(academy$city == "nesher" & academy$year =="2016")]


data$academic[which(data$city == "nesher" & data$year =="2015")] =
  academy$percent[which(academy$city == "nesher" & academy$year =="2015")]



data$academic[which(data$city == "nesher" & data$year =="2014")] =
  academy$percent[which(academy$city == "nesher" & academy$year =="2014")]


data$academic[which(data$city == "nesher" & data$year =="2013")] =
  academy$percent[which(academy$city == "nesher" & academy$year =="2013")]



data$academic[which(data$city == "nesher" & data$year =="2012")] =
  academy$percent[which(academy$city == "nesher" & academy$year =="2012")]


data$academic[which(data$city == "nesher" & data$year =="2011")] =
  academy$percent[which(academy$city == "nesher" & academy$year =="2011")]

# 17.
data$academic[which(data$city == "natanya" & data$year =="2018")] =
  academy$percent[which(academy$city == "natanya" & academy$year=="2018")]


data$academic[which(data$city == "natanya" & data$year =="2017")] =
  academy$percent[which(academy$city == "natanya" & academy$year=="2017")]


data$academic[which(data$city == "natanya" & data$year =="2016")] =
  academy$percent[which(academy$city == "natanya" & academy$year=="2016")]


data$academic[which(data$city == "natanya" & data$year =="2015")] =
  academy$percent[which(academy$city == "natanya" & academy$year=="2015")]


data$academic[which(data$city == "natanya" & data$year =="2014")] =
  academy$percent[which(academy$city == "natanya" & academy$year=="2014")]


data$academic[which(data$city == "natanya" & data$year =="2013")] =
  academy$percent[which(academy$city == "natanya" & academy$year=="2013")]



data$academic[which(data$city == "natanya" & data$year =="2012")] =
  academy$percent[which(academy$city == "natanya" & academy$year=="2012")]


data$academic[which(data$city == "natanya" & data$year =="2011")] =
  academy$percent[which(academy$city == "natanya" & academy$year=="2011")]

# 18.
data$academic[which(data$city == "petach_tikva" & data$year =="2018")] = 
  academy$percent[which(academy$city == "petach_tikva" & academy$year =="2018")]


data$academic[which(data$city == "petach_tikva" & data$year =="2017")] = 
  academy$percent[which(academy$city == "petach_tikva" & academy$year =="2017")]


data$academic[which(data$city == "petach_tikva" & data$year =="2016")] = 
  academy$percent[which(academy$city == "petach_tikva" & academy$year =="2016")]


data$academic[which(data$city == "petach_tikva" & data$year =="2015")] = 
  academy$percent[which(academy$city == "petach_tikva" & academy$year =="2015")]



data$academic[which(data$city == "petach_tikva" & data$year =="2014")] = 
  academy$percent[which(academy$city == "petach_tikva" & academy$year =="2014")]


data$academic[which(data$city == "petach_tikva" & data$year =="2013")] = 
  academy$percent[which(academy$city == "petach_tikva" & academy$year =="2013")]


data$academic[which(data$city == "petach_tikva" & data$year =="2012")] = 
  academy$percent[which(academy$city == "petach_tikva" & academy$year =="2012")]


data$academic[which(data$city == "petach_tikva" & data$year =="2011")] = 
  academy$percent[which(academy$city == "petach_tikva" & academy$year =="2011")]


# 19.
data$academic[which(data$city == "motzkin" & data$year =="2018")] =
  academy$percent[which(academy$city == "motzkin" & academy$year =="2018")]


data$academic[which(data$city == "motzkin" & data$year =="2017")] =
  academy$percent[which(academy$city == "motzkin" & academy$year =="2017")]


data$academic[which(data$city == "motzkin" & data$year =="2016")] =
  academy$percent[which(academy$city == "motzkin" & academy$year =="2016")]


data$academic[which(data$city == "motzkin" & data$year =="2015")] =
  academy$percent[which(academy$city == "motzkin" & academy$year =="2015")]


data$academic[which(data$city == "motzkin" & data$year =="2014")] =
  academy$percent[which(academy$city == "motzkin" & academy$year =="2014")]


data$academic[which(data$city == "motzkin" & data$year =="2013")] =
  academy$percent[which(academy$city == "motzkin" & academy$year =="2013")]


data$academic[which(data$city == "motzkin" & data$year =="2012")] =
  academy$percent[which(academy$city == "motzkin" & academy$year =="2012")]


data$academic[which(data$city == "motzkin" & data$year =="2011")] =
  academy$percent[which(academy$city == "motzkin" & academy$year =="2018")]

# 20.
data$academic[which(data$city == "kiryat_gat" & data$year =="2018")] =
  academy$percent[which(academy$city == "kiryat_gat"& academy$year =="2018")]


data$academic[which(data$city == "kiryat_gat" & data$year =="2017")] =
  academy$percent[which(academy$city == "kiryat_gat"& academy$year =="2017")]

data$academic[which(data$city == "kiryat_gat" & data$year =="2016")] =
  academy$percent[which(academy$city == "kiryat_gat"& academy$year =="2016")]


data$academic[which(data$city == "kiryat_gat" & data$year =="2015")] =
  academy$percent[which(academy$city == "kiryat_gat"& academy$year =="2015")]


data$academic[which(data$city == "kiryat_gat" & data$year =="2014")] =
  academy$percent[which(academy$city == "kiryat_gat"& academy$year =="2014")]



data$academic[which(data$city == "kiryat_gat" & data$year =="2013")] =
  academy$percent[which(academy$city == "kiryat_gat"& academy$year =="2013")]


data$academic[which(data$city == "kiryat_gat" & data$year =="2012")] =
  academy$percent[which(academy$city == "kiryat_gat"& academy$year =="2012")]


data$academic[which(data$city == "kiryat_gat" & data$year =="2011")] =
  academy$percent[which(academy$city == "kiryat_gat"& academy$year =="2011")]

# 21.
data$academic[which(data$city == "rosh_haain" & data$year =="2018")] =
  academy$percent[which(academy$city == "rosh_haain" & academy$year =="2018")]


data$academic[which(data$city == "rosh_haain" & data$year =="2017")] =
  academy$percent[which(academy$city == "rosh_haain" & academy$year =="2017")]


data$academic[which(data$city == "rosh_haain" & data$year =="2016")] =
  academy$percent[which(academy$city == "rosh_haain" & academy$year =="2016")]


data$academic[which(data$city == "rosh_haain" & data$year =="2015")] =
  academy$percent[which(academy$city == "rosh_haain" & academy$year =="2015")]


data$academic[which(data$city == "rosh_haain" & data$year =="2014")] =
  academy$percent[which(academy$city == "rosh_haain" & academy$year =="2014")]


data$academic[which(data$city == "rosh_haain" & data$year =="2013")] =
  academy$percent[which(academy$city == "rosh_haain" & academy$year =="2013")]


data$academic[which(data$city == "rosh_haain" & data$year =="2012")] =
  academy$percent[which(academy$city == "rosh_haain" & academy$year =="2012")]


data$academic[which(data$city == "rosh_haain" & data$year =="2011")] =
  academy$percent[which(academy$city == "rosh_haain" & academy$year =="2011")]

# 22.
data$academic[which(data$city == "sfaram" & data$year =="2018")] =
  academy$percent[which(academy$city == "sfaram" & academy$year =="2018")]


data$academic[which(data$city == "sfaram" & data$year =="2017")] =
  academy$percent[which(academy$city == "sfaram" & academy$year =="2017")]

data$academic[which(data$city == "sfaram" & data$year =="2016")] =
  academy$percent[which(academy$city == "sfaram" & academy$year =="2016")]


data$academic[which(data$city == "sfaram" & data$year =="2015")] =
  academy$percent[which(academy$city == "sfaram" & academy$year =="2015")]


data$academic[which(data$city == "sfaram" & data$year =="2014")] =
  academy$percent[which(academy$city == "sfaram" & academy$year =="2014")]


data$academic[which(data$city == "sfaram" & data$year =="2013")] =
  academy$percent[which(academy$city == "sfaram" & academy$year =="2013")]


data$academic[which(data$city == "sfaram" & data$year =="2012")] =
  academy$percent[which(academy$city == "sfaram" & academy$year =="2012")]


data$academic[which(data$city == "sfaram" & data$year =="2011")] =
  academy$percent[which(academy$city == "sfaram" & academy$year =="2011")]


# 23. ramla
data$academic[which(data$city == "ramla" & data$year =="2018")] =
  academy$percent[which(academy$city == "ramla" & academy$year =="2018")]


data$academic[which(data$city == "ramla" & data$year =="2017")] =
  academy$percent[which(academy$city == "ramla" & academy$year =="2017")]

data$academic[which(data$city == "ramla" & data$year =="2016")] =
  academy$percent[which(academy$city == "ramla" & academy$year =="2016")]


data$academic[which(data$city == "ramla" & data$year =="2015")] =
  academy$percent[which(academy$city == "ramla" & academy$year =="2015")]


data$academic[which(data$city == "ramla" & data$year =="2014")] =
  academy$percent[which(academy$city == "ramla" & academy$year =="2014")]


data$academic[which(data$city == "ramla" & data$year =="2013")] =
  academy$percent[which(academy$city == "ramla" & academy$year =="2013")]


data$academic[which(data$city == "ramla" & data$year =="2012")] =
  academy$percent[which(academy$city == "sfaram" & academy$year =="2012")]


data$academic[which(data$city == "ramla" & data$year =="2011")] =
  academy$percent[which(academy$city == "ramla" & academy$year =="2011")]


# 24. lod
data$academic[which(data$city == "lod" & data$year =="2018")] =
  academy$percent[which(academy$city == "lod" & academy$year =="2018")]


data$academic[which(data$city == "lod" & data$year =="2017")] =
  academy$percent[which(academy$city == "lod" & academy$year =="2017")]

data$academic[which(data$city == "lod" & data$year =="2016")] =
  academy$percent[which(academy$city == "lod" & academy$year =="2016")]


data$academic[which(data$city == "lod" & data$year =="2015")] =
  academy$percent[which(academy$city == "lod" & academy$year =="2015")]


data$academic[which(data$city == "lod" & data$year =="2014")] =
  academy$percent[which(academy$city == "lod" & academy$year =="2014")]


data$academic[which(data$city == "lod" & data$year =="2013")] =
  academy$percent[which(academy$city == "lod" & academy$year =="2013")]


data$academic[which(data$city == "lod" & data$year =="2012")] =
  academy$percent[which(academy$city == "lod" & academy$year =="2012")]


data$academic[which(data$city == "lod" & data$year =="2011")] =
  academy$percent[which(academy$city == "lod" & academy$year =="2011")]


table(is.na(data$academic))
## ALL FALSE? GREAT ##


### A.5 Treatment variable (Treatment = 1, Contorl = 0) ###

# Creating a military base variable for the graph
data$treatment = "0"
data$treatment[which(data$city == "rishon" | data$city == "beer_y" |
                       data$city== "ramla" | data$city == "lod")] = "1"


# Making sure there are no mistakes
table(is.na(data$treatment))

### A.6 Accident severity binary variable ###


data$HUMRAT_TEUNA_number = (data$HUMRAT_TEUNA)
data$HUMRAT_TEUNA_number[which(data$HUMRAT_TEUNA_number ==
                                 "Serious injury")] = "bad_injury"

data$HUMRAT_TEUNA_number[which(data$HUMRAT_TEUNA_number ==
                                 "Deadly injury")] = "bad_injury"

data$HUMRAT_TEUNA_number[which(data$HUMRAT_TEUNA_number
                               == "bad_injury")] = "1"


data$HUMRAT_TEUNA_number[which(data$HUMRAT_TEUNA_number == 
                                 "Easy injury")] = "0"

data$HUMRAT_TEUNA_number = as.numeric((
  as.factor(data$HUMRAT_TEUNA_number))) -1



# Bad accidents percent - Treatment 2011
summary(data$HUMRAT_TEUNA_number[data$treatment == "1" &  data$year == "2011"])
# 2.5 %

# Bad accidents percent - Control 2011
summary(data$HUMRAT_TEUNA_number[data$treatment == "0" &  data$year == "2011"])
# 7 %

# Bad accidents percent - Treatment 2018
summary(data$HUMRAT_TEUNA_number[data$treatment == "1" & data$year == "2018"])
# 23%


# Bad accidents percent - Control 2018
summary(data$HUMRAT_TEUNA_number[data$treatment == "0" & data$year == "2018"])
# 12.5


#### SECTION B: Presenting the data ####

# B.1 Bold function - very usefull
colorado <- function(src, boulder) {
  if (!is.factor(src)) src <- factor(src)                   
  src_levels <- levels(src)                                 
  brave <- boulder %in% src_levels                        
  if (all(brave)) {                                         
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels)) 
    b_vec <- rep("plain", length(src_levels))               
    b_vec[b_pos] <- "bold"                                  
    b_vec                                                   
  } else {
    stop("All elements of 'boulder' must be in src")
  }
}

## B.2 Data 2015: 2011- 2014 (before the treatment)

data_2015 = data[which(data$year %in% c("2011", "2012", "2013",
                                        "2014")),]


## B.3 Average traffic dataframe, 2011-2014

daily_avg_table_c_t = daily_avg_table

daily_avg_table_c_t$treatment = "Control"
daily_avg_table_c_t$treatment[which(daily_avg_table_c_t$city =="rishon")] = "Treatment"
daily_avg_table_c_t$treatment[which(daily_avg_table_c_t$city =="ramla")] = "Treatment"
daily_avg_table_c_t$treatment[which(daily_avg_table_c_t$city =="lod")] = "Treatment"
daily_avg_table_c_t$treatment[which(daily_avg_table_c_t$city =="beer_y")] = "Treatment"


daily_avg_table_c_t$daily_avg = as.numeric(daily_avg_table_c_t$daily_avg)

daily_avg_table_c_t = daily_avg_table_c_t %>% 
  group_by(treatment,year) %>% summarise(mean(daily_avg))

names(daily_avg_table_c_t)[3] = "mean_of_traffic"

daily_avg_table_c_t$mean_of_traffic = format(round(daily_avg_table_c_t$mean_of_traffic,2))

daily_avg_table_c_t$year = factor(daily_avg_table_c_t$year)

### optional plot: Daily traffic average- control VS. treatment, 2011-2018 ###

daily_avg_plot = ggplot(
  daily_avg_table_c_t, aes(x=year, y=mean_of_traffic, fill=treatment)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("Daily traffic average- control VS. treatment, 2011-2018")  + ylab("count") +
  theme(axis.text.x=element_text(face=colorado(daily_avg_table_c_t$year, "2015")))

daily_avg_plot



# Previous to plot 3, there were 2 plot from Excel file. They're presented in the PDF #

### plot 3: Traffic trend, Treatment VS. Contorl, 2011-2018 ###

daily_rate_plot = ggplot(daily_avg_table_c_t, aes(year,mean_of_traffic, group = treatment,
                                                  colour = treatment))+
  geom_line(size= 2) + geom_vline(xintercept = "2014", linetype="dotted", 
                                  color = "blue", size=1.5) +
  ggtitle("Plot 3: Traffic trend, Treatment VS. Contorl, 2011-2018")

daily_rate_plot


## B.2.1 plot 3 explanation table (mean, Sd, diff table)

# Treatment group only

daily_2014_treatment= daily_avg_table_c_t[daily_avg_table_c_t$year ==
                      c("2011","2012","2013","2014") & daily_avg_table_c_t$treatment ==
                      "Treatment",]


daily_2018_treatment= daily_avg_table_c_t[daily_avg_table_c_t$year ==
                         c("2015","2016","2017","2018") & daily_avg_table_c_t$treatment ==
                         "Treatment",]

daily_2014_2018_treatment = cbind(daily_2014_treatment ,daily_2018_treatment)

daily_2014_2018_treatment$mean_of_traffic = as.numeric(daily_2014_2018_treatment$mean_of_traffic)
daily_2014_2018_treatment$mean_of_traffic1 = as.numeric(daily_2014_2018_treatment$mean_of_traffic1)
daily_2014_2018_treatment$diff = 
  daily_2014_2018_treatment$mean_of_traffic1 - daily_2014_2018_treatment$mean_of_traffic

daily_2014_2018_treatment$treatment = NULL
daily_2014_2018_treatment$treatment1 = NULL
names(daily_2014_2018_treatment)[2] = "Traffic_before_treatment"
names(daily_2014_2018_treatment)[4] = "Traffic_after_treatment"

daily_2014_2018_treatment$Traffic_before_treatment  =
  as.numeric(daily_2014_2018_treatment$Traffic_before_treatment)
daily_2014_2018_treatment$Traffic_after_treatment =
  as.numeric(daily_2014_2018_treatment$Traffic_after_treatment)

mean(daily_2014_2018_treatment$Traffic_before_treatment)
# 22.485
sd(daily_2014_2018_treatment$Traffic_before_treatment)
# 1.090092

mean(daily_2014_2018_treatment$Traffic_after_treatment)
# 25.0725
sd(daily_2014_2018_treatment$Traffic_after_treatment)
# 1.255823

daily_2014_2018_treatment$diff = 
  daily_2014_2018_treatment$Traffic_after_treatment - daily_2014_2018_treatment$Traffic_before_treatment

mean(daily_2014_2018_treatment$diff)
# 2.5875
sd(daily_2014_2018_treatment$diff)
# 1.040749



# Same for contorl
daily_2014_control= daily_avg_table_c_t[daily_avg_table_c_t$year ==
                                            c("2011","2012","2013","2014") & daily_avg_table_c_t$treatment ==
                                            "Control",]


daily_2018_control= daily_avg_table_c_t[daily_avg_table_c_t$year ==
                                            c("2015","2016","2017","2018") & daily_avg_table_c_t$treatment ==
                                            "Control",]

daily_2014_2018_control = cbind(daily_2014_control ,daily_2018_control)

daily_2014_2018_control$mean_of_traffic = as.numeric(daily_2014_2018_control$mean_of_traffic)
daily_2014_2018_control$mean_of_traffic1 = as.numeric(daily_2014_2018_control$mean_of_traffic1)
daily_2014_2018_control$diff = 
  daily_2014_2018_control$mean_of_traffic1 - daily_2014_2018_control$mean_of_traffic

daily_2014_2018_control$treatment = NULL
daily_2014_2018_control$treatment1 = NULL
names(daily_2014_2018_control)[2] = "Traffic_before_treatment"
names(daily_2014_2018_control)[4] = "Traffic_after_treatment"

daily_2014_2018_control$Traffic_before_treatment =
  as.numeric(daily_2014_2018_control$Traffic_before_treatment)

mean(daily_2014_2018_control$Traffic_before_treatment)
# 33.55
sd(daily_2014_2018_control$Traffic_before_treatment)
#  0.7105397

daily_2014_2018_control$Traffic_after_treatment =
  as.numeric(daily_2014_2018_control$Traffic_after_treatment)

mean(daily_2014_2018_control$Traffic_after_treatment)
#  38.0725
sd(daily_2014_2018_control$Traffic_after_treatment)
# 2.817616


daily_2014_2018_control$diff = 
  daily_2014_2018_control$Traffic_after_treatment - daily_2014_2018_control$Traffic_before_treatment

mean(daily_2014_2018_control$diff)
# 4.5225
sd(daily_2014_2018_control$diff)
# 2.74727

## B.4 Accident types counting variable, treatment & control

acc_by_year = 
  data_2015 %>% count(treatment,as.character(year), HUMRAT_TEUNA_number)

names(acc_by_year)[2] = "year"

acc_by_year$HUMRAT_TEUNA_number[which(acc_by_year$HUMRAT_TEUNA_number=="1")] =
  "Serious injury"


acc_by_year$HUMRAT_TEUNA_number[which(acc_by_year$HUMRAT_TEUNA_number=="0")] =
  "Easy injury"


acc_by_year$treatment[which(acc_by_year$treatment=="0")] =
  "control"


acc_by_year$treatment[which(acc_by_year$treatment=="1")] =
  "treatment"

acc_by_year = data.frame(acc_by_year)

acc_by_year

## B.5 Mean of traffic per year, treatment & control, 2011-2018 

daily_rate_treatment = 
  daily_avg_table_c_t$mean_of_traffic[which(daily_avg_table_c_t$treatment == "Treatment")]

daily_rate_treatment

daily_rate_control = 
  daily_avg_table_c_t$mean_of_traffic[which(daily_avg_table_c_t$treatment == "Control")]
daily_rate_control

### plot 4: Injury Level - control VS. treatment, 2011-2014 ###

ggplot(acc_by_year, 
       aes(x = year, y = n, colour = treatment, fill = HUMRAT_TEUNA_number)) +
  geom_bar(stat = "identity", width = .5, position = "stack") +
  facet_grid(. ~ treatment) +
  ggtitle("Plot 4:Injury Level - control VS. treatment, 2011-2014")  + ylab("count") +
  labs(fill = "Injury Type")


## B.6 checking differences within treatment & control before 2015
# We hope the difference is non-significant

similar_group_reg = summary(lm(HUMRAT_TEUNA_number~ year +treatment+
                                 year*treatment , data_2015))
similar_group_reg

similar_group_reg = data.frame(similar_group_reg$coefficients)
write.csv(similar_group_reg, "similar_group_reg.csv")

## the interaction coefficient isn non-significant!

### plot 5: Injury & weather distribution, control VS. treatment, 2011-2014 ###

mezeg_all = data %>% group_by(treatment, MEZEG_AVIR) %>% count(MEZEG_AVIR)

plot_mezeg = ggplot(mezeg_all, aes(x=MEZEG_AVIR, y=n, fill=treatment)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("Plot 5.2 :Weather distribution- control VS. treatment, 2011-2014") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

humrat_all = data %>% group_by(treatment, HUMRAT_TEUNA) %>% count(HUMRAT_TEUNA)

plot_humrat = ggplot(humrat_all, aes(x=HUMRAT_TEUNA, y= n, fill=treatment)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("Plot 5.1:	Injury distribution - control VS. treatment")  + ylab("count")


humrat_mezeg_combine = grid.arrange(plot_humrat, plot_mezeg, nrow = 1)




### plot 6: Day & week distribution - control VS. treatment, 2011-2014 ###

yom_all = data %>% group_by(treatment, YOM_LAYLA) %>% count(YOM_LAYLA)


plot_yom = ggplot(yom_all, aes(x=YOM_LAYLA, y=n, fill=treatment)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("	Plot 6.1: Day|Night distribution- control VS. treatment")  + ylab("count")


week_all =  data %>% group_by(treatment, YOM_BASHAVUA) %>% count(YOM_BASHAVUA)



plot_week = ggplot(week_all, aes(x=YOM_BASHAVUA, y=n, fill=treatment)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("Plot 6.2:	Week days distribution- control VS. treatment")  + ylab("count") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


yom_week_combine = grid.arrange(plot_yom, plot_week, nrow = 1)




### plot 7: Speed distribution, control VS. treatment, 2011-2014 ###

speed_all = data %>% group_by(treatment, MEHIRUT_MUTERET) %>% count(MEHIRUT_MUTERET)


plot_speed = ggplot(speed_all, aes(x=MEHIRUT_MUTERET, y=n, fill=treatment)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  ggtitle("Plot 7:Speed restriction distribution- control VS. treatment")+
  ylab("count") + xlab(" ")

plot_speed


##  B.7 Easy VS. bad injury counting variable

data$ID <- seq.int(nrow(data))


# Let's start with treatment group (data_t)
data_t = data[(which(data$treatment == "1")),]

data_t <- setNames(
  aggregate(HUMRAT_TEUNA_number ~ factor(
    data_t$ID, levels = as.character(
      seq(min(data_t$ID), max(data_t$ID), 1))),
    data_t, sum),c("ms", "Count.correct"))


data_t = data_t %>%
  mutate(Window = paste(ms, as.numeric(ms)+4, sep = "-"), # add windows
         Rolling.correct = zoo::rollapplyr(Count.correct,
                                           nrow(data_t), sum, na.rm = T,
                                           partial = T ,fill = F, align = "right"))

data_t$percent_bad_injury = (data_t$Rolling.correct)/1:nrow(data_t)
data_t$percent_easy_injury = 1- data_t$percent_bad_injury

data_t$ratio = (data_t$percent_bad_injury)/(data_t$percent_easy_injury)

data_t$year = data$year[which(data$treatment == "1")]

# Continue with control group (data_c)

data_c = data[(which(data$treatment == "0")),]

data_c <- setNames(
  aggregate(HUMRAT_TEUNA_number ~ factor(
    data_c$ID, levels = as.character(
      seq(min(data_c$ID), max(data_c$ID), 1))),
    data_c, sum),c("ms", "Count.correct"))



data_c = data_c %>%
  mutate(Window = paste(ms, as.numeric(ms)+4, sep = "-"), # add windows
         Rolling.correct = zoo::rollapplyr(Count.correct,
                                           nrow(data_c), sum, na.rm = T,
                                           partial = T ,fill = F, align = "right"))

data_c$percent_bad_injury = (data_c$Rolling.correct)/1:nrow(data_c)
data_c$percent_easy_injury = 1- data_c$percent_bad_injury
data_c$ratio = (data_c$percent_bad_injury)/(data_c$percent_easy_injury)

data_c$year = data$year[which(data$treatment == "0")]

data_t$treatment = "1"
data_c$treatment = "0"


data_c_t = rbind(data_c,data_t)

data_c_t$ratio = round(data_c_t$ratio,2)

data_c_t = data_c_t %>%
  group_by(year,treatment) %>% summarise(mean(ratio))

names(data_c_t)[3] = "ratio"

data_c_t$treatment[which(data_c_t$treatment == "1")] = "Treatment"
data_c_t$treatment[which(data_c_t$treatment == "0")] = "Control"


## B.8 Changes in accidents trends within group

data_treatment = data[(which(data$treatment == "1")),]

bad_treatment_2018 = data_treatment[which(data_treatment$HUMRAT_TEUNA_number ==
                                            "1" & data_treatment$year == "2018"),]

easy_treatment_2018 = data_treatment[which(data_treatment$HUMRAT_TEUNA_number ==
                                             "0" & data_treatment$year == "2018"),]



bad_treatment_2011 = data_treatment[which(data_treatment$HUMRAT_TEUNA_number ==
                                            "1" & data_treatment$year == "2011"),]

easy_treatment_2011 = data_treatment[which(data_treatment$HUMRAT_TEUNA_number ==
                                             "0" & data_treatment$year == "2011"),]

## Sum accident decrease in treatment group between 2011-2018 by approx. 1/2
## Bad\easy accidents ratio increase by more than 10


bad_control_2018 = data_treatment[which(data$HUMRAT_TEUNA_number ==
                                          "1" & data$year == "2018" & data$treatment =="0"),]

easy_control_2018 = data_treatment[which(data$HUMRAT_TEUNA_number ==
                                           "0" & data$year == "2018" & data$treatment =="0"),]


bad_control_2011 = data_treatment[which(data$HUMRAT_TEUNA_number ==
                                          "1" & data$year == "2011" & data$treatment =="0"),]

easy_control_2011 = data_treatment[which(data$HUMRAT_TEUNA_number ==
                                           "0" & data$year == "2011" & data$treatment =="0"),]


## Sum accident decrease in control group between 2011-2018 by approx. 1/2
## Bad\easy accidents ratio increase by about 3

## B.8 - Summary: Israel had less accdient between 2011-2018, but more bad injuries.
## There are much more  increasment in bad accidents ratio in treatment group.

### plot 8: Easy VS. bad injury odds, Treatment VS. Contorl, 2011-2018 ###

acc_ratio_plot = 
  ggplot(data_c_t, aes(x=factor(year), y=ratio, colour=treatment,
                       group=treatment)) + geom_line(size = 1) + geom_point() +
  geom_vline(xintercept = "2014", linetype="dotted", 
             color = "blue", size=1.5) +
  ggtitle("Plot 8:Easy VS. bad injury odds, Treatment VS. Contorl, 2011-2018") +xlab("Year")

acc_ratio_plot

## B.9 Accident severity summary

summary(data$HUMRAT_TEUNA_number)

## B.10 Making factor more simple

# first, for "data" dataset

data$MEHIRUT_MUTERET[which(data$MEHIRUT_MUTERET ==
                             "100 KPH" | data$MEHIRUT_MUTERET == "90 KPH"|
                             data$MEHIRUT_MUTERET == "80 KPH"|
                             data$MEHIRUT_MUTERET == "70 KPH"|
                             data$MEHIRUT_MUTERET == "60 KPH")] = "Over 50 KPH"

data$MEHIRUT_MUTERET = factor(data$MEHIRUT_MUTERET, levels = c("Over 50 KPH", "Unknown", "50 KPH"))

levels(data$MEHIRUT_MUTERET)


data$MEZEG_AVIR[which(data$MEZEG_AVIR == "Misty"|
                        data$MEZEG_AVIR == "Other"|
                        data$MEZEG_AVIR == "Rainy"|
                        data$MEZEG_AVIR == "Unknown"|
                        data$MEZEG_AVIR == "Very hot")] = "Not Bright"

data$MEZEG_AVIR = factor(data$MEZEG_AVIR, level = c("Not Bright", "Bright"))


levels(data$MEZEG_AVIR)


# Then, for "data_treatment" dataset


data_treatment$MEHIRUT_MUTERET[which(data_treatment$MEHIRUT_MUTERET ==
                             "100 KPH" | data_treatment$MEHIRUT_MUTERET == "90 KPH"|
                             data_treatment$MEHIRUT_MUTERET == "80 KPH"|
                             data_treatment$MEHIRUT_MUTERET == "70 KPH"|
                             data_treatment$MEHIRUT_MUTERET == "60 KPH")] = "Over 50 KPH"

data_treatment$MEHIRUT_MUTERET = factor(data_treatment$MEHIRUT_MUTERET)


data_treatment$MEHIRUT_MUTERET <- relevel(data_treatment$MEHIRUT_MUTERET, ref = "Over 50 KPH")



data_treatment$MEZEG_AVIR[which(data_treatment$MEZEG_AVIR == "Misty"|
                        data_treatment$MEZEG_AVIR == "Other"|
                        data_treatment$MEZEG_AVIR == "Rainy"|
                        data_treatment$MEZEG_AVIR == "Unknown"|
                        data_treatment$MEZEG_AVIR == "Very hot")] = "Not Bright"

data_treatment$MEZEG_AVIR = factor(data_treatment$MEZEG_AVIR)

data_treatment$MEZEG_AVIR <- relevel(data_treatment$MEZEG_AVIR, ref = "Bright")

data_treatment$MEZEG_AVIR

#### SECTION C: Analyze the data - Treatment group only ####


## C.1 post variable for Diff-in-Diff




data$post = NA
data$post[which(data$year %in% c(2011,2012,2013,2014))] = "0"
data$post[which(data$year %in% c(2015,2016,2017,2018))] = "1"

data$daily_avg = as.numeric(data$daily_avg)
data$mursha = as.numeric(data$mursha)
data$month = factor(data$month)
### C.2 create a treatment dataframe
data_treatment = data[(which(data$treatment == "1")),]

### C.3 check correlation between variables
my_num_data <- data[, sapply(data, is.numeric)]

table(is.na(my_num_data))



cor_table = data.frame(cor(my_num_data, use = "complete.obs", method = "pearson"))



cor_table$HUMRAT_TEUNA_number = NULL
cor_table$ID = NULL

cor_table
## Mursha & academy are HIGHLY CORRELATED with daily_avg ##
## City is causing multicollinearity !
################################
# IMPORTANT!
# Before running the regresssion, make sure:

# "year", "mursha","daily_avg","academic", "HUMRAT_TEUNA_number","ID"  -> numeric

data_treatment$SHAA = as.factor(data_treatment$SHAA)
data_treatment$MEHIRUT_MUTERET  = as.factor(data_treatment$MEHIRUT_MUTERET)
data_treatment$YOM_LAYLA = as.character(data_treatment$YOM_LAYLA)
data_treatment$YOM_BASHAVUA = factor(data_treatment$YOM_BASHAVUA) 
data_treatment$MEZEG_AVIR = as.factor(data_treatment$MEZEG_AVIR)
data_treatment$year = factor(data_treatment$year)

is.numeric(data_treatment$mursha)
is.numeric(data_treatment$daily_avg)
is.numeric(data_treatment$academic)
is.numeric(data_treatment$HUMRAT_TEUNA_number)
is.numeric(data_treatment$ID)
# "year","SHAA","month","road","MEHIRUT_MUTERET","MEZEG_AVIR",-> factor

is.factor(data_treatment$year)
is.factor(data_treatment$SHAA)
is.factor(data_treatment$month)
is.factor(data_treatment$road)
is.factor(data$MEHIRUT_MUTERET)
is.factor(data$MEZEG_AVIR)
is.factor(data_treatment$YOM_BASHAVUA)

# "YOM_LAYLA","YOM_BASHAVUA,"treatment","post"  -> character

is.character(data_treatment$YOM_LAYLA)
is.character(data_treatment$treatment)
is.character(data_treatment$post)


################################

################################

model1_t = lm(HUMRAT_TEUNA_number ~ daily_avg + 
                post
              , data = data_treatment)

summary(model1_t)$r.squared 
# R Squared: 0.03265073

nobs(model1_t)


model1_t_test = coeftest(model1_t, vcov. = vcovHC, type = "HC1")
model1_t_test

# Daily_avg coefficient: -0.0006
# Daily_avg P-value: 0.003222
# Number of observations: 23166
################################

model2_t = lm(HUMRAT_TEUNA_number ~ daily_avg + 
                post + year + 
                academic + 
                MEHIRUT_MUTERET
              , data = data_treatment)


summary(model2_t)$r.squared 
# R Squared: 0.05790057

nobs(model2_t)


model2_t_test = coeftest(model2_t, vcov. = vcovHC, type = "HC1")
model2_t_test

# Daily_avg coefficient: -0.00157739
# Daily_avg P-value: 0.00000000000006976
# Number of observations: 23166
################################

model3_t = lm(HUMRAT_TEUNA_number ~ daily_avg +  
                post + year + 
                MEHIRUT_MUTERET + YOM_LAYLA
              +YOM_BASHAVUA  + MEZEG_AVIR
              +SHAA + academic + mursha, data = data_treatment)


summary(model3_t)$r.squared 
# R Squared: 0.09530641

nobs(model3_t)

summary(model3_t)

model3_t_test = coeftest(model3_t, vcov. = vcovHC, type = "HC1")
model3_t_test


# Daily_avg coefficient -0.00097831
# Daily_avg P-value: 0.000005567346745
# Number of observations: 23166
################################

#### SECTION D: Analyze the data - Treatment + Control ####


# IMPORTANT!
# Before running the regresssion, make sure:

data$SHAA = factor(data$SHAA)
data$MEHIRUT_MUTERET = factor(data$MEHIRUT_MUTERET)
data$YOM_LAYLA = as.character(data$YOM_LAYLA)
data$YOM_BASHAVUA = factor(data$YOM_BASHAVUA)
data$MEZEG_AVIR  = factor(data$MEZEG_AVIR)
data$year = factor(data$year)

# mursha","daily_avg","academic", "HUMRAT_TEUNA_number","ID"  -> numeric

is.numeric(data$mursha)
is.numeric(data$daily_avg)
is.numeric(data$academic)
is.numeric(data$HUMRAT_TEUNA_number)
is.numeric(data$ID)

# "year, "SHAA","month","road","YOM_BASHAVUA,"MEHIRUT_MUTERET","MEZEG_AVIR", -> factor
is.factor(data$year)
is.factor(data$SHAA)
is.factor(data$month)
is.factor(data$road)
is.factor(data$YOM_BASHAVUA)
is.factor(data$MEHIRUT_MUTERET)
is.factor(data$MEZEG_AVIR)

# "YOM_LAYLA","treatment","post"  -> character

is.character(data$YOM_LAYLA)
is.character(data$treatment)
is.character(data$post)

################################


model1 = lm(HUMRAT_TEUNA_number ~ daily_avg + 
              post
            , data = data)

summary(model1)$r.squared 
# R Squared: 0.01350931


nobs(model1)


model1_test = coeftest(model1, vcov. = vcovHC, type = "HC1")
model1_test

# Daily_avg coefficient: -0.00056504
# Daily_avg P-value:  0.0000001133
# Number of observations: 51393
################################

model2 = lm(HUMRAT_TEUNA_number ~ daily_avg + 
              post + treatment + post*treatment
            , data = data)

summary(model2)$r.squared 
# R Squared: 0.01766479

nobs(model2)


model2_test = coeftest(model2, vcov. = vcovHC, type = "HC1")
model2_test


# Daily_avg coefficient: -0.0005932
# Daily_avg P-value: 0.00000002559
# Number of observations: 51393
################################

model3 = lm(HUMRAT_TEUNA_number ~ daily_avg + 
              post + treatment + post*treatment+
              year + academic + MEHIRUT_MUTERET
            , data = data)

summary(model3)$r.squared 
# R Squared: 0.02449293

nobs(model3)


model3_test = coeftest(model3, vcov. = vcovHC, type = "HC1")
model3_test

# Daily_avg coefficient: -0.00089059
# Daily_avg P-value: 0.0000000000000002785
# Number of observations: 51393
################################

model4 = lm(HUMRAT_TEUNA_number ~ daily_avg + 
              post + treatment + post*treatment+
              year + academic + MEHIRUT_MUTERET +
              YOM_LAYLA + YOM_BASHAVUA +
              mursha + MEZEG_AVIR + SHAA
            , data = data)

summary(model4)$r.squared 
# R Squared: 0.03665822

nobs(model4)


model4_test = coeftest(model4, vcov. = vcovHC, type = "HC1")
model4_test

# Daily_avg coefficient: 
# Daily_avg P-value: 
# Number of observations: 51393
################################

