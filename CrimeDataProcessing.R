library(tidyverse)
library(sf)
library(lubridate)
library(tmap)

crimes <- read_csv("Crimes_-_2001_to_present.csv")
crimescopy <- crimes

crimes$Date <- substring(crimes$Date, first = 1, last = 10)

crimes$Date <- mdy(crimes$Date)

crimes <- crimes %>%
  filter(lubridate::month(Date) == 4) %>%
  filter(lubridate::year(Date) > 2014)

unique(crimes$`Community Area`)
write.csv(crimes, "April2015to2020Crimes.csv")



crimes <- read_csv("April2015to2020Crimes.csv")
unique(crimes$`Primary Type`)

class.violent <- c("HOMICIDE", "KIDNAPPING", "ARSON", "ROBBERY", "ASSAULT",
                   "SEX OFFENSE", "CRIMINAL SEXUAL ASSAULT", "CRIM SEXUAL ASSAULT",
                   "BATTERY")

class.property <- c("THEFT", "BURGLARY", "MOTOR VEHICLE THEFT", "CRIMINAL TRESPASS",
                    "ROBBERY")

class.drug <- c("NARCOTICS", "OTHER NARCOTIC VIOLATION")

class.sex <- c("CRIM SEXUAL ASSAULT", "CRIMINAL SEXUAL ASSAULT", "SEX OFFENSE", 
               "PUBLIC INDECENCY", "HUMAN TRAFFICKING")

violent <- crimes %>%
  filter(`Primary Type` %in% class.violent)

property <- crimes %>%
  filter(`Primary Type` %in% class.property)

drug <- crimes %>%
  filter(`Primary Type` %in% class.drug)


zips <- st_read("Boundaries - ZIP Codes")
beats <- st_read("Boundaries - Police Beats (current)")

unique.beats <- as.character(beats$beat_num)

violent.beat <- matrix(nrow = length(unique.beats), ncol = 7)
violent.beat[,1] <- unique.beats
violent.beat[,2:7] <- 0
violent.beat <- data.frame(violent.beat)
names(violent.beat) <- c("Beat_Num",(paste("V_C", 2015:2020, sep = "_")))


violent.beat[,2:7] <- 0
for (i in 2015:2020) {
  this.violent <- violent %>%
    filter(year(Date) == i)
  
  this.violent <- this.violent %>%
    group_by(Beat) %>%
    summarize(total = n())
  
  this.matched <- base::match(this.violent$Beat, violent.beat$Beat_Num)
  this.total <- this.violent$total
  
  violent.beat[this.matched, (i - 2013)] <- this.total
}

beats.copy <- beats

#2015-2019 avg and 2020 difference from average
violent.beat$V_C_Avg <- rowMeans(violent.beat[,2:6], na.rm = T)
violent.beat$V_C_Diff <- (violent.beat$V_C_2020 - violent.beat$V_C_Avg) / violent.beat$V_C_Avg

beats.copy <- st_bind_cols(beats.copy, violent.beat)


property.beat <- matrix(nrow = length(unique.beats), ncol = 7)
property.beat[,1] <- unique.beats
property.beat[,2:7] <- 0
property.beat <- data.frame(property.beat)
names(property.beat) <- c("Beat_Num",(paste("P_C", 2015:2020, sep = "_")))


property.beat[,2:7] <- 0
for (i in 2015:2020) {
  this.property <- property %>%
    filter(year(Date) == i)
  
  this.property <- this.property %>%
    group_by(Beat) %>%
    summarize(total = n())
  
  this.matched <- base::match(this.property$Beat, property.beat$Beat_Num)
  this.total <- this.property$total
  
  property.beat[this.matched, (i - 2013)] <- this.total
}

View(property.beat)
#2015-2019 avg and 2020 difference from average
property.beat$P_C_Avg <- rowMeans(property.beat[,2:6], na.rm = T)
property.beat$P_C_Diff <- (property.beat$P_C_2020 - property.beat$P_C_Avg) / property.beat$P_C_Avg

beats.copy <- st_bind_cols(beats.copy, property.beat)


all.beat <- matrix(nrow = length(unique.beats), ncol = 7)
all.beat[,1] <- unique.beats
all.beat[,2:7] <- 0
all.beat <- data.frame(all.beat)
names(all.beat) <- c("Beat_Num",(paste("A_C", 2015:2020, sep = "_")))


all.beat[,2:7] <- 0
for (i in 2015:2020) {
  this.all <- crimes %>%
    filter(year(Date) == i)
  
  this.all <- this.all %>%
    group_by(Beat) %>%
    summarize(total = n())
  
  this.matched <- base::match(this.all$Beat, all.beat$Beat_Num)
  this.total <- this.all$total
  
  all.beat[this.matched, (i - 2013)] <- this.total
}

View(all.beat)
#2015-2019 avg and 2020 difference from average
all.beat$A_C_Avg <- rowMeans(all.beat[,2:6], na.rm = T)
all.beat$A_C_Diff <- (all.beat$A_C_2020 - all.beat$A_C_Avg) / all.beat$A_C_Avg

beats.copy <- st_bind_cols(beats.copy, all.beat)


tm_shape(beats.copy) +
  tm_polygons(col = "A_C_Avg", style = "jenks")


all.crime <- beats.copy[,25:30]



st_write(beats.copy, "beats.copy.shp")


a <- st_read("zipcodecrime.shp")

tm_shape(a) +
  tm_polygons(col = "A_C_202", style = "jenks")



