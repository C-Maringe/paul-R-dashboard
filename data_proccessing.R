#### group 2 assignment R Programming

### 1.first setting Up the working directory for the project

setwd('C:/Users/Dell/Desktop/R projects/paul/dashboard')

## Install packages to be later used in this project

# install.packages("rgdal")
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("tidyr")
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("leaflet")

### 2.DATA PROCESSING

## Importing Libraries

library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(tidyverse)
library(sf)
library(leaflet)
library(readxl)
library(rgdal)

### Importing data set

df <- readxl::read_excel("mydata.xlsx")

# EXPLOTARY DATA ANALYSIS

## extracting columns of interest in df

df1 <- df[,c("zw.lat","zw.lng",  "Province", "Status", "Fellow Status", "Class", "LevelType")]

## data visualizations
ggplot(df1, aes(x = Province)) +
  geom_bar()

ggplot(df1, aes(x = Status)) +
  geom_bar()

ggplot(df1, aes(x = `Fellow Status`)) +
  geom_bar()

ggplot(df1, aes(x = Class)) +
  geom_bar()

ggplot(df1, aes(x = LevelType)) +
  geom_bar()

## DAtA MANIPULATIONS

active = df1[df1$Status == 'Active', ]

Inactive <- df1[df1$Status == 'Inactive',]

## DATA MANIPULATIONS USING ACTIVE DATA CREATED ABOVE 

ggplot(active, aes(x = `Fellow Status`)) +
  geom_bar()

# droping rows with NA values

active <- active %>% drop_na()
Inactive <- Inactive %>% drop_na()

## counting the number of rows 

nrow(active)

## Concatinating Columns

active$ProvinceFellowStatus <- paste(active$Province, active$`Fellow Status`)
active$ProvinceClass <- paste(active$Province, active$`Class`)
active$ProvinceLevelType <- paste(active$Province, active$`LevelType`)

## Counting unique rows given Province

active$CountProvince <- ave(rep(1L, nrow(active)), active$Province, FUN = length)
active$CountProvinceFellowStatus <- ave(rep(1L, nrow(active)), active$ProvinceFellowStatus, FUN = length)
active$CountProvinceClass <- ave(rep(1L, nrow(active)), active$ProvinceClass, FUN = length)
active$CountProvinceLevelType <- ave(rep(1L, nrow(active)), active$ProvinceLevelType, FUN = length)

### Creating New Data frames 

dfProvince <- active[,c("zw.lat","zw.lng",  "Province", "Status", "CountProvince")]
dfProvinceFellowStatus <- active[,c("zw.lat","zw.lng",  "Province", "Status", "Fellow Status", "ProvinceFellowStatus", "CountProvinceFellowStatus")]
dfProvinceClass <- active[,c("zw.lat","zw.lng",  "Province", "Status", "Class", "ProvinceClass", "CountProvinceClass")]
dfProvinceLevelType <- active[,c("zw.lat","zw.lng",  "Province", "Status", "LevelType", "ProvinceLevelType", "CountProvinceLevelType")]

## Removing Duplicates from new data frames

dfProvince <- dfProvince %>% distinct(Province, .keep_all = TRUE)
dfProvinceFellowStatus <- dfProvinceFellowStatus %>% distinct(ProvinceFellowStatus, .keep_all = TRUE)
dfProvinceClass <- dfProvinceClass %>% distinct(ProvinceClass, .keep_all = TRUE)
dfProvinceLevelType <- dfProvinceLevelType %>% distinct(ProvinceLevelType, .keep_all = TRUE)

## SUMMING up columns to verify 

sum(dfProvince$CountProvince)
sum(dfProvinceFellowStatus$CountProvinceFellowStatus)
sum(dfProvinceClass$CountProvinceClass)
sum(dfProvinceLevelType$CountProvinceLevelType)

## Data Visualization on map

# Load shape file
shapename <- read_sf('TM_WORLD_BORDERS_SIMPL-0.3.shp')

leaflet()%>%addTiles()%>%addCircleMarkers(data = dfProvince, lat = ~zw.lat, lng = ~zw.lng,
                                          radius = ~3)

## Exporting Data sets

write.csv(active,"active.csv", row.names = FALSE)
write.csv(dfProvince,"dfProvince.csv", row.names = FALSE)
write.csv(dfProvinceClass,"dfProvinceClass.csv", row.names = FALSE)
write.csv(Inactive, "Inactive.csv", row.names = FALSE)

#####################################################################################################################
#####################################################################################################################

#### THE END OF DATA PROCESSING

####################################################################################################################
####################################################################################################################


 