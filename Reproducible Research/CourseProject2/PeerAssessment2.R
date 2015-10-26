
##  DATA PROCESSING


# Downloading data set and loading it into R

if(!file.exists('data')){
        dir.create('data')
}

fileUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
download.file(url = fileUrl, destfile ='./data/StormData.csv.bz2' )


StormData <- read.csv('./data/StormData.csv.bz2')
str(StormData)


# After looking through columns and finiding information relevant for this assignment I'll subset my data 
# to columns with variables I need

library(dplyr)
data <- select(StormData,BGN_DATE, EVTYPE, FATALITIES:CROPDMGEXP)

data <- tbl_df(data)  # so it prints a little nicer

data$BGN_DATE <- as.Date(as.character(data$BGN_DATE), format = '%m/%d/%Y %H:%M:%S')
data$years <- as.numeric(format(data$BGN_DATE, "%Y"))
hist(data$years, breaks = 60)

# From https://www.ncdc.noaa.gov/stormevents/details.jsp we can see that 
# 1. Tornado: From 1950 through 1954, only tornado events were recorded.
# 2. Tornado, Thunderstorm Wind and Hail: From 1955 through 1992, only tornado, thunderstorm wind and hail 
#    events were keyed from the paper publications into digital data. 
#    From 1993 to 1995, only tornado, thunderstorm wind and hail events have been extracted from 
#    the Unformatted Text Files.
# 3. All Event Types (48 from Directive 10-1605): From 1996 to present, 48 event types are recorded as 
#    defined in NWS Directive 10-1605.  

# From the histogram we can see that the number of records obviously increases significantly with years.
# Since that is an indication of more complete and informative records and to avoid a possible bias created by 
# early records, we will only focus on the data from 1996 onwards.

data <- data[data$years >= 1996,]
nlevels(data$EVTYPE)
levels(data$PROPDMGEXP)
levels(data$CROPDMGEXP)

## As we can see from the  the output of levels() function some values from EVTYPE, PROPDMGEXP and CROPDMGEXP
## need to be modified in order to make them useful for our analysis.

# PROPDMGEXP and CROPDMGEXP have multipliers as their values, where:
# H = Hundred 
# K = Thousand
# M = Million
# B = Billion

# I'll assume that
# 'b' == 'B'
# 'm' == 'M'
# 'k' == 'K'
# 'h' == 'H'

# Signs like '-', '?', '+' are hard to translate into numbers and I'll assume that these are  typos and 
# change their value to 1,  so that they wouldn't affect values in PROPDMG and CROPDMG when multiplied.
# As for numbers '0', 1', '2'etc. I'll leave their values unchanged. 

data$PROPDMGEXP <- gsub('[[:punct:]]', '0', as.character(data$PROPDMGEXP))
data$PROPDMGEXP <- gsub('h', '2', as.character(data$PROPDMGEXP), ignore.case = TRUE)
data$PROPDMGEXP <- gsub('k', '3', as.character(data$PROPDMGEXP), ignore.case = TRUE)
data$PROPDMGEXP <- gsub('m', '6', as.character(data$PROPDMGEXP), ignore.case = TRUE)
data$PROPDMGEXP <- gsub('b', '9', as.character(data$PROPDMGEXP), ignore.case = TRUE)
data$PROPDMGEXP <- as.numeric(data$PROPDMGEXP)


# Note: We can see from levels(data$CROPDMGEXP) that there are no 'h' factor levels in CROPDMGEXP, 
# so we'll leave that out.
data$CROPDMGEXP <- gsub('[[:punct:]]', '0', as.character(data$CROPDMGEXP))
data$CROPDMGEXP <- gsub('k', '3', as.character(data$CROPDMGEXP), ignore.case = TRUE)
data$CROPDMGEXP <- gsub('m', '6', as.character(data$CROPDMGEXP), ignore.case = TRUE)
data$CROPDMGEXP <- gsub('b', '9', as.character(data$CROPDMGEXP), ignore.case = TRUE)
data$CROPDMGEXP <- as.numeric(data$CROPDMGEXP)

# Calculating property and crop damage:
data$PROPDMGEXP <- 10^data$PROPDMGEXP
data$CROPDMGEXP <- 10^data$CROPDMGEXP
data <- mutate(data, propdmg = PROPDMG * PROPDMGEXP, cropdmg = CROPDMG * CROPDMGEXP)
options(scipen = 1)


##  Question 1.
##  Across the United States, which types of events (as indicated in the EVTYPE variable)
##  are most harmful with respect to population health? 


harmful <- 
        data %>%
        group_by(EVTYPE) %>%
        summarize(
                fatalities = sum(FATALITIES, na.rm = TRUE),
                injuries = sum(INJURIES, na.rm = TRUE) 
                ) %>%
        arrange(desc(fatalities), desc(injuries))

harmful

# Since there are 516 unique events in my subsets and Storm Data Event Table contains only 48 permitted
# Storm Data events I'll do some 'cleaning' to see if that changes anything in EVTYPE column 


## Cleaning up the EVTYPE vector 

library(stringr)
harmful$EVTYPE <- str_trim(harmful$EVTYPE, side = 'both')  # removing spaces from strings in EVTYPE vector

# Since summary events don't provide any meaningful information about events or damage made by those events
# I'm going to remove it from EVTYPE vector.
harmful <- harmful[!grepl('summary', harmful$EVTYPE, ignore.case = TRUE),]

harmful$EVTYPE[grepl('tstm|thunderstorm', harmful$EVTYPE, ignore.case = TRUE)] <- 'THUNDERSTORM WIND'
harmful$EVTYPE[grepl('typhoon|hurricane', harmful$EVTYPE, ignore.case = TRUE)] <- 'HURRICANE(TYPHOON)'
harmful$EVTYPE[grepl('tornado', harmful$EVTYPE, ignore.case = TRUE)] <- 'TORNADO'
harmful$EVTYPE[grepl('dry', harmful$EVTYPE, ignore.case = TRUE)] <- 'DROUGHT'
harmful$EVTYPE[grepl('hail', harmful$EVTYPE, ignore.case = TRUE)] <- 'HAIL'
harmful$EVTYPE[grepl('blizzard', harmful$EVTYPE, ignore.case = TRUE)] <- 'BLIZZARD'
#harmful$EVTYPE[grepl('heavy rain|excessive rain|drizzle', harmful$EVTYPE, ignore.case = TRUE)] <- 'HEAVY RAIN'
#harmful$EVTYPE[grepl('excessive cold|wind chill|hypothermia|unseasonably cold|extreme windchill|extreme cold', harmful$EVTYPE, ignore.case = TRUE)] <- 'EXCESSIVE COLD/WIND CHILL'
harmful$EVTYPE[grepl('urban flood|small stream flood|urban|small|minor flood', harmful$EVTYPE, ignore.case = TRUE)] <- 'FLOOD'
harmful$EVTYPE[grepl('flash|flash flood', harmful$EVTYPE, ignore.case = TRUE)] <- 'FLASH FLOOD'
#harmful$EVTYPE[grepl('funnel|funnel cloud', harmful$EVTYPE, ignore.case = TRUE)] <- 'FUNNEL'
#harmful$EVTYPE[grepl('spout|waterspout', harmful$EVTYPE, ignore.case = TRUE)] <- 'WATERSPOUT'
harmful$EVTYPE[grepl('excessive heat|extreme heat|record heat|heat wave|unseasonably warm', harmful$EVTYPE, ignore.case = TRUE)] <- 'EXCESSIVE HEAT'
#harmful$EVTYPE[grepl('rip|rip current', harmful$EVTYPE, ignore.case = TRUE)] <- 'RIP CURRENT'
harmful$EVTYPE[grepl('lightning', harmful$EVTYPE, ignore.case = TRUE)] <- 'LIGHTNING'
harmful$EVTYPE[grepl('heavy snow', harmful$EVTYPE, ignore.case = TRUE)] <- 'HEAVY SNOW'
harmful$EVTYPE[grepl('winter storm', harmful$EVTYPE, ignore.case = TRUE)] <- 'WINTER STORM'
harmful$EVTYPE[grepl('high wind', harmful$EVTYPE, ignore.case = TRUE)] <- 'HIGH WIND'
harmful$EVTYPE[grepl('frost|freeze', harmful$EVTYPE, ignore.case = TRUE)] <- 'FROST/FREEZE'
harmful$EVTYPE[grepl('wild|wildfire|fire', harmful$EVTYPE, ignore.case = TRUE)] <- 'WILDFIRE'

n_distinct(harmful$EVTYPE)


fatal <- harmful %>%
        group_by(EVTYPE) %>%
        summarize(fatalities = sum(fatalities, na.rm = TRUE)) %>%
        arrange(desc(fatalities))


injury <- harmful %>%
        group_by(EVTYPE) %>%
        summarize(injuries = sum(injuries, na.rm = TRUE)) %>%
        arrange(desc(injuries))




top_fatal <- fatal[1:10,]
top_injuries <- injury[1:10,]



library(grid)
library(gridExtra)
library(ggplot2)

f <- ggplot(data = top_fatal, aes(x = EVTYPE, y = fatalities)) + geom_bar(stat="identity", fill = 'pink') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, colour ='black')) +
        labs(title = 'Severe Weather Events Causing Most Fatalities in USA (1995 - 2011)', x = 'Severe weather events', y = 'Number of fatalities') 


i <- ggplot(data = top_injuries, aes(x = EVTYPE, y = injuries)) + geom_bar(stat="identity", fill = 'pink') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, colour ='black')) +
        labs(title = 'Severe Weather Events Causing Most Injuries in USA (1995 -2012)', x = 'Severe Weather events', y = 'Number of injuries') +
        ggtitle('Severe Weather Events Causing Most Injuries in USA (1995 -2012)' )
grid.arrange(f, i, ncol = 2)


## Question 2
## Across the United States, which types of events have the greatest economic consequences?
options(scipen = 1)
damage <- data %>%
        group_by(EVTYPE) %>%
        summarize(propdmg = sum(propdmg, na.rm = TRUE),
                  cropdmg = sum(cropdmg, na.rm = TRUE)) %>%
        arrange(desc(propdmg), desc(cropdmg))


damage

## Cleaning up the EVTYPE vector 
damage$EVTYPE <- str_trim(damage$EVTYPE, side = 'both')  # removing spaces from strings in EVTYPE vector
damage <- damage[!grepl('summary', damage$EVTYPE, ignore.case = TRUE),]
damage$EVTYPE[grepl('tornado', damage$EVTYPE, ignore.case = TRUE)] <- 'TORNADO'
damage$EVTYPE[grepl('wild|wildfire|fire', damage$EVTYPE, ignore.case = TRUE)] <- 'WILDFIRE'
damage$EVTYPE[grepl('typhoon|hurricane', damage$EVTYPE, ignore.case = TRUE)] <- 'HURRICANE(TYPHOON)'
damage$EVTYPE[grepl('urban flood|small stream flood|urban|small|minor flood|major flood|river flood|floods|stream flood|ice jam flooding|excessive wetness|wet conditions', damage$EVTYPE, ignore.case = TRUE)] <- 'FLOOD'
damage$EVTYPE[grepl('coastal flood|coastal flooding|cstl|coastal  flooding|coastal erosion', damage$EVTYPE, ignore.case = TRUE)] <- 'COASTAL FLOOD'
damage$EVTYPE[grepl('tstm|thunderstorm', damage$EVTYPE, ignore.case = TRUE)] <- 'THUNDERSTORM WIND'
damage$EVTYPE[grepl('hail', damage$EVTYPE, ignore.case = TRUE)] <- 'HAIL'
damage$EVTYPE[grepl('heavy snow|excessive snow', damage$EVTYPE, ignore.case = TRUE)] <- 'HEAVY SNOW'
damage$EVTYPE[grepl('high wind', damage$EVTYPE, ignore.case = TRUE)] <- 'HIGH WIND'
damage$EVTYPE[grepl('frost|freeze', damage$EVTYPE, ignore.case = TRUE)] <- 'FROST/FREEZE'
damage$EVTYPE[grepl('heavy rain|excessive rain|drizzle|heavy precipitation|unseasonable rain', damage$EVTYPE, ignore.case = TRUE)] <- 'HEAVY RAIN'
damage$EVTYPE[grepl('flash|flash flood', damage$EVTYPE, ignore.case = TRUE)] <- 'FLASH FLOOD'
damage$EVTYPE[grepl('excessive cold|wind chill|unseasonably cold|unseasonable cold|extreme windchill|extreme cold', damage$EVTYPE, ignore.case = TRUE)] <- 'EXCESSIVE COLD/WIND CHILL'
n_distinct(damage$EVTYPE)


property <- damage %>%
        group_by(EVTYPE) %>%
        summarize(propdmg = sum(propdmg, na.rm = TRUE)) %>%
        arrange(desc(propdmg))


crop <- damage %>%
        group_by(EVTYPE) %>%
        summarize(cropdmg = sum(cropdmg, na.rm = TRUE)) %>%
        arrange(desc(cropdmg))


top_prop <- property[1:10,]
top_crop <- crop[1:10,]

p <- ggplot(top_prop, aes(EVTYPE, propdmg)) + geom_bar(stat = 'identity', fill = 'pink') + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, colour ='black')) +
        labs(title = 'Severe Weather Events Causing Greatest Property Damage', x = 'Severe weather events', y = 'Damage in Dollars')
        
c <- ggplot(top_crop, aes(EVTYPE, cropdmg)) + geom_bar(stat = 'identity', fill = 'pink') + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, colour ='black')) +
        labs(title = 'Severe Weather Events Causing Greatest Crop Damage', x = 'Severe weather events', y = 'Damage in Dollars')

grid.arrange(p, c, ncol = 2)
