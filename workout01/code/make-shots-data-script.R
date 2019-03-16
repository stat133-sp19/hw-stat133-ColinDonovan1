#############################################################
## Title: Data Preparation for Shot Charts
## Description: This file will be used during the data 
## preparation stage and will result in the 
## shots-data.summary.txt file in the output folder which
## will then be used in part 4) to obtain shot charts for
## different GSW players.
## Input: The data files for GSW players Iguodala, 
## Green, Durant, Thompson and Curry.
## Output: 
## 1. individual text files like the ones imported, but
## with added columns 'name' and 'minute' and changes to
## 'shot_made_flag' columns.
## 2. shots-data-summary.txt file with aggregated dataframe
## of all shots.
## Author: Colin Donovan
#############################################################


## Reading data sets
url1= 'https://github.com/ucb-stat133/stat133-hws/blob/master/data/andre-iguodala.csv'
iguodala <- read.table(url1 , sep= ',')
url2= 'https://github.com/ucb-stat133/stat133-hws/blob/master/data/draymond-green.csv'
green <- read.table(url2 , sep= ',')
url3= 'https://github.com/ucb-stat133/stat133-hws/blob/master/data/kevin-durant.csv'
durant <- read.table(url3, sep= ',')
url4= 'https://github.com/ucb-stat133/stat133-hws/blob/master/data/klay-thompson.csv'
thompson <- read.table(url4 , sep= ',')
url5= 'https://github.com/ucb-stat133/stat133-hws/blob/master/data/stephen-curry.csv'
curry <-  read.table(url4 , sep= ',')

## Setting up column nam
iguodala['name'] = 'Andre Iguodala'
green['name'] = 'Draymond Green'
durant['name'] = 'Kevin Durant'
thompson['name'] = 'Klay Thompson'
curry['name'] = 'Stephen Curry'

## Change shot_made_flag
iguodala_idx = which(iguodala['shot_made_flag'] == 'y')
iguodala[iguodala_idx,'shot_made_flag'] = 'shot_yes'
iguodala[-iguodala_idx,'shot_made_flag'] = "shot_no"

green_idx = which(green['shot_made_flag'] == 'y')
green[green_idx,'shot_made_flag'] = 'shot_yes'
green[-green_idx,'shot_made_flag'] = "shot_no"

durant_idx = which(durant['shot_made_flag'] == 'y')
durant[durant_idx,'shot_made_flag'] = 'shot_yes'
durant[-durant_idx,'shot_made_flag'] = "shot_no"

thompson_idx = which(thompson['shot_made_flag'] == 'y')
thompson[thompson_idx,'shot_made_flag'] = 'shot_yes'
thompson[-thompson_idx,'shot_made_flag'] = "shot_no"

curry_idx = which(curry['shot_made_flag'] == 'y')
curry[curry_idx,'shot_made_flag'] = 'shot_yes'
curry[-curry_idx,'shot_made_flag'] = "shot_no"


## Add column minute
iguodala['minute'] = (iguodala$period-1) * 12 + (12-iguodala$minutes_remaining)
green['minute'] = (green$period-1) * 12 + (12-green$minutes_remaining)
durant['minute'] = (durant$period-1) * 12 + (12-durant$minutes_remaining)
thompson['minute'] = (thompson$period-1) * 12 + (12-thompson$minutes_remaining)
curry['minute'] = (curry$period-1) * 12 + (12-curry$minutes_remaining)


## Output summary of dataframes into text files
sink('../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink('../output/draymond-green-summary.txt')
summary(green)
sink()

sink('../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink('../output/klay-thompson-summary.txt')
summary(thompson)
sink()

sink('../output/stephen-curry-summary.txt')
summary(curry)
sink()

## Stack tables
shots_data <- rbind(iguodala,green,durant,thompson,curry)

## Exporting assembled table
write.csv(shots_data,"../data/shots-data.csv")

## Output summary of assembled table into text file
sink('../output/shots-data-summary.txt')
summary(shots_data)
sink()

