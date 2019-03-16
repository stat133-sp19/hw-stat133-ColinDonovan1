#############################################################
## Title: Creation of Shot Charts
##
## Description: This file will be used to create shot charts 
## for different GSW players. 
##
## Input: 
## 1. The data files for GSW players Iguodala, 
## Green, Durant, Thompson and Curry, and aggregated data frame
## 2. JPG file with image of NBA basketball court 
## 
## Output: 
## 1. individual shot charts for each player saved as .pdf files
## 2. aggregated shot chart with shot charts for all 5 players
## saved as .pdf file.
## 3. aggregated shot chart with shot charts for all 5 players
## saved as .png file. 
##
## Author: Colin Donovan
#############################################################
library(ggplot2)
library(jpeg)
library(grid)

court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))


## 4.1) Shot Charts of each player
pdf('../images/andre-iguodala-shot-chart.pdf',width=6.5,height=5)
iguodala_shot_chart <- ggplot(data=iguodala) +
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag),size=0.5) +
  ylim(-50,420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
dev.off()

pdf('../images/draymond-green-shot-chart.pdf',width=6.5,height=5)
green_shot_chart <- ggplot(data=green) +
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag),size=0.5) +
  ylim(-50,420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
dev.off()

pdf('../images/kevin-durant-shot-chart.pdf',width=6.5,height=5)
durant_shot_chart <- ggplot(data=durant) +
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag),size=0.5) +
  ylim(-50,420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
dev.off()

pdf('../images/klay-thompson-shot-chart.pdf',width=6.5,height=5)
thompson_shot_chart <- ggplot(data=thompson) +
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag),size=0.5) +
  ylim(-50,420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
dev.off()

pdf('../images/stephen-curry-shot-chart.pdf',width=6.5,height=5)
curry_shot_chart <- ggplot(data=curry) +
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag),size=0.5) +
  ylim(-50,420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
dev.off()

## Facetted Shot Chart
pdf('../images/gsw-shot-charts.pdf',width=8,height=7)
aggregated_shot_chart <- ggplot(data= all_shots) +
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag),size=0.5) +
  ylim(-50,420) +
  ggtitle('Shot Chartr: GSW (2016 season)') +
  theme_minimal() +
  facet_wrap( ~ name, nrow=2,ncol=3) +
  theme(legend.position = 'top')
aggregated_shot_chart
dev.off()

png('../images/gsw-shot-charts.png',width=8,height=7,units='in',res=1500)
aggregated_shot_chart <- ggplot(data= all_shots) +
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag),size=0.5) +
  ylim(-50,420) +
  ggtitle('Shot Chartr: GSW (2016 season)') +
  theme_minimal() +
  facet_wrap( ~ name, nrow=2,ncol=3) +
  theme(legend.position="top")
aggregated_shot_chart
dev.off()
