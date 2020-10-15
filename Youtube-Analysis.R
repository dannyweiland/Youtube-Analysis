#Title: Analysis of Trending Youtube Videos
#Author: Daniel Weiland
#Description: EDA
#Version: 1.0

#Delete Environment-----
rm(list = ls())
par(mfrow = c(1,1))


#Libraries------
#Data Manipulation
library(dplyr)
library(readxl)
library(MASS)
library(lubridate)
library(tidyr)
#Data Visualization 
library(ggplot2)
library(corrplot)

#Import Data-----
youtube <- read.csv("USvideos.csv")

#Data Cleaning
youtube$trending_date <- ydm(youtube$trending_date)

youtube <- youtube %>% 
  mutate(
  category_name=case_when(
      category_id==1 ~ 'Film and Animation',
      category_id==2 ~ 'Autos & Vehicles',
      category_id==10 ~ 'Music',
      category_id==15 ~ 'Pets & Animals',
      category_id==17 ~ 'Sports',
      category_id==19 ~ 'Travel & Events',
      category_id==20 ~ 'Gaming',
      category_id==23 ~ 'Comedy',
      category_id==24 ~ 'Entertainment',
      category_id==25 ~ 'News & Politics',
      category_id==26 ~ 'Howto & Style',
      category_id==27 ~ 'Education',
      category_id==28 ~ 'Science & Technology',
      category_id==29 ~ 'Nonprofits & Activism'))
youtube$category_name[is.na(youtube$category_name, value)] <- 'None'


ggplot(youtube, aes(category_name, fill = category_name))+
  geom_bar(stat = 'count')+
  xlab("Category")+
  ylab("Count") +
  theme(axis.text.x=element_blank())


