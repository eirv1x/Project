##activate libraries
library(tidyverse)
library(here)
library(readxl)
library(ggrepel)

##setwd
setwd("C:/Users/Ellie/OneDrive/Documents/onedrive uni/OneDrive/masters/Data managment and visualisation/main_project/")

##opened excel spreadsheet and now stored in a df in R

pwbestlocalauth <- read.csv(here("data", "pwbest_localauth_filtered.csv"))
View(pwbestlocalauth)
head(pwbestlocalauth)

##data wrangling is needed to rename and remove columns that are not necessary for visualisation
#rename: change column/variable names to something short
pwbestlocalauth <- pwbestlocalauth %>% 
  rename(Geography = Title, pwb = X, '2016-17' = X.2, '2017-18' = X.6, '2018-19' = X.10, '2019-20' = X.14, '2020-21' = X.18)

# use select to just look at some columns
pwbestlocalauth <- pwbestlocalauth %>% 
  select(Geography, pwb, '2016-17', '2017-18','2018-19', '2019-20', '2020-21')

#remove rows from dataset
pwbestlocalauth <- pwbestlocalauth[-c(1,2),]

#practice creating a graph for 2016-2017 data

p1617 <- ggplot(data=pwbestlocalauth, mapping = aes(x=pwb,y=`2016-17`,color = Geography))
p1617 + geom_point() + geom_smooth(alpha=0.3, method = 'lm') + labs(x = "Personal Wellbeing Measures",
                                                                 y = "Scores", title = "Personal Wellbeing Scores in 2016-2017") + geom_jitter() + geom_text_repel(aes(label=Geography))
ggsave(here("figs", "2016-17pwbgeog.png"))

#creating graph for 2017-2018 data and other years

p1718 <- ggplot(data=pwbestlocalauth, mapping = aes(x=pwb,y=`2017-18`,color = Geography))
p1718 + geom_point() + geom_smooth(alpha=0.3, method = 'lm') + labs(x = "Personal Wellbeing Measures",
                                                                    y = "Scores", title = "Personal Wellbeing Scores in 2017-2018") + geom_jitter() + geom_text_repel(aes(label=Geography))
ggsave(here("figs", "2017-18pwbgeog.png"))

p1819 <- ggplot(data=pwbestlocalauth, mapping = aes(x=pwb,y=`2018-19`,color = Geography))
p1819 + geom_point() + geom_smooth(alpha=0.3, method = 'lm') + labs(x = "Personal Wellbeing Measures",
                                                                    y = "Scores", title = "Personal Wellbeing Scores in 2018-2019") + geom_jitter() + geom_text_repel(aes(label=Geography))
ggsave(here("figs", "2018-19pwbgeog.png"))

p1920 <- ggplot(data=pwbestlocalauth, mapping = aes(x=pwb,y=`2019-20`,color = Geography))
p1920 + geom_point() + geom_smooth(alpha=0.3, method = 'lm') + labs(x = "Personal Wellbeing Measures",
                                                                    y = "Scores", title = "Personal Wellbeing Scores in 2019-2020") + geom_jitter() + geom_text_repel(aes(label=Geography))
ggsave(here("figs", "2019-20pwbgeog.png"))

p2021 <- ggplot(data=pwbestlocalauth, mapping = aes(x=pwb,y=`2020-21`,color = Geography))
p2021 + geom_point() + geom_smooth(alpha=0.3, method = 'lm') + labs(x = "Personal Wellbeing Measures",
                                                                    y = "Scores", title = "Personal Wellbeing Scores in 2020-2021") + geom_jitter() + geom_text_repel(aes(label=Geography))
ggsave(here("figs", "2020-21pwbgeog.png"))




