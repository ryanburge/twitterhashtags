library(twitter)
library(ROAuth)
library(dplyr)
library(ggplot2)
library(car)
library(extrafont)

timeline <- userTimeline('CardinalsUmp', n=3200)
timeline$team <- c("Cardinals")
cards <- twListToDF(timeline)


timeline$year <- format(as.Date(timeline$created, format="%Y/%m/%d"),"%Y")

timeline <- timeline %>% filter(year ==2017)

## This tries to grab the name of the pitcher ## 55 for Cardinals
timeline$t1 <- substr(timeline$text, 55, 65)
##This grabs the first word from the previous substring
timeline$t2 <- gsub("([A-Za-z]+).*", "\\1", timeline$t1)
## This grabs hurts or helps"
timeline$good <- substr(timeline$text, 6, 10)



t17 <- timeline %>% filter(year == 2017) %>% 
  select(t2, good) %>% group_by(t2, good) %>% 
  summarise(n =n()) %>% arrange(-n) 


starters <- t17 %>% filter(t2 == "Leake" | t2 == "Martinez" | t2 == "Wainwright" | t2 == "Lynn" | t2 == "Wacha")
#starters <- t17 %>% filter(t2 == "Lester" | t2 == "Lackey" | t2 == "Hendricks" | t2 == "Arrieta" | t2 == "Anderson" | t2 = "Butler")

relievers <- t17 %>% filter(t2 == "Oh" | t2 == "Bowman" | t2 == "Siegrist" | t2 == "Broxton" | t2 == "Cecil" | t2 == "Lyons" | t2 == "Socolovich" | t2 == "Rosenthal" | t2 == "Tuivailala")

ggplot(starters, aes(x=t2, y=n, fill = good)) + geom_col(position = "dodge") + ylab("Number of Calls") + 
  xlab("Starting Pitcher") +  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")  + scale_fill_manual(values =c("dodgerblue3", "black"), labels = c("Helpful Call", "Hurtful Call")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Which Cubs Starter Is Getting the Most Calls?") +
  labs(caption = "Data from @CubsUmp")

##ggsave(file="starters.png", type = "cairo-png", width = 15, height = 10)


ggplot(relievers, aes(x=t2, y=n, fill = good)) + geom_col(position = "dodge") + ylab("Number of Calls") + 
  xlab("Starting Pitcher") +  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")  + scale_fill_manual(values =c("firebrick3", "black"), labels = c("Helpful Call", "Hurtful Call")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Which Cardinals Reliever Is Getting the Most Calls?") +
  labs(caption = "Data from @CardinalsUmp")

##ggsave(file="relievers.png", type = "cairo-png", width = 15, height = 10)

