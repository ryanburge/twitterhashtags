
library(dplyr)

timeline <- userTimeline('CardinalsUmp', n=5000)
timeline <- twListToDF(timeline)

timeline$year <- format(as.Date(timeline$created, format="%Y/%m/%d"),"%Y")


timeline$t1 <- substr(timeline$text, 55, 75)
timeline$t2 <- gsub("([A-Za-z]+).*", "\\1", timeline$t1)
timeline$good <- substr(timeline$text, 5, 10)



t17 <- timeline %>% filter(year == 2017) %>% 
  select(t2, good) %>% group_by(t2, good) %>% 
  summarise(n =n()) %>% arrange(-n) 

t17$good <- Recode("'hurts'= 'Call Hurts'")

#t17[good == "hurts"] <- "Hurtful Call"
#t17[good == "helps"] <- "Helpful Call"

starters <- t17 %>% filter(t2 == "Leake" | t2 == "Martinez" | t2 == "Wainwright" | t2 == "Lynn" | t2 == "Wacha")

relievers <- t17 %>% filter(t2 == "Oh" | t2 == "Bowman" | t2 == "Siegrist" | t2 == "Broxton" | t2 == "Cecil" | t2 == "Lyons" | t2 == "Socolovich" | t2 == "Rosenthal" | t2 == "Tuivailala")

ggplot(starters, aes(x=t2, y=n, fill = good)) + geom_col(position = "dodge") + ylab("Number of Calls") + 
  xlab("Starting Pitcher") +  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")  + scale_fill_manual(values =c("firebrick3", "black"), labels = c("Helpful Call", "Hurtful Call")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Which Cardinals Starter Is Getting the Most Calls?") +
  labs(caption = "Data from @CardinalsUmp")

ggsave(file="starters.png", type = "cairo-png", width = 15, height = 10)


ggplot(relievers, aes(x=t2, y=n, fill = good)) + geom_col(position = "dodge") + ylab("Number of Calls") + 
  xlab("Starting Pitcher") +  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")  + scale_fill_manual(values =c("firebrick3", "black"), labels = c("Helpful Call", "Hurtful Call")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Which Cardinals Reliever Is Getting the Most Calls?") +
  labs(caption = "Data from @CardinalsUmp")

ggsave(file="relievers.png", type = "cairo-png", width = 15, height = 10)



