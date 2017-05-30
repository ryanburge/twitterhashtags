
timeline <- userTimeline('OriolesUmp', n=3200)
orioles <- twListToDF(timeline)
orioles$team <- c("Orioles")


timeline <- userTimeline('RedSoxUmp', n=3200)
redsox <- twListToDF(timeline)
redsox$team <- c("Red Sox")


timeline <- userTimeline('WhiteSoxUmp', n=3200)
whitesox <- twListToDF(timeline)
whitesox$team <- c("White Sox")


timeline <- userTimeline('IndiansUmp', n=3200)
indians <- twListToDF(timeline)
indians$team <- c("Indians")

timeline <- userTimeline('TigersUmp', n=3200)
tigers <- twListToDF(timeline)
tigers$team <- c("Tigers")

timeline <- userTimeline('AstrosUmp', n=3200)
astros <- twListToDF(timeline)
astros$team <- c("Astros")

timeline <- userTimeline('RoyalsUmp', n=3200)
royals <- twListToDF(timeline)
royals$team <- c("Royals")

timeline <- userTimeline('AngelsUmp', n=3200)
angels <- twListToDF(timeline)
angels$team <- c("Angels")

timeline <- userTimeline('TwinsUmp', n=3200)
twins <- twListToDF(timeline)
twins$team <- c("Twins")

timeline <- userTimeline('YankeesUmp', n=3200)
yankees <- twListToDF(timeline)
yankees$team <- c("Yankees")

timeline <- userTimeline('AthleticsUmp', n=3200)
athletics <- twListToDF(timeline)
athletics$team <- c("Athletics")

timeline <- userTimeline('MarinersUmp', n=3200)
mariners <- twListToDF(timeline)
mariners$team <- c("Mariners")

timeline <- userTimeline('RaysUmp', n=3200)
rays <- twListToDF(timeline)
rays$team <- c("Rays")

timeline <- userTimeline('RangersUmp', n=3200)
rangers <- twListToDF(timeline)
rangers$team <- c("Rangers")

timeline <- userTimeline('BlueJaysUmp', n=3200)
bluejays <- twListToDF(timeline)
bluejays$team <- c("Blue Jays")

al <- rbind(orioles, redsox, whitesox, indians, tigers, astros, royals, angels, twins, yankees, athletics, mariners, rays, rangers, bluejays)
al$league <- c("American League")

## National League

timeline <- userTimeline('DiamondbacksUmp', n=3200)
dbacks <- twListToDF(timeline)
dbacks$team <- c("Diamondbacks")

timeline <- userTimeline('BravesUmp', n=3200)
braves <- twListToDF(timeline)
braves$team <- c("Braves")

timeline <- userTimeline('CubsUmp', n=3200)
cubs <- twListToDF(timeline)
cubs$team <- c("Cubs")

timeline <- userTimeline('RedsUmp', n=3200)
reds <- twListToDF(timeline)
reds$team <- c("Reds")

timeline <- userTimeline('RockiesUmp', n=3200)
rockies <- twListToDF(timeline)
rockies$team <- c("Rockies")

timeline <- userTimeline('DodgersUmp', n=3200)
dodgers <- twListToDF(timeline)
dodgers$team <- c("Dodgers")

timeline <- userTimeline('MarlinsUmp', n=3200)
marlins <- twListToDF(timeline)
marlins$team <- c("Marlins")

timeline <- userTimeline('BrewersUmp', n=3200)
brewers <- twListToDF(timeline)
brewers$team <- c("Brewers")

timeline <- userTimeline('MetsUmp', n=3200)
mets <- twListToDF(timeline)
mets$team <- c("Mets")

timeline <- userTimeline('PhilliesUmp', n=3200)
phillies <- twListToDF(timeline)
phillies$team <- c("Phillies")

timeline <- userTimeline('PiratesUmp', n=3200)
pirates <- twListToDF(timeline)
pirates$team <- c("Pirates")

timeline <- userTimeline('PadresUmp', n=3200)
padres <- twListToDF(timeline)
padres$team <- c("Padres")

timeline <- userTimeline('GiantsUmp', n=3200)
giants <- twListToDF(timeline)
giants$team <- c("Giants")

timeline <- userTimeline('CardinalsUmp', n=3200)
cards <- twListToDF(timeline)
cards$team <- c("Cardinals")

timeline <- userTimeline('NationalsUmp', n=3200)
nationals <- twListToDF(timeline)
nationals$team <- c("Nationals")

nl <- rbind(dbacks, reds, cubs, braves, rockies, dodgers, marlins, brewers, mets, phillies, pirates, padres, giants, cards, nationals)
nl$league <- c("National League")

total <- rbind(al, nl)

total$good <- substr(total$text, 6, 10)

total$year <- format(as.Date(total$created, format="%Y/%m/%d"),"%Y")

#total <- total %>% filter(year ==2017)

t17 <- total %>%  select(team, good, league) %>% group_by(team, good) %>% 
  summarise(n =n()) %>% arrange(-n) 

league <- total %>% group_by(team, league) %>% summarise()
t17 <-  t17 %>% left_join(league)


bad <- t17 %>% filter(good == "hurts")

ggplot(bad, aes(x=reorder(team,n), y=n, fill = league)) + geom_col(position = "dodge") + ylab("Number of Hurtful Calls in 2017") + 
  xlab("Starting Pitcher") +  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")  + scale_fill_manual(values =c("firebrick3", "dodgerblue3"), labels = c("American League"," National League")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Which Team is Getting the Worst Luck?") +
  labs(caption = "Data from @Ump Twitter Accounts") + coord_flip()

## Teams with Most Helpful Calls

good <- t17 %>% filter(good == "helps")

ggplot(good, aes(x=reorder(team,n), y=n, fill = league)) + geom_col(position = "dodge") + ylab("Number of Helpful Calls in 2017") + 
  xlab("Starting Pitcher") +  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")  + scale_fill_manual(values =c("firebrick3", "dodgerblue3"), labels = c("American League"," National League")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Which Team is Getting the Best Luck?") +
  labs(caption = "Data from @Ump Twitter Accounts") + coord_flip()


## Overall Luck Score 

bad <- bad %>% mutate(total = n*-1)
good <- good %>% mutate(gtotal = n*1)

bad <- bad %>% arrange(team)
good <- good %>% arrange(team)

good$btotal <- bad$total

plot <- good %>% mutate(overall = gtotal + btotal) %>% select(team, league, overall) %>% arrange(-overall)

ggplot(plot, aes(x=reorder(team,overall), y=overall, fill = league)) + geom_col(position = "dodge") + ylab("Hurtful Calls Subtracted From Helpful Calls") + 
  xlab("Team") +  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")  + scale_fill_manual(values =c("firebrick3", "dodgerblue3"), labels = c("American League"," National League")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Which Team is Getting the Best Luck?") +
  labs(caption = "Data from @Ump Twitter Accounts") + coord_flip()

ggsave(file="full_results.png", type = "cairo-png", width = 15, height = 10)



sked16 <- read_csv("D:/twitterhashtags/sked16.TXT")
sked16$date <- sked16$`20160403`
sked16$visiting <- sked16$NYN
sked16$home <- sked16$KCA
sked16 <- sked16 %>% select(date, visiting, home)


home <- sked16 %>% select(date, home)
visiting <- sked16 %>% select(date, visiting)
total$newdate <- sub("\\s.*", "", total$created)
total$newdate <- as.numeric(gsub("-", "", total$newdate))


home$team <- Recode(home$home, "'ANA' = 'Angels';
                    'ARI' = 'Diamondbacks';
                    'ATL' = 'Braves';
                    'BAL' = 'Orioles';
                    'BOS' = 'Red Sox';
                    'CHN' = 'Cubs';
                    'CHA' = 'White Sox';
                    'CIN' = 'Reds';
                    'CLE' = 'Indians';
                    'COL' = 'Rockies';
                    'DET' = 'Tigers';
                    'HOU' = 'Astros';
                    'KCA' = 'Royals';
                    'LAN' = 'Dodgers';
                    'MIA' = 'Marlins';
                    'MIL' = 'Brewers';
                    'MIN' = 'Twins';
                    'NYA' = 'Yankees';
                    'NYN' = 'Mets';
                    'OAK' = 'Athletics';
                    'PHI' = 'Phillies';
                    'PIT' = 'Pirates';
                    'SDN' = 'Padres';
                    'SEA' = 'Mariners';
                    'SFN' = 'Giants';
                    'SLN' = 'Cardinals';
                    'TBA' = 'Rays';
                    'TEX' = 'Rangers';
                    'TOR' = 'Blue Jays';
                    'WAS' = 'Nationals'")

m1 <- total %>% filter(year ==2016) %>% select(team, league, good, year, newdate)
homejoin <- inner_join(home, m1, by =c("date" = "newdate", "team" = "team"))

homejoin <- homejoin %>%
  select(team, good, league) %>% group_by(team, good) %>%
  summarise(n =n()) %>% arrange(-n) %>% ungroup(team, good)

homehurt <- homejoin %>% filter(good == "hurts") %>% mutate(count = n*-1) %>% select(team, count) %>% arrange(team)
homehelp <- homejoin %>% filter(good == "helps") %>% select(team, n) %>% arrange(team)

home <-  homehelp %>%  mutate(overall=homehurt$count + homehelp$n) %>% select(team, overall) %>% mutate(type = c("Home Team"))


visiting$team <- Recode(visiting$visiting, "'ANA' = 'Angels';
                    'ARI' = 'Diamondbacks';
                    'ATL' = 'Braves';
                    'BAL' = 'Orioles';
                    'BOS' = 'Red Sox';
                    'CHN' = 'Cubs';
                    'CHA' = 'White Sox';
                    'CIN' = 'Reds';
                    'CLE' = 'Indians';
                    'COL' = 'Rockies';
                    'DET' = 'Tigers';
                    'HOU' = 'Astros';
                    'KCA' = 'Royals';
                    'LAN' = 'Dodgers';
                    'MIA' = 'Marlins';
                    'MIL' = 'Brewers';
                    'MIN' = 'Twins';
                    'NYA' = 'Yankees';
                    'NYN' = 'Mets';
                    'OAK' = 'Athletics';
                    'PHI' = 'Phillies';
                    'PIT' = 'Pirates';
                    'SDN' = 'Padres';
                    'SEA' = 'Mariners';
                    'SFN' = 'Giants';
                    'SLN' = 'Cardinals';
                    'TBA' = 'Rays';
                    'TEX' = 'Rangers';
                    'TOR' = 'Blue Jays';
                    'WAS' = 'Nationals'")

m1 <- total %>% filter(year ==2016) %>% select(team, league, good, year, newdate)
visitjoin <- inner_join(visiting, m1, by =c("date" = "newdate", "team" = "team"))

visitjoin <- visitjoin %>%
  select(team, good, league) %>% group_by(team, good) %>%
  summarise(n =n()) %>% arrange(-n) %>% ungroup(team, good)

visithurt <- visitjoin %>% filter(good == "hurts") %>% mutate(count = n*-1) %>% select(team, count) %>% arrange(team)
visithelp <- visitjoin %>% filter(good == "helps") %>% select(team, n) %>% arrange(team)

visit <-  visithelp %>%  mutate(overall=visithurt$count + visithelp$n) %>% select(team, overall) %>% mutate(type = c("Visiting Team"))

home <- home %>% left_join(league)
visit <- visit %>% left_join(league)

plot <- rbind(home, visit)

ggplot(home, aes(x=reorder(team,overall), y=overall, fill = league)) + geom_col(position = "dodge") + ylab("Hurtful Calls Subtracted From Helpful Calls") + 
  xlab("Team") +  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")  + scale_fill_manual(values =c("firebrick3", "dodgerblue3"), labels = c("American League"," National League")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Which Team is Getting the Best Luck?") +
  labs(caption = "Data from @Ump Twitter Accounts") + coord_flip()

plot %>% filter(league == "American League") %>% ggplot(., aes(x=reorder(team,overall), y=overall, fill = type)) + geom_col(position = "dodge") + ylab("Hurtful Calls Subtracted From Helpful Calls") + 
  xlab("Team") +  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")  + scale_fill_manual(values =c("firebrick3", "dodgerblue3"), labels = c("Home"," Away")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Which Team is Getting the Best Luck?") +
  labs(caption = "Data from @Ump Twitter Accounts") + coord_flip()


plot %>% filter(league == "National League") %>% ggplot(., aes(x=reorder(team,overall), y=overall, fill = type)) + geom_col(position = "dodge") + ylab("Hurtful Calls Subtracted From Helpful Calls") + 
  xlab("Team") +  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")  + scale_fill_manual(values =c("firebrick3", "dodgerblue3"), labels = c("Home"," Away")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=24, family="KerkisSans")) + ggtitle("Which Team is Getting the Best Luck?") +
  labs(caption = "Data from @Ump Twitter Accounts") + coord_flip()


