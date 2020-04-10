library(ebirdst)
library(rnaturalearth)
library(ggplot2)
library(viridisLite)
library(dplyr)
library(gridExtra)
library(tidyverse)




eBird<- read_csv("Data/eBird_Regions_Weeks.csv")

Dates <- read_csv("Data/dates.csv") %>%
  mutate(st_week = ST_week) %>%
  select (date, st_week)

## Here we re-order the S&T weeks to start with Sept 7
Date_vec <-rbind(Dates[36:52,], Dates[1:35,]) %>%
  mutate(Duck_week = c(seq(1:52)))


IL_Duck_1 <- read_csv("Data/IL_Ducks_Data.csv", 
                      col_types = cols(DAY = col_date(format = "%m/%d/%Y"))) %>%
  mutate(st_week = date_to_st_week(DAY)) %>%
  select(c("YEAR","County", "AGWT", "st_week", "LOCATION", "REG", "DAY")) %>%
  filter(REG == 1)



IL_Duck_2 <- read_csv("Data/IL_Ducks_Data.csv", 
                      col_types = cols(DAY = col_date(format = "%m/%d/%Y"))) %>%
  mutate(st_week = date_to_st_week(DAY)) %>%
  select(c("YEAR","County", "AGWT", "st_week", "LOCATION", "REG")) %>%
  filter(REG == 2)


IL_Duck_3 <- read_csv("Data/IL_Ducks_Data.csv", 
                      col_types = cols(DAY = col_date(format = "%m/%d/%Y"))) %>%
  mutate(st_week = date_to_st_week(DAY)) %>%
  select(c("YEAR","County","AGWT", "st_week", "LOCATION", "REG")) %>%
  filter(REG == 3)

IL_Duck_4 <- read_csv("Data/IL_Ducks_Data.csv", 
                      col_types = cols(DAY = col_date(format = "%m/%d/%Y"))) %>%
  mutate(st_week = date_to_st_week(DAY)) %>%
  select(c("YEAR","County","AGWT", "st_week", "LOCATION", "REG")) %>%
  filter(REG == 4)


## Only use 2018 data
sub_18_1 <- IL_Duck_1[which(IL_Duck_1$YEAR == 2018),]


counts_R1 <- sub_18_1 %>%
  group_by(st_week, County) %>% 
  summarise(AGWT = sum(AGWT)) %>%
  group_by(st_week)%>%
  summarise(AGWT = sum(AGWT)) %>%
  mutate(pct = AGWT/max(AGWT),Region = "1")

counts_R1[counts_R1$st_week == 35, "st_week"] <- 36


# Re-order weeks to start in Sept.
counts_R1_DW <- merge(counts_R1, Date_vec, by = "st_week")




sub_18_2 <- IL_Duck_2[which(IL_Duck_2$YEAR == 2018),]


counts_R2 <- sub_18_2 %>%
  group_by(st_week, County) %>% 
  summarise(AGWT = sum(AGWT))%>%
  group_by(st_week)%>%
  summarise(AGWT = sum(AGWT))%>%
  mutate(pct = AGWT/max(AGWT), Region = "2")

counts_R2[counts_R2$st_week == 35, "st_week"] <- 36

counts_R2_DW <- merge(counts_R2, Date_vec, by = "st_week")



sub_18_3 <- IL_Duck_3[which(IL_Duck_3$YEAR == 2018),]


counts_R3 <- sub_18_3 %>%
  group_by(st_week, County) %>% 
  summarise(AGWT = sum(AGWT))%>%
  group_by(st_week)%>%
  summarise(AGWT = sum(AGWT))%>%
  mutate(pct = AGWT/max(AGWT),Region = "3")


counts_R3[counts_R3$st_week == 35, "st_week"] <- 36

counts_R3_DW <- merge(counts_R3, Date_vec, by = "st_week")

sub_18_4 <- IL_Duck_4[which(IL_Duck_4$YEAR == 2018),]


counts_R4 <- sub_18_4 %>%
  group_by(st_week, County) %>% 
  summarise(AGWT = sum(AGWT))%>%
  group_by(st_week)%>%
  summarise(AGWT = sum(AGWT))%>%
  mutate(pct = AGWT/max(AGWT), Region = "4")

counts_R4[counts_R4$st_week == 35, "st_week"] <- 36

counts_R4_DW <- merge(counts_R4, Date_vec, by = "st_week")


IL_surv <- rbind(counts_R1_DW, counts_R2_DW, counts_R3_DW, counts_R4_DW)










Co_1 <- ggplot(data = IL_surv[which(IL_surv$Region == 1 & IL_surv$Duck_week < 36),], aes(Duck_week, pct)) +
  geom_line(aes(color = "IL Survey"))+
  geom_rug(alpha=0.5, size=1.5, sides = "b", show.legend = TRUE, aes(color = "IL Survey Date"))+
  geom_line(aes(Duck_week, AGWT_pct, color = "eBird S&T"), eBird[which(eBird$Region==1 & eBird$Duck_week < 35),])+
  ylab("% of max abundance") +
  xlab("Week") +
  ggtitle("AGWT Region 1")+
  theme_light()+
  theme(legend.title = element_blank())


Co_2 <- ggplot(data = IL_surv[which(IL_surv$Region == 2 & IL_surv$Duck_week < 36),], aes(Duck_week, pct)) +
  geom_line(aes(color = "IL Survey"))+
  geom_rug(alpha=0.5, size=1.5, sides = "b", show.legend = TRUE, aes(color = "IL Survey Date"))+
  geom_line(aes(Duck_week, AGWT_pct, color = "eBird S&T"), eBird[which(eBird$Region==2 & eBird$Duck_week < 35),])+
  ylab("% of max abundance") +
  xlab("Week") +
  ggtitle("AGWT Region 2")+
  theme_light()+
  theme(legend.title = element_blank())


Co_3 <- ggplot(data = IL_surv[which(IL_surv$Region == 3 & IL_surv$Duck_week < 36),], aes(Duck_week, pct)) +
  geom_line(aes(color = "IL Survey"))+
  geom_rug(alpha=0.5, size=1.5, sides = "b", show.legend = TRUE, aes(color = "IL Survey Date"))+
  geom_line(aes(Duck_week, AGWT_pct, color = "eBird S&T"), eBird[which(eBird$Region==3 & eBird$Duck_week < 35),])+
  ylab("% of max abundance") +
  xlab("Week") +
  ggtitle("AGWT Region 3")+
  theme_light()+
  theme(legend.title = element_blank())



Co_4 <- ggplot(data = IL_surv[which(IL_surv$Region == 4 & IL_surv$Duck_week < 36),], aes(Duck_week, pct)) +
  geom_line(aes(color = "IL Survey"))+
  geom_rug(alpha=0.5, size=1.5, sides = "b", show.legend = TRUE, aes(color = "IL Survey Date"))+
  geom_line(aes(Duck_week, AGWT_pct, color = "eBird S&T"), eBird[which(eBird$Region==4 & eBird$Duck_week < 35),])+
  ylab("% of max abundance") +
  xlab("Week") +
  ggtitle("AGWT Region 4")+
  theme_light()+
  theme(legend.title = element_blank())


grid.arrange(Co_1, Co_2, Co_3, Co_4, ncol = 2)

