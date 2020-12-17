# switchboard

# Packages
list.of.packages <- c("tidyverse","readxl","writexl","ggthemes","ggExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# useful functions
`%notin%` <- function(x,y) !(x %in% y) 
mean.na <- function(x) mean(x, na.rm = T)
sd.na <- function(x) sd(x, na.rm = T)

# load data
d.raw <- read.csv("transitions.txt",sep="\t") %>%
  rename(fto = dur) 

# some cleaning up and enrichment
d <- d.raw %>%
  dplyr::select(-c(sexA,dobA,dialA,sexB,dobB,dialB),-starts_with(c("last","first"))) %>%
  mutate(speakeruid = paste(file,spkB,sep="_"),
         priorspeaker = ifelse(spkB == lag(spkB),"self","other"),
         dup = as.factor(ifelse(uttB == lag(uttB),1,0))) %>%
  drop_na(priorspeaker,dup)

# add turn tumbers and roles within convos
Aspeakers <- d %>% dplyr::select(file,speakeruid) %>% group_by(file) %>% slice(1)
d <- d %>%
  group_by(file) %>%
  mutate(turn = row_number()) %>%
  mutate(role = ifelse(speakeruid %in% Aspeakers$speakeruid,"A","B"))

ds <- d %>%
  dplyr::select(-c(spkA,dialActA,dialActDurA,turnDurA,uttA,uttStartA,uttEndA,uttDurA,uttNPhonA,uttNSylA,uttPosA))

write_xlsx(d,path="transitions.xlsx")
write_xlsx(ds,path="transitions_slimmed.xlsx")

# some quick counts and plots

d %>%
  group_by(priorspeaker) %>%
  summarise(fto=mean.na(fto),count=n())


d %>%
  group_by(file,priorspeaker) %>%
  summarise(fto=mean.na(fto),count=n())


ggplot(data=d,aes(x=fto,colour=priorspeaker)) +
  theme_tufte() + xlim(c(-2200,2200)) +
  ggtitle("FTO by speaker of prior turn: other (14.883) vs self (6.206)") +
  geom_density() +
  NULL
ggsave("FTO_bypriorspeaker.png",width=5,height=5)

d %>% 
  group_by(dup) %>% 
  summarise(fto=mean.na(fto),count=n())

ggplot(data=d,aes(x=fto,colour=dup)) +
  theme_tufte() + xlim(c(-2200,2200)) +
  ggtitle("FTO of unique turns (18.714) vs duplicates (2.375)") +
  geom_density() + 
  NULL
ggsave("FTO_dupvsunique.png",width=5,height=5)


d.dedup <- d %>% 
  filter(dup==0)

d.dedup %>%
  group_by(priorspeaker) %>%
  summarise(fto=mean.na(fto),count=n())

ggplot(data=d.dedup,aes(x=fto,colour=priorspeaker)) +
  theme_tufte() + xlim(c(-2200,2200)) +
  ggtitle("FTO by speaker of prior turn: other (14.789) vs self (3.925)") +
  geom_density() + 
  NULL
ggsave("FTO_bypriorspeaker_deduplicated.png",width=5,height=5)


# looking at turn-level 

test <- d %>% filter(file %in% c("sw3188.eaf"))

ggplot(data=test,aes(x=turn,y=fto)) +
  theme_tufte() + ylim(c(-1200,1200)) + xlim(c(1,max(test$turn))) +
  theme(legend.position="bottom") +
  geom_line() +
  geom_point(shape=21,size=3,aes(colour=role,fill=role)) +
  NULL
ggMarginal(last_plot(), type="density",margins='y',groupColour=T,groupFill = T)
ggsave("FTO_by_conversation_by_role.png",width=8,height=6) 

ggplot(data=test,aes(x=turn,y=fto)) +
  theme_tufte() + ylim(c(-1200,1200)) + xlim(c(1,max(test$turn))) +
  theme(legend.position="bottom") +
  geom_line() +
  geom_point(shape=21,size=3,aes(colour=priorspeaker,fill=priorspeaker)) +
  NULL
ggMarginal(last_plot(),type="density",margins='y',groupColour=T,groupFill = T)

# so what if we create a cleaner version of d and look at lag

dc <- d %>%
  filter(priorspeaker=="other",
         dup==0)

test <- dc %>% filter(file %in% c("sw3188.eaf"))


ggplot(data=test,aes(x=turn,y=fto)) +
  theme_tufte() + ylim(c(-1200,1200)) + xlim(c(1,max(test$turn))) +
  theme(legend.position="bottom") +
  geom_line() +
  geom_point(shape=21,size=3,aes(colour=role,fill=role)) +
  NULL
ggMarginal(last_plot(),type="density",margins='y',groupColour=T,groupFill = T)
