library(tidyverse)



df <- data.table::fread("actfinal3.csv")

all_dataw <-  df %>%
  filter(race %in% "6") %>%
  mutate(nonzoonotic = scale(nonzoonotic),
         zoonotic = scale(zoonotic),
         d_score = scale(d_score))

all_datab <-  df %>%
  filter(race %in% "5") %>%
  mutate(nonzoonotic = scale(nonzoonotic),
         zoonotic = scale(zoonotic),
         d_score = scale(d_score))


all_dataw <- all_dataw %>%
  mutate(year = as.numeric(year)) %>%
  filter(year < 2025) %>%
  filter(Mn_RT_all_3467 > 400) %>%
  filter(Mn_RT_all_3467 < 10000) %>%
  mutate(confed = factor(Confederate_State),
         sex = factor(sex))  %>%
  drop_na(sex) %>%
  drop_na(edu) %>%
  drop_na(age) %>%
  drop_na(pol) %>%
  drop_na(religionid) %>%
  drop_na(d_score)

all_black <- all_datab %>%
  mutate(year = as.numeric(year)) %>%
  filter(year < 2014) %>%
  filter(Mn_RT_all_3467 > 400) %>%
  filter(Mn_RT_all_3467 < 10000) %>%
  mutate(confed = factor(Confederate_State),
         sex = factor(sex))  %>%
  drop_na(sex) %>%
  drop_na(edu) %>%
  drop_na(age) %>%
  drop_na(pol) %>%
  drop_na(religionid) %>%
  drop_na(d_score)

df2 <- readxl::read_xlsx("brian_extra.xlsx")

all_datab <- merge(all_black, df2, by = "state")
allwhite <- merge(all_dataw, df2, by = "state")


modelwhite1 <- lmerTest::lmer(brmean ~ scale(gini) + scale(med_inc) + Us_Parasite.x + sex +
                                confed + scale(Ratio_exposure_Black_White_Log)+ scale(age) + scale(edu) 
                              + scale(pol) +scale(pop_dens) + scale(religionid) +
                                nonwhite + percent + mean_rnr + (1 | state), data = allwhite)
summary(modelwhite1)