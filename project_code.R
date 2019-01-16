library(dplyr)
library(survival)
library(ggpubr)
library(magrittr)
library(survminer)


# tracker data 
tracker <- read.csv("tracker_file.csv")


tracker <- tracker %>% 
  select(HHID, PN, NYEAR, NMONTH, NSCORE, EFTFASSIGN, BIRTHMO, BIRTHYR, RACE, DEGREE, STRATUM, GENDER) %>% 
  filter(EFTFASSIGN %in% c(1, 2)) %>% 
  mutate(older65=ifelse(BIRTHYR<=1952, 1, 0)) %>% 
  filter(older65==1)

wave_06_10_14 <- tracker %>% 
  filter(EFTFASSIGN==1) %>% 
  select(HHID, PN)

wave_08_12 <- tracker %>% 
  filter(EFTFASSIGN==2) %>% 
  select(HHID, PN)


# filtering out RACE for analysis 
other_race <- tracker %>% 
  filter(!RACE %in% c(0, 1, 2))

# Wave 2014
h14i_r <- read.csv("h14i_r.csv")
h14i_r <- h14i_r %>% select(HHID, PN, OI816, OI851, OI852, OI853, OI834, OI841)

# fix Gripstrength column
h14i_r$OI816 <- ifelse(h14i_r$OI816==993, 0,
                       ifelse(h14i_r$OI816==998, NA, 
                              ifelse(h14i_r$OI816==999, NA, h14i_r$OI816)))
  
h14i_r$OI851 <- ifelse(h14i_r$OI851==993, 0,
                       ifelse(h14i_r$OI851==998, NA, 
                              ifelse(h14i_r$OI851==999, NA, h14i_r$OI851)))

h14i_r$OI852 <- ifelse(h14i_r$OI852==993, 0,
                       ifelse(h14i_r$OI852==998, NA, 
                              ifelse(h14i_r$OI852==999, NA, h14i_r$OI852)))

h14i_r$OI853 <- ifelse(h14i_r$OI853==993, 0,
                       ifelse(h14i_r$OI853==998, NA, 
                              ifelse(h14i_r$OI853==999, NA, h14i_r$OI853)))

# calculate average 
h14i_r <- h14i_r %>%
  group_by(HHID, PN) %>%
  mutate(OHANDGRIPS=mean(c(OI816, OI851, OI852, OI853), na.rm = TRUE))

# remove all missing values 
h14i_r <- h14i_r %>% select(HHID, PN, OI834, OI841, OHANDGRIPS) %>% 
  filter(!is.na(OHANDGRIPS))

# Wave 2012: read and select columns  
h12i_r <- read.csv("h12i_r.csv")
h12i_r <- h12i_r %>% select(HHID, PN, NI816, NI851, NI852, NI853, NI834, NI841)

# fix Gripstrength column
h12i_r$NI816 <- ifelse(h12i_r$NI816==993, 0,
                       ifelse(h12i_r$NI816==998, NA, 
                              ifelse(h12i_r$NI816==999, NA, h12i_r$NI816)))

h12i_r$NI851 <- ifelse(h12i_r$NI851==993, 0,
                       ifelse(h12i_r$NI851==998, NA, 
                              ifelse(h12i_r$NI851==999, NA, h12i_r$NI851)))

h12i_r$NI852 <- ifelse(h12i_r$NI852==993, 0,
                       ifelse(h12i_r$NI852==998, NA, 
                              ifelse(h12i_r$NI852==999, NA, h12i_r$NI852)))

h12i_r$NI853 <- ifelse(h12i_r$NI853==993, 0,
                       ifelse(h12i_r$NI853==998, NA, 
                              ifelse(h12i_r$NI853==999, NA, h12i_r$NI853)))

h12i_r <- h12i_r %>% 
  group_by(HHID, PN) %>%
  mutate(NHANDGRIPS=mean(c(NI816, NI851, NI852, NI853), na.rm = TRUE))

h12i_r <- h12i_r %>% 
  select(HHID, PN, NI834, NI841, NHANDGRIPS) %>% 
  filter(!is.na(NHANDGRIPS))

# Wave 2010: read and select columns 
h10i_r <- read.csv("h10i_r.csv")
h10i_r <- h10i_r %>% select(HHID, PN, MI816, MI851, MI852, MI853, MI834, MI841)

# fix Gripstrength column
h10i_r$MI816 <- ifelse(h10i_r$MI816==993, 0,
                       ifelse(h10i_r$MI816==998, NA, 
                              ifelse(h10i_r$MI816==999, NA, h10i_r$MI816)))

h10i_r$MI851 <- ifelse(h10i_r$MI851==993, 0,
                       ifelse(h10i_r$MI851==998, NA, 
                              ifelse(h10i_r$MI851==999, NA, h10i_r$MI851)))

h10i_r$MI852 <- ifelse(h10i_r$MI852==993, 0,
                       ifelse(h10i_r$MI852==998, NA, 
                              ifelse(h10i_r$MI852==999, NA, h10i_r$MI852)))

h10i_r$MI853 <- ifelse(h10i_r$MI853==993, 0,
                       ifelse(h10i_r$MI853==998, NA, 
                              ifelse(h10i_r$MI853==999, NA, h10i_r$MI853)))

h10i_r <- h10i_r %>% 
  group_by(HHID, PN) %>%
  mutate(MHANDGRIPS=mean(c(MI816, MI851, MI852, MI853), na.rm = TRUE))

h10i_r <- h10i_r %>% select(HHID, PN, MI834, MI841, MHANDGRIPS) %>% 
  filter(!is.na(MHANDGRIPS))

# Wave 2008: read and select columns  
h08i_r <- read.csv("h08i_r.csv")
h08i_r <- h08i_r %>% select(HHID, PN, LI816, LI851, LI852, LI853, LI834, LI841)

# Fix Gripstrength column
h08i_r$LI816 <- ifelse(h08i_r$LI816==993, 0,
                       ifelse(h08i_r$LI816==998, NA, 
                              ifelse(h08i_r$LI816==999, NA, h08i_r$LI816)))

h08i_r$LI851 <- ifelse(h08i_r$LI851==993, 0,
                       ifelse(h08i_r$LI851==998, NA, 
                              ifelse(h08i_r$LI851==999, NA, h08i_r$LI851)))

h08i_r$LI852 <- ifelse(h08i_r$LI852==993, 0,
                       ifelse(h08i_r$LI852==998, NA, 
                              ifelse(h08i_r$LI852==999, NA, h08i_r$LI852)))

h08i_r$LI853 <- ifelse(h08i_r$LI853==993, 0,
                       ifelse(h08i_r$LI853==998, NA, 
                              ifelse(h08i_r$LI853==999, NA, h08i_r$LI853)))

h08i_r <- h08i_r %>% 
  group_by(HHID, PN) %>%
  mutate(LHANDGRIPS=mean(c(LI816, LI851, LI852, LI853), na.rm = TRUE))

h08i_r <- h08i_r %>% select(HHID, PN, LI834, LI841, LHANDGRIPS) %>% 
  filter(!is.na(LHANDGRIPS))

# Wave 2006: read and select columns  
h06i_r <- read.csv("h06i_r.csv")
h06i_r <- h06i_r %>% select(HHID, PN, KI816, KI851, KI852, KI853, KI834, KI841)


# fix Gripstrength column
h06i_r$KI816 <- ifelse(h06i_r$KI816==993, 0,
                       ifelse(h06i_r$KI816==998, NA, 
                              ifelse(h06i_r$KI816==999, NA, h06i_r$KI816)))

h06i_r$KI851 <- ifelse(h06i_r$KI851==993, 0,
                       ifelse(h06i_r$KI851==998, NA, 
                              ifelse(h06i_r$KI851==999, NA, h06i_r$KI851)))

h06i_r$KI852 <- ifelse(h06i_r$KI852==993, 0,
                       ifelse(h06i_r$KI852==998, NA, 
                              ifelse(h06i_r$KI852==999, NA, h06i_r$KI852)))

h06i_r$KI853 <- ifelse(h06i_r$KI853==993, 0,
                       ifelse(h06i_r$KI853==998, NA, 
                              ifelse(h06i_r$KI853==999, NA, h06i_r$KI853)))

h06i_r <- h06i_r %>% 
  group_by(HHID, PN) %>%
  mutate(KHANDGRIPS=mean(c(KI816, KI851, KI852, KI853), na.rm = TRUE))

h06i_r <- h06i_r %>% select(HHID, PN, KI834, KI841, KHANDGRIPS) %>%
  filter(!is.na(KHANDGRIPS))

# Creating time1 and time2 variables 
h06i_r$time1 <- 2006
h06i_r$time2 <- 2010

h08i_r$time1 <- 2008
h08i_r$time2 <- 2012

h10i_r$time1 <- 2010
h10i_r$time2 <- 2014

h12i_r$time1 <- 2012
h12i_r$time2 <- 2014

h14i_r$time1 <- 2014
h14i_r$time2 <- 2014

# subset tracker so we can merge with other data sets 
tracker2 <- tracker %>% 
  select(HHID, PN, NYEAR, NMONTH, BIRTHMO, BIRTHYR, RACE, DEGREE, STRATUM, GENDER, older65) %>% 
  mutate(AGE=2018-BIRTHYR)

############# 2006 
# merge tracker2 with other years 
h06i_r <- inner_join(h06i_r, tracker2, by=c("HHID", "PN"))

# create event variable using NYEAR death: 1 means died and 0 means alive 
h06i_r_2 <- h06i_r %>% 
  filter(NYEAR<2006 & NYEAR>0)

h06i_r$event <- ifelse((h06i_r$NYEAR <= h06i_r$time2) & (h06i_r$NYEAR >= h06i_r$time1), 1, 0)

# create weak and non-weak category 
h06i_r <- h06i_r %>% 
  mutate(gripst=ifelse(GENDER==1 & RACE ==1 & KHANDGRIPS<35, 1, 
                       ifelse(GENDER==1 & RACE ==2 & KHANDGRIPS<40, 1, 
                              ifelse(GENDER==2 & RACE ==1 & KHANDGRIPS<22, 1,
                                     ifelse(GENDER==2 & RACE ==2 & KHANDGRIPS<31, 1, 0)))))

names(h06i_r) <- c("HHID","PN", "HEIGHT", "WEIGHT", "HANDGRIPS", "TIME1", "TIME2", 
                   "NYEAR", "NMONTH", "BIRTHMO", "BIRTHYR", "RACE", "DEGREE", "STRATUM", 
                   "GENDER", "OLDER65", "AGE", "EVENT", "GRIPST")

h06i_r$TIME3 <- ifelse(h06i_r$NYEAR < h06i_r$TIME2 & h06i_r$NYEAR >= h06i_r$TIME1, 
                       h06i_r$NYEAR, h06i_r$TIME2)

h06i_r <- h06i_r %>% 
  arrange(HHID, PN)

h06i_r <- h06i_r %>% 
  arrange(HHID, PN) %>% 
  mutate(TIME4=ifelse(TIME1==2006, 0, ifelse(TIME1==2008, 24, 
                                             ifelse(TIME1==2010, 48, 
                                                    ifelse(TIME1==2012, 72,
                                                           ifelse(TIME1==2014, 95, NA))))),
         TIME5=ifelse((TIME3-2006)!=4, (TIME3-2006)*12+NMONTH, 48))


############## 2008 

# merge tracker2 with other years 
h08i_r <- inner_join(h08i_r, tracker2, by=c("HHID", "PN"))

# create event variable using NYEAR death: 1 means died and 0 means alive 
h08i_r$event <- ifelse((h08i_r$NYEAR <= h08i_r$time2) & (h08i_r$NYEAR >= h08i_r$time1), 1, 0)

# create weak and non-weak category 
h08i_r <- h08i_r %>% 
  mutate(gripst=ifelse(GENDER==1 & RACE ==1 & LHANDGRIPS<35, 1, 
                       ifelse(GENDER==1 & RACE ==2 & LHANDGRIPS<40, 1, 
                              ifelse(GENDER==2 & RACE ==1 & LHANDGRIPS<22, 1,
                                     ifelse(GENDER==2 & RACE ==2 & LHANDGRIPS<31, 1, 0)))))

names(h08i_r) <- c("HHID","PN", "HEIGHT", "WEIGHT", "HANDGRIPS", "TIME1", "TIME2", 
                   "NYEAR", "NMONTH", "BIRTHMO", "BIRTHYR", "RACE", "DEGREE", "STRATUM", 
                   "GENDER", "OLDER65", "AGE", "EVENT", "GRIPST")

h08i_r$TIME3 <- ifelse(h08i_r$NYEAR < h08i_r$TIME2 & h08i_r$NYEAR >= h08i_r$TIME1, 
                       h08i_r$NYEAR, h08i_r$TIME2)

# h08i_r_2 <- h08i_r %>% 
#   filter(NYEAR<2008 & NYEAR>0)

h08i_r <- h08i_r %>% 
  arrange(HHID, PN) %>% 
  mutate(TIME4=ifelse(TIME1==2006, 0, ifelse(TIME1==2008, 24, 
                                             ifelse(TIME1==2010, 48, 
                                                    ifelse(TIME1==2012, 72,
                                                           ifelse(TIME1==2014, 95, NA))))),
         TIME5=ifelse((TIME3-2006)!=6, (TIME3-2006)*12+NMONTH, 72))


############## 2010 

# merge tracker2 with other years 
h10i_r <- inner_join(h10i_r, tracker2, by=c("HHID", "PN"))

# create event variable using NYEAR death: 1 means died and 0 means alive 
h10i_r$event <- ifelse((h10i_r$NYEAR <= h10i_r$time2) & (h10i_r$NYEAR >= h10i_r$time1), 1, 0)

# create weak and non-weak category 
h10i_r <- h10i_r %>% 
  mutate(gripst=ifelse(GENDER==1 & RACE ==1 & MHANDGRIPS<35, 1, 
                       ifelse(GENDER==1 & RACE ==2 & MHANDGRIPS<40, 1, 
                              ifelse(GENDER==2 & RACE ==1 & MHANDGRIPS<22, 1,
                                     ifelse(GENDER==2 & RACE ==2 & MHANDGRIPS<31, 1, 0)))))

names(h10i_r) <- c("HHID","PN", "HEIGHT", "WEIGHT", "HANDGRIPS", "TIME1", "TIME2", 
                   "NYEAR", "NMONTH", "BIRTHMO", "BIRTHYR", "RACE", "DEGREE", "STRATUM", 
                   "GENDER", "OLDER65", "AGE", "EVENT", "GRIPST")

h10i_r$TIME3 <- ifelse(h10i_r$NYEAR < h10i_r$TIME2 & h10i_r$NYEAR >= h10i_r$TIME1, h10i_r$NYEAR, h10i_r$TIME2)

# h10i_r_2 <- h10i_r %>% 
#    filter(NYEAR<2010 & NYEAR>0)

h10i_r <- h10i_r %>% 
  arrange(HHID, PN) %>% 
  mutate(TIME4=ifelse(TIME1==2006, 0, ifelse(TIME1==2008, 24, 
                                             ifelse(TIME1==2010, 48, 
                                                    ifelse(TIME1==2012, 72,
                                                           ifelse(TIME1==2014, 95, NA))))),
         TIME5=ifelse((TIME3-2006)!=8, (TIME3-2006)*12+NMONTH, 95))

############## 2012 

# merge tracker2 with other years 
h12i_r <- inner_join(h12i_r, tracker2, by=c("HHID", "PN"))

# create event variable using NYEAR death: 1 means died and 0 means alive 
h12i_r$event <- ifelse((h12i_r$NYEAR <= h12i_r$time2) & (h12i_r$NYEAR >= h12i_r$time1), 1, 0)

# create weak and non-weak category 
h12i_r <- h12i_r %>% 
  mutate(gripst=ifelse(GENDER==1 & RACE ==1 & NHANDGRIPS<35, 1, 
                       ifelse(GENDER==1 & RACE ==2 & NHANDGRIPS<40, 1, 
                              ifelse(GENDER==2 & RACE ==1 & NHANDGRIPS<22, 1,
                                     ifelse(GENDER==2 & RACE ==2 & NHANDGRIPS<31, 1, 0)))))

names(h12i_r) <- c("HHID","PN", "HEIGHT", "WEIGHT", "HANDGRIPS", "TIME1", "TIME2", 
                   "NYEAR", "NMONTH", "BIRTHMO", "BIRTHYR", "RACE", "DEGREE", "STRATUM", 
                   "GENDER", "OLDER65", "AGE", "EVENT", "GRIPST")

h12i_r$TIME3 <- ifelse(h12i_r$NYEAR < h12i_r$TIME2 & h12i_r$NYEAR >= h12i_r$TIME1, h12i_r$NYEAR, h12i_r$TIME2)

h12i_r_2 <- h12i_r %>% 
   filter(NYEAR==2014 & NYEAR>0)

h12i_r <- h12i_r %>% 
  arrange(HHID, PN) %>% 
  mutate(TIME4=ifelse(TIME1==2006, 0, ifelse(TIME1==2008, 24, 
                                             ifelse(TIME1==2010, 48, 
                                                    ifelse(TIME1==2012, 72,
                                                           ifelse(TIME1==2014, 95, NA))))),
         TIME5=ifelse((TIME3-2006)!=8, (TIME3-2006)*12+NMONTH, 95))

################# 2014
# merge tracker2 with other years 
h14i_r <- inner_join(h14i_r, tracker2, by=c("HHID", "PN"))

# create event variable using NYEAR death: 1 means died and 0 means alive 
h14i_r$event <- ifelse((h14i_r$NYEAR <= h14i_r$time2) & (h14i_r$NYEAR >= h14i_r$time1), 1, 0)

# create weak and non-weak category 
h14i_r <- h14i_r %>% 
  mutate(gripst=ifelse(GENDER==1 & RACE ==1 & OHANDGRIPS<35, 1, 
                       ifelse(GENDER==1 & RACE ==2 & OHANDGRIPS<40, 1, 
                              ifelse(GENDER==2 & RACE ==1 & OHANDGRIPS<22, 1,
                                     ifelse(GENDER==2 & RACE ==2 & OHANDGRIPS<31, 1, 0)))))


names(h14i_r) <- c("HHID","PN", "HEIGHT", "WEIGHT", "HANDGRIPS", "TIME1", "TIME2", 
                   "NYEAR", "NMONTH", "BIRTHMO", "BIRTHYR", "RACE", "DEGREE", "STRATUM", 
                   "GENDER", "OLDER65", "AGE", "EVENT", "GRIPST")

h14i_r$TIME3 <- ifelse(h14i_r$NYEAR < h14i_r$TIME2 & h14i_r$NYEAR >= h14i_r$TIME1, h14i_r$NYEAR, h14i_r$TIME2)

h14i_r_2 <- h14i_r %>% 
  filter(NYEAR==2014)

h14i_r <- h14i_r %>% 
  arrange(HHID, PN) %>% 
  mutate(TIME4=ifelse(TIME1==2006, 0, ifelse(TIME1==2008, 24, 
                                             ifelse(TIME1==2010, 48, 
                                                    ifelse(TIME1==2012, 72,
                                                           ifelse(TIME1==2014, 95, NA))))),
         TIME5=(TIME3-2006)*12+NMONTH)


full.data <- rbind(h06i_r, h08i_r, h10i_r, h12i_r, h14i_r)

# full.data2 <- full.data %>% 
#   arrange(HHID, PN) %>% 
#   mutate(TIME4=ifelse(TIME1==2006, 0, ifelse(TIME1==2008, 24, 
#                                              ifelse(TIME1==2010, 48, 
#                                                     ifelse(TIME1==2012, 72,
#                                                            ifelse(TIME1==2014, 95, NA))))),
#          TIME5=(TIME3-2006)*12+NMONTH)


full.data3 <- full.data %>% 
  arrange(HHID, PN) %>% 
  mutate(EVENT1=ifelse(TIME5==96, 1, EVENT)) %>% 
  arrange(HHID, PN, TIME4)

# test <- full.data2 %>% 

#Calculate BMI score (convert from inches and pouds to meter and kgs) 
full.data3 <- full.data3 %>% 
  mutate(WEIGHT2=WEIGHT*0.45, 
         HEIGHT2=(HEIGHT*0.025)^2, 
         BMI=WEIGHT2/HEIGHT2)

# make a factor 
full.data3$EVENT1 <- as.factor(full.data3$EVENT1)

full.data3$handgrip <- factor(full.data3$GRIPST, labels = c("Non-Weak", "Weak"))

# create surv object
surv.obj2 <- Surv(time =full.data3$TIME4, time2=full.data3$TIME5, full.data3$EVENT, origin =0)

# fit the model 
fit2 <- survfit(surv.obj2 ~ handgrip, data = full.data3)
summary(fit2)

ggsurvplot(fit2, data = full.data3, tables.theme = theme_cleantable(), pval = T)

logrank <- survdiff(surv.obj2 ~ full.data3$handgrip, rho=0)

comp(ten(fit2))

#------------------------------------- Cox unadjusted model ---------------------------
full.data3 <- full.data3 %>% 
  filter(RACE %in% c(1, 2))


# Model 1 
# Cox regression model 
# unadjusted model 
fit4 <- coxph(surv.obj3 ~ handgrip, data = full.data3)
summary(fit4)
exp(confint(fit4, level = 0.95))

# Model 2 
# Cox regression model 
# adjusted for covariates 

full.data3$EDU <- ifelse(full.data3$DEGREE==1, "GED",
                         ifelse(full.data3$DEGREE==2, "High School", 
                                ifelse(full.data3$DEGREE %in% c(3, 9), "Some college", 
                                       ifelse(full.data3$DEGREE %in% c(4, 5, 6), "College and above", NA)))) 
full.data3$EDU <- factor(full.data3$EDU) #, labels = c("GED", "High School", "Some college", "College and above"))
summary(full.data3)

surv.obj3 <- Surv(time =full.data3$TIME4, time2=full.data3$TIME5, full.data3$EVENT, origin =0)
fit5 <- coxph(surv.obj3 ~ handgrip+AGE+factor(GENDER)+factor(EDU)+factor(RACE)+BMI, data = full.data3)

summary(fit5)

plot(surv.obj)

library(survMisc)

summary(fit1)
ggsurvplot(fit1, data = full.data2)



# --------------------------------------- New Model 1 -----------------------------# 
h06i_r_2 <- h06i_r %>% 
  mutate(WEIGHT2=WEIGHT*0.45, 
         HEIGHT2=(HEIGHT*0.025)^2, 
         BMI=WEIGHT2/HEIGHT2)


h06i_r_2$EDU <- ifelse(h06i_r_2$DEGREE==1, "GED",
                         ifelse(h06i_r_2$DEGREE==2, "High School", 
                                ifelse(h06i_r_2$DEGREE %in% c(3, 9), "Some college", 
                                       ifelse(h06i_r_2$DEGREE %in% c(4, 5, 6), "College and above", 0)))) 

h06i_r_2$EDU <- factor(h06i_r_2$EDU) #, labels = c("GED", "High School", "Some college", "College and above"))




surv.obj4 <- Surv(time =h06i_r_2$TIME4, time2=h06i_r_2$TIME5, h06i_r_2$EVENT, origin =0)

fit6 <- coxph(surv.obj4 ~ GRIPST+AGE+factor(GENDER)+factor(RACE)+BMI+EDU, data =h06i_r_2)

summary(fit6)

# --------------------------------------- New Model 2 -----------------------------# 

surv.obj7 <- Surv(time =full.data3$TIME4, time2=full.data3$TIME5, full.data3$EVENT, origin =0)

fit7 <- coxph(surv.obj7 ~ HANDGRIPS+AGE+factor(GENDER)+factor(RACE)+BMI+EDU, data =full.data3)


summary(fit7)




# -------------------------------------- Baseline Comparison Plot --------------------------#

# BMI 
#Calculate BMI score (convert from inches and pouds to meter and kgs) 

ttest_data <- rbind(h06i_r)

ttest_data$HEIGHT1 <- ifelse(ttest_data$HEIGHT>=80, NA, ttest_data$HEIGHT)
ttest_data$WEIGHT1 <- ifelse(ttest_data$WEIGHT>=334, NA, ttest_data$WEIGHT)

ttest_data <- ttest_data %>%
  mutate(WEIGHT2=WEIGHT1*0.45, 
         HEIGHT2=(HEIGHT1*0.025)^2, 
         BMI=WEIGHT2/HEIGHT2)


BMI1 <- ttest_data %>% 
  ungroup() %>% 
  filter(GRIPST==1) %>% 
  distinct(HHID, PN, .keep_all = TRUE) %>% 
  select(BMI)
  
BMI0 <- ttest_data %>% 
  ungroup() %>% 
  filter(GRIPST==0) %>% 
  distinct(HHID, PN, .keep_all = TRUE) %>%
  select(BMI)

# t-test 
t2 <- t.test(BMI1$BMI, BMI0$BMI)
t2

# AGE 
AGE1 <- ttest_data %>% 
  ungroup() %>% 
  filter(GRIPST==1) %>%
  distinct(HHID, PN, .keep_all = TRUE) %>%
  select(AGE)

AGE0 <- ttest_data %>% 
  ungroup() %>% 
  filter(GRIPST==0) %>%
  distinct(HHID, PN, .keep_all = TRUE) %>%
  select(AGE)

# t-test 
t1 <- t.test(AGE1$AGE, AGE0$AGE)
t1

# Degree

# 10041           0.  No degree
# 1763            1.  GED
# 16926           2.  High school diploma
# 1645            3.  Two year college degree
# 4100            4.  Four year college degree
# 2059            5.  Master degree
# 717             6.  Professional degree (Ph.D., M.D., J.D.)
# 932             9.  Degree unknown/Some College





test <- full.data3 %>% 
  select(HHID, PN, TIME4, TIME5, handgrip, AGE, BMI) %>% 
  arrange(HHID, PN, TIME4)


#write.csv(tracker, "tracker_file.csv")
#load("last_work.RData")
save.image("last_work_060007.RData")
