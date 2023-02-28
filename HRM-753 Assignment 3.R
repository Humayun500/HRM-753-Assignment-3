save.image("C:/Users/humay/Dropbox/HRM/HRM 753/Assignments/Assignment data/HRM753_InfluenzaData.RData")

library(tidyverse)
library(dplyr)
library(freqtables)
options (scipen=999)


###Variable description###
#Seasonal influenza A/H1N1 antibody response = postvax.sH1
#log transformation of postvax.sH1 = log.postvax.sH1
#pre-vaccination influenza A/H1N1 antibody response = prevax.sH1
#natural log transformation of prevax.sH1= natural.prevax.sH1 
#2 consecutive seasonal vaccinations (2008/09 and 2009/10)= cons.vacc.8.9.10


#data management 
#rename male to sex
HRM753_InfluenzaData= HRM753_InfluenzaData %>% 
  rename (sex = male)
#natural log transform 
sum(is.na (HRM753_InfluenzaData$prevax.sH1))

HRM753_InfluenzaData$natural.prevax.sH1 = log (HRM753_InfluenzaData$prevax.sH1)
sum(is.na (HRM753_InfluenzaData$natural.prevax.sH1))

#create cons.vacc.8.9.10
HRM753_InfluenzaData$cons.vacc080910 = 
  case_when(
    HRM753_InfluenzaData$vac0809 == "1" &  
    (HRM753_InfluenzaData$vacph10910.mid == "1" | 
    HRM753_InfluenzaData$intervention == "TIV") ~ "1",
    HRM753_InfluenzaData$vac0809 == "0" &
    (HRM753_InfluenzaData$vacph10910.mid == "0"|
    HRM753_InfluenzaData$intervention == "placebo") ~ "0"
  )

HRM753_InfluenzaData$cons.vacc080910= as.factor(HRM753_InfluenzaData$cons.vacc080910)
HRM753_InfluenzaData$cons.vacc080910
sum (is.na(HRM753_InfluenzaData$cons.vacc080910))

#Question 1
#a
HRM753_InfluenzaData$consecutive.seasonal.vaccinations

lm.log.postvax.sH1.multiple.1= 
  lm (log.postvax.sH1~
        cons.vacc080910+
        age+
        sex, 
      data=HRM753_InfluenzaData)

summary (lm.log.postvax.sH1.multiple.1)

confint(lm.log.postvax.sH1.multiple.1, 
        level=0.95)

#b
lm.log.postvax.sH1.multiple.2= 
  lm (log.postvax.sH1~
        cons.vacc080910+
        age+
        sex+
        natural.prevax.sH1, 
      data=HRM753_InfluenzaData)

summary (lm.log.postvax.sH1.multiple.2)

confint(lm.log.postvax.sH1.multiple.2, 
        level=0.95)

anova (lm.log.postvax.sH1.multiple.2)


#c.partial F-test
#H0= models don't differ significantly
#Ha= models differ significantly


#3: multi-collinearity

#VIF

library("olsrr")

ols_vif_tol(lm.log.postvax.sH1.multiple.2)

ols_eigen_cindex(lm.log.postvax.sH1.multiple.2)

#4

lm.log.postvax.sH1.multiple.2.cooks<- cooks.distance((lm.log.postvax.sH1.multiple.2))
lm.log.postvax.sH1.multiple.2.cooks
ols_plot_cooksd_chart(lm.log.postvax.sH1.multiple.2)

ols_plot_cooksd_bar(lm.log.postvax.sH1.multiple.2)

ols_plot_dfbetas (lm.log.postvax.sH1.multiple.2)
ols_plot_dffits(lm.log.postvax.sH1.multiple.2)

ols_plot_resid_pot(lm.log.postvax.sH1.multiple.2)

