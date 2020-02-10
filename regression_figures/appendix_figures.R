###Analysis HUN-CH paper###


#Clean workspace
rm(list=ls(all=TRUE))


#Load libraries
library(dplyr)
library(gamlss)
library(ggplot2)
library(stringr)
library(data.table)
library(readxl)

####################################################################################################################
#Appendix - pooled model
#data preparation
full_data <- data.table(read_excel("C:/_swisshun/regression_figures/full_data_nov19.xlsx"))

# international treaties for HUN correction!!!
alt <- data.table(read.csv("C:/_swisshun/hun/int.csv",sep="|"))

full_data<-merge(full_data,alt,by=c("ID"),all.x = T)
count(full_data[hir==F,])
table(full_data$hir)
full_data[hir==F,international_treaty:=0]
full_data$hir<-NULL



full_data$year <- str_sub(full_data$bill_parl_start_date,1,4)

full_data <- full_data %>%
  mutate(period = ifelse(year <= 1999, 1,
                  ifelse(year >= 2000 & year <= 2004, 2,
                  ifelse(year >= 2005 & year <= 2009, 3,
                  ifelse(year >= 2010, 4,0)))))
table(full_data$period)

full_data$period <- factor(full_data$period, levels=c("2","1","3","4"))
full_data$law_type <- factor(full_data$law_type)
full_data$CAP_majortopic <- factor(full_data$CAP_majortopic)

#models

model1 <- gamlss(dissimilarity_n2 ~ 
                   electoral_year + fcparty_pole + parliament_drafter + international_treaty +
                   europeanized_bill + period + parliamentary_review_duration_days + loglength +
                   mean_n_bills_parallel_treatment + new_law + law_type + CAP_majortopic + country,
                 nu.formula = ~   electoral_year + fcparty_pole + parliament_drafter + international_treaty +
                   europeanized_bill + period + parliamentary_review_duration_days + loglength +
								 	mean_n_bills_parallel_treatment + new_law + law_type + CAP_majortopic + country,
                 family=BEZI, data= full_data)

Rsq(model1)
nobs(model1)
summary(model1)


model2 <- gamlss(dissimilarity_n2 ~ 
                   electoral_year + fcparty_pole + parliament_drafter + international_treaty +
                   europeanized_bill + period + parliamentary_review_duration_days + loglength +
								 	mean_n_bills_parallel_treatment + new_law + law_type + CAP_majortopic + country +
                   country * electoral_year + country * fcparty_pole + country * parliament_drafter + 
                   country * international_treaty + country * europeanized_bill + country * period +
                   country * parliamentary_review_duration_days + country * loglength + country * new_law +
                   country * law_type + country * CAP_majortopic+
								 	country*mean_n_bills_parallel_treatment,
                 nu.formula = ~ electoral_year + fcparty_pole + parliament_drafter + international_treaty +
                   europeanized_bill + period + parliamentary_review_duration_days + loglength +
								 	mean_n_bills_parallel_treatment + new_law + law_type + CAP_majortopic + country +
                   country * electoral_year + country * fcparty_pole + country * parliament_drafter + 
                   country * international_treaty + country * europeanized_bill + country * period +
                   country * parliamentary_review_duration_days + country * loglength + country * new_law +
                   country * law_type + country * CAP_majortopic +
								 	country*mean_n_bills_parallel_treatment,
                 family=BEZI, data= full_data)

Rsq(model2)
nobs(model2)
summary(model2)


###

set.seed(8)
n.iter <- 1000
diff <- data.frame(matrix(NA, nrow = 12, ncol = n.iter))
diff0 <- data.frame(matrix(NA, nrow = 12, ncol = n.iter))

for (i in 1:n.iter) {
  
  temp_bills <- sample_n(full_data, 4763, replace = TRUE)
  
  m1 <- gamlss(dissimilarity_n2 ~ 
                     electoral_year + fcparty_pole + parliament_drafter + international_treaty +
                     europeanized_bill + period + parliamentary_review_duration_days + loglength +
                     +mean_n_bills_parallel_treatment+new_law + law_type + CAP_majortopic + country +
                     country * electoral_year + country * fcparty_pole + country * parliament_drafter + 
                     country * international_treaty + country * europeanized_bill + country * period +
                     country * parliamentary_review_duration_days + country * loglength + country * new_law +
                     country * law_type + country * CAP_majortopic +
  						 			country * mean_n_bills_parallel_treatment,
                   nu.formula = ~ electoral_year + fcparty_pole + parliament_drafter + international_treaty +
                     europeanized_bill + period + parliamentary_review_duration_days + loglength +
  						 	mean_n_bills_parallel_treatment + new_law + law_type + CAP_majortopic + country +
                     country * electoral_year + country * fcparty_pole + country * parliament_drafter + 
                     country * international_treaty + country * europeanized_bill + country * period +
                     country * parliamentary_review_duration_days + country * loglength + country * new_law +
                     country * law_type + country * CAP_majortopic +
  								 	 country * mean_n_bills_parallel_treatment,
                   family=BEZI, data= temp_bills)
  
  #Europeanized bill - HUN
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  elect_hun0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  elect_hun0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 1, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  elect_hun1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  elect_hun1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[3,i] <- elect_hun1 - elect_hun0
  diff0[3,i] <- (1 - elect_hun1_n) - (1 - elect_hun0_n)
  
  #Europeanized bill - CH
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  elect_ch0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  elect_ch0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 1, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  elect_ch1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  elect_ch1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[4,i] <- elect_ch1 - elect_ch0
  diff0[4,i] <- (1 - elect_ch1_n) - (1 - elect_ch0_n)
  
  #electoral year - HUN
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  elect_hun0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  elect_hun0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 1, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  elect_hun1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  elect_hun1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[9,i] <- elect_hun1 - elect_hun0
  diff0[9,i] <- (1 - elect_hun1_n) - (1 - elect_hun0_n)
  
  #electoral year - CH
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  elect_ch0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  elect_ch0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 1, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  elect_ch1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  elect_ch1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[10,i] <- elect_ch1 - elect_ch0
  diff0[10,i] <- (1 - elect_ch1_n) - (1 - elect_ch0_n)
  
  
  #International treaty - HUN
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  int_hun0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  int_hun0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 1, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  int_hun1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  int_hun1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[1,i] <- int_hun1 - int_hun0
  diff0[1,i] <- (1 - int_hun1_n) - (1 - int_hun0_n)
  
  #International treaty - CH
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  int_ch0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  int_ch0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 1, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  int_ch1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  int_ch1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[2,i] <- int_ch1 - int_ch0
  diff0[2,i] <- (1 - int_ch1_n) - (1 - int_ch0_n)
  
  
  #Pole party - HUN
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  pole_hun0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  pole_hun0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 1, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  pole_hun1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  pole_hun1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[11,i] <- pole_hun1 - pole_hun0
  diff0[11,i] <- (1 - pole_hun1_n) - (1 - pole_hun0_n)
  
  #Pole party - CH
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  pole_ch0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  pole_ch0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 1, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  pole_ch1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  pole_ch1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[12,i] <- pole_ch1 - pole_ch0
  diff0[12,i] <- (1 - pole_ch1_n) - (1 - pole_ch0_n)
  
  
  
  #law type 1 vs 0 - HUN
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  type_hun0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  type_hun0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "1",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  type_hun1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  type_hun1_n <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  
  diff[5,i] <- type_hun1 - type_hun0
  diff0[5,i] <- (1 - type_hun1_n) - (1 - type_hun0_n)
  
  #law type 1 vs 0 - CH
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  type_ch0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  type_ch0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "1",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  type_ch1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  type_ch1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[6,i] <- type_ch1 - type_ch0
  diff0[6,i] <- (1 - type_ch1_n) - (1 - type_ch0_n)
  
  #law type 2 vs 0 - HUN
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  type_hun0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  type_hun0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "2",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="HU"))
  
  type_hun1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  type_hun1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[7,i] <- type_hun1 - type_hun0
  diff0[7,i] <- (1 - type_hun1_n) - (1 - type_hun0_n)
  
  #law type 2 vs 0 - CH
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "0",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  type_ch0 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  type_ch0_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  
  billspred <- with(temp_bills, data.frame(electoral_year = 0, fcparty_pole = 0, parliament_drafter = 0,
                                           international_treaty = 0, europeanized_bill = 0, period = "3",
                                           parliamentary_review_duration_days = mean(parliamentary_review_duration_days),
                                           loglength=mean(loglength),new_law = 0, law_type = "2",
                                           CAP_majortopic = 20, mean_n_bills_parallel_treatment = mean(mean_n_bills_parallel_treatment),country="CH"))
  
  type_ch1 <- predict(m1, what = c("mu"), newdata = billspred, type="response")
  type_ch1_n <- predict(m1, what = c("nu"), newdata = billspred, type="response")
  
  diff[8,i] <- type_ch1 - type_ch0
  diff0[8,i] <- (1 - type_ch1_n) - (1 - type_ch0_n)
  
  
  #loop progress
  print(paste(round(((i / n.iter)*100),digits=2), "%"))
  
  
}

preddiff  <- data.frame(matrix(NA, nrow = 12, ncol = 5))
preddiff0 <- data.frame(matrix(NA, nrow = 12, ncol = 5))

names(preddiff)[names(preddiff)=="X1"] <- "var"
names(preddiff0)[names(preddiff0)=="X1"] <- "var"

names(preddiff)[names(preddiff)=="X2"] <- "hyp"
names(preddiff0)[names(preddiff0)=="X2"] <- "hyp"

preddiff$var[1] <- paste("HUN")
preddiff$var[2] <- paste("CH")
preddiff$var[3] <- paste("HUN")
preddiff$var[4] <- paste("CH")
preddiff$var[5] <- paste("HUN")
preddiff$var[6] <- paste("CH")
preddiff$var[7] <- paste("HUN")
preddiff$var[8] <- paste("CH")
preddiff$var[9] <- paste("HUN")
preddiff$var[10] <- paste("CH")
preddiff$var[11] <- paste("HUN")
preddiff$var[12] <- paste("CH")

preddiff0$var[1] <- paste("HUN")
preddiff0$var[2] <- paste("CH")
preddiff0$var[3] <- paste("HUN")
preddiff0$var[4] <- paste("CH")
preddiff0$var[5] <- paste("HUN")
preddiff0$var[6] <- paste("CH")
preddiff0$var[7] <- paste("HUN")
preddiff0$var[8] <- paste("CH")
preddiff0$var[9] <- paste("HUN")
preddiff0$var[10] <- paste("CH")
preddiff0$var[11] <- paste("HUN")
preddiff0$var[12] <- paste("CH")


preddiff$hyp[1] <- paste("International treaty (H1)")
preddiff$hyp[2] <- paste("International treaty (H1)")
preddiff$hyp[3] <- paste("Europeanized bill (H2)")
preddiff$hyp[4] <- paste("Europeanized bill (H2)")
preddiff$hyp[5] <- paste("Direct democracy / supermajority (H3)")
preddiff$hyp[6] <- paste("Direct democracy / supermajority (H3)")
preddiff$hyp[7] <- paste("Constitutional amendment (H3)")
preddiff$hyp[8] <- paste("Constitutional amendment (H3)")
preddiff$hyp[9] <- paste("Electoral year (H5)")
preddiff$hyp[10] <- paste("Electoral year (H5)")
preddiff$hyp[11] <- paste("Pole party (H6)")
preddiff$hyp[12] <- paste("Pole party (H6)")

preddiff0$hyp[1] <- paste("International treaty (H1)")
preddiff0$hyp[2] <- paste("International treaty (H1)")
preddiff0$hyp[3] <- paste("Europeanized bill (H2)")
preddiff0$hyp[4] <- paste("Europeanized bill (H2)")
preddiff0$hyp[5] <- paste("Direct democracy / supermajority (H3)")
preddiff0$hyp[6] <- paste("Direct democracy / supermajority (H3)")
preddiff0$hyp[7] <- paste("Constitutional amendment (H3)")
preddiff0$hyp[8] <- paste("Constitutional amendment (H3)")
preddiff0$hyp[9] <- paste("Electoral year (H5)")
preddiff0$hyp[10] <- paste("Electoral year (H5)")
preddiff0$hyp[11] <- paste("Pole party (H6)")
preddiff0$hyp[12] <- paste("Pole party (H6)")


preddiff[, 3] <- apply(diff, 1, mean)
preddiff[, 4] <- apply(diff, 1, quantile, probs=c(0.025), na.rm = TRUE)
preddiff[, 5] <- apply(diff, 1, quantile, probs=c(0.975), na.rm = TRUE)

preddiff0[, 3] <- apply(diff0, 1, mean)
preddiff0[, 4] <- apply(diff0, 1, quantile, probs=c(0.025), na.rm = TRUE)
preddiff0[, 5] <- apply(diff0, 1, quantile, probs=c(0.975), na.rm = TRUE)

preddiff$var <- factor(preddiff$var, levels=c("HUN","CH"))
preddiff0$var <- factor(preddiff0$var, levels=c("HUN","CH"))





preddiff$hyp <- factor(preddiff$hyp, levels=c("Pole party (H6)",
                                              "Electoral year (H5)",
                                              "Constitutional amendment (H3)",
                                              "Direct democracy / supermajority (H3)",
                                              "Europeanized bill (H2)",
                                              "International treaty (H1)"))


preddiff0$hyp <- factor(preddiff0$hyp, levels=c("Pole party (H6)",
                                                "Electoral year (H5)",
                                                "Constitutional amendment (H3)",
                                                "Direct democracy / supermajority (H3)",
                                                "Europeanized bill (H2)",
                                                "International treaty (H1)"))

ggplot(preddiff, aes(x=hyp, y=X3,shape=var)) + geom_point(aes(shape=preddiff$var),
                                                          position = position_dodge(width = 0.6), 
                                                          size=2) +
  geom_errorbar(data=preddiff, aes(ymin=X4, ymax=X5),  width=0,position=position_dodge(width = 0.6)) +
  coord_flip() + ylab("") + xlab("") + 
  scale_y_continuous(name="Predicted difference (rate of change)",limits = c(-0.3,0.3)) +
  theme(axis.ticks = element_blank(), panel.border=element_rect(colour="black",fill=NA,size=1), 
        panel.grid.major.x = element_line(colour = "#CFCFCF"), panel.background = element_rect(fill="white"), 
        legend.title = element_blank(),  axis.title=element_text(size=10), axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10,hjust=0), legend.text = element_text(size=10),
        legend.position = "top") +
  geom_text(label = round(rev(preddiff$X3), digits = 3),
            color = "black", vjust = -0.8,size=3,position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0,color="black",linetype="dashed") + scale_colour_grey(start=0.2,end=0.2)

#likelihood of change
ggplot(preddiff0, aes(x=hyp, y=X3,shape=var)) + geom_point(aes(shape=preddiff0$var),
                                                           position = position_dodge(width = 0.6),
                                                           size=2) +
  geom_errorbar(data=preddiff0, aes(ymin=X4, ymax=X5),  width=0,position=position_dodge(width = 0.6)) +
  coord_flip() + ylab("") + xlab("") + 
  scale_y_continuous(name="Predicted difference (likelihood of change)",limits = c(-0.55,0.55)) +
  theme(axis.ticks = element_blank(), panel.border=element_rect(colour="black",fill=NA,size=1), 
        panel.grid.major.x = element_line(colour = "#CFCFCF"), panel.background = element_rect(fill="white"), 
        legend.title = element_blank(),  axis.title=element_text(size=10), axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10,hjust=0), legend.text = element_text(size=10),
        legend.position = "top") +
  geom_text(label = round(rev(preddiff0$X3), digits = 3),
            color = "black", vjust = -0.8,size=3,position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0,color="black",linetype="dashed") + scale_colour_grey(start=0.2,end=0.2)