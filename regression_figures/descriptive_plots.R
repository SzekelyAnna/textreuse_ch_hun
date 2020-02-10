cat("\f")
# Descriptive plots - nov 19 version

rm(list=ls())
options(scipen = 999)
library(ggplot2)
library(gridExtra)
library(gtable)  
library(lattice)

gdir<-"C:/_swisshun/regression_figures/"
  
hun <- data.table(read_excel(paste0(gdir,"hun_bills.xlsx")))
ch <- data.table(read_csv(paste0(gdir,"Swiss_bills19_10.csv")))

hun$dissimilarity_n2 <- as.numeric(hun$dissimilarity_n2)
# Histograms
# ggplot histograms - ez a legjobb
ch_dissim <- as.data.table(ch$dissimilarity_n2_rep)
ch_dissim$country <- 'Switzerland'
hun_dissim <- as.data.table(hun$dissimilarity_n2)
hun_dissim$country <- 'Hungary'
dissim <- rbind(hun_dissim,ch_dissim)

ggplot(dissim,aes(x=V1)) + 
	geom_histogram(aes(y=..density..), colour="black", fill="white") + 
	facet_grid(~country) +
	xlab('Degree of bill change (dissimilarity index)') +
	ylab('Density')

# avg dissim by year 
hun_dissim_year <- hun[,c("dissimilarity_n2",
													"bill_parl_decision_date")]
hun_dissim_year$bill_parl_decision_date <- 
	format(as.Date(hun_dissim_year$bill_parl_decision_date, format="%Y-%m-%d"),"%Y")
agg_hun <-  aggregate(hun_dissim_year$dissimilarity_n2,
											by = list(hun_dissim_year$bill_parl_decision_date),
											FUN = mean)

swiss_dissim_year <- ch[,c("dissimilarity_n2_rep",
																	 "bill_parl_decision_date")]
swiss_dissim_year$bill_parl_decision_date <- 
	format(as.Date(swiss_dissim_year$bill_parl_decision_date, format="%Y-%m-%d"),"%Y")
agg_swiss <-  aggregate(swiss_dissim_year$dissimilarity_n2_rep,
												by = list(swiss_dissim_year$bill_parl_decision_date),
												FUN = mean)
agg_swiss <- agg_swiss[2:21,]

agg_hun$country <- 'Hungary'
agg_swiss$country <- 'Switzerland'
agg_dissim <- rbind(agg_hun,agg_swiss)
ggplot(data = agg_dissim, aes(x=Group.1,y = x,group = 1))+
	facet_grid(~country) +
	geom_line() +
	xlab("Year") +
	ylab("Average dissimilarity")+
	scale_x_discrete(breaks=scales::pretty_breaks(n=5))

mean(hun_dissim$V1)
median(hun_dissim$V1)
sd(hun_dissim$V1)
sum(hun_dissim$V1==0)

mean(ch_dissim$V1)
median(ch_dissim$V1)
sd(ch_dissim$V1)
