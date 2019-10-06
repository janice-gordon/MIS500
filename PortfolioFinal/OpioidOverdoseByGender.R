##############################################################################
# 
# MIS500 Foundations of Data Analytics
# Portfolio Project Option #1
# Janice Gordon
# October 5, 2019
# Data Citation: 
#   Centers for Disease Control and Prevention, National Center for Health Statistics. 
#   Multiple Cause of Death 1999-2017 on CDC WONDER Online Database, released December, 2018. 
#   Data are from the Multiple Cause of Death Files, 1999-2017, as compiled from data provided
#   by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program. 
#   Accessed at http://wonder.cdc.gov/mcd-icd10.html on Oct 5, 2019.
# Received Help from this tutorial: 
#   http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r
# 
##############################################################################
## double check which libraries need to be loaded##
# load libraries
library(lubridate)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(e1071)

# import and clean data
oo_gender <- read.table("data/MCD_ALL_YR_GE_1999-2017.txt", skip = 1, nrows=56, sep="\t", fill=TRUE, col.names=c("Notes","Year", "Year.Code","Gender","Gender.Code","Deaths", "Population", "Crude.Rate"))
oo_gender <- subset(oo_gender, Notes != "Total") #remove rows with totals
oo_gender <- oo_gender[,-1] #Remove empty Notes column
oo_gender$Year <- make_date(oo_gender$Year, 12, 31) #convert to date format, last day of year
oo_gender$Year.Code <- as.character(oo_gender$Year.Code) #conver to char
oo_gender$Gender <- droplevels(oo_gender$Gender) # drop blank factor level
oo_gender$Gender.Code <- as.character(oo_gender$Gender.Code)

# subset data by gender
oo_male <- select(filter(oo_gender,oo_gender$Gender.Code=="M"), c("Year","Year.Code","Gender","Gender.Code","Deaths","Population","Crude.Rate"))
oo_female <- select(filter(oo_gender,oo_gender$Gender.Code=="F"), c("Year","Year.Code","Gender","Gender.Code","Deaths", "Population", "Crude.Rate"))

# summary statistics for male
oo_male_sumstats_rate <- summary(oo_male$Crude.Rate)
oo_male_sumstats_rate

# summary statistics for female
oo_female_sumstats_rate <- summary(oo_female$Crude.Rate)
oo_female_sumstats_rate

# Testing assumptions prior to statistical testing
# Do male/female populations have the same variance? 
# p-value of F-test must be greater than the alpha = 0.05, our p-value = 0.005976 so variance assumption fails
oo_gender_var <- var.test(oo_male$Crude.Rate, oo_female$Crude.Rate, ratio = 1)
oo_gender_var

# Is data normally distributed? or skewed?
# males
shapiro.test(oo_male$Crude.Rate) # if p-value > 0.05 implies normal disctibution, our p-value = 0.01153 so not normal
skewness(oo_male$Crude.Rate, na.rm=TRUE) # should between -1 and 1, ours is 1.185292, so positively skewed
boxplot(oo_male$Crude.Rate) 
hist(oo_male$Crude.Rate)
ggqqplot(oo_male$Crude.Rate)
# male data not normal, try log tranformation and re-test
oo_male$Log1p.Crude.Rate <- log1p(oo_male$Crude.Rate) # add new column with log1p transformation of crude rate to achieve normality for analysis
shapiro.test(oo_male$Log1p.Crude.Rate) # if p-value > 0.05 implies normal disctibution, ours is now p-value = 0.4849 so now normal
skewness(oo_male$Log1p.Crude.Rate, na.rm=TRUE) # should between -1 and 1, ours is 0.43923, so now ok
boxplot(oo_male$Log1p.Crude.Rate) 
hist(oo_male$Log1p.Crude.Rate)
ggqqplot(oo_male$Log1p.Crude.Rate)

# females
shapiro.test(oo_female$Crude.Rate) # if p-value > 0.05 implies normal disctibution, ours is p-value = 0.6072 so is normal
skewness(oo_female$Crude.Rate, na.rm=TRUE) # should between -1 and 1, ours is 0.4734665, so is not skewed
boxplot(oo_female$Crude.Rate) 
hist(oo_female$Crude.Rate)
ggqqplot(oo_female$Crude.Rate)

# log tranform data (to match males) and re-test normality and plotting
oo_female$Log1p.Crude.Rate <- log1p(oo_female$Crude.Rate) # add new column with log1p transformation of crude rate to achieve normality for analysis
shapiro.test(oo_female$Log1p.Crude.Rate) # if p-value > 0.05 implies normal disctibution, ours is now p-value = 0.8438 so is normal
skewness(oo_female$Log1p.Crude.Rate, na.rm=TRUE) # should between -1 and 1, ours is -0.2367126, so is ok
boxplot(oo_female$Log1p.Crude.Rate) 
hist(oo_female$Log1p.Crude.Rate)
ggqqplot(oo_female$Log1p.Crude.Rate)

# Re-test variance
oo_gender_var <- var.test(oo_male$Log1p.Crude.Rate, oo_female$Log1p.Crude.Rate, ratio = 1)
oo_gender_var

oo_gender_sig <- t.test(oo_male$Log1p.Crude.Rate, oo_female$Log1p.Crude.Rate, var.equal = TRUE, paired = FALSE, alternative = "greater")
oo_gender_sig #less than the significance level alpha = 0.05, our p-value < 1.709e-05, so we have statistical significance and can reject the null hypothesis

# graph the results
g_plot <- ggplot(data = oo_gender, aes(x=Year, y=Crude.Rate)) +
  geom_point(aes(color=Gender,size=Deaths)) + 
  geom_line(aes(color=Gender)) +
  labs(title = "Opioid Overdose Death Rates by Gender",
       subtitle = "death rates per 100,000 population",
       x = "", y = "") +
  scale_color_manual(values = c("darkred","steelblue")) 
  #geom_line(size = 0.75, colour = "black", lineend="round")
print(g_plot)
