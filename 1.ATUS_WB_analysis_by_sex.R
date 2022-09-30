rm(list = ls())

library(foreign)
library(readstata13)
library(tidyverse)
library(reshape2)


setwd("Data/")

# Function that groups continuous variables by interval categories
interval.cat <- function(x, lower = 0, upper, by = 5,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}


# Read and filter the data to 2012-2013 subset and relevant weights
atus.wb <- readRDS("atus_wb_12_13.rds")
atus.act <- readRDS("atus_act_12_13.rds")
atus.act.sum <- readRDS("atus_act_sum_12_13.rds")
atus.resp <- readRDS("atus_resp_12_13.rds")
atus.act.wgt <- readRDS("atus_act_wgt_12_13.rds")

# Merge the activity duration and classification attributes to well-being data
atus.wb.act <- atus.wb %>% 
  left_join(atus.act %>% select(tucaseid, tuactivity_n, tuactdur, trcodep),
            by = c("tucaseid", "tuactivity_n"))


# Classify childcare and adult care
atus.wb.act.sum <- atus.wb.act %>% 
  mutate(trcodep = as.character(sprintf("%06d", trcodep), length = 6),
         childcare = if_else(substr(trcodep, 1, 4) == "0301" | substr(trcodep, 1, 4) == "0302" | 
                               substr(trcodep, 1, 4) == "0303" | substr(trcodep, 1, 4) == "0401" |
                               substr(trcodep, 1, 4) == "0402" | substr(trcodep, 1, 4) == "0403" |
                               trcodep == "180301" | trcodep == "180302" | trcodep == "180303" | 
                               trcodep == "180401" | trcodep == "180402" | trcodep == "180403", 1, 0),
         adultcare = if_else(substr(trcodep, 1, 4) == "0304" | substr(trcodep, 1, 4) == "0305" | 
                               substr(trcodep, 1, 4) == "0404" | substr(trcodep, 1, 4)  ==  "0405" |
                               trcodep == "180304" | trcodep == "180305" | trcodep == "180404" | 
                               trcodep == "180405", 1, 0)) %>% 
  
    # Merging in respondents' age, sex, race and Hispanic status
  left_join(atus.act.sum %>% select(tucaseid, teage, tesex), by = "tucaseid") %>% 
  
  # Flag activities by sex of caregiver
  mutate(m.childcare = if_else(childcare == 1 & tesex == "Male", 1, 0),
         m.adultcare = if_else(adultcare == 1 & tesex == "Male", 1, 0),
         f.childcare = if_else(childcare == 1 & tesex == "Female", 1, 0),
         f.adultcare = if_else(adultcare == 1 & tesex == "Female", 1, 0)) %>% 
  
    # Break down age by 5-year age groups
  mutate(age_cat = as.factor(interval.cat(teage, lower = 15, upper = 85))) %>% 
  
  # Break the activity duration by 30-minute intervals
  mutate(act_duration = as.factor(interval.cat(tuactdur, lower = 0, upper = 240, by = 30))) %>% 
  
  # Re-code the feeling scores to scale minimum to 0 (0-6) and drop the Don't Know and Refused categories
  mutate_at(.vars = vars(wuhappy, wumeaning, wusad, wupain, wustress, wutired),
            .funs = list(~ifelse(. < 0, NA, .))) %>% 
  
  # Import activity eligibility variable from the Respondent file to identify activities that were available for selection in WB Module
  # (this is required to generate ATUS mean estimates)
  left_join(atus.resp %>% select(tucaseid, wrtelig), by = "tucaseid") 
  
 
  
## Generate ATUS weighted averages for each activity type

# CHILDCARE
childcare.wgt.means <- atus.wb.act.sum %>% 
  
  # Pick only the variables to be used in the calculation
  select(tucaseid, tuactivity_n, tesex, childcare, wuhappy, wumeaning, wupain, wusad, wustress, wutired, wufnactwtc, wrtelig) %>% 
  
  # Selecting only childcare activities
  filter(childcare == 1) %>%  
  
  # Generating weighted affect numerators and denominators
  mutate(m.childcare.wgt.happy = if_else(wuhappy >= 0 & tesex == "Male", wuhappy * wufnactwtc * wrtelig, 0),
         m.childcare.wgt.elig.happy = if_else(wuhappy >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.childcare.wgt.happy = if_else(wuhappy >= 0 & tesex == "Female", wuhappy * wufnactwtc * wrtelig, 0),
         f.childcare.wgt.elig.happy = if_else(wuhappy >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0),
         
         m.childcare.wgt.meaning = if_else(wumeaning >= 0 & tesex == "Male", wumeaning * wufnactwtc * wrtelig, 0),
         m.childcare.wgt.elig.meaning = if_else(wumeaning >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.childcare.wgt.meaning = if_else(wumeaning >= 0 & tesex == "Female", wumeaning * wufnactwtc * wrtelig, 0),
         f.childcare.wgt.elig.meaning = if_else(wumeaning >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0),
         
         m.childcare.wgt.sad = if_else(wusad >= 0 & tesex == "Male", wusad * wufnactwtc * wrtelig, 0),
         m.childcare.wgt.elig.sad = if_else(wusad >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.childcare.wgt.sad = if_else(wusad >= 0 & tesex == "Female", wusad * wufnactwtc * wrtelig, 0),
         f.childcare.wgt.elig.sad = if_else(wusad >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0),
         
         m.childcare.wgt.pain = if_else(wupain >=0 & tesex == "Male", wupain * wufnactwtc * wrtelig, 0),
         m.childcare.wgt.elig.pain = if_else(wupain >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.childcare.wgt.pain = if_else(wupain >=0 & tesex == "Female", wupain * wufnactwtc * wrtelig, 0),
         f.childcare.wgt.elig.pain = if_else(wupain >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0),
         
         m.childcare.wgt.stress = if_else(wustress >= 0 & tesex == "Male", wustress * wufnactwtc * wrtelig, 0),
         m.childcare.wgt.elig.stress = if_else(wustress >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.childcare.wgt.stress = if_else(wustress >= 0 & tesex == "Female", wustress * wufnactwtc * wrtelig, 0),
         f.childcare.wgt.elig.stress = if_else(wustress >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0),
         
         m.childcare.wgt.tired = if_else(wutired >=0 & tesex == "Male", wutired * wufnactwtc * wrtelig, 0),
         m.childcare.wgt.elig.tired = if_else(wutired >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.childcare.wgt.tired = if_else(wutired >=0 & tesex == "Female", wutired * wufnactwtc * wrtelig, 0),
         f.childcare.wgt.elig.tired = if_else(wutired >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0)) %>% 
  
  # Computing the weighted means
  summarise(m.childcare.wgt.mean.happy = sum(m.childcare.wgt.happy, na.rm = T)/sum(m.childcare.wgt.elig.happy, na.rm = T),
            m.childcare.wgt.mean.meaning = sum(m.childcare.wgt.meaning, na.rm = T)/sum(m.childcare.wgt.elig.meaning, na.rm = T),
            m.childcare.wgt.mean.pain = sum(m.childcare.wgt.pain, na.rm = T)/sum(m.childcare.wgt.elig.pain, na.rm = T),
            m.childcare.wgt.mean.sad = sum(m.childcare.wgt.sad, na.rm = T)/sum(m.childcare.wgt.elig.sad, na.rm = T),
            m.childcare.wgt.mean.stress = sum(m.childcare.wgt.stress, na.rm = T)/sum(m.childcare.wgt.elig.stress, na.rm = T),
            m.childcare.wgt.mean.tired = sum(m.childcare.wgt.tired, na.rm = T)/sum(m.childcare.wgt.elig.tired, na.rm = T),
            f.childcare.wgt.mean.happy = sum(f.childcare.wgt.happy, na.rm = T)/sum(f.childcare.wgt.elig.happy, na.rm = T),
            f.childcare.wgt.mean.meaning = sum(f.childcare.wgt.meaning, na.rm = T)/sum(f.childcare.wgt.elig.meaning, na.rm = T),
            f.childcare.wgt.mean.pain = sum(f.childcare.wgt.pain, na.rm = T)/sum(f.childcare.wgt.elig.pain, na.rm = T),
            f.childcare.wgt.mean.sad = sum(f.childcare.wgt.sad, na.rm = T)/sum(f.childcare.wgt.elig.sad, na.rm = T),
            f.childcare.wgt.mean.stress = sum(f.childcare.wgt.stress, na.rm = T)/sum(f.childcare.wgt.elig.stress, na.rm = T),
            f.childcare.wgt.mean.tired = sum(f.childcare.wgt.tired, na.rm = T)/sum(f.childcare.wgt.elig.tired, na.rm = T))

         
         
         
         
# ADULTCARE
adultcare.wgt.means <- atus.wb.act.sum %>%  
  
  # Pick only the variables to be used in the calculation
  select(tucaseid, tuactivity_n, tesex, adultcare, wuhappy, wumeaning, wupain, wusad, wustress, wutired, wufnactwtc, wrtelig) %>% 
  
  # Selecting only adultcare activities
  filter(adultcare == 1) %>%  
  
  # Generating weighted affect numerators and denominators
  mutate(m.adultcare.wgt.happy = if_else(wuhappy >= 0 & tesex == "Male", wuhappy * wufnactwtc * wrtelig, 0),
         m.adultcare.wgt.elig.happy = if_else(wuhappy >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.happy = if_else(wuhappy >= 0 & tesex == "Female", wuhappy * wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.elig.happy = if_else(wuhappy >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0),
         
         m.adultcare.wgt.meaning = if_else(wumeaning >= 0 & tesex == "Male", wumeaning * wufnactwtc * wrtelig, 0),
         m.adultcare.wgt.elig.meaning = if_else(wumeaning >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.meaning = if_else(wumeaning >= 0 & tesex == "Female", wumeaning * wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.elig.meaning = if_else(wumeaning >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0),
         
         m.adultcare.wgt.sad = if_else(wusad >= 0 & tesex == "Male", wusad * wufnactwtc * wrtelig, 0),
         m.adultcare.wgt.elig.sad = if_else(wusad >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.sad = if_else(wusad >= 0 & tesex == "Female", wusad * wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.elig.sad = if_else(wusad >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0),
         
         m.adultcare.wgt.pain = if_else(wupain >=0 & tesex == "Male", wupain * wufnactwtc * wrtelig, 0),
         m.adultcare.wgt.elig.pain = if_else(wupain >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.pain = if_else(wupain >=0 & tesex == "Female", wupain * wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.elig.pain = if_else(wupain >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0),
         
         m.adultcare.wgt.stress = if_else(wustress >= 0 & tesex == "Male", wustress * wufnactwtc * wrtelig, 0),
         m.adultcare.wgt.elig.stress = if_else(wustress >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.stress = if_else(wustress >= 0 & tesex == "Female", wustress * wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.elig.stress = if_else(wustress >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0),
         
         m.adultcare.wgt.tired = if_else(wutired >=0 & tesex == "Male", wutired * wufnactwtc * wrtelig, 0),
         m.adultcare.wgt.elig.tired = if_else(wutired >= 0 & tesex == "Male", wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.tired = if_else(wutired >=0 & tesex == "Female", wutired * wufnactwtc * wrtelig, 0),
         f.adultcare.wgt.elig.tired = if_else(wutired >= 0 & tesex == "Female", wufnactwtc * wrtelig, 0)) %>% 
  
  # Computing the weighted means
  summarise(m.adultcare.wgt.mean.happy = sum(m.adultcare.wgt.happy, na.rm = T)/sum(m.adultcare.wgt.elig.happy, na.rm = T),
            m.adultcare.wgt.mean.meaning = sum(m.adultcare.wgt.meaning, na.rm = T)/sum(m.adultcare.wgt.elig.meaning, na.rm = T),
            m.adultcare.wgt.mean.pain = sum(m.adultcare.wgt.pain, na.rm = T)/sum(m.adultcare.wgt.elig.pain, na.rm = T),
            m.adultcare.wgt.mean.sad = sum(m.adultcare.wgt.sad, na.rm = T)/sum(m.adultcare.wgt.elig.sad, na.rm = T),
            m.adultcare.wgt.mean.stress = sum(m.adultcare.wgt.stress, na.rm = T)/sum(m.adultcare.wgt.elig.stress, na.rm = T),
            m.adultcare.wgt.mean.tired = sum(m.adultcare.wgt.tired, na.rm = T)/sum(m.adultcare.wgt.elig.tired, na.rm = T),
            f.adultcare.wgt.mean.happy = sum(f.adultcare.wgt.happy, na.rm = T)/sum(f.adultcare.wgt.elig.happy, na.rm = T),
            f.adultcare.wgt.mean.meaning = sum(f.adultcare.wgt.meaning, na.rm = T)/sum(f.adultcare.wgt.elig.meaning, na.rm = T),
            f.adultcare.wgt.mean.pain = sum(f.adultcare.wgt.pain, na.rm = T)/sum(f.adultcare.wgt.elig.pain, na.rm = T),
            f.adultcare.wgt.mean.sad = sum(f.adultcare.wgt.sad, na.rm = T)/sum(f.adultcare.wgt.elig.sad, na.rm = T),
            f.adultcare.wgt.mean.stress = sum(f.adultcare.wgt.stress, na.rm = T)/sum(f.adultcare.wgt.elig.stress, na.rm = T),
            f.adultcare.wgt.mean.tired = sum(f.adultcare.wgt.tired, na.rm = T)/sum(f.adultcare.wgt.elig.tired, na.rm = T))




## CALCULATING STANDARD ERRORS USING ATUS METHODOLOGY

# Creating the dataset with replicate weights and the original emotion variables
SE.dataset <- atus.act.wgt %>%
  left_join(atus.wb.act.sum %>% select(tucaseid, tesex, tuactivity_n, childcare, adultcare, 
                                       wuhappy, wumeaning, wupain, wusad, wustress, wutired, wrtelig), 
            by = c("tucaseid", "tuactivity_n"))


# Setting up storage matrices for replicate weight estimates
replicate.storage.mat <- matrix(NA, nrow = 12, ncol = 161)
replicate.storage.mat[,1] <- rep(c("Male", "Female"), each = 6)
rownames(replicate.storage.mat) <- rep(c("Happiness", "Meaning", "Sadness", "Pain", "Stress", "Tiredness"), 2)
colnames(replicate.storage.mat) <- c("Sex", paste0(rep("Replicate", 160), seq(1, 160, 1)))

childcare.replicate.store.mat <- adultcare.replicate.store.mat <- replicate.storage.mat
m.childcare.sq.diff.sums <- f.childcare.sq.diff.sums <- m.adultcare.sq.diff.sums <- f.adultcare.sq.diff.sums <- matrix(NA, nrow = 6, ncol = 160)

## Standard errors
# Iterating through the replicate weights, as per ATUS methodology for Standard Error calculation
for (i in 1:160) {
  
  # Selecting replicate weight for a given iteration of the loop
  iter.weight <- paste0("wufnactwtc", formatC(i, flag = "0", width = 3))
  
  # Generating estimates with each replicate weight
  x.iter.childcare <- SE.dataset %>% 
    
    # Selecting CHILDCARE
    filter(childcare == 1) %>% 

    # Generating weighted affect numerators and denominators (using replicate weights)
    mutate(m.childcare.wgt.happy = if_else(wuhappy >= 0 & tesex == "Male", wuhappy * !!as.symbol(iter.weight) * wrtelig, 0),
           m.childcare.wgt.elig.happy = if_else(wuhappy >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.happy = if_else(wuhappy >= 0 & tesex == "Female", wuhappy * !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.elig.happy = if_else(wuhappy >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0),
           
           m.childcare.wgt.meaning = if_else(wumeaning >= 0 & tesex == "Male", wumeaning * !!as.symbol(iter.weight) * wrtelig, 0),
           m.childcare.wgt.elig.meaning = if_else(wumeaning >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.meaning = if_else(wumeaning >= 0 & tesex == "Female", wumeaning * !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.elig.meaning = if_else(wumeaning >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0),
           
           m.childcare.wgt.sad = if_else(wusad >= 0 & tesex == "Male", wusad * !!as.symbol(iter.weight) * wrtelig, 0),
           m.childcare.wgt.elig.sad = if_else(wusad >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.sad = if_else(wusad >= 0 & tesex == "Female", wusad * !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.elig.sad = if_else(wusad >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0),
           
           m.childcare.wgt.pain = if_else(wupain >=0 & tesex == "Male", wupain * !!as.symbol(iter.weight) * wrtelig, 0),
           m.childcare.wgt.elig.pain = if_else(wupain >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.pain = if_else(wupain >=0 & tesex == "Female", wupain * !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.elig.pain = if_else(wupain >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0),
           
           m.childcare.wgt.stress = if_else(wustress >= 0 & tesex == "Male", wustress * !!as.symbol(iter.weight) * wrtelig, 0),
           m.childcare.wgt.elig.stress = if_else(wustress >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.stress = if_else(wustress >= 0 & tesex == "Female", wustress * !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.elig.stress = if_else(wustress >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0),
           
           m.childcare.wgt.tired = if_else(wutired >=0 & tesex == "Male", wutired * !!as.symbol(iter.weight) * wrtelig, 0),
           m.childcare.wgt.elig.tired = if_else(wutired >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.tired = if_else(wutired >=0 & tesex == "Female", wutired * !!as.symbol(iter.weight) * wrtelig, 0),
           f.childcare.wgt.elig.tired = if_else(wutired >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0)) %>% 
    
    # Computing the weighted replicate means
    summarise(m.childcare.wgt.mean.happy = sum(m.childcare.wgt.happy, na.rm = T)/sum(m.childcare.wgt.elig.happy, na.rm = T),
              m.childcare.wgt.mean.meaning = sum(m.childcare.wgt.meaning, na.rm = T)/sum(m.childcare.wgt.elig.meaning, na.rm = T),
              m.childcare.wgt.mean.pain = sum(m.childcare.wgt.pain, na.rm = T)/sum(m.childcare.wgt.elig.pain, na.rm = T),
              m.childcare.wgt.mean.sad = sum(m.childcare.wgt.sad, na.rm = T)/sum(m.childcare.wgt.elig.sad, na.rm = T),
              m.childcare.wgt.mean.stress = sum(m.childcare.wgt.stress, na.rm = T)/sum(m.childcare.wgt.elig.stress, na.rm = T),
              m.childcare.wgt.mean.tired = sum(m.childcare.wgt.tired, na.rm = T)/sum(m.childcare.wgt.elig.tired, na.rm = T),
              f.childcare.wgt.mean.happy = sum(f.childcare.wgt.happy, na.rm = T)/sum(f.childcare.wgt.elig.happy, na.rm = T),
              f.childcare.wgt.mean.meaning = sum(f.childcare.wgt.meaning, na.rm = T)/sum(f.childcare.wgt.elig.meaning, na.rm = T),
              f.childcare.wgt.mean.pain = sum(f.childcare.wgt.pain, na.rm = T)/sum(f.childcare.wgt.elig.pain, na.rm = T),
              f.childcare.wgt.mean.sad = sum(f.childcare.wgt.sad, na.rm = T)/sum(f.childcare.wgt.elig.sad, na.rm = T),
              f.childcare.wgt.mean.stress = sum(f.childcare.wgt.stress, na.rm = T)/sum(f.childcare.wgt.elig.stress, na.rm = T),
              f.childcare.wgt.mean.tired = sum(f.childcare.wgt.tired, na.rm = T)/sum(f.childcare.wgt.elig.tired, na.rm = T))
  
  
  x.iter.adultcare <- SE.dataset %>% 
    
    # Selecting ADULTCARE
    filter(adultcare == 1) %>% 
    
    # Generating weighted affect numerators and denominators (using replicate weights)
    mutate(m.adultcare.wgt.happy = if_else(wuhappy >= 0 & tesex == "Male", wuhappy * !!as.symbol(iter.weight) * wrtelig, 0),
           m.adultcare.wgt.elig.happy = if_else(wuhappy >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.happy = if_else(wuhappy >= 0 & tesex == "Female", wuhappy * !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.elig.happy = if_else(wuhappy >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0),
           
           m.adultcare.wgt.meaning = if_else(wumeaning >= 0 & tesex == "Male", wumeaning * !!as.symbol(iter.weight) * wrtelig, 0),
           m.adultcare.wgt.elig.meaning = if_else(wumeaning >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.meaning = if_else(wumeaning >= 0 & tesex == "Female", wumeaning * !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.elig.meaning = if_else(wumeaning >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0),
           
           m.adultcare.wgt.sad = if_else(wusad >= 0 & tesex == "Male", wusad * !!as.symbol(iter.weight) * wrtelig, 0),
           m.adultcare.wgt.elig.sad = if_else(wusad >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.sad = if_else(wusad >= 0 & tesex == "Female", wusad * !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.elig.sad = if_else(wusad >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0),
           
           m.adultcare.wgt.pain = if_else(wupain >=0 & tesex == "Male", wupain * !!as.symbol(iter.weight) * wrtelig, 0),
           m.adultcare.wgt.elig.pain = if_else(wupain >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.pain = if_else(wupain >=0 & tesex == "Female", wupain * !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.elig.pain = if_else(wupain >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0),
           
           m.adultcare.wgt.stress = if_else(wustress >= 0 & tesex == "Male", wustress * !!as.symbol(iter.weight) * wrtelig, 0),
           m.adultcare.wgt.elig.stress = if_else(wustress >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.stress = if_else(wustress >= 0 & tesex == "Female", wustress * !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.elig.stress = if_else(wustress >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0),
           
           m.adultcare.wgt.tired = if_else(wutired >=0 & tesex == "Male", wutired * !!as.symbol(iter.weight) * wrtelig, 0),
           m.adultcare.wgt.elig.tired = if_else(wutired >= 0 & tesex == "Male", !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.tired = if_else(wutired >=0 & tesex == "Female", wutired * !!as.symbol(iter.weight) * wrtelig, 0),
           f.adultcare.wgt.elig.tired = if_else(wutired >= 0 & tesex == "Female", !!as.symbol(iter.weight) * wrtelig, 0)) %>% 
    
    # Computing the weighted replicate means
    summarise(m.adultcare.wgt.mean.happy = sum(m.adultcare.wgt.happy, na.rm = T)/sum(m.adultcare.wgt.elig.happy, na.rm = T),
              m.adultcare.wgt.mean.meaning = sum(m.adultcare.wgt.meaning, na.rm = T)/sum(m.adultcare.wgt.elig.meaning, na.rm = T),
              m.adultcare.wgt.mean.pain = sum(m.adultcare.wgt.pain, na.rm = T)/sum(m.adultcare.wgt.elig.pain, na.rm = T),
              m.adultcare.wgt.mean.sad = sum(m.adultcare.wgt.sad, na.rm = T)/sum(m.adultcare.wgt.elig.sad, na.rm = T),
              m.adultcare.wgt.mean.stress = sum(m.adultcare.wgt.stress, na.rm = T)/sum(m.adultcare.wgt.elig.stress, na.rm = T),
              m.adultcare.wgt.mean.tired = sum(m.adultcare.wgt.tired, na.rm = T)/sum(m.adultcare.wgt.elig.tired, na.rm = T),
              f.adultcare.wgt.mean.happy = sum(f.adultcare.wgt.happy, na.rm = T)/sum(f.adultcare.wgt.elig.happy, na.rm = T),
              f.adultcare.wgt.mean.meaning = sum(f.adultcare.wgt.meaning, na.rm = T)/sum(f.adultcare.wgt.elig.meaning, na.rm = T),
              f.adultcare.wgt.mean.pain = sum(f.adultcare.wgt.pain, na.rm = T)/sum(f.adultcare.wgt.elig.pain, na.rm = T),
              f.adultcare.wgt.mean.sad = sum(f.adultcare.wgt.sad, na.rm = T)/sum(f.adultcare.wgt.elig.sad, na.rm = T),
              f.adultcare.wgt.mean.stress = sum(f.adultcare.wgt.stress, na.rm = T)/sum(f.adultcare.wgt.elig.stress, na.rm = T),
              f.adultcare.wgt.mean.tired = sum(f.adultcare.wgt.tired, na.rm = T)/sum(f.adultcare.wgt.elig.tired, na.rm = T))
  
  
  # Matrices storing replicate estimates 
  childcare.replicate.store.mat[,i+1] <- t(x.iter.childcare)
  adultcare.replicate.store.mat[,i+1] <- t(x.iter.adultcare)

}

# Computing the standard errors and constructing 95% confidence intervals for all emotions in selected activities
for (i in 1:160) {
  m.childcare.sq.diff.sums[,i] <- (as.numeric(childcare.replicate.store.mat[1:6,i+1]) - as.matrix(childcare.wgt.means[1:6]))^2
  f.childcare.sq.diff.sums[,i] <- (as.numeric(childcare.replicate.store.mat[7:12,i+1]) - as.matrix(childcare.wgt.means[7:12]))^2
  m.adultcare.sq.diff.sums[,i] <- (as.numeric(adultcare.replicate.store.mat[1:6,i+1]) - as.matrix(adultcare.wgt.means)[1:6])^2
  f.adultcare.sq.diff.sums[,i] <- (as.numeric(adultcare.replicate.store.mat[7:12,i+1]) - as.matrix(adultcare.wgt.means)[7:12])^2
}

m.childcare.SE <- sqrt((4/160) * rowSums(m.childcare.sq.diff.sums))
m.childcare.lower <- as.matrix(childcare.wgt.means[1:6]) - 1.96*m.childcare.SE
m.childcare.upper <- as.matrix(childcare.wgt.means[1:6]) + 1.96*m.childcare.SE

f.childcare.SE <- sqrt((4/160) * rowSums(f.childcare.sq.diff.sums))
f.childcare.lower <- as.matrix(childcare.wgt.means[7:12]) - 1.96*f.childcare.SE
f.childcare.upper <- as.matrix(childcare.wgt.means[7:12]) + 1.96*f.childcare.SE

m.adultcare.SE <- sqrt((4/160) * rowSums(m.adultcare.sq.diff.sums))
m.adultcare.lower <- as.matrix(adultcare.wgt.means[1:6]) - 1.96*m.adultcare.SE
m.adultcare.upper <- as.matrix(adultcare.wgt.means[1:6]) + 1.96*m.adultcare.SE

f.adultcare.SE <- sqrt((4/160) * rowSums(f.adultcare.sq.diff.sums))
f.adultcare.lower <- as.matrix(adultcare.wgt.means[7:12]) - 1.96*f.adultcare.SE
f.adultcare.upper <- as.matrix(adultcare.wgt.means[7:12]) + 1.96*f.adultcare.SE

## Combining the estimates and the corresponding confidence intervals into a separate object
wb.estimates <- tibble(Activity = factor(rep(c("Childcare", "Adult Care"), each = 12),
                                         levels = c("Childcare", "Adult Care"),
                                         ordered = TRUE),
                       Sex = rep(rep(c("Male", "Female"), each = 6), 2),
                       Emotion = factor(rep(c("Happy", "Meaningful", "Sad", "Pained", "Stressed", "Tired"), 4),
                                       levels = c("Happy", "Meaningful", "Sad", "Pained", "Stressed", "Tired"),
                                       ordered = TRUE),
                       Mean = t(cbind(childcare.wgt.means, adultcare.wgt.means)),
                       Lower = melt(cbind(m.childcare.lower, f.childcare.lower, m.adultcare.lower, f.adultcare.lower))[,3],
                       Upper = melt(cbind(m.childcare.upper, f.childcare.upper, m.adultcare.upper, f.adultcare.upper))[,3])



## Comparing the coefficients
## t-tests between emotions in all activities

# Disabling scientific notation
options(scipen = 99999)
require(BSDA)

# Putting the names of activities to compare in a vector
test.lst.activities.atus <- c("m.childcare", "m.adultcare", "f.childcare", "f.adultcare") 

# Split weighted means data frames by sex for testing
m.childcare.wgt.means <- childcare.wgt.means[1:6]
m.adultcare.wgt.means <- adultcare.wgt.means[1:6]
f.childcare.wgt.means <- childcare.wgt.means[7:12]
f.adultcare.wgt.means <- adultcare.wgt.means[7:12]
colnames(m.childcare.wgt.means) <- colnames(m.adultcare.wgt.means) <- colnames(f.childcare.wgt.means) <- colnames(f.adultcare.wgt.means) <- 
  c("Happy", "Meaningful", "Sad", "Pained", "Stressed", "Tired")

# Defining the function that will execute t-tests
t.test.mat.fun.atus <- function(x = test.lst.activities.atus, emotions = c("Happy", "Meaningful", "Sad", "Pained", "Stressed", "Tired")) {
  
  # Preliminaries
  n.vars <- length(emotions)    # Number of emotions 
  n.activities <- length(x)     # Number of activities
  
  # Setting up the storage matrix
  t.test.mat <- matrix(NA, nrow = n.vars * n.activities^2, ncol = 13) # COLS: Emotion, Activity, ComparisonActivity, N, Activity.Mean, Comparison.Mean, SE, SD, t-stat, p-val, 95ci lower, 95ci upper
  
  iter.num <- 1
  
  for(j in 1:n.activities) {
    for (k in 1:n.activities) {
      for (i in 1:n.vars) {
        
        # Compute the t-test
        test.res <- tsum.test(mean.x = as.numeric(get(paste0(x[j], ".wgt.means"))[i]), 
                              mean.y = as.numeric(get(paste0(x[k], ".wgt.means"))[i]), 
                              s.x = as.numeric(get(paste0(x[j], ".SE"))[i]) * sqrt(sum(atus.wb.act.sum[, x[j]])), # Convert SE to Standard Deviation
                              s.y = as.numeric(get(paste0(x[k], ".SE"))[i]) * sqrt(sum(atus.wb.act.sum[, x[k]])), # Convert SE to Standard Deviation
                              n.x = sum(atus.wb.act.sum[,x[j]]), 
                              n.y = sum(atus.wb.act.sum[,x[k]]),  
                              alternative = "two.sided", 
                              var.equal = FALSE, 
                              conf.level = 0.95)
        
        # Fill in the rows of the matrix
        t.test.mat[iter.num,] <- c(emotions[i],                                                                              # Emotion name
                                   x[j],                                                                                     # Activity type
                                   x[k],                                                                                     # Comparison Activity type
                                   sum(atus.wb.act.sum[, x[j]]),                                                             # Sample size of Activity type
                                   round(as.numeric(get(paste0(x[j], ".wgt.means"))[i]), 3),                                 # ATUS mean of the Activity type
                                   round(as.numeric(get(paste0(x[k], ".wgt.means"))[i]), 3),                                 # ATUS mean of the Comparison Activity type
                                   round(as.numeric(get(paste0(x[j], ".wgt.means"))[i]) - 
                                           as.numeric(get(paste0(x[k], ".wgt.means"))[i]), 3),                               # Difference between the means of the two activities
                                   round(as.numeric(get(paste0(x[j], ".SE"))[i]), 3),                                        # Standard Error 
                                   round(as.numeric(get(paste0(x[j], ".SE"))[i]) * sqrt(sum(atus.wb.act.sum[, x[j]])), 3),   # Standard Deviation
                                   round(as.numeric(unname(unlist(test.res)[1])), 3),                                        # t-statistic
                                   format(round(as.numeric(unname(unlist(test.res)[3])), 4), nsmall = 4),                    # p-value
                                   round(as.numeric(unname(unlist(test.res)[4])), 3),                                        # Lower 95% confidence bound
                                   round(as.numeric(unname(unlist(test.res)[5])), 3))                                        # Upper 95% confidence bound
        
        iter.num <- iter.num + 1
        
      }  
    }
  }
  
  colnames(t.test.mat) <- c("Emotion", "Activity", "Comparison.Activity", "N", "Activity.Mean", "Comparison.Mean", "Difference", "SE", "SD", "t.statistic", "p.value", "CI.lower", "CI.upper")
  t.test.mat <- t.test.mat[!(t.test.mat[, "Activity"] == t.test.mat[, "Comparison.Activity"]),]
  return(t.test.mat)
}

# Generate the table of t-tests and coefficients
t.test.matrix.atus <- t.test.mat.fun.atus()

# Export the t-statistics table
write.csv(t.test.matrix.atus, "t_tests_atus.csv", row.names = F, quote = F)


## Matrix with statistics needed to compute the indexes of positivity and negativity
atus.stats <- cbind(
  rbind(as.matrix(unname(m.childcare.wgt.means)), 
        as.matrix(unname(m.adultcare.wgt.means)),
        as.matrix(unname(f.childcare.wgt.means)), 
        as.matrix(unname(f.adultcare.wgt.means))),
  c(sum(atus.wb.act.sum[, "m.childcare"]), 
    sum(atus.wb.act.sum[, "m.adultcare"]),
    sum(atus.wb.act.sum[, "f.childcare"]), 
    sum(atus.wb.act.sum[, "f.adultcare"]))) 
colnames(atus.stats) <- c("Happy", "Meaning", "Pain", "Sad", "Stress", "Tired", "N")
rownames(atus.stats) <- c("CC.mean.male", "AC.mean.male", "CC.mean.female", "AC.mean.female")

atus.indices <- atus.stats %>%
  as_tibble() %>%
  mutate(Activity = c("Childcare", "Adult care", "Childcare", "Adult care"), 
         Sex = c("Male", "Male", "Female", "Female"),
         positive = (Happy + Meaning + (6 - Pain) + (6 - Sad) + (6 - Stress) + (6 - Tired)) / 6,
         negative = ((6 - Meaning) + (6 - Meaning) + Pain + Sad + Stress + Tired) / 6) %>% 
  select(Activity, Sex, positive, negative, N)
  

# ## Keep only the indices and the summary output
# rm(list = setdiff(ls(), c("t.test.matrix", "atus.stats", "atus.indices", "wb.estimates")))
#
# 
# ## Save the workspace environment
# save.image("atus_means_childcare_adultcare_by_sex.RData")

