rm(list = ls())

library(foreign)
library(readstata13)
library(tidyverse)
library(reshape2)

setwd("Data/")

###################################################
## Creating functions that will be used later on ##
###################################################

# Weighted mean function
w.mean <- function(x,w) {
  return(sum(x * w) / sum(w))
}

# Weighted variance function
w.var <- function(x,w) {
  return(
    sum(w * (x - w.mean(x,w))^2) / (((length(w) - 1) * sum(w)) / length(w))
  )
}

# Function that groups ages into 10-year age categories
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

######################
## Data Preparation ##
######################

# Importing data from PSID DUST2013 module
flat13 <- as_tibble(read.dta13("DUST13_FLAT_stata.dta"))
act13 <- as_tibble(read.dta13("DUST13_ACT_stata.dta"))


dust13 <- merge(flat13, act13, by = 1:3, all = TRUE)

dust13 <- dust13 %>%
  mutate(rnum = 1:nrow(dust13),
         Sex = case_when(DF2GEND == 1 ~ "Male",
                         DF2GEND == 2 ~ "Female",
                         TRUE ~ as.character(NA)))

## Preparing dataset for caregivers for adults ##
# Extracting people who have cared for adults by selecting people who reported caregiving, and spending that time with other adults
adultcare <- dust13 %>%
  filter(DA2PCODE == 7 | DA2PCODE == 8) %>%  # 7 = HH chores/helping others or 8 = Providing care      
  filter(DF2T1C1 == 1 | DF2T2C1 == 1)        # Provided unpaid adult care in time diary 1 or 2

# Changing coded affect ratings of 1-7 to 0-6 to correspond with affect ratings as reported (discarding invalid/don't know/refused)
adultcare <- adultcare %>%
  filter(DA2FHAPP %in% c(1:7) | DA2FCALM %in% c(1:7) | DA2FFRUS %in% c(1:7) | DA2FWORR %in% c(1:7) | DA2FSAD %in% c(1:7) | DA2FTIRE %in% c(1:7) | DA2FPAIN %in% c(1:7)) %>%
  filter(DA2PCODE %in% c(1:9)) %>% 
  mutate(DA2FHAPP = DA2FHAPP - 1,
         DA2FWORR = DA2FWORR - 1,
         DA2FPAIN = DA2FPAIN - 1,
         DA2FCALM = DA2FCALM - 1,
         DA2FSAD = DA2FSAD - 1,
         DA2FTIRE = DA2FTIRE - 1,
         DA2FFRUS = DA2FFRUS - 1)

## Preparing dataset for caregivers for children ##
## Extracting people who have cared for adults by selecting people who reported caregiving, and spending that time with children
childcare <- dust13 %>%
  filter(DA2PCODE == 7 | DA2PCODE == 8) %>%     # 7 = HH chores/helping others or 8 = Providing care             
  filter(DF2T1C7 == 1 | DF2T2C7 == 1)           # Provided unpaid child care in time diary 1 or 2

# Changing coded affect ratings of 1-7 to 0-6 to correspond with affect ratings as reported
childcare <- childcare %>%
  filter(DA2FHAPP %in% c(1:7) | DA2FCALM %in% c(1:7) | DA2FFRUS %in% c(1:7) | DA2FWORR %in% c(1:7) | DA2FSAD %in% c(1:7) | DA2FTIRE %in% c(1:7) | DA2FPAIN %in% c(1:7)) %>%
  filter(DA2PCODE %in% c(1:9))

childcare <- childcare %>%
  mutate(DA2FHAPP = DA2FHAPP - 1,
         DA2FWORR = DA2FWORR - 1,
         DA2FPAIN = DA2FPAIN - 1,
         DA2FCALM = DA2FCALM - 1,
         DA2FSAD = DA2FSAD - 1,
         DA2FTIRE = DA2FTIRE - 1,
         DA2FFRUS = DA2FFRUS - 1)

# Creating 10-year age categories starting at age 15 to 85, for both caregivers of adults and children
adultcare$agecat <- age.cat(adultcare$DF2CYAGE, lower = 15, upper = 85, by = 5)
childcare$agecat <- age.cat(childcare$DF2CYAGE, lower = 15, upper = 85, by = 5)

###################
## Data Analysis ##
###################

## Calculating Weighted Means of affect scores ##
# Mean affect scores for all caregivers of adults
a.summary <- adultcare %>%
  summarise(Sex = "Total",
            w.mean(DA2FHAPP, DA2DWBWT),
            w.mean(DA2FCALM, DA2DWBWT), 
            w.mean(DA2FFRUS, DA2DWBWT), 
            w.mean(DA2FWORR, DA2DWBWT), 
            w.mean(DA2FSAD, DA2DWBWT), 
            w.mean(DA2FTIRE, DA2DWBWT),
            w.mean(DA2FPAIN, DA2DWBWT)) 

# Adult care summary by sex
a.summary.by.sex <- adultcare %>%
  group_by(Sex) %>% 
  summarise(w.mean(DA2FHAPP, DA2DWBWT),
            w.mean(DA2FCALM, DA2DWBWT), 
            w.mean(DA2FFRUS, DA2DWBWT), 
            w.mean(DA2FWORR, DA2DWBWT), 
            w.mean(DA2FSAD, DA2DWBWT), 
            w.mean(DA2FTIRE, DA2DWBWT),
            w.mean(DA2FPAIN, DA2DWBWT)) %>% 
  ungroup() 


# Mean affect scores for all caregivers of children
c.summary <- childcare %>%
  summarise(Sex = "Total",
            w.mean(DA2FHAPP, DA2DWBWT),
            w.mean(DA2FCALM, DA2DWBWT),
            w.mean(DA2FFRUS, DA2DWBWT),
            w.mean(DA2FWORR, DA2DWBWT),
            w.mean(DA2FSAD, DA2DWBWT),
            w.mean(DA2FTIRE, DA2DWBWT),
            w.mean(DA2FPAIN, DA2DWBWT))

# Childcare summary by sex
c.summary.by.sex <- childcare %>%
  group_by(Sex) %>% 
  summarise(w.mean(DA2FHAPP, DA2DWBWT),
            w.mean(DA2FCALM, DA2DWBWT),
            w.mean(DA2FFRUS, DA2DWBWT),
            w.mean(DA2FWORR, DA2DWBWT),
            w.mean(DA2FSAD, DA2DWBWT),
            w.mean(DA2FTIRE, DA2DWBWT),
            w.mean(DA2FPAIN, DA2DWBWT)) %>% 
  ungroup()


summary <- cbind(t(a.summary[,2:8]), unname(t(a.summary.by.sex[,2:8])), unname(t(c.summary[,2:8])), unname(t(c.summary.by.sex[,2:8])))
colnames(summary) <- c("adultcare.total", "adultcare.female", "adultcare.male", "childcare.total", "childcare.female", "childcare.male")
#write.csv(summary, "childvsadultcare_by_sex.csv")


# Prepare data for plotting later
# Create subsets for adult care and childcare by sex
m.adultcare <- adultcare %>% filter(Sex == "Male")
f.adultcare <- adultcare %>% filter(Sex == "Female")
m.childcare <- childcare %>% filter(Sex == "Male")
f.childcare <- childcare %>% filter(Sex == "Female")

## Generating confidence intervals for affect means
# For caregivers for adults, males and females
m.adult.se <- sqrt(c(w.var(m.adultcare$DA2FHAPP, m.adultcare$DA2DWBWT),
                     w.var(m.adultcare$DA2FCALM, m.adultcare$DA2DWBWT),
                     w.var(m.adultcare$DA2FFRUS, m.adultcare$DA2DWBWT), 
                     w.var(m.adultcare$DA2FWORR, m.adultcare$DA2DWBWT),
                     w.var(m.adultcare$DA2FSAD, m.adultcare$DA2DWBWT), 
                     w.var(m.adultcare$DA2FTIRE, m.adultcare$DA2DWBWT),
                     w.var(m.adultcare$DA2FPAIN, m.adultcare$DA2DWBWT))) / sqrt(nrow(m.adultcare))
m.adult.lower <- summary[,3] - 1.96*m.adult.se
m.adult.upper <- summary[,3] + 1.96*m.adult.se

f.adult.se <- sqrt(c(w.var(f.adultcare$DA2FHAPP, f.adultcare$DA2DWBWT),
                     w.var(f.adultcare$DA2FCALM, f.adultcare$DA2DWBWT),
                     w.var(f.adultcare$DA2FFRUS, f.adultcare$DA2DWBWT), 
                     w.var(f.adultcare$DA2FWORR, f.adultcare$DA2DWBWT),
                     w.var(f.adultcare$DA2FSAD, f.adultcare$DA2DWBWT), 
                     w.var(f.adultcare$DA2FTIRE, f.adultcare$DA2DWBWT),
                     w.var(f.adultcare$DA2FPAIN, f.adultcare$DA2DWBWT))) / sqrt(nrow(f.adultcare))
f.adult.lower <- summary[,2] - 1.96*f.adult.se
f.adult.upper <- summary[,2] + 1.96*f.adult.se

# For caregivers for children, males and females
m.child.se <- sqrt(c(w.var(m.childcare$DA2FHAPP, m.childcare$DA2DWBWT),
                     w.var(m.childcare$DA2FCALM, m.childcare$DA2DWBWT),
                     w.var(m.childcare$DA2FFRUS, m.childcare$DA2DWBWT), 
                     w.var(m.childcare$DA2FWORR, m.childcare$DA2DWBWT),
                     w.var(m.childcare$DA2FSAD, m.childcare$DA2DWBWT), 
                     w.var(m.childcare$DA2FTIRE, m.childcare$DA2DWBWT),
                     w.var(m.childcare$DA2FPAIN, m.childcare$DA2DWBWT))) / sqrt(nrow(m.childcare))
m.child.lower <- summary[,6] - 1.96*m.child.se
m.child.upper <- summary[,6] + 1.96*m.child.se

f.child.se <- sqrt(c(w.var(f.childcare$DA2FHAPP, f.childcare$DA2DWBWT),
                     w.var(f.childcare$DA2FCALM, f.childcare$DA2DWBWT),
                     w.var(f.childcare$DA2FFRUS, f.childcare$DA2DWBWT), 
                     w.var(f.childcare$DA2FWORR, f.childcare$DA2DWBWT),
                     w.var(f.childcare$DA2FSAD, f.childcare$DA2DWBWT), 
                     w.var(f.childcare$DA2FTIRE, f.childcare$DA2DWBWT),
                     w.var(f.childcare$DA2FPAIN, f.childcare$DA2DWBWT))) / sqrt(nrow(f.childcare))
f.child.lower <- summary[,5] - 1.96*f.child.se
f.child.upper <- summary[,5] + 1.96*f.child.se

# Creating tables containing mean, lower bound, and upper bound of emotion scores with 95% confidence interval
df.m.adult <- rbind(summary[,3], m.adult.lower, m.adult.upper)
df.f.adult <- rbind(summary[,2], f.adult.lower, f.adult.upper)
df.m.child <- rbind(summary[,6], m.child.lower, m.child.upper)
df.f.child <- rbind(summary[,5], f.child.lower, f.child.upper)

# Separate the means in dedicated objects
m.adult <- as_tibble(t(df.m.adult[1,]))
f.adult <- as_tibble(t(df.f.adult[1,]))
m.child <- as_tibble(t(df.m.child[1,]))
f.child <- as_tibble(t(df.f.child[1,]))
colnames(m.adult) <- colnames(f.adult) <- colnames(m.child) <-
  colnames(f.child) <- c("Happy", "Calm", "Frustrated", "Worried", "Sad", "Tired", "Pained")
  

# Compile results for plotting PSID
graph.df.psid <- as_tibble(rbind(t(df.m.child), t(df.f.child), t(df.m.adult), t(df.f.adult))) %>% 
  mutate(Sex = rep(rep(c("Male", "Female"), each = 7), 2),
         #factor(rep(rep(c("Male", "Female"), each = 7), 2), 
         #              levels = c("Male", "Female")),
         Emotion = factor(rep(c("Happy", "Calm", "Frustrated", "Worried", "Sad", "Tired", "Pained"), nrow(.)/7),
                          levels = c("Happy", "Calm", "Frustrated", "Worried", "Sad", "Tired", "Pained"), 
                          ordered = TRUE),
         Activity = factor(rep(c("Childcare", "Adult Care"), each = nrow(.)/2),
                           levels = c("Childcare", "Adult Care"),
                           ordered = TRUE)) %>% 
  setNames(., c("Mean", "Lower", "Upper", "Sex", "Emotion", "Activity")) %>% 
  select(Activity, Sex, Emotion, Mean, Lower, Upper)



# Putting the names of activities to compare in a vector
test.lst.activities.psid <- c("m.child", "m.adult", "f.child", "f.adult") 

# Defining the function that will execute t-tests
t.test.mat.fun.psid <- function(x = test.lst.activities.psid, emotions = c("Happy", "Calm", "Frustrated", "Worried", "Sad", "Tired", "Pained")) {
  
  # Preliminaries
  n.vars <- length(emotions)    # Number of emotions 
  n.activities <- length(x)     # Number of activities
  
  # Setting up the storage matrix
  t.test.mat <- matrix(NA, nrow = n.vars * n.activities^2, ncol = 13) # COLS: Emotion, Activity, ComparisonActivity, N, Activity.Mean, Comparison.Mean, SE, Difference, SD, t-stat, p-val, 95ci lower, 95ci upper
  
  iter.num <- 1
  
  for(j in 1:n.activities) {
    for (k in 1:n.activities) {
      for (i in 1:n.vars) {
        
        # Compute the t-test
        test.res <- tsum.test(mean.x = as.numeric(get(x[j])[i]), 
                              mean.y = as.numeric(get(x[k])[i]), 
                              s.x = as.numeric(get(paste0(x[j], ".se"))[i]) * sqrt(nrow(get(paste0(x[j], "care")))), # Convert SE to Standard Deviation
                              s.y = as.numeric(get(paste0(x[k], ".se"))[i]) * sqrt(nrow(get(paste0(x[k], "care")))), # Convert SE to Standard Deviation
                              n.x = nrow(get(paste0(x[k], "care"))), 
                              n.y = nrow(get(paste0(x[k], "care"))),  
                              alternative = "two.sided", 
                              var.equal = FALSE, 
                              conf.level = 0.95)
        
        # Fill in the rows of the matrix
        t.test.mat[iter.num,] <- c(emotions[i],                                                                                 # Emotion name
                                   x[j],                                                                                        # Activity type
                                   x[k],                                                                                        # Comparison Activity type
                                   nrow(get(paste0(x[j], "care"))),                                                             # Sample size of Activity type
                                   round(as.numeric(get(x[j])[i]), 3),                                                          # PSID mean of the Activity type
                                   round(as.numeric(get(x[k])[i]), 3),                                                          # PSID mean of the Comparison Activity type
                                   round(as.numeric(get(x[j])[i]) - as.numeric(get(x[k])[i]), 3),                               # Difference between the means of the two activities
                                   round(as.numeric(get(paste0(x[j], ".se"))[i]), 3),                                           # Standard Error of the Activity type
                                   round(as.numeric(get(paste0(x[j], ".se"))[i]) * sqrt(nrow(get(paste0(x[j], "care")))), 3),   # Standard Deviation of the Activity type
                                   round(as.numeric(unname(unlist(test.res)[1])), 3),                                           # t-statistic
                                   format(round(as.numeric(unname(unlist(test.res)[3])), 4), nsmall = 4),                       # p-value
                                   round(as.numeric(unname(unlist(test.res)[4])), 3),                                           # Lower 95% confidence bound
                                   round(as.numeric(unname(unlist(test.res)[5])), 3))                                           # Upper 95% confidence bound
        
        iter.num <- iter.num + 1
        
      }  
    }
  }
  
  colnames(t.test.mat) <- c("Emotion", "Activity", "Comparison.Activity", "N", "Activity.Mean", "Comparison.Mean", "Difference", "SE", "SD", "t.statistic", "p.value", "CI.lower", "CI.upper")
  t.test.mat <- t.test.mat[!(t.test.mat[, "Activity"] == t.test.mat[, "Comparison.Activity"]),]
  return(t.test.mat)
}

# Generate the table of t-tests and coefficients
t.test.matrix.psid <- t.test.mat.fun.psid()

# Export the t-statistics table
write.csv(t.test.matrix.psid, "t_tests_psid.csv", row.names = F, quote = F)


## Matrix with statistics needed to compute the indices of positivity and negativity
psid.stats <- rbind(c(unlist(graph.df.psid[1:7, "Mean"]), nrow(m.childcare)),
                    c(unlist(graph.df.psid[15:21, "Mean"]), nrow(m.adultcare)),
                    c(unlist(graph.df.psid[8:14, "Mean"]), nrow(f.childcare)),
                    c(unlist(graph.df.psid[22:28, "Mean"]), nrow(f.adultcare))) 
colnames(psid.stats) <- c("Happy", "Calm", "Frustrated", "Worried", "Sad", "Tired", "Pain", "N")
rownames(psid.stats) <- c("CC.mean.male", "AC.mean.male", "CC.mean.female", "AC.mean.female")

psid.indices <- psid.stats %>%
  as_tibble() %>% 
  mutate(Activity = c("Childcare", "Adult care", "Childcare", "Adult care"), 
         Sex = c("Male", "Male", "Female", "Female"),
         positive = (Happy + Calm + (6 - Frustrated) + (6 - Worried) + (6 - Sad) + (6 - Tired) + (6 - Pain)) / 7,
         negative = ((6 - Happy) + (6 - Calm) + Frustrated + Worried + Sad + Tired + Pain) / 7) %>% 
  select(Activity, Sex, positive, negative, N)





# ## Keep only the indices and the summary output
# rm(list = setdiff(ls(), c("psid.stats", "psid.indices", "graph.df.psid")))
# 
# 
# ## Save the workspace environment
# save.image("psid_means_childcare_adultcare_by_sex.RData")
