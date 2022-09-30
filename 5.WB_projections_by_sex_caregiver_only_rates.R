## Code for projecting matrices of "positive", "negative", and overall care transfers by sex (CAREGIVER SAMPLE)

#if (!exists(c("index.ratio.cc.male", "index.ratio.ac.male", "index.ratio.cc.female", "index.ratio.ac.female"))) source("ATUS_PSID_pos_neg_ratios_by_sex.R")

library(tidyverse)
library(reshape2)


## This requires matrices of transfers by age and sex from Dukhovnov, D. & Zagheni, E. (2015) as input.
## The structure of the set is as follows: 
## 4 matrices, one for each care transfer, in this order: Male-to-Male, Male-to-Female, Female-to-Male, and Female-to-Female.
## Each matrix has 15 rows and 18 columns, denoting 5-year age groups of caregivers 15+, and care recipients up to age 85
# Reading-in the matrices of care time transfers by age and sex 
transfers.matrices.cg <- read.csv("care_transfer_matrices_by_age_and_sex_caregivers_only.csv", header = FALSE, stringsAsFactors = FALSE)

transfers.array.cg <- array(unlist(split(transfers.matrices.cg, rep(1:4, each = 15))), 
                         dim = c(15, 18, 4))

RowAgeLab <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
               "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
ColAgeLab <- c("0-4", "5-9", "10-14", RowAgeLab)

# Labeling the transfer array
rownames(transfers.array.cg) <- RowAgeLab
colnames(transfers.array.cg) <- ColAgeLab
dimnames(transfers.array.cg)[[3]] <- c("Male.to.Male", "Male.to.Female", "Female.to.Male", "Female.to.Female")


## The combined ATUS and PSID positivity and negativity ratios for both childcare and adult care are used to multiply 
## the rates of time transfers by age and sex, producing one set of positive and another set of negative matrices (each representing
## transfers by age and sex from caregivers to care recipients (Male-to-Male, Male-to-Female, Female-to-Male, and Female-to-Female,
## respectively). Note: Only matrix columns associated with 0-14 year old care recipients were multiplied by the childcare ratio, 
## whereas only columns for care recipients aged 15-85+ are multiplied are multiplied by the adult care ratios.
## 
## The marginals of the resultant positive and negative matrices represent the total amount of "positive" and "negative" time
## transferred daily on average from a particular age-sex group of caregivers or care recipients.

## Sex-specific arrays of positive and negative average daily care time transfer rates, by age and sex
pos.transfers.array.male.cg <- neg.transfers.array.male.cg <-
  pos.transfers.array.female.cg <- neg.transfers.array.female.cg <- array(NA, dim = c(15, 18, 2))

pos.transfers.array.male.cg[, 1:3, ] <- transfers.array.cg[, 1:3, 1:2] * index.ratio.cc.male[2,1]
pos.transfers.array.male.cg[, 4:18, ] <- transfers.array.cg[, 4:18, 1:2] * index.ratio.ac.male[2,1]
neg.transfers.array.male.cg[, 1:3, ] <- transfers.array.cg[, 1:3, 1:2] * index.ratio.cc.male[2,2]
neg.transfers.array.male.cg[, 4:18, ] <- transfers.array.cg[, 4:18, 1:2] * index.ratio.ac.male[2,2]

pos.transfers.array.female.cg[, 1:3, ] <- transfers.array.cg[, 1:3, 3:4] * index.ratio.cc.female[2,1]
pos.transfers.array.female.cg[, 4:18, ] <- transfers.array.cg[, 4:18, 3:4] * index.ratio.ac.female[2,1]
neg.transfers.array.female.cg[, 1:3, ] <- transfers.array.cg[, 1:3, 3:4] * index.ratio.cc.female[2,2]
neg.transfers.array.female.cg[, 4:18, ] <- transfers.array.cg[, 4:18, 3:4] * index.ratio.ac.female[2,2]


## Overall positive and negative transfer matrices by age and sex
pos.overall.mat.male.cg <- apply(pos.transfers.array.male.cg, MARGIN = c(1,2), sum)
pos.overall.mat.female.cg <- apply(pos.transfers.array.female.cg, MARGIN = c(1,2), sum)
pos.overall.mat.cg <- apply(pos.transfers.array.male.cg, MARGIN = c(1,2), sum) + apply(pos.transfers.array.female.cg, MARGIN = c(1,2), sum)
neg.overall.mat.male.cg <- apply(neg.transfers.array.male.cg, MARGIN = c(1,2), sum)
neg.overall.mat.female.cg <- apply(neg.transfers.array.female.cg, MARGIN = c(1,2), sum)
neg.overall.mat.cg <- apply(neg.transfers.array.male.cg, MARGIN = c(1,2), sum) + apply(neg.transfers.array.female.cg, MARGIN = c(1,2), sum)
overall.mat.cg <- apply(transfers.array.cg, MARGIN = c(1,2), sum)

dimnames(pos.overall.mat.male.cg) <- dimnames(pos.overall.mat.female.cg) <- dimnames(pos.overall.mat.cg) <- 
  dimnames(neg.overall.mat.male.cg) <- dimnames(neg.overall.mat.female.cg) <- dimnames(neg.overall.mat.cg) <- 
  dimnames(overall.mat.cg) <- list(dimnames(transfers.array.cg)[[1]], dimnames(transfers.array.cg)[[2]])

## Marginal profiles (sums) of average daily positive and negative time transferred
## by age of caregiver (production) or age group of care recipient (consumption)
## sex-specific and overall.
## NOTE: zeros added to the first 3 elements of marginal profiles below to reflect 0 time 
## "produced" by the age categories of children under 15

pos.male.prod.marg.cg <- c(0, 0, 0, rowSums(pos.transfers.array.male.cg[,,1]) + rowSums(pos.transfers.array.male.cg[,,2]))
pos.female.prod.marg.cg <- c(0, 0, 0, rowSums(pos.transfers.array.female.cg[,,1]) + rowSums(pos.transfers.array.female.cg[,,2]))
neg.male.prod.marg.cg <- c(0, 0, 0, rowSums(neg.transfers.array.male.cg[,,1]) + rowSums(neg.transfers.array.male.cg[,,2]))
neg.female.prod.marg.cg <- c(0, 0, 0, rowSums(neg.transfers.array.female.cg[,,1]) + rowSums(neg.transfers.array.female.cg[,,2]))

pos.male.cons.marg.cg <- colSums(pos.transfers.array.male.cg[,,1]) + colSums(pos.transfers.array.male.cg[,,2])
pos.female.cons.marg.cg <- colSums(pos.transfers.array.female.cg[,,1]) + colSums(pos.transfers.array.female.cg[,,2])
neg.male.cons.marg.cg <- colSums(neg.transfers.array.male.cg[,,1]) + colSums(neg.transfers.array.male.cg[,,2])
neg.female.cons.marg.cg <- colSums(neg.transfers.array.female.cg[,,1]) + colSums(neg.transfers.array.female.cg[,,2])

overall.male.prod.marg.cg <- c(0, 0, 0, rowSums(transfers.array.cg[,,1]) + rowSums(transfers.array.cg[,,2]))
overall.female.prod.marg.cg <- c(0, 0, 0, rowSums(transfers.array.cg[,,3]) + rowSums(transfers.array.cg[,,4]))
overall.male.cons.marg.cg <- colSums(transfers.array.cg[,,1]) + colSums(transfers.array.cg[,,2])
overall.female.cons.marg.cg <- colSums(transfers.array.cg[,,3]) + colSums(transfers.array.cg[,,4])


## Overall productions and consumption profiles (non-emotion specific)
total.prod.marg.cg <- rowSums(overall.mat.cg)
total.cons.marg.cg <- colSums(overall.mat.cg)

