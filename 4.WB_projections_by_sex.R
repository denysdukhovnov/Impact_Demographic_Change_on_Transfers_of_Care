## Code for projecting matrices of "positive", "negative", and overall care transfers by sex

#if (!exists(c("index.ratio.cc.male", "index.ratio.ac.male", "index.ratio.cc.female", "index.ratio.ac.female"))) source("ATUS_PSID_pos_neg_ratios_by_sex.R")


## This requires matrices of transfers by age and sex from Dukhovnov, D. & Zagheni, E. (2015) as input.
## The structure of the set is as follows: 
## 4 matrices, one for each care transfer, in this order: Male-to-Male, Male-to-Female, Female-to-Male, and Female-to-Female.
## Each matrix has 15 rows and 18 columns, denoting 5-year age groups of caregivers 15+, and care recipients up to age 85
# Reading-in the matrices of care time transfers by age and sex 
transfers.matrices <- read.csv("care_transfer_matrices_by_age_and_sex.csv", header = FALSE, stringsAsFactors = FALSE)

transfers.array <- array(unlist(split(transfers.matrices, rep(1:4, each = 15))), 
                         dim = c(15, 18, 4))

RowAgeLab <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
               "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
ColAgeLab <- c("0-4", "5-9", "10-14", RowAgeLab)

# Labeling the transfer array
rownames(transfers.array) <- RowAgeLab
colnames(transfers.array) <- ColAgeLab
dimnames(transfers.array)[[3]] <- c("Male.to.Male", "Male.to.Female", "Female.to.Male", "Female.to.Female")


## The combined ATUS and PSID positivity and negativity ratios for both childcare and adult care are used to multiply 
## the rates of time transfers by age and sex, producing one set of positive and another set of negative matrices (each representing
## transfers by age and sex from caregivers to care recipients (Male-to-Male, Male-to-Female, Female-to-Male, and Female-to-Female,
## respectively). Note: Only matrix columns associated with 0-14 year old care recipients were multiplied by the childcare ratio, 
## whereas only columns for care recipients aged 15-85+ are multiplied are multiplied by the adult care ratios.
## 
## The marginals of the resultant positive and negative matrices represent the total amount of "positive" and "negative" time
## transferred daily on average from a particular age-sex group of caregivers or care recipients.

## Sex-specific arrays of positive and negative average daily care time transfer rates, by age and sex
pos.transfers.array.male <- neg.transfers.array.male <-
  pos.transfers.array.female <- neg.transfers.array.female <- array(NA, dim = c(15, 18, 2))

pos.transfers.array.male[, 1:3, ] <- transfers.array[, 1:3, 1:2] * index.ratio.cc.male[2,1]
pos.transfers.array.male[, 4:18, ] <- transfers.array[, 4:18, 1:2] * index.ratio.ac.male[2,1]
neg.transfers.array.male[, 1:3, ] <- transfers.array[, 1:3, 1:2] * index.ratio.cc.male[2,2]
neg.transfers.array.male[, 4:18, ] <- transfers.array[, 4:18, 1:2] * index.ratio.ac.male[2,2]

pos.transfers.array.female[, 1:3, ] <- transfers.array[, 1:3, 3:4] * index.ratio.cc.female[2,1]
pos.transfers.array.female[, 4:18, ] <- transfers.array[, 4:18, 3:4] * index.ratio.ac.female[2,1]
neg.transfers.array.female[, 1:3, ] <- transfers.array[, 1:3, 3:4] * index.ratio.cc.female[2,2]
neg.transfers.array.female[, 4:18, ] <- transfers.array[, 4:18, 3:4] * index.ratio.ac.female[2,2]


## Overall positive and negative transfer matrices by age and sex
pos.overall.mat.male <- apply(pos.transfers.array.male, MARGIN = c(1,2), sum)
pos.overall.mat.female <- apply(pos.transfers.array.female, MARGIN = c(1,2), sum)
pos.overall.mat <- apply(pos.transfers.array.male, MARGIN = c(1,2), sum) + apply(pos.transfers.array.female, MARGIN = c(1,2), sum)
neg.overall.mat.male <- apply(neg.transfers.array.male, MARGIN = c(1,2), sum)
neg.overall.mat.female <- apply(neg.transfers.array.female, MARGIN = c(1,2), sum)
neg.overall.mat <- apply(neg.transfers.array.male, MARGIN = c(1,2), sum) + apply(neg.transfers.array.female, MARGIN = c(1,2), sum)
overall.mat <- apply(transfers.array, MARGIN = c(1,2), sum)

dimnames(pos.overall.mat.male) <- dimnames(pos.overall.mat.female) <- dimnames(pos.overall.mat) <- 
  dimnames(neg.overall.mat.male) <- dimnames(neg.overall.mat.female) <- dimnames(neg.overall.mat) <- 
  dimnames(overall.mat) <- list(dimnames(transfers.array)[[1]], dimnames(transfers.array)[[2]])

## Marginal profiles (sums) of average daily positive and negative time transferred
## by age of caregiver (production) or age group of care recipient (consumption)
## sex-specific and overall.
## NOTE: zeros added to the first 3 elements of marginal profiles below to reflect 0 time 
## "produced" by the age categories of children under 15

pos.male.prod.marg <- c(0, 0, 0, rowSums(pos.transfers.array.male[,,1]) + rowSums(pos.transfers.array.male[,,2]))
pos.female.prod.marg <- c(0, 0, 0, rowSums(pos.transfers.array.female[,,1]) + rowSums(pos.transfers.array.female[,,2]))
neg.male.prod.marg <- c(0, 0, 0, rowSums(neg.transfers.array.male[,,1]) + rowSums(neg.transfers.array.male[,,2]))
neg.female.prod.marg <- c(0, 0, 0, rowSums(neg.transfers.array.female[,,1]) + rowSums(neg.transfers.array.female[,,2]))

pos.male.cons.marg <- colSums(pos.transfers.array.male[,,1]) + colSums(pos.transfers.array.male[,,2])
pos.female.cons.marg <- colSums(pos.transfers.array.female[,,1]) + colSums(pos.transfers.array.female[,,2])
neg.male.cons.marg <- colSums(neg.transfers.array.male[,,1]) + colSums(neg.transfers.array.male[,,2])
neg.female.cons.marg <- colSums(neg.transfers.array.female[,,1]) + colSums(neg.transfers.array.female[,,2])

overall.male.prod.marg <- c(0, 0, 0, rowSums(transfers.array[,,1]) + rowSums(transfers.array[,,2]))
overall.female.prod.marg <- c(0, 0, 0, rowSums(transfers.array[,,3]) + rowSums(transfers.array[,,4]))
overall.male.cons.marg <- colSums(transfers.array[,,1]) + colSums(transfers.array[,,2])
overall.female.cons.marg <- colSums(transfers.array[,,3]) + colSums(transfers.array[,,4])


## Overall productions and consumption profiles (non-emotion specific)
total.prod.marg <- rowSums(overall.mat)
total.cons.marg <- colSums(overall.mat)



## To project the above rates into the future, use United Nations World Population Prospects
## population data by age and sex composition. Here, the extracted data represent 
## the US medium-fertility variant projection (UN WPP 2019) for data 2015-2100, by sex
## and by 5-year age groups, 0-85+ (manually top-coded)

## Load the UN population estimates (years 1950-2010) and medium-fertility future projections (2015-2100)
## for males and females ages 0-85+, by 5-year age groups and storing these in an array by sex.
pop.matrices <- read.csv("UN_pop_estimates_and_pop_projections.csv",
                          header = FALSE, 
                          skip = 1, 
                          stringsAsFactors = FALSE)[,3:20]

pop.array <- array(unlist(split(pop.matrices, rep(1:2, each = 31))),
                   dim = c(31, 18, 2))


## PROJECTION: applying "positivity" and "negativity" rates from the matrices by age and sex to the population structure 1950-2100
pos.poplvl.male.time.prod <- t(t(pop.array[,,1]) * pos.male.prod.marg)
pos.poplvl.female.time.prod <- t(t(pop.array[,,2]) * pos.female.prod.marg)
neg.poplvl.male.time.prod <- t(t(pop.array[,,1]) * neg.male.prod.marg)
neg.poplvl.female.time.prod <- t(t(pop.array[,,2]) * neg.female.prod.marg)

pos.poplvl.male.time.cons <- t(t(pop.array[,,1]) * pos.male.cons.marg)
pos.poplvl.female.time.cons <- t(t(pop.array[,,2]) * pos.female.cons.marg)
neg.poplvl.male.time.cons <- t(t(pop.array[,,1]) * neg.male.cons.marg)
neg.poplvl.female.time.cons <- t(t(pop.array[,,2]) * neg.female.cons.marg)

overall.poplvl.male.time.prod <- t(t(pop.array[,,1]) * overall.male.prod.marg)
overall.poplvl.female.time.prod <- t(t(pop.array[,,2]) * overall.female.prod.marg)
overall.poplvl.male.time.cons <- t(t(pop.array[,,1]) * overall.male.cons.marg)
overall.poplvl.female.time.cons <- t(t(pop.array[,,2]) * overall.female.cons.marg)

## Calculate the adjustment factors for positive and negative projection matrices
## to align per-capita average production to consumption in the baseline year (baseline year is 2010)
pop.male.2010 <- pop.array[13,,1]    # array index for 2010 male population
pop.female.2010 <- pop.array[13,,2]  # array index for 2010 female population

pos.adj.factor.male <- sum(pos.male.cons.marg * pop.male.2010) / sum(pos.male.prod.marg * pop.male.2010 + pos.female.prod.marg * pop.female.2010)
pos.adj.factor.female <- sum(pos.female.cons.marg * pop.female.2010) / sum(pos.male.prod.marg * pop.male.2010 + pos.female.prod.marg * pop.female.2010)
pos.adj.factor <- sum(pos.male.cons.marg * pop.male.2010 + pos.female.cons.marg * pop.female.2010) / sum(pos.male.prod.marg * pop.male.2010 + pos.female.prod.marg * pop.female.2010)

neg.adj.factor.male <- sum(neg.male.cons.marg * pop.male.2010) / sum(neg.male.prod.marg * pop.male.2010 + neg.female.prod.marg * pop.female.2010)
neg.adj.factor.female <- sum(neg.female.cons.marg * pop.female.2010) / sum(neg.male.prod.marg * pop.male.2010 + neg.female.prod.marg * pop.female.2010)
neg.adj.factor <- sum(neg.male.cons.marg * pop.male.2010 + neg.female.cons.marg * pop.female.2010) / sum(neg.male.prod.marg * pop.male.2010 + neg.female.prod.marg * pop.female.2010)

overall.adj.factor.male <- sum(overall.male.cons.marg * pop.male.2010) / sum(overall.male.prod.marg * pop.male.2010 + overall.female.prod.marg * pop.female.2010)
overall.adj.factor.female <- sum(overall.female.cons.marg * pop.female.2010) / sum(overall.male.prod.marg * pop.male.2010 + overall.female.prod.marg * pop.female.2010)
overall.adj.factor <- sum(overall.male.cons.marg * pop.male.2010 + overall.female.cons.marg * pop.female.2010) / sum(overall.male.prod.marg * pop.male.2010 + overall.female.prod.marg * pop.female.2010)


## Overall amount of projected positive and negative time produced and consumed (adjusted) by year, for each sex (caregiver) and overall, 1950-2100.
male.poplvl.pos.prod <- rowSums(pos.poplvl.male.time.prod)
female.poplvl.pos.prod <- rowSums(pos.poplvl.female.time.prod)
total.poplvl.pos.prod <- rowSums(pos.poplvl.male.time.prod) + rowSums(pos.poplvl.female.time.prod)
male.poplvl.neg.prod <- rowSums(neg.poplvl.male.time.prod)
female.poplvl.neg.prod <- rowSums(neg.poplvl.female.time.prod)
total.poplvl.neg.prod <- rowSums(neg.poplvl.male.time.prod) + rowSums(neg.poplvl.female.time.prod)

male.poplvl.pos.cons <- rowSums(pos.poplvl.male.time.cons) / pos.adj.factor.male
female.poplvl.pos.cons <- rowSums(pos.poplvl.female.time.cons) / pos.adj.factor.female
total.poplvl.pos.cons <- (rowSums(pos.poplvl.male.time.cons) + rowSums(pos.poplvl.female.time.cons)) / pos.adj.factor
male.poplvl.neg.cons <- rowSums(neg.poplvl.male.time.cons) / neg.adj.factor.male
female.poplvl.neg.cons <- rowSums(neg.poplvl.female.time.cons) / neg.adj.factor.female
total.poplvl.neg.cons <- (rowSums(neg.poplvl.male.time.cons) + rowSums(neg.poplvl.female.time.cons)) / neg.adj.factor

male.poplvl.prod <- rowSums(overall.poplvl.male.time.prod)
female.poplvl.prod <- rowSums(overall.poplvl.female.time.prod)
total.poplvl.prod <- rowSums(overall.poplvl.male.time.prod) + rowSums(overall.poplvl.female.time.prod)
male.poplvl.cons <- rowSums(overall.poplvl.male.time.cons) / overall.adj.factor.male
male.poplvl.cons <- rowSums(overall.poplvl.female.time.cons) / overall.adj.factor.female
total.poplvl.cons <- (rowSums(overall.poplvl.male.time.cons) + rowSums(overall.poplvl.female.time.cons)) / overall.adj.factor

  
## CARE SUPPORT RATIOS (CSR)
## NOTE: Positive and Negative CSR are weighted by the TOTAL adjusted consumption and thus add up to the TOTAL CSR
## It is, however, possible to generate Positive and Negative CSR, independent of the total, by replacing the
## denominator below (total adjusted consumption by year) by the feeling-specific one 
## (e.g. "total.poplvl.pos.cons" instead of the general "total.poplvl.cons")
CSR.pos.male <- as_tibble(cbind(Year = seq(1950, 2100, 5), CSR = male.poplvl.pos.prod/male.poplvl.cons, Ratio = 1, Sex = 1))
CSR.neg.male <- as_tibble(cbind(Year = seq(1950, 2100, 5), CSR = male.poplvl.neg.prod/male.poplvl.cons, Ratio = 2, Sex = 1))
CSR.total.male <- as_tibble(cbind(Year = seq(1950, 2100, 5), CSR = male.poplvl.prod/male.poplvl.cons, Ratio = 3, Sex = 1))

CSR.pos.female <- as_tibble(cbind(Year = seq(1950, 2100, 5), CSR = female.poplvl.pos.prod/male.poplvl.cons, Ratio = 1, Sex = 2))
CSR.neg.female <- as_tibble(cbind(Year = seq(1950, 2100, 5), CSR = female.poplvl.neg.prod/male.poplvl.cons, Ratio = 2, Sex = 2))
CSR.total.female <- as_tibble(cbind(Year = seq(1950, 2100, 5), CSR = female.poplvl.prod/male.poplvl.cons, Ratio = 3, Sex = 2))

CSR.pos <- as_tibble(cbind(Year = seq(1950, 2100, 5), CSR = total.poplvl.pos.prod/total.poplvl.cons, Ratio = 1, Sex = 3))
CSR.neg <- as_tibble(cbind(Year = seq(1950, 2100, 5), CSR = total.poplvl.neg.prod/total.poplvl.cons, Ratio = 2, Sex = 3))
CSR.total <- as_tibble(cbind(Year = seq(1950, 2100, 5), CSR = total.poplvl.prod/total.poplvl.cons, Ratio = 3, Sex = 3))
