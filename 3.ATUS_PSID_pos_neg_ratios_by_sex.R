## Loading or executing the main scripts

# ifelse(file.exists("atus_means_childcare_adultcare.RData"), load("atus_means_childcare_adultcare_by_sex.RData"),
#        if (!exists("atus.indices")) source("ATUS_WB_analysis_by_sex.R"))
# 
# ifelse(file.exists("psid_means_childcare_adultcare.RData"), load("psid_means_childcare_adultcare_by_sex.RData"), 
#        if (!exists("psid.indices")) source("PSID_WB_analysis_by_sex.R"))

## Combine the matrices
pos.neg.indices <- atus.indices %>% 
  bind_rows(psid.indices) %>% 
  mutate(data.source = c("ATUS", "ATUS", "ATUS", "ATUS",
                         "PSID", "PSID", "PSID", "PSID"))

## Create index and ratio for positivity and negativity for childcare by sex
index.ratio.cc.male <- pos.neg.indices %>%
  slice(1,5) %>% 
  mutate(CC.pos.male.index = sum(positive*N)/sum(N),
         CC.neg.male.index = sum(negative*N)/sum(N)) %>% 
  mutate(CC.pos.male.ratio = CC.pos.male.index/(CC.pos.male.index + CC.neg.male.index),
         CC.neg.male.ratio = CC.neg.male.index/(CC.pos.male.index + CC.neg.male.index)) %>% 
  slice(1) %>% 
  select(CC.pos.male.index, CC.neg.male.index, CC.pos.male.ratio, CC.neg.male.ratio) %>% 
  as.matrix() %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  `colnames<-`(c("Positive", "Negative")) %>% 
  `rownames<-`(c("Mean index", "Ratio"))


index.ratio.cc.female <- pos.neg.indices %>%
  slice(2,6) %>% 
  mutate(CC.pos.female.index = sum(positive*N)/sum(N),
         CC.neg.female.index = sum(negative*N)/sum(N)) %>% 
  mutate(CC.pos.female.ratio = CC.pos.female.index/(CC.pos.female.index + CC.neg.female.index),
         CC.neg.female.ratio = CC.neg.female.index/(CC.pos.female.index + CC.neg.female.index)) %>% 
  slice(1) %>% 
  select(CC.pos.female.index, CC.neg.female.index, CC.pos.female.ratio, CC.neg.female.ratio) %>% 
  as.matrix() %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  `colnames<-`(c("Positive", "Negative")) %>% 
  `rownames<-`(c("Mean index", "Ratio"))


## Create index and ratio for positivity and negativity for adult care, by sex
index.ratio.ac.male <- pos.neg.indices %>%
  slice(3,7) %>% 
  mutate(AC.pos.male.index = sum(positive*N)/sum(N),
         AC.neg.male.index = sum(negative*N)/sum(N)) %>% 
  mutate(AC.pos.male.ratio = AC.pos.male.index/(AC.pos.male.index + AC.neg.male.index),
         AC.neg.male.ratio = AC.neg.male.index/(AC.pos.male.index + AC.neg.male.index)) %>% 
  slice(1) %>% 
  select(AC.pos.male.index, AC.neg.male.index, AC.pos.male.ratio, AC.neg.male.ratio) %>% 
  as.matrix() %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  `colnames<-`(c("Positive", "Negative")) %>% 
  `rownames<-`(c("Mean index", "Ratio"))


index.ratio.ac.female <- pos.neg.indices %>%
  slice(4,8) %>% 
  mutate(AC.pos.female.index = sum(positive*N)/sum(N),
         AC.neg.female.index = sum(negative*N)/sum(N)) %>% 
  mutate(AC.pos.female.ratio = AC.pos.female.index/(AC.pos.female.index + AC.neg.female.index),
         AC.neg.female.ratio = AC.neg.female.index/(AC.pos.female.index + AC.neg.female.index)) %>% 
  slice(1) %>% 
  select(AC.pos.female.index, AC.neg.female.index, AC.pos.female.ratio, AC.neg.female.ratio) %>% 
  as.matrix() %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  `colnames<-`(c("Positive", "Negative")) %>% 
  `rownames<-`(c("Mean index", "Ratio"))


index.ratio.cc.male
index.ratio.ac.male
index.ratio.cc.female
index.ratio.ac.female

