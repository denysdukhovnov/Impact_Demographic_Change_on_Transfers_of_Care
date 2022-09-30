# Impact_Demographic_Change_on_Transfers_of_Care
Repository for materials from the paper "The Impact of Demographic Change on Transfers of Care and Associated Well-being"

Full citation:
Dukhovnov, D., Ryan, J.M. & Zagheni, E. (2022). The Impact of Demographic Change on Transfers of Care and Associated Well-being. Population Research and Policy Review. https://doi.org/10.1007/s11113-022-09736-0

## Data
- The multi-year time use American Time Use Survey data for 2003-2015 have been downloaded from the US Bureau of Labor Statistics (https://www.bls.gov/tus/#data) and pre-filtered to cover the 2012-2013 cross-section. Required files (included as processed in this repository):

Activity file, Activity Summary file, Respondent file, Well-Being module (multiple files), Replicate Weights file

- The time use data from the Panel Study of Income Dynamics, Disability and Use of Time (DUST) 2013 Module have been extracted from https://simba.isr.umich.edu/DC/f.aspx

- The data for care transfer matrices 2011-2013 by age and sex are derived from the analysis in Dukhovnov, D., & Zagheni, E. (2015). Who takes care of whom in the United States? Time transfers by age and sex. Population and development review, 41(2), 183-206.

- Data on population projections 2010-2100 have been derived from the UN World Population Prospects (2019 version). https://population.un.org/wpp/Download/Archive/Standard/

## Reproducible R code
1) ATUS_WB_analysis_by_sex.R: analysis of ATUS and extraction of well-being scores by sex of caregiver
2) PSID_WB_analysis_by_sex.R: analysis of PSID and extraction of well-being scores by sex of caregiver
3) ATUS_PSID_pos_neg_ratios_by_sex.R: calculation of well-being indices and ratios for informal care activities by sex of caregiver
4) WB_projections_by_sex.R: projection of positively and negatively weighted aggregate informal care supply to year 2010 (CSR ratios)
5) WB_projections_by_sex_caregivers.R: projection of positively and negatively weighted aggregate informal care supply to year 2010 (CSR ratios), conditional on caregiving status
6) ATUS_PSID_plots.R: oranizing and plotting the results
