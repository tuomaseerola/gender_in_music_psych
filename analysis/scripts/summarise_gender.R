# Filename: summarise_gender.R
# Author: T. Eerola, 6/4/2024
# Project: gender in music psychology
# Status: in progress

#### 1. Check for errors ------
cat("Check for errors \n")

S<-dplyr::summarise(group_by(D),n=n(),female = sum(gender=='female',na.rm = T))
S$prop <- S$female/S$n
S$male <- S$n - S$female
S<-dplyr::arrange(S,-n)

D<-ungroup(D)
tmp <- D %>%
  select(firstn,gender) %>%
  filter(gender=='female') %>%
  drop_na() %>%
  group_by(firstn) %>%
  summarise(n = n())
tmp<-dplyr::arrange(tmp,-n)
print(knitr::kable(head(tmp,5),caption='Top 5 names.'))

tmp<-dplyr::arrange(tmp,n)
print(knitr::kable(head(tmp,5),caption='Bottom 5 names.'))


#### 2. Typical stats --------

#% of female authors in this field
print(datasummary(gender + 1 ~ N + Percent(),
            data = D))
#### 3. Number of co-authors -----------


##### 4. Summaries ------------
cat("Summaries \n")

#install.packages("modelsummary",repos = "http://cran.us.r-project.org")

#% of female authors from Non-West
print(datasummary(FirstAuthorCountry_WEOG * gender + 1 ~ N + Percent(),
            data = D))
datasummary(FirstAuthorCountry_USD_Md * gender + 1 ~ N + Percent(),data = D)
datasummary(gender * FirstAuthorCountry_USD_Md + 1 ~ N + Percent(),data = D)
