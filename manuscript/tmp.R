library(here)
here::i_am("manuscript/manuscript.qmd")


source(here("scripts","load_gender_data.R"))
library(dplyr)
s <- summarise(group_by(df, BIBTEXKEY), n = n())
df <- mutate(group_by(df, BIBTEXKEY), author_N = max(author_order))

s

s2 <- summarise(group_by(df, BIBTEXKEY,Gender), n = n(),.groups="drop")

# get CI
S <- s2 %>%
  dplyr::group_by(Gender) %>%
  dplyr::summarise(N=n(),m=mean(n,na.rm = TRUE),sd=sd(n,na.rm = TRUE),.groups = 'drop') %>%
  dplyr::mutate(se=sd/sqrt(N),LCI=m+qnorm(0.025)*se,UCI=m+qnorm(0.975)*se)

T <- t.test(s2$n~s2$Gender)
