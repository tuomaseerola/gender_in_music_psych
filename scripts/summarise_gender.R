# Filename: summarise_gender.R
# Author: T. Eerola, 6/4/2024
# Project: gender in music psychology
# Status: in progress
invisible(library(modelsummary))

#### 1. Gender distribution ------

# % of female authors in this field
print(datasummary(Gender + 1 ~ N + Percent(), data = df,output='markdown'))

tmp <- df %>%
  select(first_name, Gender) %>%
  filter(Gender == 'female') %>%
  drop_na() %>%
  group_by(first_name) %>%
  summarise(n = n())
tmp <- dplyr::arrange(tmp, -n)
print(knitr::kable(head(tmp, 5), caption = 'Top 5 names.'))
rm(tmp)

#### 3. Number of co-authors -----------
#print(datasummary(factor(author_order) + 1 ~ Gender + N + Percent(), data = df))

##### NEW SUMMARIES ------
# How many author on average
cat("\nNumber of coauthors:\n")
library(dplyr)
s <- summarise(group_by(df, BIBTEXKEY), n = n())
cat(paste("\n median:",median(s$n)))
cat(paste("\n mean:",round(mean(s$n),3)))
cat(paste("\n sd:",round(sd(s$n),3)))
cat(paste("\n max:",round(max(s$n),3)))
cat("\n")
rm(s)


detach("package:modelsummary", unload=TRUE)
