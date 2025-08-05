# load_scopus_datasets.R

df1 <- bib2df::bib2df("data/MetS2000_onwards.bib",separate_names = TRUE)
stopifnot(nrow(df1)==298)
df2 <- bib2df::bib2df("data/JNMR2000_onwards.bib",separate_names = TRUE)
stopifnot(nrow(df2)==597)
df3 <- bib2df::bib2df("data/MP2000_onwards.bib",separate_names = TRUE)
stopifnot(nrow(df3)==745)
df4 <- bib2df::bib2df("data/MS2000_onwards.bib",separate_names = TRUE)
stopifnot(nrow(df4)==682)
df5 <- bib2df::bib2df("data/POM2000_onwards.bib",separate_names = TRUE)
stopifnot(nrow(df5)==1604)
d<-rbind(df1,df2,df3,df4,df5)
rm(df1,df2,df3,df4,df5)

cat(paste("\nEntries in the merged database:", nrow(d)))

#### Deal with authors, now nested into AUTHOR column 
library(tidyr)
library(dplyr)

df <- d %>%
  unnest(AUTHOR)

head(df)
cat(paste("\nEntries in the author-expanded database:", nrow(df))) # 9922 authors

# check what type of articles have missing names
df$TYPE[is.na(df$first_name)]

# filter Erratum
df <- df %>%
  filter(!str_detect(TYPE, "Erratum|Corrigendum|Retraction|Announcement|Editorial"))
cat(paste("\nEntries in the author-expanded database after filtering editorials etc:", nrow(df))) # 9662 authors

# Filter if full name is NA
df <- df %>%
  filter(!is.na(full_name))
cat(paste("\nEntries in the author-expanded database after filtering empty:", nrow(df))) # 9651 authors

# maybe remove those that are just initials

df$AFFILIATIONS <- df$AFFILIATIONS %>%
  stringr::str_replace_all("Australia, Australia", "Australia") %>%
  stringr::str_replace_all("Germany, Germany", "Germany") %>%
  stringr::str_replace_all("Australia,Australia", "Australia") %>%
  stringr::str_replace_all("Germany,Germany", "Germany") %>%
  stringr::str_replace_all("United States, Brunswick", "") %>%
  stringr::str_replace_all("United States, Georgia", "") %>%
  stringr::str_replace_all("Atlanta, United States", "") %>%
  
  stringr::str_replace_all("Oldenburg, Oldenburg, Germany", "Oldenburg, Germany") %>%
  stringr::str_replace_all("Italy, Italy", "Italy") %>%
  stringr::str_replace_all("Parma, Parma, Italy", "Parma, Italy")

#### Check categories
#table(df$TYPE)

#### Delete empty columns
df <- df %>%
  select(where(~!all(is.na(.))))

df <- dplyr::select(df,-suffix)

