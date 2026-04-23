# -------------------------------------
# Script: fix_BIBTEXKEYs.R
# Author: T. Eerola
# Date: 23/04/2026
# Purpose: Address an issue with DOI and BIBTEXKEY.
# Notes: 10 BIBTEXKEYs are duplicates even though they have unique DOIs.
# -------------------------------------


#### 1 Demonstrate the problem -----

U <- unique(df$DOI)
print(length(U)) # 3383
length(unique(df$BIBTEXKEY)) # 3373!

#### 2 Find DOIs that appear with more than one BIBTEXKEY ------

duplicates <- df %>%
  distinct(BIBTEXKEY, DOI) %>%
  group_by(BIBTEXKEY) %>%
  filter(n() > 1) %>%
  arrange(BIBTEXKEY)

#### 3 Revise BIBTEXKEYs! --------

df <- df %>%
  group_by(BIBTEXKEY) %>%
  mutate(
    n_dois = n_distinct(DOI),
    doi_rank = match(DOI, unique(DOI)),  # which DOI within this BIBTEXKEY
    BIBTEXKEY = if_else(
      n_dois > 1,
      paste0(BIBTEXKEY, letters[doi_rank]),  # Cirelli2018a, Cirelli2018b
      BIBTEXKEY
    )
  ) %>%
  ungroup() %>%
  select(-n_dois, -doi_rank)


#### 4 Show that these are now equal ---------

length(unique(df$BIBTEXKEY))
length(unique(df$DOI))

rm(U,duplicates)

#print("BIBTEXKEYs fixed!")
