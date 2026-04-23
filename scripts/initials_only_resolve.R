# -------------------------------------
# Script: initials_only_resolve.R
# Author: T. Eerola
# Date: 23/04/2026
# Purpose: Resolve those authors who only have initials.
# Notes: Required manual labour, the resolved names are
#  gender_errors_fixed.csv
# -------------------------------------

writefile <- FALSE

# how frequent, calculate those that match "M." or missing etc.
tmp <- dplyr::select(df, first_name, middle_name, Gender, full_name, DOI)
sum(table(tmp$Gender)) / nrow(tmp)
sum(is.na(tmp$Gender))
sum(is.na(tmp$first_name))
# if first name is initial (or missing)
tmp2 <- dplyr::filter(
  tmp,
  stringr::str_detect(tmp$first_name, "[A-Z]\\.") | is.na(tmp$first_name)
)
#nrow(tmp2)
# if first and middle name is initial
tmp3 <- dplyr::filter(
  tmp2,
  stringr::str_detect(tmp2$middle_name, "[A-Z]\\.") | is.na(tmp2$middle_name)
)
#nrow(tmp3) # 106 and there are mistakes!
if (writefile == TRUE) {
  write.csv(
    x = tmp3,
    file = "data/potential_gender_errors.csv",
    row.names = FALSE
  )
}

#### Manually explore and then read the fixes ----
fixes <- read.csv("data/gender_errors_fixed.csv", header = TRUE)
# how many gender were different
print(paste("Changed entries:", sum(tmp3$Gender != fixes$Gender))) # 24
print(table(fixes$Gender[tmp3$Gender != fixes$Gender])) # 19 female, 4 unresolved


# put the fixes back to data
# take only the changed entries
fixes_differences <- fixes[tmp3$Gender != fixes$Gender, ]

for (k in seq_len(nrow(fixes_differences))) {
  x <- which(df$full_name == fixes_differences$full_name[k])
  df$first_name[x] <- fixes_differences$first_name[k]
  df$middle_name[x] <- fixes_differences$middle_name[k]
  df$Gender[x] <- fixes_differences$Gender[k]
}

# Finally, remove those which cannot be resolved (4 entries)
df <- dplyr::filter(df,Gender!="unresolved")

rm(tmp, tmp2, tmp3, fixes, fixes_differences,writefile,x,k)

