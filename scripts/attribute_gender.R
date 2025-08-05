# install.packages("genderizeR")
#suppressPackageStartupMessages(library(genderizeR))

library(modelsummary)

run_genderize <- FALSE

if (run_genderize == TRUE) {
  # genderize API
  # Note: Has API limits for free usage
  my_api_key <- 'xxxx'

  first_names <- data.frame(
    Name = df$first_name,
    Country_id = df$Affiliation_country_code
  )
  head(first_names)
  write.csv(first_names, file = "data/first_names.csv", row.names = FALSE)

  # process run in website API because the function was not working in R
}
# read the results obtained from the website API
first_names_resolved <- read.csv('data/first_names_resolved.csv', header = TRUE)
df$Gender <- first_names_resolved$Gender
df$Gender.Probability <- first_names_resolved$Gender.Probability
df$Gender.Count <- first_names_resolved$Gender.Count
rm(first_names_resolved)

#hist(df$Gender.Probability)

# explore the unknowns
unattributed <- df$first_name[df$Gender == 'unknown']
write.csv(
  unattributed,
  file = "data/unattributed_first_names.csv",
  row.names = FALSE
)

# read data
inferred <- readr::read_csv(
  'data/first_names_inferred.csv',
  show_col_types = FALSE
)
#table(inferred$InferredGender)

for (k in 1:nrow(inferred)) {
  #  print(paste0("Processing ", k, " of ", nrow(inferred), " first names: ", inferred$Name[k]))
  index <- df$first_name == inferred$Name[k]
  if (sum(index) > 0) {
    df$Gender[index] <- inferred$InferredGender[k]
    df$Gender.Probability[index] <- 0.75
    df$Gender.Count[index] <- 1
  }
}

# Remove books
df <- dplyr::filter(df, CATEGORY != "BOOK")

# summarise
print(datasummary(Gender + 1 ~ N + Percent(), data = df))

# Remove unattributed gender
df <- dplyr::filter(df, Gender != 'ambiguous' & Gender != 'unknown')

cat(paste(
  "\nEntries in the author-expanded database after filtering books/unattributed:",
  nrow(df)
)) 

detach("package:modelsummary", unload=TRUE)
rm(unattributed, inferred, k, index, run_genderize)
