# eminent_author_per_country_weights.R
#
# Idea: To understand how much individuals contribute to the
# country-specific stats, let's calculate the proportion of
# the authorship the the top N authors carry in each country.

U<-unique(prominent$Affiliation_country)
U
country_counts <- table(df$Affiliation_country)
index <- country_counts > 60
prominent <- df[df$Affiliation_country %in% names(index)[index==TRUE],]

# Get detailed breakdown showing the top 3 names
S <- prominent %>%
  count(Affiliation_country, unique_name, name = "count") %>%
  group_by(Affiliation_country) %>%
  mutate(
    total_country = sum(count),
    proportion = count / total_country,
    rank = rank(-count, ties.method = "first")
  ) %>%
  filter(rank <= 3) %>%
  arrange(Affiliation_country, rank)

  # Optional: create summary with top 3 proportion
S %>%
  group_by(Affiliation_country) %>%
  mutate(
    top3_proportion = sum(proportion)
  ) %>%
  ungroup() -> S2
S2

print(S2, n = 72)
