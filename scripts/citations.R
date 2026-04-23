# citations.R

#### Initial revision as suggested by the reviewer 2: Adjust the number of citations by year.
df <- df %>%
  mutate(
    years_since_pub = 2026 - YEAR,
    Citations_per_year = Citations / years_since_pub
  )


df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'First'
df$authortype[df$author_last == 1] <- 'Other'
#table(df$authortype)

tmp<-dplyr::filter(df,authortype=='First')
citestats_first <- summarise(group_by(tmp, Gender), Md = median(Citations_per_year), M = mean(Citations_per_year),Q75=quantile(Citations_per_year,0.75,na.rm=TRUE))
stats_first <- broom::glance(kruskal.test(Citations_per_year ~ Gender, data=tmp))
citestats_first$CI_lower<-c(0,0)
citestats_first$CI_upper<-c(0,0)
citestats_first$CI_lower[1]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='female'], conf.level = 0.95)[2]
citestats_first$CI_upper[1]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='female'], conf.level = 0.95)[3]
citestats_first$CI_lower[2]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='male'], conf.level = 0.95)[2]
citestats_first$CI_upper[2]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='male'], conf.level = 0.95)[3]

# classify order: Coauthor
df$authortype <- 'Coauthor'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Other'

tmp <- dplyr::filter(df,authortype=='Coauthor')
citestats_coauthor <- summarise(group_by(tmp, Gender), Md = median(Citations_per_year), M = mean(Citations_per_year),Q75=quantile(Citations_per_year,0.75))
stats_coauthor <- broom::glance(kruskal.test(Citations_per_year ~ Gender, data=tmp))
citestats_coauthor$CI_lower<-c(0,0)
citestats_coauthor$CI_upper<-c(0,0)
citestats_coauthor$CI_lower[1]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='female'], conf.level = 0.95)[2]
citestats_coauthor$CI_upper[1]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='female'], conf.level = 0.95)[3]
citestats_coauthor$CI_lower[2]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='male'], conf.level = 0.95)[2]
citestats_coauthor$CI_upper[2]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='male'], conf.level = 0.95)[3]

# classify order: Last
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Last'

tmp <- dplyr::filter(df,authortype=='Last')
citestats_last <- summarise(group_by(tmp, Gender), Md = median(Citations_per_year), M = mean(Citations_per_year),Q75=quantile(Citations_per_year,0.75))
stats_last <- broom::glance(kruskal.test(Citations_per_year ~ Gender, data=tmp))
citestats_last$CI_lower<-c(0,0)
citestats_last$CI_upper<-c(0,0)
citestats_last$CI_lower[1]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='female'], conf.level = 0.95)[2]
citestats_last$CI_upper[1]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='female'], conf.level = 0.95)[3]
citestats_last$CI_lower[2]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='male'], conf.level = 0.95)[2]
citestats_last$CI_upper[2]<-DescTools::MedianCI(tmp$Citations_per_year[tmp$Gender=='male'], conf.level = 0.95)[3]

## Across all authors
citestats_all <- summarise(group_by(df, Gender), Md = median(Citations_per_year), M = mean(Citations_per_year),Q75=quantile(Citations_per_year,0.75))
stats_all <- broom::glance(kruskal.test(Citations_per_year ~ Gender, data=df))
citestats_all$CI_lower<-c(0,0)
citestats_all$CI_upper<-c(0,0)
citestats_all$CI_lower[1]<-DescTools::MedianCI(df$Citations_per_year[tmp$Gender=='female'], conf.level = 0.95)[2]
citestats_all$CI_upper[1]<-DescTools::MedianCI(df$Citations_per_year[tmp$Gender=='female'], conf.level = 0.95)[3]
citestats_all$CI_lower[2]<-DescTools::MedianCI(df$Citations_per_year[tmp$Gender=='male'], conf.level = 0.95)[2]
citestats_all$CI_upper[2]<-DescTools::MedianCI(df$Citations_per_year[tmp$Gender=='male'], conf.level = 0.95)[3]

#### NEW --------------
# Reviewer question:
# Main manuscript: Also, this difference in citation depending on gender could be further
# investigated by analysing whether the number of citations of a first authored male or
# female increases or decreases with the gender of the senior co-author? If that also shows a
# gender bias, I would say the abstract can talk about a small but significant gender difference of gender.
#This assumes that the gender of first author can be counter-balanced by the gender of a senior person.
#
#
# Take out the middle authors and define first and last authors
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'first'
df$authortype[df$author_last == 1] <- 'last'
#table(df$authortype)

tmp<-dplyr::filter(df,authortype!='Other')
#table(tmp$authortype,tmp$Gender)

# Step 1: pivot to wide so each paper has first & last author gender
paper_combos <- df %>%
  filter(authortype %in% c("first", "last")) %>%
  select(BIBTEXKEY, authortype, Gender, Citations_per_year) %>%
  distinct(BIBTEXKEY, authortype, .keep_all = TRUE) %>%   # one row per role per paper
  tidyr::pivot_wider(
    names_from  = authortype,
    values_from = Gender,
    names_prefix = "gender_"
  ) %>%
  # Citations should be the same regardless of which author row we used
  tidyr::drop_na(gender_first, gender_last)

# Step 2: define the four combinations
combos <- list(
  c(first = "female", last = "male"),
  c(first = "male",   last = "female"),
  c(first = "female", last = "female"),
  c(first = "male",   last = "male")
)

# Step 3: compute MedianCI for each combination
results <- purrr::map_dfr(combos, function(combo) {
  cites <- paper_combos$Citations_per_year[
    paper_combos$gender_first == combo["first"] &
      paper_combos$gender_last  == combo["last"]
  ]

  ci <- DescTools::MedianCI(cites, conf.level = 0.95)

  tibble::tibble(
    first_author = combo["first"],
    last_author  = combo["last"],
    n            = length(cites),
    median       = ci[1],
    CI_lower     = ci[2],
    CI_upper     = ci[3]
  )
})

#print(results)

# and run formal stats
# Aggregated grouping var combining both roles
paper_combos <- paper_combos %>%
  mutate(combo = paste(gender_first, gender_last, sep = "_"))

# Overall test, but this is not useful, not significant
kruskal.test(Citations_per_year ~ combo, data = paper_combos)

# Pairwise post-hoc (Dunn test)
library(dunn.test)
dunn.test(paper_combos$Citations_per_year, paper_combos$combo,
          method = "bonferroni")
