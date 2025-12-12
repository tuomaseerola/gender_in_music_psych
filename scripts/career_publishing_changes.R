# extra analysis: career changes in authorship positions
source('scripts/load_gender_data.R') # Loads the data

# take all unique author names
U<-unique(df$unique_name)
length(U) # 5312

# minimun number of publications
min_pubs <- 4

# minim number of years between first and last publication
min_years <- 7

# create a data frame to store results
career_changes<-NULL
# loop through each unique author
for (author in U) {
  # subset data for the author
  author_data <- df %>% filter(unique_name == author)
  num_pubs <- nrow(author_data)
  if (num_pubs >= min_pubs) {
    first_year <- min(author_data$YEAR)
    last_year <- max(author_data$YEAR)
    if ((last_year - first_year) >= min_years) {
      year0_position <- data.frame(year0=author_data$YEAR-first_year, author_order=author_data$author_order,authortype=author_data$authortype,Gender=author_data$Gender[1], stringsAsFactors=FALSE)
      year0_position <- dplyr::arrange(year0_position, year0)
      year0_position$authortype[year0_position$author_order==1]<-"First"
     # year0_position$authortype[year0_position$author_order==2]<-"Other" # extra
      year0_position
      early<-filter(year0_position, year0 <= max(year0_position$year0)/2)
      late<-filter(year0_position, year0 > max(year0_position$year0)/2)
      early_counts <- table(early$authortype)
      early_counts_max <- names(which.max(early_counts))
      late_counts <- table(late$authortype)
      late_counts_max <- names(which.max(late_counts))

      career_changes <- rbind(career_changes, data.frame(author=author,
                                                         gender=author_data$Gender[1],
                                                           first_year=first_year,
                                                           last_year=last_year,
                                                           num_pubs=num_pubs,
                                                           early_type_max=early_counts_max,
                                                           late_type_max=late_counts_max,
                                                           early_order_mean=mean(early$author_order),
                                                           late_order_mean=mean(late$author_order),
                                                         stringsAsFactors=FALSE))
    }
  }
}

dim(career_changes)
career_changes<-dplyr::filter(career_changes, early_type_max != "Other")
career_changes<-dplyr::filter(career_changes, late_type_max != "Other")
dim(career_changes)
head(career_changes)

#S<-summarise(group_by(career_changes,gender),M_early=mean(early_order_mean),SD_early=sd(early_order_mean),M_late=mean(late_order_mean),SD_late=sd(late_order_mean),N=n())
#S

t_early<-table(career_changes$early_type_max, career_changes$gender)
t_late<-table(career_changes$late_type_max, career_changes$gender)
t_early
t_late

# Explicit order of the rows and columns!

t <- table(
  factor(career_changes$early_type_max, levels = c("Last", "First")),
  factor(career_changes$gender,levels = c("female","male"))
)
t

# being the last author
or1 <- effectsize::oddsratio(t) # 0.64       | [0.34, 1.21]
or1

t2 <- table(
  factor(career_changes$late_type_max, levels = c("Last", "First")),
  factor(career_changes$gender,levels = c("female","male"))
)
t2
or2 <- effectsize::oddsratio(t2) # 0.68       | [0.39, 1.18]
or2

tab_early <- t(t)
tab_early
fisher.test(tab_early)

tab_late <- t(t2)
tab_late
fisher.test(tab_late)

# Pool together to test the difference
tabs <- array(c(tab_early, tab_late), dim = c(2, 2, 2))

mantelhaen.test(tabs)   # Cochran–Mantel–Haenszel test
# X-squared = 3.3601, df = 1, p-value = 0.06679

# To explore whether women and men authors transition different degrees to
# last authorship during their career, we analysed authors with at least four publications
# over a time span of at least seven years. We compared the most common
# authorship position during the first and last halves of their careers.
# During the early part of the career, the odds ratio for women being last authors
# were 0.64 [0.34, 1.21] and during the later part 0.68 [0.39, 1.18]. Comparing the early
# and late halves of the careers, there was a trend for a difference in the transition
# (X^2(1) = 3.36, p = 0.067).

