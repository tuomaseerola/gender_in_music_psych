# Filename: visualise_gender.R
# Author: T. Eerola, 6/4/2024
# Project: gender in music psychology
# Status: in progress

#### 1. Top countries ------------------
cat("Top countries \n")

#% female Country specific
S <- dplyr::summarise(
  group_by(df, Affiliation_country),
  n = n(),
  female = sum(Gender == 'female')
)
S$prop <- S$female / S$n
S$male <- -S$n - S$female
head(S)
SS <- dplyr::arrange(S, -n)
head(SS, 20)

library(ggplot2)
g1 <- ggplot(data = SS[1:10, ], aes(x = Affiliation_country, y = prop - .5)) +
  geom_col() +
  coord_flip() +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(
    breaks = seq(-.5, .5, .1),
    labels = seq(-.5, .5, .10) + .5,
    limits = c(-.25, .25)
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_linedraw()
print(g1)

# applied to my data
D2 <- dplyr::filter(
  df,
  Affiliation_country == 'US' |
    Affiliation_country == 'UK' |
    Affiliation_country == 'Australia' |
    Affiliation_country == 'Germany' |
    Affiliation_country == 'Canada' |
    Affiliation_country == 'Finland' |
    Affiliation_country == 'Israel' |
    Affiliation_country == 'Spain' |
    Affiliation_country == 'Italy' |
    Affiliation_country == 'France'
)
D2$Affiliation_country <- factor(
  D2$Affiliation_country,
  levels = rev(c(
    "US",
    "UK",
    "Australia",
    "Germany",
    "Canada",
    "Finland",
    "Israel",
    "Spain",
    "Italy",
    "France"
  ))
)

g2 <- ggplot(data = D2, aes(x = Affiliation_country, fill = Gender)) +
  geom_bar(data = subset(D2, Gender == "female"), color = 'black') +
  geom_bar(
    data = subset(D2, Gender == "male"),
    aes(y = after_stat(count) * (-1)),
    color = 'black'
  ) +
  # scale_y_continuous(
  #   breaks = seq(-150, 150, by = 50),
  #   labels = abs(seq(-150, 150, by = 50)),
  #   limits = c(-170, 150)
  # ) +
  scale_fill_brewer(palette = 4, type = 'qual', name = 'Gender') +
  coord_flip() +
  xlab('') +
  ylab('N') +
  theme_linedraw(base_size = 16)
print(g2)
#ggsave("gender_top10_countries_normal.png", g1, bg="transparent")

### %
S <- dplyr::summarise(
  group_by(D2, Affiliation_country),
  n = n(),
  female = sum(Gender == 'female')
)
S$prop <- S$female / S$n
S$male <- -S$n - S$female
#head(S,10)
S
g3 <- ggplot(data = D2, aes(x = Affiliation_country, fill = Gender)) +
  geom_bar(data = subset(D2, Gender == "female"), color = 'grey40') +
  geom_bar(
    data = subset(D2, Gender == "male"),
    aes(y = after_stat(count) * (-1)),
    color = 'grey40'
  ) +
  scale_y_continuous(
    breaks = seq(-500, 500, by = 100),
    labels = abs(seq(-500, 500, by = 100)),
    limits = c(-520, 500)
  ) +
  scale_fill_brewer(palette = 4, type = 'qual', name = 'Gender') +
  coord_flip() +
  xlab('') +
  annotate(
    "text",
    x = 1:8,
    y = S$female + 35,
    label = paste0(round(S$prop * 100), '%'),
    vjust = 0.0,
    hjust = 0.2,
    size = 4
  ) +
  ylab('N') +
  theme_light(base_size = 15)

print(g3)

