# Filename: summarise_gender.R
# Author: T. Eerola, 6/4/2024
# Project: gender in music psychology
# Status: in progress

#### 1. Gender distribution ------

gender_with_total <- df %>%
  ungroup() %>%
  count(Gender) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  bind_rows(
    summarise(.,
              Gender = "Total",
              n = sum(n),
              percentage = sum(percentage))
  )

print(knitr::kable(gender_with_total))


# tmp <- df %>%
#   select(first_name, Gender) %>%
#   filter(Gender == 'female') %>%
#   drop_na() %>%
#   group_by(first_name) %>%
#   summarise(n = n())
# tmp <- dplyr::arrange(tmp, -n)
# print(knitr::kable(head(tmp, 5), caption = 'Top 5 names.'))
# rm(tmp)

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
s$n
# create a histogram
S<-s
S$n[S$n > 9]<-10
g<-ggplot(S,aes(x=n))+
  geom_histogram(binwidth=1,fill='grey90',color='black')+
  theme_classic()+
  labs(x='Number of authors',y='Count of articles')+
  scale_x_continuous(breaks=seq(1,10,1),expand = c(0.001,0.001),limits = c(0,11),
                     labels = c(seq(1,9,1),"10+"))+
  scale_y_continuous(expand = c(0.001,0.001))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
print(g)

rm(s,gender_with_total)
