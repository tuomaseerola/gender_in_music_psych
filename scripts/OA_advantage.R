# explore_OA_advantage.R


#### Classify order: First author ----------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'First'
df$authortype[df$author_last == 1] <- 'Other'
df$authortype[df$author_single == 1] <- 'Other' # Take out single authors from first authors!
# t<-table(df$authortype,df$Gender)

table(df$authortype,df$Gender)

# Explicit order of the rows and columns!
t <- table(
  factor(df$authortype, levels = c("First", "Other")),
  factor(df$Gender,levels = c("female","male"))
)
t
print(effectsize::oddsratio(t)) # 1.41

# Specific journals
journal <- "Journal of New Music Research"
t <- table(
  factor(df$authortype[df$JOURNAL==journal], levels = c("First", "Other")),
  factor(df$Gender[df$JOURNAL==journal],levels = c("female","male"))
)
t
print(effectsize::oddsratio(t)) # 1.37
journal <- "Psychology of Music" # 1.47
journal <- "Music and Science" # 1.75
journal <- "Music Perception" # 1.26
journal <- "Musicae Scientiae" # 1.48


table(df$JOURNAL,df$authortype)
table(df$JOURNAL,df$OA)
table(df$YEAR,df$OA)


t <- table(
  factor(df$authortype[df$YEAR>2000 & df$YEAR<2010], levels = c("First", "Other")),
  factor(df$Gender[df$YEAR>2000 & df$YEAR<2010],levels = c("female","male"))
)
t
print(effectsize::oddsratio(t)) # 1.51
# 2019: 1.51
# 2010-2019: 1.47
# 2000-2009: 1.16
