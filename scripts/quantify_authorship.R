# quantify_authorship.R
# T. Eerola
# 09/08/2025
# Gender analysis of MusicPsych authors
# Use Odds ratios to quantify the probabilities of gendered authorships.

#### Classify order: Single author ----------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Other'
df$authortype[df$author_single == 1] <- 'Single' # FOCUS
t<-table(df$authortype,df$Gender)
or1 <- effectsize::oddsratio(t) # 1.10
#print(or1)

#### Classify order: First author ----------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'First'
df$authortype[df$author_last == 1] <- 'Other'
df$authortype[df$author_single == 1] <- 'Other' # Take out single authors from first authors!
t<-table(df$authortype,df$Gender)
or2 <- effectsize::oddsratio(t) # 1.32
#print(or1)

#### classify order: Coauthor --------------
df$authortype <- 'Coauthor'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Other'
df$authortype[df$author_single == 1] <- 'Other' # Take out single authors from first authors!
t<-table(df$authortype,df$Gender)

or3 <- effectsize::oddsratio(t) # 1.01
#print(or2)

#### Classify order: Last -------------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Last'
df$authortype[df$author_single == 1] <- 'Other' # Take out single authors from first authors!
t<-table(df$authortype,df$Gender)
t
or4 <- effectsize::oddsratio(t) # 0.73

or <- rbind(or1,or2,or3,or4)
or$name<-c("Single","First","Coauthor","Last")
or<-dplyr::select(or,name,Odds_ratio,CI_low,CI_high)
print(knitr::kable(or))

#### Annually ---------

# introduce 5 year bins
df$YEAR_5 <- cut(df$YEAR, breaks=seq(2000, 2025, by=5), labels=c("2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024"), right=FALSE)
table(df$YEAR,df$YEAR_5)
table(df$YEAR_5)

# classify order: First author
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'First'
df$authortype[df$author_last == 1] <- 'Other'

output <- NULL
U<-levels(df$YEAR_5)
for (k in 1:length(U)){
  tmp<-dplyr::filter(df,YEAR_5==U[k])
  t <- table(tmp$authortype,tmp$Gender)
  output <- rbind(output,effectsize::oddsratio(t))
}
output$YearRange <- U
first <- data.frame(output)
first
# classify order: Coauthor
df$authortype <- 'Coauthor'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Other'

U<-levels(df$YEAR_5)
output <- NULL
for (k in 1:length(U)){
  tmp<-dplyr::filter(df,YEAR_5==U[k])
  t <- table(tmp$authortype,tmp$Gender)
  output <- rbind(output,effectsize::oddsratio(t))
}
output$YearRange <- U
coauthor <- data.frame(output)
coauthor
# classify order: Last
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Last'

U<-levels(df$YEAR_5)
output <- NULL
for (k in 1:length(U)){
  tmp<-dplyr::filter(df,YEAR_5==U[k])
  t <- table(tmp$authortype,tmp$Gender)
  output <- rbind(output,effectsize::oddsratio(t))
}
output$YearRange <- U
last <- data.frame(output)

first$Type<-'First'
coauthor$Type<-'Co'
last$Type<-'Last'

annual<-rbind(first,coauthor,last)

fig1 <- ggplot(annual,aes(x=YearRange,y=Odds_ratio,color=Type))+
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_point(position = position_dodge(width = 0.4))+
  geom_errorbar(aes(ymin=CI_low,ymax=CI_high),width=0.2,position = position_dodge(width = 0.4))+
  scale_color_brewer(palette = "Set1") +
  xlab('Year range')+
  ylab('Female Authorship Odds Ratio')+
  theme_classic()
print(fig1)

#### Average growth rate ----------

# [GR = (ending value - ending value) - 1]
#[ AAGR = (GR1 + GR2 + ... +GRn) / N ] 
first
first$Year<-recode(first$YearRange, 
                   '2000-2004' = 1, 
                   '2005-2009' = 2, 
                   '2010-2014' = 3, 
                   '2015-2019' = 4, 
                   '2020-2024' = 5)
first
first_gr <- first %>%
  # first sort by year
  arrange(YearRange) %>%
  mutate(Diff_year = Year - lag(Year),  # Difference in time (just in case there are gaps)
         Diff_growth = Odds_ratio - lag(Odds_ratio), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/Odds_ratio * 100) # growth rate in percent

first_gr
first_gr_AAGR <- mean(first_gr$Rate_percent,na.rm = TRUE)

coauthor$Year<-recode(coauthor$YearRange, 
                   '2000-2004' = 1, 
                   '2005-2009' = 2, 
                   '2010-2014' = 3, 
                   '2015-2019' = 4, 
                   '2020-2024' = 5)

coauthor_gr <- coauthor %>%
  # first sort by year
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year),  # Difference in time (just in case there are gaps)
         Diff_growth = Odds_ratio - lag(Odds_ratio), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/Odds_ratio * 100) # growth rate in percent

coauthor_gr_AAGR <- mean(coauthor_gr$Rate_percent,na.rm = TRUE)

last$Year<-recode(last$YearRange, 
                   '2000-2004' = 1, 
                   '2005-2009' = 2, 
                   '2010-2014' = 3, 
                   '2015-2019' = 4, 
                   '2020-2024' = 5)

last_gr <- last %>%
  # first sort by year
  arrange(Year) %>%
  mutate(Diff_year = Year - lag(Year),  # Difference in time (just in case there are gaps)
         Diff_growth = Odds_ratio - lag(Odds_ratio), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/Odds_ratio * 100) # growth rate in percent

last_gr
last_gr_AAGR <- mean(last_gr$Rate_percent, na.rm = TRUE)

print('5-year growth rate of female authorships:')
annual_growth <- data.frame(
  Type = c("First", "Coauthor", "Last"),
  AAGR = c(first_gr_AAGR, coauthor_gr_AAGR, last_gr_AAGR)
)

print(knitr::kable(annual_growth,digits = 2))
