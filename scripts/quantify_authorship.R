# quantify_authorship.R
# T. Eerola
# 09/08/2025
# Gender analysis of MusicPsych authors
# Use Odds ratios to quantify the probabilities of gendered authorships.

# Store the raw counts in a separate table
stored_table <- data.frame(matrix(data = NA,nrow=5,ncol=3)); colnames(stored_table)<-c("Author Type","Female","Male")


#### Classify order: Single author ----------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Other'
df$authortype[df$author_single == 1] <- 'Single' # FOCUS
# t<-table(df$authortype,df$Gender)
# t
# Explicit order of the rows and columns!
t <- table(
  factor(df$authortype, levels = c("Single", "Other")),
  factor(df$Gender,levels = c("female","male"))
)
t
or1 <- effectsize::oddsratio(t) # 0.83       | [0.71, 0.96]
stored_table[1,2:3] <- t[1,]; stored_table[1,1]<-"Single"
#stored_table
#print(or1)
#fisher.test(t)

#### Classify order: First author ----------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'First'
df$authortype[df$author_last == 1] <- 'Other'
df$authortype[df$author_single == 1] <- 'Other' # Take out single authors from first authors!
# t<-table(df$authortype,df$Gender)
# t
# Explicit order of the rows and columns!
t <- table(
  factor(df$authortype, levels = c("First", "Other")),
  factor(df$Gender,levels = c("female","male"))
)
t

or2 <- effectsize::oddsratio(t) # 1.27       | [1.03, 1.56]
stored_table[2,2:3] <- t[1,]; stored_table[2,1]<-"First"
#print(or2)
#fisher.test(t)

#### Classify order: Coauthor --------------
df$authortype <- 'Coauthor'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Other'
df$authortype[df$author_single == 1] <- 'Other' # Take out single authors from first authors!
#t<-table(df$authortype,df$Gender)
#t
# Explicit order of the rows and columns!
t <- table(
  factor(df$authortype, levels = c("Coauthor", "Other")),
  factor(df$Gender,levels = c("female","male"))
)
t

or3 <- effectsize::oddsratio(t) # 1.06       | [0.77, 1.46]
stored_table[3,2:3] <- t[1,]; stored_table[3,1]<-"Co-author"
#print(or3)
#fisher.test(t)

#### Classify order: Last -------------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Last'
df$authortype[df$author_single == 1] <- 'Other' # Take out single authors from first authors!
#t<-table(df$authortype,df$Gender)
#t
# Explicit order of the rows and columns!
t <- table(
  factor(df$authortype, levels = c("Last", "Other")),
  factor(df$Gender,levels = c("female","male"))
)
t

or4 <- effectsize::oddsratio(t) # 0.73
stored_table[4,2:3] <- t[1,]; stored_table[4,1]<-"Last"


#print(or4)

or <- rbind(or1,or2,or3,or4)
or$name<-c("Single","First","Coauthor","Last")
or<-dplyr::select(or,name,Odds_ratio,CI_low,CI_high)
print(knitr::kable(or,digits = 3,caption = "Female authorship odds ratios by authorship type."))


t<-table(df$Gender)
t
stored_table[5,2:3] <- as.numeric(t); stored_table[5,1]<-"All"
stored_table$Female_prc <- stored_table$Female/(stored_table$Female+stored_table$Male)*100

stored_table$OR<-NA
stored_table$OR_L<-NA
stored_table$OR_U<-NA
stored_table$OR[1:4]<-or$Odds_ratio
stored_table$OR_L[1:4]<-or$CI_low
stored_table$OR_U[1:4]<-or$CI_high
stored_table

print(knitr::kable(stored_table,digits = 2,caption = "Summary of authortypes across gender (raw counts, percentage, and odds ratios)."))
#write.csv(stored_table,'data/authorship_counts_table.csv',row.names = FALSE)
#### Annually ---------

# introduce 5 year bins
df$YEAR_5 <- cut(df$YEAR, breaks=seq(2000, 2025, by=5), labels=c("2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024"), right=FALSE)
table(df$YEAR,df$YEAR_5)
table(df$YEAR_5)

#### Classify order: Single author ----------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Other'
df$authortype[df$author_single == 1] <- 'Single' # FOCUS

output <- NULL
U<-levels(df$YEAR_5)
for (k in 1:length(U)){
  tmp<-dplyr::filter(df,YEAR_5==U[k])
  #t <- table(tmp$authortype,tmp$Gender)
  # Explicit order of the rows and columns!
  # t <- table(
  #   factor(df$authortype, levels = c("Single", "Other")),
  #   factor(df$Gender,levels = c("female","male"))
  # )
  t <- table(
    factor(tmp$authortype, levels = c("Single", "Other")),
    factor(tmp$Gender,levels = c("female","male"))
  )
  output <- rbind(output,effectsize::oddsratio(t))
}
output$YearRange <- U
single <- data.frame(output)
single

#### Classify order: First author ----------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'First'
df$authortype[df$author_last == 1] <- 'Other'
df$authortype[df$author_single == 1] <- 'Other' # Take out single authors from first authors!

output <- NULL
U<-levels(df$YEAR_5)
for (k in 1:length(U)){
  tmp<-dplyr::filter(df,YEAR_5==U[k])
  #t <- table(tmp$authortype,tmp$Gender)
  # Explicit order of the rows and columns!
  # t <- table(
  #   factor(df$authortype, levels = c("First", "Other")),
  #   factor(df$Gender,levels = c("female","male"))
  # )
  t <- table(
    factor(tmp$authortype, levels = c("First", "Other")),
    factor(tmp$Gender,levels = c("female","male"))
  )
  output <- rbind(output,effectsize::oddsratio(t))
}
output$YearRange <- U
first <- data.frame(output)
first

#### Classify order: Coauthor --------------
df$authortype <- 'Coauthor'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Other'
df$authortype[df$author_single == 1] <- 'Other' # Take out single authors from first authors!

U<-levels(df$YEAR_5)
output <- NULL
for (k in 1:length(U)){
  tmp<-dplyr::filter(df,YEAR_5==U[k])
#  t <- table(tmp$authortype,tmp$Gender)
  # Explicit order of the rows and columns!
  # t <- table(
  #   factor(df$authortype, levels = c("Coauthor", "Other")),
  #   factor(df$Gender,levels = c("female","male"))
  # )
  t <- table(
    factor(tmp$authortype, levels = c("Coauthor", "Other")),
    factor(tmp$Gender,levels = c("female","male"))
  )
  output <- rbind(output,effectsize::oddsratio(t))
}
output$YearRange <- U
coauthor <- data.frame(output)
coauthor

#### Classify order: Last -------------
df$authortype <- 'Other'
df$authortype[df$author_order == 1] <- 'Other'
df$authortype[df$author_last == 1] <- 'Last'
df$authortype[df$author_single == 1] <- 'Other' # Take out single authors from first authors!

U<-levels(df$YEAR_5)
output <- NULL
for (k in 1:length(U)){
  tmp<-dplyr::filter(df,YEAR_5==U[k])
#  t <- table(tmp$authortype,tmp$Gender)
  # Explicit order of the rows and columns!
  # t <- table(
  #   factor(df$authortype, levels = c("Last", "Other")),
  #   factor(df$Gender,levels = c("female","male"))
  # )
  t <- table(
    factor(tmp$authortype, levels = c("Last", "Other")),
    factor(tmp$Gender,levels = c("female","male"))
  )
  output <- rbind(output,effectsize::oddsratio(t))
}
output$YearRange <- U
last <- data.frame(output)

single$Type<-'Single'
first$Type<-'First'
coauthor$Type<-'Co'
last$Type<-'Last'

annual<-rbind(single,first,coauthor,last)
annual$Type = factor(annual$Type, levels = c('Single','First','Co', 'Last'))
annual

# Add overall numbers
or$name = factor(or$name, levels = c('Single','First','Coauthor', 'Last'), labels = c('Single','First','Co', 'Last'))

tmp <- data.frame(Odds_ratio = or$Odds_ratio,
                  CI=0.95,
                  CI_low = or$CI_low,
                  CI_high = or$CI_high,
                  YearRange = "Overall",
                  Type = or$name)
annual2<- rbind(tmp,annual)
annual2$separator<-"Yearly"
annual2$separator[annual2$YearRange=="Overall"]<-"Overall"
annual2$YearRange <- factor(annual2$YearRange, levels = c("Overall", "2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024"))

fig1 <- ggplot(annual2,aes(x=YearRange,y=Odds_ratio,color=Type,group=Type,shape=separator))+
  #facet_wrap(~TypeGroup,nrow = 2)+
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_point(position = position_dodge(width = 0.4),show.legend=FALSE,size=3)+
  geom_errorbar(aes(ymin=CI_low,ymax=CI_high),width=0.2,position = position_dodge(width = 0.4))+
  #geom_line(alpha = 0.2,position = position_dodge(width = 0.4))+
  scale_shape_manual(values = c(22, 16)) +
  scale_color_brewer(name="Authorship",palette = "Set1") +
  xlab('Year range')+
  ylab('Female Authorship Odds Ratio (95%CI)')+
  theme_classic()
fig1

# fig1 <- ggplot(annual,aes(x=YearRange,y=Odds_ratio,color=Type,group=Type))+
#   facet_wrap(~Type,nrow = 4)+
#   geom_hline(yintercept=1, linetype="dashed", color = "black") +
#   geom_point(position = position_dodge(width = 0.4))+
#   geom_errorbar(aes(ymin=CI_low,ymax=CI_high),width=0.2,position = position_dodge(width = 0.4))+
#   geom_line(alpha = 0.2)+
#   scale_color_brewer(palette = "Set1") +
#   xlab('Year range')+
#   ylab('Female Authorship Odds Ratio')+
#   theme_classic()
# print(fig1)

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

#print('5-year growth rate of female authorships:')
annual_growth <- data.frame(
  Type = c("First", "Coauthor", "Last"),
  AAGR = c(first_gr_AAGR, coauthor_gr_AAGR, last_gr_AAGR)
)

print(knitr::kable(annual_growth,digits = 2,caption="5-year growth rate of female authorships"))
