# attribute_gender_diagnostics.R
#### Simple diagnostics

cat(paste('\nUnique studies:', length(unique(df$BIBTEXKEY)),'\n'))

cat(paste(
  "\nUnique Entries as studies and author combinations:",
  length(unique(df$paper_id_author_id)),'\n'
))

cat(paste('\n Number of rows:', nrow(df),'\n'))

journal_with_total <- df %>%
  ungroup() %>%
  count(JOURNAL) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  bind_rows(
    summarise(.,
              JOURNAL = "Total",
              n = sum(n),
              percentage = sum(percentage))
  )

print(knitr::kable(journal_with_total))

cat("\n\n")

# API finds are based on
#print(median(df$Gender.Count))
#hist(df$Gender.Count)
#hist(df$Gender.Probability)

# the majority is confidently attributed
cat("Confidence of gender attribution by the API:\n\n")
cat(paste("Prop. over .90 conf. =",round(sum(df$Gender.Probability>.90)/nrow(df),3),"\n\n"))
cat(paste("Prop. over .95 conf. =",round(sum(df$Gender.Probability>.95)/nrow(df),3),"\n\n"))
cat(paste("Prop. under .55 conf. =",round(sum(df$Gender.Probability<.55)/nrow(df),3),"\n\n"))
cat(paste("Prop. under .55 conf. =",round(sum(df$Gender.Probability<.55)/nrow(df),3),"\n\n"))

cat("\n***")

S <- dplyr::summarise(group_by(df,Gender),M=mean(Gender.Probability),Md=median(Gender.Probability))
print(knitr::kable(S,digits = 2,caption="API: Gender probability means/medians."))

cat("\n***")

upperCI<-function(x, conf.level = 0.95) {
  f <- DescTools::MeanCI(x, conf.level = conf.level)
  #lowerCI <- f[1]
  upperCI <- f[3]
  return(upperCI)
}
lowerCI<-function(x, conf.level = 0.95) {
  f <- DescTools::MeanCI(x, conf.level = conf.level)
  lowerCI <- f[2]
  #upperCI <- f[2]
  return(lowerCI)
}
# citations across gender
S <- dplyr::summarise(group_by(df,Gender),M=mean(Citations),Md=median(Citations),LCI=lowerCI(Citations),UCI=upperCI(Citations))
print(knitr::kable(S,digits = 2,caption="Citations across gender."))

# get the stats of citations across gender
print(knitr::kable(broom::glance(t.test(Citations ~ Gender, data=df)),caption = "Citations (t-test)"))
print(knitr::kable(broom::glance(wilcox.test(Citations ~ Gender, data=df)),caption = "Citations (Wilcox-test)"))

#### Gender distribution of authors -----------
dist1 <- DescTools::MultinomCI(as.numeric(table(df$Gender)),conf.level=0.95,method="sisonglaz")
print(knitr::kable(dist1,digits=3,caption='Gender Distribution'))
