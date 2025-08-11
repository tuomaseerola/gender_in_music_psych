# attribute_gender_diagnostics.R
library(modelsummary)
#### Simple diagnostics

cat(paste('\nUnique studies:', length(unique(df$BIBTEXKEY)),'\n'))

cat(paste(
  "\nUnique Entries as studies and author combinations:",
  length(unique(df$paper_id_author_id)),'\n'
))

cat(paste('\n Number of rows:', nrow(df),'\n'))

print(datasummary(JOURNAL + 1 ~ N + Percent(), data = dplyr::filter(df,author_order==1),output='markdown',title='Number of studies across journals'))

cat("\n\n")

# API finds are based on
#print(median(df$Gender.Count))
#hist(df$Gender.Count)
#hist(df$Gender.Probability)

# the majority is confidently attributed
cat(paste("Prop. over .90 conf. =",sum(df$Gender.Probability>.90)/nrow(df),"/n"))
cat(paste("Prop. over .95 conf. =",sum(df$Gender.Probability>.95)/nrow(df),"/n"))
cat(paste("Prop. under .55 conf. =",sum(df$Gender.Probability<.55)/nrow(df),"/n"))
cat(paste("Prop. under .55 conf. =",sum(df$Gender.Probability<.55),"/n/n"))

cat("\n***")

print(datasummary(Mean + Median ~ Gender.Probability * Gender, data = df,output='markdown',title='Probability of API attributed Gender.'))

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

print(datasummary(Mean + upperCI + lowerCI ~ Citations * Gender, data = df,output='markdown',title='Citation counts across Gender'))

print(broom::glance(t.test(Citations ~ Gender, data=df)))
print(broom::glance(wilcox.test(Citations ~ Gender, data=df)))

detach("package:modelsummary", unload=TRUE)


#### Gender distribution of authors -----------
dist1 <- DescTools::MultinomCI(as.numeric(table(df$Gender)),conf.level=0.95,method="sisonglaz")
print(knitr::kable(dist1,digits=3,caption='Gender Distribution'))




