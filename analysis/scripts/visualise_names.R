# Filename: visualise_names.R
# Author: T. Eerola, 6/4/2024
# Project: gender in music psychology
# Status: in progress

plotflag <- TRUE

D$firstn <- tolower(D$firstn)
D$firstn<-str_to_title(D$firstn)

men <- D %>%
  filter(gender=='male') %>%
  group_by(firstn) %>%
  summarise(n=n())

men<-dplyr::arrange(men,-n)
men$gender<-'male'
head(men)

which(men$firstn=='Petri')

women <- D %>%
  filter(gender=='female') %>%
  group_by(firstn) %>%
  summarise(n=n())

women<-dplyr::arrange(women,-n)
women$gender<-'female'
head(women)
top<-30
both<-rbind(men[1:top,],women[1:top,])
head(both,30)


if(plotflag==TRUE){
  library(ggwordcloud)
  set.seed(42)

  g1 <-ggplot(both, aes(label = firstn,size=n,color=gender)) +
    geom_text_wordcloud(area_corr = TRUE,shape='square') +
    scale_size_area(max_size = 42) +
    facet_wrap(.~gender)+
    scale_radius(range = c(8, 38), limits = c(0, NA)) +
    scale_color_brewer(palette="Set2")+
    theme_bw()
  print(g1)


}

#rm(D,data)
