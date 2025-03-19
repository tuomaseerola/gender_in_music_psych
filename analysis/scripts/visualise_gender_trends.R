# Filename: visualise_gender_trends.R
# Author: T. Eerola, 6/4/2024
# Project: gender in music psychology
# Status: in progress

tmp <- D %>%
  select(Year,gender) %>%
  drop_na() %>%
  group_by(Year) %>%
  summarise(Gender = sum(gender=='female'), n = n(),prop=Gender/n)
head(tmp)

g1<-ggplot(tmp,aes(Year,prop))+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 4),se=FALSE,color='lightblue2') +
  geom_point(size=3)+
  #  scale_y_continuous(breaks = seq(0,1,by=0.25),limits = c(0.3,1))+
  scale_y_continuous(limits=c(0.40,.65),breaks = seq(0,1,by=.1),expand = c(0.005,0.005),labels = seq(0,1,by=.1)*100)+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.2))+
  ylab('% Female')+
  theme_bw(base_size = 15)

print(g1)

