# Filename: visualise_gender.R
# Author: T. Eerola, 6/4/2024
# Project: gender in music psychology
# Status: in progress


#D <- dplyr::filter(d,study_id=='study1') # 1360 articles (1360 correct!)
#dim(D)

#install.packages("x",repos='http://cran.us.r-project.org', dependencies=TRUE)
#devtools::install_github("nsgrantham/ggdark")

#### 1. Top countries ------------------
cat("Top countries \n")

#% female Country specific 
S<-dplyr::summarise(group_by(D,FirstAuthorCountry),n=n(),female = sum(gender=='female'))
S$prop <- S$female/S$n
S$male <- - S$n - S$female
head(S)
SS<-dplyr::arrange(S,-n)
head(SS,20)
library(ggplot2)
g1<-ggplot(data = SS[1:10,], aes(x = FirstAuthorCountry,y=prop-.5)) + 
  geom_col() + 
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=seq(-.5,.5,.10)+.5,limits = c(-.25,.25)) + 
  scale_fill_brewer(palette="Dark2")+
  theme_linedraw()
print(g1)

# applied to my data
D2<-dplyr::filter(D,FirstAuthorCountry=='US' | 
                    FirstAuthorCountry=='UK' |
                    FirstAuthorCountry=='Australia' |
                    FirstAuthorCountry=='Germany' |
                    FirstAuthorCountry=='Canada' |
                    FirstAuthorCountry=='Finland' |
                    FirstAuthorCountry=='Israel' |
                    FirstAuthorCountry=='Spain' |
                    FirstAuthorCountry=='Italy' |
                    FirstAuthorCountry=='France')
D2$FirstAuthorCountry<-factor(D2$FirstAuthorCountry,levels = rev(c("US","UK","Australia","Germany","Canada","Finland","Israel","Spain","Italy","France")))

g2<-ggplot(data=D2,aes(x=FirstAuthorCountry,fill=gender)) + 
  geom_bar(data=subset(D2,gender=="female"),color='black') + 
  geom_bar(data=subset(D2,gender=="male"),aes(y=after_stat(count)*(-1)),color='black') + 
  scale_y_continuous(breaks=seq(-150,150,by=50),labels=abs(seq(-150,150,by=50)),limits = c(-170,150)) + 
  scale_fill_brewer(palette = 4,type = 'qual',name='Gender')+
  coord_flip()+
  xlab('')+
  ylab('N')+
  theme_linedraw(base_size = 16)
print(g2)


### % 
S<-dplyr::summarise(group_by(D2,FirstAuthorCountry),n=n(),female = sum(gender=='female'))
S$prop <- S$female/S$n
S$male <- - S$n - S$female
#head(S,10)

g3 <- ggplot(data=D2,aes(x=FirstAuthorCountry,fill=gender)) + 
  geom_bar(data=subset(D2,gender=="female"),color='grey40') + 
  geom_bar(data=subset(D2,gender=="male"),aes(y=after_stat(count)*(-1)),color='grey40') + 
  scale_y_continuous(breaks=seq(-150,150,by=50),labels=abs(seq(-150,150,by=50)),limits = c(-170,190)) + 
  scale_fill_brewer(palette = 4,type = 'qual',name='Gender')+
  coord_flip()+
  xlab('')+
  annotate("text",x=1:10,y=S$female+35,label=paste0(round(S$prop*100),'%'),vjust=0.5,size=6)+
  ylab('N')

print(g3)

#ggsave("gender_top10_countries_tp.png", g1, bg="transparent")

g4<-ggplot(data=D,aes(x=FirstAuthorCountry_WEOG,fill=gender)) + 
  geom_bar(data=subset(D,gender=="female"),color='black') + 
  geom_bar(data=subset(D,gender=="male"),aes(y=after_stat(count)*(-1)),color='black') + 
  #  scale_y_continuous(breaks=seq(-150,150,by=50),labels=abs(seq(-150,150,by=50)),limits = c(-170,150)) + 
  scale_fill_brewer(palette = 4,type = 'qual',name='Gender')+
  coord_flip()+
  xlab('')+
  ylab('N')+
  theme_linedraw(base_size = 16)
print(g4)


S<-dplyr::summarise(group_by(D,FirstAuthorCountry_USD_Md),n=n(),female = sum(gender=='female'))
S$prop <- S$female/S$n
S$male <- - S$n - S$female
#head(S)

S2<-dplyr::summarise(group_by(D,FirstAuthorCountry_WEOG),n=n(),female = sum(gender=='female',na.rm = T))
S2$prop <- S2$female/S2$n
S2$male <- - S2$n - S2$female
#head(S2)

#### 2. Continents ------------------
cat("Continents \n")

# continents, top 10 countries in each continent
library(rnaturalearth)
library(sf)
library(countrycode)

map <- ne_countries(
  scale="medium",
  returnclass='sf'
) %>% filter(admin !="Antarctica")

D<-ungroup(D)
table(map$continent)
european_countries<-map$name[map$continent=='Europe']
american_countries<-map$name[map$continent=='South America' | map$continent=='North America']
asian_countries<-map$name[map$continent=='Asia']
african_countries<-map$name[map$continent=='Africa']

european_countries[european_countries=='United Kingdom']<-'UK'
american_countries[american_countries=='United States of America']<-'US'

#### 3. EUROPE ----------------------
ind<-D$FirstAuthorCountry %in% european_countries
S<-dplyr::summarise(group_by(D[ind,],FirstAuthorCountry),n=n(),female = sum(gender=='female',na.rm = T))
S$prop <- S$female/S$n
S$male <- S$n - S$female
S<-dplyr::arrange(S,-n)

g5<-ggplot(data = S[1:10,], aes(x = reorder(FirstAuthorCountry,prop),y=prop-.5, label = n)) + 
  geom_col(fill='lightblue4',color='grey10') +
  geom_text(nudge_y = .02)+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.25,.25)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("% female")+
  xlab("Country (ranked)")
#  theme_linedraw(base_size = 15)
print(g5)

#### 4. AMERICA ----------------------
cat("Americas \n")

ind<-D$FirstAuthorCountry %in% american_countries
S<-dplyr::summarise(group_by(D[ind,],FirstAuthorCountry),n=n(),female = sum(gender=='female',na.rm = T))
S$prop <- S$female/S$n
S$male <- S$n - S$female
S<-dplyr::arrange(S,-n)

g6<-ggplot(data = S[1:6,], aes(x = reorder(FirstAuthorCountry,prop),y=prop-.5, label = n)) + 
  geom_col(fill='maroon1',color='grey10') +
  geom_text(nudge_y = .02)+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.3,.3)) + 
  #  scale_fill_brewer(palette="Dark2")+
  ylab("% female")+
  xlab("Country (ranked)")+
  theme_linedraw(base_size = 15)
print(g6)


#### 5. ASIA ----------------------
cat("Asia \n")

ind<-D$FirstAuthorCountry %in% asian_countries
S<-dplyr::summarise(group_by(D[ind,],FirstAuthorCountry),n=n(),female = sum(gender=='female',na.rm = T))
S$prop <- S$female/S$n
S$male <- S$n - S$female
S<-dplyr::arrange(S,-n)

g7<-ggplot(data = S, aes(x = reorder(FirstAuthorCountry,prop),y=prop-.5, label = n)) + 
  geom_col(fill='green3',color='grey10') +
  geom_text(nudge_y = .025)+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.51,.53)) + 
  #  scale_fill_brewer(palette="Dark2")+
  ylab("% female")+
  xlab("Country (ranked)")+
  theme_linedraw(base_size = 15)
print(g7)


#### 6. Comparative data from psychology --------
cat("Psychology \n")

source('../data/gonzalez.R')
gonzalez$prop<-gonzalez$prop/100

top10<-c("US","UK","Australia","Germany","Canada","Finland","Israel","Spain","Italy","France")
ID<-gonzalez$Country %in% top10
gonzalez10 <- gonzalez[ID,]

g8 <- ggplot(data = gonzalez10, aes(x = reorder(Country,prop),y=prop-.5, label = n)) + 
  geom_col(fill='yellow4',color='grey10') +
  geom_text(nudge_y = .055)+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.51,.53)) + 
  #  scale_fill_brewer(palette="Dark2")+
  ylab("% female")+
  xlab("Country (ranked)")+
  theme_linedraw(base_size = 15)
print(g8)

