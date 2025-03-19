# keyword_analysis.R
# WEIRD article
# T. Eerola, 23/3/2024
# Status: Complete

plotflag <- TRUE

## remove studies, keep articles (to avoid duplicating keywords)
D <- dplyr::filter(d,study_id=='study1') # 1360 articles (1360 correct!)

if(!dim(D)[1]==1360){
  print("incorrect number of observations! (Needs to be run at the level of studies, N=1360)")
  break
}

# delete studies where CountryDataCollected is not defined
D<-dplyr::filter(D,!is.na(CountryDataCollected))
dim(D)


KW<-NULL
d_index <- NULL
GENDER_index <- NULL
for (k in 1:nrow(D)) {
  tmp <- D$Keywords[k]
  GENDER <- as.character(D$gender[k])
  tmp <- stringi::stri_trans_general(tmp,"latin-ascii")
  if(!is.na(tmp)){
    kw<-str_split(tmp,'[;,]', simplify = TRUE)
    KW<-c(KW,kw)
    d_index<-c(d_index,rep(k,length(kw)))
    GENDER_index<-c(GENDER_index,rep(GENDER,length(kw)))
  }
}
head(d_index)
head(GENDER_index)
length(unique(d_index))

KW<-stringi::stri_trans_general(KW,"latin-ascii")

KW<-str_remove_all(KW,'^ ') # remove leading spaces
KW<-tolower(KW)
KW<-str_remove_all(KW,'ê') # remove garbage
KW<-str_remove_all(KW,'�') # remove garbage
KW<-str_remove_all(KW,'s$') # remove plurals
KW<-str_remove_all(KW,'ology$') # Methodology to method
KW<-str_remove_all(KW,'^\n') # Methodology to method
KW<-str_replace_all(KW,'stres','stress') # remove plurals
KW<-str_replace_all(KW,'sadnes','sadness') # remove plurals
KW<-str_replace_all(KW,'analysi','analysis') # remove plurals
KW<-str_replace_all(KW,'loudnes','loudness') # remove plurals
KW<-str_replace_all(KW,'aesthetic','aesthetics') # remove plurals
KW<-str_replace_all(KW,'psychophysi','psychophysiology') # remove plurals
KW<-stringi::stri_trans_general(KW,"latin-ascii")

length(KW)
length(d_index)
tmp<-data.frame(KW,GENDER_index)
head(tmp)
tmp1<-dplyr::filter(tmp,GENDER_index=='male')
dim(tmp1)
tmp2<-dplyr::filter(tmp,GENDER_index=='female')
head(tmp2)
dim(tmp2)

freq1<-data.frame(table(tmp1$KW))
head(freq1)
freq1<-dplyr::arrange(freq1,-Freq)
head(freq1)
tail(freq1)
freq2<-data.frame(table(tmp2$KW))
freq2<-dplyr::arrange(freq2,-Freq)
head(freq2)

KW_unique<-unique(KW)

freq_table<-data.frame(KW=KW_unique,male=rep(0,length(KW_unique)),female=rep(0,length(KW_unique)))
head(freq_table)
rownames(freq_table)<-NULL
length(KW_unique)
for (k in 1:length(KW_unique)) {
#  print(k)
  x1 <- freq1$Freq[which(freq1$Var1==KW_unique[k])]
  if(length(x1)>0){
    freq_table$male[k] <- as.integer(x1)
  }
  x2 <- freq2$Freq[which(freq2$Var1==KW_unique[k])]
  if(length(x2)>0){
    freq_table$female[k] <- as.integer(x2)
  }
}
freq_table<-dplyr::arrange(freq_table,-female)
head(freq_table,20)

freq_table$Freq<-freq_table$male+freq_table$female
freq_table$prop<-freq_table$female/freq_table$Freq
head(freq_table)
freq_table<-dplyr::arrange(freq_table,-female)
head(freq_table)

## New: NOrmalise size within WEOG AND non-WEOG
freq_table$male_R<-freq_table$male/max(freq_table$male)
freq_table$female_R<-freq_table$female/max(freq_table$female)
freq_table$joint_norm_freq <- (freq_table$female_R + freq_table$female_R) / 2
head(freq_table)
freq_table<-dplyr::arrange(freq_table,-joint_norm_freq)
freq_table$prop_RS<-scales::rescale(freq_table$prop,to=c(0,90))
freq_table$prop_RS<- 45-freq_table$prop_RS
head(freq_table,50)

freq_table$prop_RS<-scales::rescale(freq_table$prop^3.3,from=c(0,1), to=c(-45,45))
N <- 40
data<-freq_table[2:N,]

tmp<-dplyr::select(data,KW,Freq,male,female,joint_norm_freq,prop)
tmp$rank<-rev(1:nrow(tmp))

#### Plot wordcloud --------------
if(plotflag==TRUE){
  library(ggwordcloud)
  set.seed(42)

  mean(scales::rescale(tmp$prop,to=c(-45,75)))

  g5<-ggplot(tmp, aes(label = KW,size=Freq,color=scales::rescale(prop,to=c(-45,70)),angle=scales::rescale(prop,to=c(-45,70)))) +
    geom_text_wordcloud(area_corr = TRUE) +
    scale_size_area(max_size = 50, trans = power_trans(1/.7)) +
#    scale_color_gradientn(colours = terrain.colors(10))+
     scale_color_gradient(low ="blue",high = "red")+
    theme_bw()
  g5<-g5 + dark_mode(theme_minimal(base_size = 18))
  ggsave("gender_keywords_wordcloud.png", g5, height = 6,width = 6)
  set.seed(42)

g5<-g5 + theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

  ggsave("gender_keywords_wordcloud_tp.png", g5,height = 6,width = 6)


#### Plot positioned graph --------------
library(ggrepel)
g1 <- ggplot(tmp,aes(x=rank,y=prop,size=Freq,label=KW,color=prop))+
  geom_text_repel(show.legend=FALSE,segment.color = NA)+
  coord_flip()+
  scale_y_continuous(limits=c(0.25,0.85),breaks=seq(0,1,by=.25),label=paste0(seq(0,1,by=.25)*100,'%'))+
  scale_color_gradientn(colours = terrain.colors(10))+
  scale_fill_gradientn(colours = terrain.colors(10))+
  scale_size(range=c(4,16))+
  xlab('')+
  ylab('% Female')+
  theme_bw(base_size = 15)
g1
g1<-g1 + dark_mode(theme_minimal(base_size = 18))
ggsave("gender_keywords.png", g1,height = 6,width = 9)

g1<-g1 + theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)
g1
ggsave("gender_keywords_tp.png", g1,height = 6,width = 9)


}

rm(D,data,freq_table,freq1,freq2)
