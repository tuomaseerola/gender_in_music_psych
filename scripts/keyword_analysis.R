# keyword_analysis.R
# gender in music psych
# T. Eerola, 19/8/2025, edited also by A. M. Czepiel
# Status: In progress

#### Custom functions -------------------
#source('scripts/count2category.R')
plotflag <- FALSE
duplication <- TRUE

## remove studies, keep articles (to avoid duplicating keywords)
D <- dplyr::filter(df, author_id == 'author1') # 3373

if (!dim(D)[1] == 3373) {
  print("incorrect number of observations! (Needs to be run at the level of studies, N=1360)")
  break
}

#### keyword analysis for first authors only ------------------
if(duplication==FALSE){
  KW <- NULL
  d_index <- NULL
  GENDER_index <- NULL
  year_index <- NULL # year

  for (k in 1:nrow(D)) {
    tmp <- D$AUTHOR_KEYWORDS[k]
    GENDER <- as.character(D$Gender[k])
    y <- D$YEAR[k]
    tmp <- stringi::stri_trans_general(tmp, "latin-ascii")
    if (!is.na(tmp)) {
      kw <- str_split(tmp, '[;,]', simplify = TRUE)
      KW <- c(KW, kw)
      d_index <- c(d_index, rep(k, length(kw)))
      # u_index <- c(u_index, rep(u, length(kw)))
      # o_index <- c(o_index, rep(o, length(kw)))
      # m_index <- c(m_index, rep(m, length(kw)))
      # a_index <- c(a_index, rep(a, length(kw)))
      # g_index <- c(g_index, rep(g, length(kw)))
      year_index <- c(year_index, rep(y, length(kw)))
      GENDER_index <- c(GENDER_index, rep(GENDER, length(kw)))
    }
  }


  KW <- stringi::stri_trans_general(KW, "latin-ascii")
  KW<-stringr::str_trim(KW, side = "both")
  KW<-tolower(KW)
  length(KW)
  length(unique(KW))

  data <- data.frame(KW,
                     GENDER_index,
                     year_index)
}

#### All authors -------
#### Alternatively where all authors contribute ------
if(duplication==TRUE){

  KW <- NULL
  d_index <- NULL
  GENDER_index <- NULL
  year_index <- NULL # year

  for (k in 1:nrow(df)) {
    tmp <- df$AUTHOR_KEYWORDS[k]
    GENDER <- as.character(df$Gender[k])
    y <- df$YEAR[k]
    tmp <- stringi::stri_trans_general(tmp, "latin-ascii")
    if (!is.na(tmp)) {
      kw <- str_split(tmp, '[;,]', simplify = TRUE)
      KW <- c(KW, kw)
      year_index <- c(year_index, rep(y, length(kw)))
      GENDER_index <- c(GENDER_index, rep(GENDER, length(kw)))
    }
  }

  KW <- stringi::stri_trans_general(KW, "latin-ascii")
  KW<-stringr::str_trim(KW, side = "both")
  KW<-tolower(KW)
  length(KW)
  length(unique(KW))

  data <- data.frame(KW,
                     GENDER_index,
                     year_index)
}







## Words related to music cognition
idx = grep("cognition", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("music cognition",
                           "music and cognition",
                           "musical and cognition",
                           "language and music cognition",
                           "music and social cognition")
                     ~ "music cognition",TRUE ~ KW))

## Words related to recognition
idx = grep("recognition", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("recognition",
                           "optical music recognition",
                           "optical music recognition (omr)",
                           "omr (optical music recognition)",
                           "content-based generalized sound recognition",
                           "tempo recognition",
                           "melody recognition",
                           "serial recognition",
                           "recognition of scale and key by experts",
                           "emotion recognition",
                           "recognition task" ,
                           "auditory recognition",
                           "pattern recognition",
                           "sound recognition",
                           "recognition performance")
                 ~ "music recognition",TRUE ~ KW))

## Words related to music emotion
idx = grep("emotion", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("emotional music experience",
                           "music and emotions",
                           "music emotion recognition",
                           "emotional response to music",
                           "music emotion classification",
                           "emotional valence",
                           "music-perceived emotions",
                           "musical emotions",
                           "music induced emotions",
                           "music-induced emotion",
                           "music-evoked emotion",
                           "negative emotion in music",
                           "musical emotion mechanisms",
                           "emotional use of music",
                           "music emotion identification" ,
                           "musical emotional contagion",
                           "emotion in music",
                           "arousal",
                           "emotions during listening" )
                     ~ "music emotions",TRUE ~ KW)
)

## Words related to embodied music cognition
idx = grep("embod", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("embodied music cognition",
                               "embodiment",
                               "embodied cognition",
                               "embodied interaction",
                               "embodied music interaction",
                               "embodied musical experience",
                               "musical embodiment",
                               "embodied knowledge",
                               "embodied",
                               "embodied mind",
                               "4e cognition‌")
                     ~ "embodied music cognition",TRUE ~ KW)
)


## Words related to music performance
idx = grep("performa", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("piano performance",
                           "music performance",
                           "musical performance",
                           "violin performance",
                           "music performance studies",
                           "performance",
                           "performance psychology",
                           "ensemble performance",
                           "drum performance",
                           "guitar performance",
                           "automated piano performance",
                           "clarinet performances",
                           "expressive performance",
                           "bodily gestures in musical duo performances",
                           "performance studies",
                           "assessment of music performance",
                           "expert performance",
                           "classical music performance",
                           "guitar performance achievement",
                           "music performance quality",
                           "solo performance" )
                 ~ "music performance",TRUE ~ KW)
)


## Words related to historically informed performance
idx = grep("performa", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("historical performance practice",
                           "historically informed performance (hip)",
                           "nineteenth century music performance",
                           "historically-informed performance",
                           "historically informed performance")
                 ~ "historical performance practice",TRUE ~ KW))

## Words related to performance anxiety
data = data %>% mutate(
  KW = case_when(KW %in% c("performance anxiety",
                           "anxiety-performance relationship",
                           "musical performance anxiety",
                           "music performance anxiety characteristics",
                           "music performance anxiety inventory for adolescents")
                 ~ "music performance anxiety",TRUE ~ KW))


## Words related to music expression
idx = grep("dance", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("performance expression",
                           "expressive intentions",
                           "music expressivity",
                           "expressive singing",
                           "vocal expression" ,
                           "artistic expression",
                           "emotional facial expressions",
                           "expressive gestures",
                           "expressivity",
                           "expression" ,
                           "musical expression",
                           "musical expressivity",
                           "musical expressiveness",
                           "expressive musical terms",
                           "performer's expression",
                           "expressive music performance",
                           "emotional expression" ,
                           "emotion expression",
                           "expressive content",
                           "expressed emotion",
                           "performance expression",
                           "expressive clarinet performance",
                           "expressive intention")
                 ~ "performance expression",TRUE ~ KW))


data = data %>% mutate(
  KW = case_when(KW %in% c("new interfaces for musical expression",
                           "new interface for musical expression" )
                 ~ "new interface for musical expression",TRUE ~ KW))




# Dance related
idx = grep("dance", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("dance",
                           "dance training" ,
                           "music and dance sophistication",
                           "electronic dance" ,
                           "evolution of dance",
                           "dance tempo" ,
                           "dance & music" ,
                           "aboriginal dance-song" ,
                           "song and dance" )
                 ~ "dance",TRUE ~ KW))


# Illusion
idx = grep("illusion", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("the speech-to-song illusion",
                           "sound-to-music illusion",
                           "speech-to-song illusion"  ,
                           "time illusion" ,
                           "shepard illusion",
                           "filled duration illusion" ,
                           "time-shrinking illusion",
                           "auditory illusion",
                           "octave illusion",
                           "illusion")
                 ~ "music illusion",TRUE ~ KW))


# aesthetics
idx = grep("aesthetics", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("aesthetics",
                           "sound aesthetics",
                           "empirical aesthetics" ,
                           "medieval aesthetics",
                           "music aesthetics",
                           "neuroaesthetics"  ,
                           "musical aesthetics",
                           "experimental aesthetics")
                 ~ "music aesthetics",TRUE ~ KW))


# education
idx = grep("education", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("music education",
                           "chinese higher education",
                           "music educational technology" ,
                           "early childhood music education",
                           "childhood music education",
                           "performance-based music education" ,
                           "systematic instruction in music education",
                           "musical education" ,
                           "instrumental music education" ,
                           "music teacher education",
                           "art education"  ,
                           "special educational needs",
                           "guitar education" ,
                           "popular music education" ,
                           "generalist music education" ,
                           "music education aims" ,
                           "early music education" ,
                           "music education research",
                           "general music education" ,
                           "tertiary music education" )
                 ~ "music education",TRUE ~ KW))

# Religion
idx = grep("religi", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("black religion",
                           "spirituality/religion",
                           "religious celebrations",
                           "religion",
                           "religiosity",
                           "religious experience"  )
                 ~ "religious",TRUE ~ KW))

# Language
idx = grep("lang", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("language production",
                           "language processing" ,
                           "second language",
                           "music and language" ,
                           "natural language processing" ,
                           "context free language" ,
                           "language and music cognition" ,
                           "language musicology",
                           "language games",
                           "visual programming languages",
                           "statistical music language model",
                           "language and music",
                           "tonal language" ,
                           "tone language",
                           "american sign language" ,
                           "music-language connections",
                           "formal language complexity" ,
                           "language development",
                           "native language"  ,
                           "language prosody" ,
                           "artificial language learning"  ,
                           "language acquisition",
                           "language use" ,
                           "language analysis" ,
                           "psychology of language" ,
                           "use of language" ,
                           "first-language early reading abilities",
                           "foreign language" ,
                           "language of lyrics",
                           "foreign-language development",
                           "paralanguage songs" ,
                           "language impairment",
                           "second language learning")
                 ~ "language",TRUE ~ KW))

# Speech
idx = grep("speech", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("speech",
                           "speech-in-noise",
                           "speech prosody",
                           "musicalized speech",
                           "music and speech",
                           "speech and oral archives",
                           "expressive speech" ,
                           "sung speech",
                           "speech perception",
                           "joint speech" ,
                           "speech rhythm",
                           "perception of speech in noise",
                           "speech intonation",
                           "nonpropositional and propositional speech",
                           "pre-speech development" ,
                           "private speech",
                           "speech-to-song" ,
                           "l2 speech",
                           "dialogic speech",
                           "thai speech" )
                 ~ "speech",TRUE ~ KW))

# Prosocial
idx = c(grep("prosocial", data$KW),grep("pro-social", data$KW))
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("prosocial behavior" ,
                           "prosocial behaviors",
                           "prosocial skills" ,
                           "prosociality" ,
                           "prosocial",
                           "prosocial behaviour",
                           "intertemporal prosocial discounting" ,
                           "prosocial music",
                           "pro-social behavior",
                           "music with prosocial lyrics",
                           "prosocial information"  ,
                           "pro-social behaviour" )
                 ~ "speech",TRUE ~ KW))

# Para social
idx = grep("parasocial", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("parasocial interaction",
                           "parasocial relationship",
                           "parasocial relationships")
                 ~ "parasocial",TRUE ~ KW))

# Social
idx = grep("social", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("socially shared regulation of learning",
                          "social bonding"  ,
                           "social cognition",
                           "social relationships"  ,
                           "social dynamics",
                           "social engagement",
                           "social well-being",
                           "social evolution",
                           "social connection" ,
                           "social influence" ,
                           "social value",
                           "social interaction" ,
                           "social" ,
                           "social music cognition",
                           "social surrogacy",
                           "social and political values"   ,
                           "social cohesion" ,
                           "social disconnection" ,
                           "social behavior"  ,
                           "social entrainment",
                           "social-emotional competence" ,
                           "social aspects" ,
                           "musical sociality",
                           "social acceptance" ,
                           "sociality" ,
                           "social processes",
                           "social connectedness" ,
                           "social networks",
                           "social self",
                           "social surrogates" ,
                           "social affordances" ,
                           "social cognitive theory",
                           "socially engaged practice"  ,
                           "social fabric",
                           "social cognitive career theory"  ,
                           "social status" ,
                           "psychosocial functions"  ,
                           "social groups",
                           "social prescribing",
                           "social impact of music" ,
                           "social transformation",
                           "social anxiety",
                           "teacher social skills"   ,
                           "social influences" ,
                           "social media"  ,
                           "social contacts" ,
                           "social network map"   ,
                           "social context" ,
                           "social feedback" ,
                           "social behaviors during ensemble practice",
                           "social skills",
                           "social relatedness",
                           "social identity theory (sit)" ,
                           "social facilitation",
                           "musical socialization" ,
                           "social bonds",
                           "social anxiety disorder" ,
                           "social identity",
                           "social interactions" ,
                           "social support" ,
                           "music teacher socialization",
                           "evolutionary social psychology",
                           "social affiliation",
                           "social-emotional skills"  ,
                           "social phobia",
                           "psychosocial adaptation" ,
                           "social psychology" ,
                           "verbal/social persuasion" ,
                           "social class" ,
                           "social risk",
                           "social cure approach" ,
                           "social support scale",
                           "social representations"  ,
                           "social network analysis",
                           "social role theory",
                           "psychosocial risks" ,
                           "social constructivism" ,
                           "music and social cognition" ,
                           "emotional and social functioning" ,
                           "social wellbeing" ,
                           "social inclusion",
                           "social reinforcement",
                           "social presence",
                           "social integration" ,
                           "social comparison",
                           "social tags" ,
                           "psychosocial well-being",
                           "social responsibility" ,
                           "social uncertainty",
                           "social avoidance",
                           "socialization",
                           "social functions",
                           "social competence",
                           "social collaboration")
                 ~ "social",TRUE ~ KW))


# Mental health
idx = grep("mental health", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("mental health",
                           "mental health and the music industry",
                           "mental health interventions",
                           "mental health help-seeking" ,
                           "mental health care")
                 ~ "mental health",TRUE ~ KW))


# health
idx = grep("health", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("health musician" ,
                           "health care",
                           "health" ,
                           "health behaviors"  ,
                           "healthcare",
                           "healthy ageing" ,
                           "health-promoting behaviours" ,
                           "health promotion"  ,
                           "health-oriented preventive behavior",
                           "health-literacy" ,
                           "physical health"     ,
                           "health psychology",
                           "healthcare services" ,
                           "health outcomes"  ,
                           "public health"  ,
                           "health responsibility" ,
                           "occupational health"  ,
                           "general health" ,
                           "healthy adults"    ,
                           "job-related health problems")
                 ~ "health",TRUE ~ KW))

# art/music health
idx = grep("health", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("arts in health",
                           "music and health"  ,
                           "arts-and-health",
                           "healthy music use",
                           "unhealthy music use",
                           "vocal health" ,
                           "playing-related health problems",
                           "musician's health"  ,
                           "arts and health",
                           "health musicking" )
                 ~ "music health",TRUE ~ KW))

## Words related to children
idx = grep("child", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("children's music" ,
                           "children's musical preferences" ,
                           "children",
                           "child development" ,
                           "middle childhood",
                           "children's audience" ,
                           "children's use of singing voice" ,
                           "children's singing" ,
                           "early childhood music" ,
                           "children and music" ,
                           "music and children" ,
                           "children and adolescents",
                           "child personality",
                           "rules of children's songs",
                           "preschool children",
                           "childhood" ,
                           "parent-child communication",
                           "early childhood" ,
                           "elementary school children" ,
                           "parent-child interaction" ,
                           "young children" ,
                           "migrant children"  ,
                           "child to parent violence"  ,
                           "children's cognition"  ,
                           "children's drawings",
                           "state-trait anxiety inventory for children (staic)",
                           "parent-child interactions" )
                 ~ "children",TRUE ~ KW))

## Words related to infants / toddler 
idx = c( grep("infant", data$KW), grep("toddler", data$KW) )
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("mother-infant bonding",
                           "infants",
                           "infant" ,
                           "infant-directed singing" ,
                           "mother-infant interaction",
                           "infant communication",
                           "parent-infant groups",
                           "preterm infants",
                           "infant daily care" ,
                           "mother-infant bond" ,
                           "infants musical development"  ,
                           "maternal singing to infants" ,
                           "mother-infant vocal interaction",
                           "infants' overlapping vocalizations",
                           "mother-infant" ,
                           "toddlers")
                 ~ "infant",TRUE ~ KW))


## Words related to audience
idx = grep("audience", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("audience motion" ,
                           "audience",
                           "audience research" ,
                           "audience response"  ,
                           "technology-mediated audience participation (tmap)",
                           "global audiences" ,
                           "children's audience" ,
                           "audience expertise" ,
                           "audience behavior" ,
                           "artist-audience interaction",
                           "audiences",
                           "audience effects" )
                 ~ "audience",TRUE ~ KW))


## Words related to audience
idx = grep("concert", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("live concert",
                           "classical concert" ,
                           "concert experience",
                           "digital concert stream",
                           "concert research",
                           "research concerts",
                           "live concerts",
                           "digital concerts",
                           "concert situation" ,
                           "concert" ,
                           "concert band" ,
                           "concert attendance",
                           "mock concert",
                           "concerts" ,
                           "concert going"  )
                 ~ "concert",TRUE ~ KW))

## Words related to audience
idx = grep(" sing", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("live concert",
                           "classical concert" ,
                           "concert experience",
                           "digital concert stream",
                           "concert research",
                           "research concerts",
                           "live concerts",
                           "digital concerts",
                           "concert situation" ,
                           "concert" ,
                           "concert band" ,
                           "concert attendance",
                           "mock concert",
                           "concerts" ,
                           "concert going"  )
                 ~ "concert",TRUE ~ KW))


dim(data)
length(unique(data$KW))

#### Additional aggregating
# what about collapsing music performance and performance?
idx = grep("music performance", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("music performance",
                           "network music performance",
                           "networked music performance",
                           "generation of artificial music performances",
                           "repeats and skips in music performance",
                           "instrumental music performance",
                           "music performance evaluation",
                           "music performance assessment",
                           "virtuoso's music performance",
                           "ensemble music performance",
                           "optimal music performance")
                 ~ "performance",TRUE ~ KW))

# what about collapsing music perception and perception?
idx = grep("music perception", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("music perception",
                           "mmusic perception")
                 ~ "perception",TRUE ~ KW))

# what about collapsing music cognition and cognition?
idx = grep("music cognition", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("music cognition")
                 ~ "cognition",TRUE ~ KW))

# what about collapsing music emotion and emotion?
idx = grep("music emotion", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("music emotion",
                           "music emotions")
                 ~ "emotion",TRUE ~ KW))

# what about removing music
head(data)
data<-dplyr::filter(data,KW!="music")
data<-dplyr::filter(data,KW!="music psychology")


#-------
x1 <- count2category(data,
                     index = "GENDER_index",
                     str1 = "female",
                     str2 = "male")

FROM <- 1
TO <- 40

#### Figure 1 ----------

figure1 <- ggplot(data = x1[FROM:TO, ], aes(
  x = reorder(KW, Freq),
  y = prop,
  label = paste0("n=",Freq)
)) +
  geom_col(fill = 'grey70', color = 'grey10') +
  geom_text(nudge_y = .0167, size=3.0) +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(
    breaks = seq(0, 1, .1),
    labels = (seq(0, 1, .1)) * 100
  ) +
  scale_fill_grey() +
  coord_flip(ylim = c(.20,.80)) +
  ylab("% Female (first authors)") +
  xlab("Keyword (ranked)") +
  annotate("text", x = 20, y = 0.55, label = "NOT GOING TO BE USED", size=12,color="red",angle=45) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  theme_classic(base_size = 15) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

print(figure1)


head(x1)

all_females<-sum(x1$female)
all_males<-sum(x1$male)
x1$or<-NA
x1$or_LCI<-NA
x1$or_UCI<-NA

for (k in 1:nrow(x1)) {
  tmp<-data.frame(kw=c(x1$female[k],x1$male[k]),all=c(all_females,all_males))
  x1$or[k] <- as.numeric(effectsize::oddsratio(tmp)[1])
  x1$or_LCI[k] <- as.numeric(effectsize::oddsratio(tmp)[3])
  x1$or_UCI[k] <- as.numeric(effectsize::oddsratio(tmp)[4])
}
head(x1)

FROM <- 1
TO <- 40

#### Figure 2 ----------

figure2 <- ggplot(data = x1[FROM:TO, ], aes(
  x = reorder(KW, prop),
  y = prop,
  label = paste0("n=",Freq)
)) +
  geom_col(fill = 'grey70', color = 'grey10') +
  geom_text(nudge_y = .0197, size=3.0) +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(
    breaks = seq(0, 1, .1),
    labels = (seq(0, 1, .1)) * 100
  ) +
  scale_fill_grey() +
  coord_flip(ylim = c(.20,.70)) +
  ylab("% Female (all authors)") +
  xlab("Keyword (ranked)") +
  annotate("text", x = 20, y = 0.55, label = "NOT GOING TO BE USED", size=12,color="red",angle=45) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  theme_classic(base_size = 15) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

print(figure2)


#### odds ratio -------

figure3 <- ggplot(data = x1[FROM:TO, ], aes(
  x = reorder(KW, or),
  y = or,
  label = paste0("n=",Freq)
)) +
  geom_col(fill = 'grey80', color = 'grey10') +
  geom_errorbar(aes(ymin=or_LCI, ymax=or_UCI), width=.2, alpha=0.6,color="grey40",linetype="solid") +
  geom_text(y=.1,nudge_y = -.0997, size=2.25) +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(
    breaks = seq(0, 3.78, .25),
    limits = c(0,3.78),
    expand = c(0.001,0.01)
  ) +
  scale_fill_grey() +
  coord_flip(ylim = c(0,3.78)) +
  ylab("OR Female (all authors)") +
  xlab("Keyword (ranked)") +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "grey39") +
  theme_classic(base_size = 15) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

print(figure3)

tmp <- x1[FROM:TO, ]
dim(tmp)

nurturing_topics <- c(
tmp$Freq[which(tmp$KW=='development')],
tmp$Freq[which(tmp$KW=='social')],
tmp$Freq[which(tmp$KW=='music education')],
tmp$Freq[which(tmp$KW=='emotion regulation')],
tmp$Freq[which(tmp$KW=='well-being')],
tmp$Freq[which(tmp$KW=='mental health')],
tmp$Freq[which(tmp$KW=='music therapy')]
)
sum(nurturing_topics) / sum(tmp$Freq)

#### Save figures ----------------------
plotflag <- FALSE
if (plotflag) {
  ggsave(
    filename = 'figure1.png',
    plot = figure1,
    width = 8, height = 6, units = "in",
    dpi = 300
  )

  ggsave(
    filename = 'figure2.png',
    plot = figure2,
    width = 8, height = 6, units = "in",
    dpi = 300
  )

  ggsave(
    filename = 'figure3.png',
    plot = figure3,
    width = 8, height = 6, units = "in",
    dpi = 300
  )
}
