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


kw_num = data %>% count(KW)


# # # # # # instead of year, keep first author id
alt <- TRUE

if(alt==TRUE){

  KW <- NULL
  d_index <- NULL
  GENDER_index <- NULL
  year_index <- NULL # year

for (k in 1:nrow(df)) {
  tmp <- df$AUTHOR_KEYWORDS[k]
  GENDER <- as.character(df$Gender[k])
  y <- df$unique_name[k]
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

# # # # # #




# length(unique(data$KW))

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

idx = grep("absolute", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("absolute pitch",
                           "absolute pitch shift",
                           "absolute pitch (ap)")
                 ~ "absolute pitch",TRUE ~ KW))




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

# teaching
idx = grep("teaching", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("music teaching",
                           "instrumental teaching and learning",
                           "teaching beliefs" ,
                           "instrumental music teaching",
                           "teaching behaviours",
                           "teaching styles",
                           "studio music teaching" ,
                           "scholarship of teaching and learning",
                           "teaching methods" ,
                           "instrumental learning and teaching",
                           "teaching effectiveness",
                           "teaching" ,
                           "piano teaching",
                           "conceptions of teaching" ,
                           "teaching experience" ,
                           "instrumental teaching" ,
                           "applied teaching",
                           "instrumental learning/teaching" ,
                           "peer teaching and learning" ,
                           "teaching techniques"      )
                 ~ "teaching",TRUE ~ KW))

# learning
idx = c( grep("audio-visual", data$KW),  grep("audiovisual", data$KW), grep("audio/v", data$KW))
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("audio-visual",
                           "audio-visuals",
                           "audio-visual perception",
                           "audio-visual processing",
                           "audio-visual aids",
                           "audiovisual interaction",
                           "audiovisual processing" ,
                           "audiovisual perception",
                           "audiovisual" ,
                           "audiovisual integration",
                           "audio/visual" )
                 ~ "audio-visual",TRUE ~ KW))


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


## Words related to concert
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

## Words related to memory
idx = grep("memor", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("analytical memory",
                           "aural memory",
                           "kinesthetic memory" ,
                           "musical memory" ,
                           "visual memory" ,
                           "memory",
                           "working memory capacity"  ,
                           "memorisation strategies" ,
                           "musical semantic memories",
                           "autobiographical memory" ,
                           "tonal memory" ,
                           "features memory",
                           "music-evoked autobiographical memory" ,
                           "emotional memories",
                           "memory bias" ,
                           "music-evoked memories"  ,
                           "spontaneous autobiographical memories" ,
                           "memory for music" ,
                           "musical tempo memory" ,
                           "phonological working memory (wm)" ,
                           "short-term memory" ,
                           "melodic memory"  ,
                           "recall memory" ,
                           "singing from memory" ,
                           "tone memory" ,
                           "working memory"  ,
                           "musical working memory",
                           "visuospatial working memory",
                           "episodic memory" ,
                           "memory for harmony" ,
                           "schematic memory",
                           "veridical memory" ,
                           "auditory memory",
                           "verbal working memory"  ,
                           "music memory",
                           "auditory short-term memory" ,
                           "flashbulb memory"  ,
                           "memory encoding"   ,
                           "implicit vs. explicit memory",
                           "true and false memories" ,
                           "music memorization" ,
                           "semantic memory"  ,
                           "memory diary"  ,
                           "implicit memory"   ,
                           "working memory for auditory material"  ,
                           "memory improvement",
                           "recognition memory",
                           "false memory" ,
                           "meta-memory",
                           "expert memory" ,
                           "mesial temporal lobe memory system"  ,
                           "pitch memory" ,
                           "memorisation",
                           "memory for melody" ,
                           "collective memory" ,
                           "item-source memory",
                           "analytic memory" ,
                           "kinaesthetic memory" ,
                           "involuntary memory" ,
                           "memories of body",
                           "lyrics memorization",
                           "memory recall",
                           "long-term memory" ,
                           "echoic memory" ,
                            "music-evoked autobiographical memories",
                           "memorized music",
                           "visual working memory",
                           "memories" ,
                           "motor memory",
                           "visuospatial memory" ,
                           "performance memory",
                           "very long-term memory" ,
                           "short term memory" ,
                           "autobiographical memories" ,
                           "verbal memory" ,
                           "long-term memory of music",
                           "involuntary autobiographical/semantic memory (iam/ism)",
                           "visual-spatial working memory" ,
                           "memory errors" ,
                           "working memory model" ,
                           "flashbulb memories"   ,
                           "play from memory"  ,
                           "memorization"  ,
                           "context-dependent memory" ,
                           "memory span" )
                 ~ "concert",TRUE ~ KW))




## Words related to listening
idx = grep("listening", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("everyday music listening",
                           "music listening",
                           "listening",
                           "listening habits",
                           "solitary music listening",
                           "everyday listening",
                           "music listening behavior",
                           "listening duration" ,
                           "listening experiment",
                           "modern musical listening",
                           "listening time",
                           "adaptive functions of music listening",
                           "music listening test" ,
                           "focused listening",
                           "dichotic listening",
                           "listening engagement" ,
                           "music-listening" ,
                           "listening preferences",
                           "listening behaviors" ,
                           "listening modes" ,
                           "listening strategies",
                           "classical music listening" ,
                           "passive listening",
                           "focused music listening" ,
                           "music listening experiences"  ,
                           "listening comprehension" ,
                           "typical music listening situations",
                           "functions of music listening"  ,
                           "listening experience" ,
                           "re-listening",
                           "listening reasons" ,
                           "personal music listening" ,
                           "listening device",
                           "sad music listening",
                           "listening behavior" ,
                           "music listening preferences" ,
                           "listening maps" )
                 ~ "listening",TRUE ~ KW))


## Words related to attention
idx = grep("attention", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("attention",
                           "focus of attention",
                           "attentional resources",
                           "attentional focus" ,
                           "visual attention",
                           "audio-visual perception and attention",
                           "supramodal attentional system",
                           "divided attention" ,
                           "attention economy",
                           "joint attention",
                           "temporal attention",
                           "attentional bias" ,
                           "attentional disengagement",
                           "attentional engagement" ,
                           "attention deficit hyperactivity disorder",
                           "attentional control theory",
                           "visuospatial attention",
                           "attention states",
                           "selective attention")
                 ~ "attention",TRUE ~ KW))

## Words related to computing
idx = grep("comput", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("human-computer interaction" ,
                           "computational thinking",
                           "computing" ,
                           "computational approach",
                           "computational musicology" ,
                           "computational music analysis",
                           "computer music",
                           "computational analysis",
                           "computer-aided composition (cac)",
                           "music computing",
                           "computational creativity",
                           "movement and computing",
                           "quantum computing" ,
                           "computer-aided composition",
                           "computer-assisted composition",
                           "computational music",
                           "affective computing",
                           "computational ethnomusicology",
                           "computational methods",
                           "computational models" ,
                           "computer-based musicology",
                           "computational modeling",
                           "computer-generated music",
                           "computer-assisted musical analysis",
                           "computation",
                           "computerized test" ,
                           "computer music designer",
                           "computing in musicology",
                           "computational modelling",
                           "computer games" ,
                           "computer-based assessment" ,
                           "computer software design" )
                 ~ "computation",TRUE ~ KW))

## Words related to cross modal
idx = c(grep("crossmodal", data$KW),grep("cross-modal", data$KW))
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c( "crossmodal relationships",
                            "crossmodal correspondences",
                            "crossmodal" ,
                            "crossmodal correspondence" ,
                            "crossmodal associations",
                            "crossmodal integration",
                            "cross-modal correspondences",
                            "cross-modal",
                            "cross-modal rating",
                            "cross-modality",
                            "cross-modal matching",
                            "cross-modal associations",
                            "cross-modal perception" ,
                            "cross-modal correspondence",
                            "cross-modal emotion perception",
                            "cross-modal interaction",
                            "cross-modal integration")
                 ~ "cross-modal",TRUE ~ KW))

## Words related to evolution
idx = grep("evolution", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("human evolution",
                           "cultural evolution",
                           "evolution" ,
                           "evolution of music",
                           "hominin evolution" ,
                           "evolutionary optimisation" ,
                           "music evolution",
                           "baldwinian evolution"   ,
                           "gene-culture co-evolution" ,
                           "music and evolution" ,
                           "co-evolutionary simulation",
                           "origin and evolution of music")
                 ~ "evolution",TRUE ~ KW))


## Words related to genetics
idx = grep("genet", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("genetic algorithm",  "phylogenetic trees",  "genetic criticism" ,  "microgenetic method")
                 ~ "genetics",TRUE ~ KW))


## Words related to improvisation
idx = grep("improv", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("improvisation",
                           "sung improvisation",
                           "musical improvisation",
                           "machine improvisation",
                           "free music improvisation",
                           "professionalimprovisers",
                           "jazz improvisation",
                           "cognitive improvement",
                           "taksim (improvisation)",
                           "improvement",
                           "free improvisation",
                           "music improvisation",
                           "improvised song endings",
                           "improvising",
                           "improvise")
                 ~ "improvisation",TRUE ~ KW))

## Words related to personality
idx = grep("personality", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("personality",
                           "personality traits" ,
                           "big five personality",
                           "borderline personality disorder",
                           "music and personality",
                           "musician personality",
                           "type d personality" ,
                           "personality perception",
                           "personality traits of pianists")
                 ~ "personality",TRUE ~ KW))


## Words related to physiology
idx = grep("physiol", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("psychophysiology",
                           "physiological evidence",
                           "physiological sensing" ,
                           "physiology"  ,
                           "psychophysiological responses",
                           "physiological stress",
                           "music physiology",
                           "neurophysiology"  ,
                           "psychophysiological responses to music",
                           "physiological measurement" ,
                           "physiological parameters",
                           "physiological" ,
                           "physiological responses",
                           "physiological reactivity" )
                 ~ "physiology",TRUE ~ KW))



## Words related to harmony
idx = grep("harmony", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("harmony",
                           "romantic harmony",
                           "harmony analysis",
                           "functional harmony",
                           "harmony perception",
                           "jazz harmony",
                           "rock harmony",
                           "vertical harmony")
                 ~ "harmony",TRUE ~ KW))

## Words related to expertise
idx = c(grep("expertise", data$KW))
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("musical expertise",
                           "expertise",
                           "music expertise",
                           "musical performance and expertise",
                           "sound expertise",
                           "motor expertise",
                           "musikalische expertise",
                           "visual expertise",
                           "expertise levels" ,
                           "expertise/familiarity" ,
                           "expertise development")
                 ~ "expertise",TRUE ~ KW))

## Words related to  modeling
idx = c(grep("modeling", data$KW),grep("modelling", data$KW))
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("bayesian modeling",
                           "lda-based topic modeling" ,
                           "causal modeling",
                           "structural equation modeling",
                           "modeling",
                           "predictive modeling" ,
                           "cognitive modeling",
                           "krumhansl's modeling" ,
                           "probabilistic modeling",
                           "latent variable modeling",
                           "modeling musicality" ,
                           "multilevel modeling" ,
                           "hierarchical linear modeling",
                           "topic modelling",
                           "conceptual modelling" ,
                           "listener modelling" ,
                           "biomechanical modelling",
                           "item response modelling",
                           "performance modelling" ,
                           "melodic modelling",
                           "segmentation modelling" ,
                           "structural equation modelling (sem)",
                           "structural equation modelling")
                 ~ "modelling",TRUE ~ KW))


## Words related to AI
idx = c(grep("machine", data$KW))
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("machine learning" ,
                           "applied machine learning",
                           "machine-learning")
                 ~ "machine learning",TRUE ~ KW))



## Words related to individual differences
idx = c(grep("individual", data$KW))
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("individual differences",
                           "individual difference",
                           "individual_differences")
                 ~ "individual differences",TRUE ~ KW))


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

data = data %>% mutate(
  KW = case_when(KW %in% c("emotions")
                 ~ "emotion",TRUE ~ KW))

# Music preference
idx = grep("musical preference", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("musical preference",
                           "musical preferences")
                 ~ "music preference",TRUE ~ KW))
# Music training
idx = grep("musical training", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("musical training",
                           "school-based musical training","early musical training","age of musical training onset")
                 ~ "music training",TRUE ~ KW))

# Musician
idx = grep("musicians", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("musicianship", "expert musicianship", "professional orchestral musicians", "musicians", "professional musicians", "nonmusicians", "performing musicians", "musicians' injuries", "high-level musicians", "musicians' perspectives", "young musicians", "self-taught musicians", "musicians' medicine", "life-span musicianship", "seasoned musicians", "types of musicians", "conservatory musicians", "amateur musicians", "differences between musicians and nonmusicians", "professional popular musicians", "non-musicians", "musicians' hearing", "community musicians", "musicians' wellbeing", "expert musicians", "advanced musicians", "orchestral musicians", "orchestra musicians", "jazz musicians", "musicians in hospitals", "freelance musicians", "musicians' lifestyles","musician advantage","professional musician","musician effect","jazz musician","classical musician","musician bodily expression","amateur musician")
                 ~ "musician",TRUE ~ KW))

# Musician
idx = grep("well-being", data$KW)
unique(data$KW[idx])
data = data %>% mutate(
  KW = case_when(KW %in% c("well-being", "mother well-being", "psychological well-being", "music and well-being","eudaimonic well-being","subjective well-being","human well-being")
                 ~ "wellbeing",TRUE ~ KW))


# what about removing music
head(data)
data<-dplyr::filter(data,KW!="music")
data<-dplyr::filter(data,KW!="music psychology")


length(data$KW)
length(unique(data$KW))

#kw_num = data %>% count(KW) %>% arrange(n)
kw_num = data %>% count(KW) %>% arrange(desc(n))

# # # # #
if(alt==TRUE){
  keyword_counts <- data %>%
    count(KW, sort = TRUE, name = "keyword_count")

  data_prominent <- data %>%
    inner_join(keyword_counts, by = "KW") %>%
    filter(keyword_count >= 40) # Threshold for prominence

  top_authors_by_keyword <- data_prominent %>%
    count(KW, year_index, name = "count") %>%
    group_by(KW) %>%
    slice_max(count, n = 3, with_ties = FALSE) %>%
    arrange(KW, desc(count))
#  print(top_authors_by_keyword[top_authors_by_keyword$KW=="performance expression",])
#  print(top_authors_by_keyword[top_authors_by_keyword$KW=="emotion",])
#  print(top_authors_by_keyword[top_authors_by_keyword$KW=="emotion regulation",])
#  print(top_authors_by_keyword[top_authors_by_keyword$KW=="performance",])

  write.csv(top_authors_by_keyword,"top_authors_by_keyword.csv",row.names=FALSE)

}
# # # # #



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

#print(figure1)


#head(x1)

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

#print(figure2)


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
#print("Nurturing keywords/topics:")
#print(sum(nurturing_topics) / sum(tmp$Freq))

#### Save figures ----------------------
plotflag <- FALSE
if (plotflag) {
  # ggsave(
  #   filename = 'figure1.png',
  #   plot = figure1,
  #   width = 8, height = 6, units = "in",
  #   dpi = 300
  # )
  #
  # ggsave(
  #   filename = 'figure2.png',
  #   plot = figure2,
  #   width = 8, height = 6, units = "in",
  #   dpi = 300
  # )

  ggsave(
    filename = 'figure3.png',
    plot = figure3,
    width = 8, height = 6, units = "in",
    dpi = 300
  )
}
