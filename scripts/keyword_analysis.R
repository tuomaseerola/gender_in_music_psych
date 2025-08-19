# keyword_analysis.R
# gender in music psych
# T. Eerola, 19/8/2025
# Status: In progress

#### Custom functions -------------------
source('scripts/count2category.R')
plotflag <- TRUE

## remove studies, keep articles (to avoid duplicating keywords)
D <- dplyr::filter(df, author_id == 'author1') # 3373

if (!dim(D)[1] == 3373) {
  print("incorrect number of observations! (Needs to be run at the level of studies, N=1360)")
  break
}

#### keyword analysis ------------------
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

length(KW)
length(GENDER_index)
length(year_index)

KW <- stringi::stri_trans_general(KW, "latin-ascii")
KW
KW<-stringr::str_trim(KW, side = "both")
KW<-tolower(KW)

#source('scripts/simplify_keywords.R')

data <- data.frame(KW,
                   GENDER_index,
                   year_index)
x1 <- count2category(data,
                     index = "GENDER_index",
                     str1 = "female",
                     str2 = "male")

FROM <- 1
TO <- 25

#### Figure 1 ----------

figure1 <- ggplot(data = x1[FROM:TO, ], aes(
  x = reorder(KW, prop),
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
  coord_flip(ylim = c(.25,.75)) +
  ylab("% Female (first authors)") +
  xlab("Keyword (ranked)") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  theme_classic(base_size = 15) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

print(figure1)

#### Alternatively where all authors contribute ------

#### keyword analysis ------------------
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

length(KW) # 42782
length(GENDER_index)
length(year_index)

KW <- stringi::stri_trans_general(KW, "latin-ascii")
KW
KW<-stringr::str_trim(KW, side = "both")
KW<-tolower(KW)

#source('scripts/simplify_keywords.R')

data <- data.frame(KW,
                   GENDER_index,
                   year_index)

x1 <- count2category(data,
                     index = "GENDER_index",
                     str1 = "female",
                     str2 = "male")
all_females<-sum(x1$female)
all_males<-sum(x1$male)
x1$or<-NA

for (k in 1:nrow(x1)) {
  tmp<-data.frame(kw=c(x1$female[k],x1$male[k]),all=c(all_females,all_males))
  x1$or[k] <- as.numeric(effectsize::oddsratio(tmp)[1])
}


FROM <- 1
TO <- 25

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
  coord_flip(ylim = c(.25,.75)) +
  ylab("% Female (all authors)") +
  xlab("Keyword (ranked)") +
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
#### Figure 2 ----------

figure3 <- ggplot(data = x1[FROM:TO, ], aes(
  x = reorder(KW, or),
  y = or,
  label = paste0("n=",Freq)
)) +
  geom_col(fill = 'grey70', color = 'grey10') +
  geom_text(nudge_y = -.0997, size=2.25) +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(
    breaks = seq(0, 2, .25),
    limits = c(0,2.05),
    expand = c(0.001,0.01)
  ) +
  scale_fill_grey() +
  coord_flip(ylim = c(0,2.05)) +
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
