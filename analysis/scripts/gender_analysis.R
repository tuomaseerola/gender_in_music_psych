# gender_analysis.R
# Filename: visualise_gender.R
# Author: T. Eerola, 6/4/2024
# Project: gender in music psychology
# Status: Complete

#### 1. attribute gender -------------------------------
cat("Attribute gender \n")
#remotes::install_github("lmullen/genderdata")
#install.packages("gender",repos = "http://cran.us.r-project.org")

library(gender)
#g<-gender(x, method = "ssa")
#d<-read.csv('WEIRD part cleaned Feb24.csv',header=TRUE)

d$firstn <- d$FirstAuthorName_cleaned
#head(d$firstn)
d$firstn <- tolower(d$firstn)
d$firstn <- str_replace_all(d$firstn,'-',' ')
d$firstn <- str_replace_all(d$firstn,'^[a-z ]+, ','')
d$firstn <- str_replace_all(d$firstn,' .*$','')
#table(d$firstn)

U <- sort(unique(d$firstn))
g <- gender(U, method = "ssa")
g <- data.frame(g)

## which names are missing?

ind <- U %in% g$name
missing<-U[!ind]
missing

# fixes
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Arthurs')]<-'yuko'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Williamson')]<-'victoria'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Annemieke')] <- 'Annemieke'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Scalas')] <- 'Francesca'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Dowling')] <- 'Jay'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Windsor')] <- 'Luke'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Beaman')] <- 'Philip'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Nicholson')] <- 'Riley'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Springer')] <- 'Gregory'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Windsor')] <- 'Luke'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'McAuley')] <- 'Devin'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Anta')] <- 'Fernando'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Zhang, J. Diana')] <- 'Diana'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Schellenberg')] <- 'Glenn'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Zijl')] <- 'Anemone'
d$firstn[str_detect(d$FirstAuthorName_cleaned,'Omar')] <- 'Ali'
d$FirstAuthorName_cleaned[str_detect(d$firstn,'hi')]

# new loop
U <- sort(unique(d$firstn))
g <- gender(U, method = "ssa")
g <- data.frame(g)

dim(g)
# ADD gender to data.frame
d$gender <- NA

for (k in 1:nrow(g)) {
  ID <- str_detect(g$name[k],paste0('^',d$firstn,'$'))
  d$gender[ID]<-g$gender[k]  
}

ind <- U %in% g$name
missing<-U[!ind]
missing

#### 2. attribute the rest of the gender --------------------
cat("Attribute names - part 2\n")
# devtools::install_github("kalimu/genderizeR")

run_once <- FALSE

if(run_once==TRUE){
  library(genderizeR)
  givenNames = findGivenNames(missing, progress = TRUE)
  length(missing)
  nrow(givenNames)
  write.csv(file = 'givennames.csv', givenNames)
}

givenNames <- read.csv(file = 'data/givennames.csv')

# ADD gender to data.frame
#d$gender<-NA
for (k in 1:nrow(givenNames)) {
  ID <- str_detect(givenNames$gender[k],paste0('^',d$firstn,'$'))
  d$gender[ID]<-givenNames$probability[k]  
}

still_missing<-is.na(d$gender)

x<-cbind(d$firstn,d$FirstAuthorName_cleaned,d$gender)
head(x,20)
x[still_missing,]

#### 3. Manual fixes to gender --------------
cat("Manual fixes\n")

d$gender[d$firstn=='Annemieke']<-'female'
d$gender[d$firstn=='loerch']<-'male'
d$gender[d$firstn=='raolo,']<-'male'
d$gender[d$firstn=='helen']<-'female'
d$gender[d$firstn=='oõshea,']<-'female'
d$gender[d$firstn=='zapata;']<-'female'
d$gender[d$firstn=='oõneill,']<-'female'
d$gender[d$firstn=='morwaread']<-'female'
d$gender[d$firstn=='muhipêsanal,']<-'male'

d$gender[d$firstn=='micalena']<-'female'
d$gender[d$firstn=='cochavit']<-'female'
d$gender[d$firstn=='soyogu']<-'male'
d$gender[d$firstn=='zyxcban']<-'male'
d$gender[d$firstn=='jotthi']<-'female'
d$gender[d$firstn=='tanchyuan']<-'female'

# fix wrongly attributed
d$gender[d$firstn=='simha']<-'male'
d$gender[d$firstn=='vinoo']<-'female'
d$gender[d$firstn=='zohar']<-'male'

#Hemming, Jan is male
# "Packer, Jan" female
# "Herbst, Jan-Peter"
# "Fruhauf, Jan" male
# "Stupacher, Jan" is male
## ashley
# Vanstone, Ashley male
# Warmbrodt, Ashley female

d$gender[d$FirstAuthorName_cleaned=='Packer, Jan']<-'female'
d$gender[d$FirstAuthorName_cleaned=='Herbst, Jan-Peter']<-'male'
d$gender[d$FirstAuthorName_cleaned=='Hemming, Jan']<-'male'
d$gender[d$FirstAuthorName_cleaned=='Fruhauf, Jan']<-'male'
d$gender[d$FirstAuthorName_cleaned=='Stupacher, Jan']<-'male'
d$gender[d$FirstAuthorName_cleaned=='Vanstone, Ashley']<-'male'
d$gender[d$FirstAuthorName_cleaned=='Warmbrodt, Ashley']<-'female'

x <- cbind(d$firstn,d$FirstAuthorName_cleaned,d$gender)
x[still_missing,]

#### 4. Report success -------------------------
cat("Report sucess \n")
cat(paste0('   No. of unattributed names: ',sum(is.na(d$gender))))

savedata <- TRUE
if(savedata==TRUE){
  saveRDS(d,file = 'data/enriched_dataset.rds')
}

#### 5. clean up ---------------------------
cat("\n Clean up and process complete! \n")
rm(x,U,g,ID,ind,missing)
