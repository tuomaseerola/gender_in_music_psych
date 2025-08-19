# Filename: name_gender_fixes.R
# Author: T. Eerola, 9/8/2025
# Project: gender in music psychology
# Status: To be checked

#### 1. Manual fixes to gender by authors (TE and AC) --------------
cat("Manual fixes\n")

# Fix wrongly attributed
# from the first name only
df$Gender[df$first_name=='Simha'] <- 'male' # TE
df$Gender[df$first_name=='Zohar']<-'male'   # TE

# from a full name
df$Gender[df$full_name=='Ashley D. Vanstone']<-'male' # TE
df$Gender[df$full_name=='Andrea Schiavio']<-'male'    # TE
df$Gender[df$full_name=='J. Riikka Ahokas']<-'female'    # TE 
df$Gender[df$full_name=='J. Ginsborg']<-'female'    # TE
df$Gender[df$last_name=="graham-jackson"] <- "female" # TE

# few names encoded as initials (Ahokas and Ginsborg), but others as well
# but the gender of these are correct
df$full_name[df$last_name=="Schellenberg"]
df$full_name[df$last_name=="Dowling"]
df$full_name[df$last_name=="Neill"]
df$full_name[df$last_name=="Springer"]
df$full_name[df$last_name=="Anta"]
df$full_name[df$last_name=="McAuley"]
df$full_name[df$last_name=="Ter Bogt"]
df$full_name[df$last_name=="Fitch"]
df$full_name[df$last_name=="Windsor"]
df$full_name[df$last_name=="de Haas"]
df$full_name[df$full_name=="B. S. Gupta"]
df$full_name[df$last_name=="Fung"] # several variants and two persons
df$full_name[df$last_name=="Beaman"] 
df$full_name[df$last_name=="Yates"] 
df$full_name[df$last_name=="Zhang"] 
df$full_name[df$last_name=="Gaydecki"] 
df$full_name[df$last_name=="Ali"] 
df$full_name[df$last_name=="Lloyd"] 
df$full_name[df$last_name=="graham-jackson"] 


df$Gender[df$full_name=='Andrea Ravignani']<-'male' # AMC
df$Gender[df$full_name=='Alexis Kirke']   <-'male' # AMC
df$Gender[df$full_name=='Toni Bechtold']  <-'male' # AMC
df$Gender[df$full_name=='Gopala Koduri']  <-'male' # AMC
df$Gender[df$full_name=='Hani Yehia']     <-'male' # AMC
df$Gender[df$full_name=='Jolan Kegelars'] <-'male' # AMC
df$Gender[df$full_name=='Li Hu']          <-'male' # AMC
df$Gender[df$full_name=='N. Ruiz-Reyes']  <-'male' # N. -> Nicolas? AMC
df$Gender[df$full_name=='Nori Jacoby']    <-'male' #  AMC
df$Gender[df$full_name=='Archia Cont']    <-'male' #  AMC
df$Gender[df$full_name=='Ashlin Roy']     <-'male' #  AMC
df$Gender[df$full_name=='Chuansheng Chen']<-'male' #  AMC
df$Gender[df$full_name=='Clude Alain']    <-'male' # typo, should be Claude Alain AMC
df$Gender[df$full_name=='Hao-Min Liu']    <-'male' #  AMC
df$Gender[df$full_name=='Haruka Shoda']   <-'male' #  AMC
df$Gender[df$full_name=='Hirokata Fukushima']<-'male' #  AMC
df$Gender[df$full_name=='Ji Kim']  <-'male' #  AMC
df$Gender[df$full_name=='Junqi Deng']<-'male' #  AMC
df$Gender[df$full_name=='Li Su']<-'male' #  AMC
df$Gender[df$full_name=='Linglan Zhu']<-'male' #  AMC
df$Gender[df$full_name=='Linh Nghiem']<-'male' #  AMC
df$Gender[df$full_name=='Nery Borges']<-'male' #  AMC
df$Gender[df$full_name=='Vesa Välimäki']<-'male' #  AMC
df$Gender[df$full_name=='Yi-Hsuan Yang']<-'male' #  AMC
df$Gender[df$full_name=='Yune Lee']<-'male' #  AMC



#### Diagnostic output ------
df$composite_name <- paste(df$first_name, df$last_name, sep = " ")
df %>%
  group_by(Gender, composite_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(Gender,desc(n)) -> output

write_file <- FALSE
if(write_file==TRUE){
  write.csv(output,'check_gender.csv',row.names = FALSE)
}
