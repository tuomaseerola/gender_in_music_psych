# Filename: name_gender_fixes.R
# Author: T. Eerola, 9/8/2025
# Project: gender in music psychology
# Status: Complete

#### 1. Manual fixes to gender by authors (TE and AC) --------------
cat("Manual fixes\n")

# Fix wrongly attributed
# from the first name only
df$Gender[df$first_name=='Simha'] <- 'male' # TE
df$Gender[df$first_name=='Zohar']<-'male'   # TE

# from a full name
df$Gender[df$full_name=='Ashley D. Vanstone']<-'male' # TE
df$Gender[df$full_name=='Andrea Schiavio']<-'male'    # TE

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
