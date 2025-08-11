# Gonzalez et al 2009
# TABLE 2
Location <- c("US","UK","Germany","Canada","Netherlands","Australia","Spain","Italy","France","Belgium","Switzerland","Japan","Sweden","Israel","Brazil","Finland","China","Norway","New Zealand","Austria","Turkey","Portugal","South Africa","Others")
prop <- c(46.5,43.6,36.5,48.0,41.5,49.1,45.0,52.8,46.1,34.7,35.9,29.8,42.3,49.1,56.1,55.8,36.8,37.8,44.2,42.2,56.9,47.9,50.2,46.1)
n <- c(35980,6280,4726,4406,3205,2866,2566,1529,1500,1036,1006,793,726,689,610,600,473,473,468,365,348,303,281,3184)
gonzalez <- data.frame(Country=Location,prop,n=n)

gonzalez$male<- round((100 - gonzalez$prop)/100 * gonzalez$n)
gonzalez$female<- round((gonzalez$prop)/100 * gonzalez$n)
head(gonzalez)

total_male <- sum(gonzalez$male)
total_female <- sum(gonzalez$female)
total_female
#### First position
# if males were in the first position, from p. 5
male <- 23128 
female <- 8804
t <- matrix(NA,2,2)
t[1,1]<-female; t[1,2] <- male
t[2,1]<-total_female-female; t[2,2]<- total_male-male
colnames(t)<-c("female","male")
rownames(t)<-c("first","other")
t
or <- effectsize::oddsratio(t) # 0.73
or
chisq.test(t,simulate.p.value = FALSE)

# if the papers were signed by a women the first position, from p. 5
male<- 11473 
female <- 20589
t <- matrix(NA,2,2)
t[1,1]<-female; t[1,2] <- male
t[2,1]<-total_female-female; t[2,2]<- total_male-male
colnames(t)<-c("female","male")
rownames(t)<-c("first","other")
t
or <- effectsize::oddsratio(t) # 0.73
or
chisq.test(t,simulate.p.value = FALSE)



