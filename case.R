library(utils)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidytext)
library(lubridate)

#READING FILES WITHOUT THE FIRST NUMBERING COLUMN
country_site <- read.csv("country_site.csv")[,-1]
randomized <- read.csv("randomized.csv")[,-1]
site_subject <- read.csv("site_subject.csv")[,-1]
e_nonPI_assessment<-read.csv("eligibility_nonPI_assessment.csv")[,-1]
e_PI_assessment <-read.csv("eligibility_PI_assessment.csv")[,-1]
e_PI_cv <-read.csv("eligibility_PI_criteria_violated.csv")[,-1]

#Join at ONE FILE
t <- full_join(e_nonPI_assessment,e_PI_assessment, by="subject") %>% full_join(e_PI_cv,by="subject")
tot <-randomized %>% full_join(t, by="subject")
totall <-site_subject %>% full_join(tot, by="subject")
all <- country_site %>% full_join(totall, by="site")

#EX 1,2

# using filtration for select desirable subject - using a pipe operator
all2 <- as_tibble(all)
grupy1<-levels(as.factor(all2$eligi_pi_assessment))
all3 <- all2 %>%
  filter(eligi_pi_assessment == "Ineligible" | cm_detail_type == "Signal") %>% 
  filter(!(cm_detail_type=="Signal" & eligi_pi_assessment == "Eligible"))

# contradictory data - subject, which should be considered
checkagain <-all2 %>% filter((cm_detail_type=="Signal" & eligi_pi_assessment == "Eligible"))

Again <- count(all3) #number of all ineligible cases 
ratio <- Again/count(all2) #ratio

#count ineligible cases in every country or site
#ineligible cases
df<-all3 %>% 
  group_by(country) %>%
  count()  
dff <-all3 %>%
  group_by(site) %>% 
  count() 
#eligible cases
df1 <- all2 %>%
  group_by(country) %>%
  count()
df2 <-all2 %>%
  group_by(site) %>% 
  count()

#site and country, which has the biggest problem with correct 
#randomization of subjects - is site/country with the highest ratio (ineligible/eligible)

full <-full_join(dff,df2,by=c("site"="site"))
full[is.na(full)] <- 0
full$ratio_site<- full$n.x/full$n.y
colMax_site <- which.max(full$ratio_site)
print(full[colMax_site,])


full_country <-full_join(df,df1,by=c("country"="country"))
full_country[is.na(full_country)] <- 0
full_country$ratio_country <- full_country$n.x/full_country$n.y
colMax_country <- which.max(full_country$ratio_country)
print(full_country[colMax_country,])



#EX 3,4
# using filtration for select desirable subject - using a pipe operator
all4 <- all2 %>%
  filter(cm_detail_type == "Missing data" ) %>%
  filter(!(eligi_pi_assessment == "Ineligible" & !is.na(eligi_pi_assessment))) 


Again_MD <- count(all4) #number of all ineligible
ratio_MD <- Again/count(all2) #ratio


df<-all4 %>% 
  group_by(country) %>%
  count()
dff <-all3 %>%
  group_by(site) %>% 
  count()
df1 <- all2 %>%
  group_by(country) %>%
  count()
df2 <-all2 %>%
  group_by(site) %>% 
  count()

full_MD <-full_join(dff,df2,by=c("site"="site"))
full_MD[is.na(full_MD)] <- 0
full_MD$ratio_site<- full_MD$n.x/full_MD$n.y
colMax_MD_site <- which.max(full_MD$ratio_site)
print(full[colMax_MD_site,])


full_country_MD <-full_join(df,df1,by=c("country"="country"))
full_country_MD[is.na(full_country_MD)] <- 0
full_country_MD$ratio_country <- full_country_MD$n.x/full_country_MD$n.y
colMax_MD_country <- which.max(full_country_MD$ratio_country)
print(full_country_MD[colMax_MD_country,])


#EX 5

#the most often violated eligibility criterion
MAX_CM<-all2 %>%
  group_by(cm_crit_num) %>%
  count() %>%
  filter(!(is.na(cm_crit_num)))
MAX_CMindeks <- which.max(MAX_CM$n)
MAX_CM[MAX_CMindeks,] %>%print()

MAX_PI<-all2 %>%
  group_by(pi_crit_num) %>%
  count() %>%
  filter(!(is.na(pi_crit_num)))
MAX_PIindeks <- which.max(MAX_PI$n)
MAX_PI[MAX_PIindeks,] %>% print()

#the most often violated eligibility criterion for both (PI, CM) 
crit<-if(MAX_PI[MAX_PIindeks,]$n > MAX_CM[MAX_CMindeks,]$n) {
  print( MAX_PI[MAX_PIindeks,])
} else {
  print(MAX_CM[MAX_CMindeks,])
}




#EX 6
all2$rnd_date <- as_date(all2$rnd_date)
all2 <- all2 %>% filter(!(is.na(rnd_date))) 
all3$rnd_date <- as_date(all3$rnd_date)
all3 <- all3 %>% filter(!(is.na(rnd_date))) 
with(all3, difftime(max(all3$rnd_date), min(all3$rnd_date)))
with(all2, difftime(max(all2$rnd_date), min(all2$rnd_date)))

#SUMMARY 
sink("output3.csv")
print(paste0("number of all ineigible cases: ", Again))
print(paste0("ratio: ", ratio))
print(paste0("Check again a patient number:", checkagain$subject))                 
print(paste0("country has the biggest problem with correct randomization of subjects: ",full_country[colMax_country,"country"]))
print(paste0("site has the biggest problem with correct randomization of subjects: ",full[colMax_site,"site"]))
print(paste0("number of all missing data cases: ", Again_MD , "ratio: ", ratio_MD))                 
print(paste0("country has the biggest problem with missing data: ",full_country_MD[colMax_MD_country,"country"]))
print(paste0("site has the biggest problem with missing data: ",full_MD[colMax_MD_site,"site"]))
print(paste0("the most often violated eligibility criterion is: ",crit[,"cm_crit_num"]))
print("The incorrected randomization was for the most part")
sink()
write.csv(full, file="ineigible_cases_site.csv")
write.csv(full_country, file="ineigible_cases_country.csv")
write.csv(all3, file="ineigible_cases_all.csv")
write.csv(full_MD, file="missing_data_site.csv")
write.csv(full_country_MD, file="missing_data_country.csv")
write.csv(all4, file="missing_data_all.csv")
