load("CCES16_Common_OUTPUT_Feb2018_VV.RData")

################### INITIAL CLEANING ########################
library(dplyr)

#Selecting the above mentioned variables and filtering the NA's while selecting
obama = x[x$CC16_326 == 'Barack Obama' & is.na(x$CC16_326)== FALSE & x$tookpost == 'Yes' ,c('commonweight_vv_post', 'tookpost', 'gender', 'educ','race', 'pid7', 'CC16_326', 'CC16_410a', 'CC16_331_1','CC16_331_2', 'CC16_331_3', 'CC16_331_7')]

cols = c("SurveyWeight", "TookPost", "Gender", "Education", "Race", "PID7", "Vote_2012", "Vote_2016","GrantLegalStatus", "IncreaseBorderPatrols", "Dreamers", "DeportIllegal")

names(obama) = cols

#Creating a VotedForTrump variable to get the DV (1 = Voted for Trump, 0 = Didn't vote for Trump)
obama$VotedForTrump = ifelse(obama$Vote_2016 == 'Donald Trump (Republican)', 1, 0)

#Recoding the race variable using dplyr
obama$Race = recode(obama$Race, "White"="White", "Black"="Black", "Hispanic"="Hispanic", .default = "Other" )

#Recoding the Immigration Attitude variables 
obama$GrantLegalStatus = as.numeric(as.character(recode(obama$GrantLegalStatus, "Yes"= 1, "No"= 0)))
obama$IncreaseBorderPatrols = as.numeric(as.character(recode(obama$IncreaseBorderPatrols, "Yes"= 1, "No"= 0)))
obama$Dreamers = as.numeric(as.character(recode(obama$Dreamers, "Yes"= 1, "No"= 0)))
obama$DeportIllegal = as.numeric(as.character(recode(obama$DeportIllegal, "Yes"= 1, "No"= 0)))

#Creating the new immigration Attitude score variable
obama$ImmigrationAttitudeScore = obama$DeportIllegal+obama$IncreaseBorderPatrols+obama$Dreamers+obama$GrantLegalStatus

#Removing the missing values from the variables
obama = subset(obama, !is.na(obama$Vote_2016))
obama = subset(obama, PID7!= "Not sure")

write.csv(obama, "Obama_Trump1.csv") #Saving the file is important to get the row numbers for adding the new variables. Do save before running!

#################### PRE-PROCESSING ########################
obama = read.csv("Obama_Trump1.csv")

#Selecting the media sources variable
Obama_Trump = data.frame(obama, loc_nat = x$CC16_300b[obama$X], newspaper = x$CC16_300_3[obama$X], blog = x$CC16_300_1[obama$X],tv = x$CC16_300_2[obama$X],radio = x$CC16_300_4[obama$X], social_media = x$CC16_300_5[obama$X], none = x$CC16_300_6[obama$X] )

#Remove missing values
Obama_Trump = subset(Obama_Trump, !is.na(loc_nat))

#Recoding them to numeric form for getting index
Obama_Trump$newspaper<-recode(Obama_Trump$newspaper,'Yes'=1 , 'No'=0)
Obama_Trump$blog<-recode(Obama_Trump$blog,'Yes'=1,'No'=0)
Obama_Trump$tv<-recode(Obama_Trump$tv,'Yes'=1,'No'=0)
Obama_Trump$radio<-recode(Obama_Trump$radio,'Yes'=1,'No'=0)
Obama_Trump$social_media<-recode(Obama_Trump$social_media,'Yes'=1,'No'=0)

#Converting media variables to numeric form
Obama_Trump$tv<-as.numeric(as.character(Obama_Trump$tv))
Obama_Trump$newspaper<-as.numeric(as.character(Obama_Trump$newspaper))
Obama_Trump$blog<-as.numeric(as.character(Obama_Trump$blog))
Obama_Trump$social_media<-as.numeric(as.character(Obama_Trump$social_media))
Obama_Trump$radio<-as.numeric(as.character(Obama_Trump$radio))

#Creating the media index variable
media_index = rowSums(Obama_Trump[17:21])
Obama_Trump$mediaindex<-media_index

#Adding the family income and employment status as variables 
Obama_Trump$family_income = x$faminc[Obama_Trump$X]
Obama_Trump$employ_status = x$employ[Obama_Trump$X]

library(car)

#Creating the unemployed status variable as a binary variable
Obama_Trump$unemployed<-recode(Obama_Trump$employ_status,"c('Unemployed','Temporarily laid off')=1;c('Full-time','Homemaker','Other','Part-time','Permanently disabled','Retired','Student')=0")

#Converting teh family income to character form
Obama_Trump$family_income = as.character(Obama_Trump$family_income)

#Remove rows with family income = Prefer not to say
Obama2 = subset(Obama_Trump, Obama_Trump$family_income!= "Prefer not to say")

#Remove rows with family income = $150000 or more
Obama3 = subset(Obama2, Obama2$family_income != "$150,000 or more")


#Recoding the family income to make it numeric
Obama3$family_income[which(Obama3$family_income == "Less than $10,000" )] = 0
Obama3$family_income[which(Obama3$family_income == "$10,000 - $19,999" )] = 1
Obama3$family_income[which(Obama3$family_income == "$20,000 - $29,999" )] = 2
Obama3$family_income[which(Obama3$family_income == "$30,000 - $39,999" )] = 3
Obama3$family_income[which(Obama3$family_income == "$40,000 - $49,999" )] = 4
Obama3$family_income[which(Obama3$family_income == "$50,000 - $59,999" )] = 5
Obama3$family_income[which(Obama3$family_income == "$60,000 - $69,999" )] = 6
Obama3$family_income[which(Obama3$family_income == "$70,000 - $79,999" )] = 7
Obama3$family_income[which(Obama3$family_income == "$80,000 - $99,999" )] = 8
Obama3$family_income[which(Obama3$family_income ==  "$100,000 - $119,999" )] = 9
Obama3$family_income[which(Obama3$family_income ==  "$120,000 - $149,999" )] = 10
Obama3$family_income[which(Obama3$family_income ==  "$150,000 - $199,999" )] = 11
Obama3$family_income[which(Obama3$family_income ==  "$200,000 - $249,999" )] = 12
Obama3$family_income[which(Obama3$family_income ==  "$250,000 - $349,999" )] = 13
Obama3$family_income[which(Obama3$family_income ==  "$350,000 - $499,999" )] = 14
Obama3$family_income[which(Obama3$family_income ==  "$500,000 or more" )] = 15


#Saving the file as a CSV
write.csv(Obama3,'Obama_Trump2.csv') #This saving is optional. But do it just incase you decide to play around with more variables.

Obama_Trump = read.csv("Obama_Trump2.csv")


#################### ADDING OF NEW VARIABLES ###################### 

Obama_Trump$Religion = x$pew_religimp[Obama_Trump$X]
Obama_Trump$Economy = x$CC16_302[Obama_Trump$X]
Obama_Trump$TerrorCamp = x$CC16_414_2[Obama_Trump$X]

#Removing the NA's 
Obama_Trump = subset(Obama_Trump, Economy!= "Not sure")
Obama_Trump$Economy = factor(Obama_Trump$Economy, ordered = F)

## Recoding the Terror Camp values
Obama_Trump$TerrorCamp = ifelse(Obama_Trump$TerrorCamp == "Yes", 1,0)

#Recoding the Religion
Obama_Trump$Religion = as.character(Obama_Trump$Religion)
Obama_Trump$Religion[which(Obama_Trump$Religion == "Not at all important")] = 0 
Obama_Trump$Religion[which(Obama_Trump$Religion == "Not too important")] = 1 
Obama_Trump$Religion[which(Obama_Trump$Religion == "Somewhat important")] = 2 
Obama_Trump$Religion[which(Obama_Trump$Religion == "Very important")] = 3 
Obama_Trump$Religion = as.numeric(Obama_Trump$Religion)

#Recoding PID7

Obama_Trump$PID7_new[which(Obama_Trump$PID7 == "Strong Democrat")] = -3
Obama_Trump$PID7_new[which(Obama_Trump$PID7 == "Strong Republican")] = 3
Obama_Trump$PID7_new[which(Obama_Trump$PID7 == "Lean Democrat")] = -2
Obama_Trump$PID7_new[which(Obama_Trump$PID7 == "Lean Republican")] = 2
Obama_Trump$PID7_new[which(Obama_Trump$PID7 == "Not very strong Republican")] = 1
Obama_Trump$PID7_new[which(Obama_Trump$PID7 == "Not very strong Democrat")] = -1
Obama_Trump$PID7_new[which(Obama_Trump$PID7 == "Independent")] = 0

Obama_Trump$Economy<-relevel(Obama_Trump$Economy,ref="Stayed about the same")
Obama_Trump$Race<-relevel(Obama_Trump$Race,ref="White")
Obama_Trump$Education<-relevel(Obama_Trump$Education,ref="Some college")

write.csv(Obama_Trump, "Obama_Trump_FINALLY_FINAL.csv")



##################### INITIAL ANALYSIS AND CORRELATION CHECKING ########################
library(psych)
#Analyzing the correlation among the variables in the model
Obama_subset = Obama_Trump[names(Obama_Trump) %in% c("VotedForTrump", "PID7", "Gender", "Education","Race","unemployed","employ_status", "mediaindex","family_income","Dreamers","GrantLegalStatus","IncreaseBorderPatrols","DeportIllegal")]
pairs.panels(Obama_subset)


########################## DATA MODELING ########################

#BASE MODEL
summary(baselogit<-glm(VotedForTrump~ as.factor(PID7) + GrantLegalStatus + IncreaseBorderPatrols + Dreamers + DeportIllegal + Gender + as.factor(Race) + as.factor(Education) + unemployed + family_income + mediaindex + TerrorCamp + Religion + Economy, data=Obama_Trump,family=binomial(link="logit")))

#MODEL WITH INTERACTIONS
summary(PIDGenderlogit<-glm(VotedForTrump~ Gender*PID7_new + as.factor(Education) + GrantLegalStatus + IncreaseBorderPatrols + Dreamers + DeportIllegal + as.factor(Race) + unemployed + family_income + mediaindex + TerrorCamp + Religion + Economy, data=Obama_Trump,family=binomial(link="logit")))

#INTERACTION PLOT 
library(jtools)
interact_plot(PIDGenderlogit, pred=PID7_new, modx=Gender, interval = TRUE, x.label = "Political Identity", y.label = "Probability of Switching",main.title = "Political Identity and Gender Interaction" )

#COMPARISON OF THE BASE AND THE INTERACTION MODELS
stargazer::stargazer(baselogit, PIDGenderlogit, type = "text")