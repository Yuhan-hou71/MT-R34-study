# Cleaning script for R34 (MindTrails: Managing Anxiety) data for Sonia and Julie #

# Load packages #
library(plyr)
library(dplyr)
library(reshape2)

# Set working directory #
setwd("/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/ManagingAnxiety_final-2/Raw Data/")

#-------------------------------------------------------------------------------#
# "Participant" table                                                           #
#-------------------------------------------------------------------------------#
participant <- read.csv("ParticipantExport_recovered_Feb_02_2019.csv", header = TRUE)
part1 <- participant %>% select(participantID = id, everything()) # Create ID variables consistent with all other tables

# Find the column values
count(participant$active)
count(participant$admin)
count(participant$email_optout)
count(participant$increase30)
count(participant$over18)

# Replace column values with TRUE or FALSE
part1 <- mutate(part1, active = ifelse(active=="\001","TRUE","FALSE"))
part1 <- mutate(part1, admin = ifelse(admin=="\001","TRUE","FALSE"))
part1 <- mutate(part1, email_optout = ifelse(email_optout=="\001","TRUE","FALSE"))
part1 <- mutate(part1, increase30 = ifelse(increase30=="\001","TRUE","FALSE"))
part1 <- mutate(part1, over18 = ifelse(over18=="\001","TRUE","FALSE"))

# Remove admins, ppl < 18 years of age, scam accounts
part2 <- filter(part1, admin=="FALSE" & over18=="TRUE") #46 (admin and less 18)
part3 <- filter(part2, participantID<20 | participantID>420)

# There was no variable to identify test accounts, so I manually searched for test IDs in the database 
# and removed accounts associated with PACTlab members or if they had the word "test" in their name/email
test <- c(1,2,4,5,441,450,538,540,578,610,624,718,767,753,775,847,848,926,971,1014,1020:1026,
          1031:1033,1038,1058,1187,1213,1215,1220:1226,1232,1263,1270,1288,1309,1338,1407,
          1486:1488,1490,1499,1500,1608,1631,1740,1767,1817:1819,1831,1899,1900,1968,1971) 

# Remove test accounts
part4 <- part3[!part3$participantID%in%test,]

length(unique(part4$participantID)) # There are no duplicates

IDlist <- c(part4$participantID) #1496 participants

clean_data <- "~/Downloads/ManagingAnxiety_FINAL/ManagingAnxiety_final/Clean Data"
write.csv(part4, file.path(clean_data, "ParticipantExport_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "Anxiety Triggers" table                                                     #
#------------------------------------------------------------------------------#
trig <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/Attrition-Engagement/github-attrition/archive/Yuling-Diheng/data/r34/raw/MindTrails-feb/AnxietyTriggers_recovered_Feb_02_2019.csv", header = TRUE)

# Fix ID numbers for one participant
levels(trig$participantRSA) <- c(levels(trig$participantRSA),"532")
trig$participantRSA[trig$participantRSA=="NLBE2NPWrw/sp3mXg6WF4Sjn1FGIu2UCtbdS+hW3GyV5SsMfheBlUnvzPWocnQG7vCZiczMpxOacbCZQn7mPUOu7wtITLV3/NtWfnSNRRL2cTNUHVxhk2pkKRR8v1gg6FbbNafZB8dB8KjfBxXLyMkkFu+ZP5DomcGZ6JL7HiOZCdOH8x6qoJ6gUYmdZBIWxlveMRUKtwkOoHxGKNWoFjEfbKAY5BfbPqiTKDKth39gmjQhxmEWBkPNAp5mQEfF9j0Qk0vQfX5BbZGzJEJIJaU6CU+ngmgMUVnKoAHFobpQYtZVr6DIb1cGXfCpDJAGcmVeUqZIz/JYIfbZy/37Alw=="] <- "532"
trig$participantID <- as.numeric(as.character(trig$participantRSA))
trig1 <- trig %>% select(participantID, everything()) # Create ID variables consistent with all other tables
trig1 <- trig1[,-c(6)]

trig1 <- subset(trig1,participantID %in% IDlist) #956 observations
trig_dup <- trig1[(duplicated(trig1[, c("participantID","session")])),] #NO duplicates

# Fix "howLong" values
count(trig1$howLong)
trig1$howLong[trig1$howLong=="entre 2-5 ans"] <- "between 2-5 years"
trig1$howLong[trig1$howLong=="tra 1-3 mesi"] <- "between 1-3 months"

write.csv(trig1, file.path(clean_data, "AnxietyTriggers_02_02_2019.csv"), row.names = FALSE) #DONE

#------------------------------------------------------------------------------#
# "BBSIQ" table                                                                #
#------------------------------------------------------------------------------#

BBSIQ <- read.csv("BBSIQ_recovered_Feb_02_2019.csv", header = TRUE)
BBSIQ[68, "participantRSA"] <- 532 #Insert correct participantRSA

# All 15 duplicates are test/scam accounts
BB_dup <- BBSIQ[(duplicated(BBSIQ[, c("participantRSA","session")])),]

BBSIQ$participantID <- as.numeric(as.character(BBSIQ$participantRSA))
BBSIQ <- BBSIQ %>% select(participantID, everything()) # Create ID variables consistent with all other tables
BBSIQ <- BBSIQ[,-c(28)]

BBSIQ1 <- subset(BBSIQ,participantID %in% IDlist) #1434 observations
BB_dup <- BBSIQ1[(duplicated(BBSIQ1[, c("participantID","session")])),] # No duplicates

write.csv(BBSIQ1, file.path(clean_data, "BBSIQ_02_02_2019_new.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "CC" table                                                                   #
#------------------------------------------------------------------------------#

CC <- read.csv("CC_recovered_Feb_02_2019.csv", header = TRUE)
levels(CC$participantRSA) <- c(levels(CC$participantRSA),"531","532")
CC$participantRSA[CC$participantRSA=="aidR0ILOiwxDni8IQTeE2eUej2bpUBh5/TF+OkLMF9oKg8S1vIQTAdS8oKVHREZ9n7FMpjPvkvkzQ6aC/8tFucKEVqBenPQj1/GDlDroyb9Qc5+uIDFBwKbXmwkbwNjoQh99eeck0WIlnjRRdkvHuUEcNe/U/6es2hBZIE1ZAHxOYGHyX2p5niZu5EihFecRf4IqIDO4MeqdSnAE2axoxaWUU9b06x1wDj1Khy/ov1HDv+fv9jeXgh0gWuzn4cAyExMlqFtW6QYJ/D2FRKmj+faz6W5/Oq3UgNR5PnE9iq4qFzfROf31DpDYvrKSnDP5pTYmLF0d5T9oYiFeXZsGtw=="] <- "531"
CC$participantRSA[CC$participantRSA=="ctl7s5tAKQE8FjniSzQQ9HgXGnF1VGiw61MgKO1nKJ7B78qKwHcORu9vVb+TRa95eI9FAKWZV7jOFlx+AbeKUuprJliWlI1kLug53hA/5pRM+Y3A4HWaUE6uQJAhuD+u+bqVmQbgNwsVzp3nRN575LCIyaDtT6yWljsgql9s014uwHu6mIZvH1jdDg8r4d4QG4pfPnw4choo76TuiobnmDX5yNJ7a1N6/Ae2ljMUaDWbAq8ljTRCpV8noUt/1HAnS9owWL/vKNGR9sMGZIhN49ijujtq+x2lga8nmwykD3rvHmeKYHOBCNb/xf9b8LahyndqqNmUr8HlHfuP1yZ6QA=="] <- "532"

CC$participantID <- as.numeric(as.character(CC$participantRSA))
CC <- CC %>% select(participantID, everything()) # Create ID variables consistent with all other tables
CC <- CC[,-c(5)]
CC1 <- subset(CC,participantID %in% IDlist) # 1819 observations

ccdup <- CC1[(duplicated(CC1[, c("participantID","session")])),] #NO duplicates

write.csv(CC, file.path(clean_data, "CC_02_02_2019_new.csv"), row.names = FALSE)

#------------------------------------------------------------------------------#
# "CIHS" table                                                                 #
#------------------------------------------------------------------------------#

CIHS <- read.csv("CIHS_recovered_Feb_02_2019.csv", header = TRUE) 

# There is one case of an odd participantRSA value, but there is no obvious match in
# the Task Log, so the observation is excluded
CIHS$participantID <- as.numeric(as.character(CIHS$participantRSA))
CIHS <- CIHS %>% select(participantID, everything()) # Create ID variables consistent with all other tables
CIHS <- CIHS[,-c(6)]
CIHS1 <- subset(CIHS,participantID %in% IDlist)

cihsdup <- CIHS1[(duplicated(CIHS1[, c("participantID","session")])),]

# Fix session labels
s8_list <- c(425,426,773)
post_list <- c(793)
CIHS1$session[CIHS1$id %in% s8_list] <- "SESSION8"  
CIHS1$session[CIHS1$id %in% post_list] <- "POST"  

write.csv(CIHS1, file = "CIHS_02_02_2019.csv", row.names = FALSE) #DONE

#------------------------------------------------------------------------------#
# "DASS AS" table                                                              #
#------------------------------------------------------------------------------#
dassa <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/ManagingAnxiety_final-2/Raw Data/DASS21_AS_recovered_Feb_02_2019.csv", header = TRUE)
dassa$participantID <- as.numeric(as.character(dassa$participantDAO))

dassa <- dassa %>% select(participantID, everything()) # Create ID variables consistent with all other tables
dassa <- dassa[,-c(8,9)]

dassa1 <- subset(dassa,participantID %in% IDlist) # 1637 observations
dass_dup <- dassa1[(duplicated(dassa1[, c("participantID","session")])),] # 100 duplicates
dass_dup <- dass_dup$participantID
test <- filter(dassa1, participantID %in% dass_dup) # All duplicates are multiple eligibility questionnaires

DASS_pre <- filter(dassa1, session == "ELIGIBLE")
DASS_pre <-DASS_pre[!rev(duplicated(rev(DASS_pre$participantID))),]
DASS_other <- filter(dassa1, session != "ELIGIBLE")
dassa2 <- rbind(DASS_pre, DASS_other)

is.na(dassa2) <- dassa2 == -1; is.na(dassa2) <- dassa2 == -2

write.csv(dassa2, file.path(clean_data, "DASS21_AS_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "DASS DS" table                                                              #
#------------------------------------------------------------------------------#
dassd <- read.csv("DASS21_DS_recovered_Feb_02_2019.csv", header = TRUE)
dassd[68, "participantRSA"] <- 532 #Insert correct participantRSA

dassd$participantID <- as.numeric(as.character(dassd$participantRSA))
dassd <- dassd[,-c(10)]
dassd <- dassd %>% select(participantID,id,date,everything())
is.na(dassd[,4:10]) <- dassd[,4:10] == -1
dassd1 <- subset(dassd,participantID %in% IDlist)
dassddup <- dassd1[(duplicated(dassd1[, c("participantID","session")])),] #NO duplicates

write.csv(dassd1, file.path(clean_data, "DASS21_DS_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "DD" table                                                                   #
#------------------------------------------------------------------------------#
dd <- read.csv("DD_recovered_Feb_02_2019.csv", header = TRUE)
dd[50, "participantRSA"] <- 532 #Insert correct participantRSA

dd$participantID <- as.numeric(as.character(dd$participantRSA))
dd <- dd[,-c(9)]
dd <- dd %>% select(participantID,id,date,everything())
dd1 <- subset(dd,participantID %in% IDlist)
dd_dup <- dd1[(duplicated(dd1[, c("participantID","session")])),] #NO duplicates

write.csv(dassd1, file.path(clean_data, "DD_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "DD FU" table                                                              #
#------------------------------------------------------------------------------#
dd_fu <- read.csv("DD_FU_recovered_Feb_02_2019.csv", header = TRUE)

dd_fu$participantID <- as.numeric(as.character(dd_fu$participantRSA))
dd_fu <- dd_fu[,-c(7)]
dd_fu <- dd_fu %>% select(participantID,id,date,everything())
dd_fu1 <- subset(dd_fu,participantID %in% IDlist)
dd_fu_dup <- dd_fu1[(duplicated(dd_fu1[, c("participantID","session")])),] #NO duplicates

write.csv(dassd1, file.path(clean_data, "DD_FU_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "Demographics" table                                                         #
#------------------------------------------------------------------------------#
dem <- read.csv("Demographic_recovered_Feb_02_2019.csv", header = TRUE)

# Fix ID numbers for three participants
levels(dem$participantRSA) <- c(levels(dem$participantRSA),"534","535","536")
dem$participantRSA[dem$participantRSA=="NvorEV/x1Rn9FC1yuqkfDjcDlSRj9b8Y3ji78LPNHI8vzrvOJdQjRTpAY+Mn/C8pToWmJmY4luS8P3Rfcfk/hcZGxUwzWpbkJjuKA0q8E4Y6IPxJmLdiO2zFYh8BJKEr4j8pidKyMooi2REL5hcNeJKPpkId4R0iBpLgPXfapA+XnRqHQftVZo+JL1TD8eP362JX7A0L90OxpqkuOp3lnEce2ox6vcc4fwTsvoWy+utocR+Fp7Pgct/wBIdjvKhSLJvabMK7Ffglk9OrAIXuxj77+n+sbsW4PoGc9a2UZu5D18WKGrUW6kdLTBCDqE/DyWjcLXEGWCwCwlTdRtPtjQ=="] <- "534"
dem$participantRSA[dem$participantRSA=="j5J6cs0Yromqio+zSNruW1qA1athdsLV4AQf6AnWnhVS2M+SYJ3bEgBG9GPQDZMLN4Ttqyk76HO+x1QXEdL/FtgkyyYy82aR4j5lYJ0IeamTIK5atLtT0MZJQqhhehLp5y97TvJjootnTj4OV+l0iaxAMKxzcIqadlZ7FzD5uWDTqJCzxX/Bny7V01irJfqHgEEpxNwwLmJttjgfr3ifWWv7JseWIgPPIH1spGFUWN127vCvKmZ3bmbYCLq119q5Nn11jKe+Oq2oSvv7HPczDb/knU3Uu7Y8Js01nK+VtX6kFYAACzwZcu6GN+KGuUUexlxMJXCaLBQvEdU3Iy7lVQ=="] <- "535"
dem$participantRSA[dem$participantRSA=="VS9LPKcqvwakVj2Orq/tFLN+JB9nmbaZ5kqg0gDXF7CQ4NKpey6kMSnEErlvr3UrnJKYgKGEAk3wi0zijUZDNS5+Nzys2L7ynDGXQoE61RO07m2dqMsxZGHgv8qadNtfIyKxpjYaNoblp0d3VJ8sSce08XBmVxMfr7nLbDwC60Zm1vsBajDqX073gc3U35cq+n4vNG5v1t1zPQFqeRvXsTHGc4naet1I8PeQEcLtQJN2daxdfgZplToaOzEW7KGhdzgYYRyTwWCvwh8wHUcdmV4dW7VG3oGLvz35wLYVkOlpZMgZ+5eAr9an4JDaXwR1e3CeHYJHRDxSGibqK3rl/Q=="] <- "536"

dem$participantID <- as.numeric(as.character(dem$participantRSA))
dem <- dem %>% select(participantID, everything()) # Create ID variables consistent with all other tables
dem <- dem[,-c(11)]

# All duplicates are test/scam accounts and will be removed
dem_dup <- dem[(duplicated(dem[, c("participantID")])),] 

# Fix birth years
dem$birthYear[dem$birthYear==19019590] <- 1959
dem$birthYear[dem$birthYear==19011968] <- 1968
dem$birthYear[dem$birthYear==19001997] <- 1997
dem$birthYear[dem$birthYear==19001989] <- 1989
dem$birthYear[dem$birthYear==19001987] <- 1987
dem$birthYear[dem$birthYear==19001986] <- 1986
dem$birthYear[dem$birthYear==19001981] <- 1981
dem$birthYear[dem$birthYear==19001977] <- 1977
dem$birthYear[dem$birthYear==19001976] <- 1976
dem$birthYear[dem$birthYear==19001975] <- 1975
dem$birthYear[dem$birthYear==19001959] <- 1959
dem$birthYear[dem$birthYear==1972900] <- 1972
dem$birthYear[dem$birthYear==1900955] <- 1955
dem$birthYear[dem$birthYear==195100] <- 1951
dem$birthYear[dem$birthYear==2964] <- 1964
dem$birthYear[dem$birthYear==2222] <- NA
dem$birthYear[dem$birthYear==0] <- NA
dem$birthYear[dem$birthYear==97] <- 1997
dem$birthYear[dem$birthYear==92] <- 1992
dem$birthYear[dem$birthYear==90] <- 1990
dem$birthYear[dem$birthYear==89] <- 1989
dem$birthYear[dem$birthYear==87] <- 1987
dem$birthYear[dem$birthYear==86] <- 1986
dem$birthYear[dem$birthYear==84] <- 1984
dem$birthYear[dem$birthYear==82] <- 1982
dem$birthYear[dem$birthYear==81] <- 1981
dem$birthYear[dem$birthYear==80] <- 1980
dem$birthYear[dem$birthYear==77] <- 1977
dem$birthYear[dem$birthYear==72] <- 1972
dem$birthYear[dem$birthYear==70] <- 1970
dem$birthYear[dem$birthYear==64] <- 1964
dem$birthYear[dem$birthYear==61] <- 1961
dem$birthYear[dem$birthYear==58] <- 1958

# Remove everyone except the real participants
dem1 <- subset(dem, participantID %in% IDlist) #1308 Participants
dem_dup <- dem1[(duplicated(dem1[, c("participantID")])),] 

# Fix/translate values
count(dem1$education)
dem1[dem1[,]==""] <- NA
dem1$education[dem1$education=="????"] <- NA
dem1$education[dem1$education=="Diploma di scuola superiore"] <- "High School Graduate"
dem1$education[dem1$education=="Un lycée"] <- "Some High School"

count(dem1$employmentStatus)
dem1$employmentStatus[dem1$employmentStatus=="????"] <- NA
dem1$employmentStatus[dem1$employmentStatus=="Étudiant"] <- "Student"
dem1$employmentStatus[dem1$employmentStatus=="Lavoro part-time"] <- "Working part-time"

count(dem1$ethnicity)
dem1$ethnicity[dem1$ethnicity=="??????????"] <- NA
dem1$ethnicity[dem1$ethnicity=="Inconnu"] <- "Unknown"
dem1$ethnicity[dem1$ethnicity=="Sconosciuto"] <- "Unknown"

count(dem1$gender)
dem1$gender[dem1$gender=="?"] <- NA
dem1$gender[dem1$gender=="Femmina"] <- "Female"
dem1$gender[dem1$gender=="Mâle"] <- "Male"

count(dem1$income)
dem1$income[dem1$income=="$ 5,000 a $ 11999"] <- "$5,000 through $11,999"
dem1$income[dem1$income=="$ 50,000??$ 74,999"] <- "$50,000 through $74,999"
dem1$income[dem1$income=="Moins de 5 000 $"] <- "Less than $5,000"

count(dem1$maritalStatus)
dem1$maritalStatus[dem1$maritalStatus=="??"] <- NA
dem1$maritalStatus[dem1$maritalStatus=="Single, ma attualmente fidanzato"] <- "Single, but currently engaged to be married"
dem1$maritalStatus[dem1$maritalStatus=="Unique"] <- "Single"

count(dem1$participateReason)
dem1$participateReason[dem1$participateReason=="??????????????"] <- NA
dem1$participateReason[dem1$participateReason=="Basta navigare sul web"] <- "Just surfing the web"
dem1$participateReason[dem1$participateReason=="Lien depuis un blog, un chat ou un fil de discussion"] <- "Link from blog, chat, or discussion thread"

count(dem1$race)
dem1$race[dem1$race=="?/????"] <- NA
dem1$race[dem1$race=="Bianco / origine europea"] <- "White/European origin"
dem1$race[dem1$race=="Blanc / origine européenne"] <- "White/European origin"

write.csv(dem1, file.path(clean_data, "Demographics_02_02_2019_new.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "EMAIL LOG" table                                                            #
#------------------------------------------------------------------------------#
email <- read.csv("EmailLogDAO_recovered_Feb_02_2019.csv", header = TRUE)

email1 <- email %>% select(participantID = participantId, everything()) # Create ID variables consistent with all other tables
email1 <- subset(email1,participantID %in% IDlist)

write.csv(email1, file.path(clean_data, "EmailLog_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "IMAGERY PRIME" table                                                        #
#------------------------------------------------------------------------------#
imag <- read.csv("ImageryPrime_recovered_Feb_02_2019.csv", header = TRUE)
imag[97, "participantRSA"] <- 532 #Insert correct participantRSA

imag$participantID <- as.numeric(as.character(imag$participantRSA))
imag <- imag[,-c(3)]
imag <- imag %>% select(participantID,id,date,everything())
imag1 <- subset(imag,participantID %in% IDlist) #2038 observations
imag_dup <- imag1[(duplicated(imag1[, c("participantID","session")])),] # 5 duplicates

imag2 <- imag1[!duplicated(imag1[, c("participantID","session")]), ]
imag_dup <- imag2[(duplicated(imag2[, c("participantID","session")])),] # NO duplicates

write.csv(imag2, file.path(clean_data, "ImageryPrime_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "IMPACT ANXIOUS IMAGERY" table                                               #
#------------------------------------------------------------------------------#
imp <- read.csv("ImpactAnxiousImagery_recovered_Feb_02_2019.csv", header = TRUE)
imp[97, "participantRSA"] <- 531 #Insert correct participantRSA
imp[104, "participantRSA"] <- 532 #Insert correct participantRSA

imp$participantID <- as.numeric(as.character(imp$participantRSA))
imp <- imp[,-c(6)]
imp <- imp %>% select(participantID,id,date,everything())
imp1 <- subset(imp,participantID %in% IDlist) #2038 observations
imp_dup <- imp1[(duplicated(imp1[, c("participantID","session")])),] # 5 duplicates

imp2 <- imp1[!duplicated(imp1[, c("participantID","session")]), ]
imp_dup <- imp2[(duplicated(imp2[, c("participantID","session")])),] # NO duplicates

write.csv(imp2, file.path(clean_data, "DD_FU_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "MENTAL HEALTH HX TX" table                                                  #
#------------------------------------------------------------------------------#
mh <- read.csv("MentalHealthHxTx_recovered_Feb_02_2019.csv", header = TRUE)

# Shift columns
nc  <- ncol(mh)

d1 <- filter(mh, id>=1 & id<=571)
d1[, 30:nc] <- d1[, 28:40]
d1[, c(28,29)] <- NA
d2 <- filter(mh, id>571)

mh1 <- rbind(d1,d2)

levels(mh1$participantRSA) <- c(levels(mh1$participantRSA),"534","535","536")
mh1[5, "participantRSA"] <- 534 #Insert correct participantRSA
mh1[6, "participantRSA"] <- 535 #Insert correct participantRSA
mh1[7, "participantRSA"] <- 536 #Insert correct participantRSA

mh1$participantID <- as.numeric(as.character(mh1$participantRSA))
mh1 <- mh1[,-c(31)]
mh1 <- mh1 %>% select(participantID,id,date,everything())
mh2 <- subset(mh1,participantID %in% IDlist)
mh_dup <- mh2[(duplicated(imp1[, c("participantID","session")])),] # NO duplicates

write.csv(mh2, file.path(clean_data, "MentalHealthHxTx_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "MULTI USER EXPERIENCE" table                                                #
#------------------------------------------------------------------------------#
multi <- read.csv("MultiUserExperience_recovered_Feb_02_2019.csv", header = TRUE)
multi$participantID <- as.numeric(as.character(multi$participantRSA))
multi <- multi[,-c(16)]
multi <- multi %>% select(participantID,id,date,everything())
multi1 <- subset(multi,participantID %in% IDlist) #2038 observations
multi_dup <- multi1[(duplicated(multi1[, c("participantID","session")])),] # 5 duplicates

write.csv(imp2, file.path(clean_data, "DD_FU_02_02_2019.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "OASIS" table                                                                #
#------------------------------------------------------------------------------#
OA <- read.csv("OA_recovered_Feb_02_2019.csv", header = TRUE)
OA <- OA %>% select(participantID = participantDAO, everything()) # Create ID variables consistent with all other tables
OA <- OA[,-c(9)]

# All duplicates are test/scam accounts
oa_dup <- OA[(duplicated(OA[, c("participantID","session")])),]

OA1 <- subset(OA,participantID %in% IDlist)
oa_dup <- OA1[(duplicated(OA1[, c("participantID","session")])),] #no duplicates

write.csv(OA1, file.path(clean_data, "OA_02_02_2019_new.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "QOL" table                                                                #
#------------------------------------------------------------------------------#
QOL <- read.csv("QOL_recovered_Feb_02_2019.csv", header = TRUE)
levels(QOL$participantRSA) <- c(levels(QOL$participantRSA),"534","535","536")

QOL[81, "participantRSA"] <- 534 #Insert correct participantRSA
QOL[89, "participantRSA"] <- 536 #Insert correct participantRSA
QOL[88, "participantRSA"] <- 535 #Insert correct participantRSA

QOL$participantID <- as.numeric(as.character(QOL$participantRSA))
QOL <- QOL %>% select(participantID, everything()) # Create ID variables consistent with all other tables
QOL <- QOL[,-c(12)]

# All duplicates are test/scam accounts
QOL_dup <- QOL[(duplicated(QOL[, c("participantID","session")])),]

QOL1 <- subset(QOL,participantID %in% IDlist)
QOL_dup <- QOL1[(duplicated(QOL1[, c("participantID","session")])),] #no duplicates

write.csv(QOL1, file.path(clean_data, "QOL_02_02_2019_new.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "Recognition Ratings" table                                                  #
#------------------------------------------------------------------------------#

RR <- read.csv("RR_recovered_Feb_02_2019.csv", header = TRUE)
RR[68, "participantRSA"] <- 532 # Insert correct participantRSA
RR$participantID <- as.numeric(as.character(RR$participantRSA))
RR <- RR %>% select(participantID, everything()) # Create ID variables consistent with all other tables
RR <- RR[,-c(28)]

# All duplicates are test/scam accounts
RR_dup <- RR[(duplicated(RR[, c("participantID","session")])),]

is.na(RR) <- RR == -1
RR1 <- subset(RR,participantID %in% IDlist) # 1399 observations

RR_dup <- RR1[(duplicated(RR1[, c("participantID","session")])),] # No duplicates

write.csv(RR1, file.path(clean_data, "RR_02_02_2019_new.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "Return Intention" table                                                     #
#------------------------------------------------------------------------------#

return <- read.csv("ReturnIntention_recovered_Feb_02_2019.csv", header = TRUE)
return <- return %>% select(participantID=participantRSA, everything()) # Create ID variables consistent with all other tables

return_dup <- return[(duplicated(return[, c("participantID","session")])),] # NO duplicates

return1 <- subset(return,participantID %in% IDlist) # 1399 observations

write.csv(RR1, file.path(clean_data, "RR_02_02_2019_new.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "SUDS" table                                                                 #
#------------------------------------------------------------------------------#

SUDS <- read.csv("SUDS_recovered_Feb_02_2019.csv", header = TRUE)
SUDS[120, "participantRSA"] <- 532 # Insert correct participantRSA
SUDS[121, "participantRSA"] <- 532 # Insert correct participantRSA
SUDS[131, "participantRSA"] <- 531 # Insert correct participantRSA

SUDS$participantID <- as.numeric(as.character(SUDS$participantRSA))
SUDS <- SUDS %>% select(participantID, everything()) # Create ID variables consistent with all other tables
SUDS <- SUDS[,-c(5)]

SUDS1 <- subset(SUDS,participantID %in% IDlist) # 2504 observations

write.csv(SUDS1, file.path(clean_data, "SUDS_02_02_2019_new.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "TRIAL" table                                                                 #
#------------------------------------------------------------------------------#

trial <- read.csv("TrialDAO_recovered_Feb_02_2019.csv", header = TRUE)
trial <- trial %>% select(participantID = participant, everything()) # Create ID variables consistent with all other tables

# Most duplicates are test/scam accounts
trial_dup <- trial[(duplicated(trial[, c("participantID","session","sessionName","trial")])),]

trial1 <- subset(trial,participantID %in% IDlist)
trial_dup <- trial1[(duplicated(trial1[, c("participantID","session","sessionName","trial")])),]
trial2 <- trial1[!duplicated(trial1[, c("participantID","session","sessionName","trial")]), ]

write.csv(trial2, file.path(clean_data, "Trial_02_02_2019_new.csv"), row.names=FALSE)

#------------------------------------------------------------------------------#
# "Task Log" table                                                             #
#------------------------------------------------------------------------------#

tasklog <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/ManagingAnxiety_final/Clean Data/TaskLog_02_02_2019.csv", header = TRUE) #replace with raw table
tl_dup <- tasklog[(duplicated(tasklog[, c("participantdao_id","session_name","task_name")])),]
tasklog$participantID <- tasklog$participantdao_id
tasklog1 <- subset(tasklog, participantID %in% IDlist)

write.csv(tasklog1, file.path(clean_data, "TaskLog_02_02_2019.csv"), row.names=FALSE)
