
## set variable names and create lists of different groups of variables
id <- 'kod_id'
time <- 'week'

intro_insatser <- c('intro_bredvidgång','intro_utbildningsdag','intro_mentor')

proximal_outcomes <- c('nsfs_c_mean','nsfs_r_mean','roleclarity_mean')

distal_outcomes <- c('ex_jobsat_mean','ex_org_com_mean','nse_mean',
                     'ex_itl_mitlw_mean', 'ex_itl_mitlp_mean','olbi_mean',
                     'fitperc_mean','emot_anx_mean', 'emot_dep_mean',
                     'seq_stress_mean','seq_energy_mean')


##folder for datafiles
datafolder <- "C:/Users/jonaur/Desktop/DATA/"

###########data########
##load data
rdata2015 <- read.csv(sprintf("%s2015_data_mean_rev.csv",datafolder))
rdata2016 <- read.csv(sprintf("%s2016_data_mean_rev.csv",datafolder))
##sort data
rdata2015 <- rdata2015[with(rdata2015, order(kod_id, week)), ]
rdata2016 <- rdata2016[with(rdata2016, order(kod_id, week)), ]
##combine intro_mentor and intro_mentor2
rdata2015['intro_mentor'][which(rdata2015['intro_mentor2'] == 2),] <- 2
##Add variable to identify cohort
rdata2015['from'] <- 2015
rdata2016['from'] <- 2016
##prepare datafiles to combine them
c2015 <- colnames(rdata2015)
c2016 <- colnames(rdata2016)
rdata2016[c2015[!c2015 %in% c2016]] <- NA #Add variables that exists in 2015 and not 2016
rdata2015[c2016[!c2016 %in% c2015]] <- NA #Add variables that exists in 2016 and not 2015

##Deprecated strategy to remove variables instead of adding
#rdata2015 <- rdata2015[c2015[c2015 %in% c2016]] #removes variables not in 2016
#rdata2016 <- rdata2016[c2016[c2016 %in% c2015]] #removes variables not in 2015

##combine data
cdata <- rbind(rdata2015,rdata2016)
##Missing recode
myvar_list <- names(cdata) %in% c(id) # exclude id_variable from missing recoding
cdata[!myvar_list][which(cdata[id] == 999),] <- NA # 999 becomes NA for all variables except ID

##recode introdutionsinsatser to binary variables Nej = 0, Ja = 1
cdata['intro_bredvidgång'][which(cdata['intro_bredvidgång'] == 0),] <- NA # Vet ej till missing
cdata['intro_bredvidgång'][which(cdata['intro_bredvidgång'] == 2),] <- 0  # Nej till 0

cdata['intro_utbildningsdag'][which(cdata['intro_utbildningsdag'] == 0),] <- NA # Vet ej till missing
cdata['intro_utbildningsdag'][which(cdata['intro_utbildningsdag'] == 2),] <- 0  # Nej till 0

cdata['intro_mentor'][which(cdata['intro_mentor'] == 0),] <- NA # Vet ej till missing
cdata['intro_mentor'][which(cdata['intro_mentor'] == 2),] <- 0  # Nej till 0
cdata['intro_mentor'][which(cdata['intro_mentor'] == 3),] <- NA # Vet ej till missing
cdata['intro_mentor'][which(cdata['intro_mentor'] == 4),] <- 0  # Har ej mentor till 0
###########data########
#rdata2015[which(is.na(rdata2015)),]

#cdata[!myvar_list][which(cdata[id] == 999),]# <- NA # 999 becomes NA for all variables except ID



