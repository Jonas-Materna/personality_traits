
library(haven)
library(dplyr)
library(stats)

## Load needed data
biobirth <- read_dta(paste0("data/soep/biobirth.dta"), col_select=c("pid",
                                                                    "gebjahr",
                                                                    "sex"))

biojob    <- read_dta(paste0("data/soep/biojob.dta"), col_select=c("pid",
                                                                   "einstieg_artk"))

pgen <- read_dta(paste0("data/soep/pgen.dta"), col_select=c("pid",
                                                            "syear",
                                                            
                                                            #Marriage
                                                            "pgfamstd",
                                                            
                                                            #Nationality
                                                            "pgnation",
                                                            
                                                            #Unemployment experience
                                                            "pgexpue",
                                                            
                                                            #Job indicators
                                                            "pgisco88",
                                                            "pgisco08",
                                                            "pgemplst",
                                                            "pglfs",
                                                            
                                                            #Information on education
                                                            "pgpsbil",
                                                            "pgpbbil01",
                                                            "pgpbbil02",
                                                            "pgdegree",
                                                            
                                                            #Information on job characteristics
                                                            "pgtatzeit",
                                                            "pgvebzeit",
                                                            "pguebstd",
                                                            "pgexpft",
                                                            "pgstib",
                                                            "pglabgro",
                                                            "pgerwzeit"))


pl  <- read_dta(paste0("data/soep/pl.dta"), col_select=c("pid",
                                                         "syear",
                                                         
                                                         #Gender variables
                                                         "pla0009_v2",
                                                         "pla0009_v3",
                                                         
                                                         #Reason of job loss
                                                         "plb0304_v14",
                                                         
                                                         #Self employed
                                                         "plb0057_v3",
                                                         
                                                         #Civil servants
                                                         "plb0065",
                                                         
                                                         #Political opinion,
                                                         "plh0004",
                                                         
                                                         #Positionin job
                                                         "plb0064_v1",
                                                         "plb0064_v2",
                                                         "plb0064_v4",
                                                         
                                                         
                                                         #Measures of worries
                                                         "plh0032",
                                                         "plh0033",
                                                         "plh0335",
                                                         "plh0335",
                                                         "plh0035",
                                                         "plh0036",
                                                         "plh0037",
                                                         "plh0038",
                                                         "plh0040",
                                                         "plh0336", 
                                                         "plh0034",
                                                         "plj0046",
                                                         "plj0047",
                                                         "plh0042",
                                                         
                                                         #Big 5 questions
                                                         "plh0212", 
                                                         "plh0213",
                                                         "plh0214",
                                                         "plh0215",
                                                         "plh0216",
                                                         "plh0217",
                                                         "plh0218",
                                                         "plh0219",
                                                         "plh0220",
                                                         "plh0221",
                                                         "plh0222",
                                                         "plh0223",
                                                         "plh0224",
                                                         "plh0225",
                                                         "plh0226",
                                                         "plh0255",
                                                         
                                                         #Risk tolerance
                                                         "plh0204_h",
                                                         
                                                         #Information on satisfaction
                                                         "plb0112",
                                                         "plh0032",
                                                         "plh0033",
                                                         "plh0032",
                                                         "plh0171",
                                                         "plh0172",
                                                         "plh0173",
                                                         "plh0174",
                                                         "plh0175",
                                                         "plh0176",
                                                         "plh0177",
                                                         "plh0178",
                                                         "plh0179",
                                                         "plh0180",
                                                         "plh0181",
                                                         "plh0182",
                                                         "plh0184",
                                                         "plh0185",
                                                         "plh0186",
                                                         "plh0187",
                                                         "plh0190",
                                                         
                                                         #Self-employed
                                                         "plb0057_h1",
                                                         "plb0057_h2",
                                                         "plb0049_v6",
                                                         "plb0047",
                                                         "plb0046"))



ppathl  <- read_dta(paste0("data/soep/ppathl.dta"), col_select=c("pid",
                                                                 "syear",
                                                                 "phrf"))


gripstr <- read_dta(paste0("data/soep/gripstr.dta"))


# Merge data
df         <- merge(pgen, pl, by = c("pid", "syear"), all.x = T)
df         <- merge(df, biojob, by = c("pid"), all.x = T)
df         <- merge(df, biobirth, by = c("pid"), all.x = T)
df         <- merge(df, ppathl, by = c("pid", "syear"), all.x = T)
df         <- merge(df, gripstr, by = c("pid", "syear"), all.x = T)


# Do some basic cleaning
df[df<0]   <- NA
df <- data.frame(lapply(df, function(x) as.numeric(x)))
rm(ppathl,pl,pgen,biobirth, biojob, gripstr)

## Calculate Big 5 personality traits

##Conscientiousness
#Thorough worker:                       plh0212
#Tends to be lazy:                      plh0218
#Does things efficiently:               plh0222
#Inquisitive:                           plh0255

##Extraversion
#Am communicative:                      plh0213
#Am sociable:                           plh0219
#Is reserved:                           plh0223

##Agreeableness
#Am sometimes too coarse with others:   plh0214
#Able to forgive:                       plh0217
#Friendly with others:                  plh0224

##Openness 
#Am original:                           plh0215 
#Value artistic experiences:            plh0220
#Have lively imagination:               plh0225

##Neuroticism
#Worry a lot:                           plh0216
#Gets nervous easily:                   plh0221
#Handles stress well:                   plh0226



#Change signs questions with negative effect
df$plh0218 <- (8-df$plh0218)
df$plh0223 <- (8-df$plh0223)
df$plh0214 <- (8-df$plh0214)
df$plh0226 <- (8-df$plh0226)

#Calculate big 5
df$consc <- apply(data.frame(df$plh0212, df$plh0218, df$plh0222, df$plh0255),1,mean,na.rm=TRUE)
df$extra <- apply(data.frame(df$plh0213, df$plh0219, df$plh0223),1,mean,na.rm=TRUE)
df$agree <- apply(data.frame(df$plh0214, df$plh0217, df$plh0224),1,mean,na.rm=TRUE)
df$openn <- apply(data.frame(df$plh0215, df$plh0220, df$plh0225),1,mean,na.rm=TRUE)
df$neuro <- apply(data.frame(df$plh0216, df$plh0221, df$plh0226),1,mean,na.rm=TRUE)


# Get gender
df$female <- df$sex == 2

# Marriage status 
df$married <- df$pgfamstd == 1

# Get age
df$age                       <- df$syear - df$gebjahr 
 
# Get higher education
df$higher_edu                       <- df$pgpbbil02 %in% c(1:10)
df$higher_edu[is.na(df$higher_edu)] <- FALSE


#Generate log wage
df$pglabgro[df$pglabgro==0] <- NA
df$log_wage       <- log(df$pglabgro)

soep_data <- df
saveRDS(soep_data, "data/generated/soep_data.rds")





