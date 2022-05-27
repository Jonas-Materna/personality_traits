
library(haven)
library(dplyr)
library(stats)

## Load needed data
biobirth <- read_dta(paste0("data/soep/biobirth.dta"), col_select=c("pid",
                                                                    "gebjahr"))

pgen <- read_dta(paste0("data/soep/pgen.dta"), col_select=c("pid",
                                                            "syear",
                                                            
                                                            #Job indicators
                                                            "pgisco88",
                                                            "pgisco08",
                                                            "pgemplst",
                                                            
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
                                                         "plh0190"))







# Merge data
df         <- merge(pgen, pl, by = c("pid", "syear"), all = F)
df         <- merge(df, biobirth, by = c("pid"), all = F)

# Do some basic cleaning
df[df<0]   <- NA
df <- data.frame(lapply(df, function(x) as.numeric(x)))
rm(pgen, pl, biobirth)

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


df$consc <- (df$plh0212 + (8-df$plh0218) + df$plh0222 + df$plh0255) / 4
df$extra <- (df$plh0213 + df$plh0219 + (8-df$plh0223)) / 3
df$agree <- ((8-df$plh0214) + df$plh0217 + df$plh0224) / 3
df$openn <- (df$plh0215 + df$plh0220 + df$plh0225) / 3
df$neuro <- (df$plh0216 + df$plh0221 + (8-df$plh0226)) / 3


# Get gender
df$female                    <- df$pla0009_v2 == 2  | df$pla0009_v3 == 2
df$female[is.na(df$female)]  <- FALSE


# Get age
df$age                       <- df$syear - df$gebjahr 

 
# Get higher education
df$higher_edu                       <- df$pgpbbil02 %in% c(1:10)
df$higher_edu[is.na(df$higher_edu)] <- FALSE

soep_data <- df
saveRDS(soep_data, "data/generated/soep_data.rds")





