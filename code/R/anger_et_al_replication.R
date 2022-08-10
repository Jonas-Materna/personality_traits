library(ExPanDaR)
library(ggplot2)
library(tidyr)
library(ggpubr)
# Load data
soep_data <- readRDS("data/generated/soep_data.rds")

##########
# Sample selection
##########

#First sample period (2005-2009)
df_2005_2009 <- soep_data[soep_data$syear %in% c(2005:2009),]

#Keep only those with regular employment in 2005
reg_emp      <- unique(df_2005_2009$pid[df_2005_2009$syear %in% 2005 &
                            df_2005_2009$pgemplst %in% 1])
df_2005_2009 <- df_2005_2009[df_2005_2009$pid %in% reg_emp,]

#Keep only those without unemployment experience
no_unemp_exp  <- unique(df_2005_2009$pid[df_2005_2009$syear %in% 2005 &
                                 df_2005_2009$pgexpue == 0])
df_2005_2009  <- df_2005_2009[df_2005_2009$pid %in% no_unemp_exp,]

#Drop participants over 60 or under 18
between_18_60 <- unique(df_2005_2009$pid[!df_2005_2009$age < 18 &
                                    !df_2005_2009$age > 60])
df_2005_2009  <- df_2005_2009[df_2005_2009$pid %in% between_18_60,]


#Drop civil servants
no_civil_servant <- unique(df_2005_2009$pid[is.na(df_2005_2009$plb0065)])
df_2005_2009     <- df_2005_2009[df_2005_2009$pid %in% no_civil_servant,]

#Drop self employed
no_self_employed <- unique(df_2005_2009$pid[is.na(df_2005_2009$plb0057_v3)])
df_2005_2009     <- df_2005_2009[df_2005_2009$pid %in% no_self_employed,]



##########
# Get treatment and control group
##########
plant_closure      <- unique(df_2005_2009$pid[df_2005_2009$plb0304_v14 %in% 1])
treat              <- df_2005_2009[df_2005_2009$pid %in% plant_closure,] 
control            <- df_2005_2009[!df_2005_2009$pid %in% plant_closure,]


##########
# Get summary statistics
##########
mean(control$openn[control$syear %in% 2005], na.rm = T)
mean(control$consc[control$syear %in% 2005], na.rm = T)
mean(control$extra[control$syear %in% 2005], na.rm = T)
mean(control$agree[control$syear %in% 2005], na.rm = T)
mean(control$neuro[control$syear %in% 2005], na.rm = T)

