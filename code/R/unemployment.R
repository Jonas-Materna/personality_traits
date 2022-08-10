
library(ExPanDaR)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(reshape)
# Load data
soep_data <- readRDS("data/generated/soep_data.rds")


#Get all Business and Administration Professionals with higher education and define groups
business_pro       <-  unique(soep_data$pid[substr(soep_data$pgisco08,1,2) %in% "24"])
higher_edu         <-  unique(soep_data$pid[soep_data$higher_edu == T]) 
business_pro        <- data.frame(pid=unique(soep_data$pid[soep_data$pid %in% business_pro & soep_data$higher_edu == T]) ) 

# 3 digit groups
business_pro$fin    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,3) %in% "241"])
business_pro$adm    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,3) %in% "242"])
business_pro$smp    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,3) %in% "243"])

# 4 digit groups 
business_pro$fin_acc    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2411"])
business_pro$fin_fia    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2412"])
business_pro$fin_fa     <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2413"])

business_pro$adm_moa    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2421"])
business_pro$adm_pap    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2422"])
business_pro$adm_pcp    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2423"])
business_pro$adm_tsd    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2424"])

business_pro$smp_amp    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2431"])
business_pro$smp_prp    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2432"])
business_pro$smp_tms    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2433"])
business_pro$smp_ict    <- business_pro$pid %in% unique(soep_data$pid[substr(soep_data$pgisco08,1,4) %in% "2434"])

df <- soep_data[soep_data$pid %in% business_pro$pid,]
df <- merge(df, business_pro, by = c("pid"))



####################
# Unemployment and cases of unemployment 
####################


#Identify year of first unemployment

d <- df[df$pgemplst %in% c(1,2),]
d <- d[d$plb0304_v14 %in% c(1:8),c("pid", "syear", "plb0304_v14", "pgexpue")]
d <- d[!duplicated(d$pid),]
d <- d[d$plb0304_v14 %in% c(1:8),]


#Worries around unemployment 
unemp <- df[df$pid %in% d$pid,]
unemp <- merge(unemp, d, by = c("pid"))
unemp$rel_year <- unemp$syear.x - unemp$syear.y
unemp <- unemp[unemp$rel_year %in% c(-5:5),c("rel_year", "plh0042")]


#Aggregate
unemp_worry <- aggregate(unemp$plh0042, by= list(unemp$rel_year), mean, na.rm=T)
unemp_worry <- 3-unemp_worry$x
plot(unemp_worry)










d <- soep_data[soep_data$pgemplst %in% c(1,2),]
d <- d[d$plb0304_v14 %in% c(1:8),c("pid", "syear", "plb0304_v14", "pgexpue")]
d <- d[!duplicated(d$pid),]
d <- d[d$plb0304_v14 %in% c(1:8),]


#Worries around unemployment 
unemp <- soep_data[soep_data$pid %in% d$pid,]
unemp <- merge(unemp, d, by = c("pid"))
unemp$rel_year <- unemp$syear.x - unemp$syear.y
unemp <- unemp[unemp$rel_year %in% c(-5:5),c("rel_year", "plh0042")]


#Aggregate
unemp_worry <- aggregate(unemp$plh0042, by= list(unemp$rel_year), mean, na.rm=T)
unemp_worry <- 3-unemp_worry$x
plot(unemp_worry)



accountant    <-  unique(soep_data$pid[soep_data$pgisco88 %in% 2411  | soep_data$pgisco08 %in% 2411])
d <- soep_data[soep_data$pid %in% accountant,]
d <- d[d$pgemplst %in% c(1,2),]
d <- d[d$plb0304_v14 %in% c(1:8),c("pid", "syear", "plb0304_v14", "pgexpue")]
d <- d[!duplicated(d$pid),]
d <- d[d$plb0304_v14 %in% c(1),]


#Worries around unemployment 
unemp <- soep_data[soep_data$pid %in% d$pid,]
unemp <- merge(unemp, d, by = c("pid"))
unemp$rel_year <- unemp$syear.x - unemp$syear.y
unemp <- unemp[unemp$rel_year %in% c(-5:5),c("rel_year", "plh0042")]


#Aggregate
unemp_worry <- aggregate(unemp$plh0042, by= list(unemp$rel_year), mean, na.rm=T)
unemp_worry <- 3-unemp_worry$x
plot(unemp_worry)





