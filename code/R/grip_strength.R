library(ExPanDaR)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(reshape)
# Load data
soep_data <- readRDS("data/generated/soep_data.rds")
soep_data <- soep_data[soep_data$syear < 2012,]

#Get all Business and Administration Professionals with higher education and define groups
higher_edu         <-  unique(soep_data$pid[soep_data$higher_edu == T]) 
business_pro       <-  unique(soep_data$pid[substr(soep_data$pgisco08,1,2) %in% "24"])

business_pro        <- data.frame(pid=unique(soep_data$pid[soep_data$pid %in% business_pro & soep_data$higher_edu == T]) ) 

# 2 Digit Groups
higher_edu$bus_pro  <- 

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
# Grip strength 
####################

# Get mean grip strength of left and right hand 
gripstr_r <- setNames(
  aggregate(df$gs03, by = list(df$pid), mean, na.rm=T),
  c("pid", "Right Hand"))

gripstr_l <- setNames(
  aggregate(df$gs05, by = list(df$pid), mean, na.rm=T),
  c("pid", "Left Hand"))

gripstr            <- merge(gripstr_r, gripstr_l, by = c("pid"))
gripstr            <- gripstr[!is.na(gripstr$`Right Hand`) & !is.na(gripstr$`Left Hand`),]
gripstr$Accountant <- gripstr$pid %in% business_pro$pid[business_pro$fin_acc == T]

ggplot(gripstr, aes(x=`Right Hand`, y=`Left Hand`, color=Accountant)) +
  geom_point(size = 4)



####################
# Grip strength: Boxplot by group
####################

df$gripstr <- ifelse(df$gs01 == 1,
                     df$gs03,
                     df$gs05)

gripstr         <- df[!is.na(df$gripstr),] 

d        <- gripstr[,c("pid","gripstr", "fin_acc", "fin", "adm", "smp", "sex")]
d$sex <- ifelse(d$sex == 1,
       "Male",
       "Female")



d$fin[which(d$fin_acc== T & d$fin == T)] = F
d        <- setNames(d, c("pid","Grip Strength (kg)", 
    "Accountants", "Other Finance Professionals", 
    "Administration Professionals", 
    "Sales, Marketing and PR Professionals",
    "Gender"))


d <- setNames(melt(d, id=c("pid","Grip Strength (kg)", "Gender")), c("pid","Grip Strength (kg)","Gender", "Job", "value"))
d <- d[d$value==T,]

p <- ggplot(d, aes(x=Job, y=`Grip Strength (kg)`)) + 
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitter(0.2), size =3, aes(colour = Gender))
p



