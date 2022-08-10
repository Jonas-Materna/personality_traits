library(ExPanDaR)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(reshape2)
library(dplyr)

# Load data
soep_data <- readRDS("data/generated/soep_data.rds")


# Identify business professionals
higher_edu         <-  unique(soep_data$pid[soep_data$higher_edu == T]) 
business_pro       <-  unique(soep_data$pid[substr(soep_data$pgisco88,1,3) %in% "241"  | 
                                              substr(soep_data$pgisco08,1,2) %in% "24"]) 

df <- soep_data[soep_data$pid %in% business_pro
                & soep_data$pid %in% higher_edu,]


# Identify accountants
accountants    <-  unique(df$pid[df$pgisco88 %in% 2411  | df$pgisco08 %in% 2411]) 
df$acc         <-  df$pid %in% accountants

# Define variables
df$german <- df$pgnation == 1

# Age categories
df$age_category_1 <- df$age<40
df$age_category_2 <- df$age>=40 & df$age < 60
df$age_category_3 <- df$age>=60


# Generate person fixed big 5

agree <- setNames(aggregate(df$agree, by = list(df$pid), function(x){mean(x, na.rm=T)}),
                     c("pid", "agree")
)

consc <- setNames(aggregate(df$consc, by = list(df$pid), function(x){mean(x, na.rm=T)}),
                      c("pid", "consc")
)

extra <- setNames(aggregate(df$extra, by = list(df$pid), function(x){mean(x, na.rm=T)}),
                         c("pid", "extra")
)

neuro <- setNames(aggregate(df$neuro, by = list(df$pid), function(x){mean(x, na.rm=T)}),
                  c("pid", "neuro")
)

openn <- setNames(aggregate(df$openn, by = list(df$pid), function(x){mean(x, na.rm=T)}),
                  c("pid", "openn")
)


df$agree <- NULL
df$consc <- NULL
df$extra <- NULL
df$neuro <- NULL
df$openn <- NULL

df <- merge(df, agree, by = c("pid"), all = F)
df <- merge(df, consc, by = c("pid"), all = F)
df <- merge(df, extra, by = c("pid"), all = F)
df <- merge(df, neuro, by = c("pid"), all = F)
df <- merge(df, openn, by = c("pid"), all = F)




####################
# Descriptive Statistics
####################

length(unique(df$pid[df$acc == T]))
length(unique(df$pid[df$acc == F]))

length(df$pid[df$acc == T])
length(df$pid[df$acc == F])


# Gender
d <- unique(df[,c("pid", "female", "acc")])
mean(d$female[d$acc==T]) - mean(d$female[d$acc==F])
t.test(d$female[d$acc==T],d$female[d$acc==F],alternative="two.sided",conf.level=0.95)


# Agreeableness
d <- unique(df[,c("pid", "agree", "acc")])
mean(d$agree[d$acc==T],na.rm=T) - mean(d$agree[d$acc==F],na.rm=T)
t.test(d$agree[d$acc==T], d$agree[d$acc==F], alternative="two.sided",conf.level=0.95)
length(is.na(!d$agree[d$acc==T]))
length(is.na(!d$agree[d$acc==F]))


# Conscientiousness
d <- unique(df[,c("pid", "consc", "acc")])
mean(d$consc[d$acc==T],na.rm=T) - mean(d$consc[d$acc==F],na.rm=T)
t.test(d$consc[d$acc==T], d$consc[d$acc==F], alternative="two.sided",conf.level=0.95)
length(is.na(!d$consc[d$acc==T]))
length(is.na(!d$consc[d$acc==F]))


# Extraversion
d <- unique(df[,c("pid", "extra", "acc")])
mean(d$extra[d$acc==T],na.rm=T) - mean(d$extra[d$acc==F],na.rm=T)
t.test(d$extra[d$acc==T], d$extra[d$acc==F], alternative="two.sided",conf.level=0.95)
length(is.na(!d$extra[d$acc==T]))
length(is.na(!d$extra[d$acc==F]))


# Neuroticism
d <- unique(df[,c("pid", "neuro", "acc")])
mean(d$neuro[d$acc==T],na.rm=T) - mean(d$neuro[d$acc==F],na.rm=T)
t.test(d$neuro[d$acc==T], d$neuro[d$acc==F], alternative="two.sided",conf.level=0.95)
length(is.na(!d$neuro[d$acc==T]))
length(is.na(!d$neuro[d$acc==F]))


# Openness
d <- unique(df[,c("pid", "openn", "acc")])
mean(d$openn[d$acc==T],na.rm=T) - mean(d$openn[d$acc==F],na.rm=T)
t.test(d$openn[d$acc==T], d$openn[d$acc==F], alternative="two.sided",conf.level=0.95)
length(is.na(!d$openn[d$acc==T]))
length(is.na(!d$openn[d$acc==F]))


# Gross wage
length(df$pglabgro[df$acc==T])
length(df$pglabgro[df$acc==F])
mean(df$pglabgro[df$acc==T], na.rm =T)
mean(df$pglabgro[df$acc==F], na.rm =T)
t.test(df$pglabgro[df$acc==T], df$pglabgro[df$acc==F], alternative="two.sided",conf.level=0.95)


###########
# Big 5 
###########

# Dependent variable:     Personality traits
# Independent variable:   Accountants
# Control group:          Professionals
# Fixed effects:          No
# Interaction:            No 

d <- df[!duplicated(df$pid), ] 

reg_big5<-  prepare_regression_table(
  d,
  dvs = c("agree","consc", "extra","neuro","openn"),
  idvs = list(
    c("acc", "female"),
    c("acc", "female"),
    c("acc", "female"),
    c("acc", "female"),
    c("acc", "female")
  ),
  format = "latex"
)

sum(!is.na(d$openn[d$acc == T]))
sum(!is.na(d$openn[d$acc == F]))




####################
# Worries / Values
####################


####################
# Political opinion
####################

d <- df[!is.na(df$plh0004),]
d$acc <- factor(d$acc, levels = c(TRUE, FALSE))

mean(d$plh0004[d$acc==T])
mean(d$plh0004[d$acc==F])

sum(!is.na(d$plh0004))
sum(!is.na(d$plh0004[d$acc==T]))
sum(!is.na(d$plh0004[d$acc==F]))

chisq.test(d$acc, d$plh0004)

fig_politics <- ggplot(d, aes(x=plh0004, fill=acc)) +
  geom_histogram(aes(y=0.5*..density..),
                 alpha=0.5,position='dodge',binwidth=0.5) + 
  scale_x_discrete(name ="Political attitude", limits=c(1:10)) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) + 
  theme(text=element_text(size=15)) +
  ylab("Density") + 
  guides(fill=guide_legend(title="Accountants"))
fig_politics



####################
# Are Accountants worried about climate change ?
####################

d <- df[!is.na(df$plh0037) & df$syear > 2009,]
d$plh0037 <- 4 - d$plh0037


mean(d$plh0037)

sum(!is.na(d$plh0037))
sum(!is.na(d$plh0037[d$acc==T]))
sum(!is.na(d$plh0037[d$acc==F]))


#Get perceived job insecurity over time
climate_high <- setNames(aggregate(d$plh0037, by = list(d$syear, d$acc), function(x){sum(x==3)}),
                             c("syear","accountant", "Very Concerned")
)

climate_mid <- setNames(aggregate(d$plh0037, by = list(d$syear, d$acc), function(x){sum(x==2)}),
                            c("syear","accountant", "Somewhat Concerned")
)

climate_low <- setNames(aggregate(d$plh0037, by = list(d$syear, d$acc), function(x){sum(x==1)}),
                            c("syear","accountant", "Not Concerned At All")
)


summary     <- merge(climate_high, climate_mid, by = c("syear", "accountant"))
summary     <- merge(summary, climate_low, by = c("syear", "accountant"))
total       <- summary$`Very Concerned`+ summary$`Somewhat Concerned` + summary$`Not Concerned At All`

summary$`Very Concerned`      <- summary$`Very Concerned` / total
summary$`Somewhat Concerned`  <- summary$`Somewhat Concerned` / total
summary$`Not Concerned At All`<- summary$`Not Concerned At All`  / total



summary <- setNames(
  melt(summary, id.vars = c("syear","accountant"), measure.vars = c("Very Concerned", "Somewhat Concerned", "Not Concerned At All")),
  c("Year","Accountant", "Are You Worried About Climate Change?", "Percentage"))

summary$Accountant <- factor(summary$Accountant, levels = c(TRUE, FALSE))



summary <- summary[!summary$`Are You Worried About Climate Change?` %in% "Somewhat Concerned",]
summary$Year <- as.character(summary$Year)
fig_climate <- ggplot(data=summary, aes(y=Percentage, x=Year, colour = `Are You Worried About Climate Change?`,
                              group=interaction(`Are You Worried About Climate Change?`, Accountant))) + 
  geom_line(aes(linetype=Accountant), size = 1) + 
  theme(text=element_text(size=15)) +   
  scale_color_manual(values=c("#1B8A8F",  "#FFB43B", "#944664"))

fig_climate



# Do regression

d <- df[!is.na(df$plh0037) & df$syear > 2009,]
d$plh0037 <- 4 - d$plh0037
d$openn_acc <- d$openn*d$acc

reg_climate <-  prepare_regression_table(
  d,
  dvs = c("plh0037","plh0037","plh0037","plh0037"),
  idvs = list(
    c("acc"),
    c("acc" ,"female", "age_category_2", "age_category_3"),
    c("acc", "openn", "openn_acc", "female","age_category_2", "age_category_3"),
    c("acc", "openn", "openn_acc","female", "age_category_2", "age_category_3")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("pid", "syear")),
  format = "latex"
)

length(d$pid[d$acc==T])
length(d$pid[d$acc==F])



#fully interacted
d$openn_acc  <- d$openn*d$acc
d$female_acc <- d$female*d$acc
d$age_category_2_acc <- d$age_category_2*d$acc
d$age_category_3_acc <- d$age_category_3*d$acc

reg_climate_int <-  prepare_regression_table(
  d,
  dvs = c("plh0037"),
  idvs = 
    c("acc", "openn", "openn_acc", "female","female_acc", "age_category_2","age_category_2_acc", "age_category_3", "age_category_3_acc")
,
  feffects ="syear",
  format = "latex"
)





####################
# Are Accountants worried about crime ?
####################

d <- df[!is.na(df$plh0040) & df$syear > 2009,]
d$plh0040 <- 4 - d$plh0040

mean(d$plh0040)

sum(!is.na(d$plh0040))
sum(!is.na(d$plh0040[d$acc==T]))
sum(!is.na(d$plh0037[d$acc==F]))


#Get perceived job insecurity over time
crime_high <- setNames(aggregate(d$plh0040, by = list(d$syear, d$acc), function(x){sum(x==3)}),
                             c("syear","accountant", "Very Concerned")
)

crime_mid <- setNames(aggregate(d$plh0040, by = list(d$syear, d$acc), function(x){sum(x==2)}),
                            c("syear","accountant", "Somewhat Concerned")
)

crime_low <- setNames(aggregate(d$plh0040, by = list(d$syear, d$acc), function(x){sum(x==1)}),
                            c("syear","accountant", "Not Concerned At All")
)


summary     <- merge(crime_high, crime_mid, by = c("syear", "accountant"))
summary     <- merge(summary, crime_low, by = c("syear", "accountant"))
total       <- summary$`Very Concerned`+ summary$`Somewhat Concerned` + summary$`Not Concerned At All`

summary$`Very Concerned`      <- summary$`Very Concerned` / total
summary$`Somewhat Concerned`  <- summary$`Somewhat Concerned` / total
summary$`Not Concerned At All`<- summary$`Not Concerned At All`  / total



summary <- setNames(
  melt(summary, id.vars = c("syear","accountant"), measure.vars = c("Very Concerned", "Somewhat Concerned", "Not Concerned At All")),
  c("Year","Accountant", "Are You Worried About Crime?", "Percentage"))

summary$Accountant <- factor(summary$Accountant, levels = c(TRUE, FALSE))




summary <- summary[!summary$`Are You Worried About Crime?` %in% "Somewhat Concerned",]
summary$Year <- as.character(summary$Year)
fig_crime <- ggplot(data=summary, aes(y=Percentage, x=Year, colour = `Are You Worried About Crime?`,
                                        group=interaction(`Are You Worried About Crime?`, Accountant))) + 
  geom_line(aes(linetype=Accountant), size = 1) + 
  theme(text=element_text(size=15)) +   
  scale_color_manual(values=c("#1B8A8F",  "#FFB43B", "#944664"))

fig_crime




# Do regression

d <- df[!is.na(df$plh0040) & df$syear > 2009,]
d$plh0040 <- 4 - d$plh0040
d$openn_acc <- d$openn*d$acc

reg_crime <-  prepare_regression_table(
  d,
  dvs = c("plh0040","plh0040","plh0040","plh0040"),
  idvs = list(
    c("acc"),
    c("acc" ,"female", "age_category_2", "age_category_3"),
    c("acc", "openn", "openn_acc", "female","age_category_2", "age_category_3"),
    c("acc", "openn", "openn_acc","female", "age_category_2", "age_category_3")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("pid", "syear")),
  format = "latex"
)

length(d$pid[d$acc==T])
length(d$pid[d$acc==F])



#fully interacted
d$openn_acc  <- d$openn*d$acc
d$female_acc <- d$female*d$acc
d$age_category_2_acc <- d$age_category_2*d$acc
d$age_category_3_acc <- d$age_category_3*d$acc

reg_crime_int <-  prepare_regression_table(
  d,
  dvs = c("plh0040"),
  idvs = 
    c("acc", "openn", "openn_acc", "female","female_acc", "age_category_2","age_category_2_acc", "age_category_3", "age_category_3_acc")
  ,
  feffects ="syear",
  format = "latex"
)







####################
# Careers of Accountants
####################

df$high_job_sat <- df$plh0173 > median(df$plh0173, na.rm=T)

mean(df$openn[df$high_job_sat==T & df$acc ==F], na.rm=T)
mean(df$openn[df$high_job_sat==F & df$acc ==F], na.rm=T)
mean(df$openn[df$high_job_sat==T & df$acc ==T], na.rm=T)
mean(df$openn[df$high_job_sat==F & df$acc ==T], na.rm=T)



# Job satisfaction
reg_job_sat <-  prepare_regression_table(
  df,
  dvs = c("plh0173",
          "plh0173",
          "plh0173"
  ),
  idvs = list(
    c("acc"),
    c("acc","age_category_2","age_category_3", "female", "log_wage", "pguebstd"),
    c("acc","age_category_2","age_category_3", "female", "log_wage", "pguebstd","agree" ,"consc", "extra", "neuro", "openn")
  ),
  feffects = list(c("syear"),c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear"),c("syear")),
  format = "latex"
)


# Determinants of job satisfaction: Accountants
reg_job_sat_acc <-  prepare_regression_table(
  df[df$acc == T,],
  dvs = c("plh0173"),
  c("age_category_2","age_category_3", "female", "log_wage", "pguebstd","agree" ,"consc", "extra", "neuro", "openn"),
  feffects = c("syear"),
  cluster = c("syear"),
  format = "latex"
)

# Determinants of job satisfaction: Business Professionals
reg_job_sat_bus <-  prepare_regression_table(
  df[df$acc == F,],
  dvs = c("plh0173"),
  c("acc","age_category_2","age_category_3", "female", "log_wage", "pguebstd","agree" ,"consc", "extra", "neuro", "openn"),
  feffects = c("syear"),
  cluster = c("syear"),
  format = "latex"
)




# Leadership role
leader <- unique(df$pid[df$plb0064_v1 %in% 5  | df$plb0064_v2 %in% 6 |  df$plb0064_v4 %in% 6])
df$leader <- df$plb0064_v1 %in% 5  | df$plb0064_v2 %in% 6 |  df$plb0064_v4 %in% 6


length(unique(df$pid[df$pid %in% leader & df$acc ==T]))
length(unique(df$pid[!df$pid %in% leader & df$acc ==T]))
length(unique(df$pid[df$pid %in% leader & df$acc ==F]))
length(unique(df$pid[!df$pid %in% leader & df$acc ==F]))

mean(df$pglabgro[df$acc ==T & df$leader==T], na.rm = T)
mean(df$pglabgro[df$acc ==T & df$leader==F], na.rm = T)
mean(df$pglabgro[df$acc ==F & df$leader==T], na.rm = T)
mean(df$pglabgro[df$acc ==F & df$leader==F], na.rm = T)

mean(df$female[df$acc ==T & df$leader==T], na.rm = T)
mean(df$female[df$acc ==T & df$leader==F], na.rm = T)
mean(df$female[df$acc ==F & df$leader==T], na.rm = T)
mean(df$female[df$acc ==F & df$leader==F], na.rm = T)

mean(df$openn[df$acc ==T & df$leader==T], na.rm = T)
mean(df$openn[df$acc ==T & df$leader==F], na.rm = T)
mean(df$openn[df$acc ==F & df$leader==T], na.rm = T)
mean(df$openn[df$acc ==F & df$leader==F], na.rm = T)

reg_leader <-  prepare_regression_table(
  df,
  dvs = c("leader",
          "leader",
          "leader"
  ),
  idvs = list(
    c("acc"),
    c("acc","age_category_2","age_category_3", "female"),
    c("acc", "agree" ,"consc", "extra", "neuro", "openn","age_category_2","age_category_3", "female")
  ),
  feffects = list(c("syear"),c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear"),c("syear")),
  models = list(c("ols"),c("ols"),c("ols")),
  format = "latex"
)






reg_leader_acc <-  prepare_regression_table(
  df[df$acc == T,],
  dvs = c("leader"),
  idvs = c("agree" ,"consc", "extra", "neuro", "openn", "age_category_2","age_category_3", "female"),
  models = "ols",
  format = "latex"
)



reg_leader_bus <-  prepare_regression_table(
  df[df$acc == F,],
  dvs = c("leader"),
  idvs = c("agree" ,"consc", "extra", "neuro", "openn", "age_category_2","age_category_3", "female"),
  models = "ols",
  format = "latex"
)


# Appendix 
# Stage of career













####################
# Grip strength
####################

# Get stronger hand
df$gripstr  <- ifelse(df$gs01 == 1,
                     df$gs03,
                     df$gs05)
gripstr         <- df[!is.na(df$gripstr),] 

# Prep data
d         <- gripstr[,c("pid","gripstr", "acc", "sex")]
d$sex     <- ifelse(d$sex == 1,
                "Male",
                "Female")
d$oth_bus <- d$acc == 0

d        <- setNames(d, c("pid","Grip Strength (kg)", 
                          "Accountants", 
                          "Gender", "Other Business Professionals"))


d <- setNames(melt(d, id=c("pid","Grip Strength (kg)", "Gender")), c("pid","Grip Strength (kg)","Gender", "Job", "value"))
d <- d[d$value==T,]
d$`Grip Strength (kg)` <- d$`Grip Strength (kg)`/10
p <- ggplot(d, aes(x=Job, y=`Grip Strength (kg)`)) + 
  geom_boxplot() + 
  geom_jitter(shape=16, position=position_jitter(0.2), size =3, aes(colour = Gender))+   
  scale_color_manual(values=c("#1B8A8F",  "#FFB43B", "#944664"))
p

t.test(d$`Grip Strength (kg)`[d$Job=="Accountants"], d$`Grip Strength (kg)`[d$Job %in% "Other Business Professionals"], alternative="two.sided",conf.level=0.95)

# Do regression
# grip_reg <-  prepare_regression_table(
#   d,
#   dvs = c("gripstr","gripstr","gripstr"),
#   idvs = list(
#     c("acc"),
#     c("acc", "female", "age"),
#     c("acc", "female", "age")
#   ),
#   feffects = list("","",c("pid")),
#   format = "latex"
# )


# Are leaders in accountancy different from leaders in other professions












####################
# Do formatting and save
####################

var_names_big5 <- tibble(
  var_name = c("agree",
               "consc", 
               "extra", 
               "neuro",               
               "openn", 
               "acc",
               "female",
               "german"),
  label = cdesc_rnames <- c(
    "Agreeableness",
    "Conscientiousness",
    "Extraversion",
    "Neuroticism",
    "Openness",
    "Accountant (1 = TRUE)",
    "Female (1 = TRUE)",
    "German (1 = TRUE)"
  )
)



var_names_climate <- tibble(
  var_name = c("plh0037", 
               "acc",
               "openn",
               "openn_acc",
               "female",
               "age_category_2",
               "age_category_3"),
  label = cdesc_rnames <- c(
    "Climate change worries",
    "Accountant (1 = TRUE)",
    "Openness",
    "Openness x Accountant",
    "Female (1 = TRUE)",
    "Age Category 2 (40-60)",
    "Age Category 3 (> 60)"
  )
)


var_names_crime <- tibble(
  var_name = c("plh0040", 
               "acc",
               "openn",
               "openn_acc",
               "female",
               "age_category_2",
               "age_category_3"),
  label = cdesc_rnames <- c(
    "Crime worries",
    "Accountant (1 = TRUE)",
    "Openness",
    "Openness x Accountant",
    "Female (1 = TRUE)",
    "Age Category 2 (40-60)",
    "Age Category 3 (> 60)"
  )
)




var_names_job_sat <- tibble(
  var_name = c("plh0173",
               "acc",
               "age_category_2",
               "age_category_3",
               "female",
               "log_wage",
               "pguebstd",
               "agree" ,
               "consc",
               "extra",
               "neuro",
               "openn"
),
  label = cdesc_rnames <- c(
    "Job Satisfaction",
    "Accountant (1 = TRUE)",
    "Age Category 2 (40-60)",
    "Age Category 3 (> 60)",
    "Female (1 = TRUE)",
    "log(wage)",
    "Overtime",
    "Agreeableness",
    "Conscientiousness",
    "Extraversion",
    "Neuroticism",
    "Openness"
  )
)



var_names_job_sat_acc <- tibble(
  var_name = c("plh0173",
               "age_category_2",
               "age_category_3",
               "female",
               "log_wage",
               "pguebstd",
               "agree" ,
               "consc",
               "extra",
               "neuro",
               "openn"
  ),
  label = cdesc_rnames <- c(
    "Job Satisfaction (Sample: Accountants)",
    "Age Category 2 (40-60)",
    "Age Category 3 (> 60)",
    "Female (1 = TRUE)",
    "log(wage)",
    "Overtime",
    "Agreeableness",
    "Conscientiousness",
    "Extraversion",
    "Neuroticism",
    "Openness"
  )
)


var_names_job_sat_bus <- tibble(
  var_name = c("plh0173",
               "age_category_2",
               "age_category_3",
               "female",
               "log_wage",
               "pguebstd",
               "agree" ,
               "consc",
               "extra",
               "neuro",
               "openn"
  ),
  label = cdesc_rnames <- c(
    "Job Satisfaction (Sample: Business Professionals)",
    "Age Category 2 (40-60)",
    "Age Category 3 (> 60)",
    "Female (1 = TRUE)",
    "log(wage)",
    "Overtime",
    "Agreeableness",
    "Conscientiousness",
    "Extraversion",
    "Neuroticism",
    "Openness"
  )
)




var_names_leader <- tibble(
  var_name = c("leader",
               "acc", 
               "agree",
               "consc",
               "extra",
               "neuro",
               "openn",
               "age_category_2",
               "age_category_3",
               "female"
  ),
  label = cdesc_rnames <- c(
    "Leadership role",
    "Accountant (1 = TRUE)",
    "Agreeableness",
    "Conscientiousness",
    "Extraversion",
    "Neuroticism",
    "Openness",
    "Age Category 2 (40-60)",
    "Age Category 3 (> 60)",
    "Female (1 = TRUE)"
  )
)



var_names_leader_acc <- tibble(
  var_name = c("leader",
               "agree",
               "consc",
               "extra",
               "neuro",
               "openn",
               "age_category_2",
               "age_category_3",
               "female"
  ),
  label = cdesc_rnames <- c(
    "Leadership role (Sample: Accountants)",
    "Agreeableness",
    "Conscientiousness",
    "Extraversion",
    "Neuroticism",
    "Openness",
    "Age Category 2 (40-60)",
    "Age Category 3 (> 60)",
    "Female (1 = TRUE)"
  )
)


var_names_leader_bus <- tibble(
  var_name = c("leader",
               "agree",
               "consc",
               "extra",
               "neuro",
               "openn",
               "age_category_2",
               "age_category_3",
               "female"
  ),
  label = cdesc_rnames <- c(
    "Leadership role (Sample: Other Business Professionals)",
    "Agreeableness",
    "Conscientiousness",
    "Extraversion",
    "Neuroticism",
    "Openness",
    "Age Category 2 (40-60)",
    "Age Category 3 (> 60)",
    "Female (1 = TRUE)"
  )
)









save(
  list = c(ls(pattern = "^var_*"), ls(pattern = "^reg_*"), ls(pattern = "^fig_*")),
  file = "output/results.rda"
)


