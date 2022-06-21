library(ExPanDaR)
library(ggplot2)
library(tidyr)
library(ggpubr)
# Load data
soep_data <- readRDS("data/generated/soep_data.rds")


# Identify persons who were accountants or other professionals at some point
accountants    <-  unique(soep_data$pid[soep_data$pgisco88 %in% 2411  | soep_data$pgisco08 %in% 2411]) 
professionals  <-  unique(soep_data$pid[soep_data$pgisco88 %in% c(2000:3000)  | soep_data$pgisco08 %in% c(2000:3000)])
professionals  <-  professionals[!professionals %in% accountants]


# Check if person is an accountant or a professional
soep_data$acc    <- soep_data$pid %in% accountants 
soep_data$prof   <- soep_data$pid %in% professionals
soep_data$german <- soep_data$pgnation == 1


# Get the mean personality traits for each person
big5    <- aggregate(soep_data[,c("consc", "extra", "agree", "openn", "neuro","plh0204_h")], 
                            by = list(soep_data$pid), 
                            mean, na.rm=T)
names(big5)[names(big5) == 'Group.1'] <- 'pid'

# Get some variables
big5$acc    <- big5$pid %in% accountants 
big5$prof   <- big5$pid %in% professionals
big5$female <- big5$pid %in% unique(soep_data$pid[soep_data$female == T])  
big5$german <- big5$pid %in% unique(soep_data$pid[soep_data$pgnation == 1])  
big5$female_acc <- big5$acc*big5$female
big5$german_acc <- big5$acc*big5$german

###########
# Big 5 regression 
###########

# Dependent variable:     Personality traits
# Independent variable:   Accountants
# Control group:          Professionals
# Fixed effects:          No
# Interactiona:           No 

tab_big5_1 <-  prepare_regression_table(
  big5[big5$acc == T | big5$prof == T,],
  dvs = c("agree","consc", "extra","neuro","openn"),
  idvs = list(
    c("acc", "female", "german"),
    c("acc", "female", "german"),
    c("acc", "female", "german"),
    c("acc", "female", "german"),
    c("acc", "female", "german")
  ),
  format = "latex"
)

# Dependent variable:     Personality traits
# Independent variable:   Accountants
# Control group:          All
# Fixed effects:          No
# Interactiona:           No


tab_big5_2 <-  prepare_regression_table(
  big5,
  dvs = c("agree","consc", "extra","neuro","openn"),
  idvs = list(
    c("acc", "female", "german"),
    c("acc", "female", "german"),
    c("acc", "female", "german"),
    c("acc", "female", "german"),
    c("acc", "female", "german")
  ),
  format = "latex"
)


# Dependent variable:     Personality traits
# Independent variable:   Accountants
# Control group:          All
# Fixed effects:          No
# Interactiona:           Yes


tab_big5_3 <-  prepare_regression_table(
  big5[big5$acc == T | big5$prof == T,],
  dvs = c("agree","consc", "extra","neuro","openn"),
  idvs = list(
    c("acc", "female", "german", "female_acc", "german_acc"),
    c("acc", "female", "german", "female_acc", "german_acc"),
    c("acc", "female", "german", "female_acc", "german_acc"),
    c("acc", "female", "german", "female_acc", "german_acc"),
    c("acc", "female", "german", "female_acc", "german_acc")
  ),
  format = "latex"
)


# Dependent variable:     Personality traits
# Independent variable:   Accountants
# Control group:          All
# Fixed effects:          No
# Interaction:            Yes

tab_big5_4 <-  prepare_regression_table(
  big5,
  dvs = c("agree","consc", "extra","neuro","openn"),
  idvs = list(
    c("acc", "female", "german", "female_acc", "german_acc"),
    c("acc", "female", "german", "female_acc", "german_acc"),
    c("acc", "female", "german", "female_acc", "german_acc"),
    c("acc", "female", "german", "female_acc", "german_acc"),
    c("acc", "female", "german", "female_acc", "german_acc")
  ),
  format = "latex"
)




###########
# Satisfaction 
###########

# Dependent variable:     Life and job satisfaction
# Independent variable:   Accountants
# Control group:          Professionals
# Fixed effects:          No
# Interaction:            No


tab_sat_1 <-  prepare_regression_table(
  soep_data[soep_data$acc == T | soep_data$prof == T,],
  dvs = c("plh0173",
          "plh0182"
  ),
  idvs = list(
    c("acc"),
    c("acc")
  ),
  feffects = list(c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear")),
  format = "latex"
)



tab_sat_2 <-  prepare_regression_table(
  soep_data[soep_data$acc == T | soep_data$prof == T,],
  dvs = c("plh0173",
          "plh0182"
  ),
  idvs = list(
    c("acc", "age", "female", "german", "married", "log_wage"),
    c("acc", "age", "female", "german", "married", "log_wage")
  ),
  feffects = list(c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear")),
  format = "latex"
)


tab_sat_3 <-  prepare_regression_table(
  soep_data[soep_data$acc == T | soep_data$prof == T,],
  dvs = c("plh0173",
          "plh0182"
  ),
  idvs = list(
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage")
  ),
  feffects = list(c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear")),
  format = "latex"
)







# Compare impact of personality on job satisfaction
tab_acc_sat <-  prepare_regression_table(
  soep_data[soep_data$acc == T,],
  dvs = "plh0173",
  idvs =
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage"),
  feffects = "syear",
  format = "latex"
)


tab_prof_sat <-  prepare_regression_table(
  soep_data[soep_data$prof == T,],
  dvs = "plh0173",
  idvs =
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage"),
  feffects = "syear",
  format = "latex"
)


tab_all_sat <-  prepare_regression_table(
  soep_data,
  dvs = "plh0173",
  idvs =
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage"),
  feffects = "syear",
  format = "latex"
  
  
)


# Density plots for life satisfaction
df <- soep_data[soep_data$acc == T | soep_data$prof == T,]
df$acc[df$acc==T] <- "Accountants"
df$acc[df$acc==F] <- "Professionals"
df$acc <- factor(df$acc, levels = c("Accountants", "Professionals"))

work_life_sat <- ggplot(df, aes(x=plh0173, fill=acc)) +
  geom_density(adjust = 5, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") +
  xlab("Work Satisfaction") + 
  theme(
    axis.title.x = element_text(size=12, face = "bold"),
    legend.text=element_text(size=12),
    axis.text=element_text(size=12, face = "bold"))


work_life_sat


fig_life_sat <- ggplot(df, aes(x=plh0182, fill=acc)) +
  geom_density(adjust = 7, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") +
  xlab("Life Satisfaction") + 
  theme(
    axis.title.x = element_text(size=12, face = "bold"),
    legend.text=element_text(size=12),
    axis.text=element_text(size=12, face = "bold"))


fig_life_sat




# Density plots for big 5
df <- big5[big5$acc == T | big5$prof == T,]
df$acc[df$acc==T] <- "Accountants"
df$acc[df$acc==F] <- "Professionals"
df$acc <- factor(df$acc, levels = c("Accountants", "Professionals"))

fig_agree <- ggplot(df, aes(x=agree, fill=acc)) +
  geom_density(adjust = 2, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") +
  xlab("Agreeableness") + 
  theme(
    axis.title.x = element_text(size=12, face = "bold"),
    legend.text=element_text(size=12),
    axis.text=element_text(size=12, face = "bold"))


fig_consc <-ggplot(df, aes(x=consc, fill=acc)) +
  geom_density(adjust = 2, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") +
  xlab("Conscientiousness") + 
  theme(
    axis.title.x = element_text(size=12, face = "bold"),
    legend.text=element_text(size=12),
    axis.text=element_text(size=12, face = "bold"))


fig_extra <-ggplot(df, aes(x=extra, fill=acc)) +
  geom_density(adjust = 2, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") +
  xlab("Extraversion") + 
  theme(
    axis.title.x = element_text(size=12, face = "bold"),
    legend.text=element_text(size=12),
    axis.text=element_text(size=12, face = "bold"))






fig_neuro <-ggplot(df, aes(x=neuro, fill=acc)) +
  geom_density(adjust = 2, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") +
  xlab("Neuroticism") + 
  theme(
    axis.title.x = element_text(size=12, face = "bold"),
    legend.text=element_text(size=12),
    axis.text=element_text(size=12, face = "bold"))

fig_openn <-ggplot(df, aes(x=openn, fill=acc)) +
  geom_density(adjust = 2, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") +
  xlab("Openness") + 
  theme(
    axis.title.x = element_text(size=12, face = "bold"),
    legend.text=element_text(size=12),
    axis.text=element_text(size=12, face = "bold"))



ggarrange(fig_agree, fig_consc, fig_extra,fig_neuro , fig_openn, 
          labels = c("Agreeableness", "Conscientiousness", "C", "D", "E"),
          ncol = 3, nrow = 2)








#############################
# Focus on periods around employment (income data hardly available for pre-/post periods) 
##############################

#Get the employment status
df              <- soep_data
emply_year      <- soep_data[ soep_data$pglfs == 11, c("pid", "syear")] 
empl_start_year <- setNames(aggregate(emply_year$syear, by = list(emply_year$pid), min), c("pid", "empl_start_year")) 
empl_end_year   <- setNames(aggregate(emply_year$syear, by = list(emply_year$pid), max), c("pid", "empl_end_year"))
df              <- merge(df, empl_start_year, by = c("pid"), all = F)
df              <- merge(df, empl_end_year, by = c("pid"), all = F)

df$pre          <- df$syear < df$empl_start_year
df$empl         <- df$syear >= df$empl_start_year & df$syear <= df$empl_end_year 
df$post         <- df$syear > df$empl_end_year 
df$acc_empl     <- df$acc * df$empl
df$acc_post     <- df$acc * df$post


# Focus on period 2005 to 2019 (big 5 data must be available)
df              <- df[df$syear >= 2005 & df$syear <= 2019,]


df2 <- data.frame(group  = rep(c("Accountants", "Professionals"), each=3),
                  period = rep(c("Pre-Employment", "Employed", "Post-Employment"),2),
                  lifeSat= c(
                    mean(df$plh0182[df$pre==T & df$acc == T], na.rm=T),
                    mean(df$plh0182[df$empl==T & df$acc == T], na.rm=T),
                    mean(df$plh0182[df$post==T & df$acc == T], na.rm=T),
                    mean(df$plh0182[df$pre==T & df$prof == T], na.rm=T),
                    mean(df$plh0182[df$empl==T & df$prof == T], na.rm=T),
                    mean(df$plh0182[df$post==T & df$prof == T], na.rm=T)
                  ))
df2$period <- factor(df2$period, levels = c("Pre-Employment", "Employed", "Post-Employment"))


# Line plot with multiple groups
ggplot(data=df2, aes(x=period, y=lifeSat, group=group)) +
  geom_line(aes(color=group), size = 1.5)+
  geom_point(aes(color=group), size =2)+
  scale_color_manual(values=c("#1B8A8F", "#FFB43B", "#944664")) +
  ylim(6.5,8) + 
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text=element_text(size=12),
    axis.text=element_text(size=12, face = "bold"))




tab_sat_pid_FE <-  prepare_regression_table(
  df[df$prof == T | df$acc == T,],
  dvs = c("plh0182"),
  idvs =
    c("empl", "post", "acc_empl", "acc_post"),
  feffects = c("pid", "syear"),
  cluster = c("pid", "syear"),
  format = "latex"
)



# Get variable names and save
var_sat_pid_FE <- tibble(
  var_name = c("plh0182",
    "empl",
               "post", 
               "acc_empl", 
               "acc_post"),
  label = cdesc_rnames <- c(
    "Life Satisfaction",
    "Employed (1 = True)",
    "Post-Emploxment (1 = True)",
    "Accountant x Employed",
    "Accountant x Post-Employed"
  )
)




var_names_big5 <- tibble(
  var_name = c("agree",
               "consc", 
               "extra", 
               "neuro",               
               "openn", 
               "acc",
               "female",
               "german",
               "female_acc", 
               "german_acc"),
  label = cdesc_rnames <- c(
    "Agreeableness",
    "Conscientiousness",
    "Extraversion",
    "Neuroticism",
    "Openness",
    "Sample (1 = Accountant)",
    "Gender (1 = Female)",
    "German (1 = True)",
    "Accountant x Female",
    "Accountant x German"
  )
)


var_names_sat <- tibble(
  var_name = c("plh0173",
               "plh0182",
               "acc",
               "agree",
               "consc",
               "extra",
               "neuro",
               "openn", 
               "age", 
               "female", 
               "german",
               "married", 
               "log_wage"),
  label = cdesc_rnames <- c(
    "Work satisfaction",
    "Life satisfaction",
    "Accountant (1 = TRUE)",
    "Agreeableness",
    "Conscientiousness",
    "Extraversion",
    "Neuroticism",
    "Openness",
    "Age",
    "Gender (1 = Female)",
    "German (1 = True)",
    "Married (1 = Yes)",
    "log(Income)"
  )
)



var_names_sat_control_only <- tibble(
  var_name = c("plh0173",
               "plh0182",
               "acc",
               "age", 
               "female", 
               "german",
               "married", 
               "log_wage"),
  label = cdesc_rnames <- c(
    "Work satisfaction",
    "Life satisfaction",
    "Accountant (1 = TRUE)",
    "Age",
    "Gender (1 = Female)",
    "German (1 = True)",
    "Married (1 = Yes)",
    "log(Income)"
  )
)


save(
  list = c(ls(pattern = "^var_*"), ls(pattern = "^tab_*"), ls(pattern = "^fig_*")),
  file = "output/results.rda"
)





numb_obs <- data.frame(table(soep_data$syear))
colnames(numb_obs) <- c("Year", "Number of observations")

# Line plot with multiple groups
ggplot(data=numb_obs , aes(x=numb_obs$Year, y=numb_obs$`Number of observations`)) +
  geom_line(size = 1.5)+
  geom_point( size =2)+
  scale_color_manual(values=c("#1B8A8F", "#FFB43B", "#944664")) +
  xlab("Year")+
  ylab("Number of observations")+
  theme(
    legend.text=element_text(size=12),
    axis.title = element_text(size=12),
    axis.text.x = element_text(angle = 45),
    axis.text=element_text(size=12, face = "bold"))




