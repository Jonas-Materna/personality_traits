
library(ExPanDaR)
library(ggplot2)
source("code/R/theme_trr.R")

# Load data
soep_data <- readRDS("data/generated/soep_data.rds")


# Identify persons who were accountants or other professionals at some point
accountants    <-  unique(soep_data$pid[soep_data$pgisco88 %in% 2411  | soep_data$pgisco08 %in% 2411]) 
professionals  <-  unique(soep_data$pid[soep_data$pgisco88 %in% c(2000:3000)  | soep_data$pgisco08 %in% c(2000:3000)])
professionals  <-  professionals[!professionals %in% accountants]


# Check if person is an accountant or a professional
soep_data$acc  <- soep_data$pid %in% accountants 
soep_data$prof <- soep_data$pid %in% professionals



# Get the mean personality traits for each person
mean_traits    <- aggregate(soep_data[,c("consc", "extra", "agree", "openn", "neuro","plh0204_h")], 
                         by = list(soep_data$pid), 
                         mean, na.rm=T)
names(mean_traits)[names(mean_traits) == 'Group.1'] <- 'pid'

# Check if person is an accountant or a professional
mean_traits$acc  <- mean_traits$pid %in% accountants 
mean_traits$prof <- mean_traits$pid %in% professionals


# Get gender 
female             <- unique(soep_data$pid[soep_data$female == T])
mean_traits$female <- mean_traits$pid %in% female

# Run regressions
tab_big5_all <-  prepare_regression_table(
  mean_traits,
  dvs = c("agree", "agree", 
          "consc", "consc", 
          "extra", "extra",
          "neuro", "neuro",
          "openn", "openn"),
  idvs = list(
    c("acc"),
    c("acc", "female"),
    c("acc"),
    c("acc", "female"),
    c("acc"),
    c("acc", "female"),
    c("acc"),
    c("acc", "female"),
    c("acc"),
    c("acc", "female")
  ),
  format = "latex"
)


tab_big5_prof  <-  prepare_regression_table(
  mean_traits[mean_traits$acc == 1 | mean_traits$prof ==1,],
  dvs = c("agree", "agree", 
          "consc", "consc", 
          "extra", "extra",
          "neuro", "neuro",
          "openn", "openn"),
  idvs = list(
    c("acc"),
    c("acc", "female"),
    c("acc"),
    c("acc", "female"),
    c("acc"),
    c("acc", "female"),
    c("acc"),
    c("acc", "female"),
    c("acc"),
    c("acc", "female")
  ),
  format = "latex"
)




# Job satisfaction

#Merge data
df <- merge(soep_data, mean_traits, by = c("pid"), all = F)
names(df)[names(df) == 'agree.y']  <- 'agree'
names(df)[names(df) == 'consc.y']  <- 'consc'
names(df)[names(df) == 'extra.y']  <- 'extra'
names(df)[names(df) == 'neuro.y']  <- 'neuro'
names(df)[names(df) == 'openn.y']  <- 'openn'
names(df)[names(df) == 'female.y'] <- 'female'
df$other <-  df$acc == F & df$prof == F

tab <-  prepare_regression_table(
  df[df$acc == 1 | df$prof ==1,],
  dvs = c("plh0173",
          "plh0176",
          "plh0178",
          "plh0182"
          ),
  idvs = list(
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("syear")),
  format = "latex"
)



tab <-  prepare_regression_table(
  df,
  dvs = c("plh0173",
          "plh0176",
          "plh0178",
          "plh0182"
  ),
  idvs = list(
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("syear")),
  format = "latex"
)





# Compare impact of personality on job satisfaction
tab <-  prepare_regression_table(
  df[df$prof==T,],
  dvs = c("plh0173"
  ),
  idvs =
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage"),
  feffects = "syear",
  format = "latex"
)



tab <-  prepare_regression_table(
  df[df$other==T,],
  dvs = c("plh0173"
  ),
  idvs =
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage"),
  feffects = "syear",
  format = "latex"
)





##############################
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


df2 <- data.frame(group  = rep(c("Accountants", "Professionals", "All"), each=3),
                  period = rep(c("Pre-Employment", "Employed", "Post-Employment"),3),
                  lifeSat= c(
                    mean(df$plh0182[df$pre==T & df$acc == T], na.rm=T),
                    mean(df$plh0182[df$empl==T & df$acc == T], na.rm=T),
                    mean(df$plh0182[df$post==T & df$acc == T], na.rm=T),
                    mean(df$plh0182[df$pre==T & df$prof == T], na.rm=T),
                    mean(df$plh0182[df$empl==T & df$prof == T], na.rm=T),
                    mean(df$plh0182[df$post==T & df$prof == T], na.rm=T),
                    mean(df$plh0182[df$pre==T], na.rm=T),
                    mean(df$plh0182[df$empl==T], na.rm=T),
                    mean(df$plh0182[df$post==T], na.rm=T)
                  ))
df2$period <- factor(df2$period, levels = c("Pre-Employment", "Employed", "Post-Employment"))


# Line plot with multiple groups
ggplot(data=df2, aes(x=period, y=lifeSat, group=group)) +
  geom_line(aes(color=group), size = 1.5)+
  geom_point(aes(color=group), size =2)+
  scale_color_manual(values=c("#1B8A8F", "#FFB43B", "#944664")) +
  ylim(5,10) + 
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text=element_text(size=12, face = "bold"))


# Do some regressions 


tab <-  prepare_regression_table(
  df[df$prof == T | df$acc == T,],
  dvs = c("plh0182"),
  idvs =
    c("empl", "post", "acc_empl", "acc_post"),
  feffects = c("pid", "syear"),
  cluster = c("pid", "syear"),
  format = "latex"
)



summary(lm(plh0182~empl + post + acc_empl + acc_post + factor(pid) + factor(syear), 
           data=df[df$prof == T | df$acc == T,])) 

df$phrf


