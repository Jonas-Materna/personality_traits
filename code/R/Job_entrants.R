


library(haven)
library(dplyr)
library(stats)
library(ggplot2)
library(egg)
library(ExPanDaR)

soep_data <- readRDS("data/generated/soep_data.rds")
biojob    <- read_dta(paste0("data/soep/biojob.dta"), col_select=c("pid",
                                                                        "einstieg_artk"))


soep_data <- merge(soep_data, biojob, by = c("pid"), all = F)

###########
# Young accountants vs. young higly skilled personality traits
###########


# Get sample
# Get people working in highly skilled jobs    
highly_skilled    <- unique(soep_data$pid[soep_data$pgstib %in% c("530","540","550")])

# Get people entereing the full time workforce within the sample period
entrants          <- unique(soep_data$pid[soep_data$einstieg_artk == soep_data$syear & soep_data$pgemplst == 1])

# Get a sample of highly skilled employees entering the workforce
sample            <- highly_skilled[highly_skilled %in% entrants]
sample            <- soep_data[soep_data$pid %in% sample,]


# Generate independent variable      
accountants         <- unique(soep_data$pid[soep_data$pgisco88 %in% c("2411", "3433", "4121") | soep_data$pgisco08  %in% c("2411","3433","4311")])
sample$accountant   <- sample$pid %in% accountants
sample$entered      <- !sample$einstieg_artk > sample$syear
sample$entered_acc  <- sample$accountant == T & sample$entered == T


#Get some values for control variable
sample$pglabgro[sample$pglabgro==0] <- NA
sample$log_wage       <- log(sample$pglabgro)
sample$gender_age     <- sample$female*sample$age
sample$neuro_acc      <- sample$neuro*sample$entered_acc


# Regression for personality traits in the cross section
tab_Big5_YearFE <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("entered_acc", "female", "age", "gender_age", "higher_edu"),
    c("entered_acc", "female", "age", "gender_age", "higher_edu"),
    c("entered_acc", "female", "age", "gender_age", "higher_edu"),
    c("entered_acc", "female", "age", "gender_age", "higher_edu"),
    c("entered_acc", "female", "age", "gender_age", "higher_edu")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear"),c("syear"),c("syear"),c("syear")),
  format = "latex"
)

# Regression for personality traits after job entry
tab_Big5_PersonFE <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("entered_acc", "female", "age", "gender_age", "higher_edu"),
    c("entered_acc", "female", "age", "gender_age", "higher_edu"),
    c("entered_acc", "female", "age", "gender_age", "higher_edu"),
    c("entered_acc", "female", "age", "gender_age", "higher_edu"),
    c("entered_acc", "female", "age", "gender_age", "higher_edu")
  ),
  feffects = list(c("pid"),c("pid"),c("pid"),c("pid"),c("pid")),
  cluster = list(c("pid"),c("pid"),c("pid"),c("pid"),c("pid")),
  format = "latex",
)
  


# How does openness relate to work satisfaction ?
mean_openn <- mean(sample$neuro, na.rm=T)
sample$high_openn <- sample$openn > mean_openn
sample$high_openn_acc <- sample$high_openn * sample$entered_acc

mean(sample$plh0173[sample$entered_acc==T & sample$high_openn==T], na.rm=T)
mean(sample$plh0173[sample$entered_acc==T & sample$high_openn==F], na.rm=T)
mean(sample$plh0173[sample$entered_acc==F & sample$high_openn==T], na.rm=T)
mean(sample$plh0173[sample$entered_acc==F & sample$high_openn==F], na.rm=T)



tab_JobSat_YearFE <-  prepare_regression_table(
  sample,
  dvs = c("plh0173", "plh0173"),
  idvs = list(
    c("entered_acc","high_openn" ,"high_openn_acc", "age", "higher_edu", "log_wage", "pgtatzeit"),
  c("entered_acc","high_openn" ,"high_openn_acc", "age", "higher_edu", "log_wage", "pgtatzeit")
  ),
  feffects = list(c("syear"),c("pid")),
  cluster = list(c("syear"),c("pid")),
  format = "latex"
)



# Plot big 5 over time

acc <- sample[sample$accountant==T,]
acc$dist  <- acc$syear-acc$einstieg_artk

consc <- data.frame(consc=acc$consc[!is.na(acc$consc)], dist =acc$dist[!is.na(acc$consc)])
consc_plot <- ggplot(consc, aes(x=dist, y=consc)) +
  geom_point() +
  geom_smooth()

extra <- data.frame(extra=acc$extra[!is.na(acc$extra)], dist =acc$dist[!is.na(acc$extra)])
extra_plot <- ggplot(extra, aes(x=dist, y=extra)) +
  geom_point() +
  geom_smooth()

agree <- data.frame(agree=acc$agree[!is.na(acc$agree)], dist =acc$dist[!is.na(acc$agree)])
agree_plot <- ggplot(agree, aes(x=dist, y=agree)) +
  geom_point() +
  geom_smooth()

open <- data.frame(open=acc$open[!is.na(acc$open)], dist =acc$dist[!is.na(acc$open)])
open_plot <- ggplot(open, aes(x=dist, y=open)) +
  geom_point() +
  geom_smooth()


neuro <- data.frame(neuro=acc$neuro[!is.na(acc$neuro)], dist =acc$dist[!is.na(acc$neuro)])
neuro_plot <- ggplot(neuro, aes(x=dist, y=neuro)) +
  geom_point() +
  geom_smooth()

#fig_big5 <- ggarrange(consc_plot, extra_plot, agree_plot, open_plot, neuro_plot, ncol = 3, nrow = 2)

fig_big5 <- neuro_plot


# Define the variable names

var_names_Job_Sat <- tibble(
  var_name = c("plh0173",
               "entered_acc",
               "high_openn",
               "high_openn_acc",
               "female", 
               "age",
               "higher_edu", 
               "log_wage", 
               "pgtatzeit"),
  label = cdesc_rnames <- c(
    "Work satisfaction",
    "Sample (1 = Accountant)",
    "Openness (1 = High)",
    "Sample x Openness",
    "Gender (1 = Female)",
    "Age",
    "Higher Education (1 = Yes)",
    "log(Income)",
    "Working Time"
  )
)



var_names_big5 <- tibble(
  var_name = c("consc", 
               "extra", 
               "agree", 
               "openn", 
               "neuro",
               "entered_acc",
               "female", 
               "age",
               "gender_age",
               "higher_edu"),
  label = cdesc_rnames <- c(
    "Conscientiousness",
    "Extraversion",
    "Agreeableness",
    "Openness",
    "Neuroticism",
    "Sample (1 = Accountant)",
    "Gender (1 = Female)",
    "Age",
    "Age x Gender",
    "Higher Education (1 = Yes)"
  )
)



save(
  list = c(ls(pattern = "^fig_*"),ls(pattern = "^var_names_*"), ls(pattern = "^tab_*")),
  file = "output/results_job_entrants.rda"
  )







neuro <- data.frame(neuro=sample$neuro[!is.na(sample$neuro)]
                    , syear =sample$syear[!is.na(sample$neuro)],
                    entry =sample$einstieg_artk[!is.na(sample$neuro)],
                    accountant = sample$accountant[!is.na(sample$neuro)])
neuro$dist  <-neuro$syear-neuro$entry

ggplot(neuro, aes(x=dist, y=neuro, color=accountant)) +
  geom_point() +
  geom_smooth()



consc <- data.frame(consc=sample$consc[!is.na(sample$consc)]
                    , syear =sample$syear[!is.na(sample$consc)],
                    entry =sample$einstieg_artk[!is.na(sample$consc)],
                    accountant = sample$accountant[!is.na(sample$consc)])
consc$dist  <-consc$syear-consc$entry

ggplot(consc, aes(x=dist, y=consc, color=accountant)) +
  geom_point() +
  geom_smooth()



agree <- data.frame(agree=sample$agree[!is.na(sample$agree)]
                    , syear =sample$syear[!is.na(sample$agree)],
                    entry =sample$einstieg_artk[!is.na(sample$agree)],
                    accountant = sample$accountant[!is.na(sample$agree)])
agree$dist  <-agree$syear-agree$entry

ggplot(agree, aes(x=dist, y=agree, color=accountant)) +
  geom_point() +
  geom_smooth()





acc_complete <- acc[complete.cases(acc[,c("consc","extra", "agree", "openn","neuro")]),]
