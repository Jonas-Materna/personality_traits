
library(dplyr)
library(ExPanDaR)

soep_data <- readRDS("data/generated/soep_data.rds")
soep_data <- data.frame(soep_data)

###########
# Accountants vs. Professionals personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Accountant
# Control group:          Professionals
# Fixed effects:          Survey year

# Get sample
sample                <- soep_data[as.numeric(substr(soep_data$pgisco88, 1, 1)) %in%  2 | as.numeric(substr(soep_data$pgisco08, 1, 1)) %in%  2,]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411
sample$gender_age     <- sample$female*sample$age


tab_acc_prof_YearFE_big5 <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear"),c("syear"),c("syear"),c("syear")),
  format = "latex"
)



###########
# Accountants vs. Professionals personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Accountant
# Control group:          Professionals
# Fixed effects:          Person

# Get sample
sample                <- soep_data[as.numeric(substr(soep_data$pgisco88, 1, 1)) %in%  2 | as.numeric(substr(soep_data$pgisco08, 1, 1)) %in%  2,]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411
sample$gender_age     <- sample$female*sample$age


tab_acc_prof_PersonFE_big5 <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu")
  ),
  feffects = list(c("pid"),c("pid"),c("pid"),c("pid"),c("pid")),
  cluster = list(c("pid"),c("pid"),c("pid"),c("pid"),c("pid")),
  format = "latex"
)


###########
# Appendix
###########


###########
# Accountants vs. highly skilled personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Accountant
# Control group:          Highly skilled employees
# Fixed effects:          Survey year

# Get sample
sample                <- soep_data[soep_data$pgstib %in% c(530,540,550),]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411
sample$gender_age     <- sample$female*sample$age


tab_acc_skill_YearFE_big5 <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear"),c("syear"),c("syear"),c("syear")),
  format = "latex"
)






###########
# Young accountants vs. young higly skilled personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Young accountants
# Control group:          Young higly skilled
# Fixed effects:          Survey year

# Get sample
# Use only job entrants
sample                <- soep_data[soep_data$pgstib %in% c(530,540,550),]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411 

noexp      <- unique(sample$pid[sample$pgexpft == 0])
entry      <- unique(sample$pid[sample$pgexpft > 0])
entrants   <- entry[entry %in% noexp]
entrants   <- sample[sample$pid %in% entrants,]
entrants$gender_age     <- entrants$female*entrants$age

tab_yacc_yskill_YearFE_big5 <-  prepare_regression_table(
  entrants,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear"),c("syear"),c("syear"),c("syear")),
  format = "latex"
)



###########
# Accountants, bookkeepers and accounting clerks vs. full time employed personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Accountants, bookkeepers and accounting clerks
# Control group:          Full time employed
# Fixed effects:          Survey year

# Get sample
# Use only job entrants
sample                <- soep_data[soep_data$pgemplst %in% 1,]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411 | sample$pgisco88 %in%  3433 | sample$pgisco08 %in%  3433 | sample$pgisco88 %in%  4121 | sample$pgisco08 %in%  4311
sample$gender_age     <- sample$female*sample$age

tab_book_lab_YearFE_big5 <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear"),c("syear"),c("syear"),c("syear")),
  format = "latex"
)





###########
# Accountants vs. highly skilled personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Accountant
# Control group:          Highly skilled employees
# Fixed effects:          Person

# Get sample
sample                <- soep_data[soep_data$pgstib %in% c(530,540,550),]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411
sample$gender_age     <- sample$female*sample$age


tab_acc_skill_PersonFE_big5 <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu")
  ),
  feffects = list(c("pid"),c("pid"),c("pid"),c("pid"),c("pid")),
  cluster = list(c("pid"),c("pid"),c("pid"),c("pid"),c("pid")),
  format = "latex"
)






###########
# Young accountants vs. young higly skilled personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Young accountants
# Control group:          Young higly skilled
# Fixed effects:          Person

# Get sample
# Use only job entrants
sample                <- soep_data[soep_data$pgstib %in% c(530,540,550),]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411 

noexp      <- unique(sample$pid[sample$pgexpft == 0])
entry      <- unique(sample$pid[sample$pgexpft > 0])
entrants   <- entry[entry %in% noexp]
entrants   <- sample[sample$pid %in% entrants,]
entrants$gender_age     <- entrants$female*entrants$age

tab_yacc_yskill_PersonFE_big5 <-  prepare_regression_table(
  entrants,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu")
  ),
  feffects = list(c("pid"),c("pid"),c("pid"),c("pid"),c("pid")),
  cluster = list(c("pid"),c("pid"),c("pid"),c("pid"),c("pid")),
  format = "latex"
)



###########
# Accountants, bookkeepers and accounting clerks vs. full time employed personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Accountants, bookkeepers and Accounting clerks
# Control group:          Full time employed
# Fixed effects:          Person

# Get sample
sample                <- soep_data[soep_data$pgemplst %in% 1,]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411 | sample$pgisco88 %in%  3433 | sample$pgisco08 %in%  3433 | sample$pgisco88 %in%  4121 | sample$pgisco08 %in%  4311
sample$gender_age     <- sample$female*sample$age

tab_book_lab_PersonFE_big5 <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu"),
    c("accountant", "female", "age", "gender_age", "higher_edu")
  ),
  feffects = list(c("pid"),c("pid"),c("pid"),c("pid"),c("pid")),
  cluster = list(c("pid"),c("pid"),c("pid"),c("pid"),c("pid")),
  format = "latex"
)



###########
# Define names and safe
###########


# Define variable names for the regression tables

var_names_accountants_big5 <- tibble(
  var_name = c("consc", 
               "extra", 
               "agree", 
               "openn", 
               "neuro",
               "accountant",
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
  list = c(ls(pattern = "^var_names_*"), ls(pattern = "^tab_*")),
  file = "output/results_big5.rda"
)



