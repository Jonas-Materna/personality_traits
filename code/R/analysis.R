library(ExPanDaR)
library(ggplot2)

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
          "plh0176",
          "plh0178",
          "plh0182"
  ),
  idvs = list(
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("syear")),
  format = "latex"
)



# Dependent variable:     Life and job satisfaction
# Independent variable:   Accountants
# Control group:          All
# Fixed effects:          No
# Interaction:            No


tab_sat_2 <-  prepare_regression_table(
  soep_data,
  dvs = c("plh0173",
          "plh0176",
          "plh0178",
          "plh0182"
  ),
  idvs = list(
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage"),
    c("acc","agree" ,"consc", "extra", "neuro", "openn",  "age", "female", "german", "married", "log_wage")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("syear")),
  format = "latex"
)





# Compare impact of personality on job satisfaction
tab <-  prepare_regression_table(
  soep_data[soep_data$acc == T | soep_data$prof == T,],
  dvs = "plh0173",
  idvs =
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage"),
  feffects = "syear",
  format = "latex"
)



tab <-  prepare_regression_table(
  soep_data,
  dvs = c("plh0173"
  ),
  idvs =
    c("acc","agree" ,"consc", "extra", "neuro", "openn", "pgtatzeit", "age", "female", "married", "log_wage"),
  feffects = "syear",
  format = "latex"
)





# Density plots
df <- big5[big5$acc == T | big5$prof == T,]
df$acc[df$acc==T] <- "Accountants"
df$acc[df$acc==F] <- "Professionals"
df$acc <- factor(df$acc, levels = c("Accountants", "Professionals"))

ggplot(df, aes(x=agree, fill=acc)) + 
  geom_density(adjust = 2, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") + 
  xlab("Agreeableness")

ggplot(df, aes(x=consc, fill=acc)) + 
  geom_density(adjust = 2, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") + 
  xlab("Conscientiousness")

ggplot(df, aes(x=extra, fill=acc)) + 
  geom_density(adjust = 2, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") + 
  xlab("Extraversion")

ggplot(df, aes(x=neuro, fill=acc)) + 
  geom_density(adjust = 2, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") + 
  xlab("Neuroticism")

ggplot(df, aes(x=openn, fill=acc)) + 
  geom_density(adjust = 2, alpha=0.2) +
  scale_fill_manual(values=c("#1B8A8F",  "#FFB43B", "#944664")) +
  scale_fill_discrete(name="") + 
  xlab("Openness")
