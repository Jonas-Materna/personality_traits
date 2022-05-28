SOEP_DATA := data/generated/soep_data.rds

BIG5 := output/results_big5.rda

JOBSAT := output/results_job_sat.rda

ENTRY := output/results_job_entrants.rda

RSCRIPT := Rscript

PRESENTATION := output/presentation.pdf

TARGETS := $(PRESENTATION)

all: $(TARGETS)

$(SOEP_DATA): code/R/prep_data.R
	$(RSCRIPT) code/R/prep_data.R


$(BIG5): $(SOEP_DATA) code/R/big5_regressions.R 
	$(RSCRIPT) code/R/big5_regressions.R

$(JOBSAT): $(SOEP_DATA) code/R/job_sat_regressions.R
	$(RSCRIPT) code/R/job_sat_regressions.R

$(ENTRY): $(SOEP_DATA) code/R/Job_entrants.R
	$(RSCRIPT) code/R/Job_entrants.R


$(PRESENTATION): doc/presentation.Rmd $(BIG5) $(JOBSAT) $(ENTRY) doc/beamer_theme_trr266.sty
	$(RSCRIPT) -e 'library(rmarkdown); render("doc/presentation.Rmd")'
	mv doc/presentation.pdf output