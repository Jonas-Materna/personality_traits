SOEP_DATA := data/generated/soep_data.rds

RESULTS := output/results.rda

RSCRIPT := Rscript

PRESENTATION := output/presentation.pdf

TARGETS := $(PRESENTATION)

all: $(TARGETS)

$(SOEP_DATA): code/R/prep_data.R
	$(RSCRIPT) code/R/prep_data.R


$(RESULTS): $(SOEP_DATA) code/R/bb_berlin.R
	$(RSCRIPT) code/R/bb_berlin.R


$(PRESENTATION): doc/presentation.Rmd $(RESULTS) doc/beamer_theme_trr266.sty
	$(RSCRIPT) -e 'library(rmarkdown); render("doc/presentation.Rmd")'
	mv doc/presentation.pdf output