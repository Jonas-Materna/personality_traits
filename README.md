## Personality Traits of Current and Future Accountants

## How do I reproduce the analysis?

Assuming that you have RStudio and make/Rtools installed, this should be relatively straightforward.

1. Download, clone or fork the repository to your local computing environment.
2. The following SOEP datasets from v37 are needed: biobirth, pgen, pl. Copy them to data/soep/
3. Before building everything you most likely need to install additional packages. This repository follows the established principle not to install any packages automatically. This is your computing environment. You decide what you want to install. See the code below for installing the packages.
4. Run 'make all' either via the console or by identifying the 'Build All' button in the 'Build' tab (normally in the upper right quadrant of the RStudio screen).

If you do not see 'Build' tab this is most likely because you do not have 'make' installed on your system. 
  - For Windows: Install Rtools: https://cran.r-project.org/bin/windows/Rtools/
  - For MacOS: You need to install the Mac OS developer tools. Open a terminal and run `xcode-select --install` Follow the instructions
  - On Linux: I have never seen a Unix environment without 'make'. 


```
# Code to install packages to your system
install_package_if_missing <- function(pkg) {
  if (! pkg %in% installed.packages()[, "Package"]) install.packages(pkg)
}
install_package_if_missing("haven")
install_package_if_missing("dyplr")
install_package_if_missing("stats")
install_package_if_missing("ExPanDaR")
install_package_if_missing("RPostgres")
install_package_if_missing("DBI")
install_package_if_missing("knitr")
install_package_if_missing("kableExtra")
install_package_if_missing("rmarkdown")
install_package_if_missing("tidyverse")
install_package_if_missing("modelr")
install_package_if_missing("broom")
install_package_if_missing("lubridate")

# In addition, if you have no working LaTeX environment, consider
# installing the neat tinytex LateX distribution. It is lightweight and
# you can install it from wihtin R! See https://yihui.org/tinytex/
# To install it, run from the R console:

install_package_if_missing('tinytex')
tinytex::install_tinytex()

# That's all!
```


This repository was built based on the ['treat' template for reproducible research](https://github.com/trr266/treat).