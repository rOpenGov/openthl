#/usr/bin/R CMD BATCH document.R
~/bin/R-4.0.0/bin/R CMD build ../../ #--no-build-vignettes
~/bin/R-4.0.0/bin/R CMD check --as-cran openthl_0.1.1.tar.gz  #--no-build-vignettes
~/bin/R-4.0.0/bin/R CMD INSTALL openthl_0.1.1.tar.gz

