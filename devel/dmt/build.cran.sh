~/bin/R-patched/bin/R CMD BATCH document.R
~/bin/R-patched/bin/R CMD check --as-cran pkg
~/bin/R-patched/bin/R CMD build pkg
~/bin/R-patched/bin/R CMD INSTALL dmt_0.8.20.tar.gz

#~/bin/R-2.13.2/bin/R CMD check pkg
#~/bin/R-2.13.2/bin/R CMD build pkg
#~/bin/R-2.13.2/bin/R CMD INSTALL sorvi_0.1.29.tar.gz

