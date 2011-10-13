#!/bin/bash
R CMD roxygen Rcaline
rsync -Rav --exclude ".svn" Rcaline.roxygen/man Rcaline/man
R CMD check Rcaline --no-examples --no-install --no-tests --no-vignettes