#!/usr/bin/env Rscript

library(plumber)
pr <- plumb('/home/lukasz/web.R')
pr$run(port=5000)
