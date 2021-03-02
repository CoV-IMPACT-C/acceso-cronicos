# Code 0: Validate movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse, arsenal)

# 2. Load data  -------------------------------------------
## movid_i-19
movid_o <- haven::read_dta("input/data/210123_base_movid_version02.dta")

x <- haven::read_dta("input/data/20210125_base_movid_version_codificada.dta")

# Compare
z <- anti_join(x, movid_i)
#59 obs distintas
z <- summary(comparedf(x,movid_o))
z$diffs.table
