# Load packages
library(tidyverse)
library(fixest)
library(did2s)
library(modelsummary)

# Simulation parameters
n           = 1000
t           = 10
pr_treat    = 0.10
pr_spill    = 0.20
tau_treat   = 0.10
tau_hetro   = 0.01
tau_spill   = -0.05
spill_fade  = -0.002

# ------------------------------------------------------------------------------------- (1) Simulate data ----
# Create baseline frame
dta = data.frame("id" = c(1:n),
                 "group" = sample(c("Treated", "Spillover", "True Control"), n, replace = TRUE,
                                  prob = c(pr_treat, pr_spill, (1-(pr_treat+pr_spill)))),
                 "baseline" = rnorm(n),
                 "tr_year" = sample(c(1:t), n, replace = TRUE),
                 "sp_year" = sample(c(1:t), n, replace = TRUE),
                 "distance" = runif(n, min = 1, max = 50)) |>
  mutate(tr_year = ifelse(group == "Treated", tr_year, Inf),
         sp_year = ifelse(group == "Spillover", sp_year, Inf),
         distance = ifelse(group == "Spillover", distance, Inf))

# Expand to all years
dta = dta |>
  group_by(id) |>
  expand(time = 1:10) |>
  left_join(dta) |>
  ungroup() |>
  mutate(treated = ifelse(group == "Treated" & time >= tr_year, 1, 0),
         spill = ifelse(group == "Spillover" & time >= sp_year, 1, 0),
         tr_rel_year = ifelse(group == "Treated", time - tr_year, Inf),
         sp_rel_year = ifelse(group == "Spillover", time - sp_year, Inf),
         spillover_1_to_10 = spill*ifelse(distance <= 10, 1, 0),
         spillover_10_to_20 = spill*ifelse(distance > 10 & distance <= 20, 1, 0),
         spillover_20_to_30 = spill*ifelse(distance > 20 & distance <= 30, 1, 0),
         spillover_30_to_40 = spill*ifelse(distance > 30 & distance <= 40, 1, 0),
         spillover_40_to_50 = spill*ifelse(distance > 40 & distance <= 50, 1, 0))

# Apply treatment/spillover effects
dta = dta |>
  mutate(y = case_when(group == "True Control" ~ baseline + rnorm(n, sd = 0.10),
                       group == "Treated" ~ baseline + rnorm(n, sd = 0.10) + 
                         (treated*tau_treat) + (treated*tr_rel_year*tau_hetro),
                       group == "Spillover" ~ baseline + rnorm(n, sd = 0.10) +
                         ((spill*tau_spill)-distance*spill_fade)))

# Making a nice spillover categorical
dta = dta |>
  mutate(spillover_cat = case_when(spillover_1_to_10 == 1 ~ "G1",
                                   spillover_10_to_20 == 1 ~ "G2",
                                   spillover_20_to_30 == 1 ~ "G3",
                                   spillover_30_to_40 == 1 ~ "G4",
                                   spillover_40_to_50 == 1 ~ "G5",
                                   TRUE ~ "Other"))

# --------------------------------------------------------------------------------- (2) Estimate via TWFE ----
twfe = feols(y ~ i(treated, ref = 0) | time + id, data = dta)
twfe_all = feols(y ~ i(treated, ref = 0) + i(spillover_cat, ref = "Other") | time + id, data = dta)
twfe_some = feols(y ~ i(treated, ref = 0) + i(spillover_cat, ref = "Other")  | time + id, 
                  data = dta |> 
                    mutate(spillover_cat = case_when(spillover_cat == "G4" ~ "Other",
                                                     spillover_cat == "G5" ~ "Other",
                                                     TRUE ~ spillover_cat)))


modelsummary(models = list("No Spillovers" = twfe, 
                           "Some Spillovers" = twfe_some,
                           "All Spillovers" = twfe_all),
             stars = TRUE, gof_map = c("nobs", "adj.r.squared"),
             title = "Estimated via TWFE")


# -------------------------------------------------------------------------- (3) Estimate via 2-Stage DiD ----
did2s = did2s(yname = "y", treatment = "treated", cluster_var = "id",
              first_stage = ~ 0 | time + id,
              second_stage = ~ i(treated, ref = 0),
              data = dta)

did2s_all = did2s(yname = "y", treatment = "treated", cluster_var = "id",
              first_stage = ~ 0 | time + id,
              second_stage = ~ i(treated, ref = 0) + i(spillover_cat, ref = "Other"),
              data = dta)

did2s_some = did2s(yname = "y", treatment = "treated", cluster_var = "id",
                  first_stage = ~ 0 | time + id,
                  second_stage = ~ i(treated, ref = 0) + i(spillover_cat, ref = "Other"),
                  data = dta |> 
                    mutate(spillover_cat = case_when(spillover_cat == "G4" ~ "Other",
                                                     spillover_cat == "G5" ~ "Other",
                                                     TRUE ~ spillover_cat)))

modelsummary(models = list("No Spillovers" = did2s, 
                           "Some Spillovers" = did2s_some,
                           "All Spillovers" = did2s_all),
             stars = TRUE, gof_map = c("nobs", "adj.r.squared"),
             title = "Estimated via 2-Stage DiD")
