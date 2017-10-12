require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(afex)
require(lattice) # for qqmath
require(gridExtra)
library(ggthemes)
theme_set(theme_minimal())

#First pass
data_in <- read.csv("FPs_plus_ratings.csv")
data_in$P.s <- factor(data_in$P.s)
data_in$Item <- factor(data_in$Item)
drp_r <- droplevels(data_in[data_in$Item != "9" ,])
nas <- is.na(drp_r$spillover) | is.na(drp_r$Consequent)
drp_r <- droplevels(drp_r[!nas,])

drp_r <- drp_r %>%
  mutate(
    antecedent = Antecedent/1000,
    log_ant = log(antecedent),
    inv_ant = 1/antecedent,
    consequent = Consequent/1000,
    log_con = log(consequent),
    inv_con = 1/consequent,
    spillover = spillover/1000,
    log_spill = log(spillover),
    inv_spill = 1/spillover,
    acc_z = (Accepts - mean(Accepts))/sd(Accepts),
    #conf_z = (Confident- mean(Confident))/sd(Confident),
    log_acc= log(Accepts))

m_fp_con <- mixed(log_con ~ acc_z + (acc_z|P.s) + (Fit| Item), drp_r, progress = FALSE)
summary(m_fp_con)

m_fp_spill <- mixed(log_spill ~ acc_z + (acc_z|P.s) + (Fit| Item), drp_r, progress = FALSE)
summary(m_fp_spill)

#Regression path
data_in <- read.csv("RPs_plus_ratings.csv")
data_in$P.s <- factor(data_in$P.s)
data_in$Item <- factor(data_in$Item)
drp_r <- droplevels(data_in[data_in$Item != "9" ,])
nas <- is.na(drp_r$spillover) | is.na(drp_r$Consequent)
drp_r <- droplevels(drp_r[!nas,])

drp_r <- drp_r %>%
  mutate(
    antecedent = Antecedent/1000,
    log_ant = log(antecedent),
    inv_ant = 1/antecedent,
    consequent = Consequent/1000,
    log_con = log(consequent),
    inv_con = 1/consequent,
    spillover = spillover/1000,
    log_spill = log(spillover),
    inv_spill = 1/spillover,
    acc_z = (Accepts - mean(Accepts))/sd(Accepts),
    #conf_z = (Confident- mean(Confident))/sd(Confident),
    log_acc= log(Accepts))

m_rp_con <- mixed(log_con ~ acc_z + (acc_z|P.s) + (Fit| Item), drp_r, progress = FALSE)
summary(m_rp_con)

m_rp_spill <- mixed(log_spill ~ acc_z + (acc_z|P.s) + (Fit| Item), drp_r, progress = FALSE)
summary(m_rp_spill)

#Total Time
data_in <- read.csv("TTs_plus_ratings.csv")
data_in$P.s <- factor(data_in$P.s)
data_in$Item <- factor(data_in$Item)
drp_r <- droplevels(data_in[data_in$Item != "9" ,])
nas <- is.na(drp_r$spillover) | is.na(drp_r$Consequent)
drp_r <- droplevels(drp_r[!nas,])

drp_r <- drp_r %>%
  mutate(
    antecedent = Antecedent/1000,
    log_ant = log(antecedent),
    inv_ant = 1/antecedent,
    consequent = Consequent/1000,
    log_con = log(consequent),
    inv_con = 1/consequent,
    spillover = spillover/1000,
    log_spill = log(spillover),
    inv_spill = 1/spillover,
    acc_z = (Accepts - mean(Accepts))/sd(Accepts),
    #conf_z = (Confident- mean(Confident))/sd(Confident),
    log_acc= log(Accepts))

m_tt_con <- mixed(log_con ~ acc_z + (acc_z|P.s) + (Fit| Item), drp_r, progress = FALSE)
summary(m_tt_con)

m_tt_spill <- mixed(log_spill ~ acc_z + (acc_z|P.s) + (Fit| Item), drp_r, progress = FALSE)
summary(m_tt_spill)

#Regressions Out
data_in <- read.csv("RO_plus_ratings.csv")
data_in$P.s <- factor(data_in$P.s)
data_in$Item <- factor(data_in$Item)

drp_r <- droplevels(data_in[data_in$Item != "9" ,])
nas <- is.na(drp_r$spillover) | is.na(drp_r$Consequent)
drp_r <- droplevels(drp_r[!nas,])

drp_r <- drp_r %>%
  mutate(
    acc_z = (Accepts - mean(Accepts))/sd(Accepts))

m_con_ro <- mixed(Consequent ~ acc_z + (acc_z|P.s) + (Fit| Item), drp_r, family = binomial, method = "LRT", progress = FALSE)
summary(m_con_ro)

m_spill_ro <- mixed(spillover ~ acc_z + (acc_z|P.s) + (Fit| Item), drp_r, family = binomial, method = "LRT", progress = FALSE)
summary(m_spill_ro)
