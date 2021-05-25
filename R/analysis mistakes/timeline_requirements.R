# Script to do a timeline of admission requirements over time

rm(list = ls())

# Libraries and dirs---------------------------------------------------------------

dropbox_dir = "~/Dropbox/"
input_path = paste(dropbox_dir, 'Mistakes_in_college_admissions/',sep='')
code_path = paste(input_path, "mistakes_and_warnings/code/R", sep='')
setwd(input_path)

library(expss)
library(parallel)
library(evd)  # \342\200\230~/R/x86_64-redhat-linux-gnu-library/3.5\342\200\231 (personal library in Hawk)
library(magic)
library(mvtnorm)
library(LaplacesDemon)
library(truncnorm)
library(compiler)
library(feather)
library(magrittr)
library(geosphere)
library(Hmisc)
library(plyr)
library(dplyr)
library(tidyr)
library(fastDummies)
library(reshape2)
library(corrplot)
library(stargazer)
library(stringr)
library(ggplot2)
library(data.table)

# Plotting parameters -----------------------------------------------------

#Removing grey color background
theme_set(theme_bw())
#fill = "#1E90FF"
fill = "#B3CDE3"
line = "#1F3552"
alpha = 0.8

# Load data on admission requirements -------------------------------------

d_hist_rules = read.csv2("data/processed/historical-rules.csv", sep = ",")

#Collapse by year

rules_by_year = d_hist_rules %>% group_by(anyoproceso) %>%
  summarise(share_lm                 = mean(as.numeric(as.character(dumm_min_lm))),
            share_po                 = mean(as.numeric(as.character(dumm_min_po))),
            #share_prueba_especial    = mean(as.numeric(as.character(dumm_prueba_especial))),
            #share_pon_min_antes_pe   = mean(as.numeric(as.character(dumm_pon_min_antes_pe))),
            #share_restringe_sexo     = mean(as.numeric(as.character(dumm_restringe_sexo))),
            share_excluye_desde_pref = mean(as.numeric(as.character(dumm_excluye_desde_pref))),
            share_max_post           = mean(as.numeric(as.character(dumm_max_post))),
            share_es_pedagogia       = mean(as.numeric(as.character(dumm_es_pedagogia))))

# Plotting lines for all requirements
ggplot(data = reshape2::melt(rules_by_year,
                             id.vars = "anyoproceso",
                             measure.vars = c("share_lm",
                                              "share_po",
                                              #"share_prueba_especial",
                                              #"share_pon_min_antes_pe",
                                              #"share_restringe_sexo",
                                              "share_excluye_desde_pref",
                                              "share_max_post",
                                              "share_es_pedagogia")) %>%
                               filter(anyoproceso > 2004),
       aes(x = anyoproceso, y = value)) +
  geom_line(aes(x = anyoproceso, y = value, linetype = variable)) +
  xlab("Year") +
  ylab("Share of Programs with admission requirement") +
  scale_linetype_discrete(name="Requirement",
                    breaks = c("share_lm",
                               "share_po",
                              # "share_prueba_especial",
                              # "share_pon_min_antes_pe",
                              # "share_restringe_sexo",
                               "share_excluye_desde_pref",
                               "share_max_post",
                               "share_es_pedagogia"),
                    labels = c("Min Math-Verbal",
                               "Min Application score",
                               #"Special test",
                               #"Min Application score Special test",
                               #"Restrics gender",
                               "Restricts Rank",
                               "Restricts Number of applications",
                               "Restriction Education program")) +
  #geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/evolution_req_types_time.pdf", width=10, height=5)
