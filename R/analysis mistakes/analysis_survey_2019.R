#Survey analysis
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
library(fixest)

# Plotting parameters -----------------------------------------------------

#Removing grey color background
theme_set(theme_bw())
#fill = "#1E90FF"
fill = "#B3CDE3"
line = "#1F3552"
alpha = 0.8

#Compute confidence intervals for stats
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  #datac <- plyr::rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}


# Simulating biased beliefs -----------------------------------------------

n_sim = 10000
scores = runif(n_sim, min = 680, max = 720)
cutoff = 700
sd_true = 5
sd_noise = 5
graviating_middle = 0.65
cutoff_subj = cutoff + ifelse((scores - rep(cutoff, n_sim)) > 0,  
                              (scores - rep(cutoff, n_sim))^graviating_middle,
                              -(-(scores - rep(cutoff, n_sim)))^graviating_middle)
#sd_noise = ifelse(abs(scores - rep(cutoff, n_sim)) < 5, sd_noise*4, 
#       ifelse(abs(scores - rep(cutoff, n_sim)) < 10, sd_noise*3, sd_noise))
#bias_noise = rnorm(n_sim, mean = 0, sd = sd_noise)
#true_noise = rnorm(n_sim, mean = 0, sd = sd_true)
subj_beliefs = pnorm( (scores - (cutoff_subj))/sd_noise)
ratex_beliefs = pnorm( (scores - (cutoff))/sd_true)
hist(subj_beliefs)
hist(ratex_beliefs)


##################### 2019 ###################################

# Load preprocessed data for 2019
#NOTE: the data seems to have all the potential participants of the admission process of 2019
# currently we are copy-pasting the output of that script
#dfs_2019 = readRDS("data/interim/bcd_and_survey_2019_20210325.rds")
#dfs_2019 = readRDS("data/interim/bcd_and_survey_2019_20210420.rds")
dfs_2019 = readRDS("data/interim/bcd_and_survey_2019.rds")

# Analyze first only students who answered completely the survey
dfs_2019 %<>% filter(finished == "True")

# Filter PACE students
dfs_2019 %<>% mutate(PACE = ifelse(is.na(PACE), "No PACE","PACE"))
dfs_2019 %<>% filter(PACE != "PACE")

#Check BEA
dfs_2019 %<>% mutate(BEA = ifelse(is.na(BEA), "No BEA","BEA"))
cro_cpct(dfs_2019$BEA)

# Filter students with less than 450 points who face zero admission prob everywhere
dfs_2019 %<>% filter(pmax(PROMEDIO_LM_ACTUAL, PROMEDIO_LM_ANTERIOR) >= 450)

#Check duration distribution of the entire survey
hist(dfs_2019$duration[dfs_2019$duration < 60])
sum(dfs_2019$duration < 60)
sum(dfs_2019$duration > 5)

#TODO: we need to define which students we exclude from the survey, because they take the survey in less that X minutes
#Exlusion criteria
dfs_2019 %<>% filter(duration >= 5)

# Data wrangling ----------------------------------------------------------

dfs_mistakes_2019 = dfs_2019 %>% filter(some_mistake == 1)
dfs_mistakes_2019 %<>% mutate(know_error = ifelse(know_error == "Si", "Yes", "No"))
dfs_mistakes_2019 %<>% mutate(reason_error_yes = ifelse(reason_error_yes ==
                                                        "Al no afectar el resto de mi postulación, incluí las carreras de mi preferencia aunque no cumpliera con todos los requisitos para cada una de ellas.",
                                                        "It doesn't affect my \n application and it is one \n of my preferred programs",
                                                        ifelse(reason_error_yes ==
                                                                 "Espero un cambio en alguno(s) de mis puntajes lo cual me permitiría cumplir con los requisitos de postulación.",
                                                                "I expect my scores \n to change",
                                                               ifelse(reason_error_yes == "Otra razón, explica por favor.", "Another reason",
                                                                      ifelse(reason_error_yes ==
                                                                               "Pensé que existía la posibilidad de que la postulación a dichas carreras fuera considerada, pese a que no cumplía con alguno de los  requisitos de postulación.",
                                                                             "I believed there was \n a positive probability \n of admission anyways", "None")))))
# dfs_mistakes_2019 %<>% mutate(reason_error_no_1 = ifelse(reason_error_no_1 == "Falta de acceso a la información.", "Yes", "No"))
# dfs_mistakes_2019 %<>% mutate(reason_error_no_2 = ifelse(reason_error_no_2 == "La información es poco clara.", "Yes", "No"))


# Awareness of mistakes ---------------------------------------------------

# Aware or not
ggplot(dfs_mistakes_2019, aes(x = know_error, fill = know_error)) +
  geom_bar(position="dodge", alpha = alpha, aes(y = 100*(..count..)/sum(..count..))) +
  geom_text(aes( label = round(100*(..count..)/sum(..count..),2),
                 y= 100*(..count..)/sum(..count..) ), stat= "count", vjust = +1.5) +
  xlab("Aware of error") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                     name="Awareness") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/aware_error_2019.pdf", width=10, height=5)

# Reason conditional on being aware
ggplot(dfs_mistakes_2019 %>% subset(know_error == "Yes"), aes(x = reason_error_yes, fill = fill)) +
  geom_bar(position="dodge", alpha = alpha, aes(y = 100*(..count..)/sum(..count..))) +
  geom_text(aes( label = round(100*(..count..)/sum(..count..),2),
                 y= 100*(..count..)/sum(..count..) ), stat= "count", vjust = +1.5) +
  xlab("Reason") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Reason of mistake") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.background = element_blank())
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/reason_error_aware_Yes_2019.pdf", width=10, height=5)

# Reason conditional on being aware and biased beliefs for top-reported pref with error
dfs_mistakes_2019 %>% subset((know_error == "Yes") & (error_dumm1 == 1)) %>%
                               group_by(reason_error_yes) %>%
                               summarise(mean_bias = mean(prob_first))

ggplot(dfs_mistakes_2019 %>% subset((know_error == "Yes") & (error_dumm1 == 1)),
       aes(x = reason_error_yes, fill = reason_error_yes)) +
  geom_bar(position="dodge", alpha = alpha, aes(y = 100*(..count..)/sum(..count..))) +
  geom_point(position=position_dodge(1), alpha = alpha,
             aes(y = prob_first, x = reason_error_yes, fill = reason_error_yes),
             stat = "summary") +
  geom_text(aes( label = round(100*(..count..)/sum(..count..),2),
                 y= 100*(..count..)/sum(..count..) ), stat= "count", vjust = +1.5) +
  xlab("Reason") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Reason of mistake") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/reason_error_aware_Yes_mean_bias_2019.pdf", width=10, height=5)

# Knows someone in the program --------------------------------------------

# Fill with NAs if student did not apply in a particular preference
for (p in 1:10){
  sp = paste("0",as.character(p), sep="")
  if(p==10){
    sp = "10"
  }
  col_var =  paste('error_dumm', p, sep='')
  col_pref = paste('COD_CARRERA_PREF_', sp, sep='')
  #Replace with NA the variable when the student did not apply to that program
  dfs_2019[, col_var] = ifelse(dfs_2019[, col_pref] == 0, NA, dfs_2019[, col_var])
}
#Rename some variables
setnames(dfs_2019,
         old = c('error_dumm1','error_dumm2','error_dumm3','error_dumm4','error_dumm5','error_dumm6','error_dumm7','error_dumm8','error_dumm9','error_dumm10'),
         new = c('error_dumm_1','error_dumm_2','error_dumm_3','error_dumm_4','error_dumm_5','error_dumm_6','error_dumm_7','error_dumm_8','error_dumm_9','error_dumm_10'))
#Correlation between making an admissibility mistake and knowing someone in the program
#Select only the error dummies, if the student know someone in that program, and the number of applications
dfs_error_dumm_2019 = dfs_2019 %>% select(starts_with("nw"),
                                          starts_with("error_dumm"),
                                          starts_with("COD_CARRERA_PREF_"),
                                          ID, n_apps, some_mistake)
# Reshae data to long to compute the correlation
dfs_error_dumm_2019_l <- reshape(dfs_error_dumm_2019,
                                 direction='long',
                                 varying=list(c('nw_1','nw_2','nw_3','nw_4','nw_5','nw_6','nw_7','nw_8','nw_9','nw_10'),
                                              c('error_dumm_1','error_dumm_2','error_dumm_3','error_dumm_4','error_dumm_5','error_dumm_6','error_dumm_7','error_dumm_8','error_dumm_9','error_dumm_10'),
                                              c('COD_CARRERA_PREF_01','COD_CARRERA_PREF_02','COD_CARRERA_PREF_03','COD_CARRERA_PREF_04','COD_CARRERA_PREF_05','COD_CARRERA_PREF_06','COD_CARRERA_PREF_07','COD_CARRERA_PREF_08','COD_CARRERA_PREF_09','COD_CARRERA_PREF_10')),
                                 times=1:10,
                                 v.names=c('nw', 'error_dumm', 'codigo_car'),
                                 idvar=c('ID','n_apps','some_mistake'))
dfs_error_dumm_2019_l %<>% filter(!is.na(nw))
# Compute correlation between knowing someone and making a mistake in the application
# Nothing very interesting
cro_cpct(dfs_error_dumm_2019_l$nw,dfs_error_dumm_2019_l$error_dumm)
cro_cpct((dfs_error_dumm_2019_l %>% filter(some_mistake == 1))$nw,
         (dfs_error_dumm_2019_l %>% filter(some_mistake == 1))$error_dumm)
cor((dfs_error_dumm_2019_l %>% filter(some_mistake == 1))$nw,
    (dfs_error_dumm_2019_l %>% filter(some_mistake == 1))$error_dumm)
m1 = lm(data = (dfs_error_dumm_2019_l %>% filter(some_mistake == 1)), formula = error_dumm ~ nw)
summary(m1)

# Knowledge of requirements -----------------------------------------------

#Change some names
setnames(dfs_2019,
         old = c('reqs_codigo1','reqs_codigo2'),
         new = c('reqs_codigo_1','reqs_codigo_2'))
dfs_reqs_2019 = dfs_2019 %>% select(starts_with("req"), ID, some_mistake)
# Reshape data to long
dfs_reqs_2019_l <- reshape(dfs_reqs_2019,
                          direction='long',
                          varying =list(c("req_value_ppm_1", "req_value_ppm_2"),
                                        c("req_value_plm_1", "req_value_plm_2"),
                                        c("req_value_hycs_1", "req_value_hycs_2"),
                                        c("req_value_cs_1", "req_value_cs_2"),
                                        c("req_value_pref_1", "req_value_pref_2"),
                                        c("req_value_nvac_1", "req_value_nvac_2"),
                                        c("req_knows_ppm_1", "req_knows_ppm_2"),
                                        c("req_knows_plm_1", "req_knows_plm_2"),
                                        c("req_knows_nvac_1", "req_knows_nvac_2"),
                                        c("req_knows_hycs_1", "req_knows_hycs_2"),
                                        c("req_knows_cs_1", "req_knows_cs_2"),
                                        c("req_knows_pref_1", "req_knows_pref_2"),
                                        c("reqs_codigo_1", "reqs_codigo_2")),
                          times=1:2,
                          v.names=c("req_ppm_value",
                                    "req_plm_value",
                                    "req_hycs_value",
                                    "req_cs_value",
                                    "req_pref_value",
                                    "req_nvac_value",
                                    "req_knows_ppm",
                                    "req_knows_plm",
                                    "req_knows_nvac",
                                    "req_knows_hycs",
                                    "req_knows_cs",
                                    "req_knows_pref",
                                    "reqs_codigo"),
                          idvar=c('ID','some_mistake'))
# Drop NAs
dfs_reqs_2019_l %<>% filter(!is.na(reqs_codigo))
# Merge the data to carreras requisitos
cars_2019 = read.csv2("data/raw/demre/data-giorgio/oferta2019_201812021829.csv")
dfs_reqs_2019_l =  merge(dfs_reqs_2019_l, cars_2019, by.x = "reqs_codigo", by.y = "CODIGO", all.x = TRUE)

# Create varables
table(dfs_reqs_2019_l$EXCLUYE_DESDE_PREF, useNA = "always")
dfs_reqs_2019_l %<>% mutate(req_pref_value_correct = ifelse(EXCLUYE_DESDE_PREF    == req_pref_value, 1, 0),
                            req_ppm_value_correct  = ifelse(PTJE_PONDERADO_MINIMO == req_ppm_value, 1, 0),
                            req_plm_value_correct  = ifelse(PROM_MIN_LENG_MATE    == req_plm_value, 1, 0),
                            req_hycs_value         = ifelse(req_hycs_value == "Sí", "SI",
                                                            ifelse(req_hycs_value == "No", "NO", NA)),
                            req_cs_value           = ifelse(req_cs_value == "Sí", "SI",
                                                            ifelse(req_cs_value == "No", "NO", NA)),
                            exige_hycs            = ifelse(is.na(PCT_CIEN), "SI", "NO"),
                            exige_cien             = ifelse(is.na(PCT_HCSO), "SI", "NO"),
                            req_hycs_value_correct  = ifelse(exige_hycs == req_hycs_value, 1, 0),
                            req_cs_value_correct  = ifelse(exige_cien == req_cs_value, 1, 0),
                            req_nvac_value_correct  = ifelse((VAC_1SEM + VAC_2SEM)  == req_nvac_value, 1, 0),
                            req_pref_value_dist = (EXCLUYE_DESDE_PREF    - req_pref_value),
                            req_ppm_value_dist  = (PTJE_PONDERADO_MINIMO - req_ppm_value),
                            req_plm_value_dist  = (PROM_MIN_LENG_MATE    - req_plm_value),
                            req_nvac_value_dist  = ((VAC_1SEM + VAC_2SEM)    - req_nvac_value))
# Merge with preference and error dummies
dfs_reqs_2019_l %<>% dplyr::rename(req_number = time)
dfs_error_dumm_2019_l %<>% dplyr::rename(pref = time)
dfs_reqs_2019_l = merge(dfs_reqs_2019_l, dfs_error_dumm_2019_l %>% select(!some_mistake),
                        by.x = c("ID", "reqs_codigo"), by.y = c("ID", "codigo_car"), all.x = TRUE)

#Explore requisite's types
#View(dfs_reqs_2019_l %>% select(PTJE_PONDERADO_MINIMO, req_ppm_value, req_ppm_value_correct) %>%
#       filter(!is.na(req_ppm_value)))
#NOTE: it seems that several students are confusing the PPM with the PLM with the last cutoffs
#View(dfs_reqs_2019_l %>% select(PROM_MIN_LENG_MATE, req_plm_value, req_plm_value_correct) %>%
#       filter(!is.na(req_plm_value)))
#NOTE: several students get this wrong, because they answer the number where they applied to that program, or the next one
#View(dfs_reqs_2019_l %>% select(EXCLUYE_DESDE_PREF, req_pref_value, req_pref_value_correct, pref) %>%
#       filter(!is.na(req_pref_value)))

# Histograms
#NOTE: they might be responding garbage in the question of "Excluye desde preferencia"
hist((dfs_reqs_2019_l %>% filter(!is.na(req_pref_value_correct)))$req_pref_value_dist)
hist((dfs_reqs_2019_l %>% filter(!is.na(req_pref_value_correct)))$req_pref_value)
hist((dfs_reqs_2019_l %>% filter(!is.na(req_pref_value_correct)))$EXCLUYE_DESDE_PREF)
hist((dfs_reqs_2019_l %>% filter(!is.na(req_ppm_value_correct)))$req_ppm_value_dist)
hist((dfs_reqs_2019_l %>% filter(!is.na(req_ppm_value_correct) & PTJE_PONDERADO_MINIMO > 0))$req_ppm_value_dist)
hist((dfs_reqs_2019_l %>% filter(!is.na(req_plm_value_correct)))$req_plm_value_dist)
hist((dfs_reqs_2019_l %>% filter(!is.na(req_nvac_value_correct)))$req_nvac_value_dist)

summarySE(data = dfs_reqs_2019_l,
          measurevar = "req_knows_pref",
          groupvars="error_dumm", na.rm=TRUE)

summarySE(data = dfs_reqs_2019_l,
          measurevar = "req_knows_ppm",
          groupvars="error_dumm", na.rm=TRUE)

summarySE(data = dfs_reqs_2019_l,
          measurevar = "req_knows_plm",
          groupvars="error_dumm", na.rm=TRUE)

summarySE(data = dfs_reqs_2019_l,
          measurevar = "req_knows_nvac",
          groupvars="error_dumm", na.rm=TRUE)

summarySE(data = dfs_reqs_2019_l,
          measurevar = "req_knows_cs",
          groupvars="error_dumm", na.rm=TRUE)

summarySE(data = dfs_reqs_2019_l,
          measurevar = "req_knows_hycs",
          groupvars="error_dumm", na.rm=TRUE)

# PLots
# Knows correctly the preference exclusion
dfs_reqs_2019_l %<>% mutate(error_dumm = ifelse(error_dumm == 1, "Yes",
                                                ifelse(error_dumm == 0, "No", NA)))
# Plots
ggplot(data = summarySE(data = dfs_reqs_2019_l %>% filter(!is.na(req_pref_value_correct)),
                 measurevar = "req_pref_value_correct",
                 groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                 y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_correct_pref_req_2019.pdf", width=5, height=5)

ggplot(data = summarySE(data = dfs_reqs_2019_l %>% filter(!is.na(EXCLUYE_DESDE_PREF)),
                 measurevar = "req_knows_pref",
                 groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                 y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_pref_req_2019.pdf", width=5, height=5)

# Knows correctly the puntaje minimo ponderado
ggplot(data = summarySE(data = dfs_reqs_2019_l %>% filter(!is.na(req_ppm_value_correct)  & PTJE_PONDERADO_MINIMO > 0),
                        measurevar = "req_ppm_value_correct",
                        groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_correct_ppm_req_2019.pdf", width=5, height=5)# Knows correctly the puntaje minimo ponderado

ggplot(data = summarySE(data = dfs_reqs_2019_l %>% filter(PTJE_PONDERADO_MINIMO > 0),
                        measurevar = "req_knows_ppm",
                        groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_ppm_req_2019.pdf", width=5, height=5)# Knows correctly the puntaje minimo ponderado

# Knows correctly the puntaje promedio lenguage matematica
ggplot(data = summarySE(data = dfs_reqs_2019_l %>% filter(!is.na(req_plm_value_correct)),
                        measurevar = "req_plm_value_correct",
                        groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_correct_plm_req_2019.pdf", width=5, height=5)

ggplot(data = summarySE(data = dfs_reqs_2019_l,
                        measurevar = "req_knows_plm",
                        groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_plm_req_2019.pdf", width=5, height=5)

# Knows correctly the requires history or social sciences
ggplot(data = summarySE(data = dfs_reqs_2019_l %>% filter(!is.na(req_hycs_value_correct)),
                        measurevar = "req_hycs_value_correct",
                        groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_correct_hycs_req_2019.pdf", width=5, height=5)

ggplot(data = summarySE(data = dfs_reqs_2019_l %>% filter(exige_hycs == "SI"),
                        measurevar = "req_knows_hycs",
                        groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_hycs_req_2019.pdf", width=5, height=5)

# Knows correctly the exige ciencias
ggplot(data = summarySE(data = dfs_reqs_2019_l %>% filter(!is.na(req_cs_value_correct)),
                        measurevar = "req_cs_value_correct",
                        groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_correct_cs_req_2019.pdf", width=5, height=5)

ggplot(data = summarySE(data = dfs_reqs_2019_l %>% filter(exige_cien == "SI"),
                        measurevar = "req_knows_cs",
                        groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_cs_req_2019.pdf", width=5, height=5)

# Knows correctly the number of vacancies
ggplot(data = summarySE(data = dfs_reqs_2019_l %>% filter(!is.na(req_nvac_value_correct)),
                        measurevar = "req_nvac_value_correct",
                        groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_correct_nvac_req_2019.pdf", width=5, height=5)

ggplot(data = summarySE(data = dfs_reqs_2019_l,
                        measurevar = "req_knows_nvac",
                        groupvars="error_dumm", na.rm=TRUE),
       aes(x = as.factor(error_dumm), fill = as.factor(error_dumm))) +
  geom_bar(position="dodge", alpha = alpha, stat = "identity", aes(y = round(100*mean,2))) +
  geom_text(aes(label = round(100*mean,2),
                y= round(100*mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Admissibility mistake") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=100*(mean-ci), ymax=100*(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knows_nvac_req_2019.pdf", width=5, height=5)


#TODO: Run a regression predicting the correct knowledge (or knowledge) for some requirements by the preference
# of assignment, with program fixed effect, student fixed effect (if possible) and rank in the application list


# Characterizing reported preferences and beliefs ----------------------------------

#Create application types
dfs_2019 %<>% mutate(short_list = ifelse(n_apps < 10, 1, 0))
dfs_2019 %<>% mutate(short_list_lab = ifelse(n_apps < 10, "Short-list", "Full-list"))

# Truthteller: Most preferred is top-reported preference
dfs_2019 %<>% mutate(truth_teller = ifelse(mp_carr == COD_CARRERA_PREF_01, 1, 0))

# Misreporting excluding: Most preferred is not in the application list
dfs_2019 %<>% mutate(misreport_exclude = ifelse(applied_to_mp == 0, 1, 0))

# Misreporting ordering: Most preferred is in the application list but not in the first choice
dfs_2019 %<>% mutate(misreport_order = ifelse((applied_to_mp == 1) & (truth_teller != 1), 1, 0))

# In which reported preference the student included the mp carr?
dfs_2019 %<>% mutate(pref_mp = ifelse(applied_to_mp == 0, 0,
                                            ifelse(COD_CARRERA_PREF_01 == mp_carr, 1,
                                                   ifelse(COD_CARRERA_PREF_02 == mp_carr, 2,
                                                          ifelse(COD_CARRERA_PREF_03 == mp_carr, 3,
                                                                 ifelse(COD_CARRERA_PREF_04 == mp_carr, 4,
                                                                        ifelse(COD_CARRERA_PREF_05 == mp_carr, 5,
                                                                               ifelse(COD_CARRERA_PREF_06 == mp_carr, 6,
                                                                                      ifelse(COD_CARRERA_PREF_07 == mp_carr, 7,
                                                                                             ifelse(COD_CARRERA_PREF_08 == mp_carr, 8,
                                                                                                    ifelse(COD_CARRERA_PREF_09 == mp_carr, 9,
                                                                                                           ifelse(COD_CARRERA_PREF_10 == mp_carr, 10, NA))))))))))))


# Most preferred is program of assignment
dfs_2019 %<>% mutate(assigned_to_mp = ifelse(assigned_to == mp_carr, 1, 0))
dfs_2019 %<>% mutate(assigned_above_mp = ifelse((assigned_pref < pref_mp) &
                                                        (assigned_pref != 0) &
                                                        (pref_mp != 0), 1, 0))
#NOTE: filter to have only students with the version of the paid program question
dfs_2019_money = dfs_2019 %>% filter((rand_money == 1))
#NOTE: there are students who declare as an ideal program a reported preference below the top one
table(dfs_2019_money$pref_mp, useNA = "always")
table(dfs_2019_money$pref_mp, dfs_2019_money$assigned_to_mp)
#NOTE: from the fraction of students who misreport the order, half of them gets assigned to a higher ranked pref
# and half gets assigned to a lower-ranked pref
table(dfs_2019_money$assigned_above_mp, dfs_2019_money$misreport_order)
#NOTE: we need the survey of 2020 to dig deeper in why students missreport

#Checks
#View(dfs_2019_money %>%
#       select(mp_carr, COD_CARRERA_PREF_01,  applied_to_mp, assigned_to,
#              truth_teller, misreport_exclude, misreport_order))

# Create categorical variable
dfs_2019 %<>% mutate(application_type = ifelse(truth_teller == 1, "Truth-teller",
                                                     ifelse(misreport_exclude == 1, "Misreporting Exclusion",
                                                            ifelse(misreport_order == 1, "Misreporting Ordering", NA))))
# Application types (paid)
ggplot(dfs_2019 %>% filter(!is.na(application_type) & (rand_money == 1)),
       aes(x = short_list_lab, fill = application_type)) +
  geom_bar(position = position_dodge(width = 0.9),
           alpha = alpha,
           aes(y = 100*(..count..)/tapply(..count.., ..x.. ,sum)[..x..])) +
  geom_text(position = position_dodge(width = 0.9),
            aes(group = application_type,
                label = round(100*(..count..)/tapply(..count.., ..x.. ,sum)[..x..],2),
                y= 100*(..count..)/tapply(..count.., ..x.. ,sum)[..x..]),
            stat= "count",
            vjust = +1.5) +
  xlab("Application type") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Types") +
  #facet_wrap(~as.factor(short_list)) +
  #theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/application_types_2019.pdf", width=10, height=5)

# Application types (free)
#NOTE: there are no big differences in the aggregate level of application types by whether they got the free
# or the paid version of the question
ggplot(dfs_2019 %>% filter(!is.na(application_type) & (rand_money == 0)),
       aes(x = short_list_lab, fill = application_type)) +
  geom_bar(position = position_dodge(width = 0.9),
           alpha = alpha,
           aes(y = 100*(..count..)/tapply(..count.., ..x.. ,sum)[..x..])) +
  geom_text(position = position_dodge(width = 0.9),
            aes(group = application_type,
                label = round(100*(..count..)/tapply(..count.., ..x.. ,sum)[..x..],2),
                y= 100*(..count..)/tapply(..count.., ..x.. ,sum)[..x..]),
            stat= "count",
            vjust = +1.5) +
  xlab("Application type") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Types") +
  #facet_wrap(~as.factor(short_list)) +
  #theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/application_types_free_2019.pdf", width=10, height=5)



# Beliefs -----------------------------------------------------------------

# Admissibility mistakes and beliefs or top-reported preference
#View(dfs_2019 %>% select(prob_first, prob_1, prob_err_1))


# Histogram for admission probability conditional on making an admissibility mistake
dfs_2019 %<>% mutate(bias_top_pref_mistake = ifelse(error_dumm_1 == 1, prob_first, NA))
# Consistency in probabilities of admission for students with a valid top preference
dfs_2019 %<>% mutate(inconsistency_probs_top_choice = ifelse(error_dumm_1 == 0, prob_first - prob_1, NA))


ggplot(dfs_2019 %>% filter(error_dumm_1 == 1),
       aes(x = bias_top_pref_mistake)) +
  geom_density()

# Long version to get all subjective beliefs
# Reshape data to long
dfs_beliefs_2019_w = dfs_2019 %>% select(
  starts_with("prob_"),
  starts_with("scores_"),
  starts_with("error_dumm"),
  starts_with("COD_CARRERA_PREF_"),
  starts_with("nw_"),
  n_apps, ID, some_mistake) %>% select(!prob_first)

dfs_beliefs_2019_l = reshape(dfs_beliefs_2019_w,
                             direction='long',
                             varying=list(
                               c('prob_1','prob_2','prob_3','prob_4','prob_5','prob_6','prob_7','prob_8','prob_9','prob_10'),
                               c('prob_err_1','prob_err_2','prob_err_3','prob_err_4','prob_err_5','prob_err_6','prob_err_7','prob_err_8','prob_err_9','prob_err_10'),
                               c('scores_carr1','scores_carr2','scores_carr3','scores_carr4','scores_carr5','scores_carr6','scores_carr7','scores_carr8','scores_carr9','scores_carr10'),
                               c('error_dumm_1','error_dumm_2','error_dumm_3','error_dumm_4','error_dumm_5','error_dumm_6','error_dumm_7','error_dumm_8','error_dumm_9','error_dumm_10'),
                               c('COD_CARRERA_PREF_01','COD_CARRERA_PREF_02','COD_CARRERA_PREF_03','COD_CARRERA_PREF_04','COD_CARRERA_PREF_05','COD_CARRERA_PREF_06','COD_CARRERA_PREF_07','COD_CARRERA_PREF_08','COD_CARRERA_PREF_09','COD_CARRERA_PREF_10'),
                               c('nw_1','nw_2','nw_3','nw_4','nw_5','nw_6','nw_7','nw_8','nw_9','nw_10')
                             ),
                             times=1:10,
                             v.names=c(
                               'prob',
                               'prob_err',
                               'scores_carr',
                               'error_dumm',
                               'codigo_car',
                               'nw'),
                             idvar=c('ID','n_apps','some_mistake'))

dfs_beliefs_2019_l %<>% dplyr::rename(pref = time)
# Drop extra rows for programs where the student didn't apply to
dfs_beliefs_2019_l %<>% filter(codigo_car != 0)

# PDF for subjective beliefs by preference of assignment
ggplot(dfs_beliefs_2019_l %>% filter(error_dumm == 0 & pref %in% c(1,4,8)),
       aes(x = prob, colour = as.factor(pref))) +
  stat_ecdf()

# Densities for subjective beliefs by preference of assignment
ggplot(dfs_beliefs_2019_l %>% filter(error_dumm == 0 & pref %in% c(1,4)),
       aes(x = prob, fill = as.factor(pref))) +
  geom_density(alpha = alpha) +
  xlab("Subjective beliefs") +
  ylab("Density") +
  scale_fill_brewer(palette="Pastel1",
                    name="Preference") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/densities_subj_beliefs_2019.pdf", width=7, height=5)


table_beliefs = dfs_beliefs_2019_l %>% filter(error_dumm == 0) %>%
  group_by(pref, n_apps) %>%
  summarise(mean_belief = mean(prob))

#Create a matrix to represent this info
beliefs_mean_mat = matrix(NA, nrow = 10, ncol = 10 )
for(i in 1:10){
  for(j in 1:10){
    if(j >= i){
      beliefs_mean_mat[i,j] = (table_beliefs %>% subset(pref == i & n_apps == j))$mean_belief
    }
  }
}
# Show subjective mean beliefs of assignment probability by preference of assignment and application length
library("lattice")
library("see")
colnames(beliefs_mean_mat) <- paste( rep("L",10) , c(1:10) , sep="")
rownames(beliefs_mean_mat) <- paste( rep("P",10) , c(1:10) , sep="")
#TODO: Fix the saving
pdf(file="mistakes_and_warnings/code/R/analysis mistakes/plots/heatmap_sb_beliefs_2019.pdf", width=7, height=5)
p = levelplot(t(beliefs_mean_mat), xlab = "Application length", ylab = "Preference",
              at = 40:90)
p
dev.off()


table_scores = dfs_beliefs_2019_l %>% filter(error_dumm == 0) %>%
  group_by(n_apps) %>%
  summarise(mean_scores = mean(scores_carr))

# Average score by application length
ggplot(data = summarySE(data = dfs_beliefs_2019_l %>% filter(error_dumm == 0),
                        measurevar = "scores_carr",
                        groupvars="n_apps", na.rm=TRUE),
       aes(x = as.factor(n_apps))) +
  geom_point(position="dodge", alpha = alpha, stat = "identity", aes(y = round(mean,2))) +
  geom_text(aes(label = round(mean,2),
                y= round(mean,2)), stat= "identity", vjust = -2.5) +
  xlab("Application Length") +
  ylab("Average application score") +
  scale_fill_brewer(palette="Pastel1",
                    name="Mistake") +
  geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
  #ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/averge_score_by_app_length_2019.pdf", width=5, height=5)


# Distribution of scores by application length
dfs_beliefs_2019_l %<>% mutate(scores_carr_standard = (scores_carr - mean(scores_carr))/sd(scores_carr))

ggplot(data = dfs_beliefs_2019_l %>% filter(error_dumm == 0),
       aes(x = as.factor(n_apps), y = scores_carr_standard, fill = as.factor(n_apps))) +
  geom_violinhalf(alpha = alpha, trim=FALSE, orientation = "x") +
  geom_boxplot(width=0.1, fill="white")+
  xlab("Application Length") +
  ylab("Average application score (standardized)") +
  scale_fill_brewer(palette="Pastel1",
                    name="Application Length") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/violin_score_by_app_length_2019.pdf", width=7, height=5)

# Checks
ggplot(dfs_2019 %>% filter(error_dumm_1 == 0),
       aes(x = inconsistency_probs_top_choice, fill = BEA)) +
  geom_density(alpha = alpha)

table((dfs_2019 %>% filter(error_dumm_1 == 0))$inconsistency_probs_top_choice)

sum((dfs_2019 %>% filter(error_dumm_1 == 0 & BEA == "No BEA"))$inconsistency_probs_top_choice >= -10 &
      (dfs_2019 %>% filter(error_dumm_1 == 0 & BEA == "No BEA"))$inconsistency_probs_top_choice <= 10)/nrow((dfs_2019 %>% filter(error_dumm_1 == 0 & BEA == "No BEA")))

# Ratex beliefs and bias --------------------------------------------------

#Loading Ratex beliefs by application lists
#ratex_2018 = read.csv2(file = "data/interim/ratexs-2018.csv", sep=",", header = TRUE)
ratex_2019 = read.csv2(file = "data/interim/ratexs-2019.csv", sep=",", header = TRUE)
#ratex_mp_2018 = read.csv2(file = "data/interim/mp-ratexs-2018.csv", sep=",", header = TRUE)
ratex_mp_2019 = read.csv2(file = "data/interim/mp-ratexs-2019.csv", sep=",", header = TRUE)

#Merge ratex to the survey panel long with subjective beliefs
dfs_beliefs_2019_l = merge(dfs_beliefs_2019_l, ratex_2019,
                           by.x = c("ID","pref"), by.y = c("id","rank"), all.x = TRUE)
dfs_beliefs_2019_l %<>% mutate(ratex = 100*as.numeric(as.character(ratex_2019)),
                               adaptive = 100*as.numeric(as.character(ratex_2018)))
#dfs_beliefs_2019_l %<>% mutate(ratex = 100*as.numeric(as.character(ratex)))


# Densities for subjective beliefs by preference of assignment
ggplot(dfs_beliefs_2019_l %>% filter(error_dumm == 0 & pref %in% c(1,4)),
       aes(x = ratex, fill = as.factor(pref))) +
  geom_density(alpha = alpha) +
  xlab("Ratex beliefs") +
  ylab("Density") +
  scale_fill_brewer(palette="Pastel1",
                    name="Preference") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/densities_ratex_2019.pdf", width=7, height=5)

# Create heatmap
table_ratex = dfs_beliefs_2019_l %>% filter(error_dumm == 0) %>%
  group_by(pref, n_apps) %>%
  summarise(mean_belief = mean(ratex))
#Create a matrix to represent this info
ratex_mean_mat = matrix(NA, nrow = 10, ncol = 10 )
for(i in 1:10){
  for(j in 1:10){
    if(j >= i){
      ratex_mean_mat[i,j] = (table_ratex %>% subset(pref == i & n_apps == j))$mean_belief
    }
  }
}
# Show subjective mean beliefs of assignment probability by preference of assignment and application length
colnames(ratex_mean_mat) <- paste( rep("L",10) , c(1:10) , sep="")
rownames(ratex_mean_mat) <- paste( rep("P",10) , c(1:10) , sep="")
#TODO: Fix the saving
pdf(file="mistakes_and_warnings/code/R/analysis mistakes/plots/heatmap_ratex_2019.pdf", width=7, height=5)
p = levelplot(t(ratex_mean_mat), xlab = "Application length", ylab = "Preference",
              at = 40:90)
p
dev.off()

# Computing bias and distribution of bias
dfs_beliefs_2019_l %<>% mutate(bias_ratex = prob - ratex)

# Densities for bias in beliefs
ggplot(dfs_beliefs_2019_l %>% filter(error_dumm == 0),
       aes(x = bias_ratex)) +
  geom_density(alpha = alpha, fill = fill) +
  xlab("Bias in beliefs") +
  ylab("Density") +
  scale_fill_brewer(palette="Pastel1",
                    name="Distribution") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/density_bias_ratex_2019.pdf", width=7, height=5)


# Densities for bias in beliefs
ggplot(dfs_beliefs_2019_l %>% filter(error_dumm == 0 & (ratex >= 1.00 & ratex <= 99.0)),
       aes(x = bias_ratex)) +
  geom_density(alpha = alpha, fill = fill) +
  xlab("Bias in beliefs") +
  ylab("Density") +
  scale_fill_brewer(palette="Pastel1",
                    name="Distribution") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/density_bias_ratex_interior_2019.pdf", width=7, height=5)

# Create heatmap
table_bias_ratex = dfs_beliefs_2019_l %>% filter(error_dumm == 0) %>%
  group_by(pref, n_apps) %>%
  summarise(mean_bias_ratex = mean(bias_ratex))
#Create a matrix to represent this info
bias_ratex_mean_mat = matrix(NA, nrow = 10, ncol = 10 )
for(i in 1:10){
  for(j in 1:10){
    if(j >= i){
      bias_ratex_mean_mat[i,j] = (table_bias_ratex %>% subset(pref == i & n_apps == j))$mean_bias_ratex
    }
  }
}
# Show subjective mean beliefs of assignment probability by preference of assignment and application length
colnames(bias_ratex_mean_mat) <- paste( rep("L",10) , c(1:10) , sep="")
rownames(bias_ratex_mean_mat) <- paste( rep("P",10) , c(1:10) , sep="")
pdf(file="mistakes_and_warnings/code/R/analysis mistakes/plots/heatmap_bias_ratex_2019.pdf", width=7, height=5)
p = levelplot(t(bias_ratex_mean_mat), xlab = "Application length", ylab = "Preference")
p
dev.off()

# Create heatmap
table_bias_ratex = dfs_beliefs_2019_l %>% filter(error_dumm == 0) %>%
  group_by(pref, n_apps) %>%
  summarise(sd_bias_ratex = sd(bias_ratex))
#Create a matrix to represent this info
bias_ratex_sd_mat = matrix(NA, nrow = 10, ncol = 10 )
for(i in 1:10){
  for(j in 1:10){
    if(j >= i){
      bias_ratex_sd_mat[i,j] = (table_bias_ratex %>% subset(pref == i & n_apps == j))$sd_bias_ratex
    }
  }
}
# Show subjective mean beliefs of assignment probability by preference of assignment and application length
colnames(bias_ratex_sd_mat) <- paste( rep("L",10) , c(1:10) , sep="")
rownames(bias_ratex_sd_mat) <- paste( rep("P",10) , c(1:10) , sep="")
pdf(file="mistakes_and_warnings/code/R/analysis mistakes/plots/heatmap_bias_sd_ratex_2019.pdf", width=7, height=5)
p = levelplot(t(bias_ratex_sd_mat), xlab = "Application length", ylab = "Preference")
p
dev.off()


#Computing the risk of being unassigned (assuming independence on beliefs)
Risk_assignment = function(Probs){
  risk = 1
  for(j in 1:10){
    if(!is.na(Probs[j])){
      risk = risk * (1-Probs[j])
    }
  }
  return(risk)
}

table_risk = dfs_beliefs_2019_l %>% filter(error_dumm == 0) %>%
  group_by(ID) %>%
  summarise(risk_ratex = Risk_assignment(ratex/100),
            risk_prob = Risk_assignment(prob/100))

hist(table_risk$risk_ratex[table_risk$risk_ratex > 0.01])
hist(table_risk$risk_prob[table_risk$risk_ratex > 0.01])

# Count number of students who underpredict their risk and do face a positive risk of being unassigned
table_risk %>% summarise(under_predicts_risk_mean = 100*mean((risk_ratex > 0.7) & (risk_ratex > risk_prob)),
                         under_predicts_risk_stderr = 100*sd((risk_ratex > 0.7) & (risk_ratex > risk_prob))/sqrt(n()),
                         faces_positive_ratex_risk = 100*mean((risk_ratex > 0.7)))
table_risk %>% summarise(under_predicts_risk_mean = 100*mean((risk_ratex > 0.7) & (0.1 > risk_prob)),
                         under_predicts_risk_stderr = 100*sd((risk_ratex > 0.7) & (0.1 > risk_prob))/sqrt(n()),
                         faces_positive_ratex_risk = 100*mean((risk_ratex > 0.7)))

# Reshape for plot
table_risk_l = reshape(as.data.frame(table_risk %>% filter(risk_ratex >= 0.01)),
                       direction='long',
                       varying=list(c('risk_ratex','risk_prob')),
                       times=c("Ratex", "Subjective"),
                       v.names=c('risk'),
                       idvar=c('ID'))

table_risk_l %<>% dplyr::rename(belief_type = time)
# Difference in risk of assignment (Ratex versus subjective beliefs)
ggplot(table_risk_l) +
  geom_histogram(position = "dodge", alpha = alpha, aes(x = risk, fill = belief_type), bins = 10) +
  xlab("Risk") +
  ylab("Count") +
  scale_fill_brewer(palette="Pastel1",
                    name="Beliefs' type") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/histo_risk_2019.pdf", width=7, height=5)

# Scatter plot of Subjective vs Ratex risk
table_risk %<>% mutate(subj_risk_cat = cut(risk_prob, breaks = seq(from = 0.0, to = 1.0, by = 0.1), include.lowest = TRUE))

ggplot(table_risk %>% filter((risk_ratex >= 0.01))) +
  geom_boxplot(position = "dodge", alpha = alpha, aes(y = risk_ratex, x = subj_risk_cat, fill = fill)) +
  xlab("Risk Subjective") +
  ylab("Risk Ratex") +
  scale_fill_brewer(palette="Pastel1",
                    name="Subjective Risk") +
  theme_bw() +
  #geom_abline(intercept = -0.04, slope = 0.1) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "none")
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/scatter_risk_2019.pdf", width=5, height=5)



# Beliefs over Expected cutoffs -------------------------------------------

#Knowledge of cutoffs
dfs_2019 %<>% mutate(know_cutoff = ifelse(know_cutoff == "Conozco el puntaje de corte del año pasado para cada una de las carreras a las que postulé.",
                                          "Knows the cutoffs for \n every program in the list",
                                          ifelse(know_cutoff == "Conozco el puntaje de corte del año pasado sólo para algunas de las carreras a las que postulé.",
                                                 "Knows the cutoffs for \n some but not all programs \n in the list",
                                                 ifelse(know_cutoff == "No conozco el puntaje de corte del año pasado para ninguna de las carreras a las que postulé.",
                                                        "Does not know the cutoffs for \n the program in the list", NA))))
table(dfs_2019$know_cutoff, useNA = "always")

# Knowledge of cutoff scores
ggplot(dfs_2019, aes(x = know_cutoff, fill = fill)) +
  geom_bar(position="dodge", alpha = alpha, aes(y = 100*(..count..)/sum(..count..))) +
  geom_text(aes( label = round(100*(..count..)/sum(..count..),2),
                 y= 100*(..count..)/sum(..count..) ), stat= "count", vjust = +1.5) +
  xlab("Knowledge of cutoffs") +
  ylab("Percentage") +
  scale_fill_brewer(palette="Pastel1",
                    name="Level of knowledge") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.background = element_blank())
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/knowledge_cutoffs_2019.pdf", width=10, height=5)

# Computing the difference between realized and expected cutoffs
dfs_cutoffs_2019_w = dfs_2019 %>% select(
  starts_with("prob_"),
  starts_with("scores_"),
  starts_with("error_dumm"),
  starts_with("COD_CARRERA_PREF_"),
  starts_with("cutoff_pref_"),
  starts_with("cutoff_"),
  n_apps, ID, some_mistake,know_cutoff)

dfs_cutoffs_2019_l = reshape(dfs_cutoffs_2019_w,
                             direction='long',
                             varying=list(
                               c('prob_1','prob_2','prob_3','prob_4','prob_5','prob_6','prob_7','prob_8','prob_9','prob_10'),
                               c('prob_err_1','prob_err_2','prob_err_3','prob_err_4','prob_err_5','prob_err_6','prob_err_7','prob_err_8','prob_err_9','prob_err_10'),
                               c('scores_carr1','scores_carr2','scores_carr3','scores_carr4','scores_carr5','scores_carr6','scores_carr7','scores_carr8','scores_carr9','scores_carr10'),
                               c('error_dumm_1','error_dumm_2','error_dumm_3','error_dumm_4','error_dumm_5','error_dumm_6','error_dumm_7','error_dumm_8','error_dumm_9','error_dumm_10'),
                               c('COD_CARRERA_PREF_01','COD_CARRERA_PREF_02','COD_CARRERA_PREF_03','COD_CARRERA_PREF_04','COD_CARRERA_PREF_05','COD_CARRERA_PREF_06','COD_CARRERA_PREF_07','COD_CARRERA_PREF_08','COD_CARRERA_PREF_09','COD_CARRERA_PREF_10'),
                               c('cutoff_pref_01','cutoff_pref_02','cutoff_pref_03','cutoff_pref_04','cutoff_pref_05','cutoff_pref_06','cutoff_pref_07','cutoff_pref_08','cutoff_pref_09','cutoff_pref_10'),
                               c('cutoff_1','cutoff_2','cutoff_3','cutoff_4','cutoff_5','cutoff_6','cutoff_7','cutoff_8','cutoff_9','cutoff_10')),
                             times=1:10,
                             v.names=c(
                               'prob',
                               'prob_err',
                               'scores_carr',
                               'error_dumm',
                               'codigo_car',
                               'cutoff_pref',
                               'cutoff'),
                             idvar=c('ID','n_apps','some_mistake', 'know_cutoff'))

dfs_cutoffs_2019_l %<>% dplyr::rename(pref = time)
dfs_cutoffs_2019_l %<>% filter(codigo_car != 0)

# Distance in expected cutoff
dfs_cutoffs_2019_l %<>% mutate(distance_subj_real_cutoffs = ifelse(cutoff_pref > 0,
                                                                   (cutoff - cutoff_pref)/sd(scores_carr), NA))
sum(is.na(dfs_cutoffs_2019_l$distance_subj_real_cutoffs))
#Check Historgrams
hist(dfs_cutoffs_2019_l$cutoff)
hist(dfs_cutoffs_2019_l$cutoff_pref)
hist((dfs_cutoffs_2019_l %>% subset(pref == 1))$distance_subj_real_cutoffs)
hist((dfs_cutoffs_2019_l %>% subset(pref == 2))$distance_subj_real_cutoffs)

# Distribution of bias in expected cutoff scores by knowledge of cutoff
ggplot(data = dfs_cutoffs_2019_l %>% filter((distance_subj_real_cutoffs <= 200) &
                                              (distance_subj_real_cutoffs >= -200)),
       aes(x = distance_subj_real_cutoffs, fill = know_cutoff)) +
  geom_density(alpha = alpha) +
  #geom_boxplot(width=0.1, fill="white")+
  ylab("Density") +
  xlab("Bias in expected cutoff score (Subjective - Ratex)") +
  scale_fill_brewer(palette="Pastel1",
                    name="Knowledge of cutoffs") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/bias_exp_cutoff_by_knowledge_2019.pdf", width=7, height=5)

# Distribution of bias in expected cutoff scores by preference
ggplot(data = dfs_cutoffs_2019_l %>% filter((distance_subj_real_cutoffs <= 200) &
                                              (distance_subj_real_cutoffs >= -200) & (pref %in% c(1,4,8))),
       aes(x = distance_subj_real_cutoffs, fill = as.factor(pref))) +
  geom_density(alpha = alpha) +
  #geom_boxplot(width=0.1, fill="white")+
  ylab("Density") +
  xlab("Bias in expected cutoff score (Subjective - Ratex)") +
  scale_fill_brewer(palette="Pastel1",
                    name="Preference") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# Distribution of bias in expected cutoffs by preference
ggplot(data = dfs_cutoffs_2019_l %>% filter((distance_subj_real_cutoffs <= 0.5) &
                                              (distance_subj_real_cutoffs >= -0.5)),
       aes(x = as.factor(pref), y = distance_subj_real_cutoffs, fill = as.factor(pref))) +
  geom_violinhalf(alpha = alpha, trim=FALSE, orientation = "x") +
  geom_boxplot(width=0.1, fill="white",coef=10)+
  xlab("Preference") +
  ylab("Bias in standardzed expected cutoff score (Subjective - Ratex)") +
  scale_fill_brewer(palette="Pastel1",
                    name="Preference") +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/violin_bias_exp_cutoff_by_pref_2019.pdf", width=7, height=5)


# Distribution of bias in expected cutoffs for top-reported pref by application length
ggplot(data = dfs_cutoffs_2019_l %>% filter((distance_subj_real_cutoffs <= 0.5) &
                                              (distance_subj_real_cutoffs >= -0.5)) %>%
         subset(pref == 1),
       aes(x = as.factor(n_apps), y = distance_subj_real_cutoffs, fill = as.factor(n_apps))) +
  geom_violinhalf(alpha = alpha, trim=FALSE, orientation = "x") +
  geom_boxplot(width=0.1, fill="white",coef=10)+
  xlab("Application length") +
  ylab("Bias in standardzed expected cutoff score (Subjective - Ratex)") +
  scale_fill_brewer(palette="Pastel1",
                    name="Application length") +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/violin_bias_exp_cutoff_by_app_length_pref_1_2019.pdf", width=7, height=5)

# Distribution of bias in expected cutoffs for reported pref by application length
ggplot(data = dfs_cutoffs_2019_l %>% filter((distance_subj_real_cutoffs <= 0.5) &
                                              (distance_subj_real_cutoffs >= -0.5)) %>%
         subset(pref == 3),
       aes(x = as.factor(n_apps), y = distance_subj_real_cutoffs, fill = as.factor(n_apps))) +
  geom_violinhalf(alpha = alpha, trim=FALSE, orientation = "x") +
  geom_boxplot(width=0.1, fill="white",coef=10)+
  xlab("Application length") +
  ylab("Bias in standardzed expected cutoff score (Subjective - Ratex)") +
  scale_fill_brewer(palette="Pastel1",
                    name="Application length") +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/violin_bias_exp_cutoff_by_app_length_pref_3_2019.pdf", width=7, height=5)

# Distribution of bias in expected cutoffs for reported pref by application length
ggplot(data = dfs_cutoffs_2019_l %>% filter((distance_subj_real_cutoffs <= 0.5) &
                                              (distance_subj_real_cutoffs >= -0.5)) %>%
         subset(pref == 5),
       aes(x = as.factor(n_apps), y = distance_subj_real_cutoffs, fill = as.factor(n_apps))) +
  geom_violinhalf(alpha = alpha, trim=FALSE, orientation = "x") +
  geom_boxplot(width=0.1, fill="white",coef=10)+
  xlab("Application length") +
  ylab("Bias in standardzed expected cutoff score (Subjective - Ratex)") +
  scale_fill_brewer(palette="Pastel1",
                    name="Application length") +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/violin_bias_exp_cutoff_by_app_length_pref_5_2019.pdf", width=7, height=5)


# Create heatmap
table_bias_cutoffs = dfs_cutoffs_2019_l %>%
  group_by(pref, n_apps) %>%
  summarise(mean_bias_cutoffs = mean(distance_subj_real_cutoffs, na.rm = TRUE))
#Create a matrix to represent this info
bias_cutoffs_mean_mat = matrix(NA, nrow = 10, ncol = 10 )
for(i in 1:10){
  for(j in 1:10){
    if(j >= i){
      bias_cutoffs_mean_mat[i,j] = (table_bias_cutoffs %>% subset(pref == i & n_apps == j))$mean_bias_cutoffs
    }
  }
}
# Show subjective mean beliefs of assignment probability by preference of assignment and application length
colnames(bias_cutoffs_mean_mat) <- paste( rep("L",10) , c(1:10) , sep="")
rownames(bias_cutoffs_mean_mat) <- paste( rep("P",10) , c(1:10) , sep="")
pdf(file="mistakes_and_warnings/code/R/analysis mistakes/plots/heatmap_bias_cutoffs_2019.pdf", width=7, height=5)
p = levelplot(t(bias_cutoffs_mean_mat), xlab = "Application length", ylab = "Preference")
p
dev.off()

#TODO: standardize by the standard deviation of the Ratex distribution of cutoffs


# Adaptive beliefs --------------------------------------------------------

# Load panel of cutoffs
#TODO: Nacho has to make this the path where he saves this object from preprocess_data.R
cutoffs_panel = readRDS("data/interim/panel_cutoffs_2004_2020.rds")
# Read Oferta to create a short panel of vacancies
oferta_2018 = read.csv2("mistakes_and_warnings/data/Oferta/oferta2018_201812021829.csv", sep =";")
oferta_2019 = read.csv2("mistakes_and_warnings/data/Oferta/oferta2019_201812021829.csv", sep =";")
oferta_2020 = read.csv2("mistakes_and_warnings/data/Oferta/oferta2020_202002292151.csv", sep =";")

# Rename and select vars
oferta_2018[is.na(oferta_2018)] = 0
oferta_2018 %<>% mutate(vacs_tot_2018 = VACANTES_1SEM + VACANTES_2SEM,
                        cupos_tot_2018 = vacs_tot_2018 + SC_1S + SC_2S) %>% select(vacs_tot_2018, cupos_tot_2018, CODIGO)
oferta_2019[is.na(oferta_2019)] = 0
oferta_2019 %<>% mutate(vacs_tot_2019 = VAC_1SEM + VAC_2SEM,
                        cupos_tot_2019 = vacs_tot_2019 + SC_1SEM + SC_2SEM) %>% select(vacs_tot_2019, cupos_tot_2019, CODIGO)
oferta_2020[is.na(oferta_2020)] = 0
oferta_2020 %<>% mutate(vacs_tot_2020 = VAC_1SEM + VAC_2SEM,
                        cupos_tot_2020 = vacs_tot_2020 + SC_1SEM + SC_2SEM) %>% select(vacs_tot_2020, cupos_tot_2020, CODIGO)
oferta_panel = merge(oferta_2018, oferta_2019, by = "CODIGO", all = TRUE)
oferta_panel = merge(oferta_panel, oferta_2020, by = "CODIGO", all = TRUE)

# Paste cutoff panel to survey data and compare beliefs
dfs_cutoffs_2019_l = merge(dfs_cutoffs_2019_l, cutoffs_panel,
                           by.x = "codigo_car",
                           by.y = "codigo_carrera",
                           all.x = TRUE)
# Merge with oferta_panel 
dfs_cutoffs_2019_l = merge(dfs_cutoffs_2019_l, oferta_panel,
                           by.x = "codigo_car",
                           by.y = "CODIGO",
                           all.x = TRUE)

View(dfs_cutoffs_2019_l %>% select(cutoff, cutoff_pref, 
                                   cutoff_2018, cutoff_2019, 
                                   cutoff_2020,cutoff_mp,
                                   vacs_tot_2018,cupos_tot_2018,
                                   vacs_tot_2019,cupos_tot_2019,
                                   vacs_tot_2020,cupos_tot_2020,
                                   codigo_car, pref))

# Compute bias with respect to cutoffs 2019 (Ratex) and cutoffs 2018 (Adaptive beliefs)
dfs_cutoffs_2019_l %<>% mutate(dist_subj_cutofs_2019 = (cutoff - cutoff_2019)/sd(scores_carr),
                               dist_subj_cutofs_2018 = (cutoff - cutoff_2018)/sd(scores_carr))

# Distributions of bias in expected cutoffs

# For programs that raised cutoffs
ggplot(data = reshape2::melt(dfs_cutoffs_2019_l %>% filter(!is.na(dist_subj_cutofs_2019) &
                                                             !is.na(dist_subj_cutofs_2018)),
                             id.vars = "ID",
                             measure.vars = c("dist_subj_cutofs_2019", "dist_subj_cutofs_2018")),
       aes(x = value, fill = variable)) +
  geom_density(alpha = alpha) +
  xlab("Distance") +
  ylab("Bias in standardzed expected cutoff score (Subjective - Ratex)") +
  scale_fill_brewer(palette="Pastel1",
                    name="Beliefs",
                    breaks = c("dist_subj_cutofs_2019",
                               "dist_subj_cutofs_2018"),
                    labels = c("Ratex 2019",
                               "Ratex 2018 \n (Adaptive beliefs)")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/bias_exp_cutoff_ratex_adaptive.pdf", width=7, height=5)

# For programs that raised cutoffs
ggplot(data = reshape2::melt(dfs_cutoffs_2019_l %>% filter(!is.na(dist_subj_cutofs_2019) &
                                                             !is.na(dist_subj_cutofs_2018) &
                                                             ((cupos_tot_2019 - cupos_tot_2018)/cupos_tot_2018 >= 0.5)),
                             id.vars = "ID",
                             measure.vars = c("dist_subj_cutofs_2019", "dist_subj_cutofs_2018")),
       aes(x = value, fill = variable)) +
  geom_density(alpha = alpha) +
  xlab("Distance") +
  ylab("Bias in standardzed expected cutoff score (Subjective - Ratex)") +
  scale_fill_brewer(palette="Pastel1",
                    name="Beliefs",
                    breaks = c("dist_subj_cutofs_2019",
                               "dist_subj_cutofs_2018"),
                    labels = c("Ratex 2019",
                               "Ratex 2018 \n (Adaptive beliefs)")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/bias_exp_cutoff_ratex_adaptive_positive.pdf", width=7, height=5)

# For programs that decreased cutoffs
ggplot(data = reshape2::melt(dfs_cutoffs_2019_l %>% filter(!is.na(dist_subj_cutofs_2019) &
                                                             !is.na(dist_subj_cutofs_2018) &
                                                             ((cupos_tot_2019 - cupos_tot_2018)/cupos_tot_2018 <= -0.5)),
                             
                             id.vars = "ID",
                             measure.vars = c("dist_subj_cutofs_2019", "dist_subj_cutofs_2018")),
       aes(x = value, fill = variable)) +
  geom_density(alpha = alpha) +
  xlab("Distance") +
  ylab("Bias in standardzed expected cutoff score (Subjective - Ratex)") +
  scale_fill_brewer(palette="Pastel1",
                    name="Beliefs",
                    breaks = c("dist_subj_cutofs_2019",
                               "dist_subj_cutofs_2018"),
                    labels = c("Ratex 2019",
                               "Ratex 2018 \n (Adaptive beliefs)")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/bias_exp_cutoff_ratex_adaptive_negative.pdf", width=7, height=5)


# Prevalence and Relevance Strategic mistakes --------------------------------------------

# Prevalence of under-confidence mistakes
#Merge with Ratex beliefs
dfs_2019 = merge(dfs_2019, ratex_mp_2019, by.x = "ID", by.y = "id", all.x = TRUE)
dfs_2019 %<>% mutate(mp_ratex_19 = as.numeric(as.character(mp_ratex_19)))
hist(dfs_2019$mp_ratex_19)
sum(is.na(dfs_2019$mp_ratex_19))
# Repalce NA with zeros
dfs_2019 %<>% mutate(mp_ratex_19 = ifelse(is.na(mp_ratex_19), 0.0, mp_ratex_19))

#Bias in beliefs about top-true pref
dfs_2019 %<>% mutate(bias_cutoff_mp = mp_cutoff_belief - cutoff_mp)
dfs_2019 %<>% mutate(beliefs_cutoff_scorecarr_mp = mp_cutoff_belief - pp_mp)
hist(dfs_2019$bias_cutoff_mp)
hist(dfs_2019$beliefs_cutoff_scorecarr_mp)

# Under confident mistakes (for free and paid programs)
dfs_2019 %<>% mutate(under_confident_expost = ((n_apps < 10) & (misreport_exclude == 1) &
                                    (distance_mp >= 0) & (mp_ratex_19 > 0)),
                     under_confident_exante = ((n_apps < 10) & (misreport_exclude == 1) &
                                    (mp_ratex_19 > 0)))

#NOTE for the statistics we restrict attention to the version of the question that is NOT free

# Table of results with confidence intervals
# Exante
table_under_confident_exante = summarySE(data = dfs_2019 %>% subset(rand_money == 1),
          measurevar = "under_confident_exante",
          groupvars = c("know_cutoff"),
          na.rm=TRUE, conf.interval=.95, .drop=TRUE)
# Expost
table_under_confident_expost = summarySE(data = dfs_2019 %>% subset(rand_money == 1),
          measurevar = "under_confident_expost",
          groupvars = c("know_cutoff"),
          na.rm=TRUE, conf.interval=.95, .drop=TRUE)

# Table to count share of students with an (ex-post or ex-ante) underconfidence mistake
table_mean_underconf = dfs_2019 %>% subset(rand_money == 1) %>%
  group_by(know_cutoff) %>%
  summarise(mean_under_confidence_expost_perc = 100*mean(under_confident_expost),
            mean_under_confidence_exante_perc = 100*mean(under_confident_exante))
table_stderr_underconf = dfs_2019 %>% subset(rand_money == 1) %>%
  group_by(know_cutoff) %>%
  summarise(stderr_under_confidence_expost_perc = 100*sd(under_confident_expost)/sqrt(n()),
            stderr_under_confidence_exante_perc = 100*sd(under_confident_exante)/sqrt(n()))

#Write latex table by hand
table_latex_file = file("mistakes_and_warnings/documents/draft/tables/under_confidence.tex")
table_header = c("\\begin{table}[htbp!]",
                 "\\centering",
                 "\\caption{Under-confidence mistakes}",
                 "\\label{tab:under_confidence}",
                 "\\scalebox{0.7}{",
                 "\\begin{tabular}{lcc}",
                 "\\toprule",
                 "\\toprule")
table_columns = paste("Knowledge of cutoffs",
                      "\\makecell[c]{Under-confident mistake \\\\ (ex-post) [\\%]}",
                      "\\makecell[c]{Under-confident mistake \\\\ (ex-ante) [\\%]}", sep = " & ")
table_body = paste(table_columns, "\\\\" ," \\midrule")
space = '\\\\[0pt]'
table_stats = table_mean_underconf
table_sterr_stats = table_stderr_underconf
#Fill table body
for(i in 1:nrow(table_stats)){
  row = c()
  for(j in 1:ncol(table_stats)){
    row_stat = table_stats[i,j]
    if(j == 1){
      row = row_stat
    }
    else{
      row = paste(row, "&", round(row_stat,2))
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
  # Paste row for standard errors
  row = c()
  for(j in 1:ncol(table_sterr_stats)){
    row_stat = table_sterr_stats[i,j]
    if(j != 1){
      row = paste(row, "&", paste("(",round(row_stat,2),")",sep=""))
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
}

table_footer = c("\\bottomrule",
                 "\\multicolumn{3}{l}{\\textit{Note:} standard errors are computed in parenthesis.}",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
table_latex_code = c(table_header, table_body, table_footer)
writeLines(table_latex_code, table_latex_file)
close(table_latex_file)


# How much of this is explained by subjective beliefs?
hist(dfs_2019 %>% subset(rand_money == 1) %>%
       subset(cutoff_mp > 0) %>%
       subset(under_confident_exante == 1) %>% select(bias_cutoff_mp))
hist(dfs_2019 %>% subset(rand_money == 1) %>%
       subset(cutoff_mp > 0) %>%
       subset(under_confident_exante == 1) %>% select(beliefs_cutoff_scorecarr_mp))


# Distributions of bias for Expected cutoff of top-true preference for students by knowledge of cutoffs
ggplot(data = dfs_2019 %>% subset(rand_money == 1) %>%
         subset(cutoff_mp > 0) %>%
         subset(under_confident_exante == 1),
       aes(x = bias_cutoff_mp, fill = know_cutoff)) +
  geom_histogram(alpha = alpha, bins = 50) +
  xlab("Bias in Expected cutofffor top-true preference") +
  ylab("Frequency") +
  scale_fill_brewer(palette="Pastel1",
                    name="Knowledge of cutoffs")+
                    #breaks = c("dist_subj_cutofs_2019",
                    #           "dist_subj_cutofs_2018"),
                   #labels = c("Ratex 2019",
                   #             "Ratex 2018 \n (Adaptive beliefs)")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #legend.title = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/bias_exp_cutoff_ratex_top_true_underconfident.pdf", width=7, height=5)

# Distributions of bias for Expected cutoff of top-true preference for students by knowledge of cutoffs
ggplot(data = dfs_2019 %>% subset(rand_money == 1) %>%
         subset(cutoff_mp > 0) %>%
         subset(under_confident_exante == 1),
       aes(x = beliefs_cutoff_scorecarr_mp, fill = know_cutoff)) +
  geom_histogram(alpha = alpha, bins = 50) +
  xlab("Distance of Expected cutoff to Application score for top-true preference") +
  ylab("Frequency") +
  scale_fill_brewer(palette="Pastel1",
                    name="Knowledge of cutoffs")+
  #breaks = c("dist_subj_cutofs_2019",
  #           "dist_subj_cutofs_2018"),
  #labels = c("Ratex 2019",
  #             "Ratex 2018 \n (Adaptive beliefs)")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#ylim(0,100)
ggsave(file="mistakes_and_warnings/code/R/analysis mistakes/plots/distance_exp_cutoff_pp_top_true_underconfident.pdf", width=7, height=5)


# Table to count the share of these mistakes that can be explained by pesimistic behaviour
dfs_2019 %>% subset(rand_money == 1) %>% subset(cutoff_mp > 0) %>%
  subset(under_confident_expost == 1) %>%
  group_by(know_cutoff) %>%
  summarise(under_confidence_expost_perc_pesimistic = 100*mean((bias_cutoff_mp > 0)),
            under_confidence_expost_perc_weird = 100*mean((beliefs_cutoff_scorecarr_mp > 0)))


dfs_2019 %>% subset(rand_money == 1) %>% subset(cutoff_mp > 0) %>%
  subset(under_confident_exante == 1) %>%
  group_by(know_cutoff) %>%
  summarise(under_confidence_exante_perc_pesimistic = 100*mean((bias_cutoff_mp > 0)),
            under_confidence_exante_perc_weird = 100*mean((beliefs_cutoff_scorecarr_mp > 0)))




# Check if students who still skip programs even though the donot believe is out of their reach is due to
# measurement error or wrong beliefs about admission requirements
dfs_aux_asked_ppm =  dfs_reqs_2019_l %>% subset(!is.na(req_ppm_value_correct)) %>%
  group_by(ID) %>% summarise(mean_knows_req_ppm = mean(req_ppm_value_correct))

dfs_2019 = merge(dfs_2019, dfs_aux_asked_ppm, by = "ID", all.x = TRUE)

dfs_2019 %>% subset(rand_money == 1) %>% subset(cutoff_mp > 0) %>%
  subset(under_confident_expost == 1) %>%
  subset(!is.na(mean_knows_req_ppm)) %>%
  group_by(as.factor(mean_knows_req_ppm)) %>%
  summarise(freq = n(),
            under_confidence_expost_perc_pesimistic = 100*mean((bias_cutoff_mp > 0)))

#NOTE: we do not get enough observations to make inference and results do not go in the intuitive direction


# Projecting bias ---------------------------------------------------------

View(dfs_cutoffs_2019_l)
hist(dfs_cutoffs_2019_l$distance_subj_real_cutoffs)
# Merge with all variables
dfs_cutoffs_2019_l %<>% mutate(scores_carr_standard = ifelse(scores_carr != -1,
                                                             (scores_carr - mean(scores_carr))/sd(scores_carr), NA))
hist(dfs_cutoffs_2019_l$scores_carr_standard)
dfs_cutoffs_2019_l_merged = merge(dfs_cutoffs_2019_l, dfs_2019, by.x = "ID", by.y = "ID", all.x = TRUE)
dfs_cutoffs_2019_l_merged %<>% mutate(nem_standard         = ifelse(!is.na(PTJE_NEM),
                              (PTJE_NEM - mean(PTJE_NEM))/sd(PTJE_NEM), NA),
                              cutoff_2019_standard = ifelse(!is.na(cutoff_2019) & (cutoff_2019 > 0),
                                                            (cutoff_2019 - mean(cutoff_2019, na.rm = TRUE))/sd(cutoff_2019, na.rm = TRUE), NA),
                              dist_scores_carr_cutoff_2019 = ifelse((scores_carr!= -1) & (cutoff_2019 >= 0),
                                                                    (scores_carr - cutoff_2019)/sd(cutoff_2019, na.rm = TRUE), NA))
dfs_cutoffs_2019_l_merged %<>% mutate(last_listed = ifelse(n_apps.x == pref, 1, 0),
                                      is_mp = ifelse(pref_mp == pref, 1, 0))
# Merge with bias ratex
dfs_cutoffs_2019_l_merged = merge(dfs_cutoffs_2019_l_merged, dfs_beliefs_2019_l %>%
                                    select(ID, ratex, bias_ratex, nw, pref),
                                  by.x = c("ID", "pref"), by.y = c("ID", "pref"),
                                  all.x = TRUE)
hist(dfs_cutoffs_2019_l_merged$scores_carr)
hist(dfs_cutoffs_2019_l_merged$scores_carr_standard)
hist(dfs_cutoffs_2019_l_merged$nem_standard)
hist(dfs_cutoffs_2019_l_merged$distance_subj_real_cutoffs)
hist(dfs_cutoffs_2019_l_merged$cutoff_2019)
hist(dfs_cutoffs_2019_l_merged$cutoff_2019_standard)
hist(dfs_cutoffs_2019_l_merged$dist_scores_carr_cutoff_2019)
hist(dfs_cutoffs_2019_l_merged$bias_ratex)
table(dfs_cutoffs_2019_l_merged$last_listed, useNA = "always")
table(dfs_cutoffs_2019_l_merged$is_mp, useNA = "always")
table(dfs_cutoffs_2019_l_merged$nw, useNA = "always")
hist(dfs_cutoffs_2019_l_merged$INGRESO_BRUTO_FAM)


# Create similar dataframe but with only most preferred program 
dfs_mp_2019_merged = merge(dfs_2019 %>% filter(rand_money == 1), cutoffs_panel, by.x = "mp_carr", by.y = "codigo_carrera", all.x = TRUE)

# The variable mp_cutoff_belief is the subjective expectation

# Remove objects from the workspace to free RAM
#NOTE: does not free all memory, maybe due to heap memory usage
rm(list=setdiff(ls(), c("dfs_cutoffs_2019_l_merged", "dfs_mp_2019_merged")))
gc()



#Models with fixed effects
# Run regression with student fixed effect, program fixed effect, distance between score and cutoff,
#and rank of preference
dfs_cutoffs_2019_l_merged %<>% mutate(distance_subj_2018_cutoffs_prop = ifelse(cutoff_2018 > 0,
                                                                               100*(cutoff - cutoff_2018)/cutoff_2018, NA),
                                      dist_scores_carr_cutoff_2018_prop = ifelse((scores_carr!= -1) & (cutoff_2018 > 0),
                                                                                 100*(scores_carr - cutoff_2018)/cutoff_2018, NA),
                                      dist_scores_carr_cutoff_norm_2018_prop = abs(dist_scores_carr_cutoff_2018_prop),
                                      dist_positive = dist_scores_carr_cutoff_norm_2018_prop*(dist_scores_carr_cutoff_2018_prop > 0),
                                      dist_negative = dist_scores_carr_cutoff_norm_2018_prop*(dist_scores_carr_cutoff_2018_prop <= 0),
                                      dist_positive2 = dist_positive^2,
                                      dist_negative2 = dist_negative^2)
                                      



hist(dfs_cutoffs_2019_l_merged$dist_scores_carr_cutoff_norm_2018_prop)
hist(dfs_cutoffs_2019_l_merged$dist_positive)
hist(dfs_cutoffs_2019_l_merged$dist_positive2)
hist(dfs_cutoffs_2019_l_merged$dist_negative)
hist(dfs_cutoffs_2019_l_merged$dist_negative2)
hist(dfs_cutoffs_2019_l_merged$ratex)
#hist(dfs_cutoffs_2019_l_merged$distance_subj_2018_cutoffs)
hist(dfs_cutoffs_2019_l_merged$distance_subj_2018_cutoffs_prop)

# Two-way fixed-effects regression
bias_exp_cutoffs_tways = feols(data = dfs_cutoffs_2019_l_merged %>%
                               subset((scores_carr != -1) & !is.na(cutoff_2018)) %>%
                               subset(cutoff_2018 > 0) %>%   
                               #subset(ratex < 100) %>%
                               #subset(ratex > 0) %>%
                               filter(know_cutoff.x == "Knows the cutoffs for \n every program in the list") %>%
                               select(ID, 
                                      codigo_car,
                                      last_listed,
                                      distance_subj_2018_cutoffs_prop,
                                      #dist_scores_carr_cutoff_2019,
                                      dist_scores_carr_cutoff_norm_2018_prop,
                                      pref,
                                      scores_carr_standard,
                                      is_mp,
                                      nw,
                                      ratex,
                                      dist_positive,
                                      dist_positive2,
                                      dist_negative,
                                      dist_negative2,
                                      cutoff_2018,
                                      cutoff),
                             #fml = distance_subj_real_cutoffs ~
                             fml = distance_subj_2018_cutoffs_prop ~
                             #fml = cutoff ~
                               #cutoff_pref +
                               #scores_carr_standard  +
                               #poly(nem_standard,1)  +
                               #as.factor(is_female) +
                               #as.factor(INGRESO_BRUTO_FAM <= 5) +
                               #as.factor(BEA) +
                               #as.factor(GRUPO_DEPENCIA) +
                               #as.factor(RAMA_EDUCACIONAL) +
                               #(ratex == 0) +
                               #(ratex == 1) +
                               #as.factor(codigo_car) +
                               #as.factor(last_listed) +
                               #as.factor(is_mp) +
                               #as.factor(nw) +
                               #poly(cutoff_2019_standard,1) +
                               #dist_scores_carr_cutoff_2019 +
                               #dist_scores_carr_cutoff_square_2019*(dist_scores_carr_cutoff_2019 < 0) +
                               #dist_scores_carr_cutoff_square_2019*(dist_scores_carr_cutoff_2019 >= 0) +
                               #dist_scores_carr_cutoff_square_2019*(dist_scores_carr_cutoff_2019 < 0 ) +
                               #poly(dist_positive,2, raw = TRUE) +
                               dist_positive +
                               #dist_positive2 +
                               #poly(dist_negative,2, raw = TRUE) +
                               dist_negative +
                               #dist_negative2 +
                               #as.factor(know_cutoff) +
                               #as.factor(pref):as.factor(n_apps.x)
                               #as.factor(pref) | ID + codigo_car)
                               as.factor(pref) | ID)
summary(bias_exp_cutoffs_tways)

#Redoing the previous analysis but only for top-true preference
dfs_mp_2019_merged %<>% mutate(distance_subj_2018_cutoffs_prop = ifelse(cutoff_2018 > 0,
                                                                        100*(mp_cutoff_belief - cutoff_2018)/cutoff_2018, NA),
                                      dist_scores_carr_cutoff_2018_prop = ifelse((pp_mp!= 0) & (cutoff_2018 > 0),
                                                                                 100*(pp_mp - cutoff_2018)/cutoff_2018, NA),
                                      dist_scores_carr_cutoff_norm_2018_prop = abs(dist_scores_carr_cutoff_2018_prop),
                                      dist_positive = dist_scores_carr_cutoff_norm_2018_prop*(dist_scores_carr_cutoff_2018_prop > 0),
                                      dist_negative = dist_scores_carr_cutoff_norm_2018_prop*(dist_scores_carr_cutoff_2018_prop <= 0),
                                      dist_positive2 = dist_positive^2,
                                      dist_negative2 = dist_negative^2)
# PLots some stats
hist(dfs_mp_2019_merged$dist_scores_carr_cutoff_norm_2018_prop)
hist(dfs_mp_2019_merged$dist_positive)
hist(dfs_mp_2019_merged$dist_positive2)
hist(dfs_mp_2019_merged$dist_negative)
hist(dfs_mp_2019_merged$dist_negative2)
#hist(dfs_mp_2019_merged$ratex)
hist(dfs_mp_2019_merged$mp_cutoff_belief)
hist(dfs_mp_2019_merged$pp_mp)
#hist(dfs_mp_2019_merged$distance_subj_2018_cutoffs)
hist(dfs_mp_2019_merged$distance_subj_2018_cutoffs_prop)

# Linear regression
bias_exp_cutoffs_lm = lm(data = dfs_mp_2019_merged %>%
                                 subset((pp_mp != 0) & !is.na(cutoff_2018)) %>%
                                 subset(cutoff_2018 > 0) %>%   
                                 #subset(ratex < 100) %>%
                                 #subset(ratex > 0) %>%
                                 filter(know_cutoff == "Knows the cutoffs for \n every program in the list") %>%
                                 select(ID, 
                                        mp_carr,
                                        #last_listed,
                                        distance_subj_2018_cutoffs_prop,
                                        #dist_scores_carr_cutoff_2019,
                                        dist_scores_carr_cutoff_norm_2018_prop,
                                        #pref,
                                        #scores_carr_standard,
                                        #is_mp,
                                        #nw,
                                        #ratex,
                                        dist_positive,
                                        dist_positive2,
                                        dist_negative,
                                        dist_negative2,
                                        cutoff_2018,
                                        mp_cutoff_belief),
                               #fml = distance_subj_real_cutoffs ~
                               formula = distance_subj_2018_cutoffs_prop ~
                                 #fml = cutoff ~
                                 #cutoff_pref +
                                 #scores_carr_standard  +
                                 #poly(nem_standard,1)  +
                                 #as.factor(is_female) +
                                 #as.factor(INGRESO_BRUTO_FAM <= 5) +
                                 #as.factor(BEA) +
                                 #as.factor(GRUPO_DEPENCIA) +
                                 #as.factor(RAMA_EDUCACIONAL) +
                                 #(ratex == 0) +
                                 #(ratex == 1) +
                               #as.factor(codigo_car) +
                               #as.factor(last_listed) +
                               #as.factor(is_mp) +
                               #as.factor(nw) +
                               #poly(cutoff_2019_standard,1) +
                               #dist_scores_carr_cutoff_2019 +
                               #dist_scores_carr_cutoff_square_2019*(dist_scores_carr_cutoff_2019 < 0) +
                               #dist_scores_carr_cutoff_square_2019*(dist_scores_carr_cutoff_2019 >= 0) +
                               #dist_scores_carr_cutoff_square_2019*(dist_scores_carr_cutoff_2019 < 0 ) +
                               #poly(dist_positive,2, raw = TRUE) +
                               dist_positive +
                                 #dist_positive2 +
                                 #poly(dist_negative,2, raw = TRUE) +
                                 dist_negative)
                                 #dist_negative2 +
                                 #as.factor(know_cutoff) +
                                 #as.factor(pref):as.factor(n_apps.x)
                                 #as.factor(pref) | ID + codigo_car)
                                 #as.factor(pref) | ID)
summary(bias_exp_cutoffs_lm)

# Projecting bias on observable characteristics of students

#Regression of bias for expected cutoff over a set of characteristics
m_bias_cutoffs = lm(data = dfs_cutoffs_2019_l_merged %>%
                      subset((scores_carr != -1) & !is.na(cutoff_2018)) %>%
                      subset(cutoff_2018 > 0) %>%   
                      #subset(ratex < 100) %>%
                      #subset(ratex > 0) %>%
                      filter(know_cutoff.x == "Knows the cutoffs for \n every program in the list"),
                    formula = distance_subj_2018_cutoffs_prop ~
                      dist_positive +
                      dist_negative +
                      poly(scores_carr_standard,1, raw = TRUE)  +
                      poly(nem_standard,1, raw = TRUE)  +
                      as.factor(is_female) +
                      as.factor(INGRESO_BRUTO_FAM <= 5) +
                      as.factor(BEA) +
                      as.factor(GRUPO_DEPENCIA) +
                      as.factor(RAMA_EDUCACIONAL) +
                      #(ratex == 0) +
                      #(ratex == 1) +
                      #as.factor(codigo_car) +
                      as.factor(last_listed) +
                      as.factor(is_mp) +
                      as.factor(nw) +
                      poly(cutoff_2019_standard,1, raw = TRUE) +
                      #poly(dist_scores_carr_cutoff_2019*dist_scores_carr_cutoff_2019,1, raw = TRUE) +
                      #poly(dist_scores_carr_cutoff_2019*dist_scores_carr_cutoff_2019*(dist_scores_carr_cutoff_2019 < 0 ),1, raw = TRUE) +
                      #as.factor(know_cutoff) +
                      #as.factor(pref):as.factor(n_apps.x)
                      as.factor(pref) +
                      as.factor(n_apps.x))
#as.factor(pref)*as.factor(n_apps.x))
summary(m_bias_cutoffs)

#Regression of bias norm for expected cutoff over a set of characteristics
library(stargazer)

#TODO: the dependent variable is always positive here, so we need to take a log or so (after addressing the zeros)
m_bias_norm_cutoffs = lm(data = dfs_cutoffs_2019_l_merged %>%
                           subset((scores_carr != -1) & !is.na(cutoff_2018)) %>%
                           subset(cutoff_2018 > 0) %>%   
                           #subset(ratex < 100) %>%
                           #subset(ratex > 0) %>%
                           filter(know_cutoff.x == "Knows the cutoffs for \n every program in the list"),
                         formula = sqrt(distance_subj_2018_cutoffs_prop^2) ~
                           dist_positive +
                           dist_negative +
                           poly(scores_carr_standard,1, raw = TRUE)  +
                           poly(nem_standard,1, raw = TRUE)  +
                           as.factor(is_female) +
                           #as.factor(INGRESO_BRUTO_FAM <= 5) +
                           as.factor(BEA) +
                           as.factor(GRUPO_DEPENCIA) +
                           #as.factor(RAMA_EDUCACIONAL) +
                           #(ratex == 0) +
                           #(ratex == 1) +
                           #as.factor(codigo_car) +
                           as.factor(last_listed) +
                           as.factor(is_mp) +
                           as.factor(nw) +
                           poly(cutoff_2019_standard,1, raw = TRUE) +
                           #poly(dist_scores_carr_cutoff_2019*dist_scores_carr_cutoff_2019,1, raw = TRUE) +
                           #poly(dist_scores_carr_cutoff_2019*dist_scores_carr_cutoff_2019*(dist_scores_carr_cutoff_2019 < 0 ),1, raw = TRUE) +
                           #as.factor(know_cutoff) +
                           #as.factor(pref):as.factor(n_apps.x)
                           as.factor(pref) +
                           as.factor(n_apps.x))
#as.factor(pref)*as.factor(n_apps.x))
summary(m_bias_norm_cutoffs)

stargazer(m_bias_norm_cutoffs, type = "latex")

# Stargazer the table to latex

#stargazer(bias_exp_cutoffs_tways, 
#          #bias_exp_cutoffs_norm_tways, 
#          title="Two-way Fixed Effects Regression Results",
#          align=TRUE, 
#          #dep.var.labels=c("Bias in Expected cutoffs", "Norm of Bias in Expected cutoffs"),
#          dep.var.labels=c("Bias in Expected cutoffs"),
#          #single.row=TRUE,
#          covariate.labels=c(#"Application score",
#                             #"Application score quadratic",
#                             #"Application score cubic",
#                             "Distance score to cutoff (positive)",
#                             "Distance score to cutoff (negative)",
#                             #"Distance to cutoff quadratic",
#                             #"Distance to cutoff cubic",
#                             "Preference 2",
#                             "Preference 3",
#                             "Preference 4",
#                             "Preference 5",
#                             "Preference 6",
#                             "Preference 7",
#                             "Preference 8",
#                             "Preference 9",
#                             "Preference 10"),
#          #omit = c("CODIGO_DEMRE_2015", "mothers_edu", "fathers_edu", "income"),
#          label = "tab:two_way_fe",
#          omit.stat=c("LL","ser","f", "aic"), no.space=TRUE, report = "vc*",
#         type = "latex", out =  "mistakes_and_warnings/documents/draft/tables/two_way_fe_bias.tex")