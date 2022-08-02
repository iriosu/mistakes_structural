library(plyr)
library(dplyr)
library(tidyr)
library(fastDummies)
library(reshape2)
library(corrplot)
library(stargazer)
library(stringr)
library(ggplot2)
library(rddtools)
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
library(haven)
library(tidyverse)

rm(list=ls())
gc()

# 0. Set paths --------------------------
dropbox_dir <- "~/Dropbox/"
indir <- paste(dropbox_dir, "Mistakes/Code/Python/Cartillas/data/2022", sep="")
outdir <- paste(dropbox_dir, "Mistakes/Code/Python/Cartillas/outputs/2022", sep="")


# 1. Read inputs --------------------------
apps <- read.csv(paste(outdir, '/applications_and_probs.csv', sep=''), header=TRUE, sep=';')
st <- read.csv(paste(indir, '/students_stratas_to_complete_market.csv', sep=''), header=TRUE, sep=',')
st %<>% dplyr::rename(MRUN = mrun)

# Missing so far
cp <- read.csv(paste(indir, '/accesos-cartilla-puntajes.csv', sep=''), header=TRUE, sep=',')
cp %<>% dplyr::rename(MRUN = id) %>% dplyr::select(MRUN) %>% mutate(open_cartilla = 1)
cp %<>% group_by(MRUN) %>% mutate(rid = row_number()) %>% filter(rid == 1) %>% ungroup %>% as.data.frame()

rec <- read.csv(paste(outdir, '/recommendations_better_option.csv', sep=''), header=TRUE, sep=',')

# Merge
df <- merge(apps, st, by="MRUN")
df <- merge(df, cp, by="MRUN", all.x=TRUE)
df$open_cartilla[is.na(df$open_cartilla)] <- 0

df <- merge(df, rec, by="MRUN")

# df$better_option = rbinom(n=nrow(df), size=1, prob=0.5)

df %<>% mutate(general_message = ifelse(prob_adaptive_1 >= 0.99 & prob_interim_1 >= 0.99 & better_option == 1, 1, ifelse(overall_prob_adaptive < 0.99 & overall_prob_interim < 0.99, 2, 3)))

# 2. Assign to SMS --------------------------
n_sms = 30000

total = nrow(df)
targets <- df %>% group_by(strata_score, strata_region, strata_sexo, general_message, open_cartilla) %>% tally() %>% mutate(pct = n/total, n_to_sample = round(pct*n_sms) ) %>% as.data.frame() %>% ungroup
out = NULL
for(r in 1:nrow(targets)){
  strata_score_r = targets$strata_score[r]
  strata_region_r = targets$strata_region[r] 
  strata_sexo_r = targets$strata_sexo[r]
  general_message_r = targets$general_message[r]
  open_cartilla_r = targets$open_cartilla[r]
  n_to_sample_r = targets$n_to_sample[r]
  
  if (r==1){
    out <- df %>% filter(strata_score == strata_score_r & strata_region == strata_region_r & strata_sexo == strata_sexo_r &
                           general_message == general_message_r & open_cartilla == open_cartilla_r) %>% slice_sample(n=n_to_sample_r) 
  }else{
    out <- rbind(out, df %>% filter(strata_score == strata_score_r & strata_region == strata_region_r & strata_sexo == strata_sexo_r &
                                      general_message == general_message_r & open_cartilla == open_cartilla_r) %>% slice_sample(n=n_to_sample_r) )
  }
}
out$gets_sms <- 1

write.csv(out %>% dplyr::select(MRUN), paste(outdir, '/list_for_sms.csv', sep=''),  row.names=FALSE)

df <- merge(df, out %>% dplyr::select(MRUN, gets_sms), by="MRUN", all.x=TRUE)
df$gets_sms[is.na(df$gets_sms)] <- 0

# 3. Assign to treatments --------------------------
targets <- df %>% group_by(strata_score, strata_region, strata_sexo, general_message, open_cartilla, gets_sms) %>% tally() %>% mutate(n_to_sample = floor(n/4) ) %>% as.data.frame() %>% ungroup

out = NULL
for(r in 1:nrow(targets)){
  strata_score_r = targets$strata_score[r]
  strata_region_r = targets$strata_region[r] 
  strata_sexo_r = targets$strata_sexo[r]
  general_message_r = targets$general_message[r]
  open_cartilla_r = targets$open_cartilla[r]
  n_to_sample_r = targets$n_to_sample[r]
  gets_sms_r = targets$gets_sms[r]
  
  if (r==1){
    nr = df %>% filter(strata_score == strata_score_r & strata_region == strata_region_r & strata_sexo == strata_sexo_r &
                         general_message == general_message_r & open_cartilla == open_cartilla_r & gets_sms == gets_sms_r) %>% nrow()
    
    out <- df %>% filter(strata_score == strata_score_r & strata_region == strata_region_r & strata_sexo == strata_sexo_r &
                           general_message == general_message_r & open_cartilla == open_cartilla_r & gets_sms == gets_sms_r) %>% mutate(rid = row_number(), misfit = ifelse(rid > nr-mod(nr,4), 1, 0),  treatment = mod(rid, 4) + 1)
  }else{
    out <- rbind(out, df %>% filter(strata_score == strata_score_r & strata_region == strata_region_r & strata_sexo == strata_sexo_r &
                                      general_message == general_message_r & open_cartilla == open_cartilla_r & gets_sms == gets_sms_r) %>% mutate(rid = row_number(), misfit = ifelse(rid > nr-mod(nr,4), 1, 0), treatment = mod(rid, 4) + 1))
  }
}

# 4. Add addiitonal variables --------------------------
for(p in 1:10){
  col = paste('prob_', p, sep='')
  col_int = paste('prob_interim_', p, sep='')
  col_ada = paste('prob_adaptive_', p, sep='')
  out[,col] <- ifelse(out[,col_int] < out[,col_ada], out[,col_int], out[,col_ada])
}
out %<>% mutate(overall_prob = overall_prob_interim)

write.csv(out %>% dplyr::select(-c(rid)), paste(outdir, '/master_students_with_treatment_and_stratas.csv', sep=''),  row.names=FALSE)

# 5. Create table applications --------------------------
codes <- read.csv(paste(outdir, "/bbdd/codigos_carreras_ies.csv", sep=''), header=TRUE, sep=',')
rep <- read.csv(paste(indir, '/raw/postulaciones_actual_12_01_22.csv',sep=''), header=TRUE, sep=',')

rep$fecha_postulacion <- as.character(rep$fecha_postulacion)
rep$folio_postulacion <- as.character(rep$folio_postulacion)

out_l <- reshape(out[,c(1,2:61)], direction='long', 
                 varying=colnames(out[,c(2:61)]), 
                 times=c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'),
                 v.names=c('codigo_carrera', 'valida','puntpond', 'prob_interim', 'prob_adaptive', 'prob_level'),
                 idvar='MRUN')

colnames(out_l) <- c("student_id", "application_ranking", "CODIGO_DEMRE", "prob_adaptive", "prob_interim", "admission_probability_level", "score_application", "application_statud_id")
out_l %<>% filter(CODIGO_DEMRE > 0)

out_l <- merge(out_l, codes, by = "CODIGO_DEMRE")
out_l <- merge(out_l, rep %>% dplyr::select(mrun, folio_postulacion, fecha_postulacion) %>% dplyr::rename(student_id = mrun, application_date = fecha_postulacion, application_id = folio_postulacion), by="student_id")
out_l %<>% dplyr::rename(program_id = CODIGO_SIES, institution_id = COD_IES)
out_l %<>% mutate(admission_probability = prob_interim, distance='', 
                  score_application = score_application/100)


out_l$application_date <- "2022-01-12 00:01:00"
out_l$application_date <- strptime(out_l$application_date, '%Y-%m-%d %H:%M:%S')

cols <- c("student_id", "application_id", "institution_id", "program_id", "application_ranking", "application_statud_id",  "score_application", "admission_probability",  "admission_probability_level", "application_date", "distance")

out_apps <- out_l %>% dplyr::select(cols)

write.table(out_apps, paste(outdir, '/table_applications.csv', sep=''),  sep=';', row.names=FALSE, quote=FALSE)

# 6. Create students with extra variables --------------------------
c22 <- read.csv("/Users/iriosu/Dropbox/Mistakes/Code/Python/Cartillas/data/2022/puntajes_consolidado_v2.csv", header=TRUE, sep=',')
i22 <- read.csv("/Users/iriosu/Dropbox/Mistakes/Code/Python/Cartillas/data/2022/inscritos_consolidado_v2.csv", header=TRUE, sep=',')
i22 %<>% separate(nombres, c("primer", "segundo"), " ") %>% dplyr::rename(name = primer) %>% mutate(female = ifelse(sexo == 'F', TRUE, FALSE))
fuas <- read.csv("/Users/iriosu/Dropbox/Mistakes/Code/Python/Cartillas/data/2022/raw/reporte-FUAS-20211221-162111-62010_MRUN.csv", header=TRUE, sep=';')
fuas %<>% dplyr::rename(elegibility = INDICADOR, id = mrun) %>% dplyr::mutate(elegibility = ifelse(is.na(elegibility), 0, elegibility), fuas = '')

students <- merge(c22, i22 %>% dplyr::select(mrun, female, name), by="mrun", all.x=TRUE)
students %<>% mutate(special_admission = ifelse(bea == 'BEA', 1, ifelse(pace == 'PACE', 2, 0)), 
                     last_name = 'a',
                     parent_id = '', 
                     age = '', 
                     address = '', 
                     lat = 1.1, 
                     long = 1.1, 
                     school_region_id = ifelse(is.na(codigo_region), 0, codigo_region), 
                     school_type = ifelse(is.na(grupo_dependencia), 0, grupo_dependencia), 
                     school_score = '', 
                     diff_school = '', 
                     prior_enrollment = '') %>% dplyr::rename(gpa = promedio_notas, nem = ptje_nem, ranking = ptje_ranking, id = mrun)

students <- merge(students, out %>% mutate(application_in_process = 1) %>% dplyr::select(MRUN, application_in_process, treatment, overall_prob, better_option), by.x='id', by.y='MRUN', all.x=TRUE)
students$application_in_process[is.na(students$application_in_process)] <- 0
students$treatment[is.na(students$treatment)] <- ''
students$overall_prob[is.na(students$overall_prob)] <- ''
students$better_option[is.na(students$better_option)] <- ''

students$general_admission_probability = students$overall_prob

students$treatment_id = ifelse(students$treatment == "", '', as.numeric(students$treatment))
students$application_situation <- ''

students <- merge(students, rep %>% dplyr::select(mrun, folio_postulacion) %>% dplyr::rename(id = mrun, application_id = folio_postulacion), by="id", all.x=TRUE)
students$application_id[is.na(students$application_id)] <- ''

students <- merge(students, fuas, by="id", all.x=TRUE)
students$elegibility[is.na(students$elegibility)] <- ''
students$fuas[is.na(students$fuas)] <- ''

students$mode_language <- ''

any(is.na(students))

out_st <- students %>% dplyr::select(id, parent_id, application_id, application_in_process, special_admission, name, last_name, age, address, 
                                    lat, long, gpa, nem, ranking, female, school_region_id, school_type, school_score, diff_school, 
                                    prior_enrollment, treatment_id, application_situation, fuas, elegibility, general_admission_probability, better_option, mode_language)

out_st$female <- ifelse(out_st$female == TRUE, 1, 0)
out_st$school_region_id <- ifelse(out_st$school_region_id == 0, '', out_st$school_region_id)
out_st %<>% mutate(parent_id = row_number()) %>% mutate(parent_id = as.character(parent_id))
out_st$parent_id

write.table(out_st, paste(outdir, '/table_students_cartilla2.csv', sep=''),  sep=';', row.names=FALSE, quote=FALSE)



parent <- out_st %>% dplyr::select(parent_id, lat, long, treatment_id) %>% mutate(name = "*", last_name="*", address="*", treatment_id = 1) %>% dplyr::rename(id = parent_id)
write.table(parent, paste(outdir, '/table_parent.csv', sep=''),  sep=';', row.names=FALSE, quote=FALSE)

colnames(out_st)


student_score <- c22 %>% dplyr::select(mrun, clec_actual, mate_actual, hcso_actual, cien_actual) %>% mutate(year = 2022) %>% dplyr::rename(verbal_score = clec_actual, math_score = mate_actual, history_score = hcso_actual, science_score = cien_actual)
student_score <- rbind(student_score, c22 %>% dplyr::select(mrun, clec_anterior, mate_anterior, hcso_anterior, cien_anterior) %>% mutate(year = 2021) %>% dplyr::rename(verbal_score = clec_anterior, math_score = mate_anterior, history_score = hcso_anterior, science_score = cien_anterior))
student_score %<>% mutate(score_process = 1, 
                          percentile_nem = '', 
                          percentile_ranking = '', 
                          percentile_verbal = '', 
                          percentile_math = '', 
                          percentile_history = '', 
                          percentile_science = '', 
                          decile_nem = '', 
                          decile_ranking = '', 
                          decile_verbal = '', 
                          decile_math = '', 
                          decile_history = '', 
                          decile_science = '', 
                          decile_stem = '', 
                          decile_hum = '', 
                          decile_total = '', 
                          percent_stem = '', 
                          percent_hum = '', 
                          percent_total = '', 
                          treatment_destacado = '', 
                          percent_nemranking = '', 
                          decile_nemranking = '')



students %>% filter(treatment_id == 4) %>% dplyr::select(id)

write.table(student_score,paste(outdir, '/table_student_score_cartilla2.csv', sep=''),  sep=';', row.names=FALSE, quote=FALSE)





