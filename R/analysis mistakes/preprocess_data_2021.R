# Swithing and dropout regressions and odds ratios test if iriosu can make
# changes
rm(list = ls())

# Libraries ---------------------------------------------------------------

dropbox_dir = "~/Dropbox/"
input_path = paste(dropbox_dir, 'Dropout_matching/',sep='')
setwd(paste(dropbox_dir, "Dropout/Code/R", sep=''))


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

##################### Merge BDC 2021 ###################################
dropbox_dir <- "~/Dropbox/"
survey_2021 <- read.csv(paste(dropbox_dir, "Mistakes in college admissions/data/clean/survey-responses-2021.csv", sep=''), header=TRUE)

# Load HASH

load(paste(dropbox_dir, "Mistakes Structural/Data/2021/ABCD 2021 with Hash/2021/A_202108130306.rdata", sep = ""))
load(paste(dropbox_dir, "Mistakes Structural/Data/2021/ABCD 2021 with Hash/2021/B_202108130306.rdata", sep = ""))
load(paste(dropbox_dir, "Mistakes Structural/Data/2021/ABCD 2021 with Hash/2021/C_202108130306.rdata", sep = ""))
load(paste(dropbox_dir, "Mistakes Structural/Data/2021/ABCD 2021 with Hash/2021/D_202108130306.rdata", sep = ""))

b_2021$id <- as.character(b_2021$ID)
c_2021$id <- as.character(c_2021$ID)
d_2021$id <- as.character(d_2021$ID)
survey_2021$id <- as.character(survey_2021$id)
survey_2021$ID <- survey_2021$id

d_2021 %<>% mutate(applied_2021 = ifelse(is.na(COD_CARRERA_PREF_01), 0, 1), 
                   assigned_2021 = ifelse((ESTADO_PREF_01==24) + (ESTADO_PREF_02==24) + (ESTADO_PREF_03==24) + (ESTADO_PREF_04==24) + (ESTADO_PREF_05==24) +
                                            (ESTADO_PREF_06==24) + (ESTADO_PREF_07==24) + (ESTADO_PREF_08==24) + (ESTADO_PREF_09==24) + (ESTADO_PREF_10==24) == 1, 1, 0), 
                   number_apps = ((COD_CARRERA_PREF_01 > 0) + (COD_CARRERA_PREF_02 > 0) + (COD_CARRERA_PREF_03 > 0) + (COD_CARRERA_PREF_04 > 0) + (COD_CARRERA_PREF_05 > 0) + 
                                    (COD_CARRERA_PREF_06 > 0) + (COD_CARRERA_PREF_07 > 0) + (COD_CARRERA_PREF_08 > 0) + (COD_CARRERA_PREF_09 > 0) + (COD_CARRERA_PREF_10 > 0)), 
                   assigned_to = ifelse(ESTADO_PREF_01 == 24, COD_CARRERA_PREF_01, 
                                        ifelse(ESTADO_PREF_02 == 24, COD_CARRERA_PREF_02,
                                               ifelse(ESTADO_PREF_03 == 24, COD_CARRERA_PREF_03,
                                                      ifelse(ESTADO_PREF_04 == 24, COD_CARRERA_PREF_04,
                                                             ifelse(ESTADO_PREF_05 == 24, COD_CARRERA_PREF_05,
                                                                    ifelse(ESTADO_PREF_06 == 24, COD_CARRERA_PREF_06,
                                                                           ifelse(ESTADO_PREF_07 == 24, COD_CARRERA_PREF_07,
                                                                                  ifelse(ESTADO_PREF_08 == 24, COD_CARRERA_PREF_08,
                                                                                         ifelse(ESTADO_PREF_09 == 24, COD_CARRERA_PREF_09,
                                                                                                ifelse(ESTADO_PREF_10 == 24, COD_CARRERA_PREF_10, 0)))))))))), 
                   assigned_pref = ifelse(ESTADO_PREF_01 == 24, 1, 
                                          ifelse(ESTADO_PREF_02 == 24, 2,
                                                 ifelse(ESTADO_PREF_03 == 24, 3,
                                                        ifelse(ESTADO_PREF_04 == 24, 4,
                                                               ifelse(ESTADO_PREF_05 == 24, 5,
                                                                      ifelse(ESTADO_PREF_06 == 24, 6,
                                                                             ifelse(ESTADO_PREF_07 == 24, 7,
                                                                                    ifelse(ESTADO_PREF_08 == 24, 8,
                                                                                           ifelse(ESTADO_PREF_09 == 24, 9,
                                                                                                  ifelse(ESTADO_PREF_10 == 24, 10, 0)))))))))), 
                   valid_apps =     ((ESTADO_PREF_01 >= 24 & ESTADO_PREF_01 <= 26) + (ESTADO_PREF_02 >= 24 & ESTADO_PREF_02 <= 26) + (ESTADO_PREF_03 >= 24 & ESTADO_PREF_03 <= 26) + (ESTADO_PREF_04 >= 24 & ESTADO_PREF_04 <= 26) + (ESTADO_PREF_05 >= 24 & ESTADO_PREF_05 <= 26) + 
                                       (ESTADO_PREF_06 >= 24 & ESTADO_PREF_06 <= 26) + (ESTADO_PREF_07 >= 24 & ESTADO_PREF_07 <= 26) + (ESTADO_PREF_08 >= 24 & ESTADO_PREF_08 <= 26) + (ESTADO_PREF_09 >= 24 & ESTADO_PREF_09 <= 26) + (ESTADO_PREF_10 >= 24 & ESTADO_PREF_10 <= 26))
)


df_2021 <- merge(b_2021, c_2021[,c("id",setdiff(colnames(c_2021), colnames(b_2021)))], by="id", all.x=TRUE)
df_2021 <- merge(df_2021, d_2021[,c("id",setdiff(colnames(d_2021), colnames(df_2021)))], by="id", all.x=TRUE)

# saveRDS(df_2021, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_2021_20210420.rds", sep=''))
saveRDS(df_2021, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_2021_20210812.rds", sep=''))


## Add additional information
car21 <- read.csv(file = paste(dropbox_dir, "Mistakes in college admissions/data/raw/implementation/survey-2021/carreras.csv", sep = "") , header=TRUE, sep=';')

for(col in colnames(car21)){
  names(car21)[names(car21) == col] <- paste(col, "mp", sep="_")
}

dfs_2021 <- merge(df_2021, survey_2021, by="id", all.x=TRUE)
dfs_2021 <- merge(dfs_2021, car21, by.x="mp_carr", by.y="CODIGO_mp", all.x=TRUE)

dfs_2021 %<>% dplyr::rename(LENG_ACTUAL = CLEC_ACTUAL, PROMEDIO_LM_ACTUAL = PROMEDIO_CM_ACTUAL)

dfs_2021 %<>% mutate(psu_actual_mp = LENG_ACTUAL*PCT_LENG_mp + MATE_ACTUAL*PCT_MATE_mp + 
                       ifelse(ifelse(!is.na(CIEN_ACTUAL) & !is.na(PCT_CIEN_mp), CIEN_ACTUAL*PCT_CIEN_mp, 0) > ifelse(!is.na(HCSO_ACTUAL) & !is.na(PCT_HCSO_mp), HCSO_ACTUAL*PCT_HCSO_mp, 0), ifelse(!is.na(CIEN_ACTUAL) & !is.na(PCT_CIEN_mp), CIEN_ACTUAL*PCT_CIEN_mp, 0), ifelse(!is.na(HCSO_ACTUAL) & !is.na(PCT_HCSO_mp), HCSO_ACTUAL*PCT_HCSO_mp, 0)), 
                     psu_anterior_mp = LENG_ANTERIOR*PCT_LENG_mp + MATE_ANTERIOR*PCT_MATE_mp + 
                       ifelse(ifelse(!is.na(CIEN_ANTERIOR) & !is.na(PCT_CIEN_mp), CIEN_ANTERIOR*PCT_CIEN_mp, 0) > ifelse(!is.na(HCSO_ANTERIOR) & !is.na(PCT_HCSO_mp), HCSO_ANTERIOR*PCT_HCSO_mp, 0), ifelse(!is.na(CIEN_ANTERIOR) & !is.na(PCT_CIEN_mp), CIEN_ANTERIOR*PCT_CIEN_mp, 0), ifelse(!is.na(HCSO_ANTERIOR) & !is.na(PCT_HCSO_mp), HCSO_ANTERIOR*PCT_HCSO_mp, 0)))

dfs_2021 %<>% mutate(anterior_best_mp = ifelse(!is.na(psu_anterior_mp), ifelse(psu_anterior_mp > psu_actual_mp, 1, 0), 0))

dfs_2021 %<>%  mutate(pp_mp = ifelse(PTJE_NEM == 0, 
                                     ifelse(anterior_best_mp == 0, psu_actual_mp/(100-PCT_NOTAS_mp-PCT_RANKING_mp), psu_anterior_mp/(100-PCT_NOTAS_mp-PCT_RANKING_mp)),
                                     (PTJE_RANKING*PCT_RANKING_mp + PTJE_NEM*PCT_NOTAS_mp)/100 + ifelse(anterior_best_mp == 0, psu_actual_mp/(100), psu_anterior_mp/(100))))


dfs_2021 %<>% mutate(applied_to_mp = ifelse(is.na(mp_carr), 0, ifelse( ((COD_CARRERA_PREF_01 == mp_carr) + (COD_CARRERA_PREF_02 == mp_carr) + 
                                                                          (COD_CARRERA_PREF_03 == mp_carr) + (COD_CARRERA_PREF_04 == mp_carr) + 
                                                                          (COD_CARRERA_PREF_05 == mp_carr) + (COD_CARRERA_PREF_06 == mp_carr) + 
                                                                          (COD_CARRERA_PREF_07 == mp_carr) + (COD_CARRERA_PREF_08 == mp_carr) + 
                                                                          (COD_CARRERA_PREF_09 == mp_carr) + (COD_CARRERA_PREF_10 == mp_carr)) == 1, 1, 0)))

# Add cutoffs for all programs and also for true preference
## Transform panel of admission results from wide to long
d_2021_l <- reshape(d_2021[,c(1,4:53)], direction='long', 
                    varying=colnames(d_2021[,4:53]), 
                    times=c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10'),
                    v.names=c('COD_CARRERA_PREF', 'ESTADO_PREF','PTJE_PREF', 'LUGAR_PREF', 'POND_ACAD_PREF'),
                    idvar='ID')
colnames(d_2021_l) <- c("id", "pref", "codigo_carrera", "marca", "orden", "puntano", "puntpond") # this is just me not knowing how to use reshape
d_2021_l$puntpond <- ifelse(d_2021_l$puntpond > 10000, d_2021_l$puntpond/100, ifelse(d_2021_l$puntpond > 1000, d_2021_l$puntpond/10, d_2021_l$puntpond))


## Compute cutoffs
cutoffs_2021 <- d_2021_l %>% filter(marca == 24) %>% group_by(codigo_carrera) %>% dplyr::summarise(cutoff = min(puntpond)) %>% ungroup %>% as.data.frame()

## Merge with data
for (p in 1:10){
  sp = paste("0",as.character(p), sep="")
  if(p==10){
    sp = "10"
  }
  col = paste('COD_CARRERA_PREF_', sp, sep='')
  
  dfs_2021[, paste("COD_CARRERA_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2021[, paste("COD_CARRERA_PREF_", sp, sep="")]), 0, dfs_2021[, paste("COD_CARRERA_PREF_", sp, sep="")])
  dfs_2021[, paste("ESTADO_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2021[, paste("ESTADO_PREF_", sp, sep="")]), 0, dfs_2021[, paste("ESTADO_PREF_", sp, sep="")])
  dfs_2021[, paste("PTJE_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2021[, paste("PTJE_PREF_", sp, sep="")]), 0, dfs_2021[, paste("PTJE_PREF_", sp, sep="")])
  
  dfs_2021[, paste("PTJE_PREF_", sp, sep="")] <- ifelse( dfs_2021[, paste("PTJE_PREF_", sp, sep="")] > 10000, dfs_2021[, paste("PTJE_PREF_", sp, sep="")]/100, ifelse( dfs_2021[, paste("PTJE_PREF_", sp, sep="")] > 1000, dfs_2021[, paste("PTJE_PREF_", sp, sep="")]/10, dfs_2021[, paste("PTJE_PREF_", sp, sep="")]  )  )
  
  dfs_2021[, paste("LUGAR_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2021[, paste("LUGAR_PREF_", sp, sep="")]), 0, dfs_2021[, paste("LUGAR_PREF_", sp, sep="")])
  dfs_2021[, paste("POND_ACAD_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2021[, paste("POND_ACAD_PREF_", sp, sep="")]), 0, dfs_2021[, paste("POND_ACAD_PREF_", sp, sep="")])
  
  dfs_2021 <- merge(dfs_2021, cutoffs_2021, by.x=col, by.y="codigo_carrera", all.x=TRUE)
  
  dfs_2021$cutoff[is.na(dfs_2021$cutoff)] <- 0
  names(dfs_2021)[names(dfs_2021) == "cutoff"] <- paste("cutoff_pref_", sp, sep="")
  
  dfs_2021[, paste("distance_", sp, sep="")] <- dfs_2021[, paste("PTJE_PREF_", sp, sep="")] - dfs_2021[, paste("cutoff_pref_", sp, sep="")]
  
  # in new survey we do not have beliefs for each preference
  # col_b = paste('cutoff_', as.character(p), sep='')
  # col_c = paste('cutoff_pref_', sp, sep='')
  # new_col = paste('distance_belief_cutoff_', sp, sep='')
  # 
  # dfs_2021[is.na(dfs_2021[,col_b]),col_b] <- 0
  # dfs_2021[,new_col] = dfs_2021[,col_c]-dfs_2021[,col_b]  
  
}

dfs_2021 <- merge(dfs_2021, cutoffs_2021, by.x="mp_carr", by.y="codigo_carrera", all.x=TRUE)
names(dfs_2021)[names(dfs_2021) == "cutoff"] <- "cutoff_mp"
dfs_2021$cutoff_mp[is.na(dfs_2021$cutoff_mp)] <- 0
dfs_2021$pp_mp[is.na(dfs_2021$pp_mp)] <- 0
dfs_2021$distance_mp <- dfs_2021$pp_mp - dfs_2021$cutoff_mp

dfs_2021 %<>% mutate(some_mistake = ifelse(number_apps > valid_apps, 1, 0), all_mistakes = ifelse(number_apps > 0 & valid_apps == 0, 1, 0), 
                     is_female = ifelse(SEXO==2, 1, 0), 
                     PROMEDIO_LM_ACTUAL = ifelse(PROMEDIO_LM_ACTUAL>10000, PROMEDIO_LM_ACTUAL/100, ifelse(PROMEDIO_LM_ACTUAL>1000, PROMEDIO_LM_ACTUAL/10, PROMEDIO_LM_ACTUAL)),
                     PROMEDIO_LM_ANTERIOR = ifelse(PROMEDIO_LM_ANTERIOR>10000, PROMEDIO_LM_ANTERIOR/100, ifelse(PROMEDIO_LM_ANTERIOR>1000, PROMEDIO_LM_ANTERIOR/10, PROMEDIO_LM_ANTERIOR)))

# saveRDS(dfs_2021, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_and_survey_2021_20210420.rds", sep=''))
saveRDS(dfs_2021, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_and_survey_2021_20210812.rds", sep=''))
saveRDS(dfs_2021, paste(dropbox_dir, "Mistakes in college admissions/data/interim/bcd_and_survey_2021.rds", sep=''))


##################### Merge with MRUN ###################################

dfs_2021 <- readRDS(paste(dropbox_dir, "Mistakes in college admissions/data/interim/bcd_and_survey_2021.rds", sep=''))

library(tidyverse)
library(haven)
b_mrun <- read_dta(file = paste(dropbox_dir, "Mistakes Structural/Data/2021/ABCD 2021 with MRUN/archivo_B_innominado.dta", sep = "")) 
c_mrun <- read_dta(file = paste(dropbox_dir, "Mistakes Structural/Data/2021/ABCD 2021 with MRUN/archivo_C_innominado.dta", sep = "")) 
d_mrun <- read_dta(file = paste(dropbox_dir, "Mistakes Structural/Data/2021/ABCD 2021 with MRUN/archivo_D_innominado.dta", sep = "")) 

df_mrun <- merge(b_mrun, c_mrun[,c("mrun",setdiff(colnames(c_mrun), colnames(b_mrun)))], by="mrun", all.x=TRUE)
df_mrun <- merge(df_mrun, d_mrun[,c("mrun",setdiff(colnames(d_mrun), colnames(df_mrun)))], by="mrun", all.x=TRUE)

df_mrun %<>% dplyr::rename(promedio_lm_actual = promedio_cm_actual, leng_actual = clec_actual)

df_mrun$promedio_lm_actual <- str_replace(df_mrun$promedio_lm_actual, ",", "")
df_mrun$promedio_lm_actual <- as.numeric(df_mrun$promedio_lm_actual)

df_mrun$promedio_lm_anterior <- str_replace(df_mrun$promedio_lm_anterior, ",", "")
df_mrun$promedio_lm_anterior <- as.numeric(df_mrun$promedio_lm_anterior)

colnames(dfs_2021)
colnames(df_mrun)

vars <- c("RBD", "SEXO", "PTJE_NEM", "PTJE_RANKING",
          "COD_CARRERA_PREF_01", "COD_CARRERA_PREF_02", "COD_CARRERA_PREF_03", "COD_CARRERA_PREF_04", "COD_CARRERA_PREF_05", 
          "COD_CARRERA_PREF_06", "COD_CARRERA_PREF_07", "COD_CARRERA_PREF_08", "COD_CARRERA_PREF_09", "COD_CARRERA_PREF_10")

d1 <- dfs_2021[, c("id", vars)]
d2 <- df_mrun[,c("mrun",tolower(vars))]

d1[is.na(d1)] <- 0
d2[is.na(d2)] <- 0

d1 %<>% filter(COD_CARRERA_PREF_01 > 0)
d2 %<>% filter(cod_carrera_pref_01 > 0)

d <- merge(d1 %>% filter(COD_CARRERA_PREF_01 > 0), d2 %>% filter(cod_carrera_pref_01 > 0), by.x=vars, by.y = tolower(vars))

saveRDS(d, paste(dropbox_dir, "Mistakes in college admissions/data/interim/mapping_mrun_hash_2021.rds", sep=''))
saveRDS(d, paste(dropbox_dir, "Mistakes Structural/Data/Data_from_Mistakes_1/interim/mapping_mrun_hash_2021.rds", sep=''))


##################### Merge with GRADES 2015 ###################################
# Merge grade data 2015
dropbox_dir = "~/Dropbox/"
da <- readRDS("/Users/iriosu/Downloads/data.rds")
inpsu <- readRDS(paste(dropbox_dir, "Dropout_matching/Intermediate_data/ins_psu_and_enr_and_grades_with_vars_2015.Rda", sep=''))
# gdf <- readRDS(file = paste(input_path, "Intermediate_data/data_for_momentos_with_grades_2015.rds", sep = ""))

vars_1 = c("COD_SEXO_2015", "RBD_2015", "PTJE_NEM_2015", 
           "CODIGO_CARRERA_1_2015", "CODIGO_CARRERA_2_2015", "CODIGO_CARRERA_3_2015", "CODIGO_CARRERA_4_2015", "CODIGO_CARRERA_5_2015", 
           "CODIGO_CARRERA_6_2015", "CODIGO_CARRERA_7_2015", "CODIGO_CARRERA_8_2015", "CODIGO_CARRERA_9_2015", "CODIGO_CARRERA_10_2015")

vars_2 <- c("sexo", "rbd", "nem", 
            "codigo0", "codigo1", "codigo2", "codigo3", "codigo4", "codigo5", "codigo6", "codigo7", "codigo8", "codigo9")

d1 <- inpsu[,vars_1]
d2 <- da[,vars_2]

d1 <- mutate_all(d1, function(x) as.numeric(as.character(x)))
d2 <- mutate_all(d2, function(x) as.numeric(as.character(x)))

d1[is.na(d1)] <- 0
d2[is.na(d2)] <- 0

d1 %<>% filter(CODIGO_CARRERA_1_2015 > 0)
d2 %<>% filter(codigo0 > 0)

d2$id <- seq(1:nrow(d2))

d <- merge(d1, d2, by.x=vars_1, by.y=vars_2)


nrow(d)
nrow(d1)
nrow(d2)

d2 %>% filter(!(id %in% d$id)) 

d1 %>% filter(RBD_2015 == 1675 ) %>% dplyr::select(COD_SEXO_2015, PTJE_NEM_2015, CODIGO_CARRERA_1_2015, CODIGO_CARRERA_2_2015, CODIGO_CARRERA_3_2015) %>% arrange(PTJE_NEM_2015)

colnames(da)
colnames(gdf)



# --------- 2014 --------------
load(file = paste(dropbox_dir, "Dropout_matching/Intermediate_data/ins_psu_and_enr_with_vars_2014.Rda", sep=''))
inpsu_14 <- ins_psu_and_enr
colnames(ins_psu_and_enr)
vars_3 = c("COD_SEXO_2015", "RBD_2015", "PTJE_NEM_2015", 
           "CODIGO_CARRERA_1_2015", "CODIGO_CARRERA_2_2015", "CODIGO_CARRERA_3_2015", "CODIGO_CARRERA_4_2015", "CODIGO_CARRERA_5_2015", 
           "CODIGO_CARRERA_6_2015", "CODIGO_CARRERA_7_2015", "CODIGO_CARRERA_8_2015", "CODIGO_CARRERA_9_2015", "CODIGO_CARRERA_10_2015")

d3 <- ins_psu_and_enr[,vars_3]
d3 <- mutate_all(d3, function(x) as.numeric(as.character(x)))
d3[is.na(d3)] <- 0
d3 %<>% filter(CODIGO_CARRERA_1_2015 > 0)

dp <- merge(d3, d2, by.x=vars_3, by.y=vars_2)

# --------- 2013 --------------
load(file = paste(dropbox_dir, "Dropout_matching/Intermediate_data/ins_psu_and_enr_with_vars_2013.Rda", sep=''))
inpsu_13 <- ins_psu_and_enr
vars_3 = c("COD_SEXO_2015", "RBD_2015", "PTJE_NEM_2015", 
           "CODIGO_CARRERA_1_2015", "CODIGO_CARRERA_2_2015", "CODIGO_CARRERA_3_2015", "CODIGO_CARRERA_4_2015", "CODIGO_CARRERA_5_2015", 
           "CODIGO_CARRERA_6_2015", "CODIGO_CARRERA_7_2015", "CODIGO_CARRERA_8_2015", "CODIGO_CARRERA_9_2015", "CODIGO_CARRERA_10_2015")

d3 <- ins_psu_and_enr[,vars_3]
d3 <- mutate_all(d3, function(x) as.numeric(as.character(x)))
d3[is.na(d3)] <- 0
d3 %<>% filter(CODIGO_CARRERA_1_2015 > 0)

dp <- merge(d3, d2, by.x=vars_3, by.y=vars_2)

inpsu_14 <- ins_psu_and_enr

length(union(d$id, dp$id))

colnames(inpsu)
colnames(ins_psu_and_enr)

setdiff(ins_psu_and_enr %>% filter(CODIGO_CARRERA_1_2015 > 0) %>% dplyr::select(MRUN), inpsu %>% filter(CODIGO_CARRERA_1_2015 > 0) %>% dplyr::select(MRUN))

ins_psu_and_enr %>% filter(CODIGO_CARRERA_1_2015 > 0) %>% tally()

nrow(intersect(ins_psu_and_enr %>% dplyr::select(MRUN), inpsu %>% dplyr::select(MRUN)))

da %>% group_by(agno_egreso) %>% tally() %>% arrange(-n)

load(file = paste(dropbox_dir, "Dropout_matching/Intermediate_data/ins_psu_and_enr_panel_2012.Rda", sep=''))

colnames(ins_psu_and_enr_panel)


##################### Merge with GRADES 2014 ###################################
# Merge grade data 2015
dropbox_dir = "~/Dropbox/"
da <- readRDS("/Users/IgnacioRios/Downloads/data2014.rds")
load(file = paste(dropbox_dir, "Dropout_matching/Intermediate_data/ins_psu_and_enr_with_vars_2014.Rda", sep=''))
inpsu <- ins_psu_and_enr
# gdf <- readRDS(file = paste(input_path, "Intermediate_data/data_for_momentos_with_grades_2015.rds", sep = ""))

vars_1 = c("COD_SEXO_2014", "RBD_2014", "PTJE_NEM_2014", 
           "CODIGO_CARRERA_1_2014", "CODIGO_CARRERA_2_2014", "CODIGO_CARRERA_3_2014", "CODIGO_CARRERA_4_2014", "CODIGO_CARRERA_5_2014", 
           "CODIGO_CARRERA_6_2014", "CODIGO_CARRERA_7_2014", "CODIGO_CARRERA_8_2014", "CODIGO_CARRERA_9_2014", "CODIGO_CARRERA_10_2014")

vars_2 <- c("sexo", "rbd", "nem", 
            "post1", "post2", "post3", "post4", "post5", "post6", "post7", "post8", "post9", "post10")

vars_3 <- c("agno_ing_1año", "cod_carrera_univ_1año", "cod_carrera_demre_1año", "ingreso_1año", "sit_academica_1año", "p_acum_anual_2014_1año", 
            "agno_ing_2año", "cod_carrera_univ_2año", "cod_carrera_demre_2año", "ingreso_2año", "sit_academica_2año", "p_acum_anual_2015BO_2año", 
            "agno_ing_3año", "cod_carrera_univ_3año", "cod_carrera_demre_3año", "ingreso_3año", "sit_academica_3año", "p_acum_anual_3año")

d1 <- inpsu[,vars_1]
d2 <- da[,c(vars_2, vars_3)]

d1 <- mutate_all(d1, function(x) as.numeric(as.character(x)))
d2 <- mutate_all(d2, function(x) as.numeric(as.character(x)))

d1[is.na(d1)] <- 0
d2[is.na(d2)] <- 0

d1 %<>% filter(CODIGO_CARRERA_1_2014 > 0)
d2 %<>% filter(post1 > 0)

d2$id <- seq(1:nrow(d2))

d <- merge(d1, d2, by.x=vars_1, by.y=vars_2)




