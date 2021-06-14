# Swithing and dropout regressions and odds ratios test if iriosu can make
# changes
rm(list = ls())
# Libraries ---------------------------------------------------------------

dropbox_dir = "~/Dropbox/"

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


##################### Panel of cutoffs #######################
load(paste(dropbox_dir, "DEMRE - Datos/ArchivosABCDRemappedAEncuestas/2018/D_202011302111.rdata", sep = ""))
load(paste(dropbox_dir, "Dropout_matching/Intermediate_data/data-giorgio/2019/D_201902201847.rdata", sep = ""))
load(paste(dropbox_dir, "Mistakes in college admissions/mistakes_and_warnings/data/ABCD/2020/D_202105150226.rdata", sep = ""))

d_2018 %<>% filter(!is.na(ID) & !is.na(ANYO_PROCESO)) %>% mutate(ANYO_PROCESO = 2018) %>% as.data.frame()
d_2019 %<>% filter(!is.na(ID) & !is.na(ANYO_PROCESO)) %>% mutate(ANYO_PROCESO = 2019) %>% as.data.frame()
d_2020 %<>% filter(!is.na(ID) & !is.na(ANYO_PROCESO)) %>% mutate(ANYO_PROCESO = 2020) %>% as.data.frame()

d_2018$COD_CARRERA_PREF_01 <- as.numeric(as.character(d_2018$COD_CARRERA_PREF_01))

d_2018$ID <- as.character(d_2018$ID)
d_2019$ID <- as.character(d_2019$ID)
d_2020$ID <- as.character(d_2020$ID)

d_18_20 <- rbind(d_2018[1:54], d_2019[1:54], d_2020[1:54])
d_18_20$IDY = paste(d_18_20$ID, d_18_20$ANYO_PROCESO, sep="_")

## 2018
d_2018_l <- reshape(d_2018[,c(1,4:53)], direction='long', 
                    varying=colnames(d_2018[,4:53]), 
                    times=c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10'),
                    v.names=c('COD_CARRERA_PREF', 'ESTADO_PREF','PTJE_PREF', 'LUGAR_PREF', 'POND_ACAD_PREF'),
                    idvar='ID')
colnames(d_2018_l) <- c("id", "pref", "codigo_carrera", "marca", "orden", "puntano", "puntpond") # this is just me not knowing how to use reshape
d_2018_l$puntpond <- ifelse(d_2018_l$puntpond > 10000, d_2018_l$puntpond/100, ifelse(d_2018_l$puntpond > 1000, d_2018_l$puntpond/10, d_2018_l$puntpond))
c18 <- d_2018_l %>% filter(marca == 24) %>% group_by(codigo_carrera) %>% dplyr::summarise(cutoff = min(puntpond)) %>% ungroup %>% as.data.frame()

## 2019
d_2019_l <- reshape(d_2019[,c(1,4:53)], direction='long', 
                    varying=colnames(d_2019[,4:53]), 
                    times=c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10'),
                    v.names=c('COD_CARRERA_PREF', 'ESTADO_PREF','PTJE_PREF', 'LUGAR_PREF', 'POND_ACAD_PREF'),
                    idvar='ID')
colnames(d_2019_l) <- c("id", "pref", "codigo_carrera", "marca", "orden", "puntano", "puntpond") # this is just me not knowing how to use reshape
d_2019_l$puntpond <- ifelse(d_2019_l$puntpond > 10000, d_2019_l$puntpond/100, ifelse(d_2019_l$puntpond > 1000, d_2019_l$puntpond/10, d_2019_l$puntpond))
c19 <- d_2019_l %>% filter(marca == 24) %>% group_by(codigo_carrera) %>% dplyr::summarise(cutoff = min(puntpond)) %>% ungroup %>% as.data.frame()

## 2020
d_2020_l <- reshape(d_2020[,c(1,4:53)], direction='long', 
                    varying=colnames(d_2020[,4:53]), 
                    times=c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10'),
                    v.names=c('COD_CARRERA_PREF', 'ESTADO_PREF','PTJE_PREF', 'LUGAR_PREF', 'POND_ACAD_PREF'),
                    idvar='ID')
colnames(d_2020_l) <- c("id", "pref", "codigo_carrera", "marca", "orden", "puntano", "puntpond") # this is just me not knowing how to use reshape
d_2020_l$puntpond <- ifelse(d_2020_l$puntpond > 10000, d_2020_l$puntpond/100, ifelse(d_2020_l$puntpond > 1000, d_2020_l$puntpond/10, d_2020_l$puntpond))
c20 <- d_2020_l %>% filter(marca == 24) %>% group_by(codigo_carrera) %>% dplyr::summarise(cutoff = min(puntpond)) %>% ungroup %>% as.data.frame()

# d_18_20_l <- reshape(d_18_20, direction='long', 
#                     varying=colnames(d_18_20[,4:53]), 
#                     times=c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10'),
#                     v.names=c('COD_CARRERA_PREF', 'ESTADO_PREF','PTJE_PREF', 'LUGAR_PREF', 'POND_ACAD_PREF'),
#                     idvar='IDY')
# 
# colnames(d_18_20_l) <- c("id", "year", "situation", "bea", "idy", "pref", "codigo_carrera", "marca", "orden", "puntano", "puntpond") 
# 
# c18 <- d_18_20_l %>% filter(year == 2018 & marca == 24) %>% group_by(codigo_carrera) %>% dplyr::summarise(cutoff_2018 = min(puntpond)) %>% mutate(cutoff_2018 = ifelse(cutoff_2018 > 10000, cutoff_2018/100, ifelse(cutoff_2018 > 1000, cutoff_2018/10, cutoff_2018)))
# c19 <- d_18_20_l %>% filter(year == 2019 & marca == 24) %>% group_by(codigo_carrera) %>% dplyr::summarise(cutoff_2019 = min(puntpond)) %>% mutate(cutoff_2019 = ifelse(cutoff_2019 > 10000, cutoff_2019/100, ifelse(cutoff_2019 > 1000, cutoff_2019/10, cutoff_2019)))
# c20 <- d_18_20_l %>% filter(year == 2020 & marca == 24) %>% group_by(codigo_carrera) %>% dplyr::summarise(cutoff_2020 = min(puntpond)) %>% mutate(cutoff_2020 = ifelse(cutoff_2020 > 10000, cutoff_2020/100, ifelse(cutoff_2020 > 1000, cutoff_2020/10, cutoff_2020)))

c18$codigo_carrera <- as.character(c18$codigo_carrera)
c19$codigo_carrera <- as.character(c19$codigo_carrera)
c20$codigo_carrera <- as.character(c20$codigo_carrera)

c <- merge(c18, c19, by="codigo_carrera", all=TRUE)
c <- merge(c, c20, by="codigo_carrera", all=TRUE)
c$codigo_carrera <- as.numeric(as.character(c$codigo_carrera))
colnames(c) <- c("codigo_carrera", "cutoff_2018", "cutoff_2019", "cutoff_2020")

add_zero <- function(in_num){
  if(in_num < 10000){
    p1 <- substr(as.character(in_num),1,2)
    p2 <- substr(as.character(in_num),3,4)
    p1 <- as.numeric(p1)
    p2 <- as.numeric(p2)
    return(p1*1000+p2)
  }
  else{
    return(in_num)
  }
}

for(yr in 2004:2017){
  print(yr)
  aux <- read.csv(file=paste(dropbox_dir, "DEMRE - Datos/Solicitud DEMRE-MINEDUC 2018/PSU/Postulaciones/C_POSTULACIONES_SELECCION_PSU_",yr,"_PRIV_MRUN.csv",sep=""), sep = ";", header=TRUE, fileEncoding = "Latin1")
  aux$PUNTAJE <- as.numeric(gsub(",",".",as.character(aux$PUNTAJE)))
  aux$PUNTAJE <- ifelse(aux$PUNTAJE > 10000, aux$PUNTAJE/100, ifelse(aux$PUNTAJE > 1000, aux$PUNTAJE/10, aux$PUNTAJE))
  aux$CODIGO_CARRERA <- as.numeric(as.character(aux$CODIGO_CARRERA))
  cr <- aux %>% filter(ESTADO_PREFERENCIA==24) %>% group_by(CODIGO_CARRERA) %>% dplyr::summarise(cutoff=min(PUNTAJE))
  colnames(cr) <- c("codigo_carrera", paste("cutoff_",yr,sep=""))
  cr <- cr %>% mutate(codigo_carrera = add_zero(codigo_carrera))
  c <- merge(c, cr, by="codigo_carrera", all=TRUE)
}
colnames(c)
cutoffs <- c[,c("codigo_carrera", "cutoff_2004", "cutoff_2005", "cutoff_2006", "cutoff_2007", "cutoff_2008", "cutoff_2009", "cutoff_2010", 
          "cutoff_2011", "cutoff_2012", "cutoff_2013", "cutoff_2014", "cutoff_2015", "cutoff_2016", "cutoff_2017", "cutoff_2018", "cutoff_2019", "cutoff_2020")]

# saveRDS(cutoffs, paste(dropbox_dir, "Mistakes/Data/intermediate_data/panel_cutoffs_2004_2020_20210420.rds", sep=''))
saveRDS(cutoffs, paste(dropbox_dir, "Mistakes/Data/intermediate_data/panel_cutoffs_2004_2020_20210609.rds", sep=''))

##################### 2019 ###################################

# Consolidate files BCD 2019
load(paste(dropbox_dir, "Dropout_matching/Intermediate_data/data-giorgio/2019/B_201902201847.rdata", sep=''))
load(paste(dropbox_dir, "Dropout_matching/Intermediate_data/data-giorgio/2019/C_201902201847.rdata", sep=''))
load(paste(dropbox_dir, "Dropout_matching/Intermediate_data/data-giorgio/2019/D_201902201847.rdata", sep = ""))

b_2019$id <- as.character(b_2019$ID)
c_2019$id <- as.character(c_2019$ID)
d_2019$id <- as.character(d_2019$ID)

d_2019 %<>% mutate(applied_2019 = ifelse(is.na(COD_CARRERA_PREF_01), 0, 1), 
                   assigned_2019 = ifelse((ESTADO_PREF_01==24) + (ESTADO_PREF_02==24) + (ESTADO_PREF_03==24) + (ESTADO_PREF_04==24) + (ESTADO_PREF_05==24) +
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


df_2019 <- merge(b_2019, c_2019[,c("id",setdiff(colnames(c_2019), colnames(b_2019)))], by="id", all.x=TRUE)
df_2019 <- merge(df_2019, d_2019[,c("id",setdiff(colnames(d_2019), colnames(df_2019)))], by="id", all.x=TRUE)

# saveRDS(df_2019, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_2019_20210420.rds", sep=''))
saveRDS(df_2019, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_2019_20210609.rds", sep=''))

# Load survey 2019, merge and compute score in top-true pref
survey_2019 <- read.csv(paste(dropbox_dir, "Mistakes in college admissions/data/clean/survey-responses-2019.csv", sep=''), header=TRUE)
car19 <- read.csv(file = paste(dropbox_dir, "Dropout_matching/Intermediate_data/data-giorgio/oferta2019_201812021829.csv", sep = "") , header=TRUE, sep=';')

for(col in colnames(car19)){
  names(car19)[names(car19) == col] <- paste(col, "mp", sep="_")
}

dfs_2019 <- merge(df_2019, survey_2019, by="id", all.x=TRUE)
dfs_2019 <- merge(dfs_2019, car19, by.x="mp_carr", by.y="CODIGO_mp", all.x=TRUE)

dfs_2019 %<>% mutate(psu_actual_mp = LENG_ACTUAL*PCT_LENG_mp + MATE_ACTUAL*PCT_MATE_mp + 
                                      ifelse(ifelse(!is.na(CIEN_ACTUAL) & !is.na(PCT_CIEN_mp), CIEN_ACTUAL*PCT_CIEN_mp, 0) > ifelse(!is.na(HCSO_ACTUAL) & !is.na(PCT_HCSO_mp), HCSO_ACTUAL*PCT_HCSO_mp, 0), ifelse(!is.na(CIEN_ACTUAL) & !is.na(PCT_CIEN_mp), CIEN_ACTUAL*PCT_CIEN_mp, 0), ifelse(!is.na(HCSO_ACTUAL) & !is.na(PCT_HCSO_mp), HCSO_ACTUAL*PCT_HCSO_mp, 0)), 
                psu_anterior_mp = LENG_ANTERIOR*PCT_LENG_mp + MATE_ANTERIOR*PCT_MATE_mp + 
                                      ifelse(ifelse(!is.na(CIEN_ANTERIOR) & !is.na(PCT_CIEN_mp), CIEN_ANTERIOR*PCT_CIEN_mp, 0) > ifelse(!is.na(HCSO_ANTERIOR) & !is.na(PCT_HCSO_mp), HCSO_ANTERIOR*PCT_HCSO_mp, 0), ifelse(!is.na(CIEN_ANTERIOR) & !is.na(PCT_CIEN_mp), CIEN_ANTERIOR*PCT_CIEN_mp, 0), ifelse(!is.na(HCSO_ANTERIOR) & !is.na(PCT_HCSO_mp), HCSO_ANTERIOR*PCT_HCSO_mp, 0)))

dfs_2019 %<>% mutate(anterior_best_mp = ifelse(!is.na(psu_anterior_mp), ifelse(psu_anterior_mp > psu_actual_mp, 1, 0), 0))

dfs_2019 %<>%  mutate(pp_mp = ifelse(PTJE_NEM == 0, 
                                  ifelse(anterior_best_mp == 0, psu_actual_mp/(100-PCT_NOTAS_mp-PCT_RANKING_mp), psu_anterior_mp/(100-PCT_NOTAS_mp-PCT_RANKING_mp)),
                                  (PTJE_RANKING*PCT_RANKING_mp + PTJE_NEM*PCT_NOTAS_mp)/100 + ifelse(anterior_best_mp == 0, psu_actual_mp/(100), psu_anterior_mp/(100))))


dfs_2019 %<>% mutate(applied_to_mp = ifelse(is.na(mp_carr), 0, ifelse( ((COD_CARRERA_PREF_01 == mp_carr) + (COD_CARRERA_PREF_02 == mp_carr) + 
                                               (COD_CARRERA_PREF_03 == mp_carr) + (COD_CARRERA_PREF_04 == mp_carr) + 
                                               (COD_CARRERA_PREF_05 == mp_carr) + (COD_CARRERA_PREF_06 == mp_carr) + 
                                               (COD_CARRERA_PREF_07 == mp_carr) + (COD_CARRERA_PREF_08 == mp_carr) + 
                                               (COD_CARRERA_PREF_09 == mp_carr) + (COD_CARRERA_PREF_10 == mp_carr)) == 1, 1, 0)))

# Add cutoffs for all programs and also for true preference
## Transform panel from wide to long
d_2019_l <- reshape(d_2019[,c(1,4:53)], direction='long', 
                    varying=colnames(d_2019[,4:53]), 
                    times=c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10'),
                    v.names=c('COD_CARRERA_PREF', 'ESTADO_PREF','PTJE_PREF', 'LUGAR_PREF', 'POND_ACAD_PREF'),
                    idvar='ID')
colnames(d_2019_l) <- c("id", "pref", "codigo_carrera", "marca", "orden", "puntano", "puntpond") # this is just me not knowing how to use reshape
d_2019_l$puntpond <- ifelse(d_2019_l$puntpond > 10000, d_2019_l$puntpond/100, ifelse(d_2019_l$puntpond > 1000, d_2019_l$puntpond/10, d_2019_l$puntpond))

## Compute cutoffs
cutoffs_2019 <- d_2019_l %>% filter(marca == 24) %>% group_by(codigo_carrera) %>% dplyr::summarise(cutoff = min(puntpond)) %>% ungroup %>% as.data.frame()

## Merge with data
for (p in 1:10){
  sp = paste("0",as.character(p), sep="")
  if(p==10){
    sp = "10"
  }
  col = paste('COD_CARRERA_PREF_', sp, sep='')
  
  dfs_2019[, paste("COD_CARRERA_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2019[, paste("COD_CARRERA_PREF_", sp, sep="")]), 0, dfs_2019[, paste("COD_CARRERA_PREF_", sp, sep="")])
  dfs_2019[, paste("ESTADO_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2019[, paste("ESTADO_PREF_", sp, sep="")]), 0, dfs_2019[, paste("ESTADO_PREF_", sp, sep="")])
  dfs_2019[, paste("PTJE_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2019[, paste("PTJE_PREF_", sp, sep="")]), 0, dfs_2019[, paste("PTJE_PREF_", sp, sep="")])
  
  dfs_2019[, paste("PTJE_PREF_", sp, sep="")] <- ifelse( dfs_2019[, paste("PTJE_PREF_", sp, sep="")] > 10000, dfs_2019[, paste("PTJE_PREF_", sp, sep="")]/100, ifelse( dfs_2019[, paste("PTJE_PREF_", sp, sep="")] > 1000, dfs_2019[, paste("PTJE_PREF_", sp, sep="")]/10, dfs_2019[, paste("PTJE_PREF_", sp, sep="")]  )  )
  
  dfs_2019[, paste("LUGAR_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2019[, paste("LUGAR_PREF_", sp, sep="")]), 0, dfs_2019[, paste("LUGAR_PREF_", sp, sep="")])
  dfs_2019[, paste("POND_ACAD_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2019[, paste("POND_ACAD_PREF_", sp, sep="")]), 0, dfs_2019[, paste("POND_ACAD_PREF_", sp, sep="")])
  
  dfs_2019 <- merge(dfs_2019, cutoffs_2019, by.x=col, by.y="codigo_carrera", all.x=TRUE)
  
  dfs_2019$cutoff[is.na(dfs_2019$cutoff)] <- 0
  names(dfs_2019)[names(dfs_2019) == "cutoff"] <- paste("cutoff_pref_", sp, sep="")
  
  dfs_2019[, paste("distance_", sp, sep="")] <- dfs_2019[, paste("PTJE_PREF_", sp, sep="")] - dfs_2019[, paste("cutoff_pref_", sp, sep="")]
  

  col_b = paste('cutoff_', as.character(p), sep='')
  col_c = paste('cutoff_pref_', sp, sep='')
  new_col = paste('distance_belief_cutoff_', sp, sep='')
  
  dfs_2019[is.na(dfs_2019[,col_b]),col_b] <- 0
  dfs_2019[,new_col] = dfs_2019[,col_c]-dfs_2019[,col_b]  
  
}

dfs_2019 <- merge(dfs_2019, cutoffs_2019, by.x="mp_carr", by.y="codigo_carrera", all.x=TRUE)
names(dfs_2019)[names(dfs_2019) == "cutoff"] <- "cutoff_mp"
dfs_2019$cutoff_mp[is.na(dfs_2019$cutoff_mp)] <- 0
dfs_2019$pp_mp[is.na(dfs_2019$pp_mp)] <- 0
dfs_2019$distance_mp <- dfs_2019$pp_mp - dfs_2019$cutoff_mp

dfs_2019 %<>% mutate(some_mistake = ifelse(number_apps > valid_apps, 1, 0), all_mistakes = ifelse(number_apps > 0 & valid_apps == 0, 1, 0), 
                     is_female = ifelse(SEXO==2, 1, 0), 
                     PROMEDIO_LM_ACTUAL = ifelse(PROMEDIO_LM_ACTUAL>10000, PROMEDIO_LM_ACTUAL/100, ifelse(PROMEDIO_LM_ACTUAL>1000, PROMEDIO_LM_ACTUAL/10, PROMEDIO_LM_ACTUAL)),
                     PROMEDIO_LM_ANTERIOR = ifelse(PROMEDIO_LM_ANTERIOR>10000, PROMEDIO_LM_ANTERIOR/100, ifelse(PROMEDIO_LM_ANTERIOR>1000, PROMEDIO_LM_ANTERIOR/10, PROMEDIO_LM_ANTERIOR)))

# saveRDS(dfs_2019, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_and_survey_2019_20210420.rds", sep=''))
saveRDS(dfs_2019, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_and_survey_2019_20210609.rds", sep=''))

##################### 2020 ###################################

survey_2020 <- read.csv(paste(dropbox_dir, "Mistakes in college admissions/data/clean/survey-responses-2020.csv", sep=''), header=TRUE)
survey_2020$ID <- survey_2020$id

load(paste(dropbox_dir, "Mistakes in college admissions/mistakes_and_warnings/data/ABCD/2020/A_202105150226.rdata", sep = ""))
load(paste(dropbox_dir, "Mistakes in college admissions/mistakes_and_warnings/data/ABCD/2020/B_202105150226.rdata", sep = ""))
load(paste(dropbox_dir, "Mistakes in college admissions/mistakes_and_warnings/data/ABCD/2020/C_202105150226.rdata", sep = ""))
load(paste(dropbox_dir, "Mistakes in college admissions/mistakes_and_warnings/data/ABCD/2020/D_202105150226.rdata", sep = ""))

b_2020$id <- as.character(b_2020$ID)
c_2020$id <- as.character(c_2020$ID)
d_2020$id <- as.character(d_2020$ID)
survey_2020$ID <- as.character(survey_2020$ID)

d_2020 %<>% mutate(applied_2020 = ifelse(is.na(COD_CARRERA_PREF_01), 0, 1), 
                   assigned_2020 = ifelse((ESTADO_PREF_01==24) + (ESTADO_PREF_02==24) + (ESTADO_PREF_03==24) + (ESTADO_PREF_04==24) + (ESTADO_PREF_05==24) +
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


df_2020 <- merge(b_2020, c_2020[,c("id",setdiff(colnames(c_2020), colnames(b_2020)))], by="id", all.x=TRUE)
df_2020 <- merge(df_2020, d_2020[,c("id",setdiff(colnames(d_2020), colnames(df_2020)))], by="id", all.x=TRUE)

# saveRDS(df_2020, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_2020_20210420.rds", sep=''))
saveRDS(df_2020, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_2020_20210609.rds", sep=''))

## Add additional information
car20 <- read.csv(file = paste(dropbox_dir, "Mistakes in college admissions/data/raw/implementation/survey-2020/oferta2020_202002292151.csv", sep = "") , header=TRUE, sep=';')

for(col in colnames(car20)){
  names(car20)[names(car20) == col] <- paste(col, "mp", sep="_")
}

dfs_2020 <- merge(df_2020, survey_2020, by="id", all.x=TRUE)
dfs_2020 <- merge(dfs_2020, car20, by.x="mp_carr", by.y="CODIGO_mp", all.x=TRUE)

dfs_2020 %<>% mutate(psu_actual_mp = LENG_ACTUAL*PCT_LENG_mp + MATE_ACTUAL*PCT_MATE_mp + 
                       ifelse(ifelse(!is.na(CIEN_ACTUAL) & !is.na(PCT_CIEN_mp), CIEN_ACTUAL*PCT_CIEN_mp, 0) > ifelse(!is.na(HCSO_ACTUAL) & !is.na(PCT_HCSO_mp), HCSO_ACTUAL*PCT_HCSO_mp, 0), ifelse(!is.na(CIEN_ACTUAL) & !is.na(PCT_CIEN_mp), CIEN_ACTUAL*PCT_CIEN_mp, 0), ifelse(!is.na(HCSO_ACTUAL) & !is.na(PCT_HCSO_mp), HCSO_ACTUAL*PCT_HCSO_mp, 0)), 
                     psu_anterior_mp = LENG_ANTERIOR*PCT_LENG_mp + MATE_ANTERIOR*PCT_MATE_mp + 
                       ifelse(ifelse(!is.na(CIEN_ANTERIOR) & !is.na(PCT_CIEN_mp), CIEN_ANTERIOR*PCT_CIEN_mp, 0) > ifelse(!is.na(HCSO_ANTERIOR) & !is.na(PCT_HCSO_mp), HCSO_ANTERIOR*PCT_HCSO_mp, 0), ifelse(!is.na(CIEN_ANTERIOR) & !is.na(PCT_CIEN_mp), CIEN_ANTERIOR*PCT_CIEN_mp, 0), ifelse(!is.na(HCSO_ANTERIOR) & !is.na(PCT_HCSO_mp), HCSO_ANTERIOR*PCT_HCSO_mp, 0)))

dfs_2020 %<>% mutate(anterior_best_mp = ifelse(!is.na(psu_anterior_mp), ifelse(psu_anterior_mp > psu_actual_mp, 1, 0), 0))

dfs_2020 %<>%  mutate(pp_mp = ifelse(PTJE_NEM == 0, 
                                     ifelse(anterior_best_mp == 0, psu_actual_mp/(100-PCT_NOTAS_mp-PCT_RANKING_mp), psu_anterior_mp/(100-PCT_NOTAS_mp-PCT_RANKING_mp)),
                                     (PTJE_RANKING*PCT_RANKING_mp + PTJE_NEM*PCT_NOTAS_mp)/100 + ifelse(anterior_best_mp == 0, psu_actual_mp/(100), psu_anterior_mp/(100))))


dfs_2020 %<>% mutate(applied_to_mp = ifelse(is.na(mp_carr), 0, ifelse( ((COD_CARRERA_PREF_01 == mp_carr) + (COD_CARRERA_PREF_02 == mp_carr) + 
                                                                          (COD_CARRERA_PREF_03 == mp_carr) + (COD_CARRERA_PREF_04 == mp_carr) + 
                                                                          (COD_CARRERA_PREF_05 == mp_carr) + (COD_CARRERA_PREF_06 == mp_carr) + 
                                                                          (COD_CARRERA_PREF_07 == mp_carr) + (COD_CARRERA_PREF_08 == mp_carr) + 
                                                                          (COD_CARRERA_PREF_09 == mp_carr) + (COD_CARRERA_PREF_10 == mp_carr)) == 1, 1, 0)))


# Add cutoffs for all programs and also for true preference
## Transform panel of admission results from wide to long
d_2020_l <- reshape(d_2020[,c(1,4:53)], direction='long', 
                    varying=colnames(d_2020[,4:53]), 
                    times=c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10'),
                    v.names=c('COD_CARRERA_PREF', 'ESTADO_PREF','PTJE_PREF', 'LUGAR_PREF', 'POND_ACAD_PREF'),
                    idvar='ID')
colnames(d_2020_l) <- c("id", "pref", "codigo_carrera", "marca", "orden", "puntano", "puntpond") # this is just me not knowing how to use reshape
d_2020_l$puntpond <- ifelse(d_2020_l$puntpond > 10000, d_2020_l$puntpond/100, ifelse(d_2020_l$puntpond > 1000, d_2020_l$puntpond/10, d_2020_l$puntpond))


## Compute cutoffs
cutoffs_2020 <- d_2020_l %>% filter(marca == 24) %>% group_by(codigo_carrera) %>% dplyr::summarise(cutoff = min(puntpond)) %>% ungroup %>% as.data.frame()

## Merge with data
for (p in 1:10){
  sp = paste("0",as.character(p), sep="")
  if(p==10){
    sp = "10"
  }
  col = paste('COD_CARRERA_PREF_', sp, sep='')
  
  dfs_2020[, paste("COD_CARRERA_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2020[, paste("COD_CARRERA_PREF_", sp, sep="")]), 0, dfs_2020[, paste("COD_CARRERA_PREF_", sp, sep="")])
  dfs_2020[, paste("ESTADO_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2020[, paste("ESTADO_PREF_", sp, sep="")]), 0, dfs_2020[, paste("ESTADO_PREF_", sp, sep="")])
  dfs_2020[, paste("PTJE_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2020[, paste("PTJE_PREF_", sp, sep="")]), 0, dfs_2020[, paste("PTJE_PREF_", sp, sep="")])
  
  dfs_2020[, paste("PTJE_PREF_", sp, sep="")] <- ifelse( dfs_2020[, paste("PTJE_PREF_", sp, sep="")] > 10000, dfs_2020[, paste("PTJE_PREF_", sp, sep="")]/100, ifelse( dfs_2020[, paste("PTJE_PREF_", sp, sep="")] > 1000, dfs_2020[, paste("PTJE_PREF_", sp, sep="")]/10, dfs_2020[, paste("PTJE_PREF_", sp, sep="")]  )  )
  
  dfs_2020[, paste("LUGAR_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2020[, paste("LUGAR_PREF_", sp, sep="")]), 0, dfs_2020[, paste("LUGAR_PREF_", sp, sep="")])
  dfs_2020[, paste("POND_ACAD_PREF_", sp, sep="")] <- ifelse(is.na(dfs_2020[, paste("POND_ACAD_PREF_", sp, sep="")]), 0, dfs_2020[, paste("POND_ACAD_PREF_", sp, sep="")])
  
  dfs_2020 <- merge(dfs_2020, cutoffs_2020, by.x=col, by.y="codigo_carrera", all.x=TRUE)
  
  dfs_2020$cutoff[is.na(dfs_2020$cutoff)] <- 0
  names(dfs_2020)[names(dfs_2020) == "cutoff"] <- paste("cutoff_pref_", sp, sep="")
  
  dfs_2020[, paste("distance_", sp, sep="")] <- dfs_2020[, paste("PTJE_PREF_", sp, sep="")] - dfs_2020[, paste("cutoff_pref_", sp, sep="")]
  
  
  col_b = paste('cutoff_', as.character(p), sep='')
  col_c = paste('cutoff_pref_', sp, sep='')
  new_col = paste('distance_belief_cutoff_', sp, sep='')
  
  dfs_2020[is.na(dfs_2020[,col_b]),col_b] <- 0
  dfs_2020[,new_col] = dfs_2020[,col_c]-dfs_2020[,col_b]  
  
}

dfs_2020 <- merge(dfs_2020, cutoffs_2020, by.x="mp_carr", by.y="codigo_carrera", all.x=TRUE)
names(dfs_2020)[names(dfs_2020) == "cutoff"] <- "cutoff_mp"
dfs_2020$cutoff_mp[is.na(dfs_2020$cutoff_mp)] <- 0
dfs_2020$pp_mp[is.na(dfs_2020$pp_mp)] <- 0
dfs_2020$distance_mp <- dfs_2020$pp_mp - dfs_2020$cutoff_mp

dfs_2020 %<>% mutate(some_mistake = ifelse(number_apps > valid_apps, 1, 0), all_mistakes = ifelse(number_apps > 0 & valid_apps == 0, 1, 0), 
                     is_female = ifelse(SEXO==2, 1, 0), 
                     PROMEDIO_LM_ACTUAL = ifelse(PROMEDIO_LM_ACTUAL>10000, PROMEDIO_LM_ACTUAL/100, ifelse(PROMEDIO_LM_ACTUAL>1000, PROMEDIO_LM_ACTUAL/10, PROMEDIO_LM_ACTUAL)),
                     PROMEDIO_LM_ANTERIOR = ifelse(PROMEDIO_LM_ANTERIOR>10000, PROMEDIO_LM_ANTERIOR/100, ifelse(PROMEDIO_LM_ANTERIOR>1000, PROMEDIO_LM_ANTERIOR/10, PROMEDIO_LM_ANTERIOR)))

# saveRDS(dfs_2020, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_and_survey_2020_20210420.rds", sep=''))
saveRDS(dfs_2020, paste(dropbox_dir, "Mistakes/Data/intermediate_data/bcd_and_survey_2020_20210609.rds", sep=''))


