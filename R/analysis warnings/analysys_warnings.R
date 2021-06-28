# Analysis warnings
# Remove objects
rm(list = ls())

# Libraries ---------------------------------------------------------------

library(plyr)
library(dplyr)
library(tidyr) 
library(magrittr)
library(parallel)
library(lubridate)
library(stargazer)
library(stringr)
library(ggplot2)
library(anytime)

# Seed --------------------------------------------------------------------

#Set seed
RNGkind("L'Ecuyer-CMRG")
set.seed(1)

# Testing github and Rstudio

# Paths -------------------------------------------------------------------

path_draft_tables = "~/Dropbox/Mistakes Structural/Code/R/analysis warnings/Outputs/Tables/"
path_draft_plots = "~/Dropbox/Mistakes Structural/Code/R/analysis warnings/Outputs/Plots/"
path_intermediate_data = "~/Dropbox/Mistakes Structural/Data/intermediate_data/"

# Load data ---------------------------------------------------------------

# Load applications
#TODO: short description of these files
#TODO: ask Chris to place his Matlab scripts in the repo
# We think this is the list of all students who can participate in the Admission Process
appList <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/AppList.csv', sep=',', header=TRUE)
#TODO: what is exactly Treatment 3?
table(appList$Treatment)
sum(duplicated(appList$mrun))
# TODO: add description of file
appRCT <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/appRCT.csv', sep=',', header=TRUE)
# NOTE: we do not have T3 anymore in this file 
table(appRCT$Treatment)
sum(duplicated(appRCT$mrun))

#TODO: Check with Chris if this file is OK
# We need to see if this is most updated version of the file from Analytics and that the date he used to filter is correct
Cartilla_OpenList <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/Cartilla_OpenList_1415.csv', sep=',', header=FALSE)
appFinal <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/210217 Reporte Postulaciones MRUN v2 CB.csv', sep=',', header=TRUE)
#appFinal <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/210217 Reporte Postulaciones MRUN v2 CB.csv', sep=',', header=TRUE, fileEncoding = "UTF-8-BOM")
UBO_ErrorList <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/UBO_ErrorList.csv', sep=',', header=FALSE)
asignacion_obtenida_reg <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/asignacion_obtenida_reg.csv', sep=';', header=TRUE)
asignacion_obtenida_reg_before <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/asignacion_obtenida_reg_before.csv', sep=';', header=TRUE)
asignacion_obtenida_bea <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/asignacion_obtenida_bea.csv', sep=';', header=TRUE)
asignacion_obtenida_bea_before <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/asignacion_obtenida_bea_before.csv', sep=';', header=TRUE)
bcu <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/ACCESO_BCU-8184.ETAPA2.csv', sep=';', header=TRUE)
#bcu <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/ACCESO_BCU-8184.ETAPA2.csv', sep=';', header=TRUE, fileEncoding = "UTF-8-BOM")

#Save as Rdata
save(appList, file = "~/Dropbox/Mistakes Structural/Data/2021/AppList.Rdata")
save(appFinal, file = "~/Dropbox/Mistakes Structural/Data/2021/appFinal.Rdata")
save(appRCT,file = "~/Dropbox/Mistakes Structural/Data/2021/appRCT.Rdata")
save(Cartilla_OpenList,file = "~/Dropbox/Mistakes Structural/Data/2021/Cartilla_OpenList_1415.Rdata")
save(asignacion_obtenida_reg,file = "~/Dropbox/Mistakes Structural/Data/2021/asignacion_obtenida_reg.Rdata")
save(asignacion_obtenida_reg_before,file = "~/Dropbox/Mistakes Structural/Data/2021/asignacion_obtenida_reg_before.Rdata")
save(asignacion_obtenida_bea_before,file = "~/Dropbox/Mistakes Structural/Data/2021/asignacion_obtenida_bea_before.Rdata")
save(bcu,file = "~/Dropbox/Mistakes Structural/Data/2021/ACCESO_BCU-8184.ETAPA2.Rdata")


# Merging survey 2021 -----------------------------------------------------

dfs_2021 = read.csv2("~/Dropbox/Mistakes Structural/Data/2021/survey files/survey-responses-2021.csv", sep = ",")
apps_input_survey_2021 = read.csv2("~/Dropbox/Mistakes Structural/Data/2021/survey files/postulaciones_edited.csv", sep = ",") 
dfs_2021 = merge(dfs_2021, apps_input_survey_2021, by.x = "id", by.y = "ID", all.x = TRUE)
# Merging (INEXACT) by application lists an application scores
# Create fake ID for appFinal
Fake_app_id_fun = function(i, appFinal = appFinal){
  #print(i)
  fake_app_id = c()
  for(p in 1:10){
    code_car_aux = appFinal[i,paste("CAR_CODIGO_",p,"PREFER",sep="")]
    if(code_car_aux == -1){
      code_car_aux = 0
    }
    fake_app_id = c(fake_app_id, code_car_aux)
  }
  for(p in 1:10){
    ptje_aux = appFinal[i,paste("PUNTAJE_CARR",p,sep="")]
    if(is.na(ptje_aux)){
      ptje_aux = 0
    }
    fake_app_id = c(fake_app_id, 100*ptje_aux)
  }
  aux = paste(fake_app_id, collapse = "_")
  return(aux)
}
# Create fake ID for survey
Fake_app_id_survey_fun = function(i, dfs_2021 = dfs_2021){
  #print(i)
  fake_app_id = c()
  for(p in 1:10){
    if(p < 10){
      p = paste("0",p,sep="")
    }
    code_car_aux = dfs_2021[i,paste("COD_CARRERA_PREF_",p,sep="")]
    if(is.na(code_car_aux)){
      code_car_aux = 0
    }
    fake_app_id = c(fake_app_id, code_car_aux)
  }
  for(p in 1:10){
    if(p < 10){
      p = paste("0",p,sep="")
    }
    ptje_aux = dfs_2021[i,paste("PTJE_PREF_",p,sep="")]
    if(is.na(ptje_aux)){
      ptje_aux = 0
    }
    if(as.character(ptje_aux) == "NULL"){
      ptje_aux = -1
    }
    ptje_aux = as.numeric(as.character(ptje_aux))
    fake_app_id = c(fake_app_id, 100*ptje_aux)
  }
  aux = paste(fake_app_id, collapse = "_")
  return(aux)
}
#Create fake ids
dfs_2021$fake_app_id = sapply(1:nrow(dfs_2021), FUN = Fake_app_id_survey_fun, dfs_2021 = dfs_2021)
appFinal$fake_app_id = sapply(1:nrow(appFinal), FUN = Fake_app_id_fun, appFinal = appFinal)


# Before after vars -------------------------------------------------------

# Date
# Analyze distribution of applications per time of the day and day
#TODO: Check the timezone of the output of Chris!
#TODO: use regular expression for this to be more pro 
appFinal %<>% mutate(fecha_modified = gsub(" 9\\:"," 09:",FECHA_POSTULACION))
appFinal %<>% mutate(fecha_modified = gsub(" 8\\:"," 08:",fecha_modified))
appFinal %<>% mutate(fecha_modified = gsub(" 7\\:"," 07:",fecha_modified))
appFinal %<>% mutate(fecha_modified = gsub(" 6\\:"," 06:",fecha_modified))
appFinal %<>% mutate(fecha_modified = gsub(" 5\\:"," 05:",fecha_modified))
appFinal %<>% mutate(fecha_modified = gsub(" 4\\:"," 04:",fecha_modified))
appFinal %<>% mutate(fecha_modified = gsub(" 3\\:"," 03:",fecha_modified))
appFinal %<>% mutate(fecha_modified = gsub(" 2\\:"," 02:",fecha_modified))
appFinal %<>% mutate(fecha_modified = gsub(" 1\\:"," 01:",fecha_modified))
appFinal %<>% mutate(fecha_modified = gsub(" 0\\:"," 00:",fecha_modified))
appFinal %<>% mutate(fecha_modified = anytime(fecha_modified))
hist(as.Date(appFinal$FECHA_POSTULACION, tryFormats = c("%Y-%m-%d %H:%M")), breaks = "days")
ggplot(appFinal, aes(fecha_modified)) +
  geom_histogram(bins = 100) + 
  geom_vline(xintercept = anytime("2021-02-13 23:00")) +
  geom_vline(xintercept = anytime("2021-02-14 11:49"))
#TODO: check that these times are in Chilean time

# Check the distribution of applications
p = ggplot(appFinal %>% mutate(application_initial = ifelse(NRO_POSTULACION == 1, 1, 0)),
                               aes(fecha_modified, fill = as.factor(application_initial))) +
  geom_histogram(bins = 50, position = "dodge") +
  geom_vline(xintercept = anytime("2021-02-13 23:00"), linetype = "longdash") +
  geom_text(x = anytime("2021-02-13 23:00"), y = 5000, label = "Sample \n intervention") +
  geom_vline(xintercept = anytime("2021-02-14 11:49"), linetype = "longdash" ) +
  geom_text(x = anytime("2021-02-14 11:49"), y = 5000, label = "Email") +
  xlab("date") +
  scale_fill_brewer(palette="Pastel1",
                    name="Application",
                    breaks = c(1,
                               0),
                    labels = c("Initial",
                               "Modified")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
p
ggsave(p, file = paste(path_draft_plots,"Histogram_all_applications.png",sep=""), width=10, height=5)

#Separate Date
appFinal <- separate(data = appFinal, col = FECHA_POSTULACION, into=c("FECHA", "HORA"), sep=" ")

# Stats 
sum(duplicated(appFinal$MRUN))

# App length
appFinal %<>% mutate(app_length = ifelse(CAR_CODIGO_1PREFER == -1, 0,
                                         ifelse(CAR_CODIGO_2PREFER == -1, 1,
                                                ifelse(CAR_CODIGO_3PREFER == -1, 2,
                                                       ifelse(CAR_CODIGO_4PREFER == -1, 3,
                                                              ifelse(CAR_CODIGO_5PREFER == -1, 4,
                                                                     ifelse(CAR_CODIGO_6PREFER == -1, 5,
                                                                            ifelse(CAR_CODIGO_7PREFER == -1, 6,
                                                                                   ifelse(CAR_CODIGO_8PREFER == -1, 7,
                                                                                          ifelse(CAR_CODIGO_9PREFER == -1, 8,
                                                                                                 ifelse(CAR_CODIGO_10PREFER == -1, 9,
                                                                                                        10
                                                                                                 )))))))))))
ggplot(appFinal, aes( x = as.factor(app_length))) + geom_bar()

#NOTE: the sample that recieved an email are all students that submitted an application before "2021-02-13 23:00"
# Chile time zone

# Applications before cartilla
appFinal_before_cartilla = appFinal %>% 
  subset((FECHA < "2021-02-13") | (FECHA == "2021-02-13" & (strptime(HORA, "%H:%M") < strptime("23:00", "%H:%M"))))
# Select the last application by student
appFinal_before_cartilla_summary = appFinal_before_cartilla %>% 
  group_by(MRUN) %>% summarise(max_app_nro = max(NRO_POSTULACION))
# Merge stat
appFinal_before_cartilla = merge(appFinal_before_cartilla,
                                      appFinal_before_cartilla_summary,
                                      by = "MRUN", all.x = TRUE)
# Select last application
appFinal_before_cartilla %<>% subset(NRO_POSTULACION == max_app_nro)
#Stats
nrow(appFinal_before_cartilla)
sum(duplicated(appFinal_before_cartilla$MRUN))
mean(appFinal_before_cartilla$app_length)

# Applications after cartilla
#TODO: check if we are adding applications before sending the email
appFinal_after_cartilla = appFinal %>% 
  subset((FECHA > "2021-02-13") | (FECHA == "2021-02-13" & (strptime(HORA, "%H:%M") >= strptime("23:00", "%H:%M"))))
# Select the last application by student
appFinal_after_cartilla_summary = appFinal_after_cartilla %>% 
  group_by(MRUN) %>% summarise(max_app_nro = max(NRO_POSTULACION))
# Merge stat
appFinal_after_cartilla = merge(appFinal_after_cartilla,
                                     appFinal_after_cartilla_summary,
                                     by = "MRUN", all.x = TRUE)
# Select last application
appFinal_after_cartilla %<>% subset(NRO_POSTULACION == max_app_nro)
#Stats
nrow(appFinal_after_cartilla)
sum(duplicated(appFinal_after_cartilla$MRUN))
mean(appFinal_after_cartilla$app_length)

# Merge by MRUN applicants that appear in both data sets
appFinal_before_cartilla %<>% dplyr::rename(app_length_before = app_length)
appFinal_after_cartilla %<>% dplyr::rename(app_length_after = app_length)
# Changed application list after intervention
appFinal_after_cartilla %<>% mutate(changed_app_after = 1)

# Keeping all applicants and allowing for one application before (the last one before the intervention),
# and one application after the intervention (the last one)
appFinal_before_after = merge(appFinal_before_cartilla,
                             appFinal_after_cartilla,
                             by = "MRUN",
                             all.x = TRUE)
appFinal_before_after %<>% mutate(changed_app_after = ifelse(is.na(changed_app_after), 0, changed_app_after))
appFinal_before_after %<>% mutate(app_length_after = ifelse(is.na(app_length_after), app_length_before, app_length_after))

# Create app_length_change
appFinal_before_after %<>% mutate(app_length_change = app_length_after - app_length_before)
# Change in number of applications and applications themselves
sum(appFinal_before_after$app_length_change < 0)
sum(appFinal_before_after$app_length_change > 0)
mean(appFinal_before_after$app_length_change)
table(appFinal_before_after$changed_app_after)


# Merge to survey 2021 ----------------------------------------------------

appFinal_before_after %<>% mutate(fake_app_id_final = ifelse(is.na(fake_app_id.y), fake_app_id.x, fake_app_id.y))
sum(duplicated(appFinal_before_after$fake_app_id_final))
sum(duplicated(dfs_2021$fake_app_id))
#Drop duplicated IDs
dup_ids_dfs_2021 = dfs_2021[duplicated(dfs_2021$fake_app_id), "fake_app_id"]
dfs_2021 = dfs_2021[!(dfs_2021$fake_app_id %in% dup_ids_dfs_2021),]
dup_ids_appFinal_before_after = appFinal_before_after[duplicated(appFinal_before_after$fake_app_id_final), 
                                                      "fake_app_id_final"]
appFinal_before_after_survey = appFinal_before_after[!(appFinal_before_after$fake_app_id_final %in% dup_ids_appFinal_before_after),]

# Merge surevy to appFinal by last application
appFinal_before_after_survey = merge(appFinal_before_after_survey, dfs_2021, by.x = "fake_app_id_final", by.y = "fake_app_id", all = FALSE)

# BCU ---------------------------------------------------------------------

# Clean BCU
bcu %<>% subset(CODIGO_DEMRE != 0)
# No duplicated DEMRE Cods!
sum(duplicated(bcu$CODIGO_DEMRE))
bcu_earnings = bcu %>% select(CODIGO_DEMRE, INGRESO_4)
table(bcu_earnings$INGRESO_4, useNA = "always")
#NOTE: Many programs do not have wage info
bcu_earnings %<>% mutate(INGRESO_4 = as.character(INGRESO_4),
                         INGRESO_4 = ifelse(INGRESO_4 == "Sin Información Disponible", NA, INGRESO_4))
#Replace .
bcu_earnings %<>% mutate(INGRESO_4 = gsub("\\.", "", INGRESO_4))

# Compute midpoint of earnings in BCU
bcu_earnings %<>% separate(INGRESO_4, c("De_Sobre","Wage_lower","a_NA","Wage_higher"),sep = " ")
table(bcu_earnings$De_Sobre, useNA = "always")
table(bcu_earnings$Wage_lower, useNA = "always")
table(bcu_earnings$a_NA, useNA = "always")
table(bcu_earnings$Wage_higher, useNA = "always")
bcu_earnings %<>% mutate(mid_wage = ifelse(is.na(Wage_higher),
                                           as.numeric(Wage_lower),
                                           (as.numeric(Wage_higher) + as.numeric(Wage_lower))/2))
hist(bcu_earnings$mid_wage)
# Use our computed cutoffs
df_cutoffs = readRDS("~/Dropbox/Mistakes Structural/Data/older/panel_cutoffs_2004_2020_20210420.rds")
# Merge with bcu
bcu = merge(bcu, df_cutoffs %>% select(codigo_carrera, cutoff_2020), 
            by.x = "CODIGO_DEMRE", by.y = "codigo_carrera", all.x = TRUE)  
hist(bcu$cutoff_2020)
# NOTE: 340 programs for which we do not have a cutoff value 
sum(is.na(bcu$cutoff_2020))

# Added programs ----------------------------------------------------------


#TODO: review this code and make it more efficient 


#Obtain set of programs that the student adds
appFinal_before_after$mean_wage_before = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  #cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
        cars_before_set = c(cars_before_set,x[paste("CAR_CODIGO_",k,"PREFER.x", sep = "")])
        #cars_after_set = c(cars_after_set,x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")])
  }
  #if(x["changed_app_after"] == 0){
  #  cars_after_set = cars_before_set
  #} 
  #cars_new = setdiff(as.numeric(cars_after_set), as.numeric(cars_before_set))
  # Filter NAs and -1
  #cars_new = cars_new[!is.na(cars_new)]
  #cars_new = cars_new[cars_new != -1]
  # Compute average wage of new programs
  mean_wage_before_cars = mean((bcu_earnings %>% subset(CODIGO_DEMRE %in% cars_before_set))$mid_wage, na.rm = TRUE) 
  return(mean_wage_before_cars)
})
hist(appFinal_before_after$mean_wage_before)#Obtain set of programs that the student adds
mean((appFinal_before_after %>% subset(changed_app_after == 1))$mean_wage_before, na.rm = TRUE)

appFinal_before_after$mean_wage_after = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
        cars_before_set = c(cars_before_set,x[paste("CAR_CODIGO_",k,"PREFER.x", sep = "")])
        cars_after_set = c(cars_after_set,x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  #cars_new = setdiff(as.numeric(cars_after_set), as.numeric(cars_before_set))
  # Filter NAs and -1
  #cars_new = cars_new[!is.na(cars_new)]
  #cars_new = cars_new[cars_new != -1]
  # Compute average wage of new programs
  mean_wage_after_cars = mean((bcu_earnings %>% subset(CODIGO_DEMRE %in% cars_after_set))$mid_wage, na.rm = TRUE) 
  return(mean_wage_after_cars)
})
hist(appFinal_before_after$mean_wage_after)
mean((appFinal_before_after %>% subset(changed_app_after == 1))$mean_wage_after, na.rm = TRUE)

#Obtain set of programs that the student adds
appFinal_before_after$mean_wage_new_cars = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
        cars_before_set = c(cars_before_set,x[paste("CAR_CODIGO_",k,"PREFER.x", sep = "")])
        cars_after_set = c(cars_after_set,x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  cars_new = setdiff(as.numeric(cars_after_set), as.numeric(cars_before_set))
  # Filter NAs and -1
  cars_new = cars_new[!is.na(cars_new)]
  cars_new = cars_new[cars_new != -1]
  # Compute average wage of new programs
  mean_wage_new_cars = mean((bcu_earnings %>% subset(CODIGO_DEMRE %in% cars_new))$mid_wage, na.rm = TRUE) 
  return(mean_wage_new_cars)
})
hist(appFinal_before_after$mean_wage_new_cars)
mean((appFinal_before_after %>% subset(changed_app_after == 1))$mean_wage_new_cars, na.rm = TRUE)

#Changes in wages for added programs relative to initial programs
appFinal_before_after$mean_wage_new_cars_change = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("CAR_CODIGO_",k,"PREFER.x", sep = "")])
    cars_after_set = c(cars_after_set,x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  cars_new = setdiff(as.numeric(cars_after_set), as.numeric(cars_before_set))
  # Filter NAs and -1
  cars_new = cars_new[!is.na(cars_new)]
  cars_new = cars_new[cars_new != -1]
  # Compute average wage of new programs
  if(length(cars_new) > 0){
  mean_wage_new_cars = mean((bcu_earnings %>% subset(CODIGO_DEMRE %in% cars_new))$mid_wage, na.rm = TRUE) 
  mean_wage_before_cars = mean((bcu_earnings %>% subset(CODIGO_DEMRE %in% cars_before_set))$mid_wage, na.rm = TRUE)
  mean_wage_new_cars_change = mean_wage_new_cars - mean_wage_before_cars
  }
  else{
    mean_wage_new_cars_change = 0
  }
  return(mean_wage_new_cars_change)
})
hist(appFinal_before_after$mean_wage_new_cars_change)
mean((appFinal_before_after %>% subset(changed_app_after == 1))$mean_wage_new_cars_change, na.rm = TRUE)

# Changes in cutoffs for new programs
#Obtain set of programs that the student adds
appFinal_before_after$mean_cutoff_before = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  #cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("CAR_CODIGO_",k,"PREFER.x", sep = "")])
    #cars_after_set = c(cars_after_set,x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")])
  }
  #if(x["changed_app_after"] == 0){
  #  cars_after_set = cars_before_set
  #} 
  #cars_new = setdiff(as.numeric(cars_after_set), as.numeric(cars_before_set))
  # Filter NAs and -1
  #cars_new = cars_new[!is.na(cars_new)]
  #cars_new = cars_new[cars_new != -1]
  # Compute average wage of new programs
  mean_cutoff_before_cars = mean((bcu %>% subset(CODIGO_DEMRE %in% cars_before_set))$cutoff_2020, na.rm = TRUE) 
  return(mean_cutoff_before_cars)
})
hist(appFinal_before_after$mean_cutoff_before)#Obtain set of programs that the student adds

appFinal_before_after$mean_cutoff_after = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("CAR_CODIGO_",k,"PREFER.x", sep = "")])
    cars_after_set = c(cars_after_set,x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  #cars_new = setdiff(as.numeric(cars_after_set), as.numeric(cars_before_set))
  # Filter NAs and -1
  #cars_new = cars_new[!is.na(cars_new)]
  #cars_new = cars_new[cars_new != -1]
  # Compute average wage of new programs
  mean_cutoff_after_cars = mean((bcu %>% subset(CODIGO_DEMRE %in% cars_after_set))$cutoff_2020, na.rm = TRUE) 
  return(mean_cutoff_after_cars)
})
hist(appFinal_before_after$mean_cutoff_after)

#Obtain set of programs that the student adds
appFinal_before_after$mean_cutoff_new_cars = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("CAR_CODIGO_",k,"PREFER.x", sep = "")])
    cars_after_set = c(cars_after_set,x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  cars_new = setdiff(as.numeric(cars_after_set), as.numeric(cars_before_set))
  # Filter NAs and -1
  cars_new = cars_new[!is.na(cars_new)]
  cars_new = cars_new[cars_new != -1]
  # Compute average wage of new programs
  mean_cutoff_new_cars = mean((bcu %>% subset(CODIGO_DEMRE %in% cars_new))$cutoff_2020, na.rm = TRUE) 
  return(mean_cutoff_new_cars)
})
hist(appFinal_before_after$mean_cutoff_new_cars)

# Changes in invalid programs
appFinal_before_after$invalid_programs_before = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  #cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("PUNTAJE_CARR",k,".x", sep = "")])
    #cars_after_set = c(cars_after_set,x[paste("PUNTAJE_CARR",k,".y", sep = "")])
  }
  #invalid_programs_after = sum(as.numeric(cars_after_set[!is.na(cars_after_set)]) == -1)
  invalid_programs_before = sum(as.numeric(cars_before_set[!is.na(cars_before_set)]) == -1)
  return(invalid_programs_before)
})

hist(appFinal_before_after$invalid_programs_before)
sum(appFinal_before_after$invalid_programs_before > 0)

appFinal_before_after$invalid_programs_after = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("PUNTAJE_CARR",k,".x", sep = "")])
    cars_after_set = c(cars_after_set,x[paste("PUNTAJE_CARR",k,".y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  invalid_programs_after = sum(as.numeric(cars_after_set[!is.na(cars_after_set)]) == -1)
  #invalid_programs_before = sum(as.numeric(cars_before_set[!is.na(cars_before_set)]) == -1)
  return(invalid_programs_after)
})

hist(appFinal_before_after$invalid_programs_after)
sum(appFinal_before_after$invalid_programs_after > 0)

appFinal_before_after$invalid_programs_change = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("PUNTAJE_CARR",k,".x", sep = "")])
    cars_after_set = c(cars_after_set,x[paste("PUNTAJE_CARR",k,".y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  invalid_programs_after = sum(as.numeric(cars_after_set[!is.na(cars_after_set)]) == -1)
  invalid_programs_before = sum(as.numeric(cars_before_set[!is.na(cars_before_set)]) == -1)
  return(invalid_programs_after-invalid_programs_before)
})

hist(appFinal_before_after$invalid_programs_change)
sum(appFinal_before_after$invalid_programs_change < 0)

appFinal_before_after$invalid_programs_share_change = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("PUNTAJE_CARR",k,".x", sep = "")])
    cars_after_set = c(cars_after_set,x[paste("PUNTAJE_CARR",k,".y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  invalid_programs_share_after = sum(as.numeric(cars_after_set[!is.na(cars_after_set)]) == -1)/as.numeric(x["app_length_after"])
  invalid_programs_share_before = sum(as.numeric(cars_before_set[!is.na(cars_before_set)]) == -1)/as.numeric(x["app_length_before"])
  return(invalid_programs_share_after-invalid_programs_share_before)
})

hist(appFinal_before_after %>% filter(invalid_programs_share_change != 0) %>%                                              #command does not work
       select(invalid_programs_share_change))
hist(unlist(appFinal_before_after %>% filter(invalid_programs_share_change != 0) %>%  select(invalid_programs_share_change)))  #unlist


table(appFinal_before_after$invalid_programs_share_change, useNA = "always")
sum(appFinal_before_after$invalid_programs_share_change < 0)
sum(appFinal_before_after$invalid_programs_share_change > 0)

#Change applications
table(appFinal_before_after$changed_app_after, useNA = "always")

#Compute effects by treatement group
# Merge treatement data
appFinal_before_after = merge(appFinal_before_after, appList, by.x = "MRUN", by.y = "mrun", all.x = TRUE)
appFinal_before_after %<>% subset(!is.na(Treatment))
appFinal_before_after = fastDummies::dummy_cols(appFinal_before_after, select_columns = "Treatment")

# Merge with list of MRUN that opened cartilla
Cartilla_OpenList %<>% dplyr::rename(MRUN = V1, opening_cartilla_date = V2)
Cartilla_OpenList %<>% mutate(abrio_cartilla = 1)
#TODO: check the quality of this data and how to properly export it from Google Analytics
# Checking duplicated MRUNs
sum(duplicated(Cartilla_OpenList$MRUN))
#Cartilla_OpenList = Cartilla_OpenList[!duplicated(Cartilla_OpenList$MRUN),]
appFinal_before_after = merge(appFinal_before_after, Cartilla_OpenList,
                              by = "MRUN", all.x =TRUE)
appFinal_before_after %<>% mutate(abrio_cartilla = ifelse(is.na(abrio_cartilla), 0, abrio_cartilla))
table(appFinal_before_after$abrio_cartilla)

# Merge appRCT

# Grupo 0 recibio todo
# Grupo 1 recibio solo warnings
# Grupo 2 recibio solo recomendaciones
# Grupo 3 no recibio intervencion, salvo mensajes de beneficios y saludos (ver Excel). Este grupo no es random.
# Treatment effects of Warnings: 0 minus 2
# Treatment effects of Recomendations: 0 minus 1

#Strata_1 es Macro area of most preferred major
#Strata_2 es General Message:
#Strata_2:
# 0: "Explora mas carreras y opciones"
# 1: "Agrega safety"
# 2: "Agrega safety y reach"
# 3: "Agrega reach"
#Strata_3 es Grupos de puntaje
#Strata_4 es Private School or not

appFinal_before_after = merge(appFinal_before_after, appRCT %>% select(!Treatment),
                              by.x = "MRUN", by.y = "mrun", all.x = TRUE)

# Check changes in applications by Treatment
table_with_strata2 = appFinal_before_after %>% 
  group_by(Treatment, Strata_2, abrio_cartilla) %>% 
  summarise(n = n(),
            changed_app_after_n = sum(changed_app_after > 0),
            changed_app_after_mean = mean(changed_app_after))

# Excluding T3
appFinal_before_after %>% 
  filter(Treatment != 3) %>%
  group_by(abrio_cartilla) %>% 
  summarise(n = n(),
            changed_app_after_n = sum(changed_app_after > 0),
            changed_app_after_mean = mean(changed_app_after))

# Check some stats for students with an admissibility mistake by opening of cartilla or not
appFinal_before_after %>% filter((Treatment != 3) & (invalid_programs_before > 0)) %>% 
  group_by(abrio_cartilla) %>% 
  summarise(n = n(),
            changed_app_after_n = sum(changed_app_after > 0),
            changed_app_after_mean = mean(changed_app_after),
            reduced_add_mistakes_n = sum(invalid_programs_change < 0),
            reduced_add_mistakes_mean = mean(invalid_programs_change < 0),
            change_add_mistakes_mean = mean(invalid_programs_change),
            invalid_programs_share_reduced_mean = mean(invalid_programs_share_change < 0))

# Check some stats for students with an admissibility mistake by treatment group
appFinal_before_after %>% filter((invalid_programs_before > 0)) %>% 
  group_by(Treatment, abrio_cartilla) %>% 
  summarise(n = n(),
            changed_app_after_n = sum(changed_app_after > 0),
            changed_app_after_mean = mean(changed_app_after),
            reduced_add_mistakes_n = sum(invalid_programs_change < 0),
            reduced_add_mistakes_mean = mean(invalid_programs_change < 0),
            change_add_mistakes_mean = mean(invalid_programs_change),
            invalid_programs_share_reduced_mean = mean(invalid_programs_share_change < 0))

#TODO: check quality of treatement 3
#NOTE: here we loose thousands of observations in the analysys
#appFinal_before_after %<>% subset(Treatment != 3)

# Assignment --------------------------------------------------------------

# Merge
# Final assignment
asignados_reg = (asignacion_obtenida_reg %>% subset(marca == 24))
asignados_bea = (asignacion_obtenida_bea %>% subset(marca == 24))
asignados_df = rbind(asignados_reg, asignados_bea)
#Checking duplicated assignments
sum(duplicated(asignados_df$id_alumno))
#asignados_df = asignados_df[!duplicated(asignados_df$id_alumno),]
asignados_df %<>% dplyr::rename(MRUN = id_alumno)
asignados_df %<>% mutate(asignado = 1)
table(asignados_df$asignado)

#Counterfactual assignment with application before treatment
asignados_reg_before = (asignacion_obtenida_reg_before %>% subset(marca == 24))
asignados_bea_before = (asignacion_obtenida_bea_before %>% subset(marca == 24))
asignados_df_before = rbind(asignados_reg_before, asignados_bea_before)
#Checking duplicated assignments
sum(duplicated(asignados_df_before$id_alumno))
#asignados_df_before = asignados_df_before[!duplicated(asignados_df_before$id_alumno),]
asignados_df_before %<>% dplyr::rename(MRUN = id_alumno,
                                       codigo_carrera_before = codigo_carrera)
asignados_df_before %<>% mutate(asignado_before = 1)
table(asignados_df_before$asignado_before)
head(asignados_df_before)

#Merge assignment data with BCU data
asignados_df = merge(asignados_df, bcu_earnings %>%
                       select(CODIGO_DEMRE, mid_wage) %>%
                       dplyr::rename(mid_wage = mid_wage),
                     by.x = "codigo_carrera", by.y = "CODIGO_DEMRE",
                     all.x = TRUE)
asignados_df_before = merge(asignados_df_before, bcu_earnings %>%
                              select(CODIGO_DEMRE, mid_wage) %>%
                              dplyr::rename(mid_wage_before = mid_wage),
                     by.x = "codigo_carrera_before", by.y = "CODIGO_DEMRE",
                     all.x = TRUE)
# Analysis
table(asignados_df$mid_wage, useNA = "always")
table(asignados_df_before$mid_wage_before, useNA = "always")
mean(asignados_df$mid_wage, na.rm = TRUE)
mean(asignados_df_before$mid_wage_before, na.rm = TRUE)

# Merge Asignados ---------------------------------------------------------

appFinal_before_after = merge(appFinal_before_after, asignados_df, by = "MRUN", all.x = TRUE)
appFinal_before_after %<>% mutate(asignado = ifelse(is.na(asignado), 0, asignado),
                                  codigo_carrera = ifelse(is.na(codigo_carrera), 0, codigo_carrera))
table(appFinal_before_after$asignado)
#Merge with asignados before
appFinal_before_after = merge(appFinal_before_after, asignados_df_before, by = "MRUN", all.x = TRUE)
appFinal_before_after %<>% mutate(asignado_before = ifelse(is.na(asignado_before), 0, asignado_before),
                                  codigo_carrera_before = ifelse(is.na(codigo_carrera_before), 0, codigo_carrera_before))
table(appFinal_before_after$asignado_before)
table(appFinal_before_after$asignado_before, appFinal_before_after$asignado)

#Change in assignment status
appFinal_before_after %<>% mutate(asignado_change = asignado - asignado_before)
table(appFinal_before_after$asignado_change)
appFinal_before_after %<>% mutate(program_asig_change = (!is.na(codigo_carrera) &
                                    !is.na(codigo_carrera_before) &
                                    (codigo_carrera != codigo_carrera_before)))
table(appFinal_before_after$program_asig_change)

#Change in wage of assignment
appFinal_before_after %<>% mutate(mid_wage_change = mid_wage - mid_wage_before)
hist((appFinal_before_after %>% subset(mid_wage_change != 0))$mid_wage_change)
mean((appFinal_before_after %>% subset(mid_wage_change != 0))$mid_wage_change)
mean((appFinal_before_after %>% subset(program_asig_change == TRUE))$mid_wage_change, na.rm = TRUE)


# Tables for Treatment effects -----------------------------------------------

# Number of students who applied before February 13th at 23:00
nrow(appFinal_before_after)

# Number of students by treatement group who opened the cartilla
table(appFinal_before_after$Treatment, appFinal_before_after$abrio_cartilla)

# Labelling treatment types
appFinal_before_after %<>% mutate(Treatment_label = recode(Treatment,
                                                           "0" = "T0",
                                                           "1" = "T1",
                                                           "2" = "T2",
                                                           "3" = "T3 (out of sample)",
                                                           ),
                                  abrio_cartilla_label = recode(abrio_cartilla,
                                                           "0" = "No",
                                                           "1" = "Yes"))
# Load list of mrun that comes from the Propensity score procedure
mruns_propensity_score = readRDS("~/Dropbox/Mistakes Structural/Data/2021/matched_sampled_using_propensity_score_view_email.rds")
mruns_propensity_score %<>% select(mrun)
mruns_propensity_score %<>% mutate(propensity_score_sample = 1)
# Merge with all the sample for the anlysis
appFinal_before_after = merge(appFinal_before_after, mruns_propensity_score, by.x = "MRUN", by.y = "mrun", all.x = TRUE)
appFinal_before_after %<>% mutate(propensity_score_sample = ifelse(is.na(propensity_score_sample), 0, propensity_score_sample))
table(appFinal_before_after$propensity_score_sample)


#Propensity score second specification (Anaïs):
mruns_propensity_score_anais = readRDS(file = "~/Dropbox/Mistakes Structural/Data/2021/matched_sampled_using_propensity_score_view_email_anais.rds")
mruns_propensity_score_anais %<>% select(mrun)
mruns_propensity_score_anais %<>% mutate(propensity_score_sample_anais = 1)
# Merge with all the sample for the anlysis
appFinal_before_after = merge(appFinal_before_after, mruns_propensity_score_anais, by.x = "MRUN", by.y = "mrun", all.x = TRUE)
appFinal_before_after %<>% mutate(propensity_score_sample_anais = ifelse(is.na(propensity_score_sample_anais), 0, propensity_score_sample_anais))
table(appFinal_before_after$propensity_score_sample_anais)

# Stats for students with an admissibility mistake by treatment group
table_treatment_stats_adm_mistakes = appFinal_before_after %>% 
  filter((invalid_programs_before > 0)) %>% 
  subset(propensity_score_sample == 1) %>%
  group_by(Treatment_label, abrio_cartilla_label) %>% 
  summarise(n = n(),
            #changed_app_after_n = sum(changed_app_after > 0),
            changed_app_after_mean = round(100*mean(changed_app_after),2),
           # reduced_add_mistakes_n = sum(invalid_programs_change < 0),
            reduced_add_mistakes_mean = round(100*mean(invalid_programs_change < 0),2),
            #change_add_mistakes_mean = mean(invalid_programs_change),
            invalid_programs_share_reduced_mean = round(100*mean(invalid_programs_share_change < 0),2))

table_treatment_stats_adm_mistakes_stderr = appFinal_before_after %>% 
  filter((invalid_programs_before > 0)) %>%
  subset(propensity_score_sample == 1) %>%
  group_by(Treatment_label, abrio_cartilla_label) %>% 
  summarise(n = "-",
            #changed_app_after_n = "-",
            changed_app_after_mean = round(100*sd(changed_app_after)/sqrt(n()),2),
            #reduced_add_mistakes_n = "-",
            reduced_add_mistakes_mean = round(100*sd(invalid_programs_change < 0)/sqrt(n()),2),
            #change_add_mistakes_mean = sd(invalid_programs_change)/sqrt(n()),
            invalid_programs_share_reduced_mean = round(100*sd(invalid_programs_share_change < 0)/sqrt(n()),2))

#Write latex table by hand
table_latex_file = file(paste(path_draft_tables,"table_treatment_stats_adm_mistakes.tex", sep=""))
table_header = c("\\begin{table}[H]",
                 "\\centering",
                 "\\caption{Statistics for mistakers by treatment groups conditional on opening}",
                 "\\label{tab:table_treatment_stats_adm_mistakes}",
                 "\\scalebox{0.7}{",
                 "\\begin{tabular}{lccccc}",
                 "\\toprule",
                 "\\toprule")
table_columns = paste("Treatment",
                      "\\makecell*[c]{Opened  \\\\ website [\\%]}",
                      "Total",
                      "Modified [\\%]",
                      "\\makecell*[c]{Reduced Adm. \\\\ Mistakes [\\%]}",
                      "\\makecell*[c]{Reduced share  \\\\ Adm. Mistakes [\\%] }",
                      sep = " & ")
table_body = paste(table_columns, "\\\\" ," \\midrule")

space = '\\\\[0pt]'
table_stats = table_treatment_stats_adm_mistakes
table_sterr_stats = table_treatment_stats_adm_mistakes_stderr
#Fill table body
for(i in 1:nrow(table_stats)){
  row = c()
  for(j in 1:ncol(table_stats)){
    row_stat = table_stats[i,j]
    if(j == 1){
      row = row_stat
    }
    else{
      row = paste(row, "&", row_stat)
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
  # Paste row for standard errors
  row = c()
  for(j in 1:ncol(table_sterr_stats)){
    row_stat = table_sterr_stats[i,j]
    if(j != 1){
      if(j == 2){
        row = paste(row, "&", "")
      }else{
        row = paste(row, "&", paste("(",row_stat,")",sep=""))
      }
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
}

table_footer = c("\\bottomrule",
                 "\\multicolumn{6}{l}{\\textit{Note:} standard errors are computed in parenthesis.}",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
table_latex_code = c(table_header, table_body, table_footer)
writeLines(table_latex_code, table_latex_file)
close(table_latex_file)



# Number of applicants who modified their appplication by treatement group
table_treatment_stats = appFinal_before_after %>% 
  group_by(Treatment_label) %>%
  summarise(Total =    length(changed_app_after),
            Opens = round(100*mean(abrio_cartilla),2),
            Modifies = round(100*mean(changed_app_after),2),
            Increases_length = round(100*mean(app_length_change > 0),2),
            Decreases_length = round(100*mean(app_length_change < 0),2),
            #Largo_antes = round(mean(app_length_before),2),
            #Largo_despues = round(mean(app_length_after),2),
            Assigned_level = round(100*mean(asignado),2),
            Assigned_change = round(100*mean(asignado_change),2),
            Program_assig_change = round(100*mean(program_asig_change),2)
            #Wage_change = round(mean(mid_wage_change[program_asig_change], na.rm = TRUE),2),
            )

table_treatment_stats_stderr = appFinal_before_after %>% 
  group_by(Treatment_label) %>%
  summarise(Total =    "-",
            Opens = round(100*sd(abrio_cartilla)/sqrt(n()),2),
            Modifies = round(100*sd(changed_app_after)/sqrt(n()),2),
            Increases_length = round(100*sd(app_length_change > 0)/sqrt(n()),2),
            Decreases_length = round(100*sd(app_length_change < 0)/sqrt(n()),2),
            #Largo_antes = round(mean(app_length_before),2),
            #Largo_despues = round(mean(app_length_after),2),
            Assigned_level = round(100*sd(asignado)/sqrt(n()),2),
            Assigned_change = round(100*sd(asignado_change)/sqrt(n()),2),
            Program_assig_change = round(100*sd(program_asig_change)/sqrt(n()),2)
            #Wage_change = round(mean(mid_wage_change[program_asig_change], na.rm = TRUE),2),
  )

#Write latex table by hand
table_latex_file = file(paste(path_draft_tables,"table_treatment_stats.tex", sep=""))
table_header = c("\\begin{table}[H]",
                 "\\centering",
                 "\\caption{Aggregate statistics by treatment groups}",
                 "\\label{tab:table_treatment_stats}",
                 "\\scalebox{0.7}{",
                 "\\begin{tabular}{lcccccccc}",
                 "\\toprule",
                 "\\toprule")
table_columns = paste("Treatment",
                      "Total",
                      "\\makecell*[c]{Opened  \\\\ website [\\%]}",
                      "Modified [\\%]",
                      "\\makecell*[c]{Increased  \\\\ length [\\%]}",
                      "\\makecell*[c]{Decreased  \\\\ length [\\%]}",
                      "Assigned [\\%]",
                      "\\makecell*[c]{Changed  \\\\ assignment \\\\ state [\\%]}",
                      "\\makecell*[c]{Changed  \\\\ program of \\\\ assignment [\\%]}",
                      sep = " & ")
table_body = paste(table_columns, "\\\\" ," \\midrule")

space = '\\\\[0pt]'
table_stats = table_treatment_stats
table_sterr_stats = table_treatment_stats_stderr
#Fill table body
for(i in 1:nrow(table_stats)){
  row = c()
  for(j in 1:ncol(table_stats)){
    row_stat = table_stats[i,j]
    if(j == 1){
      row = row_stat
    }
    else{
      row = paste(row, "&", row_stat)
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
  # Paste row for standard errors
  row = c()
  for(j in 1:ncol(table_sterr_stats)){
    row_stat = table_sterr_stats[i,j]
    if(j != 1){
      row = paste(row, "&", paste("(",row_stat,")",sep=""))
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
}

table_footer = c("\\bottomrule",
                 "\\multicolumn{9}{l}{\\textit{Note:} standard errors are computed in parenthesis.}",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
table_latex_code = c(table_header, table_body, table_footer)
writeLines(table_latex_code, table_latex_file)
close(table_latex_file)


#Save as csv
#write.csv2(table_treatment_stats, file = "Outputs/Tables/table_treatment_stats.csv", row.names = FALSE)

# Consider only students who opened the cartilla
# Number of applicants who modified their appplication by treatement group
table_treatment_abrio_stats = appFinal_before_after %>% 
  subset(propensity_score_sample == 1) %>%
  group_by(Treatment_label, abrio_cartilla_label) %>%
  summarise(Total =    length(changed_app_after),
            #Opens = round(100*mean(abrio_cartilla),2),
            Modifies = round(100*mean(changed_app_after),2),
            Increases_length = round(100*mean(app_length_change > 0),2),
            Decreases_length = round(100*mean(app_length_change < 0),2),
            #Largo_antes = round(mean(app_length_before),2),
            #Largo_despues = round(mean(app_length_after),2),
            Assigned_level = round(100*mean(asignado),2),
            Assigned_change = round(100*mean(asignado_change),2),
            Program_assig_change = round(100*mean(program_asig_change),2)
            #Wage_change = round(mean(mid_wage_change[program_asig_change], na.rm = TRUE),2),
  )

table_treatment_abrio_stats_stderr = appFinal_before_after %>% 
  subset(propensity_score_sample == 1) %>%
  group_by(Treatment_label, abrio_cartilla_label) %>%
  summarise(Total =    "-",
            #Opens = round(100*sd(abrio_cartilla)/sqrt(n()),2),
            Modifies = round(100*sd(changed_app_after)/sqrt(n()),2),
            Increases_length = round(100*sd(app_length_change > 0)/sqrt(n()),2),
            Decreases_length = round(100*sd(app_length_change < 0)/sqrt(n()),2),
            #Largo_antes = round(mean(app_length_before),2),
            #Largo_despues = round(mean(app_length_after),2),
            Assigned_level = round(100*sd(asignado)/sqrt(n()),2),
            Assigned_change = round(100*sd(asignado_change)/sqrt(n()),2),
            Program_assig_change = round(100*sd(program_asig_change)/sqrt(n()),2)
            #Wage_change = round(mean(mid_wage_change[program_asig_change], na.rm = TRUE),2),
  )

#Write latex table by hand
table_latex_file = file(paste(path_draft_tables,"table_treatment_abrio_stats.tex", sep=""))
table_header = c("\\begin{table}[H]",
                 "\\centering",
                 "\\caption{Aggregate statistics by treatment groups}",
                 "\\label{tab:table_treatment_abrio_stats}",
                 "\\scalebox{0.7}{",
                 "\\begin{tabular}{lcccccccc}",
                 "\\toprule",
                 "\\toprule")
table_columns = paste("Treatment",
                      "\\makecell*[c]{Opened  \\\\ website [\\%]}",
                      "Total",
                      "Modified [\\%]",
                      "\\makecell*[c]{Increased  \\\\ length [\\%]}",
                      "\\makecell*[c]{Decreased  \\\\ length [\\%]}",
                      #"\\makecell*[c]{Largo  \\\\ antes}",
                      #"\\makecell*[c]{Largo  \\\\ despu\\'es}",
                      "Assigned [\\%]",
                      "\\makecell*[c]{Changed  \\\\ assignment \\\\ state [\\%]}",
                      "\\makecell*[c]{Changed  \\\\ program of \\\\ assignment [\\%]}",
                      sep = " & ")
table_body = paste(table_columns, "\\\\" ," \\midrule")

space = '\\\\[0pt]'
table_stats = table_treatment_abrio_stats
table_sterr_stats = table_treatment_abrio_stats_stderr
#Fill table body
for(i in 1:nrow(table_stats)){
  row = c()
  for(j in 1:ncol(table_stats)){
    row_stat = table_stats[i,j]
    if(j == 1){
      row = row_stat
    }
    else{
      row = paste(row, "&", row_stat)
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
  # Paste row for standard errors
  row = c()
  for(j in 1:ncol(table_sterr_stats)){
    row_stat = table_sterr_stats[i,j]
    if(j != 1){
      if(j == 2){
        row = paste(row, "&", "")
      }else{
        row = paste(row, "&", paste("(",row_stat,")",sep=""))
      }
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
}

table_footer = c("\\bottomrule",
                 "\\multicolumn{9}{l}{\\textit{Note:} standard errors are computed in parenthesis.}",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
table_latex_code = c(table_header, table_body, table_footer)
writeLines(table_latex_code, table_latex_file)
close(table_latex_file)

# Consider Treatment and Strata
# Number of applicants who modified their appplication by treatement group
#Strata_2:
# 0: "Explora mas carreras y opciones"
# 1: "Agrega safety"
# 2: "Agrega safety y reach"
# 3: "Agrega reach"
appFinal_before_after %<>% mutate(Strata_2_label = recode(Strata_2,
                                                          "0" = "Explore",
                                                          "1" = "Safety",
                                                          "2" = "Safety and Reach",
                                                          "3" = "Reach"))

# Replcae puntajes NA with zeros
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR1.x), "PUNTAJE_CARR1.x"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR2.x), "PUNTAJE_CARR2.x"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR3.x), "PUNTAJE_CARR3.x"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR4.x), "PUNTAJE_CARR4.x"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR5.x), "PUNTAJE_CARR5.x"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR6.x), "PUNTAJE_CARR6.x"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR7.x), "PUNTAJE_CARR7.x"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR8.x), "PUNTAJE_CARR8.x"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR9.x), "PUNTAJE_CARR9.x"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR10.x), "PUNTAJE_CARR10.x"] = 0

appFinal_before_after %<>% mutate(error_before = ifelse((PUNTAJE_CARR1.x == -1 )|
                                                          (PUNTAJE_CARR2.x == -1) |
                                                          (PUNTAJE_CARR3.x == -1) |
                                                          (PUNTAJE_CARR4.x == -1) |
                                                          (PUNTAJE_CARR5.x == -1) |
                                                          (PUNTAJE_CARR6.x == -1) |
                                                          (PUNTAJE_CARR7.x == -1) |
                                                          (PUNTAJE_CARR8.x == -1) |
                                                          (PUNTAJE_CARR9.x == -1) |
                                                          (PUNTAJE_CARR10.x == -1), 1, 0))

mean(appFinal_before_after$error_before)
sum(appFinal_before_after$PUNTAJE_CARR1.x == -1)

# Consider only students who opened the cartilla
# Number of applicants who modified their appplication by treatement group and strata
table_treatment_strata_stats = appFinal_before_after %>% 
  #subset(abrio_cartilla == 1) %>%
  subset(propensity_score_sample == 1) %>%
  subset(Strata_2_label != "Safety and Reach") %>%
  filter(Treatment != 3) %>%
  arrange(Treatment_label) %>%
  group_by(Strata_2_label, Treatment_label, abrio_cartilla_label) %>%
  summarise(Total =    length(changed_app_after),
            #Opens = round(100*mean(abrio_cartilla),2),
            Modifies = round(100*mean(changed_app_after),2),
            Increases_length = round(100*mean(app_length_change > 0),2),
            Decreases_length = round(100*mean(app_length_change < 0),2),
            #Largo_antes = round(mean(app_length_before),2),
            #Largo_despues = round(mean(app_length_after),2),
            Assigned_level = round(100*mean(asignado),2),
            Assigned_change = round(100*mean(asignado_change),2),
            Program_assig_change = round(100*mean(program_asig_change),2)
            #Wage_change = round(mean(mid_wage_change[program_asig_change], na.rm = TRUE),2),
  )

table_treatment_strata_stats_stderr = appFinal_before_after %>% 
  #subset(abrio_cartilla == 1) %>%
  subset(propensity_score_sample == 1) %>%
  subset(Strata_2_label != "Safety and Reach") %>%
  filter(Treatment != 3) %>% 
  arrange(Treatment_label) %>%
  group_by(Strata_2_label, Treatment_label, abrio_cartilla_label) %>%
  summarise(Total =    "-",
            #Opens = round(100*sd(abrio_cartilla)/sqrt(n()),2),
            Modifies = round(100*sd(changed_app_after)/sqrt(n()),2),
            Increases_length = round(100*sd(app_length_change > 0)/sqrt(n()),2),
            Decreases_length = round(100*sd(app_length_change < 0)/sqrt(n()),2),
            #Largo_antes = round(mean(app_length_before),2),
            #Largo_despues = round(mean(app_length_after),2),
            Assigned_level = round(100*sd(asignado)/sqrt(n()),2),
            Assigned_change = round(100*sd(asignado_change)/sqrt(n()),2),
            Program_assig_change = round(100*sd(program_asig_change)/sqrt(n()),2)
            #Wage_change = round(mean(mid_wage_change[program_asig_change], na.rm = TRUE),2),
  )

#Write latex table by hand
table_latex_file = file(paste(path_draft_tables,"table_treatment_strata_stats.tex", sep=""))
table_header = c("\\begin{table}[H]",
                 "\\centering",
                 "\\caption{Aggregate statistics by treatment groups and message type}",
                 "\\label{tab:table_treatment_strata_stats}",
                 "\\scalebox{0.65}{",
                 "\\begin{tabular}{lccccccccc}",
                 "\\toprule",
                 "\\toprule")
table_columns = paste("\\makecell*[c]{Message  \\\\ type}",
                      "Treatment",
                      "Opens",
                      "Total",
                      "Modified [\\%]",
                      "\\makecell*[c]{Increased  \\\\ length [\\%]}",
                      "\\makecell*[c]{Decreased  \\\\ length [\\%]}",
                      #"\\makecell*[c]{Largo  \\\\ antes}",
                      #"\\makecell*[c]{Largo  \\\\ despu\\'es}",
                      "Assigned [\\%]",
                      "\\makecell*[c]{Changed  \\\\ assignment \\\\ state [\\%]}",
                      "\\makecell*[c]{Changed  \\\\ program of \\\\ assignment [\\%]}",
                      sep = " & ")
table_body = paste(table_columns, "\\\\" ," \\midrule")

space = '\\\\[0pt]'
table_stats = table_treatment_strata_stats
table_sterr_stats = table_treatment_strata_stats_stderr
#Fill table body
for(i in 1:nrow(table_stats)){
  row = c()
  for(j in 1:ncol(table_stats)){
    row_stat = table_stats[i,j]
    if(j == 1){
      row = row_stat
    }
    else{
      row = paste(row, "&", row_stat)
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
  # Paste row for standard errors
  row = c()
  for(j in 1:ncol(table_sterr_stats)){
    row_stat = table_sterr_stats[i,j]
    if(j != 1){
      if((j == 2) | (j == 3)){
        row = paste(row, "&", "")
      }else{
        row = paste(row, "&", paste("(",row_stat,")",sep=""))
      }
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
}

table_footer = c("\\bottomrule",
                 "\\multicolumn{10}{l}{\\textit{Note:} standard errors are computed in parenthesis.}",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
table_latex_code = c(table_header, table_body, table_footer)
writeLines(table_latex_code, table_latex_file)
close(table_latex_file)

# Changes in average wages and cutoffs
table_treatment_strata_stats_wages = appFinal_before_after %>%
  subset(abrio_cartilla == 1) %>%
  subset(Strata_2_label != "Safety and Reach") %>%
  filter(Treatment !=3) %>%
  filter(changed_app_after == 1) %>%
  arrange(Treatment_label) %>%
  group_by(Strata_2_label, Treatment_label) %>%
  summarise(Total = length(changed_app_after),
            #Asignados = round(100*mean(asignado),2),
            #Asignago_change = round(100*mean(asignado_change),2),
            #Program_asig_change = round(100*mean(program_asig_change),2),
            #Wage_change = round(mean(mid_wage_change[changed_app_after]/1e3, na.rm = TRUE),2),
            Cutoffs_initial_cars = round(mean(mean_cutoff_before, na.rm = TRUE),2),
            #Cutoffs_after_cars = round(mean(mean_cutoff_after, na.rm = TRUE),2),
            Cutoffs_new_cars = round(mean(mean_cutoff_new_cars, na.rm = TRUE),2),
            Wage_initial_cars = round(mean(mean_wage_before/1e3, na.rm = TRUE),2),
            #Wage_after_cars = round(mean(mean_wage_after/1e3, na.rm = TRUE),2),
            Wage_new_cars = round(mean(mean_wage_new_cars/1e3, na.rm = TRUE),2),
            Wage_new_cars_change = round(mean(mean_wage_new_cars_change/1e3, na.rm = TRUE),2))

# Compute standard errors
table_treatment_strata_stats_wages_stderr = appFinal_before_after %>%
  subset(abrio_cartilla == 1) %>%
  subset(Strata_2_label != "Safety and Reach") %>%
  filter(Treatment !=3) %>%
  filter(changed_app_after == 1) %>%
  arrange(Treatment_label) %>%
  group_by(Strata_2_label, Treatment_label) %>%
  summarise(Total = "-",
            #Asignados = round(100*sd(asignado)/sqrt(n()),2),
            #Asignago_change = round(100*sd(asignado_change)/sqrt(n()),2),
            #Program_asig_change = round(100*mean(program_asig_change),2),
            #Wage_change = round(mean(mid_wage_change[changed_app_after]/1e3, na.rm = TRUE),2),
            Cutoffs_initial_cars = round(sd(mean_cutoff_before, na.rm = TRUE)/sqrt(sum(!is.na(mean_cutoff_before))),2),
            #Cutoffs_after_cars = round(sd(mean_cutoff_after, na.rm = TRUE)/sqrt(sum(!is.na(mean_cutoff_after))),2),
            Cutoffs_new_cars = round(sd(mean_cutoff_new_cars, na.rm = TRUE)/sqrt(sum(!is.na(mean_cutoff_new_cars))),2),
            Wage_initial_cars = round(sd(mean_wage_before/1e3, na.rm = TRUE)/sqrt(sum(!is.na(mean_wage_before))),2),
            #Wage_after_cars = round(sd(mean_wage_after/1e3, na.rm = TRUE)/sqrt(sum(!is.na(mean_wage_after))),2),
            Wage_new_cars = round(sd(mean_wage_new_cars/1e3, na.rm = TRUE)/sqrt(sum(!is.na(mean_wage_new_cars))),2),
            Wage_new_cars_change = round(sd(mean_wage_new_cars_change/1e3, na.rm = TRUE)/sqrt(sum(!is.na(mean_wage_new_cars_change))),2))


#Write latex table by hand
table_latex_file = file(paste(path_draft_tables,"table_treatment_strata_stats_wages.tex",sep=""))
table_header = c("\\begin{table}[H]",
                 "\\centering",
                 "\\caption{Characteristics of added programs for students who modified their application lists}",
                 "\\label{tab:table_treatment_strata_stats_wages}",
                 "\\scalebox{0.6}{",
                 "\\begin{tabular}{lccccccc}",
                 "\\toprule",
                 "\\toprule")
table_columns = paste("\\makecell*[c]{Message  \\\\ type}",
                      "Treatment",
                      #"\\makecell*[c]{Opened  \\\\ website [\\%]}",
                      "Total",
                      #"Assigned [\\%]",
                      #"\\makecell*[c]{Changed  \\\\ assignment \\\\ state [\\%]}",
                      #"\\makecell*[c]{Changed  \\\\ program of \\\\ assignment [\\%]}",
                      #"\\makecell*[c]{Changed  \\\\ average wage \\\\ assignment [\\$ 1000]}",
                      "\\makecell*[c]{Average cutoff \\\\ initial \\\\  programs}",
                      "\\makecell*[c]{Average cutoff \\\\ added   programs}",
                      "\\makecell*[c]{Average wage initial \\\\  programs [\\$ 1000]}",
                      "\\makecell*[c]{Average wage added \\\\  programs [\\$ 1000]}",
                      "\\makecell*[c]{Changed  \\\\ average wage added \\\\ programs [\\$ 1000]}",
                      sep = " & ")
table_body = paste(table_columns, "\\\\" ," \\midrule")

space = '\\\\[0pt]'
table_stats = table_treatment_strata_stats_wages
table_sterr_stats = table_treatment_strata_stats_wages_stderr
#Fill table body
for(i in 1:nrow(table_stats)){
  row = c()
  for(j in 1:ncol(table_stats)){
    row_stat = table_stats[i,j]
    if(j == 1){
      row = row_stat
    }
    else{
      row = paste(row, "&", row_stat)
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
  # Paste row for standard errors
  row = c()
  for(j in 1:ncol(table_sterr_stats)){
    row_stat = table_sterr_stats[i,j]
    if(j != 1){
      if(j == 2){
        row = paste(row, "&", "")
      }else{
        row = paste(row, "&", paste("(",row_stat,")",sep=""))
      }
    }
  }
  row = paste(row, space)
  table_body = c(table_body, row)
}

table_footer = c("\\bottomrule",
                 "\\multicolumn{8}{l}{\\textit{Notes:} standard errors are computed in parenthesis.} \\\\",
                 "\\multicolumn{8}{l}{Average wages at the fourth year after graduation (thousands of Chilean pesos, nominal 2021).}",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
table_latex_code = c(table_header, table_body, table_footer)
writeLines(table_latex_code, table_latex_file)
close(table_latex_file)

#Save as csv
#write.csv2(table_treatment_strata_stats_2, file = "Outputs/Tables/table_treatment_strata_stats_2.csv", row.names = FALSE)

# Check if students who opened the website are different than students who did not in terms of observable characteristics
appFinal_before_after %>%
  group_by(Strata_2_label, abrio_cartilla) %>%
  summarise(mean_score = mean(PUNTAJE_CARR1.x),
            stderr_score = sd(PUNTAJE_CARR1.x)/sqrt(n()),
            mean_cutoff_before = mean(mean_cutoff_before, na.rm = TRUE))

appFinal_before_after %>%
  group_by(Strata_3, abrio_cartilla) %>%
  summarise(mean_score = mean(PUNTAJE_CARR1.x),
            stderr_score = sd(PUNTAJE_CARR1.x)/sqrt(n()),
            mean_cutoff_before = mean(mean_cutoff_before, na.rm = TRUE))

# Save data frame to do further analysis

save(appFinal_before_after, file = paste(path_intermediate_data, "appFinal_before_after.rds", sep=""))

## Treatment groups

# Grupo 0 recibio todo
# Grupo 1 recibio solo warnings
# Grupo 2 recibio solo recomendaciones
# Grupo 3 recibio intervencion de puntajes (Pumi)
# Treatment effects of Warnings: 0 minus 2
# Treatment effects of Recomendations: 0 minus 1



# UBO ---------------------------------------------------------------------

# App length UBO
appFinal %<>% mutate(napps_ubo = (as.numeric(substr(CAR_CODIGO_1PREFER, 1, 2)) == 53) + (as.numeric(substr(CAR_CODIGO_2PREFER, 1, 2)) == 53) +
                  (as.numeric(substr(CAR_CODIGO_3PREFER, 1, 2)) == 53) + (as.numeric(substr(CAR_CODIGO_4PREFER, 1, 2)) == 53) +
                  (as.numeric(substr(CAR_CODIGO_5PREFER, 1, 2)) == 53) + (as.numeric(substr(CAR_CODIGO_6PREFER, 1, 2)) == 53) +
                  (as.numeric(substr(CAR_CODIGO_7PREFER, 1, 2)) == 53) + (as.numeric(substr(CAR_CODIGO_8PREFER, 1, 2)) == 53) +
                  (as.numeric(substr(CAR_CODIGO_9PREFER, 1, 2)) == 53) + (as.numeric(substr(CAR_CODIGO_10PREFER, 1, 2)) == 53))

table(appFinal$napps_ubo)

# UBO error students
UBO_ErrorList %<>% mutate(UBO_error = 1) %>% dplyr::rename(MRUN = V1)
nrow(UBO_ErrorList)

# Merge with appFinal
app_UBO_error = merge(appFinal, UBO_ErrorList, by = 'MRUN', all = FALSE)

# Analysis
table(app_UBO_error$NRO_POSTULACION)

# Application length
table(app_UBO_error$app_length)

# Applications before cartilla
app_UBO_error_before_cartilla = app_UBO_error %>% subset((FECHA < "2021-02-14") |
                                                           (FECHA == "2021-02-14" & (strptime(HORA, "%H:%M") < strptime("11:49", "%H:%M"))))
# Select the last application by student
app_UBO_error_before_cartilla_summary = app_UBO_error_before_cartilla %>% group_by(MRUN) %>% summarise(max_app_nro = max(NRO_POSTULACION))
# Merge stat
app_UBO_error_before_cartilla = merge(app_UBO_error_before_cartilla,
                                     app_UBO_error_before_cartilla_summary,
                                     by = "MRUN", all.x = TRUE)
# Select last application
app_UBO_error_before_cartilla %<>% subset(NRO_POSTULACION == max_app_nro)
#Stats
mean(app_UBO_error_before_cartilla$app_length)
mean(app_UBO_error_before_cartilla$napps_ubo)

# Applications after cartilla
app_UBO_error_after_cartilla = app_UBO_error %>% subset((FECHA >= "2021-02-14") |
                                                           (FECHA == "2021-02-14" & (strptime(HORA, "%H:%M") >= strptime("11:49", "%H:%M"))))
# Select the last application by student
app_UBO_error_after_cartilla_summary = app_UBO_error_after_cartilla %>% group_by(MRUN) %>% summarise(max_app_nro = max(NRO_POSTULACION))
# Merge stat
app_UBO_error_after_cartilla = merge(app_UBO_error_after_cartilla,
                                     app_UBO_error_after_cartilla_summary,
                                     by = "MRUN", all.x = TRUE)
# Select last application
app_UBO_error_after_cartilla %<>% subset(NRO_POSTULACION == max_app_nro)
#Stats
mean(app_UBO_error_after_cartilla$app_length)
mean(app_UBO_error_after_cartilla$napps_ubo)

# Merge by MRUN applicants that appear in both data sets
app_UBO_error_before_cartilla %<>% dplyr::rename(napps_ubo_before = napps_ubo)
app_UBO_error_after_cartilla %<>% dplyr::rename(napps_ubo_after = napps_ubo)
app_UBO_before_after = merge(app_UBO_error_before_cartilla %>% select(MRUN, napps_ubo_before),
                             app_UBO_error_after_cartilla %>% select(MRUN, napps_ubo_after),
                             by = "MRUN")
app_UBO_before_after %<>% mutate(n_apps_ubo_change = napps_ubo_after - napps_ubo_before)
# Change in number of applications
sum(app_UBO_before_after$n_apps_ubo_change < 0)

mean(app_UBO_before_after$n_apps_ubo_change)
hist(app_UBO_before_after$n_apps_ubo_change)






#Anaïs

#load("~/Dropbox/Mistakes Structural/Data/intermediate_data/appFinal_before_after.rds")

#Create some new variables related to the programs added/removed from the initial ROL:

    #Number of new programs introduced in the final ROL:
appFinal_before_after$nber_progr_added = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("CAR_CODIGO_",k,"PREFER.x", sep = "")])
    cars_after_set = c(cars_after_set,x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  
  cars_new = setdiff(as.numeric(cars_after_set), as.numeric(cars_before_set))
  
  # Filter NAs and -1
  cars_new = cars_new[!is.na(cars_new)]
  cars_new = cars_new[cars_new != -1]
  
  #Nber of new programs:
  cars_new_length = length(cars_new)
  return(cars_new_length)
})


    #Number of new programs removed from the initial ROL:
appFinal_before_after$nber_progr_removed = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("CAR_CODIGO_",k,"PREFER.x", sep = "")])
    cars_after_set = c(cars_after_set,x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  cars_removed = setdiff(as.numeric(cars_before_set), as.numeric(cars_after_set))
  # Filter NAs and -1
  cars_removed = cars_removed[!is.na(cars_removed)]
  cars_removed = cars_removed[cars_removed != -1]
  
  #Nber of removed programs:
  cars_removed_length = length(cars_removed)
  return(cars_removed_length)
})


    #Share of valid programs among programs added to the final ROL:
appFinal_before_after$share_valid_added_progr = apply(appFinal_before_after, MARGIN = 1, FUN = function(x){
  
  cars_before_set = c()
  cars_after_set = c()
  for(k in c("1","2","3","4","5","6","7","8","9","10")){
    cars_before_set = c(cars_before_set,x[paste("CAR_CODIGO_",k,"PREFER.x", sep = "")])
    cars_after_set = c(cars_after_set,x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")])
  }
  if(x["changed_app_after"] == 0){
    cars_after_set = cars_before_set
  } 
  cars_new = setdiff(as.numeric(cars_after_set), as.numeric(cars_before_set))
  # Filter NAs and -1
  cars_new = cars_new[!is.na(cars_new)]
  cars_new = cars_new[cars_new != -1]
  #Nber of new programs:
  cars_new_length = length(cars_new)
  
  
  #Validity of new programs
  
  cars_after_score = c()
  for(j in cars_new){
    for(k in c("1","2","3","4","5","6","7","8","9","10")){
      cars_after_score = c(cars_after_score,ifelse(x[paste("CAR_CODIGO_",k,"PREFER.y", sep = "")]==j, x[paste("PUNTAJE_CARR",k,".y", sep = "")], NA))
    }
  } 
  
  # Filter NAs
  cars_after_score = cars_after_score[!is.na(cars_after_score)]
  
  #Share of valid added programs
  invalid_programs_after = sum(as.numeric(cars_after_score) != -1)
  share_valid_added_progr = ifelse(cars_new_length!=0, invalid_programs_after/cars_new_length, NA)
  return(share_valid_added_progr)
})



    #Generate some variables used for analysis: difference between cutoffs, wages and length of initial vs final ROL:
appFinal_before_after %<>% mutate(change_mean_cutoff = mean_cutoff_after - mean_cutoff_before)
appFinal_before_after %<>% mutate(change_mean_wage = mean_wage_after - mean_wage_before)

    #Generate bundled treatment group variables, grouping T0 and T1, T0 and T2 and T0, T1 and T2:
appFinal_before_after %<>% mutate(Treatment_01 = ifelse(Treatment_0==1 | Treatment_1==1, 1 ,0))
appFinal_before_after %<>% mutate(Treatment_02 = ifelse(Treatment_0==1 | Treatment_2==1, 1 ,0))
appFinal_before_after %<>% mutate(Treatment_012 = ifelse(Treatment_0==1 | Treatment_1==1 | Treatment_2==1, 1 ,0))


    #Load the file containing students' test scores:
score <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/adm2021_archivo_C_demre_2021feb08_MRUN_Compartir.csv', sep=";", header=TRUE)
sum(duplicated(score$mrun))                                            #Check duplicates
score %<>% dplyr::rename(MRUN = mrun)                                  #Rename student ID

    #Merge this dataset to appFinal_before_after:
appFinal_before_after = merge(appFinal_before_after,
                              score %>% select(MRUN, promedio_cm_actual,  promedio_lm_anterior, pace, percentil_cm_actual, percentil_lm_anterior, clec_actual, mate_actual, cien_actual, hcso_actual),
                              by = "MRUN", all.x = TRUE)

    #Make score numeric
appFinal_before_after %<>% mutate(promedio_cm_actual = as.numeric(gsub(",", ".", promedio_cm_actual)))
appFinal_before_after %<>% mutate(promedio_lm_anterior = as.numeric(gsub(",", ".", promedio_lm_anterior)))

    #Check randomization in terms of score/PACE status:
table(appFinal_before_after %>% subset(promedio_cm_actual<450 & promedio_lm_anterior<450) %>% select(Treatment), useNA = "always")
            ##2124 students in T0, 2146 in T1, 2105 in T2
table(appFinal_before_after %>% subset(pace=="PACE") %>% select(Treatment), useNA = "always")
            ##85 students in T0, 71 in T1, 52 in T2
table(appFinal_before_after %>% subset(pace=="PACE" & promedio_cm_actual<450 & promedio_lm_anterior<450) %>% select(Treatment), useNA = "always")

    #Replace weigthed scores with those of initial ROL if the student did not modify it:
appFinal_before_after %<>% mutate(PUNTAJE_CARR1.y = ifelse(changed_app_after==0,PUNTAJE_CARR1.x, PUNTAJE_CARR1.y ) ) 
appFinal_before_after %<>% mutate(PUNTAJE_CARR2.y = ifelse(changed_app_after==0,PUNTAJE_CARR2.x, PUNTAJE_CARR2.y ) ) 
appFinal_before_after %<>% mutate(PUNTAJE_CARR3.y = ifelse(changed_app_after==0,PUNTAJE_CARR3.x, PUNTAJE_CARR3.y ) ) 
appFinal_before_after %<>% mutate(PUNTAJE_CARR4.y = ifelse(changed_app_after==0,PUNTAJE_CARR4.x, PUNTAJE_CARR4.y ) ) 
appFinal_before_after %<>% mutate(PUNTAJE_CARR5.y = ifelse(changed_app_after==0,PUNTAJE_CARR5.x, PUNTAJE_CARR5.y ) ) 
appFinal_before_after %<>% mutate(PUNTAJE_CARR6.y = ifelse(changed_app_after==0,PUNTAJE_CARR6.x, PUNTAJE_CARR6.y ) ) 
appFinal_before_after %<>% mutate(PUNTAJE_CARR7.y = ifelse(changed_app_after==0,PUNTAJE_CARR7.x, PUNTAJE_CARR7.y ) ) 
appFinal_before_after %<>% mutate(PUNTAJE_CARR8.y = ifelse(changed_app_after==0,PUNTAJE_CARR8.x, PUNTAJE_CARR8.y ) ) 
appFinal_before_after %<>% mutate(PUNTAJE_CARR9.y = ifelse(changed_app_after==0,PUNTAJE_CARR9.x, PUNTAJE_CARR9.y ) ) 
appFinal_before_after %<>% mutate(PUNTAJE_CARR10.y = ifelse(changed_app_after==0,PUNTAJE_CARR10.x, PUNTAJE_CARR10.y ) ) 


    #Replace missing weighted scores (NA) with zeros:
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR1.y), "PUNTAJE_CARR1.y"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR2.y), "PUNTAJE_CARR2.y"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR3.y), "PUNTAJE_CARR3.y"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR4.y), "PUNTAJE_CARR4.y"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR5.y), "PUNTAJE_CARR5.y"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR6.y), "PUNTAJE_CARR6.y"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR7.y), "PUNTAJE_CARR7.y"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR8.y), "PUNTAJE_CARR8.y"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR9.y), "PUNTAJE_CARR9.y"] = 0
appFinal_before_after[is.na(appFinal_before_after$PUNTAJE_CARR10.y), "PUNTAJE_CARR10.y"] = 0


    #Create a dummy variable equal to one if the student submitted at least one invalid application in his final ROL:
appFinal_before_after %<>% mutate(error_after = ifelse(((PUNTAJE_CARR1.y == -1) |
                                                          (PUNTAJE_CARR2.y == -1) |
                                                          (PUNTAJE_CARR3.y == -1) |
                                                          (PUNTAJE_CARR4.y == -1) |
                                                          (PUNTAJE_CARR5.y == -1) |
                                                          (PUNTAJE_CARR6.y == -1) |
                                                          (PUNTAJE_CARR7.y == -1) |
                                                          (PUNTAJE_CARR8.y == -1) |
                                                          (PUNTAJE_CARR9.y == -1) |
                                                          (PUNTAJE_CARR10.y == -1)), 1, 0))

table(appFinal_before_after$error_before)
table(appFinal_before_after[appFinal_before_after$error_before==1,]$error_after)   
table(appFinal_before_after[appFinal_before_after$changed_app_after==1 & appFinal_before_after$error_before==1,]$error_after)   


############################################################BALANCE TESTS############################################################

#Balance Tests T3 vs others:
balance1 <- lm(data = appFinal_before_after, formula = promedio_cm_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance1)
balance2 <- lm(data = appFinal_before_after , formula = cien_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance2)
balance3 <- lm(data = appFinal_before_after, formula = hcso_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance3)
balance4 <- lm(data = appFinal_before_after, formula = mean_cutoff_before~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance4)
balance5 <- lm(data = appFinal_before_after, formula = mean_wage_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance5)
balance6 <- lm(data = appFinal_before_after, formula = PUNTAJE_CARR1.x ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance6)
stargazer(balance1, balance2, balance3, balance4, balance5, balance6, title="Balance Tests",
          dep.var.labels=c("Mean Math-Language Score", "Score Science Test", "Score HCSO Test", "Mean Cutoff (Inital ROL)", "Mean Wage (Initial ROL)", "Mean score (1rst Choice)"),
          covariate.labels=c("Treatment 0", "Treatment 1", "Treatment 2"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)


#Dividing the T3 group in different subgroups:

#Test1: all those with math-language >= 450:
appFinal_before_after %<>% mutate(T3_test1 = ifelse(Treatment_3==1 & ((promedio_cm_actual>=450 | promedio_lm_anterior >=450)), 1, 0))
table(appFinal_before_after$T3_test1)
View(appFinal_before_after[appFinal_before_after$T3_test1==1,])

#Test2: those with math-language >= 450, and without a PACE application but with an application mistake on their top choice:
appFinal_before_after %<>% mutate(T3_test2 = ifelse(Treatment_3==1 & ((promedio_cm_actual>=450 | promedio_lm_anterior >=450) & pace!="PACE" & PUNTAJE_CARR1.x==-1), 1, 0))
table(appFinal_before_after$T3_test2)
View(appFinal_before_after[appFinal_before_after$T3_test2==1,])

#Test3: those with math-language >= 450, with a PACE application, and no application mistake in the top choice:
appFinal_before_after %<>% mutate(T3_test3 = ifelse(Treatment_3==1 & ((promedio_cm_actual>=450 | promedio_lm_anterior >=450) & PUNTAJE_CARR1.x!=-1 & pace=="PACE"), 1, 0))
table(appFinal_before_after$T3_test3)
View(appFinal_before_after[appFinal_before_after$T3_test3==1,])

#Test4: those with math-language >= 450, without application mistake and without PACE application:
appFinal_before_after %<>% mutate(T3_test4 = ifelse(Treatment_3==1 & ((promedio_cm_actual>=450 | promedio_lm_anterior >=450) & PUNTAJE_CARR1.x!=-1 & pace!="PACE"), 1, 0))
table(appFinal_before_after$T3_test4)
View(appFinal_before_after[appFinal_before_after$T3_test4==1,])



#Balance Test 1 vs T0-2:                                                  
balance1_T3_test1 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1), formula = promedio_cm_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance1_T3_test1)
balance2_T3_test1 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1), formula = cien_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance2_T3_test1)
balance3_T3_test1 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1), formula = hcso_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance3_T3_test1)
balance4_T3_test1 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1), formula = mean_cutoff_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance4_T3_test1)
balance5_T3_test1 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1), formula = mean_wage_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance5_T3_test1)
balance6_T3_test1 <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1) & (CAR_CODIGO_2PREFER.x!=-1 & PUNTAJE_CARR2.x!=-1)), formula = PUNTAJE_CARR2.x ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance6_T3_test1)

#Balance Test 1 vs T0-2 + sample restricted to those with at least an application mistake in their initial ROL:                                                  
balance1_T3_test1_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1) & error_before==1), formula = promedio_cm_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance1_T3_test1_error)
balance2_T3_test1_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1)  & error_before==1), formula = cien_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance2_T3_test1_error)
balance3_T3_test1_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1) & error_before==1), formula = hcso_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance3_T3_test1_error)
balance4_T3_test1_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1)  & error_before==1), formula = mean_cutoff_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance4_T3_test1_error) 
balance5_T3_test1_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1)  & error_before==1), formula = mean_wage_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance5_T3_test1_error)
balance6_T3_test1_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test1==1)  & error_before==1 & (CAR_CODIGO_2PREFER.x!=-1 & PUNTAJE_CARR2.x!=-1)), formula = PUNTAJE_CARR2.x ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance6_T3_test1_error)


#Balance Test 2 vs T0-2::                                                  
balance1_T3_test2 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1), formula = promedio_cm_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance1_T3_test2)
balance2_T3_test2 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1), formula = cien_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance2_T3_test2)
balance3_T3_test2 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1), formula = hcso_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance3_T3_test2)
balance4_T3_test2 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1), formula = mean_cutoff_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance4_T3_test2)
balance5_T3_test2 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1), formula = mean_wage_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance5_T3_test2)
balance6_T3_test2 <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & (CAR_CODIGO_2PREFER.x!=-1 & PUNTAJE_CARR2.x!=-1)), formula = PUNTAJE_CARR2.x ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance6_T3_test2)



#Balance Test 2 vs T0-2 + sample restricted to those with at least an application mistake in their initial ROL:                                                 
balance1_T3_test2_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = promedio_cm_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance1_T3_test2_error)
balance2_T3_test2_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1)  & error_before==1), formula = cien_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance2_T3_test2_error)
balance3_T3_test2_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = hcso_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance3_T3_test2_error)
balance4_T3_test2_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1)  & error_before==1), formula = mean_cutoff_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance4_T3_test2_error) 
balance5_T3_test2_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1)  & error_before==1), formula = mean_wage_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance5_T3_test2_error)
balance6_T3_test2_error <- lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1)  & error_before==1 & (CAR_CODIGO_2PREFER.x!=-1 & PUNTAJE_CARR2.x!=-1)), formula = PUNTAJE_CARR2.x ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance6_T3_test2_error)



#Balance Test 3 vs T0-2:                                                  
balance1_T3_test3 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test3==1), formula = promedio_cm_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance1_T3_test3)
balance2_T3_test3 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test3==1), formula = cien_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance2_T3_test3)
balance3_T3_test3 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test3==1), formula = hcso_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance3_T3_test3)
balance4_T3_test3 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test3==1), formula = mean_cutoff_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance4_T3_test3)
balance5_T3_test3 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test3==1), formula = mean_wage_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance5_T3_test3)
balance6_T3_test3 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test3==1), formula = PUNTAJE_CARR1.x ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance6_T3_test3)

#Balance Test 4 vs T0-2:                                                  
balance1_T3_test4 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test4==1), formula = promedio_cm_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance1_T3_test4)
balance2_T3_test4 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test4==1), formula = cien_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance2_T3_test4)
balance3_T3_test4 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test4==1), formula = hcso_actual ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance3_T3_test4)
balance4_T3_test4 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test4==1), formula = mean_cutoff_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance4_T3_test4)
balance5_T3_test4 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test4==1), formula = mean_wage_before ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance5_T3_test4)
balance6_T3_test4 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test4==1), formula = PUNTAJE_CARR1.x ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(balance6_T3_test4)


#Generate tables for balance tests:
stargazer(balance1_T3_test1, balance2_T3_test1, balance3_T3_test1, balance4_T3_test1, balance5_T3_test1, balance6_T3_test1, title="Balance Tests",
          dep.var.labels=c("Mean Math-Language Score", "Score Science Test", "Score HCSO Test", "Mean Cutoff (Inital ROL)", "Mean Wage (Initial ROL)", "Mean score (1rst Choice)"),
          covariate.labels=c("Treatment 0", "Treatment 1", "Treatment 2"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)
stargazer(balance1_T3_test2, balance2_T3_test2, balance3_T3_test2, balance4_T3_test2, balance5_T3_test2, balance6_T3_test2, title="Balance Tests",
          dep.var.labels=c("Mean Math-Language Score", "Score Science Test", "Score HCSO Test", "Mean Cutoff (Inital ROL)", "Mean Wage (Initial ROL)", "Mean score (1rst Choice)"),
          covariate.labels=c("Treatment 0", "Treatment 1", "Treatment 2"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)
stargazer(balance1_T3_test2_error, balance2_T3_test2_error, balance3_T3_test2_error, balance4_T3_test2_error, balance5_T3_test2_error, balance6_T3_test2_error, title="Balance Tests",
          dep.var.labels=c("Mean Math-Language Score", "Score Science Test", "Score HCSO Test", "Mean Cutoff (Inital ROL)", "Mean Wage (Initial ROL)", "Mean score (1rst Choice)"),
          covariate.labels=c("Treatment 0", "Treatment 1", "Treatment 2"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)
stargazer(balance1_T3_test3, balance2_T3_test3, balance3_T3_test3, balance4_T3_test3, balance5_T3_test3, balance6_T3_test3, title="Balance Tests",
          dep.var.labels=c("Mean Math-Language Score", "Score Science Test", "Score HCSO Test", "Mean Cutoff (Inital ROL)", "Mean Wage (Initial ROL)", "Mean score (1rst Choice)"),
          covariate.labels=c("Treatment 0", "Treatment 1", "Treatment 2"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)



############################################################ITT ANALYSIS############################################################

#######Pure ITT: T2 vs T1 and T0#######
ITT_1 = lm(data = appFinal_before_after %>% subset(Treatment_3 != 1), formula = changed_app_after ~ Treatment_0 + Treatment_1)
summary(ITT_1)
ITT_2 = lm(data = appFinal_before_after %>% subset(Treatment_3 != 1), formula = app_length_change ~ Treatment_0 + Treatment_1)
summary(ITT_2)
ITT_3 = lm(data = appFinal_before_after %>% subset(Treatment_3 != 1), formula = nber_progr_added ~ Treatment_0 + Treatment_1)
summary(ITT_3)
ITT_4 = lm(data = appFinal_before_after %>% subset(Treatment_3 != 1), formula = change_mean_cutoff ~ Treatment_0 + Treatment_1)
summary(ITT_4)
ITT_5 = lm(data = appFinal_before_after %>% subset(Treatment_3 != 1), formula = change_mean_wage ~ Treatment_0 + Treatment_1)
summary(ITT_5)
ITT_6 = lm(data = appFinal_before_after %>% subset(Treatment_3 != 1 & error_before==1), formula = error_after ~ Treatment_0 + Treatment_1)
summary(ITT_6)
ITT_7 = lm(data = appFinal_before_after %>% subset(Treatment_3 != 1 & asignado_before==0), formula = asignado ~ Treatment_0 + Treatment_1)
summary(ITT_7)
ITT_8 = lm(data = appFinal_before_after %>% subset(Treatment_3 != 1 & asignado_before==1), formula = program_asig_change ~ Treatment_0 + Treatment_1)
summary(ITT_8)

stargazer(ITT_1, ITT_2, ITT_4, ITT_5, ITT_6, ITT_7, ITT_8 , title="Intention-to-Treat",
          dep.var.labels=c("Change ROL", "Change length ROL", "Change Mean Cutoff", "Change Mean Wage", "Mistake After", "Assigned After", "Change in Program Assigned"),
          covariate.labels=c("T0", "T1"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)


#######Pure ITT restricted to students with at least one application mistake ex-ante: Sub-Group 2 of T3 vs T2 and T1 and T0#######
ITT_1_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = changed_app_after ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_1_error)
ITT_2_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = app_length_change ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_2_error)
ITT_3_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = nber_progr_added ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_3_error)
ITT_4_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = change_mean_cutoff ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_4_error)
ITT_5_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = change_mean_wage ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_5_error)
ITT_6_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = error_after ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_6_error)
ITT_7_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1)  & asignado_before==0 & error_before==1), formula = asignado ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_7_error)
ITT_8_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1)  & asignado_before==1 & error_before==1), formula = program_asig_change ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_8_error)

stargazer(ITT_1_error, ITT_2_error, ITT_4_error, ITT_5_error, ITT_6_error, ITT_7_error, ITT_8_error , title="Intention-to-Treat - Mistakes Only",
          dep.var.labels=c("Change ROL", "Change length ROL", "Change Mean Cutoff", "Change Mean Wage", "Mistake After", "Assigned After", "Change in Program Assigned"),
          covariate.labels=c("T0", "T1", "T2"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)



#######Pure ITT restricted to students with at least one application mistake ex-ante: Sub-Group 2 of T3 vs T2 and T1 and T0 - Validity of Programs Added#######

      #Create some additional variables:
          #Dummy for whether the student introduced or removed a program in the final ROL compared to the initial one:
appFinal_before_after %<>% mutate(added_progr = ifelse(nber_progr_added>0,1,0))
appFinal_before_after %<>% mutate(removed_progr = ifelse(nber_progr_removed>0,1,0))

          #Dummy=1 if the student reduced/increased the number of invalid programs ranked in his final ROL compared to the initial one
appFinal_before_after %<>% mutate(invalid_programs_removed_dummy = ifelse(invalid_programs_change<0,1,0))
appFinal_before_after %<>% mutate(more_invalid = ifelse(invalid_programs_change>0,1,0))

          #Dummy=1 if the student reduced/increased the share of invalid program in his final ROL compared to the initial one
appFinal_before_after %<>% mutate(invalid_programs_share_reduced_dummy = ifelse(invalid_programs_share_change<0,1,0))
appFinal_before_after %<>% mutate(invalid_programs_share_increase_dummy = ifelse(invalid_programs_share_change>0,1,0))


      #Regressions:
ITT_9_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = nber_progr_added ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_9_error)
ITT_10_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = nber_progr_removed ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_10_error)
ITT_11_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = error_after ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_11_error)
ITT_12_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = invalid_programs_removed_dummy ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_12_error)
ITT_13_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = more_invalid ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_13_error)
ITT_14_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = invalid_programs_share_reduced_dummy ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_14_error)
ITT_15_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = invalid_programs_share_increase_dummy ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_15_error)
ITT_16_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = invalid_programs_change ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_16_error)
ITT_17_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = invalid_programs_share_change ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_17_error)
ITT_18_error = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1), formula = share_valid_added_progr ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_18_error)


stargazer(ITT_11_error, ITT_12_error, ITT_13_error, ITT_14_error, ITT_18_error , title="Intention-to-Treat - Mistakes Only",
          dep.var.labels=c("Mistake Ex-Post", "Reduced # Mistake", "Increase # Mistake", "Reduced Share Mistake", "Share Valid Programs among Added Programs"),
          covariate.labels=c("T0", "T1", "T2"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)


#######Pure ITT - Change in Risk#######

    #Load Initial Risk Database
risk_initial <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/bootstrap_interim_admission_probabilities/applications_and_probs_last.csv', sep=';', header=TRUE)
risk_initial %<>% select(MRUN, overall_prob)
risk_initial %<>% dplyr::rename(overall_prob_initial=overall_prob)

    #Load Final Risk Database
risk_final <- read.csv('~/Dropbox/Mistakes Structural/Data/2021/bootstrap_final_admission_probabilities/applications_and_probs_last.csv', sep=';', header=TRUE)
risk_final %<>% select(MRUN, overall_prob)
risk_final %<>% dplyr::rename(overall_prob_final=overall_prob)

    #Merge both datasets
risk = merge(risk_final, risk_initial, by="MRUN", all.x = TRUE)
remove(risk_initial, risk_final)

    #Merge to main dataset
appFinal_before_after = merge(appFinal_before_after, risk, by="MRUN", all.x=TRUE)  

    #Compute the risk and the change in risk
appFinal_before_after %<>% mutate(risk_initial = 1-overall_prob_initial)
appFinal_before_after %<>% mutate(risk_final = 1-overall_prob_final)
appFinal_before_after %<>% mutate(change_risk = risk_final-risk_initial)
appFinal_before_after %<>% mutate(reduced_risk = ifelse(change_risk<0, 1, 0))
appFinal_before_after %<>% mutate(increased_risk = ifelse(change_risk>0, 1, 0))


    #Regressions: T0 & T1 vs T2
ITT_1_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 ) &  risk_initial!=0), formula = change_risk ~ Treatment_0 + Treatment_1)
summary(ITT_1_risk)
ITT_2_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  risk_initial<=0.01), formula = change_risk ~ Treatment_0 + Treatment_1)
summary(ITT_2_risk)
ITT_3_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  (risk_initial>0.01 & risk_initial<=0.30)), formula = change_risk ~ Treatment_0 + Treatment_1)
summary(ITT_3_risk)
ITT_4_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  (risk_initial>0.30 & risk_initial<=0.70)), formula = change_risk ~ Treatment_0 + Treatment_1)
summary(ITT_4_risk)
ITT_5_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  risk_initial>0.7), formula = change_risk ~ Treatment_0 + Treatment_1)
summary(ITT_5_risk)
ITT_6_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  risk_initial==1), formula = change_risk ~ Treatment_0 + Treatment_1)
summary(ITT_6_risk)


ITT_1_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  risk_initial!=0), formula = reduced_risk ~ Treatment_0 + Treatment_1)
summary(ITT_1_reducedrisk)
ITT_2_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  risk_initial<=0.01), formula = reduced_risk ~ Treatment_0 + Treatment_1)
summary(ITT_2_reducedrisk)
ITT_3_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  (risk_initial>0.01 & risk_initial<=0.30)), formula = reduced_risk ~ Treatment_0 + Treatment_1)
summary(ITT_3_reducedrisk)
ITT_4_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  (risk_initial>0.30 & risk_initial<=0.70)), formula = reduced_risk ~ Treatment_0 + Treatment_1)
summary(ITT_4_reducedrisk)
ITT_5_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  risk_initial>0.7), formula = reduced_risk ~ Treatment_0 + Treatment_1 )
summary(ITT_5_reducedrisk)
ITT_6_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  risk_initial==1), formula = reduced_risk ~ Treatment_0 + Treatment_1)
summary(ITT_6_reducedrisk)


ITT_1_increasedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  risk_initial<=0.01), formula = increased_risk ~ Treatment_0 + Treatment_1)
summary(ITT_1_increasedrisk)
ITT_2_increasedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1) &  (risk_initial>0.01 & risk_initial<=0.30)), formula = increased_risk ~ Treatment_0 + Treatment_1)
summary(ITT_2_increasedrisk)


stargazer(ITT_1_risk, ITT_2_risk, ITT_3_risk, ITT_4_risk, ITT_5_risk, ITT_6_risk, ITT_1_reducedrisk, title="Intention-to-Treat - Risk",
          dep.var.labels=c("Change Risk", "Reduced Risk"),
          covariate.labels=c("T0", "T1"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)




      #Regressions for students with at least one application mistake ex-ante: Sub-Group 2 of T3 vs T2 and T1 and T0

ITT_1_error_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & risk_initial!=0), formula = change_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_1_error_risk)
ITT_2_error_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & risk_initial<=0.01), formula = change_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_2_error_risk)
ITT_3_error_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & (risk_initial>0.01 & risk_initial<=0.30)), formula = change_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_3_error_risk)
ITT_4_error_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & (risk_initial>0.30 & risk_initial<=0.70)), formula = change_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_4_error_risk)
ITT_5_error_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & risk_initial>0.7), formula = change_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_5_error_risk)
ITT_6_error_risk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & risk_initial==1), formula = change_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_6_error_risk)


ITT_1_error_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & risk_initial!=0), formula = reduced_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_1_error_reducedrisk)
ITT_2_error_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & risk_initial<=0.01), formula = reduced_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_2_error_reducedrisk)
ITT_3_error_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & (risk_initial>0.01 & risk_initial<=0.30)), formula = reduced_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_3_error_reducedrisk)
ITT_4_error_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & (risk_initial>0.30 & risk_initial<=0.70)), formula = reduced_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_4_error_reducedrisk)
ITT_5_error_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & risk_initial>0.7), formula = reduced_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_5_error_reducedrisk)
ITT_6_error_reducedrisk = lm(data = appFinal_before_after %>% subset((Treatment_0==1 | Treatment_1==1 | Treatment_2==1 | T3_test2==1) & error_before==1 & risk_initial==1), formula = reduced_risk ~ Treatment_0 + Treatment_1 + Treatment_2)
summary(ITT_6_error_reducedrisk)



stargazer(ITT_1_error_risk, ITT_2_error_risk, ITT_3_error_risk, ITT_4_error_risk, ITT_5_error_risk, ITT_6_error_risk, ITT_1_error_reducedrisk, title="Intention-to-Treat - Risk - Mistakes Only",
          dep.var.labels=c("Change Risk", "Reduced Risk"),
          covariate.labels=c("T0", "T1", "T2"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)



############################################################WITHIN-TREATMENT ANALYSIS############################################################

#Balance Test Within Treatment Group, matched sample, controlling for treatment arm:
balance1_within <- lm(data = appFinal_before_after %>% subset(Treatment_012==1 & propensity_score_sample==1), formula = promedio_cm_actual ~ abrio_cartilla + Treatment_0 + Treatment_1)
summary(balance1_within)
balance2_within <- lm(data = appFinal_before_after %>% subset(Treatment_012==1 & propensity_score_sample==1), formula = cien_actual ~ abrio_cartilla+ Treatment_0 + Treatment_1)
summary(balance2_within)
balance3_within <- lm(data = appFinal_before_after %>% subset(Treatment_012==1 & propensity_score_sample==1), formula = hcso_actual ~ abrio_cartilla + Treatment_0 + Treatment_1)
summary(balance3_within)
balance4_within <- lm(data = appFinal_before_after %>% subset(Treatment_012==1 & propensity_score_sample==1), formula = mean_cutoff_before ~ abrio_cartilla  + Treatment_0 + Treatment_1)
summary(balance4_within)
balance5_within <- lm(data = appFinal_before_after %>% subset(Treatment_012==1 & propensity_score_sample==1), formula = mean_wage_before ~ abrio_cartilla + Treatment_0 + Treatment_1)
summary(balance5_within)
balance6_within <- lm(data = appFinal_before_after %>% subset(Treatment_012==1 & propensity_score_sample==1), formula = PUNTAJE_CARR1.x ~ abrio_cartilla + Treatment_0 + Treatment_1)
summary(balance6_within)

stargazer(balance1_within,balance2_within, balance3_within, balance4_within, balance5_within, balance6_within, title="Balance - Within",
          dep.var.labels=c("Mean Math-Language Score", "Score Science Test", "Score HCSO Test", "Mean Cutoff (Inital ROL)", "Mean Wage (Initial ROL)", "Mean score (1rst Choice)"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)




#Testing different propensity score specifications:

    #T0:
balance1_within0 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 & propensity_score_sample==1), formula = mean_cutoff_before ~ abrio_cartilla  )
summary(balance1_within0)
balance1_within0_anais <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 & propensity_score_sample_anais==1), formula = mean_cutoff_before ~ abrio_cartilla  )
summary(balance1_within0_anais)
balance2_within0 <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 & propensity_score_sample==1), formula = mean_wage_before ~ abrio_cartilla )
summary(balance2_within0)
balance2_within0_anais <- lm(data = appFinal_before_after %>% subset(Treatment_0==1 & propensity_score_sample_anais==1), formula = mean_wage_before ~ abrio_cartilla )
summary(balance2_within0_anais)

    #T1:
balance1_within1 <- lm(data = appFinal_before_after %>% subset(Treatment_1==1 & propensity_score_sample==1), formula = mean_cutoff_before ~ abrio_cartilla  )
summary(balance1_within1)
balance1_within1_anais <- lm(data = appFinal_before_after %>% subset(Treatment_1==1 & propensity_score_sample_anais==1), formula = mean_cutoff_before ~ abrio_cartilla  )
summary(balance1_within1_anais)
balance2_within1 <- lm(data = appFinal_before_after %>% subset(Treatment_1==1 & propensity_score_sample==1), formula = mean_wage_before ~ abrio_cartilla )
summary(balance2_within1)
balance2_within1_anais <- lm(data = appFinal_before_after %>% subset(Treatment_1==1 & propensity_score_sample_anais==1), formula = mean_wage_before ~ abrio_cartilla )
summary(balance2_within1_anais)

    #T2:
balance1_within2 <- lm(data = appFinal_before_after %>% subset(Treatment_2==1 & propensity_score_sample==1), formula = mean_cutoff_before ~ abrio_cartilla  )
summary(balance1_within2)
balance1_within2_anais <- lm(data = appFinal_before_after %>% subset(Treatment_2==1 & propensity_score_sample_anais==1), formula = mean_cutoff_before ~ abrio_cartilla  )
summary(balance1_within2_anais)
balance2_within2 <- lm(data = appFinal_before_after %>% subset(Treatment_2==1 & propensity_score_sample==1), formula = mean_wage_before ~ abrio_cartilla )
summary(balance2_within2)
balance2_within2_anais <- lm(data = appFinal_before_after %>% subset(Treatment_2==1 & propensity_score_sample_anais==1), formula = mean_wage_before ~ abrio_cartilla )
summary(balance2_within2_anais)


stargazer(balance1_within0, balance1_within0_anais, balance1_within1, balance1_within1_anais, balance1_within2, balance1_within2_anais, title="Balance - Mean Cutoffs",
          dep.var.labels=c("Mean Cutoff (Inital ROL)"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)


stargazer(balance2_within0, balance2_within0_anais, balance2_within1, balance2_within1_anais, balance2_within2, balance2_within2_anais, title="Balance - Mean Wages",
          dep.var.labels=c("Mean Wage (Inital ROL)"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)



#Within Treatment - Regressions

##Outcome: Changed Application Ex-Post
within_1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_01 == 1), formula = changed_app_after ~ abrio_cartilla)
summary(within_1)
within_2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_02 == 1), formula = changed_app_after ~ abrio_cartilla)
summary(within_2)
within_3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1), formula = changed_app_after ~ abrio_cartilla)
summary(within_3)
within_4 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1), formula = changed_app_after ~ abrio_cartilla)
summary(within_4)
within_5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1), formula = changed_app_after ~ abrio_cartilla)
summary(within_5)


##Outcome: Increased Length ROL Ex-Post
appFinal_before_after %<>% mutate(app_length_increase = ifelse(app_length_change>0,1,0))

within_length1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_01 == 1), formula = app_length_increase ~ abrio_cartilla)
summary(within_length1)
within_length2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_02 == 1), formula = app_length_increase ~ abrio_cartilla)
summary(within_length2)
within_length3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1), formula = app_length_increase ~ abrio_cartilla)
summary(within_length3)
within_length4 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1), formula = app_length_increase ~ abrio_cartilla)
summary(within_length4)
within_length5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1), formula = app_length_increase ~ abrio_cartilla)
summary(within_length5)

##Outcome: Nber Programs Added Ex-Post
within_nber_added1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_01 == 1), formula = nber_progr_added ~ abrio_cartilla)
summary(within_nber_added1)
within_nber_added2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_02 == 1), formula = nber_progr_added ~ abrio_cartilla)
summary(within_nber_added2)
within_nber_added3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1), formula = nber_progr_added ~ abrio_cartilla)
summary(within_nber_added3)
within_nber_added4 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1), formula = nber_progr_added ~ abrio_cartilla)
summary(within_nber_added4)
within_nber_added5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1), formula = nber_progr_added ~ abrio_cartilla)
summary(within_nber_added5)


##Outcome: Change Mean Cutoff Initial vs Final ROL
within_cutoff1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_01 == 1), formula = change_mean_cutoff ~ abrio_cartilla)
summary(within_cutoff1)
within_cutoff2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_02 == 1), formula = change_mean_cutoff ~ abrio_cartilla)
summary(within_cutoff2)
within_cutoff3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1), formula = change_mean_cutoff ~ abrio_cartilla)
summary(within_cutoff3)
within_cutoff4 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1), formula = change_mean_cutoff ~ abrio_cartilla)
summary(within_cutoff4)
within_cutoff5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1), formula = change_mean_cutoff ~ abrio_cartilla)
summary(within_cutoff5)

##Outcome: Change Mean Wage Initial vs Final ROL
within_wage1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_01 == 1), formula = change_mean_wage ~ abrio_cartilla)
summary(within_wage1)
within_wage2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_02 == 1), formula = change_mean_wage ~ abrio_cartilla)
summary(within_wage2)
within_wage3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1), formula = change_mean_wage ~ abrio_cartilla)
summary(within_wage3)
within_wage4 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1), formula = change_mean_wage ~ abrio_cartilla)
summary(within_wage4)
within_wage5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1), formula = change_mean_wage ~ abrio_cartilla)
summary(within_wage5)

##Outcome: Students with Ex Ante Mistake Only: Does the student make an application mistake ex-post?
within_error1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_01 == 1 & error_before==1), formula = error_after ~ abrio_cartilla)
summary(within_error1)
within_error2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_02 == 1 & error_before==1), formula = error_after ~ abrio_cartilla)
summary(within_error2)
within_error3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1 & error_before==1), formula = error_after ~ abrio_cartilla)
summary(within_error3)
within_error4 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1 & error_before==1), formula = error_after ~ abrio_cartilla)
summary(within_error4)
within_error5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1 & error_before==1), formula = error_after ~ abrio_cartilla)
summary(within_error5)

##Outcome: Students who would not have been assigned given their ex-ante ROL Only: Is the student assigned ex-post?
within_assign1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_01 == 1 & asignado_before==0), formula = asignado ~ abrio_cartilla)
summary(within_assign1)
within_assign2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_02 == 1  & asignado_before==0), formula = asignado ~ abrio_cartilla)
summary(within_assign2)
within_assign3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1  & asignado_before==0), formula = asignado ~ abrio_cartilla)
summary(within_assign3)
within_assign4 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1  & asignado_before==0), formula = asignado ~ abrio_cartilla)
summary(within_assign4)
within_assign5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1  & asignado_before==0), formula = asignado ~ abrio_cartilla)
summary(within_assign5) 

##Outcome: Students who would have been assigned given their ex-ante ROL Only: Is the student assigned to a different program ex-post?
within_assignprog1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 &  Treatment_01 == 1  & asignado_before==1), formula = program_asig_change ~ abrio_cartilla)
summary(within_assignprog1)
within_assignprog2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 &  Treatment_02 == 1  & asignado_before==1), formula = program_asig_change ~ abrio_cartilla)
summary(within_assignprog2)
within_assignprog3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1  & asignado_before==1), formula = program_asig_change ~ abrio_cartilla)
summary(within_assignprog3)
within_assignprog4 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1  & asignado_before==1), formula = program_asig_change ~ abrio_cartilla)
summary(within_assignprog4)
within_assignprog5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1  & asignado_before==1), formula = program_asig_change ~ abrio_cartilla)
summary(within_assignprog5) 


##Outcome: Increased risk
within_increasedrisk1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 &  Treatment_01 == 1  & risk_initial<=0.1), formula = increased_risk ~ abrio_cartilla)
summary(within_increasedrisk1)
within_increasedrisk2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 &  Treatment_02 == 1  & risk_initial<=0.1), formula = increased_risk ~ abrio_cartilla)
summary(within_increasedrisk2)
within_increasedrisk3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1  & risk_initial<=0.1), formula = increased_risk ~ abrio_cartilla)
summary(within_increasedrisk3)
within_increasedrisk4 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1  & risk_initial<=0.1), formula = increased_risk ~ abrio_cartilla)
summary(within_increasedrisk4)
within_increasedrisk5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1  & risk_initial<=0.1), formula = increased_risk ~ abrio_cartilla)
summary(within_increasedrisk5) 

##Outcome: Reduced risk
within_reducedrisk1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 &  Treatment_01 == 1  & risk_initial>0.7), formula = reduced_risk ~ abrio_cartilla)
summary(within_reducedrisk1)
within_reducedrisk2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 &  Treatment_02 == 1  & risk_initial>0.7), formula = reduced_risk ~ abrio_cartilla)
summary(within_reducedrisk2)
within_reducedrisk3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1  & risk_initial>0.7), formula = reduced_risk ~ abrio_cartilla)
summary(within_reducedrisk3)
within_reducedrisk4 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1  & risk_initial>0.7), formula = reduced_risk ~ abrio_cartilla)
summary(within_reducedrisk4)
within_reducedrisk5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1  & risk_initial>0.7), formula = reduced_risk ~ abrio_cartilla)
summary(within_reducedrisk5) 

##Outcome: Change in risk
within_changerisk1 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 &  Treatment_01 == 1  & risk_initial>0.7), formula = change_risk ~ abrio_cartilla)
summary(within_changerisk1)
within_changerisk2 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 &  Treatment_02 == 1  & risk_initial>0.7), formula = change_risk ~ abrio_cartilla)
summary(within_changerisk2)
within_changerisk3 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_0 == 1  & risk_initial>0.7), formula = change_risk ~ abrio_cartilla)
summary(within_changerisk3)
within_changerisk4= lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_1 == 1  & risk_initial>0.7), formula = change_risk ~ abrio_cartilla)
summary(within_changerisk4)
within_changerisk5 = lm(data = appFinal_before_after %>% subset(propensity_score_sample==1 & Treatment_2 == 1  & risk_initial>0.7), formula = change_risk ~ abrio_cartilla)
summary(within_changerisk5) 


stargazer(within_3,  within_4, within_5, within_length3, within_length4, within_length5,  title="Within-Treatment",
          dep.var.labels=c("Changed ROL", "Increased Length"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)

stargazer(within_cutoff3,  within_cutoff4, within_cutoff5, within_wage3, within_wage4, within_wage5,  title="Within-Treatment",
          dep.var.labels=c("Change Cutoff",  "Change Wage"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)

stargazer(within_error3,  within_error4, within_error5, within_assign3, within_assign4, within_assign5, within_assignprog3, within_assignprog4, within_assignprog5,  title="Within-Treatment",
          dep.var.labels=c("Error After", "Assigned After",   "Changed Progr. Assigned"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)

stargazer(within_increasedrisk3,  within_increasedrisk4, within_increasedrisk5, within_reducedrisk3, within_reducedrisk4, within_reducedrisk5, within_changerisk3, within_changerisk4, within_changerisk5,  title="Within-Treatment - Risk",
          dep.var.labels=c("Increased Risk", "Reduced After", "Changed Risk"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)





############################################################MAJORS RECOMMENDATION IMPACT############################################################

#Load new data:
reco <- read.csv('~/Dropbox/Mistakes Structural/Code/R/recommendation/outputs/recommendations.csv', sep=',', header=TRUE)
equiv <- read.csv('~/Dropbox/Mistakes Structural/Data/other/Tabla1IgnacioTomas.csv', sep=',', header=TRUE)

#Keep only combination DEMRE code - major code:
equiv %<>% select(Demre_ID, major_id)

#Put the recommendation database in wide format:
reco_wide <- reshape(reco, idvar= "MRUN", v.names= "Major_ID", timevar="Rec", direction = "wide")
remove(reco)

#Merge the recommendation database to the main dataset:
appFinal_before_after = merge(appFinal_before_after,
                              reco_wide,
                              by = "MRUN",
                              all.x = TRUE)


#Put the initial ROL long:
appFinal_before_after_long_initial = appFinal_before_after %>% select(MRUN, propensity_score_sample, abrio_cartilla, Treatment_0, Treatment_1, Treatment_2, Treatment_3, changed_app_after, Major_ID.1, Major_ID.2, Major_ID.3, Major_ID.4, CAR_CODIGO_1PREFER.x, CAR_CODIGO_2PREFER.x, CAR_CODIGO_3PREFER.x, CAR_CODIGO_4PREFER.x, CAR_CODIGO_5PREFER.x, CAR_CODIGO_6PREFER.x, CAR_CODIGO_7PREFER.x, CAR_CODIGO_8PREFER.x, CAR_CODIGO_9PREFER.x, CAR_CODIGO_10PREFER.x)
appFinal_before_after_long_initial %<>% pivot_longer(cols = starts_with("CAR_CODIGO_"), names_to = c("rank"), names_prefix= "CAR_CODIGO_", values_to= "initial_rol")
appFinal_before_after_long_initial$rank <- str_remove(appFinal_before_after_long_initial$rank, "PREFER.x")
#Merge equivalence major/application code
appFinal_before_after_long_initial = merge(appFinal_before_after_long_initial,
                                           equiv,
                                           by.x = "initial_rol", by.y = "Demre_ID",
                                           all.x = TRUE)

#Put the final ROL long:
appFinal_before_after_long_final = appFinal_before_after %>% select(MRUN, CAR_CODIGO_1PREFER.y, CAR_CODIGO_2PREFER.y, CAR_CODIGO_3PREFER.y, CAR_CODIGO_4PREFER.y, CAR_CODIGO_5PREFER.y, CAR_CODIGO_6PREFER.y, CAR_CODIGO_7PREFER.y, CAR_CODIGO_8PREFER.y, CAR_CODIGO_9PREFER.y, CAR_CODIGO_10PREFER.y)
appFinal_before_after_long_final %<>% pivot_longer(cols = starts_with("CAR_CODIGO_"), names_to = c("rank"), names_prefix= "CAR_CODIGO_", values_to= "final_rol")
appFinal_before_after_long_final$rank <- str_remove(appFinal_before_after_long_final$rank, "PREFER.y")
#Merge equivalence major/application code
appFinal_before_after_long_final = merge(appFinal_before_after_long_final,
                                         equiv,
                                         by.x = "final_rol", by.y = "Demre_ID",
                                         all.x = TRUE)


#Merge both:
appFinal_before_after_long = merge(appFinal_before_after_long_initial, appFinal_before_after_long_final, by=c("MRUN", "rank"))

#Rename the major categories of the programs the student applied to:
appFinal_before_after_long %<>% dplyr::rename(initial_rol_major=major_id.x)
appFinal_before_after_long %<>% dplyr::rename(final_rol_major=major_id.y)

#Replace final ROL by initial ROL if the student did not change his application after the intervention:
appFinal_before_after_long %<>% mutate(final_rol = ifelse(changed_app_after==0, initial_rol, final_rol))
appFinal_before_after_long %<>% mutate(final_rol_major = ifelse(changed_app_after==0, initial_rol_major, final_rol_major))

#Re-order variable names:
col_order <- c("MRUN", "Treatment_0", "Treatment_1", "Treatment_2", "Treatment_3", "propensity_score_sample", "abrio_cartilla", "changed_app_after", "Major_ID.1", "Major_ID.2", "Major_ID.3", "Major_ID.4", "rank", "initial_rol", "initial_rol_major", "final_rol", "final_rol_major")
appFinal_before_after_long <- appFinal_before_after_long[, col_order]

#Sort by student id, rank:
appFinal_before_after_long %<>% arrange(MRUN, as.numeric(rank))


#Create variables of interest:

      ##Dummy=1 if an application corresponds to any of the four recommended majors:
appFinal_before_after_long %<>% mutate(app_reco_initial = ifelse((initial_rol_major==Major_ID.1) | (initial_rol_major==Major_ID.2) | (initial_rol_major==Major_ID.3) | (initial_rol_major==Major_ID.4), 1, 0))
appFinal_before_after_long %<>% mutate(app_reco_final = ifelse((final_rol_major==Major_ID.1) | (final_rol_major==Major_ID.2) | (final_rol_major==Major_ID.3) | (final_rol_major==Major_ID.4), 1, 0))

      ##Dummy=1 if an application corresponds to any of the 3rd and 4th recommended majors:
appFinal_before_after_long %<>% mutate(app_reco_initial_34 = ifelse((initial_rol_major==Major_ID.3) | (initial_rol_major==Major_ID.4), 1, 0))
appFinal_before_after_long %<>% mutate(app_reco_final_34 = ifelse((final_rol_major==Major_ID.3) | (final_rol_major==Major_ID.4), 1, 0))

      ##Change in the nber of programs from recommended majors (any) btw the initial and final ROL:
appFinal_before_after_long = appFinal_before_after_long %>% group_by(MRUN) %>% mutate(app_reco_initial_nber= sum(app_reco_initial, na.rm = TRUE))
appFinal_before_after_long %<>% mutate(app_reco_initial_nber= ifelse(is.na(Major_ID.1), NA, app_reco_initial_nber))
appFinal_before_after_long = appFinal_before_after_long %>% group_by(MRUN) %>% mutate(app_reco_final_nber= sum(app_reco_final, na.rm = TRUE))
appFinal_before_after_long %<>% mutate(app_reco_final_nber= ifelse(is.na(Major_ID.1), NA, app_reco_final_nber))
appFinal_before_after_long %<>% mutate(change_reco_nber = app_reco_final_nber-app_reco_initial_nber)
appFinal_before_after_long %<>% ungroup()

      ##Change in the nber of programs from recommended majors (only from 3rd and 4th recommended majors) btw the initial and final ROL:
appFinal_before_after_long = appFinal_before_after_long %>% group_by(MRUN) %>% mutate(app_reco34_initial_nber= sum(app_reco_initial_34, na.rm = TRUE))
appFinal_before_after_long %<>% mutate(app_reco34_initial_nber= ifelse(is.na(Major_ID.1), NA, app_reco34_initial_nber))
appFinal_before_after_long = appFinal_before_after_long %>% group_by(MRUN) %>% mutate(app_reco34_final_nber= sum(app_reco_final_34, na.rm = TRUE))
appFinal_before_after_long %<>% mutate(app_reco34_final_nber= ifelse(is.na(Major_ID.1), NA, app_reco34_final_nber))
appFinal_before_after_long %<>% mutate(change_reco34_nber = app_reco34_final_nber-app_reco34_initial_nber)
appFinal_before_after_long %<>% ungroup()

    ##Dummy=1 if the student introduced more programs from recommended majors (any or only 3rd and 4th recommended majors) btw the initial and final ROL:
appFinal_before_after_long %<>% mutate(more_reco = ifelse(change_reco_nber>0, 1, 0))
appFinal_before_after_long %<>% mutate(more_reco_34 = ifelse(change_reco34_nber>0, 1, 0))


#Remove duplicates in terms of student id (in order to have again a dataset at the student level to run regressions):
appFinal_before_after_student = appFinal_before_after_long %>% distinct(MRUN, .keep_all = TRUE)

    ##Looking at some descriptives:
table(appFinal_before_after_student %>% select(change_reco_nber), useNA = "always")
table(appFinal_before_after_student %>% select(more_reco), useNA = "always")
table(appFinal_before_after_student %>% select(change_reco34_nber), useNA = "always")
table(appFinal_before_after_student %>% select(more_reco_34), useNA = "always")


mean((appFinal_before_after_student %>% subset(Treatment_0 == 1))$more_reco)
mean((appFinal_before_after_student %>% subset(Treatment_1 == 1))$more_reco)
mean((appFinal_before_after_student %>% subset(Treatment_2 == 1))$more_reco)
mean((appFinal_before_after_student %>% subset(Treatment_0 == 1 & abrio_cartilla==1))$more_reco)
mean((appFinal_before_after_student %>% subset(Treatment_1 == 1 & abrio_cartilla==1))$more_reco)
mean((appFinal_before_after_student %>% subset(Treatment_2 == 1 & abrio_cartilla==1))$more_reco)


    ##Regressions: ITT analysis - T0 and T2 vs T1 (did not received recommendations)
ITT_reco1 = lm(data = appFinal_before_after_student %>% subset(Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1), formula = more_reco ~ Treatment_0 + Treatment_2)
summary(ITT_reco1)
ITT_reco2 = lm(data = appFinal_before_after_student %>% subset(Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1), formula = change_reco_nber ~ Treatment_0 + Treatment_2)
summary(ITT_reco2)
ITT_reco3 = lm(data = appFinal_before_after_student %>% subset(Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1), formula = more_reco_34 ~ Treatment_0 + Treatment_2)
summary(ITT_reco3)
ITT_reco4 = lm(data = appFinal_before_after_student %>% subset(Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1), formula = change_reco34_nber ~ Treatment_0 + Treatment_2)
summary(ITT_reco4)
ITT_reco5 = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & app_reco34_initial_nber==0), formula = more_reco_34 ~ Treatment_0 + Treatment_2)
summary(ITT_reco5)
ITT_reco6 = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & app_reco34_initial_nber==0), formula = change_reco34_nber ~ Treatment_0 + Treatment_2)
summary(ITT_reco6)

stargazer(ITT_reco1, ITT_reco2, ITT_reco3, ITT_reco4, title="Intention-to-Treat - Recommendation of Other Majors",
          dep.var.labels=c("Increased Nber Recommended Majors", "Change in # Recommended Majors", "Increased Nber Recommended Majors", "Change in # Recommended Majors"),
          covariate.labels=c("T0", "T2"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)


ITT_reco1_changed = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & changed_app_after==1), formula = more_reco ~ Treatment_0 + Treatment_2)
summary(ITT_reco1_changed)
ITT_reco2_changed = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & changed_app_after==1), formula = change_reco_nber ~ Treatment_0 + Treatment_2)
summary(ITT_reco2_changed)
ITT_reco3_changed = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & changed_app_after==1), formula = more_reco_34 ~ Treatment_0 + Treatment_2)
summary(ITT_reco3_changed)
ITT_reco4_changed = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & changed_app_after==1), formula = change_reco34_nber ~ Treatment_0 + Treatment_2)
summary(ITT_reco4_changed)
ITT_reco5_changed = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & app_reco34_initial_nber==0 & changed_app_after==1), formula = more_reco_34 ~ Treatment_0 + Treatment_2)
summary(ITT_reco5_changed)
ITT_reco6_changed = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & app_reco34_initial_nber==0 & changed_app_after==1), formula = change_reco34_nber ~ Treatment_0 + Treatment_2)
summary(ITT_reco6_changed)

stargazer(ITT_reco1_changed, ITT_reco2_changed, ITT_reco3_changed, ITT_reco4_changed,  title="Intention-to-Treat - Recommendation of Other Majors - Subsample of Students who Changed their ROL",
          dep.var.labels=c("Increased Nber Recommended Majors", "Change in # Recommended Majors", "Increased Nber Recommended Majors", "Change in # Recommended Majors", "Increased Nber Recommended Majors", "Change in # Recommended Majors"),
          covariate.labels=c("T0", "T2"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)


ITT_reco1_abrio = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & abrio_cartilla==1), formula = more_reco ~ Treatment_0 + Treatment_2)
summary(ITT_reco1_abrio)
ITT_reco2_abrio = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & abrio_cartilla==1), formula = change_reco_nber ~ Treatment_0 + Treatment_2)
summary(ITT_reco2_abrio)
ITT_reco3_abrio = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & abrio_cartilla==1), formula = more_reco_34 ~ Treatment_0 + Treatment_2)
summary(ITT_reco3_abrio)
ITT_reco4_abrio = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & abrio_cartilla==1), formula = change_reco34_nber ~ Treatment_0 + Treatment_2)
summary(ITT_reco4_abrio)
ITT_reco5_abrio = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & app_reco34_initial_nber==0 & abrio_cartilla==1), formula = more_reco_34 ~ Treatment_0 + Treatment_2)
summary(ITT_reco5_abrio)
ITT_reco6_abrio = lm(data = appFinal_before_after_student %>% subset((Treatment_0 == 1 | Treatment_1 == 1 | Treatment_2 == 1) & app_reco34_initial_nber==0 & abrio_cartilla==1), formula = change_reco34_nber ~ Treatment_0 + Treatment_2)
summary(ITT_reco6_abrio)



##Regressions: Within-Treatment Effects
within_reco1 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1), formula = more_reco ~ abrio_cartilla)
summary(within_reco1)
within_reco2 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1), formula = more_reco ~ abrio_cartilla)
summary(within_reco2)
within_reco3 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1), formula = more_reco ~ abrio_cartilla)
summary(within_reco3)
within_reco4 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1), formula = change_reco_nber ~ abrio_cartilla)
summary(within_reco4)
within_reco5 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1), formula = change_reco_nber ~ abrio_cartilla)
summary(within_reco5)
within_reco6 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1), formula = change_reco_nber ~ abrio_cartilla)
summary(within_reco6)


stargazer(within_reco1,  within_reco2, within_reco3, within_reco4, within_reco5, within_reco6, title="Within-Treatment - Recommendation of Other Majors",
          dep.var.labels=c("More Recommendation", "Change Number Recommendations"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)



within_reco1_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1 & changed_app_after==1), formula = more_reco ~ abrio_cartilla)
summary(within_reco1_changed)
within_reco2_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1 & changed_app_after==1), formula = more_reco ~ abrio_cartilla)
summary(within_reco2_changed)
within_reco3_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1 & changed_app_after==1), formula = more_reco ~ abrio_cartilla)
summary(within_reco3_changed)
within_reco4_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1 & changed_app_after==1), formula = change_reco_nber ~ abrio_cartilla)
summary(within_reco4_changed)
within_reco5_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1 & changed_app_after==1), formula = change_reco_nber ~ abrio_cartilla)
summary(within_reco5_changed)
within_reco6_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1 & changed_app_after==1), formula = change_reco_nber ~ abrio_cartilla)
summary(within_reco6_changed)


stargazer(within_reco1_changed,  within_reco2_changed, within_reco3_changed, within_reco4_changed, within_reco5_changed, within_reco6_changed, title="Within-Treatment - Recommendation of Other Majors - Subsample of Students who Changed their ROL",
          dep.var.labels=c("More Recommendation", "Change Number Recommendations"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)




within_reco34_1 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_1)
within_reco34_2 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_2)
within_reco34_3 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_3)
within_reco34_4 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_4)
within_reco34_5 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_5)
within_reco34_6 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_6)


stargazer(within_reco34_1,  within_reco34_2, within_reco34_3, within_reco34_4, within_reco34_5, within_reco34_6, title="Within-Treatment - Recommendation of Other Majors",
          dep.var.labels=c("More Recommendation", "Change Number Recommendations"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)



within_reco34_1_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1  & changed_app_after==1), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_1_changed)
within_reco34_2_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1  & changed_app_after==1), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_2_changed)
within_reco34_3_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1  & changed_app_after==1), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_3_changed)
within_reco34_4_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1  & changed_app_after==1), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_4_changed)
within_reco34_5_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1  & changed_app_after==1), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_5_changed)
within_reco34_6_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1  & changed_app_after==1), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_6_changed)


stargazer(within_reco34_1_changed,  within_reco34_2_changed, within_reco34_3_changed, within_reco34_4_changed, within_reco34_5_changed, within_reco34_6_changed, title="Within-Treatment - Recommendation of Other Majors",
          dep.var.labels=c("More Recommendation", "Change Number Recommendations"),
          covariate.labels=c("Opened E-mail"),
          omit.stat=c("LL","ser","f"), single.row=FALSE)



within_reco34_7 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1 & app_reco34_initial_nber==0), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_7)
within_reco34_8 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1 & app_reco34_initial_nber==0), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_8)
within_reco34_9 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1 & app_reco34_initial_nber==0), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_9)
within_reco34_10 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1 & app_reco34_initial_nber==0), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_10)
within_reco34_11 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1 & app_reco34_initial_nber==0), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_11)
within_reco34_12 = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1 & app_reco34_initial_nber==0), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_12)




within_reco34_7_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1 & app_reco34_initial_nber==0  & changed_app_after==1), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_7_changed)
within_reco34_8_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1 & app_reco34_initial_nber==0  & changed_app_after==1), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_8_changed)
within_reco34_9_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1 & app_reco34_initial_nber==0 & changed_app_after==1), formula = more_reco_34 ~ abrio_cartilla)
summary(within_reco34_9_changed)
within_reco34_10_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_0 == 1 & app_reco34_initial_nber==0 & changed_app_after==1), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_10_changed)
within_reco34_11_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_1 == 1 & app_reco34_initial_nber==0 & changed_app_after==1), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_11_changed)
within_reco34_12_changed = lm(data = appFinal_before_after_student %>% subset(propensity_score_sample==1 & Treatment_2 == 1 & app_reco34_initial_nber==0 & changed_app_after==1), formula = change_reco34_nber ~ abrio_cartilla)
summary(within_reco34_12_changed)