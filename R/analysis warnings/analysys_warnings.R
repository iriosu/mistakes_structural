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

path_draft_tables = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/documents/draft/tables/"
path_draft_plots = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/documents/draft/plots/"
path_intermediate_data = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/intermediate_data/"

# Load data ---------------------------------------------------------------

# Load applications
#TODO: short description of these files
#TODO: ask Chris to place his Matlab scripts in the repo
# We think this is the list of all students who can participate in the Admission Process
appList <- read.csv('~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/AppList.csv', sep=',', header=TRUE)
#TODO: what is exactly Treatment 3?
table(appList$Treatment)
sum(duplicated(appList$mrun))
# TODO: add description of file
appRCT <- read.csv('~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/appRCT.csv', sep=',', header=TRUE)
# NOTE: we do not have T3 anymore in this file 
table(appRCT$Treatment)
sum(duplicated(appRCT$mrun))

#TODO: Check with Chris if this file is OK
# We need to see if this is most updated version of the file from Analytics and that the date he used to filter is correct
Cartilla_OpenList <- read.csv('~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/Cartilla_OpenList_1415.csv', sep=',', header=FALSE)
appFinal <- read.csv('~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/210217 Reporte Postulaciones MRUN v2 CB.csv', sep=',', header=TRUE)
UBO_ErrorList <- read.csv('~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/UBO_ErrorList.csv', sep=',', header=FALSE)
asignacion_obtenida_reg <- read.csv('~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/asignacion_obtenida_reg.csv', sep=';', header=TRUE)
asignacion_obtenida_reg_before <- read.csv('~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/asignacion_obtenida_reg_before.csv', sep=';', header=TRUE)
asignacion_obtenida_bea <- read.csv('~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/asignacion_obtenida_bea.csv', sep=';', header=TRUE)
asignacion_obtenida_bea_before <- read.csv('~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/asignacion_obtenida_bea_before.csv', sep=';', header=TRUE)
bcu <- read.csv('~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/ACCESO_BCU-8184.ETAPA2.csv', sep=';', header=TRUE)

#Save as Rdata
save(appList, file = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/AppList.Rdata")
save(appFinal, file = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/appFinal.Rdata")
save(appRCT,file = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/appRCT.Rdata")
save(Cartilla_OpenList,file = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/Cartilla_OpenList_1415.Rdata")
save(asignacion_obtenida_reg,file = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/asignacion_obtenida_reg.Rdata")
save(asignacion_obtenida_reg_before,file = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/asignacion_obtenida_reg_before.Rdata")
save(asignacion_obtenida_bea_before,file = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/asignacion_obtenida_bea_before.Rdata")
save(bcu,file = "~/Dropbox/Mistakes_in_college_admissions/mistakes_and_warnings/data/2021/ACCESO_BCU-8184.ETAPA2.Rdata")

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
#TODO: chenck if we are adding applications before sending the email
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
df_cutoffs = readRDS("~/Dropbox/Mistakes_in_college_admissions/data/interim/panel_cutoffs_2004_2020_20210420.rds")
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

hist(appFinal_before_after %>% filter(invalid_programs_share_change != 0) %>% 
       select(invalid_programs_share_change))
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
mruns_propensity_score = readRDS(file = paste(path_intermediate_data, "matched_sampled_using_propensity_score_view_email.rds", sep=""))
mruns_propensity_score %<>% select(mrun)
mruns_propensity_score %<>% mutate(propensity_score_sample = 1)
# Merge with all the sample for the anlysis
appFinal_before_after = merge(appFinal_before_after, mruns_propensity_score, by.x = "MRUN", by.y = "mrun", all.x = TRUE)
appFinal_before_after %<>% mutate(propensity_score_sample = ifelse(is.na(propensity_score_sample), 0, propensity_score_sample))
table(appFinal_before_after$propensity_score_sample)

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

# Treatment effects -------------------------------------------------------

#TODO: THIS PART HAS TO BE UPDATED WITH NACHO'S PROPENSITY MATCHING



## Treatment groups

# Grupo 0 recibio todo
# Grupo 1 recibio solo warnings
# Grupo 2 recibio solo recomendaciones
# Grupo 3 recibio intervencion de puntajes (Pumi)
# Treatment effects of Warnings: 0 minus 2
# Treatment effects of Recomendations: 0 minus 1


# Change in applications
mean((appFinal_before_after %>% subset(Treatment_0 == 1 & abrio_cartilla == 0))$changed_app_after)
mean((appFinal_before_after %>% subset(Treatment_1 == 1 & abrio_cartilla == 0))$changed_app_after)
mean((appFinal_before_after %>% subset(Treatment_2 == 1 & abrio_cartilla == 0))$changed_app_after)
mean((appFinal_before_after %>% subset(Treatment_3 == 1 & abrio_cartilla == 0))$changed_app_after)

#
mean((appFinal_before_after %>% subset(Treatment_0 == 1 & abrio_cartilla == 1))$changed_app_after)
mean((appFinal_before_after %>% subset(Treatment_1 == 1 & abrio_cartilla == 1))$changed_app_after)
mean((appFinal_before_after %>% subset(Treatment_2 == 1 & abrio_cartilla == 1))$changed_app_after)
mean((appFinal_before_after %>% subset(Treatment_3 == 1 & abrio_cartilla == 1))$changed_app_after)

# Regressions
## changed_app_after = 1 if any change, 0 otherwise
m_rel_2 = lm(data = appFinal_before_after %>% subset(abrio_cartilla == 1), formula = changed_app_after ~ Treatment_0 + Treatment_1)
summary(m_rel_2)

logit_rel_2 = glm(data = appFinal_before_after %>% subset(abrio_cartilla == 1), formula = changed_app_after ~ Treatment_0 + Treatment_1, family = 'binomial')
summary(logit_rel_2)

m_rel_0_2 = lm(data = appFinal_before_after %>% subset(abrio_cartilla == 1 & (Treatment == 0 | Treatment == 2)), formula = changed_app_after ~ Treatment_0)
summary(m_rel_0_2)

logit_rel_0_2 = glm(data = appFinal_before_after %>% subset(abrio_cartilla == 1 & (Treatment == 0 | Treatment == 2)), formula = changed_app_after ~ Treatment_0, family="binomial")
summary(logit_rel_0_2)

# Change in application length
mean((appFinal_before_after %>% subset(abrio_cartilla == 0))$app_length_change)
mean((appFinal_before_after %>% subset(Treatment_0 == 1 & abrio_cartilla == 1))$app_length_change)
mean((appFinal_before_after %>% subset(Treatment_1 == 1 & abrio_cartilla == 1))$app_length_change)
mean((appFinal_before_after %>% subset(Treatment_2 == 1 & abrio_cartilla == 1))$app_length_change)

# Regressions
m_0 = lm(data = appFinal_before_after %>% subset(abrio_cartilla == 1), formula = app_length_change ~ Treatment_0)
m_1 = lm(data = appFinal_before_after %>% subset(abrio_cartilla == 1), formula = app_length_change ~ Treatment_1)
m_2 = lm(data = appFinal_before_after %>% subset(abrio_cartilla == 1), formula = app_length_change ~ Treatment_2)
summary(m_0)
summary(m_1)
summary(m_2)
T_0_2_change_app_length = as.numeric(m_0$coefficients[2] - m_2$coefficients[2])

m_0 = lm(data = appFinal_before_after %>% subset(abrio_cartilla == 1), formula = app_length_change ~ Treatment_0 + Treatment_1)
summary(m_0)



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
