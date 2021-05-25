# Extra program in survey

# Remove objects

rm(list = ls())

# Libraries
library(dplyr)
library(magrittr)
library(parallel)

#Set seed
RNGkind("L'Ecuyer-CMRG")
set.seed(1)

# Load data

# Data processing
## Load applications
app <- read.csv('~/Dropbox/Mistakes_in_college_admissions/data/raw/implementation/survey-2021/postulaciones_202102240455puntajes.csv', sep=';', header=TRUE)

colnames(app)

head(app)

# Rename vars
names(app)[names(app) == 'CAR_CODIGO_1PREFER'] = 'COD_CARRERA_PREF_01'
names(app)[names(app) == 'CAR_CODIGO_2PREFER'] = 'COD_CARRERA_PREF_02'
names(app)[names(app) == 'CAR_CODIGO_3PREFER'] = 'COD_CARRERA_PREF_03'
names(app)[names(app) == 'CAR_CODIGO_4PREFER'] = 'COD_CARRERA_PREF_04'
names(app)[names(app) == 'CAR_CODIGO_5PREFER'] = 'COD_CARRERA_PREF_05'
names(app)[names(app) == 'CAR_CODIGO_6PREFER'] = 'COD_CARRERA_PREF_06'
names(app)[names(app) == 'CAR_CODIGO_7PREFER'] = 'COD_CARRERA_PREF_07'
names(app)[names(app) == 'CAR_CODIGO_8PREFER'] = 'COD_CARRERA_PREF_08'
names(app)[names(app) == 'CAR_CODIGO_9PREFER'] = 'COD_CARRERA_PREF_09'
names(app)[names(app) == 'CAR_CODIGO_10PREFER'] = 'COD_CARRERA_PREF_10'
#
names(app)[names(app) == 'PUNTAJE_CARR1'] = 'PTJE_PREF_01'
names(app)[names(app) == 'PUNTAJE_CARR2'] = 'PTJE_PREF_02'
names(app)[names(app) == 'PUNTAJE_CARR3'] = 'PTJE_PREF_03'
names(app)[names(app) == 'PUNTAJE_CARR4'] = 'PTJE_PREF_04'
names(app)[names(app) == 'PUNTAJE_CARR5'] = 'PTJE_PREF_05'
names(app)[names(app) == 'PUNTAJE_CARR6'] = 'PTJE_PREF_06'
names(app)[names(app) == 'PUNTAJE_CARR7'] = 'PTJE_PREF_07'
names(app)[names(app) == 'PUNTAJE_CARR8'] = 'PTJE_PREF_08'
names(app)[names(app) == 'PUNTAJE_CARR9'] = 'PTJE_PREF_09'
names(app)[names(app) == 'PUNTAJE_CARR10'] = 'PTJE_PREF_10'

#Replace -1 and NA with 0
#app[app == -1] = 0
#app[is.na(app)] = 0

## Compute frequently chosen together programs (prob de transicion)
levs <- union(app$COD_CARRERA_PREF_01, app$COD_CARRERA_PREF_02)
transition_table = as.matrix(table(factor(app$COD_CARRERA_PREF_01,levs), factor(app$COD_CARRERA_PREF_02,levs)))
transition_table = transition_table/rowSums(transition_table)

## Draw second most common program
app$second_mostcommon_program = NA

# Loop over students
for(i in 1:nrow(app)){
  print(i)
  tt_aux = transition_table
  for(j in c("02", "03", "04", "05", "06", "07", "08", "09", "10")){
    aux_pref = app[i, paste("COD_CARRERA_PREF_",j, sep = "")]
    if(is.na(aux_pref)){
      break
    }else{
      tt_aux[, as.character(aux_pref)] = 0.0
    }
  }
  if((sum(is.na(tt_aux[as.character(app[i,"COD_CARRERA_PREF_01"]),])) == 0) &
    (sum(tt_aux[as.character(app[i,"COD_CARRERA_PREF_01"]),]) > 0)){
    second_mostcommon_program = as.numeric(sample(colnames(tt_aux), size = 1,
                                                replace = TRUE,
                                                prob = tt_aux[as.character(app[i,"COD_CARRERA_PREF_01"]),]))
    app[i,"second_mostcommon_program"] = second_mostcommon_program
  }
}

# Some have NA
sum(!is.na(app$second_mostcommon_program))
sum(is.na(app$second_mostcommon_program))
#Replace by some program
app %<>% mutate(second_mostcommon_program =  ifelse(is.na(second_mostcommon_program), 
                                                    as.numeric(sample(colnames(transition_table),
                                                                      size = 1,
                                                                      replace = TRUE)),
                                                    second_mostcommon_program))

# Merge App with cutoff scores
first_last_full_2019_2020 = read.csv('~/Dropbox/Mistakes_in_college_admissions/data/raw/implementation/survey-2021/first_last_full_2019_2020.csv', header=TRUE, sep=';')

app = merge(app, first_last_full_2019_2020 %>% select(codigo_carrera, min_2020),
            by.x = "second_mostcommon_program",
            by.y = "codigo_carrera",
            all.x = TRUE)
app %<>% mutate(min_2020 = min_2020/100)
# Replace zeros by 500
app %<>% mutate(min_2020_changed = ifelse(min_2020 == 0, 500, min_2020))
#Add some noise
app$min_2020_noise = round((app$min_2020_changed + runif(length(app$min_2020_changed), -10, 10)))


# Save to csv file
write.csv(app,
          file = '~/Dropbox/Mistakes_in_college_admissions/data/raw/implementation/survey-2021/postulaciones_edited.csv',
          row.names = FALSE)
