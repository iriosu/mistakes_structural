# Recommendation algorithm

# Cooking recipe
#1. Load data from applications and majors
#2. Look at application (all pref) and find two most common majors (= carrera generica)
#3. Compute frequently chosen together majors (prob de transicion)
#4. Define variable of reachable majors given students' admission probabilities.
#5. Logic of recommendator:
#   a. Same most common major used by student.
#   b. Highest wage major within the area of the most common major (cine unesco)
#   c. Highest wage major within the area of the most common major that is reachable.
#   d. If student's score is < 600, add one CFT or IP with highest wage that is in same area. If > 600, then most common together.

#Salario, selectividad, postulaciones comunes
#/Users/iriosu/Dropbox/Mistakes/Code/R/recommendation

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
app <- read.csv('~/Dropbox/Mistakes/Code/Python/Cartillas/data/2021/Reporte_diario_postulaciones_final_mrun.csv', sep=',', header=TRUE)

colnames(app)

head(app)

# Load scores and match them by mrun
df_scores <- read.csv('~/Dropbox/Mistakes/Code/Python/Cartillas/data/2021/adm2021_archivo_C_demre_2021feb08_MRUN_Compartir.csv', header=TRUE, sep=';')

#Create max average math-lang
df_scores %<>% mutate(promedio_lm_max = pmax((mate_anterior + leng_anterior)/2, (mate_actual + clec_actual)/2))

# Merge applications to math-lang scores  
app = merge(app, df_scores %>% select(mrun, promedio_lm_max), by = 'mrun', all.x = TRUE)

# Rename vars

names(app)[names(app) == 'car_codigo_1prefer'] = 'COD_CARRERA_PREF_01'
names(app)[names(app) == 'car_codigo_2prefer'] = 'COD_CARRERA_PREF_02'
names(app)[names(app) == 'car_codigo_3prefer'] = 'COD_CARRERA_PREF_03'
names(app)[names(app) == 'car_codigo_4prefer'] = 'COD_CARRERA_PREF_04'
names(app)[names(app) == 'car_codigo_5prefer'] = 'COD_CARRERA_PREF_05'
names(app)[names(app) == 'car_codigo_6prefer'] = 'COD_CARRERA_PREF_06'
names(app)[names(app) == 'car_codigo_7prefer'] = 'COD_CARRERA_PREF_07'
names(app)[names(app) == 'car_codigo_8prefer'] = 'COD_CARRERA_PREF_08'
names(app)[names(app) == 'car_codigo_9prefer'] = 'COD_CARRERA_PREF_09'
names(app)[names(app) == 'car_codigo_10prefer'] = 'COD_CARRERA_PREF_10'
#
names(app)[names(app) == 'puntaje_carr1'] = 'PTJE_PREF_01'
names(app)[names(app) == 'puntaje_carr2'] = 'PTJE_PREF_02'
names(app)[names(app) == 'puntaje_carr3'] = 'PTJE_PREF_03'
names(app)[names(app) == 'puntaje_carr4'] = 'PTJE_PREF_04'
names(app)[names(app) == 'puntaje_carr5'] = 'PTJE_PREF_05'
names(app)[names(app) == 'puntaje_carr6'] = 'PTJE_PREF_06'
names(app)[names(app) == 'puntaje_carr7'] = 'PTJE_PREF_07'
names(app)[names(app) == 'puntaje_carr8'] = 'PTJE_PREF_08'
names(app)[names(app) == 'puntaje_carr9'] = 'PTJE_PREF_09'
names(app)[names(app) == 'puntaje_carr10'] = 'PTJE_PREF_10'
#
names(app)[names(app) == 'mrun'] = 'MRUN'

#Replace -1 and NA with 0

app[app == -1] = 0
app[is.na(app)] = 0

## Load data from majors
# level_id: 3 = u, 2 = ip, 1 = ctp/cft
# major_id = carrera generica
# area_id = cine unesco (10 categorias)
df1 <- read.csv('~/Dropbox/Mistakes/Code/R/data/Tabla1IgnacioTomas.csv', header=TRUE, sep=',')
df2 <- read.csv('~/Dropbox/Mistakes/Code/R/data/Tabla2IgnacioTomas.csv', header=TRUE, sep=',')

#Sort df1
df1 = df1[order(df1$Demre_ID),]

#Analyze
colnames(df1)
colnames(df2)

head(df1)

## Load admission probabilities
probs_python <- read.csv('~/Dropbox/Mistakes/Code/Python/Cartillas/outputs/2021/prob_in_each_program.csv',
                         header=TRUE, sep=';')
# Real data structure
#TODO: duplicated mruns!
aux = probs_python %>% subset(duplicated(mrun))
head(aux)
#NOTE: dropping them for now
probs_python %<>% subset(!duplicated(mrun))
# Merge by mrun
probs_df = merge(app, probs_python, by.x = 'MRUN', by.y = 'mrun', all.x = TRUE)
# Select columns
probs_df %<>% select(MRUN, starts_with('X'))


# Reshaping to long format the application data
app_long = reshape(app, varying=list(c("COD_CARRERA_PREF_01",
                                       "COD_CARRERA_PREF_02",
                                       "COD_CARRERA_PREF_03",
                                       "COD_CARRERA_PREF_04",
                                       "COD_CARRERA_PREF_05",
                                       "COD_CARRERA_PREF_06",
                                       "COD_CARRERA_PREF_07",
                                       "COD_CARRERA_PREF_08",
                                       "COD_CARRERA_PREF_09",
                                       "COD_CARRERA_PREF_10"),
                                     c("PTJE_PREF_01",
                                       "PTJE_PREF_02",
                                       "PTJE_PREF_03",
                                       "PTJE_PREF_04",
                                       "PTJE_PREF_05",
                                       "PTJE_PREF_06",
                                       "PTJE_PREF_07",
                                       "PTJE_PREF_08",
                                       "PTJE_PREF_09",
                                       "PTJE_PREF_10")),
                   direction="long", timevar="PREF", v.names = c("COD_CARRERA","PTJE_PREF"), idvar=c("MRUN"))

# 2. Look at application (all pref) and find two most common majors (= carrera generica)

# Drop observations for preferences where student did not apply
app_long %<>% subset(COD_CARRERA != 0)

# 2.1 Merge carrera generica
app_long = merge(app_long, df1, by.x = "COD_CARRERA", by.y = "Demre_ID", all.x = TRUE)
head(app_long)

# Select programs without major info
missing_majors = app_long %>% subset(is.na(major_id))
unique(missing_majors$COD_CARRERA)

# Subset
app_long_sub = app_long %>% select(MRUN, PREF, PTJE_PREF, promedio_lm_max, major_id)
table(app_long_sub$major_id, useNA = "always")

# Reshape to wide
app_wide_sub = reshape(app_long_sub, v.names = c("PREF", "PTJE_PREF", "major_id"),
                       timevar="PREF", idvar=c("MRUN"), direction="wide", sep = "_")

# Get the most common and second most common major
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

common_majors_list = lapply(1:nrow(app_wide_sub), FUN = function(i, app_wide_sub = app_wide_sub){
  major_vec_aux = c()
  for(j in 1:10){
    major_col = paste('major_id_', j, sep = "")
    if(!is.na(app_wide_sub[i, major_col])){
      major_vec_aux = c(major_vec_aux, app_wide_sub[i, major_col])
    }
  }
  major_most_common = NA
  major_second_most_common = NA
  if(length(major_vec_aux) > 0){
    major_most_common = getmode(major_vec_aux)
    major_second_most_common = getmode(major_vec_aux[major_vec_aux != getmode(major_vec_aux)])
  }
  return(c(app_wide_sub[i, "MRUN"], major_most_common, major_second_most_common, app_wide_sub[i,"promedio_lm_max"]))
}, app_wide_sub = app_wide_sub)
#Transform to dataframe
aux = do.call(rbind, common_majors_list)
colnames(aux) = c('MRUN', 'major_most_common_id', 'major_sec_most_common_id', 'promedio_lm_max')
#Merge variaboles to dataframe
app_wide_sub = merge(app_wide_sub, aux, by = 'MRUN', all.x = TRUE)

head(app_wide_sub)

## 3. Compute frequently chosen together majors (prob de transicion)

levs <- union(app_wide_sub$major_most_common_id, app_wide_sub$major_sec_most_common_id)
transition_table = as.matrix(table(factor(app_wide_sub$major_most_common_id,levs), factor(app_wide_sub$major_sec_most_common_id,levs)))
transition_table = transition_table/rowSums(transition_table)

## 4. Define variable of reachable majors given students' score

# 4.1 In another script we are computing the admission probability for each student with respect to each program, 
# taking into account the 2021 applications
# 4.2 We load this data frame or matrix in Python or here, and then create a matrix with students as rows and columns as major_ids. We put 1 if the student has positive admission probability to at least one pfrogram of that major, and zero otherwise 
# This is the feasibility matrix we will use for the recommendation algorithm

code_cars_probs = as.numeric(sub('.', '', colnames(probs_df)[-1])) 
#NOTE: this should be sorted by Demre_ID
cod_car_col =  df1$Demre_ID
#TODO: check order of cars and mruns
sum(code_cars_probs != cod_car_col)
major_id_col = df1$major_id
probs_cars = data.frame(major_id_col)

#TODO: check that the mruns and code cars match in the different lists

# Create 
probs_list = mclapply(1:length(probs_df$MRUN), mc.cores = 7, 
                      FUN = function(i, aux = probs_cars, probs_df_aux = probs_df){
  print(i)                        
  aux[,"probs"] = as.numeric(probs_df_aux[i,][-1])
  return(aux)
})


#TODO: the list is not indexed by the value of MRUN

# Compute matrix with feasible majors (carreras genericas) 

#Collapse at the student level the feasible majors
majors_feasible_list = mclapply(1:length(probs_list), mc.cores = 7, FUN = function(i){
  majors_feasible = probs_list[[i]] %>%
    group_by(major_id_col) %>%
    summarise(mean_major_prob = mean(probs)) 
  majors_feasible %<>% mutate(feasible = ifelse(mean_major_prob > 0, 1, 0))
  return(majors_feasible)
})

## 5. Logic of recommendator:
#a. Same most common major used by student.
#b. Highest wage major within the area of the most common major (cine unesco)
#c. Highest wage major within the area of the most common major that is reachable.
#d1. If student's score is >=600, show second common major. If NA, use the transition matrix.
#d2. If student's score is < 600, add one CFT or IP with highest wage that is in same area. 

# 5.b Select highest wage major within area (universities)

head(df2)

highest_wages = df2 %>% subset(level_id == 3 & !is.na(INGRESO_PROM_4_ANIO)) %>% group_by(Area_ID) %>%
  summarise(max_wage = max(INGRESO_PROM_4_ANIO))
# Iterate over areas
for(j in highest_wages$Area_ID){
  aux = df2 %>% subset(level_id == 3 & !is.na(INGRESO_PROM_4_ANIO)) %>% 
    select(Area_ID,Major_ID, INGRESO_PROM_4_ANIO) %>% subset(Area_ID == j)
  major_max_wage = aux[aux$INGRESO_PROM_4_ANIO == as.numeric(highest_wages[highest_wages$Area_ID == j, 'max_wage']), 'Major_ID']
  highest_wages[highest_wages$Area_ID == j, 'major_max_wage'] = major_max_wage
}

# Loop over students to get recommendations
# length(probs_list)
recommendation_list = mclapply(1:length(probs_list), mc.cores = 7, FUN = function(i){
  print(i)
  #Get MRUN
  MRUN = common_majors_list[[i]][1]
  # a) Same most common major used by student.
  mostcommon_major = common_majors_list[[i]][2]
  if(is.na(mostcommon_major)){
    print("NA in major of applications")
    return(c(NA, NA, NA, NA, NA))
  }
  #d1. If student's score is >= 600, show second common major. If NA, use the transition matrix.
  second_mostcommon_major = common_majors_list[[i]][3]
  # If it is NA, get one from the transition matrix
  tt_aux = transition_table
  if(is.na(second_mostcommon_major) | (mostcommon_major == second_mostcommon_major)){
    second_mostcommon_major = as.numeric(sample(colnames(tt_aux), size = 1, 
                                                replace = TRUE, 
                                                prob = tt_aux[as.character(mostcommon_major),]))
  }
  tt_aux[, as.character(second_mostcommon_major)] = 0.0
  # Get third most common major and fourth most common major (different)
  if(sum(tt_aux[as.character(mostcommon_major),]) > 0.0){
    third_mostcommon_major = as.numeric(sample(colnames(tt_aux), size = 1, replace = TRUE, prob = tt_aux[as.character(mostcommon_major),]))
  }else{
    #Get third and fourth random majors in this case
    random_majors = sample(setdiff(df2$Major_ID, 
                                   c(mostcommon_major,second_mostcommon_major)), size = 2, replace = FALSE)
    third_mostcommon_major = random_majors[1]   
    fourth_mostcommon_major = random_majors[2]
    aux = c(MRUN,
            mostcommon_major,
            second_mostcommon_major,
            third_mostcommon_major,
            fourth_mostcommon_major)
    return(aux)
  }
  tt_aux[, as.character(third_mostcommon_major)] = 0.0
  if(sum(tt_aux[as.character(mostcommon_major),]) > 0.0){
    fourth_mostcommon_major = as.numeric(sample(colnames(tt_aux), size = 1, replace = TRUE, prob = tt_aux[as.character(mostcommon_major),]))
  }else{
    #Get third and fourth random majors in this case
    random_majors = sample(setdiff(df2$Major_ID, 
                                   c(mostcommon_major,second_mostcommon_major)), size = 2, replace = FALSE)
    third_mostcommon_major = random_majors[1]   
    fourth_mostcommon_major = random_majors[2]
    aux = c(MRUN,
            mostcommon_major,
            second_mostcommon_major,
            third_mostcommon_major,
            fourth_mostcommon_major)
    return(aux)
  }
  # b) Highest wage major within the area of the most common major (cine unesco)   
  area_mostcommon_major = (df2 %>% subset(Major_ID == mostcommon_major))$Area_ID
  aux = df2 %>% subset(level_id == 3 & !is.na(INGRESO_PROM_4_ANIO) & Area_ID == area_mostcommon_major) 
  # Get the top 
  major_highest_wage_area_mostcommon_major = aux[which.max(aux$INGRESO_PROM_4_ANIO), "Major_ID"]
  # Get the second highest 
  aux %<>% subset(Major_ID != major_highest_wage_area_mostcommon_major)
  major_second_highest_wage_area_mostcommon_major = aux[which.max(aux$INGRESO_PROM_4_ANIO), "Major_ID"]
  # Get the third highest 
  aux %<>% subset(Major_ID != major_second_highest_wage_area_mostcommon_major)
  major_third_highest_wage_area_mostcommon_major = aux[which.max(aux$INGRESO_PROM_4_ANIO), "Major_ID"]
  #Get random major
  if(length(major_highest_wage_area_mostcommon_major) != 0 &
     length(major_second_highest_wage_area_mostcommon_major) != 0 &
     length(major_third_highest_wage_area_mostcommon_major) != 0){
    # Check if this recommendation is repeated with second most common major or most common major
    wage_majors = setdiff(c(major_highest_wage_area_mostcommon_major,
                              major_second_highest_wage_area_mostcommon_major,
                              major_third_highest_wage_area_mostcommon_major),
                            c(mostcommon_major, second_mostcommon_major))
    
    major_random_highest_wage_area_mostcommon_major = as.numeric(sample(wage_majors, size = 1, replace = TRUE))
  }else{
    # Check if this recommendation is repeated with second most common major or most common major
    wage_majors = setdiff(c(major_highest_wage_area_mostcommon_major),
                          c(mostcommon_major, second_mostcommon_major))
    if(length(wage_majors > 0)){
      major_random_highest_wage_area_mostcommon_major = wage_majors
    }else{
      major_random_highest_wage_area_mostcommon_major = third_mostcommon_major
    }
  }
  
  # c) Highest wage major within the area of the most common major that is reachable.   
  # Feasible majors
  majors_feasible_student = (majors_feasible_list[[i]] %>% subset(feasible == 1))$major_id_col 
  aux = df2 %>% subset(level_id == 3 & !is.na(INGRESO_PROM_4_ANIO) & Area_ID == area_mostcommon_major) 
  common_feasible_majors = intersect(aux$Major_ID, majors_feasible_student)
  aux %<>% subset(Major_ID %in% common_feasible_majors)
  # Get the top 
  major_highest_wage_area_mostcommon_major_feasible = aux[which.max(aux$INGRESO_PROM_4_ANIO), "Major_ID"]
  # Get the second highest 
  aux %<>% subset(Major_ID != major_highest_wage_area_mostcommon_major_feasible)
  major_second_highest_wage_area_mostcommon_major_feasible = aux[which.max(aux$INGRESO_PROM_4_ANIO), "Major_ID"]
  # Get the third highest 
  aux %<>% subset(Major_ID != major_second_highest_wage_area_mostcommon_major_feasible)
  major_third_highest_wage_area_mostcommon_major_feasible = aux[which.max(aux$INGRESO_PROM_4_ANIO), "Major_ID"]
  #Get random major
  if(length(major_highest_wage_area_mostcommon_major_feasible) != 0 &
     length(major_second_highest_wage_area_mostcommon_major_feasible) != 0 &
     length(major_third_highest_wage_area_mostcommon_major_feasible) != 0){
    # Check if this recommendation is repeated with second most common major or most common major
    wage_majors_feasible = setdiff(c(major_highest_wage_area_mostcommon_major_feasible,
                            major_second_highest_wage_area_mostcommon_major_feasible,
                            major_third_highest_wage_area_mostcommon_major_feasible),
                          c(mostcommon_major, second_mostcommon_major))
    
    major_random_highest_wage_area_mostcommon_major_feasible = as.numeric(sample(wage_majors_feasible, size = 1, replace = TRUE))
  }else{
    # Check if this recommendation is repeated with second most common major or most common major
    wage_majors_feasible = setdiff(c(major_highest_wage_area_mostcommon_major_feasible),
                          c(mostcommon_major, second_mostcommon_major))
    if(length(wage_majors_feasible > 0)){
      major_random_highest_wage_area_mostcommon_major_feasible = wage_majors_feasible
    }else{
      major_random_highest_wage_area_mostcommon_major_feasible = fourth_mostcommon_major
    }
  }
  #d2. If student's score is < 600, add one CFT or IP with highest wage that is in same area.
  aux = df2 %>% subset(level_id != 3 & !is.na(INGRESO_PROM_4_ANIO) & Area_ID == area_mostcommon_major) 
  # Get the top 
  major_highest_wage_area_mostcommon_major_CFT_IP = aux[which.max(aux$INGRESO_PROM_4_ANIO), "Major_ID"]
  # Get the second highest 
  aux %<>% subset(Major_ID != major_highest_wage_area_mostcommon_major_CFT_IP)
  major_second_highest_wage_area_mostcommon_major_CFT_IP = aux[which.max(aux$INGRESO_PROM_4_ANIO), "Major_ID"]
  # Get the third highest 
  aux %<>% subset(Major_ID != major_second_highest_wage_area_mostcommon_major_CFT_IP)
  major_third_highest_wage_area_mostcommon_major_CFT_IP = aux[which.max(aux$INGRESO_PROM_4_ANIO), "Major_ID"]
  #Get random major
  if(length(major_highest_wage_area_mostcommon_major_CFT_IP) != 0 &
     length(major_second_highest_wage_area_mostcommon_major_CFT_IP) != 0 &
     length(major_third_highest_wage_area_mostcommon_major_CFT_IP) != 0){
    # Check if this recommendation is repeated with second most common major or most common major
    wage_majors_CFT_IP = setdiff(c(major_highest_wage_area_mostcommon_major_CFT_IP,
                                     major_second_highest_wage_area_mostcommon_major_CFT_IP,
                                     major_third_highest_wage_area_mostcommon_major_CFT_IP),
                                   c(mostcommon_major, second_mostcommon_major))
    
    major_random_highest_wage_area_mostcommon_major_CFT_IP = as.numeric(sample(wage_majors_CFT_IP, size = 1, replace = TRUE))
  }else{
    # Check if this recommendation is repeated with second most common major or most common major
    wage_majors_CFT_IP = setdiff(c(major_highest_wage_area_mostcommon_major_CFT_IP),
                                   c(mostcommon_major, second_mostcommon_major))
    if(length(wage_majors_CFT_IP > 0)){
      major_random_highest_wage_area_mostcommon_major_CFT_IP = wage_majors_CFT_IP
    }else{
      major_random_highest_wage_area_mostcommon_major_CFT_IP = fourth_mostcommon_major
    }
  }
  aux = c()
  # Student scores < 600 show the CFT & IP
  score_max_lm = common_majors_list[[i]][4]
  if(score_max_lm >= 600){
    #Check repeated recommendation 
    if((major_random_highest_wage_area_mostcommon_major %in% c(mostcommon_major,
                                                              second_mostcommon_major) |
        (major_random_highest_wage_area_mostcommon_major_feasible %in% c(mostcommon_major,
                                                                second_mostcommon_major, 
                                                                major_random_highest_wage_area_mostcommon_major)))){
      major_random_highest_wage_area_mostcommon_major = third_mostcommon_major
      major_random_highest_wage_area_mostcommon_major_feasible = fourth_mostcommon_major
    }
    aux = c(MRUN,
            mostcommon_major,
            second_mostcommon_major,
            major_random_highest_wage_area_mostcommon_major, 
            major_random_highest_wage_area_mostcommon_major_feasible)
  }else{
    #Check repeated recommendation 
    if((major_random_highest_wage_area_mostcommon_major_feasible %in% c(mostcommon_major,
                                                               second_mostcommon_major) |
        (major_random_highest_wage_area_mostcommon_major_CFT_IP %in% c(mostcommon_major,
                                                                         second_mostcommon_major,
                                                                       major_random_highest_wage_area_mostcommon_major_feasible)))){
      major_random_highest_wage_area_mostcommon_major_feasible = third_mostcommon_major
      major_random_highest_wage_area_mostcommon_major_CFT_IP = fourth_mostcommon_major
    }
    aux = c(MRUN,
            mostcommon_major, 
            second_mostcommon_major,
            major_random_highest_wage_area_mostcommon_major_feasible, 
            major_random_highest_wage_area_mostcommon_major_CFT_IP)
  }
  #Return recommendations
  return(aux)
})

#Transform output
recommendation_df = do.call(rbind, recommendation_list)
colnames(recommendation_df) = c('MRUN', 'Rec_1', 'Rec_2', 'Rec_3', 'Rec_4')
recommendation_df = as.data.frame(recommendation_df)
recommendation_df %>% group_by(MRUN) %>% tally() %>% arrange(-n)
sum(duplicated(recommendation_df$MRUN))
#recommendation_df[duplicated(recommendation_df$MRUN),]
#duplicated_mruns = unique(recommendation_df[duplicated(recommendation_df$MRUN),'MRUN'])
#Drop duplicated
#recommendation_df %>% filter(MRUN == duplicated_mruns)
#recommendation_df = recommendation_df[!duplicated(recommendation_df$MRUN),]  

#Reshape to long
recommendation_df_long = reshape(recommendation_df, varying=list(c("Rec_1","Rec_2","Rec_3","Rec_4")),
                   direction="long", timevar=c("Rec"), v.names = c("Major_ID"), idvar=c("MRUN"))

# Save to csv file
write.csv(recommendation_df_long, 
          file = '~/Dropbox/Mistakes/Code/R/recommendation/outputs/recommendations.csv',
          row.names = FALSE)











