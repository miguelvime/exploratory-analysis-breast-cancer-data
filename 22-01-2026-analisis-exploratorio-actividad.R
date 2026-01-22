#### Actividad Unidad 1, Modulo 4
## Uso de las herramientas R y RStudio para la investigacion biomedica 
# Nombre y apellidos: Miguel Ángel Vicente Mesonero
# Leer y rellenar el documento de actividad correspondiente

####################################################
# Rellenar los comandos necesarios en este archivo para las preguntas propuestas en el documento de actividad.

# Cargar librerías necesarias
library(tidyverse)
library(data.table)
library(dplyr)
# 1. Lectura del archivo nationwidechildrens.org_clinical_patient_brca.txt en data frame 'breast_data'
breast_data<-fread("nationwidechildrens.org_clinical_patient_brca.txt")

## Limpieza: las primeras dos filas no corresponden a la información de los pacientes.
breast_data <- breast_data[-c(1,2),]

# 2 y 3. Centr?donos en la variable ajcc_pathologic_tumor_stage, crear un nuevo data frame (breast_data_stage) que contenga como variables la edad de diagn?stico
#, el estadio del tumor y una variable que agrupe los diferencas estadios en cinco.

breast_data_stage <- breast_data %>% 
  select(age_at_diagnosis,ajcc_pathologic_tumor_stage) %>% 
  mutate(stage = case_when(
    ajcc_pathologic_tumor_stage %in% c("Stage I", "Stage IA", "Stage IB") ~ "Stage I",
    ajcc_pathologic_tumor_stage %in% c("Stage II", "Stage IIA", "Stage IIB") ~ "Stage II",
    ajcc_pathologic_tumor_stage %in% c("Stage III", "Stage IIIA") ~ "Stage III",
    ajcc_pathologic_tumor_stage == "Stage IV" ~ "Stage IV",
    ajcc_pathologic_tumor_stage == "Stage X" ~ "Stage X"),
    age_at_diagnosis = as.numeric(age_at_diagnosis)
  )

# 4. Análisis exploratorio de diagrama de cajas (boxplot)
ggplot(breast_data_stage,aes(x = stage,y = age_at_diagnosis)) +
    geom_boxplot(fill = "lightblue", colour = "black") +
    labs(x = "Estadío", y ="Edad" )


# 5. Estudio de la edad media de diagn?stico para cada estadio de la enfermedad


# 6. Test de comparaci?n de medias muestrales independientes para el estudio de diferencias en la edad de diagn?stico cuando se compara el estadio I y estadio III.


# 7. An?lisis de supervivencia



