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
    labs(x = "Estadío", y ="Edad de diagnóstico" )


# 5. Estudio de la edad media de diagnóstico para cada estadío de la enfermedad
medias <- tapply(breast_data_stage$age_at_diagnosis,breast_data_stage$stage,mean)
media_por_grupo_frame <- data.frame(
  Estadio = names(medias),
  edad_media = medias,
  row.names = NULL
)

# 6. Test de comparación de medias muestrales independientes para el estudio de diferencias en la edad de diagn?stico cuando se compara el estadio I y estadio III.
## 6.a Extraer en un vector la edad estadío I.
vector_edad_I <- breast_data_stage %>% 
  filter(stage == "Stage I") %>% 
  select(age_at_diagnosis)

vector_edad_I <- as.numeric(vector_edad_I$age_at_diagnosis)

## 6.b Extraer vector de la edad de estadío III
vector_edad_III <- breast_data_stage %>% 
  filter(stage == "Stage III") %>% 
  select(age_at_diagnosis)

vector_edad_III <- as.numeric (vector_edad_III$age_at_diagnosis)
## 6.c Estudio de normalidad con Shapiro-Wilk
shapiro.test(vector_edad_I)
###no significativo -> es normal

shapiro.test(vector_edad_III)
###significativo (p<0.05) -> no es normal

### Comprobación visual
frame_edad_I <- data.frame(vector_edad_I)
ggplot(frame_edad_I, aes(x=vector_edad_I))+
  geom_histogram()

frame_edad_III <- data.frame(vector_edad_III)
ggplot(frame_edad_III, aes(x = vector_edad_III))+
  geom_histogram()

###Varianza de los vectores / homocedasticidad
frame_edad_I_III <- filter(breast_data_stage, stage == "Stage I" | stage == "Stage III")
library(car)
leveneTest(age_at_diagnosis~stage, data = frame_edad_I_III)
#### p > 0.4823. Acepto H0 (varianzas iguales)
#### Entonces Stage_I -> Normal
#            Stage_III -> NO Normal
#             Varianzas -> Iguales, existe homocedasticidad entre los grupos

##6.d aplica el test estadístico adecuado para comparación de medias
#t-student podría si muestras >30 pero lo debo mencionar
#Wilconxon no hace falta normalidad de los datos y las homogeneidad de varianzas no aplica

t.test(age_at_diagnosis~stage, data = frame_edad_I_III, var.equal = TRUE)
wilcox.test(age_at_diagnosis~stage, data = frame_edad_I_III)
#En ambos test p<0,05. Hubiera sido problemático si no hubieran estado de acuerdo.

# 7. Análisis de supervivencia
library(survival)
library(survminer)
## PREPARACIÓN DE DATOS:
### ->Estado (Alive/Dead)
### ->Tiempo
### -> Variable que diferencia grupos(Pr)

breast_data_pr_survival <- breast_data %>% 
  mutate(
    status = case_when(
      vital_status == "Alive" ~ 1,
      vital_status == "Dead" ~ 2),
    time_survival = case_when(
      vital_status == "Alive" ~ last_contact_days_to,
      vital_status == "Dead" ~ death_days_to),
    pr = case_when(
      pr_status_by_ihc == "Positive" ~ "Positive",
      pr_status_by_ihc == "Negative" ~ "Negative",
      pr_status_by_ihc == "[Not Evaluated]" | pr_status_by_ihc == "[Indeterminate]" ~ "Desconocido"
    )) %>%
  filter(pr != "Desconocido") %>% 
  select(status, time_survival,pr)
breast_data_pr_survival$time_survival = as.numeric(breast_data_pr_survival$time_survival)

#Elimino el grupo en el que no conozco el pr (54 casos de los 1097 = 4.9%)

#   
survobject <- Surv(breast_data_pr_survival$time_survival, breast_data_pr_survival$status)
sfit <- survfit(survobject~pr,data= breast_data_pr_survival)
ggsurvplot(sfit, pval = TRUE, risk.table =TRUE, title = "KM Curve for Breast Cancer Survival when considering PR")

