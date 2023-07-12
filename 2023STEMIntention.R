######
# INTERNET AT HOME AND THE INTENTION TO STUDY STEM EDUCATION
# Presented by: Juan David Garcia-Gonzalez
# Due Date: July 2023
######

###
# WARM UP: SETTING THE WORKING DIRECTORY & PACKAGES
###

# Setting the Working Directory
setwd("C:/Users/Juan David Garcia/OneDrive - University College London/Bocconi University/PhD Studies/Summer School_Javeriana/Data")
getwd()

# Installing Packages
install.packages("readxl")
library(readxl)
install.packages("openxlsx")
library(openxlsx)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
install.packages("marginaleffects")
library(marginaleffects)
install.packages("MatchIt")
library(MatchIt)

###
# CONSOLIDATION OF THE DATASET
###

## READING THE ORIGINAL DATABASES
# Note: The original was transformed from a txt to an excel file

## 2015_1 (First semester of 2015) database
# I started only with one period. But I will add more periods in the coming weeks
Data_20151 <- read_excel("2015_1.xlsx")
# Filtering the database by the questionnaire to pursur higher education
Data20151 <- Data_20151 %>% filter(ESTU_ANTECEDENTES == "Si")
write.xlsx(Data20151, file = "20151.xlsx")
Data_20151 <- read_excel("20151.xlsx")
summary(Data20151)
saveRDS(Data20151, file = "Data_20151.rds")

## WRANGLING AND ORGANIZING DATA

# Deleting the columns with high proportion of missing Values
# Note: 6 columns about students limitations were deleted give more than
# 95% of NAs
missingcounts <- colSums(is.na(Data20151))
missingcounts
column_table <- data.frame(
  Column_Name = names(Data20151),
  Column_Number = 1:ncol(Data20151)
)
print(column_table)
Data20151 <- Data20151[, c(-10:-15)]

# Turning some Variables into dichotomous ones
Data20151$ESTU_GENERO <- ifelse(Data20151$ESTU_GENERO == "M", 0, 1)
Data20151[,c(27:35)] <- ifelse(Data20151[,c(27:35)] == "No", 0, 1)
Data20151[,c(45:54)] <- ifelse(Data20151[,c(45:54)] == "No", 0, 1)
Data20151[,c(67:73)] <- ifelse(Data20151[,c(67:73)] == "No", 0, 1)
Data20151[,c(76:82)] <- ifelse(Data20151[,c(76:82)] == "si", 1, 0)
Data20151$EduSuperior <- ifelse(Data20151$ESTU_TIPOCARRERADESEADA == "Ninguna", 0, 1)
Data20151$EduProfesional <- ifelse(Data20151$ESTU_TIPOCARRERADESEADA == "Profesional", 1, 0)

# Mutating variables for easing analysis
Data20151 <- Data20151 %>%
  mutate(FAMI_EDUCACIONMADRE = case_when(FAMI_EDUCACIONMADRE == "Primaria incompleta" ~ '0',
                                         FAMI_EDUCACIONMADRE == "Primaria completa"|FAMI_EDUCACIONMADRE == "Secundaria (Bachillerato) incompleta" ~ '1',
                                         FAMI_EDUCACIONMADRE == "Secundaria (Bachillerato) completa"|FAMI_EDUCACIONMADRE == "Técnica o tecnológica incompleta"|FAMI_EDUCACIONMADRE == "Educación profesional incompleta" ~ '2',
                                         FAMI_EDUCACIONMADRE == "Educación profesional completa"|FAMI_EDUCACIONMADRE == "Técnica o tecnológica completa" ~ '3',
                                         FAMI_EDUCACIONMADRE == "Postgrado" ~ '4',
                                         TRUE ~ '99'))
Data20151$FAMI_EDUCACIONMADRE[Data20151$FAMI_EDUCACIONMADRE == "99"] <- 0

Data20151 <- Data20151 %>%
  mutate(FAMI_EDUCACIONPADRE = case_when(FAMI_EDUCACIONPADRE == "Primaria incompleta" ~ '0',
                                         FAMI_EDUCACIONPADRE == "Primaria completa"|FAMI_EDUCACIONMADRE == "Secundaria (Bachillerato) incompleta" ~ '1',
                                         FAMI_EDUCACIONPADRE == "Secundaria (Bachillerato) completa"|FAMI_EDUCACIONMADRE == "Técnica o tecnológica incompleta"|FAMI_EDUCACIONMADRE == "Educación profesional incompleta" ~ '2',
                                         FAMI_EDUCACIONPADRE == "Educación profesional completa"|FAMI_EDUCACIONMADRE == "Técnica o tecnológica completa" ~ '3',
                                         FAMI_EDUCACIONPADRE == "Postgrado" ~ '4',
                                         TRUE ~ '99'))
Data20151$FAMI_EDUCACIONPADRE[Data20151$FAMI_EDUCACIONPADRE == "99"] <- 0

Data20151$EducacionPadres <- as.numeric(Data20151$FAMI_EDUCACIONPADRE)+as.numeric(Data20151$FAMI_EDUCACIONMADRE)

Data20151 <- Data20151 %>%
  mutate(ESTU_ETNIA = case_when(ESTU_ETNIA == "Cancuamo"|ESTU_ETNIA == "Comunidad afrodescendiente"|
                                  ESTU_ETNIA == "Comunidades Rom (Gitanas)"|ESTU_ETNIA == "Cubeo"|
                                  ESTU_ETNIA == "Emberá"|ESTU_ETNIA == "Guambiano"|
                                  ESTU_ETNIA == "Inga"|ESTU_ETNIA == "Otro grupo étnico minoritario"|
                                  ESTU_ETNIA == "Paez"|ESTU_ETNIA == "Palenquero"|
                                  ESTU_ETNIA == "Pasto"|ESTU_ETNIA == "Pijao"|
                                  ESTU_ETNIA == "Raizal"|ESTU_ETNIA == "Sikuani"|
                                  ESTU_ETNIA == "Tucano"|ESTU_ETNIA == "Wayúu" ~ 1,
                                         TRUE ~ 0))

Data20151$PCInternet <- Data20151$FAMI_TIENECOMPUTADOR*Data20151$FAMI_TIENEINTERNET  
Data20151$ESTU_NSE_INDIVIDUAL[is.na(Data20151$ESTU_NSE_INDIVIDUAL)] <- "NSE4"
Data20151$COLE_CARACTER[is.na(Data20151$COLE_CARACTER)] <- "NO APLICA"

Data20151$Reprobo <- Data20151$ESTU_REPROBOPRIMERO + 
  Data20151$ESTU_REPROBOSEGUNDO + Data20151$ESTU_REPROBOTERCERO +
  Data20151$ESTU_REPROBOCUARTO + Data20151$ESTU_REPROBOQUINTO +
  Data20151$ESTU_REPROBOSEXTO + Data20151$ESTU_REPROBOSEPTIMO +
  Data20151$ESTU_REPROBOOCTAVO + Data20151$ESTU_REPROBONOVENO + 
  Data20151$ESTU_REPROBOONCEMAS
table(Data20151$Reprobo)
Data20151$Reprobo <- ifelse(Data20151$Reprobo > 0, 1, 0)

## EXPORTING THE CONSOLIDATED DATABASE
write.xlsx(Data20151, file = "C20151.xlsx")
Data20151 <- read_excel("C20151.xlsx")

###
# MAPS OF INTERNET ACCESS AND INENTION TO PURSUE (STEM) HIGHER EDUCATION
###

## INSTALLING PACKAGES

install.packages(c("ggplot2", "classInt", "reshape", "reshape2", "tidyr", "rgdal", "tmap", "sp", ""))
require(ggplot2)
require(reshape)
require(reshape2)
require(tidyr)
require(rgdal)
require(tmap)
library(classInt)

## FREQUENCY DATABASE FOR INTERNET AND HIGHER (STEM) EDUCATION

Internet <- Data20151 %>%
  group_by(COLE_COD_MCPIO_UBICACION) %>%
  summarise(mean(FAMI_TIENEINTERNET, na.rm=TRUE))
write.csv2(Internet, "Internet.csv")

EduSuperior <- Data20151 %>%
  group_by(COLE_COD_MCPIO_UBICACION) %>%
  summarise(mean(EduSuperior, na.rm=TRUE))
EduSuperior
write.csv2(EduSuperior, "EduSuperior.csv")

EduProfesional <- Data20151 %>%
  group_by(COLE_COD_MCPIO_UBICACION) %>%
  summarise(mean(EduProfesional, na.rm=TRUE))
write.csv2(EduProfesional, "EduProfesional.csv")

EduSTEM <- Data20151 %>%
  group_by(COLE_COD_MCPIO_UBICACION) %>%
  summarise(mean(EduSTEM, na.rm=TRUE))
write.csv2(EduSTEM, "EduSTEM.csv")

# Consolidated Database

MapsDatabase <- read.csv2("map.csv", header=TRUE)

## INTERNET MAP
mapa <- readOGR("mpio.shp",layer="mpio")
plot(mapa)
datos_mapa <- read.csv2("map.csv",header=TRUE)
datos_mapa$ID_ESPACIA <- mapa@data$ID_ESPACIA 
row.names(datos_mapa) <- row.names(mapa) 
mapa.datos <- SpatialPolygonsDataFrame(mapa,datos_mapa) #Merging
plotvar <- mapa.datos$Internet #New variable
class(plotvar)
nclr <- 5 # Colors
plotclr <- c("White","#85B6FF", "#005DE6", "#003B92", "#000F26") #Creando paleta
class <- classIntervals(plotvar,nclr, style = "equal") #Creando los intervalos
colcode <- findColours(class,plotclr)
# Legend
legendText <- c("No Data","0%","(0-50]%","(50-100)%","100%")
# Exporting Map
jpeg("Internet.jpeg",quality=10000,height = 2400,width = 3200)
plot (mapa.datos, col=colcode, border="gray40", axes=F)
legend("bottomleft",
       title = "Internet coverage",
       legend = legendText,    
       fill= attr(colcode,"palette"),
       cex=5,                  
       bty = "n")
dev.off()

## HIGHER EDUCATION MAP
mapa <- readOGR("mpio.shp",layer="mpio")
datos_mapa <- read.csv2("map.csv",header=TRUE)
datos_mapa$ID_ESPACIA <- mapa@data$ID_ESPACIA 
row.names(datos_mapa) <- row.names(mapa) 
mapa.datos <- SpatialPolygonsDataFrame(mapa,datos_mapa) #Merging
plotvar <- mapa.datos$EduSuperior #New variable
class(plotvar)
nclr <- 5 # Colors
plotclr <- c("White","#AEAEAE", "#7D7D7D", "#4F4F4F", "#0C0C0C") #Creando paleta
class <- classIntervals(plotvar,nclr, style = "equal") #Creando los intervalos
colcode <- findColours(class,plotclr)
# Legend
legendText <- c("No Data","0%","(0-50]%","(50-100)%","100%")
# Exporting Map
jpeg("Higher Education Intention.jpeg",quality=10000,height = 2400,width = 3200)
plot (mapa.datos, col=colcode, border="gray40", axes=F)
legend("bottomleft",
       title = "Higher Education Intention",
       legend = legendText,    
       fill= attr(colcode,"palette"),
       cex=5,                  
       bty = "n")
dev.off()

## HIGHER STEM EDUCATION MAP
mapa <- readOGR("mpio.shp",layer="mpio")
datos_mapa <- read.csv2("map.csv",header=TRUE)
datos_mapa$ID_ESPACIA <- mapa@data$ID_ESPACIA 
row.names(datos_mapa) <- row.names(mapa) 
mapa.datos <- SpatialPolygonsDataFrame(mapa,datos_mapa) #Merging
plotvar <- mapa.datos$EduSTEM #New variable
class(plotvar)
nclr <- 5 # Colors
plotclr <- c("White","#AEAEAE", "#7D7D7D", "#4F4F4F", "#0C0C0C") #Creando paleta
class <- classIntervals(plotvar,nclr, style = "equal") #Creando los intervalos
colcode <- findColours(class,plotclr)
# Legend
legendText <- c("No Data","0%","(0-33)%","[33-67)%","[67-100]%")
# Exporting Map
jpeg("Higher STEM Education Intention.jpeg",quality=10000,height = 2400,width = 3200)
plot (mapa.datos, col=colcode, border="gray40", axes=F)
legend("bottomleft",
       title = "STEM Education Intention",
       legend = legendText,    
       fill= attr(colcode,"palette"),
       cex=5,                  
       bty = "n")
dev.off()

###
# DESCRIPTIVE STATISTICS
###

## INSTALLING PACKAGES

install.packages(c("summarytools", "flextable", "officer"))
library(summarytools)
library(flextable)
library(officer)

## DESCRIPTIVE STATISTICS FOR OTUCOME AND EXPLANATORY VARIABLES

table(Data20151$FAMI_TIENEINTERNET)
table(Data20151$EduSuperior)
table(Data20151$EduProfesional)
table(Data20151$EduSTEM)

# Creating a summary of the Outcome and the Explanatory variables
table <- descr(Data20151[, c("EduSuperior", "EduProfesional", "EduSTEM", "FAMI_TIENEINTERNET")])
table <- flextable(as.data.frame(table))
table
doc <- read_docx("Tables.docx")
doc <- body_add_flextable(doc, value = table)
print(doc, target = "descriptive_stats.docx")

## DESCRIPTIVE STATISTICS FOR CONTROL VARIABLES
# These variables create the common support in the matching 
table(Data20151$ESTU_GENERO)
table(Data20151$ESTU_ETNIA)
table(Data20151$COLE_CARACTER)
table(Data20151$COLE_NATURALEZA)
table(Data20151$FAMI_EDUCACIONMADRE)
table(Data20151$FAMI_ESTRATOVIVIENDA)
table(Data20151$ESTU_NSE_INDIVIDUAL)

Controls <- cbind.data.frame(Data20151$ESTU_GENERO, Data20151$ESTU_ETNIA, 
              Data20151$FAMI_EDUCACIONMADRE, Data20151$FAMI_ESTRATOVIVIENDA,
              Data20151$ESTU_NSE_INDIVIDUAL, Data20151$COLE_CARACTER, 
              Data20151$COLE_NATURALEZA)

###
# SPURIOUS REGRESSIONS
# Note: only for get to know a bit more about the Data
###

# Intention to Higher Education
model1 <- glm (Data20151$EduSuperior ~ Data20151$FAMI_TIENEINTERNET +
               Data20151$FAMI_TIENECOMPUTADOR + 
               Data20151$FAMI_TIENEINTERNET*Data20151$FAMI_TIENECOMPUTADOR, 
               family = "quasibinomial")
summary(model1)

# Intention to Higher (Professional) Education
model2 <- glm (Data20151$EduProfesional ~ Data20151$FAMI_TIENEINTERNET +
                 Data20151$FAMI_TIENECOMPUTADOR + 
                 Data20151$FAMI_TIENEINTERNET*Data20151$FAMI_TIENECOMPUTADOR, 
               family = "quasibinomial")
summary(model2)

# Intention to STEM Higher Education
model3 <- glm (Data20151$EduSTEM ~ Data20151$FAMI_TIENEINTERNET +
                 Data20151$FAMI_TIENECOMPUTADOR + 
                 Data20151$FAMI_TIENEINTERNET*Data20151$FAMI_TIENECOMPUTADOR, 
               family = "quasibinomial")
summary(model3)

###
# PROPENSITY SCORE MATCHING
###

## MATCHING

table(Data20151$FAMI_TIENEINTERNET)
Initial_Imbalance <- matchit(FAMI_TIENEINTERNET ~ ESTU_GENERO + ESTU_ETNIA +
                       FAMI_EDUCACIONMADRE +  FAMI_ESTRATOVIVIENDA + 
                       ESTU_NSE_INDIVIDUAL + COLE_CARACTER + 
                       COLE_NATURALEZA, data = Data20151, 
                     method = NULL, distance = "glm")
summary(Initial_Imbalance)

psm_model0 <- matchit(FAMI_TIENEINTERNET ~ ESTU_GENERO + ESTU_ETNIA +
                               FAMI_EDUCACIONMADRE +  FAMI_ESTRATOVIVIENDA + 
                               ESTU_NSE_INDIVIDUAL + COLE_CARACTER + 
                               COLE_NATURALEZA, data = Data20151, 
                             method = "nearest", distance = "glm")
summary(psm_model0)

psm_model <- matchit(FAMI_TIENEINTERNET ~ ESTU_GENERO + ESTU_ETNIA +
                       FAMI_EDUCACIONMADRE +  FAMI_ESTRATOVIVIENDA + 
                       ESTU_NSE_INDIVIDUAL + COLE_CARACTER + 
                       COLE_NATURALEZA, data = Data20151, 
                     method = "full", distance = "glm", link = "probit")
summary(psm_model, un = FALSE)
plot(summary(psm_model))
plot(psm_model, type = "jitter", interactive = FALSE)

# Creating the Matched Dataset
MatchedData <- match.data(psm_model)
head(MatchedData)

## RESULTS

# Results for pursuing Higher Education
MatchedData$EduSuperior
fit <- glm(EduSuperior ~
            FAMI_TIENEINTERNET * (ESTU_GENERO + ESTU_ETNIA +
                                    FAMI_EDUCACIONMADRE +  FAMI_ESTRATOVIVIENDA +
                                    ESTU_NSE_INDIVIDUAL + COLE_CARACTER +
                                    COLE_NATURALEZA), 
           data = MatchedData, family = "quasibinomial", weights = weights)
summary(fit)

avg_comparisons(fit,
                variables = "FAMI_TIENEINTERNET",
                vcov = ~subclass,
                newdata = subset(MatchedData, FAMI_TIENEINTERNET == 1),
                wts = "weights", 
                comparison = "lnratioavg", 
                transform = "exp")

# Results for pursuing Professional Education
MatchedData$EduProfesional
fit1 <- glm(EduProfesional ~
            FAMI_TIENEINTERNET * (ESTU_GENERO + ESTU_ETNIA +
                                    FAMI_EDUCACIONMADRE +  FAMI_ESTRATOVIVIENDA +
                                    ESTU_NSE_INDIVIDUAL + COLE_CARACTER +
                                    COLE_NATURALEZA),
            data = MatchedData, family = "quasibinomial", weights = weights)
summary(fit1)

avg_comparisons(fit1,
                variables = "FAMI_TIENEINTERNET",
                vcov = ~subclass,
                newdata = subset(MatchedData, FAMI_TIENEINTERNET == 1),
                wts = "weights", 
                comparison = "lnratioavg", 
                transform = "exp")

# Results for pursuing STEM Higher Education
table(MatchedData$EduSTEM)
fit2 <- glm(EduSTEM ~
            FAMI_TIENEINTERNET * (ESTU_GENERO + ESTU_ETNIA +
                                     FAMI_EDUCACIONMADRE +  FAMI_ESTRATOVIVIENDA +
                                     ESTU_NSE_INDIVIDUAL + COLE_CARACTER +
                                     COLE_NATURALEZA),
            data = MatchedData, family = "quasibinomial", weights = weights)
summary(fit2)

avg_comparisons(fit2,
                variables = "FAMI_TIENEINTERNET",
                vcov = ~subclass,
                newdata = subset(MatchedData, FAMI_TIENEINTERNET == 1),
                wts = "weights", 
                comparison = "lnratioavg", 
                transform = "exp")

######
# END
######