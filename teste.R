install.packages("openxlsx")
library(openxlsx)
install.packages("tidyverse")
library(tidyverse)

df <- read.xlsx("AED_CP23_Saúde_Copia.xlsx")
View(df)
colnames(df)[2] <- "Zona Geográfica"
colnames(df)[3] <- "Género"
colnames(df)[4] <- "Idades"
colnames(df)[15] <- "Horas_de_Sono"
colnames(df)[5] <- "Ciclo de Escolaridade"
colnames(df)[6:13] <- gsub("_depressao","",colnames(df)[6:13])
colnames(df)

summary(df)

df[df == 99] <- NA
df$v39[is.na(df$v39)] <- median(df$v39, na.rm = TRUE)
df$v41[is.na(df$v41)] <- median(df$v41, na.rm = TRUE)
df$v46[is.na(df$v46)] <- median(df$v46, na.rm = TRUE)
df$v49[is.na(df$v49)] <- median(df$v49, na.rm = TRUE)
df$v52[is.na(df$v52)] <- median(df$v52, na.rm = TRUE)
df$v53[is.na(df$v53)] <- median(df$v53, na.rm = TRUE)
df$v57[is.na(df$v57)] <- median(df$v57, na.rm = TRUE)
df$v76[is.na(df$v76)] <- median(df$v76, na.rm = TRUE)


