install.packages("openxlsx")
library(openxlsx)

df <- read.xlsx("AED_CP23_Saúde_Copia.xlsx")
View(df)
colnames(df)[2] <- "Zona Geográfica"
colnames(df)[3] <- "Género"
colnames(df)[4] <- "Idades"
colnames(df)[15] <- "Horas de Sono"
colnames(df)[5] <- "Ciclo de Escolaridade"
colnames(df)[6:13] <- gsub("_depressao","",colnames(df)[6:13])
colnames(df)

summary(df)

df[df == 99] <- NA
