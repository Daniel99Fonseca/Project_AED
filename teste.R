# Bibliotecas
install.packages("openxlsx")
library(openxlsx)
install.packages("tidyverse")
library(tidyverse)

# Leitura do Excel
df <- read.xlsx("AED_CP23_Saúde_Copia.xlsx")

# Qual o tipo de classe: data.frame
class(df)

# Ver o DataFrame dentro do R numa nova janela
View(df)

# Averiguar o nome das colunas do DataFrame
colnames(df)

# Mudança de nome de várias colunas para ser mais acessível
# Não foram usados carateres especiais para facilitar a parte do código do
# projeto
colnames(df)[2] <- "Zona geografica"
colnames(df)[3] <- "Genero"
colnames(df)[4] <- "Idades"
colnames(df)[15] <- "Horas de Sono"
colnames(df)[5] <- "Ciclo de Escolaridade"

# Remoção de ' depressão' dos nomes das colunas
colnames(df)[6:13] <- gsub("_depressao","",colnames(df)[6:13])
colnames(df)

# Verificação de summary do dataframe, podemos ver que há bastantes '99'
summary(df)

# Atribuição do código 99 ('não-resposta') ao valor missing
df[df == 99] <- NA

#Substituição dos valores missing pela mediana da respetiva coluna.
# Dizer que podíamos usar a função lapply, mas foi sem sucesso. Seria uma forma
# mais eficiente de fazer este passo.
df$v39[is.na(df$v39)] <- median(df$v39, na.rm = TRUE)
df$v41[is.na(df$v41)] <- median(df$v41, na.rm = TRUE)
df$v46[is.na(df$v46)] <- median(df$v46, na.rm = TRUE)
df$v49[is.na(df$v49)] <- median(df$v49, na.rm = TRUE)
df$v52[is.na(df$v52)] <- median(df$v52, na.rm = TRUE)
df$v53[is.na(df$v53)] <- median(df$v53, na.rm = TRUE)
df$v57[is.na(df$v57)] <- median(df$v57, na.rm = TRUE)
df$v76[is.na(df$v76)] <- median(df$v76, na.rm = TRUE)

# Tabela com valores absolutos e percentuais sobre género (uni variada)
n <- table(df$Genero)
n

# Observou-se que os géneros não tem o seu código atribuído
# Atribuição do código ao respetivo número
df$Genero <- factor(df$Genero, levels=c(1:4), labels=c("Masculino","Feminino","Outro","Prefiro não responder"))

# Tabela com códigos atualizados
n <- table(df$Genero)
n

# Criação da tabela com Frequências relativas
Percen <- round((prop.table(n)*100),1)
Percen

# Tabela Final 
tab_generos <- rbind(n,Percen)
tab_generos
