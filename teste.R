# Bibliotecas
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("descr")
install.packages("flextable")
install.packages("moments")
install.packages("DescTools")

library(openxlsx)
library(tidyverse)
library(descr)
library(flextable)
library(moments)
library(DescTools)

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
colnames(df)[2] <- "Zona.geografica"
colnames(df)[3] <- "Genero"
colnames(df)[4] <- "Idades"
colnames(df)[15] <- "Horas.de.Sono"
colnames(df)[5] <- "Ciclo.de.Escolaridade"

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
gen_n <- table(df$Genero)
gen_n

# Observou-se que os géneros não tem o seu código atribuído
# Atribuição do código ao respetivo número
df$Genero <- factor(df$Genero, levels=c(1:4), labels=c("Masculino","Feminino","Outro","Prefiro não responder"))

# Tabela com códigos atualizados
gen_n <- table(df$Genero)
gen_n

# Criação da tabela com Frequências relativas
percen <- round((prop.table(gen_n)*100),1)
percen

# Tabela Final 
tab_generos <- rbind(gen_n,percen)
tab_generos

# Criação de FlexTable. Univariada com o objetivo de entender a % de género da amostra.
# Definição de numeric as frequencias absolutas
masc_n <- as.numeric(gen_n[1])
fem_n <- as.numeric(gen_n[2])
outro_n <- as.numeric(gen_n[3])
nao_resp_n <- as.numeric(gen_n[4])

# Definição de numeric as frequencias relativas
masc_p <- as.numeric(percen[1])
fem_p<- as.numeric(percen[2])
outro_p <- as.numeric(percen[3])
nao_resp_p <- as.numeric(percen[4])

# Criação dos headers
Genero <- c("Masculino", "Feminino", "Outro", "Não respondeu")
Percentagem <- c(masc_p, fem_p, outro_p, nao_resp_p)
n <- c(masc_n,fem_n,outro_n,nao_resp_n)

table_gen <- data.frame(Genero,n, Percentagem)
ftable_gen <- flextable(table_gen)
ftable_gen

# Customização da flextable
ftable_gen <- bg(ftable_gen, bg = "#3895D3", part = "header")
ftable_gen

ftable_gen <- color(ftable_gen, color = "white", part = "header")
ftable_gen <- autofit(ftable_gen) 
ftable_gen

# Medidas descritivas de uma variável quantitativa: Horas de sono
# A FAZER - VER SCRIPT TABELAS2_TP7+TP8
# A FAZER - VER SCRIPT TABELAS2_TP7+TP8
# A FAZER - VER SCRIPT TABELAS2_TP7+TP8
# A FAZER - VER SCRIPT TABELAS2_TP7+TP8
# A FAZER - VER SCRIPT TABELAS2_TP7+TP8
# A FAZER - VER SCRIPT TABELAS2_TP7+TP8
# A FAZER - VER SCRIPT TABELAS2_TP7+TP8
# A FAZER - VER SCRIPT TABELAS2_TP7+TP8
# A FAZER - VER SCRIPT TABELAS2_TP7+TP8
# A FAZER - VER SCRIPT TABELAS2_TP7+TP8
summary(df$Horas.de.Sono)

# Gráfico com um fundo menos marcante e barras azuis
Histogram_H_Sono <- ggplot(data=df, aes(Horas.de.Sono))+geom_bar()
Histogram_H_Sono

# FAZER BARRAS 4,5 E 6 NUMA SÓ
# FAZER BARRAS 4,5 E 6 NUMA SÓ
# FAZER BARRAS 4,5 E 6 NUMA SÓ
# FAZER BARRAS 4,5 E 6 NUMA SÓ
# FAZER BARRAS 4,5 E 6 NUMA SÓ
# FAZER BARRAS 4,5 E 6 NUMA SÓ
Histogram_H_Sono+geom_bar(fill='skyblue')+ theme_minimal()+xlab("Horas de Sono") + ylab("Frequência")


