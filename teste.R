# Bibliotecas
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("descr")
install.packages("flextable")
install.packages("moments")
install.packages("DescTools")
install.packages("ggplot2")

library(openxlsx)
library(tidyverse)
library(descr)
library(flextable)
library(moments)
library(DescTools)
library(ggplot2)

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

##################################
# Tabela com valores absolutos e percentuais sobre idades (uni variada)
idade_n <- table(df$Idades)
idade_n

# Tabela com códigos atualizados
idade_n <- table(df$Idades)
idade_n

# Criação da tabela com Frequências relativas
percen <- round((prop.table(idade_n)*100),1)
percen

# Tabela Final 
tab_Idadess <- rbind(idade_n,percen)
tab_Idadess

# Criação de FlexTable. Univariada com o objetivo de entender a % de idades da amostra.
# Definição de numeric as frequencias absolutas
um_n <- as.numeric(idade_n[1])
dois_n <- as.numeric(idade_n[2])
tres_n <- as.numeric(idade_n[3])
quatro_n <- as.numeric(idade_n[4])
cinco_n <- as.numeric(idade_n[5])
seis_n <- as.numeric(idade_n[6])
sete_n <- as.numeric(idade_n[7])
oito_n <- as.numeric(idade_n[8])
nove_n <- as.numeric(idade_n[9])
dez_n <- as.numeric(idade_n[10])

# Definição de numeric as frequencias relativas
um_p <- as.numeric(percen[1])
dois_p<- as.numeric(percen[2])
tres_p <- as.numeric(percen[3])
quatro_p <- as.numeric(percen[4])
cinco_p <- as.numeric(percen[5])
seis_p <- as.numeric(percen[6])
sete_p <- as.numeric(percen[7])
oito_p <- as.numeric(percen[8])
nove_p <- as.numeric(percen[9])
dez_p <- as.numeric(percen[10])

# Criação dos headers
Idades <- c("11","12","13","14","15","16","17","18","19","20")
Percentagem <- c(um_p, dois_p, tres_p, quatro_p, cinco_p, seis_p, sete_p, oito_p, nove_p, dez_p)
n <- c(um_n,dois_n,tres_n,quatro_n,cinco_n,seis_n,sete_n,oito_n,nove_n,dez_n)

table_idade <- data.frame(Idades,n, Percentagem)
ftable_idade <- flextable(table_idade)
ftable_idade

# Customização da flextable
ftable_idade <- bg(ftable_idade, bg = "#3895D3", part = "header")
ftable_idade
ftable_idade <- color(ftable_idade, color = "white", part = "header")
ftable_idade <- autofit(ftable_idade) 
ftable_idade

#############################################

# Medidas descritivas de uma variável quantitativa: Horas de sono
class(df$Horas.de.Sono)
mean(df$Horas.de.Sono)
mean(df$Horas.de.Sono,na.rm=TRUE)
round(mean(df$Horas.de.Sono, na.rm = TRUE),1)

n <- length(df$Horas.de.Sono)
media <- round(mean(df$Horas.de.Sono,na.rm=TRUE),1)
mediana <- median(df$Horas.de.Sono,na.rm=TRUE)
desvpadr <- round(sd(df$Horas.de.Sono, na.rm=TRUE),1)
varianc <- round(var(df$Horas.de.Sono, na.rm=TRUE),1)
minimo <- min(df$Horas.de.Sono, na.rm=TRUE)
maximo <- max(df$Horas.de.Sono, na.rm=TRUE)
firstqt <- round(quantile(df$Horas.de.Sono, 0.25, na.rm=TRUE),1)
thirdqt <- round(quantile(df$Horas.de.Sono, 0.75, na.rm=TRUE), 1)
assimetria <- round(skewness(df$Horas.de.Sono, na.rm=TRUE),1)
curtose <- round(kurtosis(df$Horas.de.Sono, na.rm=TRUE),1)


# construção do data frame para a tebela de medidas descritivas
table_agreg <- data.frame(
  Estatística = c("Média",
                  "Mediana",
                  "Desvio Padrão",
                  "Variância",
                  "Mínimo",
                  "Máximo", 
                  "1º Quartil",
                  "3º Quartil",
                  "Assimetria",
                  "Curtose"),
  Valor = c(media,
            mediana,
            desvpadr,
            varianc,
            minimo,
            maximo,
            firstqt,
            thirdqt,
            assimetria,
            curtose)
)

# construção da tabela de medidas descritivas
ftab_desc_sono <- flextable(table_agreg)
ftab_desc_sono

#cor no fundo do cabeçalho - bg
#codigo de cores
ftab_desc_sono <- bg(ftab_desc_sono, bg = "#3895D3", part = "header")
ftab_desc_sono

#cor da letra do cabeçalho em branco
ftab_desc_sono <- color(ftab_desc_sono, color = "white", part = "header")
ftab_desc_sono <- autofit(ftab_desc_sono)
ftab_desc_sono
summary(df$Horas.de.Sono)
############################################

#boxplot med idade
#calculo da medida da tabela Idades
media_idade <- mean(df$Idades, na.rm = TRUE)

#criar o data frame para a med idade 
df_media <- data.frame(Grupo = "Média das Idades", Idades = media_idade)

#criar o boxplot 
ggplot(df, aes(y = Idades)) + geom_boxplot() + 
  labs(title = "Boxplot das Idades", y = "Idades") +
  theme_minimal()
######################################


# Questão 1
# Histograma das Horas de Sono
Histogram_H_Sono <- ggplot(data=df, aes(Horas.de.Sono))+geom_bar()
Histogram_H_Sono
Histogram_H_Sono+geom_bar(fill='skyblue')+ theme_minimal()+xlab("Horas de Sono") + ylab("Frequência")

# Histograma das idades
<<<<<<< HEAD
h_idades <- hist(df$Idades,breaks = c(10,12,14,16,18,20), col=c("Light blue", "darkblue","blue","navy"))
=======
hist(df$Idades,breaks = c(10,12,14,16,20), col=c("Light blue", "darkblue","blue","navy"))
>>>>>>> 64cfd4d6b73657827741207f4ee80cb3dec10aa2

# Questão 2
# Gráfico de Barras - Género vs Depressão
# Criação da coluna Média de respostas de depressão
df$Mediadp <- (df$v39 + df$v41 + df$v46 + df$v49 + df$v52 + df$v53 + df$v57)/7

# Aplicação de filtro para obter apenas Genero Masculino e Feminino
df_q2 <- df %>% filter(Genero %in% c("Masculino", "Feminino"))

# Criação do gráfico de barras de Género e Depressão
ggplot(df_q2, aes(x=Genero,y=Mediadp, fill=Genero)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title="Relação entre Género e Depressão",x="Género",y="Média de Níveis de Depressão") +
  scale_fill_manual(values = c("blue","pink")) +
  theme_minimal()

# Remover Idades 19 e 20 por haver apenas 1 de cada, resultando numa média "biased" em relação a estas idades
df_q3 <- df
df_q3 <- df%>% filter(!(Idades %in% c(19,20)))

# Questão 4
# Gráfico de barras para analise das médias de depressão por idade
ggplot(df_q3, aes(x=as.factor(Idades),y=Mediadp, fill=Idades)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title="Relação entre Idade e Depressão",x="Idade",y="Média de Níveis de Depressão") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), # remove a escala numérica da média
                     labels = c("Menos sintomas\nde depressão", "", "", "", "Mais sintomas\nde depressão")) + #adiciona nova legenda ao y
  theme(legend.position = 'none') # remove legenda à direita
