#...................................Ketherini F Girardello......................................#
#............................https://github.com/ketgirardello...................................#

# AULA SEDIMENTOLOGIA
# Dados coletados na aula de campo

# Instalando a nova versão do rysgran (rysgran_2.2.0.tar.gz)
          install.packages("remotes")
          library("remotes")
          remotes::install_github("mauricio-camargo/rysgran")

# Método de Peneiramento e Pipetagem. Granulometria:
# preparando a planilha
setwd("C:/Users/Keth/Desktop/TCC/1TCC/Granulometria/Sedimentologia_23")

library(readxl)
library(tidyverse)
# Antes de carregar a planilha, 
# certificar que os valores estão classificados como números
dados= read_excel('dados-granulometricos-sed-2023.xlsx',col_names = T)
dados # Aparecem NAs
dados=dados[-2,]
dados #read_excel traz a planilha como "Tibble"
dad=as.data.frame(dados) # transforma para dataframe.
rownames(dad)# esta com numeração das linhas

as.vector(dad[,1]) # função para transformar os nomes das amostras em vetores
rownames(dad)=as.vector(dad[,1])  #Atribui nome das linhas à primeira coluna. 
dad # tem duas colunas de nomes
dad=dad[,-1] #Exclui a primeira coluna

View(dad)
summary(dad) # estatistica basica por coluna ou por fração granulometrica

library(rysgran)
rownames(dad)

# Criando os parametros granulometricos por FOLK n' Ward
rys= gran.stats (dad, method="folk", verbal=FALSE)
rys  #mean / median / sorting / skewness / kurtosis
# salvar rys como planilha excel:
library("writexl")
write_xlsx(x=rys, 
           path = "C:/Users/Keth/Desktop/TCC/1TCC/Granulometria/Sedimentologia_23//PEG_rys_Sedimentologia_23.xlsx")

#weight percentages
cl=class.percent (dad, mode="total")
cl
write_xlsx(x=cl, 
           path = "C:/Users/Keth/Desktop/TCC/1TCC/Granulometria/Sedimentologia_23//PEG_percents_Sedimentologia_23.xlsx")

### HISTOGRAMAS
# São 24 histogramas!! 6*4=24

env4<- rep(c("I","E","D","K"),each=6)
env4
env4<- factor(env4,levels= c("I","E", "D", "K"))

hist<- rysgran.hist(dad,subset = env4, which= "I")
hist


hist <- rysgran.hist(dad,subset = env4, which= "E")
hist

hist <- rysgran.hist(dad,subset = env4, which= "D")
hist


hist <- rysgran.hist(dad,subset = env4, which= "K")
hist


# DIAGRAMAS, Shepard, Pejrup, Flemming. Classificam as texturas por subdivisões granulométricas.
#

# APENAS PARA VISUALIZAÇÃO com as 4 variáveis:
percent <- class.percent(dad, mode="total")
x<-percent[1:4] 

rysgran.ternary  (x, method= "shepard", z = rys$Sorting, z.cex.range = c(0.5,2), col = "blue", pch = 20)

# O Shepard precisa tirar o gravel: 
x
y<-percent[2:4] 
y
rysgran.ternary (y, method = "shepard",
                 z = rys$Sorting, z.cex.range = c(0.5,2), col = "blue", pch = 20)

######### com relação a energia Hidrodinamica
rysgran.ternary (x = percent[2:4], method = "pejrup") # estuário
rysgran.ternary (x = percent[2:4], method = "flemming")


#--
# Outros gráficos:

# Bivariated plot da media com o grau de seleção
rysgran.plot ("mean" , "sort" , data = dad, method="folk", pch=19)

#adicionando kurtosis
rysgran.plot ("mean" , "sort" , data=dad, method="folk", pch = 21,
              col = "black", z=rys$Kurtosis, z.cex.range=c(1,5),bg="blue")
#adicionando legenda:
legend.bubbles ("bottomleft", z=rys$Kurtosis , nleg=3, pch=21, col="black", 
                z.cex.range=c(1,5), x.intersp=1.3, y.intersp=1.6, digits=1, title="Kurtosis")

# PROCURAR ENTENDER A COLORAÇÃO COMO CLASSIFICÁ-LA DE ACORDO COM QUAL [ENV] #add: z=rys$Skewness para mudar tamanho das bubbles
# env 3, de acordo com SUGUIO:
env3<- rep(c("praia1", "praia2"),each=24)
env3<- factor(env3,levels= c("praia1", "praia2"))

rysgran.plot ("mean" , "skew" , data=dad, method="folk", pch = 19, 
              col = c("black")[env3], show.labels = T, cex.labels = 0.8,
              z=rys$Sorting, z.cex.range=c(1,3))

rysgran.plot ("mean" , "skew" , data=dad, method="folk", pch = 19, 
              col = c("black")[env3], show.labels = F, cex.labels = 0.8,
              z=rys$Sorting, z.cex.range=c(1,3))

#legenda env3
R="praia1 (>)"
f="praia2 (>)"
legend ("topleft", legend = R)
legend("bottomleft", legend =f )
legend.bubbles ("topright", z=rys$Sorting , nleg=3, pch=21, col="black", 
                z.cex.range=c(1,3), x.intersp=1.3, y.intersp=1.3, digits=1, title="Sorting")


