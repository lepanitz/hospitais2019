# para ler o excell é necessário ter o pacote readxl instalado
# media>mean
# mediana>median
# agrupar>aggregate
# 
install.packages("tinytex")

library(dplyr)


#para estatisticas descritivas
install.packages("descr")
install.packages("")

#chama a biblioteca que importa o excell
library("readxl")
# le a tabela excell para a variáveis DB_2019
BD_2019 <- read_excel("BD_2019.xlsx")
#chama a variávels BD_2019 para a memória
attach(BD_2019)



#alguma função agrupada por outra variável
txocupa <- aggregate(C_RZ_OCUPACAO ~ PORTE_SUS,
         FUN=mean)
txocupa


#verificando dados de leitos mensais em um dataframe
leitosmes <- LEITOS_SUS/12 # cria uma variael com leitos / 12
media <- data.frame(aggregate(leitosmes ~ PORTE_SUS,
                              FUN=median)) 

#GRAFICOS
library(ggplot2)

##FREQUENCIA
#-distruição de hospitais por número de leitos
ggplot(BD_2019, aes(LEITOS_SUS)) +geom_histogram (binwidth=50)+
  scale_fill_gradient("Count", low = "#DCDCDC", high = "#7C7C7C")+
  stat_function(fun=dnorm, color="red", args=list(mean=mean(LEITOS_SUS), sd=sd(LEITOS_SUS)))
#-distruição das taxas de ocupação por hopspital
ggplot(BD_2019, aes(C_RZ_OCUPACAO)) +geom_histogram (binwidth=0.005)
#-distruição das internações por condições sensiveis à atenção primária
ggplot(BD_2019, aes(C_RZ_ICSAP)) +geom_histogram (binwidth=0.005, data = NULL)+
  scale_fill_gradient("Count", low = "#DCDCDC", high = "#7C7C7C")+
  stat_function(fun=dnorm, color="red", args=list(mean=mean(C_RZ_ICSAP, sd=sd(C_RZ_ICSAP))))

##DENSIDADE
#-aproveitamento de leitos
ggplot(BD_2019,aes(x = C_AIH_LEITO_MES))+geom_density()+
  geom_density(fill = "lightblue", color = "blue")
ggplot(BD_2019,aes(x = C_AIH_LEITO_MES, fill=GRUPO))+geom_density()+
  geom_density(alpha = .5)
ggplot(BD_2019,aes(x = C_AIH_LEITO_MES, fill=PORTE_SUS))+geom_density()+
  geom_density(alpha = .5)
#-taxa de ocupação
ggplot(BD_2019,aes(x = C_RZ_OCUPACAO))+geom_density()+
  geom_density(fill = "lightblue", color = "blue")
ggplot(BD_2019,aes(x = C_RZ_OCUPACAO, fill=GRUPO))+geom_density()+
  geom_density(alpha = .5)
ggplot(BD_2019,aes(x = C_RZ_OCUPACAO, fill=PORTE_SUS))+geom_density()+
  geom_density(alpha = .5)
ggplot(BD_2019,aes(x = C_RZ_OCUPACAO, fill=FXPOP2019))+geom_density()+
  geom_density(alpha = .5)
#-internações por condições sensiveis à atenção primária
ggplot(BD_2019,aes(x = C_RZ_ICSAP))+geom_density()+
  geom_density(fill = "lightblue", color = "blue")
ggplot(BD_2019,aes(x = C_RZ_ICSAP, fill=PORTE_SUS))+geom_density()+
  geom_density(alpha = .5)
#-media de permanancia
ggplot(BD_2019,aes(x = C_MEDIA_PERM))+geom_density()+
  geom_density(fill = "lightblue", color = "blue")
ggplot(BD_2019,aes(x = C_MEDIA_PERM, fill=PORTE_SUS))+geom_density()+
  geom_density(alpha = .5)

##DISPERSSÃO (dos hospitais)
#-taxas de ocupação por número de leitos
ggplot(BD_2019,aes(LEITOS_SUS,C_RZ_OCUPACAO))+geom_point(aes(colour=PORTE_SUS))+labs(x="Número de Leitos", y="Taxa de Ocupação", title="Dispersão de Hospitais por Número de Leitos e Taxa de Ocupação")
#-taxas de ocupação por valor das internações
ggplot(BD_2019,aes(TOTAL_VALOR,C_RZ_OCUPACAO))+geom_point(aes(colour=PORTE_SUS))
#-taxas de ocupação por total de internações
ggplot(BD_2019,aes(TOTAL_AIH,C_RZ_OCUPACAO))+geom_point(aes(colour=PORTE_SUS))

##BOXPLOT
#-taxas de ocupação por porte de leitos
ggplot(BD_2019, aes(x=PORTE_SUS, y=C_RZ_OCUPACAO, fill=PORTE_SUS)) + 
  geom_boxplot() 
#-internações APS  por porte de leitos
ggplot(BD_2019, aes(x=PORTE_SUS, y=C_RZ_ICSAP, fill=PORTE_SUS)) + 
  geom_boxplot() 
#-internações de alta complexidade  por porte de leitos
ggplot(BD_2019, aes(x=PORTE_SUS, y=C_RZ_AC, fill=PORTE_SUS)) + 
  geom_boxplot() 
#-internações de outros municipos  por porte de leitos
ggplot(BD_2019, aes(x=PORTE_SUS, y=C_RZ_MUNREF, fill=PORTE_SUS)) + 
  geom_boxplot() 
ggplot(BD_2019, aes(x=PORTE_SUS, y=C_RZ_TRANSF, fill=PORTE_SUS)) + 
  geom_boxplot() 
ggplot(BD_2019, aes(x=PORTE_SUS, y=C_MEDIA_PERM, fill=PORTE_SUS)) + 
  geom_boxplot() 

#teste para leitos por população


pop <- POP2019/1000
leitopop <- LEITOS_SUS/pop
ggplot(BD_2019, aes(x=UF, y=leitopop, fill=UF)) + 
  geom_boxplot() 

#teste mix publico - privado (% de leitos dedicados ao SUS)

perc_leitos <- (LEITOS_SUS/LEITOS_TOT)*100
ggplot(BD_2019,aes(x = perc_leitos))+geom_density()+
  geom_density(fill = "lightblue", color = "blue")

#professor waldecy:tamanho de mnicipio, 2 regionlizações por estado e por regiao.

# TAXA DE OCUPAÇÃO POR UF
ggplot(BD_2019, aes(x=UF, y=C_RZ_OCUPACAO, fill=UF)) + 
  geom_boxplot() 
