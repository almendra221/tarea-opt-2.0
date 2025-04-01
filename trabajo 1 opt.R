
install.packages("pacman")
pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2,# Para la mayoría de los gráficos
               car) 

#Base de datos
base_enusc2023 <- read_sav("input/base-usuario-20-enusc-2023bf436364553a489fab8e92a1ef14a520.sav")


View(base_enusc2023)
names(base_enusc2023)

##variables a utilizar

P_INSEG_OSCURO_1 # Durante los últimos doce meses ¿qué tan seguro/a se siente en las siguientes situaciones? Caminando solo/a por su barrio cuando ya está oscuro
P_INSEG_DIA_1 #Durante los últimos doce meses ¿qué tan seguro/a se siente en las siguientes situaciones? Caminando solo/a por su barrio durante el día
rph_sexo #sexo


base_filtrada <- base_enusc2023 %>% select( P_INSEG_OSCURO_1,
                                            P_INSEG_DIA_1, 
                                            rph_sexo) 

# Comprobar
names(base_filtrada)

# Comprobar
names(base_filtrada)

#clasificar los na
base_filtrada<- base_filtrada %>% set_na(., na = c(85,88,99))

base_filtrada<-na.omit(base_filtrada)
dim(base_filtrada)

#tabla de frecuencia
frq(base_filtrada$P_INSEG_OSCURO_1)

# recodificacion persepcion inseg. de noche
base_filtrada$P_INSEG_OSCURO_1 <- car::recode(base_filtrada$P_INSEG_OSCURO_1, "c(1,2)=1; c(3,4)=2")

base_filtrada$P_INSEG_OSCURO_1 <- factor(base_filtrada$P_INSEG_OSCURO_1,
                             labels = c("Inseguro/a", "Seguro/a"),
                             levels = c(1, 2))

frq(base_filtrada$P_INSEG_OSCURO_1)

base_filtrada<- rename(base_filtrada,"P.inseguridad_cuando_esta_oscuro"=P_INSEG_OSCURO_1)




#tabla de frecuencia
frq(base_filtrada$P_INSEG_DIA_1)

##RECODIFICACION PERCEPCION DE INSEG. EN EL DIA
base_filtrada$P_INSEG_DIA_1 <- car::recode(base_filtrada$P_INSEG_DIA_1, "c(1,2)=1; c(3,4)=2")

base_filtrada$P_INSEG_DIA_1 <- factor(base_filtrada$P_INSEG_DIA_1,
                                         labels = c("Inseguro/a", "Seguro/a"),
                                         levels = c(1, 2))
frq(base_filtrada$P_INSEG_DIA_1)

base_filtrada<- rename(base_filtrada,"P.inseguridad_cuando_es_dia"=P_INSEG_DIA_1)

##SEXO
#tabla de frecuencia

frq(base_filtrada$rph_sexo)

base_filtrada$rph_sexo<- car::recode(base_filtrada$rph_sexo, "1=0;2=1")

base_filtrada$rph_sexo <- factor(base_filtrada$rph_sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))
frq(base_filtrada$rph_sexo)

##generar tabla de descriptivos
library(sjPlot)
sjt.xtab(base_filtrada$P.inseguridad_cuando_esta_oscuro, base_filtrada$rph_sexo, encoding = "UTF-8")

#generar grafico relacion entre variables
base_filtrada %>% ggplot(aes(x =P.inseguridad_cuando_es_dia )) + 
  geom_bar() +
  xlab("persepcion de inseguridad en el dia") +
  ylab("Cantidad")+
  facet_wrap(~rph_sexo)

#generar grafico univariado
grafico1 <-base_filtrada %>% ggplot(aes(x = P.inseguridad_cuando_esta_oscuro)) + 
  geom_bar(fill="blue")+
  labs(title = "persepcion de inseguridad en el barrio cuando esta oscuro",
       x = "percepcion de inseguridad en el barrio cuando esta oscuro",
       y = "Frecuencia")+
theme_bw()
grafico1



grafico2 <-base_filtrada %>% ggplot(aes(x = P.inseguridad_cuando_es_dia)) + 
  geom_bar(fill="yellow")+
  labs(title = "persepcion de inseguridad en el barrio cuando es de dia",
       x = "percepcion de inseguridad en el barrio cuando es de dia",
       y = "Frecuencia")+
  theme_bw()
grafico2










