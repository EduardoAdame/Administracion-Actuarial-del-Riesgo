
# Fijamos la ruta de trabajo
setwd(****)


# Cargamos librerías #ctrl+shift+c
# install.packages("tidyverse")


library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# readr (Para leer las bases de datos)
# Leemos los archivos de base

base <- read***("datos/base_final.csv",col_names = TRUE)


# Con read_delim
base <- read****("datos/base_final.csv",",")

****(base)


# Podemos especificar el tipo de columna

****(base)

# Carga la base de datos asegurandote que la columna de SUMA_ASEG y MONTO_MO sean doubles
# Además que la columna de SINIESTRO sea un factor
base <- read_csv("datos/base_final.csv",col_names = TRUE,
                 **** = cols(POLIZA=col_character(),
                                  ESTADO=col_character(),
                                  MUNICIPIO=col_character(),
                                  SUMA_ASEG=col_double(),
                                  MONTO_MO=col_double(),
                                  SINIESTRO=col_character()))


# Estado y municipio
estado <- read_delim("datos/estado.txt",col_names = TRUE,****="|",
                     **** = cols(ESTADO=col_character()))

municipio <- read_delim("datos/municipio.txt",col_names = T,delim="|",
                        **** = cols(MUNICIPIO=col_character()))


# Como son nuestras columnas
glimpse(base)
glimpse(estado)
glimpse(municipio)


# dplyr

# El uso del %>%... Explicación...
base *** head(10) 
base *** head(10) *** View() 


# Realizar una consulta tal que la nueva tabla sólo contenga la columna POLIZA, COBERTURA y SA

base %>% 
  *****(POLIZA,COBERTURA,SUMA_ASEG) %>% 
  ******()


# De esa misma consulta ordena por coberturas
base %>% 
  *****(POLIZA,COBERTURA,SUMA_ASEG) %>% 
  *****(COBERTURA)


# Seleccione todas las columnas menos AGENTE y FMA_PAGO

base %>% 
  select(*****)

base %>% 
  select(*******)



# Seleccione las variables POLIZA,COBERTURA,SUMA_ASEG y todas entre SINIESTRO y MONTO_MO

base %>% 
  select(POLIZA,COBERTURA,SUMA_ASEG,*******) 



# El nombre de la columna SUMA_ASEG es muy extenso modificalo por SA
# Asi igual el nombre de MONTO_MO modificalo por MONTO, y PRIMA

base <- base %>% 
  ****(SA=SUMA_ASEG,
         MONTO=MONTO_MO,
         PRIMA=COB_PRIMA)



# Crea una columna llamada SEV en la que se construirá el valor de severidad
base <- base %>% 
  *****(SEV=MONTO/SA)



# Cambia los valores NA de SINIESTRO por "0" y los MONTOS POR 0
base <- base %>% 
  mutate(SINIESTRO = *****(SINIESTRO, ***)) %>% 
  mutate(MONTO=******(MONTO, ***))


# ---------------------------------------------------------------------------------------------
# Una consulta muuuuuy usual...  --------------------------------------
# ---------------------------------------------------------------------------------------------



# Muestra el monto total de Prima, SA y Monto Reclamado en pesos mexicanos por cada cobertura

# 1
q1 <- base %>% 
  ****(COBERTURA,SA,PRIMA,MONTO,T_CAMBIO,T_CAMBIO_SIN,MONEDA)

# 2
q2 <- q1 %>% 
  ****(SA= ****(MONEDA=="DL",SA*T_CAMBIO,SA),
         PRIMA= ****(MONEDA=="DL",PRIMA*T_CAMBIO,PRIMA),
         MONTO=****(MONEDA=="DL",MONTO*T_CAMBIO_SIN,MONTO))

# 3
q3 <- q2 %>% 
  ****(COBERTURA) %>% 
  ****(SA=sum(SA),
            MONTO=sum(MONTO, na.rm = TRUE),
            PRIMA=sum(PRIMA)) %>% 
  ****()

# De un jalón:

qfinal <- base %>% 
  ****(COBERTURA,SA,PRIMA,MONTO,T_CAMBIO,T_CAMBIO_SIN,MONEDA) %>% #q1
  mutate(SA=****(MONEDA=="DL",SA*T_CAMBIO,SA),
         PRIMA=****(MONEDA=="DL",PRIMA*T_CAMBIO,PRIMA),
         MONTO=****(MONEDA=="DL",MONTO*T_CAMBIO_SIN,MONTO)) %>% #q2
  ****(COBERTURA) %>% 
  ****(SA=sum(SA),
            MONTO=sum(MONTO,na.rm = TRUE),
            PRIMA=sum(PRIMA)) %>% 
  ****() #q3 %>%  


qfinal


# Qué coberturas no tienen monto de siniestro?
qfinal %>% 
  ****(MONTO==0) %>% 
  ****(COBERTURA)



# Vamos a trabajar con la cobertura C-15

base.15 <- base %>% 
  filter(COBERTURA=="C-15") %>% 
  mutate(SINIESTRO=as.numeric(SINIESTRO)) 


****( base.15 , aes(x = SINIESTRO)) +
  ****(fill = "steelblue") +
  ****(title = "Distribución de la frecuencia de siniestros",
       x = "Siniestro") +
  ****() +
  ****(plot.title = element_text(face = "bold"))

****(base.15,aes(x = SINIESTRO)) +
  ****(geom = "step", color = "steelblue", size = 1) +
  ****(title = "Función de distribución empírica",
       x = "Siniestro",
       y = "CDF") +
  ****() +
  ****(plot.title = element_text(face = "bold"))
