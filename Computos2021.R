
#########   ###########  ####    ###  |     COMPARACION DE DISENIOS DE MUESTREO COMPLEJO.
########    ##########   ####    ###  |     
###         ###          ####    ###  |     EN ESTE CODIGO USAREMOS LA BASE DE DATOS PROPORCIONADA POR EL INE
###         #######      ###########  |     SOBRE LAS ELECCIONES FEDERALES DEL 2021 PARA LA ELECCION DE DIPUTADOS
###         ######       ###########  |     https://computos2021.ine.mx/base-de-datos
###         ###          ####    ###  |
########    ###          ####    ###  |     LOS CALCULOS SE REALIZARAN EN R Y POSTERIORMENTE SE REALIZARA 
#########   ###          ####    ###  |     UN ESTUDIO EN POWER BI`


# El objetivo del muestreo es estimar la razon de votos, es decir, la proporcion de votos hacia una coalicion
# u otra. Deberemos estimar el total de votos para cada coalicion y estimar el total de votos computados.
# Usaremos tres modelos distintos y determinaremos cual es el mas optimo para realizar este estudio.


#----------------------------------------------------------------------------------------------------------------------#
#                               LIMPIEZA DEL ENTORNO DE TRABAJO Y CARGA DE LIBRERIAS

rm(list = ls(all.names = TRUE))
library(survey)
library(sampling)
library(tidyverse)
library(latex2exp)
library(data.table)
library(hms)
library(ggplot2)
library(patchwork) # Para juntar gráficos

# semilla para replicar los resultados
set.seed(123)

#----------------------------------------------------------------------------------------------------------------------#
#                                               DEFINICION DE FUNCIONES

# Esta funcion elimina multiples objetos si existen
borrar <- function(...) {
  objs <- as.character(substitute(list(...)))[-1]
  objs_exist <- objs[sapply(objs, exists, envir = .GlobalEnv)]
  rm(list = objs_exist, envir = .GlobalEnv)
}

# esta funcion calcula el tiempo empleado en un proceso
tiempo <- function(inicio, final) {
  # Calcular la diferencia de tiempo en segundos
  tiempo_transcurrido <- difftime(final, inicio, units = "secs")
  
  # Convertir la diferencia a formato hms
  tiempo_hms <- as_hms(as.numeric(tiempo_transcurrido))
  
  # Convertir a formato hh:mm:ss.sss
  tiempo_formateado <- sprintf("%02d:%02d:%02d", 
                               hour(tiempo_hms), 
                               minute(tiempo_hms), 
                               second(tiempo_hms))
  
  # Imprimir el resultado
  cat("Tiempo empleado:", tiempo_formateado, "\n")
}


#----------------------------------------------------------------------------------------------------------------------#
#                                           CARGA DE LA BASE DE DATOS

# cargamos los datos y eliminamos informacion que no necesitamos
datos = fread("diputaciones.csv", drop = c("CLAVE_ACTA", "SECCION", "TIPO_CASILLA", "EXT_CONTIGUA", "NUM_ACTA_IMPRESO",
                                           "OBSERVACIONES", "MECANISMOS_TRASLADO", "FECHA_HORA"),
              encoding = "Latin-1", skip = 6)

# definimos una columna de ID para cada distrito
datos[, ID_DIST := paste0(ID_ESTADO, "-", ID_DISTRITO)]

# obtendremos los votos que obtuvo cada coalicion en cada casilla y limpiamos el total de votos calculados
datos = datos %>% mutate(VPM = PAN + PRI + PRD + `PAN-PRI-PRD` + `PAN-PRI` + `PAN-PRD` + `PRI-PRD`,
                         SHH = MORENA + PT + PVEM + `PVEM-PT` + `PVEM-MORENA` + `PT-MORENA` + `PVEM-PT-MORENA`,
                         TOTAL_VOTOS_CALCULADOS = TOTAL_VOTOS_CALCULADOS - `VOTOS NULOS`,
                         OTROS = TOTAL_VOTOS_CALCULADOS - VPM - SHH) %>%
                  select(-c(`VOTOS NULOS`))
# modificaremos algunos valores de la columna NOMBRE_ESTADO para poder graficar correctamente en Power BI
datos[NOMBRE_ESTADO == "CIUDAD DE MÉXICO", NOMBRE_ESTADO := "CDMX"]
datos[NOMBRE_ESTADO == "MORELOS", NOMBRE_ESTADO := "ESTADO DE MORELOS"]
datos[NOMBRE_ESTADO == "MÉXICO", NOMBRE_ESTADO := "ESTADO DE MEXICO"]
datos[NOMBRE_ESTADO == "MICHOACÁN", NOMBRE_ESTADO := "MICHOACAN"]
datos[NOMBRE_ESTADO == "NUEVO LEÓN", NOMBRE_ESTADO := "NUEVO LEON"]
datos[NOMBRE_ESTADO == "QUERÉTARO", NOMBRE_ESTADO := "QUERETARO"]
datos[NOMBRE_ESTADO == "SAN LUIS POTOSÍ", NOMBRE_ESTADO := "SAN LUIS POTOSI"]
datos[NOMBRE_ESTADO == "YUCATÁN", NOMBRE_ESTADO := "YUCATAN"]

# NOTA: Tener cuidado con los acentos, al descargar el archivo podrian cambiar a caracteres no reconocidos

# creamos una tabla que contenga el correspondiente Estado y Diputacion de cada ID
Distritos = datos %>% select(ID_DIST, ID_ESTADO, ID_DISTRITO, NOMBRE_ESTADO, NOMBRE_DISTRITO) %>%
  # agregamos una columna de pais que nos ayudara mas adelante en Power BI
  mutate(Pais = "MEXICO") %>% unique()

# limpiamos la base
datos = datos %>% select(ID_DIST, CLAVE_CASILLA, CASILLA, LISTA_NOMINAL_CASILLA, VPM, SHH,
                         OTROS, TOTAL_VOTOS_CALCULADOS) %>%
                  # agregamos un ID a cada casilla
                  mutate(ID = 1:nrow(datos))

# Para calcular el ECM, calcularemos los valores reales
total_VPM <- sum(datos$VPM)
total_SHH <- sum(datos$SHH)
total_Votos <- sum(datos$TOTAL_VOTOS_CALCULADOS)

r_pob_vpm <- total_VPM/total_Votos
r_pob_shh <- total_SHH/total_Votos


#----------------------------------------------------------------------------------------------------------------------#
#                                           PRIMER MODELO: MAS BIETAPICO

# El primer modelo sera un muestreo aleatorio simple bietapico, es decir, seleccionaremos 30 distritos de los 300
# y posteriormente seleccionaremos 20 casillas de cada distrito seleccionado en la primera etapa

# difinimos N_I como el tamanio de la primera muestra
(N_I <- length(unique(datos$ID_DIST)))

# obtenemos el total de casillas en cada distrito
dip_upmI = datos %>% group_by(ID_DIST) %>% summarise(N_i=n())

# verificamos que no perdimos informacion
(sum(dip_upmI$N_i)) == nrow(datos)

# definimos el tamanio total de la poblacion
N = nrow(datos)
# muestra de la primera etapa
n_I = 30
# muestra de la segunda etapa
n_iI = 20

# calculamos la probabilidad de inclusion de la segunda etapa para las casillas de cada distrito asumiendo MAS
dip_upmI = dip_upmI %>% mutate(n_iI = n_iI, pi.ki = n_iI/N_i)

# definimos una funcion para obtener una muestra bajo este modelo
modelo1 = function(N_I, n_I, n_iI, return_s = F){
  # ---- PRIMERA ETAPA
  s_Ia = sample(1:N_I, n_I, FALSE)
  # tomamos los distritos elegidos
  s_I = dip_upmI[s_Ia,]
  
  # ---- SEGUNDA ETAPA
  
  # seleccionamos todas las casillas de los distritos seleccionados
  Pob.s_I = datos %>% filter(ID_DIST %in% s_I$ID_DIST)
  # agregamos las probabilidades de inclusion de la segunda etapa
  Pob.s_I = merge(Pob.s_I, s_I[, c("ID_DIST", "pi.ki")], by = "ID_DIST")
  
  # dividimos la poblacion en distritos
  Pob.s_Idiv <- split(Pob.s_I, list(Pob.s_I$ID_DIST))
  # Aqui seleccionamos las 20 casillas por distrito
  samplesSi <- lapply(Pob.s_Idiv, function(x) sample(x$ID, n_iI, FALSE))
  IndexS <- unlist(samplesSi)
  # Finalmente, la muestra
  s = Pob.s_I %>% filter(ID %in% IndexS)
  
  # Definimos las probabilidades de inclusion de primer orden como la probabilidad
  # de haber seleccionado el distrito electoral y la probabilidad de seleccion de
  # la segunda
  s$pik = (n_I/N_I)*(s$pi.ki)
  
  # Calculamos el factor de expansion
  s$wk = 1/s$pik
  
  # ---- ESTIMADORES
  
  stiest_VPM = s %>% group_by(ID_DIST) %>% summarise(estVPM=sum(VPM/pi.ki))
  stiest_SHH = s %>% group_by(ID_DIST) %>% summarise(estSHH=sum(SHH/pi.ki))
  stiest_TVC = s %>% group_by(ID_DIST) %>%
    summarise(estTCV=sum(TOTAL_VOTOS_CALCULADOS/pi.ki))
  
  # ec. 153
  test_votos_I <- sum(stiest_TVC$estTCV)*(1/(n_I/N_I))
  test_vpm_I <- sum(stiest_VPM$estVPM)*(1/(n_I/N_I))
  test_shh_I <- sum(stiest_SHH$estSHH)*(1/(n_I/N_I))
  # Para obtener el estimador de razon solo debemos dividir la estimacion
  # de ambos totales
  R_est_vpm_I <- test_vpm_I/test_votos_I
  R_est_shh_I <- test_shh_I/test_votos_I
  
  # agregamos los resultados a un df
  resultados <- data.frame(VPM = R_est_vpm_I,
                           SHH = R_est_shh_I)
  
  # definimos si queremos que la funcion devuelva la muestra o no
  if (return_s == F){
    return(resultados)
  } else {
    return(list(resultados,s))
  }
}


# obtenemos una primera muestra
muestra1 = modelo1(N_I, n_I, n_iI, T)

# observamos los resultados
muestra1[1]
# observamos la muestra considerada
muestra1[2]

# guardamos la primera muestra para mostrarla en Power BI
fwrite(x = data.frame(muestra1[2]), file = "Muestras\\S_Modelo1.csv")
borrar(muestra1)

#--------------------------------------------------------------

# Ahora, repetiremos el proceso 5,000 veces y calcularemos el Error Cuadratico Medio (ECM) a partir de 
# los resultados
B = 0
resultados1 = data.frame()

# tiempo de inicio del proceso
inicio = Sys.time()
while(B < 5000){
  temp_resultados = modelo1(N_I, n_I, n_iI, F)
  resultados1 = rbind(resultados1, temp_resultados)
  B = B + 1
}
final = Sys.time()
tiempo(inicio, final)
borrar(temp_resultados, inicio, final, B)

# calculamos el ECM
ECM_I_vpm <- round(mean((resultados1$VPM-r_pob_vpm)^2), 6)
ECM_I_shh <- round(mean((resultados1$SHH-r_pob_shh)^2), 6)

#--------------------------------------------------------------

# Histograma para VPM
h1 <- ggplot(resultados1, aes(x = VPM)) +
  geom_histogram(bins = 75, fill = "#87CEFA", color = "#104E8B") +
  geom_vline(xintercept = r_pob_vpm, color = "black", linetype = "dashed") +
  labs(
    title = "Estimador razón de votos a la coalición VPM. \n Modelo MAS bietápico.",
    x = TeX("$\\widehat{R}_{z\\pi}$"),
    y = "Frecuencia"
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("ECM =", ECM_I_vpm), 
           hjust = 1.1, vjust = 1.5, size = 3, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

# Histograma para SHH
h2 <- ggplot(resultados1, aes(x = SHH)) +
  geom_histogram(bins = 75, fill = "#FF6A6A", color = "#722F37") +
  geom_vline(xintercept = r_pob_shh, color = "black", linetype = "dashed") +
  labs(
    title = "Estimador razón de votos a la coalición SHH. \n Modelo MAS bietápico.",
    x = TeX("$\\widehat{R}_{z\\pi}$"),
    y = "Frecuencia"
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("ECM =", ECM_I_shh), 
           hjust = 1.1, vjust = 1.5, size = 3, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

# Mostramos los ambos graficos
graf_modelo1 <- h1 + h2 + plot_layout(ncol = 2) # Gráficos lado a lado
graf_modelo1

#--------------------------------------------------------------

# Podemos calcular la varianza poblacional a partir de formulas teoricas
# y compararla con la obtenida via simulaciones
# definimos las variables de linealizacion
soporte_varianza = datos %>% mutate(vk_vpm = (1/total_Votos)*(VPM - (r_pob_vpm*TOTAL_VOTOS_CALCULADOS)),
                              vk_shh = (1/total_Votos)*(SHH - (r_pob_shh*TOTAL_VOTOS_CALCULADOS)))
# agreguemos ahora N_i y n_i
soporte_varianza = merge(soporte_varianza, dip_upmI[,1:3], by = "ID_DIST")
# Varianza poblacional del muestreo bietapico, con m.a.s en I y m.a.s en II
Uti = soporte_varianza %>% group_by(ID_DIST) %>% 
                  summarise(ti_vpm = sum(vk_vpm), Vi_vpm = unique(N_i)^2/unique(n_iI) * (1 - unique(n_iI)/unique(N_i)) 
                            * var(vk_vpm),
                            ti_shh = sum(vk_shh), Vi_shh = unique(N_i)^2/unique(n_iI) * (1 - unique(n_iI)/unique(N_i)) 
                            * var(vk_shh)) %>% 
                  ungroup()

Uti$Vi_vpm[is.na(Uti$Vi_vpm)] = 0
Uti$Vi_shh[is.na(Uti$Vi_shh)] = 0

# calculamos la varianza para VPM
PSU_vpm = (N_I^2/n_I*(1-n_I/N_I)*var(Uti$ti_vpm))
SSU_vpm = (sum(Uti$Vi_vpm)*N_I/n_I)
var_vpm_I = PSU_vpm + SSU_vpm

# calculamos la varianza para SHH
PSU_shh = (N_I^2/n_I*(1-n_I/N_I)*var(Uti$ti_shh))
SSU_shh = (sum(Uti$Vi_shh)*N_I/n_I)
var_shh_I = PSU_shh + SSU_shh

# comparamos los resultados teoricos con los resultados obtenidos via simulacion
varianzasI = data.frame(Coalicion = c("ECM", "VAR Teorica"),
                       VPM =  c(ECM_I_vpm, var_vpm_I),
                       SHH =  c(ECM_I_shh, var_shh_I))
varianzasI

borrar(N, n_I, N_I, n_iI, PSU_shh, PSU_vpm, SSU_shh, SSU_vpm, var_shh_I, var_vpm_I, h1, h2, Uti,
       ECM_I_shh, ECM_I_vpm)



#----------------------------------------------------------------------------------------------------------------------#
#        SEGUNDO MODELO: ESTRATIFICADO A PARTIR DE LAS 32 ENTIDADES FEDERATIVAS DEL PAIS, MAS DE 19 CASILLAS

# definimos el tamanio de la muestra para cada Entidad
n_iII = 19
# obtenemos el numero de casillas por Entidad
dip_upmII = datos %>% left_join(Distritos[, c("ID_DIST", "NOMBRE_ESTADO")], by = "ID_DIST") %>%
  group_by(NOMBRE_ESTADO) %>% summarise(N_i=n())
# agregramos las probabilidades de inclusion
dip_upmII$pik <- n_iII/dip_upmII$N_i
# agregamos el numero de casillas por estado a los datos
dip_upmII = merge(merge(datos, Distritos[, c("ID_DIST", "NOMBRE_ESTADO")], by = "ID_DIST")
                  , dip_upmII, by = "NOMBRE_ESTADO")
# separamos la informacion por Entidad
dip_splitII <- split(dip_upmII, list(dip_upmII$NOMBRE_ESTADO))

# Creamos una funcion que obtenga la muestra
modelo2 = function(n_iII, return_s = F){
  # ---- SELECCION DE LA MUESTRA
  # seleccionamos las casillas de cada estado
  samplesSi_II <- lapply(dip_splitII, function(x) sample(x$ID, n_iII, FALSE))
  # seleccionamos la muestra
  IndexSII <- unlist(samplesSi_II)
  sII=dip_upmII %>% filter(ID %in% IndexSII)
  
  # ---- ESTIMACIONES
  stiest_VPM_II = sII %>% group_by(NOMBRE_ESTADO) %>% summarise(estVPM=sum(VPM/pik))
  stiest_SHH_II = sII %>% group_by(NOMBRE_ESTADO) %>% summarise(estSHH=sum(SHH/pik))
  stiest_TVC_II = sII %>% group_by(NOMBRE_ESTADO) %>%
    summarise(estTCV=sum(TOTAL_VOTOS_CALCULADOS/pik))
  
  test_vpm_II <- sum(stiest_VPM_II$estVPM)
  test_shh_II <- sum(stiest_SHH_II$estSHH)
  test_votos_II <- sum(stiest_TVC_II$estTCV)
  
  R_est_vpm_II <- test_vpm_II/test_votos_II
  R_est_shh_II <- test_shh_II/test_votos_II
  
  # agregamos los resultados a un df
  resultados <- data.frame(VPM = R_est_vpm_II,
                           SHH = R_est_shh_II)
  
  # definimos si queremos que la funcion devuelva la muestra o no
  if (return_s == F){
    return(resultados)
  } else {
    return(list(resultados,sII))
  }
}

# obtenemos una primera muestra
muestra2 = modelo2(n_iII, T)

# observamos los resultados
muestra2[1]
# observamos la muestra considerada
muestra2[2]

# guardamos la primera muestra para mostrarla en Power BI
fwrite(x = data.frame(muestra2[2]), file = "Muestras\\S_Modelo2.csv")
borrar(muestra2)

#--------------------------------------------------------------

# Ahora, repetiremos el proceso 5,000 veces y calcularemos el Error Cuadratico Medio (ECM) a partir de 
# los resultados
B = 0
resultados2 = data.frame()

# tiempo de inicio del proceso
inicio = Sys.time()
while(B < 5000){
  temp_resultados = modelo2(n_iII, F)
  resultados2 = rbind(resultados2, temp_resultados)
  B = B + 1
}
final = Sys.time()
tiempo(inicio, final)
borrar(temp_resultados, inicio, final, B)

# Calculamos el ECM
ECM_II_vpm <- round(mean((resultados2$VPM-r_pob_vpm)^2), 6)
ECM_II_shh <- round(mean((resultados2$SHH-r_pob_shh)^2), 6)

#--------------------------------------------------------------

# Histograma para VPM
h1 <- ggplot(resultados2, aes(x = VPM)) +
  geom_histogram(bins = 75, fill = "#87CEFA", color = "#104E8B") +
  geom_vline(xintercept = r_pob_vpm, color = "black", linetype = "dashed") +
  labs(
    title = "Estimador razón de votos a la coalición VPM. \n Modelo Estratificado.",
    x = TeX("$\\widehat{R}_{z\\pi}$"),
    y = "Frecuencia"
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("ECM =", ECM_II_vpm), 
           hjust = 1.1, vjust = 1.5, size = 3, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

# Histograma para SHH
h2 <- ggplot(resultados2, aes(x = SHH)) +
  geom_histogram(bins = 75, fill = "#FF6A6A", color = "#722F37") +
  geom_vline(xintercept = r_pob_shh, color = "black", linetype = "dashed") +
  labs(
    title = "Estimador razón de votos a la coalición SHH. \n Modelo Estratificado.",
    x = TeX("$\\widehat{R}_{z\\pi}$"),
    y = "Frecuencia"
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("ECM =", ECM_II_shh), 
           hjust = 1.1, vjust = 1.5, size = 3, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

# Mostramos los ambos graficos
graf_modelo2 <- h1 + h2 + plot_layout(ncol = 2) # Gráficos lado a lado
graf_modelo2

#--------------------------------------------------------------

# Tambien podemos calcular la varianza teorica sobre este modelo

# agrupamos la informacion por Entidad
Uti2 = merge(soporte_varianza, Distritos[,c("ID_DIST", "NOMBRE_ESTADO")]) %>%
          group_by(NOMBRE_ESTADO) %>% summarise(Svh2_vpm = var(vk_vpm), Svh2_shh = var(vk_shh))
# agregamos el numero de casillas por estado
Uti2 = merge(Uti2, unique(dip_upmII[,c("NOMBRE_ESTADO", "N_i")]), by = "NOMBRE_ESTADO")
# agregamos el numero de casillas seleccionadas por estado
Uti2$n_i = n_iII

# Calculamos la varianza para cada coalicion
Uti2 = Uti2 %>% mutate(Vh_vpm = (N_i^2/n_i)*(1-(n_i/N_i))*Svh2_vpm,
                       Vh_shh = (N_i^2/n_i)*(1-(n_i/N_i))*Svh2_shh)
var_vpm_II = sum(Uti2$Vh_vpm)
var_shh_II = sum(Uti2$Vh_shh)

# comparamos los resultados teoricos con los resultados obtenidos via simulacion
varianzasII = data.frame(Coalicion = c("ECM", "VAR Teorica"),
                        VPM =  c(ECM_II_vpm, var_vpm_II),
                        SHH =  c(ECM_II_shh, var_shh_II))
varianzasII


borrar(n_iII, var_shh_II, var_vpm_II, h1, h2, dip_upmII, dip_splitII, Uti2, ECM_II_shh, ECM_II_vpm, soporte_varianza)


#----------------------------------------------------------------------------------------------------------------------#
#             TERCER MODELO: ESTRATIFICACION CONSIDERANDO LAS 5 CIRCUNSCRIPCIONES ELECTORALES.
#                            EN CADA CIRCUNSCRIPCION SE SELECCIONARAN 8 DISTRITOS CON MAS
#                            EN CADA DISTRITO SELECCIONADO SELECCIONAREMOS 15 CASILLAS USANDO MAS

# primero debemos agregar las circunscripciones correpondientes a cada Entidad
circunscripciones = list(
  "1" = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", "DURANGO", "JALISCO", "NAYARIT", "SINALOA", "SONORA"),
  "2" = c("AGUASCALIENTES", "COAHUILA", "GUANAJUATO", "NUEVO LEON", "QUERETARO",
          "SAN LUIS POTOSI", "TAMAULIPAS", "ZACATECAS"),
  "3" = c("CAMPECHE", "CHIAPAS", "OAXACA", "QUINTANA ROO", "TABASCO", "VERACRUZ", "YUCATAN"),
  "4" = c("CDMX", "GUERRERO", "ESTADO DE MORELOS", "PUEBLA", "TLAXCALA"),
  "5" = c("COLIMA", "HIDALGO", "ESTADO DE MEXICO", "MICHOACAN")
)

circ_df <- do.call(rbind, lapply(names(circunscripciones), function(circ) {
  data.frame(Circ = circ, NOMBRE_ESTADO = circunscripciones[[circ]], stringsAsFactors = FALSE)
}))

# agregamos esta informacion a los datos
Distritos = Distritos %>% left_join(circ_df, by = "NOMBRE_ESTADO")
datos = datos %>% left_join(unique(Distritos[,c("ID_DIST", "Circ", "NOMBRE_ESTADO")]), by = "ID_DIST")
borrar(circ_df, circunscripciones)

# contamos el numero de distritos electorales por circunscripcion
dip_upmIII = datos %>% group_by(Circ) %>%
  summarise(N_i = n_distinct(ID_DIST))

# definimos el tamanio de la muestra de la primera etapa
n_III = 8
dip_upmIII$pik = n_III/dip_upmIII$N_i

# agregamos esta informacion a los datos
dip_upmIII = merge(datos, dip_upmIII, by = "Circ")

# agregamos el numero de casillas por cada distrito electoral, disponible en dip_upmI
dip_upmIII = merge(dip_upmIII, select(dip_upmI, ID_DIST, N_ii = N_i), by = "ID_DIST")

# ahora agregamos la probabilidad de inclusion de segundo orden, es decir, seleccionar 15 casillas del distrito seleccionado
n_iIII = 15
dip_upmIII$pi.ki = n_iIII/dip_upmIII$N_ii

# dividimos a la poblacion por ciscunscripcion
dip_splitIII <- split(dip_upmIII, list(dip_upmIII$Circ))


# definimos el proceso para seleccionar una muestra
modelo3 = function(n_III, n_iIII, return_s = F){
  # ---- PRIMERA ETAPA
  # seleccionamos 8 distritos por circunscripcion
  sampleSi_III = lapply(dip_splitIII, function(x) sample(unique(x$ID_DIST), n_III, replace = FALSE))
  IndexSIII = unlist(sampleSi_III)
  
  # ---- SEGUNDA ETAPA
  # ahora seleccionamos 15 casillas de cada distrito
  s1 = dip_upmIII %>% filter(ID_DIST %in% IndexSIII)
  s1_split = split(s1, list(s1$ID_DIST))
  
  sampleSi_III_2 = lapply(s1_split, function(x) sample(unique(x$ID), n_iIII, replace = FALSE))
  IndexSIII_2 = unlist(sampleSi_III_2)
  
  # obtenemos la muestra
  s2 = s1 %>% filter(ID %in% IndexSIII_2)
  
  # ---- ESTIMACIONES
  stiest_VPM_III = s2 %>% group_by(ID_DIST) %>% summarise(estVPM = sum(VPM/pi.ki))
  stiest_SHH_III = s2 %>% group_by(ID_DIST) %>% summarise(estSHH = sum(SHH/pi.ki))
  stiest_TVC_III = s2 %>% group_by(ID_DIST) %>%
    summarise(estTCV = sum(TOTAL_VOTOS_CALCULADOS/pi.ki))
  
  # recuperamos las circunscripciones y las probabilidades de inclusion de primer orden
  stiest_III = merge(merge(stiest_VPM_III, stiest_SHH_III, by = "ID_DIST"), stiest_TVC_III, by = "ID_DIST")
  stiest_III = stiest_III %>% left_join(unique(dip_upmIII[, c("ID_DIST", "Circ", "pik")]), by = "ID_DIST")
  
  # estimadores de razon
  total_VPM_III <- stiest_III %>% group_by(Circ) %>%
    summarise(estVPM=sum(estVPM))
  total_SHH_III <- stiest_III %>% group_by(Circ) %>%
    summarise(estSHH=sum(estSHH))
  total_TCV_III <- stiest_III %>% group_by(Circ) %>%
    summarise(estTVC=sum(estTCV))
  
  # dividimos entre las probabilidades de inclusion de primer orden !!!!
  total_VPM_III = total_VPM_III %>% left_join(unique(stiest_III[, c("Circ", "pik")]), by = "Circ") %>% 
    mutate(estVPM = estVPM/pik)
  total_SHH_III = total_SHH_III %>% left_join(unique(stiest_III[, c("Circ", "pik")]), by = "Circ") %>% 
    mutate(estSHH = estSHH/pik)
  total_TVC_III = total_TCV_III %>% left_join(unique(stiest_III[, c("Circ", "pik")]), by = "Circ") %>% 
    mutate(estTVC = estTVC/pik)
  
  # sumamos cada circunscripcion y obtenemos el estimador de razon
  R_est_vpm_III = sum(total_VPM_III$estVPM)/sum(total_TVC_III$estTVC)
  R_est_shh_III = sum(total_SHH_III$estSHH)/sum(total_TVC_III$estTVC)
  
  # agregamos los resultados a un df
  resultados <- data.frame(VPM = R_est_vpm_III,
                           SHH = R_est_shh_III)
  
  
  
  # definimos si queremos que la funcion devuelva la muestra o no
  if (return_s == F){
    return(resultados)
  } else {
    return(list(resultados,s2))
  }
}

muestra3 = modelo3(n_III, n_iIII, T)
# observamos los resultados
muestra3[1]
# observamos la muestra considerada
muestra3[2]

# guardamos la primera muestra para mostrarla en Power BI
fwrite(x = data.frame(muestra3[2]), file = "Muestras\\S_Modelo3.csv")
borrar(muestra3)

#--------------------------------------------------------------

# Ahora, repetiremos el proceso 5,000 veces y calcularemos el Error Cuadratico Medio (ECM) a partir de 
# los resultados
B = 0
resultados3 = data.frame()

# tiempo de inicio del proceso
inicio = Sys.time()
while(B < 5000){
  temp_resultados = modelo3(n_III, n_iIII, F)
  resultados3 = rbind(resultados3, temp_resultados)
  B = B + 1
}
final = Sys.time()
tiempo(inicio, final)
borrar(temp_resultados, inicio, final, B)

# Calculamos el ECM
ECM_III_vpm <- round(mean((resultados3$VPM-r_pob_vpm)^2), 6)
ECM_III_shh <- round(mean((resultados3$SHH-r_pob_shh)^2), 6)

#--------------------------------------------------------------

# Histograma para VPM
h1 <- ggplot(resultados3, aes(x = VPM)) +
  geom_histogram(bins = 75, fill = "#87CEFA", color = "#104E8B") +
  geom_vline(xintercept = r_pob_vpm, color = "black", linetype = "dashed") +
  labs(
    title = "Estimador razón de votos a la coalición VPM. \n Estratificado por Circunscripción.",
    x = TeX("$\\widehat{R}_{z\\pi}$"),
    y = "Frecuencia"
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("ECM =", ECM_III_vpm), 
           hjust = 1.1, vjust = 1.5, size = 3, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

# Histograma para SHH
h2 <- ggplot(resultados3, aes(x = SHH)) +
  geom_histogram(bins = 75, fill = "#FF6A6A", color = "#722F37") +
  geom_vline(xintercept = r_pob_shh, color = "black", linetype = "dashed") +
  labs(
    title = "Estimador razón de votos a la coalición VPM. \n Estratificado por Circunscripción.",
    x = TeX("$\\widehat{R}_{z\\pi}$"),
    y = "Frecuencia"
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("ECM =", ECM_III_shh), 
           hjust = 1.1, vjust = 1.5, size = 3, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

# Mostramos los ambos graficos
graf_modelo3 <- h1 + h2 + plot_layout(ncol = 2) # Gráficos lado a lado
graf_modelo3

#--------------------------------------------------------------

# Este modelo no permite calcular una varianza teorica, por lo que consideraremos
# unicamente el ECM obtenido con las simulaciones

varianzasIII = data.frame(Coalicion = "ECM",
                         VPM =  c(ECM_III_vpm),
                         SHH =  c(ECM_III_shh))
varianzasIII

borrar(dip_splitIII, dip_upmI, dip_upmIII, h1, h2, ECM_III_shh, ECM_III_vpm, n_iIII, n_III)

#----------------------------------------------------------------------------------------------------------------------#
#                                             CONCLUSIONES

# Recordemos los ECM obtenidos con cada modelo

varianzasI; varianzasII; varianzasIII

all_graf = graf_modelo1 / graf_modelo2 / graf_modelo3
allgraf


# En base en los resultados obtenidos, el segundo modelo es el mejor. Es decir, seleccionar casillas de cada uno de 
# las 32 Entidades. Podemos intuir que esto se debe a una mejor representacion de todos los votantes.
# Profundizaremos mas en este tema en Poweri BI.


# guardamos nuestros datos.
fwrite(datos[,1:9], "Muestras\\Diputaciones_clean.csv")
fwrite(Distritos, "Muestras\\Distritos.csv")
fwrite(resultados1, "Muestras\\Resultados1.csv")
fwrite(resultados2, "Muestras\\Resultados2.csv")
fwrite(resultados3, "Muestras\\Resultados3.csv")

