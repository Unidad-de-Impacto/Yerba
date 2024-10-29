/*******************************************************************************
									   Yerba
			  Ministerio de Desregulación y Transformación del Estado
					 Secretaria de Simplificación del Estado
********************************************************************************/
clear all
set more off

else if  "`c(username)'" == "Usuario" {
	global main "direccion"
	}

*Crear carpetas de "input" (donde estará la base de datos) y "output" (para las tablas/gráficos):
global input "$main/input"
global output "$main/output"

cd "$main"

import excel "$input/yerba_precio_est.xlsx", firstrow clear
drop mes

*===========================*
*		ESTIMACIONES
*===========================*
*Se generaron dos variables clave para el análisis: una que captura la tendencia temporal durante el periodo estudiado, y otra que identifica los meses afectados por la modificación del DNU. Los meses tratados son aquellos posteriores a la fecha de la modificación/derogación, por lo que se consideran como tratados las observaciones luego de diciembre de 2023. Además, para minimizar el ruido en los datos, restringimos el análisis a un periodo cercano a la intervención, limitándolo a las variaciones observadas entre marzo de 2022 y la actualidad.
sort Periodo
gen treat=0
replace treat=1 if Periodo>mdy(12,1,2023)
gen t=_n

*Agregamos un control más a la estimación que se define como la interacción entre la variable de tratamiento y la tendencia:
gen treat_t=treat*t

*A continuación, se presentan las estimaciones utilizando un modelo "Before and After" con tendencia temporal. En todas las especificaciones, la variable de interés es la interacción entre el tratamiento y la tendencia temporal, es decir, Tratamiento*Tendencia. El coeficiente asociado a esta variable capturará el cambio en la tendencia tras la modificación implementada a partir del DNU, permitiendo determinar si la intervención generó en promedio una disminución o un aumento en el precio de la Yerba Mate a precios constantes.
*De acuerdo con los resultados, el coeficiente de la interacción es negativo y significativo, lo que sugiere que, tras la intervención, el precio del kilo de yerba a precios constantes comenzó a disminuir a lo largo del tiempo.
 
label variable t "Tendencia"
label variable treat "Tratamiento"
label variable treat_ "Tratamiento*Tendencia"
label variable yerba_real "Precio del kilo de yerba a precios constantes"

reg yerba_real treat t treat_t
outreg2 using "$output/yerba_tabla.doc", nocons dec(2) label replace nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

*Sin estacionalidad:
reg residuos treat t treat_t

*Realizmaos otras estimaciones para chequear que los resultados son consistentes:
extrdate month mes = Periodo 
extrdate month year = Periodo

reg yerba_real treat t treat_t i.mes

reg yerba_real treat t treat_t, robust

reg yerba_real treat t treat_t i.mes, robust

*Permutaciones de inferencia (Heß, 2017): dado que el número de observaciones es relativamente pequeño, realizamos un análisis de inferencia aleatoria mediante permutaciones basadas en simulaciones de Monte Carlo a través del comando ritest.
cap ssc install ritest

ritest treat (_b[treat]/_se[treat]), reps(1000) seed(123) kdensityplot:reg yerba_real treat t treat_t


ritest treat (_b[treat]/_se[treat]), reps(1000) seed(123) kdensityplot: reg residuos treat t treat_t

*Además, reportaremos los errores estándar consistentes con heterocedasticidad y autocorrelación de Newey y West (1994).
*El número óptimo de rezagos es el numero entero inferior de floor[4(30/100)^{2/9}]=3:
tsset t
unique t

newey yerba_real treat t treat_t, lag(3)
outreg2 using "$output/yerba_newey_tabla.doc", dec(2) label replace nonotes addnote("Errores estandar consistentes con heterocedasticidad y autocorrelacion de Newey y West entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")
newey yerba_real treat t treat_t i.mes, lag(3)

**** Estimaciones 2023-2024 ****
import excel "$input/yerba_precio_est.xlsx", firstrow clear
drop mes
sort Periodo
drop if Periodo<=mdy(12,1,2022)
gen treat=0
replace treat=1 if Periodo>mdy(12,1,2023)
gen t=_n
gen treat_t=treat*t
label variable t "Tendencia"
label variable treat "Tratamiento"
label variable treat_ "Tratamiento*Tendencia"
label variable yerba_real "Precio del kilo de yerba a precios constantes"
extrdate month mes = Periodo 
extrdate month year = Periodo

reg yerba_real treat t treat_t i.mes
