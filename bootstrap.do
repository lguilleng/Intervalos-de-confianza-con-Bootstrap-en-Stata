
/* Estimación de Intervalos de Confianza con Bootstrap 

   Para la estimación de intervalos de confianza para casi cualquier
   estimador, se ha propuesto la técnica de bootstrap que, además permite
   considerar el efecto de imputación de datos en el cálculo de
   errores estánda.
   
   Para el desarrollo de los ejemplos de utiliza el archivos de datos
   sumaria-2021.dta, el cual contienen un conjunto de variables agregadas
   de a Encuesta Nacional de Hogares 2021, ejecutada por el
   Instituto Nacional de Estadítica e Informática (INEI) Perú. */

   * Abrimos el archivo de datos
     use sumaria-2021, clear

   /* Estimación del error estándar e intervalo de confianza de la mediana
      para el gasto total bruto anual de los hogares (gashog2d) */
	  
	  bootstrap r(p50), reps(250): summarize gashog2d, detail

   /* Estimación del error estándar de la razón entre la media aritmética
      y la mediana del gashog2d */
	  
	  bootstrap razon=(r(mean)/r(p50)), reps(250): summarize gashog2d, detail
 
   /* Las anteriores estimaciones suponen muestreo aleatorio simple,
      si consideramos el diseño complejo de la muestra, la estimación
	  del error estándar de la mediana y su intervalo de confianza es: */
 
      bootstrap r(p50), reps(250) cluster(conglome) strata(estrato) /*
	  */ force: summarize gashog2d [aw=factor07], detail

   /* Cálculo del error estándar en presencia de datos imputados por
      media aritmética */
 
	  /* Simulamos una variable con valores missing para gashog2d, con base
	     en una variable aleatoria de valores 0 y 1, donde el número 0
		 aparece aproximadamente el 2% de las veces. */

		 gen aleatorio = rbinomial(1,0.98)

         * Creamos una variable con valores perdidos en GASHOG2D
           gen gashog2d_missing=gashog2d*aleatorio
           replace gashog2d_missing=. if gashog2d_missing==0

      /* Creamos una variable con valores imputados por la MEDIA
         para GASHOG2D_MISSING */

		 g gashog2d_imputada=gashog2d_missing
         summarize gashog2d_imputada [aw=factor07]
         replace gashog2d_imputada=r(mean) if gashog2d_imputada==.

      /* Calculamos error estándar de la MEDIA de las variables
         gashog2d (se supone completa), gashog2d_imputada (por media)
         y gashog2d_missing (con valores perdidos = sin imputar) */

		 svyset conglome [pw=factor07], strata(estrato)
         svy: mean gashog2d
         svy: mean  gashog2d_imputada
         svy: mean  gashog2d_missing

      /* Calculamos el error estándar de la variable GASHOG2D_MISSING
         con imputación por media */

         g auxiliar=gashog2d_missing!=.
         program imputacion, rclass
           version 15
		   summarize gashog2d_missing [aw=factor07]
           local media = r(mean)
           replace gashog2d_missing=`media' if gashog2d_missing==.
 		   summarize gashog2d_missing [aw=factor07]
		   local media_imputada = r(mean)
		   replace gashog2d_missing=gashog2d_missing*auxiliar
		   replace gashog2d_missing=. if gashog2d_missing==0
		   return scalar media_imputada = `media_imputada'
         end
 
         bootstrap r(media_imputada), sav(media_imputada) reps(250) /*
                   */ cluster(conglome) strata(estrato): imputacion

         use media_imputada, clear
         quietly summarize _bs_1

         di _n as text "Estimación bootstrap con datos imputados" _n _n /* 
         */ "    Media     Err. Estándar           IC(95%)               CV%" /*
         */ _n "{hline 63}" _n %10.2f as result r(mean) "   " %10.2f r(sd) /*
         */ "      " %10.2f r(mean)-1.96*r(sd)  "   "  %10.2f /*
         */ r(mean)+1.96*r(sd) " "  %10.2f r(sd)/r(mean)*100 as text _n /*
		 */ "{hline 63}"
