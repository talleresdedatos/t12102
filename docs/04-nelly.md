
# Participación de las mujeres jóvenes en el 2019 Y 2020 {#participacion-mujeres}

*Nelly Reyes Mendoza*




## Introducción

El siguiente reporte pretende revisar de acuerdo a la data de la ENAHO - Encuesta Nacional de Hogares 2019 y 2020, como viene dándose la participación de las mujeres jóvenes a nivel nacional, teniendo en cuenta que el 2020 iniciamos en el país un contexto de emergencia sanitaria debido a la pandemia del Covid -19. Realizaremos algunas preguntas a partir de la ENAHO - Encuesta Nacional de Hogares que nos permitan describir como viene dándose la participación de las mujeres jóvenes:

1. ¿Cuántas mujeres jóvenes pertenecen, participan o están inscritas en algún grupo de organización, asociación y/o programa social?
2. ¿Cuántos de los miembros del hogar que participan o están inscritas en algún grupo, organización, asociación y/o programa social son mujeres jóvenes?
3. ¿Cuántas mujeres jóvenes que participan o están inscritas por tipo de organización, asociación y/o programas sociales?
4. ¿Cuántas mujeres jóvenes ocupan un cargo en la organización, asociación, y/o programa social?
5. ¿Cómo accedieron las mujeres jóvenes que participan en la organización, asociación, y/o programa social?
6. ¿Cuáles son los motivos por la cual una mujer joven no ha participado en las elecciones de las organizaciones, asociaciones, y/o programas sociales?

## Paquetes usados

Usaré dos paquetes para la lectura de reportes en R que son tidyverse y haven que nos permitirá leer archivos en SPSS.


```r
library(tidyverse)
library(haven)
```

## Conjuntos de datos usados

Usaremos 06 base de datos de la ENAHO, tres de la ENAHO 2019 y tres base de datos 2020. Estas bases forman parte de dos módulos. El primer módulo es referido a los miembros del hogar (módulo 2) , este módulo cuenta con 20 preguntas, de las cuales vamos a tomar solo 3 que son las referidas a: relación de parentesco con el/la jefe del núcleo familiar, sexo y edad. Y el segundo módulo es sobre participación ciudadana (módulo 84) , este módulo cuenta con 6 preguntas, en este módulo si usaremos las seis preguntas.Además al finalizar el cargado de la data colocamos el as_factor() que nos permite una lectura adecuada de archivos SPSS.

DATA ENAHO 2019:


```r
enaho_2019_miembros <-  read_sav("data/Enaho01-2019-200.sav") %>% 
  as_factor()

enaho_2019_participacion_1 <-  read_sav("data/Enaho01-2019-800A.sav") %>% 
  as_factor()

enaho_2019_participacion_2<-read_sav("data/Enaho01-2019-800B.sav") %>%
  as_factor()
```


```r
miembros_hogar_2019 <- enaho_2019_miembros %>% filter(P204 == "Si")
```


DATA ENAHO 2020:


```r
enaho_2020_miembros<-read_sav("data/Enaho01-2020-200.sav") %>% 
  as_factor()

enaho_2020_participacion_1<-read_sav("data/Enaho01-2020-800A.sav") %>% 
  as_factor()

enaho_2020_participacion_2<-read_sav("data/Enaho01-2020-800B.sav") %>% 
  as_factor()
```


```r
miembros_hogar_2020 <- enaho_2020_miembros %>% filter(P204 == "Si")
```

## Limpieza de base de datos

 1. Para la primera pregunta de ¿Cuántas mujeres jóvenes pertenecen, participan o están inscritas en algún grupo de organización, asociación y/o programa social? seleccionaremos las variables de la base de datos enaho_2019_miembros que están referidas a sexo y edad solo desde los 15 a 29 años y de la base enaho_2019_ participación_1 que está referido a si algún miembro del hogar pertenece o participa en algún grupo, organización o asociación, lo mismo haremos para la base de datos de enaho 2020. 
 
- enaho_2019_miembros: 
    - P207
    - P208A
- enaho_2019_participación_1: 
    - P801_19
    
- enaho_2020_miembros: 
    - P207
    - P208A
- enaho_2020_participación_1: 
    - P801_19







Según la ficha de técnica de la ENAHO, La metodología de estimación para procesar los datos de la Encuesta Nacional de Hogares, involucra el uso de un peso o factor de expansión para cada registro que será  multiplicado por todos los datos que conforman el registro correspondiente (FICHA TÉCNICA ENCUESTA NACIONAL DE HOGARES SOBRE CONDICIONES DE VIDA Y POBREZA - 2019).Para poder conocer los resultados adecuados a las preguntas, incorporaremos el factor de expansión en la elaboración de los códigos. Esto lo aplicaremos previo al bloque de resultado de todas las preguntas y que esta precisada como FACPOB07. 

Con factor de expansión:


```r
resultado_1_2019 <- miembros_hogar_2019 %>%
  filter(P207 == "Mujer", P208A >= 15, P208A <= 29) %>%
  select(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO, P207, P208A, FACPOB07) %>%
  left_join(enaho_2019_participacion_2) %>% 
  select(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO, P207, P208A, P804, FACPOB07) %>%
  mutate(participa = if_else(is.na(P804), "No participa", "Sí participa")) %>% 
  group_by(AÑO, participa) %>%
  summarise(recuento = sum(FACPOB07)) %>%
  mutate(porcentaje = recuento / sum(recuento) * 100) %>% 
  ungroup()
```



```r
resultado_1_2020 <- enaho_2020_miembros %>%
  filter(P204 == "Si") %>% 
  filter(P207 == "Mujer", P208A >= 15, P208A <= 29) %>%
  select(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO, P207, P208A, FACPOB07) %>%
  left_join(enaho_2020_participacion_2) %>% 
  select(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO, P207, P208A, P804, FACPOB07) %>%
  mutate(participa = if_else(is.na(P804), "No participa", "Sí participa")) %>% 
  group_by(AÑO, participa) %>%
  summarise(recuento = sum(FACPOB07)) %>%
  mutate(porcentaje = recuento / sum(recuento) * 100) %>% 
  ungroup()
```

2. Respecto a la pregunta dos de ¿Cuántos de los miembros del hogar que participan o están inscritas en algún grupo, organización, asociación y/o programa social son mujeres jóvenes?, se realizó el ejercicio de procesarla; sin embargo los resultados no mostraron algún resultado relevante a diferencia la primera pregunta. Es importante precisar que el universo a analizar era a partir de los miembros del hogar, y conocer el porcentaje de mujeres jóvenes que participan en su hogar.

3. En el caso de la tercera pregunta ¿Cuántas mujeres jóvenes que participan o están inscritas por tipo de organización, asociación y/o programas sociales? seleccionaremos 19 variables referidas a preguntas sobre el tipo de organización como: clubes y asociaciones deportivas,Agrupación o partido político, clubes culturales, asociación vecinal / junta vecinal, ronda campesina, asociación de regantes,asociación profesional,asociación de trabajadores o sindicato, club de madres, asociación de padres de familia (APAFA), vaso de leche, comedor popular, proceso del presupuesto participativo, comunidad campesina, asociación agropecuaria, participación en la preparación de desayuno y/o almuerzo escolar, otro/s. Esta selección será para ambas base de datos del 2019 y 2020. Luego de ello veremos de acuerdo a los miembros mujeres jóvenes cuantas de ellas vienen participando en los distintos tipos de organización/asociación tanto en el año 2019 y 2020. 

- enaho_2019_miembros/ enaho_2020_miembros 
    - P207
    - P208A
- enaho_2019_participacion_1/enaho_2020_participacion_1
    - P801_1
    - P801_2
    - P801_3
    - P801_4
    - P801_5
    - P801_6
    - P801_7
    - P801_8
    - P801_9
    - P801_10
    - P801_11
    - P801_12
    - P801_13
    - P801_14
    - P801_15
    - P801_16
    - P801_17
    - P801_18
    - P801_20


```r
muj_tipo_org_2019 <- miembros_hogar_2019 %>%
  filter(P207=="Mujer", P208A>=15,P208A<=29) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO,P207, P208A, FACPOB07) %>% 
  left_join(enaho_2019_participacion_2) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO, P207, P208A, P803 , FACPOB07) %>%
  filter(!is.na(P803)) %>% 
  group_by(AÑO, P803) %>% 
  summarise(recuento = sum(FACPOB07)) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  ungroup()
```



```r
muj_tipo_org_2020<- enaho_2020_miembros %>%
  filter(P204 == "Si") %>% 
  filter(P207=="Mujer", P208A>=15,P208A<=29) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO,P207, P208A, FACPOB07) %>% 
  left_join(enaho_2020_participacion_2) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO, P207, P208A, P803 , FACPOB07) %>%
  filter(!is.na(P803)) %>% 
  group_by(AÑO, P803) %>% 
  summarise(recuento = sum(FACPOB07)) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  ungroup()
```


4. Para la cuarta pregunta realizada de ¿Cuántas mujeres jóvenes ocupan un cargo en la organización, asociación, y/o programa social? se selecciona las variables referidas a sexo, edad en la base de miembros, mientas en la siguiente base de datos si participa como miembro activo, miembro no activo, representante/dirigente en ambos años 2019 y 2020. 

- enaho_2019_miembros/enaho_2020_miembros
    - P207
    - P208A
- enaho_2019_participacion_2/ enaho_2020_participacion_2
    - P804


```r
muj_cargos_2019 <-miembros_hogar_2019 %>%
  filter(P207=="Mujer", P208A>=15,P208A<=29) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO,P207, P208A, FACPOB07) %>% 
  left_join(enaho_2019_participacion_2) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO, P207, P208A, P804,FACPOB07) %>%
  filter(!is.na(P804)) %>%  
  group_by(AÑO, P804) %>% 
  summarise(recuento = sum(FACPOB07)) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  ungroup()
```


```r
muj_cargos_2020<- enaho_2020_miembros %>%
  filter(P204 == "Si") %>% 
  filter(P207=="Mujer",P208A>=15,P208A<=29) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO,P207,P208A,FACPOB07)%>% 
  left_join(enaho_2020_participacion_2)%>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO, P207, P208A, P804, FACPOB07) %>%
  filter(!is.na(P804)) %>% 
  group_by(AÑO,P804) %>% 
  summarise(recuento = sum(FACPOB07)) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  ungroup()
```

5. Respecto a  la quinta pregunta de cómo accedieron las mujeres jóvenes que participan en la organización, asociación, y/o programa social, tenemos seis posibles respuestas de acuerdo a la base de datos para hacer el comparativo entre los años 2019 y 2020. Estas son: Fue elegido/a en proceso de elección, por amistad, fue designado o seleccionado, porque pagó, por afiliación y otro. Para ello seleccionamos la variable en la base de datos 2019 y 2020

- enaho_2019_miembros/ enaho_2020_miembros
    - P207
    - P208A
- enaho_2019_participacion_2/enaho_2020_participacion_2
    - P805


```r
muj_acc_2019<- miembros_hogar_2019 %>%
  filter(P207=="Mujer", P208A>=15,P208A<=29) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO,P207, P208A, FACPOB07) %>% 
  left_join(enaho_2019_participacion_2) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO, P207, P208A, P805, FACPOB07) %>%
  filter(!is.na(P805)) %>% 
  group_by(AÑO, P805) %>% 
  summarise(recuento = sum(FACPOB07)) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  ungroup()
```


```r
muj_acc_2020<- enaho_2020_miembros %>%
  filter(P204 == "Si") %>% 
  filter(P207=="Mujer", P208A>=15,P208A<=29) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO,P207, P208A, FACPOB07) %>% 
  left_join(enaho_2020_participacion_2) %>% 
  select(AÑO,CONGLOME,VIVIENDA,HOGAR,CODPERSO, P207, P208A, P805, FACPOB07) %>%
  filter(!is.na(P805)) %>% 
  group_by(AÑO, P805) %>% 
  summarise(recuento = sum(FACPOB07)) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  ungroup()
```


6. En la sexta pregunta realizada respecto a Cuáles son los motivos por la cual una mujer joven no ha participado en las elecciones de las organizaciones, asociaciones, y/o programas sociales, podemos responder que ninguna de la data que se tiene permite responderla ya que se tiene registrada las respuestas referidas al hogar en general y no por miembro, que en este caso nos interesaría respuestas de una mujer joven miembro de una familia. 

## Resultados

### ¿Cuántas mujeres jóvenes pertenecen, participan o están inscritas en algún grupo de organización, asociación y/o programa social? 

Al realizar un cruce de los códigos previos de la data 2019 y 2020 tenemos que en el año 2019 el 89.2% de mujeres jóvenes no participaba y el 10.8% de mujeres participaba o estaba inscrita en alguna organización, asociación y/o programa social. Y el año 2020 tenemos que 95.7% de mujeres jóvenes que no participan y 4.2% de mujeres jóvenes que participan. Esto quiere decir que la cantidad de mujeres jóvenes que venían participando ha disminuido casi en un 6%, hay menos mujeres que participan. 


```r
bind_rows(resultado_1_2019, resultado_1_2020) %>% 
  select(AÑO,participa,porcentaje) %>% 
  pivot_wider(names_from = AÑO, values_from= porcentaje)
```

```{=html}
<template id="28f2a7f4-5891-4adb-ae7d-bf864e3a1e28"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  margin-top: 1.275em;
  margin-bottom: 1.275em;
  border-color: transparent;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
</style><div class="tabwid"><style>.cl-c31f859c{table-layout:auto;width:0%;}.cl-c3176f4c{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c3176f4d{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c3179666{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c3179667{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c317bd76{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c317bd77{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c317bd78{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c317bd79{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-c31f859c'>
```

```{=html}
<thead><tr style="overflow-wrap:break-word;"><td class="cl-c317bd79"><p class="cl-c3179666"><span class="cl-c3176f4c">participa</span></p></td><td class="cl-c317bd78"><p class="cl-c3179667"><span class="cl-c3176f4c">2019</span></p></td><td class="cl-c317bd78"><p class="cl-c3179667"><span class="cl-c3176f4c">2020</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-c317bd77"><p class="cl-c3179666"><span class="cl-c3176f4d">No participa</span></p></td><td class="cl-c317bd76"><p class="cl-c3179667"><span class="cl-c3176f4d">89.22</span></p></td><td class="cl-c317bd76"><p class="cl-c3179667"><span class="cl-c3176f4d">95.70</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c317bd79"><p class="cl-c3179666"><span class="cl-c3176f4d">Sí participa</span></p></td><td class="cl-c317bd78"><p class="cl-c3179667"><span class="cl-c3176f4d">10.78</span></p></td><td class="cl-c317bd78"><p class="cl-c3179667"><span class="cl-c3176f4d">4.30</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="e57324b3-f58b-443e-b052-1c156f1340f6"></div>
<script>
var dest = document.getElementById("e57324b3-f58b-443e-b052-1c156f1340f6");
var template = document.getElementById("28f2a7f4-5891-4adb-ae7d-bf864e3a1e28");
var caption = template.content.querySelector("caption");
if(caption) {
  caption.style.cssText = "display:block;text-align:center;";
  var newcapt = document.createElement("p");
  newcapt.appendChild(caption)
  dest.parentNode.insertBefore(newcapt, dest.previousSibling);
}
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>

```

¿Que ocurrió? ¿A qué se debe? De hecho uno de los factores trascendentales fue el contexto de emergencia sanitaria que empezó en el país el 2020 generó inmovilización y distanciamiento, sin embargo el 2019 casi un 90% de mujeres jóvenes que no estaban involucradas tampoco en algún espacio de participación ciudadana.

### ¿Cuántas mujeres jóvenes que participan o están inscritas por tipo de organización, asociación y/o programas sociales?

Podemos observar el siguiente cuadro donde podemos comparar los porcentajes del año 2019 y 2020. Por ejemplo vemos que en el caso de la participación de mujeres jóvenes en una asociación vecinal/junta vecinal ha incrementado de 7.3% que fue el año 2019 a 11.3% en el 2020. Igual observamos en el caso de la participación de las mujeres jóvenes en una asociación profesional que fue 3.4% en el 2019 e incrementó a 5.8% en el año 2020.  También tenemos que la participación de mujeres jóvenes en la APAFA incrementó de 5.1% en el 2019 a 7.5% en el 2020. 


```r
bind_rows(muj_tipo_org_2019, muj_tipo_org_2020) %>% 
  select(AÑO,P803,porcentaje) %>% 
  pivot_wider(names_from = AÑO, values_from= porcentaje)
```

```{=html}
<template id="747ab602-bbb7-48ce-bf9d-a648efc9b02a"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  margin-top: 1.275em;
  margin-bottom: 1.275em;
  border-color: transparent;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
</style><div class="tabwid"><style>.cl-c335a5d4{table-layout:auto;width:0%;}.cl-c3300084{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c3300085{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c3300086{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c3300087{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c3304ea4{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c3304ea5{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c3304ea6{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c3304ea7{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-c335a5d4'>
```

```{=html}
<thead><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300084">P803</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300084">2019</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300084">2020</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea5"><p class="cl-c3300086"><span class="cl-c3300085">Clubes y asociaciones deportivas</span></p></td><td class="cl-c3304ea4"><p class="cl-c3300087"><span class="cl-c3300085">1.83</span></p></td><td class="cl-c3304ea4"><p class="cl-c3300087"><span class="cl-c3300085">2.38</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Agrupación o partido político</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">0.39</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">0.05</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Clubes culturales (danza, música,etc.)</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">2.52</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">1.60</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Asociación vecinal / Junta vecinal</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">7.38</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">11.34</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Ronda campesina</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">0.73</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">1.18</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Asociación de regantes</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">0.16</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">0.31</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Asociación profesional</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">3.45</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">5.86</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Asociación de trabajadores o sindicato</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">0.33</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">1.61</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Club de madres</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">0.01</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">0.09</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Asociación de padres de familia (APAFA)</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">5.15</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">7.53</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Vaso de leche</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">31.97</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">40.72</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Comedor popular</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">1.22</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">2.39</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Proceso de presupuesto participativo</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">0.01</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085"></span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Comunidad campesina</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">7.18</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">8.23</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Asociación Agropecuaria</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">0.12</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085"></span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Otro/a</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">8.07</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">5.44</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3304ea7"><p class="cl-c3300086"><span class="cl-c3300085">Participación en la preparación de desayuno y/o almuerzo escolar</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">29.47</span></p></td><td class="cl-c3304ea6"><p class="cl-c3300087"><span class="cl-c3300085">11.25</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="8f9b32d0-1481-44bd-983d-e1b8139a12a0"></div>
<script>
var dest = document.getElementById("8f9b32d0-1481-44bd-983d-e1b8139a12a0");
var template = document.getElementById("747ab602-bbb7-48ce-bf9d-a648efc9b02a");
var caption = template.content.querySelector("caption");
if(caption) {
  caption.style.cssText = "display:block;text-align:center;";
  var newcapt = document.createElement("p");
  newcapt.appendChild(caption)
  dest.parentNode.insertBefore(newcapt, dest.previousSibling);
}
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>

```

Por otro lado observamos un incremento importante de la participación de las mujeres jóvenes en el vaso de leche pasando de un 31.9% a un 40.7% entre el año 2019 y 2020 respectivamente. También observamos una disminución importante de la participación de las mujeres jóvenes en la preparación del desayuno/almuerzo escolar pasando del 29.4% al 11.2% del año 2019 y 2020. Respecto a los demás tipos de organizaciones donde vienen participando las mujeres jóvenes hay un incremento o disminución en 1% o en algunos menos del 1% tal como se muestra en el cuadro.


### ¿Cuántas mujeres jóvenes ocupan un cargo en la organización, asociación, y/o programa social?

Podemos observar en el siguiente cuadro codificado. En el el año 2019 el 12.7% de mujeres jóvenes fueron dirigentes/representantes mientras en el 2020 fueron 11.5%, se observa una disminución de un poco más del 1%. En el caso de miembros activos vemos que el 84.3% fueron jóvenes mujeres mientras en el año 2020 se registra que el 83.1% la participación de las mujeres jóvenes.Y para los miembros no activos, donde también se encuentran mujeres pasó del 2.9% al 5.3% en el año 2019 y 2020 respectivamente; esto quiere decir que incrementó los miembros no activos en una organización/asociación de parte de las mujeres jóvenes.  


```r
bind_rows(muj_cargos_2019, muj_cargos_2020) %>% 
  select(AÑO,P804,porcentaje) %>% 
  pivot_wider(names_from = AÑO, values_from= porcentaje)
```

```{=html}
<template id="733c52b4-069e-4f54-8d00-cea9f61b6765"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  margin-top: 1.275em;
  margin-bottom: 1.275em;
  border-color: transparent;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
</style><div class="tabwid"><style>.cl-c34a1834{table-layout:auto;width:0%;}.cl-c3436174{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c3436175{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c3436176{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c3436177{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c3438884{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c3438885{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c3438886{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c3438887{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-c34a1834'>
```

```{=html}
<thead><tr style="overflow-wrap:break-word;"><td class="cl-c3438887"><p class="cl-c3436176"><span class="cl-c3436174">P804</span></p></td><td class="cl-c3438886"><p class="cl-c3436177"><span class="cl-c3436174">2019</span></p></td><td class="cl-c3438886"><p class="cl-c3436177"><span class="cl-c3436174">2020</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-c3438885"><p class="cl-c3436176"><span class="cl-c3436175">Dirigente/representante</span></p></td><td class="cl-c3438884"><p class="cl-c3436177"><span class="cl-c3436175">12.72</span></p></td><td class="cl-c3438884"><p class="cl-c3436177"><span class="cl-c3436175">11.51</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3438887"><p class="cl-c3436176"><span class="cl-c3436175">Miembro activo</span></p></td><td class="cl-c3438886"><p class="cl-c3436177"><span class="cl-c3436175">84.35</span></p></td><td class="cl-c3438886"><p class="cl-c3436177"><span class="cl-c3436175">83.15</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c3438887"><p class="cl-c3436176"><span class="cl-c3436175">Miembro no activo</span></p></td><td class="cl-c3438886"><p class="cl-c3436177"><span class="cl-c3436175">2.93</span></p></td><td class="cl-c3438886"><p class="cl-c3436177"><span class="cl-c3436175">5.34</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="5b74f1ec-55cc-4e08-a0b9-dfe5492a1c16"></div>
<script>
var dest = document.getElementById("5b74f1ec-55cc-4e08-a0b9-dfe5492a1c16");
var template = document.getElementById("733c52b4-069e-4f54-8d00-cea9f61b6765");
var caption = template.content.querySelector("caption");
if(caption) {
  caption.style.cssText = "display:block;text-align:center;";
  var newcapt = document.createElement("p");
  newcapt.appendChild(caption)
  dest.parentNode.insertBefore(newcapt, dest.previousSibling);
}
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>

```

### ¿Cómo accedieron las mujeres jóvenes que participan en la organización, asociación, y/o programa social?

Podemos observar que el 12.1% de mujeres jóvenes fueron elegidas en un proceso electoral en el 2019, y el 11.0% de mujeres jóvenes fueron elegidas en el 2020. También observamos que el 21.5% de mujeres jóvenes fueron designadas/seleccionadas en el 2019 mientras el 12.8% de mujeres jóvenes fueron designadas/seleccionadas en el año 2020. 


```r
bind_rows(muj_acc_2019, muj_acc_2020) %>% 
  select(AÑO,P805,porcentaje) %>% 
  pivot_wider(names_from = AÑO, values_from= porcentaje)
```

```{=html}
<template id="1d81ac0f-bf11-4970-9251-75a47936ee44"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  margin-top: 1.275em;
  margin-bottom: 1.275em;
  border-color: transparent;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
</style><div class="tabwid"><style>.cl-c35d51ec{table-layout:auto;width:0%;}.cl-c357fabc{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c357fabd{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-c357fabe{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c357fabf{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-c35821e0{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c35821e1{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c35821e2{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c35821e3{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-c35d51ec'>
```

```{=html}
<thead><tr style="overflow-wrap:break-word;"><td class="cl-c35821e3"><p class="cl-c357fabe"><span class="cl-c357fabc">P805</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabc">2019</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabc">2020</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-c35821e1"><p class="cl-c357fabe"><span class="cl-c357fabd">Fue elegido en proceso de elección</span></p></td><td class="cl-c35821e0"><p class="cl-c357fabf"><span class="cl-c357fabd">12.15</span></p></td><td class="cl-c35821e0"><p class="cl-c357fabf"><span class="cl-c357fabd">11.06</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c35821e3"><p class="cl-c357fabe"><span class="cl-c357fabd">Por amistad</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabd">1.13</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabd">1.13</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c35821e3"><p class="cl-c357fabe"><span class="cl-c357fabd">Fue designado o seleccionado</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabd">21.59</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabd">12.81</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c35821e3"><p class="cl-c357fabe"><span class="cl-c357fabd">Porque pagó</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabd">0.47</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabd">3.23</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c35821e3"><p class="cl-c357fabe"><span class="cl-c357fabd">Por afiliación</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabd">64.66</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabd">71.73</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-c35821e3"><p class="cl-c357fabe"><span class="cl-c357fabd">Otro</span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabd"></span></p></td><td class="cl-c35821e2"><p class="cl-c357fabf"><span class="cl-c357fabd">0.03</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="6b2b248e-89ea-40e9-b581-256047dcf9d4"></div>
<script>
var dest = document.getElementById("6b2b248e-89ea-40e9-b581-256047dcf9d4");
var template = document.getElementById("1d81ac0f-bf11-4970-9251-75a47936ee44");
var caption = template.content.querySelector("caption");
if(caption) {
  caption.style.cssText = "display:block;text-align:center;";
  var newcapt = document.createElement("p");
  newcapt.appendChild(caption)
  dest.parentNode.insertBefore(newcapt, dest.previousSibling);
}
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>

```

Observamos también un incremento importante por afiliación de parte de las mujeres jóvenes que fue el 64.6% el año 2019 mientras el 71.2% en el año 2020. Además de ello otro incremento que se observa es el pago que realizan las mujeres jóvenes para ser parte de la asociación/organización pasando de un 0.4% al 3.2% en el año 2019 y 2020 respectivamente. 

## Conclusiones

Observamos en general que ha habido una disminución importante respecto a la participación de las mujeres jóvenes entre el año 2019 y 2020, acercándose a una caída del 6% de participación ciudadana. Y que probablemente siga en este tipo de avance.

Incrementó considerable la participación de las mujeres jóvenes en el vaso de leche pasando de 31% a un 40%  en los años 2019 y 2020 respectivamente, y disminuyó su participación en la preparación de desayunos/almuerzos de 29% a 11% entre el 2019 y 2020.

Incrementó la cantidad de mujeres jóvenes como miembros no activos de una organización/asociación de 2% a 5% entre los años 2019 y 2020 respectivamente. Y como miembros activos o dirigentes de la organización disminuyo en 1% el año 2020 a diferencia del 2019.

Hubo un incremento de parte de la participación de mujeres jóvenes por afiliación a una una organización/asociación que fueron 64% el 2019 y 71% el 2020 de mujeres jóvenes. Mientras disminuyó su ingreso por designación o selección pasando de de un 21% a un 12% de mujeres durante el 2019 y 2020 respectivamente. 

De las seis preguntas elaboradas sobre la participación de mujeres jóvenes respecto a la revisión de la base de datos de la ENAHO 2019 y 2020, se consideró responder cuatro de ellas, tomando en cuenta que para una de ellas era imposible responder ya que la base de datos participación_2 (que la nombre arbitrariamente) no contiene la información que nos permita resolver la pregunta.  



