
# Participación de las mújeres jóvenes en el 2019 Y 2020 {#participacion-mujeres}

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



```r
resultado_1_2019 <- miembros_hogar_2019 %>%
  filter(P207 == "Mujer", P208A >= 15, P208A <= 29) %>%
  select(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO, P207, P208A) %>%
  left_join(enaho_2019_participacion_2) %>% 
  select(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO, P207, P208A, P804) %>%
  mutate(participa = if_else(is.na(P804), "No participa", "Sí participa")) %>% 
  group_by(AÑO, participa) %>%
  summarise(recuento = n()) %>%
  mutate(porcentaje = recuento / sum(recuento) * 100) %>% 
  ungroup()
```



```r
resultado_1_2020 <- enaho_2020_miembros %>%
  filter(P204 == "Si") %>% 
  filter(P207 == "Mujer", P208A >= 15, P208A <= 29) %>%
  select(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO, P207, P208A) %>%
  left_join(enaho_2020_participacion_2) %>% 
  select(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO, P207, P208A, P804) %>%
  mutate(participa = if_else(is.na(P804), "No participa", "Sí participa")) %>% 
  group_by(AÑO, participa) %>%
  summarise(recuento = n()) %>%
  mutate(porcentaje = recuento / sum(recuento) * 100) %>% 
  ungroup()
```

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


3.En el caso de la tercera pregunta ¿Cuántas mujeres jóvenes que participan o están inscritas por tipo de organización, asociación y/o programas sociales? seleccionaremos 19 variables referidas a preguntas sobre el tipo de organización como: clubes y asociaciones deportivas,Agrupación o partido político, clubes culturales, asociación vecinal / junta vecinal, ronda campesina, asociación de regantes,asociación profesional,asociación de trabajadores o sindicato, club de madres, asociación de padres de familia (APAFA), vaso de leche, comedor popular, proceso del presupuesto participativo, comunidad campesina, asociación agropecuaria, participación en la preparación de desayuno y/o almuerzo escolar, otro/s. Esta selección será para ambas base de datos del 2019 y 2020. Luego de ello veremos de acuerdo a los miembros mujeres jóvenes cuantas de ellas vienen participando en los distintos tipos de organización/asociación tanto en el año 2019 y 2020. 

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
<template id="c0f4603d-7f2d-4ca6-a465-44c6c3069fda"><style>
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
</style><div class="tabwid"><style>.cl-a761c0aa{table-layout:auto;width:0%;}.cl-a75c1b5a{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a75c1b5b{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a75c1b5c{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a75c1b5d{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a75c426a{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a75c426b{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a75c426c{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a75c426d{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-a761c0aa'>
```

```{=html}
<thead><tr style="overflow-wrap:break-word;"><td class="cl-a75c426d"><p class="cl-a75c1b5c"><span class="cl-a75c1b5a">participa</span></p></td><td class="cl-a75c426c"><p class="cl-a75c1b5d"><span class="cl-a75c1b5a">2019</span></p></td><td class="cl-a75c426c"><p class="cl-a75c1b5d"><span class="cl-a75c1b5a">2020</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-a75c426b"><p class="cl-a75c1b5c"><span class="cl-a75c1b5b">No participa</span></p></td><td class="cl-a75c426a"><p class="cl-a75c1b5d"><span class="cl-a75c1b5b">89.22</span></p></td><td class="cl-a75c426a"><p class="cl-a75c1b5d"><span class="cl-a75c1b5b">95.70</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a75c426d"><p class="cl-a75c1b5c"><span class="cl-a75c1b5b">Sí participa</span></p></td><td class="cl-a75c426c"><p class="cl-a75c1b5d"><span class="cl-a75c1b5b">10.78</span></p></td><td class="cl-a75c426c"><p class="cl-a75c1b5d"><span class="cl-a75c1b5b">4.30</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="d75051e7-bebb-49b8-a409-e7ca23a982e4"></div>
<script>
var dest = document.getElementById("d75051e7-bebb-49b8-a409-e7ca23a982e4");
var template = document.getElementById("c0f4603d-7f2d-4ca6-a465-44c6c3069fda");
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
<template id="348bdc13-1ac6-4c27-9faa-f994db445742"><style>
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
</style><div class="tabwid"><style>.cl-a785ea7a{table-layout:auto;width:0%;}.cl-a780452a{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a780452b{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a780452c{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a780452d{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a7809340{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a7809341{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a7809342{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a7809343{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-a785ea7a'>
```

```{=html}
<thead><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452a">P803</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452a">2019</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452a">2020</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-a7809341"><p class="cl-a780452c"><span class="cl-a780452b">Clubes y asociaciones deportivas</span></p></td><td class="cl-a7809340"><p class="cl-a780452d"><span class="cl-a780452b">1.83</span></p></td><td class="cl-a7809340"><p class="cl-a780452d"><span class="cl-a780452b">2.38</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Agrupación o partido político</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">0.39</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">0.05</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Clubes culturales (danza, música,etc.)</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">2.52</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">1.60</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Asociación vecinal / Junta vecinal</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">7.38</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">11.34</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Ronda campesina</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">0.73</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">1.18</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Asociación de regantes</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">0.16</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">0.31</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Asociación profesional</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">3.45</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">5.86</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Asociación de trabajadores o sindicato</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">0.33</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">1.61</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Club de madres</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">0.01</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">0.09</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Asociación de padres de familia (APAFA)</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">5.15</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">7.53</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Vaso de leche</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">31.97</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">40.72</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Comedor popular</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">1.22</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">2.39</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Proceso de presupuesto participativo</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">0.01</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b"></span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Comunidad campesina</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">7.18</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">8.23</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Asociación Agropecuaria</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">0.12</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b"></span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Otro/a</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">8.07</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">5.44</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7809343"><p class="cl-a780452c"><span class="cl-a780452b">Participación en la preparación de desayuno y/o almuerzo escolar</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">29.47</span></p></td><td class="cl-a7809342"><p class="cl-a780452d"><span class="cl-a780452b">11.25</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="35c0219d-8362-42bc-8855-2db47eed9cdf"></div>
<script>
var dest = document.getElementById("35c0219d-8362-42bc-8855-2db47eed9cdf");
var template = document.getElementById("348bdc13-1ac6-4c27-9faa-f994db445742");
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
<template id="53a78667-b122-4dee-a7ba-4bcb661fbd31"><style>
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
</style><div class="tabwid"><style>.cl-a799727a{table-layout:auto;width:0%;}.cl-a7924680{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a7924681{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a7924682{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a7924683{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a7926d90{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a7926d91{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a7926d92{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a7926d93{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-a799727a'>
```

```{=html}
<thead><tr style="overflow-wrap:break-word;"><td class="cl-a7926d93"><p class="cl-a7924682"><span class="cl-a7924680">P804</span></p></td><td class="cl-a7926d92"><p class="cl-a7924683"><span class="cl-a7924680">2019</span></p></td><td class="cl-a7926d92"><p class="cl-a7924683"><span class="cl-a7924680">2020</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-a7926d91"><p class="cl-a7924682"><span class="cl-a7924681">Dirigente/representante</span></p></td><td class="cl-a7926d90"><p class="cl-a7924683"><span class="cl-a7924681">12.72</span></p></td><td class="cl-a7926d90"><p class="cl-a7924683"><span class="cl-a7924681">11.51</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7926d93"><p class="cl-a7924682"><span class="cl-a7924681">Miembro activo</span></p></td><td class="cl-a7926d92"><p class="cl-a7924683"><span class="cl-a7924681">84.35</span></p></td><td class="cl-a7926d92"><p class="cl-a7924683"><span class="cl-a7924681">83.15</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7926d93"><p class="cl-a7924682"><span class="cl-a7924681">Miembro no activo</span></p></td><td class="cl-a7926d92"><p class="cl-a7924683"><span class="cl-a7924681">2.93</span></p></td><td class="cl-a7926d92"><p class="cl-a7924683"><span class="cl-a7924681">5.34</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="14ef13c7-9819-4d9e-95ad-a564c651b29b"></div>
<script>
var dest = document.getElementById("14ef13c7-9819-4d9e-95ad-a564c651b29b");
var template = document.getElementById("53a78667-b122-4dee-a7ba-4bcb661fbd31");
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
<template id="f887cbbf-e39d-4fbc-873d-a235ad91c360"><style>
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
</style><div class="tabwid"><style>.cl-a7ab9aea{table-layout:auto;width:0%;}.cl-a7a5a770{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a7a5a771{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a7a5a772{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a7a5a773{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a7a5ce8a{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a7a5ce8b{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a7a5ce8c{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a7a5ce8d{background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 1pt solid rgba(102, 102, 102, 1.00);border-right: 1pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-a7ab9aea'>
```

```{=html}
<thead><tr style="overflow-wrap:break-word;"><td class="cl-a7a5ce8d"><p class="cl-a7a5a772"><span class="cl-a7a5a770">P805</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a770">2019</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a770">2020</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-a7a5ce8b"><p class="cl-a7a5a772"><span class="cl-a7a5a771">Fue elegido en proceso de elección</span></p></td><td class="cl-a7a5ce8a"><p class="cl-a7a5a773"><span class="cl-a7a5a771">12.15</span></p></td><td class="cl-a7a5ce8a"><p class="cl-a7a5a773"><span class="cl-a7a5a771">11.06</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7a5ce8d"><p class="cl-a7a5a772"><span class="cl-a7a5a771">Por amistad</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a771">1.13</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a771">1.13</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7a5ce8d"><p class="cl-a7a5a772"><span class="cl-a7a5a771">Fue designado o seleccionado</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a771">21.59</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a771">12.81</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7a5ce8d"><p class="cl-a7a5a772"><span class="cl-a7a5a771">Porque pagó</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a771">0.47</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a771">3.23</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7a5ce8d"><p class="cl-a7a5a772"><span class="cl-a7a5a771">Por afiliación</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a771">64.66</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a771">71.73</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a7a5ce8d"><p class="cl-a7a5a772"><span class="cl-a7a5a771">Otro</span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a771"></span></p></td><td class="cl-a7a5ce8c"><p class="cl-a7a5a773"><span class="cl-a7a5a771">0.03</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="a55ce52e-64ee-416d-bd07-80b428b03abf"></div>
<script>
var dest = document.getElementById("a55ce52e-64ee-416d-bd07-80b428b03abf");
var template = document.getElementById("f887cbbf-e39d-4fbc-873d-a235ad91c360");
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



