
<!-- README.md is generated from README.Rmd. Please edit that file -->

# enaho

El paquete `enaho` permite descargar, leer y combinar bases del
Instituto Nacional de Estadística e Informática (INEI) del Perú.

<!-- badges: start -->

[![](https://img.shields.io/github/r-package/v/dopatendo/enaho)](https://github.com/dopatendo/enaho)
<!-- badges: end -->

## Instalación

Puede instalar la versión desarrollador de `enaho` usando:

``` r
remotes::install_github("dopatendo/ILSAmerge")
```

## Identificar bases y módulos disponibles

Para identificar qué bases y módulos están disponibles, use
`modulos.desc()`. Debe introducir qué encuesta quiere consultar
(`"ENAHO"` o `"ENAHOpanel"`), y qué módulos. Si omite el nombre de la
encuesta se usará `"ENAHO"`. Si omite los módulos, se mostrarán todos
los disponibles:

``` r
modulos.desc(encuesta = "ENAHO")
##                                                           Nombre Módulo
## 1                     Características de la Vivienda y del Hogar     01
## 2                      Características de los Miembros del Hogar     02
## 3                                                      Educación     03
## 4                                                          Salud     04
## 5                                              Empleo e Ingresos     05
## 6                     Gastos en Alimentos y Bebidas (Módulo 601)     07
## 7                                        Instituciones Beneficas     08
## 8                                   Mantenimiento de la Vivienda     09
## 9                                   Transportes y Comunicaciones     10
## 10                                       Servicios a la Vivienda     11
## 11               Esparcimiento, Diversion y Servicios de Cultura     12
## 12                                             Vestido y Calzado     13
## 13                                      Gastos de Transferencias     15
## 14                                             Muebles y Enseres     16
## 15                                      Otros Bienes y Servicios     17
## 16                                        Equipamiento del Hogar     18
## 17                                           Producción Agrícola     22
## 18                                        Subproductos Agricolas     23
## 19                                           Producción Forestal     24
## 20                Gastos en Actividades Agricolas y/o Forestales     25
## 21                                           Producción Pecuaria     26
## 22                                        Subproductos Pecuarios     27
## 23                               Gastos en Actividades Pecuarias     28
## 24                                 Condiciones de vida y Pobreza     29
## 25                               Sumarias (Variables Calculadas)     34
## 26                       Programas Sociales (Miembros del Hogar)     37
## 27                         Ingresos del Trabajador Independiente     77
## 28                     Bienes y Servicios de Cuidados Personales     78
## 29                                       Participación Ciudadana     84
## 30                    Gobernabilidad, Democracia y Transparencia     85
## 31                             Indicadores Nutricionales – CENAN    124
## 32 Beneficiarios de Instituciones sin fines de lucro: Olla Común   1825
```

También puede obtener los años para los que cada módulo está disponible
usando `mostrarannos = TRUE`

``` r
modulos.desc(encuesta = "ENAHO", modulos = c(29,32), mostrarannos = TRUE)
##                           Nombre Módulo           Años
## 24 Condiciones de vida y Pobreza     29 2004;2005;2006
```

## Descargar bases

Para descargar bases use `descargar.inei()`. Tendrá que seleccionar un
módulo con `modulo`; los años que necesite con `annos`; el directorio de
descarga con `dirdescarga`; y el período usando `"anual"`, `"t1"`,
`"t2"`, `"t3"`, `"t4"`.

Por ejemplo, para descargar el módulo 37 del primer trimestre de 2011 en
el directorio temporal:

``` r
descargar.inei(encuesta = "ENAHO", modulo = 37, anno = 2011, dirdescarga = tempdir())
## Descargando 1 archivo.
```

Note que una carpeta se habrá creado en el directorio:

``` r
list.files(tempdir(),pattern = "enaho")
## [1] "enaho_2011_Modulo37_anual"
```

Dentro de esta carpeta se habrán descomprimido todos los archivos
relacionados a ese período y módulo:

``` r
list.files(file.path(tempdir(),"enaho_2011_Modulo37_anual"))
## [1] "CED-01-700 2011.pdf"   "Diccionario2011.pdf"   "Enaho01-2011-700.sav" 
## [4] "FichaTecnica_2011.pdf"
```
