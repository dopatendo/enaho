
<!-- README.md is generated from README.Rmd. Please edit that file -->

# enaho

El paquete `enaho` permite descargar, leer y combinar bases del
Instituto Nacional de Estadística e Informática (INEI) del Perú.

## Instalación

Puede instalar la versión desarrollador de `enaho` usando:

``` r
remotes::install_github("dopatendo/enaho")
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
descargar.inei(encuesta = "ENAHO", modulo = 37, anno = 2011, tipo = "t1", dirdescarga = tempdir())
## Descargando 1 archivo.
```

Note que una carpeta se habrá creado en el directorio:

``` r
list.files(tempdir(),pattern = "enaho")
## [1] "enaho_2011_Modulo37_t1"
```

Dentro de esta carpeta se habrán descomprimido todos los archivos
relacionados a ese período y módulo:

``` r
list.files(file.path(tempdir(),"enaho_2011_Modulo37_t1"))
## [1] "CED-01-700 2011.pdf"  "DiccionarioDatos.pdf" "Enaho01-2011-700.sav"
## [4] "FichaTecnica.pdf"
```

## Leer bases descargadas y combinarlas

Las bases descargads con `descargar.inei()` pueden ser leídas con
`leer.inei()`. El módulo y los períodos deben ser especificados. Si más
de una base es leída, la función producirá una lista. Por ejemplo,
descargando también el segundo trimestre de 2011:

``` r
descargar.inei(encuesta = "ENAHO", modulo = 37, annos = 2011, tipo = "t2", dirdescarga = tempdir())
## Descargando 1 archivo.
```

Podemos leer ambos trimestres:

``` r
leidas <- leer.inei(encuesta = "ENAHO", modulo = 37, annos = 2011, tipo = c("t1","t2"), directorio = tempdir())
## Leyendo 2 archivos.
## Leyendo archivo 1 de 2.
## Leyendo archivo 2 de 2.
leidas
## $`2011_Modulo37_t1`
## # A tibble: 4,625 × 24
##    AÑO   MES   CONGLOME VIVIENDA HOGAR UBIGEO DOMINIO ESTRATO CODINFOR  P702
##    <chr> <chr> <chr>    <chr>    <chr> <chr>    <dbl>   <dbl> <chr>    <dbl>
##  1 2011  01    0007     049      11    010101       4       4 01           3
##  2 2011  01    0007     075      11    010101       4       4 02           5
##  3 2011  01    0027     047      11    010504       4       7 02           2
##  4 2011  01    0027     047      11    010504       4       7 02           3
##  5 2011  01    0027     047      11    010504       4       7 02           5
##  6 2011  01    0027     047      11    010504       4       7 02           5
##  7 2011  01    0027     108      11    010504       4       7 02           3
##  8 2011  01    0027     108      11    010504       4       7 02           4
##  9 2011  01    0027     109      11    010504       4       7 01           4
## 10 2011  01    0027     110      11    010504       4       7 02           4
## # ℹ 4,615 more rows
## # ℹ 14 more variables: P703 <dbl+lbl>, P704 <dbl+lbl>, P705 <dbl+lbl>,
## #   P7061 <dbl+lbl>, P7062 <dbl+lbl>, P7063 <dbl+lbl>, P7064 <dbl+lbl>,
## #   P7065 <dbl+lbl>, P706A1 <dbl+lbl>, P706A2 <dbl+lbl>, P706A3 <dbl+lbl>,
## #   P706A4 <dbl+lbl>, TICUEST01 <dbl+lbl>, FACTRIM <dbl>
## 
## $`2011_Modulo37_t2`
## # A tibble: 3,940 × 24
##    AÑO   MES   CONGLOME VIVIENDA HOGAR UBIGEO DOMINIO ESTRATO CODINFOR  P702
##    <chr> <chr> <chr>    <chr>    <chr> <chr>    <dbl>   <dbl> <chr>    <dbl>
##  1 2011  04    0001     019      11    010101       4       4 02           1
##  2 2011  04    0001     019      11    010101       4       4 02           2
##  3 2011  04    0001     019      11    010101       4       4 02           3
##  4 2011  04    0001     019      11    010101       4       4 02           4
##  5 2011  04    0001     019      11    010101       4       4 02           5
##  6 2011  04    0047     032      11    010201       7       4 02           2
##  7 2011  04    0047     032      11    010201       7       4 02           3
##  8 2011  04    0047     032      11    010201       7       4 02           4
##  9 2011  04    0047     069      22    010201       7       4 02           6
## 10 2011  04    0047     082      11    010201       7       4 02           1
## # ℹ 3,930 more rows
## # ℹ 14 more variables: P703 <dbl+lbl>, P704 <dbl+lbl>, P705 <dbl+lbl>,
## #   P7061 <dbl+lbl>, P7062 <dbl+lbl>, P7063 <dbl+lbl>, P7064 <dbl+lbl>,
## #   P7065 <dbl+lbl>, P706A1 <dbl+lbl>, P706A2 <dbl+lbl>, P706A3 <dbl+lbl>,
## #   P706A4 <dbl+lbl>, TICUEST01 <dbl+lbl>, FACTRIM <dbl>
```

Y, usando `combinar.inei()` (o el argumento `combinar` en `leer.inei`)
podemos combinar ambos trimestres. También podemos especificar si sólo
queremos combinar las variables comunes o todas las variables:

``` r
combinar.inei(x = leidas, solocomunes = FALSE)
## # A tibble: 8,565 × 24
##    AÑO   MES   CONGLOME VIVIENDA HOGAR UBIGEO DOMINIO ESTRATO CODINFOR  P702
##  * <chr> <chr> <chr>    <chr>    <chr> <chr>    <dbl>   <dbl> <chr>    <dbl>
##  1 2011  01    0007     049      11    010101       4       4 01           3
##  2 2011  01    0007     075      11    010101       4       4 02           5
##  3 2011  01    0027     047      11    010504       4       7 02           2
##  4 2011  01    0027     047      11    010504       4       7 02           3
##  5 2011  01    0027     047      11    010504       4       7 02           5
##  6 2011  01    0027     047      11    010504       4       7 02           5
##  7 2011  01    0027     108      11    010504       4       7 02           3
##  8 2011  01    0027     108      11    010504       4       7 02           4
##  9 2011  01    0027     109      11    010504       4       7 01           4
## 10 2011  01    0027     110      11    010504       4       7 02           4
## # ℹ 8,555 more rows
## # ℹ 14 more variables: P703 <dbl+lbl>, P704 <dbl+lbl>, P705 <dbl+lbl>,
## #   P7061 <dbl+lbl>, P7062 <dbl+lbl>, P7063 <dbl+lbl>, P7064 <dbl+lbl>,
## #   P7065 <dbl+lbl>, P706A1 <dbl+lbl>, P706A2 <dbl+lbl>, P706A3 <dbl+lbl>,
## #   P706A4 <dbl+lbl>, TICUEST01 <dbl+lbl>, FACTRIM <dbl>
```

## Leer bases directamente desde la web

Usando `leer.inei.web()` podemos leer las bases directamente desde la
web, aplicando las mismas reglas que `leer.inei()`:

``` r
leer.inei.web(encuesta = "ENAHO", modulo = 37, annos = 2011, tipo = c("t1"))
## Descargando 1 archivo.
## Leyendo 1 archivo.
## Leyendo archivo 1 de 1.
## # A tibble: 4,625 × 24
##    AÑO   MES   CONGLOME VIVIENDA HOGAR UBIGEO DOMINIO ESTRATO CODINFOR  P702
##    <chr> <chr> <chr>    <chr>    <chr> <chr>    <dbl>   <dbl> <chr>    <dbl>
##  1 2011  01    0007     049      11    010101       4       4 01           3
##  2 2011  01    0007     075      11    010101       4       4 02           5
##  3 2011  01    0027     047      11    010504       4       7 02           2
##  4 2011  01    0027     047      11    010504       4       7 02           3
##  5 2011  01    0027     047      11    010504       4       7 02           5
##  6 2011  01    0027     047      11    010504       4       7 02           5
##  7 2011  01    0027     108      11    010504       4       7 02           3
##  8 2011  01    0027     108      11    010504       4       7 02           4
##  9 2011  01    0027     109      11    010504       4       7 01           4
## 10 2011  01    0027     110      11    010504       4       7 02           4
## # ℹ 4,615 more rows
## # ℹ 14 more variables: P703 <dbl+lbl>, P704 <dbl+lbl>, P705 <dbl+lbl>,
## #   P7061 <dbl+lbl>, P7062 <dbl+lbl>, P7063 <dbl+lbl>, P7064 <dbl+lbl>,
## #   P7065 <dbl+lbl>, P706A1 <dbl+lbl>, P706A2 <dbl+lbl>, P706A3 <dbl+lbl>,
## #   P706A4 <dbl+lbl>, TICUEST01 <dbl+lbl>, FACTRIM <dbl>
```
