Terremoto\_cqb\_miercolesdedatos
================

# Los datos desde el gitbub de datos-de-miercoles

## Datos para plotear mundo

(hay que buscar las coordenadas de la zona a plotear  
Para Coquimbo usar `xlim = c(-83.826, -63.204),ylim = c( -23.037,
-39.669)` dentro de `coord_sf()`

## Preparando los datos

### Escala Richter

Para visualizar mejor los diferentes sismos transformar la escala
Richter con logaritmo
(<http://matematicasentumundo.es/NATURALEZA/naturaleza_Richter.htm>) :

``` r
terremotos <- terremotos %>%   filter( tipo == "terremoto" ) %>% 
  mutate(energia =10^(11.8 + 1.5 *magnitud) )
```

``` r
# Verificando el dia del terremoto
terremotos %>% filter(fecha == "2015-09-16") %>%  arrange(desc(magnitud))
```

    ## # A tibble: 12 x 8
    ##    fecha      hora   latitud longitud tipo     profundidad magnitud energia
    ##    <date>     <drtn>   <dbl>    <dbl> <chr>          <dbl>    <dbl>   <dbl>
    ##  1 2015-09-16 22:54   -31.6     -71.7 terremo~        22.4      8.3 1.78e24
    ##  2 2015-09-16 23:18   -31.6     -71.4 terremo~        28.4      7   2.00e22
    ##  3 2015-09-16 22:59   -31.6     -71.7 terremo~        26.7      6.4 2.51e21
    ##  4 2015-09-16 07:40     1.88    126.  terremo~        41.6      6.3 1.78e21
    ##  5 2015-09-16 14:03    -6.01    151.  terremo~         6        6.1 8.91e20
    ##  6 2015-09-16 23:03   -31.8     -71.7 terremo~        19.1      6.1 8.91e20
    ##  7 2015-09-16 23:16   -31.6     -71.9 terremo~        35        6.1 8.91e20
    ##  8 2015-09-16 23:38   -31.8     -72.0 terremo~        12.5      5.9 4.47e20
    ##  9 2015-09-16 23:09   -31.6     -71.7 terremo~        35        5.7 2.24e20
    ## 10 2015-09-16 23:23   -31.2     -71.8 terremo~        32.1      5.7 2.24e20
    ## 11 2015-09-16 23:09   -31.1     -71.4 terremo~        37.8      5.6 1.58e20
    ## 12 2015-09-16 23:28   -30.7     -71.8 terremo~        20.7      5.5 1.12e20

``` r
terremoto_coquimbo <- terremotos %>% 
  filter(fecha > "2015-07-16" & fecha < "2015-11-16") %>%  
  filter(latitud < -23.037 & latitud > -39.669) %>% 
  filter( longitud < -63.204 & longitud > -83.826 )


dim(terremoto_coquimbo) # 51 obs
```

    ## [1] 51  8

``` r
library(ggthemes)
library(gganimate)



terremoto_coquimbo %>%  ggplot() +
  borders("world", colour = "gray85", # color limites de paises
           fill = "black") +  # color de la tierra
  coord_sf( xlim = c(-83.826, -63.204),ylim = c( -23.037, -39.669))+ 
    theme_map()+
    theme(panel.grid.major = element_line(colour = "white"), # lineas del grid
          panel.background = element_rect(fill = "darkgrey")) +
  geom_point(aes(x = longitud, y = latitud, size = energia, colour= magnitud),
             alpha= 0.9) +
  scale_colour_gradientn(colours = c("blue", "green", "yellow", "orange", "red"))+
  scale_size_continuous(range = c(1,80))+
  theme(legend.position = "none")+  
  # DESDE AQUÍ SE ANIMA!
  transition_states(fecha, wrap = F, transition_length = 0)+
  labs(title = "Terremotos (Escala Richter >= 5.5)", subtitle = "Fecha: {closest_state}", caption = "#DatosDeMierRcoles")
```

![](terremoto_cqb_files/figure-gfm/unnamed-chunk-4-1.gif)<!-- -->

En el plot anterior no se apreciar correctamente los sismos pre y post
terremotos porque hay días que no se registran sismos, esto hace que la
gráfica pierde el
“ritmo”.

## Rellenar los días sin sismos

``` r
relleno <- data.frame(fecha =seq(as.Date("2015-07-16"), as.Date("2015-11-16"), "days") )


terremoto_coquimbo1<- dplyr::full_join(terremoto_coquimbo, relleno , by ="fecha") %>% 
  arrange(fecha) %>%
  replace(., is.na(.), 0) %>% 
  filter(fecha >= "2015-08-23" & fecha <= "2015-10-10")

#PLOTEANDO (EL FINAL)

gif_cqb <- terremoto_coquimbo1 %>%  ggplot() +
    borders("world", colour = "gray85", # color limites de paises
             fill = "black") +  # color de la tierra
    coord_sf( xlim = c(-83.826, -63.204),ylim = c( -23.037, -39.669))+ 
      theme_map()+
      theme(panel.grid.major = element_line(colour = "white"), # lineas del grid
            panel.background = element_rect(fill = "darkgrey"),
            plot.caption = element_text(size = 13),
            legend.position = "none") +
    geom_point(aes(x = longitud, y = latitud, size = energia, colour= magnitud),
               alpha= 0.9) +
    scale_colour_gradientn(colours = c("blue", "green", "yellow", "orange",  "red"))+
    scale_size_continuous(range = c(2,140))+
    #DESDE AQUÍ SE ANIMA!
    transition_states(fecha, wrap = F, transition_length = 0)+
    shadow_wake(0.4)+ # para que los puntos salgan lentamente (ver otras funciones)
    labs(title = "Sismos (>5.5) días previos y post terremoto en la Región de Coquimbo(2015)", subtitle = "fecha: {closest_state}" ,caption = "#DatosDeMieRcoles")

 # animate(gif_cqb, nframes = 60) # para verlo en consola
 a_gif <- animate(gif_cqb, nframes = 60)
  a_gif
```

![](terremoto_cqb_files/figure-gfm/unnamed-chunk-5-1.gif)<!-- -->

``` r
 #guardar

anim_save(a_gif,filename = "cqb-2.gif")
```
