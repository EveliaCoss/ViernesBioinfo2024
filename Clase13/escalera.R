# BioFreelancer 2022
# Ejercicio adapatado de la idea del plot original de:
# https://codingwithavery.com/posts/2020-08-12-tidy-tuesday-avatar/

# Cargamos librerias
library( "pacman" )     # un paquete para cargar paquetes

# usamos la funcion p_load de pacman
p_load( "vroom",        # para leer datos facilmente
        "dplyr",        # para manejar datos
        "ggplot2",      # para graficar
        "ggrepel",      # para poner textos sobre el grafico
        "ggsci" )       # para usar la paleta de colores D3

# Cargamos datos
episodios <- vroom( file = "https://data.biofreelancer.com/tronos" )    # Leemos los datos desde internet

# creamos un df con las coordenadas
# para los promedios por temporada
graficable1 <- episodios %>%                                # a partir de los datos completos...
  group_by( temporada ) %>%                                # agrupamos la tabla en subtablas, de acuerdo a la temporada del episodio
  mutate( promedio_temporada = mean( calificacion ) ) %>%  # creamos una nueva columna llamada promedio_temporada. El mean( ) se va  a calcular en cada subset del dataframe
  mutate( inicio_temporada = min( indice ) ) %>%           # creamos nuevas columnas. La coordenada x 1 sera el numero  global del primer episodio
  mutate( fin_temporada = max( indice ) ) %>%              # La coordenada x2 sera el numero global del ultimo episodio de la temporada
  ungroup( )                                               # por buena practica, despues del group by y su operacion, desagrupamos con ungroup

# necesitamos un dataframe con las coordenadas
# de los segmentos del promedio
segmentos_promedio <- graficable1 %>%           # a partir de donde calculamos los promedios
  select( temporada,                            # seleccionamos solo las columnas temporada,
          promedio_temporada,                   # y promedio
          inicio_temporada,                     # y el indice del primer episodio
          fin_temporada ) %>%                   # y el indice del ultimo episodio
  unique( )                                     # y eliminamos filas repetidas

# plot de puntos
escalera1 <- ggplot( data = graficable1,                       # a partir del datframe graficable
                     mapping = aes( x = indice,                # en el eje x va el numero global del episodio
                                    y = calificacion,          # en el eje y va la calificacion de la critica
                                    color = temporada ) ) +    # el color de lo dibujado dependera del valor "temporada"
  geom_segment( mapping = aes( xend = indice,                  # dibujamos el palo de paleta, el valor en x es el mismo indice de episodio
                               yend = promedio_temporada ),    # y el final del palo de paleta sera el promedio de cada temporada
                linetype = "dashed" ) +                        # tiempo de linea "rayada"
  geom_point(  )                                               # dibujamos un punto al final de la paleta

# Visualizamos
escalera1

# Agregamos la linea de promedio por temporada
escalera2 <- escalera1 +                                       # a partir del grafico 1
  geom_segment( data = segmentos_promedio,                     # usamos otro dataframe
                mapping = aes( x = inicio_temporada,           # la raya del promedio Empieza en el primer episodio de la temporada
                               xend = fin_temporada,           # la raya del promedio Termina en el primer episodio de la temporada
                               y = promedio_temporada,         # la raya del promedio, en el eje y empieza en el promedio
                               yend = promedio_temporada ),    # y termina tambien en el promedio
                size = 1.5,                                    # hacemos la linea poco mas gruesa
                lineend = "round" )                            # el extremo de las lineas sera redondeado

# Vis
escalera2

# Agregamos texto de eventos destacables
escalera3 <- escalera2 +
  geom_text_repel( mapping = aes( label = evento ),    # el texto que aparecera dependera de el evento destacado que traemos en la tabla
                   seed = 99 )                         # ponemos una semilla de azar para que nos salga igual el

# Vis
escalera3

# ajustamos escalas y titulos
escalera4 <- escalera3 +
  scale_y_continuous( breaks = 4:10,                       # ponemos marcas del 4 al 10, de uno en uno
                      sec.axis = dup_axis( ) ) +           # duplicamos el eje del otro lado
  scale_color_d3( ) +                                      # usamos una escala del paquete ggsci 
  labs( title = "La Decepcion de Game Of Thrones",
        subtitle = "La ultima temporada obtuvo pesima critica IMDB",
        x = "Episodios",
        y = "Calificacion IMDB",
        color = "Temporadas" )

# Vis
escalera4

# Mejoramos el tema, parte 1
escalera5 <- escalera4 +
  theme_minimal( base_size = 15 ) +                         # el tamanio de las letras sera 15
  theme( plot.title = element_text( hjust = 0.5 ) ) +       # centramos el titulo
  theme( plot.subtitle = element_text( hjust = 0.5 ) ) +    # centramos el subtitulo
  theme( axis.title.y.right = element_blank( ) )            # eliminamos el titulo del segundo eje Y (el duplicado)

# Vis
escalera5

# Mejoramos tema parte2
escalera6 <- escalera5 +
  theme( axis.text.x = element_blank( ) ) +    # eliminamos los numeros del eje X
  theme( axis.line.x =  element_line( ) )      # y agregamos una linea sobre el borde del eje X

# Vis
escalera6

# Mejoramos tema parte3
escalera7 <- escalera6 +
  theme( panel.grid.major.y = element_line( linetype = "dashed",      # las guias del eje y van a ser de tipo punteado
                                            color = "gray80" ) ) +    # color gris
  theme( panel.grid.major.x = element_blank( ) ) +                    # Eliminamos guias mayores del eje X
  theme( panel.grid.minor.x = element_blank( ) ) +                    # Eliminamos guias menores del eje X
  theme( panel.grid.minor.y = element_blank( ) )                      # Eliminamos guias menores del eje Y

# Vis
escalera7

# Mejoramos tema parte4
escalera8 <- escalera7 +
  theme( legend.position = "top" ) +                                  # movemos la leyenda a la parte superior
  guides( color = guide_legend( nrow = 1 ) ) +                        # forzamos que la leyenda se acomoda en un solo renglon
  theme( legend.box.background = element_rect( color = "black" ) )    # Remarcamos con negro el rectangulo al rededor de la leyenda

# Vis
escalera8

# guardamos el plot
ggsave( filename = "tronos.pdf",    # el nombre del archivo a crear. Sera en formato PDF
        plot = escalera8,           # que grafico vamos a guardar?
        width = 14,                 # ancho de 14 pulgadas
        height = 5 )                # alto de 5 pulgadas

# guardamos la tabla
# con las coordenadas de promedios
write.csv( x = segmentos_promedio,    # cual de tus dataframes quieres guardar?
           file = "segmentos.csv",    # con que nombre se va a guardar?
           row.names = FALSE )        # NO guardamos el numero de fila (no es necesario)

## FIN DEL EJERCICIO - BioFreelancer 2022