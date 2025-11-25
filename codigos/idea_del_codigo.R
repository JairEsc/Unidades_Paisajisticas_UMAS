# Previo
library(raster)
lista_rasters = list.files("inputs/rasters/",pattern = ".tif$",full.names = T) |>  lapply(raster::raster)


rasters_as_matrix = matrix(data = (lista_rasters |>  lapply((raster::getValues))) |> unlist(),
                         nrow = 998*978,
                         ncol = 5,
                         byrow = F) |>  as.data.frame()

names(rasters_as_matrix) = list.files("inputs/rasters/",pattern = ".tif$") |> basename() |>  tools::file_path_sans_ext()

#Pendiente es la tercera columna

##El código no apareció. 

##La idea del código es definir el número de grupos usando criterios de información Bayesiana. 

##Nos salieron 15 así que se ejecuta un clustering gaussiano (mclust)
library(raster)
"inputs/rasters/" |> list.files(pattern = ".tif$",full.names = T) |> lapply(\(z){plot(raster(z))})
library(mclust)

datos = read.csv("outputs/rasters_as_matrix.csv")

datos = datos |> 
  dplyr::mutate(V5 = dplyr::if_else(condition = V5 == 0, true = NA, false = V5))

### Pendiente arriba de 90, la definimos como el promedio de la pendiente que tienen valores menores a 90.
datos = datos |>
  dplyr::mutate(
    V3 = dplyr::if_else(
      condition = V3 > 90,
      true = mean(V3[V3 < 90], na.rm = TRUE),
      false = V3
    )
  )


datos = scale(datos) |>  as.data.frame()
datos = datos |> 
  dplyr::mutate(V1 = V1 + V1 |>  min(na.rm = T) |>  abs(),
                V2 = V2 + V2 |>  min(na.rm = T) |>  abs(),
                V3 = V3 + V3 |>  min(na.rm = T) |>  abs(),
                V4 = V4 + V4 |>  min(na.rm = T) |>  abs(),
                V5 = V5 + V5 |>  min(na.rm = T) |>  abs())


datos$V1 |>  min(na.rm = T)
datos$V2 |>  min(na.rm = T)
datos$V3 |>  min(na.rm = T)
datos$V4 |>  min(na.rm = T)
datos$V5 |>  min(na.rm = T)




datos = datos |> 
  dplyr::mutate(
    dplyr::across(.cols = V1:V5,
                  .fns = ~ dplyr::if_else(is.na(.x), true = -1, false = .x)
                  )
  )


BIC2 = mclust::mclustBIC(data = datos, G = 6:15, modelNames = c("EEI","EEV","EEE"))
plot(BIC2)

modelo = mclust::Mclust(data = datos, x = BIC2)

resumen = summary(object = modelo, parameters = T)
clasificacion = resumen$classification |>  as.numeric()
clasificacion_matriz = matrix(data = clasificacion, nrow = 998, ncol = 978, byrow = T)

clasificacion_raster_data = raster::raster(clasificacion_matriz)
raster::plot(clasificacion_raster_data, col=rainbow(15))

