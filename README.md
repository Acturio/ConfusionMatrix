# Model Performance Studio

Dashboard Shiny para evaluar modelos de clasificacion mediante matriz de
confusion, curvas ROC y Precision-Recall, y metricas de desempeno.

## Ejecutar

```r
shiny::runApp()
```

La aplicacion permite cargar un archivo CSV y seleccionar desde la interfaz
las columnas que participan en el analisis.

## Respuesta dicotomica

El CSV necesita:

- Una columna con el valor real, con exactamente dos categorias.
- Una columna numerica con la probabilidad de la categoria positiva, entre
  `0` y `1`.

Despues de elegir la categoria positiva, el slider modifica la probabilidad
de corte y actualiza la matriz, las metricas y el punto marcado en las
curvas. Las curvas son interactivas: al colocar el cursor sobre el punto del
`Selected Threshold` se despliegan las metricas calculadas para ese corte.

Ejemplo compatible: `probas.csv`, seleccionando `response` como valor real y
`prob` como probabilidad.

## Respuesta multiclase

El CSV necesita:

- Una columna con el valor real, con dos o mas categorias.
- Una columna numerica de probabilidad, entre `0` y `1`, para cada categoria.

La interfaz solicita indicar `Number of classes`, con valores entre `3` y
`20`. A continuacion crea selectores genericos `Class 1`, `Class 2`, ...,
segun el numero indicado, y muestra el orden de categorias detectado en
`Actual Class` para mapear las probabilidades correctamente. La prediccion
final se calcula tomando la categoria con mayor probabilidad en cada fila.
El dashboard muestra la matriz multiclase y, para cada categoria, metricas y
curvas bajo el esquema uno contra el resto.

Para probar este modo se incluye `multiclass_dummy.csv`. Seleccione:

- `actual_class` como `Actual class column`.
- `3` como `Number of classes`.
- `prob_Bajo` para `Class 1`.
- `prob_Medio` para `Class 2`.
- `prob_Alto` para `Class 3`.

Ejemplo de estructura:

```csv
real,prob_A,prob_B,prob_C
A,0.80,0.15,0.05
B,0.10,0.75,0.15
C,0.05,0.20,0.75
```
