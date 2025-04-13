# Problem-Set-2

#### Juan Esteban Díaz Torres - 202020319
#### Natalia Plata Ángel - 201730699
#### Ángel David Ramírez Torres - 202112704
#### Juan Pablo Grimaldos - 202122627

## Tabla de Contenidos
- [Documentación](#Documentación)
- [Scripts](#Scripts)
- [Stores](#Stores)
- [Views](#Views)

## Documentación
Esta carpeta contiene el archivo final con la solución del taller en formato pdf y en formato Tex.

## Scripts
La carpeta *Scripts* contiene el script utilizado para limpiar y procesar las bases de datos, el script utilizado para realizar la estadísticas descriptivas y cuatro script con la implentación de los algoritmos utilizados para predecir la pobreza de los hogares. A continuación, se detalla la estructura y contenido:

- **Script de limpieza:** El archivo "1_Limpieza_Consolidado_v2.R" toma las bases de datos provistas como inputs para realizar el taller y realiza la imputacion de valores faltantes, tratamiento de valores extremos, limpieza de los datos y la consolidacion de las bases de entrenamiento y de testeo a nivel de hogar. 


- **Script de estadísticas descriptivas:** En el archivo "Poner_nombre.R" se realiza la revisión de las observaciones y las variables de interés y se generan las figuras y tablas que contienen las estadísticas descriptivas. 

- **Implementación de los algoritmos:** Cuatro archivos *Elastic Net y Logit.R*, *Random_Forest_0.R*, *Random_Forest.R* y *Boosting.R* donde se realiza la implementación y la ejecución de la prediccion de la pobreza de los hogares por medio de Elastic Net y Regresion Logística (Logit), Random Forest y Adaptative Boosting (Adaboost) respectivamente. 

- **Carpeta de scripts iniciales:** Finalmente, se incluye la carpeta *Deprecated* con los scripts que cada miembro del equipo utilizó para comenzar a desarrollar el taller y posteriormente fueron remplazados por scripts consolidados después de un proceso de ajuste. 

## Stores
Esta carpeta contiene las bases de datos provistas como insumos para realizar el taller (*train_personas.rds, train_hogares.csv, test_personas.csv, test_hogares.csv y sample_submission.csv*), la base datos *db_final.rds* consolidada usando el script *1_Limpieza_Consolidado_v2.R* y la carpeta *Predicciones*. Todos los archivos están almacenados en formato csv salvo *train_personas.rds* y *db_final.rds*. Se decidió cambiar el formato de estos archivos para poderlos subirlos a github y facilitar el trabajo. 

- **Predicciones:** Se incluye una carpeta con las predicciones enviadas a Kaggle por los miembros del equipo. 

## Views
Esta carpeta contiene las gráficas y tablas generadas en la solución del taller. Las gráficas están en formato PNG y las tablas en formato txt.

### Gráficas (formato PNG):
**desempeño_validation_set.png:** Gráfica que muestra el desempeño del modelo en el conjunto de validación.

**dinamica_promedio_salario_por_hora_con_la_edad:** Gráfica que ilustra la dinámica del salario promedio en función de la edad diferenciado por género.

**distribucion_errores.png:** Gráfica que representa la distribución de los errores del modelo.

**salario_por_edad.png:** Gráfica que muestra la relación entre el salario y la edad.

**salario_por_edad_genero.png:** Gráfica que compara los salarios por edad y género.

**salario_por_edad_scatter.png:** Gráfica de dispersión que representa la relación entre el salario y la edad.

**y_ingLab_m_ha.png:** Gráfica que posiblemente muestra ingresos laborales en función de alguna variable (el nombre completo no está especificado).

### Tablas (formato TXT):
**desc_est.txt:** Tabla con estadísticas descriptivas de los datos.

**mod1.txt:** Tabla que contiene los resultados o parámetros del primer modelo.

**tabla_comparacion_errores_prediccion.txt:** Tabla que compara los errores de predicción de las dififerentes aproximaciones.

**tabla_desempeño_validation_set.txt:** Tabla que detalla el desempeño de los diferentes modelos en el conjunto de validación.

**tabla_intervalos_edad_pico.txt:** Tabla que presenta intervalos de edad donde se observan picos de ingreso.
