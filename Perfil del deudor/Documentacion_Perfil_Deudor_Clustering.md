# Documentación de Proyecto: Perfilamiento de Deudores mediante Clustering

## 🎯 Objetivo General
Segmentar y describir perfiles de clientes deudores utilizando técnicas de clustering (jerárquico, k-modes, k-prototypes), apoyadas en análisis de reducción de dimensionalidad (PCA y MCA) para mejorar la interpretación y robustez de los resultados.

---

## 📦 Archivos involucrados

- `perfil_deudor_jerarquico_balanceado.R`
- `perfil_deudor_jerarquico_final.R`
- `perfil_deudor_jerarquico_final_kespecificado.R`
- `perfil_deudor_kmodes.R`
- `perfil_deudor_kprototypes.R`
- `reduccion_dimensionalidad.R`

---

## 📊 Descripción del problema

El análisis parte de un dataset derivado de un estudio previo sobre predicción de pagos. Para todos los scripts, el filtro inicial considera solo aquellos clientes cuyo estado del juicio es "en trámite" y define una variable binaria `AL_MENOS_UN_PAGO` como variable objetivo, con base en si el último pago fue posterior a 1980-01-01.

La edad se calcula a partir de la fecha de nacimiento y la fecha de referencia del análisis.

---

## 🔧 Reducción de dimensionalidad

Archivo: `reduccion_dimensionalidad.R`

- **PCA**: aplicada a variables cuantitativas, se retienen 9 variables clave que explican más del 60% de la varianza en 5 componentes principales.
- **MCA**: aplicada a variables categóricas, permite seleccionar las variables cualitativas más informativas eliminando aquellas redundantes o con demasiadas categorías.

---

## 🤖 Técnicas de clustering aplicadas

1. **Clustering Jerárquico Mixto**  
   Archivos:
   - `perfil_deudor_jerarquico_balanceado.R`
   - `perfil_deudor_jerarquico_final.R`
   - `perfil_deudor_jerarquico_final_kespecificado.R`

   - Se aplican métodos de clustering jerárquico con matrices de distancia mixtas (`daisy`).
   - En versiones avanzadas, se optimiza el número de clusters mediante el estadístico Gap, codo del WSS, y se reduce por cercanía entre centroides.
   - Se usan versiones balanceadas del dataset para mejorar la interpretación y evitar sesgos.

2. **K-Modes**  
   Archivo: `perfil_deudor_kmodes.R`

   - Variables numéricas se discretizan en rangos para convertir todo a categórico.
   - Se utiliza la métrica de Gower.
   - Se evalúa la calidad del clustering con índices: Silhouette (≈0.07), Davies-Bouldin (≈3.03), y Calinski-Harabasz (≈1469), mostrando moderada separación.

3. **K-Prototypes**  
   Archivo: `perfil_deudor_kprototypes.R`

   - Se trabaja directamente con datos mixtos.
   - Utiliza disimilitud combinada (euclidiana para numéricas, Gower para categóricas).
   - Aplica NbClust por lotes para determinar el número de clusters.
   - Se evalúa con Silhouette para cuantitativas y cualitativas combinadas (≈0.085).

---

## 📌 Recomendaciones finales

- Consolidar la lógica de reducción y clustering en un pipeline reproducible.
- Incluir validación cruzada o benchmarking contra modelos supervisados.
- Visualizar resultados de clustering con gráficos de contribución y proyección.
- Automatizar la interpretación de los perfiles por medio de reglas o árboles de decisión.

---

## ✅ Resultado esperado

Obtención de grupos homogéneos de deudores basados en sus características sociodemográficas, crediticias y de comportamiento de pago, que sirvan como entrada para estrategias de segmentación, priorización de cobros o personalización de gestiones.
