# T3 — Regresión económica: CASEN y ENE

**Curso:** Fundamentos de Programación para Análisis Económico (UdeC, Chillán, 2026-2)
**Autor:** Cristóbal Sepúlveda
**Fecha:** octubre de 2026

## Pregunta de la Parte B

¿Cuánto cambia la probabilidad de ser informal según el nivel educacional entre las personas ocupadas en Chile? ¿Cuánto de esa diferencia se mantiene al comparar personas de la misma edad, sexo, región y rama de actividad? ¿Y es distinta entre hombres y mujeres?

## Respuesta

Frente a quien tiene educación media, tener básica o menos se asocia con una probabilidad de ser informal 18,1 pp mayor y tener universitaria o más con una 16,4 pp menor (informalidad promedio: 26,5 %).
Al comparar personas de la misma edad, sexo, región y rama, las diferencias se reducen (15,9 pp y -11,4 pp); la rama de actividad es el control que más las mueve.
En mujeres las diferencias por educación son algo mayores (alrededor de 3 pp). Son asociaciones, no efectos causales.

## Cómo correr el script

1. Abrir `T3.Rproj` en RStudio (fija la carpeta de trabajo; no se usa `setwd()`).
2. Instalar, si no están, los paquetes `dplyr`, `broom`, `modelsummary` y `tidyr`.
3. Abrir `scripts/tarea_t3.R` y ejecutarlo completo (Ctrl + Shift + Enter).

El script corre de principio a fin y genera los archivos de `data/processed/`.

## Estructura

```
T3/
├── T3.Rproj
├── README.md
├── data/
│   ├── raw/
│   │   ├── casen_reducido.csv        # CASEN reducida (60 filas)
│   │   └── ene-2026-06-mjj.csv       # ENE, trimestre mayo-junio-julio 2026
│   └── processed/
│       ├── t3_tabla_ene.csv          # modelos de la Parte B (modelsummary)
│       └── t3_coeficientes_casen.csv # coeficientes de m2 (Parte A)
└── scripts/
    └── tarea_t3.R                    # script completo (Partes A, B y C)
```

## Decisiones de limpieza (ENE)

- Solo ocupados (`activ == 1`), porque `ocup_form` solo existe para ellos.
- `cine11_1d`: el código 9 ("sin clasificación", 58 casos) se convierte a `NA`. Educación se agrupa en 4 niveles con "Media" como categoría base.
- `r_p_rev4cl_caenes`: el código 999 ("sin clasificación", 96 casos) se convierte a `NA`.
- Región y rama se usan como texto, no como números.
- Todos los modelos usan la misma muestra (41.697 casos completos) y se ponderan con `fact_cal`.

## Declaración de autoría y uso de IA

- **Herramienta utilizada:** Claude (Anthropic).
- **Para qué la usé:**
  - Proponer ideas de pregunta para la Parte B y revisar que no se parecieran al laboratorio.
  - Generar el código base de cada etapa, que luego corrí, verifiqué con los resultados esperados y comenté.
  - Explicar funciones y conceptos (`read.csv2`, `case_when`, `augment`, distancia de Cook, R² ajustado, interacciones, modelo de probabilidad lineal).
  - Depurar errores: la ruta del archivo (se resolvió con un proyecto `.Rproj`) y la exportación a CSV con `modelsummary` ("objeto 'd' no encontrado").
  - Guiarme con preguntas y plantillas para redactar las interpretaciones, y corregir errores de signo, de unidades (pp) y de lenguaje causal en mis respuestas.
  - Revisar el script completo contra la rúbrica y darle formato final (ortografía y orden).
- **Qué hice yo:** elegí la pregunta de la Parte B, el tercer modelo de la Parte A y la variante con interacción por sexo; corrí cada etapa en RStudio y comparé los resultados; respondí las preguntas de interpretación (A3–A6 y B6) eligiendo y redactando las conclusiones, que luego se corrigieron en redacción; hice los commits por etapa.
- **Prompts clave (bitácora):**
  1. "Analiza los datos y rúbrica de la tarea. Luego empecemos con ideas para analizar."
  2. "¿La 4 no se parecerá a algún guion o lab de las últimas semanas?"
  3. "Explícame mejor cómo quedaría la 1."
  4. Error de lectura: "no se puede abrir la conexión" y error de `modelsummary`: "objeto 'd' no encontrado".
  5. "¿Qué conclusión debería sacar de A3?" (y lo mismo para A4, A5, A6 y B6).
  6. "Quiero ver si encuentran algún error según la pauta."
- **Verificación:** confirmo que entiendo y puedo explicar todo lo que entrego.
