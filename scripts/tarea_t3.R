# =============================================================================
# Tarea T3 — Regresión económica: CASEN y ENE
# Fundamentos de Programación para Análisis Económico
# Autor:  Cristóbal Sepúlveda
# Fecha:  13 de octubre de 2026
# Objetivo:
#   Parte A: estima ecuaciones de ingreso de Mincer con la CASEN, compara
#            modelos y realiza un diagnóstico de observaciones influyentes.
#   Parte B: estima si el nivel educacional se asocia con la probabilidad de
#            ser informal, usando la ENE (ocupados, modelos ponderados).
#   Parte C: presenta los modelos de la Parte B y exporta los resultados.
# Ejecución: abrir T3.Rproj y correr el script completo (rutas relativas).
# =============================================================================

library(dplyr)
library(broom)
library(modelsummary)


# =============================================================================
# PARTE A — CASEN
# =============================================================================

# A1. Dos modelos --------------------------------------------------------------
# Experiencia potencial de Mincer: años transcurridos desde el término de los
# estudios. pmax(..., 0) evita valores negativos.
casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

m1 <- lm(ingreso ~ educ, data = casen)                            # simple
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)     # con controles
# lm() descarta las 5 filas con ingreso NA, por lo que n = 55.


# A2. Tabla de coeficientes de m2 ----------------------------------------------
tidy(m2, conf.int = TRUE) |>                                # coeficientes e IC 95 %
  filter(term != "(Intercept)") |>                          # sin el intercepto
  mutate(across(c(estimate, conf.low, conf.high), round),   # pesos enteros
         p.value       = round(p.value, 4),
         significativo = p.value < 0.05) |>                 # TRUE si p < 0,05
  select(term, estimate, conf.low, conf.high, p.value, significativo) |>
  arrange(p.value)                                          # más significativo arriba
# Resultado:
#   educ          42246   29487   55004  0.0000  TRUE
#   generoM       55686  -31246  142618  0.2043  FALSE
#   experiencia    -517   -3408    2375  0.7213  FALSE


# A3. Tercer modelo: perfil cóncavo de Mincer ----------------------------------
# I(experiencia^2) permite que el ingreso crezca con la experiencia a una tasa
# decreciente. I() indica a R que calcule el cuadrado antes de estimar.
m3 <- lm(ingreso ~ educ + experiencia + I(experiencia^2) + genero, data = casen)

modelos <- list(simple = m1, controles = m2, mio = m3)
lapply(modelos, glance) |>
  bind_rows(.id = "modelo") |>
  select(modelo, r.squared, adj.r.squared, nobs)
# Resultado:
#   simple     0.458  0.448  55
#   controles  0.475  0.444  55
#   mio        0.481  0.440  55
#
# Al agregar variables, el R² sube pero el R² ajustado baja. Esto significa que
# la experiencia y la experiencia² no aportan a explicar el ingreso. Por lo
# tanto, mi modelo no mejora, y con solo 55 personas agregar variables le resta
# precisión al modelo sin explicar más.


# A4. Coeficiente de educación en los tres modelos -----------------------------
lapply(modelos, tidy) |>
  bind_rows(.id = "modelo") |>
  filter(term == "educ") |>
  mutate(estimate = round(estimate)) |>
  select(modelo, estimate, p.value)
# Resultado: simple 42080 · controles 42246 · mio 42690

# Correlaciones que explican el resultado:
cor(casen$educ, casen$experiencia)                           # -0.156
cor(casen$experiencia, casen$ingreso, use = "complete.obs")  # -0.088
#
# El coeficiente de educación casi no cambia entre los tres modelos (42.080,
# 42.246 y 42.690). Esto ocurre porque la experiencia casi no tiene relación con
# la educación (cor = -0,156) ni con el ingreso (cor = -0,088). Como no cumple
# ambas condiciones, agregarla no mueve el coeficiente de educación.


# A5. Diagnóstico con augment() ------------------------------------------------
# augment() entrega una fila por persona usada en el modelo, con el valor
# predicho (.fitted), el residuo (.resid), el apalancamiento (.hat) y la
# distancia de Cook (.cooksd). .rownames corresponde a la fila original en
# casen, ya que lm() descartó las 5 filas con NA.

# 1) Las tres personas peor predichas (mayor residuo en valor absoluto).
augment(m2) |>
  mutate(sector = casen$sector[as.numeric(.rownames)],   # sector desde casen
         across(c(.fitted, .resid), round)) |>
  arrange(desc(abs(.resid))) |>
  select(.rownames, ingreso, .fitted, .resid, educ, experiencia, genero, sector) |>
  head(3)
# Resultado: filas 47, 51 y 40, con residuos -298210, -296393 y -249293.
#
# Los tres tienen en común que son hombres, trabajan en agricultura y tienen más
# de 30 años de experiencia. Tienen residuo negativo, es decir, ganan menos de lo
# que el modelo predice. Esto puede deberse a que m2 no incluye el sector, y la
# agricultura paga menos que otros sectores (en esta muestra tiene el ingreso
# promedio más bajo: $447.846).

# 2) Máxima distancia de Cook y máximo apalancamiento.
augment(m2) |>
  summarise(max_cook = max(.cooksd),
            max_hat  = max(.hat))
# Resultado: max_cook 0.0869 · max_hat 0.175

# 3) Re-estimación de m2 sin la persona con mayor distancia de Cook.
#    La fila se obtiene del código y no se escribe a mano, para que el
#    procedimiento siga siendo válido si los datos cambian.
fila_cook <- augment(m2) |>
  slice_max(.cooksd, n = 1) |>
  pull(.rownames) |>
  as.numeric()
fila_cook
# Resultado: 43

m2_sin <- casen |>
  filter(row_number() != fila_cook) |>
  lm(ingreso ~ educ + experiencia + genero, data = _)   # _ = datos del pipe

bind_rows(todos = tidy(m2), sin_cook = tidy(m2_sin), .id = "muestra") |>
  filter(term == "educ") |>
  mutate(estimate = round(estimate)) |>
  select(muestra, estimate, p.value)
# Resultado: todos 42246 · sin_cook 45218
#
# Al excluir a la persona con mayor distancia de Cook, el coeficiente de
# educación pasa de 42.246 a 45.218 y sigue siendo significativo, por lo que la
# conclusión no cambia. Sin embargo, que una sola persona de 55 lo mueva en un
# 7 % muestra que, con una muestra tan pequeña, los resultados son poco precisos
# y pueden depender de pocos casos.


# A6. Interpretación de la Parte A ---------------------------------------------
# En el modelo con controles, cada año adicional de educación se asocia con
# $42.246 más de ingreso mensual, en promedio, manteniendo experiencia y género
# constantes (IC 95 %: 29.487 a 55.004).
#
# El coeficiente de generoM es 55.686. Esto significa que, a igual educación y
# experiencia, los hombres se asocian con un ingreso en promedio $55.686 mayor
# que las mujeres. Sin embargo, su IC va de -31.246 a 142.618 e incluye el cero
# (p = 0,20), por lo que no es significativo al 5 %.
#
# Significancia frente a importancia: generoM es grande (55.686 pesos), pero no
# es significativo. Con n = 55 esto no significa que la diferencia no exista,
# sino que no hay datos suficientes para distinguirla de cero con precisión.


# =============================================================================
# PARTE B — ENE
# =============================================================================

# B1. Pregunta -----------------------------------------------------------------
# ¿Cuánto cambia la probabilidad de ser informal según el nivel educacional
# entre las personas ocupadas en Chile? ¿Cuánto de esa diferencia se mantiene
# al comparar personas de la misma edad, sexo, región y rama de actividad?
# ¿Y es distinta entre hombres y mujeres?


# B2. Carga de la base ---------------------------------------------------------
# La ENE usa ";" como separador y "," como decimal; read.csv2() lee ese formato.
ene <- read.csv2("data/raw/ene-2026-06-mjj.csv")

dim(ene)
# Resultado: 97946 filas, 222 columnas

class(ene$fact_cal)   # verifica que el factor de expansión se leyó como número
# Resultado: "numeric"


# B2. Auditoría de variables ---------------------------------------------------
# ocup_form (formal/informal) solo existe para ocupados, por lo que se filtra
# primero. activ: 1 = ocupado, 2 = desocupado, 3 = fuera de la fuerza de trabajo.
ocupados_crudo <- ene |>
  filter(activ == 1)

nrow(ocupados_crudo)
# Resultado: 41850

# Se revisa cada variable antes de modelar: lm() trata todo como número y no
# advierte si hay códigos especiales (por ejemplo, 9 = "Sin clasificación").

# 1) Resumen de casos problemáticos por variable.
ocupados_crudo |>
  summarise(
    personas        = n(),
    informal_NA     = sum(is.na(ocup_form)),
    educ_sin_clasif = sum(cine11_1d == 9),             # código 9
    rama_sin_clasif = sum(r_p_rev4cl_caenes == 999),   # código 999
    edad_min        = min(edad),
    edad_max        = max(edad)
  )
# Resultado: 41850 · 0 · 58 · 96 · 15 · 99

# 2) Distribución completa de las variables nuevas (no usadas en el laboratorio).
ocupados_crudo |> count(cine11_1d)          # educación: códigos 0 a 6 y 9
ocupados_crudo |> count(r_p_rev4cl_caenes)  # rama: códigos 1 a 21 y 999

# Informalidad, edad, sexo y región están limpias. Educación tiene 58 casos con
# código 9 y rama tiene 96 con código 999, ambos "sin clasificación". Se
# convierten a NA porque no son niveles reales; son pocos casos (< 0,5 %).


# B2. Construcción de las variables del modelo ---------------------------------
# Vectores de nombres para que región y rama queden como texto y no como números.
# El orden sigue los códigos del manual (el código 1 es el primer nombre, etc.).
regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
              "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
              "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
              "Arica y Parinacota", "Ñuble")

ramas <- c("Agricultura y pesca", "Minería", "Industria", "Electricidad y gas",
           "Agua y desechos", "Construcción", "Comercio", "Transporte",
           "Alojamiento y comidas", "Información y comunicaciones",
           "Financieras", "Inmobiliarias", "Profesionales y científicas",
           "Servicios administrativos", "Administración pública", "Enseñanza",
           "Salud", "Arte y recreación", "Otros servicios",
           "Hogares como empleadores", "Organismos extraterritoriales")

ocupados <- ocupados_crudo |>
  mutate(
    # Variable dependiente lógica (TRUE = informal). lm() la trata como 1/0:
    # modelo de probabilidad lineal, coeficientes en puntos porcentuales (x 100).
    informal = ocup_form == 2,
    mujer    = sexo == 2,

    # Educación en cuatro niveles. El código 9 se asigna a NA de forma
    # explícita, para que no sea un NA silencioso.
    educacion = case_when(
      cine11_1d %in% 0:1 ~ "Básica o menos",
      cine11_1d == 2     ~ "Media",
      cine11_1d == 3     ~ "Técnica superior",
      cine11_1d %in% 4:6 ~ "Universitaria o más",
      cine11_1d == 9     ~ NA_character_
    ),
    # "Media" como primer nivel: es la categoría base de comparación.
    educacion = factor(educacion, levels = c("Media", "Básica o menos",
                                             "Técnica superior",
                                             "Universitaria o más")),

    region = regiones[region],                       # código -> nombre
    rama   = ramas[na_if(r_p_rev4cl_caenes, 999)]    # 999 -> NA
  ) |>
  select(informal, mujer, edad, educacion, region, rama, fact_cal)

# Chequeo 1: cantidad de personas por nivel educacional.
ocupados |> count(educacion)
# Resultado: Media 17373 · Básica o menos 9119 · Técnica superior 5624 ·
#            Universitaria o más 9676 · NA 58

# Chequeo 2: NA resultantes, casos completos e informalidad ponderada.
ocupados |>
  summarise(educ_NA      = sum(is.na(educacion)),
            rama_NA      = sum(is.na(rama)),
            completos    = sum(complete.cases(ocupados)),
            informalidad = round(weighted.mean(informal, fact_cal), 3))
# Resultado: 58 · 96 · 41697 · 0.265
# (41.850 - 153 = 41.697 personas con información completa).


# B3. Modelos ------------------------------------------------------------------
# Muestra común: solo personas con educación y rama válidas, para que todos los
# modelos usen las mismas observaciones. Así, un cambio en el coeficiente se
# debe a los controles y no a un cambio de muestra.
ocupados_mod <- ocupados |>
  filter(!is.na(educacion), !is.na(rama))

nrow(ocupados_mod)
# Resultado: 41697

# Modelo base: educación y edad (con edad² por la forma de U de la informalidad
# según la edad). Ponderado con fact_cal.
m_base <- lm(informal ~ educacion + edad + I(edad^2),
             data = ocupados_mod, weights = fact_cal)

# Modelo con controles: agrega sexo, región y rama de actividad.
# La rama es el control nuevo respecto del laboratorio.
m_ctrl <- lm(informal ~ educacion + edad + I(edad^2) + mujer + region + rama,
             data = ocupados_mod, weights = fact_cal)

# Coeficientes de educación del modelo con controles, en puntos porcentuales.
tidy(m_ctrl) |>
  filter(grepl("educacion", term)) |>
  mutate(pp = round(100 * estimate, 1)) |>
  select(term, pp)
# Resultado: Básica o menos 15.9 · Técnica superior -9.1 · Universitaria o más -11.4


# B4. Extracción y comparación con broom + dplyr -------------------------------

# 1) Coeficientes de educación en ambos modelos, en pp con IC 95 %.
bind_rows(base      = tidy(m_base, conf.int = TRUE),
          controles = tidy(m_ctrl, conf.int = TRUE),
          .id = "modelo") |>                          # columna con el modelo
  filter(grepl("^educacion", term)) |>                # términos de educación
  mutate(across(c(estimate, conf.low, conf.high),
                ~ round(100 * .x, 1))) |>             # a puntos porcentuales
  select(modelo, term, estimate, conf.low, conf.high) |>
  arrange(term, modelo)
# Resultado (estimate):
#   Básica o menos       base  18.1 · controles  15.9
#   Técnica superior     base -11.0 · controles  -9.1
#   Universitaria o más  base -16.4 · controles -11.4

# 2) Ajuste de ambos modelos.
list(base = m_base, controles = m_ctrl) |>
  lapply(glance) |>
  bind_rows(.id = "modelo") |>
  select(modelo, r.squared, adj.r.squared, nobs)
# Resultado: base 0.103 / 0.102 · controles 0.137 / 0.136 · nobs 41697

# 3) Interacción educación × sexo: ¿la asociación es distinta en mujeres?
#    educacion * mujer = educacion + mujer + educacion:mujer. El término
#    educacion:mujer mide cuánto cambia la diferencia por educación en mujeres.
m_int <- lm(informal ~ educacion * mujer + edad + I(edad^2) + region + rama,
            data = ocupados_mod, weights = fact_cal)

tidy(m_int, conf.int = TRUE) |>
  filter(grepl(":mujer", term)) |>                    # términos de interacción
  mutate(across(c(estimate, conf.low, conf.high), ~ round(100 * .x, 1)),
         p.value = round(p.value, 3)) |>
  select(term, estimate, conf.low, conf.high, p.value)
# Resultado: Básica:mujer 3.2 (p 0.009) · Técnica:mujer 0.3 (p 0.814) ·
#            Universitaria:mujer -3.3 (p 0.001)


# B5. Sensibilidad: con y sin el control de rama -------------------------------
# Se estima el modelo con controles sin la rama y se compara el coeficiente de
# educación con el del modelo que sí la incluye.
m_sin_rama <- lm(informal ~ educacion + edad + I(edad^2) + mujer + region,
                 data = ocupados_mod, weights = fact_cal)

bind_rows(sin_rama = tidy(m_sin_rama),
          con_rama = tidy(m_ctrl),
          .id = "modelo") |>
  filter(grepl("^educacion", term)) |>
  mutate(pp = round(100 * estimate, 1)) |>
  select(modelo, term, pp) |>
  tidyr::pivot_wider(names_from = modelo, values_from = pp) |>  # una columna por modelo
  mutate(cambio_pp = con_rama - sin_rama)                        # variación en pp
# Resultado:
#   Básica o menos       sin_rama  18.2 · con_rama  15.9 · cambio -2.3
#   Técnica superior     sin_rama -11.6 · con_rama  -9.1 · cambio  2.5
#   Universitaria o más  sin_rama -16.7 · con_rama -11.4 · cambio  5.3


# B6. Interpretación de la Parte B ---------------------------------------------
# La informalidad promedio de los ocupados es 26,5 %. En el modelo base, a igual
# edad y frente a quien tiene educación media, tener básica o menos se asocia con
# una probabilidad de ser informal 18,1 pp mayor, y tener universitaria o más con
# una 16,4 pp menor. Al comparar personas de la misma edad, sexo, región y rama,
# estas diferencias se reducen (15,9 pp y -11,4 pp): el modelo base responde
# cuánto difieren los grupos educacionales y el modelo con controles, cuánto
# difieren personas similares. La rama es la que más mueve el resultado
# (universitaria pasa de -16,7 a -11,4 pp), lo que sugiere que parte de la
# asociación se debe a que los universitarios trabajan en ramas más formales. En
# mujeres, las diferencias por educación son mayores: universitaria se asocia con
# 3,3 pp menos de informalidad adicional y básica con 3,2 pp más; en técnica
# superior no hay diferencia por sexo. Con 41.697 observaciones casi todo es
# significativo, pero estos coeficientes además son grandes: cerca de la mitad de
# la informalidad promedio. Limitaciones: (1) los errores estándar de lm() con
# weights no consideran el diseño muestral de la ENE (estratos y conglomerados),
# por lo que los IC son más estrechos de lo que deberían; (2) es un solo
# trimestre y faltan variables que la ENE no mide (como experiencia real o
# ingreso), por lo que no es una relación causal; (3) el modelo de probabilidad
# lineal puede predecir valores bajo 0 % o sobre 100 %.


# =============================================================================
# PARTE C — Comunicar
# =============================================================================

# C1. Tabla de los modelos de la Parte B ---------------------------------------
modelos_ene <- list("Base"          = m_base,
                    "Con controles" = m_ctrl,
                    "Interacción"   = m_int)

modelsummary(modelos_ene,
             stars     = TRUE,                    # * p<0.1, ** p<0.05, *** p<0.01
             coef_omit = "region|rama",           # oculta las 35 filas de región y rama
             gof_map   = c("nobs", "r.squared"))  # n y R²
# Nota: región y rama están incluidas en los modelos; solo se ocultan de la
# tabla para mejorar su legibilidad.


# C2. Exportación a CSV --------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE)

# La exportación directa a .csv con modelsummary arrojó un error ("objeto 'd'
# no encontrado"), por lo que se hace en dos pasos: se obtiene la tabla como
# data frame y luego se guarda con write.csv().
modelsummary(modelos_ene,
             coef_omit = "region|rama",
             gof_map   = c("nobs", "r.squared"),
             output    = "data.frame") |>
  write.csv("data/processed/t3_tabla_ene.csv", row.names = FALSE)

tidy(m2, conf.int = TRUE) |>
  write.csv("data/processed/t3_coeficientes_casen.csv", row.names = FALSE)

