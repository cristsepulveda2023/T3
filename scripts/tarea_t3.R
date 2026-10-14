# =============================================================================
# Tarea T3 — Regresión económica: CASEN y ENE
# Autor: Cristóbal Sepúlveda
# Fecha: octubre 2026
# Qué hace: Parte A estima ecuaciones de ingreso con la CASEN (Mincer) y las
#           diagnostica. Parte B estima si la educación se asocia con la
#           probabilidad de ser informal, usando la ENE (ocupados, ponderado).
# =============================================================================

library(dplyr)
library(broom)
library(modelsummary)


# =============================================================================
# PARTE A — CASEN
# =============================================================================
# (la dejamos para después)


# =============================================================================
# PARTE B — ENE
# =============================================================================

# B1. Pregunta ----------------------------------------------------------------
# ____ (escríbela tú, con lenguaje "se asocia")


# B2. Cargar la base ----------------------------------------------------------

ene <- read.csv2("data/raw/ene-2026-06-mjj.csv")   # csv2: sep ";" y decimal ","

# ✅ Deberías ver: 97946 filas, 222 columnas
dim(ene)

# ✏️ Comprueba que fact_cal quedó como número y no como texto.
class(ene$fact_cal)   # si dijera "character", la lectura falló
# ✅ Deberías ver: "numeric"


# B2. Auditoría: solo ocupados ------------------------------------------------
# ocup_form (formal/informal) solo existe para ocupados -> filtramos primero.
# ✏️ ¿Qué valor de activ significa "ocupado"? (manual, o lab 8)
ocupados_crudo <- ene |>
  filter(activ == 1)            # 1 = ocupado (2 desocupado, 3 fuera de la FT)

# ✅ Deberías ver: 41850
nrow(ocupados_crudo)

# Auditoría ------------------------------------------------------------------
# Antes de modelar reviso cada variable: lm() trata todo como número y no avisa
# si hay códigos especiales (ej. 9 = "Sin clasificación" en educación).

# 1) Resumen en UNA fila: cuántos casos problemáticos tiene cada variable.
ocupados_crudo |>
  summarise(
    personas          = n(),
    informal_NA       = sum(is.na(ocup_form)),
    educ_sin_clasif   = sum(cine11_1d == 9),                  # código 9
    rama_sin_clasif   = sum(r_p_rev4cl_caenes == 999),        # código 999
    edad_min          = min(edad),
    edad_max          = max(edad)
  )
# ✅ Deberías ver: 41850 · 0 · 58 · 96 · 15 · 99

# 2) Distribución completa de las dos variables nuevas (las que el lab no usó).
#    count() muestra cada código y cuántas personas tiene.

ocupados_crudo |> count(cine11_1d)          # educación: 0 a 6, y el 9
ocupados_crudo |> count(r_p_rev4cl_caenes)  # rama: 1 a 21, y el 999

# Encontré que informal, edad, sexo y región están limpias. Educación tiene
# 58 casos con 9 y rama tiene 96 con 999, que significan "sin clasificación".
# Los paso a NA porque no son niveles reales; son pocos casos (<0,5 %).


# B2. Crear las variables del modelo ------------------------------------------
# Nombres para que region y rama queden como TEXTO (no como números 1 a 16).
# El orden sigue los códigos del manual: el código 1 es el primer nombre, etc.

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
    # Dependiente: TRUE si es informal. lm() la lee como 1/0 -> modelo de
    # probabilidad lineal, coeficientes en puntos porcentuales (x 100).
    informal = ocup_form == 2,
    mujer    = sexo == 2,

    # Educación en 4 niveles. El 9 ("Sin clasificación") va a NA a propósito,
    # escrito explícitamente para que no sea un NA silencioso.
    educacion = case_when(
      cine11_1d %in% 0:1 ~ "Básica o menos",
      cine11_1d == 2     ~ "Media",
      cine11_1d == 3     ~ "Técnica superior",
      cine11_1d %in% 4:6 ~ "Universitaria o más",
      cine11_1d == 9     ~ NA_character_
    ),
    # factor() con "Media" primero: el primer nivel es la categoría BASE,
    # así cada coeficiente se lee como "comparado con quien tiene media".
    educacion = factor(educacion, levels = c("Media", "Básica o menos",
                                             "Técnica superior",
                                             "Universitaria o más")),

    region = regiones[region],                        # código -> nombre
    rama   = ramas[na_if(r_p_rev4cl_caenes, 999)]     # 999 -> NA -> nombre NA
  ) |>
  select(informal, mujer, edad, educacion, region, rama, fact_cal)

# Chequeo 1: ¿cada nivel de educación tiene la cantidad esperada?
ocupados |> count(educacion)
# ✅ Deberías ver: Media 17373 · Básica o menos 9119 · Técnica superior 5624 ·
#                 Universitaria o más 9676 · NA 58

# Chequeo 2: ¿cuántos NA quedaron y cuántas personas tienen todo completo?
ocupados |>
  summarise(educ_NA      = sum(is.na(educacion)),
            rama_NA      = sum(is.na(rama)),
            completos    = sum(complete.cases(ocupados)),
            informalidad = round(weighted.mean(informal, fact_cal), 3))
# ✅ Deberías ver: 58 · 96 · 41697 · 0.265
#    (41850 - 153 = 41697: esas 153 personas las descartará lm() por tener NA)


# B3. Modelos -----------------------------------------------------------------
# Muestra común: dejo solo personas con educación y rama válidas, para que
# los DOS modelos usen exactamente las mismas personas. Si no, el cambio en
# el coeficiente podría deberse a que cambió la muestra y no a los controles.

ocupados_mod <- ocupados |>
  filter(!is.na(educacion), !is.na(rama))

nrow(ocupados_mod)
# ✅ Deberías ver: 41697

# Modelo base: educación + edad (edad^2 porque la informalidad tiene forma de U
# con la edad, como vimos en el lab). Ponderado con fact_cal.
m_base <- lm(informal ~ educacion + edad + I(edad^2),
             data = ocupados_mod, weights = fact_cal)

# Modelo con controles: agrego sexo, región y rama de actividad.
# rama es el control nuevo (el lab no la usó).
m_ctrl <- lm(informal ~ educacion + edad + I(edad^2) + mujer + region + rama,
             data = ocupados_mod, weights = fact_cal)

# Vistazo rápido: solo los coeficientes de educación, en puntos porcentuales.
tidy(m_ctrl) |>
  filter(grepl("educacion", term)) |>       # grepl: términos que contienen "educacion"
  mutate(pp = round(100 * estimate, 1)) |>  # x 100 -> puntos porcentuales
  select(term, pp)
# ✅ Deberías ver: Básica o menos 15.9 · Técnica superior -9.1 ·
#                 Universitaria o más -11.4

# B4. Extraer y comparar con broom + dplyr ------------------------------------

# 1) Coeficientes de educación en ambos modelos, lado a lado, en pp con IC 95 %.
bind_rows(base      = tidy(m_base, conf.int = TRUE),
          controles = tidy(m_ctrl, conf.int = TRUE),
          .id = "modelo") |>                       # .id: columna con el nombre del modelo
  filter(grepl("^educacion", term)) |>             # ^ = que EMPIECE con "educacion"
  mutate(across(c(estimate, conf.low, conf.high),
                ~ round(100 * .x, 1))) |>          # las tres columnas a pp
  select(modelo, term, estimate, conf.low, conf.high) |>
  arrange(term, modelo)                            # cada nivel con sus dos modelos juntos
# ✅ Deberías ver (estimate):
#    Básica o menos       base 18.1 · controles 15.9
#    Técnica superior     base -11.0 · controles -9.1
#    Universitaria o más  base -16.4 · controles -11.4

# 2) Ajuste de ambos modelos apilado.
list(base = m_base, controles = m_ctrl) |>
  lapply(glance) |>                                # glance() a cada modelo
  bind_rows(.id = "modelo") |>
  select(modelo, r.squared, adj.r.squared, nobs)
# ✅ Deberías ver: base 0.103 / 0.102 · controles 0.137 / 0.136 · nobs 41697

# 3) Interacción: ¿la asociación educación-informalidad es distinta en mujeres?
#    educacion * mujer = educacion + mujer + educacion:mujer (el término extra
#    mide cuánto CAMBIA la diferencia por educación si la persona es mujer).
m_int <- lm(informal ~ educacion * mujer + edad + I(edad^2) + region + rama,
            data = ocupados_mod, weights = fact_cal)

tidy(m_int, conf.int = TRUE) |>
  filter(grepl(":mujer", term)) |>                 # solo los términos de interacción
  mutate(across(c(estimate, conf.low, conf.high), ~ round(100 * .x, 1)),
         p.value = round(p.value, 3)) |>
  select(term, estimate, conf.low, conf.high, p.value)
# ✅ Deberías ver: Básica:mujer 3.2 (p 0.009) · Técnica:mujer 0.3 (p 0.814) ·
#                 Universitaria:mujer -3.3 (p 0.001)

# B5. Sensibilidad: con y sin el control de rama ------------------------------
# Decisión que pruebo: ¿incluir o no la rama de actividad? Estimo el modelo con
# controles SIN rama y comparo el coeficiente de educación con el que SÍ la tiene.
m_sin_rama <- lm(informal ~ educacion + edad + I(edad^2) + mujer + region,
                 data = ocupados_mod, weights = fact_cal)

bind_rows(sin_rama = tidy(m_sin_rama),
          con_rama = tidy(m_ctrl),
          .id = "modelo") |>
  filter(grepl("^educacion", term)) |>
  mutate(pp = round(100 * estimate, 1)) |>
  select(modelo, term, pp) |>
  tidyr::pivot_wider(names_from = modelo, values_from = pp) |>  # una columna por modelo
  mutate(cambio_pp = con_rama - sin_rama)                         # cuánto se mueve
# ✅ Deberías ver:
#    Básica o menos       sin_rama 18.2 · con_rama 15.9 · cambio -2.3
#    Técnica superior     sin_rama -11.6 · con_rama -9.1 · cambio  2.5
#    Universitaria o más  sin_rama -16.7 · con_rama -11.4 · cambio  5.3