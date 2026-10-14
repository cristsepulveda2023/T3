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

# ✏️ Anota aquí con tus palabras qué encontraste y qué harás con el 9 y el 999:
# ____


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
