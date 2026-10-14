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
