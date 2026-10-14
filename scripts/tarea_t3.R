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
# # A1. Dos modelos --------------------------------------------------------------
# experiencia potencial de Mincer: años desde que terminó de estudiar.
# pmax(..., 0) evita experiencias negativas (si alguien tuviera edad < educ + 6).
casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

m1 <- lm(ingreso ~ educ, data = casen)                            # simple
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)     # con controles
# lm() descarta solo las 5 filas con ingreso NA -> n = 55.

# A2. Tabla de coeficientes de m2 (sin summary, en un solo pipe) ----------------
tidy(m2, conf.int = TRUE) |>                         # coeficientes + IC 95 %
  filter(term != "(Intercept)") |>                   # sin el intercepto
  mutate(across(c(estimate, conf.low, conf.high), round),   # a pesos enteros
         p.value       = round(p.value, 4),
         significativo = p.value < 0.05) |>          # columna lógica: TRUE si p < 5 %
  select(term, estimate, conf.low, conf.high, p.value, significativo) |>
  arrange(p.value)                                   # el más significativo arriba
# ✅ Deberías ver:
#    educ          42246   29487   55004  0.0000  TRUE
#    generoM       55686  -31246  142618  0.2043  FALSE
#    experiencia    -517   -3408    2375  0.7213  FALSE

# A3. Tercer modelo: perfil cóncavo de Mincer ----------------------------------
# I(experiencia^2) permite que el ingreso suba con la experiencia pero cada vez
# menos (curva que se aplana). I() le dice a R: "calcula esto antes de modelar".
m3 <- lm(ingreso ~ educ + experiencia + I(experiencia^2) + genero, data = casen)

modelos <- list(simple = m1, controles = m2, mio = m3)
lapply(modelos, glance) |>
  bind_rows(.id = "modelo") |>
  select(modelo, r.squared, adj.r.squared, nobs)
# ✅ Deberías ver:
#    simple     0.458  0.448  55
#    controles  0.475  0.444  55
#    mio        0.481  0.440  55
#
# Al agregar variables, el R² sube pero el R² ajustado baja. Eso significa que
# la experiencia y la experiencia² no aportan a explicar el ingreso. Por lo
# tanto, mi modelo no mejora, y con solo 55 personas agregar variables
# le resta precisión al modelo sin explicar más.


# A4. ¿Cambió el coeficiente de educación? -------------------------------------
lapply(modelos, tidy) |>
  bind_rows(.id = "modelo") |>
  filter(term == "educ") |>
  mutate(estimate = round(estimate)) |>
  select(modelo, estimate, p.value)
# ✅ Deberías ver: simple 42080 · controles 42246 · mio 42690

# Dato para tu comentario:
cor(casen$educ, casen$experiencia)                          # ✅ -0.156
cor(casen$experiencia, casen$ingreso, use = "complete.obs") # ✅ -0.088
#
# El coeficiente de educación casi no cambia entre los tres modelos (42.080,
# 42.246 y 42.690). Esto pasa porque la experiencia casi no tiene relación con
# la educación (cor = -0,156) ni con el ingreso (cor = -0,088). Como no cumple
# las dos condiciones, agregarla no mueve el coeficiente de educación.

# A5. Diagnóstico con augment() ------------------------------------------------
# augment() devuelve una fila por persona usada en el modelo, con lo que el
# modelo predijo (.fitted), el error (.resid), el apalancamiento (.hat) y la
# distancia de Cook (.cooksd). .rownames = número de fila ORIGINAL en casen
# (lm() descartó las 5 filas con NA, así que la numeración ya no es 1, 2, 3...).

# 1) Las tres personas peor predichas: mayor error, sin importar el signo.
augment(m2) |>
  mutate(sector = casen$sector[as.numeric(.rownames)],  # traigo el sector desde casen
         across(c(.fitted, .resid), round)) |>
  arrange(desc(abs(.resid))) |>                         # abs(): tamaño del error
  select(.rownames, ingreso, .fitted, .resid, educ, experiencia, genero, sector) |>
  head(3)
# ✅ Deberías ver: filas 47, 51 y 40, con residuos -298210, -296393 y -249293.
#
# Los tres tienen en común que son hombres, trabajan en agricultura y tienen
# más de 30 años de experiencia. Tienen residuo negativo, o sea, ganan menos de
# lo que el modelo predice. Esto puede pasar porque m2 no incluye el sector, y
# la agricultura paga menos que otros sectores.

# 2) Máxima distancia de Cook y máximo apalancamiento.
augment(m2) |>
  summarise(max_cook = max(.cooksd),
            max_hat  = max(.hat))
# ✅ Deberías ver: max_cook 0.0869 · max_hat 0.175

# 3) Re-estimar m2 SIN la persona con mayor distancia de Cook.
#    Guardo su fila en un objeto en vez de escribir el número a mano: si los
#    datos cambian, el código sigue funcionando.
fila_cook <- augment(m2) |>
  slice_max(.cooksd, n = 1) |>      # la de mayor Cook
  pull(.rownames) |>                # saco su número de fila
  as.numeric()
fila_cook
# ✅ Deberías ver: 43

m2_sin <- casen |>
  filter(row_number() != fila_cook) |>               # todos menos esa fila
  lm(ingreso ~ educ + experiencia + genero, data = _) # _ = los datos del pipe

bind_rows(todos = tidy(m2), sin_cook = tidy(m2_sin), .id = "muestra") |>
  filter(term == "educ") |>
  mutate(estimate = round(estimate)) |>
  select(muestra, estimate, p.value)
# ✅ Deberías ver: todos 42246 · sin_cook 45218 (sube unos 3.000 pesos, un 7 %)
#
# Al sacar la persona con mayor distancia de Cook, el coeficiente de educación
# pasa de 42.246 a 45.218 y sigue siendo significativo, así que la conclusión
# no cambia. Pero que una sola persona de 55 lo mueva en un 7 % muestra que,
# con una muestra tan chica, los resultados son poco precisos y pueden
# depender de pocos casos.

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

# =============================================================================
# PARTE C — Comunicar
# =============================================================================

# C1. Tabla de los modelos de la Parte B lado a lado ---------------------------
modelos_ene <- list("Base"          = m_base,
                    "Con controles" = m_ctrl,
                    "Interacción"   = m_int)

modelsummary(modelos_ene,
             stars    = TRUE,                     # * p<0.1, ** p<0.05, *** p<0.01
             coef_omit = "region|rama",           # oculto las 35 filas de región y rama
             gof_map  = c("nobs", "r.squared"))   # solo n y R² abajo
# Nota: region y rama SÍ están en los modelos; solo las oculto para que la
# tabla sea legible. Lo dice la fila de abajo de cada modelo (nobs, R²).

# C2. Exportar a CSV -----------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE)   # crea la carpeta si no existe

# Exportar directo a .csv con modelsummary dio error ("objeto 'd' no encontrado"),
# así que lo hago en dos pasos: 1) pido la tabla como data frame,
# 2) la guardo con write.csv().
modelsummary(modelos_ene,
             coef_omit = "region|rama",
             gof_map   = c("nobs", "r.squared"),
             output    = "data.frame") |>
  write.csv("data/processed/t3_tabla_ene.csv", row.names = FALSE)

