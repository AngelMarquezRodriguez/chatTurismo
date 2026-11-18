# Wayra v2 – Archivos nuevos y modificados

Copia estos archivos sobre tu proyecto (respeta las carpetas).

| Archivo | Estado | Qué hace |
|---|---|---|
| `.Renviron.example` → `.Renviron` | NUEVO | Credenciales fuera del código + clave de Claude |
| `.gitignore` | NUEVO | Evita subir secretos |
| `sql/migracion_v2.sql` | NUEVO | Nuevas columnas (ejecutar 1 vez en Supabase > SQL Editor) |
| `R_scripts/conexion_bd.R` | MODIFICADO | Lee credenciales de `.Renviron` |
| `R_scripts/librerias.R` | MODIFICADO | Lista completa de paquetes |
| `R_scripts/mod1_crawling_scraping.R` | MODIFICADO | ~45 destinos, Wikipedia + Wikivoyage, imágenes, enlaces reales |
| `R_scripts/mod3_contenido_pln_clustering.R` | MODIFICADO | Clústeres con nombres reales, NER-LOC (spaCy opcional), sentimiento con negación y alertas |
| `R_scripts/mod5_mineria_uso_logs.R` | MODIFICADO | Logs con sesión/canal/intención + ranking en vivo |
| `R_scripts/motor_chat.R` | NUEVO | Conversación: intenciones, memoria, RAG, API de Claude, respaldo local |
| `R_scripts/mod6_servidor_api.R` | MODIFICADO | API POST /chat, /ranking, /dashboard, /recargar, /health |
| `main.R` | MODIFICADO | Arranque |
| `web/index.html` | MODIFICADO | Chat con voz (micrófono + lectura en voz alta), tarjetas con imagen, ranking en vivo |

`mod2` y `mod4` no cambian.

## Pasos
1. Cambia la contraseña de Supabase (la anterior estaba dentro del ZIP) y llena `.Renviron`.
2. Ejecuta `sql/migracion_v2.sql` en Supabase.
3. `source("R_scripts/librerias.R")` (con renv: luego `renv::snapshot()`).
4. En `main.R` pon `RECALCULAR_MINERIA <- TRUE` una vez (si usas Windows, ejecuta mod1 → mod2 → mod3 → mod4) y luego vuelve a `FALSE`.
5. `Rscript main.R` y abre `web/index.html` en **Chrome o Edge** (el micrófono no funciona en Firefox).

La clave `ANTHROPIC_API_KEY` es opcional: sin ella el chatbot usa el motor local.

---

# Wayra v3 – Solo turismo en Bolivia, tono humano y multi-idioma

| Archivo | Cambio |
|---|---|
| `R_scripts/motor_chat.R` | Límite de tema, detección de idioma ES/EN, respuestas más humanas, prompt de Claude reescrito |
| `R_scripts/mod6_servidor_api.R` | `/chat` recibe `idioma` (POST y GET) |
| `web/index.html` | Selector 🇧🇴 Español / 🇺🇸 English, interfaz traducida, tarjetas sin jerga, voz en el idioma elegido |

## 1. Límite de tema (solo turismo en Bolivia)
Cada mensaje pasa por `evaluar_tema()` antes de responder:
- **Rechazo directo**: matemática (2+2), programación, política/economía, "ignora tus instrucciones / actúa como…", otros países sin mencionar Bolivia, etc.
- **Aceptado**: saludos/gracias, lugares bolivianos, vocabulario de turismo (ES/EN), o seguimiento de la conversación.
- **Duda** (sin evidencia clara): si hay `ANTHROPIC_API_KEY`, Claude decide (responde `[FUERA_DE_TEMA]` si no es turismo); si no hay clave, se rechaza.
- Respuesta de rechazo: "Lo siento, solo puedo ayudarte con turismo en Bolivia…" + una pregunta de ejemplo (en el idioma del turista).
- Las consultas rechazadas se guardan en los logs con intención `fuera_tema` (útil para la minería de uso) y **no** entran al historial que se envía a Claude.
- Para ampliar/ajustar: edita `PAT_PROHIBIDO`, `PAT_TURISMO` y `PAT_EXTRANJERO` en `motor_chat.R`.

## 2. Respuestas humanas y claras
- Prompt de Claude: tono de amigo local, frases cortas, explica palabras como *soroche* o *salteña*, no usa tecnicismos (PageRank, K-Means…), no inventa precios/horarios.
- Motor local: intro cálida, 2 frases limpias (sin paréntesis ni referencias de Wikipedia), consejo de alertas y pregunta de cierre.
- Las tarjetas muestran "⭐ Popularidad X/10" en vez de PageRank/Authority (los datos técnicos siguen en la pestaña Minería).

## 3. Idiomas
- Botones **Español / English** arriba. Se recuerda la elección; por defecto usa el idioma del navegador.
- Si el turista escribe en otro idioma que el elegido, el bot responde en el idioma del mensaje y la interfaz se ajusta sola.
- Con Claude activo, cualquier otro idioma (portugués, francés…) también funciona en la respuesta.
- Modo local en inglés: busca el resumen en inglés del mismo artículo vía Wikipedia (enlaces interlingüe); si no hay conexión, muestra el texto en español con una nota.
- **Agregar un idioma** (ej. portugués): añade un bloque en `TXT` y en `ALERTAS_*` de `motor_chat.R`, su código en `IDIOMAS_SOPORTADOS`, un bloque en `I18N` de `index.html` y un botón en `.lang-toggle`.

## Otros arreglos
- Botón "Nueva" duplicado en el chat (había dos que hacían lo mismo).
- `geocodificar_entidades`: usaba un vector como patrón en `grepl` (solo miraba el primer elemento).
- Preguntas por categoría ("¿qué se come?") ya no arrastran el destino anterior.
