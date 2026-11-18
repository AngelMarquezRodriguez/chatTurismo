# Imagen ligera para la nube: solo lo que necesita el servidor del chatbot
FROM rocker/r-ver:4.3.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libpq-dev libsodium-dev zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Paquetes que usan main.R, mod5, mod6, motor_chat y conexion_bd (binarios rápidos de Posit Package Manager)
RUN R -e "install.packages(c('plumber','DBI','RPostgres','dplyr','httr','jsonlite','stringr','stringi'))"

WORKDIR /app
COPY . /app
# renv es solo para tu PC (Windows); en la nube se usan los paquetes de arriba
RUN rm -f /app/.Rprofile && rm -rf /app/renv

ENV PORT=10000
EXPOSE 10000
CMD ["Rscript", "main.R"]
