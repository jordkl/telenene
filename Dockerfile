FROM rocker/shiny:4.2.0

# Install system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libudunits2-dev \
    libgeos-dev \
    libproj-dev \
    build-essential \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install R packages (matching exact order in global.R)
RUN R -e "install.packages(c(\
    'shiny', \
    'leaflet', \
    'dplyr', \
    'DT', \
    'sf', \
    'readr', \
    'tidyr', \
    'lubridate', \
    'leaflet.extras', \
    'pryr', \
    'ggplot2', \
    'geosphere'), \
    repos = 'https://cloud.r-project.org', \
    dependencies = TRUE)"

# Create app directory structure
RUN mkdir -p /srv/shiny-server/nenemap/ui \
    && mkdir -p /srv/shiny-server/nenemap/server 

# Copy app files maintaining directory structure
COPY app.R /srv/shiny-server/nenemap/
COPY global.R /srv/shiny-server/nenemap/
COPY about.md /srv/shiny-server/nenemap/
COPY ui/* /srv/shiny-server/nenemap/ui/
COPY server/* /srv/shiny-server/nenemap/server/
COPY Rprofile.site /usr/lib/R/etc/

# Set working directory
WORKDIR /srv/shiny-server/nenemap

# Make the app available at port 3838
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/nenemap', host = '0.0.0.0', port = 3838)"]
