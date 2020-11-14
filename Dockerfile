FROM rocker/r-base

MAINTAINER Christian Ballejo "cballejo@anlis.gov.ar"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'shinydashboard', 'shinydashboardPlus'), repos='https://cloud.r-project.org/')"

# install dependencies of the sala app
RUN R -e "install.packages(c('httr', 'dplyr', 'tidyr', 'stringr', 'forcats', 'tibble', 'readr', 'lubridate', 'rlang', 'tmap', 'sf', 'waiter', 'treemap', 'highcharter'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/sala
COPY euler /root/sala

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/sala')"]