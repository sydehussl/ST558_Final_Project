FROM rstudio/plumber

RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev libpng-dev pandoc

RUN R -e "install.packages(c('tidyverse', 'tidymodels', 'ranger', 'plumber'))"

COPY plumber.R plumber.R
COPY rf_mod.rds rf_mod.rds
COPY water_potability.csv water_potability.csv

EXPOSE 8000

ENTRYPOINT ["R", "-e", \
"pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"] 