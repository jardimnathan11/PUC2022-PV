FROM rocker/tidyverse:4

RUN R -e "install.packages('tidyquant')"
RUN R -e "install.packages('PerformanceAnalytics')"
RUN R -e "install.packages('writexl')"

WORKDIR /data

### run 
#COPY ./projeto_corrup.R /projeto_corrup.R
#COPY ./output /output
## copy

## install R-packages
#RUN Rscript ./projeto_corrup.R
