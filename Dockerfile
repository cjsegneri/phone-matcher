FROM cjsegneri/phone-matcher

MAINTAINER Connor Segneri (connorsegneri@gmail.com)

# install R package dependencies
RUN apt-get update && apt-get install -y \
    ##### ADD YOUR DEPENDENCIES
    ## clean up
    && apt-get clean \ 
    && rm -rf /var/lib/apt/lists/ \ 
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    ##### ADD YOUR CRAN PACKAGES
    && Rscript -e "install.packages(c('shiny', 'rmarkdown', 'DT', 'shinyjs', 'shinythemes')" \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## assume shiny app is in build folder /shiny
COPY ./myapp/ /srv/shiny-server/myapp/