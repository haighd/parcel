FROM rocker/r-ver:4.1.2
RUN apt-get update && apt-get install -y  git-core libcairo2-dev libcurl4-openssl-dev libgit2-dev libgl1-mesa-dev libglu1-mesa-dev libicu-dev libpng-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.1.6")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "0.4.12")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.5.2")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("tidyselect",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.7")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.0")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.3")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("htmlwidgets",upgrade="never", version = "1.5.4")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("here",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.14.2")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2")'
RUN Rscript -e 'remotes::install_version("varhandle",upgrade="never", version = "2.0.5")'
RUN Rscript -e 'remotes::install_version("tippy",upgrade="never", version = "0.1.0")'
RUN Rscript -e 'remotes::install_version("thinkr",upgrade="never", version = "0.15")'
RUN Rscript -e 'remotes::install_version("sortable",upgrade="never", version = "0.4.4")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("shinydashboardPlus",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("readxl",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("gbp",upgrade="never", version = "0.1.0.4")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.20")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');parcel::run_app()"]
