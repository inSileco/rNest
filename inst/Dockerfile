# Run rNest on R 3.2.0, with devtools.
#
# Build from the package root:
#   docker build -f inst/Dockerfile -t rnest .
#
# Run R in the container:
#   docker run --rm -it rnest
#
# The rNest() GUI uses tcltk, so it needs an X server. On Linux:
#   xhost +local:docker
#   docker run --rm -it -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix rnest
#
# rocker/r-ver:3.2.0 is Debian jessie, whose packages now live on
# archive.debian.org. Its MRAN snapshot is gone, so R packages are installed
# from the CRAN archive, pinned to versions available in mid-2015.
FROM rocker/r-ver:3.2.0

RUN echo 'deb http://archive.debian.org/debian jessie main' > /etc/apt/sources.list \
  && echo 'deb http://archive.debian.org/debian-security jessie/updates main' \
    >> /etc/apt/sources.list \
  && echo 'Acquire::Check-Valid-Until "false";' > /etc/apt/apt.conf.d/99archive \
  && apt-get update \
  && apt-get install -y --force-yes --no-install-recommends \
    tcl8.5 tk8.5 \
    libx11-6 libsm6 libxt6 libxext6 libxrender1 libxss1 libxft2 \
    libgdal-dev libgeos-dev libproj-dev \
    libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev \
    make gcc g++ gfortran locales \
  && echo 'en_US.ISO-8859-1 ISO-8859-1' >> /etc/locale.gen \
  && locale-gen \
  && rm -rf /var/lib/apt/lists/*

# Pinned CRAN archive versions, listed in installation (dependency) order.
RUN echo 'options(repos = c(CRAN = "https://cran.r-project.org"))' \
    > /usr/local/lib/R/etc/Rprofile.site \
  && for pkg in \
    Rcpp_0.11.6 \
    plyr_1.8.2 \
    reshape2_1.4.1 \
    chron_2.3-45 \
    data.table_1.9.4 \
    sp_1.1-0 \
    rgdal_0.9-3 \
    rgeos_0.3-11 \
    maptools_0.8-36 \
    maps_2.3-9 \
    mapdata_2.2-3 \
    splancs_2.01-37 \
    svMisc_0.9-70 \
    RColorBrewer_1.1-2 \
    yaml_2.1.13 \
    digest_0.6.8 \
    jsonlite_0.9.16 \
    mime_0.3 \
    R6_2.0.1 \
    bitops_1.0-6 \
    RCurl_1.95-4.6 \
    httr_0.6.1 \
    memoise_0.2.1 \
    whisker_0.3-2 \
    evaluate_0.7 \
    rstudioapi_0.3.1 \
    git2r_0.10.1 \
    brew_1.0-6 \
    roxygen2_4.1.1 \
    XML_3.98-1.1 \
    rversions_1.0.0 \
    devtools_1.8.0 \
  ; do \
    name=${pkg%%_*}; \
    curl -fsSL -o /tmp/$pkg.tar.gz \
      https://cran.r-project.org/src/contrib/Archive/$name/$pkg.tar.gz \
    && R CMD INSTALL /tmp/$pkg.tar.gz \
    && rm /tmp/$pkg.tar.gz \
    || exit 1; \
  done

# The sources are encoded in latin1 (they were written on Windows), including
# some identifiers, so they must be parsed in a latin1 locale.
COPY . /tmp/rNest
RUN LC_ALL=en_US.ISO-8859-1 R CMD INSTALL /tmp/rNest && rm -rf /tmp/rNest

CMD ["R"]
