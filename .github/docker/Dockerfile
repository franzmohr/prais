# Mirrors the ubuntu-latest leg of .github/workflows/R-CMD-check.yaml so the
# same checks can be run locally before anything is pushed to GitHub.
#
# The build context is the package root, but only DESCRIPTION is sent (see
# .dockerignore), so the dependency layer is rebuilt only when DESCRIPTION
# changes. The sources and check.sh are bind-mounted when the container runs.

ARG R_VERSION=latest
FROM rocker/r-ver:${R_VERSION}

# pandoc builds the vignette; ghostscript and qpdf back --compact-vignettes=gs+qpdf.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        pandoc \
        ghostscript \
        qpdf \
        git \
    && rm -rf /var/lib/apt/lists/*

# pak resolves and installs the package dependencies, including their system
# requirements, the same way r-lib/actions/setup-r-dependencies@v2 does on CI.
RUN R -q -e 'install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.version$os, R.version$arch))'

COPY DESCRIPTION /tmp/pkg/DESCRIPTION
RUN R -q -e 'pak::local_install_dev_deps("/tmp/pkg", dependencies = TRUE)' \
    && R -q -e 'pak::pkg_install(c("rcmdcheck", "covr", "xml2"))' \
    && rm -rf /tmp/pkg

ENV R_KEEP_PKG_SOURCE=yes \
    _R_CHECK_CRAN_INCOMING_REMOTE_=false

WORKDIR /work
ENTRYPOINT ["bash", "/ci/check.sh"]
