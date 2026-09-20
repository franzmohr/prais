#!/usr/bin/env bash
# Entry point of the CI container: copies the mounted sources into a writable
# working directory and runs the same checks as the GitHub Actions workflows.
#
#   check.sh            R CMD check, as the R-CMD-check workflow runs it
#   check.sh coverage   additionally report test coverage, as test-coverage does
set -euo pipefail

mkdir -p /work/pkg
cp -a /src/. /work/pkg
rm -rf /work/pkg/.git /work/pkg/.Rproj.user /work/pkg/.claude

cd /work

Rscript -e '
  res <- rcmdcheck::rcmdcheck(
    "pkg",
    args = c("--no-manual", "--as-cran"),
    build_args = c("--no-manual", "--compact-vignettes=gs+qpdf"),
    check_dir = "check",
    error_on = "warning"
  )
'

if [ "${1:-}" = "coverage" ]; then
  Rscript -e '
    cov <- covr::package_coverage("pkg", quiet = FALSE, clean = FALSE)
    print(cov)
  '
fi
