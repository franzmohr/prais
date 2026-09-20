#!/usr/bin/env bash
# Run the GitHub Actions checks locally, inside a Docker image.
#
# Usage:
#   .github/docker/run-checks.sh              # R CMD check, as CI runs it
#   .github/docker/run-checks.sh coverage     # also report test coverage
#
# The R version can be chosen with R_VERSION, e.g. R_VERSION=4.4.1 to
# approximate the oldrel leg of the workflow matrix.
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
r_version=${R_VERSION:-latest}
image="prais-ci:r-${r_version}"

# Git for Windows rewrites arguments that look like paths; keep them intact.
export MSYS2_ARG_CONV_EXCL='*'
if command -v cygpath >/dev/null 2>&1; then
  mount_src=$(cygpath -m "$repo_root")
else
  mount_src=$repo_root
fi

echo "==> Building $image"
docker build \
  --build-arg "R_VERSION=${r_version}" \
  -f "${mount_src}/.github/docker/Dockerfile" \
  -t "$image" \
  "$mount_src"

echo "==> Running checks in $image"
docker run --rm \
  -v "${mount_src}:/src:ro" \
  -v "${mount_src}/.github/docker:/ci:ro" \
  "$image" "$@"
