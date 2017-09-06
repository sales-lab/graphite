# Copyright 2017 Gabriele Sales <gabriele.sales@unipd.it>
#
#
# This file is part of graphite.
#
# graphite is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License
# version 3 as published by the Free Software Foundation.
#
# graphite is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with graphite. If not, see <http://www.gnu.org/licenses/>.


cache <- paste(getwd(), "cache", sep = "/")
if (!dir.exists(cache)) {
  stop("cache directory does not exist")
} else if (!(cache %in% .libPaths())) {
  stop("cache directory is not in the library paths")
}

pkgs <- c("devtools", "rcmdcheck", "XMLRPC")
for (pkg in pkgs) {
  if (!suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE))) {
    install.packages(pkg)
  }
}

library(BiocInstaller)
biocLite(NULL, ask = FALSE)

pkgs <- c("clipper", "DEGraph", "SPIA", "topologyGSA")
for (pkg in pkgs) {
  if (!suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE))) {
    biocLite(pkg, ask = FALSE)
  }
}

devtools::install_dev_deps()
