# Copyright 2015-2022 Gabriele Sales <gabriele.sales@unipd.it>
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


pathways <- function(species, database) {
  ds <- .dbs[[species]]
  if (is.null(ds))
    stop("unsupported species: ", species,
         call.=FALSE)

  if (!(database %in% ds))
    stop("no database \"", database, "\" for species \"", species, "\"",
         call.=FALSE)

  loadData(paste(species, database, sep = "-"))
}

pathwayDatabases <-function() {
  species <- names(.dbs)
  do.call(rbind,
    lapply(seq_along(.dbs), function(i)
      data.frame(species=species[i], database=.dbs[[i]])))
}


.server <- "https://graphiteweb.bio.unipd.it/pathways"

.dbs <- list(
  athaliana=c("kegg", "pathbank", "wikipathways"),
  btaurus=c("kegg", "pathbank", "reactome", "wikipathways"),
  celegans=c("kegg", "pathbank", "reactome", "wikipathways"),
  cfamiliaris=c("kegg", "reactome", "wikipathways"),
  dmelanogaster=c("kegg", "pathbank", "reactome", "wikipathways"),
  drerio=c("kegg", "reactome", "wikipathways"),
  ecoli=c("kegg", "pathbank", "wikipathways"),
  ggallus=c("kegg", "reactome", "wikipathways"),
  hsapiens=c("kegg", "panther", "pathbank", "pharmgkb", "reactome", "smpdb", "wikipathways"),
  mmusculus=c("kegg", "pathbank", "reactome", "wikipathways"),
  rnorvegicus=c("kegg", "pathbank", "reactome", "wikipathways"),
  scerevisiae=c("kegg", "pathbank", "reactome", "wikipathways"),
  sscrofa=c("kegg", "reactome", "wikipathways"),
  xlaevis=c("kegg"))

.version <- 20


loadData <- function(name, retry = TRUE) {
  path <- archivePath(name)
  if (!file.exists(path)) {
    fetchRemote(name, path)
  }

  ps <- loadLocal(path)
  if (!is.null(ps)) {
    ps
  } else {
    if (!retry) {
      stop("Error loading pathway data. Please retry the operation at a later time.")
    } else {
      unlink(path)
      loadData(name, FALSE)
    }
  }
}

archivePath <- function(name) {
  d <- archiveDir()
  paste0(d, "/", name, ".rds")
}

archiveDir <- function() {
  d <- user_cache_dir("graphite-bioc", "graphiteweb.bio.unipd.it",
                      as.character(.version))

  if (!file.exists(d)) {
    if (!dir.create(d, FALSE, TRUE))
      stop("error creating directory: ", d)
  }

  return(d)
}

loadLocal <- function(archive) {
  res <- try(readRDS(archive), silent = TRUE)
  if (is(res, "try-error")) NULL else res
}

fetchRemote <- function(name, archive) {
  url <- remoteUrl(name)
  tmp <- paste0(archive, ".tmp")

  res <- try(GET(url, write_disk(tmp, overwrite = TRUE)))
  if (!is(res, "response")) {
    stop("cannot download pathway data: are you offline?")
  } else if (http_status(res)$category != "Success") {
    stop("cannot download pathway data: are you using the latest graphite version?")
  } else {
    file.rename(tmp, archive)
  }
}

remoteUrl <- function(name) {
  v <- as.character(.version)
  paste0(.server, "/", v, "/", name, ".rds")
}


metabolites <- function() {
  loadData("metabolites")
}

purgeCache <- function() {
  unlink(archiveDir(), recursive = TRUE)
}
