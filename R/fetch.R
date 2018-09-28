# Copyright 2015-2018 Gabriele Sales <gabriele.sales@unipd.it>
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
    stop("Unsupported species: ", species,
         call.=FALSE)

  if (!(database %in% ds))
    stop("No such pathway database: ", database,
         call.=FALSE)

  if (species == "hsapiens") {
    get(database, .hsapiens)
  } else {
    loadPathways(species, database)
  }
}

pathwayDatabases <-function() {
  species <- names(.dbs)
  do.call(rbind,
    lapply(seq_along(.dbs), function(i)
      data.frame(species=species[i], database=.dbs[[i]])))
}


.server <- "https://graphiteweb.bio.unipd.it/pathways/"

.dbs <- list(
  athaliana=c("kegg", "reactome", "smpdb"),
  btaurus=c("kegg", "reactome"),
  celegans=c("kegg", "reactome"),
  cfamiliaris=c("kegg", "reactome"),
  dmelanogaster=c("kegg", "reactome"),
  drerio=c("kegg", "reactome"),
  ecoli=c("kegg", "smpdb"),
  ggallus=c("kegg", "reactome"),
  hsapiens=c("biocarta", "humancyc", "kegg", "nci", "panther", "pharmgkb", "reactome", "smpdb"),
  mmusculus=c("kegg", "reactome", "smpdb"),
  rnorvegicus=c("kegg", "reactome"),
  scerevisiae=c("kegg", "reactome", "smpdb"),
  sscrofa=c("kegg", "reactome"),
  xlaevis=c("kegg"))

.version <- 9


loadPathways <- function(species, database) {
  env <- loadEnv(species)
  get(database, envir = env)
}

loadEnv <- function(name) {
  path <- archivePath(name)
  if (!file.exists(path))
    fetchRemote(name, path)

  loadLocal(path)
}

archivePath <- function(name) {
  d <- archiveDir()
  paste0(d, "/", name, ".rda")
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
  env <- new.env(emptyenv())
  load(archive, env)
  env
}

fetchRemote <- function(name, archive) {
  url <- remoteUrl(name)
  tmp <- paste0(archive, ".tmp")

  res <- try(GET(url, write_disk(tmp, overwrite = TRUE)))
  if (class(res) != "response") {
    stop("cannot download pathway data: are you offline?")
  } else if (http_status(res)$category != "Success") {
    stop("cannot download pathway data: are you using the latest graphite version?")
  } else {
    file.rename(tmp, archive)
  }
}

remoteUrl <- function(name) {
  v <- as.character(.version)
  paste0(.server, "/", v, "/", name, ".rda")
}


metabolites <- function() {
  get("table", (loadEnv("metabolites")))
}

purgeCache <- function() {
  unlink(archiveDir(), recursive = TRUE)
}
