# Copyright 2015 Gabriele Sales <gabriele.sales@unipd.it>
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

  if (species == "hsapiens")
    get(database, pos="package:graphite")@content
  else {
    f <- localArchive(species)
    if (file.exists(f))
      loadLocal(f, database)
    else
      loadRemote(species, database, f)
  }
}

pathwayDatabases <-function() {
  species <- names(.dbs)
  do.call(rbind,
    lapply(seq_along(.dbs), function(i)
      data.frame(species=species[i], database=.dbs[[i]])))
}


.server <- "http://graphiteweb.bio.unipd.it/pathways/"

.dbs <- list(
  athaliana=c("kegg", "reactome"),
  btaurus=c("kegg", "reactome"),
  celegans=c("reactome"),
  cfamiliaris=c("kegg", "reactome"),
  dmelanogaster=c("kegg", "reactome"),
  drerio=c("kegg", "reactome"),
  ecoli=c("kegg"),
  ggallus=c("kegg", "reactome"),
  hsapiens=c("biocarta", "humancyc", "kegg", "nci", "panther", "reactome"),
  mmusculus=c("kegg", "reactome"),
  rnorvegicus=c("kegg", "reactome"),
  scerevisiae=c("kegg", "reactome"),
  sscrofa=c("kegg", "reactome"),
  xlaevis=c("kegg"))

.version <- 1


localArchive <- function(species) {
  d <- archiveDir()
  paste0(d, "/", species, ".rda")
}

archiveDir <- function() {
  v <- as.character(.version)
  h <- normalizePath("~")
  d <- paste0(h, "/.graphite-bioc/", v)

  if (!file.exists(d)) {
    if (!dir.create(d, FALSE, TRUE))
      stop("error creating directory: ", d)
  }

  return(d)
}

loadLocal <- function(archive, database) {
  env <- new.env(emptyenv())
  load(archive, env)
  get(database, envir=env)
}

loadRemote <- function(species, database, archive) {
  v <- as.character(.version)
  url <- paste0(.server, "/", v, "/", species, ".rda")
  tmp <- paste0(archive, ".tmp")
  res <- download.file(url, tmp)
  if (res != 0)
    stop("cannot download pathway data")

  file.rename(tmp, archive)
  loadLocal(archive, database)
}
