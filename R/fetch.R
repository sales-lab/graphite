# Copyright 2015-2025 Gabriele Sales <gabriele.sales@unipd.it>
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

  checkArchiveV1()
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

.version <- 24


loadData <- function(name, retry = TRUE) {
  withArchiveDir(\(dir) {
    path <- archivePath(dir, name)
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
  })
}

withArchiveDir <- function(func) {
  path <- user_cache_dir(
    "graphite-bioc2",
    "graphiteweb.bio.unipd.it",
    packageVersion("graphite"),
    opinion = FALSE
  )
  lock <- dir.expiry::lockDirectory(path)
  on.exit(dir.expiry::unlockDirectory(lock))

  if (!file.exists(path)) {
    if (!dir.create(path, showWarnings = FALSE, recursive = TRUE)) {
      stop("error creating directory: ", path)
    }
  }

  out <- func(path)
  dir.expiry::touchDirectory(path)

  out
}

archivePath <- function(dir, name) {
  paste0(dir, "/", name, ".rds")
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

purgeCache <- function() {
  withArchiveDir(\(dir) {
    archives <- list.files(dir, full.names = TRUE)
    file.remove(archives)
  })
  invisible(NULL)
}

metabolites <- function() {
  loadData("metabolites")
}

checkArchiveV1 <- function() {
  if (dir.exists(archivePathV1())) {
    lifecycle::deprecate_warn(
      when = "1.55.1",
      what = I("Local storage of pathway data using format v1"),
      details =
        c("v" = "graphite will automatically migrate to and use format v2 going forward.",
          "i" = "If you no longer need compatibility with older graphite versions, run purgeCacheV1() to reclaim disk space."),
      env = rlang::caller_env(),
      user_env = rlang::caller_env(2)
    )
  }
}

archivePathV1 <- function() {
  user_cache_dir("graphite-bioc", "graphiteweb.bio.unipd.it")
}

purgeCacheV1 <- function() {
  unlink(archivePathV1(), recursive = TRUE)
}
