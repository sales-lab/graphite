# Copyright 2011-2018 Gabriele Sales <gabriele.sales@unipd.it>
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


setClass("MetaboliteDb", representation(table = "data.frame"))

setMethod("columns", signature("MetaboliteDb"), function(x) colnames(x@table))

setMethod("mapIds", signature("MetaboliteDb"),
  function(x, keys, column, keytype, ..., multiVals) {
    stopifnot(multiVals == "list")

    table <- selectConvColumns(keytype, column, x@table)
    lapply(keys, function(k) {
      where <- which(table$source == k)
      targets <- unique(na.omit(table[where, "target"]))
      if (length(targets) == 0) NA else targets
    })
  })

selectConvColumns <- function(from, to, table) {
  if (!all(c(from, to) %in% colnames(table))) {
    stop("Invalid conversion requested.")
  }

  sel <- table[, c(from, to)]
  colnames(sel) <- c("source", "target")
  sel
}


setGeneric("convertIdentifiers",
  function(x, to) standardGeneric("convertIdentifiers"))


setMethod("convertIdentifiers", "PathwayList",
  function(x, to) {
    species <- x@species

    cl <- parallelCluster(x@entries, "psock")
    if (!is.null(cl)) {
      on.exit(parallel::stopCluster(cl), add = TRUE)

      parallel::clusterExport(cl, "species", envir = environment())
      parallel::clusterEvalQ(cl, {
        dbs <- graphite:::loadDbs(species)
        convertCluster <- function(x, to) graphite:::convertWithDbs(x, to, dbs)
      })
      conv <- function(elts) parallel::parLapply(cl, elts, quote(convertCluster), to)

    } else {
      dbs <- loadDbs(species)
      conv <- function(elts) lapply(elts, convertWithDbs, to, dbs)
    }

    x@entries <- conv(x@entries)
    return(x)
  })

setMethod("convertIdentifiers", "Pathway", function(x, to) {
  dbs <- loadDbs(x@species)
  convertWithDbs(x, to, dbs)
})

convertWithDbs <- function(x, to, dbs) {
  mapping <- selectMapping(to, dbs)

  x@protEdges <- convertEdges(x@protEdges, mapping)
  x@protPropEdges <- convertEdges(x@protPropEdges, mapping)
  x@metabolEdges <- convertEdges(x@metabolEdges, mapping)
  x@metabolPropEdges <- convertEdges(x@metabolPropEdges, mapping)
  x@mixedEdges <- convertEdges(x@mixedEdges, mapping)

  if (nrow(x@protEdges) + nrow(x@protPropEdges) + nrow(x@metabolEdges) +
      nrow(x@metabolPropEdges) + nrow(x@mixedEdges) == 0) {
    warning("the conversion lost all edges of pathway \"", x@title, "\"")
  }

  return(x)
}

loadDbs <- function(species) {
  proteinDb <- loadProteinDb(species)
  metabolDb <- loadMetaboliteDb()
  list(proteinDb, metabolDb)
}

loadProteinDb <- function(species) {
  db <- selectDb(species)

  tryCatch(get(db),
    error=function(e) {
      if (!suppressPackageStartupMessages(require(db, character.only=TRUE)))
        stop("package \"", db, "\" is missing", call.=FALSE)
      get(db)
    })
}

selectDb <- function(species) {
  l <- list(athaliana="org.At.tair.db",
            btaurus="org.Bt.eg.db",
            celegans="org.Ce.eg.db",
            cfamiliaris="org.Cf.eg.db",
            dmelanogaster="org.Dm.eg.db",
            drerio="org.Dr.eg.db",
            ecoli="org.EcK12.eg.db",
            ggallus="org.Gg.eg.db",
            hsapiens="org.Hs.eg.db",
            mmusculus="org.Mm.eg.db",
            rnorvegicus="org.Rn.eg.db",
            scerevisiae="org.Sc.sgd.db",
            sscrofa="org.Ss.eg.db",
            xlaevis="org.Xl.eg.db")

  n <- l[[species]]
  if (is.null(n))
    stop("unsupported species")

  return(n)
}

loadMetaboliteDb <- function() {
  new("MetaboliteDb", table = metabolites())
}

selectMapping <- function(to, dbs) {
  if (to == "entrez")
    to <- "ENTREZID"
  else if (to == "symbol")
    to <- "SYMBOL"

  for (db in dbs) {
    if (checkIdentifier(to, db)) {
      return(list(to = to, db = db))
    }
  }

  stop(to, " is not supported in this species")
}

checkIdentifier <- function(id, db) id %in% columns(db)

convertEdges <- function(edges, mapping) {
  convertSide(convertSide(edges, "src", mapping),
              "dest", mapping)
}

convertSide <- function(edges, column, mapping) {
  if (nrow(edges) == 0) {
    return(edges)
  }

  typeColumn <- paste0(column, "_type")
  parts <- nameLapply(splitByType(edges, typeColumn),
                      convertColumn(edges, column, typeColumn, mapping))

  merged <- do.call(rbind.data.frame, parts)
  row.names(merged) <- NULL
  return(merged)
}

splitByType <- function(edges, typeColumn) {
  split(seq.int(nrow(edges)), edges[typeColumn], drop = TRUE)
}

convertColumn <- function(edges, column, typeColumn, mapping) {
  function(type, ixs) {
    if (type == mapping$to || !checkIdentifier(type, mapping$db)) {
      return(edges[ixs,])
    }

    converted <- lookupKeys(mapping, edges[ixs, column], type)
    if (is.null(converted)) {
      return(edges[0,])
    }

    runLen <- sapply(converted, length)
    extended <- data.frame(lapply(edges[ixs,], rep.int, runLen),
                           stringsAsFactors=FALSE)
    extended[column] <- unlist(converted)
    extended[typeColumn] <- factor(mapping$to)
    na.omit(extended)
  }
}

lookupKeys <- function(mapping, keys, from)
  tryCatch(mapIds(mapping$db, keys, mapping$to, from, multiVals="list"),
           error=function(e) NULL)
