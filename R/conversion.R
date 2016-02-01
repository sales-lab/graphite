# Copyright 2011,2015,2016 Gabriele Sales <gabriele.sales@unipd.it>
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


setGeneric("convertIdentifiers",
  function(x, to) standardGeneric("convertIdentifiers"))


setMethod("convertIdentifiers", "PathwayList",
  function(x, to) {
    x@entries <- lapply(x@entries,
                   function(p) convertIdentifiers(p, to))
    return(x)
  })

setMethod("convertIdentifiers", "DeprecatedPathwayList",
  function(x, to) {
    deprecatedObj(x@name)
    convertIdentifiers(x@content, to)
  })

setMethod("convertIdentifiers", "Pathway",
  function(x, to) {

    db <- loadDb(x@species)
    to <- destIdentifier(to, db)

    if (x@identifier != to) {

      cn <- colnames(x@edges)

      es <- convert(db, x@edges, "src", x@identifier, to)
      es <- convert(db, es, "dest", x@identifier, to)

      x@edges <- es[, cn]
      x@identifier <- to

      if (nrow(x@edges) == 0)
        warning("the conversion lost all edges of pathway \"", x@title, "\"")
    }

    return(x)
  })

loadDb <- function(species) {
  db <- selectDb(species)

  tryCatch(get(db),
    error=function(e) {
      if (!suppressPackageStartupMessages(require(db, character.only=TRUE)))
        stop("package \"", db, "\" is missing", call.=FALSE)
      get(db)
    })
}

destIdentifier <- function(to, db) {

  if (to == "entrez")
    to <- "ENTREZID"
  else if (to == "symbol")
    to <- "SYMBOL"

  if (!(to %in% columns(db)))
    stop(to, " is not supported with this species.",
         call.=FALSE)

  return(to)
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
    stop("no such species")

  return(n)
}

convert <- function(db, edges, colname, from, to) {
  keys <- unique(edges[[colname]])
  tbl <- lookupKeys(db, keys, from, to)

  if (is.null(tbl)) {
    edges[0,]
  } else {
    tbl <- tbl[,c(from, to)]
    colnames(tbl) <- c(colname, "converted")
    edges <- merge(edges, tbl)
    replaceColumn(colname, "converted", edges)
  }
}

lookupKeys <- function(db, keys, from, to)
  tryCatch(
    na.omit(suppressWarnings(select(db, keys, to, from))),
    error=function(e) NULL)

replaceColumn <- function(old, new, df) {
  df <- df[, !colnames(df)==old, drop=FALSE]
  colnames(df)[colnames(df)==new] <- old
  return(df)
}
