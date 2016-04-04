# Copyright 2011,2015-2016 Gabriele Sales <gabriele.sales@unipd.it>
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
    checkIdentifier(x@identifier, db)
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

checkIdentifier <- function(id, db) {
  if (!(id %in% columns(db)))
    stop(id, " is not supported in this species")
}

destIdentifier <- function(to, db) {

  if (to == "entrez")
    to <- "ENTREZID"
  else if (to == "symbol")
    to <- "SYMBOL"

  checkIdentifier(to, db)
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
    stop("unsupported species")

  return(n)
}

convert <- function(db, edges, colname, from, to) {

  converted <- lookupKeys(db, edges[[colname]], from, to)
  if (is.null(converted))
    return(edges[0,])

  runLen <- sapply(converted, length)
  extended <- data.frame(lapply(edges, function(x) rep.int(x, runLen)),
                         stringsAsFactors=FALSE)
  extended[colname] <- unlist(converted)
  na.omit(extended)
}

lookupKeys <- function(db, keys, from, to)
  tryCatch(mapIds(db, keys, to, from, multiVals="list"),
           error=function(e) NULL)
