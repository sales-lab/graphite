# Copyright 2011 Gabriele Sales <gabriele.sales@unipd.it>
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


prepareSPIA <- function(db, pathwaySetName, print.names=FALSE) {
  path.info <- Filter(Negate(is.null), lapply(db, function(p) {
    if (print.names)
      cat(p@title, "\n")

    p <- convertIdentifiers(p, "entrez")
    es <- edges(p)
    if (NROW(es) == 0 || NROW(unique(es[,1:2])) < 5)
      return(NULL)

    ns <- nodes(p)
    es <- merge(es, spiaConv, all.x=TRUE)[c("src", "dest", "direction", "spiaType")]

    l <- sapply(spiaAttributes,
                simplify=FALSE,
                USE.NAMES=TRUE,
                function(edgeType) {
                  est <- es[es[,4]==edgeType, , drop=FALSE]
                  gnl <- buildGraphNEL(ns, est, TRUE)
                  t(as(gnl, "matrix"))
                })

    l$title             <- p@title
    l$nodes             <- ns
    l$NumberOfReactions <- 0

    return(l)
  }))

  save(path.info, file=datasetName(pathwaySetName))
}

runSPIA <- function(de, all, pathwaySetName, ...) {
  if (!require(SPIA))
    stop("library SPIA is missing")

  checkPkgVersion("SPIA", "2.2")

  if (!(datasetName(pathwaySetName) %in% dir()))
    stop("There is no dataset corresponding to the pathway set name: ", pathwaySetName, "\n",
         "Did you forget to run prepareSPIA?")

  optArgs <- list(...)
  if (!is.null(optArgs$organism))
    warning("Ignoring the \"organism\" parameter.")
  optArgs$organism <- pathwaySetName

  spiaEnv <- environment(spia)
  fakeEnv <- new.env(parent=spiaEnv)
  assign("system.file", fakeSystemFile, fakeEnv)

  tryCatch({
    environment(spia) <- fakeEnv
    do.call(spia, c(list(de, all), optArgs))[,c(-2,-12)]
  }, finally={
    environment(spia) <- spiaEnv
  })
}

datasetName <- function(pathwaySetName) {
  paste(pathwaySetName, "SPIA.RData", sep="")
}

fakeSystemFile <- function(name, package=NULL, ...) {
  if (package == "SPIA" && name=="extdata")
    getwd()
  else
    base::system.file(name, package=package, ...)
}
