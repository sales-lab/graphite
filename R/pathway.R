# Copyright 2011-2017 Gabriele Sales <gabriele.sales@unipd.it>
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


setClass("Pathways", contains="VIRTUAL")


setClass("PathwayList",
  representation(name="character",
                 species="character",
                 entries="list",
                 timestamp="Date"),
  contains="Pathways")

setMethod("length", signature("PathwayList"),
  function(x) length(x@entries))

setMethod("names", signature("PathwayList"),
  function(x) names(x@entries))

setMethod("show", signature("PathwayList"),
  function(object) {
    cat(object@name, " pathways for ", object@species, "\n",
        length(object), " entries, retrieved on ",
        prettyDate(object@timestamp), "\n",
        sep="")
  })

setMethod("$", signature("PathwayList"),
  function(x, name) x@entries[[name]])

setMethod("[", signature("PathwayList"),
  function(x, i, ...) {
    x@entries <- x@entries[i, ...]
    return(x)
  })

setMethod("[[", signature("PathwayList"),
  function(x, i) x@entries[[i]])

as.list.PathwayList <- function(x, ...) as.list(x@entries, ...)


setClass("DeprecatedPathwayList",
  representation(name="character",
                 content="PathwayList"),
  contains="Pathways")

setMethod("length", signature("DeprecatedPathwayList"),
  function(x) length(x@content))

setMethod("names", signature("DeprecatedPathwayList"),
  function(x) names(x@content))

setMethod("show", signature("DeprecatedPathwayList"),
  function(object) show(object@content))

setMethod("$", signature("DeprecatedPathwayList"),
  function(x, name) {
    deprecatedObj(x@name)
    x@content[[name]]
  })

setMethod("[", signature("DeprecatedPathwayList"),
  function(x, i, ...) {
    deprecatedObj(x@name)
    x@content[i, ...]
  })

setMethod("[[", signature("DeprecatedPathwayList"),
  function(x, i) {
    deprecatedObj(x@name)
    x@content[[i]]
  })

as.list.DeprecatedPathwayList <- function(x, ...) {
  deprecatedObj(x@name)
  as.list(x@content, ...)
}


setClass("Pathway",
  representation(
    id = "character",
    title = "character",
    database = "character",
    species = "character",
    protEdges = "data.frame",
    protPropEdges = "data.frame",
    metabolEdges = "data.frame",
    metabolPropEdges = "data.frame",
    mixedEdges = "data.frame",
    timestamp = "Date"))

pathwayId <- function(p) {
  p@id
}

pathwayTitle <- function(p) {
  p@title
}

pathwayDatabase <- function(p) {
  p@database
}

pathwaySpecies <- function(p) {
  p@species
}

pathwayTimestamp <- function(p) {
  p@timestamp
}

setMethod("nodes", signature("Pathway"),
  function(object, which = c("proteins", "metabolites", "mixed")) {
    es <- edges(object, which = which, stringsAsFactors = FALSE)
    typed <- c(paste(es$src_type, es$src, sep = ":"),
               paste(es$dest_type, es$dest, sep = ":"))
    unique(typed)
  })

setMethod("edges", signature("Pathway", "character"),
  function(object, which = c("proteins", "metabolites", "mixed"),
           stringsAsFactors = TRUE) {

    es <- switch(match.arg(which),
      proteins = rbind(object@protEdges, object@protPropEdges),
      metabolites = rbind(object@metabolEdges, object@metabolPropEdges),
      mixed = rbind(object@protEdges, object@metabolEdges, object@mixedEdges),
      stop("invalid pathway variant (\"which\" parameter)")
    )

    if (!stringsAsFactors) {
      es[] <- lapply(es, as.character)
    }

    return(es)
  })

setMethod("edges", signature("Pathway", "missing"),
  function(object, stringsAsFactors = TRUE) edges(object, "proteins", stringsAsFactors))

setMethod("show", signature("Pathway"),
  function(object) {
    node_num <- length(nodes(object, "mixed"))

    edge_num <- nrow(object@protEdges) + nrow(object@protPropEdges) +
                nrow(object@metabolEdges) + nrow(object@metabolPropEdges) +
                nrow(object@mixedEdges)

    cat('"', object@title, '" pathway\n',
        "Native ID       = ", object@id, "\n",
        "Database        = ", object@database, "\n",
        "Species         = ", object@species, "\n",
        "Number of nodes = ", node_num, "\n",
        "Number of edges = ", edge_num, "\n",
        "Retrieved on    = ", prettyDate(object@timestamp), "\n",
        sep="")
  })

prettyDate <- function(d) format(d, "%d-%m-%Y")
