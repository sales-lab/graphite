# Copyright 2011,2013,2015 Gabriele Sales <gabriele.sales@unipd.it>
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

setMethod("length", "PathwayList",
  function(x) length(x@entries))

setMethod("names", "PathwayList",
  function(x) names(x@entries))

setMethod("show", "PathwayList",
  function(object) {
    cat(object@name, " pathways for ", object@species, "\n",
        length(object), " entries, retrieved on ",
        prettyDate(object@timestamp), "\n",
        sep="")
  })

setMethod("$", "PathwayList",
  function(x, name) x@entries[[name]])

setMethod("[", "PathwayList",
  function(x, i, ...) {
    x@entries <- x@entries[i, ...]
    return(x)
  })

setMethod("[[", "PathwayList",
  function(x, i) x@entries[[i]])

as.list.PathwayList <- function(x, ...) as.list(x@entries, ...)


setClass("DeprecatedPathwayList",
  representation(name="character",
                 content="PathwayList"),
  contains="Pathways")

setMethod("length", "DeprecatedPathwayList",
  function(x) length(x@content))

setMethod("names", "DeprecatedPathwayList",
  function(x) names(x@content))

setMethod("show", "DeprecatedPathwayList",
  function(object) show(object@content))

setMethod("$", "DeprecatedPathwayList",
  function(x, name) {
    deprecatedObj(x@name)
    x@content[[name]]
  })

setMethod("[", "DeprecatedPathwayList",
  function(x, i, ...) {
    deprecatedObj(x@name)
    x@content[i, ...]
  })

setMethod("[[", "DeprecatedPathwayList",
  function(x, i) {
    deprecatedObj(x@name)
    x@content[[i]]
  })

as.list.DeprecatedPathwayList <- function(x, ...) {
  deprecatedObj(x@name)
  as.list(x@content, ...)
}


setClass("Pathway",
  representation(id="character",
                 title="character",
                 edges="data.frame",
                 database="character",
                 species="character",
                 identifier="vector",
                 timestamp="Date"))

setMethod("nodes", "Pathway",
  function(object) {
    es <- object@edges
    unique(c(es$src, es$dest))
  })

setMethod("edges", "Pathway",
  function(object) object@edges)

setMethod("show", "Pathway",
  function(object) {
    cat('"', object@title, '" pathway\n',
        "Native ID           = ", object@id, "\n",
        "Database            = ", object@database, "\n",
        "Species             = ", object@species, "\n",
        "Type of identifiers = ", object@identifier, "\n",
        "Number of nodes     = ", length(nodes(object)), "\n",
        "Number of edges     = ", NROW(object@edges), "\n",
        "Retrieved on        = ", prettyDate(object@timestamp), "\n",
        sep="")
  })


prettyDate <- function(d) format(d, "%d-%m-%Y")
