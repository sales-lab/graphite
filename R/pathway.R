# Copyright 2011-2021 Gabriele Sales <gabriele.sales@unipd.it>
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


checkPathwayList <- function(object) {
  errors <- character()
  
  if (length(object@entries) > 0) {
    if (any(sapply(object@entries, function(p) p@species != object@species))) {
      errors <- c(errors,
                  paste("All pathways should belong to the species",
                        object@species))
    }
  }

  if (length(errors) == 0) TRUE else errors
}

setClass("PathwayList",
  representation(name="character",
                 species="character",
                 entries="list",
                 timestamp="Date"),
  contains="Pathways",
  validity = checkPathwayList)

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


pathwayURL <- function(object) {
  f <- switch(object@database,
              KEGG = keggURL,
              panther = pantherURL,
              PathBank = pathbankURL,
              PharmGKB = pharmgkbURL,
              Reactome = reactomeURL,
              SMPDB = smpdbURL,
              WikiPathways = wikipathwaysURL,
              NULL)

  if (is.null(f)) {
    NULL
  } else {
    f(object)
  }
}

keggURL <- function(p) {
  parts <- unlist(strsplit(p@id, ":", fixed = TRUE))
  paste0("http://www.kegg.jp/kegg-bin/show_pathway?org_name=", parts[1],
         "&mapno=", parts[2])
}

pantherURL <- function(p) {
  paste0("http://identifiers.org/panther.pathway/", p@id)
}

pathbankURL <- function(p) {
  paste0("http://pathbank.org/view/", p@id)
}

pharmgkbURL <- function(p) {
  paste0("https://www.pharmgkb.org/pathway/", sub("^.*\\.", "", p@id))
}

reactomeURL <- function(p) {
  paste0("http://reactome.org/PathwayBrowser/#/", p@id)
}

smpdbURL <- function(p) {
  paste0("http://smpdb.ca/pathwhiz/pathways/", sub("^.*/", "", p@id))
}

wikipathwaysURL <- function(p) {
  paste0("https://www.wikipathways.org/index.php/Pathway:", p@id)
}


setMethod("nodes", signature("Pathway"),
  function(object, which = c("proteins", "metabolites", "mixed")) {
    es <- edges(object, which = which, stringsAsFactors = FALSE)
    typed <- c(paste(es$src_type, es$src, sep = ":"),
               paste(es$dest_type, es$dest, sep = ":"))
    unique(typed)
  })

setMethod("edges", signature("Pathway"),
  function(object, which = c("proteins", "metabolites", "mixed"),
           stringsAsFactors = TRUE) {

    if (missing(which)) {
      which <- "proteins"
    } else {
      which <- match.arg(which)
    }
    es <- switch(which,
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

setMethod("show", signature("Pathway"),
  function(object) {
    node_num <- length(nodes(object, "mixed"))

    edge_num <- nrow(object@protEdges) + nrow(object@protPropEdges) +
                nrow(object@metabolEdges) + nrow(object@metabolPropEdges) +
                nrow(object@mixedEdges)

    fmt <- c('"', object@title, '" pathway\n',
             "Native ID       = ", object@id, "\n",
             "Database        = ", object@database, "\n",
             "Species         = ", object@species, "\n",
             "Number of nodes = ", node_num, "\n",
             "Number of edges = ", edge_num, "\n",
             "Retrieved on    = ", prettyDate(object@timestamp), "\n")

    url <- pathwayURL(object)
    if (!is.null(url)) {
      fmt <- c(fmt,
               "URL             = ", url, "\n")
    }

    cat(fmt, sep="")
  })

prettyDate <- function(d) format(d, "%d-%m-%Y")
