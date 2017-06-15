# Copyright 2016 Gabriele Sales <gabriele.sales@unipd.it>
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


buildPathway <- function(id, title, edges, species, database, identifier,
                         timestamp=NULL) {
  checkTitle(title)
  edges <- fixEdges(edges)

  if (is.null(timestamp)) {
    timestamp <- Sys.Date()
  }

  new("Pathway",
      id = id,
      title = title,
      edges = edges,
      species = species,
      database = database,
      identifier = identifier,
      timestamp = timestamp)
}

checkTitle <- function(title) {
  if (length(title) == 0)
    stop("pathway title is required")
}

fixEdges <- function(edges) {
  if (!is.data.frame(edges) ||
      colnames(edges) != c("src", "dest", "direction", "type")) {
    stop("edges must be a data.frame with the following columns: src, dest, direction and type.")
  }

  edges <- data.frame(lapply(edges, as.character),
                      stringsAsFactors=FALSE)

  if (!all(edges[,"direction"] %in% c("directed", "undirected"))) {
    stop("edge direction must be one of: directed, undirected")
  }

  return(edges)
}
