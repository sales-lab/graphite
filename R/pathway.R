# Copyright 2011,2013 Gabriele Sales <gabriele.sales@unipd.it>
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

setClass("pathway",
         representation(title="vector",
                        nodes="vector",
                        edges="data.frame",
                        ident="vector",
                        database="vector",
                        species="vector",
                        timestamp="Date"))

setMethod("show", signature(object="pathway"),
          function(object) {
            cat(paste('"', object@title, '" pathway from ', object@database, "\n",
                      "Number of nodes     = ", length(object@nodes), "\n",
                      "Number of edges     = ", NROW(object@edges), "\n",
                      "Type of identifiers = ", object@ident, "\n",
                      "Retrieved on        = ", object@timestamp, "\n", 
                      sep=""))
          })

setMethod("nodes", signature(object="pathway"), function(object) object@nodes)

setMethod("edges", signature(object="pathway"), function(object) object@edges)
