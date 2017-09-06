# Copyright 2017 Gabriele Sales <gabriele.sales@unipd.it>
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


args <- commandArgs(trailing = TRUE)
if (length(args) != 1) {
  stop("Usage: check.R PKG_TARBALL")
}

x <- rcmdcheck::rcmdcheck(path = args[1], args = "--timings")
if (length(x$errors) > 0 || length(x$warnings) > 0) {
  q(status = 1)
}
