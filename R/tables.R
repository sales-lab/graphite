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


edgeTypes <- c("binding","catalysisIn(ACTIVATION)","catalysisOut(ACTIVATION)","catalysisOut(INHIBITION)","indirect","biochemicalReaction","complexAssembly","catalysisIn(INHIBITION)","binding/association","methylation","activation","expression","inhibition","phosphorylation","dephosphorylation","indirect effect","dissociation","ubiquination","repression","missing","state change","missing interaction","catalysisOut(INHIBITION-COMPETITIVE)","modulation(ACTIVATION)")

edgeArrows <- c("No Arrow", "Arrow","Arrow","T","No Arrow","Arrow","No Arrow","T","No Arrow","No Arrow","Arrow","Arrow","T","No Arrow","No Arrow","No Arrow","No Arrow","No Arrow","T","No Arrow","No Arrow","No Arrow","T","Arrow")

spiaAttributes <- c("activation", "compound", "binding/association", "expression", "inhibition", "activation_phosphorylation", "phosphorylation", "inhibition_phosphorylation", "inhibition_dephosphorylation", "dissociation", "dephosphorylation", "activation_dephosphorylation", "state change", "activation_indirect effect", "inhibition_ubiquination", "ubiquination", "expression_indirect effect", "inhibition_indirect effect", "repression", "dissociation_phosphorylation", "indirect effect_phosphorylation", "activation_binding/association", "indirect effect", "activation_compound", "activation_ubiquination")

spiaConv <- matrix(ncol=2, byrow=TRUE,
                   c("activation", "activation",
                     "binding", "binding/association",
                     "binding/association", "binding/association",
                     "biochemicalReaction", "activation",
                     "catalysisIn(ACTIVATION)", "activation",
                     "catalysisIn(INHIBITION)", "inhibition",
                     "catalysisOut(ACTIVATION)", "activation",
                     "catalysisOut(INHIBITION-COMPETITIVE)", "inhibition",
                     "catalysisOut(INHIBITION)", "inhibition",
                     "complexAssembly", "binding/association",
                     "dephosphorylation", "dephosphorylation",
                     "dissociation", "dissociation",
                     "expression", "expression",
                     "indirect", "indirect effect",
                     "indirect effect", "indirect effect",
                     "inhibition", "inhibition",
                     "methylation", "indirect effect",
                     "missing", "indirect effect",
                     "missing interaction", "indirect effect",
                     "modulation(ACTIVATION)", "activation",
                     "phosphorylation", "phosphorylation",
                     "repression", "repression",
                     "state change", "state change",
                     "ubiquination", "ubiquination"))
colnames(spiaConv) <- c("type", "spiaType")
