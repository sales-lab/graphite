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

spiaAttributes <- c("activation", "compound", "binding/association","expression", "inhibition", "activation_phosphorylation", "phosphorylation", "inhibition_phosphorylation", "inhibition_dephosphorylation", "dissociation", "dephosphorylation", "activation_dephosphorylation", "state change", "activation_indirect effect", "inhibition_ubiquination", "ubiquination", "expression_indirect effect", "inhibition_indirect effect", "repression", "dissociation_phosphorylation", "indirect effect_phosphorylation", "activation_binding/association", "indirect effect", "activation_compound", "activation_ubiquination")

spiaConv <- matrix(ncol=2, byrow=TRUE,
                   c("ACTIVATION", "activation",
                     "activation", "activation",
                     "activation_dephosphorylation", "activation_dephosphorylation",
                     "activation_indirect", "activation_indirect",
                     "activation_phosphorylation", "activation_phosphorylation",
                     "binding", "binding/association",
                     "binding/association", "binding/association",
                     "binding/association_phosphorylation", "binding/association_phosphorylation",
                     "biochemicalReaction", "activation_phosphorylation",
                     "biochemicalReaction", "activation",
                     "catalysisIn(ACTIVATION)", "activation",
                     "catalysisIn(INHIBITION)", "inhibition",
                     "catalysisOut(ACTIVATION)", "activation",
                     "catalysisOut(INHIBITION)", "inhibition",
                     "catalysisOut(INHIBITION-COMPETITIVE)", "inhibition",
                     "complexAssembly", "binding/association",
                     "compound", "compound",
                     "dephosphorylation", "dephosphorylation",
                     "dephosphorylation_inhibition", "dephosphorylation_inhibition",
                     "dissociation", "dissociation",
                     "dissociation_phosphorylation ", "dissociation_phosphorylation ",
                     "expression", "expression",
                     "expression_indirect", "expression_indirect",
                     "indirect", "indirect",
                     "indirect effect", "indirect",
                     "indirect_inhibition", "indirect_inhibition",
                     "indirect_phosphorylation", "indirect_phosphorylation",
                     "INHIBITION", "inhibition",
                     "inhibition", "inhibition",
                     "inhibition_phosphorylation", "inhibition_phosphorylation",
                     "inhibition_ubiquination", "inhibition_ubiquination",
                     "methylation", "ubiquination",
                     "missing", "activation",
                     "missing interaction", "activation",
                     "modulation(ACTIVATION)", "activation",
                     "phosphorylation", "phosphorylation",
                     "repression", "repression",
                     "state", "state",
                     "state change", "state",
                     "transportWithBiochemicalReaction", "activation_phosphorylation",
                     "ubiquination", "ubiquination"))
colnames(spiaConv) <- c("type", "spiaType")
