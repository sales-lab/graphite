# Copyright 2011,2014-2015 Gabriele Sales <gabriele.sales@unipd.it>
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


edgeTypes <- c("binding",
               "control(In(ACTIVATION))",
               "control(indirect)",
               "control(In(INHIBITION))",
               "control(In(INHIBITION-COMPETITIVE))",
               "control(Out(ACTIVATION))",
               "control(Out(INHIBITION))",
               "control(Out(INHIBITION-COMPETITIVE))",
               "process",
               "process(activation)",
               "process(binding/association)",
               "process(BiochemicalReaction)",
               "process(dephosphorylation)",
               "process(dissociation)",
               "process(expression)",
               "process(indirect)",
               "process(indirect effect)",
               "process(inhibition)",
               "process(methylation)",
               "process(missing)",
               "process(missing interaction)",
               "process(phosphorylation)",
               "process(repression)",
               "process(state change)",
               "process(ubiquitination)")

edgeArrows <- c("No Arrow",
                "Arrow",
                "Arrow",
                "T",
                "T",
                "Arrow",
                "T",
                "T",
                "Arrow",
                "Arrow",
                "No Arrow",
                "Arrow",
                "Arrow",
                "No Arrow",
                "Arrow",
                "Arrow",
                "Arrow",
                "T",
                "Arrow",
                "No Arrow",
                "No Arrow",
                "Arrow",
                "T",
                "No Arrow",
                "Arrow")

spiaAttributes <- c("activation",
                    "compound",
                    "binding/association",
                    "expression",
                    "inhibition",
                    "activation_phosphorylation",
                    "phosphorylation",
                    "inhibition_phosphorylation",
                    "inhibition_dephosphorylation",
                    "dissociation",
                    "dephosphorylation",
                    "activation_dephosphorylation",
                    "state change",
                    "activation_indirect effect",
                    "inhibition_ubiquination",
                    "ubiquination",
                    "expression_indirect effect",
                    "inhibition_indirect effect",
                    "repression",
                    "dissociation_phosphorylation",
                    "indirect effect_phosphorylation",
                    "activation_binding/association",
                    "indirect effect",
                    "activation_compound",
                    "activation_ubiquination")

spiaConv <- matrix(ncol=2, byrow=TRUE,
                   c("binding", "binding/association",
                     "control(In(ACTIVATION))", "activation",
                     "control(indirect)", "indirect effect",
                     "control(In(INHIBITION))", "inhibition",
                     "control(In(INHIBITION-COMPETITIVE))", "inhibition",
                     "control(Out(ACTIVATION))", "activation",
                     "control(Out(INHIBITION))", "inhibition",
                     "control(Out(INHIBITION-COMPETITIVE))", "inhibition",
                     "process", "activation",
                     "process(activation)", "activation",
                     "process(binding/association)", "binding/association",
                     "process(BiochemicalReaction)", "activation",
                     "process(dephosphorylation)", "dephosphorylation",
                     "process(dissociation)", "dissociation",
                     "process(expression)", "expression",
                     "process(indirect)", "indirect effect",
                     "process(indirect effect)", "indirect effect",
                     "process(inhibition)", "inhibition",
                     "process(methylation)", "indirect effect",
                     "process(missing)", "indirect effect",
                     "process(missing interaction)", "indirect effect",
                     "process(phosphorylation)", "phosphorylation",
                     "process(repression)", "inhibition",
                     "process(state change)", "indirect effect",
                     "process(ubiquitination)", "ubiquination"))
colnames(spiaConv) <- c("type", "spiaType")
