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


nodeShape <- literalDataFrame(c("type", "shape"), c(
  "CAS", "ELLIPSE",
  "CHEBI", "ELLIPSE",
  "ENSEMBL", "ROUND_RECTANGLE",
  "ENSEMBLPROT", "ROUND_RECTANGLE",
  "ENTREZID", "ROUND_RECTANGLE",
  "FLYBASECG", "ROUND_RECTANGLE",
  "KEGGCOMP", "ELLIPSE",
  "KEGGDRUG", "ELLIPSE",
  "KEGGGLYCAN", "ELLIPSE",
  "ORF", "ROUND_RECTANGLE",
  "PUBCHEM", "ELLIPSE",
  "TAIR", "ROUND_RECTANGLE",
  "UNIPROT", "ROUND_RECTANGLE"
))


edgeInfo <- literalDataFrame(c("type", "arrow", "spiaType"), c(
  "Binding", "NONE", "binding/association",
  "Control(In: ACTIVATION of BiochemicalReaction)", "ARROW", "activation",
  "Control(In: ACTIVATION of ComplexAssembly)", "ARROW", "activation",
  "Control(In: ACTIVATION of Degradation)", "ARROW", "activation",
  "Control(In: ACTIVATION of Transport)", "ARROW", "activation",
  "Control(In: INHIBITION of BiochemicalReaction)", "T", "inhibition",
  "Control(In: INHIBITION of ComplexAssembly)", "T", "inhibition",
  "Control(In: INHIBITION of Transport)", "T", "inhibition",
  "Control(In: INHIBITION-COMPETITIVE of BiochemicalReaction)", "T", "inhibition",
  "Control(In: unknown of BiochemicalReaction)", "ARROW", "indirect effect",
  "Control(In: unknown of Process)", "ARROW", "indirect effect",
  "Control(In: unknown of Transport)", "ARROW", "activation",
  "Control(Out: ACTIVATION of ACTIVATION)", "ARROW", "activation",
  "Control(Out: ACTIVATION of BiochemicalReaction)", "ARROW", "activation",
  "Control(Out: ACTIVATION of ComplexAssembly)", "ARROW", "activation",
  "Control(Out: ACTIVATION of Process)", "ARROW", "activation",
  "Control(Out: ACTIVATION of TemplateReaction)", "ARROW", "activation",
  "Control(Out: ACTIVATION of Transport)", "ARROW", "activation",
  "Control(Out: ACTIVATION-ALLOSTERIC of ACTIVATION)", "ARROW", "activation",
  "Control(Out: INHIBITION of ACTIVATION)", "T", "inhibition",
  "Control(Out: INHIBITION of BiochemicalReaction)", "T", "inhibition",
  "Control(Out: INHIBITION of ComplexAssembly)", "T", "inhibition",
  "Control(Out: INHIBITION of Process)", "T", "inhibition",
  "Control(Out: INHIBITION of TemplateReaction)", "T", "inhibition",
  "Control(Out: INHIBITION of Transport)", "T", "inhibition",
  "Control(Out: INHIBITION-COMPETITIVE of ACTIVATION)", "T", "inhibition",
  "Control(Out: INHIBITION-COMPETITIVE of BiochemicalReaction)", "T", "inhibition",
  "Control(Out: INHIBITION-NONCOMPETITIVE of ACTIVATION)", "T", "inhibition",
  "Control(Out: INHIBITION-OTHER of ACTIVATION)", "T", "inhibition",
  "Control(Out: unknown of BiochemicalReaction)", "ARROW", "indirect effect",
  "Control(Out: unknown of Process)", "ARROW", "indirect effect",
  "Control(Out: unknown of Transport)", "ARROW", "indirect effect",
  "Control(indirect)", "ARROW", "indirect effect",
  "Process", "ARROW", "activation",
  "Process(BiochemicalReaction)", "ARROW", "activation",
  "Process(activation)", "ARROW", "activation",
  "Process(binding/association)", "NONE", "binding/association",
  "Process(demethylation)", "ARROW", "state change",
  "Process(dephosphorylation)", "ARROW", "dephosphorylation",
  "Process(dissociation)", "NONE", "dissociation",
  "Process(expression)", "ARROW", "expression",
  "Process(glycosylation)", "ARROW", "glycosylation",
  "Process(indirect effect)", "NONE", "indirect effect",
  "Process(indirect)", "ARROW", "indirect effect",
  "Process(inhibition)", "T", "inhibition",
  "Process(methylation)", "ARROW", "methylation",
  "Process(missing interaction)", "NONE", "indirect effect",
  "Process(missing)", "NONE", "indirect effect",
  "Process(phosphorylation)", "ARROW", "phosphorylation",
  "Process(repression)", "T", "inhibition",
  "Process(state change)", "NONE", "indirect effect",
  "Process(ubiquitination)", "ARROW", "ubiquitination"
))



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
