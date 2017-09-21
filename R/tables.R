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
  "Binding", "No Arrow", "binding/association",
  "Control(In: ACTIVATION of BiochemicalReaction)", "Arrow", "activation",
  "Control(In: ACTIVATION of ComplexAssembly)", "Arrow", "activation",
  "Control(In: ACTIVATION of Degradation)", "Arrow", "activation",
  "Control(In: ACTIVATION of Transport)", "Arrow", "activation",
  "Control(In: INHIBITION of BiochemicalReaction)", "T", "inhibition",
  "Control(In: INHIBITION of ComplexAssembly)", "T", "inhibition",
  "Control(In: INHIBITION of Transport)", "T", "inhibition",
  "Control(In: INHIBITION-COMPETITIVE of BiochemicalReaction)", "T", "inhibition",
  "Control(In: unknown of BiochemicalReaction)", "Arrow", "indirect effect",
  "Control(In: unknown of Process)", "Arrow", "indirect effect",
  "Control(In: unknown of Transport)", "Arrow", "activation",
  "Control(Out: ACTIVATION of ACTIVATION)", "Arrow", "activation",
  "Control(Out: ACTIVATION of BiochemicalReaction)", "Arrow", "activation",
  "Control(Out: ACTIVATION of ComplexAssembly)", "Arrow", "activation",
  "Control(Out: ACTIVATION of Process)", "Arrow", "activation",
  "Control(Out: ACTIVATION of TemplateReaction)", "Arrow", "activation",
  "Control(Out: ACTIVATION of Transport)", "Arrow", "activation",
  "Control(Out: ACTIVATION-ALLOSTERIC of ACTIVATION)", "Arrow", "activation",
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
  "Control(Out: unknown of BiochemicalReaction)", "Arrow", "indirect effect",
  "Control(Out: unknown of Process)", "Arrow", "indirect effect",
  "Control(Out: unknown of Transport)", "Arrow", "indirect effect",
  "Control(indirect)", "Arrow", "indirect effect",
  "Process", "Arrow", "activation",
  "Process(BiochemicalReaction)", "Arrow", "activation",
  "Process(activation)", "Arrow", "activation",
  "Process(binding/association)", "No Arrow", "binding/association",
  "Process(demethylation)", "Arrow", "state change",
  "Process(dephosphorylation)", "Arrow", "dephosphorylation",
  "Process(dissociation)", "No Arrow", "dissociation",
  "Process(expression)", "Arrow", "expression",
  "Process(glycosylation)", "Arrow", "glycosylation",
  "Process(indirect effect)", "No Arrow", "indirect effect",
  "Process(indirect)", "Arrow", "indirect effect",
  "Process(inhibition)", "T", "inhibition",
  "Process(methylation)", "Arrow", "methylation",
  "Process(missing interaction)", "No Arrow", "indirect effect",
  "Process(missing)", "No Arrow", "indirect effect",
  "Process(phosphorylation)", "Arrow", "phosphorylation",
  "Process(repression)", "T", "inhibition",
  "Process(state change)", "No Arrow", "indirect effect",
  "Process(ubiquitination)", "Arrow", "ubiquitination"
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
