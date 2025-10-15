# bootstrap
R code (with data of test) to test if mean and median of one value in one group is different than in another biggest group

only the parameters between lines 9 and 19 to modify

on line 9 modify the name of your file in repository (name of example file in code)


on line 10 put the number of the column with iddentity of group (endosperm, PEG and MEG in example)


on line 11, put the name of the bigest group used as control (Endosperm for endosperm genes in example)


on line 12, put the number of the column of the tested value (log of expression level by gene copy in example)


on line 13, put the name of the group  tested (PEG in example)


on line 15, put the size of the tested group (size of PEG+MEG in exampl)


on line 16, put a name for the analysis used for all output files (plots in svg format + summary table)


on line 17, put the number of replicate for the simulated mean and median of control group with size of tested group (1000 in example)


on line 18, put the treshold, between 0 and 1, based on distribution of simulated mean and median to consider mean and median of tested group as significantly lower than control group (0.05 in examplle)


on line 19, put the treshold, between 0 and 1, based on distribution of simulated mean and median to consider mean and median of tested group as significantly higher than control group (0.95 in examplle)
