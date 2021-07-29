files: 
03 processes WOS and Scopus 
04 joins WOS and Scopus
05 Add OA in PW, do the bootstrap runs


USED FOR MANUSCRIPT
functions_ms/AppFig3.R
functions_ms/AppFig1.R
functions_ms/DivRichCalc.R
functions_ms/Country_removal_div_fig.R
functions_ms/CountryPlot.R
functions_ms/DivBootFig_MSv2.R
functions_ms/DivBootFig_shannon.R
functions_ms/IncomePlot_MSv2.R
functions_ms/n_OA_plots.R
functions_ms/permutation_tests_OA_fig.R
functions_ms/RichBootFig_MSv2.R
functions_ms/RegionPlot_MSv2.R
functions_ms/SummaryTable.R
functions_ms/Table2.R
functions_ms/WaiverGrpPlot_MSv2.R
prep_for_sampleOAinPW.R


mirror pairs cleanup and save is duplicated in 3 and 4

used in MS

"./data_clean/MirrorPairs.csv" (03)
"./data_clean/WaiverCountries.csv"
"./data_clean/all_data_analysis.csv"
"./output/BootOAinPW_Countries.csv"
"./output/BootMirror_Countries.csv"
"./output/BootOAinPW_RichDiv.csv"
"./output/BootMirror_RichDiv.csv"
"./data_clean/stipends.csv"




used in APPENDIX
"./output/MirrorvOAinPW_permutations.csv"
"./data_clean/WaiverCountries.csv"
"./data_clean/NON_WavierCountries.csv"



USED FOR ANALYSES
source("./Rscript/functions/SubSamplePWvsOA_comparison.R")
source("./Rscript/functions/DivRichCalcTable_Solo.R")
source("./Rscript/functions/DivRichCalcSummaryTable_sampled.R")
source("./Rscript/functions_figures/Table2.R")
source("./Rscript/functions_figures/DivBootFig.R")
source("./Rscript/functions_figures/RichBootFig.R")
source("./Rscript/functions_figures/AppFig3.R") 
source("./Rscript/functions/DivRichCalc.R") 
source("./Rscript/functions/SummaryTable.R") 
source("./Rscript/functions_figures/AppFig3.R") 

source("./Rscript/functions_figures/AltFig1.R") 
source("./Rscript/functions_figures/AltFig1B.R") 
source("./Rscript/functions_figures/AltFig1_hist.R") 
source("./Rscript/functions_figures/Fig2.R") 