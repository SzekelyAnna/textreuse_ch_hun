# textreuse_ch_hun

Paper replication (draft version)

Viscosity Revisited: The Power of Legislatures in New and Old Democracies
A Comparative Text Reuse Analysis

By Miklós Sebők - Pascal Sciarini - Julien M. Jaquet - Tamás Berki – Flóra Bolonyai

Please do not quote or cite!




Workflow

Need to change gdir parameter in each R file which sets the working directory after downloading.

A. HUN bills (raw datasets can be downloaded from https://cap.tk.mta.hu/en)
	1. create Rda files from files in draft_raw and passed_clean folders with 	search_match…._new_2019_november.R 
	2. calculate dissimilarity indices from Rda files with swiss_based_dissim_hun.R

B. Swiss bills (raw datasets can be downloaded from https://cap.tk.mta.hu/en)
	1. text_pre_processing.R includes all steps starting from input bills and output bills folders

C. regression & figures
	1. regression_figures.R uses Swiss_bills_2019_10.csv and hun_bills.xlsx and input files

D. plots & figures
	1. descriptive_plots uses Swiss_bills_2019_10.csv and hun_bills.xlsx and input files
	2. appendix_figures.R uses full_data_nov19.xlsx as input file
