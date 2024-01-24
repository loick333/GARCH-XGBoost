I have provided an R Markdown file with all the code that I used, including comments.

The following R files are also provided and each can run  in the following order.

1. data_manipulation.R
2. msGARCH.R
3. rugarch.R
4. xgboost.R
5. VaR_Valculations.R
6. xgboost_VaR.R
7. analysis.R
8. violations.R


Since the VaR calculations take a while to run, I have provided a set of .csv files which were the output from the first 6 steps, which is then used at step 7 and 8. Those files are multi_VaR.csv, simulation_df and violation_analysis.csv. Simply download them, and the code will import them automatically when run. The files should be run in that order. 

