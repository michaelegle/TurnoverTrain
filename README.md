# TurnoverTrain
In this repository is the final 2021 Big Data Bowl submission for the Turnover Train (Max Bolger, Michael Egle, and Conor McQuiston). There are three major files in the repository:

# tCPOE_calcs.Rmd
This is the file used to create the Targeted Completion Percentage Over Expectation for each player. It creates the secondary.csv file, which contains the tCPOE value for each player with at least 20 targets in the 2018 season. This minimum target number can easily be altered in the code. This file and the db_sep.R file can be run in either order.

# db_sep.R
This is the script used to create the Excpected Completion Percentage per Frame (xComp) for each defender. It creates the xcomp_vals.csv file which contains the xComp (as well as averages of several other values which are used in the model) for every player which was covering someone for at least a single frame. A minimum filter can easily be altered in the code. This file and tCPOE_calcs.Rmd can be run in either order. 

# Notes
- Any questions regarding the code can be shared with any of the three authors listed above, we're all available on Twitter (Max at [@mnpykings](https://twitter.com/mnpykings), Michael at [@deceptivespeed](https://twitter.com/deceptivespeed_), and Conor at [@ConorMcQ5](https://twitter.com/ConorMcQ5)).
