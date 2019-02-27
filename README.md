# WinBUGSio
SAS macro for writing WinBUGS models, data and scripts, executing the model and returning output to SAS

## Abstract
This is a macro which facilitates remote execution of WinBUGS from within SAS. The
macro pre-processes data for WinBUGS, writes the WinBUGS batch-script, executes this
script and reads in output statistics from the WinBUGS log-file back into SAS native
format. The user specifies the input and output file names and directory path as well
as the statistics to be monitored in WinBUGS. The code works best for a model that
has already been set up and checked for convergence diagnostics within WinBUGS. An
obvious extension of the use of this macro is for running simulations where the input and
output files all have the same name but all that differs between simulation iterations is
the input dataset. The functionality and syntax of the macro call are described in this
paper and illustrated using a simple linear regression model.
