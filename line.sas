%inc 'c:\temp\WinBUGSio.sas';
data line;
  input x y;
  cards;
  1 1
  2 3
  3 3
  4 3
  5 5
  ;
  run;

%WINBUGSIO(
        dsin=line,                  /* Input dataset Specify external file within quotes */
        dir=c:\temp\,                       /* Directory path for all files       */
        datafile1=data1,                    /* Filename for column format dataset */
        datafile2=data2,                    /* Filename for constants dataset     */
        constlist=%str(N=5, xbar=3),                    /* List of constants in S-Language    */
        dsvar=%str(x, y),                       /* Input SAS variables for &datafile1 */
        dsfmt=%str(),                       /* Formats for SAS variables in dsvar list */
        varlab=%str('x[]',  'Y[]'),                     /* Variable labels for &datafile1     */
        initsfile=lineinits,                /* List of filenames for MCMC initial values */
        modelfile=linemodel,                    /* Location of WinBUGS model file     */
        burnin=100,                         /* Number of iterations used for burn-in on MCMC chain */
        samples=500,                        /* Number of samples to be taken at each node from the MCMC chain */
        nodes=%str(alpha, beta, sigma),                     /* List of model parameters           */
        codavar=%str(*),                     /* List of model parameters for iteration information */
        logfile=log,                        /* Filename for WinBUGS log file      */
        batchfile=batch,                    /* Filename for WinBUGS batch file    */
        dsout=WBoutput);                /* Output SAS dataset for node summary statistics Specify external file within quotes */
