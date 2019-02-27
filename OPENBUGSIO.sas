***************************************************************************
*                                                                         *
*     Program Name       : openbugsio.sas                             *
*                                                                         *
*     System             : Windows SAS v9.2                               *
*                                                                         *
*     Program's function : SAS code for running OPENBUGS and handling I/O  *
*                          with SAS.                                      *
*                                                                         *
*     Assumptions        : Valid model file is available check this using *              
*                          scripts within OPENBUGS first                   *
*                                                                         *
*     Programmers Name   : Mike K Smith PGRD Sandwich                     *
*                          Helen Richardson PGRD Sandwich                 *
*						   Jane Temple PhD placement student			  *
*                                                                         *
*     Contact Details    : Mike.K.Smith@Pfizer.Com                        *
*                                                                         *
*                                                                         *
*    Program history:                                                     *
*    ================                                                     *
*-------------+--------------------+--------------------------------------*
*      Date   |        Name        |          Reason/Comments             *
*-------------+--------------------+--------------------------------------*
* 15-Oct-10   | Jane Temple		   | Using copy of WINBUGSIO.sas and      *
*			  |					   | adapting for OPENBUGS				  *
*-------------+--------------------+--------------------------------------*
***************************************************************************;

options pagesize=32767 nodate nocenter nonumber formdlim=' ' mprint nosymbolgen nomlogic;
title1 ' ';

%macro OPENBUGSIO(
        dsin=%str(WBinput),				    /* Input dataset Specify external file within quotes */                    
		dir=c:/program files/winbugs14/,	/* Directory path for all files       */
        datafile1=data1,					/* Filename for column format dataset */
		datafile2=data2,					/* Filename for constants dataset     */
		constlist=%str(),					/* List of constants in S-Language    */
		dsvar=%str(),						/* Input SAS variables for &datafile1 */
		dsfmt=%str(),                       /* Formats for SAS variables in dsvar list */
		varlab=%str(),						/* Variable labels for &datafile1     */ 
		initsfile=%str(inits),				/* List of filenames for MCMC initial values */
	    modelfile=model,					/* Location of WinBUGS model file     */
		burnin=100,							/* Number of iterations used for burn-in on MCMC chain */
		samples=500,						/* Number of samples to be taken at each node from the MCMC chain */
		nodes=%str(),						/* List of model parameters           */
		codavar=%str(),						/* List of model parameters for iteration information */
        logfile=log,						/* Filename for WinBUGS log file      */
        batchfile=batch,					/* Filename for WinBUGS batch file    */
		dsout=%str(WBoutput));			    /* Output SAS dataset for node summary statistics Specify external file within quotes */

*************************************************************;
**  Separate parameters in macro variable DSVAR and VARLAB **;
**  Print out data in column format in &DATAFILE1 file     **;
*************************************************************;
%if "&dsvar" ne "" and %bquote(%upcase(&dsvar)) ne EXTERNAL %then %do;
   * split DSVAR *; 
   %let i=0;
   %do %until (&&dsvar&i=%str());
      %let i=%eval(&i+1);
      %let dsvar&i=%cmpres(%qscan(&dsvar,&i,","));
   %end;
   %if &&dsvar&i=%str() %then %let dsvartot=%eval(&i-1);

   * split DSFMT if available*;
   %if "&dsfmt" ne "" %then %do;
      %let i=0;
      %do %until (&&dsfmt&i=%str());
         %let i=%eval(&i+1);
         %let dsfmt&i=%cmpres(%qscan(&dsfmt,&i,","));
      %end;
      %if &&dsfmt&i=%str() %then %let dsfmttot=%eval(&i-1);
      * Check the number of formats match the number of variables defined *;
      * if not then do not use format information                         *;
      %if &dsfmttot^=&dsvartot %then %do;
          %let dsfmt=%str();
	      %put ERROR: The number of formats declared in DSFMT (&dsfmttot) does not match the number of variables declared in DSVAR (&dsvartot). The DSFMT formats will be ignored;
      %end;
	%end;

   * Split VARLAB *;
   %let i=0;
   %do %until (&&varlab&i=%str());
      %let i=%eval(&i+1);
      %let j=%eval((2*&i)-1);
      %let varlab&i=%cmpres(%qscan(&varlab,&j,"'"));
   %end;
   %if &&varlab&i=%str() %then %let varlabtot=%eval(&i-1);

   * Output DATFILE1 in column format *;
   filename data1 "&dir.&datafile1..txt";
   data _null_;
      set &dsin end=eof;
      file "&dir.&datafile1..txt";
      if _n_=1 then do;
         put %do i=1 %to &varlabtot; "&&varlab&i" +2 %end;;
      end;
      %if "&dsfmt" ne "" %then put %do i=1 %to &dsvartot; &&dsvar&i &&dsfmt&i +2 %end;
      %else put %do i=1 %to &dsvartot; &&dsvar&i best12. +2 %end;;
      if eof then do;
         put 'END';
      end;
      run;
%end;

*************************************************************;
**  Print out constants in list format in &DATAFILE2 file  **;
*************************************************************;
%if "&constlist" ne "" and %bquote(%upcase(&constlist)) ne EXTERNAL  %then %do;
   filename data2 "&dir.&datafile2..txt";
   data _null_;
      file data2;
      put @1 "list(";
      put @1 "&constlist";
      put @1 ")";
   run;
   proc printto; run;
%end;

*************************************************************;
** User can specify an external file for the column data   **;
** and/or the constant data.  This file can be stored in   **;
** the &dir directory or in a different directory. Users   **;
** must specify file extension eg. .dat                    **;
** The &dsvar and/or &constlist should take value of       **;
** EXTERNAL                                                **;
** Check the files exist and generate flags                **;
*************************************************************;
data _null_;
   %if %bquote(%upcase(&dsvar)) eq EXTERNAL %then %do;
      %if %index(&datafile1,\)>0 or %index(&datafile1,/)>0 %then %let dirsp1=1;
      %else %let dirsp1=0;
      %if &dirsp1=1 %then call symput('dataflag1',fileexist("&datafile1"));
      %else call symput('dataflag1',fileexist("&dir.&datafile1"));;
   %end;
   %if %bquote(%upcase(&constlist)) eq EXTERNAL %then %do;
      %if %index(&datafile2,\)>0 or %index(&datafile2,/)>0 %then %let dirsp2=1;
      %else %let dirsp2=0;
      %if &dirsp2=1 %then call symput('dataflag2',fileexist("&datafile2"));
      %else call symput('dataflag2',fileexist("&dir.&datafile2"));;
   %end;
   run;

*************************************************************;
**  Separate parameters in macro variable INITSFILE        **;
**  Derive number of files in macro variable INITSFILE     **;
*************************************************************;
%let i=0;
%do %until (&&file&i=%str());
   %let i=%eval(&i+1);
   %let file&i=%cmpres(%qscan(&initsfile,&i,","));
%end;
%if &&file&i=%str() %then %let filetot=%eval(&i-1);

*************************************************************;
**  Separate parameters in macro variable NODE             **;
*************************************************************;
%if "&nodes" ne "" %then %do;
   %let i=0;
   %do %until (&&node&i=%str());
      %let i=%eval(&i+1);
      %let node&i=%cmpres(%qscan(&nodes,&i,","));
   %end;
   %if &&node&i=%str() %then %let nodetot=%eval(&i-1);
%end;

*************************************************************;
**  Separate parameters in macro variable CODAVAR          **;
**  Check that the parameters specified in CODAVAR are a   **;
**  subset of NODES                                       **;
**  If * is specified then splitting is not required       **;
*************************************************************;
%let codaerr=N;
%if "&codavar" ne "" and "&codavar" ne "*" %then %do;
   %let i=0;
   %do %until (&&codavar&i=%str());
      %let i=%eval(&i+1);
      %let codavar&i=%cmpres(%qscan(&codavar,&i,","));
	  %if &&codavar&i^= %then %do;
	     %let chk=0;
	     %do j=1 %to &nodetot;
		    %if %lowcase(&&node&j)=%lowcase(&&codavar&i) %then %let chk=1;
		 %end;
         %if &chk=0 %then %do;
	        %put ERROR: &&codavar&i not declared in nodes macro variable (&nodes).  Coda commands will not be generated in the batch file;
		    %let codaerr=Y;
	     %end;
	  %end;
   %end;
   %if &&codavar&i=%str() %then %let codavartot=%eval(&i-1);
%end;

*************************************************************;
**  Write batch file                                       **;
*************************************************************;
*************************************************************;
**  Reformat directory separators to be / for WinBUGS and \ for SAS;
*************************************************************;
%let wbdir=%sysfunc(translate(&dir,'/','\'));

filename fileout2 "c:\program files\OpenBugs\&batchfile..txt";
data _null_;
   file fileout2;
   put@1 "modelDisplay('log')";
   put@1 "modelCheck('&wbdir.&modelfile..txt')";
   %if %bquote(%upcase(&dsvar)) eq EXTERNAL %then %do;
     %if &dirsp1=0 and &dataflag1 %then put@1 "modelData('&wbdir.&datafile1')";
     %else %if &dirsp1=1 and &dataflag1 %then put@1 "modelData('&datafile1')";;
   %end;
   %else %if %bquote(%upcase(&dsvar)) ne EXTERNAL  and "&dsvar" ne "" %then %do;
     put@1 "modelData('&wbdir.&datafile1..txt')";
   %end;
   %if %bquote(%upcase(&constlist)) eq EXTERNAL %then %do;
     %if &dirsp2=0 and &dataflag2 %then put@1 "modelData('&wbdir.&datafile2')";
     %else %if &dirsp2=1 and &dataflag2 %then put@1 "modelData('&datafile2')";;
   %end;
   %else %if %bquote(%upcase(&constlist)) ne EXTERNAL  and "&constlist" ne "" %then %do;
     put@1 "modelData('&wbdir.&datafile2..txt')";
   %end;
   put@1 "modelCompile(&filetot)";
   %do i=1 %to &filetot;
      put@1 "modelInits('&wbdir.&&file&i...txt',&i)";
   %end;
   put@1 "modelGenInits()";
   put@1 "modelUpdate(&burnin)";
   %if "&nodes" ne "" %then %do;
      %do i=1 %to &nodetot;
         put@1 "samplesSet(&&node&i)";
      %end;
   %end;
   put@1 "modelUpdate(&samples)";
   put@1 "samplesStats('*')";
   put@1 "modelSaveLog('&wbdir.&logfile..txt')";
   %if "&codavar" eq "*" %then %do;
      put@1 "samplesCoda('*','&wbdir.coda')";
   %end;
   %else %if "&codavar" ne "" and "&codaerr" ne "Y" %then %do;
      %do i=1 %to &codavartot;
         put@1 "samplesCoda('&&codavar&i','&wbdir.&&codavar&i')";
      %end;
   %end;
   put@1 "modelDisplay('window')";
   put@1 "modelQuit('Yes')";
run;
proc printto; run;

************************************************;
** Execute the OpenBUGS run in batch mode        ;
************************************************;
options xmin noxwait;

x cd c:\program files\OpenBugs;
x winbugs/PAR &batchfile..txt /HEADLESS;


************************************************;
** Read in log file of batch run which includes ;
** stats for Gibbs Sampler                      ;
** Calculate new priors for the mean parameters ;
**  mu[1] and mu[2].  Using tau=1/variance      ;
************************************************;
data _null_;
  retain i j 0;
  infile "&dir.&logfile..txt" expandtabs truncover;
  length text $200;
  input text $ 1-200;
  if scan(text,1)="Node" then i=_n_;
  call symput("i",i+2);
  run;

data &dsout;
  infile "&dir&logfile..txt" firstobs=&i expandtabs truncover;
  length var $20;
  input var $ mean se MCMCer lowCI median uppCI start samp;
  run;

*************************************************************;
**  Read in iteration files for parameter nodes and create **;
**  work datasets                                          **;
*************************************************************;
%if "&codavar" ne "" and "&codaerr" ne "Y" %then %do;
   %if "&codavar" eq "*" %then %do;
      %let codavartot=1;
      %let codavar1=codaCODA;
   %end;
%do i=1 %to &codavartot;
   data index;
      infile "&dir.&&codavar&i..index.txt" expandtabs truncover;
      input parameter $ start stop;
	  parm=translate(parameter,'_','[]');
	  run;

   data _null_;
	  set index end=last;
      call symput("parm"||left(_n_),parm);
      call symput("start"||left(_n_),start);
      call symput("stop"||left(_n_),stop);
      if last then call symput("total",_n_);
   run;
   
   proc datasets;
      delete index coda;
	  quit;
   run;

   %do j=1 %to &total;
      data &&parm&j;
         infile "&dir.&&codavar&i..chain1.txt" expandtabs truncover firstobs=&&start&j obs=&&stop&j;
         input iteration value ;
	     parameter="&&parm&j";
      run;
      %if "&codavar" eq "*" %then %do;
         proc append base=codaCODAchain data=&&parm&j;
         run;

    	 proc datasets;
	        delete &&parm&j;
			quit;
	     run;
	  %end;
   %end;
%end;
%end;

%mend;
