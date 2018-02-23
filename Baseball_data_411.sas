libname mydata "/sscc/home/n/ngg135/assigment1/" access=readonly;

proc datasets library=mydata; 
run;
 quit;

data training;
set mydata.moneyball;
proc contents data=training;
run;

proc print data=training ;
run;


proc contents data=training;
proc contents data=testing;


*///Exploratory data analysis///;


proc corr data=traning;
with target_wins;
run;

proc means data=training ; 
run;

proc means data=training NMISS N; 
run;

ods graphics on;
proc corr data training plot matrix; 
with TARGET_WINS;
run;
ods graphics off;

proc univariate data=training;
histogram TEAM_BASERUN_CS  /normal;
run;

proc univariate data=training;
histogram TEAM_BATTING_BB /normal;
run;


proc univariate data=training normal;
    var TARGET_WINS TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BATTING_BB TEAM_BASERUN_SB TEAM_FIELDING_E TEAM_PITCHING_BB TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_BASERUN_SB; 
     histogram;
   
*///Imputing missing values///;  
data training_imp;
    set training;
    IP_TEAM_BASERUN_SB =  TEAM_BASERUN_SB;
    I_IP_TEAM_BASERUN_SB = 0;
    IP_TEAM_BASERUN_CS =  TEAM_BASERUN_CS;
    I_IP_TEAM_BASERUN_CS = 0;
    IP_TEAM_BATTING_HBP  =  TEAM_BATTING_HBP ;
    I_IP_TEAM_BATTING_HBP = 0;
     IP_TEAM_FIELDING_DP  = TEAM_FIELDING_DP ;
    I_IP_TEAM_FIELDING_DP = 0;
    label IP_TEAM_BASERUN_SB = 'impute TEAM_BASERUN_SB with mean';
    label I_IP_TEAM_BASERUN_CS = 'indicator of imputation for IP_TEAM_BASERUN_SB';
    label IP_TEAM_BASERUN_CS = 'impute TEAM_BASERUN_CS with mean';
    label I_IP_TEAM_BASERUN_SB = 'indicator of imputation for IP_TEAM_BASERUN_CS';
    label IP_TEAM_BATTING_HBP = 'impute TEAM_BATTING_HBP with mean';
    label IP_TEAM_BATTING_HBP = 'indicator of imputation for IP_TEAM_BATTING_HBP';
       label IP_TEAM_FIELDING_DP = 'impute TEAM_BATTING_SO with mean';
    label IP_TEAM_FIELDING_DP = 'indicator of imputation for IP_TEAM_BATTING_SO';
    if missing(IP_TEAM_BASERUN_SB) then do;
        IP_TEAM_BASERUN_SB = 124.761772;
        I_IP_TEAM_BASERUN_SB = 1;
    end;
    if missing (IP_TEAM_BASERUN_CS) then do;
    IP_TEAM_BASERUN_CS=52.803;
    I_IP_TEAM_BASERUN_CS=1;
    END;
     if missing (IP_TEAM_BATTING_HBP) then do;
    IP_TEAM_BATTING_HBP=59.3560209;
    I_IP_TEAM_BATTING_HBP=1;
    END;
      if missing (IP_TEAM_FIELDING_DP) then do;
    IP_TEAM_FIELDING_DP=146.387;
    I_IP_TEAM_FIELDING_DP=1;
    END;
 
 *///Outliers indicators///;
 data training_imp_o;
    set training_imp;
    if TEAM_FIELDING_E < 486 then I_TEAM_FIELDING_E = 0.0;
    else I_TEAM_FIELDING_E = 1;
    label I_TEAM_FIELDING_E = 'Outlier Indicator for Errors in all the games';

    if TEAM_PITCHING_BB < 874 then I_TEAM_PITCHING_BB = 0.0;
    else I_TEAM_PITCHING_BB = 1.0;
    label I_TEAM_PITCHING_BB = 'Outlier Indicator for Walks allowed';

    if TEAM_PITCHING_H < 2041 then I_TEAM_PITCHING_H = 0.0;
    else I_TEAM_PITCHING_H = 1.0;
    label I_TEAM_PITCHING_H = 'Outlier Indicator for Hits allowed';
 
 
 *///variable transformation///;
 
data training_imp_transform;
 set training_imp_o;
     
sqrt_IP_TEAM_BASERUN_SB = sqrt(IP_TEAM_BASERUN_SB);
log_IP_TEAM_BASERUN_SB = log(IP_TEAM_BASERUN_SB);
label log_IP_TEAM_BASERUN_SB= 'log of IP_TEAM_BASERUN_CS';
 
sqrt_TEAM_BATTING_3B = sqrt(TEAM_BATTING_3B);
log_TEAM_BATTING_3B = log(TEAM_BATTING_3B);

sqrt_TEAM_BATTING_BB = sqrt(TEAM_BATTING_BB);
log_TEAM_BATTING_BB = log(TEAM_BATTING_BB);

sqrt_TEAM_BATTING_H = sqrt(TEAM_BATTING_H);
log_TEAM_BATTING_H = log(TEAM_BATTING_H);

sqrt_TEAM_BATTING_HR = sqrt(TEAM_BATTING_HR);
log_TEAM_BATTING_HR = log(TEAM_BATTING_HR);

sqrt_TEAM_PITCHING_HR = sqrt(TEAM_PITCHING_HR);
log_TEAM_PITCHING_HR = log(TEAM_PITCHING_HR);

sqrt_IP_TEAM_BASERUN_CS = sqrt(IP_TEAM_BASERUN_CS);
log_IP_TEAM_BASERUN_CS = log(IP_TEAM_BASERUN_CS);



proc univariate data=training_imp_transform;
histogram log_IP_TEAM_BASERUN_SB /normal;
run;

proc univariate data=training_imp_transform;
histogram log_TEAM_BATTING_3B /normal;
run;
 
proc univariate data=training_imp_transform;
histogram sqrt_TEAM_BATTING_HR /normal;
run;
 

proc print data=training_imp (obs=10);
run;

*///Regression modelling///;
proc reg data=training_imp_transform;
   model TARGET_WINS = TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_BB TEAM_BATTING_HR
    TEAM_BATTING_3B   log_IP_TEAM_BASERUN_SB TEAM_PITCHING_BB TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_FIELDING_E IP_TEAM_BASERUN_CS ;



proc reg data=training_imp_transform;
   model TARGET_WINS = TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_BB 
   log_IP_TEAM_BASERUN_SB TEAM_PITCHING_HR TEAM_FIELDING_E  ;



proc reg data=training_imp_transform;
   model TARGET_WINS = TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B  TEAM_BATTING_BB TEAM_BATTING_HR
      IP_TEAM_BASERUN_SB IP_TEAM_BASERUN_CS TEAM_FIELDING_E IP_TEAM_FIELDING_DP  TEAM_PITCHING_BB TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_SO  /
  selection=adjrsq aic bic cp best=5;


proc reg data=training_imp_transform;
   model TARGET_WINS = TEAM_BATTING_H I_TEAM_FIELDING_E TEAM_BATTING_3B TEAM_BATTING_BB TEAM_BATTING_HR IP_TEAM_BASERUN_SB TEAM_FIELDING_E  IP_TEAM_FIELDING_DP TEAM_PITCHING_H I_TEAM_PITCHING_H;


     
*///Testing data///;
       
libname mydata "/sscc/home/n/ngg135/assigment1/" access=readonly;

proc datasets library=mydata; 
run;
 quit;

data testing;
set mydata.moneyball_test;
proc contents data=testing;
run;

*///Handling missing values///;

data testing_imp;
    set testing;
    IP_TEAM_BASERUN_SB =  TEAM_BASERUN_SB;
    I_IP_TEAM_BASERUN_SB = 0;
    IP_TEAM_BASERUN_CS =  TEAM_BASERUN_CS;
    I_IP_TEAM_BASERUN_CS = 0;
    IP_TEAM_BATTING_HBP  =  TEAM_BATTING_HBP ;
    I_IP_TEAM_BATTING_HBP = 0;
    IP_TEAM_FIELDING_DP  = TEAM_FIELDING_DP ;
    I_IP_TEAM_FIELDING_DP = 0;
    label IP_TEAM_BASERUN_SB = 'impute TEAM_BASERUN_SB with mean';
    label I_IP_TEAM_BASERUN_CS = 'indicator of imputation for IP_TEAM_BASERUN_SB';
    label IP_TEAM_BASERUN_CS = 'impute TEAM_BASERUN_CS with mean';
    label I_IP_TEAM_BASERUN_SB = 'indicator of imputation for IP_TEAM_BASERUN_CS';
    label IP_TEAM_BATTING_HBP = 'impute TEAM_BATTING_HBP with mean';
    label IP_TEAM_BATTING_HBP = 'indicator of imputation for IP_TEAM_BATTING_HBP';
    label IP_TEAM_FIELDING_DP = 'impute TEAM_BATTING_SO with mean';
    label IP_TEAM_FIELDING_DP = 'indicator of imputation for IP_TEAM_BATTING_SO';
    if missing(IP_TEAM_BASERUN_SB) then do;
        IP_TEAM_BASERUN_SB = 124.761772;
        I_IP_TEAM_BASERUN_SB = 1;
    end;
    if missing (IP_TEAM_BASERUN_CS) then do;
    IP_TEAM_BASERUN_CS=52.803;
    I_IP_TEAM_BASERUN_CS=1;
    END;
     if missing (IP_TEAM_BATTING_HBP) then do;
    IP_TEAM_BATTING_HBP=59.3560209;
    I_IP_TEAM_BATTING_HBP=1;
    END;
      if missing (IP_TEAM_FIELDING_DP) then do;
    IP_TEAM_FIELDING_DP=146.387;
    I_IP_TEAM_FIELDING_DP=1;
    END;
   
    data testing_imp_o;
    set testing_imp;
    if TEAM_FIELDING_E < 486 then I_TEAM_FIELDING_E = 0.0;
    else I_TEAM_FIELDING_E = 1;
    label I_TEAM_FIELDING_E = 'Outlier Indicator for Errors in all the games';

    if TEAM_PITCHING_BB < 874 then I_TEAM_PITCHING_BB = 0.0;
    else I_TEAM_PITCHING_BB = 1.0;
    label I_TEAM_PITCHING_BB = 'Outlier Indicator for Walks allowed';

    if TEAM_PITCHING_H < 2041 then I_TEAM_PITCHING_H = 0.0;
    else I_TEAM_PITCHING_H = 1.0;
    label I_TEAM_PITCHING_H = 'Outlier Indicator for Hits allowed';
   
   
   proc print data=testing_imp (obs=100);
   
*///prediction///;

 data predictions;
    set testing_imp_o;
    P_TARGET_WINS = 24.74850 + 0.04437 * TEAM_BATTING_H + 12.25772 * I_TEAM_FIELDING_E + 0.07650 * TEAM_BATTING_3B + 0.01494 * TEAM_BATTING_BB  + 0.03156  * TEAM_BATTING_HR + 0.02017 * IP_TEAM_BASERUN_SB - 0.04209 * TEAM_FIELDING_E  - 0.12840 * IP_TEAM_FIELDING_DP + 0.00031154 * TEAM_PITCHING_H + I_TEAM_PITCHING_H * 6.28084  ;
    keep index P_TARGET_WINS;
    
    
       
     data predictions_score;
   set predictions;
 
   if p_target_wins = '.' then p_target_wins=81;
   P_TARGET_WINS = round(P_TARGET_WINS,1);
   P_TARGET_WINS = min( P_TARGET_WINS, 162 );
   P_TARGET_WINS = max( P_TARGET_WINS, 0 );  
   keep index p_target_wins;
   run; 
 
 data home.final_prediction;
 set predictions_score;
 run;









