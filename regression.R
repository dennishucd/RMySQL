#! /usr/bin/env Rscript
library("DBI");
library("RMySQL");

argv<-commandArgs(TRUE);
arg_wsguid<-argv[1];
arg_csecguid<-argv[2]
arg_pcode<-argv[3];
arg_predict_days<-7;
if(length(argv)>3){
  arg_predict_days<-argv[4];
}

# GI TEST
arg_wsguid<-'4bb3bce8bee24c6b947b433e9c42';
arg_csecguid<-'68980539-9a65-4a32-b36b-5e69869be36e';
arg_pcode<-'GD_00';

arg_codetype<-substr(arg_pcode,1,2);
arg_surveytype<-'';

MySQL_GROUP<-'RF7';

RegAnalysis_NLS<-function(t,u){
  errcnt<<-0;
  weight = as.data.frame(cbind(t,u),row.names = c('t','u'));
  a<-1;
  b<-0.1;
  l<-1;
  fo.sigma<-100000;
  print("--> Try u ~ A*exp(-B/t)");
  tryCatch({
    tmpnls<-nls(u ~ A*exp(-B/t), start=list(A=a, B=-b),data=weight);
    summary(tmpnls);
    if(fo.sigma > summary(tmpnls)$sigma){
      fo.sigma<-summary(tmpnls)$sigma;
      fo.sol<-tmpnls;
    }
  }
  ,warning = function(e) { print("warning on u ~ A*exp(-B/t)"); print(e);}
  ,error = function(e) { print("error on u ~ A*exp(-B/t)"); print(e);errcnt<<-errcnt+1;print(errcnt);}
  );
  print("--> Try u ~ A*log10(L+t)+B");
  tryCatch({
    tmpnls<-nls(u ~ A*log10(L+t)+B , start=list(A=a, B=b,L=l),data=weight);    
    if(fo.sigma > summary(tmpnls)$sigma){
      fo.sigma<-summary(tmpnls)$sigma;
      fo.sol<-tmpnls;
    }
  }
  , warning = function(e) { print("warning on u ~ A*log10(L+t)+B"); print(e) }
  , error = function(e) { print("error on u ~ A*log10(L+t)+B"); print(e);errcnt<<-errcnt+1;print(errcnt);}
  );
  print("--> Try u ~ t/(A+B*t)");
  tryCatch({
    tmpnls<-nls(u ~ t/(A+B*t), start=list(A=a, B=b),data=weight);
    summary(tmpnls);
    if(fo.sigma > summary(tmpnls)$sigma){
      fo.sigma<-summary(tmpnls)$sigma;
      fo.sol<-tmpnls;
    }
  }
  , warning = function(e) { print("warning on u ~ t/(A+B*t)"); print(e) }
  , error = function(e) {print("error on u ~ t/(A+B*t)"); print(e);errcnt<<-errcnt+1;print(errcnt);}
  );
  # print(errcnt);
  if(errcnt==3){return(NULL);}
  fo.summary<-summary(fo.sol);  
  fo.sol;
}



sql <- paste("SELECT  wsguid, csecguid, pcode, date(surveytime) as survey_date, AVG(coordinatez) AS coordinatez FROM ws01_survey_detail WHERE datastatus = 1 AND wsguid = '"
             , arg_wsguid
             , "' AND csecguid = '"
             , arg_csecguid
             , "' AND pcode = '"
             , arg_pcode
             , "' GROUP BY date(surveytime) ORDER BY date(surveytime)"
             , sep = "");

con <- dbConnect(dbDriver("MySQL")
                 , group=MySQL_GROUP);
sql_pre<-"SET NAMES utf8";
dbGetQuery(con,sql_pre);
rows_init<-dbGetQuery(con,sql);
dbDisconnect(con);
if(nrow(rows_init)<3) {
  print("Too few records for calculation!");  
} else {
  dt<-as.POSIXlt(rows_init[,4]);
  len<-length(dt);
  diffdate<-diff(dt)+1;
  m<-matrix(1,len-1,len-1);
  m[upper.tri(m)]=0;
  accumudays<-rbind(1,as.array(m %*% diffdate));
  #accumudays<-rbind(1,accumudays);
  x<-rows_init[,5];
  #x<-x-min(x);
  
  predictdates<-c(dt[-len], seq(from=dt[len],by="1 day", length.out=arg_predict_days));
  t_predict<-c(accumudays,(seq(from=accumudays[len-1],by=1,length.out=arg_predict_days))[-1]);
  fo.sol<-RegAnalysis_NLS(accumudays,x);
  if((exists("fo.sol")==T) & (is.null(fo.sol)==F)){
    fo.summary<-summary(fo.sol);
    fo.params<-fo.summary$parameter[,1];
    fo.formula<-gsub(pattern = "A", round(fo.params[[1]], digits = 4), fo.summary$formula[3]);
    fo.formula<-gsub(pattern = "B", round(fo.params[2], digits = 4), fo.formula);
    if(length(fo.params)==3){
      fo.formula<-gsub(pattern = "L", round(fo.params[3], digits = 4), fo.formula);
    }
    fo.formula<-paste("u = ", fo.formula);
    print(fo.formula);
    predictrslt<-predict(fo.sol,newdata = list(t = as.matrix(t_predict)));
    jsonstr<-paste('{"predictdays":',arg_predict_days,
                 ',"datearray":["',paste(predictdates,collapse='","'),'"]',
                 ',"timespan":["', paste(t_predict,collapse='","'),'"]',
                 ',"actualarray":[',paste(x,collapse=","),']',
                 ',"predictarray":[',paste(predictrslt,collapse=","),']}'               
                 ,sep="");  
  }
  #plot(dt, x, type = "l", col = "red");
  #lines(predictdates,predictrslt, col = "green");
  sql<- paste("CALL UPSERT_WS01_PREDICT(\'",arg_wsguid,"\',\'",arg_csecguid,"\','",arg_pcode
              ,"\',\'",arg_codetype
              ,"\',\'",arg_surveytype
              ,"\',\'",fo.formula
              ,"\',\'", jsonstr
              ,"')", sep = "");
  con <- dbConnect(dbDriver("MySQL")
                   , group=MySQL_GROUP);
  dbGetQuery(con,sql);
  dbDisconnect(con);  
}
