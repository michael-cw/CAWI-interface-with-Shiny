####################
##  Generate LOGIN code for WEB
####################
genCode<-function(data=NULL, chars=5) {
  if (!is.data.table(data)) data<-data.table(data)
  df<-copy(data)
  df<-df[,code:=sprintf("%s%s%s%s%s",
                        ifelse(sample(2,1)==1,letters[sample(1:26,1)], LETTERS[sample(1:26,1)]),
                        ifelse(sample(2,1)==2,letters[sample(1:26,1)], LETTERS[sample(1:26,1)]),
                        ifelse(sample(2,1)==1,letters[sample(1:26,1)], LETTERS[sample(1:26,1)]),
                        ifelse(sample(2,1)==2,letters[sample(1:26,1)], LETTERS[sample(1:26,1)]),
                        ifelse(sample(2,1)==1,letters[sample(1:26,1)], LETTERS[sample(1:26,1)])),
         by=Email]
  
  return(df)
}
