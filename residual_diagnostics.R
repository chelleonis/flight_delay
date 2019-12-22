
#certain variables were chosen to be categorical via as.factor()
#Departure Delays was calculated similarly, but with weather variables coded as .x (the starting destination) instead of .y

Arr_result = biglm.big.matrix (ArrDelay ~ Year + Month + DayofMonth + DayOfWeek + Distance + TEMP.y + DEWP.y + SLP.y + VISIB.y + 
                                 WDSP.y + MXSPD.y + PRCP.y + SNDP.y + UniqueCarrier + Origin + Dest + Fog.y + Rain.y + Snow.y + 
                                 Hail.y + Thunder.y + Tornado.y + season, data = dat )


#####obtain Arrival Departure GVIF for multilinearity diagnosis
#VIF for Departure Delays was calculated similarly
VIF = function(lr_result){
  ##obtain linear regression result list
  v = vcov(lr_result)
  assign = lr_result$assign
  if (names(coefficients(lr_result)[1]) == "(Intercept)") {
    v = v[-1, -1]
    assign = assign[-1]
  }
  terms = labels(terms(lr_result))
  n.terms = length(terms)
  ##obtain covariance matrix
  R = cov2cor(v)
  detR = det(R)
  result = matrix(0, n.terms, 3)
  rownames(result) = terms
  colnames(result) = c("GVIF", "Df", "GVIF^(1/(2*Df))")
  ##calculate VIF
  for (term in 1:n.terms) {
    subs = which(assign == term)
    result[term, 1] = det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] = length(subs)
  }
  if (all(result[, 2] == 1)) result = result[, 1] else result[, 3] = result[, 1]^(1/(2 * result[, 2]))
}

VIF(Arr_result)


####PRESS for ArrDelay
##PRESS for DepDelay was calculated similarly, but with weather variables coded as .x (the starting destination) instead of .y

#set seed for reproducibility
set.seed(1234)
dat1 = dat[complete.cases(dat), ]
##create k folds to do cross validation
flds = createFolds(dat1$ArrDelay, k = 10, list = TRUE, returnTrain = FALSE)
n=length(flds)
##functions for obtain fitted values based of estimated coefficients
##producing dummy variables
factorize=function(name, data = dat1){
  len = nrow(data)
  q = levels(name)
  q = sort(q)
  group_num = length(q)
  x_matrix = matrix(0, len, group_num)
  ###produce indicate variable for each group of the categorical variable x
  for (i in 1:group_num) {x_matrix[, i] = name==q[i]}
  x_matrix = x_matrix[, -1]
  return(x_matrix)
}
##set default value
sum_rresidual = 0
SSY = 0
Ybar = mean(dat1$ArrDelay)
##form design matrix for the model
b=cbind(rep(1, nrow(dat1)), factorize(dat1$Year), dat1$Month, dat1$DayofMonth, factorize(dat1$DayOfWeek), dat1$Distance, dat1$TEMP.y,
        dat1$DEWP.y, dat1$SLP.y, dat1$VISIB.y, dat1$WDSP.y, dat1$MXSPD.y, dat1$PRCP.y, dat1$SNDP.y, factorize(dat1$UniqueCarrier),
        factorize(dat1$Origin),factorize(dat1$Dest),factorize(dat1$Fog.y), factorize(dat1$Rain.y), factorize(dat1$Snow.y), 
        factorize(dat1$Hail.y),factorize(dat1$Thunder.y), factorize(dat1$Tornado.y), factorize(dat1$season))
##fit model with training data and estimate PREE for each fold
for (i in 1:n){
  a = biglm.big.matrix (ArrDelay ~ Year + Month + DayofMonth + DayOfWeek + Distance + TEMP.y + DEWP.y + SLP.y + VISIB.y + WDSP.y + 
                          MXSPD.y + PRCP.y + SNDP.y + UniqueCarrier + Origin + Dest + Fog.y + Rain.y + Snow.y + Hail.y + Thunder.y + 
                          Tornado.y + season, data = dat1[-flds[[i]],] )
  x = b[flds[[i]],]
  ##calculate fitted value using coefficients and design matrix
  c = x%*%summary(a)[[2]][,1]
  ##Y-Y_hat
  c = dat1[flds[[i]], ]$ArrDelay - c
  SSY = SSY + sum((dat1[flds[[i]], ]$ArrDelay - Ybar)^2 )
  n1 = n1 + length(c)
  sum_rresidual = sum_rresidual + sum(c^2)
}
##calculate R_PRESS
R_PRESS = 1 - sum_rresidual/SSY
R_PRESS
