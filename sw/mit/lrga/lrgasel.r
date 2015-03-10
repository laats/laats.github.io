################################################################################
#
# File:         lrgasel.r
# RCS:          $Header: $
# Description:  
# Author:       Staal Vinterbo
# Created:      Tue Nov 18 14:58:52 2003
# Modified:     Thu Dec 20 20:25:16 2007 (Staal Vinterbo) staal@STUMPA
# Language:     ESS[S]
# Package:      N/A
# Status:       GPL distributable
#
# (c) Copyright 2003, Staal Vinterbo, all rights reserved.
#
# lrgasel.r is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# lrgasel.r is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with lrgasel.r; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
################################################################################
#
# Revisions:
#
# Thu Dec 20 20:24:44 2007 (Staal Vinterbo) staal@STUMPA
#  Used context variable passed to eval and objective.
# Fri Nov 21 15:26:14 2003 (Staal Vinterbo) staal@MGV
#  Added GPL info
################################################################################

source("ooga.r")
source("cindex.r")

computeci <- function(trains,tests){
  nc <- ncol(trains)
  predictors <- colnames(trains)[1:(nc - 1)]
  outcome <- colnames(trains)[nc]

  # create formula
  formulaText <- paste(paste(outcome, " ~ "), paste(predictors, collapse="+"))
  formula <- as.formula(formulaText)

  # do the regression
  lrmodel <- glm(formula, family = binomial(link = logit), data=trains)
  pred <- predict.glm(lrmodel, tests, type="response", se.fit=FALSE)
  return(cindex(pred,tests[,nc])[1])
}

objective.logical <- function(a, context, ...){
  if(sum(a) < 1)
    return(0)    # no use in doing empty predictors...
  incl <- c(a, T)
  ci <- computeci(context$train[,incl,drop=F], context$hold[,incl,drop=F])
  return(context$rho * ci + (1-context$rho)*(length(a) - sum(a))/length(a))
}

lrvsga <- function(train,hold,test,rho=0.5,...){ 
  context = list(train=train, hold=hold,test=test,rho=rho)
  myseed <- rep(F,ncol(train)-1)
  res <- ga(myseed, context=context,...)

  #report solution
  incl <- c(res$sol,T)
  ci <- computeci(train[,incl],train[,incl])
  cat(c("-----\nci train", ci, "len", sum(res$sol), "\n"))
  ci <- computeci(train[,incl],test[,incl])
  cat(c("ci test", ci, "len", sum(res$sol), "\n"))
  cat(c("variables\n", colnames(train)[res$sol], "\n----\n"))
  #end report solution
  
  return(list(ci,res))
}
  
# try
# result <- lrvsga(train=trainingdata,test=testdata,hold=trainingdata)


