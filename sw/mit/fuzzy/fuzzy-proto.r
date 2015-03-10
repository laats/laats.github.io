################################################################################
#
# File:         fuzzy-example.r
# RCS:          $Header: $
# Description:  Example implementation of algorithm presented in paper
#               "Small, Fuzzy, and interpretable gene expression classifiers".
# Author:       Staal Vinterbo
# Created:      Tue Oct 26 11:41:03 2004
# Modified:     Tue May 03 11:50:58 2005 (Staal Vinterbo) staal@MGV
# Language:     ESS[S]
# Package:      N/A
# Status:       Experimental (Do Not Distribute)
#
# (c) Copyright 2004, Staal Vinterbo, all rights reserved.
#
################################################################################
library(mgcv) # uniquecombs

######## Determine Membership functions
# Memberships are triangular and hence determined by three points.
# This is the way they are stored.

# used to implement triangular membership
ramp <- function(val, v1, v2){
  if(val <= v1) return (0)
  if(val >= v2) return (1)
  slope <- 1/(v2-v1)
  return ((val - v1)*slope)
}

# determines the membership of point x in triangular membership determined
# by v1 (first intersect with 0), v2 (top), and v3 (last intersect with 0)
member3 <- function(x,v1,v2,v3){
  if(x <= v2){
    if(v1 == -Inf)
      return(1)
    return(ramp(x,v1,v2))
  }
  if(v3 == Inf)
    return(1)
  return(1 - ramp(x,v2,v3))
}

# vectorized version
member <- function(x,v){
  return(member3(x,v[1],v[2],v[3]))
}

# Determine and store the membership function triplets in a list structure
# for each dimension in a data frame/set.
# There is one triplet for each level, so if nlevel=3 then
# we use 9 points to represent the three membership functions.
gencmatrix <- function(indata,nlev=3){
  l <- apply(indata, 2,
             function(col){
               c(-Inf,quantile(col,probs=seq(0,1,1/(nlev-1))), Inf)
             })
  l <- apply(l, 2, list) # make list of matrix
  l <- lapply(l, unlist)
  names(l) <- colnames(indata)
  return(l)
}

######### Applying memberships to data points
# Given the new data, the membership list structure and the rules, generate
# a 'hit' matrix storing how much each point in the data is a member in each
# rule antecedent.

vforapply <- function(obj, rule, cmatrix){
  v <- c()
  for(i in 1:(length(rule) - 1)){
    if(!is.na(rule[i])){
      n <- names(rule[i])
      l <- cmatrix[[n]]
      if(sum(is.na(l)) == 0)
        v <- c(v,member(obj[n], l[(rule[i]+1):(rule[i]+3)]))
      else # no cuts available, do strict match
        v <- c(v,(rule[i] == obj[n])+0)
    }
  }
  return(v)
}

forapply <- function(obj, rule, cmatrix){
  return(min(vforapply(obj, rule, cmatrix)))
}

                       
rapply <- function(indata,rules,cmatrix){
  fun <- function(r){
    return(as.vector(apply(indata,1,forapply,r,cmatrix)))
  }
  return(t(apply(rules, 1, fun)))
}

dovote <- function(mm,            # fire matrix
                   clss,          # classes to vote on
                   rt,            # rule consequents
                   ...){
  ncls <- length(clss)
  votes <- matrix(rep(0, ncol(mm)*ncls), ncol = ncls)
  colnames(votes) <- clss
  for(i in clss){
    ci <- (1:ncls)[clss == i] # class index in votes array
    ri <- rt==i         # rules that have this class...
    if(sum(ri) > 0){
        votes[,ci] <- pmax(votes[,ci],apply(mm[ri,,drop=F],2,max))
    }
  }
  return(votes)
}

######## Discretization of data
# Discretize according to cuts determined by quantiles

cuts.quantile <- function(array, nlev = 3,...){
  if(length(levels(factor(array))) <= nlev)
    return(NULL)
  qs <- quantile(unlist(array),probs=seq(0,1,1/(nlev-1)))
  cuts <- c()
  for(i in 1:(length(qs)-1))
    cuts <- c(cuts,qs[i] + (qs[i+1] - qs[i])/2)
  return(cuts)
}
cuts.apply.array <- function(array, cuts){
  array <- unlist(array)
  or <- order(array)
  j <- 1
  c <- NULL
  for(i in 1:length(array)){
    while(j <= length(cuts) && array[or[i]] > cuts[j])
      j <- j + 1
    c[or[i]] <- j - 1
  }
  return(c)
}

cuts.table <- function(table, cuts.function, ...){
  r <- list()
  for(i in 1:ncol(table)){
    l <- cuts.function(table[,i], ...)
    if(!is.null(l))
      r <- append(r,list(list(col=i, cuts=l)))
  }
  return(r)
}

cuts.apply <- function(table, clist){
  if(is.null(clist))
    return(table)
  for(l in clist)
    table[,l$col] <- cuts.apply.array(table[,l$col], l$cuts)
  return(table)
}

#### Generate rules from discretized data table
# computes the discernibility matrix row/column for object i
mi <- function(data, i){ t(apply(data, 1,
                             function(r,a){diag(outer(r,a,"!="))},data[i,]))}
greedySC <- function(sets, cost = rep(1,nrow(sets)), mit = nrow(sets)*5){
  u <- rep(T,ncol(sets))
  m <- 1:ncol(sets)
  avail <- 1:nrow(sets)
  sol <- c()
  it <- 0
  while(sum(u) > 0 && length(avail) > 0 && it < mit){
    it <- it + 1
    a <- 0
    b <- 0
    for(i in avail){
      d <- sum(sets[i,u])/cost[i]
      if(d >= a){
        a <- d
        b <- i
      }
    }
    cat(c("adding", b, "\n"))
    sol <- c(sol, b)
    res <- apply(sets[sol,,drop=F],2,sum) > 0
    u[res] <- F
    avail <- setdiff(avail,b)
  }
  return(sol)
}
greedy <- function(collection){
  collection <- collection[apply(collection,1,sum) > 0,,drop=F]
  return(greedySC(t(collection)))
}

# instantiate template to generate a rule
itemplate <- function(rtemplate, cols){
  cols <- as.vector(cols)
  h <- rep(T,length(rtemplate))
  h[unique(c(cols,length(rtemplate)))] <- F
  rtemplate[h] <- NA
  return(rtemplate)
}

# computes the rules.
# returns a tabular representation with don't cares (NA)
# note that duplicates are not removed here, nor are inconsistencies
# dealt with 
genrules <- function(indata,...){
  indata <- as.matrix(indata)
  dims <- dim(indata)
  rules <- c()
  for(i in 1:dims[1]){
    cat(c("Element ", i, " of ", dims[1], "\n"))
    coll <- mi(indata[,1:(dims[2]-1),drop=F],i)
    coll <- coll[indata[,dims[2]] != indata[i,dims[2]],,drop=F]
    if(sum(coll != 0) == 0){
      cat(c("WARNING: element", i, "produced no rules...\n"))
      next
    }
    r <- greedy(coll)
    rules <- rbind(rules,itemplate(indata[i,],unlist(r)))
  }
  return(rules)
}

# filter the rules
filter <- function(vm, rt, dt, bc = 10){
  cm <- outer(rt,dt,function(a,b){
    n <- a == b
    n[!n] <- -1
    return(n)
  })
  
  vm <- vm * cm

  maxvm <- max(abs(vm))
  classes <- unique(rt)

  for(i in classes){
    av <- (rt == i) + 0
    nc <- ceiling(bc * (length(rt) - sum(av)))
    vm <- cbind(vm,av*(nc*maxvm))
  }

  return(greedy(t(vm)))
}
# remove duplicate rules 
rdr <- function(rules){
  notused <- -999999999 # assumed not to be a valid value 
  if(sum(rules == notused, na.rm=T) > 0)
    stop("internal error in rdr\n")
  rules[is.na(rules)] <- notused
  ur <- uniquecombs(as.matrix(rules))
  ur[ur == notused] <- NA
  colnames(ur) <- colnames(rules)
  return(as.data.frame(ur))
}
####### Apply the above

do.generate <- function(trd, nlevels=2){
  cutlist <- cuts.table(trd[,1:(ncol(trd)-1)],cuts.quantile,nlev=nlevels)
  trdd <- cbind(cuts.apply(trd[,1:(ncol(trd)-1)],cutlist), trd[,ncol(trd)])
  colnames(trdd) <- colnames(trd)
  rules <- genrules(trdd)
  rules <- rdr(rules)
  cmatrix <- gencmatrix(trd,nlev=nlevels)
  mmtrd <- rapply(trd,rules,cmatrix)
  frules <- rules[filter(mmtrd, rules[,ncol(rules)], trd[,ncol(trd)]), ,drop=F]
  return(list(rules=frules, cmatrix=cmatrix,orules=rules))
}

# returns a matrix of outcome memberships...
do.apply <- function(todata, rules, cmatrix){
  mmtsdf <- rapply(todata,rules,cmatrix)
  memships <- dovote(mmtsdf,sort(unique(todata[,ncol(todata)])),
                  rules[,ncol(rules)])
  return(memships)
}

# prettyprint rules...
pprules <- function(rules){
  rules <- as.data.frame(rules) # create names if there are none
  nv <- colnames(rules)
  oi <- ncol(rules)
  classes <- sort(unique(rules[,oi]))

  pr <- function(rule){
    inc <- !is.na(rule)
    inc[length(rule)] <- F
    paste(paste(nv[inc], rule[inc],
                sep="=", collapse=" & "),
          paste(nv[length(rule)], rule[length(rule)], sep="="),
          sep=" => ")
  }
  unlist(sapply(classes,
                function(i) sort(apply(rules[rules[,oi] == i,], 1, pr),
                                 decreasing=T)))
}

# even prettier
ppprules <- function(rules){
  cat(paste(pprules(rules), collect="\n"))
}

#example, assumes dataset is dataset frame, last column is class tag.
# > res <- do.generate(dataset)
# > ppprules(res$rules)
# > mems <- do.apply(dataset, res$rules, res$cmatrix)
# >                            # use res$orules above for unfiltered rules
# > cbind(mems, dataset[,ncol(dataset)])

# that's all folks
  
