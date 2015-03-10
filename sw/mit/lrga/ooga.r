################################################################################
#
# File:         ooga.r
# RCS:          $Header: $
# Description:  Object oriented genetic algorithm example
# Author:       Staal Vinterbo
# Created:      Thu Jul 03 15:14:00 2003
# Modified:     Thu Dec 20 20:25:43 2007 (Staal Vinterbo) staal@STUMPA
# Language:     ESS[S]
# Package:      N/A
# Status:       GPL Distributable
#
# (c) Copyright 2003, Staal Vinterbo, all rights reserved.
#
# ooga.r is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# ooga.r is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ooga.r; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
################################################################################
#
# Revisions:
#
# Thu Dec 20 20:25:24 2007 (Staal Vinterbo) staal@STUMPA
#  Added a context variable to pass to objective via eval.
# Wed Nov 19 15:34:52 2003 (Staal Vinterbo) staal@MGV
#  First revision, contains a logical (bit vector) implementation and a
#  ready to run bit vector example. 
################################################################################

mate <- function(a,b,...) UseMethod("mate")
mutate <- function(a,...) UseMethod("mutate")
clone <- function(a,...) UseMethod("clone")
objective <- function(a,context,...) UseMethod("objective")
initp <- function(a,...) UseMethod("initp")

# defaults: not really needed, but good for warnings
mate.default <- function(a, b, ...)
  stop(c("no mate operation defined for", class(a)))
mutate.default <- function(a, ...)
  stop(c("no mutate operation defined for", class(a)))
clone.default <- function(a, ...)
  stop(c("no clone operation defined for", class(a)))
objective.default <- function(a, context, ...)
  stop(c("no objective function defined for", class(a)))
initp.default <- function(a, psize, ...)
  stop(c("no initp function defined for", class(a)))

#### GA framework
# is generational, static population size, elitism is optional

eval <- function(p, context=NULL, ...)
  sapply(p, objective, context, ...)

scale <- function(f,epsilon=0.001, ...){   # a hack...
  f[is.na(f)] <- 0
  if(min(f) <= 0)
    f + min(f) + epsilon
  else
    f
}

recomb <- function(p, ps, o, elite=T, context=NULL,...){
  p <- as.list(p)
  o <- as.list(o)
  po <- c(p,o)
  os <- eval(o,context=context,...)
  alls <- c(ps,os)
  f <- scale(alls,...)
  inc <- sample(1:length(po), length(p), replace=T, prob=f/sum(f))
  if(elite){
    maxs <- max(alls)
    maxi <- (1:length(alls))[alls == maxs][1]
    if(!(maxi %in% inc)){
      mins <- min(alls[inc])
      mini <- (1:length(inc))[alls[inc] == mins][1]
      inc[mini] <- maxi
    }
  }
  return(list(p=po[inc], s=(c(ps,os))[inc]))
}

ga <- function(seed, psize = 50, ngen = 100,
               nmate = ceiling(psize/10),
               nmut=ceiling(psize/30),
               verbose=T,
               dostats=F, context=NULL,...){
  if(verbose)
    cat("initializing population...\n")
  p <- initp(seed, psize=psize, ...)
  if(verbose)
    cat("evaluating initialized population...\n")
  s <- eval(p,context,...)
  mi <- as.list(p)[s == max(s)][[1]]
  ms <- max(s)
  statsm <- NULL
  
  for(gen in 1:ngen){
    if(verbose)
      cat(c("doing generation", gen, "hi", ms, "\n"))
    if(dostats)
      statsm <- rbind(statsm, c(min(s),mean(s),max(s),ms))
    f <- scale(s)
    b <- sample(1:psize, 2*nmate, prob=f/sum(f))
    u <- sample(1:psize, nmut, prob=f/sum(f))
    o <- lapply(1:nmate, function(i) { mate(p[[i]], p[[i+nmate]],...) })
    o <- c(o,lapply(1:nmut, function(i) { mutate(p[[i]],...) }))
    ps <- recomb(p,s,o,context=context,...)
    p <- ps$p
    s <- ps$s
    if(max(s) > ms){
      mi <- as.list(p)[s == max(s)][[1]]
      ms <- max(s)
    }
  }
  return(list(sol=mi, score=ms, p=p,s=s, stats=statsm))
}

###### end GA framework







########## logical (Bit Vector) implementation #######################
# Should maybe implement inversion...

# one point crossover
mate.logical <- function(a,b,...){
  point <- sample(1:length(a),1)
  a[point:length(a)] <- b[point:length(a)]
  return(a)
}
  
# bit flip
mutate.logical <- function(a,pp=0.2,...){
  m <- sample(c(T,F), length(a), replace=T, prob=c(pp,1-pp))
  a[m] <- !a[m]
  return(a)
}

# create new 
clone.logical <- function(a,ip=0.5,...)
  sample(c(T,F), length(a), replace=T, prob=c(ip,1-ip))

as.list.matrix <- function(m,...)
  lapply(1:nrow(m), function(i) { m[i,] })

# init population
initp.logical <- function(seed, psize = 50,...)
  as.list.matrix(t(sapply(1:psize, function(i,...) { clone(seed,...) },...)))



####### Ready to run example #######
# try:
# objective.logical<-function(a,context=NULL,...)sum(sapply(1:(length(a)-1),function(i)xor(a[i],a[i+1])))
# res<-ga(rep(F,20),ngen=300,nmut=1,elite=F,dostats=T)
# plot(c(min(res$stats),res$stats[,4]))
# for(i in 1:3)lines(res$stats[,i],col=i)


# that's all folks

