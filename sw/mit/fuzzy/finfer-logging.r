################################################################################
#
# File:         finfer-logging.r
# RCS:          $Header: $
# Description:  Logging utilities for fuzzy inference (finfer.r)
# Author:       Staal Vinterbo
# Created:      Thu Nov  6 10:57:46 2008
# Modified:     Wed Dec 17 17:16:25 2008 (Staal Vinterbo) staal@ding
# Language:     ESS[S]
# Package:      N/A
# Status:       Experimental
#
# finfer-logging.r is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# finfer-logging.r is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with finfer-logging.r; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# (c) Copyright 2008, Staal Vinterbo, all rights reserved.
#
################################################################################


# depends on finfer.r


###### Extra utility functions for rule analysis

# For tuning of rules, it is useful to know which rules fire
# with which strength at which time. 
# application:
#    call logging.initiate()   before first call to mkrule
#    call logging.analyzelog() after all infer()'s are done
#    call logging.terminate()  to turn off logging and clean up


# initate logging
logging.initiate = function(){
  logging.rid <<- 1
  logging.nround <<- 0
  logging.log <<- list()
  logging.tmp <<- c()
  logging.bp <<- list()

  # install logging zipapply to capture limit value and individual alphas values
  logging.save.zipapply <<- zipapply
  zipapply <<- function(funlist, pointlist){
    vlist = logging.save.zipapply(funlist, pointlist)
    logging.log <<- append(logging.log,
                           list(c(logging.tmp[1:2],min(vlist), vlist,
                                  logging.tmp[3:length(logging.tmp)])))
    return(vlist)
  }

  # install the logging version of mkrule:
  logging.save.mkrule <<- mkrule
  mkrule <<- function(alphas, beta){
    id = logging.rid
    logging.rid <<- logging.rid + 1
    imp = logging.save.mkrule(alphas, beta)
    function(singleton){
      logging.tmp <<- c(logging.nround, id, singleton)
      rval = imp(singleton)
      return(rval)
  }}

  # install the logging version of infer that calls logging.newround()
  logging.save.infer <<- infer
  infer <<- function(rules, x, samplepoints){
    logging.newround()
    bp = logging.save.infer(rules, x, samplepoints)
    logging.bp <<- append(logging.bp, list(bp))
    return(bp)
  }
}



# called between each call to infer()
logging.newround = function()
  logging.nround <<- logging.nround + 1


# terminate logging and clean up, returns log
logging.terminate = function(){
  mkrule <<- logging.save.mkrule
  infer <<- logging.save.infer
  zipapply <<- logging.save.zipapply
  log = logging.log
  rm(list=c('logging.rid', 'logging.nround', 'logging.log', 'logging.tmp',
       'logging.save.mkrule', 'logging.save.infer', 'logging.bp',
       'logging.save.zipapply'), envir = .GlobalEnv)
  invisible(log)
}


# analyze rule activation log
#  alphas: list of list. Each positional element is a list of
#          fuzzy membership functions for that input dimension.
#          optional.
#  betan:  names of beta fuzzy sets. optional.
logging.analyzelog = function(alphas=list(), betan=c()){
  def.par <- par(no.readonly = TRUE)
  logl = logging.log
  layout(matrix(1:(1+length(alphas)+(length(betan) > 0)), ncol=1),
         heights=rep(8,1+length(alphas)+(length(betan) > 0)))
  log = t(sapply(logl, function(r) r[1:3]))
  cat('Rule log analysis:\n')
  cat('------------------\n')
  firing = log[,3] != 0
  rulids = unique(log[firing,2])
  cat(' Rules that fired:', sort(rulids), '\n')
  nr = max(rulids)
  i = rep(T,nr)
  i[rulids] = F
  cat('   holes:', (1:nr)[i], '\n')
  cat(' Order they first fired in:', rulids, '\n')
  rnds = sort(unique(log[,1]))
  log = log[firing,]
  acts = sapply(rnds, function(rnd){
    a = rep(0,nr)
    ri = log[,1] == rnd
    a[log[ri,2]] = log[ri,3]
    return(a)
  })
  rocc = t(sapply(sort(rulids), function(rid)
    c(length(log[log[,2] == rid,2]), max(log[log[,2] == rid,3]))))
  rocc = data.frame(rocc)
  rownames(rocc) = rulids
  colnames(rocc) = c('Activation count', 'max activation')
  cat(' Rule activations:\n')
  print(rocc)
  xl = 'time'
  if(length(alphas)) xl = NULL
  barplot(acts, col=hsv(seq(0,1,1/nr)), legend.text = paste(1:nr), xlab=xl,
          main='Rule activation at successive infer() calls')

  nd = length(alphas) 
  if (nd > 0) {
    getnames = function(fl)
      apply(zip(fl, 1:length(fl)), 1, function(r)
             if(is.null(attr(r[[1]], 'name'))) r[[2]] else attr(r[[1]], 'name'))
    nrounds = max(rnds)
    nr = length(logl)/nrounds
    pntsm = sapply(logl[(1:nrounds)*nr],
      function(r) r[(length(r)-length(alphas)+1):length(r)])
    for (i in 1:nd) {
      al = alphas[[i]]
      nal = length(al)
      pnts = pntsm[i,]
      mal = sapply(pnts, function(x) sapply(al, function(a,x) a(x), x))
      barplot(mal, col=hsv(seq(0,1,1/nal)),
              legend.text = getnames(al), xlab=xl,
          main=paste('Alpha', i, 'activations'))
    }
  }
  if(length(betan)){
    mb = sapply(logging.bp, function(r) r)
    barplot(mb, col=hsv(seq(0,1,1/length(betan))),
              legend.text = betan, xlab='time',
          main='Beta activations')
  }
  par(def.par)
  return (logl)
}

