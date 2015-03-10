################################################################################
#
# File:         data2graph.r
# RCS:          $Header: $
# Description:  
# Author:       Staal Vinterbo
# Created:      Sun Jun 19 12:03:53 2011
# Modified:     Mon Jul 11 17:44:03 2011 (Staal Vinterbo) staal@dink
# Language:     ESS[S]
# Package:      N/A
# Status:       Experimental
#
# (c) Copyright 2011, Staal Vinterbo, all rights reserved.
#
################################################################################

library(foreign) # read.arff
library(igraph)
library(vcd) # assocstats

# measures

cc = function(a,b) assocstats(table(a,b))$contingency
phi = function(a,b) assocstats(table(a,b))$phi
cramer = function(a,b) assocstats(table(a,b))$cramer
pp = function(a,b) assocstats(table(a,b))$chisq_tests[6]
prvxy = function(X, Y) {
  v = function(r) sum(sapply(r/sum(r), function(p) p*(1-p)))
  vary = v(table(Y))
  if (vary == 0) return( 0 )
  varyx = mean(apply(table(X,Y), 1, v))
  (vary - varyx)/vary
}
prvyx = function(X, Y) prvxy(Y, X)
histodiff = function(X, Y) {
  a = table(X)
  b = table(Y)
  lena = length(a)
  lenb = length(b)

  if(lena > lenb) {
    b = c(b, rep(0, lena - lenb))
  } else if (lenb > lena) {
    a = c(a, rep(0, lenb - lena))
  }
  
  a = sort(a/max(a), decreasing=TRUE)
  b = sort(b/max(b), decreasing=TRUE)
  sum(abs(a - b), na.rm=TRUE)/max(lena, lenb)
}

sim.functions = c(cc=cc, pp=pp, pxy = prvxy, pyx = prvyx, hdiff=histodiff)

repeated = function(f, X, Y, n = 1, m = 5000000000) {
  vals = sapply(1:n, function(i) {
    idx = sample(1:length(X), min(m, length(X)))
    f(X[idx], Y[idx])
  })
  median(vals)
}



gen.sym = function(indata, measure = phi, measures = sim.functions, n=100, m=50) {
  mat = t(combn(colnames(indata), 2))
  mat = cbind(mat,
    weight=apply(mat, 1, function(r) repeated(measure,
      indata[,r[1]], indata[,r[2]], n, m)))
  for (meas in measures)
    mat = cbind(mat, apply(mat, 1, function(r) repeated(meas,
      indata[,r[1]], indata[,r[2]], n, m)))
  colnames(mat) = c('X', 'Y', 'weight', names(measures))
  g = graph.data.frame(data.frame(mat), directed=FALSE)
  # add sorted value histogram as vertex info
  for (i in V(g)) {
    n = V(g)[i]$name
    vals = sort(table(indata[,n]), decreasing=T)
    vals = vals/sum(vals)
    for (j in 1:length(vals)) {
      g = set.vertex.attribute(g, paste('A', j, sep=''), index=i, value=vals[j])
    }
  }
  g
}

gen.sym.df = function(indata, measure = phi, measures = sim.functions, n=100, m=50) {
  mat = t(combn(colnames(indata), 2))
  mat = cbind(mat,
    weight=apply(mat, 1, function(r) repeated(measure,
      indata[,r[1]], indata[,r[2]], n, m)))
  for (meas in measures)
    mat = cbind(mat, apply(mat, 1, function(r) repeated(meas,
      indata[,r[1]], indata[,r[2]], n, m)))
  colnames(mat) = c('X', 'Y', 'weight', names(measures))
  data.frame(mat)
}

write.amatrix = function(g, fname) 
  write.table(get.adjacency(g, type='both', attr='weight'),
              file=fname, row.names=F, col.names=T, quote=F, sep=',')
  
uscale = function (x, h = max(x), l = min(x))  (x - l)/(h - l)

gen.asym = function(indata) {
  n = colnames(as.data.frame(indata))
  vmat = matrix(as.numeric(as.factor(as.matrix(indata))) - 1, ncol=ncol(indata))
  mat = t(combn(1:ncol(indata), 2))
  mat = rbind(mat, mat[,2:1])
  out = t(apply(mat, 1,
    function(r) {
      vf = max(prv(vmat[,r[1]], vmat[,r[2]]), 0)
      return(c(X=n[r[2]], Y=n[r[1]], weight=vf))
    }))
  compare = function(a,b) {if (a == b) 0 else { if(a < b) -1 else 1 }}
  comp = sapply(1:(nrow(mat)/2),
    function(i) compare(out[i,3],out[i + nrow(mat)/2, 3]))
  comp = c(comp, -comp)
  keep = comp > 0
  keep[1:(nrow(mat)/2)] = comp[1:(nrow(mat)/2)] == 0
  out = data.frame(out[keep,])
  tmp = as.numeric(levels(out$weight)[as.numeric(out$weight)])
  g = graph.data.frame(out, directed=TRUE)
  g
}


write.welist = function(g, fname) {
}

  
g2file = function(g, fname) {
  write.graph(g, fname, format='graphml')
  #rite.amatrix(g, fname)
  invisible(fname)
}

# if not interactively loaded, read arff file from stdin
# write graphml graph to stdout
#  Since write.graph uses writeBin (!) to write the graph
#  representation, and R's concept of stdout is text, we have
#  to open a pipe in binary mode to cat to print the xml
#  to stdout...
#   to change to directed graph representation change
#   gen.sym to gen.asym
# usage: cat data.arff | R --slave -f data2graph.r > graph.graphml
if (! interactive() ){
  g2file(gen.sym(read.arff(file('stdin'))), pipe('cat', open='wb'))
}
