################################################################################
#
# File:         mapeval.r
# RCS:          $Header: $
# Description:  Column recovery experiments
# Author:       Staal Vinterbo
# Created:      Fri Sep 30 15:09:46 2011
# Modified:     Mon Oct  3 16:57:21 2011 (Staal Vinterbo) staal@dink
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
library(cluster) # daisy

source('arec.r')
source('data2graph.r')

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

pairs = function(h, x = 0, y = 0){
  if (length(h) == 0) return(x)
  n = head(h, 1)
  pairs(tail(h, -1), x + y*n, y + n)
}

genvval = function(g) {
  v = sapply(list.vertex.attributes(g)[-1],
    function(n) as.numeric(get.vertex.attribute(g, n)))
  apply(v, 1, pairs)
}

gengmat = function(g) {
  m = get.adjacency(g, attr='weight')
  m = matrix(as.numeric(m), ncol=ncol(m))
  v = genvval(g)
  m = rbind(vertex.values=v, m)
  colnames(m) = V(g)$name
  m
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

getmap = function(g1, g2, k=5, c=10) {
  fn1 = '/tmp/g1230269.graphml'
  fn2 = '/tmp/g2230269.graphml'
  fn3 = '/tmp/t230269.txt'  
  g2file(g1, fn1)
  g2file(g2, fn2)
  res = system2('yagm.py', c('-v', '--external',
    '--coverage',  paste(c), '-k', paste(k), '--xoutonly', fn1, fn2), stdout=fn3)
  b = read.table(fn3, header=FALSE, sep=',')
  colnames(b) = c('m', 'n', 'k', 'coverage', 'seconds', 'top.correct', 'correct', 'score', 'namescore')
  b
}


# experiment entrypoint
runexperiment = function(datatable, fraction = 0.5, times = 10, k=5, c=20, fstem='graph', out=FALSE) {
  results = c()
  i = 1
  while(times > 0) {
    rows = sample(1:nrow(datatable), ceiling(fraction*nrow(datatable)))
    cols = sample(1:ncol(datatable), ceiling(fraction*ncol(datatable)))
    subtable = datatable[rows, cols]
    # make sure that no columns are constant
    arities = apply(subtable, 2, function(x) length(unique(x)))
    if (sum(arities < 2) > 0) next
    # write problem instances to disk
    g1 = gen.sym(subtable, n=1, m=100000000)
    g2 = gen.sym(datatable, n=1, m=100000000)
    if (out) {
      base = paste(fstem, i, sep='.')
      g2file(g1, paste(base, 'G1', 'graphml', sep='.'))
      g2file(g2, paste(base, 'G2', 'graphml', sep='.'))      
      write.table(gengmat(g1), paste(base, 'G1', 'txt', sep='.'),
                  quote=FALSE, row.names=FALSE, sep=',')
      write.table(gengmat(g2), paste(base, 'G2', 'txt', sep='.'),
                  quote=FALSE, row.names=FALSE, sep=',')
    }
    # do the deed
    results = rbind(results, getmap(g1, g2, k=k, c=c))
    times = times - 1
    i = i + 1
  }
  results
}

put = function(m, file="", append=TRUE) {
  cat(colnames(m), '\n', file=file, append=append)
  write(t(as.matrix(m)), ncolumn=ncol(m), file=file, append=append)
}

# summarize
summarize = function(res, file = ""){
  cat('Summary of table:\n', file=file, append=TRUE)
  put(res, file=file, append=TRUE)
  # show averages:
  cat('averages:\n', file=file, append=TRUE)
  cat(apply(res, 2, mean), '\n', file=file, append=TRUE)
  cat('standard deviations:\n', file=file, append=TRUE)
  cat(apply(res, 2, sd), '\n', file=file, append=TRUE)  
  cat('mean correct fractions: ', mean(res$correct/res$m), '\n', file=file, append=TRUE)
  cat('mean top/score fraction: ', mean(res$top.correct/res$correct), '\n', file=file, append=TRUE)
}

# do experiments with default parameters for fractions 0.5, 0.75, 0.9
do3 = function(datatable, name='table', k = 5, c=10) {
  rlist = list()
  contents = c(0.25, 0.5, 0.85)
  fracs = sqrt(contents)
  for (cont in contents) {
    frac = sqrt(cont)
    res = runexperiment(datatable, frac, k=k, c=c,
      out=TRUE, fstem=paste(name, cont, sep='.'))
    rlist = c(rlist, list(res))
  }
  names(rlist) = paste('content =', contents)
  pdf(paste(name, 'figure', 'pdf', sep='.'))
  boxplot(Map(function(res) res$correct/res$m, rlist), main = "Correctness fraction")
  dev.off()
  Map(function(x) summarize(x, paste(name, 'summary', 'txt', sep='.')), rlist)
  save(rlist, file=paste(name, 'rlist', 'rdata', sep='.'))
  rlist
}
