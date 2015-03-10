################################################################################
#
# File:         arec.r
# RCS:          $Header: $
# Description:  attribute value recovery routines
#     Task: Given two m x n1, m x n2 data tables V and W with columns vi and wi
#     respectively, create an assignment from s(vi) to s(wi), where s(x) is the
#     set of values in x. Approach:
#       Let gi be a weighted bipartite graph with edge sets s(vi) and s(wi),
#       and let h(gi, vi) -> wi' be a function that uses gi to translate values
#       in vi to s(wi) creating a vector wi'. Given h and G = [gi] we can
#       translate V to H(V,G) = W' = [h(gi,vi) | i = 1,2,...,m]^t.
#       Now let m(X,i) be a
#       function that creates another function pi that estimates the conditional
#       probability pi(v,X[j,l\=i]) = P(X[j,i] = v | X[j,l \= i])
#       for v in s(ci) where ci is column i in table X. Let u
#       be a function taking a bipartite graph g = (A,B,E) and a triple
#       (a,b,x) in A x B x R and updates g to produce g'. Given pi, V, W', and W
#       we can produce a list of triples (a,b,w) as
#            (V[j,i], W'[j,i], pi(W'[j,i], W'[j, k \= i])) for rows j in V
#       let gl(pi, V, W, W') produce that list. Using gl, we can compute gi'
#       as gi' = foldl u gi gl(pi, V, W, W' = H(V,G)).
# Author:       Staal Vinterbo
# Created:      Fri Sep  2 10:34:30 2011
# Modified:     Tue Sep  6 20:33:30 2011 (Staal Vinterbo) staal@dink
# Language:     ESS[S]
# Package:      N/A
# Status:       Experimental
#
# (c) Copyright 2011, Staal Vinterbo, all rights reserved.
#
################################################################################

library(clue)
library(e1071)

initg = function(v, w) {
  fv = factor(v)
  fw = factor(w)
  x = table(fv)
  y = table(fw)
  1 - t(sapply(x/sum(x), function(v) abs(v - y/sum(y))))
}

initgs = function(V, W) {
  l = Map(function(i) initg(V[,i], W[,i]), 1:ncol(V))
  names(l) = colnames(W)
  l
}

gadd = function(triplets, g, alpha=0.5) {
  g[triplets[,1:2]] = (1-alpha)*g[triplets[,1:2]] + alpha*as.numeric(triplets[,3])
  g
}

makemap = function(g) {
  rn = rownames(g)
  cn = colnames(g)
  p = solve_LSAP(g, maximum=TRUE)
  map = cn[p]
  names(map) = rn
  map
}

makeVp = function(gs, V) {
  Vp = c()
  for (i in 1:ncol(V)) {
    vmap = makemap(gs[[i]])
    Vp = cbind(Vp, vmap[factor(V[,i])])
  }
  colnames(Vp) = names(gs)
  Vp
}
  

updategs = function(gs, V, preds, alpha = 0.5) {

  for (i in 1:ncol(V)) {

    Vp = makeVp(gs, V)
    ptab = preds[[i]](Vp)
    cn = 1:ncol(ptab)
    names(cn) = colnames(ptab)
    
    idx = cbind(1:nrow(ptab), cn[Vp[,i]])
    scores = ptab[idx]
    triplets = cbind(as.vector(V[,i]), as.vector(Vp[, i]), scores)

    gs[[i]] = gadd(triplets, gs[[i]], alpha)

  }

  gs

}

makesvmpreds = function(W) {
  cn = colnames(W)
  Map(function(n) {
    model = svm(as.formula(paste(n, " ~ .")), data=W, type='C-classification',
      probability=TRUE)
    function(Vp) {
      attr(predict(model, Vp, probability=TRUE), "probabilities")
    }
  }, cn)
}
      
  
    
simplerec = function(V, W, times=100) {
  preds = makesvmpreds(W)
  gs = initgs(V, W)
  if (times < 1) return (gs)
  for (i in 1:times)
    gs = updategs(gs, V, preds)
  gs
}
    
  
