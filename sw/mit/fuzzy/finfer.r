################################################################################
#
# File:         finfer.r
# RCS:          $Header: $
# Description:  Fuzzy set inference engine
# Author:       Staal Vinterbo
# Created:      Tue Nov  4 02:05:48 2008
# Modified:     Wed Dec 17 17:55:32 2008 (Staal Vinterbo) staal@ding
# Language:     ESS[S]
# Package:      N/A
# Status:       Experimental
#
# finfer.r is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# finfer.r is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with finfer.r; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# (c) Copyright 2008, Staal Vinterbo, all rights reserved.
#
################################################################################



####### Fuzzy Inference Definitions

## implication type
# minimum implication
implication.minimum = function(beta, limit) function(y) pmin(limit, beta(y))
# product implication
implication.product = function(beta, limit) function(y) limit * beta(y)
# default is minimum implication:
implication = implication.minimum


## rule functions

# zip(list1, list2)
#  returns a n x 2 matrix with each row i is c(list1[i], list2[i])
# since cbind recycles the shorter of the lists, and we want truncation of
# the longer list, we have to do this:
zip = function(l1, l2){
  len = min(length(l1),length(l2))
  cbind(l1[1:len], l2[1:len])
}

# zipapply(funlist, pointlist)
#  apply the functions in funlist to their corresponding point in pointlist
#  returns the list of computed values
zipapply = function(funlist, pointlist)
  apply(zip(funlist, pointlist),1, function(r) r[[1]](r[[2]]))

# mkrule(alphas, beta)
#  define a rule (alpha_1 /\ ... /\ alpha_k) -> beta
#    alphas: a list of fuzzy set membership functions, alpha_i is applied
#            to element i of the singleton input vector (see below)
#            tip: if your rule does not use all input dimensions,
#            use function(x) 1 in the not used positions in the antecedent 
#    beta  : a vectorized membership function (vectorized means
#            that when applied to a vector of points it
#            returns a vector of corrsponding function values)
#  returns a function that when applied to a vector of lenght(alphas)
#            returns the (vectorized) function beta', the inferred fuzzy set.
mkrule = function(alphas, beta)
  function(singleton) implication(beta, min(zipapply(alphas, singleton)))

# combinator(rules, x)
#  union combinator
#    rules: list of rules (each as created by mkrule)
#    x    : input point representing the singleton premise
#  returns a function that applies all the rules and combines
#  the results as a union
combinator = function(rules, x)
  function(y) apply(sapply(rules, function(r,y) r(x)(y), y),1,max)

# infer(rules, x, samplepoints)
#  inference engine applying rules to point x
#   rules : list of rules (each as created by mkrule)
#   x     : input point (singleton premise)
#   samplepoints : the inferred results are computed on these points
#  returns a vector of the inferred fuzzy set evaluated on samplepoints
infer = function(rules, x, samplepoints)
  combinator(rules, x)(samplepoints)

# cog(fuzzy.set, samplepoints)
#  computes the center of gravity for fuzzy set values
#  corresponding to the elements given in samplepoints
#   fuzzy.set   : the fuzzy memberships corresponding to samplepoints
#   samplepoints: the elements for which the fuzzy.set contains memberships 
cog = function(beta.values, samplepoints){
  bs = sum(beta.values)
  if (bs == 0) 0 else sum(samplepoints*beta.values)/bs
}

###### End Fuzzy Inference Definitions


###### Utility triangular, shoulder and step membership function maker function

# mkmem(left, top, right, fact=1)
#  construct triangular membership function
#   left:   point where left leg intersects  0
#   right:  point where right leg intersects 0
#   top  :  point where left and right leg meet
#   fact :  height of function at top.
#   name :  optional name for function
#  left or right given as -Inf or Inf, respectively
#  results in shoulder type membership functions.
mkmem = function(left, top, right, heigth=1, name=NULL){
  f = function(x) triang(x, left, top, right) * heigth
  if(!is.null(name)) attr(f,'name') = name
  return(f)
}

# helper functions
ramp = function(x, v1, v2) 
  if (v1 == v2) (x > v1) + 0 else pmin(1, pmax(0, (x - v1)/(v2-v1)))
triang = function(v, l, t, r) sapply(v, function(x){ 
  if(x <= t) {if(l == -Inf) return(1) else return(ramp(x,l,t))}
  if(r == Inf)  return(1) else return(1 - ramp(x, t, r))
})

# that's all folks

