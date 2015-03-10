# -*-Python-*-
################################################################################
#
# File:         dummify.py
# RCS:          $Header: $
# Description:  
# Author:       Staal Vinterbo
# Created:      Tue May  5 07:13:12 2009
# Modified:     Fri May  8 12:54:10 2009 (Staal Vinterbo) staal@ball
# Language:     Python
# Package:      N/A
# Status:       Experimental
#
# (c) Copyright 2009, Staal Vinterbo, all rights reserved.
#
################################################################################

from random import random, sample
from operator import add

#### restoration stratgy functions

def wheel(probs):
        '''sample index with probability'''
        if len(probs) == 0:
            raise ValueError, 'cannot draw from empty distribution'
        sp = sum(probs)
        if sp == 0:
            return sample(range(len(probs)), 1)
        t = random()*sp # scale
        tot = 0.0
        for i, v in enumerate(probs):
            tot += v
            if t <= tot:
                return i
        raise AssertionError, "internal draw error"

def maxi(vals):
    '''return the index of the largest argument'''
    return max(range(len(vals)), key = lambda i: vals[i])

#### dummify/restore

def mutate(l, i, v):
    '''set l[i] = v'''
    l[i] = v
    return l

def gendomfn(codomain, dedum):
    '''generate dummyfy, restore function of an ordered codomain'''
    dmap = dict((v,i) for (i,v) in enumerate(codomain))
    return (lambda v : mutate([0]*len(codomain), dmap[v], 1),
            lambda vals : codomain[dedum(vals)])

def gendomf2(codomain, dedum):
    '''generate dummyfy, restore function of a size 2 ordered codomain'''
    return (lambda v : [int(v != codomain[0])],
            lambda vals: codomain[dedum([1-vals[0], vals[0]])])

def gendomf1(codomain, dedum):
    '''generate dummyfy, restore function of a constant codomain'''
    return (lambda v : [0], lambda vals: codomain[0])


def dedumslice(fun, sel):
    '''returns function that returns fun applied to slice sel of argument'''
    return lambda v : fun(v[sel])


# compute functions that can be applied to vectors/lists
def dummifyf(m, which, dedum=maxi):
    place = 0
    funs = []
    for i,col in enumerate(map(list, zip(*m))):
        cdom = sorted(set(col))
        lc = len(cdom)
        fun, inc = [(lambda a,dedum=None : (lambda v: [v], lambda x : x[0]), 1),
                    (gendomf1,1), (gendomf2,1),
                    (gendomfn,lc)][int(i in which)*min(lc,3)]         
        sel = slice(place, place + inc)
        place += inc
        df, rf = fun(cdom, dedum=dedum)
        funs.append((df,dedumslice(rf,sel)))
    return funs

#### convenience application functions

def applydf(m, funs):
    '''apply dummifying functions to iterable of lists/arrays'''
    f = lambda r:reduce(add, map(lambda ((df,rf),v):df(v), zip(funs,r)),[])
    return map(f, m)

def applyrf(m, funs):
    '''apply restoring functions to iterable of lists/arrays'''
    return map(lambda r: map(lambda (df,rf): rf(r), funs), m)

if __name__ == '__main__':

    from numpy.linalg import svd
    from numpy import dot, array, diag
    from random import shuffle

    def shuffled(l):
        shuffle(l) # does in-place shuffling
        return l

    m = [['a',1,'2',3],
         ['a',1,'3',2],
         ['a',0,'1',1]]

    print('original matrix')
    print(m)
    funs = dummifyf(m, [0,1,2,3])
    md = applydf(m, funs)
    print('dummified matrix:')
    print(md)
    u,s,vh = svd(array(md),0)
    ua = array(map(list, zip(*map(shuffled, map(list, zip(*u))))))
    aa = dot(ua,dot(diag(s),vh)) 
    mar = applyrf(aa, funs)
    mr = applyrf(md, funs)
    print('Original reconstructed dummified matrix:')
    print(mr)
    print('spectral shuffled reconstructed dummified matrix:')
    print(mar)

