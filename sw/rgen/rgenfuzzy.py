# -*-Python-*-
################################################################################
#
# File:         rgenfuzzy.py
# RCS:          $Header: $
# Description:  Create drop-in functions for rgen.py module
# Author:       Staal Vinterbo
# Created:      Thu Apr  2 13:59:21 2009
# Modified:     Tue Apr  7 14:31:53 2009 (Staal Vinterbo) staal@ball
# Language:     Python
# Package:      rgen
# Status:       Experimental
#
# rgenfuzzy.py is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# rgenfuzzy.py is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with rgenfuzzy.py; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# (c) Copyright 2009, Staal Vinterbo, all rights reserved.
#
################################################################################
#
# Revisions:
#
# Tue Apr  7 14:31:32 2009 (Staal Vinterbo) staal@ball
#  Fixed oops in '?' handling code (moved it into ramp())
# Mon Apr  6 19:31:22 2009 (Staal Vinterbo) staal@ding
#  Included code to handle missing values encoded as '?'
# Fri Apr  3 00:12:36 2009 (Staal Vinterbo) staal@ding
#  Initial revision.
################################################################################
'''supply drop in functions for rgen.py to enable fuzzy rules

Supplies two drop in functions 'makeFuzzyA' and 'fuzzy_descriptor_code' 
to be used instead of 'makeA' and 'fuzzy_descriptor_code', respectively.

usage:

 import this module and after importing rgen but before using makeA and 
 descriptor_code, and then do::

  makeA = makeFuzzyA
  descriptor_code = fuzzy_descriptor_code

See for example rgen.py how this is used.

'''
__all__ = ['makeFuzzyA', 'fuzzy_descriptor_code']

from collections import defaultdict
from string import Template
from math import modf, floor

def ramp(v,w):
    '''generate ramp function that goes from 0 to 1 linearly in [v,w]'''
    return (lambda x : 0 if x == '?' else (
                1 if x >= w else
            (0 if x <= v else (float(x) - v)/(w - v))))
            
def tri(u,v,w):
    '''generate triangular function'''
    if u == None and w == None: return lambda x : 1
    if u == None: return lambda x: 1 - ramp(v,w)(x)
    if w == None: return ramp(u,v)
    return lambda x : (ramp(u,v)(x) if x <= v else 1 - ramp(v,w)(x))

def gen_tri(cuts):
    '''generate a list of triangular functions from list of reals'''
    cuts = [None] + sorted(cuts) + [None]
    return [tri(*cuts[i:i+3]) for i in range(len(cuts) - 2)]
    
def gen_bin_cuts(v, n):
    '''generate n equally spaced cuts spanning the range of v'''
    part = (float(max(v)) - min(v))/(n+1)
    return [min(v) + (i+1)*part for i in range(n)]

def quantile(y, q):
    '''compute quantile q in data y'''
    n, y, a, b, c, d = len(y), sorted(y), 1,  -1, 0, 1
    g, j = modf(a + (n+b) * q - 1)
    if j < 0: return y[0]
    elif j > n: return y[n]
    j = int(floor(j))
    return y[j] if g == 0 else y[j] + (y[j+1] - y[j]) * (c + d * g)

def gen_cuts(v, n):
    '''generate n quantile based cuts'''
    return [quantile(v,q) for q in gen_bin_cuts([0,1], n)]

class FuzzyA(list):
    '''extra wrapper class to put household info into'''
    pass

def disc(v, memfuns):
    return [max(range(len(memfuns)), key = lambda i: memfuns[i](x)) for x in v]

def makeFuzzyA(M, n = 3):
    ''' make ordered set A of fuzzy attribute functions'''
    mt = map(list, zip(*M))
    d = dict()
    newt = []
    for i,col in enumerate(mt):
        if (len(set(col)) > n and
            all(type(x) == int or type(x) == float for x in col)):
            cuts = gen_cuts(col, n = n)
            newt.append(disc(col, gen_tri(cuts)))
            d[i] = cuts
        else:
            newt.append(col)
            d[i] = []
    def promise(column):
        return lambda i : column[i] if i < len(column) else '?'
    A = FuzzyA(map(promise,newt))
    A.d = d
    return A


def fuzzy_descriptor_code(A,X):
    '''generate a string for descriptor(a,av) function'''
    dc = Template('''
def descriptor(a, av):
    def tri(u,v,w):
        def ramp(v,w):
           return (lambda x : 0 if x == '?' else (
                       1 if x >= w else
                   (0 if x <= v else (float(x) - v)/(w - v))))
        if u == None and w == None: return lambda x : 1
        if u == None: return lambda x: 1 - ramp(v,w)(x)
        if w == None: return ramp(u,v)
        return (lambda x : (ramp(u,v)(x) if x <= v else 1 - ramp(v,w)(x)))
    d = $d
    if not d[a]:
        return lambda y: 1 if (av==y and y!='?') else 0
    return tri(*([None] + d[a] + [None])[av:av+3])
''')
    return dc.substitute({'d': repr(A.d)})
