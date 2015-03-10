#!/usr/bin/env python
################################################################################
#
# File:         rgen.py
# RCS:          $Header: $
# Description:  rule generation prototype
# Author:       Staal Vinterbo
# Created:      Wed Apr  1 14:13:10 2009
# Modified:     Tue Apr 28 17:59:27 2009 (Staal Vinterbo) staal@ding
# Language:     Python
# Package:      rgen
# Status:       Experimental
#
# rgen.py is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# rgen.py is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with rgen.py; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# (c) Copyright 2009, Staal Vinterbo, all rights reserved.
#
################################################################################
#
# Revisions:
#
# Tue Apr 28 17:58:33 2009 (Staal Vinterbo) staal@ding
#  Fixed error in the computation of stopping criterion for mda(r).
# Fri Apr 24 16:35:34 2009 (Staal Vinterbo) staal@ding
#  Fixed oops in mda, error in selecting next attribute in solution.
# Wed Apr  8 09:38:36 2009 (Staal Vinterbo) staal@ball
#  Added mda and mdar and their use unless --reference flag is given.
# Tue Apr  7 14:13:40 2009 (Staal Vinterbo) staal@ball
#  Removed partial matching option and made partial matching permanent
#  in the case where non-partial fails.
# Mon Apr  6 18:35:34 2009 (Staal Vinterbo) staal@ding
#  Added partial rule matching and last class strength only options
#  to generated classifier.
# Thu Apr  2 11:28:18 2009 (Staal Vinterbo) staal@ding
#  Initial revision.
################################################################################
'''generate a rule based classifer learned from input data.

%(prog)s
========

:Author: Staal A. Vinterbo
:Copyright: 2009 Staal A. Vinterbo
:Version: generated for %(version)s
:Availability: `GPL <http://www.gnu.org/copyleft/gpl.html>`_
:URI: `%(url)s <%(url)s>`_


**Contents**:
  - `Module Synopsis`_
  - `Standalone Invocation`_
  - `Theory`_
  

Module Synopsis
---------------

Let m be a data matrix (list of rows) where columns represent 
attributes and rows represent the attribute values for objects. Then::

 X, A = makeX(m), makeA(m)
 P = makeP(X)
 I = greedy_cover([P(a) for a in A(m)]))

computes (currently somewhat inefficiently, due to the size of P(a)) the 
approximation of a minimum index set I such that distinct rows in m remain 
distinct when m is projected onto the columns indexed by I.

Unless A is significantly larger than X, the same can be computed more
efficiently as::

 mda(A, X)

If we consider the last column in m to be the class labels, the code::

 X, A = makeX(m), makeA(m)
 S = makeS(X)
 z = X[0]
 I = greedy_cover([S(z)(a) & S(z)(A[-1]) for a in A[:-1]])

computes the index set I of attributes from which we compute the antecedent 
for the rule instantiated from the first row in m.

Unless A is significantly larger than X, the same can be computed more
efficiently as::

 I = mdar(A[:-1], X, z, A[-1])

Continuing::

 exec(descriptor_code(A,X))
 r = eval(rule_descriptor_code(I,z))

computes a function r that when when applied to a data point p yields a 
tuple (o,v) where o is the class label and v is the rule strength. Example::

 r(m[0])

applies r to the first row in m.

Also, if m is::

 m = [[1, 2, 0],
      [1, 3, 1],
      [2, 2, 1]]

the code::

 print(rule_descriptor_code(I,z))

prints::

 lambda p,f = (lambda x:x) : (0,
             min(f(map(lambda (a, av, pv): descriptor(a,av)(pv),
             zip([0, 1], [1, 2], [p[i] for i in [0, 1]])))))

The f argument in the lambda above allows processing of the list of values
returned by the individual propositions in the rule antecedent. An example
use is to implement partial antecedent matching in that a rule is allowed 
to apply to a point even though it is not contained in one or more of
the antecedent propositions' truth sets.

The code in this module allows the more or less direct implementation of the 
theory description. One change is that P(a)(z) is more efficiently computed 
as S(z)(a). Furthermore, the equivalence predicate takes the symbol '?' for 
missing values into account.


Standalone Invocation
---------------------

Run as a standalone program this module will compute a rule based classifier
and print a python program that implements it.

Note that the rectangular data set required as input is assumed to contain
attribute names on the first line. Each subsequent line contains the
attribute values for an object. The last attribute, i.e., last column, is 
taken to contain class labels. 

Try (assuming a shell command line)::
 
 $ python %(prog)s -h
 $ python %(prog)s -e | less
 $ python %(prog)s dataset.txt | less
 $ python %(prog)s dataset.txt | python - h
 $ python %(prog)s dataset.txt | python - dataset.txt
 $ python %(prog)s dataset.txt | python - -c dataset.txt

To generate a html version of this short explanation::

 $ python %(prog)s -e | rst2html > explanation.html

rst2html is a part of the python docutils package 
http://docutils.sourceforge.net/docs/

Note that the standalone program can also attempt to reduce the number
of rules by filtering. This filtering tries to cover all instances in 
the training data with as few rules that have the same label in the 
antecedent as the instances they cover.

Fuzzy Rules
~~~~~~~~~~~

If there is a loadable module 
`%(companion)s <%(companion)s.py>`_ then the option of generating fuzzy 
rules is enabled. 

With this module loaded the fuzzy rules are computed much in the same way as 
described in the paper::

 Vinterbo, S.A., Kim, E. & Ohno-Machado, L
 Small, fuzzy and interpretable gene expression based classifiers. 
 Bioinformatics Vol. 21(9), pp. 1964-1970, 2005.

`Find the above Bioinformatics paper here.
<http://dx.doi.org/10.1093/bioinformatics/bti287>`_

Theory
------

Let U be a set of objects, let X be a subset of U, 
and let A be a set of functions from U to some set Y.
Further let l be function from U to a set L of class labels.

The goal is that only given the restriction of l to X, we wish to represent an 
as large extension of l as possible using X. We want to represent this extension
as classification rules for elements in U using A. 
In order to generalize as best as possible we adopt the strategy of creating 
as short rule antecedents as possible. A principled way of going about this is 
as follows.

Rules
~~~~~
 
Let a rule be a function r on X that when applied to an element x returns 
a tuple (l, v) where l is the class label associated with the consequent of 
the rule and v is the value of 'applying' the antecedent of the rule. 

We will conveniently represent rule r as a tuple r = (c, alpha) 
of antecedent and class label, respectively. We can then, abusing notation 
slightly, compute r(x) as::

 r(x) = (c, alpha(x)).

This computation can be interpreted as assigning c to x with strength alpha(x). 
The tuples resulting from an application of rules in a set of rules can
be combined to yield an assignment of strengths to the possible class
labels. This assignment can in turn be used to make an assignment of 
label to x.

Propositions over U and antecedent functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By a proposition over U we mean a characteristic function of some subset of U.

A rule antecedent alpha is constructed as a conjunction of such propositions.
We can view such a conjunction as a set. If alpha is the conjunction::

  p_i and p_j and ... and p_k

we can represent alpha as a set::

  alpha = {p_i, p_j, ...,  p_k}

and (again abusing notation) compute alpha(x) as::

  min {p(x) | p in alpha}.

Given a function a in A and an element x in U we can form a proposition p(a,x)
over U that is the characteristic function of::

 T(p(a,x)) = { y in U | a(y) = a(x) },

i.e, p(a,x)(z) = 1 iff z lies in T(p(a,x)).

Given a subset A' of A and a fixed element z in X, we can form a 
conjunction ant(z) as::

 ant(z) = {p(a, z) | a in A'}.

(Recall that we compute ant(z)(x) for some x as min {p(x) | p in ant(z)}).

We will translate the the wish of a maximal extension of the classifier, or 
labelling function, l, into a criterion for rule r = (c, ant(z)):

 the antecedent should be as short as possible while not being applicable
 to any elements y in X that have a different class label, i.e.,
 we require that ant(y) = 0 if l(y) != l(z) (if y and z are discernable
 by A, that is).

Let f be any function on X (note that we assume that we have a predicate '='
that lets us determine equivalence and discernibility for each point in the 
image of X under f), then we define the set of pairs of elements from X that 
we can discern between by the use of f as::

 P(f) = {(x,y) in X^2 | not f(x) = f(y) }

Given an element x in X we have that the set of elements in Y 
that are discernable from x by f as::

 P(f)(x) = { y in X | not f(x) = f(y) }

The criterion on ant(z) using A' can then be formulated in terms of X as
as::

  | D(A') | / | D(A) | >= t

where::

  D(B) =  intersection {union {P(a)(z) | a in B}, P(l)(z)}.

If t = 1 then the requirement is strict. However, we can avoid overfitting 
if t < 1. Doing that reduces the correctness of the rule on X however.
A more detailed description of the function of t above can be found in::

 Vinterbo, S. & Ohrn, A.
 Minimal Approximate Hitting Sets and Rule Templates.
 International Journal of Approximate Reasoning, Vol. 25(2), pp. 123-143, 2000.

`Find the above IJAR paper here.
<http://dx.doi.org/10.1016/S0888-613X(00)00051-7>`_


Finding a minimum A' such that ant(y) = 0 if l(y) != l(z) is provably NP-hard
(for a proof of this and approximation properties of the case where l is 
injective see `here <http://dx.doi.org/10.1186/1471-2105-7-8>`_).

We can transform it to a standard set covering problem for the following 
collection of sets:: 

  [P(a)(z) & P(l)(z) | a in A].

Let set_cover(z) be a function that computes the above cover, then
a rule r can be computed by::

  r = (l(z), {p(a_i, z) | i in set_cover([P(a)(z) & P(l)(z) | a in A])}).


'''

Version = "0.07"

__all__ = ['makeA', 'makeP', 'makeX', 'makeS','greedy_cover',
           'xprod', '_eq',
           'rule_descriptor_code', 'descriptor_code', 'mda', 'mdar']

from operator import add, or_
from collections import defaultdict
from string import Template
from random import shuffle
from math import floor

def xprod(LL):
    """computes the cartesian product of a list of lists.

    >>> print [x for x in xprod([[1,2],[3,4]])
    [[1, 4], [1, 5], [2, 4], [2, 5], [3, 4], [3, 5]]
    """
    if LL:
        for x in xprod(LL[:-1]):
            for y in LL[-1]:
                yield x + [y]
    else:
        yield []

def _eq(x,y):
    '''in-discernibility predicate with missing values incorporated'''
    return(x == y or x == '?' or y == '?')

def makeA(M):
    ''' make ordered set A of attribute functions

    each function in A corresponds to a column in m and accepts
    arguments from {0,1,...,n-1} where n is the number of rows in m.'''
    def promise(column):
        return lambda i : column[i] if i < len(column) else '?'
    return map(promise,map(list, zip(*M)))

def makeX(M):
    '''make ordered set X of object identifiers from m'''
    return range(len(M))

def makeP(X):
    '''make P such that P(a) = {(x,y) in X^2 | a(x) != a(y) }'''
    return lambda a : set((x,y) for (x,y) in xprod([X,X]) if not _eq(a(x),a(y)))

def makeS(X):
    '''make S such that S(x)(a) = { y in X | a(x) != a(y) }'''
    return lambda x : (lambda a : set(y for y in X if not _eq(a(x),a(y))))

def greedy_cover(C, tau=1.0):
    '''greedy setcover algorithm

    C - a list of sets ('C' for collection of sets)
    tau  - fraction of cover we need to cover cover with elements from coll

    returns a list of indices of sets in C such that their union contains 
    at least a fraction tau of the union of all elements in C
    '''
    X = reduce(or_, C, set()) # X - elements we still need to cover
    if len(X) == 0:    # sanity check
        return []
    notused, S, n = set(range(len(C))), [], len(X)
    while notused and float(n - len(X))/n < tau:
        i = max(notused, key = lambda i: len(X & C[i]))
        S.append(i)
        X -= C[i]
        notused.remove(i)
    return S

def filter_rules(rules, points, tau=0.99, cover_fraction = 1.0):
    '''filter rule set'''
    coll = []
    def eval_point(p,r):
        (o,v) = r(p)
        return -v if o != p[-1] else v 
    for r in rules:
        s = filter(lambda (i,p) : eval_point(p,r) >= tau, enumerate(points))
        coll.append(set(t[0] for t in s))
    return greedy_cover(coll, cover_fraction)

def rule_descriptor_code(D,x):
    '''generate a string that contains a python lambda for the rule.

       The f argument for the lambda was included to allow for partial
       matching of rule antecedent to be employed.
       '''
    d = dict(vals=repr(map(lambda i: A[i](x), D)),
             D=repr(D),
             o=repr(A[-1](x)))
    return Template(
        '''lambda p,f = (lambda x:x) : ($o,
             min(f(map(lambda (a, av, pv): descriptor(a,av)(pv),
             zip($D, $vals, [p[i] for i in $D])))))''').substitute(d)

def descriptor_code(A,X):
    '''implement the individual (crisp) propositions'''
    return "descriptor = lambda a,av: lambda y: 1 if (av==y and y!='?') else 0"


def rule_string(D,x):
    '''compute a human readable rule'''
    dnames = [anames[a] for a in D]
    descriptors = [name + "=" + repr(value) for (name, value) in
                   zip(dnames, map(lambda i: A[i](x), D))]
    antecedent = ' and '.join(descriptors)
    return ' => '.join([antecedent, repr(A[-1](x))])

### helper functions below    

def simple_read_table(f):
    '''read an input data table'''
    rex = re.compile('[\s,:]') # separators are whitespace, ',' and ':'
    m = [rex.split(line.strip()) for line in f]
    # remove empty and comment lines (start with '#')
    m = filter(lambda r: (not all(map(lambda x: x == '', r))
                          and (r[0] == '' or r[0][0] != '#')), m)
    anames = m[0]
    m = m[1:]
    newmt = []
    for col in map(list, zip(*m)):
        try:
            newmt.append([int(x) for x in col])
        except ValueError:
            try:
                newmt.append([float(x) for x in col])
            except ValueError:
                newmt.append(col)
    m = map(list, zip(*newmt))
    return (anames, m)
    
def split_data(m, pcent = 0.3):
    '''split data set, preserving outcome distribution'''
    nrow = len(m)
    target = map(list, zip(*m))[-1]
    md, fd = [], []
    for o in set(target):
        idxs = [i for i in range(nrow) if target[i] == o]
        shuffle(idxs)
        mn = int(floor(pcent * len(idxs)))
        fd.extend(m[i] for i in idxs[:mn])
        md.extend(m[i] for i in idxs[mn:])
    return (md, fd)


##### Faster implementations

def npairs(s):
    '''computes sum_{i=0}^{len(s) - 2} sum_{j=i+1}^{len(s) -1} s[i]*s[j].

       if s is the list of sizes of equivalence classes induced by a function
       on a set, the above is the number of pairs in that set that the
       function discerns between.
    '''
    acc, prev = 0, 0
    for x in s:
        acc += x * prev
        prev = x + prev
    return acc

def countp(LL):             # O(log(|X|)*(sum_l |l|) < O(log(|X|)|X|^2)
    '''compute the number of pairs of elements that are discerned by cover.

    Note that in the case of missing values, what is usually a
    partition is not a partition anymore but a cover.
    As an example, [[1,2,3],[4,5,3]] means
    that for X = [1,2,3,4,5] we have that 3 cannot be discerned from
    the others, while pairs [(1,4),(1,5),(2,4),(2,5)] are discerned as
    they lie in different cover elements.
    '''
    d = defaultdict(list)
    sizes = []
    for i,l in enumerate(LL):
        sizes.append(len(l))
        for x in l:
            d[x].append(i)
    orgsizes = [s for s in sizes]
    sub = 0
    for key,occs in d.items():
        for where in occs[1:]:
            sizes[where] -= 1
            sub += (orgsizes[where] - 1)
    return npairs(sizes) - sub


def part(X, a):
    '''partitions X into equivalence classes according to a.

       the equivalence classes are returned as a list of lists.
    '''
    d = defaultdict(lambda : [])
    for x in X:
        d[a(x)].append(x)
    m = d['?']
    del d['?']
    return [l + m for l in d.values()]

def partition(ll):
    '''returns function returning a refinement of ll according to argument.'''
    return lambda a : reduce(add, map(lambda l : part(l,a), ll), [])


# Much faster computation of 
#   greedy_cover([P(a) & P(A[-1]) for a in A[:-1]], tau)
# as
#   mda(A[:-1], X, A[-1], tau)
def mda(A, X, l = lambda x:x, tau = 1.0):
    '''approximates the minimum set of discerning attributes

       A - set of attributes
       X - iterable of elments
       l - class label function
       tau - fraction of differently labelled pairs we must discern

       mda computes an approximately minimal set of indexes into A 
       that discern between a fraction tau of the element pairs that l discerns
       between.
    '''
    # compute how many pairs we need to cover...
    X = filter(lambda x : l(x) != '?', X)
    AP = reduce(lambda LL,a : partition(LL)(a), A, [X]) # is O(|A|*|X|*log(|X|))
    Apairs = countp(AP)
    lAP = partition(AP)(l)
    lApairs = countp(lAP)
    notpossible = lApairs - Apairs # the pairs of l that A is not able to cover

    # the two values we need: 
    #   note: lpairs - notpossible = no of l pairs that we can discern by A 
    lpairs = countp(part(X, l))
    want = int(floor(tau*(lpairs - notpossible)))

    notused, S, XP, covered = set(range(len(A))), [], [X], 0
    while notused and covered < want: 
        PP = partition(XP)
        PP2 = partition(PP(l))
        (rest,i) = min(map(lambda i: (countp(PP2(A[i])) -
                                      countp(PP(A[i])), i), notused))
        XP = PP(A[i])
        covered = lpairs - rest
        S.append(i)
        notused.remove(i)
    return S

# Much faster computation of 
#   greedy_cover([S(x)(a) & S(x)(A[-1]) for a in A[:-1]], tau)
# as
#   mdar(A[:-1], X, x, A[-1], tau)
def mdar(A, X, x, l = lambda x:x, tau = 1.0):
    '''approximates the minimum set of attributes discerning x

       A - set of attributes
       X - iterable of elments
       x - the element x to be discerned from
       l - class label function
       tau - fraction of differently labelled pairs we must discern

       mdar computes an approximately minimal set of indexes into A 
       that discern x from a fraction tau of the elements for which 
       l yields a distinct value.
    '''
    if l(x) == '?':
        return [] # cannot discern a missing value

    # elements we are interested in discerning
    U = filter(lambda y : l(x) != l(y), X)

    # function that returns filter function on argument
    aleft = lambda U: (lambda a: filter(lambda y : _eq(a(x),a(y)), U))

    # find out which elements we are not able to discern from x by A
    AU = reduce(lambda U,a : aleft(U)(a), A, U)

    # adjust U to those we are actually able to discern
    U = sorted(set(U) - set(AU))

    leave = len(U) - int(floor(tau*len(U)))
    notused, S = set(range(len(A))), []
    while notused and len(U) > leave:
        (rest, i) = min(map(lambda i : (len(aleft(U)(A[i])), i), notused))
        S.append(i)
        notused.remove(i)
        U = aleft(U)(A[i])
    return S
        
    
    

if __name__ == "__main__":

    # the code below roughly does 5 things in order
    # 1. set up command line parser and parse the commandline
    # 2. read data, split if filtering, compute statistics
    # 3. compute classification rules
    # 4. filter rules if requested
    # 5. generate and print python executable code

    try:
        import psyco
        psyco.full()
        pass
    except ImportError:
        pass
    from urllib import urlopen
    from optparse import OptionParser
    from random import seed
    import re
    import sys
    import os.path

    # set up some globals 
    progname = os.path.basename(sys.argv[0])
    url = progname
    companion = 'rgenfuzzy' # name of companion fuzzy enabling module

    # see if we can do the fuzzy thing...
    have_fuzzy = False
    try:
        import rgenfuzzy
        have_fuzzy = True
    except ImportError:
        pass

    ### set up parser and parse command line


    parser = OptionParser(usage = "".join(
        ["%prog [options] URL\n",
         "\n Version: ", Version, " SAV (C) 2007\n",
         " Generate classification rule classifier and print to stdout.\n\n",
         " URL points to a rectangular data table where the first line\n",
         " contains attribute names. Table entries are separated by /\s,:/.\n",
         " URL can also be a simple filename or '-' for stdin.\n"]),
                          version = ''.join(["%prog ",Version," (C) SAV 2007"]))
    parser.add_option("-v", "--verbose", dest="verbose",
                      default=False, action="store_true",
                      help="print progress and other info to stderr.")
    parser.add_option("-f", "--filter", dest="filter",
                      default=False, action="store_true",
                      help="filter ruleset.")
    parser.add_option("-s", "--suppress", dest="suppress",
                      default=False, action="store_true",
                      help="suppress the writing of human readable rules.")
    parser.add_option("-t", "--tau", dest="tau", type="float", metavar="REAL",
                      default = 1.0, help="Set tau parameter in [0,1] range "
                      "(default: %default).")
    parser.add_option("-r", "--rulethreshold", dest="rtau",
                      type="float", metavar="REAL", default = 1.0,
                      help="Set rule filter threshold in [0,1] range "
                      "(default: %default).")
    parser.add_option("-R", "--rulefraction", dest="rfraction",
                      type="float", metavar="REAL", default = 1.0,
                      help="Set rule filter cover fraction in [0,1] range "
                      "(default: %default).")
    parser.add_option("-p", "--filterfraction", dest="filterp",
                      type="float", metavar="REAL", default = 0.3, 
                      help="data fraction in [0,1] range to use for filtering "
                      "(default: %default).")
    parser.add_option("-S", "--seed", dest="seed",
                      type="int", metavar="INT",
                      default = None, help="set random seed "
                         "(default: %default).")
    parser.add_option("-e", "--explain", dest="explain",
                      default=False, action="store_true",
                      help="print description of what this program does.")
    parser.add_option("-m", "--mda", dest="mda",
                      default=False, action="store_true",
                      help="Use total minimal number of attributes rules.")
    parser.add_option("--reference", dest="reference",
                      default=False, action="store_true",
                      help="Use slower reference computations.")

    if have_fuzzy:
       parser.add_option("-c", "--cuts", dest="cuts", type="int", metavar="NUM",
                         default = 3, help="Set number of cuts to use. "
                         "(default: %default).") 
       parser.add_option("-F", "--Fuzzy", dest="fuzzy",
                         default=False, action="store_true",
                         help="Generate fuzzy rules.")

    (opt, args) = parser.parse_args()


    if opt.explain:
        print(__doc__ % {'prog': progname,
                         'version' : Version,
                         'url': url,
                         'companion':companion})
        sys.exit(0)

    if len(args) < 1:
        print "Missing input file. Try -h for help.\n"
        sys.exit(1)

    if opt.seed:
        seed(opt.seed)

    if args[0] == '-':
        datafile = 'standard input'
    else:
        datafile = os.path.basename(args[0])

    ### read data, compute priors, and split if filtering

    f = (args[0] == '-' and sys.stdin or urlopen(args[0]))
    (anames, m) = simple_read_table(f)

    # compute priors
    target = map(list, zip(*m))[-1]
    priors = defaultdict(lambda : 0)
    for o in target:
        priors[o] += 1.0/len(target)
    (defclass, prevalence) = max(priors.items(), key = lambda (k,c) : c)

    # split data if filter is requested
    # m is used to induce rules and filterdata is used to 
    # remove 'redundant' rules
    if opt.filter:
        (m, filterdata) = split_data(m, opt.filterp)


    ### compute rules

    # use drop in functions from rgenfuzzy if availble and requested
    if have_fuzzy:
        if opt.fuzzy:
            descriptor_code = rgenfuzzy.fuzzy_descriptor_code
            makeA = lambda m: rgenfuzzy.makeFuzzyA(m,opt.cuts)

    # compute representations
    X,A = makeX(m),makeA(m)
    P,S = makeP(X), makeS(X)

    # minimal total number of antecedent attributes?
    if opt.mda:
        if opt.reference:
            lx = P(A[-1])
            mdaS = sorted(greedy_cover([P(a) & lx for a in A[:-1]], opt.tau))
        else:
            mdaS = sorted(mda(A[:-1], X, A[-1], opt.tau))

    # compute per object rules
    lambdas = []
    rulestrings = []
    for i,x in enumerate(X):
        if opt.reference:
            Lx = S(x)(A[-1])
            D = mdaS if opt.mda else \
                sorted(greedy_cover([S(x)(a) & Lx for a in A[:-1]],
                                    tau=opt.tau))
        else:
            D = mdaS if opt.mda else sorted(mdar(A[:-1],X,x,A[-1],opt.tau))
        if D:
            lambdas.append(rule_descriptor_code(D,x))
            if not opt.suppress:
                rulestrings.append(str(i) + ': ' + rule_string(D,x))


    ### filter the rules if requested
    if opt.filter:
        exec(descriptor_code(A,X))
        I = sorted(filter_rules([eval(l) for l in lambdas], m,
                                opt.rtau, opt.rfraction))
        if len(I) == 0: # removed all rules, reinstate all rules
            I = range(len(lambdas))
        lambdas = [lambdas[i] for i in I]
        rulestrings = [rulestrings[i] for i in I]

    ### output computed rules as executable python program

    # output human readable information to the generated python program
    if not opt.suppress:
        print("'''")
        print('This program was generated by ' + progname + '.')
        print('Learned from ' + datafile +
              ': ' + str(len(X)) + ' x ' + str(len(A)))
        if have_fuzzy:
            if opt.fuzzy:
                print('Fuzzy rules were generated using ' + str(opt.cuts) +
                      ' fuzzy sets per data dimension.')
        print('most common class: ' + str(defclass) + ', prevalence: '
              + str(prevalence))
        if opt.filter:
            print('Filtered on ' + str(len(filterdata)) + ' points')
        print(str(len(rulestrings)) + ' rules:')
        for rs in rulestrings:
            print(rs)
        print("'''")

    # Print the ouput code.
    print(Template('''
__all__ = ['combinators', 'classify', 'simple_read_table']

from collections import defaultdict
from operator import add

combinators = {'max':max, 'sum':add}



$rules

$descriptor_code

def classify(points, rules, combinator = add, prob = False):
    """classify a list of points using a list of rules

    computes a dict of class label strengths per point and returns as a list
    if none of the rules are applicable, i.e., all return strength 0
    the item is classified according to training set prevalences.
    """
    preds = []
    for point in points:
        d = defaultdict(lambda : 0)
        for (outcome, value) in map(lambda r: r(point), rules):
            d[outcome] = combinator(d[outcome], value)
        n = float(sum(d.values()))
        if not n: # re-attempt with partial matching
            for (outcome, value) in map(lambda r: r(point,partial), rules):
                d[outcome] = combinator(d[outcome], value)
            n = float(sum(d.values()))
            if not n:
                d = defaultdict(lambda : 0, $priors)
                n = float(sum(d.values()))
        if n and prob:
            for k in d.keys():
                d[k] /= n
        preds.append(d)
    return preds

def simple_read_table(f):
    """reads a rectangular data file with attribute names on first line"""
    rex = re.compile('[\s,:]') # separators are whitespace, ',' and ':'
    m = [rex.split(line.strip()) for line in f]
    # remove empty and comment lines (start with '#')
    m = filter(lambda r: (not all(map(lambda x: x == '', r))
                          and (r[0] == '' or r[0][0] != '#')), m)
    anames = m[0]
    m = m[1:]
    newmt = []
    for col in map(list, zip(*m)):
        try:
            newmt.append([int(x) for x in col])
        except ValueError:
            try:
                newmt.append([float(x) for x in col])
            except ValueError:
                newmt.append(col)
    m = map(list, zip(*newmt))
    return (anames, m)

def partial(l):
    """transform antecedent proposition match list to allow for partial match.

       all elements are divided by 1/(1+#non-matched-propositions^2) in order
       to penalize only partial matches.
    """
    nonz = filter(lambda x: x>0, l)
    if not nonz:
        return l
    return map(lambda x : x/(1.0 + (len(l)-len(nonz))**2), nonz)


if __name__ == "__main__":

    try:
        import psyco
        psyco.full()
        pass
    except ImportError:
        pass
    from urllib import urlopen
    from optparse import OptionParser
    import re
    import sys

    Version = $version
    parser = OptionParser(usage = "".join(
        ["%prog [options] URL\\n"
         "\\n Version: ", Version, " SAV (C) 2009\\n",
         " Apply rules learned from data read from $dfile.\\n\\n",
         " URL points to a rectangular data table where the first line\\n",
         " contains attribute names. Table entries are separated by /\s,:/.\\n",
         " URL can also be a simple filename or '-' for stdin.\\n"]),
                          version = ''.join(["%prog ",Version," (C) SAV 2009"]))
    parser.add_option("-v", "--verbose", dest="verbose",
                      default=False, action="store_true",
                      help="print progress and other info to stderr.")
    parser.add_option("-c", "--call", dest="call",
                      default=False, action="store_true",
                      help="make class lable calls.")
    parser.add_option("-r", "--reducer", dest="reducer", type="choice",
                      default='sum', choices = ('sum','max'),
                      metavar="FUN",help="FUN in ['sum', 'max']. "
                      "FUN is used to combine the strenghts assigned to "
                      "a class label by several rules "
                      "(default: %default). Useful when fuzzy rules are used.")
    parser.add_option("-p", "--probabilities", dest="prob",
                      default=False, action="store_true",
                      help="convert class strengths to sum to 1.")
    parser.add_option("-l", "--last", dest="plast",
                      default=False, action="store_true",
                      help="return strength for lexicographically last "
                      "class only.")
    (opt, args) = parser.parse_args()
    if len(args) < 1:
        print "Missing input file. Try -h for help.\\n"
        sys.exit(1)
    f = (args[0] == '-' and sys.stdin or urlopen(args[0]))
    (anames, m) = simple_read_table(f)
    preds = classify(m, rules, combinator=combinators[opt.reducer],
                     prob=opt.prob)
    print('[')
    if opt.call:
        print(',\\n'.join(repr(max([(y,x) for (x,y) in pred.items()])[1])
              for pred in preds))
    else:
        select = (lambda x : x[-1][1]) if opt.plast else (lambda x: x)
        print(',\\n'.join(repr(select(sorted(pred.items()))) for pred in preds))
    print(']')

''').substitute({'dfile':datafile,
                 'version':repr(Version),
                 'priors':repr(dict(priors)),
                 'rules':'rules=[\n'+',\n'.join(lambdas)+'\n]',
                 'descriptor_code':descriptor_code(A,X)}))
    
# that's all folks    

