#!/usr/bin/env python
################################################################################
#
# File:         minkunion.py
# RCS:          $Header: $
# Description:  minimum k union algorithm implementation, with application
#               to k-ambiguation by cell suppression. See
#               Vinterbo, S. A.
#               A Stab at Approximating Minimum Subadditive Join
#               Algorithms and Data Structures: 10th International Workshop,
#               WADS 2007, Halifax, Canada, August 15-17, 2007
#               Proceedings, Springer Verlag, 2007 
# Author:       Staal Vinterbo
# Created:      Thu Oct  9 17:23:41 2008
# Modified:     Wed Dec 15 17:51:48 2010 (Staal Vinterbo) staal@dink
# Language:     Python
# Package:      N/A
# Status:       Experimental
#
# minkunion.py is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# minkunion.py is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with minkunion.py; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# (c) Copyright 2008-2009, Staal Vinterbo, all rights reserved.
#
################################################################################
'''minimum k union algorithm implementation

%(prog)s
===============

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

Let m be a list of lists representation of a collection of sets, and let
k be a positive integer. Then::

 from minkunion import minkunion
 d = minkunion(m, k)

produces a list of elements that is an approximation of the minimum union of
k elements from m.

If we let X be list of lists, each of which represents a row in a data matrix,
and we let k be a positive integer, 
then::

 from minkunion import kambiguate
 Y = kambiguate(X, k)

produces a k-ambiguated version of X in Y by cell-suppression. This means
that each list in Y matches at least k lists in X if we interpret '?' as
representing any possible value.

Standalone Invocation
---------------------

Run as a standalone program this module will perform k-ambiguation on
the input data set.

Note that the rectangular data set required as input is assumed to contain
attribute names on the first line. Each subsequent line contains the
attribute values for an object. The last attribute, i.e., last column, is 
taken to contain class labels. 

Try (assuming a shell command line)::
 
 $ python %(prog)s -h
 $ python %(prog)s -e | less
 $ python %(prog)s dataset.txt | less
 $ python %(prog)s -k 3 dataset.txt | less 

To generate a html version of this short explanation::

 $ python %(prog)s -e | rst2html > explanation.html

rst2html is a part of the python docutils package 
http://docutils.sourceforge.net/docs/


Theory
------

The minimum k union of a collection of sets is the set of smallest
cardinality obtainable by taking the union of k members of the collection.

This module contains a simple greedy algorithm that iteratively selects the
set that contributes the least to the union. This algorithm is used
to implement k-ambiguation by cell suppression.

Details in::

 Vinterbo, S. A.
 A Stab at Approximating Minimum Subadditive Join
 Algorithms and Data Structures: 10th International Workshop,
 WADS 2007, Halifax, Canada, August 15-17, 2007
 Proceedings, Springer Verlag, 2007

http://www.springerlink.com/content/j2w1373m448648lg/ 
'''


__all__ = ['minkunion', 'mtocollection', 'kambiguate']

from itertools import izip, imap

argmax = lambda funct, items: max(izip(imap(funct, items), items))
argmin = lambda funct, items: min(izip(imap(funct, items), items))

def minkunion(collection, k):
    '''try to find the minimum k-union of the input collection'''
    if len(collection) < k:
        return []
    sol = []
    cover = set()
    avail = set(range(len(collection)))
    for idx in xrange(k):
        best = argmin(lambda i : len(cover | collection[i]), avail)[1]
        sol.append(best)
        cover |= collection[best]
        avail.remove(best)
    return cover

def mtocollection(m, target):
    '''construct discernibility matrix (collection) relative to target'''
    v = m[target]
    collection = []
    for i,row in enumerate(m):
        if i == target:
            continue
        collection.append(set([j for j in xrange(len(v)) if v[j] != row[j]]))
    return collection

def kambiguate(m,k):
    '''k-ambiguate input matrix by cell suppression'''
    newm = []
    for i,r in enumerate(m):
        newr = [x for x in r]
        for j in minkunion(mtocollection(m,i), k):
            newr[j] = '?'
        newm.append(newr)
    return newm


if __name__ == "__main__":
    try:
        import psyco
        psyco.full()
        pass
    except ImportError:
        pass

    from urllib import urlopen
    from optparse import OptionParser
    from logging import debug, warning, error, info
    import logging
    import re
    import sys
    import os
    
    progname = os.path.basename(sys.argv[0])
    url = progname
    Version = "0.01"
    parser = OptionParser(usage = "".join(
        ["%prog [options] URL\n",
         "\n Version: ", Version, " SAV (C) 2009\n",
         " K-ambiguate by cell suppression.\n\n",
         " URL points to a rectangular data table where the first line\n",
         " contains attribute names. Table entries are separated by /\s,:/.\n",
         " URL can also be a simple filename or '-' for stdin.\n"]),
                          version = ''.join(["%prog ",Version," (C) SAV 2009"]))
    parser.add_option("-v", "--verbose", dest="verbose", action="count",
                      default = 0,
                      help="print more stuff than strictly needed. Given"
                      " twice results in debug info.")
    parser.add_option("-k", dest="k", type='int',
                      default=5, metavar='INTEGER',
                      help="Ambiguation: how many original data "
                      "points must match the suppressed row."
                      "(default: '%default')")
    parser.add_option("-e", "--explain", dest="explain",
                      default=False, action="store_true",
                      help="print description of what this program does.")

    (opt, args) = parser.parse_args()

    verbose = opt.verbose > 0
    logging.basicConfig(level=max(logging.DEBUG,
                                  logging.WARNING - 10*opt.verbose),
                        format='%(levelname)-5s %(message)s')

    if opt.explain:
        print(__doc__ % {'prog': progname,
                         'version' : Version,
                         'url': url})
        sys.exit(0)


    if len(args) < 1:
        error("Missing input file. Try -h for help.")
        sys.exit(1)

    info('fetching data...')
    f = (args[0] == '-' and sys.stdin or urlopen(args[0]))
    rex = re.compile('[\s,:]') # separators are whitespace, ',' and ':'
    m = [rex.split(line.strip()) for line in f]
    # remove empty and comment lines (start with '#')
    m = filter(lambda r: (not all(map(lambda x: x == '', r))
               and (r[0] == '' or r[0][0] != '#')), m)
    info('fetched ' + str(len(m) - 1) + ' rows.')    

    info(str(opt.k) + '-ambiguating...')
    tra = kambiguate(m[1:],opt.k) # note that we are assuming that the first
                                  # line contains column (attribute) names

    info('printing output...')
    print(' '.join(m[0]))         # print column names
    for row in tra:
        print(' '.join(row))
    info('done.\n')    

        
