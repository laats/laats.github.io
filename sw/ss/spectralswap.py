# -*-Python-*-
################################################################################
#
# File:         spectralswap.py
# RCS:          $Header: $
# Description:  spectral swapping anonymization algorithm
# Author:       Staal Vinterbo
# Created:      Wed May  6 14:57:26 2009
# Modified:     Tue Sep 18 12:09:39 2012 (Staal Vinterbo) staal@mats
# Language:     Python
# Package:      N/A
# Status:       Experimental
#
# spectralswap.py is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# spectralswap.py is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with spectralswap.py; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# (c) Copyright 2009, Staal Vinterbo, all rights reserved.
#
################################################################################
'''apply spectral swapping algorithm to data

%(prog)s
===============

:Author: Staal A. Vinterbo
:Copyright: 2009-2012 Staal A. Vinterbo
:Version: generated for %(version)s
:Availability: `GPL <http://www.gnu.org/copyleft/gpl.html>`_
:URI: `%(url)s <%(url)s>`_, `%(companion)s <%(companion)s.py>`_


**Contents**:
  - `Module Synopsis`_
  - `Theory`_
  

Module Synopsis
---------------

Let m be a data matrix (list of rows) where columns represent 
attributes and rows represent the attribute values for objects. Also
let which be a list of column indices that contains categorical data. Then::

 from dummify import maxi
 from spectralswap import spectralswapping
 d = spectralswapping(m, which, maxi)

produces a matrix d that essentially is m with permuted columns. The function::

 spectralswapping

depends on the module dummify.

To generate a html version of this short explanation::

 $ python %(prog)s -e | rst2html > explanation.html

rst2html is a part of the python docutils package 
http://docutils.sourceforge.net/docs/


Theory
------

In order to preserve inter-column relationships, the actual permutation is
done along eigenvectors. Details can be found in::

  Thomas A. Lasko, Staal A. Vinterbo, "Spectral Anonymization of Data,"
  IEEE Transactions on Knowledge and Data Engineering, pp. 437-446, March, 2010 

http://doi.ieeecomputersociety.org/10.1109/TKDE.2009.88

Note
----
This implementation was supported by NIH NLM grant 7R01LM007273-07 and NIH Roadmap for Medical Research grant U54 HL108460.

'''

__all__ = ['spectralswapping']

Version = '0.1'

from numpy.linalg import svd
from numpy import dot, array, diag
from random import shuffle

from dummify import dummifyf, applydf, applyrf, maxi, wheel

def shuffled(l):
    '''returns a shuffled version of input l'''
    shuffle(l) # does in-place shuffling
    return l

def spectralswapping(m, which, restore = maxi):
    '''applies spectral swapping to list of multidiemsional data points

    m       -- an iterable of multivariate data points as vectors/lists
    which   -- which dimensions that need to be dummified
    restore -- function that takes a list of real numbers and selects
               an index into the list. This is used to reconstitute a
               categorical variable from its dummy variables.
               the dummify module implements two: 
               * maxi  -- choose an index of a maximal entry
               * wheel -- view values as a probability distrubution over
                          indicies and sample an index accordingly using 
                          a roulette wheel. 

    the specral swapping method is described in 
    @article{ 10.1109/TKDE.2009.88,
      author = {Thomas A. Lasko and Staal A. Vinterbo},
      title = {Spectral Anonymization of Data},
      journal ={IEEE Transactions on Knowledge and Data Engineering},
      volume = {99},
      number = {1},
      issn = {1041-4347},
      year = {5555},
      doi = {http://doi.ieeecomputersociety.org/10.1109/TKDE.2009.88},
      publisher = {IEEE Computer Society},
      address = {Los Alamitos, CA, USA},
    }
    '''
    funs = dummifyf(m, which, dedum=restore)
    u,s,vh = svd(array(applydf(m, funs)),0)
    return applyrf(dot(array(map(list,zip(*map(shuffled, map(list, zip(*u)))))),
                       dot(diag(s),vh)), funs)


if __name__ == '__main__':

    import os
    import sys
    progname = os.path.basename(sys.argv[0])
    url = progname
    companion = 'dummify' # name of companion dummyfy module

    print(__doc__ % {'prog': progname,
                     'version' : Version,
                     'url': url,
                     'companion':companion})
