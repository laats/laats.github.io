/* -*-C++-*-
********************************************************************************
*
* File:         tsp.cc
* RCS:          $Header: $
* Description:  
* Author:       Staal Vinterbo
* Created:      Tue Apr 19 10:45:15 2005
* Modified:     Wed May 11 15:37:08 2005 (Staal Vinterbo) staal@MGV
* Language:     C++
* Package:      TSP
* Status:       Experimental
*
* (c) Copyright 2005, Staal Vinterbo, all rights reserved.
*
* This file is part of TSP.
*
* TSP is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*
* TSP is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with TSP; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*
********************************************************************************
*/

#include "tsp.h"
#include <string>
#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <iterator>
#include <assert.h>
#include <functional>

using std::string;
using std::istringstream;
using std::istream;
using std::ostream;
using std::vector;
using std::min;
using std::max;
using std::istream_iterator;
using std::cerr;
using std::max_element;
using std::equal_to;


template <typename T>
ostream& operator<<(ostream &stream, vector<T> &seq)
 {
   stream << "[";
   for(typename vector<T>::iterator i = seq.begin();
       i != seq.end(); )
   {
     stream << *i;
     i++;
     if(i != seq.end())
       stream << " ";
   }
   stream << "]\n";
   return stream;
 }

istream & tsp::read(istream& stream)
{
  matrix.clear();
  string buffer;
  size_t maxlen = 0;
  size_t minlen = barrier;

  while(getline(stream, buffer)){
    istringstream iss(buffer);
    istream_iterator<dist_t> iis(iss);
    vector<dist_t> v(iis,istream_iterator<dist_t>());

    size_t vsize = v.size();

    // figure out the largest value not equal to barrier...
    vector<dist_t> cop;

    remove_copy_if(v.begin(), v.end(), 
               back_inserter(cop),
               bind2nd(equal_to<dist_t>(), (dist_t)barrier));

    maxstep = max(maxstep, *max_element(cop.begin(), cop.end()));

    maxlen = max(maxlen, vsize);
    minlen = min(minlen, vsize);

    matrix.push_back(v);
  }

  if((minlen != maxlen) || (maxlen != matrix.size())){
    cerr << "Matrix not square, bye.\n";
    exit(-1);
  }
  size = minlen;

  // compute the holes...
  for(size_t i = 0; i < size; i++)
    for(size_t j = 0; j < size; j++)
      if(matrix[i][j] == (dist_t)barrier)
        barriers.push_back(pair<int,int>(i,j));
  
  assert(maxstep > static_cast<dist_t>(0.0));

  //  cerr << "Read Matrix: \n" << matrix;

  return stream;
}

dist_t tsp::length(vector<int>& path)
{

  vector<int>::iterator i = path.begin();
  if(i == path.end()){
    throw string("tsp::length, empty path supplied");
  }
  
  dist_t sum = 0.0;
  int from = *i;

  while(++i != path.end()){
    int to = *i;
    assert((size_t)from < size && (size_t)to < size);
    sum += matrix[from][to];
    from = to;
  }

  sum += matrix[from][path[0]];
  return sum;
}

