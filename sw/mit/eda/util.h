/* -*-C++-*-
********************************************************************************
*
* File:         util.h
* RCS:          $Header: $
* Description:  
* Author:       Staal Vinterbo
* Created:      Fri Apr 22 14:38:36 2005
* Modified:     Wed May 11 11:12:05 2005 (Staal Vinterbo) staal@MGV
* Language:     C++
* Package:      EDA
* Status:       Experimental 
*
* (c) Copyright 2005, Staal Vinterbo, all rights reserved.
*
* This file is part of EDA.
*
* EDA is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*
* EDA is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with EDA; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*
********************************************************************************
*/

#ifndef _INCL_UTIL_H
#define _INCL_UTIL_H

#include <vector>
#include <algorithm>
#include <limits>
#include <iostream>
#include <string>
#include <sstream>
#include <iterator>
#include <set>

using std::vector;
using std::pair;
using std::sort;
using std::numeric_limits;
using std::istream;
using std::ostream;
using std::string;
using std::istringstream;
using std::istream_iterator;
using std::ostream_iterator;
using std::min;
using std::max;
using std::copy;
using std::set;

namespace util{
  

template <typename T>
bool _comp (pair<T, int> a, pair<T, int> b)
{
     return a.first < b.first;
}


// dependant on existence of operator<(T, T)
template <typename T>
vector<int> order(vector<T> &seq, bool descending = false)
{
  int size = seq.size();
  vector<pair<T,int> > tmp(size);
  vector<int> res(size);

  for(int i = 0; i < size; i++){
    tmp[i].first = seq[i];
    tmp[i].second = i;
  }

  if(!descending)
    sort(tmp.begin(), tmp.end(), _comp<float>); // increasing
  else
    sort(tmp.rbegin(), tmp.rend(), _comp<float>); // decreasing

  for(int i = 0; i < size; i++)
    res[i] = tmp[i].second;

  return res;
}

template <typename T>
T unif_rand()
{
  return static_cast<T>(rand())/static_cast<T>(RAND_MAX);
}

  

template <typename T> 
vector <int> sample_nr(int nans, vector<T> &prob)
{
  int n = prob.size();
  vector<int> o = order<T>(prob, true);
  T totalmass = accumulate(prob.begin(), prob.end(), static_cast<T>(0));
  T mass;
  T rT;
  
  vector<int> res;
  res.reserve(nans);
  int i;
  int n1;
  
  for(i = 0, n1 = n - 1; i < nans; i++, n1--){
    rT = unif_rand<T>() * totalmass;
    mass = 0;
    int j;
    for(j = 0; j < n1; j++){
      mass += prob[o[j]];
      if(rT <= mass)
        break;
    }
    
    res.push_back(o[j]);
    totalmass -= prob[o[j]];
    o.erase(o.begin() + j);
  }
  return(res);
}

template <typename T>
ostream& operator<<(ostream &stream, vector<T> &seq)
{
  for(typename vector<T>::iterator i = seq.begin();
        i != seq.end(); )
  {
    stream << *i;
    i++;
    if(i != seq.end())
      stream << " ";
  }
  stream << std::endl;
  return stream;
}
template <typename T>
ostream& operator<<(ostream &stream, set<T> &seq)
{
  copy(seq.begin(), seq.end(), ostream_iterator<T>(stream, " "));
  return stream;
}

template <typename T>
istream & operator>>(istream & stream, vector<T> & vec)
{
  string buffer;
  if(getline(stream, buffer)){
    vec.clear();
    istringstream iss(buffer);
    istream_iterator<T> iis(iss);
    vec = vector<T>(iis, istream_iterator<T>());
  } else {
    throw string("operator>>: could not read line");
  }
   return stream;
}
  

template <typename T>
pair<int,int> read_matrix(istream& stream, vector<vector<T> > &matrix)
{
  matrix.clear();
  string buffer;
  size_t maxlen = 0;
  size_t minlen = numeric_limits<size_t>::max();

  while(getline(stream, buffer)){
    istringstream iss(buffer);
    istream_iterator<T> iis(iss);
    vector<T> v(iis,istream_iterator<T>());

    size_t vsize = v.size();

    maxlen = max(maxlen, vsize);
    minlen = min(minlen, vsize);

    if((minlen != maxlen)){ // || (maxlen != matrix.size())){
      throw string("read_matrix: Matrix rows of uneven length");
    }

    matrix.push_back(v);
  }

  //  cerr << "Read Matrix: \n" << matrix;

  return pair<int,int>(matrix.size(), maxlen);
}

template <typename T>
istream& operator>>(istream &stream, vector<vector<T> > &matrix)
{
  if(matrix.size() == 0){  // don't know size...
    read_matrix(stream, matrix);
    return stream;
  }
  string buffer;
  size_t maxlen = 0;
  size_t minlen = numeric_limits<size_t>::max();

  int nrows = matrix.size();
  
  matrix.clear();
  int i = 0;
  while(i++ < nrows && getline(stream, buffer)){
    istringstream iss(buffer);
    istream_iterator<T> iis(iss);
    vector<T> v(iis,istream_iterator<T>());
    
    size_t vsize = v.size();
    
    maxlen = max(maxlen, vsize);
    minlen = min(minlen, vsize);
    
    if((minlen != maxlen)){// || (maxlen != matrix.size())){
      throw string("operator>>: Matrix rows of uneven length");
    }
    
    matrix.push_back(v);
  }
  return stream;
}
 
}

#endif // _INCL_UTIL_H
