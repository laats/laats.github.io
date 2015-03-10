/* -*-C++-*-
********************************************************************************
*
* File:         tsp.h
* RCS:          $Header: $
* Description:  
* Author:       Staal Vinterbo
* Created:      Tue Apr 19 10:44:47 2005
* Modified:     Wed May 11 15:35:27 2005 (Staal Vinterbo) staal@MGV
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

#ifndef _INCL_TSP_H
#define _INCL_TSP_H

#include <vector>
#include <iostream>
#include <objective.h>

using std::vector;
using std::pair;
using std::istream;
using std::ostream;
using std::cin;

typedef score_t dist_t;

enum { barrier = 1000000 }; // there is no connection here...

class tsp 
{
  vector< vector<dist_t> > matrix;
  size_t size;
  dist_t maxstep;
  vector<pair<int,int> > barriers;
  
 public:
  tsp() : size(0),maxstep(0) {}
  istream & read(istream& stream = cin);
  dist_t length(vector<int> &path);
  vector<pair<int,int> > & holes(){return barriers;}
  size_t dim() {return size;}
  dist_t path_bound(){ return (size + 1)*maxstep;}
};

class TspObjective : public Objective
{
  tsp &tspp;
  
 public:
  TspObjective(tsp &d) : Objective(minimize), tspp(d) {}
  dist_t raw(vector<int> &ind) { return (tspp.length(ind)); }
};





#endif /* _INCL_TSP_H */
