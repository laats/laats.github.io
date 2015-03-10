/* -*-C++-*-
********************************************************************************
*
* File:         objective.h
* RCS:          $Header: $
* Description:  
* Author:       Staal Vinterbo
* Created:      Wed Apr 20 19:42:08 2005
* Modified:     Wed May 11 13:29:33 2005 (Staal Vinterbo) staal@MGV
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

#ifndef _INCL_OBJECTIVE_H
#define _INCL_OBJECTIVE_H

#include <vector>
#include <functional>
using std::vector;
using std::less;

#include "farith.h"

typedef Double score_t;

class Objective
{
 public:
  typedef enum minmaxenum { minimize = -1, maximize = 1 } minmaxtype;
 private:
  minmaxtype minmax; // -1 == minimization, 1 maximization
public:
  Objective(minmaxtype minmaxval = maximize) : minmax(minmaxval) {}
  virtual score_t raw(vector<int>&) = 0;
  minmaxtype direction() {return minmax;};
  void set_direction(minmaxtype dir) {minmax = dir;};
  inline bool is_better(score_t &v1, score_t &v2){
    return minmax == minimize ?
      (v1 < v2) : (v1 > v2);  // using farith
  }
};


#endif /* _INCL_OBJECTIVE_H */
