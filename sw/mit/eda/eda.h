/* -*-C++-*-
********************************************************************************
*
* File:         eda.h
* RCS:          $Header: $
* Description:  
* Author:       Staal Vinterbo
* Created:      Tue Apr 19 10:43:47 2005
* Modified:     Wed May 11 14:46:39 2005 (Staal Vinterbo) staal@MGV
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

#ifndef _INCL_EDA_H
#define _INCL_EDA_H

#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <numeric>

using std::cerr;
using std::vector;
using std::pair;
using std::set;
using std::ostream;
using std::accumulate;
using std::cout;

#include "farith.h"
#include "objective.h"


typedef Double prob_t;


template <typename A, typename B>
ostream& operator<<(ostream &stream, pair<A,B> &a)
{
  stream << "(" << a.first << ", " << a.second << ")";
  return stream;
}


class Distribution;

class Population
{
public:
  vector< vector<int> > pop;
  vector<score_t> scores;
  
  Population(int n, Distribution &d);
  Population(){}; // do nothing
  
};


class Distribution
{
private:
  int dim;
  prob_t alpha;
  prob_t eps;
  prob_t dv;
  prob_t df;
  vector< pair<int,int> > zeros;
  vector<prob_t> prob_matrix;
  
  int sign(prob_t v){return farith_sign(v);}
  prob_t delta(prob_t v){return 0.5 - (prob_t)sign(v)*v;}
  prob_t edaf(prob_t v){
    return (1/(v*dv+1))/df;
  };

public:

  Distribution(int n, vector< pair<int,int> > &zers, prob_t aval = 0.2);
  vector<int> sample();
  void update(Population &, vector<int>  &);//= vector<int>());
  float setalpha(prob_t aval){return alpha = aval;}
  float seteps(prob_t eval){return eps = eval;}
  float setdf(prob_t eval){return df = eval;}
  float setdv(prob_t eval){return dv = eval;}
  int getdim(){return dim;}
  prob_t combine(prob_t, prob_t);
};


class EDA
{
  Distribution d;
  Population p;
  Objective &o;
  vector<int> best;
  score_t best_score;
  int best_gen;
  int ne;
  int ni;
  int ps;
  int es;
  int gen;
  int maxgen;
  static bool stop; // can be set from signal handler. Terminates run()
  int noimp;
  score_t bestmean;
  bool verbose;
  bool dolog;
  int loginterval;

public:
  EDA(Distribution &initialDist,
      Objective &objective,
      int psize = 20,
      int nest = 7,
      bool tellit = 0,
      int nelite = 1,
      int niter = 30);
  Population& run(ostream &logstream = cerr);
  Distribution& dist(){return d;}
  Population& pop(){return p;}
  vector<int> &sol(){return best;}
  score_t score(){return best_score;}
  int bgen(){return best_gen;}
  int generation(){return gen;}
  bool togglelog(){return dolog = !dolog;}
  int setloginterval(int interval = 1){return loginterval = interval;}
  int setmaxgen(int mgen){return maxgen = mgen;}
  int setnest(int n){return es = n;}
  int setwait(int n){return ni = n;}
  int setelite(int n){return ne = n;}
  static void stopnow() {EDA::stop = true;}
};

#endif /*_INCL_EDA_H */
