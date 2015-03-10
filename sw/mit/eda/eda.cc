/* -*-C++-*-
********************************************************************************
*
* File:         eda.cc
* RCS:          $Header: $
* Description:  
* Author:       Staal Vinterbo
* Created:      Tue Apr 19 10:44:05 2005
* Modified:     Wed May 11 11:12:15 2005 (Staal Vinterbo) staal@MGV
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

#include "util.h"
using util::sample_nr;
using util::order;


#include "eda.h"

using std::min_element;
using std::max_element;

#include <limits>
using std::numeric_limits;

#include <string>
using std::string;

#include <assert.h>

#include <iostream>
using std::cerr;

bool EDA::stop = false;

static inline int mat_ind(int i, int j, int rdim)
{
  return i * rdim + j;
}

Distribution::Distribution(int n, vector< pair<int,int> > &zers, prob_t aval)
  : dim(n), alpha(aval), eps(0.0001),
    dv(100000.0), df(100.0), zeros(zers),
    prob_matrix(n*n, static_cast<prob_t>(1)/static_cast<prob_t>(n))
{
}

// funky, empirically established update function...    
prob_t Distribution::combine(prob_t newprob, prob_t oldprob)
{
    prob_t val = alpha*newprob + (1-alpha)*oldprob;
    prob_t v = 0.5 - val;
    return(val + sign(v)*edaf(delta(v)));
}

void Distribution::update(Population &p, vector<int> &filter)
{
  vector<int> counts(dim*dim, 0);

  prob_t divisor = static_cast<prob_t>(filter.size());
  
  // for each selected individual...
  for(vector<int>::iterator it = filter.begin();
      it != filter.end(); it++){
    vector<int> *ind = &p.pop[*it]; // get pointer to individual 
    
    vector<int>::iterator elm = ind->begin();
    int current = *elm;
    int first = current;
    
    counts[mat_ind(current,current,dim)]++;// the diagonal is the start counts
    
    elm++;
    while(elm != ind->end()){
      counts[mat_ind(current,*elm, dim)]++;// count[from][to]++
      current = *elm;
      elm++;
    }
    // don't forget the last one...
    counts[mat_ind(current,first, dim)]++;
  }
  

  // divide each row in counts with the count of 
  prob_t dsum = 0;
  for(int i = 0; i < dim; i++){
    prob_t sum = 0;
    for(int j = 0; j < dim; j++){
      int mi = mat_ind(i,j,dim);
      prob_matrix[mi] =
        combine(static_cast<prob_t>(counts[mi])/divisor,
                prob_matrix[mi]);
#define RENORM
#ifdef RENORM
      sum += prob_matrix[mi];   // sum each row
#endif
    }
    // renormalize
#ifdef RENORM
    int ii = mat_ind(i,i,dim);  // diagonal index
    dsum += prob_matrix[ii];    // sum diagonals
    sum -= prob_matrix[ii];     // don't count diagonal
    //    prob_t qcsum = 0;
    for(int j = 0; j < dim; j++)
      if(j != i){
        int mi = mat_ind(i,j,dim);
        prob_matrix[mi] /= sum;  // normalize row probs
        //        qcsum += prob_matrix[mi]; // quality control
      }
    //cerr << i << " row: " << qcsum << " " << static_cast<prob_t>(1) << "\n";
    //    assert(qcsum == static_cast<prob_t>(1));
#endif
  }

#ifdef RENORM
  // renormalize diagonal
  //  prob_t qcsum = 0;
  for(int i = 0; i < dim; i++){
    int ii = mat_ind(i,i,dim);
    prob_matrix[ii] /= dsum;
    //    qcsum += prob_matrix[ii];
  }
  //  cerr << "diag : " << qcsum << " " << static_cast<prob_t>(1) << "\n";  
  //  assert(qcsum == static_cast<prob_t>(1));
#endif  

  for(vector<pair<int,int> >::iterator z = zeros.begin();
      z != zeros.end(); z++)
    prob_matrix[mat_ind((*z).first, (*z).second, dim)] =
      numeric_limits<float>::epsilon()*2; // something small...
}


vector<int> Distribution::sample()
{
    vector<prob_t> start(dim);
    vector<int> res;
    set<int> available;

    res.reserve(dim);

    // sample starting point
    for(int i = 0; i < dim; i++){
      int mi = mat_ind(i, i, dim);
      start[i] = prob_matrix[mi]; // diagonal is start distribution
      prob_matrix[mi] = 0;              // do not sample i to i transition...
      available.insert(i);
    }

    int current = sample_nr<prob_t>(1,start).front();

    res.push_back(current);
    available.erase(current);
    
    vector<prob_t> distr;
    vector<int> what;

    what.reserve(dim);
    distr.reserve(dim);
    
    for(int i = 1; i < dim; i++){
      what.clear();
      distr.clear();
      // prepare dist from which to sample
      for(set<int>::iterator hi =available.begin();hi != available.end();hi++){
        what.push_back(*hi);
        distr.push_back(prob_matrix[mat_ind(current,*hi, dim)]);
      }

      // do the sampling
      int sres = sample_nr<prob_t>(1,distr).front();
      int nxt = what[sres];

      // store the result
      res.push_back(nxt);
      current = nxt;
      available.erase(current);
    }
    
    // restore diagonal probabilities
    for(int i = 0; i < dim; i++){
      prob_matrix[mat_ind(i, i, dim)] = start[i];
    }

    return(res);
}

Population::Population(int n, Distribution &d)
{
  for(int i = 0; i < n; i++)
    pop.push_back(d.sample());
}

bool f(pair<score_t,int> a, pair<score_t,int> b)
{
  // assumes positive...
  return(a.first < b.first);
}



EDA::EDA(Distribution &initialDist,
         Objective &objective,
         int psize,// = 20,
         int nest,// = 7,
         bool tellit,
         int nelite,// = 1,
         int niter)// = 30,
         : d(initialDist), p(psize,initialDist), o(objective),
           ne(nelite), ni(niter), ps(psize), es(nest), gen(0),
           maxgen(numeric_limits<int>::max()),
           noimp(0), verbose(tellit), dolog(0), loginterval(1)
{
    if(verbose){
      cerr << "EDA: psize " << psize << " nelite: " << nelite <<
        " niter: " << niter << " nest: " << nest << "\n";
      cerr << "Evaluating initial pop...";
    }
    
    for(int j = 0; j < ps; j++)
      p.scores.push_back(o.raw(p.pop[j]));

    // find best
    best_score = p.scores[0];
    best = p.pop[0];
    for(int j = 1; j < ps; j++)
      if(o.is_better(p.scores[j], best_score)){
        best_score = p.scores[j];
        best = p.pop[j];
      }
    bestmean =
      (score_t)accumulate(p.scores.begin(), p.scores.end(), 0.0)/(score_t)ps;
    if(verbose)
      cerr << "done.\n";
}

Population& EDA::run(ostream& logstream)
{
    if(verbose)
      cerr << "Starting evolution...\n";

    stop = false;  // in case of restart

#ifndef DONTUSEORDER

    //    cerr << "using new ordering stuff...\n";
    

    vector<int> filter;
    filter.reserve(es);

    for(;!stop; gen++){

      vector<int> po = order<score_t>(p.scores,
                                      o.direction() == Objective::maximize);
      filter.clear();
      filter.insert(filter.end(), po.begin(), po.begin() + es);

      d.update(p,filter);

      // replace the population.size - nelite worst
      int j = 0;
      int besti = -1; // index to new best individual
      vector<int>::reverse_iterator i = po.rbegin();
      while((j < ps-ne) && i != po.rend()){
        p.pop[*i] = d.sample();
        p.scores[*i] = o.raw(p.pop[*i]);
        
        if(o.is_better(p.scores[*i], best_score)){
          best_score = p.scores[*i];
          besti = *i; // use index to avoid multiple copying...
          noimp = 0;
          best_gen = gen;
        }

        i++;
        j++;
      }
#else      
    vector<pair<score_t,int> > score(ps);
    vector<int> filter(es);

    for(;!stop; gen++){

       for(int j = 0; j < ps; j++){
         score[j].first = p.scores[j];
         score[j].second = j;
       }

       if(o.direction() == Objective::minimize)
         sort(score.begin(), score.end(), f); // increasing
       else
         sort(score.rbegin(), score.rend(), f); // decreasing

 //      cerr << "scores: " << score[0] << " " << score[score.size()-1] << "\n";
      
       for(int j = 0; j < es; j++)
         filter[j] = score[j].second;

       d.update(p,filter);

       vector<pair<score_t,int> >::reverse_iterator i = score.rbegin();
       int j = 0;
       int besti = -1; // index to new best individual

       while((j < ps-ne) && i != score.rend()){

         p.pop[(*i).second] = d.sample();
         p.scores[(*i).second] = o.raw(p.pop[(*i).second]);
        
         if(o.is_better(p.scores[(*i).second], best_score)){
           best_score = p.scores[(*i).second];
           besti = (*i).second; // use index to avoid multiple copying...
           noimp = 0;
           best_gen = gen;
         }

         i++;
         j++;
       }
#endif
      if(besti > -1)
        best = p.pop[besti];

      score_t mean =
        (score_t)accumulate(p.scores.begin(), p.scores.end(), 0.0)/(score_t)ps;

      if(o.is_better(mean, bestmean)){
        noimp = 0;
        bestmean = mean;
      }

      if(noimp > ni)
        stop = 1;

      if(gen >= maxgen)
        stop = 1;

      noimp++;

      if(dolog && (gen % loginterval) == 0)
        logstream << (logstream == cerr ? "\033[80D\033[K" : "") << 
          gen << " " <<
          *min_element(p.scores.begin(), p.scores.end()) << " (" <<
          mean << ", " << bestmean << ") " <<
          *max_element(p.scores.begin(), p.scores.end()) << " " <<
          best_score << " " << noimp <<
          (logstream == cerr ? "" : "\n");
    }
    if(dolog && logstream == cerr)
      logstream << "\n";
      
    return p;
}
