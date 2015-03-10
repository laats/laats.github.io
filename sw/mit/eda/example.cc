/* -*-C++-*-
********************************************************************************
*
* File:         example.cc
* RCS:          $Header: $
* Description:  Example usage of the eda library. Reads a space separated
*               list of integers from stdin and ouputs an ordering of these
*               such that if applied sorts the list.
* Author:       Staal Vinterbo
* Created:      Tue May 10 12:36:09 2005
* Modified:     Wed May 11 15:17:38 2005 (Staal Vinterbo) staal@MGV
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

#include <eda.h>
#include <iostream>
using namespace std;


#include <util.h>
using util::operator<<;
using util::operator>>;

#include <parameter.h>
using clpar::Parameter;
using clpar::ToggleParameter;
using clpar::always_ok;
using clpar::dogetoptcheck;
using clpar::usage;



class MyObjective : public Objective 
{
  vector<int> target;
public:
  MyObjective(vector<int> &t) : Objective(maximize), target(t) {}
  virtual ~MyObjective(){}
	score_t raw(vector<int> &ind) { 
		int sum = 0;
    for(vector<int>::iterator i = ind.begin() + 1; i != ind.end(); i++)
      sum += static_cast<int>(target[*(i - 1)] <= target[*i]);
		return static_cast<score_t>(sum);
	}
};

#define CATCH(action)               \
  catch(string &s){                 \
    cerr << "error: " << s << endl; \
    action;                         \
  }                                 \
  catch(...){                       \
    cerr << "Unknown error..."      \
         << endl;                   \
    action;                         \
  }                                 


int main(int argc, char * argv[])
{

  enum optenum { wait, descending, help, notused};

  vector<Parameter*> parms;
  parms.push_back(new Parameter(wait, 'w', "wait", "30",
        "How many iterations to wait for improvement before halting."));
  parms.push_back(new ToggleParameter(descending,'d', "descending",
                                        "order descending"));
  parms.push_back(new ToggleParameter(help,'h', "help",
                                        "Print short usage description."));
  
  int nerrors = dogetoptcheck(argc, argv, parms);

  if(parms[help]->is_set()){
    cout << "Input is a space separated list of integers read from stdin."
         << endl;
    cout << usage(parms);
    return 1;
  }

  for(vector<Parameter*>::iterator i = parms.begin(); i != parms.end(); i++){
    try{
      (*i)->check(always_ok<int>()); // check if required have been set...
      cerr << "Parameter " << (*i)->getpname()
           << " was" << ((*i)->is_set() ? " set " : " not set ")
           << "and has value " << (*i)->getvalue<string>() << endl;
    }
    CATCH(nerrors++);
  }


  

  // now check w > 0
  try{
    parms[wait]->check(bind2nd(greater<int>(), 0));
  }
  CATCH(return -1);

  if(nerrors > 0){
    cout << "Command line errors." << endl << usage(parms);
    return -1;
  }

  if (optind < argc) {
    cerr << "Warning: non-option ARGV-elements: ";
    while (optind < argc)
      cerr << argv[optind++] << " ";
    cerr << endl;
  }


  vector<int> t;
  std::cin >> t;
  MyObjective o(t);
  vector< pair<int,int> > zers;
  Distribution d(t.size(),zers);
  EDA eda(d, o);
  eda.setwait(parms[wait]->getvalue<int>());
  eda.run(cerr);

  vector<int> sol = eda.sol();
  
  if(parms[descending]->getvalue<bool>())
    reverse(sol.begin(), sol.end());

  cout << sol;
}

