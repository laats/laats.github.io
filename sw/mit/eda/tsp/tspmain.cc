/* -*-C++-*-
********************************************************************************
*
* File:         tspmain.cc
* RCS:          $Header: $
* Description:  
* Author:       Staal Vinterbo
* Created:      Tue Apr 19 10:43:38 2005
* Modified:     Wed May 04 17:21:08 2005 (Staal Vinterbo) staal@MGV
* Language:     C++
* Package:      N/A
* Status:       Experimental (Do Not Distribute)
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

#include <eda.h>
#include <util.h>
using util::operator<<;

#include "tsp.h"
#include <parameter.h>
#include "version.h"
#include "build.h"

#include <signal.h>

#include <string>
using std::string;

// clpar namespace from parameter.h
using clpar::Parameter;
using clpar::ToggleParameter;
using clpar::usage;
using clpar::is_in_range;
using clpar::always_ok;

#include <limits>
using std::numeric_limits;




void SIGINT_handler(int signal)
{
  EDA::stopnow();
}

  


void tsp_usage(string pname, vector<Parameter*> &parms){
  cout << "usage: " << pname << " [options]\n";
  cout << "Input is read from stdin,"
       << " and output is given on stdout.\n";
  cout << usage(parms);
}


int main(int argc, char * argv[])
{
  string pname = argv[0];
  bool blabber = false;
  
  enum optenum { verbose, help, psize, dv, df, niter, nest,
               logfile, dolog, logint, alpha, nelite, seed, maxgen,
                 version, report, notused};

  vector<Parameter*> parms;
  try{
    parms.push_back(new ToggleParameter(verbose,'v', "verbose"));
    parms.push_back(new ToggleParameter(help,'h', "help"));
    parms.push_back(new Parameter(psize, 'p', "psize", "100",
                                  "Population sise to use"));
    parms.push_back(new Parameter(dv, 0, "dv", "10000",
                                  "Sets dv used in probability scaling."));
    parms.push_back(new Parameter(df, 0, "df", "100",
                                  "Sets df used in probability scaling."));
    parms.push_back(new Parameter(niter, 'n', "niter", "100",
                                  "Sets number of generations with\n"
      "   no improvement to wait before quitting."));
    parms.push_back(new Parameter(nest, 'e', "nest", "20",
                                  "Sets number of best individuals"
                                  " to use each\n" 
                "   generation to update the distribution estimate." ));
    parms.push_back(new Parameter(logfile, 'f', "logfile", "cerr",
                                  "Where to print log."));
    parms.push_back(new ToggleParameter(dolog, 'l', "log",
                                  "Turn on logging of generational stats."));
    parms.push_back(new Parameter(logint, 0, "loginterval", "20",
           "Sets the number of generations to wait between each log output."));
    parms.push_back(new Parameter(alpha, 'a', "alpha", "0.15",
                          "Sets alpha, the distribution update speed."));
    parms.push_back(new Parameter(nelite, 'i', "nelite", "3",
              "How many of the best to transfer to the next population."));
    parms.push_back(new Parameter(nelite, 's', "seed", "none",
              "Set the random seed."));
    parms.push_back(new Parameter(nelite, 'm', "maxgen",
                                  "none",
              "Maximal number of generations to run."));
    parms.push_back(new ToggleParameter(version, 0, "version",
                                        "Print version and exit"));
    parms.push_back(new ToggleParameter(report, 'r', "report",
                                        "Report total generations, "
          "generation at which the best was found, and score."));
  }
  catch(...){
    cerr << "Could not initialize parameter vector";
    return 1;
  }
  
  int nerrors = dogetoptcheck(argc, argv, parms);

  // prioritize help, verbose and seed
  try{
    if(parms[help]->is_set()){
      tsp_usage(pname, parms);
      return 1;
    }
    if(parms[version]->is_set()){
      cout << pname << " version "
           << VERSION << "-" << BUILD << " (C) SAV 2005\n";
      return 1;
    }
    if(parms[verbose]->is_set()){
      blabber = true;
    }
    if(parms[seed]->is_set()){
      int theseed = parms[seed]->getvalue<int>();
      cerr << "Initializing random number generator with " << theseed << "\n";
      srand(theseed);
    }
    else
      srand(time(0));    
  }
  catch(string &s){
    cerr << "error: " << s << "\n";
    return -1;
  }
  catch(...){
    cerr << "Unknown error...\n";
    return -1;
  }
  

  for(vector<Parameter*>::iterator i = parms.begin(); i != parms.end(); i++){
    try{
      (*i)->check(always_ok<string>()); // check if required have been set...
      if(blabber && (*i)->is_set())
        cerr << "Parameter " << (*i)->getpname()
             << " was" << " set to "
             << (*i)->getvalue<string>() << "\n";
    }
    catch(string &s){
      cerr << "error: " << s << "\n";
      nerrors++;
    }
    catch(...){
      cerr << "Unknown error...\n";
      nerrors++;
    }
  }

  if(nerrors > 0){
    cerr << usage(parms);
    return -1;
  }

  if (optind < argc) {
    cerr << "Warning: unused non-option ARGV-elements:\n";
    while (optind < argc)
      cerr << argv[optind++] << " ";
    cerr << "\n";
  }



  try{
    
    tsp tsp;
    tsp.read();



    if(blabber)
      cerr << "init o... ";
    TspObjective o(tsp);
    if(blabber)
      cerr << "done.\n";
    
    if(blabber) cerr << "init d... ";
    vector<pair<int,int> > zeros;
    Distribution d(tsp.dim(), tsp.holes(),
                   parms[alpha]->getvalue<prob_t>());
    d.setdf(parms[df]->getvalue<prob_t>());
    d.setdv(parms[dv]->getvalue<prob_t>());
    
    if(blabber) cerr << "done.\n";
    
    if(blabber) cerr << "done.\ninit eda...\n";
    EDA eda(d, // distribution
            o, // objective
            parms[psize]->getvalue<int>(), // psize
            parms[nest]->getvalue<int>(),  // nest
            parms[verbose]->getvalue<bool>(),   // tellit
            parms[nelite]->getvalue<int>(),    // nelite
            parms[niter]->getvalue<int>());    // niter
    if(parms[dolog]->getvalue<bool>())
      eda.togglelog();

    if(parms[maxgen]->is_set())
      eda.setmaxgen(parms[maxgen]->getvalue<int>());
    
    eda.setloginterval(parms[logint]->getvalue<int>());
    if(blabber) cerr << "done.\neda.run()...\n";

    string fname = parms[logfile]->getvalue<string>();
    ostream * stream = &cerr;
    if(fname == "cout")
      stream = &cout;
    else if(fname != "cerr"){
      cerr <<
        "Sorry, logging to files not implemented," <<
        " use cout or cerr (default).\n";
    }
    // if it don't work, ignore it...
    void (*oldhandler)(int) = signal(SIGINT, SIGINT_handler);
    if(oldhandler == SIG_ERR)
      cerr << "Warning: could not install SIGINT signal handler.\n";

    eda.run(*stream);
    if(blabber){
      cerr << "done.\n";
      cerr << "generations: " << eda.generation()
           << ", best discovered at " << eda.bgen()
           << " best: " << tsp.length(eda.sol()) << "\n";
    }

    if(parms[report]->is_set()){
      cout << eda.generation() << std::endl
           << eda.bgen() << std::endl
           << tsp.length(eda.sol()) << std::endl;
    }
    cout << eda.sol();
  }
  catch(string &s){
    cerr << "error: " << s << "\n";
    return -1;
  }
  catch(...){
    cerr << "Unknown error...\n";
    return -1;
  }
}

