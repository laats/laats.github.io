/* -*-C++-*-
********************************************************************************
*
* File:         parameter.cc
* RCS:          $Header: $
* Description:  
* Author:       Staal Vinterbo
* Created:      Fri Apr 15 16:40:24 2005
* Modified:     Wed May 11 11:17:16 2005 (Staal Vinterbo) staal@MGV
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

#include "parameter.h"
using namespace clpar;
#include <iostream>
#include <vector>
using std::vector;

namespace clpar
{

  template <> string Parameter::getvalue()
  {
    return val;
  }
  
  struct option * getoptoptions(vector<Parameter*> &parms)
  {
    struct option * ol = new struct option[parms.size()+1];
    int k = 0;
    for(vector<Parameter*>::iterator i = parms.begin();
        i != parms.end();
        i++){
      ol[k++] = (*i)->getoptlong();
    }
    ol[k] = (struct option){0,0,0,0};
    return ol;
  }
  
  string getoptoptstring(vector<Parameter*> &parms)
  {
    string s = "";
    for(vector<Parameter*>::iterator i = parms.begin();i != parms.end(); i++)
      s += (*i)->getoptshort();
    return s;
  }
  
  string usage(vector<Parameter*> &parms)
  {
    string s = "Options are:\n";
    for(vector<Parameter*>::iterator i = parms.begin();
        i != parms.end();
        i++){
      if((*i)->getsname() != 0)
        s += "-" + string(1,(*i)->getsname()) + " ";
      if((*i)->getlname() != "")
        s += "--" + (*i)->getlname() + " ";
      if((*i)->has_args() > 0)
        s += " <value>";
      s += "\n";
      s += (*i)->is_required() ? "   Is required. " :"   Is optional. " ;
      s += "Current value: " + (*i)->getvalue<string>() + "\n";
      if((*i)->get_explanation() != "")
        s += "   " + (*i)->get_explanation() + "\n";
    }
    return s;
  }
  
    
  int dogetoptcheck(int &argc, char ** &argv, vector<Parameter*> &parms)
  {
      struct option * long_options = getoptoptions(parms);
      string optstring = getoptoptstring(parms);
      int nerrors = 0;
      while (1) {
        int option_index = 0;
        int c = getopt_long(argc, argv, optstring.c_str(),
                            long_options, &option_index);
        if (c == -1)
          break;
        
        bool caught = false;
        for(vector<Parameter*>::iterator i = parms.begin();
            i != parms.end(); i++){
          try{
            if(c == (*i)->getid() || c == (*i)->getsname()){
              (*i)->set(optarg);
              caught = true;
            }
          }
          catch(string &s){
            std::cerr << "error: " << s << "\n";
          }
          catch(...){
            std::cerr << "Unknown error...\n";
          }
        }
        if(!caught){
          nerrors++;
          std::cerr << "option " << (char)c << " unknown.\n";
        }
      }
      return nerrors;
  }
} // namespace clpar
