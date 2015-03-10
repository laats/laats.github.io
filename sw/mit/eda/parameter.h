/* -*-C++-*-
********************************************************************************
*
* File:         parameter.h
* RCS:          $Header: $
* Description:  
* Author:       Staal Vinterbo
* Created:      Fri Apr 15 16:40:34 2005
* Modified:     Wed May 11 11:16:42 2005 (Staal Vinterbo) staal@MGV
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

#ifndef _INCL_CLPARAMETER_H
#define _INCL_CLPARAMETER_H

#include <string>
#include <sstream>
#include <functional>
#include <vector>
#include "compose2.h"


using std::string;
using std::ostringstream;
using std::istringstream;
using std::unary_function;
using std::logical_and;
using std::bind2nd;
using std::less;
using std::greater;
using std::compose2;
using std::vector;


#include <typeinfo>
#include <getopt.h>

#include <iostream>

namespace clpar{

class Parameter;
  

// convenience functions:
struct option * getoptoptions(vector<Parameter*> &parms);
string usage(vector<Parameter*> &parms);
string getoptoptstring(vector<Parameter*> &parms);
int dogetoptcheck(int &argc, char ** &arcv, vector<Parameter*> &parms);

// checker must be a model of unary_function<T, bool>
template <typename T>
struct unary_bool_function : public unary_function<T, bool>
{
  virtual bool operator()(const T) const {
      return true;
  }
};

// convenience checkers
template <typename T> struct always_ok : public unary_bool_function<T>
{
  bool operator()(T) const {
    return true;
  }
};

template <typename T>
struct is_in_range : public unary_bool_function<T>
{
  T _upper;
  T _lower;
  is_in_range(T lower, T upper) : _lower(lower), _upper(upper)
  {
  }
  
  bool operator()(T val)
  {
    return compose2(logical_and<bool>(),
                    bind2nd(less<float>(), _upper),
                    bind2nd(greater<float>(),_lower))(val);
  }
};

struct is_empty : public unary_bool_function<string>
{
  bool operator()(string val)
  {
    return val != "";
  }
};

// The main class...

class Parameter
{
protected:
  int id;
  char sname;  // see getopt_long
  string lname;  // ditto
  string val;  
  string expl;   // for usage()
  bool needed; // is required?
  int takes_arg; // argument required?
  bool _is_set;
  
public:

  Parameter(int identifier,
            char shortname = 0,
            string longname = string(""),
            string value = string(""),
            string explanation = string(""),
            bool required = 0,
            int takes_argument = 0)
    :id(identifier),
     sname(shortname),
     lname(longname),
     val(value),
     expl(explanation),
     needed(required),
     takes_arg(takes_argument),
     _is_set(false)
  {
    if(value != "")
      takes_arg = 1;
  }

  virtual ~Parameter()
  {
  }
  
  template <typename UnaryBoolFunction>
  bool check(UnaryBoolFunction is_ok =
             always_ok<typename UnaryBoolFunction::argument_type>())
  {
    
    std::ostringstream ids;
    ids << id;
    if(getpname() == "")
      throw string("Parameter ") + ids.str() + string(" has no name");
    
    if(is_required() && !is_set())
      throw string("Parameter ") + getpname() + " is required but not set" ;

    bool ok = is_ok(getvalue<typename UnaryBoolFunction::argument_type>());
    if(!ok){
      throw string("Parameter ") + getpname() +
        ", is not ok. Value: \"" + val + "\""; 
    }
    return ok;
  }

  virtual void set(string value = 0) {
    val = value;_is_set = true;
  }

  virtual void set(char * value = 0) {
    _is_set = true;
    if(value == 0)
      return;
    set(string(value));
  }
  string getoptshort(){return sname != 0 ?
                         (takes_arg ? string(1,sname)+":":string(1,sname))
                         : "";}
  struct option getoptlong(){
    return (struct option) {lname.c_str(), (int)takes_arg, 0, id};
  }

  string getpname(){return sname != 0 ? lname + " ('" + sname + "')" : lname;}
  int getid() {return id;}
  bool is_set(){return _is_set;}
  bool is_required() {return needed;}
  template <typename T>  T getvalue();
  const string& getlname() const {return lname;}
  const char getsname() const {return sname;}
  int has_args() {return takes_arg;}
  string get_explanation() {return expl;}
};

template <typename T>  T Parameter::getvalue()
{
  std::istringstream ids(val);
  T value;
  ids >> value;
  if(ids.fail())
    throw string("Could not convert string \"") + val +
      "\" to type " + typeid(value).name(); 
  return value;
}

// specialization for string, needs to be in .cc file
template <> string Parameter::getvalue(); 

class ToggleParameter : public Parameter
{
public:
  void set(string value = string("0"))
  {
    val = string("1");
    _is_set = true;
  }
  
  void set(char * value = 0)
  {
    set(string("1"));
  }

  ToggleParameter(int identifier,
                  char shortname = 0,
                  string longname = string(""),
                  string explanation = "",
                  bool required = false)
    : Parameter(identifier, shortname, longname, "0", explanation, required)
  {
    takes_arg = false;
  }
  virtual ~ToggleParameter()
  {
  }
};


} // namespace clpar  
    

  
  
#endif // _INCL_CLPARAMETER_H
