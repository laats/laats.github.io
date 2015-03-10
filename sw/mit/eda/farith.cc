/* -*-C++-*-
********************************************************************************
*
* File:         farith.cc
* RCS:          $Header: $
* Description:  Avoid numerical instabilities...
* Author:       Staal Vinterbo
* Created:      Thu Apr 21 10:52:38 2005
* Modified:     Wed May 11 11:16:25 2005 (Staal Vinterbo) staal@MGV
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

#include <gmp.h>
#include <limits>
using std::numeric_limits;

#include "farith.h"

bool operator<(Float v1, float v2)
{
  mpf_t mv1;
  mpf_init_set_d(mv1, static_cast<double>(v1));
  return (mpf_cmp_d(mv1,static_cast<double>(v2)) < 0);
}
  
bool operator>(Float v1, float v2)
{
  mpf_t mv1;
  mpf_init_set_d(mv1, static_cast<double>(v1));
  return (mpf_cmp_d(mv1,static_cast<double>(v2)) > 0);
}

bool operator==(Float v1, float v2)
{
  mpf_t op1, op2;
  mpf_init_set_d(op1, static_cast<double>(v1));
  mpf_init_set_d(op2, static_cast<double>(v2));
  return static_cast<bool>(mpf_eq (op1, op2,
                                   numeric_limits<float>::digits10 - 1));
}

int farith_sign(float v)
{
  mpf_t mv1;
  mpf_init_set_d(mv1, static_cast<double>(v));
  return mpf_sgn(mv1);
}


// inline bool operator-(float, float);
// inline bool operator+(float, float);
// inline bool operator*(float, float);
// inline bool operator/(float, float);


bool operator<(Double v1, double v2)
{
  mpf_t mv1;
  mpf_init_set_d(mv1, v1);
  return (mpf_cmp_d(mv1,v2) < 0); 
}

bool operator>(Double v1, double v2)
{
  mpf_t mv1;
  mpf_init_set_d(mv1,v1);
  return (mpf_cmp_d(mv1,v2) > 0);
}

bool operator==(Double v1, double v2)
{
  mpf_t op1, op2;
  mpf_init_set_d(op1, v1);
  mpf_init_set_d(op2, v2);
  return static_cast<bool>(mpf_eq (op1, op2,
                                   numeric_limits<double>::digits10 - 1)); 
}

int farith_sign(double v)
{
  mpf_t mv1;
  mpf_init_set_d(mv1, v);
  return mpf_sgn(mv1);
}



// inline bool operator-(double, double);
// inline bool operator+(double, double);
// inline bool operator*(double, double);
// inline bool operator/(double, double);

