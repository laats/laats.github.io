/* -*-C++-*-
********************************************************************************
*
* File:         farith.h
* RCS:          $Header: $
* Description:  
* Author:       Staal Vinterbo
* Created:      Thu Apr 21 10:49:29 2005
* Modified:     Wed May 11 11:11:46 2005 (Staal Vinterbo) staal@MGV
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

#ifndef _INCLUDE_FARITH_H
#define _INCLUDE_FARITH_H

struct Float
{
  float f;
  operator float &()
  {
    return f;
  }
  operator float () const 
  {
    return f;
  }
  Float & operator=(const float v)
  {
    f = v;
    return *this;
  }
  Float(const float v = 0.0)
  {
    f = v;
  }
};


struct Double
{
  double d;
  operator double &()
  {
    return d;
  }
  operator double () const 
  {
    return d;
  }
  Double & operator=(const double v)
  {
    d = v;
    return *this;
  }
  Double(const double v = 0.0)
  {
    d = v;
  }
};





bool operator<(Float, float);
bool operator>(Float, float);
bool operator==(Float, float);
int farith_sign(float);

/* bool operator-(float, float); */
/* bool operator+(float, float); */
/* bool operator*(float, float); */
/* bool operator/(float, float); */


bool operator<(Double, double);
bool operator>(Double, double);
bool operator==(Double, double);
int farith_sign(double);

/* bool operator-(double, double); */
/* bool operator+(double, double); */
/* bool operator*(double, double); */
/* bool operator/(double, double); */

#endif // _INCLUDE_FARITH_H
