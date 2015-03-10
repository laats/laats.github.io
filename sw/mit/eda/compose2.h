/* -*-C++-*-
********************************************************************************
*
* File:         compose2.h
* RCS:          $Header: $
* Description:  Non standard SGI STL replacement for compose2
* Author:       Staal Vinterbo
* Created:      Fri Apr 15 16:40:43 2005
* Modified:     Wed May 11 12:36:38 2005 (Staal Vinterbo) staal@MGV
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

#ifndef _INCLUDE_COMPOSE2_H
#define _INCLUDE_COMPOSE2_H
#include <functional>
#ifndef __SGI_STL_FUNCTIONAL

namespace std 
{

  template <class BFT, class UFT1, class UFT2>
  class combiner211
    : public unary_function<typename UFT1::argument_type,
                            typename BFT::result_type>
  {
  protected:
    BFT bf;
    UFT1 uf1;
    UFT2 uf2;
  public:
    combiner211(const BFT &_bf, const UFT1 &_uf1, const UFT2 &_uf2)
      : bf(_bf), uf1(_uf1), uf2(_uf2) {}
    typename BFT::result_type
    operator()(const typename UFT1::argument_type &a) const 
    {
      return bf(uf1(a), uf2(a));
    }
  };
  
  
  template <class BFT, class UFT1, class UFT2>
  inline combiner211<BFT,UFT1,UFT2>
  compose2(const BFT &bf, const UFT1 &uf1, const UFT2 &uf2)
  {
    return combiner211<BFT,UFT1,UFT2>(bf, uf1, uf2);
  }
};

#endif // __SGI_STL_FUNCTIONAL


#endif // _INCLUDE_COMPOSE2_H
