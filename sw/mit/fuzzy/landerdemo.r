################################################################################
#
# File:         landerdemo.r
# RCS:          $Header: $
# Description:  Planetary lander demo of fuzzy inference
# Author:       Staal Vinterbo
# Created:      Wed Nov  5 22:58:26 2008
# Modified:     Wed Dec 17 17:23:11 2008 (Staal Vinterbo) staal@ding
# Language:     ESS[S]
# Package:      N/A
# Status:       Experimental
#
# landerdemo.r is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# landerdemo.r is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with landerdemo.r; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# (c) Copyright 2008, Staal Vinterbo, all rights reserved.
#
################################################################################

# load fuzzy inference engine and utilities
source('finfer.r')
source('finfer-logging.r')


########## Demonstration of use: planetary lander
## animates only when device is 'X11' or 'windows'
#  realtime == T makes animation realtime
#  realtime == F is max speed animation

landerdemo = function(realtime=T){

  logging.initiate() # see finfer-logging.r for explanation of what this does

  ####### Planetary Physics
  # planet X: gravity 9 m/s^2 (constant), no atmosphere
  # lander: controlled by 'dt' seconds bursts of up to 5 thrusters
  # thruster: each contributes 4 m/s^2 upward acceleration

  dt = 1         # seconds burst time
  g  = 9         # gravity: 9 m/s^2
  Tval  = 4      # each thruster has 4 m/s^2 acceleration
  newvel  = function(oldvel, n) g * dt + oldvel - n * Tval * dt # new velocity
  newdist = function(olddist, vel) olddist - vel * dt        # new distance

  # initial physics state
  startd = 10000 # starting distance (m) above surface
  startv = 300   # starting velocity (m/s) (downwards)

  
  ##### fuzzy lander controller sets

  # note that we can give the consequent sets as functions that
  #   ignore their arguments and return the fuzzy set values vectors
  
  # Thruster sets.   Count:      0    1    2    3    4    5
  break.hard   = function(...) c(0,   0  , 0  , 0  , 0  , 1)
  break.medium = function(...) c(0,   0  , 0.3, 1  , 0.8, 0)
  break.soft   = function(...) c(0,   0.2, 1  , 0.5, 0.3, 0)
  break.min    = function(...) c(0,   1  , 0  , 0  , 0  , 0)
  break.no     = function(...) c(1,   0  , 0  , 0  , 0  , 0)

  thrusters = length(break.hard(0)) # how many thrusters we have
  
  
  # velocities
  backwards  = mkmem(-Inf, 0, 0, name='backwards')
  veryslow   = mkmem(0, 0 , 15,  name='veryslow')
  slow       = mkmem(0,30,50,    name='slow')
  fast       = mkmem(40,100,150, name='fast')
  reallyfast = mkmem(100,150,Inf,name='reallyfast')

  thespeeds = c(backwards, veryslow, slow, fast, reallyfast) # for animation

  # distances
  veryfar    = mkmem(0.5 * startd, 0.8  * startd, Inf,        name='veryfar')
  far        = mkmem(0.3 * startd, 0.55  * startd,0.8* startd,name='far')
  close      = mkmem(0           , 0.4 * startd, 0.6 *startd, name='close')
  reallyclose= mkmem(-Inf        , 0.1  * startd, 0.3*startd,name='reallyclose')

  thedistances = c(reallyclose, close, far, veryfar)


  ### rules
  #  (dist and vel) -> breaking
  rules = c(
    mkrule(c(veryfar,     reallyfast), break.soft),     #1
    mkrule(c(veryfar,     fast),       break.min),      #2
    mkrule(c(veryfar,     slow),       break.no),       #3
    
    mkrule(c(far,         reallyfast), break.soft),     #4
    mkrule(c(far,         fast),       break.soft),     #5
    mkrule(c(far,         slow),       break.no),       #6
    
    mkrule(c(close,       reallyfast), break.hard),     #7
    mkrule(c(close,       fast),       break.hard),     #8
    mkrule(c(close,       slow),       break.min),      #9
    
    mkrule(c(reallyclose, reallyfast), break.hard),     #10
    mkrule(c(reallyclose, fast),       break.hard),     #11
    mkrule(c(reallyclose, slow),       break.medium),   #12
    mkrule(c(reallyclose, veryslow),   break.soft),     #13  
    mkrule(c(reallyclose, backwards),  break.no))       #14


  ##### Simulation 
  # 
  cols = rev(topo.colors(length(thespeeds))) # for animation
  tcols = heat.colors(thrusters+1)           # for animation
  animate = getOption('device') == 'X11' ||  getOption('device') == 'windows'

  curdist = startd # set start point
  curvel  = startv # set start velocity
  iteration = 0 
  tsecs = 0
  cat(iteration,': distance:', curdist, '\tspeed:', curvel)

  while(curdist > 0){
    # compute number of thrusters to use
    numt = round(cog(infer(rules,
      c(curdist,curvel),1:thrusters),1:thrusters)) - 1


    # update location and velocity of lander
    oldvel = curvel
    olddist = curdist
    curvel = newvel(curvel, numt)
    curdist = newdist(curdist, mean(c(oldvel, curvel)))

    # report in terminal and log
    cat('\tthrusters:', numt, '\n')
    tsecs = tsecs + numt    
    iteration = iteration + 1
    cat(iteration,': distance:', curdist, '\tspeed:', curvel)


    # animate
    if(animate){ 
      # plot lander    
      cli = which.max(sapply(thespeeds, function(s) s(curvel)))
      plot(0,curdist, ylim=c(0,startd),bg=cols[cli],pch=24,cex=2,xaxt='n',
           ylab='height (m)', xlab='')
      if(curdist > 0){
        # plot thruster flames
        points(0,curdist-150, ylim=c(0,startd),bg=tcols[numt+1],pch=24,
               cex=seq(0.5,1.5,1/thrusters)[numt+1])
        # write speed legend
        legend('bottomleft', legend=paste('speed:', curvel, 'm/s'))
      }
      if(realtime) Sys.sleep(dt)
    }

  }
  
  # compute ground hitting speed
  svel = round(oldvel + olddist*(curvel - oldvel)/(olddist - curdist),1)
  
  if(animate) legend('bottomleft', legend=paste('Hit at speed:', svel, 'm/s'))
  cat('\nHit surface at', svel, 'm/s. ')
  if (svel <= 9) cat('Yeah, survived!\n') else cat('Crashed.\n')
  cat('Used', dt*iteration, 'seconds and', dt*tsecs,
      'thrustseconds to do so.\n')
  cat('Done.\n')
  
  logging.analyzelog(list(thedistances, thespeeds), 1:thrusters - 1)
  logging.terminate()
}

  
  
