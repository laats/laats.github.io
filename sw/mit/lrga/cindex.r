################################################################################
#
# File:         cindex.r
# RCS:          $Header: $
# Description:  
# Author:       Staal Vinterbo and Aleksander Ohrn
# Created:      Tue Jul 01 15:00:43 2003
# Modified:     Thu Dec 20 20:30:26 2007 (Staal Vinterbo) staal@STUMPA
# Language:     ESS[S]
# Package:      N/A
# Status:       GPL Distributable
#
# (c) Copyright 2003, Staal Vinterbo and Aleksander Ohrn, all rights reserved.
#
# cindex.r is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# cindex.r is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with cindex.r; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
################################################################################
#
# Revisions:
#
# Fri Nov 21 15:28:00 2003 (Staal Vinterbo) staal@MGV
#  Added GPL stuff
################################################################################

cindex <- function(preds, acts){
  if(length(preds) != length(acts)){
    warning("ci: length(preds) != length(act)\n")
    return(NA)
  }
  up <- sort(unique(preds))

  Q1 <- 0.0
  Q2 <- 0.0
  W <- 0.0

  n <- length(acts)
  ones <- sum(acts)
  zeros <- n - ones
  pairs <- ones * zeros

  if(pairs == 0){
    warning("ci: no pairs\n")
    return(NA)
  }

  obelow <- 0
  zbelow <- 0
  
  for(l in up){
    is <- (1:length(preds))[preds == l]
    iones <- sum(acts[is])
    izeros <- length(is) - iones

    oabove <- ones - (obelow + iones)   # num ones ranked above curr
    zabove <- zeros - (zbelow + izeros) # nr zeros ranked above curr

    w <- iones * (zbelow + 0.5 * izeros)
    W <- W + w

    q2 <- iones *
      (zbelow * zbelow + zbelow * izeros +
       (1.0/3.0) * izeros * izeros)

    Q2 <- Q2 + q2

    q1 <-izeros *
      (oabove*oabove +
       oabove * iones +
       (1.0/3.0) * iones * iones)

    Q1 <- Q1 + q1

    obelow <- obelow + iones
    zbelow <- zbelow + izeros
  }

  cindex <- W/pairs

  csq <- cindex * cindex    

  Q1 <- Q1 /(pairs * ones)
  Q2 <- Q2 /(pairs * zeros)
    
  n1 <- cindex * (1.0 - cindex)
  n2 <- (ones - 1) * (Q1 - csq)
  n3 <- (zeros - 1) * (Q2 - csq)

  se <- sqrt((n1 + n2 + n3)/pairs)

      # calculate se according to more conservative method
  q1 <- cindex/(2.0 - cindex)
  q2 <- (2.0 * csq)/(1.0 + cindex)
  n2 <- (ones - 1) * (q1 - csq)
  n3 <- (zeros - 1) * (q2 - csq)

  seb <- sqrt((n1 + n2 + n3)/pairs)

  m <- pairs/2.0
  sd <- sqrt((pairs * (ones + zeros + 1))/12.0)
  z = (W - m)/sd

  if(cindex < 0.5)
    cindex <- 1 - cindex

  return(c(cindex=cindex,se=se,seb=seb,z=z,m=m,sd=sd))
}

hanleyR <- function(corr,auc){

    # Do a range check.
    if (auc < 0.700 || auc > 0.975) { 
      warning("Mean AUC ($auc) outside range of Hanley-McNeil lookup table\n");
      return(NA);
    }
    if (corr < 0.02 || corr > 0.90) {
      warning("Mean correlation ($corr) outside range of Hanley-McNeil lookup table\n");
      return(NA);
    }

    staticr = c(
# 0.700 0.725 0.750 0.775 0.800 0.825 0.850 0.875 0.900 0.925 0.950 0.975
	0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01, # 0.02
	0.04, 0.04, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.02, 0.02, 0.02, # 0.04
	0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.04, 0.04, 0.04, 0.03, 0.02, # 0.06
	0.07, 0.07, 0.07, 0.07, 0.07, 0.06, 0.06, 0.06, 0.06, 0.05, 0.04, 0.03, # 0.08
	0.09, 0.09, 0.09, 0.09, 0.08, 0.08, 0.08, 0.07, 0.07, 0.06, 0.06, 0.04, # 0.10
	0.11, 0.11, 0.11, 0.10, 0.10, 0.10, 0.09, 0.09, 0.08, 0.08, 0.07, 0.05, # 0.12
	0.13, 0.12, 0.12, 0.12, 0.12, 0.11, 0.11, 0.11, 0.10, 0.09, 0.08, 0.06, # 0.14
	0.14, 0.14, 0.14, 0.14, 0.13, 0.13, 0.13, 0.12, 0.11, 0.11, 0.09, 0.07, # 0.16
	0.16, 0.16, 0.16, 0.16, 0.15, 0.15, 0.14, 0.14, 0.13, 0.12, 0.11, 0.09, # 0.18
	0.18, 0.18, 0.18, 0.17, 0.17, 0.17, 0.16, 0.15, 0.15, 0.14, 0.12, 0.10, # 0.20
	0.20, 0.20, 0.19, 0.19, 0.19, 0.18, 0.18, 0.17, 0.16, 0.15, 0.14, 0.11, # 0.22
	0.22, 0.22, 0.21, 0.21, 0.21, 0.20, 0.19, 0.19, 0.18, 0.17, 0.15, 0.12, # 0.24
	0.24, 0.23, 0.23, 0.23, 0.22, 0.22, 0.21, 0.20, 0.19, 0.18, 0.16, 0.13, # 0.26
	0.26, 0.25, 0.25, 0.25, 0.24, 0.24, 0.23, 0.22, 0.21, 0.20, 0.18, 0.15, # 0.28
	0.27, 0.27, 0.27, 0.26, 0.26, 0.25, 0.25, 0.24, 0.23, 0.21, 0.19, 0.16, # 0.30
	0.29, 0.29, 0.29, 0.28, 0.28, 0.27, 0.26, 0.26, 0.24, 0.23, 0.21, 0.18, # 0.32
	0.31, 0.31, 0.31, 0.30, 0.30, 0.29, 0.28, 0.27, 0.26, 0.25, 0.23, 0.19, # 0.34
	0.33, 0.33, 0.32, 0.32, 0.31, 0.31, 0.30, 0.29, 0.28, 0.26, 0.24, 0.21, # 0.36
	0.35, 0.35, 0.34, 0.34, 0.33, 0.33, 0.32, 0.31, 0.30, 0.28, 0.26, 0.22, # 0.38
	0.37, 0.37, 0.36, 0.36, 0.35, 0.35, 0.34, 0.33, 0.32, 0.30, 0.28, 0.24, # 0.40
	0.39, 0.39, 0.38, 0.38, 0.37, 0.36, 0.36, 0.35, 0.33, 0.32, 0.29, 0.25, # 0.42
	0.41, 0.40, 0.40, 0.40, 0.39, 0.38, 0.38, 0.37, 0.35, 0.34, 0.31, 0.27, # 0.44
	0.43, 0.42, 0.42, 0.42, 0.41, 0.40, 0.39, 0.38, 0.37, 0.35, 0.33, 0.29, # 0.46
	0.45, 0.44, 0.44, 0.43, 0.43, 0.42, 0.41, 0.40, 0.39, 0.37, 0.35, 0.30, # 0.48
	0.47, 0.46, 0.46, 0.45, 0.45, 0.44, 0.43, 0.42, 0.41, 0.39, 0.37, 0.32, # 0.50
	0.49, 0.48, 0.48, 0.47, 0.47, 0.46, 0.45, 0.44, 0.43, 0.41, 0.39, 0.34, # 0.52
	0.51, 0.50, 0.50, 0.49, 0.49, 0.48, 0.47, 0.46, 0.45, 0.43, 0.41, 0.36, # 0.54
	0.53, 0.52, 0.52, 0.51, 0.51, 0.50, 0.49, 0.48, 0.47, 0.45, 0.43, 0.38, # 0.56
	0.55, 0.54, 0.54, 0.53, 0.53, 0.52, 0.51, 0.50, 0.49, 0.47, 0.45, 0.40, # 0.58
	0.57, 0.56, 0.56, 0.55, 0.55, 0.54, 0.53, 0.52, 0.51, 0.49, 0.47, 0.42, # 0.60
	0.59, 0.58, 0.58, 0.57, 0.57, 0.56, 0.55, 0.54, 0.53, 0.51, 0.49, 0.45, # 0.62
	0.61, 0.60, 0.60, 0.59, 0.59, 0.58, 0.58, 0.57, 0.55, 0.54, 0.51, 0.47, # 0.64
	0.63, 0.62, 0.62, 0.62, 0.61, 0.60, 0.60, 0.59, 0.57, 0.56, 0.53, 0.49, # 0.66
	0.65, 0.64, 0.64, 0.64, 0.63, 0.62, 0.62, 0.61, 0.60, 0.58, 0.56, 0.51, # 0.68
	0.67, 0.66, 0.66, 0.66, 0.65, 0.65, 0.64, 0.63, 0.62, 0.60, 0.58, 0.54, # 0.70
	0.69, 0.69, 0.68, 0.68, 0.67, 0.67, 0.66, 0.65, 0.64, 0.63, 0.60, 0.56, # 0.72
	0.71, 0.71, 0.70, 0.70, 0.69, 0.69, 0.68, 0.67, 0.66, 0.65, 0.63, 0.59, # 0.74
	0.73, 0.73, 0.72, 0.72, 0.72, 0.71, 0.71, 0.70, 0.69, 0.67, 0.65, 0.61, # 0.76
	0.75, 0.75, 0.75, 0.74, 0.74, 0.73, 0.73, 0.72, 0.71, 0.70, 0.68, 0.64, # 0.78
	0.77, 0.77, 0.77, 0.76, 0.76, 0.76, 0.75, 0.74, 0.73, 0.72, 0.70, 0.67, # 0.80
	0.79, 0.79, 0.79, 0.79, 0.78, 0.78, 0.77, 0.77, 0.76, 0.75, 0.73, 0.70, # 0.82
	0.82, 0.81, 0.81, 0.81, 0.81, 0.80, 0.80, 0.79, 0.78, 0.77, 0.76, 0.73, # 0.84
	0.84, 0.84, 0.83, 0.83, 0.83, 0.82, 0.82, 0.81, 0.81, 0.80, 0.78, 0.75, # 0.86
	0.86, 0.86, 0.86, 0.85, 0.85, 0.85, 0.84, 0.84, 0.83, 0.82, 0.81, 0.79, # 0.88
	0.88, 0.88, 0.88, 0.88, 0.87, 0.87, 0.87, 0.86, 0.86, 0.85, 0.84, 0.82  # 0.90
		     );

    staticr <- matrix(staticr, ncol = 12, byrow=TRUE)
    aucdelta <- 0.025;
    aucstart <- 0.700;

    # Lookup AUC index.
    indexauc   <- (auc - aucstart) / aucdelta;
    indexaucl <- floor(indexauc);
    indexaucr <- ceiling(indexauc);
    diffaucl  <- auc - (aucstart + (indexaucl * aucdelta));
    diffaucr  <- (aucstart + (indexaucr * aucdelta)) - auc;

    corrdelta <- 0.02;
    corrstart <- 0.02;

    # Lookup correlation index.
    indexcorr   <- (corr - corrstart) / corrdelta;
    indexcorrl <- floor(indexcorr);
    indexcorrr <- ceiling(indexcorr);
    diffcorrl  <- corr - (corrstart + (indexcorrl * corrdelta));
    diffcorrr  <- (corrstart + (indexcorrr * corrdelta)) - corr;

    
    # Lookup candidates.
    rll <- staticr[indexcorrl+1,indexaucl+1];
    rlr <- staticr[indexcorrl+1,indexaucr+1];
    rrl <- staticr[indexcorrr+1,indexaucl+1];
    rrr <- staticr[indexcorrr+1,indexaucr+1];

    # Normalize index lookup errors.
    diffaucln  <- diffaucl  / (diffaucl  + diffaucr);
    diffaucrn  <- diffaucr  / (diffaucl  + diffaucr);
    diffcorrln <- diffcorrl / (diffcorrl + diffcorrr);
    diffcorrrn <- diffcorrr / (diffcorrl + diffcorrr);

    # Compute interpolation weights.
    weightll <- (1.0 - diffcorrln) * (1.0 - diffaucln);
    weightlr <- (1.0 - diffcorrln) * (1.0 - diffaucrn);
    weightrl <- (1.0 - diffcorrrn) * (1.0 - diffaucln);
    weightrr <- (1.0 - diffcorrrn) * (1.0 - diffaucrn);

    r <- (weightll * rll) + (weightlr * rlr) +
            (weightrl * rrl) + (weightrr * rrr);
    return(r);
}

# main Hanley McNeil test routine (same cases)
hanley <- function(paref1, paref2, aaref,ctype="pearson"){
  aaref2 <- aaref
  aaref1 <- aaref

#  cat(c("p1 [", paref1, "]\n"))
#  cat(c("p2 [", paref2, "]\n"))
#  cat(c("a [", aaref2, "]\n"))    

  if(length(paref1) != length(aaref1) || length(paref2) != length(aaref2)
     || length(paref1) != length(aaref2)){
    warning("C-index difference test: series size difference\n");
    return(NA);
  }
  if(length(paref1) == 0){
    warning("C-index difference test: empty input, nothing to do\n");
    return(NA);
  }

  ci1 <- cindex(paref1, aaref1)
  ci2 <- cindex(paref2, aaref2)
  auc1 <- ci1[1]
  se1 <- ci1[2]
  auc2 <- ci2[1]
  se2 <- ci2[2]
  zi <- aaref1 == 0
  oi <- !zi
  
  corr0s <- cor.test(paref1[zi],paref2[zi], method=ctype)$estimate
  corr1s <- cor.test(paref1[oi],paref2[oi], method=ctype)$estimate    
  
  auc  <- 0.5 * (auc1 + auc2);
  corr <- 0.5 * (corr0s + corr1s);
  if(is.na(corr)){
    warning("correlation NA in hanley()\n")
    return(NA)
  }
  r <- hanleyR(corr, auc);
  z = (auc1 - auc2) /
    sqrt((se1 * se1) + (se2 * se2) - (2 * r * se1 * se2));
  p = 2 * (1 - pnorm(abs(z))) # (z < 0.0) ? -z : z));
  return (c(p=p, auc1=auc1, auc2=auc2, se1=se1, se2=se2, z=z, r=r, corr0s=corr0s,
            corr1s=corr1s));
}
  
mcnemar <- function(corr1, corr2){
  rr <- sum(corr1 & corr2)
  rw <- sum(corr1 & !corr2)
  wr <- sum(corr2 & !corr1)
  ww <- sum(!corr1 & !corr2)
  if(rw + wr == 0) {
    warning("McNemar: diagonal contingency matrix, Z-statistic not defined.");
    return(NA);
  }

  z = (abs(rw - wr) - 1) / sqrt(rw + wr);
  p = 2 * (1 - pnorm(z));

  return(p=p,z=z)
}
