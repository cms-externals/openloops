* -*-Fortran-*-

***********************************************************************
*     file params_cache.h                                             *
*     global input parameters for integral CACHE                      *
*---------------------------------------------------------------------*
*     19.06.09  Ansgar Denner     last changed  19.06.09              *
***********************************************************************

      integer maxtype,maxcalc,maxcall,maxfct,maxx,maxcache

      parameter(maxtype=5,maxcalc=5800,maxcall=35000,maxfct=166,maxx=15,
     &    maxcache=50)
c      parameter(maxtype=5,maxcalc=580,maxcall=3500,maxfct=86,maxx=15,
c     &    maxcache=3)
c     parameter(maxtype=5,maxcalc=5,maxcall=35,maxfct=86,maxx=15)
