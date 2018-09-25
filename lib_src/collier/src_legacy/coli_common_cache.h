* -*-Fortran-*-

***********************************************************************
*     file common_cache.h                                             *
*     contains global common blocks for CACHE                         *
*---------------------------------------------------------------------*
*     19.06.09  Ansgar Denner     last changed  19.06.09              *
***********************************************************************

      complex*16 f(maxfct,maxtype,maxcalc),
     &    arg(maxx,maxtype,maxcalc)
      integer grank(maxtype,maxcalc,maxcache),
     &    gswitch(maxtype,maxcalc,maxcache)
      integer lrank(maxtype,maxcalc,maxcache),
     &    lswitch(maxtype,maxcalc,maxcache)
      integer ncalc(maxtype,maxcache),calc(maxtype,maxcall,maxcache)
      integer pointer(maxtype,maxcall,maxcache),ncall(maxtype,maxcache)
     &    ,ninit(maxcache),cache
      character*11 name(maxtype,maxcache)

      integer usecache(maxcache),usecachesave(maxcache),
     &    cachelevel(maxcache),cacheleveli(maxcache)
      common/cacher/f,arg
      common/cachei/grank,lrank,gswitch,lswitch,
     &             ncalc,calc,pointer,ncall,ninit,cache
      common/cachec/name
      common/cachem/usecache,usecachesave,cachelevel,cacheleveli
