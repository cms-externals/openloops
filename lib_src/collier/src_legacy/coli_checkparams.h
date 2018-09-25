* -*-Fortran-*-

***********************************************************************
*     file checkparams_coli.h                                         *
*     global input parameters for check option of  COLI               *
*---------------------------------------------------------------------*
*     02.05.08  Ansgar Denner     last changed  02.05.08              *
***********************************************************************

#ifdef CHECK

#ifdef PUBCHECK
      logical argcheck,conscheck,oldcheck,pubcheck
c      common /check/argcheck,conscheck,oldcheck,pubcheck
#else
      logical argcheck,conscheck,oldcheck
c      common /check/argcheck,conscheck,oldcheck
#endif

      integer   testout,errout	
      parameter (testout=20, errout=6)

      real*8 testacc
      parameter (testacc=1d-4)   
c      parameter (testacc=1d-11)   

      parameter (argcheck=.true.)
      parameter (conscheck=.true.)
      parameter (oldcheck=.true.)

c      argcheck=.true.
c      conscheck=.true.
c      oldcheck=.true.

#ifdef PUBCHECK
c      pubcheck=.true.
      parameter (pubcheck=.true.)
#endif
#endif
