* -*-Fortran-*-

***********************************************************************
*     file checkparams_coli.h                                         *
*     global input parameters for check option of  COLI               *
*---------------------------------------------------------------------*
*     02.05.08  Ansgar Denner     last changed  07.06.13              *
***********************************************************************

#ifdef CHECK

      logical argcheck

      integer testout,errout
      parameter (testout=20, errout=6)

      real*8 testacc
      parameter (testacc=1d-4)
c      parameter (testacc=1d-11)

      parameter (argcheck=.true.)



#endif
