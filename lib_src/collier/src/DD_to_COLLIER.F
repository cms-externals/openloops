**********************************************************************
        subroutine DDgetglobal(n,r,r1,r2,r3,r4,r5,r6)
**********************************************************************
*       get global DD parameters from common block
*       and pass them to COLLIER
*---------------------------------------------------------------------
*       27.2.2013 Lars Hofer
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer n,r,r1,r2,r3,r4,r5,r6

        n  = nmax
        r  = rmax
        r1 = rmax1
        r2 = rmax2
        r3 = rmax3
        r4 = rmax4
        r5 = rmax5
        r6 = rmax6

        end


**********************************************************************
        subroutine GetAccFlag_dd(val)
**********************************************************************
*       read accuracy information from DD and 
*       set accurcay flag of Collier
*---------------------------------------------------------------------
*       24.11.2014 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer val

        val=0

        if (accflag.eq.1) val = -1
        if (errflag.eq.1) val = -2

        end
**********************************************************************
        subroutine GetErrFlag_dd(val)
**********************************************************************
*       read error information from DD and 
*       set error flag of Collier
*---------------------------------------------------------------------
*       24.11.2014 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)

c DD global settings and common blocks
#       include "DD_global.inc"

c local variables
        integer val

        val = stopflag

        end
