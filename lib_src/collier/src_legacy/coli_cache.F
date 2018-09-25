***********************************************************************
*                                                                     *
*     cache for loop integrals                                        *
*                                                                     *
***********************************************************************
*                                                                     *
*     last changed  19.06.09  Ansgar Denner                           *
*                                                                     *
***********************************************************************
* subroutines:                                                        *
* cacheoff,cacheon,setcachelevel                                      *
* cacheinit,cachewrite,cacheread                                      *
* writeinit,writecount                                                *
***********************************************************************
c cache

***********************************************************************
*                                                                     *
*     fast evaluation of tensor integrals                             *
*                                                                     *
*     written by Markus Roth,          3.01.05                        *
*     adapted by Ansgar Denner,       18.01.05                        *
*     adapted to multiple caches      19.06.09                        *
*     last modified by Ansgar Denner, 19.06.09                        *
*                                                                     *
* subroutines:                                                        *
* cacheon, cacheoff, setcachelevel, cachetempoff, cachereon           *
* cacheinit, cacheread, cachewrite                                    *
***********************************************************************
      subroutine cacheon(cachein)
***********************************************************************
*     switch cache for loop integrals on                              *
*---------------------------------------------------------------------*
*     20.01.05  Ansgar Denner     last changed  19.06.09              *
***********************************************************************
      implicit none

      include "coli_params_cache.h"
      include "coli_common_cache.h"

      integer cachein

      usecache(cachein) = 1
      usecachesave(cachein) = 1

      ninit(cachein)=0      

      end
***********************************************************************
      subroutine cacheoff(cachein)
***********************************************************************
*     switch cache for loop integrals off                             *
*---------------------------------------------------------------------*
*     20.01.05  Ansgar Denner     last changed  19.06.09              *
***********************************************************************
      implicit none

      include "coli_params_cache.h"
      include "coli_common_cache.h"

      integer cachein
      usecache(cachein) = 0
      usecachesave(cachein) = 0

      end
***********************************************************************
      subroutine setcachelevel(cachelevelin,cachein)
***********************************************************************
*     switch level for loop integral cache                            *
*     E = 1, D = 2, C = 3, B = 4                                      *
*---------------------------------------------------------------------*
*     20.01.05  Ansgar Denner     last changed  19.06.09              *
***********************************************************************
      implicit none

      include "coli_params_cache.h"
      include "coli_common_cache.h"

      integer  cachelevelin,cachein

      cachelevel(cachein) = cachelevelin
      cacheleveli(cachein) = cachelevelin
      end
***********************************************************************
      subroutine cachetempoff
***********************************************************************
*     switch cache for loop integrals temporarily off                 *
*---------------------------------------------------------------------*
*     20.01.05  Ansgar Denner     last changed  19.06.09              *
***********************************************************************
      implicit none
      integer cachein

      include "coli_params_cache.h"
      include "coli_common_cache.h"

      usecache(cache) = 0

      end

***********************************************************************
      subroutine cachereon
***********************************************************************
*     switch cache for loop integrals back on                         *
*---------------------------------------------------------------------*
*     20.01.05  Ansgar Denner     last changed  19.06.09              *
***********************************************************************
      implicit none

      include "coli_params_cache.h"
      include "coli_common_cache.h"

      usecache(cache) = usecachesave(cache)

      end


***********************************************************************
      subroutine getmaxcache(maxcacheout)
***********************************************************************
*     returns value of maxcache parameter                             *
***********************************************************************
      implicit none
      integer maxcacheout

      include "coli_params_cache.h"

      maxcacheout=maxcache

      end

***********************************************************************
      subroutine cachetempoffch(cachein)
***********************************************************************
*     switch specific cache channel temporarily off                   *
*---------------------------------------------------------------------*
*                                                                     *
***********************************************************************
      implicit none
      integer cachein

      include "coli_params_cache.h"
      include "coli_common_cache.h"

      usecache(cachein) = 0

      end

***********************************************************************
      subroutine cachereonch(cachein)
***********************************************************************
*     switch specific cache channel back on                           *
*---------------------------------------------------------------------*
*
***********************************************************************
      implicit none
      integer cachein

      include "coli_params_cache.h"
      include "coli_common_cache.h"

      usecache(cachein) = usecachesave(cachein)

      end

***********************************************************************
      subroutine cacheinit(nout,cachein)
***********************************************************************
*     initialization of fast evaluation of tensor integrals           *
*                                                                     *
*     written by Markus Roth, 3.1.2005                                *
*     adapted by Ansgar Denner                                        *
*---------------------------------------------------------------------*
*     20.01.05  Ansgar Denner     last changed  19.06.09              *
***********************************************************************
      implicit none

      include "coli_params_cache.h"
      include "coli_common_cache.h"

      integer cachein

c local variables
      integer i1,i2,nout

      common/cacheh/ncalc2,ncalc3
      integer ncalc2(maxtype,maxcache),ncalc3(maxtype,maxcache)

      data usecache/maxcache*0/,usecachesave/maxcache*0/,
     &     cachelevel /maxcache*20/, cacheleveli /maxcache*20/, 
     &    ninit /maxcache*0/

      integer countC
      common /countC/ countC
      integer countD
      common /countD/ countD
      integer countE
      common /countE/ countE

c>    write(*,*) 'cacheinit cachein= ',cachein,maxcache
c>    write(*,*) 'cacheinit cachein: ',ninit
c>    write(*,*) 'cacheinit cachein: ',usecache
c>    write(*,*) 'cacheinit cachein: ',usecachesave

      cache=cachein

c initialization for the first call
      if(ninit(cache).eq.0)then
        do i1=1,maxtype
          ncalc(i1,cache)=0
          do i2=1,maxcall
            calc(i1,i2,cache)=0
          enddo
        enddo
      endif
c output of initialization
      if(nout.ne.0.and.ninit(cache).le.maxtype.and.usecache(cache).gt.0)
     &    then
        if(ninit(cache).eq.0)then
          write(nout,'(a)')' '
          write(nout,'(1x,a5,i2,(a))') 'cache',cache,
     *      ': Tensor integral      Type        Calls     Calculations'
        elseif(ncalc(ninit(cache),cache).ne.0)then
          write(nout,'(1x,a5,i2,a2,a11,3(4x,i9))') 'cache',cache,': ',
     &        name(ninit(cache),cache),ninit(cache),
     &        ncall(ninit(cache),cache),ncalc(ninit(cache),cache)
        endif 
      endif

c initialization for each event
      if(usecache(cache).gt.0) then
        if (ninit(cache).le.maxtype+10) then
          do i2=1,maxcalc
            do i1=max(ninit(cache)-10,1),min(maxtype,ninit(cache)-1)
              if (lrank(i1,i2,cache).lt.grank(i1,i2,cache)) then
                grank(i1,i2,cache) = lrank(i1,i2,cache)
                gswitch(i1,i2,cache) = lswitch(i1,i2,cache)
              else if (lrank(i1,i2,cache).eq.grank(i1,i2,cache)
     &                 .and.lswitch(i1,i2,cache).eq.0) then
                gswitch(i1,i2,cache) = 0
              end if   
            end do
          end do
c>          do i1=max(ninit(cache)-10,1),min(maxtype,ninit(cache)-1)
c>            do i2=1,maxcalc
c>              if (ninit(cache).eq.1) then
c>                write(11,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              else if (ninit(cache).eq.2) then
c>                write(12,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              else if (ninit(cache).eq.3) then
c>                write(13,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              else if (ninit(cache).eq.4) then
c>                write(14,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              else if (ninit(cache).eq.5) then
c>                write(15,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              else if (ninit(cache).eq.6) then
c>                write(16,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              else if (ninit(cache).eq.7) then
c>                write(17,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              else if (ninit(cache).eq.8) then
c>                write(18,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              else if (ninit(cache).eq.9) then
c>                write(19,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              else if (ninit(cache).eq.10) then
c>                write(20,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              else if (ninit(cache).eq.11) then
c>                write(21,*) i1,i2, grank(i1,i2,cache),gswitch(i1,i2,cache)
c>              end if
c>            end do
c>          end do
        end if

        ninit(cache)=ninit(cache)+1

c>      write(*,*) 'cacheinit:      ninit(cache) = ',ninit(cache)
c>      write(*,*) 'cacheinit:     ncalc = ',
c>     &    ncalc(1,cache),ncalc(2,cache),ncalc(3,cache),ncalc(4,cache)
c>      write(*,*) 'cacheinit:     ncall = ',
c>     &    ncall(1,cache),ncall(2,cache),ncall(3,cache),ncall(4,cache)

        do i1=1,maxtype 
          ncall(i1,cache)=0
c        ncalc2(i1,cache)=0
c        ncalc3(i1,cache)=0
        end do

      end if

      countC = 0
      countD = 0
      countE = 0

      end

***********************************************************************
      subroutine cacheread(fct,x,nfct,nx,type,nrank,nswitch,
     &    namefct,nocalc)
***********************************************************************
*     fast evaluation of tensor integrals                             *
*                                                                     *
*     written by Markus Roth, 3.1.2005                                *
*     adapted by Ansgar Denner                                        *
*---------------------------------------------------------------------*
*     20.01.05  Ansgar Denner     last changed  19.06.09              *
***********************************************************************
      implicit none

      include "coli_params_cache.h"
      include "coli_common_cache.h"

      complex*16 fct(maxfct),x(maxx)
      integer i1,i2,nfct,nx,type,nrank,nswitch
      logical nocalc,same
      character*11 namefct

      nocalc=.false.
c      write(*,*) 'cacheread in ',
c     &    usecache(cache),usecachesave(cache),cachelevel(cache),cacheleveli(cache),
c     &    n,type,cachelevel(cache).lt.type,n.lt.type
      if(cachelevel(cache).lt.type.or.ninit(cache).lt.type
     &    .or.usecache(cache).eq.0)
     &    return
c      write(*,*)  'cacheread2 ',ninit(cache),type,ncall(type,cache)+1
      if(ninit(cache).gt.type+10)then
c use full optimization
        ncall(type,cache)=ncall(type,cache)+1
c        if (pointer(type,ncall(type,cache),cache).eq.0) then
c        write(*,*) 'cacheread1 ',type,ninit(cache),ncall(type,cache),
c     &      calc(type,ncall(type,cache),cache)
c        write(*,*) 'cacheread2 ',
c     &      nrank,pointer(type,ncall(type,cache),cache)
c        write(*,*) 'cacheread3 ',
c     &      usecache(cache),usecachesave(cache),cachelevel(cache),cacheleveli(cache)
c        write(*,*) 'cacheread4 ',
c     &      grank(type,pointer(type,ncall(type,cache)),cache)
c        end if

        if(calc(type,ncall(type,cache),cache).eq.1)then
c calculation of function
c          ncalc2(type,cache) = ncalc2(type,cache) + 1
c          ncalc3(type,cache) = ncalc3(type) + 1
c          if (pointer(type,ncall(type,cache),cache).ne.
c     &      ncalc2(type,cache)) then
c            write(*,*)  'cache error',
c     &          pointer(type,ncall(type,cache),cache),ncalc2(type,cache)
c          end if
          if (nrank.eq.
     &        grank(type,pointer(type,ncall(type,cache),cache),cache))
     &        then
            if (nswitch.eq.0) then
              nswitch =  
     &            gswitch(type,pointer(type,ncall(type,cache),cache)
     &            ,cache)
            else if (nswitch.eq.
     &           -gswitch(type,pointer(type,ncall(type,cache),cache)
     &            ,cache)) 
     &            then
              write(*,*) 'increase of nrank should not happen! a '
              nrank = nrank + 1
              nswitch = 0
            end if
          else if (nrank.lt.
     &          grank(type,pointer(type,ncall(type,cache),cache),cache))
     &          then
            nrank = 
     &          grank(type,pointer(type,ncall(type,cache),cache),cache)
            nswitch = 
     &          gswitch(type,pointer(type,ncall(type,cache),cache),
     &          cache)
          end if 
          lrank(type,pointer(type,ncall(type,cache),cache),cache) 
     &        = nrank
          lswitch(type,pointer(type,ncall(type,cache),cache),cache) 
     &        = nswitch
c     check whether calculated rank is sufficient
        else if 
     &        (nrank.lt.
     &        lrank(type,pointer(type,ncall(type,cache),cache),cache)
     &        .or.
     &        nrank.eq.
     &        lrank(type,pointer(type,ncall(type,cache),cache),cache)
     &        .and.
     &        (nswitch.eq.
     &        lswitch(type,pointer(type,ncall(type,cache),cache),cache)
     &        .or.nswitch.eq.0)) then
c using already calculated result to fasten computation
          nocalc=.true.
          do i1=1,nfct
            fct(i1)=f(i1,type,pointer(type,ncall(type,cache),cache))
          enddo
        else
c recalculate with cache off
          cacheleveli(cache) = cachelevel(cache)
          cachelevel(cache) = type
c          write(*,*) 'cachelevel changed to ',cachelevel(cache)
c          ncalc3(type,cache) = ncalc3(type,cache) + 1
          if (nrank.eq.
     &        lrank(type,pointer(type,ncall(type,cache),cache),cache)) 
     &        then
            if (lswitch(type,pointer(type,ncall(type,cache),cache),
     &          cache).eq.0) 
     &          then
              lswitch(type,pointer(type,ncall(type,cache),cache),cache) 
     &            = nswitch
            else 
              write(*,*) 'increase of nrank should not happen! b '
              nrank = nrank + 1
              nswitch = 0
              lrank(type,pointer(type,ncall(type,cache),cache),cache) 
     &            = nrank
              lswitch(type,pointer(type,ncall(type,cache),cache),cache) 
     &            = 0
            end if
          endif
        end if
      else if(ninit(cache).gt.type)then
c try to minimize grank and gswitch
c not yet full optimization
        ncall(type,cache)=ncall(type,cache)+1
c        if (pointer(type,ncall(type,cache),cache).eq.0) then
c        write(*,*) 'cacheread1 ',type,n,ncall(type,cache),calc(type,ncall(type,cache),cache)
c        write(*,*) 'cacheread2 ',nrank,pointer(type,ncall(type,cache),cache)
c        write(*,*) 'cacheread3 ',usecache(cache),usecachesave(cache),cachelevel(cache),cacheleveli(cache)
c        write(*,*) 'cacheread4 ',grank(type,pointer(type,ncall(type,cache),cache),cache)
c        end if

        if(calc(type,ncall(type,cache),cache).eq.1)then
c calculation of function
          lrank(type,pointer(type,ncall(type,cache),cache),cache) 
     &        = nrank
          lswitch(type,pointer(type,ncall(type,cache),cache),cache) 
     &        = nswitch
c          ncalc2(type,cache) = ncalc2(type,cache) + 1
c          ncalc3(type,cache) = ncalc3(type,cache) + 1
c          if (pointer(type,ncall(type,cache),cache).ne.ncalc2(type,cache)) then
c            write(*,*)  'cache error',
c     &          pointer(type,ncall(type,cache),cache),ncalc2(type,cache)
c          end if
          if (nrank.eq.
     &        grank(type,pointer(type,ncall(type,cache),cache),cache) )
     &        then
            if (nswitch.eq.0) then
              nswitch =  
     &            gswitch(type,pointer(type,ncall(type,cache),cache)
     &            ,cache)
            else if (nswitch.eq.
     &            -gswitch(type,pointer(type,ncall(type,cache),cache)
     &            ,cache)) then
              write(*,*) 'increase of nrank should not happen! c '
              nrank = nrank + 1
              nswitch = 0
            end if
          else if (nrank.lt.
     &          grank(type,pointer(type,ncall(type,cache),cache),cache))
     &          then
            nrank = grank(type,pointer(type,ncall(type,cache),cache)
     &          ,cache)
            nswitch =gswitch(type,pointer(type,ncall(type,cache),cache)
     &          ,cache)
          end if 
        else 
c store maximal rank to lrank
          if (nrank.gt.lrank(type,pointer(type,ncall(type,cache),cache),
     &        cache)) then
            lrank(type,pointer(type,ncall(type,cache),cache),cache)
     &          = nrank
            lswitch(type,pointer(type,ncall(type,cache),cache),cache)
     &          = nswitch
          else if (nrank.eq.
     &          lrank(type,pointer(type,ncall(type,cache),cache),cache))
     &          then
            if (nswitch.ne.lswitch(type,pointer(type,ncall(type,cache)
     &          ,cache),cache)
     &          .and.nswitch.ne.0) then
              if (lswitch(type,pointer(type,ncall(type,cache),cache)
     &            ,cache).eq.0)
     &            then
                lswitch(type,pointer(type,ncall(type,cache),cache)
     &              ,cache) = nswitch
              else 
                lrank(type,pointer(type,ncall(type,cache),cache),cache) 
     &               =lrank(type,pointer(type,ncall(type,cache),cache)
     &              ,cache) 
     &              + 1
                lswitch(type,pointer(type,ncall(type,cache),cache)
     &              ,cache) = 0
              end if
            end if
          end if              
c     check whether calculated rank is sufficient
          if (nrank.lt.grank(type,pointer(type,ncall(type,cache),cache)
     &        ,cache).or.
     &       nrank.eq.grank(type,pointer(type,ncall(type,cache),cache)
     &        ,cache)
     &        .and.
     &        (nswitch.eq.gswitch(type,pointer(type,ncall(type,cache)
     &        ,cache),cache)
     &        .or.nswitch.eq.0)) then
c using already calculated result to fasten computation
            nocalc=.true.
            do i1=1,nfct
              fct(i1)=f(i1,type,pointer(type,ncall(type,cache),cache))
            enddo
          else
c recalculate with cache off
            cacheleveli(cache) = cachelevel(cache)
            cachelevel(cache) = type
c     write(*,*) 'cachelevel changed to ',cachelevel(cache)
c            ncalc3(type) = ncalc3(type,cache) + 1
            if (nrank.eq.
     &          grank(type,pointer(type,ncall(type,cache),cache),cache)) 
     &          then
              if (gswitch(type,pointer(type,ncall(type,cache),cache)
     &            ,cache).ne.0) 
     &            then
                write(*,*) 'increase of nrank should not happen! d '
                write(*,*) 'type,rank ',type,nrank,
     &              grank(type,pointer(type,ncall(type,cache),cache)
     &              ,cache)
                write(*,*) 'switch ',nswitch,
     &              gswitch(type,pointer(type,ncall(type,cache),cache)
     &              ,cache)
                nrank = nrank + 1
                nswitch = 0
              end if
            else
              lrank(type,pointer(type,ncall(type,cache),cache),cache)
     &            = nrank
              lswitch(type,pointer(type,ncall(type,cache),cache),cache)
     &            = nswitch
            endif
          end if
        end if
      elseif(ninit(cache).eq.type)then
c      write(*,*)  'cacheread n=type'
c     error messages
        if(type.gt.maxtype)then
          write(*,'(a)')' fastin: maxtype too small'
          stop
        endif
        if(ncall(type,cache)+1.gt.maxcall)then
          write(*,'(a)')' fastin: maxcall too small'
          write(*,*) type,ncall(type,cache),maxcall
          stop
        endif
        if(ncalc(type,cache)+1.gt.maxcalc)then
          write(*,'(a)')' fastin: maxcalc too small'
          stop
        endif
        if(nfct.gt.maxfct)then
          write(*,'(a)')' fastin: maxfct too small'
          stop
        endif
        if(nx.gt.maxx)then
          write(*,'(a)')' fastin: maxx too small'
          stop
        endif
c initialization for function type
        ncall(type,cache)=ncall(type,cache)+1
        do i1=1,ncalc(type,cache)
          same=.true.
          do i2=1,nx
            if(arg(i2,type,i1).ne.x(i2))same=.false.
          enddo
          if(same)then
            pointer(type,ncall(type,cache),cache)=i1
            calc(type,ncall(type,cache),cache)=0
c            grank(type,i1,cache)=max(grank(type,i1),nrank,cache)
            if (nrank.gt.grank(type,i1,cache)) then
              grank(type,i1,cache) = nrank
              gswitch(type,i1,cache) = nswitch
            else if (nrank.eq.grank(type,i1,cache)) then
              if (nswitch.ne.gswitch(type,i1,cache).and.nswitch.ne.0)
     &            then
                if (gswitch(type,i1,cache).eq.0) then
                  gswitch(type,i1,cache) = nswitch
                else 
                  write(*,*) 'increase of nrank should not happen! e '
                  nrank = nrank + 1
                  nswitch = 0
                  grank(type,i1,cache) = nrank
                  gswitch(type,i1,cache) = 0
                end if
              end if
            end if              
            return
          endif
        enddo
        ncalc(type,cache)=ncalc(type,cache)+1
        pointer(type,ncall(type,cache),cache)=ncalc(type,cache)
        calc(type,ncall(type,cache),cache)=1
        grank(type,ncalc(type,cache),cache)=nrank
        gswitch(type,ncalc(type,cache),cache)=nswitch
        name(type,cache)=namefct
        do i1=1,nx
          arg(i1,type,ncalc(type,cache))=x(i1)
        enddo
      endif

      return

c      write(*,10)
      write(*,12) 'cacheread: usecache =',usecache(cache),usecachesave
     &                     ,cachelevel(cache),cacheleveli(cache)
      write(*,10) 'cacheread:  ninit = ',ninit(cache)
      write(*,10) 'cacheread:   type = ',type
      write(*,10) 'cacheread:  ncalc = ',ncalc(type,cache)
c      write(*,10) 'cacheread: ncalc3 = ',ncalc3(type,cache)
c      write(*,10) 'cacheread: ncalc2 = ',ncalc2(type,cache)
      write(*,10) 'cacheread:  ncall = ',ncall(type,cache)
      write(*,10) 'cacheread:     pt = ',
     &    pointer(type,ncall(type,cache),cache)
      write(*,11) 'cacheread:   name = ',name(type,cache)
      if (ninit(cache).ge.type) then
      write(*,10) 'cacheread:  grank = ',
     &      grank(type,ncalc(type,cache),cache)
      write(*,10) 'cacheread: gswitch= ',
     &    gswitch(type,ncalc(type,cache),cache)
      write(*,10) 'cacheread:  nrank = ',nrank
      end if
 10   format(A21,i4)
 11   format(A21,a11)
 12   format(A22,3i4)
 
      end

***********************************************************************
      subroutine cachewrite(fct,nfct,type)
***********************************************************************
*     fast evaluation of tensor integrals                             *
*                                                                     *
*     written by Markus Roth, 3.1.2005                                *
*     adapted by Ansgar Denner                                        *
*---------------------------------------------------------------------*
*     20.01.05  Ansgar Denner     last changed  19.06.09              *
***********************************************************************
      implicit none

      include "coli_params_cache.h"
      include "coli_common_cache.h"

      complex*16 fct(maxfct)
      integer i1,nfct,type

      if(cachelevel(cache).lt.type.or.ninit(cache).lt.type
     &    .or.usecache(cache).eq.0)return
      if(calc(type,ncall(type,cache),cache).eq.1)then
c storing result of function type
        do i1=1,nfct
          f(i1,type,pointer(type,ncall(type,cache),cache))=fct(i1)
        enddo
      endif
c      if (usecache(cache).lt.usecachesavei) write(*,*) 'usecache reset to',
c     &      usecachesavei
 
      cachelevel(cache) = cacheleveli(cache)

      return

      write(*,10)
      write(*,12) 'cachewrite: usecache = ',
     &    usecache(cache),usecachesave(cache),
     &    cachelevel(cache),cacheleveli(cache)
      write(*,10) 'cachewrite:  ninit = ',ninit
      write(*,10) 'cachewrite:   type = ',type
      write(*,10) 'cachewrite:  ncalc = ',ncalc(type,cache)
c      write(*,10) 'cachewrite: ncalc3 = ',ncalc3(type,cache)
c      write(*,10) 'cachewrite: ncalc2 = ',ncalc2(type,cache)
      write(*,10) 'cachewrite:  ncall = ',ncall(type,cache)
      write(*,10) 'cachewrite:     pt = ',
     &    pointer(type,ncall(type,cache),cache)
c      write(*,11) 'cachewrite:   name = ',name(type,cache)
      write(*,10) 'cachewrite:  grank = ',
     &    grank(type,ncalc(type,cache),cache)
      write(*,10) 'cachewrite: gswitch= ',
     &    gswitch(type,ncalc(type,cache),cache)
 10   format(A21,i4)
 11   format(A21,a11)
 12   format(A22,3i4)
      end



***********************************************************************
      subroutine countinit
***********************************************************************
*     initialize counts for different branches in C and D reduction   *
*---------------------------------------------------------------------*
*     23.03.05  Ansgar Denner     last changed  24.03.05              *
***********************************************************************
      implicit   none

      integer    Ccount(0:40)
      integer    Dcount(0:40)
      integer    i

      common /Ccount/ Ccount
      common /Dcount/ Dcount

      real*8     accbad1,accbad2,accbad3
      common /accbad/ accbad1,accbad2,accbad3

      accbad1  = 1d-4
      accbad2  = 1d-2
      accbad3  = 1d0
      do i=0,40
        Ccount(i) = 0
        Dcount(i) = 0
      end do 

      end 

***********************************************************************
      subroutine writecount(out)
***********************************************************************
*     output counts for different branches in C and D reduction       *
*---------------------------------------------------------------------*
*     23.03.05  Ansgar Denner     last changed  23.05.05              *
*                            cosmetic changes 01.06.06 Ansgar Denner  *
***********************************************************************
      implicit   none
      integer    out
      integer    Ccount(0:40)
      integer    Dcount(0:40)
      integer    Ccount1,Ccount2,Ccount3,Ccount4
      integer    Dcount1,Dcount2,Dcount3,Dcount4
      integer    i
      real*8     accbad1,accbad2,accbad3

      common /Ccount/ Ccount
      common /Dcount/ Dcount
      common /accbad/ accbad1,accbad2,accbad3

      Ccount1 = 0
      Ccount2 = 0
      Ccount3 = 0
      Ccount4 = 0
      Dcount1 = 0
      Dcount2 = 0
      Dcount3 = 0
      Dcount4 = 0
      do i=1,9
        Ccount1 = Ccount1 + Ccount(i)
        Ccount2 = Ccount2 + Ccount(i+10)
        Ccount3 = Ccount3 + Ccount(i+20)
        Ccount4 = Ccount4 + Ccount(i+30)
        Dcount1 = Dcount1 + Dcount(i)
        Dcount2 = Dcount2 + Dcount(i+10)
        Dcount3 = Dcount3 + Dcount(i+20)
        Dcount4 = Dcount4 + Dcount(i+30)
      end do 
      Ccount1 = Ccount1 - Ccount(6)
      Ccount2 = Ccount2 - Ccount(6+10)
      Ccount3 = Ccount3 - Ccount(6+20)
      Ccount4 = Ccount4 - Ccount(6+30)
      Dcount1 = Dcount1 - Dcount(6)
      Dcount2 = Dcount2 - Dcount(6+10)
      Dcount3 = Dcount3 - Dcount(6+20)
      Dcount4 = Dcount4 - Dcount(6+30)
      
      if (Dcount1.eq.0d0)  Dcount1=1d0
      if (Ccount1.eq.0d0)  Ccount1=1d0

      write(out,100)  
 100  format (/' Numbers for calls of different branches in C and D',
     &     ' reduction'/)
      
      write(out,300) (Ccount(i),dble(Ccount(i))/Ccount1*1d2,i=1,9),
     &         Ccount(0),dble(Ccount(0))/Ccount1*1d2
      write(out,400) (Dcount(i),dble(Dcount(i))/Dcount1*1d2,i=1,9),
     &         Dcount(0),dble(Dcount(0))/Dcount1*1d2

 300  format(' #calls Cpv = ',i20,' or ',F10.5,' %'/
     &       ' #calls Cg  = ',i20,' or ',F10.5,' %'/
     &       ' #calls Cgy = ',i20,' or ',F10.5,' %'/
     &       ' #calls Cy  = ',i20,' or ',F10.5,' %'/
     &       ' #calls Cgp = ',i20,' or ',F10.5,' %'/
c     &       ' #calls     = ',i20,' or ',F10.5,' %'/
     &       ' #calls     = ',i20,' or ',F10.5,' %'/
     &       ' #calls     = ',i20,' or ',F10.5,' %'/
     &       ' #calls Cpv = ',i20,' or ',F10.5,' %'/
     &       ' #calls Cy  = ',i20,' or ',F10.5,' %'/
     &       ' #calls C   = ',i20,' or ',F10.5,' %'/)
 400  format(' #calls Dpv = ',i20,' or ',F10.5,' %'/
     &       ' #calls Dg  = ',i20,' or ',F10.5,' %'/
     &       ' #calls Dgy = ',i20,' or ',F10.5,' %'/
     &       ' #calls Dy  = ',i20,' or ',F10.5,' %'/
     &       ' #calls Dgp = ',i20,' or ',F10.5,' %'/
     &       ' #calls     = ',i20,' or ',F10.5,' %'/
     &       ' #calls     = ',i20,' or ',F10.5,' %'/
c     &       ' #calls     = ',i20,' or ',F10.5,' %'/
c     &       ' #calls     = ',i20,' or ',F10.5,' %'/
     &       ' #calls Dpv = ',i20,' or ',F10.5,' %'/
     &       ' #calls Dy  = ',i20,' or ',F10.5,' %'/
     &       ' #calls D   = ',i20,' or ',F10.5,' %'/)

 101  format (/' Numbers for calls of different branches in C and D',
     &     ' reduction'/ ' accuracy worse than',G12.4)

      write(out,101) accbad1
      write(out,300) (Ccount(i),dble(Ccount(i))/Ccount1*1d2,i=11,19),
     &         Ccount2,dble(Ccount2)/Ccount1*1d2
      write(out,400) (Dcount(i),dble(Dcount(i))/Dcount1*1d2,i=11,19),
     &         Dcount2,dble(Dcount2)/Dcount1*1d2

      write(out,101) accbad2
      write(out,300) (Ccount(i),dble(Ccount(i))/Ccount1*1d2,i=21,29),
     &         Ccount3,dble(Ccount3)/Ccount1*1d2
      write(out,400) (Dcount(i),dble(Dcount(i))/Dcount1*1d2,i=21,29),
     &         Dcount3,dble(Dcount3)/Dcount1*1d2

      write(out,101) accbad3
      write(out,300) (Ccount(i),dble(Ccount(i))/Ccount1*1d2,i=31,39),
     &         Ccount4,dble(Ccount4)/Ccount1*1d2
      write(out,400) (Dcount(i),dble(Dcount(i))/Dcount1*1d2,i=31,39),
     &         Dcount4,dble(Dcount4)/Dcount1*1d2
      end 
