
c#define WRITERP
c#define WRITESD
c#define WRITESTD

************************************************************************
*                                                                      *
*     Regular scalar 4-point function                                  *
*                                                                      *
************************************************************************
*                                                                      *
*     last changed  15.06.11  Ansgar Denner                            *
*                                                                      *
************************************************************************
* Subroutines:                                                         *
* Functions:                                                           *
* D0regrp_coli,D0regrm_coli                                            *
* D0regsd_coli,                                                        *
************************************************************************
      function D0regrp_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  scalar 4-point function  for r13 real positive                      *
*  regular case  based on general result of                            *
*        A.Denner, U.Nierste and R.Scharf, Nucl. Phys. B367 (1991) 637 *
*  valid for real positive r13 and vicinity                            *
*                                                                      *
*                     m22                                              *
*       p12  ---------------------  q23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | m32                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       q14  ---------------------  q34                                *
*                     m42                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  29.12.08 Ansgar Denner       last changed 17.03.10 Ansgar Denner    *
************************************************************************
      implicit   none
      complex*16 D0regrp_coli
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42

      complex*16 l12,l13,l14,l23,l24,l34
      complex*16 r12,r13,r14,r23,r24,r34
      complex*16 r21,r31,r41,r32,r42,r43
      complex*16 a,b,c,d,det
      complex*16 x(2,4)
      complex*16 ch0,ch1,ch2,ch3,ch4,ch5,l1,l2,argl1
      complex*16 eta
      real*8     ir12,ir13,ir14,ir23,ir24,ir34
      real*8     ix(2,4),ipop(2)
      real*8     test0,test1,test2,test3,test4,test5
      real*8     test01,test23,test45
      real*8     u,v
      complex*16 cspcos_coli,cln_coli,eta2s_coli
      integer    i

      logical    flag2
      save       flag2
      data       flag2 /.true./
c      logical    errflag
c      save       errflag
c      data       errflag /.true./

#ifdef CHECK
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 elimminf2_coli
      complex*16 D0regrp_check
c     complex*16 cD0reg
      integer    j
      logical flag(0:1)
      data    flag /2*.true./
      save    flag  
#endif

      include "coli_params.h"
      include "coli_checkparams.h"

      integer error_code
      common /coli_error/ error_code
      save /coli_error/

c>      write(*,*) ' D0regrp_coli in ',
c>     &    p12,p23,p34,p14,p13,p24,m12,m22,m32,m42

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ms42 = elimminf2_coli(m42)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps34 = elimminf2_coli(p34)
        ps14 = elimminf2_coli(p14)
        ps24 = elimminf2_coli(p24)
        ps13 = elimminf2_coli(p13)

        if(ms32.eq.0d0.or.ms12.eq.cd0.or.ms22.eq.cd0.or.ms42.eq.cd0
     &      .or.dimag(p12).ne.0d0.or.dimag(p23).ne.0d0
     &      .or.dimag(p34).ne.0d0.or.dimag(p14).ne.0d0
     &      .or.dimag(p24).ne.0d0.or.dimag(p13).ne.0d0) then
          write(*,100)' D0regrp_coli called improperly:   (3.61)'
          write(*,111)' D0regrp_coli: p12 = ',p12
          write(*,111)' D0regrp_coli: p23 = ',p23
          write(*,111)' D0regrp_coli: p34 = ',p34
          write(*,111)' D0regrp_coli: p14 = ',p14
          write(*,111)' D0regrp_coli: p13 = ',p13
          write(*,111)' D0regrp_coli: p24 = ',p24
          write(*,111)' D0regrp_coli: m12 = ',m12
          write(*,111)' D0regrp_coli: m22 = ',m22
          write(*,111)' D0regrp_coli: m32 = ',m32
          write(*,111)' D0regrp_coli: m42 = ',m42
          write(*,*)' D0regrp_coli: test= ',
     &        ms32.eq.0d0,ms12.eq.cd0,ms22.eq.cd0,ms42.eq.cd0
     &        ,dimag(p12).ne.0d0,dimag(p23).ne.0d0
     &        ,dimag(p34).ne.0d0,dimag(p14).ne.0d0
     &        ,dimag(p24).ne.0d0,dimag(p13).ne.0d0
          stop
        end if
      end if
#endif

#ifdef CHECK
      if(flag(1))then
        write(*,100)' D0regrp_coli:   (3.61)',
c     &      '    not yet tested at all'
     &      '    not yet tested in physical process'
        write(*,111)' D0regrp_coli: p12 = ',p12
        write(*,111)' D0regrp_coli: p23 = ',p23
        write(*,111)' D0regrp_coli: p34 = ',p34
        write(*,111)' D0regrp_coli: p14 = ',p14
        write(*,111)' D0regrp_coli: p13 = ',p13
        write(*,111)' D0regrp_coli: p24 = ',p24
        write(*,111)' D0regrp_coli: m12 = ',m12
        write(*,111)' D0regrp_coli: m22 = ',m22
        write(*,111)' D0regrp_coli: m32 = ',m32
        write(*,111)' D0regrp_coli: m42 = ',m42
        flag(1)=.false.
      endif
#endif

      l12 = (m12+m22-p12)
      l13 = (m12+m32-p13)
      l14 = (m12+m42-p14)
      l23 = (m22+m32-p23)
      l24 = (m22+m42-p24)
      l34 = (m32+m42-p34)

      if(l12.ne.cd0)then
        r12 = l12/(2d0*m22)*(1d0+sqrt(1d0-4d0*m12*m22/l12**2))
      else
        r12 = dcmplx(0d0,1d0)*sqrt(m12/m22)
      endif
      r21 = r12*m22/m12
      if(l13.ne.cd0)then
        if(p13.eq.cd0)then
          r31=cd1
        elseif(abs(m32).le.abs(m12))then
          r31 = l13/(2d0*m12)*(1d0+sqrt(1d0-4d0*m12*m32/l13**2))
        else
          r31 = (2d0*m32)/(l13*(1d0+sqrt(1d0-4d0*m12*m32/l13**2)))
        endif 
      else
        r31 = dcmplx(0d0,1d0)*sqrt(m32/m12)
      endif
      r13 = r31*m12/m32

c          write(*,*)' D0regrp_coli: r13 1 = ',r31,r13

      if(l14.ne.cd0)then
        r14 = l14/(2d0*m42)*(1d0+sqrt(1d0-4d0*m12*m42/l14**2))
      else
        r14 = dcmplx(0d0,1d0)*sqrt(m12/m42)
      endif
      r41 = r14*m42/m12
      if(l23.ne.cd0)then
        r23 = l23/(2d0*m32)*(1d0+sqrt(1d0-4d0*m22*m32/l23**2))
      else
        r23 = dcmplx(0d0,1d0)*sqrt(m22/m32)
      endif
      r32 = r23*m32/m22
      if(l24.ne.cd0)then
        r24 = l24/(2d0*m42)*(1d0+sqrt(1d0-4d0*m22*m42/l24**2))
      else
        r24 = dcmplx(0d0,1d0)*sqrt(m22/m42)
      endif
      r42 = r24*m42/m22
      if(l34.ne.cd0)then
        r34 = l34/(2d0*m42)*(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
      else
        r34 = dcmplx(0d0,1d0)*sqrt(m32/m42)
      endif
      r43 = r34*m42/m32


c for testing
c      r31 = 1d0/r13
c      r13 = m12/m32/r13
      r42 = 1d0/r24
      r24 = m22/m42/r24

      if( dimag(r31).ne.0d0.and.
     &    (dimag(m22*r31*r31).gt.0d0.or.dimag(m42*r31*r31).gt.0d0.or.
     &    dimag(r31*l23).gt.0d0.or.dimag(r31*l34).gt.0d0.or.
     &    dimag(r31*r31*l24).gt.0d0)) then
        r31 = 1d0/r13
        r13 = m12/m32/r13
      endif

c          write(*,*)' D0regrp_coli: r13 2 = ',r31,r13


#ifdef CHECK
      if(conscheck)then
        if( dimag(r31).eq.cd0.and.dreal(r31).lt.0d0.or.
     &      dimag(m22*r31*r31).gt.0d0.or.dimag(m42*r31*r31).gt.0d0.or.
     &      dimag(r31*l23).gt.0d0.or.dimag(r31*l34).gt.0d0.or.
     &      dimag(r31*r31*l24).gt.0d0) then
          write(*,100)'  D0regrp_coli: analytic continuation doubtful'
          write(*,111)' D0regrp_coli: r13 = ',r13,r31
          write(*,111)' D0regrp_coli: p12 = ',p12
          write(*,111)' D0regrp_coli: p23 = ',p23
          write(*,111)' D0regrp_coli: p34 = ',p34
          write(*,111)' D0regrp_coli: p14 = ',p14
          write(*,111)' D0regrp_coli: p13 = ',p13
          write(*,111)' D0regrp_coli: p24 = ',p24
          write(*,111)' D0regrp_coli: m12 = ',m12
          write(*,111)' D0regrp_coli: m22 = ',m22
          write(*,111)' D0regrp_coli: m32 = ',m32
          write(*,111)' D0regrp_coli: m42 = ',m42
          write(*,*)' D0regrp_coli: test= ',
     &        dimag(r13).eq.cd0.and.dreal(r13).lt.0d0,
     &        dimag(m22*r31*r31).gt.0d0,dimag(m42*r31*r31).gt.0d0,
     &        dimag(r31*l23).gt.0d0,dimag(r31*l34).gt.0d0,
     &        dimag(r31*r31*l24).gt.0d0
c          stop

        endif
      endif
#endif      

c>      ir12 = sign(1d0,1d0-abs(r12*r21))
c>      ir13 = sign(1d0,1d0-abs(r13*r31))
c>      ir14 = sign(1d0,1d0-abs(r14*r41))
c>      ir23 = sign(1d0,1d0-abs(r23*r32))
c>      ir24 = sign(1d0,1d0-abs(r24*r42))
c>      ir34 = sign(1d0,1d0-abs(r34*r43))

      ir12 = sign(1d0,dreal((r12*r21-1d0)*r12*m12))
      ir13 = sign(1d0,dreal((r13*r31-1d0)*r13*m12))
      ir14 = sign(1d0,dreal((r14*r41-1d0)*r14*m12))
      ir23 = sign(1d0,dreal((r23*r32-1d0)*r23*m22))
      ir24 = sign(1d0,dreal((r24*r42-1d0)*r24*m22))
      ir34 = sign(1d0,dreal((r34*r43-1d0)*r34*m32))


      a   =  m42*(l34/r42-l23 + (l12-l14/r42)*r31)
      b   =  m12*m42*(1d0/r13-r31)*(1d0/r42-r24)+l12*l34-l14*l23
      c   =  m12*(l34*r24-l23 + (l12-l14*r24)/r13)
      d   =  l23-r31*l12 - (l34-r31*l14)*r24
      det =  sqrt(b*b-4d0*a*c)

      if(a.eq.cd0) then
        write(*,100)' D0regrp_coli: case not implemented'
        write(*,100)' a = 0 '
        write(*,111)' D0regrp: p12 = ',p12
        write(*,111)' D0regrp: p23 = ',p23
        write(*,111)' D0regrp: p34 = ',p34
        write(*,111)' D0regrp: p14 = ',p14
        write(*,111)' D0regrp: p24 = ',p24
        write(*,111)' D0regrp: p13 = ',p13
        write(*,111)' D0regrp: m12 = ',m12
        write(*,111)' D0regrp: m22 = ',m22
        write(*,111)' D0regrp: m32 = ',m32
        write(*,111)' D0regrp: m42 = ',m42
        write(*,*) 
        error_code = 1
      endif

      x(1,4) = (-b+sqrt(b*b-4d0*a*c))/(2d0*a)
      x(2,4) = (-b-sqrt(b*b-4d0*a*c))/(2d0*a)
      if(abs(x(1,4)).gt.abs(x(2,4))) then
        x(2,4) = c/(a*x(1,4))
      else
        x(1,4) = c/(a*x(2,4))
      end if

      ix(1,4) = -sign(1d0,dreal(d))
      ix(2,4) = +sign(1d0,dreal(d))
      ix(1,1) =  sign(1d0,ix(1,4)*dreal(r24)) ! needed for correct
      ix(2,1) =  sign(1d0,ix(2,4)*dreal(r24)) ! continuation

#ifdef WRITERP
      write(*,*) 'D0regrp_coli l ',l12,l13,l14
      write(*,*) 'D0regrp_coli l ',l23,l24,l34
      write(*,*) 'D0regrp_coli r ',r12,r13,r14
      write(*,*) 'D0regrp_coli r ',r21,r31,r41
      write(*,*) 'D0regrp_coli r ',r23,r24,r34
      write(*,*) 'D0regrp_coli r ',r32,r42,r43
      write(*,*) 'D0regrp_coli x ',x(1,4),x(2,4)
      write(*,*) 'D0regrp_coli ir ',ir12,ir13,ir14
      write(*,*) 'D0regrp_coli ir ',ir23,ir24,ir34
      write(*,*) 'D0regrp_coli ix ',ix(1,4),ix(2,4)
#endif

      x(1,1) = x(1,4)/r24
      x(2,1) = x(2,4)/r24
c      x(1,2) = x(1,4)/r24*r13
c      x(2,2) = x(2,4)/r24*r13
c      x(1,3) = x(1,4)*r13
c      x(2,3) = x(2,4)*r13
 
      D0regrp_coli = dcmplx(0d0)
      do i=1,2
        eta=eta2s_coli(-x(i,4),1d0/r24,-ix(i,4),-ir24,-ix(i,1))

        if(eta.ne.0d0)then
          ch0   = m42*(r31*x(i,4))**2+l34*r31*x(i,4)+m32
          test0 = abs(ch0)/max(abs(m42*(r31*x(i,4))**2),
     &        abs(l34*r31*x(i,4)),abs(m32))
          ch1   = m42*(x(i,4))**2+l14*x(i,4)+m12
          test1 = abs(ch1)/
     &        max(abs(m42*(x(i,4))**2),abs(l14*x(i,4)),abs(m12))
          test01=min(test0,test1)
          ch2   = m22*(r31*x(i,1))**2+l23*r31*x(i,1)+m32
          test2 = abs(ch2)/max(abs(m22*(r31*x(i,1))**2),
     &        abs(l23*r31*x(i,1)),abs(m32))
          ch3   = m22*(x(i,1))**2+l12*x(i,1)+m12
          test3 = abs(ch3)/
     &        max(abs(m22*(x(i,1))**2),abs(l12*x(i,1)),abs(m12))
          test23 = min(test2,test3)

          ch4   = (m42*r31*x(i,4)*(1d0/r42-r24)+(l23-r24*l34))*r31
          test4 = abs(ch4)/max(abs(m42*r31*r31*x(i,4)/r42),
     &        abs(m42*r31*r31*x(i,4)*r24),abs(l23*r31),
     &        abs(r31*r24*l34))
          ch5   = m42*x(i,4)*(1d0/r42-r24)+(l12-r24*l14)
          test5 = abs(ch5)/max(abs(m42*x(i,4)/r42),
     &        abs(m42*x(i,4)*r24),abs(l12),abs(r24*l14))
          test45 = min(test2,test3)

c>          write(*,*) 'D0regrp: chi/chj:', ch2/ch3,ch0/ch1,ch4/ch5

          if (test23.gt.test01.and.test23.gt.test45) then
            argl1 = ch2/ch3
          elseif(test01.gt.test45)then
            argl1 = ch0/ch1      
          else
            argl1 = ch4/ch5      
          end if

          if(abs(dimag(argl1)).lt.1d1*impacc*abs(argl1)) then
            
            if(abs(dimag(r24)).gt.impacc*abs(r24))then
              v=dimag(x(i,4))/dimag(r24)
              u=dimag(x(i,4)/r24)/dimag(1d0/r24)
              ipop(i) = -m12-m22*v*v-m42*u*u-l12*v-l14*u-l24*u*v
c>                ipop(i) = m32+m22*(r31*v)**2+m42*(r31*u)**2
c>     &              +l23*r31*v+l34*r31*u+l24*r31*r31*u*v
c>     &              -m12-m22*v*v-m42*u*u-l12*v-l14*u-l24*u*v
            else                ! imaginary part results only from x4(i)
              ipop(i) =m42*(r24-1d0/r42)*ix(i,4)*d*r31
            endif
          else
            ipop(i) = 0D0
          endif

          l1 = cln_coli(argl1,ipop(i))

c          write(*,*) 'D0regrp_coli l1=',l1,argl1,ipop(i),r24

#ifdef CHECK
          if(conscheck)then
            if (test23.gt.test01.and.test23.gt.test45) then
              if(abs(dimag(ch2/ch3)).gt.impacc*abs(dreal(ch2/ch3)))
     &            goto 90
            elseif(test01.gt.test45)then
              if(abs(dimag(ch0/ch1)).gt.impacc*abs(dreal(ch0/ch1)))
     &            goto 90
            else
              if(abs(dimag(ch4/ch5)).gt.impacc*abs(dreal(ch4/ch5)))
     &            goto 90
            endif
            if(dimag(r24).ne.0d0) then
              v=dimag(x(i,4))/dimag(r24)
              u=dimag(x(i,4)/r24)/dimag(1d0/r24)
              l2=cln_coli(m32+l23*r31*v+l34*r31*u+m22*(r31*v)**2
     &            +m42*(r31*u)**2+l24*r31*r31*u*v,-1d0)
     &            -cln_coli(m12+l12*v+l14*u+m22*v*v
     &            +m42*u*u+l24*u*v,-1d0)
              
#ifdef WRITERP
              write(*,*) 'D0regrp_coli comp1 l2= ',l2,l1,l2/l1
#endif              

            else
c>              if(test45.gt.test23.and.test45.gt.test01)then
              l2=cln_coli(ch4,dreal(r31*r31*m42*(1d0/r42-r24)*ix(i,4)))
     &            -cln_coli(ch5,dreal(m42*(1d0/r42-r24)*ix(i,4)))
c>              elseif(test23.gt.test01)then
c>                l2=cln_coli(ch4,dreal(m42*(1d0/r42-r24)*ix(i,4))
c>     &              -cln_coli(ch5,dreal(m42*(1d0/r42-r24)*ix(i,4))
c>              else
c>                l2=cln_coli(ch4,dreal(m42*(1d0/r42-r24)*ix(i,4))
c>     &              -cln_coli(ch5,dreal(m42*(1d0/r42-r24)*ix(i,4))
c>              endif

#ifdef WRITERP
              write(*,*) 'D0regrp_coli comp2 l2= ',l2,l1,l2/l1
#endif

            endif 
            
c              write(*,*) 'D0regrp_coli l2= ',l2,l1,l2/l1

            if(abs(l2/l1-1d0).gt.testacc) then
              if (flag2) then
                write(*,*) 
                write(*,*) 'D0regrp_coli: eta terms possibly wrong'
                flag2 = .false.
              end if
              write(*,*) 'D0regrp_coli r24',r24
              write(*,*) 
     &            'D0regrp_coli chi ',ch0,ch1,ch2,ch3,ch4,ch5
              write(*,*) 'l1 = ',l1
              write(*,*) 'l2 = ',l2
              write(*,*) 'diff   = ',abs((l2-l1)/l1)

              write(errout,*) 
              write(errout,*) 'D0regrp_coli: eta terms possibly wrong'
              write(errout,*) 
     &            'D0regrp_coli args ',p12,p23,p34,p14,p13,p24,
     &            m12,m22,m32,m42
              write(errout,*) 
     &            'D0regrp_coli chi ',ch0,ch1,ch2,ch3
              write(errout,*) 'l1 = ',l1
              write(errout,*) 'l2 = ',l2
              write(errout,*) 'diff   = ',abs((l2-l1)/l1)
            endif 
 90         continue
          endif
#endif


          if(eta.ne.0d0)then
            D0regrp_coli = D0regrp_coli + (2*i-3) * (
     &          + eta*(l1-cln_coli(m32/m12,dreal(m32-m12))) ) 
          endif
            
#ifdef WRITERP
          write(*,*) 'D0regrp_coli eta i ',
     &         eta*(l1-cln_coli(m32/m12,dreal(m32-m12)))
#endif
          
        endif
        
        D0regrp_coli = D0regrp_coli + (2*i-3) * (
     &      - cspcos_coli(-x(i,4),r43*r31,-ix(i,4),ir34)
     &      - cspcos_coli(-x(i,4),r31/r34,-ix(i,4),-ir34)
     &      + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14)
     &      + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14)
     &      - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12)
     &      - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12)
     &      + cspcos_coli(-x(i,1),r31*r23,-ix(i,1),ir23)
     &      + cspcos_coli(-x(i,1),r31/r32,-ix(i,1),-ir23))
        
#ifdef WRITERP
c>        write(*,*) 'D0regrp_coli arg1 i ',
c>     &      -x(i,4),r43*r31,-ix(i,4),ir34
c>        write(*,*) 'D0regrp_coli arg2 i ',
c>     &      -x(i,4),r31/r34,-ix(i,4),-ir34
c>        write(*,*) 'D0regrp_coli arg3 i ',
c>     &      -x(i,4),r41,-ix(i,4),ir14
c>        write(*,*) 'D0regrp_coli arg4 i ',
c>     &      -x(i,4),1d0/r14,-ix(i,4),-ir14
c>        write(*,*) 'D0regrp_coli arg5 i ',
c>     &      -x(i,1),r21,-ix(i,1),ir12
c>        write(*,*) 'D0regrp_coli arg6 i ',
c>     &      -x(i,1),1d0/r12,-ix(i,1),-ir12
c>        write(*,*) 'D0regrp_coli arg7 i ',
c>     &      -x(i,1),r31*r23,-ix(i,1),ir23
c>        write(*,*) 'D0regrp_coli arg8 i ',
c>     &      -x(i,1),r31/r32,-ix(i,1),-ir23
        
        write(*,*) 'D0regrp_coli contr i ',
     &      - cspcos_coli(-x(i,4),r43*r31,-ix(i,4),ir34),
     &      - cspcos_coli(-x(i,4),r31/r34,-ix(i,4),-ir34),
     &      + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14),
     &      + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14),
     &      - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12),
     &      - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12),
     &      + cspcos_coli(-x(i,1),r31*r23,-ix(i,1),ir23),
     &      + cspcos_coli(-x(i,1),r31/r32,-ix(i,1),-ir23)
        write(*,*) 'D0regrp_coli sum i ',
     &      - cspcos_coli(-x(i,4),r43*r31,-ix(i,4),ir34)
     &      - cspcos_coli(-x(i,4),r31/r34,-ix(i,4),-ir34)
     &      + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14)
     &      + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14)
     &      - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12)
     &      - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12)
     &      + cspcos_coli(-x(i,1),r31*r23,-ix(i,1),ir23)
     &      + cspcos_coli(-x(i,1),r31/r32,-ix(i,1),-ir23)
#endif
        
      enddo
      
#ifdef WRITERP
      write(*,*) 'D0regrp_coli sum ',D0regrp_coli,det
#endif
      
      D0regrp_coli = D0regrp_coli/det
      
#ifdef WRITERP
      write(*,*) 'D0regrp_coli=   ',D0regrp_coli
#endif

#ifdef CHECK

      if (conscheck.and.(
     &    dimag(x(1,4)).eq.0d0.or.dimag(x(2,4)).eq.0d0)) then

        do j=0,0
          if(j.eq.0) then
            ix(1,4) = -sign(1d0,dreal(d))
            ix(2,4) = +sign(1d0,dreal(d))
          elseif(j.eq.1) then
            ix(1,4) = +sign(1d0,dreal(d))
            ix(2,4) = -sign(1d0,dreal(d))
          elseif(j.eq.2) then
            ix(1,4) = +sign(1d0,dreal(d))
            ix(2,4) = +sign(1d0,dreal(d))
          elseif(j.eq.3) then
            ix(1,4) = -sign(1d0,dreal(d))
            ix(2,4) = -sign(1d0,dreal(d))
          endif
          ix(1,1) =  sign(1d0,ix(1,4)*dreal(r24)) ! needed for correct
          ix(2,1) =  sign(1d0,ix(2,4)*dreal(r24)) ! continuation


#ifdef WRITERP
      write(*,*) 'D0regrp_check x ',x(1,4),x(2,4)
      write(*,*) 'D0regrp_check ix ',ix(1,4),ix(2,4)
#endif

          D0regrp_check = dcmplx(0d0)
          do i=1,2
            eta=eta2s_coli(-x(i,4),1d0/r24,-ix(i,4),-ir24,-ix(i,1))
            
            if(eta.ne.0d0)then
              ch0   = m42*(r31*x(i,4))**2+l34*r31*x(i,4)+m32
              test0 = abs(ch0)/max(abs(m42*(r31*x(i,4))**2),
     &            abs(l34*r31*x(i,4)),abs(m32))
              ch1   = m42*(x(i,4))**2+l14*x(i,4)+m12
              test1 = abs(ch1)/
     &            max(abs(m42*(x(i,4))**2),abs(l14*x(i,4)),abs(m12))
              test01=min(test0,test1)
              ch2   = m22*(r31*x(i,1))**2+l23*r31*x(i,1)+m32
              test2 = abs(ch2)/max(abs(m22*(r31*x(i,1))**2),
     &            abs(l23*r31*x(i,1)),abs(m32))
              ch3   = m22*(x(i,1))**2+l12*x(i,1)+m12
              test3 = abs(ch3)/
     &            max(abs(m22*(x(i,1))**2),abs(l12*x(i,1)),abs(m12))
              test23 = min(test2,test3)


              if (test23.gt.test01.and.test23.gt.test45) then
                argl1 = ch2/ch3
              elseif(test01.gt.test45)then
                argl1 = ch0/ch1      
              else
                argl1 = ch4/ch5      
              end if
              
              if(abs(dimag(argl1)).lt.1d1*impacc*abs(argl1)) then
                
                if(abs(dimag(r24)).gt.impacc*abs(r24))then
                  v=dimag(x(i,4))/dimag(r24)
                  u=dimag(x(i,4)/r24)/dimag(1d0/r24)
                  ipop(i) = -m12-m22*v*v-m42*u*u-l12*v-l14*u-l24*u*v
c>                ipop(i) = m32+m22*(r31*v)**2+m42*(r31*u)**2
c>     &              +l23*r31*v+l34*r31*u+l24*r31*r31*u*v
c>     &              -m12-m22*v*v-m42*u*u-l12*v-l14*u-l24*u*v
                else            ! imaginary part results only from x4(i)
                  ipop(i) =m42*(r24-1d0/r42)*ix(i,4)*d*r31
                endif
              else
                ipop(i) = 0D0
              endif

              l1 = cln_coli(argl1,ipop(i))

c>              write(*,*) 'D0regrp_check l1=',l1,argl1,ipop(i),r24


              if(conscheck)then
                if(dimag(r24).ne.0d0) then
                  v=dimag(x(i,4))/dimag(r24)
                  u=dimag(x(i,4)/r24)/dimag(1d0/r24)
                  l2=cln_coli(m32+l23*r31*v+l34*r31*u+m22*(r31*v)**2
     &                +m42*(r31*u)**2+l24*r31*r31*u*v,-1d0)
     &                -cln_coli(m12+l12*v+l14*u+m22*v*v
     &                +m42*u*u+l24*u*v,-1d0)
                  
#ifdef WRITERP
                  write(*,*) 'D0regrp_check comp1 l2= ',l2,l1,l2/l1
#endif
                else
                  l2=cln_coli(ch4,
     &                dreal(r31*r31*m42*(1d0/r42-r24)*ix(i,4)))
     &                -cln_coli(ch5,dreal(m42*(1d0/r42-r24)*ix(i,4)))
                  
#ifdef WRITERP
                  write(*,*) 'D0regrp_check comp2 l2= ',l2,l1,l2/l1
#endif
                endif 
                
c     write(*,*) 'D0regrp_check l2= ',l2,l1,l2/l1
                
                if(abs(l2/l1-1d0).gt.testacc) then
                  if (flag2) then
                    write(*,*) 
                    write(*,*) 'D0regrp_check: eta terms possibly wrong'
                    flag2 = .false.
                  end if
                  write(errout,*) 
                  write(errout,*) 
     &                'D0regrp_check: eta terms possibly wrong'
                  write(errout,*) 
     &                'D0regrp_check args ',p12,p23,p34,p14,p13,p24,
     &                m12,m22,m32,m42
                  write(errout,*) 
     &                'D0regrp_check chi ',ch0,ch1,ch2,ch3
                  write(errout,*) 'l1 = ',l1
                  write(errout,*) 'l2 = ',l2
                  write(errout,*) 'diff   = ',abs((l2-l1)/l1)
                endif 
              endif
              
              
              if(eta.ne.0d0)then
                D0regrp_check = D0regrp_check + (2*i-3) * (
     &              +  eta*(l1-cln_coli(m32/m12,dreal(m32-m12))) )
              endif 
              
#ifdef WRITERP
              write(*,*) 'D0regrp_check eta i ',
     &             eta*(l1-cln_coli(m32/m12,dreal(m32-m12)))
#endif
              
            endif
            
            D0regrp_check = D0regrp_check + (2*i-3) * (
     &          - cspcos_coli(-x(i,4),r43*r31,-ix(i,4),ir34)
     &          - cspcos_coli(-x(i,4),r31/r34,-ix(i,4),-ir34)
     &          + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14)
     &          + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14)
     &          - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12)
     &          - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12)
     &          + cspcos_coli(-x(i,1),r31*r23,-ix(i,1),ir23)
     &          + cspcos_coli(-x(i,1),r31/r32,-ix(i,1),-ir23))
            
#ifdef WRITERP
            write(*,*) 'D0regrp_check arg1 i ',
     &          -x(i,4),r43*r31,-ix(i,4),ir34
            write(*,*) 'D0regrp_check arg2 i ',
     &          -x(i,4),r31/r34,-ix(i,4),-ir34
            write(*,*) 'D0regrp_check arg3 i ',
     &          -x(i,4),r41,-ix(i,4),ir14
            write(*,*) 'D0regrp_check arg4 i ',
     &          -x(i,4),1d0/r14,-ix(i,4),-ir14
            write(*,*) 'D0regrp_check arg5 i ',
     &          -x(i,1),r21,-ix(i,1),ir12
            write(*,*) 'D0regrp_check arg6 i ',
     &          -x(i,1),1d0/r12,-ix(i,1),-ir12
            write(*,*) 'D0regrp_check arg7 i ',
     &          -x(i,1),r31*r23,-ix(i,1),ir23
            write(*,*) 'D0regrp_check arg8 i ',
     &          -x(i,1),r31/r32,-ix(i,1),-ir23
            
            write(*,*) 'D0regrp_check contr i ',
     &          - cspcos_coli(-x(i,4),r43*r31,-ix(i,4),ir34),
     &          - cspcos_coli(-x(i,4),r31/r34,-ix(i,4),-ir34),
     &          + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14),
     &          + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14),
     &          - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12),
     &          - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12),
     &          + cspcos_coli(-x(i,1),r31*r23,-ix(i,1),ir23),
     &          + cspcos_coli(-x(i,1),r31/r32,-ix(i,1),-ir23)
            write(*,*) 'D0regrp_check sum i ',
     &          - cspcos_coli(-x(i,4),r43*r31,-ix(i,4),ir34)
     &          - cspcos_coli(-x(i,4),r31/r34,-ix(i,4),-ir34)
     &          + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14)
     &          + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14)
     &          - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12)
     &          - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12)
     &          + cspcos_coli(-x(i,1),r31*r23,-ix(i,1),ir23)
     &          + cspcos_coli(-x(i,1),r31/r32,-ix(i,1),-ir23)
#endif
            
          enddo
          
#ifdef WRITERP
          write(*,*) 'D0regrp_check sum ',D0regrp_check,det
#endif
          
          D0regrp_check = D0regrp_check/det
          
#ifdef WRITERP
          write(*,*) 'D0regrp_check =  ',D0regrp_check
#endif

          if(cdabs(D0regrp_coli/D0regrp_check-1d0).gt.testacc) then
            write(testout,*) ' D0regrp_coli inconsistent ',j
            write(testout,111) ' D0regrp_coli: p12 = ',p12
            write(testout,111) ' D0regrp_coli: p23 = ',p23
            write(testout,111) ' D0regrp_coli: p34 = ',p34
            write(testout,111) ' D0regrp_coli: p14 = ',p14
            write(testout,111) ' D0regrp_coli: p13 = ',p13
            write(testout,111) ' D0regrp_coli: p24 = ',p24
            write(testout,111) ' D0regrp_coli: m12 = ',m12
            write(testout,111) ' D0regrp_coli: m22 = ',m22
            write(testout,111) ' D0regrp_coli: m32 = ',m32
            write(testout,111) ' D0regrp_coli: m42 = ',m42
            write(testout,111) ' D0regrp_coli  = ',D0regrp_coli
            write(testout,111) ' D0regrp_check = ',D0regrp_check
            write(testout,101) ' diff        = ',
     &          cdabs(D0regrp_coli/D0regrp_check-1d0)
            
            write(*,*)   ' D0regrp_coli inconsistent ',j
            write(*,111) ' D0regrp_coli: p12 = ',p12
            write(*,111) ' D0regrp_coli: p23 = ',p23
            write(*,111) ' D0regrp_coli: p34 = ',p34
            write(*,111) ' D0regrp_coli: p14 = ',p14
            write(*,111) ' D0regrp_coli: p13 = ',p13
            write(*,111) ' D0regrp_coli: p24 = ',p24
            write(*,111) ' D0regrp_coli: m12 = ',m12
            write(*,111) ' D0regrp_coli: m22 = ',m22
            write(*,111) ' D0regrp_coli: m32 = ',m32
            write(*,111) ' D0regrp_coli: m42 = ',m42
            write(*,111) ' D0regrp_coli  = ',D0regrp_coli
            write(*,111) ' D0regrp_check = ',D0regrp_check
            write(*,101) ' diff        = ',
     &          cdabs(D0regrp_coli/D0regrp_check-1d0)
c            stop
          endif
          
        enddo
      endif
#endif

      end

************************************************************************
      function D0comb_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  general scalar 4-point function                                     *
*  regular case based on propagator identity                           *
*  reduces result to 2 4-point function with one vanishing pij         *
*                                                                      *
*                     m22                                              *
*       p12  ---------------------  p23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | m32                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       p14  ---------------------  p34                                *
*                     m42                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  09.03.10 Ansgar Denner       last changed 09.03.10 Ansgar Denner    *
************************************************************************
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 q2(0:3,0:3),m2(0:3)
      complex*16 D0comb_coli,D0regrp_coli

      real*8     lambda,al
      complex*16 mm2,qq2,D01,D02
      integer    i,j,k,l,i1,i2,i3,i4
c      complex*16 cd0
c      data       cd0 /(0d0,0d0)/
 
#ifdef CHECK
c      complex*16 D0reg_check,D0mass_std
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 elimminf2_coli
c      logical flag
c      data    flag /.true./
c      save    flag  
#endif

c      complex*16 cln_coli,cspenc_coli

      include "coli_params.h"
      include "coli_checkparams.h"

      integer error_code
      common /coli_error/ error_code
      save /coli_error/

c>      write(*,*) ' D0comb_coli in ',p12,p23,p34,p14,p13,p24,
c>     &    m12,m22,m32,m42

 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 100  format(((a)))
 101  format(a22,g25.17)
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ms42 = elimminf2_coli(m42)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps34 = elimminf2_coli(p34)
        ps14 = elimminf2_coli(p14)
        ps24 = elimminf2_coli(p24)
        ps13 = elimminf2_coli(p13)

        if(ms32.eq.0d0.or.ms12.eq.cd0.or.ms22.eq.cd0.or.ms42.eq.cd0
     &      .or.dimag(p12).ne.0d0.or.dimag(p23).ne.0d0
     &      .or.dimag(p34).ne.0d0.or.dimag(p14).ne.0d0
     &      .or.dimag(p24).ne.0d0.or.dimag(p13).ne.0d0) then
          write(*,100)' D0comb_coli called improperly:'
          write(*,111)' D0comb_coli: p12 = ',p12
          write(*,111)' D0comb_coli: p23 = ',p23
          write(*,111)' D0comb_coli: p34 = ',p34
          write(*,111)' D0comb_coli: p14 = ',p14
          write(*,111)' D0comb_coli: p13 = ',p13
          write(*,111)' D0comb_coli: p24 = ',p24
          write(*,111)' D0comb_coli: m12 = ',m12
          write(*,111)' D0comb_coli: m22 = ',m22
          write(*,111)' D0comb_coli: m32 = ',m32
          write(*,111)' D0comb_coli: m42 = ',m42
          stop
        end if
      end if
#endif

      q2(0,1)=p12
      q2(1,0)=p12
      q2(0,2)=p13
      q2(2,0)=p13
      q2(0,3)=p14
      q2(3,0)=p14
      q2(2,1)=p23
      q2(1,2)=p23
      q2(3,1)=p24
      q2(1,3)=p24
      q2(2,3)=p34
      q2(3,2)=p34
      m2(0)=m12
      m2(1)=m22
      m2(2)=m32
      m2(3)=m42

c>      maxp2=dreal(q2(1,2))
c>      i1=1
c>      i2=2
c>      do i=1,3
c>      do j=2,4
c>        if(q2(i,j).eq.cd0) then 
c>          i1=i
c>          i2=j
c>          goto 10
c>        elseif(dreal(q2(i,j)).gt.maxp2)then
c>          i1=i
c>          i2=j
c>          maxp2=dreal(q2(i,j))
c>        endif
c>      enddo
c>      enddo
c>        
c>      i3=mod(max(i2,i1)+1,4)+1
c>      maxp2=dreal(q2(i1,i3))          
c>      do k=1,4
c>        if(k.ne.i1.and.k.ne.i2) then
c>          if(dreal(q2(i1,k)).gt.maxp2)then
c>            i3=k
c>            maxp2=dreal(q2(i1,k))
c>          endif
c>          if(dreal(q2(i2,k)).gt.maxp2)then
c>            i3=k
c>            maxp2=dreal(q2(i2,k))
c>          endif
c>        endif
c>      enddo
c>
c>      if(dreal(q2(i2,i3)).gt.dreal(q2(i1,i3)) then
c>        k=i1
c>        i1=i2
c>        i2=k
c>      endif

      do i=0,3
      do j=0,3
      if(j.ne.i)then
        if(q2(i,j).eq.0d0)then
          i1=i
          i3=j
          if(mod(i3-i1,2).eq.0)then
            i2=mod(i1+1,4)
            i4=mod(i1+3,4)   
          elseif(mod(i3-i1,4).eq.1)then
            i2=mod(i3+1,4)
            i4=mod(i3+2,4)   
          else
            i2=mod(i1+1,4)
            i4=mod(i1+2,4)   
          endif    

c>          write(*,*) 'D0comb ii = ',i1,i2,i3,i4
c>          write(*,*) 'D0comb q2 = ',q2(i1,i2),q2(i2,i3),q2(i3,i4),
c>     &        q2(i4,i1),
c>     &        q2(i1,i3),q2(i2,i4),m2(i1),m2(i2),m2(i3),m2(i4)
c>          
          D0comb_coli=D0regrp_coli(q2(i1,i2),q2(i2,i3),q2(i3,i4),
     &        q2(i4,i1),
     &        q2(i1,i3),q2(i2,i4),m2(i1),m2(i2),m2(i3),m2(i4))

c>          write(*,*) 'D0comb p2=0 = ',D0comb_coli
c>
          return

        endif
      endif
      enddo
      enddo

      do i=0,3
      do j=0,3
      if(j.ne.i)then
      do k=0,3
      if(k.ne.i.and.k.ne.j)then
        if(dreal(q2(i,j)).ge.0d0.and.dreal(q2(i,k)).ge.0d0)then
          lambda=(q2(i,j)-q2(i,k))**2-2d0*q2(j,k)*(q2(i,j)+q2(i,k))
     &      +q2(j,k)**2
          if(lambda.ge.0d0)then
            i1=i
            i2=j
            i3=k
            if(dreal(q2(j,k)).gt.0d0)then
              if(dreal(q2(j,k)).gt.dreal(q2(i,k)).and.
     &            dreal(q2(j,k)).gt.dreal(q2(i,j)))then
                i1=k
                i3=i
              elseif(dreal(q2(i,k)).gt.dreal(q2(i,j)))then
                i2=k
                i3=j
              endif
            endif
            if(q2(i2,i3).eq.cd0) then
              al=0d0
            elseif(q2(i1,i3).eq.cd0) then
              al=1d0
            elseif(q2(i1,i2).eq.cd0) then
              al=q2(i2,i3)/(q2(i2,i3)-q2(i1,i3))
            elseif(dreal(q2(i1,i2)-q2(i1,i3)+q2(i2,i3)).gt.0d0)then
              al=(q2(i1,i2)-q2(i1,i3)+q2(i2,i3)+sqrt(lambda))
     &            /(2d0*q2(i1,i2))

c>              write(*,*) 'D0comb q2 ',q2(i1,i2),q2(i1,i3),q2(i2,i3)
c>              write(*,*) 'D0comb al '
c>     &            ,lambda,al,
c>     &            2d0*q2(i2,i3)
c>     &            /(q2(i1,i2)-q2(i1,i3)+q2(i2,i3)-sqrt(lambda)),
c>     &         al*al*q2(i1,i2)+al*(q2(i1,i3)-q2(i1,i2)-q2(i2,i3))
c>     &            +q2(i2,i3)
               
            else
              al=2d0*q2(i2,i3)
     &            /(q2(i1,i2)-q2(i1,i3)+q2(i2,i3)-sqrt(lambda))

c>              write(*,*) 'D0comb q2 ',q2(i1,i2),q2(i1,i3),q2(i2,i3)
c>              write(*,*) 'D0comb al ',lambda,al,
c>     &            (q2(i1,i2)-q2(i1,i3)+q2(i2,i3)+sqrt(lambda))
c>     &            /(2d0*q2(i1,i2)),
c>     &         al*al*q2(i1,i2)+al*(q2(i1,i3)-q2(i1,i2)-q2(i2,i3))
c>     &            +q2(i2,i3)

            endif
            if(al.lt.0d0.or.al.gt.1d0)then
              write(*,*) 'D0comb_coli: inconsistent alpha1 =',al,lambda
              write(*,*) 'D0comb_coli: ijk ',i1,i2,i3
              write(*,*) 'D0comb_coli: qijk ',q2(i1,i2),q2(i1,i3),
     &            q2(i2,i3)
              write(*,111)' D0reg_coli: p12 = ',p12
              write(*,111)' D0reg_coli: p23 = ',p23
              write(*,111)' D0reg_coli: p34 = ',p34
              write(*,111)' D0reg_coli: p14 = ',p14
              write(*,111)' D0reg_coli: p13 = ',p13
              write(*,111)' D0reg_coli: p24 = ',p24
              write(*,111)' D0reg_coli: m12 = ',m12
              write(*,111)' D0reg_coli: m22 = ',m22
              write(*,111)' D0reg_coli: m32 = ',m32
              write(*,111)' D0reg_coli: m42 = ',m42
c              stop
            endif
            do l=0,3
              if(l.ne.i1.and.l.ne.i2.and.l.ne.i3) i4=l
            enddo 
 
c            write(*,*) 'D0comb al = ',al

            mm2=al*(m2(i1)-q2(i1,i3)) + (1d0-al)*(m2(i2)-q2(i2,i3)) 
            qq2=al*(q2(i1,i4)-q2(i1,i3)) 
     &          + (1d0-al)*(q2(i2,i4)-q2(i2,i3))

c>            write(*,*) 'D0comb ii 1',i1,i2,i3,i4
c>            write(*,*) 'D0comb arg D1',q2(i3,i2), 
c>     &          q2(i2,i1)*al*al,q2(i1,i4),
c>     &         q2(i4,i3),
c>     &          cd0,q2(i2,i4),m2(i3),m2(i2),mm2,m2(i4)
c>            write(*,*) 'D0comb arg D2',q2(i3,i1),
c>     &          (1d0-al)*(1d0-al)*q2(i1,i2),q2(i2,i4),q2(i4,i3),
c>     &          cd0,q2(i1,i4),m2(i3),m2(i1),mm2,m2(i4)


C D01 may be undefined for al=0!!!!!!!!!!!!!
            D01= D0regrp_coli(q2(i3,i2),q2(i2,i1)*al*al,qq2,
     &         q2(i4,i3),
     &          cd0,q2(i2,i4),m2(i3),m2(i2),mm2,m2(i4))

            D02= D0regrp_coli(q2(i3,i1),
     &          (1d0-al)*(1d0-al)*q2(i1,i2),qq2,q2(i4,i3),
     &          cd0,q2(i1,i4),m2(i3),m2(i1),mm2,m2(i4))

            D0comb_coli=al*D01+(1-al)*D02
                
c>            write(*,*) 'D0comb a1 = ',D0comb_coli,D01,D02            

            return
          else
              write(*,*) 'D0comb_coli: case 1, lambda<0 =',lambda
              write(*,*) 'D0comb_coli: ijk ',i1,i2,k
              write(*,*) 'D0comb_coli: qijk ',q2(i1,i2),q2(i1,k),
     &            q2(i2,k)
              write(*,111)' D0reg_coli: p12 = ',p12
              write(*,111)' D0reg_coli: p23 = ',p23
              write(*,111)' D0reg_coli: p34 = ',p34
              write(*,111)' D0reg_coli: p14 = ',p14
              write(*,111)' D0reg_coli: p13 = ',p13
              write(*,111)' D0reg_coli: p24 = ',p24
              write(*,111)' D0reg_coli: m12 = ',m12
              write(*,111)' D0reg_coli: m22 = ',m22
              write(*,111)' D0reg_coli: m32 = ',m32
              write(*,111)' D0reg_coli: m42 = ',m42
              error_code = 1
          endif
        elseif(dreal(q2(i,j)).lt.0d0.and.dreal(q2(i,k)).lt.0d0
     &        .and.dreal(q2(j,k)).gt.0d0)then
          lambda=(q2(i,j)-q2(i,k))**2-2d0*q2(j,k)*(q2(i,j)+q2(i,k))
     &      +q2(j,k)**2
          if(lambda.ge.0d0)then
            i1=i
            i2=j
            i3=k
            if(dreal(q2(i1,i2)-q2(i1,i3)+q2(i2,i3)).lt.0d0)then
              al=(q2(i1,i2)-q2(i1,i3)+q2(i2,i3)-sqrt(lambda))
     &            /(2d0*q2(i1,i2))

              write(*,*) 'D0recomb q2 ',q2(i1,i2),q2(i1,i3),q2(i2,i3)
              write(*,*) 'D0recomb al ',q2(i1,i2)-q2(i1,i3)+q2(i2,i3)
     &            ,lambda,al,
     &            2d0*q2(i2,i3)
     &            /(q2(i1,i2)-q2(i1,i3)+q2(i2,i3)+sqrt(lambda))
               
            else
              al=2d0*q2(i2,i3)
     &            /(q2(i1,i2)-q2(i1,i3)+q2(i2,i3)+sqrt(lambda))
            endif
            if(al.lt.0d0.or.al.gt.1d0)then
              write(*,*) 'D0comb_coli: inconsistent alpha2 =',al,lambda
              write(*,*) 'D0comb_coli: ijk ',i1,i2,k
              write(*,*) 'D0comb_coli: qijk ',q2(i1,i2),q2(i1,k),
     &            q2(i2,k)
              write(*,111)' D0reg_coli: p12 = ',p12
              write(*,111)' D0reg_coli: p23 = ',p23
              write(*,111)' D0reg_coli: p34 = ',p34
              write(*,111)' D0reg_coli: p14 = ',p14
              write(*,111)' D0reg_coli: p13 = ',p13
              write(*,111)' D0reg_coli: p24 = ',p24
              write(*,111)' D0reg_coli: m12 = ',m12
              write(*,111)' D0reg_coli: m22 = ',m22
              write(*,111)' D0reg_coli: m32 = ',m32
              write(*,111)' D0reg_coli: m42 = ',m42
              error_code = 1
            endif
            do l=0,3
              if(l.ne.i1.and.l.ne.i2.and.l.ne.i3) i4=l
            enddo 

c>            write(*,*) 'D0comb al = ',al
c>            write(*,*) 'D0comb ii = ',i1,i2,i3,i4

            mm2=al*(m2(i1)-q2(i1,i3)) + (1d0-al)*(m2(i2)-q2(i2,i3)) 
            qq2=al*(q2(i1,i4)-q2(i1,i3)) 
     &          + (1d0-al)*(q2(i2,i4)-q2(i2,i3))

            D0comb_coli=
     &          al*D0regrp_coli(q2(i3,i4),qq2,
     &          q2(i1,i2)*al*al,q2(i2,i3),
     &          cd0,q2(i2,i4),m2(i3),m2(i4),mm2,m2(i2))
     &        +(1d0-al)*D0regrp_coli(q2(i3,i1),
     &          (1d0-al)*(1d0-al)*q2(i1,i2),qq2,q2(i4,i3),
     &          cd0,q2(i1,i4),m2(i3),m2(i1),mm2,m2(i4))
                
c>            write(*,*) 'D0comb b = ',D0comb_coli

            return
          else
              write(*,*) 'D0comb_coli: case 2, lambda<0 =',lambda
              write(*,*) 'D0comb_coli: ijk ',i1,i2,k
              write(*,*) 'D0comb_coli: qijk ',q2(i1,i2),q2(i1,k),
     &            q2(i2,k)
              write(*,111)' D0reg_coli: p12 = ',p12
              write(*,111)' D0reg_coli: p23 = ',p23
              write(*,111)' D0reg_coli: p34 = ',p34
              write(*,111)' D0reg_coli: p14 = ',p14
              write(*,111)' D0reg_coli: p13 = ',p13
              write(*,111)' D0reg_coli: p24 = ',p24
              write(*,111)' D0reg_coli: m12 = ',m12
              write(*,111)' D0reg_coli: m22 = ',m22
              write(*,111)' D0reg_coli: m32 = ',m32
              write(*,111)' D0reg_coli: m42 = ',m42
              error_code = 1
          endif
        endif
      endif
 99   enddo
      endif
      enddo
      enddo

c      return

      write(*,*) 'D0comb_coli: no viable case found'
      write(*,111)' D0comb_coli: p12 = ',p12
      write(*,111)' D0comb_coli: p23 = ',p23
      write(*,111)' D0comb_coli: p34 = ',p34
      write(*,111)' D0comb_coli: p14 = ',p14
      write(*,111)' D0comb_coli: p13 = ',p13
      write(*,111)' D0comb_coli: p24 = ',p24
      write(*,111)' D0comb_coli: m12 = ',m12
      write(*,111)' D0comb_coli: m22 = ',m22
      write(*,111)' D0comb_coli: m32 = ',m32
      write(*,111)' D0comb_coli: m42 = ',m42
      error_code = 1

      end

#ifdef CHUNK

c D0regrm_coli not useful for complex r13!

************************************************************************
      function D0regrm_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  scalar 4-point function  for r13 real positive                      *
*  regular case  based on general result of                            *
*        A.Denner, U.Nierste and R.Scharf, Nucl. Phys. B367 (1991) 637 *
*                                                                      *
*                     m22                                              *
*       p12  ---------------------  q23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | m32                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       q14  ---------------------  q34                                *
*                     m42                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  29.12.08 Ansgar Denner       last changed 29.12.08 Ansgar Denner    *
************************************************************************
      implicit   none
      complex*16 D0regrm_coli
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42

      complex*16 l12,l13,l14,l23,l24,l34
      complex*16 r12,r13,r14,r23,r24,r34
      complex*16 r21,r31,r41,r32,r42,r43
      complex*16 a,b,c,d,det
      complex*16 x(2,4)
      complex*16 pol12,pol14,pol23,pol34,q1,q2,q3,q4,qb1od,qb2od,qb1,qb2
      complex*16 ch0,ch1,ch2,ch3,l1,l2,l3,l4,l3t,l4t
      complex*16 eta1,eta2,eta3,eta4,eta
      real*8     ir12,ir13,ir14,ir23,ir24,ir34
      real*8     ix(2,4),iqbod1,iqbod2
      real*8     t12,t23,t34,t14,t1,t2,t3,t4,td,tqb1,tqb2
      real*8     test01,test23,test0,test1,test2,test3,ipop(2)
      real*8     u,v
      complex*16 cspcos_coli,cln_coli,eta2s_coli
      integer    i

      logical    flag2,flag3
      save       flag2,flag3
      data       flag2 /.true./, flag3 /.true./
c      logical    errflag
c      save       errflag
c      data       errflag /.true./

#ifdef CHECK
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 elimminf2_coli
      complex*16 D0regrm_check
c     complex*16 cD0reg
      integer    j
      logical flag(0:1)
      data    flag /2*.true./
      save    flag  
#endif

      include "coli_params.h"
      include "coli_checkparams.h"

c      write(*,*) ' D0regrm_coli in ',
c     &    p12,p23,p34,p14,p13,p24,m12,m22,m32,m42

c      D0regrm_coli=cD0reg(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
c      write(*,*) ' D0regrm_coli: cD0reg= ',D0regrm_coli


#ifdef CHECK
 100  format(((a)))
 101  format(a22,g25.17)
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ms42 = elimminf2_coli(m42)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps34 = elimminf2_coli(p34)
        ps14 = elimminf2_coli(p14)
        ps24 = elimminf2_coli(p24)
        ps13 = elimminf2_coli(p13)

        if(ms32.eq.0d0.or.ms12.eq.cd0.or.ms22.eq.cd0.or.ms42.eq.cd0
     &      .or.dimag(p12).ne.0d0.or.dimag(p23).ne.0d0
     &      .or.dimag(p34).ne.0d0.or.dimag(p14).ne.0d0
     &      .or.dimag(p24).ne.0d0.or.dimag(p13).ne.0d0) then
          write(*,100)' D0regrm_coli called improperly:'
          write(*,111)' D0regrm_coli: p12 = ',p12
          write(*,111)' D0regrm_coli: p23 = ',p23
          write(*,111)' D0regrm_coli: p34 = ',p34
          write(*,111)' D0regrm_coli: p14 = ',p14
          write(*,111)' D0regrm_coli: p13 = ',p13
          write(*,111)' D0regrm_coli: p24 = ',p24
          write(*,111)' D0regrm_coli: m12 = ',m12
          write(*,111)' D0regrm_coli: m22 = ',m22
          write(*,111)' D0regrm_coli: m32 = ',m32
          write(*,111)' D0regrm_coli: m42 = ',m42
          write(*,*)' D0regrm_coli: test= ',
     &        m32.ne.0d0,m12.eq.cd0,m22.eq.cd0,m42.eq.cd0,
     &        p23.eq.m22.and.p13.eq.m12
     &        ,p23.eq.m22.and.p34.eq.m42
     &        ,p13.eq.m12.and.p34.eq.m42
     &        ,ps23.eq.cd0.and.ms22.eq.cd0
     &        ,ps34.eq.cd0.and.ms42.eq.cd0
     &        ,ps13.eq.cd0.and.ms12.eq.cd0
     &        ,ps12.eq.cd0.and.ms12.eq.cd0.and.ms22.eq.cd0
     &        ,ps14.eq.cd0.and.ms12.eq.cd0.and.ms42.eq.cd0
     &        ,ps24.eq.cd0.and.ms22.eq.cd0.and.ms22.eq.cd0
     &        ,dimag(p12).ne.0d0,dimag(p23).ne.0d0
     &        ,dimag(p34).ne.0d0,dimag(p14).ne.0d0
     &        ,dimag(p24).ne.0d0,dimag(p13).ne.0d0
          stop
        end if
      end if
#endif

      if(flag(1))then
        write(*,100)' D0regrm_coli: ',
     &      '    not yet tested at all'
c     &      '    not yet tested in physical process'
        write(*,111)' D0regrm_coli: p12 = ',p12
        write(*,111)' D0regrm_coli: p23 = ',p23
        write(*,111)' D0regrm_coli: p34 = ',p34
        write(*,111)' D0regrm_coli: p14 = ',p14
        write(*,111)' D0regrm_coli: p13 = ',p13
        write(*,111)' D0regrm_coli: p24 = ',p24
        write(*,111)' D0regrm_coli: m12 = ',m12
        write(*,111)' D0regrm_coli: m22 = ',m22
        write(*,111)' D0regrm_coli: m32 = ',m32
        write(*,111)' D0regrm_coli: m42 = ',m42
        flag(1)=.false.
      endif

      l12 = (m12+m22-p12)
      l13 = (m12+m32-p13)
      l14 = (m12+m42-p14)
      l23 = (m22+m32-p23)
      l24 = (m22+m42-p24)
      l34 = (m32+m42-p34)

      if(l12.ne.cd0)then
        r12 = l12/(2d0*m22)*(1d0+sqrt(1d0-4d0*m12*m22/l12**2))
      else
        r12 = dcmplx(0d0,1d0)*sqrt(m12/m22)
      endif
      r21 = r12*m22/m12
      if(l13.ne.cd0)then
        r13 = l13/(2d0*m32)*(1d0+sqrt(1d0-4d0*m12*m32/l13**2))
      else
        r13 = dcmplx(0d0,1d0)*sqrt(m12/m32)
      endif
      r31 = r13*m32/m12
      if(l14.ne.cd0)then
        r14 = l14/(2d0*m42)*(1d0+sqrt(1d0-4d0*m12*m42/l14**2))
      else
        r14 = dcmplx(0d0,1d0)*sqrt(m12/m42)
      endif
      r41 = r14*m42/m12
      if(l23.ne.cd0)then
        r23 = l23/(2d0*m32)*(1d0+sqrt(1d0-4d0*m22*m32/l23**2))
      else
        r23 = dcmplx(0d0,1d0)*sqrt(m22/m32)
      endif
      r32 = r23*m32/m22
      if(l24.ne.cd0)then
        r24 = l24/(2d0*m42)*(1d0+sqrt(1d0-4d0*m22*m42/l24**2))
      else
        r24 = dcmplx(0d0,1d0)*sqrt(m22/m42)
      endif
      r42 = r24*m42/m22
      if(l34.ne.cd0)then
        r34 = l34/(2d0*m42)*(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
      else
        r34 = dcmplx(0d0,1d0)*sqrt(m32/m42)
      endif
      r43 = r34*m42/m32


c for testing
c      r31 = 1d0/r13
c      r13 = m12/m32/r13
c      r42 = 1d0/r24
c      r24 = m22/m42/r24

#ifdef CHECK
      if(conscheck)then
        if( dimag(r13).eq.cd0.and.dreal(r13).gt.0d0.or.
     &      dimag(l23-r31*l12).gt.0d0.or.dimag(l34-r31*l14).gt.0d0.or.
     &      dimag(m12*(1d0/r13-r31)).gt.0d0.or.
     &      dimag(m12*(1d0/r13-r31)*r31).gt.0d0.or.
     &      dimag(m32-m12*r31*r31).gt.0d0.or.
     &      dimag(2d0*m32+l13*r31).gt.0d0) then
          write(*,100)'  D0regrm_coli: analytic continuation doubtful'
          write(*,111)' D0regrm_coli: p12 = ',p12
          write(*,111)' D0regrm_coli: p23 = ',p23
          write(*,111)' D0regrm_coli: p34 = ',p34
          write(*,111)' D0regrm_coli: p14 = ',p14
          write(*,111)' D0regrm_coli: p13 = ',p13
          write(*,111)' D0regrm_coli: p24 = ',p24
          write(*,111)' D0regrm_coli: m12 = ',m12
          write(*,111)' D0regrm_coli: m22 = ',m22
          write(*,111)' D0regrm_coli: m32 = ',m32
          write(*,111)' D0regrm_coli: m42 = ',m42
          write(*,*)' D0regrm_coli: test= ',
     &        dimag(r13).eq.cd0.and.dreal(r13).gt.0d0,
     &      dimag(l23-r31*l12).gt.0d0,dimag(l34-r31*l14).gt.0d0,
     &      dimag(m12*(1d0/r13-r31)).gt.0d0,
     &      dimag(m12*(1d0/r13-r31)*r31).gt.0d0,
     &      dimag(m32-m12*r31*r31).gt.0d0,
     &      dimag(2d0*m32+l13*r31).gt.0d0
          write(*,*)' D0regrm_coli: test2= ',
     &      m12*(1d0/r13-r31)*r31,
     &      m32-m12*r31*r31,
     &      2d0*m32+l13*r31
        endif
      endif
#endif      

      ir12 = sign(1d0,1d0-abs(r12*r21))
      ir13 = sign(1d0,1d0-abs(r13*r31))
      ir14 = sign(1d0,1d0-abs(r14*r41))
      ir23 = sign(1d0,1d0-abs(r23*r32))
      ir24 = sign(1d0,1d0-abs(r24*r42))
      ir34 = sign(1d0,1d0-abs(r34*r43))

      a   =  m42*(l34/r42-l23 + (l12-l14/r42)*r31)
      b   =  m12*m42*(1d0/r13-r31)*(1d0/r42-r24)+l12*l34-l14*l23
      c   =  m12*(l34*r24-l23 + (l12-l14*r24)/r13)
      d   =  l23-r31*l12 - (l34-r31*l14)*r24
      det =  sqrt(b*b-4d0*a*c)
      x(1,4) = (-b+sqrt(b*b-4d0*a*c))/(2d0*a)
      x(2,4) = (-b-sqrt(b*b-4d0*a*c))/(2d0*a)
      if(abs(x(1,4)).gt.abs(x(2,4))) then
        x(2,4) = c/(a*x(1,4))
      else
        x(1,4) = c/(a*x(2,4))
      end if

      ix(1,4) = -sign(1d0,dreal(d))
      ix(2,4) = +sign(1d0,dreal(d))

      write(*,*) 'D0regrm_coli l ',l12,l13,l14
      write(*,*) 'D0regrm_coli l ',l23,l24,l34
      write(*,*) 'D0regrm_coli r ',r12,r13,r14
      write(*,*) 'D0regrm_coli r ',r21,r31,r41
      write(*,*) 'D0regrm_coli r ',r23,r24,r34
      write(*,*) 'D0regrm_coli r ',r32,r42,r43
      write(*,*) 'D0regrm_coli x ',x(1,4),x(2,4)
      write(*,*) 'D0regrm_coli ir ',ir12,ir13,ir14
      write(*,*) 'D0regrm_coli ir ',ir23,ir24,ir34
      write(*,*) 'D0regrm_coli ix ',ix(1,4),ix(2,4)

      x(1,1) = x(1,4)/r24
      x(2,1) = x(2,4)/r24
      x(1,2) = x(1,4)/r24*r31
      x(2,2) = x(2,4)/r24*r31
      x(1,3) = x(1,4)*r31
      x(2,3) = x(2,4)*r31
 
      ix(1,1) =  sign(1d0,ix(1,4)*dreal(r24)) ! needed for correct
      ix(2,1) =  sign(1d0,ix(2,4)*dreal(r24)) ! continuation
      ix(1,3) =  sign(1d0,ix(1,4)*dreal(r13)) ! needed for correct
      ix(2,3) =  sign(1d0,ix(2,4)*dreal(r13)) ! continuation
      ix(1,2) =  sign(1d0,ix(1,4)*dreal(r24)*dreal(r13)) ! needed for correct
      ix(2,2) =  sign(1d0,ix(2,4)*dreal(r24)*dreal(r13)) ! continuation

      D0regrm_coli = dcmplx(0d0)


c *** improves numerical stability in extreme cases

c check from here

      do i=1,2
c m12*P(1,x/r24,0,0) / (x/r24)
        pol12 = m12/x(i,1)+l12+m22*x(i,1)
c m12*P(1,0,0,x) / x
        pol14 = m12/x(i,4)+l14+m42*x(i,4)
c m32*P(0,r13*x/r24,1,0) / (r13*x/r24)
        pol23 = m32/x(i,2)+l23+m22*x(i,2)
c m32*P(0,0,1,r13*x) / (r13*x)
        pol34 = m32/x(i,3)+l34+m42*x(i,3)

c m4*m3*Q(1,0,0,x) / x
        q4  = (l34-r31*l14)+m12*(1/r13-r31)/x(i,4)
c m2*m3*Q(1,x/r24,0,0) / (x/r24)
        q1  = (l23-r31*l12)+m12*(1/r13-r31)/x(i,1)
c m2*m3 * Q(0,r13*x/r24,1,0)  / (r13*x/r24)
        q2  = (l23-r31*l12)+m32*(1d0-r31*r13)/x(i,2)
c m4*m3 * Q(0,0,1,r13*x)  / (r13)
        q3  = (l34-r31*l14)+m32*(1d0-r31*r13)/x(i,3)


c m3*m2 qbar(0,0,1,r13*x)/(m2*m3*d)
        qb2  = ((l23-r24*l34)-m42*(r24-1d0/r42)*x(i,3))
c m1*m2 qbar(1,0,0,x)/(m2*m3*d)
        qb1  = ((l12-r24*l14)-m42*(r24-1d0/r42)*x(i,4))

        qb1od=qb1/d
        qb2od=qb2/d

        write(*,*) 'cD0regrm qb1od ',qb1od,pol12/q1,pol14/q4
        write(*,*) 'cD0regrm qb2od ',qb2od,pol34/q3,pol23/q2
        write(*,*) 'cD0regrm q1 ',q1,pol12/qb1od
        write(*,*) 'cD0regrm q4 ',q4,pol14/qb1od

        t12 = abs(p12)/max(abs(m12/x(i,1)),abs(l12),abs(m22*x(i,1)))
        t14 = abs(p14)/
     &        max(abs(m12/x(i,4)),abs(l14),abs(m42*x(i,4)))
        t23 = abs(p23)/
     &        max(abs(m32/(x(i,2))),abs(l23),abs(m22*x(i,2)))
        t34 = abs(p34)/
     &        max(abs(m32/(x(i,3))),abs(l34),abs(m42*x(i,3)))
        t1  = abs(q1)/
     &      max(abs(l23),abs(m12*r31/x(i,1)),abs(r31*l12),
     &      abs(m12/r13/x(i,1)))
        t4  = abs(q4)/
     &      max(abs(l34),abs(m12*r31/x(i,4)),abs(r31*l14),
     &      abs(m12/r13/x(i,4)))
        t2  = abs(q2)/max(abs(l23),abs(r31*l12),
     &                     abs(m32/x(i,2)),abs(r31*r13/x(i,2)))
        t3  = abs(q3)/max(abs(l34),abs(r31*l14),
     &                     abs(m32/x(i,3)),abs(m32*r31*r13/x(i,3)))

        td= abs(d)/
     &      max(abs(l23),abs(r31*l12),abs(l34*r24),abs(r31*l14*r24))
        tqb1=abs(qb1od*d)/max(abs(l12),abs(r24*l14),
     &      abs(m42*r24*x(i,4)),abs(m42/r42*x(i,4)))
        tqb2=abs(qb2od*d)/max(abs(l23),abs(r24*l34),
     &      abs(m42*r24*x(i,3)),abs(m42/r42*x(i,3)))

        if (min(t12,t1).gt.min(t14,t4).and.min(t12,t1).gt.min(tqb2,td)) 
     &      qb1od =pol12/q1
        if (min(t14,t4).gt.min(t12,t1).and.min(t14,t4).gt.min(tqb2,td)) 
     &      qb1od =pol14/q4
        if (min(t23,t2).gt.min(t34,t3).and.min(t23,t2).gt.min(tqb1,td)) 
     &      qb2od =pol23/q2
        if (min(t34,t3).gt.min(t23,t2).and.min(t34,t3).gt.min(tqb1,td)) 
     &      qb2od =pol34/q3
        if (min(t12,tqb2,td).gt.t1) q1=pol12/qb1od
        if (min(t14,tqb2,td).gt.t4) q4=pol14/qb1od

        write(*,*) 'cD0regrm qb1od ',qb1od,pol12/q1,pol14/q4
        write(*,*) 'cD0regrm qb2od ',qb2od,pol34/q3,pol23/q2
        write(*,*) 'cD0regrm q1 ',q1,pol12/qb1od
        write(*,*) 'cD0regrm q4 ',q4,pol14/qb1od

        eta1=eta2s_coli(-x(i,1),r31,-ix(i,1),ir13,-ix(i,2))

        if(eta1.ne.cd0)then
           l1=cln_coli(q1,-1d0)

           D0regrm_coli = D0regrm_coli - (2*i-3) * eta1*l1 

           write(*,*) 'D0regrm_coli eta1 i ',eta1*l1
        endif

        eta2=eta2s_coli(-x(i,4),r31,-ix(i,4),ir13,-ix(i,3))

        if(eta2.ne.cd0)then
           l2=cln_coli(q4,-1d0) + cln_coli(r24,ir24)

           D0regrm_coli = D0regrm_coli + (2*i-3) * eta2*l2 

           write(*,*) 'D0regrm_coli eta1 i ',eta2*l2
        endif

        eta3=eta2s_coli(-x(i,4),1d0/r24,-ix(i,4),-ir24,-ix(i,1))

        if(eta3.ne.cd0)then
          if(dimag(x(i,4)).ne.0d0)then
            iqbod1=d-qb1
          elseif(dreal(x(i,4)).gt.0d0) then
            iqbod1=q4-pol14
          else
            iqbod1=q1-pol12
          endif            

          l3=cln_coli(qb1od,iqbod1) + cln_coli(x(i,1),ix(i,1)) 
     &        - log(m12)

#ifdef CHECK
          if(conscheck)then
            if(dimag(r24).ne.0d0) then
              v=dimag(x(i,4))/dimag(r24)
              u=dimag(x(i,4)/r24)/dimag(1d0/r24)
              l3t=cln_coli(m12+l12*v+l14*u+m22*v*v
     &            +m42*u*u+l24*u*v,-1d0)
     &            -cln_coli(m12*(1d0/r13-r31)+(l23-r31*l12)*v
     &            +(l34-r31*l14)*u,-1d0)
              
              write(*,*) 'D0regrm_coli comp1 l3= ',l3,l3t,l3t/l3

            elseif(dimag(x(i,4)).ne.0d0) then
              l3t=cln_coli(qb1,-1d0)-cln_coli(d,-1d0)

              write(*,*) 'D0regrm_coli comp2 l3= ',l3,l3t,l3t/l3

            elseif(dreal(x(i,4)).gt.0d0) then
              l3t=cln_coli(pol14,-1d0)-cln_coli(q4,-1d0)

              write(*,*) 'D0regrm_coli comp2 l3= ',l3,l3t,l3t/l3

            elseif(dreal(x(i,1)).gt.0d0)then
              l3t=cln_coli(pol12,-1d0)-cln_coli(q1,-1d0)

              write(*,*) 'D0regrm_coli comp3 l3= ',l3,l3t,l3t/l3

            else
              if (flag3) then
                write(*,*) 
                write(*,*) 'D0regrm_coli: case for l3 not implemented'
                flag3 = .false.
              end if
              write(errout,*) 
              write(errout,*) 
     &            'D0regrm_coli: case for l3 not implemented '
              write(errout,*) 
     &            'D0regrm_coli: args ',p12,p23,p34,p14,p13,p24,
     &            m12,m22,m32,m42
              write(errout,*) 'r24 = ',r24
              write(errout,*) 'x(i,4) = ',x(i,4)
              
              write(errout,*) 'x(i,1) = ',x(i,1)
            endif 
            
            if(abs(l3t/l3-1d0).gt.testacc) then
              if (flag2) then
                write(*,*) 
                write(*,*) 'D0regrm_coli: eta terms possibly wrong'
                flag2 = .false.
              end if
              write(errout,*) 
              write(errout,*) 'D0regrm_coli: eta terms possibly wrong'
              write(errout,*) 
     &            'D0regrm_coli args ',p12,p23,p34,p14,p13,p24,
     &            m12,m22,m32,m42
              write(errout,*) 
     &            'D0regrm_coli qs ',qb1,d,pol14,q4,pol12,q1
              write(errout,*) 'l3  = ',l3
              write(errout,*) 'l3t = ',l3t
              write(errout,*) 'diff   = ',abs((l3-l3t)/l3)
            endif 
          endif
#endif

           D0regrm_coli = D0regrm_coli + (2*i-3) * eta3*l3 

           write(*,*) 'D0regrm_coli eta1 i ',eta3*l3
         endif

        eta4=eta2s_coli(-x(i,3),1d0/r24,-ix(i,3),-ir24,-ix(i,2))

        if(eta4.ne.cd0)then
          if(dimag(x(i,3)).ne.0d0)then
            iqbod2=d-qb2
          elseif(dreal(x(i,3)).gt.0d0) then
            iqbod2=q3-pol34
          else
            iqbod2=q2-pol23
          endif            

#ifdef CHECK
          if(conscheck)then
            if(dimag(r24).ne.0d0) then
              v=dimag(x(i,3))/dimag(r24)
              u=dimag(x(i,3)/r24)/dimag(1d0/r24)
              l4t=cln_coli(m32+l23*v+l34*u+m22*v*v
     &            +m42*u*u+l24*u*v,-1d0)
     &            -cln_coli(m32*(1d0-r13*r31)+(l23-r31*l12)*v
     &            +(l34-r31*l14)*u,-1d0)
              
              write(*,*) 'D0regrm_coli comp1 l4= ',l4,l4t,l4t/l4

            elseif(dimag(x(i,3)).ne.0d0) then
              l4t=cln_coli(qb2,-1d0)-cln_coli(d,-1d0)

              write(*,*) 'D0regrm_coli comp2 l4= ',l4,l4t,l4t/l4

            elseif(dreal(x(i,3)).gt.0d0) then
              l4t=cln_coli(pol34,-1d0)-cln_coli(q3,-1d0)

              write(*,*) 'D0regrm_coli comp2 l4= ',l4,l4t,l4t/l4

            elseif(dreal(x(i,1)).gt.0d0)then
              l4t=cln_coli(pol23,-1d0)-cln_coli(q2,-1d0)

              write(*,*) 'D0regrm_coli comp3 l4= ',l4,l4t,l4t/l4

            else
              if (flag3) then
                write(*,*) 
                write(*,*) 'D0regrm_coli: case for l4 not implemented'
                flag3 = .false.
              end if
              write(errout,*) 
              write(errout,*) 
     &            'D0regrm_coli: case for l4 not implemented '
              write(errout,*) 
     &            'D0regrm_coli: args ',p12,p23,p34,p14,p13,p24,
     &            m12,m22,m32,m42
              write(errout,*) 'r24 = ',r24
              write(errout,*) 'x(i,4) = ',x(i,4)
              
              write(errout,*) 'x(i,1) = ',x(i,1)
            endif 
            
            if(abs(l4t/l4-1d0).gt.testacc) then
              if (flag2) then
                write(*,*) 
                write(*,*) 'D0regrm_coli: eta terms possibly wrong'
                flag2 = .false.
              end if
              write(errout,*) 
              write(errout,*) 'D0regrm_coli: eta terms possibly wrong'
              write(errout,*) 
     &            'D0regrm_coli args ',p12,p23,p34,p14,p13,p24,
     &            m12,m22,m32,m42
              write(errout,*) 
     &            'D0regrm_coli qs ',qb1,d,pol14,q4,pol12,q1
              write(errout,*) 'l4  = ',l4
              write(errout,*) 'l4t = ',l4t
              write(errout,*) 'diff   = ',abs((l4-l4t)/l4)
            endif 
          endif
#endif

           l4=cln_coli(qb2od,iqbod2) + cln_coli(x(i,2),ix(i,2)) 
     &         - log(m32)

           D0regrm_coli = D0regrm_coli - (2*i-3) * eta4*l4 

           write(*,*) 'D0regrm_coli eta1 i ',eta4*l4
        endif


        D0regrm_coli = D0regrm_coli + (2*i-3) * (
     &      - cspcos_coli(-x(i,3),r43,-ix(i,3),ir34)
     &      - cspcos_coli(-x(i,3),1d0/r34,-ix(i,3),-ir34)
     &      + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14)
     &      + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14)
     &      - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12)
     &      - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12)
     &      + cspcos_coli(-x(i,2),r23,-ix(i,2),ir23)
     &      + cspcos_coli(-x(i,2),1d0/r32,-ix(i,2),-ir23))
        
        write(*,*) 'D0regrm_coli arg1 i ',
     &      -x(i,3),r43,-ix(i,4),ir34
        write(*,*) 'D0regrm_coli arg2 i ',
     &      -x(i,3),1d0/r34,-ix(i,4),-ir34
        write(*,*) 'D0regrm_coli arg3 i ',
     &      -x(i,4),r41,-ix(i,4),ir14
        write(*,*) 'D0regrm_coli arg4 i ',
     &      -x(i,4),1d0/r14,-ix(i,4),-ir14
        write(*,*) 'D0regrm_coli arg5 i ',
     &      -x(i,1),r21,-ix(i,1),ir12
        write(*,*) 'D0regrm_coli arg6 i ',
     &      -x(i,1),1d0/r12,-ix(i,1),-ir12
        write(*,*) 'D0regrm_coli arg7 i ',
     &      -x(i,2),r23,-ix(i,2),ir23
        write(*,*) 'D0regrm_coli arg8 i ',
     &      -x(i,2),1d0/r32,-ix(i,2),-ir23
        
        write(*,*) 'D0regrm_coli contr i ',
     &      - cspcos_coli(-x(i,3),r43,-ix(i,3),ir34),
     &      - cspcos_coli(-x(i,3),1d0/r34,-ix(i,3),-ir34),
     &      + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14),
     &      + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14),
     &      - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12),
     &      - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12),
     &      + cspcos_coli(-x(i,2),r23,-ix(i,2),ir23),
     &      + cspcos_coli(-x(i,2),1d0/r32,-ix(i,2),-ir23)
        write(*,*) 'D0regrm_coli sum i ',
     &      - cspcos_coli(-x(i,3),r43,-ix(i,3),ir34)
     &      - cspcos_coli(-x(i,3),1d0/r34,-ix(i,3),-ir34)
     &      + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14)
     &      + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14)
     &      - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12)
     &      - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12)
     &      + cspcos_coli(-x(i,2),r23,-ix(i,2),ir23)
     &      + cspcos_coli(-x(i,2),1d0/r32,-ix(i,2),-ir23)
        
      enddo
      
      write(*,*) 'D0regrm_coli sum ',D0regrm_coli
      
      D0regrm_coli = D0regrm_coli/det
      
      write(*,*) 'D0regrm_coli=   ',D0regrm_coli

#ifdef CHECK

      if (conscheck.and.(
     &    dimag(x(1,4)).eq.0d0.or.dimag(x(2,4)).eq.0d0)) then

        do j=1,3
          if(j.eq.1) then
            ix(1,4) = +sign(1d0,dreal(d))
            ix(2,4) = -sign(1d0,dreal(d))
          elseif(j.eq.2) then
            ix(1,4) = +sign(1d0,dreal(d))
            ix(2,4) = +sign(1d0,dreal(d))
          elseif(j.eq.3) then
            ix(1,4) = -sign(1d0,dreal(d))
            ix(2,4) = -sign(1d0,dreal(d))
          endif
          ix(1,1) =  sign(1d0,ix(1,4)*dreal(r24)) ! needed for correct
          ix(2,1) =  sign(1d0,ix(2,4)*dreal(r24)) ! continuation


          D0regrm_check = dcmplx(0d0)
          do i=1,2
            eta=eta2s_coli(-x(i,4),1d0/r24,-ix(i,4),-ir24,-ix(i,1))
            
            if(eta.ne.0d0)then
              ch0   = m42*(r31*x(i,4))**2+l34*r31*x(i,4)+m32
              test0 = abs(ch0)/max(abs(m42*(r31*x(i,4))**2),
     &            abs(l34*r31*x(i,4)),abs(m32))
              ch1   = m42*(x(i,4))**2+l14*x(i,4)+m12
              test1 = abs(ch1)/
     &            max(abs(m42*(x(i,4))**2),abs(l14*x(i,4)),abs(m12))
              test01=min(test0,test1)
              ch2   = m22*(r31*x(i,1))**2+l23*r31*x(i,1)+m32
              test2 = abs(ch2)/max(abs(m22*(r31*x(i,1))**2),
     &            abs(l23*r31*x(i,1)),abs(m32))
              ch3   = m22*(x(i,1))**2+l12*x(i,1)+m12
              test3 = abs(ch3)/
     &            max(abs(m22*(x(i,1))**2),abs(l12*x(i,1)),abs(m12))
              test23 = min(test2,test3)
              if(dreal(x(i,4)).gt.0d0)then
                ipop(i)=dreal(ch0-ch1)
              elseif(dreal(x(i,1)).gt.0d0)then
                ipop(i)=dreal(ch2-ch3)
              endif
              if (test23.gt.test01) then
                l1 = cln_coli(ch2/ch3,ipop(i))
              else
                l1 = cln_coli(ch0/ch1,ipop(i))      
              end if
              
              if(conscheck)then
                if(dimag(r24).ne.0d0) then
                  v=dimag(x(i,4))/dimag(r24)
                  u=dimag(x(i,4)/r24)/dimag(1d0/r24)
                  l2=cln_coli(m32+l23*r31*v+l34*r31*u+m22*(r31*v)**2
     &                +m42*(r31*u)**2+l24*r31*r31*u*v,-1d0)
     &                -cln_coli(m12+l12*v+l14*u+m22*v*v
     &                +m42*u*u+l24*u*v,-1d0)
                  
                  write(*,*) 'D0regrm_check comp1 l2= ',l2,l1,l2/l1
                  
                elseif(dimag(x(i,4)).ne.0d0.or.dreal(x(i,4)).gt.0d0)then
                  if(test01.gt.test23)then
                    l2=cln_coli(ch0,-1d0)-cln_coli(ch1,-1d0)
                  elseif(test0.gt.test1)then
                    l2=cln_coli(ch0,-1d0)-cln_coli(ch0*ch3/ch2,-1d0)
                  else
                    l2=cln_coli(ch1*ch2/ch3,-1d0)-cln_coli(ch1,-1d0)
                  endif
                  
                  write(*,*) 'D0regrm_check comp2 l2= ',l2,l1,l2/l1
                  
                elseif(dreal(x(i,1)).gt.0d0)then
                  if(test23.gt.test01)then
                    l2=cln_coli(ch2,-1d0)-cln_coli(ch3,-1d0)
                  elseif(test2.gt.test3)then
                    l2=cln_coli(ch2,-1d0)-cln_coli(ch2*ch1/ch0,-1d0)
                  else
                    l2=cln_coli(ch3*ch0/ch1,-1d0)-cln_coli(ch3,-1d0)
                  endif
                  
                  write(*,*) 'D0regrm_check comp3 l2= ',l2,l1,l2/l1
                  
                else
                  if (flag3) then
                    write(*,*) 
                    write(*,*) '
     &                  D0regrm_check: case for l2 not implemented'
                    flag3 = .false.
                  end if
                  write(errout,*) 
                  write(errout,*) 
     &                'D0regrm_check: case for l2 not implemented '
                  write(errout,*) 
     &                'D0regrm_check: args ',p12,p23,p34,p14,p13,p24,
     &                m12,m22,m32,m42
                  write(errout,*) 'r24 = ',r24
                  write(errout,*) 'x(i,4) = ',x(i,4)
                  
                  write(errout,*) 'x(i,1) = ',x(i,1)
                endif 
                
c     write(*,*) 'D0regrm_check l2= ',l2,l1,l2/l1
                
                if(abs(l2/l1-1d0).gt.testacc) then
                  if (flag2) then
                    write(*,*) 
                    write(*,*) 'D0regrm_check: eta terms possibly wrong'
                    flag2 = .false.
                  end if
                  write(errout,*) 
                  write(errout,*) 
     &                'D0regrm_check: eta terms possibly wrong'
                  write(errout,*) 
     &                'D0regrm_check args ',p12,p23,p34,p14,p13,p24,
     &                m12,m22,m32,m42
                  write(errout,*) 
     &                'D0regrm_check chi ',ch0,ch1,ch2,ch3
                  write(errout,*) 'l1 = ',l1
                  write(errout,*) 'l2 = ',l2
                  write(errout,*) 'diff   = ',abs((l2-l1)/l1)
                endif 
              endif
              
              
              if(eta.ne.0d0)then
                D0regrm_check = D0regrm_check + (2*i-3) * (
     &              + eta*(l1-log(m32/m12)) ) 
              endif 
              
              write(*,*) 'D0regrm_check eta i ',
     &            eta*(l1-log(m32/m12))
              
            endif
            
            D0regrm_check = D0regrm_check + (2*i-3) * (
     &          - cspcos_coli(-x(i,4),r43*r31,-ix(i,4),ir34)
     &          - cspcos_coli(-x(i,4),r31/r34,-ix(i,4),-ir34)
     &          + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14)
     &          + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14)
     &          - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12)
     &          - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12)
     &          + cspcos_coli(-x(i,1),r31*r23,-ix(i,1),ir23)
     &          + cspcos_coli(-x(i,1),r31/r32,-ix(i,1),-ir23))
            
            write(*,*) 'D0regrm_check arg1 i ',
     &          -x(i,4),r43*r31,-ix(i,4),ir34
            write(*,*) 'D0regrm_check arg2 i ',
     &          -x(i,4),r31/r34,-ix(i,4),-ir34
            write(*,*) 'D0regrm_check arg3 i ',
     &          -x(i,4),r41,-ix(i,4),ir14
            write(*,*) 'D0regrm_check arg4 i ',
     &          -x(i,4),1d0/r14,-ix(i,4),-ir14
            write(*,*) 'D0regrm_check arg5 i ',
     &          -x(i,1),r21,-ix(i,1),ir12
            write(*,*) 'D0regrm_check arg6 i ',
     &          -x(i,1),1d0/r12,-ix(i,1),-ir12
            write(*,*) 'D0regrm_check arg7 i ',
     &          -x(i,1),r31*r23,-ix(i,1),ir23
            write(*,*) 'D0regrm_check arg8 i ',
     &          -x(i,1),r31/r32,-ix(i,1),-ir23
            
            write(*,*) 'D0regrm_check contr i ',
     &          - cspcos_coli(-x(i,4),r43*r31,-ix(i,4),ir34),
     &          - cspcos_coli(-x(i,4),r31/r34,-ix(i,4),-ir34),
     &          + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14),
     &          + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14),
     &          - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12),
     &          - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12),
     &          + cspcos_coli(-x(i,1),r31*r23,-ix(i,1),ir23),
     &          + cspcos_coli(-x(i,1),r31/r32,-ix(i,1),-ir23)
            write(*,*) 'D0regrm_check sum i ',
     &          - cspcos_coli(-x(i,4),r43*r31,-ix(i,4),ir34)
     &          - cspcos_coli(-x(i,4),r31/r34,-ix(i,4),-ir34)
     &          + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14)
     &          + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14)
     &          - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12)
     &          - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12)
     &          + cspcos_coli(-x(i,1),r31*r23,-ix(i,1),ir23)
     &          + cspcos_coli(-x(i,1),r31/r32,-ix(i,1),-ir23)
            
          enddo
          
          write(*,*) 'D0regrm_check sum ',D0regrm_check
          
          D0regrm_check = D0regrm_check/det
          
          write(*,*) 'D0regrm_check =  ',D0regrm_check

          if(cdabs(D0regrm_coli/D0regrm_check-1d0).gt.testacc) then
            write(testout,*) ' D0regrm_coli inconsistent ',j
            write(testout,111) ' D0regrm_coli: p12 = ',p12
            write(testout,111) ' D0regrm_coli: p23 = ',p23
            write(testout,111) ' D0regrm_coli: p34 = ',p34
            write(testout,111) ' D0regrm_coli: p14 = ',p14
            write(testout,111) ' D0regrm_coli: p13 = ',p13
            write(testout,111) ' D0regrm_coli: p24 = ',p24
            write(testout,111) ' D0regrm_coli: m12 = ',m12
            write(testout,111) ' D0regrm_coli: m22 = ',m22
            write(testout,111) ' D0regrm_coli: m32 = ',m32
            write(testout,111) ' D0regrm_coli: m42 = ',m42
            write(testout,111) ' D0regrm_coli  = ',D0regrm_coli
            write(testout,111) ' D0regrm_check = ',D0regrm_check
            write(testout,101) ' diff        = ',
     &          cdabs(D0regrm_coli/D0regrm_check-1d0)
            
            write(*,*)   ' D0regrm_coli inconsistent ',i
            write(*,111) ' D0regrm_coli: p12 = ',p12
            write(*,111) ' D0regrm_coli: p23 = ',p23
            write(*,111) ' D0regrm_coli: p34 = ',p34
            write(*,111) ' D0regrm_coli: p14 = ',p14
            write(*,111) ' D0regrm_coli: p13 = ',p13
            write(*,111) ' D0regrm_coli: p24 = ',p24
            write(*,111) ' D0regrm_coli: m12 = ',m12
            write(*,111) ' D0regrm_coli: m22 = ',m22
            write(*,111) ' D0regrm_coli: m32 = ',m32
            write(*,111) ' D0regrm_coli: m42 = ',m42
            write(*,111) ' D0regrm_coli  = ',D0regrm_coli
            write(*,111) ' D0regrm_check = ',D0regrm_check
            write(*,101) ' diff        = ',
     &          cdabs(D0regrm_coli/D0regrm_check-1d0)
            stop
          endif
          
        enddo
      endif
#endif

      end

c#endif

c#ifdef CHUNK

************************************************************************
      function cD0gen(q12,q23,q34,q14,q13,q24,m12,m22,m32,m42)
************************************************************************
*  general scalar 4-point function                                     *
*  regular case  based on general result of                            *
*        A.Denner, U.Nierste and R.Scharf, Nucl. Phys. B367 (1991) 637 *
*                                                                      *
*                     m22                                              *
*       q12  ---------------------  q23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | m32                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       q14  ---------------------  q34                                *
*                     m42                                              *
*                                                                      *
*  imaginary part sometimes wrong  ????????                            *
*----------------------------------------------------------------------*
*  28.05.02 Ansgar Denner       last changed 16.08.08 Ansgar Denner    *
************************************************************************
      implicit   none
      complex*16 q12,q23,q34,q14,q13,q24
      complex*16 m12,m22,m32,m42
      complex*16 k12,k13,k14,k23,k24,k34
      complex*16 m1,m2,m3,m4
      real*8     ir12,ir14,ir23,ir24,ir34,ir13,ir13or24
      real*8     ix(2,4),is(4),gamma(2)
      complex*16 r12,r13,r14,r23,r24,r34
      complex*16 a,b,c,d,det
      real*8     rd
      complex*16 x(2,4),s(4)
      complex*16 q1,q2,q3,q4,p12,p34,p14,p23,l1,l2,l3,l4
      real*8     t1,t2,t3,t4,t12,t34,t14,t23
      complex*16 cD0gen,cspcoe,olietae,ettile,cdln,cD0m0,cD0p0
      real*8     swap
      integer    k,j
      complex*16 cd0
      data       cd0 /(0d0,0d0)/

      integer error_code
      common /coli_error/ error_code
      save /coli_error/

c      write(*,*) 'cD0gen in ',q12,q23,q34,q14,q13,q24,m12,m22,m32,m42

      if (m12.eq.0d0) then
        cD0gen = cD0m0(q23,q12,q14,q34,q13,q24,m32,m22,cd0,m42)
      else if (m22.eq.0d0) then
        cD0gen = cD0m0(q14,q12,q23,q34,q24,q13,m42,m12,cd0,m32)
      else if (m32.eq.0d0) then
        cD0gen = cD0m0(q12,q23,q34,q14,q13,q24,m12,m22,cd0,m42)
      else if (m42.eq.0d0) then
        cD0gen = cD0m0(q23,q34,q14,q12,q24,q13,m22,m32,cd0,m12)
      else if (q12.eq.0d0.and.dimag(m12/m22).eq.0d0) then
        cD0gen = cD0p0(q34,q23,q12,q14,q24,q13,m42,m32,m22,m12)
      else if (q23.eq.0d0.and.dimag(m32/m22).eq.0d0) then
        cD0gen = cD0p0(q14,q12,q23,q34,q24,q13,m42,m12,m22,m32)
      else if (q34.eq.0d0.and.dimag(m32/m42).eq.0d0) then
        cD0gen = cD0p0(q12,q23,q34,q14,q13,q24,m12,m22,m32,m42)
      else if (q14.eq.0d0.and.dimag(m12/m42).eq.0d0) then
        cD0gen = cD0p0(q23,q34,q14,q12,q24,q13,m22,m32,m42,m12)
      else if (q24.eq.0d0.and.dimag(m42/m22).eq.0d0) then
        cD0gen = cD0p0(q13,q34,q24,q12,q14,q23,m12,m32,m42,m22)
      else if (q13.eq.0d0.and.dimag(m12/m32).eq.0d0) then
        cD0gen = cD0p0(q24,q12,q13,q34,q14,q23,m42,m22,m12,m32)
      else

        if(dimag(m12).ne.0d0.or.dimag(m22).ne.0d0.or.
     &       dimag(m32).ne.0d0.or.dimag(m42).ne.0d0) then
          write(*,*) 'D0 for 4 nonzero masses and finite width'
          write(*,*) 'not implemented'
          write(*,*) 'cD0reg: q12 = ',q12
          write(*,*) 'cD0reg: q23 = ',q23
          write(*,*) 'cD0reg: q34 = ',q34
          write(*,*) 'cD0reg: q14 = ',q14
          write(*,*) 'cD0reg: q24 = ',q24
          write(*,*) 'cD0reg: q13 = ',q13
          write(*,*) 'cD0reg: m12 = ',m12
          write(*,*) 'cD0reg: m22 = ',m22
          write(*,*) 'cD0reg: m32 = ',m32
          write(*,*) 'cD0reg: m42 = ',m42
          error_code = 1
        end if

      m1 = sqrt(m12) 
      m2 = sqrt(m22) 
      m3 = sqrt(m32)
      m4 = sqrt(m42) 
      k12 = (m12+m22-q12)/(m1*m2)
      k13 = (m12+m32-q13)/(m1*m3)
      k14 = (m12+m42-q14)/(m1*m4)
      k23 = (m22+m32-q23)/(m2*m3)
      k24 = (m22+m42-q24)/(m2*m4)
      k34 = (m32+m42-q34)/(m3*m4)
      
      if (abs(k13).lt.2d0) then
         if (abs(k24).ge.2d0) then
            swap = k13
            k13  = k24
            k24  = swap
            swap = k14 
            k14  = k23
            k23  = swap
         else if (abs(k12).ge.2d0) then 
            swap = k13
            k13  = k12
            k12  = swap
            swap = k24 
            k24  = k34
            k34  = swap
         else if (abs(k14).ge.2d0) then 
            swap = k13
            k13  = k14
            k14  = swap
            swap = k24 
            k24  = k23
            k23  = swap
         else if (abs(k23).ge.2d0) then 
            swap = k13
            k13  = k23
            k23  = swap
            swap = k24 
            k24  = k14
            k14  = swap
         else if (abs(k34).ge.2d0) then 
            swap = k13
            k13  = k34
            k34  = swap
            swap = k24 
            k24  = k12
            k12  = swap
         end if
      end if

      if (k12.ne.0d0) then
        r12 = k12/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k12**2)))
      else
        r12 = dcmplx(0d0,1d0)
      end if
      if (k14.ne.0d0) then
        r14 = k14/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k14**2)))
      else
        r14 = dcmplx(0d0,1d0)
      end if
      if (k23.ne.0d0) then
        r23 = k23/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k23**2)))
      else
        r23 = dcmplx(0d0,1d0)
      end if
      if (k34.ne.0d0) then
        r34 = k34/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k34**2)))
      else
        r34 = dcmplx(0d0,1d0)
      end if
      if (k13.ne.0d0) then
        r13 = k13/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k13**2)))
      else
        r13 = dcmplx(0d0,1d0)
      end if
c      r13 = 1d0/r13
      if (k24.ne.0d0) then
        r24 = k24/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k24**2)))
      else
        r24 = dcmplx(0d0,1d0)
      end if
c      r24 = 1d0/r24
      a   =  k34/r24-k23 + (k12-k14/r24)*r13
      b   =  (1d0/r13-r13)*(1d0/r24-r24)+k12*k34-k14*k23
      c   =  k34*r24-k23 + (k12-k14*r24)/r13
      d   =  k23-r13*k12 - (k34-r13*k14)*r24
      det = sqrt(b*b-4d0*a*c)
      x(1,4) = (-b+det)/(2d0*a)
      x(2,4) = (-b-det)/(2d0*a)

c      write(*,*) 'cD0gen a ',a,k34,r24,k23,k12,k14,r13

c>      x(1,4) = (-b+sqrt(b*b-4d0*a*c))/(2d0*a)
c>      x(2,4) = (-b-sqrt(b*b-4d0*a*c))/(2d0*a)
      if(abs(x(1,4)).gt.abs(x(2,4))) then
        x(2,4) = c/(a*x(1,4))
      else
        x(1,4) = c/(a*x(2,4))
      end if

      if(dreal(k12).lt.-2d0) then
        ir12 = sign(1d1,1d0-abs(r12))
      else
        ir12 = 0d0
      end if
      if(dreal(k14).lt.-2d0) then
        ir14 = sign(1d1,1d0-abs(r14))
      else
        ir14 = 0d0
      end if
      if(dreal(k23).lt.-2d0) then
        ir23 = sign(1d1,1d0-abs(r23))
      else
        ir23 = 0d0
      end if
      if(dreal(k34).lt.-2d0) then
        ir34 = sign(1d1,1d0-abs(r34))
      else
        ir34 = 0d0
      end if
c      if(k24.le.-2d0) then
        ir24 = sign(1d1,1d0-abs(r24))
c      else
c        ir24 = 0d0
c      end if
c      if(k13.le.-2d0) then
        ir13 = sign(1d1,1d0-abs(r13))
c      else
c        ir13 = 0d0
c      end if
      ir13or24 = sign(1d1,dreal(r24)*ir13-dreal(r13)*ir24)
      

       rd = dreal(d)
c imaginary parts of x(i,4) also needed for x(i,4) < 0 in ettile!!
c      if(dreal(x(1,4)).gt.0d0) then
         ix(1,4) = sign(1d0,-rd)
c      else
c         ix(1,4) = 0d0
c      end if
c      if(dreal(x(2,4)).gt.0d0) then
         ix(2,4) = sign(1d0,rd)
c      else
c         ix(2,4) = 0d0
c      end if
c --<

c>      write(*,*) 'cD0gen q ',q12,q23,q34
c>      write(*,*) 'cD0gen q ',q14,q13,q24
c>      write(*,*) 'cD0gen m ',m12,m22,m32,m42
c>      write(*,*) 'cD0gen k ',k12,k13,k14
c>      write(*,*) 'cD0gen k ',k23,k24,k34
c>      write(*,*) 'cD0gen r ',r12,r13,r14
c>      write(*,*) 'cD0gen r ',r23,r24,r34
c>      write(*,*) 'cD0gen ir ',ir12,ir13,ir14
c>      write(*,*) 'cD0gen ir ',ir23,ir24,ir34
c>      write(*,*) 'cD0gen a ',a,b,c,d
c>      write(*,*) 'cD0gen x ',x(1,4),x(2,4)
c>      write(*,*) 'cD0gen ix ',ix(1,4),ix(2,4)

      x(1,1) = x(1,4)/r24
      x(2,1) = x(2,4)/r24
      x(1,2) = x(1,4)/r24*r13
      x(2,2) = x(2,4)/r24*r13
      x(1,3) = x(1,4)*r13
      x(2,3) = x(2,4)*r13
      s(1)  = r12
      s(2)  = r23
      s(3)  = r34
      s(4)  = r14

      is(1)  = ir12
      is(2)  = ir23
      is(3)  = ir34
      is(4)  = ir14

      if(dreal(x(1,1)).gt.0d0) then
         ix(1,1) = sign(1d0,-rd/dreal(r24))
      else
         ix(1,1) = 0d0
      end if
      if(dreal(x(2,1)).gt.0d0) then
         ix(2,1) = sign(1d0,rd/dreal(r24))
      else
         ix(2,1) = 0d0
      end if
      if(dreal(x(1,3)).gt.0d0) then
         ix(1,3) = sign(1d0,-rd*dreal(r13))
      else
         ix(1,3) = 0d0
      end if
      if(dreal(x(2,3)).gt.0d0) then
         ix(2,3) = sign(1d0,rd*dreal(r13))
      else
         ix(2,3) = 0d0
      end if
      if(dreal(x(1,2)).gt.0d0) then
         ix(1,2) = sign(1d0,-rd*dreal(r13/r24))
      else
         ix(1,2) = 0d0
      end if
      if(dreal(x(2,2)).gt.0d0) then
         ix(2,2) = sign(1d0,rd*dreal(r13/r24))
      else
         ix(2,2) = 0d0
      end if
c      gamma(1) = dsign(1d0,dreal(a*(x(1,4)-x(2,4))))
      gamma(1) = dsign(1d0,dreal(det))
      gamma(2) = -gamma(1)

      cD0gen = dcmplx(0d0)
c      do 20 k=2,1,-1
      do 20 k=1,2
        do 10 j=1,4
          cD0gen = cD0gen + (-1)**(j+k) * (
     &           cspcoe(-x(k,j),s(j),-ix(k,j),is(j))
     &         + cspcoe(-x(k,j),1d0/s(j),-ix(k,j),-is(j)) )

c>          write(*,*) 'cD0gen ',k,j,x(k,j),s(j),ix(k,j),is(j)
c>          write(*,*) 'cD0gen ',
c>     &         cspcoe(-x(k,j),s(j),-ix(k,j),is(j)) 
c>     &         , cspcoe(-x(k,j),1d0/s(j),-ix(k,j),-is(j)) 
c>          write(*,*) 'cD0gen ', (-1)**(j+k) * (
c>     &         cspcoe(-x(k,j),s(j),-ix(k,j),is(j)) 
c>     &         + cspcoe(-x(k,j),1d0/s(j),-ix(k,j),-is(j)) )
c>          write(*,*) 'cD0gen= ',k,j, cD0gen

10      continue

c        write(*,*) 'cD0gen sp = ',cD0gen

c *** improves numerical stability in extreme cases

        p12 = r24/x(k,4)+k12+x(k,4)/r24
        p14 = 1/x(k,4)+k14+x(k,4)
        p23 = r24/(x(k,4)*r13)+k23+x(k,4)*r13/r24
        p34 = 1/(r13*x(k,4))+k34+x(k,4)*r13
        q1  = (k34-r13*k14)+(1/r13-r13)/x(k,4)
        q2  = k23-r13*k12+(1/r13-r13)*r24/x(k,4)
        q3  = ((k23-r24*k34)-(r24-1d0/r24)*r13*x(k,4))/d
        q4  = ((k12-r24*k14)-(r24-1d0/r24)*x(k,4))/d

c        write(*,*) 'cD0gen ',q1,p34/q3,p14/q4
c        write(*,*) 'cD0gen ',q2,p12/q4,p23/q3
c        write(*,*) 'cD0gen ',q3,p23/q2,p34/q1
c        write(*,*) 'cD0gen ',q4,p12/q2,p14/q1
c        write(*,*) 'cD0gen ',p12,q4*q2
c        write(*,*) 'cD0gen ',p14,q4*q1
c        write(*,*) 'cD0gen ',p23,q3*q2
c        write(*,*) 'cD0gen ',p34,q3*q1

        t12 = abs(p12)/max(abs(r24/x(k,4)),abs(k12),abs(x(k,4)/r24))
        t14 = abs(p14)/
     &        max(abs(1/x(k,4)),abs(k14),abs(x(k,4)))
        t23 = abs(p23)/
     &        max(abs(r24/(r13*x(k,4))),abs(k23),abs(x(k,4)*r13/r24))
        t34 = abs(p34)/
     &        max(abs(1/(r13*x(k,4))),abs(k34),abs(x(k,4)*r13))
        t1  = abs(q1)/
     &      max(abs(k34*x(k,4)),abs(r13),abs(r13*k14*x(k,4)),abs(1/r13))
        t2  = abs(q2)/max(abs(k23),abs(r13*k12),
     &                     abs(r24*r13/x(k,4)),abs(r24/r13/x(k,4)))
        t3  = abs(q3)/max(abs(k23),abs(r24*k34),
     &                     abs(r24*r13*x(k,4)),abs(r13/r24*x(k,4)))
        t4  = abs(q4)/
     &      max(abs(k12/x(k,4)),abs(r24),abs(r24*k14/x(k,4)),abs(1/r24))

        if (min(t12,t2).gt.min(t14,t1).and.min(t12,t2).gt.t4) q4 =p12/q2
        if (min(t14,t1).gt.min(t12,t2).and.min(t14,t1).gt.t4) q4 =p14/q1
        if (min(t23,t2).gt.min(t34,t1).and.min(t23,t2).gt.t3) q3 =p23/q2
        if (min(t34,t1).gt.min(t23,t2).and.min(t34,t1).gt.t3) q3 =p34/q1
        if (min(t34,t3).gt.min(t14,t4).and.min(t34,t3).gt.t1) q1 =p34/q3
        if (min(t14,t4).gt.min(t34,t3).and.min(t14,t4).gt.t1) q1 =p14/q4
        if (min(t12,t4).gt.min(t23,t3).and.min(t12,t4).gt.t2) q2 =p12/q4
        if (min(t23,t3).gt.min(t12,t4).and.min(t23,t3).gt.t1) q2 =p23/q3
        if (min(t4,t2).gt.t12) p12=q4*q2
        if (min(t4,t1).gt.t14) p14=q4*q1
        if (min(t3,t2).gt.t23) p23=q3*q2
        if (min(t3,t1).gt.t34) p34=q3*q1


c ***
      
      if (dimag(r13).eq.0d0) then

        l1 = cdln(q1,-1d0)
        l3 = cdln(q3,sign(1d0,gamma(k)*dreal(r13)*ir24))
        l2 = cdln(q2,-1d0)
        l4 = cdln(q4,sign(1d0,gamma(k)*ir24))

c        l1 = cdln((k13-2*r13)/x(k,4)+k34-r13*k14,-1d0)
c        l3 = cdln(((k24-2*r24)*r13*x(k,4)+k23-r24*k34)/d,
c     &            dsign(1d0,gamma(k)*dreal(r13)*ir24))
c        l2 = cdln((k13-2*r13)*r24/x(k,4)+k23-r13*k12,-1d0)
c        l4 = cdln(((k24-2*r24)*x(k,4)+k12-r24*k14)/d,
c     &            dsign(1d0,gamma(k)*ir24))
        
        cD0gen = cD0gen - (-1)**k *( 
     &            ettile(-x(k,4),r13,-ix(k,4),ir13) *
     &            (  cdln(r13*x(k,4),sign(1d0,dreal(r13))*ix(k,4)) 
     &               + l1 + l3)
     &          + ettile(-x(k,4),1/r24,-ix(k,4),-ir24) *
     &            (  cdln(x(k,4)/r24,sign(1d0,dreal(r24))*ix(k,4)) 
     &               + l2 + l4)
     &          - (ettile(-x(k,4),r13/r24,-ix(k,4),ir13or24) 
     &             + olietae(r13,1/r24,ir13,-ir24,ir13or24)) *
     &            (cdln(x(k,4)*r13/r24,sign(1d0,dreal(r13/r24))*ix(k,4)) 
     &               + l3 + l2)
     &          + ettile(-x(k,4),-r13/r24,-ix(k,4),-ir13or24) 
     &            * olietae(r13,1/r24,ir13,-ir24,ir13or24) 
     &             )

c>        write(*,*) 'cD0gen a ',
c>     &            ettile(-x(k,4),r13,-ix(k,4),ir13) *
c>     &            (  cdln(r13*x(k,4),sign(1d0,dreal(r13))*ix(k,4)) 
c>     &               + l1 + l3)
c>        write(*,*) 'cD0gen b ',
c>     &          + ettile(-x(k,4),1/r24,-ix(k,4),-ir24) *
c>     &            (  cdln(x(k,4)/r24,sign(1d0,dreal(r24))*ix(k,4)) 
c>     &               + l2 + l4)
c>        write(*,*) 'cD0gen b ',
c>     &          + ettile(-x(k,4),1/r24,-ix(k,4),-ir24) ,
c>     &              cdln(x(k,4)/r24,sign(1d0,dreal(r24))*ix(k,4)) 
c>     &              , + l2, + l4
c>        write(*,*) 'cD0gen c ',
c>     &          - (ettile(-x(k,4),r13/r24,-ix(k,4),ir13or24) 
c>     &             + olietae(r13,1/r24,ir13,-ir24,ir13or24)) *
c>     &            (cdln(x(k,4)*r13/r24,sign(1d0,dreal(r13/r24))*ix(k,4)) 
c>     &               + l3 + l2)
c>        write(*,*) 'cD0gen d ',
c>     &          + ettile(-x(k,4),-r13/r24,-ix(k,4),-ir13or24) 
c>     &            * olietae(r13,1/r24,ir13,-ir24,ir13or24) 
c>
c>        write(*,*) 'cD0gen real r= ',cD0gen
       
      else

c branch not yet fully tested!!
        l1 = cdln(p34,-sign(1d0,gamma(k)*dimag(r13*x(k,4)*b)))
        l2 = cdln(p12,-sign(1d0,gamma(k)*dimag(x(k,4)/r24*b)))
        l3 = cdln(p23,-sign(1d0,gamma(k)*dimag(r13*x(k,4)/r24*b)))
c        l1 = cdln(1d0/(r13*x(k,4))+k34+r13*x(k,4),
c     &             -dsign(1d0,gamma(k)*dimag(r13*x(k,4)*b)))
c        l2 = cdln(r24/x(k,4)+k12+x(k,4)/r24,
c     &             -dsign(1d0,gamma(k)*dimag(x(k,4)/r24*b)))
c        l3 = cdln(r24/(r13*x(k,4))+k23+r13*x(k,4)/r24,
c     &             -dsign(1d0,gamma(k)*dimag(r13*x(k,4)/r24*b)))
c        write(*,*) 'cD0gen l ',l1,l2,l3
        cD0gen = cD0gen - (-1)**k *( 
     &            olietae(-x(k,4),r13,-ix(k,4),ir13,0d0) *
     &            (  cdln(r13*x(k,4),sign(1d0,dreal(r13))*ix(k,4)) 
     &               + l1)
     &          + olietae(-x(k,4),1/r24,-ix(k,4),-ir24,0d0) *
     &            (  cdln(x(k,4)/r24,sign(1d0,ix(k,4)/dreal(r24))) 
     &               + l2)
     &          - (olietae(-x(k,4),r13/r24,-ix(k,4),ir13-ir24,0d0) 
     &             + olietae(r13,1/r24,ir13,-ir24,ir13-ir24)) *
     &           (cdln(x(k,4)*r13/r24,sign(1d0,dreal(r13/r24))*ix(k,4)) 
     &               + l3 + l2)
c--> changed 21.06.05
c     &          + (olietae(-x(k,4),-r13/r24,-ix(k,4),-ir13+ir24,0d0) 
c     &             * olietae(r13,1/r24,ir13,-ir24,ir13-ir24)) *
c     &               (1-gamma(k)*sign(1d0,dreal(b)))
     &             )
        if ((1-gamma(k)*sign(1d0,dreal(b))).ne.0.and.
     &       olietae(r13,1/r24,ir13,-ir24,ir13-ir24).ne.0d0) then  
        cD0gen = cD0gen - (-1)**k *( 
     &          + (olietae(-x(k,4),-r13/r24,-ix(k,4),-ir13+ir24,0d0) 
     &             * olietae(r13,1/r24,ir13,-ir24,ir13-ir24)) *
     &               (1-gamma(k)*sign(1d0,dreal(b)))
     &             )
        end if
c <-- changed 21.06.05

c>         write(*,*) 'cD0gen eta a',
c>     &            olietae(-x(k,4),r13,-ix(k,4),ir13,0d0) *
c>     &            (  cdln(r13*x(k,4),sign(1d0,dreal(r13))*ix(k,4)) 
c>     &               + l1)
c>         write(*,*) 'cD0gen eta b',
c>     &          + olietae(-x(k,4),1/r24,-ix(k,4),-ir24,0d0) *
c>     &            (  cdln(x(k,4)/r24,sign(1d0,ix(k,4)/dreal(r24))) 
c>     &               + l2)
c>         write(*,*) 'cD0gen eta c',
c>     &          - (olietae(-x(k,4),r13/r24,-ix(k,4),ir13-ir24,0d0) 
c>     &             + olietae(r13,1/r24,ir13,-ir24,ir13-ir24)) *
c>     &           (cdln(x(k,4)*r13/r24,sign(1d0,dreal(r13/r24))*ix(k,4)) 
c>     &               + l3 + l2)
c>        if ((1-gamma(k)*sign(1d0,dreal(b))).ne.0.and.
c>     &      olietae(r13,1/r24,ir13,-ir24,ir13-ir24).ne.0d0) then  
c>         write(*,*) 'cD0gen eta d',
c>     &          + (olietae(-x(k,4),-r13/r24,-ix(k,4),-ir13+ir24,0d0) 
c>     &             * olietae(r13,1/r24,ir13,-ir24,ir13-ir24)) *
c>     &               (1-gamma(k)*sign(1d0,dreal(b)))
c>        end if
c>        write(*,*) 'cD0gen imr ',cD0gen
       
        end if

20    continue

c      write(*,*) 'cD0gen sum ',cD0gen

      cD0gen = cD0gen/(m1*m2*m3*m4*sqrt(b*b-4d0*a*c))

c      write(*,*) 'cD0gen =   ',cD0gen    

      end if
      end

************************************************************************
      function cD0p0(q12i,q23i,q34i,q14i,q13i,q24i,m12i,m22i,m32i,m42i)
************************************************************************
*  general scalar 4-point function                                     *
*  case with one zero external invariant                               *
*  based on equation (23) of                                           *
*        A.Denner, U.Nierste and R.Scharf, Nucl. Phys. B367 (1991) 637 *
*                                                                      *
*                     m22                                              *
*       q12  ---------------------  q23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | m32                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       q14  ---------------------  q34 = 0                            *
*                     m42                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  24.01.07 Ansgar Denner       last changed 16.06.08 Ansgar Denner    *
************************************************************************
      implicit   none
      complex*16 q12,q23,q34,q14,q13,q24
      complex*16 m12,m22,m32,m42
      complex*16 q12i,q23i,q34i,q14i,q13i,q24i
      complex*16 m12i,m22i,m32i,m42i
      complex*16 k12,k13,k14,k23,k24,k34
      complex*16 l12,l13,l14,l23,l24,l34
      complex*16 m1,m2,m3,m4
      real*8     ir12,ir14,ir23,ir34,ir13
      real*8     iy(2),iz(2)
      complex*16 r12,r13,r14,r23,r24,r34
      complex*16 r21,r31,r41,r32,r42,r43
      complex*16 a,b,c,d,det
      real*8     rd
      complex*16 x(2),y(2),z(2),eta,v
      complex*16 p12,p34,p14,p23
      real*8     t12,t34,t14,t23
      complex*16 cD0p0,cspcoe,olietae,cdln,cD0m0,cD0p0o
      integer    k
      complex*16 cd0
      data       cd0 /(0d0,0d0)/

      integer    ltest
      common /ltest/ ltest
      integer    flag
      data       flag /0/
      save       flag
      integer    errflag
      save       errflag
      data       errflag /0/
      integer    errout
      data       errout /90/
      real*8     testaccd0
      common /testaccd0/ testaccd0

      integer error_code
      common /coli_error/ error_code
      save /coli_error/

c      write(*,*) 'cD0gen in ',q12,q23,q34,q14,q13,q24,m12,m22,m32,m42

      if (flag.eq.0) then
        write(*,*) 
        write(*,*) 'cD0p0 not yet tested in practice'
        flag = 1
      end if 

      if (m12i.eq.0d0) then
        cD0p0=cD0m0(q23i,q12i,q14i,q34i,q13i,q24i,m32i,m22i,cd0,m42i)
      else if (m22i.eq.0d0) then
        cD0p0=cD0m0(q14i,q12i,q23i,q34i,q24i,q13i,m42i,m12i,cd0,m32i)
      else if (m32i.eq.0d0) then
        cD0p0=cD0m0(q12i,q23i,q34i,q14i,q13i,q24i,m12i,m22i,cd0,m42i)
      else if (m42i.eq.0d0) then
        cD0p0=cD0m0(q23i,q34i,q14i,q12i,q24i,q13i,m22i,m32i,cd0,m12i)
      else

        if(dimag(m42i/m32i).ne.0d0.or.q34i.ne.0d0) then
          write(*,*) 'cD0p0 called improperly:'
          write(*,*) 'cD0p0: q12 = ',q12i
          write(*,*) 'cD0p0: q23 = ',q23i
          write(*,*) 'cD0p0: q34 = ',q34i
          write(*,*) 'cD0p0: q14 = ',q14i
          write(*,*) 'cD0p0: q24 = ',q24i
          write(*,*) 'cD0p0: q13 = ',q13i
          write(*,*) 'cD0p0: m12 = ',m12i
          write(*,*) 'cD0p0: m22 = ',m22i
          write(*,*) 'cD0p0: m32 = ',m32i
          write(*,*) 'cD0p0: m42 = ',m42i
          error_code = 1
        end if

        q13=q12i
        q23=q23i
        q24=q34i
        q14=q14i
        q12=q13i
        q34=q24i
        m12=m12i
        m32=m22i
        m22=m32i
        m42=m42i

        l12 = (m12+m22-q12)
        l13 = (m12+m32-q13)
        l14 = (m12+m42-q14)
        l23 = (m22+m32-q23)
        l24 = (m22+m42-q24)
        l34 = (m32+m42-q34)      
        
        if (l12.ne.0d0) then
          r12 = l12/(2d0*m22)*(1d0+sqrt(1d0-4d0*m12*m22/l12**2))
        else
          r12 = sqrt(m12/m22)*dcmplx(0d0,1d0)
        end if
        r21= r12*m22/m12
        if (l14.ne.0d0) then
          r14 = l14/(2d0*m42)*(1d0+sqrt(1d0-4d0*m12*m42/l14**2))
        else
          r14 = sqrt(m12/m42)*dcmplx(0d0,1d0)
        end if
        r41 = r14*m42/m12
        if (l34.ne.0d0) then
          r34 = l34/(2d0*m42)*(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
        else
          r34 = sqrt(m12/m42)*dcmplx(0d0,1d0)
        end if
        r43 = r34*m42/m32
        if (l23.ne.0d0) then
          r23 = l23/(2d0*m32)*(1d0+sqrt(1d0-4d0*m22*m32/l23**2))
        else
          r23 = sqrt(m22/m32)*dcmplx(0d0,1d0)
        end if
        r32 = r23*m32/m22
        if (l13.ne.0d0) then
          r13 = l13/(2d0*m32)*(1d0+sqrt(1d0-4d0*m12*m32/l13**2))
        else
          r13 = sqrt(m12/m32)*dcmplx(0d0,1d0)
        end if
        r31 = r13*m32/m12
c       r13 = 1d0/r13
        r24=1d0
        r42=m42/m22
        a   =  m42*((l34/r42-l23) + (l12-l14/r42)*r31)
        b   =  m32*(1d0/r31-r13)*m42*(1d0/r42-r24)+l12*l34-l14*l23
        c   =  m12*((l34*r24-l23) + (l12-l14*r24)/r13)
        d   =  m12*(l23-r31*l12 - (l34-r31*l14)*r24)
        det = sqrt(b*b-4d0*a*c)
        y(1) = (-b+det)/(2d0*a)
        y(2) = (-b-det)/(2d0*a)
        if(abs(y(1)).gt.abs(y(2))) then
          y(2) = c/(a*y(1))
        else
          y(1) = c/(a*y(2))
        end if

        if(dreal(l12).lt.0d0) then
          ir12 = sign(1d1,1d0-abs(r12*r12*m22/m12))
        else
          ir12 = 0d0
        end if
        if(dreal(l14).lt.0d0) then
          ir14 = sign(1d1,1d0-abs(r14*r14*m42/m12))
        else
          ir14 = 0d0
        end if
        if(dreal(l23).lt.0d0) then
          ir23 = sign(1d1,1d0-abs(r23*r23*m32/m22))
        else
          ir23 = 0d0
        end if
        if(dreal(l34).lt.0d0) then
          ir34 = sign(1d1,1d0-abs(r34*r34*m42/m32))
        else
          ir34 = 0d0
        end if
c       if(k13.le.-2d0) then
        ir13 = sign(1d1,1d0-abs(r13*r13*m32/m12))
c       else
c         ir13 = 0d0
c       end if

c>        write(*,*) 'cD0p0 q ',q12,q23,q34
c>        write(*,*) 'cD0p0 q ',q14,q13,q24
c>        write(*,*) 'cD0p0 m ',m12,m22,m32,m42
c>        write(*,*) 'cD0p0 l ',l12,l13,l14
c>        write(*,*) 'cD0p0 l ',l23,l24,l34
c>        write(*,*) 'cD0p0 r ',r12,r13,r14
c>        write(*,*) 'cD0p0 r ',r21,r31,r41
c>        write(*,*) 'cD0p0 r ',r23,r24,r34
c>        write(*,*) 'cD0p0 r ',r32,r42,r43
c>        write(*,*) 'cD0p0 ir',ir12,ir13,ir14
c>        write(*,*) 'cD0p0 ir',ir23,ir24,ir34
c>        write(*,*) 'cD0p0 a ',a*m42*m2*m3,b*m1*m2*m3*m4,c*m12*m2*m3,
c>     &      d*m12*m2*m3
c>        write(*,*) 'cD0p0 a ',a,b,c,d
c>        write(*,*) 'cD0p0 x ',x(1),x(2)
c>        write(*,*) 'cD0p0 ix',ix(1),ix(2)
        
        z(1) = y(1)*r31
        z(2) = y(2)*r31
        
c     result should be independent of imaginary parts of y and z!!!!
        rd = dreal(d)
        if(dreal(y(1)).gt.0d0) then
          iy(1) = sign(1d0,-rd)
        else
          iy(1) = 0d0
        end if
        if(dreal(y(2)).gt.0d0) then
          iy(2) = sign(1d0,rd)
        else
          iy(2) = 0d0
        end if
        if(dreal(z(1)).gt.0d0) then
          iz(1) = sign(1d0,-rd*dreal(r13))
        else
          iz(1) = 0d0
        end if
        if(dreal(z(2)).gt.0d0) then
          iz(2) = -sign(1d0,rd*dreal(r13))
        else
          iz(2) = 0d0
        end if
        
c>        write(*,*) 'cD0p0 y ',y(1),y(2)
c>        write(*,*) 'cD0p0 iy',iy(1),iy(2)
c>        write(*,*) 'cD0p0 z',z(1),z(2)
c>        write(*,*) 'cD0p0 iz',iz(1),iz(2)
        
        cD0p0 = dcmplx(0d0)
c       do k=2,1,-1
        do k=1,2
          cD0p0 = cD0p0 - (-1)**k * (
     &        cspcoe(-y(k),1d0/r12,-iy(k),-ir12)
     &        +cspcoe(-y(k),r21,-iy(k),ir12)
     &        -cspcoe(-y(k),1d0/r14,-iy(k),-ir14)
     &        -cspcoe(-y(k),r41,-iy(k),ir14)
     &        +cspcoe(-z(k),1d0/r34,-iz(k),-ir34)
     &        +cspcoe(-z(k),r43,-iz(k),ir34)
     &        -cspcoe(-z(k),1d0/r32,-iz(k),-ir23)
     &        -cspcoe(-z(k),r23,-iz(k),ir23)
     &        )          
          
c>          write(*,*) 'cD0p0 args yz',
c>     &        -y(k),-iy(k),-z(k),-iz(k)
c>          write(*,*) 'cD0p0 args r',
c>     &        1d0/r12,r21,1d0/(r14),r41,
c>     &        1d0/r34,r43,1d0/(r32),r23
c>          write(*,*) 'cD0p0 args ',
c>     &        1d0+y(k)/(r12),
c>     &        1d0+y(k)*r21,
c>     &        1d0+y(k)/r14,
c>     &        1d0+y(k)*r41,
c>     &        1d0+z(k)/r34,
c>     &        1d0+z(k)*r43,
c>     &        1d0+z(k)/r32,
c>     &        1d0+z(k)*r23
c>          write(*,*) 'cD0p0 csp ',
c>     &        cspcoe(-y(k),1d0/r12,-iy(k),-ir12),
c>     &        +cspcoe(-y(k),r21,-iy(k),ir12),
c>     &        -cspcoe(-y(k),1d0/r14,-iy(k),-ir14),
c>     &        -cspcoe(-y(k),r41,-iy(k),ir14),
c>     &        +cspcoe(-z(k),1d0/r34,-iz(k),-ir34),
c>     &        +cspcoe(-z(k),r43,-iz(k),ir34),
c>     &        -cspcoe(-z(k),1d0/r32,-iz(k),-ir23),
c>     &        -cspcoe(-z(k),r23,-iz(k),ir23)
c>          
c>          write(*,*) 'cD0p0 cspsum ',cD0p0
                    
c     *** improves numerical stability in extreme cases
          
          eta=olietae(-y(k),-1/z(k),-iy(k),iz(k),-ir13)
          
          if(eta.ne.0d0)then
            p14 = m12+m42*y(k)*y(k)+l14*y(k)
            p12 = m12+m22*y(k)*y(k)+l12*y(k)
            p34 = m32+m42*z(k)*z(k)+l34*z(k)
            p23 = m32+m22*z(k)*z(k)+l23*z(k)
            
            t14=abs(p14)/max(abs(m12),abs(m42*y(k)*y(k)),abs(l14*y(k)))
            t12=abs(p12)/max(abs(m12),abs(m22*y(k)*y(k)),abs(l12*y(k)))
            t34=abs(p34)/max(abs(m32),abs(m42*z(k)*z(k)),abs(l34*z(k)))
            t23=abs(p23)/max(abs(m32),abs(m22*z(k)*z(k)),abs(l23*z(k)))
            
            if (min(t12,t14).gt.min(t34,t23)) then
              if(dimag(p14/p12).ne.0d0)then
                v= cdln(p14/p12,0d0)
              else
                v=cdln(p14,-1d0)-cdln(p12,-1d0)
              endif
            else
              if(dimag(p34/p23).ne.0d0)then
                v= cdln(p34/p23,0d0)
              else
                v=cdln(p34,-1d0)-cdln(p23,-1d0)
              endif
            endif

c>            write(*,*) 'cD0p0 v',p14/p12,p34/p23              
c>            write(*,*) 'cD0p0 v',cdln(p14,-1d0)-cdln(p12,-1d0),
c>     &          cdln(p34,-1d0)-cdln(p23,-1d0)
            
            cD0p0 = cD0p0 
     &          - (-1)**k *eta*v

c>            write(*,*) 'cD0p0 etacon ', - (-1)**k *v,eta
c>            write(*,*) 'cD0p0 etasum ',cD0p0
          endif
        enddo
        
c        write(*,*) 'cD0p0 sum ',cD0p0

        cD0p0 = cD0p0/det

c        write(*,*) 'cD0p0 =   ',cD0p0    
        
c end new version

        if (ltest.lt.10) return

c begin old version

        m1 = sqrt(m12) 
        m2 = sqrt(m22) 
        m3 = sqrt(m32)
        m4 = sqrt(m42) 
        l12 = (m12+m22-q12)
        l13 = (m12+m32-q13)
        l14 = (m12+m42-q14)
        l23 = (m22+m32-q23)
        l24 = (m22+m42-q24)
        l34 = (m32+m42-q34)
        k12 = (m12+m22-q12)/(m1*m2)
        k13 = (m12+m32-q13)/(m1*m3)
        k14 = (m12+m42-q14)/(m1*m4)
        k23 = (m22+m32-q23)/(m2*m3)
        k24 = (m22+m42-q24)/(m2*m4)
        k34 = (m32+m42-q34)/(m3*m4)
        
        
        if (k12.ne.0d0) then
          r12 = k12/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k12**2)))
        else
          r12 = dcmplx(0d0,1d0)
        end if
        if (k14.ne.0d0) then
          r14 = k14/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k14**2)))
        else
          r14 = dcmplx(0d0,1d0)
        end if
        if (k23.ne.0d0) then
          r23 = k23/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k23**2)))
        else
          r23 = dcmplx(0d0,1d0)
        end if
        if (k34.ne.0d0) then
          r34 = k34/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k34**2)))
        else
          r34 = dcmplx(0d0,1d0)
        end if
        if (k13.ne.0d0) then
          r13 = k13/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k13**2)))
        else
          r13 = dcmplx(0d0,1d0)
        end if
c     r13 = 1d0/r13
        r24=m4/m2
        a   =  k34/r24-k23 + (k12-k14/r24)*r13
        b   =  (1d0/r13-r13)*(1d0/r24-r24)+k12*k34-k14*k23
        c   =  k34*r24-k23 + (k12-k14*r24)/r13
        d   =  k23-r13*k12 - (k34-r13*k14)*r24
        det = sqrt(b*b-4d0*a*c)
        x(1) = (-b+det)/(2d0*a)
        x(2) = (-b-det)/(2d0*a)
c>      x(1) = (-b+sqrt(b*b-4d0*a*c))/(2d0*a)
c>      x(2) = (-b-sqrt(b*b-4d0*a*c))/(2d0*a)
        if(abs(x(1)).gt.abs(x(2))) then
          x(2) = c/(a*x(1))
        else
          x(1) = c/(a*x(2))
        end if
        
        if(dreal(k12).lt.-2d0) then
          ir12 = sign(1d1,1d0-abs(r12))
        else
          ir12 = 0d0
        end if
        if(dreal(k14).lt.-2d0) then
          ir14 = sign(1d1,1d0-abs(r14))
        else
          ir14 = 0d0
        end if
        if(dreal(k23).lt.-2d0) then
          ir23 = sign(1d1,1d0-abs(r23))
        else
          ir23 = 0d0
        end if
        if(dreal(k34).lt.-2d0) then
          ir34 = sign(1d1,1d0-abs(r34))
        else
          ir34 = 0d0
        end if
c       if(k13.le.-2d0) then
        ir13 = sign(1d1,1d0-abs(r13))
c       else
c         ir13 = 0d0
c       end if

c>        write(*,*) 'cD0p0o q ',q12,q23,q34
c>        write(*,*) 'cD0p0o q ',q14,q13,q24
c>        write(*,*) 'cD0p0o m ',m12,m22,m32,m42
c>        write(*,*) 'cD0p0o k ',k12,k13,k14
c>        write(*,*) 'cD0p0o k ',k23,k24,k34
c>        write(*,*) 'cD0p0o k ',l12,l13,l14
c>        write(*,*) 'cD0p0o k ',l23,l24,l34
c>        write(*,*) 'cD0p0o r ',r12,r13,r14
c>        write(*,*) 'cD0p0o r ',r23,r24,r34
c>        write(*,*) 'cD0p0o r ',r12*m1/m2,r13*m1/m3,r14*m1/m4
c>        write(*,*) 'cD0p0o r ',r12*m2/m1,r13*m3/m1,r14*m4/m1
c>        write(*,*) 'cD0p0o r ',r23*m2/m3,r24*m2/m4,r34*m3/m4
c>        write(*,*) 'cD0p0o r ',r23*m3/m2,r24*m4/m2,r34*m4/m3
c>        write(*,*) 'cD0p0o ir',ir12,ir13,ir14
c>        write(*,*) 'cD0p0o ir',ir23,ir24,ir34
c>        write(*,*) 'cD0p0o a ',a,b,c,d
c>        write(*,*) 'cD0p0o x ',x(1),x(2)
c>        write(*,*) 'cD0p0o ix',ix(1),ix(2)
        
        y(1) = x(1)*m1/m4
        y(2) = x(2)*m1/m4
        z(1) = x(1)*m3/m4*r13
        z(2) = x(2)*m3/m4*r13
        
c     result should be independent of imaginary parts of y and z!!!!
        rd = dreal(d)
        if(dreal(y(1)).gt.0d0) then
          iy(1) = sign(1d0,-rd)
        else
          iy(1) = 0d0
        end if
        if(dreal(y(2)).gt.0d0) then
          iy(2) = sign(1d0,rd)
        else
          iy(2) = 0d0
        end if
        if(dreal(z(1)).gt.0d0) then
          iz(1) = sign(1d0,-rd*dreal(r13))
        else
          iz(1) = 0d0
        end if
        if(dreal(z(2)).gt.0d0) then
          iz(2) = -sign(1d0,rd*dreal(r13))
        else
          iz(2) = 0d0
        end if
        
c>        write(*,*) 'cD0p0o y ',y(1),y(2)
c>        write(*,*) 'cD0p0o iy',iy(1),iy(2)
c>        write(*,*) 'cD0p0o z',z(1),z(2)
c>        write(*,*) 'cD0p0o iz',iz(1),iz(2)
        
        cD0p0o = dcmplx(0d0)
c     do k=2,1,-1
        do k=1,2
          cD0p0o = cD0p0o - (-1)**k * (
     &        cspcoe(-y(k),m2/(m1*r12),-iy(k),-ir12)
     &        +cspcoe(-y(k),m2/m1*r12,-iy(k),ir12)
     &        -cspcoe(-y(k),m4/(m1*r14),-iy(k),-ir14)
     &        -cspcoe(-y(k),m4/m1*r14,-iy(k),ir14)
     &        +cspcoe(-z(k),m4/(m3*r34),-iz(k),-ir34)
     &        +cspcoe(-z(k),m4/m3*r34,-iz(k),ir34)
     &        -cspcoe(-z(k),m2/(m3*r23),-iz(k),-ir23)
     &        -cspcoe(-z(k),m2/m3*r23,-iz(k),ir23)
     &      )          
          
c>          write(*,*) 'cD0p0o args yz',
c>     &        -y(k),-iy(k),-z(k),-iz(k)
c>          write(*,*) 'cD0p0o args r',
c>     &        m2/(m1*r12),m2/m1*r12,m4/(m1*r14),m4/m1*r14,
c>     &        m4/(m3*r34),m4/m3*r34,m2/(m3*r23),m2/m3*r23
c>          write(*,*) 'cD0p0o args ',
c>     &        1d0+y(k)*m2/(m1*r12),
c>     &        1d0+y(k)*m2/m1*r12,
c>     &        1d0+y(k)*m4/(m1*r14),
c>     &        1d0+y(k)*m4/m1*r14,
c>     &        1d0+z(k)*m4/(m3*r34),
c>     &        1d0+z(k)*m4/m3*r34,
c>     &        1d0+z(k)*m2/(m3*r23),
c>     &        1d0+z(k)*m2/m3*r23
c>          write(*,*) 'cD0p0o csp ',
c>     &        cspcoe(-y(k),m2/(m1*r12),-iy(k),-ir12),
c>     &        +cspcoe(-y(k),m2/m1*r12,-iy(k),ir12),
c>     &        -cspcoe(-y(k),m4/(m1*r14),-iy(k),-ir14),
c>     &        -cspcoe(-y(k),m4/m1*r14,-iy(k),ir14),
c>     &        +cspcoe(-z(k),m4/(m3*r34),-iz(k),-ir34),
c>     &        +cspcoe(-z(k),m4/m3*r34,-iz(k),ir34),
c>     &        -cspcoe(-z(k),m2/(m3*r23),-iz(k),-ir23),
c>     &        -cspcoe(-z(k),m2/m3*r23,-iz(k),ir23)
c>
c>          write(*,*) 'cD0p0o cspsum ',cD0p0o
                    
c     *** improves numerical stability in extreme cases
          
          eta=olietae(-y(k),-1/z(k),-iy(k),iz(k),-ir13)
          
          if(eta.ne.0d0)then
            p14 = m12+m42*y(k)*y(k)+l14*y(k)
            p12 = m12+m22*y(k)*y(k)+l12*y(k)
            p34 = m32+m42*z(k)*z(k)+l34*z(k)
            p23 = m32+m22*z(k)*z(k)+l23*z(k)
            
            t14=abs(p14)/max(abs(m12),abs(m42*y(k)*y(k)),abs(l14*y(k)))
            t12=abs(p12)/max(abs(m12),abs(m22*y(k)*y(k)),abs(l12*y(k)))
            t34=abs(p34)/max(abs(m32),abs(m42*z(k)*z(k)),abs(l34*z(k)))
            t23=abs(p23)/max(abs(m32),abs(m22*z(k)*z(k)),abs(l23*z(k)))
            
            if (min(t12,t14).gt.min(t34,t23)) then
              if(dimag(p14/p12).ne.0d0)then
                v= cdln(p14/p12,0d0)
              else
                v=cdln(p14,-1d0)-cdln(p12,-1d0)
              endif
            else
              if(dimag(p34/p23).ne.0d0)then
                v= cdln(p34/p23,0d0)
              else
                v=cdln(p34,-1d0)-cdln(p23,-1d0)
              endif
            endif
            
c>            write(*,*) 'cD0p0o v',p14/p12,p34/p23              
c>            write(*,*) 'cD0p0o v',cdln(p14,-1d0)-cdln(p12,-1d0),
c>     &          cdln(p34,-1d0)-cdln(p23,-1d0)
            
            cD0p0o = cD0p0o 
     &          - (-1)**k *eta*v

c>            write(*,*) 'cD0p0o etacon ', - (-1)**k *v,eta
c>            write(*,*) 'cD0p0o etasum ',cD0p0o
          endif
        enddo
        
c        write(*,*) 'cD0p0o sum ',cD0p0o

        cD0p0o = cD0p0o/(m1*m2*m3*m4*det)

c        write(*,*) 'cD0p0o =   ',cD0p0o    
        
c end old version
      
        if (abs((cd0p0-cd0p0o)/cd0p0).gt.testaccd0) then
          if (errflag.eq.0) then
            write(*,*) 
            write(*,*) 'cD0p0 possibly wrong '
            errflag = 1
          end if
          write(errout,*) 
          write(errout,*) 'cD0p0 possibly wrong '
          write(errout,*) 'cD0p0 args ',
     &        q12,q23,q34,q14,q13,q24,m12,m22,m32,m42
          write(errout,*) 'cD0p0o = ',cD0p0o
          write(errout,*) 'cD0p0  = ',cD0p0
          write(errout,*) 'diff   = ',abs((cD0p0-cD0p0o)/cD0p0)
        end if
     
      endif
        
      end

************************************************************************
      function cD0msp0(q12i,q23i,q34i,q14i,q13i,q24i,
     &    m12i,m22i,m32i,m42i)
************************************************************************
*  general scalar 4-point function                                     *
*  case with one zero external invariant                               *
*  and two associated small real masses                                *
*  based on equation (23) of                                           *
*        A.Denner, U.Nierste and R.Scharf, Nucl. Phys. B367 (1991) 637 *
*                                                                      *
*                    m22                                               *
*       q12  ---------------------  q23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | m32 small                                *
*                 |         |                                          *
*                 |    4    |                                          *
*       q14  ---------------------  q34 = 0                            *
*                  m42 small                                           *
*                                                                      *
*----------------------------------------------------------------------*
*  24.01.07 Ansgar Denner       last changed 26.01.07 Ansgar Denner    *
************************************************************************
      implicit   none
      complex*16 q12,q23,q34,q14,q13,q24
      complex*16 m12,m22,m32,m42
      complex*16 q12i,q23i,q34i,q14i,q13i,q24i
      complex*16 m12i,m22i,m32i,m42i
      complex*16 k12,k13,k14,k23,k24,k34
      complex*16 l12,l13,l14,l23,l24,l34
      complex*16 m1,m2,m3,m4
      real*8     ir12,ir14,ir23,ir34,ir13
      real*8     iy(2),iz(2)
      complex*16 r12,r13,r14,r23,r24,r34
      complex*16 r21,r31,r41,r32,r42,r43
      complex*16 a,b,c,d
      real*8     rd
      complex*16 x(2),y(2),z(2),eta,v
      complex*16 p12,p34,p14,p23
      real*8     t12,t34,t14,t23
      complex*16 cD0msp0,cspcoe,olietae,cdln,cD0m0,cD0msp0o

      complex*16 cd0
      data       cd0 /(0d0,0d0)/
 
      real*8     pi,pi2_6
      common /pietc/  pi,pi2_6
      real*8     lambda,lambda2
      common /lambda/ lambda,lambda2
      real*8     minfscale,minfscale2,shiftms2
      common /cmsing/ minfscale,minfscale2,shiftms2

      integer    ltest
      common /ltest/ ltest
      integer    flag
      data       flag /0/
      save       flag
      integer    errflag
      save       errflag
      data       errflag /0/
      integer    errout
      data       errout /90/
      real*8     testaccd0
      common /testaccd0/ testaccd0

      integer error_code
      common /coli_error/ error_code
      save /coli_error/

 2    if (flag.eq.0) then
        write(*,*) 
        write(*,*) 'cD0msp0 not yet tested in practice'
        flag = 1
      end if 

      if (m12i.eq.0d0) then
        cD0msp0 = cD0m0(q23i,q12i,q14i,q34i,q13i,q24i,
     &      m32i,m22i,cd0,m42i)
      else if (m22i.eq.0d0) then
        cD0msp0 = cD0m0(q14i,q12i,q23i,q34i,q24i,q13i,
     &      m42i,m12i,cd0,m32i)
      else if (m32i.eq.0d0) then
        cD0msp0 = cD0m0(q12i,q23i,q34i,q14i,q13i,q24i,
     &      m12i,m22i,cd0,m42i)
      else if (m42i.eq.0d0) then
        cD0msp0 = cD0m0(q23i,q34i,q14i,q12i,q24i,q13i,
     &      m22i,m32i,cd0,m12i)
      else

        if(dimag(m42i).ne.0d0.or.dimag(m32i).ne.0d0.or.
     &       (q34i).ne.0d0) then
          write(*,*) 'cD0msp0 called improperly:'
          write(*,*) 'cD0msp0: q12 = ',q12i
          write(*,*) 'cD0msp0: q23 = ',q23i
          write(*,*) 'cD0msp0: q34 = ',q34i
          write(*,*) 'cD0msp0: q14 = ',q14i
          write(*,*) 'cD0msp0: q24 = ',q24i
          write(*,*) 'cD0msp0: q13 = ',q13i
          write(*,*) 'cD0msp0: m12 = ',m12i
          write(*,*) 'cD0msp0: m22 = ',m22i
          write(*,*) 'cD0msp0: m32 = ',m32i
          write(*,*) 'cD0msp0: m42 = ',m42i
          error_code = 1
        end if

      q12=q13i
      q23=q23i
      q34=q24i
      q14=q14i
      q13=q12i
      q24=q34i
      m12=m12i
      m22=m32i*minfscale2
      m32=m22i
      m42=m42i*minfscale2


c new version
 100  continue

      l12 = (m12    -q12)
      l13 = (m12+m32-q13)
      l14 = (m12    -q14)
      l23 = (    m32-q23)
      l24 = (       -q24)
      l34 = (m32    -q34)
      
      r12 = l12/m22
      r21 = l12/m12  
      r14 = l14/m42
      r41 = l14/m12
      r23 = l23/m32
      r32 = l23/m22
      r34 = l34/m42
      r43 = l34/m32
      if (l13.ne.0d0) then
        r13 = l13/(2d0*m32)*(1d0+sqrt(1d0-4d0*m12*m32/l13**2))
      else
        r13 = sqrt(m12/m32)*dcmplx(0d0,1d0)
      end if
      r31 = r13*m32/m12
c      r13 = 1d0/r13
      r24=1d0
      r42=m42/m22
      a   =  m42*(l34/r42-l23 + (l12-l14/r42)*r31)
      b   =  l12*l34-l14*l23
      c   =  m12*(l34*r24-l23 + (l12-l14*r24)/r13)
      d   =  m12*(l23-r31*l12 - (l34-r31*l14)*r24)
      y(1) = -c/b
      y(2) = -b/a

      if(dreal(r12).lt.0d0) then
        ir12 = -1d1
      else
        ir12 = 0d0
      end if
      if(dreal(r14).lt.0d0) then
        ir14 = -1d1
      else
        ir14 = 0d0
      end if
      if(dreal(r23).lt.0d0) then
        ir23 = -1d1
      else
        ir23 = 0d0
      end if
      if(dreal(r34).lt.0d0) then
        ir34 = -1d1
      else
        ir34 = 0d0
      end if
c      if(k13.le.-2d0) then
        ir13 = sign(1d1,1d0-abs(r13*r13*m32/m12))
c      else
c        ir13 = 0d0
c      end if

c>      write(*,*) 'cD0msp0 q ',q12,q23,q34
c>      write(*,*) 'cD0msp0 q ',q14,q13,q24
c>      write(*,*) 'cD0msp0 m ',m12,m22,m32,m42
c>      write(*,*) 'cD0msp0 k ',l12,l13,l14
c>      write(*,*) 'cD0msp0 k ',l23,l24,l34
c>      write(*,*) 'cD0msp0 r ',r12,r13,r14
c>      write(*,*) 'cD0msp0 r ',r21,r31,r41
c>      write(*,*) 'cD0msp0 r ',r23,r24,r34
c>      write(*,*) 'cD0msp0 r ',r32,r42,r43
c>      write(*,*) 'cD0msp0 ir',ir12,ir13,ir14
c>      write(*,*) 'cD0msp0 ir',ir23,ir24,ir34
c>      write(*,*) 'cD0msp0 a ',a,b,c,d

      z(1) = y(1)*r31
      z(2) = y(2)*r31

      rd = dreal(d)
c result should be independent of imaginary parts of y and z!!!!
      if(dreal(y(1)).gt.0d0) then
         iy(1) = sign(1d0,-rd)
      else
         iy(1) = 0d0
      end if
      if(dreal(y(2)).gt.0d0) then
         iy(2) = sign(1d0,rd)
      else
         iy(2) = 0d0
      end if
      if(dreal(z(1)).gt.0d0) then
         iz(1) = sign(1d0,-rd*dreal(r13))
      else
         iz(1) = 0d0
      end if
      if(dreal(z(2)).gt.0d0) then
         iz(2) = -sign(1d0,rd*dreal(r13))
      else
         iz(2) = 0d0
      end if

c>      write(*,*) 'cD0msp0 y ',y(1),y(2)
c>      write(*,*) 'cD0msp0 iy',iy(1),iy(2)
c>      write(*,*) 'cD0msp0 z',z(1),z(2)
c>      write(*,*) 'cD0msp0 iz',iz(1),iz(2)

      cD0msp0 = dcmplx(0d0)
      cD0msp0 = cD0msp0 + (
     &      cspcoe(-y(1),cd0,-iy(1),-ir12)
     &      +cspcoe(-y(1),r21,-iy(1),ir12)
     &      -cspcoe(-y(1),cd0,-iy(1),-ir14)
     &      -cspcoe(-y(1),r41,-iy(1),ir14)
     &      +cspcoe(-z(1),cd0,-iz(1),-ir34)
     &      +cspcoe(-z(1),r43,-iz(1),ir34)
     &      -cspcoe(-z(1),cd0,-iz(1),-ir23)
     &      -cspcoe(-z(1),r23,-iz(1),ir23)
     &      )          

c>      write(*,*) 'cD0msp0 args yz',
c>     &      -y(1),-iy(1),-z(1),-iz(1)
c>      write(*,*) 'cD0msp0 args r',
c>     &      1d0/(r12),r21,1d0/(r14),r41,
c>     &      1d0/(r34),r43,1d0/(r32),r23
c>      write(*,*) 'cD0msp0 args ',
c>     &      1d0+y(1)/(r12),
c>     &      1d0+y(1)*r21,
c>     &      1d0+y(1)/(r14),
c>     &      1d0+y(1)*r41,
c>     &      1d0+z(1)/(r34),
c>     &      1d0+z(1)*r43,
c>     &      1d0+z(1)/(r32),
c>     &      1d0+z(1)*r23
c>      write(*,*) 'cD0msp0 csp ',
c>     &      cspcoe(-y(1),cd0,-iy(1),-ir12),
c>     &      +cspcoe(-y(1),r21,-iy(1),ir12),
c>     &      -cspcoe(-y(1),cd0,-iy(1),-ir14),
c>     &      -cspcoe(-y(1),r41,-iy(1),ir14),
c>     &      +cspcoe(-z(1),cd0,-iz(1),-ir34),
c>     &      +cspcoe(-z(1),r43,-iz(1),ir34),
c>     &      -cspcoe(-z(1),cd0,-iz(1),-ir23),
c>     &      -cspcoe(-z(1),r23,-iz(1),ir23)
c>      write(*,*) 'cD0msp0 cspsum ',cD0msp0


c *** improves numerical stability in extreme cases

        eta=olietae(-y(1),-1/z(1),-iy(1),iz(1),-ir13)

        if(eta.ne.0d0)then
          p14 = m12+l14*y(1)
          p12 = m12+l12*y(1)
          p34 = m32+l34*z(1)
          p23 = m32+l23*z(1)
          
          t14 = abs(p14)/max(abs(m12),abs(l14*y(1)))
          t12 = abs(p12)/max(abs(m12),abs(l12*y(1)))
          t34 = abs(p34)/max(abs(m32),abs(l34*z(1)))
          t23 = abs(p23)/max(abs(m32),abs(l23*z(1)))
          
          if (min(t12,t14).gt.min(t34,t23)) then
            if(dimag(p14/p12).ne.0d0)then
              v= cdln(p14/p12,0d0)
            else
              v=cdln(p14,-1d0)-cdln(p12,-1d0)
            endif
          else
            if(dimag(p34/p23).ne.0d0)then
              v= cdln(p34/p23,0d0)
            else
              v=cdln(p34,-1d0)-cdln(p23,-1d0)
            endif
          endif

c>          write(*,*) 'cD0msp0 v',p14/p12,p34/p23              
c>          write(*,*) 'cD0msp0 v',cdln(p14,-1d0)-cdln(p12,-1d0),
c>     &        cdln(p34,-1d0)-cdln(p23,-1d0)

          cD0msp0 = cD0msp0 
     &        +eta*v

c>          write(*,*) 'cD0msp0 etacon ', - (-1)**k *v,eta
c>          write(*,*) 'cD0msp0 etasum ',cD0msp0
        endif


        cD0msp0 = cD0msp0 - (
     &      cspcoe(-y(2),1d0/r12,-iy(2),-ir12)     
     &      -.5d0*(cdln(-y(2),-iy(2))+cdln(r21,ir12))**2
     &      -cspcoe(cd0,1d0/r21,iy(2),-ir12)
     &      -cspcoe(-y(2),1d0/(r14),-iy(2),-ir14)
     &      +.5d0*(cdln(-y(2),-iy(2))+cdln(r41,ir14))**2
     &      +cspcoe(cd0,1d0/r41,iy(2),-ir14)
     &      +cspcoe(-z(2),1d0/r34,-iz(2),-ir34)
     &      -.5d0*(cdln(-z(2),-iz(2))+cdln(r43,ir34))**2
     &      -cspcoe(cd0,1d0/r43,iz(2),-ir34)
     &      -cspcoe(-z(2),1d0/(r32),-iz(2),-ir23)
     &      +.5d0*(cdln(-z(2),-iz(2))+cdln(r23,ir23))**2
     &      +cspcoe(cd0,1d0/r23,iz(2),-ir23)
     &      ) 

c>      write(*,*) 'cD0msp0 args yz',
c>     &      -y(2),-iy(2),-z(2),-iz(2)
c>      write(*,*) 'cD0msp0 args r',
c>     &      1d0/(r12),r21,1d0/(r14),r41,
c>     &      1d0/(r34),r43,1d0/(r32),r23
c>      write(*,*) 'cD0msp0 args ',
c>     &      1d0+y(1)/(r12),
c>     &      1d0+y(1)*r21,
c>     &      1d0+y(1)/(r14),
c>     &      1d0+y(1)*r41,
c>     &      1d0+z(1)/(r34),
c>     &      1d0+z(1)*r43,
c>     &      1d0+z(1)/(r32),
c>     &      1d0+z(1)*r23
c>      write(*,*) 'cD0msp0 csp ',
c>     &      cspcoe(-y(2),1d0/(r12),-iy(2),-ir12),     
c>     &      -.5d0*(cdln(-y(2),-iy(2))+cdln(r21,ir12))**2
c>     &      -cspcoe(cd0,1d0/(r21),iy(2),-ir12),
c>     &      -cspcoe(-y(2),1d0/(r14),-iy(2),-ir14),
c>     &      +.5d0*(cdln(-y(2),-iy(2))+cdln(r41,ir14))**2
c>     &      +cspcoe(cd0,1d0/(r41),iy(2),-ir14),
c>     &      +cspcoe(-z(2),1d0/(r34),-iz(2),-ir34),
c>     &      -.5d0*(cdln(-z(2),-iz(2))+cdln(r43,ir34))**2
c>     &      -cspcoe(cd0,1d0/(r43),iz(2),-ir34),
c>     &      -cspcoe(-z(2),1d0/(r32),-iz(2),-ir23),
c>     &      +.5d0*(cdln(-z(2),-iz(2))+cdln(r23,ir23))**2
c>     &      +cspcoe(cd0,1d0/(r23),iz(2),-ir23)
c>
c>      write(*,*) 'cD0msp0 cspsum ',cD0msp0


c *** improves numerical stability in extreme cases

        eta=olietae(-y(2),-1/z(2),-iy(2),iz(2),-ir13)

        if(eta.ne.0d0)then
          p14 = m42*y(2)*y(2)+l14*y(2)
          p12 = m22*y(2)*y(2)+l12*y(2)
          p34 = m42*z(2)*z(2)+l34*z(2)
          p23 = m22*z(2)*z(2)+l23*z(2)
          
          t14 = abs(p14)/max(abs(m42*y(2)*y(2)),abs(l14*y(2)))
          t12 = abs(p12)/max(abs(m22*y(2)*y(2)),abs(l12*y(2)))
          t34 = abs(p34)/max(abs(m42*z(2)*z(2)),abs(l34*z(2)))
          t23 = abs(p23)/max(abs(m22*z(2)*z(2)),abs(l23*z(2)))
          
          if (min(t12,t14).gt.min(t34,t23)) then
            if(dimag(p14/p12).ne.0d0)then
              v= cdln(p14/p12,0d0)
            else
              v=cdln(p14,-1d0)-cdln(p12,-1d0)
            endif
          else
            if(dimag(p34/p23).ne.0d0)then
              v= cdln(p34/p23,0d0)
            else
              v=cdln(p34,-1d0)-cdln(p23,-1d0)
            endif
          endif

c>          write(*,*) 'cD0msp0 v',p14/p12,p34/p23              
c>          write(*,*) 'cD0msp0 v',cdln(p14,-1d0)-cdln(p12,-1d0),
c>     &        cdln(p34,-1d0)-cdln(p23,-1d0)

          cD0msp0 = cD0msp0 
     &        - eta*v

c>          write(*,*) 'cD0msp0 etacon ', - (-1)**k *v,eta
c>          write(*,*) 'cD0msp0 etasum ',cD0msp0
        endif

c      write(*,*) 'cD0msp0 sum ',cD0msp0

      cD0msp0 = cD0msp0/b

c      write(*,*) 'cD0msp0 =   ',cD0msp0    

c end new version

      if (ltest.lt.10) return

c begin old version

      m1 = sqrt(m12) 
      m2 = sqrt(m22) 
      m3 = sqrt(m32)
      m4 = sqrt(m42) 
      l12 = (m12    -q12)
      l13 = (m12+m32-q13)
      l14 = (m12    -q14)
      l23 = (    m32-q23)
      l24 = (       -q24)
      l34 = (m32    -q34)
      k12 = (m12    -q12)/(m1*m2)
      k13 = (m12+m32-q13)/(m1*m3)
      k14 = (m12    -q14)/(m1*m4)
      k23 = (   +m32-q23)/(m2*m3)
      k24 = (       -q24)/(m2*m4)
      k34 = (m32    -q34)/(m3*m4)
      

      r12 = k12
      r14 = k14
      r23 = k23
      r34 = k34
      if (k13.ne.0d0) then
        r13 = k13/2d0*(1d0+sqrt(dcmplx(1d0-4d0/k13**2)))
      else
        r13 = dcmplx(0d0,1d0)
      end if
c      r13 = 1d0/r13
      r24=m4/m2
      a   =  k34/r24-k23 + (k12-k14/r24)*r13
      b   =  k12*k34-k14*k23
      c   =  k34*r24-k23 + (k12-k14*r24)/r13
      d   =  k23-r13*k12 - (k34-r13*k14)*r24
      x(1) = -c/b
      x(2) = -b/a

      if(dreal(r12).lt.0d0) then
        ir12 = -1d1
      else
        ir12 = 0d0
      end if
      if(dreal(r14).lt.0d0) then
        ir14 = -1d1
      else
        ir14 = 0d0
      end if
      if(dreal(r23).lt.0d0) then
        ir23 = -1d1
      else
        ir23 = 0d0
      end if
      if(dreal(r34).lt.0d0) then
        ir34 = -1d1
      else
        ir34 = 0d0
      end if
c      if(k13.le.-2d0) then
        ir13 = sign(1d1,1d0-abs(r13))
c      else
c        ir13 = 0d0
c      end if

c>      write(*,*) 'cD0msp0o q ',q12,q23,q34
c>      write(*,*) 'cD0msp0o q ',q14,q13,q24
c>      write(*,*) 'cD0msp0o m ',m12,m22,m32,m42
c>      write(*,*) 'cD0msp0o k ',k12,k13,k14
c>      write(*,*) 'cD0msp0o k ',k23,k24,k34
c>      write(*,*) 'cD0msp0o r ',r12,r13,r14
c>      write(*,*) 'cD0msp0o r ',r23,r24,r34
c>      write(*,*) 'cD0msp0o r ',r12*m1/m2,r13*m1/m3,r14*m1/m4
c>      write(*,*) 'cD0msp0o r ',r12*m2/m1,r13*m3/m1,r14*m4/m1
c>      write(*,*) 'cD0msp0o r ',r23*m2/m3,r24*m2/m4,r34*m3/m4
c>      write(*,*) 'cD0msp0o r ',r23*m3/m2,r24*m4/m2,r34*m4/m3
c>      write(*,*) 'cD0msp0o ir',ir12,ir13,ir14
c>      write(*,*) 'cD0msp0o ir',ir23,ir24,ir34
c>      write(*,*) 'cD0msp0o a ',a,b,c,d
c>      write(*,*) 'cD0msp0o a ',a*m42*m2*m3,b*m1*m2*m3*m4,c*m12*m2*m3,
c>     &    d*m12*m2*m3
c>      write(*,*) 'cD0msp0o x ',x(1),x(2)
c>      write(*,*) 'cD0msp0o ix',ix(1),ix(2)

      y(1) = x(1)*m1/m4
      y(2) = x(2)*m1/m4
      z(1) = x(1)*m3/m4*r13
      z(2) = x(2)*m3/m4*r13

c result should be independent of imaginary parts of y and z!!!!
      if(dreal(y(1)).gt.0d0) then
         iy(1) = sign(1d0,-rd)
      else
         iy(1) = 0d0
      end if
      if(dreal(y(2)).gt.0d0) then
         iy(2) = sign(1d0,rd)
      else
         iy(2) = 0d0
      end if
      if(dreal(z(1)).gt.0d0) then
         iz(1) = sign(1d0,-rd*dreal(r13))
      else
         iz(1) = 0d0
      end if
      if(dreal(z(2)).gt.0d0) then
         iz(2) = -sign(1d0,rd*dreal(r13))
      else
         iz(2) = 0d0
      end if

c>      write(*,*) 'cD0msp0o y ',y(1),y(2)
c>      write(*,*) 'cD0msp0o iy',iy(1),iy(2)
c>      write(*,*) 'cD0msp0o z',z(1),z(2)
c>      write(*,*) 'cD0msp0o iz',iz(1),iz(2)

      cD0msp0o = dcmplx(0d0)
      cD0msp0o = cD0msp0o + (
     &      cspcoe(-y(1),cd0,-iy(1),-ir12)
     &      +cspcoe(-y(1),m2/m1*r12,-iy(1),ir12)
     &      -cspcoe(-y(1),cd0,-iy(1),-ir14)
     &      -cspcoe(-y(1),m4/m1*r14,-iy(1),ir14)
     &      +cspcoe(-z(1),cd0,-iz(1),-ir34)
     &      +cspcoe(-z(1),m4/m3*r34,-iz(1),ir34)
     &      -cspcoe(-z(1),cd0,-iz(1),-ir23)
     &      -cspcoe(-z(1),m2/m3*r23,-iz(1),ir23)
     &      )          

c>      write(*,*) 'cD0msp0o args yz',
c>     &      -y(1),-iy(1),-z(1),-iz(1)
c>      write(*,*) 'cD0msp0o args r',
c>     &      m2/(m1*r12),m2/m1*r12,m4/(m1*r14),m4/m1*r14,
c>     &      m4/(m3*r34),m4/m3*r34,m2/(m3*r23),m2/m3*r23
c>      write(*,*) 'cD0msp0o args ',
c>     &      1d0+y(1)*m2/(m1*r12),
c>     &      1d0+y(1)*m2/m1*r12,
c>     &      1d0+y(1)*m4/(m1*r14),
c>     &      1d0+y(1)*m4/m1*r14,
c>     &      1d0+z(1)*m4/(m3*r34),
c>     &      1d0+z(1)*m4/m3*r34,
c>     &      1d0+z(1)*m2/(m3*r23),
c>     &      1d0+z(1)*m2/m3*r23
c>      write(*,*) 'cD0msp0o csp ',
c>     &      cspcoe(-y(1),cd0,-iy(1),-ir12),
c>     &      +cspcoe(-y(1),m2/m1*r12,-iy(1),ir12),
c>     &      -cspcoe(-y(1),cd0,-iy(1),-ir14),
c>     &      -cspcoe(-y(1),m4/m1*r14,-iy(1),ir14),
c>     &      +cspcoe(-z(1),cd0,-iz(1),-ir34),
c>     &      +cspcoe(-z(1),m4/m3*r34,-iz(1),ir34),
c>     &      -cspcoe(-z(1),cd0,-iz(1),-ir23),
c>     &      -cspcoe(-z(1),m2/m3*r23,-iz(1),ir23)
c>
c>      write(*,*) 'cD0msp0o cspsum ',cD0msp0o


c *** improves numerical stability in extreme cases

        eta=olietae(-y(1),-1/z(1),-iy(1),iz(1),-ir13)

        if(eta.ne.0d0)then
          p14 = m12+l14*y(1)
          p12 = m12+l12*y(1)
          p34 = m32+l34*z(1)
          p23 = m32+l23*z(1)
          
          t14 = abs(p14)/max(abs(m12),abs(l14*y(1)))
          t12 = abs(p12)/max(abs(m12),abs(l12*y(1)))
          t34 = abs(p34)/max(abs(m32),abs(l34*z(1)))
          t23 = abs(p23)/max(abs(m32),abs(l23*z(1)))
          
          if (min(t12,t14).gt.min(t34,t23)) then
            if(dimag(p14/p12).ne.0d0)then
              v= cdln(p14/p12,0d0)
            else
              v=cdln(p14,-1d0)-cdln(p12,-1d0)
            endif
          else
            if(dimag(p34/p23).ne.0d0)then
              v= cdln(p34/p23,0d0)
            else
              v=cdln(p34,-1d0)-cdln(p23,-1d0)
            endif
          endif

c>          write(*,*) 'cD0msp0o v',p14/p12,p34/p23              
c>          write(*,*) 'cD0msp0o v',cdln(p14,-1d0)-cdln(p12,-1d0),
c>     &        cdln(p34,-1d0)-cdln(p23,-1d0)
c>
          cD0msp0o = cD0msp0o 
     &        +eta*v

c>          write(*,*) 'cD0msp0o etacon ', - (-1)**k *v,eta
c>          write(*,*) 'cD0msp0o etasum ',cD0msp0o
        endif


        cD0msp0o = cD0msp0o - (
     &      cspcoe(-y(2),m2/(m1*r12),-iy(2),-ir12)     
     &      -.5d0*(cdln(-y(2),-iy(2))+cdln(m2/m1*r12,ir12))**2
     &      -cspcoe(cd0,m1/(m2*r12),iy(2),-ir12)
     &      -cspcoe(-y(2),m4/(m1*r14),-iy(2),-ir14)
     &      +.5d0*(cdln(-y(2),-iy(2))+cdln(m4/m1*r14,ir14))**2
     &      +cspcoe(cd0,m1/(m4*r14),iy(2),-ir14)
     &      +cspcoe(-z(2),m4/(m3*r34),-iz(2),-ir34)
     &      -.5d0*(cdln(-z(2),-iz(2))+cdln(m4/m3*r34,ir34))**2
     &      -cspcoe(cd0,m3/(m4*r34),iz(2),-ir34)
     &      -cspcoe(-z(2),m2/(m3*r23),-iz(2),-ir23)
     &      +.5d0*(cdln(-z(2),-iz(2))+cdln(m2/m3*r23,ir23))**2
     &      +cspcoe(cd0,m3/(m2*r23),iz(2),-ir23)
     &      ) 

c>      write(*,*) 'cD0msp0o args yz',
c>     &      -y(2),-iy(2),-z(2),-iz(2)
c>      write(*,*) 'cD0msp0o args r',
c>     &      m2/(m1*r12),m2/m1*r12,m4/(m1*r14),m4/m1*r14,
c>     &      m4/(m3*r34),m4/m3*r34,m2/(m3*r23),m2/m3*r23
c>      write(*,*) 'cD0msp0o args ',
c>     &      1d0+y(2)*m2/(m1*r12),
c>     &      1d0+y(2)*m2/m1*r12,
c>     &      1d0+y(2)*m4/(m1*r14),
c>     &      1d0+y(2)*m4/m1*r14,
c>     &      1d0+z(2)*m4/(m3*r34),
c>     &      1d0+z(2)*m4/m3*r34,
c>     &      1d0+z(2)*m2/(m3*r23),
c>     &      1d0+z(2)*m2/m3*r23
c>      write(*,*) 'cD0msp0o csp ',
c>     &      cspcoe(-y(2),m2/(m1*r12),-iy(2),-ir12),     
c>     &      -.5d0*(cdln(-y(2),-iy(2))+cdln(m2/m1*r12,ir12))**2
c>     &      -cspcoe(cd0,m1/(m2*r12),iy(2),-ir12),
c>     &      -cspcoe(-y(2),m4/(m1*r14),-iy(2),-ir14),
c>     &      +.5d0*(cdln(-y(2),-iy(2))+cdln(m4/m1*r14,ir14))**2
c>     &      +cspcoe(cd0,m1/(m4*r14),iy(2),-ir14),
c>     &      +cspcoe(-z(2),m4/(m3*r34),-iz(2),-ir34),
c>     &      -.5d0*(cdln(-z(2),-iz(2))+cdln(m4/m3*r34,ir34))**2
c>     &      -cspcoe(cd0,m3/(m4*r34),iz(2),-ir34),
c>     &      -cspcoe(-z(2),m2/(m3*r23),-iz(2),-ir23),
c>     &      +.5d0*(cdln(-z(2),-iz(2))+cdln(m2/m3*r23,ir23))**2
c>     &      +cspcoe(cd0,m3/(m2*r23),iz(2),-ir23)
c>
c>      write(*,*) 'cD0msp0o cspsum ',cD0msp0o


c *** improves numerical stability in extreme cases

        eta=olietae(-y(2),-1/z(2),-iy(2),iz(2),-ir13)

        if(eta.ne.0d0)then
          p14 = m42*y(2)*y(2)+l14*y(2)
          p12 = m22*y(2)*y(2)+l12*y(2)
          p34 = m42*z(2)*z(2)+l34*z(2)
          p23 = m22*z(2)*z(2)+l23*z(2)
          
          t14 = abs(p14)/max(abs(m42*y(2)*y(2)),abs(l14*y(2)))
          t12 = abs(p12)/max(abs(m22*y(2)*y(2)),abs(l12*y(2)))
          t34 = abs(p34)/max(abs(m42*z(2)*z(2)),abs(l34*z(2)))
          t23 = abs(p23)/max(abs(m22*z(2)*z(2)),abs(l23*z(2)))
          
          if (min(t12,t14).gt.min(t34,t23)) then
            if(dimag(p14/p12).ne.0d0)then
              v= cdln(p14/p12,0d0)
            else
              v=cdln(p14,-1d0)-cdln(p12,-1d0)
            endif
          else
            if(dimag(p34/p23).ne.0d0)then
              v= cdln(p34/p23,0d0)
            else
              v=cdln(p34,-1d0)-cdln(p23,-1d0)
            endif
          endif

c>          write(*,*) 'cD0msp0o v',p14/p12,p34/p23              
c>          write(*,*) 'cD0msp0o v',cdln(p14,-1d0)-cdln(p12,-1d0),
c>     &        cdln(p34,-1d0)-cdln(p23,-1d0)

          cD0msp0o = cD0msp0o 
     &        - eta*v

c>          write(*,*) 'cD0msp0o etacon ', - (-1)**k *v,eta
c>          write(*,*) 'cD0msp0o etasum ',cD0msp0o
        endif

c      write(*,*) 'cD0msp0o sum ',cD0msp0o

      cD0msp0o = cD0msp0o/(m1*m2*m3*m4*b)

c      write(*,*) 'cD0msp0o =   ',cD0msp0o    

c end old version
      
      if (abs((cD0msp0-cD0msp0o)/cD0msp0).gt.testaccd0) then
        if (errflag.eq.0) then
          write(*,*) 
          write(*,*) 'cD0msp0 possibly wrong '
          errflag = 1
        end if
        write(errout,*) 
        write(errout,*) 'cD0msp0 possibly wrong '
        write(errout,*) 'cD0msp0 args ',
     &      q12,q23,q34,q14,q13,q24,m12,m22,m32,m42
        write(errout,*) 'cD0msp0o = ',cD0msp0o
        write(errout,*) 'cD0msp0  = ',cD0msp0
        write(errout,*) 'diff   = ',abs((cD0msp0-cD0msp0o)/cD0msp0)
      end if

      end if
      end

#endif

************************************************************************
      function D0regsd_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  scalar 4-point function                                             *
*  regular case  based on general result of Stefan Dittmaier           *
*                                                                      *
*                     m22                                              *
*       q12  ---------------------  q23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | m32                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       q14  ---------------------  q34                                *
*                     m42                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  05.01.10 Ansgar Denner       last changed 02.02.10 Ansgar Denner    *
************************************************************************
      implicit   none
      complex*16 D0regsd_coli
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42

      complex*16 l12,l13,l14,l23,l24,l34
      complex*16 r12,r13,r14,r23,r24,r34
      complex*16 r21,r31,r41,r32,r42,r43
      complex*16 det
      real*8     ir12,ir13,ir14,ir23,ir24,ir34
      complex*16 n2,n12,n22,n32,n42
      complex*16 z11,z12,z21,z22,z31,z32
      complex*16 x11,x12,x21,x22,x31,x32
      complex*16 xp11,xp12,xp21,xp22,xp31,xp32
      complex*16 xpp11,xpp12,xpp21,xpp22,xpp31,xpp32
      complex*16 a1,a2,a3,b1,b2,b3,c1,c2,c3
      real*8     in,im12,im22,im32,im42
      real*8     ix11,ix12,ix21,ix22,ix31,ix32
      real*8     ixp11,ixp12,ixp21,ixp22,ixp31,ixp32
      real*8     ixpp11,ixpp12,ixpp21,ixpp22,ixpp31,ixpp32
      real*8     al,lal,be1,be2,be3,lbe1,lbe2,lbe3,ald,bed
      real*8     x1,x2,x3,x4
c      real*8     ix
      complex*16 lreg_coli,pa,pb
      real*8     pab
c      integer    k

      real*8     fixix,in2,ii2,ij2,ik2,il2,ifac
      complex*16 mi2,lij,mj2,mk2,lkl,ml2,x,xp,ni2,nj2,nk2,nl2

#ifdef WRITESD
      complex*16 cln_coli
#endif

#ifdef CHECK
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 elimminf2_coli
      complex*16 D0regsd_check,D0mass_std
c      complex*16 cspenc_coli,cspcos_coli,cln_coli
      logical flag(0:1)
      data    flag /2*.true./
      save    flag  
#endif

      include "coli_params.h"
      include "coli_checkparams.h"

      integer error_code
      common /coli_error/ error_code
      save /coli_error/

c      write(*,*) ' D0regsd_coli in ',
c     &    p12,p23,p34,p14,p13,p24,m12,m22,m32,m42

c      D0regsd_coli=cD0reg(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
c      write(*,*) ' D0regsd_coli: cD0reg= ',D0regsd_coli

c       pab(ald,bed)=1d0*(1d0+ald)*(1d0+bed)*m12
c     &    - l12*(1d0+ald) - l13*(ald+bed+2d0*ald*bed)
c     &    - l14*(1d0+bed) + l23*ald + l24 + 2d0*ald*bed*m32 + l34*bed

       pab(ald,bed)=(1d0+ald)*p12
     &    + (ald+bed+2d0*ald*bed)*p13
     &    + (1d0+bed)*p14 - ald*p23  - p24 - bed*p34

       pa(x1,x2,x3,x4,ald)=
     &     -(1d0+ald)*(2d0*m12*x1+l12*x2+l13*x3+l14*x4)
     &           +ald*(2d0*m32*x3+l13*x1+l23*x2+l34*x4)
     &           +    (2d0*m42*x4+l14*x1+l24*x2+l34*x3)

       pb(x1,x2,x3,x4,bed)=
     &     -(1d0+bed)*(2d0*m12*x1+l12*x2+l13*x3+l14*x4)
     &           +bed*(2d0*m32*x3+l13*x1+l23*x2+l34*x4)
     &           +    (2d0*m22*x2+l12*x1+l23*x3+l24*x4)

       fixix(x,xp,ifac,mi2,lij,mj2,ni2,nj2,mk2,lkl,ml2,nk2,nl2,
     &     in2,ii2,ij2,ik2,il2)=
     &     ( in2*((1d0+x)**2*(mk2+lkl*xp+ml2*xp*xp)
     &         -(1d0+xp)**2*(mi2+lij*x+mj2*x*x))
     &     + (ik2+xp*il2)*(1d0+xp)*(1d0+x)*(ni2+nj2*x)
     &     - (ii2+x*ij2)*(1d0+x)*(1d0+xp)*(nk2+nl2*xp))/
     &     ( ifac*( (nk2+nl2*xp)*(mi2+lij*x+mj2*x*x)
     &              +nl2*(1d0+xp)*(mi2+lij*x+mj2*x*x)
     &              - (1d0+x)*(ni2+nj2*x)*(lkl+2d0*ml2*xp) )
     &         -  ( (ni2+nj2*x)*(mk2+lkl*xp+ml2*xp*xp)
     &              +nj2*(1d0+x)*(mk2+lkl*xp+ml2*xp*xp)
     &              - (1d0+xp)*(nk2+nl2*xp)*(lij+2d0*mj2*x) )
     &     )


#ifdef CHECK
 100  format(((a)))
 101  format(a22,g25.17)
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ms42 = elimminf2_coli(m42)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps34 = elimminf2_coli(p34)
        ps14 = elimminf2_coli(p14)
        ps24 = elimminf2_coli(p24)
        ps13 = elimminf2_coli(p13)

        if(ms32.eq.0d0.or.ms12.eq.cd0.or.ms22.eq.cd0.or.ms42.eq.cd0
     &      .or.dimag(p12).ne.0d0.or.dimag(p23).ne.0d0
     &      .or.dimag(p34).ne.0d0.or.dimag(p14).ne.0d0
     &      .or.dimag(p24).ne.0d0.or.dimag(p13).ne.0d0) then
          write(*,100)' D0regsd_coli called improperly:'
          write(*,111)' D0regsd_coli: p12 = ',p12
          write(*,111)' D0regsd_coli: p23 = ',p23
          write(*,111)' D0regsd_coli: p34 = ',p34
          write(*,111)' D0regsd_coli: p14 = ',p14
          write(*,111)' D0regsd_coli: p13 = ',p13
          write(*,111)' D0regsd_coli: p24 = ',p24
          write(*,111)' D0regsd_coli: m12 = ',m12
          write(*,111)' D0regsd_coli: m22 = ',m22
          write(*,111)' D0regsd_coli: m32 = ',m32
          write(*,111)' D0regsd_coli: m42 = ',m42
          write(*,*)' D0regsd_coli: test= ',
     &        m32.ne.0d0,m12.eq.cd0,m22.eq.cd0,m42.eq.cd0,
     &        p23.eq.m22.and.p13.eq.m12
     &        ,p23.eq.m22.and.p34.eq.m42
     &        ,p13.eq.m12.and.p34.eq.m42
     &        ,ps23.eq.cd0.and.ms22.eq.cd0
     &        ,ps34.eq.cd0.and.ms42.eq.cd0
     &        ,ps13.eq.cd0.and.ms12.eq.cd0
     &        ,ps12.eq.cd0.and.ms12.eq.cd0.and.ms22.eq.cd0
     &        ,ps14.eq.cd0.and.ms12.eq.cd0.and.ms42.eq.cd0
     &        ,ps24.eq.cd0.and.ms22.eq.cd0.and.ms22.eq.cd0
     &        ,dimag(p12).ne.0d0,dimag(p23).ne.0d0
     &        ,dimag(p34).ne.0d0,dimag(p14).ne.0d0
     &        ,dimag(p24).ne.0d0,dimag(p13).ne.0d0
          stop
        end if
      end if
#endif

#ifdef CHECK
      if(flag(1))then
        write(*,100)' D0regsd_coli: ',
     &      '    not yet tested in physical process'
        write(*,111)' D0regsd_coli: p12 = ',p12
        write(*,111)' D0regsd_coli: p23 = ',p23
        write(*,111)' D0regsd_coli: p34 = ',p34
        write(*,111)' D0regsd_coli: p14 = ',p14
        write(*,111)' D0regsd_coli: p13 = ',p13
        write(*,111)' D0regsd_coli: p24 = ',p24
        write(*,111)' D0regsd_coli: m12 = ',m12
        write(*,111)' D0regsd_coli: m22 = ',m22
        write(*,111)' D0regsd_coli: m32 = ',m32
        write(*,111)' D0regsd_coli: m42 = ',m42
        flag(1)=.false.
      endif
#endif

#ifdef WRITESD
        write(*,111)' D0regsd_coli: p12 = ',p12
        write(*,111)' D0regsd_coli: p23 = ',p23
        write(*,111)' D0regsd_coli: p34 = ',p34
        write(*,111)' D0regsd_coli: p14 = ',p14
        write(*,111)' D0regsd_coli: p13 = ',p13
        write(*,111)' D0regsd_coli: p24 = ',p24
        write(*,111)' D0regsd_coli: m12 = ',m12
        write(*,111)' D0regsd_coli: m22 = ',m22
        write(*,111)' D0regsd_coli: m32 = ',m32
        write(*,111)' D0regsd_coli: m42 = ',m42
#endif

      l12 = (m12+m22-p12)
      l13 = (m12+m32-p13)
      l14 = (m12+m42-p14)
      l23 = (m22+m32-p23)
      l24 = (m22+m42-p24)
      l34 = (m32+m42-p34)

      if(l12.ne.cd0)then
        r12 = l12/(2d0*m22)*(1d0+sqrt(1d0-4d0*m12*m22/l12**2))
      else
        r12 = dcmplx(0d0,1d0)*sqrt(m12/m22)
      endif
      r21 = r12*m22/m12
      if(l13.ne.cd0)then
        r13 = l13/(2d0*m32)*(1d0+sqrt(1d0-4d0*m12*m32/l13**2))
      else
        r13 = dcmplx(0d0,1d0)*sqrt(m12/m32)
      endif
      r31 = r13*m32/m12
      if(l14.ne.cd0)then
        r14 = l14/(2d0*m42)*(1d0+sqrt(1d0-4d0*m12*m42/l14**2))
      else
        r14 = dcmplx(0d0,1d0)*sqrt(m12/m42)
      endif
      r41 = r14*m42/m12
      if(l23.ne.cd0)then
        r23 = l23/(2d0*m32)*(1d0+sqrt(1d0-4d0*m22*m32/l23**2))
      else
        r23 = dcmplx(0d0,1d0)*sqrt(m22/m32)
      endif
      r32 = r23*m32/m22
      if(l24.ne.cd0)then
        r24 = l24/(2d0*m42)*(1d0+sqrt(1d0-4d0*m22*m42/l24**2))
      else
        r24 = dcmplx(0d0,1d0)*sqrt(m22/m42)
      endif
      r42 = r24*m42/m22
      if(l34.ne.cd0)then
        r34 = l34/(2d0*m42)*(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
      else
        r34 = dcmplx(0d0,1d0)*sqrt(m32/m42)
      endif
      r43 = r34*m42/m32


c for testing
c>      r31 = 1d0/r13
c>      r13 = m12/m32/r13
c>      r42 = 1d0/r24
c>      r24 = m22/m42/r24
c>      r32 = 1d0/r23
c>      r23 = m22/m32/r23
c>      r41 = 1d0/r14
c>      r14 = m12/m42/r14
c>      r34 = 1d0/r43
c>      r43 = m42/m32/r43
c>      r12 = 1d0/r21
c>      r21 = m22/m12/r21
c end testing

      ir12 = sign(1d0,1d0-abs(r12*r21))
      ir13 = sign(1d0,1d0-abs(r13*r31))
      ir14 = sign(1d0,1d0-abs(r14*r41))
      ir23 = sign(1d0,1d0-abs(r23*r32))
      ir24 = sign(1d0,1d0-abs(r24*r42))
      ir34 = sign(1d0,1d0-abs(r34*r43))

#ifdef WRITESD
      write(*,*) 'D0regsd_coli l ',l12,l13,l14
      write(*,*) 'D0regsd_coli l ',l23,l24,l34
      write(*,*) 'D0regsd_coli r ',r12,r13,r14
      write(*,*) 'D0regsd_coli r ',r21,r31,r41
      write(*,*) 'D0regsd_coli r ',r23,r24,r34
      write(*,*) 'D0regsd_coli r ',r32,r42,r43
      write(*,*) 'D0regsd_coli ir ',ir12,ir13,ir14
      write(*,*) 'D0regsd_coli ir ',ir23,ir24,ir34
#endif

      lal = p14+p13-p34
      if(p13.eq.cd0)then
        al=-p14/lal
      elseif(p14.eq.cd0)then
        al=0d0
      elseif(dreal(1d0-4d0*p13*p14/lal**2).ge.0d0)then
        al = -lal/(2d0*p13)*(1d0+sqrt(1d0-4d0*p13*p14/lal**2))
      else
        write(*,*) 'D0regsd_coli: alpha complex',p13,p14,p34
        error_code = 1
      endif

#ifdef WRITESD
      write(*,*)  'D0regsd_coli: alpha ',p13,p34,p14
#endif

      lbe1 = p24+p14-p12
      if(p14.eq.cd0)then
        be1=-p24/lbe1
      elseif(p24.eq.cd0)then
        be1=0d0
      elseif(p12.eq.cd0)then
        be1=-1d0
      elseif(dreal(1d0-4d0*p14*p24/lbe1**2).ge.0d0)then
        be1 = -lbe1/(2d0*p14)*(1d0+sqrt(1d0-4d0*p14*p24/lbe1**2))
      else
        write(*,*) 'D0regsd_coli: beta1 complex'
        write(*,*) 'D0regsd_coli: pij = ',p12,p24,p14
        error_code = 1
      endif
      lbe2 = p23+p34-p24
      if(p34.eq.cd0)then
        be2=-p23/lbe2
      elseif(p23.eq.cd0)then
        be2=0d0
      elseif(p24.eq.cd0)then
        be2=-1d0
      elseif(dreal(1d0-4d0*p34*p23/lbe2**2).ge.0d0)then
        be2 = -lbe2/(2d0*p34)*(1d0+sqrt(1d0-4d0*p34*p23/lbe2**2))
      else
        write(*,*) 'D0regsd_coli: beta2 complex'
        write(*,*) 'D0regsd_coli: pij = ',p24,p23,p34
        error_code = 1
      endif
      lbe3 = p13+p12-p23
      if(p13.eq.cd0)then
        be3=-p12/lbe3
      elseif(p12.eq.cd0)then
        be3=0d0
      elseif(p23.eq.cd0)then
        be3=-1d0
      elseif(dreal(1d0-4d0*p13*p12/lbe3**2).ge.0d0)then
        be3 = -lbe3/(2d0*p13)*(1d0+sqrt(1d0-4d0*p13*p12/lbe3**2))
      else
        write(*,*) 'D0regsd_coli: beta3 complex'
        write(*,*) 'D0regsd_coli: pij = ',p23,p13,p12

        error_code = 1
      endif

#ifdef WRITESD
      write(*,*) 'd0regsd:al,be',al,be1,be2,be3
#endif

      n2  = -(1d0+al)*m12 + al*m32 + m42
      n12 = n2 - p14 - al*p13
      n22 = n2 + (1d0+al)*p12 - al*p23 - p24
      n32 = n2 + (1d0+al)*p13 - p34
      n42 = n2 + (1d0+al)*p14 - al*p34

      im12 = -1d0
      im22 = -2d0
      im32 = -5d0
      im42 = -10d0

      im12 = -m12
      im22 = -1.3d0*m22
      im32 = -2.1d0*m32
      im42 = -3.1d0*m42

      in =  -(1d0+al)*im12 + al*im32 + im42
       
#ifdef WRITESD
      write(*,*) 'd0regsd: n',n12,n22,n32,n42
#endif

      D0regsd_coli = cd0

      if(al.ne.0d0)then
        a1 = al*(p13*pab(al,al*(1d0+be1))
     &      -pa(-1d0,0d0,1d0,0d0,al)*pb(-1d0,0d0,1d0,0d0,al*(1d0+be1))
     &      +(1d0+be1)*(pa(-1d0,0d0,1d0,0d0,al))**2)

        b1 = (l13-2d0*m12-2d0*al*p13)*pab(al,al*(1d0+be1))
     &      -pa(1d0+al,0d0,-al,0d0,al)*pb(-1d0,0d0,1d0,0d0,al*(1d0+be1))
     &      -pa(-1d0,0d0,1d0,0d0,al)*pb(1d0+al,0d0,-al,0d0,al*(1d0+be1))
     &      +2d0*(1d0+be1)
     &      *pa(-1d0,0d0,1d0,0d0,al)*pa(1d0+al,0d0,-al,0d0,al)

        c1 = 1d0/al*(((1d0+al)*l14-al*l34-m42)*pab(al,al*(1d0+be1))
     &      -pa(1d0+al,0d0,-al,0d0,al)
     &      *pb(1d0+al,0d0,-al,0d0,al*(1d0+be1))
     &      +(1d0+be1)*(pa(1d0+al,0d0,-al,0d0,al))**2)

        det = sqrt(b1*b1-4d0*a1*c1)

        if(dreal(b1).gt.0d0) then
          z12 = (-b1-det)/(2d0*a1)
          z11 = c1/(a1*z12)
        else
          z11 = (-b1+det)/(2d0*a1)
          z12 = c1/(a1*z11)
        endif

c        ix11=-1d0
c        ix12=-1d0

        if(be1.ne.0d0)then
          xp11=-z11/(z11+be1)
          xp12=-z12/(z12+be1)
c          ixp11=-be1*ix11
c          ixp12=-be1*ix12

          x11=z11/(1d0-z11)
          x12=z12/(1d0-z12)

c i=4 j=2
c>          ixp11=(1d0+xp11)*(-in*(1d0+xp11)+im42 + xp11*im22)/
c>     &        (n42+n22-l24+2d0*xp11*(n22-m22))
c>          ixp12=(1d0+xp12)*(-in*(1d0+xp12)+im42 + xp12*im22)/
c>     &        (n42+n22-l24+2d0*xp12*(n22-m22))
c>
c>c i=4 j=1
c>        ix11=(1d0+x11)*(-in*(1d0+x11)+im42 + x11*im12)/
c>     &      (n42+n12-l14+2d0*x11*(n12-m12))
c>        ix12=(1d0+x12)*(-in*(1d0+x12)+im42 + x12*im12)/
c>     &      (n42+n12-l14+2d0*x12*(n12-m12))

          ix11 = fixix(x11,xp11,-be1*dreal(xp11*xp11/(x11*x11)),
     &        m42,l14,m12,n42,n12,
     &        m42,l24,m22,n42,n22,
     &        in,im42,im12,im42,im22)
          
          ix12 = fixix(x12,xp12,-be1*dreal(xp12*xp12/(x12*x12)),
     &        m42,l14,m12,n42,n12,
     &        m42,l24,m22,n42,n22,
     &        in,im42,im12,im42,im22)

          ixp11=-be1*ix11
          ixp12=-be1*ix12

#ifdef WRITESD
          write(*,*) 'D0regsd ixp11',ixp11,ix11
          write(*,*) 'D0regsd ixp12',ixp12,ix12
#endif

c>          write(*,*) 'D0regsd resxp11',
c>     &        (1d0+xp11)*(n42+n22*xp11)/(m42+l24*xp11+m22*xp11**2)
c>     &        /(1d0+x11)/(n42+n12*x11)*(m42+l14*x11+m12*x11**2)
c>          write(*,*) 'D0regsd resxp12',
c>     &        (1d0+xp12)*(n42+n22*xp12)/(m42+l24*xp12+m22*xp12**2)
c>     &        /(1d0+x12)/(n42+n12*x12)*(m42+l14*x12+m12*x12**2)
c>
c>
c>          write(*,*) 'D0regsd xp11',xp11,-x11/(be1*(1d0+x11)+x11)
c>          write(*,*) 'D0regsd xp12',xp12,-x12/(be1*(1d0+x12)+x12)


#ifdef WRITESD
          write(*,*)
          write(*,*) 'd0regsd:   x1p ',xp11,xp12,ixp11,ixp12
          write(*,*) 'd0regsd: -ln(m)',-cln_coli(m42,-1d0)
          write(*,*) 'd0regsd:  ln(M)', cln_coli(n42,in)
          write(*,*) 'd0regsd: Mj2/Mi2',n22/n42,-in*(n22-n42)
          write(*,*) 'd0regsd: root1 ',r24,ir24
          write(*,*) 'd0regsd: root2 ',1d0/r42
#endif
          
          D0regsd_coli = D0regsd_coli 
     &        +lreg_coli(xp11,m42,n42,n22,r24,1d0/r42,ir24,in,ixp11)
     &        -lreg_coli(xp12,m42,n42,n22,r24,1d0/r42,ir24,in,ixp12)

#ifdef WRITESD
          write(*,*) 'd0regsd: sum11 ', D0regsd_coli
#endif

        endif

        if(be1.ne.-1d0)then
          xpp11=(1d0-z11)/(z11+be1)
          xpp12=(1d0-z12)/(z12+be1)
          ixpp11=-(1d0+be1)*ix11
          ixpp12=-(1d0+be1)*ix12

          x11=z11/(1d0-z11)
          x12=z12/(1d0-z12)

c i=1 j=2
c          ixpp11=(1d0+xpp11)*(-in*(1d0+xpp11)+im12 + xpp11*im22)/
c     &        (n12+n22-l12+2d0*xpp11*(n22-m22))
c          ixpp12=(1d0+xpp12)*(-in*(1d0+xpp12)+im12 + xpp12*im22)/
c     &        (n12+n22-l12+2d0*xpp12*(n22-m22))

c>c i=4 j=1
c>        ix11=(1d0+x11)*(-in*(1d0+x11)+im42 + x11*im12)/
c>     &      (n42+n12-l14+2d0*x11*(n12-m12))
c>        ix12=(1d0+x12)*(-in*(1d0+x12)+im42 + x12*im12)/
c>     &      (n42+n12-l14+2d0*x12*(n12-m12))

          ix11 = fixix(x11,xpp11,-(1d0+be1)*dreal(xpp11*xpp11),
     &        m42,l14,m12,n42,n12,
     &        m12,l12,m22,n12,n22,
     &        in,im42,im12,im12,im22)
          
          ix12 = fixix(x12,xpp12,-(1d0+be1)*dreal(xpp12*xpp12),
     &        m42,l14,m12,n42,n12,
     &        m12,l12,m22,n12,n22,
     &        in,im42,im12,im12,im22)

c>          ix11= ((1d0+x11)*(-in*(1d0+x11)+im42 + x11*im12)
c>     &        -(1d0+xpp11)*(-in*(1d0+xpp11)+im12 + xpp11*im22))/
c>     &        ((n42+n12-l14+2d0*x11*(n12-m12))
c>     &        +(1d0+be1)*xpp11*xpp11*(n12+n22-l12+2d0*xpp11*(n22-m22)))
c>          ix12= ((1d0+x12)*(-in*(1d0+x12)+im42 + x12*im12)
c>     &        -(1d0+xpp12)*(-in*(1d0+xpp12)+im12 + xpp12*im22))/
c>     &        ((n42+n12-l14+2d0*x12*(n12-m12))
c>     &        +(1d0+be1)*xpp12*xpp12*(n12+n22-l12+2d0*xpp12*(n22-m22)))

          ixpp11=-(1d0+be1)*ix11
          ixpp12=-(1d0+be1)*ix12

#ifdef WRITESD
          write(*,*) 'D0regsd ixpp11',ixpp11,ix11
          write(*,*) 'D0regsd ixpp12',ixpp12,ix12
#endif

c>          write(*,*) 'D0regsd resxpp11',
c>     &        (1d0+xpp11)*(n12+n22*xpp11)/(m12+l12*xpp11+m22*xpp11**2)
c>     &        /(1d0+x11)/(n42+n12*x11)*(m42+l14*x11+m12*x11**2)
c>          write(*,*) 'D0regsd resxpp12',
c>     &        (1d0+xpp12)*(n12+n22*xpp12)/(m12+l12*xpp12+m22*xpp12**2)
c>     &        /(1d0+x12)/(n42+n12*x12)*(m42+l14*x12+m12*x12**2)
c>
c>          write(*,*) 'D0regsd xpp11',xpp11,1d0/(be1*(1d0+x11)+x11)
c>          write(*,*) 'D0regsd xpp12',xpp12,1d0/(be1*(1d0+x12)+x12)


          
#ifdef WRITESD
          write(*,*)
          write(*,*) 'd0regsd:   x1pp',xpp11,xpp12,ixpp11,ixpp12
          write(*,*) 'd0regsd: -ln(m)',-cln_coli(m12,-1d0)
          write(*,*) 'd0regsd:  ln(M)', cln_coli(n12,in)
          write(*,*) 'd0regsd:mj2/mi2',n22/n12,-in*(n22-n12)
          write(*,*) 'd0regsd: root1 ',r21,ir12
          write(*,*) 'd0regsd: root2 ',1d0/r12
#endif
          
          D0regsd_coli = D0regsd_coli
     &        -lreg_coli(xpp11,m12,n12,n22,r21,1d0/r12,ir12,in,ixpp11)
     &        +lreg_coli(xpp12,m12,n12,n22,r21,1d0/r12,ir12,in,ixpp12)

#ifdef WRITESD
          write(*,*) 'd0regsd: sum12 ',D0regsd_coli
#endif

        endif

        x11=z11/(1d0-z11)
        x12=z12/(1d0-z12)

c i=4 j=1
c>        ix11=(1d0+x11)*(-in*(1d0+x11)+im42 + x11*im12)/
c>     &      (n42+n12-l14+2d0*x11*(n12-m12))
c>        ix12=(1d0+x12)*(-in*(1d0+x12)+im42 + x12*im12)/
c>     &      (n42+n12-l14+2d0*x12*(n12-m12))
c>
c>          write(*,*) 'D0regsd resx11',
c>     &        (1d0+x11)*(n42+n12*x11)/(m42+l14*x11+m12*x11**2),
c>     &        (1d0+x11),(n42+n12*x11),(m42+l14*x11+m12*x11**2)
c>          write(*,*) 'D0regsd resx12',
c>     &        (1d0+x12)*(n42+n12*x12)/(m42+l14*x12+m12*x12**2),
c>     &        (1d0+x12),(n42+n12*x12),(m42+l14*x12+m12*x12**2)

        if(ix11*ixpp11*(1d0+be1).gt.0d0) then
          write(*,*) 'ixpp11 inconsistent'
        endif
        if(ix12*ixpp12*(1d0+be1).gt.0d0) then
          write(*,*) 'ixpp12 inconsistent'
        endif
        if(ix11*ixp11*be1.gt.0d0) then
          write(*,*) 'ixp11 inconsistent'
        endif
        if(ix12*ixp12*be1.gt.0d0) then
          write(*,*) 'ixp12 inconsistent'
        endif

#ifdef WRITESD
        write(*,*) 'D0reg_sd ixp11',ixp11,-ix11*be1
        write(*,*) 'D0reg_sd ixp12',ixp12,-ix12*be1
        write(*,*) 'D0reg_sd ixpp11',ixpp11,-ix11*(1d0+be1)
        write(*,*) 'D0reg_sd ixpp12',ixpp12,-ix12*(1d0+be1)
#endif

#ifdef WRITESD
        write(*,*)
        write(*,*) 'd0regsd:   x1  ',x11,x12,ix12,ix12
        write(*,*) 'd0regsd: -ln(m)',-cln_coli(m42,-1d0)
        write(*,*) 'd0regsd:  ln(M)', cln_coli(n42,in)
        write(*,*) 'd0regsd:mj2/mi2',n12/n42,-in*(n12-n42)
        write(*,*) 'd0regsd: root1 ',r14,ir14
        write(*,*) 'd0regsd: root2 ',1d0/r41
#endif

        D0regsd_coli = D0regsd_coli
     &      -lreg_coli(x11,m42,n42,n12,r14,1d0/r41,ir14,in,ix11)
     &      +lreg_coli(x12,m42,n42,n12,r14,1d0/r41,ir14,in,ix12)

#ifdef WRITESD
        write(*,*) 'd0regsd: sum13 ',D0regsd_coli
#endif

      endif

#ifdef WRITESD
      write(*,*) 'd0regsd: sum1 ',D0regsd_coli
#endif

      if(al.ne.-1d0)then
        a2 = (1d0+al)*(-p13*pab(al,-1d0-(1d0+al)*be2)
     &      +pa(1d0,0d0,-1d0,0d0,al)
     &      *pb(1d0,0d0,-1d0,0d0,-1d0-(1d0+al)*be2)
     &      +be2*(pa(1d0,0d0,-1d0,0d0,al))**2)
        
        b2 = (2d0*m32-l13)*pab(al,-1d0-(1d0+al)*be2)
     &      +pa(1d0,0d0,-1d0,0d0,al)
     &      *pb(0d0,0d0,1d0,0d0,-1d0-(1d0+al)*be2)
     &      +pa(0d0,0d0,1d0,0d0,al)
     &      *pb(1d0,0d0,-1d0,0d0,-1d0-(1d0+al)*be2)
     &      +2d0*be2*pa(1d0,0d0,-1d0,0d0,al)*pa(0d0,0d0,1d0,0d0,al)

        c2 = 1d0/(1d0+al)*(-m32*pab(al,-1d0-(1d0+al)*be2)
     &      +pa(0d0,0d0,1d0,0d0,al)
     &      *pb(0d0,0d0,1d0,0d0,-1d0-(1d0+al)*be2)
     &      +be2*(pa(0d0,0d0,1d0,0d0,al))**2)

        det = sqrt(b2*b2-4d0*a2*c2)

        if(dreal(b2).gt.0d0) then
          z22 = (-b2-det)/(2d0*a2)
          z21 = c2/(a2*z22)
        else
          z21 = (-b2+det)/(2d0*a2)
          z22 = c2/(a2*z21)
        endif

        ix21=1d0
        ix22=1d0

        if(be2.ne.0d0)then
          xp21=-z21/(z21+be2)
          xp22=-z22/(z22+be2)
          ixp21=-be2*ix21
          ixp22=-be2*ix22

          x21=z21/(1d0-z21)
          x22=z22/(1d0-z22)

c i=3 j=4
c>          ixp21=(1d0+xp21)*(-in*(1d0+xp21)+im32 + xp21*im42)/
c>     &        (n32+n42-l34+2d0*xp21*(n42-m42))
c>          ixp22=(1d0+xp22)*(-in*(1d0+xp22)+im32 + xp21*im42)/
c>     &        (n32+n42-l34+2d0*xp22*(n42-m42))

          ix21 = fixix(x21,xp21,-be2*dreal(xp21*xp21/(x21*x21)),
     &        m32,l34,m42,n32,n42,
     &        m32,l23,m22,n32,n22,
     &        in,im32,im42,im32,im22)
          
          ix22 = fixix(x22,xp22,-be2*dreal(xp22*xp22/(x22*x22)),
     &        m32,l34,m42,n32,n42,
     &        m32,l23,m22,n32,n22,
     &        in,im32,im42,im32,im22)
          
          ixp21=-be2*ix21
          ixp22=-be2*ix22

#ifdef WRITESD
          write(*,*)
          write(*,*) 'd0regsd:   x2p ',xp21,xp22,ixp21,ixp22
          write(*,*) 'd0regsd: -ln(m)',-cln_coli(m32,-1d0)
          write(*,*) 'd0regsd:  ln(M)', cln_coli(n32,in)
          write(*,*) 'd0regsd: Mj2/Mi2',n22/n32,-in*(n22-n32)
          write(*,*) 'd0regsd: root1 ',r23
          write(*,*) 'd0regsd: root2 ',1d0/r32
#endif
          
          D0regsd_coli = D0regsd_coli 
     &        +lreg_coli(xp21,m32,n32,n22,r23,1d0/r32,ir23,in,ixp21)
     &        -lreg_coli(xp22,m32,n32,n22,r23,1d0/r32,ir23,in,ixp22)
          
#ifdef WRITESD
          write(*,*) 'd0regsd: sum21 ',D0regsd_coli
#endif

        endif

        if(be2.ne.-1d0)then
          xpp21=(1d0-z21)/(z21+be2)
          xpp22=(1d0-z22)/(z22+be2)
          ixpp21=-(1d0+be2)*ix21
          ixpp22=-(1d0+be2)*ix22
          
          x21=z21/(1d0-z21)
          x22=z22/(1d0-z22)

c i=4 j=2
c>          ixpp21=(1d0+xpp11)*(-in*(1d0+xpp21)+im42 + xpp21*im22)/
c>     &        (n42+n22-l24+2d0*xpp21*(n22-m22))
c>          ixpp22=(1d0+xpp12)*(-in*(1d0+xpp22)+im42 + xpp21*im22)/
c>     &        (n42+n22-l24+2d0*xpp22*(n22-m22))

          ix21 = fixix(x21,xpp21,-(1d0+be2)*dreal(xpp21*xpp21),
     &        m32,l34,m42,n32,n42,
     &        m42,l24,m22,n42,n22,
     &        in,im32,im42,im42,im22)
          
          ix22 = fixix(x22,xpp22,-(1d0+be2)*dreal(xpp22*xpp22),
     &        m32,l34,m42,n32,n42,
     &        m42,l24,m22,n42,n22,
     &        in,im32,im42,im42,im22)

          ixpp21=-(1d0+be2)*ix21
          ixpp22=-(1d0+be2)*ix22

#ifdef WRITESD
          write(*,*)
          write(*,*) 'd0regsd:   x2pp',xpp21,xpp22,ixpp21,ixpp22
          write(*,*) 'd0regsd: -ln(m)',-cln_coli(m42,-1d0)
          write(*,*) 'd0regsd:  ln(M)', cln_coli(n42,in)
          write(*,*) 'd0regsd:mj2/mi2',n22/n42,-in*(n22-n42)
          write(*,*) 'd0regsd: root1 ',r24
          write(*,*) 'd0regsd: root2 ',1d0/r42
#endif
          
          D0regsd_coli = D0regsd_coli 
     &        -lreg_coli(xpp21,m42,n42,n22,r24,1d0/r42,ir24,in,ixpp21)
     &        +lreg_coli(xpp22,m42,n42,n22,r24,1d0/r42,ir24,in,ixpp22)
          
#ifdef WRITESD
          write(*,*) 'd0regsd: sum22 ',D0regsd_coli
#endif
         
        endif
 
        x21=z21/(1d0-z21)
        x22=z22/(1d0-z22)

c i=3 j=4
c>          ix21=(1d0+x21)*(-in*(1d0+x21)+im32 + x21*im42)/
c>     &        (n32+n42-l34+2d0*x21*(n42-m42))
c>          ix22=(1d0+x22)*(-in*(1d0+x22)+im32 + x21*im42)/
c>     &        (n32+n42-l34+2d0*x22*(n42-m42))

        if(ix21*ixpp21*(1d0+be2).gt.0d0) then
          write(*,*) 'ixpp21 inconsistent'
        endif
        if(ix22*ixpp22*(1d0+be2).gt.0d0) then
          write(*,*) 'ixpp22 inconsistent'
        endif
        if(ix21*ixp21*be2.gt.0d0) then
          write(*,*) 'ixp21 inconsistent'
        endif
        if(ix22*ixp22*be2.gt.0d0) then
          write(*,*) 'ixp22 inconsistent'
        endif

#ifdef WRITESD
        write(*,*) 'D0reg_sd ixp21',ixp21,-ix21*be2
        write(*,*) 'D0reg_sd ixp22',ixp22,-ix22*be2
        write(*,*) 'D0reg_sd ixpp21',ixpp21,-ix21*(1d0+be2)
        write(*,*) 'D0reg_sd ixpp22',ixpp22,-ix22*(1d0+be2)
#endif


#ifdef WRITESD
        write(*,*)
        write(*,*) 'd0regsd:   x2  ',x21,x22,ix21,ix22
        write(*,*) 'd0regsd: -ln(m)',-cln_coli(m32,-1d0)
        write(*,*) 'd0regsd:  ln(M)', cln_coli(n32,in)
        write(*,*) 'd0regsd:mj2/mi2',n42/n32,-in*(n42-n32)
        write(*,*) 'd0regsd: root1 ',r43
        write(*,*) 'd0regsd: root2 ',1d0/r34
#endif

        D0regsd_coli = D0regsd_coli 
     &      -lreg_coli(x21,m32,n32,n42,r43,1d0/r34,ir34,in,ix21)
     &      +lreg_coli(x22,m32,n32,n42,r43,1d0/r34,ir34,in,ix22)

#ifdef WRITESD
        write(*,*) 'd0regsd: sum23 ',D0regsd_coli
#endif

      endif

#ifdef WRITESD
      write(*,*) 'd0regsd: sum2 ',D0regsd_coli
#endif


      a3 = p13*pab(al,be3)
     &    -pa(-1d0,0d0,1d0,0d0,al)*pb(-1d0,0d0,1d0,0d0,be3)

      b3 = (l13-2d0*m12)*pab(al,be3)
     &    -pa(1d0,0d0,0d0,0d0,al)*pb(-1d0,0d0,1d0,0d0,be3)
     &    -pa(-1d0,0d0,1d0,0d0,al)*pb(1d0,0d0,0d0,0d0,be3)

      c3 = m12*pab(al,be3)
     &    -pa(1d0,0d0,0d0,0d0,al)*pb(1d0,0d0,0d0,0d0,be3)

      det = sqrt(b3*b3-4d0*a3*c3)

      if(dreal(b3).gt.0d0) then
        z32 = (-b3-det)/(2d0*a3)
        z31 = c3/(a3*z32)
      else
        z31 = (-b3+det)/(2d0*a3)
        z32 = c3/(a3*z31)
      endif

      ix31=1d0
      ix32=1d0

      if(be3.ne.0d0)then
        xp31=-z31/(z31+be3)
        xp32=-z32/(z32+be3)
        ixp31=-be3*ix31
        ixp32=-be3*ix32
        
        x31=z31/(1d0-z31)
        x32=z32/(1d0-z32)

c i=1 j=2
c>      ixp31=(1d0+xp31)*(-in*(1d0+xp31)+im12 + xp31*im22)/
c>     &    (n12+n22-l12+2d0*xp31*(n22-m22))
c>      ixp32=(1d0+xp32)*(-in*(1d0+xp32)+im12 + xp31*im22)/
c>     &    (n12+n22-l12+2d0*xp32*(n22-m22))

        ix31 = fixix(x31,xp31,-be3*dreal(xp31*xp31/(x31*x31)),
     &      m12,l13,m32,n12,n32,
     &      m12,l12,m22,n12,n22,
     &      in,im12,im32,im12,im22)
          
        ix32 = fixix(x32,xp32,-be3*dreal(xp32*xp32/(x32*x32)),
     &      m12,l13,m32,n12,n32,
     &      m12,l12,m22,n12,n22,
     &      in,im12,im32,im12,im22)
        
        ixp31=-be3*ix31
        ixp32=-be3*ix32

#ifdef WRITESD
        write(*,*)
        write(*,*) 'd0regsd:   x3p ',xp31,xp32,ixp31,ixp32
        write(*,*) 'd0regsd: -ln(m)',-cln_coli(m12,-1d0)
        write(*,*) 'd0regsd:  ln(M)', cln_coli(n12,in)
        write(*,*) 'd0regsd: Mj2/Mi2',n22/n12,-in*(n22-n12)
        write(*,*) 'd0regsd: root1 ',r21
        write(*,*) 'd0regsd: root2 ',1d0/r12
#endif
        
        D0regsd_coli = D0regsd_coli
     &      +lreg_coli(xp31,m12,n12,n22,r21,1d0/r12,ir12,in,ixp31)
     &      -lreg_coli(xp32,m12,n12,n22,r21,1d0/r12,ir12,in,ixp32)
        
#ifdef WRITESD
        write(*,*) 'd0regsd: sum31 ',D0regsd_coli
#endif

      endif

      if(be3.ne.-1d0)then
        xpp31=(1d0-z31)/(z31+be3)
        xpp32=(1d0-z32)/(z32+be3)
        ixpp31=-(1d0+be3)*ix31
        ixpp32=-(1d0+be3)*ix32
        
        x31=z31/(1d0-z31)
        x32=z32/(1d0-z32)

c i=3 j=2
c>      ixpp31=(1d0+xpp31)*(-in*(1d0+xpp31)+im32 + xpp31*im22)/
c>     &    (n32+n22-l23+2d0*xpp31*(n22-m22))
c>      ixpp32=(1d0+xpp32)*(-in*(1d0+xpp32)+im32 + xpp31*im22)/
c>     &    (n32+n22-l23+2d0*xpp32*(n22-m22))

        ix31 = fixix(x31,xpp31,-(1d0+be3)*dreal(xpp31*xpp31),
     &      m12,l13,m32,n12,n32,
     &      m32,l23,m22,n32,n22,
     &      in,im12,im32,im32,im22)
        
        ix32 = fixix(x32,xpp32,-(1d0+be3)*dreal(xpp32*xpp32),
     &      m12,l13,m32,n12,n32,
     &      m32,l23,m22,n32,n22,
     &      in,im12,im32,im32,im22)

        ixpp31=-(1d0+be3)*ix31
        ixpp32=-(1d0+be3)*ix32

#ifdef WRITESD
        write(*,*)
        write(*,*) 'd0regsd:   x3pp',xpp31,xpp32,ixpp31,ixpp32
        write(*,*) 'd0regsd: -ln(m)',-cln_coli(m32,-1d0)
        write(*,*) 'd0regsd:  ln(M)', cln_coli(n32,in)
        write(*,*) 'd0regsd:mj2/mi2',n22/n32,-in*(n22-n32)
        write(*,*) 'd0regsd: root1 ',r23
        write(*,*) 'd0regsd: root2 ',1d0/r32
#endif

        D0regsd_coli = D0regsd_coli 
     &      -lreg_coli(xpp31,m32,n32,n22,r23,1d0/r32,ir23,in,ixpp31)
     &      +lreg_coli(xpp32,m32,n32,n22,r23,1d0/r32,ir23,in,ixpp32)
        
#ifdef WRITESD
        write(*,*) 'd0regsd: sum32 ',D0regsd_coli
#endif
      
      endif

      x31=z31/(1d0-z31)
      x32=z32/(1d0-z32)

c i=1 j=3
c>      ix31=(1d0+x31)*(-in*(1d0+x31)+im12 + x31*im32)/
c>     &    (n12+n32-l13+2d0*x31*(n32-m32))
c>      ix32=(1d0+x32)*(-in*(1d0+x32)+im12 + x31*im32)/
c>     &    (n12+n32-l13+2d0*x32*(n32-m32))

      if(ix31*ixpp31*(1d0+be3).gt.0d0) then
        write(*,*) 'ixpp31 inconsistent'
      endif
      if(ix32*ixpp32*(1d0+be3).gt.0d0) then
        write(*,*) 'ixpp32 inconsistent'
      endif
      if(ix31*ixp31*be3.gt.0d0) then
        write(*,*) 'ixp31 inconsistent'
      endif
      if(ix32*ixp32*be3.gt.0d0) then
        write(*,*) 'ixp32 inconsistent'
      endif

#ifdef WRITESD
      write(*,*) 'D0reg_sd ixp31',ixp31,-ix31*be3
      write(*,*) 'D0reg_sd ixp32',ixp32,-ix32*be3
      write(*,*) 'D0reg_sd ixpp31',ixpp31,-ix31*(1d0+be3)
      write(*,*) 'D0reg_sd ixpp32',ixpp32,-ix32*(1d0+be3)
#endif


#ifdef WRITESD
      write(*,*)
      write(*,*) 'd0regsd:   x3  ',x31,x32,ix31,ix32
      write(*,*) 'd0regsd: -ln(m)',-cln_coli(m12,-1d0)
      write(*,*) 'd0regsd:  ln(M)', cln_coli(n12,in)
      write(*,*) 'd0regsd:mj2/mi2',n32/n12,-in*(n32-n12)
      write(*,*) 'd0regsd: root1 ',r31
      write(*,*) 'd0regsd: root2 ',1d0/r13
#endif

      D0regsd_coli = D0regsd_coli 
     &    -lreg_coli(x31,m12,n12,n32,r31,1d0/r13,ir13,in,ix31)
     &    +lreg_coli(x32,m12,n12,n32,r31,1d0/r13,ir13,in,ix32)


#ifdef WRITESD
      write(*,*) 'd0regsd: sum33 ',D0regsd_coli
#endif

      det = sqrt(b3*b3-4d0*a3*c3)

#ifdef WRITESD
      write(*,*) 'd0regsd: det = ',sqrt(b1*b1-4d0*a1*c1),
     &    sqrt(b2*b2-4d0*a2*c2),
     &    sqrt(b3*b3-4d0*a3*c3),det

       write(*,*) 'D0regsd_coli sum ',D0regsd_coli,det     
#endif

      D0regsd_coli = D0regsd_coli/det

#ifdef WRITESD
      write(*,*)
      write(*,*) 'D0regsd_coli= ',D0regsd_coli    
      write(*,*)
#endif

#ifdef CHECK
#ifdef D0MASS

      if(conscheck)then

        D0regsd_check=
     &      D0mass_std(p23,p34,p14,p12,p24,p13,m22,m32,m42,m12)

        if(cdabs(D0regsd_coli/D0regsd_check-1d0).gt.testacc) then
          write(testout,*) ' D0regsd_coli inconsistent std'
          write(testout,111) ' D0regsd_coli: p12 = ',p12
          write(testout,111) ' D0regsd_coli: p23 = ',p23
          write(testout,111) ' D0regsd_coli: p34 = ',p34
          write(testout,111) ' D0regsd_coli: p14 = ',p14
          write(testout,111) ' D0regsd_coli: p13 = ',p13
          write(testout,111) ' D0regsd_coli: p24 = ',p24
          write(testout,111) ' D0regsd_coli: m12 = ',m12
          write(testout,111) ' D0regsd_coli: m22 = ',m22
          write(testout,111) ' D0regsd_coli: m32 = ',m32
          write(testout,111) ' D0regsd_coli: m42 = ',m42
          write(testout,111) ' D0regsd_coli  = ',D0regsd_coli
          write(testout,111) ' D0regsd_check = ',D0regsd_check
          write(testout,101) ' diff        = ',
     &        cdabs(D0regsd_coli/D0regsd_check-1d0)
          
          write(*,*)   ' D0regsd_coli inconsistent std'
          write(*,111) ' D0regsd_coli: p12 = ',p12
          write(*,111) ' D0regsd_coli: p23 = ',p23
          write(*,111) ' D0regsd_coli: p34 = ',p34
          write(*,111) ' D0regsd_coli: p14 = ',p14
          write(*,111) ' D0regsd_coli: p13 = ',p13
          write(*,111) ' D0regsd_coli: p24 = ',p24
          write(*,111) ' D0regsd_coli: m12 = ',m12
          write(*,111) ' D0regsd_coli: m22 = ',m22
          write(*,111) ' D0regsd_coli: m32 = ',m32
          write(*,111) ' D0regsd_coli: m42 = ',m42
          write(*,111) ' D0regsd_coli  = ',D0regsd_coli
          write(*,111) ' D0regsd_check = ',D0regsd_check
          write(*,101) ' diff        = ',
     &        cdabs(D0regsd_coli/D0regsd_check-1d0)
c          stop
        endif

      endif
#endif
#endif

      end

************************************************************************
      function lreg_coli(x,m12,n12,n22,r1,r2,ir,in,ix)
************************************************************************
*  auxiliary function L_ij for                                         *
*  regular case  of D0 by Stefan Dittmaier                             *
*----------------------------------------------------------------------*
*  05.01.10 Ansgar Denner       last changed 21.01.10 Ansgar Denner    *
************************************************************************
      implicit none
      complex*16  lreg_coli
      complex*16  x,m12,n12,n22,r1,r2
      real*8      ix,ir,in
      complex*16  cspcos_coli,cln_coli,cspenc_coli

      include "coli_params.h"
      include "coli_checkparams.h"

c      ix = -1d0
      if(n12.ne.cd0) then
        lreg_coli= -(cln_coli(n12,in)-log(m12))*cln_coli(-x,-ix)
     &      +cspenc_coli(1d0+x,ix)
     &      +cspcos_coli(n22/n12,-x,in*dreal(n12-n22),-ix)
     &      -cspcos_coli(r1,-x,ir,-ix)
     &      -cspcos_coli(r2,-x,-ir,-ix)

#ifdef WRITESD
        write(*,*) 'lreg_coli contr',
     &   -(cln_coli(n12,in)-log(m12))*cln_coli(-x,-ix),
     &      +cspenc_coli(1d0+x,ix),
     &      +cspcos_coli(n22/n12,-x,in*dreal(n12-n22),-ix),
     &      -cspcos_coli(r1,-x,ir,-ix),
     &      -cspcos_coli(r2,-x,-ir,-ix)    
#endif

      else
        lreg_coli= -(cln_coli(n22,in)-log(m12))*cln_coli(-x,-ix)
     &      +cspenc_coli(1d0+x,ix)
     &      - .5d0*(cln_coli(-x,-ix))**2
     &      -cspcos_coli(r1,-x,ir,-ix)
     &      -cspcos_coli(r2,-x,-ir,-ix)
      endif    

#ifdef WRITESD
        write(*,*) 'lreg_coli res',lreg_coli
#endif

      end

#ifdef D0MASS
**********************************************************************
        function D0mass_std(q1,q2,q3,q4,q12,q23,m12,m22,m32,m42)
**********************************************************************
*       scalar 4-point function with all masses =/= 0
*	masses can be complex
*
*             q1 \           / q4
*                 \   m12   /
*                  *-------*
*                  |       |
*              m22 |       | m42
*                  |       |
*                  *-------*
*                 /   m32   \
*             q2 /           \ q3
*---------------------------------------------------------------------
*       2.4.2008 Stefan Dittmaier
**********************************************************************
        implicit real*8 (a-z)
        complex*16 D0mass_std,sqe_dd,cspen_dd,eta_dd,ieps
        complex*16 m12,m22,m32,m42,m2(4),m2e(4),caux,Y(4,4),Ye(4,4)
        complex*16 Ax(3),Bx(3),Cx(3),Axe(3),Bxe(3),Cxe(3)
        complex*16 x(9,2),xx(9,2),xe(9,2),pre,pre1,pre2
	complex*16 arg(0:9),arge(0:9),l(9),argl(9),argle(9),etal
	complex*16 r(9),re(9),s(9,2),se(9,2),rx(9,2),sx(9,2,2)
        real*8 p2(4,4),beta(3)
        integer i,j,k

        integer error_code
        common /coli_error/ error_code
        save /coli_error/

	if (m12*m22*m32*m42.eq.(0d0,0d0)) then
          write(*,*) 'D0mass_dd: 4 masses must be non-zero !'
	  error_code = 1
	endif

	eps   = 1d-13
	ieps  = dcmplx(0d0,eps)
	eps2  = 1d-20
	crit  = 1d-10
	critcalc = 1d-10

        p2(1,2) = q1
        p2(2,3) = q2
        p2(3,4) = q3
        p2(1,4) = q4
        p2(1,3) = q12
        p2(2,4) = q23
        m2(1) = m12
        m2(2) = m22
        m2(3) = m32
        m2(4) = m42

        m2e(1) = m2(1)-abs(m2(1))*ieps
        m2e(2) = m2(2)-abs(m2(2))*ieps*1.3d0
        m2e(3) = m2(3)-abs(m2(3))*ieps*2.1d0
        m2e(4) = m2(4)-abs(m2(4))*ieps*3.1d0

        do i=1,3
        do j=i+1,4
          Y(i,j)  = m2(i)+m2(j)-p2(i,j)
          Ye(i,j) = m2e(i)+m2e(j)-p2(i,j)
        enddo
        enddo

c real variables for shifts in Euler transformations
	if ((q23+q3-q2)**2-4d0*q23*q3.gt.0d0) then
	  alp = sqe_dd(dcmplx(q23),dcmplx(-q2+q23+q3),dcmplx(q3))
	else
	  write(*,*) 'D0mass_std: complex alp not supported'
	  error_code = 1
	endif

#ifdef WRITESTD
        write(*,*) 'D0mass_std: alpha',q23,q2,q3
#endif

	if ((q12+q3-q4)**2-4d0*q12*q3.gt.0d0) then
	  beta(1) = sqe_dd(dcmplx(q3),dcmplx(q12+q3-q4),dcmplx(q12))
	else
	  write(*,*) 'D0mass_std: complex beta(1) not supported'
	  error_code = 1
	endif

        if(q12.eq.0d0) beta(1)=0d0

	if ((q1+q2-q12)**2-4d0*q1*q2.gt.0d0) then
	  beta(2) = sqe_dd(dcmplx(q2),dcmplx(q1+q2-q12),dcmplx(q1))
	else
	  write(*,*) 'D0mass_std: complex beta(2) not supported'
	  error_code = 1
	endif

	if ((q23+q4-q1)**2-4d0*q23*q4.gt.0d0) then
	  beta(3) = sqe_dd(dcmplx(q23),dcmplx(q23+q4-q1),dcmplx(q4))
	else
	  write(*,*) 'D0mass_std: complex beta(3) not supported'
	  error_code = 1
	endif

        if (abs(alp).lt.critcalc) alp = 0d0
        if (abs(alp+1d0).lt.critcalc) alp = -1d0
        do i=1,3
          if (abs(beta(i)).lt.critcalc) beta(i) = 0d0
          if (abs(beta(i)+1d0).lt.critcalc) beta(i) = -1d0
        enddo

#ifdef WRITESTD
        write(*,*) 'D0mass_std: al ',alp,beta(1),beta(2),beta(3)
#endif

c complex solutions of quadratic equations for denominators
        AX(1) = -(2D0*M2(3)*M2(4))-2D0*BETA(1)*M2(3)*M2(4)+2D0*
     &  BETA(1)*M2(4)**2+M2(4)*Y(1,3)+M2(4)*Y(1,4)-M2(4)*Y(3,4)-2D0*
     &  BETA(1)*M2(4)*Y(3,4)-Y(1,4)*Y(3,4)+Y(3,4)**2+BETA(1)*Y(3,
     &  4)**2+2D0*BETA(1)*M2(4)**2*ALP+M2(4)*Y(1,2)*ALP+M2(4)*Y(1,
     &  4)*ALP-M2(4)*Y(2,3)*ALP-BETA(1)*M2(4)*Y(2,3)*ALP-BETA(1)*
     &  M2(4)*Y(2,4)*ALP-Y(1,4)*Y(2,4)*ALP-M2(4)*Y(3,4)*ALP-BETA(1)*
     &  M2(4)*Y(3,4)*ALP+Y(2,4)*Y(3,4)*ALP+BETA(1)*Y(2,4)*Y(3,4)*ALP
        BX(1) = -(4D0*M2(3)*M2(4))-8D0*BETA(1)*M2(3)*M2(4)+2D0*
     &  M2(4)*Y(1,3)-2D0*M2(3)*Y(1,4)+2D0*M2(3)*Y(3,4)+2D0*BETA(1)*
     &  M2(3)*Y(3,4)+2D0*BETA(1)*M2(4)*Y(3,4)-4D0*M2(3)*M2(4)*ALP-
     &  4D0*BETA(1)*M2(3)*M2(4)*ALP+2D0*M2(4)*Y(1,3)*ALP-2D0*
     &  BETA(1)*M2(4)*Y(2,3)*ALP-Y(1,4)*Y(2,3)*ALP+2D0*M2(3)*Y(2,4)*
     &  ALP+2D0*BETA(1)*M2(3)*Y(2,4)*ALP-Y(1,3)*Y(2,4)*ALP+2D0*
     &  BETA(1)*M2(4)*Y(3,4)*ALP+Y(1,2)*Y(3,4)*ALP
        CX(1) = 2D0*M2(3)**2+2D0*BETA(1)*M2(3)**2-2D0*BETA(1)*M2(3)*
     &  M2(4)-M2(3)*Y(1,3)-M2(3)*Y(1,4)-M2(3)*Y(3,4)-2D0*BETA(1)*
     &  M2(3)*Y(3,4)+Y(1,3)*Y(3,4)+BETA(1)*Y(3,4)**2-2D0*BETA(1)*
     &  M2(3)*M2(4)*ALP+M2(3)*Y(1,2)*ALP-M2(3)*Y(1,4)*ALP+M2(3)*Y(2,
     &  3)*ALP+BETA(1)*M2(3)*Y(2,3)*ALP-Y(1,3)*Y(2,3)*ALP+BETA(1)*
     &  M2(3)*Y(2,4)*ALP-M2(3)*Y(3,4)*ALP-BETA(1)*M2(3)*Y(3,4)*ALP+
     &  Y(1,3)*Y(3,4)*ALP-BETA(1)*Y(2,3)*Y(3,4)*ALP+BETA(1)*Y(3,4)**
     &  2*ALP
        AX(2) = -(2D0*BETA(2)*M2(3)**2)-M2(3)*Y(1,3)-M2(3)*Y(1,4)+
     &  M2(3)*Y(2,3)+BETA(2)*M2(3)*Y(2,3)+M2(3)*Y(2,4)+BETA(2)*
     &  M2(3)*Y(2,4)+BETA(2)*M2(3)*Y(3,4)+Y(1,3)*Y(3,4)-Y(2,3)*Y(3,
     &  4)-BETA(2)*Y(2,3)*Y(3,4)-2D0*M2(2)*M2(3)*ALP-2D0*BETA(2)*
     &  M2(2)*M2(3)*ALP+M2(3)*Y(1,2)*ALP-M2(3)*Y(1,4)*ALP-BETA(2)*
     &  M2(3)*Y(2,3)*ALP-Y(1,3)*Y(2,3)*ALP+Y(2,3)**2*ALP+BETA(2)*
     &  Y(2,3)**2*ALP+M2(3)*Y(2,4)*ALP+BETA(2)*M2(3)*Y(2,4)*ALP+
     &  BETA(2)*M2(3)*Y(3,4)*ALP+Y(1,3)*Y(3,4)*ALP-Y(2,3)*Y(3,4)*
     &  ALP-BETA(2)*Y(2,3)*Y(3,4)*ALP
        BX(2) = 4D0*M2(2)*M2(3)+4D0*BETA(2)*M2(2)*M2(3)-2D0*M2(3)*
     &  Y(1,2)-2D0*BETA(2)*M2(3)*Y(2,3)-Y(1,4)*Y(2,3)+2D0*BETA(2)*
     &  M2(3)*Y(2,4)+Y(1,3)*Y(2,4)-2D0*M2(2)*Y(3,4)-2D0*BETA(2)*
     &  M2(2)*Y(3,4)+Y(1,2)*Y(3,4)-4D0*BETA(2)*M2(2)*M2(3)*ALP-2D0*
     &  M2(2)*Y(1,3)*ALP+2D0*M2(2)*Y(2,3)*ALP+2D0*BETA(2)*M2(2)*Y(2,
     &  3)*ALP-Y(1,4)*Y(2,3)*ALP+2D0*BETA(2)*M2(3)*Y(2,4)*ALP+Y(1,
     &  3)*Y(2,4)*ALP-2D0*M2(2)*Y(3,4)*ALP-2D0*BETA(2)*M2(2)*Y(3,4)*
     &  ALP+Y(1,2)*Y(3,4)*ALP
        CX(2) = 2D0*BETA(2)*M2(2)*M2(3)+M2(2)*Y(1,3)-M2(2)*Y(1,4)+
     &  M2(2)*Y(2,3)+BETA(2)*M2(2)*Y(2,3)-Y(1,2)*Y(2,3)-BETA(2)*Y(2,
     &  3)**2-M2(2)*Y(2,4)-BETA(2)*M2(2)*Y(2,4)+Y(1,2)*Y(2,4)+
     &  BETA(2)*Y(2,3)*Y(2,4)-BETA(2)*M2(2)*Y(3,4)+2D0*M2(2)**2*ALP+
     &  2D0*BETA(2)*M2(2)**2*ALP-M2(2)*Y(1,2)*ALP-M2(2)*Y(1,4)*ALP-
     &  BETA(2)*M2(2)*Y(2,3)*ALP-M2(2)*Y(2,4)*ALP-BETA(2)*M2(2)*Y(2,
     &  4)*ALP+Y(1,2)*Y(2,4)*ALP+BETA(2)*Y(2,3)*Y(2,4)*ALP-BETA(2)*
     &  M2(2)*Y(3,4)*ALP
        AX(3) = 2D0*M2(2)*M2(4)+2D0*BETA(3)*M2(2)*M2(4)+M2(2)*Y(1,
     &  3)-M2(2)*Y(1,4)-BETA(3)*M2(2)*Y(2,3)-Y(1,2)*Y(2,3)+BETA(3)*
     &  M2(2)*Y(2,4)+Y(1,2)*Y(2,4)+Y(2,3)*Y(2,4)+BETA(3)*Y(2,3)*Y(2,
     &  4)-Y(2,4)**2-BETA(3)*Y(2,4)**2-M2(2)*Y(3,4)-BETA(3)*M2(2)*
     &  Y(3,4)-2D0*BETA(3)*M2(2)**2*ALP+2D0*M2(2)*M2(4)*ALP+2D0*
     &  BETA(3)*M2(2)*M2(4)*ALP-M2(2)*Y(1,2)*ALP-M2(2)*Y(1,4)*ALP+
     &  M2(2)*Y(2,4)*ALP+2D0*BETA(3)*M2(2)*Y(2,4)*ALP+Y(1,2)*Y(2,4)*
     &  ALP-Y(2,4)**2*ALP-BETA(3)*Y(2,4)**2*ALP
        BX(3) = 4D0*BETA(3)*M2(2)*M2(4)+2D0*M2(4)*Y(1,2)+2D0*M2(4)*
     &  Y(2,3)+2D0*BETA(3)*M2(4)*Y(2,3)-Y(1,4)*Y(2,3)-2D0*M2(4)*Y(2,
     &  4)-2D0*BETA(3)*M2(4)*Y(2,4)+Y(1,3)*Y(2,4)-2D0*BETA(3)*M2(2)*
     &  Y(3,4)-Y(1,2)*Y(3,4)+4D0*M2(2)*M2(4)*ALP+8D0*BETA(3)*M2(2)*
     &  M2(4)*ALP+2D0*M2(4)*Y(1,2)*ALP-2D0*M2(2)*Y(1,4)*ALP-2D0*
     &  BETA(3)*M2(2)*Y(2,4)*ALP-2D0*M2(4)*Y(2,4)*ALP-2D0*BETA(3)*
     &  M2(4)*Y(2,4)*ALP
        CX(3) = -(2D0*M2(4)**2)-2D0*BETA(3)*M2(4)**2+M2(4)*Y(1,3)+
     &  M2(4)*Y(1,4)+BETA(3)*M2(4)*Y(2,3)+BETA(3)*M2(4)*Y(2,4)+
     &  M2(4)*Y(3,4)+BETA(3)*M2(4)*Y(3,4)-Y(1,4)*Y(3,4)-BETA(3)*Y(2,
     &  4)*Y(3,4)+2D0*BETA(3)*M2(2)*M2(4)*ALP-2D0*M2(4)**2*ALP-2D0*
     &  BETA(3)*M2(4)**2*ALP+M2(4)*Y(1,2)*ALP+M2(4)*Y(1,4)*ALP+
     &  M2(4)*Y(2,4)*ALP+2D0*BETA(3)*M2(4)*Y(2,4)*ALP-Y(1,4)*Y(2,4)*
     &  ALP-BETA(3)*Y(2,4)**2*ALP

        AXE(1) = -(2D0*M2E(3)*M2E(4))-2D0*BETA(1)*M2E(3)*M2E(4)+2D0*
     &  BETA(1)*M2E(4)**2+M2E(4)*YE(1,3)+M2E(4)*YE(1,4)-M2E(4)*YE(3,
     &  4)-2D0*BETA(1)*M2E(4)*YE(3,4)-YE(1,4)*YE(3,4)+YE(3,4)**2+
     &  BETA(1)*YE(3,4)**2+2D0*BETA(1)*M2E(4)**2*ALP+M2E(4)*YE(1,2)*
     &  ALP+M2E(4)*YE(1,4)*ALP-M2E(4)*YE(2,3)*ALP-BETA(1)*M2E(4)*
     &  YE(2,3)*ALP-BETA(1)*M2E(4)*YE(2,4)*ALP-YE(1,4)*YE(2,4)*ALP-
     &  M2E(4)*YE(3,4)*ALP-BETA(1)*M2E(4)*YE(3,4)*ALP+YE(2,4)*YE(3,
     &  4)*ALP+BETA(1)*YE(2,4)*YE(3,4)*ALP
        BXE(1) = -(4D0*M2E(3)*M2E(4))-8D0*BETA(1)*M2E(3)*M2E(4)+2D0*
     &  M2E(4)*YE(1,3)-2D0*M2E(3)*YE(1,4)+2D0*M2E(3)*YE(3,4)+2D0*
     &  BETA(1)*M2E(3)*YE(3,4)+2D0*BETA(1)*M2E(4)*YE(3,4)-4D0*
     &  M2E(3)*M2E(4)*ALP-4D0*BETA(1)*M2E(3)*M2E(4)*ALP+2D0*M2E(4)*
     &  YE(1,3)*ALP-2D0*BETA(1)*M2E(4)*YE(2,3)*ALP-YE(1,4)*YE(2,3)*
     &  ALP+2D0*M2E(3)*YE(2,4)*ALP+2D0*BETA(1)*M2E(3)*YE(2,4)*ALP-
     &  YE(1,3)*YE(2,4)*ALP+2D0*BETA(1)*M2E(4)*YE(3,4)*ALP+YE(1,2)*
     &  YE(3,4)*ALP
        CXE(1) = 2D0*M2E(3)**2+2D0*BETA(1)*M2E(3)**2-2D0*BETA(1)*
     &  M2E(3)*M2E(4)-M2E(3)*YE(1,3)-M2E(3)*YE(1,4)-M2E(3)*YE(3,4)-
     &  2D0*BETA(1)*M2E(3)*YE(3,4)+YE(1,3)*YE(3,4)+BETA(1)*YE(3,4)**
     &  2-2D0*BETA(1)*M2E(3)*M2E(4)*ALP+M2E(3)*YE(1,2)*ALP-M2E(3)*
     &  YE(1,4)*ALP+M2E(3)*YE(2,3)*ALP+BETA(1)*M2E(3)*YE(2,3)*ALP-
     &  YE(1,3)*YE(2,3)*ALP+BETA(1)*M2E(3)*YE(2,4)*ALP-M2E(3)*YE(3,
     &  4)*ALP-BETA(1)*M2E(3)*YE(3,4)*ALP+YE(1,3)*YE(3,4)*ALP-
     &  BETA(1)*YE(2,3)*YE(3,4)*ALP+BETA(1)*YE(3,4)**2*ALP
        AXE(2) = -(2D0*BETA(2)*M2E(3)**2)-M2E(3)*YE(1,3)-M2E(3)*
     &  YE(1,4)+M2E(3)*YE(2,3)+BETA(2)*M2E(3)*YE(2,3)+M2E(3)*YE(2,
     &  4)+BETA(2)*M2E(3)*YE(2,4)+BETA(2)*M2E(3)*YE(3,4)+YE(1,3)*
     &  YE(3,4)-YE(2,3)*YE(3,4)-BETA(2)*YE(2,3)*YE(3,4)-2D0*M2E(2)*
     &  M2E(3)*ALP-2D0*BETA(2)*M2E(2)*M2E(3)*ALP+M2E(3)*YE(1,2)*ALP-
     &  M2E(3)*YE(1,4)*ALP-BETA(2)*M2E(3)*YE(2,3)*ALP-YE(1,3)*YE(2,
     &  3)*ALP+YE(2,3)**2*ALP+BETA(2)*YE(2,3)**2*ALP+M2E(3)*YE(2,4)*
     &  ALP+BETA(2)*M2E(3)*YE(2,4)*ALP+BETA(2)*M2E(3)*YE(3,4)*ALP+
     &  YE(1,3)*YE(3,4)*ALP-YE(2,3)*YE(3,4)*ALP-BETA(2)*YE(2,3)*
     &  YE(3,4)*ALP
        BXE(2) = 4D0*M2E(2)*M2E(3)+4D0*BETA(2)*M2E(2)*M2E(3)-2D0*
     &  M2E(3)*YE(1,2)-2D0*BETA(2)*M2E(3)*YE(2,3)-YE(1,4)*YE(2,3)+
     &  2D0*BETA(2)*M2E(3)*YE(2,4)+YE(1,3)*YE(2,4)-2D0*M2E(2)*YE(3,
     &  4)-2D0*BETA(2)*M2E(2)*YE(3,4)+YE(1,2)*YE(3,4)-4D0*BETA(2)*
     &  M2E(2)*M2E(3)*ALP-2D0*M2E(2)*YE(1,3)*ALP+2D0*M2E(2)*YE(2,3)*
     &  ALP+2D0*BETA(2)*M2E(2)*YE(2,3)*ALP-YE(1,4)*YE(2,3)*ALP+2D0*
     &  BETA(2)*M2E(3)*YE(2,4)*ALP+YE(1,3)*YE(2,4)*ALP-2D0*M2E(2)*
     &  YE(3,4)*ALP-2D0*BETA(2)*M2E(2)*YE(3,4)*ALP+YE(1,2)*YE(3,4)*
     &  ALP
        CXE(2) = 2D0*BETA(2)*M2E(2)*M2E(3)+M2E(2)*YE(1,3)-M2E(2)*
     &  YE(1,4)+M2E(2)*YE(2,3)+BETA(2)*M2E(2)*YE(2,3)-YE(1,2)*YE(2,
     &  3)-BETA(2)*YE(2,3)**2-M2E(2)*YE(2,4)-BETA(2)*M2E(2)*YE(2,4)+
     &  YE(1,2)*YE(2,4)+BETA(2)*YE(2,3)*YE(2,4)-BETA(2)*M2E(2)*YE(3,
     &  4)+2D0*M2E(2)**2*ALP+2D0*BETA(2)*M2E(2)**2*ALP-M2E(2)*YE(1,
     &  2)*ALP-M2E(2)*YE(1,4)*ALP-BETA(2)*M2E(2)*YE(2,3)*ALP-M2E(2)*
     &  YE(2,4)*ALP-BETA(2)*M2E(2)*YE(2,4)*ALP+YE(1,2)*YE(2,4)*ALP+
     &  BETA(2)*YE(2,3)*YE(2,4)*ALP-BETA(2)*M2E(2)*YE(3,4)*ALP
        AXE(3) = 2D0*M2E(2)*M2E(4)+2D0*BETA(3)*M2E(2)*M2E(4)+M2E(2)*
     &  YE(1,3)-M2E(2)*YE(1,4)-BETA(3)*M2E(2)*YE(2,3)-YE(1,2)*YE(2,
     &  3)+BETA(3)*M2E(2)*YE(2,4)+YE(1,2)*YE(2,4)+YE(2,3)*YE(2,4)+
     &  BETA(3)*YE(2,3)*YE(2,4)-YE(2,4)**2-BETA(3)*YE(2,4)**2-
     &  M2E(2)*YE(3,4)-BETA(3)*M2E(2)*YE(3,4)-2D0*BETA(3)*M2E(2)**2*
     &  ALP+2D0*M2E(2)*M2E(4)*ALP+2D0*BETA(3)*M2E(2)*M2E(4)*ALP-
     &  M2E(2)*YE(1,2)*ALP-M2E(2)*YE(1,4)*ALP+M2E(2)*YE(2,4)*ALP+
     &  2D0*BETA(3)*M2E(2)*YE(2,4)*ALP+YE(1,2)*YE(2,4)*ALP-YE(2,4)**
     &  2*ALP-BETA(3)*YE(2,4)**2*ALP
        BXE(3) = 4D0*BETA(3)*M2E(2)*M2E(4)+2D0*M2E(4)*YE(1,2)+2D0*
     &  M2E(4)*YE(2,3)+2D0*BETA(3)*M2E(4)*YE(2,3)-YE(1,4)*YE(2,3)-
     &  2D0*M2E(4)*YE(2,4)-2D0*BETA(3)*M2E(4)*YE(2,4)+YE(1,3)*YE(2,
     &  4)-2D0*BETA(3)*M2E(2)*YE(3,4)-YE(1,2)*YE(3,4)+4D0*M2E(2)*
     &  M2E(4)*ALP+8D0*BETA(3)*M2E(2)*M2E(4)*ALP+2D0*M2E(4)*YE(1,2)*
     &  ALP-2D0*M2E(2)*YE(1,4)*ALP-2D0*BETA(3)*M2E(2)*YE(2,4)*ALP-
     &  2D0*M2E(4)*YE(2,4)*ALP-2D0*BETA(3)*M2E(4)*YE(2,4)*ALP
        CXE(3) = -(2D0*M2E(4)**2)-2D0*BETA(3)*M2E(4)**2+M2E(4)*YE(1,
     &  3)+M2E(4)*YE(1,4)+BETA(3)*M2E(4)*YE(2,3)+BETA(3)*M2E(4)*
     &  YE(2,4)+M2E(4)*YE(3,4)+BETA(3)*M2E(4)*YE(3,4)-YE(1,4)*YE(3,
     &  4)-BETA(3)*YE(2,4)*YE(3,4)+2D0*BETA(3)*M2E(2)*M2E(4)*ALP-
     &  2D0*M2E(4)**2*ALP-2D0*BETA(3)*M2E(4)**2*ALP+M2E(4)*YE(1,2)*
     &  ALP+M2E(4)*YE(1,4)*ALP+M2E(4)*YE(2,4)*ALP+2D0*BETA(3)*
     &  M2E(4)*YE(2,4)*ALP-YE(1,4)*YE(2,4)*ALP-BETA(3)*YE(2,4)**2*
     &  ALP


c>        write(*,*) 'D0mass_std: abc1 ',ax(1),bx(1),cx(1)
c>        write(*,*) 'D0mass_std: abc1 ',axe(1),bxe(1),cxe(1)
c>        write(*,*) 'D0mass_std: abc2 ',ax(2),bx(2),cx(2)
c>        write(*,*) 'D0mass_std: abc2 ',axe(2),bxe(2),cxe(2)
c>        write(*,*) 'D0mass_std: abc3 ',ax(3),bx(3),cx(3)
c>        write(*,*) 'D0mass_std: abc3 ',axe(3),bxe(3),cxe(3)

	do i=1,3
          x(3*i,1)  = sqe_dd(ax(i),bx(i),cx(i))
          x(3*i,2)  = cx(i)/ax(i)/x(3*i,1)
          xe(3*i,1) = sqe_dd(axe(i),bxe(i),cxe(i))
          xe(3*i,2) = cxe(i)/axe(i)/xe(3*i,1)
	enddo

c common prefactor
	pre = -1d0/ax(3)/(x(9,1)-x(9,2))
	if (alp.ne.0d0) then
	  pre1 = -alp/ax(1)/(x(3,1)-x(3,2))
	  if (dreal(pre1/pre).lt.0d0) then
	    caux   = x(3,1)
	    x(3,1) = x(3,2)
	    x(3,2) = caux
	  endif
	endif
	if (alp.ne.-1d0) then
	  pre2 = (1d0+alp)/ax(2)/(x(6,1)-x(6,2))
	  if (dreal(pre2/pre).lt.0d0) then
	    caux   = x(6,1)
	    x(6,1) = x(6,2)
	    x(6,2) = caux
	  endif
	endif

	do i=1,3
          if (abs(x(3*i,1)-xe(3*i,1)).gt.abs(x(3*i,1)-xe(3*i,2))) then
            caux      = xe(3*i,1)
            xe(3*i,1) = xe(3*i,2)
            xe(3*i,2) = caux
          endif
	enddo

c remaining zeroes of denominators
	do i=1,3
	  x(3*i-1,1)  = 1d0/(beta(i)+(1d0+beta(i))*x(3*i,1))
	  x(3*i-1,2)  = 1d0/(beta(i)+(1d0+beta(i))*x(3*i,2))
	  xe(3*i-1,1) = 1d0/(beta(i)+(1d0+beta(i))*xe(3*i,1))
	  xe(3*i-1,2) = 1d0/(beta(i)+(1d0+beta(i))*xe(3*i,2))
	  x(3*i-2,1)  = -x(3*i,2)*x(3*i-1,2)
	  x(3*i-2,2)  = -x(3*i,1)*x(3*i-1,1)
	  xe(3*i-2,1) = -xe(3*i,2)*xe(3*i-1,2)
	  xe(3*i-2,2) = -xe(3*i,1)*xe(3*i-1,1)
	enddo

c>        write(*,*) 'D0mass_std: x11   ',x(3,1),xe(3,1)
c>        write(*,*) 'D0mass_std: x12   ',x(3,2),xe(3,2)
c>        write(*,*) 'D0mass_std: x11p  ',x(1,1),xe(1,1)
c>        write(*,*) 'D0mass_std: x12p  ',x(1,2),xe(1,2)
c>        write(*,*) 'D0mass_std: x11pp ',x(2,1),xe(2,1)
c>        write(*,*) 'D0mass_std: x12pp ',x(2,2),xe(2,2)
c>        write(*,*) 'D0mass_std: x21   ',x(6,1),xe(6,1)
c>        write(*,*) 'D0mass_std: x22   ',x(6,2),xe(6,2)
c>        write(*,*) 'D0mass_std: x21p  ',x(4,1),xe(4,1)
c>        write(*,*) 'D0mass_std: x22p  ',x(4,2),xe(4,2)
c>        write(*,*) 'D0mass_std: x21pp ',x(5,1),xe(5,1)
c>        write(*,*) 'D0mass_std: x22pp ',x(5,2),xe(5,2)
c>        write(*,*) 'D0mass_std: x31   ',x(9,1),xe(9,1)
c>        write(*,*) 'D0mass_std: x32   ',x(9,2),xe(9,2)
c>        write(*,*) 'D0mass_std: x31p  ',x(7,1),xe(7,1)
c>        write(*,*) 'D0mass_std: x32p  ',x(7,2),xe(7,2)
c>        write(*,*) 'D0mass_std: x31pp ',x(8,1),xe(8,1)
c>        write(*,*) 'D0mass_std: x32pp ',x(8,2),xe(8,2)


c inverse zeroes of arguments of logarithms
        S(1,1) = sqe_dd(M2(3),Y(1,3),M2(1))
        S(2,1) = sqe_dd(M2(4),Y(1,4),M2(1))
        S(3,1) = sqe_dd(M2(3),Y(3,4),M2(4))
        S(4,1) = sqe_dd(M2(2),Y(1,2),M2(1))
        S(5,1) = sqe_dd(M2(3),Y(1,3),M2(1))
        S(6,1) = sqe_dd(M2(2),Y(2,3),M2(3))
        S(7,1) = sqe_dd(M2(4),Y(1,4),M2(1))
        S(8,1) = sqe_dd(M2(2),Y(1,2),M2(1))
        S(9,1) = sqe_dd(M2(4),Y(2,4),M2(2))

        Se(1,1) = sqe_dd(M2e(3),Ye(1,3),M2e(1))
        Se(2,1) = sqe_dd(M2e(4),Ye(1,4),M2e(1))
        Se(3,1) = sqe_dd(M2e(3),Ye(3,4),M2e(4))
        Se(4,1) = sqe_dd(M2e(2),Ye(1,2),M2e(1))
        Se(5,1) = sqe_dd(M2e(3),Ye(1,3),M2e(1))
        Se(6,1) = sqe_dd(M2e(2),Ye(2,3),M2e(3))
        Se(7,1) = sqe_dd(M2e(4),Ye(1,4),M2e(1))
        Se(8,1) = sqe_dd(M2e(2),Ye(1,2),M2e(1))
        Se(9,1) = sqe_dd(M2e(4),Ye(2,4),M2e(2))

	S(1,2) = M2(1)/M2(3)/S(1,1)
        S(2,2) = M2(1)/M2(4)/S(2,1)
        S(3,2) = M2(4)/M2(3)/S(3,1)
        S(4,2) = M2(1)/M2(2)/S(4,1)
        S(5,2) = M2(1)/M2(3)/S(5,1)
        S(6,2) = M2(3)/M2(2)/S(6,1)
        S(7,2) = M2(1)/M2(4)/S(7,1)
        S(8,2) = M2(1)/M2(2)/S(8,1)
        S(9,2) = M2(2)/M2(4)/S(9,1)

	Se(1,2) = M2e(1)/M2e(3)/Se(1,1)
        Se(2,2) = M2e(1)/M2e(4)/Se(2,1)
        Se(3,2) = M2e(4)/M2e(3)/Se(3,1)
        Se(4,2) = M2e(1)/M2e(2)/Se(4,1)
        Se(5,2) = M2e(1)/M2e(3)/Se(5,1)
        Se(6,2) = M2e(3)/M2e(2)/Se(6,1)
        Se(7,2) = M2e(1)/M2e(4)/Se(7,1)
        Se(8,2) = M2e(1)/M2e(2)/Se(8,1)
        Se(9,2) = M2e(2)/M2e(4)/Se(9,1)

	do i=1,9
          if (abs(S(i,1)-Se(i,1)).gt.abs(S(i,1)-Se(i,2))) then
            caux    = Se(i,1)
            Se(i,1) = Se(i,2)
            Se(i,2) = caux
          endif
	enddo

	arg(0)  = Y(1,3)-Y(1,4)*(1d0+ALP)+Y(1,2)*ALP
	arge(0) = Ye(1,3)-Ye(1,4)*(1d0+ALP)+Ye(1,2)*ALP
	arg(1)  = 2D0*M2(3)-(1D0+ALP)*Y(3,4)+Y(2,3)*ALP
	arge(1) = 2D0*M2e(3)-(1D0+ALP)*Ye(3,4)+Ye(2,3)*ALP
	arg(2)  = -2D0*(1D0+ALP)*M2(4)+Y(3,4)+Y(2,4)*ALP
	arge(2) = -2D0*(1D0+ALP)*M2e(4)+Ye(3,4)+Ye(2,4)*ALP
	arg(4)  = Y(2,3)-(1D0+ALP)*Y(2,4)+2D0*M2(2)*ALP
	arge(4) = Ye(2,3)-(1D0+ALP)*Ye(2,4)+2D0*M2e(2)*ALP

#ifdef WRITESTD
        write(*,*) 'd0mass_std: n1234 ',arg(2),arg(0),arg(4),arg(1)
#endif

	m2abs = abs(m2(1))+abs(m2(2))+abs(m2(3))+abs(m2(4))
	if (abs(arg(0)/m2abs).lt.critcalc) arg(0) = 0d0
	if (abs(arg(1)/m2abs).lt.critcalc) arg(1) = 0d0
	if (abs(arg(2)/m2abs).lt.critcalc) arg(2) = 0d0
	if (abs(arg(4)/m2abs).lt.critcalc) arg(4) = 0d0

        arg(3)  = arg(1)
        arg(5)  = arg(1)
        arg(6)  = arg(4)
        arg(7)  = arg(2)
        arg(8)  = arg(4)
        arg(9)  = arg(2)
        arge(3) = arge(1)
        arge(5) = arge(1)
        arge(6) = arge(4)
        arge(7) = arge(2)
        arge(8) = arge(4)
        arge(9) = arge(2)

        D0mass_std = 0d0

	do 100 j=1,9
	
	  if (j.eq.1) then
	    if ((alp.eq.0d0).or.(beta(1).eq.0d0)) goto 100
            L(1) = -LOG(M2(3))
	    if (arg(j).ne.(0d0,0d0)) then
	      argl(1)  = arg(1)
	      argle(1) = arge(1)
              R(1)  = -arg(0)/arg(1)
              Re(1) = -arge(0)/arge(1)
	    else
	      argl(1)  = arg(0)
	      argle(1) = arge(0)
	    endif
	  elseif (j.eq.2) then
	    if ((alp.eq.0d0).or.(beta(1).eq.-1d0)) goto 100
            L(2) = -LOG(M2(4))
	    if (arg(j).ne.(0d0,0d0)) then
	      argl(2)  = arg(2)
	      argle(2) = arge(2)
              R(2)  = -arg(0)/arg(2)
              Re(2) = -arge(0)/arge(2)
	    else
	      argl(2)  = arg(0)
	      argle(2) = arge(0)
	    endif
	  elseif (j.eq.3) then
	    if (alp.eq.0d0) goto 100
            L(3) = -LOG(M2(3))
	    if (arg(j).ne.(0d0,0d0)) then
	      argl(3)  = arg(1)
	      argle(3) = arge(1)
              R(3)  = -arg(2)/arg(1)
              Re(3) = -arge(2)/arge(1)
	    else
	      argl(3)  = arg(2)
	      argle(3) = arge(2)
	    endif
	  elseif (j.eq.4) then
	    if ((alp.eq.-1d0).or.(beta(2).eq.0d0)) goto 100
            L(4) = -LOG(M2(2))
	    if (arg(j).ne.(0d0,0d0)) then
	      argl(4)  = arg(4)
	      argle(4) = arge(4)
              R(4)  = -arg(0)/arg(4)
              Re(4) = -arge(0)/arge(4)
	    else
	      argl(4)  = arg(0)
	      argle(4) = arge(0)
	    endif
	  elseif (j.eq.5) then
	    if ((alp.eq.-1d0).or.(beta(2).eq.-1d0)) goto 100
            L(5) = -LOG(M2(3))
	    if (arg(j).ne.(0d0,0d0)) then
	      argl(5)  = arg(1)
	      argle(5) = arge(1)
              R(5)  = -arg(0)/arg(1)
              Re(5) = -arge(0)/arge(1)
	    else
	      argl(5)  = arg(0)
	      argle(5) = arge(0)
	    endif
	  elseif (j.eq.6) then
	    if (alp.eq.-1d0) goto 100
            L(6) = -LOG(M2(2))
	    if (arg(j).ne.(0d0,0d0)) then
	      argl(6)  = arg(4)
	      argle(6) = arge(4)
              R(6)  = -arg(1)/arg(4)
              Re(6) = -arge(1)/arge(4)
	    else
	      argl(6)  = arg(1)
	      argle(6) = arge(1)
	    endif
	  elseif (j.eq.7) then
	    if (beta(3).eq.0d0) goto 100
            L(7) = -LOG(M2(4))
	    if (arg(j).ne.(0d0,0d0)) then
	      argl(7)  = arg(2)
	      argle(7) = arge(2)
              R(7)  = -arg(0)/arg(2)
              Re(7) = -arge(0)/arge(2)
	    else
	      argl(7)  = arg(0)
	      argle(7) = arge(0)
	    endif
	  elseif (j.eq.8) then
	    if (beta(3).eq.-1d0) goto 100
            L(8) = -LOG(M2(2))
	    if (arg(j).ne.(0d0,0d0)) then
	      argl(8)  = arg(4)
	      argle(8) = arge(4)
              R(8)  = -arg(0)/arg(4)
              Re(8) = -arge(0)/arge(4)
	    else
	      argl(8)  = arg(0)
	      argle(8) = arge(0)
	    endif
	  elseif (j.eq.9) then
            L(9) = -LOG(M2(4))
	    if (arg(j).ne.(0d0,0d0)) then
	      argl(9)  = arg(2)
	      argle(9) = arge(2)
              R(9)  = -arg(4)/arg(2)
              Re(9) = -arge(4)/arge(2)
	    else
	      argl(9)  = arg(4)
	      argle(9) = arge(4)
	    endif
	  endif

          if (abs(dimag(argl(j)))/abs(argl(j)).lt.crit) then
	    l(j) = l(j) + log(
     &         dcmplx( dreal(argl(j)),
     &	               (abs(dimag(argl(j)))+abs(argl(j))*eps2)
     &	               *sign(1d0,dimag(argle(j))) ))
	  else
	    l(j) = l(j) + log(argl(j))
	  endif

	do 200 i=1,2

#ifdef WRITESTD
          write(*,*) 
          write(*,*) 'D0mass_std, contr:',i,j
          write(*,*) 'D0mass_std,  x:   ',x(j,i)
          write(*,*) 'D0mass_std, -log(m):',l(j)-log(argl(j))
          write(*,*) 'D0mass_std,  log(M):',log(argl(j))
          write(*,*) 'D0mass_std,  Mj2/Mi2:',-r(j),-re(j)
          write(*,*) 'D0mass_std,  root1:',-s(j,1),-se(j,1)
          write(*,*) 'D0mass_std,  root2:',-s(j,2),-se(j,2)
#endif

	  sgn = (-1)**(i+1)
	  xx(j,i) = x(j,i)
          if (abs(dimag(xx(j,i)))/abs(xx(j,i)).lt.crit) xx(j,i) = 
     &       dcmplx( dreal(xx(j,i)),
     &	             (abs(dimag(xx(j,i)))+abs(xx(j,i))*eps2)
     &	             *sign(1d0,dimag(xe(j,i))) )

#ifdef WRITESTD
          write(*,*) 'D0mass_std, x:',xx(j,i)
#endif

          D0mass_std = D0mass_std + sgn*pre*cspen_dd(1d0+xx(j,i)) 

	  if (arg(j).ne.(0d0,0d0)) then
  	    rx(j,i) = 1d0-r(j)*x(j,i)
            if (abs(dimag(rx(j,i)))/abs(rx(j,i)).lt.crit) rx(j,i) = 
     &         dcmplx( dreal(rx(j,i)),
     &	               (abs(dimag(rx(j,i)))+abs(rx(j,i))*eps2)
     &	               *sign(1d0,dimag(-re(j)*xe(j,i))) )

#ifdef WRITESTD
            write(*,*) 'D0mass_std, mj2/mi2:',-r(j),-re(j)
#endif

	    etal = eta_dd(-xe(j,i),-re(j))

c	write(*,*) j,i,-xe(j,i),-re(j)

	    if (etal.ne.(0d0,0d0)) etal = etal*log(rx(j,i))
            D0mass_std = D0mass_std + sgn*pre*(
     &	      - l(j)*log(-xx(j,i)) + cspen_dd(rx(j,i)) + etal )

#ifdef WRITESTD
            write(*,*) 'D0mass_std, contra:',
     &          - sgn*l(j)*log(-xx(j,i)),
     &          sgn*cspen_dd(1d0+xx(j,i)), 
     &          sgn*(cspen_dd(rx(j,i)) + etal)
#endif

	  else
            D0mass_std = D0mass_std + sgn*pre*(
     &	      - l(j)*log(-xx(j,i)) - log(-xx(j,i))**2/2d0 )
	  endif

	do 200 k=1,2
	  sx(j,k,i) = 1d0-s(j,k)*x(j,i)
          if (abs(dimag(sx(j,k,i)))/abs(sx(j,k,i)).lt.crit) sx(j,k,i) = 
     &       dcmplx( dreal(sx(j,k,i)),
     &	             (abs(dimag(sx(j,k,i)))+abs(sx(j,k,i))*eps2)
     &	             *sign(1d0,dimag(-se(j,k)*xe(j,i))) )
	  etal = eta_dd(-xe(j,i),-se(j,k))
	  if (etal.ne.(0d0,0d0)) etal = etal*log(sx(j,k,i))
          D0mass_std = D0mass_std - sgn*pre*(
     &	    + cspen_dd(sx(j,k,i)) + etal )

#ifdef WRITESTD
            write(*,*) 'D0mass_std, contrb:',
     &      - sgn*(cspen_dd(sx(j,k,i)) + etal )
#endif

 200    continue

#ifdef WRITESTD
            write(*,*) 'D0mass_std sum:', D0mass_std/pre       
            write(*,*) 'D0mass_std=', D0mass_std       
            write(*,*)
#endif

 100  continue


      end
#endif
