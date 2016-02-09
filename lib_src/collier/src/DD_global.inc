c global settings           (set rmax1 >= rmax2, rmax3, ... )
        integer rmax1,rmax2,rmax3,rmax4,rmax5,rmax6,nmax,rmax
        parameter(rmax1=30,rmax2=15,rmax3=15,rmax4=12,rmax5=5,rmax6=6)
        parameter(nmax=7,rmax=50)

c LH parameters needed for cache
c       Ncoefmax1 = 2*(rmax1+1)                         ! not used
c       Ncoefmax2 = 2*Binomial[rmax2+2,2] + 13          !          13 auxiliary parameters
c       Ncoefmax3 = 2*Binomial[rmax3+3,3] + 3*rmax3 + 8 ! 3*rmax3 + 8 auxiliary parameters
c       Ncoefmax4 = 2*Binomial[rmax4+4,4] + 2*rmax4 + 7 ! 2*rmax4 + 7 auxiliary parameters
c       Ncoefmax5 = 2*Binomial[rmax5+5,5] +   rmax5 + 6 !   rmax5 + 6 auxiliary parameters
c       Ncoefmax6 = 2*Binomial[rmax6+6,6] +   rmax6 + 6 !   rmax6 + 6 auxiliary parameters
        integer Ncoefmax2,Ncoefmax3,Ncoefmax4,Ncoefmax5,Ncoefmax6
        parameter(Ncoefmax2=285,Ncoefmax3=1685,Ncoefmax4=3671,
     &            Ncoefmax5=515,Ncoefmax6=1860)

c common DD_mode
        integer mode34,mode5,mode6,outlevel,outchannel
        integer cout,coutmax
        logical cout_on
        common/DD_mode/cacc,dacc,mode34,mode5,mode6,
     &                 outlevel,outchannel,cout,coutmax,cout_on

c common DD_acc
        integer nmaster,r2master,accflag,errflag,stopflag
        common/DD_acc/aimacc(nmax),erracc(nmax),
     &     resaccabs(0:2**nmax,0:rmax),resaccrel(0:2**nmax,0:rmax),
     &     resaccabs2(0:2**nmax,0:rmax),resaccrel2(0:2**nmax,0:rmax),
     &     nmaster,r2master,accflag,errflag,stopflag

c common DD_dimreg
        common/DD_dimreg/deltauv,muv2,delta2ir,delta1ir,mir2,mx2(100)

c common DD_aux
        complex*16 scalint(0:2**nmax),scalintnew(0:2**nmax)
        complex*16 B_aux(0:2**nmax,0:2*rmax2,0:rmax2)
        complex*16 Buv_aux(0:2**nmax,0:2*rmax2,0:rmax2)
        complex*16 B0_aux(0:2**nmax,0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 Buv0_aux(0:2**nmax,0:2*rmax2,0:rmax2,0:rmax2)
        complex*16 C_aux(0:2**nmax,0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv_aux(0:2**nmax,0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C_new_aux(0:2**nmax,0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv_new_aux(0:2**nmax,0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 C0_aux(0:2**nmax,0:2*rmax3,0:rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv0_aux(0:2**nmax,0:2*rmax3,0:rmax3,0:rmax3,0:rmax3)
        complex*16 D_aux(0:2**nmax,0:2*rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv_aux(0:2**nmax,0:2*rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Sh3_aux(0:2**nmax,2,0:2*rmax3,0:rmax3,0:rmax3)
        complex*16 auxc(0:2**nmax,7)
        complex*16 x2_aux(0:2**nmax,0:2,0:2),tx2_aux(0:2**nmax,0:2,0:2)
        complex*16 ttx2_aux(0:2**nmax,0:2,0:2,0:2,0:2)
        real*8 auxr(0:2**nmax,2),z2_aux(0:2**nmax,2,2)
        real*8 tz2_aux(0:2**nmax,2,2),z2i_aux(0:2**nmax,2,2)
        real*8 ttz2_aux(0:2**nmax,2,2,2,2)
        real*8 acc_pave(0:2**nmax),acc_new(0:2**nmax)
        real*8 accr2_aux(0:2**nmax,0:2*rmax3)
        real*8 accr2_newprelim(0:2**nmax,0:2*rmax3)
        real*8 accr2_new_aux(0:2**nmax,0:2*rmax3)
        real*8 Cij_err(0:2**nmax,0:2*rmax3),C00_err(0:2**nmax,0:2*rmax3)
        real*8 Dij_err(0:2**nmax,0:2*rmax4),D00_err(0:2**nmax,0:2*rmax4)
        real*8 Cij_err_newprelim(0:2**nmax,0:2*rmax3)
        real*8 C00_err_newprelim(0:2**nmax,0:2*rmax3)
        real*8 Cij_err_new(0:2**nmax,0:2*rmax3)
        real*8 C00_err_new(0:2**nmax,0:2*rmax3)
        real*8 Dij_err_new(0:2**nmax,0:2*rmax4)
        real*8 D00_err_new(0:2**nmax,0:2*rmax4)
        real*8 Eij_err(0:2**nmax,0:2*rmax5),E00_err(0:2**nmax,0:2*rmax5)
        real*8 Fij_err(0:2**nmax,0:2*rmax6),F00_err(0:2**nmax,0:2*rmax6)
        real*8 Cij_err2(0:2**nmax,0:2*rmax3)
        real*8 Dij_err2(0:2**nmax,0:2*rmax4)
        real*8 Eij_err2(0:2**nmax,0:2*rmax5)
        real*8 Fij_err2(0:2**nmax,0:2*rmax6)
        real*8 maxz(0:2**nmax),maxzi(0:2**nmax),am02(0:2**nmax)
        real*8 maxzif(0:2**nmax),ziff(0:2**nmax)
        real*8 maxtxij(0:2**nmax)
        real*8 maxttx0klm(0:2**nmax),maxttx0ijm(0:2**nmax)
        real*8 maxtz_nj(0:2**nmax)
        real*8 maxttz_knlm(0:2**nmax),ttzff_kl(0:2**nmax)
        integer auxi(0:2**nmax,6),r2_aux(0:2**nmax),r20_aux(0:2**nmax)
        integer r2_new_aux(0:2**nmax),r2_newprelim(0:2**nmax)
        integer qmethod(0:2**nmax),qmethod_new(0:2**nmax)
        common/DD_aux/scalint,scalintnew,B_aux,Buv_aux,B0_aux,Buv0_aux,
     &         C_aux,Cuv_aux,C0_aux,Cuv0_aux,C_new_aux,Cuv_new_aux,
     &         D_aux,Duv_aux,
     &         Cij_err,C00_err,Dij_err,D00_err,
     &         Cij_err_newprelim,C00_err_newprelim,
     &         Cij_err_new,C00_err_new,Dij_err_new,D00_err_new,
     &         Eij_err,E00_err,Fij_err,F00_err,
     &         Cij_err2,Dij_err2,Eij_err2,Fij_err2,
     &         am02,maxz,maxzi,maxzif,ziff,
     &         maxtz_nj,maxttz_knlm,ttzff_kl,
     &         maxtxij,maxttx0ijm,maxttx0klm,
     &         Sh3_aux,auxc,x2_aux,tx2_aux,ttx2_aux,
     &         auxr,z2_aux,tz2_aux,z2i_aux,ttz2_aux,
     &         acc_pave,acc_new,accr2_aux,accr2_new_aux,accr2_newprelim,
     &         auxi,r2_aux,r20_aux,r2_new_aux,r2_newprelim,
     &         qmethod,qmethod_new

c common DD_debug
        integer nc_DDin,nr_DDin,ni_DDin
        complex*16 c_DDin(100)
        real*8     r_DDin(100)
        integer    i_DDin(100)
        character*20 s_DDin
        common/DD_debug/c_DDin,r_DDin,i_DDin,s_DDin,
     &                  nc_DDin,nr_DDin,ni_DDin


