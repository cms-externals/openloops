c global settings
        integer rmax1,rmax2,rmax3,rmax4,rmax5,rmax6,nmax
c       parameter(rmax1=25,rmax2=25,rmax3=25,rmax4=25,rmax5=5,rmax6=5)
c       parameter(rmax1=20,rmax2=20,rmax3=20,rmax4=20,rmax5=5,rmax6=5)
        parameter(rmax1=16,rmax2=16,rmax3=16,rmax4=16,rmax5=5,rmax6=6)
c       parameter(rmax1=12,rmax2=12,rmax3=12,rmax4=12,rmax5=5,rmax6=6)
	parameter(nmax=6)

c common DD_mode
        integer mode34,mode5,mode6,outlevel
        common/DD_mode/resacc(0:2**nmax),cacc,dacc,
     &		mode34,mode5,mode6,outlevel

c common DD_dimreg
        common/DD_dimreg/deltauv,muv2,delta2ir,delta1ir,mir2,mx2(10)

c common DD_aux
        complex*16 scalint(0:2**nmax),scalintnew(0:2**nmax)
        complex*16 B_aux(0:2**nmax,0:rmax2,0:rmax2)
        complex*16 Buv_aux(0:2**nmax,0:rmax2,0:rmax2)
        complex*16 B0_aux(0:2**nmax,0:rmax2,0:rmax2,0:rmax2)
        complex*16 Buv0_aux(0:2**nmax,0:rmax2,0:rmax2,0:rmax2)
        complex*16 C_aux(0:2**nmax,0:rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv_aux(0:2**nmax,0:rmax3,0:rmax3,0:rmax3)
        complex*16 C_new_aux(0:2**nmax,0:rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv_new_aux(0:2**nmax,0:rmax3,0:rmax3,0:rmax3)
        complex*16 C0_aux(0:2**nmax,0:rmax3,0:rmax3,0:rmax3,0:rmax3)
        complex*16 Cuv0_aux(0:2**nmax,0:rmax3,0:rmax3,0:rmax3,0:rmax3)
        complex*16 Sh3_aux(0:2**nmax,2,0:rmax3,0:rmax3,0:rmax3)
        complex*16 auxc(0:2**nmax,7)
        complex*16 x2_aux(0:2**nmax,0:2,0:2),tx2_aux(0:2**nmax,0:2,0:2)
        complex*16 ttx2_aux(0:2**nmax,0:2,0:2,0:2,0:2)
        real*8 auxr(0:2**nmax,2),z2_aux(0:2**nmax,2,2)
        real*8 tz2_aux(0:2**nmax,2,2),z2i_aux(0:2**nmax,2,2)
        real*8 ttz2_aux(0:2**nmax,2,2,2,2)
	real*8 acc_pave(0:2**nmax),acc_new(0:2**nmax)
	real*8 accr2_aux(0:2**nmax,0:rmax3)
	real*8 accr2_new_aux(0:2**nmax,0:rmax3)
        real*8 Cij_err(0:2**nmax,0:rmax3),C00_err(0:2**nmax,0:rmax3)
        real*8 Dij_err(0:2**nmax,0:rmax4),D00_err(0:2**nmax,0:rmax4)
        real*8 Cij_err_new(0:2**nmax,0:rmax3)
	real*8 C00_err_new(0:2**nmax,0:rmax3)
        real*8 Dij_err_new(0:2**nmax,0:rmax4)
	real*8 D00_err_new(0:2**nmax,0:rmax4)
        real*8 maxz(0:2**nmax),maxzi(0:2**nmax),am02(0:2**nmax)
        real*8 maxzif(0:2**nmax),ziff(0:2**nmax)
	real*8 maxttx0klm(0:2**nmax)
	real*8 maxttx0ijm(0:2**nmax)
        real*8 maxtz_nj(0:2**nmax)
	real*8 maxttz_knlm(0:2**nmax),ttzff_kl(0:2**nmax)
        integer auxi(0:2**nmax,6),r2_aux(0:2**nmax),r20_aux(0:2**nmax)
        integer r2_new_aux(0:2**nmax),dr2exp_aux(0:2**nmax)
        integer qmethod(0:2**nmax),qmethod_new(0:2**nmax)
        common/DD_aux/scalint,scalintnew,B_aux,Buv_aux,B0_aux,Buv0_aux,
     &         C_aux,Cuv_aux,C0_aux,Cuv0_aux,C_new_aux,Cuv_new_aux,
     &	       Cij_err,C00_err,Dij_err,D00_err,
     &	       Cij_err_new,C00_err_new,Dij_err_new,D00_err_new,
     &	       am02,maxz,maxzi,maxzif,ziff,
     &	       maxtz_nj,maxttz_knlm,ttzff_kl,
     &	       maxttx0ijm,maxttx0klm,
     &	       Sh3_aux,auxc,x2_aux,tx2_aux,ttx2_aux,
     &         auxr,z2_aux,tz2_aux,z2i_aux,ttz2_aux,
     &	       acc_pave,acc_new,accr2_aux,accr2_new_aux,
     &         auxi,r2_aux,r20_aux,r2_new_aux,dr2exp_aux,
     &	       qmethod,qmethod_new

c common DD_debug
	integer nc_DDin,nr_DDin,ni_DDin
	complex*16 c_DDin(100)
	real*8     r_DDin(100)
	integer    i_DDin(100)
	character*20 s_DDin
	common/DD_debug/c_DDin,r_DDin,i_DDin,s_DDin,
     &	                nc_DDin,nr_DDin,ni_DDin


