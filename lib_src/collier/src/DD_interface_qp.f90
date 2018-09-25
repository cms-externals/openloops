

















module MODULE_DD_INIT
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Initialization of global DD parameters
  !--------------------------------------------------------------
  ! 18.2.2016 Stefan Dittmaier
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none
  contains

  subroutine Init_DD_global(nmax_in,ritmax_in)
    use dd_global_qp
    use dd_2pt_qp
    use dd_3pt_qp
    use dd_4pt_qp
    use dd_5pt_qp
    use dd_6pt_qp

    integer, intent(in) :: nmax_in,ritmax_in
    integer             :: id,n,k

    ritmax = ritmax_in

  ! dimensions for array declarations
    rmax6  = 6
    rmax5  = 5
    rmax4  = ritmax+3
    rmax3  = rmax4+2
    rmax2  = rmax4+4
    rmax   = 2*rmax2

  ! maximal rank of N-point tensors
  !  -> dimensions for arrays in internal cache
    r2max6  = rmax6
    r2max5  = r2max6-1
  !  r2max4  = r2max5-1
  !changed SD 28.6.17
    r2max4  = rmax4
    r2max3  = rmax3
    r2max2  = rmax2

  ! store some  binomial coefficients
    if (allocated(BinC)) then
      deallocate(BinC)
    endif
    allocate(BinC(0:r2max3,0:r2max3))
    do n=0,r2max3
      do k=0,r2max3
        BinC(n,k) = Binomial_DD(n,k)
      enddo
    enddo

    Ncoefmax4_int = Binomial_DD(r2max4+4,4)
    Ncoefmax3_int = Binomial_DD(r2max3+3,3)

    Ncoefmax2 = 2*Binomial_DD(r2max2+2,2) + 2*r2max2 + 13
    Ncoefmax3 = 2*Binomial_DD(r2max3+3,3) + 8*r2max3 + 11
    Ncoefmax4 = 2*Binomial_DD(r2max4+4,4) + 7*r2max4 + 10    ! conservative upper limit
    Ncoefmax5 = 2*Binomial_DD(r2max5+5,5) + 6*r2max5 + 9
    Ncoefmax6 = 2*Binomial_DD(r2max6+6,6) + 6*r2max6 + 9

    nmax = nmax_in

  ! identifiers for N-point functions
    if (allocated(tid)) then
      deallocate(tid,ntid)
    end if
    allocate(tid(0:2**nmax_in-1),ntid(0:nmax_in))
    ntid(0:nmax) = 0
    do id=0,2**nmax-1
      n=0
      do k=0,nmax-1
        if (mod(id,2**(k+1))/2**k.eq.0) n=n+1
      enddo
      ntid(n) = ntid(n)+1
      tid(id) = ntid(n)
    enddo

    if (allocated(aimacc)) then
      deallocate(aimacc,erracc)
    end if
    allocate(aimacc(nmax),erracc(nmax))

    if (allocated(resaccabs)) then
      deallocate(resaccabs,resaccrel,resaccabs2,resaccrel2)
    end if
    allocate(resaccabs(0:2**nmax,0:rmax),resaccrel(0:2**nmax,0:rmax))
    allocate(resaccabs2(0:2**nmax,0:rmax),resaccrel2(0:2**nmax,0:rmax))

  ! arrays depending on nmax and any r2max#

    if (allocated(B_cache)) then
      deallocate(B_cache,Buv_cache)
    end if
    if (nmax.ge.2) then
      allocate(B_cache(ntid(2),0:r2max2,0:r2max2))
      allocate(Buv_cache(ntid(2),0:r2max2,0:r2max2))
    endif

    if (allocated(C_cache)) then
      deallocate(C_cache,Cuv_cache,C_new_cache,Cuv_new_cache)
      deallocate(Cij_err,C00_err,Cij_err2)
      deallocate(Cij_err_newprelim,C00_err_newprelim,Cij_err_new,C00_err_new)
      deallocate(accr2_aux,accr2_newprelim,accr2_new_aux)
    endif
    if (nmax.ge.3) then
      allocate(C_cache(ntid(3),Ncoefmax3_int))
      allocate(Cuv_cache(ntid(3),Ncoefmax3_int))
      allocate(C_new_cache(ntid(3),Ncoefmax3_int))
      allocate(Cuv_new_cache(ntid(3),Ncoefmax3_int))
      allocate(Cij_err(ntid(3),0:2*r2max3))
      allocate(C00_err(ntid(3),0:2*r2max3))
      allocate(Cij_err2(ntid(3),0:2*r2max3))
      allocate(Cij_err_newprelim(ntid(3),0:2*r2max3))
      allocate(C00_err_newprelim(ntid(3),0:2*r2max3))
      allocate(Cij_err_new(ntid(3),0:2*r2max3))
      allocate(C00_err_new(ntid(3),0:2*r2max3))
      allocate(accr2_aux(ntid(3),0:2*r2max3))
      allocate(accr2_newprelim(ntid(3),0:2*r2max3))
      allocate(accr2_new_aux(ntid(3),0:2*r2max3))
    endif

    if (allocated(D_cache)) then
      deallocate(D_cache,Duv_cache)
      deallocate(Dij_err,D00_err,Dij_err_new,D00_err_new,Dij_err2)
    endif
    if (nmax.ge.4) then
      allocate(D_cache(ntid(4),Ncoefmax4_int))
      allocate(Duv_cache(ntid(4),Ncoefmax4_int))
      allocate(Dij_err(ntid(4),0:2*r2max4))
      allocate(D00_err(ntid(4),0:2*r2max4))
      allocate(Dij_err_new(ntid(4),0:2*r2max4))
      allocate(D00_err_new(ntid(4),0:2*r2max4))
      allocate(Dij_err2(ntid(4),0:2*r2max4))
    endif

    if (allocated(Eij_err)) then
      deallocate(Eij_err,Eij_err2)
    endif
    if (nmax.ge.5) then
      allocate(Eij_err(ntid(5),0:2*r2max5))
      allocate(Eij_err2(ntid(5),0:2*r2max5))
    endif

    if (allocated(Fij_err)) then
      deallocate(Fij_err,Fij_err2)
    endif
    if (nmax.ge.6) then
      allocate(Fij_err(ntid(6),0:2*r2max6))
      allocate(Fij_err2(ntid(6),0:2*r2max6))
    endif

  ! arrays depending only on nmax

    if (allocated(z2i_aux)) then
      deallocate(z2_aux,tz2_aux,z2i_aux,ttz2_aux)
      deallocate(x2_aux,tx2_aux,ttx2_aux)
    endif
    if (nmax.ge.3) then
      allocate(z2i_aux(ntid(3),2,2),ttz2_aux(ntid(3),2,2,2,2))
      allocate(z2_aux(ntid(3),2,2),tz2_aux(ntid(3),2,2))
      allocate(x2_aux(ntid(3),0:2,0:2),tx2_aux(ntid(3),0:2,0:2))
      allocate(ttx2_aux(ntid(3),0:2,0:2,0:2,0:2))
    endif

    if (allocated(scalint)) then
      deallocate(scalint,scalint_err,scalintnew)
      deallocate(auxc)
      deallocate(auxr)
      deallocate(acc_pave,acc_new)
      deallocate(maxtxij,maxttx0klm,maxttx0ijm)
      deallocate(maxtz_nj,maxttz_knlm,ttzff_kl)
      deallocate(auxi,r2_aux,r2_new_aux,r2_newprelim,qmethod,qmethod_new)
    end if

    allocate(scalint(0:2**nmax),scalint_err(0:2**nmax),scalintnew(0:2**nmax))
    allocate(maxtxij(0:2**nmax),maxttx0klm(0:2**nmax),maxttx0ijm(0:2**nmax))
    allocate(maxtz_nj(0:2**nmax),maxttz_knlm(0:2**nmax),ttzff_kl(0:2**nmax))
    allocate(acc_pave(0:2**nmax),acc_new(0:2**nmax))
    allocate(auxr(0:2**nmax,2))
    allocate(auxc(0:2**nmax,7))
    allocate(auxi(0:2**nmax,6))
    allocate(r2_aux(0:2**nmax),r2_new_aux(0:2**nmax),r2_newprelim(0:2**nmax))
    allocate(qmethod(0:2**nmax),qmethod_new(0:2**nmax))
  end subroutine Init_DD_global

  integer (kind=8) function Binomial_DD(n,m)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Binomial coefficient
    !--------------------------------------------------------------
    ! 18.2.2016 Stefan Dittmaier
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer :: i,n,m
    Binomial_DD = 1
    if ((m.ge.0).and.(m.le.n)) then
      do i=1,m
        Binomial_DD = Binomial_DD*(n+1-i)
        Binomial_DD = Binomial_DD/i
      enddo
    else
      Binomial_DD = 0
    endif
  end function Binomial_DD

end module MODULE_DD_INIT


module dd_interface
  use dd_aux_qp, only: DDsetmode_qp => DDsetmode, &
                     & DDsetparam_qp => DDsetparam, &
                     & DDsetaccthr_qp => DDsetaccthr, &
                     & DDseterrthr_qp => DDseterrthr
  use MODULE_DD_INIT, only: Init_DD_global_qp => Init_DD_global
  use dd_2pt_qp, only: A_dd_qp => A_dd, B_dd_qp => B_dd
  use dd_3pt_qp, only: C_dd_qp => C_dd
  use dd_4pt_qp, only: D_dd_qp => D_dd
  use dd_5pt_qp, only: E_dd_qp => E_dd
  use dd_6pt_qp, only: F_dd_qp => F_dd
  implicit none
  interface A_dd_mp
    subroutine A_dd(A, Auv, xm02, r, id)
      use dd_global_qp, only: dp
      integer,     intent(in)  :: r, id
      complex(dp), intent(out) :: A(0:r/2), Auv(0:r/2)
      complex(dp), intent(in)  :: xm02
    end subroutine A_dd
    module procedure :: A_dd_qp
  end interface A_dd_mp
  interface B_dd_mp
    subroutine B_dd(B, Buv, xp2, xm02, xm12, r2, id)
      use dd_global_qp, only: dp
      integer,     intent(in)  :: r2, id
      complex(dp), intent(out) :: B(0:r2,0:r2), Buv(0:r2,0:r2)
      real(dp),    intent(in)  :: xp2
      complex(dp), intent(in)  :: xm02, xm12
    end subroutine B_dd
    module procedure :: B_dd_qp
  end interface B_dd_mp
  interface C_dd_mp
    subroutine C_dd(C, Cuv, p01, p12, p20, m02, m12, m22, r2, id)
      use dd_global_qp, only: dp
      integer,     intent(in)  :: r2, id
      complex(dp), intent(out) :: C(0:r2,0:r2,0:r2), Cuv(0:r2,0:r2,0:r2)
      real(dp),    intent(in)  :: p01, p12, p20
      complex(dp), intent(in)  :: m02, m12, m22
    end subroutine C_dd
    module procedure :: C_dd_qp
  end interface C_dd_mp
  interface D_dd_mp
    subroutine D_dd(D, Duv, p1, p2, p3, p4, p12, p23, m02, m12, m22, m32, r2, id)
      use dd_global_qp, only: dp
      integer,     intent(in)  :: r2, id
      complex(dp), intent(out) :: D(0:r2,0:r2,0:r2,0:r2), Duv(0:r2,0:r2,0:r2,0:r2)
      real(dp),    intent(in)  :: p1, p2, p3, p4, p12, p23
      complex(dp), intent(in)  :: m02, m12, m22, m32
    end subroutine D_dd
    module procedure :: D_dd_qp
  end interface D_dd_mp
  interface E_dd_mp
    subroutine E_dd(E, p1, p2, p3, p4, p5, p12, p23, p34, p45, p15, m02, m12, m22, m32, m42, r2, id)
      use dd_global_qp, only: dp
      integer,     intent(in)  :: r2, id
      complex(dp), intent(out) :: E(0:r2/2,0:r2,0:r2,0:r2,0:r2)
      real(dp),    intent(in)  :: p1, p2, p3, p4, p5, p12, p23, p34, p45, p15
      complex(dp), intent(in)  :: m02, m12, m22, m32, m42
    end subroutine E_dd
    module procedure :: E_dd_qp
  end interface E_dd_mp
  interface F_dd_mp
    subroutine F_dd(F, p1, p2, p3, p4, p5, p6, p12, p23, p34, p45, p56, p16, &
                  & p123, p234, p345, m02, m12, m22, m32, m42, m52, r2, id)
      use dd_global_qp, only: dp
      integer,     intent(in)  :: r2, id
      complex(dp), intent(out) :: F(0:r2/2,0:r2,0:r2,0:r2,0:r2,0:r2)
      real(dp),    intent(in)  :: p1, p2, p3, p4, p5, p6, p12, p23, p34, p45, p56, p16, p123, p234, p345
      complex(dp), intent(in)  :: m02, m12, m22, m32, m42, m52
    end subroutine F_dd
    module procedure :: F_dd_qp
  end interface F_dd_mp
end module dd_interface
