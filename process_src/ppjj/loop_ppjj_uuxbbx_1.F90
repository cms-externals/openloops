
module ol_colourmatrix_ppjj_uuxbbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND
  implicit none
  logical, save           :: colmat_not_initialised = .true.
  complex(REALKIND), save :: K1(24,2), K2(2,2), KL(2,2)
  contains
  subroutine colourmatrix_init
    use ol_parameters_decl_/**/REALKIND, only: CI
    implicit none
    colmat_not_initialised = .false.
    ! colour matrix

  K1( 1,:) = [   9,   3]
  K1( 2,:) = [   3,   9]
  K1( 3,:) = [  12,   4]
  K1( 4,:) = [   4,  12]
  K1( 5,:) = [   0,  -4]
  K1( 6,:) = [  -4, -12]
  K1( 7,:) = [  12,   4]
  K1( 8,:) = [   4,  12]
  K1( 9,:) = [   0,   4]
  K1(10,:) = [   4,   0]
  K1(11,:) = [ -12,  -4]
  K1(12,:) = [  -4,   0]
  K1(13,:) = [  12,   4]
  K1(14,:) = [   4,  12]
  K1(15,:) = [ -12,  -4]
  K1(16,:) = [  -4,   0]
  K1(17,:) = [   0,   4]
  K1(18,:) = [   4,   0]
  K1(19,:) = [   0,  -4]
  K1(20,:) = [  -4, -12]
  K1(21,:) = [  12,   4]
  K1(22,:) = [   4,  12]
  K1(23,:) = [   9,   3]
  K1(24,:) = [   3,   9]

  K2(1,:) = [ 9, 3]
  K2(2,:) = [ 3, 9]

  KL(1,:) = [ 9, 3]
  KL(2,:) = [ 3, 9]

  end subroutine colourmatrix_init
end module ol_colourmatrix_ppjj_uuxbbx_1_/**/REALKIND



module ol_forced_parameters_ppjj_uuxbbx_1_/**/REALKIND
  implicit none
  contains
  subroutine check_forced_parameters
    use ol_parameters_decl_/**/REALKIND
    use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
    use ol_loop_parameters_decl_/**/DREALKIND, only: LeadingColour, nc, nf
#endif
    implicit none
    ! e.g.
    ! if (ME /= 0) write(*,101) 'ME = 0'
  if (nc /= 3) write(*,101) 'nc = 3'
  if (nf /= 6) write(*,101) 'nf = 6'
  if (ME /= 0) write(*,101) 'ME = 0'
  if (MM /= 0) write(*,101) 'MM = 0'
  if (ML /= 0) write(*,101) 'ML = 0'
  if (MU /= 0) write(*,101) 'MU = 0'
  if (MD /= 0) write(*,101) 'MD = 0'
  if (MS /= 0) write(*,101) 'MS = 0'
  if (MC /= 0) write(*,101) 'MC = 0'
  if (LeadingColour /= 0) write(*,101) 'LeadingColour = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'
  if (wMB /= 0) write(*,101) 'wMB = 0'

    101 format('========',/,'WARNING: code was generated with ',A,/,'========')
  end subroutine check_forced_parameters
end module ol_forced_parameters_ppjj_uuxbbx_1_/**/REALKIND

module ol_loop_ppjj_uuxbbx_1_/**/REALKIND
  use KIND_TYPES, only: REALKIND, intkind1
  implicit none
  ! diagram prefactors
  integer,           save :: fac_status_loop1 = -1, fac_status_loop2 = -1
  complex(REALKIND), save :: f(9), c(15)
  ! tree wavefunctions
  complex(REALKIND), save :: wf(4,-4+1:5)
  ! denominators
  complex(REALKIND), save :: den(3)
  ! Born colour vector for each helicity configuration
  complex(REALKIND), save :: M0(2,16)
  ! zero helicity identifier
  logical,           save :: zerohel(16) = .true., zerohel_ct(16) = .true.

  contains

! **********************************************************************
subroutine fac_init_loop()
! Writes diagram prefactors to 'f', rsp. 'c'
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND
  use ol_parameters_init_/**/REALKIND, only: parameters_init, loop_parameters_init
  use ol_loop_parameters_decl_/**/REALKIND
#ifndef PRECISION_dp
  use ol_loop_parameters_decl_/**/DREALKIND, only: SwF, SwB
#endif
  implicit none
  if (parameters_status == 0) call parameters_init()
  if (loop_parameters_status == 0) call loop_parameters_init()
  fac_status_loop1 = parameters_status
  fac_status_loop2 = loop_parameters_status
  ! factors of the diagrams
    f(1) = CI*gQCD**2
    f(2) = CI*countertermnorm*gQCD**4
    f(3) = CI*countertermnorm*ctGbb*gQCD**4
    f(4) = CI*countertermnorm*ctGqq*gQCD**4
    f(5) = CI*gQCD**4*integralnorm*SwB
    f(6) = (gQCD**4*integralnorm*SwB)/2._/**/REALKIND
    f(7) = gQCD**4*integralnorm*SwB
    f(8) = gQCD**4*integralnorm*SwF
    f(9) = 2*gQCD**4*integralnorm*SwF

  c = [ 9*CI*f(5), 27*CI*f(5), 18*f(6), 54*f(6), f(7), 3*f(7), 6*f(7), 10*f(7), 18*f(7), 21*f(7), 54*f(7), 3*f(8), 9*f(8), 3*f(9) &
    , 9*f(9) ]
  c = (1._/**/REALKIND / 36) * c
end subroutine fac_init_loop


! **********************************************************************
subroutine tree_wavefunctions(P, H, M1, M2)
! P(0:3,npart) = 2 -> n-2 external momenta (standard representation)
! H(npart)     = external-particle helicities
! Writes the tree wave functions to 'wf', denominators to 'den'.
! Returns the Born and counterterm colour vectors M1 and M2.
! **********************************************************************
  use ol_parameters_decl_/**/REALKIND ! masses
  use ol_loop_parameters_decl_/**/REALKIND ! counterterms
  use ol_momenta_decl_/**/REALKIND, only: Q
  use ol_wavefunctions_/**/REALKIND
  use ol_propagators_/**/REALKIND
  use ol_vertices_/**/REALKIND
  use ol_contractions_/**/REALKIND
  use ol_counterterms_/**/REALKIND
  implicit none

  real(REALKIND),    intent(in)  :: P(0:3,4)
  integer,           intent(in)  :: H(4)
  complex(REALKIND), intent(out) :: M1(2), M2(2)
  complex(REALKIND) :: A(4)

  ! external WFs

  call wf_Q(P(:,1), rZERO, H(1), wf(:,0))
  call wf_A(P(:,2), rZERO, H(2), wf(:,-1))
  call wf_Q(P(:,3), rMB, H(3), wf(:,-2))
  call wf_A(P(:,4), rMB, H(4), wf(:,-3))


  ! internal WFs

  call vert_QA_V(wf(:,0),wf(:,-1),wf(:,1))
  call vert_QA_V(wf(:,-2),wf(:,-3),wf(:,2))
  call counter_QA_V(wf(:,-2),wf(:,-3),wf(:,3))
  call counter_QA_V(wf(:,0),wf(:,-1),wf(:,4))
  call counter_V_V(ctGG,wf(:,1),Q(:,3),wf(:,5))


  ! propagators

    den(1) = 1 / (Q(5,3))
    den(2) = 1 / (Q(5,12))


  ! denominators

    den(3) = den(1)*den(2)


  ! colour stripped tree amplitudes

  A(1) = cont_VV(wf(:,1),wf(:,2)) * den(1)

  A(2) = cont_VV(wf(:,1),wf(:,3)) * den(1)
  A(3) = cont_VV(wf(:,2),wf(:,4)) * den(2)
  A(4) = cont_VV(wf(:,2),wf(:,5)) * den(3)


  ! colour vectors

  M1(1) = (A(1)*f(1))/2._/**/REALKIND
  M1(2) = -(A(1)*f(1))/6._/**/REALKIND

  M2(1) = -(A(4)*f(2))/2._/**/REALKIND+(A(2)*f(3))/2._/**/REALKIND+(A(3)*f(4))/2._/**/REALKIND
  M2(2) = (A(4)*f(2))/6._/**/REALKIND-(A(2)*f(3))/6._/**/REALKIND-(A(3)*f(4))/6._/**/REALKIND


end subroutine tree_wavefunctions

end module ol_loop_ppjj_uuxbbx_1_/**/REALKIND
