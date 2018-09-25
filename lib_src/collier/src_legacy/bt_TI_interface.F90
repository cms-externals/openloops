
module TI_lib_interface
  implicit none
  integer, save :: TI_library = 1 ! 1 -> COLI   2 -> DD
end module TI_lib_interface


! ******************************
function olA0(m2)
! ************************************************************************
! *	scalar 1-point function with complex mass
! ************************************************************************
  use TI_lib_interface
  implicit none
  complex*16 m2,olA0,A0_coli,xA0_

  if (TI_library == 1) then

    olA0 = A0_coli(m2)

  else if (TI_library == 2) then

    olA0 = xA0_(m2)

  end if
end function olA0


! ***********************************************************************
subroutine olBp12345(p10,m02,m12,B0,B1,B2,B3,B4,B5,B6,rankin,switchin)
! ***********************************************************************
! *     2-point tensor coefficient functions B0,B1,B2,B3,B4,B5          *
  use TI_lib_interface
  implicit   none
  complex*16 p10
  complex*16 m02,m12
  complex*16 B0,B1,B2(0:1,0:1),B3(0:1,0:1,0:1)
  complex*16 B4(0:1,0:1,0:1,0:1),B5(0:1,0:1,0:1,0:1,0:1)
  complex*16 B6(0:1,0:1,0:1,0:1,0:1,0:1)
  integer    rankin,switchin

  if (TI_library == 1) then

    call cBp12345(p10,m02,m12,B0,B1,B2,B3,B4,B5,B6,rankin,switchin)

  else if (TI_library == 2) then

    call xB012345_(p10,m02,m12,B0,B1,B2,B3,B4,B5,B6,rankin,switchin)

  end if
end subroutine olBp12345


! ***********************************************************************
subroutine olCp12345(p10,p21,p20,m02,m12,m22,C0,C1,C2,C3,C4,C5,C6,rankin)
! ***********************************************************************
! *     3-point tensor coefficient functions C0,C1,C2,C3,C4,C5          *
! ***********************************************************************
  use TI_lib_interface
  implicit   none
  complex*16 p10,p21,p20
  complex*16 q10,q21,q20
  complex*16 m02,m12,m22
  complex*16 mm02,mm12,mm22
  complex*16 C0,C1(2),C2(0:2,0:2),C3(0:2,0:2,0:2)
  complex*16 C4(0:2,0:2,0:2,0:2),C5(0:2,0:2,0:2,0:2,0:2)
  complex*16 C6(0:2,0:2,0:2,0:2,0:2,0:2)
  integer    rankin

  if (TI_library == 1) then

    call cCp12345(p10,p21,p20,m02,m12,m22,C0,C1,C2,C3,C4,C5,C6,rankin)

  else if (TI_library == 2) then

    call xC01234_(dreal(p10),dreal(p21),dreal(p20),m02,m12,m22,C0,C1,C2,C3,C4,C5,C6,rankin)

  end if
end subroutine olCp12345

! ***********************************************************************
subroutine olDp12345(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32, &
                     D0,D1,D2,D3,D4,D5,rankin,switchin)
! ***********************************************************************
! *     4-point tensor coefficient functions D0,D1,D2,D3,D4,D5          *
! ***********************************************************************
  use TI_lib_interface
  implicit   none
  complex*16 p10,p21,p32,p30,p20,p31
  complex*16 m02,m12,m22,m32
  complex*16 D0,D1(3),D2(0:3,0:3),D3(0:3,0:3,0:3)
  complex*16 D4(0:3,0:3,0:3,0:3),D5(0:3,0:3,0:3,0:3,0:3)
  integer    rankin,switchin

  if (TI_library == 1) then

    call cDp12345(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32, &
                  D0,D1,D2,D3,D4,D5,rankin,switchin)

  else if (TI_library == 2) then

    call xD012345_(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32, &
                   D0,D1,D2,D3,D4,D5,rankin,switchin)

  end if
end subroutine olDp12345


! ***********************************************************************
subroutine olEp12345(p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42, &
                    En0,En1,En2,En3,En4,En5,rankin)
! ***********************************************************************
! *     5-point tensor coefficient functions  E0,E1,E2,E3,E4            *
! ***********************************************************************
  use TI_lib_interface
  implicit   none
  complex*16 p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
  complex*16 m02,m12,m22,m32,m42
  complex*16 En0,En1(4),En2(0:4,0:4),En3(0:4,0:4,0:4)
  complex*16 En4(0:4,0:4,0:4,0:4),En5(0:4,0:4,0:4,0:4,0:4)
  integer    rankin

  if (TI_library == 1) then

    call cEp12345(p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42, &
                  En0,En1,En2,En3,En4,En5,rankin)

  else if (TI_library == 2) then

    call xE012345_new(p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42, &
                  En0,En1,En2,En3,En4,En5,rankin)

  end if
end subroutine olEp12345


! ***********************************************************************
subroutine olFp12345(p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,p51,p30,p41,p52, &
                    m02,m12,m22,m32,m42,m52,Fn0,Fn1,Fn2,Fn3,Fn4,Fn5,rank)
! ***********************************************************************
! *     6-point tensor coefficient functions F0,F1,F2,F3,F4             *
! ***********************************************************************
  use TI_lib_interface
  implicit   none
  complex*16 p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,p51,p30,p41,p52
  complex*16 m02,m12,m22,m32,m42,m52
  complex*16 Fn0,Fn1(5),Fn2(0:5,0:5),Fn3(0:5,0:5,0:5)
  complex*16 Fn4(0:5,0:5,0:5,0:5),Fn5(0:5,0:5,0:5,0:5,0:5)
  integer    rank

  if (TI_library == 1) then

    call cFp12345(p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,p51,p30,p41,p52, &
                  m02,m12,m22,m32,m42,m52,Fn0,Fn1,Fn2,Fn3,Fn4,Fn5,rank)

  else if (TI_library == 2) then

    call xF012345_new(p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,p51,p30,p41,p52, &
                      m02,m12,m22,m32,m42,m52,Fn0,Fn1,Fn2,Fn3,Fn4,Fn5,rank)

  end if
end subroutine olFp12345



