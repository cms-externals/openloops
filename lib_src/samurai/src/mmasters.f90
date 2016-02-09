module mmasters
!AC!	use mmisqcdloop
    	use mmisavholo
!AC!	use mmisgolem
!AC!	use mmislooptools
	use precision
        use options
        implicit none
	public :: initmasters, exitmasters
        public :: getMI4, getMI3, getMI2, getMI2hr, getMI1

contains

subroutine initmasters()
          use avh_olo, only: olo_onshell
	implicit none

        select case(isca)
!AC!		case(1) 
!AC!			call qlinit
    		case(2) 
    			call olo_onshell(1.e-8_ki)
!AC!	case(3) 
!AC!		continue
!AC!		case(4) 
!AC!			call ltini
		case default 
			print *,'error in initmasters: isca value not allowed' 
        end select
end subroutine initmasters

subroutine exitmasters()
	implicit none
	
	select case(isca)
!AC!		case(1) 
!AC!			continue
    		case(2) 
    			continue
!AC!	case(3) 
!AC!		continue
!AC!		case(4) 
!AC!			call ltexi
		case default
			print *,'error in exitmasters: isca value not allowed'
	end select
end subroutine exitmasters

subroutine getMI4(V,m,scale2,MI4,cache_flag, cache_offset, scalar_cache)
        implicit none
        real(ki),    dimension(1:6),  intent(in ) :: V
        complex(ki), dimension(0:3),  intent(in ) :: m
        real(ki),                     intent(in ) :: scale2
        complex(ki), dimension(-2:0), intent(out) :: MI4
        
        logical,     intent(in   ), optional                    :: cache_flag
        integer,     intent(inout), optional                    :: cache_offset
        complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache

        select case(isca)
!AC!             case(1)
!AC!                     call qcdloop4  (V,m,scale2,MI4,cache_flag, cache_offset, scalar_cache)
                case(2) 
                        call avholo4   (V,m,scale2,MI4,cache_flag, cache_offset, scalar_cache)
!AC!          case(3) 
!AC!                  call golemMI4  (V,m,scale2,MI4,cache_flag, cache_offset, scalar_cache)
!AC!             case(4) 
!AC!                     call looptools4(V,m,scale2,MI4,cache_flag, cache_offset, scalar_cache)
                case default
			print *, 'error in getMI4, isca value not allowed'
        end select

end subroutine

subroutine getMI3(V,m,scale2,MI3,cache_flag, cache_offset, scalar_cache)
        implicit none
        
        real(ki),    dimension(1:3),  intent(in ) :: V
        complex(ki), dimension(0:2),  intent(in ) :: m
        real(ki),                     intent(in ) :: scale2
        complex(ki), dimension(-2:0), intent(out) :: MI3
        
        logical,     intent(in   ), optional                    :: cache_flag
        integer,     intent(inout), optional                    :: cache_offset
        complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache

        select case(isca)
!AC!             case(1)
!AC!                     call qcdloop3  (V,m,scale2,MI3,cache_flag, cache_offset, scalar_cache)
                case(2) 
                        call avholo3   (V,m,scale2,MI3,cache_flag, cache_offset, scalar_cache)
!AC!          case(3) 
!AC!                  call golemMI3  (V,m,scale2,MI3,cache_flag, cache_offset, scalar_cache)
!AC!             case(4) 
!AC!                     call looptools3(V,m,scale2,MI3,cache_flag, cache_offset, scalar_cache)
                case default
			print *, 'error in getMI3, isca value not allowed'
        end select

end subroutine

subroutine getMI2(K11,m,scale2,MI2a,MI2b,MI2c,MI2d,MI2e,cache_flag, cache_offset, scalar_cache)
        implicit none
        
        real(ki),                       intent(in   )           :: K11
        complex(ki), dimension(0:1),    intent(in   )           :: m
        real(ki),                       intent(in   )           :: scale2
        complex(ki), dimension(-2:0),   intent(out  )           :: MI2a, MI2b, MI2c, MI2d, MI2e
        
        logical,                        intent(in   ), optional :: cache_flag
        integer,                        intent(inout), optional :: cache_offset
        complex(ki), dimension(-2:0,*), intent(inout), optional :: scalar_cache

        select case(isca)
!AC!             case(1) 
!AC!                     call qcdloop2  (K11,m,scale2,MI2a,MI2b,MI2c,MI2d,MI2e,cache_flag, cache_offset, scalar_cache)
                case(2) 
                        call avholo2   (K11,m,scale2,MI2a,MI2b,MI2c,MI2d,MI2e,cache_flag, cache_offset, scalar_cache)
!AC!          case(3) 
!AC!                  call golemMI2  (K11,m,scale2,MI2a,MI2b,MI2c,MI2d,MI2e,cache_flag, cache_offset, scalar_cache)
!AC!             case(4) 
!AC!                     call looptools2(K11,m,scale2,MI2a,MI2b,MI2c,MI2d,MI2e,cache_flag, cache_offset, scalar_cache)
                case default
			print *, 'error in getMI2, isca value not allowed'
        end select

end subroutine

subroutine getMI2hr(K11,m,scale2,MI2J111)!, cache_flag, cache_offset, scalar_cache)
        implicit none
        real(ki),    intent(in) :: K11
        complex(ki), dimension(0:1), intent(in) :: m
        real(ki),    intent(in) :: scale2
        complex(ki), dimension(-2:0), intent(out) :: MI2J111 

!        logical,     intent(in   ), optional                    :: cache_flag
!        integer,     intent(inout), optional                    :: cache_offset
!        complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache

        select case(isca)
!AC!             case(1) 
!AC!                     call qcdloop2hr  (K11,m,scale2,MI2J111)!,cache_flag, cache_offset, scalar_cache)
                case(2) 
                        call avholo2hr   (K11,m,scale2,MI2J111)!,cache_flag, cache_offset, scalar_cache)
!AC!          case(3) 
!AC!                  call golemMI2hr1 (K11,m,scale2,MI2J111)!,cache_flag, cache_offset, scalar_cache)
!AC!             case(4) 
!AC!                     call looptools2hr(K11,m,scale2,MI2J111)!,cache_flag, cache_offset, scalar_cache)
                case default
			print *, 'error in getMI2hr, isca value not allowed'
        end select

end subroutine

subroutine getMI1(m0,scale2,MI1, cache_flag, cache_offset, scalar_cache)
        implicit none
        complex(ki),                      intent(in ) :: m0
        complex(ki), dimension(-2:0),     intent(out) :: MI1
        real(ki),                         intent(in ) :: scale2
        
        logical,     intent(in   ), optional                    :: cache_flag
        integer,     intent(inout), optional                    :: cache_offset
        complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache


select case(isca)
!AC!           case(1)
!AC!                   call qcdloop1  (m0,scale2,MI1,cache_flag, cache_offset, scalar_cache)
              case(2) 
                      call avholo1   (m0,scale2,MI1,cache_flag, cache_offset, scalar_cache)
!AC!        case(3) 
!AC!                call golemMI1  (m0,scale2,MI1,cache_flag, cache_offset, scalar_cache)
!AC!           case(4) 
!AC!                   call looptools1(m0,scale2,MI1,cache_flag, cache_offset, scalar_cache)
        case default
			print *, 'error in getMI1, isca value not allowed'
end select

end subroutine

end module
