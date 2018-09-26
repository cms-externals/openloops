
! This template is filled by the code generator SCons script

module ol_versiondata_eevjj
  implicit none
  character(4) :: generator_revision = "1990"
  interface
    subroutine version_eevjj(outstring)
      implicit none
      character(60), intent(out), optional :: outstring
    end subroutine version_eevjj
  end interface
end module ol_versiondata_eevjj


subroutine ol_version_eevjj(outstring)
  use ol_version, only: version, revision
  use ol_versiondata_eevjj, only: generator_revision
  implicit none
  character(60), intent(out), optional :: outstring
  character(60) :: version_string

  version_string = trim("OpenLoops " // trim(version))                  // & ! 26
                      & ", rev. " // trim(revision)                     // & !  7 + 4
                      & ", generator rev. " // trim(generator_revision) // & ! 17 + 4
                      & char(0)                                              !  1 --> 59 characters

  if (present(outstring)) then
    outstring = version_string
  else
    write(*,*) version_string
  end if
end subroutine ol_version_eevjj
