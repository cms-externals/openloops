
! This template is filled by the code generator SCons script

module ol_versiondata_eettttj
  implicit none
  character(4) :: generator_revision = "1497"
  interface
    subroutine version_eettttj(outstring)
      implicit none
      character(60), intent(out), optional :: outstring
    end subroutine version_eettttj
  end interface
end module ol_versiondata_eettttj


subroutine ol_version_eettttj(outstring)
  use ol_version, only: version, revision
  use ol_versiondata_eettttj, only: generator_revision
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
end subroutine ol_version_eettttj
