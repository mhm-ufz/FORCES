!> \file    mo_forces_info.f90
!> \copydoc mo_forces_info

#ifndef FORCESVERSION
#define FORCESVERSION "0.0.0-dev0"
#endif
#ifndef FORCESDATE
#define FORCESDATE __DATE__
#endif
#define set_version(x) character(len = *), parameter :: version = x
#define set_date(x) character(len = *), parameter :: version_date = x

!> \brief   module with information about FORCES
!> \details Provides all information about FORCES as parameter.
!!          The \c version parameter will be set during compilation to
!!          \"\htmlinclude version.txt \latexinclude version.txt\".
!!          The \c version_date parameter will be set during compilation to
!!          \"\htmlinclude version_date.txt \latexinclude version_date.txt\",
!!          if it is a release version, otherwise it will be the current date.
!> \authors Sebastian Mueller
!> \date    May 2021
module mo_forces_info

  implicit none

  set_version(FORCESVERSION)
  !< Current program version (will be set to \htmlinclude version.txt \latexinclude version.txt)

  set_date(FORCESDATE)
  !< Time of current program version release (will be set to \htmlinclude version_date.txt \latexinclude version_date.txt)

end module mo_forces_info
