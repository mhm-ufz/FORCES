!> \dir src
!> \brief Forces source files.

!> \dir examples
!> \brief Forces examples files.

!> \dir examples/01_grids
!> \brief Forces grid example files.

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
!!          The \p version parameter will be set during compilation
!!          to content of \a version.txt.
!!          The \p version_date parameter will be set during compilation
!!          to content of \a version_date.txt,
!!          if it is a release version, otherwise it will be the current date.
!> \authors Sebastian Mueller
!> \date    May 2021
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_forces_info

  implicit none

  set_version(FORCESVERSION)
  !< Current program version

  set_date(FORCESDATE)
  !< Time of current program version release

end module mo_forces_info
