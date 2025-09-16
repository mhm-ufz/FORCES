# LICENSE

[TOC]

## COPYRIGHT

This file is part of the UFZ CHS Fortran library for Computational Environmental Systems (FORCES).


## COPYRIGHT HOLDERS

 Copyright(c) 2005-2025, the CHS Developers, Sabine Attinger: All rights reserved.

The code is a property of:

> The Department Computational Hydrosystems (CHS) at the
> Helmholtz-Zentrum fuer Umweltforschung GmbH - UFZ<br/>
> Registered Office: Leipzig<br/>
> Registration Office: Amtsgericht Leipzig<br/>
> Trade Register: Nr. B 4703

The chronological list of FORCES developers is provided in the AUTHORS file.

The UFZ (CHS) FORCES code is free software. You can
redistribute it and/or modify it under the terms of the GNU General
Public License as published by the free Software Foundation either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You received a copy of the GNU Lesser General Public License along
with the UFZ CHS FORCES code. It can be found
in the files `COPYING` and `COPYING.LESSER` provided with this software.
The complete GNU license text can also be found at <http://www.gnu.org/licenses/>.


## Contact

- mHM Admins (E-mail: mhm-admin@ufz.de)
- Project Leader: Prof. Dr. Luis Samaniego (E-mail: luis.samaniego@ufz.de)
- Stephan Thober (E-mail: stephan.thober@ufz.de)
- Sebastian Müller (E-mail: sebastian.mueller@ufz.de)
- Supervisor: Prof. Dr. Sabine Attinger (E-mail: sabine.attinger@ufz.de)

> Department Computational Hydrosystems (CHS)<br/>
> Helmholtz Centre for Environmental Research - UFZ<br/>
> Permoserstr. 15<br/>
> 04318 Leipzig, Germany


## Redistribution

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

-  Redistributions of source code must retain the above
   copyright notice, this list of conditions, the following disclaimer
   and the modification conditions.
-  Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions, the following disclaimer and the
   modification conditions in the documentation and/or other materials
   provided with the distribution.
-  Neither the name of Helmholtz-Zentrum fuer Umweltforschung GmbH -
   UFZ, nor the names of its contributors may be used to endorse or
   promote products derived from this software without specific prior
   written permission.
-  Redistributions of source code are allowed for research purposes
   ONLY. For commercial applications you need to consult the contact
   persons of the Department Computational Hydrosystems (CHS)
   at the UFZ.


## Modification

If the software is modified to produce derivative works, such modified
software should be clearly marked, so as not to confuse it with the
version available from UFZ.

We request to include the following notice in any derivative work
based on FORCES. Please include the following lines at every location
of the derivative code where pieces of the original FORCES code
have been taken:

```fortran
!> \note The following lines have been obtained from the FORCES source (https://git.ufz.de/chs/forces)
!! - gitversion:        < Provide git version of the original source >
!! - origin:            < Provide the name of the original subroutine >
!! - lines:             < Provide from-to lines in the original subroutine >
!! - authors:           < Provide the complete list of original authors >
!! - modifications:     < Indicate in detail your contribution >
!! - date_modification: < Indicate the date of the modification >
!> \copyright (c) 2005 - today, CHS-Developers (CHS)
```

> **NOTE**
> Failure to provide this information in derivative works
> constitutes a breach of the FORCES Copyright.
> < .. > information need to be provided by any developer using
> the original code of FORCES.


## Breach of the Copyright

The following are the potential causes for the Breach of FORCES Copyright.

-   Deletion of the original copyright in the derived codes.
-   Improper citation of the used material in the derived codes.
    See the notice above.
-   Deletion of the original author's list and the development history
    (partially or in totality).
-   Changing any paragraph of the GNU licence provided.
-   Not inclusion of the GNU licence in the derived code.
-   Not citing the suggested papers in any written work that is based on
    the FORCES code or derived software.

If the derived FORCES code has breached the Copyright, the Copyright Holders
will request the repository provider (e.g., GitHub) to take down the
respective code following the corresponding guidelines. In the case of GitHub,
this is specified at

https://docs.github.com/en/site-policy/content-removal-policies/dmca-takedown-policy


## Disclaimer of Warranty

THERE IS NO WARRANTY FOR THIS SOFTWARE, TO THE EXTENT PERMITTED BY
APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE
HELMHOLTZ-ZENTRUM FUER UMWELTFORSCHUNG GMBH - UFZ AND CONTRIBUTORS
PROVIDE THIS SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS
WITH YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF
ALL NECESSARY SERVICING, REPAIR OR CORRECTION.


## Limitation of Liability

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL THE HELMHOLTZ-ZENTRUM FUER UMWELTFORSCHUNG GMBH - UFZ AND
CONTRIBUTORS OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS THE
SOFTWARE AS PERMITTED BY GNU GENERAL PUBLIC LICENSE, BE LIABLE FOR
DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL
DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE SOFTWARE
(INCLUDING BUT NOT LIMITED TO LOSS OF DATA, OR DATA BEING RENDERED
INACCURATE OR LOSSES SUSTAINED BY THE USER OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER PROGRAMS),  EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


## Used and included libraries

- `doxygen-awesome-css` (`doc/doxygen-awesome-css/`)
  - purpose: documentation formatting
  - sources: https://github.com/jothepro/doxygen-awesome-css
  - authors: jothepro
  - license: MIT License (included)

- `cmake-fortran-scripts` (`cmake/`)
  - purpose: cmake scripts
  - sources: https://git.ufz.de/chs/cmake-fortran-scripts
  - authors: CHS Developers
  - license: MIT License (included)

- `HPC-Fortran-module-loads` (`hpc-module-loads/`)
  - purpose: module load scripts
  - sources: https://git.ufz.de/chs/HPC-Fortran-module-loads
  - authors: CHS Developers
  - license: MIT License (included)

- `flogging` (`src/mo_logging.F90`)
  - purpose: basis for the logging module
  - sources: https://github.com/DaanVanVugt/flogging
  - authors: Daan van Vugt
  - license: MIT License (included)

- `daglib` (`src/mo_dag.f90`)
  - purpose: A directed acyclic graph implementation.
  - sources: https://github.com/jacobwilliams/daglib
  - authors: Jacob Williams
  - license: BSD 3-Clause License (included)

- `flist` (`src/mo_list.f90`)
  - purpose: A generic list implementation.
  - sources: https://github.com/jacobwilliams/flist
  - authors: Jacob Williams
  - license: BSD 3-Clause License (included)

- `jams-fortran`
  - purpose: FORCES is a minimal fork of the jams-fortran library that was formerly developed at the CHS department
  - sources: https://github.com/mcuntz/jams_fortran
  - authors: Matthias Cuntz, Juliane Mai, Stephan Thober
  - license: MIT License
