# FORtran library for Computational Environmental Systems

<div align="center">
<img src="https://git.ufz.de/chs/logos/-/raw/master/Forces.png" alt="Forces-LOGO" width="251px" style="width:251px;"/>
</div>

This is the FORTRAN library of the

> Department Computational Hydrosystems<br/>
> Helmholtz Centre for Environmental Research - UFZ<br/>
> Permoserstr. 15<br/>
> 04318 Leipzig, Germany

It is a lightweight fork of the `jams_fortran` library maintained by Matthias Cuntz et al: https://github.com/mcuntz/jams_fortran

The `jams_fortran` library was formerly developed at the CHS department at the UFZ and is now released under the [MIT license](https://github.com/mcuntz/jams_fortran/blob/master/LICENSE).


## Dependencies

NetCDF-Fortran

Recent versions can be downloaded from [here](ftp://ftp.unidata.ucar.edu/pub/netcdf/).
Instructions can be found [here](https://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html).

It is recommended to have a clean installation at a custom location
for a C compiler, a Fortran compiler, the NetCDF C library and the
NetCDF Fortran library with consistent compilers.

You can also use a conda environment to get all dependencies:

```bash
conda create -y --prefix ./forces_env
conda activate ./forces_env
conda config --add channels conda-forge
conda config --set channel_priority strict
conda install -y cmake make fortran-compiler netcdf-fortran fypp
cmake -DCMAKE_BUILD_TYPE=Release -B build -S .
cmake --build build --parallel
```

## License

This file is part of the UFZ CHS FORCES code base.

The routines is released under the GNU Lesser General Public License. The following applies:
The UFZ Fortran library is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

FORCES is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

### Doxygen Awesome

Doxygen Awesome is included as subrepo under `doc/doxygen-awesome-css/` from https://github.com/jothepro/doxygen-awesome-css. It is released under the MIT license.

### Cmake Fortran Scripts

The CHS Cmake Fortran Scripts repository is included as subrepo under `cmake/` from https://git.ufz.de/chs/cmake-fortran-scripts. It is released under the GNU LGPLv3 license.

### HPC Fortran Module Loads

The CHS HPC Fortran Module Loads repository is included as subrepo under `hpc-module-loads/` from https://git.ufz.de/chs/HPC-Fortran-module-loads. It is released under the GNU LGPLv3 license.
