/**
 * \file netcdf_wrapper_c.c
 * \ingroup f_io
 * \brief C string-copy helper for the Fortran NetCDF wrapper.
 * \details Supports mo_netcdf_wrapper by copying NetCDF's error descriptions
 * into caller-owned storage instead of exposing the library's string pointer
 * to Fortran.
 * \see mo_netcdf_wrapper
 * \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
 * FORCES is released under the LGPLv3+ license \license_note
 */
#include <netcdf.h>
#include <stddef.h>
#include <string.h>

/**
 * \brief Copy the error description for a NetCDF status code.
 * \param[in] status NetCDF status code passed to nc_strerror().
 * \param[out] buffer Destination buffer; NULL is ignored.
 * \param[in] buffer_len Buffer capacity in bytes, including the terminator.
 * \details A zero-capacity buffer is left untouched. Otherwise, at most
 * buffer_len - 1 bytes are copied and a null terminator is appended. If NetCDF
 * returns no message, the buffer receives an empty string. Truncation is silent;
 * the caller owns the buffer and the NetCDF message remains unchanged.
 */
void forces_nc_strerror_copy(int status, char *buffer, size_t buffer_len)
{
    const char *msg = nc_strerror(status);
    size_t copy_len;

    if (buffer == NULL || buffer_len == 0) {
        return;
    }

    if (msg == NULL) {
        buffer[0] = '\0';
        return;
    }

    copy_len = strlen(msg);
    if (copy_len >= buffer_len) {
        copy_len = buffer_len - 1;
    }
    memcpy(buffer, msg, copy_len);
    buffer[copy_len] = '\0';
}
