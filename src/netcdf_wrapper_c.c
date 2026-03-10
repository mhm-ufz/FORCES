#include <netcdf.h>
#include <stddef.h>
#include <string.h>

void forces_nc_strerror_copy(int status, char *buffer, size_t buffer_len)
{
    const char *msg = nc_strerror(status);

    if (buffer == NULL || buffer_len == 0) {
        return;
    }

    if (msg == NULL) {
        buffer[0] = '\0';
        return;
    }

    strncpy(buffer, msg, buffer_len - 1);
    buffer[buffer_len - 1] = '\0';
}
