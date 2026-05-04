#include <netcdf.h>
#include <stddef.h>
#include <string.h>

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
