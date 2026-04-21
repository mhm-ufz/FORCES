#include <errno.h>
#include <stddef.h>
#include <string.h>

#ifdef _WIN32
#include <direct.h>
#include <stdlib.h>
#include <windows.h>
#else
#include <limits.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

enum {
    FORCES_OS_PATH_MISSING = 0,
    FORCES_OS_PATH_FILE = 1,
    FORCES_OS_PATH_DIR = 2
};

#ifdef _WIN32
static void forces_os_normalize_separators(char *path)
{
    size_t i;

    if (path == NULL) {
        return;
    }

    for (i = 0; path[i] != '\0'; ++i) {
        if (path[i] == '\\') {
            path[i] = '/';
        }
    }
}

static void forces_os_to_windows_path(char *path)
{
    size_t i;

    if (path == NULL) {
        return;
    }

    for (i = 0; path[i] != '\0'; ++i) {
        if (path[i] == '/') {
            path[i] = '\\';
        }
    }
}
#endif

int forces_os_getcwd(char *buffer, size_t buffer_len)
{
    if (buffer == NULL || buffer_len == 0) {
        return EINVAL;
    }

    buffer[0] = '\0';

#ifdef _WIN32
    {
        char *cwd = _getcwd(NULL, 0);

        if (cwd == NULL) {
            return errno;
        }

        forces_os_normalize_separators(cwd);
        strncpy(buffer, cwd, buffer_len - 1);
        buffer[buffer_len - 1] = '\0';
        free(cwd);
    }
#else
    if (getcwd(buffer, buffer_len) == NULL) {
        return errno;
    }
#endif

    return 0;
}

int forces_os_chdir(const char *path)
{
    int code;

    if (path == NULL || path[0] == '\0') {
        return EINVAL;
    }

#ifdef _WIN32
    {
        size_t len = strlen(path);
        char *native_path = (char *) malloc(len + 1);

        if (native_path == NULL) {
            return ENOMEM;
        }

        memcpy(native_path, path, len + 1);
        forces_os_to_windows_path(native_path);
        code = _chdir(native_path);
        free(native_path);
    }
#else
    code = chdir(path);
#endif

    return (code == 0) ? 0 : errno;
}

int forces_os_path_kind(const char *path)
{
    if (path == NULL || path[0] == '\0') {
        return FORCES_OS_PATH_MISSING;
    }

#ifdef _WIN32
    {
        DWORD attrs;
        size_t len = strlen(path);
        char *native_path = (char *) malloc(len + 1);

        if (native_path == NULL) {
            return FORCES_OS_PATH_MISSING;
        }

        memcpy(native_path, path, len + 1);
        forces_os_to_windows_path(native_path);
        attrs = GetFileAttributesA(native_path);
        free(native_path);

        if (attrs == INVALID_FILE_ATTRIBUTES) {
            return FORCES_OS_PATH_MISSING;
        }

        if ((attrs & FILE_ATTRIBUTE_DIRECTORY) != 0) {
            return FORCES_OS_PATH_DIR;
        }

        return FORCES_OS_PATH_FILE;
    }
#else
    {
        struct stat status;

        if (stat(path, &status) != 0) {
            return FORCES_OS_PATH_MISSING;
        }

        if (S_ISDIR(status.st_mode)) {
            return FORCES_OS_PATH_DIR;
        }

        return FORCES_OS_PATH_FILE;
    }
#endif
}
