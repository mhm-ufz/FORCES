/**
 * \file mo_os_backend.c
 * \ingroup f_io
 * \brief Portable C backend for the Fortran mo_os module.
 * \details Implements working-directory operations and path classification
 * through the POSIX or Windows APIs. Arguments use null-terminated C strings;
 * mo_os handles conversion to and from Fortran character variables.
 * \see mo_os
 * \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
 * FORCES is released under the LGPLv3+ license \license_note
 */
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

/** \brief Path classifications returned by forces_os_path_kind(). */
enum {
    FORCES_OS_PATH_MISSING = 0, /**< Missing path or failed path lookup. */
    FORCES_OS_PATH_FILE = 1, /**< Existing entry that is not a directory. */
    FORCES_OS_PATH_DIR = 2 /**< Existing directory. */
};

#ifdef _WIN32
/**
 * \brief Replace Windows backslashes with FORCES' forward-slash separators.
 * \param[in,out] path Null-terminated path to modify; NULL is ignored.
 */
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

/**
 * \brief Replace forward slashes with native Windows path separators.
 * \param[in,out] path Null-terminated path to modify; NULL is ignored.
 */
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

/**
 * \brief Copy the current working directory into a caller-owned buffer.
 * \param[out] buffer Destination for a null-terminated path on success.
 * \param[in] buffer_len Buffer capacity in bytes, including the terminator.
 * \return Zero on success, EINVAL for a NULL or zero-capacity buffer, or the
 * underlying system error from errno.
 * \details On POSIX, an insufficient buffer is reported by getcwd(). On Windows,
 * the path is converted to forward slashes and truncated to buffer_len - 1 bytes
 * if necessary; truncation still returns success. The caller retains ownership
 * of the destination buffer.
 */
int forces_os_getcwd(char *buffer, size_t buffer_len)
{
    if (buffer == NULL || buffer_len == 0) {
        return EINVAL;
    }

    buffer[0] = '\0';

#ifdef _WIN32
    {
        char *cwd = _getcwd(NULL, 0);
        size_t copy_len;

        if (cwd == NULL) {
            return errno;
        }

        forces_os_normalize_separators(cwd);
        copy_len = strlen(cwd);
        if (copy_len >= buffer_len) {
            copy_len = buffer_len - 1;
        }
        memcpy(buffer, cwd, copy_len);
        buffer[copy_len] = '\0';
        free(cwd);
    }
#else
    if (getcwd(buffer, buffer_len) == NULL) {
        return errno;
    }
#endif

    return 0;
}

/**
 * \brief Change the process-wide current working directory.
 * \param[in] path Null-terminated path to the destination directory.
 * \return Zero on success, EINVAL for a NULL or empty path, ENOMEM if the Windows
 * path copy cannot be allocated, or the underlying system error from errno.
 * \details Windows paths are copied and converted to native separators without
 * modifying the caller's string. Relative paths use the current working directory.
 */
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

/**
 * \brief Classify an existing path as a directory or a non-directory entry.
 * \param[in] path Null-terminated path to query.
 * \return FORCES_OS_PATH_DIR for a directory, FORCES_OS_PATH_FILE for another
 * existing entry, or FORCES_OS_PATH_MISSING for a missing path or failed lookup.
 * \details NULL and empty paths, and Windows path-allocation failures, also
 * return FORCES_OS_PATH_MISSING. Windows queries a native-separator copy using
 * GetFileAttributesA(); POSIX uses stat(), following symbolic links.
 */
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
