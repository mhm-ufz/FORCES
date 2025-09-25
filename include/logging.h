/**
 * @file logging.h
 * @author Daan van Vugt
 * @brief This file was taken from the flogging library (https://github.com/DaanVanVugt/flogging).
 * @version 0.1
 * @date 2022-09-08
 *
 * @copyright This file was originally published under the MIT license.
 *
 * Copyright (c) 2016 Daan van Vugt
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

/* The lines below have little spacing to ease the fortran line-length requirements */

/* Log level constants */
#define LOG_LEVEL_FATAL_DEF 1
#define LOG_LEVEL_ERROR_DEF 2
#define LOG_LEVEL_WARN_DEF 3
#define LOG_LEVEL_INFO_DEF 4
#define LOG_LEVEL_DEBUG_DEF 5
#define LOG_LEVEL_TRACE_DEF 6
#define LOG_LEVEL_SUBTRACE_DEF 7

#define log_macro(level,format) if(logp(level))write(logu(level),format)trim(logl(level,__FILE__,__LINE__))//" ",
#define log_root(level,format) if(logp(level,0))write(logu(level),format)trim(logl(level,__FILE__,__LINE__))//" ",

/* First four log levels */
#define log_fatal(format) log_macro(LOG_LEVEL_FATAL_DEF,format)
#define log_error(format) log_macro(LOG_LEVEL_ERROR_DEF,format)
#define log_warn(format) log_macro(LOG_LEVEL_WARN_DEF,format)
#define log_info(format) log_macro(LOG_LEVEL_INFO_DEF,format)

#define log_fatal_root(format) log_root(LOG_LEVEL_FATAL_DEF,format)
#define log_error_root(format) log_root(LOG_LEVEL_ERROR_DEF,format)
#define log_warn_root(format) log_root(LOG_LEVEL_WARN_DEF,format)
#define log_info_root(format) log_root(LOG_LEVEL_INFO_DEF,format)

#ifdef DISABLE_LOG_DEBUG
#define log_debug(format) if(.false.)write(logu(LOG_LEVEL_DEBUG_DEF),format)
#define log_debug_root(format) if(.false.)write(logu(LOG_LEVEL_DEBUG_DEF),format)
#else
#define log_debug(format) log_macro(LOG_LEVEL_DEBUG_DEF,format)
#define log_debug_root(format) log_root(LOG_LEVEL_DEBUG_DEF,format)
#endif

#ifdef ENABLE_LOG_TRACE
#define log_trace(format) log_macro(LOG_LEVEL_TRACE_DEF,format)
#define log_trace_root(format) log_root(LOG_LEVEL_TRACE_DEF,format)
#define log_subtrace(format) log_macro(LOG_LEVEL_SUBTRACE_DEF,format)
#define log_subtrace_root(format) log_root(LOG_LEVEL_SUBTRACE_DEF,format)
#else
#define log_trace(format) if(.false.)write(logu(LOG_LEVEL_TRACE_DEF),format)
#define log_trace_root(format) if(.false.)write(logu(LOG_LEVEL_TRACE_DEF),format)
#define log_subtrace(format) if(.false.)write(logu(LOG_LEVEL_SUBTRACE_DEF),format)
#define log_subtrace_root(format) if(.false.)write(logu(LOG_LEVEL_SUBTRACE_DEF),format)
#endif
