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

#define log_write(level,format) if(logp(level))write(logu(level),format)logl(level,__FILE__,__LINE__),
#define log_plain(level,format) if(logp(level))write(logu(level),format)logl(level,__FILE__,__LINE__,.true.),
#define log_root(level,format) if(logp(level,0))write(logu(level),format)logl(level,__FILE__,__LINE__),
#define log_plain_root(level,format) if(logp(level,0))write(logu(level),format)logl(level,__FILE__,__LINE__,.true.),
#define log_core(core,level,format) if(logp(level,core))write(logu(level),format)logl(level,__FILE__,__LINE__),
#define log_plain_core(core,level,format) if(logp(level,core))write(logu(level),format)logl(level,__FILE__,__LINE__,.true.),

#define scope_write(scope_name,level,format) if(logp(level,scope=scope_name))write(logu(level),format)logl(level,__FILE__,__LINE__,scope=scope_name),
#define scope_plain(scope_name,level,format) if(logp(level,scope=scope_name))write(logu(level),format)logl(level,__FILE__,__LINE__,.true.,scope=scope_name),
#define scope_root(scope_name,level,format) if(logp(level,0,scope=scope_name))write(logu(level),format)logl(level,__FILE__,__LINE__,scope=scope_name),
#define scope_plain_root(scope_name,level,format) if(logp(level,0,scope=scope_name))write(logu(level),format)logl(level,__FILE__,__LINE__,.true.,scope=scope_name),
#define scope_core(core,scope_name,level,format) if(logp(level,core,scope=scope_name))write(logu(level),format)logl(level,__FILE__,__LINE__,scope=scope_name),
#define scope_plain_core(core,scope_name,level,format) if(logp(level,core,scope=scope_name))write(logu(level),format)logl(level,__FILE__,__LINE__,.true.,scope=scope_name),

/* First four log levels */
#define log_fatal(format) log_write(LOG_LEVEL_FATAL_DEF,format)
#define log_error(format) log_write(LOG_LEVEL_ERROR_DEF,format)
#define log_warn(format) log_write(LOG_LEVEL_WARN_DEF,format)
#define log_info(format) log_write(LOG_LEVEL_INFO_DEF,format)
#define log_text(format) log_plain(LOG_LEVEL_INFO_DEF,format)
#define scope_fatal(scope_name,format) scope_write(scope_name,LOG_LEVEL_FATAL_DEF,format)
#define scope_error(scope_name,format) scope_write(scope_name,LOG_LEVEL_ERROR_DEF,format)
#define scope_warn(scope_name,format) scope_write(scope_name,LOG_LEVEL_WARN_DEF,format)
#define scope_info(scope_name,format) scope_write(scope_name,LOG_LEVEL_INFO_DEF,format)
#define scope_text(scope_name,format) scope_plain(scope_name,LOG_LEVEL_INFO_DEF,format)

#define log_fatal_root(format) log_root(LOG_LEVEL_FATAL_DEF,format)
#define log_error_root(format) log_root(LOG_LEVEL_ERROR_DEF,format)
#define log_warn_root(format) log_root(LOG_LEVEL_WARN_DEF,format)
#define log_info_root(format) log_root(LOG_LEVEL_INFO_DEF,format)
#define log_text_root(format) log_plain_root(LOG_LEVEL_INFO_DEF,format)
#define scope_fatal_root(scope_name,format) scope_root(scope_name,LOG_LEVEL_FATAL_DEF,format)
#define scope_error_root(scope_name,format) scope_root(scope_name,LOG_LEVEL_ERROR_DEF,format)
#define scope_warn_root(scope_name,format) scope_root(scope_name,LOG_LEVEL_WARN_DEF,format)
#define scope_info_root(scope_name,format) scope_root(scope_name,LOG_LEVEL_INFO_DEF,format)
#define scope_text_root(scope_name,format) scope_plain_root(scope_name,LOG_LEVEL_INFO_DEF,format)
#define scope_fatal_core(core,scope_name,format) scope_core(core,scope_name,LOG_LEVEL_FATAL_DEF,format)
#define scope_error_core(core,scope_name,format) scope_core(core,scope_name,LOG_LEVEL_ERROR_DEF,format)
#define scope_warn_core(core,scope_name,format) scope_core(core,scope_name,LOG_LEVEL_WARN_DEF,format)
#define scope_info_core(core,scope_name,format) scope_core(core,scope_name,LOG_LEVEL_INFO_DEF,format)
#define scope_text_core(core,scope_name,format) scope_plain_core(core,scope_name,LOG_LEVEL_INFO_DEF,format)

#ifdef DISABLE_LOG_DEBUG
#define log_debug(format) ! debug comment:
#define log_debug_root(format) ! root debug comment:
#define scope_debug(scope_name,format) ! scope debug comment:
#define scope_debug_root(scope_name,format) ! scope root debug comment:
#define scope_debug_core(core,scope_name,format) ! scope core debug comment:
#else
#define log_debug(format) log_write(LOG_LEVEL_DEBUG_DEF,format)
#define log_debug_root(format) log_root(LOG_LEVEL_DEBUG_DEF,format)
#define scope_debug(scope_name,format) scope_write(scope_name,LOG_LEVEL_DEBUG_DEF,format)
#define scope_debug_root(scope_name,format) scope_root(scope_name,LOG_LEVEL_DEBUG_DEF,format)
#define scope_debug_core(core,scope_name,format) scope_core(core,scope_name,LOG_LEVEL_DEBUG_DEF,format)
#endif

#ifdef ENABLE_LOG_TRACE
#define log_trace(format) log_write(LOG_LEVEL_TRACE_DEF,format)
#define log_trace_root(format) log_root(LOG_LEVEL_TRACE_DEF,format)
#define log_subtrace(format) log_write(LOG_LEVEL_SUBTRACE_DEF,format)
#define log_subtrace_root(format) log_root(LOG_LEVEL_SUBTRACE_DEF,format)
#define scope_trace(scope_name,format) scope_write(scope_name,LOG_LEVEL_TRACE_DEF,format)
#define scope_trace_root(scope_name,format) scope_root(scope_name,LOG_LEVEL_TRACE_DEF,format)
#define scope_trace_core(core,scope_name,format) scope_core(core,scope_name,LOG_LEVEL_TRACE_DEF,format)
#define scope_subtrace(scope_name,format) scope_write(scope_name,LOG_LEVEL_SUBTRACE_DEF,format)
#define scope_subtrace_root(scope_name,format) scope_root(scope_name,LOG_LEVEL_SUBTRACE_DEF,format)
#define scope_subtrace_core(core,scope_name,format) scope_core(core,scope_name,LOG_LEVEL_SUBTRACE_DEF,format)
#else
#define log_trace(format) ! trace comment:
#define log_trace_root(format) ! root trace comment:
#define log_subtrace(format) ! subtrace comment:
#define log_subtrace_root(format) ! root subtrace comment:
#define scope_trace(scope_name,format) ! scope trace comment:
#define scope_trace_root(scope_name,format) ! scope trace root comment:
#define scope_trace_core(core,scope_name,format) ! scope trace core comment:
#define scope_subtrace(scope_name,format) ! scope subtrace comment:
#define scope_subtrace_root(scope_name,format) ! scope subtrace root comment:
#define scope_subtrace_core(core,scope_name,format) ! scope subtrace core comment:
#endif
