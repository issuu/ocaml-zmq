/* 
 * Copyright (c) 2010 Andre Nathan <andre@sneakymustard.com>
 *                    Jeff Shaw <shawjef3@msu.edu>
 */

#ifndef CAML_UINT_UINT64_H_
#define CAML_UINT_UINT64_H_

#include <stdint.h>

#include <caml/mlvalues.h>

#define CAML_UINT_Uint64_val(v) (*((uint64 *)Data_custom_val(v)))

value caml_uint_copy_uint64(uint64 i);

#endif  // CAML_UINT_UINT64_H_

