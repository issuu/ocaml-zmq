/* 
 * Copyright (c) 2010 Andre Nathan <andre@sneakymustard.com>
 *                    Jeff Shaw <shawjef3@msu.edu>
 */

#include "caml_uint/uint64.h"

#include <stdint.h>

#include <caml/intext.h>
#include <caml/custom.h>
#include <caml/memory.h>

static int
uint64_cmp(value v1, value v2)
{
    uint64 i1 = CAML_UINT_Uint64_val(v1);
    uint64 i2 = CAML_UINT_Uint64_val(v2);
    return (i1 > i2) - (i1 < i2);
}

static intnat
uint64_hash(value v)
{
    return((intnat)CAML_UINT_Uint64_val(v));
}

static void
uint64_serialize(value v, uintnat *wsize_32, uintnat *wsize_64)
{
    caml_serialize_int_8(CAML_UINT_Uint64_val(v));
    *wsize_32 = *wsize_64 = 8;
}

static uintnat
uint64_deserialize(void *dst)
{
    *((uint64 *) dst) = caml_deserialize_uint_8();
    return 8;
}

static struct custom_operations uint64_ops = {
    "uint.uint64",
    custom_finalize_default,
    uint64_cmp,
    uint64_hash,
    uint64_serialize,
    uint64_deserialize
};

CAMLprim value
caml_uint_copy_uint64(uint64 i)
{
    CAMLparam0();
    value res = caml_alloc_custom(&uint64_ops, 8, 0, 1);
    CAML_UINT_Uint64_val(res) = i;
    CAMLreturn (res);
};

