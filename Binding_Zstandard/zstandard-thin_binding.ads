--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with System;
with Interfaces.C.Strings;

package Zstandard.Thin_Binding is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   ------------------
   --  Data Types  --
   ------------------

   type Zstd_uint64 is mod 2 ** 64;
   type Zstd_uint32 is mod 2 ** 32;

   type ZSTD_CCtx_ptr  is new System.Address;
   type ZSTD_DCtx_ptr  is new System.Address;
   type ZSTD_CDict_ptr is new System.Address;
   type ZSTD_DDict_ptr is new System.Address;

   ---------------
   --  Version  --
   ---------------

   function ZSTD_versionNumber return IC.unsigned;
   pragma Import (C, ZSTD_versionNumber, "ZSTD_versionNumber");

   ------------------------
   --  Helper functions  --
   ------------------------

   --  tells if a `size_t` function result is an error code
   function ZSTD_isError (code : IC.size_t) return IC.unsigned;
   pragma Import (C, ZSTD_isError, "ZSTD_isError");

   --  provides readable string for an error code
   function ZSTD_getErrorName (code : IC.size_t) return ICS.chars_ptr;
   pragma Import (C, ZSTD_getErrorName, "ZSTD_getErrorName");

   --  maximum compressed size (worst case scenario)
   function ZSTD_compressBound (srcSize : IC.size_t) return IC.size_t;
   pragma Import (C, ZSTD_compressBound, "ZSTD_compressBound");

   --  maximum compression level available
   function ZSTD_maxCLevel return IC.int;
   pragma Import (C, ZSTD_maxCLevel, "ZSTD_maxCLevel");

   ------------------------
   --  Simple functions  --
   ------------------------

   --  Compresses `src` buffer into already allocated `dst`.
   --  Hint : compression runs faster if `dstCapacity` >=  `ZSTD_compressBound(srcSize)`.
   --  @return : the number of bytes written into `dst` (<= `dstCapacity),
   --            or an error code if it fails (which can be tested using ZSTD_isError())
   function ZSTD_compress
     (dst : ICS.chars_ptr; dstCapacity : IC.size_t;
      src : ICS.chars_ptr; srcSize     : IC.size_t;
      compressionLevel : IC.int) return IC.size_t;
   pragma Import (C, ZSTD_compress, "ZSTD_compress");

   --  @return : decompressed size as a 64-bits value _if known_, 0 otherwise.
   --   note 1 : decompressed size can be very large (64-bits value),
   --            potentially larger than what local system can handle as a single memory segment.
   --            In which case, it's necessary to use streaming mode to decompress data.
   --   note 2 : decompressed size is an optional field, that may not be present.
   --            When `return==0`, consider data to decompress could have any size.
   --            In which case, it's necessary to use streaming mode to decompress data,
   --            or rely on application's implied limits.
   --            (e.g., it may know that its own data is necessarily cut into blocks <= 16 KB).
   --   note 3 : decompressed size could be wrong or intentionally modified !
   --            Always ensure result fits within application's authorized limits !
   --            Each application can have its own set of conditions.
   --            If the intention is to decompress public data compressed by zstd command line
   --            utility, it is recommended to support at least 8 MB for extended compatibility.
   --   note 4 : when `return==0`, if precise failure cause is needed, use ZSTD_getFrameParams()
   --            to know more.
   function ZSTD_getDecompressedSize
     (src     : ICS.chars_ptr;
      srcSize : IC.size_t) return Zstd_uint64;
   pragma Import (C, ZSTD_getDecompressedSize, "ZSTD_getDecompressedSize");

   --  `compressedSize` : is the _exact_ size of compressed input, else decompression will fail.
   --  `dstCapacity` must be equal or larger than originalSize (see ZSTD_getDecompressedSize() ).
   --  If originalSize is unknown, and if there is no implied application-specific limitations,
   --  it's necessary to use streaming mode to decompress data.
   --  @return : the number of bytes decompressed into `dst` (<= `dstCapacity`),
   --            or an errorCode if it fails (which can be tested using ZSTD_isError())
   function ZSTD_decompress
     (dst : ICS.chars_ptr; dstCapacity    : IC.size_t;
      src : ICS.chars_ptr; compressedSize : IC.size_t) return IC.size_t;
   pragma Import (C, ZSTD_decompress, "ZSTD_decompress");

   -----------------------------
   --  Simple dictionary API  --
   -----------------------------

   --  Compression using a predefined Dictionary (see dictBuilder/zdict.h).
   --  Note : This function load the dictionary, resulting in a significant startup time.
   function ZSTD_compress_usingDict
     (ctx  : ZSTD_CCtx_ptr;
      dst  : ICS.chars_ptr; dstCapacity : IC.size_t;
      src  : ICS.chars_ptr; srcSize     : IC.size_t;
      dict : ICS.chars_ptr; dictSize    : IC.size_t;
      compressionLevel : IC.int) return IC.size_t;
   pragma Import (C, ZSTD_compress_usingDict, "ZSTD_compress_usingDict");

   --  Decompression using a predefined Dictionary (see dictBuilder/zdict.h).
   --  Dictionary must be identical to the one used during compression.
   --  Note : This function load the dictionary, resulting in a significant startup time
   function ZSTD_decompress_usingDict
     (dctx : ZSTD_DCtx_ptr;
      dst  : ICS.chars_ptr; dstCapacity : IC.size_t;
      src  : ICS.chars_ptr; srcSize     : IC.size_t;
      dict : ICS.chars_ptr; dictSize    : IC.size_t) return IC.size_t;
   pragma Import (C, ZSTD_decompress_usingDict, "ZSTD_decompress_usingDict");

   ---------------------------
   --  Fast Dictionary API  --
   ---------------------------

   --  Create a digested dictionary, ready to start compression operation without startup delay.
   --  `dict` can be released after creation
   function ZSTD_createCDict
     (dict : ICS.chars_ptr; dictSize : IC.size_t;
      compressionLevel : IC.int) return ZSTD_CDict_ptr;
   pragma Import (C, ZSTD_createCDict, "ZSTD_createCDict");

   function ZSTD_freeCDict (CDict : ZSTD_CDict_ptr) return IC.size_t;
   pragma Import (C, ZSTD_freeCDict, "ZSTD_freeCDict");

   --  Compression using a pre-digested Dictionary.
   --  Faster startup than ZSTD_compress_usingDict(), recommended when same dictionary is
   --  used multiple times.  Note that compression level is decided during dictionary creation.
   function ZSTD_compress_usingCDict
     (ctx   : ZSTD_CCtx_ptr;
      dst   : ICS.chars_ptr; dstCapacity : IC.size_t;
      src   : ICS.chars_ptr; srcSize     : IC.size_t;
      CDict : ZSTD_CDict_ptr) return IC.size_t;
   pragma Import (C, ZSTD_compress_usingCDict, "ZSTD_compress_usingCDict");

   --  Create a digested dictionary, ready to start decompression operation without startup delay.
   --  `dict` can be released after creation
   function ZSTD_createDDict (dict : ICS.chars_ptr; dictSize : IC.size_t) return ZSTD_DDict_ptr;
   pragma Import (C, ZSTD_createDDict, "ZSTD_createDDict");

   function ZSTD_freeDDict (ddict : ZSTD_DDict_ptr) return IC.size_t;
   pragma Import (C, ZSTD_freeDDict, "ZSTD_freeDDict");

   --  Decompression using a digested Dictionary
   --  Faster startup than ZSTD_decompress_usingDict(), recommended when same dictionary is
   --  used multiple times.
   function ZSTD_decompress_usingDDict
     (dctx  : ZSTD_DCtx_ptr;
      dst   : ICS.chars_ptr; dstCapacity : IC.size_t;
      src   : ICS.chars_ptr; srcSize     : IC.size_t;
      ddict : ZSTD_DDict_ptr) return IC.size_t;
   pragma Import (C, ZSTD_decompress_usingDDict, "ZSTD_decompress_usingDDict");

   ----------------------------------
   --  Explicit memory management  --
   ----------------------------------

   --  Compression context
   function ZSTD_createCCtx return ZSTD_CCtx_ptr;
   pragma Import (C, ZSTD_createCCtx, "ZSTD_createCCtx");

   function ZSTD_freeCCtx (cctx : ZSTD_CCtx_ptr) return IC.size_t;
   pragma Import (C, ZSTD_freeCCtx, "ZSTD_freeCCtx");

   --  Same as ZSTD_compress(), requires an allocated ZSTD_CCtx (see ZSTD_createCCtx())
   function ZSTD_compressCCtx
     (ctx : ZSTD_CCtx_ptr;
      dst : ICS.chars_ptr; dstCapacity : IC.size_t;
      src : ICS.chars_ptr; srcSize     : IC.size_t;
      compressionLevel : IC.int) return IC.size_t;
   pragma Import (C, ZSTD_compressCCtx, "ZSTD_compressCCtx");

   --  Decompression context
   function ZSTD_createDCtx return ZSTD_DCtx_ptr;
   pragma Import (C, ZSTD_createDCtx, "ZSTD_createDCtx");

   function ZSTD_freeDCtx (dctx : ZSTD_DCtx_ptr) return IC.size_t;
   pragma Import (C, ZSTD_freeDCtx, "ZSTD_freeDCtx");

   --  Same as ZSTD_decompress(), requires an allocated ZSTD_DCtx (see ZSTD_createDCtx())
   function ZSTD_decompressDCtx
     (ctx : ZSTD_DCtx_ptr;
      dst : ICS.chars_ptr; dstCapacity : IC.size_t;
      src : ICS.chars_ptr; srcSize     : IC.size_t) return IC.size_t;
   pragma Import (C, ZSTD_decompressDCtx, "ZSTD_decompressDCtx");

end Zstandard.Thin_Binding;
