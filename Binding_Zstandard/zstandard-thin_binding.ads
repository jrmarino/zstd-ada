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

   ------------------------
   --  Simple functions  --
   ------------------------

   --  Compresses `srcSize` bytes from buffer `src` into buffer `dst` of size `dstCapacity`.
   --  Destination buffer must be already allocated.
   --  Compression runs faster if `dstCapacity` >=  `ZSTD_compressBound(srcSize)`.
   --  @return : the number of bytes written into `dst`,
   --            or an error code if it fails (which can be tested using ZSTD_isError())
   function ZSTD_compress
     (dst : ICS.chars_ptr; dstCapacity : IC.size_t;
      src : ICS.chars_ptr; srcSize     : IC.size_t;
      compressionLevel : IC.int) return IC.size_t;
   pragma Import (C, ZSTD_compress, "ZSTD_compress");

   --  @return : decompressed size if known, 0 otherwise.
   --     note : to know precise reason why result is `0`, follow up with ZSTD_getFrameParams()
   function ZSTD_getDecompressedSize
     (src     : ICS.chars_ptr;
      srcSize : IC.size_t) return Zstd_uint64;
   pragma Import (C, ZSTD_getDecompressedSize, "ZSTD_getDecompressedSize");

   --  `compressedSize` : is the _exact_ size of compressed input, else decompression will fail.
   --  `dstCapacity` must be equal or larger than originalSize.
   --  @return : the number of bytes decompressed into `dst` (<= `dstCapacity`),
   --            or an errorCode if it fails (which can be tested using ZSTD_isError())
   function ZSTD_decompress
     (dst : ICS.chars_ptr; dstCapacity    : IC.size_t;
      src : ICS.chars_ptr; compressedSize : IC.size_t) return IC.size_t;
   pragma Import (C, ZSTD_decompress, "ZSTD_decompress");


   -----------------------------
   --  Simple dictionary API  --
   -----------------------------

   --  Compression using a pre-defined Dictionary content (see dictBuilder).
   --  Note 1 : This function load the dictionary, resulting in a significant startup time.
   --  Note 2 : `dict` must remain accessible and unmodified during compression operation.
   --  Note 3 : `dict` can be `NULL`, in which case, it's equivalent to ZSTD_compressCCtx() */
   function ZSTD_compress_usingDict
     (ctx  : ZSTD_CCtx_ptr;
      dst  : ICS.chars_ptr; dstCapacity : IC.size_t;
      src  : ICS.chars_ptr; srcSize     : IC.size_t;
      dict : ICS.chars_ptr; dictSize    : IC.size_t;
      compressionLevel : IC.int) return IC.size_t;
   pragma Import (C, ZSTD_compress_usingDict, "ZSTD_compress_usingDict");

   --  Decompression using a pre-defined Dictionary content (see dictBuilder).
   --  Dictionary must be identical to the one used during compression.
   --  Note 1 : This function load the dictionary, resulting in a significant startup time
   --  Note 2 : `dict` must remain accessible and unmodified during compression operation.
   --  Note 3 : `dict` can be `NULL`, in which case, it's equivalent to ZSTD_decompressDCtx()
   function ZSTD_decompress_usingDict
     (dctx : ZSTD_DCtx_ptr;
      dst  : ICS.chars_ptr; dstCapacity : IC.size_t;
      src  : ICS.chars_ptr; srcSize     : IC.size_t;
      dict : ICS.chars_ptr; dictSize    : IC.size_t) return IC.size_t;
   pragma Import (C, ZSTD_decompress_usingDict, "ZSTD_decompress_usingDict");

   -------------------------------
   --  Advanced Dictionary API  --
   -------------------------------

   --  Create a digested dictionary, ready to start compression operation without startup delay.
   --  `dict` can be released after creation
   function ZSTD_createCDict
     (dict : ICS.chars_ptr; dictSize : IC.size_t;
      compressionLevel : IC.int) return ZSTD_CDict_ptr;
   pragma Import (C, ZSTD_createCDict, "ZSTD_createCDict");

   function ZSTD_freeCDict (CDict : ZSTD_CDict_ptr) return IC.size_t;
   pragma Import (C, ZSTD_freeCDict, "ZSTD_freeCDict");

   --  Compression using a pre-digested Dictionary.
   --  Much faster than ZSTD_compress_usingDict() when same dictionary is used multiple times.
   --  Note that compression level is decided during dictionary creation
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

   --   Decompression using a pre-digested Dictionary
   --  Much faster than ZSTD_decompress_usingDict() when same dictionary is used multiple times.
   function ZSTD_decompress_usingDDict
     (dctx  : ZSTD_DCtx_ptr;
      dst   : ICS.chars_ptr; dstCapacity : IC.size_t;
      src   : ICS.chars_ptr; srcSize     : IC.size_t;
      ddict : ZSTD_DDict_ptr) return IC.size_t;
   pragma Import (C, ZSTD_decompress_usingDDict, "ZSTD_decompress_usingDDict");

end Zstandard.Thin_Binding;
