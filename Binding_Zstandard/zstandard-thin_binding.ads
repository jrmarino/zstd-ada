--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with System;
with Interfaces.C.Strings;

package Zstandard.Thin_Binding is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   -----------------
   --  Constants  --
   -----------------

   ZSTD_REP_NUM : constant IC.int := 3;

   ------------------
   --  Data Types  --
   ------------------

   type Zstd_uint64 is mod 2 ** 64;
   type Zstd_uint32 is mod 2 ** 32;

   type Zstd_block32 is array (Natural range <>) of Zstd_uint32;
   type Zstd_block64 is array (Natural range <>) of Zstd_uint64;

   type Zstd_uint32_Access is access all Zstd_uint32;

   type ZSTD_strategy is
     (ZSTD_fast,
      ZSTD_dfast,
      ZSTD_greedy,
      ZSTD_lazy,
      ZSTD_lazy2,
      ZSTD_btlazy2,
      ZSTD_btopt);
   pragma Convention (C, ZSTD_strategy);

   type ZSTD_compressionParameters is record
      windowLog    : IC.unsigned;
      chainLog     : IC.unsigned;
      hashLog      : IC.unsigned;
      searchLog    : IC.unsigned;
      searchLength : IC.unsigned;
      targetLength : IC.unsigned;
      strategy     : ZSTD_strategy;
   end record;
   pragma Convention (C, ZSTD_compressionParameters);

   type ZSTD_frameParameters is record
      contentSizeFlag : IC.unsigned;
      checksumFlag    : IC.unsigned;
      noDictIDFlag    : IC.unsigned;
   end record;
   pragma Convention (C, ZSTD_frameParameters);

   type ZSTD_parameters is record
      cParams : ZSTD_compressionParameters;
      fParams : ZSTD_frameParameters;
   end record;
   pragma Convention (C, ZSTD_parameters);

   type XXH64_state_t is record
      total_len : Zstd_uint64;
      seed      : Zstd_uint64;
      v1        : Zstd_uint64;
      v2        : Zstd_uint64;
      v3        : Zstd_uint64;
      v4        : Zstd_uint64;
      mem64     : Zstd_block64 (1 .. 4);
      memsize   : IC.unsigned;
   end record;
   pragma Convention (C, XXH64_state_t);

   type ZSTD_customMem is record
      customAlloc : System.Address;  --  void* (*ZSTD_allocFunction) (void* opaque, size_t size);
      customFree  : System.Address;  --  void  (*ZSTD_freeFunction) (void* opaque, void* address);
      opaque      : System.Address;
   end record;
   pragma Convention (C, ZSTD_customMem);

   type ZSTD_CCtx is record
      nextSrc          : ICS.chars_ptr;
      base             : ICS.chars_ptr;
      dictBase         : ICS.chars_ptr;
      dictLimit        : Zstd_uint32;
      lowLimit         : Zstd_uint32;
      nextToUpdate     : Zstd_uint32;
      nextToUpdate3    : Zstd_uint32;
      hashLog3         : Zstd_uint32;
      loadedDictEnd    : Zstd_uint32;
      stage            : Zstd_uint32;
      rep              : Zstd_block32 (1 .. ZSTD_REP_NUM);
      savedRep         : Zstd_block32 (1 .. ZSTD_REP_NUM);
      dictID           : Zstd_uint32;
      params           : ZSTD_parameters;
      workspace        : System.Address;
      workSpaceSize    : IC.size_t;
      blockSize        : IC.size_t;
      frameContentSize : Zstd_uint64;
      xxhState         : XXH64_state_t;
      customMem        : ZSTD_customMem;
      seqStore         :
      hashTable        : Zstd_uint32_Access;
      hashTable3       : Zstd_uint32_Access;
      chainTable       : Zstd_uint32_Access;
   end record;



   ---------------
   --  Version  --
   ---------------

   function ZSTD_versionNumber return IC.unsigned;
   pragma Import (C, ZSTD_versionNumber, "ZSTD_versionNumber");

   ------------------------
   --  Helper functions  --
   ------------------------

   --  tells if a `size_t` function result is an error code
   function ZSTD_isError (IC.size_t code) return IC.unsigned;
   pragma Import (C, ZSTD_isError, "ZSTD_isError");

   --  provides readable string for an error code
   function ZSTD_getErrorName (IC.size_t code) return ICS.chars_ptr;
   pragma Import (C, ZSTD_getErrorName, "ZSTD_getErrorName");

   --  maximum compressed size (worst case scenario)
   function ZSTD_compressBound (IC.size_t srcSize) return IC.size_t;
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
     (ctx  : ZSTD_CCtx_Access;
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
     (dctx : ZSTD_DCtx_Access;
      dst  : ICS.chars_ptr; dstCapacity : IC.size_t;
      src  : ICS.chars_ptr; srcSize     : IC.size_t;
      dict : ICS.chars_ptr; dictSize    : IC.size_t) return IC.size_t;
   pragma Import (C, ZSTD_decompress_usingDict, "ZSTD_decompress_usingDict");

end Zstandard.Thin_Binding;
