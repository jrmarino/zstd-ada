--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Zstandard.Functions is

   --------------------
   --  Zstd_Version  --
   --------------------
   function Zstd_Version return String
   is
      function trim (num : Natural) return String;
      function trim (num : Natural) return String
      is
         numstr : constant String := num'Img;
      begin
         return numstr (numstr'First + 1 .. numstr'Last);
      end trim;

      dot   : constant String := ".";
      res   : Thin.IC.unsigned;
      ver   : Natural;
      major : Natural;
      minor : Natural;
      point : Natural;
   begin
      res   := Thin.ZSTD_versionNumber;
      ver   := Natural (res);
      point := ver mod 100;
      minor := (ver / 100) mod 100;
      major := (ver / 10000) mod 100;

      return trim (major) & dot & trim (minor) & dot & trim (point);
   end Zstd_Version;


   ------------------
   --  convert #1  --
   ------------------
   function convert (data : Thin.IC.char_array) return String
   is
      use type Thin.IC.size_t;
      result : String (1 .. data'Length);
      arrow : Thin.IC.size_t := data'First;
   begin
      for z in result'Range loop
         result (z) := Character (data (arrow));
         arrow := arrow + 1;
      end loop;
      return result;
   end convert;


   ------------------
   --  convert #2  --
   ------------------
   function convert (data : String) return Thin.IC.char_array
   is
      use type Thin.IC.size_t;
      reslen : Thin.IC.size_t := Thin.IC.size_t (data'Length);
      result : Thin.IC.char_array (1 .. reslen);
      arrow  : Thin.IC.size_t := 1;
   begin
      for z in data'Range loop
         result (arrow) := Thin.IC.char (data (z));
         arrow := arrow + 1;
      end loop;
      return result;
   end convert;


   ----------------
   --  Compress  --
   ----------------
   function Compress
     (source_data : String;
      successful  : out Boolean;
      quality     : Compression_Level := Fastest_Compression) return String
   is
      comp_bytes  : Thin.IC.size_t;
      is_error    : Thin.IC.unsigned;
      level       : constant Thin.IC.int := Thin.IC.int (quality);

      src         : aliased Thin.IC.char_array := convert (source_data);
      srcSize     : constant Thin.IC.size_t := Thin.IC.size_t (source_data'Length);
      dstCapacity : constant Thin.IC.size_t := Thin.ZSTD_compressBound (srcSize);
      dst         : aliased Thin.IC.char_array := (1 .. dstCapacity => Thin.IC.nul);

      dst_pointer : Thin.ICS.chars_ptr := Thin.ICS.To_Chars_Ptr (dst'Unchecked_Access);
      src_pointer : Thin.ICS.chars_ptr := Thin.ICS.To_Chars_Ptr (src'Unchecked_Access);
   begin
      comp_bytes := Thin.ZSTD_compress (dst              => dst_pointer,
                                        dstCapacity      => dstCapacity,
                                        src              => src_pointer,
                                        srcSize          => srcSize,
                                        compressionLevel => level);
      is_error := Thin.ZSTD_isError (code => comp_bytes);
      successful := (Natural (is_error) = 0);
      if successful then
         return convert (dst (1 .. comp_bytes));
      else
         return Thin.ICS.Value (Thin.ZSTD_getErrorName (code => comp_bytes));
      end if;
   end Compress;


   ------------------
   --  Decompress  --
   ------------------
   function Decompress
     (source_data : String;
      successful  : out Boolean) return String
   is
      use type Thin.Zstd_uint64;
      use type Thin.IC.size_t;

      dcmp_bytes  : Thin.IC.size_t;

      src         : aliased Thin.IC.char_array := convert (source_data);
      srcSize     : constant Thin.IC.size_t := Thin.IC.size_t (source_data'Length);
      src_pointer : Thin.ICS.chars_ptr := Thin.ICS.To_Chars_Ptr (src'Unchecked_Access);
      full_size   : constant Thin.Zstd_uint64 :=
                    Thin.ZSTD_getDecompressedSize (src     => src_pointer,
                                                   srcSize => srcSize);
   begin
      if full_size = 0 then
         successful := False;
         return "ERROR: Original size unknown";
      end if;

      if full_size > Thin.Zstd_uint64 (Thin.IC.size_t'Last) then
         successful := False;
         return "ERROR: Hit 4Gb limit imposed by this architecture";
      end if;

      declare
         dstCapacity : constant Thin.IC.size_t := Thin.IC.size_t (full_size);
         dst         : aliased Thin.IC.char_array := (1 .. dstCapacity => Thin.IC.nul);
         dst_pointer : Thin.ICS.chars_ptr := Thin.ICS.To_Chars_Ptr (dst'Unchecked_Access);
      begin
         dcmp_bytes := Thin.ZSTD_decompress (dst            => dst_pointer,
                                             dstCapacity    => dstCapacity,
                                             src            => src_pointer,
                                             compressedSize => srcSize);
         successful := (dcmp_bytes = dstCapacity);
         if successful then
            return convert (dst);
         else
            return Thin.ICS.Value (Thin.ZSTD_getErrorName (code => dcmp_bytes));
         end if;
      end;
   end Decompress;


   ---------------------
   --  Compress_File  --
   ---------------------
   function Compress_File
     (source_file : String;
      output_file : String;
      source_size : out File_Size;
      output_size : out File_Size;
      error_msg   : out String;
      quality     : Compression_Level := Fastest_Compression) return Boolean
   is
   begin
      source_size := 0;
      output_size := 0;
      error_msg := "TBD";
      return False;
   end Compress_File;


   -----------------------
   --  Decompress_File  --
   -----------------------
   function Decompress_File
     (source_file : String;
      output_file : String;
      source_size : out File_Size;
      output_size : out File_Size;
      error_msg   : out String) return Boolean
   is
   begin
      source_size := 0;
      output_size := 0;
      error_msg := "TBD";
      return False;
   end Decompress_File;

end Zstandard.Functions;
