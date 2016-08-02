--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Direct_IO;

package body Zstandard.Functions is

   package DIR renames Ada.Directories;

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
      quality     : Compression_Level := Default_Compression) return String
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
         return Warn_orig_size_fail;
      end if;

      if full_size > Thin.Zstd_uint64 (Thin.IC.size_t'Last) then
         successful := False;
         return Warn_way_too_big;
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
   --  File_Contents  --
   ---------------------
   function File_Contents (filename : String;
                           filesize : Natural;
                           nominal  : out Boolean) return String
   is
      subtype File_String    is String (1 .. filesize);
      package File_String_IO is new Ada.Direct_IO (File_String);
      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      nominal := False;
      File_String_IO.Open (File => File,
                           Mode => File_String_IO.In_File,
                           Name => filename);
      File_String_IO.Read (File => File,
                           Item => Contents);
      File_String_IO.Close (File);
      nominal := True;
      return Contents;
   exception
      when others =>
         if File_String_IO.Is_Open (File) then
            File_String_IO.Close (File);
         end if;
         return "";
   end File_Contents;


   -------------------------
   --  write_entire_file  --
   -------------------------
   function Write_Entire_File (filename : String; contents : String) return Boolean
   is
      new_file_size : constant Natural := contents'Length;

      subtype File_String    is String (1 .. new_file_size);
      package File_String_IO is new Ada.Direct_IO (File_String);
      output_Handle : File_String_IO.File_Type;
   begin
      File_String_IO.Create (File => output_Handle,
                             Mode => File_String_IO.Out_File,
                             Name => filename);
      File_String_IO.Write (output_Handle, File_String (contents));
      File_String_IO.Close (output_Handle);
      return True;
   exception
      when others =>
         if File_String_IO.Is_Open (output_Handle) then
            File_String_IO.Close (output_Handle);
         end if;
         return False;
   end Write_Entire_File;


   ---------------------
   --  Compress_File  --
   ---------------------
   function Compress_File
     (source_file : String;
      output_file : String;
      source_size : out File_Size;
      output_size : out File_Size;
      successful  : out Boolean;
      quality     : Compression_Level := Default_Compression) return String is
   begin
      source_size := 0;
      output_size := 0;
      successful  := False;
      if not DIR.Exists (source_file) then
         return Warn_src_file_DNE;
      end if;

      source_size := File_Size (DIR.Size (source_file));

      declare
         good_dump : Boolean;
         payload : constant String := File_Contents (filename => source_file,
                                                     filesize => Natural (source_size),
                                                     nominal  => good_dump);
      begin
         if not good_dump then
            return Warn_src_read_fail;
         end if;

         declare
            good_compress : Boolean;
            compact : constant String := Compress (source_data => payload,
                                                   successful  => good_compress,
                                                   quality     => quality);
         begin
            if not good_compress then
               return Warn_compress_fail & " (" & compact & ")";
            end if;

            if Write_Entire_File (filename => output_file, contents => compact) then
               output_size := File_Size (compact'Length);
               successful := True;
               return "";
            else
               return Warn_dst_write_fail;
            end if;
         end;
      end;
   end Compress_File;


   -----------------------
   --  Decompress_File  --
   -----------------------
   function Decompress_File
     (source_file : String;
      output_file : String;
      source_size : out File_Size;
      output_size : out File_Size;
      successful  : out Boolean) return String is
   begin
      source_size := 0;
      output_size := 0;
      successful  := False;
      if not DIR.Exists (source_file) then
         return Warn_src_file_DNE;
      end if;

      source_size := File_Size (DIR.Size (source_file));

      declare
         good_dump : Boolean;
         payload : constant String := File_Contents (filename => source_file,
                                                     filesize => Natural (source_size),
                                                     nominal  => good_dump);
      begin
         if not good_dump then
            return Warn_src_read_fail;
         end if;

         declare
            good_expansion : Boolean;
            fulldata : constant String := Decompress (source_data => payload,
                                                      successful  => good_expansion);
         begin
            if not good_expansion then
               return Warn_decompress_fail & " (" & fulldata & ")";
            end if;

            if Write_Entire_File (filename => output_file, contents => fulldata) then
               output_size := File_Size (fulldata'Length);
               successful := True;
               return "";
            else
               return Warn_dst_write_fail;
            end if;
         end;
      end;
   end Decompress_File;


   -------------------------------------
   --  Create_Compression_Dictionary  --
   -------------------------------------
   function Create_Compression_Dictionary
     (sample  : String;
      quality : Compression_Level := Default_Compression) return Compression_Dictionary
   is
      dict         : aliased Thin.IC.char_array := convert (sample);
      dictSize     : constant Thin.IC.size_t := Thin.IC.size_t (sample'Length);
      dict_pointer : Thin.ICS.chars_ptr := Thin.ICS.To_Chars_Ptr (dict'Unchecked_Access);
      level        : constant Thin.IC.int := Thin.IC.int (quality);
   begin
      return Thin.ZSTD_createCDict (dict             => dict_pointer,
                                    dictSize         => dictSize,
                                    compressionLevel => level);
   end Create_Compression_Dictionary;


   -----------------------------------------------
   --  Create_Compression_Dictionary_From_File  --
   -----------------------------------------------
   function Create_Compression_Dictionary_From_File
     (sample_file : String;
      successful  : out Boolean;
      quality     : Compression_Level := Default_Compression) return Compression_Dictionary
   is
      sample_file_size : Natural;
      new_dictionary   : Compression_Dictionary;
   begin
      successful := False;
      if not DIR.Exists (sample_file) then
         return Thin.Null_CDict_pointer;
      end if;

      sample_file_size := Natural (DIR.Size (sample_file));

      declare
         use type Thin.ZSTD_CDict_ptr;
         good_dump : Boolean;
         payload : constant String := File_Contents (filename => sample_file,
                                                     filesize => sample_file_size,
                                                     nominal  => good_dump);
      begin
         if not good_dump then
            return Thin.Null_CDict_pointer;
         end if;
         new_dictionary := Create_Compression_Dictionary (payload, quality);
         successful := (new_dictionary /= Thin.Null_CDict_pointer);
         return new_dictionary;
      end;
   end Create_Compression_Dictionary_From_File;


   --------------------------------------
   --  Destroy_Compression_Dictionary  --
   --------------------------------------
   procedure Destroy_Compression_Dictionary (digest : Compression_Dictionary)
   is
      res : Thin.IC.size_t;
   begin
      res := Thin.ZSTD_freeCDict (CDict => digest);
   end Destroy_Compression_Dictionary;


   ---------------------------------------
   --  Create_Decompression_Dictionary  --
   ---------------------------------------
   function Create_Decompression_Dictionary (sample : String) return Decompression_Dictionary
   is
      dict         : aliased Thin.IC.char_array := convert (sample);
      dictSize     : constant Thin.IC.size_t := Thin.IC.size_t (sample'Length);
      dict_pointer : Thin.ICS.chars_ptr := Thin.ICS.To_Chars_Ptr (dict'Unchecked_Access);
   begin
      return Thin.ZSTD_createDDict (dict => dict_pointer, dictSize => dictSize);
   end Create_Decompression_Dictionary;


   -------------------------------------------------
   --  Create_Decompression_Dictionary_From_File  --
   -------------------------------------------------
   function Create_Decompression_Dictionary_From_File (sample_file : String;
                                                       successful  : out Boolean)
                                                       return Decompression_Dictionary
   is
      sample_file_size : Natural;
      new_dictionary   : Decompression_Dictionary;
   begin
      successful := False;
      if not DIR.Exists (sample_file) then
         return Thin.Null_DDict_pointer;
      end if;

      sample_file_size := Natural (DIR.Size (sample_file));

      declare
         use type Thin.ZSTD_DDict_ptr;
         good_dump : Boolean;
         payload : constant String := File_Contents (filename => sample_file,
                                                     filesize => sample_file_size,
                                                     nominal  => good_dump);
      begin
         if not good_dump then
            return Thin.Null_DDict_pointer;
         end if;
         new_dictionary := Create_Decompression_Dictionary (payload);
         successful := (new_dictionary /= Thin.Null_DDict_pointer);
         return new_dictionary;
      end;
   end Create_Decompression_Dictionary_From_File;


   ----------------------------------------
   --  Destroy_Decompression_Dictionary  --
   ----------------------------------------
   procedure Destroy_Decompression_Dictionary (digest : Decompression_Dictionary)
   is
      res : Thin.IC.size_t;
   begin
      res := Thin.ZSTD_freeDDict (ddict => digest);
   end Destroy_Decompression_Dictionary;


   -----------------------------
   --  Compress (dictionary)  --
   -----------------------------
   function Compress
     (source_data : String;
      digest      : Compression_Dictionary;
      successful  : out Boolean) return String
   is
      comp_bytes  : Thin.IC.size_t;
      is_error    : Thin.IC.unsigned;

      src         : aliased Thin.IC.char_array := convert (source_data);
      srcSize     : constant Thin.IC.size_t := Thin.IC.size_t (source_data'Length);
      dstCapacity : constant Thin.IC.size_t := Thin.ZSTD_compressBound (srcSize);
      dst         : aliased Thin.IC.char_array := (1 .. dstCapacity => Thin.IC.nul);

      dst_pointer : Thin.ICS.chars_ptr := Thin.ICS.To_Chars_Ptr (dst'Unchecked_Access);
      src_pointer : Thin.ICS.chars_ptr := Thin.ICS.To_Chars_Ptr (src'Unchecked_Access);

      cctx        : Thin.ZSTD_CCtx_ptr;
      freeres     : Thin.IC.size_t;
   begin
      cctx := Thin.ZSTD_createCCtx;
      comp_bytes := Thin.ZSTD_compress_usingCDict (ctx         => cctx,
                                                   dst         => dst_pointer,
                                                   dstCapacity => dstCapacity,
                                                   src         => src_pointer,
                                                   srcSize     => srcSize,
                                                   CDict       => digest);
      freeres := Thin.ZSTD_freeCCtx (cctx);
      is_error := Thin.ZSTD_isError (code => comp_bytes);
      successful := (Natural (is_error) = 0);
      if successful then
         return convert (dst (1 .. comp_bytes));
      else
         return Thin.ICS.Value (Thin.ZSTD_getErrorName (code => comp_bytes));
      end if;
   end Compress;


   ----------------------------------
   --  Compress_File (dictionary)  --
   ----------------------------------
   function Compress_File
     (source_file : String;
      output_file : String;
      digest      : Compression_Dictionary;
      source_size : out File_Size;
      output_size : out File_Size;
      successful  : out Boolean) return String is
   begin
      source_size := 0;
      output_size := 0;
      successful  := False;
      if not DIR.Exists (source_file) then
         return Warn_src_file_DNE;
      end if;

      source_size := File_Size (DIR.Size (source_file));

      declare
         good_dump : Boolean;
         payload : constant String := File_Contents (filename => source_file,
                                                     filesize => Natural (source_size),
                                                     nominal  => good_dump);
      begin
         if not good_dump then
            return Warn_src_read_fail;
         end if;

         declare
            good_compress : Boolean;
            compact : constant String := Compress (source_data => payload,
                                                   digest      => digest,
                                                   successful  => good_compress);
         begin
            if not good_compress then
               return Warn_compress_fail & " (" & compact & ")";
            end if;

            if Write_Entire_File (filename => output_file, contents => compact) then
               output_size := File_Size (compact'Length);
               successful := True;
               return "";
            else
               return Warn_dst_write_fail;
            end if;
         end;
      end;
   end Compress_File;


   -------------------------------
   --  Decompress (dictionary)  --
   -------------------------------
   function Decompress
     (source_data : String;
      digest      : Decompression_Dictionary;
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
      dctx        : Thin.ZSTD_DCtx_ptr;
      freeres     : Thin.IC.size_t;
   begin
      successful := False;
      if full_size = 0 then
         return Warn_orig_size_fail;
      end if;

      if full_size > Thin.Zstd_uint64 (Thin.IC.size_t'Last) then
         return Warn_way_too_big;
      end if;

      declare
         dstCapacity : constant Thin.IC.size_t := Thin.IC.size_t (full_size);
         dst         : aliased Thin.IC.char_array := (1 .. dstCapacity => Thin.IC.nul);
         dst_pointer : Thin.ICS.chars_ptr := Thin.ICS.To_Chars_Ptr (dst'Unchecked_Access);
      begin
         dctx := Thin.ZSTD_createDCtx;
         dcmp_bytes := Thin.ZSTD_decompress_usingDDict (dctx        => dctx,
                                                        dst         => dst_pointer,
                                                        dstCapacity => dstCapacity,
                                                        src         => src_pointer,
                                                        srcSize     => srcSize,
                                                        ddict       => digest);
         freeres := Thin.ZSTD_freeDCtx (dctx);
         successful := (dcmp_bytes = dstCapacity);
         if successful then
            return convert (dst);
         else
            return Thin.ICS.Value (Thin.ZSTD_getErrorName (code => dcmp_bytes));
         end if;
      end;
   end Decompress;


   ------------------------------------
   --  Decompress_File (dictionary)  --
   ------------------------------------
   function Decompress_File
     (source_file : String;
      output_file : String;
      digest      : Decompression_Dictionary;
      source_size : out File_Size;
      output_size : out File_Size;
      successful  : out Boolean) return String is
   begin
      source_size := 0;
      output_size := 0;
      successful  := False;
      if not DIR.Exists (source_file) then
         return Warn_src_file_DNE;
      end if;

      source_size := File_Size (DIR.Size (source_file));

      declare
         good_dump : Boolean;
         payload : constant String := File_Contents (filename => source_file,
                                                     filesize => Natural (source_size),
                                                     nominal  => good_dump);
      begin
         if not good_dump then
            return Warn_src_read_fail;
         end if;

         declare
            good_expansion : Boolean;
            fulldata : constant String := Decompress (source_data => payload,
                                                      digest      => digest,
                                                      successful  => good_expansion);
         begin
            if not good_expansion then
               return Warn_decompress_fail & " (" & fulldata & ")";
            end if;

            if Write_Entire_File (filename => output_file, contents => fulldata) then
               output_size := File_Size (fulldata'Length);
               successful := True;
               return "";
            else
               return Warn_dst_write_fail;
            end if;
         end;
      end;
   end Decompress_File;

end Zstandard.Functions;
