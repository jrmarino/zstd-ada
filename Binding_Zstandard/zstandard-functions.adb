--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Direct_IO;
with Ada.Text_IO;

package body Zstandard.Functions is

   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;

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
   --  file_contents  --
   ---------------------
   function file_contents (filename : String;
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
   end file_contents;


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
      output_Handle : TIO.File_Type;
   begin
      source_size := 0;
      output_size := 0;
      error_msg   := "";
      if not DIR.Exists (source_file) then
         error_msg := "ERROR: Source file does not exist";
         return False;
      end if;

      begin
         TIO.Create (File => output_Handle,
                     Mode => TIO.Out_File,
                     Name => output_file);
      exception
         when others =>
            error_msg := "ERROR: Failed to create output file";
            return False;
      end;

      source_size := File_Size (DIR.Size (source_file));

      declare
         good_dump : Boolean;
         payload : constant String := file_contents (filename => source_file,
                                                     filesize => Natural (source_size),
                                                     nominal  => good_dump);
      begin
         if not good_dump then
            TIO.Close (output_Handle);
            error_msg := "ERROR: Failed to read source file";
            return False;
         end if;

         declare
            good_compress : Boolean;
            compact : constant String := Compress (source_data => payload,
                                                   successful  => good_compress,
                                                   quality     => quality);
         begin
            if not good_compress then
               TIO.Close (output_Handle);
               error_msg := "ERROR: Failed to compress data after reading source file";
               return False;
            end if;

            begin
               TIO.Put (File => output_Handle, Item => compact);
               TIO.Close (output_Handle);
               output_size := File_Size (compact'Length);
               return True;
            exception
               when others =>
                  TIO.Close (output_Handle);
                  error_msg := "ERROR: Failed to write to open output file";
                  return False;
            end;
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
      error_msg   : out String) return Boolean
   is
   begin
      source_size := 0;
      output_size := 0;
      error_msg := "TBD";
      return False;
   end Decompress_File;

end Zstandard.Functions;
