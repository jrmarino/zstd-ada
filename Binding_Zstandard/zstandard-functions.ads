--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Zstandard.Thin_Binding;

package Zstandard.Functions is

   package Thin renames Zstandard.Thin_Binding;

   ------------------
   --  Data Types  --
   ------------------

   type Compression_Level is range 1 .. 22;
   type File_Size         is mod 2 ** 64;


   -----------------
   --  Constants  --
   -----------------

   Fastest_Compression : constant Compression_Level := Compression_Level'First;
   Highest_Compression : constant Compression_Level := Compression_Level'Last;


   --------------------
   -- Identification --
   --------------------

   --  Returns the library version in the format "X.Y.Z", no leading zeros
   function Zstd_Version return String;


   ------------------
   -- Compression  --
   ------------------

   --  This function returns the compressed version of "source_data".  Should the operation fail,
   --  "successful" variable will be set to False and the resulting string will contain the
   --  related error message.
   function Compress
     (source_data : String;
      successful  : out Boolean;
      quality     : Compression_Level := Fastest_Compression) return String;

   --  This function creates an output file that is a compressed version of the "source_file".
   --  It returns True if successful and the "error_msg" is blank in that case.  Should the
   --  operation fail, result is False and the "error_msg" describes the problem.
   --  For convenience, the size of the source and output files are also provided.
   function Compress_File
     (source_file : String;
      output_file : String;
      source_size : out File_Size;
      output_size : out File_Size;
      error_msg   : out String;
      quality     : Compression_Level := Fastest_Compression) return Boolean;


   --------------------
   -- Decompression  --
   --------------------

   --  This function returns the decompressed version of "source_data".  Should the operation fail,
   --  "successful" variable will be set to False and the resulting string will contain the
   --  related error message.
   function Decompress
     (source_data : String;
      successful  : out Boolean) return String;

   --  This function creates an output file that is a decompressed version of the "source_file".
   --  It returns True if successful and the "error_msg" is blank in that case.  Should the
   --  operation fail, result is False and the "error_msg" describes the problem.
   --  For convenience, the size of the source and output files are also provided.
   function Decompress_File
     (source_file : String;
      output_file : String;
      source_size : out File_Size;
      output_size : out File_Size;
      error_msg   : out String) return Boolean;

private

   function convert (data : Thin.IC.char_array) return String;
   function convert (data : String) return Thin.IC.char_array;

end Zstandard.Functions;
