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

   subtype Compression_Dictionary   is Thin.ZSTD_CDict_ptr;
   subtype Decompression_Dictionary is Thin.ZSTD_DDict_ptr;

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
   --  It returns a blank string if successful and "successful" is set to True.  Should the
   --  operation fail, "successful" is set to False and an error message is returned.
   --  For convenience, the size of the source and output files are also provided.
   function Compress_File
     (source_file : String;
      output_file : String;
      source_size : out File_Size;
      output_size : out File_Size;
      successful  : out Boolean;
      quality     : Compression_Level := Fastest_Compression) return String;


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
   --  It returns a blank string if successful and "successful" is set to True.  Should the
   --  operation fail, "successful" is set to False and an error message is returned.
   --  For convenience, the size of the source and output files are also provided.
   function Decompress_File
     (source_file : String;
      output_file : String;
      source_size : out File_Size;
      output_size : out File_Size;
      successful  : out Boolean) return String;

   --  Helper function to dump contents of a file into a string
   --  Potentially useful when desirable to have a compressed copy of the file in memory
   function File_Contents (filename : String;
                           filesize : Natural;
                           nominal  : out Boolean) return String;

   --  Helper function to create a new file with the exact value of "contents" string
   --  Potentially useful for writing compressed or plain text from memory
   function Write_Entire_File (filename : String; contents : String) return Boolean;


   ---------------------------
   --  Dictionary Handling  --
   ---------------------------

   --  Dictionaries are meant to be used to compress multiple similar files.  Before compression,
   --  the dictionary is created by giving it a sample of the types to be compressed.
   function Create_Compression_Dictionary
     (sample  : String;
      quality : Compression_Level := Fastest_Compression) return Compression_Dictionary;

   --  Similar to "Create_Compression_Dictionary" but the sample comes from a file
   --  Normally this is created by "zstd --train" command
   function Create_Compression_Dictionary_From_File
     (sample_file : String;
      successful  : out Boolean;
      quality     : Compression_Level := Fastest_Compression) return Compression_Dictionary;

   --  Release the compression dictionary after use.
   procedure Destroy_Compression_Dictionary (digest : Compression_Dictionary);

   --  Files compressed with dictionaries have to be decompressed using the same dictionaries
   --  created from the same sample data used to create the compression dictionaries.
   function Create_Decompression_Dictionary (sample : String) return Decompression_Dictionary;

   --  Similar to "Create_Decompression_Dictionary" but the sample comes from a file
   --  Normally this is created by "zstd --train" command
   function Create_Decompression_Dictionary_From_File
     (sample_file : String;
      successful  : out Boolean) return Decompression_Dictionary;

   --  Release the decompression dictionary after use.
   procedure Destroy_Decompression_Dictionary (digest : Decompression_Dictionary);


   --------------------------------------------
   --  Dictionary De/Compression Operations  --
   --------------------------------------------

   --  This function returns the dictionary-biased compressed version of "source_data".
   --  Should the operation fail, "successful" variable will be set to False and the resulting
   --  string will contain the related error message.  The compression level is pre-set during
   --  the creation of the "digest" dictionary.
   function Compress
     (source_data : String;
      digest      : Compression_Dictionary;
      successful  : out Boolean) return String;

   --  This function creates an output file that is a dictionary-biased compressed version of the
   --  "source_file". It returns a blank string if successful and "successful" is set to True.
   --  Should the operation fail, "successful" is set to False and an error message is returned.
   --  For convenience, the size of the source and output files are also provided.
   --  The compression level is pre-set during the creation of the "digest" dictionary.
   function Compress_File
     (source_file : String;
      output_file : String;
      digest      : Compression_Dictionary;
      source_size : out File_Size;
      output_size : out File_Size;
      successful  : out Boolean) return String;

   --  This function returns the decompressed version of "source_data" compressed using a
   --  dictionary.  Should the operation fail, the "successful" variable will be set to False
   --  and the resulting string will contain the related error message.
   function Decompress
     (source_data : String;
      digest      : Decompression_Dictionary;
      successful  : out Boolean) return String;

   --  This function creates an output file that is a decompressed version of the "source_file"
   --  that was compressed using a dictionary.  It returns a blank string if successful and
   --  "successful" is set to True.  Should the operation fail, "successful" is set to False and
   --  an error message is returned. For convenience, the size of the source and output files
   --  are also provided.
   function Decompress_File
     (source_file : String;
      output_file : String;
      digest      : Decompression_Dictionary;
      source_size : out File_Size;
      output_size : out File_Size;
      successful  : out Boolean) return String;

private

   Warn_src_file_DNE    : constant String := "ERROR: Source file does not exist";
   Warn_src_read_fail   : constant String := "ERROR: Failed to read source file";
   Warn_dst_write_fail  : constant String := "ERROR: Failed to write to open output file";
   Warn_compress_fail   : constant String := "ERROR: Failed to compress data after reading " &
                                                    "source file";
   Warn_decompress_fail : constant String := "ERROR: Failed to decompress data after reading " &
                                                    "source file";
   Warn_way_too_big     : constant String := "ERROR: Hit size limit imposed by this architecture";
   Warn_orig_size_fail  : constant String := "ERROR: Original size unknown";

   function convert (data : Thin.IC.char_array) return String;
   function convert (data : String) return Thin.IC.char_array;

end Zstandard.Functions;
