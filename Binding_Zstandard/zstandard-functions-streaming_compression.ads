--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Streams;

package Zstandard.Functions.Streaming_Compression is

   package STR renames Ada.Streams;

   type Compressor is tagged private;

   --  This is the initialization procedure.
   --  The output stream and compression level are externally provided
   procedure Initialize
     (mechanism     : out Compressor;
      output_stream : not null access STR.Root_Stream_Type'Class;
      quality       : Compression_Level := Default_Compression);

   --  Compress data as they are received
   procedure Compress_Data
     (mechanism : out Compressor;
      data      : STR.Stream_Element_Array);

   --  Finalize compression (flush)
   procedure Finalize_Compression_Frame (mechanism : Compressor);

   --  Recommended input buffer size (for Compress_Data).
   --  If called before "Compress_Data", it will be the standard ZSTD_CStreamInSize result
   --  After "Compress_Data" execution, it will be passed on the resultant hint
   function Next_Data_Size_Recommendation (mechanism : Compressor) return Positive;

   streaming_compression_initialization : exception;
   streaming_compression_error          : exception;
   streaming_compression_finalization   : exception;

private

   Buffer_Output_Size : constant Thin.IC.size_t := Thin.ZSTD_CStreamOutSize;

   type Compressor is tagged
      record
         target_stream  : access STR.Root_Stream_Type'Class;
         zstd_stream    : Thin.ZSTD_CStream_ptr := Thin.Null_CStream_pointer;
         data_size_hint : Thin.IC.size_t := Buffer_Output_Size;
      end record;

   function convert_to_stream_array
     (char_data         : Thin.IC.char_array;
      number_characters : Thin.IC.size_t) return STR.Stream_Element_Array;

end Zstandard.Functions.Streaming_Compression;
