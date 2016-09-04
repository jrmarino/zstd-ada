--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Streams;

package Zstandard.Functions.Streaming_Decompression is

   package STR renames Ada.Streams;

   Output_container_size : constant Thin.IC.size_t := Thin.ZSTD_DStreamOutSize;

   subtype Output_Data_Container is
     STR.Stream_Element_Array (1 .. STR.Stream_Element_Offset (Output_container_size));

   type Decompressor is tagged private;

   --  This is the initialization procedure.
   --  The input stream and output buffer capacity are externally provided
   procedure Initialize
     (mechanism       : out Decompressor;
      input_stream    : not null access STR.Root_Stream_Type'Class);

   --  Decompress data as each input chunk is received
   --  if "complete" then the decompression is complete (don't call procedure any more)
   --  The "last_element" is the end of the container range (e.g. 1 .. last_element)
   procedure Decompress_Data
     (mechanism    :     Decompressor;
      complete     : out Boolean;
      output_data  : out Output_Data_Container;
      last_element : out Natural);

   streaming_decompression_initialization : exception;
   streaming_decompression_error          : exception;

private

   Recommended_Chunk_Size : constant Thin.IC.size_t := Thin.ZSTD_DStreamInSize;

   type Decompressor is tagged
      record
         source_stream    : access STR.Root_Stream_Type'Class;
         zstd_stream      : Thin.ZSTD_DStream_ptr := Thin.Null_DStream_pointer;
      end record;

   function convert_to_stream_array
     (char_data         : Thin.IC.char_array;
      number_characters : Thin.IC.size_t) return STR.Stream_Element_Array;

   function convert_to_char_array
     (stream_data       : STR.Stream_Element_Array;
      output_array_size : Thin.IC.size_t) return Thin.IC.char_array;

end Zstandard.Functions.Streaming_Decompression;
