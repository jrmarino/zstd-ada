--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Zstandard.Functions.Streaming_Decompression is

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize
     (mechanism       : out Decompressor;
      input_stream    : not null access STR.Root_Stream_Type'Class)
   is
      use type Thin.ZSTD_DStream_ptr;
      initResult : Thin.IC.size_t;
   begin
      mechanism.source_stream := input_stream;
      mechanism.zstd_stream   := Thin.ZSTD_createDStream;

      if mechanism.zstd_stream = Thin.Null_DStream_pointer then
         raise streaming_decompression_initialization with "ZSTD_createDStream failure";
      end if;

      initResult := Thin.ZSTD_initDStream (zds => mechanism.zstd_stream);
      if Natural (Thin.ZSTD_isError (code => initResult)) /= 0 then
         raise streaming_decompression_initialization with "ZSTD_initDStream failure";
      end if;
   end Initialize;


   -----------------------
   --  Decompress_Data  --
   -----------------------
   procedure Decompress_Data
     (mechanism    :     Decompressor;
      complete     : out Boolean;
      output_data  : out Output_Data_Container;
      last_element : out Natural)
   is
      use type Thin.ZSTD_DStream_ptr;

      sin_last  : STR.Stream_Element_Offset := STR.Stream_Element_Offset (Recommended_Chunk_Size);
      Last      : STR.Stream_Element_Offset;

      data_in   : aliased Thin.IC.char_array := (1 .. Recommended_Chunk_Size => Thin.IC.nul);
      data_out  : aliased Thin.IC.char_array := (1 .. Output_container_size  => Thin.IC.nul);
      data_sin  : STR.Stream_Element_Array (1 .. sin_last);
      index     : Thin.IC.size_t := data_in'First;
      size_hint : Thin.IC.size_t;
      inbuffer  : aliased Thin.ZSTD_inBuffer_s :=
                          (src  => Thin.ICS.To_Chars_Ptr (data_in'Unchecked_Access),
                           size => Recommended_Chunk_Size,
                           pos  => 0);
      outbuffer : aliased Thin.ZSTD_outBuffer_s :=
                          (dst  => Thin.ICS.To_Chars_Ptr (data_out'Unchecked_Access),
                           size => Output_container_size,
                           pos  => 0);
   begin
      if mechanism.zstd_stream = Thin.Null_DStream_pointer then
         raise streaming_decompression_error with "Run initialize procedure first";
      end if;

      mechanism.source_stream.Read (Item => data_sin, Last => Last);

      if Natural (Last) = 0 then
         last_element := 0;
         complete := True;
         return;
      end if;

      data_in := convert_to_char_array (data_sin (1 .. Last), Recommended_Chunk_Size);
      complete := (Natural (Last) /= Natural (Recommended_Chunk_Size));

      size_hint := Thin.ZSTD_decompressStream (zds    => mechanism.zstd_stream,
                                               output => outbuffer'Unchecked_Access,
                                               input  => inbuffer'Unchecked_Access);

      last_element := Natural (outbuffer.pos);
      output_data (1 .. STR.Stream_Element_Offset (last_element)) :=
        convert_to_stream_array (data_out, outbuffer.pos);

   end Decompress_Data;


   -------------------------------
   --  convert_to_stream_array  --
   -------------------------------
   function convert_to_stream_array
     (char_data         : Thin.IC.char_array;
      number_characters : Thin.IC.size_t) return STR.Stream_Element_Array
   is
      product : STR.Stream_Element_Array (1 .. STR.Stream_Element_Offset (number_characters));
      dondx   : Thin.IC.size_t;
   begin
      for z in product'Range loop
         dondx := Thin.IC.size_t (z);
         product (z) := STR.Stream_Element (Character'Pos (Thin.IC.To_Ada (char_data (dondx))));
      end loop;
      return product;
   end convert_to_stream_array;


   -----------------------------
   --  convert_to_char_array  --
   -----------------------------
   function convert_to_char_array
     (stream_data       : STR.Stream_Element_Array;
      output_array_size : Thin.IC.size_t) return Thin.IC.char_array
   is
      use type Thin.IC.size_t;

      product : Thin.IC.char_array := (1 .. output_array_size => Thin.IC.nul);
      dondx   : Thin.IC.size_t := 1;
   begin
      for z in stream_data'Range loop
         product (dondx) := Thin.IC.To_C (Character'Val (stream_data (z)));
         dondx := dondx + 1;
      end loop;
      return product;
   end convert_to_char_array;

end Zstandard.Functions.Streaming_Decompression;
