--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Zstandard.Functions.Streaming_Compression is

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize
     (mechanism     : out Compressor;
      output_stream : not null access STR.Root_Stream_Type'Class;
      quality       : Compression_Level := Default_Compression)
   is
      use type Thin.ZSTD_CStream_ptr;
      initResult : Thin.IC.size_t;
   begin
      mechanism.target_stream := output_stream;
      mechanism.zstd_stream := Thin.ZSTD_createCStream;
      if mechanism.zstd_stream = Thin.Null_CStream_pointer then
         raise streaming_compression_initialization with "ZSTD_createCStream failure";
      end if;
      initResult := Thin.ZSTD_initCStream (zcs              => mechanism.zstd_stream,
                                           compressionLevel => Thin.IC.int (quality));
      if Natural (Thin.ZSTD_isError (code => initResult)) /= 0 then
         raise streaming_compression_initialization with "ZSTD_initCStream failure";
      end if;
   end Initialize;


   ---------------------
   --  Compress_Data  --
   ---------------------
   procedure Compress_Data (mechanism : out Compressor;
                            data      : STR.Stream_Element_Array)
   is
      use type Thin.IC.size_t;
      use type Thin.ZSTD_CStream_ptr;

      data_in   : aliased Thin.IC.char_array := (1 .. data'Length => Thin.IC.nul);
      data_out  : aliased Thin.IC.char_array := (1 .. Buffer_Output_Size => Thin.IC.nul);
      index     : Thin.IC.size_t := data_in'First;
      inbuffer  : aliased Thin.ZSTD_inBuffer_s :=
                          (src  => Thin.ICS.To_Chars_Ptr (data_in'Unchecked_Access),
                           size => data'Length,
                           pos  => 0);
      outbuffer : aliased Thin.ZSTD_outBuffer_s :=
                          (dst  => Thin.ICS.To_Chars_Ptr (data_out'Unchecked_Access),
                           size => Buffer_Output_Size,
                           pos  => 0);
   begin
      if mechanism.zstd_stream = Thin.Null_CStream_pointer then
         raise streaming_compression_error with "Run initialize procedure first";
      end if;
      for z in data'Range loop
         data_in (index) := Thin.IC.To_C (Character'Val (data (z)));
         index := index + 1;
      end loop;
      loop
         exit when inbuffer.pos >= inbuffer.size;
         outbuffer.pos := 0;
         mechanism.data_size_hint :=
           Thin.ZSTD_compressStream (zcs    => mechanism.zstd_stream,
                                     output => outbuffer'Unchecked_Access,
                                     input  => inbuffer'Unchecked_Access);
         mechanism.target_stream.Write (convert_to_stream_array (data_out, outbuffer.pos));
      end loop;
   end Compress_Data;


   ----------------------------------
   --  Finalize_Compression_Frame  --
   ----------------------------------
   procedure Finalize_Compression_Frame (mechanism : Compressor)
   is
      data_out  : aliased Thin.IC.char_array := (1 .. Buffer_Output_Size => Thin.IC.nul);
      outbuffer : aliased Thin.ZSTD_outBuffer_s :=
                          (dst  => Thin.ICS.To_Chars_Ptr (data_out'Unchecked_Access),
                           size => Buffer_Output_Size,
                           pos  => 0);
      remaining : Thin.IC.size_t;
   begin
      remaining := Thin.ZSTD_endStream (zcs    => mechanism.zstd_stream,
                                        output => outbuffer'Unchecked_Access);
      if Natural (remaining) > 0 then
         raise streaming_compression_finalization with "not fully flushed";
      end if;
      mechanism.target_stream.Write (convert_to_stream_array (data_out, outbuffer.pos));
   end Finalize_Compression_Frame;


   ------------------------------------
   --  Recommended_Data_Buffer_Size  --
   ------------------------------------
   function Next_Data_Size_Recommendation (mechanism : Compressor) return Positive is
   begin
      return Positive (mechanism.data_size_hint);
   end Next_Data_Size_Recommendation;


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


end Zstandard.Functions.Streaming_Compression;
