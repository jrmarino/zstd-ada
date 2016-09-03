with Zstandard.Functions.Streaming_Compression; use Zstandard.Functions;
with Ada.Command_line;      use Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Streams.Stream_IO; use Ada.Streams;

procedure Demo_Ada
is
   level   : Compression_Level := Fastest_Compression;
   BUFSIZE : constant Stream_Element_Offset := 1024;
begin
   if Argument_Count = 0 or else Argument_Count > 2 then
      Put_Line ("Zstandard version: " & Zstd_Version);
      Put_Line ("usage:");
      Put_Line (Command_Name & " <path/to/file> [compression level=1]");
      return;
   end if;

   if not Exists (Argument (1)) then
      Put_Line ("File '" & Argument (1) & "' does not exist, aborting.");
      return;
   end if;

   if Argument_Count = 2 then
      declare
         complevel : String renames Argument (2);
         myint : Integer;
      begin
         myint := Integer'Value (complevel);
         if myint >= 1 and then myint <= 22 then
            level := Compression_Level (myint);
         end if;
      exception
         when others => null;
      end;
   end if;

   declare
      path2file : String renames Argument (1);
      compfile  : String := path2file & ".zst";
      srcsize   : Natural := 0;
      dstsize   : Natural;
      srcbuffer : Stream_Element_Array (1 .. BUFSIZE);
      src_file  : Stream_IO.File_Type;
      dst_file  : Stream_IO.File_Type;
      Last      : Stream_Element_Offset;

      units     : Natural;
      tenths    : Natural;
      tenthstr  : String (1 .. 2);
      mech      : Streaming_Compression.Compressor;
   begin
      Stream_IO.Open (File => src_file,
                      Mode => Stream_IO.In_File,
                      Name => path2file);
      Stream_IO.Create (File => dst_file,
                        Mode => Stream_IO.Out_File,
                        Name => compfile);

      mech.Initialize (output_stream => Stream_IO.Stream (dst_file),
                       quality       => level);

      loop
         exit when Stream_IO.End_Of_File (src_file);
         Stream_IO.Read (File => src_file,
                         Item => srcbuffer,
                         Last => Last);
         mech.Compress_Data (srcbuffer (1 .. Last));
         srcsize := srcsize + Natural (Last);
      end loop;
      mech.Finalize_Compression_Frame;
      Stream_IO.Close (src_file);
      Stream_IO.Close (dst_file);

      dstsize := Natural (Size (compfile));

      units := Natural (100 * dstsize / srcsize);
      tenths := (Natural (10 * dstsize / srcsize)) mod 10;
      tenthstr := tenths'Img;
      Put_Line ("   original file size:" & srcsize'Img);
      Put_Line (" compressed file size:" & dstsize'Img);
      Put_Line ("percentage compressed:" & units'Img & "." & tenthstr (2 ..2));
      Put_Line ("             new file: " & compfile);
   exception
      when Error : others =>
         Put_Line (Exception_Information(Error));
         if Stream_IO.Is_Open (src_file) then
            Stream_IO.Close (src_file);
         end if;
         if Stream_IO.Is_Open (dst_file) then
            Stream_IO.Close (dst_file);
         end if;
   end;

end Demo_Ada;
