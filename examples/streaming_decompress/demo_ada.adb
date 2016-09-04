with Zstandard.Functions.Streaming_Decompression; use Zstandard.Functions;
with Ada.Command_line;      use Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Streams.Stream_IO; use Ada.Streams;

procedure Demo_Ada is
begin
   if Argument_Count /= 1 then
      Put_Line ("Zstandard version: " & Zstd_Version);
      Put_Line ("usage:");
      Put_Line (Command_Name & " <path/to/file.zst>");
      return;
   end if;

   declare
      path2file : String renames Argument (1);
      namelen   : constant Natural := path2file'Length;
      nlast     : constant Natural := path2file'Last;
   begin
      if not Exists (path2file) then
         Put_Line ("File '" & path2file & "' does not exist, aborting.");
         return;
      else
         if namelen < 5 or else
           path2file (nlast - 3 .. nlast) /= ".zst"
         then
            Put_Line ("File name doesn't have the .zst extension, aborting.");
            return;
         end if;
      end if;
   end;

   declare
      type Real4 is digits 4;
      path2file : String renames Argument (1);
      src_file  : Stream_IO.File_Type;
      mech      : Streaming_Decompression.Decompressor;
      complete  : Boolean := False;
      plaintext : Streaming_Decompression.Output_Data_Container;
      last_one  : Natural;
      sum       : Natural := 0;
   begin
      Stream_IO.Open (File => src_file,
                      Mode => Stream_IO.In_File,
                      Name => path2file);

      mech.Initialize (input_stream => Stream_IO.Stream (src_file));

      loop
         exit when complete;
         mech.Decompress_Data (complete     => complete,
                               output_data  => plaintext,
                               last_element => last_one);
         if last_one > 0 then
            declare
               message : String (1 .. last_one);
            begin
               for z in message'Range loop
                  message (z) := Character'val (plaintext (Stream_Element_Offset (z)));
               end loop;
               Put (message);
               sum := sum + last_one;
            end;
         end if;
      end loop;

       Stream_IO.Close (src_file);

      Put_Line ("total length:" & sum'Img);

   exception
      when Error : others =>
         Put_Line (Exception_Information(Error));
         if Stream_IO.Is_Open (src_file) then
            Stream_IO.Close (src_file);
         end if;
   end;

end Demo_Ada;
