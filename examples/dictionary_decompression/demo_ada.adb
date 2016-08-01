with Zstandard.Functions; use Zstandard.Functions;
with Ada.Command_line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

procedure Demo_Ada is
begin
   if Argument_Count < 2 then
      Put_Line ("Zstandard version: " & Zstd_Version);
      Put_Line ("usage:");
      Put_Line (Command_Name & " <file#1> [additional files] <dictionary>");
      return;
   end if;

   for z in 1 .. Argument_Count loop
      if not Exists (Argument (z)) then
         Put_Line ("File '" & Argument (z) & "' does not exist, aborting.");
         return;
      end if;
   end loop;
   
   for z in 1 .. Argument_Count - 1 loop
      declare
         path2file : String renames Argument (z);
         namelen   : constant Natural := path2file'Length;
         nlast     : constant Natural := path2file'Last;
      begin
         if namelen < 5 or else
           path2file (nlast - 3 .. nlast) /= ".zst"
         then
            Put_Line ("File name" & z'Img & " doesn't have the .zst extension, aborting.");
            return;
         end if;
      end;
   end loop;

   declare
      dictionary : String renames Argument (Argument_Count);
      srcsize   : Zstandard.Functions.File_Size;
      dstsize   : Zstandard.Functions.File_Size;
      gooddecmp : Boolean;
      gooddict  : Boolean;
      digest    : Decompression_Dictionary;
   begin
      digest := Create_Decompression_Dictionary_From_File (sample_file => dictionary,
                                                           successful  => gooddict);
      if not gooddict then
         Put_Line ("Failed to load dictionary");
         return;
      end if;

      for z in 1 .. Argument_Count - 1 loop
         declare
            orig_file : String renames Argument (z);
            nlast     : constant Natural := orig_file'Last;
            dest_file : String := orig_file (orig_file'First .. nlast - 4);
            error_msg : String := Decompress_File (source_file => orig_file,
                                                   output_file => dest_file,
                                                   digest      => digest,
                                                   source_size => srcsize,
                                                   output_size => dstsize,
                                                   successful  => gooddecmp);
         begin
            if gooddecmp then
               Put_Line ("");
               Put_Line ("   original file size:" & srcsize'Img);
               Put_Line (" compressed file size:" & dstsize'Img);
               Put_Line ("             new file: " & dest_file);
            else
               Put_Line (error_msg);
            end if;
         end;
      end loop;
      Destroy_Decompression_Dictionary (digest);
   end;

end Demo_Ada;
