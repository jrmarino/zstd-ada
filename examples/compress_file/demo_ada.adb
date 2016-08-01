with Zstandard.Functions; use Zstandard.Functions;
with Ada.Command_line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

procedure Demo_Ada
is
   level     : Compression_Level := Fastest_Compression;
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
      type Real4 is digits 4;
      path2file : String renames Argument (1);
      compfile  : String := path2file & ".zst";
      srcsize   : Zstandard.Functions.File_Size;
      dstsize   : Zstandard.Functions.File_Size;
      goodcomp  : Boolean;
      percent   : Real4;
      error_msg : String := Compress_File (source_file => path2file,
                                           output_file => compfile,
                                           source_size => srcsize,
                                           output_size => dstsize,
                                           successful  => goodcomp,
                                           quality     => level);
   begin
      if goodcomp then
         percent := 100.0 * Real4 (dstsize) / Real4 (srcsize);
         Put_Line ("   original file size:" & srcsize'Img);
         Put_Line (" compressed file size:" & dstsize'Img);
         Put_Line ("percentage compressed:" & percent'Img);
         Put_Line ("             new file: " & compfile);
      else
         Put_Line (error_msg);
      end if;
   end;

end Demo_Ada;
