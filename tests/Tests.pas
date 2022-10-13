{
    Copyright (c) 2020 by Ryan Joseph

    TOML Parser Tests
}

{$mode objfpc}
{$H+}

program Tests;
uses
	TOML, SysUtils, BaseUnix, Classes, FPJSON, FGL;

function ScanDir(path: ansistring; fullPath: boolean = false): TStringList;
var
  handle: PDir;
  entry: PDirent;
  name: pchar;
begin
  result := nil;
  handle := fpOpenDir(path);
  if assigned(handle) then
    begin
      while true do
        begin
          entry := fpReadDir(handle);
          if assigned(entry) then
            begin
              name := pchar(@entry^.d_name[0]);
              if (name = '.') or (name = '..') then
                continue;
              if result = nil then
                result := TStringList.Create;
              if fullPath then
                result.Add(path+'/'+name)
              else
                result.Add(name);
            end
          else
            break;
        end;
      fpCloseDir(handle);
    end;
end;

function ReadFile(path: string): string;
var
  list: TStringList;
begin
  try
    list := TStringList.Create;
    list.LoadFromFile(path);
    result := list.Text;
  except
    on E:Exception do
      writeln(path+': ', E.Message);
  end;
  list.Free;
end;

procedure RunTests(dir: string; expectedFail: boolean; showJSON: boolean = false); 
var
  files: TStringList;
  name, ext, path: string;
  contents: string;
  doc: TTOMLDocument;
  json: TJSONData;
begin
  dir := ExpandFileName(dir);
  files := ScanDir(dir, true);
  for path in files do
    begin
      name := ExtractFileName(path);
      ext := ExtractFileExt(path);
      if ext = '.toml' then
        begin
          write(ExtractFileName(dir), '/', name);
          doc := nil;
          contents := ReadFile(path);
          try
            doc := GetTOML(contents);
            if expectedFail then
              begin
                WriteLn(' 🔴 Failed!');
                halt;
              end;
          except
            on E: Exception do
              begin
                if not expectedFail then
                  begin
                    WriteLn(' 🔴 ', E.Message);
                    halt;
                  end;
              end;
          end;
          if showJSON then
            begin
              json := doc.AsJSON;
              writeln(json.FormatJSON);
              json.Free;
            end;
          doc.Free;
          writeln(' ✓');
        end;
    end;
  writeln('🟢 All tests passed!');
end;

begin
  RunTests('./pass', false);
  RunTests('./fail', true);
end.