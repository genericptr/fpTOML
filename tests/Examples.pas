{
    Copyright (c) 2020 by Ryan Joseph

    TOML Parser Examples
}

{$mode objfpc}
{$H+}

program Examples;
uses
	TOML, SysUtils, BaseUnix, Classes, FPJSON, FGL;

function ReadFile(path: string): string;
var
  list: TStringList;
begin
  path := ExpandFileName(path);
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

procedure TestErrors;
begin
  GetTOML(ReadFile('./tests/fail/f15.toml'));
end;

procedure TestDates;
var
  doc: TTOMLDocument;
  date: TTOMLDate;
begin
  doc := GetTOML(ReadFile('./tests/pass/t0.toml'));
  date := doc['owner']['dob'] as TTOMLDate;
  writeln('ToString: ', date.ToString);
  writeln('DateTime: ', FloatToStr(date.AsDateTime));
  writeln('DateToStr: ', DateToStr(date.AsDateTime));
end;

procedure TestJSON(test: string);
var
  doc: TTOMLDocument;
begin
  doc := GetTOML(ReadFile('./tests/pass/'+test+'.toml'));
  writeln(doc.AsJSON.FormatJSON);
  doc.Free;
end;

procedure TestAccess;
var
  doc: TTOMLDocument;
  value: TTOMLData;
begin
  doc := GetTOML(ReadFile('./tests/pass/t0.toml'));
  value := doc['database']['ports'][0];
  writeln('port #0: ', string(value));

  writeln('ports:');
  for value in doc['database']['ports'] do
    writeln('  - ', string(value));
  doc.Free;
end;

{
  BUGS:

  🥕 quot15 = '''Here are fifteen quotation marks: """""""""""""""'''

    "quot15" : "ere are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"",
  
  🥕  numbers = [ 0.1, 0.2, 0.5, 1, 2, 5 ]
  
    "numbers" : [
      1.0000000000000001E-001,
      2.0000000000000001E-001,
      5.0000000000000000E-001,
      1,
      2,
      5
    ],


}

begin
  TestJSON('t14');
end.