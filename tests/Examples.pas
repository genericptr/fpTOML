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

procedure TestJSON(path: string);
var
  doc: TTOMLDocument;
begin
  doc := GetTOML(ReadFile(path));
  writeln(doc.AsJSON.FormatJSON);
  doc.Free;
end;

function TestString(test: string): TTOMLDocument;
begin
  writeln(test);
  result := GetTOML(test);
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

var
  path: string;
begin
  path := './tests/pass/t15.toml';
  TestJSON(path);
  //TestJSON('./tests/fail/f16.toml');
  //TestString('quot15 = ''''''Here are fifteen quotation marks: """""""""""""""''''''');
  //doc := TestString('flt5 = 1e06');
  //writeln(Double(doc['real']):1:1);
end.