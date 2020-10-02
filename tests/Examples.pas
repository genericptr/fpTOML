{
    Copyright (c) 2020 by Ryan Joseph

    TOML Parser Examples
}

{$mode objfpc}
{$H+}

program Examples;
uses
	SysUtils, BaseUnix, Classes, FPJSON, FGL, TOMLParser, TOMLTypes;

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

var
	contents: string;
  doc: TTOMLDocument;
  json: TJSONData;
  value: TTOMLData;
  table: TTOMLTable;
  a: TTOMLArray;
begin
	contents := ReadFile('./tests/pass/t10.toml');
	doc := GetTOML(contents);

  //value := doc['table-1']['key1'];
  //writeln('value: ', string(value));

  //table := doc['table-1'] as TTOMLTable;
  //writeln('value: ', string(table['key1']));

  //for value in doc['table-1']['array'] do
  //  writeln('value: ', string(value));

  //for value in doc['table-1'] do
  //  writeln('value: ', string(value));

  json := doc.AsJSON;
  writeln(json.FormatJSON);
end.