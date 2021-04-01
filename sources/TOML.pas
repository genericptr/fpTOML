{$mode objfpc}
{$unitpath sources}

unit TOML;
interface
uses
  TOMLParser,// in 'sources/TOMLParser.pas',
  TOMLTypes;// in 'sources/TOMLTypes.pas';

type
  TTOMLStringType = TOMLTypes.TTOMLStringType;
  TTOMLKeyType = TOMLTypes.TTOMLStringType;
  TTOMLValueType = TOMLTypes.TTOMLStringType;
  TTOMLNumberType = TOMLTypes.TTOMLStringType;

type
  TTOMLData = TOMLTypes.TTOMLData;
  TTOMLValue = TOMLTypes.TTOMLValue;
  TTOMLNumber = TOMLTypes.TTOMLNumber;
  TTOMLDate = TOMLTypes.TTOMLDate;
  TTOMLArray = TOMLTypes.TTOMLArray;
  TTOMLTable = TOMLTypes.TTOMLTable;
  TTOMLDocument = TOMLTypes.TTOMLDocument;

function GetTOML(contents: TTOMLStringType): TTOMLDocument;

{ TOMLData Operators }
operator Explicit (right: TTOMLData): ansistring; overload;
operator Explicit (right: TTOMLData): shortstring; overload;
operator Explicit (right: TTOMLData): integer; overload;
operator Explicit (right: TTOMLData): single; overload;
operator Explicit (right: TTOMLData): double; overload;

implementation

{ TOMLData Operators }

operator Explicit (right: TTOMLData): ansistring;
begin
  result := right.ToString;
end;

operator Explicit (right: TTOMLData): shortstring;
begin
  result := right.ToString;
end;

operator Explicit (right: TTOMLData): integer;
begin
  result := right.ToInteger;
end;

operator Explicit (right: TTOMLData): single;
begin
  result := right.ToFloat;
end;

operator Explicit (right: TTOMLData): double;
begin
  result := right.ToFloat;
end;

function GetTOML(contents: TTOMLStringType): TTOMLDocument;
begin
  result := TOMLParser.GetTOML(contents);
end;

end.