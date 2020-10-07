{$mode objfpc}

unit TOML;
interface
uses
  TOMLParser,
  TOMLTypes;

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

implementation

function GetTOML(contents: TTOMLStringType): TTOMLDocument;
begin
  result := TOMLParser.GetTOML(contents);
end;

end.