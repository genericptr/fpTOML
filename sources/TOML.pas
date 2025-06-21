{
    Copyright (c) 2020 by Ryan Joseph

    Main unit for fpTOML parser
    
    ********************************************************************

    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
    INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
    PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
    OR OTHER DEALINGS IN THE SOFTWARE.
}

{$mode objfpc}
{$unitpath sources}

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

{ TOMLData Operators }

operator Explicit (right: TTOMLData): ansistring; overload;
operator Explicit (right: TTOMLData): shortstring; overload;
operator Explicit (right: TTOMLData): integer; overload;
operator Explicit (right: TTOMLData): single; overload;
operator Explicit (right: TTOMLData): double; overload;

operator := (right: variant): TTOMLData;

implementation

{ TOMLData Operators }

operator := (right: variant): TTOMLData;
begin
  result := TTOMLValue.Create(right);
end;

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