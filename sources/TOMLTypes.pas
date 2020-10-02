{$mode objfpc}
{$modeswitch advancedrecords}
{$scopedenums on}

unit TOMLTypes;
interface
uses
  SysUtils, FGL, FPJSON;

type
  TTOMLStringType = AnsiString;
  TTOMLKeyType = ShortString;
  TTOMLValueType = Variant;
  TTOMLNumberType = (Integer, 
                     Float,
                     Octal,
                     Boolean,
                     Binary,
                     Hexadecimal);

  { TTOMLData }
  ETOMLData = class(Exception);
  TTOMLData = class
    private type
      TEnumerator = record
        private
          container: TTOMLData;
          currentValue: TTOMLData;
          index: integer;
        public
          constructor Create(inContainer: TTOMLData); 
          function MoveNext: Boolean;
          procedure Reset;
          property CurrentIndex: integer read index;
          property Current: TTOMLData read currentValue;
      end;
    private
      function GetItem(index: integer): TTOMLData; overload; virtual;
      function GetItem(key: TTOMLKeyType): TTOMLData; overload; virtual;
    public
      parent: TTOMLData;
      function AsJSON: TJSONData; virtual;
      function Count: integer; virtual;
      property Items[index: integer]: TTOMLData read GetItem; default;
      function GetEnumerator: TEnumerator;
  end;
  TTOMLDataList = specialize TFPGObjectList<TTOMLData>;
  TTOMLDataMap = specialize TFPGMapObject<String, TTOMLData>;
  
  { TTOMLValue }

  TTOMLValue = class(TTOMLData)
    private
      m_value: TTOMLValueType;
    public
      constructor Create(const inValue: TTOMLValueType);
      function ToString: ansistring; override;
      function AsJSON: TJSONData; override;
      function TypeString: String;
      property Value: TTOMLValueType read m_value;
  end;

  { TTOMLNumber }

  TTOMLNumber = class(TTOMLValue)
    private
      m_type: TTOMLNumberType;
    public
      constructor Create(const inValue: TTOMLValueType; const inType: TTOMLNumberType);
      property &Type: TTOMLNumberType read m_type;
  end;

  { TTOMLDate }
  
  TTOMLDate = class(TTOMLData)
    public type
      TTime = record
        hours: integer;
        minutes: integer;
        seconds: double;
        function IsSet: boolean;
      end;
    public
      year: integer;
      month: integer;
      day: integer;
      time: TTime;
      
      { To unambiguously represent a specific instant in time, 
        you may use an RFC 3339 formatted date-time with offset. 
        https://tools.ietf.org/html/rfc3339}

      offset: TTime;

      { A suffix which, when applied to a time, denotes a UTC
      offset of 00:00; often spoken "Zulu" from the ICAO
      phonetic alphabet representation of the letter "Z". }
      z: boolean;
    public
      constructor Create(localTime: TTime); overload;
      function ToString: ansistring; override;
      function AsJSON: TJSONData; override;
  end;

  { TTOMContainer }

  TTOMLContainer = class(TTOMLData);
  TTOMLContainerList = specialize TFPGList<TTOMLContainer>;

  { TTOMLArray }

  TTOMLArray = class(TTOMLContainer)
    private
      list: TTOMLDataList;
    public
      terminated: boolean;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(const value: TTOMLValueType); overload;
      procedure Add(const data: TTOMLData); overload;
      function Last: TTOMLData;
      function AsJSON: TJSONData; override;
      function GetItem(index: integer): TTOMLData; override;
      function Count: integer; override;
  end;

  { TTOMLTable }

  TTOMLTable = class(TTOMLContainer)
    private
      map: TTOMLDataMap;
      m_name: string;
    public
      defined: boolean;
      terminated: boolean;
      parentIsArray: boolean;
    public
      constructor Create(name: string = '');
      destructor Destroy; override;
      
      procedure Add(const key: TTOMLKeyType; const value: TTOMLValueType); overload;
      procedure Add(const key: TTOMLKeyType; const data: TTOMLData); overload;
      function Find(const key: TTOMLKeyType): TTOMLData; inline;
      function Contains(const key: TTOMLKeyType): boolean; inline;
      function AsJSON: TJSONData; override;
      function Count: integer; override;

      function GetItem(key: TTOMLKeyType): TTOMLData; override;
      function GetItem(index: integer): TTOMLData; override;

      property Name: string read m_name;
  end;

  { TTOMLDocument }

  TTOMLDocument = class(TTOMLTable);

{ TOMLData Operators }
operator Explicit (right: TTOMLData): ansistring; overload;
operator Explicit (right: TTOMLData): shortstring; overload;
operator Explicit (right: TTOMLData): integer; overload;
operator Explicit (right: TTOMLData): single; overload;
operator Explicit (right: TTOMLData): double; overload;

implementation
uses
  Variants, Types;

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
  result := 0;
end;

operator Explicit (right: TTOMLData): single;
begin
  result := 0;
end;

operator Explicit (right: TTOMLData): double;
begin
  result := 0;
end;

{ TTOMLData }

function TTOMLData.GetEnumerator: TEnumerator;
begin
  result := TEnumerator.Create(self);
end;

constructor TTOMLData.TEnumerator.Create(inContainer: TTOMLData);
begin
  container := inContainer;
  index := 0;
end;
    
function TTOMLData.TEnumerator.MoveNext: Boolean;
var
  count: integer;
begin
  count := container.Count;
  if index = count then
    exit(false);
  while index < count do
    begin
      currentValue := container[index];
      index += 1;
      if currentValue <> Default(TTOMLData) then
        break;
    end;
  result := index <= count;
end;
    
procedure TTOMLData.TEnumerator.Reset;
begin
  index := 0;
end;

function TTOMLData.GetItem(index: integer): TTOMLData;
begin
  Assert(false, ClassName+' doesn''t implement indexing');
end;

function TTOMLData.GetItem(key: TTOMLKeyType): TTOMLData;
begin
  Assert(false, ClassName+' doesn''t implement keys');
end;

function TTOMLData.Count: integer;
begin
  Assert(false, ClassName+' doesn''t implement indexing');
end;

function TTOMLData.AsJSON: TJSONData;
begin
  Assert(false, 'TOML data can''t be converted to JSON');
end;

{ TTOMLValue }

function TTOMLValue.TypeString: String;
begin
  case VarType(value) of
    varEmpty: result := 'Empty';
    varNull: result := 'Null';
    varSingle: result := 'Single';
    varDouble: result := 'Double';
    varDecimal: result := 'Decimal';
    varCurrency: result := 'Currency';
    varDate: result := 'Date';
    varOleStr: result := 'UnicodeString';
    varString: result := 'Dynamic string';
    varBoolean: result := 'Boolean';
    varVariant: result := 'Variant';
    varUnknown: result := 'unknown';
    varShortInt: result := 'ShortInt';
    varSmallint: result := 'Smallint';
    varInteger: result := 'Integer';
    varInt64: result := 'Int64';
    varByte: result := 'Byte';
    varWord: result := 'Word'; 
    varLongWord: result := 'LongWord';
    varQWord: result := 'QWord';
    varError: result := 'ERROR';
    otherwise
      result := 'unknown';
  end;
end;

function TTOMLValue.AsJSON: TJSONData;
begin
  case VarType(value) of
    varSingle,
    varDouble,
    varDecimal,
    varCurrency:
      result := CreateJSON(Double(value));
    varDate:
      ;
    varOleStr,
    varStrArg,
    varString:
      result := CreateJSON(TTOMLStringType(value));
    varBoolean:
      result := CreateJSON(Boolean(value));
    varShortInt,
    varSmallint,
    varInteger,
    varInt64,
    varByte,
    varWord,
    varLongWord,
    varQWord:
      result := CreateJSON(LongInt(value));
    otherwise
      Assert(false, 'TOML value '+IntToStr(VarType(value))+' couldn''t be mapped to JSON value.');
    end;
end;

function TTOMLValue.ToString: ansistring;
begin
  result := ansistring(value);
end;

constructor TTOMLValue.Create(const inValue: TTOMLValueType);
begin
  m_value := inValue;
end;

{ TTOMLDate }

function TTOMLDate.TTime.IsSet: boolean;
begin
  result := (hours > 0) or (minutes > 0) or (seconds > 0);
end;

constructor TTOMLDate.Create(localTime: TTime);
begin
  time := localTime;
end;

function TTOMLDate.ToString: ansistring;
begin
  // TODO: we need to convert this to a proper date
  // https://tools.ietf.org/html/rfc3339

  result := IntToStr(year)+'/'+
            IntToStr(month)+'/'+
            IntToStr(day);

  if time.IsSet then
    begin
      result += ' '+IntToStr(time.hours)+':'+
                    IntToStr(time.minutes)+':'+
                    FloatToStr(time.seconds);
    end;
end;

function TTOMLDate.AsJSON: TJSONData;
begin
  result := CreateJSON(ToString);
end;

{ TTOMLNumber }

constructor TTOMLNumber.Create(const inValue: TTOMLValueType; const inType: TTOMLNumberType);
begin
  m_value := inValue;
  m_type := inType;
end;

{ TTOMLArray }

function TTOMLArray.GetItem(index: integer): TTOMLData;
begin
  result := list[index];
end;

function TTOMLArray.AsJSON: TJSONData;
var
  arr: TJSONArray;
  data: TTOMLData;
begin
  arr := TJSONArray.Create;
  for data in list do
    arr.Add(data.AsJSON);
  result := arr;
end;

function TTOMLArray.Last: TTOMLData;
begin
  result := list.Last;
end;

function TTOMLArray.Count: integer;
begin
  result := list.Count;
end;

procedure TTOMLArray.Add(const value: TTOMLValueType);
begin
  Add(TTOMLValue.Create(value));
end;

procedure TTOMLArray.Add(const data: TTOMLData);
begin
  list.Add(data);
end;

constructor TTOMLArray.Create;
begin
  list := TTOMLDataList.Create(true);
end;

destructor TTOMLArray.Destroy;
begin
  list.Free;
  inherited;
end;

{ TTOMLTable }

function TTOMLTable.GetItem(key: TTOMLKeyType): TTOMLData;
begin
  result := map[key];
end;

function TTOMLTable.GetItem(index: integer): TTOMLData;
begin
  result := map.data[index];
end;

function TTOMLTable.Count: integer;
begin
  result := map.Count;
end;

function TTOMLTable.AsJSON: TJSONData;
var
  i: integer;
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  for i := 0 to map.Count - 1 do
    obj.Add(map.Keys[i], map.Data[i].AsJSON);
  result := obj;
end;

procedure TTOMLTable.Add(const key: String; const data: TTOMLData);
begin
  if Contains(key) then
    raise ETOMLData.Create('Key "'+key+'" already exists in table "'+name+'"');
  map.Add(key, data);
end;

procedure TTOMLTable.Add(const key: String; const value: TTOMLValueType);
begin
  Add(key, TTOMLValue.Create(value));
end;

function TTOMLTable.Contains(const key: TTOMLKeyType): boolean;
var
  data: TTOMLData;
begin
  result := map.TryGetData(key, data);
end;

function TTOMLTable.Find(const key: TTOMLKeyType): TTOMLData;
var
  data: TTOMLData;
begin
  if map.TryGetData(key, data) then
    result := data
  else
    result := nil;
end;

constructor TTOMLTable.Create(name: string);
begin
  m_name := name;
  defined := false;
  map := TTOMLDataMap.Create(true);
end;

destructor TTOMLTable.Destroy;
begin
  map.Free;
  inherited;
end;


end.