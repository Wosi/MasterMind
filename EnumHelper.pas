unit EnumHelper;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

type
  TEnumHelper<T> = class
    class function EnumToStr(const Value: T): String;
    class function StrToEnum(const Value: String): T;
  end;

implementation

uses
  typinfo;

class function TEnumHelper<T>.EnumToStr(const Value: T): String;
var
  TInfo : PTypeInfo;
begin
  TInfo := TypeInfo(T);
  Result := GetEnumName(TInfo, PByte(@Value)^);
end;

class function TEnumHelper<T>.StrToEnum(const Value: String): T;
var
  TInfo : PTypeInfo;
  i : Byte;
  pt : ^T;
begin
  TInfo := TypeInfo(T);
  i := GetEnumValue(TInfo, Value);
  pt := @i;
  Result := pt^;
end;

end.