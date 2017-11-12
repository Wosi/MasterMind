unit EnumHelper;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

type
  TEnumHelper<T> = class
    class function EnumToStr(const Value: T): String;
    class function StrToEnum(const Value: String): T;
    class procedure CheckArraysEqual(const Left, Right: array of T);
    class function ArrayToString(const Arr: array of T): String;
  end;

implementation

uses
  typinfo, fpcunit;

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

class procedure TEnumHelper<T>.CheckArraysEqual(const Left, Right: array of T);
begin
  TAssert.AssertEquals(ArrayToString(Left), ArrayToString(Right));
end;

class function TEnumHelper<T>.ArrayToString(const Arr: array of T): String;
var
  ElementList, ElementString: String;
  Element: T;
begin
  ElementList := '';
  for Element in Arr do
  begin
    if ElementList <> '' then
      ElementList := ElementList + ', ';
    ElementString :=  EnumToStr(Element);
    ElementList := ElementList + ElementString;
  end;

  Result := '[' + ElementList + ']';
end;

end.