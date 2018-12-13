unit SystemRestartStackWithKeyboard;

{$mode objfpc}{$H+}

interface

implementation
uses
 SystemRestartStack,
 GlobalConfig,
 GlobalConst,
 GlobalTypes,
 Platform,
 Threads,
 SysUtils,
 Classes,
 Ultibo,
 DWCOTG,Keyboard,
 Logging;

var
 KeyboardThreadHandle:TThreadHandle;
 KeyboardChar:Char;

function KeyboardThread(Parameter:Pointer):PtrInt;
begin
 Result:=0;
 while True do
  begin
   ConsoleGetKey(KeyboardChar,Nil);
   if KeyboardChar = #27 then
    PopKernel(0);
  end;
end;

initialization
 BeginThread(@KeyboardThread,Nil,KeyboardThreadHandle,THREAD_STACK_DEFAULT_SIZE);
end.
