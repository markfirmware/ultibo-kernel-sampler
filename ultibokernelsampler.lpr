program UltiboKernelSampler;

{$mode objfpc}{$H+}

uses
 SystemRestartStack,
 {$ifdef BUILD_MODE_RPI } RaspberryPi,  {$endif}
 {$ifdef BUILD_MODE_RPI2} RaspberryPi2, {$endif}
 {$ifdef BUILD_MODE_RPI3} RaspberryPi3, {$endif}
 GlobalConfig,
 GlobalConst,
 GlobalTypes,
 Platform,
 Threads,
 SysUtils,
 Classes,
 Ultibo,
 DWCOTG,Keyboard,
 Console,
 Logging;

procedure StartLogging;
begin
 LOGGING_INCLUDE_COUNTER:=False;
 LOGGING_INCLUDE_TICKCOUNT:=True;
 CONSOLE_REGISTER_LOGGING:=True;
 CONSOLE_LOGGING_POSITION:=CONSOLE_POSITION_RIGHT;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
end;

var
 Console1:TWindowHandle;
 KeyboardChar:Char;
 SampleKernels:Array of String;
 UseableKernels:Array of String;
 OperatingBuildMode:String;

var
 FirstLoop:Boolean;
 I:Integer;

procedure AddKernel(Name:String);
begin
 SetLength(SampleKernels,Length(SampleKernels) + 1);
 SampleKernels[Length(SampleKernels) - 1]:=Name;
end;

procedure AddKernels;
begin
 SetLength(SampleKernels,0);
 {$i samplekernels.inc}
end;

procedure FilterKernels;
var
 I,ParsePosition:Integer;
 SampleKernelFileName,LeftSide,RightSide,UseableName:String;
 UseIt:Boolean;
begin
 SetLength(UseableKernels,0);
 for I:=0 to Length(SampleKernels) - 1 do
  begin
   SampleKernelFileName:=SampleKernels[I];
   ParsePosition:=LastDelimiter('-',SampleKernelFileName);
   LeftSide:=LeftStr(SampleKernelFileName,ParsePosition - 1);
   RightSide:=RightStr(SampleKernelFileName,Length(SampleKernelFileName) - ParsePosition);
   UseableName:=LeftSide + '-' + OperatingBuildMode + '.img';
   UseIt:=SampleKernelFileName = UseableName;
   if not UseIt and (OperatingBuildMode = 'rpi3') and (RightSide = 'rpi2.img') then
    begin
     if I + 1 <= High(SampleKernels) then
      UseIt:=SampleKernels[I + 1] <> UseableName
     else
      UseIt:=True;
    end;
   if UseIt then
    begin
     SetLength(UseableKernels,Length(UseableKernels) + 1);
     UseableKernels[Length(UseableKernels) - 1]:=SampleKernelFileName
    end;
  end;
end;

var
 Letter:Char;
 SelectedKernelFileName:String;

begin
 Console1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
 {$ifdef BUILD_MODE_RPI } OperatingBuildMode:='rpi';  {$endif}
 {$ifdef BUILD_MODE_RPI2} OperatingBuildMode:='rpi2'; {$endif}
 {$ifdef BUILD_MODE_RPI3} OperatingBuildMode:='rpi3'; {$endif}
 AddKernels;
 FilterKernels;
 KeyboardChar:=Char(0);
 FirstLoop:=True;
 while True do
  begin
   if not FirstLoop then
    ConsoleGetKey(KeyboardChar,Nil);
   ConsoleWindowClear(Console1);
   if not SystemRestartStackIsEmpty then
    begin
     WriteLn('escape key - exit ultibo kernel sampler');
     if KeyboardChar = #27 then
      PopKernel(0);
     WriteLn('');
    end;
   Letter:='a';
   for I:=0 to Length(UseableKernels) - 1 do
    begin
     SelectedKernelFileName:=UseableKernels[I];
     WriteLn(Letter + ' ' + SelectedKernelFileName);
     if KeyboardChar = Letter then
      begin
       if FileExists('c:\samplekernels\' + SelectedKernelFileName) then
        begin
         PushKernel('ultibokernelsampler-config.txt','samplekernels/' + selectedKernelFileName,'samplekernels/empty-cmdline.txt');
        end
       else
        begin
         WriteLn('');
         WriteLn(Format('%s is not on the sd card',[SelectedKernelFileName]));
         WriteLn('');
        end;
      end;
     Letter:=Char(Ord(Letter) + 1);
    end;
   FirstLoop:=False;
  end;
end.
