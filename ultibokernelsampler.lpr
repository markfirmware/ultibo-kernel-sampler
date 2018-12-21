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

const
 MouseTag=1;
 KeyboardTag=2;
 HdmiDisplayTag=4;
 ThreadsAndCoresTag=8;
 Vc4GraphicsTag=16;
 NetworkingTag=32;
 ProgrammingTag=64;

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
 FirstLoop:Boolean;
 I:Integer;
 TagsMask:Integer;

procedure WriteLnAndClear(S:String);
begin
 ConsoleWindowWrite(Console1,S);
 ConsoleWindowClearEx(Console1,ConsoleWindowGetX(Console1),ConsoleWindowGetY(Console1),ConsoleWindowGetMaxX(Console1),ConsoleWindowGetY(Console1),False);
 ConsoleWindowWriteLn(Console1,'');
end;

procedure ClearToEndOfScreen;
begin
 ConsoleWindowClearEx(Console1,ConsoleWindowGetMinX(Console1),ConsoleWindowGetY(Console1),ConsoleWindowGetMaxX(Console1),ConsoleWindowGetMaxY(Console1),False);
end;

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

function YesNo(Tag:Integer):String;
begin
 if (Tag and TagsMask) <> 0 then
  Result:='yes'
 else
  Result:='no ';
end;

function FilterIncludes(FileName:String):Boolean;
var
 LowerCaseFileName:String;
begin
 Result:=True;
 if TagsMask <> 0 then
  begin
   Result:=False;
   LowerCaseFileName:=LowerCase(FileName);
   if (AnsiPos('mouse',LowerCaseFileName) <> 0) and ((TagsMask and MouseTag) <> 0) then
    Result:=True
   else if (AnsiPos('keyboard',LowerCaseFileName) <> 0) and ((TagsMask and KeyboardTag) <> 0) then
    Result:=True
   else if (AnsiPos('cpu',LowerCaseFileName) <> 0) and ((TagsMask and ThreadsAndCoresTag) <> 0) then
    Result:=True
   else if (AnsiPos('thread',LowerCaseFileName) <> 0) and ((TagsMask and ThreadsAndCoresTag) <> 0) then
    Result:=True
   else if (AnsiPos('screen',LowerCaseFileName) <> 0) and ((TagsMask and HdmiDisplayTag) <> 0) then
    Result:=True
   else if (AnsiPos('pfd',LowerCaseFileName) <> 0) and ((TagsMask and Vc4GraphicsTag) <> 0) then
    Result:=True
   else if (AnsiPos('industrialclock',LowerCaseFileName) <> 0) and ((TagsMask and Vc4GraphicsTag) <> 0) then
    Result:=True
   else if (AnsiPos('project1',LowerCaseFileName) <> 0) and ((TagsMask and Vc4GraphicsTag) <> 0) then
    Result:=True
   else if (AnsiPos('web',LowerCaseFileName) <> 0) and ((TagsMask and NetworkingTag) <> 0) then
    Result:=True
   else if (AnsiPos('exception',LowerCaseFileName) <> 0) and ((TagsMask and ProgrammingTag) <> 0) then
    Result:=True
   else if (AnsiPos('pascal',LowerCaseFileName) <> 0) and ((TagsMask and ProgrammingTag) <> 0) then
    Result:=True
   else if (AnsiPos('log',LowerCaseFileName) <> 0) and ((TagsMask and ProgrammingTag) <> 0) then
    Result:=True
   else if (AnsiPos('demo',LowerCaseFileName) <> 0) and (((TagsMask and ProgrammingTag) <> 0) or ((TagsMask and KeyboardTag) <> 0) or ((TagsMask and MouseTag) <> 0)) then
    Result:=True
   else if (AnsiPos('timedate',LowerCaseFileName) <> 0) and ((TagsMask and ProgrammingTag) <> 0) then
    Result:=True;
  end;
end;

function UsesOpenMax(SelectedKernelFileName:String):Boolean;
var
 Lower:String;
begin
 Lower:=LowerCase(SelectedKernelFileName);
 if AnsiPos('project1-kernel',Lower) <> 0 then
  Result:=True
 else if AnsiPos('camera-kernel',Lower) <> 0 then
  Result:=True
 else
  Result:=False;
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
 TagsMask:=0;
 FirstLoop:=True;
 while True do
  begin
   if not FirstLoop then
    ConsoleGetKey(KeyboardChar,Nil);
   ConsoleWindowSetXY(Console1,1,1);
   if not SystemRestartStackIsEmpty then
    begin
     WriteLnAndClear('escape key - exit ultibo kernel sampler');
     if KeyboardChar = #27 then
      PopKernel(0);
     WriteLnAndClear('');
    end;
   WriteLnAndClear(Format('Running on a %s',[BoardTypeToString(BoardGetType)]));
   WriteLnAndClear('');
   WriteLnAndClear('Press the indicated letter to start the corresponding kernel ...');
   WriteLnAndClear(' While running the selected kernel, the escape key will return to the list of samples ...');
   WriteLnAndClear(' Also, if power is removed and restored, the system will return to the list of samples');
   WriteLnAndClear('');
   for I:=0 to 6 do
    if KeyboardChar = Char(Ord('1') + I) then
     begin
      if (TagsMask and (1 shl I)) <> 0 then
       TagsMask:=TagsMask and (not (1 shl I))
      else
       TagsMask:=TagsMask or (1 shl I)
     end;
   WriteLnAndClear('Press a digit indicated below to include only kernels that feature the corresponding aspect');
   WriteLnAndClear(Format('Filter: 1 mouse %s 2 keyboard %s 3 hdmi display %s 4 threads and cores %s 5 vc4 graphics %s 6 networking %s 7 programming %s',[YesNo(MouseTag),YesNo(KeyboardTag),YesNo(HdmiDisplayTag),YesNo(ThreadsAndCoresTag),YesNo(Vc4GraphicsTag),YesNo(NetworkingTag),YesNo(ProgrammingTag)]));
   WriteLnAndClear('');
   Letter:='a';
   for I:=0 to Length(UseableKernels) - 1 do
    begin
     SelectedKernelFileName:=UseableKernels[I];
     if FilterIncludes(SelectedKernelFileName) then
      begin
       WriteLnAndClear(Letter + ' ' + SelectedKernelFileName);
       if KeyboardChar = Letter then
        begin
         if FileExists('c:\samplekernels\' + SelectedKernelFileName) then
          begin
           PushKernel('ultibokernelsampler-config.txt','samplekernels/' + selectedKernelFileName,'samplekernels/empty-cmdline.txt',UsesOpenMax(SelectedKernelFileName));
          end
         else
          begin
           WriteLnAndClear('');
           WriteLnAndClear(Format('%s is not on the sd card',[SelectedKernelFileName]));
           WriteLnAndClear('');
          end;
        end;
       Letter:=Char(Ord(Letter) + 1);
      end;
    end;
   ClearToEndOfScreen;
   FirstLoop:=False;
  end;
end.
