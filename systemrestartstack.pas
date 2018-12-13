unit SystemRestartStack;

{$mode objfpc}{$H+}

interface
procedure PushKernel(CurrentConfigFileName,KernelFileName,CmdFileName:String);
procedure PopKernel(Parameter:Integer);
function SystemRestartStackIsEmpty:Boolean;
procedure WaitForBootFilesStable;

implementation
uses
 GlobalConfig,
 GlobalTypes,
 Platform,
 Threads,
 SysUtils,
 Classes,
 Ultibo,
 FileSystem,MMC,FATFS;

var
 BootFilesStable:Boolean;
 StackLines:Array of String;
 IsPrevious:Boolean;
 PreviousConfigFileName:String;

function BoolToInt(X:Boolean):Integer;
begin
 if X then
  Result:=0
 else
  Result:=1;
end;

procedure PrintStack(Message:String);
var
 I:Integer;
begin
 WriteLn(Format('%s - PrintStack - BootFilesStable %d IsPrevious %d StackLength %d',[Message,BoolToInt(BootFilesStable),BoolToInt(IsPrevious),Length(StackLines)]));
 if Length(StackLines) > 0 then
  for I:=Low(StackLines) to High(StackLines) do
   WriteLn(Format('    %d <%s>',[I,StackLines[I]]));
 Sleep(10*1000);
end;

procedure WaitForBootFilesStable;
begin
 while not BootFilesStable do
  Sleep(100);
end;

procedure PopKernel(Parameter:Integer);
begin
 WaitForBootFilesStable;
 SystemRestart(Parameter);
end;

function SystemRestartStackIsEmpty:Boolean;
begin
// PrintStack('SystemRestartStackIsEmpty');
 Result:=(Length(StackLines) = 0) and not IsPrevious;
end;

procedure ReadStack;
var
 StackFile:TextFile;
begin
 SetLength(StackLines,0);
 try
  AssignFile(StackFile,'systemrestartstack.txt');
  Reset(StackFile);
  while not Eof(StackFile) do
   begin
    SetLength(StackLines,Length(StackLines) + 1);
    ReadLn(StackFile,StackLines[High(StackLines)]);
   end;
  Close(StackFile);
 except on E:Exception do
//  PrintStack(Format('ReadStack exception %s',[E.Message]));
 end;
// PrintStack('ReadStack done');
end;

procedure WriteStack;
var
 NewStackFile:TextFile;
 I:Integer;
begin
// PrintStack('WriteStack start ...');
 try
  AssignFile(NewStackFile,'newsystemrestartstack.txt');
  Rewrite(NewStackFile);
  if Length(StackLines) > 0 then
   for I:=Low(StackLines) to High(StackLines) do
    WriteLn(NewStackFile,StackLines[I]);
  Close(NewStackFile);
  CopyFile('newsystemrestartstack.txt','systemrestartstack.txt',False);
 except on E:Exception do
//  PrintStack(Format('WriteStack exception %s',[E.Message]));
 end;
// PrintStack('WriteStack done');
end;

procedure PrepareReturnConfig;
var
 ConfigFile:TextFile;
begin
 while not DirectoryExists('C:\') do
  Sleep(100);
 ReadStack;
 IsPrevious:=False;
 if Length(StackLines) > 0 then
  begin
   IsPrevious:=True;
   PreviousConfigFileName:=StackLines[High(StackLines)];
   if FileExists(PreviousConfigFileName) then
    CopyFile(PChar(PreviousConfigFileName),'config.txt',False);
   SetLength(StackLines,Length(StackLines) - 1);
   WriteStack;
  end;
end;

procedure PushKernel(CurrentConfigFileName,KernelFileName,CmdFileName:String);
var
 StackFile:TextFile;
 ConfigFile:TextFile;
begin
 WaitForBootFilesStable;
 ReadStack;
 if IsPrevious then
  begin
   SetLength(StackLines,Length(StackLines) + 1);
   StackLines[High(StackLines)]:=PreviousConfigFileName;
  end;
 SetLength(StackLines,Length(StackLines) + 1);
 StackLines[High(StackLines)]:=CurrentConfigFileName;
 WriteStack;
 AssignFile(ConfigFile,'config.txt');
 Rewrite(ConfigFile);
 WriteLn(ConfigFile,'kernel=' + KernelFileName);
 WriteLn(ConfigFile,'cmdline=' + CmdFileName);
 Close(ConfigFile);
 SystemRestart(0);
end;

function RestoreThread(Parameter:Pointer):PtrInt;
begin
 Result:=0;
 PrepareReturnConfig;
 BootFilesStable:=True;
 ThreadHalt(0);
end;

var
 RestoreThreadHandle:TThreadHandle;

initialization
 BootFilesStable:=False;
 RestoreThreadHandle:=0;
 BeginThread(@RestoreThread,Nil,RestoreThreadHandle,THREAD_STACK_DEFAULT_SIZE);
end.
