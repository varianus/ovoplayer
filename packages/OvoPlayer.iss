#define SourceDir "C:\source\ovoplayer"
#define MyAppName "OvoPlayer"

#define FileHandle
#define FileLine
#define MyAppVer = "unknown"
#if FileHandle = FileOpen(SourceDir + "\src\version.inc")
  #expr FileLine = FileRead(FileHandle)
  #define MyAppVer = Copy(FileLine, 2, Len(FileLine) -2)
  #pragma message "Detected Version: " + MyAppVer
  #expr FileClose(FileHandle)
#endif

[Files]
Source: {#SourceDir}\bin\win32\ovoplayer.exe; DestDir: {app}; Flags: overwritereadonly createallsubdirs recursesubdirs; 
Source: {#SourceDir}\bin\win32\sqlite3.dll; DestDir: {app};
Source: {#SourceDir}\language\ovoplayer.po; DestDir: {app}\locale;  
Source: {#SourceDir}\language\ovoplayer.it.po; DestDir: {app}\locale;
Source: {#SourceDir}\images\logo.png; DestDir: {app}\Resources;
Source: {#SourceDir}\images\nocover.png; DestDir: {app}\Resources; 
Source: {#SourceDir}\LICENSE.txt; DestDir: {app};

[Dirs]
Name: Resources; 
Name: locale; 

[Icons]
Name: {group}\OvoPlayer; Filename: {app}\ovoplayer.exe; WorkingDir: {app}; IconFilename: {app}\ovoplayer.exe; Comment: OvoPlayer; 

[Setup]
SetupIconFile={#SourceDir}\images\ovoplayer.ico
AppCopyright=Copyright 2012 Marco Caselli, Felipe Monteiro de Carvalho
AppName={#MyAppName}
AppVerName= {#MyAppName}{#MyAppVer}
DefaultDirName={pf}\{#MyAppName}
OutputDir={#SourceDir}\packages
AppID={{1166B883-FEDE-422B-927C-411EEECB57FF}
Compression=lzma/Ultra
InternalCompressLevel=Ultra
DefaultGroupName={#MyAppName}
AlwaysShowDirOnReadyPage=true
AlwaysShowGroupOnReadyPage=true
LicenseFile={#SourceDir}\LICENSE.txt
OutputBaseFilename={#MyAppName}-{#MyAppVer}
