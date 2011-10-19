{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mcaselli; 

interface

uses
  ThemedSlider, ImageTrack, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ThemedSlider', @ThemedSlider.Register); 
  RegisterUnit('ImageTrack', @ImageTrack.Register); 
end; 

initialization
  RegisterPackage('mcaselli', @Register); 
end.
