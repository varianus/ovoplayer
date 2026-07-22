unit registerMCaselli;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FontImageList, ThemedSlider, ImageTrack, fontimagelisteditor, ComponentEditors;

  procedure Register;

implementation

procedure Register;
begin
  // Registra il componente
  RegisterComponents('MCaselli', [TFontImageList,TThemedSlider,TImageTrack]);
  RegisterComponentEditor(TFontImageList,TFontImageListEditor);

end;

end.

