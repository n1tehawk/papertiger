unit imageformunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TImageForm }

  TImageForm = class(TForm)
    ScanImage: TImage;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ImageForm: TImageForm;

implementation

{$R *.lfm}

end.

