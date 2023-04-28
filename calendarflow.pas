unit CalendarFlow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

const
  weektitle_zh:array[1..7] of string = ('周一','周二','周三','周四','周五','周六','周日');
  weektitle_en:array[1..7] of string = ('MON','TUE','WED','THU','FRI','SAT','SUN');
  monthtitle_zh:array[1..12] of string = ('一月','二月','三月','四月','五月','六月','七月','八月','九月','十月','十一月','十二月');
  monthtitle_en:array[1..12] of string = ('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC');

type

  TCalendarCellSize = class
    FCellWidth:Integer;
    FCellHeight:Integer;
    FBeltWidth:Integer;
    FTitleHeight:Integer;
  published
    property CellWidth:Integer read FCellWidth write FCellWidth default 40;
    property CellHeight:Integer read FCellHeight write FCellHeight default 30;
    property BeltWidth:Integer read FBeltWidth write FBeltWidth default 20;
    property TitleHeight:Integer read FTitleHeight write FTitleHeight default 30;
  end;

  TCalendarApperance = class
    FColor:TColor;
    FColorSel:TColor;
    FColorCnt:TColor;
    FColorBgm:TColor;
    FFont:TFont;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Color:Integer read FColor write FColor default clWhite;
    property ColorSel:Integer read FColorSel write FColorSel default clHighlight;
    property ColorCnt:Integer read FColorCnt write FColorCnt default clSkyBlue;
    property ColorBgm:Integer read FColorBgm write FColorBgm default clForm;
    property Font:TFont read FFont write FFont;
  end;

  TCalendarOption = (coAutoHorizontalCellSize, //自动分配水平方格大小
                     coAutoVerticalCellSize,   //自动分配垂直方格大小
                     coShowWeekCell,           //显示周行标
                     coShowMonthCell,          //显示月行标
                     coShowYearCell            //显示年行标
                     );
  TCalendarOptions = set of TCalendarOption;
  TDayRect = packed record
    Date:TDate;
    Rect:TRect;
  end;
  TNumberRect = packed record
    Number:Integer;
    Rect:TRect;
  end;

  TCalendarFlow = class(TPanel)
  private
    FCalendarFlowSize:TCalendarCellSize;
    FCalendarApperance:TCalendarApperance;
    FOptions:TCalendarOptions;
    FDateFirstTmp:TDate;//存储显示起点
    FDateTop:TDate;//存储显示起点
    FDateCurrent:TDate;
    FDateCountIn:TList;
    FRowCount:Integer;//仅在coAutoVerticalCellSize时有效
    //for paint and mouseclick
    FDays:array of TDayRect;
    FMonths:array of TNumberRect;
    FYears:array of TNumberRect;
    FWeeks:array of TNumberRect;


  private
    procedure Paint; override;
    procedure MouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  //events
  private
    FUpdating:Boolean;
    FOnDateChange:TNotifyEvent;

  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property OnMouseWheel;

  //DateCountIn
  public
    procedure CountInDate(ADate:TDate);
    procedure CountOutDate(ADate:TDate);
    function DateCounted(ADate:TDate):boolean;
    procedure ClearCountDate;
  protected
    procedure SetCurrentDate(ADate:TDate);
  public
    property CurrentDate:TDate read FDateCurrent write SetCurrentDate;
  public
    procedure Refresh;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CellSize:TCalendarCellSize read FCalendarFlowSize;
    property Options:TCalendarOptions read FOptions write FOptions;
    property RowCount:Integer read FRowCount write FRowCount default 6;
    property OnDateChange:TNotifyEvent read FOnDateChange write FOnDateChange;

  end;

procedure Register;

implementation
uses math;

procedure Register;
begin
  {$I calendarflow_icon.lrs}
  RegisterComponents('Apiglio Component',[TCalendarFlow]);
end;

{ TCalendarApperance }
constructor TCalendarApperance.Create;
begin
  inherited Create;
  FColor:=clWhite;
  FColorSel:=clHighlight;
  FColorCnt:=clSkyBlue;
  FColorBgm:=clForm;
  FFont:=TFont.Create;
end;
destructor TCalendarApperance.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

{ TCalendarFlow }

function compare_date(Item1,Item2:Pointer):Integer;
begin
  if pdouble(Item1)^ > pdouble(Item2)^ then result:=1
  else if pdouble(Item1)^ < pdouble(Item2)^ then result:=-1
  else result:=0;
end;

procedure TCalendarFlow.CountInDate(ADate:TDate);
var dtmp:pdouble;
begin
  getmem(dtmp,sizeof(double));
  dtmp^:=TDate(ADate);
  FDateCountIn.Add(dtmp);
  //FDateCountIn.Sort(@compare_date);
  if not FUpdating then Paint;
end;
procedure TCalendarFlow.CountOutDate(ADate:TDate);
var pi:integer;
begin
  pi:=0;
  while pi < FDateCountIn.Count do begin
    if TDate(FDateCountIn.Items[pi]^)=ADate then begin
      freemem(FDateCountIn.Items[pi],sizeof(double));
      FDateCountIn.Delete(pi);
      if not FUpdating then Paint;
      exit;
    end;
  end;
end;
function TCalendarFlow.DateCounted(ADate:TDate):boolean;
var pi:integer;
begin
  pi:=0;
  result:=true;
  while pi < FDateCountIn.Count do begin
    if trunc(double(FDateCountIn.Items[pi]^))=trunc(double(ADate)) then exit;
    inc(pi);
  end;
  result:=false;
end;
procedure TCalendarFlow.ClearCountDate;
begin
  while FDateCountIn.Count>0 do begin
    freemem(pdouble(FDateCountIn.Items[0]),sizeof(double));
    FDateCountIn.Delete(0);
  end;
end;

procedure TCalendarFlow.SetCurrentDate(ADate:TDate);
begin
  FDateCurrent:=ADate;
  FDateTop:=ADate-7;
  if assigned(FOnDateChange) then FOnDateChange(Self);
  if not FUpdating then Paint;
end;

procedure TCalendarFlow.Refresh;
begin
  Paint;
end;

procedure TCalendarFlow.Paint;
const boundary = 1;
var FirstDate,LastDate:TDate;
    pi,px,py,len:int64;
    ww,hh,x1,y1,x2,y2,fw,fh:integer;
    yy,mm,dd:word;
    CellAreaWidth,CellAreaHeight,MonthBeltLeft:integer;
    cell_rect:TRect;
    stmp:string;
    function offset(ARect:TRect;boundary:integer):TRect;
    begin
      result.Top:=ARect.Top+boundary;
      result.Left:=ARect.Left+boundary;
      result.Right:=ARect.Right-boundary;
      result.Bottom:=ARect.Bottom-boundary;
    end;

begin
  //Inherited Paint;
  //Paint 这代码写得好丑

  SetLength(FDays,0);
  SetLength(FMonths,0);
  SetLength(FYears,0);
  SetLength(FWeeks,0);

  Canvas.Brush.Color:=FCalendarApperance.ColorBgm;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Color:=clNone;
  Canvas.Clear;

  FirstDate:=FDateTop - DayOfWeek(FDateTop) mod 7 - 5;
  if coAutoVerticalCellSize in FOptions then begin
    CellAreaHeight:=Height - FCalendarFlowSize.TitleHeight;
    FCalendarFlowSize.CellHeight:=CellAreaHeight div FRowCount;
    LastDate:=FirstDate+7*FRowCount+6;
  end else begin
    LastDate:=FirstDate+7*(Height div FCalendarFlowSize.CellHeight)+13;
  end;
  if coAutoHorizontalCellSize in FOptions then begin
    CellAreaWidth:=Width;
    if coShowYearCell in Options then dec(CellAreaWidth,FCalendarFlowSize.BeltWidth);
    if coShowMonthCell in Options then dec(CellAreaWidth,FCalendarFlowSize.BeltWidth);
    if coShowWeekCell in Options then dec(CellAreaWidth,FCalendarFlowSize.BeltWidth);
    FCalendarFlowSize.CellWidth:=CellAreaWidth div 7;
  end else begin
    //
  end;

  Canvas.Brush.Color:=FCalendarApperance.Color;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Color:=clNone;
  Canvas.Font:=FCalendarApperance.Font;
  px:=0;
  py:=-7;
  ww:=FCalendarFlowSize.CellWidth;
  hh:=FCalendarFlowSize.CellHeight;
  DecodeDate(FirstDate,yy,mm,dd);

  if coShowYearCell in Options then begin
    MonthBeltLeft:=FCalendarFlowSize.BeltWidth;
    SetLength(FYears,1);
    FYears[0].Rect:=Classes.Rect(0,-2*hh,FCalendarFlowSize.BeltWidth,0);
    FYears[0].Number:=yy;
  end else begin
    MonthBeltLeft:=0;
  end;
  if coShowMonthCell in Options then begin
    SetLength(FMonths,1);
    FMonths[0].Rect:=Classes.Rect(MonthBeltLeft,-hh,MonthBeltLeft+FCalendarFlowSize.BeltWidth,0);
    FMonths[0].Number:=mm;
  end;

  FOR pi:=floor(double(FirstDate)) TO floor(double(LastDate)) DO BEGIN
    if pi=floor(double(FDateCurrent)) then begin
      Canvas.Brush.Color:=FCalendarApperance.ColorSel;
    end else if DateCounted(TDate(pi)) then begin
      Canvas.Brush.Color:=FCalendarApperance.ColorCnt;
    end else begin
      Canvas.Brush.Color:=FCalendarApperance.Color;
    end;

    DecodeDate(TDate(double(pi)),yy,mm,dd);
    if dd=1 then begin
      inc(py,7);
      if coShowMonthCell in Options then begin
        len:=Length(FMonths);
        {tmp}y2:=FCalendarFlowSize.TitleHeight+floor(py/7)*hh;
        FMonths[len-1].Rect.Bottom:={tmp}y2;
        SetLength(FMonths,len+1);
        FMonths[len].Number:=mm;
        FMonths[len].Rect:=Classes.Rect(MonthBeltLeft,{tmp}y2,MonthBeltLeft+FCalendarFlowSize.BeltWidth,{tmp}y2);
      end;
      if (mm=1) and (coShowYearCell in Options) then begin
        len:=Length(FYears);
        {tmp}y2:=FCalendarFlowSize.TitleHeight+floor(py/7)*hh;
        FYears[len-1].Rect.Bottom:={tmp}y2;
        SetLength(FYears,len+1);
        FYears[len].Number:=yy;
        FYears[len].Rect:=Classes.Rect(0,{tmp}y2,FCalendarFlowSize.BeltWidth,{tmp}y2);
      end;
    end;

    stmp:=IntToStr(dd);
    x1:=px*ww;
    y1:=FCalendarFlowSize.TitleHeight+floor(py/7)*hh;
    if coShowYearCell in Options then inc(x1,FCalendarFlowSize.BeltWidth);
    if coShowMonthCell in Options then inc(x1,FCalendarFlowSize.BeltWidth);
    x2:=x1+ww;
    y2:=y1+hh;

    fh:=Canvas.TextHeight(stmp);
    fw:=Canvas.TextWidth(stmp);
    cell_rect:=Classes.Rect(x1,y1,x2,y2);
    len:=Length(FDays);
    SetLength(FDays,len+1);
    FDays[len].Rect:=cell_rect;
    FDays[len].Date:=TDate(double(pi));

    Canvas.Rectangle(offset(cell_rect,boundary));
    Canvas.TextRect(cell_rect,x1+(ww-fw) div 2,y1+(hh-fh) div 2,stmp);
    px:=(px+1) mod 7;
    py:=py+1;
  END;
  Canvas.Brush.Color:=FCalendarApperance.Color;
  if coShowYearCell in Options then begin
    len:=Length(FYears);
    FYears[len-1].Rect.Bottom:=cell_rect.Bottom;
    for pi:=0 to len-1 do begin
      stmp:=IntToStr(FYears[pi].Number);
      fw:=Canvas.TextWidth(stmp);
      fh:=Canvas.TextHeight(stmp);
      with FYears[pi] do begin
        Canvas.Rectangle(offset(Rect,boundary));
        Canvas.TextRect(Rect,Rect.Left+(Rect.Width - fw) div 2,Rect.Top+(Rect.Height - fh) div 2,stmp);
      end;
    end;
  end;
  if coShowMonthCell in Options then begin
    len:=Length(FMonths);
    FMonths[len-1].Rect.Bottom:=cell_rect.Bottom;
    for pi:=0 to len-1 do begin
      stmp:=monthtitle_zh[FMonths[pi].Number];
      fw:=Canvas.TextWidth(stmp);
      fh:=Canvas.TextHeight(stmp);
      with FMonths[pi] do begin
        Canvas.Rectangle(offset(Rect,boundary));
        Canvas.TextRect(Rect,Rect.Left+(Rect.Width - fw) div 2,Rect.Top+(Rect.Height - fh) div 2,stmp);
      end;
    end;
  end;
  Canvas.Brush.Color:=FCalendarApperance.ColorBgm;
  with FCalendarFlowSize do begin
    if coShowYearCell in Options then
      Canvas.Rectangle(Classes.Rect(0,0,BeltWidth,TitleHeight));
    if coShowMonthCell in Options then
      Canvas.Rectangle(Classes.Rect(MonthBeltLeft,0,MonthBeltLeft+BeltWidth,TitleHeight));
    px:=MonthBeltLeft+BeltWidth;
    for pi:=1 to 7 do begin
      {tmp}py:=px+FCalendarFlowSize.CellWidth;
      cell_rect:=Classes.Rect(px,0,{tmp}py,TitleHeight);
      stmp:=weektitle_zh[pi];
      fw:=Canvas.TextWidth(stmp);
      fh:=Canvas.TextHeight(stmp);
      Canvas.Rectangle(cell_rect);
      Canvas.TextRect(cell_rect,cell_rect.Left+(cell_rect.Width-fw) div 2,cell_rect.Top+(cell_rect.Height-fh) div 2,stmp);
      px:={tmp}py;
    end;
    if coShowWeekCell in Options then
      Canvas.Rectangle(Classes.Rect(px,0,px+BeltWidth,TitleHeight));
  end;
end;
procedure TCalendarFlow.MouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var pi:integer;
    function Contains(APoint:TPoint;ARect:TRect):boolean;
    begin
      result:=false;
      if APoint.Y<=ARect.Top then exit;
      if APoint.X<=ARect.Left then exit;
      if APoint.X>=ARect.Right then exit;
      if APoint.Y>=ARect.Bottom then exit;
      result:=true;
    end;
begin
  for pi:=0 to Length(FDays)-1 do begin
    if Contains(Classes.Point(X,Y),FDays[pi].Rect) then begin
      FDateCurrent:=FDays[pi].Date;
      if assigned(FOnDateChange) then FOnDateChange(Self);
      Paint;
      exit;
    end;
  end;
end;
procedure TCalendarFlow.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta>0 then begin
    FDateTop:=FDateTop-7;
  end else begin
    FDateTop:=FDateTop+7;
  end;
  Paint;//变换大小后第一次滚轮之后重绘有问题
end;

procedure TCalendarFlow.BeginUpdate;
begin
  FUpdating:=true;
end;
procedure TCalendarFlow.EndUpdate;
begin
  FUpdating:=false;
end;

constructor TCalendarFlow.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCalendarFlowSize:=TCalendarCellSize.Create;
  FCalendarApperance:=TCalendarApperance.Create;
  FDateCountIn:=TList.Create;
  FDateCurrent:=Now;
  FDateTop:=Now-7;
  FRowCount:=6;
  //object inspector 里头设置的初始值怎么不管用？？
  FCalendarFlowSize.CellHeight:=30;
  FCalendarFlowSize.CellWidth:=40;
  FCalendarFlowSize.TitleHeight:=20;
  FCalendarFlowSize.BeltWidth:=30;
  FOptions:=[coShowMonthCell,coShowYearCell,coShowWeekCell];
  //FOptions:=FOptions + [coAutoHorizontalCellSize,coAutoVerticalCellSize];
  OnMouseWheel:=@MouseWheel;
  OnMouseUp:=@MouseUp;
  FUpdating:=false;

end;

destructor TCalendarFlow.Destroy;
begin
  ClearCountDate;
  FCalendarFlowSize.Free;
  FCalendarApperance.Free;
  inherited Destroy;
end;

end.
