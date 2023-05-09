unit CalendarFlow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

const
  weektitle_zh:array[0..7] of string = ('周日','周一','周二','周三','周四','周五','周六','周日');
  weektitle_en:array[0..7] of string = ('SUN','MON','TUE','WED','THU','FRI','SAT','SUN');
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
                     coShowYearCell,           //显示年行标
                     coBeginWithSunday         //以周日为一周的开始
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

  TCustomCalendarFlow = class(TCustomPanel)
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
    function GetWeekNumber(date:TDate):integer;
    procedure Paint; override;
    function MousePosDelta(MousePos:TPoint):integer;
    procedure MouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  //events
  private
    FUpdating:Boolean;
    FOnDateChange:TNotifyEvent;
    FOnBeforeSelect:TNotifyEvent;
    FOnAfterSelect:TNotifyEvent;

  public
    procedure BeginUpdate;
    procedure EndUpdate;

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
  public
    property CellSize:TCalendarCellSize read FCalendarFlowSize;
    property Options:TCalendarOptions read FOptions write FOptions;
    property RowCount:Integer read FRowCount write FRowCount default 6;
    property OnDateChange:TNotifyEvent read FOnDateChange write FOnDateChange;//当前日期更改时执行。
    property OnBeforeSelect:TNotifyEvent read FOnBeforeSelect write FOnBeforeSelect;//点击界面选中一个日期，在修改当前日期之前执行，无论日期是否有更改。
    property OnAfterSelect:TNotifyEvent read FOnAfterSelect write FOnAfterSelect;//点击界面选中一个日期，在修改当前日期之后执行，无论日期是否有更改。

  end;

  TCalendarFlow = class(TCustomCalendarFlow)
  published
    property CellSize;
    property Options;
    property RowCount default 6;
    property OnDateChange;
    property OnBeforeSelect;
    property OnAfterSelect;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelColor;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
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

{ TCustomCalendarFlow }

function compare_date(Item1,Item2:Pointer):Integer;
begin
  if pdouble(Item1)^ > pdouble(Item2)^ then result:=1
  else if pdouble(Item1)^ < pdouble(Item2)^ then result:=-1
  else result:=0;
end;

procedure TCustomCalendarFlow.CountInDate(ADate:TDate);
var dtmp:pdouble;
begin
  getmem(dtmp,sizeof(double));
  dtmp^:=TDate(ADate);
  FDateCountIn.Add(dtmp);
  //FDateCountIn.Sort(@compare_date);
  if not FUpdating then Paint;
end;
procedure TCustomCalendarFlow.CountOutDate(ADate:TDate);
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
function TCustomCalendarFlow.DateCounted(ADate:TDate):boolean;
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
procedure TCustomCalendarFlow.ClearCountDate;
begin
  while FDateCountIn.Count>0 do begin
    freemem(pdouble(FDateCountIn.Items[0]),sizeof(double));
    FDateCountIn.Delete(0);
  end;
end;

procedure TCustomCalendarFlow.SetCurrentDate(ADate:TDate);
begin
  FDateCurrent:=ADate;
  FDateTop:=ADate-7;
  if assigned(FOnDateChange) then FOnDateChange(Self);
  if not FUpdating then Paint;
end;

procedure TCustomCalendarFlow.Refresh;
begin
  Paint;
end;

function TCustomCalendarFlow.GetWeekNumber(date:TDate):integer;
var yy,mm,dd:word;
    jan1st:TDate;
    daycount:int64;
    dow:byte;
begin
  decodeDate(date,yy,mm,dd);
  jan1st:=EncodeDate(yy,1,1);
  dow:=DayOfWeek(jan1st);
  if coBeginWithSunday in Options then
    daycount:=trunc(date)-trunc(jan1st)+(dow-1) mod 7
  else
    daycount:=trunc(date)-trunc(jan1st)+(dow-2) mod 7;
  result:=daycount div 7 + 1;
end;

procedure TCustomCalendarFlow.Paint;
const boundary = 1;
var FirstDate,LastDate:TDate;
    pi,px,py,len:int64;
    ww,hh,x1,y1,x2,y2,fw,fh:integer;
    yy,mm,dd:word;
    CellAreaWidth,CellAreaHeight,MonthBeltLeft,WeekBeltLeft:integer;
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

  if coBeginWithSunday in Options then
    FirstDate:=FDateTop - DayOfWeek(FDateTop) -7+1
  else
    FirstDate:=FDateTop - DayOfWeek(FDateTop) -7+2;
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

  WeekBeltLeft:=0;
  if coShowYearCell in Options then begin
    MonthBeltLeft:=FCalendarFlowSize.BeltWidth;
    SetLength(FYears,1);
    FYears[0].Rect:=Classes.Rect(0,-2*hh,FCalendarFlowSize.BeltWidth,0);
    FYears[0].Number:=yy;
    inc(WeekBeltLeft,FCalendarFlowSize.BeltWidth);
  end else begin
    MonthBeltLeft:=0;
  end;
  if coShowMonthCell in Options then begin
    SetLength(FMonths,1);
    FMonths[0].Rect:=Classes.Rect(MonthBeltLeft,-hh,MonthBeltLeft+FCalendarFlowSize.BeltWidth,0);
    FMonths[0].Number:=mm;
    inc(WeekBeltLeft,FCalendarFlowSize.BeltWidth);
  end;
  inc(WeekBeltLeft,7*FCalendarFlowSize.CellWidth);
  if coShowWeekCell in Options then begin
    SetLength(FWeeks,1);
    FWeeks[0].Rect:=Classes.Rect(WeekBeltLeft,-hh,Width,0);
    FWeeks[0].Number:=0;
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
    if ((px=0) or (mm=1) and (dd=1)) and (coShowWeekCell in Options) then begin
      len:=Length(FWeeks);
      {tmp}y2:=FCalendarFlowSize.TitleHeight+floor(py/7)*hh;
      FWeeks[len-1].Rect.Bottom:={tmp}y2;
      SetLength(FWeeks,len+1);
      FWeeks[len].Number:=GetWeekNumber(TDate(double(pi)));
      FWeeks[len].Rect:=Classes.Rect(WeekBeltLeft,{tmp}y2,Width,{tmp}y2);
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
  //特殊列绘制
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
  if coShowWeekCell in Options then begin
    len:=Length(FWeeks);
    FWeeks[len-1].Rect.Bottom:=cell_rect.Bottom;
    for pi:=0 to len-1 do begin
      stmp:='第'+IntToStr(FWeeks[pi].Number)+'周';
      fw:=Canvas.TextWidth(stmp);
      fh:=Canvas.TextHeight(stmp);
      with FWeeks[pi] do begin
        Canvas.Rectangle(offset(Rect,boundary));
        Canvas.TextRect(Rect,Rect.Left+(Rect.Width - fw) div 2,Rect.Top+(Rect.Height - fh) div 2,stmp);
      end;
    end;
  end;
  //标题绘制
  Canvas.Brush.Color:=FCalendarApperance.ColorBgm;
  with FCalendarFlowSize do begin
    if coShowYearCell in Options then
      Canvas.Rectangle(Classes.Rect(0,0,BeltWidth,TitleHeight));
    if coShowMonthCell in Options then
      Canvas.Rectangle(Classes.Rect(MonthBeltLeft,0,MonthBeltLeft+BeltWidth,TitleHeight));
    px:=MonthBeltLeft;
    if coShowMonthCell in Options then inc(px,BeltWidth);
    if coBeginWithSunday in Options then {tmp}len:=0 else {tmp}len:=1;
    for pi:={tmp}len to {tmp}len+6 do begin
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
      Canvas.Rectangle(Classes.Rect(px,0,Width,TitleHeight));
  end;
end;
procedure TCustomCalendarFlow.MouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
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
      if assigned(FOnBeforeSelect) then FOnBeforeSelect(Self);
      if assigned(FOnDateChange) and (FDays[pi].Date<>FDateCurrent) then FOnDateChange(Self);
      FDateCurrent:=FDays[pi].Date;
      Paint;
      if assigned(FOnAfterSelect) then FOnAfterSelect(Self);
      exit;
    end;
  end;
end;
function TCustomCalendarFlow.MousePosDelta(MousePos:TPoint):integer;
var sy,sm:boolean;
    col:integer;
begin
  result:=0;
  if MousePos.Y<CellSize.TitleHeight then exit;
  result:=7;
  sy:=coShowYearCell in Options;
  sm:=coShowMonthCell in Options;
  if not (sy or sm) then exit;
  result:=30;
  col:=MousePos.X div CellSize.BeltWidth;
  if sy and sm and (col=1) then exit;
  if (not sy) and sm and (col<1) then exit;
  result:=365;
  if sy and (col<1) then exit;
  result:=7;
end;
procedure TCustomCalendarFlow.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var base:integer;
begin
  base:=MousePosDelta(MousePos);
  if WheelDelta>0 then begin
    FDateTop:=FDateTop-base;
  end else begin
    FDateTop:=FDateTop+base;
  end;
  if base<>0 then Paint;//变换大小后第一次滚轮之后重绘有问题
end;

procedure TCustomCalendarFlow.BeginUpdate;
begin
  FUpdating:=true;
end;
procedure TCustomCalendarFlow.EndUpdate;
begin
  FUpdating:=false;
end;

constructor TCustomCalendarFlow.Create(TheOwner: TComponent);
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

destructor TCustomCalendarFlow.Destroy;
begin
  ClearCountDate;
  FCalendarFlowSize.Free;
  FCalendarApperance.Free;
  inherited Destroy;
end;

end.
