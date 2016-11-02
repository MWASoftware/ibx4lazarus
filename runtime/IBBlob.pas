{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{************************************************************************}

unit IBBlob;

{$mode Delphi}

interface

uses
  SysUtils, Classes, DB, IB, IBDatabase;


const
  DefaultBlobSegmentSize = 16 * 1024;

type
  TIBBlobStates = (bsUninitialised, bsDataPending, bsData, bsModified);

  { TIBBlobStream }
  TIBBlobStream = class(TStream)
  private
    FBase: TIBBase;
    FBlob: IBlob;
    FBlobMaxSegmentSize: Int64;
    FBlobNumSegments: Int64;
    FBlobSize: Int64;
    FBlobType: TBlobType;
    FBuffer: PChar;
    FColumnName: string;
    FMode: TBlobStreamMode;
    FPosition: Int64;
    FBlobState: TIBBlobStates;
    FRelationName: string;
    function GetBlobID: TISC_QUAD;
    function GetModified: Boolean;
    procedure CheckActive;
  protected
    procedure CloseBlob;
    procedure EnsureBlobInitialized;
    procedure EnsureLoaded;
    procedure GetBlobInfo;
    function  GetSize: Int64; override;
    function GetDatabase: TIBDatabase;
    function GetTransaction: TIBTransaction;
    procedure OpenBlob;
    procedure SetBlobID(Value: TISC_QUAD);
    procedure SetDatabase(Value: TIBDatabase);
    procedure SetMode(Value: TBlobStreamMode);
    procedure SetState(aValue: TIBBlobStates);
    procedure SetTransaction(Value: TIBTransaction);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckReadable;
    procedure CheckWritable;
    procedure Finalize;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(Stream: TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(Stream: TStream);
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SetField(aField: TField);
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    procedure Truncate;
    function Write(const Buffer; Count: Longint): Longint; override;
    property BlobID: TISC_QUAD read GetBlobID write SetBlobID;
    property Blob: IBlob read FBlob;
    property BlobMaxSegmentSize: Int64 read FBlobMaxSegmentSize;
    property BlobNumSegments: Int64 read FBlobNumSegments;
    property BlobSize: Int64 read GetSize;
    property BlobType: TBlobType read FBlobType;
    property Database: TIBDatabase read GetDatabase write SetDatabase;
    property Mode: TBlobStreamMode read FMode write SetMode;
    property Modified: Boolean read GetModified;
    property Transaction: TIBTransaction read GetTransaction write SetTransaction;
    property RelationName: string read FRelationName;
    property ColumnName: string read FColumnName;
  end;

implementation

uses FBMessages, IBCustomDataSet;

{ TIBBlobStream }
constructor TIBBlobStream.Create;
begin
  inherited Create;
  FBase := TIBBase.Create(Self);
  FBuffer := nil;
  FBlobSize := 0;
  FBlobState := bsUninitialised;
  FBlob := nil;
end;

destructor TIBBlobStream.Destroy;
begin
  CloseBlob;
  FBase.Free;
  SetSize(0);
  inherited Destroy;
end;

procedure TIBBlobStream.CheckReadable;
begin
  if FMode = bmWrite then IBError(ibxeBlobCannotBeRead, [nil]);
end;

procedure TIBBlobStream.CheckWritable;
begin
  if FMode = bmRead then IBError(ibxeBlobCannotBeWritten, [nil]);
end;

function TIBBlobStream.GetModified: Boolean;
begin
  Result := FBlobState = bsModified;
end;

procedure TIBBlobStream.CheckActive;
begin
  if Database = nil then
    IBError(ibxeDatabaseNotAssigned,[nil]);

  if (Database.Attachment = nil) or
                     not Database.Attachment.IsConnected then
    IBError(ibxeDatabaseClosed,[nil]);

  if Transaction = nil then
    IBError(ibxeTransactionNotAssigned,[nil]);

  if (Transaction.TransactionIntf = nil) or
      not Transaction.TransactionIntf.InTransaction then
    IBError(ibxeNotInTransaction,[nil]);
end;

function TIBBlobStream.GetBlobID: TISC_QUAD;
begin
  if (FBlob = nil) or (FBlobSize = 0) then
  begin
    Result.gds_quad_high := 0;
    Result.gds_quad_low := 0;
  end
  else
    Result := FBlob.GetBlobID;
end;

procedure TIBBlobStream.CloseBlob;
begin
  Finalize;
  FBlob := nil;
  SetState(bsUninitialised);
end;

procedure TIBBlobStream.EnsureBlobInitialized;
begin
  if FBlobState <> bsUninitialised then Exit;

  if FMode = bmWrite then
    SetState(bsData)
  else
  begin
    CheckReadable;
    if FBlob = nil then Exit;
    try
      GetBlobInfo;
      {Defer reading in blob until read method called}
    except
      FBlob := nil;
      raise;
    end;
    SetState(bsDataPending);
  end;
end;

procedure TIBBlobStream.EnsureLoaded;
begin
  EnsureBlobInitialized;
  if (FBlobState = bsDataPending) and (FBlob <> nil) then
  begin
    SetSize(FBlobSize);
    FBlob.Read(FBuffer^, FBlobSize);
    SetState(bsData);
  end;
end;

procedure TIBBlobStream.Finalize;
begin
  if FBlobState <> bsModified then
    exit;
  CheckWritable;
  if FBlobSize > 0 then
  begin
    { need to start writing to a blob, create one }
    FBlob := Database.Attachment.CreateBlob(Transaction.TransactionIntf,RelationName,ColumnName);
    FBlob.Write(FBuffer^, FBlobSize);
    FBlob.Close;
  end;
  SetState(bsData);
end;

procedure TIBBlobStream.GetBlobInfo;
var
  iBlobSize: Int64;
begin
  if FBlob = nil then Exit;

  FBlob.GetInfo(FBlobNumSegments, FBlobMaxSegmentSize, iBlobSize, FBlobType);
  SetSize(iBlobSize);
end;

function TIBBlobStream.GetSize: Int64;
begin
  EnsureBlobInitialized;
  Result := FBlobSize;
end;

function TIBBlobStream.GetDatabase: TIBDatabase;
begin
  result := FBase.Database;
end;

function TIBBlobStream.GetTransaction: TIBTransaction;
begin
  result := FBase.Transaction;
end;

procedure TIBBlobStream.LoadFromFile(Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TIBBlobStream.LoadFromStream(Stream: TStream);
begin
  CheckWritable;
  EnsureBlobInitialized;
  Stream.Position := 0;
  SetSize(Stream.Size);
  if FBlobSize <> 0 then
    Stream.ReadBuffer(FBuffer^, FBlobSize);
  SetState(bsModified);
end;

procedure TIBBlobStream.OpenBlob;
begin
  CheckReadable;
  try
    GetBlobInfo;
    {Defer reading in blob until read method called}
  except
    FBlob.Close;
    raise;
  end;
  SetState(bsDataPending);
end;

function TIBBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  CheckReadable;
  EnsureLoaded;
  if Count <= 0 then
  begin
    result := 0;
    exit;
  end;
  if (FPosition + Count > FBlobSize) then
    result := FBlobSize - FPosition
  else
    result := Count;
  Move(FBuffer[FPosition], Buffer, result);
  Inc(FPosition, Result);
end;

procedure TIBBlobStream.SaveToFile(Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TIBBlobStream.SaveToStream(Stream: TStream);
begin
  CheckReadable;
  EnsureLoaded;
  if FBlobSize <> 0 then
  begin
    Seek(0, soFromBeginning);
    Stream.WriteBuffer(FBuffer^, FBlobSize);
  end;
end;

function TIBBlobStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  EnsureBlobInitialized;
  case Origin of
    soBeginning     : FPosition := Offset;
    soCurrent	    : Inc(FPosition, Offset);
    soEnd           : FPosition := FBlobSize + Offset;
  end;
  result := FPosition;
end;

procedure TIBBlobStream.SetField(aField: TField);
begin
  FRelationName := '';
  if aField.FieldDef <> nil then
    FRelationName := (aField.FieldDef as TIBFieldDef).RelationName;
  FColumnName := aField.FieldName;;
end;

procedure TIBBlobStream.SetBlobID(Value: TISC_QUAD);
begin
  CheckActive;
  FBlob := nil;
  if (Value.gds_quad_high = 0) and (Value.gds_quad_low = 0) then
    Exit;
  FBlob := Database.Attachment.OpenBlob(Transaction.TransactionIntf,RelationName,ColumnName,Value);
  if FBlobState <> bsData then
    SetState(bsUninitialised);
end;

procedure TIBBlobStream.SetDatabase(Value: TIBDatabase);
begin
  FBase.Database := Value;
  SetState(bsUninitialised);
end;

procedure TIBBlobStream.SetMode(Value: TBlobStreamMode);
begin
  FMode := Value;
  SetState(bsUninitialised);
end;

procedure TIBBlobStream.SetState(aValue: TIBBlobStates);
begin
  if FBlobState = aValue then Exit;

  if (FBlobState = bsDataPending) and (FBlob <> nil) then
    FBlob.Close;

  FBlobState := aValue;
end;

procedure TIBBlobStream.SetSize(const NewSize: Int64);
begin
  if (NewSize <> FBlobSize) then
  begin
    ReallocMem(FBuffer, NewSize);
    FBlobSize := NewSize;
    if NewSize = 0 then
      FBuffer := nil;
  end;
end;

procedure TIBBlobStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure TIBBlobStream.SetTransaction(Value: TIBTransaction);
begin
  FBase.Transaction := Value;
  SetState(bsUninitialised);
end;

procedure TIBBlobStream.Truncate;
begin
  SetSize(0);
  SetState(bsModified);
end;

function TIBBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  CheckWritable;
  EnsureLoaded;  {Could be an untruncated bmReadWrite Blob}
  result := Count;
  if Count <= 0 then
    exit;
  if (FPosition + Count > FBlobSize) then
    SetSize(FPosition + Count);
  Move(Buffer, FBuffer[FPosition], Count);
  Inc(FPosition, Count);
  SetState(bsModified);
end;

end.
