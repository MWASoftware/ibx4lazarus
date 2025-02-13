(*
 *  IBX For Lazarus (Firebird Express)
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)

{$interfaces corba}

{
  The purpose of these interfaces is to define a set of interfaces that permit the
  IBControls package to be independent of the IBX package. These interfaces
  are provided by IBX but may also be provided by other database adapters for
  Firebird and other databases.

  Note the use of CORBA interfaces. In this mode, the interface is not a
  managed type. Any objects providing such an interface must be both
  explicitly created and destroyed.
}

{
  The following interfaces allow a dynamic component to register with a dataset
  supporting dynamic SQL Update, and to be notified.

  A Dynamic SQL Dataset provides the IDynamicSQLDataset and IDynamicSQLEditor
  interfaces.

  A Dynamic SQL Component provides the IDynamicSQLComponent interface.
}

type
  {The IDynamicSQLDataset interface is used to register/unregister a dynamic
   SQL component with a dataset providing this interface. The component
   must provide the IDynamicSQLComponent. Otherwise an exception is raised
   when RegisterDynamicComponent is called.}

  TDynamicDatasetCapability = (dcChangeDatasetOrder, {supports OrderBy, GetOrderByClause and SetOrderByClause interfaces}
                               dcUpdateWhereClause,  {supports Add2WhereClause interface}
                               dcSetParams);         {supports IDynamicSQLParam}
  TDynamicDatasetCapabilities = set of TDynamicDatasetCapability;

  IDynamicSQLDataset = interface
  ['{c94afb6a-a28d-4b2b-b62e-8611816cf21e}']
    procedure RegisterDynamicComponent(aComponent: TComponent);
    procedure UnRegisterDynamicComponent(aComponent: TComponent);
    function GetCapabilities: TDynamicDatasetCapabilities;
  end;

  {The IDynamicSQLEditor interface allows a user to update the dataset's select SQL.
   It is provided by a dataset or a specialised SQL parser.

   OrderBy: the select SQL Order By clause is updated to order the dataset by
   the field name in ascending or descending order.

   Add2WhereClause: the select SQL Where clause is updated to add the provided
   condition. This condition may include parameters in the IBX parameter syntax
   i.e. a valid SQL identifier preceded by a colon.

   QuoteIdentifierIfNeeded: . The returned value is the same as "s" but
   double quoted if the s is not a valid SQL Identifier.

   SQLSafeString: parses a text string and adds escapes for any unsafe SQL sequences
   i.e. embedded single quotes.
  }

  IDynamicSQLEditor = interface
  ['{3367a89a-4059-49c5-b25f-3ff0fa4f3d55}']
    procedure OrderBy(fieldname: string; ascending: boolean); overload;
    procedure OrderBy(columnNo: integer; ascending: boolean); overload;
    procedure Add2WhereClause(const Condition: string; OrClause: boolean=false; IncludeUnions: boolean = false);
    function QuoteIdentifierIfNeeded(const s: string): string;
    function SQLSafeString(const s: string): string;
    function GetOrderByClause: string;
    procedure SetOrderByClause(const Value: string);
  end;

  {The IDynamicSQLParam interface is provided by a Dynamic SQL Dataset. This allows
   the caller to set param values including "null".
  }

  IDynamicSQLParam = interface
  ['{02dc5296-25e0-4767-95f5-9a4a29a89ddb}']
    function GetParamValue(ParamName: string): variant;
    procedure SetParamValue(ParamName: string; ParamValue: variant);
  end;

  {The IDynamicSQLComponent interface is provided by a Dynamic SQL Component.
   This interface is used by a Dynamic SQL Dataset to tell the component
   when it should Update the SQL and when it should set parameters.
   The UpdateSQL procedure is called before the dataset is opened and before the
   OnBeforeOpen event is called. The SetParams procedure is called before the dataset
   is opened and after the OnBeforeOpen event is called. This sequence allows
   a user to set dataset parameters in an OnBeforeOpen event handler, while
   allowing the component priority over setting any parameter values - typically
   those included in conditional parts to the Where Clause added by the component.
  }

  IDynamicSQLComponent = interface
  ['{4814f5fd-9292-4028-afde-0106ed00ef84}']
    procedure UpdateSQL(SQLEditor: IDynamicSQLEditor);
    procedure SetParams(SQLParamProvider: IDynamicSQLParam);
  end;

  {The following two types are used, when required, to support component events
   called by an IDynamicSQLComponent provider}

  TOnUpdateSQL = procedure(Sender: TObject; SQLEditor: IDynamicSQLEditor) of object;
  TOnSetParams = procedure(Sender: TObject; SQLParamProvider: IDynamicSQLParam) of object;

  {The IArrayField interface provides access to a TField instance that is for
   an array field.
  }
  IArrayFieldDef = interface
  ['{10d1c460-168f-40a8-b98c-05c6971c09f5}']
    function GetArrayDimensions: integer;
    function GetArrayLowerBound(dim: integer): integer;
    function GetArrayUpperBound(dim: integer): integer;
  end;

  IArrayField = interface(IArrayFieldDef)
  ['{1c2492a4-09c7-4515-852e-f6affc6f78da}']
    function IsEmpty: boolean;
    function GetEltAsString(index: array of integer): string;
    procedure SetEltAsString(index: array of integer; aValue: string);
  end;



