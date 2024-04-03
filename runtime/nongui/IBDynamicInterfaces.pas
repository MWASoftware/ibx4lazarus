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
unit IBDynamicInterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils;

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

  IDynamicSQLDataset = interface(IUnknown)
  ['{c94afb6a-a28d-4b2b-b62e-8611816cf21e}']
    procedure RegisterDynamicComponent(aComponent: TComponent);
    procedure UnRegisterDynamicComponent(aComponent: TComponent);
  end;

  {The IDynamicSQLEditor interface is also provided by a Dynamic SQL Dataset.
   This interface allows a user to update the dataset's select SQL.

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
    procedure OrderBy(fieldname: string; ascending: boolean);
    procedure Add2WhereClause(Condition: string; OrClause: boolean=false);
    function QuoteIdentifierIfNeeded(const s: string): string;
    function SQLSafeString(const s: string): string;
    function GetOrderByClause: string;
    procedure SetOrderByClause(Value: string);
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
   when is should Update the SQL and when it should set parameters. Typically,
   the UpdateSQL procedure is called before the dataset is opened and before the
   OnBeforeOpen event is called. The SetParams procedure is called before the dataset
   is opened and after the OnBeforeOpen event is called. This sequence allows
   a user to set dataset parameters in an OnBeforeOpen event handler, while
   allowing the component priority over setting any parameter values - typically
   those included in conditional parts to the Where Clause added by the component.
  }

  IDynamicSQLComponent = interface
  ['{4814f5fd-9292-4028-afde-0106ed00ef84}']
    procedure UpdateSQL(Sender: IDynamicSQLEditor);
    procedure SetParams(Sender: IDynamicSQLParam);
  end;

implementation

end.

