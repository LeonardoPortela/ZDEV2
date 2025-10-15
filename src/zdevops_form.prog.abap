*&---------------------------------------------------------------------*
*& Include ZDEVOPS_FORM
*&---------------------------------------------------------------------*

TYPES: BEGIN OF TY_ESTRUTURA,
  OP TYPE STRING,
  PATH TYPE STRING,
  FROM TYPE STRING,
  VALUE TYPE STRING,
END OF TY_ESTRUTURA.


DATA : lv_json    TYPE string VALUE '{"op": "add","path": "/fields/System.TeamProject","from": null,"value": proj}',
       lr_data    TYPE REF TO data,
       ls_estrutura TYPE ty_estrutura.

/ui2/cl_json=>deserialize(
  EXPORTING
    json         = lv_json
    pretty_name  = /ui2/cl_json=>pretty_mode-user
    assoc_arrays = abap_true
  CHANGING
    data         = lr_data ).

IF lr_data IS BOUND.

  ASSIGN lr_data->* TO FIELD-SYMBOL(<lfs_data>).

  DO 3 TIMES. "Number of fields

    CASE sy-index.
      WHEN 1. DATA(lv_fname) = 'OP'.
      WHEN 2. lv_fname = 'PATH'.
      WHEN 3. lv_fname = 'FROM'.
      WHEN 4. lv_fname = 'VALUE'.
    ENDCASE.

    ASSIGN COMPONENT sy-index OF STRUCTURE ls_estrutura TO FIELD-SYMBOL(<lfs_field>).
    ASSIGN COMPONENT lv_fname OF STRUCTURE <lfs_data> TO FIELD-SYMBOL(<lfs_ref_value>).
    IF <lfs_ref_value> IS ASSIGNED AND <lfs_field> IS ASSIGNED.
      lr_data = <lfs_ref_value>.
      ASSIGN lr_data->* TO FIELD-SYMBOL(<lfs_actual_value>).
      IF <lfs_actual_value> IS ASSIGNED.
        <lfs_field> = <lfs_actual_value>.
      ENDIF.
    ENDIF.
  ENDDO.

  cl_demo_output=>display( ls_estrutura ).

ENDIF.



*var dat = msg.payload.vdata;
*var hist = msg.payload.idhist;
*var tit = msg.payload.nmtitulo;
*var sprint = msg.payload.tagsprint;
*var proj = msg.payload.projeto;
*var inter = msg.payload.intersprint;
*var subproj = msg.payload.subprojeto;
*var linkazure = "https://dev.azure.com/Amaggi/SAP/_apis/wit/workItems/";
*msg.payload = [
*    {
*        "op": "add",
*        "path": "/fields/System.TeamProject",
*        "from": null,
*        "value": proj
*    },
*    {
*        "op": "add",
*        "path": "/fields/System.Tags",
*        "from": null,
*        "value": sprint
*    },
*    {
*        "op": "add",
*        "path": "/fields/System.Title",
*        "from": null,
*        "value": dat+" - 01 - Dev " + tit
*    },
*    {
*        "op": "add",
*        "path": "/fields/System.IterationPath",
*        "from": null,
*        "value": inter
*    },
*    {
*        "op": "add",
*        "path": "/fields/System.AreaPath",
*        "from": null,
*        "value": subproj
*    },
*    {
*        "op": "add",
*        "path": "/fields/System.State",
*        "from": null,
*        "value": "To Do"
*    },
*    {
*        "op": "add",
*        "path": "/fields/System.Reason",
*        "from": null,
*        "value": "Moved to state To Do"
*    },
*    {
*        "op": "add",
*        "path": "/fields/System.AssignedTo",
*        "from": null,
*        "value": "Pablo Alves"
*    },
*    {
*        "op": "add",
*        "path": "/fields/Microsoft.VSTS.Common.Activity",
*        "from": null,
*        "value": "Development"
*    },
*    {
*        "op": "add",
*        "path": "/fields/System.WorkItemType",
*        "from": null,
*        "value": "Task"
*    },
*    {
*        "op": "add",
*        "path": "/relations/-",
*        "value": {
*            "rel": "System.LinkTypes.Hierarchy-Reverse",
*            "url": linkazure + hist
*        }
*    }
*];
*
*return msg;
