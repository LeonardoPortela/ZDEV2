*&---------------------------------------------------------------------*
*& Report ZSDR_TAB_JSON
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr_tab_json.

PARAMETERS p_exp RADIOBUTTON GROUP gb01.
PARAMETERS p_imp RADIOBUTTON GROUP gb01.
PARAMETERS p_table TYPE tabname16 OBLIGATORY.
PARAMETERS p_file TYPE char100 DEFAULT 'C:\Teste\file.json'.

START-OF-SELECTION.

  CASE abap_true.
    WHEN p_exp.
      PERFORM f_export.
    WHEN p_imp.
      PERFORM f_import.
  ENDCASE.


*&---------------------------------------------------------------------*
*& Form f_export
*&---------------------------------------------------------------------*
FORM f_export .

  DATA: table_name       TYPE string,
        lo_dynamic_table TYPE REF TO data,
        lo_dynamic_line  TYPE REF TO data.

  FIELD-SYMBOLS: <lt_table_structure> TYPE table,
                 <ls_table_structure> TYPE any.

  table_name = p_table.

  CREATE DATA lo_dynamic_table TYPE TABLE OF (table_name).
  ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

  CREATE DATA lo_dynamic_line LIKE LINE OF <lt_table_structure>.
  ASSIGN lo_dynamic_line->* TO <ls_table_structure>.

  CHECK p_table IS NOT INITIAL.

  SELECT * FROM (p_table)
  INTO TABLE <lt_table_structure>.

  CHECK sy-subrc EQ 0.

  DATA(lv_json) = /ui2/cl_json=>serialize(
  EXPORTING data = <lt_table_structure>
        pretty_name = abap_true ).

  DATA lv_filename  TYPE string.

  lv_filename = p_file.

  CHECK lv_filename IS NOT INITIAL.

  DATA  lt_my_string_table TYPE STANDARD TABLE OF string.

  APPEND lv_json TO lt_my_string_table.


  cl_gui_frontend_services=>gui_download(
        EXPORTING
             filename = lv_filename
        CHANGING

             data_tab =       lt_my_string_table ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_import
*&---------------------------------------------------------------------*
FORM f_import .

  DATA: table_name       TYPE string,
        lo_dynamic_table TYPE REF TO data,
        lo_dynamic_line  TYPE REF TO data.

  FIELD-SYMBOLS: <lt_table_structure> TYPE table,
                 <ls_table_structure> TYPE any.

  table_name = p_table.

  CREATE DATA lo_dynamic_table TYPE TABLE OF (table_name).
  ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

  CREATE DATA lo_dynamic_line LIKE LINE OF <lt_table_structure>.
  ASSIGN lo_dynamic_line->* TO <ls_table_structure>.

  DATA lv_filename  TYPE string.

  lv_filename = p_file.

  DATA lv_json TYPE string.
  DATA  lt_my_string_table TYPE STANDARD TABLE OF string.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename = lv_filename
    CHANGING
      data_tab = lt_my_string_table.


  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json        = lt_my_string_table[ 1 ]
      pretty_name = abap_true
    CHANGING
      data        = <lt_table_structure>.


  CHECK sy-sysid NE 'PRD'.

  MODIFY (p_table) FROM TABLE <lt_table_structure>.

  COMMIT WORK AND WAIT.

ENDFORM.
