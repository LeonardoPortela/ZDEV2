*&---------------------------------------------------------------------*
*& Report Z_CARGA_CSV_GENERIC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_carga_csv_generic.

TABLES: rsrd1.


SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
  SELECT-OPTIONS:
p_table   FOR rsrd1-tbma_val NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK part1.


START-OF-SELECTION.

  DATA: table_name       TYPE string,
        lo_dynamic_table TYPE REF TO data,
        lo_dynamic_line  TYPE REF TO data,
        file_name        TYPE string.

  FIELD-SYMBOLS: <lt_table_structure> TYPE table,
                 <ls_table_structure> TYPE any,
                 <fs_dados>           TYPE any,
                 <fs_header>          TYPE any.

  table_name = p_table-low.


  DATA: lt_fieldcat TYPE TABLE OF dfies,
        lt_columns  TYPE TABLE OF string,
        lv_TABLE    TYPE tabname.
  lv_table = table_name .

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = lv_TABLE
    TABLES
      dfies_tab = lt_fieldcat
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

*  IF sy-subrc = 0.
*BREAK-POINT.
*  ENDIF.


  CREATE DATA lo_dynamic_table TYPE TABLE OF (table_name).
  ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

  CREATE DATA lo_dynamic_line LIKE LINE OF <lt_table_structure>.
  ASSIGN lo_dynamic_line->* TO <ls_table_structure>.


  DATA: ifiletab TYPE filetable.
  DATA: xfiletab LIKE LINE OF ifiletab.
  DATA: rc TYPE i.

  DATA: itab TYPE TABLE OF string.
  DATA: xtab TYPE string.

START-OF-SELECTION.


  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table = ifiletab
      rc         = rc.
  READ TABLE ifiletab INTO xfiletab INDEX 1.
  file_name = xfiletab-filename.

  CHECK NOT file_name IS INITIAL.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                 = file_name
    TABLES
      data_tab                 = itab
    EXCEPTIONS
      file_open_error          = 1
      file_read_error          = 2
      no_batch                 = 3
      gui_refused_filetransfer = 4
      invalid_type             = 5
      no_authority             = 6
      OTHERS                   = 7.

  LOOP AT itab ASSIGNING FIELD-SYMBOL(<insert_table>).
    IF sy-tabix = 1.
      SPLIT <insert_table> AT ';' INTO TABLE DATA(it_header).
    ELSE.
      SPLIT <insert_table> AT ';' INTO TABLE DATA(it_dados).
      LOOP AT it_dados ASSIGNING <fs_dados>.
        READ TABLE it_header ASSIGNING FIELD-SYMBOL(<_get>) INDEX sy-tabix.

        CASE <_get>.
          WHEN 'ITEM_NUMBER'.
            DATA: LVA_MATNR_18    TYPE MATNR18.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <fs_dados>
              IMPORTING
                output = LVA_MATNR_18.
            <ls_table_structure>-(<_get>) = LVA_MATNR_18.
           WHEN 'SUPPLIER_NUMBER'.
            DATA: LVA_LIFNR    TYPE LIFNR.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <fs_dados>
              IMPORTING
                output = LVA_LIFNR.
            <ls_table_structure>-(<_get>) = LVA_LIFNR.
          WHEN 'BUKRS' OR 'WERKS'.

            DATA:_unidades TYPE bukrs.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <fs_dados>
              IMPORTING
                output = _unidades.
            <ls_table_structure>-(<_get>) = _unidades.
          WHEN 'TDLNR'.

            DATA:_TDLNR TYPE tdlnr.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <fs_dados>
              IMPORTING
                output = _TDLNR.
            <ls_table_structure>-(<_get>) = _TDLNR.

          WHEN OTHERS.
            <ls_table_structure>-(<_get>) = <fs_dados>.
        ENDCASE.

      ENDLOOP.
      APPEND <ls_table_structure> TO <lt_table_structure>.
    ENDIF.

  ENDLOOP.


  MODIFY (table_name) FROM TABLE <lt_table_structure>.
  COMMIT WORK.


  cl_demo_output=>display( <lt_table_structure> ).
