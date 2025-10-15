*&---------------------------------------------------------------------*
*& Report ZPABLO011
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_carga_zfit0084.

DATA: lv_file_path TYPE string,
      lt_data      TYPE STANDARD TABLE OF string WITH HEADER LINE.

DATA ano(4) TYPE c.
DATA mes(2) TYPE c.
DATA dia(2) TYPE c.
DATA hr(2) TYPE c.
DATA mm(2) TYPE c.
DATA ss(2) TYPE c.
DATA hrs(8) TYPE c.
DATA dt(10) TYPE c.

TYPES: BEGIN OF ty_zfit0084,
         mandt        TYPE zfit0084-mandt,
         bukrs        TYPE zfit0084-bukrs,
         mes_ano      TYPE zfit0084-mes_ano,
         saknr        TYPE zfit0084-saknr,
         belnr        TYPE zfit0084-belnr,
         buzei        TYPE zfit0084-buzei,
         lifnr        TYPE zfit0084-lifnr,
         dmbtr        TYPE zfit0084-dmbtr,
         dmbe2        TYPE zfit0084-dmbe2,
         dmbe3        TYPE zfit0084-dmbe3,
         tx_usd       TYPE zfit0084-tx_usd,
         tx_brl       TYPE zfit0084-tx_brl,
         sdo_corr_mi2 TYPE zfit0084-sdo_corr_mi2,
         sdo_corr_mi3 TYPE zfit0084-sdo_corr_mi3,
         vlr_corr_mi2 TYPE zfit0084-vlr_corr_mi2,
         vlr_corr_mi3 TYPE zfit0084-vlr_corr_mi3,
         obj_key      TYPE zfit0084-obj_key,
         obj_key_est  TYPE zfit0084-obj_key_est,
         usnam        TYPE zfit0084-usnam,
         aedat        TYPE zfit0084-aedat,
         cputm        TYPE zfit0084-cputm,
       END OF ty_zfit0084,

       BEGIN OF ty_msg,
         msg TYPE string,
       END OF ty_msg,

       BEGIN OF ty_tmp,
         line(150) TYPE c,
       END OF ty_tmp.
DATA: it_msg TYPE STANDARD TABLE OF ty_msg INITIAL SIZE 0.
DATA: wa_msg TYPE ty_msg.
DATA: it_dados TYPE STANDARD TABLE OF ty_tmp INITIAL SIZE 0.
DATA: it_header TYPE STANDARD TABLE OF ty_tmp INITIAL SIZE 0.
DATA: wa_header TYPE ty_tmp.
DATA: it_ZFIT0084 TYPE STANDARD TABLE OF ty_zfit0084 INITIAL SIZE 0.
DATA: wa_ZFIT0084 TYPE ty_zfit0084.
DATA: wa_ZFIT0084_full TYPE zfit0084.

FIELD-SYMBOLS: <fs_structure> TYPE any,
               <fs_values>    TYPE any,
               <fs_dados>     TYPE any,
               <fs_field>     TYPE dfies,
               <fs_header>    TYPE any.

" Specify the file path
lv_file_path = 'C:\Amaggi\ZFIT0084.csv'.

" Upload the file
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    filename                 = lv_file_path
  TABLES
    data_tab                 = lt_data[]
  EXCEPTIONS
    file_open_error          = 1
    file_read_error          = 2
    no_batch                 = 3
    gui_refused_filetransfer = 4
    invalid_type             = 5
    no_authority             = 6
    OTHERS                   = 7.
IF sy-subrc <> 0.
  MESSAGE 'Error uploading file' TYPE 'E'.
  RETURN.
ELSE.

*  DATA: lt_fieldcat TYPE TABLE OF dfies,
*        lt_columns  TYPE TABLE OF string,
*        lv_zfit0084 TYPE tabname VALUE 'zfit0084'.
*
*  CALL FUNCTION 'DDIF_FIELDINFO_GET'
*    EXPORTING
*      tabname   = lv_zfit0084
*    TABLES
*      dfies_tab = lt_fieldcat
*    EXCEPTIONS
*      not_found = 1
*      OTHERS    = 2.
*
*  IF sy-subrc = 0.
*
*  ENDIF.

  LOOP AT lt_data[] ASSIGNING FIELD-SYMBOL(<insert_table>).
    IF sy-tabix = 1.
      SPLIT <insert_table> AT ';' INTO TABLE it_header.

    ELSE.
      SPLIT <insert_table> AT ';' INTO TABLE it_dados.
      LOOP AT it_dados ASSIGNING <fs_dados>.
        READ TABLE it_header ASSIGNING <fs_header> INDEX sy-tabix.
        CONDENSE <fs_dados> NO-GAPS.
        CASE sy-tabix.
          WHEN  1 .
            wa_zfit0084-mandt = <fs_dados>.
          WHEN  2 .
            wa_zfit0084-bukrs = <fs_dados>.
          WHEN  3 .
            wa_zfit0084-mes_ano  = <fs_dados>.
          WHEN  4 .
            wa_zfit0084-saknr = <fs_dados>.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_zfit0084-saknr
              IMPORTING
                output = wa_zfit0084-saknr.
          WHEN  5 .
            wa_zfit0084-belnr = <fs_dados>.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_zfit0084-belnr
              IMPORTING
                output = wa_zfit0084-belnr.
          WHEN  6 .
            wa_zfit0084-buzei = <fs_dados>.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_zfit0084-buzei
              IMPORTING
                output = wa_zfit0084-buzei.
          WHEN  7 .
            wa_zfit0084-lifnr = <fs_dados>.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_zfit0084-lifnr
              IMPORTING
                output = wa_zfit0084-lifnr.
          WHEN  8 .
            REPLACE ALL OCCURRENCES OF '.' IN  <fs_dados> WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN <fs_dados> WITH '.'.
            wa_zfit0084-dmbtr = <fs_dados>.
          WHEN  9 .
            REPLACE ALL OCCURRENCES OF '.' IN  <fs_dados> WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN <fs_dados> WITH '.'.
            wa_zfit0084-dmbe2 = <fs_dados>.
          WHEN  10  .
            REPLACE ALL OCCURRENCES OF '.' IN  <fs_dados> WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN <fs_dados> WITH '.'.
            wa_zfit0084-dmbe3 = <fs_dados>.
          WHEN  11  .
            REPLACE ALL OCCURRENCES OF '.' IN  <fs_dados> WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN <fs_dados> WITH '.'.
            wa_zfit0084-tx_usd = <fs_dados>.
          WHEN  12  .
            REPLACE ALL OCCURRENCES OF '.' IN  <fs_dados> WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN <fs_dados> WITH '.'.
            wa_zfit0084-tx_brl = <fs_dados>.
          WHEN  13  .
            REPLACE ALL OCCURRENCES OF '.' IN  <fs_dados> WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN <fs_dados> WITH '.'.
            wa_zfit0084-sdo_corr_mi2 = <fs_dados>.
          WHEN  14  .
            REPLACE ALL OCCURRENCES OF '.' IN  <fs_dados> WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN <fs_dados> WITH '.'.
            wa_zfit0084-sdo_corr_mi3 = <fs_dados>.
          WHEN  15  .
            REPLACE ALL OCCURRENCES OF '.' IN  <fs_dados> WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN <fs_dados> WITH '.'.
            wa_zfit0084-vlr_corr_mi2 = <fs_dados>.
          WHEN  16  .
            REPLACE ALL OCCURRENCES OF '.' IN  <fs_dados> WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN <fs_dados> WITH '.'.
            wa_zfit0084-vlr_corr_mi3 = <fs_dados>.
          WHEN  17  .
            wa_zfit0084-obj_key = <fs_dados>.
          WHEN  18  .
            wa_zfit0084-obj_key_est = <fs_dados>.
          WHEN  19  .
            wa_zfit0084-USNAm = <fs_dados>.
          WHEN  20  .
            CLEAR: ano,mes,dia,hrs,mm,ss,hr,dt.
            dt = <fs_dados>+0(10).
            REPLACE ALL OCCURRENCES OF '/' IN dt WITH ''.
            REPLACE ALL OCCURRENCES OF '-' IN dt WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN dt WITH ''.
            ano = dt+0(4).
            mes = dt+4(2).
            dia = dt+6(2).
            wa_zfit0084-aedat = |{ ano }{ mes }{ dia }|.
          WHEN  21  .
            CLEAR: ano,mes,dia,hrs,mm,ss,hr,dt.
            hrs = <fs_dados>+0(8).
            REPLACE ALL OCCURRENCES OF ':' IN hrs WITH ''.
            hr = hrs+0(2).
            mm = hrs+2(2).
            ss = hrs+4(2).
            wa_zfit0084-cputm = |{ hr }{ mm }{ ss }|.
        ENDCASE.

      ENDLOOP.

      APPEND wa_zfit0084 TO it_zfit0084.

      CLEAR: wa_zfit0084.
*      LOOP AT lt_fieldcat ASSIGNING <fs_field> WHERE fieldname = <fs_header>.
*        "APPEND <fs_field>-fieldname TO lt_columns.
*        ASSIGN COMPONENT <fs_header> OF STRUCTURE <fs_field>-fieldname TO <fs_field>.
*        IF sy-subrc = 0.
*          BREAK-POINT.
*        ENDIF.
*      ENDLOOP.
*
*      LOOP AT it_dados ASSIGNING <fs_dados>.
*
*
*        "ASSIGN COMPONENT lv_column OF STRUCTURE <fs_wa> TO <fs_col>.
*
*      LOOP AT it_header ASSIGNING <fs_structure>.
*
*  ASSIGN COMPONENT |{ <fs_header> }| OF STRUCTURE <fs_structure> TO <fs_field>.
*  IF sy-subrc = 0.
*  <fs_field> = <fs_dados>.
*ENDIF.
*  ENDLOOP.
*      ENDLOOP.
      "APPEND it_ZFIT0084 TO it_ZFIT0084[].

    ENDIF.
  ENDLOOP.

  delete it_zfit0084 WHERE mandt is INITIAL.

  CLEAR:wa_zfit0084,wa_msg.

  LOOP AT it_zfit0084 INTO wa_zfit0084.
    MOVE-CORRESPONDING wa_zfit0084 TO wa_zfit0084_full.
    MODIFY zfit0084 FROM wa_zfit0084_full.
    IF sy-subrc <> 0.
      wa_msg-msg = | Falha ao gravar linha { sy-tabix }|.
      APPEND wa_msg TO it_msg.
    ELSE.
      wa_msg-msg = | Sucesso ao gravar linha { sy-tabix }|.
      APPEND wa_msg TO it_msg.
    ENDIF.
    CLEAR:wa_msg,wa_zfit0084,wa_zfit0084_full.
  ENDLOOP.

  IF it_msg IS NOT INITIAL.
    cl_demo_output=>display( it_msg ).
  ENDIF.

ENDIF.
