
TABLES: t001,t001w.

TYPES: BEGIN OF  ty_znotaimpad.
         INCLUDE STRUCTURE znota_import_ad.
TYPES:   celltab TYPE  lvc_t_styl,
       END OF  ty_znotaimpad,

       BEGIN OF ty_celltab,
         fieldname TYPE  lvc_fname,
         style     TYPE  lvc_style,
         style2    TYPE  lvc_style,
         style3    TYPE  lvc_style,
         style4    TYPE  lvc_style,
         maxlen    TYPE  int4,
       END OF ty_celltab.

DATA: it_screen_status TYPE TABLE OF sy-ucomm.

DATA:
  it_saida          TYPE STANDARD TABLE OF ty_znotaimpad INITIAL SIZE 0,
  it_saida2         TYPE STANDARD TABLE OF znota_import_ad INITIAL SIZE 0,
  lt_modify_celltab TYPE STANDARD TABLE OF ty_celltab INITIAL SIZE 0,
  IT_znotaimpad     TYPE STANDARD TABLE OF znota_import_ad INITIAL SIZE 0,
  wa_znotaimpad     TYPE znota_import_ad,
  wa_saida          TYPE ty_znotaimpad,
  wa_saida2         TYPE znota_import_ad,
  p_tipo            TYPE char1,
  lo_cols_tab       TYPE REF TO cl_salv_columns_table,
  lo_col_tab        TYPE REF TO cl_salv_column_table,
  lo_events         TYPE REF TO cl_salv_events_table,
  docking           TYPE REF TO cl_gui_docking_container,
  container_main    TYPE REF TO cl_gui_custom_container,
  painel_control    TYPE REF TO cl_gui_splitter_container,
  painel1           TYPE REF TO cl_gui_container,
  painel2           TYPE REF TO cl_gui_container,
  lo_cols           TYPE REF TO cl_salv_columns,
  lo_cols_ref       TYPE        salv_t_column_ref,
  lo_cols_list      TYPE REF TO cl_salv_column_list,
  lo_col_list       LIKE LINE OF lo_cols_ref,
  lo_column         TYPE REF TO cl_salv_column,
  ls_ddic_f4_ref    TYPE salv_s_ddic_reference,
  o_alv             TYPE REF TO cl_salv_table.

*SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
*  SELECT-OPTIONS: p_bukrs   FOR t001-bukrs  NO-EXTENSION NO INTERVALS OBLIGATORY,
*                  p_cdesp   FOR t001w-werks  NO-EXTENSION NO INTERVALS.
*SELECTION-SCREEN END OF BLOCK part1.

*SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-002 .
*  PARAMETERS: r1 RADIOBUTTON GROUP rad1,
*              r2 RADIOBUTTON GROUP rad1, "DEFAULT 'X',
*              r3 RADIOBUTTON GROUP rad1.
*SELECTION-SCREEN END OF BLOCK part2.

*SELECTION-SCREEN BEGIN OF BLOCK part3 WITH FRAME TITLE TEXT-003 .
*  PARAMETERS: p_vriant LIKE tline-tdline VISIBLE LENGTH 25.
*SELECTION-SCREEN END OF BLOCK part3.

*AT SELECTION-SCREEN OUTPUT.
*
*  it_screen_status = VALUE #( ( CONV sy-ucomm( '' ) ) ).
*
*  IF sy-dynnr = 1000.
*
*    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
*      EXPORTING
*        p_status  = 'STATUS_1000'
*        p_program = sy-repid
*      TABLES
*        p_exclude = it_screen_status.
*
*  ENDIF.
*
*AT SELECTION-SCREEN.
*  CASE sy-ucomm.
*    WHEN 'ONLI'.
*      LOOP AT SCREEN.
*        IF p_bukrs IS INITIAL.
**          IF screen-name(7) = 'P_BUKRS'.
**            screen-required = '2'.
**            MODIFY SCREEN.
**          ENDIF.
*        ENDIF.
*      ENDLOOP.
*      IF p_bukrs IS INITIAL.
*        MESSAGE 'Empresa é Obrigatorio!' TYPE 'E'.
*        STOP.
*      ENDIF.
*
*      CALL SELECTION-SCREEN '0100'.
*
*    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
*      SET SCREEN 0.
*      LEAVE SCREEN.
*  ENDCASE.


START-OF-SELECTION.

  lt_modify_celltab = VALUE #(
  ( fieldname =	'NR_SEQ_ADICAO'	style = '00080000' )
( fieldname = 'NR_ADICAO'  style = '00080000' )
( fieldname =	'CFABRICANTE'	style = '00080000' )
( fieldname = 'VLR_DESCONTO'  style = '00080000' )
( fieldname =	'NR_PED_COMPRA'	style = '00080000' )
( fieldname = 'NR_PED_COMPRA_IT'  style = '00080000' )
( fieldname =	'NR_DRAWBACK'	style = '00080000' )

).

  FREE: it_saida2,it_saida.
  CLEAR: p_tipo,wa_saida,wa_saida2.
  "Get ZSAPLZNOTA_IMPLM to LZNOTA_IMPORTACAOO22
  IMPORT it_saida2 FROM MEMORY ID 'ZSAPLZNOTA_IMPLM'.
  "Importando do programa ZPMR0091/ZPMR0093 para ZPMR0094.
  IF it_saida2 IS NOT INITIAL.

    CLEAR:wa_saida.
    LOOP AT it_saida2 ASSIGNING FIELD-SYMBOL(<fs_saida2>).

      MOVE-CORRESPONDING <fs_saida2> TO wa_saida.

      APPEND wa_saida TO it_saida ASSIGNING FIELD-SYMBOL(<_put_celltab>).
      <_put_celltab>-celltab = lt_modify_celltab."lt_celltab.

    ENDLOOP.
    SORT it_saida ASCENDING.

    CLEAR: wa_saida.

    DATA: _docnum        TYPE znota_import_ad-docnum,
          _itmnum        TYPE znota_import_ad-itmnum,
          _itdidoc       TYPE znota_import_ad-itdidoc,
          _nr_adicao     TYPE znota_import_ad-nr_adicao,
          _nr_seq_adicao TYPE znota_import_ad-nr_seq_adicao.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

      _docnum = <fs_saida>-docnum.
      _itmnum = <fs_saida>-itmnum.
      _itdidoc = <fs_saida>-itdidoc.

      SELECT SINGLE * FROM znota_import_ad
        WHERE docnum = @_docnum
        AND itmnum = @_itmnum
        AND itdidoc = @_itdidoc
        INTO @DATA(ls_impad).

      IF sy-subrc = 0.
        <fs_saida>-nr_seq_adicao = ls_impad-nr_seq_adicao.
        <fs_saida>-nr_adicao = ls_impad-nr_adicao.
        <fs_saida>-cfabricante = ls_impad-cfabricante.
        <fs_saida>-vlr_desconto = ls_impad-vlr_desconto.
        <fs_saida>-nr_ped_compra = ls_impad-nr_ped_compra.
        <fs_saida>-nr_ped_compra_it = ls_impad-nr_ped_compra_it.
        <fs_saida>-nr_drawback = ls_impad-nr_drawback.
      ENDIF.

    ENDLOOP.

    FREE MEMORY ID 'ZSAPLZNOTA_IMPLM'.

  ENDIF.


  "select * from znota_import_ad WHERE docnum

  CALL SELECTION-SCREEN '0100'.
