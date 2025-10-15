
TABLES: t001,t001w.

TYPES: BEGIN OF  ty_zpmr0011_2.
         INCLUDE STRUCTURE zpmr0011.
TYPES:   action TYPE  char10,
       END OF  ty_zpmr0011_2.

TYPES: BEGIN OF  ty_zpmr0011.
         INCLUDE STRUCTURE zpmr0011.
TYPES:   ktext   TYPE cskt-ktext,
         celltab TYPE  lvc_t_styl,
       END OF  ty_zpmr0011,

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
  it_saida          TYPE STANDARD TABLE OF ty_zpmr0011 INITIAL SIZE 0,
  it_saida2         TYPE STANDARD TABLE OF ty_zpmr0011_2 INITIAL SIZE 0,
  it_saida3         TYPE STANDARD TABLE OF ty_zpmr0011_2 INITIAL SIZE 0,
  lt_modify_celltab TYPE STANDARD TABLE OF ty_celltab INITIAL SIZE 0,
  IT_zpmr0011       TYPE STANDARD TABLE OF zpmr0011 INITIAL SIZE 0,
  wa_zpmr0011       TYPE zpmr0011,
  wa_saida          TYPE ty_zpmr0011,
  wa_saida2         TYPE ty_zpmr0011_2,
  wa_saida3         TYPE ty_zpmr0011_2,
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

  FREE: it_saida2.
  CLEAR: p_tipo.
  IMPORT it_saida2 FROM MEMORY ID 'ZPMR0094'.
  "Importando do programa ZPMR0091/ZPMR0093 para ZPMR0094.
  IF it_saida2 IS NOT INITIAL.

    CLEAR:wa_saida.
    LOOP AT it_saida2 ASSIGNING FIELD-SYMBOL(<fs_saida2>).

      CASE <fs_saida2>-action.
        WHEN 'COPY'.
          P_tipo = 'C'.
        WHEN 'EDIT'.
          P_tipo = 'E'.
        WHEN 'INSERT'.
          P_tipo = 'I'.
        WHEN OTHERS.
      ENDCASE.

      IF p_tipo = 'E'.
        lt_modify_celltab = VALUE #(
    ( fieldname = 'PERMIT' style = '00080000' )
    ( fieldname = 'WAERS' style = '00080000' )
    ( fieldname = 'VALOR_DE' style = '00080000' )
    ( fieldname = 'VALOR_ATE' style = '00080000' )
    ( fieldname = 'USUA_SUBST' style = '00080000' )
    ( fieldname = 'DATA_LIM' style = '00080000' )
    ).

      ELSE.
        "AQUI INCLUI AS COLUNAS QUE PODEM SER EDITADAS
        lt_modify_celltab = VALUE #(
      ( fieldname = 'KOSTL' style = '00080000'  )
      ( fieldname = 'NIVEL' style = '00080000' )
      ( fieldname = 'APROVADOR' style = '00080000' )
      ( fieldname = 'PERMIT' style = '00080000' )
      ( fieldname = 'WAERS' style = '00080000' )
      ( fieldname = 'VALOR_DE' style = '00080000' )
      ( fieldname = 'VALOR_ATE' style = '00080000' )
      ( fieldname = 'USUA_SUBST' style = '00080000' )
      ( fieldname = 'DATA_LIM' style = '00080000' )
        ).
      ENDIF.

      MOVE-CORRESPONDING <fs_saida2> TO wa_saida.

      IF wa_saida-kostl IS NOT INITIAL AND ( p_tipo = 'C' OR p_tipo = 'E' ).
        SELECT SINGLE ktext FROM cskt WHERE kostl = @wa_saida-kostl AND spras = 'P' INTO @wa_saida-ktext.
      ENDIF.

      APPEND wa_saida TO it_saida ASSIGNING FIELD-SYMBOL(<_put_celltab>).
      <_put_celltab>-celltab = lt_modify_celltab."lt_celltab.

    ENDLOOP.
    SORT it_saida ASCENDING.

    FREE MEMORY ID 'ZPMR0094'.

  ENDIF.

  CALL SELECTION-SCREEN '0100'.
