
tables: t001, Zsdt0370.

data: it_screen_status type table of sy-ucomm.

types: begin of ty_celltab,
         fieldname type  lvc_fname,
         style     type  lvc_style,
         style2    type  lvc_style,
         style3    type  lvc_style,
         style4    type  lvc_style,
         maxlen    type  int4,
       end of ty_celltab.

types: begin of  ty_zsdt0370.
         include structure zsdt0370.
types:   aprocc  type icon-name,
         celltab type  lvc_t_styl,
       end of  ty_zsdt0370.

data: it_saida          type standard table of ty_zsdt0370 initial size 0,
      it_saida2         type standard table of ty_zsdt0370 initial size 0,
      wa_saida          type ty_zsdt0370,
      it_zsdt0370       type table of Zsdt0370,
      wa_zsdt0370       type Zsdt0370,
      lt_modify_celltab type standard table of ty_celltab initial size 0.

data: o_alv  type ref to cl_salv_table,
      t_salv type standard table of ref to cl_salv_table.

data: retorn_msg type char72.
*SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
*  SELECT-OPTIONS: p_bukrs   FOR t001-bukrs  NO-EXTENSION NO INTERVALS. "OBLIGATORY
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


start-of-selection.

  lt_modify_celltab = value #(
( fieldname = 'CH_MODELO' style = '00080000' )
( fieldname = 'AUART' style = '00080000' )
( fieldname = 'VKAUS' style = '00080000' )
( fieldname = 'BRSCH' style = '00080000' )
( fieldname = 'UF_CENTRO' style = '00080000' )
( fieldname = 'UF_CLIENTE' style = '00080000' )
( fieldname = 'CITYC' style = '00080000' )
( fieldname = 'MWSK1' style = '00080000' )
( fieldname = 'OWNPR' style = '00080000' )
( fieldname = 'SHTYP' style = '00080000' )
( fieldname = 'TDLNR' style = '00080000' )
( fieldname = 'BUKRS_TOMA' style = '00080000' )
( fieldname = 'BUKRS_EMIT' style = '00080000' )
( fieldname = 'MTORG' style = '00080000' )
( fieldname = 'MATNR' style = '00080000' )
( fieldname = 'MATKL' style = '00080000' )
( fieldname = 'EXTWG' style = '00080000' )
( fieldname = 'STEUC' style = '00080000' )
( fieldname = 'J_1BTXSDC' style = '00080000' )
( fieldname = 'J_1BTAXLW1' style = '00080000' )
( fieldname = 'J_1BTAXLW2' style = '00080000' )
( fieldname = 'J_1BTAXLW4' style = '00080000' )
( fieldname = 'J_1BTAXLW5' style = '00080000' )
).

  free: it_saida2,it_saida.
  import it_saida2 from memory id 'ZSDI0023'.
  "Importando do programa ZSDI0022 para ZSDI0023.

  if it_saida2 is not initial.
    clear: wa_saida.
    loop at it_saida2 assigning field-symbol(<fs_saida2>).
      move-corresponding <fs_saida2> to wa_saida.

      append wa_saida to it_saida assigning field-symbol(<_put_celltab>).
      <_put_celltab>-celltab = lt_modify_celltab."lt_celltab.
      clear: wa_saida.
    endloop.
    sort it_saida ascending.
    free memory id 'ZSDI0023'.
    free: it_saida2.
  endif.

  call selection-screen '0100'.
