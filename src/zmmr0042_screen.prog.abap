*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_SCREEN
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS    : p_painel RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND us1,
                  p_reenvi RADIOBUTTON GROUP g1.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_werks   FOR zppt0030-werks,
                  s_idcot   FOR zppt0030-id_cotton MODIF ID gr1,
                  s_data    FOR zppt0030-data_proc DEFAULT sy-datum.
  PARAMETERS    : p_pend     AS CHECKBOX           MODIF ID gr1.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'
SELECTION-SCREEN FUNCTION KEY 2.  "Will have a function code of 'FC01'

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

*-CS2022000876-14.09.2022-#90363-JT-inicio
  l_sel_button-icon_id   = icon_activity.
  l_sel_button-icon_text = 'Parametrização'.
  sscrfields-functxt_01  = l_sel_button.
* sscrfields-ucomm       = 'FC01'.
  l_sel_button-icon_id   = icon_activity.
  l_sel_button-icon_text = 'Parametrização JOBS'.
  sscrfields-functxt_02  = l_sel_button.
* sscrfields-ucomm       = 'FC02'.
*-CS2022000876-14.09.2022-#90363-JT-fim

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*-CS2022000876-14.09.2022-#90363-JT-inicio
  CASE sy-ucomm.
    WHEN 'FC01'.

*---------------------------------
* --- parametro usuario
*---------------------------------
*---> Migração S4 - 19.07.2023 - MIGNOW
*      CALL FUNCTION 'ISH_USR05_GET'
*        EXPORTING
*          ss_bname         = sy-uname
*          ss_parid         = 'ZPP_PARJOB'
*        IMPORTING
*          ss_value         = l_value
*        EXCEPTIONS
*          parid_not_found  = 1
*          bname_is_initial = 2
*          parid_is_initial = 3
*          OTHERS           = 4.
*
*      IF l_value <> '*'.
*        MESSAGE s024(sd) WITH text-150 DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.

      SELECT SINGLE parva INTO l_value FROM usr05
             WHERE bname = sy-uname
               AND parid = 'ZPP_PARJOB'.
      IF sy-subrc NE 0.
        MESSAGE s024(sd) WITH TEXT-150 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
*<--- Migração S4 - 19.07.2023 - MIGNOW

*---- Parametrização
      CALL TRANSACTION 'ZPPT0004'.

    WHEN 'FC02'.

*---------------------------------
* --- parametro usuario
*---------------------------------
*---> Migração S4 - 19.07.2023 - MIGNOW
*      CALL FUNCTION 'ISH_USR05_GET'
*        EXPORTING
*          ss_bname         = sy-uname
*          ss_parid         = 'ZPP_PARJOB'
*        IMPORTING
*          ss_value         = l_value
*        EXCEPTIONS
*          parid_not_found  = 1
*          bname_is_initial = 2
*          parid_is_initial = 3
*          OTHERS           = 4.
*
*      IF l_value <> '*'.
*        MESSAGE s024(sd) WITH text-150 DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.

      SELECT SINGLE parva INTO l_value FROM usr05
             WHERE bname = sy-uname
               AND parid = 'ZPP_PARJOB'.
      IF sy-subrc NE 0.
        MESSAGE s024(sd) WITH TEXT-150 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
*<--- Migração S4 - 19.07.2023 - MIGNOW

*---- Parametrização
      CALL TRANSACTION 'ZPPT0005'.

  ENDCASE.
*-CS2022000876-14.09.2022-#90363-JT-fim

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'GR1'.
      IF p_painel = abap_true.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZLESR0152'.
  SET TITLEBAR 'ZLESR0152'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'ZLESR0152B'.
  SET TITLEBAR 'ZLESR0152B'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  FREE: t_rows[].

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  CASE ok_code.
    WHEN '&REFRESH'.
      PERFORM f_selecao_dados.
      PERFORM f_processa_dados.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0200 INPUT.

  CASE ok_code.
    WHEN '&REFRESH'.
      PERFORM f_selecao_reenvio.
      PERFORM f_processa_reenvio.

    WHEN 'REENVIO'.
      PERFORM f_reenvio_trace.
      PERFORM f_selecao_reenvio.
      PERFORM f_processa_reenvio.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
