*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_SCREEN
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_werks   FOR zppt0030-werks,
                  s_data    FOR zppt0030-data_proc DEFAULT sy-datum.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

  l_sel_button-icon_id   = icon_activity.
  l_sel_button-icon_text = 'Parametrização'.
  sscrfields-functxt_01  = l_sel_button.

AT SELECTION-SCREEN.

*-CS2022000876-14.09.2022-#90363-JT-inicio
  CASE sy-ucomm.
    WHEN 'FC01'.

      SELECT SINGLE parva INTO l_value FROM usr05
             WHERE bname = sy-uname
               AND parid = 'ZPP_PARJOB'.
      IF sy-subrc NE 0.
        MESSAGE s024(sd) WITH TEXT-150 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      CALL TRANSACTION 'ZPPT0004'.

    WHEN 'FC02'.

      SELECT SINGLE parva INTO l_value FROM usr05
             WHERE bname = sy-uname
               AND parid = 'ZPP_PARJOB'.
      IF sy-subrc NE 0.
        MESSAGE s024(sd) WITH TEXT-150 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      CALL TRANSACTION 'ZPPT0005'.

  ENDCASE.

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
