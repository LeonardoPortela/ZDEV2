*----------------------------------------------------------------------*
***INCLUDE LZGSD_BAIXA_COMPRAO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  FREE: t_anexos, l_lines.

  l_obj_key = l_chave.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
    EXPORTING
      classname          = 'ZS_BAIXANF'
      objkey             = l_obj_key
      client             = sy-mandt
    TABLES
      gos_connections    = t_anexos
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3
      OTHERS             = 4.

  DESCRIBE TABLE t_anexos LINES l_lines.

  LOOP AT SCREEN.
    IF screen-name = 'BOTAO_EXIBIR'.
      IF l_lines = 0.
        screen-input  = 0.
      ELSE.
        screen-input  = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

* IF zeditor->container IS INITIAL.
  IF zeditor IS INITIAL.
    CREATE OBJECT zeditor
      EXPORTING
        im_title        = ' '
        im_display_mode = ' '
        im_longtext_tab = t_text.

    CALL METHOD zeditor->start.
  ENDIF.

  SET PF-STATUS 'EFETUA_BAIXA'.
  SET TITLEBAR 'EFETUA_BAIXA'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE ok_code.

    WHEN '&OK'.
      PERFORM f_salvar_baixa.
      CALL METHOD zeditor->free.
      CLEAR zeditor.
      LEAVE TO SCREEN 0.

    WHEN 'ANEXAR'.
      PERFORM f_anexa_doctos.

    WHEN 'EXIBIR'.
      PERFORM f_exibe_doctos.

    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      CALL METHOD zeditor->free.
      CLEAR zeditor.
      LEAVE TO SCREEN 0.

    WHEN '&CANCEL'.
      PERFORM f_elimina_anexos.
      CALL METHOD zeditor->free.
      CLEAR zeditor.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CLEAR ok_code.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_DT_BAIXA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_dt_baixa INPUT.

  CHECK ok_code <> 'EXIT'    AND
        ok_code <> 'CANCEL'  AND
        ok_code <> 'BACK'    AND
        ok_code <> '&CANCEL'.

  IF g_dt_baixa IS INITIAL.
    CLEAR ok_code.
    MESSAGE e024(sd) WITH text-011.
  ENDIF.

  IF g_dt_baixa > sy-datum.
    CLEAR ok_code.
    MESSAGE e024(sd) WITH text-010.
  ENDIF.

  IF g_dt_baixa(4) <> sy-datum(4).
    CLEAR ok_code.
    MESSAGE i024(sd) WITH text-012.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_BAIXAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_baixar INPUT.

  CHECK ok_code <> 'EXIT'    AND
        ok_code <> 'CANCEL'  AND
        ok_code <> 'BACK'    AND
        ok_code <> '&CANCEL'.

  IF g_baixar IS INITIAL.
    CLEAR ok_code.
    MESSAGE e024(sd) WITH text-013.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DROP_DOWN_BOX  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE drop_down_box OUTPUT.

  FREE: t_vrm_values.

  w_vrm_id            = 'G_BAIXAR'.
  w_vrm_value-key     = 'S'.
  w_vrm_value-text    = 'Sim'.
  APPEND w_vrm_value TO t_vrm_values.
  w_vrm_value-key     = 'N'.
  w_vrm_value-text    = 'Não'.
  APPEND w_vrm_value TO t_vrm_values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = w_vrm_id
      values = t_vrm_values.

ENDMODULE.
