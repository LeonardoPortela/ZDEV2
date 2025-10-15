*----------------------------------------------------------------------*
***INCLUDE LZGSD_EMBARQUE_FERTI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN '&OK'.
      CLEAR l_ok.

      CALL METHOD g_grid->check_changed_data.

      PERFORM f_valida_dados USING l_ok.

      IF l_ok = abap_true.
        PERFORM f_grava.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*& F_valida dados
*&---------------------------------------------------------------------*
FORM f_valida_dados USING p_ok.

  DATA: l_menge    TYPE ekpo-menge.

  p_ok = abap_true.

  DELETE t_pedido WHERE ebeln IS INITIAL.

  LOOP AT t_pedido INTO w_pedido.

    SELECT SINGLE menge
             INTO l_menge
             FROM ekpo
            WHERE ebeln = w_pedido-ebeln
              AND ebelp = w_pedido-ebelp.

    IF sy-subrc <> 0.
      p_ok = abap_false.
      MESSAGE s000(fb) WITH text-100 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF w_pedido-qtd_disp > l_menge.
      p_ok = abap_false.
      MESSAGE s000(fb) WITH text-101 text-102 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& F_GRAVA
*&---------------------------------------------------------------------*
FORM f_grava.

  DELETE t_pedido WHERE ebeln IS INITIAL.

  SORT t_pedido BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM t_pedido
                        COMPARING ebeln ebelp.

  DELETE FROM zsdt0265 WHERE nro_sol     = g_nro_sol
                         AND seq         = g_seq
                         AND filial_resp = g_filial_resp.

  LOOP AT t_pedido      INTO w_pedido.
    w_zsdt0265-mandt       = sy-mandt.
    w_zsdt0265-nro_sol     = g_nro_sol.
    w_zsdt0265-seq         = g_seq.
    w_zsdt0265-filial_resp = g_filial_resp.
    w_zsdt0265-ebeln       = w_pedido-ebeln.
    w_zsdt0265-ebelp       = w_pedido-ebelp.
    w_zsdt0265-qtd_disp    = w_pedido-qtd_disp.

    MODIFY zsdt0265     FROM w_zsdt0265.
  ENDLOOP.

  IF t_pedido[] IS INITIAL.
    l_ped_imp = abap_false.
  ELSE.
    l_ped_imp = abap_true.
  ENDIF.

  UPDATE zsdt0137 SET ped_imp     = l_ped_imp
                WHERE nro_sol     = g_nro_sol
                  AND seq         = g_seq
                  AND filial_resp = g_filial_resp.

  COMMIT WORK AND WAIT.

ENDFORM.
