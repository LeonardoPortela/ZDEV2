*&---------------------------------------------------------------------*
*& Include          ZSDR0060_7211
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_7211 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_7211 OUTPUT.

  CLEAR ok_code_7211.

  SET PF-STATUS 'STATUS_7211'.
  SET TITLEBAR 'TITLE_7211'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_7211  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_7211 INPUT.

  DATA: lv_error TYPE char01.

  CASE ok_code_7211.
    WHEN 'SALVAR'.
      PERFORM f_gravar_ajuste_frete CHANGING lv_error.

      IF lv_error = abap_false.
        gwa_cab_carga_saida-preco_frete = lwa_preco_frete.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'VOLTAR'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR ok_code_7211.

ENDMODULE.

FORM f_gravar_ajuste_frete CHANGING p_erro.

  DATA: lv_header    TYPE zsds382,
        lv_mesg_erro TYPE string.

  FREE: p_erro.

  lv_header-nro_cg                = gwa_cab_carga_saida-nro_cg.
  lv_header-preco_frete           = lwa_preco_frete.

  IF lwa_motivo_ajuste_frete1 IS NOT INITIAL AND
     lwa_motivo_ajuste_frete2 IS NOT INITIAL AND
     lwa_motivo_ajuste_frete3 IS NOT INITIAL.
    lv_header-motivo_ajuste_frete = lwa_motivo_ajuste_frete1 && '|' && lwa_motivo_ajuste_frete2 && '|' && lwa_motivo_ajuste_frete3.
  ENDIF.

  lv_mesg_erro = zcl_carga_saida_insumos=>gravar_ajuste_frete( i_header = lv_header ).

  IF lv_mesg_erro IS NOT INITIAL.
    p_erro = abap_true.
    MESSAGE lv_mesg_erro TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.
