
MODULE user_command_1000 INPUT.

  gv_ucomm = sy-ucomm.

  CASE gv_ucomm.
    WHEN 'BACK' OR 'LEAVE'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'VINC'.
      PERFORM f_vincular_conferencia.
    WHEN 'DESV'.
      PERFORM f_desvicular_conferencia.
    WHEN 'ACEITE' OR 'CANC_AC'.
      PERFORM f_aceite_fiscal.
    WHEN 'CONF_AC'.
      PERFORM f_conferir_carga.
    WHEN 'ANU_CONF'.
      PERFORM f_anular_conferencia.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1002 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.

MODULE pbo_1001_config_campos OUTPUT.

  DATA(_embarque_luft_filial)   = zcl_carga_saida_insumos=>get_embarque_luft( EXPORTING i_nro_cg = gv_carga ).
  DATA(_embarque_armazem)       = zcl_carga_saida_insumos=>check_embarque_armazem( EXPORTING i_nro_cg = gv_carga ).

  LOOP AT SCREEN.


    CASE screen-group1 .
      WHEN 'ACT'. "Botões de Aceite Fiscal - Conferencia com Nota

        IF _embarque_luft_filial EQ abap_true or _embarque_armazem eq abap_true.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'CNF'. "Botões de Conferencia - Conferencia sem Nota

        IF _embarque_luft_filial EQ abap_false AND _embarque_armazem eq abap_false.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

    ENDCASE.

  ENDLOOP.
ENDMODULE.
