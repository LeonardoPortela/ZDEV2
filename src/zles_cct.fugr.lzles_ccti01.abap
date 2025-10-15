*----------------------------------------------------------------------*
***INCLUDE LZLES_CCTI01.
*----------------------------------------------------------------------*

MODULE user_command_0122 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF ( entrega_control-modo EQ c_entrega_view ).
        LEAVE TO SCREEN 0.
      ENDIF.

      CALL METHOD obj_alv_0122->check_changed_data.

*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING
*          TITLEBAR              = 'Confirmação'
*          TEXT_QUESTION         = 'Deseja realmente salvar os registros?'
*          TEXT_BUTTON_1         = 'Sim'
*          TEXT_BUTTON_2         = 'Não'
*          DEFAULT_BUTTON        = '1'
*          DISPLAY_CANCEL_BUTTON = ''
*        IMPORTING
*          ANSWER                = VAR_ANSWER
*        EXCEPTIONS
*          TEXT_NOT_FOUND        = 1
*          OTHERS                = 2.
*
*      CHECK VAR_ANSWER EQ '1'.

      LOOP AT it_saida_0122_itm ASSIGNING FIELD-SYMBOL(<fs_saida_0122_itm>) WHERE ck_modify EQ abap_true.
*        <FS_SAIDA_0122_ITM>-PESO_BRUTO_ENTREGUE = <FS_SAIDA_0122_ITM>-PESO_BRUTO_TOTAL.
*        IF <FS_SAIDA_0122_ITM>-DESTINO_COUNTRY IS INITIAL.
*          MESSAGE 'País é um campo obrigatório!' TYPE 'S'.
*          RETURN.
*        ENDIF.
*
*        IF <FS_SAIDA_0122_ITM>-DESTINO_QTDE_UE_EXPORTADA IS INITIAL.
*          MESSAGE 'Quantidade Exportada é um campo obrigatório!' TYPE 'S'.
*          RETURN.
*        ENDIF.
      ENDLOOP.

      "MESSAGE 'Registros salvos com sucesso!' TYPE 'S'.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.


MODULE tab_active_entrega INPUT.

  CASE sy-ucomm.
    WHEN entrega_tb01.
      info_entrega_tab-activetab = entrega_tb01.
    WHEN entrega_tb02.
      info_entrega_tab-activetab = entrega_tb02.
  ENDCASE.

ENDMODULE.


MODULE user_command_0100 INPUT.

  CASE ok_code_0100.
    WHEN entrega_tb01.
      CLEAR: ok_code_0100.
      entrega_dynnr_000 = entrega_0110.
    WHEN entrega_tb02.
      CLEAR: ok_code_0100.
      entrega_dynnr_000 = entrega_0120.
      WHEN c_save.
      CLEAR: ok_code_0100.
      PERFORM f_gravar_entrega.
    WHEN c_cancel.
      CLEAR: ok_code_0100.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0120 INPUT.

ENDMODULE.
