*&---------------------------------------------------------------------*
*&  Include           ZGL016_I01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK-CODE.
    WHEN 'EXIBIR'.
       SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD WG_CABECALHO-LOTE.
      SUBMIT ZGL018   WITH P_BUKRS    = WG_CABECALHO-BUKRS
                      WITH P_LOTE-LOW = WG_CABECALHO-LOTE
      AND RETURN.
    WHEN C_SEARCH.
      PERFORM F_BUSCA_DADOS.
    WHEN C_LIBERAR.
      PERFORM F_LIBERAR.
    WHEN C_REINICIAR.
      PERFORM F_REINICIAR.
    WHEN C_SHOW_MSGRE.
*      PERFORM f_verifica_erros.
*      IF tg_msg_ret[] IS NOT INITIAL.
*        CALL FUNCTION 'Z_DOC_CHECK_NEW'
*          EXPORTING
*            i_screen      = '100'
*            i_show        = c_x
*            i_repid       = sy-repid
*            i_popup       = 0
*            i_pressed_tab = 'TS_100-PRESSED_TAB'
*            i_set_field   = 'X_FIELD'
*            i_set_cell    = 'WG_CELL'
*            i_set_obj     = 'WG_OBJ'
*          IMPORTING
*            e_messagem    = wg_mensagem
*          TABLES
*            it_msgs       = tg_msg_ret.
*      ENDIF.
    WHEN C_BOTAOTESTE.
    WHEN C_CANCEL.
      SET SCREEN 100.
    WHEN C_BACK.
      SET SCREEN 100.
    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
