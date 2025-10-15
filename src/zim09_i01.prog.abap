*&---------------------------------------------------------------------*
*&  Include           ZIM09_I01
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TS 'ABA_SELECAO'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE aba_selecao_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_aba_selecao-tab1.
      g_aba_selecao-pressed_tab = c_aba_selecao-tab1.
    WHEN c_aba_selecao-tab2.
      g_aba_selecao-pressed_tab = c_aba_selecao-tab2.
    WHEN c_aba_selecao-tab3.
      g_aba_selecao-pressed_tab = c_aba_selecao-tab3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "ABA_SELECAO_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0010 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0112  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0112 INPUT.

  PERFORM recupera_valores_112.

  CASE sy-ucomm.
    WHEN 'BLOCK'.

      IF  <r_bukrs2> IS INITIAL OR
          <r_gjahr2> IS INITIAL." OR
*        <r_safra2> is initial.

        MESSAGE i000(z01) WITH 'Preencher todos os EMPRESA' 'e ANO'.

      ELSE.
        PERFORM executa_bloqueio.
      ENDIF.
    WHEN 'CLEAR12'.

      CLEAR: <r_bukrs2>, <r_gjahr2>, <r_safra2>.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0112  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0111  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0111 INPUT.

  PERFORM recupera_valores_111.
  DATA: wl_m,
        wl_n.
  CASE sy-ucomm.
    WHEN 'EXEC'.

      IF  <r_bukrs>    IS INITIAL OR
          <r_dtini>    IS INITIAL OR
          <r_datai>    IS INITIAL OR
          <r_dataf>    IS INITIAL OR
          modelo_exerc IS INITIAL OR
*          modelo_safra IS INITIAL OR
          novo_exerc   IS INITIAL." OR
*          novo_safra   IS INITIAL.
        MESSAGE i000(z01) WITH 'Preencher todos os campos da tela'.
      ELSE.

        CLEAR: wl_m, wl_n.

        IF NOT modelo_safra IS INITIAL.
          wl_m = 'X'.
        ENDIF.

        IF NOT novo_safra IS INITIAL.
          wl_n = 'X'.
        ENDIF.

        IF wl_m NE wl_n.
          MESSAGE i000(z01) WITH 'De/Para SAFRA incorreto!'.
        ELSE.

          PERFORM executa_copia.
        ENDIF.
      ENDIF.
    WHEN 'CLEAR11'.

      CLEAR: <r_bukrs>, <r_dtini>, <r_datai>, <r_dataf>,
      modelo_exerc,
      modelo_safra,
      novo_exerc,
      novo_safra.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0111  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0113  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0113 INPUT.

  PERFORM recupera_valores_113.

  DATA wl_safra2 TYPE zim02_sol_ap_ctl-safra2.

  CASE sy-ucomm.
    WHEN 'ALTER'.

      IF <r_bukrs3> IS INITIAL OR
         <r_gjahr3> IS INITIAL OR
         <r_fase3>  IS INITIAL OR
         <r_datai3> IS INITIAL OR
         <r_dataf3> IS INITIAL OR
         <r_dtini3> IS INITIAL .
        MESSAGE i000(z01) WITH 'Preencher todos os campos da tela'.
      ELSE.


        IF <r_safra3> IS INITIAL.
          CLEAR wl_safra2.
        ELSE.
          wl_safra2 = <r_safra3> + 1.
        ENDIF.

        UPDATE zim02_sol_ap_ctl SET   dt_aprov_in  = <r_datai3>
                                      dt_aprov_fim = <r_dataf3>
                                      dt_inicio    = <r_dtini3>
                                      fase         = <r_fase3>

                                WHERE bukrs  IN <r_bukrs3> AND
                                      ano    EQ <r_gjahr3> AND
                                      safra  EQ <r_safra3> AND
                                      safra2 EQ wl_safra2.

        IF sy-subrc = 0.
          MESSAGE i000(z01) WITH 'Exercício(s) alterado(s)!'.
        ELSE.
          MESSAGE i000(z01) WITH 'Menhum registro encontrado para seleção!'.
        ENDIF.


      ENDIF.

    WHEN 'CLEAR13'.
      CLEAR:
      <r_bukrs3>,
      <r_gjahr3>,
      <r_safra3>,
      <r_fase3>,
      <r_datai3>,
      <r_dataf3>,
      <r_dtini3>.

    WHEN OTHERS.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0113  INPUT
