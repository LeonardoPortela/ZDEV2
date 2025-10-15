*----------------------------------------------------------------------*
***INCLUDE MZIMP01_CONFIG_TELAO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CONFIG_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE config_tela OUTPUT.


  IF w_zimp_cabecalho-estorno EQ space AND
      w_zimp_cabecalho-belnr   NE space.
    LOOP AT SCREEN.
      screen-input = c_0.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    CASE sy-tcode.

      WHEN c_tcode_exibir.

        LOOP AT SCREEN.
          screen-input = c_0.
          MODIFY SCREEN.
        ENDLOOP.

      WHEN c_tcode_modificar.

        LOOP AT SCREEN.
          IF screen-name = 'W_ZIMP_CABECALHO-BUKRS'.
* Início Alteração Ricardo Furst 18.07.2009
*           OR
*           screen-name = 'W_ZIMP_CABECALHO-BUDAT'.
* Fim Alteração Ricardo Furst 18.07.2009

            screen-input = c_0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.

    ENDCASE.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name = 'W_ZIMP_CABECALHO-DT_PER_APUR'.
      IF NOT v_imp_dt_apuracao IS INITIAL.
        screen-required = c_1.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF screen-name = 'W_ZIMP_CABECALHO-MES_ANO_COMP'.
      IF NOT v_imp_mes_apuracao IS INITIAL.
        screen-required = c_1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

* OTR. ENT.
  IF w_zimp_cabecalho-tp_arrec = '03'.
    w_cols-invisible = c_0.
  ELSE.
    w_cols-invisible = c_1.
  ENDIF.

  MODIFY tc_detalhes-cols FROM w_cols INDEX 6
    TRANSPORTING invisible.

* JUROS/MULTA
  IF NOT w_zimp_cabecalho-cta_multa IS INITIAL.
    w_cols-invisible = c_0.
  ELSE.
    w_cols-invisible = c_1.
  ENDIF.

*  MODIFY tc_detalhes-cols FROM w_cols INDEX 7
*    TRANSPORTING invisible.

  MODIFY tc_detalhes-cols FROM w_cols INDEX 4
    TRANSPORTING invisible.

* Início Alteração Ricardo Furst
  IF NOT W_ZIMP_CABECALHO-CTA_JUROS IS INITIAL.
    w_cols-invisible = c_0.
  ELSE.
    w_cols-invisible = c_1.
  ENDIF.

  MODIFY tc_detalhes-cols FROM w_cols INDEX 7
    TRANSPORTING invisible.

*  MODIFY tc_detalhes-cols FROM w_cols INDEX 4
*    TRANSPORTING invisible.
* Fim Alteração Ricardo Furst

* Início Alteração Ricardo Furst 20.07.2009
* CONTA TARIFA
  IF NOT w_zimp_cabecalho-cta_tse IS INITIAL.
    w_cols-invisible = c_0.
  ELSE.
    w_cols-invisible = c_1.
  ENDIF.

  MODIFY tc_detalhes-cols FROM w_cols INDEX 8
    TRANSPORTING invisible.

* VALOR ATUAL MONETARIO
  IF NOT w_zimp_cabecalho-cta_at_mon IS INITIAL.
    w_cols-invisible = c_0.
  ELSE.
    w_cols-invisible = c_1.
  ENDIF.

  MODIFY tc_detalhes-cols FROM w_cols INDEX 5
    TRANSPORTING invisible.
* Fim Alteração Ricardo Furst 20.07.2009

* VLR. MONETÁRIO
*  IF w_zimp_cabecalho-tp_arrec = '12'.
*    w_cols-invisible = c_0.
*  ELSE.
*    w_cols-invisible = c_1.
*  ENDIF.
*
*  MODIFY tc_detalhes-cols FROM w_cols INDEX 5
*    TRANSPORTING invisible.

  IF w_zimp_cabecalho-tp_arrec <> '01' AND
     w_zimp_cabecalho-cod_pgto  = '2631'.

    w_cols-screen-required = c_1.

*/ Validação a parte para código do pagamento 2631.
  ELSEIF w_zimp_cabecalho-cod_pgto  = '2631'.

    w_cols-screen-required = c_1.
*/
  ELSE.

    w_cols-screen-required = c_0.

  ENDIF.

  MODIFY tc_detalhes-cols FROM w_cols INDEX 2
    TRANSPORTING screen-required.

** DT APU.
*  IF v_imp_dt_apuracao  = 'X'.
*
*    w_cols-screen-required = c_1.
*
*  ELSE.
*
*    w_cols-screen-required = c_0.
*
*  ENDIF.
*
*  MODIFY w_zimp_cabecalho-dt_per_apur FROM w_cols INDEX 1
*    TRANSPORTING screen-required.

** MES APU
*  IF v_imp_mes_apuracao  = 'X'.
*
*    w_cols-screen-required = c_1.
*
*  ELSE.
*
*    w_cols-screen-required = c_0.
*
*  ENDIF.
*
*  MODIFY w_zimp_cabecalho-dt_per_apur FROM w_cols INDEX 2
*    TRANSPORTING screen-required.

* COD BAR
  IF v_imp_cod_barras IS INITIAL.

    w_cols-screen-required = c_0.

  ELSE.

    w_cols-screen-required = c_1.

  ENDIF.

  MODIFY tc_detalhes-cols FROM w_cols INDEX 10
    TRANSPORTING screen-required.

ENDMODULE.                 " CONFIG_TELA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  IF w_zimp_cabecalho-estorno EQ space AND
     w_zimp_cabecalho-belnr   NE space.
    SET PF-STATUS '0100' EXCLUDING 'SAVE'.
    SET TITLEBAR '0100' WITH 'Exibição de'.
  ELSE.

    IF sy-tcode = c_tcode_exibir.
      SET PF-STATUS '0100' EXCLUDING 'SAVE'.
    ELSE.
      SET PF-STATUS '0100'.
    ENDIF.


    SET TITLEBAR '0100' WITH v_titulo.
  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS '0001'.
  SET TITLEBAR '0100' WITH v_titulo.

ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_TOTAL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atualiza_total OUTPUT.

  v_total = w_zimp_detalhe-vlr_principal  +
            w_zimp_detalhe-vlr_multa      +
            w_zimp_detalhe-vlr_juros      +
* Eduardo
w_zimp_detalhe-vlr_outras_ent +
* Eduardo

* Início Alteração Ricardo Furst 20.07.2009
            w_zimp_detalhe-vlr_atual_mone +
            w_zimp_detalhe-tse.
* Fim Alteração Ricardo Furst 20.07.2009

ENDMODULE.                 " ATUALIZA_TOTAL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CONFIG_IMPOSTO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE config_imposto OUTPUT.

  CHECK NOT sy-tcode = c_tcode_exibir.


  IF w_zimp_cabecalho-estorno EQ space AND
      w_zimp_cabecalho-belnr   NE space.
    CHECK sy-tcode = c_tcode_exibir.
  ENDIF.

*  IF w_zimp_cabecalho-tp_arrec = '02'.
*    LOOP AT SCREEN.
*      IF screen-name = 'W_ZIMP_CABECALHO-MES_ANO_COMP'.
*        screen-input = c_0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
  IF w_zimp_cabecalho-tp_arrec = '01'.
    LOOP AT SCREEN.
      IF screen-name = 'W_ZIMP_CABECALHO-COD_PGTO'.
        screen-input = c_0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.


ENDMODULE.                 " CONFIG_IMPOSTO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CONFIG_TELA_TC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE config_tela_tc OUTPUT.

  IF w_zimp_cabecalho-estorno EQ space AND
      w_zimp_cabecalho-belnr   NE space.
    LOOP AT SCREEN.
      screen-input = c_0.
      MODIFY SCREEN.
    ENDLOOP.

  ELSE.
    CASE sy-tcode.

* Início Alteração Ricardo Furst 20.07.2009
*    WHEN c_tcode_criar.
*
*      LOOP AT SCREEN.
*        IF NOT W_ZIMP_CABECALHO-CTA_TSE IS INITIAL.
*          IF screen-name = 'W_ZIMP_DETALHE-TSE'.
*            screen-input = c_1.
*            screen-INVISIBLE = c_0.
*            screen-active = c_0.
*            MODIFY SCREEN.
*          ENDIF.
*        ELSE.
*          IF screen-name = 'W_ZIMP_DETALHE-TSE'.
*            screen-input = c_0.
*            screen-INVISIBLE = c_0.
*            screen-active = c_0.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
* Fim Alteração Ricardo Furst 20.07.2009

      WHEN c_tcode_exibir.

        LOOP AT SCREEN.
          screen-input = c_0.
          MODIFY SCREEN.
        ENDLOOP.

      WHEN c_tcode_modificar.

if w_zimp_cabecalho-belnr eq space.
          LOOP AT SCREEN.
          IF screen-name = 'W_ZIMP_DETALHE-GSBER'.
            screen-input = c_1.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
  else.
        LOOP AT SCREEN.
          IF screen-name = 'W_ZIMP_DETALHE-GSBER'.
            screen-input = c_0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
endif.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " CONFIG_TELA_TC  OUTPUT
