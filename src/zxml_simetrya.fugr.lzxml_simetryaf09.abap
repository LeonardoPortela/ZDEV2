*----------------------------------------------------------------------*
***INCLUDE LZXML_SIMETRYAF09.
*----------------------------------------------------------------------*
*{   INSERT         DEVK9A22VX                                        1
*&---------------------------------------------------------------------*
*& Form fs_valida_exc_quantidades
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*-------------------------------------------------------------------------------------
*-US 128284-28-06-2024-#128284-RJF-inicio
FORM fs_valida_exc_quantidades using p_chave_nfe type ZDE_CHAVE_DOC_E.

   DATA: lv_quantidade_conv TYPE j_1bnetqty,
         lv_conf            TYPE j_1bnetqty,
         lv_und_trib        TYPE char03,
         lv_und_comerci     TYPE char03.


  TRY.
      SELECT *
        UP TO 1 ROWS
       INTO @DATA(wa_zib_nfe_dist_itm)
        FROM zib_nfe_dist_itm            " Itens da Nota Fiscal Eletrônica
        WHERE chave_nfe EQ @p_chave_nfe.
      ENDSELECT.


      " Só processar se TO, TON e KG (Comercial e Tributável)
      CHECK ( sy-subrc IS INITIAL
     AND ( wa_zib_nfe_dist_itm-prod_und_trib    EQ 'TO' OR wa_zib_nfe_dist_itm-prod_und_trib    EQ 'TON' OR wa_zib_nfe_dist_itm-prod_und_trib    EQ 'KG' )
     AND ( wa_zib_nfe_dist_itm-prod_und_comerci EQ 'TO' OR wa_zib_nfe_dist_itm-prod_und_comerci EQ 'TON' OR wa_zib_nfe_dist_itm-prod_und_comerci EQ 'KG' ) ).

      SELECT SINGLE * FROM zmmt0180 " Verifica se tem exceção
        INTO @DATA(wa_zmmt0180)
        WHERE chave_nfe EQ @p_chave_nfe.

*>>>>>>>>>>>>>>>>>Inicio ajuste USER STORY 160673 / AOENNING.
      CLEAR: lv_und_trib, lv_und_comerci.
      SELECT SINGLE LOW FROM TVARVC INTO @DATA(LV_UND)
        WHERE name eq 'ZUN_TRIB_NFE'
        and low EQ @wa_zib_nfe_dist_itm-prod_und_trib.
      "Verificar STVARV UND_TRIBUTADA_NFE.
      if SY-SUBRC EQ 0.
        lv_und_trib = 'TO'.
      ELSE.
        lv_und_trib = wa_zib_nfe_dist_itm-prod_und_trib.
      ENDIF.
      CONDENSE lv_und_trib NO-GAPS.

      CLEAR: LV_UND.
      SELECT SINGLE LOW FROM TVARVC INTO LV_UND
      WHERE name eq 'ZUN_TRIB_NFE'
        and low EQ wa_zib_nfe_dist_itm-prod_und_comerci.
      "Verificar STVARV UND_TRIBUTADA_NFE.
      if SY-SUBRC EQ 0.
        lv_und_comerci = 'TO'.
      ELSE.
        lv_und_comerci = wa_zib_nfe_dist_itm-prod_und_comerci.
      ENDIF.
      CONDENSE lv_und_comerci NO-GAPS.

      " Unidade Diferente (Sim)
*      IF wa_zib_nfe_dist_itm-prod_und_comerci NE wa_zib_nfe_dist_itm-prod_und_trib.
       IF lv_und_trib NE lv_und_comerci.
*>>>>>>>>>>>>>>>>>Fim ajuste USER STORY 160673 / AOENNING.

          " Tonelada Tributavel (Sim) - VALIDAR
          IF wa_zib_nfe_dist_itm-prod_und_trib EQ 'TO' OR wa_zib_nfe_dist_itm-prod_und_trib EQ 'TON'.
            lv_quantidade_conv = wa_zib_nfe_dist_itm-prod_qtd_trib * 1000.
            lv_conf = abs( lv_quantidade_conv - wa_zib_nfe_dist_itm-prod_qtd_comerci ).

            IF lv_conf GT wa_zmmt0180-qtde_exc.
              DATA(lv_msg) = abap_true. " se maior mensagem..
            ENDIF.

            " Tonelada Comercial (Sim) - VALIDAR
          ELSEIF wa_zib_nfe_dist_itm-prod_und_comerci EQ 'TO' OR wa_zib_nfe_dist_itm-prod_und_comerci EQ 'TON'.
            lv_quantidade_conv = wa_zib_nfe_dist_itm-prod_qtd_comerci * 1000.
            lv_conf = abs( lv_quantidade_conv - wa_zib_nfe_dist_itm-prod_qtd_trib ).

            IF lv_conf GT wa_zmmt0180-qtde_exc.
              lv_msg = abap_true. " se maior mensagem..
            ENDIF.

          ENDIF.

*-------------------------------------------------------------------------------------
        ELSEIF wa_zib_nfe_dist_itm-prod_und_trib  EQ wa_zib_nfe_dist_itm-prod_und_comerci. " Unidade Igual (Sim) e quantidade Diferente (Sim)
*-------------------------------------------------------------------------------------
          " Unidade Igual (Sim) e quantidade Diferente (Sim)
          IF wa_zib_nfe_dist_itm-prod_qtd_comerci NE wa_zib_nfe_dist_itm-prod_qtd_trib.

            lv_conf = abs( wa_zib_nfe_dist_itm-prod_qtd_comerci - wa_zib_nfe_dist_itm-prod_qtd_trib ).
            IF lv_conf GT wa_zmmt0180-qtde_exc.
              lv_msg = abap_true.
            ENDIF.
          ENDIF.
      ENDIF.

      IF lv_msg IS NOT INITIAL.
        MESSAGE e398(00) WITH  'Quantidade tributada diferente'
                               'de quantidade comercial,'
                               'contatar o fornecedor para emitir'
                               'nova nota.'
                         RAISING error.
        RETURN.
      ENDIF.

    CATCH cx_root.
      MESSAGE e398(00) WITH  'ERROR: Quantidade tributada diferente'
                             'de quantidade comercial,'
                             'contatar o fornecedor para emitir'
                             'nova nota.'
                      RAISING error.
      RETURN.
  ENDTRY.

ENDFORM.
*-US 128284-28-06-2024-#128284-RJF-fim
*-------------------------------------------------------------------------------------
*}   INSERT
