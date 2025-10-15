FUNCTION zsdmf_executa_hedge_new.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_DIRECAO) TYPE  BEZEI30
*"     REFERENCE(IS_POPUP_VALUES) TYPE  ZSDS078
*"     REFERENCE(IS_0090) TYPE  ZSDT0090
*"     REFERENCE(IS_0040) TYPE  ZSDT0040 OPTIONAL
*"     REFERENCE(IV_UCOMM) TYPE  SYUCOMM DEFAULT 'PRICE_NEW'
*"     REFERENCE(IV_CPROG) TYPE  SYCPROG DEFAULT 'ZSDR0042'
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"----------------------------------------------------------------------

*  CHECK 1 EQ 2.
  DATA: vl_trava_cambio  TYPE c,
        vl_orig_trv_cam  TYPE c,
        vl_ov_precedente TYPE vbeln,
        vl_qtde_equal    TYPE c.

  sy-cprog = iv_cprog.

  PERFORM f_sap_indicator USING 'Executando hedge...'.

  CASE iv_direcao.

    WHEN 'D'. " <------------ DEDUÇÃO

      PERFORM f_hedge_deduzir USING is_popup_values.


    WHEN 'T'.
      zcl_webservice_tx_curva=>hedge_insumos( i_numero = is_popup_values-doc_simulacao
                                              i_dir    =  iv_direcao
                                              i_vbeln  = is_popup_values-vbeln
                                              i_tipo   = 'EST'
                                              i_0090_manual = 'X'
                                              i_seq = is_0090-sequencia
                                             ).
    WHEN OTHERS.

      IF is_0040 IS INITIAL.

        SELECT SINGLE * FROM zsdt0040
         INTO @DATA(ls_0040)
          WHERE doc_simulacao EQ @is_popup_values-doc_simulacao.

      ELSE.

        ls_0040 = is_0040.

      ENDIF.

      IF ls_0040-tpsim NE 'PM'.
        IF ls_0040-tpsim NE 'BN'.

          CLEAR: vl_trava_cambio, vl_orig_trv_cam, vl_ov_precedente.
          IF ls_0040-waerk EQ 'USD'.
            CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
              EXPORTING
                i_doc_simulacao     = is_0090-doc_simulacao
                i_vbeln             = is_0090-vbelv
              CHANGING
                c_trava_cambio      = vl_trava_cambio
                c_trava_cambio_prec = vl_orig_trv_cam
                c_ov_precedente     = vl_ov_precedente.
          ENDIF.

          CALL FUNCTION 'ZSDMF001_COMPARE_UNIT_MAT'
            EXPORTING
              i_matnr_01 = is_0090-matnr
              i_menge_01 = is_0090-zmeng
              i_matnr_02 = is_0090-matnrv
              i_menge_02 = is_0090-zmengv
            IMPORTING
              e_equal    = vl_qtde_equal.

          IF ( ls_0040-waerk EQ 'BRL' ) OR

             ( ( ls_0040-waerk EQ 'USD' ) AND
               ( vl_trava_cambio IS NOT INITIAL ) AND
               ( iv_direcao NE 'P' ) ).

            CASE iv_direcao.
              WHEN 'A' OR 'E' OR 'O'.
                zcl_webservice_tx_curva=>hedge_insumos( i_acao   = iv_ucomm
                                                        i_0090 = is_0090
                                                        i_tipo   = 'VDI'
                                                       ).

              WHEN OTHERS.

                IF ( 'U_M' CS iv_direcao                          ) AND
                   ( is_0090-matklv        EQ is_0090-matkl      ) AND
                   ( is_0090-matklv        NE '658445'           ) AND
                   ( vl_qtde_equal         EQ abap_false         ).
                  "( ABS( p_lanc_0090-ZMENGV ) NE ABS( p_lanc_0090-ZMENG ) ).

                  zcl_webservice_tx_curva=>hedge_insumos( i_acao   = iv_ucomm
                                                          i_0090   = is_0090
                                                          i_tipo   = 'VDI'
                                                          i_dir    = iv_direcao
                                                         ).
                ELSEIF is_0090-matklv NE is_0090-matkl.
                  zcl_webservice_tx_curva=>hedge_insumos( i_acao   = iv_ucomm
                                                          i_0090 = is_0090
                                                          i_tipo   = 'VDI'
                                                         ).
                ELSEIF is_0090-netpr NE is_0090-netprv. " REDISTRIBUIÇÃO
                  IF iv_direcao EQ 'R'.
                    zcl_webservice_tx_curva=>hedge_insumos( i_acao   = iv_ucomm
                                                            i_0090 = is_0090
                                                            i_tipo   = 'VDI'
                                                           ).
                  ENDIF.
                ENDIF.
            ENDCASE.
          ELSEIF ls_0040-waerk EQ 'USD'.

            IF iv_direcao EQ 'P'.

              zcl_webservice_tx_curva=>hedge_insumos( i_0090 = is_0090
                                                      i_acao = iv_ucomm
                                                      i_tipo = 'VDI'
                                                      i_taxa_boleta = is_popup_values-taxa_curv_proj
                                                     ).
              EXIT.
            ENDIF.

          ENDIF.
        ENDIF.

*    DISPARO DO HEDGE PARA FRETE
        CASE iv_direcao.
          WHEN 'A' OR 'E'.
            zcl_webservice_tx_curva=>hedge_insumos( i_acao   = iv_ucomm
                                                    i_0090 = is_0090
                                                    i_tipo   = 'FRI'
                                                   ).
          WHEN 'R'.
*          Dispara o Frete quando a Alteração no Preço e se For Defensivos
            IF is_0090-netpr NE is_0090-netprv. " REDISTRIBUIÇÃO
              IF is_0090-matklv EQ '658445'.
                zcl_webservice_tx_curva=>hedge_insumos( i_acao = iv_ucomm
                                                        i_0090 = is_0090
                                                        i_tipo = 'FRI'
                                                       ).
              ENDIF.
            ENDIF.

            IF is_0090-werks NE is_0090-werksv. " REDISTRIBUIÇÃO
              IF is_0090-matklv NE '658445'.
                zcl_webservice_tx_curva=>hedge_insumos( i_acao = iv_ucomm
                                                        i_0090 = is_0090
                                                        i_tipo = 'FRI'
                                                       ).
              ENDIF.
            ENDIF.
          WHEN 'O'.
*  Desconto ABS
*  Dispara o Frete Somnete para Defensivos
            IF is_0090-matklv EQ '658445'.
              zcl_webservice_tx_curva=>hedge_insumos( i_acao   = iv_ucomm
                                                      i_0090   = is_0090
                                                      i_tipo   = 'FRI'
                                                     ).

            ENDIF.
*         Dispara em caso da Alteração do Incoterms
          WHEN 'F'.
            zcl_webservice_tx_curva=>hedge_insumos( i_acao   = iv_ucomm
                                                    i_0090 = is_0090
                                                    i_tipo   = 'INV'
                                                    i_dir    = iv_direcao
                                                   ).

          WHEN OTHERS. " POR ENQUANTO TROCA DE MATERIAIS

            IF ( 'U_M' CS iv_direcao             ) AND
               ( is_0090-matklv EQ is_0090-matkl ) AND
               ( is_0090-matklv NE '658445'      ) AND
               ( vl_qtde_equal  EQ abap_false    ).
              "( ABS( p_lanc_0090-ZMENGV ) NE ABS( p_lanc_0090-ZMENG ) ).

              zcl_webservice_tx_curva=>hedge_insumos( i_acao   = iv_ucomm
                                                      i_0090   = is_0090
                                                      i_tipo   = 'FRI'
                                                      i_dir    = iv_direcao
                                                     ).

            ELSEIF ( is_0090-matklv NE is_0090-matkl ) OR ( is_0090-inco1v NE is_0090-inco1 ).
              zcl_webservice_tx_curva=>hedge_insumos( i_acao   = iv_ucomm
                                                      i_0090 = is_0090
                                                      i_tipo   = 'FRI'
                                                     ).
            ENDIF.

        ENDCASE.
      ENDIF.

  ENDCASE.


ENDFUNCTION.
