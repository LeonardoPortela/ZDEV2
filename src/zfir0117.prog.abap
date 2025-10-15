**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Carolini Santos ( carolini.santos@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| JOB DE RECUPERAÇÃO DE BOLETOS                                             |*
**/===========================================================================\*

REPORT zfir0117.

DATA: gt_status      TYPE zde_btcstatus_t,
      gs_itau_boleto TYPE zde_in_itau_boleto,
      lv_guid        TYPE guid,
      lv_nosso_nro   TYPE zfit0056-nosso_nro.

START-OF-SELECTION.

*---------------------------------------------
* Se tem Job ativo, abandona
*---------------------------------------------
  IF sy-batch = abap_true.
    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = sy-cprog
            i_sdldate    = sy-datum
            i_status     = gt_status
          IMPORTING
            e_quantidade = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd > 1.
      EXIT.
    ENDIF.
  ENDIF.

  TRY.
      lv_guid = cl_system_uuid=>create_uuid_x16_static( ).
    CATCH cx_uuid_error INTO DATA(/afl/oref).
  ENDTRY.

*** Inicio - Rubenilson Pereira - 21.07.25
  SELECT *
    FROM zsdt0159
    INTO TABLE @DATA(lt_0159)
    WHERE id_transacao_financeira <> @space
      AND data_atual >= '20250401'.
  IF sy-subrc IS INITIAL.
    DATA(lt_0159_aux) = lt_0159.
    SORT lt_0159_aux BY id_transacao_financeira.
    DELETE ADJACENT DUPLICATES FROM lt_0159_aux COMPARING id_transacao_financeira.

    SELECT *
      FROM zfit0056
      INTO TABLE @DATA(lt_0056)
      FOR ALL ENTRIES IN @lt_0159_aux
      WHERE nosso_nro = @lt_0159_aux-id_transacao_financeira(20).
    IF sy-subrc IS INITIAL.
      SORT lt_0056 BY nosso_nro.

      DATA(lt_0056_aux) = lt_0056.
      SORT lt_0056_aux BY cod_arq.
      DELETE ADJACENT DUPLICATES FROM lt_0056_aux COMPARING cod_arq.

      SELECT *
        FROM zfit0054
        INTO TABLE @DATA(lt_0054)
        FOR ALL ENTRIES IN @lt_0056_aux
        WHERE cod_arq = @lt_0056_aux-cod_arq.
      IF sy-subrc IS INITIAL.
        SORT lt_0054 BY cod_arq.

        LOOP AT lt_0159_aux ASSIGNING FIELD-SYMBOL(<fs_0059>).

          READ TABLE lt_0056 ASSIGNING FIELD-SYMBOL(<fs_0056>)
          WITH KEY nosso_nro = <fs_0059>-id_transacao_financeira(20)
                   cod_retorno = '06' BINARY SEARCH. "CBRAND - 14.10.2025 ( Add 06 - Para pagamentos que foram feitos e não foram lançados 0159).
          IF sy-subrc IS INITIAL.
            "READ TABLE lt_0054 TRANSPORTING NO FIELDS "CBRAND - 14.10.2025.
            READ TABLE lt_0054  ASSIGNING FIELD-SYMBOL(<fs_0054>)
            WITH KEY cod_arq = <fs_0056>-cod_arq
            BINARY SEARCH.
            IF ( sy-subrc IS NOT INITIAL ) OR ( <fs_0054>-origem_receb IS INITIAL ). "CBRAND 14.10.2025 ( Add Origem recebimento para buscar novamente).
              DATA(lv_nao_encontrou) = abap_true.
              lv_nosso_nro = <fs_0056>-nosso_nro.
            ENDIF.
          ELSE.
            lv_nao_encontrou = abap_true.
            lv_nosso_nro = <fs_0059>-id_transacao_financeira(20).
          ENDIF.

          IF lv_nao_encontrou IS NOT INITIAL.
            CLEAR lv_nao_encontrou.
*** Fim - Rubenilson Pereira - 21.07.25
            gs_itau_boleto =
                 VALUE #(
           id_beneficiario = '293800527831'
           codigo_carteira = '109' "Rubenilson Pereira - 21.07.25
           nosso_numero = lv_nosso_nro"Rubenilson Pereira - 21.07.25
           correlationid   = lv_guid
         ).

            CALL METHOD zcl_fi_utils=>run_api_itau
              EXPORTING
                i_itau_boleto = gs_itau_boleto.

            CLEAR lv_nosso_nro.
*** Inicio - Rubenilson Pereira - 21.07.25
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
*** Fim - Rubenilson Pereira - 21.07.25

*** AJUSTE - DT_CREDITO - CBRAND - 05.08.2025 - Inicio
* Na 54 pega tudo marcado como PIX
  DATA: ls_itau_boleto TYPE zde_in_itau_boleto,
        c_retorno_itau TYPE zde_retorno_itau,
        lva_dtcred     TYPE sy-datum.

  lva_dtcred = sy-datum - 60. "Aumentamos de 10 para 60 dias.

  SELECT *
    FROM zfit0054
   INTO TABLE @DATA(lt_zfit0054)
    WHERE api = 'X'
      AND origem_receb = 'PIX'.
  IF sy-subrc IS INITIAL.
    SELECT *
      FROM zfit0056
    INTO TABLE @DATA(lt_zfit0056)
      FOR ALL ENTRIES IN @lt_zfit0054
        WHERE cod_arq = @lt_zfit0054-cod_arq
          AND cod_retorno = '06'.
    IF sy-subrc IS INITIAL.

      SELECT *
        FROM zfit0057
      INTO TABLE @DATA(lt_zfit0057)
        FOR ALL ENTRIES IN @lt_zfit0056
          WHERE cod_arq = @lt_zfit0056-cod_arq
            AND dt_credito >= @lva_dtcred.

      IF sy-subrc IS INITIAL.
*** Verificar a data de crédito.
* Passar o NOSSO_NRO passar na API
        LOOP AT lt_zfit0057 INTO DATA(ls_zfit0057).
          CLEAR: lv_guid.
          READ TABLE lt_zfit0056 INTO DATA(ls_zfit0056) WITH KEY cod_arq = ls_zfit0057-cod_arq.
          IF sy-subrc = 0.
            TRY.
                lv_guid = cl_system_uuid=>create_uuid_x16_static( ).
              CATCH cx_uuid_error INTO /afl/oref.
            ENDTRY.

            ls_itau_boleto =
              VALUE #(
                       id_beneficiario = '293800527831' "PIX
                       nosso_numero = ls_zfit0056-nosso_nro
                       correlationid = lv_guid
           ).

* "// Chama a API de Boleto
            CALL METHOD zcl_fi_utils=>get_boletos_itau
              EXPORTING
                i_request      = ls_itau_boleto
              IMPORTING
                e_retorno_itau = DATA(e_response_boleto).

            CHECK e_response_boleto-data IS NOT INITIAL.

            LOOP AT e_response_boleto-data INTO DATA(ls_dados).
              READ TABLE ls_dados-dado_boleto-dados_individuais_boleto INTO DATA(lwa_boleto) INDEX 1.

              IF lwa_boleto-situacao_geral_boleto = 'Baixada'.
                READ TABLE ls_dados-dado_boleto-pagamentos_cobranca INTO DATA(lwa_pgto) INDEX 1.
                "SPLIT lwa_pgto-data_inclusao_pagamento AT 'T' INTO DATA(lv_dt_credito) DATA(lv_time).
                SPLIT ls_dados-dado_boleto-baixa-data_inclusao_alteracao_baixa AT 'T' INTO DATA(lv_dt_credito) DATA(lv_time).
                REPLACE ALL OCCURRENCES OF '-' IN lv_dt_credito WITH ''.
                IF lv_dt_credito IS NOT INITIAL.
                  IF ls_zfit0057-dt_credito <> lv_dt_credito.
                    UPDATE zfit0057
                        SET dt_credito = lv_dt_credito
                    WHERE cod_arq EQ ls_zfit0057-cod_arq.
                    IF sy-subrc IS INITIAL.
                      COMMIT WORK.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
          CLEAR: ls_zfit0057, ls_zfit0056, ls_itau_boleto, e_response_boleto, c_retorno_itau, ls_dados, lwa_boleto, lwa_pgto, lv_dt_credito, lv_time.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
*** AJUSTE - DT_CREDITO - CBRAND - 05.08.2025 - Fim
