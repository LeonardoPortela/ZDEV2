*&---------------------------------------------------------------------*
*& Include          ZFIR0123_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_busca_dados_bancarios
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_processa_comprovantes .

  TYPES: BEGIN OF ty_retorno,
           data       TYPE zfie_retorno_comprovantes2,
           pagination TYPE zfie_paginacao,
         END OF ty_retorno.

  DATA: lv_info_request TYPE string,
        lv_string       TYPE string,
        lt_retorno      TYPE ty_retorno,
        lv_conta        TYPE char8,
        lt_zfit0091     TYPE TABLE OF zfit0091,
        lv_total_page   TYPE int4,
        lv_page         TYPE int4,
        lv_cnpj         TYPE zfit0091-cnpj_cpf_forn.

  SELECT *
    FROM zfit0228
    INTO TABLE @DATA(lt_0228).
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Falta cadastrar parâmetros na tabela ZFIT0228!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.

  ELSE.

    SELECT bukrs,hbkid,bankl
      FROM t012
      INTO TABLE @DATA(lt_agencia)
      FOR ALL ENTRIES IN @lt_0228
      WHERE bukrs = @lt_0228-empresa
        AND hbkid = @lt_0228-banco.
    IF sy-subrc IS INITIAL.
      SORT lt_agencia BY bukrs hbkid.
    ENDIF.

  ENDIF.

  LOOP AT lt_0228 ASSIGNING FIELD-SYMBOL(<fs_0228>).

    CLEAR: lv_total_page,
           lv_page.

    REPLACE ALL OCCURRENCES OF '/' IN <fs_0228>-conta WITH ' '.
    REPLACE ALL OCCURRENCES OF '.' IN <fs_0228>-conta WITH ' '.
    REPLACE ALL OCCURRENCES OF '-' IN <fs_0228>-conta WITH ' '.

    CONDENSE <fs_0228>-conta NO-GAPS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_0228>-conta
      IMPORTING
        output = lv_conta.

    DO .
      IF lv_total_page > 0.
        ADD 1 TO lv_page.
        lv_info_request =  <fs_0228>-agencia && ';' && lv_conta && ';' && <fs_0228>-cnpj && ';' && lv_page.
      ELSE.
        CONCATENATE <fs_0228>-agencia ';' lv_conta ';' <fs_0228>-cnpj INTO lv_info_request.
      ENDIF.

      TRY .
          zcl_int_ob_cons_comprovantes=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lv_info_request IMPORTING e_integracao = DATA(r_response) ).

        CATCH zcx_integracao INTO DATA(lo_integ).
          MESSAGE lo_integ->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.

        CATCH zcx_error INTO DATA(lo_error).
          MESSAGE lo_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.

      ENDTRY.

      IF r_response-ds_data_retorno IS NOT INITIAL.

        lv_string = r_response-ds_data_retorno.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_string CHANGING data = lt_retorno ).

        IF lv_page IS INITIAL AND lt_retorno-pagination-totalpages > 0.
          lv_total_page = lt_retorno-pagination-totalpages.
        ENDIF.

        IF lt_retorno IS NOT INITIAL.

          LOOP AT lt_retorno-data-itens ASSIGNING FIELD-SYMBOL(<fs_retorno>).
            APPEND INITIAL LINE TO lt_zfit0091 ASSIGNING FIELD-SYMBOL(<fs_zfit0091>).

            <fs_zfit0091>-bukrs          = <fs_0228>-empresa.
            <fs_zfit0091>-augbl          = <fs_retorno>-referencia_empresa.
            <fs_zfit0091>-recusado       = COND #( WHEN <fs_retorno>-status NE 'Efetuado' THEN abap_true ELSE abap_false ). "// BUG-187271 04/08/25 WBABROSA
            <fs_zfit0091>-motivo         = <fs_retorno>-motivo. "// BUG-187271 04/08/25 WBABROSA

            TRY.
                <fs_zfit0091>-buzei          = <fs_retorno>-referencia_empresa+10(3).
              CATCH cx_root.
            ENDTRY.

            <fs_zfit0091>-archive        = <fs_0228>-banco && <fs_retorno>-numero_lote && <fs_retorno>-numero_lancamento.
            <fs_zfit0091>-cnpj_empresa   = <fs_0228>-cnpj.

            READ TABLE lt_agencia ASSIGNING FIELD-SYMBOL(<fs_agencia>)
            WITH KEY bukrs = <fs_0228>-empresa
                     hbkid = <fs_0228>-banco
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_zfit0091>-banco          = <fs_agencia>-bankl(3).
            ENDIF.

            <fs_zfit0091>-agencia        = <fs_0228>-agencia.
            <fs_zfit0091>-conta_corrente = <fs_0228>-conta.
            REPLACE ALL OCCURRENCES OF '-' IN <fs_retorno>-data_pagamento WITH space.
            CONDENSE <fs_retorno>-data_pagamento NO-GAPS.
            <fs_zfit0091>-dt_pgto        = <fs_retorno>-data_pagamento.
            <fs_zfit0091>-vlr_pgto       = <fs_retorno>-valor_pagamento.
            <fs_zfit0091>-cnpj_cpf_forn  = <fs_retorno>-cpf_cnpj.

**** Seleciona Fornecedor - Inicio - CBRAND  - 187408
*            PERFORM f_seleciona_fornecedor IN PROGRAM zfis31
*                                           USING
*                                           <fs_zfit0091>-bukrs
*                                           <fs_zfit0091>-augbl
*                                           <fs_zfit0091>-dt_pgto
*                                           <fs_zfit0091>-lifnr
*                                           lv_cnpj.

            IF NOT <fs_zfit0091>-augbl IS INITIAL.
              SELECT SINGLE lifnr
                FROM bsak
                INTO <fs_zfit0091>-lifnr
               WHERE bukrs = <fs_zfit0091>-bukrs
                 AND belnr = <fs_zfit0091>-augbl
                 AND augdt = <fs_zfit0091>-dt_pgto.

              SELECT SINGLE *
                FROM lfa1
                INTO @DATA(wl_lfa1)
               WHERE lifnr = @<fs_zfit0091>-lifnr.

              IF ( wl_lfa1-stkzn = abap_true ).
                lv_cnpj = wl_lfa1-stcd2.
              ELSE.
                lv_cnpj = wl_lfa1-stcd1.
              ENDIF.

            ELSE.
              "//CNPJ
              IF strlen( lv_cnpj ) > 11.
                SELECT SINGLE lifnr
                  FROM lfa1
                  INTO <fs_zfit0091>-lifnr
                 WHERE stcd1 = lv_cnpj.

                "//CPF
              ELSE.
                SELECT SINGLE lifnr
                  FROM lfa1
                  INTO <fs_zfit0091>-lifnr
                 WHERE stcd2 = lv_cnpj.
              ENDIF.
            ENDIF.
            CLEAR: wl_lfa1.
**** Seleciona Fornecedor - Fim - CBRAND  - 187408

            <fs_zfit0091>-ag_fav         = <fs_retorno>-numero_agencia.
            CONDENSE <fs_retorno>-numero_conta NO-GAPS.
            <fs_zfit0091>-cc_fav         = <fs_retorno>-numero_conta.
            <fs_zfit0091>-cod_autent     = <fs_retorno>-id_pagamento.
          ENDLOOP.

          IF lv_page >= lv_total_page.
            EXIT.
          ENDIF.

        ENDIF.

      ELSE.

        EXIT.

      ENDIF.

    ENDDO.

  ENDLOOP.

  IF lt_zfit0091 IS NOT INITIAL.
    MODIFY zfit0091 FROM TABLE lt_zfit0091.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.

      MESSAGE 'Comprovantes processados com sucesso!' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.
