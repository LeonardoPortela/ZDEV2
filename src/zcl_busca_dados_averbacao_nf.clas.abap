class ZCL_BUSCA_DADOS_AVERBACAO_NF definition
  public
  final
  create public .

public section.

  class-methods BUSCA_DADOS
    importing
      !I_BUKRS type BUKRS_RAN_ITAB
      !I_WERKS type RANGE_T_WERKS
      !I_DATA_DOC type DATUM_RANGE_TAB optional
      !I_MATNR type RANGES_MATNR_TT optional
      !I_DOCNUM type J_1BNFE_TT_RANGE_DOCNUM optional
      !I_CHAVE_NFE type ZSDT_CHAVE_NFE optional
      !I_CFOP type ZSDT_CFOP_LIN optional
      !I_KUNNR type FAGL_RANGE_T_KUNNR optional
      !I_DT_EVENTO type DATUM_RANGE_TAB optional
    exporting
      !E_RETORNO_DADOS type ZSDT_DADOS_AVERBACAO_NFE
      !E_MSG_ERRO type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BUSCA_DADOS_AVERBACAO_NF IMPLEMENTATION.


  METHOD busca_dados.
    TYPES: BEGIN OF ty_total,
             chave_nfe    TYPE zi_sd_dados_documento_fiscal-chave_nfe,
             qtd_averbada TYPE zi_sd_dados_documento_fiscal-qtd_averbada,
           END OF ty_total.

    DATA: lt_total_qtd_avrb TYPE TABLE OF ty_total,
          lw_total_avrb     TYPE ty_total.

    IF i_dt_evento[] IS INITIAL AND i_data_doc[] IS INITIAL.
      e_msg_erro = 'Favor preencher campos Data Documento ou Data Evento'.
      EXIT.
    ENDIF.

    SELECT *
      FROM zi_sd_dados_documento_fiscal
      INTO TABLE @DATA(lt_dados)
      WHERE bukrs IN @i_bukrs
        AND filial IN @i_werks
        AND docdat IN @i_data_doc
        AND produto IN @i_matnr
        AND doc_num IN @i_docnum
        AND chave_nfe IN @i_chave_nfe
        AND cfop IN @i_cfop
        AND cliente IN @i_kunnr
        AND dt_evento IN @i_dt_evento.

    IF sy-subrc IS INITIAL.
      sort lt_dados.
      DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING ALL FIELDS.
      DATA(lt_dados_aux) = lt_dados.
      SORT lt_dados_aux BY bukrs branch.
      DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING bukrs branch.

      SELECT *
        FROM j_1bbranch
        INTO TABLE @DATA(lt_branch)
        FOR ALL ENTRIES IN @lt_dados_aux
       WHERE bukrs  EQ @lt_dados_aux-bukrs
         AND branch EQ @lt_dados_aux-branch.
      IF sy-subrc IS INITIAL.
        SORT lt_branch BY bukrs branch.
        DATA(lt_branch_aux) = lt_branch.
        SORT lt_branch_aux BY adrnr.
        DELETE ADJACENT DUPLICATES FROM lt_branch_aux COMPARING adrnr.

        SELECT *
          FROM adrc INTO TABLE @DATA(lt_adrc)
          FOR ALL ENTRIES IN @lt_branch_aux
         WHERE addrnumber EQ @lt_branch_aux-adrnr
           AND region     NE ''.

        IF ( sy-subrc = 0 ).
          SORT lt_adrc BY addrnumber.
        ENDIF.

      ENDIF.

      SELECT *
        FROM setleaf INTO TABLE @DATA(lt_set_uf)
       WHERE setname = 'MAGGI_UF_UTRIB_EXP'.
      IF sy-subrc IS INITIAL.
        SORT lt_set_uf BY valfrom.
      ENDIF.

      SELECT *
        FROM setleaf INTO TABLE @DATA(lt_set_cfop)
       WHERE setname = 'MAGGI_CFOP_UTRIB_EXP'.
      IF sy-subrc IS INITIAL.
        SORT lt_set_cfop BY valfrom.
      ENDIF.

      SELECT *
        FROM setleaf INTO TABLE @DATA(lt_set_ncm)
       WHERE setname = 'MAGGI_NCM_UTRIB_EXP'.
      IF sy-subrc IS INITIAL.
        SORT lt_set_ncm BY valfrom.
      ENDIF.


      LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_retorno_dados>).

        READ TABLE lt_branch ASSIGNING FIELD-SYMBOL(<fs_branch>)
        WITH KEY bukrs = <fs_retorno_dados>-bukrs
                 branch = <fs_retorno_dados>-branch
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_adrc ASSIGNING FIELD-SYMBOL(<fs_adrc>)
          WITH KEY addrnumber = <fs_branch>-adrnr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            READ TABLE lt_set_uf TRANSPORTING NO FIELDS
            WITH KEY valfrom = <fs_retorno_dados>-uf
            BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE lt_set_cfop TRANSPORTING NO FIELDS
              WITH KEY valfrom = <fs_retorno_dados>-cfop(4)
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                DATA(vl_ncm_exp) = <fs_retorno_dados>-nbm.
                REPLACE ALL OCCURRENCES OF '.' IN vl_ncm_exp WITH '' IGNORING CASE.

                READ TABLE lt_set_ncm TRANSPORTING NO FIELDS
                WITH KEY valfrom = vl_ncm_exp
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  CASE <fs_retorno_dados>-unidade.
                    WHEN 'KG'.
                      <fs_retorno_dados>-qtd_averbada = <fs_retorno_dados>-qtd_averbada * 1000.
*                      <fs_retorno_dados>-qtd_nao_averbada = <fs_retorno_dados>-quantidade - <fs_retorno_dados>-qtd_averbada.
                  ENDCASE.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.


        lw_total_avrb-chave_nfe    = <fs_retorno_dados>-chave_nfe.
        lw_total_avrb-qtd_averbada = <fs_retorno_dados>-qtd_averbada.
        COLLECT lw_total_avrb INTO lt_total_qtd_avrb.
        CLEAR lw_total_avrb.

      ENDLOOP.

      SORT lt_total_qtd_avrb BY chave_nfe.

      SORT lt_dados BY chave_nfe due DESCENDING.
      lt_dados_aux = lt_dados.
      DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING chave_nfe.

      LOOP AT lt_dados_aux ASSIGNING FIELD-SYMBOL(<fs_dados_aux>).

        READ TABLE lt_dados TRANSPORTING NO FIELDS
        WITH KEY chave_nfe = <fs_dados_aux>-chave_nfe
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_Dados ASSIGNING FIELD-SYMBOL(<fs_dados>) FROM sy-tabix.
            IF <fs_dados>-chave_nfe <> <fs_dados_aux>-chave_nfe.
              EXIT.
            ENDIF.

            IF <fs_dados>-due = <fs_dados_aux>-due.
              APPEND INITIAL LINE TO e_retorno_dados ASSIGNING FIELD-SYMBOL(<fs_retorno_dados2>).
              READ TABLE lt_total_qtd_avrb ASSIGNING FIELD-SYMBOL(<fs_total>)
              with key chave_nfe = <fs_dados>-chave_nfe
              BINARY SEARCH.
              IF sy-subrc is INITIAL.
                <fs_dados>-qtd_nao_averbada = <fs_dados>-quantidade - <fs_total>-qtd_averbada.
              ENDIF.

              MOVE-CORRESPONDING <fs_dados> TO <fs_retorno_dados2>.

            ELSE.

              CLEAR: <fs_dados>-vlr_unit,
                     <fs_dados>-valor_nf,
                     <fs_dados>-qtd_nao_averbada,
                     <fs_dados>-quantidade.

              APPEND INITIAL LINE TO e_retorno_dados ASSIGNING <fs_retorno_dados2>.
              MOVE-CORRESPONDING <fs_dados> TO <fs_retorno_dados2>.
            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDLOOP.

      MOVE-CORRESPONDING lt_dados TO e_retorno_dados.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
