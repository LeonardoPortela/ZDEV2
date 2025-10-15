FUNCTION znfe_inbound_j_1bnfe_active.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(C_J_1BNFE_ATIVE) TYPE  J_1BNFE_ACTIVE
*"----------------------------------------------------------------------

  "BREAK MBARBARA.

  CHECK c_j_1bnfe_ative-docnum IS NOT INITIAL.
  CHECK c_j_1bnfe_ative-form IS INITIAL.
  CHECK c_j_1bnfe_ative-regio IS NOT INITIAL.
  CHECK c_j_1bnfe_ative-nfyear IS NOT INITIAL.
  CHECK c_j_1bnfe_ative-nfmonth IS NOT INITIAL.
  CHECK c_j_1bnfe_ative-stcd1 IS NOT INITIAL.
  CHECK c_j_1bnfe_ative-model IS NOT INITIAL.
  CHECK c_j_1bnfe_ative-serie IS NOT INITIAL.
  CHECK c_j_1bnfe_ative-nfnum9 IS NOT INITIAL.

  DATA(ch_chave) = c_j_1bnfe_ative-regio && c_j_1bnfe_ative-nfyear && c_j_1bnfe_ative-nfmonth &&
                   c_j_1bnfe_ative-stcd1 && c_j_1bnfe_ative-model && c_j_1bnfe_ative-serie && c_j_1bnfe_ative-nfnum9 && '%'.

  CASE c_j_1bnfe_ative-model.
    WHEN zif_doc_eletronico=>at_st_model_nfe.

      "– [000-889]: Aplicativo do Contribuinte; Emitente=CNPJ; Assinatura pelo e-CNPJ do contribuinte (procEmi<>1,2);
      "- [900-909]: Emissão no site do Fisco (NFA-e); Emitente= CNPJ; Assinatura pelo e-CNPJ da SEFAZ (procEmi=1), ou Assinatura pelo e-CNPJ do contribuinte (procEmi=2);
      "- [910-919]: Emissão no site do Fisco (NFA-e); Emitente= CPF; Assinatura pelo e-CNPJ da SEFAZ (procEmi=1), ou Assinatura pelo e-CPF do contribuinte (procEmi=2);
      "- [920-969]: Aplicativo do Contribuinte; Emitente=CPF; Assinatura pelo e-CPF do contribuinte (procEmi<>1,2);

      SELECT SINGLE * INTO @DATA(wa_nfe_inbound)
        FROM zib_nfe_dist_ter
       WHERE chave_nfe LIKE @ch_chave.

      IF sy-subrc IS INITIAL.
        c_j_1bnfe_ative-docnum9  = wa_nfe_inbound-chave_nfe+34(9).
        c_j_1bnfe_ative-cdv      = wa_nfe_inbound-chave_nfe+43(1).
        c_j_1bnfe_ative-docsta   = '1'.
        c_j_1bnfe_ative-authcod  = wa_nfe_inbound-nr_protocolo.
        c_j_1bnfe_ative-authdate = wa_nfe_inbound-dt_protocolo.
        c_j_1bnfe_ative-authtime = wa_nfe_inbound-hr_protocolo.
        c_j_1bnfe_ative-tpemis   = wa_nfe_inbound-cd_form_emissao.
        c_j_1bnfe_ative-code     = wa_nfe_inbound-cd_msg_sefaz.

        SELECT SINGLE * INTO @DATA(wa_zib_nfe_forn)
          FROM zib_nfe_forn
         WHERE nu_chave EQ @wa_nfe_inbound-chave_nfe.

        IF sy-subrc IS INITIAL.
          wa_zib_nfe_forn-docnum     = c_j_1bnfe_ative-docnum.
          wa_zib_nfe_forn-bukrs      = c_j_1bnfe_ative-bukrs.
          wa_zib_nfe_forn-branch     = c_j_1bnfe_ative-branch.
          wa_zib_nfe_forn-atualizado = abap_true.

          wa_zib_nfe_forn-id_identificador = 1.
          wa_zib_nfe_forn-user_identificador = sy-uname.
          wa_zib_nfe_forn-data_identificador = sy-datum.
          wa_zib_nfe_forn-hora_identificador = sy-uzeit.

          MODIFY zib_nfe_forn FROM wa_zib_nfe_forn.
        ENDIF.
      ELSEIF c_j_1bnfe_ative-parid IS NOT INITIAL AND c_j_1bnfe_ative-serie BETWEEN '890' AND '899'.

        "- [890-899]: Emissão no site do Fisco (NFA-e - Avulsa); Emitente= CNPJ / CPF; Assinatura pelo e-CNPJ da SEFAZ (procEmi=1);
        CASE c_j_1bnfe_ative-partyp.
          WHEN 'C'.
            TRY .
                DATA(lc_regio) = CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = c_j_1bnfe_ative-parid ) )->at_kna1-regio.
              CATCH zcx_parceiros.    " .
                CLEAR: lc_regio.
            ENDTRY.
          WHEN 'V'.
            TRY .
                lc_regio = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = c_j_1bnfe_ative-parid ) )->at_lfa1-regio.
              CATCH zcx_parceiros.    " .
                CLEAR: lc_regio.
            ENDTRY.
        ENDCASE.

        IF lc_regio IS NOT INITIAL.

          SELECT * INTO TABLE @DATA(it_secretarias)
            FROM lfa1
           WHERE ktokk EQ 'ZIMP'
             AND regio EQ @lc_regio
             AND loevm EQ @space
             AND sperr EQ @space
             AND sperm EQ @space
             AND stcd1 NE @space.

          LOOP AT it_secretarias INTO DATA(wa_secretarias).

            IF c_j_1bnfe_ative-authcod IS NOT INITIAL.
              CONTINUE.
            ENDIF.

            ch_chave = c_j_1bnfe_ative-regio && c_j_1bnfe_ative-nfyear && c_j_1bnfe_ative-nfmonth && wa_secretarias-stcd1 && c_j_1bnfe_ative-model && c_j_1bnfe_ative-serie && c_j_1bnfe_ative-nfnum9 && '%'.

            SELECT SINGLE * INTO @wa_nfe_inbound
              FROM zib_nfe_dist_ter
             WHERE chave_nfe LIKE @ch_chave.

            IF sy-subrc IS INITIAL.
              c_j_1bnfe_ative-stcd1    = wa_nfe_inbound-chave_nfe+6(14).
              c_j_1bnfe_ative-docnum9  = wa_nfe_inbound-chave_nfe+34(9).
              c_j_1bnfe_ative-cdv      = wa_nfe_inbound-chave_nfe+43(1).
              c_j_1bnfe_ative-docsta   = '1'.
              c_j_1bnfe_ative-authcod  = wa_nfe_inbound-nr_protocolo.
              c_j_1bnfe_ative-authdate = wa_nfe_inbound-dt_protocolo.
              c_j_1bnfe_ative-authtime = wa_nfe_inbound-hr_protocolo.
              c_j_1bnfe_ative-tpemis   = wa_nfe_inbound-cd_form_emissao.
              c_j_1bnfe_ative-code     = wa_nfe_inbound-cd_msg_sefaz.

              SELECT SINGLE * INTO @wa_zib_nfe_forn
                FROM zib_nfe_forn
               WHERE nu_chave EQ @wa_nfe_inbound-chave_nfe.

              IF sy-subrc IS INITIAL.
                wa_zib_nfe_forn-docnum     = c_j_1bnfe_ative-docnum.
                wa_zib_nfe_forn-bukrs      = c_j_1bnfe_ative-bukrs.
                wa_zib_nfe_forn-branch     = c_j_1bnfe_ative-branch.
                wa_zib_nfe_forn-atualizado = abap_true.

                wa_zib_nfe_forn-id_identificador = 2.
                wa_zib_nfe_forn-user_identificador = sy-uname.
                wa_zib_nfe_forn-data_identificador = sy-datum.
                wa_zib_nfe_forn-hora_identificador = sy-uzeit.

                MODIFY zib_nfe_forn FROM wa_zib_nfe_forn.
              ENDIF.
            ENDIF.

          ENDLOOP.
        ENDIF.

      ENDIF.

    WHEN zif_doc_eletronico=>at_st_model_cte.

      SELECT SINGLE * INTO @DATA(wa_cte_inbound)
        FROM zib_cte_dist_ter
       WHERE cd_chave_cte LIKE @ch_chave.

      IF sy-subrc IS INITIAL.
        c_j_1bnfe_ative-docnum9  = wa_cte_inbound-cd_chave_cte+34(9).
        c_j_1bnfe_ative-cdv      = wa_cte_inbound-cd_chave_cte+43(1).
        c_j_1bnfe_ative-docsta   = '1'.
        c_j_1bnfe_ative-authcod  = wa_cte_inbound-nr_protocolo.
        c_j_1bnfe_ative-authdate = wa_cte_inbound-dt_protocolo.
        c_j_1bnfe_ative-authtime = wa_cte_inbound-hr_protocolo.
        c_j_1bnfe_ative-code     = wa_cte_inbound-cd_status_sefaz.

        SELECT SINGLE * INTO @wa_zib_nfe_forn
          FROM zib_nfe_forn
         WHERE nu_chave EQ @wa_cte_inbound-cd_chave_cte.

        IF sy-subrc IS INITIAL.
          wa_zib_nfe_forn-docnum     = c_j_1bnfe_ative-docnum.
          wa_zib_nfe_forn-bukrs      = c_j_1bnfe_ative-bukrs.
          wa_zib_nfe_forn-branch     = c_j_1bnfe_ative-branch.
          wa_zib_nfe_forn-atualizado = abap_true.

          wa_zib_nfe_forn-id_identificador = 3.
          wa_zib_nfe_forn-user_identificador = sy-uname.
          wa_zib_nfe_forn-data_identificador = sy-datum.
          wa_zib_nfe_forn-hora_identificador = sy-uzeit.

          MODIFY zib_nfe_forn FROM wa_zib_nfe_forn.
        ENDIF.

      ENDIF.

  ENDCASE.

ENDFUNCTION.
