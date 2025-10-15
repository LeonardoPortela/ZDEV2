*&---------------------------------------------------------------------*
*& Report ZMMR207
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr207.

DATA nfe TYPE REF TO zcl_nfse_inbound.
DATA e_ret TYPE zmme_saida_miro_automatica.

PARAMETERS p_idfila TYPE zde_id_fila_job OBLIGATORY.
PARAMETERS p_nfe TYPE /tcsr/e_guid_header.
PARAMETERS p_venc TYPE  zde_dt_vencimento.
PARAMETERS p_issqn TYPE j_1btaxval.
PARAMETERS p_inss TYPE j_1btaxval.
PARAMETERS p_pis TYPE j_1btaxval.
PARAMETERS p_cof TYPE j_1btaxval.
PARAMETERS p_csll TYPE j_1btaxval.
PARAMETERS p_irrf TYPE j_1btaxval.

PARAMETERS p_pccsll TYPE j_1btaxval.
PARAMETERS p_bissqn TYPE j_1btaxval.
PARAMETERS p_binss TYPE j_1btaxval.
PARAMETERS p_birrf TYPE j_1btaxval.

PARAMETERS p_estor TYPE char1.

PARAMETERS p_name TYPE syuname.

START-OF-SELECTION.

  TRY .

      CREATE OBJECT nfe
        EXPORTING
          i_guid = p_nfe.

      "Localizando a NF-e
      nfe->zif_cadastro~set_registro( i_id_registro = p_nfe ).

      " 20.09.2022 - Set valor imposto -->

      CALL METHOD nfe->set_impostos
        EXPORTING
          i_issqn           = p_issqn
          i_inss            = p_inss
          i_pis             = p_pis
          i_cof             = p_cof
          i_csll            = p_csll
          i_irrf            = p_irrf
          i_piscofcsll      = p_pccsll
          i_base_issqn      = p_bissqn
          i_base_inss       = p_binss
          i_base_irrf       = p_birrf
          i_data_vencimento = p_venc.

      CASE p_estor.

        WHEN abap_true.

          nfe->nfse_inbound_cancela_fatura( ).

          e_ret-e_sucesso = '1'.

        WHEN abap_false.

          "Aceite Fatura
          "nfe->ck_ignora_data_se_vencimento = abap_true.
          nfe->set_aceitar_faturar( EXPORTING i_us_miro = p_name ).

          IF nfe->zif_cadastro~gravar_registro( ) EQ abap_true.
            "DATA(nota) = nfe->get_info_nota( ).

            e_ret-e_belnr   = nfe->ms_nfse_001-belnr.

            e_ret-e_gjahr   = nfe->ms_nfse_001-gjahr.

            IF e_ret-e_belnr IS NOT INITIAL AND e_ret-e_gjahr IS NOT INITIAL.

              e_ret-e_sucesso = '1'.

            ELSE.

              DATA(lw_ret) = nfe->get_last_message( ).

              IF lw_ret IS NOT INITIAL.
                e_ret-e_messagem_erro = lw_ret-message.
              ENDIF.

            ENDIF.

          ELSE.

            lw_ret = nfe->get_last_message( ).

            IF lw_ret IS NOT INITIAL.
              e_ret-e_messagem_erro = lw_ret-message.
            ENDIF.

          ENDIF.

      ENDCASE.

      IF nfe IS NOT INITIAL.
        nfe->free( ).
        CLEAR: nfe.
      ENDIF.

  ENDTRY.

  DATA lv_dados_json TYPE string.

  lv_dados_json = /ui2/cl_json=>serialize( data = e_ret ).

  UPDATE zjob0003 SET dados_processados = lv_dados_json
    WHERE id_fila_job = p_idfila.

  COMMIT WORK AND WAIT.
