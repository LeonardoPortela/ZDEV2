FUNCTION znfe_inbound_j_1bnfdoc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_J_1BNFE_ATIVE) TYPE  J_1BNFE_ACTIVE
*"  CHANGING
*"     REFERENCE(C_J_1BNFDOC) TYPE  J_1BNFDOC
*"----------------------------------------------------------------------

  "BREAK MBARBARA.

  CHECK c_j_1bnfdoc-nfe EQ abap_true.
  CHECK c_j_1bnfdoc-form IS INITIAL.
  CHECK c_j_1bnfdoc-xmlvers IS NOT INITIAL.

  CHECK i_j_1bnfe_ative-docnum IS NOT INITIAL.
  CHECK i_j_1bnfe_ative-form IS INITIAL.
  CHECK i_j_1bnfe_ative-regio IS NOT INITIAL.
  CHECK i_j_1bnfe_ative-nfyear IS NOT INITIAL.
  CHECK i_j_1bnfe_ative-nfmonth IS NOT INITIAL.
  CHECK i_j_1bnfe_ative-stcd1 IS NOT INITIAL.
  CHECK i_j_1bnfe_ative-model IS NOT INITIAL.
  CHECK i_j_1bnfe_ative-serie IS NOT INITIAL.
  CHECK i_j_1bnfe_ative-nfnum9 IS NOT INITIAL.
  CHECK i_j_1bnfe_ative-docnum9 IS NOT INITIAL.
  CHECK i_j_1bnfe_ative-cdv IS NOT INITIAL.
  CHECK i_j_1bnfe_ative-authcod IS NOT INITIAL.

  DATA(ch_chave) = i_j_1bnfe_ative-regio && i_j_1bnfe_ative-nfyear && i_j_1bnfe_ative-nfmonth &&
                   i_j_1bnfe_ative-stcd1 && i_j_1bnfe_ative-model && i_j_1bnfe_ative-serie && i_j_1bnfe_ative-nfnum9 &&
                   i_j_1bnfe_ative-docnum9 && i_j_1bnfe_ative-cdv.

  CASE i_j_1bnfe_ative-model.
    WHEN zif_doc_eletronico=>at_st_model_nfe.

      SELECT SINGLE * INTO @DATA(wa_nfe_inbound)
        FROM zib_nfe_dist_ter
       WHERE chave_nfe EQ @ch_chave.

      IF sy-subrc IS INITIAL.
        c_j_1bnfdoc-xmlvers = wa_nfe_inbound-xmlvers.
        c_j_1bnfdoc-docstat  = '1'.
        c_j_1bnfdoc-authcod  = wa_nfe_inbound-nr_protocolo.
        c_j_1bnfdoc-authdate = wa_nfe_inbound-dt_protocolo.
        c_j_1bnfdoc-authtime = wa_nfe_inbound-hr_protocolo.
        c_j_1bnfdoc-code     = wa_nfe_inbound-cd_msg_sefaz.
      ENDIF.

    WHEN zif_doc_eletronico=>at_st_model_cte.

      SELECT SINGLE * INTO @DATA(wa_cte_inbound)
        FROM zib_cte_dist_ter
       WHERE cd_chave_cte EQ @ch_chave.

      IF sy-subrc IS INITIAL.
        c_j_1bnfdoc-xmlvers = wa_cte_inbound-xmlvers.
        c_j_1bnfdoc-docstat  = '1'.
        c_j_1bnfdoc-authcod  = wa_cte_inbound-nr_protocolo.
        c_j_1bnfdoc-authdate = wa_cte_inbound-dt_protocolo.
        c_j_1bnfdoc-authtime = wa_cte_inbound-hr_protocolo.
        c_j_1bnfdoc-code     = wa_cte_inbound-cd_status_sefaz.
      ENDIF.

  ENDCASE.

ENDFUNCTION.
