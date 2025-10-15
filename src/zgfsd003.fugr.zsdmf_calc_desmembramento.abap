FUNCTION ZSDMF_CALC_DESMEMBRAMENTO.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_TP_DESMEM) TYPE  CHAR1 DEFAULT 'Q'
*"     REFERENCE(IV_VLR_DESME) TYPE  NETWR_AP OPTIONAL
*"     REFERENCE(IV_QTD_DESME) TYPE  RFMNG OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"  TABLES
*"      CT_DESMEMBRA STRUCTURE  ZSDS081
*"--------------------------------------------------------------------

  CHECK iv_tp_desmem IS NOT INITIAL.

  CASE iv_tp_desmem.

      " ---- calcula por Quantidade
    WHEN 'Q'.

      PERFORM f_calcula_qtd USING iv_qtd_desme CHANGING ct_desmembra[] ev_erro.

      " ---- calcula por Valor
    WHEN 'V'.

      PERFORM f_calcula_vlr USING iv_vlr_desme CHANGING ct_desmembra[] ev_erro.

    WHEN OTHERS.
  ENDCASE.


ENDFUNCTION.
