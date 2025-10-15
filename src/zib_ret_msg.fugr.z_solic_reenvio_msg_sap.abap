FUNCTION Z_SOLIC_REENVIO_MSG_SAP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ZOB_RET_MSG) TYPE  ZOB_RET_MSG
*"----------------------------------------------------------------------

  DATA: _ZOB_RET_MSG TYPE ZOB_RET_MSG.

  CLEAR: _ZOB_RET_MSG.

  MOVE-CORRESPONDING I_ZOB_RET_MSG TO _ZOB_RET_MSG.

  _ZOB_RET_MSG-DT_REGISTRO    = SY-DATUM.
  _ZOB_RET_MSG-HR_REGISTRO    = SY-UZEIT.
  _ZOB_RET_MSG-RG_ATUALIZADO  = 'N'.
  MODIFY ZOB_RET_MSG FROM _ZOB_RET_MSG.

  CASE _ZOB_RET_MSG-CD_PROCESSO.
    WHEN '001'. "Romaneio
      IF SY-SUBRC NE 0.
        MESSAGE |Houve um erro no processamento do romaneio: { _ZOB_RET_MSG-MSG_V2 } - Chave: { _ZOB_RET_MSG-MSG_V1 } ! | TYPE 'S'.
      ENDIF.
    WHEN '002'. "Distribuição NF-e/CT-e
      IF SY-SUBRC NE 0.
        MESSAGE |Houve um erro no processamento da distribuição: Chave: { _ZOB_RET_MSG-MSG_V1 } ! | TYPE 'S'.
      ENDIF.
    WHEN '003'. "Outros
  ENDCASE.


ENDFUNCTION.
