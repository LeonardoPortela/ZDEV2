"Name: \FU:FCJ_MF_PRINT_JOURNAL\SE:BEGIN\EI
ENHANCEMENT 0 Z_FBCJ_FECHA2.
*------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------
*BLOQUEAR DOCUMENTOS COM STATUS DIFERENTE DE 'P' EFETIVADOS

  DATA: V_DOCUMENT_STATUS TYPE TCJ_DOCUMENTS-DOCUMENT_STATUS.
  CLEAR: V_DOCUMENT_STATUS.

  SELECT SINGLE DOCUMENT_STATUS
    INTO V_DOCUMENT_STATUS
       FROM TCJ_DOCUMENTS
    WHERE COMP_CODE   = LS_POSTINGS-COMP_CODE
      AND CAJO_NUMBER = LS_POSTINGS-CAJO_NUMBER
      AND FISC_YEAR   = LS_POSTINGS-FISC_YEAR
      AND POSTING_NUMBER = LS_POSTINGS-POSTING_NUMBER.
  if sy-subrc = 0.
    IF ( V_DOCUMENT_STATUS NE 'P'). " STATUS NÃO EFETIVADO
     MESSAGE W000(Z_FI) WITH 'Para gerar impressão solicitar' 'ao gestor efetivar o lançamento'.
     EXIT.
    ENDIF.
 endif.



ENDENHANCEMENT.
