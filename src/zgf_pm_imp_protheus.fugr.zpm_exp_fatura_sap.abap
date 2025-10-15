FUNCTION ZPM_EXP_FATURA_SAP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(VIEW) TYPE  STRING
*"     REFERENCE(FATURA) TYPE  CHAR10
*"     REFERENCE(CNPJ) TYPE  STCD1
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  STRING
*"----------------------------------------------------------------------

  DATA(_VIEW) = |{ VIEW CASE = LOWER }|.

  CASE _VIEW.
    WHEN 'i_fatura'.
* Verificando se existem faturas com erro.
      ZCL_WEBSERVIC_PROTHEUS=>GET_CONS_FATURA(
        EXPORTING
          FATURA = FATURA
          CNPJ   = CNPJ
        RECEIVING
          E_RETURNG = DATA(E_FATURA) ).

      RESULT = ZCL_FMCALL_PM=>ABAP2JSON( ABAP_DATA = E_FATURA ).

    WHEN OTHERS.
  ENDCASE.
ENDFUNCTION.
