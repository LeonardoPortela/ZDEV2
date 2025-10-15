FUNCTION znfe_inbound_aceite.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHAVE) TYPE  ZDE_CHAVE_DOC_E OPTIONAL
*"     REFERENCE(I_NOTA) TYPE REF TO  ZCL_NFE_INBOUND OPTIONAL
*"  EXCEPTIONS
*"      ZCX_NFE_INBOUND_EXCEPTION
*"      ZCX_CADASTRO
*"----------------------------------------------------------------------

  IF i_nota IS NOT INITIAL.
    obj_nfe_inbound = i_nota.
  ELSE.

    TRY.
        CREATE OBJECT obj_nfe_inbound
          EXPORTING
            i_chave_nfe = i_chave.
      CATCH zcx_nfe_inbound_exception.
      CATCH zcx_cadastro.
    ENDTRY.
  ENDIF.

  PERFORM get_info_tela.

  "Chamar Tela
  CALL SCREEN 1500 STARTING AT 15  01  "*-CS2025000249-04.06.2025-#168929-JT
                     ENDING AT 166 21. "*-CS2025000249-04.06.2025-#168929-JT

  PERFORM limpar_variaveis.

ENDFUNCTION.
