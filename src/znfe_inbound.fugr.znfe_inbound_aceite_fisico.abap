FUNCTION znfe_inbound_aceite_fisico.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHAVE) TYPE  ZDE_CHAVE_DOC_E OPTIONAL
*"     REFERENCE(I_NOTA) TYPE REF TO  ZCL_NFE_INBOUND OPTIONAL
*"  RAISING
*"      ZCX_NFE_INBOUND_EXCEPTION
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
*-CS2025000249-08.04.2025-#173180-JT-inicio
* CALL SCREEN 1600 STARTING AT 30 01.
  CALL SCREEN 1600 STARTING AT 15 01
                     ENDING AT 175 22.
*-CS2025000249-08.04.2025-#173180-JT-fim

  PERFORM limpar_variaveis.

ENDFUNCTION.
