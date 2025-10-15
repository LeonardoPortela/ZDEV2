FUNCTION znfe_inbound_aceite_fatura.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHAVE) TYPE  ZDE_CHAVE_DOC_E OPTIONAL
*"     REFERENCE(I_NOTA) TYPE REF TO  ZCL_NFE_INBOUND OPTIONAL
*"  RAISING
*"      ZCX_NFE_INBOUND_EXCEPTION
*"      ZCX_CADASTRO
*"----------------------------------------------------------------------

  DATA: lc_cte TYPE zib_cte_dist_ter.

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

  PERFORM get_info_banco_parceiro.

  PERFORM get_info_valores.

*-CS2025000249-17.04.2025-#173311-JT-inicio
  IF obj_nfe_inbound->get_ck_miro_automatica( ) = abap_true.
    PERFORM f_efetuar_faturamento.
  ELSE.
    "Chamar Tela
    CALL SCREEN 1700 STARTING AT 30 01.
  ENDIF.
*-CS2025000249-17.04.2025-#173311-JT-fim

  PERFORM limpar_variaveis.

ENDFUNCTION.
