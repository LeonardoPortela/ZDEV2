FUNCTION zmm_busca_dados_notas_ecc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OPERACAO) TYPE  CHAR02 OPTIONAL
*"     REFERENCE(I_DATA_INI) TYPE  SY-DATUM OPTIONAL
*"     REFERENCE(I_DATA_FIM) TYPE  SY-DATUM OPTIONAL
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"----------------------------------------------------------------------

  FREE: l_destination,
        l_zsdt0231,
        l_j_1bnfdoc,
        l_j_1bnflin,
        l_j_1bnfnad,
        l_j_1bnfstx,
        l_j_1bnfe_active,
        l_zib_nfe.

  CASE sy-sysid.
    WHEN 'DEV'.
      l_destination =  'DEV_ECC'.
    WHEN 'QAS'.
      l_destination =  'QAS_ECC'.
    WHEN 'PRD'.
      l_destination =  'PRD_ECC'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  CHECK l_destination IS NOT INITIAL.

  CALL FUNCTION 'ZLES_ZMM0127_CONTINGENCIA'
    DESTINATION l_destination
    EXPORTING
      i_data_ini       = i_data_ini
      i_data_fim       = i_data_fim
      i_docnum         = i_docnum
    IMPORTING
      e_zsdt0231       = l_zsdt0231
      e_j_1bnfdoc      = l_j_1bnfdoc
      e_j_1bnflin      = l_j_1bnflin
      e_j_1bnfnad      = l_j_1bnfnad
      e_j_1bnfstx      = l_j_1bnfstx
      e_j_1bnfe_active = l_j_1bnfe_active
      e_zib_nfe        = l_zib_nfe.

  /ui2/cl_json=>deserialize( EXPORTING json = l_zsdt0231       CHANGING data = t_zsdt0231_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_j_1bnfdoc      CHANGING data = t_j_1bnfdoc_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_j_1bnflin      CHANGING data = t_j_1bnflin_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_j_1bnfnad      CHANGING data = t_j_1bnfnad_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_j_1bnfstx      CHANGING data = t_j_1bnfstx_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_j_1bnfe_active CHANGING data = t_j_1bnfe_active_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zib_nfe        CHANGING data = t_zib_nfe_ecc ).

  PERFORM f_selecao_hana.

  IF i_operacao = '01'.
    PERFORM f_verifica_duplicidade.
  ELSE.
    PERFORM f_carrega_notas.
  ENDIF.
ENDFUNCTION.
