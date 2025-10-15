FUNCTION zmm_busca_dados_algodao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OPERACAO) TYPE  CHAR02 OPTIONAL
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_LGORT) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(I_CHARG) TYPE  CHARG_D OPTIONAL
*"     REFERENCE(I_NR_ROMANEIO) TYPE  ZNR_ROMANEIO OPTIONAL
*"----------------------------------------------------------------------

  FREE: l_destination.

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

  CALL FUNCTION 'ZMM_FATURA_CONTINGENCIA'
    DESTINATION l_destination
    EXPORTING
      i_operacao    = i_operacao
      i_werks       = i_werks
      i_lgort       = i_lgort
      i_charg       = i_charg
      i_nr_romaneio = i_nr_romaneio
    TABLES
      t_zmmt0008    = t_zmmt0008_ecc.

  IF t_zmmt0008_ecc[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Nao ha dados a serem importados!'.
    RETURN.
  ENDIF.

  SELECT *
    FROM zmmt0008
    INTO TABLE t_zmmt0008_hana
     FOR ALL ENTRIES IN t_zmmt0008_ecc
   WHERE werks = t_zmmt0008_ecc-werks
     AND lgort = t_zmmt0008_ecc-lgort
     AND charg = t_zmmt0008_ecc-charg.

  IF     i_operacao = '03'.
    PERFORM f_verifica_duplicidade.
  ELSEIF i_operacao = '01' OR i_operacao = '02'.
    PERFORM f_carrega_tabela.
  ELSEIF i_operacao = '04'.
    PERFORM f_carrega_sobregrava.
  ENDIF.

ENDFUNCTION.
