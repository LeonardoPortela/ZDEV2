FUNCTION zles_busca_dados_frete_aquav .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OPERACAO) TYPE  CHAR02 OPTIONAL
*"     REFERENCE(I_DATA_INI) TYPE  DATUM OPTIONAL
*"     REFERENCE(I_DATA_FIM) TYPE  DATUM OPTIONAL
*"     REFERENCE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_NR_VIAGEM) TYPE  NUM4 OPTIONAL
*"     REFERENCE(I_ANO_VIAGEM) TYPE  AJAHR OPTIONAL
*"----------------------------------------------------------------------

  FREE: l_destination, l_zlest0056, l_zlest0057, l_zlest0058,
                       l_zlest0060, l_zlest0061, l_zlest0063,
                       l_zlest0068, l_zlest0076, l_zlest0166,
                       l_zlest0073, l_zlest0158, l_zlest0205.

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

  CALL FUNCTION 'ZLES_FRETE_AQUAV_CONTINGENCIA'
    DESTINATION l_destination
    EXPORTING
      i_operacao   = i_operacao
      i_data_ini   = i_data_ini
      i_data_fim   = i_data_fim
      i_bukrs      = i_bukrs
      i_werks      = i_werks
      i_nr_viagem  = i_nr_viagem
      i_ano_viagem = i_ano_viagem
    IMPORTING
      e_zlest0056  = l_zlest0056
      e_zlest0057  = l_zlest0057
      e_zlest0058  = l_zlest0058
      e_zlest0060  = l_zlest0060
      e_zlest0061  = l_zlest0061
      e_zlest0063  = l_zlest0063
      e_zlest0068  = l_zlest0068
      e_zlest0073  = l_zlest0073
      e_zlest0076  = l_zlest0076
      e_zlest0158  = l_zlest0158
      e_zlest0166  = l_zlest0166
      e_zlest0205  = l_zlest0205.

  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0056 CHANGING data = t_zlest0056_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0057 CHANGING data = t_zlest0057_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0058 CHANGING data = t_zlest0058_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0060 CHANGING data = t_zlest0060_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0061 CHANGING data = t_zlest0061_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0063 CHANGING data = t_zlest0063_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0068 CHANGING data = t_zlest0068_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0073 CHANGING data = t_zlest0073_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0076 CHANGING data = t_zlest0076_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0158 CHANGING data = t_zlest0158_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0166 CHANGING data = t_zlest0166_ecc ).
  /ui2/cl_json=>deserialize( EXPORTING json = l_zlest0205 CHANGING data = t_zlest0205_ecc ).

  PERFORM f_selecao_hana.

  IF i_operacao = '03' OR i_operacao = '04'.
    PERFORM f_verifica_duplicidade.
  ELSE.
    PERFORM f_carrega_tabelas.
  ENDIF.

ENDFUNCTION.
