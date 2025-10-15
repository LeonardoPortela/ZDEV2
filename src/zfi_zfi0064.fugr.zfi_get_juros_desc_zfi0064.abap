FUNCTION zfi_get_juros_desc_zfi0064.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  RANGE_C10_T
*"     REFERENCE(I_AUGDT) TYPE  RANGE_C10_T
*"     REFERENCE(I_KUNNR) TYPE  RANGE_C10_T
*"     REFERENCE(I_NR_OV) TYPE  RANGE_C10_T
*"     REFERENCE(I_NR_SOL) TYPE  RANGE_C10_T
*"     REFERENCE(I_AUART) TYPE  RANGE_C10_T
*"     REFERENCE(I_OPCAO) TYPE  ZCHAR02
*"     REFERENCE(I_WAERS) TYPE  WAERS DEFAULT 'BRL'
*"     REFERENCE(I_TIPO) TYPE  ZCHAR02
*"  EXPORTING
*"     REFERENCE(R_SAIDA) TYPE  ZFIS_SAIDA_JUROS_ZFI0064_T
*"----------------------------------------------------------------------
*---------------------------------------------------------------------------------------------------------------*
* Inicio Processamento
*---------------------------------------------------------------------------------------------------------------*

  CLEAR: r_saida[].


  "Cenário 1: Descontos de antecipação com Adiantamentos contra banco -
  "Nesse cenário, o adiantamento de Cliente nasce Contra Banco - Ex: Documento: 1400119625 Empresa: 0001
  PERFORM f_build_desc_juros_cenario1 USING i_opcao
                                            i_tipo
                                            i_waers
                                            i_bukrs
                                            i_augdt
                                            i_auart
                                            i_nr_ov
                                            i_nr_sol
                                            i_kunnr
                                   CHANGING r_saida.

ENDFUNCTION.
