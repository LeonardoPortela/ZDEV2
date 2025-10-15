FUNCTION zfi_zfi0098.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EMPRESA) TYPE  ZFI_REL_ZFI0098-EMPRESA
*"     REFERENCE(I_MES_ANO) TYPE  ZFI_REL_ZFI0098-MES_ANO
*"     REFERENCE(I_MOEDA) TYPE  ZFI_REL_ZFI0098-MOEDA
*"  EXPORTING
*"     REFERENCE(IT_SAIDA) TYPE  ZFIE0014_T
*"----------------------------------------------------------------------

*  APPEND VALUE #( sign = 'I' option = 'EQ' low = i_empresa high = i_empresa ) TO p_bukrs.

  PERFORM f_prepare_run_time_info USING abap_false.
  SUBMIT zfir0062 WITH p_bukrs  = i_empresa
                  WITH p_mm_ano = i_mes_ano
                  WITH p_waers  = i_moeda
                  AND RETURN.

  PERFORM f_get_runtime_info.

  IF <t_data> IS ASSIGNED.
    FREE: it_saida.
    MOVE-CORRESPONDING <t_data> TO it_alv_saida.
    MOVE-CORRESPONDING it_alv_saida TO it_saida.

  ENDIF.



ENDFUNCTION.
