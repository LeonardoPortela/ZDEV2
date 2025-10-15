FUNCTION zhcmf_limpa_aprov_temp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  DATUM
*"----------------------------------------------------------------------

*------------------------------------
*-Filtro tabela
*------------------------------------
  SELECT *
    FROM zhcmt_py_0005
    INTO TABLE t_py0005.

*------------------------------------
*-LImpa campos aprovadores temporarios
*------------------------------------
  LOOP AT t_py0005 INTO w_py0005.
    IF w_py0005-aprovador_tmp IS NOT INITIAL.
      IF w_py0005-dt_val_ate_tmp < i_data.
        CLEAR: w_py0005-aprovador_tmp,
               w_py0005-dt_val_de_tmp,
               w_py0005-dt_val_ate_tmp.
        MODIFY zhcmt_py_0005 FROM w_py0005.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
