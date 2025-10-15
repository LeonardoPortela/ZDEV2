FUNCTION zmm_me21n_gera_solicitacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SEQUENCIAL) TYPE  CHAR10 OPTIONAL
*"     REFERENCE(IW_ZSDT0158) TYPE  ZSDT0158 OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  TABLES
*"      ET_BAPIRET2 STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  DATA lv_erro TYPE c.
  DATA lw_0158 TYPE zsdt0158.

  lw_0158 = iw_zsdt0158.

  PERFORM f_recupera_registro
    USING i_sequencial
    CHANGING lw_0158.

  PERFORM f_valida_reg_solic
    USING lw_0158
 CHANGING lv_erro
          et_bapiret2[].

  CHECK lv_erro IS INITIAL.

  PERFORM f_gera_numeracao CHANGING lw_0158.

  CHECK lw_0158-nro_sol_ov IS NOT INITIAL.

  CHECK i_commit IS NOT INITIAL.

  UPDATE zsdt0158 SET nro_sol_ov = lw_0158-nro_sol_ov
                      rg_atualizado = '1'
            WHERE sequencial = lw_0158-sequencial.

  COMMIT WORK AND WAIT.

ENDFUNCTION.
