
PROCESS BEFORE OUTPUT.

  MODULE zm_status.

  LOOP AT t_t0020 INTO s_t0020 WITH CONTROL tc_t0020
    CURSOR tc_t0020-current_line.
    MODULE zm_trata_campos.
  ENDLOOP.

PROCESS AFTER INPUT.

  LOOP AT t_t0020.

    CHAIN.
      FIELD: s_t0020-periodo,
             s_t0020-referencia,
             s_t0020-dt_premio,
             s_t0020-niv_premio,
             s_t0020-dt_chicago,
             s_t0020-niv_chicago,
             s_t0020-spread,
             s_t0020-spread,
             s_t0020-dt_fix_ptax,
             s_t0020-frete_fob,
             s_t0020-moeda_fob,
             s_t0020-frete_cif,
             s_t0020-custo_financ,
             s_t0020-comissao,
             s_t0020-pis,
             s_t0020-cofins,
             s_t0020-ptax_medio,
             s_t0020-icms
             MODULE z_atualiza_campos ON CHAIN-REQUEST.
    ENDCHAIN.

    FIELD: s_t0020-marc     MODULE z_marc    ON REQUEST,
           s_t0020-vbeln    MODULE z_vbeln   ON REQUEST,
           s_t0020-qtd_mes  MODULE z_qtd_mes ON REQUEST,
           s_t0020-qtd_fix_premio
                            MODULE z_qtd_pre ON REQUEST,
           s_t0020-qtd_fix_chicago
                            MODULE z_qtd_chi ON REQUEST,
           s_t0020-qtd
                            MODULE z_qtd     ON REQUEST.

  ENDLOOP.

  MODULE zm_user_command.

  MODULE zm_exit_command AT EXIT-COMMAND.
