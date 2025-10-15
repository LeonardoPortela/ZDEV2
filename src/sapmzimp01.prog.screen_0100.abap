
PROCESS BEFORE OUTPUT.
  MODULE status_0100.
  MODULE config_tela.

  FIELD w_zimp_cabecalho-tp_arrec MODULE config_imposto.

  LOOP AT t_tc_detalhes INTO w_zimp_detalhe
    WITH CONTROL tc_detalhes.

    MODULE atualiza_total.
    MODULE atualiza_desc_lifnr.
    MODULE config_tela_tc.

  ENDLOOP.

PROCESS AFTER INPUT.

  MODULE exit_user_command AT EXIT-COMMAND.

  LOOP AT t_tc_detalhes.

    CHAIN.
      FIELD:  w_zimp_detalhe-lifnr,
              w_zimp_detalhe-cod_ident,
              w_zimp_detalhe-vlr_principal,
              w_zimp_detalhe-vlr_multa,
              w_zimp_detalhe-vlr_outras_ent,
              w_zimp_detalhe-vlr_atual_mone,
              w_zimp_detalhe-vlr_juros,
* Início Alteração Ricardo Furst 20.07.2009
              w_zimp_detalhe-tse,
* Fim Alteração Ricardo Furst 20.07.2009
              v_total,
              w_zimp_detalhe-sgtxt,
              w_zimp_detalhe-cod_barras,
              w_zimp_detalhe-gsber.

      MODULE atualiza_tc ON CHAIN-REQUEST.

    ENDCHAIN.

    FIELD w_zimp_detalhe-mark MODULE set_mark.

    FIELD w_zimp_detalhe-lifnr MODULE verif_cons_lifnr_det.

  ENDLOOP.

* Verifica necessidade de preenchimento das contas
  FIELD: w_zimp_cabecalho-cta_multa MODULE verif_cta_multa,
         w_zimp_cabecalho-cta_juros MODULE verif_cta_juros.

* Verifica consistência dos dados
  FIELD: w_zimp_cabecalho-bukrs       MODULE verif_cons_bukrs,
         w_zimp_cabecalho-lifnr       MODULE verif_cons_lifnr,
         w_zimp_cabecalho-cta_imposto MODULE verif_cons_cta_imposto,
         w_zimp_cabecalho-cta_juros   MODULE verif_cons_cta_juros,
         w_zimp_cabecalho-cta_multa   MODULE verif_cons_cta_multa,
         w_zimp_cabecalho-tp_arrec    MODULE verif_cons_tp_arrec,
* Início Alteração Ricardo Furst 18.07.2009
         w_zimp_cabecalho-cta_at_mon  MODULE verif_cons_cta_mon,
         w_zimp_cabecalho-cta_tse     MODULE verif_cons_cta_tse.
* Fim Alteração Ricardo Furst 18.07.2009

  MODULE user_command_0100.
