
process before output.
  module liste_initialisieren.
  loop at extract with control
   tctrl_zlest0036 cursor nextline.
    module liste_show_liste.
    module busca_nome_filial.
  endloop.
*
process after input.
  module liste_exit_command at exit-command.
  module liste_before_loop.
  loop at extract.
    module liste_init_workarea.
    chain.
      field zlest0036-nrparametro    .
      field zlest0036-bukrs          .
      field zlest0036-branch         .
      field zlest0036-transportadora .
      field zlest0036-nr_cf_inicial  .
      field zlest0036-nr_cf_final    .
      field zlest0036-OBSERVACAO     .
      module set_update_flag on chain-request.
    endchain.
    field nm_filial  module busca_nome_filial.
    field vim_marked module liste_mark_checkbox.
    chain.
      field zlest0036-nrparametro .
      module liste_update_liste.
    endchain.
  endloop.
  module liste_after_loop.
