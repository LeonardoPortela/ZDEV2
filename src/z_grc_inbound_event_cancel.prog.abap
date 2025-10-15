*&---------------------------------------------------------------------*
*& Report  Z_GRC_INBOUND_EVENT_CANCEL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report z_grc_inbound_event_cancel.

select * into table @data(tib_cancel_grc)
  from zib_cancel_grc
 where rg_atualizado eq '0'.

loop at tib_cancel_grc into data(wa_tib_cancel_grc).

  data(lc_model) = wa_tib_cancel_grc-chnfe+20(2).

  case lc_model.
    when zif_doc_eletronico=>at_st_model_nfe.

      select single * into @data(wa_zib_nfe_dist_ter)
        from zib_nfe_dist_ter
       where chave_nfe eq @wa_tib_cancel_grc-chnfe.

      if sy-subrc is initial.
        wa_zib_nfe_dist_ter-docsta = '1'.
        wa_zib_nfe_dist_ter-cancel = abap_true.
        modify zib_nfe_dist_ter from wa_zib_nfe_dist_ter.
      endif.

      select single * into @data(wa_zib_nfe_forn)
        from zib_nfe_forn
       where nu_chave eq @wa_tib_cancel_grc-chnfe.

      if sy-subrc is initial.
        wa_zib_nfe_forn-st_nota = '2'.
        wa_zib_nfe_forn-nu_code = wa_tib_cancel_grc-cstat.
        modify zib_nfe_forn from wa_zib_nfe_forn.
      endif.

    when zif_doc_eletronico=>at_st_model_cte.

      select single * into @data(wa_zib_cte_dist_ter)
        from zib_cte_dist_ter
       where cd_chave_cte eq @wa_tib_cancel_grc-chnfe.

      if sy-subrc is initial.
        wa_zib_cte_dist_ter-docsta = '1'.
        wa_zib_cte_dist_ter-cancel = abap_true.
        modify zib_cte_dist_ter from wa_zib_cte_dist_ter.
      endif.

      select single * into @wa_zib_nfe_forn
        from zib_nfe_forn
       where nu_chave eq @wa_tib_cancel_grc-chnfe.

      if sy-subrc is initial.
        wa_zib_nfe_forn-st_nota = '2'.
        wa_zib_nfe_forn-nu_code = wa_tib_cancel_grc-cstat.
        modify zib_nfe_forn from wa_zib_nfe_forn.
      endif.

  endcase.

endloop.

commit work.
