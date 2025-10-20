function z_les_informa_imp_retidos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_BUKRS) TYPE  BUKRS
*"     REFERENCE(P_LIFNR) TYPE  LIFNR
*"     REFERENCE(P_VISUALIZA) TYPE  CHAR01 DEFAULT ' '
*"     REFERENCE(V_BASE_SUGERIDO) TYPE  J_1BBASE OPTIONAL
*"  TABLES
*"      IMP_RETIDOS STRUCTURE  ZLES0043_IMP_RETIDOS
*"  CHANGING
*"     VALUE(P_CANCELADO) TYPE  CHAR01 OPTIONAL
*"  EXCEPTIONS
*"      SEM_IMPOSTOS_RETIDOS
*"      SEM_IMPOSTOS_RETIDOS_BR
*"----------------------------------------------------------------------

  data: it_lfbw                 type table of lfbw with header line,
        it_t059u                type table of t059u with header line,
        wa_zles0043_imp_retidos type zles0043_imp_retidos,
        imp_retidos_bck         type table of zles0043_imp_retidos with header line.

  move imp_retidos[] to imp_retidos_bck[].

  select * into table it_lfbw
    from lfbw
   where lifnr = p_lifnr
     and bukrs = p_bukrs.

  if it_lfbw[] is initial.
    message e071(zles) with p_lifnr p_bukrs raising sem_impostos_retidos.
  endif.

  select * into table it_t059u
    from t059u
     for all entries in it_lfbw
   where spras eq sy-langu
     and land1 eq 'BR'
     and witht eq it_lfbw-witht.

  loop at it_lfbw.

    clear: wa_zles0043_imp_retidos.

    wa_zles0043_imp_retidos-bukrs     = it_lfbw-bukrs.
    wa_zles0043_imp_retidos-lifnr     = it_lfbw-lifnr.

    read table it_t059u with key witht = it_lfbw-witht.
    if sy-subrc is initial.
      wa_zles0043_imp_retidos-text40 = it_t059u-text40.
    endif.

    read table imp_retidos with key witht     = it_lfbw-witht
                                    wt_withcd = it_lfbw-wt_withcd.
    if sy-subrc is initial.
      move-corresponding imp_retidos to wa_zles0043_imp_retidos.
      wa_zles0043_imp_retidos-mark   = c_x_0101.
      modify imp_retidos index sy-tabix from wa_zles0043_imp_retidos transporting mark text40.
    else.
      wa_zles0043_imp_retidos-mark      = space.
      wa_zles0043_imp_retidos-base      = v_base_sugerido.
      wa_zles0043_imp_retidos-witht     = it_lfbw-witht.
      wa_zles0043_imp_retidos-wt_withcd = it_lfbw-wt_withcd.
      append wa_zles0043_imp_retidos to imp_retidos.
    endif.

  endloop.

  if imp_retidos[] is initial.
    message e072(zles) with p_lifnr p_bukrs raising sem_impostos_retidos_br.
  endif.

  perform carrega_impostos_retidos tables imp_retidos using p_bukrs p_lifnr p_visualiza p_cancelado.

  if p_cancelado is initial.
    delete imp_retidos where mark eq space.
  else.
    clear: imp_retidos[].
    move imp_retidos_bck[] to imp_retidos[].
  endif.

endfunction.
