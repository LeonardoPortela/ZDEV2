*&---------------------------------------------------------------------*
*&  Include           ZGL015_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_verifica_erros.
  data: vl_sum_int     type zglt036-vlr_moeda_int,
        vl_sum_for     type zglt036-vlr_moeda_forte,
        vl_sum_grp     type zglt036-vlr_moeda_grupo,
        vl_sum_doc     type zglt036-vlr_moeda_doc,

        vl_sum_ints    type zglt036-vlr_moeda_int,
        vl_sum_fors    type zglt036-vlr_moeda_forte,
        vl_sum_grps    type zglt036-vlr_moeda_grupo,
        vl_sum_docs    type zglt036-vlr_moeda_doc,

        vl_pos_int     type zglt036-vlr_moeda_int,
        vl_pos_for     type zglt036-vlr_moeda_forte,
        vl_pos_doc     type zglt036-vlr_moeda_doc,
        vl_pos_grp     type zglt036-vlr_moeda_grupo,
        var_qte        type i,
        wl_linha(6),
        tabix          type sy-tabix,
        p_data_val     type sy-datum,
        vl_qtde        type i,
        vl_tam         type i,
        wl_zglt034     type zglt034,
        wl_zglt037     type zglt037,
        wl_zglt031     type zglt031,
        wl_tcurc       type tcurc,
        wl_tgsb        type tgsb,
        wl_tbsl        type tbsl,
        wl_j_1bbranch  type j_1bbranch,
        wl_lfbk        type lfbk,
        wl_knbk        type knbk,
        wl_lfa1        type lfa1,
        wl_lfb1        type lfb1,
        wl_kna1        type kna1,
        wl_t880        type t880,
        wl_knb1        type knb1,
        wl_t012        type t012,
        wl_tka02       type tka02,
        wl_aufk        type aufk,
        wl_mara        type mara,
        wl_csks        type csks,
        wl_cepc        type cepc,
        wl_ska1        type ska1,
        wl_skb1        type skb1,
        wl_t042z       type t042z,
        wl_t001        type t001,
        wl_t005        type t005,
        wl_t003        type t003,
        wl_t007a       type t007a,
        wl_t030k       type t030k,
        wa_t095        type t095,
        wa_caufv       type caufv,
        wa_afvc        type afvc,
        l_cobl         type cobl,

        tl_zglt036     type table of zglt036,
        wl_zglt036     type zglt036,
        tl_tbsl2       type table of tbsl,
        wl_tbsl2       type tbsl,
        vpgt_forn(1),
        vpgt_pgt(1),

        w_flagi(1),
        w_flagm(1),
        w_flagt(1),
        w_div1(1),
        w_div2(1),
        e_status(1),
        e_messa(64),
        vg_depto       type zfit0033-dep_resp,
        vbudat(10),
        vbudat2        type bsis-budat,
        vg_last_day    type sy-datum,
        vg_first_day   type sy-datum,
        vdata(10),
        smes(2),
        vmes           type i,
        sano(4),
        vano           type i,
        w_civa         type i,
        xsako          type xsako,
        tl_tbsl        type table of tbsl      with header line,
        tl_cskb        type table of cskb      with header line,
        tl_cepc        type table of cepc      with header line,
        wl_tabwt       type tabwt,
        wl_anla        type anla,
        wa_data        type sy-datum,
        wl_n_uteis     type sy-index,

        v_anln1        type anla-anln1,
        v_anln2        type anla-anln2,

        vakont         type lfb1-akont,
        msge(50)       type c,
        flagm(1)       type c,
        tl_zfit0030    type table of zfit0030  with header line,
        tl_setleaf     type table of setleaf   with header line,
        t_hkont        type standard table of  rgsb4 with header line,
        t_divisao      type standard table of  rgsb4  with header line,
        wa_datas       type iscal_day,
        wl_sab_dom_fer type table of iscal_day with header line.

  clear:    tg_msg_ret.
  refresh:  tg_msg_ret.

  " Divisões de empresas
  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = 'MAGGI_ZGL0016_DIV'
    tables
      set_values    = t_divisao
    exceptions
      set_not_found = 1
      others        = 2.
  if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  sort t_divisao by from.

  select single *
    from t001
    into wl_t001
    where bukrs = wg_zglt035-bukrs.

  select single *
    from t005
    into wl_t005
    where land1 = wl_t001-land1.

  "ALRS verificar se LOTE é de fornecedor/cliente
  refresh: tl_tbsl2, tl_zglt036.
  select  *
    from zglt035
   inner join zglt036
    on zglt036~doc_lcto eq zglt035~doc_lcto
     into corresponding fields of table tl_zglt036
  where zglt035~lote     =  wg_zglt034-lote
  and   zglt035~loekz    = ''.

  if wl_t001-land1 ne 'NL'.
    loop at tl_zglt036 into wl_zglt036.
      if wl_zglt036-hbkid is not initial or wl_zglt036-zlsch is not  initial.
        select *
        from tbsl
        into table tl_tbsl2
        where bschl eq  wl_zglt036-bschl
        and   koart eq 'K'.
        if tl_tbsl2[] is not initial.
          exit.
        endif.
      endif.
    endloop.
  endif.

  clear vpgt_forn. "checa tem pagamento no lote corrente
  loop at tg_zglt036 into wg_zglt036.
    select single *
      from tbsl
      into wl_tbsl2
      where bschl = wg_zglt036-bschl
      and   koart in ('K').

    if sy-subrc = 0 and
       ( wg_zglt036-hbkid is not initial or wg_zglt036-zlsch is not initial ).
      vpgt_forn = 'X'.
      exit.
    endif.

  endloop.

  clear vpgt_pgt.
  if tl_zglt036[] is not initial.
    if tl_tbsl2[] is initial and vpgt_forn = 'X'.
      move: 'WG_ZGLT034-LOTE'   to tg_msg_ret-field,
            text-e89            to tg_msg_ret-msg.
      append tg_msg_ret.  clear: tg_msg_ret.
      vpgt_pgt = 'X'.
    endif.
  endif.

  if tl_tbsl2[] is not initial and vpgt_forn = ''.
    move: 'WG_ZGLT034-LOTE'   to tg_msg_ret-field,
          text-e90            to tg_msg_ret-msg.
    append tg_msg_ret.  clear: tg_msg_ret.
    vpgt_pgt = 'X'.
  endif.

  refresh: tl_tbsl2.
  if vpgt_pgt is initial.
    if tl_zglt036[] is not  initial. "
      select *
         from tbsl
         into table tl_tbsl2
         for all entries in  tl_zglt036
         where bschl eq  tl_zglt036-bschl
         and   koart in ('K', 'D').
    endif.

    loop at tg_zglt036[] into data(wl_036).

      if ( wl_036-bschl eq 19
        or wl_036-bschl eq 31
        or wl_036-bschl eq 21
        or wl_036-bschl eq 29
        or wl_036-bschl eq 39
        or wl_036-bschl eq 24
        or wl_036-bschl eq 34
        or wl_036-bschl eq 01
        or wl_036-bschl eq 02
        or wl_036-bschl eq 11
        or wl_036-bschl eq 12
        or wl_036-bschl eq 09
        or wl_036-bschl eq 19 ) and ( wl_036-dt_vct is initial ).
        move: 'WG_ZGLT036-DT_VCT' to tg_msg_ret-field,
               text-f01 to tg_msg_ret-msg.
*              'Dt. De Vencimento obrigatória quando Cliente e Fornecedor.' TO TG_MSG_RET-MSG.
        append tg_msg_ret.  clear: tg_msg_ret.
      endif.

    endloop.

    clear: wl_036.



* Valida se a conta informada é permitida somente para lançamentos internos
    loop at tg_zglt036 into wl_036.

      select single *
              from tbsl
              into @data(wl_tbsl3)
              where bschl eq @wl_036-bschl.

      if ( wl_036-hkont is not initial  and  wl_tbsl3-koart = 'S' ).
        select single xintb from skb1 into @data(vl_intb)
          where bukrs eq @wg_zglt035-bukrs
            and saknr eq @wl_036-hkont+0(10)
            and xintb eq @abap_true.
        if ( sy-subrc eq 0 ).
          tg_msg_ret-field  = 'WG_ZGLT036-HKONT'.
          tg_msg_ret-msg    = |{ wl_036-hkont }<-{ text-f03 }|.
          append tg_msg_ret.    clear tg_msg_ret.
        endif.
        clear: vl_intb.
      endif.
    endloop.

    clear: wl_036.

    clear vpgt_forn. "checa tem forn/cliente no lote corrente
    loop at tg_zglt036 into wg_zglt036.
      select single *
        from tbsl
        into wl_tbsl2
        where bschl = wg_zglt036-bschl
        and   koart in ('K', 'D').

      if sy-subrc = 0.
        vpgt_forn = 'X'.
        exit.
      endif.

    endloop.

    if tl_zglt036[] is not initial.
      if tl_tbsl2[] is initial and vpgt_forn = 'X'.
        move: 'WG_ZGLT034-LOTE'   to tg_msg_ret-field,
              text-e83            to tg_msg_ret-msg.
        append tg_msg_ret.  clear: tg_msg_ret.
      endif.
    endif.

    if tl_tbsl2[] is not initial and vpgt_forn = ''.
      move: 'WG_ZGLT034-LOTE'   to tg_msg_ret-field,
            text-e84            to tg_msg_ret-msg.
      append tg_msg_ret.  clear: tg_msg_ret.
    endif.
  endif.

  "LOCAL PARA EDIÇÃO
  if wg_zglt035-bukrs is not initial and
     wg_zglt035-budat is not initial.
    select single * from zglt034 into wl_zglt034 where lote eq wg_zglt034-lote.
    vg_depto = wl_zglt034-dep_resp.
    call function 'Z_CONTROLE_FECHAMES'
      exporting
        i_bukrs    = wg_zglt035-bukrs
        i_data     = wg_zglt035-budat
        i_dep_resp = vg_depto
        i_monat    = wg_zglt035-monat
      importing
        e_status   = e_status
        e_messa    = e_messa
      exceptions
        error      = 1
        others     = 2.
    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
    if  e_status = 'E'.
      move: 'WG_ZGLT034-LOTE'      to tg_msg_ret-field,
             e_messa               to tg_msg_ret-msg.
      append tg_msg_ret.  clear: tg_msg_ret.
    endif.
  endif.


  authority-check object 'F_SKA1_BUK'
       id 'ACTVT' field '03'       "display
       id 'BUKRS' field wg_zglt035-bukrs.
  if sy-subrc <> 0.
    move: 'WG_ZGLT034-LOTE'      to tg_msg_ret-field,
    text-e86      to tg_msg_ret-msg.
    append tg_msg_ret.  clear: tg_msg_ret.
  endif.


  if  wg_zglt034-lote is initial.
    move: 'WG_ZGLT034-LOTE'      to tg_msg_ret-field,
          text-e12      to tg_msg_ret-msg.
    append tg_msg_ret.  clear: tg_msg_ret.
  else.
    select single * from zglt034 into wl_zglt034 where lote eq wg_zglt034-lote.
    if sy-subrc ne 0.
      move: 'WG_ZGLT034-LOTE'      to tg_msg_ret-field,
            text-e13  to tg_msg_ret-msg.
      append tg_msg_ret.  clear: tg_msg_ret.
    elseif wl_zglt034-status_lote = 'L'.
      move: 'WG_ZGLT034-LOTE'      to tg_msg_ret-field,
            text-e14   to tg_msg_ret-msg.
      append tg_msg_ret.  clear: tg_msg_ret.
    elseif wl_zglt034-status_lote = 'A'.
      move: 'WG_ZGLT034-LOTE'      to tg_msg_ret-field,
            text-e52   to tg_msg_ret-msg.
      append tg_msg_ret.  clear: tg_msg_ret.
    endif.
  endif.

  if  wg_zglt035-tp_lcto is initial.
*    MOVE: 'WG_ZGLT035-TP_LCTO'              TO TG_MSG_RET-FIELD,
*          TEXT-E15       TO TG_MSG_RET-MSG.
*    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  else.
    select single * from zglt031 into wl_zglt031 where tp_lcto eq wg_zglt035-tp_lcto.
    if sy-subrc ne 0.
      move: 'WG_ZGLT035-TP_LCTO'              to tg_msg_ret-field,
            text-e16  to tg_msg_ret-msg.
      append tg_msg_ret.  clear: tg_msg_ret.
    endif.
  endif.

  if  wg_zglt035-budat is initial.
    move: 'WG_ZGLT035-BUDAT'                to tg_msg_ret-field,
          text-e17    to tg_msg_ret-msg.
    append tg_msg_ret.  clear: tg_msg_ret.
  elseif wg_zglt035-bukrs is not initial.
    call function 'Z_RET_DATA_MES_ABERTO'
      exporting
        p_data_ent  = wg_zglt035-budat
        p_bukrs     = wg_zglt035-bukrs
        p_periodo   = wg_zglt035-monat
      importing
        p_data_val  = p_data_val
      exceptions
        sem_periodo = 1
        others      = 2.

    if not sy-subrc is initial.
      "   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

    "IF P_DATA_VAL+0(6) NE WG_ZGLT035-BUDAT+0(6) AND WG_ZGLT035-MONAT LE 12.
    if p_data_val+0(6) ne wg_zglt035-budat+0(6) and wg_zglt035-monat le 16.
      move: 'WG_ZGLT035-BUDAT'                to tg_msg_ret-field,
             text-e18    to tg_msg_ret-msg.
      append tg_msg_ret.  clear: tg_msg_ret.
    endif.
    if wg_zglt035-prov_est = 'X'.
      concatenate  wg_zglt035-budat+6(2) wg_zglt035-budat+4(2) wg_zglt035-budat+0(4) into vbudat separated by '.'.
      if wg_zglt035-budat+4(2) = '12'.
        vano = wg_zglt035-budat+0(4).
        add 1 to vano.
        sano = vano.
        concatenate sano '0101' into vbudat2.
      else.
        vmes = wg_zglt035-budat+4(2).
        add 1 to vmes.
        smes = vmes.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = smes
          importing
            output = smes.
        concatenate wg_zglt035-budat+0(4) smes '01' into vbudat2.
      endif.
      concatenate vbudat2+6(2) '.' vbudat2+4(2) '.' vbudat2+0(4) into vdata.

      call function 'Z_RET_DATA_MES_ABERTO'
        exporting
          p_data_ent  = vbudat2
          p_bukrs     = wg_zglt035-bukrs
        importing
          p_data_val  = p_data_val
        exceptions
          sem_periodo = 1
          others      = 2.

      if not sy-subrc is initial.
        "   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.

      if p_data_val+0(6) ne vbudat2+0(6) and vbudat2+4(2) le 12.
        move: 'WG_ZGLT035-BUDAT'                to tg_msg_ret-field,
               text-e18    to tg_msg_ret-msg.
        concatenate tg_msg_ret-msg vdata into tg_msg_ret-msg separated by space.
        append tg_msg_ret.  clear: tg_msg_ret.
      endif.
    endif.

  endif.

  if  wg_zglt035-bldat is initial.
    move: 'WG_ZGLT035-BLDAT'                to tg_msg_ret-field,
          text-e19     to tg_msg_ret-msg.
    append tg_msg_ret.  clear: tg_msg_ret.
  endif.

  " ajuste IR092504 - Inclusão do ano na validação do periodo
  if  wg_zglt035-budat+4(2) ne wg_zglt035-monat. "and wg_zglt035-budat(4) ne wg_zglt035-gjahr.
    " ajuste IR092504 - Inclusão do ano na validação do periodo
    if wg_zglt035-budat+4(2) = '12' and '12_13_14_15_16' cs wg_zglt035-monat.
      "OK
    else.
      move: 'WG_ZGLT035-MONAT'                to tg_msg_ret-field,
            text-e81     to tg_msg_ret-msg.
      append tg_msg_ret.  clear: tg_msg_ret.
    endif.
  endif.


  " ajuste IR092504 - Inclusão do ano na validação do periodo
  if   wg_zglt035-budat(4) ne wg_zglt035-gjahr.
    move: 'WG_ZGLT035-GJAHR'                to tg_msg_ret-field,
          text-f04     to tg_msg_ret-msg.
    append tg_msg_ret.  clear: tg_msg_ret.
  endif.
  " ajuste IR092504 - Inclusão do ano na validação do periodo

  if wg_zglt035-moeda_doc is initial .
    move: c_ts_100-tab1 to tg_msg_ret-aba,
          text-e20 to tg_msg_ret-msg,
          'WG_ZGLT035-MOEDA_DOC'    to tg_msg_ret-field.
    append tg_msg_ret.
    clear: tg_msg_ret.
  else.
    select single *
       from tcurc
       into wl_tcurc
        where  waers eq wg_zglt035-moeda_doc.
    if sy-subrc ne 0.
      move:  c_ts_100-tab1 to tg_msg_ret-aba,
             text-e21    to tg_msg_ret-msg,
             'WG_ZGLT035-MOEDA_DOC'          to tg_msg_ret-field.

      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.
  endif.

  if wg_zglt035-blart is initial .
    move: c_ts_100-tab1 to tg_msg_ret-aba,
          text-e22 to tg_msg_ret-msg,
          'WG_ZGLT035-BLART'    to tg_msg_ret-field.
    append tg_msg_ret.
    clear: tg_msg_ret.
  else.
    select single *
       from t003
       into wl_t003
        where  blart eq wg_zglt035-blart.
    if sy-subrc ne 0.
      move:  c_ts_100-tab1 to tg_msg_ret-aba,
             text-e23     to tg_msg_ret-msg,
             'WG_ZGLT035-BLART'          to tg_msg_ret-field.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.

    "CS2017002479 - 21.11.2017 - Ini
    if wg_zglt035-blart = 'SG'. "Seguro
      move:  c_ts_100-tab1 to tg_msg_ret-aba,
             text-ee7      to tg_msg_ret-msg,
             'WG_ZGLT035-BLART'          to tg_msg_ret-field.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.
    "CS2017002479 - 21.11.2017 - Fim

  endif.

  if wg_zglt035-moeda_int_hist = 'X' .
    if wg_zglt035-moeda_interna is initial .
      move: c_ts_100-tab1 to tg_msg_ret-aba,
            text-e24        to tg_msg_ret-msg,
            'WG_ZGLT035-MOEDA_INTERNA'    to tg_msg_ret-field.
      append tg_msg_ret.
      clear: tg_msg_ret.
    else.
      select single *
         from tcurc
         into wl_tcurc
          where  waers eq wg_zglt035-moeda_interna.
      if sy-subrc ne 0.
        move:  c_ts_100-tab1 to tg_msg_ret-aba,
               text-e25     to tg_msg_ret-msg,
               'WG_ZGLT035-MOEDA_INTERNA'    to tg_msg_ret-field.

        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.
    endif.
  endif.

  if wg_zglt035-moeda_ft_hist = 'X'.
    if wg_zglt035-moeda_forte is initial .
      move: c_ts_100-tab1 to tg_msg_ret-aba,
            text-e26      to tg_msg_ret-msg,
            'WG_ZGLT035-MOEDA_FORTE'    to tg_msg_ret-field.
      append tg_msg_ret.
      clear: tg_msg_ret.
    else.
      select single *
         from tcurc
         into wl_tcurc
          where  waers eq wg_zglt035-moeda_forte.
      if sy-subrc ne 0.
        move:  c_ts_100-tab1 to tg_msg_ret-aba,
               text-e27      to tg_msg_ret-msg,
               'WG_ZGLT035-MOEDA_FORTE'    to tg_msg_ret-field.

        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.
    endif.
  endif.

  select * from tbsl into table tl_tbsl.

  select single *
          from tka02
          into wl_tka02
          where bukrs  = wg_zglt035-bukrs.

  select  *                            "#EC CI_DB_OPERATION_OK[2389136]
    from cskb
    into table tl_cskb
    where  kokrs  = wl_tka02-kokrs
    and    datab  le sy-datum
    and    datbi  ge sy-datum.

  select  *
    from cepc
    into table tl_cepc
    where  kokrs  = wl_tka02-kokrs
    and    datab  le sy-datum
    and    datbi  ge sy-datum.

  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = 'MAGGI_CTAS_TPMV_BPC'
    tables
      set_values    = t_hkont
    exceptions
      set_not_found = 1
      others        = 2.
  if sy-subrc <> 0.

  endif.

  select * from setleaf into table tl_setleaf where setname eq 'CONTAS_EC-CS'.

  sort: tl_tbsl     by bschl,
    tl_cskb     by kstar,
    tl_cepc     by prctr,
    tl_zfit0030 by hkont,
    tl_setleaf  by valfrom.

  " somente para lançamentos inseridos
  if wg_zglt035-tp_lcto eq  0.

    loop at tg_zglt036 into wg_zglt036.
      wl_linha = sy-tabix.
      read table tl_tbsl into tl_tbsl with key bschl = wg_zglt036-bschl binary search.
      if wg_zglt036-vbeln is not initial.
        if tl_tbsl-koart eq 'D'.
          select single *
            from vbak
            into @data(_vbak)
            where vbeln = @wg_zglt036-vbeln.
          if sy-subrc ne 0 .
            move:  c_ts_100-tab1 to tg_msg_ret-aba.
            concatenate text-ee8 '' wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.
            clear: tg_msg_ret.
          endif.
        else.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate text-ee9 '' wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.
          clear: tg_msg_ret.
        endif.
      endif.

      if wg_zglt035-st_ap_fiscal = 'X' and wg_zglt036-matnr_fi is initial.
        move:  c_ts_100-tab1 to tg_msg_ret-aba.
        concatenate text-e92 '' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.

      if wg_zglt036-bschl is initial
        and ( wg_zglt036-hkont is initial or wg_zglt036-umskz is initial or wg_zglt036-anbwa is initial or
              wg_zglt036-bewar is initial or wg_zglt036-vbund is initial or wg_zglt036-kostl is initial or
              wg_zglt036-prctr is initial or wg_zglt036-aufnr is initial or wg_zglt036-matnr is initial or
              wg_zglt036-zuonr is initial or wg_zglt036-sgtxt is initial ).

        move:  c_ts_100-tab1 to tg_msg_ret-aba.
        concatenate text-e61 '' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.

      if wg_zglt036-hkont is initial.
        move:  c_ts_100-tab1 to tg_msg_ret-aba.
        concatenate text-e62 '' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      else.
        select single *                "#EC CI_DB_OPERATION_OK[2389136]
          from ska1                    "#EC CI_DB_OPERATION_OK[2431747]
          into wl_ska1
          where ktopl = '0050'
          and   saknr = wg_zglt036-hkont+0(10).
        read table tl_tbsl into tl_tbsl with key bschl = wg_zglt036-bschl binary search.

        if wl_ska1-ktoks = 'YB03' and tl_tbsl-koart ne 'D' and tl_tbsl-koart ne 'K'.
          select single *              "#EC CI_DB_OPERATION_OK[2431747]
             from skb1
             into wl_skb1
             where bukrs = wg_zglt035-bukrs
             and   saknr = wg_zglt036-hkont+0(10).
          if sy-subrc eq 0.
            if wl_skb1-mitkz is not initial.
              move:  c_ts_100-tab1 to tg_msg_ret-aba.
              concatenate text-e63'' wg_zglt036-hkont text-e64 '' wl_linha into  tg_msg_ret-msg separated by space.
              append tg_msg_ret.
              clear: tg_msg_ret.
            endif.
          endif.
        endif.

      endif.

      if wg_zglt036-anbwa is initial.
        clear: tl_tbsl.
        read table tl_tbsl into tl_tbsl with key bschl = wg_zglt036-bschl
                                                 koart = 'A'
                                        binary search.
        if sy-subrc eq 0.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate text-e65 '' wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.
          clear: tg_msg_ret.
        endif.
      else.
        select single *
          from tabwt
          into wl_tabwt
          where bwasl = wg_zglt036-anbwa.
        if sy-subrc ne 0.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate text-e66 '' wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.
          clear: tg_msg_ret.
        endif.
      endif.

      if wg_zglt036-umskz is initial.
        clear: tl_tbsl.
        read table tl_tbsl into tl_tbsl with key bschl = wg_zglt036-bschl
                                                 xsonu = 'X'
                                        binary search.
        if sy-subrc eq 0.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate text-e67 '' wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.
          clear: tg_msg_ret.
        endif.
      endif.

      if wg_zglt036-umskz is not initial.
        clear: tl_tbsl.
        read table tl_tbsl into tl_tbsl with key bschl = wg_zglt036-bschl
                                                 xsonu = 'X'
                                        binary search.
        if sy-subrc ne 0.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate text-e68 '' wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.
          clear: tg_msg_ret.
        endif.
      endif.

      clear: tl_tbsl, vakont.
      read table tl_tbsl into tl_tbsl with key bschl = wg_zglt036-bschl binary search.

      if sy-subrc eq 0.
        case tl_tbsl-koart.
          when 'K'.
            select single * from lfa1 into wl_lfa1 where lifnr eq wg_zglt036-hkont+0(10).
            if sy-subrc ne 0.
              move:  c_ts_100-tab1 to tg_msg_ret-aba.
              concatenate text-e69 '' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.
              clear: tg_msg_ret.
            endif.
            select single akont into vakont from lfb1 where lifnr = wg_zglt036-hkont+0(10) and bukrs = wg_zglt035-bukrs.
            clear wl_lfa1.
            select single *
              from lfa1
              into wl_lfa1
              where lifnr = wg_zglt036-hkont+0(10).
            if wl_lfa1-sperr = 'X'.
              move:  c_ts_100-tab1               to tg_msg_ret-aba.
              concatenate text-e70 ''  wg_zglt036-hkont text-e71 '' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.  clear: tg_msg_ret.
            endif.
          when 'D'.
            select single * from kna1 into wl_kna1 where kunnr eq wg_zglt036-hkont+0(10).
            if sy-subrc ne 0.
              move:  c_ts_100-tab1 to tg_msg_ret-aba.
              concatenate text-e72 '' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.
              clear: tg_msg_ret.
            endif.
            select single akont into vakont from knb1 where kunnr = wg_zglt036-hkont+0(10) and bukrs = wg_zglt035-bukrs.
            clear wl_kna1.
            select single *
              from kna1
              into wl_kna1
              where kunnr = wg_zglt036-hkont+0(10).
            if wl_kna1-sperr = 'X'.
              move:  c_ts_100-tab1               to tg_msg_ret-aba.
              concatenate text-e73 ''  wg_zglt036-hkont text-e71 '' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.  clear: tg_msg_ret.
            endif.
          when 'A'. " Imobilizado
            clear  wl_ska1.
            split wg_zglt036-hkont at '-' into v_anln1 v_anln2.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = v_anln1
              importing
                output = v_anln1.

            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = v_anln2
              importing
                output = v_anln2.
            select single * from anla into wl_anla  where bukrs eq wg_zglt035-bukrs
                                                 and   anln1 eq v_anln1
                                                 and   anln2 eq v_anln2.
            if sy-subrc ne 0.
              move:  c_ts_100-tab1 to tg_msg_ret-aba.
              concatenate text-e93 '' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.
              clear: tg_msg_ret.
            endif.
            select single * from t095 into wa_t095
                          where ktopl = '0050'
                          and   ktogr = wl_anla-ktogr
                          and   afabe = 1.
            vakont = wa_t095-ktansw.
            select single *            "#EC CI_DB_OPERATION_OK[2389136]
              from ska1                "#EC CI_DB_OPERATION_OK[2431747]
              into wl_ska1
              where ktopl = '0050'
              and   saknr = vakont.
            if  wl_ska1-xspeb = 'X'.
              move:  c_ts_100-tab1               to tg_msg_ret-aba.
              concatenate text-e63 ''  wg_zglt036-hkont text-e71 '' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.  clear: tg_msg_ret.
            endif.
          when others.
            clear  wl_ska1.
            vakont = wg_zglt036-hkont.
            select single *            "#EC CI_DB_OPERATION_OK[2431747]
              from ska1                "#EC CI_DB_OPERATION_OK[2389136]
              into wl_ska1
              where ktopl = '0050'
              and   saknr = wg_zglt036-hkont+0(10).
            if  wl_ska1-xspeb = 'X'.
              move:  c_ts_100-tab1               to tg_msg_ret-aba.
              concatenate text-e63 ''  wg_zglt036-hkont text-e71 '' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.  clear: tg_msg_ret.
            endif.

        endcase.
      endif.
      read table t_hkont with key from = vakont.
      if sy-subrc eq 0.
        if wg_zglt036-bewar is initial.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate text-e74 '' wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.
          clear: tg_msg_ret.
        else.
          select *
           into table tl_zfit0030
           from zfit0030
           where hkont  = vakont .
          if tl_zfit0030[] is not initial.
            clear: msge, flagm.
            flagm = 'N'.
            loop at tl_zfit0030.
              if tl_zfit0030-bewar eq wg_zglt036-bewar.
                flagm = 'S'.
              endif.
            endloop.
            if flagm = 'N'.
              loop at tl_zfit0030.
                concatenate msge tl_zfit0030-bewar into msge separated by space.
              endloop.
              concatenate text-e75 '' msge into msge separated by space.
              move:  c_ts_100-tab1 to tg_msg_ret-aba.
              concatenate msge wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.
              clear: tg_msg_ret.
            endif.
          endif.
        endif.
      endif.

      if wg_zglt036-vbund is initial.
        clear: tl_setleaf.
        read table tl_setleaf into tl_setleaf with key valfrom = wg_zglt036-akont "conta do razão
                                              binary search.
        if sy-subrc eq 0.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate text-e76 '' wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.
          clear: tg_msg_ret.
        endif.
      else.
        select single * from t880 into wl_t880 where rcomp eq wg_zglt036-vbund.
        if sy-subrc ne 0.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate text-e79 '' wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.
          clear: tg_msg_ret.
        else.
          clear: tl_tbsl, vakont.
          read table tl_tbsl into tl_tbsl with key bschl = wg_zglt036-bschl binary search.
          if sy-subrc eq 0.
            case tl_tbsl-koart.
              when 'K'.
                select single * from lfa1 into wl_lfa1 where lifnr eq wg_zglt036-hkont+0(10).
                if sy-subrc eq 0.
                  if wg_zglt036-vbund ne wl_lfa1-vbund and wl_lfa1-ktokk ne 'IFRS'.
                    move:  c_ts_100-tab1 to tg_msg_ret-aba.
                    concatenate text-e79 '' wl_linha into  tg_msg_ret-msg.
                    append tg_msg_ret.
                    clear: tg_msg_ret.
                  endif.
                endif.
              when 'D'.
                select single * from kna1 into wl_kna1 where kunnr eq wg_zglt036-hkont+0(10).
                if sy-subrc eq 0.
                  if wg_zglt036-vbund ne wl_kna1-vbund and wl_kna1-ktokd ne 'IFRS'.
                    move:  c_ts_100-tab1 to tg_msg_ret-aba.
                    concatenate text-e79 '' wl_linha into  tg_msg_ret-msg.
                    append tg_msg_ret.
                    clear: tg_msg_ret.
                  endif.
                endif.
            endcase.
          endif.
        endif.
      endif.

      clear: wg_zglt036, tl_tbsl, tl_zfit0030, tl_setleaf, tl_cskb.
    endloop.
  endif.

  clear: vl_sum_ints, vl_sum_fors, vl_sum_grps,vl_sum_docs. "sociedade parceira
  clear: wg_zglt036, vl_sum_int, vl_sum_for, vl_sum_grp,vl_sum_doc.
  clear:             vl_pos_int, vl_pos_for, vl_pos_grp,vl_pos_doc.

*  SELECT SINGLE * FROM ZGLT031 INTO WG_ZGLT031 WHERE TP_LCTO EQ WG_ZGLT035-TP_LCTO.

  clear: w_flagm,w_flagi,w_flagt.
  vl_qtde = 0.
  w_civa = 0.
  loop at tg_zglt036 into wg_zglt036.
    wl_linha = sy-tabix.
    if wg_zglt036-umskz = 'F'.
      move:  c_ts_100-tab1 to tg_msg_ret-aba.
      concatenate text-e68 '' wl_linha into  tg_msg_ret-msg.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.

    if wg_zglt036-check = 'X'.
      vl_tam = strlen( wg_zglt036-sgtxt ).
      if vl_tam le 0.
        move:  c_ts_100-tab1 to tg_msg_ret-aba.
        concatenate text-ee1 '' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.
      loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
        tabix  = sy-tabix.
        if wg_obj-quantity ne 0 and wg_obj-base_uom is initial.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate text-e96 '' wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.
          clear: tg_msg_ret.
        endif.

        if wg_obj-quantity eq 0 and wg_obj-base_uom is not initial.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate text-e97 '' wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.
          clear: tg_msg_ret.
        endif.
        if '40_50' cs wg_zglt036-bschl and wg_zglt035-bukrs ne '0100'. "ALRS
          clear: tl_cskb.
          read table tl_cskb into tl_cskb with key kstar = wg_zglt036-hkont+0(10)
                                          binary search.
          if sy-subrc eq 0.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = wg_obj-prctr
              importing
                output = wg_obj-prctr.
            if tl_cskb-katyp eq '01' and wg_obj-matnr is not initial .
              move:  c_ts_100-tab1 to tg_msg_ret-aba.
              concatenate text-ee2 '' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.
              clear: tg_msg_ret.
            endif.
            if tl_cskb-katyp eq '01' and wg_obj-kostl is initial .
              if wg_obj-aufnr is initial.
                move:  c_ts_100-tab1 to tg_msg_ret-aba.
                concatenate text-e02 '' wl_linha into  tg_msg_ret-msg.
                append tg_msg_ret.
                clear: tg_msg_ret.
              endif.
            elseif tl_cskb-katyp eq '01' and wg_obj-aufnr is not initial .

              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = wg_zglt036-aufnr
                importing
                  output = wg_zglt036-aufnr.

              "checar ccusto da ordem aqui
              select single *
                   from aufk
                   into wl_aufk
                   where bukrs  = wl_zglt034-bukrs
                   and   aufnr  = wg_zglt036-aufnr.
              if sy-subrc eq 0.
                if wg_obj-kostl ne wl_aufk-kostl. "KOSTV.
                  move:  c_ts_100-tab1 to tg_msg_ret-aba.
                  concatenate text-e77 '' wl_linha into  tg_msg_ret-msg.
                  append tg_msg_ret.
                  clear: tg_msg_ret.
                endif.
                if wl_aufk-autyp = '30'.
                  if wg_zglt036-vornr  is initial.
                    move:  c_ts_100-tab1 to tg_msg_ret-aba.
                    concatenate text-ee4 '' wl_linha into  tg_msg_ret-msg.
                    append tg_msg_ret.
                    clear: tg_msg_ret.
                  else.
                    select single *
                      from caufv
                      into wa_caufv
                      where aufnr  = wg_zglt036-aufnr.
                    select single *
                      from afvc
                      into wa_afvc
                      where aufpl = wa_caufv-aufpl
                      and   vornr = wg_zglt036-vornr.
                    if sy-subrc ne 0.
                      move:  c_ts_100-tab1 to tg_msg_ret-aba.
                      concatenate text-ee5 '' wl_linha into  tg_msg_ret-msg.
                      append tg_msg_ret.
                      clear: tg_msg_ret.
                    endif.
                  endif.
                elseif wg_zglt036-vornr is not initial.
                  move:  c_ts_100-tab1 to tg_msg_ret-aba.
                  concatenate text-ee6 '' wl_linha into  tg_msg_ret-msg.
                  append tg_msg_ret.
                  clear: tg_msg_ret.

                endif.
              endif.
            elseif '11_12' cs tl_cskb-katyp  and wg_obj-prctr is initial and wg_obj-matnr is initial.
              move:  c_ts_100-tab1 to tg_msg_ret-aba.
              concatenate text-e98 '' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.
              clear: tg_msg_ret.
            elseif '11_12' cs tl_cskb-katyp and wg_obj-prctr eq '0000009900' and wg_obj-matnr is initial.
              move:  c_ts_100-tab1 to tg_msg_ret-aba.
              concatenate text-e99 '' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.
              clear: tg_msg_ret.
            elseif '11_12' cs tl_cskb-katyp and wg_obj-prctr ne '0000009900' and wg_obj-matnr is not initial.
              wg_obj-prctr = '0000009900'.
              modify tg_obj from wg_obj index tabix transporting prctr.
            elseif '11_12' cs tl_cskb-katyp  and wg_obj-prctr is not initial.
              read table tl_cepc into tl_cepc with key  prctr = wg_obj-prctr
                                        binary search.
              if sy-subrc ne 0.
                move:  c_ts_100-tab1 to tg_msg_ret-aba.
                concatenate text-e77 '' wl_linha into  tg_msg_ret-msg.
                append tg_msg_ret.
                clear: tg_msg_ret.
              endif.
            endif.
          endif.
        endif.
      endloop.

      add:  wg_zglt036-vlr_moeda_int    to vl_sum_int,
            wg_zglt036-vlr_moeda_forte  to vl_sum_for,
            wg_zglt036-vlr_moeda_doc    to vl_sum_doc,
            wg_zglt036-vlr_moeda_grupo  to vl_sum_grp.

      if wg_zglt036-vbund is not initial. "totaliza sociedade parceira
        add:  wg_zglt036-vlr_moeda_int    to vl_sum_ints,
              wg_zglt036-vlr_moeda_forte  to vl_sum_fors,
              wg_zglt036-vlr_moeda_doc    to vl_sum_docs,
              wg_zglt036-vlr_moeda_grupo  to vl_sum_grps.
      endif.
      add 1 to  vl_qtde.
      if wg_zglt036-vlr_moeda_int gt 0.
        add  wg_zglt036-vlr_moeda_int    to vl_pos_int.
      endif.
      if wg_zglt036-vlr_moeda_forte gt 0.
        add  wg_zglt036-vlr_moeda_forte    to vl_pos_for.
      endif.
      if wg_zglt036-vlr_moeda_grupo gt 0.
        add  wg_zglt036-vlr_moeda_grupo    to vl_pos_grp.
      endif.
      if wg_zglt036-vlr_moeda_doc gt 0.
        add  wg_zglt036-vlr_moeda_doc    to vl_pos_doc.
      endif.

      if wg_zglt035-st_ap_fiscal = 'X' and wg_zglt036-matnr_fi is initial.
        move:  c_ts_100-tab1 to tg_msg_ret-aba.
        concatenate text-e92 '' wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.

      if wg_zglt036-xclasse = 'X'
        and wg_zglt036-kostl is initial
        and wg_zglt036-prctr is initial
        and wg_zglt036-aufnr is initial
        and wg_zglt036-matnr is initial.
        move:  c_ts_100-tab1               to tg_msg_ret-aba.
        concatenate text-e02 text-e30 wl_linha into  tg_msg_ret-msg.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.

      if wg_zglt036-matnr is not initial.
        select single *
          from mara
          into wl_mara
        where matnr = wg_zglt036-matnr.

        if sy-subrc ne 0 .
          move:  c_ts_100-tab1  to tg_msg_ret-aba.
          concatenate text-e80 '' wl_linha into  tg_msg_ret-msg separated by space.
          append tg_msg_ret.  clear: tg_msg_ret.
        endif.
      endif.

      if wg_zglt036-matnr_fi is not initial.
        select single *
          from mara
          into wl_mara
        where matnr = wg_zglt036-matnr_fi.

        if sy-subrc ne 0 .
          move:  c_ts_100-tab1  to tg_msg_ret-aba.
          concatenate text-e80 '' wl_linha into  tg_msg_ret-msg separated by space.
          append tg_msg_ret.  clear: tg_msg_ret.
        endif.
      endif.

      if wg_zglt036-aufnr is not initial.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = wg_zglt036-aufnr
          importing
            output = wg_zglt036-aufnr.

        select single *
          from aufk
          into wl_aufk
          where bukrs  = wl_zglt034-bukrs
          and   aufnr  = wg_zglt036-aufnr.
        if sy-subrc ne 0.
          move:  c_ts_100-tab1               to tg_msg_ret-aba.
          concatenate text-e94  wl_linha into  tg_msg_ret-msg separated by space.
          append tg_msg_ret.  clear: tg_msg_ret.
        elseif wl_aufk-astnr ne 0.
          move:  c_ts_100-tab1               to tg_msg_ret-aba.
          concatenate text-e94  wl_linha into  tg_msg_ret-msg separated by space.
          append tg_msg_ret.  clear: tg_msg_ret.
        endif.
      endif.


      if wg_zglt036-kostl is not initial.
        select single *
          from tka02
          into wl_tka02
          where bukrs  = wl_zglt034-bukrs.

        select single *
            from csks
            into wl_csks
            where  kokrs  = wl_tka02-kokrs
            and    kostl  = wg_zglt036-kostl
            and    datab  le sy-datum
            and    datbi  ge sy-datum
            and    bukrs  eq  wl_zglt034-bukrs.

        if sy-subrc ne 0.
          move:  c_ts_100-tab1               to tg_msg_ret-aba.
          concatenate text-e78  wl_linha into  tg_msg_ret-msg separated by space.
          append tg_msg_ret.  clear: tg_msg_ret.
        elseif wl_csks-bkzkp = 'X'.
          move:  c_ts_100-tab1               to tg_msg_ret-aba.
          concatenate text-ee3  wl_linha into  tg_msg_ret-msg separated by space.
          append tg_msg_ret.  clear: tg_msg_ret.
        elseif wg_zglt036-divisao is not initial.
          select single *
            from tka02
            into wl_tka02
            where bukrs  = wl_zglt034-bukrs.

          select single *
              from csks
              into wl_csks
              where  kokrs  = wl_tka02-kokrs
              and    kostl  = wg_zglt036-kostl
              and    gsber  = wg_zglt036-divisao
              and    datab  le sy-datum
              and    datbi  ge sy-datum.

          if sy-subrc ne 0.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e31 text-e30'' wl_linha into  tg_msg_ret-msg separated by space.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.
        endif.
      endif.

      if wg_zglt036-tax_code is not initial.
        select single *
          from t007a
          into wl_t007a
          where kalsm = wl_t005-kalsm
          and   mwskz = wg_zglt036-tax_code.
        if sy-subrc ne 0.
          move:  c_ts_100-tab1 to tg_msg_ret-aba.
          concatenate wg_zglt036-tax_code text-e87 wl_t005-kalsm wl_linha into  tg_msg_ret-msg separated by space.
          append tg_msg_ret.
          clear: tg_msg_ret.
        else.
          select single *
            from t030k
              into  wl_t030k
            where ktopl = '0050'
            and   mwskz = wg_zglt036-tax_code.
          if sy-subrc ne 0.
            move:  c_ts_100-tab1 to tg_msg_ret-aba.
            concatenate wg_zglt036-tax_code text-e87 wl_t005-kalsm wl_linha into  tg_msg_ret-msg separated by space.
            append tg_msg_ret.
            clear: tg_msg_ret.
          endif.
        endif.
      endif.
                                                            " 75407 -
      "75407 -Empresas que permite lançamento sem divisão --LPORTELA

      select single low
         into @data(t_tvarvc)
         from tvarvc
        where name = 'Z_ZGL_SEM_DIVISAO'
        and low eq  @wg_zglt035-bukrs.

      if t_tvarvc is initial."<<fim " 75407 -

        if wg_zglt036-divisao is initial.
          move:  c_ts_100-tab1               to tg_msg_ret-aba.
          concatenate text-e03 text-e30 wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.  clear: tg_msg_ret.
        else.
          clear: w_div1, w_div2.
          loop at t_divisao.
            if t_divisao-from+0(4) = wg_zglt035-bukrs.
              w_div1 = 'X'.
              if t_divisao-from+5(4) = wg_zglt036-divisao.
                w_div2 = 'X'.
              endif.
            endif.
          endloop.
          if w_div1 = 'X' and w_div2 is initial.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e04 text-e30 wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          elseif w_div1 is initial.
            if wl_t001-land1 = 'BR' and wg_zglt035-bukrs ne '0004'.
              select single *
                from j_1bbranch
                into wl_j_1bbranch
                where bukrs  = wg_zglt035-bukrs
                and   branch = wg_zglt036-divisao.
            else.
              select single * from tgsb
                into  wl_tgsb
              where gsber eq wg_zglt036-divisao.
            endif.
            if sy-subrc ne 0.
              move:  c_ts_100-tab1               to tg_msg_ret-aba.
              concatenate text-e04 text-e30 wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.  clear: tg_msg_ret.
            endif.
          endif.
        endif.
      endif.
      if wg_zglt036-matnr is not initial.
        w_flagm = 'X'. " SHDB Material
      endif.
      if wg_zglt036-bschl = '70' or wg_zglt036-bschl = '75'.
        w_flagi = 'X'. " SHDB Imobilizado
      endif.
      if wg_zglt036-tax_code is not initial.
        w_flagt = 'X'. " SHDB IVA
        add 1 to w_civa.
      endif.
      "

      select single * from tbsl
        into wl_tbsl
      where bschl eq wg_zglt036-bschl.

      if wl_tbsl-koart eq 'K'.
        select  single *
          from zglt037
          into wl_zglt037
          where bukrs     le wg_zglt035-bukrs
          and   bukrs_ate ge wg_zglt035-bukrs
          and   pgt_forn  eq 'X'
          and   aprovador eq sy-uname
          and   dt_val_de le sy-datum                "modificação 10.10.2016
          and   dt_val_ate ge sy-datum.
        if sy-subrc = 0.
          move:  c_ts_100-tab1               to tg_msg_ret-aba.
          concatenate text-e88  wl_linha into  tg_msg_ret-msg separated by space.
          append tg_msg_ret.  clear: tg_msg_ret.
        endif.
      endif.

      if 'K_D' cs wl_tbsl-koart .
        if  wg_zglt036-zlsch   is initial.
*          MOVE:  C_TS_100-TAB1               TO TG_MSG_RET-ABA.
*          CONCATENATE 'Informe Forma de Pagamento'  ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        else.
          select single *
            from t042z
            into wl_t042z
            where zlsch = wg_zglt036-zlsch
            and   land1 = wl_t001-land1.
          if sy-subrc ne 0.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e11 text-e30 wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.

        endif.

        if  wg_zglt036-zlspr   is initial.
*          MOVE:  C_TS_100-TAB1               TO TG_MSG_RET-ABA.
*          CONCATENATE 'Informe Bloqueio de Pagamento'  ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        endif.

        if  wg_zglt036-dt_vct is initial.
*          MOVE:  C_TS_100-TAB1               TO TG_MSG_RET-ABA.
*          CONCATENATE 'Informe a data de Vencimento' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        endif.
        if wg_zglt036-hbkid is initial.
*          MOVE:  C_TS_100-TAB1               TO TG_MSG_RET-ABA.
*          CONCATENATE TEXT-E09 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        else.
          select single *
            from t012
            into wl_t012
            where bukrs = wg_zglt035-bukrs
            and   hbkid = wg_zglt036-hbkid.
          if sy-subrc ne 0.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e10 text-e30 wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.
        endif.
      else.
        if  wg_zglt036-zlsch   is not initial.
          move:  c_ts_100-tab1               to tg_msg_ret-aba.
          concatenate text-e82  wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.  clear: tg_msg_ret.
        endif.
        clear wl_ska1.
        select single *                "#EC CI_DB_OPERATION_OK[2389136]
          from ska1                    "#EC CI_DB_OPERATION_OK[2431747]
          into wl_ska1
          where ktopl = '0050'
          and   saknr = wg_zglt036-hkont+0(10).

        if  wl_ska1-xspeb = 'X'.
          move:  c_ts_100-tab1               to tg_msg_ret-aba.
          concatenate text-e32  wg_zglt036-hkont text-e38 wl_linha into  tg_msg_ret-msg.
          append tg_msg_ret.  clear: tg_msg_ret.
        endif.

        clear wl_ska1.
        if wl_tbsl-koart eq 'S'.
          select single *              "#EC CI_DB_OPERATION_OK[2431747]
              from skb1
              into wl_skb1
              where bukrs = wg_zglt035-bukrs
              and   saknr = wg_zglt036-hkont+0(10).
          if sy-subrc ne 0.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e85  wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          elseif  wl_skb1-xspeb = 'X'.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e32  wg_zglt036-hkont text-e33 text-e30 wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.
        endif.

      endif.

      case wl_tbsl-koart.
        when 'K'.
          select single *
            from lfb1
            into wl_lfb1
            where bukrs = wg_zglt035-bukrs
            and   lifnr = wg_zglt036-hkont+0(10).
          if sy-subrc ne 0.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e34 ''  wg_zglt036-hkont text-e35 wg_zglt035-bukrs text-e30 wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          elseif wl_lfb1-sperr = 'X'.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e36 ' '  wg_zglt036-hkont text-e37  wg_zglt035-bukrs text-e30'  ' wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.

          clear wl_lfa1.
          select single *
            from lfa1
            into wl_lfa1
            where lifnr = wg_zglt036-hkont+0(10).
          if wl_lfa1-sperr = 'X'.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e36  wg_zglt036-hkont text-e38  text-e30'' wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.

          if wg_zglt036-hbkid is initial and
             wg_zglt036-bvtyp is not initial.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e09  text-e30'' wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.
          if wg_zglt036-hbkid is not initial and
             wg_zglt036-bvtyp is initial.
            if wg_zglt036-zlsch ne 'E' and wg_zglt035-bukrs ne '0100'. " CS2016001230
              move:  c_ts_100-tab1               to tg_msg_ret-aba.
              concatenate text-e07 text-e30 ' ' wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.  clear: tg_msg_ret.
            endif.
          endif.

          if ( wg_zglt036-hbkid is not initial or
             wg_zglt036-bvtyp is not initial ) and
            wg_zglt036-zlsch   is initial.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e39  text-e30 wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.

          if ( wg_zglt036-hbkid is initial or
             wg_zglt036-bvtyp is initial ) and
            wg_zglt036-zlsch   is not initial.
            if wg_zglt036-zlsch ne 'E'.
              if wg_zglt035-bukrs ne '0100'. "CS2016001230
                move:  c_ts_100-tab1               to tg_msg_ret-aba.
                concatenate text-e40  text-e30 wl_linha into  tg_msg_ret-msg.
                append tg_msg_ret.  clear: tg_msg_ret.
              endif.
            endif.
          endif.

          if  wg_zglt036-hbkid is not initial.
            if  wg_zglt036-dt_vct is not initial.

              concatenate wg_zglt036-dt_vct(6) '01' into vg_first_day.
              call function 'BKK_GET_MONTH_LASTDAY'
                exporting
                  i_date = vg_first_day
                importing
                  e_date = vg_last_day.

*-US 155653-04-11-2024-#155653-RJF-Inicio
              data: lv_hol_cal type scal-hcalid,
                    lv_fac_cal type scal-fcalid.

              if sy-tcode is initial.
                data(lv_nomep)   = sy-cprog.
                data(lv_tp_proc) = 'J'.
              else.
                lv_nomep = sy-tcode.
                lv_tp_proc = 'T'.
              endif.

              zcl_calendario=>get_calendario( exporting i_bukrs            = wg_zglt035-bukrs
*                                                        i_tipo_processo = lv_tp_proc
*                                                        i_nome_processo = lv_nomep
                                              importing e_holiday_calendar = lv_hol_cal
                                                        e_factory_calendar = lv_fac_cal ).

              if lv_hol_cal is initial.
                lv_hol_cal = 'BR'.
              endif.
              if ( lv_fac_cal is initial ).
                lv_fac_cal = 'ZF'.
              endif.
*-US 155653-04-11-2024-#155653-RJF-Fim

              refresh wl_sab_dom_fer.
              call function 'HOLIDAY_GET'
                exporting
                  holiday_calendar           = lv_hol_cal "-US 155653-04-11-2024-#155653-RJF
                  factory_calendar           = lv_fac_cal "-US 155653-04-11-2024-#155653-RJF
*                 holiday_calendar           = 'MG'
*                 factory_calendar           = 'ZF'
                  date_from                  = sy-datum
                  date_to                    = wg_zglt036-dt_vct "VG_LAST_DAY
                tables
                  holidays                   = wl_sab_dom_fer
                exceptions
                  factory_calendar_not_found = 1
                  holiday_calendar_not_found = 2
                  date_has_invalid_format    = 3
                  date_inconsistency         = 4
                  others                     = 5.

              read table wl_sab_dom_fer into wa_datas with key date = wg_zglt036-dt_vct.
              if sy-subrc ne 0.
                "Feriados Extras
                select single *
                  from zmmt0117
                  into  @data(w_117)
                  where data = @wg_zglt036-dt_vct.
                if sy-subrc = 0.
                  wa_datas-date       = wg_zglt036-dt_vct.
                  wa_datas-freeday    = ' '.
                  wa_datas-holiday    = 'X'.
                  wa_datas-holiday_id = ' '.
                  wa_datas-txt_short  = w_117-descr_fer(10).
                  wa_datas-txt_long   = w_117-descr_fer(30).
                  append wa_datas to wl_sab_dom_fer.
                endif.
              endif.

              wa_data = sy-datum.
              add 3 to wa_data.
              describe table wl_sab_dom_fer lines wl_n_uteis.

              add wl_n_uteis to wa_data.

              read table wl_sab_dom_fer with key date = wg_zglt036-dt_vct.
              if sy-subrc = 0.
                move:  c_ts_100-tab1               to tg_msg_ret-aba,
                text-e95            to tg_msg_ret-msg.
                append tg_msg_ret.  clear: tg_msg_ret.
              elseif ( wa_data gt wg_zglt036-dt_vct ) and sy-langu = 'P'.
                move:  c_ts_100-tab1               to tg_msg_ret-aba,
                       text-eee            to tg_msg_ret-msg.
                append tg_msg_ret.  clear: tg_msg_ret.

              endif.
            else.
              move:  c_ts_100-tab1               to tg_msg_ret-aba,
                     text-e95            to tg_msg_ret-msg.
              append tg_msg_ret.  clear: tg_msg_ret.
            endif.
          endif.

          if wg_zglt036-bvtyp is initial.
*            MOVE:  C_TS_100-TAB1               TO TG_MSG_RET-ABA.
*            CONCATENATE TEXT-E07 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          else.
            select single * from lfbk
              into wl_lfbk
              where lifnr = wg_zglt036-hkont+0(10)
              and   bvtyp = wg_zglt036-bvtyp.
            if sy-subrc ne 0.
              move:  c_ts_100-tab1               to tg_msg_ret-aba.
              concatenate text-e08 text-e30 wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.  clear: tg_msg_ret.
            endif.
          endif.
        when 'D'.
          select single *
            from knb1
            into wl_knb1
            where bukrs = wg_zglt035-bukrs
            and   kunnr = wg_zglt036-hkont+0(10).
          if sy-subrc ne 0.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e41  wg_zglt036-hkont text-e42 wg_zglt035-bukrs text-e30 wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          elseif wl_knb1-sperr = 'X'.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e43  wg_zglt036-hkont text-e44'' wg_zglt035-bukrs text-e30 wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.

          clear wl_kna1.
          select single *
            from kna1
            into wl_kna1
            where kunnr = wg_zglt036-hkont+0(10).
          if wl_kna1-sperr = 'X'.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e37  wg_zglt036-hkont text-e38 text-e30 wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.
          if wg_zglt036-bvtyp is initial.
*            MOVE:  C_TS_100-TAB1               TO TG_MSG_RET-ABA.
*            CONCATENATE TEXT-E07 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          else.
            select single * from knbk
              into wl_knbk
                   where kunnr =  wg_zglt036-hkont+0(10)
                   and   bvtyp = wg_zglt036-bvtyp.
            if sy-subrc ne 0.
              move:  c_ts_100-tab1               to tg_msg_ret-aba.
              concatenate text-e08 text-e30 wl_linha into  tg_msg_ret-msg.
              append tg_msg_ret.  clear: tg_msg_ret.
            endif.
          endif.
        when 'A'.
          clear  wl_ska1.
          split wg_zglt036-hkont at '-' into v_anln1 v_anln2.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = v_anln1
            importing
              output = v_anln1.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = v_anln2
            importing
              output = v_anln2.
          select single * from anla into wl_anla  where bukrs eq wg_zglt035-bukrs
                                                 and   anln1 eq v_anln1
                                                 and   anln2 eq v_anln2.
          if sy-subrc ne 0.
            move:  c_ts_100-tab1 to tg_msg_ret-aba.
            concatenate text-e93 '' wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.
            clear: tg_msg_ret.
          endif.
          select single * from t095 into wa_t095
                        where ktopl = '0050'
                        and   ktogr = wl_anla-ktogr
                        and   afabe = 1.
          vakont = wa_t095-ktansw.
          select single *              "#EC CI_DB_OPERATION_OK[2431747]
            from ska1                  "#EC CI_DB_OPERATION_OK[2389136]
            into wl_ska1
            where ktopl = '0050'
            and   saknr = vakont.
          if  wl_ska1-xspeb = 'X'.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e63 ''  vakont text-e71 '' wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.

          select single *              "#EC CI_DB_OPERATION_OK[2431747]
           from skb1
           into wl_skb1
           where bukrs = wg_zglt035-bukrs
           and   saknr = vakont.
          if sy-subrc ne 0.
            move:  c_ts_100-tab1               to tg_msg_ret-aba.
            concatenate text-e85  wl_linha into  tg_msg_ret-msg.
            append tg_msg_ret.  clear: tg_msg_ret.
          endif.

      endcase.
    endif.

  endloop.

  if tg_zglt036[] is initial.
    move: c_ts_100-tab1                 to tg_msg_ret-aba,
          text-e46  to tg_msg_ret-msg.
    append tg_msg_ret.  clear: tg_msg_ret.
  endif.

  if wg_zglt035-st_lc_moeda = 'X'.
    var_qte = 0.
    if   vl_pos_int gt 0.
      add 1 to var_qte.
    endif.

    if   vl_pos_for gt 0.
      add 1 to var_qte.
    endif.

    if  vl_pos_grp gt 0 .
      add 1 to var_qte.
    endif.

    if  vl_pos_doc gt 0 .
      add 1 to var_qte.
    endif.

    if var_qte gt 2.
      concatenate text-e47'' text-e48''  into  tg_msg_ret-msg separated by space.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.
  else.
    if vl_pos_int = 0.
      concatenate text-e05 'Vlr. Moeda Int. ' into  tg_msg_ret-msg separated by space.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.

    if vl_pos_doc = 0.
      concatenate text-e05 'Vlr. Moeda Doc. ' into  tg_msg_ret-msg separated by space.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.

    if  vl_pos_for = 0.
      concatenate text-e05 'Vlr. Moeda Forte. ' into  tg_msg_ret-msg separated by space.
      append tg_msg_ret.
      clear: tg_msg_ret.
    endif.

    if wl_t005-waers ne 'BRL'.
      if  vl_pos_grp = 0.
        concatenate text-e05 'Vlr. Moeda Grupo ' into  tg_msg_ret-msg separated by space.
        append tg_msg_ret.
        clear: tg_msg_ret.
      endif.
    endif.
  endif.

  if vl_sum_int <> 0 or vl_sum_for <> 0 or vl_sum_grp <> 0 or vl_sum_doc <> 0.
    move: c_ts_100-tab1                                 to tg_msg_ret-aba,
          text-e49  to tg_msg_ret-msg.
    append tg_msg_ret.  clear: tg_msg_ret.
  endif.


  if  vl_pos_int eq 0 and  vl_pos_for eq 0 and vl_pos_doc eq 0 and vl_pos_grp eq 0.
    move: c_ts_100-tab1                 to tg_msg_ret-aba,
      text-e50 to tg_msg_ret-msg.
    append tg_msg_ret.  clear: tg_msg_ret.
  endif.
endform.                    " F_VERIFICA_ERROS

*&---------------------------------------------------------------------*
*&      Form  F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_trata_campos  using p_field p_group1 p_value p_invisible.
  tg_fields-campo     = p_field.
  tg_fields-group1    = p_group1.
  tg_fields-value     = p_value.
  tg_fields-invisible = p_invisible.
  append tg_fields.
endform.                    " F_TRATA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_montar_layout  using p_edit p_visao.
  data: p_edit2    type c,
        tabix      type sy-tabix,
        tl_zglt030 type table of zglt030      with header line,
        l_edit     type c,
        lv_zgl019  type c.

  l_edit = p_edit.
  p_edit2 = p_edit.
  if wg_zglt035-tp_lcto is not initial.
    clear: p_edit2.
  endif.

*  SELECT *
*    FROM zglt037
*    INTO @DATA(ls_user)
*    UP TO 1 ROWS
*    WHERE aprovador = @sy-uname.
*  ENDSELECT.
*  IF sy-subrc IS INITIAL.
  if sy-tcode = 'ZGL016A'.
    clear: p_edit2,
           l_edit.
  endif.

  refresh tg_fieldcatalog.
  perform f_montar_estrutura using:
        0  ' '           ' '               'TG_ZGLT036'  'CHECK'           text-a12          '6'   l_edit  ' ' ' ' ' ' ' ',
        1  ' '           ' '               'TG_ZGLT036'  'SEQITEM'         text-a13          '7'   ' '     ' ' ' ' ' ' ' ',
        2   'TBSLT'      'BSCHL'           'TG_ZGLT036'  'BSCHL'           text-a14          '7'  p_edit2  ' ' ' ' ' ' ' '.
  if x_visao = 'X'.
    perform f_montar_estrutura using:
          4   ' '          ' '               'TG_ZGLT036'  'AKONT'         text-a01          '11'  ' '     ' ' ' ' 'X' ' ',
          5   ' '          ' '               'TG_ZGLT036'  'DESCR_A'       text-a15          '30'  ' '     ' ' ' ' ' ' ' '.
  else.
    perform f_montar_estrutura using:
          4   ' '          ' '               'TG_ZGLT036'  'HKONT'         text-a01          '17'  l_edit     ' ' ' ' 'X' ' ',
          5   ' '          ' '               'TG_ZGLT036'  'DESCR'         text-a15          '30'  ' '         ' ' ' ' ' ' ' '.
  endif.
  perform f_montar_estrutura using:
        6   ' '          ' '               'TG_ZGLT036'  'UMSKZ'           text-a19           '6'  p_edit2    ' ' ' ' ' ' ' ',
        7   'TABWT'     'BWASL'            'TG_ZGLT036'  'ANBWA'           text-a38           '10' p_edit2    ' ' ' ' ' ' ' ',
        8   'T856T'     'TRTYP'            'TG_ZGLT036'  'BEWAR'           text-a37           '10' p_edit2    ' ' ' ' ' ' ' ',
        9   ' '          ' '               'TG_ZGLT036'  'D_C'             text-a20           '3'   ' '     ' ' ' ' ' ' ' ',
       10   ' '          ' '               'TG_ZGLT036'  'TAX_CODE'        text-a30           '05'  l_edit    ' ' ' ' ' ' ' ',
       11   ' '          ' '               'TG_ZGLT036'  'ICON'            text-a21           '7'  ' '     ' ' ' ' ' ' 'X',
       12   'ZGLT036'    'VLR_MOEDA_DOC'   'TG_ZGLT036'  'VLR_MOEDA_DOC'   text-a08           '15'  l_edit  'X' ' ' ' ' ' ',
       13   'ZGLT036'    'VLR_MOEDA_INT'   'TG_ZGLT036'  'VLR_MOEDA_INT'   text-a09           '15'  l_edit  'X' ' ' ' ' ' ',
       14   'ZGLT036'    'VLR_MOEDA_FORTE' 'TG_ZGLT036'  'VLR_MOEDA_FORTE' text-a10           '15'  l_edit  'X' ' ' ' ' ' ',
       15   'ZGLT036'    'VLR_MOEDA_GRUPO' 'TG_ZGLT036'  'VLR_MOEDA_GRUPO' text-a11           '15'  l_edit  'X' ' ' ' ' ' ',
       16   'ZGLT036'    'MATNR_FI'        'TG_ZGLT036'  'MATNR_FI'        text-a26           '18'  l_edit  ' ' ' ' ' ' ' ',
       17   'ZGLT036'    'QUANTITY'        'TG_ZGLT036'  'QUANTITY'        text-a61           '18'  l_edit  ' ' ' ' ' ' ' ',
       18   ' '          ' '               'TG_ZGLT036'  'ZUONR'           text-a27           '18'  l_edit  ' ' ' ' ' ' ' ',
       19   ' '          ' '               'TG_ZGLT036'  'VBELN'           text-a50           '10'  l_edit  ' ' ' ' ' ' ' ',
       20   ' '          ' '               'TG_ZGLT036'  'DIVISAO'         text-a28           '10'  l_edit  ' ' ' ' 'X' ' ',
       21   'EKPO'       'WERKS'           'TG_ZGLT036'  'WERKS'           text-a48           '10'  l_edit  ' ' ' ' 'X' ' ',
       22   'TSTGC'      'RCOMP'           'TG_ZGLT036'  'VBUND'           text-a29           '10'  p_edit2   ' ' ' ' ' ' ' ',
       23   'ZGLT036'    'SGTXT'           'TG_ZGLT036'  'SGTXT'           text-a31           '25'  l_edit  ' ' ' ' ' ' ' ',
       24   'ZGLT036'    'DT_VCT'          'TG_ZGLT036'  'DT_VCT'          text-a32           '12'  l_edit  ' ' ' ' ' ' ' ',
       25   ' '          ' '               'TG_ZGLT036'  'HBKID'           text-a33           '07'  l_edit  ' ' ' ' 'X' ' ',
       26   ' '          ' '               'TG_ZGLT036'  'BVTYP'           text-a34           '07'  l_edit  ' ' ' ' 'X' ' ',
       27   ' '          ' '               'TG_ZGLT036'  'ZLSCH'           text-a35           '07'  l_edit  ' ' ' ' 'X' ' ',
       28   'BSEG'       'ZLSPR'           'TG_ZGLT036'  'ZLSPR'           text-a36           '07'  l_edit  ' ' ' ' 'X' ' '.

  if wg_zglt035-bukrs is not initial.
    select *
      from zglt030
      into table tl_zglt030
      where bukrs = wg_zglt035-bukrs.

    sort tl_zglt030 by campo.
    loop at tg_fieldcatalog into wg_fieldcatalog.
      tabix = sy-tabix.
      read table tl_zglt030 with key campo = wg_fieldcatalog-fieldname binary search.
      if sy-subrc = 0.
        wg_fieldcatalog-col_opt = 'X'.
        modify tg_fieldcatalog from wg_fieldcatalog index tabix transporting col_opt.
      endif.
    endloop.
    delete tg_fieldcatalog where col_opt = 'X'.
  endif.

endform.                    " F_MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_montar_estrutura  using  p_col_pos     p_ref_tabname   p_ref_fieldname
                                p_tabname     p_field         p_scrtext_l
                                p_outputlen   p_edit          p_sum
                                p_emphasize   p_f4            p_ico.

  clear wg_fieldcatalog.
  wg_fieldcatalog-fieldname    = p_field.
  wg_fieldcatalog-tabname      = p_tabname.
  wg_fieldcatalog-ref_table    = p_ref_tabname.
  wg_fieldcatalog-ref_field    = p_ref_fieldname.
  wg_fieldcatalog-key          = ' '.
  case p_field.
    when 'CHECK' or 'SEQITEM' or 'BSCHL' or 'HKONT' or 'DESCR' or 'AKONT' or 'DESCR_A'.
      wg_fieldcatalog-key           = 'X'.
    when others.
  endcase.
  wg_fieldcatalog-edit         = p_edit.
  wg_fieldcatalog-do_sum       = p_sum.

  wg_fieldcatalog-col_pos      = p_col_pos.

  if p_outputlen is not initial.
    wg_fieldcatalog-outputlen  = p_outputlen.
  endif.

  wg_fieldcatalog-no_out       = ' '.
  wg_fieldcatalog-reptext      = p_scrtext_l.
  wg_fieldcatalog-scrtext_s    = p_scrtext_l.
  wg_fieldcatalog-scrtext_m    = p_scrtext_l.
  wg_fieldcatalog-scrtext_l    = p_scrtext_l.
  wg_fieldcatalog-emphasize    = p_emphasize.

  if p_f4 is not initial.
    wg_fieldcatalog-f4availabl = c_x.
  endif.

  if p_field = 'CHECK'.
    wg_fieldcatalog-checkbox = c_x.
  endif.

  case p_field.
    when 'HKONT' or 'UMSKZ' or 'TAX_CODE' .
      wg_fieldcatalog-f4availabl = c_x.
    when others.
  endcase.

  if p_ico is not initial.
    wg_fieldcatalog-hotspot    = c_x.
    wg_fieldcatalog-icon       = c_x.
  endif.

  append wg_fieldcatalog to tg_fieldcatalog.
endform.                    " F_MONTAR_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_busca_dados.
  data: wl_zglt034         type zglt034,
        wl_zglt033         type zglt033,
        wl_zglt031         type zglt031,
        tl_zglt032         type table of zglt032,
        wl_zglt032         type zglt032,
        wl_zglt035         type zglt035,
        wl_zglt036         type zglt036,
        wl_akont           type lfb1-akont,
        wl_hkont           type t074-hkont,
        wl_zglt036_aux     type zglt036,
        tl_zglt036         type table of zglt036,
        tl_zglt036_aux     type table of zglt036,
        wl_tbsl            type tbsl,
        wl_skb1            type skb1,
* ---> S4 Migration - 17/07/2023 - CA
*        wl_cskb            TYPE cskb,
* <--- S4 Migration - 17/07/2023 - CA
        wl_csks            type csks,
        wl_tka02           type tka02,
        wl_bkpf            type bkpf,
        wl_t001            type t001,
        vl_vlr_moeda_int   type zglt036-vlr_moeda_int,
        vl_vlr_moeda_forte type zglt036-vlr_moeda_forte,
        vl_vlr_moeda_doc   type zglt036-vlr_moeda_doc,
        vl_vlr_moeda_grupo type zglt036-vlr_moeda_grupo,
        wl_cont            type sy-tabix,
        wl_cont_aux        type sy-tabix,
        wl_cont_aux2       type sy-tabix,
        wl_item            type sy-tabix,
        tabix              type sy-tabix,
        xobj_key           type zib_contabil-obj_key,
        wl_anla            type anla,
        v_anln1            type anla-anln1,
        v_anln2            type anla-anln2,
        wa_t095            type t095.

* ---> S4 Migration - 17/07/2023 - CA
  data: lt_returns         type table of bapiret2,
        ls_coeldes         type bapi1030_ceoutputlist,
        lv_controllingarea type bapi1030_gen-co_area,
        lv_costelement     type bapi1030_gen-cost_elem,
        lv_keydate         type bapi1030_gen-some_date.
* <--- S4 Migration - 17/07/2023 - CA

  if wg_zglt034-lote is not initial.
    select single * from zglt034 into wl_zglt034 where lote eq wg_zglt034-lote.
    if wl_zglt034-status_lote = 'L'.
      message s836(sd) display like 'E' with text-e51 .
      exit.
    endif.
    wg_zglt034-descr_lote = wl_zglt034-descr_lote.

    if wl_zglt034-status_lote = 'A'.
      message s836(sd) display like 'E' with text-e52.
      exit.
    endif.

    if wl_zglt034-status_lote ne ' '.
      message s836(sd) display like 'E' with text-e53 .
      exit.
    endif.

    if wl_zglt034 is not initial.
      move: wl_zglt034-bukrs to wg_zglt035-bukrs.
    endif.

    select single * from zglt033 into wl_zglt033 where cod_depto eq wl_zglt034-dep_resp.

    select single *
      from t001
      into wl_t001
      where bukrs = wl_zglt034-bukrs.

    if sy-subrc = 0.
      move: wl_t001-butxt   to wg_zglt035-butxt.
    endif.
  endif.

  if wg_acao = c_add. "Novo Lançamento

    if wg_zglt035-tp_lcto is not initial.
      clear: wg_zglt036. refresh: tg_zglt036,tg_zglt036_ori, tg_obj.
      refresh tg_obj.
      select single * from zglt031 into wl_zglt031 where tp_lcto eq wg_zglt035-tp_lcto.
      if wl_zglt031-st_trans_banc = 'X' or
        wl_zglt031-st_conc_banc = 'X'.
        message s836(sd) display like 'E' with text-e91 .
        exit.
      endif.
      check sy-subrc = 0.
      move: wl_zglt031-descricao        to wg_zglt035-descricao,
            wl_zglt031-moeda_doc        to wg_zglt035-moeda_doc,
            wl_zglt031-st_lc_moeda      to wg_zglt035-st_lc_moeda,
            wl_zglt031-moeda_interna    to wg_zglt035-moeda_interna,
            wl_zglt031-moeda_int_hist   to wg_zglt035-moeda_int_hist,
            wl_zglt031-moeda_forte      to wg_zglt035-moeda_forte,
            wl_zglt031-moeda_ft_hist    to wg_zglt035-moeda_ft_hist,
            wl_zglt031-moeda_grupo      to wg_zglt035-moeda_grupo,
            wl_zglt031-moeda_gp_hist    to wg_zglt035-moeda_gp_hist,
            wl_zglt031-blart            to wg_zglt035-blart,
            wl_zglt031-xblnr            to wg_zglt035-xblnr,
            wl_zglt031-bktxt            to wg_zglt035-bktxt,
            wl_zglt031-dt_doc           to wg_zglt035-dt_doc,
            wl_zglt031-dt_doc_ult_mes   to wg_zglt035-dt_doc_ult_mes,
            wl_zglt031-dt_lcto          to wg_zglt035-dt_lcto,
            wl_zglt031-dt_lcto_ult_mes  to wg_zglt035-dt_lcto_ult_mes,
            wl_zglt031-prov_est         to wg_zglt035-prov_est,
            wl_zglt031-st_ap_fiscal     to wg_zglt035-st_ap_fiscal,
            wl_zglt031-st_agrupa        to wg_zglt035-st_agrupa.

      wg_zglt035-monat = sy-datum+4(2).
      wg_zglt035-gjahr = sy-datum+0(4).
      wg_zglt035-budat = sy-datum.
      "WG_ZGLT035-BLDAT = SY-DATUM.
      select * from zglt032 into table tl_zglt032 where tp_lcto eq wg_zglt035-tp_lcto.
      sort    tl_zglt032 by buzei.
      check sy-subrc = 0.
      clear wl_item.
      loop at tl_zglt032 into wl_zglt032.
        move-corresponding wl_zglt032 to wg_zglt036.
        if wg_zglt035-bukrs is not initial.

*          SELECT SINGLE GSBER
*          FROM ZIMP_CAD_DEPTO
*          INTO  WG_ZGLT036-DIVISAO
*          WHERE BUKRS EQ WG_ZGLT035-BUKRS
*          AND   GSBER NE ''.

          if wg_zglt035-bukrs = '0200'.
            select single * from zglt034
              into @data(wzglt034)
            where lote eq @wg_zglt034-lote.

            select single gsber  from zimp_cad_depto
              into wg_zglt036-divisao
            where dep_resp eq wzglt034-dep_resp.
          else.
            select single gsber
            from zimp_cad_depto
            into  wg_zglt036-divisao
            where bukrs eq wg_zglt035-bukrs
            and   gsber ne ''.

          endif.

        endif.

        move wl_zglt032-hkont to wg_zglt036-akont.
*        WG_ZGLT036-SEQITEM  = SY-TABIX.

        clear: wa_style.
        refresh: wg_zglt036-style, style.

        if wg_zglt036-bschl is not initial.
          select single * from tbsl into wl_tbsl where bschl eq wg_zglt036-bschl.
          if sy-subrc eq 0.
            case wl_tbsl-koart.
              when 'K'.
                select single name1 from lfa1 into wg_zglt036-descr where lifnr eq wg_zglt036-hkont+0(10).
                select single akont from lfb1 into wl_akont         where lifnr = wg_zglt036-hkont+0(10)  and bukrs = wg_zglt035-bukrs.
                wg_zglt036-akont = wl_akont.
                if wl_zglt032-umskz ne ''.
                  select single skont from t074 into wl_hkont
                    where ktopl = '0050'
                    and   koart = 'K'
                    and   umskz = wl_zglt032-umskz
                    and   hkont = wl_akont.
                  wg_zglt036-akont = wl_hkont.
                endif.
                select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                                     and spras eq sy-langu
                                                                     and ktopl eq '0050'.
              when 'D'.
                select single name1 from kna1 into wg_zglt036-descr where kunnr eq wg_zglt036-hkont+0(10).
                select single akont from knb1 into wl_akont         where kunnr = wg_zglt036-hkont+0(10)  and bukrs = wg_zglt035-bukrs.
                wg_zglt036-akont = wl_akont.
                if wl_zglt032-umskz ne ''.
                  select single skont from t074 into wl_hkont
                    where ktopl = '0050'
                    and   koart = 'D'
                    and   umskz = wl_zglt032-umskz
                    and   hkont = wl_akont.
                  wg_zglt036-akont = wl_hkont.
                endif.
                select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                                     and spras eq sy-langu
                                                                     and ktopl eq '0050'.

              when 'S'.
                select single txt50 from skat into wg_zglt036-descr where saknr eq wg_zglt036-hkont+0(10)
                                                                     and spras eq sy-langu
                                                                     and ktopl eq '0050'.
                wg_zglt036-descr_a = wg_zglt036-descr.
              when 'A'.
                split wg_zglt036-hkont  at '-' into v_anln1 v_anln2.
                if v_anln2 is initial.
                  v_anln2 = '0000'.
                endif.

                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = v_anln1
                  importing
                    output = v_anln1.

                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = v_anln2
                  importing
                    output = v_anln2.

                clear: wl_anla, wa_t095.
                select single * from anla into wl_anla where bukrs eq wg_zglt035-bukrs
                                              and   anln1 eq v_anln1
                                              and   anln2 eq v_anln2.

                concatenate wl_anla-txt50 wl_anla-txa50 into wg_zglt036-descr separated by space.

                select single * from t095 into wa_t095
                  where ktopl = '0050'
                  and   ktogr = wl_anla-ktogr
                  and   afabe = 1.

                wg_zglt036-akont = wa_t095-ktansw.
                select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                        and spras eq sy-langu
                                                        and ktopl eq '0050'.
            endcase.

            if wl_tbsl-shkzg eq 'S'.
              wg_zglt036-d_c = 'D'.
            else.
              wg_zglt036-d_c = 'C'.
            endif.

            select single *
              from tka02
              into wl_tka02
              where bukrs  = wl_zglt034-bukrs.

* ---> S4 Migration - 17/07/2023 - CA
*            CLEAR wl_cskb.
*            SELECT SINGLE *
*                FROM cskb
*                INTO wl_cskb
*                WHERE  kokrs  = wl_tka02-kokrs
*                AND    kstar  = wg_zglt036-hkont+0(10)
*                AND    datab  LE sy-datum
*                AND    datbi  GE sy-datum.
* <--- S4 Migration - 17/07/2023 - CA

            "ALRS 27.11.2015
            clear wl_skb1.
            if '0200_0201_0202' cs wg_zglt035-bukrs. "EUROPA
              select single *          "#EC CI_DB_OPERATION_OK[2431747]
                from skb1
                into wl_skb1
                where bukrs = wg_zglt035-bukrs
                and   saknr = wg_zglt036-hkont+0(10)
                and fstag   = 'YB09'.
            endif.

            if wg_zglt036-hkont is not initial.
              wa_style-fieldname = 'HKONT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              insert  wa_style into table style .
            endif.

* ---> S4 Migration - 17/07/2023 - CA
            lv_controllingarea  = wl_tka02-kokrs.
            lv_costelement      = wg_zglt036-hkont+0(10).
            lv_keydate          = sy-datum.

            clear: lt_returns[], ls_coeldes.

            call function 'K_COSTELEM_BAPI_GETDETAIL'
              exporting
                controllingarea   = lv_controllingarea
                costelement       = lv_costelement
                keydate           = lv_keydate
              importing
                costelementdetail = ls_coeldes
              tables
                return            = lt_returns.

            if ( ls_coeldes-cost_elem is not initial and ls_coeldes-celem_category is not initial or wl_skb1-fstag   = 'YB09' ) and wl_tbsl-koart  = 'S'.
*            IF ( wl_cskb-kstar IS NOT INITIAL OR wl_skb1-fstag   = 'YB09' ) AND wl_tbsl-koart  = 'S'.
* <--- S4 Migration - 17/07/2023 - CA
              move 'X' to wg_zglt036-xclasse.
              wg_zglt036-icon     = '@1F@'.

              wa_style-fieldname = 'VLR_MOEDA_INT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              insert  wa_style into table style .

              wa_style-fieldname = 'VLR_MOEDA_FORTE'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              insert  wa_style into table style .

              wa_style-fieldname = 'VLR_MOEDA_GRUPO'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              insert  wa_style into table style .

              wa_style-fieldname = 'VLR_MOEDA_DOC'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              insert  wa_style into table style .

            else.
              clear:  wg_zglt036-xclasse, wg_zglt036-icon.
            endif.
          endif.
        endif.

        if '40_50' cs wg_zglt036-bschl.
          wa_style-fieldname = 'DT_VCT'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .

          wa_style-fieldname = 'HBKID'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .

          wa_style-fieldname = 'BVTYP'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .

          wa_style-fieldname = 'ZLSCH'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .

          wa_style-fieldname = 'ZLSPR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .

        endif.
        insert lines of style into table  wg_zglt036-style.

        "Só uma linha
        if wg_zglt035-st_agrupa = 'X'.
          read table tg_zglt036 into wg_zglt036_tot with key bschl    = wg_zglt036-bschl
                                                             hkont    = wg_zglt036-hkont+0(10)
                                                             matnr_fi = wg_zglt036-matnr_fi
                                                             tax_code = wg_zglt036-tax_code.
          if sy-subrc ne 0.
            add 1 to wl_item.
            wg_zglt036-seqitem  = wl_item.
            append wg_zglt036 to tg_zglt036.
          else.
            wg_zglt036-seqitem  = wg_zglt036_tot-seqitem.
          endif.
        else.
          add 1 to wl_item.
          wg_zglt036-seqitem  = wl_item.
          append wg_zglt036 to tg_zglt036.
        endif.

        move-corresponding wg_zglt036 to wg_obj.
        append wg_obj to tg_obj.
        clear wg_obj.

        clear: wg_zglt036.
      endloop.
      "
      tg_zglt036_ori[] = tg_zglt036[].
      "
      wg_acao = c_modif.
      perform f_trata_campos using  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
      perform f_trata_campos using  space
                                    'GR2'
                                    c_1       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
    else.
      wg_acao = c_modif.
      perform f_trata_campos using  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
      perform f_trata_campos using  space
                                    'GR2'
                                    c_1       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0


    endif.
  elseif wg_acao = c_displa.

    if wg_zglt035-doc_lcto is not initial.
      clear: wg_zglt036. refresh: tg_zglt036, tg_zglt036_ori, tg_obj.
      select single * from zglt035 into wl_zglt035 where doc_lcto eq wg_zglt035-doc_lcto and loekz = ''.
      check sy-subrc eq 0.
      select single * from zglt031 into wl_zglt031 where tp_lcto eq wl_zglt035-tp_lcto.
      move: wl_zglt031-descricao        to wg_zglt035-descricao,
            wl_zglt035-lote             to wg_zglt034-lote,
            wl_zglt035-bukrs            to wg_zglt035-bukrs,
            wl_zglt035-tp_lcto          to wg_zglt035-tp_lcto,
            wl_zglt035-dpto_resp        to wg_zglt035-dpto_resp,
            wl_zglt035-taxa             to wg_zglt035-taxa,
            wl_zglt035-moeda_doc        to wg_zglt035-moeda_doc,
            wl_zglt035-st_lc_moeda      to wg_zglt035-st_lc_moeda,
            wl_zglt035-moeda_interna    to wg_zglt035-moeda_interna,
            wl_zglt035-moeda_int_hist   to wg_zglt035-moeda_int_hist,
            wl_zglt035-moeda_forte      to wg_zglt035-moeda_forte,
            wl_zglt035-moeda_ft_hist    to wg_zglt035-moeda_ft_hist,
            wl_zglt035-moeda_grupo      to wg_zglt035-moeda_grupo,
            wl_zglt035-moeda_gp_hist    to wg_zglt035-moeda_gp_hist,
            wl_zglt035-blart            to wg_zglt035-blart,
            wl_zglt035-xblnr            to wg_zglt035-xblnr,
            wl_zglt035-bktxt            to wg_zglt035-bktxt,
            wl_zglt035-bldat            to wg_zglt035-bldat,
            wl_zglt035-budat            to wg_zglt035-budat,
            wl_zglt035-prov_est         to wg_zglt035-prov_est,
            wl_zglt035-usnam            to wg_zglt035-usnam,
            wl_zglt035-budat            to wg_zglt035-budat,
            wl_zglt035-bldat            to wg_zglt035-bldat,
            wl_zglt035-dt_entrada       to wg_zglt035-dt_entrada,
            wl_zglt035-hr_entrada       to wg_zglt035-hr_entrada,
            wl_zglt035-monat            to wg_zglt035-monat,
            wl_zglt035-gjahr            to wg_zglt035-gjahr,
            wl_zglt035-belnr            to wg_zglt035-belnr,
            wl_zglt035-st_ap_fiscal     to wg_zglt035-st_ap_fiscal,
            wl_zglt035-st_fecha         to wg_zglt035-st_fecha,
            wl_zglt035-st_agrupa        to wg_zglt035-st_agrupa.

      if wg_zglt035-belnr is initial.
        concatenate 'ZGL17' wg_zglt035-doc_lcto wg_zglt035-budat+0(4) into wa_zib_contabil_chv-obj_key.
        select single obj_key belnr bukrs gjahr
        from zib_contabil_chv
        into wa_zib_contabil_chv
        where obj_key eq wa_zib_contabil_chv-obj_key.

        if sy-subrc = 0.
          wg_zglt035-belnr = wa_zib_contabil_chv-belnr.
        endif.

        if wg_zglt035-prov_est = 'X' and wg_zglt035-belnr is not initial. "130130 - CS2023000969 Gisele Follmann PSA

          data: reversao_obj_key     type zib_contabil_chv-obj_key,
                aux_zib_contabil_chv type zib_contabil_chv,
                aux_zib_contabil_err type zib_contabil_err.
          clear: reversao_obj_key.
          reversao_obj_key = |{ wa_zib_contabil_chv-obj_key }R|.

          select single * from zib_contabil_chv
          where obj_key = @reversao_obj_key
          into @aux_zib_contabil_chv.

          if sy-subrc = 0.
            wg_zglt035-reversao_doc = aux_zib_contabil_chv-belnr.
            wg_zglt035-reversao_status = icon_led_green.

          else.

            select single * from zib_contabil_err
          where obj_key = @reversao_obj_key
          into @aux_zib_contabil_err.

            if sy-subrc = 0.
              clear: wg_zglt035-reversao_doc.
              wg_zglt035-reversao_status = icon_message_error.

            else.

              clear: wg_zglt035-reversao_doc.
              wg_zglt035-reversao_status = icon_led_yellow.

            endif.

          endif.

        endif.

      endif.
      if wg_zglt035-belnr is not initial.
        select single *
         from bkpf
         into wl_bkpf
         where bukrs = wg_zglt035-bukrs
         and   belnr = wg_zglt035-belnr
         and   gjahr = wg_zglt035-gjahr.
        if sy-subrc = 0.
          wg_zglt035-belnr_e = wl_bkpf-stblg.
          wg_zglt035-gjahr_e = wl_bkpf-stjah.
        endif.
      endif.
      select single * from zglt033 into wl_zglt033 where cod_depto eq wg_zglt035-dpto_resp.
      select single *
        from t001
        into wl_t001
        where bukrs = wl_zglt035-bukrs.

      if sy-subrc = 0.
        move: wl_t001-butxt   to wg_zglt035-butxt.
      endif.

      "Erros contabil
      refresh it_zib_contabil_err.
      concatenate 'ZGL17' wl_zglt035-doc_lcto wl_zglt035-budat+0(4) into xobj_key.
      select  obj_key nr_item interface dt_atualizacao hr_atualizacao type id num message message_v1 message_v2 message_v3  message_v4
       from zib_contabil_err
       into table it_zib_contabil_err
       where obj_key like xobj_key.

      refresh: tg_editor.
      clear: wl_cont_aux2, wl_cont_aux, wl_cont.
      wl_cont = strlen( wl_zglt035-ref_lcto ).
      wl_cont_aux = wl_cont / 72.

      do.
        move: wl_zglt035-ref_lcto+wl_cont_aux2 to wg_editor-line.
        add 72 to wl_cont_aux2.
        append wg_editor to tg_editor.

        if wl_cont_aux2 gt wl_cont.
          exit.

        endif.
      enddo.

      call method obg_descbox->set_text_as_r3table
        exporting
          table = tg_editor.
      call method obg_descbox->set_readonly_mode
        exporting
          readonly_mode = 1.

      select single * from zglt031 into wl_zglt031 where tp_lcto eq wg_zglt035-tp_lcto.
      if sy-subrc = 0.
        move: wl_zglt031-dt_doc           to wg_zglt035-dt_doc,
              wl_zglt031-dt_doc_ult_mes   to wg_zglt035-dt_doc_ult_mes,
              wl_zglt031-dt_lcto          to wg_zglt035-dt_lcto,
              wl_zglt031-dt_lcto_ult_mes  to wg_zglt035-dt_lcto_ult_mes.
      endif.
      select *
        from zglt036 into table tl_zglt036 where doc_lcto eq wg_zglt035-doc_lcto.

      check sy-subrc eq 0.

      refresh tg_obj.

      tl_zglt036_aux[] = tl_zglt036[].
      sort: tl_zglt036     by seqitem seqsub,
            tl_zglt036_aux by seqitem seqsub.

      delete adjacent duplicates from tl_zglt036 comparing seqitem.
      sort    tl_zglt036 by seqitem.
      loop at tl_zglt036 into wl_zglt036.
        move-corresponding wl_zglt036 to wg_zglt036.
        move wl_zglt036-hkont to wg_zglt036-akont.
        move wl_zglt036-gsber  to wg_zglt036-divisao.
        wg_zglt036-seqitem  = wg_zglt036-seqitem.
        wg_zglt036-icon     = '@1F@'.

        if wl_zglt036-bschl is not initial.
          select single * from tbsl into wl_tbsl where bschl eq wl_zglt036-bschl.
          if sy-subrc eq 0.
            case wl_tbsl-koart.
              when 'K'.
                select single name1 from lfa1 into wg_zglt036-descr where lifnr eq wl_zglt036-hkont+0(10).
                select single akont from lfb1 into wl_akont         where lifnr = wg_zglt036-hkont+0(10) and bukrs = wg_zglt035-bukrs.
                wg_zglt036-akont = wl_akont.
                if wl_zglt036-umskz ne ''.
                  select single skont from t074 into wl_hkont
                    where ktopl = '0050'
                    and   koart = 'K'
                    and   umskz = wl_zglt036-umskz
                    and   hkont = wl_akont.
                  wg_zglt036-akont = wl_hkont.
                endif.
                select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                                     and spras eq sy-langu
                                                                     and ktopl eq '0050'.
              when 'D'.
                select single name1 from kna1 into wg_zglt036-descr where kunnr eq wl_zglt036-hkont+0(10).
                select single akont from knb1 into wl_akont         where kunnr = wg_zglt036-hkont+0(10)  and bukrs = wg_zglt035-bukrs.
                wg_zglt036-akont = wl_akont.
                if wl_zglt036-umskz ne ''.
                  select single skont from t074 into wl_hkont
                    where ktopl = '0050'
                    and   koart = 'D'
                    and   umskz = wl_zglt036-umskz
                    and   hkont = wl_akont.
                  wg_zglt036-akont = wl_hkont.
                endif.
                select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                                     and spras eq sy-langu
                                                                     and ktopl eq '0050'.
              when 'S'.
                select single txt50 from skat into wg_zglt036-descr where saknr eq wl_zglt036-hkont+0(10)
                                                                     and spras eq sy-langu
                                                                     and ktopl eq '0050'.
                wg_zglt036-descr_a = wg_zglt036-descr.
              when 'I'.
                select single mcoa1 from anla into wg_zglt036-descr where anln1 eq wg_zglt036-hkont.
                wg_zglt036-descr_a = wg_zglt036-descr.
              when 'A'.
                split wg_zglt036-hkont  at '-' into v_anln1 v_anln2.
                if v_anln2 is initial.
                  v_anln2 = '0'.
                endif.

                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = v_anln1
                  importing
                    output = v_anln1.


                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = v_anln2
                  importing
                    output = v_anln2.
                clear wl_anla.
                select single * from anla into wl_anla where bukrs eq wg_zglt035-bukrs
                                              and   anln1 eq v_anln1
                                              and   anln2 eq v_anln2.

                concatenate wl_anla-txt50 wl_anla-txa50 into wg_zglt036-descr separated by space.

                clear wa_t095.
                select single * from t095 into wa_t095
                  where ktopl = '0050'
                  and   ktogr = wl_anla-ktogr
                  and   afabe = 1.

                wg_zglt036-akont = wa_t095-ktansw.
                select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                        and spras eq sy-langu
                                                        and ktopl eq '0050'.
            endcase.

            if wl_tbsl-shkzg eq 'S'.
              wg_zglt036-d_c = 'D'.
            else.
              wg_zglt036-d_c = 'C'.
            endif.

            select single *
              from tka02
              into wl_tka02
              where bukrs  = wl_zglt035-bukrs.

* ---> S4 Migration - 17/07/2023 - CA
*            CLEAR wl_cskb.
*            SELECT SINGLE *
*                FROM cskb
*                INTO wl_cskb
*                WHERE  kokrs  = wl_tka02-kokrs
*                AND    kstar  = wg_zglt036-hkont+0(10)
*                AND    datab  LE sy-datum
*                AND    datbi  GE sy-datum.
* <--- S4 Migration - 17/07/2023 - CA

            "ALRS 27.11.2015
            clear wl_skb1.
            if '0200_0201_0202' cs wg_zglt035-bukrs. "EUROPA
              select single *          "#EC CI_DB_OPERATION_OK[2431747]
                from skb1
                into wl_skb1
                where bukrs = wg_zglt035-bukrs
                and   saknr = wg_zglt036-hkont+0(10)
                and fstag   = 'YB09'.
            endif.

* ---> S4 Migration - 17/07/2023 - CA
            lv_controllingarea  = wl_tka02-kokrs.
            lv_costelement      = wg_zglt036-hkont+0(10).
            lv_keydate          = sy-datum.

            clear: lt_returns[], ls_coeldes.

            call function 'K_COSTELEM_BAPI_GETDETAIL'
              exporting
                controllingarea   = lv_controllingarea
                costelement       = lv_costelement
                keydate           = lv_keydate
              importing
                costelementdetail = ls_coeldes
              tables
                return            = lt_returns.

            if ( ls_coeldes-cost_elem is not initial and ls_coeldes-celem_category is not initial or wl_skb1-fstag   = 'YB09' ) and wl_tbsl-koart  = 'S'.
*            IF ( wl_cskb-kstar IS NOT INITIAL OR wl_skb1-fstag   = 'YB09' ) AND wl_tbsl-koart  = 'S'.
* <--- S4 Migration - 17/07/2023 - CA
              move 'X' to wg_zglt036-xclasse.
              wg_zglt036-icon     = '@1F@'.
            else.
              clear:  wg_zglt036-xclasse, wg_zglt036-icon.
            endif.

          endif.
        endif.

        vl_vlr_moeda_int    = 0.
        vl_vlr_moeda_forte  = 0.
        vl_vlr_moeda_doc    = 0.
        vl_vlr_moeda_grupo  = 0.
        loop at tl_zglt036_aux into wl_zglt036_aux where seqitem = wl_zglt036-seqitem.
          if wl_zglt036_aux-seqsub gt 0.
            move-corresponding wl_zglt036_aux to wg_obj.
            append wg_obj to tg_obj.
            add wl_zglt036_aux-vlr_moeda_int   to vl_vlr_moeda_int.
            add wl_zglt036_aux-vlr_moeda_forte to vl_vlr_moeda_forte.
            add wl_zglt036_aux-vlr_moeda_doc   to vl_vlr_moeda_doc.
            add wl_zglt036_aux-vlr_moeda_grupo to vl_vlr_moeda_grupo.
          endif.
        endloop.
        if wl_zglt036-seqsub gt 0.
          move :  vl_vlr_moeda_int    to wg_zglt036-vlr_moeda_int,
                  vl_vlr_moeda_forte  to wg_zglt036-vlr_moeda_forte,
                  vl_vlr_moeda_doc    to wg_zglt036-vlr_moeda_doc ,
                  vl_vlr_moeda_grupo  to wg_zglt036-vlr_moeda_grupo .
        endif.

        if wg_zglt036-d_c = 'C'.
          multiply  wg_zglt036-vlr_moeda_int   by -1.
          multiply  wg_zglt036-vlr_moeda_forte by -1.
          multiply  wg_zglt036-vlr_moeda_doc   by -1.
          multiply  wg_zglt036-vlr_moeda_grupo by -1.
        endif.
        if wg_zglt036-vlr_moeda_int   ne 0 or
           wg_zglt036-vlr_moeda_forte ne 0 or
           wg_zglt036-vlr_moeda_grupo ne 0 or
           wg_zglt036-vlr_moeda_doc   ne 0.
          wg_zglt036-check = 'X'.
        endif.

        "Só uma linha
        if wg_zglt035-tp_lcto is not initial and wg_zglt036-xclasse = 'X'.
          read table tg_zglt036 into wg_zglt036_tot with key bschl = wg_zglt035-bukrs
                                                             hkont = wg_zglt036-hkont+0(10).
          if sy-subrc eq 0.
            tabix = sy-tabix.
            add wg_zglt036_tot-vlr_moeda_doc   to wg_zglt036-vlr_moeda_doc.
            add wg_zglt036_tot-vlr_moeda_int   to wg_zglt036-vlr_moeda_int.
            add wg_zglt036_tot-vlr_moeda_forte to wg_zglt036-vlr_moeda_forte.
            add wg_zglt036_tot-vlr_moeda_grupo to wg_zglt036-vlr_moeda_grupo.
            modify tg_zglt036 from wg_zglt036 index tabix transporting vlr_moeda_doc vlr_moeda_int vlr_moeda_forte vlr_moeda_grupo.
          else.
            append wg_zglt036 to tg_zglt036.
          endif.
        else.
          append wg_zglt036 to tg_zglt036.
        endif.

        clear: wg_zglt036.
      endloop.
      if wg_zglt035-belnr is initial.
        wg_acao = c_modif.
      endif.

    endif.
  endif.

endform.                    " F_BUSCA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_obtem_proximo.
  data: vl_number type i.

  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = '01'
      object                  = 'ZDOC_LCTO'
    importing
      number                  = vl_number
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
       with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    wg_zglt035-doc_lcto = vl_number.
  endif.
endform.                    " F_OBTEM_PROXIMO

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_grava_dados.
  check wg_zglt035-doc_lcto is not initial.

  data: wl_input_glt035 type zglt035,
        tl_input_glt036 type table of zglt036 with header line,
        vseqsub         type zglt036-seqsub,
        vflag_cus(1).

  move: sy-mandt                    to wl_input_glt035-mandt,
        wg_zglt034-lote             to wl_input_glt035-lote,
        wg_zglt035-doc_lcto         to wl_input_glt035-doc_lcto,
        wg_zglt035-bukrs            to wl_input_glt035-bukrs,
        wg_zglt035-tp_lcto          to wl_input_glt035-tp_lcto,
        wg_zglt035-taxa             to wl_input_glt035-taxa,
        wg_zglt035-dpto_resp        to wl_input_glt035-dpto_resp,
        wg_zglt035-moeda_doc        to wl_input_glt035-moeda_doc,
        wg_zglt035-st_lc_moeda      to wl_input_glt035-st_lc_moeda,
        wg_zglt035-moeda_interna    to wl_input_glt035-moeda_interna,
        wg_zglt035-moeda_int_hist   to wl_input_glt035-moeda_int_hist,
        wg_zglt035-moeda_forte      to wl_input_glt035-moeda_forte,
        wg_zglt035-moeda_ft_hist    to wl_input_glt035-moeda_ft_hist,
        wg_zglt035-moeda_grupo      to wl_input_glt035-moeda_grupo,
        wg_zglt035-moeda_gp_hist    to wl_input_glt035-moeda_gp_hist,
        wg_zglt035-blart            to wl_input_glt035-blart,
        wg_zglt035-xblnr            to wl_input_glt035-xblnr,
        wg_zglt035-bktxt            to wl_input_glt035-bktxt,
        wg_zglt035-budat            to wl_input_glt035-budat,
        wg_zglt035-bldat            to wl_input_glt035-bldat,
        wg_zglt035-prov_est         to wl_input_glt035-prov_est,
        wg_zglt035-st_ap_fiscal     to wl_input_glt035-st_ap_fiscal,
        wg_zglt035-st_fecha         to wl_input_glt035-st_fecha,
        wg_zglt035-st_agrupa        to wl_input_glt035-st_agrupa,
        wg_zglt035-monat            to wl_input_glt035-monat,
        wg_zglt035-gjahr            to wl_input_glt035-gjahr,
        sy-uname                    to wl_input_glt035-usnam,
        sy-datum                    to wl_input_glt035-dt_entrada,
        sy-uzeit                    to wl_input_glt035-hr_entrada.

  refresh: tg_editor.
  if obg_descbox is not initial.
    call method obg_descbox->get_text_as_r3table
      importing
        table = tg_editor.

    loop at tg_editor into wg_editor.
      if sy-tabix eq 1.
        wl_input_glt035-ref_lcto = wg_editor-line.
      elseif sy-tabix ge 2.
        concatenate wl_input_glt035-ref_lcto wg_editor-line into wl_input_glt035-ref_lcto. " SEPARATED BY space.
      endif.
    endloop.
  endif.

  sort tg_obj by seqitem.
  delete tg_zglt036 where check ne 'X'.
  delete tg_zglt036 where vlr_moeda_int eq 0 and vlr_moeda_forte eq 0 and vlr_moeda_doc eq 0 and vlr_moeda_grupo eq 0.

  loop at tg_zglt036 into wg_zglt036.
    move: sy-mandt                    to tl_input_glt036-mandt,
          wl_input_glt035-doc_lcto    to tl_input_glt036-doc_lcto,
          wg_zglt036-seqitem          to tl_input_glt036-seqitem,
          0                           to tl_input_glt036-seqsub,
          wg_zglt036-tp_lcto          to tl_input_glt036-tp_lcto,
          wg_zglt036-bschl            to tl_input_glt036-bschl,
          wg_zglt036-hkont            to tl_input_glt036-hkont,
          wg_zglt036-umskz            to tl_input_glt036-umskz,
          wg_zglt036-anbwa            to tl_input_glt036-anbwa,
          wg_zglt036-bewar            to tl_input_glt036-bewar,
          wg_zglt036-vbund            to tl_input_glt036-vbund,
          wg_zglt036-kostl            to tl_input_glt036-kostl,
          wg_zglt036-prctr            to tl_input_glt036-prctr,
          wg_zglt036-aufnr            to tl_input_glt036-aufnr,
          wg_zglt036-vornr            to tl_input_glt036-vornr,
          wg_zglt036-matnr            to tl_input_glt036-matnr,
          wg_zglt036-matnr_fi         to tl_input_glt036-matnr_fi,
*          wg_zglt036-menge            to tl_input_glt036-menge,
          wg_zglt036-zuonr            to tl_input_glt036-zuonr,
          wg_zglt036-vbeln            to tl_input_glt036-vbeln,
          wg_zglt036-tax_code         to tl_input_glt036-tax_code,
          wg_zglt036-divisao          to tl_input_glt036-gsber,
          wg_zglt036-werks            to tl_input_glt036-werks,
          wg_zglt036-dt_vct           to tl_input_glt036-dt_vct,
          wg_zglt036-hbkid            to tl_input_glt036-hbkid,
          wg_zglt036-bvtyp            to tl_input_glt036-bvtyp,
          wg_zglt036-zlsch            to tl_input_glt036-zlsch,
          wg_zglt036-zlspr            to tl_input_glt036-zlspr,
          wg_zglt036-dt_vct           to tl_input_glt036-dt_vct,
          wg_zglt036-sgtxt            to tl_input_glt036-sgtxt,
          wg_zglt036-quantity         to tl_input_glt036-quantity,
          wg_zglt036-base_uom         to tl_input_glt036-base_uom,
          wg_zglt036-vlr_moeda_int    to tl_input_glt036-vlr_moeda_int,
          wg_zglt036-vlr_moeda_forte  to tl_input_glt036-vlr_moeda_forte,
          wg_zglt036-vlr_moeda_grupo  to tl_input_glt036-vlr_moeda_grupo,
          wg_zglt036-vlr_moeda_doc    to tl_input_glt036-vlr_moeda_doc.

    if  wg_zglt036-vlr_moeda_int lt 0.
      multiply tl_input_glt036-vlr_moeda_int by -1.
    endif.

    if  wg_zglt036-vlr_moeda_forte lt 0.
      multiply tl_input_glt036-vlr_moeda_forte by -1.
    endif.

    if  wg_zglt036-vlr_moeda_doc lt 0.
      multiply tl_input_glt036-vlr_moeda_doc by -1.
    endif.

    if  wg_zglt036-vlr_moeda_grupo lt 0.
      multiply tl_input_glt036-vlr_moeda_grupo by -1.
    endif.

    vflag_cus = 'N'.
    vseqsub = 0.
    loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
      if wg_obj-vlr_moeda_int eq 0 and
         wg_obj-vlr_moeda_forte eq 0 and
         wg_obj-vlr_moeda_grupo eq 0 and
         wg_obj-vlr_moeda_doc eq 0.
        continue.
      endif.
      vflag_cus = 'S'.
      add 1 to vseqsub.
      move :   vseqsub                 to tl_input_glt036-seqsub,
*               WG_OBJ-VBUND            TO TL_INPUT_GLT036-VBUND,
               wg_zglt036-vbund        to tl_input_glt036-vbund,
               wg_obj-kostl            to tl_input_glt036-kostl,
               wg_obj-prctr            to tl_input_glt036-prctr,
               wg_obj-aufnr            to tl_input_glt036-aufnr,
               wg_obj-matnr            to tl_input_glt036-matnr,
               wg_obj-quantity         to tl_input_glt036-quantity,
               wg_obj-base_uom         to tl_input_glt036-base_uom,
               wg_obj-vlr_moeda_int    to tl_input_glt036-vlr_moeda_int,
               wg_obj-vlr_moeda_forte  to tl_input_glt036-vlr_moeda_forte,
               wg_obj-vlr_moeda_grupo  to tl_input_glt036-vlr_moeda_grupo,
               wg_obj-vlr_moeda_doc    to tl_input_glt036-vlr_moeda_doc.

      if  tl_input_glt036-vlr_moeda_int lt 0.
        multiply tl_input_glt036-vlr_moeda_int by -1.
      endif.

      if  tl_input_glt036-vlr_moeda_forte lt 0.
        multiply tl_input_glt036-vlr_moeda_forte by -1.
      endif.

      if  tl_input_glt036-vlr_moeda_doc lt 0.
        multiply tl_input_glt036-vlr_moeda_doc by -1.
      endif.

      if  tl_input_glt036-vlr_moeda_grupo lt 0.
        multiply tl_input_glt036-vlr_moeda_grupo by -1.
      endif.

      append tl_input_glt036.
    endloop.
    if vflag_cus = 'N'.
      append tl_input_glt036.
      clear: tl_input_glt036, wg_zglt036.
    endif.
  endloop.

*  CLEAR: tl_input_glt036, wg_zglt035, wg_zglt036.

  delete from zglt036 where doc_lcto = wg_zglt035-doc_lcto.
  modify zglt035 from       wl_input_glt035.
  modify zglt036 from table tl_input_glt036.

  message s836(sd) with text-e54 wg_zglt035-doc_lcto text-e55.
endform.                    " F_GRAVA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA_OBJ
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_montar_estrutura_obj  using  p_col_pos     p_ref_tabname   p_ref_fieldname
                                    p_tabname     p_field         p_scrtext_l
                                    p_outputlen   p_edit          p_sum
                                    p_emphasize   p_f4            p_ico.

  clear wl_fieldcat.

  wl_fieldcat-fieldname     = p_field.
  wl_fieldcat-tabname       = p_tabname.
  wl_fieldcat-ref_tabname   = p_ref_tabname.
  wl_fieldcat-ref_fieldname = p_ref_fieldname.
  wl_fieldcat-key           = ' '.
  wl_fieldcat-edit          = p_edit.
  wl_fieldcat-do_sum        = p_sum.

  wl_fieldcat-col_pos       = p_col_pos.

  if p_outputlen is not initial.
    wl_fieldcat-outputlen  = p_outputlen.
  endif.

  wl_fieldcat-no_out        = ' '.
  wl_fieldcat-reptext_ddic  = p_scrtext_l.
  wl_fieldcat-seltext_s     = p_scrtext_l.
  wl_fieldcat-seltext_m     = p_scrtext_l.
  wl_fieldcat-seltext_l     = p_scrtext_l.
  wl_fieldcat-emphasize     = p_emphasize.

*  IF p_f4 IS NOT INITIAL.
*    wl_fieldcat-f4availabl  = c_x.
*  ENDIF.

  if p_ico is not initial.
    wl_fieldcat-hotspot     = c_x.
    wl_fieldcat-icon        = c_x.
  endif.

*  APPEND wl_fieldcat TO tl_fieldcat.
  append wl_fieldcat.

endform.                    " F_MONTAR_ESTRUTURA_OBJ

*&---------------------------------------------------------------------*
*&      Form  F_STATUS_POPUP
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_status_popup using p_set type kkblo_t_extab.
  data: fcode type table of sy-ucomm.

  data: gr_events       type ref to lcl_event_handler,
        ls_sel_hide     type slis_sel_hide_alv,
        it_fieldcatalog type lvc_t_fcat,
        wa_fieldcatalog type lvc_s_fcat,
        is_table        type lvc_s_stbl.

  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    importing
      es_sel_hide = ls_sel_hide
      e_grid      = ref1.

  call method ref1->get_frontend_fieldcatalog
    importing
      et_fieldcatalog = it_fieldcatalog.


  call method ref1->set_frontend_fieldcatalog
    exporting
      it_fieldcatalog = it_fieldcatalog.


  is_table-row = 'X'.
  is_table-col = 'X'.

  call method ref1->refresh_table_display
    exporting
      is_stable      = is_table
      i_soft_refresh = 'X'.

  "IF INIT IS INITIAL.
  call method ref1->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  call method ref1->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  create object gr_events.
  set handler:    gr_events->on_data_changed_pop for ref1.
  init = 'X'.
  "ENDIF.

  refresh: fcode.
  set pf-status 'STATUSPOPUP' excluding fcode.
endform.                    "F_STATUS_POPUP

*&---------------------------------------------------------------------*
*&      Form  F_USER_COMANDO_POPUP
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_user_comando_popup using p_ucomm     type sy-ucomm
                                p_selfield  type slis_selfield.

*   col_stable(1) type c
*   row_stable(1) type c

  case p_ucomm.
    when '&AC1'.
      set screen 0.
      leave screen.
    when '&ADD'.
      clear: wg_obj.
      append wg_obj to tl_obj_aux.

      p_selfield-refresh    = 'X'.
    when '&DEL'.
      delete tl_obj_aux index p_selfield-tabindex.

      p_selfield-refresh    = 'X'.
    when '&ONT'.
  endcase.

endform.                    "F_USER_COMANDO_POPUP

*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_limpa_campos.
  clear:    wg_zglt034, wg_zglt035, wg_zglt036, wg_obj.

  refresh:  tg_zglt036, tg_obj, tg_zglt036_excel.
  clear x_visao.
*  BTN_VISAO = TEXT-B05.
endform.                    " F_LIMPA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ALV
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form f_atualiza_alv.
  data: fg_doc(1),
        vlr_toler   type zglt036-vlr_moeda_int,
        vlr_tot_int type zglt036-vlr_moeda_int,
        vlr_tot_doc type zglt036-vlr_moeda_doc,
        vlr_tot_gru type zglt036-vlr_moeda_grupo,
        vlr_tot_for type zglt036-vlr_moeda_forte.

  call method grid1->get_selected_cells
    importing
      et_cell = tg_selectedcell.

  clear : fg_doc, vlr_tot_doc, vlr_tot_int, vlr_tot_for,vlr_tot_gru.

**  Begin of CS2022000638   #80266 FF   12.01.2023
  if tg_selectedcell[] is initial and tg_zglt036[] is not initial.
    append wg_selectedcell to tg_selectedcell. "Insere uma linha apenas para forçar a entrada no loop e atualizar os dados do ALV.
  endif.
** End of FF  12.01.2023

  loop at tg_selectedcell into wg_selectedcell.
    loop at tg_zglt036 into wg_zglt036.

      translate wg_zglt036-sgtxt to upper case.

      if wg_zglt036-d_c = 'C'.
        if wg_zglt036-vlr_moeda_int   >= 0.
          wg_zglt036-vlr_moeda_int    = wg_zglt036-vlr_moeda_int  * -1.
        endif.

        if wg_zglt036-vlr_moeda_forte >= 0.
          wg_zglt036-vlr_moeda_forte  = wg_zglt036-vlr_moeda_forte * -1.
        endif.

        if wg_zglt036-vlr_moeda_grupo >= 0.
          wg_zglt036-vlr_moeda_grupo  = wg_zglt036-vlr_moeda_grupo * -1.
        endif.

        if wg_zglt036-vlr_moeda_doc >= 0.
          wg_zglt036-vlr_moeda_doc  = wg_zglt036-vlr_moeda_doc * -1.
        endif.

        if wg_zglt036-vlr_moeda_doc   = 0 and wg_zglt036-check = 'X'.
*          FG_DOC = 'X'. "Linha com zero Erro
        endif.
      endif.
      "Arredondamento
      add wg_zglt036-vlr_moeda_doc   to vlr_tot_doc.
      add wg_zglt036-vlr_moeda_grupo to vlr_tot_gru.
      add wg_zglt036-vlr_moeda_int   to vlr_tot_int.
      add wg_zglt036-vlr_moeda_forte to vlr_tot_for.

      if ( wg_zglt036-dt_vct is not initial ) and ( wg_zglt036-dt_vct < wg_zglt035-budat ).
        clear: wg_zglt036-dt_vct.
        modify tg_zglt036 from wg_zglt036.
        message text-f02 type 'I'.
*        MESSAGE 'Dt.Vencimento deve ser maior que Dt.de Lançamento' TYPE 'I'.
        exit.
      endif.

      clear: wa_style.
      refresh: wg_zglt036-style, style.
      if wg_zglt036-xclasse = 'X'.
        wa_style-fieldname = 'VLR_MOEDA_INT'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .

        wa_style-fieldname = 'VLR_MOEDA_FORTE'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .

        wa_style-fieldname = 'VLR_MOEDA_GRUPO'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .

        wa_style-fieldname = 'VLR_MOEDA_DOC'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .
      endif.
      if '40_50' cs wg_zglt036-bschl.
        wa_style-fieldname = 'DT_VCT'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .

        wa_style-fieldname = 'HBKID'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .

        wa_style-fieldname = 'BVTYP'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .

        wa_style-fieldname = 'ZLSCH'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .

        wa_style-fieldname = 'ZLSPR'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .

      endif.

      if wg_zglt035-tp_lcto is not initial.
        if wg_zglt036-hkont is not initial.
          wa_style-fieldname = 'HKONT'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.
      endif.

      insert lines of style into table  wg_zglt036-style.

      modify tg_zglt036 from wg_zglt036.
    endloop.

  endloop.

**  Begin of CS2022000638   #80266 FF   02.02.2023
  delete tg_selectedcell where row_id is initial.
** End of FF  02.02.2023

  if wg_acao = c_displa.
    exit.
  endif.

  vlr_toler = 2 / 100.
  "INTERNO
  if fg_doc is initial and vlr_tot_doc = 0
    and  abs( vlr_tot_int ) gt 0
    and  abs( vlr_tot_int ) le vlr_toler. "Arredondar
    loop at tg_zglt036 into wg_zglt036.
      if wg_zglt036-check ne 'X'.
        continue.
      endif.
      if vlr_tot_int gt 0.
        if wg_zglt036-d_c = 'D'.
          subtract vlr_tot_int from wg_zglt036-vlr_moeda_int.
          modify tg_zglt036 from wg_zglt036 index sy-tabix transporting vlr_moeda_int.
          if wg_zglt036-xclasse = 'X'.
            loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
              if wg_obj-vlr_moeda_int eq 0.
                continue.
              endif.
*              WG_OBJ-VLR_MOEDA_INT = WG_ZGLT036-VLR_MOEDA_INT.
*              IF WG_OBJ-VLR_MOEDA_INT LT 0.
*                MULTIPLY WG_OBJ-VLR_MOEDA_INT BY -1.
*              ENDIF.
              vlr_tot_int = abs( vlr_tot_int ).
              subtract vlr_tot_int from wg_obj-vlr_moeda_int.
              modify tg_obj from wg_obj index sy-tabix transporting vlr_moeda_int.
              exit.
            endloop.
          endif.
          exit.
        endif.
      else.
        if wg_zglt036-d_c = 'C'.
          subtract vlr_tot_int from wg_zglt036-vlr_moeda_int.
          modify tg_zglt036 from wg_zglt036 index sy-tabix transporting vlr_moeda_int.
          if wg_zglt036-xclasse = 'X'.
            loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
              if wg_obj-vlr_moeda_int eq 0.
                continue.
              endif.
*              WG_OBJ-VLR_MOEDA_INT = WG_ZGLT036-VLR_MOEDA_INT.
*              IF WG_OBJ-VLR_MOEDA_INT LT 0.
*                MULTIPLY WG_OBJ-VLR_MOEDA_INT  BY -1.
*              ENDIF.
              vlr_tot_int = abs( vlr_tot_int ).
              subtract vlr_tot_int from wg_obj-vlr_moeda_int.
              modify tg_obj from wg_obj index sy-tabix transporting vlr_moeda_int.
              exit.
            endloop.
          endif.
          exit.
        endif.
      endif.
    endloop.
  endif.
  "FORTE
  if fg_doc is initial and vlr_tot_doc = 0 and
    abs( vlr_tot_for ) gt 0
    and  abs( vlr_tot_for ) le vlr_toler. "Arredondar
    loop at tg_zglt036 into wg_zglt036.
      if wg_zglt036-check ne 'X'.
        continue.
      endif.
      if vlr_tot_for gt 0.
        if wg_zglt036-d_c = 'D'.
          subtract vlr_tot_for from wg_zglt036-vlr_moeda_forte.
          modify tg_zglt036 from wg_zglt036 index sy-tabix transporting vlr_moeda_forte.
          if wg_zglt036-xclasse = 'X'.
            loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
              if wg_obj-vlr_moeda_forte eq 0.
                continue.
              endif.
*              WG_OBJ-VLR_MOEDA_FORTE = WG_ZGLT036-VLR_MOEDA_FORTE.
*              IF WG_OBJ-VLR_MOEDA_FORTE LT 0.
*                MULTIPLY WG_OBJ-VLR_MOEDA_FORTE BY -1.
*              ENDIF.
              vlr_tot_for = abs( vlr_tot_for ).
              subtract vlr_tot_for from wg_obj-vlr_moeda_forte.
              modify tg_obj from wg_obj index sy-tabix transporting vlr_moeda_forte.
              exit.
            endloop.
          endif.
          exit.
        endif.
      else.
        if wg_zglt036-d_c = 'C'.
          subtract vlr_tot_for from wg_zglt036-vlr_moeda_forte.
          modify tg_zglt036 from wg_zglt036 index sy-tabix transporting vlr_moeda_forte.
          if wg_zglt036-xclasse = 'X'.
            loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
              if wg_obj-vlr_moeda_forte eq 0.
                continue.
              endif.
*              WG_OBJ-VLR_MOEDA_FORTE = WG_ZGLT036-VLR_MOEDA_FORTE.
*              IF WG_OBJ-VLR_MOEDA_FORTE LT 0.
*                MULTIPLY WG_OBJ-VLR_MOEDA_FORTE BY -1.
*              ENDIF.
              vlr_tot_for = abs( vlr_tot_for ).
              subtract vlr_tot_for from  wg_obj-vlr_moeda_forte.
              modify tg_obj from wg_obj index sy-tabix transporting vlr_moeda_forte.
              exit.
            endloop.
          endif.
          exit.
        endif.
      endif.
    endloop.
  endif.
  "GRUPO
  if fg_doc is initial and vlr_tot_doc = 0 and
    abs( vlr_tot_gru ) gt 0
    and  abs( vlr_tot_gru ) le vlr_toler. "Arredondar
    loop at tg_zglt036 into wg_zglt036.
      if wg_zglt036-check ne 'X'.
        continue.
      endif.
      if vlr_tot_gru gt 0.
        if wg_zglt036-d_c = 'D'.
          subtract vlr_tot_gru from wg_zglt036-vlr_moeda_grupo.
          modify tg_zglt036 from wg_zglt036 index sy-tabix transporting vlr_moeda_grupo.
          if wg_zglt036-xclasse = 'X'.
            loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
              if wg_obj-vlr_moeda_grupo eq 0.
                continue.
              endif.
*              WG_OBJ-VLR_MOEDA_GRUPO = WG_ZGLT036-VLR_MOEDA_GRUPO.
*              IF WG_OBJ-VLR_MOEDA_GRUPO LT 0.
*                MULTIPLY WG_OBJ-VLR_MOEDA_GRUPO BY -1.
*              ENDIF.
              vlr_tot_gru = abs( vlr_tot_gru ).
              subtract vlr_tot_gru from wg_obj-vlr_moeda_grupo.
              modify tg_obj from wg_obj index sy-tabix transporting vlr_moeda_grupo.
              exit.
            endloop.
          endif.
          exit.
        endif.
      else.
        if wg_zglt036-d_c = 'C'.
          subtract vlr_tot_gru from wg_zglt036-vlr_moeda_grupo.
          modify tg_zglt036 from wg_zglt036 index sy-tabix transporting vlr_moeda_grupo.
          if wg_zglt036-xclasse = 'X'.
            loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
              if wg_obj-vlr_moeda_grupo eq 0.
                continue.
              endif.
*              WG_OBJ-VLR_MOEDA_GRUPO = WG_ZGLT036-VLR_MOEDA_GRUPO.
*              IF WG_OBJ-VLR_MOEDA_GRUPO LT 0.
*                MULTIPLY WG_OBJ-VLR_MOEDA_GRUPO BY -1.
*              ENDIF.
              vlr_tot_gru = abs( vlr_tot_gru ).
              subtract vlr_tot_gru from wg_obj-vlr_moeda_grupo.
              modify tg_obj from wg_obj index sy-tabix transporting vlr_moeda_grupo.
              exit.
            endloop.
          endif.
          exit.
        endif.
      endif.
    endloop.
  endif.

endform.                    " F_ATUALIZA_ALV
*&---------------------------------------------------------------------*
*&      Form  F_ELIMINAR_LANCAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_eliminar_lancamento .
  data: wl_zglt035 type zglt035,
        wl_zglt067 type zglt067.

  select single *
    from zglt035
    into wl_zglt035
   where doc_lcto = wg_zglt035-doc_lcto.

  if sy-subrc is initial.
    if wl_zglt035-loekz is initial.

      select single *
        from zglt067
        into wl_zglt067
       where doc_lcto = wg_zglt035-doc_lcto.

      move: c_x to wl_zglt035-loekz,
            c_0 to wl_zglt067-doc_lcto.

      modify zglt035 from wl_zglt035.
      modify zglt067 from wl_zglt067.

      message s836(sd) with text-e56.
    else.
      message s836(sd) display like 'E' with  text-e57
                            text-e58.
    endif.
  endif.
endform.                    " F_ELIMINAR_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Module  VALIDA_PARAMETROS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module valida_parametros input.
  refresh: tg_zglt036.

  move: c_search to ok-code,
        c_add    to wg_acao,
        ''       to wg_zglt035-monat.
endmodule.                 " VALIDA_PARAMETROS  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_doc input.
  data: tl_return_tab type table of ddshretval with header line,
        tl_dselc      type table of dselc      with header line.

  data: begin of tl_docs occurs 0,
          lote     type zglt035-lote,
          doc_lcto type zglt035-doc_lcto,
          bukrs    type zglt034-bukrs,
        end of tl_docs.

  select  lote doc_lcto bukrs
     from zglt035
     into table tl_docs.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'DOC_LCTO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT035-DOC_LCTO'
      value_org       = 'S'
    tables
      value_tab       = tl_docs
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
endmodule.                 " SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ERR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_err .
  refresh tg_fieldcatalog.
  perform f_montar_estrutura using:
        1 'ZIB_CONTABIL_ERR'         'OBJ_KEY'        'IT_ZIB_CONTABIL_ERR' 'OBJ_KEY'         ' '   '20' ' ' ' ' ' ' ' ' ' ',
        1 'ZIB_CONTABIL_ERR'         'NR_ITEM'        'IT_ZIB_CONTABIL_ERR' 'NR_ITEM'         ' '   '10' ' ' ' ' ' ' ' ' ' ',
        2 'ZIB_CONTABIL_ERR'         'INTERFACE'      'IT_ZIB_CONTABIL_ERR' 'INTERFACE'       ' '   '15' ' ' ' ' ' ' ' ' ' ',
        3 'ZIB_CONTABIL_ERR'         'DT_ATUALIZACAO' 'IT_ZIB_CONTABIL_ERR' 'DT_ATUALIZACAO'  ' '   '15' ' ' ' ' ' ' ' ' ' ',
        4 'ZIB_CONTABIL_ERR'         'HR_ATUALIZACAO' 'IT_ZIB_CONTABIL_ERR' 'HR_ATUALIZACAO'  ' '   '15' ' ' ' ' ' ' ' ' ' ',
        5 'ZIB_CONTABIL_ERR'         'TYPE'           'IT_ZIB_CONTABIL_ERR' 'TYPE'            ' '   '08' ' ' ' ' ' ' ' ' ' ',
        6 'ZIB_CONTABIL_ERR'         'ID'             'IT_ZIB_CONTABIL_ERR' 'ID'              ' '   '10' ' ' ' ' ' ' ' ' ' ',
        7 'ZIB_CONTABIL_ERR'         'NUM'            'IT_ZIB_CONTABIL_ERR' 'NUM'             ' '   '10' ' ' ' ' ' ' ' ' ' ',
        8 ' '                        ' '              'IT_ZIB_CONTABIL_ERR' 'MESSAGE'         'Mensagem de Erro '   '100' ' ' ' ' ' ' ' ' ' ',
        9 'ZIB_CONTABIL_ERR'         'MESSAGE_V1'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V1'      ' '   '50' ' ' ' ' ' ' ' ' ' ',
       10 'ZIB_CONTABIL_ERR'         'MESSAGE_V2'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V2'      ' '   '30' ' ' ' ' ' ' ' ' ' ',
       11 'ZIB_CONTABIL_ERR'         'MESSAGE_V3'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V3'      ' '   '30' ' ' ' ' ' ' ' ' ' ',
       12 'ZIB_CONTABIL_ERR'         'MESSAGE_V4'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V4'      ' '   '30' ' ' ' ' ' ' ' ' ' '.
endform.                    " MONTAR_LAYOUT_ERR

*&---------------------------------------------------------------------*
*&      Module  SEARCH_TP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_tp input.
  data: tl_return_tab2    type table of ddshretval with header line,
        tl_dselc2         type table of dselc      with header line,
        tl_zimp_cad_depto type table of zimp_cad_depto with header line,
        wl_zglt034        type zglt034,
        wl_dep_resp       type zimp_cad_depto.


  data: begin of tl_tp occurs 0,
          bukrs         type zglt031-bukrs,
          tp_lcto       type zglt031-tp_lcto,
          descricao     type zglt031-descricao,
          dep_resp      type zglt031-dpto_resp,
          dep_resp_desc type zimp_cad_depto-dep_resp_desc,
        end of tl_tp.

  data: l_dynpfields like dynpread occurs 0 with header line.
  refresh l_dynpfields.
  clear   l_dynpfields.


  l_dynpfields-fieldname  = 'WG_ZGLT034-LOTE'.
  append l_dynpfields.

  call function 'DYNP_VALUES_READ'
    exporting
      dyname     = sy-repid
      dynumb     = sy-dynnr
    tables
      dynpfields = l_dynpfields.
  read table l_dynpfields index 1.
  move l_dynpfields-fieldvalue to wg_zglt034-lote.


  check wg_zglt034-lote is not initial.

  select single *
    from zglt034
    into wl_zglt034
    where lote = wg_zglt034-lote.

  select single *
    from zimp_cad_depto
    into wl_dep_resp
  where dep_resp = wl_zglt034-dep_resp.

  if wl_dep_resp-bukrs is initial.
    select zglt031~bukrs zglt031~tp_lcto zglt031~descricao zglt031~dpto_resp  zimp_cad_depto~dep_resp_desc
       from zglt031
       inner join zimp_cad_depto on zimp_cad_depto~dep_resp = zglt031~dpto_resp
       into table tl_tp.
  else.
    select zglt031~bukrs zglt031~tp_lcto zglt031~descricao zglt031~dpto_resp  zimp_cad_depto~dep_resp_desc
       from zglt031
       inner join zimp_cad_depto on zimp_cad_depto~dep_resp = zglt031~dpto_resp
       into table tl_tp
       where zimp_cad_depto~bukrs = wl_dep_resp-bukrs.

  endif.


  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'TP_LCTO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT031-TP_LCTO'
      value_org       = 'S'
    tables
      value_tab       = tl_tp
      return_tab      = tl_return_tab2
      dynpfld_mapping = tl_dselc2.
endmodule.                 " SEARCH_TP  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_lote input.
  data: tl_return_tab3 type table of ddshretval with header line,
        tl_dselc3      type table of dselc      with header line.

  data: begin of tl_lote occurs 0,
          bukrs         type zglt034-bukrs,
          lote          type zglt034-lote,
          descr_lote    type zglt034-descr_lote,
          usnam         type zglt034-usnam,
          dep_resp      type zimp_cad_depto-dep_resp,
          dep_resp_desc type zimp_cad_depto-dep_resp_desc,
        end of tl_lote.

  select zglt034~bukrs zglt034~lote zglt034~descr_lote zglt034~usnam zglt034~dep_resp zimp_cad_depto~dep_resp_desc
     from zglt034
     inner join zimp_cad_depto on zimp_cad_depto~dep_resp = zglt034~dep_resp
     into table tl_lote
     where zglt034~status_lote = ''
     and   zglt034~usnam       = sy-uname
    order by lote.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'LOTE'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZGLT034-LOTE'
      value_org       = 'S'
    tables
      value_tab       = tl_lote
      return_tab      = tl_return_tab3
      dynpfld_mapping = tl_dselc3.
endmodule.                 " SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*&      Module  TROCA_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module troca_data input.
  data: hojed   type sy-datum,
        ultimod type sy-datum.

  if wg_acao = c_modif and wg_zglt035-tp_lcto is not initial and wg_zglt035-monat is not initial.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wg_zglt035-monat
      importing
        output = wg_zglt035-monat.

    if wg_zglt035-gjahr is initial.
      if wg_zglt035-monat gt 12.
        concatenate sy-datum+0(4) '1201' into hojed.
      else.
        concatenate sy-datum+0(4) wg_zglt035-monat '01' into hojed.
      endif.
    else.
      if wg_zglt035-monat gt 12.
        concatenate wg_zglt035-gjahr  '1201' into hojed.
      else.
        concatenate wg_zglt035-gjahr wg_zglt035-monat '01' into hojed.
      endif.
    endif.


    call function 'FKK_LAST_DAY_OF_MONTH'
      exporting
        day_in            = hojed
      importing
        last_day_of_month = ultimod
      exceptions
        day_in_no_date    = 1
        others            = 2.

    if wg_zglt035-dt_lcto = 'X'. " Informar Data
    else.
      wg_zglt035-budat = ultimod.
    endif.

    if wg_zglt035-dt_doc = 'X'. " Informar Data
    else.
      wg_zglt035-bldat = ultimod.
    endif.

  endif.
endmodule.                 " TROCA_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_1413   text
*----------------------------------------------------------------------*
form f_carregar_eventos using    name form.
  clear xs_events.
  xs_events-name = name.
  xs_events-form = form.
  append xs_events to pop_events.
endform.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Module  RECALC_MOEDA_FT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module recalc_moeda_ft input.

  check tg_zglt036_excel[] is initial. " CS2022000638   #80266 FF   03.02.2023

  data: vdata      type tcurr-gdatu,
        vdatax     type sy-datum,
        vdata_f    type tcurr-gdatu,
        wl_t001    type t001,
        wl_t005    type t005,
        tl_tcurr   type table of tcurr,
        wl_tcurr   type tcurr,
        wl_tcurr_f type tcurr,
        wl_tcurr_g type tcurr.


  data: chdat(8)   type c,
        houtput(8) type n.

  clear vdata.

  call function 'SAPGUI_SET_FUNCTIONCODE'
    exporting
      functioncode           = '=ENT' "ENTER
    exceptions
      function_not_supported = 1
      others                 = 2.


  clear wl_tcurr.
  loop at tg_zglt036 into wg_zglt036.
    if wg_zglt036-check = 'X'.
      wg_zglt036-vlr_moeda_forte = 0.
      wg_zglt036-vlr_moeda_doc   = 0.
      wg_zglt036-vlr_moeda_int   = 0.
      wg_zglt036-vlr_moeda_grupo = 0.
      modify tg_zglt036 from wg_zglt036 index sy-tabix transporting vlr_moeda_int vlr_moeda_doc vlr_moeda_forte vlr_moeda_grupo.
    endif.

  endloop.


**** Método de atualização de dados na Tela
  call method grid1->refresh_table_display
    exporting
      is_stable = wa_stable.


endmodule.                 " RECALC_MOEDA_FT  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_VAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_vat .
  refresh tg_fieldcatalog.
  perform f_montar_estrutura using:
        1 ' '         ' '        'TG_VAT' 'SEQITEM'       text-a13   '07' ' ' ' ' ' ' ' ' ' ',
        1 ' '         ' '        'TG_VAT' 'MWSKZ'         text-a40   '10' ' ' ' ' ' ' ' ' ' ',
        1 ' '         ' '        'TG_VAT' 'MSATZ'         text-a41   '10' ' ' ' ' ' ' ' ' ' ',
        1 ' '         ' '        'TG_VAT' 'BASE'          text-a42   '15' ' ' 'X' ' ' ' ' ' ',
        1 ' '         ' '        'TG_VAT' 'KBETR'         text-a43   '15' ' ' 'X' ' ' ' ' ' ',
        1 ' '         ' '        'TG_VAT' 'HKONT'         text-a44   '15' ' ' ' ' ' ' ' ' ' ',
        1 ' '         ' '        'TG_VAT' 'TXT50'         text-a45   '40' ' ' ' ' ' ' ' ' ' ',
        1 ' '         ' '        'TG_VAT' 'KOSTL'         text-a04   '12' ' ' ' ' ' ' ' ' ' ',
        1 ' '         ' '        'TG_VAT' 'PRCTR'         text-a05   '12' ' ' ' ' ' ' ' ' ' ',
        1 ' '         ' '        'TG_VAT' 'AUFNR'         text-a06   '12' ' ' ' ' ' ' ' ' ' ',
        1 ' '         ' '        'TG_VAT' 'MATNR'         text-a07   '15' ' ' ' ' ' ' ' ' ' '.
endform.                    " MONTAR_LAYOUT_VAT

form montar_layout_obj.
  refresh tg_fieldcatalog.
  perform f_montar_estrutura using:
        1   ' '       ' '                 'TG_OBJ'  'HKONT'                text-a01          '12'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TG_OBJ'  'TXT50'                text-a02          '15'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TG_OBJ'  'VBUND'                text-a03          '10'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TG_OBJ'  'IVA'                  'IVA'             '30'  ' ' ' ' ' ' ' ' ' ',
        1   'CSKS'    'KOSTL'             'TG_OBJ'  'KOSTL'                text-a04          '12'  'X' ' ' ' ' ' ' ' ',
        2   'CEPC'    'PRCTR'             'TG_OBJ'  'PRCTR'                text-a05          '12'  'X' ' ' ' ' ' ' ' ',
        3   'AFIH'    'AUFNR'             'TG_OBJ'  'AUFNR'                text-a06          '12'  'X' ' ' ' ' ' ' ' ',
        3   ' '       ' '                 'TG_OBJ'  'VORNR'                text-a49          '05'  'X' ' ' ' ' 'X' ' ',
        4   'ZGLT036' 'MATNR'             'TG_OBJ'  'MATNR'                text-a07          '12'  'X' ' ' ' ' ' ' ' ',
        5   'ZGLT036' 'VLR_MOEDA_DOC'     'TG_OBJ'  'VLR_MOEDA_DOC'        text-a08          '20'  'X' ' ' ' ' ' ' ' ',
        5   'ZGLT036' 'VLR_MOEDA_INT'     'TG_OBJ'  'VLR_MOEDA_INT'        text-a09          '20'  'X' ' ' ' ' ' ' ' ',
        6   'ZGLT036' 'VLR_MOEDA_FORTE'   'TG_OBJ'  'VLR_MOEDA_FORTE'      text-a10          '20'  'X' ' ' ' ' ' ' ' ',
        7   'ZGLT036' 'VLR_MOEDA_GRUPO'   'TG_OBJ'  'VLR_MOEDA_GRUPO'      text-a11          '20'  'X' ' ' ' ' ' ' ' ',
        7   'ZGLT036' 'QUANTITY'          'TG_OBJ'  'QUANTITY'             text-a46          '10'  'X' ' ' ' ' ' ' ' ',
        7   'ZGLT036' 'BASE_UOM'          'TG_OBJ'  'BASE_UOM'             text-a47          '07'  'X' ' ' ' ' ' ' ' '.
endform.                    " MONTAR_LAYOUT_VAT

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_LANCAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_copiar_lancamento .

  data: wl_zglt034         type zglt034,
        wl_zglt033         type zglt033,
        wl_zglt031         type zglt031,
        tl_zglt032         type table of zglt032,
        wl_zglt032         type zglt032,
        wl_zglt035         type zglt035,
        wl_zglt036         type zglt036,
        wl_akont           type lfb1-akont,
        wl_hkont           type t074-hkont,
        wl_zglt036_aux     type zglt036,
        tl_zglt036         type table of zglt036,
        tl_zglt036_aux     type table of zglt036,
        wl_tbsl            type tbsl,
        wl_skb1            type skb1,
* ---> S4 Migration - 17/07/2023 - CA
*        wl_cskb            TYPE cskb,
* <--- S4 Migration - 17/07/2023 - CA
        wl_csks            type csks,
        wl_tka02           type tka02,
        wl_t001            type t001,
        wl_bkpf            type bkpf,
        vl_vlr_moeda_int   type zglt036-vlr_moeda_int,
        vl_vlr_moeda_forte type zglt036-vlr_moeda_forte,
        vl_vlr_moeda_doc   type zglt036-vlr_moeda_doc,
        vl_vlr_moeda_grupo type zglt036-vlr_moeda_grupo,
        wl_cont            type sy-tabix,
        wl_cont_aux        type sy-tabix,
        wl_cont_aux2       type sy-tabix,
        wl_item            type sy-tabix,
        tabix              type sy-tabix,
        xobj_key           type zib_contabil-obj_key,
        w_answer_c.

* ---> S4 Migration - 17/07/2023 - CA
  data: lt_returns         type table of bapiret2,
        ls_coeldes         type bapi1030_ceoutputlist,
        lv_controllingarea type bapi1030_gen-co_area,
        lv_costelement     type bapi1030_gen-cost_elem,
        lv_keydate         type bapi1030_gen-some_date.
* <--- S4 Migration - 17/07/2023 - CA

  if wg_zglt035-belnr is not initial.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wg_zglt035-belnr
      importing
        output = wg_zglt035-belnr.
    select single *
      from bkpf
      into wl_bkpf
      where belnr = wg_zglt035-belnr
      and   awkey like 'ZGL17%'.
    if sy-subrc ne 0.
      message 'Lançamento não é da ZGL016' type 'I'.
      exit.
    endif.
    wg_zglt035-doc_lcto = wl_bkpf-awkey+5(10).
  elseif ( vg_belnr_copia is not initial ).
    wg_zglt035-belnr = vg_belnr_copia.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wg_zglt035-belnr
      importing
        output = wg_zglt035-belnr.
    select single *
      from bkpf
      into wl_bkpf
      where belnr = wg_zglt035-belnr
      and   awkey like 'ZGL17%'.
    if sy-subrc ne 0.
      message 'Lançamento não é da ZGL016' type 'I'.
      exit.
    endif.
    wg_zglt035-doc_lcto = wl_bkpf-awkey+5(10).
  endif.
  call function 'POPUP_TO_CONFIRM'
    exporting
*     TITLEBAR              = ' '
      text_question         = text-p04
      text_button_1         = text-p02 "'Sim'(001)
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = text-p03 "'Não'(002)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    importing
      answer                = w_answer_c
    exceptions
      text_not_found        = 1
      others                = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  if w_answer_c = '1'.

  endif.

  clear: wg_zglt036.
  refresh: tg_zglt036, tg_zglt036_ori, tg_obj.

  select single * from zglt035 into wl_zglt035 where doc_lcto eq wg_zglt035-doc_lcto and loekz = ''.
  check sy-subrc eq 0.
  select single * from zglt031 into wl_zglt031 where tp_lcto eq wl_zglt035-tp_lcto.

  move: wl_zglt031-descricao        to wg_zglt035-descricao,
        wl_zglt035-lote             to wg_zglt034-lote,
*        WL_ZGLT035-BUKRS            TO WG_ZGLT035-BUKRS,
        wl_zglt035-tp_lcto          to wg_zglt035-tp_lcto,
        wl_zglt035-dpto_resp        to wg_zglt035-dpto_resp,
        wl_zglt035-taxa             to wg_zglt035-taxa,
        wl_zglt035-moeda_doc        to wg_zglt035-moeda_doc,
        wl_zglt035-st_lc_moeda      to wg_zglt035-st_lc_moeda,
        wl_zglt035-moeda_interna    to wg_zglt035-moeda_interna,
        wl_zglt035-moeda_int_hist   to wg_zglt035-moeda_int_hist,
        wl_zglt035-moeda_forte      to wg_zglt035-moeda_forte,
        wl_zglt035-moeda_ft_hist    to wg_zglt035-moeda_ft_hist,
        wl_zglt035-moeda_grupo      to wg_zglt035-moeda_grupo,
        wl_zglt035-moeda_gp_hist    to wg_zglt035-moeda_gp_hist,
        wl_zglt035-blart            to wg_zglt035-blart,
        wl_zglt035-xblnr            to wg_zglt035-xblnr,
        wl_zglt035-bktxt            to wg_zglt035-bktxt,
        wl_zglt035-bldat            to wg_zglt035-bldat,
        wl_zglt035-budat            to wg_zglt035-budat,
        wl_zglt035-prov_est         to wg_zglt035-prov_est,
        wl_zglt035-usnam            to wg_zglt035-usnam,
        wl_zglt035-budat            to wg_zglt035-budat,
        wl_zglt035-bldat            to wg_zglt035-bldat,
        wl_zglt035-dt_entrada       to wg_zglt035-dt_entrada,
        wl_zglt035-hr_entrada       to wg_zglt035-hr_entrada,
        wl_zglt035-monat            to wg_zglt035-monat,
        wl_zglt035-gjahr            to wg_zglt035-gjahr,
        wl_zglt035-belnr            to wg_zglt035-belnr,
        wl_zglt035-st_ap_fiscal     to wg_zglt035-st_ap_fiscal,
        wl_zglt035-st_fecha         to wg_zglt035-st_fecha,
        wl_zglt035-st_agrupa        to wg_zglt035-st_agrupa.


  select single * from zglt033 into wl_zglt033 where cod_depto eq wg_zglt035-dpto_resp.
  select single *
    from t001
    into wl_t001
    where bukrs = wl_zglt035-bukrs.

  if sy-subrc = 0.
*    MOVE: WL_T001-BUTXT   TO WG_ZGLT035-BUTXT.
  endif.

  "Erros contabil
  refresh it_zib_contabil_err.

  refresh: tg_editor.
  clear: wl_cont_aux2, wl_cont_aux, wl_cont.
  wl_cont = strlen( wl_zglt035-ref_lcto ).
  wl_cont_aux = wl_cont / 72.

  do.
    move: wl_zglt035-ref_lcto+wl_cont_aux2 to wg_editor-line.
    add 72 to wl_cont_aux2.
    append wg_editor to tg_editor.

    if wl_cont_aux2 gt wl_cont.
      exit.
    endif.
  enddo.

  call method obg_descbox->set_text_as_r3table
    exporting
      table = tg_editor.
  call method obg_descbox->set_readonly_mode
    exporting
      readonly_mode = 1.

  select single * from zglt031 into wl_zglt031 where tp_lcto eq wg_zglt035-tp_lcto.
  if sy-subrc = 0.
    move: wl_zglt031-dt_doc           to wg_zglt035-dt_doc,
          wl_zglt031-dt_doc_ult_mes   to wg_zglt035-dt_doc_ult_mes,
          wl_zglt031-dt_lcto          to wg_zglt035-dt_lcto,
          wl_zglt031-dt_lcto_ult_mes  to wg_zglt035-dt_lcto_ult_mes.
  endif.
  select *
    from zglt036 into table tl_zglt036 where doc_lcto eq wg_zglt035-doc_lcto.

  check sy-subrc eq 0.

  refresh tg_obj.

  tl_zglt036_aux[] = tl_zglt036[].
  sort: tl_zglt036     by seqitem seqsub,
        tl_zglt036_aux by seqitem seqsub.

  delete adjacent duplicates from tl_zglt036 comparing seqitem.
  sort    tl_zglt036 by seqitem.
  loop at tl_zglt036 into wl_zglt036.
    move-corresponding wl_zglt036 to wg_zglt036.
    wg_zglt036-check = 'X'.
    if w_answer_c eq '1'.
      clear : wg_zglt036-vlr_moeda_int,
              wg_zglt036-vlr_moeda_forte,
              wg_zglt036-vlr_moeda_doc ,
              wg_zglt036-vlr_moeda_grupo .
    endif.
    move wl_zglt036-hkont to wg_zglt036-akont.
    move wl_zglt036-gsber  to wg_zglt036-divisao.
    wg_zglt036-seqitem  = wg_zglt036-seqitem.
    wg_zglt036-icon     = '@1F@'.

    if wl_zglt036-bschl is not initial.
      select single * from tbsl into wl_tbsl where bschl eq wl_zglt036-bschl.
      if sy-subrc eq 0.
        case wl_tbsl-koart.
          when 'K'.
            select single name1 from lfa1 into wg_zglt036-descr where lifnr eq wl_zglt036-hkont+0(10).
            select single akont from lfb1 into wl_akont         where lifnr = wg_zglt036-hkont+0(10) and bukrs = wg_zglt035-bukrs.
            wg_zglt036-akont = wl_akont.
            if wl_zglt036-umskz ne ''.
              select single skont from t074 into wl_hkont
                where ktopl = '0050'
                and   koart = 'K'
                and   umskz = wl_zglt036-umskz
                and   hkont = wl_akont.
              wg_zglt036-akont = wl_hkont.
            endif.
            select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                                 and spras eq sy-langu
                                                                 and ktopl eq '0050'.
          when 'D'.
            select single name1 from kna1 into wg_zglt036-descr where kunnr eq wl_zglt036-hkont+0(10).
            select single akont from knb1 into wl_akont         where kunnr = wg_zglt036-hkont+0(10)  and bukrs = wg_zglt035-bukrs.
            wg_zglt036-akont = wl_akont.
            if wl_zglt036-umskz ne ''.
              select single skont from t074 into wl_hkont
                where ktopl = '0050'
                and   koart = 'D'
                and   umskz = wl_zglt036-umskz
                and   hkont = wl_akont.
              wg_zglt036-akont = wl_hkont.
            endif.
            select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                                 and spras eq sy-langu
                                                                 and ktopl eq '0050'.
          when 'S'.
            select single txt50 from skat into wg_zglt036-descr where saknr eq wl_zglt036-hkont+0(10)
                                                                 and spras eq sy-langu
                                                                 and ktopl eq '0050'.
            wg_zglt036-descr_a = wg_zglt036-descr.
          when 'I'.
            select single mcoa1 from anla into wg_zglt036-descr where anln1 eq wg_zglt036-hkont.
            wg_zglt036-descr_a = wg_zglt036-descr.
        endcase.

        if wl_tbsl-shkzg eq 'S'.
          wg_zglt036-d_c = 'D'.
        else.
          wg_zglt036-d_c = 'C'.
        endif.

        select single *
          from tka02
          into wl_tka02
          where bukrs  = wl_zglt035-bukrs.

* ---> S4 Migration - 17/07/2023 - CA
*        CLEAR wl_cskb.
*        SELECT SINGLE *
*            FROM cskb
*            INTO wl_cskb
*            WHERE  kokrs  = wl_tka02-kokrs
*            AND    kstar  = wg_zglt036-hkont+0(10)
*            AND    datab  LE sy-datum
*            AND    datbi  GE sy-datum.
* <--- S4 Migration - 17/07/2023 - CA

        "ALRS 27.11.2015
        clear wl_skb1.
        if '0200_0201_0202' cs wg_zglt035-bukrs. "EUROPA
          select single *              "#EC CI_DB_OPERATION_OK[2431747]
            from skb1
            into wl_skb1
            where bukrs = wg_zglt035-bukrs
            and   saknr = wg_zglt036-hkont+0(10)
            and fstag   = 'YB09'.
        endif.

* ---> S4 Migration - 17/07/2023 - CA
        lv_controllingarea  = wl_tka02-kokrs.
        lv_costelement      = wg_zglt036-hkont+0(10).
        lv_keydate          = sy-datum.

        clear: lt_returns[], ls_coeldes.

        call function 'K_COSTELEM_BAPI_GETDETAIL'
          exporting
            controllingarea   = lv_controllingarea
            costelement       = lv_costelement
            keydate           = lv_keydate
          importing
            costelementdetail = ls_coeldes
          tables
            return            = lt_returns.

        if ( ls_coeldes-cost_elem is not initial and ls_coeldes-celem_category is not initial or wl_skb1-fstag   = 'YB09' ) and wl_tbsl-koart  = 'S'.
*        IF ( wl_cskb-kstar IS NOT INITIAL OR wl_skb1-fstag   = 'YB09' ) AND wl_tbsl-koart  = 'S'.
* <--- S4 Migration - 17/07/2023 - CA
          move 'X' to wg_zglt036-xclasse.
          wg_zglt036-icon     = '@1F@'.
        else.
          clear:  wg_zglt036-xclasse, wg_zglt036-icon.
        endif.

      endif.
    endif.

    vl_vlr_moeda_int    = 0.
    vl_vlr_moeda_forte  = 0.
    vl_vlr_moeda_doc    = 0.
    vl_vlr_moeda_grupo  = 0.

    loop at tl_zglt036_aux into wl_zglt036_aux where seqitem = wl_zglt036-seqitem.
      if wl_zglt036_aux-seqsub gt 0.
        if w_answer_c eq '1'.
          clear : wl_zglt036_aux-vlr_moeda_int,
                  wl_zglt036_aux-vlr_moeda_forte,
                  wl_zglt036_aux-vlr_moeda_doc ,
                  wl_zglt036_aux-vlr_moeda_grupo .
        endif.

        move-corresponding wl_zglt036_aux to wg_obj.

        select single *
            from skat
            into @data(wl_skat)
            where spras eq @sy-langu
            and ktopl eq '0050'
            and saknr eq  @wg_obj-hkont.

        wg_obj-txt50 = wl_skat-txt50.

        append wg_obj to tg_obj.
        add wl_zglt036_aux-vlr_moeda_int   to vl_vlr_moeda_int.
        add wl_zglt036_aux-vlr_moeda_forte to vl_vlr_moeda_forte.
        add wl_zglt036_aux-vlr_moeda_doc   to vl_vlr_moeda_doc.
        add wl_zglt036_aux-vlr_moeda_grupo to vl_vlr_moeda_grupo.
      endif.
    endloop.

    if wl_zglt036-seqsub gt 0.
      move :  vl_vlr_moeda_int    to wg_zglt036-vlr_moeda_int,
              vl_vlr_moeda_forte  to wg_zglt036-vlr_moeda_forte,
              vl_vlr_moeda_doc    to wg_zglt036-vlr_moeda_doc ,
              vl_vlr_moeda_grupo  to wg_zglt036-vlr_moeda_grupo .
    endif.

    if wg_zglt036-d_c = 'C'.
      multiply  wg_zglt036-vlr_moeda_int   by -1.
      multiply  wg_zglt036-vlr_moeda_forte by -1.
      multiply  wg_zglt036-vlr_moeda_doc   by -1.
      multiply  wg_zglt036-vlr_moeda_grupo by -1.
    endif.
    if wg_zglt036-vlr_moeda_int   ne 0 or
       wg_zglt036-vlr_moeda_forte ne 0 or
       wg_zglt036-vlr_moeda_grupo ne 0 or
       wg_zglt036-vlr_moeda_doc   ne 0.
      wg_zglt036-check = 'X'.
    endif.

    "Só uma linha
    if wg_zglt035-tp_lcto is not initial and wg_zglt036-xclasse = 'X'.
      read table tg_zglt036 into wg_zglt036_tot with key bschl = wg_zglt035-bukrs
                                                         hkont = wg_zglt036-hkont+0(10).
      if sy-subrc eq 0.
        tabix = sy-tabix.
        add wg_zglt036_tot-vlr_moeda_doc   to wg_zglt036-vlr_moeda_doc.
        add wg_zglt036_tot-vlr_moeda_int   to wg_zglt036-vlr_moeda_int.
        add wg_zglt036_tot-vlr_moeda_forte to wg_zglt036-vlr_moeda_forte.
        add wg_zglt036_tot-vlr_moeda_grupo to wg_zglt036-vlr_moeda_grupo.
        modify tg_zglt036 from wg_zglt036 index tabix transporting vlr_moeda_doc vlr_moeda_int vlr_moeda_forte vlr_moeda_grupo.
      else.
        append wg_zglt036 to tg_zglt036.
      endif.
    else.
      append wg_zglt036 to tg_zglt036.
    endif.

    clear: wg_zglt036.
  endloop.

  wg_acao = 'COPY'.

  if  v_lote15 is not initial.
    wg_zglt034-lote = v_lote15.
    clear v_lote15.
  endif.

  refresh: tg_fields.
  clear: wg_zglt035-bukrs.
  perform f_trata_campos using  space
                                'GR2'
                                c_0       "INPUT 1     NO INPUT 0
                                c_0.      "INVISIBLE 1 VISIBLE 0

  perform f_trata_campos using  space
                                'GR1'
                                c_1       "INPUT 1     NO INPUT 0
                                c_0.      "INVISIBLE 1 VISIBLE 0
  perform f_trata_campos using  space
                                'GR3'
                                c_0       "INPUT 1     NO INPUT 0
                                c_0.      "INVISIBLE 1 VISIBLE 0

  call method obg_descbox->set_text_as_r3table
    exporting
      table = tg_editor.

  call method obg_descbox->set_readonly_mode
    exporting
      readonly_mode = 0.

  clear: wg_zglt035-doc_lcto, wg_zglt034-lote.


endform.

*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_CONTAS
*&---------------------------------------------------------------------*
*       Valida as contas após informação do lote
*----------------------------------------------------------------------*
form f_valida_contas .
  field-symbols: <wl_zglt036> like line of tg_zglt036.

  loop at tg_zglt036 assigning <wl_zglt036>.

    select single *
      from tbsl
      into @data(wl_tbsl)
      where bschl eq @<wl_zglt036>-bschl.

    case wl_tbsl-koart.
      when 'K'.

        select single akont
         from lfb1
         into @data(wl_akont)
         where lifnr eq @<wl_zglt036>-hkont+0(10)
           and bukrs eq @wg_zglt035-bukrs.

        if ( sy-subrc eq 0 ).
          <wl_zglt036>-akont = wl_akont.
        endif.

        if ( <wl_zglt036>-umskz ne '' ).
          select single skont
            from t074
            into @data(wl_hkont)
            where ktopl eq '0050'
              and koart eq 'K'
              and umskz eq @<wl_zglt036>-umskz
              and hkont eq @wl_akont.

          if ( sy-subrc eq 0 ).
            <wl_zglt036>-akont = wl_hkont.
          endif.

        endif.

      when 'D'.

        select single akont
          from knb1
          into wl_akont
          where kunnr = <wl_zglt036>-hkont+0(10)
            and bukrs = wg_zglt035-bukrs.

        if ( sy-subrc eq 0 ).
          <wl_zglt036>-akont = wl_akont.
        endif.

        if ( <wl_zglt036>-umskz ne '' ).
          select single skont
            from t074
            into wl_hkont
            where ktopl eq '0050'
              and koart eq 'D'
              and umskz eq <wl_zglt036>-umskz
              and hkont eq wl_akont.

          if ( sy-subrc eq 0 ) .
            <wl_zglt036>-akont = wl_hkont.
          endif.

        endif.

    endcase.

    select single txt50
             from skat
             into <wl_zglt036>-descr_a
             where saknr eq <wl_zglt036>-akont
               and spras eq sy-langu
               and ktopl eq '0050'.

    clear: wl_tbsl, wl_akont, wl_hkont.

  endloop.

endform.
*&---------------------------------------------------------------------*
*& Form:  f_busca_arquivo                                              *
*& Descrição: Utilizado para ajuda de pesquisa do caminho de diretorio *
*&---------------------------------------------------------------------*
form f_busca_arquivo using p1 type string. "CS2022000638   #80266 FF   06.01.2023

  data: l_rc   type i,
        t_file type filetable,
        e_file type file_table.

  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      file_filter       = cl_gui_frontend_services=>filetype_excel
      initial_directory = 'C:\'
    changing
      file_table        = t_file
      rc                = l_rc.

  read table t_file into e_file index 1.

  p1 = e_file-filename.

endform.                    "f_busca_arquivo
*&---------------------------------------------------------------------*
*& Form:  f_le_arquivo                                                 *
*& Descrição: Utilizado para leitura do arquivo                        *
*&---------------------------------------------------------------------*
form f_le_arquivo.

  data: vs_filename type rlgrap-filename.

*** Função para leitura do arquivo de entrada
  vs_filename = vg_file.
  call function 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    exporting
      filename                = vs_filename
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 28
      i_end_row               = 99999
    tables
      intern                  = tg_excel
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.

  if sy-subrc <> 0.
    clear tg_excel[].
  endif.

endform.                    "f_le_arquivo
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ARQ_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_monta_arq_saida .

  data: gr_events type ref to lcl_event_handler.
  create object gr_events.

  tables lvc_s_fcat.
  data: lt_good_cell    type lvc_t_modi,
        lt_mod_cells    type lvc_t_modi,
        lt_fieldcatalog type lvc_t_fcat.

  data(lo_er_data_changed) = new cl_alv_changed_data_protocol( ).

  loop at tg_excel assigning field-symbol(<fs_line>).

    case <fs_line>-col.
      when 1.
        append initial line to tg_zglt036_excel assigning field-symbol(<zglt036_excel>).
        <zglt036_excel>-bschl = <fs_line>-value.
        <zglt036_excel>-seqitem = sy-tabix.
        <zglt036_excel>-check = 'X'.
      when 2.
        <zglt036_excel>-hkont = <fs_line>-value.
      when 3.
        <zglt036_excel>-umskz = <fs_line>-value.
      when 4.
        unpack <fs_line>-value to <zglt036_excel>-kostl.
      when 5.
        <zglt036_excel>-prctr = <fs_line>-value.
      when 6.
        <zglt036_excel>-aufnr = <fs_line>-value.
      when 7.
        <zglt036_excel>-vornr = <fs_line>-value.
      when 8.
        unpack <fs_line>-value to <zglt036_excel>-matnr.
      when 9.
        __format_value: <fs_line>-value <zglt036_excel>-quantity.
      when 10.
        <zglt036_excel>-base_uom = <fs_line>-value.
      when 11.
        <zglt036_excel>-anbwa = <fs_line>-value.
      when 12.
        <zglt036_excel>-bewar = <fs_line>-value.
      when 13.
        __format_value: <fs_line>-value <zglt036_excel>-vlr_moeda_doc.
      when 14.
        __format_value: <fs_line>-value <zglt036_excel>-vlr_moeda_int.
      when 15.
        __format_value: <fs_line>-value <zglt036_excel>-vlr_moeda_forte.
      when 16.
        __format_value: <fs_line>-value <zglt036_excel>-vlr_moeda_grupo.
      when 17.
        unpack <fs_line>-value to <zglt036_excel>-matnr_fi.
      when 18.
        <zglt036_excel>-zuonr = <fs_line>-value.
      when 19.
        <zglt036_excel>-vbeln = <fs_line>-value.
      when 20.
        unpack <fs_line>-value to <zglt036_excel>-divisao.
      when 21.
        unpack <fs_line>-value to <zglt036_excel>-werks.
      when 22.
        <zglt036_excel>-vbund = <fs_line>-value.
      when 23.
        <zglt036_excel>-sgtxt = <fs_line>-value.
      when 24.
        __format_date: <fs_line>-value <zglt036_excel>-dt_vct.
      when 25.
        <zglt036_excel>-hbkid = <fs_line>-value.
      when 26.
        <zglt036_excel>-bvtyp = <fs_line>-value.
      when 27.
        <zglt036_excel>-zlsch = <fs_line>-value.
      when 28.
        <zglt036_excel>-zlspr = <fs_line>-value.
    endcase.

  endloop.

  refresh tg_zglt036.
  move-corresponding tg_zglt036_excel[] to tg_zglt036[].

  loop at tg_zglt036 into data(wa_zglt036).

    data(lv_tabix) = sy-tabix.

    append initial line to lt_good_cell assigning field-symbol(<fs_good_cell>).
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'BSCHL'.
    <fs_good_cell>-value     = wa_zglt036-bschl.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'HKONT'.
    <fs_good_cell>-value = wa_zglt036-hkont.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'UMSKZ'.
    <fs_good_cell>-value = wa_zglt036-umskz.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'KOSTL'.
    <fs_good_cell>-value = wa_zglt036-kostl.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'PRCTR'.
    <fs_good_cell>-value = wa_zglt036-prctr.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'AUFNR'.
    <fs_good_cell>-value = wa_zglt036-aufnr.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'VORNR'.
    <fs_good_cell>-value = wa_zglt036-vornr.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'MATNR'.
    <fs_good_cell>-value = wa_zglt036-matnr.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'QUANTITY'.
    <fs_good_cell>-value = wa_zglt036-quantity.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'BASE_UOM'.
    <fs_good_cell>-value = wa_zglt036-base_uom.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'ANBWA'.
    <fs_good_cell>-value = wa_zglt036-anbwa.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'BEWAR'.
    <fs_good_cell>-value = wa_zglt036-bewar.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'VLR_MOEDA_DOC'.
    <fs_good_cell>-value = wa_zglt036-vlr_moeda_doc.
    condense <fs_good_cell>-value no-gaps.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'VLR_MOEDA_INT'.
    <fs_good_cell>-value = wa_zglt036-vlr_moeda_int.
    condense <fs_good_cell>-value no-gaps.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'VLR_MOEDA_FORTE'.
    <fs_good_cell>-value = wa_zglt036-vlr_moeda_forte.
    condense <fs_good_cell>-value no-gaps.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'VLR_MOEDA_GRUPO'.
    <fs_good_cell>-value = wa_zglt036-vlr_moeda_grupo.
    condense <fs_good_cell>-value no-gaps.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'MATNR_FI'.
    <fs_good_cell>-value = wa_zglt036-matnr_fi.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'ZUONR'.
    <fs_good_cell>-value = wa_zglt036-zuonr.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'VBELN'.
    <fs_good_cell>-value = wa_zglt036-vbeln.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'DIVISAO'.
    <fs_good_cell>-value = wa_zglt036-divisao.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'WERKS'.
    <fs_good_cell>-value = wa_zglt036-werks.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'VBUND'.
    <fs_good_cell>-value = wa_zglt036-vbund.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'SGTXT'.
    <fs_good_cell>-value = wa_zglt036-sgtxt.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'DT_VCT'.
    <fs_good_cell>-value = wa_zglt036-dt_vct.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'HBKID'.
    <fs_good_cell>-value = wa_zglt036-hbkid.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'BVTYP'.
    <fs_good_cell>-value = wa_zglt036-bvtyp.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'ZLSCH'.
    <fs_good_cell>-value = wa_zglt036-zlsch.

    append initial line to lt_good_cell assigning <fs_good_cell>.
    <fs_good_cell>-row_id = lv_tabix.
    <fs_good_cell>-tabix = lv_tabix.
    <fs_good_cell>-fieldname = 'ZLSPR'.
    <fs_good_cell>-value = wa_zglt036-zlspr.
  endloop.

  lt_mod_cells[] = lt_good_cell[].

  lo_er_data_changed->mt_good_cells   = lt_good_cell[].
  lo_er_data_changed->mt_fieldcatalog = tg_fieldcatalog[].
  lo_er_data_changed->mt_mod_cells    = lt_mod_cells[].

  get reference of tg_zglt036 into lo_er_data_changed->mp_mod_rows. "Preenche o atruibuto MP_MOD_ROWS

  call method gr_events->on_data_changed
    exporting
      er_data_changed = lo_er_data_changed.



  call method gr_events->on_data_changed_finished. "Atualiza os valores de debito e crédito no ALV e outros...

  tg_obj = corresponding #( tg_zglt036 mapping txt50 = descr ). "tg_obj usado no popup do objeto de custo
  delete tg_obj where kostl is initial
                  and prctr is initial.

  tl_obj_aux[] = tg_obj[].

*Inicio Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266
  call method gr_events->on_data_changed_pop
    exporting
      er_data_changed = lo_er_data_changed.
*Fim Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266

  clear tg_excel[]. "Dentro do método on_data_changed é verificado se esta tabela tem dado para executar ou não update de valores.

** End of FF  02.02.2023
endform.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_INFO_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_popup_info_layout .

  data: cancelled.

  call function 'POPUP_DISPLAY_TEXT'
    exporting
*     LANGUAGE       = SY-LANGU
      popup_title    = 'LAYOUT DO ARQUIVO PARA IMPORTAR'
      start_column   = 10
      start_row      = 3
      text_object    = 'ZPOPUP_ZGL016' "Criado na SE61
      help_modal     = 'X'
    importing
      cancelled      = cancelled
    exceptions
      text_not_found = 1
      others         = 2.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

endform.
