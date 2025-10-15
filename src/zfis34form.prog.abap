*&---------------------------------------------------------------------*
*&  Include           ZFIS34FORM
*&---------------------------------------------------------------------*

form monta_principal.

  clear: wa_zfit0138, wa_zib_log, wa_zib_err, wa_saida_prin, wa_t001w, wa_saida_item, xicone,
         xzib, mes, ano, dia_final, wa_aux_136, wa_aux_137, wa_zfit0136, wa_zfit0137, wa_zfit0138,
         wa_aux_138, wa_j_1bnflin_aux.

  free: it_saida_prin, it_saida_item, it_aux_136, it_aux_137, it_zfit0138, it_j_1bnflin, it_j_1bnfdoc,
        it_j_1bnfdoc_aux, it_j_1bnfdoc_aux, r_werks, it_j_1bnflin_aux, it_t001w, r_centro, r_cfop, r_werks.

  perform seleciona_dados.
  if it_zfit0138[] is initial and it_j_1bnflin[] is initial.
    message text-002 type 'I'.
    call screen 0100.
  else.
    perform agrupa_principal.
  endif.


endform.

form seleciona_dados.

  data: conta      type i,
        l_bukrs(7) type c,
        l_docnum   type j_1bnfdoc-docnum value is initial.


  write p_mes to mes.
  mes = |{ mes+3(2) }|.
  write p_ano to ano.
  ano = |{ ano+3(4) }|.
  write p_bukrs to l_bukrs.
  l_bukrs = |{ l_bukrs+3(4) }|.

  dia_aux = |{ ano }{ mes }01|.
  write dia_aux to dia_inicio.

  call function 'RP_LAST_DAY_OF_MONTHS'
    exporting
      day_in            = dia_inicio
    importing
      last_day_of_month = dia_final.



*************SELECIONA LISTA DE CFOP (PARÂMETROS)*********************

  select bukrs cfop tp_mercado from zfit0136
    into corresponding fields of table it_aux_136
    where cfop is not null
     and  bukrs in p_bukrs "Inicio USER STORY 112601 / CS2023000234 / AOENNING
    order by cfop.

  if it_aux_136 is not initial.
    loop at it_aux_136 into wa_aux_136.
      wa_aux_136-cfop = |{ wa_aux_136-cfop+0(4) }{ wa_aux_136-cfop+5(2) }|.
      r_cfop-sign = 'I'.
      r_cfop-option = 'EQ'.
      r_cfop-low = wa_aux_136-cfop.
      append r_cfop.
    endloop.
  endif.

*************SELECIONA LISTA DE CENTROS (PARÂMETROS)*********************

  select bukrs werks kostl from zfit0137
    into corresponding fields of table it_aux_137
    where werks is not null
     and  bukrs in p_bukrs "Inicio USER STORY 112601 / CS2023000234 / AOENNING
    order by werks.

  if it_aux_137 is not initial.
    loop at it_aux_137 into wa_aux_137.
      r_werks-sign = 'I'.
      r_werks-option = 'EQ'.
      r_werks-low = wa_aux_137-werks.
      append r_werks.
    endloop.
  endif.

  select werks name1 j_1bbranch
    from t001w
    into table it_t001w
    for all entries in r_werks
    where j_1bbranch eq r_werks-low. " IT_J_1BNFLIN-WERKS.

  if it_t001w is not initial.
    loop at it_t001w into wa_t001w.
      r_centro-sign = 'I'.
      r_centro-option = 'EQ'.
      r_centro-low = wa_t001w-werks.
      append r_centro.
    endloop.
  endif.

  select * from zfit0138
    into table it_zfit0138
    where bukrs in p_bukrs and "Inicio USER STORY 112601 / CS2023000234 / AOENNING
          werks in p_werks and
          monat in p_mes   and
          gjahr in p_ano.

  select docnum nftype docdat pstdat credat nftot nfenum series parid cretim
         crenam direct
     from j_1bnfdoc
     into corresponding fields of table it_j_1bnfdoc
     where bukrs  in p_bukrs    and  "Inicio USER STORY 112601 / CS2023000234 / AOENNING
           branch in p_werks    and
           pstdat ge dia_inicio and
           pstdat le dia_final  and
           doctyp ne 5          and
           cancel ne 'X'.

  if it_j_1bnfdoc[] is not initial.

    select docnum taxval
      from j_1bnfstx
      into corresponding fields of table it_j_1bnfstx
      for all entries in it_j_1bnfdoc
      where docnum eq it_j_1bnfdoc-docnum and
            taxtyp eq 'ICM1'.

*************SELECIONA LISTA DE GRUPO MERCADORIA (PARÂMETROS ZFIS43)*********************
*&--------------Inicio USER STORY 112601 / CS2023000234 / AOENNING
    select * from zfit0195
   into corresponding fields of table it_aux_195
   where bukrs in p_bukrs
   order by matkl bukrs.

    FREE: r_matkl[].
    if it_aux_195 is not initial.
      loop at it_aux_195 into wa_aux_195.
        r_matkl-sign = 'I'.
        r_matkl-option = 'EQ'.
        r_matkl-low = wa_aux_195-matkl.
        append r_matkl.
      endloop.
    endif.

*&--------------Fim USER STORY 112601 / CS2023000234 / AOENNING

    select docnum cfop werks itmnum matnr maktx refkey menge nfpri nfnet netwr
           from j_1bnflin
           into corresponding fields of table it_j_1bnflin
           for all entries in it_j_1bnfdoc
           where docnum   eq  it_j_1bnfdoc-docnum and
                 cfop     in  r_cfop              and
                 werks    in  r_centro            and
                 matkl    in  r_matkl.            "Inicio USER STORY 112601 / CS2023000234 / AOENNING

    sort it_j_1bnflin by werks docnum ascending.

  endif.

endform.

form agrupa_principal.

  clear r_werks. free  r_werks.

  data: chave_zib  type zib_contabil_chv-obj_key,
        total_nf   type j_1bnflin-nfnet,
        total_est  type j_1bnflin-nfnet,
        v_filial   type t001w-werks,
        xcount     type i,
        xcount2    type i,
        v_encerra  type i,
        v_mi_nf(8) type p decimals 4.

  if it_zfit0138[] is not initial.
    loop at it_zfit0138 into wa_zfit0138.

      perform busca_status using wa_zfit0138-obj_key.

      read table it_t001w     into wa_t001w     with key werks = wa_zfit0138-werks.


      wa_saida_prin-status        = xicone.
      wa_saida_prin-filial        = wa_zfit0138-werks.
      wa_saida_prin-nome_filial   = wa_t001w-name1.
      wa_saida_prin-centro_custo  = wa_zfit0138-kostl.
*---> 09/06/2023 - Migração S4 - JS
*      WA_SAIDA_PRIN-TOTAL_ME    = WA_ZFIT0138-VL_TOTNF_ME.
*      WA_SAIDA_PRIN-TOTAL_MI    = WA_ZFIT0138-VL_TOTNF_MI.
*      WA_SAIDA_PRIN-TOTAL_NF    = WA_ZFIT0138-VL_TOTNF.
*      WA_SAIDA_PRIN-P_ESTORNO   = WA_ZFIT0138-PERC_EST.
*      WA_SAIDA_PRIN-CRED_ICMS   = WA_ZFIT0138-VL_CR_ICMS.
*      WA_SAIDA_PRIN-V_ESTORNO   = WA_ZFIT0138-VL_EST_CRED.
      wa_saida_prin-total_me  = conv #( wa_zfit0138-vl_totnf_me ).
      wa_saida_prin-total_mi  = conv #( wa_zfit0138-vl_totnf_mi ).
      wa_saida_prin-total_nf  = conv #( wa_zfit0138-vl_totnf ).
      wa_saida_prin-p_estorno = conv #( wa_zfit0138-perc_est ).
      wa_saida_prin-cred_icms = conv #( wa_zfit0138-vl_cr_icms ).
      wa_saida_prin-v_estorno = conv #( wa_zfit0138-vl_est_cred ).
*<--- 09/06/2023 - Migração S4 - JS
      if xzib is not initial.
        if wa_zfit0138-estornar is not initial.
          wa_saida_prin-nr_doc_estorno = xzib.
        else.
          wa_saida_prin-nr_documento  = xzib.
        endif.
      else.
        if wa_zfit0138-estornar is not initial.
          wa_saida_prin-nr_doc_estorno = l_erro.
        else.
          wa_saida_prin-nr_documento  = l_erro.
        endif.
      endif.

      collect wa_saida_prin into it_saida_prin.

      clear: wa_zfit0138, wa_zib_log, wa_zib_err, wa_saida_prin, wa_t001w, xicone, xzib, l_erro.

    endloop.
  endif.

  if it_j_1bnflin[] is not initial.

    if it_saida_prin[] is not initial.

      loop at it_saida_prin into wa_saida_prin.
        r_werks-sign = 'I'.
        r_werks-option = 'EQ'.
        r_werks-low = wa_saida_prin-filial.
        append r_werks.
      endloop.

      loop at it_t001w into wa_t001w where j_1bbranch in r_werks.
        r_centro_aux-sign = 'I'.
        r_centro_aux-option = 'EQ'.
        r_centro_aux-low = wa_t001w-werks.
        append r_centro_aux.
      endloop.

      clear wa_saida_prin.
    endif.

*    IF R_CENTRO_AUX[] IS INITIAL.
*      R_CENTRO_AUX-SIGN = 'I'.
*      R_CENTRO_AUX-OPTION = 'EQ'.
*      R_CENTRO_AUX-LOW = 'XXXX'.
*      APPEND R_CENTRO_AUX.
*    ENDIF.

    clear: total_me, total_mi, total_en.

    append lines of it_j_1bnflin to it_j_1bnflin_aux.
    sort it_j_1bnflin_aux[] by werks docnum ascending.


    loop at it_j_1bnflin_aux into wa_j_1bnflin_aux.

*      CHECK WA_J_1BNFLIN_AUX-WERKS NOT IN R_CENTRO_AUX[].
      check not line_exists( r_centro_aux[ low = wa_j_1bnflin_aux-werks ] ).
      clear wa_saida_prin.


      wa_j_1bnflin_aux-cfop = |{ wa_j_1bnflin_aux-cfop+0(4) }/{ wa_j_1bnflin_aux-cfop+4(2) }|.

      read table it_t001w     into wa_t001w     with key werks  = wa_j_1bnflin_aux-werks.
      read table it_aux_136   into wa_zfit0136  with key cfop   = wa_j_1bnflin_aux-cfop.
      read table it_aux_137   into wa_zfit0137  with key werks  = wa_t001w-j_1bbranch.
      read table it_j_1bnfstx into wa_j_1bnfstx with key docnum = wa_j_1bnflin_aux-docnum.

      case wa_zfit0136-tp_mercado.
        when 'ME'.
          if wa_j_1bnflin_aux-nfnet is not initial.
            wa_saida_prin-total_me  = wa_j_1bnflin_aux-nfnet.
          else.
            wa_saida_prin-total_me  = wa_j_1bnflin_aux-netwr.
          endif.
        when 'MI'.
          if wa_j_1bnflin_aux-nfnet is not initial.
            wa_saida_prin-total_mi  = wa_j_1bnflin_aux-nfnet.
          else.
            wa_saida_prin-total_mi  = wa_j_1bnflin_aux-netwr.
          endif.
        when 'EN'.
          wa_saida_prin-cred_icms   = wa_j_1bnfstx-taxval.
*          IF WA_J_1BNFLIN_AUX-NFNET IS NOT INITIAL.
*            WA_SAIDA_PRIN-CRED_ICMS  = WA_J_1BNFLIN_AUX-NFNET.
*          ELSE.
*            WA_SAIDA_PRIN-CRED_ICMS  = WA_J_1BNFLIN_AUX-NETWR.
*          ENDIF.
      endcase.

      xicone = icon_activity.
      wa_saida_prin-status        = xicone.
      wa_saida_prin-filial        = wa_t001w-j_1bbranch.

      read table it_t001w into wa_t001w with key werks = wa_zfit0137-werks.

      wa_saida_prin-nome_filial   = wa_t001w-name1.
      wa_saida_prin-centro_custo  = wa_zfit0137-kostl.

      if xzib is not initial.
        wa_saida_prin-nr_documento  = xzib.
      else.
        wa_saida_prin-nr_documento = l_erro.
      endif.

      clear: wa_j_1bnflin_aux, wa_t001w, wa_zfit0136, wa_zfit0137, wa_j_1bnfstx.

      if wa_saida_prin is not initial.
        collect wa_saida_prin into it_saida_prin[].
      endif.

    endloop.

  endif.

  clear: wa_saida_prin, wa_t001w, wa_zfit0136, wa_zfit0137, wa_j_1bnfstx.
  sort it_saida_prin by filial ascending.

***********Soma dos campos NF, %ESTORNO, VALOR ESTORNO******************
  loop at it_saida_prin into wa_saida_prin.

    wa_saida_prin-total_nf  = wa_saida_prin-total_me + wa_saida_prin-total_mi.
    v_mi_nf                 = wa_saida_prin-total_mi / wa_saida_prin-total_nf.
    wa_saida_prin-p_estorno = v_mi_nf * 100.
    v_estorno               = wa_saida_prin-p_estorno * wa_saida_prin-cred_icms.

    if ( v_estorno is not initial ).
      wa_saida_prin-v_estorno = v_estorno / 100.
    endif.

    if ( wa_saida_prin-total_me is initial ) and ( wa_saida_prin-cred_icms is not initial ).
      wa_saida_prin-v_estorno = wa_saida_prin-cred_icms.
    endif.

    modify it_saida_prin from wa_saida_prin.

  endloop.

endform.

form busca_status using p_ojb_key.

  clear: wa_zib_log, wa_zib_err, l_erro, xicone, xzib.

  select single obj_key belnr from zib_contabil_chv
        into wa_zib_log
        where obj_key eq p_ojb_key. "<---------

  if wa_zib_log is not initial.

    xzib   = wa_zib_log-belnr.

    if wa_zfit0138-estornar is not initial.
      xicone = icon_activity.
    else.
      xicone = icon_checked.
    endif.

  else.

    select single obj_key message message_v1 from zib_contabil_err
      into wa_zib_err
      where obj_key eq p_ojb_key and
            id      ne 'RW' . "<---------

    if wa_zib_err is initial.

      select single obj_key message message_v1 from zib_contabil_err
      into wa_zib_err
      where obj_key eq p_ojb_key.

    endif.

    if wa_zib_err is not initial.
      l_erro  = wa_zib_err-message.
      xicone  = icon_incomplete.
    endif.

  endif.

  if xicone is initial.
    xicone = icon_activity.
  endif.

endform.



form seleciona_item using p_v_tpmerc p_v_filial.

  data: r_cfop      type range of j_1bnflin-cfop with header line.

  clear: wa_saida_item, it_saida_item, r_cfop, r_centro.
  free:  r_cfop, r_centro.


  loop at it_aux_136 into wa_aux_136 where tp_mercado eq p_v_tpmerc.
    wa_aux_136-cfop = |{ wa_aux_136-cfop+0(4) }{ wa_aux_136-cfop+5(2) }|.
    r_cfop-sign = 'I'.
    r_cfop-option = 'EQ'.
    r_cfop-low = wa_aux_136-cfop.
    append r_cfop.
  endloop.

  loop at it_t001w into wa_t001w where j_1bbranch eq p_v_filial.
    r_centro-sign = 'I'.
    r_centro-option = 'EQ'.
    r_centro-low = wa_t001w-werks.
    append r_centro.
  endloop.


  loop at it_j_1bnflin into wa_j_1bnflin where cfop in r_cfop and werks in r_centro.

    read table it_j_1bnfdoc into wa_j_1bnfdoc with key docnum = wa_j_1bnflin-docnum.
    read table it_t001w     into wa_t001w     with key werks  = wa_j_1bnflin-werks.


    wa_saida_item-docnum      = wa_j_1bnflin-docnum.
    wa_saida_item-nota_fiscal = |{ wa_j_1bnfdoc-nfenum }-{ wa_j_1bnfdoc-series }|.
    wa_saida_item-dt_lcto     = wa_j_1bnfdoc-pstdat.
    wa_saida_item-dt_dcto     = wa_j_1bnfdoc-docdat.
    wa_saida_item-filial      = wa_j_1bnflin-werks.
    wa_saida_item-filial_name = wa_t001w-name1.
    wa_saida_item-matnr       = wa_j_1bnflin-matnr.
    wa_saida_item-maktx       = wa_j_1bnflin-maktx.
    wa_saida_item-quantidade  = wa_j_1bnflin-menge.
    wa_saida_item-preco       = wa_j_1bnflin-nfpri.
    wa_saida_item-valor_nota  = wa_j_1bnflin-nfnet.
    wa_saida_item-cfop        = wa_j_1bnflin-cfop.

    append wa_saida_item to it_saida_item.
    clear wa_saida_item.  clear wa_t001w.  clear wa_j_1bnfdoc.  clear wa_j_1bnflin.


  endloop.

endform.      "SELECIONA_ITEM



form gerar_contabil.

  data: seqitem         type zib_contabil-seqitem,
        dia_fim_zib(10) type c,
        l_bschl         type i.



  loop at it_saida_gera into wa_saida_gera.

    clear: wa_zfit0138,  wa_zib_cont.
    free:  it_zib_cont_aux[], it_zib_cont[].

    "Verifica o documento, caso já tenha sido gerado com sucesso, bloquear nova chave.
    "Se documento foi gerado com erro e corrigido, gerar nova chave.
    perform valida_contabil using p_bukrs-low  wa_saida_gera-filial  mes  ano.

    "Retorno do form VALIDA_CONTABIL. Se estiver preenchido,
    "não prosseguir com a contabilização.
    if wa_zib_log is not initial and sy-ucomm eq 'GERAR' and wa_zfit0138-estornar is initial.

      clear wa_saida_gera.
      message text-011 type 'I'.
      exit.

    else.

      perform obtem_proximo.

      wa_zfit0138-bukrs       = p_bukrs-low.
      wa_zfit0138-werks       = wa_saida_gera-filial.
      wa_zfit0138-monat       = mes.
      wa_zfit0138-gjahr       = ano.
      wa_zfit0138-kostl       = wa_saida_gera-centro_custo.
      wa_zfit0138-vl_totnf_me = wa_saida_gera-total_me.
      wa_zfit0138-vl_totnf_mi = wa_saida_gera-total_mi.
      wa_zfit0138-vl_totnf    = wa_saida_gera-total_nf.
      wa_zfit0138-perc_est    = wa_saida_gera-p_estorno.
      wa_zfit0138-vl_cr_icms  = wa_saida_gera-cred_icms.
      wa_zfit0138-vl_est_cred = wa_saida_gera-v_estorno.
      wa_zfit0138-obj_key     = |ZFIS44{ v_snum }{ ano }|.
      wa_zfit0138-usnam       = sy-uname.
      wa_zfit0138-data_atual  = sy-datum.
      wa_zfit0138-hora_atual  = sy-uzeit.

      if sy-ucomm eq 'ESTORNAR'.
        wa_zfit0138-estornar = 'X'.
      else.
        wa_zfit0138-estornar = ''.
      endif.

      modify zfit0138 from wa_zfit0138.


      select obj_key seqitem from zib_contabil
        into corresponding fields of table it_zib_cont_aux
        where obj_key eq wa_zfit0138-obj_key.

      describe table it_zib_cont_aux lines count_item.

      read table it_zib_cont_aux into wa_zib_cont_aux index count_item.

      if wa_zib_cont_aux is not initial.
        seqitem = wa_zib_cont_aux-seqitem + 1.
      else.
        seqitem = 000001.
      endif.

      dia_fim_zib = |{ dia_final+6(2) }.{ dia_final+4(2) }.{ dia_final+0(4) }|.

      if sy-ucomm = 'GERAR'.
        l_bschl = 40.
        wa_zib_cont-kostl         = wa_zfit0138-kostl.
      elseif sy-ucomm = 'ESTORNAR'.
        l_bschl = 50.
        wa_zib_cont-kostl         = wa_zfit0138-kostl.
      endif.
********ZIB_CONTÁBIL DÉBITO***********************************
      wa_zib_cont-obj_key       = wa_zfit0138-obj_key.
      wa_zib_cont-seqitem       = seqitem.
      wa_zib_cont-bschl         = l_bschl.
      wa_zib_cont-gsber         = wa_zfit0138-werks.
      wa_zib_cont-bukrs         = wa_zfit0138-bukrs.
      wa_zib_cont-interface     = 0.
      wa_zib_cont-bktxt         = 'Ap.Estorno Energia'.
      wa_zib_cont-bldat         = dia_fim_zib.
      wa_zib_cont-budat         = dia_fim_zib.
      wa_zib_cont-gjahr         = wa_zfit0138-gjahr.
      wa_zib_cont-monat         = wa_zfit0138-monat.
      wa_zib_cont-blart         = 'LM'.

      wa_zib_cont-hkont         = 412023.

      select single *
        from zfit0151 into @data(_wl_0151)
       where werks eq @wa_saida_gera-filial.

      if ( sy-subrc eq 0 ) and
         ( wa_saida_gera-filial is not initial ) and
         ( _wl_0151-saknr is not initial ).
        wa_zib_cont-hkont = _wl_0151-saknr.
      endif.

      wa_zib_cont-wrbtr         = wa_zfit0138-vl_est_cred.
      wa_zib_cont-waers         = 'BRL'.
      wa_zib_cont-sgtxt         = 'Estorno ICMS s/energia elétrica propor MI'.
      wa_zib_cont-waers_i       = 'BRL'.
      wa_zib_cont-dmbtr         = wa_zfit0138-vl_est_cred.
      wa_zib_cont-rg_atualizado = 'N'.

      modify zib_contabil from wa_zib_cont.
      clear wa_zib_cont_aux.  clear count_item.

********ZIB_CONTÁBIL CRÉDITO***********************************

      select obj_key seqitem from zib_contabil
        into corresponding fields of table it_zib_cont
        where obj_key eq wa_zfit0138-obj_key.

      describe table it_zib_cont lines count_item.
      read table it_zib_cont into wa_zib_cont index count_item.

      if wa_zib_cont is not initial.
        seqitem = wa_zib_cont-seqitem + 1.
      else.
        seqitem = 000001.
      endif.

      if sy-ucomm = 'GERAR'.
        l_bschl = 50.
      elseif sy-ucomm = 'ESTORNAR'.
        l_bschl = 40.
        wa_zib_cont-kostl         = wa_zfit0138-kostl.
      endif.

      wa_zib_cont-obj_key       = wa_zfit0138-obj_key.
      wa_zib_cont-seqitem       = seqitem.
      wa_zib_cont-bschl         = l_bschl.
      wa_zib_cont-gsber         = wa_zfit0138-werks.
      wa_zib_cont-bukrs         = wa_zfit0138-bukrs.
      wa_zib_cont-interface     = 0.
      wa_zib_cont-bktxt         = 'Ap.Estorno Energia'.
      wa_zib_cont-bldat         = dia_fim_zib.
      wa_zib_cont-budat         = dia_fim_zib.
      wa_zib_cont-gjahr         = wa_zfit0138-gjahr.
      wa_zib_cont-monat         = wa_zfit0138-monat.
      wa_zib_cont-blart         = 'LM'.
      wa_zib_cont-hkont         = 113202.
      wa_zib_cont-wrbtr         = wa_zfit0138-vl_est_cred.
      wa_zib_cont-waers         = 'BRL'.
      wa_zib_cont-sgtxt         = 'Estorno ICMS s/energia elétrica propor MI'.
      wa_zib_cont-waers_i       = 'BRL'.
      wa_zib_cont-dmbtr         = wa_zfit0138-vl_est_cred.
      wa_zib_cont-rg_atualizado = 'N'.

      modify zib_contabil from wa_zib_cont.
      clear: wa_zib_cont_aux, count_item.


      if sy-ucomm eq 'GERAR'.
        message text-008 type 'S'.
      elseif sy-ucomm eq 'ESTORNAR'.
*
      endif.
    endif.
  endloop.
endform.      "GERAR_CONTABIL

form obtem_proximo.
  data: vl_number(10) type c.

  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = '01'
      object                  = 'ZFI_44'
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
    write vl_number to v_snum.
  endif.
endform.


form valida_contabil using pa_bukrs
                           pa_werks
                           pa_monat
                           pa_gjahr.

  clear: wa_zib_log, wa_zfit0138.

  select single * from zfit0138
    into wa_zfit0138
    where bukrs eq pa_bukrs and
          werks eq pa_werks and
          monat eq pa_monat and
          gjahr eq pa_gjahr.

  if wa_zfit0138 is initial and sy-ucomm eq 'ESTORNAR'.
    message text-013 type 'I'.
    exit.
  endif.

*    IF WA_ZFIT0138 IS INITIAL AND SY-UCOMM EQ 'ATUALIZAR'.
*      EXIT.
*    ENDIF.

  if wa_zfit0138 is not initial.

    select single obj_key
      from zib_contabil_chv
      into corresponding fields of wa_zib_log
      where obj_key eq wa_zfit0138-obj_key.

  endif.


endform.
