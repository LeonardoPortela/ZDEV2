function zmm_out_zmm0141.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(T_T001) TYPE  IT001
*"     REFERENCE(T_MARA) TYPE  EXPO_MARA_T OPTIONAL
*"     REFERENCE(T_EKKO) TYPE  ME_EKKO OPTIONAL
*"     REFERENCE(T_RESWK) TYPE  CVP_TT_T001W OPTIONAL
*"     REFERENCE(T_WERKS) TYPE  CVP_TT_T001W OPTIONAL
*"     REFERENCE(T_BUDAT_L) TYPE  EKBE-BUDAT
*"     REFERENCE(T_BUDAT_H) TYPE  EKBE-BUDAT
*"     REFERENCE(I_SIT_CARGA2) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_OUT_ZMMM0141) TYPE  ZDE_OUT_ZMM0141_T
*"----------------------------------------------------------------------
  data: it_saida_aux type  zde_out_zmm0141_t,
        wa_saida_aux type  zde_out_zmm0141,

        it_dados     type  zde_out_zmm0141_t,

        it_ekbe      type table of ekbe,
        wa_ekbe      type  ekbe,
        it_ekbe_sg   type table of ekbe,
        wa_ekbe_sg   type  ekbe,
        it_makt      type table of makt,
        wa_makt      type makt,
        wa_saida     type zde_out_zmm0141.



  data: v_refkey type j_1bnflin-refkey.

  data: v_xcf          type t001w-name1,
        v_xcr          type t001w-name1,
        v_xdtsaida     type ekbe-budat,
        v_xdtmovs      type ekbe-cpudt,
        v_xhrmovs      type ekbe-cputm,
        v_xdocmatsd    type ekbe-belnr,
        v_xrem         type ekbe-vbeln_st,
        v_xqtsd        type ekbe-menge,
        v_xanosd       type ekbe-gjahr,
        v_xdtentr      type ekbe-budat,
        v_xdtmove      type ekbe-cpudt,
        v_xhrmove      type ekbe-cputm,
        v_xdocmatet    type ekbe-belnr,
        v_xpesochegada type zsdt0001-peso_liq.


  ranges: r_bukrs for  t001-bukrs,
          r_matnr for ekbe-matnr,
          r_ebeln for ekbe-ebeln,
          r_reswk for ekko-reswk,
          r_werks for t001w-werks,
          r_ano   for ekbe-gjahr.

  clear: r_bukrs[],
         r_matnr[],
         r_ebeln[],
         r_reswk[],
         r_werks[],
         r_ano[].

  loop at t_t001 into data(_t001).
    r_bukrs-sign   = 'I'.
    r_bukrs-option = 'EQ'.
    r_bukrs-low    = _t001-bukrs.
    append r_bukrs.
  endloop.

  loop at t_mara into data(_mara).
    r_matnr-sign   = 'I'.
    r_matnr-option = 'EQ'.
    r_matnr-low    = _mara-matnr.
    append r_matnr.
  endloop.

  loop at t_ekko into data(_ekko).
    r_ebeln-sign     = 'I'.
    r_ebeln-option   = 'EQ'.
    r_ebeln-low      = _ekko-ebeln.
    append r_ebeln.
  endloop.

  loop at t_reswk into data(_reswk).
    r_reswk-sign     = 'I'.
    r_reswk-option   = 'EQ'.
    r_reswk-low      = _reswk-werks.
    append r_reswk.
  endloop.

  loop at t_werks into data(_werks).
    r_werks-sign     = 'I'.
    r_werks-option   = 'EQ'.
    r_werks-low      = _werks-werks.
    append r_werks.
  endloop.

  r_ano-sign   = 'I'.
  r_ano-option = 'BT'.
  r_ano-low    = t_budat_l+0(4).
  r_ano-high   = t_budat_h+0(4).
  append r_ano.


  if  t_mara is not initial and t_ekko is initial .

    select  *  from ekbe  into table it_ekbe
    where gjahr in r_ano
    and   bwart in ('862', '864', '861', '863', 'ZX3')
    and   budat between t_budat_l and t_budat_h
    and   matnr in r_matnr.

  elseif  t_mara  is initial   and t_ekko is not initial .

    select  *  from ekbe  into table it_ekbe
     where gjahr in r_ano
     and   bwart in ('862', '864', '861', '863', 'ZX3')
     and   budat between t_budat_l and t_budat_h
     and   ebeln in r_ebeln.

  elseif t_mara is not initial and t_ekko is not initial.

    select  *  from ekbe    into table it_ekbe
        where gjahr in r_ano
        and   bwart in ('862', '864', '861', '863', 'ZX3')
        and   budat between t_budat_l and t_budat_h
        and   matnr in r_matnr
        and   ebeln in r_ebeln.
  else.

    select  *  from ekbe   into table it_ekbe
    where gjahr in r_ano
    and   bwart in ('862', '864', '861', '863', 'ZX3')
    and   budat between t_budat_l and t_budat_h.

  endif.

  if it_ekbe is not initial.

    select *   from makt into table it_makt
     for all entries in it_ekbe
    where matnr eq  it_ekbe-matnr
    and   spras	eq 'PT'.

  endif.

  move it_ekbe to it_ekbe_sg.

  delete it_ekbe where ( bwart = '861' or  bwart = '863' )." OR BWART = 'ZX3' ). Ajuste BUG SOLTO 154660 / IR190834.

  delete it_ekbe_sg where ( bwart = '862' or  bwart = '864' or bwart = 'ZX3' ). "Ajuste BUG SOLTO 154660 / IR190834.


  loop at it_ekbe into wa_ekbe where bwart = '862' or bwart = '864' or bwart = 'ZX3'. "Ajuste BUG SOLTO 154660 / IR190834.

    v_xdtsaida    =  wa_ekbe-budat.
    v_xdtmovs     =  wa_ekbe-cpudt.
    v_xhrmovs     =  wa_ekbe-cputm.
    v_xdocmatsd   =  wa_ekbe-belnr.
    v_xrem        =  wa_ekbe-vbeln_st.
    v_xqtsd       =  wa_ekbe-menge.
    v_xanosd      =  wa_ekbe-gjahr.

    if wa_ekbe-vbeln_st is not initial.

      select single *  from zsdt0001
        into @data(wa_zsdt0001)
      where  doc_rem eq @wa_ekbe-vbeln_st
        and  tp_movimento = 'S'.

      if wa_zsdt0001-doc_rem is not initial.
        wa_saida_aux-nr_romaneio = wa_zsdt0001-nr_romaneio.
        wa_saida_aux-placa_cav   = wa_zsdt0001-placa_cav.
        v_xpesochegada           = wa_zsdt0001-peso_liq.
      endif.
    endif.

    clear: v_refkey.
    v_refkey = |{ v_xdocmatsd }{ v_xanosd }|.

    condense v_refkey.

    select single  *  from j_1bnflin
      into @data(wa_j_1bnflin)
    where  refkey eq @v_refkey.

    if wa_j_1bnflin-docnum is not initial.
      select single  nfenum
        from j_1bnfdoc
        into wa_saida_aux-nfenum
      where docnum  eq wa_j_1bnflin-docnum.
    endif.


    if _reswk is not initial.
      select single  *  from ekko
        into @data(wa_ekko)
      where ebeln eq @wa_ekbe-ebeln
        and bukrs in @r_bukrs
        and reswk in @r_reswk.
    else.
      select single * from ekko
        into wa_ekko
      where ebeln eq wa_ekbe-ebeln
        and bukrs in r_bukrs.
    endif.

    if wa_ekko is not initial.
      wa_saida_aux-bukrs = wa_ekko-bukrs.
      wa_saida_aux-reswk = wa_ekko-reswk.

      select single  *
        from t001w  into @data(wa_t001w)
      where werks eq @wa_ekko-reswk.

      wa_saida_aux-xcf = wa_t001w-name1.

      select single *  from t001 into @data(wa_t001)
       where bukrs eq @wa_ekko-bukrs.

      wa_saida_aux-butxt  = wa_t001-butxt.
    endif.


    if _werks is not initial.
      select single  *  from ekpo into @data(wa_ekpo)
      where ebeln	eq @wa_ekbe-ebeln
       and  ebelp	eq @wa_ekbe-ebelp
        and bukrs in @r_bukrs
        and werks in @r_werks.
    else.
      select single *
        from ekpo into wa_ekpo
      where ebeln	eq wa_ekbe-ebeln
       and  ebelp	eq wa_ekbe-ebelp
        and bukrs in r_bukrs.
    endif.

    if wa_ekpo is not initial.
      wa_saida_aux-werks = wa_ekpo-werks.

      select single *
        from t001w  into @data(wa_t001w_ek)
      where  werks eq @wa_ekpo-werks.

      wa_saida_aux-xcr = wa_t001w_ek-name1.
    endif.

    if wa_ekpo is not initial.

      read table it_makt into wa_makt with key matnr = wa_ekbe-matnr.
      if sy-subrc = 0.
        wa_saida_aux-maktx =  wa_makt-maktx.
      endif.

      wa_saida_aux-ebeln     =  wa_ekbe-ebeln.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_ekbe-matnr
        importing
          output = wa_saida_aux-matnr.

      wa_saida_aux-xdtsaida     = v_xdtsaida .
      wa_saida_aux-xdocmatsd    = v_xdocmatsd.
      wa_saida_aux-xqtsd        = v_xqtsd.
      wa_saida_aux-xdtentr      = v_xdtentr.
      wa_saida_aux-xdocmatet    = v_xdocmatet.
      wa_saida_aux-xpesochegada = v_xpesochegada.
      wa_saida_aux-vbeln_st     = v_xrem.

      if wa_ekbe-bwart = '862' or wa_ekbe-bwart = 'ZX3'.
        select single * from mseg into @data(wa_mseg)
          where smbln eq @wa_ekbe-belnr
          and   mjahr eq @wa_ekbe-gjahr
          and   bwart eq '864'.

        if wa_mseg is not initial.
          select single  *  from ekbe into @data(wa_ekbe_aux)
            where belnr eq @wa_mseg-smbln.
        endif.
      else.

        select single * from mseg into wa_mseg
          where mblnr eq wa_ekbe-belnr
          and   mjahr eq wa_ekbe-gjahr
          and   bwart eq '864'.

        if wa_mseg is not initial.
          select single  * from ekbe into wa_ekbe_aux
            where belnr eq wa_mseg-mblnr
            and   gjahr eq wa_mseg-mjahr
            and   bwart eq '864'.
        endif.
      endif.

      wa_saida_aux-bwart     = wa_ekbe-bwart.

      if wa_ekbe_aux-belnr is initial.
        append wa_saida_aux to it_saida_aux.
      endif.
    endif.

    clear:  wa_saida,     wa_ekbe,
            wa_ekko,      wa_ekpo,
            wa_t001,      wa_t001w,
            wa_t001w_ek,  wa_zsdt0001,
            wa_j_1bnflin, wa_makt,
            wa_saida_aux,
            wa_mseg, wa_ekbe_aux.

    clear: v_xcf,       v_xcr,
           v_xdtsaida,  v_xdtmovs,
           v_xhrmovs,   v_xdocmatsd,
           v_xrem,      v_xqtsd,
           v_xanosd,    v_xdtentr,
           v_xdtmove,   v_xhrmove,
           v_xdocmatet, v_xpesochegada.


  endloop.

  "Inicio ajuste BUG SOLTO 154660 / IR190834.
  free: it_dados.
  sort it_saida_aux by bwart.
  it_dados = it_saida_aux.
  delete it_dados where bwart ne 'ZX3'.
  delete it_saida_aux where bwart eq 'ZX3'.
  "Fim ajuste BUG SOLTO 154660 / IR190834.

  loop at it_saida_aux into wa_saida_aux.

    read table it_ekbe_sg into wa_ekbe_sg with key vbeln_st = wa_saida_aux-vbeln_st.
    "IF SY-SUBRC = 0.
    if ( wa_ekbe_sg-bwart = '861'  or  wa_ekbe_sg-bwart = '863' )."  OR WA_EKBE_SG-BWART = 'ZX3')."Ajuste BUG SOLTO 154660 / IR190834.
      v_xdtentr   = wa_ekbe_sg-budat.
      v_xdtmove   = wa_ekbe_sg-cpudt.
      v_xhrmove   = wa_ekbe_sg-cputm.
      v_xdocmatet = wa_ekbe_sg-belnr.

      if v_xdtentr = '00000000'.
        call function 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
          exporting
            i_date_from = wa_saida_aux-xdtsaida
            i_date_to   = sy-datum
          importing
            e_days      = wa_saida-xdias.
      else.
        call function 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
          exporting
            i_date_from = wa_saida_aux-xdtsaida
            i_date_to   = v_xdtentr
          importing
            e_days      = wa_saida-xdias.
      endif.

      wa_saida-bukrs        =  wa_saida_aux-bukrs.
      wa_saida-butxt        =  wa_saida_aux-butxt.
      wa_saida-reswk        =  wa_saida_aux-reswk.
      wa_saida-xcf          =  wa_saida_aux-xcf.
      wa_saida-werks        =  wa_saida_aux-werks.
      wa_saida-xcr          =  wa_saida_aux-xcr.
      wa_saida-ebeln        =  wa_saida_aux-ebeln.
      wa_saida-vbeln_st     =  wa_saida_aux-vbeln_st.
      wa_saida-nr_romaneio  =  wa_saida_aux-nr_romaneio.
      wa_saida-placa_cav    =  wa_saida_aux-placa_cav.
      wa_saida-matnr        =  wa_saida_aux-matnr.
      wa_saida-maktx        =  wa_saida_aux-maktx.
      wa_saida-nfenum       =  wa_saida_aux-nfenum.
      wa_saida-xdtsaida     =  wa_saida_aux-xdtsaida.
      wa_saida-xdocmatsd    =  wa_saida_aux-xdocmatsd.
      wa_saida-xqtsd        =  wa_saida_aux-xqtsd.
      wa_saida-xdtentr      =  v_xdtentr.
      wa_saida-xdocmatet    =  v_xdocmatet.
      wa_saida-xpesochegada =  wa_saida_aux-xpesochegada.

      if wa_ekbe-bwart = '861'.
        select single * from mseg into wa_mseg
          where smbln eq wa_ekbe_sg-belnr
          and   mjahr eq wa_ekbe_sg-gjahr
          and   bwart eq '863'.

        if wa_mseg is not initial.
          select single  *  from ekbe into wa_ekbe_aux
            where belnr eq wa_mseg-smbln.
        endif.
      else.

        select single * from mseg into wa_mseg
          where mblnr eq wa_ekbe_sg-belnr
          and   mjahr eq wa_ekbe_sg-gjahr
          and   bwart eq '863'.

        if wa_mseg is not initial.
          select single  * from ekbe into wa_ekbe_aux
            where belnr eq wa_mseg-mblnr
            and   gjahr eq wa_mseg-mjahr
            and   bwart eq '863'.
        endif.
      endif.

    else.

      if v_xdtentr = '00000000'.
        call function 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
          exporting
            i_date_from = wa_saida_aux-xdtsaida
            i_date_to   = sy-datum
          importing
            e_days      = wa_saida-xdias.
      else.
        call function 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
          exporting
            i_date_from = wa_saida_aux-xdtsaida
            i_date_to   = v_xdtentr
          importing
            e_days      = wa_saida-xdias.
      endif.

      wa_saida-bukrs        =  wa_saida_aux-bukrs.
      wa_saida-butxt        =  wa_saida_aux-butxt.
      wa_saida-reswk        =  wa_saida_aux-reswk.
      wa_saida-xcf          =  wa_saida_aux-xcf.
      wa_saida-werks        =  wa_saida_aux-werks.
      wa_saida-xcr          =  wa_saida_aux-xcr.
      wa_saida-ebeln        =  wa_saida_aux-ebeln.
      wa_saida-vbeln_st     =  wa_saida_aux-vbeln_st.
      wa_saida-nr_romaneio  =  wa_saida_aux-nr_romaneio.
      wa_saida-placa_cav    =  wa_saida_aux-placa_cav.
      wa_saida-matnr        =  wa_saida_aux-matnr.
      wa_saida-maktx        =  wa_saida_aux-maktx.
      wa_saida-nfenum       =  wa_saida_aux-nfenum.
      wa_saida-xdtsaida     =  wa_saida_aux-xdtsaida.
      wa_saida-xdocmatsd    =  wa_saida_aux-xdocmatsd.
      wa_saida-xqtsd        =  wa_saida_aux-xqtsd.

    endif.
    "    ENDIF.


    select single  *  from zmmt0095
      into   @data(wa_zmmt0095)
      where bukrs eq @wa_saida_aux-bukrs
      and   werks eq @wa_saida_aux-werks.

    if  wa_zmmt0095 is initial.

      select single  *  from zmmt0095
        into  wa_zmmt0095
        where bukrs eq wa_saida_aux-bukrs.

      if wa_zmmt0095-werks is initial and
         wa_zmmt0095-bukrs =  wa_saida_aux-bukrs.

        if  wa_saida-xdias = 0 and wa_saida-xdtentr is not initial.
          wa_saida-status = icon_checked.
          wa_saida-xdias = ''.

        elseif ( wa_zmmt0095-atage >= wa_saida-xdias and wa_saida-xdtentr is initial and
                wa_saida-xdocmatet is initial     and wa_saida-xpesochegada is initial ) or
               (  wa_saida-xdias = 0 and wa_saida-xdtentr is  initial ).

          wa_saida-status = icon_transport.

        elseif wa_zmmt0095-atage < wa_saida-xdias and wa_saida-xdtentr is initial and
               wa_saida-xdocmatet is initial      and  wa_saida-xpesochegada is initial.
          wa_saida-status = icon_led_red.
        else.
          wa_saida-status = icon_checked.
        endif.

      endif.

    else.

      if wa_zmmt0095-werks = wa_saida_aux-werks and
         wa_zmmt0095-bukrs = wa_saida_aux-bukrs.

        if  wa_saida-xdias = 0 and wa_saida-xdtentr is not initial.
          wa_saida-status = icon_checked.
          wa_saida-xdias = ''.

        elseif (  wa_zmmt0095-atage >= wa_saida-xdias and wa_saida-xdtentr is initial and
                  wa_saida-xdocmatet is initial       and wa_saida-xpesochegada is initial ) or
               (  wa_saida-xdias = 0 and wa_saida-xdtentr is  initial ).

          wa_saida-status = icon_transport.

        elseif  wa_zmmt0095-atage < wa_saida-xdias and wa_saida-xdtentr is initial and
                wa_saida-xdocmatet is initial      and wa_saida-xpesochegada is initial.
          wa_saida-status = icon_led_red.
        else.
          wa_saida-status = icon_checked.
        endif.
      endif.
    endif.

    "Inicio ajuste BUG SOLTO 154660 / IR190834.
    read table it_dados into data(ws_dados) with key ebeln = wa_saida-ebeln
                                                  vbeln_st = wa_saida-vbeln_st.
    if sy-subrc eq 0.
      wa_saida-xdtentr = ws_dados-xdtsaida.
      wa_saida-xpesochegada = ws_dados-xpesochegada.
      wa_saida-xdias = ''.
      wa_saida-xdocmatet = ws_dados-xdocmatsd.
      wa_saida-status = icon_checked.
    endif.

    wa_saida-bwart         =  wa_saida_aux-bwart.
    "Fim ajuste BUG SOLTO 154660 / IR190834.

    if i_sit_carga2 = '1'. "PENDENTE
      if v_xdtentr = '00000000' and wa_saida-bukrs is not initial.
        if wa_ekbe_aux-belnr is initial.
          append wa_saida to e_out_zmmm0141.
        endif.
      endif.
    elseif i_sit_carga2 = '2'. "CONCLUIDA
      if v_xdtentr <> '00000000' and wa_saida-bukrs is not initial.
        if wa_ekbe_aux-belnr is initial.
          append wa_saida to e_out_zmmm0141.
        endif.
      endif.
    elseif i_sit_carga2 = '3'. "TODAS
      if  wa_saida-bukrs is not initial.
        if wa_ekbe_aux-belnr is initial.
          append wa_saida to e_out_zmmm0141.
        endif.
      endif.
    endif.


    clear:  wa_saida,     wa_ekbe_sg,
            wa_ekko,      wa_ekpo,
            wa_t001,      wa_t001w,
            wa_t001w_ek,  wa_zsdt0001,
            wa_j_1bnflin, wa_makt,
            wa_saida_aux, wa_mseg,
            wa_zmmt0095, wa_ekbe_aux.

    clear: v_xcf,       v_xcr,
           v_xdtsaida,  v_xdtmovs,
           v_xhrmovs,   v_xdocmatsd,
           v_xrem,      v_xqtsd,
           v_xanosd,    v_xdtentr,
           v_xdtmove,   v_xhrmove,
           v_xdocmatet, v_xpesochegada.

  endloop.

endfunction.
