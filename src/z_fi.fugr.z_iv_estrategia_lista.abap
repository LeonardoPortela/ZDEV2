function z_iv_estrategia_lista.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) LIKE  SY-UNAME
*"     VALUE(LOTE) TYPE  ZFIT0036-LOTE OPTIONAL
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_LOTES STRUCTURE  ZIV_LOTES_IMP
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_IMP OPTIONAL
*"      T_DOCS STRUCTURE  ZIV_DOCS_IMP OPTIONAL
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  type-pools: icon.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
  types: begin of ty_cadlote,
           empresa(30) type c,
           lote(50)    type c,
           usuario(20) type c,
           total       type zfit0036-vlr_pgto,
           dep_resp(2),
         end of ty_cadlote,


         begin of ty_estra ,
           bukrs     type zinv_lotes_aprov-bukrs,
           lote      type zinv_lotes_aprov-lote,
           valor_de  type zinv_aprovador-valor_de,
           valor_ate type zinv_aprovador-valor_ate,
           aprovador type zinv_aprovador-aprovador,
           nivel     type zinv_aprovador-nivel,
           estado(4),
           opcoes(4),
         end of ty_estra,

         begin of ty_docs ,
           lote       type zimp_cad_lote-lote,
           obj_key    type zfit0036-obj_key,
           bukrs      type zfit0036-bukrs,
           invoice    type zfit0036-invoice,
           nro_sol_ov type zsdt0051-nro_sol_ov,
           navio      type zfit0036-navio,
           vlr_pgto   type zfit0036-vlr_pgto,
           hbkid      type zfit0036-hbkid,
           name1      type lfa1-name1,
           observacao type zfit0036-observacao,
         end of ty_docs,

         begin of ty_zimp_cad_lote,
           lote        type zimp_cad_lote-lote,
           bukrs       type zimp_cad_lote-bukrs,
           descr_lote  type zimp_cad_lote-descr_lote,
           status_lote type zimp_cad_lote-status_lote,
           usnam       type zimp_cad_lote-usnam,
           dep_resp    type zimp_cad_lote-dep_resp,
           dt_venc     type zimp_cad_lote-dt_venc,
         end of ty_zimp_cad_lote,

         begin of ty_zfit0036,
           lote        type zfit0036-lote,
           bukrs       type zfit0036-bukrs,
           status_lote type zfit0036-status,
           dt_pgto     type zfit0036-dt_pgto,
           moeda_pgto  type zfit0036-moeda_pgto,
           vlr_pgto    type zfit0036-vlr_pgto,
           status      type zfit0036-status,
           obj_key     type zfit0036-obj_key,
           invoice     type zfit0036-invoice,
           navio       type zfit0036-navio,
           hbkid       type zfit0036-hbkid,
           observacao  type zfit0036-observacao,
           usuario     type zfit0036-usuario,
           opera       type zfit0036-operacao,
           matnr       type zfit0036-matnr,
           lifnr       type lfa1-lifnr,
           ds_operacao type zfit0043-ds_operacao,
           belnr       type bsik-belnr,
         end of ty_zfit0036,

         begin of ty_zib_contabil,
           obj_key type zib_contabil-obj_key,
           hkont   type zib_contabil-hkont,
         end of ty_zib_contabil,

         begin of ty_lfa1,
           lifnr type lfa1-lifnr,
           name1 type lfa1-name1,
         end of ty_lfa1,

         begin of ty_bsik,
           bukrs type bsik-bukrs,
           lifnr type bsik-lifnr,
           belnr type bsik-belnr,
           dmbtr type bsik-dmbtr,
           dmbe2 type bsik-dmbe2,
           budat type bsik-budat,
           buzei type bsik-buzei,
           gsber type bsik-gsber,
         end of ty_bsik,

         begin of ty_t001,
           bukrs type t001-bukrs,
           butxt type t001-butxt,
         end of ty_t001,


         begin of ty_zinv_aprovador,
           bukrs       type zinv_aprovador-bukrs,
           bukrs_ate   type zinv_aprovador-bukrs_ate,
           tipo        type zinv_aprovador-tipo,
           tp_operacao type zinv_aprovador-tp_operacao,
           matnr       type zinv_aprovador-matnr,
           nivel       type zinv_aprovador-nivel,
           waers       type zinv_aprovador-waers,
           aprovador   type zinv_aprovador-aprovador,
           valor_de    type zinv_aprovador-valor_de,
           valor_ate   type zinv_aprovador-valor_ate,
           dt_val_de   type zinv_aprovador-dt_val_de,               "modificação 11.10.2016
           dt_val_ate  type zinv_aprovador-dt_val_ate,              "modificação 11.10.2016
           hr_val_de   type zinv_aprovador-hr_val_de,               "modificação 03.01.2017
           hr_val_ate  type zinv_aprovador-hr_val_ate,              "modificação 03.01.2017
         end of ty_zinv_aprovador,

         begin of ty_zinv_lotes_aprov,
           bukrs      type zinv_lotes_aprov-bukrs,
           lote       type zinv_lotes_aprov-lote,
           nivel      type zinv_lotes_aprov-nivel,
           aprovador  type zinv_lotes_aprov-aprovador,
           valor_de   type zinv_lotes_aprov-valor_de,
           valor_ate  type zinv_lotes_aprov-valor_ate,
           data_atual type zinv_lotes_aprov-data_atual,
           hora_atual type zinv_lotes_aprov-hora_atual,
           usuario    type zinv_lotes_aprov-usuario,
         end of ty_zinv_lotes_aprov.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
  data:
    begin of tg_lotes occurs 0,
      status(4),
      empresa(30) type c,
      lote        type zfit0036-lote,
      dt_venc(10),
      moeda_pgto  type zfit0036-moeda_pgto,
      total       type zfit0036-vlr_pgto,
      usuario     type zfit0036-usuario,
      tipo(12),
      tp_operacao type zfit0043-tp_operacao,
      ds_operacao type zfit0043-ds_operacao,
      matnr       type zfit0036-matnr,
      maktx       type makt-maktx,
      color(4),
    end of tg_lotes.

** Criação de tabela dinamica
  data:
    wg_cadlote          type ty_cadlote,
    wa_zfit0036         type ty_zfit0036,
    wa_zfit0036_tot     type ty_zfit0036,
    wa_zfit0043         type zfit0043,
    wa_zinv_aprovador   type ty_zinv_aprovador,
    wa_zinv_lotes_aprov type ty_zinv_lotes_aprov,
    wa_zib_contabil     type ty_zib_contabil,
    wa_t001             type ty_t001,
    wa_makt             type makt,
    wl_lotes            like line of tg_lotes,
    wa_estra            type ty_estra,
    wa_docs             type ty_docs,
    wa_bsik             type ty_bsik,
    wa_lfa1             type ty_lfa1,

    tg_estra            type table of ty_estra,
    it_zfit0036         type table of ty_zfit0036,
    it_zfit0036_adt     type table of ty_zfit0036,
    it_zfit0036_tot     type table of ty_zfit0036,
    it_zfit0043         type table of zfit0043,
    it_zinv_aprovador   type table of ty_zinv_aprovador,
    it_zinv_lotes_aprov type table of ty_zinv_lotes_aprov,
    it_zib_contabil     type table of ty_zib_contabil,
    it_t001             type table of ty_t001,
    it_lfa1             type table of ty_lfa1,
    it_lfa1_aux         type table of ty_lfa1,
    it_bsik_adt         type table of ty_bsik,
    it_makt             type table of makt.

  data aux_matnr    type matnr18.


*&--------------------------------------------------------------------&*
*& Declaração Variáveis                                               &*
*&--------------------------------------------------------------------&*
  data: vvalor_ate   type zimp_lotes_aprov-valor_ate,
        xtotal       type zimp_lanc_imp_ct-valor_imp,
        vdep_resp(2),
        vflag(1),
        vflg_ico(1),
        vtipo(2),
* ---> S4 Migration - 20/06/2023 - MA
*        VCHAVE1(22),
        vchave1(50),
*VCHAVE2(22).
        vchave2(50).
* <--- S4 Migration - 20/06/2023 - MA
*&--------------------------------------------------------------------&*
*& Monta Saida                                                        &*
*&--------------------------------------------------------------------&*
  if lote is initial.
    select lote  bukrs  status dt_pgto moeda_pgto vlr_pgto status obj_key invoice navio hbkid observacao usuario operacao matnr lifnr
      from zfit0036
      into table it_zfit0036
     where status = 'A'
       and eliminar ne 'X'.
  else.
    select lote  bukrs  status dt_pgto moeda_pgto vlr_pgto status obj_key invoice navio hbkid observacao usuario operacao matnr lifnr
      from zfit0036
      into table it_zfit0036
     where status = 'A'
       and lote   = lote
       and eliminar ne 'X'.
  endif.

  check it_zfit0036[] is not initial.

  select obj_key hkont
        from zib_contabil
        into table  it_zib_contabil
        for all entries in it_zfit0036
        where obj_key eq it_zfit0036-obj_key
        and   bschl   in ('31','21','29','39').

  sort it_zib_contabil by obj_key.

  if it_zib_contabil[] is not initial.
    select lifnr name1
      from lfa1
      into table it_lfa1
      for all entries in it_zib_contabil
      where lifnr eq it_zib_contabil-hkont.
  endif.

  " Adiantamento doc esta contido no obj_key
  it_zfit0036_adt[] = it_zfit0036[].
  delete it_zfit0036_adt[] where obj_key+0(1) ne 'P'.
  loop at it_zfit0036_adt into wa_zfit0036.
    wa_zfit0036-belnr = wa_zfit0036-obj_key+1(10).
    modify it_zfit0036_adt from  wa_zfit0036 index sy-tabix transporting belnr.
  endloop.

  if it_zfit0036_adt[] is not initial.
    select bsik~bukrs bsik~lifnr bsik~belnr bsik~dmbtr bsik~dmbe2 bsik~budat bsik~buzei bsik~gsber
         from bsik
         into table it_bsik_adt
         for all entries in it_zfit0036_adt
         where bsik~bukrs eq it_zfit0036_adt-bukrs
         and   bsik~belnr eq it_zfit0036_adt-belnr.
    if it_bsik_adt[] is not initial.
      select lifnr name1
        from lfa1
        appending table it_lfa1
        for all entries in it_bsik_adt
        where lifnr eq it_bsik_adt-lifnr.
    endif.
  endif.

  loop at it_zfit0036 into wa_zfit0036.
    if wa_zfit0036-matnr is not initial.

**********************************************************************
* PSA CONVERT MATNR 18
      "DATA AUX_matNR    TYPE matnr18.
*** Formata o código do material
      call function 'CONVERSION_EXIT_MATN1_INPUT'
        exporting
          input  = wa_zfit0036-matnr
        importing
          output = aux_matnr.
      clear: wa_zfit0036-matnr.
      wa_zfit0036-matnr = aux_matnr.
      clear: aux_matnr.

* END CONVERT
**********************************************************************

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = wa_zfit0036-matnr
*        IMPORTING
*          output = wa_zfit0036-matnr.
      modify it_zfit0036 from  wa_zfit0036 index sy-tabix transporting matnr.
    endif.

    if wa_zfit0036-obj_key+0(2) eq 'AC'.
      wa_zfit0036-lifnr = wa_zfit0036-obj_key+13(6).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_zfit0036-lifnr
        importing
          output = wa_zfit0036-lifnr.
      modify it_zfit0036 from  wa_zfit0036 index sy-tabix transporting lifnr.
    elseif wa_zfit0036-obj_key+0(1) eq 'A'.
      wa_zfit0036-ds_operacao = wa_zfit0036-invoice.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_zfit0036-lifnr
        importing
          output = wa_zfit0036-lifnr.
      modify it_zfit0036 from  wa_zfit0036 index sy-tabix transporting ds_operacao lifnr.
    elseif wa_zfit0036-obj_key+0(1) eq 'P'.
      wa_zfit0036-belnr = wa_zfit0036-obj_key+1(10).
      modify it_zfit0036 from  wa_zfit0036 index sy-tabix transporting belnr.
    endif.

  endloop.

*  SELECT *
*    FROM ZFIT0043
*    INTO TABLE IT_ZFIT0043
*    FOR ALL ENTRIES IN IT_ZFIT0036
*    WHERE DS_OPERACAO = IT_ZFIT0036-DS_OPERACAO.

  " Filtrar somente adiantamentos
  select *
    from zfit0043
    into table it_zfit0043
    for all entries in it_zfit0036
    where tp_operacao = it_zfit0036-opera.
  "and spras = sy-langu.

  select lifnr name1
        from lfa1
        appending table it_lfa1
        for all entries in it_zfit0036
        where lifnr eq it_zfit0036-lifnr.

  sort: it_bsik_adt by bukrs belnr,
        it_lfa1     by lifnr,
        it_zfit0043 by tp_operacao.

  select bukrs butxt
    from t001
    into table it_t001
    for all entries in it_zfit0036
    where  bukrs eq it_zfit0036-bukrs.

  select  bukrs bukrs_ate tipo tp_operacao matnr  nivel waers aprovador valor_de valor_ate
    dt_val_de dt_val_ate                                                                       "modificação 11.10.2016
    hr_val_de hr_val_ate                                                                       "modificação 03.01.2017
    from zinv_aprovador
    into table it_zinv_aprovador
    for all entries in it_zfit0036
    where bukrs     le it_zfit0036-bukrs
    and   bukrs_ate ge it_zfit0036-bukrs.

  select bukrs lote nivel aprovador valor_de valor_ate data_atual hora_atual usuario
    from zinv_lotes_aprov
    into table it_zinv_lotes_aprov
    for all entries in it_zfit0036
    where lote = it_zfit0036-lote.

  sort: it_t001             by bukrs,
        it_zfit0036         by lote,
        it_bsik_adt         by bukrs belnr,
        it_lfa1             by lifnr,
        it_zinv_aprovador   by bukrs bukrs_ate tipo tp_operacao matnr nivel,
        it_zinv_lotes_aprov by lote nivel aprovador.


  refresh: tg_lotes, tg_estra.
  it_zfit0036_tot[] = it_zfit0036[].

  sort: it_zfit0036_tot     by lote bukrs dt_pgto,
        it_zinv_lotes_aprov by lote nivel aprovador.

  delete adjacent duplicates from it_zfit0036_tot comparing lote bukrs dt_pgto.

  select *
    from makt
    into table it_makt
    for all entries in it_zfit0036_tot
    where matnr = it_zfit0036_tot-matnr
    and   spras = 'P'.

  sort it_makt by matnr.

  loop at it_zfit0036_tot into wa_zfit0036_tot.
    read table it_t001 into wa_t001 with key bukrs = wa_zfit0036_tot-bukrs binary search.
    concatenate wa_zfit0036_tot-bukrs '-' wa_t001-butxt into  tg_lotes-empresa.

    tg_lotes-lote    = wa_zfit0036_tot-lote.

    concatenate wa_zfit0036_tot-dt_pgto+6(2) wa_zfit0036_tot-dt_pgto+4(2) wa_zfit0036_tot-dt_pgto+0(4)  into tg_lotes-dt_venc separated by '.' .
    tg_lotes-moeda_pgto = wa_zfit0036_tot-moeda_pgto.
    tg_lotes-usuario = wa_zfit0036_tot-usuario.
    if wa_zfit0036_tot-obj_key+0(1) = 'P'.
      tg_lotes-tipo = '02-PERFORMANCE'.
    elseif wa_zfit0036_tot-obj_key+0(1) = 'A'.
      tg_lotes-tipo = '03-Adiantamento'.
    elseif wa_zfit0036_tot-opera ne  '' and wa_zfit0036_tot-bukrs ne '0200'.
      tg_lotes-tipo = '04-INVOICE - Grupo'.
    else.
      tg_lotes-tipo = '01-INVOICE - Terceiro'.
    endif.
    if wa_zfit0036_tot-obj_key+0(1)  = 'A' or ( wa_zfit0036_tot-bukrs eq '0200' and wa_zfit0036_tot-opera is not initial ).
      read table it_zfit0043 into wa_zfit0043 with key tp_operacao = wa_zfit0036_tot-opera binary search.
      tg_lotes-tp_operacao = wa_zfit0043-tp_operacao.
      tg_lotes-ds_operacao = wa_zfit0036_tot-ds_operacao.
    endif.


**********************************************************************
* PSA CONVERT MATNR 18
    "DATA AUX_matNR    TYPE matnr18.
*** Formata o código do material
    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input  = wa_zfit0036_tot-matnr
      importing
        output = aux_matnr.
    clear: tg_lotes-matnr.
    tg_lotes-matnr       = aux_matnr .
    "wa_zinv_aprovador-matnr = AUX_matNR.
    clear: aux_matnr.

* END CONVERT
**********************************************************************


    "tg_lotes-matnr       = wa_zfit0036_tot-matnr.
    read table it_makt into wa_makt with key matnr = wa_zfit0036_tot-matnr binary search.
    if sy-subrc = 0.
      tg_lotes-maktx = wa_makt-maktx.
    endif.


    if wa_zfit0036_tot-dt_pgto lt sy-datum.
      tg_lotes-status = icon_alert.
      tg_lotes-color = 'C611'.
    else.
      clear tg_lotes-status.
      clear tg_lotes-color.
    endif.
    xtotal = 0.
    loop at it_zfit0036 into wa_zfit0036  where lote = wa_zfit0036_tot-lote.
      add wa_zfit0036-vlr_pgto to xtotal.
    endloop.
    tg_lotes-total =  xtotal.

    move tg_lotes to wl_lotes.
    vflg_ico = 'N'.
    vtipo = tg_lotes-tipo+0(2).
    vvalor_ate = 0.

    if vtipo = '04' or wa_zfit0036_tot-bukrs = '0200' . " não coloca material na chave de comparacao
      concatenate vtipo wl_lotes-tp_operacao                into vchave1.
    else.
      concatenate vtipo wl_lotes-tp_operacao wl_lotes-matnr into vchave1.
    endif.
    loop at it_zinv_aprovador into wa_zinv_aprovador.
      if  wa_zinv_aprovador-bukrs_ate is initial.
        if  wa_zinv_aprovador-bukrs ne wl_lotes-empresa+0(4).
          continue.
        endif.
      elseif wa_zinv_aprovador-bukrs     gt wl_lotes-empresa+0(4) or
             wa_zinv_aprovador-bukrs_ate lt wl_lotes-empresa+0(4).
        continue.
      endif.

**********************************************************************
* PSA CONVERT MATNR 18
      "DATA AUX_matNR    TYPE matnr18.
*** Formata o código do material
      call function 'CONVERSION_EXIT_MATN1_INPUT'
        exporting
          input  = wa_zinv_aprovador-matnr
        importing
          output = aux_matnr.
      clear: wa_zinv_aprovador-matnr.
      wa_zinv_aprovador-matnr = aux_matnr.
      clear: aux_matnr.

* END CONVERT
**********************************************************************

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = wa_zinv_aprovador-matnr
*        IMPORTING
*          output = wa_zinv_aprovador-matnr.

      if vtipo = '04'. " não coloca material na chave de comparacao
        concatenate wa_zinv_aprovador-tipo wa_zinv_aprovador-tp_operacao                          into vchave2.
      else.
        concatenate wa_zinv_aprovador-tipo wa_zinv_aprovador-tp_operacao wa_zinv_aprovador-matnr  into vchave2.
      endif.
      if vchave1 = vchave2.
        if ( wa_zinv_aprovador-dt_val_de  lt sy-datum and          "/Modificação 20.03.2017
             wa_zinv_aprovador-dt_val_ate gt sy-datum )            "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          or
           ( wa_zinv_aprovador-dt_val_de  eq sy-datum and          "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
             wa_zinv_aprovador-dt_val_ate eq sy-datum and
             wa_zinv_aprovador-hr_val_de  le sy-uzeit and
             wa_zinv_aprovador-hr_val_ate ge sy-uzeit )
          or
           ( wa_zinv_aprovador-dt_val_de  eq sy-datum and          "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
             wa_zinv_aprovador-dt_val_ate gt sy-datum and
             wa_zinv_aprovador-hr_val_de  le sy-uzeit )
          or
           ( wa_zinv_aprovador-dt_val_de  lt sy-datum and          "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
             wa_zinv_aprovador-dt_val_ate eq sy-datum and
             wa_zinv_aprovador-hr_val_ate ge sy-uzeit ) .
          if wl_lotes-total > vvalor_ate.
            vvalor_ate = wa_zinv_aprovador-valor_ate.
          endif.
        endif.
      endif.
    endloop.

    loop at it_zinv_aprovador into wa_zinv_aprovador .
      if  wa_zinv_aprovador-bukrs_ate is initial.
        if  wa_zinv_aprovador-bukrs ne wl_lotes-empresa+0(4).
          continue.
        endif.
      elseif wa_zinv_aprovador-bukrs     gt wl_lotes-empresa+0(4) or
             wa_zinv_aprovador-bukrs_ate lt wl_lotes-empresa+0(4).
        continue.
      endif.
      concatenate wa_zinv_aprovador-tipo wa_zinv_aprovador-tp_operacao wa_zinv_aprovador-matnr into vchave2.
      if " WA_ZINV_APROVADOR-VALOR_ATE <= VVALOR_ATE AND VCHAVE1 = VCHAVE2
         " AND WA_ZINV_APROVADOR-DT_VAL_DE LE SY-DATUM AND WA_ZINV_APROVADOR-DT_VAL_ATE GE SY-DATUM                     "modificação 11.10.2016
         " AND WA_ZINV_APROVADOR-HR_VAL_DE LE SY-UZEIT AND WA_ZINV_APROVADOR-HR_VAL_ATE GE SY-UZEIT.                    "modificação 03.01.2017

        ( wa_zinv_aprovador-valor_ate  <= vvalor_ate and            "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          vchave1 = vchave2 and
          wa_zinv_aprovador-dt_val_de  lt sy-datum and
          wa_zinv_aprovador-dt_val_ate gt sy-datum )
        or
        ( wa_zinv_aprovador-valor_ate  <= vvalor_ate and            "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
          vchave1 = vchave2 and
          wa_zinv_aprovador-dt_val_de  eq sy-datum and
          wa_zinv_aprovador-dt_val_ate eq sy-datum and
          wa_zinv_aprovador-hr_val_de  le sy-uzeit and
          wa_zinv_aprovador-hr_val_ate ge sy-uzeit )
        or
        ( wa_zinv_aprovador-valor_ate  <= vvalor_ate and            "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          vchave1 = vchave2 and
          wa_zinv_aprovador-dt_val_de  eq sy-datum and
          wa_zinv_aprovador-dt_val_ate gt sy-datum and
          wa_zinv_aprovador-hr_val_de  le sy-uzeit )
        or
        ( wa_zinv_aprovador-valor_ate  <= vvalor_ate and            "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
          vchave1 = vchave2 and
          wa_zinv_aprovador-dt_val_de  lt sy-datum and
          wa_zinv_aprovador-dt_val_ate eq sy-datum and
          wa_zinv_aprovador-hr_val_ate ge sy-uzeit ).

        wa_estra-bukrs        = wl_lotes-empresa+0(4).
        wa_estra-lote         = wl_lotes-lote.
        wa_estra-valor_de     = wa_zinv_aprovador-valor_de.
        wa_estra-valor_ate    = wa_zinv_aprovador-valor_ate.
        "WA_ESTRA-APROVADOR    = WA_ZINV_APROVADOR-APROVADOR.                                                         "modificação 03.01.2017
        wa_estra-nivel        = wa_zinv_aprovador-nivel.

        read table it_zinv_lotes_aprov into wa_zinv_lotes_aprov with key lote      = wl_lotes-lote
                                                                         nivel     = wa_zinv_aprovador-nivel
                                                                         "APROVADOR = WA_ZINV_APROVADOR-APROVADOR     "modificação 03.01.2017
                                                                         binary search.
        if sy-subrc = 0.
          wa_estra-estado       = icon_checked .
          wa_estra-opcoes       = icon_system_undo .
          vflg_ico = 'N'.
          wa_estra-aprovador    = wa_zinv_lotes_aprov-aprovador.                                                      "modificação 03.01.2017
        elseif vflg_ico = 'S'.
          wa_estra-estado       = icon_led_yellow .
          wa_estra-opcoes       = '' .
          wa_estra-aprovador    = wa_zinv_aprovador-aprovador.                                                        "modificação 03.01.2017
        else.
          if v_usuario ne wa_zinv_aprovador-aprovador.
            wa_estra-estado       =  ' '.
            wa_estra-opcoes       = icon_led_yellow  .
          else.
            wa_estra-estado       = icon_led_yellow .
            wa_estra-opcoes       = icon_set_state  .
          endif.
          wa_estra-aprovador    = wa_zinv_aprovador-aprovador.                                              "modificação 03.01.2017
          vflg_ico = 'X'.
        endif.

        if vtipo = wa_zinv_aprovador-tipo.
          if vflg_ico = 'X'.
            vflg_ico = 'S'.
          endif.
          append wa_estra to tg_estra.
        endif.

      endif.
    endloop.

    append tg_lotes.
    clear tg_lotes.
  endloop.

  if tg_lotes[] is not initial.
    sort tg_estra by lote aprovador.
    loop at tg_lotes.
      clear vflag.
      loop at tg_estra into wa_estra where lote      = tg_lotes-lote
                                     and   aprovador = v_usuario.
        vflag = 'X'.
        exit.
      endloop.
      loop at tg_estra into wa_estra where lote      = tg_lotes-lote.
        move-corresponding wa_estra to t_estra.
        append t_estra.
      endloop.
      if vflag = 'X'.
        loop at it_zfit0036 into wa_zfit0036 where lote = tg_lotes-lote.
          wa_docs-lote          = tg_lotes-lote.
          wa_docs-obj_key       = wa_zfit0036-obj_key.
          wa_docs-bukrs         = wa_zfit0036-bukrs.
          if wa_zfit0036-obj_key+0(1) eq 'P'.
            wa_docs-nro_sol_ov = wa_zfit0036-invoice..
          else.
            wa_docs-invoice       = wa_zfit0036-invoice.
          endif.
          wa_docs-navio         = wa_zfit0036-navio.
          wa_docs-vlr_pgto      = wa_zfit0036-vlr_pgto.
          wa_docs-hbkid         = wa_zfit0036-hbkid.
          wa_docs-observacao    = wa_zfit0036-observacao.
          clear wa_lfa1.
          if wa_zfit0036-obj_key+0(1) eq 'P'.
            read table it_bsik_adt into wa_bsik with key bukrs = wa_zfit0036-bukrs
                                                         belnr = wa_zfit0036-belnr binary search.
            read table it_lfa1 into wa_lfa1 with key lifnr = wa_bsik-lifnr binary search.
            wa_docs-name1 = wa_lfa1-name1.
          elseif wa_zfit0036-obj_key+0(1) eq 'A'.
            read table it_lfa1 into wa_lfa1 with key lifnr = wa_zfit0036-lifnr binary search.
            wa_docs-name1 = wa_lfa1-name1.
          else.
            read table it_zib_contabil into wa_zib_contabil with key obj_key = wa_zfit0036-obj_key binary search.
            read table it_lfa1 into wa_lfa1 with key lifnr = wa_zib_contabil-hkont binary search.
            wa_docs-name1 = wa_lfa1-name1.
          endif.
          move-corresponding wa_docs to t_docs.
          append t_docs.
          clear wa_docs.
        endloop.
        move-corresponding tg_lotes to t_lotes.
        append t_lotes.
      endif.
    endloop.
    if t_lotes[] is not initial.
      msg = 'Sucesso'.
    else.
      msg = 'Não há lotes à aprovar.'.
    endif.

  endif.

endfunction.
