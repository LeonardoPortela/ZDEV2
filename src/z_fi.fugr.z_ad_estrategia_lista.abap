function z_ad_estrategia_lista.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) LIKE  SY-UNAME
*"     VALUE(V_NRO_SOL) LIKE  ZFIT0045-NRO_SOL OPTIONAL
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_LOTES STRUCTURE  ZAD_LOTES_IMP
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_IMP OPTIONAL
*"      T_DOCS STRUCTURE  ZAD_DOCS_IMP OPTIONAL
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
           nro_sol     type zfit0045-nro_sol,
           usuario(20) type c,
           total       type zfit0046-vlr_adiantamento,
           dep_resp    type zfit0045-dep_resp,
         end of ty_cadlote,

         begin of ty_estra ,
           bukrs     type zadt_sol_aprov-bukrs,
           nro_sol   type zadt_sol_aprov-nro_sol,
           valor_de  type zadto_aprovador-valor_de,
           valor_ate type zadto_aprovador-valor_ate,
           aprovador type zadto_aprovador-aprovador,
           nivel     type zadto_aprovador-nivel,
           estado(4),
           opcoes(4),
         end of ty_estra,


         begin of ty_zimp_cad_depto,
           dep_resp      type zimp_cad_depto-dep_resp,
           dep_resp_desc type zimp_cad_depto-dep_resp_desc,
         end of ty_zimp_cad_depto,


         begin of ty_user,
           bname     type v_usr_name-bname,
           name_text type v_usr_name-name_text,
         end of ty_user,

         begin of ty_makt,
           matnr type makt-matnr,
           maktx type makt-maktx,
         end of ty_makt,

         begin of ty_lfa1,
           lifnr type lfa1-lifnr,
           name1 type lfa1-name1,
         end of ty_lfa1,

         begin of ty_lfa1_po,
           lifnr type lfa1-lifnr,
           name1 type lfa1-name1,
         end of ty_lfa1_po,

         begin of ty_tcurr,
           kurst type tcurr-kurst,
           fcurr type tcurr-fcurr,
           tcurr type tcurr-tcurr,
           gdatu type tcurr-gdatu,
           ukurs type tcurr-ukurs,
         end of ty_tcurr,

         begin of ty_t005,
           land1 type t005-land1,
           waers type t005-waers,
         end of   ty_t005,

         begin of ty_t001,
           bukrs type t001-bukrs,
           butxt type t001-butxt,
           land1 type t001-land1,
         end of ty_t001,

         begin of ty_ekko,
           ebeln type ekko-ebeln,
           lifnr type ekko-lifnr,
           waers type ekko-waers,
         end of ty_ekko,

         begin of ty_docs ,
           nro_sol          type zadt_sol_aprov-nro_sol,
           ebeln            type zfit0046-ebeln,
           lifnr            type zfit0045-lifnr,
           name1            type lfa1-name1,
           lifnr_po         type ekko-lifnr,
           name1_po         type lfa1-name1,
           ebelp            type zfit0046-ebelp,
           matnr            type zfit0046-matnr,
           maktx            type makt-maktx,
           saldo_item       type zfit0046-saldo_item,
           pgtos_real       type zfit0046-pgtos_real,
           sdo_disponivel   type zfit0046-sdo_disponivel,
           vlr_adiantamento type zfit0046-vlr_adiantamento,
           dt_pgto          type zfit0045-dt_pgto,
           motivo           type zfit0045-motivo,
           dt_prev_liq      type zfit0045-dt_prev_liq,
           solicitante      type v_usr_name-name_text,
           negociador       type v_usr_name-name_text,
         end of ty_docs,


         begin of ty_zadto_aprovador,
           bukrs      type zadto_aprovador-bukrs,
           bukrs_ate  type zadto_aprovador-bukrs_ate,
           dep_resp   type zadto_aprovador-dep_resp,
           waers      type zadto_aprovador-waers,
           nivel      type zadto_aprovador-nivel,
           aprovador  type zadto_aprovador-aprovador,
           valor_de   type zadto_aprovador-valor_de,
           valor_ate  type zadto_aprovador-valor_ate,
           dt_val_de  type zadto_aprovador-dt_val_de,           "modificação 11.10.2016
           dt_val_ate type zadto_aprovador-dt_val_ate,          "modificação 11.10.2016
           hr_val_de  type zadto_aprovador-hr_val_de,           "modificação 03.01.2017
           hr_val_ate type zadto_aprovador-hr_val_ate,          "modificação 03.01.2017
         end of ty_zadto_aprovador,

         begin of ty_zadt_sol_aprov,
           bukrs      type zadt_sol_aprov-bukrs,
           nro_sol    type zadt_sol_aprov-nro_sol,
           nivel      type zadt_sol_aprov-nivel,
           aprovador  type zadt_sol_aprov-aprovador,
           valor_de   type zadt_sol_aprov-valor_de,
           valor_ate  type zadt_sol_aprov-valor_ate,
           data_atual type zadt_sol_aprov-data_atual,
           hora_atual type zadt_sol_aprov-hora_atual,
           usuario    type zadt_sol_aprov-usuario,
         end of ty_zadt_sol_aprov.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
  data:
    begin of tg_lotes occurs 0,
      status(4),
      empresa(30)  type c,
      nro_sol      type zfit0045-nro_sol,
      dep_resp(25) type c,
      dt_venc      type zfit0045-dt_prev_liq,
      total        type zfit0046-vlr_adiantamento,
      color(4),
      moeda(5)     type c,
      motivo       type char255,
      sgtxt	       type sgtxt,
      dt_pagamento type dats,
    end of tg_lotes.


** Criação de tabela dinamica
  data:
    wg_cadlote         type ty_cadlote,

    wa_zfit0045        type zfit0045,
    wa_zfit0046        type zfit0046,

    wa_zimp_cad_depto  type ty_zimp_cad_depto,
    wa_zadto_aprovador type ty_zadto_aprovador,
    wa_zadt_sol_aprov  type ty_zadt_sol_aprov,
    wa_user            type ty_user,
    wa_makt            type ty_makt,
    wa_lfa1            type ty_lfa1,
    wa_t001            type ty_t001,
    wa_t005            type ty_t005,
    wa_ekko            type ty_ekko,
    wa_tcurr           type ty_tcurr,
    wl_lotes           like line of tg_lotes,

    wa_estra           type ty_estra,
    wa_docs            type ty_docs,


    tg_estra           type table of ty_estra,
    it_zfit0045        type table of zfit0045,
    it_zfit0046        type table of zfit0046,

    it_zimp_cad_depto  type table of ty_zimp_cad_depto,
    it_zadto_aprovador type table of ty_zadto_aprovador,
    it_zadt_sol_aprov  type table of ty_zadt_sol_aprov,
    it_user            type table of ty_user,
    it_makt            type table of ty_makt,
    it_lfa1            type table of ty_lfa1,
    it_lfa1_po         type table of ty_lfa1_po,

    it_t001            type table of ty_t001,
    it_t005            type table of ty_t005,
    it_ekko            type table of ty_ekko,
    t_tcurr            type table of ty_tcurr,

    it_estra           type table of ty_estra.


  data: vdep_resp   type zfit0045-dep_resp,
        vflag(1),
        vflg_ico(1),
        vvalor_ate  type zadt_sol_aprov-valor_ate,
        xtotal      type zfit0046-vlr_adiantamento.

  data: vg_date_ref           type sy-datum,
        vg_date_ref_pgto      type sy-datum,
        vg_date_ref_pgto_util type sy-datum,
        vg_date_ref_util      type sy-datum.


  " Seleção
  if v_nro_sol is initial.
    select *
           from zfit0045
           into table it_zfit0045
           where status = 'L'
           and   loekz  = ''
           and   status ne 'B'.

  else.
    select *
           from zfit0045
           into table it_zfit0045
           where nro_sol = v_nro_sol.
  endif.

  check it_zfit0045[] is not initial.

  select bukrs butxt land1
      from t001
      into table it_t001
      for all entries in it_zfit0045
      where  bukrs eq it_zfit0045-bukrs.

  select ebeln lifnr waers
    from ekko
    into table it_ekko
    for all entries in it_zfit0045
    where ebeln = it_zfit0045-ebeln.

  select land1 waers
  from t005
  into table it_t005
  for all entries in it_t001
  where land1 = it_t001-land1.

  select kurst fcurr tcurr gdatu ukurs
    from tcurr
    into table t_tcurr
    for all entries in it_t005
    where kurst = 'B'
    and   fcurr eq 'USD'
    and   tcurr eq it_t005-waers.

  select dep_resp dep_resp_desc
    from zimp_cad_depto
    into table it_zimp_cad_depto
    for all entries in it_zfit0045
    where dep_resp = it_zfit0045-dep_resp.


  select bukrs nro_sol nivel aprovador valor_de valor_ate data_atual hora_atual usuario
    from zadt_sol_aprov
    into table it_zadt_sol_aprov
    for all entries in it_zfit0045
    where nro_sol eq it_zfit0045-nro_sol.

  select lifnr name1
    from lfa1
    into table it_lfa1
    for all entries in it_zfit0045
    where lifnr = it_zfit0045-lifnr.

  select   bname name_text
    from v_usr_name
    into table it_user
    for all entries in it_zfit0045
    where bname = it_zfit0045-usnam.

  select   bname name_text
    from v_usr_name
    appending table it_user
    for all entries in it_zfit0045
    where bname = it_zfit0045-resp_neg.

  select *
    from zfit0046
    into table it_zfit0046
    for all entries in it_zfit0045
    where nro_sol eq it_zfit0045-nro_sol.

  select lifnr name1
         from lfa1
         into table it_lfa1_po
         for all entries in it_ekko
         where lifnr = it_ekko-lifnr.

  sort it_lfa1_po by lifnr.

  loop at it_zfit0046 into wa_zfit0046.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_zfit0046-matnr
      importing
        output = wa_zfit0046-matnr.
    modify it_zfit0046 from wa_zfit0046 index sy-tabix transporting matnr.
  endloop.

  select matnr maktx
  from makt
  into table it_makt
  for all entries in it_zfit0046
  where matnr = it_zfit0046-matnr
  and   spras = 'P'.


  select  bukrs bukrs_ate dep_resp waers nivel aprovador valor_de valor_ate
    dt_val_de dt_val_ate                                                        "modificação 11.10.2016
    hr_val_de hr_val_ate                                                        "modificação 03.01.2017
    from zadto_aprovador
    into table it_zadto_aprovador
    for all entries in it_zfit0045
    where bukrs     le it_zfit0045-bukrs
    and   bukrs_ate ge it_zfit0045-bukrs
    and   dt_val_de  le sy-datum
    and   dt_val_ate ge sy-datum.


  sort: it_t001             by bukrs,
        it_zfit0045         by nro_sol,
        it_zfit0046         by nro_sol,
        it_zadto_aprovador  by bukrs nivel aprovador,
        it_zadt_sol_aprov   by nro_sol nivel aprovador,
        it_zimp_cad_depto   by dep_resp,
        it_t005             by land1,
        it_ekko             by ebeln,
        it_user             by bname,
        it_makt             by matnr,
        it_lfa1             by lifnr.

  data wl_data type datum.

  refresh: tg_lotes, tg_estra.
  loop at it_zfit0045 into wa_zfit0045.

    read table it_t001 into wa_t001 with key bukrs = wa_zfit0045-bukrs binary search.
    concatenate wa_zfit0045-bukrs '-' wa_t001-butxt into  tg_lotes-empresa.

    tg_lotes-nro_sol      = wa_zfit0045-nro_sol.
    read table it_zimp_cad_depto into wa_zimp_cad_depto with key dep_resp = wa_zfit0045-dep_resp binary search.

    concatenate wa_zfit0045-dep_resp '-' wa_zimp_cad_depto-dep_resp_desc into tg_lotes-dep_resp.

    call function 'ADD_TIME_TO_DATE'
      exporting
        i_idate               = wa_zfit0045-dt_pgto
        i_time                = -3
        i_iprkz               = ''
      importing
        o_idate               = wl_data
      exceptions
        invalid_period        = 1
        invalid_round_up_rule = 2
        internal_error        = 3
        others                = 4.


* User Story 144133 / aoenning.
    clear: vg_date_ref, vg_date_ref_util, vg_date_ref_pgto.
    vg_date_ref = wa_zfit0045-dt_atual + 5.
    zcl_miro=>get_proximo_dia_util( exporting i_data_base = vg_date_ref
                                              i_signum    = '+'
                                    receiving r_data      = vg_date_ref_util ).

    vg_date_ref_pgto = sy-datum + 3.
    zcl_miro=>get_proximo_dia_util( exporting i_data_base = vg_date_ref_pgto
                                              i_signum    = '+'
                                    receiving r_data      = vg_date_ref_pgto_util ).



* User Story 144133 / AOENNING.
    if wa_zfit0045-dt_pgto < vg_date_ref_pgto_util.
      wa_zfit0045-dt_pgto = vg_date_ref_pgto_util. "* User Story 144133 / AOENNING.
    endif.

    tg_lotes-dt_venc = wa_zfit0045-dt_pgto.

    if vg_date_ref_util > vg_date_ref_pgto_util. "* User Story 144133 / AOENNING.

*    if wl_data lt sy-datum.
      tg_lotes-status = icon_alert.
      tg_lotes-color = 'C611'.
    else.
      clear tg_lotes-status.
      clear tg_lotes-color.
    endif.
*    "limpar para subir prd 08/07/2025
*    clear tg_lotes-status.
*    clear tg_lotes-color.

    xtotal = 0.

    clear wa_tcurr .

    read table it_t005 into wa_t005 with key land1 =  wa_t001-land1 binary search.
    loop at t_tcurr into wa_tcurr where tcurr = wa_t005-waers.
      exit.
    endloop.

    read table it_ekko into wa_ekko with key ebeln = wa_zfit0045-ebeln binary search.

    read table it_zadto_aprovador into wa_zadto_aprovador with key dep_resp = wa_zfit0045-dep_resp.

    if wa_zfit0045-moeda_pgto = wa_zadto_aprovador-waers. "moeda do pagamento igual estrategia
      wa_tcurr-ukurs = 1.
    endif.

    tg_lotes-moeda        = wa_zadto_aprovador-waers.
    tg_lotes-sgtxt        = wa_zfit0045-sgtxt.
    if wa_zfit0045-sgtxt <> wa_zfit0045-motivo.
      tg_lotes-motivo       = wa_zfit0045-motivo.
    else.
      clear: tg_lotes-motivo.
    endif.
    tg_lotes-dt_pagamento = wa_zfit0045-dt_pgto.

    loop at it_zfit0046 into wa_zfit0046  where nro_sol = wa_zfit0045-nro_sol.
      if wa_zadto_aprovador-waers = 'USD' and  wa_zfit0045-moeda_pgto ne 'USD'.
        xtotal = xtotal + ( wa_zfit0046-vlr_adiantamento / wa_tcurr-ukurs ).
      else.
        xtotal = xtotal + ( wa_zfit0046-vlr_adiantamento * wa_tcurr-ukurs ).
      endif.
    endloop.

    tg_lotes-total = xtotal.
    "Inicio
    move tg_lotes to wl_lotes.
    clear vdep_resp.
    vvalor_ate = 0.
    vflg_ico = 'N'.

    loop at it_zadto_aprovador into wa_zadto_aprovador.
      if  wa_zadto_aprovador-bukrs_ate is initial.
        if  wa_zadto_aprovador-bukrs ne wl_lotes-empresa+0(4).
          continue.
        endif.
      elseif wa_zadto_aprovador-bukrs     gt wl_lotes-empresa+0(4) or
             wa_zadto_aprovador-bukrs_ate lt wl_lotes-empresa+0(4).
        continue.
      endif.
      if wa_zfit0045-dep_resp = wa_zadto_aprovador-dep_resp and
        ( ( wa_zadto_aprovador-dt_val_de  lt sy-datum and           "/Modificação 20.03.2017
            wa_zadto_aprovador-dt_val_ate gt sy-datum )             "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          or
          ( wa_zadto_aprovador-dt_val_de  eq sy-datum and           "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
            wa_zadto_aprovador-dt_val_ate eq sy-datum and
            wa_zadto_aprovador-hr_val_de  le sy-uzeit and
            wa_zadto_aprovador-hr_val_ate ge sy-uzeit )
          or
          ( wa_zadto_aprovador-dt_val_de  eq sy-datum and           "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
            wa_zadto_aprovador-dt_val_ate gt sy-datum and
            wa_zadto_aprovador-hr_val_de  le sy-uzeit )
          or
          ( wa_zadto_aprovador-dt_val_de  lt sy-datum and           "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
            wa_zadto_aprovador-dt_val_ate eq sy-datum and
            wa_zadto_aprovador-hr_val_ate ge sy-uzeit ) ).
        if wl_lotes-total > vvalor_ate.
          vvalor_ate = wa_zadto_aprovador-valor_ate.
          vdep_resp = wa_zadto_aprovador-dep_resp.
        endif.
      endif.
    endloop.
    if vdep_resp is initial.
      loop at it_zadto_aprovador into wa_zadto_aprovador.
        if  wa_zadto_aprovador-bukrs_ate is initial.
          if  wa_zadto_aprovador-bukrs ne wl_lotes-empresa+0(4).
            continue.
          endif.
        elseif wa_zadto_aprovador-bukrs     gt wl_lotes-empresa+0(4) or
               wa_zadto_aprovador-bukrs_ate lt wl_lotes-empresa+0(4).
          continue.
        endif.
        if wa_zadto_aprovador-dep_resp is initial.
          if wl_lotes-total > vvalor_ate.
            if ( wa_zadto_aprovador-dt_val_de  lt sy-datum and           "/Modificação 20.03.2017
                 wa_zadto_aprovador-dt_val_ate gt sy-datum )             "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
               or
               ( wa_zadto_aprovador-dt_val_de  eq sy-datum and           "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
                 wa_zadto_aprovador-dt_val_ate eq sy-datum and
                 wa_zadto_aprovador-hr_val_de  le sy-uzeit and
                 wa_zadto_aprovador-hr_val_ate ge sy-uzeit )
               or
               ( wa_zadto_aprovador-dt_val_de  eq sy-datum and           "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
                 wa_zadto_aprovador-dt_val_ate gt sy-datum and
                 wa_zadto_aprovador-hr_val_de  le sy-uzeit )
               or
               ( wa_zadto_aprovador-dt_val_de  lt sy-datum and           "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
                 wa_zadto_aprovador-dt_val_ate eq sy-datum and
                 wa_zadto_aprovador-hr_val_ate ge sy-uzeit ).
              vvalor_ate = wa_zadto_aprovador-valor_ate.
              vdep_resp = wa_zadto_aprovador-dep_resp.
            endif.
          endif.
        endif.
      endloop.
    endif.

    loop at it_zadto_aprovador into wa_zadto_aprovador  where dep_resp = vdep_resp.
      if  wa_zadto_aprovador-bukrs_ate is initial.
        if  wa_zadto_aprovador-bukrs ne wl_lotes-empresa+0(4).
          continue.
        endif.
      elseif wa_zadto_aprovador-bukrs     gt wl_lotes-empresa+0(4) or
             wa_zadto_aprovador-bukrs_ate lt wl_lotes-empresa+0(4).
        continue.
      endif.
      if  "WA_ZADTO_APROVADOR-VALOR_ATE <= VVALOR_ATE AND
          "WA_ZADTO_APROVADOR-DT_VAL_DE LE SY-DATUM AND             "modificação 11.10.2016
          "WA_ZADTO_APROVADOR-DT_VAL_ATE GE SY-DATUM.               "modificação 11.10.2016
          "WA_ZADTO_APROVADOR-HR_VAL_DE LE SY-UZEIT AND             "modificação 03.01.2017
          "WA_ZADTO_APROVADOR-HR_VAL_ATE GE SY-UZEIT.               "modificação 03.01.2017

        ( wa_zadto_aprovador-valor_ate  <= vvalor_ate and            "Caso p/ DT_VAL_DE < SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          wa_zadto_aprovador-dt_val_de  lt sy-datum and
          wa_zadto_aprovador-dt_val_ate gt sy-datum )
        or
        ( wa_zadto_aprovador-valor_ate  <= vvalor_ate and            "Caso p/ DT_VAL_DE = SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
          wa_zadto_aprovador-dt_val_de  eq sy-datum and
          wa_zadto_aprovador-dt_val_ate eq sy-datum and
          wa_zadto_aprovador-hr_val_de  le sy-uzeit and
          wa_zadto_aprovador-hr_val_ate ge sy-uzeit )
        or
        ( wa_zadto_aprovador-valor_ate  <= vvalor_ate and            "Caso p/ DT_VAL_DE = SY-DATUM < DT_VAL_ATE (modificação 11.01.2017)
          wa_zadto_aprovador-dt_val_de  eq sy-datum and
          wa_zadto_aprovador-dt_val_ate gt sy-datum and
          wa_zadto_aprovador-hr_val_de  le sy-uzeit )
        or
        ( wa_zadto_aprovador-valor_ate  <= vvalor_ate and            "Caso p/ DT_VAL_DE < SY-DATUM = DT_VAL_ATE (modificação 11.01.2017)
          wa_zadto_aprovador-dt_val_de  lt sy-datum and
          wa_zadto_aprovador-dt_val_ate eq sy-datum and
          wa_zadto_aprovador-hr_val_ate ge sy-uzeit ).

        wa_estra-bukrs        = wl_lotes-empresa+0(4).
        wa_estra-nro_sol      = wl_lotes-nro_sol.
        wa_estra-valor_de     = wa_zadto_aprovador-valor_de.
        wa_estra-valor_ate    = wa_zadto_aprovador-valor_ate.
        "WA_ESTRA-APROVADOR    = WA_ZADTO_APROVADOR-APROVADOR.    "modificação 03.01.2017
        wa_estra-nivel        = wa_zadto_aprovador-nivel.

        read table it_zadt_sol_aprov into wa_zadt_sol_aprov with key nro_sol   = wl_lotes-nro_sol
                                                                     nivel     = wa_zadto_aprovador-nivel
                                                                     "APROVADOR = WA_ZADTO_APROVADOR-APROVADOR      "modificação 03.01.2017
                                                                     binary search.
*        IF SY-SUBRC = 0.
*          WA_ESTRA-ESTADO       = ICON_CHECKED .
*          WA_ESTRA-OPCOES       = ICON_SYSTEM_UNDO .
*          VFLG_ICO = 'N'.
*        ELSEIF VFLG_ICO = 'S'.
*          WA_ESTRA-ESTADO       = ICON_LED_YELLOW .
*          WA_ESTRA-OPCOES       = '' .
*        ELSE.
*          IF V_USUARIO NE WA_ZADTO_APROVADOR-APROVADOR.
*            WA_ESTRA-ESTADO       =  ' '.
*            WA_ESTRA-OPCOES       = ICON_LED_YELLOW  .
*          ELSE.
*            WA_ESTRA-ESTADO       = ICON_LED_YELLOW .
*            WA_ESTRA-OPCOES       = ICON_SET_STATE  .
*          ENDIF.
*          VFLG_ICO = 'X'.
*        ENDIF.

        if sy-subrc = 0.
          wa_estra-estado       = icon_checked .
          wa_estra-opcoes       = icon_system_undo .
          vflg_ico = 'N'.
          wa_estra-aprovador    = wa_zadt_sol_aprov-aprovador.         "modificação 03.01.2017
        elseif vflg_ico = 'S'.
          wa_estra-estado       = icon_led_yellow .
          wa_estra-opcoes       = '' .
          wa_estra-aprovador    = wa_zadto_aprovador-aprovador.         "modificação 03.01.2017
        else.
          if v_usuario ne wa_zadto_aprovador-aprovador.
            wa_estra-estado       =  ' '.
            wa_estra-opcoes       = icon_led_yellow  .
          else.
            wa_estra-estado       = icon_led_yellow .
            wa_estra-opcoes       = icon_set_state  .
          endif.
          vflg_ico = 'X'.
          wa_estra-aprovador    = wa_zadto_aprovador-aprovador.         "modificação 03.01.2017
        endif.

        if vdep_resp is initial.
          if vflg_ico = 'X'.
            vflg_ico = 'S'.
          endif.
          append wa_estra to tg_estra.
        elseif vdep_resp = wa_zadto_aprovador-dep_resp.
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
    sort tg_estra by nro_sol aprovador.
    loop at tg_lotes.
      clear vflag.
      loop at tg_estra into wa_estra where nro_sol   = tg_lotes-nro_sol
                                     and   aprovador = v_usuario.
        vflag = 'X'.
        exit.
      endloop.
      loop at tg_estra into wa_estra where nro_sol   = tg_lotes-nro_sol.
        move-corresponding wa_estra to t_estra.
        move  wa_estra-nro_sol to t_estra-lote.
        append t_estra.
      endloop.
      if vflag = 'X'.
        loop at it_zfit0046 into wa_zfit0046 where nro_sol = tg_lotes-nro_sol.
          move-corresponding wa_zfit0046 to wa_docs.
          read table it_zfit0045 into wa_zfit0045 with key nro_sol = wa_zfit0046-nro_sol binary search.
          wa_docs-dt_pgto     = wa_zfit0045-dt_pgto.
          wa_docs-motivo      = wa_zfit0045-motivo.
          wa_docs-dt_prev_liq = wa_zfit0045-dt_prev_liq.

          read table it_lfa1 into wa_lfa1 with key lifnr = wa_zfit0045-lifnr binary search.
          concatenate wa_lfa1-lifnr '-' wa_lfa1-name1 into wa_docs-name1.

          " Início - DEVK9A1SRA - ABAP - #128276 RSA
          read table it_ekko into wa_ekko with key ebeln = wa_zfit0045-ebeln binary search.
          read table it_lfa1_po into data(wa_lfa1_po) with key lifnr = wa_ekko-lifnr binary search.
          wa_docs-lifnr_po = wa_ekko-lifnr.
          concatenate wa_ekko-lifnr '-' wa_lfa1_po-name1 into wa_docs-name1_po.
          " Fim - DEVK9A1SRA - ABAP - #128276 RSA

          read table it_user into wa_user with key bname = wa_zfit0045-usnam binary search.
          wa_docs-solicitante = wa_user-name_text.
          read table it_user into wa_user with key bname = wa_zfit0045-resp_neg binary search.
          wa_docs-negociador = wa_user-name_text.
          read table it_makt into wa_makt with key matnr = wa_zfit0046-matnr binary search.
          wa_docs-maktx = wa_makt-maktx.
          move-corresponding wa_docs to t_docs.
          append t_docs.
        endloop.
        move-corresponding tg_lotes to t_lotes.
        concatenate tg_lotes-dt_venc+6(2) '.' tg_lotes-dt_venc+4(2) '.' tg_lotes-dt_venc+0(4) into t_lotes-dt_venc.
        append t_lotes.
      endif.
    endloop.
    if t_lotes[] is not initial.
      msg = 'Sucesso'.
    else.
      msg = 'Não há solicitações à aprovar.'.
    endif.

  endif.


endfunction.
