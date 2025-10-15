*&---------------------------------------------------------------------*
*& Report  Z_GRAVA_ZIB_ZGL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  z_grava_zib_zgl.

type-pools: pmst.

types: begin of ty_zib_contabil.
         include structure zib_contabil.
types:   mark type c,
       end of ty_zib_contabil.

types: begin of ty_t001,
         bukrs type t001-bukrs,
         land1 type t001-land1,
         ktopl type t001-ktopl,
       end of   ty_t001,

       begin of ty_t005,
         land1 type t005-land1,
         waers type t005-waers,
         curin type t005-curin,
         curha type t005-curha,
       end of   ty_t005.

data: begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data: end of it_msg.

data: wl_mode(1),
      v_anln1    type anla-anln1,
      v_anln2    type anla-anln2,
      wa_t001    type ty_t001,
      wa_t095    type t095,
      wa_anla    type anla,
      wa_t003t   type t003t,
      wa_t005    type ty_t005,
      wa_kna1    type kna1,
      wa_lfa1    type lfa1,
      wa_skat    type skat.

data: wl_message   type pmst_raw_message,
      tg_zshdb_msg type table of zshdbt0002 with header line,
      tg_zshdb     type table of zshdbt0001 with header line,
      tl_shdb      type table of zshdbt0001 with header line.

data: ti_bdcdata       type standard table of bdcdata ,   "Guarda o mapeamento
      wa_bdcdata       like line of ti_bdcdata,
      t_messtab        type table of bdcmsgcoll,
      wl_shdbnr        type zshdbt0001-shdbnr,
      vobj_key         type zib_contabil_err-obj_key,
      vst_lc_moeda     type zglt035-st_lc_moeda,
      account_name(20) type c,
      wg_documento(10).

data: wa_zglt090 type zglt090,
      it_zglt090 type table of zglt090.

data: v_msg    type char50,
      t_lotes  type table of zfi_lotes_imp,
      w_lotes  type          zfi_lotes_imp,
      vg_lote  type          zglt034-lote,

      t_estra  type table of zfi_estrategia_zgl,
      w_estra  type          zfi_estrategia_zgl,

      t_docs   type table of zgl_docs_imp,
      w_docs   type          zgl_docs_imp,

      tg_estra type table of zfi_estrategia_imp,
      wg_estra type zfi_estrategia_imp.

data: gv_object_reference type c length 40,
      gv_job_name         type tbtcm-jobname,
      gv_job_count        type tbtcm-jobcount.

data: it_zglt035          type table of zglt035 with header line,
      wa_zglt035          type zglt035,
      wa_zglt034          type zglt034,
      wa_zimp_cad_depto   type zimp_cad_depto,
      it_zglt036          type table of zglt036 with header line,
      it_zglt036_iva      type table of zglt036 with header line,
      it_zglt036_con      type table of zglt036 with header line,
      it_zglt036_mat      type table of zglt036 with header line,
      it_zglt036_mat2     type table of zglt036 with header line,
      it_zglt031          type table of zglt031 with header line,
      it_zib_contabil     type table of ty_zib_contabil,
      wa_zib_contabil     type ty_zib_contabil,
      wa_zib_contabil_chv type zib_contabil_chv,
      wa_zib_contabil_err type zib_contabil_err,
      it_zib_contabil_err type table of zib_contabil_err with header line,
      tg_msg              type table of bdcmsgcoll with header line,
      wg_par              type ctu_params,
      tabix               type sy-tabix.

selection-screen: begin of block b1 with frame title text-001.
  parameters: p_lote type zglt035-lote,
              p_test type c.
selection-screen: end of block b1.

start-of-selection.

  perform seleciona_dados.
  perform organiza_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciona_dados .
  if sy-batch is not initial.
    call function 'GET_JOB_RUNTIME_INFO'
      importing
        jobname         = gv_job_name
        jobcount        = gv_job_count
      exceptions
        no_runtime_info = 1
        others          = 2.
    if sy-subrc <> 0.
    endif.

    split gv_job_name at '|' into data(lv_desc)
                                    data(lv_lote).
    if lv_lote is not initial.
      p_lote = lv_lote.
    endif.

  endif.

  select single *
    into wa_zglt090
    from zglt090 where lote = p_lote.

  if sy-subrc ne 0.
    refresh: t_lotes, t_estra, t_docs.
    call function 'Z_GL_ESTRATEGIA_LISTA'
      exporting
        v_usuario = sy-uname
        v_lote    = p_lote
      importing
        msg       = v_msg
      tables
        t_lotes   = t_lotes
        t_estra   = t_estra
        t_docs    = t_docs.

    refresh tg_estra .
    sort t_estra by nivel.
    loop at t_estra into w_estra.
      move-corresponding w_estra to wg_estra.
      append wg_estra to tg_estra.
      move-corresponding wg_estra to wa_zglt090.
      wa_zglt090-data_atual = sy-datum.
      wa_zglt090-hora_atual = sy-uzeit.
      wa_zglt090-usuario    = sy-uname.
      append wa_zglt090 to it_zglt090.
    endloop.
    "
    " grava estratégia
    modify zglt090 from table it_zglt090.
    commit work."

  endif.

  select *
    into table it_zglt035
    from zglt035
    where lote = p_lote
    and loekz  = ''.

  check it_zglt035[] is not initial.

  loop at it_zglt035 .
    tabix = sy-tabix.
    concatenate 'ZGL17' it_zglt035-doc_lcto it_zglt035-budat+0(4) into vobj_key .
    select single *
    from zib_contabil
    into wa_zib_contabil
    where obj_key like vobj_key
    and   bukrs = it_zglt035-bukrs.

    if sy-subrc = 0. " sucesso
      delete it_zglt035 index tabix.
    endif.
  endloop.

  check it_zglt035[] is not initial.

  select *
    into table it_zglt036
    from zglt036
    for all entries in it_zglt035
    where doc_lcto = it_zglt035-doc_lcto.

  check it_zglt036[] is not initial.

  select *
    into table it_zglt031
    from zglt031
    for all entries in it_zglt036
    where tp_lcto = it_zglt036-tp_lcto.

endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form organiza_dados .
  data: vseq        type i value 0,
        vbudat(10),
        vbldat(10),
        "VOBJ_KEY TYPE ZIB_CONTABIL_ERR-OBJ_KEY,
        vbudat2     type bsis-budat,
        wl_tbsl     type tbsl,
        wl_tbsl_con type tbsl,
        wl_loop(1),
        p_erro(1),
        vdata(10),
        smes(2),
        vmes        type i,
        sano(4),
        vano        type i,
        vflag(1),
        vwrbtr      type bseg-wrbtr,
        vwrbtr_con  type bseg-wrbtr,
        wl_vlr(16),
        vcampo(15),
        vcampo2(15),
        cnum_seq(2),
        vnum_seq    type i,
        cont_lines  like sy-tabix,
        prox_lines  like sy-tabix,
        tab_lines   like sy-tabix,
        tab_loop    type sy-tabix,
        tab_limite  type sy-tabix,
        vgsber      type zglt036-gsber,
        vbschl      type zglt036-bschl.

  sort: it_zglt035 by doc_lcto,
        it_zglt036 by doc_lcto,
        it_zglt031 by tp_lcto.

  "Seleção dados material.
  select matnr, mtart, meins from mara
  into table @data(it_mara)
  for all entries in @it_zglt036
  where matnr eq @it_zglt036-matnr_fi.

  loop at it_zglt035 .
    vseq = 0.
    clear vflag.

    if it_zglt035-belnr  is not initial.
      continue.
    endif.

    concatenate  it_zglt035-bldat+6(2) it_zglt035-bldat+4(2) it_zglt035-bldat+0(4) into vbldat separated by '.'.
    concatenate  it_zglt035-budat+6(2) it_zglt035-budat+4(2) it_zglt035-budat+0(4) into vbudat separated by '.'.

    select single bukrs land1 ktopl
       from t001
       into wa_t001
       where bukrs = it_zglt035-bukrs.

    select single land1 waers curin curha
      from t005
      into wa_t005
      where land1 = wa_t001-land1.

    refresh it_zib_contabil.
    loop at it_zglt036 where doc_lcto = it_zglt035-doc_lcto.

      if it_zglt036-vlr_moeda_int   eq 0 and
         it_zglt036-vlr_moeda_forte eq 0 and
         it_zglt036-vlr_moeda_grupo eq 0 and
         it_zglt036-vlr_moeda_doc   eq 0.
        continue.
      endif.

      add 1 to vseq.

      concatenate 'ZGL17' it_zglt035-doc_lcto it_zglt035-budat+0(4) into wa_zib_contabil-obj_key.
      wa_zib_contabil-seqitem   = vseq.
      wa_zib_contabil-xblnr     = it_zglt035-xblnr.
      wa_zib_contabil-bschl     = it_zglt036-bschl.
      wa_zib_contabil-gsber     = it_zglt036-gsber.
      wa_zib_contabil-werks     = it_zglt036-werks.
      wa_zib_contabil-zlsch     = it_zglt036-zlsch.
      wa_zib_contabil-zlspr     = it_zglt036-zlspr.
      wa_zib_contabil-bukrs     = it_zglt035-bukrs.
      wa_zib_contabil-interface = ' '.
      wa_zib_contabil-zuonr = it_zglt035-bldat. " Rubenilson Pereira - 24.07.25 - #185993

      if  ( wa_zib_contabil-bukrs <> '0100' ) and
          ( wa_zib_contabil-bukrs <> '0101' ) and
          ( wa_zib_contabil-bukrs <> '0200' ) and
          ( wa_zib_contabil-bukrs <> '0201' ) and
*" RMNI - CS1042045 - IR118896 - INICIO - 12/08/2022
" DESCOMENTAR ESTE CÓDIGO APÓS GO-LIVE DESTE CASE
*          ( WA_ZIB_CONTABIL-BUKRS <> '0202' ) AND
*" RMNI - CS1042045 - IR118896 - FIM - 12/08/2022
          ( wa_zib_contabil-bukrs <> '0004' ) and
          ( wa_zib_contabil-bukrs <> '0037' ).

        wa_zib_contabil-bupla = wa_zib_contabil-gsber.

      endif.

      if ( wa_zib_contabil-gsber = 'TGRM' ).

        wa_zib_contabil-bupla = '3802'.

      endif.

      select single *
        from zglt034
        into wa_zglt034
        where lote = it_zglt035-lote.

      select single *
        from zimp_cad_depto
        into wa_zimp_cad_depto
        where dep_resp = wa_zglt034-dep_resp.

      concatenate wa_zglt034-usnam '-' wa_zimp_cad_depto-dep_resp_desc   into wa_zib_contabil-bktxt.

      select single * from tbsl into wl_tbsl where bschl eq it_zglt036-bschl.

      if  it_zglt036-sgtxt is initial.
        select single *
            from t003t
            into wa_t003t
            where spras = sy-langu
            and   blart = it_zglt035-blart.
        if sy-subrc = 0.
          wa_zib_contabil-sgtxt     = wa_t003t-ltext.

          case wl_tbsl-koart.
            when 'D'. " Quando Cliente
              select single *
                from kna1
                into wa_kna1
               where kunnr = it_zglt036-hkont.
              account_name = wa_kna1-name1.
              translate account_name to upper case.

            when 'K'. " Quando fornecedor
              select single *
                from lfa1
                into wa_lfa1
               where lifnr = it_zglt036-hkont.
              account_name = wa_lfa1-name1.
              translate account_name to upper case.

            when others.

              select single *
                from skat
                into wa_skat
               where spras = sy-langu
                 and ktopl = wa_t001-ktopl
                 and saknr = it_zglt036-hkont.
              account_name = wa_skat-txt20.
          endcase.

          concatenate wa_zib_contabil-sgtxt account_name into wa_zib_contabil-sgtxt separated by ' '.
        endif.
      else.
        wa_zib_contabil-sgtxt     = it_zglt036-sgtxt.
      endif.

      concatenate  it_zglt035-bldat+6(2) it_zglt035-bldat+4(2) it_zglt035-bldat+0(4) into wa_zib_contabil-bldat separated by '.'.
      concatenate  it_zglt035-budat+6(2) it_zglt035-budat+4(2) it_zglt035-budat+0(4) into wa_zib_contabil-budat separated by '.'.

      clear wa_zib_contabil-zfbdt .
      if it_zglt036-dt_vct is not initial.
        concatenate  it_zglt036-dt_vct+6(2) it_zglt036-dt_vct+4(2) it_zglt036-dt_vct+0(4) into wa_zib_contabil-zfbdt separated by '.'.
        " RMNI - CS1042045 - IR118896 - INICIO - 12/08/2022
      else.
        concatenate  sy-datum+6(2) sy-datum+4(2) sy-datum(4) into wa_zib_contabil-zfbdt separated by '.'.
        " RMNI - CS1042045 - IR118896 - FIM - 12/08/2022
      endif.

      wa_zib_contabil-gjahr     = it_zglt035-budat+0(4).
      if it_zglt035-monat is not initial.
        wa_zib_contabil-monat     = it_zglt035-monat.
      else.
        wa_zib_contabil-monat     = it_zglt035-budat+4(2).
      endif.
      wa_zib_contabil-blart     = it_zglt035-blart.


      if wl_tbsl-koart = 'A'. "Imobilizado
        split it_zglt036-hkont  at '-' into v_anln1 v_anln2.
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

        select single * from anla into wa_anla where anln1 eq v_anln1
                                                 and anln2 eq v_anln2.

        if sy-subrc = 0.
          select single * from t095 into wa_t095
            where ktopl = '0050'
            and   ktogr = wa_anla-ktogr
            and   afabe = 1.
          wa_zib_contabil-hkont     = wa_t095-ktansw.
          wa_zib_contabil-anln1     = v_anln1.
          wa_zib_contabil-anln2     = v_anln2.
          wa_zib_contabil-bewar       = it_zglt036-anbwa.
        endif.
      else.
        wa_zib_contabil-hkont     = it_zglt036-hkont.
        wa_zib_contabil-bewar     = it_zglt036-bewar.
      endif.

      wa_zib_contabil-kostl     = it_zglt036-kostl.
      wa_zib_contabil-aufnr     = it_zglt036-aufnr.
      wa_zib_contabil-vornr     = it_zglt036-vornr.
      wa_zib_contabil-vbeln     = it_zglt036-vbeln.
      wa_zib_contabil-prctr     = it_zglt036-prctr.
      wa_zib_contabil-umskz     = it_zglt036-umskz.

      vst_lc_moeda = it_zglt035-st_lc_moeda.

      "SE lançar em apenas uma coluna, considerar em uma unica moeda tambem
      "Moeda Interna
      if it_zglt036-vlr_moeda_int   ne 0 and
         it_zglt036-vlr_moeda_forte eq 0 and
         it_zglt036-vlr_moeda_grupo eq 0 and
         it_zglt036-vlr_moeda_doc eq 0.
        vst_lc_moeda = 'X'.
      endif.

      "Moeda Forte
      if it_zglt036-vlr_moeda_int   eq 0 and
         it_zglt036-vlr_moeda_forte ne 0 and
         it_zglt036-vlr_moeda_grupo eq 0 and
         it_zglt036-vlr_moeda_doc eq 0.
        vst_lc_moeda = 'X'.
      endif.

      "Moeda Grupo
      if it_zglt036-vlr_moeda_int   eq 0 and
         it_zglt036-vlr_moeda_forte eq 0 and
         it_zglt036-vlr_moeda_grupo ne 0 and
         it_zglt036-vlr_moeda_doc eq 0.
        vst_lc_moeda = 'X'.
      endif.

      if vst_lc_moeda = 'X'. "o lançamento em unica moeda.
        wa_zib_contabil-wrbtr     = it_zglt036-vlr_moeda_doc.
      elseif it_zglt035-moeda_doc = wa_t005-waers.
        wa_zib_contabil-wrbtr     = it_zglt036-vlr_moeda_int.
      elseif it_zglt035-moeda_doc = wa_t005-curha and it_zglt036-vlr_moeda_forte gt 0.
        wa_zib_contabil-wrbtr     = it_zglt036-vlr_moeda_forte.
      elseif it_zglt035-moeda_doc = wa_t005-curin. " moeda grupo
        wa_zib_contabil-wrbtr     = it_zglt036-vlr_moeda_grupo.
      else.
        wa_zib_contabil-wrbtr     = it_zglt036-vlr_moeda_doc.
      endif.
      wa_zib_contabil-waers     = it_zglt035-moeda_doc.


      if it_zglt036-vlr_moeda_int ne 0.
        if wa_t005-land1 = 'PA'.
          wa_zib_contabil-waers_i   = 'USD'.
        else.
          wa_zib_contabil-waers_i   = wa_t005-waers.
        endif.
        if it_zglt036-vlr_moeda_int lt 0.
          wa_zib_contabil-dmbtr     = it_zglt036-vlr_moeda_int * -1.
        else.
          wa_zib_contabil-dmbtr     = it_zglt036-vlr_moeda_int .
        endif.
      else.
        wa_zib_contabil-waers_i   = ''.
        wa_zib_contabil-dmbtr     = 0.
      endif.

      if   vst_lc_moeda = 'X'. "o lançamento em moeda interna é a moeda do documento se não for as moedas padrão
        wa_zib_contabil-waers_i   = 'X'.
      endif.

      if it_zglt036-vlr_moeda_forte ne 0.
        wa_zib_contabil-waers_f   = wa_t005-curha.
        if it_zglt036-vlr_moeda_forte lt 0.
          wa_zib_contabil-dmbe2     = it_zglt036-vlr_moeda_forte * -1.
        else.
          wa_zib_contabil-dmbe2     = it_zglt036-vlr_moeda_forte .
        endif.
      else.
        wa_zib_contabil-waers_f   = ''.
        wa_zib_contabil-dmbe2     = 0.
      endif.


      if it_zglt036-vlr_moeda_grupo ne 0.
        wa_zib_contabil-waers_g   = wa_t005-curin.
        if it_zglt036-vlr_moeda_grupo lt 0.
          wa_zib_contabil-dmbe3     = it_zglt036-vlr_moeda_grupo * -1.
        else.
          wa_zib_contabil-dmbe3     = it_zglt036-vlr_moeda_grupo .
        endif.
      else.
        wa_zib_contabil-waers_g   = ''.
        wa_zib_contabil-dmbe3     = 0.
      endif.


      wa_zib_contabil-vbund       = it_zglt036-vbund .
      wa_zib_contabil-tax_code    = it_zglt036-tax_code.
*      wa_zib_contabil-zuonr       = it_zglt036-zuonr. " Rubenilson Pereira - 24.07.25 - #185993
      wa_zib_contabil-hbkid       = it_zglt036-hbkid.
      wa_zib_contabil-bvtyp       = it_zglt036-bvtyp.
      wa_zib_contabil-matnr_fi    = it_zglt036-matnr_fi.

      if it_zglt035-st_ap_fiscal ne 'X'.
        wa_zib_contabil-matnr       = it_zglt036-matnr.
      else.
        clear  wa_zib_contabil-matnr.
      endif.

      wa_zib_contabil-rg_atualizado	=	'N'.
      wa_zib_contabil-valut       = it_zglt036-valut.
      wa_zib_contabil-quantity    = it_zglt036-quantity.
      wa_zib_contabil-base_uom    = it_zglt036-base_uom.
      wa_zib_contabil-esrnr       = it_zglt036-esrnr.
      wa_zib_contabil-esrre       = it_zglt036-esrre.

      if wa_zib_contabil-matnr_fi is not initial
         and wa_zib_contabil-quantity is not initial
         and wa_zib_contabil-base_uom is initial.
        read table it_mara into data(ws_mara) with key matnr = wa_zib_contabil-matnr_fi.
        if sy-subrc eq 0.
          wa_zib_contabil-base_uom = ws_mara-meins.
        endif.
      endif.


      append wa_zib_contabil to it_zib_contabil.
*      INSERT INTO  ZIB_CONTABIL VALUES WA_ZIB_CONTABIL.
*      IF SY-SUBRC NE 0.
*        ROLLBACK WORK.
*      ELSE.
*        COMMIT WORK.
*      ENDIF.
      clear:  wa_zib_contabil, ws_mara.
    endloop.
    modify zib_contabil from table it_zib_contabil.
    commit work.
  endloop.

  perform zib_contabil_reversao. "130130 - CS2023000969 Gisele Follmann PSA


*"130130 - CS2023000969 Gisele Follmann PSA / Comentado.
*  " Checar se há estorno mês seguinte de provisão
*  LOOP AT it_zglt035 .
*    BREAK-POINT.
*    IF it_zglt035-prov_est = 'X'. " Gera Estorno mês seguinte
*      CONCATENATE 'ZGL17' it_zglt035-doc_lcto it_zglt035-budat+0(4) INTO vobj_key .
*      wl_loop = 'X'.
*      WHILE wl_loop = 'X'.
*        WAIT UP TO 5 SECONDS. " espera
*        SELECT SINGLE *
*        FROM zib_contabil_err
*        INTO wa_zib_contabil_err
*        WHERE obj_key LIKE vobj_key.
*        IF sy-subrc = 0. " Houve erro
*          wl_loop = ''. " sair do loop
*          CONTINUE.
*        ENDIF.
*
*        SELECT SINGLE *
*        FROM zib_contabil_chv
*        INTO wa_zib_contabil_chv
*        WHERE obj_key LIKE vobj_key
*        AND   bukrs = it_zglt035-bukrs.
*
*        IF sy-subrc = 0. " sucesso
*          vbudat = it_zglt035-budat.
*
*          IF vbudat+4(2) = '12'.
*            vano = vbudat+0(4).
*            ADD 1 TO vano.
*            sano = vano.
*            CONCATENATE sano '0101' INTO vbudat2.
*          ELSE.
*            vmes = vbudat+4(2).
*            ADD 1 TO vmes.
*            smes = vmes.
*            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*              EXPORTING
*                input  = smes
*              IMPORTING
*                output = smes.
*            CONCATENATE vbudat+0(4) smes '01' INTO vbudat2.
*          ENDIF.
*
*          " SHDB
*          CONCATENATE vbudat2+6(2) '.' vbudat2+4(2) '.' vbudat2+0(4) INTO vdata.
*          REFRESH ti_bdcdata.
*          PERFORM f_bdc_data USING:
*                'SAPMF05A'  '0105'  'X'  ''                 ' ',
*                ''          ''      ''   'BDC_OKCODE'        '/00',
*                ''          ''      ''   'RF05A-BELNS'      wa_zib_contabil_chv-belnr,
*                ''          ''      ''   'BKPF-BUKRS'       wa_zib_contabil_chv-bukrs,
*                ''          ''      ''   'RF05A-GJAHS'      vbudat+0(4),
*                ''          ''      ''   'UF05A-STGRD'      '02',
*                ''          ''      ''   'BSIS-BUDAT'       vdata,
*                'SAPMF05A'  '0105'  'X'  ''                 ' ',
*                ''          ''      ''   'BDC_OKCODE'        '=BU'.
*
*          CLEAR p_erro.
*          CONCATENATE 'ZGL17' it_zglt035-doc_lcto it_zglt035-budat+0(4) INTO vobj_key .
*          PERFORM zf_call_transaction USING 'FB08' CHANGING p_erro.
*
*          wl_loop = ''. " sai do loop com erro ou não
*        ENDIF.
*
*      ENDWHILE.
*    ENDIF.
*
*  ENDLOOP.
**"130130 - CS2023000969 Gisele Follmann PSA / Comentado.
endform.                  " ORGANIZA_DADOS


*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_program   programa
*      -->P_dynpro    tela
*      -->P_start     define a tela
*      -->P_fnam      nome do campo ou comando
*      -->P_fval      conteúdo do campo ou comando
*----------------------------------------------------------------------*
form f_bdc_data  using p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  clear wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  append wa_bdcdata to ti_bdcdata.

endform.                    " F_BDC_DATA


*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*----------------------------------------------------------------------*
form zf_call_transaction using p_trans changing p_erro.
  constants: c_msgid like it_msg-msgid value 'F5',
             c_msgnr like it_msg-msgnr value '312',
             c_msgne like it_msg-msgnr value '539'.

  data: wl_cont     type sy-tabix.

  refresh: it_msg, it_zib_contabil_err.
  clear it_zib_contabil_err.

  wl_mode = 'E'.
  if p_trans = 'F-53'.
    call transaction p_trans using ti_bdcdata
      mode wl_mode
      messages into it_msg
      update 'S'.
  else.
    call transaction p_trans using ti_bdcdata
          mode wl_mode
          messages into it_msg.
  endif.
  clear: wl_cont.

  loop at it_msg where msgtyp eq 'E'.
    add 1 to wl_cont.
  endloop.
  if wl_cont  gt 0.
    clear wl_cont.
    delete from zib_contabil_err where obj_key  = vobj_key.
    loop at it_msg where msgtyp eq 'E'.
      add 1 to wl_cont.
      clear: wl_message.
      call function 'CUTC_GET_MESSAGE'
        exporting
          msg_type       = it_msg-msgtyp
          msg_id         = it_msg-msgid
          msg_no         = sy-msgno
          msg_arg1       = sy-msgv1
          msg_arg2       = sy-msgv2
          msg_arg3       = sy-msgv3
          msg_arg4       = sy-msgv4
        importing
          raw_message    = wl_message
        exceptions
          msg_not_found  = 1
          internal_error = 2
          others         = 3.

      if ( sy-subrc ne 0 ).
        wl_message = 'Erro na mensagem do BATCH-INPUT'.
      endif.

      it_zib_contabil_err-obj_key            = vobj_key.
      it_zib_contabil_err-nr_item            = wl_cont.
      it_zib_contabil_err-interface          = ''.
      it_zib_contabil_err-dt_atualizacao     = sy-datum.
      it_zib_contabil_err-hr_atualizacao     = sy-uzeit.
      it_zib_contabil_err-type               = it_msg-msgtyp.
      it_zib_contabil_err-id                 = it_msg-msgid.
      it_zib_contabil_err-num                = sy-msgno.
      it_zib_contabil_err-message            = wl_message.
      it_zib_contabil_err-message_v1         = it_msg-msgv1.
      it_zib_contabil_err-message_v2         = it_msg-msgv2.
      it_zib_contabil_err-message_v3         = it_msg-msgv3.
      it_zib_contabil_err-message_v4         = it_msg-msgv4.

      append it_zib_contabil_err.
      clear it_zib_contabil_err.

    endloop.

    modify zib_contabil_err from table it_zib_contabil_err.
  endif.

  read table it_msg with key msgtyp = 'A'.
  if sy-subrc = 0.
    p_erro = 'X'.
  else.
    read table it_msg with key msgtyp = 'E'.
    if sy-subrc = 0.
      p_erro = 'X'.
    endif.
  endif.

  clear wg_documento.
  if p_trans = 'FBRA'.
    read table it_msg with key msgid = c_msgid
                           msgnr = c_msgne
                           msgtyp = 'S'.
  else.
    read table it_msg with key msgid = c_msgid
                               msgnr = c_msgnr
                               msgtyp = 'S'.
  endif.
  if sy-subrc = 0.
    move it_msg-msgv1 to wg_documento.
  endif.

  if  wg_documento is initial.
    p_erro = 'X'.
  else.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wg_documento
      importing
        output = wg_documento.
  endif.


endform.                    "ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*& Form zib_contabil_normal
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form zib_contabil_reversao. "130130 - CS2023000969 Gisele Follmann PSA

  data: wl_loop(1),
        vbudat(10),
        vano       type i,
        sano(4),
        vbudat2    type bsis-budat,
        vmes       type i,
        smes(2).

  loop at it_zglt035 .

    if it_zglt035-prov_est = 'X'. " Gera Reversão mês seguinte
      concatenate 'ZGL17' it_zglt035-doc_lcto it_zglt035-budat+0(4) into vobj_key . "130130 - CS2023000969 Gisele Follmann PSA
      wl_loop = 'X'.
      while wl_loop = 'X'.

        wait up to 5 seconds. " espera
        select single *
        from zib_contabil_err
        into wa_zib_contabil_err
        where obj_key = vobj_key.
        if sy-subrc = 0. " Houve erro
          wl_loop = ''. " sair do loop
          continue.
        endif.

        select single *
        from zib_contabil_chv
        into wa_zib_contabil_chv
        where obj_key = vobj_key
        and   bukrs = it_zglt035-bukrs.

        if sy-subrc = 0. " sucesso

          select * from zib_contabil
              where obj_key like @vobj_key
              and   bukrs = @it_zglt035-bukrs
                into table @data(lt_zib_contabil).

          data: _new_obj_key type zib_contabil_err-obj_key.
          clear: _new_obj_key.
          _new_obj_key = |{ vobj_key }R| . "130130 - CS2023000969 Gisele Follmann PSA

          loop at lt_zib_contabil assigning field-symbol(<_search>).

            clear: <_search>-obj_key.

            <_search>-obj_key = _new_obj_key.

            select single * from tbsl where bschl = @<_search>-bschl into @data(lr_tbsl).

            clear: <_search>-bschl.

            <_search>-bschl = lr_tbsl-stbsl.

            clear: <_search>-rg_atualizado.

            <_search>-rg_atualizado = 'N'.

            clear: vbudat,vbudat2,vano,sano,vmes,smes.

            data(temp_budat) = <_search>-budat.
            condense temp_budat no-gaps.
            replace all occurences of '.' in temp_budat with ''.

            clear: <_search>-budat.

            data: v_mes type i,
                  v_ano type i.

            v_mes = temp_budat+2(2).
            v_ano = temp_budat+4(4).

            if v_mes = '12'.
              <_search>-budat = |01.01.{ v_ano + 1 }|.
            else.
              if v_mes < 9.
                smes = |0{ v_mes + 1 }|.
              else.
                smes = v_mes + 1.
              endif.
              <_search>-budat = |01.{ smes }.{ v_ano }|.
            endif.

            <_search>-gjahr = <_search>-budat+6(4). "// 02-01-2024 WBARBOSA BUG-162759

            clear: <_search>-monat.

            <_search>-monat = <_search>-budat+3(2).

          endloop.

          "Grava ZibContabil
          modify zib_contabil from table lt_zib_contabil.
          commit work.

          wl_loop = ''. " sai do loop com erro ou não

        endif.
      endwhile.
    endif.

  endloop.

endform.
