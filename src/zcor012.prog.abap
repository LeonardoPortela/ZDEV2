************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 09.11.2012                                          *
* Objetivo    ...: Apropriação de Custo de Frete                       *
* Transação   ...: ZCO0017                                             *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 09.11.2012   Antonio Luiz  Criação                       DEVK925312  *
************************************************************************

report  zcor012.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
type-pools: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
tables: bsis.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
types:

  " Contabilidade financ.: índice secundário p/contas do Razão
  begin of  ty_bsis,
    bukrs            type bsis-bukrs,
    hkont            type bsis-hkont,
    gjahr            type bsis-gjahr,
    monat            type bsis-monat,
    budat            type bsis-budat,
    gsber            type bsis-gsber,
    xblnr            type bsis-xblnr,
    exti1            type vttk-exti1,
    zuonr            type bsis-zuonr,
    blart            type bsis-blart,
    shkzg            type bsis-shkzg,
    sgtxt            type bsis-sgtxt,
    dmbtr            type bsis-dmbtr,
    dmbe2            type bsis-dmbe2,
    belnr            type bsis-belnr,
    buzei            type bsis-buzei,
    xm_mesf(20)      type c,
    xm_frft(10)      type c,
    obj_key          type zpfe_lote_item-obj_key,
    tknum            type zlest0032-tknum,
    message          type zib_contabil_err-message,
    tknum_we         type zlest0032-tknum,
    estornado_we     type c,
    est_diff_comp_we type c,
    ebeln_we         type mseg-ebeln,
    lfbnr_we         type mseg-lfbnr,
  end of ty_bsis,

  " Cabeçalho do documento contábil
  begin of ty_bkpf,
    bukrs       type bkpf-bukrs	,
    belnr       type bkpf-belnr,
    gjahr       type bkpf-gjahr	,
    blart       type bkpf-blart	,
    awkey       type bkpf-awkey,
    tcode       type bkpf-tcode,
    stblg       type bkpf-stblg,
    stjah       type bkpf-stjah,
    bldat       type bkpf-bldat,
    awtyp       type bkpf-awtyp,
    bktxt       type bkpf-bktxt,
    xm_frft(10) type c,
    mblnr       type mseg-mblnr,
    mjahr       type mseg-mjahr,
  end of ty_bkpf,

  begin of ty_zlest0032,
    tknum type zlest0032-tknum,
    belnr type zlest0032-belnr,
  end of ty_zlest0032,

  begin of ty_xdoctrans,
    tknum       type zlest0032-tknum,
    xm_mesf(20) type c,
    belnr       type ekbe-belnr,
  end of ty_xdoctrans,

  begin of ty_vttp,
    tknum type vttp-tknum,
    vbeln type vttp-vbeln,
  end of ty_vttp,

  begin of ty_lips,
    vbeln type lips-vbeln,
    matnr type lips-matnr,
    werks type lips-werks,
    vgbel type lips-vgbel,
  end of ty_lips,

  begin of ty_likp,
    vbeln type likp-vbeln,
    kunnr type likp-kunnr,
    vkorg type likp-vkorg,
    tcode type likp-tcode,
    btgew type likp-btgew,
    vbtyp type likp-vbtyp,
  end of ty_likp,

  begin of ty_ekko,
    ebeln type ekko-ebeln,
    bukrs type ekko-bukrs,
  end of ty_ekko,

  begin of ty_vttk,
    tknum type vttk-tknum,
    shtyp type vttk-shtyp,

  end of ty_vttk,

  begin of ty_tvtkt,
    shtyp type tvtkt-shtyp,
    bezei type tvtkt-bezei,
  end of ty_tvtkt,

  begin of ty_zcot0001,
    shtyp   type zcot0001-shtyp,
    tp_oper type zcot0001-tp_oper,
    saknr   type zcot0001-saknr,
    hkont   type zcot0001-hkont,
  end of ty_zcot0001,

  begin of ty_zcot0005,
    matnr type zcot0005-matnr	,
    werks type zcot0005-werks,
    kostl type zcot0005-kostl,
  end of ty_zcot0005,

  begin of ty_zcot0007,
    mandt     type zcot0007-mandt,
    bukrs     type zcot0007-bukrs,
    belnr     type zcot0007-belnr,
    buzei     type zcot0007-buzei,
    gjahr     type zcot0007-gjahr,
    tp_oper   type zcot0007-tp_oper,
    shtyp     type zcot0007-shtyp,
    budat     type zcot0007-budat,
    blart     type zcot0007-blart,
    xblnr     type zcot0007-xblnr,
    zuonr     type zcot0007-zuonr,
    belnr_f   type zcot0007-belnr_f,
    dmbtr     type zcot0007-dmbtr,
    dmbe2     type zcot0007-dmbe2,
    sgtxt     type zcot0007-sgtxt,
    matnr     type zcot0007-matnr,
    werks     type zcot0007-werks,
    doc_mr22  type zcot0007-doc_mr22,
    obj_key   type zcot0007-obj_key,
    dt_apropr type zcot0007-dt_apropr,
    vbeln     type zcot0007-vbeln,
    vbeln_v   type zcot0007-vbeln_v,
    ebeln     type zcot0007-ebeln,
    kostl     type zcot0007-kostl,
    cpudt     type zcot0007-cpudt,
    cputm     type zcot0007-cputm,
    status    type zcot0007-status,
  end of ty_zcot0007,

  begin of ty_vbfa,
    vbeln   type vbfa-vbeln,
    vbtyp_n type vbfa-vbtyp_n,
    vbtyp_v type vbfa-vbtyp_v,
    vbelv   type vbfa-vbelv,
  end of ty_vbfa,

  begin of ty_vbpa,
    vbeln type vbpa-vbeln,
    parvw type vbpa-parvw,
    lifnr type vbpa-lifnr,
    kunnr type vbpa-kunnr,
  end of ty_vbpa,

  begin of ty_zsdt_depara_depo,
    werks    type zsdt_depara_depo-werks,
    lifnr    type zsdt_depara_depo-lifnr,
    operacao type zsdt_depara_depo-operacao,
    werks_v  type zsdt_depara_depo-werks_v,
  end of ty_zsdt_depara_depo,

  begin of ty_zsdt_depara_cen,
    centrov_1   type zsdt_depara_cen-centrov_1,
    centro_real type zsdt_depara_cen-centro_real,
  end of ty_zsdt_depara_cen,

  begin of ty_mseg,
    vbelv type vbfa-vbelv,
    vbeln type vbfa-vbeln,
    mjahr type vbfa-mjahr,
    ebeln type mseg-ebeln,
  end of ty_mseg,

  begin of ty_makt,
    matnr type makt-matnr,
    maktx type makt-maktx,
  end of ty_makt,

  begin of ty_t001w,
    werks type  t001w-werks,
    name1 type  t001w-name1,
  end of ty_t001w,

  begin of ty_kna1,
    kunnr type kna1-kunnr,
    name1 type kna1-name1,
  end of ty_kna1,

  begin of ty_rbkp,
    belnr type rbkp-belnr,
    gjahr type rbkp-gjahr,
    stblg type rbkp-stblg,
  end of ty_rbkp,

  begin of ty_zib_contabil_chv,
    obj_key type zib_contabil_chv-obj_key,
    belnr   type zib_contabil_chv-belnr,
    bukrs   type zib_contabil_chv-bukrs,
    gjahr   type zib_contabil_chv-gjahr,
  end of ty_zib_contabil_chv,

  begin of ty_zib_contabil_err,
    obj_key type zib_contabil_err-obj_key,
    nr_item type zib_contabil_err-nr_item,
    message type zib_contabil_err-message,
  end of ty_zib_contabil_err,

  begin of ty_t134g,
    werks type t134g-werks,
    gsber type t134g-gsber,
  end of ty_t134g,

  begin of ty_saida,
    sel,
    icon(4)          type c,
    tp_oper(30)      type c,
    shtyp(30)        type c,
    belnr            type bsis-belnr,
    buzei            type bsis-buzei,
    budat            type bsis-budat,
    blart            type bsis-blart,
    xblnr            type bsis-xblnr,
    zuonr            type bsis-zuonr,
    awkey            type bkpf-awkey,
    dmbtr            type bsis-dmbtr,
    dmbe2            type bsis-dmbe2,
    sgtxt            type bsis-sgtxt,
    matnr            type lips-matnr,
    maktx            type makt-maktx,
    werks            type lips-werks,
    name1            type t001w-name1,
    vbeln            type vttp-vbeln,
    btgew            type likp-btgew,
    vbelv            type vbfa-vbelv,
    ebeln            type mseg-ebeln,
    gjahr            type bsis-gjahr,
    bukrs            type bsis-bukrs,
    bldat            type bkpf-bldat,
    message          type zib_contabil_err-message,
    kostl            type zcot0005-kostl,
    doc_mr22         type zcot0007-doc_mr22,
    vshdb(1)         type c,
    obj_key          type zib_contabil-obj_key,
    tknum_we         type zlest0032-tknum,
    estornado_we     type c,
    est_diff_comp_we type c,
    ebeln_we         type mseg-ebeln,
    lfbnr_we         type mseg-lfbnr,
  end of ty_saida.

types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

types: begin of ty_zib_contabil.
         include structure zib_contabil.
types:   mark type c,
       end of ty_zib_contabil.



constants   c_x(001)   type c value 'X'.
*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*


data: t_bdcdata           type bdcdata    occurs 0 with header line,
      t_bdcmsgcoll        type bdcmsgcoll occurs 0 with header line,

      it_bsis             type table of ty_bsis,
      it_bsis_2           type table of ty_bsis,
      it_bkpf             type table of ty_bkpf,
      it_bkpf_2           type table of ty_bkpf,
      it_bkpf_sf          type table of ty_bkpf,
      it_bkpf_we          type table of ty_bkpf,
      it_mseg_we          type table of mseg,
      it_ekbe_we          type table of ekbe,
      it_zlest0032        type table of ty_zlest0032,
      it_zpfe_lote_item   type table of zpfe_lote_item, "ALRS
      it_xdoctrans        type table of ty_xdoctrans,
      it_xdoctrans_2      type table of ty_xdoctrans,
      it_vttp             type table of ty_vttp,
      it_lips             type table of ty_lips,
      it_likp             type table of ty_likp,
      it_ekko             type table of ty_ekko,
      it_vttk             type table of ty_vttk,
      it_tvtkt            type table of ty_tvtkt,
      it_zcot0001         type table of ty_zcot0001,
      it_zcot0005         type table of ty_zcot0005,
      it_zcot0007         type table of zcot0007,
      it_zcot0007_sf      type table of zcot0007,
      it_zcot0009         type table of zcot0009,
      it_ekbe             type table of ekbe,
      it_essr             type table of essr,
      it_vbfa             type table of ty_vbfa,
      it_vbfa_aux         type table of ty_vbfa,
      it_vbpa             type table of ty_vbpa,
      it_vbpa_aux         type table of ty_vbpa,
      it_zsdt_depara_depo type table of ty_zsdt_depara_depo,
      it_zsdt_depara_cen  type table of ty_zsdt_depara_cen,
      it_mseg_aux         type table of ty_mseg,
      it_mseg             type table of ty_mseg,
      it_makt             type table of ty_makt,
      it_t001w            type table of ty_t001w,
      it_t001w_aux        type table of ty_t001w,
      it_kna1             type table of ty_kna1,
      it_rbkp             type table of ty_rbkp,
      it_zib_contabil_chv type table of ty_zib_contabil_chv,
      it_zib_contabil_err type table of ty_zib_contabil_err,
      it_t134g            type table of ty_t134g,
      it_saida            type table of ty_saida.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
data:
  wa_cont             type ref to cl_gui_custom_container,
  wa_alv              type ref to cl_gui_alv_grid,
  wa_layout           type lvc_s_layo,

  wa_bsis             type ty_bsis,
  wa_bkpf             type ty_bkpf,
  wa_bkpf_aux         type bkpf,
  wa_zlest0032        type ty_zlest0032,
  wa_xdoctrans        type ty_xdoctrans,
  wa_vttp             type ty_vttp,
  wa_lips             type ty_lips,
  wa_likp             type ty_likp,
  wa_ekko             type ty_ekko,
  wa_vttk             type ty_vttk,
  wa_tvtkt            type ty_tvtkt,
  wa_zcot0001         type ty_zcot0001,
  wa_zcot0005         type ty_zcot0005,
  wa_zcot0007         type ty_zcot0007,
  wl_zcot0007         type zcot0007,
  wa_zcot0009         type zcot0009,
  wa_zpfe_lote_item   type zpfe_lote_item,
  wa_vbfa             type ty_vbfa,
  wa_vbpa             type ty_vbpa,
  wa_zsdt_depara_depo type ty_zsdt_depara_depo,
  wa_depara           type zsdt_depara_depo,
  wa_zsdt_depara_cen  type ty_zsdt_depara_cen,
  wa_mseg             type ty_mseg,
  wa_mseg_aux         type ty_mseg,
  wa_makt             type ty_makt,
  wa_t001w            type ty_t001w,
  wa_kna1             type ty_kna1,
  wa_rbkp             type ty_rbkp,
  wa_zib_contabil     type ty_zib_contabil,
  wa_zib_contabil_chv type ty_zib_contabil_chv,
  wa_zib_contabil_err type ty_zib_contabil_err,
  wa_t134g            type ty_t134g,
  wa_saida            type ty_saida.



*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
data:
  it_fcat    type table of ty_estrutura,
  s_variant  type disvariant           , " Tabela Estrutura co
  t_top      type slis_t_listheader,
  xs_events  type slis_alv_event,
  events     type slis_t_event,
  gd_layout  type slis_layout_alv,
  t_print    type slis_print_alv,
  v_report   like sy-repid,
  t_sort     type slis_t_sortinfo_alv with header line,
  it_setleaf like table of setleaf initial size 0 with header line,
  estrutura  type table of ty_estrutura,
  vg_i       type i,
  vtabix     type sy-tabix,
  vshdb(1)   type c.

define mc_preenche_class.
  vg_i = vg_i + 1.
  clear t_sort.
  t_sort-spos      = vg_i.
  t_sort-fieldname = &1.
  t_sort-group     = &2.
  t_sort-up        = &3.
  t_sort-subtot    = &4.
  append t_sort.
end-of-definition.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

selection-screen: begin of block b1 with frame title text-001.
  select-options:
                   p_bukrs for bsis-bukrs , "OBLIGATORY,
                   p_gsber for bsis-gsber ,
                   p_budat for bsis-budat . "OBLIGATORY.
selection-screen: end of block b1.

at selection-screen output.
  if sy-batch = ''.
    if p_bukrs-low is initial .
      message 'Informe a Empresa' type 'I'.
      set cursor field 'P_BUKRS-LOW' .
    endif.

    if p_budat-low is initial .
      message 'Informe a Data de Lançamento' type 'I'.
      set cursor field 'P_BUDAT-LOW' .
    endif.
  endif.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
start-of-selection.
*
  if sy-batch = 'X'.
    refresh: p_bukrs, p_budat.
    p_bukrs-sign   = 'I'.
    p_bukrs-option = 'EQ'.
    p_bukrs-low = '0001'.
    append p_bukrs.

    p_bukrs-sign   = 'I'.
    p_bukrs-option = 'EQ'.
    p_bukrs-low = '0015'.
    append p_bukrs.

    p_bukrs-sign   = 'I'.
    p_bukrs-option = 'EQ'.
    p_bukrs-low = '0018'.
    append p_bukrs.

    p_budat-sign   = 'I'.
    p_budat-option = 'BT'.
    p_budat-low = sy-datum - 40.
    p_budat-high = sy-datum.
    append p_budat.

  endif.

  perform:  f_seleciona_dados, " Form seleciona dados
            f_saida. " Form de saida

  if sy-batch = 'X'.
    perform    f_grava.
  elseif  p_bukrs-low is not initial and p_budat-low is not initial .
    perform:  f_iniciar_variaves, " Cabeçalho
              f_imprime_dados.
  endif.


end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_iniciar_variaves .
  data:
    w_texto1(10),
    w_texto2(10),
    w_texto3(40),

    w_empresa_texto(40),
    w_centro_texto(40),
    w_per_texto(40),

    empresa             type c length 50,
    centro              type c length 50,
    periodo             type c length 50.


  v_report = sy-repid.

  w_texto3 = 'Apropriação de Custo de Frete'.
  perform f_construir_cabecalho using 'H' w_texto3.


  if p_bukrs is not initial.
    w_empresa_texto = 'Empresa:'.
    if ( p_bukrs-low is not initial ) and ( p_bukrs-high is not initial ).
      concatenate w_empresa_texto  p_bukrs-low 'á' p_bukrs-high into empresa separated by space.
    elseif ( p_bukrs-low is not initial ).
      concatenate w_empresa_texto p_bukrs-low  into empresa separated by space.
    else.
      concatenate w_empresa_texto 'Todas'  into empresa separated by space.
    endif.
    perform f_construir_cabecalho using 'S' empresa.
  endif.

  if p_gsber is not initial.
    w_centro_texto = 'Centro:'.
    if ( p_gsber-low is not initial ) and ( p_gsber-high is not initial ).
      concatenate w_centro_texto  p_gsber-low 'á' p_gsber-high into centro separated by space.
    elseif ( p_gsber-low is not initial ).
      concatenate w_centro_texto p_gsber-low  into centro separated by space.
    else.
      concatenate w_centro_texto 'Todas'  into centro separated by space.
    endif.
    perform f_construir_cabecalho using 'S' centro.
  endif.

  if ( not  p_budat is initial ).
    w_per_texto = 'Período :'.
    concatenate p_budat-low+6(2)   '.' p_budat-low+4(2)  '.' p_budat-low(4)  into w_texto1.
    concatenate p_budat-high+6(2)  '.' p_budat-high+4(2) '.' p_budat-high(4) into w_texto2.
    concatenate w_per_texto w_texto1 ' - ' w_texto2 into periodo   separated by space.
    perform f_construir_cabecalho using 'S' periodo .
  endif.

endform.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0362   text
*      -->P_EMPRESA  text
*----------------------------------------------------------------------*
form f_construir_cabecalho    using typ text.
  data: ls_line type slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  append ls_line to t_top.
endform.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_seleciona_dados .
  data: vtabix     type sy-tabix,
        vtknum     type vttk-tknum,
        p_data_val type datum.


  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = 'Preparando dados'.

  ranges: p_gjahr for  bsis-gjahr .
  p_gjahr-sign   = 'I'.
  p_gjahr-option = 'BT'.
  p_gjahr-low = p_budat-low+0(4).
  p_gjahr-high = p_budat-high+0(4).
  append p_gjahr.

  if sy-batch = 'X'.
    select bsis~bukrs bsis~hkont bsis~gjahr bsis~monat bsis~budat bsis~gsber bsis~xblnr bsis~xblnr bsis~zuonr bsis~blart bsis~shkzg bsis~sgtxt bsis~dmbtr bsis~dmbe2 bsis~belnr bsis~buzei
      from bsis
      into table it_bsis
      where bukrs	in  p_bukrs
      and   hkont eq  '0000114997'
      and   gjahr in p_gjahr
      and   budat in  p_budat
      and   gsber  in  p_gsber
      and   blart  ne 'FA'
      and   not exists ( select * from zcot0007 where bukrs = bsis~bukrs
                                                and   belnr = bsis~belnr
                                                and   buzei = bsis~buzei
                                                and   gjahr = bsis~gjahr
                                                and   budat = bsis~budat
                                                and   blart = bsis~blart ).

  else.
    select bsis~bukrs bsis~hkont bsis~gjahr bsis~monat bsis~budat bsis~gsber bsis~xblnr bsis~xblnr bsis~zuonr bsis~blart bsis~shkzg bsis~sgtxt bsis~dmbtr bsis~dmbe2 bsis~belnr bsis~buzei
      from bsis
      into table it_bsis
      where bukrs	in  p_bukrs
      and   hkont eq  '0000114997'
      and   gjahr in p_gjahr
      and   budat in  p_budat
      and   gsber  in  p_gsber
      and   blart  ne 'FA'.
  endif.


  check it_bsis is not initial.

  loop at it_bsis into wa_bsis.
    call function 'Z_RET_DT_AJUSTADA_FI_MM'
      exporting
        p_data_ent     = wa_bsis-budat
        p_bukrs        = wa_bsis-bukrs
        p_val_fi       = 'X'
        p_val_mm       = 'X'
      importing
        p_data_val     = p_data_val
      exceptions
        data_fi_mm_nao = 1
        others         = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      stop.
    endif.
    wa_bsis-budat = p_data_val.
    wa_bsis-monat = p_data_val+4(2).
    modify it_bsis  index sy-tabix  from wa_bsis  transporting budat monat.
    if not 'FR_FT_WR' cs wa_bsis-blart .
      wa_bsis-xm_mesf = wa_bsis-zuonr+3(14).
      if wa_bsis-xm_mesf is initial .
        wa_bsis-xm_frft = 'PROPRIO'.
      else.
        clear wa_bsis-xm_frft.
      endif.
      modify it_bsis  index sy-tabix  from wa_bsis  transporting xm_frft xm_mesf.
    endif.
  endloop.

  select   bukrs belnr gjahr blart awkey tcode stblg stjah bldat awtyp bktxt
    from bkpf
    into table it_bkpf
    for all entries in it_bsis
    where bukrs  = it_bsis-bukrs
    and belnr  = it_bsis-belnr
    and gjahr  = it_bsis-gjahr.

  sort it_bkpf by bukrs belnr gjahr .

  sort it_bsis by bukrs belnr gjahr .
  loop at it_bkpf into wa_bkpf.
    vtabix = sy-tabix.

    read table it_bsis into wa_bsis with key bukrs = wa_bkpf-bukrs
                                             belnr = wa_bkpf-belnr
                                             gjahr = wa_bkpf-gjahr binary search.
    if sy-subrc = 0.
      if wa_bsis-xm_frft = 'PROPRIO'.
        wa_bsis-xm_mesf = wa_bkpf-awkey.
        if  wa_bkpf-stblg is not initial and wa_bkpf-awkey+0(3) ne 'LES'.
          select single awkey
            into wa_bsis-xm_mesf
           from bkpf
            where bukrs  = wa_bkpf-bukrs
            and   belnr  = wa_bkpf-stblg
            and   gjahr  = wa_bkpf-stjah.
        endif.
        modify it_bsis  index sy-tabix  from wa_bsis  transporting xm_mesf.
      elseif 'FR_FT_WR' cs wa_bsis-blart.
        if wa_bkpf-awkey+0(1) = 'E'.
          wa_bkpf-xm_frft = wa_bkpf-awkey+1(10).
        elseif wa_bkpf-awkey+0(3) = 'ACD'.

          select single * into wa_bkpf_aux
            from bkpf
           where bukrs = wa_bkpf-bukrs
             and belnr = wa_bkpf-awkey+03(10)
             and gjahr = wa_bkpf-awkey+13(04).

          if wa_bkpf_aux-awkey+0(1) = 'E'.
            wa_bkpf-xm_frft = wa_bkpf_aux-awkey+1(10).
          else.
            wa_bkpf-xm_frft = wa_bkpf_aux-awkey+0(10).
          endif.
        else.
          if wa_bkpf-tcode = 'MIRO'.
            wa_bkpf-xm_frft = wa_bkpf-awkey+0(10).
          else.
            wa_bkpf-xm_frft = wa_bkpf-belnr. "ALRS 06/05/2016
          endif.
        endif.
        modify it_bkpf  index vtabix    from wa_bkpf  transporting xm_frft.
        wa_bsis-xm_frft = wa_bkpf-xm_frft.
        modify it_bsis  index sy-tabix  from wa_bsis  transporting xm_frft.
      elseif wa_bsis-blart = 'SF'. "TIP
        wa_bsis-obj_key = wa_bkpf-awkey.
        modify it_bsis  index sy-tabix  from wa_bsis  transporting obj_key.
      elseif wa_bsis-blart = 'WE'. "Contabilização pela VI
        vtknum = wa_bkpf-bktxt.
        vtknum = |{ vtknum alpha = in }|.
        wa_bsis-tknum_we = vtknum.
        modify it_bsis  index sy-tabix  from wa_bsis  transporting tknum_we.
      endif.
    endif.
  endloop.

  "Estorno da TIP IR013568
  it_bkpf_sf[] = it_bkpf[].
  delete it_bkpf_sf where tcode ne 'FB08' and blart ne 'SF'.
  sort it_bkpf_sf by bukrs stblg stjah.

  if it_bkpf_sf[] is not initial.
    select *
      from zcot0007
      into table it_zcot0007_sf
      for all entries in it_bkpf_sf
         where bukrs = it_bkpf_sf-bukrs
         and   belnr = it_bkpf_sf-stblg
         and   gjahr = it_bkpf_sf-stjah.
    sort it_zcot0007_sf by bukrs belnr gjahr.
  endif.

  it_bkpf_2[]  = it_bkpf[].
  delete it_bkpf_2 where xm_frft is initial.

  if it_bkpf_2[] is not initial.
    select *
    from zcot0009
    into table it_zcot0009
    for all entries in it_bkpf_2
    where bukrs  eq it_bkpf_2-bukrs
    and   belnr  eq it_bkpf_2-xm_frft.

    select tknum belnr
      from zlest0032
      into table it_zlest0032
      for all entries in it_bkpf_2
      where belnr  eq it_bkpf_2-xm_frft.

    select tknum
      from zlest0032
      into table it_xdoctrans
      for all entries in it_bkpf_2
      where belnr	eq it_bkpf_2-xm_frft.

    select tknum
       from zcot0009
       appending table it_xdoctrans
       for all entries in it_bkpf_2
       where bukrs  eq it_bkpf_2-bukrs
         and belnr  eq it_bkpf_2-xm_frft.

    "CS2020000503 "frete complementar
    select *
      from ekbe
      into table it_ekbe
      for all entries in it_bkpf_2
      where belnr  eq it_bkpf_2-xm_frft
      and   lfbnr  ne ' '
      and   lfpos = 0.

    if it_ekbe[] is not initial.
      select *
        from essr
        into table it_essr
        for all entries in it_ekbe
        where lblni = it_ekbe-lfbnr.
      if it_essr[] is not initial.
        sort: it_essr by lblni.
        loop at it_ekbe into data(w_ekbe).
          read table it_essr into data(w_essr) with key lblni = w_ekbe-lfbnr binary search.
          if sy-subrc = 0.
            wa_xdoctrans-tknum = w_essr-txz01+0(10).
            wa_xdoctrans-belnr = w_ekbe-belnr.
            append wa_xdoctrans to it_xdoctrans.
          endif.
        endloop.

      endif.
    endif.
    "CS2020000503

  endif.

  refresh: it_bsis_2, it_xdoctrans_2.

  it_bsis_2[] = it_bsis[].
  sort it_bsis_2 by xm_mesf.
  delete it_bsis_2 where xm_mesf is initial.
  delete it_bsis_2 where xm_frft eq 'PROPRIO'.
  if it_bsis_2[] is not initial.
    select tknum exti2
      from vttk
      into table it_xdoctrans_2
      for all entries in it_bsis_2
      where exti2 eq it_bsis_2-xm_mesf.
  endif.

  loop at it_xdoctrans_2 into wa_xdoctrans.
    append wa_xdoctrans to it_xdoctrans.
  endloop.

** Alteração solicitada em 03.12.2012
  refresh: it_bsis_2, it_xdoctrans_2.
  it_bsis_2[] = it_bsis[].
  sort it_bsis_2 by xm_mesf.
  delete it_bsis_2 where xblnr is initial.
  if it_bsis_2[] is not initial.
    select tknum  exti1
      from vttk
      into table it_xdoctrans_2
      for all entries in it_bsis_2
      where exti1 eq it_bsis_2-exti1.
  endif.

  loop at it_xdoctrans_2 into wa_xdoctrans.
    append wa_xdoctrans to it_xdoctrans.
  endloop.

** Alteração solicitada em 04.12.2017
  refresh: it_bsis_2, it_xdoctrans_2.
  it_bsis_2[] = it_bsis[].
  sort it_bsis_2 by obj_key.
  delete it_bsis_2 where obj_key is initial.
  if it_bsis_2[] is not initial.
    select *
      from zpfe_lote_item
      into table it_zpfe_lote_item
    for all entries in it_bsis_2
      where obj_key eq it_bsis_2-obj_key.

    if it_zpfe_lote_item[] is not initial.
      select tknum
        from vttk
        into table it_xdoctrans_2
        for all entries in it_zpfe_lote_item
        where tknum eq it_zpfe_lote_item-tknum.
    endif.
  endif.

  loop at it_xdoctrans_2 into wa_xdoctrans.
    append wa_xdoctrans to it_xdoctrans.
  endloop.
************************************************************
  "
** documentos "ME" sem referencia
  refresh: it_bsis_2, it_xdoctrans_2.
* próprio
  it_bsis_2[] = it_bsis[].
  sort it_bsis_2 by xm_mesf.
  delete it_bsis_2 where xm_frft ne 'PROPRIO'.
  delete it_bsis_2 where xm_mesf eq ' '.
  if it_bsis_2[] is not initial.
    select tknum obj_key_ped
     from zlest0032
     into table it_xdoctrans_2
     for all entries in it_bsis_2
     where obj_key_ped eq it_bsis_2-xm_mesf
     and   obj_key_ped ne ' '.

    select *
       from zcot0009 appending table it_zcot0009
        for all entries in it_bsis_2
      where bukrs  eq it_bsis_2-bukrs
        and belnr  eq it_bsis_2-belnr.

    select tknum
     from zcot0009 appending table it_xdoctrans_2
      for all entries in it_bsis_2
    where bukrs  eq it_bsis_2-bukrs
      and belnr  eq it_bsis_2-belnr.

  endif.

  loop at it_xdoctrans_2 into wa_xdoctrans.
    append wa_xdoctrans to it_xdoctrans.
  endloop.

** Contabilização pela VI 17.05.2019 - Ini
  refresh: it_bsis_2, it_xdoctrans_2.
  it_bsis_2[] = it_bsis[].
  sort it_bsis_2 by tknum_we.
  delete it_bsis_2 where tknum_we is initial.
  if it_bsis_2[] is not initial.
    "Caso de estorno depois de apropriação
    select *
      from zcot0007
      into table it_zcot0007
      for all entries in it_bsis_2
      where xblnr eq it_bsis_2-xblnr
      and   bukrs eq it_bsis_2-bukrs
      and   blart eq it_bsis_2-blart
      and   dmbtr gt 0.

    select tknum
      from vttk
      into table it_xdoctrans_2
      for all entries in it_bsis_2
      where tknum eq it_bsis_2-tknum_we.

    select *
      from zcot0009 appending table it_zcot0009
       for all entries in it_bsis_2
     where bukrs  eq it_bsis_2-bukrs
       and belnr  eq it_bsis_2-belnr.

    select tknum
      from zcot0009 appending table it_xdoctrans_2
       for all entries in it_bsis_2
     where bukrs  eq it_bsis_2-bukrs
       and belnr  eq it_bsis_2-belnr.

    "Identificar Estorno...
    clear: it_bkpf_we[], it_mseg_we[], it_ekbe_we[].

    loop at it_bsis_2 into data(wl_bsis_2).
      read table it_bkpf into data(wl_bkpf_aux)  with key bukrs = wl_bsis_2-bukrs
                                                          belnr = wl_bsis_2-belnr binary search.

      if ( sy-subrc eq 0 ) and ( wl_bkpf_aux-awtyp eq 'MKPF' ) and ( wl_bkpf_aux-awkey is not initial ).
        wl_bkpf_aux-mblnr = wl_bkpf_aux-awkey(10).
        wl_bkpf_aux-mjahr = wl_bkpf_aux-awkey+10(4).
        append wl_bkpf_aux to it_bkpf_we.
      endif.
    endloop.

    sort it_bkpf_we by bukrs belnr gjahr.

    if it_bkpf_we[] is not initial.
      select *
        from mseg into table it_mseg_we
         for all entries in it_bkpf_we
       where mblnr = it_bkpf_we-mblnr
         and mjahr = it_bkpf_we-mjahr.


      if it_mseg_we[] is not initial.
        select *
          from ekbe into table it_ekbe_we
           for all entries in it_mseg_we
         where ebeln = it_mseg_we-ebeln
           and lfbnr = it_mseg_we-lfbnr
           and vgabe = '1'.
      endif.
    endif.

    sort it_mseg_we  by mblnr mjahr.
    sort it_ekbe_we  by ebeln lfbnr shkzg.

    if it_ekbe_we[] is not initial.
      loop at it_bsis assigning field-symbol(<fs_bsis_we>) where blart = 'WE'.
        read table it_bkpf_we into data(wl_bkpf_we) with key bukrs = <fs_bsis_we>-bukrs
                                                             belnr = <fs_bsis_we>-belnr binary search.
        check sy-subrc eq 0.

        read table it_mseg_we into data(wl_mseg_we) with key mblnr = wl_bkpf_we-mblnr
                                                             mjahr = wl_bkpf_we-mjahr binary search.
        check sy-subrc eq 0.

        data(_shkzg_s) = abap_false.
        data(_shkzg_h) = abap_false.

        read table it_ekbe_we into data(wl_ekbe_we) with key ebeln = wl_mseg_we-ebeln
                                                             lfbnr = wl_mseg_we-lfbnr
                                                             shkzg = 'S' binary search.
        if sy-subrc = 0.
          _shkzg_s = abap_true.
        endif.

        read table it_ekbe_we into data(wl_ekbe_we2) with key ebeln = wl_mseg_we-ebeln
                                                             lfbnr = wl_mseg_we-lfbnr
                                                             shkzg = 'H' binary search.
        if sy-subrc = 0.
          _shkzg_h = abap_true.
        endif.


*        LOOP AT IT_EKBE_WE INTO DATA(WL_EKBE_WE) WHERE EBELN = WL_MSEG_WE-EBELN
*                                                   AND LFBNR = WL_MSEG_WE-LFBNR.
*          CASE WL_EKBE_WE-SHKZG.
*            WHEN 'S'.
*              _SHKZG_S = ABAP_TRUE.
*            WHEN 'H'.
*              _SHKZG_H = ABAP_TRUE.
*          ENDCASE.
*        ENDLOOP.

        if ( _shkzg_s eq abap_true ) and ( _shkzg_h eq abap_true ).
          <fs_bsis_we>-estornado_we = abap_true.
          <fs_bsis_we>-ebeln_we     = wl_mseg_we-ebeln.
          <fs_bsis_we>-lfbnr_we     = wl_mseg_we-lfbnr.
        endif.
      endloop.

    endif.

  endif.

  sort it_xdoctrans_2 by tknum.
  delete adjacent duplicates from it_xdoctrans_2 comparing tknum.

  loop at it_xdoctrans_2 into wa_xdoctrans.
    append wa_xdoctrans to it_xdoctrans.
  endloop.
** Contabilização pela VI 17.05.2019 - Fim



  if it_xdoctrans[] is not initial.
    select tknum vbeln
       from vttp
       into table it_vttp
       for all entries in it_xdoctrans
       where tknum  = it_xdoctrans-tknum.
  endif.

  if it_vttp[] is not initial.
    select  vbeln matnr werks vgbel
      from lips
      into table it_lips
      for all entries in it_vttp
      where vbeln eq it_vttp-vbeln.
  endif.

  if not it_lips[] is initial.
    select ebeln bukrs
      from ekko
      into table it_ekko
      for all entries in it_lips
      where ebeln = it_lips-vgbel.


    select centrov_1 centro_real
      from zsdt_depara_cen
      into table it_zsdt_depara_cen
      for all entries in it_lips
      where centrov_1	=	it_lips-werks.

    select matnr werks kostl
      from zcot0005
      into table it_zcot0005
      for all entries in it_lips
      where matnr = it_lips-matnr
      and   werks = it_lips-werks.

    select werks name1
      from t001w
      into table it_t001w
      for all entries in it_lips
      where werks = it_lips-werks.

    select matnr maktx
      from makt
      into table it_makt
      for all entries in it_lips
      where matnr eq it_lips-matnr
        and spras eq sy-langu.

    select  vbeln kunnr vkorg tcode btgew vbtyp
      from likp
      into table it_likp
      for all entries in it_vttp
      where vbeln eq it_vttp-vbeln.

    select kunnr name1
      from kna1
      into table it_kna1
      for all entries in it_likp
      where kunnr eq it_likp-kunnr.

    select tknum shtyp
      from vttk
      into table it_vttk
      for all entries in it_xdoctrans
      where tknum	=	it_xdoctrans-tknum.

    select shtyp bezei
      from tvtkt
      into table it_tvtkt
      for all entries in it_tvtkt
      where shtyp = it_tvtkt-shtyp
      and   spras	=	'PT'.

    select shtyp tp_oper saknr hkont
      from zcot0001
      into table it_zcot0001
      for all entries in it_vttk
      where shtyp	=	it_vttk-shtyp.

    select vbeln vbtyp_n vbtyp_v vbelv
      from vbfa
      into table it_vbfa
      for all entries in it_vttp
      where vbeln	=	it_vttp-vbeln
      and vbtyp_n	in ('J')
      and vbtyp_v	in ('C').

    select vbeln vbtyp_n vbtyp_v vbelv
      from vbfa
      into table it_vbfa_aux
      for all entries in it_vttp
      where vbeln	=	it_vttp-tknum
      and vbtyp_n	in ('8')
      and vbtyp_v	in ('7').

    append lines of it_vbfa_aux to it_vbfa.

  endif.

  data: rgparvw type range of parvw,
        waparvw like line of rgparvw.

  waparvw-sign   = 'I'.
  waparvw-option = 'EQ'.
  waparvw-high   = 'Z1'.
  waparvw-low    = 'Z1'.
  append waparvw to rgparvw.
  waparvw-high   = 'WE'.
  waparvw-low    = 'WE'.
  append waparvw to rgparvw.

  if it_vbfa[] is not initial.
    select vbeln parvw lifnr kunnr
      from vbpa
      into table it_vbpa
      for all entries in it_vbfa
      where vbeln eq  it_vbfa-vbelv
        and parvw in rgparvw.
  endif.

  if it_vbfa_aux[] is not initial.
    select vbeln parvw lifnr kunnr
      from vbpa
      appending table it_vbpa
      for all entries in it_vbfa_aux
      where vbeln eq  it_vbfa_aux-vbelv
        and parvw eq 'LF'.
  endif.

  "Privilegiar PARCEIRO Z1 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  data(it_vbpa_kunnr) = it_vbpa[].
  loop at it_vbpa_kunnr into data(wa_vbpa_kunnr).
    read table it_vbpa with key vbeln = wa_vbpa_kunnr-vbeln parvw = 'Z1' transporting no fields.
    if sy-subrc is initial.
      "Se achou Z1 apaga WE LF
      delete it_vbpa where vbeln eq wa_vbpa_kunnr-vbeln and parvw eq 'WE'.
      delete it_vbpa where vbeln eq wa_vbpa_kunnr-vbeln and parvw eq 'LF'.
    else.
      read table it_vbpa with key vbeln = wa_vbpa_kunnr-vbeln parvw = 'WE' transporting no fields.
      if sy-subrc is initial.
        "Se achou WE apaga LF
        delete it_vbpa where vbeln eq wa_vbpa_kunnr-vbeln and parvw eq 'LF'.
      endif.
    endif.
  endloop.
  clear: it_vbpa_kunnr, it_vbpa_kunnr[].
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  it_vbpa_kunnr = it_vbpa[].
  delete it_vbpa_kunnr where kunnr is initial.
  sort it_vbpa_kunnr by kunnr.
  delete adjacent duplicates from it_vbpa_kunnr comparing kunnr.

  if it_vbpa_kunnr is not initial.

    select * into table @data(lc_it_kna1)
      from kna1
       for all entries in @it_vbpa_kunnr
     where kunnr eq @it_vbpa_kunnr-kunnr.

    if sy-subrc is initial.
      select * into table @data(it_lfa1)
        from lfa1
         for all entries in @lc_it_kna1
       where stcd1 eq @lc_it_kna1-stcd1
         and stcd2 eq @lc_it_kna1-stcd2
         and stcd3 eq @lc_it_kna1-stcd3.
    endif.

    sort it_lfa1 by stcd1 stcd2 stcd3.

    loop at lc_it_kna1 into data(wa_kna1).
      read table it_lfa1 into data(wa_lfa1) with key stcd1 = wa_kna1-stcd1 stcd2 = wa_kna1-stcd2 stcd3 = wa_kna1-stcd3 binary search.
      if sy-subrc is not initial.
        continue.
      endif.
      loop at it_vbpa assigning field-symbol(<fs_vbpa>) where kunnr eq wa_kna1-kunnr.
        <fs_vbpa>-lifnr = wa_lfa1-lifnr.
      endloop.
    endloop.

  endif.

  if it_vbpa[] is not initial.
    select werks lifnr operacao werks_v
      from zsdt_depara_depo
      into table it_zsdt_depara_depo
      for all entries in it_vbpa
      where   lifnr  = it_vbpa-lifnr.
    if sy-subrc is initial.
      select werks name1
        from t001w
        into table it_t001w_aux
        for all entries in it_zsdt_depara_depo
        where werks = it_zsdt_depara_depo-werks_v.
    endif.
  endif.

  loop at it_t001w_aux into wa_t001w.
    append wa_t001w to it_t001w.
  endloop.
  refresh it_t001w_aux.
  if it_zsdt_depara_cen[] is not initial.
    select werks name1
    from t001w
    into table it_t001w_aux
    for all entries in it_zsdt_depara_cen
    where werks = it_zsdt_depara_cen-centro_real.
    loop at it_t001w_aux into wa_t001w.
      append wa_t001w to it_t001w.
    endloop.
  endif.

*---> 04/07/2023 - Migração S4 - WS
  sort it_t001w.
*<--- 04/07/2023 - Migração S4 - WS

  delete adjacent duplicates from it_t001w comparing all fields.

  if it_t001w[] is not initial.
    select werks gsber
        from t134g
        into table it_t134g
        for all entries in it_t001w
        where werks = it_t001w-werks.
  endif.

  if it_vttp[] is not initial.
    select vbfa~vbelv vbfa~vbeln vbfa~mjahr
      from vbfa
      into table it_mseg_aux
      for all entries in it_vttp
      where vbelv =	it_vttp-vbeln
      and vbtyp_n	=	'R'
      and vbtyp_v	=	'J'.
  endif.

*---> 04/07/2023 - Migração S4 - WS
  sort it_mseg_aux.
*<--- 04/07/2023 - Migração S4 - WS

  delete adjacent duplicates from it_mseg_aux comparing all fields.
  if it_mseg_aux[] is not initial.
    select mblnr mblnr mjahr ebeln
      from mseg
      into table it_mseg
      for all entries in it_mseg_aux
      where mblnr	=	it_mseg_aux-vbeln
      and   mjahr = it_mseg_aux-mjahr.
  endif.
endform.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_saida .
*
  data: vxdoctrans    type vttk-tknum,
        vtabix        type sy-tabix,
        vflag_bkpf(1) type c,
        vflag_err(1)  type c,
        vvkorg        type likp-vkorg,
        vdoc_mr22     type zcot0007-doc_mr22.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = 'Gerando relatório'.

  it_xdoctrans_2[] = it_xdoctrans[].
  delete it_xdoctrans_2 where belnr is initial.
  sort it_xdoctrans_2  by belnr.


  sort: it_bkpf      by bukrs belnr gjahr,
        it_zcot0007  by xblnr,
        it_vttk      by tknum,
        it_vttp      by tknum,
        it_zcot0001  by shtyp,
        it_xdoctrans by xm_mesf,
        it_zlest0032 by belnr,
        it_lips      by vbeln,
        it_likp      by vbeln,
        it_ekko      by ebeln,
        it_vbfa      by vbeln,
        it_vbpa      by vbeln,
        it_zpfe_lote_item by obj_key,
        it_zsdt_depara_depo by lifnr werks operacao,
        it_zsdt_depara_cen  by centrov_1,
        it_tvtkt     by shtyp,
        it_mseg_aux  by vbelv,
        it_mseg      by vbelv mjahr,
        it_bsis      by budat gjahr,
        it_makt      by matnr,
        it_t001w     by werks,
        it_kna1      by kunnr,
        it_zcot0005  by werks matnr,
        it_zcot0009  by bukrs belnr,
        it_t134g     by werks.

  loop at it_bsis into wa_bsis.
    vtabix = sy-tabix.
    vflag_bkpf = 'N'.
    read table it_bkpf into wa_bkpf with key  bukrs = wa_bsis-bukrs
                                              belnr = wa_bsis-belnr
                                              gjahr = wa_bsis-gjahr binary search.
    if sy-subrc = 0.
      wa_saida-bldat = wa_bkpf-bldat.
      vflag_bkpf = 'S'.

*      IF  WA_BKPF-STBLG IS NOT INITIAL.  "ALRS 09.10.2017
*        CONTINUE.
*      ENDIF.

    endif.
    clear vxdoctrans.
    clear wl_zcot0007.
    wa_saida-icon  = icon_status_open.
    if wa_bkpf-tcode = 'FB08' and ( wa_bkpf-blart = 'SF' or wa_bkpf-blart = 'AG' ).
      read table it_zcot0007_sf  into wl_zcot0007 with key bukrs = wa_bkpf-bukrs
                                                           belnr = wa_bkpf-stblg
                                                           gjahr = wa_bkpf-stjah binary search.
      if wa_zcot0001-tp_oper = '01'.
        loop at it_zcot0007  into wl_zcot0007 where bukrs = wa_bkpf-bukrs
                                              and   belnr = wa_bkpf-stblg
                                              and   gjahr = wa_bkpf-stjah.
          if wl_zcot0007-kostl is not initial.
            sy-subrc = 0.
            exit.
          endif.
        endloop.
      else.
        read table it_zcot0007_sf  into wl_zcot0007 with key bukrs = wa_bkpf-bukrs
                                                           belnr = wa_bkpf-stblg
                                                           gjahr = wa_bkpf-stjah binary search.
      endif.

      if sy-subrc = 0.
        select single shtyp tp_oper saknr hkont
             from zcot0001
             into wa_zcot0001
             where shtyp  = wl_zcot0007-shtyp.

        clear wa_lips.
        select single shtyp bezei
            from tvtkt
            into  wa_tvtkt
            where shtyp = wl_zcot0007-shtyp
            and   spras	=	sy-langu.
        concatenate wl_zcot0007-shtyp '-' wa_tvtkt-bezei into  wa_saida-shtyp.
        wa_saida-kostl = wl_zcot0007-kostl.

        select single shtyp tp_oper saknr hkont
           from zcot0001
           into wa_zcot0001
           where shtyp  = wl_zcot0007-shtyp.
        if sy-subrc = 0.
          if wa_zcot0001-tp_oper = '01'.
            wa_saida-tp_oper = '01 – Despesas de Vendas' .
          elseif wa_zcot0001-tp_oper = '03'.
            wa_saida-tp_oper = '03 – Estoque' .
          elseif wa_zcot0001-tp_oper = '04'.
            wa_saida-tp_oper = '04 – Estoque Porto' .
          else.
            wa_saida-tp_oper = wa_zcot0001-tp_oper.
            wa_saida-icon  = icon_led_red.
          endif.
        else.
          wa_saida-icon  = icon_led_red.
        endif.
      else.
        wa_saida-icon  = icon_led_red.
      endif.
    elseif wa_bsis-obj_key is not initial. "TIPCARD
      read table it_zpfe_lote_item into wa_zpfe_lote_item with key obj_key = wa_bsis-obj_key binary search.
      if sy-subrc = 0.
        vxdoctrans = wa_zpfe_lote_item-tknum.
        read table it_vttk into wa_vttk with key tknum = wa_zpfe_lote_item-tknum.

        read table it_tvtkt into wa_tvtkt with key shtyp = wa_vttk-shtyp binary search.
        concatenate wa_vttk-shtyp '-' wa_tvtkt-bezei into  wa_saida-shtyp.

        read table it_zcot0001 into wa_zcot0001 with key shtyp = wa_vttk-shtyp.
        if sy-subrc = 0.
          if wa_zcot0001-tp_oper = '01'.
            wa_saida-tp_oper = '01 – Despesas de Vendas' .
          elseif wa_zcot0001-tp_oper = '03'.
            wa_saida-tp_oper = '03 – Estoque' .
          elseif wa_zcot0001-tp_oper = '04'.
            wa_saida-tp_oper = '04 – Estoque Porto' .
          else.
            wa_saida-tp_oper = wa_zcot0001-tp_oper.
            wa_saida-icon  = icon_led_red.
          endif.
        else. " Alterãção solicitada em 03.12.2012
          wa_saida-icon  = icon_led_red.
        endif.
      endif.

    elseif wa_bsis-tknum_we is not initial.

      read table it_zcot0009 into wa_zcot0009 with key bukrs = wa_bsis-bukrs
                                                       belnr = wa_bsis-belnr.
      if sy-subrc = 0.
        wa_bsis-tknum_we = wa_zcot0009-tknum.
      endif.

      read table it_xdoctrans into wa_xdoctrans with key tknum = wa_bsis-tknum_we.
      if sy-subrc = 0.
        vxdoctrans = wa_xdoctrans-tknum.
        read table it_vttk into wa_vttk with key tknum = wa_xdoctrans-tknum.

        read table it_tvtkt into wa_tvtkt with key shtyp = wa_vttk-shtyp binary search.
        concatenate wa_vttk-shtyp '-' wa_tvtkt-bezei into  wa_saida-shtyp.

        read table it_zcot0001 into wa_zcot0001 with key shtyp = wa_vttk-shtyp.
        if sy-subrc = 0.
          if wa_zcot0001-tp_oper = '01'.
            wa_saida-tp_oper = '01 – Despesas de Vendas' .
          elseif wa_zcot0001-tp_oper = '03'.
            wa_saida-tp_oper = '03 – Estoque' .
          elseif wa_zcot0001-tp_oper = '04'.
            wa_saida-tp_oper = '04 – Estoque Porto' .
          else.
            wa_saida-tp_oper = wa_zcot0001-tp_oper.
            wa_saida-icon  = icon_led_red.
          endif.
        else.
          wa_saida-icon  = icon_led_red.
        endif.
      else.
        read table it_zcot0007  into wl_zcot0007 with key xblnr =  wa_bsis-xblnr binary search.
        select single shtyp tp_oper saknr hkont
             from zcot0001
             into wa_zcot0001
             where shtyp  = wl_zcot0007-shtyp.
        if wa_zcot0001-tp_oper = '01'.
          loop at it_zcot0007  into wl_zcot0007 where xblnr =  wa_bsis-xblnr.
            if wl_zcot0007-kostl is not initial.
              sy-subrc = 0.
              exit.
            endif.
          endloop.
        else.
          read table it_zcot0007  into wl_zcot0007 with key xblnr =  wa_bsis-xblnr binary search.
        endif.
        if sy-subrc = 0.
          clear wa_lips.
          select single shtyp bezei
              from tvtkt
              into  wa_tvtkt
              where shtyp = wl_zcot0007-shtyp
              and   spras	=	sy-langu.
          concatenate wl_zcot0007-shtyp '-' wa_tvtkt-bezei into  wa_saida-shtyp.
          wa_saida-kostl = wl_zcot0007-kostl.

          select single shtyp tp_oper saknr hkont
             from zcot0001
             into wa_zcot0001
             where shtyp  = wl_zcot0007-shtyp.
          if sy-subrc = 0.
            if wa_zcot0001-tp_oper = '01'.
              wa_saida-tp_oper = '01 – Despesas de Vendas' .
            elseif wa_zcot0001-tp_oper = '03'.
              wa_saida-tp_oper = '03 – Estoque' .
            elseif wa_zcot0001-tp_oper = '04'.
              wa_saida-tp_oper = '04 – Estoque Porto' .
            else.
              wa_saida-tp_oper = wa_zcot0001-tp_oper.
              wa_saida-icon  = icon_led_red.
            endif.
          else.
            wa_saida-icon  = icon_led_red.
          endif.
        else.
          wa_saida-icon  = icon_led_red.
        endif.
      endif.

      wa_saida-tknum_we         = wa_bsis-tknum_we.
      wa_saida-estornado_we     = wa_bsis-estornado_we.
      wa_saida-est_diff_comp_we = wa_bsis-est_diff_comp_we.
      wa_saida-ebeln_we         = wa_bsis-ebeln_we.
      wa_saida-lfbnr_we         = wa_bsis-lfbnr_we.

    elseif wa_bsis-xm_mesf is not initial.
      read table it_zcot0009 into wa_zcot0009 with key bukrs = wa_bsis-bukrs
                                                       belnr = wa_bsis-belnr.
      if sy-subrc = 0.
        wa_xdoctrans-tknum = wa_zcot0009-tknum.
      else.
        read table it_xdoctrans into wa_xdoctrans with key xm_mesf = wa_bsis-xm_mesf binary search.
      endif.
      if sy-subrc = 0.
        vxdoctrans = wa_xdoctrans-tknum.
        read table it_vttk into wa_vttk with key tknum = wa_xdoctrans-tknum.

        read table it_tvtkt into wa_tvtkt with key shtyp = wa_vttk-shtyp binary search.
        concatenate wa_vttk-shtyp '-' wa_tvtkt-bezei into  wa_saida-shtyp.

        read table it_zcot0001 into wa_zcot0001 with key shtyp = wa_vttk-shtyp.
        if sy-subrc = 0.
          if wa_zcot0001-tp_oper = '01'.
            wa_saida-tp_oper = '01 – Despesas de Vendas' .
          elseif wa_zcot0001-tp_oper = '03'.
            wa_saida-tp_oper = '03 – Estoque' .
          elseif wa_zcot0001-tp_oper = '04'.
            wa_saida-tp_oper = '04 – Estoque Porto' .
          else.
            wa_saida-tp_oper = wa_zcot0001-tp_oper.
            wa_saida-icon  = icon_led_red.
          endif.
        else. " Alterãção solicitada em 03.12.2012
          wa_saida-icon  = icon_led_red.
        endif.
      elseif wa_bsis-exti1 is not initial.
        read table it_xdoctrans into wa_xdoctrans with key xm_mesf = wa_bsis-exti1 binary search.
        if sy-subrc = 0.
          vxdoctrans = wa_xdoctrans-tknum.
          read table it_vttk into wa_vttk with key tknum = wa_xdoctrans-tknum.

          read table it_tvtkt into wa_tvtkt with key shtyp = wa_vttk-shtyp binary search.
          concatenate wa_vttk-shtyp '-' wa_tvtkt-bezei into  wa_saida-shtyp.

          read table it_zcot0001 into wa_zcot0001 with key shtyp = wa_vttk-shtyp.
          if sy-subrc = 0.
            if wa_zcot0001-tp_oper = '01'.
              wa_saida-tp_oper = '01 – Despesas de Vendas' .
            elseif wa_zcot0001-tp_oper = '03'.
              wa_saida-tp_oper = '03 – Estoque' .
            elseif wa_zcot0001-tp_oper = '04'.
              wa_saida-tp_oper = '04 – Estoque Porto' .
            else.
              wa_saida-tp_oper = wa_zcot0001-tp_oper.
              wa_saida-icon  = icon_led_red.
            endif.
          else.
            wa_saida-icon  = icon_led_red.
          endif.
        else.
          wa_saida-icon  = icon_led_red.
        endif.
      else.
        wa_saida-icon  = icon_led_red.
      endif.
    elseif wa_bkpf-xm_frft is not initial.
      read table it_zcot0009  into wa_zcot0009 with key bukrs = wa_bkpf-bukrs
                                                        belnr = wa_bkpf-xm_frft binary search. "ALRS Verifica antes
      if sy-subrc = 0.
        vxdoctrans = wa_zcot0009-tknum.
        read table it_vttk into wa_vttk with key tknum = wa_zcot0009-tknum.
      else.
        read table it_zlest0032 into wa_zlest0032 with key belnr = wa_bkpf-xm_frft binary search.
        if sy-subrc = 0.
          vxdoctrans = wa_zlest0032-tknum.
          read table it_vttk into wa_vttk with key tknum = wa_zlest0032-tknum.
        else. "frete complementar
          read table it_xdoctrans_2 into wa_xdoctrans with key belnr = wa_bkpf-xm_frft binary search.
          if sy-subrc = 0.
            vxdoctrans = wa_xdoctrans-tknum.
            read table it_vttk into wa_vttk with key tknum = wa_xdoctrans-tknum.
          endif.
        endif.
      endif.
      if sy-subrc = 0.
        read table it_tvtkt into wa_tvtkt with key shtyp = wa_vttk-shtyp binary search.
        concatenate wa_vttk-shtyp '-' wa_tvtkt-bezei into  wa_saida-shtyp.

        read table it_zcot0001 into wa_zcot0001 with key shtyp = wa_vttk-shtyp.
        if sy-subrc = 0.
          if wa_zcot0001-tp_oper = '01'.
            wa_saida-tp_oper = '01 – Despesas de Vendas' .
          elseif wa_zcot0001-tp_oper = '03'.
            wa_saida-tp_oper = '03 – Estoque' .
          elseif wa_zcot0001-tp_oper = '04'.
            wa_saida-tp_oper = '04 – Estoque Porto' .
          else.
            wa_saida-tp_oper = wa_zcot0001-tp_oper.
            wa_saida-icon  = icon_led_red.
          endif.
        else.
          wa_saida-icon  = icon_led_red.
        endif.
      else.
        wa_saida-icon  = icon_led_red.
      endif.

    else.
      wa_saida-icon  = icon_led_red.
    endif.
    wa_saida-belnr = wa_bsis-belnr.
    wa_saida-buzei = wa_bsis-buzei.
    wa_saida-budat = wa_bsis-budat.
    wa_saida-blart = wa_bsis-blart.
    wa_saida-xblnr = wa_bsis-xblnr.
    wa_saida-zuonr = wa_bsis-zuonr.
    if wa_bkpf-tcode = 'MIRO'.
      wa_saida-awkey = wa_bkpf-awkey+0(10).
    else.
      clear wa_saida-awkey.
    endif.
    if wa_bsis-shkzg = 'H'.
      wa_saida-dmbtr = wa_bsis-dmbtr * -1.
      wa_saida-dmbe2 = wa_bsis-dmbe2 * -1.
    else.
      wa_saida-dmbtr = wa_bsis-dmbtr.
      wa_saida-dmbe2 = wa_bsis-dmbe2.
    endif.
    wa_saida-sgtxt = wa_bsis-sgtxt.

    if vxdoctrans is not initial.
      read table it_vttp into wa_vttp with key tknum = vxdoctrans binary search.
      if sy-subrc = 0.
        read table it_lips into wa_lips with key vbeln = wa_vttp-vbeln binary search.
        wa_saida-matnr = wa_lips-matnr.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = wa_saida-matnr
          importing
            output = wa_saida-matnr.
        " CS2024000595 Corrigir transação ZCO0017 - PANF #146077 - inicio
**        READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_lips-matnr BINARY SEARCH.
**        wa_saida-maktx = wa_makt-maktx.
        " CS2024000595 Corrigir transação ZCO0017 - PANF #146077 - Fim
        if wa_saida-tp_oper+0(2) = '01'.
          read table it_zcot0005 into wa_zcot0005 with key werks = wa_lips-werks
                                                           matnr = wa_lips-matnr binary search.
          if sy-subrc = 0.
            wa_saida-kostl = wa_zcot0005-kostl .
          else.
            clear wa_saida-kostl.
            wa_saida-icon  = icon_message_error.
          endif.
        else.
          clear: wa_saida-kostl.
        endif.
        read table it_vbfa into wa_vbfa with key vbeln = wa_vttp-vbeln.
        if sy-subrc is initial.
          wa_saida-vbelv = wa_vbfa-vbelv.
        else.
          read table it_vbfa into wa_vbfa with key vbelv = wa_vttp-vbeln.
          if sy-subrc is initial.
            wa_saida-vbelv = wa_vbfa-vbelv.
          endif.
        endif.

        read table it_vbpa into wa_vbpa with key vbeln = wa_vbfa-vbelv.
        wa_saida-vbeln = wa_vttp-vbeln.
        read table it_mseg_aux into wa_mseg_aux with key vbelv  = wa_vttp-vbeln binary search.
        read table it_mseg     into wa_mseg     with key vbelv  = wa_mseg_aux-vbeln
                                                          mjahr  = wa_mseg_aux-mjahr binary search.
        wa_saida-ebeln = wa_mseg-ebeln.

      endif.
    elseif wl_zcot0007 is not initial.
      vflag_err = 'S'.
      wa_saida-matnr = wl_zcot0007-matnr.
      " CS2024000595 Corrigir transação ZCO0017 - PANF #146077 - inicio
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = wl_zcot0007-matnr
*        IMPORTING
*          output = wl_zcot0007-matnr.
*      SELECT SINGLE matnr maktx INTO wa_makt FROM makt WHERE spras = sy-langu AND matnr = wl_zcot0007-matnr.
*      wa_saida-maktx = wa_makt-maktx.
      " CS2024000595 Corrigir transação ZCO0017 - PANF #146077 - Fim
      wa_saida-vbeln = wl_zcot0007-vbeln.
      wa_saida-vbelv = wl_zcot0007-vbeln_v.
      wa_saida-ebeln = wl_zcot0007-ebeln.
      wa_saida-werks = wl_zcot0007-werks.
      select single werks name1
        from t001w
        into wa_t001w
        where werks = wa_saida-werks.
      wa_saida-name1 = wa_t001w-name1.
    else.
      wa_saida-icon  = icon_led_red.
      if  wa_bkpf-xm_frft is not initial and vflag_bkpf = 'S'.
        select single belnr gjahr stblg
           into wa_rbkp
           from rbkp
           where belnr  = wa_bkpf-xm_frft
           and gjahr    = wa_bkpf-gjahr.
        if sy-subrc = 0 and wa_rbkp-stblg is not initial. "ALRS 09.10.2017
*          WA_SAIDA-ICON = ICON_CHECKED. " para eliminar abaixo
        endif.
      endif.

      if ( wa_bsis-tknum_we is not initial ) and ( wa_bsis-estornado_we eq abap_true ).
        wa_saida-icon = icon_system_undo.
      endif.
    endif.
    vflag_err = 'N'.
    read table it_likp into wa_likp with key vbeln = wa_vttp-vbeln binary search.
    if sy-subrc = 0.
      wa_saida-btgew = wa_likp-btgew.
      clear vvkorg.
      if wa_likp-vbtyp = '7'.
        read table it_ekko into wa_ekko with key ebeln = wa_lips-vgbel binary search.
        if sy-subrc = 0.
          vvkorg = wa_ekko-bukrs.
        endif.
      else.
        vvkorg = wa_likp-vkorg.
      endif.

      if vvkorg ne wa_bsis-bukrs.
        wa_saida-icon  = icon_led_red.
        clear: wa_saida-werks, wa_saida-name1, wa_saida-vbeln,wa_saida-vbelv, wa_saida-ebeln, wa_saida-matnr,wa_saida-maktx.
        vflag_err = 'S'.
      endif.
    endif.
    if vflag_err = 'N'.
      if wa_saida-tp_oper+0(2) ne '04' and wa_saida-tp_oper is not initial.
        if wa_saida-tp_oper+0(2) = '03'.
          if wa_vttk-shtyp = 'Z021' or wa_vttk-shtyp = 'Z027' or wa_vttk-shtyp = 'Z031'.
            read table it_zsdt_depara_cen into wa_zsdt_depara_cen with key centrov_1 = wa_lips-werks binary search.
            if sy-subrc = 0.
              wa_saida-werks = wa_zsdt_depara_cen-centro_real.
              read table it_t001w into wa_t001w with key werks = wa_saida-werks binary search.
              if sy-subrc = 0.
                wa_saida-name1 = wa_t001w-name1.
              endif.
            else.
              wa_saida-message = | Centro Virtual { wa_lips-werks } não esta cadastradado DE-PARA|.
            endif.
          elseif wa_vttk-shtyp = 'Z025' or wa_vttk-shtyp = 'Z014'.
            wa_saida-werks = wa_lips-werks.
            read table it_t001w into wa_t001w with key werks = wa_lips-werks binary search.
            if sy-subrc = 0.
              wa_saida-name1 = wa_t001w-name1.
            endif.
          else.
            read table it_likp into wa_likp with key vbeln = wa_vttp-vbeln binary search.
            if sy-subrc = 0.
              wa_saida-btgew = wa_likp-btgew.
              if wa_likp-vkorg eq wa_bsis-bukrs.
                wa_saida-werks = wa_likp-kunnr+6(4).
                read table it_kna1 into wa_kna1 with key kunnr = wa_likp-kunnr binary search.
                if sy-subrc = 0.
                  wa_saida-name1 = wa_kna1-name1.
                endif.
              else.
                wa_saida-icon  = icon_led_red.
                clear: wa_saida-werks, wa_saida-name1, wa_saida-vbeln,wa_saida-vbelv, wa_saida-ebeln, wa_saida-matnr,wa_saida-maktx.
              endif.

            endif.
          endif.
        elseif wa_lips-werks is not initial.
          wa_saida-werks = wa_lips-werks.
          read table it_t001w into wa_t001w with key werks = wa_lips-werks binary search.
          if sy-subrc = 0.
            wa_saida-name1 = wa_t001w-name1.
          endif.
        endif.
      else.

*        READ TABLE IT_ZSDT_DEPARA_DEPO INTO WA_ZSDT_DEPARA_DEPO WITH KEY LIFNR = WA_VBPA-LIFNR
*                                                                         WERKS = WA_LIPS-WERKS BINARY SEARCH.
        data(_opera) = 'RF'.
        select single *
                     from vbak
                     into @data(w_vbak)
                     where vbeln = @wa_lips-vgbel.
        if  w_vbak-auart eq 'ZIND'.
          _opera = 'RI'.
        endif.
        call function 'Z_BUSCA_DEPARA'
          exporting
            i_werks          = wa_lips-werks
            i_lifnr          = wa_vbpa-lifnr
            i_opera          = _opera
          importing
            zsdt_depara_depo = wa_depara.
        move-corresponding wa_depara to wa_zsdt_depara_depo.

        if wa_zsdt_depara_depo-werks_v is not initial.
          if sy-subrc = 0.
            wa_saida-werks = wa_zsdt_depara_depo-werks_v.
            read table it_t001w into wa_t001w with key werks = wa_saida-werks binary search.
            if sy-subrc = 0.
              wa_saida-name1 = wa_t001w-name1.
            endif.
          else.
            wa_saida-message = | Centro Virtual { wa_lips-werks } não esta cadastradado DE-PARA|.
          endif.
        endif.
      endif.
    endif.

    " CS2024000595 Corrigir transação ZCO0017 - PANF #146077 - inicio
    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input  = wa_saida-matnr
      importing
        output = wa_saida-matnr.

    read table it_makt into wa_makt with key matnr = wa_saida-matnr binary search.
    wa_saida-maktx = wa_makt-maktx.

    call function 'CONVERSION_EXIT_MATN1_OUTPUT'
      exporting
        input  = wa_saida-matnr
      importing
        output = wa_saida-matnr.
    " CS2024000595 Corrigir transação ZCO0017 - PANF #146077 - Fim

    wa_saida-bukrs = wa_bsis-bukrs.
    wa_saida-gjahr = wa_bsis-gjahr.

**********ATUALIZA ICONES
    " Checar se tem BUZEI = ''
    select  single zib_contabil_err~obj_key zib_contabil_err~nr_item zib_contabil_err~message
    from zib_contabil_err
    inner join zcot0007
    on zib_contabil_err~obj_key = zcot0007~obj_key
    into wa_zib_contabil_err
    where zcot0007~bukrs = wa_bsis-bukrs
    and   zcot0007~belnr = wa_bsis-belnr
    and   zcot0007~buzei = ''
    and   zcot0007~gjahr = wa_bsis-gjahr
    and   zcot0007~obj_key ne ''.

    if sy-subrc = 0 .
      wa_saida-obj_key = wa_zib_contabil_err-obj_key.
      wa_saida-message = wa_zib_contabil_err-message.
      wa_saida-icon    = icon_incomplete. " reprocessar
    else.
      select  single zib_contabil_err~obj_key zib_contabil_err~nr_item zib_contabil_err~message
          from zib_contabil_err
          inner join zcot0007
          on zib_contabil_err~obj_key = zcot0007~obj_key
          into wa_zib_contabil_err
          where zcot0007~bukrs = wa_bsis-bukrs
          and   zcot0007~belnr = wa_bsis-belnr
          and   zcot0007~buzei = wa_bsis-buzei
          and   zcot0007~gjahr = wa_bsis-gjahr
          and   zcot0007~obj_key ne ''.

      if sy-subrc = 0 .
        wa_saida-obj_key = wa_zib_contabil_err-obj_key.
        wa_saida-message = wa_zib_contabil_err-message.
        wa_saida-icon    = icon_incomplete. " reprocessar
      else.
        select single zib_contabil_chv~obj_key
         from zib_contabil_chv
         inner join zcot0007
         on zib_contabil_chv~obj_key = zcot0007~obj_key
         into wa_zib_contabil_chv
         where zcot0007~bukrs = wa_bsis-bukrs
         and   zcot0007~belnr = wa_bsis-belnr
         and   zcot0007~buzei = ''
         and   zcot0007~gjahr = wa_bsis-gjahr
         and   zcot0007~obj_key ne ''.

        if sy-subrc = 0 .
          wa_saida-message = 'Atualizado com sucesso'.
          wa_saida-icon    = icon_checked.
        else.
          select single zib_contabil_chv~obj_key
                from zib_contabil_chv
                inner join zcot0007
                on zib_contabil_chv~obj_key = zcot0007~obj_key
                into wa_zib_contabil_chv
                where zcot0007~bukrs = wa_bsis-bukrs
                and   zcot0007~belnr = wa_bsis-belnr
                and   zcot0007~buzei = wa_bsis-buzei
                and   zcot0007~gjahr = wa_bsis-gjahr
                and   zcot0007~obj_key ne ''.
          if sy-subrc = 0.
            wa_saida-message = 'Atualizado com sucesso'.
            wa_saida-icon    = icon_checked.
          else.
            select single zib_contabil~obj_key
                   from zib_contabil
                   inner join zcot0007
                   on zib_contabil~obj_key = zcot0007~obj_key
                   into wa_zib_contabil
                   where zcot0007~bukrs = wa_bsis-bukrs
                   and   zcot0007~belnr = wa_bsis-belnr
                   and   zcot0007~buzei = ''
                   and   zcot0007~gjahr = wa_bsis-gjahr
                   and   rg_atualizado  = 'N'
                   and   zcot0007~obj_key ne ''.
            if sy-subrc = 0.
              wa_saida-icon    = icon_activity.
            else.
              select single zib_contabil~obj_key
               from zib_contabil
               inner join zcot0007
               on zib_contabil~obj_key = zcot0007~obj_key
               into wa_zib_contabil
               where zcot0007~bukrs = wa_bsis-bukrs
               and   zcot0007~belnr = wa_bsis-belnr
               and   zcot0007~buzei = wa_bsis-buzei
               and   zcot0007~gjahr = wa_bsis-gjahr
               and   zcot0007~obj_key ne ''
               and   rg_atualizado  = 'N'.
              if sy-subrc = 0.
                wa_saida-icon    = icon_activity.
              else.
                select single zib_contabil~obj_key
                     from zib_contabil
                     inner join zcot0007
                     on zib_contabil~obj_key = zcot0007~obj_key
                     into wa_zib_contabil-obj_key
                     where zcot0007~bukrs = wa_bsis-bukrs
                     and   zcot0007~belnr = wa_bsis-belnr
                     and   zcot0007~buzei = ''
                     and   zcot0007~gjahr = wa_bsis-gjahr
                     and   zcot0007~obj_key ne ''
                     and   rg_atualizado  = 'S'.
                if sy-subrc = 0.
                  wa_saida-obj_key = wa_zib_contabil-obj_key.
                  wa_saida-icon    = icon_replace.
                else.
                  select single zib_contabil~obj_key
                   from zib_contabil
                   inner join zcot0007
                   on zib_contabil~obj_key = zcot0007~obj_key
                   into wa_zib_contabil-obj_key
                   where zcot0007~bukrs = wa_bsis-bukrs
                   and   zcot0007~belnr = wa_bsis-belnr
                   and   zcot0007~buzei = wa_bsis-buzei
                   and   zcot0007~gjahr = wa_bsis-gjahr
                   and   zcot0007~obj_key ne ''
                   and   rg_atualizado  = 'S'.
                  if sy-subrc = 0.
                    wa_saida-obj_key = wa_zib_contabil-obj_key.
                    wa_saida-icon    = icon_replace.
                  endif.
                endif.
              endif.
            endif.
          endif.
        endif.
      endif.
    endif.
************************

    if wa_saida-icon    = icon_checked and
      ( wa_saida-tp_oper+0(2) = '03' or  wa_saida-tp_oper+0(2) = '04' ).
      clear vdoc_mr22.
      select single doc_mr22
      from zcot0007
      into  vdoc_mr22
      where bukrs  = wa_bsis-bukrs
      and   belnr  = wa_bsis-belnr
      and   buzei  = wa_bsis-buzei
      and   gjahr  = wa_bsis-gjahr
      and   budat  = wa_saida-budat.
      if sy-subrc = 0.
        if vdoc_mr22 is initial.
          wa_saida-icon = icon_green_light. "reprocessa apenas MR22
        endif.
      endif.
    endif.

    append wa_saida to it_saida.

    wa_bsis-tknum = vxdoctrans.
    modify it_bsis index vtabix from wa_bsis transporting tknum.
    clear: wa_saida  ,
        wa_bkpf      ,
        wa_vttk      ,
        wa_vttp      ,
        wa_zcot0001  ,
        wa_xdoctrans ,
        wa_zlest0032 ,
        wa_lips      ,
        wa_vbfa      ,
        wa_vbpa      ,
        wa_zsdt_depara_depo ,
        wa_zsdt_depara_cen  ,
        wa_tvtkt     ,
        wa_mseg_aux  ,
        wa_mseg      ,
        wa_bsis      ,
        wa_makt      ,
        wa_t001w     ,
        wa_zcot0005  .
  endloop.
  "ALRS 09.10.2017
  delete it_saida where icon = icon_checked.
  sort it_saida    by belnr buzei gjahr.

  loop at it_saida assigning field-symbol(<fs_saida_we>) where estornado_we eq abap_true
                                                           and ebeln_we     is not initial
                                                           and lfbnr_we     is not initial
                                                           and icon         eq icon_system_undo.
    data(_count) = 0.

    loop at it_saida into data(wl_saida_we) where estornado_we eq abap_true
                                              and ebeln_we     eq <fs_saida_we>-ebeln_we
                                              and lfbnr_we     eq <fs_saida_we>-lfbnr_we
                                              and icon         eq icon_system_undo.
      add 1 to _count.
    endloop.

    if _count <= 1.
      <fs_saida_we>-icon = icon_message_warning.
    endif.
  endloop.

endform.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_imprime_dados .

  perform f_definir_eventos.
  perform layout.
  perform f_alv.
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program       = v_report
      is_layout                = gd_layout
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      it_fieldcat              = it_fcat[]
      it_sort                  = t_sort[]
      i_save                   = 'X'
      it_events                = events
      is_print                 = t_print
*     IS_VARIANT               = VG_VARIANT
    tables
      t_outtab                 = it_saida.


endform.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
form set_pf_status using rt_extab type slis_t_extab.        "#EC CALLED
  describe table rt_extab. "Avoid Extended Check Warning
  set pf-status 'STANDARD_FULLSCREEN'.
endform. "Set_pf_status

*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_definir_eventos .
  perform f_carregar_eventos using:
                                    slis_ev_top_of_page  'XTOP_OF_PAGE'.

endform.                    " F_DEFINIR_EVENTOS

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
form xtop_of_page.                                          "#EC CALLED

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = t_top.

endform. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
form f_carregar_eventos using    name form.
  clear xs_events.
  xs_events-name = name.
  xs_events-form = form.
  append xs_events to events.
endform.                      " F_CARREGAR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

form f_alv .
  perform alv_preenche_cat using:
              'ICON'        ' '            '02'       ' '     ' '    ' ' , " Icon
              'TP_OPER'     text-011       '25'       ' '     ' '    ' ' , " Tp.Operação
              'SHTYP'       text-012       '25'       ' '     ' '    ' ' , " Tp.Transportes
              'BELNR'       text-002       '15'       'X'     ' '    ' ' , " Nro.Doc.
              'BLDAT'       text-023       '15'       ' '     ' '    ' ' , " Data Doc.
              'BUDAT'       text-003       '12'       ' '     ' '    ' ' , " Data Lcto
              'BLART'       text-004       '07'       ' '     ' '    ' ' , " Tp.Doc.
              'AWKEY'       text-007       '25'       'X'     ' '    ' ' , " Nro.MIRO
              'DMBTR'       text-008       '15'       ' '     ' '    ' ' , " Valor R$
              'DMBE2'       text-009       '15'       ' '     ' '    ' ' , " Valor US$
              'MATNR'       text-013       '15'       ' '     ' '    ' ' , " Material
              'MAKTX'       text-018       '30'       ' '     ' '    ' ' , " Descrição Material
              'WERKS'       text-014       '10'       ' '     ' '    ' ' , " Centro Apropr.
              'NAME1'       text-019       '30'       ' '     ' '    ' ' , " Descrição Centro.
              'VBELN'       text-015       '10'       'X'     ' '    ' ' , " Remessa
              'BTGEW'       text-022       '10'       ' '     ' '    ' ' , " Qtde.Remessa
              'VBELV'       text-016       '10'       'X'     ' '    ' ' , " Ordem Venda
              'EBELN'       text-017       '10'       'X'     ' '    ' ' , " Pedido
              'KOSTL'       text-020       '10'       'X'     ' '    ' ' , " Centro Custo
              'XBLNR'       text-005       '15'       ' '     ' '    ' ' , " Referência
              'ZUONR'       text-006       '15'       ' '     ' '    ' ' , " Atribuição
              'SGTXT'       text-010       '50'       ' '     ' '    ' ' , " Texto Contabil
              'MESSAGE'     text-021       '99'      ' '     ' '    ' ' .  " Log Erro
endform.                    " F_AL
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

form alv_preenche_cat  using   p_campo  type c
                               p_desc   type c
                               p_tam    type c
                               p_hot    type c
                               p_zero   type c
                               p_soma   type c.


  data: wl_fcat type ty_estrutura.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-seltext_s = p_desc.
  wl_fcat-seltext_m = p_desc.
  wl_fcat-seltext_l = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-do_sum    = p_soma.
  if p_campo = 'ICON'.
    wl_fcat-icon      = 'X'.
  endif.
  append wl_fcat to it_fcat.
endform.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
form user_command using r_ucomm     like sy-ucomm           "#EC CALLED
                        rs_selfield type slis_selfield.
  read table it_saida into wa_saida index rs_selfield-tabindex.
  case rs_selfield-fieldname.

    when 'AWKEY' .
      if wa_saida-awkey is not initial.
        set parameter id 'RBN' field wa_saida-awkey.
        set parameter id 'GJR' field wa_saida-gjahr .
        call transaction 'MIR4' and skip first screen.
      endif.
    when 'VBELN'.
      if wa_saida-vbeln is not initial.
        if wa_saida-shtyp+0(4) = 'Z021' or wa_saida-shtyp+0(4) = 'Z027'.
          set parameter id 'VL' field wa_saida-vbeln .
          call transaction 'VL33N' and skip first screen.
        else.
          set parameter id 'VL' field wa_saida-vbeln .
          call transaction 'VL03N' and skip first screen.
        endif.
      endif.
    when 'VBELV'.
      if wa_saida-vbelv is not initial.
        set parameter id 'AUN' field wa_saida-vbelv .
        call transaction 'VA03' and skip first screen.
      endif.
    when 'EBELN'.
      if wa_saida-ebeln is not initial.
        set parameter id 'BES' field wa_saida-ebeln.
        call transaction 'ME23N' and skip first screen.
      endif.
    when 'BELNR'.
      if wa_saida-belnr is not initial.
        set parameter id 'BLN' field wa_saida-belnr.
        set parameter id 'BUK' field wa_saida-bukrs.
        set parameter id 'GJR' field wa_saida-gjahr.
        call transaction 'FB03' and skip first screen.
      endif.
  endcase.
  if r_ucomm eq '&GERAR'.
    perform gerar_lancamentos.
    rs_selfield-refresh = 'X'.
  elseif r_ucomm eq '&REPROC'.
    perform reproc_lancamentos.
    rs_selfield-refresh = 'X'.
  elseif r_ucomm eq '&EXCLUIR' and rs_selfield-tabindex gt 0.
    read table it_saida into wa_saida index rs_selfield-tabindex.
    perform excluir_lancamentos.
    rs_selfield-refresh = 'X'.
  endif.
endform.                    "USER_COMMAND

*&---------------------------------------------------------------------*
*&  FORM F_DYNPRO
*&---------------------------------------------------------------------*
*   MONTA A ESTRUTURA PARA O BATCH INPUT
*----------------------------------------------------------------------*
form f_dynpro using p_dynbegin type any
                    p_fnam     type any
                    p_fval     type any.

  if p_dynbegin = c_x.
    t_bdcdata-dynbegin = c_x.
    t_bdcdata-program  = p_fnam.
    t_bdcdata-dynpro   = p_fval.
  else.
    t_bdcdata-fnam = p_fnam.
    t_bdcdata-fval = p_fval.
  endif.

  append t_bdcdata.
  clear  t_bdcdata.

endform.                    " f_dynpro

*&---------------------------------------------------------------------*
*&  Form  F_EXECUTA_SHDB
*&---------------------------------------------------------------------*
*   EXECUTA O SHDB
*----------------------------------------------------------------------*
form f_executa_shdb using p_code   type any
                          p_mode   type any
                          p_update type any.

  call transaction p_code
    using t_bdcdata
      mode   p_mode
      update p_update
    messages into t_bdcmsgcoll.
  if sy-subrc = 0.
    read table t_bdcmsgcoll with key msgtyp = 'S'
                                  msgnr = '019'.
    if sy-subrc eq 0.
      wa_saida-doc_mr22 = t_bdcmsgcoll-msgv1.
      modify it_saida index vtabix from wa_saida transporting doc_mr22.
      if wa_saida-icon = icon_green_light.
        wa_saida-icon = icon_checked.
        modify it_saida index vtabix from wa_saida transporting icon.
        update zcot0007 set  doc_mr22 = wa_saida-doc_mr22
                             cpudt    = sy-datum
                             cputm    = sy-uzeit
           where bukrs       = wa_saida-bukrs
           and belnr         = wa_saida-belnr
           and buzei         = wa_saida-buzei
           and gjahr         = wa_saida-gjahr
           and budat         = wa_saida-budat.
      endif.
    else.
      vshdb = 'E'.
      wa_saida-vshdb = 'E'.
      modify it_saida index vtabix from wa_saida transporting vshdb.
    endif.
  else.
    vshdb = 'E'.
    wa_saida-vshdb = 'E'.
    modify it_saida index vtabix from wa_saida transporting vshdb.
  endif.
  refresh t_bdcdata.
  refresh t_bdcmsgcoll.

endform.                    " F_EXECUTA_SHDB
*&---------------------------------------------------------------------*
*&      Form  GERAR_LANCAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form gerar_lancamentos .
  data: l_mode2    type c,
        vnum(12)   type c,
        vseq(12)   type p,
        vdatec(10) type c,
        vgjahr     type bsis-gjahr,
        vmonat     type bsis-monat,
        vstatus    type zcot0007-status,
        vdoc_mr22  type zcot0007-doc_mr22.

  data: vg_matnr type char18.


  l_mode2 = 'N'.
  sort :  it_bsis     by belnr buzei gjahr,
          it_saida    by belnr buzei gjahr,
          it_vttk     by tknum,
          it_t134g    by werks,
          it_zcot0001 by shtyp.


  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = 'Gerando Lançamentos '.

  " Grava Débito
  loop at it_bsis into wa_bsis.
    read table it_saida into wa_saida with key  belnr = wa_bsis-belnr
                                           buzei = wa_bsis-buzei
                                           gjahr = wa_bsis-gjahr binary search.
    vtabix = sy-tabix.
    if wa_saida-icon = icon_replace and sy-subrc = 0 and wa_saida-sel = 'X'. " gravou na zib e não gravou na ERR e CHV
      update zib_contabil set rg_atualizado = 'N'
      where obj_key = wa_saida-obj_key.
      "
      wa_saida-icon = icon_activity.
      modify it_saida index vtabix from wa_saida transporting icon.
      continue.
    endif.


    if ( wa_saida-icon  ne icon_status_open and wa_saida-icon  ne icon_incomplete and wa_saida-icon  ne icon_green_light ) or wa_saida-sel ne 'X' or sy-subrc ne 0.
      continue.
    endif.

    vshdb = 'S'.
*****************************************************************
* DEBITO
*****************************************************************
    if  wa_saida-tp_oper+0(2) = '01'.
      read table it_vttk into wa_vttk with key  tknum = wa_bsis-tknum binary search.
      if sy-subrc = 0.
        read table it_zcot0001 into wa_zcot0001 with key shtyp = wa_vttk-shtyp binary search.
        wa_zib_contabil-hkont     = wa_zcot0001-saknr.
      else.
        select single *
          from zcot0001
          into corresponding fields of wa_zcot0001
          where shtyp = wa_saida-shtyp+0(4).
        wa_zib_contabil-hkont     = wa_zcot0001-saknr.
      endif.
    elseif  '03_04' cs wa_saida-tp_oper+0(2).
      wa_zib_contabil-hkont     = 511000.
      "CHECA se já fez o SHDB e deu erro na ZIB_CONTABIL (faz somente a zib)
      clear vdoc_mr22.
      select single doc_mr22
      from zcot0007
      into  vdoc_mr22
      where bukrs  = wa_bsis-bukrs
      and   belnr  = wa_bsis-belnr
      and   buzei  = wa_bsis-buzei
      and   gjahr  = wa_bsis-gjahr
      and   budat  = wa_saida-budat.

      if  ( wa_saida-icon  eq icon_status_open or wa_saida-icon  eq icon_green_light ) and vdoc_mr22 is initial.
        perform f_monta_shdb.
        perform f_executa_shdb using 'MR22' l_mode2 'S'.
        if wa_saida-icon  eq icon_green_light or wa_saida-icon = icon_checked.
          "RS_SELFIELD-REFRESH = 'X'.
          continue.
        endif.
      endif.
    elseif wa_saida-icon  eq icon_status_open.
      vshdb = 'E'.
    endif.

    if vshdb = 'E'.
      wa_saida-icon = icon_message_critical_small.
      modify it_saida index vtabix from wa_saida transporting icon.
      continue.
    endif.

    call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr = '01'
        object      = 'ZID_CO'
      importing
        number      = vseq.

    vnum = vseq .
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = vnum
      importing
        output = vnum.

    vgjahr      = wa_bsis-budat+0(4).
    vmonat      = wa_bsis-budat+4(2).
    concatenate wa_bsis-budat+6(2) '.' wa_bsis-budat+4(2) '.' wa_bsis-budat+0(4) into vdatec.
    concatenate 'APF' vnum wa_bsis-gjahr  into wa_zib_contabil-obj_key.
    wa_zib_contabil-seqitem   = 1.
    concatenate 'FRE-' wa_bsis-belnr into wa_zib_contabil-xblnr.
    if wa_bsis-shkzg = 'S'.
      wa_zib_contabil-bschl     = '40'.
    else.
      wa_zib_contabil-bschl     = '50'.
    endif.
    if  wa_saida-tp_oper+0(2) = '01'.
      wa_zib_contabil-gsber     = wa_saida-werks.
    else.
      wa_zib_contabil-gsber     = wa_bsis-gsber.
    endif.
    if wa_zib_contabil-hkont     = 511000.
      read table it_t134g into wa_t134g with key werks = wa_saida-werks binary search.
      wa_zib_contabil-gsber     = wa_t134g-gsber.
    endif.


*>>>>>>>>>Inicio ajuste / USER story 167629 / aoenning.
    if wa_saida-matnr is not initial.
      clear: vg_matnr.
      vg_matnr = |{ wa_saida-matnr alpha = out }|. "Retira zero esquerda.
      vg_matnr = |{ vg_matnr alpha = in }|. "Add zero esquerda com tamonho 18 caractere.
      wa_zib_contabil-matnr_fi = vg_matnr.
      wa_zib_contabil-werks = wa_saida-werks.
    endif.
*>>>>>>>>>Fim ajuste / USER STORY 167629 / AOENNING.

    wa_zib_contabil-bukrs     = wa_bsis-bukrs.
    wa_zib_contabil-interface = '35'.
    wa_zib_contabil-bktxt     = ''.
    wa_zib_contabil-bldat     = vdatec.
    wa_zib_contabil-budat     = vdatec.
    wa_zib_contabil-gjahr     = vgjahr.
    wa_zib_contabil-monat     = vmonat.
    wa_zib_contabil-blart     = 'FA'.
    wa_zib_contabil-kostl     = wa_saida-kostl.
    wa_zib_contabil-wrbtr     = wa_bsis-dmbtr.
    wa_zib_contabil-waers     = 'BRL'.
    wa_zib_contabil-bupla     = wa_bsis-gsber.
    wa_zib_contabil-sgtxt     = 'Apropriação de Frete'.
    wa_zib_contabil-waers_i   = 'BRL'.
    wa_zib_contabil-dmbtr     = wa_bsis-dmbtr.
    wa_zib_contabil-waers_f   = 'USD'.
    wa_zib_contabil-dmbe2     = wa_bsis-dmbe2.
    wa_zib_contabil-rg_atualizado	=	'N'.

    insert into  zib_contabil values wa_zib_contabil.
    if sy-subrc ne 0.
      rollback work.
      wa_saida-vshdb = 'E'.
      modify it_saida index vtabix from wa_saida transporting vshdb.
    else.
      commit work.
    endif.
    clear wa_zib_contabil.
*****************************************************************
* CREDITO
*****************************************************************
    if wa_bsis-shkzg = 'S'.
      wa_zib_contabil-bschl     = '50'.
    else.
      wa_zib_contabil-bschl     = '40'.
    endif.
    concatenate 'APF' vnum wa_bsis-gjahr  into wa_zib_contabil-obj_key.
    concatenate 'FRE-' wa_bsis-belnr into wa_zib_contabil-xblnr.
    wa_zib_contabil-seqitem   = 2.

*>>>>>>>>>Inicio ajuste / USER STORY 167629 / AOENNING.
    if wa_saida-matnr is not initial.
      clear: vg_matnr.
      vg_matnr = |{ wa_saida-matnr alpha = out }|. "Retira zero esquerda.
      vg_matnr = |{ vg_matnr alpha = in }|. "Add zero esquerda com tamonho 18 caractere.
      wa_zib_contabil-matnr_fi = vg_matnr.
      wa_zib_contabil-werks = wa_saida-werks.
    endif.
*>>>>>>>>>Fim ajuste / USER STORY 167629 / AOENNING.



    wa_zib_contabil-gsber     = wa_bsis-gsber.
    wa_zib_contabil-bukrs     = wa_bsis-bukrs.
    wa_zib_contabil-interface   = '35'.
    wa_zib_contabil-bktxt     =  ' '.
    wa_zib_contabil-bldat     = vdatec.
    wa_zib_contabil-budat     = vdatec.
    wa_zib_contabil-gjahr     = vgjahr.
    wa_zib_contabil-monat     = vmonat.
    wa_zib_contabil-blart     = 'FA'.
    wa_zib_contabil-hkont     = 114997.
    wa_zib_contabil-kostl     =  ' '.
    wa_zib_contabil-wrbtr     = wa_bsis-dmbtr.
    wa_zib_contabil-waers     = 'BRL'.
    wa_zib_contabil-bupla     = wa_bsis-gsber.
    wa_zib_contabil-sgtxt     = 'Apropriação de Frete'.
    wa_zib_contabil-waers_i   = 'BRL'.
    wa_zib_contabil-dmbtr     = wa_bsis-dmbtr.
    wa_zib_contabil-waers_f   = 'USD'.
    wa_zib_contabil-dmbe2     = wa_bsis-dmbe2.
    wa_zib_contabil-rg_atualizado	=	'N'.
    insert into  zib_contabil values wa_zib_contabil.
    if sy-subrc ne 0.
      rollback work.
    else.
      commit work.
    endif.

    read table it_saida into wa_saida index vtabix.
    clear vstatus.
    select single status
       from zcot0007
       into  vstatus
       where bukrs  = wa_bsis-bukrs
       and   belnr  = wa_bsis-belnr
       and   buzei  = wa_bsis-buzei
       and   gjahr  = wa_bsis-gjahr
       and   status = 'P'.

    if vstatus = 'P'. " Já processado em job, apenas atualizar
      concatenate 'APF' vnum wa_bsis-gjahr  into wa_zcot0007-obj_key.
      update zcot0007 set obj_key  = wa_zcot0007-obj_key
                          doc_mr22 = wa_saida-doc_mr22
                          status   = ''
                          cpudt    = sy-datum
                          cputm    = sy-uzeit
      where bukrs       = wa_saida-bukrs
      and belnr         = wa_saida-belnr
      and buzei         = wa_saida-buzei
      and gjahr         = wa_saida-gjahr
      and budat         = wa_saida-budat.
      wa_saida-icon = icon_activity.
      modify it_saida index vtabix from wa_saida transporting icon.
    elseif wa_saida-obj_key is not initial.
      update zcot0007 set obj_key = wa_zib_contabil-obj_key
      where bukrs       = wa_saida-bukrs
      and belnr         = wa_saida-belnr
      and buzei         = wa_saida-buzei
      and gjahr         = wa_saida-gjahr
      and budat         = wa_saida-budat.
      wa_saida-icon = icon_activity.
      modify it_saida index vtabix from wa_saida transporting icon.
    else.
      wa_zcot0007-bukrs       = wa_saida-bukrs.
      wa_zcot0007-belnr       = wa_saida-belnr.
      wa_zcot0007-buzei       = wa_saida-buzei.
      wa_zcot0007-gjahr       = wa_saida-gjahr.
      wa_zcot0007-tp_oper     = wa_saida-tp_oper+0(2).
      wa_zcot0007-shtyp       = wa_saida-shtyp+0(4).
      wa_zcot0007-budat       = wa_saida-budat.
      wa_zcot0007-blart       = wa_saida-blart.
      wa_zcot0007-xblnr       = wa_saida-xblnr.
      wa_zcot0007-zuonr       = wa_saida-zuonr.
      wa_zcot0007-belnr_f     = wa_saida-awkey.
      wa_zcot0007-dmbtr       = wa_saida-dmbtr.
      wa_zcot0007-dmbe2       = wa_saida-dmbe2.
      wa_zcot0007-sgtxt       = wa_saida-sgtxt.
      wa_zcot0007-matnr       = wa_saida-matnr.
      wa_zcot0007-werks       = wa_saida-werks.
      wa_zcot0007-doc_mr22    = wa_saida-doc_mr22.
      concatenate 'APF' vnum wa_bsis-gjahr  into wa_zcot0007-obj_key.
      wa_zcot0007-dt_apropr   = wa_saida-budat.
      wa_zcot0007-vbeln       = wa_saida-vbeln.
      wa_zcot0007-vbeln_v     = wa_saida-vbelv.
      wa_zcot0007-ebeln       = wa_saida-ebeln.
      wa_zcot0007-kostl       = wa_saida-kostl.
      wa_zcot0007-cpudt       = sy-datum.
      wa_zcot0007-cputm       = sy-uzeit.
      insert into zcot0007 values wa_zcot0007.
      if sy-subrc = 0.
        wa_saida-icon = icon_activity.
        modify it_saida index vtabix from wa_saida transporting icon.
      else. " já gravou
        update zcot0007 set obj_key  = wa_zcot0007-obj_key
            where bukrs       = wa_saida-bukrs
            and belnr         = wa_saida-belnr
            and buzei         = wa_saida-buzei
            and gjahr         = wa_saida-gjahr
            and budat         = wa_saida-budat.
        if sy-subrc = 0.
          wa_saida-icon = icon_activity.
          modify it_saida index vtabix from wa_saida transporting icon.
        endif.
      endif.
    endif.
    clear wa_zib_contabil.
  endloop.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = 'Lançamentos Gerados'.
endform.                    " GERAR_LANCAMENTOS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_monta_shdb .
  data: vxblnr      type bsis-xblnr,
        vdata(10)   type c,
        vvalor1(16) type c,
        vvalor2(16) type c,
        vdmbtr      type bsis-dmbtr,
        vdmbe2      type bsis-dmbe2.

  concatenate wa_bsis-budat+6(2) wa_bsis-budat+4(2) wa_bsis-budat+0(4) into vdata separated by '.'.
  if wa_bsis-shkzg = 'H'.
    vdmbtr = wa_bsis-dmbtr * -1.
    vdmbe2 = wa_bsis-dmbe2 * -1.
    write: vdmbtr to vvalor1.
    write: vdmbe2 to vvalor2.
  else.
    write: wa_bsis-dmbtr to vvalor1.
    write: wa_bsis-dmbe2 to vvalor2.
  endif.
  concatenate 'FRE-' wa_bsis-belnr into vxblnr.

  perform f_dynpro using:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_CURSOR'                 'MR21HEAD-XBLNR',
                           ' ' 'BDC_OKCODE'                 '=ENTR',
                           ' ' 'MR21HEAD-BUDAT'             vdata,
                           ' ' 'MR21HEAD-BUKRS'             wa_bsis-bukrs,
                           ' ' 'MR21HEAD-WERKS'             wa_saida-werks,
                           ' ' 'MR21HEAD-XBLNR'             vxblnr,
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL_0250'.

  perform f_dynpro using:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_OKCODE'                 '=ENTR',
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'BDC_CURSOR'                 'CKI_MR22_0250-ZUUMB(01)',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL - OHNE BWKEY_025',
                           ' ' 'CKI_MR22_0250-MATNR(01)'    wa_saida-matnr,
                           ' ' 'CKI_MR22_0250-ZUUMB(01)'    vvalor1.

  perform f_dynpro using:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_OKCODE'                 '=TAB2',
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'BDC_CURSOR'                 'CKI_MR22_0250-MATNR(02)',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL - OHNE BWKEY_025'.

  perform f_dynpro using:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_OKCODE'                 '=ENTR',
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'BDC_CURSOR'                 'CKI_MR22_0250-ZUUMB(01)',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL - OHNE BWKEY_025',
                           ' ' 'CKI_MR22_0250-ZUUMB(01)'    vvalor2.

  perform f_dynpro using:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_OKCODE'                 '=TAB3',
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'BDC_CURSOR'                 'CKI_MR22_0250-MATNR(02)',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL - OHNE BWKEY_025'.

  perform f_dynpro using:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_OKCODE'                 '=CALC',
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'BDC_CURSOR'                 'CKI_MR22_0250-MATNR(01)',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL - OHNE BWKEY_025',
                           ' ' 'CKI_MR22_0250-SELKZ(01)'    'X'.

  perform f_dynpro using: 'X' 'SAPRCKM_MR22'                '0400',
                          ' ' 'BDC_CURSOR'                  '%#AUTOTEXT001',
                          ' ' 'BDC_OKCODE'                  '=ENTR',
                          ' ' 'DISPLAY-F_CURR1-SELKZ'       'X'.

  perform f_dynpro using: 'X' 'SAPRCKM_MR22'                '0201',
                          ' ' 'BDC_OKCODE'                  '=SAVE',
                          ' ' 'BDC_SUBSCR'                  'SAPRCKM_MR22                            0250MR22_SUB',
                          ' ' 'BDC_CURSOR'                  'CKI_MR22_0250-MATNR(02)',
                          ' ' 'MR21HEAD-SCREEN_VARIANT'     'LAGERMATERIAL - OHNE BWKEY_025'.
  " CS2024000595 Corrigir transação ZCO0017 - PANF #146077 - inicio
  if vdmbe2 is initial.

    perform f_dynpro using: 'X' 'SAPLSPO1'                '0100',
                            ' ' 'BDC_OKCODE'                  '=YES'.

  endif.
  " CS2024000595 Corrigir transação ZCO0017 - PANF #146077 - Fim

endform.                    " F_MONTA_SHDB
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form layout .
  gd_layout-no_input          = 'X'.
  gd_layout-zebra             = 'X'.
  gd_layout-colwidth_optimize = 'X'.
  gd_layout-box_fieldname     = 'SEL'.
  gd_layout-box_tabname       = 'IT_SAIDA'.
  gd_layout-window_titlebar   = 'Apropriação de Custo de Frete'.
  gd_layout-detail_titlebar   = 'Apropriação de Custo de Frete'.
endform.                    " LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_grava .
  sort :  it_bsis     by belnr buzei gjahr,
          it_saida    by belnr buzei gjahr.

  refresh it_zcot0007.
  loop at it_bsis into wa_bsis.
    read table it_saida into wa_saida with key  belnr = wa_bsis-belnr
                                           buzei = wa_bsis-buzei
                                           gjahr = wa_bsis-gjahr binary search.
    vtabix = sy-tabix.
    if ( wa_saida-icon  ne icon_status_open  or sy-subrc ne 0 ).
      continue.
    endif.

    wa_zcot0007-bukrs       = wa_saida-bukrs.
    wa_zcot0007-belnr       = wa_saida-belnr.
    wa_zcot0007-buzei       = wa_saida-buzei.
    wa_zcot0007-gjahr       = wa_saida-gjahr.
    wa_zcot0007-tp_oper     = wa_saida-tp_oper+0(2).
    wa_zcot0007-shtyp       = wa_saida-shtyp+0(4).
    wa_zcot0007-budat       = wa_saida-budat.
    wa_zcot0007-blart       = wa_saida-blart.
    wa_zcot0007-xblnr       = wa_saida-xblnr.
    wa_zcot0007-zuonr       = wa_saida-zuonr.
    wa_zcot0007-belnr_f     = wa_saida-awkey.
    wa_zcot0007-dmbtr       = wa_saida-dmbtr.
    wa_zcot0007-dmbe2       = wa_saida-dmbe2.
    wa_zcot0007-sgtxt       = wa_saida-sgtxt.
    wa_zcot0007-matnr       = wa_saida-matnr.
    wa_zcot0007-werks       = wa_saida-werks.
*    WA_ZCOT0007-DOC_MR22    = WA_SAIDA-DOC_MR22.
*    CONCATENATE 'APF' VNUM WA_BSIS-GJAHR  INTO WA_ZCOT0007-OBJ_KEY.
    wa_zcot0007-dt_apropr   = wa_saida-budat.
    wa_zcot0007-vbeln       = wa_saida-vbeln.
    wa_zcot0007-vbeln_v     = wa_saida-vbelv.
    wa_zcot0007-ebeln       = wa_saida-ebeln.
    wa_zcot0007-kostl       = wa_saida-kostl.
    wa_zcot0007-cpudt       = sy-datum.
    wa_zcot0007-cputm       = sy-uzeit.


    move-corresponding wa_zcot0007 to wl_zcot0007.
    wl_zcot0007-status       = 'P'.
    append wl_zcot0007 to it_zcot0007.
    clear: wa_zcot0007, wa_zcot0007.

  endloop.
  modify zcot0007 from table it_zcot0007.

endform.                    " F_GRAVA
*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_LANCAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form excluir_lancamentos .

  if ( wa_saida-icon  ne icon_status_open ).
    message 'Exclusão não permitida' type 'I'.
    exit.
  endif.

  refresh it_zcot0007.
  clear: wa_zcot0007, wa_zcot0007.
  wa_zcot0007-bukrs       = wa_saida-bukrs.
  wa_zcot0007-belnr       = wa_saida-belnr.
  wa_zcot0007-buzei       = wa_saida-buzei.
  wa_zcot0007-gjahr       = wa_saida-gjahr.
  wa_zcot0007-tp_oper     = wa_saida-tp_oper+0(2).
  wa_zcot0007-shtyp       = wa_saida-shtyp+0(4).
  wa_zcot0007-budat       = wa_saida-budat.
  wa_zcot0007-blart       = wa_saida-blart.
  wa_zcot0007-xblnr       = wa_saida-xblnr.
  wa_zcot0007-zuonr       = wa_saida-zuonr.
  wa_zcot0007-belnr_f     = wa_saida-awkey.
  wa_zcot0007-dmbtr       = wa_saida-dmbtr.
  wa_zcot0007-dmbe2       = wa_saida-dmbe2.
  wa_zcot0007-sgtxt       = wa_saida-sgtxt.
  wa_zcot0007-matnr       = wa_saida-matnr.
  wa_zcot0007-werks       = wa_saida-werks.
  wa_zcot0007-dt_apropr   = wa_saida-budat.
  wa_zcot0007-vbeln       = wa_saida-vbeln.
  wa_zcot0007-vbeln_v     = wa_saida-vbelv.
  wa_zcot0007-ebeln       = wa_saida-ebeln.
  wa_zcot0007-kostl       = wa_saida-kostl.
  wa_zcot0007-cpudt       = sy-datum.
  wa_zcot0007-cputm       = sy-uzeit.


  move-corresponding wa_zcot0007 to wl_zcot0007.
  wl_zcot0007-status       = 'P'.
  append wl_zcot0007 to it_zcot0007.
  clear: wa_zcot0007, wa_zcot0007.


  modify zcot0007 from table it_zcot0007.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = 'Lançamentos Excluido'.

endform.                    " EXCLUIR_LANCAMENTOS
*&---------------------------------------------------------------------*
*&      Form  REPROC_LANCAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form reproc_lancamentos .

endform.                    " REPROC_LANCAMENTOS
