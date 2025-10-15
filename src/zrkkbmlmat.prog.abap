*&---------------------------------------------------------------------*
*& Modulpool         RKKBMLMAT                                         *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*SL 26.10.2011: OSS note 1645751
*SL 19.04.2011: OSS note 1580559
* RHU 23.03.2009 Wrong material texts                       Note 1320867
*SLP6BK000895 29.05.2002 Hinweis 519552 Parameter Curtp optional und mit
*                                     Wertehilfe
*AL0K020500 25.05.01 Korrektur: Lesebaustein CKMS_PERIOD_READ_ONLY statt
*                               CKMS_PERIOD_READ_WITH_ITAB
*P9CK078355  29.09.00
*ALRK261429  01.12.99 Anpassung Status, CURTP als Parameter (no display)
*ALRK238664  21.09.99 Detailbild: Drucktaste ohne Aktion
program zrkkbmlmat message-id c+ no standard page heading.
*ENHANCEMENT-POINT rkkbmlmat_g4 SPOTS es_rkkbmlmat STATIC.
*ENHANCEMENT-POINT rkkbmlmat_g5 SPOTS es_rkkbmlmat.
*ENHANCEMENT-POINT rkkbmlmat_g6 SPOTS es_rkkbmlmat STATIC.
*ENHANCEMENT-POINT rkkbmlmat_g7 SPOTS es_rkkbmlmat.

* global data
*INCLUDE rkkbmlmattop.


type-pools: ckmv0,
            kkbml,
            vrm,
            ckm0.
include lckm0top_vorgaenge.         " Konstanten für Vorgangsschlüssel

include: rkkbeq30.

constants:
* number of rows for cursor-fetch
  c_package_size type i value 20000, "note 2456791 (to improve performance - 200 was too low)
  y_curtp_lc(2)  value '10'.

constants:
* Status
  y_undefiniert
    like ckmlpp-status value ckm0_status-undefiniert,
  y_neu_angelegt
    like ckmlpp-status value ckm0_status-neu_angelegt,
  y_ohne_bestand_eroeffnet
    like ckmlpp-status value ckm0_status-ohne_bestand_eroeffnet,
  y_periode_eroeffnet
    like ckmlpp-status value ckm0_status-periode_eroeffnet,
  y_preisaenderung_erfolgt
    like ckmlpp-status value ckm0_status-preisaenderung_erfolgt,
  y_mengen_und_werte_erfasst
    like ckmlpp-status value ckm0_status-mengen_und_werte_erfasst,
  y_nur_werte_erfasst
    like ckmlpp-status value ckm0_status-nur_werte_erfasst,
  y_einstufig_abgerechnet
    like ckmlpp-status value ckm0_status-einstufig_abgerechnet,
  y_mehrstufig_abgerechnet
    like ckmlpp-status value ckm0_status-mehrstufig_abgerechnet,
  y_abschlussbuchung_storniert
    like ckmlpp-status value ckm0_status-abschlussbuchung_storniert,
  y_abschlussbuchung_erfolgt
    like ckmlpp-status value ckm0_status-abschlussbuchung_erfolgt.

types:
*  Material/valuated sales order/project stock data
  begin of s_stock,
    kalnr type ckmlhd-kalnr,
    mlast type ckmlhd-mlast,
    matnr type ckmlhd-matnr,
    bwkey type ckmlhd-bwkey,
    bwtar type ckmlhd-bwtar,
    sobkz type ckmlhd-sobkz,
    vbeln type ckmlhd-vbeln,
    posnr type ckmlhd-posnr,
    pspnr type ckmlhd-pspnr,
    bklas type mbew-bklas,
    mtart type mara-mtart,
    matkl type mara-matkl,
    spart type mara-spart,
    meins type mara-meins,
  end of s_stock,
  t_stock     type standard table of s_stock
              with key kalnr,
* total records
  t_ckmlpp    type standard table of ckmlpp
              with key kalnr bdatj poper,
  t_ckmlcr    type standard table of ckmlcr
              with key kalnr bdatj poper curtp,
  t_mlrunlist type standard table of mlrunlist,
  t_makt      type standard table of makt
           with key matnr,
* output-table
  begin of s_out,
    kalnr       type ckmlhd-kalnr,
    mlast       type ckmlhd-mlast,
    matnr       type ckmlhd-matnr,
    bwkey       type ckmlhd-bwkey,
    bwtar       type ckmlhd-bwtar,
    sobkz       type ckmlhd-sobkz,
    vbeln       type ckmlhd-vbeln,
    posnr       type ckmlhd-posnr,
    pspnr       type ckmlhd-pspnr,
    bdatj       type ckmlpp-bdatj,
    poper       type ckmlpp-poper,
* status      type ckmlpp-status,
* status_text type dd07v-ddtext,
    konts       type t030-konts,
    ico_xsettle type ckml_icon4,
    ico_xclose  type ckml_icon4,
    curtp       type ckmlcr-curtp,
    rldnr       type fmlt_price-rldnr,
    bklas       type mbew-bklas,
    mtart       type mara-mtart,
    matkl       type mara-matkl,
    spart       type mara-spart,
    ktext       type makt-maktx,
    vprsv       type ckmlcr-vprsv,
    lbkum       type ckmlpp-lbkum,
    meins       type mara-meins,
    salk3       type ckmlcr-salk3,
    salk3_u     type ckmlcr-salk3,
    salkv       type ckmlcr-salkv,
    eb_dif      type cki_doc_ml-eb_dif,
    stprs       type ckmlcr-stprs,
    pvprs       type ckmlcr-pvprs,
    prabw_prz   type ck_prabw_prz,
    peinh       type ckmlcr-peinh,
    waers       type waers,
    salkv_u     type ckmlcr-salk3,
    werks       type t001w-werks,
    gsber       type t134g-gsber,
  end of s_out,
  t_out type standard table of s_out with key kalnr,
*  informations according to curtp (for one val. area)
  begin of s_curr_info,
    bwkey       type ckmlhd-bwkey,
    curtp       type ckmlcr-curtp,
    waers       type cki_ml_cty-waers,
    text        type cki_ml_cty-text,
    currtyp     type cki_ml_cty-currtyp,
    rldnr       type cki_ml_cty-rldnr,
    ext_curtype type cki_ml_cty-ext_curtype,
    valutyp     type cki_ml_cty-valutyp,
  end of s_curr_info,
  t_curr_info type standard table of s_curr_info
            with key bwkey curtp,
  begin of s_status,
    status type ckmlpp-status,
    text   type dd07v-ddtext,
  end of s_status,
  t_status type standard table of s_status
         with key status,

  begin of g_ty_ckmlpp_aux,
    kalnr  type ckmlpp-kalnr,
    bdatj  type ckmlpp-bdatj,
    poper  type ckmlpp-poper,
    fiscal type jahrper,
  end of g_ty_ckmlpp_aux,

  begin of g_ty_ckmlpp,
    kalnr  type ck_kalnr,
    anomes type c length 7,
  end of g_ty_ckmlpp,

  begin of g_ty_ckmlcr,
    kalnr type ckmlcr-kalnr,
    curtp type ckmlcr-curtp,
  end of g_ty_ckmlcr,

  begin of g_ty_extract,
    kalnr type acdoca_m_extract-kalnr,
    vmsl  type acdoca_m_extract-vmsl,
    hsl   type acdoca_m_extract-hsl,
    ksl   type acdoca_m_extract-ksl,
  end of g_ty_extract,

  begin of g_ty_kalnr,    "note 1902569
    kalnr type ck_kalnr,  "note 1902569
  end of g_ty_kalnr,      "note 1902569
  gt_kalnr type standard table of g_ty_kalnr.
*----------------------------------------------------------------------

data: begin of f_dyn,                  "Felder zur Dynprosteuerung
        lastindex    like sy-tabix,       "Gesamtanzahl Tabellenzeilen
        loopc        like sy-loopc,       "Anzahl LOOP-Zeilen auf Dynpro
        pageindex    like sy-tabix,       "TABIX der ersten LOOP-Zeile
*        pickbdatj    LIKE mlpp-bdatj, "BDATJ der ausgewählten LOOP-Zeile
        pickcurtp    like ckmlcr-curtp, "CURTP der ausgewählten LOOP-Zeile
        pickcurtptxt like cki_ml_cty-text, "Währungstext
        pickindex    like sy-tabix,   "TABIX der ausgewählten LOOP-Zeile
        "CUA-Titel zu ergänzen)
*        pickpoper    LIKE mlpp-poper, "POPER der ausgewählten LOOP-Zeile
*        readindex    LIKE sy-tabix,       "TABIX der bearb. LOOP-Zeile
*       Nachfolgende Variablen sind kleiner, damit auf Dynpro
*       oder in Titel kompaktere Anzeige möglich ist. Leider
*       sind die TABIX-artigen Variablen wegen Schnittstellen-
*       definition in Funktionsbausteinen weiterhin nötig.
*        pickpos      LIKE mlit-posnr,     "wie pickindex
*        bispos       LIKE mlit-posnr,     "wie lastindex
*        vonpos       LIKE mlit-posnr,     "wie pageindex
      end   of f_dyn.
data:
* output table with all currency
  g_t_out        type t_out,
  g_out          type s_out,
* output table with one currency
  g_t_alv_list   type t_out,
*  Buffer for currency informations
  g_curr_info    type s_curr_info,       "Current bwkey/currency type
  g_t_curr_info  type t_curr_info,     "All read bwkey/currency type
*
  g_t_status     type t_status,
  g_price_toggle type abap_bool,
  gh_old_bwkey   type mlkey-bwkey.

*----------------------------------------------------------------------
tables: ckmlhd,
        ckmlpp,
        mara,
        mbew,
        marv,
        t001k,
        sscrfields.

*Tabelle für Wertehilfe Curtp
data: begin of t_curtp_f4 occurs 0,
        curtp like mlkey-curtp,
        rldnr like cki_ml_cty-rldnr,
        text  like ckmlcur-ddtext,
      end of t_curtp_f4.

data: t_curtp_vrm  type vrm_values,
      gs_curtp_vrm type vrm_value.

data: t_rldnr_vrm  type vrm_values,
      gs_rldnr_vrm type vrm_value.

data: t_curtp like cki_ml_cty occurs 0 with header line.

data: lh_bwkey   type mlkey-bwkey,
      lh_bukrs   type mlkey-bukrs,
      ls_ckmlcur type ckmlcur.

*TYPES:
*  BEGIN OF ts_matlledgervalncrcyrole,
*    ledgervaluationcurrencyrole    TYPE r_MatlLedgerValnCrcyRole-LedgerValuationCurrencyRole,
*    ledger                         TYPE r_matlledgervalncrcyrole-ledger,
*    currencyrole                   TYPE r_matlledgervalncrcyrole-CurrencyRole,
*    currency                       TYPE r_matlledgervalncrcyrole-Currency,
*    currencyrolename               TYPE r_matlledgervalncrcyrole-CurrencyRoleName,
*    ledgername                     TYPE r_matlledgervalncrcyrole-LedgerName,
*    acctgvalnvwsubviewcombinedname TYPE r_matlledgervalncrcyrole-AcctgValnVwSubviewCombinedName,
*  END OF ts_matlledgervalncrcyrole.
*DATA: gt_table_curval TYPE STANDARD TABLE OF ts_matlledgervalncrcyrole.
*DATA: gr_event_handler_curval TYPE REF TO lcl_event_handler_curval,
*      gr_table_curval         TYPE REF TO cl_salv_table.


data: t_dd07v like dd07v occurs 20 with header line.

* Variablen für den ALV.
data: exit.

type-pools slis.
data: fieldcat      type slis_t_fieldcat_alv.
data: fieldcathead  type slis_fieldcat_alv.
data: is_variant    like disvariant.
data: it_events     type slis_t_event.
data: it_eventhead  type slis_alv_event.


* Extakte
data: gs_extract1     like disextract.
data: gs_extract2     like disextract.
data: gs_admin        like ltexadmin.

*CLASS lcl_event_handler_curval DEFINITION.
*  PUBLIC SECTION.
*    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_table
*      IMPORTING
*        row column.
*    METHODS on_before_salv_functions FOR EVENT before_salv_function OF cl_salv_events_table
*      IMPORTING
*        e_salv_function.
*ENDCLASS.


* Selektionsbild Extraktverwaltung
* Ausgabe als Popup
selection-screen begin of screen 4500 as window title text-ext.
  selection-screen begin of line.
    parameters: p_noex radiobutton group extr.
    selection-screen comment 3(50) text-ex1 for field p_noex.
  selection-screen end of line.

  selection-screen begin of line.
    parameters: p_save radiobutton group extr.
    selection-screen comment 3(50) text-ex3 for field p_save.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 3(29) text-ex2 for field p_ex1.
    parameters: p_ex1 like ltex-exname.
  selection-screen end of line.
  selection-screen begin of line.
    selection-screen comment 3(29) text-ex5 for field p_ext1.
    parameters: p_ext1 like ltex-text.
  selection-screen end of line.

  selection-screen begin of line.
    parameters: p_load radiobutton group extr.
    selection-screen comment 3(50) text-ex4 for field p_load.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment 3(29) text-ex2 for field p_ex2.
    parameters: p_ex2 like ltex-exname.
    selection-screen comment 47(40) p_ext2 for field p_ex2.
  selection-screen end of line.
selection-screen end of screen 4500.


tables: tcurm, t134g, mlkey, t030.
data: p_bwkey  like ckmlhd-bwkey.      "Hilfsfeld Bewertungskreis

********************** Selektionsbildschirm ****************************
* Objekteingrenzung
selection-screen skip 2.
select-options: r_matnr for  ckmlhd-matnr.
**INCLUDE rckm_par_bukrs_werks_ml_prod.  "Parameter BUKRS/WERKS

parameters: p_bukrs like mlkey-bukrs_ml_productive
                    obligatory
                    memory id buk.

parameters: p_2bukrs like mlkey-bukrs_ml_productive.

select-options:
***              s_hkont FOR t030-konts,
              s_werks for mlkey-werks_ml_productive no intervals memory id wrk.

select-options: r_bwtar for  ckmlhd-bwtar  memory id bwt.
selection-screen skip.
select-options: r_mtart for  mara-mtart    memory id mta modif id puk,
                r_bklas for  mbew-bklas    modif id puk,
                r_matkl for  mara-matkl    memory id mtl modif id puk,
                r_spart for  mara-spart    memory id spa modif id puk,
                r_mlast for  ckmlhd-mlast  no-extension no intervals
                                           modif id puk,

*                r_status FOR ckmlpp-status MODIF ID puk,
                r_vbeln for  ckmlhd-vbeln  modif id puk,
                r_posnr for  ckmlhd-posnr  modif id puk,
                r_pspnr for  ckmlhd-pspnr  modif id puk.

select-options: s_gsber for  t134g-gsber no intervals.
parameters:
  r_st_d like bsid-umskz as checkbox  default ' ',
  r_st_z like bsid-umskz as checkbox  default ' '.
parameters:     p_ex_sel type c            default space
                                           no-display.
selection-screen skip 1.
* Periode
selection-screen begin of block parameter with frame title text-101.
  parameters: p_poper like cki_doc_ml-sl_periode
                      memory id mlp,
              p_bdatj like ckmlrunplant-gjahr memory id mlj.
selection-screen end of block parameter.

selection-screen begin of block currency with frame title text-008.
  selection-screen begin of line.
    selection-screen comment (31) text-007 for field p_curtp.
    parameters: p_curtp like ckmlcr-curtp visible length 30.
    parameters: p_ctext like cki_ml_cty-text visible length 30.
    parameters: p_rldnr like cki_ml_cty-rldnr visible length 30.
    parameters: p_rtext type ldtxt.
  selection-screen end of line.
selection-screen end of block currency.

selection-screen begin of block extract with frame title text-104.
  selection-screen begin of line.
    selection-screen comment (31) text-ex2 for field p_pex2
      modif id ext.
    parameters: p_pex2 like ltex-exname
                                                      modif id ext.
    selection-screen comment 47(40) p_pext2 for field p_pex2
      modif id ext.
  selection-screen end of line.
selection-screen end of block extract.

selection-screen begin of block variant with frame title text-102.
  selection-screen begin of line.
    selection-screen comment (31) text-103 for field p_varian
      modif id var.
    parameters p_varian like disvariant-variant modif id var.
    selection-screen comment 47(40) p_vartxt for field p_varian
      modif id var.

  selection-screen end of line.
selection-screen end of block variant.
************************************************************************

************************************************************************
initialization.

***

  clear: ls_ckmlcur.
  set pf-status 'SELECTION'.

  clear is_variant.
  is_variant-report = sy-repid.
  is_variant-username = sy-uname.

  call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
    exporting
      i_save     = 'A'
    changing
      cs_variant = is_variant
    exceptions
      not_found  = 2.
  if sy-subrc = 0.
    p_varian = is_variant-variant.
    p_vartxt = is_variant-text.
  endif.

  call function 'REUSE_ALV_EXTRACT_AT_INIT'
    changing
      cs_extract1 = gs_extract1
      cs_extract2 = gs_extract2.


************************************************************************
at selection-screen output.
************************************************************************
  g_price_toggle = cl_fml_toggle=>get_instance( )->is_price_redesign_active( ).
  perform display_bukrs_or_werks.

  if s_gsber[] is initial and s_werks[] is initial and p_2bukrs is initial.
    message 'Informe Centro e/ou Divisão e/ou Empresa' type 'I'.
    set cursor field 'S_WERKS-LOW' .
  endif.

*** modify screen
  check sy-dynnr ne '4500'.
  if p_load = 'X'.
    p_pex2 = p_ex2.
    p_pext2 = p_ext2.
  elseif p_save = 'X'.
    p_pex2 = p_ex1.
    p_pext2 = p_ext1.
  else.
    clear: p_pex2, p_pext2.
  endif.

  refresh: t_curtp_vrm.
  clear: lh_bwkey, lh_bukrs, ls_ckmlcur.


  if not s_werks[] is initial.

    loop at s_werks.

      call function 'DETERMIN_BWKEY_BUKRS_FOR_PLANT'
        exporting
          werk  = s_werks-low
        importing
          bwkey = lh_bwkey
          bukrs = lh_bukrs.

      if lh_bwkey <> gh_old_bwkey.
        gh_old_bwkey = lh_bwkey.

        call function 'GET_BWKEY_CURRENCY_INFO'
          exporting
            bwkey               = lh_bwkey
            call_by_init_prog   = ' '
            i_customizing       = ' '
            iv_ext_process      = abap_true
          tables
            t_curtp_for_va      = t_curtp
          exceptions
            bwkey_not_found     = 1
            bwkey_not_active    = 2
            matled_not_found    = 3
            internal_error      = 4
            more_than_3_curtp   = 5
            customizing_changed = 6
            others              = 7.

        if sy-subrc <> 0.
* Keine messages in F4-Hilfen!
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        endif.
      endif.

      loop at t_curtp.
        clear: gs_curtp_vrm.
        gs_curtp_vrm-key = t_curtp-curtp.
        gs_curtp_vrm-text = t_curtp-text.
        append gs_curtp_vrm to t_curtp_vrm.

        clear: gs_rldnr_vrm.
        gs_rldnr_vrm-key = t_curtp-rldnr.
        append gs_rldnr_vrm to t_rldnr_vrm.
      endloop.

    endloop.

  elseif s_werks[] is initial.

    if g_price_toggle = abap_true.
      select single * into @data(ls_finsc_curtypet) from finsc_curtypet
        where langu = @sy-langu
          and curtype = @y_curtp_lc.
      ls_ckmlcur-ddtext = ls_finsc_curtypet-name.
      p_rldnr = cl_fins_acdoc_util=>get_leading_ledger( ).
      p_rtext = cl_finsvc_ledger=>get_instance( )->get_ledger_description( p_rldnr ).
    else.
      select single * into ls_ckmlcur from ckmlcur
           where sprsl = sy-langu
           and   curtp = y_curtp_lc.
    endif.
    clear: gs_curtp_vrm, t_curtp_vrm.
    gs_curtp_vrm-key = y_curtp_lc.
    gs_curtp_vrm-text = ls_ckmlcur-ddtext.
    append gs_curtp_vrm to t_curtp_vrm.
    p_curtp = y_curtp_lc.
    p_ctext = ls_ckmlcur-ddtext.

  endif.

  read table t_curtp_vrm with key key = p_curtp
                              transporting no fields.
  if sy-subrc <> 0 or
     p_curtp is initial.
    p_curtp = y_curtp_lc.
  endif.

  call function 'VRM_SET_VALUES'
    exporting
      id     = 'P_CURTP'
      values = t_curtp_vrm
* EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

*modifications for the extracts
  perform modify_selection_screen.
* modifications for extended selections
  perform modify_object_selection tables r_vbeln
                                         r_posnr
                                         r_pspnr
                                         r_mtart
                                         r_bklas
                                         r_matkl
                                         r_spart
                                  using  p_ex_sel.
* set default period if necessary
  perform set_period changing p_poper p_bdatj.

************************************************************************
at selection-screen.
************************************************************************
  if not p_2bukrs is initial.

*    SELECT bwkey
*      FROM t001k
*      INTO TABLE @DATA(lt_t001k)
*      WHERE bukrs = p_burks2.
*
*    s_werks = VALUE #( FOR ls_t001k IN lt_t001k
*                        ( sign = 'I'
*                         option = 'EQ'
*                         low = ls_t001k-bwkey ) ).

  elseif s_werks is not initial.
    p_bwkey = s_werks-low.
  else.
    p_bwkey = s_gsber-low.
  endif.

  if s_gsber is not initial.

    data: lr_werks type range of t001w-werks.

    select werks
      from t001w
      into table @data(lt_t001w)
      where j_1bbranch in @s_gsber.

    if sy-subrc = 0.
      lr_werks = value #( for ls_t001w in lt_t001w
                          ( sign = 'I'
                          option = 'EQ'
                          low = ls_t001w-werks ) ).
      append lines of lr_werks to s_werks.
    endif.


  endif.

*** Zugriffserlaubnis überprüfen
  if sy-ucomm ne 'EXTR'.
    call function 'REUSE_ALV_EXTRACT_AT_SELSCREEN'
      exporting
        i_p_save    = p_save
        i_p_load    = p_load
      changing
        c_p_ex1     = p_ex1
        c_p_ex2     = p_ex2
        c_p_ext1    = p_ext1
        c_p_ext2    = p_ext2
        cs_extract1 = gs_extract1
        cs_extract2 = gs_extract2.
  endif.
  check sy-dynnr ne '4500'.
  perform variant_check_existence.
  perform user_command.

************************************************************************
at selection-screen on p_bukrs.
************************************************************************
  if not p_bukrs is initial.
    perform check_bwkey_bukrs using p_bukrs.
  endif.

************************************************************************
at selection-screen on s_werks.
************************************************************************
  if not s_werks[] is initial.
    perform check_bwkey .
  endif.

************************************************************************
at selection-screen on value-request for p_varian.
  perform f4_for_variant.

************************************************************************
at selection-screen on value-request for p_ex1.
  call function 'REUSE_ALV_EXTRACT_AT_F4_P_EX1'
    changing
      c_p_ex1     = p_ex1
      c_p_ext1    = p_ext1
      cs_extract1 = gs_extract1.

************************************************************************
at selection-screen on value-request for p_ex2.
  call function 'REUSE_ALV_EXTRACT_AT_F4_P_EX2'
    changing
      c_p_ex2     = p_ex2
      c_p_ext2    = p_ext2
      cs_extract2 = gs_extract2.
***********************************************************************
at selection-screen on value-request for p_curtp.

  clear: lh_bwkey, lh_bukrs, ls_ckmlcur.
  refresh: t_curtp_f4.

  if not s_werks is initial.

    loop at s_werks.

      call function 'DETERMIN_BWKEY_BUKRS_FOR_PLANT'
        exporting
          werk  = s_werks-low
        importing
          bwkey = lh_bwkey
          bukrs = lh_bukrs.

      if lh_bwkey <> gh_old_bwkey.
        gh_old_bwkey = lh_bwkey.


        call function 'GET_BWKEY_CURRENCY_INFO'
          exporting
            bwkey               = lh_bwkey
*           CALL_BY_INIT_PROG   = ' '
*           I_CUSTOMIZING       = ' '
          tables
            t_curtp_for_va      = t_curtp
          exceptions
            bwkey_not_found     = 1
            bwkey_not_active    = 2
            matled_not_found    = 3
            internal_error      = 4
            more_than_3_curtp   = 5
            customizing_changed = 6
            others              = 7.

        if sy-subrc <> 0.
* Keine messages in F4-Hilfen!
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        endif.
      endif.

      loop at t_curtp.
        clear t_curtp_f4.
        t_curtp_f4-curtp = cond #( when g_price_toggle = abap_true
                                   then t_curtp-ext_curtype
                                   else t_curtp-curtp ).
        t_curtp_f4-text = t_curtp-text.
        t_curtp_f4-rldnr = t_curtp-rldnr.
        append t_curtp_f4.
      endloop.

    endloop.

  elseif s_werks is initial.

    if g_price_toggle = abap_true.
      select single * into @data(ls_finsc_curtypet) from finsc_curtypet
        where langu = @sy-langu
          and curtype = @y_curtp_lc.
      ls_ckmlcur-ddtext = ls_finsc_curtypet-name.
      t_curtp_f4-rldnr = cl_fins_acdoc_util=>get_leading_ledger( ).
      p_rldnr = cl_fins_acdoc_util=>get_leading_ledger( ).
      p_rtext = cl_finsvc_ledger=>get_instance( )->get_ledger_description( p_rldnr ).
    else.
      select single * into ls_ckmlcur from ckmlcur
      where sprsl = sy-langu
      and   curtp = y_curtp_lc.
    endif.

    clear: t_curtp_f4.
    t_curtp_f4-curtp = y_curtp_lc.
    t_curtp_f4-text = ls_ckmlcur-ddtext.
    append t_curtp_f4.
    p_curtp = y_curtp_lc.
  endif.

  read table t_curtp_f4 with key curtp = p_curtp
                      transporting no fields.
  if sy-subrc <> 0 or

     p_curtp is initial.

    p_curtp = y_curtp_lc.

  endif.



  if not t_curtp_f4[] is initial.

    data lt_return type standard table of ddshretval with empty key.
    data lt_mapping type standard table of dselc.
    data lt_dynprofelder type table of dynpread.

    lt_mapping = value #( ( fldname = 'F0001' dyfldname = 'P_CURTP' )
                          ( fldname = 'F0002' dyfldname = 'P_RLDNR' )
                          ( fldname = 'F0003' dyfldname = 'P_CTEXT' ) ).

    clear lt_dynprofelder.
    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'CURTP'
        dynpnr          = '1000'
        value_org       = 'S'
        dynprofield     = 'P_CURTP'
      tables
        value_tab       = t_curtp_f4
        return_tab      = lt_return
        dynpfld_mapping = lt_mapping.

    if sy-subrc = 0.
* Get currency type and currency type description from user select
      read table lt_return assigning field-symbol(<fs_return>) with key retfield = 'P_CURTP'.
      if sy-subrc = 0.
        p_curtp = <fs_return>-fieldval.
      endif.
      read table lt_return assigning <fs_return> with key retfield = 'P_CTEXT'.
      if sy-subrc = 0.
        lt_dynprofelder = value #( base lt_dynprofelder ( fieldname = 'P_CTEXT' fieldvalue = <fs_return>-fieldval ) ).
      endif.
* Get ledger and ledger description from user select
      if g_price_toggle = abap_true.
        read table lt_return assigning <fs_return> with key retfield = 'P_RLDNR'.
        if sy-subrc = 0.
          data(lv_rtext) = cl_finsvc_ledger=>get_instance( )->get_ledger_description( conv #( <fs_return>-fieldval ) ).
          lt_dynprofelder = value #( ( fieldname = 'P_RLDNR' fieldvalue = <fs_return>-fieldval  )
                                     ( fieldname = 'P_RTEXT' fieldvalue =  lv_rtext ) ).
        endif.
      endif.
    endif.
*   Set value back to Screen fields.
    call function 'DYNP_VALUES_UPDATE'
      exporting
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      tables
        dynpfields = lt_dynprofelder
      exceptions
        others     = 1.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.
  endif.

************************************************************************













************************************************************************
start-of-selection.

  if p_2bukrs is not initial.
    select bwkey
    from t001k
    into table @data(lt_t001k)
    where bukrs = @p_2bukrs.

    s_werks[] = value #( for ls_t001k in lt_t001k
                        ( sign = 'I'
                         option = 'EQ'
                         low = ls_t001k-bwkey ) ).
  endif.

  if s_gsber[] is initial and s_werks[] is initial.
    exit.
  endif.

***
* set default period if necessary
  perform set_period changing p_poper p_bdatj.

* get output table by reading from DB or loading an extract
  if p_load is initial.
*   read currencies of bwkey
*    PERFORM read_curr_info USING p_bwkey.
    perform read_curr_info_swerks.
    if g_price_toggle = abap_true.
      read table g_t_curr_info into g_curr_info
                               with key ext_curtype = p_curtp
                                        rldnr = p_rldnr.
    else.
      read table g_t_curr_info into g_curr_info
                               with key curtp = p_curtp.
    endif.
    if sy-subrc <> 0.
      message i618(c+) with p_curtp.
      leave list-processing.
    endif.
*   read texts of all states
    perform read_texts_of_states.
*   read ML-data from DB
    perform read_data changing g_t_out.
*   filter currency
    perform filter_currency using g_t_out g_curr_info-curtp
                                          g_curr_info-rldnr
                            changing g_t_alv_list.
  else.
*   load extract
    call function 'REUSE_ALV_EXTRACT_LOAD'
      exporting
        is_extract = gs_extract2
      importing
        es_admin   = gs_admin
      tables
        et_exp01   = g_t_out.

*   set global parameters
    read table g_t_out index 1 into g_out.
    p_bwkey = g_out-bwkey.
    p_bdatj = g_out-bdatj.
    p_poper = g_out-poper.
*   read currencies of bwkey
    perform read_curr_info using p_bwkey.
*   filter currency
    perform filter_currency using g_t_out g_curr_info-curtp
                                          g_curr_info-rldnr
                            changing g_t_alv_list.
  endif.

* display or save output table
  if not g_t_out is initial.
*   display the list
    if p_save is initial.
      perform display_list.
*   save extract
    else.
      call function 'REUSE_ALV_EXTRACT_SAVE'
        exporting
          is_extract = gs_extract1
        tables
          it_exp01   = g_t_out.
    endif.
  else.
    message i114.
  endif.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*&      Select materials, read ML-data and build output-table
*&---------------------------------------------------------------------*
form read_data
  changing e_t_out type t_out.

  data: l_cursor            type cursor,
        l_xflag_end_of_data,
        l_t_stock           type t_stock,
        l_t_ckmlpp          type t_ckmlpp,
        l_t_ckmlcr          type t_ckmlcr,
        t_t030              type table of t030 with header line,
        l_t_ml              type cl_fml_join_ckmlpp_cr_buffer=>tt_ml_period_data_with_curtp,
        l_t_mlrunlist       type t_mlrunlist,
        ls_kalnr            type g_ty_kalnr,          " note 1902569
        lt_kalnr_all        type table of g_ty_kalnr, " note 1902569
        lt_kalnr_zero       type table of g_ty_kalnr, " note 1902569
        lt_kalnr_pack       type table of g_ty_kalnr, " note 1902569
        lv_pack_index       type i,                   " note 1902569
        lv_last_index       type i,                   " note 1902569
        lv_package_size     type i.                   "note 2456791 - this variable is introduced solely for debugging purposes

  data: wl_t134g       type t134g,
        ls_ckmlpp      type g_ty_ckmlpp,
        lt_ckmlpp      type table of g_ty_ckmlpp,

        ls_ckmlcr      type g_ty_ckmlcr,
        lt_ckmlcr      type table of g_ty_ckmlcr,

        ls_extract     type g_ty_extract,
        lt_extract     type table of g_ty_extract,
        lt_extract_s   type table of g_ty_extract,

        ls_ckmlpp_aux  type g_ty_ckmlpp_aux,
        lt_ckmlpp_aux  type table of g_ty_ckmlpp_aux,

        v_txt_wrk(100).
  if s_werks[] is initial.
    select *
      from t134g
      into wl_t134g
      where gsber in s_gsber
      and   spart eq '01'.
      s_werks-low    = wl_t134g-werks.
      clear s_werks-high.
      s_werks-sign   = 'I'.
      s_werks-option = 'EQ'.
      append s_werks.
    endselect.

  endif.


  sort t_t030 by  bklas.
  delete adjacent duplicates from t_t030 comparing  bklas.
  loop at t_t030.
    r_bklas-low    = t_t030-bklas.
    clear r_bklas-high.
    r_bklas-sign   = 'I'.
    r_bklas-option = 'EQ'.
    append r_bklas.
  endloop.

  if 1 = 2.
* get list of relevant KALNRS (note 1902569)
    if r_mtart   is initial and
        r_matkl  is initial and
        r_spart  is initial.
      select kalnr
         from ckmlhd
         into table lt_kalnr_all
         where   bwkey in s_werks"p_bwkey
             and matnr in r_matnr
             and bwtar in r_bwtar
             and vbeln in r_vbeln
             and posnr in r_posnr
             and pspnr in r_pspnr
             and mlast in r_mlast.
    else.
      select h~kalnr
         from ckmlhd as h join mara as m
              on h~matnr = m~matnr
         into table lt_kalnr_all
       where h~bwkey = p_bwkey
             and h~matnr in r_matnr
             and h~bwtar in r_bwtar
             and h~vbeln in r_vbeln
             and h~posnr in r_posnr
             and h~pspnr in r_pspnr
             and h~mlast in r_mlast
             and m~mtart in r_mtart
             and m~matkl in r_matkl
             and m~spart in r_spart.
    endif.


    describe table lt_kalnr_all lines lv_last_index.

    if lv_last_index = 0.
*  nothing found ...
      exit.
    endif.
  endif.

  "US149509 ALRS
  loop at s_werks.
    select kalnr, anomes
      from zi_mm_ckmlpp( p_bdatj = @p_bdatj, p_poper = @p_poper, p_bwkey = @s_werks-low )
      appending table @lt_ckmlpp.
  endloop.

  loop at lt_ckmlpp into ls_ckmlpp.
    ls_ckmlpp_aux-kalnr = ls_ckmlpp-kalnr.
    ls_ckmlpp_aux-bdatj = ls_ckmlpp-anomes+0(4).
    ls_ckmlpp_aux-poper = ls_ckmlpp-anomes+4(3).
    ls_ckmlpp_aux-fiscal = ls_ckmlpp-anomes.
    append ls_ckmlpp_aux to lt_ckmlpp_aux.
  endloop.

  check lt_ckmlpp_aux[] is not initial.


  select
    kalnr,
    vmsl,
    hsl,
    ksl
  from acdoca_m_extract
  into table @lt_extract
  for all entries in @lt_ckmlpp_aux
  where  kalnr = @lt_ckmlpp_aux-kalnr
  and    fiscyearper > @lt_ckmlpp_aux-fiscal.

  loop at lt_extract into ls_extract.
    collect ls_extract into lt_extract_s.
  endloop.

*  delete lt_extract_s where vmsl = 0.   BUG172916

  if r_st_z eq 'X'.
    delete lt_extract_s where hsl <> 0  and ksl <> 0.
  endif.

  check lt_extract_s[] is not initial.

  select kalnr
     from ckmlhd
     into table lt_kalnr_all
     for all entries in lt_extract_s
     where ckmlhd~kalnr eq lt_extract_s-kalnr
     and   ckmlhd~matnr in r_matnr
     and   ckmlhd~bwtar in r_bwtar
     and   ckmlhd~vbeln in r_vbeln
     and   ckmlhd~posnr in r_posnr
     and   ckmlhd~pspnr in r_pspnr
     and   ckmlhd~mlast in r_mlast.
  "US149509 ALRS

  describe table lt_kalnr_all lines lv_last_index.

  if lv_last_index = 0.
    exit.
  endif.

  clear lv_package_size.
  lv_package_size = c_package_size.
*
  while ( lv_pack_index <= lv_last_index ).

    perform build_kalnr_package
        tables   lt_kalnr_all
                 lt_kalnr_pack
        using    lv_package_size
                 lv_last_index
        changing lv_pack_index.

*ENHANCEMENT-SECTION rkkbmlmat_01 SPOTS es_rkkbmlmat .
    if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RKKBMLMAT_EHP4\RKKBMLMAT_01\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
*   get data from CKMLHD and MARA
      select h~bwkey h~kalnr h~mlast h~matnr h~bwtar
      h~sobkz h~vbeln h~posnr h~pspnr
      m~mtart m~matkl m~spart m~/cwm/valum as meins
      from ckmlhd as h join mara as m
      on h~matnr = m~matnr
      into corresponding fields of table l_t_stock
      for all entries in lt_kalnr_pack
      where kalnr = lt_kalnr_pack-kalnr.
    else.
*   get data from CKMLHD and MARA
      select h~bwkey h~kalnr h~mlast h~matnr h~bwtar
      h~sobkz h~vbeln h~posnr h~pspnr
      m~mtart m~matkl m~spart m~meins
      from ckmlhd as h join mara as m
      on h~matnr = m~matnr
      into corresponding fields of table l_t_stock
      for all entries in lt_kalnr_pack
      where kalnr = lt_kalnr_pack-kalnr.
    endif.

*END-ENHANCEMENT-SECTION.

*   read val. classes for all material/sales order/project stocks
    perform get_bklas changing l_t_stock.
*   delete materials which do not satisfy sel.-crit. for bklas
    if not r_bklas is initial.
      delete l_t_stock where not bklas in r_bklas.
    endif.

    if l_t_stock is not initial.
      if g_price_toggle = abap_true.
        perform read_total_records_rldnr using l_t_stock p_bwkey changing l_t_ml l_t_mlrunlist.
        perform build_output_rldnr changing l_t_stock l_t_ml l_t_mlrunlist e_t_out.
      else.
*   read total records
        perform read_total_records using l_t_stock p_bwkey
                                changing l_t_ckmlpp l_t_ckmlcr l_t_mlrunlist.
*   build output table
        perform build_output changing l_t_stock l_t_ckmlpp l_t_ckmlcr l_t_mlrunlist
                                       e_t_out.

        perform get_hkont changing e_t_out.

        perform get_gsber changing e_t_out.

      endif.
    endif.

    clear: l_t_stock,
           l_t_ckmlpp,
           l_t_ckmlcr,
           lt_kalnr_pack.

  endwhile.
endform.                    "read_data

*----------------------------------------------------------------------*
*       FORM read_curr_info
*----------------------------------------------------------------------*
*       read currency informations for a val. area
*       check authority for each currency
*----------------------------------------------------------------------*
form read_curr_info
   using i_bwkey           type ckmlhd-bwkey.

  data: l_curtp_for_va   type cki_ml_cty,
        l_t_curtp_for_va type cki_ml_cty occurs 0.
  data: l_xauth.

  call function 'GET_BWKEY_CURRENCY_INFO'
    exporting
      bwkey             = i_bwkey
    tables
      t_curtp_for_va    = l_t_curtp_for_va
    exceptions
      bwkey_not_found   = 1
      bwkey_not_active  = 2
      matled_not_found  = 3
      more_than_3_curtp = 4
      internal_error    = 5
      others            = 6.

* write infos into buffer
  clear g_t_curr_info.
  clear g_curr_info.
  g_curr_info-bwkey = i_bwkey.
  loop at l_t_curtp_for_va into l_curtp_for_va.
*   check authority and insert currency into buffer if possible
    call function 'TP_VALUATION_AUTHORITY'
      exporting
        i_bwkey = i_bwkey
        i_cvtyp = l_curtp_for_va-curtp
      importing
        e_xauth = l_xauth
      exceptions
        others  = 1.
    if l_xauth <> space.
      move-corresponding l_curtp_for_va to g_curr_info.
      append g_curr_info to g_t_curr_info.
    endif.
  endloop.

* set current currency which will be displayed first
  sort g_t_curr_info by bwkey curtp.
  read table g_t_curr_info index 1 into g_curr_info.
endform.                    "read_curr_info


*----------------------------------------------------------------------*
*       FORM read_curr_info
*----------------------------------------------------------------------*
*       read currency informations for a val. area
*       check authority for each currency
*----------------------------------------------------------------------*
form read_curr_info_swerks .

  data lv_bwkey           type ckmlhd-bwkey.

  data: l_curtp_for_va   type cki_ml_cty,
        l_t_curtp_for_va type cki_ml_cty occurs 0.
  data: l_xauth.

* write infos into buffer
  clear g_t_curr_info.
  clear g_curr_info.

  loop at s_werks assigning field-symbol(<fs_werks>).

    lv_bwkey = <fs_werks>-low.

    call function 'GET_BWKEY_CURRENCY_INFO'
      exporting
        bwkey             = lv_bwkey
      tables
        t_curtp_for_va    = l_t_curtp_for_va
      exceptions
        bwkey_not_found   = 1
        bwkey_not_active  = 2
        matled_not_found  = 3
        more_than_3_curtp = 4
        internal_error    = 5
        others            = 6.

    g_curr_info-bwkey = lv_bwkey.

    loop at l_t_curtp_for_va into l_curtp_for_va.
*   check authority and insert currency into buffer if possible
      call function 'TP_VALUATION_AUTHORITY'
        exporting
          i_bwkey = lv_bwkey
          i_cvtyp = l_curtp_for_va-curtp
        importing
          e_xauth = l_xauth
        exceptions
          others  = 1.
      if l_xauth <> space.
        move-corresponding l_curtp_for_va to g_curr_info.
        append g_curr_info to g_t_curr_info.
      endif.

    endloop.

* set current currency which will be displayed first
    sort g_t_curr_info by bwkey curtp.
    read table g_t_curr_info index 1 into g_curr_info.

  endloop.
endform.                    "read_curr_info

*----------------------------------------------------------------------*
*       FORM read_texts_of_states
*----------------------------------------------------------------------*
*       read text for each state und write this information into buffer
*----------------------------------------------------------------------*
form read_texts_of_states.
  data: l_dfies   type dfies,
        l_dd07v   type dd07v,
        l_t_dd07v type dd07v occurs 0,
        l_status  type s_status.

  l_dfies-fieldname = 'STATUS'.
  l_dfies-tabname = 'CKMLPP'.
  call function 'G_FIELD_READ'
    exporting
      fieldname  = l_dfies-fieldname
      table      = l_dfies-tabname
      text_flag  = 'X'
    importing
      field_attr = l_dfies
    exceptions
      not_found  = 1
      others     = 2.

  call function 'DDUT_DOMVALUES_GET'
    exporting
      name          = l_dfies-domname
      langu         = sy-langu
      texts_only    = 'X'
    tables
      dd07v_tab     = l_t_dd07v
    exceptions
      illegal_input = 1
      others        = 2.
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

* write states/texts into buffer
  loop at l_t_dd07v into l_dd07v.
    l_status-status = l_dd07v-domvalue_l.
    l_status-text   = l_dd07v-ddtext.
    append l_status to g_t_status.
  endloop.
endform.                    "read_texts_of_states

*----------------------------------------------------------------------*
*       FORM read_curr_info
*----------------------------------------------------------------------*
*       filter currency
*----------------------------------------------------------------------*
form filter_currency
   using i_t_out type t_out
         i_curtp type curtp
         i_rldnr type rldnr
   changing e_t_alv_list type t_out.

  data: l_out type s_out.
  data: s_rldnr type fmlt_price-rldnr.
  data: w_out type s_out.
  data  it_out type standard table of s_out with key kalnr.

  clear s_rldnr.
  s_rldnr = cond #( when g_price_toggle = abap_true
                    then i_rldnr ).

*  CLEAR e_t_alv_list.
*  LOOP AT i_t_out INTO l_out
*          WHERE curtp = i_curtp
*            AND rldnr = s_rldnr.
*    APPEND l_out TO e_t_alv_list.
*  ENDLOOP.

  it_out[] = i_t_out[].
  clear e_t_alv_list.
  loop at i_t_out into l_out
          where curtp = i_curtp.
    if r_st_d = 'X'.
      read table  it_out into w_out with key  kalnr =  l_out-kalnr
                                              curtp = '40'.
      if sy-subrc = 0.
        l_out-salk3_u = w_out-salk3.
        l_out-salkv_u = w_out-salkv. "PBI - 72568 - CBRAND
      endif.
    endif.
    append l_out to e_t_alv_list.
  endloop.

endform.                    "filter_currency

*----------------------------------------------------------------------*
*       FORM get_bklas
*----------------------------------------------------------------------*
*       read val. classes for all material/sales order/project stocks
*----------------------------------------------------------------------*
form get_bklas
   changing et_stock       type t_stock.

  field-symbols: <stock> type s_stock.
  data: lt_bklas type kkbml_t_bklas,
        ls_bklas type kkbml_s_bklas.

  loop at et_stock assigning <stock>.
    move-corresponding <stock> to ls_bklas.
    append ls_bklas to lt_bklas.
  endloop.

  call function 'K_KKB_READ_BKLAS'
    changing
      ct_bklas = lt_bklas.

  sort et_stock by bwkey matnr bwtar sobkz vbeln posnr pspnr.
  loop at et_stock assigning <stock>.
    read table lt_bklas index sy-tabix
                               into ls_bklas.
    <stock>-bklas = ls_bklas-bklas.
  endloop.
endform.                               " GET_BKLAS

*&---------------------------------------------------------------------*
*&      Form read_total_records
*&---------------------------------------------------------------------*
form read_total_records
   using    i_t_stock                type t_stock
            i_bwkey                  type bwkey
   changing e_t_ckmlpp               type t_ckmlpp
            e_t_ckmlcr               type t_ckmlcr
            e_t_mlrunlist            type t_mlrunlist.

  data: l_stock   type s_stock,
        l_t_stock type t_stock.

  data: ls_ckmlpp type ckmlpp,
        ls_ckmlcr type ckmlcr,
        f_untper  type poper value '000',
        l_bdatj   type bdatj,
        l_poper   type poper.

  data: l_lfgja type mbew-lfgja,
        l_lfmon type mbew-lfmon,
        h_bdatj type bdatj,
        h_poper type poper.

  data: ls_periods        type cl_fml_join_ckmlpp_cr_buffer=>ts_gs_period,
        lt_periods        type cl_fml_join_ckmlpp_cr_buffer=>tt_gt_period,
        lt_ckmlcr         type cl_fml_join_ckmlpp_cr_buffer=>tt_gt_ckmlcr,
        lt_ckmlpp         type cl_fml_join_ckmlpp_cr_buffer=>tt_gt_ckmlpp,
        lt_period_ml_data type cl_fml_join_ckmlpp_cr_buffer=>tt_gt_ml_period_data,
        lt_kalnr          type ckmv0_matobj_tbl,
        ls_kalnr          type ckmv0_matobj_str,
        lt_ckmlhd         type standard table of ckmlhd,
        ls_mlrunlist      type mlrunlist.

  field-symbols: <s_ckmlpp> type ckmlpp,
                 <s_ckmlcr> type ckmlcr,
                 <s_stock>  type s_stock.

  clear: e_t_ckmlpp, e_t_ckmlcr.

  "note 2444258 -begin
  "- correction from S4H 1610 upwards
  "data will not be read from CKMLPP and CKMLCR but from the ACDOCA_M_EXTRACT
  "with corresponding joins on CKMLPP and CKMLCR

  l_t_stock = i_t_stock.

  clear lt_periods.
  loop at i_t_stock into l_stock.
    clear ls_periods.
    ls_periods-kalnr = l_stock-kalnr.
    ls_periods-idx = sy-tabix.
    move p_poper to  ls_periods-poper.
    move p_bdatj to ls_periods-bdatj .
    append ls_periods to lt_periods.
  endloop.

  sort l_t_stock by kalnr ascending mlast ascending.

* Preread Actual Costing data if actual costing is active
  if cl_fcml4h_actual_costing=>valuation_area_is_active( i_bwkey ) = abap_true.
    loop at l_t_stock assigning <s_stock>.
      ls_kalnr-kalnr = <s_stock>-kalnr.
      ls_kalnr-obtyp = 'MB'.         "Material
      append ls_kalnr to lt_kalnr.
    endloop.
    call function 'CKMS_HEADER_READ_WITH_ITAB'
      tables
        in_kalnr   = lt_kalnr
        out_ckmlhd = lt_ckmlhd.
    call function 'FCML4H_INV_BUFFER_REFRESH'.
    call function 'FCML4H_STATUS_BUFFER_REFRESH'.
    call function 'FCML4H_STATUS_PREREAD'
      exporting
        it_kalnr = lt_kalnr
        i_bdatj1 = p_bdatj
        i_poper1 = p_poper.
  endif.

  sort lt_periods by bdatj poper kalnr.
  delete adjacent duplicates from lt_periods comparing bdatj poper kalnr.
  sort lt_periods by idx.

  clear lt_period_ml_data.
  call method cl_fml_join_ckmlpp_cr_buffer=>get_most_recent_ml_pd_mass
    exporting
      iv_untper         = f_untper
      iv_bwkey          = i_bwkey
      it_kalnr          = lt_periods
      iv_poper          = p_poper
      iv_bdatj          = p_bdatj
    importing
      et_ml_period_data = lt_period_ml_data.

  if cl_fcml4h_actual_costing=>valuation_area_is_active( i_bwkey ) = abap_true.
    perform read_ending_inv_diff
     tables lt_kalnr
            lt_period_ml_data
      using p_bdatj
            p_poper.
  endif.
  "ALRS

  loop at lt_period_ml_data assigning field-symbol(<fs_period_ml_data>).

    if <fs_period_ml_data>-curtp = '10'.
      clear ls_ckmlpp.
      move-corresponding <fs_period_ml_data> to ls_ckmlpp.
      "note 2543809 - begin
      read table l_t_stock into l_stock with key kalnr = ls_ckmlpp-kalnr binary search.
      if sy-subrc = 0 and cl_fcml4h_actual_costing=>valuation_area_is_active( l_stock-bwkey ) = abap_true.
        "if actual costing is active in valuation area, get status from mlrunlist
        call function 'FCML4H_STATUS_GET'
          exporting
            i_runref  = 'ACT'
            i_kalnr   = <fs_period_ml_data>-kalnr
            i_bdatj   = p_bdatj
            i_poper   = p_poper
            i_otype   = 'MB'
          importing
            e_xsettle = ls_mlrunlist-xsettle
            e_xclose  = ls_mlrunlist-xclose
            e_status  = ls_ckmlpp-status.
        ls_mlrunlist-kalnr = <fs_period_ml_data>-kalnr.
        append ls_mlrunlist to e_t_mlrunlist.
      else. " note 2543809 - end
        if ls_ckmlpp-poper < p_poper or ls_ckmlpp-bdatj < p_bdatj.
          call function 'CKML_F_GET_NEW_STATUS'
            exporting
              i_vorgang    = y_periodenverschieber
              i_old_status = ls_ckmlpp-status
            importing
              o_new_status = ls_ckmlpp-status.
        endif.
      endif.
      ls_ckmlpp-bdatj  = p_bdatj.
      ls_ckmlpp-poper  = p_poper.
      append ls_ckmlpp to e_t_ckmlpp.
    endif.

    clear ls_ckmlcr.
    move-corresponding <fs_period_ml_data> to ls_ckmlcr.
    ls_ckmlcr-bdatj = p_bdatj.
    ls_ckmlcr-poper = p_poper.

    read table l_t_stock binary search with key kalnr = ls_ckmlpp-kalnr mlast = '3' transporting no fields.
    if sy-subrc = 0 and
      ls_ckmlpp-kalnr = ls_ckmlcr-kalnr and
      ls_ckmlpp-status ne y_mehrstufig_abgerechnet and
      ls_ckmlpp-status ne y_abschlussbuchung_storniert and
      ls_ckmlpp-status ne y_abschlussbuchung_erfolgt and
      ls_ckmlpp-status ne y_einstufig_abgerechnet.
      clear ls_ckmlcr-ebprd_ea.
      clear ls_ckmlcr-ebkdm_ea.
      clear ls_ckmlcr-ebprd_ma.
      clear ls_ckmlcr-ebkdm_ma.
    endif.
    append ls_ckmlcr to e_t_ckmlcr.
  endloop.




endform.                    "read_total_records

*&---------------------------------------------------------------------*
*&      Form  build_output
*&---------------------------------------------------------------------*
form build_output
  changing i_t_stock  type t_stock
           i_t_ckmlpp type t_ckmlpp
           i_t_ckmlcr type t_ckmlcr
           i_t_mlrunlist type t_mlrunlist
           e_t_out type t_out.

  data: l_out       type s_out,
        l_stock     type s_stock,
        l_ckmlpp    type ckmlpp,
        l_ckmlcr    type ckmlcr,
        l_makt      type makt,
        l_curr_info type s_curr_info,
        lt_makt     type t_makt.
  data: ls_mlrunlist type mlrunlist.


* merge the given tables together
  sort i_t_stock by kalnr.
  sort i_t_ckmlpp by kalnr.
  sort i_t_ckmlcr by kalnr curtp.
  sort i_t_mlrunlist by kalnr.

  "pick up text for material
  clear lt_makt.
  select  matnr, maktx from makt
            for all entries in @i_t_stock
            where matnr = @i_t_stock-matnr and spras = @sy-langu
    into corresponding fields of table @lt_makt.

  sort lt_makt by matnr.
  delete adjacent duplicates from lt_makt.

  "create output
  clear l_ckmlpp.
  loop at i_t_ckmlpp into l_ckmlpp.
*     check if state satisfy the given sel.-crit.
*    IF NOT l_ckmlpp-status IN r_status.
*      CONTINUE.
*    ENDIF.
*     fill output-line so far
    clear l_out.

    clear l_stock.
    read table i_t_stock binary search with key kalnr = l_ckmlpp-kalnr into l_stock.
    move-corresponding l_stock to l_out.

    l_out-lbkum   = l_ckmlpp-lbkum.
    l_out-bdatj   = l_ckmlpp-bdatj.
    l_out-poper   = l_ckmlpp-poper.

    "pferraz - performance - 29/11/23 - inicio
    l_out-werks = l_stock-bwkey.
    "pferraz - performance - 29/11/23 - fim

    clear ls_mlrunlist.
    read table i_t_mlrunlist into ls_mlrunlist
      with key kalnr = l_ckmlpp-kalnr
      binary search.
    if ls_mlrunlist-xsettle = 'X'.
      l_out-ico_xsettle = icon_led_green.
    else.
      if l_out-mlast = '3'.
        l_out-ico_xsettle = icon_led_red.
      else.
        l_out-ico_xsettle = icon_led_inactive.
      endif.
    endif.
    if ls_mlrunlist-xclose = 'X'.
      l_out-ico_xclose = icon_led_green.
    else.
      if l_out-mlast = '3'.
        l_out-ico_xclose = icon_led_red.
      else.
        l_out-ico_xclose = icon_led_inactive.
      endif.
    endif.

*     get texts for the materials
    if l_out-matnr = l_makt-matnr.
      l_out-ktext = l_makt-maktx.
    else.
      clear l_makt.
      read table lt_makt binary search with key matnr = l_out-matnr into l_makt.
      if sy-subrc = 0.
        l_out-ktext = l_makt-maktx.
      else.
        clear l_makt.
        clear l_out-ktext.
      endif.
    endif.

*     Process all currencies/valuations for one material
    read table i_t_ckmlcr binary search transporting no fields
              with key kalnr = l_ckmlpp-kalnr.
    clear l_ckmlcr.
    loop at i_t_ckmlcr into l_ckmlcr from sy-tabix.

      if l_ckmlpp-kalnr <> l_ckmlcr-kalnr.
        exit.
      else.

*       fill in the keyfigures
        l_out-curtp   = l_ckmlcr-curtp.
        l_out-salk3   = l_ckmlcr-salk3.
        l_out-salkv   = l_ckmlcr-salkv.
        l_out-eb_dif   = l_ckmlcr-ebprd_ea + l_ckmlcr-ebprd_ma +
                        l_ckmlcr-ebkdm_ea + l_ckmlcr-ebkdm_ma.
        l_out-vprsv   = l_ckmlcr-vprsv.
        l_out-stprs   = l_ckmlcr-stprs.
        l_out-pvprs   = l_ckmlcr-pvprs.
        perform calc_prabw_prz using l_out-stprs l_out-pvprs
                               changing l_out-prabw_prz.
        l_out-peinh   = l_ckmlcr-peinh.
        read table g_t_curr_info with table key curtp = l_ckmlcr-curtp  bwkey = l_stock-bwkey into l_curr_info.
        l_out-waers = l_curr_info-waers.

*       append the output-line to the output-table
        append l_out to e_t_out.
      endif.
    endloop.
  endloop.

endform.                    "build_output
*&---------------------------------------------------------------------*
*&      Form read_total_records_rldnr
*&---------------------------------------------------------------------*
form read_total_records_rldnr
   using    i_t_stock                type t_stock
            i_bwkey                  type bwkey
   changing e_t_ml                   type cl_fml_join_ckmlpp_cr_buffer=>tt_ml_period_data_with_curtp
            e_t_mlrunlist            type t_mlrunlist.

  data lt_key type if_fml_ckmlpp_ckmlcr_rd_facade=>tt_key.
  data ls_key type if_fml_ckmlpp_ckmlcr_rd_facade=>ts_key.
  data: lo_facade type ref to if_fml_ckmlpp_ckmlcr_rd_facade.
  data: lt_kalnr          type ckmv0_matobj_tbl.
  data: ls_kalnr          type ckmv0_matobj_str.
  data: lt_unique_curtype type if_fml_inv_qty_val_price_amdp=>tt_price_curtype,
        lt_ckmlhd         type standard table of ckmlhd,
        lt_cr_pp_with_act type cl_fml_join_ckmlpp_cr_buffer=>tt_ml_period_data_with_curtp.
  data: l_stock                 type s_stock,
        l_t_stock               type t_stock,
        lt_period_ml_data_rldnr type cl_fml_join_ckmlpp_cr_buffer=>tt_ml_period_data_with_curtp,
        ls_mlrunlist            type mlrunlist.

  field-symbols: <s_stock>  type s_stock.

  l_t_stock = i_t_stock.
  sort l_t_stock by kalnr ascending mlast ascending.

* Preread Actual Costing data if actual costing is active
  if cl_fcml4h_actual_costing=>valuation_area_is_active( i_bwkey ) = abap_true.
    loop at l_t_stock assigning <s_stock>.
      ls_kalnr-kalnr = <s_stock>-kalnr.
      ls_kalnr-obtyp = 'MB'.         "Material
      append ls_kalnr to lt_kalnr.
    endloop.
    call function 'CKMS_HEADER_READ_WITH_ITAB'
      tables
        in_kalnr   = lt_kalnr
        out_ckmlhd = lt_ckmlhd.
    call function 'FCML4H_INV_BUFFER_REFRESH'.
    call function 'FCML4H_STATUS_BUFFER_REFRESH'.
    call function 'FCML4H_STATUS_PREREAD'
      exporting
        it_kalnr = lt_kalnr
        i_bdatj1 = p_bdatj
        i_poper1 = p_poper.
*  TODO - after Ralf provide the Function moudle.
  endif.

  loop at i_t_stock assigning field-symbol(<fs_stock>).
    ls_key-kalnr  = <fs_stock>-kalnr.
    ls_key-bdatj  = p_bdatj.
    ls_key-poper  = p_poper.
    append ls_key to lt_key.
  endloop.
  sort lt_key by kalnr bdatj poper .
  delete adjacent duplicates from lt_key comparing kalnr bdatj poper.

  clear lt_period_ml_data_rldnr.
  lo_facade = cl_fml_ckmlpp_ckmlcr_factory=>get_factory( )->get_read_facade( ).
  lo_facade->get_pp_cr_quant_val_price_amdp( exporting it_key                    = lt_key
                                                       it_unique_curtype         = corresponding #( g_t_curr_info )
                                             importing et_ckmlcr_ckmlpp_combined = data(lt_cr_pp_combined) ).

  lt_period_ml_data_rldnr = corresponding #( lt_cr_pp_combined ).

  if cl_fcml4h_actual_costing=>valuation_area_is_active( i_bwkey ) = abap_true.
    perform read_ending_inv_diff_rldnr
     tables lt_kalnr
            lt_period_ml_data_rldnr
      using p_bdatj
            p_poper.
  endif.

  sort lt_period_ml_data_rldnr by kalnr rldnr.
  loop at lt_period_ml_data_rldnr assigning field-symbol(<fs_ml_rldnr>) group by <fs_ml_rldnr>-kalnr
                                             into data(lv_kalnr).
    loop at group lv_kalnr assigning field-symbol(<fs_group_kalnr>) group by <fs_group_kalnr>-rldnr
                                             into data(lv_rldnr) .
      read table l_t_stock into l_stock with key kalnr = lv_kalnr binary search.
      if sy-subrc = 0 and  cl_fcml4h_actual_costing=>valuation_area_is_active( l_stock-bwkey ) = abap_true.

        call function 'FCML4H_STATUS_GET'
          exporting
            i_runref  = cl_ml4h_run_util=>get_instance( )->get_runref_for_ledger_run( lv_rldnr )
            i_kalnr   = lv_kalnr
            i_bdatj   = p_bdatj
            i_poper   = p_poper
            i_otype   = 'MB'
          importing
            e_xsettle = ls_mlrunlist-xsettle
            e_xclose  = ls_mlrunlist-xclose.
        ls_mlrunlist-kalnr = lv_kalnr.
        append ls_mlrunlist to e_t_mlrunlist.

      endif.
    endloop.
  endloop.

  e_t_ml = corresponding #( lt_period_ml_data_rldnr ).

endform.                    "read_total_records_rldnr

*&---------------------------------------------------------------------*
*&      Form  build_output_rldnr
*&---------------------------------------------------------------------*
form build_output_rldnr
  changing i_t_stock  type t_stock
           i_t_ml type cl_fml_join_ckmlpp_cr_buffer=>tt_ml_period_data_with_curtp
           i_t_mlrunlist type t_mlrunlist
           e_t_out type t_out.

  data: l_out        type s_out,
        l_stock      type s_stock,
        l_makt       type makt,
        l_curr_info  type s_curr_info,
        lt_makt      type t_makt,
        ls_mlrunlist type mlrunlist.

* merge the given tables together
  sort i_t_stock by kalnr.

  "pick up text for material
  clear lt_makt.
  select  matnr, maktx from makt
            for all entries in @i_t_stock
            where matnr = @i_t_stock-matnr and spras = @sy-langu
    into corresponding fields of table @lt_makt.

  sort lt_makt by matnr.
  delete adjacent duplicates from lt_makt.

  "create output
  loop at i_t_ml assigning field-symbol(<fs_t_ml>).
    clear l_out.
    clear l_stock.
    read table i_t_stock binary search with key kalnr = <fs_t_ml>-kalnr into l_stock.
    move-corresponding l_stock to l_out.
    move-corresponding <fs_t_ml> to l_out.

    clear ls_mlrunlist.
    read table i_t_mlrunlist into ls_mlrunlist
      with key kalnr = <fs_t_ml>-kalnr
      binary search.
    if ls_mlrunlist-xsettle = 'X'.
      l_out-ico_xsettle = icon_led_green.
    else.
      if l_out-mlast = '3'.
        l_out-ico_xsettle = icon_led_red.
      else.
        l_out-ico_xsettle = icon_led_inactive.
      endif.
    endif.
    if ls_mlrunlist-xclose = 'X'.
      l_out-ico_xclose = icon_led_green.
    else.
      if l_out-mlast = '3'.
        l_out-ico_xclose = icon_led_red.
      else.
        l_out-ico_xclose = icon_led_inactive.
      endif.
    endif.

*     get texts for the materials
    if l_out-matnr = l_makt-matnr.
      l_out-ktext = l_makt-maktx.
    else.
      clear l_makt.
      read table lt_makt binary search with key matnr = l_out-matnr into l_makt.
      if sy-subrc = 0.
        l_out-ktext = l_makt-maktx.
      else.
        clear l_makt.
        clear l_out-ktext.
      endif.
    endif.

    perform calc_prabw_prz using l_out-stprs l_out-pvprs
                           changing l_out-prabw_prz.
    read table g_t_curr_info with key curtp = <fs_t_ml>-curtp
                                      bwkey = l_stock-bwkey
                                      rldnr = <fs_t_ml>-rldnr
                             into l_curr_info.
    append l_out to e_t_out.
  endloop.

endform.                    "build_output_rldnr
*&---------------------------------------------------------------------*
*&      Form read_ending_inv_diff_rldnr
*&---------------------------------------------------------------------*
form read_ending_inv_diff_rldnr tables it_kalnr type ckmv0_matobj_tbl
         ct_period_ml_data type cl_fml_join_ckmlpp_cr_buffer=>tt_ml_period_data_with_curtp
   using i_bdatj           type bdatj
         i_poper           type poper.

  data:
    begin of lt_eidiff occurs 0,
      kalnr     type ck_kalnr,
      rldnr     type fins_ledger,
      curtp     type fins_curtype,
      jahrper   type ml4h_jahrper,
      runref    type ml4h_runref,
      prd       type ml4h_prd,
      kdm       type ml4h_kdm,
      xkdm_nact type ml4h_kdm_nact,
    end of lt_eidiff,
    lt_kalnr_range type range of ck_kalnr,
    ls_kalnr_range like line of lt_kalnr_range,
    ld_jahrper     type ml4h_jahrper.
*
  field-symbols:
    <ls_kalnr>  type ckmv0_matobj_str,
    <ls_eidiff> like line of lt_eidiff.
*    <ls_ml_data> TYPE cl_fml_join_ckmlpp_cr_buffer=>ts_gs_ml_period_data.
*
  ld_jahrper+0(4) = i_bdatj.
  ld_jahrper+4(3) = i_poper.

  ls_kalnr_range-sign   = 'I'.
  ls_kalnr_range-option = 'EQ'.
*
  loop at it_kalnr assigning <ls_kalnr>.
    ls_kalnr_range-low = <ls_kalnr>-kalnr.
    append ls_kalnr_range to lt_kalnr_range.
  endloop.
*
  select kalnr      as kalnr,
         rldnr      as rldnr,
         curtp      as curtp,
         jahrper    as jahrper,
         runref     as runref,
         xkdm_nact  as xkdm_nact,
         sum( prd ) as prd,
         sum( kdm ) as kdm
    from mldoc into corresponding fields of table @lt_eidiff
    where kalnr   in @lt_kalnr_range
      and jahrper = @ld_jahrper
      and runref like 'ACT%'
      and categ   = 'EB'
      and posart  = 'ST'
    group by kalnr, rldnr, curtp, jahrper, runref, xkdm_nact.

  sort lt_eidiff by kalnr rldnr curtp.
  sort ct_period_ml_data by kalnr rldnr curtp.

  loop at ct_period_ml_data assigning field-symbol(<ls_ml_data>).
    read table lt_eidiff transporting no fields
      with key kalnr = <ls_ml_data>-kalnr
               rldnr = <ls_ml_data>-rldnr
               curtp = <ls_ml_data>-curtp
      binary search.
    if sy-subrc = 0.
      clear: <ls_ml_data>-ebprd_ea,
             <ls_ml_data>-ebkdm_ea.
      loop at lt_eidiff from sy-tabix assigning <ls_eidiff>.
        if <ls_eidiff>-kalnr ne <ls_ml_data>-kalnr or
           <ls_eidiff>-curtp ne <ls_ml_data>-curtp.
          exit.
        endif.
        subtract <ls_eidiff>-prd from <ls_ml_data>-ebprd_ea.  "In database they have inverted sign
        if <ls_eidiff>-xkdm_nact is initial.                  "as we want to show them
          subtract <ls_eidiff>-kdm from <ls_ml_data>-ebkdm_ea.
        endif.
      endloop.
    endif.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  get_text
*&---------------------------------------------------------------------*
form get_text
  using i_fieldname
        i_value
  changing e_text.

  data: l_s_status type s_status,
        l_status   type s_status-status.

  clear e_text.

  case i_fieldname.
    when 'STATUS'.
      l_status = i_value.
      read table g_t_status into l_s_status
                            with key status = l_status.
      e_text = l_s_status-text.
  endcase.
endform.                    "get_text

*&---------------------------------------------------------------------*
*&      Form  calc_prabw_prz
*&---------------------------------------------------------------------*
form calc_prabw_prz
  using i_stprs type s_out-stprs
        i_pvprs type s_out-pvprs
  changing e_prabw_prz type s_out-prabw_prz.

  data: l_diff type s_out-stprs.
  data: l_float type float.

  clear e_prabw_prz.
  l_diff = i_pvprs - i_stprs.
  if l_diff <> 0.
    e_prabw_prz = 999999.
    if i_stprs <> 0.
      l_float = ( l_diff / i_stprs ) * 100.
      if l_float <= 999999.
        e_prabw_prz = l_float.
      endif.
    endif.
  endif.
endform.                    "calc_prabw_prz

*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
form display_list.
  data: lt_sp_groups type slis_t_sp_group_alv,
        l_t_fieldcat type slis_t_fieldcat_alv,
        l_layout     type slis_layout_alv,
        l_t_sort     type slis_t_sortinfo_alv,
        l_sort       type slis_sortinfo_alv,
        l_t_event    type slis_t_event,
        l_event      type slis_alv_event.
  data: l_repid      like sy-repid.

* if sales order/project stocks do not exist => do not show the key
* fields
  field-symbols: <out> type s_out.
  data: l_xbwtar,
        l_xvbeln,
        l_xpspnr.
  loop at g_t_alv_list assigning <out>.
    if not <out>-bwtar is initial.
      l_xbwtar = 'X'.
    endif.
    if not <out>-vbeln is initial.
      l_xvbeln = 'X'.
    endif.
    if not <out>-pspnr is initial.
      l_xpspnr = 'X'.
    endif.
  endloop.

* field groups
  perform set_fieldgroups changing lt_sp_groups.

* fieldcat
  perform build_fieldcat using l_xbwtar l_xvbeln l_xpspnr
                         changing l_t_fieldcat.

* layout
  l_layout-colwidth_optimize = 'X'.
  l_layout-get_selinfos = 'X'.
  l_layout-f2code = 'DETAIL'.

* Events
  l_event-name = slis_ev_top_of_page.
  l_event-form = 'ALV_TOP_OF_PAGE'.
  append l_event to l_t_event.

* Sort
  l_sort-fieldname = 'MTART'.
  l_sort-spos = 1.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  append l_sort to l_t_sort.

  clear l_sort.
  l_sort-fieldname = 'BKLAS'.
  l_sort-spos = 2.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  append l_sort to l_t_sort.

  clear l_sort.
  l_sort-fieldname = 'MATNR'.
  l_sort-spos = 3.
  l_sort-up = 'X'.
  append l_sort to l_t_sort.

  clear l_sort.
  l_sort-fieldname = 'BWTAR'.
  l_sort-spos = 4.
  l_sort-up = 'X'.
  append l_sort to l_t_sort.

  clear l_sort.
  l_sort-fieldname = 'VBELN'.
  l_sort-spos = 5.
  l_sort-up = 'X'.
  append l_sort to l_t_sort.

  clear l_sort.
  l_sort-fieldname = 'POSNR'.
  l_sort-spos = 6.
  l_sort-up = 'X'.
  append l_sort to l_t_sort.

  clear l_sort.
  l_sort-fieldname = 'PSPNR'.
  l_sort-spos = 7.
  l_sort-up = 'X'.
  append l_sort to l_t_sort.

* call ALV
  l_repid = sy-repid.
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
*     i_interface_check        = 'X'
      i_callback_program       = l_repid
      i_callback_pf_status_set = 'ALV_PF_STATUS_SET'
      i_callback_user_command  = 'ALV_USER_COMMAND'
*     I_STRUCTURE_NAME         =
      is_layout                = l_layout
      it_fieldcat              = l_t_fieldcat
*     IT_EXCLUDING             =
      it_special_groups        = lt_sp_groups
      it_sort                  = l_t_sort
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
      is_variant               = is_variant
      it_events                = l_t_event
    tables
      t_outtab                 = g_t_alv_list
    exceptions
      program_error            = 1
      others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
           with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    "display_list

*&---------------------------------------------------------------------*
*&      Form  set_fieldgroups
*&---------------------------------------------------------------------*
form set_fieldgroups
  changing ct_sp_groups type slis_t_sp_group_alv.

  data: lf_sp_group type slis_sp_group_alv.

  lf_sp_group-sp_group = '0001'.
  append lf_sp_group to ct_sp_groups.
  lf_sp_group-sp_group = '0002'.
  append lf_sp_group to ct_sp_groups.
  lf_sp_group-sp_group = '0003'.
  append lf_sp_group to ct_sp_groups.
  lf_sp_group-sp_group = '0004'.
  append lf_sp_group to ct_sp_groups.
  lf_sp_group-sp_group = '0005'.
  append lf_sp_group to ct_sp_groups.
  lf_sp_group-sp_group = '0006'.
  append lf_sp_group to ct_sp_groups.

  call function 'SET_FIELDGROUP_TEXTS'
    changing
      ct_special_groups = ct_sp_groups.
endform.                    "set_fieldgroups

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
form build_fieldcat
  using i_xbwtar i_xvbeln i_xpspnr
  changing e_t_fieldcat type slis_t_fieldcat_alv.

  data: l_fieldcat type slis_fieldcat_alv,
        l_col_pos  type i.


  clear l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'WERKS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'WERKS'.
  l_fieldcat-ref_tabname = 'T001W'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-sp_group = '0005'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'GSBER'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'GSBER'.
  l_fieldcat-ref_tabname = 'T134G'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-sp_group = '0005'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'MTART'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MTART'.
  l_fieldcat-ref_tabname = 'MARA'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-sp_group = '0005'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'KONTS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'KONTS'.
  l_fieldcat-ref_tabname = 'T030'.
  l_fieldcat-sp_group = '0006'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'SPART'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'SPART'.
  l_fieldcat-ref_tabname = 'MARA'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0005'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'MATKL'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MATKL'.
  l_fieldcat-ref_tabname = 'MARA'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0005'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'BKLAS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'BKLAS'.
  l_fieldcat-ref_tabname = 'MBEW'.
  l_fieldcat-key = ' '.
  l_fieldcat-sp_group = '0005'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'MATNR'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MATNR'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  l_fieldcat-sp_group = '0005'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_fieldcat-col_pos + 1.
  l_fieldcat-fieldname = 'KTEXT'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MAKTX'.
  l_fieldcat-ref_tabname = 'MAKT'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0005'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'BWTAR'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'BWTAR'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  if i_xbwtar is initial.
    l_fieldcat-no_out = 'X'.
  endif.
  l_fieldcat-sp_group = '0005'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'VBELN'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'VBELN'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  l_fieldcat-ddictxt = 'L'.
  if i_xvbeln is initial.
    l_fieldcat-no_out = 'X'.
  endif.
  l_fieldcat-sp_group = '0004'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'POSNR'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'POSNR'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  if i_xvbeln is initial.
    l_fieldcat-no_out = 'X'.
  endif.
  l_fieldcat-sp_group = '0004'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'PSPNR'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'PSPNR'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  l_fieldcat-ddictxt = 'L'.
  if i_xpspnr is initial.
    l_fieldcat-no_out = 'X'.
  endif.
  l_fieldcat-sp_group = '0004'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'LBKUM'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'LBKUM'.
  l_fieldcat-ref_tabname = 'CKMLPP'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-qfieldname = 'MEINS'.
  l_fieldcat-sp_group = '0003'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'MEINS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MEINS'.
  l_fieldcat-ref_tabname = 'MARA'.
  l_fieldcat-sp_group = '0005'.
  if   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    log-point id /cwm/enh subkey to_upper( sy-tcode && '\/CWM/APPL_MM_RKKBMLMAT_EHP4\RKKBMLMAT_02\' && sy-cprog ) fields /cwm/cl_enh_layer=>get_field( ).
    l_fieldcat-ref_tabname = 'CKMLPP'.
  endif.
*ENHANCEMENT-POINT rkkbmlmat_02 SPOTS es_rkkbmlmat .
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'SALK3'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'SALK3'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-cfieldname = 'WAERS'.
  l_fieldcat-do_sum = 'X'.
  l_fieldcat-sp_group = '0001'.
  append l_fieldcat to e_t_fieldcat.


  "SALK3_U
  if r_st_d = 'X'.
    clear l_fieldcat.
    l_col_pos = l_col_pos + 1.
    l_fieldcat-fieldname = 'SALK3_U'.
    l_fieldcat-tabname = 'G_T_ALV_LIST'.
    l_fieldcat-ref_fieldname = 'SALK3'.
    l_fieldcat-ref_tabname = 'CKMLCR'.
    l_fieldcat-cfieldname = 'WAERS'.
    l_fieldcat-do_sum = 'X'.
    l_fieldcat-sp_group = '0001'.
*    L_FIELDCAT-OUTPUTLEN = 15.
    concatenate 'Vlr Total' 'USD' into  l_fieldcat-seltext_l separated by space.
    l_fieldcat-seltext_s = l_fieldcat-seltext_l.
    l_fieldcat-seltext_m = l_fieldcat-seltext_l.
    append l_fieldcat to e_t_fieldcat.
  endif.

  if r_st_d = 'X'.
    clear l_fieldcat.
    l_col_pos = l_col_pos + 1.
    l_fieldcat-fieldname = 'SALKV_U'.
    l_fieldcat-tabname = 'G_T_ALV_LIST'.
    l_fieldcat-ref_fieldname = 'SALK3'.
    l_fieldcat-ref_tabname = 'CKMLCR'.
    l_fieldcat-cfieldname = 'WAERS'.
    l_fieldcat-do_sum = 'X'.
    l_fieldcat-sp_group = '0001'.
*    L_FIELDCAT-OUTPUTLEN = 15.
    concatenate 'Valor/Preço PIP' 'USD' into  l_fieldcat-seltext_l separated by space.
    l_fieldcat-seltext_s = l_fieldcat-seltext_l.
    l_fieldcat-seltext_m = l_fieldcat-seltext_l.
    append l_fieldcat to e_t_fieldcat.
  endif.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'SALKV'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'SALKV'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-ddictxt = 'L'.
  l_fieldcat-cfieldname = 'WAERS'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0001'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'EB_DIF'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'EB_DIF'.
  l_fieldcat-ref_tabname = 'CKI_DOC_ML'.
  l_fieldcat-cfieldname = 'WAERS'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0002'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'MLAST'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'MLAST'.
  l_fieldcat-ref_tabname = 'CKMLHD'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0001'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'ICO_XSETTLE'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'XSETTLE'.
  l_fieldcat-ref_tabname = 'MLRUNLIST'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0006'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'ICO_XCLOSE'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'XCLOSE'.
  l_fieldcat-ref_tabname = 'MLRUNLIST'.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-sp_group = '0006'.
  append l_fieldcat to e_t_fieldcat.


  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'VPRSV'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'VPRSV'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-sp_group = '0001'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'STPRS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'STPRS'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-cfieldname = 'WAERS'.
  l_fieldcat-no_sum = 'X'.
  l_fieldcat-sp_group = '0001'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'PVPRS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'PVPRS'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-cfieldname = 'WAERS'.
  l_fieldcat-no_sum = 'X'.
  l_fieldcat-sp_group = '0001'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'PRABW_PRZ'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'PRABW_PRZ'.
  l_fieldcat-ref_tabname = 'CKML_RUN_ALV_LIST01 '.
  l_fieldcat-no_out = 'X'.
  l_fieldcat-no_sum = 'X'.
  l_fieldcat-sp_group = '0001'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'PEINH'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'PEINH'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-no_sum = 'X'.
  l_fieldcat-sp_group = '0001'.
  append l_fieldcat to e_t_fieldcat.

  clear l_fieldcat.
  l_col_pos = l_col_pos + 1.
  l_fieldcat-fieldname = 'WAERS'.
  l_fieldcat-tabname = 'G_T_ALV_LIST'.
  l_fieldcat-ref_fieldname = 'WAERS'.
  l_fieldcat-ref_tabname = 'CKMLCR'.
  l_fieldcat-sp_group = '0001'.
  append l_fieldcat to e_t_fieldcat.
endform.                    "build_fieldcat

*---------------------------------------------------------------------*
*       FORM ALV_PF_STATUS_SET
*---------------------------------------------------------------------*
form alv_pf_status_set
using rt_extab type slis_t_extab.
* perform alv_exclude_fcode using '&LFO' changing rt_extab.
  set pf-status 'STANDARD_FULLSCREEN' excluding rt_extab.
endform.                    "alv_pf_status_set

*---------------------------------------------------------------------*
*       FORM alv_exclude_fcode
*---------------------------------------------------------------------*
form alv_exclude_fcode
using i_fcode
changing e_t_extab type slis_t_extab.
  data: l_extab type slis_extab.
  l_extab-fcode = i_fcode.
  collect l_extab into e_t_extab.
endform.                    "alv_exclude_fcode

*---------------------------------------------------------------------*
*       FORM ALV_USER_COMMAND
*---------------------------------------------------------------------*
form alv_user_command
using r_ucomm like sy-ucomm
      rs_selfield type slis_selfield.

  case r_ucomm.
    when 'CURR'.
      perform select_currency.
      rs_selfield-refresh = 'X'.
      rs_selfield-col_stable = 'X'.
      rs_selfield-row_stable = 'X'.
    when 'DETAIL'.
      perform display_detail using rs_selfield.
    when others.
  endcase.
endform.                    "alv_user_command

*---------------------------------------------------------------------*
*       FORM select_currency
*---------------------------------------------------------------------*
*       (1) display popup to choose a currency
*       (2) modify alv-list according to this currency
*---------------------------------------------------------------------*
form select_currency.

  data: l_curr_info   type s_curr_info,
        l_curtp       type curtp,
        l_rldnr       type rldnr,
        l_ext_curtype type fins_ext_curtype,
        lt_mapping    type standard table of dselc.
  data: begin of l_value,
          ext_curtype type cki_ml_cty-ext_curtype,
          rldnr       type cki_ml_cty-rldnr,
          text        type cki_ml_cty-text,
          waers       type cki_ml_cty-waers,
          "          curtp       TYPE cki_ml_cty-curtp,
        end of l_value,
        l_t_value  like l_value occurs 0,
        l_return   type ddshretval,
        l_t_return type ddshretval occurs 0.

* (1) display popup to choose a currency

  loop at g_t_curr_info into l_curr_info.
    move-corresponding l_curr_info to l_value.
    append l_value to l_t_value.
  endloop.

  lt_mapping = value #( ( fldname = 'F0001' dyfldname = 'EXT_CURTYPE' )
                        ( fldname = 'F0002' dyfldname = 'RLDNR' ) ).

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'EXT_CURTYPE'
      value_org       = 'S'
    tables
      value_tab       = l_t_value
      return_tab      = l_t_return
      dynpfld_mapping = lt_mapping
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

* (2) modify alv-list according to this currency
  if sy-subrc = 0.
    read table l_t_return assigning field-symbol(<fs_curtp>) with key retfield = 'EXT_CURTYPE'.
    if sy-subrc = 0.
      l_ext_curtype = <fs_curtp>-fieldval.
    endif.
    read table l_t_return assigning field-symbol(<fs_rldnr>) with key retfield = 'RLDNR'.
    if sy-subrc = 0.
      l_rldnr = <fs_rldnr>-fieldval.
    endif.
    read table g_t_curr_info into g_curr_info
                               with key ext_curtype =  l_ext_curtype
                                        rldnr = l_rldnr.
    perform filter_currency using g_t_out g_curr_info-curtp g_curr_info-rldnr
                            changing g_t_alv_list.

  endif.
endform.                    "select_currency

*CLASS lcl_event_handler_curval IMPLEMENTATION.
*
*  METHOD on_double_click.
*    DATA: lt_rows TYPE salv_t_row.
*
*    lt_rows = gr_table_curval->get_selections( )->get_selected_rows( ).
*    IF lines( lt_rows ) = 1.
*      READ TABLE gt_table_curval INDEX lt_rows[ 1 ] INTO DATA(ls_row).
*
*      f_dyn-pickcurtp = ls_row-ledgervaluationcurrencyrole.
*    ENDIF.
*
*    gr_table_curval->close_screen( ).
*    gr_table_curval->refresh( ).
*  ENDMETHOD.
*
*  METHOD on_before_salv_functions.
*    DATA: lt_rows TYPE salv_t_row.
*    CASE e_salv_function.
*      WHEN '&ONT'.
*        lt_rows = gr_table_curval->get_selections( )->get_selected_rows( ).
*        IF lines( lt_rows ) = 1.
*          READ TABLE gt_table_curval INDEX lt_rows[ 1 ] INTO DATA(ls_row).
*
*          f_dyn-pickcurtp = ls_row-ledgervaluationcurrencyrole.
*        ENDIF.
*    ENDCASE.
*
*    gr_table_curval->close_screen( ).
*    gr_table_curval->refresh( ).
*  ENDMETHOD.
*
*ENDCLASS.
*---------------------------------------------------------------------*
*       FORM display_detail
*---------------------------------------------------------------------*
*       show ML-data for the selected material
*---------------------------------------------------------------------*
form display_detail
  using i_selfield type slis_selfield.

  data: l_out type s_out.

  if i_selfield-tabindex > 0.
    read table g_t_alv_list index i_selfield-tabindex into l_out.
    if sy-subrc = 0.
      call function 'CKM8_ML_DATA_DISPLAY'
        exporting
          i_matnr = l_out-matnr
          i_bwkey = l_out-bwkey
          i_bwtar = l_out-bwtar
          i_vbeln = l_out-vbeln
          i_posnr = l_out-posnr
          i_pspnr = l_out-pspnr
          i_bdatj = l_out-bdatj
          i_poper = l_out-poper
          i_curtp = g_curr_info-curtp.
    endif.
  else.
  endif.
endform.                    "display_detail

*---------------------------------------------------------------------*
*       FORM ALV_TOP_OF_page                                          *
*---------------------------------------------------------------------*
form alv_top_of_page.
  data: lf_comment type slis_listheader,
        lt_comment type slis_t_listheader,
        l_jahrper  type jahrper.

* Bewertungsebene
  clear: lf_comment.
  lf_comment-typ = 'S'.
  case tcurm-bwkrs_cus.                "Bewertungsebene
    when 1.
      write 'Werk'(004) to lf_comment-key.
    when 3.
      write 'Buchungskreis'(005) to lf_comment-key.
    when others.
      write '? ? ? ? ? ?'(006) to lf_comment-key.
  endcase.
  write p_bwkey to lf_comment-info.
  append lf_comment to lt_comment.

* Periode
  clear: lf_comment.
  lf_comment-typ = 'S'.
  write 'Periode'(002) to lf_comment-key.
  call function 'K_KKB_JAHRPER_SET'
    exporting
      i_jahr    = p_bdatj
      i_periode = p_poper
    importing
      o_jahrper = l_jahrper.
  write l_jahrper to lf_comment-info.
  append lf_comment to lt_comment.

  select single bukrs from t001k into @data(lv_bukrs) where bwkey = @p_bwkey.
  select single * from r_matlledgervalncrcyrole
        into @data(ls_mlvalncrcyrole)
        where companycode = @lv_bukrs
        and ledgervaluationcurrencyrole = @g_curr_info-curtp.

  "Empresa
  select single butxt
    from t001
    into @data(lv_name)
    where bukrs = @lv_bukrs.
  if sy-subrc = 0.
    lf_comment-typ = 'S'.
    lf_comment-key = 'Empresa'.
    lf_comment-info = lv_name.
    append lf_comment to lt_comment.
    clear: lf_comment.
  endif.



  data: lv_comment_help type c length 62.
  data: ls_dfies type dfies.

  " add ledger information
  clear: lf_comment, lv_comment_help.
  lf_comment-typ = 'S'.
  lf_comment-key = text-012.
  if ls_mlvalncrcyrole-ledgername is not initial.
    concatenate '(' ls_mlvalncrcyrole-ledgername ')' into lv_comment_help.
    concatenate ls_mlvalncrcyrole-ledger lv_comment_help into lf_comment-info separated by space.
  else.
    lf_comment-info = ls_mlvalncrcyrole-ledger.
  endif.
  append lf_comment to lt_comment.

  " add external Currency Type/Valuation View information
  clear: lf_comment, lv_comment_help.
  call function 'DDIF_FIELDINFO_GET'
    exporting
      tabname        = 'FINS_EXT_CURTYPE'
      langu          = sy-langu         " Language of the Texts
      all_types      = 'X'
    importing
      dfies_wa       = ls_dfies
    exceptions
      not_found      = 1
      internal_error = 2
      others         = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  lf_comment-typ = 'S'.
  lf_comment-key = ls_dfies-scrtext_m.
  if ls_mlvalncrcyrole-currencyrolename is not initial.
    concatenate '(' ls_mlvalncrcyrole-currencyrolename ')' into lv_comment_help.
    concatenate ls_mlvalncrcyrole-currencyrole lv_comment_help into lf_comment-info separated by space.
  else.
    lf_comment-info = ls_mlvalncrcyrole-currencyrole.
  endif.
  append lf_comment to lt_comment.

  " add Currency Key information
  clear: lf_comment.
  lf_comment-typ = 'S'.
  lf_comment-key = text-009.
  lf_comment-info = ls_mlvalncrcyrole-currency.
  append lf_comment to lt_comment.

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = lt_comment
*     I_LOGO             =
    .
endform.                    "alv_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_caller_exit_at_start
*---------------------------------------------------------------------*
form alv_caller_exit_at_start.

  call function 'REUSE_ALV_LIST_LAYOUT_INFO_GET'
*      IMPORTING
*           ES_LAYOUT                  =
*           ET_FIELDCAT                =
*           ET_SORT                    =
*           ET_FILTER                  =
*           ES_LIST_SCROLL             =
*           ES_VARIANT                 =
*           E_WIDTH                    =
*           ET_MARKED_COLUMNS          =
*           ET_FILTERED_ENTRIES        =
*           ET_FILTERED_ENTRIES_HEADER =
*           ET_FILTERED_ENTRIES_ITEM   =
*      TABLES
*           ET_OUTTAB                  =
*           ET_OUTTAB_HEADER           =
*           ET_OUTTAB_ITEM             =
*           ET_COLLECT00               =
*           ET_COLLECT01               =
*           ET_COLLECT02               =
*           ET_COLLECT03               =
*           ET_COLLECT04               =
*           ET_COLLECT05               =
*           ET_COLLECT06               =
*           ET_COLLECT07               =
*           ET_COLLECT08               =
*           ET_COLLECT09               =
*      EXCEPTIONS
*           NO_INFOS                   = 1
*           PROGRAM_ERROR              = 2
*           OTHERS                     = 3
    .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
endform.                    "alv_caller_exit_at_start

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
form user_command.

  case sscrfields-ucomm.
    when 'EXTR'.
* extracts for alv-list
      call selection-screen '4500' starting at 1 1.
    when 'EXON'.
* extended selections ON
      p_ex_sel = 'X'.
    when 'EXOFF'.
* extended selections OFF
      clear p_ex_sel.
    when 'ONLI'.
* go for it
      authority-check object 'K_ML_VA'
      id 'BWKEY' field p_bwkey
      id 'ACTVT' field '03'.
      if sy-subrc ne 0.
        message e054(c+) with p_bwkey. "Keine Berechtigung zum
        "Ausführen
      endif.
  endcase.

endform.                               " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
form f4_for_variant.
  data: locl_variant like disvariant.

  clear exit.
  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = is_variant
      i_save     = 'A'
    importing
      e_exit     = exit
      es_variant = locl_variant
    exceptions
      not_found  = 2.
  if sy-subrc = 2.
    message id sy-msgid type 'S'  number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    if exit = space.
      p_varian = locl_variant-variant.
      p_vartxt = locl_variant-text.
      move locl_variant to is_variant.
    endif.
  endif.

endform.                               " F4_FOR_VARIANT
*&----------------------------------------------------------------------
*&      Form  VARIANT_CHECK_EXISTENCE
*&----------------------------------------------------------------------
form variant_check_existence.
  data: locl_variant like disvariant.

  if not p_varian is initial.
    move is_variant to locl_variant.
    move p_varian to locl_variant-variant.

    call function 'REUSE_ALV_VARIANT_EXISTENCE'
      exporting
        i_save     = 'A'
      changing
        cs_variant = locl_variant.
    is_variant = locl_variant.
    p_vartxt = is_variant-text.
  else.
    clear is_variant. clear p_vartxt.
    is_variant-report = sy-repid.
    is_variant-username = sy-uname.
  endif.

endform.                               "VARTIANT_CHECK_EXISTENCE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
form modify_selection_screen.
  loop at screen.

    if screen-name = 'P_CURTP'.
      if s_werks is initial.
        screen-input     = '0'.
        modify screen.
      endif.
    endif.

    if screen-name = 'P_CTEXT' or screen-name = 'P_RTEXT'
       or screen-name =  'P_RLDNR'.
      screen-input = '0'.
      modify screen.
    endif.

    if p_load ne 'X' and p_save ne 'X'.
      if screen-group1(2) = 'EX'.
        screen-invisible = '1'.
        screen-input     = '0'.
        modify screen.
        continue.
      endif.
    elseif p_load = 'X'.
      if screen-group1 = 'EXT' .
        screen-invisible = '0'.
        screen-active    = '1'.
        screen-input     = '0'.
        modify screen.
        continue.
      elseif screen-group1 ne 'VAR' and screen-group1(2) ne 'EX'
                                    and screen-group3 ne 'BLK'.
        screen-invisible = '1'.
        screen-input     = '0'.
        modify screen.
        continue.
      endif.
    elseif p_save = 'X'.
      if screen-group1(2) = 'EX' .
        screen-invisible = '0'.
        screen-active    = '1'.
        screen-input     = '0'.
        modify screen.
        continue.
      endif.
    endif.
  endloop.
endform.                               " MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  MODIFY_OBJECT_SELECTION
*&---------------------------------------------------------------------*
*      -->P_EX_SEL  text
*----------------------------------------------------------------------*
form modify_object_selection tables &r_vbeln structure r_vbeln
                                    &r_posnr structure r_posnr
                                    &r_pspnr structure r_pspnr
                                    &r_mtart structure r_mtart
                                    &r_bklas structure r_bklas
                                    &r_matkl structure r_matkl
                                    &r_spart structure r_spart
*                                    &r_error STRUCTURE r_error
                             using  display type c.

  if display eq space.
* clear the select options, that are not displayed
    refresh: &r_vbeln,
             &r_posnr,
             &r_pspnr,
             &r_mtart,
             &r_bklas,
             &r_matkl,
             &r_spart.
*            &r_error.
    clear: &r_vbeln,
           &r_posnr,
           &r_pspnr,
           &r_mtart,
           &r_bklas,
           &r_matkl,
           &r_spart.
*          &r_error.
    set parameter id 'MTA' field &r_mtart.
* the next one dosn't exist anymore!?
*    SET PARAMETER ID 'MTL' FIELD &r_matkl.
    set parameter id 'SPA' field &r_spart.
    loop at screen.
      if screen-group1 eq 'PUK'.
        screen-invisible = '1'.
        screen-input = '0'.
        modify screen.
      endif.
    endloop.
  endif.
endform.                               " MODIFY_OBJECT_SELECTION

*---------------------------------------------------------------------*
*       FORM CHECK_BWKEY                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_BWKEY                                                       *
*---------------------------------------------------------------------*
form check_bwkey.

  select single * from t001k where bwkey in s_werks.
  if sy-subrc ne 0.
    message e316(c+) with p_bwkey.     "bwkey not found
  endif.
  if t001k-mlbwa is initial.
    message e012(c+) with p_bwkey.     "ML is not active
  endif.

endform.                               "CHECK_BWKEY

*---------------------------------------------------------------------*
*       FORM CHECK_BWKEY                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_BWKEY                                                       *
*---------------------------------------------------------------------*
form check_bwkey_bukrs using p_bwkey.

  select single * from t001k where bwkey = p_bwkey.
  if sy-subrc ne 0.
    message e316(c+) with p_bwkey.     "bwkey not found
  endif.
  if t001k-mlbwa is initial.
    message e012(c+) with p_bwkey.     "ML is not active
  endif.

endform.                               "CHECK_BWKEY


*----------------------------------------------------------------------*
*      FORM  set_period
*----------------------------------------------------------------------*
*      check entered period
*----------------------------------------------------------------------*
form set_period
  changing e_poper type poper
           e_bdatj type bdatj.

  data: l_cu_poper type poper,
        l_cu_bdatj type bdatj.

  if e_poper is initial or e_bdatj is initial.
    perform get_current_period changing l_cu_poper l_cu_bdatj.
    e_bdatj = l_cu_bdatj.
    if e_poper is initial.
      e_poper = l_cu_poper.
    endif.
  endif.
endform.                    "set_period

*----------------------------------------------------------------------*
*      FORM  get_current_period
*----------------------------------------------------------------------*
*      determine current period of bukrs
*----------------------------------------------------------------------*
form  get_current_period
  changing e_poper type poper
           e_bdatj type bdatj.

  if marv-bukrs <> t001k-bukrs.
    select single * from marv where bukrs = t001k-bukrs.
    if sy-subrc <> 0.
      exit.
    endif.
  endif.
  move: marv-lfmon to e_poper,
        marv-lfgja to e_bdatj.
endform.                    "get_current_period

*&---------------------------------------------------------------------*
*&      Form  build_kalnr_package
*&---------------------------------------------------------------------*
*       Performance improvement  (note 1902569)
*----------------------------------------------------------------------*
*      -->IT_KALNR_ALL     text
*      -->ET_KALNR_PACK    text
*      -->IV_PACKAGE_SIZE  text
*      -->IV_LAST_INDEX    text
*      -->CV_PACK_INDEX    text
*----------------------------------------------------------------------*
form build_kalnr_package
          tables   it_kalnr_all    type gt_kalnr
                   et_kalnr_pack   type gt_kalnr
          using    iv_package_size type i
                   iv_last_index   type i
          changing cv_pack_index   type i.

  data: lv_pck_size   type  i,
        lv_pck_start  type  i,
        ls_kalnr_pck2 type  g_ty_kalnr.

  clear et_kalnr_pack.
  lv_pck_size   = 0.
  lv_pck_start  = cv_pack_index.

  check lv_pck_start le iv_last_index.

* build packages
  loop at   it_kalnr_all
       into ls_kalnr_pck2
       from lv_pck_start.                               "#EC CI_NOORDER

    if ( lv_pck_size lt iv_package_size ).

      lv_pck_size  = lv_pck_size + 1.
      cv_pack_index = sy-tabix + 1.

      append ls_kalnr_pck2 to et_kalnr_pack.
    else.
*      Package is finished.
      exit.                                             "#EC CI_NOORDER
    endif.

  endloop.

endform.                    "build_kalnr_package
*&---------------------------------------------------------------------*
*& Form READ_ENDING_INV_DIFF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
form read_ending_inv_diff
  tables it_kalnr          type ckmv0_matobj_tbl
         ct_period_ml_data type cl_fml_join_ckmlpp_cr_buffer=>tt_gt_ml_period_data
   using i_bdatj           type bdatj
         i_poper           type poper.

  data:
    begin of lt_eidiff occurs 0,
      kalnr     type ck_kalnr,
      curtp     type fins_curtype,
      jahrper   type ml4h_jahrper,
      runref    type ml4h_runref,
      prd       type ml4h_prd,
      kdm       type ml4h_kdm,
      xkdm_nact type ml4h_kdm_nact,
    end of lt_eidiff,
    lt_kalnr_range type range of ck_kalnr,
    ls_kalnr_range like line of lt_kalnr_range,
    ld_jahrper     type ml4h_jahrper.

  field-symbols:
    <ls_kalnr>   type ckmv0_matobj_str,
    <ls_eidiff>  like line of lt_eidiff,
    <ls_ml_data> type cl_fml_join_ckmlpp_cr_buffer=>ts_gs_ml_period_data.

  ld_jahrper+0(4) = i_bdatj.
  ld_jahrper+4(3) = i_poper.

  ls_kalnr_range-sign   = 'I'.
  ls_kalnr_range-option = 'EQ'.

  loop at it_kalnr assigning <ls_kalnr>.
    ls_kalnr_range-low = <ls_kalnr>-kalnr.
    append ls_kalnr_range to lt_kalnr_range.
  endloop.

  select kalnr      as kalnr,
         curtp      as curtp,
         jahrper    as jahrper,
         runref     as runref,
         sum( prd ) as prd,
         sum( kdm ) as kdm,
         xkdm_nact  as xkdm_nact
    from mldoc into corresponding fields of table @lt_eidiff
    where kalnr   in @lt_kalnr_range
      and jahrper = @ld_jahrper
      and runref  = 'ACT'
      and categ   = 'EB'
      and posart  = 'ST'
    group by kalnr, curtp, jahrper, runref, xkdm_nact.

  sort lt_eidiff by kalnr curtp.

  loop at ct_period_ml_data assigning <ls_ml_data>.
    read table lt_eidiff transporting no fields
      with key kalnr = <ls_ml_data>-kalnr
               curtp = <ls_ml_data>-curtp
      binary search.
    if sy-subrc = 0.
      clear: <ls_ml_data>-ebprd_ea,
             <ls_ml_data>-ebprd_ma,
             <ls_ml_data>-ebkdm_ea,
             <ls_ml_data>-ebkdm_ma.
      loop at lt_eidiff from sy-tabix assigning <ls_eidiff>.
        if <ls_eidiff>-kalnr ne <ls_ml_data>-kalnr or
           <ls_eidiff>-curtp ne <ls_ml_data>-curtp.
          exit.
        endif.
        subtract <ls_eidiff>-prd from <ls_ml_data>-ebprd_ea.  "In database they have inverted sign
        if <ls_eidiff>-xkdm_nact is initial.                  "as we want to show them
          subtract <ls_eidiff>-kdm from <ls_ml_data>-ebkdm_ea.
        endif.
      endloop.
    endif.
  endloop.

endform.

include rckm_va_bukrs_werks.

include rkkbmlmat_class.

*&---------------------------------------------------------------------*
*& Form get_hkont
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- E_T_OUT
*&---------------------------------------------------------------------*
form get_hkont changing e_t_out type t_out.

  if e_t_out is not initial.

    "--Incluir conta razão
    select matnr, bwkey,  bklas
      from mbew
      into table @data(lt_classe)
      for all entries in @e_t_out
     where matnr = @e_t_out-matnr
       and bwkey = @e_t_out-bwkey.

    if sy-subrc = 0.

      data(_plano_ctas)   = '0050'. "//Plano contas: Quadro geral
      data(_key_operacao) = 'BSX'.  "//Chave de operação

      select bklas, konts
        from t030
        into table @data(lt_t030)
       for all entries in @lt_classe
       where ktopl = @_plano_ctas
         and ktosl = @_key_operacao
         and bklas = @lt_classe-bklas.

      if sy-subrc = 0.

        sort: lt_classe by matnr bwkey bklas,
              lt_t030 by bklas konts.

        loop at e_t_out assigning field-symbol(<fs_out>).

          read table lt_classe assigning field-symbol(<fs_class>)
                  with key matnr = <fs_out>-matnr
                           bwkey = <fs_out>-bwkey binary search.

          if sy-subrc = 0.

            read table lt_t030 assigning field-symbol(<fs_t030>)
                with key bklas = <fs_class>-bklas binary search.

            if sy-subrc = 0.
              <fs_out>-konts = <fs_t030>-konts.
            endif.

          endif.

        endloop.

      endif.

    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*& Form get_gsber
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- E_T_OUT
*&---------------------------------------------------------------------*
form get_gsber  changing e_t_out type t_out.

  select werks, gsber
    from t134g
    into table @data(lt_div)
    for all entries in @e_t_out
    where werks = @e_t_out-werks.

  if sy-subrc = 0.

    sort lt_div by werks.

    loop at e_t_out assigning field-symbol(<fs_out>) .

      read table lt_div assigning field-symbol(<fs_div>)
          with key werks = <fs_out>-werks binary search.
      if sy-subrc = 0.
        <fs_out>-gsber = <fs_div>-gsber.
      endif.

    endloop.

  endif.

endform.
