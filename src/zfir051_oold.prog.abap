*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo R Tavares                                       &*
*& Data.....: 15/07/2014                                              &*
*& Descrição: Atualização de Partidas Moeda Funcional / Estrangeira   &*
*& Transação: ZFI0063                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         14.07.2014                            &*
*&--------------------------------------------------------------------&*

report  zfir051.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
type-pools: slis, kkblo.
tables: zfit0084.
include <icon>.

types: begin of ty_setleaf.
         include type setleaf.
types:   ktoks type ska1-ktoks,
       end of ty_setleaf,

       begin of ty_bkpf,
         bukrs type bkpf-bukrs,
         belnr type bkpf-belnr,
         gjahr type bkpf-gjahr,
         budat type bkpf-budat,
         stblg type bkpf-stblg,
         stjah type bkpf-stjah,
       end of ty_bkpf,

       begin of ty_bsik_bsid,
         bukrs type bsik-bukrs,
         belnr type bsik-belnr,
         buzei type bsik-buzei,
         gjahr type bsik-gjahr,
         budat type bsik-budat,
         hkont type bsik-hkont,
         waers type bsik-waers,
         monat type bsik-monat,
         dmbtr type bsik-dmbtr,
         dmbe2 type bsik-dmbe2,
         dmbe3 type bsik-dmbe3,
         lifnr type bsik-lifnr,
         xblnr type bsik-xblnr,
         blart type bsik-blart,
         doc   type bsik-ebeln,
         item  type bsik-ebelp,
         umsks type bsik-umsks,
         umskz type bsik-umskz,
         shkzg type bsik-shkzg,
         bschl type bsik-bschl,
         augbl type bsik-augbl,
       end of ty_bsik_bsid,

       begin of ty_bsik_bsid_aux,
         bukrs type bsik-bukrs,
         belnr type bsik-belnr,
         buzei type bsik-buzei,
         gjahr type bsik-gjahr,
         budat type bsik-budat,
         hkont type bsik-hkont,
         waers type bsik-waers,
         monat type bsik-monat,
         dmbtr type bsik-bdif2,
         dmbe2 type bsik-bdif2,
         dmbe3 type bsik-bdif2,
         lifnr type bsik-lifnr,
         xblnr type bsik-xblnr,
         blart type bsik-blart,
         doc   type bsik-ebeln,
         item  type bsik-ebelp,
         umsks type bsik-umsks,
         umskz type bsik-umskz,
         shkzg type bsik-shkzg,
         bschl type bsik-bschl,
       end of ty_bsik_bsid_aux,

       begin of ty_saida,
         mark,
         kunnr           type bsid-kunnr,
         name1           type kna1-name1,
         hkont           type bsid-hkont,
         txt50           type skat-txt50,
         belnr2          type bsik-belnr,
         buzei           type bsik-buzei,
         gjahr           type bsik-gjahr,
         waers           type bsik-waers,
         budat           type bsik-budat,
         blart           type bsik-blart,
         xblnr           type bsik-xblnr,
         doc             type bsik-ebeln,
         item            type bsik-ebelp,
         dmbtr           type bsik-bdif2,
         dmbe2           type bsik-dmbe2,
         dmbe3           type bsik-dmbe3,
         tx_usd          type zfit0084-tx_usd,
         tx_brl          type zfit0084-tx_brl,
         saldo_corr      type bsik-dmbtr,
         saldo_corr2     type bsik-dmbe2,
         vlr_ajust       type bsik-dmbtr,
         vlr_ajust2      type bsik-dmbe2,
         bschl           type bsik-bschl,
         umskz           type bsik-umskz,
         belnr           type zib_contabil_chv-belnr,
         belnr_est       type zib_contabil_chv-belnr,
         obj_key         type zib_contabil_chv-obj_key,
         obj_key_est     type zib_contabil_chv-obj_key,
         belnr_inv       type zib_contabil_chv-belnr,
         belnr_inv_est   type zib_contabil_chv-belnr,
         obj_key_inv     type zib_contabil_chv-obj_key,
         obj_key_inv_est type zib_contabil_chv-obj_key,
         vbund           type bseg-vbund,
         bewar           type bseg-bewar,
         stblg           type bkpf-stblg, " USER STORY 155163 // MMSILVA - 15.10.2024
         log(4),
         log_inv(4),
         line_color(4)   type c,
       end of ty_saida,

       begin of ty_saidar,
         mark,
         racct         type faglflext-racct,
         rassc         type faglflext-rassc,
         txt50         type skat-txt50,
         ktoks         type ska1-ktoks,
         curr1         type faglflext-hslvt,
         curr2         type faglflext-kslvt,
         curr3         type faglflext-oslvt,
         tx_usd        type zfit0082-tx_usd,
         tx_brl        type zfit0082-tx_brl,
         saldo_corr    type faglflext-hslvt,
         saldo_corr2   type faglflext-hslvt,
         vlr_ajust     type faglflext-kslvt,
         vlr_ajust2    type faglflext-kslvt,
         belnr         type zib_contabil_chv-belnr,
         belnr_est     type zib_contabil_chv-belnr,
         obj_key       type zib_contabil_chv-obj_key,
         obj_key_est   type zib_contabil_chv-obj_key,
         stblg         type bkpf-stblg, " USER STORY 155163 // MMSILVA - 15.10.2024
         log(4),
         line_color(4) type c,
       end of ty_saidar,

       begin of ty_saida_exec,
         icon(4),
         tipo    type c length 30,
         lifnr   type lfa1-lifnr,
         name1   type lfa1-name1,
         hkont   type bsik-hkont,
         racct   type faglflext-racct,
         txt50   type skat-txt50,
         msg(80),
       end of ty_saida_exec,

       begin of ty_saknr,
         saknr type ska1-saknr,
       end of ty_saknr.

types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
data: tg_setleaf       type table of ty_setleaf,
      tg_setleaf_02    type table of ty_setleaf,
      tg_t882g         type table of t882g,
      tg_t001          type table of t001,
      tg_ska1          type table of ska1,
      tg_skat          type table of skat,
      tg_tcurr         type table of tcurr,
      tg_zfit0185      type table of zfit0185,
      tg_tcurr_lday    type table of tcurr,
      tg_zfit0185_lday type table of zfit0185,
      tg_0084          type table of zfit0084,
      tg_0084_aux      type table of zfit0084,
      tg_0081          type table of zfit0081,
      tg_lfa1          type table of lfa1,
      tg_kna1          type table of kna1,
      tg_bsik_bsid     type table of ty_bsik_bsid,
      tg_bsik_bsid_aux type table of ty_bsik_bsid,
      tg_bkpf_lanc     type table of bkpf,               " Lançamento
      tg_bkpf_est      type table of bkpf,               " Estorno
      tg_zib           type table of zib_contabil,
      tg_zib_chv       type table of zib_contabil_chv,
      tg_zib_err       type table of zib_contabil_err,
      t_hkont          type standard table of  rgsb4 with header line,
      tg_bseg          type table of bseg,
      tg_saida         type table of ty_saida,
      tg_saida_aux     type table of ty_saida,
      tg_saida_res     type table of ty_saida,
      tg_saida_exec    type table of ty_saida_exec,
      it_saknr         type table of ty_saknr,
      wa_saknr         type ty_saknr.

data:
  tg_skb1       type table of skb1,
  tg_0082       type table of zfit0082,
  tg_0082_aux   type table of zfit0082,
  tg_faglflext  type table of faglflext,
  tg_saidar     type table of ty_saidar,

  wg_saidar     type ty_saidar,
  wg_0082       type zfit0082,
  wg_faglflext  type faglflext,
  wl_input_0082 type zfit0082,
  gs_variant_c  type disvariant.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
data: wg_setleaf       type ty_setleaf,
      wg_setleaf_02    type ty_setleaf,
      wg_t882g         type t882g,
      wg_t001          type t001,
      wg_ska1          type ska1,
      wg_skat          type skat,
      wg_0084          type zfit0084,
      wg_tcurr         type tcurr,
      wg_zfit0185      type zfit0185,
      wg_tcurr_lday    type tcurr,
      wg_zfit0185_lday type zfit0185,
      wg_lfa1          type lfa1,
      wg_kna1          type kna1,
      wg_0081          type zfit0081,
      wg_zib_chv       type zib_contabil_chv,
      wg_zib_err       type zib_contabil_err,
      wg_bsik_bsid     type ty_bsik_bsid,
      wg_bkpf          type bkpf,
      wg_bseg          type bseg,
      wg_bkpf_fb08     type ty_bkpf,
      wg_bkpf_fb08_e   type ty_bkpf,
      wg_bkpfe         type bkpf,
      wg_bsik_bsid_aux type ty_bsik_bsid_aux,
      wg_saida         type ty_saida,
      wg_saida_exec    type ty_saida_exec.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
data: xs_events    type slis_alv_event,
      events       type slis_t_event,
      t_print      type slis_print_alv,
      estrutura    type table of ty_estrutura,
      wa_estrutura type ty_estrutura,
      v_report     like sy-repid,
      v_carga(1),
      t_top        type slis_t_listheader,
      lt_sort      type slis_t_sortinfo_alv,
      ls_sort      type slis_sortinfo_alv,
      tabix        type sy-tabix,
      tabix2       type sy-tabix.
*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
data: vg_ryear            type faglflext-ryear,
      vg_last_day         type sy-datum,
      vg_first_day        type sy-datum,
      w_answer(1),
      vg_last_day_aux(8),
      vg_last_day_aux2(8),
      v_vbund             type bseg-vbund,
      v_bewar             type bseg-bewar,
      v_saknr             type zfit0081-saknr,
      e_status(1),
      e_messa(64).


data: begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data: end of it_msg.

data: ti_bdcdata       type standard table of bdcdata ,   "Guarda o mapeamento
      wa_bdcdata       like line of ti_bdcdata,
      t_messtab        type table of bdcmsgcoll,
      vobj_key         type zib_contabil_err-obj_key,
      wl_message       type pmst_raw_message,
      wg_documento(10),
      wl_mode(1),
      it_selection     type table of rsparams,
      wa_selection     like line of it_selection.
*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
constants: c_x(1) type c value 'X'.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
define acao.

  call function 'SAPGUI_SET_FUNCTIONCODE'
    exporting
      functioncode           = &1
    exceptions
      function_not_supported = 1.

end-of-definition.
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_receiver definition.
  public section.
    class-methods
      on_button_click for event button_click of cl_gui_alv_grid
        importing es_col_id es_row_no.
  private section.
endclass.                    "LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
* TELA DE SELECAO.
*----------------------------------------------------------------------*
selection-screen: begin of block b1 with frame title text-h01.


  select-options: s_bukrs for zfit0084-bukrs obligatory no intervals no-extension,
                  s_mes   for zfit0084-mes_ano no intervals no-extension obligatory.
  parameters:
    r_av_f radiobutton group rad1 user-command act default 'X' modif id b,
    r_av_e radiobutton group rad1 modif id b.
  selection-screen: begin of block b2 with frame title text-h02.



    parameters: p_clie radiobutton group g1 user-command act.
    select-options s_saknrc for zfit0084-saknr.

    selection-screen begin of line.
      selection-screen comment 1(1) text-su3.
    selection-screen end of line.

    parameters: p_forn radiobutton group g1.
    select-options s_saknr for zfit0084-saknr.

    selection-screen begin of line.
      selection-screen comment 1(1) text-su3.
    selection-screen end of line.

    parameters: p_raz radiobutton group g1.
    select-options s_saknrs for zfit0084-saknr.

    selection-screen begin of line.
      selection-screen comment 1(1) text-su3.
    selection-screen end of line.

    parameters: p_hkont2 radiobutton group g1 modif id a.
    select-options: s_hkont2 for zfit0084-saknr modif id a.

  selection-screen: end of block b2.
  selection-screen: begin of block b3 with frame title text-h02.


    parameters: p_proc radiobutton group g2,
                p_reve radiobutton group g2,
                p_visu radiobutton group g2.

  selection-screen: end of block b3.
selection-screen: end of block b1.

initialization.
  select single *
     from usr05
     into @data(_usr05)
     where bname = @sy-uname
     and parid   = 'BUK'.
  if sy-subrc = 0.
    s_bukrs-sign    = 'I'.
    s_bukrs-option  = 'EQ'.
    s_bukrs-low = _usr05-parva+0(4).
    append s_bukrs.
  endif.

at selection-screen output.
  loop at screen.
    if sy-tcode ne 'ZFI0108'.
      if screen-group1 = 'B'.
        screen-active = 0.
      endif.
    endif.
    if r_av_e = 'X'.
      if screen-group1 = 'A'.
        screen-active = 1.
      endif.
    else.
      if screen-group1 = 'A'.
        screen-active = 0.
      endif.
    endif.
    modify screen.
  endloop.
  if p_forn = 'X'.
    refresh:  s_saknrc, s_saknrs, s_hkont2.
  elseif p_clie = 'X'.
    refresh: s_saknr, s_saknrs, s_hkont2.
  elseif p_raz = 'X'.
    refresh: s_saknr, s_saknrc, s_hkont2.
  else.
    refresh: s_saknr, s_saknrc,s_saknrs.
  endif.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.
  method on_button_click.
    data: tl_texto    type catsxt_longtext_itab,
          wl_texto    type line of catsxt_longtext_itab,
          it_zib_err  type table of zib_contabil_err,
          wl_text     type sytitle,
          wl_text_est type sytitle.

    refresh: tl_texto.
    clear: wl_texto.
    if es_col_id eq 'LOG'.
      if tg_saidar[] is not initial.
        read table tg_saidar into wg_saidar index es_row_no-row_id.
        wg_saida-obj_key = wg_saidar-obj_key.
      else.
        read table tg_saida into wg_saida index es_row_no-row_id.
      endif.
**       Contabilização
      if wg_saida-obj_key is not initial.
        select *
          from zib_contabil_err
          into table it_zib_err
          where obj_key eq wg_saida-obj_key.

        loop at it_zib_err into wg_zib_err  where obj_key eq wg_saida-obj_key.

          wl_texto = wg_zib_err-message.

          append wl_texto to tl_texto.
          clear: wl_texto.
        endloop.
        if tl_texto[] is not initial.
          wl_text = text-001.
          call function 'CATSXT_SIMPLE_TEXT_EDITOR'
            exporting
              im_title        = wl_text           "" Título
              im_display_mode = c_x
            changing
              ch_text         = tl_texto.
        endif.
      endif.
    endif.


  endmethod.                    "ON_BUTTON_CLICK
endclass.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

initialization.       "to set titlebar on selection screen
  if sy-tcode = 'ZFI0108'.
    sy-title = text-064.
  else.
    sy-title = text-065.
  endif.

start-of-selection.


  if r_av_e = 'X'.
    vg_ryear  = s_mes-low+2(4).
    concatenate vg_ryear s_mes-low(2) '01' into vg_last_day_aux.
    vg_last_day = vg_last_day_aux.
    call function 'BKK_GET_MONTH_LASTDAY'
      exporting
        i_date = vg_last_day
      importing
        e_date = vg_last_day.
    "
    refresh it_selection.
    "P_RAZ
    loop at s_saknrs.
      wa_selection-selname = 'S_HKONT'.
      wa_selection-kind    = 'S'.
      wa_selection-sign    = s_saknrs-sign.
      wa_selection-option  = s_saknrs-option.
      wa_selection-low     = s_saknrs-low.
      wa_selection-high    = s_saknrs-high.
      append wa_selection to it_selection.
    endloop.
    "
    "P_FORN
    loop at s_saknr.
      wa_selection-selname = 'S_KONT'.
      wa_selection-kind    = 'S'.
      wa_selection-sign    = s_saknr-sign.
      wa_selection-option  = s_saknr-option.
      wa_selection-low     = s_saknr-low.
      wa_selection-high    = s_saknr-high.
      append wa_selection to it_selection.
    endloop.

    "P_CLIE
    loop at s_saknrc.
      wa_selection-selname = 'S_DONT'.
      wa_selection-kind    = 'S'.
      wa_selection-sign    = s_saknrc-sign.
      wa_selection-option  = s_saknrc-option.
      wa_selection-low     = s_saknrc-low.
      wa_selection-high    = s_saknrc-high.
      append wa_selection to it_selection.
    endloop.

    "P_HKONT2
    loop at s_hkont2.
      wa_selection-selname = 'S_DONT'.
      wa_selection-kind    = 'S'.
      wa_selection-sign    = s_hkont2-sign.
      wa_selection-option  = s_hkont2-option.
      wa_selection-low     = s_hkont2-low.
      wa_selection-high    = s_hkont2-high.
      append wa_selection to it_selection.
    endloop.

    submit zgl027 with selection-table it_selection
                  with p_bukrs  = s_bukrs-low
                  with p_budat  = vg_last_day
                  with p_c_lanc = p_proc
                  with p_e_lanc = p_reve
                  with p_v_lanc = p_visu
                  with p_hkont  = p_raz
                  with p_lifnr  = p_forn
                  with p_kunnr  = p_clie
                  with p_hkont2 = p_hkont2
    and return.

  else.
    if p_raz = 'X'.
      perform selecionar_dados_r.
    else.
      perform selecionar_dados.
    endif.


    perform iniciar_variaveis.
    if p_raz is initial.
      perform organizacao_dados.
    else.
      perform organizacao_dados_r.
    endif.
    perform imprimir_dados.
  endif.

*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form iniciar_variaveis.

  clear: wg_t001.
  read table tg_t001 into wg_t001 index 1.

  if p_forn is not initial.
    perform f_construir_cabecalho using 'H' text-003.
  elseif p_clie is not initial..
    perform f_construir_cabecalho using 'H' text-004.
  else.
    perform f_construir_cabecalho using 'H' text-056.
  endif.

endform.                    " INICIAR_VARIAVES

form xuser_commandr using ucomm like sy-ucomm
                         selfield type kkblo_selfield.
  data: tl_zib      type table of zib_contabil with header line,
        tl_zib_chv  type table of zib_contabil_chv with header line,
        tl_zib_err  type table of zib_contabil_err with header line,
        tl_saida    type table of ty_saidar,
        wl_obj_key  type zib_contabil_chv-obj_key,
        wa_zfit0082 type zfit0082.

  refresh: tg_saida_exec, tl_zib, tl_zib_chv, tl_zib_err, tl_saida.
  clear:wl_obj_key.

  tl_saida[]  = tg_saidar[].
  delete tl_saida where obj_key is initial.
  if tl_saida[] is not initial.
    select *
      from zib_contabil
      into table tl_zib
       for all entries in tl_saida
       where obj_key eq tl_saida-obj_key.
  endif.


  if tl_zib[] is not initial.
    select *
     from zib_contabil_chv
     into table tl_zib_chv
      for all entries in tl_zib
      where obj_key eq tl_zib-obj_key.

    select *
     from zib_contabil_err
     into table tl_zib_err
      for all entries in tl_zib
      where obj_key eq tl_zib-obj_key.

  endif.

  sort: tl_zib     by obj_key,
        tl_zib_chv by obj_key,
        tl_zib_err by obj_key.

  case ucomm.
    when 'GERA'.

      loop at tg_saidar into wg_saidar
        where mark is not initial.
        if wg_saidar-obj_key is not initial and wg_saidar-belnr is initial.
          read table tl_zib with key obj_key = wg_saidar-obj_key binary search.
          if tl_zib-rg_atualizado eq 'N'.
            clear: wg_saida_exec.
            wg_saida_exec-icon   = icon_yellow_light.
            wg_saida_exec-racct  = wg_saidar-racct.
            wg_saida_exec-txt50  = wg_saidar-txt50.
            wg_saida_exec-msg    = text-m01 .
            append wg_saida_exec to tg_saida_exec.
            continue.
          elseif tl_zib-rg_atualizado eq 'S'.
            read table tl_zib_chv with key obj_key = wg_saidar-obj_key binary search.
            if sy-subrc is initial.
              clear: wg_saida_exec.
              wg_saida_exec-icon   = icon_green_light.
              wg_saida_exec-racct  = wg_saidar-racct.
              wg_saida_exec-txt50  = wg_saidar-txt50.
              wg_saida_exec-msg    = text-m02 .
              append wg_saida_exec to tg_saida_exec.
              acao '&ATUAL'.
              continue.
            else.
              read table tl_zib_err  with key obj_key = wg_saidar-obj_key binary search.
              if sy-subrc is initial.
                clear: wg_saida_exec.
                wg_saida_exec-icon   = icon_red_light.
                wg_saida_exec-racct  = wg_saidar-racct.
                wg_saida_exec-txt50  = wg_saidar-txt50.
                wg_saida_exec-msg    = text-m03 .
                append wg_saida_exec to tg_saida_exec.

                call function 'POPUP_TO_CONFIRM'
                  exporting
                    text_question         = text-m04
*                   TEXT_BUTTON_1         = 'Sim'(100)
                    text_button_1         = text-b01
                    icon_button_1         = 'ICON_OKAY'
*                   TEXT_BUTTON_2         = 'Não'(101)
                    text_button_2         = text-b02
                    icon_button_2         = 'ICON_CANCEL'
                    default_button        = '1'
                    display_cancel_button = ' '
                    start_column          = 25
                    start_row             = 6
                  importing
                    answer                = w_answer
                  exceptions
                    text_not_found        = 1
                    others                = 2.

                if sy-subrc <> 0.
                  message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                endif.
                if w_answer = '1'.
                  delete from zib_contabil_err where obj_key = wg_saidar-obj_key.
                  delete from zib_contabil     where obj_key = wg_saidar-obj_key.
                  delete from zfit0082         where obj_key = wg_saidar-obj_key.
                else.
                  acao '&ATUAL'.
                  continue.
                endif.
              else.
                clear: wg_saida_exec.
                wg_saida_exec-icon   = icon_yellow_light.
                wg_saida_exec-racct  = wg_saidar-racct.
                wg_saida_exec-txt50  = wg_saidar-txt50.
                wg_saida_exec-msg    = text-m01 .
                append wg_saida_exec to tg_saida_exec.
                continue.
              endif.
            endif.
          endif.
        endif.

        if wg_saidar-obj_key is initial.
          select single *
            from zfit0082
            into wa_zfit0082
            where bukrs     = s_bukrs-low
            and   mes_ano   = s_mes-low
            and   saknr     = wg_saidar-racct
            and   vbund     = wg_saidar-rassc
            and   obj_key   ne ''.
          if sy-subrc = 0.
            continue.
          endif.

        endif.

        if wg_saidar-vlr_ajust = 0.
          clear: wg_saida_exec.
          wg_saida_exec-icon   = icon_red_light.
          wg_saida_exec-racct  = wg_saidar-racct.
          wg_saida_exec-txt50  = wg_saidar-txt50.
          wg_saida_exec-msg    = text-m11 .
          append wg_saida_exec to tg_saida_exec.
          continue.
        endif.

        if wg_saidar-belnr is not initial.
          clear: wg_saida_exec.
          wg_saida_exec-icon   = icon_red_light.
          wg_saida_exec-racct  = wg_saidar-racct.
          wg_saida_exec-txt50  = wg_saidar-txt50.
          wg_saida_exec-msg    = text-m05 .
          append wg_saida_exec to tg_saida_exec.
          continue.
        endif.
        clear: wl_obj_key.
        perform gera_contabilr using wg_saidar space changing wl_obj_key.

        wg_saidar-obj_key = wl_obj_key.
        clear: wg_saidar-obj_key_est, wg_saidar-belnr_est.
        modify tg_saidar from wg_saidar transporting obj_key obj_key_est belnr_est.
      endloop.

      selfield-col_stable = c_x.
      selfield-row_stable = c_x.
      selfield-refresh = c_x.

      perform imprimir_exec.
      acao '&ATUAL'.
    when 'ESTORNO'.
      perform estorna_documentos using 'EST'.
      selfield-col_stable = c_x.
      selfield-row_stable = c_x.
      selfield-refresh    = c_x.
      perform imprimir_exec.
    when 'REVERTE'.
      perform estorna_documentos using 'REV'.
      selfield-col_stable = c_x.
      selfield-row_stable = c_x.
      selfield-refresh    = c_x.
      perform imprimir_exec.
    when '&IC1'.
* Lê na tabela de saída
      read table tg_saidar into wg_saidar index selfield-tabindex.

      if sy-subrc eq 0.

        if selfield-fieldname = 'BELNR'.
          if wg_saidar-belnr is not initial.
            set parameter id 'BLN' field wg_saidar-belnr.
            set parameter id 'GJR' field wg_saidar-obj_key+16(4).
            set parameter id 'BUK' field s_bukrs-low.

            call transaction 'FB03' and skip first screen.
          endif.
        endif.

        if selfield-fieldname = 'STBLG'.
          if wg_saidar-stblg is not initial.
            set parameter id 'BLN' field wg_saidar-stblg.
            set parameter id 'GJR' field wg_saidar-obj_key+16(4).
            set parameter id 'BUK' field s_bukrs-low.

            call transaction 'FB03' and skip first screen.
          endif.
        endif.
      endif.
    when '&ATUAL'.
      perform atualiza_saidar tables tg_saidar
                                    tl_zib
                                    tl_zib_chv
                                    tl_zib_err.
  endcase.

  selfield-col_stable = c_x.
  selfield-row_stable = c_x.
  selfield-refresh = c_x.
endform. "XUSER_COMMAND

*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form xuser_command using ucomm like sy-ucomm
                         selfield type kkblo_selfield.

  types: begin of ty_documento,
           bukrs type bukrs,
           belnr type belnr_d,
           gjahr type gjahr.
  types: end of ty_documento.

  data: tl_zib     type table of zib_contabil with header line,
        tl_zib_chv type table of zib_contabil_chv with header line,
        tl_zib_err type table of zib_contabil_err with header line,
        tl_saida   type table of ty_saida,
        vg_belnr   type ty_documento.

  refresh: tg_saida_exec, tl_zib, tl_zib_chv, tl_zib_err, tl_saida.

  "Ajusta Documento de Variação """"""""""""""""""""""""""""""""""""""""""""""""""
  "*******************************************************************************
  tl_saida[]  = tg_saida[].
  delete tl_saida where obj_key is initial.
  if tl_saida[] is not initial.
    select *
      from zib_contabil
      into table tl_zib
       for all entries in tl_saida
     where obj_key eq tl_saida-obj_key.
  endif.

  if tl_zib[] is not initial.
    select *
     from zib_contabil_chv
     into table tl_zib_chv
      for all entries in tl_zib
      where obj_key eq tl_zib-obj_key.

    select *
     from zib_contabil_err
     into table tl_zib_err
      for all entries in tl_zib
      where obj_key eq tl_zib-obj_key.
  endif.

  sort: tl_zib     by obj_key,
        tl_zib_chv by obj_key,
        tl_zib_err by obj_key.

  case ucomm.
    when 'GERA'.
      if p_proc is not initial.
        perform gera_documentos tables tl_zib tl_zib_chv tl_zib_err.
        selfield-col_stable = c_x.
        selfield-row_stable = c_x.
        selfield-refresh    = c_x.
        perform imprimir_exec.
      endif.

    when 'ESTORNO'.
      perform estorna_documentos using 'EST'.
      selfield-col_stable = c_x.
      selfield-row_stable = c_x.
      selfield-refresh    = c_x.
      perform imprimir_exec.

    when 'REVERTE'.
      perform estorna_documentos using 'REV'.
      selfield-col_stable = c_x.
      selfield-row_stable = c_x.
      selfield-refresh    = c_x.
      perform imprimir_exec.
    when '&IC1'.
* Lê na tabela de saída
      read table tg_saida into wg_saida index selfield-tabindex.
      if sy-subrc eq 0.
        clear: vg_belnr.
        if selfield-fieldname = 'BELNR'.
          if wg_saida-belnr is not initial and wg_saida-belnr+0(1) ne '@'.
            vg_belnr-bukrs = s_bukrs-low.
            vg_belnr-belnr = wg_saida-belnr.
            vg_belnr-gjahr = wg_saida-obj_key+16(4).
          endif.
        elseif selfield-fieldname = 'BELNR2'.
          if wg_saida-belnr2 is not initial and wg_saida-belnr2+0(1) ne '@'.
            vg_belnr-bukrs = s_bukrs-low.
            vg_belnr-belnr = wg_saida-belnr2.
            vg_belnr-gjahr = wg_saida-gjahr.
          endif.
        elseif selfield-fieldname = 'BELNR_EST'.
          if wg_saida-belnr_est is not initial and wg_saida-belnr_est+0(1) ne '@'.
            vg_belnr-bukrs = s_bukrs-low.
            vg_belnr-belnr = wg_saida-belnr_est.
            vg_belnr-gjahr = wg_saida-obj_key_est+16(4).
          endif.
        elseif selfield-fieldname = 'BELNR_INV'.
          if wg_saida-belnr_inv is not initial and wg_saida-belnr_inv+0(1) ne '@'.
            vg_belnr-bukrs = s_bukrs-low.
            vg_belnr-belnr = wg_saida-belnr_inv.
            vg_belnr-gjahr = wg_saida-obj_key_inv+16(4).
          endif.
        elseif selfield-fieldname = 'BELNR_INV_EST'.
          if wg_saida-belnr_inv_est is not initial and wg_saida-belnr_inv_est+0(1) ne '@'.
            vg_belnr-bukrs = s_bukrs-low.
            vg_belnr-belnr = wg_saida-belnr_inv_est.
            vg_belnr-gjahr = wg_saida-obj_key_inv_est+16(4).
          endif.
        endif.

        if vg_belnr is not initial.
          set parameter id 'BLN' field vg_belnr-belnr.
          set parameter id 'GJR' field vg_belnr-gjahr.
          set parameter id 'BUK' field vg_belnr-bukrs.
          call transaction 'FB03' and skip first screen.
          "USER STORY 155163 // MMSILVA - 24.10.2024 - Inicio
        elseif selfield-fieldname = 'STBLG'.
          if wg_saida-stblg is not initial.
            set parameter id 'BLN' field wg_saida-stblg.
            set parameter id 'GJR' field wg_saida-obj_key+16(4).
            set parameter id 'BUK' field s_bukrs-low.

            call transaction 'FB03' and skip first screen.
          endif.
          "USER STORY 155163 // MMSILVA - 24.10.2024 - Fim
        endif.

      endif.

    when '&ATUAL'.
      perform atualiza_saida tables tg_saida
                                    tl_zib
                                    tl_zib_chv
                                    tl_zib_err.
  endcase.

  selfield-col_stable = c_x.
  selfield-row_stable = c_x.
  selfield-refresh = c_x.
endform. "XUSER_COMMAND
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form xpf_status_set using e_comm.                           "#EC CALLED
  data: gr_events      type ref to lcl_event_receiver,
        ck_atualiza(1).
  data: fcode type table of sy-ucomm.
  refresh: fcode.
  data : ls_sel_hide            type slis_sel_hide_alv.
  data: ref1             type ref to cl_gui_alv_grid,
        tl_fieldcatalog  type lvc_t_fcat,
        tl_fieldcatalog2 type lvc_t_fcat,
        wl_fieldcatalog  type lvc_s_fcat,
        wl_fieldcatalog2 type lvc_s_fcat,
        is_table         type lvc_s_stbl.

  if v_carga is initial.
    v_carga = 'X'.

    call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      importing
        es_sel_hide = ls_sel_hide
        e_grid      = ref1.

    create object gr_events.
    call method ref1->get_frontend_fieldcatalog
      importing
        et_fieldcatalog = tl_fieldcatalog.

    loop at tl_fieldcatalog into wl_fieldcatalog.
      tabix = sy-tabix.
      read table estrutura into wa_estrutura with key fieldname = wl_fieldcatalog-fieldname.
      if sy-subrc = 0.
        wl_fieldcatalog-col_pos = wa_estrutura-col_pos.
        modify tl_fieldcatalog from wl_fieldcatalog index tabix.
      endif.
    endloop.
    sort tl_fieldcatalog by col_pos.

    ck_atualiza  = abap_false.

    read table tl_fieldcatalog into wl_fieldcatalog with key fieldname = 'LOG'.
    if sy-subrc is initial.
      wl_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
      modify tl_fieldcatalog from wl_fieldcatalog index sy-tabix transporting style edit.
      call method ref1->set_frontend_fieldcatalog
        exporting
          it_fieldcatalog = tl_fieldcatalog.
      is_table-row = 'X'.
      is_table-col = 'X'.
      ck_atualiza  = abap_true.
    endif.

    read table tl_fieldcatalog into wl_fieldcatalog with key fieldname = 'LOG_INV'.
    if sy-subrc is initial.
      wl_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
      modify tl_fieldcatalog from wl_fieldcatalog index sy-tabix transporting style edit.
      call method ref1->set_frontend_fieldcatalog
        exporting
          it_fieldcatalog = tl_fieldcatalog.
      is_table-row = 'X'.
      is_table-col = 'X'.
      ck_atualiza  = abap_true.
    endif.

    if ck_atualiza eq abap_true.
      call method ref1->refresh_table_display
        exporting
          is_stable      = is_table
          i_soft_refresh = 'X'.
    endif.

    set handler lcl_event_receiver=>on_button_click for ref1.
  endif.

  if p_visu is not initial.
    append 'GERA' to fcode.
    append 'ESTORNO' to fcode.
    append 'REVERTE' to fcode.
  else.
    if p_proc is initial.
      append 'GERA' to fcode.
    endif.

    if p_reve is initial.
      append 'REVERTE' to fcode.
    else.
      append 'ESTORNO' to fcode.
    endif.
  endif.



  set pf-status 'STATUS_UNI'  excluding fcode.

endform. "XPF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form xtop_of_page.                                          "#EC CALLED

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = t_top
      i_logo             = ''.

endform. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
form f_construir_cabecalho using typ text.
  data: cabec type string.
  if p_proc = 'X'.
    concatenate text-066 text into cabec separated by space.
  elseif p_reve = 'X'.
    concatenate text-067 text into cabec separated by space.
  else.
    concatenate text-068 text into cabec separated by space.
  endif.
  data: ls_line type slis_listheader.
  ls_line-typ = typ.
  ls_line-info = cabec.
  append ls_line to t_top.


  ls_line-typ = 'S'.
  if wg_t001-land1 eq 'BR' or wg_t001-land1 eq 'NL' or  wg_t001-land1 eq 'CH'.
*    LS_LINE-KEY = 'Empresa:'.
    ls_line-key = text-008.
    concatenate  wg_t001-bukrs '-' wg_t001-butxt into ls_line-info separated by space.
  elseif wg_t001-land1 eq 'AR'
      or wg_t001-land1 eq 'PY'.
*    LS_LINE-KEY = 'Sociedad:'.
    ls_line-key = text-009.
    concatenate  wg_t001-bukrs '-' wg_t001-butxt into ls_line-info separated by space.
  endif.
  append ls_line to t_top.

  if wg_t001-land1 eq 'BR' or wg_t001-land1 eq 'NL' or  wg_t001-land1 eq 'CH'.
*    LS_LINE-KEY = 'Mês/Ano:'.
    ls_line-key = text-010.
    concatenate  s_mes-low(2) '/' s_mes-low+2(4)  into ls_line-info separated by space.
  elseif wg_t001-land1 eq 'AR'
      or wg_t001-land1 eq 'PY'.
*    LS_LINE-KEY = 'Ejercicio:'.
    ls_line-key = text-011.
    concatenate  s_mes-low(2) '/' s_mes-low+2(4)  into ls_line-info separated by space.
  endif.
  append ls_line to t_top.

endform.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form imprimir_dados .
  data: wl_layout type slis_layout_alv,
        w_repid   type sy-repid.

  perform definir_eventos.
  if p_raz is initial.
    perform montar_layout using 'TG_SAIDA'.
*
    clear       v_carga.
    if p_proc is not initial or p_reve is not initial.
      wl_layout-box_fieldname = 'MARK'.
    endif.
    wl_layout-info_fieldname    = 'LINE_COLOR'.

*    WL_LAYOUT-COL_STABLE = 'X'.
*    WL_LAYOUT-ROW_STABLE = 'X'.

    gs_variant_c-report      = sy-repid.
    call function 'REUSE_ALV_GRID_DISPLAY'
      exporting
        is_variant         = gs_variant_c
        i_callback_program = sy-repid
        it_fieldcat        = estrutura[]
        is_layout          = wl_layout
        i_save             = 'X'
        it_events          = events
        is_print           = t_print
        it_sort            = lt_sort
      tables
        t_outtab           = tg_saida.
  else.

    perform montar_layout using 'TG_SAIDAR'.

*
    clear       v_carga.
    if p_proc is not initial or p_reve is not initial.
      wl_layout-box_fieldname = 'MARK'.
    endif.
    wl_layout-info_fieldname    = 'LINE_COLOR'.
    concatenate sy-repid 'R' into w_repid.
    gs_variant_c-report      = w_repid.
    call function 'REUSE_ALV_GRID_DISPLAY'
      exporting
        is_variant         = gs_variant_c
        i_callback_program = sy-repid
        it_fieldcat        = estrutura[]
        is_layout          = wl_layout
        i_save             = 'X'
        it_events          = events
        is_print           = t_print
        it_sort            = lt_sort
      tables
        t_outtab           = tg_saidar.
  endif.


endform.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form definir_eventos.

  if p_proc is not initial or p_reve is not initial  or p_raz is not initial.
    if p_raz is initial.
      perform f_carregar_eventos using:
                                       slis_ev_user_command  'XUSER_COMMAND', "para tira duplo click
                                       slis_ev_pf_status_set 'XPF_STATUS_SET',
                                       slis_ev_top_of_page   'XTOP_OF_PAGE'.
    else.
      perform f_carregar_eventos using:
                                 slis_ev_user_command  'XUSER_COMMANDR', "para tira duplo click
                                 slis_ev_pf_status_set 'XPF_STATUS_SET',
                                 slis_ev_top_of_page   'XTOP_OF_PAGE'.
    endif.
  else.
    perform f_carregar_eventos using:
                                     slis_ev_pf_status_set 'XPF_STATUS_SET',
                                     slis_ev_top_of_page   'XTOP_OF_PAGE'.
  endif.

endform.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
form f_carregar_eventos using    name form.
  clear xs_events.
  xs_events-name = name.
  xs_events-form = form.
  append xs_events to events.

endform.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout using p_tabname.
  data: wl_curr1_aux  type dd03p-scrtext_l,
        wl_curr2_aux  type dd03p-scrtext_l,
        wl_curr3_aux  type dd03p-scrtext_l,
        wl_saldo_cor  type dd03p-scrtext_l,
        wl_saldo_cor2 type dd03p-scrtext_l,
        wl_vlr_ajst   type dd03p-scrtext_l,
        wl_vlr_ajst2  type dd03p-scrtext_l.


  refresh estrutura.
  clear: wg_t001.
  read table tg_t001 into wg_t001 index 1.

  if p_tabname eq 'TG_SAIDAR'.
    read table tg_t882g into wg_t882g
      with key rbukrs =  s_bukrs-low
               binary search.

    if wg_t001-land1 eq 'BR' or wg_t001-land1 eq 'NL' or wg_t001-land1 eq 'CH' or wg_t001-land1 eq 'LU'.
      concatenate text-060 wg_t882g-curr1 into wl_curr1_aux separated by space.
      concatenate text-060 wg_t882g-curr2 into wl_curr2_aux separated by space.
      if s_bukrs-low = '0004' or s_bukrs-low = '0037'.
        concatenate text-061 wg_t882g-curr1 into wl_saldo_cor separated by space.
        concatenate text-062 wg_t882g-curr1 into wl_vlr_ajst separated by space.
      else.
        concatenate text-061 wg_t882g-curr2 into wl_saldo_cor separated by space.
        concatenate text-062 wg_t882g-curr2 into wl_vlr_ajst separated by space.
      endif.

      perform montar_estrutura using:
                1  'FAGLFLEXT'         'RACCT'          'TG_SAIDAR' 'RACCT'                   text-a25                   ' ' ,
                1  'FAGLFLEXT'         'RASSC'          'TG_SAIDAR' 'RASSC'                   text-a26                   ' ' ,
                2  'SKAT'              'TXT50'          'TG_SAIDAR' 'TXT50'                   text-a27                   ' ' ,
                3  'SKA1'              'KTOKS'          'TG_SAIDAR' 'KTOKS'                   text-a28                   ' ' ,
                4  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'CURR1'                   wl_curr1_aux              '15' ,
                5  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'CURR2'                   wl_curr2_aux              '15' ,
                6  'ZFIT0082'          'TX_USD'         'TG_SAIDAR' 'TX_USD'                  text-a29                  '10' ,
                7  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'SALDO_CORR'              wl_saldo_cor              '15' ,
                8  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'VLR_AJUST'               wl_vlr_ajst               '15' ,
                9  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDAR' 'BELNR'                   text-a30                  ' ' ,
               11  ' '                 ' '              'TG_SAIDAR' 'LOG'                     text-a14                  ' ' ,
               12  'BKPF'              'STBLG'          'TG_SAIDAR' 'STBLG'                   text-a33             ' ' . " USER STORY 155163 // MMSILVA - 15.10.2024
    elseif wg_t001-land1 eq 'AR' or wg_t001-land1 eq 'PY'.
      concatenate text-060 wg_t882g-curr1 into wl_curr1_aux  separated by space.
      concatenate text-060 wg_t882g-curr2 into wl_curr2_aux  separated by space.
      concatenate text-060 wg_t882g-curr3 into wl_curr3_aux  separated by space.
      concatenate text-063 wg_t882g-curr2 into wl_saldo_cor  separated by space.
      concatenate text-063 wg_t882g-curr3 into wl_saldo_cor2 separated by space.
      concatenate text-061 wg_t882g-curr2 into wl_vlr_ajst   separated by space.
      concatenate text-061 wg_t882g-curr3 into wl_vlr_ajst2  separated by space.

      perform montar_estrutura using:
            1  'FAGLFLEXT'         'RACCT'          'TG_SAIDAR' 'RACCT'                   text-a31             ' ' ,
            2  'SKAT'              'TXT50'          'TG_SAIDAR' 'TXT50'                   text-a32             ' ' ,
            3  'SKA1'              'KTOKS'          'TG_SAIDAR' 'KTOKS'                   text-a13             ' ' ,
            4  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'CURR1'                   wl_curr1_aux         '15' ,
            5  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'CURR2'                   wl_curr2_aux         '15' ,
            6  'FAGLFLEXT'         'OSLVT'          'TG_SAIDAR' 'CURR3'                   wl_curr3_aux         '15' ,
            7  'ZFIT0082'          'TX_USD'         'TG_SAIDAR' 'TX_USD'                  text-a29             '10' ,
            8  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'VLR_AJUST'               wl_vlr_ajst          '15' ,
            9  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'SALDO_CORR'              wl_saldo_cor         '15' ,

           10  'ZFIT0082'          'TX_BRL'         'TG_SAIDAR' 'TX_BRL'                  text-a09             '10' ,
           11  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'VLR_AJUST2'              wl_vlr_ajst2         '15' ,
           12  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'SALDO_CORR2'             wl_saldo_cor2        '15' ,

           13  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDAR' 'BELNR'                   text-a30             ' ' ,
           15  ' '                 ' '              'TG_SAIDAR' 'LOG'                     text-a14             ' ' .
      "
    endif.
  elseif p_tabname eq 'TG_SAIDA'.
    read table tg_t882g into wg_t882g
      with key rbukrs =  s_bukrs-low
               binary search.
    if wg_t001-land1 eq 'BR' or wg_t001-land1 eq 'NL' or wg_t001-land1 eq 'CH' or wg_t001-land1 eq 'LU'.
      concatenate text-012 wg_t882g-curr1 into wl_curr1_aux separated by space.
      concatenate text-012 wg_t882g-curr2 into wl_curr2_aux separated by space.
      if s_bukrs-low = '0004' or s_bukrs-low = '0037'.
        concatenate text-013 wg_t882g-curr1 into wl_saldo_cor separated by space.
        concatenate text-014 wg_t882g-curr1 into wl_vlr_ajst separated by space.
      else.
        concatenate text-013 wg_t882g-curr2 into wl_saldo_cor separated by space.
        concatenate text-014 wg_t882g-curr2 into wl_vlr_ajst separated by space.
      endif.

      if p_clie is not initial.
        perform montar_estrutura using:
                  1  'BSID'              'KUNNR'          'TG_SAIDA' 'KUNNR'                   text-a01             '06' ,
                  2  'KNA1'              'NAME1'          'TG_SAIDA' 'NAME1'                   text-a02             '05' ,
                  3  'BSID'              'HKONT'          'TG_SAIDA' 'HKONT'                   text-a03             '08' ,
                  4  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   text-a04             '05' ,
                  5  ' '                 ' '              'TG_SAIDA' 'VBUND'                   text-a05             '05' ,
                  6  ' '                 ' '              'TG_SAIDA' 'BEWAR'                   text-a06             '05' ,
                  7  'BSIK'              'BELNR'          'TG_SAIDA' 'BELNR2'                  ' '                  ' ' ,
                  8  'BSIK'              'BUZEI'          'TG_SAIDA' 'BUZEI'                   ' '                  '03' ,
                  9  'BSIK'              'WAERS'          'TG_SAIDA' 'WAERS'                   ' '                  '05' ,
                 10  'BSIK'              'BUDAT'          'TG_SAIDA' 'BUDAT'                   ' '                  '08' ,
                 11  'BSIK'              'BLART'          'TG_SAIDA' 'BLART'                   ' '                  '03' ,
                 12  'BSIK'              'XBLNR'          'TG_SAIDA' 'XBLNR'                   text-a07             '03',
                 13  'BSID'              'VBEL2'          'TG_SAIDA' 'DOC'                     text-a08             '03',
                 14  'BSID'              'VPOS2'          'TG_SAIDA' 'ITEM'                    text-a10             '03' ,
                 15  'BSIK'              'DMBTR'          'TG_SAIDA' 'DMBTR'                   wl_curr1_aux         ' ' ,
                 16  'BSIK'              'DMBE2'          'TG_SAIDA' 'DMBE2'                   wl_curr2_aux         ' ' ,
                 17  'ZFIT0084'          'TX_USD'         'TG_SAIDA' 'TX_USD'                  text-a11             ' ' ,
                 18  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR'              wl_saldo_cor         ' ' ,
                 19  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST'               wl_vlr_ajst          ' ' ,
                 20  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDA' 'BELNR'                   text-a12             ' ' ,
                 22  ' '                 ' '              'TG_SAIDA' 'LOG'                     text-a14             ' ' ,
                 23  'BKPF'              'STBLG'          'TG_SAIDA' 'STBLG'                   text-a33             ' ' . " USER STORY 155163 // MMSILVA - 15.10.2024

      else.
        perform montar_estrutura using:
                       1  'BSIK'              'LIFNR'          'TG_SAIDA' 'KUNNR'                   text-a18                 '06' ,
                       2  'LFA1'              'NAME1'          'TG_SAIDA' 'NAME1'                   text-a19                 '05' ,
                       3  'BSID'              'HKONT'          'TG_SAIDA' 'HKONT'                   text-a03                 '08' ,
                       4  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   text-a04                 '05' ,
                       5  ' '                 ' '              'TG_SAIDA' 'VBUND'                   text-a05                 '05' ,
                       6  ' '                 ' '              'TG_SAIDA' 'BEWAR'                   text-a06                 '05' ,
                       7  'BSIK'              'BELNR'          'TG_SAIDA' 'BELNR2'                  ' '                      ' ' ,
                       8  'BSIK'              'BUZEI'          'TG_SAIDA' 'BUZEI'                   ' '                      '03' ,
                       9  'BSIK'              'WAERS'          'TG_SAIDA' 'WAERS'                   ' '                      '05' ,
                      10  'BSIK'              'BUDAT'          'TG_SAIDA' 'BUDAT'                   ' '                      '08' ,
                      11  'BSIK'              'BLART'          'TG_SAIDA' 'BLART'                   ' '                      '03' ,
                      12  'BSIK'              'XBLNR'          'TG_SAIDA' 'XBLNR'                   text-a07                 '03' ,
                      13  'BSIK'              'EBELN'          'TG_SAIDA' 'DOC'                     text-a20                 '03' ,
                      14  'BSIK'              'EBELP'          'TG_SAIDA' 'ITEM'                    text-a10                 '03' ,
                      15  'BSIK'              'DMBTR'          'TG_SAIDA' 'DMBTR'                   wl_curr1_aux             ' ' ,
                      16  'BSIK'              'DMBE2'          'TG_SAIDA' 'DMBE2'                   wl_curr2_aux             ' ' ,
                      17  'ZFIT0084'          'TX_USD'         'TG_SAIDA' 'TX_USD'                  text-a11                 ' ' ,
                      18  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR'              wl_saldo_cor             ' ' ,
                      19  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST'               wl_vlr_ajst              ' ' ,
                      20  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDA' 'BELNR'                   text-a12                 ' ' ,
                      22  ' '                 ' '              'TG_SAIDA' 'LOG'                     text-a14                 ' ' ,
                      23  'BKPF'              'STBLG'          'TG_SAIDA' 'STBLG'                   text-a33                 ' ' . " USER STORY 155163 // MMSILVA - 15.10.2024

      endif.
    elseif wg_t001-land1 eq 'AR' or wg_t001-land1 eq 'PY' or wg_t001-land1 eq 'LU'..

      concatenate text-012 wg_t882g-curr1 into wl_curr1_aux separated by space.
      concatenate text-012 wg_t882g-curr2 into wl_curr2_aux separated by space.
      concatenate text-012 wg_t882g-curr3 into wl_curr3_aux separated by space.
      concatenate text-a21 wg_t882g-curr2 into wl_saldo_cor separated by space.
      concatenate text-a21 wg_t882g-curr3 into wl_saldo_cor2 separated by space.
      concatenate text-a22 wg_t882g-curr2 into wl_vlr_ajst separated by space.
      concatenate text-a22 wg_t882g-curr3 into wl_vlr_ajst2 separated by space.
      if p_clie is not initial.
        perform montar_estrutura using:
                  1  'BSID'              'KUNNR'          'TG_SAIDA' 'KUNNR'                   'Cliente'          ' ' ,
                  2  'KNA1'              'NAME1'          'TG_SAIDA' 'NAME1'                   'Nombre cliente'   ' ' ,
                  3  'BSID'              'HKONT'          'TG_SAIDA' 'HKONT'                   'Cuenta '          ' ' ,
                  4  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   'Descripción'      ' ' ,
                  5  ' '                 ' '              'TG_SAIDA' 'VBUND'                   'Soc.Parc.'        ' ' ,
                  6  ' '                 ' '              'TG_SAIDA' 'BEWAR'                   'Tp.Movto'         ' ' ,
                  7  'BSIK'              'BELNR'          'TG_SAIDA' 'BELNR2'                  ' '         ' ' ,
                  8  'BSIK'              'WAERS'          'TG_SAIDA' 'WAERS'                   ' '         ' ' ,
                  9  'BSIK'              'BUDAT'          'TG_SAIDA' 'BUDAT'                   'Dt.Fecha'         ' ' ,
                 10  'BSIK'              'BLART'          'TG_SAIDA' 'BLART'                   'Tp.Dcto.'         ' ' ,
                 11  'BSIK'              'XBLNR'          'TG_SAIDA' 'XBLNR'                   'Referência'       ' ' ,
                 12  'BSID'              'VBEL2'          'TG_SAIDA' 'DOC'                     'Nro.OV'           ' ' ,
                 13  'BSID'              'VPOS2'          'TG_SAIDA' 'ITEM'                    'Item'             ' ' ,
                 14  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBTR'                   wl_curr1_aux       ' ' ,
                 15  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBE2'                   wl_curr2_aux       ' ' ,
                 16  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBE3'                   wl_curr3_aux       ' ' ,
                 17  'ZFIT0084'          'TX_USD'         'TG_SAIDA' 'TX_USD'                  'Taxa USD'         ' ' ,
                 18  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR'              wl_saldo_cor       ' ' ,
                 19  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST'               wl_vlr_ajst        ' ' ,
                 20  'ZFIT0084'          'TX_BRL'         'TG_SAIDA' 'TX_BRL'                  'Taxa BRL'         ' ' ,
                 21  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR2'             wl_saldo_cor2      ' ' ,
                 22  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST2'              wl_vlr_ajst2       ' ' ,
                 23  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDA' 'BELNR'                   'Nro.documento'    ' ' ,
                 25  ' '                 ' '              'TG_SAIDA' 'LOG'                     'Log'              ' ' .

      else.
        perform montar_estrutura using:
                       1  'BSIK'              'LIFNR'          'TG_SAIDA' 'KUNNR'                   'Proveedor'            ' ' ,
                       2  'LFA1'              'NAME1'          'TG_SAIDA' 'NAME1'                   'Nombre Proveedor'     ' ' ,
                       3  'BSID'              'HKONT'          'TG_SAIDA' 'HKONT'                   'Cuenta'               ' ' ,
                       4  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   'Descripción'          ' ' ,
                       5  ' '                 ' '              'TG_SAIDA' 'VBUND'                   'Soc.Parc.'          ' ' ,
                       6  ' '                 ' '              'TG_SAIDA' 'BEWAR'                   'Tp.Movto'           ' ' ,
                       7  'BSIK'              'BELNR'          'TG_SAIDA' 'BELNR2'                  ' '             ' ' ,
                       8  'BSIK'              'WAERS'          'TG_SAIDA' 'WAERS'                   ' '             ' ' ,
                       9  'BSIK'              'BUDAT'          'TG_SAIDA' 'BUDAT'                   'Dt.Fecha'             ' ' ,
                      10  'BSIK'              'BLART'          'TG_SAIDA' 'BLART'                   'Tp.Dcto.'             ' ' ,
                      11  'BSIK'              'XBLNR'          'TG_SAIDA' 'XBLNR'                   'Referência'           ' ' ,
                      12  'BSIK'              'EBELN'          'TG_SAIDA' 'DOC'                     'Solicitud'            ' ' ,
                      13  'BSIK'              'EBELP'          'TG_SAIDA' 'ITEM'                    'Item'                 ' ' ,
                      14  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBTR'                   wl_curr1_aux           ' ' ,
                      15  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBE2'                   wl_curr2_aux           ' ' ,
                      16  'BSIK'              'BDIF2'          'TG_SAIDA' 'DMBE3'                   wl_curr3_aux           ' ' ,
                      17  'ZFIT0084'          'TX_USD'         'TG_SAIDA' 'TX_USD'                  'Taxa USD'             ' ' ,
                      18  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR'              wl_saldo_cor           ' ' ,
                      19  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST'               wl_vlr_ajst            ' ' ,
                      20  'ZFIT0084'          'TX_BRL'         'TG_SAIDA' 'TX_BRL'                  'Taxa BRL'             ' ' ,
                      21  'BSIK'              'DMBTR'          'TG_SAIDA' 'SALDO_CORR2'             wl_saldo_cor2          ' ' ,
                      22  'BSIK'              'DMBTR'          'TG_SAIDA' 'VLR_AJUST2'              wl_vlr_ajst2           ' ' ,
                      23  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDA' 'BELNR'                   'Nro.documento'        ' ' ,
                      25  ' '                 ' '              'TG_SAIDA' 'LOG'                     'Log'                  ' ' .
      endif.
    endif.
  elseif p_tabname eq 'TG_SAIDA_EXEC'.

    if p_clie is not initial.
      perform montar_estrutura using:
          1  ' '          ' '                'TG_SAIDA_EXEC' 'ICON'      text-a23             ' ',
          2  ' '          ' '                'TG_SAIDA_EXEC' 'MSG'       'Msg'                '80',
          3  ' '          ' '                'TG_SAIDA_EXEC' 'TIPO'      text-a24             '30',
          4  'KNA1'       'KUNNR'            'TG_SAIDA_EXEC' 'LIFNR'     text-a01             ' ',
          5  'KNA1'       'NAME1'            'TG_SAIDA_EXEC' 'NAME1'     text-a02             ' ',
          6  'BSIK'       'HKONT'            'TG_SAIDA_EXEC' 'HKONT'     text-a03             ' ',
          7  'SKAT'       'TXT50'            'TG_SAIDA_EXEC' 'TXT50'     text-a04             ' '.

    elseif  p_forn is not initial..
      perform montar_estrutura using:
        1  ' '          ' '                'TG_SAIDA_EXEC' 'ICON'       text-a23               ' ',
        2  ' '          ' '                'TG_SAIDA_EXEC' 'MSG'       'Msg'                   '80',
        3  ' '          ' '                'TG_SAIDA_EXEC' 'TIPO'       text-a24               '30',
        4  'LFA1'       'LIFNR'            'TG_SAIDA_EXEC' 'LIFNR'      text-a18               ' ',
        5  'LFA1'       'NAME1'            'TG_SAIDA_EXEC' 'NAME1'      text-a19               ' ',
        6  'BSIK'       'HKONT'            'TG_SAIDA_EXEC' 'HKONT'      text-a03               ' ',
        7  'SKAT'       'TXT50'            'TG_SAIDA_EXEC' 'TXT50'      text-a04               ' '.
    else.
      perform montar_estrutura using:
          1  ' '          ' '                'TG_SAIDA_EXEC' 'ICON'      text-a23           ' ',
          2  ' '          ' '                'TG_SAIDA_EXEC' 'MSG'       text-a04           '80',
          3  'FAGLFLEXT'  'RACCT'            'TG_SAIDA_EXEC' 'RACCT'     text-a25           ' ',
          4  'SKAT'       'TXT50'            'TG_SAIDA_EXEC' 'TXT50'     text-a27           ' '.


    endif.

  endif.

endform.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
form montar_estrutura using value(p_col_pos)       type i
                            value(p_ref_tabname)   like dd02d-tabname
                            value(p_ref_fieldname) like dd03d-fieldname
                            value(p_tabname)       like dd02d-tabname
                            value(p_field)         like dd03d-fieldname
                            p_scrtext_l
*                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            value(p_outputlen).

  clear wa_estrutura.

  if p_field eq 'BELNR'
  or p_field eq 'BELNR_EST'
  or p_field eq 'BELNR_INV'
  or p_field eq 'BELNR_INV_EST'
  or p_field eq 'BELNR2'
  or p_field eq 'STBLG'. "USER STORY 155163 // MMSILVA - 24.10.2024
    wa_estrutura-just = 'C'.
    wa_estrutura-hotspot = c_x.
  endif.


  wa_estrutura-outputlen     = p_outputlen.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.

  if p_scrtext_l is not initial.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  endif.

  translate  wa_estrutura-fieldname     to upper case.
  translate  wa_estrutura-tabname       to upper case.
  translate  wa_estrutura-ref_tabname   to upper case.
  translate  wa_estrutura-ref_fieldname to upper case.

  append wa_estrutura to estrutura.

endform.                    " MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selecionar_dados .
  data: wl_tabix     type sy-tabix,
        wl_bsik_bsid type ty_bsik_bsid.

  select *                             "#EC CI_DB_OPERATION_OK[2431747]
        from t882g
        into table tg_t882g
         where rbukrs in s_bukrs.

  if sy-subrc is initial.
    select *
      from t001
      into table tg_t001
       for all entries in tg_t882g
       where bukrs eq tg_t882g-rbukrs.
  else.
    exit.
  endif.

  ranges: rl_budat for bsik-budat.

  refresh: rl_budat.
** Processamento
  if p_proc is not initial or p_reve is not initial.
    vg_ryear  = s_mes-low+2(4).
    concatenate vg_ryear s_mes-low(2) '01' into vg_last_day_aux.
    vg_last_day = vg_last_day_aux.
    rl_budat-sign = 'I'.
    rl_budat-option = 'BT'.

    call function 'BKK_GET_MONTH_LASTDAY'
      exporting
        i_date = vg_last_day
      importing
        e_date = vg_last_day.

    convert inverted-date vg_last_day into date vg_last_day_aux2.

    vg_first_day = vg_last_day.

    add 1 to vg_first_day.


    rl_budat-high      = vg_last_day.
    append rl_budat.
    clear: rl_budat.


    convert inverted-date vg_first_day into date vg_last_day_aux.
*
    if p_forn is not initial.
      read table tg_t882g into wg_t882g index 1.
      if s_bukrs-low eq '0004' or s_bukrs-low = '0037'.
        wg_t882g-curr1 = 'USD'.
      endif.
      select bukrs belnr buzei gjahr budat hkont waers monat dmbtr dmbe2
             dmbe3 lifnr xblnr blart ebeln ebelp umsks umskz shkzg bschl
        from bsik
        into table tg_bsik_bsid
         where bukrs  in s_bukrs
           and budat  in rl_budat
           and hkont  in s_saknr
           and blart  ne 'VC'
           and waers  eq wg_t882g-curr1
           and anln1  eq ''.

      "ALRS 13.11.2014
      select bukrs belnr buzei gjahr budat hkont waers monat dmbtr dmbe2
       dmbe3 lifnr xblnr blart ebeln ebelp umsks umskz shkzg bschl
        from bsak
        appending table tg_bsik_bsid
         where bukrs  in s_bukrs
           and augdt  gt vg_last_day
           and budat  in rl_budat
           and hkont  in s_saknr
           and blart  ne 'VC'
           and waers  eq wg_t882g-curr1
           and anln1  eq ''.

      if tg_bsik_bsid[] is not initial.
        select *
          from lfa1
          into table tg_lfa1
           for all entries in tg_bsik_bsid
            where lifnr eq tg_bsik_bsid-lifnr.
      endif.
    else.
      read table tg_t882g into wg_t882g index 1.
      if s_bukrs-low eq '0004' or s_bukrs-low eq '0037' .
        wg_t882g-curr1 = 'USD'.
      endif.
      select bukrs belnr buzei gjahr budat hkont waers monat dmbtr dmbe2
             dmbe3 kunnr xblnr blart vbel2 vpos2 umsks umskz shkzg bschl augbl
        from bsid
        into table tg_bsik_bsid
         where bukrs  in s_bukrs
           and budat  in rl_budat
           and hkont  in s_saknrc
           and blart  ne 'VC'
           and waers  eq wg_t882g-curr1.

      "ALRS 13.11.2014
      select bukrs belnr buzei gjahr budat hkont waers monat dmbtr dmbe2
             dmbe3 kunnr xblnr blart vbel2 vpos2 umsks umskz shkzg bschl augbl
        from bsad
        appending table tg_bsik_bsid
         where bukrs  in s_bukrs
           and augdt  gt vg_last_day
           and budat  in rl_budat
           and hkont  in s_saknrc
           and blart  ne 'VC'
           and waers  eq wg_t882g-curr1.

      if tg_bsik_bsid[] is not  initial.
        select *
          from kna1
          into table tg_kna1
           for all entries in tg_bsik_bsid
            where kunnr eq tg_bsik_bsid-lifnr.

      endif.
    endif.

* Visualização
  else.
    select *
      from zfit0084
      into table tg_0084
       where bukrs   in s_bukrs
         and mes_ano in s_mes
         and saknr   in s_saknr
         and saknr   in s_saknrc.

    if tg_0084 is not initial.
      select *
          from zfit0081
          into table tg_0081
          where bukrs  in s_bukrs.

      if p_forn is not initial.
        read table tg_t882g into wg_t882g index 1.
        select bukrs belnr buzei gjahr budat hkont waers monat dmbtr dmbe2
               dmbe3 lifnr xblnr blart ebeln ebelp umsks umskz shkzg bschl augbl
          from bsik
          into table tg_bsik_bsid
          for all entries in tg_0084
           where bukrs  eq tg_0084-bukrs
             and belnr  eq tg_0084-belnr
             and buzei  eq tg_0084-buzei.

        select bukrs belnr buzei gjahr budat hkont waers monat dmbtr dmbe2
               dmbe3 lifnr xblnr blart ebeln ebelp umsks umskz shkzg bschl augbl
          from bsak
          appending table tg_bsik_bsid
          for all entries in tg_0084
           where bukrs  eq tg_0084-bukrs
             and lifnr  eq tg_0084-lifnr
             and belnr  eq tg_0084-belnr
             and buzei  eq tg_0084-buzei
             and augdt  gt vg_last_day
             and budat  in rl_budat.


        if tg_bsik_bsid[] is not initial.
          select *
            from lfa1
            into table tg_lfa1
             for all entries in tg_bsik_bsid
              where lifnr eq tg_bsik_bsid-lifnr.
        endif.

      else.
        read table tg_t882g into wg_t882g index 1.
        select bukrs belnr buzei gjahr budat hkont waers monat dmbtr dmbe2
               dmbe3 kunnr xblnr blart vbel2 vpos2 umsks umskz shkzg bschl augbl
          from bsid
          into table tg_bsik_bsid
           for all entries in tg_0084
           where bukrs  eq tg_0084-bukrs
             and kunnr  eq tg_0084-lifnr
             and belnr  eq tg_0084-belnr
             and buzei  eq tg_0084-buzei.

        select bukrs belnr buzei gjahr budat hkont waers monat dmbtr dmbe2
              dmbe3 kunnr xblnr blart vbel2 vpos2 umsks umskz shkzg bschl augbl
         from bsad
         appending table tg_bsik_bsid
          for all entries in tg_0084
          where bukrs  eq tg_0084-bukrs
            and kunnr  eq tg_0084-lifnr
            and belnr  eq tg_0084-belnr
            and buzei  eq tg_0084-buzei
            and augdt  gt vg_last_day
            and budat  in rl_budat.

        if tg_bsik_bsid[] is not initial.
          select *
            from kna1
            into table tg_kna1
             for all entries in tg_bsik_bsid
              where kunnr eq tg_bsik_bsid-lifnr.

        endif.
      endif.

      if tg_bsik_bsid[] is not initial.
        select *                       "#EC CI_DB_OPERATION_OK[2389136]
          from ska1                    "#EC CI_DB_OPERATION_OK[2431747]
          into table tg_ska1
           for all entries in tg_bsik_bsid
           where saknr eq tg_bsik_bsid-hkont
           and ktopl = '0050'.

        if sy-subrc is initial.
          select *
           from skat
           into table tg_skat
            for all entries in tg_ska1
            where saknr eq tg_ska1-saknr
              and ktopl eq '0050'
              and spras eq sy-langu.
        endif.
      endif.

      select *
        from zib_contabil
        into table tg_zib
         for all entries in tg_0084
         where obj_key eq tg_0084-obj_key.

      select *
        from zib_contabil_chv
        into table tg_zib_chv
         for all entries in tg_0084
         where obj_key eq tg_0084-obj_key.

      select *
        from zib_contabil_err
        into table tg_zib_err
         for all entries in tg_0084
         where obj_key eq tg_0084-obj_key.

      read table tg_t882g into wg_t882g index 1.

    endif.
    exit.
  endif.

** Documentos estornados
  read table tg_t882g into wg_t882g index 1.

*  SELECT *
*    FROM BKPF
*    INTO CORRESPONDING FIELDS OF TABLE TG_BKPF_LANC
*    WHERE BUKRS  IN S_BUKRS
*     AND  BUDAT  IN RL_BUDAT
*     AND  STBLG  NE ''
*     AND  BLART  EQ 'LM'
*     AND  WAERS  EQ WG_T882G-CURR1.
*
*  IF TG_BKPF_LANC[] IS NOT INITIAL.
*    SELECT *
*      FROM BKPF
*      INTO CORRESPONDING FIELDS OF TABLE TG_BKPF_EST
*      FOR ALL ENTRIES IN TG_BKPF_LANC
*      WHERE BUKRS EQ TG_BKPF_LANC-BUKRS
*       AND  GJAHR EQ TG_BKPF_LANC-STJAH
*       AND  STBLG EQ TG_BKPF_LANC-BELNR
*       AND  ( ( MONAT NE TG_BKPF_LANC-MONAT AND GJAHR EQ TG_BKPF_LANC-GJAHR ) OR
*              ( MONAT EQ TG_BKPF_LANC-MONAT AND GJAHR NE TG_BKPF_LANC-GJAHR ) ).
*
*    IF SY-SUBRC IS INITIAL.
*      FIELD-SYMBOLS : <FS_BKPF> TYPE BKPF.
*
*      LOOP AT TG_BKPF_LANC ASSIGNING <FS_BKPF>.
*        READ TABLE TG_BKPF_EST INTO WG_BKPF WITH KEY BELNR = <FS_BKPF>-STBLG.
*        IF SY-SUBRC IS NOT INITIAL.
*          DELETE TG_BKPF_LANC WHERE STBLG = <FS_BKPF>-STBLG.
*        ENDIF.
*      ENDLOOP.
*
*      SELECT *
*        FROM BSEG
*        INTO CORRESPONDING FIELDS OF TABLE TG_BSEG
*        FOR ALL ENTRIES IN TG_BKPF_LANC
*        WHERE BELNR EQ TG_BKPF_LANC-BELNR
*         AND  BUKRS EQ TG_BKPF_LANC-BUKRS
*         AND  HKONT IN S_SAKNR
*         AND  GJAHR EQ TG_BKPF_LANC-GJAHR.
*
*      IF SY-SUBRC IS INITIAL.
*        IF P_FORN IS NOT INITIAL.
*          DELETE TG_BSEG WHERE KOART NE 'K'.
*        ELSE.
*          DELETE TG_BSEG WHERE KOART NE 'D'.
*        ENDIF.
*        DELETE TG_BSEG WHERE KUNNR IS INITIAL.
*
*        LOOP AT TG_BSEG INTO WG_BSEG.
*          CLEAR WL_BSIK_BSID.
*          READ TABLE TG_BKPF_LANC INTO WG_BKPF WITH KEY BELNR = WG_BSEG-BELNR.
*          IF SY-SUBRC IS INITIAL.
*            READ TABLE TG_BSIK_BSID INTO WL_BSIK_BSID  WITH KEY BELNR = WG_BSEG-BELNR
*                                                                BUZEI = WG_BSEG-BUZEI.
*            IF SY-SUBRC NE 0.
*              CLEAR WL_BSIK_BSID.
*              MOVE-CORRESPONDING WG_BKPF TO WL_BSIK_BSID.
*
*              WL_BSIK_BSID-BUZEI = WG_BSEG-BUZEI.
*              WL_BSIK_BSID-HKONT = WG_BSEG-HKONT.
*              WL_BSIK_BSID-DMBTR = WG_BSEG-DMBTR.
*              WL_BSIK_BSID-DMBE2 = WG_BSEG-DMBE2.
*              WL_BSIK_BSID-DMBE3 = WG_BSEG-DMBE3.
*              WL_BSIK_BSID-LIFNR = WG_BSEG-KUNNR.
*              WL_BSIK_BSID-DOC   = WG_BSEG-EBELN.
*              WL_BSIK_BSID-ITEM  = WG_BSEG-EBELP.
*              WL_BSIK_BSID-UMSKS = WG_BSEG-UMSKS.
*              WL_BSIK_BSID-UMSKZ = WG_BSEG-UMSKZ.
*              WL_BSIK_BSID-SHKZG = WG_BSEG-SHKZG.
*              WL_BSIK_BSID-BSCHL = WG_BSEG-BSCHL.
*
*              APPEND WL_BSIK_BSID TO TG_BSIK_BSID.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.

  "Eliminar documentos estornados  até a data BASE  - ALRS
  if tg_bsik_bsid[] is not initial.
    select *
       from bkpf into table tg_bkpf_lanc
     for all entries in  tg_bsik_bsid
     where bukrs = tg_bsik_bsid-bukrs
     and   belnr = tg_bsik_bsid-belnr
     and   gjahr = tg_bsik_bsid-gjahr
     and   ( stgrd ne space or bstat = 'S' ).
    if sy-subrc = 0.
      "ALRS 13.11.2014 Estorno dentro do mês não leva
      select *
         from bkpf
         into table tg_bkpf_est
          for all entries in tg_bkpf_lanc
           where bukrs eq tg_bkpf_lanc-bukrs
             and belnr eq tg_bkpf_lanc-stblg
             and gjahr eq tg_bkpf_lanc-stjah.

      loop at tg_bkpf_lanc into wg_bkpf.
        read table tg_bkpf_est into wg_bkpfe with key bukrs = wg_bkpf-bukrs
                                                      belnr = wg_bkpf-stblg
                                                      gjahr = wg_bkpf-stjah.
        if sy-subrc = 0 and wg_bkpf-bstat ne 'S'. "07.03.2017 ALRS
          if  wg_bkpfe-budat le vg_last_day.
            delete tg_bsik_bsid where bukrs = wg_bkpf-bukrs and
                                  belnr = wg_bkpf-belnr and
                                  gjahr = wg_bkpf-gjahr.

          endif.
        else.
          delete tg_bsik_bsid where bukrs = wg_bkpf-bukrs and
                                belnr = wg_bkpf-belnr and
                                gjahr = wg_bkpf-gjahr.
        endif.
      endloop.
    endif.
  endif.

  if tg_bsik_bsid[] is not initial.
*    IF P_VISU = 'X'.
*      SELECT *
*       FROM ZFIT0084
*       INTO TABLE TG_0084
*       FOR ALL ENTRIES IN TG_BSIK_BSID
*        WHERE BUKRS   EQ TG_BSIK_BSID-BUKRS
*          AND MES_ANO IN S_MES
*          AND SAKNR   EQ TG_BSIK_BSID-HKONT
*          AND BELNR   EQ TG_BSIK_BSID-BELNR.
*
*    ENDIF.

    select *                           "#EC CI_DB_OPERATION_OK[2389136]
     from ska1                         "#EC CI_DB_OPERATION_OK[2431747]
     into table tg_ska1
      for all entries in tg_bsik_bsid
      where saknr eq tg_bsik_bsid-hkont
      and ktopl = '0050'.

    if sy-subrc is initial.
      select *
      from skat
      into table tg_skat
       for all entries in tg_ska1
       where saknr eq tg_ska1-saknr
         and ktopl eq '0050'
         and spras eq sy-langu.
    endif.

**ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg US 100617 - BG - Inicio

    data: v_mes type zde_mes,
          v_ano type zde_ano.

    v_mes = s_mes-low(2).
    v_ano = s_mes-low+2(4).

    select *
    from zfit0185 into table tg_zfit0185
    where mes eq v_mes
      and ano eq v_ano
      and empresa eq s_bukrs-low
      and  categoria eq 'B'
      and moeda_origem in ('USD', 'BRL')
      and moeda_destino in ('BRL', 'ARS', 'PYG', 'EUR', 'CHF').

    select *
 from zfit0185 into table tg_zfit0185_lday
 where mes eq v_mes
   and ano eq v_ano
   and empresa eq s_bukrs-low
     and categoria eq 'B'
      and moeda_origem in ('USD', 'BRL')
      and moeda_destino in ('BRL', 'ARS', 'PYG', 'EUR', 'CHF').

    select *
      from tcurr
      into table tg_tcurr
       where gdatu eq vg_last_day_aux
         and kurst eq 'B'
         and fcurr in ('USD', 'BRL')
         and tcurr in ('BRL', 'ARS', 'PYG', 'EUR', 'CHF').

    select *
      from tcurr
      into table tg_tcurr_lday
       where gdatu eq vg_last_day_aux2
         and kurst eq 'B'
         and fcurr in ('USD', 'BRL')
         and tcurr in ('BRL', 'ARS', 'PYG', 'EUR', 'CHF').

    select *
      from zfit0081
      into table tg_0081
      where bukrs  in s_bukrs.

    if p_visu ne 'X'.
      tg_bsik_bsid_aux[] = tg_bsik_bsid[].
      sort tg_bsik_bsid_aux by hkont.
      delete adjacent duplicates from tg_bsik_bsid_aux comparing hkont.

      select *
         from zfit0084
         into table tg_0084
         for all entries in  tg_bsik_bsid_aux
          where bukrs   in s_bukrs
            and mes_ano in s_mes
            and saknr   eq tg_bsik_bsid_aux-hkont .
      refresh tg_bsik_bsid_aux.
    endif.

    if sy-subrc is initial.
      refresh: tg_0084_aux.
      tg_0084_aux[] = tg_0084[].
      delete tg_0084_aux where obj_key is initial.
      if tg_0084_aux[] is not initial.
        select *
          from zib_contabil
          into table tg_zib
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key.

        select *
          from zib_contabil_chv
          into table tg_zib_chv
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key.

        select *
          from zib_contabil_err
          into table tg_zib_err
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key.
      endif.

      refresh: tg_0084_aux.
      tg_0084_aux[] = tg_0084[].
      delete tg_0084_aux where obj_key_inv is initial.
      if tg_0084_aux[] is not initial.
        select *
          from zib_contabil
           appending table tg_zib
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key_inv.

        select *
          from zib_contabil_chv
          appending table tg_zib_chv
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key_inv.

        select *
          from zib_contabil_err
          appending table tg_zib_err
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key_inv.
      endif.

**             Estorno
      refresh: tg_0084_aux.
      tg_0084_aux[] = tg_0084[].
      delete tg_0084_aux where obj_key_est is initial.
      if tg_0084_aux[] is not initial.
        select *
          from zib_contabil
          appending table tg_zib
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key_est.

        select *
          from zib_contabil_chv
          appending table tg_zib_chv
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key_est.

        select *
          from zib_contabil_err
          appending table tg_zib_err
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key_est.
      endif.

      refresh: tg_0084_aux.
      tg_0084_aux[] = tg_0084[].
      delete tg_0084_aux where obj_key_inv_est is initial.
      if tg_0084_aux[] is not initial.
        select *
          from zib_contabil
          appending table tg_zib
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key_inv_est.

        select *
          from zib_contabil_chv
          appending table tg_zib_chv
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key_inv_est.

        select *
          from zib_contabil_err
          appending table tg_zib_err
           for all entries in tg_0084_aux
           where obj_key eq tg_0084_aux-obj_key_inv_est.
      endif.

    endif.
  else.
*    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Para essa seleção não foram encontrados'
*                                           'dados para visualização.'.
    message s836(sd) display like 'E' with text-015
                                           text-016.
    stop.
  endif.


endform.                    " SELECIONAR_DADOS
"
"alrs
form selecionar_dados_r.
  data: wl_tabix type sy-tabix.
  data: so_data type range of mkpf-budat,
        wa_data like line of so_data.

  select *
       from zfit0081
       into table tg_0081
        where cta_monet eq 'S'
          and conta_de ne ''
          and bukrs in s_bukrs.

** Processamento
  if p_proc is not initial or p_reve is not initial.
    vg_ryear  = s_mes-low+2(4).
    concatenate vg_ryear s_mes-low(2) '01' into vg_last_day_aux.
    vg_last_day = vg_last_day_aux.
    call function 'BKK_GET_MONTH_LASTDAY'
      exporting
        i_date = vg_last_day
      importing
        e_date = vg_last_day.

    convert inverted-date vg_last_day into date vg_last_day_aux2.

*    IF  NOT '0101_0200' CS S_BUKRS-LOW.
    add 1 to vg_last_day.

    convert inverted-date vg_last_day into date vg_last_day_aux.

*    ENDIF.

*    IF  NOT '0101_0200' CS S_BUKRS-LOW.
    subtract 1 from vg_last_day.
*    ENDIF.

    vg_first_day = vg_last_day.

    add 1 to vg_first_day.

    select *
      from setleaf
      into table tg_setleaf
       where setname eq 'MAGGI_ZFI061'.

    select *
      from setleaf
      into table tg_setleaf_02
       where setname eq 'MAGGI_ZFI0063'.


    if sy-subrc ne 0.
      select *                         "#EC CI_DB_OPERATION_OK[2431747]
           from t882g
           into table tg_t882g
            where rbukrs in s_bukrs.

      if sy-subrc is initial.
        select *
          from t001
          into table tg_t001
           for all entries in tg_t882g
           where bukrs eq tg_t882g-rbukrs.
      endif.
      exit.
    endif.

    if sy-subrc is initial.
      loop at tg_setleaf into wg_setleaf.
        move wg_setleaf-valfrom to wg_setleaf-ktoks.
        modify tg_setleaf from wg_setleaf.
        "
        wa_data-sign = 'I'.
        wa_data-option = 'EQ'.
        wa_data-low = wg_setleaf-ktoks.
        append wa_data  to so_data.
      endloop.

      loop at tg_setleaf_02 into wg_setleaf_02.
*        MOVE WG_SETLEAF_02-VALFROM TO WG_SETLEAF_02-KTOKS.
*        MODIFY TG_SETLEAF_02 FROM WG_SETLEAF_02.
        "
        wa_saknr-saknr = wg_setleaf_02-valfrom.
        append wa_saknr  to it_saknr.
      endloop.


      select *                         "#EC CI_DB_OPERATION_OK[2431747]
            from t882g
            into table tg_t882g
             where rbukrs in s_bukrs.

      if sy-subrc is initial.
        select *
          from t001
          into table tg_t001
           for all entries in tg_t882g
           where bukrs eq tg_t882g-rbukrs.

        if sy-subrc is initial.
*          SELECT *
*            FROM SKA1
*            INTO TABLE TG_SKA1
*             FOR ALL ENTRIES IN TG_SETLEAF
*             WHERE KTOPL EQ '0050'
*               AND KTOKS EQ TG_SETLEAF-KTOKS
*               AND SAKNR IN S_SAKNRS.
          select *                     "#EC CI_DB_OPERATION_OK[2389136]
          from ska1                    "#EC CI_DB_OPERATION_OK[2431747]
          into table tg_ska1
           for all entries in tg_0081
           where ktopl eq '0050'
             and saknr eq tg_0081-conta_de
             and saknr in s_saknrs
              and ktoks in so_data.

          if sy-subrc is initial.
            select *
              from skat
              into table tg_skat
               for all entries in tg_ska1
               where saknr eq tg_ska1-saknr
               and ktopl eq '0050'
               and spras eq sy-langu.

            select *                   "#EC CI_DB_OPERATION_OK[2431747]
              from skb1
              into table tg_skb1
               for all entries in tg_ska1
               where saknr eq tg_ska1-saknr
                 and bukrs in s_bukrs.

            if sy-subrc is initial.
              select *
                from zfit0082
                into table tg_0082
                 for all entries in tg_skb1
                 where bukrs   in s_bukrs
                   and mes_ano in s_mes
                   and saknr   eq tg_skb1-saknr.


              if sy-subrc is initial.
                refresh: tg_0082_aux.
                tg_0082_aux[] = tg_0082[].
                delete tg_0082_aux where obj_key is initial.
                if tg_0082_aux[] is not initial.
                  select *
                    from zib_contabil
                    into table tg_zib
                     for all entries in tg_0082_aux
                     where obj_key eq tg_0082_aux-obj_key.

                  select *
                    from zib_contabil_chv
                    into table tg_zib_chv
                     for all entries in tg_0082_aux
                     where obj_key eq tg_0082_aux-obj_key.

                  select *
                    from zib_contabil_err
                    into table tg_zib_err
                     for all entries in tg_0082_aux
                     where obj_key eq tg_0082_aux-obj_key.
                endif.
**             Estorno
                refresh: tg_0082_aux.
                tg_0082_aux[] = tg_0082[].
                delete tg_0082_aux where obj_key_est is initial.
                if tg_0082_aux[] is not initial.
                  select *
                    from zib_contabil
                    appending table tg_zib
                     for all entries in tg_0082_aux
                     where obj_key eq tg_0082_aux-obj_key_est.

                  select *
                    from zib_contabil_chv
                    appending table tg_zib_chv
                     for all entries in tg_0082_aux
                     where obj_key eq tg_0082_aux-obj_key_est.

                  select *
                    from zib_contabil_err
                    appending table tg_zib_err
                     for all entries in tg_0082_aux
                     where obj_key eq tg_0082_aux-obj_key_est.
                endif.
              endif.

              if tg_skb1[] is not initial.
                select *
                  from faglflext
                  into table tg_faglflext
                   for all entries in tg_skb1
                   where ryear  eq vg_ryear
                     and rbukrs in s_bukrs
                     and racct  eq tg_skb1-saknr
                     and rldnr  eq '0L'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG

                data: v_mes type zde_mes,
                      v_ano type zde_ano.

                v_mes = s_mes-low(2).
                v_ano = s_mes-low+2(4).

                select *
                from zfit0185 into table tg_zfit0185
                where mes eq v_mes
                   and ano eq v_ano
                   and empresa eq s_bukrs-low
                   and categoria eq 'B'
                   and moeda_origem in ('USD', 'BRL')
                   and moeda_destino in ('BRL', 'ARS', 'PYG','EUR','CHF').

                select *
             from zfit0185
             into table tg_zfit0185_lday
             where mes eq v_mes
                and ano eq v_ano
                and empresa eq s_bukrs-low
                and categoria eq 'B'
                and moeda_origem in ('USD', 'BRL')
                and moeda_destino in ('BRL', 'ARS', 'PYG','EUR','CHF').

                select *
           from zfit0185 into table tg_zfit0185
           where mes eq v_mes
              and ano eq v_ano
              and empresa eq s_bukrs-low
              and categoria eq 'B'
              and moeda_origem in ('USD', 'BRL')
              and moeda_destino in ('BRL', 'ARS', 'PYG','EUR','CHF').

                select *
                from zfit0185 into table tg_zfit0185_lday
                  where mes eq v_mes
                     and ano eq v_ano
                     and empresa eq s_bukrs-low
                     and categoria eq 'B'
                     and moeda_origem in ('USD', 'BRL')
                     and moeda_destino in ('BRL', 'ARS', 'PYG','EUR','CHF').

                "----------------------------------------------



                select *
               from tcurr
               into table tg_tcurr
                where gdatu eq vg_last_day_aux
                  and kurst eq 'B'
                  and fcurr in ('USD', 'BRL')
                  and tcurr in ('BRL', 'ARS', 'PYG','EUR','CHF').

                select *
                  from tcurr
                  into table tg_tcurr_lday
                   where gdatu eq vg_last_day_aux2
                     and kurst eq 'B'
                     and fcurr in ('USD', 'BRL')
                     and tcurr in ('BRL', 'ARS', 'PYG','EUR','CHF').

                select *
                  from tcurr
                  into table tg_tcurr
                   where gdatu eq vg_last_day_aux
                     and kurst eq 'B'
                     and fcurr in ('USD', 'BRL')
                     and tcurr in ('BRL', 'ARS', 'PYG','EUR','CHF').

                select *
                  from tcurr
                  into table tg_tcurr_lday
                   where gdatu eq vg_last_day_aux2
                     and kurst eq 'B'
                     and fcurr in ('USD', 'BRL')
                     and tcurr in ('BRL', 'ARS', 'PYG','EUR','CHF').



              endif.

            endif.
          endif.

        endif.
      endif.
    endif.

** Visualização
  else.
    select *
      into corresponding fields of table tg_0082
      from zfit0082
      inner join skb1 on skb1~bukrs = zfit0082~bukrs "#EC CI_DB_OPERATION_OK[2431747]
                     and skb1~saknr = zfit0082~saknr
                     and skb1~mitkz = ''

       where zfit0082~bukrs   in s_bukrs
         and zfit0082~mes_ano in s_mes
         and zfit0082~saknr   in s_saknrs.

    if sy-subrc is initial.
      select *                         "#EC CI_DB_OPERATION_OK[2431747]
        from t882g
        into table tg_t882g
         where rbukrs in s_bukrs.

      if sy-subrc is initial.
        select *
          from t001
          into table tg_t001
           for all entries in tg_t882g
           where bukrs eq tg_t882g-rbukrs.
      endif.
      select *                         "#EC CI_DB_OPERATION_OK[2431747]
        from ska1                      "#EC CI_DB_OPERATION_OK[2389136]
        into table tg_ska1
         for all entries in tg_0082
         where ktopl eq '0050'
           and saknr eq tg_0082-saknr.

      if sy-subrc is initial.
        select *
          from skat
          into table tg_skat
           for all entries in tg_ska1
           where saknr eq tg_ska1-saknr
           and ktopl eq '0050'
           and spras eq sy-langu.
      endif.

      select *
          from t001
          into table tg_t001
           for all entries in tg_0082
           where bukrs eq tg_0082-bukrs.

      refresh: tg_0082_aux.
      tg_0082_aux[] = tg_0082[].
      delete tg_0082_aux where obj_key is initial.
      if tg_0082_aux[] is not initial.
        select *
          from zib_contabil
          into table tg_zib
           for all entries in tg_0082_aux
           where obj_key eq tg_0082_aux-obj_key.

        select *
          from zib_contabil_chv
          into table tg_zib_chv
           for all entries in tg_0082_aux
           where obj_key eq tg_0082_aux-obj_key.

        select *
          from zib_contabil_err
          into table tg_zib_err
           for all entries in tg_0082_aux
           where obj_key eq tg_0082_aux-obj_key.
      endif.
**             Estorno
      refresh: tg_0082_aux.
      tg_0082_aux[] = tg_0082[].
      delete tg_0082_aux where obj_key_est is initial.
      if tg_0082_aux[] is not initial.
        select *
          from zib_contabil
          appending table tg_zib
           for all entries in tg_0082_aux
           where obj_key eq tg_0082_aux-obj_key_est.

        select *
          from zib_contabil_chv
          appending table tg_zib_chv
           for all entries in tg_0082_aux
           where obj_key eq tg_0082_aux-obj_key_est.

        select *
          from zib_contabil_err
          appending table tg_zib_err
           for all entries in tg_0082_aux
           where obj_key eq tg_0082_aux-obj_key_est.
      endif.

    else.
*                                           'dados para visualização.'.
      message s836(sd) display like 'E' with text-015
                                             text-016.
      stop.
    endif.
  endif.


endform.                    " SELECIONAR_DADOS



form organizacao_dados_r.
  data: wl_saldo_mi  type faglflext-hslvt,
        wl_saldo_mi2 type faglflext-kslvt,
        wl_saldo_mi3 type faglflext-oslvt,
        wl_saidar    like wg_saidar,
        wl_update    type sy-tabix,
        vg_racct     type faglflext-racct,
        wl_flag.

** Processamento
  if p_proc is not initial.
    sort: tg_skat          by saknr,
          tg_ska1          by saknr,
          tg_tcurr         by fcurr tcurr,
          tg_tcurr_lday    by fcurr tcurr,
          tg_zfit0185      by moeda_origem moeda_destino,
          tg_zfit0185_lday by moeda_origem moeda_destino,
          tg_t001          by bukrs,
          tg_t882g         by rbukrs,
          tg_0082          by saknr vbund,
          tg_zib_chv       by obj_key,
          tg_zib_err       by obj_key.

    loop at tg_faglflext into wg_faglflext.
      clear: wl_saldo_mi, wl_saldo_mi2, wl_saldo_mi3,
      wl_update, wl_saidar, wl_flag.

      "Documento Processado não deve mais consultar pois vai alterar novamente
      "com base diferente, a variação ou valor em moeda forte irá mudar
      "IR56206 ALRS 13.03.2015
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wg_faglflext-racct
        importing
          output = vg_racct.

      "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
      read table it_saknr into wa_saknr with key saknr =  wg_faglflext-racct.
      if sy-subrc = 0.
        read table tg_0082 into wg_0082 with key saknr = wg_faglflext-racct
                                                 vbund = wg_faglflext-rassc.
      else.
        read table tg_0082 into wg_0082 with key saknr = wg_faglflext-racct.
      endif.
      if ( sy-subrc is initial ) and ( wg_0082-obj_key is not initial ) and ( wg_0082-obj_key_est is initial ).
        continue.
      endif.
      clear: wg_0082.

      "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
      read table it_saknr into wa_saknr with key saknr =  wg_faglflext-racct.
      if sy-subrc = 0.
        read table tg_saidar into wl_saidar with key racct = wg_faglflext-racct
                                                   rassc = wg_faglflext-rassc.
      else.
        read table tg_saidar into wl_saidar with key racct = wg_faglflext-racct.
      endif.
      if sy-subrc is initial.
        wl_update = sy-tabix.
        add wl_saidar-curr1 to wl_saldo_mi.
        add wl_saidar-curr2 to wl_saldo_mi2.
        add wl_saidar-curr3 to wl_saldo_mi3.
      endif.

**  Saldo Mi1 ***************************************************************
      add wg_faglflext-hslvt to wl_saldo_mi.
      if s_mes-low(2) ge '01'.
        add wg_faglflext-hsl01 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '02'.
        add wg_faglflext-hsl02 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '03'.
        add wg_faglflext-hsl03 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '04'.
        add wg_faglflext-hsl04 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '05'.
        add wg_faglflext-hsl05 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '06'.
        add wg_faglflext-hsl06 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '07'.
        add wg_faglflext-hsl07 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '08'.
        add wg_faglflext-hsl08 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '09'.
        add wg_faglflext-hsl09 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '10'.
        add wg_faglflext-hsl10 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '11'.
        add wg_faglflext-hsl11 to wl_saldo_mi.
      endif.
      if s_mes-low(2) ge '12'.
        add wg_faglflext-hsl12 to wl_saldo_mi.
        add wg_faglflext-hsl13 to wl_saldo_mi.
        add wg_faglflext-hsl14 to wl_saldo_mi.
        add wg_faglflext-hsl15 to wl_saldo_mi.
        add wg_faglflext-hsl16 to wl_saldo_mi.
      endif.

**  Saldo Mi2 *****************************************************************
      add wg_faglflext-kslvt to wl_saldo_mi2.
      if s_mes-low(2) ge '01'.
        add wg_faglflext-ksl01 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '02'.
        add wg_faglflext-ksl02 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '03'.
        add wg_faglflext-ksl03 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '04'.
        add wg_faglflext-ksl04 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '05'.
        add wg_faglflext-ksl05 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '06'.
        add wg_faglflext-ksl06 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '07'.
        add wg_faglflext-ksl07 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '08'.
        add wg_faglflext-ksl08 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '09'.
        add wg_faglflext-ksl09 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '10'.
        add wg_faglflext-ksl10 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '11'.
        add wg_faglflext-ksl11 to wl_saldo_mi2.
      endif.
      if s_mes-low(2) ge '12'.
        add wg_faglflext-ksl12 to wl_saldo_mi2.
        add wg_faglflext-ksl13 to wl_saldo_mi2.
        add wg_faglflext-ksl14 to wl_saldo_mi2.
        add wg_faglflext-ksl15 to wl_saldo_mi2.
        add wg_faglflext-ksl16 to wl_saldo_mi2.
      endif.

      read table tg_t001 into wg_t001 with key bukrs = wg_faglflext-rbukrs binary search.

      if wl_update is initial.
        read table tg_skat into wg_skat with key saknr = wg_faglflext-racct binary search.
        read table tg_ska1 into wg_ska1 with key saknr = wg_faglflext-racct binary search.
        "IR56206 ALRS 13.03.2015
        read table it_saknr into wa_saknr with key saknr =  wg_faglflext-racct.
        if sy-subrc = 0.

          "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
          read table tg_0082 into wg_0082 with key saknr = wg_faglflext-racct
                                                   vbund = wg_faglflext-rassc binary search.
        else.
          read table tg_0082 into wg_0082 with key saknr = wg_faglflext-racct binary search.
        endif.
        if sy-subrc is initial.
          clear: wg_zib_chv.
          read table tg_zib_chv into wg_zib_chv with key obj_key = wg_0082-obj_key binary search.
          if sy-subrc is not initial.
            read table tg_zib_err into wg_zib_err with key obj_key = wg_0082-obj_key binary search.
          elseif wg_0082-obj_key_est is not initial.   " estorno
            clear: wg_zib_chv.
            read table tg_zib_chv  into wg_zib_chv with key obj_key = wg_0082-obj_key_est binary search.
            if sy-subrc is not initial.
              read table tg_zib_err  into wg_zib_err with key obj_key = wg_0082-obj_key_est binary search.
            else.
              wg_saidar-belnr_est = wg_zib_chv-belnr.
            endif.
          elseif wg_0082-obj_key_est is initial.
            wg_saidar-belnr = wg_zib_chv-belnr.
          endif.
        endif.
        wg_saidar-obj_key     = wg_0082-obj_key.
        wg_saidar-obj_key_est = wg_0082-obj_key_est.
        wg_saidar-racct       = wg_faglflext-racct.
        wg_saidar-txt50       = wg_skat-txt50.
        wg_saidar-ktoks       = wg_ska1-ktoks.
      endif.
      wg_saidar-curr1       = wl_saldo_mi.
      wg_saidar-curr2       = wl_saldo_mi2.

      case wg_t001-land1.
        when 'BR'.
          "ajuste   CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - BG US 100617
          "Recupera
          clear: wg_tcurr, wg_zfit0185.

          read table tg_zfit0185 into wg_zfit0185
            with key moeda_origem  = 'USD'
                     moeda_destino = 'BRL'.
          "BINARY SEARCH.


          if sy-subrc is initial.
            wg_saidar-tx_usd = wg_zfit0185-taxa.
            try.
                if wg_t001-bukrs eq '0101'.
                  wg_saidar-saldo_corr  = ( ( wg_saidar-curr1 * 100 ) / wg_saidar-tx_usd ).
                else.
                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                endif.
              catch cx_sy_zerodivide .
            endtry.
            wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
          else.

            read table tg_tcurr into wg_tcurr
              with key fcurr = 'USD'
                       tcurr = 'BRL'.
            "BINARY SEARCH.

            if sy-subrc is initial.
              wg_saidar-tx_usd = wg_tcurr-ukurs.
              try.
                  if wg_t001-bukrs eq '0101'.
                    wg_saidar-saldo_corr  = ( ( wg_saidar-curr1 * 100 ) / wg_saidar-tx_usd ).
                  else.
                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                  endif.
                catch cx_sy_zerodivide .
              endtry.
              wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
            endif.

          endif.


        when 'NL'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
          "Recupera
          clear: wg_tcurr_lday.

          read table tg_zfit0185_lday into wg_zfit0185_lday
            with key moeda_origem = 'USD'
                     moeda_destino = 'EUR'.
          "BINARY SEARCH.

          if sy-subrc is initial.
            wg_saidar-tx_usd = wg_zfit0185_lday-taxa.
            if wg_saidar-tx_usd lt 0.
              multiply wg_saidar-tx_usd by -1.
              try.
                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            else.
              try.
                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            endif.

          else.
            read table tg_tcurr_lday into wg_tcurr_lday
            with key fcurr = 'USD'
                     tcurr = 'EUR'.
            "BINARY SEARCH.
            if sy-subrc is initial.
              wg_saidar-tx_usd = wg_tcurr_lday-ukurs.
              if wg_saidar-tx_usd lt 0.
                multiply wg_saidar-tx_usd by -1.
                try.
                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
                  catch cx_sy_zerodivide .
                endtry.
              else.
                try.
                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                  catch cx_sy_zerodivide .
                endtry.
              endif.

            endif.
          endif.


          wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).


          "============================= INICIO DEVK9A0R37 - FI - [IR053183] Ajuste ZFI0063 - AOENNING
        when 'LU'.
*          CLEAR: wg_tcurr_lday.
*          READ TABLE tg_tcurr_lday INTO wg_tcurr_lday

*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
          clear: wg_tcurr, wg_zfit0185.

          read table tg_zfit0185 into wg_zfit0185
            with key moeda_origem = 'USD'
                     moeda_destino = 'EUR'.
          "BINARY SEARCH.

          if sy-subrc is initial.

            wg_saidar-tx_usd = wg_zfit0185-taxa.
            if wg_saidar-tx_usd lt 0.
              multiply wg_saidar-tx_usd by -1.
              try.
                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            else.
              try.
                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            endif.
            wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).

          else.

            read table tg_tcurr into wg_tcurr
            with key fcurr = 'USD'
                     tcurr = 'EUR'.
            "BINARY SEARCH.
            if sy-subrc is initial.
              wg_saidar-tx_usd = wg_tcurr-ukurs.
              if wg_saidar-tx_usd lt 0.
                multiply wg_saidar-tx_usd by -1.
                try.
                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
                  catch cx_sy_zerodivide .
                endtry.
              else.
                try.
                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                  catch cx_sy_zerodivide .
                endtry.
              endif.
              wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
            endif.

          endif.


          "============================= INICIO DEVK9A0R37 - FI - [IR053183] Ajuste ZFI0063 - AOENNING
        when 'CH'.
          "Recupera
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
          clear: wg_tcurr_lday, wg_zfit0185_lday.

          read table tg_zfit0185_lday into wg_zfit0185_lday
            with key moeda_origem = 'USD'
                     moeda_destino = 'CHF'.
          "BINARY SEARCH.

          if sy-subrc is initial.
            wg_saidar-tx_usd = wg_zfit0185_lday-taxa.
            if wg_saidar-tx_usd lt 0.
              multiply wg_saidar-tx_usd by -1.
              try.
                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            else.
              try.
                  wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            endif.
          else.
            read table tg_tcurr_lday into wg_tcurr_lday
            with key fcurr = 'USD'
                     tcurr = 'CHF'.
            "BINARY SEARCH.
            if sy-subrc is initial.
              wg_saidar-tx_usd = wg_tcurr_lday-ukurs.
              if wg_saidar-tx_usd lt 0.
                multiply wg_saidar-tx_usd by -1.
                try.
                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
                  catch cx_sy_zerodivide .
                endtry.
              else.
                try.
                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                  catch cx_sy_zerodivide .
                endtry.
              endif.
            endif.

          endif.
          wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
        when 'AR' or 'PY'.

**  Saldo Mi3 ****************************************************************************
          add wg_faglflext-oslvt to wl_saldo_mi3.
          if s_mes-low(2) ge '01'.
            add wg_faglflext-osl01 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '02'.
            add wg_faglflext-osl02 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '03'.
            add wg_faglflext-osl03 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '04'.
            add wg_faglflext-osl04 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '05'.
            add wg_faglflext-osl05 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '06'.
            add wg_faglflext-osl06 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '07'.
            add wg_faglflext-osl07 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '08'.
            add wg_faglflext-osl08 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '09'.
            add wg_faglflext-osl09 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '10'.
            add wg_faglflext-osl10 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '11'.
            add wg_faglflext-osl11 to wl_saldo_mi3.
          endif.
          if s_mes-low(2) ge '12'.
            add wg_faglflext-osl12 to wl_saldo_mi3.
            add wg_faglflext-osl13 to wl_saldo_mi3.
            add wg_faglflext-osl14 to wl_saldo_mi3.
            add wg_faglflext-osl15 to wl_saldo_mi3.
            add wg_faglflext-osl16 to wl_saldo_mi3.
          endif.


          wg_saidar-curr3 = wl_saldo_mi3.
          if wg_faglflext-rbukrs eq '0100'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
            clear: wg_tcurr, wg_zfit0185.

            read table tg_zfit0185 into wg_zfit0185
            with key moeda_origem = 'USD'
                     moeda_destino = 'ARS'.
            "BINARY SEARCH.

            if sy-subrc is initial.
              wg_saidar-tx_usd = wg_zfit0185-taxa.
              try.
                  wg_saidar-saldo_corr = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                catch cx_sy_zerodivide.
              endtry.
              wg_saidar-vlr_ajust = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).

            else.
              read table tg_tcurr into wg_tcurr
              with key fcurr = 'USD'
                       tcurr = 'ARS'.
              "BINARY SEARCH.
              if sy-subrc is initial.
                wg_saidar-tx_usd = wg_tcurr-ukurs.
                try.
                    wg_saidar-saldo_corr = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                  catch cx_sy_zerodivide.
                endtry.
                wg_saidar-vlr_ajust = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
              endif.

            endif.


            clear: wg_tcurr, wg_zfit0185.

            read table tg_zfit0185 into wg_zfit0185
              with key moeda_origem = 'BRL'
                       moeda_destino = 'ARS'.
            "BINARY SEARCH.
            if sy-subrc is initial.
              wg_saidar-tx_brl      = wg_zfit0185-taxa.
              try.
                  wg_saidar-saldo_corr2 = ( wg_saidar-curr1 / wg_saidar-tx_brl ).
                catch cx_sy_zerodivide.
              endtry.
              wg_saidar-vlr_ajust2 = ( wg_saidar-saldo_corr2 - wg_saidar-curr3 ).
            else.
              read table tg_tcurr into wg_tcurr
              with key fcurr = 'BRL'
                       tcurr = 'ARS'.
              "BINARY SEARCH.
              if sy-subrc is initial.
                wg_saidar-tx_brl      = wg_tcurr-ukurs.
                try.
                    wg_saidar-saldo_corr2 = ( wg_saidar-curr1 / wg_saidar-tx_brl ).
                  catch cx_sy_zerodivide.
                endtry.
                wg_saidar-vlr_ajust2 = ( wg_saidar-saldo_corr2 - wg_saidar-curr3 ).
              endif.
            endif.

          elseif wg_faglflext-rbukrs eq '0101'.

            clear: wg_tcurr, wg_zfit0185.
            read table tg_tcurr into wg_tcurr
              with key fcurr = 'USD'
                       tcurr = 'PYG'.
            "BINARY SEARCH.
            if sy-subrc is initial.
              wg_saidar-tx_usd = wg_tcurr-ukurs.
              try.
                  if wg_t001-bukrs eq '0101'.
                    wg_saidar-saldo_corr  = ( ( wg_saidar-curr1 * 100 ) / wg_saidar-tx_usd ).
                  else.
                    wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
                  endif.
                catch cx_sy_zerodivide.
              endtry.
              wg_saidar-vlr_ajust = ( wg_saidar-saldo_corr - wg_saidar-curr2 ).
            endif.

            clear: wg_tcurr, wg_zfit0185.

            read table tg_zfit0185 into wg_zfit0185
              with key moeda_origem = 'BRL'
                       moeda_destino = 'PYG'.
            "BINARY SEARCH.
            if sy-subrc is initial.

              wg_saidar-tx_brl = wg_zfit0185-taxa.
              try.
                  wg_saidar-saldo_corr2 = ( ( wg_saidar-curr1 * 100 )  / wg_saidar-tx_brl ).
                catch cx_sy_zerodivide.
              endtry.
              wg_saidar-vlr_ajust2 = ( wg_saidar-saldo_corr2 - wg_saidar-curr3 ).

            else.
              read table tg_tcurr into wg_tcurr
              with key fcurr = 'BRL'
                       tcurr = 'PYG'.
              "BINARY SEARCH.
              if sy-subrc is initial.
                wg_saidar-tx_brl = wg_tcurr-ukurs.
                try.
                    wg_saidar-saldo_corr2 = ( ( wg_saidar-curr1 * 100 )  / wg_saidar-tx_brl ).
                  catch cx_sy_zerodivide.
                endtry.
                wg_saidar-vlr_ajust2 = ( wg_saidar-saldo_corr2 - wg_saidar-curr3 ).
              endif.
            endif.
          endif.
      endcase.

      loop at tg_0081 into wg_0081.
        if wg_0081-conta_de is not initial.
          if wg_faglflext-racct eq wg_0081-conta_de.
            wl_flag = c_x.
            exit.
          endif.
        endif.
        clear:wg_0081.
      endloop.

      if wg_0081-cta_monet ne 'S'.
        continue.
      endif.

      "IR56206 ALRS 13.03.2015
      "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
      read table it_saknr into wa_saknr with key saknr =  wg_faglflext-racct.
      if sy-subrc = 0.

        read table tg_0082 into wg_0082
         with key saknr = wg_faglflext-racct
                  vbund = wg_faglflext-rassc binary search.
      else.
        read table tg_0082 into wg_0082
          with key saknr = wg_faglflext-racct binary search.
      endif.
      " Se for estorno recupera valor de ajuste
      if sy-subrc = 0.
        if wg_0082-obj_key_est is initial and wg_0082-obj_key is not initial.
          wg_saidar-vlr_ajust  = wg_0082-vlr_corr_mi2.
          wg_saidar-vlr_ajust2 = wg_0082-vlr_corr_mi3.
        endif.
      endif.

      "IR56206 ALRS 13.03.2015
      "IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
      read table it_saknr into wa_saknr with key saknr =  wg_faglflext-racct.
      if sy-subrc = 0.

        wg_saidar-rassc = wg_faglflext-rassc.
      endif.


      if wl_flag is not initial.
        if wl_update is initial.
          append wg_saidar to tg_saidar.
        else.
          modify tg_saidar from wg_saidar index wl_update transporting curr1 curr2 curr3 tx_usd tx_brl saldo_corr saldo_corr2 vlr_ajust vlr_ajust2.
        endif.
      endif.
      clear: wg_saidar, wg_t001, wg_ska1, wg_skat, wg_0082, wl_flag.
    endloop.

    "10.09.2015 ALRS
    if s_bukrs-low eq '0101'.
      loop at  tg_saidar into wg_saidar.
        multiply wg_saidar-curr1 by 100.
        modify tg_saidar from wg_saidar index sy-tabix transporting curr1.
      endloop.
    endif.

    "**********************************************************************************************
    " Ajusta Saida de Documento Processado """"""""""""""""""""""""""""""""""""""""""""""""""""""""
    loop at tg_0082 into wg_0082.
      "IR56206 ALRS 13.03.2015
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wg_0082-saknr
        importing
          output = vg_racct.
      "IF '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
      read table it_saknr into wa_saknr with key saknr = wg_0082-saknr.
      if sy-subrc = 0.

        read table tg_saidar into wl_saidar with key racct = wg_0082-saknr
                                                   rassc = wg_0082-vbund.
      else.
        read table tg_saidar into wl_saidar with key racct = wg_0082-saknr.
      endif.
      if sy-subrc is not initial.
        read table tg_skat into wg_skat with key saknr = wg_0082-saknr binary search.
        read table tg_ska1 into wg_ska1 with key saknr = wg_0082-saknr binary search.
        wl_saidar-racct        = wg_0082-saknr.
        "IR56206 ALRS 13.03.2015
        wl_saidar-rassc        = wg_0082-vbund.
        wl_saidar-txt50        = wg_skat-txt50.
        wl_saidar-ktoks        = wg_ska1-ktoks.

        wl_saidar-curr1        = wg_0082-dmbtr.


        wl_saidar-curr2        = wg_0082-dmbe2.
        wl_saidar-curr3        = wg_0082-dmbe3.
        wl_saidar-tx_usd       = wg_0082-tx_usd.
        wl_saidar-tx_brl       = wg_0082-tx_brl.
        wl_saidar-saldo_corr   = wg_0082-sdo_corr_mi2.
        wl_saidar-saldo_corr2  = wg_0082-sdo_corr_mi3.
        wl_saidar-vlr_ajust    = wg_0082-vlr_corr_mi2.
        wl_saidar-vlr_ajust2   = wg_0082-vlr_corr_mi3.
        wl_saidar-obj_key      = wg_0082-obj_key.
        wl_saidar-obj_key_est  = wg_0082-obj_key_est.
        read table tg_zib_chv into wg_zib_chv with key obj_key = wg_0082-obj_key binary search.
        if sy-subrc is initial.
          wl_saidar-belnr = wg_zib_chv-belnr.
        endif.
        append wl_saidar to tg_saidar.
      endif.
    endloop.
    "**********************************************************************************************

    sort tg_saidar by racct.


    perform atualiza_saidar tables tg_saidar
                                   tg_zib
                                   tg_zib_chv
                                   tg_zib_err.

    if tg_saidar[] is initial.
      message s836(sd) display like 'E' with text-015
                                             text-016.
      stop.
    endif.

** Visualização
  else.
    sort: tg_skat    by saknr,
          tg_ska1    by saknr,
          tg_t001    by bukrs,
          tg_zib_chv by obj_key,
          tg_zib_err by obj_key.

    loop at tg_0082 into wg_0082.
      clear: wl_flag.

      read table tg_skat into wg_skat with key saknr = wg_0082-saknr binary search.
      read table tg_ska1 into wg_ska1 with key saknr = wg_0082-saknr binary search.

      wg_saidar-racct        = wg_0082-saknr.
      wg_saidar-txt50        = wg_skat-txt50.
      wg_saidar-ktoks        = wg_ska1-ktoks.
      wg_saidar-curr1        = wg_0082-dmbtr.
      wg_saidar-curr2        = wg_0082-dmbe2.
      wg_saidar-curr3        = wg_0082-dmbe3.
      wg_saidar-tx_usd       = wg_0082-tx_usd.
      wg_saidar-saldo_corr   = wg_0082-sdo_corr_mi2.
      wg_saidar-vlr_ajust    = wg_0082-vlr_corr_mi2.
      wg_saidar-tx_brl       = wg_0082-tx_brl.
      wg_saidar-saldo_corr2  = wg_0082-sdo_corr_mi3.
      wg_saidar-vlr_ajust2   = wg_0082-vlr_corr_mi3.
      wg_saidar-obj_key      = wg_0082-obj_key.
      wg_saidar-obj_key_est  = wg_0082-obj_key_est.

      loop at tg_0081 into wg_0081.

        if wg_0081-conta_de is not initial.
          if wg_saidar-racct eq wg_0081-conta_de.
            wl_flag = c_x.
            exit.
          endif.
        endif.
        clear:wg_0081.
      endloop.
      if wl_flag is not initial.
        append wg_saidar to tg_saidar.
      endif.
      clear: wg_saidar, wg_ska1, wg_skat, wl_flag.
    endloop.

    perform atualiza_saidar tables tg_saidar
                                  tg_zib
                                  tg_zib_chv
                                  tg_zib_err.


  endif.



** Processamento
  if p_proc is not initial.
    if s_bukrs-low eq '0101'.
*      LOOP AT  TG_SAIDA INTO WG_SAIDA.
*        MULTIPLY WG_SAIDA-CURR1 BY 100.
*        MODIFY TG_SAIDA FROM WG_SAIDA INDEX SY-TABIX TRANSPORTING CURR1.
*      ENDLOOP.
    elseif s_bukrs-low eq '0004' or s_bukrs-low eq '0037'. "recalcula ajuste
      loop at  tg_saidar into wg_saidar.
        tabix = sy-tabix.
        clear wg_0082.
        read table tg_0082 into wg_0082
              with key saknr = wg_saidar-racct  binary search.
        " Se for estorno recupera valor de ajuste
        if sy-subrc ne 0
          or  ( wg_0082-obj_key_est is not initial or  wg_0082-obj_key is not initial ).
          wg_saidar-saldo_corr = wg_saidar-curr2 * wg_saidar-tx_usd.
          wg_saidar-vlr_ajust  = wg_saidar-saldo_corr - wg_saidar-curr1.
          modify tg_saidar from wg_saidar index tabix transporting vlr_ajust saldo_corr.
        endif.
      endloop.
    endif.
  endif.

endform.                    " ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form organizacao_dados .
  data: wl_saida  like wg_saida,
        wl_update type sy-tabix.


  sort: tg_skat     by saknr,
        tg_ska1     by saknr,
        tg_tcurr    by fcurr tcurr,
        tg_zfit0185 by moeda_origem moeda_destino,
        tg_t001     by bukrs,
        tg_t882g    by rbukrs,
        tg_0084     by belnr buzei,
        tg_lfa1     by lifnr,
        tg_kna1     by kunnr,
        tg_zib      by obj_key,
        tg_zib_chv  by obj_key,
        tg_zib_err  by obj_key.

  if p_visu eq 'X'.
    sort: tg_0084 by saknr,
          tg_bsik_bsid by bukrs belnr buzei.
    refresh tg_saida.
    loop at tg_0084 into wg_0084.
      clear:  wl_saida.
      wg_saida-obj_key         = wg_0084-obj_key.

      read table tg_bsik_bsid into wg_bsik_bsid_aux with key bukrs = wg_0084-bukrs
                                                             belnr = wg_0084-belnr
                                                             buzei = wg_0084-buzei binary search.
      if sy-subrc ne 0.
        continue.
      endif.

      read table tg_t001 into wg_t001
             with key bukrs = wg_0084-bukrs
                      binary search.

      read table tg_skat into wg_skat
        with key saknr = wg_0084-saknr
                 binary search.
      if sy-subrc = 0.
        wg_saida-txt50       = wg_skat-txt50.
      endif.
      wg_saida-hkont       = wg_0084-saknr.
      wg_saida-kunnr       = wg_0084-lifnr.

      read table tg_ska1 into wg_ska1
        with key saknr = wg_0084-saknr
                 binary search.

      clear: wg_zib_chv.
      if wg_0084-obj_key is not initial.
        read table tg_zib_chv  into wg_zib_chv
          with key obj_key = wg_0084-obj_key
                     binary search.
        if sy-subrc = 0.
          wg_saida-belnr       = wg_zib_chv-belnr.
        endif.
      endif.

      if p_forn is not initial.
        read table tg_lfa1 into wg_lfa1
          with key lifnr = wg_0084-lifnr
                   binary search.

        wg_saida-name1       = wg_lfa1-name1.
      else.
        read table tg_kna1 into wg_kna1
         with key kunnr = wg_0084-lifnr
                  binary search.

        wg_saida-name1       = wg_kna1-name1.
      endif.

      wg_saida-bschl       = wg_bsik_bsid_aux-bschl.
      wg_saida-belnr2      = wg_bsik_bsid_aux-belnr.
      wg_saida-buzei       = wg_bsik_bsid_aux-buzei.
      wg_saida-gjahr       = wg_bsik_bsid_aux-gjahr.
      wg_saida-waers       = wg_bsik_bsid_aux-waers.
      wg_saida-budat       = wg_bsik_bsid_aux-budat.
      wg_saida-blart       = wg_bsik_bsid_aux-blart.
      wg_saida-xblnr       = wg_bsik_bsid_aux-xblnr.
      wg_saida-doc         = wg_bsik_bsid_aux-doc.
      wg_saida-item        = wg_bsik_bsid_aux-item.

      wg_saida-umskz       = wg_bsik_bsid_aux-umskz.
      "calculos
      wg_saida-dmbtr       = wg_0084-dmbtr.
      wg_saida-dmbe2       = wg_0084-dmbe2.
      wg_saida-dmbe3       = wg_0084-dmbe3.
      wg_saida-vlr_ajust   = wg_0084-vlr_corr_mi2.
      wg_saida-vlr_ajust2  = wg_0084-vlr_corr_mi3.
      wg_saida-saldo_corr  = wg_0084-sdo_corr_mi2.


      loop at tg_0081 into wg_0081.
        if wg_0081-conta_de is not initial.
          if wg_saida-hkont eq wg_0081-conta_de.
            exit.
          endif.
        endif.
        clear:wg_0081.
      endloop.

      if wg_0081 is not initial .
        if wg_t001-land1 eq 'AR' or wg_t001-land1 eq 'PY'.
          v_saknr      =  wg_0081-saknr.
        elseif wg_0081-saknr is not initial.
          v_saknr      =  wg_0081-saknr.
        elseif wg_0081-hkont is not initial.
          v_saknr      =  wg_0081-hkont.
        endif.

        clear v_vbund.
        read table t_hkont with key from = v_saknr.
        if sy-subrc = 0.
          if p_forn = 'X'.
            select single vbund into v_vbund from lfa1 where lifnr = wg_saida-kunnr.
          else.
            select single vbund into v_vbund from kna1 where kunnr = wg_saida-kunnr.
          endif.
        endif.
        wg_saida-vbund = v_vbund.

        clear v_bewar.
        select single bewar
          into v_bewar
          from zfit0030
         where hkont eq v_saknr
           and cond  eq ''.
        wg_saida-bewar      = v_bewar.
      endif.

      append wg_saida to tg_saida.
      clear: wg_saida, wg_t001, wg_ska1, wg_skat, wg_0084.
    endloop.


    perform atualiza_saida tables tg_saida
                                  tg_zib
                                  tg_zib_chv
                                  tg_zib_err.

    exit. "visualização

  endif.

  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = 'CONTAS_EC-CS'
    tables
      set_values    = t_hkont
    exceptions
      set_not_found = 1
      others        = 2.

  refresh: tg_bsik_bsid_aux.
  loop at tg_bsik_bsid into wg_bsik_bsid_aux.
    if wg_bsik_bsid_aux-shkzg eq 'H'.
      multiply wg_bsik_bsid_aux-dmbtr by -1.
      multiply wg_bsik_bsid_aux-dmbe2 by -1.
      multiply wg_bsik_bsid_aux-dmbe3 by -1.
    endif.

    append  wg_bsik_bsid_aux to tg_bsik_bsid_aux.
    clear: wg_bsik_bsid_aux.
  endloop.

*---> 05/07/2023 - Migração S4 - DL
  sort tg_tcurr_lday by kurst fcurr.
  sort tg_t001 by bukrs.
  sort tg_skat by saknr.
  sort tg_ska1 by saknr.
  sort tg_0084 by belnr buzei.
  sort tg_zib_chv by obj_key.
  sort tg_lfa1 by lifnr.
  sort tg_kna1 by kunnr.
  sort tg_zfit0185 by moeda_origem moeda_destino.
  sort tg_tcurr by fcurr tcurr.
  sort tg_zfit0185_lday by moeda_origem moeda_destino.
  sort tg_0084 by belnr buzei.
*<--- 05/07/2023 - Migração S4 - DL


  loop at tg_bsik_bsid_aux into wg_bsik_bsid_aux.
    clear:  wl_saida.

    read table tg_t001 into wg_t001
           with key bukrs = wg_bsik_bsid_aux-bukrs
                    binary search.

    read table tg_skat into wg_skat
      with key saknr = wg_bsik_bsid_aux-hkont
               binary search.
    if sy-subrc = 0.
      wg_saida-txt50       = wg_skat-txt50.
    endif.
    wg_saida-hkont       = wg_bsik_bsid_aux-hkont.
    wg_saida-kunnr       = wg_bsik_bsid_aux-lifnr.

    read table tg_ska1 into wg_ska1
      with key saknr = wg_bsik_bsid_aux-hkont
               binary search.

    clear wg_0084.
    " Testar com BUZEI ANTES
    read table tg_0084 into wg_0084
    with key belnr = wg_bsik_bsid_aux-belnr
             buzei = wg_bsik_bsid_aux-buzei binary search.
    if sy-subrc ne 0.
      read table tg_0084 into wg_0084
      with key belnr = wg_bsik_bsid_aux-belnr binary search.
      if wg_0084-buzei is not initial.
        sy-subrc = 4.
      endif.
    endif.

    if sy-subrc is initial.
      wg_saida-obj_key         = wg_0084-obj_key.
      "Documento da Variação
      clear: wg_zib_chv.
      if wg_0084-obj_key is not initial.
        read table tg_zib_chv  into wg_zib_chv
          with key obj_key = wg_0084-obj_key
                     binary search.
        if sy-subrc = 0.
          wg_saida-belnr       = wg_zib_chv-belnr.
        endif.
      endif.

    endif.

    if p_forn is not initial.
      read table tg_lfa1 into wg_lfa1
        with key lifnr = wg_bsik_bsid_aux-lifnr
                 binary search.

      wg_saida-name1       = wg_lfa1-name1.
    else.
      read table tg_kna1 into wg_kna1
       with key kunnr = wg_bsik_bsid_aux-lifnr
                binary search.

      wg_saida-name1       = wg_kna1-name1.
    endif.

    wg_saida-bschl       = wg_bsik_bsid_aux-bschl.
    wg_saida-belnr2      = wg_bsik_bsid_aux-belnr.
    wg_saida-buzei       = wg_bsik_bsid_aux-buzei.
    wg_saida-gjahr       = wg_bsik_bsid_aux-gjahr.
    wg_saida-waers       = wg_bsik_bsid_aux-waers.
    wg_saida-budat       = wg_bsik_bsid_aux-budat.
    wg_saida-blart       = wg_bsik_bsid_aux-blart.
    wg_saida-xblnr       = wg_bsik_bsid_aux-xblnr.
    wg_saida-doc         = wg_bsik_bsid_aux-doc.
    wg_saida-item        = wg_bsik_bsid_aux-item.
    wg_saida-dmbtr       = wg_bsik_bsid_aux-dmbtr.
    wg_saida-dmbe2       = wg_bsik_bsid_aux-dmbe2.
    wg_saida-umskz       = wg_bsik_bsid_aux-umskz.

    case wg_t001-land1.
      when 'BR'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
        clear: wg_tcurr, wg_zfit0185.

        read table tg_zfit0185 into wg_zfit0185
         with key moeda_origem = 'USD'
                  moeda_destino = 'BRL'.
        "BINARY SEARCH.

        if sy-subrc is initial.
          wg_saida-tx_usd = wg_zfit0185-taxa.
          try.
              wg_saida-saldo_corr  = ( wg_saida-dmbtr / wg_saida-tx_usd ).
            catch cx_sy_zerodivide .
          endtry.
          wg_saida-vlr_ajust   = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
        else.
          read table tg_tcurr into wg_tcurr
          with key fcurr = 'USD'
                   tcurr = 'BRL'.
          "BINARY SEARCH.
          if sy-subrc is initial.
            wg_saida-tx_usd = wg_tcurr-ukurs.
            try.
                wg_saida-saldo_corr  = ( wg_saida-dmbtr / wg_saida-tx_usd ).
              catch cx_sy_zerodivide .
            endtry.
            wg_saida-vlr_ajust   = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
          endif.
        endif.



      when 'NL'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
        clear: wg_tcurr_lday, wg_zfit0185_lday.

        read table tg_zfit0185_lday into wg_zfit0185_lday
         with key  moeda_origem = 'USD'
                   moeda_destino = 'EUR'.
        "BINARY SEARCH.
        if sy-subrc is initial.


          wg_saida-tx_usd = wg_zfit0185_lday-taxa.
          if wg_saida-tx_usd lt 0.
            multiply wg_saida-tx_usd by -1.
            try.
                wg_saida-saldo_corr  = ( wg_saida-dmbtr * wg_saida-tx_usd ).
              catch cx_sy_zerodivide .
            endtry.
          else.
            try.
                wg_saida-saldo_corr  = ( wg_saida-dmbtr / wg_saida-tx_usd ).
              catch cx_sy_zerodivide .
            endtry.
          endif.
          wg_saida-vlr_ajust   = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).


        else.

          read table tg_tcurr_lday into wg_tcurr_lday
          with key fcurr = 'USD'
                   tcurr = 'EUR'.
          "BINARY SEARCH.
          if sy-subrc is initial.
            wg_saida-tx_usd = wg_tcurr_lday-ukurs.
            if wg_saida-tx_usd lt 0.
              multiply wg_saida-tx_usd by -1.
              try.
                  wg_saida-saldo_corr  = ( wg_saida-dmbtr * wg_saida-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            else.
              try.
                  wg_saida-saldo_corr  = ( wg_saida-dmbtr / wg_saida-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            endif.
            wg_saida-vlr_ajust   = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
          endif.
        endif.

        "============================= INICIO DEVK9A0R37 - FI - [IR053183] Ajuste ZFI0063 - AOENNING
      when 'LU'.
*        CLEAR: wg_tcurr_lday.
*        READ TABLE tg_tcurr_lday INTO wg_tcurr_lday
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
        clear: wg_tcurr, wg_zfit0185.
        read table tg_zfit0185 into wg_zfit0185
        with key moeda_origem = 'USD'
                 moeda_destino = 'EUR'.
        "BINARY SEARCH.
        if sy-subrc is initial.
          wg_saida-tx_usd = wg_zfit0185-taxa.
          if wg_saida-tx_usd lt 0.
            multiply wg_saida-tx_usd by -1.
            try.
                wg_saida-saldo_corr  = ( wg_saida-dmbtr * wg_saida-tx_usd ).
              catch cx_sy_zerodivide .
            endtry.
          else.
            try.
                wg_saida-saldo_corr  = ( wg_saida-dmbtr / wg_saida-tx_usd ).
              catch cx_sy_zerodivide .
            endtry.
          endif.
          wg_saida-vlr_ajust   = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
        else.
          read table tg_tcurr into wg_tcurr
          with key fcurr = 'USD'
                   tcurr = 'EUR'.
          "BINARY SEARCH.
          if sy-subrc is initial.
            wg_saida-tx_usd = wg_tcurr-ukurs.
            if wg_saida-tx_usd lt 0.
              multiply wg_saida-tx_usd by -1.
              try.
                  wg_saida-saldo_corr  = ( wg_saida-dmbtr * wg_saida-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            else.
              try.
                  wg_saida-saldo_corr  = ( wg_saida-dmbtr / wg_saida-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            endif.
            wg_saida-vlr_ajust   = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
          endif.
        endif.



        "=============================FIM DEVK9A0R37 - FI - [IR053183] Ajuste ZFI0063 - AOENNING
      when 'CH'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
        clear: wg_tcurr_lday, wg_zfit0185_lday .

        read table tg_zfit0185_lday into wg_zfit0185_lday
        with key moeda_origem = 'USD'
                 moeda_destino = 'CHF'.
        "BINARY SEARCH.
        if sy-subrc is initial.
          wg_saida-tx_usd = wg_zfit0185-taxa.
          if wg_saida-tx_usd lt 0.
            multiply wg_saida-tx_usd by -1.
            try.
                wg_saida-saldo_corr  = ( wg_saida-dmbtr * wg_saida-tx_usd ).
              catch cx_sy_zerodivide .
            endtry.
          else.
            try.
                wg_saida-saldo_corr  = ( wg_saida-dmbtr / wg_saida-tx_usd ).
              catch cx_sy_zerodivide .
            endtry.
          endif.
          wg_saida-vlr_ajust   = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
        else.
          read table tg_tcurr_lday into wg_tcurr_lday
          with key fcurr = 'USD'
                   tcurr = 'CHF'.
          "BINARY SEARCH.
          if sy-subrc is initial.
            wg_saida-tx_usd = wg_tcurr_lday-ukurs.
            if wg_saida-tx_usd lt 0.
              multiply wg_saida-tx_usd by -1.
              try.
                  wg_saida-saldo_corr  = ( wg_saida-dmbtr * wg_saida-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            else.
              try.
                  wg_saida-saldo_corr  = ( wg_saida-dmbtr / wg_saida-tx_usd ).
                catch cx_sy_zerodivide .
              endtry.
            endif.
            wg_saida-vlr_ajust   = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
          endif.
        endif.


      when 'AR' or 'PY'.
        wg_saida-dmbe3 = wg_bsik_bsid_aux-dmbe3.
        if wg_bsik_bsid_aux-bukrs eq '0100'.
*ajuste CS2023000007 transação p/ cadastrar taxa de fechamento para a empresa 0203-Luxembourg - US 100617  - BG
          clear: wg_tcurr, wg_zfit0185.

          read table tg_zfit0185 into wg_zfit0185
            with key moeda_origem = 'USD'
                     moeda_destino = 'ARS'.
          "BINARY SEARCH.
          if sy-subrc is initial.
            wg_saida-tx_usd = wg_zfit0185-taxa.
            try.
                wg_saida-saldo_corr = ( wg_saida-dmbtr / wg_saida-tx_usd ).
              catch cx_sy_zerodivide.
            endtry.
            wg_saida-vlr_ajust = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
          else.
            read table tg_tcurr into wg_tcurr
            with key fcurr = 'USD'
                     tcurr = 'ARS'.
            "BINARY SEARCH.
            if sy-subrc is initial.
              wg_saida-tx_usd = wg_tcurr-ukurs.
              try.
                  wg_saida-saldo_corr = ( wg_saida-dmbtr / wg_saida-tx_usd ).
                catch cx_sy_zerodivide.
              endtry.
              wg_saida-vlr_ajust = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
            endif.
          endif.

          clear: wg_tcurr, wg_zfit0185.


          read table tg_zfit0185 into wg_zfit0185
            with key moeda_origem = 'BRL'
                     moeda_destino = 'ARS'
                     binary search.
          if sy-subrc is initial.
            wg_saida-tx_brl      = wg_zfit0185-taxa.
            try.
                wg_saida-saldo_corr2 = ( wg_saida-dmbtr / wg_saida-tx_brl ).
              catch cx_sy_zerodivide.
            endtry.
            wg_saida-vlr_ajust2 = ( wg_saida-saldo_corr2 - wg_saida-dmbe3 ).
          else.
            read table tg_tcurr into wg_tcurr
            with key fcurr = 'BRL'
                     tcurr = 'ARS'
                     binary search.
            if sy-subrc is initial.
              wg_saida-tx_brl      = wg_tcurr-ukurs.
              try.
                  wg_saida-saldo_corr2 = ( wg_saida-dmbtr / wg_saida-tx_brl ).
                catch cx_sy_zerodivide.
              endtry.
              wg_saida-vlr_ajust2 = ( wg_saida-saldo_corr2 - wg_saida-dmbe3 ).
            endif.
          endif.

        elseif wg_bsik_bsid_aux-bukrs eq '0101'.
          clear: wg_tcurr, wg_zfit0185.
          multiply wg_saida-dmbtr by 100. "Ajusta decimal Paraguay

          read table tg_zfit0185 into wg_zfit0185
          with key moeda_origem = 'USD'
                   moeda_destino = 'PYG'.
          "BINARY SEARCH.
          if sy-subrc is initial.
            wg_saida-tx_usd = wg_zfit0185-taxa.
            try.
                wg_saida-saldo_corr = ( wg_saida-dmbtr / wg_saida-tx_usd ).
              catch cx_sy_zerodivide.
            endtry.
            wg_saida-vlr_ajust = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
          else.
            read table tg_tcurr into wg_tcurr
            with key fcurr = 'USD'
                     tcurr = 'PYG'.
            "BINARY SEARCH.
            if sy-subrc is initial.
              wg_saida-tx_usd = wg_tcurr-ukurs.
              try.
                  wg_saida-saldo_corr = ( wg_saida-dmbtr / wg_saida-tx_usd ).
                catch cx_sy_zerodivide.
              endtry.
              wg_saida-vlr_ajust = ( wg_saida-saldo_corr - wg_saida-dmbe2 ).
            endif.
          endif.



          clear: wg_tcurr, wg_zfit0185.

          read table tg_zfit0185 into wg_zfit0185
            with key moeda_origem = 'BRL'
                     moeda_destino = 'PYG'
                     binary search.
          if sy-subrc is initial.
            wg_saida-tx_brl = wg_zfit0185-taxa.
            try.
                wg_saida-saldo_corr2 = ( wg_saida-dmbtr / wg_saida-tx_brl ).
              catch cx_sy_zerodivide.
            endtry.
            wg_saida-vlr_ajust2 = ( wg_saida-saldo_corr2 - wg_saida-dmbe3 ).
          else.
            read table tg_tcurr into wg_tcurr
            with key fcurr = 'BRL'
                     tcurr = 'PYG'
                     binary search.
            if sy-subrc is initial.
              wg_saida-tx_brl = wg_tcurr-ukurs.
              try.
                  wg_saida-saldo_corr2 = ( wg_saida-dmbtr / wg_saida-tx_brl ).
                catch cx_sy_zerodivide.
              endtry.
              wg_saida-vlr_ajust2 = ( wg_saida-saldo_corr2 - wg_saida-dmbe3 ).
            endif.
          endif.

        endif.
    endcase.

    read table tg_0084 into wg_0084
    with key belnr = wg_bsik_bsid_aux-belnr
             buzei = wg_bsik_bsid_aux-buzei binary search.

    if sy-subrc ne 0.
      read table tg_0084 into wg_0084
      with key belnr = wg_bsik_bsid_aux-belnr binary search.
      if wg_0084-buzei is not initial.
        sy-subrc = 4.
      endif.
    endif.

    " Se for estorno recupera valor de ajuste
    if sy-subrc = 0.
      if wg_0084-obj_key_est is initial and wg_0084-obj_key is not initial.
        wg_saida-vlr_ajust  = wg_0084-vlr_corr_mi2.
        wg_saida-vlr_ajust2 = wg_0084-vlr_corr_mi3.
      endif.
    endif.

    loop at tg_0081 into wg_0081.
      if wg_0081-conta_de is not initial.
        if wg_saida-hkont eq wg_0081-conta_de.
          exit.
        endif.
      endif.
      clear:wg_0081.
    endloop.

    if wg_0081 is not initial .
      if wg_t001-land1 eq 'AR' or wg_t001-land1 eq 'PY'.
        v_saknr      =  wg_0081-saknr.
      elseif wg_0081-saknr is not initial.
        v_saknr      =  wg_0081-saknr.
      elseif wg_0081-hkont is not initial.
        v_saknr      =  wg_0081-hkont.
      endif.

      clear v_vbund.
      read table t_hkont with key from = v_saknr.
      if sy-subrc = 0.
        if p_forn = 'X'.
          select single vbund into v_vbund from lfa1 where lifnr = wg_saida-kunnr.
        else.
          select single vbund into v_vbund from kna1 where kunnr = wg_saida-kunnr.
        endif.
      endif.
      wg_saida-vbund = v_vbund.

      clear v_bewar.
      select single bewar
        into v_bewar
        from zfit0030
       where hkont eq v_saknr
         and cond  eq ''.
      wg_saida-bewar      = v_bewar.
    endif.


    append wg_saida to tg_saida.
    clear: wg_saida, wg_t001, wg_ska1, wg_skat, wg_0084.
  endloop.

  if s_bukrs-low eq '0004' or s_bukrs-low eq '0037'. " recalcula ajuste
    loop at  tg_saida into wg_saida.
      wg_saida-saldo_corr = wg_saida-dmbe2 * wg_saida-tx_usd.
      wg_saida-vlr_ajust  = wg_saida-saldo_corr - wg_saida-dmbtr.
      modify tg_saida from wg_saida index sy-tabix transporting vlr_ajust saldo_corr.
    endloop.
  endif.

  perform atualiza_saida tables tg_saida
                                tg_zib
                                tg_zib_chv
                                tg_zib_err.

endform.                    " ORGANIZACAO_DADOS

form gera_contabilr  using   wl_saida type ty_saidar
                             wl_estorno
                   changing  p_obj_key type zib_contabil_chv-obj_key.

  data: wl_input_zib  type zib_contabil,
        tl_input_zib  type table of zib_contabil,
        wl_obj_key(9),
        v_saknr       type zfit0081-saknr,
        v_bewar       type bseg-bewar,
        vanomes(7).

  concatenate s_mes-low(2) '/' s_mes-low+2(4) into vanomes.

  call function 'Z_CONTROLE_FECHAMES'
    exporting
      i_bukrs  = s_bukrs-low
      i_data   = vg_last_day
    importing
      e_status = e_status
      e_messa  = e_messa
    exceptions
      error    = 1
      others   = 2.
  if e_status = 'E'.
    message e398(00) with e_messa.
    exit.
  endif.

  refresh: tl_input_zib.
  clear: wl_input_zib, wl_input_0082.
  read table tg_t001 into wg_t001
      with key bukrs =  s_bukrs-low
               binary search.

  read table tg_t882g into wg_t882g
    with key rbukrs =  s_bukrs-low
             binary search.

  loop at tg_0081 into wg_0081.
    if wg_0081-conta_de is not initial.
      if wl_saida-racct eq wg_0081-conta_de.
        exit.
      endif.
    endif.
    clear:wg_0081.
  endloop.
  if wg_0081 is not initial .
    if wg_t001-land1 eq 'AR' or wg_t001-land1 eq 'PY'.
      v_saknr      =  wg_0081-saknr.
    elseif wg_0081-saknr is not initial.
      v_saknr      =  wg_0081-saknr.
    elseif wg_0081-hkont is not initial.
      v_saknr      =  wg_0081-hkont.
    endif.

    if p_obj_key is initial.
      perform get_next_number  using  'Z_SALDOMO2'
                                      '01'
                             changing wl_obj_key.
    else.
      wl_obj_key = p_obj_key+7(9).
    endif.
** 1 Partida
    wl_input_zib-mandt    = sy-mandt.
    concatenate 'ZFI0061' wl_obj_key vg_ryear into  wl_input_zib-obj_key.
    wl_input_zib-seqitem    = '0001'.
    concatenate s_mes-low(2) '/' s_mes-low+2(4) into wl_input_zib-xblnr.
    if wl_estorno is initial.
      if wl_saida-vlr_ajust lt 0.
        wl_input_zib-bschl      = '50'.
      else.
        wl_input_zib-bschl      = '40'.
      endif.
    else.
      if wl_saida-vlr_ajust lt 0.
        wl_input_zib-bschl      = '40'.
      else.
        wl_input_zib-bschl      = '50'.
      endif.
    endif.
    if wg_t001-land1 eq 'BR'.
      concatenate s_bukrs-low+2(2) '01' into wl_input_zib-gsber.
    elseif wg_t001-land1 eq 'NL'.
      if s_bukrs-low eq '0201'.
        wl_input_zib-gsber = 'H201'.
      else.
        wl_input_zib-gsber = 'H202'.
      endif.
    elseif wg_t001-land1 eq 'LU'.
      if s_bukrs-low eq '0203'.
        wl_input_zib-gsber = 'L203'.
      endif.
    elseif wg_t001-land1 eq 'CH'.
      wl_input_zib-gsber = 'S201'.
    elseif wg_t001-land1 eq 'AR'
        or wg_t001-land1 eq 'PY'.
      if s_bukrs-low eq '0100'.
        wl_input_zib-gsber = 'T001'.
      else.
        wl_input_zib-gsber = 'P001'.
      endif.
    endif.
    wl_input_zib-bukrs      = s_bukrs-low.
    wl_input_zib-interface    = '35'.
    wl_input_zib-bktxt      = text-050.
    write vg_last_day to  wl_input_zib-bldat.
    write vg_last_day to  wl_input_zib-budat.
*    WL_INPUT_ZIB-BUDAT      = VG_LAST_DAY.
    wl_input_zib-gjahr      = vg_ryear.
    wl_input_zib-monat      = s_mes-low(2).
    wl_input_zib-blart      = 'VC'.
    wl_input_zib-hkont      = wl_saida-racct.
    wl_input_zib-wrbtr      = 0.
    wl_input_zib-vbund      = wl_saida-rassc.

    if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
      wl_input_zib-waers      = 'USD'.
    else.
      wl_input_zib-waers      = wg_t882g-curr1.
    endif.

    wl_input_zib-bupla      = space.

    if wg_t001-land1 eq 'AR'
      or wg_t001-land1 eq 'PY'.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      concatenate text-051 vanomes
            into wl_input_zib-sgtxt separated by space.
    elseif wg_0081-saknr is not initial.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      concatenate text-051 vanomes
          into wl_input_zib-sgtxt separated by space.
    elseif wg_0081-hkont is not initial.
*      CONCATENATE 'Ajuste moeda apresentação ' VANOMES
      concatenate text-052 vanomes
          into wl_input_zib-sgtxt separated by space.
    endif.

    clear v_bewar.
    select single bewar
      into v_bewar
      from zfit0030
      where hkont eq wl_input_zib-hkont
      and   cond  eq ''.

    wl_input_zib-bewar      = v_bewar.

    wl_input_zib-waers_i    = 'X'.

    if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
      wl_input_zib-dmbtr      = wl_saida-vlr_ajust.
      if wl_input_zib-dmbtr lt 0.
        multiply wl_input_zib-dmbtr by -1.
      endif.
    else.
      wl_input_zib-dmbtr      = 0.
    endif.

    wl_input_zib-waers_f    = wg_t882g-curr2.

    if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
      wl_input_zib-dmbe2     = 0.
    else.
      wl_input_zib-dmbe2      = wl_saida-vlr_ajust.
      if wl_input_zib-dmbe2 lt 0.
        multiply wl_input_zib-dmbe2 by -1.
      endif.
    endif.

    wl_input_zib-rg_atualizado  = 'N'.

    append wl_input_zib to tl_input_zib.
    clear: wl_input_zib.

** 2 Partida
    wl_input_zib-mandt    = sy-mandt.
    concatenate 'ZFI0061' wl_obj_key vg_ryear into  wl_input_zib-obj_key.
    wl_input_zib-seqitem    = '0002'.
    concatenate s_mes-low(2) '/' s_mes-low+2(4) into wl_input_zib-xblnr.
    if wl_estorno is initial.
      if wl_saida-vlr_ajust lt 0.
        wl_input_zib-bschl      = '40'.
      else.
        wl_input_zib-bschl      = '50'.
      endif.
    else.
      if wl_saida-vlr_ajust lt 0.
        wl_input_zib-bschl      = '50'.
      else.
        wl_input_zib-bschl      = '40'.
      endif.
    endif.
    if wg_t001-land1 eq 'BR'.
      concatenate s_bukrs-low+2(2) '01' into wl_input_zib-gsber.
    elseif wg_t001-land1 eq 'NL'.
      if s_bukrs-low eq '0201'.
        wl_input_zib-gsber = 'H201'.
      else.
        wl_input_zib-gsber = 'H202'.
      endif.
    elseif wg_t001-land1 eq 'LU'.
      if s_bukrs-low eq '0203'.
        wl_input_zib-gsber = 'L203'.
      endif.
    elseif wg_t001-land1 eq 'CH'.
      wl_input_zib-gsber = 'S201'.
    elseif wg_t001-land1 eq 'AR'
        or wg_t001-land1 eq 'PY'.
      if s_bukrs-low eq '0100'.
        wl_input_zib-gsber = 'T001'.
      else.
        wl_input_zib-gsber = 'P001'.
      endif.
    endif.
    wl_input_zib-bukrs      = s_bukrs-low.
    wl_input_zib-interface    = '35'.
    wl_input_zib-bktxt      = text-050.
    write vg_last_day to  wl_input_zib-bldat.
*    WL_INPUT_ZIB-BLDAT      = VG_LAST_DAY.
    write vg_last_day to  wl_input_zib-budat.
*    WL_INPUT_ZIB-BUDAT      = VG_LAST_DAY.
    wl_input_zib-gjahr      = vg_ryear.
    wl_input_zib-monat      = s_mes-low(2).
    wl_input_zib-blart      = 'VC'.

    if wg_t001-land1 eq 'AR'
        or wg_t001-land1 eq 'PY'.
      wl_input_zib-hkont      =  wg_0081-saknr.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      concatenate text-051 vanomes
         into wl_input_zib-sgtxt separated by space.
    elseif wg_0081-saknr is not initial.
      wl_input_zib-hkont      =  wg_0081-saknr.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      concatenate text-051 vanomes
      into wl_input_zib-sgtxt separated by space.
    elseif wg_0081-hkont is not initial.
      wl_input_zib-hkont      =  wg_0081-hkont.
*      CONCATENATE 'Ajuste moeda apresentação ' VANOMES
      concatenate text-052 vanomes
      into wl_input_zib-sgtxt separated by space.
    endif.

    clear v_bewar.
    select single bewar
      into v_bewar
      from zfit0030
      where hkont eq wl_input_zib-hkont
      and   cond  eq ''.

    wl_input_zib-bewar      = v_bewar.
    wl_input_zib-vbund      = wl_saida-rassc.

    wl_input_zib-wrbtr      = 0.

    if s_bukrs-low = '0004' or  s_bukrs-low eq '0037'.
      wl_input_zib-waers      = 'USD'.
    else.
      wl_input_zib-waers      = wg_t882g-curr1.
    endif.


    wl_input_zib-bupla      = space.

    wl_input_zib-waers_i    = 'X'.

    if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
      wl_input_zib-dmbtr      = wl_saida-vlr_ajust.
      if wl_input_zib-dmbtr lt 0.
        multiply wl_input_zib-dmbtr by -1.
      endif.
    else.
      wl_input_zib-dmbtr      = 0.
    endif.

    wl_input_zib-waers_f    = wg_t882g-curr2.
    if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
      wl_input_zib-dmbe2     = 0.
    else.
      wl_input_zib-dmbe2      = wl_saida-vlr_ajust.
      if wl_input_zib-dmbe2 lt 0.
        multiply wl_input_zib-dmbe2 by -1.
      endif.
    endif.

    wl_input_zib-rg_atualizado  = 'N'.

    append wl_input_zib to tl_input_zib.
    clear: wl_input_zib.


    modify zib_contabil from table tl_input_zib.
    commit work.
    if wl_estorno is initial.
      wl_input_0082-mandt = sy-mandt.
      wl_input_0082-bukrs = s_bukrs-low.
      wl_input_0082-mes_ano = s_mes-low.
      wl_input_0082-saknr = wl_saida-racct.
      wl_input_0082-vbund = wl_saida-rassc.
      wl_input_0082-dmbtr = wl_saida-curr1.
      wl_input_0082-dmbe2 = wl_saida-curr2.
      wl_input_0082-dmbe3 = wl_saida-curr3.
      wl_input_0082-tx_usd = wl_saida-tx_usd.
      wl_input_0082-tx_brl = wl_saida-tx_brl.
      wl_input_0082-sdo_corr_mi2 = wl_saida-saldo_corr.
      wl_input_0082-vlr_corr_mi2 = wl_saida-vlr_ajust.
      if wg_t001-land1 eq 'AR'
      or wg_t001-land1 eq 'PY'.
        wl_input_0082-sdo_corr_mi3 = wl_saida-saldo_corr2.
        wl_input_0082-vlr_corr_mi3 = wl_saida-vlr_ajust2.
      elseif wg_t001-land1 eq 'BR' or wg_t001-land1 eq 'NL' or wg_t001-land1 eq 'CH'.
        clear: wl_input_0082-sdo_corr_mi3, wl_input_0082-vlr_corr_mi3.
      endif.
      concatenate 'ZFI0061' wl_obj_key vg_ryear into wl_input_0082-obj_key.
      p_obj_key = wl_input_0082-obj_key.

      wl_input_0082-obj_key_est  = space.
      wl_input_0082-usnam        = sy-uname.
      wl_input_0082-aedat        = sy-datum.
      wl_input_0082-cputm        = sy-uzeit.

      modify  zfit0082 from wl_input_0082.
      commit work.
      wg_saida_exec-icon   = icon_green_light.
      wg_saida_exec-racct = wl_saida-racct.
      wg_saida_exec-txt50 = wl_saida-txt50.
*      WG_SAIDA_EXEC-MSG   = 'Documento foi importado na ZIB_CONTABIL com sucesso.' .
      wg_saida_exec-msg   = text-053 .
      append wg_saida_exec to tg_saida_exec.
      clear: wl_input_0082, wg_saida_exec.
    else.
      concatenate 'ZFI0061' wl_obj_key vg_ryear into wl_input_0082-obj_key_est.
      p_obj_key = wl_input_0082-obj_key_est.

      update zfit0082 set obj_key_est = wl_input_0082-obj_key_est
                      where bukrs   eq s_bukrs-low
                        and mes_ano eq s_mes-low
                        and saknr   eq wl_saida-racct
                        and vbund   eq wl_saida-rassc.
      commit work.

      wg_saida_exec-icon  = icon_green_light.
      wg_saida_exec-racct = wl_saida-racct.
      wg_saida_exec-txt50 = wl_saida-txt50.
      wg_saida_exec-msg   = text-054 .
      append wg_saida_exec to tg_saida_exec.

    endif.
  else.
    clear: wg_saida_exec.
    wg_saida_exec-icon   = icon_red_light.
    wg_saida_exec-racct  = wl_saida-racct.
    wg_saida_exec-txt50  = wl_saida-txt50.
    wg_saida_exec-msg    = text-055.
    append wg_saida_exec to tg_saida_exec.
  endif.
endform.                    " GERA_CONTABIL

*&---------------------------------------------------------------------*
*&      Form  GERA_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_SAIDA  text
*----------------------------------------------------------------------*
form gera_contabil  using    wl_saida type ty_saida
                             wl_estorno
                             wl_varicao
                   changing  p_obj_key type zib_contabil_chv-obj_key.

  data: wl_input_zib  type zib_contabil,
        tl_input_zib  type table of zib_contabil,
        wl_input_0084 type zfit0084,
        wa_zfit0084   type zfit0084,
        wa_zfit0086   type zfit0086,
        wa_saida_aux  type ty_saida,
        v_tipo        type c length 30,
        wl_obj_key(9),
        vanomes(7).

  select single *
    from zfit0086
    into wa_zfit0086
    where saknr = wl_saida-hkont.

  if sy-subrc ne 0.
*    MESSAGE E836(SD) WITH 'Cadastrar fornecedor/cliente'
*                          'para esta conta'.
    message e836(sd) with text-017
                          text-018.
    exit.
  endif.

  concatenate s_mes-low(2) '/' s_mes-low+2(4) into vanomes.
  refresh: tl_input_zib.
  clear: wl_input_zib, wl_input_0084.
  read table tg_t001 into wg_t001 with key bukrs =  s_bukrs-low binary search.
  read table tg_t882g into wg_t882g with key rbukrs = s_bukrs-low binary search.

  loop at tg_0081 into wg_0081.
    if wg_0081-conta_de is not initial.
      if wl_saida-hkont eq wg_0081-conta_de.
        exit.
      endif.
    endif.
    clear:wg_0081.
  endloop.

  if wg_0081 is not initial .
    if wg_t001-land1 eq 'AR' or wg_t001-land1 eq 'PY'.
      v_saknr      =  wg_0081-saknr.
    elseif wg_0081-saknr is not initial.
      v_saknr      =  wg_0081-saknr.
    elseif wg_0081-hkont is not initial.
      v_saknr      =  wg_0081-hkont.
    endif.

    "Sumariza conta por VBUND e BEWAR
    if p_obj_key is not initial .
      if wl_varicao is not  initial.
        tg_saida_aux[]  = tg_saida[].
        delete tg_saida_aux where obj_key     ne p_obj_key.
        sort tg_saida_aux by vbund bewar.
        delete adjacent duplicates from tg_saida_aux  comparing vbund bewar.
        loop at tg_saida_aux into wa_saida_aux.
          wa_saida_aux-vlr_ajust  = 0.
          wa_saida_aux-vlr_ajust2 = 0.
          loop at tg_saida into wg_saida where hkont = wa_saida_aux-hkont
                                         and   vbund = wa_saida_aux-vbund
                                         and   bewar = wa_saida_aux-bewar.
            if wl_estorno is not initial.
              select single *
                from zfit0084
                   into wa_zfit0084
                   where bukrs   eq s_bukrs-low
                   and mes_ano eq s_mes-low
                   and saknr   eq wg_saida-hkont
                   and belnr   eq wg_saida-belnr2
                   and buzei   eq wg_saida-buzei.
              if sy-subrc = 0.
                add wa_zfit0084-vlr_corr_mi2 to wa_saida_aux-vlr_ajust.
                add wa_zfit0084-vlr_corr_mi3 to wa_saida_aux-vlr_ajust2.
              endif.
            else.
              add wg_saida-vlr_ajust  to wa_saida_aux-vlr_ajust.
              add wg_saida-vlr_ajust2 to wa_saida_aux-vlr_ajust2.
            endif.
          endloop.
          wa_saida_aux-kunnr = wa_zfit0086-cd_cli_for.
          append wa_saida_aux to tg_saida_res.
        endloop.
      endif.
    else.
      if wl_varicao is not  initial.
        tg_saida_aux[]  = tg_saida[].
        delete tg_saida_aux where hkont       ne wl_saida-hkont.
        delete tg_saida_aux where obj_key     is not initial
                            and   obj_key_est is initial.
        sort tg_saida_aux by vbund bewar.
        delete adjacent duplicates from tg_saida_aux  comparing vbund bewar.
        loop at tg_saida_aux into wa_saida_aux.
          wa_saida_aux-vlr_ajust  = 0.
          wa_saida_aux-vlr_ajust2 = 0.
          loop at tg_saida into wg_saida where hkont = wa_saida_aux-hkont
                                         and   vbund = wa_saida_aux-vbund
                                         and   bewar = wa_saida_aux-bewar.
            add wg_saida-vlr_ajust  to wa_saida_aux-vlr_ajust.
            add wg_saida-vlr_ajust2 to wa_saida_aux-vlr_ajust2.
          endloop.
          wa_saida_aux-kunnr = wa_zfit0086-cd_cli_for.
          append wa_saida_aux to tg_saida_res.
        endloop.
      endif.
    endif.
    "
    if tg_saida_res[] is initial.
      message text-019 type 'I'.
      exit.
    endif.

    loop at tg_saida_res into wl_saida. "ALRS
      tabix = sy-tabix.
      v_vbund = wl_saida-vbund.

      perform get_next_number using 'Z_SALDOMO2' '01' changing wl_obj_key.

** 1 Partida *************************************************************************************************************
      wl_input_zib-mandt    = sy-mandt.
      concatenate 'ZFI0063' wl_obj_key vg_ryear into  wl_input_zib-obj_key.
      wl_input_zib-seqitem    = '0001'.
      concatenate s_mes-low(2) '/' s_mes-low+2(4) into wl_input_zib-xblnr.

      perform busca_chave using p_forn wl_estorno wl_varicao wl_saida-vlr_ajust wl_saida-umskz changing wl_input_zib-bschl.

      if wg_t001-land1 eq 'BR'.
        concatenate s_bukrs-low+2(2) '01' into wl_input_zib-gsber.
      elseif wg_t001-land1 eq 'NL'.
        if s_bukrs-low eq '0201'.
          wl_input_zib-gsber = 'H201'.
        else.
          wl_input_zib-gsber = 'H202'.
        endif.

      elseif wg_t001-land1 eq 'LU'.
        if s_bukrs-low eq '0203'.
          wl_input_zib-gsber = 'L203'.
        endif.
      elseif wg_t001-land1 eq 'CH'.
        wl_input_zib-gsber = 'S201'.
      elseif wg_t001-land1 eq 'AR' or wg_t001-land1 eq 'PY'.
        if s_bukrs-low eq '0100'.
          wl_input_zib-gsber = 'T001'.
        else.
          wl_input_zib-gsber = 'P001'.
        endif.
      endif.
      wl_input_zib-bukrs      = s_bukrs-low.
      wl_input_zib-interface  = '35'.
*      WL_INPUT_ZIB-BKTXT      = 'VAR.CTAS MONETÁRIAS'.
      wl_input_zib-bktxt      = text-020.
      write vg_last_day to wl_input_zib-bldat.
      write vg_last_day to wl_input_zib-budat.
      wl_input_zib-gjahr      = vg_ryear.
      wl_input_zib-monat      = s_mes-low(2).
      wl_input_zib-blart      = 'VC'.
      wl_input_zib-hkont      = wl_saida-kunnr.

      wl_input_zib-wrbtr      =  0.


      if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
        wl_input_zib-waers    = 'USD'.
      else.
        wl_input_zib-waers    = wg_t882g-curr1.
      endif.

      wl_input_zib-bupla      = space.
      if wg_t001-land1 eq 'AR'
         or wg_t001-land1 eq 'PY'.
*        CONCATENATE 'Ajuste moeda funcional ' VANOMES
        concatenate text-021 vanomes
              into wl_input_zib-sgtxt separated by space.
      elseif wg_0081-saknr is not initial.
*        CONCATENATE 'Ajuste moeda funcional ' VANOMES
        concatenate text-021 vanomes
            into wl_input_zib-sgtxt separated by space.
      elseif wg_0081-hkont is not initial.
        concatenate text-022 vanomes
            into wl_input_zib-sgtxt separated by space.
      endif.

      wl_input_zib-vbund      = v_vbund.

      wl_input_zib-bewar      = wl_saida-bewar.

      wl_input_zib-waers_i    = 'X'.
      if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
        wl_input_zib-dmbtr      = wl_saida-vlr_ajust.
        if wl_input_zib-dmbtr lt 0.
          multiply wl_input_zib-dmbtr by -1.
        endif.
      else.
        wl_input_zib-dmbtr      = 0.
      endif.

      wl_input_zib-waers_f    = wg_t882g-curr2.
      if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
        wl_input_zib-dmbe2     = 0.
      else.
        if wl_saida-vlr_ajust gt 0.
          wl_input_zib-dmbe2 = wl_saida-vlr_ajust.
        else.
          wl_input_zib-dmbe2 = wl_saida-vlr_ajust * -1.
        endif.
      endif.

      wl_input_zib-zuonr         = wl_saida-belnr2.
      wl_input_zib-umskz         = wl_saida-umskz.
      wl_input_zib-rg_atualizado = 'N'.

      append wl_input_zib to tl_input_zib.
      clear: wl_input_zib.

** 2 Partida *************************************************************************************************************************
      wl_input_zib-mandt    = sy-mandt.
      concatenate 'ZFI0063' wl_obj_key vg_ryear into  wl_input_zib-obj_key.
      wl_input_zib-seqitem    = '0002'.
      concatenate s_mes-low(2) '/' s_mes-low+2(4) into wl_input_zib-xblnr.
      if wl_estorno is initial.
        if wl_saida-vlr_ajust lt 0.
          wl_input_zib-bschl      = '40'.
        else.
          wl_input_zib-bschl      = '50'.
        endif.
      else.
        if wl_saida-vlr_ajust lt 0.
          wl_input_zib-bschl      = '50'.
        else.
          wl_input_zib-bschl      = '40'.
        endif.
      endif.
      if wg_t001-land1 eq 'BR'.
        concatenate s_bukrs-low+2(2) '01' into wl_input_zib-gsber.
      elseif wg_t001-land1 eq 'NL'.
        if s_bukrs-low eq '0201'.
          wl_input_zib-gsber = 'H201'.
        else.
          wl_input_zib-gsber = 'H202'.
        endif.
      elseif wg_t001-land1 eq 'LU'.
        if s_bukrs-low eq '0203'.
          wl_input_zib-gsber = 'L203'.
        endif.
      elseif wg_t001-land1 eq 'CH'.
        wl_input_zib-gsber = 'S201'.
      elseif wg_t001-land1 eq 'AR'
          or wg_t001-land1 eq 'PY'.
        if s_bukrs-low eq '0100'.
          wl_input_zib-gsber = 'T001'.
        else.
          wl_input_zib-gsber = 'P001'.
        endif.
      endif.
      wl_input_zib-bukrs      = s_bukrs-low.
      wl_input_zib-interface  = '35'.
*      WL_INPUT_ZIB-BKTXT      = 'VAR.CTAS MONETÁRIAS'.
      wl_input_zib-bktxt      = text-020.
      write vg_last_day to wl_input_zib-bldat.
      write vg_last_day to wl_input_zib-budat.
      wl_input_zib-gjahr      = vg_ryear.
      wl_input_zib-monat      = s_mes-low(2).
      wl_input_zib-blart      = 'VC'.

      if wg_t001-land1 eq 'AR' or wg_t001-land1 eq 'PY'.
        wl_input_zib-hkont      =  wg_0081-saknr.
        concatenate text-021 vanomes into wl_input_zib-sgtxt separated by space.
      elseif wg_0081-saknr is not initial.
        wl_input_zib-hkont      =  wg_0081-saknr.
        concatenate text-021 vanomes into wl_input_zib-sgtxt separated by space.
      elseif wg_0081-hkont is not initial.
        wl_input_zib-hkont      =  wg_0081-hkont.
        concatenate text-022 vanomes into wl_input_zib-sgtxt separated by space.
      endif.
      wl_input_zib-vbund      = v_vbund.
      wl_input_zib-bewar      = wl_saida-bewar.

      wl_input_zib-wrbtr      = 0.

      if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
        wl_input_zib-waers      = 'USD'.
      else.
        wl_input_zib-waers      = wg_t882g-curr1.
      endif.

      wl_input_zib-bupla      = space.

      wl_input_zib-waers_i    = 'X'.

      if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
        wl_input_zib-dmbtr      = wl_saida-vlr_ajust.
        if wl_input_zib-dmbtr lt 0.
          multiply wl_input_zib-dmbtr by -1.
        endif.
      else.
        wl_input_zib-dmbtr      = 0.
      endif.

      wl_input_zib-waers_f    = wg_t882g-curr2.

      if s_bukrs-low = '0004' or s_bukrs-low eq '0037'.
        wl_input_zib-dmbe2     = 0.
      else.
        if wl_saida-vlr_ajust gt 0.
          wl_input_zib-dmbe2      = wl_saida-vlr_ajust.
        else.
          wl_input_zib-dmbe2      = wl_saida-vlr_ajust * -1.
        endif.
      endif.

      wl_input_zib-zuonr          = wl_saida-belnr2.
      wl_input_zib-rg_atualizado  = 'N'.

      append wl_input_zib to tl_input_zib.
      clear: wl_input_zib.


      "Grava Chave
      if wl_estorno is initial.
        if wl_varicao is not initial.
          concatenate 'ZFI0063' wl_obj_key vg_ryear into wl_saida-obj_key.
          wl_saida-obj_key_est = space.
        else.
          concatenate 'ZFI0063' wl_obj_key vg_ryear into wl_saida-obj_key_inv.
          wl_saida-obj_key_inv_est = space.
        endif.
      else.
        if wl_varicao is not initial.
          concatenate 'ZFI0063' wl_obj_key vg_ryear into wl_saida-obj_key_est.
        else.
          concatenate 'ZFI0063' wl_obj_key vg_ryear into wl_saida-obj_key_inv_est.
        endif.
      endif.
      modify tg_saida_res from wl_saida index tabix transporting obj_key obj_key_est obj_key_inv obj_key_inv_est.
    endloop. "ALRS

    "inverte todas de uma vez só 09.03.2015
    clear: wg_saida_exec.
    if wl_varicao is initial.
      v_tipo = 'Inverso'.
      loop at tl_input_zib into wl_input_zib.
        tabix2 = sy-tabix.
        perform inverte_chave using wg_saida_exec changing wl_input_zib.
        if wg_saida_exec is not initial.
          wg_saida_exec-tipo  = v_tipo.
          wg_saida_exec-lifnr = wl_saida-kunnr.
          wg_saida_exec-name1 = wl_saida-name1.
          wg_saida_exec-hkont = wl_saida-hkont.
          wg_saida_exec-txt50 = wl_saida-txt50.
          append wg_saida_exec to tg_saida_exec.
        endif.
        modify tl_input_zib index tabix2 from wl_input_zib transporting bldat budat bschl gjahr monat.
      endloop.
    else.
      v_tipo = text-m12. "'Variação'.
    endif.

    "CHECK WG_SAIDA_EXEC IS INITIAL. "ALRS 10.03.2015
    if tl_input_zib[] is initial.
      message text-023 type 'I'.
      exit.
    endif.
    modify zib_contabil from table tl_input_zib.
    if sy-subrc ne 0.
      message text-024 type 'I'.
      exit.
    endif.
    commit work.

    " Atualiza ALV (tela)
    loop at tg_saida_res into wa_saida_aux.
      loop at tg_saida into wl_saida where hkont = wa_saida_aux-hkont
                                     and   vbund = wa_saida_aux-vbund
                                     and   bewar = wa_saida_aux-bewar.
        tabix = sy-tabix.
        if wl_estorno is initial.
          wl_input_0084-mandt        = sy-mandt.
          wl_input_0084-bukrs        = s_bukrs-low.
          wl_input_0084-mes_ano      = s_mes-low.
          wl_input_0084-saknr        = wl_saida-hkont.
          wl_input_0084-belnr        = wl_saida-belnr2.
          wl_input_0084-buzei        = wl_saida-buzei.
          wl_input_0084-lifnr        = wl_saida-kunnr.
          wl_input_0084-dmbtr        = wl_saida-dmbtr.
          wl_input_0084-dmbe2        = wl_saida-dmbe2.
          wl_input_0084-dmbe3        = wl_saida-dmbe3.
          wl_input_0084-tx_usd       = wl_saida-tx_usd.
          wl_input_0084-tx_brl       = wl_saida-tx_brl.
          wl_input_0084-sdo_corr_mi2 = wl_saida-saldo_corr.
          wl_input_0084-vlr_corr_mi2 = wl_saida-vlr_ajust.

          wl_input_0084-obj_key      = wa_saida_aux-obj_key.
          wl_input_0084-obj_key_inv  = wa_saida_aux-obj_key_inv.
          "
          if wg_t001-land1 eq 'AR' or wg_t001-land1 eq 'PY'.
            wl_input_0084-sdo_corr_mi3 = wl_saida-saldo_corr2.
            wl_input_0084-vlr_corr_mi3 = wl_saida-vlr_ajust2.
          elseif wg_t001-land1 eq 'BR' or wg_t001-land1 eq 'NL'  or wg_t001-land1 eq 'CH'.
            clear: wl_input_0084-sdo_corr_mi3, wl_input_0084-vlr_corr_mi3.
          endif.

          wa_saida_aux-obj_key      = wa_saida_aux-obj_key.
          wa_saida_aux-obj_key_inv  = wa_saida_aux-obj_key_inv.

          if wl_varicao is not initial.
            wl_input_0084-obj_key_est = space.
            wa_saida_aux-obj_key_est = space.
            modify tg_saida from wa_saida_aux index tabix transporting obj_key obj_key_est.
          else.
            wl_input_0084-obj_key_inv_est = space.
            wa_saida_aux-obj_key_inv_est = space.
            modify tg_saida from wa_saida_aux index tabix transporting obj_key_inv obj_key_inv_est.
          endif.
          wl_input_0084-usnam        = sy-uname.
          wl_input_0084-aedat        = sy-datum.
          wl_input_0084-cputm        = sy-uzeit.

          modify zfit0084 from wl_input_0084.
          commit work.

          clear: wl_input_0084.
        else.
          if wl_varicao is not initial.
            update zfit0084
               set obj_key_est = wa_saida_aux-obj_key_est
             where bukrs   eq s_bukrs-low
               and mes_ano eq s_mes-low
               and saknr   eq wl_saida-hkont
               and belnr   eq wl_saida-belnr2
               and buzei   eq wl_saida-buzei.
            if sy-subrc ne 0.
              update zfit0084
                set obj_key_est = wa_saida_aux-obj_key_est
              where bukrs   eq s_bukrs-low
                and mes_ano eq s_mes-low
                and saknr   eq wl_saida-hkont
                and belnr   eq wl_saida-belnr2.
            endif.
            modify tg_saida from wa_saida_aux index tabix transporting obj_key_est.
          else.
            update zfit0084
               set obj_key_inv_est = wa_saida_aux-obj_key_inv_est
             where bukrs   eq s_bukrs-low
               and mes_ano eq s_mes-low
               and saknr   eq wl_saida-hkont
               and belnr   eq wl_saida-belnr2
               and buzei   eq wl_saida-buzei.
            if sy-subrc ne 0.
              update zfit0084
               set obj_key_inv_est = wa_saida_aux-obj_key_inv_est
             where bukrs   eq s_bukrs-low
               and mes_ano eq s_mes-low
               and saknr   eq wl_saida-hkont
               and belnr   eq wl_saida-belnr2.
            endif.
            modify tg_saida from wa_saida_aux index tabix transporting obj_key_inv_est.
          endif.
          commit work.
        endif.
      endloop.
      if wl_estorno is initial.
        wg_saida_exec-icon  = icon_green_light.
        wg_saida_exec-tipo  = v_tipo.
        wg_saida_exec-lifnr = wa_saida_aux-kunnr.
        wg_saida_exec-name1 = wa_saida_aux-name1.
        wg_saida_exec-hkont = wa_saida_aux-hkont.
        wg_saida_exec-txt50 = wa_saida_aux-txt50.
        wg_saida_exec-msg   = text-025 .
        append wg_saida_exec to tg_saida_exec.
        clear wg_saida_exec.
      else.
        wg_saida_exec-icon  = icon_green_light.
        wg_saida_exec-tipo  = v_tipo.
        wg_saida_exec-lifnr = wa_saida_aux-kunnr.
        wg_saida_exec-name1 = wa_saida_aux-name1.
        wg_saida_exec-hkont = wa_saida_aux-hkont.
        wg_saida_exec-txt50 = wa_saida_aux-txt50.
        wg_saida_exec-msg   = text-026 .
        append wg_saida_exec to tg_saida_exec.
        clear wg_saida_exec.
      endif.

    endloop.
  else.
    clear: wg_saida_exec.
    wg_saida_exec-icon   = icon_red_light.
    wg_saida_exec-tipo   = v_tipo.
    wg_saida_exec-lifnr  = wl_saida-kunnr.
    wg_saida_exec-name1  = wl_saida-name1.
    wg_saida_exec-hkont  = wl_saida-hkont.
    wg_saida_exec-txt50  = wl_saida-txt50.
    wg_saida_exec-msg    = text-027.
    append wg_saida_exec to tg_saida_exec.
  endif.
endform.                    " GERA_CONTABIL
*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
form get_next_number  using    p_object   "TYPE nrobj
                               p_nr_range "TYPE nrnr
*                               P_COMMIT
                      changing p_number.

*  IF P_COMMIT IS NOT INITIAL.
  clear p_number.

  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = p_nr_range
      object                  = p_object
    importing
      number                  = p_number
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.
  if sy-subrc ne 0.
    clear: p_number.
    message e836(sd) with text-028
                          text-029.

  endif.
*  ELSE.
*    P_NUMBER = '$00000001'.
*    WG_FLAG = C_X.
*  ENDIF.

endform.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form imprimir_exec .
  if tg_saida_exec[] is not initial.
    perform montar_layout using 'TG_SAIDA_EXEC'.
    call function 'REUSE_ALV_GRID_DISPLAY'
      exporting
*       i_callback_program    = v_report
*       I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
        it_fieldcat           = estrutura[]
*       IT_SORT               = T_SORT[]
        i_save                = 'A'
        i_screen_start_column = 3
        i_screen_start_line   = 3
        i_screen_end_column   = 100
        i_screen_end_line     = 13
      tables
        t_outtab              = tg_saida_exec.
  endif.
endform.                    " IMPRIMIR_EXEC

form atualiza_saidar  tables   tl_saida   like tg_saidar
                              tl_zib     structure zib_contabil
                              tl_zib_chv structure zib_contabil_chv
                              tl_zib_err structure zib_contabil_err.

  sort: tl_zib     by obj_key rg_atualizado,
        tl_zib_chv by obj_key,
        tl_zib_err by obj_key.

  loop at tl_saida.
    if tl_saida-obj_key is not initial.
      clear: tl_zib, tl_saida-belnr, wg_bkpf_fb08, wg_bkpf_fb08_e, tl_saida-stblg.
      read table tl_zib
        with key obj_key       = tl_saida-obj_key
                 rg_atualizado = 'S'
                         binary search.

      if sy-subrc is initial.
        clear: tl_zib_chv.
        read table tl_zib_chv
        with key obj_key       = tl_saida-obj_key
                         binary search.
        if sy-subrc is initial.
          tl_saida-belnr = tl_zib_chv-belnr.

          if sy-subrc = 0.
            select single   bukrs belnr gjahr budat
               from bkpf
               into wg_bkpf_fb08_e
               where bukrs eq wg_bkpf_fb08-bukrs
               and   belnr eq wg_bkpf_fb08-stblg
               and   gjahr eq wg_bkpf_fb08-stjah.
            if wg_bkpf_fb08_e-budat+0(6) = wg_bkpf_fb08-budat+0(6). "estorno e lançamento mesmo mês, limpa pra gerar outro documento
              delete from zib_contabil_err where obj_key  = tl_saida-obj_key.
*              clear: tl_saida-belnr, tl_saida-obj_key.
            else.
              tl_saida-line_color = 'C601'.
              modify tl_saida transporting line_color.
            endif.
          endif.

          "FB08
          clear: wg_bkpf_fb08.
          select single   bukrs belnr  gjahr  budat stblg stjah
            from bkpf
            into wg_bkpf_fb08
            where bukrs eq tl_zib_chv-bukrs
            and   belnr eq tl_zib_chv-belnr
            and   gjahr eq tl_zib_chv-gjahr
            and   stblg ne ''.
*          " US 155163 // MMSILVA - 16.10.2024
          if p_reve is not initial.
            tl_saida-stblg = wg_bkpf_fb08-stblg.
          endif.
*          " US 155163 // MMSILVA - 16.10.2024

          tl_saida-log   = icon_enter_more.
          modify tl_saida transporting belnr  obj_key  log stblg.
          clear: tl_saida-stblg, wg_bkpf_fb08-stblg.
        else.
          clear: tl_zib_err.
          read table tl_zib_err
            with key obj_key       = tl_saida-obj_key
                             binary search.
          if sy-subrc is initial.
            tl_saida-belnr = space.
            tl_saida-log   = icon_display_more.
            modify tl_saida transporting belnr log.
          endif.
        endif.
      else.
        tl_saida-belnr = icon_operation.
        modify tl_saida transporting belnr.
      endif.
    else.
      tl_saida-obj_key = space.
      tl_saida-belnr = space.
      tl_saida-log   = icon_enter_more.
      modify tl_saida transporting log belnr obj_key.
    endif.

  endloop.

endform.                    " ATUALIZA_SAIDA

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TG_SAIDA  text
*      -->P_TL_ZIB  text
*      -->P_TL_ZIB_CHV  text
*      -->P_TL_ZIB_ERR  text
*----------------------------------------------------------------------*
form atualiza_saida  tables   tl_saida   like tg_saida
                              tl_zib     structure zib_contabil
                              tl_zib_chv structure zib_contabil_chv
                              tl_zib_err structure zib_contabil_err.

  sort: tl_zib     by obj_key rg_atualizado,
        tl_zib_chv by obj_key,
        tl_zib_err by obj_key.

*  IF P_VISU EQ 'X'.
*    EXIT.
*  ENDIF.

  loop at tl_saida.

    " Ajusta Visualização de Lançamento da Variação Cambial """""""""""""""""""""""""""
    "**********************************************************************************
    if tl_saida-obj_key is not initial.
      clear: tl_zib.
      read table tl_zib with key obj_key = tl_saida-obj_key rg_atualizado = 'S' binary search.

      if sy-subrc is initial.
        clear: tl_zib_chv.
        read table tl_zib_chv with key obj_key = tl_saida-obj_key binary search.
        if sy-subrc is initial.
          tl_saida-belnr = tl_zib_chv-belnr.
          if sy-subrc = 0.
            select single   bukrs belnr gjahr budat
               from bkpf
               into wg_bkpf_fb08_e
               where bukrs eq wg_bkpf_fb08-bukrs
               and   belnr eq wg_bkpf_fb08-stblg
               and   gjahr eq wg_bkpf_fb08-stjah.
            if wg_bkpf_fb08_e-budat+0(6) = wg_bkpf_fb08-budat+0(6). "estorno e lançamento mesmo mês, limpa pra gerar outro documento
              delete from zib_contabil_err where obj_key  = tl_saida-obj_key.
*              clear: tl_saida-belnr, tl_saida-obj_key.
            else.
              tl_saida-line_color = 'C601'.
              modify tl_saida transporting line_color.
            endif.
          endif.

          "FB08
          clear: wg_bkpf_fb08.
          select single   bukrs belnr  gjahr  budat stblg stjah
            from bkpf
            into wg_bkpf_fb08
            where bukrs eq tl_zib_chv-bukrs
            and   belnr eq tl_zib_chv-belnr
            and   gjahr eq tl_zib_chv-gjahr
            and   stblg ne ''.
*          " US 155163 // MMSILVA - 16.10.2024
          if p_reve is not initial.
            tl_saida-stblg = wg_bkpf_fb08-stblg.
          endif.
*            " US 155163 // MMSILVA - 16.10.2024
          tl_saida-log   = icon_enter_more.
          modify tl_saida transporting belnr obj_key log stblg.
        else.
          clear: tl_zib_err.
          read table tl_zib_err with key obj_key = tl_saida-obj_key binary search.
          if sy-subrc is initial.
*            TL_SAIDA-OBJ_KEY = SPACE.
            tl_saida-belnr   = space.
            tl_saida-log    = icon_display_more.
            modify tl_saida transporting belnr  log.
          endif.
        endif.
      else.
        read table tl_zib with key obj_key = tl_saida-obj_key binary search.
        if sy-subrc = 0.
          tl_saida-belnr = icon_operation.
          modify tl_saida transporting belnr.
        else.
          tl_saida-obj_key = space.
          tl_saida-belnr = space.
          tl_saida-log   = icon_system_cancel.
          modify tl_saida transporting log belnr obj_key.
        endif.
      endif.
    else.
      tl_saida-obj_key = space.
      tl_saida-belnr   = space.
      tl_saida-log     = icon_enter_more.
      modify tl_saida transporting log obj_key belnr.
    endif.
    clear: tl_saida.
  endloop.

endform.                    " ATUALIZA_SAIDA


*&---------------------------------------------------------------------*
*&      Form  GERA_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form gera_documentos tables tl_zib     structure zib_contabil
                            tl_zib_chv structure zib_contabil_chv
                            tl_zib_err structure zib_contabil_err.

  data: wl_obj_key  type zib_contabil_chv-obj_key,
        wa_zfit0084 type zfit0084,
        w_cont      type i.

  refresh tg_saida_aux.
  refresh tg_saida_res.
  w_cont = 0.
  loop at tg_saida into wg_saida where mark is not initial.
    add 1 to w_cont.
  endloop.
  if w_cont ne 1.
    message text-034 type 'I'.
    exit.
  endif.

  call function 'Z_CONTROLE_FECHAMES'
    exporting
      i_bukrs  = s_bukrs-low
      i_data   = vg_last_day
    importing
      e_status = e_status
      e_messa  = e_messa
    exceptions
      error    = 1
      others   = 2.
  if e_status = 'E'.
    message e398(00) with e_messa.
    exit.
  endif.

  loop at tg_saida into wg_saida where mark is not initial.
    if wg_saida-obj_key is not initial and wg_saida-belnr is initial.
      read table tl_zib with key obj_key = wg_saida-obj_key binary search.
      if tl_zib-rg_atualizado eq 'N'.
        clear: wg_saida_exec.
        wg_saida_exec-icon   = icon_yellow_light.
        wg_saida_exec-tipo   = text-m12. "'Variação'.
        wg_saida_exec-lifnr  = wg_saida-kunnr.
        wg_saida_exec-name1  = wg_saida-name1.
        wg_saida_exec-hkont  = wg_saida-hkont.
        wg_saida_exec-txt50  = wg_saida-txt50.
        wg_saida_exec-msg    = text-030 .
        append wg_saida_exec to tg_saida_exec.
        continue.
      elseif tl_zib-rg_atualizado eq 'S'.
        read table tl_zib_chv with key obj_key = wg_saida-obj_key binary search.
        if sy-subrc is initial.
          clear: wg_saida_exec.
          wg_saida_exec-icon   = icon_green_light.
          wg_saida_exec-tipo   = text-m12. "'Variação'.
          wg_saida_exec-lifnr  = wg_saida-kunnr.
          wg_saida_exec-name1  = wg_saida-name1.
          wg_saida_exec-hkont  = wg_saida-hkont.
          wg_saida_exec-txt50  = wg_saida-txt50.
          wg_saida_exec-msg    = text-031 .
          append wg_saida_exec to tg_saida_exec.
          acao '&REFRESH'.
          continue.
        else.
          read table tl_zib_err with key obj_key = wg_saida-obj_key binary search.
          if sy-subrc is initial.
            clear: wg_saida_exec.
            wg_saida_exec-icon   = icon_red_light.
            wg_saida_exec-tipo   = text-m12. "'Variação'.
            wg_saida_exec-lifnr  = wg_saida-kunnr.
            wg_saida_exec-name1  = wg_saida-name1.
            wg_saida_exec-hkont  = wg_saida-hkont.
            wg_saida_exec-txt50  = wg_saida-txt50.
            wg_saida_exec-msg    = text-032 .
            append wg_saida_exec to tg_saida_exec.
            call function 'POPUP_TO_CONFIRM'
              exporting
*               TEXT_QUESTION         = '“Contabilização com erro, reprocessa, sim ou não?'
                text_question         = text-035
                text_button_1         = text-036
                icon_button_1         = 'ICON_OKAY'
                text_button_2         = text-037
                icon_button_2         = 'ICON_CANCEL'
                default_button        = '1'
                display_cancel_button = ' '
                start_column          = 25
                start_row             = 6
              importing
                answer                = w_answer
              exceptions
                text_not_found        = 1
                others                = 2.

            if sy-subrc <> 0.
              message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            endif.
            if w_answer = '1'.
              delete from zib_contabil_err where obj_key = wg_saida-obj_key.
              delete from zib_contabil     where obj_key = wg_saida-obj_key.
              delete from zfit0084         where obj_key = wg_saida-obj_key.
            else.
              acao '&REFRESH'.
              continue.
            endif.
          else.
            clear: wg_saida_exec.
            wg_saida_exec-icon   = icon_yellow_light.
            wg_saida_exec-tipo   = text-m12. "'Variação'.
            wg_saida_exec-lifnr  = wg_saida-kunnr.
            wg_saida_exec-name1  = wg_saida-name1.
            wg_saida_exec-hkont  = wg_saida-hkont.
            wg_saida_exec-txt50  = wg_saida-txt50.
            wg_saida_exec-msg    = text-030 .
            append wg_saida_exec to tg_saida_exec.
            continue.
          endif.
        endif.
      endif.
    endif.

    if wg_saida-obj_key is initial.
      select single *
        from zfit0084
        into wa_zfit0084
        where bukrs     = s_bukrs-low
        and   mes_ano   = s_mes-low
        and   saknr     = wg_saida-hkont
        and   belnr     = wg_saida-belnr2
        and   buzei     = wg_saida-buzei
        and   obj_key   ne ''.
      if sy-subrc = 0.
        continue.
      endif.

    endif.

    if wg_saida-vlr_ajust = 0.
      clear: wg_saida_exec.
      wg_saida_exec-icon   = icon_red_light.
      wg_saida_exec-tipo   = text-m12. "'Variação'.
      wg_saida_exec-lifnr  = wg_saida-kunnr.
      wg_saida_exec-name1  = wg_saida-name1.
      wg_saida_exec-hkont  = wg_saida-hkont.
      wg_saida_exec-txt50  = wg_saida-txt50.
      wg_saida_exec-msg    = text-m11.
      append wg_saida_exec to tg_saida_exec.
      continue.
    endif.

    if wg_saida-belnr is not initial.
      clear: wg_saida_exec.
      wg_saida_exec-icon   = icon_red_light.
      wg_saida_exec-tipo   = text-m12. "'Variação'.
      wg_saida_exec-lifnr  = wg_saida-kunnr.
      wg_saida_exec-name1  = wg_saida-name1.
      wg_saida_exec-hkont  = wg_saida-hkont.
      wg_saida_exec-txt50  = wg_saida-txt50.
      wg_saida_exec-msg    = text-038 .
      append wg_saida_exec to tg_saida_exec.
      continue.
    endif.
    clear wl_obj_key.
    perform gera_contabil using wg_saida space c_x changing wl_obj_key.

  endloop.

endform.                    " GERA_DOCUMENTOS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_CHAVE
*&---------------------------------------------------------------------*
*       Busca Chave de Lançamento
*----------------------------------------------------------------------*
form busca_chave  using    pforn         type char1
                           wl_estorno    type char1
                           wl_varicao    type char1
                           vlr_ajust     like bsik-dmbtr
                           umskz         like bsik-umskz
                  changing zib_bschl     like bsik-bschl.

  if pforn = 'X'.
    if vlr_ajust > 0 and  umskz is initial.  zib_bschl =  '21'. endif.
    if vlr_ajust < 0 and  umskz is initial.  zib_bschl =  '31'. endif.

    if vlr_ajust > 0 and  umskz is not initial.  zib_bschl =  '29'. endif.
    if vlr_ajust < 0 and  umskz is not initial.  zib_bschl =  '39'. endif.

    if wl_estorno is not initial.
      if zib_bschl = '21'.
        zib_bschl = '31'.
      elseif zib_bschl = '31'.
        zib_bschl = '21'.
      endif.

      if zib_bschl = '29'.
        zib_bschl = '39'.
      elseif zib_bschl = '39'.
        zib_bschl = '29'.
      endif.
    endif.

  else.
    if vlr_ajust > 0 and  umskz is initial.  zib_bschl =  '01'. endif.
    if vlr_ajust < 0 and  umskz is initial.  zib_bschl =  '11'. endif.

    if vlr_ajust > 0 and  umskz is not initial.  zib_bschl =  '09'. endif.
    if vlr_ajust < 0 and  umskz is not initial.  zib_bschl =  '19'. endif.

    if wl_estorno is not initial.
      if zib_bschl = '01'.
        zib_bschl = '11'.
      elseif zib_bschl = '11'.
        zib_bschl = '01'.
      endif.

      if zib_bschl = '09'.
        zib_bschl = '19'.
      elseif zib_bschl = '19'.
        zib_bschl = '09'.
      endif.
    endif.
  endif.

endform.                    " BUSCA_CHAVE

*&---------------------------------------------------------------------*
*&      Form  BUSCA_CHAVE2
*&---------------------------------------------------------------------*
*       Busca Chave de Lançamento
*----------------------------------------------------------------------*
form busca_chave2 using    pforn         type char1
                           wl_estorno    type char1
                           wl_varicao    type char1
                           vlr_ajust     like bsik-dmbtr
                           umskz         like bsik-umskz
                  changing zib_bschl     like bsik-bschl.

  if pforn = 'X'.
    if vlr_ajust < 0 and  umskz is initial.  zib_bschl =  '21'. endif.
    if vlr_ajust > 0 and  umskz is initial.  zib_bschl =  '31'. endif.

    if vlr_ajust < 0 and  umskz is not initial.  zib_bschl =  '29'. endif.
    if vlr_ajust > 0 and  umskz is not initial.  zib_bschl =  '39'. endif.

    if wl_estorno is not initial.
      if zib_bschl = '21'.
        zib_bschl = '31'.
      elseif zib_bschl = '31'.
        zib_bschl = '21'.
      endif.

      if zib_bschl = '27'.
        zib_bschl = '34'.
      elseif zib_bschl = '34'.
        zib_bschl = '27'.
      endif.

      if zib_bschl = '29'.
        zib_bschl = '39'.
      elseif zib_bschl = '39'.
        zib_bschl = '29'.
      endif.
    endif.

  else.
    if vlr_ajust < 0 and  umskz is initial.  zib_bschl =  '01'. endif.
    if vlr_ajust > 0 and  umskz is initial.  zib_bschl =  '11'. endif.

    if vlr_ajust < 0 and  umskz is not initial.  zib_bschl =  '09'. endif.
    if vlr_ajust > 0 and  umskz is not initial.  zib_bschl =  '19'. endif.

    if wl_estorno is not initial.
      if zib_bschl = '01'.
        zib_bschl = '11'.
      elseif zib_bschl = '11'.
        zib_bschl = '01'.
      endif.

      if zib_bschl = '09'.
        zib_bschl = '19'.
      elseif zib_bschl = '19'.
        zib_bschl = '09'.
      endif.
    endif.

  endif.

endform.                    " BUSCA_CHAVE2

*&---------------------------------------------------------------------*
*&      Form  INVERTE_CHAVE
*&---------------------------------------------------------------------*
*       Inverter chave de Lançamento para Inverso de Variação Cambial
*----------------------------------------------------------------------*
*      <--ZIB  - Registro de Geração de Documentos
*----------------------------------------------------------------------*
form inverte_chave using wa_saida type ty_saida_exec changing zib type zib_contabil.

  data: v_entrada type bschl.

  write vg_first_day to zib-bldat.
  write vg_first_day to zib-budat.
  zib-gjahr = vg_first_day(4).
  zib-monat = vg_first_day+4(2).

  move zib-bschl to v_entrada.

  case zib-bschl.
      "Conta Razão """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    when '50'.
      zib-bschl = '40'.
    when '40'.
      zib-bschl = '50'.
      "Fornecedor """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    when '21'.
      zib-bschl = '31'.
    when '31'.
      zib-bschl = '21'.
    when '27'.
      zib-bschl = '34'.
    when '34'.
      zib-bschl = '27'.
    when '29'.
      zib-bschl = '39'.
    when '39'.
      zib-bschl = '29'.
      "Cliente """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    when '01'.
      zib-bschl = '11'.
    when '11'.
      zib-bschl = '01'.
    when '09'.
      zib-bschl = '19'.
    when '19'.
      zib-bschl = '09'.
    when: '06'.
      zib-bschl = '01'.
    when: '15'.
      zib-bschl = '11'.
  endcase.



  if v_entrada eq zib-bschl.
    wa_saida-icon = icon_red_light.
    concatenate text-039 zib-bschl into wa_saida-msg separated by space.
  endif.

endform.                    " INVERTE_CHAVE
*&---------------------------------------------------------------------*
*&      Form  REVERTE_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form estorna_documentos using p_tipo.

  data: w_cont     type i,
        vdata(10),
        p_erro(1),
        v_estorno  type uf05a-stgrd,
        wa_zib_chv type zib_contabil_chv,
        v_msg(60).



  vg_ryear  = s_mes-low+2(4).
  concatenate vg_ryear s_mes-low(2) '01' into vg_last_day_aux.
  vg_last_day = vg_last_day_aux.
  call function 'BKK_GET_MONTH_LASTDAY'
    exporting
      i_date = vg_last_day
    importing
      e_date = vg_last_day.

  if p_tipo = 'REV'.
    if vg_last_day_aux+0(6) = sy-datum+0(6).
      message text-041 type 'I'.
      exit.
    endif.
    add 1 to vg_last_day.
    v_estorno = '02'.
  else.
    v_estorno = '01'.
  endif.

  call function 'Z_CONTROLE_FECHAMES'
    exporting
      i_bukrs  = s_bukrs-low
      i_data   = vg_last_day
    importing
      e_status = e_status
      e_messa  = e_messa
    exceptions
      error    = 1
      others   = 2.
  if e_status = 'E'.
    message e398(00) with e_messa.
    exit.
  endif.

  if p_tipo = 'REV'.
    v_msg = text-045.
  else.
    v_msg = text-042.
  endif.
  call function 'POPUP_TO_CONFIRM'
    exporting
      text_question         = v_msg
      text_button_1         = text-036
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = text-037
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    importing
      answer                = w_answer
    exceptions
      text_not_found        = 1
      others                = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  if w_answer ne '1'.
    exit.
  endif.

  if p_raz is not initial.
    refresh tg_saida.
    loop at tg_saidar into wg_saidar where mark is not initial.
      move-corresponding wg_saidar to wg_saida.
      wg_saida-hkont = wg_saidar-racct.
      wg_saida-vbund = wg_saidar-rassc.
      append wg_saida to tg_saida.
    endloop.
  endif.


  loop at tg_saida into wg_saida where mark is not initial.
    if wg_saida-belnr is initial.
      clear: wg_saida_exec.
      wg_saida_exec-icon   = icon_yellow_light.
      wg_saida_exec-tipo   = text-m12. "'Variação'.
      wg_saida_exec-lifnr  = wg_saida-kunnr.
      wg_saida_exec-name1  = wg_saida-name1.
      wg_saida_exec-hkont  = wg_saida-hkont.
      wg_saida_exec-txt50  = wg_saida-txt50.
      wg_saida_exec-msg    = text-030 .
      append wg_saida_exec to tg_saida_exec.
    else.
      select single *
        from zib_contabil_chv
        into  wa_zib_chv
         where obj_key eq wg_saida-obj_key.
      "FB08
      select single   bukrs belnr  gjahr  budat stblg stjah
        from bkpf
        into wg_bkpf_fb08
        where bukrs eq wa_zib_chv-bukrs
        and   belnr eq wa_zib_chv-belnr
        and   gjahr eq wa_zib_chv-gjahr
        and   stblg ne ''.
      if sy-subrc = 0.
        clear: wg_saida_exec.
        wg_saida_exec-icon   = icon_yellow_light.
        wg_saida_exec-tipo   = text-m12. "'Variação'.
        wg_saida_exec-lifnr  = wg_saida-kunnr.
        wg_saida_exec-name1  = wg_saida-name1.
        wg_saida_exec-hkont  = wg_saida-hkont.
        wg_saida_exec-txt50  = wg_saida-txt50.
        wg_saida_exec-msg    = text-044 .
        append wg_saida_exec to tg_saida_exec.
      endif.
    endif.

  endloop.

  "
  if tg_saida_exec[] is not initial.
    exit.
  endif.

  loop at tg_saida into wg_saida where mark is not initial.
    select single *
        from zib_contabil_chv
        into  wa_zib_chv
         where obj_key eq wg_saida-obj_key.
    concatenate  vg_last_day+6(2) '.' vg_last_day+4(2) '.' vg_last_day+0(4) into vdata.

    refresh ti_bdcdata.
    perform f_bdc_data using:
          'SAPMF05A'  '0105'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '/00',
          ''          ''      ''   'RF05A-BELNS'      wa_zib_chv-belnr,
          ''          ''      ''   'BKPF-BUKRS'       wa_zib_chv-bukrs,
          ''          ''      ''   'RF05A-GJAHS'      wa_zib_chv-gjahr,
          ''          ''      ''   'UF05A-STGRD'      v_estorno,
          ''          ''      ''   'BSIS-BUDAT'       vdata,
          'SAPMF05A'  '0105'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '=BU'.

    clear p_erro.
    vobj_key = wg_saida-obj_key.
    perform zf_call_transaction using 'FB08' changing p_erro.

    if p_erro is not initial.
      message text-043 type 'I'.
    elseif p_raz is not initial and p_tipo = 'EST'.
      delete from zfit0082
      where bukrs   = s_bukrs-low
      and   mes_ano = s_mes-low
      and   saknr   = wg_saida-hkont
      and   vbund   = wg_saida-vbund.
    elseif p_tipo = 'EST'.
      delete from zfit0084
      where obj_key    = wg_saida-obj_key.
      commit work.
      exit.
    endif.

  endloop.
endform.

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

form zf_call_transaction using p_trans changing p_erro.
  constants: c_msgid like it_msg-msgid value 'F5',
             c_msgnr like it_msg-msgnr value '312',
             c_msgne like it_msg-msgnr value '539'.

  data: wl_cont     type sy-tabix.

  refresh: it_msg, tg_zib_err.
  clear tg_zib_err.

  wl_mode = 'E'.

  call transaction p_trans using ti_bdcdata
        mode wl_mode
        messages into it_msg.
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

      wg_zib_err-obj_key            = vobj_key.
      wg_zib_err-nr_item            = wl_cont.
      wg_zib_err-interface          = ''.
      wg_zib_err-dt_atualizacao     = sy-datum.
      wg_zib_err-hr_atualizacao     = sy-uzeit.
      wg_zib_err-type               = it_msg-msgtyp.
      wg_zib_err-id                 = it_msg-msgid.
      wg_zib_err-num                = sy-msgno.
      wg_zib_err-message            = wl_message.
      wg_zib_err-message_v1         = it_msg-msgv1.
      wg_zib_err-message_v2         = it_msg-msgv2.
      wg_zib_err-message_v3         = it_msg-msgv3.
      wg_zib_err-message_v4         = it_msg-msgv4.

      append wg_zib_err to tg_zib_err.
      clear wg_zib_err.

    endloop.

    modify zib_contabil_err from table tg_zib_err.
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

  read table it_msg with key msgid = c_msgid
                             msgnr = c_msgnr
                             msgtyp = 'S'.

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
