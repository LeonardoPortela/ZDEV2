*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio L R Silva                                       &*
*& Data.....: 21/11/18                                                &*
*& Descrição: Atualização de Partidas de apresentação                 &*
*& Transação: ZFI00                                                   &*
*&--------------------------------------------------------------------&*


REPORT  zfir071.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis, kkblo.
TABLES: zfit0084.
INCLUDE <icon>.

TYPES: BEGIN OF ty_saidar,
         mark,
         racct         TYPE faglflext-racct,
         rassc         TYPE faglflext-rassc,
         txt50         TYPE skat-txt50,
         ktoks         TYPE ska1-ktoks,
         curr1         TYPE faglflext-hslvt,
         curr2         TYPE faglflext-kslvt,
         curr3         TYPE faglflext-oslvt,
         tx_usd        TYPE zfit0082-tx_usd,
         tx_brl        TYPE zfit0082-tx_brl,
         saldo_corr    TYPE faglflext-hslvt,
         saldo_corr2   TYPE faglflext-hslvt,
         vlr_ajust     TYPE faglflext-kslvt,
         vlr_ajust2    TYPE faglflext-kslvt,
         belnr         TYPE zib_contabil_chv-belnr,
         belnr_est     TYPE zib_contabil_chv-belnr,
         belnr_rev     TYPE zib_contabil_chv-belnr,
         obj_key       TYPE zib_contabil_chv-obj_key,
         obj_key_est   TYPE zib_contabil_chv-obj_key,
         obj_key_rev   TYPE zib_contabil_chv-obj_key,
         log(4),
         line_color(4) TYPE c,
       END OF ty_saidar,

       BEGIN OF ty_saida,
         mark,
         racct         TYPE faglflext-racct,
         rassc         TYPE faglflext-rassc,
         txt50         TYPE skat-txt50,
         ktoks         TYPE ska1-ktoks,
         curr1         TYPE faglflext-hslvt,
         curr2         TYPE faglflext-kslvt,
         curr3         TYPE faglflext-oslvt,
         tx_usd        TYPE zfit0082-tx_usd,
         tx_brl        TYPE zfit0082-tx_brl,
         saldo_corr    TYPE faglflext-hslvt,
         saldo_corr2   TYPE faglflext-hslvt,
         vlr_ajust     TYPE faglflext-kslvt,
         vlr_ajust2    TYPE faglflext-kslvt,
         belnr         TYPE zib_contabil_chv-belnr,
         belnr_est     TYPE zib_contabil_chv-belnr,
         belnr_rev     TYPE zib_contabil_chv-belnr,
         obj_key       TYPE zib_contabil_chv-obj_key,
         obj_key_est   TYPE zib_contabil_chv-obj_key,
         log(4),
         line_color(4) TYPE c,

         kunnr         TYPE bsid-kunnr,
         name1         TYPE kna1-name1,
         hkont         TYPE bsid-hkont,
         vbund         TYPE bseg-vbund,
         bewar         TYPE bseg-bewar,
         obj_key_rev   TYPE zib_contabil_chv-obj_key,
       END OF ty_saida,

       BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         budat TYPE bkpf-budat,
         stblg TYPE bkpf-stblg,
         stjah TYPE bkpf-stjah,
       END OF ty_bkpf,

       BEGIN OF ty_saida_exec,
         icon(4),
         tipo    TYPE c LENGTH 30,
         lifnr   TYPE lfa1-lifnr,
         name1   TYPE lfa1-name1,
         hkont   TYPE bsik-hkont,
         racct   TYPE faglflext-racct,
         txt50   TYPE skat-txt50,
         msg(80),
       END OF ty_saida_exec.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA:
  tg_t882g       TYPE TABLE OF t882g,
  tg_t001        TYPE TABLE OF t001,
  tg_ska1        TYPE TABLE OF ska1,
  tg_skat        TYPE TABLE OF skat,
  tg_tcurr       TYPE TABLE OF tcurr,
  tg_tcurr_lday  TYPE TABLE OF tcurr,
  tg_0084        TYPE TABLE OF zfit0084,
  tg_0084_aux    TYPE TABLE OF zfit0084,
  tg_0081        TYPE TABLE OF zfit0081,
  tg_lfa1        TYPE TABLE OF lfa1,
  tg_kna1        TYPE TABLE OF kna1,
  wg_bkpf_fb08   TYPE ty_bkpf,
  wg_bkpf_fb08_e TYPE ty_bkpf,
  tg_bkpf_lanc   TYPE TABLE OF bkpf,               " Lançamento
  tg_bkpf_est    TYPE TABLE OF bkpf,               " Estorno
  tg_zib         TYPE TABLE OF zib_contabil,
  tg_zib_chv     TYPE TABLE OF zib_contabil_chv,
  tg_zib_err     TYPE TABLE OF zib_contabil_err,
  t_hkont        TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
  t_moeda        TYPE STANDARD TABLE OF  rgsb4  WITH HEADER LINE,
  tg_saida_exec  TYPE TABLE OF ty_saida_exec.

DATA:
  tg_skb1       TYPE TABLE OF skb1,
  tg_0082       TYPE TABLE OF zfit0082,
  tg_0082_aux   TYPE TABLE OF zfit0082,
  tg_0082_rev   TYPE TABLE OF zfit0082,
  tg_faglflext  TYPE TABLE OF faglflext,
  tg_saida      TYPE TABLE OF ty_saida,
  tg_saidar     TYPE TABLE OF ty_saidar,

  wg_saida      TYPE ty_saida,
  wg_saidar     TYPE ty_saidar,
  wg_0082       TYPE zfit0082,
  wg_faglflext  TYPE faglflext,
  wl_input_0082 TYPE zfit0082,
  gs_variant_c  TYPE disvariant.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wg_t882g      TYPE t882g,
  wg_t001       TYPE t001,
  wg_ska1       TYPE ska1,
  wg_skat       TYPE skat,
  wg_0084       TYPE zfit0084,
  wg_tcurr      TYPE tcurr,
  wg_tcurr_lday TYPE tcurr,
  wg_lfa1       TYPE lfa1,
  wg_kna1       TYPE kna1,
  wg_0081       TYPE zfit0081,
  wg_zib_chv    TYPE zib_contabil_chv,
  wg_zib_err    TYPE zib_contabil_err,

  wg_bkpf       TYPE bkpf,
  wg_bseg       TYPE bseg,

  wg_bkpfe      TYPE bkpf,

  wg_saida_exec TYPE ty_saida_exec.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      v_moedad     TYPE bkpf-waers,
      v_moedap     TYPE bkpf-waers,
      v_carga(1),
      t_top        TYPE slis_t_listheader,
      lt_sort      TYPE slis_t_sortinfo_alv,
      ls_sort      TYPE slis_sortinfo_alv,
      tabix        TYPE sy-tabix,
      tabix2       TYPE sy-tabix.
*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
DATA: vg_ryear            TYPE faglflext-ryear,
      vg_rmonth           TYPE char02,
      vg_last_day         TYPE sy-datum,
      vg_first_day        TYPE sy-datum,
      w_answer(1),
      vg_last_day_aux(8),
      vg_last_day_aux2(8),
      v_vbund             TYPE bseg-vbund,
      v_bewar             TYPE bseg-bewar,
      v_saknr             TYPE zfit0081-saknr,
      e_status(1),
      e_messa(64).


DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

DATA: ti_bdcdata       TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata       LIKE LINE OF ti_bdcdata,
      t_messtab        TYPE TABLE OF bdcmsgcoll,
      vobj_key         TYPE zib_contabil_err-obj_key,
      wl_message       TYPE pmst_raw_message,
      wg_documento(10),
      wl_mode(1),
      it_selection     TYPE TABLE OF rsparams,
      wa_selection     LIKE LINE OF it_selection.
*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: c_x(1) TYPE c VALUE 'X'.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE acao.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = &1
    EXCEPTIONS
      function_not_supported = 1.

END-OF-DEFINITION.
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS
      on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.
  PRIVATE SECTION.
ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
* TELA DE SELECAO.
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-h01.


  SELECT-OPTIONS: s_bukrs FOR zfit0084-bukrs OBLIGATORY NO INTERVALS NO-EXTENSION,
                  s_mes   FOR zfit0084-mes_ano NO INTERVALS NO-EXTENSION OBLIGATORY.

  SELECT-OPTIONS s_saknrs FOR zfit0084-saknr.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(1) TEXT-su3.
  SELECTION-SCREEN END OF LINE.


  SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-h02.


    PARAMETERS: p_proc RADIOBUTTON GROUP g2,
                p_reve RADIOBUTTON GROUP g2,
                p_visu RADIOBUTTON GROUP g2.

  SELECTION-SCREEN: END OF BLOCK b3.
SELECTION-SCREEN: END OF BLOCK b1.

INITIALIZATION.
  SELECT SINGLE *
     FROM usr05
     INTO @DATA(_usr05)
     WHERE bname = @sy-uname
     AND parid   = 'BUK'.
  IF sy-subrc = 0.
    s_bukrs-sign    = 'I'.
    s_bukrs-option  = 'EQ'.
    s_bukrs-low = _usr05-parva+0(4).
    APPEND s_bukrs.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD on_button_click.
    DATA: tl_texto    TYPE catsxt_longtext_itab,
          wl_texto    TYPE LINE OF catsxt_longtext_itab,
          it_zib_err  TYPE TABLE OF zib_contabil_err,
          wl_text     TYPE sytitle,
          wl_text_est TYPE sytitle.

    REFRESH: tl_texto.
    CLEAR: wl_texto.
    READ TABLE  tg_saidar INTO  wg_saidar INDEX es_row_no-row_id.
    IF es_col_id EQ 'LOG'.
**       Contabilização
      IF p_reve IS INITIAL.
        IF wg_saidar-obj_key IS NOT INITIAL.
          SELECT *
            FROM zib_contabil_err
            INTO TABLE it_zib_err
            WHERE obj_key EQ wg_saidar-obj_key.

          LOOP AT it_zib_err INTO wg_zib_err  WHERE obj_key EQ wg_saidar-obj_key.

            wl_texto = wg_zib_err-message.

            APPEND wl_texto TO tl_texto.
            CLEAR: wl_texto.
          ENDLOOP.
          IF tl_texto[] IS NOT INITIAL.
            wl_text = TEXT-001.
            CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
              EXPORTING
                im_title        = wl_text           "" Título
                im_display_mode = c_x
              CHANGING
                ch_text         = tl_texto.
          ENDIF.
        ENDIF.
      ELSE.

        IF wg_saidar-obj_key_rev IS NOT INITIAL.
          SELECT *
            FROM zib_contabil_err
            INTO TABLE it_zib_err
            WHERE obj_key EQ wg_saidar-obj_key_rev.

          LOOP AT it_zib_err INTO wg_zib_err  WHERE obj_key EQ wg_saidar-obj_key_rev.

            wl_texto = wg_zib_err-message.

            APPEND wl_texto TO tl_texto.
            CLEAR: wl_texto.
          ENDLOOP.
          IF tl_texto[] IS NOT INITIAL.
            wl_text = TEXT-001.
            CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
              EXPORTING
                im_title        = wl_text           "" Título
                im_display_mode = c_x
              CHANGING
                ch_text         = tl_texto.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.



  ENDMETHOD.                    "ON_BUTTON_CLICK
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

INITIALIZATION.       "to set titlebar on selection screen

START-OF-SELECTION.

  PERFORM selecionar_dados_r.
  PERFORM iniciar_variaveis.
  PERFORM organizacao_dados_r.
  PERFORM imprimir_dados.

*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM iniciar_variaveis.

  CLEAR: wg_t001.
  READ TABLE tg_t001 INTO wg_t001 INDEX 1.

  PERFORM f_construir_cabecalho USING 'H' TEXT-056.


ENDFORM.                    " INICIAR_VARIAVES

FORM xuser_commandr USING ucomm LIKE sy-ucomm
                         selfield TYPE kkblo_selfield.
  DATA: tl_zib      TYPE TABLE OF zib_contabil WITH HEADER LINE,
        tl_zib_chv  TYPE TABLE OF zib_contabil_chv WITH HEADER LINE,
        tl_zib_err  TYPE TABLE OF zib_contabil_err WITH HEADER LINE,
        tl_saida    TYPE TABLE OF ty_saidar,
        wl_obj_key  TYPE zib_contabil_chv-obj_key,
        wa_zfit0082 TYPE zfit0082.

  REFRESH: tg_saida_exec, tl_zib, tl_zib_chv, tl_zib_err, tl_saida.
  CLEAR:wl_obj_key.

  tl_saida[]  = tg_saidar[].
  DELETE tl_saida WHERE obj_key IS INITIAL.
  IF tl_saida[] IS NOT INITIAL.
    SELECT *
      FROM zib_contabil
      INTO TABLE tl_zib
       FOR ALL ENTRIES IN tl_saida
       WHERE obj_key EQ tl_saida-obj_key.
  ENDIF.


  IF tl_zib[] IS NOT INITIAL.
    SELECT *
     FROM zib_contabil_chv
     INTO TABLE tl_zib_chv
      FOR ALL ENTRIES IN tl_zib
      WHERE obj_key EQ tl_zib-obj_key.

    SELECT *
     FROM zib_contabil_err
     INTO TABLE tl_zib_err
      FOR ALL ENTRIES IN tl_zib
      WHERE obj_key EQ tl_zib-obj_key.

  ENDIF.

  SORT: tl_zib     BY obj_key,
        tl_zib_chv BY obj_key,
        tl_zib_err BY obj_key.

  CASE ucomm.
    WHEN 'GERA'.

      LOOP AT tg_saidar INTO wg_saidar
        WHERE mark IS NOT INITIAL.
        IF wg_saidar-obj_key IS NOT INITIAL AND wg_saidar-belnr IS INITIAL.
          READ TABLE tl_zib WITH KEY obj_key = wg_saidar-obj_key BINARY SEARCH.
          IF tl_zib-rg_atualizado EQ 'N'.
            CLEAR: wg_saida_exec.
            wg_saida_exec-icon   = icon_yellow_light.
            wg_saida_exec-racct  = wg_saidar-racct.
            wg_saida_exec-txt50  = wg_saidar-txt50.
            wg_saida_exec-msg    = TEXT-m01 .
            APPEND wg_saida_exec TO tg_saida_exec.
            CONTINUE.
          ELSEIF tl_zib-rg_atualizado EQ 'S'.
            READ TABLE tl_zib_chv WITH KEY obj_key = wg_saidar-obj_key BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              CLEAR: wg_saida_exec.
              wg_saida_exec-icon   = icon_green_light.
              wg_saida_exec-racct  = wg_saidar-racct.
              wg_saida_exec-txt50  = wg_saidar-txt50.
              wg_saida_exec-msg    = TEXT-m02 .
              APPEND wg_saida_exec TO tg_saida_exec.
              acao '&ATUAL'.
              CONTINUE.
            ELSE.
              READ TABLE tl_zib_err  WITH KEY obj_key = wg_saidar-obj_key BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                CLEAR: wg_saida_exec.
                wg_saida_exec-icon   = icon_red_light.
                wg_saida_exec-racct  = wg_saidar-racct.
                wg_saida_exec-txt50  = wg_saidar-txt50.
                wg_saida_exec-msg    = TEXT-m03 .
                APPEND wg_saida_exec TO tg_saida_exec.

                CALL FUNCTION 'POPUP_TO_CONFIRM'
                  EXPORTING
                    text_question         = TEXT-m04
*                   TEXT_BUTTON_1         = 'Sim'(100)
                    text_button_1         = TEXT-b01
                    icon_button_1         = 'ICON_OKAY'
*                   TEXT_BUTTON_2         = 'Não'(101)
                    text_button_2         = TEXT-b02
                    icon_button_2         = 'ICON_CANCEL'
                    default_button        = '1'
                    display_cancel_button = ' '
                    start_column          = 25
                    start_row             = 6
                  IMPORTING
                    answer                = w_answer
                  EXCEPTIONS
                    text_not_found        = 1
                    OTHERS                = 2.

                IF sy-subrc <> 0.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ENDIF.
                IF w_answer = '1'.
                  DELETE FROM zib_contabil_err WHERE obj_key = wg_saidar-obj_key.
                  DELETE FROM zib_contabil     WHERE obj_key = wg_saidar-obj_key.
                  DELETE FROM zfit0082         WHERE obj_key = wg_saidar-obj_key.
                ELSE.
                  acao '&ATUAL'.
                  CONTINUE.
                ENDIF.
              ELSE.
                CLEAR: wg_saida_exec.
                wg_saida_exec-icon   = icon_yellow_light.
                wg_saida_exec-racct  = wg_saidar-racct.
                wg_saida_exec-txt50  = wg_saidar-txt50.
                wg_saida_exec-msg    = TEXT-m01 .
                APPEND wg_saida_exec TO tg_saida_exec.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        IF wg_saidar-obj_key IS INITIAL.
          SELECT SINGLE *
            FROM zfit0082
            INTO wa_zfit0082
            WHERE bukrs     = s_bukrs-low
            AND   mes_ano   = s_mes-low
            AND   saknr     = wg_saidar-racct
            AND   vbund     = wg_saidar-rassc
            AND   obj_key   NE ''.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.

        ENDIF.

        IF wg_saidar-vlr_ajust = 0.
          CLEAR: wg_saida_exec.
          wg_saida_exec-icon   = icon_red_light.
          wg_saida_exec-racct  = wg_saidar-racct.
          wg_saida_exec-txt50  = wg_saidar-txt50.
          wg_saida_exec-msg    = TEXT-m11 .
          APPEND wg_saida_exec TO tg_saida_exec.
          CONTINUE.
        ENDIF.

        IF wg_saidar-belnr IS NOT INITIAL.
          CLEAR: wg_saida_exec.
          wg_saida_exec-icon   = icon_red_light.
          wg_saida_exec-racct  = wg_saidar-racct.
          wg_saida_exec-txt50  = wg_saidar-txt50.
          wg_saida_exec-msg    = TEXT-m05 .
          APPEND wg_saida_exec TO tg_saida_exec.
          CONTINUE.
        ENDIF.
        CLEAR: wl_obj_key.
        PERFORM gera_contabilr USING wg_saidar space CHANGING wl_obj_key.

        wg_saidar-obj_key = wl_obj_key.
        CLEAR: wg_saidar-obj_key_est, wg_saidar-belnr_est.
        MODIFY tg_saidar FROM wg_saidar TRANSPORTING obj_key obj_key_est belnr_est.
      ENDLOOP.

      selfield-col_stable = c_x.
      selfield-row_stable = c_x.
      selfield-refresh = c_x.

      PERFORM imprimir_exec.
      acao '&ATUAL'.
    WHEN 'ESTORNO'.
      PERFORM estorna_documentos USING 'EST'.
      selfield-col_stable = c_x.
      selfield-row_stable = c_x.
      selfield-refresh    = c_x.
      PERFORM imprimir_exec.
    WHEN 'REVERTE'.
*      PERFORM estorna_documentos USING 'REV'.
      "USER STORY 75855 / ABAP - AOENNING
*      PERFORM gerar_rev.
      PERFORM exect_reversao.
      "USER STORY 75855 / ABAP - AOENNING
      selfield-col_stable = c_x.
      selfield-row_stable = c_x.
      selfield-refresh    = c_x.
      PERFORM imprimir_exec.
      acao '&ATUAL'.
    WHEN '&IC1'.
* Lê na tabela de saída
      READ TABLE tg_saidar INTO wg_saidar INDEX selfield-tabindex.

      IF sy-subrc EQ 0.

        IF selfield-fieldname = 'BELNR'.
          IF wg_saidar-belnr IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wg_saidar-belnr.
            SET PARAMETER ID 'GJR' FIELD wg_saidar-obj_key+16(4).
            SET PARAMETER ID 'BUK' FIELD s_bukrs-low.

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
        IF selfield-fieldname = 'BELNR_REV'.
          IF wg_saidar-belnr_rev IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wg_saidar-belnr_rev.
            SET PARAMETER ID 'GJR' FIELD wg_saidar-obj_key_rev+16(4).
            SET PARAMETER ID 'BUK' FIELD s_bukrs-low.

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '&ATUAL'.
      PERFORM atualiza_saidar TABLES tg_saidar
                                    tl_zib
                                    tl_zib_chv
                                    tl_zib_err.
  ENDCASE.

  selfield-col_stable = c_x.
  selfield-row_stable = c_x.
  selfield-refresh = c_x.
ENDFORM. "XUSER_COMMAND

*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xpf_status_set USING e_comm.                           "#EC CALLED
  DATA: gr_events      TYPE REF TO lcl_event_receiver,
        ck_atualiza(1).
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.
  DATA : ls_sel_hide            TYPE slis_sel_hide_alv.
  DATA: ref1             TYPE REF TO cl_gui_alv_grid,
        tl_fieldcatalog  TYPE lvc_t_fcat,
        tl_fieldcatalog2 TYPE lvc_t_fcat,
        wl_fieldcatalog  TYPE lvc_s_fcat,
        wl_fieldcatalog2 TYPE lvc_s_fcat,
        is_table         TYPE lvc_s_stbl.

  IF v_carga IS INITIAL.
    v_carga = 'X'.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        es_sel_hide = ls_sel_hide
        e_grid      = ref1.

    CREATE OBJECT gr_events.
    CALL METHOD ref1->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = tl_fieldcatalog.

    LOOP AT tl_fieldcatalog INTO wl_fieldcatalog.
      tabix = sy-tabix.
      READ TABLE estrutura INTO wa_estrutura WITH KEY fieldname = wl_fieldcatalog-fieldname.
      IF sy-subrc = 0.
        wl_fieldcatalog-col_pos = wa_estrutura-col_pos.
        MODIFY tl_fieldcatalog FROM wl_fieldcatalog INDEX tabix.
      ENDIF.
    ENDLOOP.
    SORT tl_fieldcatalog BY col_pos.

    ck_atualiza  = abap_false.

    READ TABLE tl_fieldcatalog INTO wl_fieldcatalog WITH KEY fieldname = 'LOG'.
    IF sy-subrc IS INITIAL.
      wl_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
      MODIFY tl_fieldcatalog FROM wl_fieldcatalog INDEX sy-tabix TRANSPORTING style edit.
      CALL METHOD ref1->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = tl_fieldcatalog.
      is_table-row = 'X'.
      is_table-col = 'X'.
      ck_atualiza  = abap_true.
    ENDIF.

    READ TABLE tl_fieldcatalog INTO wl_fieldcatalog WITH KEY fieldname = 'LOG_INV'.
    IF sy-subrc IS INITIAL.
      wl_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
      MODIFY tl_fieldcatalog FROM wl_fieldcatalog INDEX sy-tabix TRANSPORTING style edit.
      CALL METHOD ref1->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = tl_fieldcatalog.
      is_table-row = 'X'.
      is_table-col = 'X'.
      ck_atualiza  = abap_true.
    ENDIF.

    IF ck_atualiza EQ abap_true.
      CALL METHOD ref1->refresh_table_display
        EXPORTING
          is_stable      = is_table
          i_soft_refresh = 'X'.
    ENDIF.

    SET HANDLER lcl_event_receiver=>on_button_click FOR ref1.
  ENDIF.

  IF p_visu IS NOT INITIAL.
    APPEND 'GERA' TO fcode.
    APPEND 'ESTORNO' TO fcode.
    APPEND 'REVERTE' TO fcode.
  ELSE.
    IF p_proc IS INITIAL.
      APPEND 'GERA' TO fcode.
    ENDIF.

    IF p_reve IS INITIAL.
      APPEND 'REVERTE' TO fcode.
    ELSE.
      APPEND 'ESTORNO' TO fcode.
    ENDIF.
  ENDIF.



  SET PF-STATUS 'STATUS_UNI'  EXCLUDING fcode.

ENDFORM. "XPF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.
  DATA: cabec TYPE string.
  IF p_proc = 'X'.
    CONCATENATE TEXT-066 text INTO cabec SEPARATED BY space.
  ELSEIF p_reve = 'X'.
    CONCATENATE TEXT-067 text INTO cabec SEPARATED BY space.
  ELSE.
    CONCATENATE TEXT-068 text INTO cabec SEPARATED BY space.
  ENDIF.
  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = cabec.
  APPEND ls_line TO t_top.


  ls_line-typ = 'S'.
  IF wg_t001-land1 EQ 'BR' OR wg_t001-land1 EQ 'NL' OR  wg_t001-land1 EQ 'CH'.
*    LS_LINE-KEY = 'Empresa:'.
    ls_line-key = TEXT-008.
    CONCATENATE  wg_t001-bukrs '-' wg_t001-butxt INTO ls_line-info SEPARATED BY space.
  ELSEIF wg_t001-land1 EQ 'AR'
      OR wg_t001-land1 EQ 'PY'.
*    LS_LINE-KEY = 'Sociedad:'.
    ls_line-key = TEXT-009.
    CONCATENATE  wg_t001-bukrs '-' wg_t001-butxt INTO ls_line-info SEPARATED BY space.
  ENDIF.
  APPEND ls_line TO t_top.

  IF wg_t001-land1 EQ 'BR' OR wg_t001-land1 EQ 'NL' OR  wg_t001-land1 EQ 'CH'.
*    LS_LINE-KEY = 'Mês/Ano:'.
    ls_line-key = TEXT-010.
    CONCATENATE  s_mes-low(2) '/' s_mes-low+2(4)  INTO ls_line-info SEPARATED BY space.
  ELSEIF wg_t001-land1 EQ 'AR'
      OR wg_t001-land1 EQ 'PY'.
*    LS_LINE-KEY = 'Ejercicio:'.
    ls_line-key = TEXT-011.
    CONCATENATE  s_mes-low(2) '/' s_mes-low+2(4)  INTO ls_line-info SEPARATED BY space.
  ENDIF.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_dados .
  DATA: wl_layout TYPE slis_layout_alv,
        w_repid   TYPE sy-repid.

  PERFORM definir_eventos.


  PERFORM montar_layout USING 'TG_SAIDAR'.

*
  CLEAR       v_carga.
  IF p_proc IS NOT INITIAL OR p_reve IS NOT INITIAL.
    wl_layout-box_fieldname = 'MARK'.
  ENDIF.
  wl_layout-info_fieldname    = 'LINE_COLOR'.
  CONCATENATE sy-repid 'R' INTO w_repid.
  gs_variant_c-report      = w_repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      is_variant         = gs_variant_c
      i_callback_program = sy-repid
      it_fieldcat        = estrutura[]
      is_layout          = wl_layout
      i_save             = 'X'
      it_events          = events
      is_print           = t_print
      it_sort            = lt_sort
    TABLES
      t_outtab           = tg_saidar.

ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.

  IF p_proc IS NOT INITIAL OR p_reve IS NOT INITIAL.

    PERFORM f_carregar_eventos USING:
                               slis_ev_user_command  'XUSER_COMMANDR', "para tira duplo click
                               slis_ev_pf_status_set 'XPF_STATUS_SET',
                               slis_ev_top_of_page   'XTOP_OF_PAGE'.
  ELSE.
    PERFORM f_carregar_eventos USING:
                                     slis_ev_pf_status_set 'XPF_STATUS_SET',
                                     slis_ev_top_of_page   'XTOP_OF_PAGE'.
  ENDIF.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout USING p_tabname.
  DATA: wl_curr1_aux  TYPE dd03p-scrtext_l,
        wl_curr2_aux  TYPE dd03p-scrtext_l,
        wl_curr3_aux  TYPE dd03p-scrtext_l,
        wl_saldo_cor  TYPE dd03p-scrtext_l,
        wl_saldo_cor2 TYPE dd03p-scrtext_l,
        wl_vlr_ajst   TYPE dd03p-scrtext_l,
        wl_vlr_ajst2  TYPE dd03p-scrtext_l.


  REFRESH estrutura.
  CLEAR: wg_t001.
  READ TABLE tg_t001 INTO wg_t001 INDEX 1.

  IF p_tabname EQ 'TG_SAIDAR'.
    READ TABLE tg_t882g INTO wg_t882g
      WITH KEY rbukrs =  s_bukrs-low
               BINARY SEARCH.


    CONCATENATE TEXT-060 wg_t882g-curr1 INTO wl_curr1_aux SEPARATED BY space.
    CONCATENATE TEXT-060 wg_t882g-curr2 INTO wl_curr2_aux SEPARATED BY space.
    CONCATENATE TEXT-061 v_moedad INTO wl_saldo_cor SEPARATED BY space.
    CONCATENATE TEXT-062 v_moedad INTO wl_vlr_ajst SEPARATED BY space.

    PERFORM montar_estrutura USING:
              1  'FAGLFLEXT'         'RACCT'          'TG_SAIDAR' 'RACCT'                   TEXT-a25                   ' ' ,
              1  'FAGLFLEXT'         'RASSC'          'TG_SAIDAR' 'RASSC'                   TEXT-a26                   ' ' ,
              2  'SKAT'              'TXT50'          'TG_SAIDAR' 'TXT50'                   TEXT-a27                   ' ' ,
              3  'SKA1'              'KTOKS'          'TG_SAIDAR' 'KTOKS'                   TEXT-a28                   ' ' ,
              4  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'CURR1'                   wl_curr1_aux              '15' ,
              5  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'CURR2'                   wl_curr2_aux              '15' ,
              6  'ZFIT0082'          'TX_USD'         'TG_SAIDAR' 'TX_USD'                  TEXT-a29                  '10' ,
              7  'FAGLFLEXT'         'HSLVT'          'TG_SAIDAR' 'SALDO_CORR'              wl_saldo_cor              '15' ,
              8  'FAGLFLEXT'         'KSLVT'          'TG_SAIDAR' 'VLR_AJUST'               wl_vlr_ajst               '15' ,
              9  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDAR' 'BELNR'                   TEXT-a30                  ' ' ,
             10  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDAR' 'BELNR_REV'               TEXT-a33                  ' ' ,
             11  ' '                 ' '              'TG_SAIDAR' 'LOG'                     TEXT-a14                  ' ' .

  ELSEIF p_tabname EQ 'TG_SAIDA_EXEC'.


    PERFORM montar_estrutura USING:
        1  ' '          ' '                'TG_SAIDA_EXEC' 'ICON'      TEXT-a23           ' ',
        2  ' '          ' '                'TG_SAIDA_EXEC' 'MSG'       TEXT-a04           '80',
        3  'FAGLFLEXT'  'RACCT'            'TG_SAIDA_EXEC' 'RACCT'     TEXT-a25           ' ',
        4  'SKAT'       'TXT50'            'TG_SAIDA_EXEC' 'TXT50'     TEXT-a27           ' '.



  ENDIF.

ENDFORM.                    " MONTAR_LAYOUT
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
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            p_scrtext_l
*                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(p_outputlen).

  CLEAR wa_estrutura.
  DATA: t_ska1 TYPE TABLE OF ska1.

  IF p_field EQ 'BELNR'
  OR p_field EQ 'BELNR_EST'
  OR p_field EQ 'BELNR_INV'
  OR p_field EQ 'BELNR_REV'
  OR p_field EQ 'BELNR_INV_EST'
  OR p_field EQ 'BELNR2'.
    wa_estrutura-just = 'C'.
    wa_estrutura-hotspot = c_x.
  ENDIF.


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

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA
FORM selecionar_dados_r.
  DATA: wl_tabix TYPE sy-tabix.
  DATA: t_ska1 TYPE TABLE OF ska1.
  DATA: r_saknr TYPE RANGE OF saknr.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZFI0111_MOE'
    TABLES
      set_values    = t_moeda
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT t_moeda BY from.
  CLEAR: v_moedad.
  LOOP AT t_moeda.
    IF t_moeda-from+0(4) = s_bukrs-low.
      v_moedad = t_moeda-from+5(3).
      v_moedap = t_moeda-from+9(3).
    ENDIF.
  ENDLOOP.

  IF v_moedad IS INITIAL.
    MESSAGE 'Não existe moeda de apresentação cadastrada!' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT *
  FROM zfit0081
  INTO TABLE tg_0081
   WHERE cta_monet EQ 'N'
   AND   bukrs IN s_bukrs.

  "Inclusão CS2020001220 - 15/10/2020 - AOENNING.
  "=======================================================================================
  IF tg_0081 IS NOT INITIAL.
    FREE t_ska1.
    SELECT * FROM ska1                 "#EC CI_DB_OPERATION_OK[2389136]
    INTO TABLE t_ska1                  "#EC CI_DB_OPERATION_OK[2431747]
    FOR ALL ENTRIES IN tg_0081
      WHERE saknr EQ tg_0081-conta_de
        AND ktopl EQ '0050'
        AND ktoks IN ('YB05', 'YB07', 'YB08', 'YB06').
    IF t_ska1 IS NOT INITIAL.
      "Deletar contas da tabela tg_0081.
      r_saknr = VALUE #( FOR s IN t_ska1 ( sign = 'I' option = 'EQ' low = s-saknr ) ).
      IF r_saknr IS NOT INITIAL.
        SORT tg_0081 BY  conta_de.
        DELETE tg_0081 WHERE conta_de IN r_saknr.
      ENDIF.
    ENDIF.
  ENDIF.
  "==========================================================================================

** Processamento
  IF p_proc IS NOT INITIAL OR p_reve IS NOT INITIAL.
    vg_ryear  = s_mes-low+2(4).
    CONCATENATE vg_ryear s_mes-low(2) '01' INTO vg_last_day_aux.
    vg_last_day = vg_last_day_aux.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = vg_last_day
      IMPORTING
        e_date = vg_last_day.

    CONVERT INVERTED-DATE vg_last_day INTO DATE vg_last_day_aux2.

*    IF  NOT '0101_0200' CS S_BUKRS-LOW.
    ADD 1 TO vg_last_day.
*    ENDIF.


    CONVERT INVERTED-DATE vg_last_day INTO DATE vg_last_day_aux.

*    IF  NOT '0101_0200' CS S_BUKRS-LOW.
    SUBTRACT 1 FROM vg_last_day.
*    ENDIF.

    vg_first_day = vg_last_day.

    ADD 1 TO vg_first_day.
    IF sy-subrc NE 0.
      SELECT *                         "#EC CI_DB_OPERATION_OK[2431747]
           FROM t882g
           INTO TABLE tg_t882g
            WHERE rbukrs IN s_bukrs.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM t001
          INTO TABLE tg_t001
           FOR ALL ENTRIES IN tg_t882g
           WHERE bukrs EQ tg_t882g-rbukrs.
      ENDIF.
      EXIT.
    ENDIF.

    IF sy-subrc IS INITIAL.

      SELECT *                         "#EC CI_DB_OPERATION_OK[2431747]
            FROM t882g
            INTO TABLE tg_t882g
             WHERE rbukrs IN s_bukrs.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM t001
          INTO TABLE tg_t001
           FOR ALL ENTRIES IN tg_t882g
           WHERE bukrs EQ tg_t882g-rbukrs.

        IF sy-subrc IS INITIAL.
          SELECT *                     "#EC CI_DB_OPERATION_OK[2389136]
            FROM ska1                  "#EC CI_DB_OPERATION_OK[2431747]
            INTO TABLE tg_ska1
             FOR ALL ENTRIES IN tg_0081
             WHERE ktopl EQ '0050'
               AND saknr EQ tg_0081-conta_de
               AND saknr IN s_saknrs.

          IF sy-subrc IS INITIAL.
            SELECT *
              FROM skat
              INTO TABLE tg_skat
               FOR ALL ENTRIES IN tg_ska1
               WHERE saknr EQ tg_ska1-saknr
               AND ktopl EQ '0050'
               AND spras EQ sy-langu.

            SELECT *                   "#EC CI_DB_OPERATION_OK[2431747]
              FROM skb1
              INTO TABLE tg_skb1
               FOR ALL ENTRIES IN tg_ska1
               WHERE saknr EQ tg_ska1-saknr
                 AND bukrs IN s_bukrs.

            IF sy-subrc IS INITIAL.
              SELECT *
                FROM zfit0082
                INTO TABLE tg_0082
                 FOR ALL ENTRIES IN tg_skb1
                 WHERE bukrs   IN s_bukrs
                   AND mes_ano IN s_mes
                   AND saknr   EQ tg_skb1-saknr.


              IF sy-subrc IS INITIAL.
                REFRESH: tg_0082_aux.
                tg_0082_aux[] = tg_0082[].
                tg_0082_rev[] = tg_0082[].
                DELETE tg_0082_aux WHERE obj_key IS INITIAL.
                DELETE tg_0082_rev WHERE obj_key IS INITIAL.

                IF tg_0082_aux[] IS NOT INITIAL OR tg_0082_rev[] IS NOT INITIAL.
                  SELECT *
                    FROM zib_contabil
                    INTO TABLE tg_zib
                     FOR ALL ENTRIES IN tg_0082_aux
                     WHERE obj_key EQ tg_0082_aux-obj_key.

                  SELECT *
                    FROM zib_contabil_chv
                    INTO TABLE tg_zib_chv
                     FOR ALL ENTRIES IN tg_0082_aux
                     WHERE obj_key EQ tg_0082_aux-obj_key.

                  SELECT *
                    FROM zib_contabil_err
                    INTO TABLE tg_zib_err
                     FOR ALL ENTRIES IN tg_0082_aux
                     WHERE obj_key EQ tg_0082_aux-obj_key.
                ENDIF.

**             Estorno
                REFRESH: tg_0082_aux.
                tg_0082_aux[] = tg_0082[].
                tg_0082_rev[] = tg_0082[].
                DELETE tg_0082_aux WHERE obj_key_est IS INITIAL.
                DELETE tg_0082_rev WHERE obj_key_rev IS INITIAL.

                IF tg_0082_aux[] IS NOT INITIAL.
                  SELECT *
                    FROM zib_contabil
                    APPENDING TABLE tg_zib
                     FOR ALL ENTRIES IN tg_0082_aux
                     WHERE obj_key EQ tg_0082_aux-obj_key_est.

                  SELECT *
                    FROM zib_contabil_chv
                    APPENDING TABLE tg_zib_chv
                     FOR ALL ENTRIES IN tg_0082_aux
                     WHERE obj_key EQ tg_0082_aux-obj_key_est.

                  SELECT *
                    FROM zib_contabil_err
                    APPENDING TABLE tg_zib_err
                     FOR ALL ENTRIES IN tg_0082_aux
                     WHERE obj_key EQ tg_0082_aux-obj_key_est.
                ENDIF.

****                "Doc reversão.
                IF tg_0082_rev[] IS NOT INITIAL.
                  SELECT *
                    FROM zib_contabil
                    APPENDING TABLE tg_zib
                     FOR ALL ENTRIES IN tg_0082_rev
                     WHERE obj_key EQ tg_0082_rev-obj_key_rev.

                  SELECT *
                    FROM zib_contabil_chv
                    APPENDING TABLE tg_zib_chv
                     FOR ALL ENTRIES IN tg_0082_rev
                     WHERE obj_key EQ tg_0082_rev-obj_key_rev.

                  SELECT *
                    FROM zib_contabil_err
                    APPENDING TABLE tg_zib_err
                     FOR ALL ENTRIES IN tg_0082_rev
                     WHERE obj_key EQ tg_0082_rev-obj_key_rev.
                ENDIF.
              ENDIF.

              IF tg_skb1[] IS NOT INITIAL.
                SELECT *
                  FROM faglflext
                  INTO TABLE tg_faglflext
                   FOR ALL ENTRIES IN tg_skb1
                   WHERE ryear  EQ vg_ryear
                     AND rbukrs IN s_bukrs
                     AND racct  EQ tg_skb1-saknr
                     AND rldnr  EQ '0L'.

                SELECT *
                  FROM tcurr
                  INTO TABLE tg_tcurr
                   WHERE gdatu EQ vg_last_day_aux
                     AND kurst EQ 'B'
                     AND fcurr EQ v_moedad
                     AND tcurr EQ v_moedap.

              ENDIF.

            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

** Visualização
  ELSE.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE tg_0082
      FROM zfit0082
      INNER JOIN skb1 ON skb1~bukrs = zfit0082~bukrs "#EC CI_DB_OPERATION_OK[2431747]
                     AND skb1~saknr = zfit0082~saknr
                     AND skb1~mitkz = ''

       WHERE zfit0082~bukrs   IN s_bukrs
         AND zfit0082~mes_ano IN s_mes
         AND zfit0082~saknr   IN s_saknrs.

    IF sy-subrc IS INITIAL.
      SELECT *                         "#EC CI_DB_OPERATION_OK[2431747]
        FROM t882g
        INTO TABLE tg_t882g
         WHERE rbukrs IN s_bukrs.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM t001
          INTO TABLE tg_t001
           FOR ALL ENTRIES IN tg_t882g
           WHERE bukrs EQ tg_t882g-rbukrs.
      ENDIF.
      SELECT *                         "#EC CI_DB_OPERATION_OK[2389136]
        FROM ska1                      "#EC CI_DB_OPERATION_OK[2431747]
    INTO TABLE tg_ska1
         FOR ALL ENTRIES IN tg_0082
         WHERE ktopl EQ '0050'
           AND saknr EQ tg_0082-saknr.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM skat
          INTO TABLE tg_skat
           FOR ALL ENTRIES IN tg_ska1
           WHERE saknr EQ tg_ska1-saknr
           AND ktopl EQ '0050'
           AND spras EQ sy-langu.
      ENDIF.

      SELECT *
          FROM t001
          INTO TABLE tg_t001
           FOR ALL ENTRIES IN tg_0082
           WHERE bukrs EQ tg_0082-bukrs.

      REFRESH: tg_0082_aux.
      tg_0082_aux[] = tg_0082[].
      DELETE tg_0082_aux WHERE obj_key IS INITIAL.
      IF tg_0082_aux[] IS NOT INITIAL.
        SELECT *
          FROM zib_contabil
          INTO TABLE tg_zib
           FOR ALL ENTRIES IN tg_0082_aux
           WHERE obj_key EQ tg_0082_aux-obj_key.

        SELECT *
          FROM zib_contabil_chv
          INTO TABLE tg_zib_chv
           FOR ALL ENTRIES IN tg_0082_aux
           WHERE obj_key EQ tg_0082_aux-obj_key.

        SELECT *
          FROM zib_contabil_err
          INTO TABLE tg_zib_err
           FOR ALL ENTRIES IN tg_0082_aux
           WHERE obj_key EQ tg_0082_aux-obj_key.
      ENDIF.
**             Estorno
      REFRESH: tg_0082_aux.
      tg_0082_aux[] = tg_0082[].
      tg_0082_rev[] = tg_0082[].
      DELETE tg_0082_aux WHERE obj_key_est IS INITIAL.
      DELETE tg_0082_rev WHERE obj_key_rev IS INITIAL.
      IF tg_0082_aux[] IS NOT INITIAL.
        SELECT *
          FROM zib_contabil
          APPENDING TABLE tg_zib
           FOR ALL ENTRIES IN tg_0082_aux
           WHERE obj_key EQ tg_0082_aux-obj_key_est.

        SELECT *
          FROM zib_contabil_chv
          APPENDING TABLE tg_zib_chv
           FOR ALL ENTRIES IN tg_0082_aux
           WHERE obj_key EQ tg_0082_aux-obj_key_est.

        SELECT *
          FROM zib_contabil_err
          APPENDING TABLE tg_zib_err
           FOR ALL ENTRIES IN tg_0082_aux
           WHERE obj_key EQ tg_0082_aux-obj_key_est.
      ENDIF.

      "Doc reversão.
      IF tg_0082_rev[] IS NOT INITIAL.
        SELECT *
          FROM zib_contabil
          APPENDING TABLE tg_zib
           FOR ALL ENTRIES IN tg_0082_rev
           WHERE obj_key EQ tg_0082_rev-obj_key_rev.

        SELECT *
          FROM zib_contabil_chv
          APPENDING TABLE tg_zib_chv
           FOR ALL ENTRIES IN tg_0082_rev
           WHERE obj_key EQ tg_0082_rev-obj_key_rev.

        SELECT *
          FROM zib_contabil_err
          APPENDING TABLE tg_zib_err
           FOR ALL ENTRIES IN tg_0082_rev
           WHERE obj_key EQ tg_0082_rev-obj_key_rev.
      ENDIF.

    ELSE.
*                                           'dados para visualização.'.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-015
                                             TEXT-016.
      STOP.
    ENDIF.
  ENDIF.


ENDFORM.                    " SELECIONAR_DADOS
FORM organizacao_dados_r.
  DATA: wl_saldo_mi  TYPE faglflext-hslvt,
        wl_saldo_mi2 TYPE faglflext-kslvt,
        wl_saldo_mi3 TYPE faglflext-oslvt,
        wl_saidar    LIKE wg_saidar,
        wl_update    TYPE sy-tabix,
        vg_racct     TYPE faglflext-racct,
        wl_flag.

*---> 05/07/2023 - Migração S4 - DL
  SORT tg_t882g BY rbukrs.
*<--- 05/07/2023 - Migração S4 - DL
  READ TABLE tg_t882g INTO wg_t882g
    WITH KEY rbukrs =  s_bukrs-low
             BINARY SEARCH.
** Processamento
  IF p_proc IS NOT INITIAL.
    SORT: tg_skat       BY saknr,
          tg_ska1       BY saknr,
          tg_tcurr      BY fcurr tcurr,
          tg_tcurr_lday BY fcurr tcurr,
          tg_t001       BY bukrs,
          tg_t882g      BY rbukrs,
          tg_0082       BY saknr vbund,
          tg_zib_chv    BY obj_key,
          tg_zib_err    BY obj_key.

    LOOP AT tg_faglflext INTO wg_faglflext.
      CLEAR: wl_saldo_mi, wl_saldo_mi2, wl_saldo_mi3,
      wl_update, wl_saidar, wl_flag.

      "Documento Processado não deve mais consultar pois vai alterar novamente
      "com base diferente, a variação ou valor em moeda forte irá mudar
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wg_faglflext-racct
        IMPORTING
          output = vg_racct.

      IF  '214101_224100_121130_113130_113131_214121' CS vg_racct.
        READ TABLE tg_0082 INTO wg_0082 WITH KEY saknr = wg_faglflext-racct
                                                 vbund = wg_faglflext-rassc.
      ELSE.
        READ TABLE tg_0082 INTO wg_0082 WITH KEY saknr = wg_faglflext-racct.
      ENDIF.
      IF ( sy-subrc IS INITIAL ) AND ( wg_0082-obj_key IS NOT INITIAL ) AND ( wg_0082-obj_key_est IS INITIAL ).
        CONTINUE.
      ENDIF.
      CLEAR: wg_0082.

      IF  '214101_224100_121130_113130_113131_214121' CS vg_racct.
        READ TABLE tg_saidar INTO wl_saidar WITH KEY racct = wg_faglflext-racct
                                                   rassc = wg_faglflext-rassc.
      ELSE.
        READ TABLE tg_saidar INTO wl_saidar WITH KEY racct = wg_faglflext-racct.
      ENDIF.
      IF sy-subrc IS INITIAL.
        wl_update = sy-tabix.
        ADD wl_saidar-curr1 TO wl_saldo_mi.
        ADD wl_saidar-curr2 TO wl_saldo_mi2.
        ADD wl_saidar-curr3 TO wl_saldo_mi3.
      ENDIF.

**  Saldo Mi1 ***************************************************************
      ADD wg_faglflext-hslvt TO wl_saldo_mi.
      IF s_mes-low(2) GE '01'.
        ADD wg_faglflext-hsl01 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '02'.
        ADD wg_faglflext-hsl02 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '03'.
        ADD wg_faglflext-hsl03 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '04'.
        ADD wg_faglflext-hsl04 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '05'.
        ADD wg_faglflext-hsl05 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '06'.
        ADD wg_faglflext-hsl06 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '07'.
        ADD wg_faglflext-hsl07 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '08'.
        ADD wg_faglflext-hsl08 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '09'.
        ADD wg_faglflext-hsl09 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '10'.
        ADD wg_faglflext-hsl10 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '11'.
        ADD wg_faglflext-hsl11 TO wl_saldo_mi.
      ENDIF.
      IF s_mes-low(2) GE '12'.
        ADD wg_faglflext-hsl12 TO wl_saldo_mi.
        ADD wg_faglflext-hsl13 TO wl_saldo_mi.
        ADD wg_faglflext-hsl14 TO wl_saldo_mi.
        ADD wg_faglflext-hsl15 TO wl_saldo_mi.
        ADD wg_faglflext-hsl16 TO wl_saldo_mi.
      ENDIF.

**  Saldo Mi2 *****************************************************************
      ADD wg_faglflext-kslvt TO wl_saldo_mi2.
      IF s_mes-low(2) GE '01'.
        ADD wg_faglflext-ksl01 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '02'.
        ADD wg_faglflext-ksl02 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '03'.
        ADD wg_faglflext-ksl03 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '04'.
        ADD wg_faglflext-ksl04 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '05'.
        ADD wg_faglflext-ksl05 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '06'.
        ADD wg_faglflext-ksl06 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '07'.
        ADD wg_faglflext-ksl07 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '08'.
        ADD wg_faglflext-ksl08 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '09'.
        ADD wg_faglflext-ksl09 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '10'.
        ADD wg_faglflext-ksl10 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '11'.
        ADD wg_faglflext-ksl11 TO wl_saldo_mi2.
      ENDIF.
      IF s_mes-low(2) GE '12'.
        ADD wg_faglflext-ksl12 TO wl_saldo_mi2.
        ADD wg_faglflext-ksl13 TO wl_saldo_mi2.
        ADD wg_faglflext-ksl14 TO wl_saldo_mi2.
        ADD wg_faglflext-ksl15 TO wl_saldo_mi2.
        ADD wg_faglflext-ksl16 TO wl_saldo_mi2.
      ENDIF.

**  Saldo Mi3 ****************************************************************************
      ADD wg_faglflext-oslvt TO wl_saldo_mi3.
      IF s_mes-low(2) GE '01'.
        ADD wg_faglflext-osl01 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '02'.
        ADD wg_faglflext-osl02 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '03'.
        ADD wg_faglflext-osl03 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '04'.
        ADD wg_faglflext-osl04 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '05'.
        ADD wg_faglflext-osl05 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '06'.
        ADD wg_faglflext-osl06 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '07'.
        ADD wg_faglflext-osl07 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '08'.
        ADD wg_faglflext-osl08 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '09'.
        ADD wg_faglflext-osl09 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '10'.
        ADD wg_faglflext-osl10 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '11'.
        ADD wg_faglflext-osl11 TO wl_saldo_mi3.
      ENDIF.
      IF s_mes-low(2) GE '12'.
        ADD wg_faglflext-osl12 TO wl_saldo_mi3.
        ADD wg_faglflext-osl13 TO wl_saldo_mi3.
        ADD wg_faglflext-osl14 TO wl_saldo_mi3.
        ADD wg_faglflext-osl15 TO wl_saldo_mi3.
        ADD wg_faglflext-osl16 TO wl_saldo_mi3.
      ENDIF.


      READ TABLE tg_t001 INTO wg_t001 WITH KEY bukrs = wg_faglflext-rbukrs BINARY SEARCH.

      IF wl_update IS INITIAL.
        READ TABLE tg_skat INTO wg_skat WITH KEY saknr = wg_faglflext-racct BINARY SEARCH.
        READ TABLE tg_ska1 INTO wg_ska1 WITH KEY saknr = wg_faglflext-racct BINARY SEARCH.
        "IR56206 ALRS 13.03.2015
        IF  '214101_224100_121130_113130_113131_214121' CS vg_racct.
          READ TABLE tg_0082 INTO wg_0082 WITH KEY saknr = wg_faglflext-racct
                                                   vbund = wg_faglflext-rassc BINARY SEARCH.
        ELSE.
          READ TABLE tg_0082 INTO wg_0082 WITH KEY saknr = wg_faglflext-racct BINARY SEARCH.
        ENDIF.
        IF sy-subrc IS INITIAL.
          CLEAR: wg_zib_chv.
          READ TABLE tg_zib_chv INTO wg_zib_chv WITH KEY obj_key = wg_0082-obj_key BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            READ TABLE tg_zib_err INTO wg_zib_err WITH KEY obj_key = wg_0082-obj_key BINARY SEARCH.
          ELSEIF wg_0082-obj_key_est IS NOT INITIAL.   " estorno
            CLEAR: wg_zib_chv.
            READ TABLE tg_zib_chv  INTO wg_zib_chv WITH KEY obj_key = wg_0082-obj_key_est BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE tg_zib_err  INTO wg_zib_err WITH KEY obj_key = wg_0082-obj_key_est BINARY SEARCH.
            ELSE.
              wg_saidar-belnr_est = wg_zib_chv-belnr.
            ENDIF.
          ELSEIF wg_0082-obj_key_est IS INITIAL.
            wg_saidar-belnr = wg_zib_chv-belnr.
          ENDIF.
        ENDIF.
        wg_saidar-obj_key     = wg_0082-obj_key.
        wg_saidar-obj_key_est = wg_0082-obj_key_est.
        wg_saidar-obj_key_rev = wg_0082-obj_key_rev.
        wg_saidar-racct       = wg_faglflext-racct.
        wg_saidar-txt50       = wg_skat-txt50.
        wg_saidar-ktoks       = wg_ska1-ktoks.
      ENDIF.
      wg_saidar-curr1       = wl_saldo_mi.
      wg_saidar-curr2       = wl_saldo_mi2.
      wg_saidar-curr3       = wl_saldo_mi3.

      CLEAR: wg_tcurr.
      READ TABLE tg_tcurr INTO wg_tcurr
        WITH KEY fcurr = v_moedad "USD BRL
                 tcurr = v_moedap "BRL USD
                 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wg_saidar-tx_usd = wg_tcurr-ukurs.
        IF v_moedad = 'USD'.
          IF wg_saidar-tx_usd LT 0.
            MULTIPLY wg_saidar-tx_usd BY -1.
            TRY.
                wg_saidar-saldo_corr  = ( wg_saidar-curr1 * wg_saidar-tx_usd ).
              CATCH cx_sy_zerodivide .
            ENDTRY.
          ELSE.
            TRY.
                wg_saidar-saldo_corr  = ( wg_saidar-curr1 / wg_saidar-tx_usd ).
              CATCH cx_sy_zerodivide .
            ENDTRY.
          ENDIF.
          wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr  - wg_saidar-curr2 ).
        ELSE.
          IF wg_saidar-tx_usd LT 0.
            MULTIPLY wg_saidar-tx_usd BY -1.
            TRY.
                wg_saidar-saldo_corr  = ( wg_saidar-curr2 * wg_saidar-tx_usd ).
              CATCH cx_sy_zerodivide .
            ENDTRY.
          ELSE.
            TRY.
                wg_saidar-saldo_corr  = ( wg_saidar-curr2 / wg_saidar-tx_usd ).
              CATCH cx_sy_zerodivide .
            ENDTRY.
          ENDIF.
          wg_saidar-vlr_ajust   = ( wg_saidar-saldo_corr  - wg_saidar-curr1 ).
        ENDIF.
      ENDIF.

      LOOP AT tg_0081 INTO wg_0081.
        IF wg_0081-conta_de IS NOT INITIAL.
          IF wg_faglflext-racct EQ wg_0081-conta_de.
            wl_flag = c_x.
            EXIT.
          ENDIF.
        ENDIF.
        CLEAR:wg_0081.
      ENDLOOP.

      "IR56206 ALRS 13.03.2015
      IF  '214101_224100_121130_113130_113131_214121' CS vg_racct.
        READ TABLE tg_0082 INTO wg_0082
         WITH KEY saknr = wg_faglflext-racct
                  vbund = wg_faglflext-rassc BINARY SEARCH.
      ELSE.
        READ TABLE tg_0082 INTO wg_0082
          WITH KEY saknr = wg_faglflext-racct BINARY SEARCH.
      ENDIF.
      " Se for estorno recupera valor de ajuste
      IF sy-subrc = 0.
        IF wg_0082-obj_key_est IS INITIAL AND wg_0082-obj_key IS NOT INITIAL.
          wg_saidar-vlr_ajust  = wg_0082-vlr_corr_mi2.
          wg_saidar-vlr_ajust2 = wg_0082-vlr_corr_mi3.
        ENDIF.
      ENDIF.

      "IR56206 ALRS 13.03.2015
      IF  '214101_224100_121130_113130_113131_214121' CS vg_racct.
        wg_saidar-rassc = wg_faglflext-rassc.
      ENDIF.


      IF wl_flag IS NOT INITIAL.
        IF wl_update IS INITIAL.
          APPEND wg_saidar TO tg_saidar.
        ELSE.
          MODIFY tg_saidar FROM wg_saidar INDEX wl_update TRANSPORTING curr1 curr2 curr3 tx_usd tx_brl saldo_corr saldo_corr2 vlr_ajust vlr_ajust2.
        ENDIF.
      ENDIF.
      CLEAR: wg_saidar, wg_t001, wg_ska1, wg_skat, wg_0082, wl_flag.
    ENDLOOP.

    "10.09.2015 ALRS
    IF s_bukrs-low EQ '0101'.
      LOOP AT  tg_saidar INTO wg_saidar.
        MULTIPLY wg_saidar-curr1 BY 100.
        MODIFY tg_saidar FROM wg_saidar INDEX sy-tabix TRANSPORTING curr1.
      ENDLOOP.
    ENDIF.

    "**********************************************************************************************
    " Ajusta Saida de Documento Processado """"""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT tg_0082 INTO wg_0082.
      "IR56206 ALRS 13.03.2015
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wg_0082-saknr
        IMPORTING
          output = vg_racct.
      IF '214101_224100_121130_113130_113131_214121' CS vg_racct.
        READ TABLE tg_saidar INTO wl_saidar WITH KEY racct = wg_0082-saknr
                                                   rassc = wg_0082-vbund.
      ELSE.
        READ TABLE tg_saidar INTO wl_saidar WITH KEY racct = wg_0082-saknr.
      ENDIF.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE tg_skat INTO wg_skat WITH KEY saknr = wg_0082-saknr BINARY SEARCH.
        READ TABLE tg_ska1 INTO wg_ska1 WITH KEY saknr = wg_0082-saknr BINARY SEARCH.
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
        wl_saidar-obj_key_rev  = wg_0082-obj_key_rev.
        READ TABLE tg_zib_chv INTO wg_zib_chv WITH KEY obj_key = wg_0082-obj_key BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wl_saidar-belnr = wg_zib_chv-belnr.
        ENDIF.

        READ TABLE tg_zib_chv INTO wg_zib_chv WITH KEY obj_key = wg_0082-obj_key_rev.
        IF sy-subrc IS INITIAL.
          wl_saidar-belnr_rev = wg_zib_chv-belnr.
        ENDIF.

        APPEND wl_saidar TO tg_saidar.
      ENDIF.
    ENDLOOP.
    "**********************************************************************************************

    SORT tg_saidar BY racct.


    PERFORM atualiza_saidar TABLES tg_saidar
                                   tg_zib
                                   tg_zib_chv
                                   tg_zib_err.

    IF tg_saidar[] IS INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-015
                                             TEXT-016.
      STOP.
    ENDIF.

** Visualização
  ELSE.
    SORT: tg_skat    BY saknr,
          tg_ska1    BY saknr,
          tg_t001    BY bukrs,
          tg_zib_chv BY obj_key,
          tg_zib_err BY obj_key.

    LOOP AT tg_0082 INTO wg_0082.
      CLEAR: wl_flag.

      READ TABLE tg_skat INTO wg_skat WITH KEY saknr = wg_0082-saknr BINARY SEARCH.
      READ TABLE tg_ska1 INTO wg_ska1 WITH KEY saknr = wg_0082-saknr BINARY SEARCH.

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
      wg_saidar-obj_key_rev  = wg_0082-obj_key_rev.

      READ TABLE tg_zib_chv INTO wg_zib_chv WITH KEY obj_key = wg_0082-obj_key_rev.
      IF sy-subrc IS INITIAL.
        wl_saidar-belnr_rev = wg_zib_chv-belnr.
      ENDIF.

      LOOP AT tg_0081 INTO wg_0081.

        IF wg_0081-conta_de IS NOT INITIAL.
          IF wg_saidar-racct EQ wg_0081-conta_de.
            wl_flag = c_x.
            EXIT.
          ENDIF.
        ENDIF.
        CLEAR:wg_0081.
      ENDLOOP.
      IF wl_flag IS NOT INITIAL.
        APPEND wg_saidar TO tg_saidar[].
      ENDIF.
      CLEAR: wg_saidar, wg_ska1, wg_skat, wl_flag.
    ENDLOOP.

    PERFORM atualiza_saidar TABLES tg_saidar
                                  tg_zib
                                  tg_zib_chv
                                  tg_zib_err.


  ENDIF.



** Processamento
  IF p_proc IS NOT INITIAL.
    IF s_bukrs-low EQ '0101'.
*      LOOP AT  TG_SAIDA INTO WG_SAIDA.
*        MULTIPLY WG_SAIDA-CURR1 BY 100.
*        MODIFY TG_SAIDA FROM WG_SAIDA INDEX SY-TABIX TRANSPORTING CURR1.
*      ENDLOOP.
    ELSEIF s_bukrs-low EQ '0004'. " recalcula ajuste
      LOOP AT  tg_saidar INTO wg_saidar.
        tabix = sy-tabix.
        CLEAR wg_0082.
        READ TABLE tg_0082 INTO wg_0082
              WITH KEY saknr = wg_saidar-racct  BINARY SEARCH.
        " Se for estorno recupera valor de ajuste
        IF sy-subrc NE 0
          OR  ( wg_0082-obj_key_est IS NOT INITIAL OR  wg_0082-obj_key IS NOT INITIAL ).
          wg_saidar-saldo_corr = wg_saidar-curr2 * wg_saidar-tx_usd.
          wg_saidar-vlr_ajust  = wg_saidar-saldo_corr - wg_saidar-curr1.
          MODIFY tg_saidar FROM wg_saidar INDEX tabix TRANSPORTING vlr_ajust saldo_corr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " ORGANIZACAO_DADOS

*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
FORM get_next_number  USING    p_object   "TYPE nrobj
                               p_nr_range "TYPE nrnr
*                               P_COMMIT
                      CHANGING p_number.

*  IF P_COMMIT IS NOT INITIAL.
  CLEAR p_number.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = p_nr_range
      object                  = p_object
    IMPORTING
      number                  = p_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc NE 0.
    CLEAR: p_number.
    MESSAGE e836(sd) WITH TEXT-028
                          TEXT-029.

  ENDIF.
*  ELSE.
*    P_NUMBER = '$00000001'.
*    WG_FLAG = C_X.
*  ENDIF.

ENDFORM.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_exec .
  IF tg_saida_exec[] IS NOT INITIAL.
    PERFORM montar_layout USING 'TG_SAIDA_EXEC'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       i_callback_program    = v_report
*       I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
        it_fieldcat           = estrutura[]
*       IT_SORT               = T_SORT[]
        i_save                = 'A'
        i_screen_start_column = 3
        i_screen_start_line   = 3
        i_screen_end_column   = 100
        i_screen_end_line     = 13
      TABLES
        t_outtab              = tg_saida_exec.
  ENDIF.
ENDFORM.                    " IMPRIMIR_EXEC

FORM atualiza_saidar  TABLES   tl_saida   LIKE tg_saidar
                              tl_zib     STRUCTURE zib_contabil
                              tl_zib_chv STRUCTURE zib_contabil_chv
                              tl_zib_err STRUCTURE zib_contabil_err.

  SORT: tl_zib     BY obj_key rg_atualizado,
        tl_zib_chv BY obj_key,
        tl_zib_err BY obj_key.

  LOOP AT tl_saida.
    IF tl_saida-obj_key IS NOT INITIAL.
      CLEAR: tl_zib.

      READ TABLE tl_zib WITH KEY obj_key = tl_saida-obj_key rg_atualizado = 'S' BINARY SEARCH.



      IF sy-subrc IS INITIAL.
        CLEAR: tl_zib_chv.

        READ TABLE tl_zib_chv  WITH KEY obj_key       = tl_saida-obj_key BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          tl_saida-belnr = tl_zib_chv-belnr.
          "FB08
          SELECT SINGLE   bukrs belnr  gjahr  budat stblg stjah
            FROM bkpf
            INTO wg_bkpf_fb08
            WHERE bukrs EQ tl_zib_chv-bukrs
            AND   belnr EQ tl_zib_chv-belnr
            AND   gjahr EQ tl_zib_chv-gjahr
            AND   stblg NE ''.
          IF sy-subrc = 0.
            SELECT SINGLE   bukrs belnr gjahr budat
               FROM bkpf
               INTO wg_bkpf_fb08_e
               WHERE bukrs EQ wg_bkpf_fb08-bukrs
               AND   belnr EQ wg_bkpf_fb08-stblg
               AND   gjahr EQ wg_bkpf_fb08-stjah.
            IF wg_bkpf_fb08_e-budat+0(6) = wg_bkpf_fb08-budat+0(6). "estorno e lançamento mesmo mês, limpa pra gerar outro documento
              DELETE FROM zib_contabil_err WHERE obj_key  = tl_saida-obj_key.
              CLEAR: tl_saida-belnr, tl_saida-obj_key.
            ELSE.
              tl_saida-line_color = 'C601'.
              MODIFY tl_saida TRANSPORTING line_color.
            ENDIF.
          ENDIF.




          tl_saida-log   = icon_enter_more.
          MODIFY tl_saida TRANSPORTING belnr  obj_key  log.
        ELSE.
          CLEAR: tl_zib_err.
          READ TABLE tl_zib_err
            WITH KEY obj_key       = tl_saida-obj_key
                             BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            tl_saida-belnr = space.
            tl_saida-log   = icon_display_more.
            MODIFY tl_saida TRANSPORTING belnr log.
          ENDIF.
        ENDIF.
      ELSE.
        tl_saida-belnr = icon_operation.
        MODIFY tl_saida TRANSPORTING belnr.
      ENDIF.

      CLEAR: tl_zib_chv.
      READ TABLE tl_zib_chv  WITH KEY obj_key = tl_saida-obj_key_rev.
      IF sy-subrc EQ 0.
        tl_saida-belnr_rev = tl_zib_chv-belnr.
        tl_saida-line_color = 'C601'.
        MODIFY tl_saida TRANSPORTING belnr_rev line_color.
      ELSE.
        CLEAR: tl_zib_err.
        READ TABLE tl_zib_err WITH KEY obj_key = tl_saida-obj_key_rev.
        IF sy-subrc IS INITIAL.
          tl_saida-belnr_rev = space.
          tl_saida-log   = icon_display_more.
          MODIFY tl_saida TRANSPORTING belnr_rev log.
        ENDIF.
      ENDIF.
    ELSE.
      tl_saida-obj_key = space.
      tl_saida-belnr = space.
      tl_saida-log   = icon_enter_more.
      MODIFY tl_saida TRANSPORTING log belnr obj_key.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " ATUALIZA_SAIDA

*&---------------------------------------------------------------------*
*&      Form  BUSCA_CHAVE2
*&---------------------------------------------------------------------*
*       Busca Chave de Lançamento
*----------------------------------------------------------------------*
FORM busca_chave2 USING    pforn         TYPE char1
                           wl_estorno    TYPE char1
                           wl_varicao    TYPE char1
                           vlr_ajust     LIKE bsik-dmbtr
                           umskz         LIKE bsik-umskz
                  CHANGING zib_bschl     LIKE bsik-bschl.

  IF pforn = 'X'.
    IF vlr_ajust < 0 AND  umskz IS INITIAL.  zib_bschl =  '21'. ENDIF.
    IF vlr_ajust > 0 AND  umskz IS INITIAL.  zib_bschl =  '31'. ENDIF.

    IF vlr_ajust < 0 AND  umskz IS NOT INITIAL.  zib_bschl =  '29'. ENDIF.
    IF vlr_ajust > 0 AND  umskz IS NOT INITIAL.  zib_bschl =  '39'. ENDIF.

    IF wl_estorno IS NOT INITIAL.
      IF zib_bschl = '21'.
        zib_bschl = '31'.
      ELSEIF zib_bschl = '31'.
        zib_bschl = '21'.
      ENDIF.

      IF zib_bschl = '27'.
        zib_bschl = '34'.
      ELSEIF zib_bschl = '34'.
        zib_bschl = '27'.
      ENDIF.

      IF zib_bschl = '29'.
        zib_bschl = '39'.
      ELSEIF zib_bschl = '39'.
        zib_bschl = '29'.
      ENDIF.
    ENDIF.

  ELSE.
    IF vlr_ajust < 0 AND  umskz IS INITIAL.  zib_bschl =  '01'. ENDIF.
    IF vlr_ajust > 0 AND  umskz IS INITIAL.  zib_bschl =  '11'. ENDIF.

    IF vlr_ajust < 0 AND  umskz IS NOT INITIAL.  zib_bschl =  '09'. ENDIF.
    IF vlr_ajust > 0 AND  umskz IS NOT INITIAL.  zib_bschl =  '19'. ENDIF.

    IF wl_estorno IS NOT INITIAL.
      IF zib_bschl = '01'.
        zib_bschl = '11'.
      ELSEIF zib_bschl = '11'.
        zib_bschl = '01'.
      ENDIF.

      IF zib_bschl = '09'.
        zib_bschl = '19'.
      ELSEIF zib_bschl = '19'.
        zib_bschl = '09'.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " BUSCA_CHAVE2

*&---------------------------------------------------------------------*
*&      Form  REVERTE_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM estorna_documentos USING p_tipo.

  DATA: w_cont     TYPE i,
        vdata(10),
        p_erro(1),
        v_estorno  TYPE uf05a-stgrd,
        wa_zib_chv TYPE zib_contabil_chv,
        v_msg(60).



  vg_ryear  = s_mes-low+2(4).
  CONCATENATE vg_ryear s_mes-low(2) '01' INTO vg_last_day_aux.
  vg_last_day = vg_last_day_aux.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = vg_last_day
    IMPORTING
      e_date = vg_last_day.

  IF p_tipo = 'REV'.
    IF vg_last_day_aux+0(6) = sy-datum+0(6).
      MESSAGE TEXT-041 TYPE 'I'.
      EXIT.
    ENDIF.
    ADD 1 TO vg_last_day.
    v_estorno = '02'.
  ELSE.
    v_estorno = '01'.
  ENDIF.

  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
    EXPORTING
      i_bukrs  = s_bukrs-low
      i_data   = vg_last_day
    IMPORTING
      e_status = e_status
      e_messa  = e_messa
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.
  IF e_status = 'E'.
    MESSAGE e398(00) WITH e_messa.
    EXIT.
  ENDIF.

  IF p_tipo = 'REV'.
    v_msg = TEXT-045.
  ELSE.
    v_msg = TEXT-042.
  ENDIF.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = v_msg
      text_button_1         = TEXT-036
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = TEXT-037
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = w_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF w_answer NE '1'.
    EXIT.
  ENDIF.


  REFRESH tg_saida.
  LOOP AT tg_saidar INTO wg_saidar WHERE mark IS NOT INITIAL.
    MOVE-CORRESPONDING wg_saidar TO wg_saida.
    wg_saida-hkont = wg_saidar-racct.
    wg_saida-vbund = wg_saidar-rassc.
    APPEND wg_saida TO tg_saida.
  ENDLOOP.



  LOOP AT tg_saida INTO wg_saida WHERE mark IS NOT INITIAL AND obj_key_rev IS INITIAL.
    IF wg_saida-belnr IS INITIAL.
      CLEAR: wg_saida_exec.
      wg_saida_exec-icon   = icon_yellow_light.
      wg_saida_exec-tipo   = TEXT-m12. "'Variação'.
      wg_saida_exec-lifnr  = wg_saida-kunnr.
      wg_saida_exec-name1  = wg_saida-name1.
      wg_saida_exec-hkont  = wg_saida-hkont.
      wg_saida_exec-txt50  = wg_saida-txt50.
      wg_saida_exec-msg    = TEXT-030 .
      APPEND wg_saida_exec TO tg_saida_exec.
    ELSE.
      SELECT SINGLE *
        FROM zib_contabil_chv
        INTO  wa_zib_chv
         WHERE obj_key EQ wg_saida-obj_key.
      "FB08
      SELECT SINGLE   bukrs belnr  gjahr  budat stblg stjah
        FROM bkpf
        INTO wg_bkpf_fb08
        WHERE bukrs EQ wa_zib_chv-bukrs
        AND   belnr EQ wa_zib_chv-belnr
        AND   gjahr EQ wa_zib_chv-gjahr
        AND   stblg NE ''.
      IF sy-subrc = 0.
        CLEAR: wg_saida_exec.
        wg_saida_exec-icon   = icon_yellow_light.
        wg_saida_exec-tipo   = TEXT-m12. "'Variação'.
        wg_saida_exec-lifnr  = wg_saida-kunnr.
        wg_saida_exec-name1  = wg_saida-name1.
        wg_saida_exec-hkont  = wg_saida-hkont.
        wg_saida_exec-txt50  = wg_saida-txt50.
        wg_saida_exec-msg    = TEXT-044 .
        APPEND wg_saida_exec TO tg_saida_exec.
      ENDIF.
    ENDIF.

  ENDLOOP.

  "
  IF tg_saida_exec[] IS NOT INITIAL.
    EXIT.
  ENDIF.

  LOOP AT tg_saida INTO wg_saida WHERE mark IS NOT INITIAL.
    SELECT SINGLE *
        FROM zib_contabil_chv
        INTO  wa_zib_chv
         WHERE obj_key EQ wg_saida-obj_key.
    CONCATENATE  vg_last_day+6(2) '.' vg_last_day+4(2) '.' vg_last_day+0(4) INTO vdata.

    REFRESH ti_bdcdata.
    PERFORM f_bdc_data USING:
          'SAPMF05A'  '0105'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '/00',
          ''          ''      ''   'RF05A-BELNS'      wa_zib_chv-belnr,
          ''          ''      ''   'BKPF-BUKRS'       wa_zib_chv-bukrs,
          ''          ''      ''   'RF05A-GJAHS'      wa_zib_chv-gjahr,
          ''          ''      ''   'UF05A-STGRD'      v_estorno,
          ''          ''      ''   'BSIS-BUDAT'       vdata,
          'SAPMF05A'  '0105'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '=BU'.

    CLEAR p_erro.
    vobj_key = wg_saida-obj_key.
    PERFORM zf_call_transaction USING 'FB08' CHANGING p_erro.

    IF p_erro IS NOT INITIAL.
      MESSAGE TEXT-043 TYPE 'I'.
    ELSEIF p_tipo = 'EST'.
      DELETE FROM zfit0082
      WHERE bukrs   = s_bukrs-low
      AND   mes_ano = s_mes-low
      AND   saknr   = wg_saida-hkont
      AND   vbund   = wg_saida-vbund.
    ELSEIF p_tipo = 'EST'.
      DELETE FROM zfit0084
      WHERE obj_key    = wg_saida-obj_key.
      COMMIT WORK.
      EXIT.
    ENDIF.

  ENDLOOP.
ENDFORM.

FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA

FORM zf_call_transaction USING p_trans CHANGING p_erro.
  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  DATA: wl_cont     TYPE sy-tabix.

  REFRESH: it_msg, tg_zib_err.
  CLEAR tg_zib_err.

  wl_mode = 'E'.

  CALL TRANSACTION p_trans USING ti_bdcdata
        MODE wl_mode
        MESSAGES INTO it_msg.
  CLEAR: wl_cont.

  LOOP AT it_msg WHERE msgtyp EQ 'E'.
    ADD 1 TO wl_cont.
  ENDLOOP.
  IF wl_cont  GT 0.
    CLEAR wl_cont.
    DELETE FROM zib_contabil_err WHERE obj_key  = vobj_key.
    LOOP AT it_msg WHERE msgtyp EQ 'E'.
      ADD 1 TO wl_cont.
      CLEAR: wl_message.
      CALL FUNCTION 'CUTC_GET_MESSAGE'
        EXPORTING
          msg_type       = it_msg-msgtyp
          msg_id         = it_msg-msgid
          msg_no         = sy-msgno
          msg_arg1       = sy-msgv1
          msg_arg2       = sy-msgv2
          msg_arg3       = sy-msgv3
          msg_arg4       = sy-msgv4
        IMPORTING
          raw_message    = wl_message
        EXCEPTIONS
          msg_not_found  = 1
          internal_error = 2
          OTHERS         = 3.

      IF ( sy-subrc NE 0 ).
        wl_message = 'Erro na mensagem do BATCH-INPUT'.
      ENDIF.

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

      APPEND wg_zib_err TO tg_zib_err.
      CLEAR wg_zib_err.

    ENDLOOP.

    MODIFY zib_contabil_err FROM TABLE tg_zib_err.
  ENDIF.

  READ TABLE it_msg WITH KEY msgtyp = 'A'.
  IF sy-subrc = 0.
    p_erro = 'X'.
  ELSE.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      p_erro = 'X'.
    ENDIF.
  ENDIF.

  CLEAR wg_documento.

  READ TABLE it_msg WITH KEY msgid = c_msgid
                             msgnr = c_msgnr
                             msgtyp = 'S'.

  IF sy-subrc = 0.
    MOVE it_msg-msgv1 TO wg_documento.
  ENDIF.

  IF  wg_documento IS INITIAL.
    p_erro = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_documento
      IMPORTING
        output = wg_documento.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION


FORM gera_contabilr  USING   wl_saida TYPE ty_saidar
                             wl_estorno
                   CHANGING  p_obj_key TYPE zib_contabil_chv-obj_key.

  DATA: wl_input_zib  TYPE zib_contabil,
        tl_input_zib  TYPE TABLE OF zib_contabil,
        wl_obj_key(9),
        v_saknr       TYPE zfit0081-saknr,
        v_bewar       TYPE bseg-bewar,
        vanomes(7).

  CONCATENATE s_mes-low(2) '/' s_mes-low+2(4) INTO vanomes.

  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
    EXPORTING
      i_bukrs  = s_bukrs-low
      i_data   = vg_last_day
    IMPORTING
      e_status = e_status
      e_messa  = e_messa
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.
  IF e_status = 'E'.
    MESSAGE e398(00) WITH e_messa.
    EXIT.
  ENDIF.

  REFRESH: tl_input_zib.
  CLEAR: wl_input_zib, wl_input_0082.
  READ TABLE tg_t001 INTO wg_t001
      WITH KEY bukrs =  s_bukrs-low
               BINARY SEARCH.

  READ TABLE tg_t882g INTO wg_t882g
    WITH KEY rbukrs =  s_bukrs-low
             BINARY SEARCH.

  LOOP AT tg_0081 INTO wg_0081.
    IF wg_0081-conta_de IS NOT INITIAL.
      IF wl_saida-racct EQ wg_0081-conta_de.
        EXIT.
      ENDIF.
    ENDIF.
    CLEAR:wg_0081.
  ENDLOOP.
  IF wg_0081 IS NOT INITIAL .
    IF p_obj_key IS INITIAL.
      PERFORM get_next_number  USING  'Z_SALDOMO2'
                                      '01'
                             CHANGING wl_obj_key.
    ELSE.
      wl_obj_key = p_obj_key+7(9).
    ENDIF.
** 1 Partida
    wl_input_zib-mandt    = sy-mandt.
    CONCATENATE 'ZFI0111' wl_obj_key vg_ryear INTO  wl_input_zib-obj_key.
    wl_input_zib-seqitem    = '0001'.
    CONCATENATE s_mes-low(2) '/' s_mes-low+2(4) INTO wl_input_zib-xblnr.
    IF wl_estorno IS INITIAL.
      IF wl_saida-vlr_ajust LT 0.
        wl_input_zib-bschl      = '50'.
      ELSE.
        wl_input_zib-bschl      = '40'.
      ENDIF.
    ELSE.
      IF wl_saida-vlr_ajust LT 0.
        wl_input_zib-bschl      = '40'.
      ELSE.
        wl_input_zib-bschl      = '50'.
      ENDIF.
    ENDIF.
    wl_input_zib-gsber      = '9000'.
    wl_input_zib-bukrs      = s_bukrs-low.
    wl_input_zib-interface  = '35'.
    wl_input_zib-bktxt      = TEXT-050.
    WRITE vg_last_day TO  wl_input_zib-bldat.
    WRITE vg_last_day TO  wl_input_zib-budat.

    wl_input_zib-gjahr      = vg_ryear.
    wl_input_zib-monat      = s_mes-low(2).
    wl_input_zib-blart      = 'VA'.

    IF  wg_0081-conta_ajt IS NOT INITIAL.
      wl_input_zib-hkont      = wg_0081-conta_ajt.
    ELSE.
      wl_input_zib-hkont      = wl_saida-racct.
    ENDIF.

    CONCATENATE TEXT-052 vanomes
        INTO wl_input_zib-sgtxt SEPARATED BY space.


    wl_input_zib-wrbtr      = 0.
    wl_input_zib-vbund      = wl_saida-rassc.

    wl_input_zib-waers      = v_moedad.


    SELECT SINGLE *
      FROM j_1bbranch
      INTO @DATA(w_branch)
      WHERE bukrs      EQ @s_bukrs-low
      AND   branch     NE '0001'
      AND   cgc_branch EQ 1.


    wl_input_zib-bupla      = w_branch-branch.


    CLEAR v_bewar.
    SELECT SINGLE bewar
      INTO v_bewar
      FROM zfit0030
      WHERE hkont EQ wl_input_zib-hkont
      AND   cond  EQ ''.

    wl_input_zib-bewar      = v_bewar.

    IF wl_input_zib-hkont = '0000248001'.
      wl_input_zib-bewar = 'P17'.
    ENDIF.

    wl_input_zib-waers_i    = 'X'.
    wl_input_zib-waers_f    = 'USD'.

    IF v_moedad = 'USD'.
      wl_input_zib-dmbe2      = wl_saida-vlr_ajust.
      wl_input_zib-dmbtr      = 0.
      IF wl_input_zib-dmbe2 LT 0.
        MULTIPLY wl_input_zib-dmbe2 BY -1.
      ENDIF.
    ELSE.
      wl_input_zib-dmbtr      = wl_saida-vlr_ajust.
      wl_input_zib-dmbe2      = 0.
      IF wl_input_zib-dmbtr LT 0.
        MULTIPLY wl_input_zib-dmbtr BY -1.
      ENDIF.
    ENDIF.

    wl_input_zib-rg_atualizado  = 'N'.

*** BUG #184187 - MMSILVA - 01.07.2025 - Ini ***
    wl_input_zib-zuonr = 'MOEDA APRESENTACAO'.
*** BUG #184187 - MMSILVA - 01.07.2025 - Fim ***

    APPEND wl_input_zib TO tl_input_zib.
    CLEAR: wl_input_zib.

** 2 Partida
    wl_input_zib-mandt    = sy-mandt.
    CONCATENATE 'ZFI0111' wl_obj_key vg_ryear INTO  wl_input_zib-obj_key.
    wl_input_zib-seqitem    = '0002'.
    CONCATENATE s_mes-low(2) '/' s_mes-low+2(4) INTO wl_input_zib-xblnr.
    IF wl_estorno IS INITIAL.
      IF wl_saida-vlr_ajust LT 0.
        wl_input_zib-bschl      = '40'.
      ELSE.
        wl_input_zib-bschl      = '50'.
      ENDIF.
    ELSE.
      IF wl_saida-vlr_ajust LT 0.
        wl_input_zib-bschl      = '50'.
      ELSE.
        wl_input_zib-bschl      = '40'.
      ENDIF.
    ENDIF.
    wl_input_zib-gsber      = '9000'.
    wl_input_zib-bukrs      = s_bukrs-low.
    wl_input_zib-interface  = '35'.
    wl_input_zib-bktxt      = TEXT-050.
    WRITE vg_last_day TO  wl_input_zib-bldat.
*    WL_INPUT_ZIB-BLDAT      = VG_LAST_DAY.
    WRITE vg_last_day TO  wl_input_zib-budat.
*    WL_INPUT_ZIB-BUDAT      = VG_LAST_DAY.
    wl_input_zib-gjahr      = vg_ryear.
    wl_input_zib-monat      = s_mes-low(2).
    wl_input_zib-blart      = 'VA'.

    wl_input_zib-hkont      =  wg_0081-hkont.
    CONCATENATE TEXT-052 vanomes
    INTO wl_input_zib-sgtxt SEPARATED BY space.


    CLEAR v_bewar.
    SELECT SINGLE bewar
      INTO v_bewar
      FROM zfit0030
      WHERE hkont EQ wl_input_zib-hkont
      AND   cond  EQ ''.

    wl_input_zib-bewar      = v_bewar.

    IF wl_input_zib-hkont = '0000248001'.
      wl_input_zib-bewar = 'P17'.
    ENDIF.

    wl_input_zib-vbund      = wl_saida-rassc.


    wl_input_zib-waers      = v_moedad.

    wl_input_zib-wrbtr      = 0.


    wl_input_zib-bupla      = w_branch-branch.

    wl_input_zib-waers_i    = 'X'.
    wl_input_zib-waers_f    = 'USD'.

    IF v_moedad = 'USD'.
      wl_input_zib-dmbe2      = wl_saida-vlr_ajust.
      wl_input_zib-dmbtr      = 0.
      IF wl_input_zib-dmbe2 LT 0.
        MULTIPLY wl_input_zib-dmbe2 BY -1.
      ENDIF.
    ELSE.
      wl_input_zib-dmbtr      = wl_saida-vlr_ajust.
      wl_input_zib-dmbe2      = 0.
      IF wl_input_zib-dmbtr LT 0.
        MULTIPLY wl_input_zib-dmbtr BY -1.
      ENDIF.
    ENDIF.

    wl_input_zib-rg_atualizado  = 'N'.

*** BUG #184187 - MMSILVA - 01.07.2025 - Ini ***
    wl_input_zib-zuonr = 'MOEDA APRESENTACAO'.
*** BUG #184187 - MMSILVA - 01.07.2025 - Fim ***

    APPEND wl_input_zib TO tl_input_zib.
    CLEAR: wl_input_zib.


    MODIFY zib_contabil FROM TABLE tl_input_zib.
    COMMIT WORK.
    IF wl_estorno IS INITIAL.
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

      CLEAR: wl_input_0082-sdo_corr_mi3, wl_input_0082-vlr_corr_mi3.

      CONCATENATE 'ZFI0111' wl_obj_key vg_ryear INTO wl_input_0082-obj_key.
      p_obj_key = wl_input_0082-obj_key.

      wl_input_0082-obj_key_est  = space.
      wl_input_0082-usnam        = sy-uname.
      wl_input_0082-aedat        = sy-datum.
      wl_input_0082-cputm        = sy-uzeit.

      MODIFY  zfit0082 FROM wl_input_0082.
      COMMIT WORK.
      wg_saida_exec-icon   = icon_green_light.
      wg_saida_exec-racct = wl_saida-racct.
      wg_saida_exec-txt50 = wl_saida-txt50.
      wg_saida_exec-msg   = TEXT-053 .
      APPEND wg_saida_exec TO tg_saida_exec.
      CLEAR: wl_input_0082, wg_saida_exec.
    ELSE.
      CONCATENATE 'ZFI0111' wl_obj_key vg_ryear INTO wl_input_0082-obj_key_est.
      p_obj_key = wl_input_0082-obj_key_est.

      UPDATE zfit0082 SET obj_key_est = wl_input_0082-obj_key_est
                      WHERE bukrs   EQ s_bukrs-low
                        AND mes_ano EQ s_mes-low
                        AND saknr   EQ wl_saida-racct
                        AND vbund   EQ wl_saida-rassc.
      COMMIT WORK.

      wg_saida_exec-icon  = icon_green_light.
      wg_saida_exec-racct = wl_saida-racct.
      wg_saida_exec-txt50 = wl_saida-txt50.
      wg_saida_exec-msg   = TEXT-054 .
      APPEND wg_saida_exec TO tg_saida_exec.

    ENDIF.
  ELSE.
    CLEAR: wg_saida_exec.
    wg_saida_exec-icon   = icon_red_light.
    wg_saida_exec-racct  = wl_saida-racct.
    wg_saida_exec-txt50  = wl_saida-txt50.
    wg_saida_exec-msg    = TEXT-055.
    APPEND wg_saida_exec TO tg_saida_exec.
  ENDIF.
ENDFORM.                    " GERA_CONTABIL
*&---------------------------------------------------------------------*
*&      Form  EXECT_REVERSAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exect_reversao .

  DATA: w_cont     TYPE i,
        vdata(10),
        p_erro(1),
        v_estorno  TYPE uf05a-stgrd,
        wa_zib_chv TYPE zib_contabil_chv,
        it_zib     TYPE TABLE OF zib_contabil,
        wa_zib     TYPE zib_contabil,
        wa_zib_aux TYPE zib_contabil,
        wl_obj_key TYPE zib_contabil-obj_key,
        v_msg(60).

  CLEAR: vg_ryear, vg_rmonth, vg_last_day_aux, vg_last_day, e_status.
  FREE: it_zib.

  vg_ryear  = s_mes-low+2(4).
  vg_rmonth = s_mes-low+0(2).

  IF vg_rmonth >= '12'.
    vg_rmonth = '01'.
    vg_ryear  = vg_ryear + 1.

  ELSE.
    vg_rmonth = vg_rmonth + 1.
    IF vg_rmonth < 10.
      vg_rmonth = |0{ vg_rmonth }|.
    ENDIF.
  ENDIF.


  CONCATENATE vg_ryear vg_rmonth '01' INTO vg_last_day_aux.
  vg_last_day = vg_last_day_aux.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = vg_last_day
    IMPORTING
      e_date = vg_last_day.

  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
    EXPORTING
      i_bukrs  = s_bukrs-low
      i_data   = vg_last_day
    IMPORTING
      e_status = e_status
      e_messa  = e_messa
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.
  IF e_status = 'E'.
    MESSAGE e398(00) WITH e_messa.
    EXIT.
  ENDIF.


  v_msg = TEXT-045.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = v_msg
      text_button_1         = TEXT-036
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = TEXT-037
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = w_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF w_answer NE '1'.
    EXIT.
  ENDIF.


*  REFRESH tg_saida.
*  LOOP AT tg_saidar INTO wg_saidar WHERE mark IS NOT INITIAL.
*    MOVE-CORRESPONDING wg_saidar TO wg_saida.
*    wg_saida-hkont = wg_saidar-racct.
*    wg_saida-vbund = wg_saidar-rassc.
*    APPEND wg_saida TO tg_saida.
*  ENDLOOP.



  LOOP AT tg_saidar ASSIGNING FIELD-SYMBOL(<wg_saida>) WHERE mark IS NOT INITIAL AND belnr_rev IS INITIAL.

    CLEAR: wa_zib_chv, wa_zib.
    SELECT SINGLE *
      FROM zib_contabil
      INTO  wa_zib
       WHERE obj_key EQ <wg_saida>-obj_key
         AND bschl = '50'.

    IF sy-subrc EQ 0.
      CLEAR: wl_obj_key.
      PERFORM get_next_number  USING  'Z_SALDOMO2'
                                      '01'
                             CHANGING wl_obj_key.

      CONCATENATE 'ZFI0111' wl_obj_key vg_ryear INTO  wa_zib-obj_key.
      wa_zib-budat = |{ vg_last_day_aux+6(2) }.{ vg_last_day_aux+4(2) }.{ vg_last_day_aux+0(4) } |.
      wa_zib-monat = vg_rmonth.
      wa_zib-gjahr = vg_ryear.

      wa_zib-rg_atualizado = 'N'.
      wa_zib-seqitem = '000001'.
      wa_zib-bschl = '40'.
*** US #181796 - MMSILVA - 27.06.2025 - Ini ***
      wa_zib-xblnr = <wg_saida>-belnr.
      wa_zib-zuonr = 'MOEDA APRESENTACAO'.
*** US #181796 - MMSILVA - 27.06.2025 - Fim ***

      APPEND wa_zib TO it_zib.

      CLEAR: wa_zib_aux.
      SELECT SINGLE *
        FROM zib_contabil
        INTO  wa_zib_aux
         WHERE obj_key EQ <wg_saida>-obj_key
           AND bschl = '40'.
      IF wa_zib_aux IS NOT INITIAL.
        wa_zib_aux-obj_key = wa_zib-obj_key.
        wa_zib_aux-budat = |{ vg_last_day_aux+6(2) }.{ vg_last_day_aux+4(2) }.{ vg_last_day_aux+0(4) } |.
        wa_zib_aux-monat = vg_rmonth.
        wa_zib_aux-gjahr = vg_ryear.

        wa_zib_aux-rg_atualizado = 'N'.
        wa_zib_aux-seqitem = '000002'.
        wa_zib_aux-bschl = '50'.
*** US #181796 - MMSILVA - 27.06.2025 - Ini ***
        wa_zib_aux-xblnr = <wg_saida>-belnr.
        wa_zib_aux-zuonr = 'MOEDA APRESENTACAO'.
*** US #181796 - MMSILVA - 27.06.2025 - Fim ***
        APPEND wa_zib_aux TO it_zib.

        <wg_saida>-belnr_rev = icon_configuration.
        <wg_saida>-obj_key_rev = wa_zib-obj_key.
        <wg_saida>-line_color = 'C601'.
        <wg_saida>-belnr_rev = icon_system_settings.
        SELECT SINGLE * FROM zfit0082 INTO @DATA(wl_0082)
          WHERE obj_key EQ @<wg_saida>-obj_key.
        IF wl_0082 IS NOT INITIAL.
          wl_0082-obj_key_rev = wa_zib-obj_key.
          MODIFY zfit0082 FROM wl_0082.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: wa_zib.
  ENDLOOP.

  MODIFY zib_contabil FROM TABLE it_zib.

  COMMIT WORK.
  MESSAGE s024(sd) WITH 'Lançamento reversão realizada com sucesso.'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GERAR_REV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_rev .


  DATA: tl_zib      TYPE TABLE OF zib_contabil WITH HEADER LINE,
        tl_zib_chv  TYPE TABLE OF zib_contabil_chv WITH HEADER LINE,
        tl_zib_err  TYPE TABLE OF zib_contabil_err WITH HEADER LINE,
        tl_saida    TYPE TABLE OF ty_saidar,
        wl_obj_key  TYPE zib_contabil_chv-obj_key,
        wa_zfit0082 TYPE zfit0082.

  REFRESH: tg_saida_exec, tl_zib, tl_zib_chv, tl_zib_err, tl_saida.
  CLEAR:wl_obj_key.

  DATA: w_cont     TYPE i,
        vdata(10),
        p_erro(1),
        v_estorno  TYPE uf05a-stgrd,
        wa_zib_chv TYPE zib_contabil_chv,
        it_zib     TYPE TABLE OF zib_contabil,
        wa_zib     TYPE zib_contabil,
        v_msg(60).



  vg_ryear  = s_mes-low+2(4).
  CONCATENATE vg_ryear s_mes-low(2) '01' INTO vg_last_day_aux.
  vg_last_day = vg_last_day_aux.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = vg_last_day
    IMPORTING
      e_date = vg_last_day.

  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
    EXPORTING
      i_bukrs  = s_bukrs-low
      i_data   = vg_last_day
    IMPORTING
      e_status = e_status
      e_messa  = e_messa
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.
  IF e_status = 'E'.
    MESSAGE e398(00) WITH e_messa.
    EXIT.
  ENDIF.


  v_msg = TEXT-045.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = v_msg
      text_button_1         = TEXT-036
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = TEXT-037
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = w_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF w_answer NE '1'.
    EXIT.
  ENDIF.


  tl_saida[]  = tg_saidar[].
  DELETE tl_saida WHERE obj_key_rev IS INITIAL.
  IF tl_saida[] IS NOT INITIAL.
    SELECT *
      FROM zib_contabil
      INTO TABLE tl_zib
       FOR ALL ENTRIES IN tl_saida
       WHERE obj_key EQ tl_saida-obj_key_rev.
  ENDIF.


  IF tl_zib[] IS NOT INITIAL.
    SELECT *
     FROM zib_contabil_chv
     INTO TABLE tl_zib_chv
      FOR ALL ENTRIES IN tl_zib
      WHERE obj_key EQ tl_zib-obj_key.

    SELECT *
     FROM zib_contabil_err
     INTO TABLE tl_zib_err
      FOR ALL ENTRIES IN tl_zib
      WHERE obj_key EQ tl_zib-obj_key.

  ENDIF.

  SORT: tl_zib     BY obj_key,
        tl_zib_chv BY obj_key,
        tl_zib_err BY obj_key.

  LOOP AT tg_saidar INTO wg_saidar
        WHERE mark IS NOT INITIAL.
    IF wg_saidar-obj_key_rev IS NOT INITIAL AND wg_saidar-belnr_rev IS INITIAL.
      READ TABLE tl_zib WITH KEY obj_key = wg_saidar-obj_key_rev BINARY SEARCH.
      IF tl_zib-rg_atualizado EQ 'N'.
        CLEAR: wg_saida_exec.
        wg_saida_exec-icon   = icon_yellow_light.
        wg_saida_exec-racct  = wg_saidar-racct.
        wg_saida_exec-txt50  = wg_saidar-txt50.
        wg_saida_exec-msg    = TEXT-m01 .
        APPEND wg_saida_exec TO tg_saida_exec.
        CONTINUE.
      ELSEIF tl_zib-rg_atualizado EQ 'S'.
        READ TABLE tl_zib_chv WITH KEY obj_key = wg_saidar-obj_key_rev.
        IF sy-subrc IS INITIAL.
          CLEAR: wg_saida_exec.
          wg_saida_exec-icon   = icon_green_light.
          wg_saida_exec-racct  = wg_saidar-racct.
          wg_saida_exec-txt50  = wg_saidar-txt50.
          wg_saida_exec-msg    = TEXT-m02 .
          APPEND wg_saida_exec TO tg_saida_exec.
          acao '&ATUAL'.
          CONTINUE.
        ELSE.
          READ TABLE tl_zib_err  WITH KEY obj_key = wg_saidar-obj_key_rev.
          IF sy-subrc IS INITIAL.
            CLEAR: wg_saida_exec.
            wg_saida_exec-icon   = icon_red_light.
            wg_saida_exec-racct  = wg_saidar-racct.
            wg_saida_exec-txt50  = wg_saidar-txt50.
            wg_saida_exec-msg    = TEXT-m03 .
            APPEND wg_saida_exec TO tg_saida_exec.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                text_question         = TEXT-m04
*               TEXT_BUTTON_1         = 'Sim'(100)
                text_button_1         = TEXT-b01
                icon_button_1         = 'ICON_OKAY'
*               TEXT_BUTTON_2         = 'Não'(101)
                text_button_2         = TEXT-b02
                icon_button_2         = 'ICON_CANCEL'
                default_button        = '1'
                display_cancel_button = ' '
                start_column          = 25
                start_row             = 6
              IMPORTING
                answer                = w_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
            IF w_answer = '1'.
              DELETE FROM zib_contabil_err WHERE obj_key = wg_saidar-obj_key_rev.
              DELETE FROM zib_contabil     WHERE obj_key = wg_saidar-obj_key_rev.
              DELETE FROM zfit0082         WHERE obj_key = wg_saidar-obj_key_rev.
            ELSE.
              acao '&ATUAL'.
              CONTINUE.
            ENDIF.
          ELSE.
            CLEAR: wg_saida_exec.
            wg_saida_exec-icon   = icon_yellow_light.
            wg_saida_exec-racct  = wg_saidar-racct.
            wg_saida_exec-txt50  = wg_saidar-txt50.
            wg_saida_exec-msg    = TEXT-m01 .
            APPEND wg_saida_exec TO tg_saida_exec.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wg_saidar-obj_key_rev IS INITIAL.
      SELECT SINGLE *
        FROM zfit0082
        INTO wa_zfit0082
        WHERE bukrs     = s_bukrs-low
        AND   mes_ano   = s_mes-low
        AND   saknr     = wg_saidar-racct
        AND   vbund     = wg_saidar-rassc
        AND   obj_key_rev   NE ''.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

    ENDIF.

*    IF wg_saidar-vlr_ajust = 0.
*      CLEAR: wg_saida_exec.
*      wg_saida_exec-icon   = icon_red_light.
*      wg_saida_exec-racct  = wg_saidar-racct.
*      wg_saida_exec-txt50  = wg_saidar-txt50.
*      wg_saida_exec-msg    = text-m11 .
*      APPEND wg_saida_exec TO tg_saida_exec.
*      CONTINUE.
*    ENDIF.

    IF wg_saidar-belnr_rev IS NOT INITIAL.
      CLEAR: wg_saida_exec.
      wg_saida_exec-icon   = icon_red_light.
      wg_saida_exec-racct  = wg_saidar-racct.
      wg_saida_exec-txt50  = wg_saidar-txt50.
      wg_saida_exec-msg    = TEXT-m05 .
      APPEND wg_saida_exec TO tg_saida_exec.
      CONTINUE.
    ENDIF.

*    PERFORM gera_contabilr USING wg_saidar space CHANGING wl_obj_key.

*==================================================================
    CLEAR: wa_zib_chv, wa_zib.
    FREE: it_zib.
    SELECT *
      FROM zib_contabil
      INTO  TABLE it_zib
       WHERE obj_key EQ wg_saidar-obj_key.
    IF sy-subrc EQ 0.

      vg_ryear  = s_mes-low+2(4).
      IF wa_zib IS NOT INITIAL.
        CLEAR: wl_obj_key.
        PERFORM get_next_number  USING  'Z_SALDOMO2'
                                        '01'
                               CHANGING wl_obj_key.

        CONCATENATE 'ZFI0111' wl_obj_key vg_ryear INTO  wa_zib-obj_key.
        wa_zib-rg_atualizado = 'N'.
        wa_zib-seqitem = '000001'.
        wa_zib-bschl = '40'.

        APPEND wa_zib TO it_zib.

        wa_zib-rg_atualizado = 'N'.
        wa_zib-seqitem = '000002'.
        wa_zib-bschl = '50'.
        APPEND wa_zib TO it_zib.

*      <wg_saida>-belnr_rev = icon_configuration.
*      <wg_saida>-obj_key_rev = wa_zib-obj_key.
        SELECT SINGLE * FROM zfit0082 INTO @DATA(wl_0082)
          WHERE obj_key EQ @wg_saida-obj_key.
        IF wl_0082 IS NOT INITIAL.
          wl_0082-obj_key_rev = wa_zib-obj_key.
          MODIFY zfit0082 FROM wl_0082.
          COMMIT WORK.
        ENDIF.

        wg_saidar-obj_key_rev = wa_zib-obj_key.
      ENDIF.
    ELSE.
      MESSAGE s024(sd) WITH 'Chave lançamento' wa_zib-obj_key 'Não encontrada'.
      EXIT.
    ENDIF.

    CLEAR: wg_saidar-obj_key_rev, wa_zib.
    MODIFY tg_saidar FROM wg_saidar TRANSPORTING obj_key_rev.
  ENDLOOP.

  MODIFY zib_contabil FROM TABLE it_zib.

  COMMIT WORK.
  MESSAGE s024(sd) WITH 'Lançamento reversão realizada com sucesso.'.

ENDFORM.
