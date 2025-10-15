**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Bruna Guarez ( bruna.guarez@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Carolini Santos ( carolini.santos@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Contabilização Imposto diferido                                           |*
**/===========================================================================\*
REPORT zglr072.

INCLUDE zglr072_plt. "List Box Tipo

**********************************************************************
* tabelas
**********************************************************************
TABLES: t001, icon, zglt0100, zib_contabil, zglt0101, zglt0102,  zglt0103, zglt0106, zglt042.

**********************************************************************
* includes
**********************************************************************
*INCLUDE <icon>.

**********************************************************************
* field symbols
**********************************************************************
FIELD-SYMBOLS: <fs_fld> TYPE any.

**********************************************************************
* typesabelas
**********************************************************************

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.


TYPES: BEGIN OF ty_active,
         stcd1       TYPE lfa1-stcd1,
         serie       TYPE j_1bnfe_active-serie,
         nfnum9      TYPE j_1bnfe_active-nfnum9,
         cnpj_filial TYPE j_1bbranch-stcd1.
TYPES: END OF ty_active.




TYPES: BEGIN OF ty_alv_saida,
         status(4)      TYPE c,
         tp_imposto     TYPE zglt0101-tp_imposto,
         desc_imposto   TYPE zglt0101-desc_tp_imposto,
         conta          TYPE zglt0101-conta,
         desc_conta     TYPE zglt0101-desc_conta,
         saldo_brl      TYPE zde_vlr15_02,
         saldo_usd      TYPE zde_vlr15_02,
*Inicio Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
         s_irpj_brl     TYPE zde_vlr15_02, "Saldo
         s_irpj_usd     TYPE zde_vlr15_02,
         s_csll_brl     TYPE zde_vlr15_02,
         s_csll_usd     TYPE zde_vlr15_02,
*Fim Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
         movi_brl       TYPE zde_vlr15_02,
         movi_usd       TYPE zde_vlr15_02,
*Inicio Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
         m_irpj_brl     TYPE zde_vlr15_02, "Movimentação
         m_irpj_usd     TYPE zde_vlr15_02,
         m_csll_brl     TYPE zde_vlr15_02,
         m_csll_usd     TYPE zde_vlr15_02,
*Fim Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
         saldo_atu_brl  TYPE zde_vlr15_02,
         saldo_atu_usd  TYPE zde_vlr15_02,
         irpj_brl       TYPE zde_vlr15_02, "Saldo atual
         irpj_usd       TYPE zde_vlr15_02,
         csll_brl       TYPE zde_vlr15_02,
         csll_usd       TYPE zde_vlr15_02,
         doc_contabil   TYPE num10,
         doc_estorno    TYPE num10,
         obj_key        TYPE zib_contabil_chv-obj_key,
         lote           TYPE zglt0106-lote,
         doc_lcto       TYPE num10,
         obj_key_2      TYPE zib_contabil_chv-obj_key,
         lote_2         TYPE zglt0106-lote_2,
         doc_lcto_2     TYPE num10,
         doc_contabil_2 TYPE zglt0106-doc_contabil_2,
         doc_estorno_2  TYPE zglt0106-doc_estorno_2,
         line_color(4)  TYPE c,
         cellstyles     TYPE lvc_t_styl,
         color_cell     TYPE lvc_t_scol.
TYPES: END   OF ty_alv_saida.

TYPES: BEGIN OF ty_alv_saida_total,
         tp_imposto    TYPE zglt0101-tp_imposto,
         saldo_brl     TYPE zde_vlr15_02,
         saldo_usd     TYPE zde_vlr15_02,
*Inicio Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
         s_irpj_brl    TYPE zde_vlr15_02, "Saldo
         s_irpj_usd    TYPE zde_vlr15_02,
         s_csll_brl    TYPE zde_vlr15_02,
         s_csll_usd    TYPE zde_vlr15_02,
*Fim Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
         movi_brl      TYPE zde_vlr15_02,
         movi_usd      TYPE zde_vlr15_02,
*Inicio Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
         m_irpj_brl    TYPE zde_vlr15_02, "Movimentação
         m_irpj_usd    TYPE zde_vlr15_02,
         m_csll_brl    TYPE zde_vlr15_02,
         m_csll_usd    TYPE zde_vlr15_02,
*Fim Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
         saldo_atu_brl TYPE zde_vlr15_02,
         saldo_atu_usd TYPE zde_vlr15_02,
         irpj_brl      TYPE zde_vlr15_02,
         irpj_usd      TYPE zde_vlr15_02,
         csll_brl      TYPE zde_vlr15_02,
         csll_usd      TYPE zde_vlr15_02.
TYPES: END   OF ty_alv_saida_total.

TYPES: BEGIN OF ty_icon,
         id   TYPE icon-id,
         name TYPE icon-name.
TYPES: END   OF ty_icon.

TYPES: BEGIN OF ty_tcurr,
         kurst TYPE tcurr-kurst,
         fcurr TYPE tcurr-fcurr,
         tcurr TYPE tcurr-tcurr,
         gdatu TYPE tcurr-gdatu,
         ukurs TYPE tcurr-ukurs,
       END OF ty_tcurr.

TYPES: BEGIN OF ty_zib_contabil_err.
         INCLUDE STRUCTURE zib_contabil_err.
TYPES:   mark TYPE c,
       END OF ty_zib_contabil_err.

TYPES: BEGIN OF ty_zib_contabil_chv.
         INCLUDE STRUCTURE zib_contabil_chv.
TYPES:   bldat TYPE zib_contabil-bldat,
       END OF ty_zib_contabil_chv.



TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         budat TYPE bkpf-budat,
         stblg TYPE bkpf-stblg,
         stjah TYPE bkpf-stjah,
       END OF ty_bkpf.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.


**********************************************************************
* variaveis
**********************************************************************
DATA: l_sel_button         TYPE smp_dyntxt,
      l_opcao              TYPE char1,
      l_nfps               TYPE znfnum,
      l_data_char          TYPE char10,
      l_tabix              TYPE sy-tabix,
      l_icon_name          TYPE icon-name,
      l_leave              TYPE syst_ucomm,
*


      t_active             TYPE TABLE OF ty_active,
      ws_active            TYPE ty_active,
      t_j_1bbranch         TYPE TABLE OF j_1bbranch,
      ws_j_1bbranch        TYPE j_1bbranch,
      t_lfa1               TYPE TABLE OF lfa1,
      t_kna1               TYPE TABLE OF kna1,
      ws_lfa1              TYPE lfa1,
      t_zglt0101           TYPE TABLE OF zglt0101,
      t_tab                TYPE TABLE OF alsmex_tabline,
      t_raw                TYPE truxs_t_text_data,
      t_alv_saida          TYPE TABLE OF ty_alv_saida,
      t_icon               TYPE TABLE OF ty_icon,

      w_zglt0101           TYPE  zglt0101,

      w_alv_saida          TYPE ty_alv_saida,
      wl_zglt0106          TYPE zglt0106,
      wa_zib_contabil_err  TYPE ty_zib_contabil_err,
      wa_zib_contabil_chv  TYPE ty_zib_contabil_chv,
      it_zib_contabil_err  TYPE TABLE OF ty_zib_contabil_err,
      it_zib_contabil_chv  TYPE TABLE OF ty_zib_contabil_chv,
      wg_bkpf_fb08         TYPE ty_bkpf,
      wg_bkpf_fb08_e       TYPE ty_bkpf,

*------------------------------------
*---- ALV
*------------------------------------
      dg_splitter_1        TYPE REF TO cl_gui_splitter_container,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      picture              TYPE REF TO cl_gui_picture,
      l_graphic_conv       TYPE i,
      l_graphic_offs       TYPE i,
      graphic_size         TYPE i,
      l_graphic_xstr       TYPE xstring,
      url(255)             TYPE c,
      graphic_url(255),
      t_function           TYPE ui_functions,
      w_function           TYPE ui_func,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      ok_code              TYPE sy-ucomm,
*
      zcl_util             TYPE REF TO zcl_util,
      gr_tree              TYPE REF TO cl_salv_tree.

DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader.

DATA: variante         LIKE disvariant.
DATA: gs_variant_c TYPE disvariant.


DATA: wa_contas                  TYPE zlc_emp_contas,
      it_contas                  TYPE zct_emp_contas,
      it_saldo_contas            TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
      it_saldo_contas_2          TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
      it_saldo_contas_3          TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
      it_saldo_contas_anterior   TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
      it_saldo_contas_2_anterior TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
      it_saldo_contas_3_anterior TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
      it_saldo_contas_atual      TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
      it_saldo_contas_2_atual    TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
      it_saldo_contas_3_atual    TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
*      it_saldo_contas_4 TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
*      it_saldo_contas_5 TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
*      it_saldo_contas_6 TYPE TABLE OF  zde_fi_gl_saldo_faglflext,
      t_tcurr                    TYPE TABLE OF ty_tcurr,
      t_coltab                   TYPE lvc_t_scol.

DATA: it_dta   TYPE STANDARD TABLE OF bdcdata WITH HEADER LINE,
      wa_dta   TYPE bdcdata,
      wg_bdc   TYPE bdcdata,
      tg_bdc   TYPE TABLE OF bdcdata,
      tg_msg   TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wg_msg   TYPE bdcmsgcoll,
      opt      TYPE ctu_params,
      wa_tcurr TYPE ty_tcurr.

DATA: v_p_db_tab(8)  TYPE c,
      v_p_stcnam(12) TYPE c,
      v_p_scmant(4)  TYPE c,
      v_p_title(40)  TYPE c.


DATA: v_irpj     TYPE zglt0103-percentual,
      v_csll     TYPE zglt0103-percentual,
      v_desc_emp TYPE t001-butxt.

DATA: v_ultimo_dia TYPE sy-datum,
      v_data1      TYPE sy-datum,
      v_data       TYPE char10,
      xtx_usd      TYPE tcurr-ukurs,
      v_tp_taxa    TYPE z_tp_taxa,
      v_mes        TYPE zglt042-mes_de,
      "v_ano        TYPE  sy-uzeit,
      v_ano        TYPE char4.

*------------------------------------
*---- figuras
*------------------------------------
DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

**********************************************************************
* ranges
**********************************************************************
RANGES: r_name      FOR icon-name.

**********************************************************************
* constantes
**********************************************************************
CONSTANTS: BEGIN OF gc,
             verde   TYPE lvc_s_colo-col      VALUE '5',
             amarelo TYPE lvc_s_colo-col      VALUE '3',
             padrao  TYPE lvc_s_colo-col      VALUE '2',
             azul    TYPE lvc_s_colo-col      VALUE '1',
             inv     TYPE lvc_s_colo-inv      VALUE '0',
             int     TYPE lvc_s_colo-int      VALUE '0',
           END OF gc.

**********************************************************************
* SELECTION-SCREEN
**********************************************************************

*-----------------------------------------------------------------*
*   TYPE-POOLS                                                    *
*-----------------------------------------------------------------*
TYPE-POOLS: slis.

*-----------------------------------------------------------------*
*   INTERNAL TABLES                                               *
*-----------------------------------------------------------------*
DATA: t_rkey    TYPE STANDARD TABLE OF rsvarkey WITH HEADER LINE,
      t_selctab TYPE STANDARD TABLE OF rsscr WITH HEADER LINE,
      t_vari    TYPE STANDARD TABLE OF rvari WITH HEADER LINE,
      it_extab  TYPE slis_t_extab,
      wa_extab  LIKE LINE OF it_extab.

*-----------------------------------------------------------------*
*   VARIABLES                                                     *
*-----------------------------------------------------------------*
DATA: w_variant        TYPE rsvar-variant,
      w_user_vari      TYPE rsvar-variant,
      w_vari_report    TYPE rsvar-report,
      sel_variant      TYPE rsvar-variant,
      sel_variant_text TYPE rsvar-vtext,
      w_report         TYPE rsvar-report,
      variant_exists   TYPE c.

**********************************************************************


PARAMETERS:     p_submit TYPE char20 NO-DISPLAY.

*Inicio Alteração - Leandro Valentim ferreira - 10.08.23 - #119411
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-142.
*  SELECTION-SCREEN PUSHBUTTON 01(17)  bt01 USER-COMMAND fc01.
*  SELECTION-SCREEN PUSHBUTTON 19(17)  bt02 USER-COMMAND fc02.
*  SELECTION-SCREEN PUSHBUTTON 36(29)  bt03 USER-COMMAND fc03.
*  SELECTION-SCREEN PUSHBUTTON 70(17)  bt04 USER-COMMAND fc04.
*  SELECTION-SCREEN PUSHBUTTON 89(26) bt05 USER-COMMAND fc05.
*  SELECTION-SCREEN PUSHBUTTON 117(28) bt06 USER-COMMAND fc06.
SELECTION-SCREEN: END OF BLOCK b1.

"SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME.
"SELECTION-SCREEN: END OF BLOCK b2.
*Fim Alteração - Leandro Valentim ferreira - 10.08.23 - #119411

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-010.
  SELECT-OPTIONS: s_empre  FOR t001-bukrs          NO-EXTENSION NO INTERVALS, "OBLIGATORY,
                  s_mes    FOR zglt042-mes_de      NO-EXTENSION NO INTERVALS, "OBLIGATORY,
                  s_ano    FOR zglt042-ano_de      NO-EXTENSION NO INTERVALS, "OBLIGATORY,
                  s_tp_imp FOR zglt0101-tp_imposto NO-EXTENSION NO INTERVALS.
  PARAMETERS:     p_lbtipo AS LISTBOX VISIBLE LENGTH 12 DEFAULT '1'.
SELECTION-SCREEN END   OF BLOCK b3.

*Inicio Alteração - Leandro Valentim ferreira - 10.08.23 - #119411
***SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'
***SELECTION-SCREEN FUNCTION KEY 2.  "Will have a function code of 'FC02'
***SELECTION-SCREEN FUNCTION KEY 3.  "Will have a function code of 'FC03'
***SELECTION-SCREEN FUNCTION KEY 4.  "Will have a function code of 'FC04'
***SELECTION-SCREEN FUNCTION KEY 5.  "Will have a function code of 'FC05'
*Fim Alteração - Leandro Valentim ferreira - 10.08.23 - #119411

**********************************************************************
* classes / implementacoes
**********************************************************************
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

  ENDMETHOD.

  METHOD on_hotspot_click.

    DATA: wl_ano(4).

    TYPES: BEGIN OF ty_itab ,
             name(80) TYPE c,
           END OF ty_itab.

    DATA: msg_alv  TYPE char80,
          itab_msg TYPE TABLE OF ty_itab,
          wtab_msg TYPE  ty_itab.

    READ TABLE t_alv_saida INTO w_alv_saida INDEX e_row_id-index.
    IF sy-subrc = 0.
      CASE e_column_id .
        WHEN 'DOC_CONTABIL'.
          IF w_alv_saida-doc_contabil IS NOT INITIAL.

            SET PARAMETER ID 'BLN' FIELD w_alv_saida-doc_contabil.
            SET PARAMETER ID 'BUK' FIELD s_empre-low.
            SET PARAMETER ID 'GJR' FIELD s_ano-low.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

          ENDIF.

        WHEN 'DOC_CONTABIL_2'.
          IF w_alv_saida-doc_contabil_2 IS NOT INITIAL.

            SET PARAMETER ID 'BLN' FIELD w_alv_saida-doc_contabil_2.
            SET PARAMETER ID 'BUK' FIELD s_empre-low.
            SET PARAMETER ID 'GJR' FIELD s_ano-low.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

          ENDIF.

        WHEN 'DOC_ESTORNO'.
          IF w_alv_saida-doc_estorno IS NOT INITIAL.

            SET PARAMETER ID 'BLN' FIELD w_alv_saida-doc_estorno.
            SET PARAMETER ID 'BUK' FIELD s_empre-low.
            SET PARAMETER ID 'GJR' FIELD s_ano-low.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

          ENDIF.

        WHEN 'DOC_ESTORNO_2'.
          IF w_alv_saida-doc_estorno_2 IS NOT INITIAL.

            SET PARAMETER ID 'BLN' FIELD w_alv_saida-doc_estorno_2.
            SET PARAMETER ID 'BUK' FIELD s_empre-low.
            SET PARAMETER ID 'GJR' FIELD s_ano-low.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

          ENDIF.

        WHEN 'LOTE'.
          IF w_alv_saida-lote IS NOT INITIAL.

*            SET PARAMETER ID 'BUK' FIELD s_empre-low.
*            SET PARAMETER ID 'lote' FIELD w_alv_saida-lote.
*            CALL TRANSACTION 'ZGL020' AND SKIP FIRST SCREEN.



            SUBMIT zgl018       WITH p_bukrs  = s_empre-low
                                WITH p_lote = w_alv_saida-lote
                                WITH p_eqdif = 'X'
                                AND RETURN.

          ENDIF.

        WHEN 'LOTE_2'.
          IF w_alv_saida-lote_2 IS NOT INITIAL.

*            SET PARAMETER ID 'BUK' FIELD s_empre-low.
*            SET PARAMETER ID 'lote' FIELD w_alv_saida-lote.
*            CALL TRANSACTION 'ZGL020' AND SKIP FIRST SCREEN.



            SUBMIT zgl018       WITH p_bukrs  = s_empre-low
                                WITH p_lote = w_alv_saida-lote_2
                                WITH p_eqdif = 'X'
                                AND RETURN.

          ENDIF.


        WHEN 'STATUS'.

          IF w_alv_saida-status = icon_led_red .
            SELECT *
               FROM zib_contabil_err
               INTO TABLE it_zib_contabil_err
               WHERE obj_key  = w_alv_saida-obj_key.

            wtab_msg-name    = '------------------------MENSAGEM ERRO---------------------------------------'.
            APPEND wtab_msg TO itab_msg .
            CLEAR wtab_msg.
            LOOP AT it_zib_contabil_err INTO wa_zib_contabil_err.
              wtab_msg-name = wa_zib_contabil_err-message.
              APPEND wtab_msg TO itab_msg .
              CLEAR wtab_msg.
            ENDLOOP.

            CONCATENATE 'DOCUMENTO ' w_alv_saida-doc_contabil INTO msg_alv SEPARATED BY space.
            CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
              EXPORTING
                endpos_col   = 140
                endpos_row   = 20
                startpos_col = 60
                startpos_row = 15
                titletext    = msg_alv
              TABLES
                valuetab     = itab_msg
              EXCEPTIONS
                break_off    = 1
                OTHERS       = 2.
          ELSEIF w_alv_saida-status = icon_led_yellow.
            wtab_msg-name    = '------------------------ MENSAGEM ---------------------------------------'.
            APPEND wtab_msg TO itab_msg .
            CLEAR wtab_msg.

            IF w_alv_saida-doc_lcto IS NOT INITIAL.
              wtab_msg = 'AGUARDANDO POCESSAMENTO DA ZIB_CONTABIL'.
              APPEND wtab_msg TO itab_msg .
            ELSE.

              wtab_msg = 'DOCUMENTO CONTABIL AINDA NÃO FOI GERADO'.
              APPEND wtab_msg TO itab_msg .
            ENDIF.
            CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
              EXPORTING
                endpos_col   = 140
                endpos_row   = 20
                startpos_col = 60
                startpos_row = 15
                titletext    = msg_alv
              TABLES
                valuetab     = itab_msg
              EXCEPTIONS
                break_off    = 1
                OTHERS       = 2.

          ENDIF.


      ENDCASE.


    ENDIF.
  ENDMETHOD.

  METHOD data_changed_finished.

    DATA: wa_good_cells TYPE lvc_s_modi.
    DATA: v_tabix TYPE sy-tabix.

    DATA: wa_zglt0110 TYPE zglt0110.
    DATA:
      v_irpj_brl TYPE zde_vlr15_02,
      v_irpj_usd TYPE zde_vlr15_02,
      v_csll_brl TYPE zde_vlr15_02,
      v_csll_usd TYPE zde_vlr15_02.


    FIELD-SYMBOLS: <w_saida>  TYPE ty_alv_saida.

    CLEAR: it_sel_rows[], wa_sel_rows.

    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_index_rows = it_sel_rows.

    CLEAR: v_csll, v_irpj.
    SELECT SINGLE percentual FROM  zglt0103 INTO v_csll WHERE desc_taxa EQ 'CSLL'.
    SELECT SINGLE percentual FROM  zglt0103 INTO v_irpj WHERE desc_taxa EQ 'IRPJ'.

    LOOP AT et_good_cells INTO wa_good_cells.

      READ TABLE t_alv_saida ASSIGNING <w_saida> INDEX wa_good_cells-row_id.

      SELECT SINGLE * FROM zglt0110 INTO wa_zglt0110 WHERE conta = <w_saida>-conta.

      v_tabix  = wa_good_cells-row_id.
      IF e_modified EQ abap_true.
        IF wa_good_cells-fieldname EQ 'SALDO_BRL' .
          "MOVE wa_good_cells-value TO w_alv_saida-saldo_brl.

*&---------------------------------Bug Solto 147433 / AOENNING / &*
          "movimentacao BRL saldo 31/12 - saldo mes atual
*          if <w_saida>-saldo_brl is not initial and   <w_saida>-saldo_atu_brl is not initial.
          IF <w_saida>-saldo_brl IS NOT INITIAL.
            <w_saida>-movi_brl = <w_saida>-saldo_brl - <w_saida>-saldo_atu_brl.
*** BUG - 153691 - Inicio - CBRAND
            IF <w_saida>-saldo_brl IS NOT INITIAL.
              v_irpj_brl =  <w_saida>-saldo_brl * ( v_irpj / 100 ).  "'IRPJ' BRL
              v_csll_brl =  <w_saida>-saldo_brl * ( v_csll / 100 ).
            ELSE.
              v_irpj_brl = 0.
              v_csll_brl = 0.
            ENDIF.

            v_mes = s_mes-low + 1.
            v_ano = s_ano-low.
            IF v_mes > 12.
              v_mes = 01.
              v_ano = s_ano-low + 1.
            ENDIF.
            CONCATENATE '01.' v_mes '.' v_ano INTO v_data.

            PERFORM f_taxa_usd USING v_data
                  CHANGING xtx_usd.

            PERFORM f_tp_taxa USING <w_saida>-tp_imposto
                              CHANGING v_tp_taxa.

            CASE v_tp_taxa. "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01 É HISTORICO 02 É FECHAMENTO

              WHEN '02'. "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 02 É FECHAMENTO

                IF xtx_usd IS NOT INITIAL.
                  IF <w_saida>-saldo_brl IS NOT INITIAL.
                    IF v_irpj_brl IS NOT INITIAL.
                      v_irpj_usd = v_irpj_brl / abs( xtx_usd ).
                    ELSE.
                      v_irpj_usd = <w_saida>-saldo_usd * ( v_irpj / 100 ).
                    ENDIF.

                    IF v_csll_brl IS NOT INITIAL .
                      v_csll_usd = v_csll_brl / abs( xtx_usd ).
                    ELSE.
                      v_csll_usd = <w_saida>-saldo_usd * ( v_csll / 100 ).
                    ENDIF.
                  ENDIF.
                ENDIF.
            ENDCASE.

            IF wa_zglt0110 IS INITIAL OR ( wa_zglt0110-csll IS INITIAL AND  wa_zglt0110-irpj IS INITIAL ).
              <w_saida>-s_irpj_brl = v_irpj_brl.
              <w_saida>-s_csll_brl = v_csll_brl.
              <w_saida>-s_irpj_usd = v_irpj_usd.
              <w_saida>-s_csll_usd = v_csll_usd.
            ELSE.
              IF wa_zglt0110-irpj IS NOT INITIAL.
                <w_saida>-s_irpj_brl = v_irpj_brl.
                <w_saida>-s_irpj_usd = v_irpj_usd.

              ELSE.
                <w_saida>-s_irpj_brl = 0.
                IF v_tp_taxa = '02'.
                  <w_saida>-s_irpj_usd = 0.
                ENDIF.
              ENDIF.

              IF wa_zglt0110-csll IS NOT INITIAL.
                <w_saida>-s_csll_brl = v_csll_brl.
                <w_saida>-s_csll_usd = v_csll_usd.
              ELSE.
                <w_saida>-s_csll_brl = 0.
                IF v_tp_taxa = '02'.
                  <w_saida>-s_csll_usd = 0.
                ENDIF.
              ENDIF.
            ENDIF.

            IF v_tp_taxa = '02'.

              IF wa_zglt0110 IS INITIAL OR ( wa_zglt0110-csll IS INITIAL AND  wa_zglt0110-irpj IS INITIAL ).
                <w_saida>-s_irpj_usd = v_irpj_usd.
                <w_saida>-s_csll_usd = v_csll_usd.
              ELSE.
                IF wa_zglt0110-irpj IS NOT INITIAL.
                  <w_saida>-s_irpj_usd = v_irpj_usd.
                ELSE.
                  <w_saida>-s_irpj_usd = 0.
                ENDIF.

                IF wa_zglt0110-csll IS NOT INITIAL.
                  <w_saida>-s_csll_usd = v_csll_usd.
                ELSE.
                  <w_saida>-s_csll_usd = 0.
                ENDIF.
              ENDIF.
            ENDIF.
*** BUG - 153691 - Fim - CBRAND
          ELSE.
            MOVE 0 TO <w_saida>-movi_brl.
          ENDIF.
*** BUG - 153691 - Inicio - CBRAND
          <w_saida>-m_irpj_brl =  <w_saida>-s_irpj_brl - <w_saida>-irpj_brl.
          <w_saida>-m_irpj_usd =  <w_saida>-s_irpj_usd - <w_saida>-irpj_usd.
          <w_saida>-m_csll_brl =  <w_saida>-s_csll_brl - <w_saida>-csll_brl.
          <w_saida>-m_csll_usd =  <w_saida>-s_csll_usd - <w_saida>-csll_usd.
*** BUG - 153691 - Fim - CBRAND
        ELSEIF wa_good_cells-fieldname EQ 'SALDO_USD'.

          "MOVE wa_good_cells-value TO w_alv_saida-saldo_usd.
          "movimentacao USD
*          if <w_saida>-saldo_usd is not initial and   <w_saida>-saldo_atu_usd is not initial.
          IF <w_saida>-saldo_usd IS NOT INITIAL.
            <w_saida>-movi_usd = <w_saida>-saldo_usd - <w_saida>-saldo_atu_usd.
          ELSE.
            MOVE 0 TO <w_saida>-movi_usd.
          ENDIF.
*** BUG - 153691 - Inicio - CBRAND
          IF <w_saida>-saldo_usd IS NOT INITIAL.

            CASE v_tp_taxa. "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01 É HISTORICO 02 É FECHAMENTO
              WHEN '01'. "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01 .

                v_irpj_usd = <w_saida>-saldo_usd * ( v_irpj / 100 ).  "'IRPJ' USD
                v_csll_usd = <w_saida>-saldo_usd * ( v_csll / 100 ).  "'CSLL' USD

            ENDCASE.
            IF v_tp_taxa = '01'. "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01.
              IF wa_zglt0110 IS INITIAL OR ( wa_zglt0110-csll IS INITIAL AND  wa_zglt0110-irpj IS INITIAL ).
                <w_saida>-s_irpj_usd = v_irpj_usd.
                <w_saida>-s_csll_usd = v_csll_usd.
              ELSE.

                IF wa_zglt0110-irpj IS NOT INITIAL.
                  <w_saida>-s_irpj_usd = v_irpj_usd.
                ELSE.
                  <w_saida>-s_irpj_usd = 0.
                ENDIF.

                IF wa_zglt0110-csll IS NOT INITIAL.
                  <w_saida>-s_csll_usd = v_csll_usd.
                ELSE.
                  <w_saida>-s_csll_usd = 0.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            IF v_tp_taxa EQ '01'.
              MOVE 0 TO <w_saida>-s_csll_usd.
              MOVE 0 TO <w_saida>-s_irpj_usd.
            ENDIF.
          ENDIF.

          <w_saida>-m_irpj_brl =  <w_saida>-s_irpj_brl - <w_saida>-irpj_brl.
          <w_saida>-m_irpj_usd =  <w_saida>-s_irpj_usd - <w_saida>-irpj_usd.
          <w_saida>-m_csll_brl =  <w_saida>-s_csll_brl - <w_saida>-csll_brl.
          <w_saida>-m_csll_usd =  <w_saida>-s_csll_usd - <w_saida>-csll_usd.

*** BUG - 153691 - Fim - CBRAND

        ELSEIF wa_good_cells-fieldname EQ 'SALDO_ATU_BRL'.
*&---------------------------------Bug Solto 147433 / AOENNING / &*
          "movimentacao BRL saldo 31/12 - saldo mes atual
*          if <w_saida>-saldo_brl is not initial and   <w_saida>-saldo_atu_brl is not initial.
          IF <w_saida>-saldo_atu_brl IS NOT INITIAL.
            <w_saida>-movi_brl = <w_saida>-saldo_brl - <w_saida>-saldo_atu_brl.
          ELSE.
            MOVE 0 TO <w_saida>-movi_brl.
          ENDIF.
*&---------------------------------Bug Solto 147433 / AOENNING / &*
          IF <w_saida>-saldo_atu_brl IS NOT INITIAL.
            v_irpj_brl =  <w_saida>-saldo_atu_brl * ( v_irpj / 100 ).  "'IRPJ' BRL
            v_csll_brl =  <w_saida>-saldo_atu_brl * ( v_csll / 100 ).
          ELSE.
            v_irpj_brl = 0.
            v_csll_brl = 0.
          ENDIF.

          v_mes = s_mes-low + 1.
          v_ano = s_ano-low.
          IF v_mes > 12.
            v_mes = 01.
            v_ano = s_ano-low + 1.
          ENDIF.
          CONCATENATE '01.' v_mes '.' v_ano INTO v_data.
          "fm auste bug 98606
          "CONCATENATE '01.' s_mes-low '.' s_ano-low INTO v_data.
          PERFORM f_taxa_usd USING v_data
                CHANGING xtx_usd.

          PERFORM f_tp_taxa USING <w_saida>-tp_imposto
                            CHANGING v_tp_taxa.
          "ajuste bug 98606 se for tipo de taxa 02 calcula o irpj e csll USD com base no campo de saldo atual BRL
          CASE v_tp_taxa. "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01 É HISTORICO 02 É FECHAMENTO

            WHEN '02'. "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 02 É FECHAMENTO

              IF xtx_usd IS NOT INITIAL.
                IF <w_saida>-saldo_atu_brl IS NOT INITIAL.
                  IF v_irpj_brl IS NOT INITIAL.
                    v_irpj_usd = v_irpj_brl / abs( xtx_usd ).
                  ELSE.
                    v_irpj_usd = <w_saida>-saldo_atu_usd * ( v_irpj / 100 ).
                  ENDIF.

                  IF v_csll_brl IS NOT INITIAL .
                    v_csll_usd = v_csll_brl / abs( xtx_usd ).
                  ELSE.
                    v_csll_usd = <w_saida>-saldo_atu_usd * ( v_csll / 100 ).
                  ENDIF.
                ENDIF.
              ENDIF.

          ENDCASE.

          IF wa_zglt0110 IS INITIAL OR ( wa_zglt0110-csll IS INITIAL AND  wa_zglt0110-irpj IS INITIAL ).
            <w_saida>-irpj_brl = v_irpj_brl.
            <w_saida>-csll_brl = v_csll_brl.
            <w_saida>-irpj_usd = v_irpj_usd.
            <w_saida>-csll_usd = v_csll_usd.
          ELSE.
            IF wa_zglt0110-irpj IS NOT INITIAL.
              <w_saida>-irpj_brl = v_irpj_brl.
              <w_saida>-irpj_usd = v_irpj_usd.
            ELSE.
              <w_saida>-irpj_brl = 0.
              IF v_tp_taxa = '02'.
                <w_saida>-irpj_usd = 0.
              ENDIF.
            ENDIF.

            IF wa_zglt0110-csll IS NOT INITIAL.
              <w_saida>-csll_brl = v_csll_brl.
              <w_saida>-csll_usd = v_csll_usd.
            ELSE.
              <w_saida>-csll_brl = 0.
              IF v_tp_taxa = '02'.
                <w_saida>-csll_usd = 0.
              ENDIF.
            ENDIF.

          ENDIF.

          IF v_tp_taxa = '02'.

            IF wa_zglt0110 IS INITIAL OR ( wa_zglt0110-csll IS INITIAL AND  wa_zglt0110-irpj IS INITIAL ).
              <w_saida>-irpj_usd = v_irpj_usd.
              <w_saida>-csll_usd = v_csll_usd.
            ELSE.
              IF wa_zglt0110-irpj IS NOT INITIAL.
                <w_saida>-irpj_usd = v_irpj_usd.
              ELSE.
                <w_saida>-irpj_usd = 0.
              ENDIF.

              IF wa_zglt0110-csll IS NOT INITIAL.
                <w_saida>-csll_usd = v_csll_usd.
              ELSE.
                <w_saida>-csll_usd = 0.
              ENDIF.
            ENDIF.
          ENDIF.
*** BUG - 153691 - Inicio - CBRAND
          <w_saida>-m_irpj_brl =  <w_saida>-s_irpj_brl - <w_saida>-irpj_brl.
          <w_saida>-m_irpj_usd =  <w_saida>-s_irpj_usd - <w_saida>-irpj_usd.
          <w_saida>-m_csll_brl =  <w_saida>-s_csll_brl - <w_saida>-csll_brl.
          <w_saida>-m_csll_usd =  <w_saida>-s_csll_usd - <w_saida>-csll_usd.
*** BUG - 153691 - fim - CBRAND
*&---------------------------------Bug Solto 147433 / AOENNING / &*
        ELSEIF wa_good_cells-fieldname EQ 'SALDO_ATU_USD'.

          "MOVE wa_good_cells-value TO w_alv_saida-saldo_atu_usd.
          "movimentacao USD
*          if <w_saida>-saldo_usd is not initial and   <w_saida>-saldo_atu_usd is not initial.
          IF <w_saida>-saldo_atu_usd IS NOT INITIAL.
            <w_saida>-movi_usd = <w_saida>-saldo_usd - <w_saida>-saldo_atu_usd.
          ELSE.
            MOVE 0 TO <w_saida>-movi_usd.
          ENDIF.
*&---------------------------------Bug Solto 147433 / AOENNING / &*
          IF <w_saida>-saldo_atu_usd IS NOT INITIAL.
            "ajuste da data que busca a taxa do dolar. sempre a do mes subsequente ao parametro bug #98606
*            v_mes = s_mes-low + 1.
*            v_ano = s_ano-low.
*            IF v_mes > 12.
*              v_mes = 01.
*              v_ano = s_ano-low + 1.
*            ENDIF.
*            CONCATENATE '01.' v_mes '.' v_ano INTO v_data.
*            "fm auste bug 98606
*            "CONCATENATE '01.' s_mes-low '.' s_ano-low INTO v_data.
*            PERFORM f_taxa_usd USING v_data
*                  CHANGING xtx_usd.
*
*            PERFORM f_tp_taxa USING <w_saida>-tp_imposto
*                              CHANGING v_tp_taxa.

            CASE v_tp_taxa. "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01 É HISTORICO 02 É FECHAMENTO
              WHEN '01'. "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01 .

                v_irpj_usd = <w_saida>-saldo_atu_usd * ( v_irpj / 100 ).  "'IRPJ' USD
                v_csll_usd = <w_saida>-saldo_atu_usd * ( v_csll / 100 ).  "'CSLL' USD

*              WHEN '02'.
*
*                IF xtx_usd IS NOT INITIAL.
*
*                  IF v_irpj_brl IS NOT INITIAL.
*                    v_irpj_usd = v_irpj_brl / abs( xtx_usd ).
*                  ELSE.
*                    v_irpj_usd = <w_saida>-saldo_atu_usd * ( v_irpj / 100 ).
*                  ENDIF.
*
*                  IF v_csll_brl IS NOT INITIAL.
*                    v_csll_usd = v_csll_brl / abs( xtx_usd ).
*                  ELSE.
*                    v_csll_usd = <w_saida>-saldo_atu_usd * ( v_csll / 100 ).
*                  ENDIF.
*
*                ENDIF.

            ENDCASE.
            IF v_tp_taxa = '01'. "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01.
              IF wa_zglt0110 IS INITIAL OR ( wa_zglt0110-csll IS INITIAL AND  wa_zglt0110-irpj IS INITIAL ).
                <w_saida>-irpj_usd = v_irpj_usd.
                <w_saida>-csll_usd = v_csll_usd.
              ELSE.

                IF wa_zglt0110-irpj IS NOT INITIAL.
                  <w_saida>-irpj_usd = v_irpj_usd.
                ELSE.

                  <w_saida>-irpj_usd = 0.

                ENDIF.

                IF wa_zglt0110-csll IS NOT INITIAL.
                  <w_saida>-csll_usd = v_csll_usd.
                ELSE.

                  <w_saida>-csll_usd = 0.

                ENDIF.

              ENDIF.
            ENDIF.
          ELSE.
            IF v_tp_taxa EQ '01'.
              MOVE 0 TO <w_saida>-csll_usd.
              MOVE 0 TO <w_saida>-irpj_usd.
            ENDIF.
          ENDIF.
*** BUG - 153691 - Inicio - CBRAND
          <w_saida>-m_irpj_brl =  <w_saida>-s_irpj_brl - <w_saida>-irpj_brl.
          <w_saida>-m_irpj_usd =  <w_saida>-s_irpj_usd - <w_saida>-irpj_usd.
          <w_saida>-m_csll_brl =  <w_saida>-s_csll_brl - <w_saida>-csll_brl.
          <w_saida>-m_csll_usd =  <w_saida>-s_csll_usd - <w_saida>-csll_usd.
*** BUG - 153691 - Fim - CBRAND
        ELSEIF wa_good_cells-fieldname EQ 'movi_brl'.

          "MOVE wa_good_cells-value TO w_alv_saida-movi_brl.

        ELSEIF wa_good_cells-fieldname EQ 'movi_usd'.

          "MOVE wa_good_cells-value TO w_alv_saida-movi_usd.

        ELSEIF wa_good_cells-fieldname EQ 'irpj_brl' .

          "MOVE wa_good_cells-value TO w_alv_saida-irpj_brl.

        ELSEIF wa_good_cells-fieldname EQ 'irpj_usd' .

          "MOVE wa_good_cells-value TO w_alv_saida-irpj_usd.

        ELSEIF wa_good_cells-fieldname EQ 'csll_brl' .

          "MOVE wa_good_cells-value TO w_alv_saida-csll_brl.

        ELSEIF wa_good_cells-fieldname EQ 'csll_usd' .

          "MOVE wa_good_cells-value TO w_alv_saida-csll_usd.

        ENDIF.


      ENDIF.

    ENDLOOP.

    IF et_good_cells[] IS NOT INITIAL.
      PERFORM f_salvar.
      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = w_stable.
    ENDIF.



  ENDMETHOD.

ENDCLASS.

**********************************************************************
*SELECTION-SCREEN p_file
**********************************************************************
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  PERFORM f_select_file USING p_file.


**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN.

  FREE MEMORY ID 'ZLESR0154'.
  FREE: l_leave.

  CASE sy-ucomm.
    WHEN 'BT001'."'FC01'.
      v_p_db_tab = 'ZGLT0100'.
      v_p_stcnam = 'ZGLT0100_OUT'.
      v_p_scmant = '0147'.
      v_p_title = 'Cadastro Tipo de Imposto Diferido'.
    WHEN 'BT002'."'FC02'.
      v_p_db_tab  = 'ZGLT0101'.
      v_p_stcnam = 'ZGLT0101_OUT'.
      v_p_scmant = '0148'.
      v_p_title = 'Vincular Contas Imposto Diferido'.

    WHEN 'BT003'."'FC03'.
      v_p_db_tab  = 'ZGLT0102'.
      v_p_stcnam = 'ZGLT0102_OUT'.
      v_p_scmant = '0151'.
      v_p_title = 'Parametrizar Contabilização Impostos Diferidos'.

    WHEN 'BT004'."'FC04'.
      v_p_db_tab  = 'ZGLT0103'.
      v_p_stcnam = 'ZGLT0103_OUT'.
      v_p_scmant = '0152'.
      v_p_title = 'Taxas Imposto Diferido'.
    WHEN 'BT005'."'FC05'.
      v_p_db_tab  = 'ZGLT0110'.
      v_p_stcnam = 'ZGLT0110_OUT'.
      v_p_scmant = '0168'.
      v_p_title = 'Config. Imposto p/ Conta'.
*Inicio Alteração - Leandro Valentim ferreira - 10.08.23 - #119411
    WHEN 'BT006'."'FC06'.
      v_p_db_tab  = 'ZGLT0113'.
      v_p_stcnam = 'ZGLT0113_OUT'.
      v_p_scmant = '0206'.
      v_p_title = 'Vinculo de Contas - Resultado'.
*Fim Alteração - Leandro Valentim ferreira - 10.08.23 - #119411
    WHEN 'ONLI'.
      PERFORM f_start_selection.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.


  IF v_p_db_tab IS NOT INITIAL.
    l_opcao = '1'.
    SUBMIT zregister_data       WITH p_db_tab  = v_p_db_tab
                                WITH p_stcnam = v_p_stcnam
                                WITH p_scmant = v_p_scmant
                                WITH p_title = v_p_title
    AND RETURN.
    CLEAR: v_p_db_tab, v_p_stcnam, v_p_scmant, v_p_title.
  ENDIF.

**********************************************************************
*SELECTION-SCREEN output
**********************************************************************
AT SELECTION-SCREEN OUTPUT.

*Inicio Alteração - Leandro Valentim ferreira - 10.08.23 - #119411
  "CONCATENATE icon_submit   TEXT-b01 INTO bt01. "Tipo de Imposto
  "CONCATENATE icon_submit   TEXT-b02 INTO bt02. "Vincular Contas
  "CONCATENATE icon_simulate TEXT-b03 INTO bt03. "Parametrizar Contabilização
  "CONCATENATE icon_simulate TEXT-b04 INTO bt04. "Taxas IRPJ CSLL
  "CONCATENATE icon_submit   TEXT-b05 INTO bt05. "Config. Imposto p/ Conta
  "CONCATENATE icon_submit   TEXT-b06 INTO bt06. "Vinculo de Contas - Resultado
*Fim Alteração - Leandro Valentim ferreira - 10.08.23 - #119411


  IF l_opcao = 2 OR l_opcao = 3.
    LOOP AT SCREEN.
      screen-active    = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
    ENDLOOP.


**********************************************************************
* inicio
**********************************************************************
*INITIALIZATION.

    FREE MEMORY ID 'ZGLR072'.
    l_leave = 'LEAVE'.
    EXPORT l_leave FROM l_leave TO MEMORY ID 'ZGLR072'.

    l_opcao                = '1'.

  ENDIF.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

*-----------------------------------------------------------------*
*   CONTAS DE RESULTADO  - PSA                                                   *
*-----------------------------------------------------------------*

  RANGES lr_ktoks FOR ska1-ktoks.

  lr_ktoks-sign = 'I'.
  lr_ktoks-option = 'EQ'.
  lr_ktoks-low = 'YB05'.
  APPEND lr_ktoks.
  lr_ktoks-sign = 'I'.
  lr_ktoks-option = 'EQ'.
  lr_ktoks-low = 'YB06'.
  APPEND lr_ktoks.
  lr_ktoks-sign = 'I'.
  lr_ktoks-option = 'EQ'.
  lr_ktoks-low = 'YB07'.
  APPEND lr_ktoks.
  lr_ktoks-sign = 'I'.
  lr_ktoks-option = 'EQ'.
  lr_ktoks-low = 'YB08'.
  APPEND lr_ktoks.

  SET PF-STATUS '1000'.

  FREE MEMORY ID 'ZGLR072'.
  l_leave = 'LEAVE'.
  EXPORT l_leave FROM l_leave TO MEMORY ID 'ZGLR072'.

  l_opcao                = '1'.

  "CONCATENATE icon_dangerous_goods 'Tipo de Imposto' INTO bt01.
  "CONCATENATE icon_warehouse   'Vincular Contas' INTO bt02.
  "CONCATENATE icon_work_center 'Parametrizar Contabilização' INTO bt03.
  "CONCATENATE icon_work_center 'Taxas IRPJ CSLL' INTO bt04.
***  CONCATENATE icon_work_center 'Config. Imposto p/ Conta' INTO bt05.
***  CONCATENATE icon_submit   text-b06 INTO bt06. "Parametrizar Reflexa PL



*Inicio Alteração - Leandro Valentim ferreira - 10.08.23 - #119411
***  l_sel_button-icon_id   = icon_dangerous_goods.
***  l_sel_button-icon_text = 'Tipo de Imposto'.
***  sscrfields-functxt_01  = l_sel_button.
***
***  l_sel_button-icon_id   = icon_warehouse.
***  l_sel_button-icon_text = 'Vincular Contas'.
***  sscrfields-functxt_02  = l_sel_button.
***
***  l_sel_button-icon_id   = icon_work_center.
***  l_sel_button-icon_text = 'Parametrizar Contabilização'.
***  sscrfields-functxt_03  = l_sel_button.
***
***
***  l_sel_button-icon_id   = icon_work_center.
***  l_sel_button-icon_text = 'Taxas IRPJ CSLL'.
***  sscrfields-functxt_04  = l_sel_button.
***
***  l_sel_button-icon_id   = icon_work_center.
***  l_sel_button-icon_text = 'Config. Imposto p/ Conta'.
***  sscrfields-functxt_05  = l_sel_button.
*Fim Alteração - Leandro Valentim ferreira - 10.08.23 - #119411

**********************************************************************
* START
**********************************************************************
START-OF-SELECTION.

  PERFORM f_start_selection.

  PERFORM f_selecao_dados.

  CASE p_lbtipo.
    WHEN '1'.
      PERFORM f_exibir_dados.
    WHEN '2'.
      PERFORM f_alv_tree.
    WHEN '3'.
      PERFORM f_alv_conciliacao.
*Inicio Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
    WHEN '4'.
      PERFORM f_alv_resultado.
*Fim Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411f
    WHEN OTHERS.
  ENDCASE.

  PERFORM refresh_doc_contabil .


**********************************************************************
* selecao dados
**********************************************************************

FORM f_selecao_dados.

  DATA: w_campos_nfe   TYPE zde_campos_nfe,
        r_lifnr        TYPE RANGE OF lifnr,
        r_branch       TYPE RANGE OF j_1bbranc_,
        cont           TYPE n LENGTH 2,
        v_ano          TYPE gjahr,
        vano1          TYPE gjahr,
        vano2          TYPE gjahr,
        ryear          TYPE gjahr,
        wl_zib_chave   TYPE zib_contabil_chv,
        wl_zib_erro    TYPE zib_contabil_err,
        wl_zib_chave_2 TYPE zib_contabil_chv,
        wl_zib_erro_2  TYPE zib_contabil_err.

  RANGES: r_gdatu FOR tcurr-gdatu,
           r_fcurr FOR tcurr-fcurr.

  FREE: t_alv_saida[].
  CLEAR: w_alv_saida.

*------------------------------------------------
* selecao
*------------------------------------------------
  IF s_tp_imp-low IS INITIAL.
    SELECT * FROM zglt0101 INTO TABLE t_zglt0101.
  ELSE.
    SELECT * FROM zglt0101 INTO TABLE t_zglt0101 WHERE tp_imposto =  s_tp_imp-low.
  ENDIF.

*** busca descrição da emoresa*************
  SELECT SINGLE butxt FROM  t001 INTO v_desc_emp WHERE bukrs = s_empre-low.


  CHECK t_zglt0101[] IS NOT INITIAL.

  SORT t_zglt0101 BY conta ASCENDING.

  LOOP AT t_zglt0101 INTO w_zglt0101.

    REFRESH: it_contas,
             it_saldo_contas,
             it_saldo_contas_2,
             it_saldo_contas_3,
             it_saldo_contas_anterior,
             it_saldo_contas_2_anterior,
             it_saldo_contas_3_anterior,
             it_saldo_contas_atual,
             it_saldo_contas_2_atual,
             it_saldo_contas_3_atual.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_zglt0101-conta
      IMPORTING
        output = w_zglt0101-conta.

    wa_contas-bukrs = s_empre-low.
    wa_contas-saknr =  w_zglt0101-conta.

    APPEND wa_contas TO it_contas.

*Inicio Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
    SELECT SINGLE *
           INTO @DATA(wl_ska1)
           FROM ska1
           WHERE saknr EQ @w_zglt0101-conta
             AND ktopl EQ '0050'.
*Fim Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411

    w_alv_saida-tp_imposto = w_zglt0101-tp_imposto.
    w_alv_saida-desc_imposto = w_zglt0101-desc_tp_imposto.
    w_alv_saida-conta = w_zglt0101-conta.
    w_alv_saida-desc_conta = w_zglt0101-desc_conta.


    IF w_zglt0101-conta IS NOT INITIAL AND w_alv_saida-desc_conta IS INITIAL.
      SELECT SINGLE txt50 INTO w_alv_saida-desc_conta FROM gl_acct_ca_text WHERE saknr = w_zglt0101-conta AND spras = 'P' AND ktopl EQ '0050'.
    ENDIF.

    SELECT SINGLE *
   FROM zglt0106
   INTO @DATA(wa_zglt0106)
   WHERE bukrs = @s_empre-low AND
         mes = @s_mes-low AND
         ano = @s_ano-low AND
         tipo_imposto = @w_zglt0101-tp_imposto AND
         conta = @w_zglt0101-conta.

    PERFORM retorna_status_zib USING wa_zglt0106-doc_lcto
                                     s_ano-low"WL_ZGLT034-DATA_ATUAL(4)
                            CHANGING wl_zib_chave
                                     wl_zib_erro.

    PERFORM retorna_status_zib_2 USING     wa_zglt0106-doc_lcto_2 "DEVK9A209H - 21/05/24 - #138696 RSA
                                           s_ano-low"WL_ZGLT034-DATA_ATUAL(4)
                                  CHANGING wl_zib_chave_2
                                           wl_zib_erro_2.


    IF wl_zib_erro IS NOT INITIAL AND wl_zib_erro_2 IS NOT INITIAL.
      w_alv_saida-status = icon_led_red.
    ELSE.
      IF wa_zglt0106-doc_contabil IS NOT INITIAL OR wa_zglt0106-doc_contabil_2 IS NOT INITIAL.
        w_alv_saida-doc_contabil   = wa_zglt0106-doc_contabil.
        w_alv_saida-doc_contabil_2 = wa_zglt0106-doc_contabil_2.

        IF  wa_zglt0106-doc_estorno IS NOT INITIAL AND wa_zglt0106-doc_estorno_2 IS NOT INITIAL.
          w_alv_saida-status = icon_led_yellow.
        ELSE.
          w_alv_saida-status = icon_led_green.
        ENDIF.
      ELSE .
        IF wa_zglt0106-doc_lcto IS NOT INITIAL AND wa_zglt0106-doc_lcto_2 IS NOT INITIAL.
          w_alv_saida-status = icon_operation.
        ELSE.
          w_alv_saida-status = icon_led_yellow.
        ENDIF.
      ENDIF.
    ENDIF.

*Inicio ajustes na transação ZGL076-Imposto Difer - #138696 AOENNING.
    IF wa_zglt0106-doc_estorno IS NOT INITIAL. " AND wa_zglt0106-doc_estorno_2 IS NOT INITIAL.
      w_alv_saida-doc_estorno   = wa_zglt0106-doc_estorno.
*      w_alv_saida-doc_estorno_2 = wa_zglt0106-doc_estorno_2.

      SELECT SINGLE budat FROM bkpf INTO @DATA(v_budat) WHERE bukrs = @s_empre-low AND belnr = @wa_zglt0106-doc_estorno.

      IF v_budat <> v_ultimo_dia.
        w_alv_saida-line_color = 'C601'.
      ENDIF.

      MOVE icon_led_yellow     TO w_alv_saida-status.
*    else.
*      move icon_led_green      to <saida>-status.
    ENDIF.
*Fim ajustes na transação ZGL076-Imposto Difer - #138696 AOENNING.


*Inicio ajustes na transação ZGL076-Imposto Difer - #138696 AOENNING.
    IF wa_zglt0106-doc_estorno_2 IS NOT INITIAL.
*      w_alv_saida-doc_estorno   = wa_zglt0106-doc_estorno.
      w_alv_saida-doc_estorno_2 = wa_zglt0106-doc_estorno_2.

      SELECT SINGLE budat FROM bkpf INTO @v_budat WHERE bukrs = @s_empre-low AND belnr = @wa_zglt0106-doc_estorno_2.

      IF v_budat <> v_ultimo_dia.
        w_alv_saida-line_color = 'C601'.
      ENDIF.

      MOVE icon_led_yellow     TO w_alv_saida-status.

    ENDIF.
*Fim ajustes na transação ZGL076-Imposto Difer - #138696 AOENNING.


    "185994 - CS2025000543 - IMPOSTO DIFERIDO REVERSÃO - RGA - início
    IF wa_zglt0106-obj_key_rev IS NOT INITIAL.

      SELECT SINGLE belnr
        FROM zib_contabil_chv
        INTO @DATA(lv_estorno)
        WHERE obj_key = @wa_zglt0106-obj_key_rev.
      IF sy-subrc EQ 0.

        IF w_alv_saida-doc_estorno IS INITIAL. " AND wa_zglt0106-doc_estorno_2 IS NOT INITIAL.

          w_alv_saida-doc_estorno = lv_estorno.

*      w_alv_saida-doc_estorno_2 = wa_zglt0106-doc_estorno_2.
          MOVE icon_led_yellow     TO w_alv_saida-status.
*    else.
*      move icon_led_green      to <saida>-status.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_zglt0106-obj_key_rev2 IS NOT INITIAL.

      SELECT SINGLE belnr
        FROM zib_contabil_chv
        INTO @DATA(lv_estorno2)
        WHERE obj_key = @wa_zglt0106-obj_key_rev2.
      IF sy-subrc EQ 0.

        IF w_alv_saida-doc_estorno_2 IS INITIAL. " AND wa_zglt0106-doc_estorno_2 IS NOT INITIAL.

          w_alv_saida-doc_estorno_2 = lv_estorno2.

*      w_alv_saida-doc_estorno_2 = wa_zglt0106-doc_estorno_2.
          MOVE icon_led_yellow     TO w_alv_saida-status.
*    else.
*      move icon_led_green      to <saida>-status.
        ENDIF.
      ENDIF.
    ENDIF.
    "185994 - CS2025000543 - IMPOSTO DIFERIDO REVERSÃO - RGA - fim

    w_alv_saida-doc_lcto   = wa_zglt0106-doc_lcto.
    w_alv_saida-doc_lcto_2 = wa_zglt0106-doc_lcto_2.
    w_alv_saida-obj_key    = wa_zglt0106-obj_key.
    w_alv_saida-obj_key_2  = wa_zglt0106-obj_key_2.
    w_alv_saida-lote       = wa_zglt0106-lote.
    w_alv_saida-lote_2     = wa_zglt0106-lote_2.

    v_ano = s_ano-low - 1.
    vano1 = s_ano-low.
    vano2 = s_ano-low.

    "if ( w_alv_saida-doc_contabil is not initial and w_alv_saida-doc_contabil_2 is not initial and"se ja existe docContabil e não foi estornado ainda abre o relatório em forma de consulta mostrando os salos gravados na tabela.
    "w_alv_saida-doc_estorno is not initial and w_alv_saida-doc_estorno_2 is not initial ) or w_alv_saida-doc_contabil is initial and w_alv_saida-doc_contabil_2 is initial."Se ainda não foi gerado doc cont. ou se foi estornado pega o saldo da função.
    "145378 CS2024000343 Ajustes na transação ZGL076 PSA DEVK9A23BD
    IF ( w_alv_saida-doc_contabil IS INITIAL AND w_alv_saida-doc_estorno IS INITIAL )
             OR
       ( w_alv_saida-doc_contabil IS NOT INITIAL AND w_alv_saida-doc_estorno IS NOT INITIAL ).


**********************************************************************
*INIIO SALDO 31/12, VALOR ATUAL
      "PEGA VALORES CONTAS
      DATA ano_anterior TYPE gjahr.
      CLEAR: ano_anterior,it_saldo_contas_anterior,it_saldo_contas_2_anterior,it_saldo_contas_3_anterior.

      ano_anterior = vano1 - 1 .
      "Pega Valores refrentes ao ano anterior
      CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
        EXPORTING
          ryear         = ano_anterior
          contas        = it_contas
          p_gerar_todas = 'X'
          rldnr         = '0L'
        TABLES
          it_saldos     = it_saldo_contas_anterior
          it_saldos_2   = it_saldo_contas_2_anterior
          it_saldos_3   = it_saldo_contas_3_anterior.

      SORT it_saldo_contas_anterior BY racct ASCENDING.
      SORT it_saldo_contas_2_anterior BY racct ASCENDING.
      SORT it_saldo_contas_3_anterior BY racct ASCENDING.

      READ TABLE it_saldo_contas_anterior INTO DATA(w_saldo_brl_passado) WITH KEY racct = w_zglt0101-conta
                                                            rbukrs = s_empre-low
                                                            ryear =  ano_anterior.

      READ TABLE it_saldo_contas_2_anterior INTO DATA(w_saldo_usd_passado) WITH KEY racct = w_zglt0101-conta
                                            rbukrs = s_empre-low
                                            ryear =  ano_anterior.

      "Pega Valores refrentes ao ano atual
      DATA ano_antual TYPE gjahr.
      CLEAR: ano_antual,it_saldo_contas_atual,it_saldo_contas_2_atual,it_saldo_contas_3_atual.
      ano_antual = vano2.

      " Pega Valores refrentes ao ano passado
      CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
        EXPORTING
          ryear         = ano_antual
          contas        = it_contas
          p_gerar_todas = 'X'
          rldnr         = '0L'
        TABLES
          it_saldos     = it_saldo_contas_atual
          it_saldos_2   = it_saldo_contas_2_atual
          it_saldos_3   = it_saldo_contas_3_atual.

      SORT it_saldo_contas_atual BY racct ASCENDING.
      SORT it_saldo_contas_2_atual BY racct ASCENDING.
      SORT it_saldo_contas_3_atual ASCENDING.

      READ TABLE it_saldo_contas_atual INTO DATA(w_saldo_brl_atual) WITH KEY racct = w_zglt0101-conta
                                                                  rbukrs = s_empre-low
                                                                  ryear =  ano_antual.

      READ TABLE it_saldo_contas_2_atual INTO DATA(w_saldo_usd_atual) WITH KEY racct = w_zglt0101-conta
                                                      rbukrs = s_empre-low
                                                      ryear =  ano_antual.
*FIM SALDO 31/12, VALOR ATUAL
**********************************************************************
**********************************************************************
*INICIO CONFIÇÃO QUE AS CONTAS SÃO DE RESULTADO
      IF wl_ska1-ktoks IN lr_ktoks. "É Conta de resultado
**********************************************************************
*Saldo 31/12 BRL - É conta de Resultado - Ano Anterior
        IF w_alv_saida-saldo_brl IS NOT INITIAL AND w_alv_saida-conta = w_saldo_brl_passado-racct.
          CLEAR: cont,w_alv_saida-saldo_brl.
          cont = 1.
          DO.
            IF cont > 15.
              EXIT.
            ENDIF.
            CONCATENATE 'SL' cont INTO DATA(fielname1).
            ASSIGN COMPONENT fielname1 OF STRUCTURE w_saldo_brl_passado TO FIELD-SYMBOL(<value1>).
            IF <value1> IS ASSIGNED.
              ADD <value1> TO w_alv_saida-saldo_brl.
            ENDIF.
            ADD 1 TO cont.
          ENDDO.
          w_alv_saida-saldo_brl = w_alv_saida-saldo_brl + w_saldo_brl_passado-slvt.
        ELSE.
          w_alv_saida-saldo_brl = 0.
        ENDIF.


**********************************************************************
*Saldo 31/12 USD - É conta de Resultado - Ano Anterior
        IF w_saldo_usd_passado IS NOT INITIAL AND w_alv_saida-conta = w_saldo_usd_passado-racct.
          CLEAR: cont,w_alv_saida-saldo_usd.
          cont = 1.
          DO.
            IF cont > 15.
              EXIT.
            ENDIF.
            CONCATENATE 'SL' cont INTO DATA(fielname2).
            ASSIGN COMPONENT fielname2 OF STRUCTURE w_saldo_usd_passado TO FIELD-SYMBOL(<value2>) .
            IF <value2> IS ASSIGNED.
              ADD <value2> TO w_alv_saida-saldo_usd.
            ENDIF.
            ADD 1 TO cont.
          ENDDO.
          w_alv_saida-saldo_usd = w_alv_saida-saldo_usd + w_saldo_usd_passado-slvt.
        ELSE.
          w_alv_saida-saldo_usd = 0.
        ENDIF.

**********************************************************************
*Saldo Atual BRL - É conta de Resultado - Ano Atual
        IF w_saldo_brl_atual IS NOT INITIAL AND w_alv_saida-conta = w_saldo_brl_atual-racct.
          CLEAR: cont,w_alv_saida-saldo_atu_brl.
          CLEAR: cont.
          cont = 1.
          DO.
            IF cont > s_mes-low.
              EXIT.
            ENDIF.
            CONCATENATE 'SL' cont INTO DATA(fielname3).
            ASSIGN COMPONENT fielname3 OF STRUCTURE w_saldo_brl_atual TO FIELD-SYMBOL(<value3>).
            IF <value3> IS ASSIGNED.
              ADD <value3> TO w_alv_saida-saldo_atu_brl.
            ENDIF.
            ADD 1 TO cont.
          ENDDO.
          w_alv_saida-saldo_atu_brl = w_alv_saida-saldo_atu_brl + w_saldo_usd_passado-slvt.
        ELSE.
          w_alv_saida-saldo_atu_brl = 0.
        ENDIF.
**********************************************************************
*Saldo Atual USD - É conta de Resultado - Ano Atual
        IF w_saldo_usd_atual IS NOT INITIAL AND w_alv_saida-conta = w_saldo_usd_atual-racct.
          CLEAR: cont,w_alv_saida-saldo_atu_usd.
          CLEAR: cont.
          cont = 1.
          DO.
            IF cont > s_mes-low.
              EXIT.
            ENDIF.
            CONCATENATE 'SL' cont INTO DATA(fielname4).
            ASSIGN COMPONENT fielname4 OF STRUCTURE w_saldo_usd_atual TO FIELD-SYMBOL(<value4>).
            IF <value4> IS ASSIGNED.
              ADD <value4> TO w_alv_saida-saldo_atu_usd.
            ENDIF.
            ADD 1 TO cont.
          ENDDO.
          w_alv_saida-saldo_atu_usd = w_alv_saida-saldo_atu_usd + w_saldo_usd_atual-slvt.
        ELSE.
          w_alv_saida-saldo_atu_usd = 0.
        ENDIF.
*FIM CONFIÇÃO QUE AS CONTAS SÃO DE RESULTADO
**********************************************************************
**********************************************************************
*INICIO CONDIÇÃO QUE AS CONTAS NÃO SÃO DE RESULTADO

      ELSE. "Não é conta de Resultado

**********************************************************************
*Saldo 31/12 BRL/USD - Não é conta de Resultado - Ano Anterior

        IF w_saldo_brl_atual-slvt IS NOT INITIAL AND w_alv_saida-conta = w_saldo_brl_atual-racct.
          w_alv_saida-saldo_brl = w_saldo_brl_atual-slvt.
        ENDIF.

        IF w_saldo_usd_atual-slvt IS NOT INITIAL AND w_alv_saida-conta = w_saldo_usd_atual-racct.
          w_alv_saida-saldo_usd = w_saldo_usd_atual-slvt.
        ENDIF.
**********************************************************************
*Saldo Atual BRL - Não é conta de Resultado - Ano Atual
        IF w_saldo_brl_atual IS NOT INITIAL AND w_alv_saida-conta =  w_saldo_brl_atual-racct.
          CLEAR: cont,w_alv_saida-saldo_atu_brl.
          CLEAR: cont.
          cont = 1.
          DO.
            IF cont > s_mes-low.
              EXIT.
            ENDIF.
            CONCATENATE 'SL' cont INTO DATA(fielname5).
            ASSIGN COMPONENT fielname5 OF STRUCTURE w_saldo_brl_atual TO FIELD-SYMBOL(<value5>).
            IF <value5> IS ASSIGNED.
              ADD <value5> TO w_alv_saida-saldo_atu_brl.
            ENDIF.
            ADD 1 TO cont.
          ENDDO.

          w_alv_saida-saldo_atu_brl = w_alv_saida-saldo_atu_brl + w_saldo_brl_atual-slvt.
        ELSE.
          w_alv_saida-saldo_atu_brl = 0.
        ENDIF.

**********************************************************************
*Saldo Atual USD - Não é conta de Resultado - Ano Atual
        IF w_saldo_usd_atual IS NOT INITIAL AND w_alv_saida-conta = w_saldo_usd_atual-racct.
          CLEAR: cont,w_alv_saida-saldo_atu_usd.
          cont = 1.
          DO.
            IF cont > s_mes-low.
              EXIT.
            ENDIF.

            CONCATENATE 'SL' cont INTO DATA(fielname6).
            ASSIGN COMPONENT fielname6 OF STRUCTURE w_saldo_usd_atual TO FIELD-SYMBOL(<value6>).
            IF <value6> IS ASSIGNED.
              ADD <value6> TO w_alv_saida-saldo_atu_usd.
            ENDIF.

            ADD 1 TO cont.

          ENDDO.

          w_alv_saida-saldo_atu_usd = w_alv_saida-saldo_atu_usd + w_saldo_usd_atual-slvt.

        ELSE.
          w_alv_saida-saldo_atu_usd = 0.
        ENDIF.
      ENDIF.
*FIM CONDIÇÃO QUE AS CONTAS NÃO SÃO DE RESULTADO
**********************************************************************
**********************************************************************
*INICIO MOVIMNETO BRL E USD

      w_alv_saida-movi_brl = w_alv_saida-saldo_atu_brl - w_alv_saida-saldo_brl. "MOVIMNETO BRL
      w_alv_saida-movi_usd = w_alv_saida-saldo_atu_usd - w_alv_saida-saldo_usd . "MOVIMENTO USD

** BUG - 153691 - Inicio - CBRAND
      w_alv_saida-m_irpj_brl =  w_alv_saida-s_irpj_brl - w_alv_saida-irpj_brl.
      w_alv_saida-m_irpj_usd =  w_alv_saida-s_irpj_usd - w_alv_saida-irpj_usd.
      w_alv_saida-m_csll_brl =  w_alv_saida-s_csll_brl - w_alv_saida-csll_brl.
      w_alv_saida-m_csll_usd =  w_alv_saida-s_csll_usd - w_alv_saida-csll_usd.
** BUG - 153691 - Fim - CBRAND


*FIM MOVIMNETO NRL E USD
**********************************************************************

      PERFORM f_tp_taxa USING w_alv_saida-tp_imposto
                             CHANGING v_tp_taxa.
      "Ajute para buscar a taxa do dolar referente ao  mes subsequente ao mes do parametro 98606
      v_mes = s_mes-low + 1.
      v_ano = s_ano-low.
      IF v_mes > 12.
        v_mes = 01.
        v_ano = s_ano-low + 1.
      ENDIF.
      CONCATENATE '01.' v_mes '.' v_ano INTO v_data.
      " fim ajuste bug 98606
      PERFORM f_taxa_usd USING v_data
             CHANGING xtx_usd.

      SELECT SINGLE percentual FROM  zglt0103 INTO v_irpj WHERE desc_taxa EQ 'IRPJ'.  " Percentual 'IRPJ'
      SELECT SINGLE percentual FROM  zglt0103 INTO v_csll WHERE desc_taxa EQ 'CSLL'.  " Percentual 'CSLL'

      IF v_csll IS NOT INITIAL.
        w_alv_saida-csll_brl = w_alv_saida-saldo_atu_brl * ( v_csll / 100 ).  "'CSLL' BRL do Saldo Atual
        w_alv_saida-s_csll_brl = w_alv_saida-saldo_brl * ( v_csll / 100 ).  "'CSLL' BRL do Saldo
        w_alv_saida-m_csll_brl = w_alv_saida-movi_brl * ( v_csll / 100 ).  "'CSLL' BRL da Movimentação

        CASE v_tp_taxa. "USD "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01 É HISTORICO 02 É FECHAMENTO

          WHEN '01'. "Somente USD "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01 É HISTORICO
            w_alv_saida-csll_usd = w_alv_saida-saldo_atu_usd * ( v_csll / 100 ).  "'CSLL' USD
            w_alv_saida-s_csll_usd = w_alv_saida-saldo_usd * ( v_csll / 100 ).  "'CSLL' USD do Saldo
            w_alv_saida-m_csll_usd = w_alv_saida-movi_usd * ( v_csll / 100 ).  "'CSLL' USD do Saldo

          WHEN '02'. "USD/BRL "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> 02 É FECHAMENTO

            IF xtx_usd IS NOT INITIAL.

              IF w_alv_saida-csll_brl IS NOT INITIAL. "Ajuste pois estava buscando valor no campo errado.
                w_alv_saida-csll_usd = w_alv_saida-csll_brl / abs( xtx_usd ).
              ELSE.
                w_alv_saida-csll_usd = w_alv_saida-saldo_atu_usd * ( v_csll / 100 ).
              ENDIF.

              IF w_alv_saida-s_csll_brl IS NOT INITIAL.
                w_alv_saida-s_csll_usd = w_alv_saida-s_csll_brl / abs( xtx_usd ).
              ELSE.
                w_alv_saida-s_csll_usd = w_alv_saida-saldo_usd * ( v_csll / 100 ).
              ENDIF.

              IF w_alv_saida-m_csll_brl IS NOT INITIAL.
                w_alv_saida-m_csll_usd = w_alv_saida-m_csll_brl / abs( xtx_usd ).
              ELSE.
                w_alv_saida-m_csll_usd = w_alv_saida-movi_usd * ( v_csll / 100 ).
              ENDIF.

            ENDIF.
        ENDCASE.



      ENDIF.

      IF v_irpj IS NOT INITIAL.
        w_alv_saida-irpj_brl = w_alv_saida-saldo_atu_brl * ( v_irpj / 100 ).  "'IRPJ' BRL do Saldo Atual
        w_alv_saida-s_irpj_brl = w_alv_saida-saldo_brl * ( v_irpj / 100 ).  "'IRPJ' BRL do Saldo
        w_alv_saida-m_irpj_brl = w_alv_saida-movi_brl * ( v_irpj / 100 ).  "'IRPJ' BRL da Movimentação


        CASE v_tp_taxa. "USD "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01 É HISTORICO 02 É FECHAMENTO

          WHEN '01'. "Somente USD "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 01 É HISTORICO
            w_alv_saida-irpj_usd = w_alv_saida-saldo_atu_usd * ( v_irpj / 100 ).  "'IRPJ' USD
            w_alv_saida-s_irpj_usd = w_alv_saida-saldo_usd * ( v_irpj / 100 ).  "'IRPJ' USD do Saldo
            w_alv_saida-m_irpj_usd = w_alv_saida-movi_usd * ( v_irpj / 100 ).  "'IRPJ' USD do Saldo

          WHEN '02'. "USD/BRL "CADASTRO DE TIPO DE IMPOPOTO DEFERIDO -> TIPO 02 É FECHAMENTO

            IF xtx_usd IS NOT INITIAL.
              IF w_alv_saida-irpj_brl IS NOT INITIAL.
                w_alv_saida-irpj_usd = w_alv_saida-irpj_brl / abs( xtx_usd ).
              ELSE.
                w_alv_saida-irpj_usd = w_alv_saida-saldo_atu_usd * ( v_irpj / 100 ).
              ENDIF.

*Inicio Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
*             Cálculo sobre o campo Saldo
              IF w_alv_saida-s_irpj_brl IS NOT INITIAL.
                w_alv_saida-s_irpj_usd = w_alv_saida-s_irpj_brl / abs( xtx_usd ).
              ELSE.
                w_alv_saida-s_irpj_usd = w_alv_saida-saldo_usd * ( v_irpj / 100 ).
              ENDIF.

*             Cálculo sobre o campo Movimentação
              IF w_alv_saida-m_irpj_brl IS NOT INITIAL.
                w_alv_saida-m_irpj_usd = w_alv_saida-m_irpj_brl / abs( xtx_usd ).
              ELSE.
                w_alv_saida-m_irpj_usd = w_alv_saida-movi_usd * ( v_irpj / 100 ).
              ENDIF.

*Fim Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
            ENDIF.
        ENDCASE.

      ENDIF.
    ELSE.
      IF w_alv_saida-conta = wa_zglt0106-conta.
        w_alv_saida-saldo_brl      = wa_zglt0106-saldoanual_brl.
        w_alv_saida-saldo_usd      = wa_zglt0106-saldoanual_usd.
        w_alv_saida-movi_brl       = wa_zglt0106-movimentacao_brl.
        w_alv_saida-movi_usd       = wa_zglt0106-movimentacao_usd.
        w_alv_saida-saldo_atu_brl  = wa_zglt0106-saldoatual_brl.
        w_alv_saida-saldo_atu_usd  = wa_zglt0106-saldoatual_usd.
        w_alv_saida-irpj_brl       = wa_zglt0106-irpj_brl.
        w_alv_saida-irpj_usd       = wa_zglt0106-irpj_usd.
        w_alv_saida-csll_brl       = wa_zglt0106-csll_brl.
        w_alv_saida-csll_usd       = wa_zglt0106-csll_usd.
        w_alv_saida-lote           = wa_zglt0106-lote.
        w_alv_saida-m_irpj_brl     = wa_zglt0106-m_irpj_brl.
        w_alv_saida-m_irpj_usd     = wa_zglt0106-m_irpj_usd.
        w_alv_saida-m_csll_brl     = wa_zglt0106-m_csll_brl.
        w_alv_saida-m_csll_usd     = wa_zglt0106-m_csll_usd.
*** BUG - 152228 - Inicio - CBRAND
        w_alv_saida-s_irpj_brl     = wa_zglt0106-s_irpj_brl.
        w_alv_saida-s_irpj_usd     = wa_zglt0106-s_irpj_usd.
        w_alv_saida-s_csll_brl     = wa_zglt0106-s_csll_brl.
        w_alv_saida-s_csll_usd     = wa_zglt0106-s_csll_usd.
*** BUG - 152228 - Fim - CBRAND

      ENDIF.
    ENDIF.

    IF w_alv_saida-conta(1) EQ 'M' AND w_alv_saida-conta = wa_zglt0106-conta.
      w_alv_saida-saldo_brl      = wa_zglt0106-saldoanual_brl.
      w_alv_saida-saldo_usd      = wa_zglt0106-saldoanual_usd.
      w_alv_saida-movi_brl       = wa_zglt0106-movimentacao_brl.
      w_alv_saida-movi_usd       = wa_zglt0106-movimentacao_usd.
      w_alv_saida-saldo_atu_brl  = wa_zglt0106-saldoatual_brl.
      w_alv_saida-saldo_atu_usd  = wa_zglt0106-saldoatual_usd.
      w_alv_saida-irpj_brl       = wa_zglt0106-irpj_brl.
      w_alv_saida-irpj_usd       = wa_zglt0106-irpj_usd.
      w_alv_saida-csll_brl       = wa_zglt0106-csll_brl.
      w_alv_saida-csll_usd       = wa_zglt0106-csll_usd.

*** BUG - 152228 - Inicio - CBRAND
      w_alv_saida-s_irpj_brl     = wa_zglt0106-s_irpj_brl.
      w_alv_saida-s_irpj_usd     = wa_zglt0106-s_irpj_usd.
      w_alv_saida-s_csll_brl     = wa_zglt0106-s_csll_brl.
      w_alv_saida-s_csll_usd     = wa_zglt0106-s_csll_usd.
*** BUG - 152228 - Fim - CBRAND

*** BUG - 153691 - Inicio - CBRAND
      w_alv_saida-m_irpj_brl     = wa_zglt0106-m_irpj_brl.
      w_alv_saida-m_irpj_usd     = wa_zglt0106-m_irpj_usd.
      w_alv_saida-m_csll_brl     = wa_zglt0106-m_csll_brl.
      w_alv_saida-m_csll_usd     = wa_zglt0106-m_csll_usd.
*** BUG - 153691 - IFim - CBRAND

    ENDIF.

    APPEND w_alv_saida           TO t_alv_saida.

    CLEAR: w_alv_saida, wa_zglt0106. "w_saldo_brl, w_saldo_usd,
  ENDLOOP.

  SORT t_alv_saida BY tp_imposto.

  PERFORM f_editar_linha.
ENDFORM.



**********************************************************************
* exibir
**********************************************************************
FORM f_exibir_dados.

  CALL SCREEN 100.

ENDFORM.



**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

  DATA: wl_layout TYPE slis_layout_alv.
  DATA:
    p_text      TYPE sdydo_text_element,
    filtros	    TYPE zif_screen_linha_filtro,
    i_filtros	  TYPE zif_screen_linha_filtro_t,
    v_valor(60).

  PERFORM f_fieldcatalog.
  variante = VALUE #( report = sy-repid ).

*Inicio Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
  PERFORM zf_preenche_cell_color.
*Fim Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411

  IF g_grid IS INITIAL.

    IF v_irpj IS INITIAL OR v_irpj IS INITIAL.
      CLEAR: v_csll, v_irpj.
      SELECT SINGLE percentual FROM  zglt0103 INTO v_csll WHERE desc_taxa EQ 'CSLL'.
      SELECT SINGLE percentual FROM  zglt0103 INTO v_irpj WHERE desc_taxa EQ 'IRPJ'.
    ENDIF.

    CLEAR: i_filtros.

    CONCATENATE s_empre-low '-' v_desc_emp INTO v_valor SEPARATED BY space.
    APPEND VALUE #( parametro = 'Empresa:' valor = v_valor ) TO i_filtros.
    APPEND VALUE #( parametro = 'Mês:' valor = s_mes-low ) TO i_filtros.
    APPEND VALUE #( parametro = 'Ano: ' valor = s_ano-low ) TO i_filtros.
    APPEND VALUE #( parametro = 'Taxa do IRPJ:' valor = v_irpj ) TO i_filtros.
    APPEND VALUE #( parametro = 'Taxa do CSLL:' valor = v_csll ) TO i_filtros.

  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      EXPORTING
        i_titulo  = CONV #( p_text )
        i_filtros = i_filtros
      CHANGING
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.

    w_layout-sel_mode   = 'A'.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.
*
    w_layout-info_fname   = 'LINE_COLOR'.
    w_layout-ctab_fname   = 'COLOR_CELL'.
***    w_layout-zebra        = abap_false.

    w_layout-stylefname   = 'CELLSTYLES'.


    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.
    SET HANDLER lcl_event_handler=>data_changed_finished FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = t_function
        is_variant                    = variante
      CHANGING
        it_outtab                     = t_alv_saida[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ELSE.
    CALL METHOD g_grid->refresh_table_display( is_stable = w_stable ).
  ENDIF.

  wl_layout-colwidth_optimize = 'X'.


ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
"01  02              03                 04              05                  06                            07   08   09   10   11   12   13   14   15   16   17
 01  ''              ''                 'T_ALV_SAIDA'   'STATUS'            'Status'                     '05'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  'X'  ' ',
 02  ''              ''                 'T_ALV_SAIDA'   'TP_IMPOSTO'        'Tp Imposto'                 '05'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''              ''                 'T_ALV_SAIDA'   'DESC_IMPOSTO'      'Descr. Imposto'             '25'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''              ''                 'T_ALV_SAIDA'   'CONTA'             'Conta'                      '18'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''              ''                 'T_ALV_SAIDA'   'DESC_CONTA'        'Descr. Conta'               '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'SALDO_BRL'         'Saldo 31/12 BRL'            '18'  'X'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'SALDO_USD'         'Saldo 31/12 USD'            '18'  'X'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
*Inicio Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
 08  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'S_IRPJ_BRL'        'Sd 31/12 IRPJ BRL'          '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'S_IRPJ_USD'        'Sd 31/12 IRPJ USD'          '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 10  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'S_CSLL_BRL'        'Sd 31/12 CSLL BRL'          '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 11  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'S_CSLL_USD'        'Sd 31/12 CSLL USD'          '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
*Fim Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
 12  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'MOVI_BRL'          'Movimentação BRL'           '20'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 13  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'MOVI_USD'          'Movimentação USD'           '20'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
*Inicio Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
 14  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'M_IRPJ_BRL'        'Movim. IRPJ BRL'            '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 15  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'M_IRPJ_USD'        'Movim. IRPJ USD'            '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 16  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'M_CSLL_BRL'        'Movim. CSLL BRL'            '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 17  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'M_CSLL_USD'        'Movim. CSLL USD'            '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
*Fim Alteração - Leandro Valentim Ferreira - 15.08.23 - #119411
 18  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'SALDO_ATU_BRL'     'Saldo Mês Atual BRL'        '25'  'X'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 19  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'SALDO_ATU_USD'     'Saldo Mês Atual USD'        '15'  'X'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 20  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'IRPJ_BRL'          'IRPJ BRL'                   '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 21  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'IRPJ_USD'          'IRPJ USD'                   '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 22  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'CSLL_BRL'          'CSLL BRL'                   '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 23  'ZGLT0106'      'SALDOATUAL_BRL'   'T_ALV_SAIDA'   'CSLL_USD'          'CSLL USD'                   '15'  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 24  ''              ''                 'T_ALV_SAIDA'   'LOTE'              'Lote'                       '20'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' ',
 25  ''              ''                 'T_ALV_SAIDA'   'DOC_CONTABIL'      'Doc Contábil'               '15'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' ',
 26  ''              ''                 'T_ALV_SAIDA'   'DOC_ESTORNO'       'Doc Estorno'                '15'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' ',
 27  ''              ''                 'T_ALV_SAIDA'   'LOTE_2'            'Lote 2'                     '20'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' ',"DEVK9A209H - 16/05/24 - #138696 RSA
 28  ''              ''                 'T_ALV_SAIDA'   'DOC_CONTABIL_2'    'Doc Contábil 2'             '15'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' ',"DEVK9A209H - 16/05/24 - #138696 RSA
 29  ''              ''                 'T_ALV_SAIDA'   'DOC_ESTORNO_2'     'Doc Estorno 2'              '15'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '."DEVK9A209H - 16/05/24 - #138696 RSA

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).                                    "17

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
*  imagen
**********************************************************************
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM


FORM gera_contabilizacao.

  TYPES: BEGIN OF ty_lote_ger,
           bukrs TYPE zglt034-bukrs,
           lote  TYPE zglt034-lote,
         END OF ty_lote_ger.

  DATA: var_answer TYPE c.

  FIELD-SYMBOLS: <saida>  TYPE ty_alv_saida,
                 <saida1> TYPE ty_alv_saida.

  DATA: e_num_lote          TYPE zlote_num,
        e_num_lote_2        TYPE zlote_num,
        wl_zglt031          TYPE zglt031,
        gt_zglt032          TYPE TABLE OF zglt032,
        wa_zglt032          TYPE zglt032,
        wl_zfit0098         TYPE zfit0098,
        gt_zglt036          TYPE TABLE OF zglt036,
        gt_zglt036_2        TYPE TABLE OF zglt036,
        wl_zglt036          TYPE zglt036,
        wl_zglt036_2        TYPE zglt036,
        dp_resp             TYPE char2,
        wl_zglt035          TYPE zglt035,
        wl_zglt035_2        TYPE zglt035,
        vl_zuonr            TYPE string,
        vl_gsber            TYPE zglt036-gsber,
        tg_lote_ger         TYPE TABLE OF ty_lote_ger WITH HEADER LINE,
        tg_lote_ger_2       TYPE TABLE OF ty_lote_ger WITH HEADER LINE,
        v_objkey            TYPE char20,
        v_tabix             TYPE char1,
        v_sing_irpj_brl     TYPE i,
        v_sing_irpj_usd     TYPE i,
        v_sing_csll_brl     TYPE i,
        v_sing_csll_usd     TYPE i,
        v_gerar_segundo_doc TYPE c.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente gerar o documento contábil?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  DATA: v_tipo_imposto  TYPE zglt0101-tp_imposto,
        v_saldo_brl     TYPE zde_vlr15_02,
        v_saldo_usd     TYPE zde_vlr15_02,
        v_movi_brl      TYPE zde_vlr15_02,
        v_movi_usd      TYPE zde_vlr15_02,
        v_saldo_atu_brl TYPE zde_vlr15_02,
        v_saldo_atu_usd TYPE zde_vlr15_02,
        v_irpj_brl      TYPE zde_vlr15_02,
        v_irpj_usd      TYPE zde_vlr15_02,
        v_csll_brl      TYPE zde_vlr15_02,
        v_csll_usd      TYPE zde_vlr15_02,
        v_blart         TYPE zglt031-blart,
        v_bktxt         TYPE zglt031-bktxt,
        v_prov_est      TYPE zglt031-prov_est,
        v_saldo(1)      TYPE c.

  CLEAR: v_saldo_brl, v_saldo_usd, v_movi_brl, v_movi_usd, v_saldo_atu_brl, v_saldo_atu_usd, v_irpj_brl, v_irpj_usd, v_csll_brl, v_csll_usd.
  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE t_alv_saida ASSIGNING <saida> INDEX wa_sel_rows-index.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    CHECK v_tipo_imposto <> <saida>-tp_imposto.

    IF <saida>-doc_contabil IS NOT INITIAL AND  ( <saida>-doc_estorno IS INITIAL  ).
      CONTINUE.
    ENDIF.

    "SELECT * FROM zglt0102 INTO TABLE @DATA(it_zglt0102) WHERE tp_imposto EQ @<saida>-tp_imposto.


    SORT t_alv_saida ASCENDING BY tp_imposto.

    LOOP AT t_alv_saida INTO DATA(wa_saida) WHERE tp_imposto = <saida>-tp_imposto. "and w_zglt0102-tp_imposto = <saida>-tp_imposto.
      v_saldo_brl     = v_saldo_brl + wa_saida-saldo_brl .
      v_saldo_usd     = v_saldo_usd + wa_saida-saldo_usd.
      v_movi_brl      = v_movi_brl + wa_saida-movi_brl.
      v_movi_usd      = v_movi_usd + wa_saida-movi_usd.
      v_saldo_atu_brl = v_saldo_atu_brl + wa_saida-saldo_atu_brl.
      v_saldo_atu_usd = v_saldo_atu_usd + wa_saida-saldo_atu_usd.
      v_irpj_brl      = v_irpj_brl + wa_saida-irpj_brl.
      v_irpj_usd      = v_irpj_usd + wa_saida-irpj_usd.
      v_csll_brl      = v_csll_brl + wa_saida-csll_brl.
      v_csll_usd      = v_csll_usd + wa_saida-csll_usd.
    ENDLOOP.
    v_tipo_imposto = <saida>-tp_imposto.


    "CHECK ( v_irpj_brl <> '0' AND v_csll_brl <> '0'  ) OR ( v_irpj_usd <> '0' AND  v_csll_usd <> '0' ) .  "Não encontrou saldo em nenhuma moeda
    CHECK ( v_irpj_brl <> '0' OR v_irpj_usd <> '0'  ) OR  ( v_csll_brl <> '0' OR v_csll_usd <> '0' ).

    "Não prosseguir, pular para próximo imposto.

    "DEVK9A209H - 16/05/24 - #138696 RSA
    "Valida Gerar dois documentos quando a moeda BRL for <> de USD
    v_sing_irpj_brl = sign( v_irpj_brl ).
    v_sing_irpj_usd = sign( v_irpj_usd ).
    v_sing_csll_brl = sign( v_csll_brl ).
    v_sing_csll_usd = sign( v_csll_usd ).

    IF v_sing_irpj_brl NE 0 AND v_sing_irpj_usd NE 0.
      IF v_sing_irpj_brl NE v_sing_irpj_usd.
        v_gerar_segundo_doc = abap_true.
      ENDIF.
    ENDIF.

    IF v_sing_csll_brl NE 0 AND v_sing_csll_usd NE 0.
      IF v_sing_csll_brl NE v_sing_csll_usd.
        v_gerar_segundo_doc = abap_true.
      ENDIF.
    ENDIF.
    "DEVK9A209H - 16/05/24 - #138696 RSA



    IF v_irpj_brl =  0 AND v_csll_brl = 0.

      IF v_irpj_usd < 0.
        v_saldo = '2'.
      ELSEIF v_irpj_usd > 0.
        v_saldo = '1'.
      ELSEIF v_irpj_usd = 0.
        IF v_csll_usd < 0.
          v_saldo = '2'.
        ELSE.
          v_saldo = '1'.
        ENDIF.
      ENDIF.
    ELSE.

      IF v_irpj_brl < 0.
        v_saldo = '2'.
      ELSEIF v_irpj_brl > 0.
        v_saldo = '1'.
      ELSEIF v_irpj_brl = 0.
        IF v_csll_brl < 0.
          v_saldo = '2'.
        ELSE.
          v_saldo = '1'.
        ENDIF.
      ENDIF.

    ENDIF.


    SELECT  * FROM zglt0102 INTO TABLE @DATA(it_zglt0102) WHERE tp_imposto = @<saida>-tp_imposto AND  saldo = @v_saldo.

    IF sy-subrc NE 0.
      CONCATENATE 'Tipo de imposto' <saida>-tp_imposto 'sem parâmetro de contabilização!' INTO DATA(w_msg) SEPARATED BY space.
      MESSAGE w_msg TYPE 'E'.
      CONTINUE.
    ENDIF.


    dp_resp = '83'. "'09'. "Contabilidade " bug 99642
    CLEAR: e_num_lote. "BUG - 152228 - CBRAND

    CALL METHOD zcl_gerar_lote=>create_lote
      EXPORTING
        i_bukrs       = s_empre-low
        i_descr_lote  = 'Imposto Diferido'
        i_user_resp   = sy-uname
        i_dep_resp    = dp_resp
        i_status_lote = 'L'
      IMPORTING
        e_num_lote    = e_num_lote.

    CLEAR: tg_lote_ger, tg_lote_ger[].
    tg_lote_ger-bukrs = s_empre-low.
    tg_lote_ger-lote  = e_num_lote.
    APPEND tg_lote_ger.



*    SELECT * FROM zglt0102 INTO TABLE @DATA(it_zglt0102) WHERE tp_imposto EQ @<saida>-tp_imposto.
*
*      if v_irpj_brl =  0 and v_csll_brl = 0.
*
*        IF v_irpj_usd < 0.
*      v_saldo = '2'.
*    ELSEIF v_irpj_usd > 0.
*      v_saldo = '1'.
*    ELSEIF v_irpj_usd = 0.
*      IF v_csll_usd < 0.
*        v_saldo = '2'.
*      ELSE.
*        v_saldo = '1'.
*      ENDIF.
*    ENDIF.
*        else.
*
*          IF v_irpj_brl < 0.
*      v_saldo = '2'.
*    ELSEIF v_irpj_brl > 0.
*      v_saldo = '1'.
*    ELSEIF v_irpj_brl = 0.
*      IF v_csll_brl < 0.
*        v_saldo = '2'.
*      ELSE.
*        v_saldo = '1'.
*      ENDIF.
*    ENDIF.
*
*          endif.


    READ TABLE it_zglt0102 INTO DATA(w_it_zglt0102) WITH KEY saldo = v_saldo.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE blart bktxt prov_est FROM zglt031 INTO ( v_blart, v_bktxt, v_prov_est ) WHERE tp_lcto = w_it_zglt0102-tp_lcto.

    ENDIF.


    MOVE:    e_num_lote                  TO wl_zglt035-lote,
             s_empre-low                 TO wl_zglt035-bukrs,
             w_it_zglt0102-tp_lcto       TO wl_zglt035-tp_lcto,
             dp_resp                     TO wl_zglt035-dpto_resp,
             'BRL'                       TO wl_zglt035-moeda_doc,
             v_blart                     TO wl_zglt035-blart,
             'IMPOSTO DIFERIDO'          TO wl_zglt035-xblnr,
             v_bktxt                     TO wl_zglt035-bktxt,
             v_ultimo_dia                TO wl_zglt035-budat,
             v_ultimo_dia                TO wl_zglt035-bldat,
             sy-datum                    TO wl_zglt035-dt_lcto,
             v_prov_est                  TO wl_zglt035-prov_est,
             s_mes-low                   TO wl_zglt035-monat,
             s_ano-low                   TO wl_zglt035-gjahr,
             sy-uname                    TO wl_zglt035-usnam,
             sy-datum                    TO wl_zglt035-dt_entrada,
             sy-uzeit                    TO wl_zglt035-hr_entrada.

    IF v_irpj_brl = '0' AND v_csll_brl = '0' AND v_irpj_usd <> '0'  AND  v_csll_usd <> '0'. "Encontrou saldo Somente em dolar
      MOVE              'X'                  TO wl_zglt035-st_lc_moeda.
    ELSE.
      MOVE              ' '                  TO wl_zglt035-st_lc_moeda.
    ENDIF.

    IF v_gerar_segundo_doc EQ abap_true."DEVK9A209H - 17/05/24 - #138696 RSA
      MOVE              'X'                  TO wl_zglt035-st_lc_moeda.
    ENDIF.

    CLEAR: gt_zglt036.

    SELECT * FROM zglt032 INTO TABLE @DATA(it_zglt032) WHERE tp_lcto = @wl_zglt035-tp_lcto.
    SORT it_zglt032 BY buzei.
    LOOP AT it_zglt032 INTO wa_zglt032.
      wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
      wl_zglt036-hkont = wa_zglt032-hkont.
      wl_zglt036-bschl = wa_zglt032-bschl.
      wl_zglt036-sgtxt = wa_zglt032-sgtxt.

      CONCATENATE s_empre-low+2(2) '01' INTO wl_zglt036-gsber.
      "wl_zglt036-gsber = '0101'.
      IF v_gerar_segundo_doc EQ abap_false."DEVK9A209H - 17/05/24 - #138696 RSA
        IF v_irpj_brl <> '0' AND v_csll_brl <> '0' AND v_irpj_usd <> '0'  AND  v_csll_usd <> '0'. "Encontrou saldo em todas as moedas
          CASE sy-tabix.
            WHEN '1'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc =  abs( v_irpj_brl ). "Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_int = abs( v_irpj_brl )."Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_forte = abs( v_irpj_usd )."Coluna IRPJ / USD
            WHEN '2'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc =  abs( v_irpj_brl ). "Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_int =  abs( v_irpj_brl ). "Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_forte =  abs( v_irpj_usd ). " Coluna IRPJ / USD
            WHEN '3'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc =  abs( v_csll_brl ). "Coluna CSLL / BRL
              wl_zglt036-vlr_moeda_int =   abs( v_csll_brl ). "Coluna CSLL / BRL
              wl_zglt036-vlr_moeda_forte =   abs( v_csll_usd ). "Coluna CSLL / USD
            WHEN '4'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc =  abs( v_csll_brl ). "Coluna CSLL / BRL
              wl_zglt036-vlr_moeda_int =  abs( v_csll_brl ). "Coluna CSLL / BRL
              wl_zglt036-vlr_moeda_forte =  abs( v_csll_usd ). "Coluna CSLL / USD
          ENDCASE.
        ELSEIF v_irpj_brl = '0' AND v_csll_brl = '0' AND v_irpj_usd <> '0'  AND  v_csll_usd <> '0'.
          CASE sy-tabix.
            WHEN '1'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = ' '.
              wl_zglt036-vlr_moeda_int = ' '.
              wl_zglt036-vlr_moeda_forte =  abs( v_irpj_usd )."Coluna IRPJ / USD
            WHEN '2'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = ' '.
              wl_zglt036-vlr_moeda_int = ' '.
              wl_zglt036-vlr_moeda_forte =  abs( v_irpj_usd ). " Coluna IRPJ / USD
            WHEN '3'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = ' '.
              wl_zglt036-vlr_moeda_int =  ' '.
              wl_zglt036-vlr_moeda_forte =   abs( v_csll_usd ). "Coluna CSLL / USD
            WHEN '4'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = ' '.
              wl_zglt036-vlr_moeda_int = ' '.
              wl_zglt036-vlr_moeda_forte =  abs( v_csll_usd ). "Coluna CSLL / USD
          ENDCASE.
        ELSEIF v_irpj_brl <> '0' AND v_irpj_usd <> '0' AND v_csll_brl = '0' AND v_csll_usd = '0'.
          CASE sy-tabix.
            WHEN '1'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_irpj_brl ).
              wl_zglt036-vlr_moeda_int = abs( v_irpj_brl ).
              wl_zglt036-vlr_moeda_forte =  abs( v_irpj_usd )."Coluna IRPJ / USD
            WHEN '2'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_irpj_brl ).
              wl_zglt036-vlr_moeda_int = abs( v_irpj_brl ).
              wl_zglt036-vlr_moeda_forte =  abs( v_irpj_usd ). " Coluna IRPJ / USD
            WHEN '3'.
              v_tabix = '0'.
            WHEN '4'.
              v_tabix = '0'.
          ENDCASE.
        ELSEIF v_irpj_brl = '0' AND v_irpj_usd = '0' AND v_csll_brl <> '0' AND v_csll_usd <> '0'.
          CASE sy-tabix.
            WHEN '1'.
              v_tabix = '0'.
            WHEN '2'.
              v_tabix = '0'.
            WHEN '3'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_csll_brl ).
              wl_zglt036-vlr_moeda_int =  abs( v_csll_brl ).
              wl_zglt036-vlr_moeda_forte =   abs( v_csll_usd ). "Coluna CSLL / USD
            WHEN '4'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_csll_brl ).
              wl_zglt036-vlr_moeda_int = abs( v_csll_brl ).
              wl_zglt036-vlr_moeda_forte =  abs( v_csll_usd ). "Coluna CSLL / USD
          ENDCASE.
        ENDIF.

      ELSE.
        " Regra Gerar dois documentos "DEVK9A209H - 17/05/24 - #138696 RSA
        IF     v_irpj_brl <> '0' AND v_irpj_usd <> '0' AND v_csll_brl <> '0' AND v_csll_usd <> '0'.
          CASE sy-tabix.
            WHEN '1'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_irpj_brl ). "Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_int = abs( v_irpj_brl )."Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_forte = ''.
              wl_zglt036-vlr_moeda_grupo = ''.
            WHEN '2'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_irpj_brl ). "Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_int = abs( v_irpj_brl )."Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_forte = ''.
              wl_zglt036-vlr_moeda_grupo = ''.
            WHEN '3'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_csll_brl ). "Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_int = abs( v_csll_brl )."Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_forte = ''.
              wl_zglt036-vlr_moeda_grupo = ''.
            WHEN '4'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_csll_brl ). "Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_int = abs( v_csll_brl )."Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_forte = ''.
              wl_zglt036-vlr_moeda_grupo = ''.
          ENDCASE.

        ELSEIF     v_irpj_brl <> '0' AND v_irpj_usd <> '0' AND v_csll_brl = '0' AND v_csll_usd = '0'.
          CASE sy-tabix.
            WHEN '1'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_irpj_brl ). "Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_int = abs( v_irpj_brl )."Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_forte = ''.
              wl_zglt036-vlr_moeda_grupo = ''.
            WHEN '2'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_irpj_brl ). "Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_int = abs( v_irpj_brl )."Coluna IRPJ / BRL
              wl_zglt036-vlr_moeda_forte = ''.
              wl_zglt036-vlr_moeda_grupo = ''.
            WHEN '3'.
              v_tabix = '0'.
            WHEN '4'.
              v_tabix = '0'.
          ENDCASE.
        ELSEIF v_irpj_brl = '0' AND v_irpj_usd = '0' AND v_csll_brl <> '0' AND v_csll_usd <> '0'.
          CASE sy-tabix.
            WHEN '1'.
              v_tabix = '0'.
            WHEN '2'.
              v_tabix = '0'.
            WHEN '3'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_csll_brl ). "Coluna CSLL / BRL
              wl_zglt036-vlr_moeda_int = abs( v_csll_brl )."Coluna CSLL / BRL
              wl_zglt036-vlr_moeda_forte = ''.
              wl_zglt036-vlr_moeda_grupo = ''.
            WHEN '4'.
              v_tabix = 'X'.
              wl_zglt036-vlr_moeda_doc = abs( v_csll_brl ). "Coluna CSLL / BRL
              wl_zglt036-vlr_moeda_int = abs( v_csll_brl )."Coluna CSLL / BRL
              wl_zglt036-vlr_moeda_forte = ''.
              wl_zglt036-vlr_moeda_grupo = ''.
          ENDCASE.
        ENDIF.

      ENDIF.

      wl_zglt036-seqitem = sy-tabix.
      IF v_tabix = 'X'.
        APPEND wl_zglt036 TO gt_zglt036.
      ENDIF.

      CLEAR: wl_zglt036, wa_zglt032.

    ENDLOOP.

    CALL METHOD zcl_gerar_lote=>contabilizar_lote(
      CHANGING
        i_zglt036 = gt_zglt036
        i_zglt035 = wl_zglt035 ).

    COMMIT WORK.

    LOOP AT tg_lote_ger.

      CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
        EXPORTING
          p_num_lote = tg_lote_ger-lote.

    ENDLOOP.


    CONCATENATE 'ZGL17' wl_zglt035-doc_lcto s_ano-low INTO v_objkey.

*** US #181728 - MMSILVA - 02.07.2025 - Ini ***
    SELECT SINGLE obj_key FROM zib_contabil INTO @DATA(ls_zib_contabil) WHERE obj_key = @v_objkey.

    IF sy-subrc IS INITIAL.
      UPDATE zib_contabil SET zuonr = 'IMPOSTO DIFERIDO' WHERE obj_key = v_objkey.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.
*** US #181728 - MMSILVA - 02.07.2025 - Fim ***

    SELECT SINGLE * FROM zglt0106 INTO @DATA(w_zglt0106) WHERE bukrs = @s_empre-low AND
                              mes = @s_mes-low AND
                              ano = @s_ano-low AND
                              tipo_imposto = @<saida>-tp_imposto.

    IF sy-subrc IS INITIAL.
      CONCATENATE 'ZGL17' wl_zglt035-doc_lcto s_ano-low INTO v_objkey.
      LOOP AT t_alv_saida INTO DATA(wa_saida2) WHERE tp_imposto = <saida>-tp_imposto.

        UPDATE zglt0106 SET doc_lcto = wl_zglt035-doc_lcto
                            doc_contabil = '0000000000'
                            doc_estorno = '0000000000'
                            saldoanual_brl = wa_saida2-saldo_brl
                            saldoanual_usd = wa_saida2-saldo_usd
                            movimentacao_brl = wa_saida2-movi_brl
                            movimentacao_usd = wa_saida2-movi_usd
                            saldoatual_brl = wa_saida2-saldo_atu_brl
                            saldoatual_usd = wa_saida2-saldo_atu_usd
                            irpj_brl = wa_saida2-irpj_brl
                            irpj_usd = wa_saida2-irpj_usd
                            csll_brl = wa_saida2-csll_brl
                            csll_usd = wa_saida2-csll_usd
*** BUG - 152228 - Inicio - CBRAND
                            s_irpj_brl    = wa_saida2-s_irpj_brl
                            s_irpj_usd    = wa_saida2-s_irpj_usd
                            s_csll_brl    = wa_saida2-s_csll_brl
                            s_csll_usd    = wa_saida2-s_csll_usd
*** BUG - 152228 - Fim - CBRAND
                            lote = e_num_lote
                            obj_key = v_objkey
      WHERE bukrs = s_empre-low AND
            mes = s_mes-low AND
            ano = s_ano-low AND
            tipo_imposto = wa_saida2-tp_imposto AND
            conta = wa_saida2-conta.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ENDIF.

      ENDLOOP.
    ELSE.


      LOOP AT t_alv_saida INTO DATA(wa_saida1) WHERE tp_imposto = <saida>-tp_imposto.

        wl_zglt0106-bukrs = s_empre-low.
        wl_zglt0106-mes = s_mes-low.
        wl_zglt0106-ano = s_ano-low.
        wl_zglt0106-tipo_imposto = <saida>-tp_imposto.
        wl_zglt0106-conta = wa_saida1-conta.
        wl_zglt0106-saldoanual_brl = wa_saida1-saldo_brl.
        wl_zglt0106-saldoanual_usd = wa_saida1-saldo_usd.
        wl_zglt0106-movimentacao_brl = wa_saida1-movi_brl.
        wl_zglt0106-movimentacao_usd = wa_saida1-movi_usd.
        wl_zglt0106-saldoatual_brl = wa_saida1-saldo_atu_brl.
        wl_zglt0106-saldoatual_usd = wa_saida1-saldo_atu_usd.

        wl_zglt0106-m_irpj_brl = wa_saida1-m_irpj_brl.
        wl_zglt0106-m_irpj_usd = wa_saida1-m_irpj_usd.
        wl_zglt0106-m_csll_brl = wa_saida1-m_csll_brl.
        wl_zglt0106-m_csll_usd = wa_saida1-m_csll_usd.

        wl_zglt0106-irpj_brl = wa_saida1-irpj_brl. "v_irpj_brl.
        wl_zglt0106-irpj_usd = wa_saida1-irpj_usd. "v_irpj_usd.
        wl_zglt0106-csll_brl = wa_saida1-csll_brl. "v_csll_brl.
        wl_zglt0106-csll_usd = wa_saida1-csll_usd. "v_csll_usd.

*** BUG - 152228 - Inicio - CBRAND
        wl_zglt0106-s_irpj_brl    = wa_saida1-s_irpj_brl.
        wl_zglt0106-s_irpj_usd    = wa_saida1-s_irpj_usd.
        wl_zglt0106-s_csll_brl    = wa_saida1-s_csll_brl.
        wl_zglt0106-s_csll_usd    = wa_saida1-s_csll_usd.
*** BUG - 152228 - Fim - CBRAND

        wl_zglt0106-doc_lcto = wl_zglt035-doc_lcto.
        wl_zglt0106-lote = e_num_lote.
        wl_zglt0106-obj_key = v_objkey.
        MODIFY zglt0106 FROM wl_zglt0106.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT t_alv_saida ASSIGNING <saida1> WHERE tp_imposto = <saida>-tp_imposto.
      MOVE ' ' TO <saida1>-doc_contabil.
      MOVE ' ' TO <saida1>-doc_estorno.
      MOVE wl_zglt035-doc_lcto TO <saida1>-doc_lcto.
      MOVE e_num_lote TO <saida1>-lote.
      MOVE icon_operation TO <saida1>-status.
      MOVE v_objkey TO <saida1>-obj_key.
    ENDLOOP.

    "Gerar dois documentos quando a moeda BRL for <> de USD "DEVK9A209H - 16/05/24 - #138696 RSA
    IF v_gerar_segundo_doc = abap_true."DEVK9A209H - 16/05/24 - #138696 RSA.


      IF     v_saldo EQ '1'.
        v_saldo = '2'.
      ELSEIF v_saldo EQ '2'.
        v_saldo = '1'.
      ENDIF.

      SELECT  * FROM zglt0102 INTO TABLE @DATA(it_zglt0102_2) WHERE tp_imposto = @<saida>-tp_imposto AND  saldo = @v_saldo.

      IF sy-subrc NE 0.
        CONCATENATE 'Tipo de imposto' <saida>-tp_imposto 'sem parâmetro de contabilização!' INTO DATA(w_msg_2) SEPARATED BY space.
        MESSAGE w_msg_2 TYPE 'E'.
        CONTINUE.
      ENDIF.

      dp_resp = '83'. "'09'. "Contabilidade " bug 99642

      CLEAR e_num_lote.
      CALL METHOD zcl_gerar_lote=>create_lote
        EXPORTING
          i_bukrs       = s_empre-low
          i_descr_lote  = 'Imposto Diferido'
          i_user_resp   = sy-uname
          i_dep_resp    = dp_resp
          i_status_lote = 'L'
        IMPORTING
          e_num_lote    = e_num_lote.

      CLEAR: tg_lote_ger, tg_lote_ger[].
      tg_lote_ger-bukrs = s_empre-low.
      tg_lote_ger-lote  = e_num_lote.
      APPEND tg_lote_ger.



*    SELECT * FROM zglt0102 INTO TABLE @DATA(it_zglt0102) WHERE tp_imposto EQ @<saida>-tp_imposto.
*
*      if v_irpj_brl =  0 and v_csll_brl = 0.
*
*        IF v_irpj_usd < 0.
*      v_saldo = '2'.
*    ELSEIF v_irpj_usd > 0.
*      v_saldo = '1'.
*    ELSEIF v_irpj_usd = 0.
*      IF v_csll_usd < 0.
*        v_saldo = '2'.
*      ELSE.
*        v_saldo = '1'.
*      ENDIF.
*    ENDIF.
*        else.
*
*          IF v_irpj_brl < 0.
*      v_saldo = '2'.
*    ELSEIF v_irpj_brl > 0.
*      v_saldo = '1'.
*    ELSEIF v_irpj_brl = 0.
*      IF v_csll_brl < 0.
*        v_saldo = '2'.
*      ELSE.
*        v_saldo = '1'.
*      ENDIF.
*    ENDIF.
*
*          endif.


      READ TABLE it_zglt0102_2 INTO DATA(w_it_zglt0102_2) WITH KEY saldo = v_saldo.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE blart  bktxt prov_est FROM zglt031 INTO ( v_blart, v_bktxt, v_prov_est ) WHERE tp_lcto = w_it_zglt0102_2-tp_lcto.

      ENDIF.

      CLEAR wl_zglt035.
      MOVE:    e_num_lote                  TO wl_zglt035-lote,
               s_empre-low                 TO wl_zglt035-bukrs,
               w_it_zglt0102_2-tp_lcto     TO wl_zglt035-tp_lcto,
               dp_resp                     TO wl_zglt035-dpto_resp,
               'USD'                       TO wl_zglt035-moeda_doc,
               v_blart                     TO wl_zglt035-blart,
               'IMPOSTO DIFERIDO'          TO wl_zglt035-xblnr,
               v_bktxt                     TO wl_zglt035-bktxt,
               v_ultimo_dia                TO wl_zglt035-budat,
               v_ultimo_dia                TO wl_zglt035-bldat,
               sy-datum                    TO wl_zglt035-dt_lcto,
               v_prov_est                  TO wl_zglt035-prov_est,
               s_mes-low                   TO wl_zglt035-monat,
               s_ano-low                   TO wl_zglt035-gjahr,
               sy-uname                    TO wl_zglt035-usnam,
               sy-datum                    TO wl_zglt035-dt_entrada,
               sy-uzeit                    TO wl_zglt035-hr_entrada.

      IF v_irpj_brl = '0' AND v_csll_brl = '0' AND v_irpj_usd <> '0'  AND  v_csll_usd <> '0'. "Encontrou saldo Somente em dolar
        MOVE              'X'                  TO wl_zglt035-st_lc_moeda.
      ELSE.
        MOVE              ' '                  TO wl_zglt035-st_lc_moeda.
      ENDIF.

      IF v_gerar_segundo_doc EQ abap_true."DEVK9A209H - 17/05/24 - #138696 RSA
        MOVE              'X'                  TO wl_zglt035-st_lc_moeda.
      ENDIF.

      CLEAR: gt_zglt036, wl_zglt036.

      SELECT * FROM zglt032 INTO TABLE @DATA(it_zglt032_2) WHERE tp_lcto = @wl_zglt035-tp_lcto.
      SORT it_zglt032_2 BY buzei.
      LOOP AT it_zglt032_2 INTO wa_zglt032.
        wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
        wl_zglt036-hkont   = wa_zglt032-hkont.
        wl_zglt036-bschl   = wa_zglt032-bschl.
        wl_zglt036-sgtxt   = wa_zglt032-sgtxt.

        CONCATENATE s_empre-low+2(2) '01' INTO wl_zglt036-gsber.
        "wl_zglt036-gsber = '0101'.
        IF v_gerar_segundo_doc EQ abap_true."DEVK9A209H - 17/05/24 - #138696 RSA
          IF     v_irpj_brl <> '0' AND v_irpj_usd <> '0' AND v_csll_brl <> '0' AND v_csll_usd <> '0'.
            CASE sy-tabix.
              WHEN '1'.
                v_tabix = 'X'.
                wl_zglt036-vlr_moeda_doc = ''. "Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_int = ''."Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_forte = abs( v_irpj_usd ).
                wl_zglt036-vlr_moeda_grupo = abs( v_irpj_usd ).
              WHEN '2'.
                v_tabix = 'X'.
                wl_zglt036-vlr_moeda_doc = ''. "Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_int = ''."Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_forte = abs( v_irpj_usd ).
                wl_zglt036-vlr_moeda_grupo = abs( v_irpj_usd ).
              WHEN '3'.
                v_tabix = 'X'.
                wl_zglt036-vlr_moeda_doc = ''. "Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_int = ''."Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_forte = abs( v_csll_usd ).
                wl_zglt036-vlr_moeda_grupo = abs( v_csll_usd ).
              WHEN '4'.
                v_tabix = 'X'.
                wl_zglt036-vlr_moeda_doc = ''."Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_int = ''."Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_forte = abs( v_csll_usd ).
                wl_zglt036-vlr_moeda_grupo = abs( v_csll_usd ).
            ENDCASE.
          ELSEIF     v_irpj_brl <> '0' AND v_irpj_usd <> '0' AND v_csll_brl = '0' AND v_csll_usd = '0'.
            CASE sy-tabix.
              WHEN '1'.
                v_tabix = 'X'.
                wl_zglt036-vlr_moeda_doc = ''."Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_int = ''."Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_forte = abs( v_irpj_usd ).
                wl_zglt036-vlr_moeda_grupo = abs( v_irpj_usd ).
              WHEN '2'.
                v_tabix = 'X'.
                wl_zglt036-vlr_moeda_doc = ''. "Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_int = ''."Coluna IRPJ / BRL
                wl_zglt036-vlr_moeda_forte = abs( v_irpj_usd ).
                wl_zglt036-vlr_moeda_grupo = abs( v_irpj_usd ).
              WHEN '3'.
                v_tabix = '0'.
              WHEN '4'.
                v_tabix = '0'.
            ENDCASE.
          ELSEIF v_irpj_brl = '0' AND v_irpj_usd = '0' AND v_csll_brl <> '0' AND v_csll_usd <> '0'.
            CASE sy-tabix.
              WHEN '1'.
                v_tabix = '0'.
              WHEN '2'.
                v_tabix = '0'.
              WHEN '3'.
                v_tabix = 'X'.
                wl_zglt036-vlr_moeda_doc = ''. "Coluna CSLL / BRL
                wl_zglt036-vlr_moeda_int = ''."Coluna CSLL / BRL
                wl_zglt036-vlr_moeda_forte = abs( v_csll_usd ).
                wl_zglt036-vlr_moeda_grupo = abs( v_csll_usd ).
              WHEN '4'.
                v_tabix = 'X'.
                wl_zglt036-vlr_moeda_doc = ''. "Coluna CSLL / BRL
                wl_zglt036-vlr_moeda_int = ''."Coluna CSLL / BRL
                wl_zglt036-vlr_moeda_forte = abs( v_csll_usd ).
                wl_zglt036-vlr_moeda_grupo = abs( v_csll_usd ).
            ENDCASE.
          ENDIF.
        ENDIF.


        wl_zglt036-seqitem = sy-tabix.
        IF v_tabix = 'X'.
          APPEND wl_zglt036 TO gt_zglt036.
        ENDIF.

        CLEAR: wl_zglt036, wa_zglt032.

      ENDLOOP.

      CALL METHOD zcl_gerar_lote=>contabilizar_lote(
        CHANGING
          i_zglt036 = gt_zglt036
          i_zglt035 = wl_zglt035 ).

      COMMIT WORK.

      LOOP AT tg_lote_ger.

        CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
          EXPORTING
            p_num_lote = tg_lote_ger-lote.

      ENDLOOP.

      CLEAR v_objkey.
      CONCATENATE 'ZGL17' wl_zglt035-doc_lcto s_ano-low INTO v_objkey.

      SELECT SINGLE * FROM zglt0106 INTO @DATA(w_zglt0106_2)
                                WHERE bukrs        = @s_empre-low AND
                                      mes          = @s_mes-low   AND
                                      ano          = @s_ano-low   AND
                                      tipo_imposto = @<saida>-tp_imposto.

      IF sy-subrc IS INITIAL.
        CONCATENATE 'ZGL17' wl_zglt035-doc_lcto s_ano-low INTO v_objkey.
        LOOP AT t_alv_saida INTO DATA(wa_alv_saida2) WHERE tp_imposto = <saida>-tp_imposto.

          UPDATE zglt0106 SET doc_lcto_2       = wl_zglt035-doc_lcto
                              doc_contabil     = '0000000000'
                              doc_estorno      = '0000000000'
                              saldoanual_brl   = wa_alv_saida2-saldo_brl
                              saldoanual_usd   = wa_alv_saida2-saldo_usd
                              movimentacao_brl = wa_alv_saida2-movi_brl
                              movimentacao_usd = wa_alv_saida2-movi_usd
                              saldoatual_brl   = wa_alv_saida2-saldo_atu_brl
                              saldoatual_usd   = wa_alv_saida2-saldo_atu_usd
                              irpj_brl         = wa_alv_saida2-irpj_brl
                              irpj_usd         = wa_alv_saida2-irpj_usd
                              csll_brl         = wa_alv_saida2-csll_brl
                              csll_usd         = wa_alv_saida2-csll_usd
                              lote_2           = e_num_lote
                              obj_key_2        = v_objkey
                        WHERE bukrs        = s_empre-low
                        AND   mes          = s_mes-low
                        AND   ano          = s_ano-low
                        AND   tipo_imposto = wa_alv_saida2-tp_imposto
                        AND   conta        = wa_alv_saida2-conta.
          IF sy-subrc EQ 0.
            COMMIT WORK.
          ENDIF.

        ENDLOOP.
      ELSE.


        LOOP AT t_alv_saida INTO DATA(wa_saida_2) WHERE tp_imposto = <saida>-tp_imposto.

          wl_zglt0106-bukrs            = s_empre-low.
          wl_zglt0106-mes              = s_mes-low.
          wl_zglt0106-ano              = s_ano-low.
          wl_zglt0106-tipo_imposto     = <saida>-tp_imposto.
          wl_zglt0106-conta            = wa_saida_2-conta.
          wl_zglt0106-saldoanual_brl   = wa_saida_2-saldo_brl.
          wl_zglt0106-saldoanual_usd   = wa_saida_2-saldo_usd.
          wl_zglt0106-movimentacao_brl = wa_saida_2-movi_brl.
          wl_zglt0106-movimentacao_usd = wa_saida_2-movi_usd.
          wl_zglt0106-saldoatual_brl   = wa_saida_2-saldo_atu_brl.
          wl_zglt0106-saldoatual_usd   = wa_saida_2-saldo_atu_usd.

          wl_zglt0106-m_irpj_brl       = wa_saida_2-m_irpj_brl.
          wl_zglt0106-m_irpj_usd       = wa_saida_2-m_irpj_usd.
          wl_zglt0106-m_csll_brl       = wa_saida_2-m_csll_brl.
          wl_zglt0106-m_csll_usd       = wa_saida_2-m_csll_usd.

          wl_zglt0106-irpj_brl         = wa_saida_2-irpj_brl. "v_irpj_brl.
          wl_zglt0106-irpj_usd         = wa_saida_2-irpj_usd. "v_irpj_usd.
          wl_zglt0106-csll_brl         = wa_saida_2-csll_brl. "v_csll_brl.
          wl_zglt0106-csll_usd         = wa_saida_2-csll_usd. "v_csll_usd.

          wl_zglt0106-doc_lcto_2       = wl_zglt035-doc_lcto.
          wl_zglt0106-lote_2           = e_num_lote.
          wl_zglt0106-obj_key_2        = v_objkey.
          MODIFY zglt0106 FROM wl_zglt0106.
          IF sy-subrc EQ 0.
            COMMIT WORK.
          ENDIF.
        ENDLOOP.
      ENDIF.

      LOOP AT t_alv_saida ASSIGNING <saida1> WHERE tp_imposto = <saida>-tp_imposto.
        MOVE ' '                 TO <saida1>-doc_contabil.
        MOVE ' '                 TO <saida1>-doc_estorno.
        "MOVE wl_zglt035-doc_lcto TO <saida1>-doc_lcto.
        MOVE wl_zglt035-doc_lcto TO <saida1>-doc_lcto_2.
        MOVE e_num_lote          TO <saida1>-lote_2.
        MOVE icon_operation      TO <saida1>-status.
        MOVE v_objkey            TO <saida1>-obj_key_2.
      ENDLOOP.

    ENDIF.

    CLEAR: v_saldo_brl , v_saldo_usd , v_movi_brl, v_movi_usd, v_saldo_atu_brl, v_saldo_atu_usd, v_gerar_segundo_doc,
          v_irpj_brl, v_irpj_usd, v_csll_brl, v_csll_usd, v_blart, v_bktxt, v_prov_est, w_it_zglt0102, wl_zglt035, w_zglt0106, wa_saida2, wa_saida1.

    REFRESH:  tg_lote_ger, gt_zglt036.

  ENDLOOP.
  PERFORM f_editar_linha.
  CALL METHOD g_grid->refresh_table_display.

ENDFORM.

FORM refresh_doc_contabil .

  FIELD-SYMBOLS: <saida> TYPE ty_alv_saida.
  FIELD-SYMBOLS: <saida1> TYPE ty_alv_saida.

  DATA: wl_zglt034     TYPE zglt034,
        wl_zglt035     TYPE zglt035,
        wl_zib_chave   TYPE zib_contabil_chv,
        wl_zib_erro    TYPE zib_contabil_err,
        wl_zib_chave_2 TYPE zib_contabil_chv,
        wl_zib_erro_2  TYPE zib_contabil_err.

  DATA: v_stblg   TYPE bkpf-stblg,
        v_stblg_2 TYPE bkpf-stblg.

  DATA: rg_doc_contabil TYPE RANGE OF bkpf-belnr,
        wa_doc_contabil LIKE LINE OF rg_doc_contabil.

  CHECK g_grid IS NOT INITIAL.

  UNASSIGN: <saida>, <saida1>.
  CHECK t_alv_saida IS NOT INITIAL.

  LOOP AT t_alv_saida ASSIGNING <saida>.

    SELECT *
      FROM zglt0106
    INTO TABLE @DATA(rec_zgl0106)
      WHERE bukrs            = @s_empre-low
            AND mes          = @s_mes-low
            AND ano          = @s_ano-low
            AND tipo_imposto = @<saida>-tp_imposto
            AND conta        = @<saida>-conta.

    READ TABLE rec_zgl0106 INTO DATA(w_rec_zgl0106) INDEX 1.

    CLEAR: wl_zib_chave, wl_zib_erro, wl_zib_chave_2, wl_zib_erro_2.

    PERFORM retorna_status_zib USING w_rec_zgl0106-doc_lcto
                                     s_ano-low"WL_ZGLT034-DATA_ATUAL(4)
                            CHANGING wl_zib_chave
                                     wl_zib_erro.

    PERFORM retorna_status_zib_2 USING    w_rec_zgl0106-doc_lcto_2
                                          s_ano-low"WL_ZGLT034-DATA_ATUAL(4)
                                 CHANGING wl_zib_chave_2
                                          wl_zib_erro_2.

    IF ( wl_zib_chave   IS NOT INITIAL ) AND NOT <saida>-lote IS INITIAL.
      MOVE: icon_led_green             TO <saida>-status,
            wl_zib_chave-belnr   TO <saida>-doc_contabil.

      IF NOT s_empre-low IS INITIAL AND NOT w_rec_zgl0106-doc_lcto IS INITIAL.
        UPDATE zglt0106 SET doc_contabil = wl_zib_chave-belnr
                            usuario      = sy-uname
                            data         = sy-datum
                            hora         = sy-uzeit
          WHERE bukrs    EQ s_empre-low
          AND   doc_lcto EQ w_rec_zgl0106-doc_lcto.
        IF sy-subrc EQ 0. "DEVK9A1Z8L - 26.04.2024 FI - Erro documento se repetindo  - #139304 RSA
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ( wl_zib_chave_2 IS NOT INITIAL ) AND NOT <saida>-lote_2 IS INITIAL."DEVK9A1Z8L - 26.04.2024 FI - Erro documento se repetindo  - #139304 RSA
      MOVE: icon_led_green             TO <saida>-status,
            wl_zib_chave_2-belnr TO <saida>-doc_contabil_2.
      IF NOT s_empre-low IS INITIAL AND NOT w_rec_zgl0106-doc_lcto_2 IS INITIAL.
        UPDATE zglt0106 SET doc_contabil_2 = wl_zib_chave_2-belnr
                            usuario        = sy-uname
                            data           = sy-datum
                            hora           = sy-uzeit
          WHERE bukrs      EQ s_empre-low
          AND   doc_lcto_2 EQ w_rec_zgl0106-doc_lcto_2.
        IF sy-subrc EQ 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDIF.

*** BUG - 152228 - Inicio - CBRAND
    " 1º doc lancto  - preenchido
    " 2º doc lancto  - preenchido
    IF w_rec_zgl0106-doc_lcto_2 IS NOT INITIAL AND w_rec_zgl0106-doc_lcto IS NOT  INITIAL.
      IF wl_zib_chave-belnr IS NOT INITIAL AND wl_zib_chave_2-belnr IS NOT INITIAL.
        MOVE: icon_led_green             TO <saida>-status.
      ELSE.
        IF wl_zib_erro IS NOT INITIAL OR wl_zib_erro_2 IS NOT INITIAL.
          MOVE icon_led_red     TO <saida>-status.
        ELSE.
          MOVE icon_led_yellow     TO <saida>-status.
        ENDIF.
      ENDIF.
    ENDIF.

    "1º doc lancto  - preenchido
    "2º doc lancto  - vazio
    "Validar apenas 1º doc lancto
    IF w_rec_zgl0106-doc_lcto IS NOT  INITIAL AND w_rec_zgl0106-doc_lcto_2 IS  INITIAL.
      IF wl_zib_chave-belnr IS NOT INITIAL .
        MOVE: icon_led_green             TO <saida>-status.
      ELSE.
        IF wl_zib_erro IS NOT INITIAL .
          MOVE icon_led_red     TO <saida>-status.
        ELSE.
          MOVE icon_led_yellow     TO <saida>-status.
        ENDIF.
      ENDIF.
    ENDIF.

    "1º doc lancto  - vazio
    "2º doc lancto  - preenchido
    "Validar apenas 2º doc lancto
    IF w_rec_zgl0106-doc_lcto_2 IS NOT INITIAL AND w_rec_zgl0106-doc_lcto IS INITIAL.
      IF wl_zib_chave_2-belnr IS NOT INITIAL.
        MOVE: icon_led_green             TO <saida>-status.
      ELSE.
        IF  wl_zib_erro_2 IS NOT INITIAL.
          MOVE icon_led_red     TO <saida>-status.
        ELSE.
          MOVE icon_led_yellow     TO <saida>-status.
        ENDIF.
      ENDIF.
    ENDIF.

*    IF wl_zib_erro IS NOT INITIAL AND wl_zib_erro_2 IS NOT INITIAL.
*      MOVE icon_led_red     TO <saida>-status.
*    ELSE.
*
*      IF <saida>-doc_contabil IS NOT  INITIAL  AND <saida>-doc_contabil_2 IS NOT  INITIAL.
*        IF  w_rec_zgl0106-doc_estorno IS NOT INITIAL AND w_rec_zgl0106-doc_estorno_2 IS NOT INITIAL.
*          MOVE icon_led_yellow     TO <saida>-status.
*        ELSE.
*          MOVE icon_led_green      TO <saida>-status.
*        ENDIF.
*      ELSE .
*        IF w_rec_zgl0106-doc_lcto IS NOT INITIAL AND w_rec_zgl0106-doc_lcto_2 IS NOT INITIAL.
*          MOVE icon_operation    TO <saida>-status.
*        ELSE.
*          MOVE icon_led_yellow     TO <saida>-status.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*** BUG - 152228 - Fim - CBRAND

    IF <saida>-doc_estorno IS INITIAL AND <saida>-doc_estorno_2 IS INITIAL.

      "DEVK9A209H - 16/05/24 - #138696 RSA
      CLEAR rg_doc_contabil[].

      wa_doc_contabil-sign   = 'I'.
      wa_doc_contabil-option = 'EQ'.
      IF NOT <saida>-doc_contabil IS INITIAL.
        wa_doc_contabil-low    = <saida>-doc_contabil.
        APPEND wa_doc_contabil TO rg_doc_contabil.
      ENDIF.

      wa_doc_contabil-sign   = 'I'.
      wa_doc_contabil-option = 'EQ'.
      IF NOT <saida>-doc_contabil_2 IS INITIAL.
        wa_doc_contabil-low    = <saida>-doc_contabil_2.
        APPEND wa_doc_contabil TO rg_doc_contabil.
      ENDIF.
      "DEVK9A209H - 16/05/24 - #138696 RSA

      IF NOT rg_doc_contabil[] IS INITIAL.
        SELECT belnr, stblg
               FROM bkpf
               INTO TABLE @DATA(t_bkpf)
               WHERE bukrs EQ @s_empre-low
               AND   belnr IN @rg_doc_contabil.

        READ TABLE t_bkpf INTO DATA(wa_bkpf) WITH KEY belnr = <saida>-doc_contabil.
        v_stblg   = wa_bkpf-stblg.
        READ TABLE t_bkpf INTO wa_bkpf       WITH KEY belnr = <saida>-doc_contabil_2.
        v_stblg_2 = wa_bkpf-stblg.
      ENDIF.

      IF v_stblg IS NOT INITIAL AND v_stblg_2 IS NOT INITIAL.
        LOOP AT  t_alv_saida ASSIGNING <saida1> WHERE tp_imposto = <saida>-tp_imposto.

          MOVE v_stblg   TO <saida1>-doc_estorno.
          MOVE v_stblg_2 TO <saida1>-doc_estorno_2."DEVK9A209H - 16/05/24 - #138696 RSA
          MOVE icon_led_yellow TO <saida1>-status.

          SELECT SINGLE budat FROM bkpf INTO @DATA(v_budat) WHERE bukrs = @s_empre-low AND belnr = @<saida1>-doc_estorno.

          IF v_budat <> v_ultimo_dia.

            w_alv_saida-line_color = 'C601'.
          ENDIF.

          SELECT SINGLE budat FROM bkpf INTO @v_budat WHERE bukrs = @s_empre-low AND belnr = @<saida1>-doc_estorno_2.

          IF v_budat <> v_ultimo_dia.
            w_alv_saida-line_color = 'C601'.
          ENDIF.

        ENDLOOP.

        IF NOT s_empre-low          IS INITIAL AND NOT s_mes-low IS INITIAL          AND
           NOT s_ano-low            IS INITIAL AND NOT <saida>-tp_imposto IS INITIAL AND
           NOT <saida>-doc_contabil IS INITIAL.

          UPDATE zglt0106 SET doc_estorno = v_stblg
            WHERE bukrs        = s_empre-low
              AND   mes          = s_mes-low
              AND   ano          = s_ano-low
              AND   tipo_imposto = <saida>-tp_imposto
              AND   doc_contabil = <saida>-doc_contabil.
          IF sy-subrc EQ 0. "DEVK9A1Z8L - 26.04.2024 FI - Erro documento se repetindo  - #139304 RSA
            COMMIT WORK.
          ENDIF.
        ENDIF.

        IF NOT s_empre-low            IS INITIAL AND NOT s_mes-low IS INITIAL          AND
           NOT s_ano-low              IS INITIAL AND NOT <saida>-tp_imposto IS INITIAL AND
           NOT <saida>-doc_contabil_2 IS INITIAL.

          UPDATE zglt0106 SET doc_estorno_2 = v_stblg_2
            WHERE bukrs          = s_empre-low
              AND   mes            = s_mes-low
              AND   ano            = s_ano-low
              AND   tipo_imposto   = <saida>-tp_imposto
              AND   doc_contabil_2 = <saida>-doc_contabil_2.
          IF sy-subrc EQ 0. "DEVK9A1Z8L - 26.04.2024 FI - Erro documento se repetindo  - #139304 RSA
            COMMIT WORK.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    MESSAGE s836(sd) WITH TEXT-009.

    CLEAR: rec_zgl0106, w_rec_zgl0106,wl_zib_chave, wl_zib_erro, v_stblg.

  ENDLOOP.
  PERFORM f_editar_linha.

  CALL METHOD g_grid->refresh_table_display.

ENDFORM.

FORM retorna_status_zib USING i_doc_lcto TYPE zglt077-doc_lcto
                              i_ano_lcto TYPE zglt077-gjahr
                     CHANGING e_zibchv   TYPE zib_contabil_chv
                              e_ziberr   TYPE zib_contabil_err.

  DATA v_objkey    TYPE char20.
  CLEAR: e_zibchv, e_ziberr.

  CONCATENATE 'ZGL17' i_doc_lcto i_ano_lcto INTO v_objkey.

  SELECT SINGLE *
    FROM zib_contabil_chv
    INTO e_zibchv
   WHERE obj_key = v_objkey.

  IF ( sy-subrc IS NOT INITIAL ).

    SELECT SINGLE *
      FROM zib_contabil_err
      INTO e_ziberr
     WHERE obj_key = v_objkey.
  ENDIF.

ENDFORM.

FORM estornar_contabilizacao.

  DATA: var_answer TYPE c.
  DATA: v_tipo_imposto  TYPE zglt0101-tp_imposto.

  FIELD-SYMBOLS: <saida> TYPE ty_alv_saida.
  FIELD-SYMBOLS: <saida1> TYPE ty_alv_saida.
  CLEAR: it_sel_rows[], wa_sel_rows.
  UNASSIGN: <saida>, <saida1>.

  DATA: it_msg TYPE TABLE OF bdcmsgcoll, " WITH HEADER LINE,
        wa_msg TYPE bdcmsgcoll.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK NOT it_sel_rows IS INITIAL.

  IF ( lines( it_sel_rows ) < 1 ).
    MESSAGE s836(sd) WITH TEXT-011.
    EXIT.
  ENDIF.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente estornar o documento contábil?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    "READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
    READ TABLE t_alv_saida ASSIGNING <saida> INDEX wa_sel_rows-index.

    CHECK <saida>-doc_contabil   IS NOT INITIAL.

    SORT t_alv_saida ASCENDING BY tp_imposto.


    IF v_tipo_imposto IS INITIAL OR v_tipo_imposto NE <saida>-tp_imposto.

      SELECT SINGLE *
        INTO @DATA(wa_bsad)
        FROM bsas_view
       WHERE belnr = @<saida>-doc_contabil
         AND bukrs = @s_empre-low.

      IF sy-subrc = 0.
        MESSAGE 'Documento de Compensação deve ser cancelado primeiro!' TYPE 'S'.
        RETURN.
      ENDIF.

      FREE: it_dta.
      DEFINE shdb.
        CLEAR it_dta.
        wa_dta-program   = &1.
        wa_dta-dynpro    = &2.
        wa_dta-dynbegin  = &3.
        wa_dta-fnam      = &4.
        wa_dta-fval      = &5.
        APPEND wa_dta TO it_dta.
      END-OF-DEFINITION.

      shdb:
      'SAPMF05A' '0105' 'X'  ' '           ' ',
      ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
      ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
      ' '        ' '    ' '  'RF05A-BELNS' <saida>-doc_contabil,
      ' '        ' '    ' '  'BKPF-BUKRS'  s_empre-low,
      ' '        ' '    ' '  'RF05A-GJAHS' s_ano-low,
      ' '        ' '    ' '  'UF05A-STGRD' '01'.

      opt-dismode = 'E'.
      CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.




      IF sy-subrc IS INITIAL.
        " MOVE '@08@'     TO <saida>-status.

        SELECT SINGLE stblg
          FROM bkpf
          INTO @DATA(v_stblg)
          WHERE bukrs = @s_empre-low AND
          belnr = @<saida>-doc_contabil.

        IF v_stblg IS NOT INITIAL.

          LOOP AT  t_alv_saida ASSIGNING <saida1> WHERE tp_imposto = <saida>-tp_imposto.

            MOVE v_stblg TO <saida1>-doc_estorno.
            MOVE icon_led_yellow TO <saida1>-status.

          ENDLOOP.

          UPDATE zglt0106 SET doc_estorno = v_stblg WHERE bukrs = s_empre-low AND
               mes   = s_mes-low AND
               ano   = s_ano-low AND
               tipo_imposto = <saida>-tp_imposto AND
               doc_contabil = <saida>-doc_contabil .

        ENDIF.

      ENDIF.

      v_tipo_imposto = <saida>-tp_imposto.


      "DEVK9A209H - 16/05/24 - #138696 RSA
      "Estorna Doc. contábil 2
      SELECT SINGLE *
             INTO @DATA(wa_bsas_view)
             FROM bsas_view
             WHERE belnr = @<saida>-doc_contabil_2
             AND   bukrs = @s_empre-low.
      IF sy-subrc = 0.
        MESSAGE 'Documento de Compensação deve ser cancelado primeiro!' TYPE 'S'.
        RETURN.
      ENDIF.

      FREE: it_dta.
      DEFINE shdb.
        CLEAR it_dta.
        wa_dta-program   = &1.
        wa_dta-dynpro    = &2.
        wa_dta-dynbegin  = &3.
        wa_dta-fnam      = &4.
        wa_dta-fval      = &5.
        APPEND wa_dta TO it_dta.
      END-OF-DEFINITION.

      CHECK <saida>-doc_contabil_2 IS NOT INITIAL.

      shdb:
      'SAPMF05A' '0105' 'X'  ' '           ' ',
      ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
      ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
      ' '        ' '    ' '  'RF05A-BELNS' <saida>-doc_contabil_2,
      ' '        ' '    ' '  'BKPF-BUKRS'  s_empre-low,
      ' '        ' '    ' '  'RF05A-GJAHS' s_ano-low,
      ' '        ' '    ' '  'UF05A-STGRD' '01'.

      opt-dismode = 'E'.
      CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

      IF sy-subrc IS INITIAL.
        " MOVE '@08@'     TO <saida>-status.

        SELECT stblg UP TO 1 ROWS
               FROM bkpf
               INTO @DATA(v_stblg_2)
               WHERE bukrs = @s_empre-low
               AND   belnr = @<saida>-doc_contabil_2.
        ENDSELECT.

        IF v_stblg_2 IS NOT INITIAL.
          LOOP AT  t_alv_saida ASSIGNING <saida1> WHERE tp_imposto = <saida>-tp_imposto.

            MOVE v_stblg_2       TO <saida1>-doc_estorno_2.
            MOVE icon_led_yellow TO <saida1>-status.
          ENDLOOP.

          UPDATE zglt0106 SET doc_estorno_2 = v_stblg_2
           WHERE bukrs          EQ s_empre-low
           AND   mes            EQ s_mes-low
           AND   ano            EQ s_ano-low
           AND   tipo_imposto   EQ <saida>-tp_imposto
           AND   doc_contabil_2 EQ <saida>-doc_contabil_2.

        ENDIF.

      ENDIF.
      "DEVK9A209H - 16/05/24 - #138696 RSA
    ENDIF.

  ENDLOOP.

  PERFORM f_editar_linha.

  CALL METHOD g_grid->refresh_table_display.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ESTORNO_SUBSEQUENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM estorno_subsequente .


  DATA: var_answer TYPE c,
        v_data     TYPE dats.
  DATA: v_tipo_imposto TYPE zglt0101-tp_imposto.


  DATA lv_ano TYPE bkpf-gjahr." LENGTH 4.
  DATA lv_mes TYPE bkpf-monat."c LENGTH 2.
  DATA lv_ano_budat TYPE c LENGTH 4.
  DATA lv_mes_budat TYPE c LENGTH 2.



  FIELD-SYMBOLS: <saida> TYPE ty_alv_saida.
  FIELD-SYMBOLS: <saida1> TYPE ty_alv_saida.

  CLEAR: it_sel_rows[], wa_sel_rows.
  UNASSIGN: <saida> , <saida1>.

  DATA: it_msg TYPE TABLE OF bdcmsgcoll, " WITH HEADER LINE,
        wa_msg TYPE bdcmsgcoll.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK NOT it_sel_rows IS INITIAL.

  IF ( lines( it_sel_rows ) < 1 ).
    MESSAGE s836(sd) WITH TEXT-011.
    EXIT.
  ENDIF.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente estornar o documento contábil?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.
  LOOP AT it_sel_rows INTO wa_sel_rows.
    "READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
    READ TABLE t_alv_saida ASSIGNING <saida> INDEX wa_sel_rows-index.

    CHECK <saida>-doc_contabil IS NOT INITIAL.
    SORT t_alv_saida ASCENDING BY tp_imposto.

    IF v_tipo_imposto IS INITIAL OR v_tipo_imposto NE <saida>-tp_imposto.

      SELECT SINGLE *
        INTO @DATA(wa_bsad)
        FROM bsas_view
       WHERE belnr = @<saida>-doc_contabil
         AND bukrs = @s_empre-low.

      IF sy-subrc = 0.
        MESSAGE 'Documento de Compensação deve ser cancelado primeiro!' TYPE 'S'.
        RETURN.
      ENDIF.


      IF <saida>-obj_key IS NOT INITIAL.
        "185994 - CS2025000543 - IMPOSTO DIFERIDO REVERSÃO - RGA - início
        SELECT *
          FROM zib_contabil
          INTO TABLE @DATA(lt_zibcont)
          WHERE obj_key   EQ @<saida>-obj_key.

        IF sy-subrc EQ 0.

          LOOP AT lt_zibcont ASSIGNING FIELD-SYMBOL(<fs_zibcont>).

            <fs_zibcont>-obj_key = <fs_zibcont>-obj_key && 'R'.

            "185994 - CS2025000543 - IMPOSTO DIFERIDO REVERSÃO - FA - início
*            SELECT SINGLE bschl
            SELECT SINGLE stbsl
        "185994 - CS2025000543 - IMPOSTO DIFERIDO REVERSÃO - FA - Fim

              FROM tbsl
              INTO @DATA(lv_bschl)
              WHERE bschl EQ @<fs_zibcont>-bschl.
            IF sy-subrc EQ 0.
              <fs_zibcont>-bschl = lv_bschl.
            ENDIF.

            " Extrair ano e mês
            lv_ano = <fs_zibcont>-bldat+6(4).
            lv_mes = <fs_zibcont>-bldat+3(2).

            PERFORM f_mes_subsequente USING lv_ano
                                            lv_mes
                                            CHANGING
                                            <fs_zibcont>-bldat.

            "budat
            lv_ano = <fs_zibcont>-budat+6(4).
            lv_mes = <fs_zibcont>-budat+3(2).

            PERFORM f_mes_subsequente USING lv_ano
                                            lv_mes
                                           CHANGING
                                           <fs_zibcont>-budat.

            <fs_zibcont>-gjahr = lv_ano.
            <fs_zibcont>-monat = lv_mes.
            <fs_zibcont>-rg_atualizado = 'N'.

          ENDLOOP.

          MODIFY zib_contabil FROM TABLE lt_zibcont.

        ENDIF.
      ENDIF.

      IF <saida>-obj_key_2 IS NOT INITIAL.
        "obj_key2
        SELECT *
          FROM zib_contabil
          INTO TABLE @DATA(lt_zibcont2)
          WHERE obj_key EQ @<saida>-obj_key_2.

        IF sy-subrc EQ 0.

          LOOP AT lt_zibcont2 ASSIGNING <fs_zibcont>.

            <fs_zibcont>-obj_key = <fs_zibcont>-obj_key && 'R'.

            CLEAR lv_bschl.

            "185994 - CS2025000543 - IMPOSTO DIFERIDO REVERSÃO - FA - início
*            SELECT SINGLE bschl
            SELECT SINGLE stbsl
        "185994 - CS2025000543 - IMPOSTO DIFERIDO REVERSÃO - FA - Fim
              FROM tbsl
              INTO @lv_bschl
              WHERE bschl EQ @<fs_zibcont>-bschl.
            IF sy-subrc EQ 0.
              <fs_zibcont>-bschl = lv_bschl.
            ENDIF.

            " Extrair ano e mês
            lv_ano = <fs_zibcont>-bldat+6(4).
            lv_mes = <fs_zibcont>-bldat+3(2).

            PERFORM f_mes_subsequente USING lv_ano
                                            lv_mes
                                            CHANGING
                                            <fs_zibcont>-bldat.

            "budat
            lv_ano = <fs_zibcont>-budat+6(4).
            lv_mes = <fs_zibcont>-budat+3(2).

            PERFORM f_mes_subsequente USING lv_ano
                                            lv_mes
                                           CHANGING
                                           <fs_zibcont>-budat.

            <fs_zibcont>-gjahr = lv_ano.
            <fs_zibcont>-monat = lv_mes.
            <fs_zibcont>-rg_atualizado = 'N'.

          ENDLOOP.

          MODIFY zib_contabil FROM TABLE lt_zibcont2.

        ENDIF.
      ENDIF.

*
*      v_mes = s_mes-low + 1.
*      v_ano = s_ano-low.
*      IF v_mes > 12.
*        v_mes = 01.
*        v_ano = s_ano-low + 1.
*      ENDIF.
*      CONCATENATE  '01'  v_mes  v_ano INTO v_data.

      "185994 - anulação da FB08
*      FREE: it_dta.
*      DEFINE shdb.
*        CLEAR it_dta.
*        wa_dta-program   = &1.
*        wa_dta-dynpro    = &2.
*        wa_dta-dynbegin  = &3.
*        wa_dta-fnam      = &4.
*        wa_dta-fval      = &5.
*        APPEND wa_dta TO it_dta.
*      END-OF-DEFINITION.
*
*
*      shdb:
*      'SAPMF05A' '0105' 'X'  ' '           ' ',
*      ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
*      ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
*      ' '        ' '    ' '  'RF05A-BELNS' <saida>-doc_contabil,
*      ' '        ' '    ' '  'BKPF-BUKRS'  s_empre-low,
*      ' '        ' '    ' '  'RF05A-GJAHS' s_ano-low,
*      ' '        ' '    ' '  'BSIS-BUDAT' v_data,
*      ' '        ' '    ' '  'UF05A-STGRD' '02'.
*
*      opt-dismode = 'E'.
*      CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.
      "185994 - anulação FB08

*      IF sy-subrc IS INITIAL.
      " MOVE '@08@'     TO <saida>-status.

      IF lt_zibcont IS NOT INITIAL OR
         lt_zibcont IS NOT INITIAL.

        READ TABLE lt_zibcont INTO DATA(ls_zibcont) INDEX 1.
        READ TABLE lt_zibcont2 INTO DATA(ls_zibcont2) INDEX 1.

        SELECT SINGLE belnr
           FROM zib_contabil_chv
           INTO @DATA(lv_doc_estorno)
           WHERE bukrs IN @s_empre
           AND   obj_key EQ @ls_zibcont-obj_key.

        SELECT SINGLE belnr
         FROM zib_contabil_chv
         INTO @DATA(lv_doc_estorno2)
         WHERE bukrs IN @s_empre
         AND   obj_key EQ @ls_zibcont2-obj_key.

      ENDIF.

*        SELECT SINGLE stblg
*          FROM bkpf
*          INTO @DATA(v_stblg)
*          WHERE bukrs = @s_empre-low AND
*          belnr = @<saida>-doc_contabil.

*      IF lv_doc_estorno IS NOT INITIAL OR
*         lv_doc_estorno2 IS NOT INITIAL.
      LOOP AT  t_alv_saida ASSIGNING <saida1> WHERE tp_imposto = <saida>-tp_imposto.

        MOVE lv_doc_estorno TO <saida1>-doc_estorno.
        MOVE lv_doc_estorno2 TO <saida1>-doc_estorno_2.
        MOVE icon_led_yellow TO <saida1>-status.


        UPDATE zglt0106 SET obj_key_rev = ls_zibcont-obj_key
                           obj_key_rev2 = ls_zibcont2-obj_key
        WHERE bukrs = s_empre-low AND
              mes   = s_mes-low AND
              ano   = s_ano-low AND
              tipo_imposto = <saida>-tp_imposto.

      ENDLOOP.



*      ENDIF.

*      ENDIF.
      "185994 - CS2025000543 - IMPOSTO DIFERIDO REVERSÃO - RGA - fim

      v_tipo_imposto = <saida>-tp_imposto.

    ENDIF.
  ENDLOOP.

  CALL METHOD g_grid->refresh_table_display.
ENDFORM.

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZGLR072'.
  SET TITLEBAR  'ZGLR072'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  FREE: t_rows[].

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

**  Begin of #96800 FF   01.12.2022 15:16:17
  IF t_rows[] IS INITIAL.
    MESSAGE s068(poc_main) DISPLAY LIKE 'E'.
  ENDIF.
** End of FF  01.12.2022 15:16:17

  CASE ok_code.
    WHEN '&F03' OR '&F15' OR '&F15'.
      LEAVE TO SCREEN 0.

    WHEN 'GER_CONT'.
      PERFORM gera_contabilizacao.

    WHEN 'ESTORNAR'.
      PERFORM estornar_contabilizacao.
    WHEN 'ATUALIZAR'.
      PERFORM f_selecao_dados.
** BUG - 152228 - Inicio - CBRAND
*PERFORM refresh_doc_contabil.
      PERFORM refresh_doc_contabil.
** BUG - 152228 - Inicio - CBRAND

      CALL METHOD g_grid->refresh_table_display.
    WHEN 'EST_SUBSEQ'.
      PERFORM estorno_subsequente.
    WHEN 'SAVE '.
      PERFORM f_salvar.
  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

**********************************************************************
**********************************************************************

FORM f_editar_linha. "CHANGING p_it_celltab TYPE lvc_t_styl.
  DATA: vl_cont        TYPE i,
        wa_celltab     TYPE lvc_s_styl,
        status         TYPE raw4,
        status_inativo TYPE raw4,
        it_celltab     TYPE lvc_t_styl.



  LOOP AT t_alv_saida INTO w_alv_saida.
    CLEAR w_alv_saida-cellstyles.
    vl_cont = sy-tabix.
    IF w_alv_saida-conta(1) EQ 'M' AND ( w_alv_saida-doc_lcto IS INITIAL OR w_alv_saida-doc_estorno IS NOT INITIAL OR w_alv_saida-doc_estorno_2 IS NOT INITIAL ).
      status = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      status = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    it_celltab = VALUE #(
                             ( fieldname = 'SALDO_BRL'              style = status )
                             ( fieldname = 'SALDO_USD'              style =  status )
                             ( fieldname = 'SALDO_ATU_BRL'          style = status )
                             ( fieldname = 'SALDO_ATU_USD'          style = status )
*                             ( fieldname = 'movi_brl'               style = status )
*                             ( fieldname = 'movi_usd'               style = status )
*                             ( fieldname = 'irpj_brl'               style = status )
*                             ( fieldname = 'irpj_usd'               style = status )
*                             ( fieldname = 'csll_brl'               style = status )
*                             ( fieldname = 'csll_usd'               style = status )
                             ).

    INSERT LINES OF it_celltab INTO TABLE w_alv_saida-cellstyles.
    MODIFY t_alv_saida FROM w_alv_saida INDEX vl_cont.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SALVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_salvar .

  LOOP AT t_alv_saida INTO w_alv_saida.
    IF w_alv_saida-conta(1) EQ 'M' AND  ( w_alv_saida-doc_contabil IS INITIAL OR w_alv_saida-doc_estorno IS NOT INITIAL ).

      SELECT SINGLE *
        FROM zglt0106
        INTO @DATA(w_zglt0106)
        WHERE bukrs = @s_empre-low AND
              mes   = @s_mes-low AND
              ano   = @s_ano-low AND
              tipo_imposto = @w_alv_saida-tp_imposto AND
              conta    = @w_alv_saida-conta.

      IF sy-subrc IS INITIAL.

        UPDATE zglt0106
        SET saldoanual_brl    = w_alv_saida-saldo_brl
            saldoanual_usd    = w_alv_saida-saldo_usd
            saldoatual_brl    = w_alv_saida-saldo_atu_brl
            saldoatual_usd    = w_alv_saida-saldo_atu_usd
            movimentacao_brl  = w_alv_saida-movi_brl
            movimentacao_usd  = w_alv_saida-movi_usd
            irpj_brl          = w_alv_saida-irpj_brl
            irpj_usd          = w_alv_saida-irpj_usd
            csll_brl          = w_alv_saida-csll_brl
            csll_usd          = w_alv_saida-csll_usd
*** BUG - 153691 - Inicio - CBRAND
            m_irpj_brl = w_alv_saida-m_irpj_brl
            m_irpj_usd = w_alv_saida-m_irpj_usd
            m_csll_brl = w_alv_saida-m_csll_brl
            m_csll_usd = w_alv_saida-m_csll_usd
            s_irpj_brl = w_alv_saida-s_irpj_brl
            s_irpj_usd = w_alv_saida-s_irpj_usd
            s_csll_brl = w_alv_saida-s_csll_brl
            s_csll_usd = w_alv_saida-s_csll_usd
*** BUG - 153691 - Fim - CBRAND

       WHERE  bukrs        = s_empre-low AND
              mes          = s_mes-low AND
              ano          = s_ano-low AND
              tipo_imposto = w_alv_saida-tp_imposto AND
              conta        = w_alv_saida-conta.

      ELSE.

        CLEAR: wl_zglt0106. "BUG - 152228 - CBRAND
        wl_zglt0106-bukrs = s_empre-low.
        wl_zglt0106-mes   = s_mes-low.
        wl_zglt0106-ano   =  s_ano-low.
        wl_zglt0106-tipo_imposto = w_alv_saida-tp_imposto.
        wl_zglt0106-conta        = w_alv_saida-conta.
        wl_zglt0106-saldoanual_brl = w_alv_saida-saldo_brl .
        wl_zglt0106-saldoanual_usd = w_alv_saida-saldo_usd.
        wl_zglt0106-movimentacao_brl = w_alv_saida-movi_brl.
        wl_zglt0106-movimentacao_usd = w_alv_saida-movi_usd.
        wl_zglt0106-saldoatual_brl = w_alv_saida-saldo_atu_brl.
        wl_zglt0106-saldoatual_usd = w_alv_saida-saldo_atu_usd.
        wl_zglt0106-irpj_brl = w_alv_saida-irpj_brl.
        wl_zglt0106-irpj_usd = w_alv_saida-irpj_usd.
        wl_zglt0106-csll_brl = w_alv_saida-csll_brl.
        wl_zglt0106-csll_usd = w_alv_saida-csll_usd.

        wl_zglt0106-m_irpj_brl = w_alv_saida-m_irpj_brl.
        wl_zglt0106-m_irpj_usd = w_alv_saida-m_irpj_usd.
        wl_zglt0106-m_csll_brl = w_alv_saida-m_csll_brl.
        wl_zglt0106-m_csll_usd = w_alv_saida-m_csll_usd.

*** BUG - 153691 - Inicio - CBRAND
        wl_zglt0106-s_irpj_brl = w_alv_saida-s_irpj_brl.
        wl_zglt0106-s_irpj_usd = w_alv_saida-s_irpj_usd.
        wl_zglt0106-s_csll_brl = w_alv_saida-s_csll_brl.
        wl_zglt0106-s_csll_usd = w_alv_saida-s_csll_usd.
*** BUG - 153691 - Fim - CBRAND

        wl_zglt0106-usuario = sy-uname.
        wl_zglt0106-data =  sy-datum.
        wl_zglt0106-hora =  sy-uzeit.

        MODIFY zglt0106 FROM wl_zglt0106.
      ENDIF.

    ENDIF.
  ENDLOOP.



ENDFORM.



FORM f_taxa_usd USING    v_data TYPE any
            CHANGING      taxa  TYPE tcurr-ukurs.

  RANGES: r_gdatu FOR tcurr-gdatu,
         r_fcurr FOR tcurr-fcurr.

  "IF cont IS NOT INITIAL.
  "CONCATENATE '01.' cont '.' s_ano-low INTO v_data.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = v_data
    IMPORTING
      output = r_gdatu-low.

  APPEND r_gdatu.


  SELECT kurst fcurr tcurr gdatu ukurs
     FROM tcurr
     INTO TABLE t_tcurr
    WHERE kurst = 'B'
          AND fcurr = 'BRL'
          AND tcurr = 'USD'
          AND gdatu = r_gdatu-low.

  IF sy-subrc = 0.
*Como o campo é de data invertida para pegar oa mais atual a data GDATU tem que ser ordenada do menor pro maior
    SORT t_tcurr BY gdatu ASCENDING fcurr ASCENDING .

    READ TABLE t_tcurr INTO wa_tcurr WITH KEY fcurr = 'BRL'
                                              tcurr = 'USD' BINARY SEARCH.
    IF sy-subrc = 0.
      taxa = wa_tcurr-ukurs.
    ENDIF.
  ENDIF.
  "ENDIF.

ENDFORM.



FORM f_tp_taxa USING tp_imposto TYPE z_tp_imposto
  CHANGING tp_taxa TYPE z_tp_taxa.

  SELECT SINGLE tp_taxa FROM zglt0100 INTO tp_taxa WHERE tp_imposto = tp_imposto.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_tree .
  DATA: lr_settings       TYPE REF TO cl_salv_tree_settings,
        lv_title          TYPE salv_de_tree_text,
        lt_saida          TYPE TABLE OF zgle_alv_tree,
        lt_totais         TYPE TABLE OF ty_alv_saida_total,
        lt_totais_old     TYPE TABLE OF ty_alv_saida_total,
        ls_totais         TYPE ty_alv_saida_total,
        ls_saida          TYPE zgle_alv_tree,
        ls_saida_filho    TYPE zgle_alv_tree,
        lo_header         TYPE REF TO cl_salv_form_layout_grid,
        lo_h_label        TYPE REF TO cl_salv_form_label,
        lo_h_flow         TYPE REF TO cl_salv_form_layout_flow,
        p_text            TYPE sdydo_text_element,
        filtros	          TYPE zif_screen_linha_filtro,
        i_filtros	        TYPE zif_screen_linha_filtro_t,
        v_valor(60),
        lv_texto          TYPE string,
        lv_irpj           TYPE string,
        lv_csll           TYPE string,
        lva_imposto       TYPE zglt0101-tp_imposto,
        lva_saldo_brl     TYPE zde_vlr15_02,
        lva_saldo_usd     TYPE zde_vlr15_02,
        lva_s_irpj_brl    TYPE zde_vlr15_02,
        lva_s_irpj_usd    TYPE zde_vlr15_02,
        lva_s_csll_brl    TYPE zde_vlr15_02,
        lva_s_csll_usd    TYPE zde_vlr15_02,
        lva_movi_brl      TYPE zde_vlr15_02,
        lva_movi_usd      TYPE zde_vlr15_02,
        lva_m_irpj_brl    TYPE zde_vlr15_02,
        lva_m_irpj_usd    TYPE zde_vlr15_02,
        lva_m_csll_brl    TYPE zde_vlr15_02,
        lva_m_csll_usd    TYPE zde_vlr15_02,
        lva_saldo_atu_brl TYPE zde_vlr15_02,
        lva_saldo_atu_usd TYPE zde_vlr15_02.

  TRY.
      cl_salv_tree=>factory(
        IMPORTING
          r_salv_tree = gr_tree
        CHANGING
          t_table     = lt_saida ).
    CATCH cx_salv_no_new_data_allowed cx_salv_error.
      EXIT.
  ENDTRY.

  CREATE OBJECT lo_header.

  CONCATENATE s_empre-low '-' v_desc_emp INTO v_valor SEPARATED BY space.
  CONCATENATE 'Empresa:' v_valor INTO lv_texto SEPARATED BY space.

*   Writing Header texts
  lo_h_flow = lo_header->create_flow( row = 1 column = 1 ).
  lo_h_flow->create_text( text = lv_texto ).

  CLEAR lv_texto.
  CONCATENATE 'Mês:' s_mes-low INTO lv_texto SEPARATED BY space.

  lo_h_flow = lo_header->create_flow( row = 2 column = 1 ).
  lo_h_flow->create_text( text = lv_texto ).

  CLEAR lv_texto.
  CONCATENATE 'Ano:' s_ano-low INTO lv_texto SEPARATED BY space.
  lo_h_flow = lo_header->create_flow( row = 3 column = 1 ).
  lo_h_flow->create_text( text = lv_texto ).

  lv_irpj = v_irpj.
  CONDENSE lv_irpj NO-GAPS.

  CLEAR lv_texto.
  CONCATENATE 'Taxa do IRPJ:' lv_irpj INTO lv_texto SEPARATED BY space.
  lo_h_flow = lo_header->create_flow( row = 4 column = 1 ).
  lo_h_flow->create_text( text = lv_texto ).


  lv_csll = v_csll.
  CONDENSE lv_csll NO-GAPS.
  CLEAR lv_texto.
  CONCATENATE 'Taxa do CSLL:' lv_csll INTO lv_texto SEPARATED BY space.
  lo_h_flow = lo_header->create_flow( row = 5 column = 1 ).
  lo_h_flow->create_text( text = lv_texto ).

*   Set the top of list
  gr_tree->set_top_of_list( lo_header ).

* build the hierarchy header

  lr_settings = gr_tree->get_tree_settings( ).
  lr_settings->set_hierarchy_header( TEXT-hd1 ).
  lr_settings->set_hierarchy_tooltip( TEXT-hd1 ).
  lr_settings->set_hierarchy_size( 70 ).


  lv_title = sy-title.
  lr_settings->set_header( lv_title ).

  DATA: ls_data TYPE alv_t_t2.
  DATA: lv_key       TYPE lvc_nkey,
        l_connid_key TYPE lvc_nkey,
        l_last_key   TYPE lvc_nkey,
        lv_text      TYPE lvc_value.

  DATA: lr_nodes TYPE REF TO cl_salv_nodes,
        lr_node  TYPE REF TO cl_salv_node,
        lr_item  TYPE REF TO cl_salv_item.

  LOOP AT t_alv_saida ASSIGNING FIELD-SYMBOL(<fs_alv_saida>).
*** BUG - 153691 - Inicio - CBRAND
    DATA(lva_tabix) = sy-tabix.
*    IF ( s_empre-low NE '0001' ) AND <fs_alv_saida>-tp_imposto EQ '0015' OR <fs_alv_saida>-tp_imposto EQ '0016'."DEVK9A209H - 16/05/24 - #138696 RSA
*      MOVE-CORRESPONDING <fs_alv_saida> TO ls_totais.
*      APPEND ls_totais TO lt_totais.
*    ELSE.
*      MOVE-CORRESPONDING <fs_alv_saida> TO ls_totais.
*      COLLECT ls_totais INTO lt_totais.
*    ENDIF.

*** BUG - 153691 - Fim - CBRAND

    IF <fs_alv_saida>-conta+0(1) = 'M'.
      MOVE-CORRESPONDING <fs_alv_saida> TO ls_totais.

      IF lva_saldo_brl     = ls_totais-saldo_brl.
        CLEAR: ls_totais-saldo_brl.
      ENDIF.
      IF lva_saldo_usd     = ls_totais-saldo_usd.
        CLEAR: ls_totais-saldo_usd.
      ENDIF.

      IF lva_s_irpj_brl    = ls_totais-s_irpj_brl.
        CLEAR:ls_totais-s_irpj_brl.
      ENDIF.
      IF lva_s_irpj_usd    = ls_totais-s_irpj_brl.
        CLEAR:ls_totais-s_irpj_brl.
      ENDIF.
      IF lva_s_csll_brl    = ls_totais-s_csll_brl.
        CLEAR: ls_totais-s_csll_brl.
      ENDIF.
      IF lva_s_csll_usd    = ls_totais-s_csll_usd.
        CLEAR: ls_totais-s_csll_usd.
      ENDIF.
      IF lva_movi_brl      = ls_totais-movi_brl.
        CLEAR: ls_totais-movi_brl.
      ENDIF.
      IF lva_movi_usd      = ls_totais-movi_usd.
        CLEAR: ls_totais-movi_usd.
      ENDIF.
      IF lva_m_irpj_brl    = ls_totais-m_irpj_brl.
        CLEAR: ls_totais-m_irpj_brl.
      ENDIF.
      IF lva_m_irpj_usd    = ls_totais-m_irpj_usd.
        CLEAR: ls_totais-m_irpj_usd.
      ENDIF.
      IF lva_m_csll_brl    = ls_totais-m_csll_brl.
        CLEAR: ls_totais-m_csll_brl.
      ENDIF.
      IF lva_m_csll_usd    = ls_totais-m_csll_usd.
        CLEAR: ls_totais-m_csll_usd.
      ENDIF.
      IF lva_saldo_atu_brl = ls_totais-saldo_atu_brl.
        CLEAR: ls_totais-saldo_atu_brl.
      ENDIF.
      IF lva_saldo_atu_usd = ls_totais-saldo_atu_usd.
        CLEAR: ls_totais-saldo_atu_usd.
      ENDIF.

      IF <fs_alv_saida>-tp_imposto <> lva_imposto.
        lva_imposto       =  <fs_alv_saida>-tp_imposto.
        lva_saldo_brl     = ls_totais-saldo_brl.
        lva_saldo_usd     = ls_totais-saldo_usd.
        lva_s_irpj_brl    = ls_totais-s_irpj_brl.
        lva_s_irpj_usd    = ls_totais-s_irpj_usd.
        lva_s_csll_brl    = ls_totais-s_csll_brl.
        lva_s_csll_usd    = ls_totais-s_csll_usd.
        lva_movi_brl      = ls_totais-movi_brl.
        lva_movi_usd      = ls_totais-movi_usd.
        lva_m_irpj_brl    = ls_totais-m_irpj_brl.
        lva_m_irpj_usd    = ls_totais-m_irpj_usd.
        lva_m_csll_brl    = ls_totais-m_csll_brl.
        lva_m_csll_usd    = ls_totais-m_csll_usd.
        lva_saldo_atu_brl = ls_totais-saldo_atu_brl.
        lva_saldo_atu_usd = ls_totais-saldo_atu_usd.
      ENDIF.

      COLLECT ls_totais INTO lt_totais.

    ELSE.
      MOVE-CORRESPONDING <fs_alv_saida> TO ls_totais.
      COLLECT ls_totais INTO lt_totais.
    ENDIF.

*** BUG - 153691 - Fim - CBRAND

  ENDLOOP.

  SORT lt_totais BY tp_imposto.

  LOOP AT t_alv_saida ASSIGNING <fs_alv_saida>.
    ON CHANGE OF <fs_alv_saida>-tp_imposto.
*... §0 working with nodes
      lr_nodes = gr_tree->get_nodes( ).
      lv_text = <fs_alv_saida>-desc_imposto.

      TRY.
          READ TABLE lt_totais ASSIGNING FIELD-SYMBOL(<fs_totais>)
          WITH KEY tp_imposto = <fs_alv_saida>-tp_imposto
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE-CORRESPONDING <fs_totais> TO ls_saida.
          ENDIF.

          lr_node = lr_nodes->add_node( related_node = ''
                                        data_row     = ls_saida
                                        relationship = cl_gui_column_tree=>relat_last_child
                                        text         = lv_text ).

          lv_key = lr_node->get_key( ).
        CATCH cx_salv_msg.
      ENDTRY.

    ENDON.

    CLEAR: lr_nodes,
           lr_node.

    lr_nodes = gr_tree->get_nodes( ).

    MOVE-CORRESPONDING <fs_alv_saida> TO ls_saida_filho.

    TRY.
        lr_node = lr_nodes->add_node( related_node = lv_key
                                      data_row     = ls_saida_filho
                                      relationship = cl_gui_column_tree=>relat_last_child
                                      text         = lv_text ).

*    lv_key = node->get_key( ).
      CATCH cx_salv_msg.
    ENDTRY.

  ENDLOOP.

  TRY.
      gr_tree->set_screen_status(
        pfstatus      = 'STANDARD'
        report        = sy-repid
        set_functions = gr_tree->c_functions_all ).
    CATCH cx_root.                                      "#EC NO_HANDLER
  ENDTRY.
*... set the columns technical
  DATA: lr_columns TYPE REF TO cl_salv_columns_tree,
        lr_column  TYPE REF TO cl_salv_column.

  lr_columns = gr_tree->get_columns( ).
  lr_columns->set_optimize( ).

  TRY.
      lr_column = lr_columns->get_column( 'STATUS' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column = lr_columns->get_column( 'TP_IMPOSTO' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( 'DESC_IMPOSTO' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( 'DESC_CONTA' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( 'SALDO_BRL' ).
      lr_column->set_fixed_header_text( 'L' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( 'SALDO_USD' ).
      lr_column->set_fixed_header_text( 'L' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( 'MOVI_BRL' ).
      lr_column->set_fixed_header_text( 'L' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( 'MOVI_USD' ).
      lr_column->set_fixed_header_text( 'L' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( 'SALDO_ATU_BRL' ).
      lr_column->set_fixed_header_text( 'L' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( 'SALDO_ATU_USD' ).
      lr_column->set_fixed_header_text( 'L' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  gr_tree->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_CONCILIACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_conciliacao .

  TYPES: BEGIN OF ty_alv.
           INCLUDE STRUCTURE zgle_alv__conci.
  TYPES color TYPE lvc_t_scol.
  TYPES END OF ty_alv.

  DATA: lr_alv              TYPE REF TO cl_salv_table,
        lt_saida            TYPE TABLE OF ty_alv,
        lt_totais_ir        TYPE TABLE OF zgle_alv__conci,
        lt_totais_cs        TYPE TABLE OF zgle_alv__conci,
        ls_totais           TYPE zgle_alv__conci,
        lv_spacos           TYPE char15,
        lt_contas           TYPE TABLE OF zlc_emp_contas,
        lt_contas_pas       TYPE TABLE OF zlc_emp_contas,
        lt_contas_resultado TYPE TABLE OF zlc_emp_contas,
        lt_saldos           TYPE TABLE OF zde_fi_gl_saldo_faglflext,
        lt_saldos_usd       TYPE TABLE OF zde_fi_gl_saldo_faglflext,
        lt_i011z            TYPE TABLE OF rf011z,
        lt_param            TYPE TABLE OF zscre000,
        lt_plano            TYPE TABLE OF zscreplano,
        lt_totais           TYPE TABLE OF zgle_alv__conci,
        lv_total_brl        TYPE zgle_alv__conci-saldo_brl,
        lv_total_usd        TYPE zgle_alv__conci-saldo_usd,
        lv_index            TYPE numc2,
        lv_brl_atual        TYPE zgle_alv__conci-saldo_brl,
        lv_usd_atual        TYPE zgle_alv__conci-saldo_brl,
        lv_mes              TYPE string,
        lv_resul_brl        TYPE zgle_alv__conci-movi_brl,
        lv_resul_usd        TYPE zgle_alv__conci-movi_brl,
        lt_fieldcat         TYPE slis_t_fieldcat_alv,
        ls_color            TYPE lvc_s_scol,
        lt_color            TYPE TABLE OF lvc_s_scol,
        lv_cont             TYPE sy-tabix,
        lv_tabix            TYPE sy-tabix,
        lr_descr            TYPE RANGE OF zscreplano-descr.

*... set the columns technical
  DATA: lr_columns      TYPE REF TO cl_salv_columns,
        lr_column       TYPE REF TO cl_salv_column,
        lo_columns      TYPE REF TO cl_salv_columns_table,
        lo_column_price TYPE REF TO cl_salv_column_table.

  FIELD-SYMBOLS: <fs_valor> TYPE hslxx12.

  DATA: rg_saknr TYPE RANGE OF zlc_emp_contas-saknr,
        wa_saknr LIKE LINE  OF rg_saknr.

  CONSTANTS: c_conta_ativo(23)   TYPE c VALUE 'ZSD_ZGLR072_CONTA_ATIVO',
             c_conta_passivo(25) TYPE c VALUE 'ZSD_ZGLR072_CONTA_PASSIVO'.

  SELECT *
         FROM zglt0110
         INTO TABLE @DATA(lt_zglt0110).
  IF sy-subrc IS INITIAL.
    SORT lt_zglt0110 BY tp_imposto conta.
  ENDIF.

  SELECT *
    FROM zglt0103
    INTO TABLE @DATA(lt_zglt0103).
  IF sy-subrc IS INITIAL.
    SORT lt_zglt0103 BY taxa.
  ENDIF.

  LOOP AT t_alv_saida ASSIGNING FIELD-SYMBOL(<fs_alv_saida>).

    READ TABLE lt_zglt0110 ASSIGNING FIELD-SYMBOL(<fs_zglt0110>)
    WITH KEY tp_imposto = <fs_alv_saida>-tp_imposto
             conta      = <fs_alv_saida>-conta
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      IF <fs_zglt0110>-irpj IS NOT INITIAL.

        MOVE-CORRESPONDING <fs_alv_saida> TO ls_totais.
        ls_totais-descricao = 'Base de Cálculo para IR diferido'.
        COLLECT ls_totais INTO lt_totais_ir.
        CONTINUE.

      ELSEIF <fs_zglt0110>-csll IS NOT INITIAL.

        MOVE-CORRESPONDING <fs_alv_saida> TO ls_totais.
        ls_totais-descricao = 'Base de Cálculo para CS diferido'.
        COLLECT ls_totais INTO lt_totais_cs.
        CONTINUE.

      ENDIF.

    ENDIF.

    MOVE-CORRESPONDING <fs_alv_saida> TO ls_totais.
    ls_totais-descricao = 'Base de Cálculo para IR diferido'.
    COLLECT ls_totais INTO lt_totais_ir.

    CLEAR ls_totais.

    MOVE-CORRESPONDING <fs_alv_saida> TO ls_totais.
    ls_totais-descricao = 'Base de Cálculo para CS diferido'.
    COLLECT ls_totais INTO lt_totais_cs.

  ENDLOOP.

  READ TABLE lt_totais_ir ASSIGNING FIELD-SYMBOL(<fs_totais_ir>) INDEX 1.
  IF sy-subrc IS INITIAL.
    APPEND INITIAL LINE TO lt_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
    MOVE-CORRESPONDING <fs_totais_ir> TO <fs_saida>.
  ENDIF.

  READ TABLE lt_totais_cs ASSIGNING FIELD-SYMBOL(<fs_totais_cs>) INDEX 1.
  IF sy-subrc IS INITIAL.
    APPEND INITIAL LINE TO lt_saida ASSIGNING <fs_saida>.
    MOVE-CORRESPONDING <fs_totais_cs> TO <fs_saida>.
  ENDIF.

  READ TABLE lt_totais_ir ASSIGNING <fs_totais_ir> INDEX 1.
  IF sy-subrc IS INITIAL.
    READ TABLE lt_zglt0103 ASSIGNING FIELD-SYMBOL(<fs_zglt0103>)
    WITH KEY taxa = '1'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_totais_ir>-saldo_brl = <fs_totais_ir>-saldo_brl * ( <fs_zglt0103>-percentual / 100 ).
      <fs_totais_ir>-saldo_usd = <fs_totais_ir>-saldo_usd * ( <fs_zglt0103>-percentual / 100 ).
      <fs_totais_ir>-movi_brl  = <fs_totais_ir>-movi_brl * ( <fs_zglt0103>-percentual / 100 ).
      <fs_totais_ir>-movi_usd  = <fs_totais_ir>-movi_usd * ( <fs_zglt0103>-percentual / 100 ).
      <fs_totais_ir>-saldo_atu_brl = <fs_totais_ir>-saldo_atu_brl * ( <fs_zglt0103>-percentual / 100 ).
      <fs_totais_ir>-saldo_atu_usd = <fs_totais_ir>-saldo_atu_usd * ( <fs_zglt0103>-percentual / 100 ).

      lv_spacos = <fs_zglt0103>-percentual.
      SHIFT lv_spacos RIGHT DELETING TRAILING ''.
      <fs_totais_ir>-descricao = '(-) Imposto de Renda' && lv_spacos && '%'.
    ENDIF.

    APPEND INITIAL LINE TO lt_saida ASSIGNING <fs_saida>.
    MOVE-CORRESPONDING <fs_totais_ir> TO <fs_saida>.

  ENDIF.

  READ TABLE lt_totais_cs ASSIGNING <fs_totais_cs> INDEX 1.
  IF sy-subrc IS INITIAL.
    READ TABLE lt_zglt0103 ASSIGNING <fs_zglt0103>
    WITH KEY taxa = '2'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_totais_cs>-saldo_brl     = <fs_totais_cs>-saldo_brl * ( <fs_zglt0103>-percentual / 100 ).
      <fs_totais_cs>-saldo_usd     = <fs_totais_cs>-saldo_usd * ( <fs_zglt0103>-percentual / 100 ).
      <fs_totais_cs>-movi_brl      = <fs_totais_cs>-movi_brl * ( <fs_zglt0103>-percentual / 100 ).
      <fs_totais_cs>-movi_usd      = <fs_totais_cs>-movi_usd * ( <fs_zglt0103>-percentual / 100 ).
      <fs_totais_cs>-saldo_atu_brl = <fs_totais_cs>-saldo_atu_brl * ( <fs_zglt0103>-percentual / 100 ).
      <fs_totais_cs>-saldo_atu_usd = <fs_totais_cs>-saldo_atu_usd * ( <fs_zglt0103>-percentual / 100 ).

      lv_spacos = <fs_zglt0103>-percentual.
      SHIFT lv_spacos RIGHT DELETING TRAILING ''.

      <fs_totais_cs>-descricao = '(-) Contribuição Sindical' && lv_spacos && '%'.
    ENDIF.

    APPEND INITIAL LINE TO lt_saida ASSIGNING <fs_saida>.
    MOVE-CORRESPONDING <fs_totais_cs> TO <fs_saida>.

    APPEND INITIAL LINE TO lt_saida ASSIGNING <fs_saida>.

    FREE lt_color.

    APPEND INITIAL LINE TO lt_color ASSIGNING FIELD-SYMBOL(<fs_color>).

    <fs_color>-color-col = 3.
    <fs_color>-color-col = 3.
    <fs_color>-color-col = 3.

    <fs_saida>-color = lt_color.
    <fs_saida>-descricao     = 'Total da apuração dos impostos diferidos'.
    <fs_saida>-saldo_brl     = <fs_totais_cs>-saldo_brl      + <fs_totais_ir>-saldo_brl .
    <fs_saida>-saldo_usd     = <fs_totais_cs>-saldo_usd      + <fs_totais_ir>-saldo_usd .
    <fs_saida>-movi_brl      = <fs_totais_cs>-movi_brl       + <fs_totais_ir>-movi_brl  .
    <fs_saida>-movi_usd      = <fs_totais_cs>-movi_usd       + <fs_totais_ir>-movi_usd  .
    <fs_saida>-saldo_atu_brl = <fs_totais_cs>-saldo_atu_brl  + <fs_totais_ir>-saldo_atu_brl.
    <fs_saida>-saldo_atu_usd = <fs_totais_cs>-saldo_atu_usd  + <fs_totais_ir>-saldo_atu_usd.

  ENDIF.

  APPEND INITIAL LINE TO lt_param ASSIGNING FIELD-SYMBOL(<fs_param>).

  <fs_param>-nome_cl_param = 'ZSCRRFC'.
  <fs_param>-atrib_param   = 'EMPRESA'.
  <fs_param>-low           = s_empre-low.


  APPEND INITIAL LINE TO lt_param ASSIGNING <fs_param>.

  <fs_param>-nome_cl_param = 'ZSCRRFC'.
  <fs_param>-atrib_param   = 'VERSN'.
  <fs_param>-low           = '0010'.


  APPEND INITIAL LINE TO lt_param ASSIGNING <fs_param>.

  <fs_param>-nome_cl_param = 'ZSCRRFC'.
  <fs_param>-atrib_param   = 'IDIOMA'.
  <fs_param>-low           = 'PT'.

  CALL FUNCTION 'Z_SCR_PLANO_CONTAS'
    TABLES
      i011z                  = lt_i011z
      is_param               = lt_param
      zscreplano             = lt_plano
    EXCEPTIONS
      estrutura_bal_idioma   = 1
      sem_versn              = 2
      sem_empresa            = 3
      sem_permissao_execucao = 4
      OTHERS                 = 5.
  IF sy-subrc = 0.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'PROVISÃO P/IMPOSTO DE RENDA') TO lr_descr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'PROVISÃO P/CONTRIBUIÇÃO SOCIAL') TO lr_descr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '(-) IMPAIRMENT S/ IMPOSTO DIFERIDO') TO lr_descr.

    READ TABLE lt_plano TRANSPORTING NO FIELDS
    WITH KEY descr = 'PROVISÃO PARA IR E CS DIFERIDOS'.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_plano ASSIGNING FIELD-SYMBOL(<fs_plano>) FROM sy-tabix.
        ADD 1 TO lv_cont.
        lv_tabix = sy-tabix.
        IF lv_cont EQ 1.
          CONTINUE.
        ELSEIF <fs_plano>-indsa = 'S' AND lt_contas IS NOT INITIAL AND <fs_plano>-descr NOT IN lr_descr.
          EXIT.
        ELSE.
          APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_contas>).
          <fs_contas>-bukrs = s_empre-low.
          <fs_contas>-saknr = <fs_plano>-cod_sa.

        ENDIF.
      ENDLOOP.
    ENDIF.

    DELETE lt_plano TO lv_tabix.

    CLEAR lv_cont.
    READ TABLE lt_plano TRANSPORTING NO FIELDS
    WITH KEY descr = 'PROVISÃO PARA IR E CS DIFERIDOS'.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_plano ASSIGNING <fs_plano> FROM sy-tabix.
        ADD 1 TO lv_cont.
        IF lv_cont EQ 1.
          CONTINUE.
        ELSEIF <fs_plano>-indsa = 'S' AND lt_contas_pas IS NOT INITIAL AND <fs_plano>-descr NOT IN lr_descr..
          EXIT.
        ELSE.
          APPEND INITIAL LINE TO lt_contas_pas ASSIGNING <fs_contas>.
          <fs_contas>-bukrs = s_empre-low.
          <fs_contas>-saknr = <fs_plano>-cod_sa.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT lt_contas.
    DELETE ADJACENT DUPLICATES FROM lt_contas COMPARING ALL FIELDS.

    SORT lt_contas_pas.
    DELETE ADJACENT DUPLICATES FROM lt_contas_pas COMPARING ALL FIELDS.


    "DEVK9A209H - 15/05/24 - FI - Ajustes na transação ZGL076-Imposto Diferido - #138696 RSA
    CLEAR: lr_descr[], lt_contas_resultado[].


    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CSLL DIFERIDO')                              TO lr_descr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'IRPJ DIFERIDO')                              TO lr_descr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'PROVISOES P/ IRPJ E CSLL CORRENTES')         TO lr_descr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'PROVISOES P/ IRPJ E CSLL DIFERIDOS')         TO lr_descr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'PROVISÃO IRPJ E CSLL - INVEST. NO EXTERIOR') TO lr_descr.

    CLEAR lv_cont.
    READ TABLE lt_plano TRANSPORTING NO FIELDS
*&---------------------------------Bug Solto 147433 / AOENNING / &*
*    with key descr = 'PROVISOES P/ IMPOSTO DE RENDA E CONTR. SOCIAL'.
     WITH KEY descr = 'PROVISOES P/ IRPJ E CSLL DIFERIDOS'.
*&---------------------------------Bug Solto 147433 / AOENNING / &*

    IF sy-subrc IS INITIAL.
      LOOP AT lt_plano ASSIGNING <fs_plano> FROM sy-tabix.
        ADD 1 TO lv_cont.
        IF lv_cont EQ 1.
          CONTINUE.
        ELSEIF <fs_plano>-indsa = 'S' AND lt_contas_resultado IS NOT INITIAL AND <fs_plano>-descr NOT IN lr_descr.
          EXIT.
        ELSE.
          APPEND INITIAL LINE TO lt_contas_resultado ASSIGNING <fs_contas>.
          <fs_contas>-bukrs = s_empre-low.
          <fs_contas>-saknr = <fs_plano>-cod_sa.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT lt_contas_resultado.
    DELETE ADJACENT DUPLICATES FROM lt_contas_resultado COMPARING ALL FIELDS.
    "DEVK9A209H - 15/05/24 - FI - Ajustes na transação ZGL076-Imposto Diferido - #138696 RSA

  ENDIF.

  "DEVK9A209H - 15/05/24 - FI - Ajustes na transação ZGL076-Imposto Diferido - #138696 RSA
  CLEAR rg_saknr[].
  SELECT low
         FROM tvarvc
         INTO TABLE @DATA(lt_conta_ativo)
         WHERE name EQ @c_conta_ativo.
  LOOP AT lt_conta_ativo INTO DATA(wa_conta_ativo).
    wa_saknr-sign   = 'I'.
    wa_saknr-option = 'EQ'.
    wa_saknr-low    = wa_conta_ativo-low.
    APPEND wa_saknr TO rg_saknr.
  ENDLOOP.

  IF NOT lt_contas[] IS INITIAL AND NOT rg_saknr[] IS INITIAL.
    DELETE lt_contas WHERE saknr IN rg_saknr.
  ENDIF.
  "DEVK9A209H - 15/05/24 - FI - Ajustes na transação ZGL076-Imposto Diferido - #138696 RSA

  IF s_mes-low EQ '12'."DEVK9A209H - 15/05/24 - #138696 RSA
    s_mes-low = '15'.
  ENDIF.

*** Cálculo ativos - Inicio
  CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
    EXPORTING
      ryear                = s_ano-low
      contas               = lt_contas
      p_gerar_todas        = abap_true
      p_gerar_soc_parceira = abap_true
      rldnr                = '0L'
    TABLES
      it_saldos            = lt_saldos
      it_saldos_2          = lt_saldos_usd
    EXCEPTIONS
      moeda_nao_adm        = 1
      erro_ledger          = 2
      OTHERS               = 3.
  IF sy-subrc = 0.

    LOOP AT lt_saldos ASSIGNING FIELD-SYMBOL(<fs_saldos>).

      DO s_mes-low TIMES.

        ADD 1 TO lv_index.

        lv_mes = 'sl' && lv_index.

        ASSIGN COMPONENT lv_mes OF STRUCTURE <fs_saldos> TO <fs_valor>.

        lv_brl_atual = lv_brl_atual + <fs_valor>.

      ENDDO.

      CLEAR lv_index.

      lv_total_brl = lv_total_brl + <fs_saldos>-slvt.

    ENDLOOP.

    LOOP AT lt_saldos_usd ASSIGNING <fs_saldos>.

      DO s_mes-low TIMES.

        ADD 1 TO lv_index.

        lv_mes = 'sl' && lv_index.

        ASSIGN COMPONENT lv_mes OF STRUCTURE <fs_saldos> TO <fs_valor>.

        lv_usd_atual = lv_usd_atual + <fs_valor>.

      ENDDO.

      CLEAR lv_index.
      lv_total_usd = lv_total_usd + <fs_saldos>-slvt.

    ENDLOOP.

    APPEND INITIAL LINE TO lt_saida ASSIGNING <fs_saida>.

    <fs_saida>-descricao     = 'Ativos Fiscais Diferidos'.
    <fs_saida>-saldo_brl     = lv_total_brl.
    <fs_saida>-saldo_usd     = lv_total_usd.
    <fs_saida>-saldo_atu_brl = lv_brl_atual + lv_total_brl.
    <fs_saida>-saldo_atu_usd = lv_usd_atual + lv_total_usd.

    CLEAR: lv_total_brl,
           lv_total_usd.

  ENDIF.
*** Cálculo ativos - Fim



*** Cálculo passivos - Inicio
  CLEAR: lv_brl_atual,
         lv_usd_atual,
         lv_index,
         lv_mes,
         lv_total_brl,
         lv_total_usd.

  FREE: lt_saldos,
        lt_saldos_usd,
        rg_saknr.

  "DEVK9A209H - 15/05/24 - FI - Ajustes na transação ZGL076-Imposto Diferido - #138696 RSA
  SELECT low
         FROM tvarvc
         INTO TABLE @DATA(lt_conta_passivo)
         WHERE name EQ @c_conta_passivo.
  LOOP AT lt_conta_passivo INTO DATA(wa_conta_passivo).
    wa_saknr-sign   = 'I'.
    wa_saknr-option = 'EQ'.
    wa_saknr-low    = wa_conta_passivo-low.
    APPEND wa_saknr TO rg_saknr.
  ENDLOOP.

  IF NOT lt_contas_pas[] IS INITIAL AND NOT rg_saknr[] IS INITIAL.
    DELETE lt_contas_pas WHERE saknr IN rg_saknr.
  ENDIF.
  "DEVK9A209H - 15/05/24 - FI - Ajustes na transação ZGL076-Imposto Diferido - #138696 RSA

  CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
    EXPORTING
      ryear                = s_ano-low
      contas               = lt_contas_pas
      p_gerar_todas        = abap_true
      p_gerar_soc_parceira = abap_true
      rldnr                = '0L'
    TABLES
      it_saldos            = lt_saldos
      it_saldos_2          = lt_saldos_usd
    EXCEPTIONS
      moeda_nao_adm        = 1
      erro_ledger          = 2
      OTHERS               = 3.
  IF sy-subrc = 0.

    LOOP AT lt_saldos ASSIGNING <fs_saldos>.

      DO s_mes-low TIMES.

        ADD 1 TO lv_index.

        lv_mes = 'sl' && lv_index.

        ASSIGN COMPONENT lv_mes OF STRUCTURE <fs_saldos> TO <fs_valor>.

        lv_brl_atual = lv_brl_atual + <fs_valor>.

      ENDDO.

      CLEAR lv_index.

      lv_total_brl = lv_total_brl + <fs_saldos>-slvt.

    ENDLOOP.

    LOOP AT lt_saldos_usd ASSIGNING <fs_saldos>.

      DO s_mes-low TIMES.

        ADD 1 TO lv_index.

        lv_mes = 'sl' && lv_index.

        ASSIGN COMPONENT lv_mes OF STRUCTURE <fs_saldos> TO <fs_valor>.

        lv_usd_atual = lv_usd_atual + <fs_valor>.

      ENDDO.

      CLEAR lv_index.
      lv_total_usd = lv_total_usd + <fs_saldos>-slvt.

    ENDLOOP.

    APPEND INITIAL LINE TO lt_saida ASSIGNING <fs_saida>.

    <fs_saida>-descricao     = 'Passivos Fiscais Diferidos'.
    <fs_saida>-saldo_brl     = lv_total_brl.
    <fs_saida>-saldo_usd     = lv_total_usd.
    <fs_saida>-saldo_atu_brl = lv_brl_atual + lv_total_brl.
    <fs_saida>-saldo_atu_usd = lv_usd_atual + lv_total_usd.


  ENDIF.
*** Cálculo passivos - Fim


  " Cálculo Resultado
  FREE: lt_saldos,
        lt_saldos_usd.

  CLEAR: lv_mes,
         lv_index,
         lv_brl_atual,
         lv_total_brl,
         lv_usd_atual,
         lv_total_usd.


  CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
    EXPORTING
      ryear                = s_ano-low
      contas               = lt_contas_resultado
      p_gerar_todas        = abap_true
      p_gerar_soc_parceira = abap_true
      rldnr                = '0L'
    TABLES
      it_saldos            = lt_saldos
      it_saldos_2          = lt_saldos_usd
    EXCEPTIONS
      moeda_nao_adm        = 1
      erro_ledger          = 2
      OTHERS               = 3.
  IF sy-subrc = 0.

    LOOP AT lt_saldos ASSIGNING FIELD-SYMBOL(<fs_saldos_res>).

      DO s_mes-low TIMES.

        ADD 1 TO lv_index.

        lv_mes = 'sl' && lv_index.

        ASSIGN COMPONENT lv_mes OF STRUCTURE <fs_saldos_res> TO <fs_valor>.

        lv_brl_atual = lv_brl_atual + <fs_valor>.

      ENDDO.

      CLEAR lv_index.

      lv_total_brl = lv_total_brl + <fs_saldos_res>-slvt + lv_brl_atual.
      CLEAR lv_brl_atual.
    ENDLOOP.

    LOOP AT lt_saldos_usd ASSIGNING <fs_saldos>.

      DO s_mes-low TIMES.

        ADD 1 TO lv_index.

        lv_mes = 'sl' && lv_index.

        ASSIGN COMPONENT lv_mes OF STRUCTURE <fs_saldos> TO <fs_valor>.

        lv_usd_atual = lv_usd_atual + <fs_valor>.

      ENDDO.

      CLEAR lv_index.

      lv_total_usd = lv_total_usd + <fs_saldos>-slvt + lv_usd_atual.
      CLEAR lv_usd_atual.
    ENDLOOP.

  ENDIF.

  IF s_mes-low EQ '15'."DEVK9A209H - 15/05/24 - #138696 RSA
    s_mes-low = '12'.
  ENDIF.


  APPEND INITIAL LINE TO lt_saida ASSIGNING <fs_saida>.

  <fs_saida>-descricao = 'Resultado'.

  READ TABLE lt_saida ASSIGNING FIELD-SYMBOL(<fs_saida_5>) INDEX 5.
  READ TABLE lt_saida ASSIGNING FIELD-SYMBOL(<fs_saida_6>) INDEX 6.


*  <fs_saida>-movi_brl  = <fs_saida_5>-movi_brl - <fs_saida_6>-movi_brl .
*  <fs_saida>-movi_usd  = <fs_saida_5>-movi_usd - <fs_saida_6>-movi_usd  .
  <fs_saida>-movi_brl  = lv_total_brl.
  <fs_saida>-movi_usd  = lv_total_usd.


  APPEND INITIAL LINE TO lt_saida ASSIGNING <fs_saida>.

  READ TABLE lt_saida ASSIGNING FIELD-SYMBOL(<fs_saida_7>) INDEX 7.
  READ TABLE lt_saida ASSIGNING FIELD-SYMBOL(<fs_saida_8>) INDEX 8.

  FREE lt_color.

  APPEND INITIAL LINE TO lt_color ASSIGNING <fs_color>.

  <fs_color>-color-col = 3.
  <fs_color>-color-col = 3.
  <fs_color>-color-col = 3.

  <fs_saida>-color = lt_color.
  <fs_saida>-descricao     = 'Saldo Contábil'.
  <fs_saida>-saldo_brl     = <fs_saida_6>-saldo_brl     + <fs_saida_7>-saldo_brl + <fs_saida_8>-saldo_brl.
  <fs_saida>-saldo_usd     = <fs_saida_6>-saldo_usd     + <fs_saida_7>-saldo_usd + <fs_saida_8>-saldo_usd.
  <fs_saida>-movi_brl      = <fs_saida_6>-movi_brl      + <fs_saida_7>-movi_brl + <fs_saida_8>-movi_brl.
  <fs_saida>-movi_usd      = <fs_saida_6>-movi_usd      + <fs_saida_7>-movi_usd + <fs_saida_8>-movi_usd.
  <fs_saida>-saldo_atu_brl = <fs_saida_6>-saldo_atu_brl + <fs_saida_7>-saldo_atu_brl + <fs_saida_8>-saldo_atu_brl.
  <fs_saida>-saldo_atu_usd = <fs_saida_6>-saldo_atu_usd + <fs_saida_7>-saldo_atu_usd + <fs_saida_8>-saldo_atu_usd.

  APPEND INITIAL LINE TO lt_saida ASSIGNING <fs_saida>.

  READ TABLE lt_saida ASSIGNING FIELD-SYMBOL(<fs_saida_9>) INDEX 9.

  FREE lt_color.

  APPEND INITIAL LINE TO lt_color ASSIGNING <fs_color>.

  <fs_color>-color-col = 3.
  <fs_color>-color-col = 3.
  <fs_color>-color-col = 3.

  <fs_saida>-color = lt_color.
  <fs_saida>-descricao     = 'Check'.

*** BUG - 153691 - Inicio - CBRAND

*  <fs_saida>-saldo_brl     = <fs_saida_5>-saldo_brl     - <fs_saida_9>-saldo_brl .
*  <fs_saida>-saldo_usd     = <fs_saida_5>-saldo_usd     - <fs_saida_9>-saldo_usd .
*  <fs_saida>-movi_brl      = <fs_saida_5>-movi_brl      - <fs_saida_9>-movi_brl .
*  <fs_saida>-movi_usd      = <fs_saida_5>-movi_usd      - <fs_saida_9>-movi_usd .
*  <fs_saida>-saldo_atu_brl = <fs_saida_5>-saldo_atu_brl - <fs_saida_9>-saldo_atu_brl .
*  <fs_saida>-saldo_atu_usd = <fs_saida_5>-saldo_atu_usd - <fs_saida_9>-saldo_atu_usd.

  IF ( <fs_saida_9>-saldo_brl < 0 AND <fs_saida_5>-saldo_brl > 0 ) OR ( <fs_saida_9>-saldo_brl > 0 AND <fs_saida_5>-saldo_brl < 0 ).
    <fs_saida>-saldo_brl     = <fs_saida_5>-saldo_brl     + <fs_saida_9>-saldo_brl .
  ELSE.
    <fs_saida>-saldo_brl     = <fs_saida_5>-saldo_brl     - <fs_saida_9>-saldo_brl .
  ENDIF.

  IF ( <fs_saida_9>-saldo_usd > 0 AND <fs_saida_5>-saldo_usd < 0 ) OR  ( <fs_saida_9>-saldo_usd < 0 AND <fs_saida_5>-saldo_usd > 0 ).
    <fs_saida>-saldo_usd     = <fs_saida_5>-saldo_usd     + <fs_saida_9>-saldo_usd .
  ELSE.
    <fs_saida>-saldo_usd     = <fs_saida_5>-saldo_usd     - <fs_saida_9>-saldo_usd .
  ENDIF.

  IF ( <fs_saida_9>-movi_brl > 0 AND <fs_saida_5>-movi_brl < 0 ) OR ( <fs_saida_9>-movi_brl < 0 AND <fs_saida_5>-movi_brl > 0 ).
    <fs_saida>-movi_brl      = <fs_saida_5>-movi_brl      + <fs_saida_9>-movi_brl .
  ELSE.
    <fs_saida>-movi_brl      = <fs_saida_5>-movi_brl      - <fs_saida_9>-movi_brl .
  ENDIF.

  IF ( <fs_saida_9>-movi_usd > 0 AND <fs_saida_5>-movi_usd < 0 ) OR ( <fs_saida_9>-movi_usd < 0 AND <fs_saida_5>-movi_usd > 0 ).
    <fs_saida>-movi_usd      = <fs_saida_5>-movi_usd    + <fs_saida_9>-movi_usd .
  ELSE.
    <fs_saida>-movi_usd      = <fs_saida_5>-movi_usd    - <fs_saida_9>-movi_usd .
  ENDIF.
  IF ( <fs_saida_9>-saldo_atu_brl > 0 AND <fs_saida_5>-saldo_atu_brl < 0 ) OR ( <fs_saida_9>-saldo_atu_brl < 0 AND <fs_saida_5>-saldo_atu_brl > 0 ).
    <fs_saida>-saldo_atu_brl = <fs_saida_5>-saldo_atu_brl + <fs_saida_9>-saldo_atu_brl .
  ELSE.
    <fs_saida>-saldo_atu_brl = <fs_saida_5>-saldo_atu_brl - <fs_saida_9>-saldo_atu_brl .
  ENDIF.
  IF ( <fs_saida_9>-saldo_atu_usd > 0 AND <fs_saida_5>-saldo_atu_usd < 0 ) OR  ( <fs_saida_9>-saldo_atu_usd < 0 AND <fs_saida_5>-saldo_atu_usd > 0 ).
    <fs_saida>-saldo_atu_usd = <fs_saida_5>-saldo_atu_usd + <fs_saida_9>-saldo_atu_usd.
  ELSE.
    <fs_saida>-saldo_atu_usd = <fs_saida_5>-saldo_atu_usd - <fs_saida_9>-saldo_atu_usd.
  ENDIF.
*** BUG - 153691 - Fim - CBRAND

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lr_alv
        CHANGING
          t_table      = lt_saida ).


      lr_columns = lr_alv->get_columns( ).
      lo_columns = lr_alv->get_columns( ).

      CALL METHOD lo_columns->set_color_column
        EXPORTING
          value = 'COLOR'.

      lr_columns->set_optimize( ).

      lr_alv->set_screen_status(
        pfstatus      = 'STANDARD_FULLSCREEN'
        report        = 'ZGLR072'
        set_functions = lr_alv->c_functions_all ).

      lr_alv->display( ).

    CATCH cx_salv_no_new_data_allowed cx_salv_error.
      EXIT.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_RESULTADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_alv_resultado .

  DATA: lr_alv       TYPE REF TO cl_salv_table,
        tl_zglt0113  TYPE TABLE OF zglt0113,
        tl_alv_resul TYPE TABLE OF zgle_alv_resultado,
        wl_alv_resul TYPE zgle_alv_resultado,
        wl_slvt_brl  TYPE	hslvt12,
        wl_slvt_usd  TYPE	hslvt12,
        cont         TYPE n LENGTH 2.

*... set the columns technical
  DATA: lr_columns      TYPE REF TO cl_salv_columns,
        lo_columns      TYPE REF TO cl_salv_columns_table,
        lr_aggregations TYPE REF TO cl_salv_aggregations,
        lr_groups       TYPE REF TO cl_salv_sorts,
        toolbar         TYPE REF TO cl_salv_functions_list.

*------------------------------------------------
* selecao
*------------------------------------------------
  IF s_tp_imp-low IS INITIAL.
    SELECT * FROM zglt0113 INTO TABLE tl_zglt0113.
  ELSE.
    SELECT * FROM zglt0113 INTO TABLE tl_zglt0113 WHERE tp_imposto =  s_tp_imp-low.
  ENDIF.

  IF tl_zglt0113[] IS NOT INITIAL.
    SELECT *
           INTO TABLE t_zglt0101
           FROM zglt0101
           FOR ALL ENTRIES IN tl_zglt0113
           WHERE tp_imposto EQ tl_zglt0113-tp_imposto.
  ENDIF.

  CHECK tl_zglt0113[] IS NOT INITIAL.


  LOOP AT tl_zglt0113 INTO DATA(wl_zglt0113).
    REFRESH: it_contas,
             it_saldo_contas,
             it_saldo_contas_2,
             it_saldo_contas_3.

    IF wl_zglt0113-csll EQ abap_true.
      wl_alv_resul-imposto      = 'CSLL'.
    ENDIF.

    IF wl_zglt0113-irpj EQ abap_true.
      wl_alv_resul-imposto      = 'IRPJ'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_zglt0113-saknr
      IMPORTING
        output = wl_zglt0113-saknr.

    wa_contas-bukrs = s_empre-low.
    wa_contas-saknr =  wl_zglt0113-saknr.
    APPEND wa_contas TO it_contas.

    wl_alv_resul-tp_imposto   = wl_zglt0113-tp_imposto.
    wl_alv_resul-desc_imposto = wl_zglt0113-desc_impost.
    wl_alv_resul-conta        = wl_zglt0113-saknr.
    wl_alv_resul-desc_conta   = wl_zglt0113-desc_conta.

    "119411 CS2023000539 Melhoria na transação ZGL076-Relatório Imposto Diferido - PSA
    SELECT SINGLE *
           INTO @DATA(wl_ska1)
           FROM ska1
           WHERE saknr EQ @wl_alv_resul-conta
             AND ktopl EQ '0050'.

    READ TABLE t_zglt0101 INTO DATA(wl_zglt0101) WITH KEY tp_imposto = wl_zglt0113-tp_imposto.
    IF sy-subrc EQ 0.
      IF wl_alv_resul-desc_imposto IS INITIAL.
        wl_alv_resul-desc_imposto = wl_zglt0101-desc_tp_imposto.
      ENDIF.
    ENDIF.

    IF wl_zglt0113-saknr IS NOT INITIAL AND wl_alv_resul-desc_conta IS INITIAL.
      SELECT SINGLE txt50 INTO wl_alv_resul-desc_conta FROM gl_acct_ca_text WHERE saknr = wl_zglt0113-saknr AND spras = 'P' AND ktopl EQ '0050'.
    ENDIF.

    DATA: v_ano_anterior        TYPE gjahr.
    DATA: v_ano_selecao        TYPE gjahr.
    DATA: v_ano        TYPE gjahr.
    CLEAR: v_ano_anterior,v_ano_selecao,v_ano.

    "DEVK9A209H - 16/05/24 - #138696 RSA
*    v_ano_anterior = s_ano-low - 1.
*    v_ano_selecao = s_ano-low.
*    IF wl_ska1-ktoks EQ 'YB05' OR wl_ska1-ktoks EQ 'YB06' OR
*       wl_ska1-ktoks EQ 'YB07' OR wl_ska1-ktoks EQ 'YB08'.
*      v_ano = v_ano_anterior.
*    ELSE.
*      v_ano = v_ano_selecao.
*    ENDIF.
    "DEVK9A209H - 16/05/24 - #138696 RSA

    IF s_mes-low EQ '12'.
      s_mes-low = '15'.
    ENDIF.
    v_ano = s_ano-low.

    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear         = v_ano
        contas        = it_contas
        p_gerar_todas = 'X'
        rldnr         = '0L'
      TABLES
        it_saldos     = it_saldo_contas
        it_saldos_2   = it_saldo_contas_2
        it_saldos_3   = it_saldo_contas_3.

    READ TABLE it_saldo_contas INTO DATA(w_saldo_brl) WITH KEY racct  = wl_zglt0113-saknr
                                                               rbukrs = s_empre-low
                                                               ryear  = v_ano. "Regra da coluna 'Saldo 31/12' BRL

    "119411 CS2023000539 Melhoria na transação ZGL076-Relatório Imposto Diferido - PSA
    IF sy-subrc IS INITIAL.

      IF wl_ska1-ktoks EQ 'YB05' OR wl_ska1-ktoks EQ 'YB06' OR wl_ska1-ktoks EQ 'YB07' OR wl_ska1-ktoks EQ 'YB08'.
        cont = 1.
        DO.
          IF cont > 15 OR
             cont > s_mes-low."DEVK9A209H - 16/05/24 - #138696 RSA
            EXIT.
          ENDIF.
          CONCATENATE 'SL' cont INTO DATA(fielname).
          ASSIGN COMPONENT fielname OF STRUCTURE w_saldo_brl TO FIELD-SYMBOL(<value>).
          IF <value> IS ASSIGNED.
            ADD <value> TO wl_alv_resul-s_balc_brl.
          ENDIF.
          ADD 1 TO cont.
        ENDDO.
        wl_alv_resul-s_balc_brl = wl_alv_resul-s_balc_brl + w_saldo_brl-slvt.
      ELSE.
        wl_alv_resul-s_balc_brl = w_saldo_brl-slvt. "'Saldo 31/12' BRL
      ENDIF.

    ENDIF.

**********************************************************************
    "119411 CS2023000539 Melhoria na transação ZGL076-Relatório Imposto Diferido - PSA
    "
    READ TABLE it_saldo_contas_2 INTO DATA(w_saldo_usd) WITH KEY racct = wl_zglt0113-saknr
                                                                rbukrs = s_empre-low
                                                                 ryear =  v_ano. "Regra da coluna 'Saldo 31/12' USD

    IF sy-subrc IS INITIAL.

      IF wl_ska1-ktoks EQ 'YB05' OR wl_ska1-ktoks EQ 'YB06' OR wl_ska1-ktoks EQ 'YB07' OR wl_ska1-ktoks EQ 'YB08'.
        cont = 1.
        DO.
          IF cont > 15 OR
             cont > s_mes-low."DEVK9A209H - 16/05/24 - #138696 RSA
            EXIT.
          ENDIF.
          CONCATENATE 'SL' cont INTO DATA(fielname1).
          ASSIGN COMPONENT fielname1 OF STRUCTURE w_saldo_usd TO FIELD-SYMBOL(<value1>).
          IF <value1> IS ASSIGNED.
            ADD <value1> TO wl_alv_resul-s_balc_usd.
          ENDIF.
          ADD 1 TO cont.
        ENDDO.
        wl_alv_resul-s_balc_usd = wl_alv_resul-s_balc_usd + w_saldo_usd-slvt.
      ELSE.
        wl_alv_resul-s_balc_usd = w_saldo_usd-slvt. "'Saldo 31/12' USD
      ENDIF.


    ENDIF.

    IF s_mes-low EQ '15'.
      s_mes-low = '12'.
    ENDIF.

    CLEAR: wl_alv_resul-movi_brl, wl_alv_resul-movi_usd.

    LOOP AT t_alv_saida INTO DATA(wl_alv_said) WHERE tp_imposto EQ wl_zglt0113-tp_imposto.

      IF wl_alv_resul-imposto = 'IRPJ'."wl_zglt0113-irpj EQ abap_true.

        wl_alv_resul-movi_brl = wl_alv_resul-movi_brl + wl_alv_said-m_irpj_brl.
        wl_alv_resul-movi_usd = wl_alv_resul-movi_usd + wl_alv_said-m_irpj_usd.

      ENDIF.

      IF wl_alv_resul-imposto = 'CSLL'."wl_zglt0113-csll EQ abap_true.

        wl_alv_resul-movi_brl = wl_alv_resul-movi_brl + wl_alv_said-m_csll_brl.
        wl_alv_resul-movi_usd = wl_alv_resul-movi_usd + wl_alv_said-m_csll_usd.

      ENDIF.
    ENDLOOP.

    wl_alv_resul-result_brl = wl_alv_resul-movi_brl + wl_alv_resul-s_balc_brl.
    wl_alv_resul-result_usd = wl_alv_resul-movi_usd + wl_alv_resul-s_balc_usd. "Saldo Balancete USD

    APPEND wl_alv_resul TO tl_alv_resul.
    CLEAR wl_alv_resul.
    CLEAR: wl_slvt_brl, wl_slvt_usd, w_saldo_usd, w_saldo_brl.


  ENDLOOP.

  SORT tl_alv_resul BY  tp_imposto conta imposto.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lr_alv
        CHANGING
          t_table      = tl_alv_resul ).
    CATCH cx_salv_no_new_data_allowed cx_salv_error.
      EXIT.
  ENDTRY.

  lr_columns = lr_alv->get_columns( ).
  lo_columns = lr_alv->get_columns( ).

***  CALL METHOD lo_columns->set_color_column
***    EXPORTING
***      value = 'COLOR'.

  lr_columns->set_optimize( ).

  lr_alv->set_screen_status(
    pfstatus      = 'STANDARD_FULLSCREEN'
    report        = 'ZGLR072'
    set_functions = lr_alv->c_functions_all ).


  lr_aggregations = lr_alv->get_aggregations( ).
  toolbar = lr_alv->get_functions( ) .
  toolbar->set_all( value = if_salv_c_bool_sap=>true ).

  lr_aggregations->clear( ).
  lr_groups = lr_alv->get_sorts( ) .
  lr_groups->clear( ).

  TRY.
      lr_groups->add_sort(
        columnname = 'IMPOSTO'
        position   = 5
        subtotal   = abap_true
        sequence   = if_salv_c_sort=>sort_up ).

    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
  ENDTRY.
  TRY.
      lr_aggregations->add_aggregation( columnname = 'S_BALC_BRL' )."DEVK9A209H - 15/05/24 - #138696 RSA
      lr_aggregations->add_aggregation( columnname = 'S_BALC_USD' )."DEVK9A209H - 15/05/24 - #138696 RSA
      lr_aggregations->add_aggregation( columnname = 'MOVI_BRL' ).
      lr_aggregations->add_aggregation( columnname = 'MOVI_USD' ).
      lr_aggregations->add_aggregation( columnname = 'RESULT_BRL' )."DEVK9A209H - 15/05/24 - #138696 RSA
      lr_aggregations->add_aggregation( columnname = 'RESULT_USD' )."DEVK9A209H - 15/05/24 - #138696 RSA
    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
  ENDTRY.

  lr_alv->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_CELL_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_preenche_cell_color.

  DATA:
    w_fcat    LIKE LINE OF t_fieldcat,
    w_celltab TYPE lvc_s_styl,
    w_col     TYPE lvc_s_scol,
    w_color   TYPE lvc_s_colo.

  REFRESH: t_coltab.

  LOOP AT t_fieldcat INTO w_fcat.
    CASE w_fcat-fieldname.
      WHEN 'MOVI_BRL' OR 'MOVI_USD' OR 'M_IRPJ_BRL' OR 'M_IRPJ_USD' OR 'M_CSLL_BRL' OR 'M_CSLL_USD'.
        w_color-col   = gc-verde.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = w_fcat-fieldname.  "Nome da coluna
        w_col-color  = w_color.
        APPEND w_col TO t_coltab.
      WHEN 'SALDO_BRL' OR 'SALDO_USD' OR 'S_IRPJ_BRL' OR 'S_IRPJ_USD' OR 'S_CSLL_BRL' OR 'S_CSLL_USD'.
        w_color-col   = gc-azul.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = w_fcat-fieldname.  "Nome da coluna
        w_col-color  = w_color.
        APPEND w_col TO t_coltab.
      WHEN 'SALDO_ATU_BRL' OR 'SALDO_ATU_USD' OR 'IRPJ_BRL' OR 'IRPJ_USD' OR 'CSLL_BRL' OR 'CSLL_USD'.
        w_color-col   = gc-amarelo.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = w_fcat-fieldname.  "Nome da coluna
        w_col-color  = w_color.
        APPEND w_col TO t_coltab.
      WHEN OTHERS.
        w_color-col   = gc-padrao.
        w_color-int   = gc-int.
        w_color-inv   = gc-inv.
        w_col-fname   = w_fcat-fieldname.  "Nome da coluna
        w_col-color   = w_color.
        APPEND w_col TO t_coltab.
    ENDCASE.
  ENDLOOP.

  LOOP AT t_alv_saida ASSIGNING FIELD-SYMBOL(<fs_alv_saida>).
    REFRESH: <fs_alv_saida>-color_cell[].
    INSERT LINES OF t_coltab[] INTO TABLE  <fs_alv_saida>-color_cell[].
  ENDLOOP.

ENDFORM.

FORM f_start_selection.

  IF s_empre-low  IS INITIAL OR s_mes-low IS INITIAL OR s_ano-low IS INITIAL.
    MESSAGE s024(sd) WITH 'Informar campos obrigatórios.' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF s_mes-low > 12. " CBRAND - 20.01.2023
    CONCATENATE s_ano-low  '12' '31' INTO v_ultimo_dia.
  ELSE.

    CONCATENATE s_ano-low  s_mes-low '01' INTO v_data1.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = v_data1
      IMPORTING
        last_day_of_month = v_ultimo_dia.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form retorna_status_zib_2
*&---------------------------------------------------------------------*
FORM retorna_status_zib_2 USING    i_doc_lcto_2 TYPE zglt077-doc_lcto
                                   i_ano_lcto_2 TYPE zglt077-gjahr
                          CHANGING e_zibchv_2   TYPE zib_contabil_chv
                                   e_ziberr_2   TYPE zib_contabil_err.

  DATA v_objkey    TYPE char20.
  CLEAR: e_zibchv_2, e_ziberr_2.

  CONCATENATE 'ZGL17' i_doc_lcto_2 i_ano_lcto_2 INTO v_objkey.

  SELECT SINGLE *
    FROM zib_contabil_chv
    INTO e_zibchv_2
   WHERE obj_key = v_objkey.

  IF ( sy-subrc IS NOT INITIAL ).

    SELECT SINGLE *
      FROM zib_contabil_err
      INTO e_ziberr_2
     WHERE obj_key = v_objkey.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_mes_subsequente
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ANO
*&      <-- LV_MES
*&---------------------------------------------------------------------*
FORM f_mes_subsequente  USING    p_ano TYPE bseg-gjahr
                                 p_mes TYPE bkpf-monat
                        CHANGING p_data.

  " Incrementar o mês
  DATA(lv_mes_sub) = p_mes + 1.

  UNPACK lv_mes_sub TO p_mes.
  " Verificar mudança de ano
  IF lv_mes_sub > 12.
    p_mes = '01'.
    p_ano = p_ano + 1.
  ENDIF.

  " Formar a nova data (primeiro dia do próximo mês)
  CONCATENATE  '01' '.' p_mes '.' p_ano INTO p_data.


ENDFORM.
