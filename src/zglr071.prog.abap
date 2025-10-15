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
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Carolini Santos ( carolini.santos@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Gerar Fluxo de Arrendamentos                                              |*
**/===========================================================================\*

REPORT zglr071.

TYPE-POOLS: slis.

PARAMETERS: p_cont   TYPE zgle0003 NO-DISPLAY,
            p_mes    TYPE char2    NO-DISPLAY,
            p_ano    TYPE char4    NO-DISPLAY,
            p_dtvenc TYPE sy-datum NO-DISPLAY,
            p_tp_arr TYPE zgle0001 NO-DISPLAY.


TYPES: BEGIN OF ty_saida_02,
         fl_venc_comp   TYPE sy-datum, "01 Fluxo de vencimento 'Competência'
         vl_parcela     TYPE zgle0004, "01 Valor da Parcela
         t_pagar_brl    TYPE zgle0004, "01 Total a Pagar BRL
         t_pagar_usd    TYPE zgle0004, "01 Total a Pagar USD
         taxa_aa        TYPE zgle0004, "01 Taxa AA
         taxa_am        TYPE zgle0004, "01 Taxa AM
         vp_un_presente TYPE zgle0004, "01 Valor Unitario Presente
         vp_vl_presente TYPE zgle0004, "01 Valor Presente
         vp_vl_juros    TYPE zgle0004, "01 Valor Presente Juros
         vp_vl_total    TYPE zgle0004, "01 Valor Presente Total
         bp_cir_pri     TYPE zgle0004, "01 Balanço Patrimonial Circulante Principal
         bp_cir_jur     TYPE zgle0004, "01 Balanço Patrimonial Circulante Juros
         bp_ncir_pri    TYPE zgle0004, "01 Balanço Patrimonial Não Circulante Principal
         bp_ncir_jur    TYPE zgle0004, "01 Balanço Patrimonial Não Circulante Juros
         bp_tot_pri     TYPE zgle0004, "01 Balanço Patrimonial Total Principal
         bp_tot_jur     TYPE zgle0004, "01 Balanço Patrimonial Total Juros
         jircp          TYPE zgle0004, "01 Juros Impacto no Resultado CP
         jirlp          TYPE zgle0004, "01 Juros Impacto no Resultado LP
         jirso          TYPE zgle0004, "01 Juros Impacto no Resultado Soma
         aprov_jur      TYPE zgle0004, "01 Aprov Juros
         transflpcp     TYPE zgle0004, "01 Transf.LP/CP
         saldo_cp       TYPE zgle0004, " Saldo cp
         saldo_lp       TYPE zgle0004, " Saldo LP
         doc_s_ini      TYPE char10,   "Doc.Saldo Inicial
         est_s_ini      TYPE char10,   "Est.Saldo Inicial
         doc_a_jur      TYPE char10,   "Doc. Apro. Juros
         est_a_jur      TYPE char10,   "Est. Apro. Juros
         doc_lpcp       TYPE char10,   "Doc. LP/CP
         est_lpcp       TYPE char10,   "Est. LP/CP
         obj_keyi(20)   TYPE c,
         obj_keyj(20)   TYPE c,
         obj_keyl(20)   TYPE c,
         doc_lcti(10)   TYPE n,
         doc_lctj(10)   TYPE n,
         doc_lctl(10)   TYPE n,
         lt_i           TYPE zlote_num,
         lt_j           TYPE zlote_num,
         lt_l           TYPE zlote_num,
         tcolor         TYPE slis_t_specialcol_alv,
         color(4)       TYPE c,
         status(4),
       END OF ty_saida_02.

TYPES: BEGIN OF ty_saida_03,
         num_parcela      TYPE zglt094-num_parcela,
         dt_atualizacao   TYPE zglt094-dt_vencimento, " Competencia vencimento
         comp             TYPE char7, " Competencia mes e ano
         qnt_sacas        TYPE zgle0017,
         qnt_sacas_ms     TYPE zgle0017,
         saca_acumulada   TYPE zgle0017, " Saca acumulada
         preco_saca       TYPE zgle0017, " Preco saca mes
         preco_saca_a     TYPE zgle0017, " Saca Parcela Anual
         vlr_p_m_sacas    TYPE zgle0017, " Saca Parcela Mes
         vlr_p_brl        TYPE zgle0017,
         vlr_p_m_brl      TYPE zgle0017,
         vlr_p_usd        TYPE zgle0017,
         vlr_p_m_usd      TYPE zgle0017,
         preco_saca_brl   TYPE zgle0017,
         taxa_01          TYPE zgle0017,
         preco_saca_usd   TYPE zgle0017,
         vlr_saca_brl(16) TYPE p DECIMALS 2,
         taxa_02(16)      TYPE p DECIMALS 4,
         vlr_saca_usd     TYPE zgle0017,
         vlr_atual_brl    TYPE zgle0017, " Valor atualizado
         variacao_m_brl   TYPE zgle0017, " Variacao mes brl
         vlr_atual_usd    TYPE zgle0017, " Variacao mes usd
         ptax             TYPE zgle0041, " PTAX
         pis_brl          TYPE zgle0017, "
         pis_usd          TYPE zgle0017, "
         cofins_brl       TYPE zgle0017, "
         cofins_usd       TYPE zgle0017, "
         variacao_m_usd   TYPE zgle0017,
         obj_key          TYPE char20,
         doc_lcto(10)     TYPE n,
         lt_doc           TYPE zlote_num,  " Lote
         doc_contabil     TYPE char10,     " Doc_contabil
         doc_estorno      TYPE char10,     " Estorno
         celltab          TYPE lvc_t_styl,
         tcolor           TYPE slis_t_specialcol_alv,
         color(4)         TYPE c,
         status(4),
       END OF ty_saida_03.

TYPES: BEGIN OF ty_saida_05.
         INCLUDE TYPE zglt096g.
TYPES:
         mark      TYPE c,
         comp      TYPE char7, " Competencia mes e ano
         celltab   TYPE lvc_t_styl,
         tcolor    TYPE slis_t_specialcol_alv,
         color(4)  TYPE c,
         status(4),
       END OF ty_saida_05.

TYPES BEGIN OF ty_096e.
INCLUDE TYPE zglt096e.
TYPES END OF ty_096e.

TYPES BEGIN OF ty_096f.
INCLUDE TYPE zglt096f.
TYPES END OF ty_096f.

TYPES: BEGIN OF ty_zib_contabil_err.
         INCLUDE STRUCTURE zib_contabil_err.
TYPES:   mark TYPE c,
       END OF ty_zib_contabil_err.

TYPES: BEGIN OF ty_saida_04_1,
         vlr_contrato_brl TYPE zglt094-valor_brl,
         vlr_contrato_usd TYPE zglt094-valor_usd,
       END OF ty_saida_04_1.


TYPES: BEGIN OF ty_saida_04,
         num_parcela     TYPE zglt094-num_parcela,
         periodo(8)      TYPE c,
         apropriacao_brl TYPE zgle0017,
         apropriacao_usd TYPE zgle0017,
         sald_a_apro_blr TYPE zgle0017,
         sald_a_apro_usd TYPE zgle0017,
         pis             TYPE zgle0017,
         cofins          TYPE zgle0017,
         documento       TYPE zglt094-num_documento,
       END OF ty_saida_04.

TYPES BEGIN OF ty_0094.
INCLUDE TYPE zglt094.
TYPES END OF ty_0094.

TYPES: BEGIN OF ty_calc,
         num_parcela    TYPE zglt094-num_parcela,
         vlr_at_brl_ant TYPE zgle0017,
         vlr_at_usd_ant TYPE zgle0017,
       END OF ty_calc.


TYPES: BEGIN OF ty_zglt100.
         INCLUDE STRUCTURE zglt100.
TYPES:   ptax TYPE tcurr-ukurs,
       END OF ty_zglt100.



DATA: wa_092              TYPE zglt092,
      it_096              TYPE TABLE OF zglt096,
      wa_096              TYPE zglt096,
      it_096g             TYPE TABLE OF zglt096g,
      wa_096g             TYPE zglt096g,
      it_096f             TYPE TABLE OF zglt096f,
      it_035              TYPE TABLE OF zglt035,
      it_096fl            TYPE TABLE OF zglt096f,
      wa_096fl            TYPE ty_096f,
      wa_096f             TYPE ty_096f,
      wa_096fsi           TYPE ty_096f,
      it_094              TYPE TABLE OF ty_0094,
      wa_094              TYPE  ty_0094,
      it_092              TYPE TABLE OF zglt092,
      it_096c             TYPE TABLE OF zglt096c,
      it_zglt100          TYPE TABLE OF ty_zglt100,
      wa_096c             TYPE zglt096c,
      it_saida_02         TYPE TABLE OF ty_saida_02,
      wa_saida_02         TYPE ty_saida_02,
      it_saida_02_temp    TYPE TABLE OF ty_saida_02,
      wa_saida_02_temp    TYPE ty_saida_02,
      it_zib_contabil_err TYPE TABLE OF ty_zib_contabil_err,
      wa_zib_contabil_err TYPE ty_zib_contabil_err,
      it_saida_03         TYPE TABLE OF ty_saida_03,
      wa_saida_03         TYPE ty_saida_03,
      it_saida_05         TYPE TABLE OF ty_saida_05,
      wa_saida_05         TYPE ty_saida_05,
      it_saida_04         TYPE TABLE OF ty_saida_04,
      wa_saida_04         TYPE ty_saida_04,
      it_saida_04_1       TYPE TABLE OF ty_saida_04_1,
      wa_saida_04_1       TYPE ty_saida_04_1,
      it_calc             TYPE TABLE OF ty_calc,
      wa_calc             TYPE ty_calc,
      aux_glaccount_type  TYPE ska1-glaccount_type.

DATA: it_dta     TYPE STANDARD TABLE OF bdcdata WITH HEADER LINE,
      wa_dta     TYPE bdcdata,
      wg_bdc     TYPE bdcdata,
      var_answer TYPE c,
      tg_bdc     TYPE TABLE OF bdcdata,
      tg_msg     TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wg_msg     TYPE bdcmsgcoll,
      opt        TYPE ctu_params.

DATA: "g_custom_container      TYPE REF TO cl_gui_custom_container,
  g_custom_container01    TYPE REF TO cl_gui_custom_container,
  g_custom_container02    TYPE REF TO cl_gui_custom_container,
  g_alv                   TYPE REF TO cl_gui_alv_grid,
  "g_alv01                 TYPE REF TO cl_gui_alv_grid,
  g_alv02                 TYPE REF TO cl_gui_alv_grid,
  dg_splitter_1           TYPE REF TO cl_gui_splitter_container,
  dg_parent_1             TYPE REF TO cl_gui_container,
  dg_splitter_2           TYPE REF TO cl_gui_splitter_container,
  dg_parent_2             TYPE REF TO cl_gui_container,
  dg_parent_2a            TYPE REF TO cl_gui_container,
  dg_parent_alv           TYPE REF TO cl_gui_container,
  picture                 TYPE REF TO cl_gui_picture,
  it_fieldcat             TYPE lvc_t_fcat,
  it_fieldcat01           TYPE lvc_t_fcat,
  it_exclude_fcode        TYPE ui_functions,
  wa_exclude_fcode        LIKE LINE OF it_exclude_fcode,
  dg_dyndoc_id            TYPE REF TO cl_dd_document,
  table_element           TYPE REF TO cl_dd_table_element,
  column                  TYPE REF TO cl_dd_area,
  table_element2          TYPE REF TO cl_dd_table_element,
  column_1                TYPE REF TO cl_dd_area,
  column_2                TYPE REF TO cl_dd_area,
  wa_layout               TYPE lvc_s_layo,
  wa_stable               TYPE lvc_s_stbl VALUE 'XX',
  dg_html_cntrl           TYPE REF TO cl_gui_html_viewer,
  sdydo_text_element(255),
  p_text_table            TYPE sdydo_text_table.

DATA: gt_estilo01          TYPE lvc_t_styl.

DATA: it_zglt036_flg TYPE zde_zglt036_flg_t,
      wa_zglt036_flg TYPE zde_zglt036_flg.

DATA: taxa           TYPE zgle0004,
      tx(25)         TYPE c,
      vlr_at_brl_ant TYPE zgle0017,
      vlr_at_usd_ant TYPE zgle0017,
      vlr_out_taxa   TYPE tcurr-ukurs.


START-OF-SELECTION.

  PERFORM select_dados.

  CALL SCREEN 0100.


CLASS lcl_event_hander DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.
    METHODS handle_set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object.
    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.
ENDCLASS.

CLASS lcl_event_hander IMPLEMENTATION.

  METHOD on_hotspot_click.

    DATA: wl_ano(4),
          lv_competencia TYPE char7,
          lv_belnr       TYPE belnr_d.

    TYPES: BEGIN OF ty_itab ,
             name(80) TYPE c,
           END OF ty_itab.

    DATA: msg_alv  TYPE char80,
          itab_msg TYPE TABLE OF ty_itab,
          wtab_msg TYPE  ty_itab.

    DATA: lr_lote TYPE RANGE OF zlote_num,
          lw_lote LIKE LINE OF lr_lote.


    CASE p_tp_arr.
      WHEN '03'.

        READ TABLE it_saida_03 INTO wa_saida_03 INDEX e_row_id-index.
        IF sy-subrc = 0.

          CASE e_column_id .
            WHEN 'STATUS'.

              IF wa_saida_03-status = icon_led_red .
                SELECT DISTINCT *
                   FROM zib_contabil_err
                   INTO TABLE it_zib_contabil_err
                   WHERE obj_key  = wa_saida_03-obj_key.

                wtab_msg-name    = 'MENSAGEM ERRO:'.
                APPEND wtab_msg TO itab_msg .
                CLEAR wtab_msg.
                LOOP AT it_zib_contabil_err INTO wa_zib_contabil_err.
                  wtab_msg-name = wa_zib_contabil_err-message.
                  APPEND wtab_msg TO itab_msg .
                  CLEAR wtab_msg.
                ENDLOOP.

                IF wa_saida_03-doc_lcto IS INITIAL.
                  wtab_msg-name = 'Doc. ainda não gerado!'.
                  APPEND wtab_msg TO itab_msg .
                  CLEAR wtab_msg.
                ENDIF.

                CONCATENATE 'DOCUMENTO ' wa_saida_03-doc_lcto INTO msg_alv SEPARATED BY space.
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
              ELSEIF wa_saida_03-status = icon_led_yellow.
                wtab_msg-name    = 'MENSAGEM:'.
                APPEND wtab_msg TO itab_msg .
                CLEAR wtab_msg.
*--------

                IF wa_saida_03-obj_key IS NOT INITIAL.
                  SELECT DISTINCT *
                     FROM zib_contabil_err
                     INTO TABLE it_zib_contabil_err
                     WHERE obj_key  = wa_saida_03-obj_key.
                ELSE.
                  REFRESH it_zib_contabil_err.
                ENDIF.

*                IF wa_saida_02_temp-obj_keyj IS NOT INITIAL.
*                  SELECT *
*                     FROM zib_contabil_err
*                     APPENDING TABLE it_zib_contabil_err
*                     WHERE obj_key  = wa_saida_02_temp-obj_keyj.
*                ELSE.
*                  REFRESH it_zib_contabil_err.
*                ENDIF.
*
*                IF wa_saida_02_temp-obj_keyl IS NOT INITIAL.
*                  SELECT *
*                     FROM zib_contabil_err
*                     APPENDING TABLE it_zib_contabil_err
*                     WHERE obj_key  = wa_saida_02_temp-obj_keyl.
*                ELSE.
*                  REFRESH it_zib_contabil_err.
*                ENDIF.

                IF it_zib_contabil_err[] IS NOT INITIAL.
                  wtab_msg-name    = 'MENSAGEM ERRO:'.
                  APPEND wtab_msg TO itab_msg .
                  CLEAR wtab_msg.
                  LOOP AT it_zib_contabil_err INTO wa_zib_contabil_err.
                    wtab_msg-name = wa_zib_contabil_err-message.
                    APPEND wtab_msg TO itab_msg .
                    CLEAR wtab_msg.
                  ENDLOOP.
                  wa_saida_03-status = icon_led_red.
                  MODIFY it_saida_03 FROM wa_saida_03 INDEX e_row_id-index.
                ENDIF.
*--------
                IF wa_saida_03-doc_lcto IS NOT INITIAL AND itab_msg[] IS INITIAL.
                  wtab_msg = 'AGUARDANDO PROCESSAMENTO DA ZIB_CONTABIL'.
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
              ELSEIF wa_saida_03-status = icon_led_green.
                wtab_msg-name    = 'MENSAGEM:'.
                APPEND wtab_msg TO itab_msg .
                CLEAR wtab_msg.

                IF wa_saida_03-doc_lcto IS NOT INITIAL.
                  wtab_msg = 'DOCUMENTO GERADO COM SUCESSO.'.
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

            WHEN 'DOC_CONTABIL'.

              IF wa_saida_03-doc_contabil IS NOT INITIAL.
                lv_belnr = wa_saida_03-doc_contabil.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_03-dt_atualizacao(4)."p_ano.
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'DOC_ESTORNO'.

              IF wa_saida_03-doc_estorno IS NOT INITIAL.
                lv_belnr = wa_saida_03-doc_estorno.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_03-dt_atualizacao(4).
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'LT_DOC'.
              IF wa_saida_03-lt_doc IS NOT INITIAL.
                lw_lote-sign = 'I'.
                lw_lote-option = 'EQ'.
                lw_lote-low = wa_saida_03-lt_doc.
                APPEND lw_lote TO lr_lote.
                CLEAR lw_lote.

                SUBMIT zgl018   WITH p_bukrs    EQ wa_092-bukrs
                                WITH p_lote     IN lr_lote
                AND RETURN.

              ENDIF.

          ENDCASE.
        ENDIF.

      WHEN '02'.
        READ TABLE it_saida_02 INTO wa_saida_02_temp INDEX e_row_id-index.
        IF sy-subrc = 0.

          CASE e_column_id .
            WHEN 'STATUS'.

              IF wa_saida_02_temp-status = icon_led_red .
                SELECT DISTINCT *
                   FROM zib_contabil_err
                   INTO TABLE it_zib_contabil_err
                   WHERE obj_key  = wa_saida_02_temp-obj_keyi.

                SELECT DISTINCT *
                   FROM zib_contabil_err
                   APPENDING TABLE it_zib_contabil_err
                   WHERE obj_key  = wa_saida_02_temp-obj_keyj.

                SELECT DISTINCT *
                   FROM zib_contabil_err
                   APPENDING TABLE it_zib_contabil_err
                   WHERE obj_key  = wa_saida_02_temp-obj_keyl.


                wtab_msg-name    = 'MENSAGEM ERRO:'.
                APPEND wtab_msg TO itab_msg .
                CLEAR wtab_msg.
                LOOP AT it_zib_contabil_err INTO wa_zib_contabil_err.
                  wtab_msg-name = wa_zib_contabil_err-message.
                  APPEND wtab_msg TO itab_msg .
                  CLEAR wtab_msg.
                ENDLOOP.

                IF wa_saida_02_temp-doc_lcti IS INITIAL.
                  wtab_msg-name = 'Doc. ainda não gerado!'.
                  APPEND wtab_msg TO itab_msg .
                  CLEAR wtab_msg.
                ENDIF.

                CONCATENATE 'DOCUMENTO ' wa_saida_02_temp-doc_lcti INTO msg_alv SEPARATED BY space.
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
              ELSEIF wa_saida_02_temp-status = icon_led_yellow.
                wtab_msg-name    = 'MENSAGEM:'.
                APPEND wtab_msg TO itab_msg .
                CLEAR wtab_msg.
*--------

                IF wa_saida_02_temp-obj_keyi IS NOT INITIAL.
                  SELECT DISTINCT *
                     FROM zib_contabil_err
                     INTO TABLE it_zib_contabil_err
                     WHERE obj_key  = wa_saida_02_temp-obj_keyi.
                ELSE.
                  REFRESH it_zib_contabil_err.
                ENDIF.

                IF wa_saida_02_temp-obj_keyj IS NOT INITIAL.
                  SELECT DISTINCT *
                     FROM zib_contabil_err
                     APPENDING TABLE it_zib_contabil_err
                     WHERE obj_key  = wa_saida_02_temp-obj_keyj.
                ELSE.
                  REFRESH it_zib_contabil_err.
                ENDIF.

                IF wa_saida_02_temp-obj_keyl IS NOT INITIAL.
                  SELECT DISTINCT *
                     FROM zib_contabil_err
                     APPENDING TABLE it_zib_contabil_err
                     WHERE obj_key  = wa_saida_02_temp-obj_keyl.
                ELSE.
                  REFRESH it_zib_contabil_err.
                ENDIF.

                IF it_zib_contabil_err[] IS NOT INITIAL.
                  wtab_msg-name    = 'MENSAGEM ERRO:'.
                  APPEND wtab_msg TO itab_msg .
                  CLEAR wtab_msg.
                  LOOP AT it_zib_contabil_err INTO wa_zib_contabil_err.
                    wtab_msg-name = wa_zib_contabil_err-message.
                    APPEND wtab_msg TO itab_msg .
                    CLEAR wtab_msg.
                  ENDLOOP.
                  wa_saida_02_temp-status = icon_led_red.
                  MODIFY it_saida_02_temp FROM wa_saida_02_temp INDEX e_row_id-index.
                ENDIF.
*--------
                IF wa_saida_02_temp-doc_lcti IS NOT INITIAL AND itab_msg[] IS INITIAL.
                  wtab_msg = 'AGUARDANDO PROCESSAMENTO DA ZIB_CONTABIL'.
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
              ELSEIF wa_saida_02_temp-status = icon_led_green.
                wtab_msg-name    = 'MENSAGEM:'.
                APPEND wtab_msg TO itab_msg .
                CLEAR wtab_msg.

                IF wa_saida_02_temp-doc_lcti IS NOT INITIAL.
                  wtab_msg = 'DOCUMENTO GERADO COM SUCESSO.'.
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

            WHEN 'DOC_S_INI'.

              IF wa_saida_02_temp-doc_s_ini IS NOT INITIAL.
                lv_belnr = wa_saida_02_temp-doc_s_ini.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-fl_venc_comp(4)."p_ano.
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'EST_S_INI'.

              IF wa_saida_02_temp-est_s_ini IS NOT INITIAL.
                lv_belnr = wa_saida_02_temp-est_s_ini.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-fl_venc_comp(4).
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'DOC_A_JUR'.

              IF wa_saida_02_temp-doc_a_jur IS NOT INITIAL.
                lv_belnr = wa_saida_02_temp-doc_a_jur.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-fl_venc_comp(4).
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'EST_A_JUR'.

              IF wa_saida_02_temp-est_a_jur IS NOT INITIAL.
                lv_belnr = wa_saida_02_temp-est_a_jur.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-fl_venc_comp(4).
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'DOC_LPCP'.
              IF wa_saida_02_temp-doc_lpcp IS NOT INITIAL.
                lv_belnr = wa_saida_02_temp-doc_lpcp.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-fl_venc_comp(4).
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'EST_LPCP'.
              IF wa_saida_02_temp-est_lpcp IS NOT INITIAL.
                lv_belnr = wa_saida_02_temp-est_lpcp.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-fl_venc_comp(4).
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'LT_I'.
              IF wa_saida_02_temp-lt_i IS NOT INITIAL.
                lw_lote-sign = 'I'.
                lw_lote-option = 'EQ'.
                lw_lote-low = wa_saida_02_temp-lt_i.
                APPEND lw_lote TO lr_lote.
                CLEAR lw_lote.

                SUBMIT zgl018   WITH p_bukrs    EQ wa_092-bukrs
                                WITH p_lote     IN lr_lote
                AND RETURN.

              ENDIF.

            WHEN 'LT_J'.
              IF wa_saida_02_temp-lt_j IS NOT INITIAL.
                lw_lote-sign = 'I'.
                lw_lote-option = 'EQ'.
                lw_lote-low = wa_saida_02_temp-lt_j.
                APPEND lw_lote TO lr_lote.
                CLEAR lw_lote.

                SUBMIT zgl018   WITH p_bukrs EQ wa_092-bukrs
                                WITH p_lote  IN lr_lote
                AND RETURN.

              ENDIF.

            WHEN 'LT_L'.
              IF wa_saida_02_temp-lt_l IS NOT INITIAL.
                lw_lote-sign = 'I'.
                lw_lote-option = 'EQ'.
                lw_lote-low = wa_saida_02_temp-lt_l.
                APPEND lw_lote TO lr_lote.
                CLEAR lw_lote.

                SUBMIT zgl018   WITH p_bukrs EQ wa_092-bukrs
                                WITH p_lote  IN lr_lote
                AND RETURN.

              ENDIF.
          ENDCASE.
        ENDIF.

      WHEN '05'.

        READ TABLE it_saida_05 INTO wa_saida_05 INDEX e_row_id-index.
        IF sy-subrc = 0.

          CASE e_column_id .
            WHEN 'STATUS'.

              IF wa_saida_05-status = icon_led_red .
                SELECT DISTINCT *
                   FROM zib_contabil_err
                   INTO TABLE it_zib_contabil_err
                   WHERE obj_key  = wa_saida_05-obj_key.

                wtab_msg-name    = 'MENSAGEM ERRO:'.
                APPEND wtab_msg TO itab_msg .
                CLEAR wtab_msg.
                LOOP AT it_zib_contabil_err INTO wa_zib_contabil_err.
                  wtab_msg-name = wa_zib_contabil_err-message.
                  APPEND wtab_msg TO itab_msg .
                  CLEAR wtab_msg.
                ENDLOOP.

                IF wa_saida_05-doc_lcto IS INITIAL.
                  wtab_msg-name = 'Doc. ainda não gerado!'.
                  APPEND wtab_msg TO itab_msg .
                  CLEAR wtab_msg.
                ENDIF.

                CONCATENATE 'DOCUMENTO ' wa_saida_05-doc_lcto INTO msg_alv SEPARATED BY space.
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
              ELSEIF wa_saida_05-status = icon_led_yellow.
                wtab_msg-name    = 'MENSAGEM:'.
                APPEND wtab_msg TO itab_msg .
                CLEAR wtab_msg.
*--------

                IF wa_saida_05-obj_key IS NOT INITIAL.
                  SELECT DISTINCT *
                     FROM zib_contabil_err
                     INTO TABLE it_zib_contabil_err
                     WHERE obj_key  = wa_saida_05-obj_key.
                ELSEIF wa_saida_05-obj_keyr IS NOT INITIAL.
                  SELECT DISTINCT *
                     FROM zib_contabil_err
                     APPENDING TABLE it_zib_contabil_err
                     WHERE obj_key  = wa_saida_05-obj_keyr.
                ELSE.
                  REFRESH it_zib_contabil_err.
                ENDIF.

*                IF wa_saida_02_temp-obj_keyj IS NOT INITIAL.
*                  SELECT *
*                     FROM zib_contabil_err
*                     APPENDING TABLE it_zib_contabil_err
*                     WHERE obj_key  = wa_saida_02_temp-obj_keyj.
*                ELSE.
*                  REFRESH it_zib_contabil_err.
*                ENDIF.
*
*                IF wa_saida_02_temp-obj_keyl IS NOT INITIAL.
*                  SELECT *
*                     FROM zib_contabil_err
*                     APPENDING TABLE it_zib_contabil_err
*                     WHERE obj_key  = wa_saida_02_temp-obj_keyl.
*                ELSE.
*                  REFRESH it_zib_contabil_err.
*                ENDIF.

                IF it_zib_contabil_err[] IS NOT INITIAL.
                  wtab_msg-name    = 'MENSAGEM ERRO:'.
                  APPEND wtab_msg TO itab_msg .
                  CLEAR wtab_msg.
                  LOOP AT it_zib_contabil_err INTO wa_zib_contabil_err.
                    wtab_msg-name = wa_zib_contabil_err-message.
                    APPEND wtab_msg TO itab_msg .
                    CLEAR wtab_msg.
                  ENDLOOP.
                  wa_saida_05-status = icon_led_red.
                  MODIFY it_saida_05 FROM wa_saida_05 INDEX e_row_id-index.
                ENDIF.
*--------
                IF wa_saida_05-doc_lcto IS NOT INITIAL AND itab_msg[] IS INITIAL.
                  wtab_msg = 'AGUARDANDO PROCESSAMENTO DA ZIB_CONTABIL'.
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
              ELSEIF wa_saida_05-status = icon_led_green.
                wtab_msg-name    = 'MENSAGEM:'.
                APPEND wtab_msg TO itab_msg .
                CLEAR wtab_msg.

                IF wa_saida_05-doc_lcto IS NOT INITIAL.
                  wtab_msg = 'DOCUMENTO GERADO COM SUCESSO.'.
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

            WHEN 'DOC_CONTABIL'.

              IF wa_saida_05-doc_contabil IS NOT INITIAL.
                lv_belnr = wa_saida_05-doc_contabil.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_05-dt_atualizacao(4)."p_ano.
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'DOC_ESTORNO'.

              IF wa_saida_05-doc_estorno IS NOT INITIAL.
                lv_belnr = wa_saida_05-doc_estorno.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_05-dt_atualizacao(4).
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'LT_DOC'.
              IF wa_saida_05-lt_doc IS NOT INITIAL.
                lw_lote-sign = 'I'.
                lw_lote-option = 'EQ'.
                lw_lote-low = wa_saida_05-lt_doc.
                APPEND lw_lote TO lr_lote.
                CLEAR lw_lote.

                SUBMIT zgl018   WITH p_bukrs    EQ wa_092-bukrs
                                WITH p_lote     IN lr_lote
                AND RETURN.

              ENDIF.

            WHEN 'DOC_CONTABILR'.

              IF wa_saida_05-doc_contabilr IS NOT INITIAL.
                lv_belnr = wa_saida_05-doc_contabilr.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrsp.
                SET PARAMETER ID 'GJR' FIELD wa_saida_05-dt_atualizacao(4)."p_ano.
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'DOC_ESTORNOR'.

              IF wa_saida_05-doc_estornor IS NOT INITIAL.
                lv_belnr = wa_saida_05-doc_estornor.
                SET PARAMETER ID 'BLN' FIELD lv_belnr.
                SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
                SET PARAMETER ID 'GJR' FIELD wa_saida_05-dt_atualizacao(4).
                CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
              ENDIF.

            WHEN 'LT_DOCR'.
              IF wa_saida_05-lt_docr IS NOT INITIAL.
                lw_lote-sign = 'I'.
                lw_lote-option = 'EQ'.
                lw_lote-low = wa_saida_05-lt_docr.
                APPEND lw_lote TO lr_lote.
                CLEAR lw_lote.

                SUBMIT zgl018   WITH p_bukrs    EQ wa_092-bukrsp
                                WITH p_lote     IN lr_lote
                AND RETURN.

              ENDIF.

          ENDCASE.
        ENDIF.

    ENDCASE.
    CALL METHOD g_alv->refresh_table_display( is_stable = wa_stable ).

  ENDMETHOD.

  METHOD on_data_changed.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname EQ 'VLR_SACA_BRL' OR
            fieldname EQ 'TAXA_02'.

      LOOP AT it_saida_03 INTO wa_saida_03.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.
          WHEN 'VLR_SACA_BRL'.
            wa_saida_03-vlr_saca_brl = wa_good_cells-value.
            MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_good_cells-row_id.

          WHEN 'TAXA_02'.
            wa_saida_03-taxa_02 = wa_good_cells-value.
            MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_good_cells-row_id.

        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    CALL METHOD g_alv->refresh_table_display( EXPORTING is_stable = wa_stable ).

  ENDMETHOD.

  METHOD handle_set_toolbar.

  ENDMETHOD.
  METHOD handle_user_command.

  ENDMETHOD.

  METHOD on_data_changed_finished.

    CLEAR: vlr_at_brl_ant,  vlr_at_usd_ant.


    LOOP AT  et_good_cells INTO DATA(wa_cells)
      WHERE fieldname EQ 'VLR_SACA_BRL' OR
            fieldname EQ 'TAXA_02'.

      LOOP AT it_saida_03 INTO wa_saida_03.

        CHECK wa_cells-row_id EQ sy-tabix.

        IF wa_saida_03-vlr_saca_brl IS NOT INITIAL AND wa_saida_03-taxa_02 IS NOT INITIAL.

          wa_saida_03-vlr_saca_usd   = ( wa_saida_03-vlr_saca_brl / wa_saida_03-taxa_02  ).
          wa_saida_03-vlr_atual_brl  = ( wa_saida_03-qnt_sacas_ms * wa_saida_03-vlr_saca_brl ).
          wa_saida_03-vlr_atual_usd  = ( wa_saida_03-qnt_sacas_ms * wa_saida_03-vlr_saca_usd ).


          CASE wa_saida_03-num_parcela.
            WHEN 1.
              vlr_at_brl_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_brl ).
              vlr_at_usd_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_usd ).

              wa_saida_03-variacao_m_brl  = vlr_at_brl_ant.
              wa_saida_03-variacao_m_usd  = vlr_at_usd_ant.

            WHEN OTHERS.

              DATA(num_parc_ant) = wa_saida_03-num_parcela - 1.

              READ TABLE it_calc INTO wa_calc WITH KEY num_parcela = num_parc_ant.
              IF sy-subrc = 0.
                vlr_at_brl_ant = wa_calc-vlr_at_brl_ant.
                vlr_at_usd_ant = wa_calc-vlr_at_usd_ant.
              ENDIF.

              wa_saida_03-variacao_m_brl  = ( wa_saida_03-vlr_atual_brl - vlr_at_brl_ant ).
              wa_saida_03-variacao_m_usd  = ( wa_saida_03-vlr_atual_usd - vlr_at_usd_ant ).

              CLEAR: vlr_at_brl_ant,  vlr_at_usd_ant.
              vlr_at_brl_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_brl ).
              vlr_at_usd_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_usd ).
          ENDCASE.

          MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_cells-row_id.

          wa_calc-num_parcela    = wa_saida_03-num_parcela.
          wa_calc-vlr_at_brl_ant = vlr_at_brl_ant.
          wa_calc-vlr_at_usd_ant = vlr_at_usd_ant.

          APPEND wa_calc TO it_calc.

        ENDIF.

        CLEAR wa_saida_03.
      ENDLOOP.
    ENDLOOP.

    CALL METHOD g_alv->refresh_table_display( EXPORTING is_stable = wa_stable ).
  ENDMETHOD.

ENDCLASS.


FORM select_dados.

  SELECT  SINGLE *
  FROM zglt092  INTO wa_092
  WHERE cod_contrato    EQ p_cont
    AND tp_arrendamento EQ p_tp_arr.

  IF sy-subrc EQ 0.

    SELECT DISTINCT *
      FROM zglt096
      INTO TABLE it_096
      WHERE cod_contrato EQ wa_092-cod_contrato.

    SELECT DISTINCT *
      FROM zglt094
      INTO TABLE it_094
      WHERE cod_contrato EQ wa_092-cod_contrato.

    SELECT DISTINCT *
      FROM zglt096f
      INTO TABLE it_096f
      WHERE cod_contrato EQ wa_092-cod_contrato.

    IF it_096f[] IS NOT INITIAL.
      SELECT *
         FROM zglt035
       INTO TABLE it_035
        FOR ALL ENTRIES IN it_096f
        WHERE doc_lcto EQ it_096f-doc_lcti
          AND bukrs    EQ wa_092-bukrs
          AND tp_lcto  EQ '0000000000'.

      SELECT *
         FROM zglt035
       APPENDING TABLE it_035
        FOR ALL ENTRIES IN it_096f
        WHERE doc_lcto EQ it_096f-doc_lctj
          AND bukrs    EQ wa_092-bukrs
          AND tp_lcto  EQ '0000000000'.

      SELECT *
         FROM zglt035
       APPENDING TABLE it_035
        FOR ALL ENTRIES IN it_096f
        WHERE doc_lcto EQ it_096f-doc_lctl
          AND bukrs    EQ wa_092-bukrs
          AND tp_lcto  EQ '0000000000'.
    ENDIF.

    "PSA
    SELECT DISTINCT * FROM zglt100 INTO TABLE it_zglt100
      WHERE regiao    EQ wa_092-municipio.
*            AND tp_arrendamento EQ wa_092-tp_arrendamento.

    SELECT DISTINCT *
       FROM zglt096g INTO TABLE it_096g
     WHERE  cod_contrato EQ wa_092-cod_contrato
       AND mes NE space.

    CASE p_tp_arr.
      WHEN '01' OR '02'.
        PERFORM consolidacao_de_dados.
        PERFORM f_atualizar_documento.
      WHEN '03'.
*          PERFORM tratar_dados_03 USING sy-subrc.
        PERFORM consolidacao_de_dados_03.
      WHEN '04'.
        PERFORM tratar_dados_04.
      WHEN '05'.
        PERFORM consolidacao_de_dados_05.
    ENDCASE.
  ENDIF.
ENDFORM.

FORM imprimi_dados.

  PERFORM z_monta_cabecalho.

*  IF g_custom_container01 IS INITIAL.
*
*    CREATE OBJECT g_custom_container01
*      EXPORTING
*        container_name = 'CONTAINER01'.
*
*    IF g_alv01 IS INITIAL AND g_custom_container01 IS NOT INITIAL.
*
*      CREATE OBJECT g_alv01
*        EXPORTING
*          i_parent = g_custom_container01.
*    ENDIF.
*
*    CASE p_tp_arr.
*      WHEN '01' OR '02'.
*
*        wa_layout-sel_mode   = 'A'.
*        wa_layout-stylefname = 'CELLSTYLES'.
*        wa_layout-ctab_fname = 'TCOLOR'.
*
*        CALL METHOD g_alv01->set_table_for_first_display
*          EXPORTING
*            is_layout       = wa_layout
*            i_save          = 'A'
*          CHANGING
*            it_outtab       = it_saida_02[]
*            it_fieldcatalog = it_fieldcat.
*
*      WHEN '03'.
*
*        wa_layout-stylefname = 'CELLTAB'.
*
*        SET HANDLER:  lcl_event_hander=>on_data_changed FOR g_alv01,
*                      lcl_event_hander=>on_data_changed_finished FOR g_alv01.
*
*        CALL METHOD g_alv01->set_table_for_first_display
*          EXPORTING
*            is_layout       = wa_layout
*            i_save          = 'A'
*          CHANGING
*            it_outtab       = it_saida_03[]
*            it_fieldcatalog = it_fieldcat.
*
*      WHEN '04'.
*
*        CALL METHOD g_alv01->set_table_for_first_display
*          EXPORTING
*            is_layout       = wa_layout
*            i_save          = 'A'
*          CHANGING
*            it_outtab       = it_saida_04[]
*            it_fieldcatalog = it_fieldcat.
*    ENDCASE.
*
*    CALL METHOD g_alv01->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    CALL METHOD g_alv01->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*  ELSE.
*    CALL METHOD g_alv01->refresh_table_display( is_stable = wa_stable ).
*  ENDIF.

ENDFORM.

FORM top-of-page.

  DATA: _dt_atual(10)   TYPE c,
        _dt_vig_de(10)  TYPE c,
        _dt_vig_ate(10) TYPE c.

  CLEAR: p_text_table.

  CASE p_tp_arr.
    WHEN '01' OR '02'.

      sdydo_text_element =  ' '.
      APPEND sdydo_text_element TO p_text_table.

      CONCATENATE 'Matric. do Contrato' wa_092-cod_contrato INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.

      CONCATENATE 'Empresa'  wa_092-bukrs   INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.

      CONCATENATE sy-datum+6(2) '-' sy-datum+4(2) '-'  sy-datum(4) INTO _dt_atual.
      CONCATENATE 'Data Atual' _dt_atual   INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.

      sdydo_text_element = ' '.
      APPEND sdydo_text_element TO p_text_table.

      WRITE wa_092-tx_usd_futuro TO tx.
      CONCATENATE 'USD Média Futuro'  tx   INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.

    WHEN '03' OR '04' OR '05'.

      sdydo_text_element =  ' '.
      APPEND sdydo_text_element TO p_text_table.

      CONCATENATE 'Empresa'  wa_092-bukrs   INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.

      CONCATENATE 'Fornecedor'  wa_092-fornecedor  INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.

      CONCATENATE wa_096-vigencia_de+6(2) '-' wa_096-vigencia_de+4(2) '-' wa_096-vigencia_de+0(4) INTO _dt_vig_de.
      CONCATENATE 'Vigência de ' _dt_vig_de  INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.

      CONCATENATE  wa_096-vigencia_ate+6(2) '-' wa_096-vigencia_ate+4(2) '-' wa_096-vigencia_ate+0(4)  INTO _dt_vig_ate.
      CONCATENATE 'Vigência Até' _dt_vig_ate INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.

      WRITE wa_092-tx_contrato TO tx.
      CONCATENATE 'Taxa Contrato'  tx   INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
  ENDCASE.
ENDFORM.
*

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat .

  REFRESH it_fieldcat.

  CASE p_tp_arr.
    WHEN '01' OR'02'.
      it_fieldcat =  VALUE lvc_t_fcat(
          ( fieldname = 'STATUS'              coltext = 'Status'                     outputlen = '05'     decimals_o  = ''   edit = ''  hotspot = 'X' )
          ( fieldname = 'FL_VENC_COMP'    coltext = 'Competência'                    outputlen = '15'     fix_column = 'X'    )
          ( fieldname = 'T_PAGAR_BRL'     coltext = 'Total a Pagar BRL'              outputlen = '15'     decimals_o  = 2  )
          ( fieldname = 'T_PAGAR_USD'     coltext = 'Total a Pagar USD'              outputlen = '15'     decimals_o  = 2  )
          ( fieldname = 'TAXA_AA'         coltext = 'Taxa AA'                        outputlen = '08'     decimals_o  = 9  )
          ( fieldname = 'TAXA_AM'         coltext = 'Taxa AM'                        outputlen = '08'     decimals_o  = 9  )
          ( fieldname = 'VP_VL_PRESENTE'  coltext = 'Valor Presente'                 outputlen = '10'     decimals_o  = 2  do_sum = abap_true )
          ( fieldname = 'VP_VL_JUROS'     coltext = 'Juros'                          outputlen = '12'     decimals_o  = 2  )
          ( fieldname = 'VP_VL_TOTAL'     coltext = 'Total'                          outputlen = '12'     decimals_o  = 2  )
          ( fieldname = 'BP_CIR_PRI '     coltext = 'B.P. Circulante'                outputlen = '20'     decimals_o  = 2  )
*          ( fieldname = 'BP_CIR_JUR '     coltext = 'B.P. Circulante Juros'          outputlen = '20'     decimals_o  = 2  )
          ( fieldname = 'BP_NCIR_PRI'     coltext = 'B.P. Não Circulante'            outputlen = '20'     decimals_o  = 2  )
*          ( fieldname = 'BP_NCIR_JUR'     coltext = 'B.P. Não Circulante Juros'      outputlen = '20'     decimals_o  = 2  )
          ( fieldname = 'BP_TOT_PRI '     coltext = 'B.P. Total'                     outputlen = '15'      decimals_o  = 2  )
          ( fieldname = 'APROV_JUR'       coltext = 'Aprop. Juros'                   outputlen = '15'      decimals_o  = 2  do_sum = abap_true )
          ( fieldname = 'TRANSFLPCP'      coltext = 'Transf.LP/CP'                   outputlen = '15'      decimals_o  = 2  do_sum = abap_true )
          ( fieldname = 'SALDO_CP'        coltext = 'Saldo CP'                       outputlen = '15'      decimals_o  = 2  do_sum = abap_true )
          ( fieldname = 'SALDO_LP'        coltext = 'Saldo LP'                       outputlen = '15'      decimals_o  = 2  do_sum = abap_true )
          ( fieldname = 'DOC_S_INI'       coltext = 'Doc.Saldo Inicial'              outputlen = '12' hotspot = 'X' )
          ( fieldname = 'EST_S_INI'       coltext = 'Est.Saldo Inicial'              outputlen = '12' hotspot = 'X' )
          ( fieldname = 'LT_I'            coltext = 'Lote Saldo Inicial'             outputlen = '12' hotspot = 'X' )
          ( fieldname = 'DOC_A_JUR'       coltext = 'Doc. Apro. Juros'               outputlen = '12' hotspot = 'X' )
          ( fieldname = 'EST_A_JUR'       coltext = 'Est. Apro. Juros'               outputlen = '12' hotspot = 'X' )
          ( fieldname = 'LT_J'            coltext = 'Lote Apro. Juros'               outputlen = '12' hotspot = 'X' )
          ( fieldname = 'DOC_LPCP'        coltext = 'Doc. LP/CP'                     outputlen = '12' hotspot = 'X' )
          ( fieldname = 'EST_LPCP'        coltext = 'Est. LP/CP'                     outputlen = '12' hotspot = 'X' )
          ( fieldname = 'LT_L'            coltext = 'Lote LP/CP'                     outputlen = '12' hotspot = 'X' )
          ).


*          ( fieldname = 'BP_TOT_JUR '     coltext = 'B.P. Total Juros'               outputlen = '12'     decimals_o  = 2  )
*          ( fieldname = 'JIRCP'           coltext = 'J.Imp. Resultado CP'            outputlen = '12'     decimals_o  = 2  )
*          ( fieldname = 'JIRLP'           coltext = 'J.Imp. Resultado LP'            outputlen = '12'     decimals_o  = 2  )
*          ( fieldname = 'JIRSO'           coltext = 'J.Imp. Resultado Soma'          outputlen = '12'     decimals_o  = 2  )


    WHEN '03'.
      it_fieldcat =  VALUE lvc_t_fcat(
          ( fieldname = 'STATUS'           coltext = 'Status'                    outputlen = '06'     decimals_o  = ''   edit = ''  hotspot = 'X' )
*          ( fieldname = 'NUM_PARCELA'      coltext = 'Parcela'                   outputlen = '07'                          edit = ''   )
*          ( fieldname = 'DT_ATUALIZACAO'   coltext = 'Data Atualização'          outputlen = '25'                          edit = ''   )
          ( fieldname = 'COMP'              coltext = 'Competência'               outputlen = '11'                          edit = ''   )
*          ( fieldname = 'QNT_SACAS'        coltext = 'Quant.Sacas'               outputlen = '10'       decimals_o  = 2    edit = ''   )
*          ( fieldname = 'QNT_SACAS'        coltext = 'Saca parcela anual'        outputlen = '10'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'PRECO_SACA_A'     coltext = 'Saca parcela anual'        outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
*          ( fieldname = 'QNT_SACAS_MS'     coltext = 'Quant.Sacas Mensal'        outputlen = '16'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VLR_P_M_SACAS'    coltext = 'Saca Parcela Mensal'       outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'SACA_ACUMULADA'   coltext = 'Saca Acumulada'            outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'PRECO_SACA'       coltext = 'Preço Saca Mês'            outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
*          ( fieldname = 'VLR_P_BRL'        coltext = 'Vlr Parcela BRL'           outputlen = '20'       decimals_o  = 2    edit = ''   )
*          ( fieldname = 'VLR_P_M_BRL'      coltext = 'Vlr Parcela Mensal BRL'    outputlen = '25'       decimals_o  = 2    edit = ''   )
*          ( fieldname = 'VLR_P_USD'        coltext = 'Vlr Parcela USD'           outputlen = '20'       decimals_o  = 2    edit = ''   )
*          ( fieldname = 'VLR_P_M_USD'      coltext = 'Vlr Parcela Mensal USD'    outputlen = '25'       decimals_o  = 2    edit = ''   )
*          ( fieldname = 'PRECO_SACA_BRL'   coltext = 'Preço Saca BRL'            outputlen = '14'       decimals_o  = 2    edit = ''   )
*          ( fieldname = 'TAXA_01'          coltext = 'Taxa'                      outputlen = '10'       decimals_o  = 2    edit = ''   )
*          ( fieldname = 'PRECO_SACA_USD'   coltext = 'Preço Saca USD'            outputlen = '14'       decimals_o  = 2    edit = ''   )
*          ( fieldname = 'VLR_SACA_BRL'     coltext = 'Vlr Saca BRL'              outputlen = '12'       decimals    = 2    edit = 'X'  inttype = 'F'  )
*          ( fieldname = 'TAXA_02'          coltext = 'Taxa'                      outputlen = '10'       decimals    = 4    edit = 'X'  inttype = 'F'  )
*          ( fieldname = 'VLR_SACA_USD'     coltext = 'Vlr Saca USD'              outputlen = '12'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'VLR_ATUAL_BRL'    coltext = 'Vlr Atualizado BRL'        outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'VARIACAO_M_BRL'   coltext = 'Variação Mensal BRL'       outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
*          ( fieldname = 'VLR_ATUAL_USD'    coltext = 'Vlr Atualizado USD'        outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'VARIACAO_M_USD'   coltext = 'Variação Mensal USD'       outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'PTAX'             coltext = 'PTAX'                      outputlen = '20'       decimals_o  = 4    edit = ''  do_sum = abap_true )
          ( fieldname = 'PIS_BRL'          coltext = 'PIS BRL'                   outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true ) "PSA
          ( fieldname = 'PIS_USD'          coltext = 'PIS USD'                   outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true ) "PSA
          ( fieldname = 'COFINS_BRL'       coltext = 'COFINS BRL'                outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true ) "PSA
          ( fieldname = 'COFINS_USD'       coltext = 'COFINS USD'                outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true ) "PSA
          ( fieldname = 'DOC_CONTABIL'     coltext = 'Doc. Contábil'             outputlen = '20'       decimals_o  = 2    edit = ''  hotspot = 'X' )
          ( fieldname = 'DOC_ESTORNO'      coltext = 'Doc. Estorno'              outputlen = '20'       decimals_o  = 2    edit = ''  hotspot = 'X' )
          ( fieldname = 'LT_DOC'           coltext = 'Lote'                      outputlen = '20'       decimals_o  = 2    edit = ''  hotspot = 'X' ) ).

    WHEN '04'.

      it_fieldcat01 =  VALUE lvc_t_fcat(
          ( fieldname = 'VLR_CONTRATO_BRL'   coltext  = 'Valor Contrato BRL'         outputlen = '15'      decimals_o  = 2 do_sum = abap_true )
          ( fieldname = 'VLR_CONTRATO_USD'   coltext  = 'Valor Contrato USD'         outputlen = '15'      decimals_o  = 2 do_sum = abap_true  ) ).

      it_fieldcat = VALUE lvc_t_fcat(
        ( fieldname = 'NUM_PARCELA'       coltext  = 'Parcela'                 outputlen = '07'                       )
        ( fieldname = 'PERIODO'           coltext  = 'Periodo'                 outputlen = '07'                       )
        ( fieldname = 'APROPRIACAO_BRL'   coltext  = 'Apropriação BRL'         outputlen = '15'      decimals_o  = 2  do_sum = abap_true )
        ( fieldname = 'APROPRIACAO_USD'   coltext  = 'Apropriação USD'         outputlen = '15'      decimals_o  = 2  do_sum = abap_true )
        ( fieldname = 'SALD_A_APRO_BLR'   coltext  = 'Saldo à Apropriar BRL'   outputlen = '20'      decimals_o  = 2  )
        ( fieldname = 'SALD_A_APRO_USD'   coltext  = 'Saldo à Apropriar USD'   outputlen = '20'      decimals_o  = 2  )
        ( fieldname = 'PIS'               coltext  = 'PIS'                     outputlen = '05'      decimals_o  = 2  do_sum = abap_true )
        ( fieldname = 'COFINS'            coltext  = 'COFINS'                  outputlen = '05'      decimals_o  = 2  do_sum = abap_true )
        ( fieldname = 'DOCUMENTO'         coltext  = 'Documento'               outputlen = '10'                         ) ).

    WHEN '05'.
      it_fieldcat =  VALUE lvc_t_fcat(
          ( fieldname = 'STATUS'           coltext = 'Status'                    outputlen = '06'     decimals_o  = ''   edit = ''  hotspot = 'X' )
          ( fieldname = 'COMP'              coltext = 'Competência'               outputlen = '11'                          edit = ''   )
          ( fieldname = 'VALOR_BRL'        coltext = 'Valor BRL'        outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'VALOR_USD'        coltext = 'Valor USD'        outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'PTAX'             coltext = 'PTAX'             outputlen = '20'       decimals_o  = 4    edit = ''  do_sum = abap_true )
          ( fieldname = 'PIS_BRL'          coltext = 'PIS BRL'          outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'PIS_USD'          coltext = 'PIS USD'          outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'COFINS_BRL'       coltext = 'COFINS BRL'       outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'COFINS_USD'       coltext = 'COFINS USD'       outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'PIS_ARREND'       coltext = 'PIS BRL Arrendante'          outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'PIS_U_ARREND'     coltext = 'PIS USD Arrendante'          outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'COFINS_ARREND'    coltext = 'COFINS BRL Arrendante'       outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'COFINS_U_ARREND'  coltext = 'COFINS USD Arrendante'       outputlen = '20'       decimals_o  = 2    edit = ''  do_sum = abap_true )
          ( fieldname = 'DOC_CONTABIL'     coltext = 'Doc. Despesa'             outputlen = '20'       decimals_o  = 2    edit = ''  hotspot = 'X' )
          ( fieldname = 'DOC_ESTORNO'      coltext = 'Est. Despesa'              outputlen = '20'       decimals_o  = 2    edit = ''  hotspot = 'X' )
          ( fieldname = 'LT_DOC'           coltext = 'Lote Despesa'                      outputlen = '20'       decimals_o  = 2    edit = ''  hotspot = 'X' )
          ( fieldname = 'DOC_CONTABILR'     coltext = 'Doc. Receita'             outputlen = '20'       decimals_o  = 2    edit = ''  hotspot = 'X' )
          ( fieldname = 'DOC_ESTORNOR'      coltext = 'Est. Receita'              outputlen = '20'       decimals_o  = 2    edit = ''  hotspot = 'X' )
          ( fieldname = 'LT_DOCR'           coltext = 'Lote Receita'                      outputlen = '20'       decimals_o  = 2    edit = ''  hotspot = 'X' )
          ).

  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONSOLIDACAO_DE_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consolidacao_de_dados.

  DATA: tabix        TYPE sy-tabix,
        lv_cont      TYPE i,
        lv_comp      TYPE sy-datum,
        v_ultimo_dia TYPE sy-datum,
        lv_saldocp   TYPE zgle0004,
        lv_soma      TYPE zgle0004,
        vl_jirso     TYPE zgle0004.

  TRY .
      DATA(dt_vigencia) = it_096[ 1 ]-vigencia_de.
    CATCH cx_sy_itab_line_not_found.
      CLEAR dt_vigencia.
  ENDTRY.

  CHECK dt_vigencia IS NOT INITIAL.

  dt_vigencia = |{ dt_vigencia(6) }01|.

  PERFORM interval_date USING '-' CHANGING dt_vigencia.

  SORT it_094 BY dt_vencimento DESCENDING.

  TRY .
      DATA(dt_vencimento) = it_094[ 1 ]-dt_vencimento.
    CATCH cx_sy_itab_line_not_found.
      CLEAR dt_vencimento.
  ENDTRY.

  CONCATENATE p_ano p_mes '01' INTO lv_comp.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_comp
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  DO.

    IF dt_vigencia >= dt_vencimento.
      EXIT.
    ENDIF.

    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = dt_vigencia
      IMPORTING
        e_date = dt_vigencia.

    APPEND
    VALUE #(
              fl_venc_comp  = dt_vigencia
           ) TO it_saida_02.

    dt_vigencia = |{ dt_vigencia(6) }01|.

    PERFORM interval_date USING '+' CHANGING dt_vigencia.

  ENDDO.

* Data wa_saida_02-fl_venc_comp = <fs_096>-vigencia_de.
  READ TABLE it_096 ASSIGNING FIELD-SYMBOL(<fs_096>) INDEX 1.
  IF sy-subrc IS INITIAL.
    READ TABLE it_saida_02 INTO DATA(wa_saida_02) INDEX 1.
    IF sy-subrc IS INITIAL.
      wa_saida_02-fl_venc_comp = <fs_096>-vigencia_de.
      MODIFY it_saida_02 FROM wa_saida_02 INDEX 1.
    ENDIF.
  ENDIF.

  LOOP AT it_saida_02 ASSIGNING FIELD-SYMBOL(<f_saida>).

    <f_saida>-taxa_aa     = wa_092-tx_juros_desc.

    READ TABLE it_094 INTO DATA(wa_094) WITH KEY dt_vencimento(6) = <f_saida>-fl_venc_comp(6).
    IF sy-subrc IS INITIAL.
      <f_saida>-vl_parcela = wa_094-valor_brl.
    ELSE.
      <f_saida>-vl_parcela = 0.
    ENDIF.

  ENDLOOP.

  LOOP AT it_saida_02 ASSIGNING <f_saida>.
    tabix = sy-tabix.

    PERFORM valorbrl USING <f_saida>-fl_venc_comp CHANGING <f_saida>-t_pagar_brl.
    <f_saida>-t_pagar_usd = <f_saida>-t_pagar_brl / wa_092-tx_usd_futuro.

    taxa = ( ( ( ( <f_saida>-taxa_aa / 100 ) ) + 1 ) ** ( 1 / 12 ) - 1 ).
    PERFORM valor_presente USING taxa <f_saida>-fl_venc_comp CHANGING <f_saida>-vp_vl_presente.

    <f_saida>-taxa_am     = ( ( ( <f_saida>-taxa_aa / 100 ) + 1 ) ** ( 1 / 12 ) - 1 ) * 100.

    <f_saida>-vp_vl_juros    = <f_saida>-t_pagar_brl - <f_saida>-vp_vl_presente.
    <f_saida>-vp_vl_total    = <f_saida>-vp_vl_presente + <f_saida>-vp_vl_juros.

    PERFORM bp_cir_pri USING <f_saida>-fl_venc_comp CHANGING <f_saida>-bp_cir_pri.
    PERFORM bp_cir_jur USING <f_saida>-fl_venc_comp CHANGING <f_saida>-bp_cir_jur.

    <f_saida>-bp_cir_jur = <f_saida>-bp_cir_jur - <f_saida>-bp_cir_pri.

    <f_saida>-bp_ncir_pri    = <f_saida>-t_pagar_brl - <f_saida>-bp_cir_pri.
    <f_saida>-bp_ncir_jur    = - <f_saida>-vp_vl_juros - <f_saida>-bp_cir_jur.
    <f_saida>-bp_tot_pri     = <f_saida>-bp_cir_pri + <f_saida>-bp_ncir_pri.
    <f_saida>-bp_tot_jur     = <f_saida>-bp_cir_jur + <f_saida>-bp_ncir_jur.

    IF tabix NE 1.
      <f_saida>-jircp          = abs( <f_saida>-bp_cir_jur ) - abs( it_saida_02[ tabix - 1 ]-bp_cir_jur ).
      <f_saida>-jirlp          = abs( <f_saida>-bp_ncir_jur ) - abs( it_saida_02[ tabix - 1 ]-bp_ncir_jur ).
      ADD <f_saida>-jircp TO vl_jirso.
      ADD <f_saida>-jirlp TO vl_jirso.
      <f_saida>-jirso          = vl_jirso.
    ENDIF.

* RJF
*         aprop_jur      TYPE zgle0004, "01 Aprov Juros
*         transflpcp     TYPE zgle0004, "01 Transf.LP/CP
*         Aprop. Juros = ( - Valor Presente Juros linha atual + Valor Presente Juros linha anterior )
*         Trans.. LP/CP  = Valor  Aprop. Juros mês atual + 12 meses

    READ TABLE it_saida_02 ASSIGNING FIELD-SYMBOL(<f_saidaa>) INDEX ( tabix - 1 ).
    IF sy-subrc IS INITIAL.
      <f_saida>-aprov_jur = ( - <f_saida>-vp_vl_juros + <f_saidaa>-vp_vl_juros ).
    ELSE.
      CLEAR <f_saida>-aprov_jur.
    ENDIF.

    <f_saida>-aprov_jur  = <f_saida>-aprov_jur.

    READ TABLE it_saida_02 ASSIGNING <f_saidaa> INDEX ( tabix + 11 ).

    IF sy-subrc IS INITIAL.
      <f_saida>-transflpcp = <f_saida>-aprov_jur + <f_saidaa>-aprov_jur.
    ELSE.
      <f_saida>-transflpcp = <f_saida>-aprov_jur.
    ENDIF.
*    PERFORM cores USING tabix CHANGING <f_saida>.

    IF <f_saida>-fl_venc_comp(6) EQ v_ultimo_dia(6).
      <f_saida>-color = 'C300'.
    ENDIF.

  ENDLOOP.

  LOOP AT it_saida_02 ASSIGNING <f_saida>.
    LOOP AT it_saida_02 ASSIGNING FIELD-SYMBOL(<f_saidai>) FROM ( sy-tabix + 1 ).
      IF lv_cont LE 11.
        lv_cont = lv_cont + 1.
        lv_soma = <f_saidai>-aprov_jur.
      ELSEIF lv_cont GT 11.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_cont GT 11 AND lv_soma IS NOT INITIAL.
      <f_saida>-transflpcp = lv_soma.
      CLEAR: lv_soma, lv_cont.

    ELSE.
      CLEAR: lv_soma, lv_cont, <f_saida>-transflpcp.
    ENDIF.
*---------------------------------

**          CONCATENATE 'ZGL17' i_doc_lcto i_ano_lcto INTO v_objkey.
*          wa_saida_02_temp-obj_key  = wa_zglt096e-obj_key.
*          wa_saida_02_temp-obj_keyd = wa_zglt096e-obj_keyd.
*
*          SELECT SINGLE *
*            FROM zib_contabil_chv
*            INTO wa_zibchv
*           WHERE obj_key = wa_zglt096e-obj_key.
*
*          SELECT SINGLE *
*            FROM zib_contabil_chv
*            INTO wa_zibchvd
*           WHERE obj_key = wa_zglt096e-obj_keyd.
*
*          IF ( sy-subrc IS NOT INITIAL ).
*
*            SELECT SINGLE *
*              FROM zib_contabil_err
*              INTO wa_ziberr
*             WHERE obj_key = wa_zglt096e-obj_key.
*
*          ENDIF.

*--------------------------------

*    IF <f_saida>-doc_s_ini IS INITIAL AND <f_saida>-doc_a_jur IS INITIAL AND <f_saida>-doc_lpcp IS INITIAL.
*      <f_saida>-status = icon_led_yellow.
*    ELSE
    IF <f_saida>-doc_s_ini IS NOT INITIAL AND <f_saida>-doc_a_jur IS NOT INITIAL AND <f_saida>-doc_lpcp IS NOT INITIAL.
      <f_saida>-status = icon_led_green.
    ELSE.
      <f_saida>-status = icon_led_yellow.
    ENDIF.

    READ TABLE it_096f INTO wa_096f WITH KEY cod_contrato = p_cont
                                             fl_venc_comp = <f_saida>-fl_venc_comp.
    IF sy-subrc IS INITIAL.

      READ TABLE it_035 INTO DATA(wa_035) WITH KEY doc_lcto = wa_096f-doc_lcti.
      IF sy-subrc IS INITIAL.
        <f_saida>-lt_i = wa_035-lote.
      ENDIF.

      READ TABLE it_035 INTO DATA(wa_035j) WITH KEY doc_lcto = wa_096f-doc_lctj.
      IF sy-subrc IS INITIAL.
        <f_saida>-lt_j = wa_035j-lote.
      ENDIF.

      READ TABLE it_035 INTO DATA(wa_035l) WITH KEY doc_lcto = wa_096f-doc_lctl.
      IF sy-subrc IS INITIAL.
        <f_saida>-lt_l = wa_035l-lote.
      ENDIF.

      <f_saida>-obj_keyi     =  wa_096f-obj_keyi.
      <f_saida>-obj_keyj     =  wa_096f-obj_keyj.
      <f_saida>-obj_keyl     =  wa_096f-obj_keyl.
      <f_saida>-doc_lcti     =  wa_096f-doc_lcti.
      <f_saida>-doc_lctj     =  wa_096f-doc_lctj.
      <f_saida>-doc_lctl     =  wa_096f-doc_lctl.
      <f_saida>-doc_s_ini    =  wa_096f-doc_s_ini.
      <f_saida>-est_s_ini    =  wa_096f-est_s_ini.
      <f_saida>-doc_a_jur    =  wa_096f-doc_a_jur.
      <f_saida>-est_a_jur    =  wa_096f-est_a_jur .
      <f_saida>-doc_lpcp     =  wa_096f-doc_lpcp.
      <f_saida>-est_lpcp     =  wa_096f-est_lpcp.
    ENDIF.

  ENDLOOP.

  LOOP AT it_saida_02 ASSIGNING <f_saida>.
    IF sy-tabix EQ 1.
      LOOP AT it_saida_02 ASSIGNING <f_saidaa>.
        IF sy-tabix GT 1 AND sy-tabix LE 13.
          lv_saldocp = lv_saldocp + <f_saidaa>-aprov_jur.
        ELSEIF sy-tabix GT 13.
          EXIT.
        ENDIF.
      ENDLOOP.
      <f_saida>-saldo_cp = lv_saldocp.

    ELSE. " Demais comp
      IF sy-tabix GT 1.
        <f_saida>-saldo_cp = lv_saldocp - <f_saida>-aprov_jur + <f_saida>-transflpcp.
        lv_saldocp = <f_saida>-saldo_cp.
      ENDIF.
    ENDIF.
*    CLEAR lv_saldocp.
*    <f_saida>-saldo_cp = lv_saldocp.
*    CLEAR lv_saldocp.
    <f_saida>-saldo_lp = <f_saida>-vp_vl_juros - <f_saida>-saldo_cp.
  ENDLOOP.
  CLEAR lv_saldocp.

* Lines uptate
  SELECT DISTINCT *
    FROM zglt096f
    INTO TABLE it_096fl
    FOR ALL ENTRIES IN it_saida_02
    WHERE cod_contrato  EQ p_cont
      AND fl_venc_comp  EQ it_saida_02-fl_venc_comp.

  IF sy-subrc IS INITIAL AND it_096fl[] IS NOT INITIAL.

    SELECT DISTINCT *
    FROM zib_contabil_err
    INTO TABLE @DATA(it_ziberr)
    FOR ALL ENTRIES IN @it_096fl
    WHERE obj_key EQ @it_096fl-obj_keyi.

    SELECT DISTINCT *
    FROM zib_contabil_err
    APPENDING TABLE @it_ziberr
    FOR ALL ENTRIES IN @it_096fl
    WHERE obj_key EQ @it_096fl-obj_keyj.

    SELECT DISTINCT *
    FROM zib_contabil_err
    APPENDING TABLE @it_ziberr
    FOR ALL ENTRIES IN @it_096fl
    WHERE obj_key EQ @it_096fl-obj_keyl.

    LOOP AT it_saida_02 ASSIGNING <f_saida>.
      DATA(lv_tabix) = sy-tabix.
      READ TABLE it_096fl INTO wa_096fl WITH KEY fl_venc_comp = <f_saida>-fl_venc_comp.

      IF sy-subrc IS INITIAL.
        IF wa_096fl-obj_keyi IS NOT INITIAL.
          READ TABLE it_ziberr INTO DATA(wa_ziberri) WITH KEY obj_key = wa_096fl-obj_keyi.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_ziberri.
          ENDIF.
        ENDIF.

        IF wa_096fl-obj_keyj IS NOT INITIAL.
          READ TABLE it_ziberr INTO DATA(wa_ziberrj) WITH KEY obj_key = wa_096fl-obj_keyj.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_ziberrj.
          ENDIF.
        ENDIF.

        IF wa_096fl-obj_keyl IS NOT INITIAL.
          READ TABLE it_ziberr INTO DATA(wa_ziberrl) WITH KEY obj_key = wa_096fl-obj_keyl.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_ziberrl.
          ENDIF.
        ENDIF.

      ELSE.
        CLEAR: wa_096fl, wa_ziberri, wa_ziberrj, wa_ziberrl.
      ENDIF.

      IF wa_096fl-doc_s_ini IS NOT INITIAL AND wa_096fl-doc_a_jur IS INITIAL AND wa_096fl-doc_lpcp IS INITIAL " SI
        AND ( wa_ziberri-message IS INITIAL AND wa_ziberrj-message IS INITIAL AND wa_ziberrl-message IS INITIAL ).
        <f_saida> = CORRESPONDING #( wa_096fl ).
        <f_saida>-status = icon_led_green.
      ELSEIF wa_096fl-doc_s_ini IS INITIAL AND wa_096fl-doc_a_jur IS NOT INITIAL AND wa_096fl-doc_lpcp IS NOT INITIAL " NSI
        AND ( wa_ziberri-message IS INITIAL AND wa_ziberrj-message IS INITIAL AND wa_ziberrl-message IS INITIAL ).
        <f_saida> = CORRESPONDING #( wa_096fl ).
        <f_saida>-status = icon_led_green.
      ELSEIF wa_096fl-doc_s_ini IS INITIAL AND wa_096fl-doc_a_jur IS INITIAL OR wa_096fl-doc_lpcp IS INITIAL
        AND ( wa_ziberri-message IS INITIAL AND wa_ziberrj-message IS INITIAL AND wa_ziberrl-message IS INITIAL  ).
        <f_saida>-status = icon_led_yellow.
      ELSEIF wa_096fl-doc_s_ini IS INITIAL AND wa_096fl-doc_a_jur IS NOT INITIAL OR wa_096fl-doc_lpcp IS NOT INITIAL
        AND ( wa_ziberri-message IS NOT INITIAL OR wa_ziberrj-message IS NOT INITIAL OR wa_ziberrl-message IS NOT INITIAL ).
        <f_saida> = CORRESPONDING #( wa_096fl ).
        <f_saida>-status = icon_led_red.
      ELSEIF wa_096fl-doc_s_ini IS NOT INITIAL OR wa_096fl-doc_a_jur IS INITIAL OR wa_096fl-doc_lpcp IS NOT INITIAL
        AND ( wa_ziberri-message IS NOT INITIAL OR wa_ziberrj-message IS NOT INITIAL OR wa_ziberrl-message IS NOT INITIAL ).
        <f_saida> = CORRESPONDING #( wa_096fl ).
        <f_saida>-status = icon_led_red.
      ELSEIF wa_096fl-doc_s_ini IS INITIAL AND wa_096fl-doc_a_jur IS INITIAL AND wa_096fl-doc_lpcp IS INITIAL
        AND ( wa_ziberri-message IS NOT INITIAL OR wa_ziberrj-message IS NOT INITIAL OR wa_ziberrl-message IS NOT INITIAL ).
        <f_saida>-status = icon_led_red.
      ELSEIF ( wa_096fl-est_s_ini IS NOT INITIAL OR wa_096fl-est_a_jur IS NOT INITIAL OR wa_096fl-est_lpcp IS NOT INITIAL )
        AND ( wa_ziberri-message IS INITIAL AND wa_ziberrj-message IS INITIAL AND wa_ziberrl-message IS INITIAL ).
        <f_saida>-est_s_ini = wa_096fl-est_s_ini.
        <f_saida>-est_a_jur = wa_096fl-est_a_jur.
        <f_saida>-est_lpcp = wa_096fl-est_lpcp.
        <f_saida>-status = icon_led_yellow.
      ELSEIF wa_096fl-doc_s_ini IS INITIAL AND wa_096fl-doc_a_jur IS INITIAL AND wa_096fl-doc_lpcp IS INITIAL
        AND ( wa_ziberri-message IS INITIAL AND wa_ziberrj-message IS INITIAL AND wa_ziberrl-message IS INITIAL ).
        <f_saida>-status = icon_led_yellow.
      ENDIF.
    ENDLOOP.
  ENDIF.

  READ TABLE it_saida_02 INTO wa_saida_02 INDEX 1.
  IF sy-subrc IS INITIAL.
    CLEAR: wa_saida_02-aprov_jur,
           wa_saida_02-transflpcp.
    MODIFY it_saida_02 FROM wa_saida_02 INDEX 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALOR_PRESENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valor_presente USING taxa_am
                          fl_venc_comp
                    CHANGING vp_vl_presente.

  DATA num_parcelas TYPE sy-tabix.

  num_parcelas = 0.

  LOOP AT it_saida_02 INTO DATA(wa_saida).
    IF wa_saida-fl_venc_comp <= fl_venc_comp.
    ELSE.
      ADD 1 TO num_parcelas.
      vp_vl_presente = vp_vl_presente + ( wa_saida-vl_parcela / ( ( 1 + taxa_am ) ** num_parcelas ) ).
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INTERVAL_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0581   text
*      <--P_DT_VIGENCIA  text
*----------------------------------------------------------------------*
FORM interval_date  USING    vl_sinal
                    CHANGING vl_dt_vigencia.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = vl_dt_vigencia
      days      = '00'
      months    = '01'
      signum    = vl_sinal
      years     = '00'
    IMPORTING
      calc_date = vl_dt_vigencia.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALORBRL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<F_SAIDA>_T_PAGAR_BRL  text
*----------------------------------------------------------------------*
FORM valorbrl  USING fl_venc_comp CHANGING p_pagar_brl.
  LOOP AT it_saida_02 INTO DATA(wa_saida).
    IF wa_saida-fl_venc_comp(6) <= fl_venc_comp(6).
    ELSE.
      ADD wa_saida-vl_parcela TO p_pagar_brl.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BP_CIR_PRI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<F_SAIDA>_BP_CIR_PRI  text
*----------------------------------------------------------------------*
FORM bp_cir_pri USING fl_venc_comp CHANGING p_bp_cir_pri.
  DATA dt_venc TYPE sy-datum.

  dt_venc = fl_venc_comp + 365.

  LOOP AT it_saida_02 INTO DATA(wa_saida).
    IF fl_venc_comp < wa_saida-fl_venc_comp.
      IF dt_venc(6) > wa_saida-fl_venc_comp(6).
        ADD wa_saida-vl_parcela TO p_bp_cir_pri.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BP_CIR_JUR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<F_SAIDA>_FL_VENC_COMP  text
*      <--P_<F_SAIDA>_BP_CIR_JUR  text
*----------------------------------------------------------------------*
FORM bp_cir_jur  USING    p_fl_venc_comp
                 CHANGING p_bp_cir_jur.

  DATA num_parcelas TYPE sy-tabix.
  DATA dt_venc TYPE sy-datum.

  num_parcelas = 0.
  dt_venc = p_fl_venc_comp + 365.

  LOOP AT it_saida_02 INTO DATA(wa_saida).
    IF p_fl_venc_comp < wa_saida-fl_venc_comp.
      IF dt_venc(6) > wa_saida-fl_venc_comp(6).

        ADD 1 TO num_parcelas.
        p_bp_cir_jur = p_bp_cir_jur + ( wa_saida-vl_parcela / ( 1 + taxa ) ** num_parcelas ).

        IF wa_saida-vl_parcela  IS NOT INITIAL.
          num_parcelas = 0.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.
  p_bp_cir_jur = p_bp_cir_jur - ( taxa / 100 ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TABIX  text
*      <--P_<F_SAIDA>_TCOLOR  text
*----------------------------------------------------------------------*
FORM cores  USING    p_tabix
            CHANGING p_saida TYPE ty_saida_02.

  FIELD-SYMBOLS: <fs_campo>  TYPE any,
                 <fs_campo1> TYPE any.
  DATA campo      TYPE char20.

  LOOP AT it_fieldcat INTO DATA(wa_cat).
  ENDLOOP.

  IF p_tabix EQ 1.
    LOOP AT it_fieldcat INTO wa_cat.
      campo = |P_SAIDA-{ wa_cat-fieldname }|.
      ASSIGN (campo) TO <fs_campo1>.
      IF <fs_campo1> < 0.
        APPEND VALUE #( fieldname = wa_cat-fieldname color-col = 6 color-int = 0 color-inv = 1 ) TO p_saida-tcolor.
      ELSE.
        APPEND VALUE #( fieldname = wa_cat-fieldname color-col = 4 color-int = 1 color-inv = 0 ) TO p_saida-tcolor.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT it_fieldcat INTO wa_cat.
      campo = |P_SAIDA-{ wa_cat-fieldname }|.
      ASSIGN (campo) TO <fs_campo1>.

      IF <fs_campo1> < 0.
        APPEND VALUE #( fieldname = wa_cat-fieldname color-col = 6 color-int = 0 color-inv = 1 ) TO p_saida-tcolor.
*      ELSE.
*        APPEND VALUE #( FIELDNAME = WA_CAT-FIELDNAME COLOR-COL = P_TABIX COLOR-INT = 0 COLOR-INV = 0 ) TO P_SAIDA-TCOLOR.
      ENDIF.

    ENDLOOP.

    APPEND VALUE #( fieldname = 'FL_VENC_COMP' color-col = 4 color-int = 0 color-inv = 0 ) TO p_saida-tcolor.

    APPEND VALUE #( fieldname = 'T_PAGAR_BRL' color-col = 7 color-int = 0 color-inv = 0 ) TO p_saida-tcolor.
    APPEND VALUE #( fieldname = 'T_PAGAR_USD' color-col = 7 color-int = 0 color-inv = 0 ) TO p_saida-tcolor.
    APPEND VALUE #( fieldname = 'TAXA_AA' color-col = 7 color-int = 0 color-inv = 0 ) TO p_saida-tcolor.
    APPEND VALUE #( fieldname = 'TAXA_AM' color-col = 7 color-int = 0 color-inv = 0 ) TO p_saida-tcolor.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRATAR_DADOS_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tratar_dados_03 USING p_subrc TYPE sy-subrc.

  DATA: e_max_days     TYPE  i,
        _days          LIKE t5a4a-dlydy,
        _month         TYPE  i,
        _year          TYPE  i,
        _dt_atual      TYPE sy-datum,
        _dt_vencimento TYPE sy-datum,
        _num_parcela   TYPE i VALUE 0.

  CLEAR: vlr_at_brl_ant,  vlr_at_usd_ant.

  CASE p_subrc.
    WHEN 4.
      REFRESH gt_estilo01.

      _dt_vencimento = p_dtvenc.

      DO 12 TIMES.

        _num_parcela = _num_parcela + 1.
        _month = _dt_vencimento+4(2).
        _year  = _dt_vencimento+0(4).

        CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
          EXPORTING
            i_date_month = _month
            i_date_year  = _year
          IMPORTING
            e_max_days   = e_max_days.

        _days  =    e_max_days.

        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = _dt_vencimento
            days      = _days
            months    = '00'
            signum    = '+'
            years     = '00'
          IMPORTING
            calc_date = _dt_atual.


        READ TABLE it_zglt100 INTO DATA(wa_zglt100) WITH KEY
                                                            "cod_contrato    = p_cont
                                                             regiao          = wa_092-municipio
                                                             competencia(6)  = p_dtvenc(6).

        "131204 ZGL071 - Arrendamento - PSA TIPO 3
        CLEAR: wa_zglt100-ptax,vlr_out_taxa.
        PERFORM pega_taxa USING _dt_atual CHANGING vlr_out_taxa.
        wa_zglt100-ptax = vlr_out_taxa.


        "SACA ACUMULADA VERIFICAR SE ESTA GERANDO O VALOR - 131204 ZGL071 - Arrendamento  PSA
*        IF sy-subrc IS INITIAL AND wa_zglt100-saca_acumulada IS NOT INITIAL.
*          wa_saida_03-saca_acumulada  =  wa_zglt100-saca_acumulada.
*          wa_saida_03-preco_saca      =  wa_zglt100-preco_saca. " Preço saca mes
*        ELSE.

        READ TABLE it_saida_03 INTO DATA(wa_saida_03_ant) WITH KEY num_parcela = wa_saida_03-num_parcela - 1.
        IF sy-subrc IS INITIAL.
          wa_saida_03-saca_acumulada    = wa_saida_03-qnt_sacas_ms + wa_saida_03_ant-saca_acumulada.
        ENDIF.
*        ENDIF.

        wa_saida_03-vlr_atual_brl = wa_saida_03-saca_acumulada * wa_saida_03-preco_saca. " 'Vlr Atualizado BRL'

        READ TABLE it_096 INTO  wa_096 WITH KEY cod_contrato = p_cont
                                                vigencia_de  = p_dtvenc.

        IF sy-subrc = 0.
          wa_saida_03-qnt_sacas       = wa_096-sacas_p_ano.                                    " 'Quant.Sacas' - Saca parcela anual
          wa_saida_03-qnt_sacas_ms    = ( ( wa_saida_03-qnt_sacas / 12 )    *  _num_parcela ). " 'Quant.Sacas Mensal'
*          wa_saida_03-vlr_p_m_sacas   = ( ( wa_saida_03-qnt_sacas_ms / 12 ) *  _num_parcela )." 'Vlr Parcela Mensal Sacas' - Saca parcela mensal
          wa_saida_03-vlr_p_m_sacas   = ( wa_saida_03-qnt_sacas / 12 ).                        " Saca parcela mensal
          wa_saida_03-vlr_p_brl       = ( wa_saida_03-qnt_sacas * wa_096-preco_sacas ).        " 'Vlr Parcela BRL'
          wa_saida_03-vlr_p_m_brl     = ( ( wa_saida_03-vlr_p_brl / 12 ) * _num_parcela ).     " 'Vlr Parcela Mensal BRL'
          wa_saida_03-vlr_p_usd       = ( wa_saida_03-vlr_p_brl /  wa_092-tx_contrato ).       " 'Vlr Parcela USD'
          wa_saida_03-vlr_p_m_usd     = ( ( wa_saida_03-vlr_p_usd / 12 ) * _num_parcela ).     " 'Vlr Parcela Mensal USD'
          wa_saida_03-preco_saca_brl  = wa_096-preco_sacas.                                    " 'Preço Saca BRL'
          wa_saida_03-taxa_01         = ( wa_saida_03-vlr_p_brl / wa_saida_03-vlr_p_usd  ).    " 'Taxa'
          wa_saida_03-preco_saca_usd  = ( wa_saida_03-vlr_p_usd / wa_saida_03-qnt_sacas ).     " 'Preço Saca USD'
          "WA_SAIDA_03-VLR_SACA_BRL    =  '62.00'.                                             " 'Vlr Saca BRL'
          "WA_SAIDA_03-TAXA_02         =  '3.9407'.                                            " 'Taxa' 2
          wa_saida_03-vlr_saca_usd    =  COND #( WHEN wa_saida_03-vlr_saca_brl / wa_saida_03-taxa_02 < 0  " 'Vlr Saca USD'
                                         THEN  0
                                         ELSE wa_saida_03-vlr_saca_brl / wa_saida_03-taxa_02  ).

*          wa_saida_03-vlr_atual_brl   =  ( wa_saida_03-qnt_sacas_ms * wa_saida_03-vlr_saca_brl ). " 'Vlr Atualizado BRL'


          wa_saida_03-vlr_atual_usd   =  ( wa_saida_03-qnt_sacas_ms * wa_saida_03-vlr_saca_usd ). " 'Vlr Atualizado USD'


          READ TABLE it_saida_03 INTO wa_saida_03_ant WITH KEY num_parcela = wa_saida_03-num_parcela - 1.
          IF sy-subrc IS INITIAL AND wa_saida_03_ant-vlr_atual_brl IS NOT INITIAL.
            wa_saida_03-variacao_m_brl  = wa_saida_03-vlr_atual_brl - wa_saida_03_ant-vlr_atual_brl.
          ELSE.
            wa_saida_03-variacao_m_brl  = wa_saida_03-vlr_atual_brl.
          ENDIF.

          IF wa_zglt100-ptax IS NOT INITIAL.
            wa_saida_03-variacao_m_usd  = wa_saida_03-vlr_atual_usd / wa_zglt100-ptax.
          ELSE.
            wa_saida_03-variacao_m_usd  = wa_saida_03-vlr_atual_usd.
          ENDIF.

* Pis e Cofins - RJF
          wa_saida_03-pis_brl    = ( wa_saida_03-variacao_m_brl * wa_096-pis ).
          wa_saida_03-pis_usd    = ( wa_saida_03-variacao_m_usd * wa_096-pis ).
          wa_saida_03-cofins_brl = ( wa_saida_03-variacao_m_brl * wa_096-cofins ).
          wa_saida_03-cofins_usd = ( wa_saida_03-variacao_m_usd * wa_096-cofins ).

*          CASE _num_parcela.
*            WHEN 1.
*              vlr_at_brl_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_brl ).
*              vlr_at_usd_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_usd ).
*
*              wa_saida_03-variacao_m_brl  = wa_saida_03-vlr_atual_brl.
*              wa_saida_03-variacao_m_usd  = wa_saida_03-vlr_atual_usd.
*
*            WHEN OTHERS.
*              wa_saida_03-variacao_m_brl  = ( wa_saida_03-vlr_atual_brl - vlr_at_brl_ant ).
*              wa_saida_03-variacao_m_usd  = ( wa_saida_03-vlr_atual_usd - vlr_at_usd_ant ).   " 'Variação Mensal USD'
*
*              CLEAR: vlr_at_brl_ant,  vlr_at_usd_ant.
*              vlr_at_brl_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_brl ).
*              vlr_at_usd_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_usd ).
*          ENDCASE.

*        ELSE. " se não achar as saca_acumul... " aqui
*
        ENDIF.

        IF wa_saida_03-doc_contabil IS NOT INITIAL.
          wa_saida_03-status = icon_led_green.
        ELSE.
          wa_saida_03-status = icon_led_yellow.
        ENDIF.

        FREE wa_saida_03-celltab.
        gt_estilo01 =  VALUE #( ( fieldname = 'VLR_SACA_BRL' style = cl_gui_alv_grid=>mc_style_disabled )
                                ( fieldname = 'TAXA_02'      style = cl_gui_alv_grid=>mc_style_disabled )  ).
        INSERT LINES OF gt_estilo01 INTO TABLE wa_saida_03-celltab.


        APPEND VALUE #(
                      num_parcela      = _num_parcela
                      dt_atualizacao   = _dt_atual
                      status           = wa_saida_03-status
                      qnt_sacas        = wa_saida_03-qnt_sacas
                      qnt_sacas_ms     = wa_saida_03-qnt_sacas_ms
                      vlr_p_m_sacas    = wa_saida_03-vlr_p_m_sacas
                      vlr_p_brl        = wa_saida_03-vlr_p_brl
                      vlr_p_m_brl      = wa_saida_03-vlr_p_m_brl
                      vlr_p_usd        = wa_saida_03-vlr_p_usd
                      vlr_p_m_usd      = wa_saida_03-vlr_p_m_usd
                      preco_saca_brl   = wa_saida_03-preco_saca_brl
                      taxa_01          = wa_saida_03-taxa_01
                      preco_saca_usd   = wa_saida_03-preco_saca_usd
                      vlr_saca_brl     = wa_saida_03-vlr_saca_brl
                      taxa_02          = wa_saida_03-taxa_02
                      vlr_saca_usd     = wa_saida_03-vlr_saca_usd
                      vlr_atual_brl    = wa_saida_03-vlr_atual_brl
                      variacao_m_brl   = wa_saida_03-variacao_m_brl
                      vlr_atual_usd    = wa_saida_03-vlr_atual_usd
                      doc_contabil     = wa_saida_03-doc_contabil
                      doc_estorno      = wa_saida_03-doc_estorno
                      variacao_m_usd   = wa_saida_03-variacao_m_usd  ) TO it_saida_03.


        CLEAR _dt_vencimento.
        _dt_vencimento =  _dt_atual.

        CLEAR: _month, _year,  e_max_days, _dt_atual, _days, wa_saida_03.
      ENDDO.


    WHEN 0.

      IF wa_096c-data IS INITIAL.
        wa_096c-data = sy-datum.
      ENDIF.

      IF wa_096c-hora IS INITIAL.
        wa_096c-hora = sy-uzeit.
      ENDIF.

      LOOP AT  it_096c INTO wa_096c.
        wa_saida_03-num_parcela      =  wa_096c-cod_contrato.
        wa_saida_03-dt_atualizacao   =  wa_096c-dt_atualizacao.
        wa_saida_03-qnt_sacas        =  wa_096c-qtd_sacas.
        wa_saida_03-qnt_sacas_ms     =  wa_096c-qtd_sacas_mensal.
        wa_saida_03-vlr_p_m_sacas    =  wa_096c-vlr_parcmes_sacas.
        wa_saida_03-vlr_p_brl        =  wa_096c-vlr_parc_brl.
        wa_saida_03-vlr_p_m_brl      =  wa_096c-vlr_parcmes_brl.
        wa_saida_03-vlr_p_usd        =  wa_096c-vlr_parc_usd.
        wa_saida_03-vlr_p_m_usd      =  wa_096c-vlr_parcmes_usd.
        wa_saida_03-preco_saca_brl   =  wa_096c-preco_saca_brl.
        wa_saida_03-taxa_01          =  wa_096c-taxa_01.
        wa_saida_03-preco_saca_usd   =  wa_096c-preco_saca_usd.
        wa_saida_03-vlr_saca_brl     =  wa_096c-vlr_saca_brl.
        wa_saida_03-taxa_02          =  wa_096c-taxa_fechamento.
        wa_saida_03-vlr_saca_usd     =  wa_096c-vlr_saca_usd.
        wa_saida_03-vlr_atual_brl    =  wa_096c-vlr_atualizado_brl.
        wa_saida_03-variacao_m_brl   =  wa_096c-variacao_mes_brl.
        wa_saida_03-vlr_atual_usd    =  wa_096c-vlr_atualizado_usd.
        wa_saida_03-variacao_m_usd   =  wa_096c-variacao_mes_usd.
        wa_saida_03-doc_contabil     =  wa_096c-doc_contabil.
        wa_saida_03-doc_estorno      =  wa_096c-doc_estorno.
*        wa_saida_03-data             =  wa_096c-data.
*        wa_saida_03-hora             =  wa_096c-hora.
*        wa_saida_03-usuario          =  wa_096c-usuario.
        wa_saida_03-obj_key          =  wa_096c-obj_key.
        wa_saida_03-doc_lcto         =  wa_096c-doc_lcto.

        IF wa_saida_03-doc_contabil IS NOT INITIAL.
          wa_saida_03-status = icon_led_green.
        ELSE.
          wa_saida_03-status = icon_led_yellow.
        ENDIF.

        APPEND wa_saida_03 TO it_saida_03.
        CLEAR: wa_saida_03, wa_096c.
      ENDLOOP.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  PERFORM fieldcat.

  PERFORM imprimi_dados.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'GERAR'.

      CASE p_tp_arr.
        WHEN '02'.
          PERFORM f_gerar_documento USING '01'. " Doc. Fluxo Saldo Inicial
          PERFORM f_gerar_documento USING '02'. " Doc. Apropriação do Juros s/ Arrendamento
          PERFORM f_gerar_documento USING '03'. " Doc. Transferência de LP/CP
        WHEN '03'.
* -----
          PERFORM f_gerar_documento_03 USING '06'. " Doc. Fluxo receber
        WHEN '05'.
          PERFORM f_gerar_documento_05 USING '08'.
          PERFORM f_gerar_documento_05 USING '07'.
      ENDCASE.

    WHEN 'ESTSALDOINICIAL'.     "ESTORNO SALDO INICIAL

      PERFORM estorno.

    WHEN 'ESTAPROJUR'. "ESTORNO APRO JUROS

      PERFORM estorno.

    WHEN 'ESTLPCP'. "ESTORNO LP/CP

      PERFORM estorno.

    WHEN 'REFRESH'.

      CASE p_tp_arr.
        WHEN '02'.
          PERFORM f_atualizar_documento.
        WHEN '03'.
          PERFORM f_atualizar_documento_03.
        WHEN '05'.
          PERFORM f_atualizar_documento_05.
      ENDCASE.

  ENDCASE.

  CALL METHOD g_alv->refresh_table_display( EXPORTING is_stable = wa_stable ).

ENDMODULE.


FORM pega_logo USING nome_logo CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRATAR_DADOS_04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tratar_dados_04.

  DATA: _num         TYPE i,
        _num_meses   TYPE i,
        _num_parcela TYPE i VALUE 0,
        _month(2)    TYPE c,
        _year(4)     TYPE c.

  LOOP AT it_094 INTO DATA(wa_zglt094)
    WHERE dt_vencimento+0(4) EQ p_ano.
    wa_saida_04_1-vlr_contrato_brl = wa_saida_04_1-vlr_contrato_brl +  wa_zglt094-valor_brl.
    wa_saida_04_1-vlr_contrato_usd = wa_saida_04_1-vlr_contrato_usd +  wa_zglt094-valor_usd.
  ENDLOOP.
  APPEND wa_saida_04_1 TO it_saida_04_1.

  DATA(data_ano) = p_ano && '0101'.

  LOOP AT it_094 INTO wa_094
    WHERE periodo_de  LE data_ano
      AND periodo_ate GE data_ano.

    READ TABLE it_096 INTO wa_096 WITH KEY cod_contrato = wa_094-cod_contrato.

    "Busca Perídos para Divisão do Valor Total
    CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES'
      EXPORTING
        i_datum_bis = wa_094-periodo_ate
        i_datum_von = wa_094-periodo_de
      IMPORTING
        e_monate    = _num_meses.

    ADD 1 TO _num_meses.

    IF wa_094-periodo_ate(4) GT p_ano.
      wa_094-periodo_ate = p_ano && '1230'.
    ENDIF.

    IF wa_094-periodo_de(4) LT p_ano.
      wa_094-periodo_de = p_ano && '0101'.
    ENDIF.

    "Busca Quantidade Meses dentro do Ano Informado.
    CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES'
      EXPORTING
        i_datum_bis = wa_094-periodo_ate
        i_datum_von = wa_094-periodo_de
      IMPORTING
        e_monate    = _num.

    ADD 1 TO _num.

    DO _num TIMES.

      _num_parcela = _num_parcela + 1.
      _month       = _num_parcela.
      _year        =  p_ano.
      wa_saida_04-num_parcela = _num_parcela.
      CONCATENATE _month '/' _year INTO  wa_saida_04-periodo.

      wa_saida_04-apropriacao_brl = ( wa_094-valor_brl / _num_meses ).
      wa_saida_04-apropriacao_usd = ( wa_094-valor_usd / _num_meses ).

      CASE _num_parcela.
        WHEN 1.
          wa_saida_04-sald_a_apro_blr = ( ( ( wa_094-valor_brl / _num_meses ) * _num ) - wa_saida_04-apropriacao_brl ).
          wa_saida_04-sald_a_apro_usd = ( ( ( wa_094-valor_usd / _num_meses ) * _num ) - wa_saida_04-apropriacao_usd ).

          DATA(_saldo_pro_brl_ant) = wa_saida_04-sald_a_apro_blr.
          DATA(_saldo_pro_usd_ant) = wa_saida_04-sald_a_apro_usd.

        WHEN OTHERS.
          wa_saida_04-sald_a_apro_blr = ( _saldo_pro_brl_ant - wa_saida_04-apropriacao_brl ).
          wa_saida_04-sald_a_apro_usd = ( _saldo_pro_usd_ant - wa_saida_04-apropriacao_usd ).
          CLEAR: _saldo_pro_brl_ant, _saldo_pro_usd_ant.

          _saldo_pro_brl_ant = wa_saida_04-sald_a_apro_blr.
          _saldo_pro_usd_ant = wa_saida_04-sald_a_apro_usd.
      ENDCASE.

      wa_saida_04-pis    = ( wa_saida_04-apropriacao_brl *  ( wa_092-tx_pis / 100 ) ).
      wa_saida_04-cofins = ( wa_saida_04-apropriacao_brl *  ( wa_092-tx_cofins / 100 ) ).

      READ TABLE it_saida_04 WITH KEY periodo = wa_saida_04-periodo ASSIGNING FIELD-SYMBOL(<fs_periodo>).
      IF sy-subrc IS INITIAL.
        ADD wa_saida_04-apropriacao_brl TO <fs_periodo>-apropriacao_brl.
        ADD wa_saida_04-apropriacao_usd TO <fs_periodo>-apropriacao_usd.
        ADD wa_saida_04-sald_a_apro_blr TO <fs_periodo>-sald_a_apro_blr.
        ADD wa_saida_04-sald_a_apro_usd TO <fs_periodo>-sald_a_apro_usd.
        ADD wa_saida_04-pis    TO <fs_periodo>-pis.
        ADD wa_saida_04-cofins TO <fs_periodo>-cofins.
      ELSE.
        APPEND wa_saida_04 TO it_saida_04.
      ENDIF.
    ENDDO.

    CLEAR: wa_saida_04, wa_094, _num_parcela , _num, _year, _num_meses.
  ENDLOOP.
ENDFORM.


*&      Form  Z_MONTA_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_monta_cabecalho .

  DATA: url(255)           TYPE c,
        p_text             TYPE sdydo_text_element,
        filtros	           TYPE zif_screen_linha_filtro,
        i_filtros	         TYPE zif_screen_linha_filtro_t,
        tx_data            TYPE c LENGTH 10,
        tx_valor           TYPE c LENGTH 30,
        tx_valor2          TYPE c LENGTH 20,
        value_tab          TYPE TABLE OF dd07v,
        value_tab_l        TYPE TABLE OF dd07l,
        lc_apropriacao_brl TYPE zgle0017,
        lc_apropriacao_usd TYPE zgle0017,
        lc_pis             TYPE zgle0017,
        lc_cofins          TYPE zgle0017,
        lv_valor           TYPE p LENGTH 16 DECIMALS 2.

*  p_text = text-003.

  IF g_alv IS INITIAL.

    CLEAR: i_filtros.

    IF p_tp_arr IS NOT INITIAL.
      CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname      = 'ZGLD0001'
        TABLES
          values_tab   = value_tab
          values_dd07l = value_tab_l.
      READ TABLE value_tab WITH KEY  domvalue_l = p_tp_arr INTO DATA(wa_value_tab).
      APPEND VALUE #( parametro = 'Tipo de Arrendamento' valor = wa_value_tab-ddtext ) TO i_filtros.
    ENDIF.

    IF p_cont IS NOT INITIAL.
      APPEND VALUE #( parametro = 'Contrato' valor = p_cont ) TO i_filtros.
    ENDIF.

    IF p_dtvenc IS NOT INITIAL.
      WRITE p_dtvenc TO tx_data.
      APPEND VALUE #( parametro = 'Dt. Vencimento' valor = tx_data ) TO i_filtros.
    ENDIF.

    CLEAR: filtros.

    SELECT SINGLE * INTO @DATA(wa_empresa)
      FROM t001
     WHERE bukrs EQ @wa_092-bukrs.

    CLEAR: filtros.
    IF zcl_string=>initialcap( i_str = CONV #( wa_empresa-butxt ) ) IS NOT INITIAL.
      filtros-parametro = 'Empresa'.
      filtros-valor     = zcl_string=>initialcap( i_str = CONV #( wa_empresa-butxt ) ).
    ENDIF.

    IF filtros IS NOT INITIAL.
      APPEND filtros TO i_filtros.
    ENDIF.

    IF p_dtvenc IS NOT INITIAL.
      p_ano = p_dtvenc(4).
      p_mes = p_dtvenc+4(2).
    ENDIF.

    IF p_mes IS NOT INITIAL.
      filtros-parametro = 'Mês'.
      filtros-valor     = p_mes.
    ENDIF.

    IF filtros IS NOT INITIAL.
      APPEND filtros TO i_filtros.
    ENDIF.

    IF p_ano IS NOT INITIAL.
      filtros-parametro = 'Ano'.
      filtros-valor     = p_ano.
    ENDIF.
    IF filtros IS NOT INITIAL.
      APPEND filtros TO i_filtros.
    ENDIF.

*    IF wa_092-cliente IS NOT INITIAL.
*      TRY .
*          zcl_clientes=>zif_parceiros~get_instance(
*            )->set_parceiro( i_parceiro = wa_092-cliente
*            )->get_name( IMPORTING e_name = DATA(e_name)
*            ).
*          filtros-parametro2 = 'Cliente'.
*          filtros-valor2     = zcl_string=>initialcap( i_str = CONV #( e_name ) ).
*        CATCH zcx_parceiros.    " .
*      ENDTRY.
*    ENDIF.
*
*    IF wa_092-fornecedor IS NOT INITIAL.
*      TRY .
*          zcl_fornecedores=>zif_parceiros~get_instance(
*            )->set_parceiro( i_parceiro = wa_092-fornecedor
*            )->get_name( IMPORTING e_name = e_name
*            ).
*          filtros-parametro2 = 'Fornecedor'.
*          filtros-valor2     = zcl_string=>initialcap( i_str = CONV #( e_name ) ).
*        CATCH zcx_parceiros.    " .
*      ENDTRY.
*    ENDIF.

*    APPEND filtros TO i_filtros.

    CASE wa_092-tp_arrendamento.
      WHEN '01' OR '02'.

      WHEN '03' OR '04'.

*        IF wa_092-tp_arrendamento EQ '04'.
*          lc_apropriacao_brl = 0.
*          lc_apropriacao_usd = 0.
*          lc_pis             = 0.
*          lc_cofins          = 0.
*
*          LOOP AT it_saida_04 INTO DATA(wa_saida_04).
*            ADD wa_saida_04-apropriacao_brl TO lc_apropriacao_brl.
*            ADD wa_saida_04-apropriacao_usd TO lc_apropriacao_usd.
*            ADD wa_saida_04-pis TO lc_pis.
*            ADD wa_saida_04-cofins TO lc_cofins.
*          ENDLOOP.
*
*          lv_valor = lc_apropriacao_brl.
*          WRITE lv_valor TO tx_valor.
*          CONDENSE tx_valor NO-GAPS.
*
*          lv_valor = lc_apropriacao_usd.
*          WRITE lv_valor TO tx_valor2.
*          CONDENSE tx_valor2 NO-GAPS.
*          APPEND VALUE #( parametro = 'Total Apropriar BRL' valor = tx_valor parametro2 = 'Total Apropriar USD' valor2 = tx_valor2 direita = abap_true ) TO i_filtros.
*
*          lv_valor = lc_pis.
*          WRITE lv_valor TO tx_valor.
*          CONDENSE tx_valor NO-GAPS.
*
*          lv_valor = lc_cofins.
*          WRITE lv_valor TO tx_valor2.
*          CONDENSE tx_valor2 NO-GAPS.
*          APPEND VALUE #( parametro = 'Total PIS' valor = tx_valor parametro2 = 'Total COFINS' valor2 = tx_valor2 direita = abap_true ) TO i_filtros.
*
*        ENDIF.
*
*        WRITE wa_092-tx_pis TO tx_valor.
*        CONDENSE tx_valor NO-GAPS.
*        tx_valor = |{ tx_valor } %|.
*        WRITE wa_092-tx_cofins TO tx_valor2.
*        CONDENSE tx_valor2 NO-GAPS.
*        tx_valor2 = |{ tx_valor2 } %|.
*        APPEND VALUE #( parametro = 'PIS' valor = tx_valor parametro2 = 'COFINS' valor2 = tx_valor2  direita = abap_true ) TO i_filtros.

    ENDCASE.

  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
        EXPORTING
          i_titulo  = CONV #( p_text )
          i_filtros = i_filtros
        CHANGING
          split     = dg_splitter_1
          alv       = g_alv ) = abap_true.

    CASE p_tp_arr.
      WHEN '01' OR '02'.

        wa_layout-sel_mode   = 'A'.
        wa_layout-stylefname = 'CELLSTYLES'.
        wa_layout-ctab_fname = 'TCOLOR'.
        wa_layout-info_fname = 'COLOR'.

        SET HANDLER:  lcl_event_hander=>on_data_changed  FOR g_alv,
                      lcl_event_hander=>on_hotspot_click FOR g_alv,
                      lcl_event_hander=>on_data_changed_finished FOR g_alv.


*    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
*    APPEND wl_function TO tl_function.

        CALL METHOD g_alv->set_table_for_first_display
          EXPORTING
            is_layout       = wa_layout
            i_save          = 'A'
          CHANGING
            it_outtab       = it_saida_02[]
            it_fieldcatalog = it_fieldcat.

      WHEN '03'.

*        wa_layout-stylefname = 'CELLTAB'.
        wa_layout-sel_mode   = 'A'.
        wa_layout-stylefname = 'CELLSTYLES'.
        wa_layout-ctab_fname = 'TCOLOR'.
        wa_layout-info_fname = 'COLOR'.

        SET HANDLER:  lcl_event_hander=>on_data_changed FOR g_alv,
                      lcl_event_hander=>on_hotspot_click FOR g_alv,
                      lcl_event_hander=>on_data_changed_finished FOR g_alv.

        CALL METHOD g_alv->set_table_for_first_display
          EXPORTING
            is_layout       = wa_layout
            i_save          = 'A'
          CHANGING
            it_outtab       = it_saida_03[]
            it_fieldcatalog = it_fieldcat.

      WHEN '04'.

        CALL METHOD g_alv->set_table_for_first_display
          EXPORTING
            is_layout       = wa_layout
            i_save          = 'A'
          CHANGING
            it_outtab       = it_saida_04[]
            it_fieldcatalog = it_fieldcat.

      WHEN '05'.
        wa_layout-sel_mode   = 'A'.
        wa_layout-stylefname = 'CELLSTYLES'.
        wa_layout-ctab_fname = 'TCOLOR'.
        wa_layout-info_fname = 'COLOR'.

        SET HANDLER:  lcl_event_hander=>on_data_changed FOR g_alv,
                      lcl_event_hander=>on_hotspot_click FOR g_alv,
                      lcl_event_hander=>on_data_changed_finished FOR g_alv.

        CALL METHOD g_alv->set_table_for_first_display
          EXPORTING
            is_layout       = wa_layout
            i_save          = 'A'
          CHANGING
            it_outtab       = it_saida_05[]
            it_fieldcatalog = it_fieldcat.

    ENDCASE.

    CALL METHOD g_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD g_alv->refresh_table_display( is_stable = wa_stable ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gerar_documento USING p_class TYPE zgle0029.

  DATA:
    dp_resp      TYPE char2,
    e_num_lote   TYPE zlote_num,
    wl_zglt035   TYPE zglt035,
    v_ultimo_dia TYPE sy-datum,
    gt_zglt032   TYPE TABLE OF zglt032,
    wa_zglt032   TYPE zglt032,
    gt_zglt036   TYPE TABLE OF zglt036,
    wl_zglt036   TYPE zglt036,
    v_objkey     TYPE char20,
    lv_dats      TYPE char10,
    lv_doc       TYPE zgle0004,
    lv_text      TYPE string,
    lv_int       TYPE zgle0004,
    lv_cont      TYPE i,
    lv_tab95     TYPE syst_tabix,
    lv_for       TYPE zgle0004,
    lv_comp      TYPE sy-datum,
    lv_datap     TYPE sy-datum,
    vl_data      TYPE c LENGTH 10,
    data         TYPE sy-datum,
    wl_tcurr     TYPE tcurr,
    lv_ukurs     TYPE zgle0018,
    lv_ult_dia   TYPE sy-datum,
    lv_datum     TYPE sy-datum,
    v_qtdforn    TYPE i,
    v_data       TYPE sy-datum.

  CONCATENATE p_ano p_mes '01' INTO lv_comp.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_comp
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  MOVE v_ultimo_dia TO v_data.

* Taxa
  lv_datap = v_data + 1.
  zcl_util=>conv_data_us_br( EXPORTING i_data = lv_datap
                             RECEIVING e_data = vl_data ).

  " Função standard para converter data em formato gravado na TCURR
  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = vl_data
    IMPORTING
      output = data.

  " Tabela responsavel por gravar a taxa do câmbio.
  SELECT SINGLE *
    FROM tcurr
     INTO wl_tcurr
   WHERE fcurr EQ 'BRL'
     AND tcurr EQ 'USD'
     AND kurst EQ 'B'
     AND gdatu EQ data.
  IF sy-subrc IS INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
    lv_ukurs = abs( wl_tcurr-ukurs ).
    IF lv_ukurs IS INITIAL.
      lv_ukurs = 1.
    ENDIF.
  ENDIF.

  READ TABLE it_saida_02 INTO wa_saida_02 WITH KEY fl_venc_comp = v_ultimo_dia.
  IF sy-subrc IS INITIAL.
    DATA(lv_tabix) = sy-tabix.
    wa_saida_02_temp = wa_saida_02.
  ENDIF.

  IF p_class EQ '01'.

    CLEAR var_answer.

    READ TABLE it_saida_02 INTO DATA(wa_saida_02si) INDEX 1.
    IF wa_saida_02si-fl_venc_comp(6) EQ v_ultimo_dia(6).
      DATA(lv_tabixsi) = sy-tabix.
      DATA(lv_ecomp) = abap_true.
    ENDIF.
    IF sy-subrc IS INITIAL AND ( wa_saida_02si-doc_s_ini IS NOT INITIAL AND lv_ecomp IS NOT INITIAL ) OR wa_saida_02-doc_a_jur IS NOT INITIAL.
      MESSAGE 'Já existe documento gerado!' TYPE 'E'.
      EXIT.
    ELSEIF wa_saida_02-fl_venc_comp IS INITIAL.
      MESSAGE 'Período informado inválido!' TYPE 'E'.
    ENDIF.

    CONCATENATE v_ultimo_dia+6(2) '/' v_ultimo_dia+4(2) '/' v_ultimo_dia(4) INTO lv_dats.

    IF lv_ecomp IS NOT INITIAL.
      CONCATENATE 'Deseja realmente gerar os documentos da competência'
      ' (' lv_dats ') e Saldo Inicial?'
      INTO lv_text.
    ELSE.
      CONCATENATE 'Deseja realmente gerar os documentos da competência'
      ' (' lv_dats ')?'
      INTO lv_text.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = lv_text
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
  ELSE.
    CHECK var_answer EQ '1'.
  ENDIF.

  IF lv_ecomp IS INITIAL AND p_class EQ '01'.
    EXIT.
  ENDIF.

  READ TABLE it_saida_02 INTO wa_saida_02 WITH KEY fl_venc_comp = v_ultimo_dia. "INDEX 1.
  IF sy-subrc IS INITIAL AND (     wa_saida_02si-doc_s_ini IS NOT INITIAL
                               OR  wa_saida_02-doc_a_jur IS NOT INITIAL
                               OR  wa_saida_02-doc_lpcp  IS NOT INITIAL ).
    MESSAGE 'Já existe documento gerado!' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT *
    FROM zglt095
    INTO TABLE @DATA(it_zglt095)
    WHERE tp_arrendamento EQ @wa_092-tp_arrendamento
      AND classificacao EQ @p_class.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Parâmetros de Contabilização não encontrado para o tipo de contrato.' TYPE 'E'.
  ELSE.
    SORT it_zglt095 BY id_parametro.
  ENDIF.

  "dp_resp = '83'. "Contabilidade
  dp_resp = '09'. "Contabilidade
  "PSA zcl_gerar_lote 1

  CALL METHOD zcl_gerar_lote=>create_lote
    EXPORTING
      i_bukrs      = wa_092-bukrs
      i_descr_lote = 'Arrendamento de Contratos'(002)
      i_user_resp  = sy-uname
      i_dep_resp   = dp_resp
    IMPORTING
      e_num_lote   = e_num_lote.

  MOVE:    e_num_lote                  TO wl_zglt035-lote,
           wa_092-bukrs                TO wl_zglt035-bukrs,
           ''                          TO wl_zglt035-tp_lcto,
           dp_resp                     TO wl_zglt035-dpto_resp,
           'BRL'                       TO wl_zglt035-moeda_doc,
           'LM'                        TO wl_zglt035-blart,
           TEXT-002                    TO wl_zglt035-bktxt,
           v_data                      TO wl_zglt035-budat,
           v_data                      TO wl_zglt035-bldat,
           sy-datum                    TO wl_zglt035-dt_lcto,
           ''                          TO wl_zglt035-prov_est,
           p_mes                       TO wl_zglt035-monat,
           p_ano                       TO wl_zglt035-gjahr,
           sy-uname                    TO wl_zglt035-usnam,
           sy-datum                    TO wl_zglt035-dt_entrada,
           sy-uzeit                    TO wl_zglt035-hr_entrada.


*  SELECT SINGLE * FROM zglt096c
*    INTO @DATA(wa_zglt096c)
*    WHERE cod_contrato EQ @wa_092-cod_contrato.

* Itens
  IF p_class EQ '01'.

    SELECT * FROM zglt092a
      INTO TABLE @DATA(it_zglt092a)
      WHERE cod_contrato EQ @wa_092-cod_contrato
        AND ( tp_arrendamento EQ @wa_092-tp_arrendamento
        OR tp_arrendamento EQ @wa_092-tp_arrendamento+1(1) ).

    IF sy-subrc IS INITIAL.
      v_qtdforn = lines( it_zglt092a ).
    ENDIF.

    LOOP AT it_zglt092a ASSIGNING FIELD-SYMBOL(<f_zglt092a>). " Fornecedor/clientes

* Item 1 - forn/cliente bp circulante
      lv_cont = lv_cont + 1.

      IF it_zglt095[] IS NOT INITIAL.
        READ TABLE it_zglt095 INTO DATA(wa_zglt095f) INDEX lv_cont.
      ENDIF.

      wl_zglt036-seqitem = lv_cont.
      wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.

      IF <f_zglt092a>-fornecedor IS NOT INITIAL.
        wl_zglt036-hkont = <f_zglt092a>-fornecedor.
      ELSEIF <f_zglt092a>-cliente IS NOT INITIAL.
        wl_zglt036-hkont = <f_zglt092a>-cliente.
      ELSE.
        MESSAGE 'Cliente/Fornecedor não cadastrado!' TYPE 'E' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF <f_zglt092a>-cliente IS NOT INITIAL.
        wl_zglt036-bschl = '21'.
      ELSE.
        wl_zglt036-bschl = '39'.
      ENDIF.

      wl_zglt036-dt_vct = v_data.
      wl_zglt036-umskz = <f_zglt092a>-razao_especial.
      wl_zglt036-kostl = ' '.
      wl_zglt036-anbwa = ' '.
      wl_zglt036-bewar = ''.
      wl_zglt036-matnr = ' '.
      wl_zglt036-sgtxt = wa_zglt095f-texto_historico.
      wl_zglt036-gsber = wa_092-filial.
      wl_zglt036-zuonr = wa_092-atrib.

      IF wa_saida_02si-bp_cir_pri IS NOT INITIAL AND v_qtdforn IS NOT INITIAL.
        lv_doc = abs( wa_saida_02si-bp_cir_pri / v_qtdforn ).
        wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
      ENDIF.

      lv_int = abs( wa_saida_02si-bp_cir_pri / v_qtdforn ).
      wl_zglt036-vlr_moeda_int = CONV #( lv_int ).


      IF wa_zglt095f-tp_taxa EQ '03'.
        IF wa_saida_02si-bp_cir_pri IS NOT INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
* Taxa
          lv_ukurs = abs( wl_tcurr-ukurs ).
          lv_for = abs( ( wa_saida_02si-bp_cir_pri / v_qtdforn ) / lv_ukurs ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ELSE.
          wl_zglt036-vlr_moeda_forte =  abs( ( wa_saida_02si-bp_cir_pri / v_qtdforn ) ).
          IF lv_ukurs IS INITIAL.
            lv_ukurs = 1.
          ENDIF.
          lv_for = abs( ( wa_saida_02si-bp_cir_pri / v_qtdforn ) / lv_ukurs ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ENDIF.
      ELSEIF wa_zglt095f-tp_taxa EQ '04'.
* Taxa fut - wa_092-tx_usd_futuro
        IF wa_saida_02si-bp_cir_pri IS NOT INITIAL AND wa_092-tx_usd_futuro IS NOT INITIAL.
* Taxa
          lv_ukurs = abs( wa_092-tx_usd_futuro ).
          lv_for = abs( ( wa_saida_02si-bp_cir_pri / v_qtdforn ) / lv_ukurs ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ELSE.
          lv_for = abs( ( wa_saida_02si-bp_cir_pri / v_qtdforn ) ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ENDIF.
      ENDIF.

      APPEND wl_zglt036 TO gt_zglt036.
      CLEAR: wl_zglt036.

* Item 2 - bp não circulante
      lv_cont = lv_cont + 1.

      wl_zglt036-seqitem = lv_cont.
      wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.

      IF <f_zglt092a>-fornecedor IS NOT INITIAL.
        wl_zglt036-hkont = <f_zglt092a>-fornecedor.
      ELSEIF <f_zglt092a>-cliente IS NOT INITIAL.
        wl_zglt036-hkont = <f_zglt092a>-cliente.
      ELSE.
        MESSAGE 'Cliente/Fornecedor não cadastrado!' TYPE 'E' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF <f_zglt092a>-cliente IS NOT INITIAL.
        wl_zglt036-bschl = '21'.
      ELSE.
        wl_zglt036-bschl = '39'.
      ENDIF.

      wl_zglt036-dt_vct = v_data.

      wl_zglt036-umskz = <f_zglt092a>-razao_especial.
      wl_zglt036-kostl =  ' '.
      wl_zglt036-anbwa = ' '.
      wl_zglt036-bewar = ' '.
      wl_zglt036-matnr = ' '.
      " MATKL não encontrado? É automático via transac

      wl_zglt036-sgtxt = wa_zglt095f-texto_historico.
      wl_zglt036-gsber = wa_092-filial.
      wl_zglt036-zuonr = wa_092-atrib.

      IF wa_saida_02si-bp_ncir_pri IS NOT INITIAL AND v_qtdforn IS NOT INITIAL.
        lv_doc = abs( wa_saida_02si-bp_ncir_pri / v_qtdforn ).
        wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
      ENDIF.

      lv_int = abs( wa_saida_02si-bp_ncir_pri / v_qtdforn ).
      wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

      IF wa_zglt095f-tp_taxa EQ '03'.
        IF wa_saida_02si-bp_ncir_pri IS NOT INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
* Taxa
          lv_ukurs = abs( wl_tcurr-ukurs ).
          lv_for = abs( ( wa_saida_02si-bp_ncir_pri / v_qtdforn ) / lv_ukurs ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ELSE.
          wl_zglt036-vlr_moeda_forte =  abs( ( wa_saida_02si-bp_ncir_pri / v_qtdforn ) ).
          IF lv_ukurs IS INITIAL.
            lv_ukurs = 1.
          ENDIF.
          lv_for = abs( ( wa_saida_02si-bp_ncir_pri / v_qtdforn ) / lv_ukurs ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ENDIF.
      ELSEIF wa_zglt095f-tp_taxa EQ '04'.
* Taxa fut
        IF wa_saida_02si-bp_ncir_pri IS NOT INITIAL AND wa_092-tx_usd_futuro IS NOT INITIAL.
* Taxa
          lv_ukurs = abs( wa_092-tx_usd_futuro ).
          lv_for = abs( ( wa_saida_02si-bp_ncir_pri / v_qtdforn ) / lv_ukurs ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ELSE.
          lv_for = abs( ( wa_saida_02si-bp_ncir_pri / v_qtdforn ) ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ENDIF.
      ENDIF.

      APPEND wl_zglt036 TO gt_zglt036.
      CLEAR: wl_zglt036.
    ENDLOOP.
  ENDIF.

  LOOP AT it_zglt095 ASSIGNING FIELD-SYMBOL(<f_zglt095>). " Contabilização
    lv_tab95 = sy-tabix.
    lv_cont = lv_cont + 1.
    wl_zglt036-seqitem = lv_cont.
*-----
    IF p_class EQ '01'.
      wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
      wl_zglt036-bschl = <f_zglt095>-chave_lcto.
      IF wl_zglt036-bschl EQ '70' OR  wl_zglt036-bschl EQ '75'.
        wl_zglt036-hkont = wa_092-anln1.
        wl_zglt036-anbwa = wa_092-anbwa.
        wl_zglt036-bewar = wa_092-bewar.
      ELSE.
        wl_zglt036-hkont = <f_zglt095>-conta.
        wl_zglt036-anbwa = <f_zglt095>-tipo_mov_i.
        wl_zglt036-bewar = <f_zglt095>-bewar.
      ENDIF.
      wl_zglt036-umskz = <f_zglt095>-razao_especial.
      wl_zglt036-sgtxt = <f_zglt095>-texto_historico.
      wl_zglt036-matnr = <f_zglt095>-matnr.
      wl_zglt036-gsber = wa_092-filial.
      wl_zglt036-dt_vct = v_data.
      wl_zglt036-zuonr = wa_092-atrib.

      CASE lv_tab95.

        WHEN '1'.
          lv_doc = abs( wa_saida_02si-vp_vl_presente ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
          lv_int = abs( wa_saida_02si-vp_vl_presente ).
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

          IF <f_zglt095>-tp_taxa EQ '03'.

            IF wa_saida_02si-vp_vl_presente IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
              lv_for = abs( wa_saida_02si-vp_vl_presente / lv_ukurs ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02si-vp_vl_presente ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.

          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
            IF wa_saida_02si-vp_vl_presente IS NOT INITIAL AND wa_092-tx_usd_futuro IS NOT INITIAL.
              lv_for = abs( wa_saida_02si-vp_vl_presente / wa_092-tx_usd_futuro ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02si-vp_vl_presente ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.
          ENDIF.
        WHEN '2'.
          lv_doc = abs( wa_saida_02si-saldo_cp ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
          lv_int = abs( wa_saida_02si-saldo_cp ).
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

          IF <f_zglt095>-tp_taxa EQ '03'.

            IF wa_saida_02si-saldo_cp IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
              lv_for = abs( wa_saida_02si-saldo_cp / lv_ukurs ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02si-saldo_cp ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.

          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
            IF wa_saida_02si-saldo_cp IS NOT INITIAL AND wa_092-tx_usd_futuro IS NOT INITIAL.
              lv_for = abs( wa_saida_02si-saldo_cp / wa_092-tx_usd_futuro ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02si-saldo_cp ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.
          ENDIF.
        WHEN '3'.
          lv_doc = abs( wa_saida_02si-saldo_lp ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
          lv_int = abs( wa_saida_02si-saldo_lp ).
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

          IF <f_zglt095>-tp_taxa EQ '03'.

            IF wa_saida_02si-saldo_lp IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
              lv_for = abs( wa_saida_02si-saldo_lp / lv_ukurs ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02si-saldo_lp ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.

          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
            IF wa_saida_02si-saldo_lp IS NOT INITIAL AND wa_092-tx_usd_futuro IS NOT INITIAL.
              lv_for = abs( wa_saida_02si-saldo_lp / wa_092-tx_usd_futuro ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            wl_zglt036-vlr_moeda_forte = ( wa_saida_02_temp-vp_vl_presente / wa_092-tx_usd_futuro ).
            ELSE.
              lv_for = abs( wa_saida_02si-saldo_lp ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDIF.
*----------------------------------------------------------
    IF p_class EQ '02'.

      wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
      wl_zglt036-bschl = <f_zglt095>-chave_lcto.
      IF wl_zglt036-bschl EQ '70' OR  wl_zglt036-bschl EQ '75'.
        wl_zglt036-hkont = wa_092-anln1.
        wl_zglt036-anbwa = wa_092-anbwa.
        wl_zglt036-bewar = wa_092-bewar.
      ELSE.
        wl_zglt036-hkont = <f_zglt095>-conta.
        wl_zglt036-anbwa = <f_zglt095>-tipo_mov_i.
        wl_zglt036-bewar = <f_zglt095>-bewar.
      ENDIF.
      wl_zglt036-umskz = <f_zglt095>-razao_especial.

      wl_zglt036-sgtxt = wa_zglt032-sgtxt.
      wl_zglt036-gsber = wa_092-filial.

      wl_zglt036-sgtxt = <f_zglt095>-texto_historico.
      wl_zglt036-matnr = <f_zglt095>-matnr.
      wl_zglt036-zuonr = wa_092-atrib.
*      wl_zglt036-dt_vct = v_data.

      CASE lv_tab95.

        WHEN '1'.
          lv_doc = abs( wa_saida_02_temp-aprov_jur ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
          lv_int = abs( wa_saida_02_temp-aprov_jur ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_int ).

          IF <f_zglt095>-tp_taxa EQ '03'.

            IF lv_ukurs IS NOT INITIAL.
              lv_for = abs( wa_saida_02_temp-aprov_jur / lv_ukurs ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02_temp-aprov_jur ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.

          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
            IF wa_092-tx_usd_futuro IS NOT INITIAL.
              lv_for = abs( wa_saida_02_temp-aprov_jur / wa_092-tx_usd_futuro ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

            ELSE.
              lv_for = abs( wa_saida_02_temp-aprov_jur ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.
          ENDIF.
        WHEN '2'.
          lv_doc = abs( wa_saida_02_temp-aprov_jur ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
          lv_int = abs( wa_saida_02_temp-aprov_jur ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_int ).

          IF <f_zglt095>-tp_taxa EQ '03'.

            IF lv_ukurs IS NOT INITIAL.
              lv_for = abs( wa_saida_02_temp-aprov_jur / lv_ukurs ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02_temp-aprov_jur ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.

          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
            IF wa_092-tx_usd_futuro IS NOT INITIAL.
              lv_for = abs( wa_saida_02_temp-aprov_jur / wa_092-tx_usd_futuro ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02_temp-aprov_jur ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.
          ENDIF.

      ENDCASE.

    ENDIF.
* -----
    IF p_class EQ '03'.

      wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
*      wl_zglt036-hkont = <f_zglt095>-conta.
      wl_zglt036-bschl = <f_zglt095>-chave_lcto.
      IF wl_zglt036-bschl EQ '70' OR  wl_zglt036-bschl EQ '75'.
        wl_zglt036-hkont = wa_092-anln1.
        wl_zglt036-anbwa = wa_092-anbwa.
        wl_zglt036-bewar = wa_092-bewar.
      ELSE.
        wl_zglt036-hkont = <f_zglt095>-conta.
        wl_zglt036-anbwa = <f_zglt095>-tipo_mov_i.
        wl_zglt036-bewar = <f_zglt095>-bewar.
      ENDIF.
      wl_zglt036-umskz = <f_zglt095>-razao_especial.

      wl_zglt036-sgtxt = wa_zglt032-sgtxt.
      wl_zglt036-gsber = wa_092-filial.

      wl_zglt036-sgtxt = <f_zglt095>-texto_historico.
      wl_zglt036-matnr = <f_zglt095>-matnr.
      wl_zglt036-dt_vct = v_data.
      wl_zglt036-zuonr  = wa_092-atrib.

      CASE lv_tab95.

        WHEN '1'.

          lv_doc = abs( wa_saida_02_temp-transflpcp ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
          lv_int = abs( wa_saida_02_temp-transflpcp ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_int ).

          IF <f_zglt095>-tp_taxa EQ '03'.

            IF lv_ukurs IS NOT INITIAL.
              lv_for = abs( wa_saida_02_temp-transflpcp / lv_ukurs ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02_temp-transflpcp ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.

          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
            IF wa_092-tx_usd_futuro IS NOT INITIAL.
              lv_for = abs( wa_saida_02_temp-transflpcp / wa_092-tx_usd_futuro ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

            ELSE.
              lv_for = abs( wa_saida_02_temp-transflpcp ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.
          ENDIF.
        WHEN '2'.

          lv_doc = abs( wa_saida_02_temp-transflpcp ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
          lv_int = abs( wa_saida_02_temp-transflpcp ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_int ).

          IF <f_zglt095>-tp_taxa EQ '03'.

            IF lv_ukurs IS NOT INITIAL.
              lv_for = abs( wa_saida_02_temp-transflpcp / lv_ukurs ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02_temp-transflpcp ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ENDIF.

          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
            IF wa_092-tx_usd_futuro IS NOT INITIAL.
              lv_for = abs( wa_saida_02_temp-transflpcp / wa_092-tx_usd_futuro ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
            ELSE.
              lv_for = abs( wa_saida_02_temp-transflpcp ).
              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

            ENDIF.
          ENDIF.
      ENDCASE.

    ENDIF.

    APPEND wl_zglt036 TO gt_zglt036.
    CLEAR: wl_zglt036.
  ENDLOOP.

  CLEAR wl_zglt036.
  LOOP AT gt_zglt036 INTO wl_zglt036.

    wa_zglt036_flg-doc_lcto        = wl_zglt036-doc_lcto.
    wa_zglt036_flg-seqitem         = wl_zglt036-seqitem.
    wa_zglt036_flg-seqsub          = wl_zglt036-seqsub.
    " wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
    " wa_zglt036_flg-fl_cv_moeda_int = abap_true.
    " wa_zglt036_flg-fl_cv_moeda_for = abap_true.
    wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
    APPEND  wa_zglt036_flg TO it_zglt036_flg.
  ENDLOOP.
  "131204 ZGL071 - Arrendamento - PSA GRAVA LOTE - Lote LP/CP
  "IF wl_zglt036-vlr_moeda_doc > 0 AND wl_zglt036-vlr_moeda_int > 0 AND wl_zglt036-vlr_moeda_forte > 0.
  IF lv_doc > 0 AND lv_int > 0 AND lv_for > 0.
    "PSA zcl_gerar_lote 2

    CALL METHOD zcl_gerar_lote=>contabilizar_lote(
      EXPORTING
        i_arredonda   = abap_true
        i_zglt036_flg = it_zglt036_flg
      CHANGING
        i_zglt036     = gt_zglt036
        i_zglt035     = wl_zglt035 ).
*
    COMMIT WORK.

    CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
      EXPORTING
        p_num_lote = e_num_lote.
    COMMIT WORK.

    "CHECK wl_zglt035-doc_lcto IS NOT INITIAL.
    CONCATENATE 'ZGL17' wl_zglt035-doc_lcto p_ano INTO v_objkey.

    SELECT *
      UP TO 1 ROWS
       FROM zib_contabil
       INTO @DATA(wa_zib_contabil)
       WHERE obj_key  = @v_objkey.
    ENDSELECT.

    SELECT *
      UP TO 1 ROWS
       FROM zib_contabil_chv
       INTO @DATA(wa_zib_contabilx)
       WHERE obj_key  = @v_objkey.
    ENDSELECT.

    IF p_class EQ '01'.
      IF wa_saida_02-est_s_ini IS INITIAL AND wa_zib_contabilx-belnr IS NOT INITIAL.
        MOVE:    wa_zib_contabilx-belnr TO wa_saida_02si-doc_s_ini,
                 wl_zglt035-doc_lcto    TO wa_saida_02si-doc_lcti,
                 v_objkey               TO wa_saida_02si-obj_keyi,
                 e_num_lote             TO wa_saida_02si-lt_i,
                 abap_false             TO wa_saida_02si-est_s_ini.
      ELSE.
        MOVE:
                 wl_zglt035-doc_lcto    TO wa_saida_02si-doc_lcti,
                 v_objkey               TO wa_saida_02si-obj_keyi,
                 e_num_lote             TO wa_saida_02si-lt_i.
      ENDIF.
    ELSEIF p_class EQ '02'.
      IF wa_saida_02-est_a_jur IS INITIAL AND wa_zib_contabilx-belnr IS NOT INITIAL.
        MOVE:    wa_zib_contabilx-belnr TO wa_saida_02-doc_a_jur,
                 wl_zglt035-doc_lcto    TO wa_saida_02-doc_lctj,
                 v_objkey               TO wa_saida_02-obj_keyj,
                 e_num_lote             TO wa_saida_02-lt_j,
                 abap_false             TO wa_saida_02-est_a_jur.
*               icon_led_green      TO wa_saida_02_temp-status.
      ELSE.
        MOVE:
                       wl_zglt035-doc_lcto    TO wa_saida_02-doc_lctj,
               v_objkey               TO wa_saida_02-obj_keyj,
               e_num_lote             TO wa_saida_02-lt_j.
      ENDIF.
    ELSE.
      IF wa_saida_02-est_lpcp IS INITIAL AND wa_zib_contabilx-belnr IS NOT INITIAL.
        MOVE:    wa_zib_contabilx-belnr TO wa_saida_02-doc_lpcp,
                 wl_zglt035-doc_lcto    TO wa_saida_02-doc_lctl,
                 v_objkey               TO wa_saida_02-obj_keyl,
                 e_num_lote             TO wa_saida_02-lt_l,
                 abap_false             TO wa_saida_02-est_lpcp,
                 icon_led_green         TO wa_saida_02-status.
      ELSE.
        MOVE:
                       wl_zglt035-doc_lcto    TO wa_saida_02-doc_lctl,
               v_objkey               TO wa_saida_02-obj_keyl,
               e_num_lote             TO wa_saida_02-lt_l.
      ENDIF.
    ENDIF.

    IF v_objkey IS NOT INITIAL.
      SELECT *
         FROM zib_contabil_err
         INTO TABLE @DATA(it_zib_contabil_err)
         WHERE obj_key  = @v_objkey.

      IF sy-subrc IS INITIAL AND it_zib_contabil_err[] IS NOT INITIAL.
        wa_saida_02-status = icon_led_red.

        IF p_class EQ '01'.
          MOVE:
                 wl_zglt035-doc_lcto    TO wa_saida_02si-doc_lcti,
                 e_num_lote             TO wa_saida_02si-lt_i,
                 v_objkey               TO wa_saida_02si-obj_keyi.

        ELSEIF p_class EQ '02'.
          MOVE:
                wl_zglt035-doc_lcto    TO wa_saida_02-doc_lctj,
                e_num_lote             TO wa_saida_02-lt_j,
                v_objkey               TO wa_saida_02-obj_keyj.
        ELSEIF p_class EQ '03'.
          MOVE:  wl_zglt035-doc_lcto    TO wa_saida_02-doc_lctl,
                 e_num_lote             TO wa_saida_02-lt_l,
                 v_objkey               TO wa_saida_02-obj_keyl.
        ENDIF.

      ELSE.
        REFRESH it_zib_contabil_err.
      ENDIF.

    ENDIF.

    CHECK lv_tabix IS NOT INITIAL.

    IF lv_ecomp IS NOT INITIAL AND lv_ecomp IS NOT INITIAL.
      MODIFY it_saida_02 FROM wa_saida_02si INDEX 1.
      wa_096fsi = CORRESPONDING #( wa_saida_02si ).
      wa_096fsi-cod_contrato = wa_092-cod_contrato.
      wa_096fsi-cmuser = sy-uname.
      MODIFY zglt096f FROM wa_096fsi.
      COMMIT WORK AND WAIT.
    ENDIF.

    IF wa_saida_02 IS NOT INITIAL.
      MODIFY it_saida_02 FROM wa_saida_02 INDEX lv_tabix.
      wa_096f = CORRESPONDING #( wa_saida_02 ).
      wa_096f-cod_contrato = wa_092-cod_contrato.
      wa_096f-cmuser = sy-uname.
      MODIFY zglt096f FROM wa_096f.
      COMMIT WORK AND WAIT.
    ENDIF.

    CLEAR: wa_096f, wa_saida_02, v_objkey, wa_096fsi, wa_saida_02si.

    IF p_class NE '01' AND p_class NE '02' AND wa_saida_02-doc_lpcp IS NOT INITIAL.
      MESSAGE 'Documentos gerados com sucesso!' TYPE 'S'.
    ELSEIF p_class NE '01' AND p_class NE '02'.
      MESSAGE 'Processo realizado!' TYPE 'S' DISPLAY LIKE 'S'.
    ENDIF.
  ENDIF. "FIM 131204 ZGL071 - Arrendamento - PSA GRAVA LOTE - Lote LP/CP
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estornar_documento . "Estorno
  DATA: var_answer TYPE c.

  DATA: it_msg        TYPE TABLE OF bdcmsgcoll, " WITH HEADER LINE,
        lv_comp       TYPE sy-datum,
        v_ultimo_dia  TYPE sy-datum,
        v_ultimo_diac TYPE char10,
        lv_text       TYPE string,
        wa_msg        TYPE bdcmsgcoll.

  CONCATENATE p_ano p_mes '01' INTO lv_comp.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_comp
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  READ TABLE it_saida_02 INTO DATA(wa_saida_02si) INDEX 1.

  READ TABLE it_saida_02 INTO wa_saida_02_temp WITH KEY fl_venc_comp = v_ultimo_dia. "INDEX 1.

  DATA(lv_tabix) = sy-tabix.

  CONCATENATE v_ultimo_dia+6(2) '.' v_ultimo_dia+4(2) '.' v_ultimo_dia(4) INTO v_ultimo_diac.

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

  IF sy-ucomm = 'ESTLPCP' AND wa_saida_02_temp-doc_lpcp IS NOT INITIAL. "Coluna LPCP
    CONCATENATE 'Deseja realmente estornar os documentos da competência'
    ' (' v_ultimo_dia ') e LP/CP?'
    INTO lv_text.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = lv_text
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

    shdb:
    'SAPMF05A' '0105' 'X'  ' '           ' ',
    ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
    ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
    ' '        ' '    ' '  'RF05A-BELNS' wa_saida_02_temp-doc_lpcp,
    ' '        ' '    ' '  'BKPF-BUKRS'  wa_092-bukrs,
    ' '        ' '    ' '  'RF05A-GJAHS' v_ultimo_dia(4),
    ' '        ' '    ' '  'BSIS-BUDAT' v_ultimo_diac,
    ' '        ' '    ' '  'UF05A-STGRD' '01'.

    opt-dismode = 'N'.
    CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

    IF sy-subrc IS INITIAL.

* Selecionar campo BKPF-STBLG
      SELECT stblg FROM bkpf
        UP TO 1 ROWS
        INTO @DATA(lv_stblg)
        WHERE bukrs EQ @wa_092-bukrs
          AND belnr EQ @wa_saida_02_temp-doc_lpcp.
      ENDSELECT.

      IF sy-subrc IS INITIAL AND lv_stblg IS NOT INITIAL.

        MOVE:    lv_stblg                    TO wa_saida_02_temp-est_lpcp,
                 abap_false                  TO wa_saida_02_temp-doc_lpcp.

      ELSE.
        MOVE:    abap_false  TO wa_saida_02_temp-est_lpcp,
                 icon_led_red TO wa_saida_02_temp-status.
      ENDIF.

      MODIFY it_saida_02_temp FROM wa_saida_02_temp INDEX lv_tabix.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSEIF sy-ucomm = 'ESTSALDOINICIAL' AND wa_saida_02si-doc_s_ini IS NOT INITIAL.

    CONCATENATE 'Deseja realmente estornar os documentos da competência'
  ' (' v_ultimo_dia ') e Saldo Inicial?'
  INTO lv_text.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = lv_text
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

    shdb:
    'SAPMF05A' '0105' 'X'  ' '           ' ',
    ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
    ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
    ' '        ' '    ' '  'RF05A-BELNS' wa_saida_02si-doc_s_ini,
    ' '        ' '    ' '  'BKPF-BUKRS'  wa_092-bukrs,
    ' '        ' '    ' '  'RF05A-GJAHS' v_ultimo_dia(4),
    ' '        ' '    ' '  'BSIS-BUDAT' v_ultimo_diac,
    ' '        ' '    ' '  'UF05A-STGRD' '01'.

    opt-dismode = 'N'.
    CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

    IF sy-subrc IS INITIAL.
* Selecionar campo BKPF-STBLG
      SELECT stblg FROM bkpf
        UP TO 1 ROWS
        INTO @lv_stblg
        WHERE bukrs EQ @wa_092-bukrs
          AND belnr EQ @wa_saida_02si-doc_s_ini.
      ENDSELECT.

      IF sy-subrc IS INITIAL AND lv_stblg IS NOT INITIAL.

        MOVE:    lv_stblg                    TO wa_saida_02si-est_s_ini,
                 abap_false                  TO wa_saida_02si-doc_s_ini.

      ELSE.
        MOVE:    abap_false  TO wa_saida_02si-est_s_ini,
                 icon_led_red TO wa_saida_02si-status.
      ENDIF.

      MODIFY it_saida_02_temp FROM wa_saida_02si INDEX 1.

      wa_096f = CORRESPONDING #( wa_saida_02si ).
      wa_096f-cod_contrato = wa_092-cod_contrato.
      wa_096f-cmuser = sy-uname.
      MODIFY zglt096f FROM wa_096f.
      COMMIT WORK AND WAIT.

    ELSE.
      CLEAR lv_stblg.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSEIF sy-ucomm = 'ESTAPROJUR' AND wa_saida_02_temp-doc_a_jur IS NOT INITIAL.
    CONCATENATE 'Deseja realmente estornar os documentos da competência'
    ' (' v_ultimo_dia ') e Apro. Juros?'
    INTO lv_text.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = lv_text
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

    shdb:
    'SAPMF05A' '0105' 'X'  ' '           ' ',
    ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
    ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
    ' '        ' '    ' '  'RF05A-BELNS' wa_saida_02_temp-doc_a_jur,
    ' '        ' '    ' '  'BKPF-BUKRS'  wa_092-bukrs,
    ' '        ' '    ' '  'RF05A-GJAHS' v_ultimo_dia(4),
    ' '        ' '    ' '  'BSIS-BUDAT' v_ultimo_diac,
    ' '        ' '    ' '  'UF05A-STGRD' '01'.

    opt-dismode = 'N'.
    CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

    IF sy-subrc IS INITIAL.

* Selecionar campo BKPF-STBLG
      SELECT stblg FROM bkpf
        UP TO 1 ROWS
        INTO @DATA(lv_stblgd)
        WHERE bukrs EQ @wa_092-bukrs
          AND belnr EQ @wa_saida_02_temp-doc_a_jur.
      ENDSELECT.

      IF sy-subrc IS INITIAL AND lv_stblgd IS NOT INITIAL.
        MOVE:    lv_stblgd    TO wa_saida_02_temp-est_a_jur,
                 abap_false  TO wa_saida_02_temp-doc_a_jur,
                 icon_led_yellow TO wa_saida_02_temp-status.

      ELSE.

        MOVE:    abap_false  TO wa_saida_02_temp-est_a_jur,

                 icon_led_red TO wa_saida_02_temp-status.

      ENDIF.


      MODIFY it_saida_02_temp FROM wa_saida_02_temp INDEX lv_tabix.

      wa_096f = CORRESPONDING #( wa_saida_02_temp ).
      wa_096f-cod_contrato = wa_092-cod_contrato.
      wa_096f-cmuser = sy-uname.
      MODIFY zglt096f FROM wa_096f.
      COMMIT WORK AND WAIT.
      CLEAR: wa_096f, wa_saida_02_temp.

      IF wa_saida_02_temp-doc_a_jur IS INITIAL.
        MESSAGE 'Documentos estornados com sucesso!' TYPE 'S'.
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE. "não tem documento a ser estornado!

    MESSAGE 'Não existe documento a ser estornado!' TYPE 'E'.
    EXIT.

  ENDIF.

  CALL METHOD g_alv->refresh_table_display( is_stable = wa_stable ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atualizar_documento .

  DATA: it_zglt036 TYPE TABLE OF zglt036.

* Seleções tab principais
  SELECT *
    FROM zglt096f " zglt096f - fluxo tb
    INTO TABLE it_096f
    WHERE cod_contrato EQ wa_092-cod_contrato.

  IF it_096f[] IS NOT INITIAL.
    SELECT *
       FROM zib_contabil_err
       INTO TABLE @DATA(it_zib_contabil_erri)
      FOR ALL ENTRIES IN @it_096f
       WHERE obj_key  EQ @it_096f-obj_keyi.

    SELECT *
       FROM zib_contabil_err
       INTO TABLE @DATA(it_zib_contabil_errj)
      FOR ALL ENTRIES IN @it_096f
       WHERE obj_key  EQ @it_096f-obj_keyj.

    SELECT *
       FROM zib_contabil_err
       INTO TABLE @DATA(it_zib_contabil_errl)
      FOR ALL ENTRIES IN @it_096f
       WHERE obj_key  EQ @it_096f-obj_keyl.

    SELECT *
       FROM zib_contabil_chv
       INTO TABLE @DATA(it_zib_contabil_chvi)
      FOR ALL ENTRIES IN @it_096f
       WHERE obj_key  EQ @it_096f-obj_keyi.

    SELECT *
       FROM zib_contabil_chv
       INTO TABLE @DATA(it_zib_contabil_chvj)
      FOR ALL ENTRIES IN @it_096f
       WHERE obj_key  EQ @it_096f-obj_keyj.

    SELECT *
       FROM zib_contabil_chv
       INTO TABLE @DATA(it_zib_contabil_chvl)
      FOR ALL ENTRIES IN @it_096f
       WHERE obj_key  EQ @it_096f-obj_keyl.
* -----
    SELECT bukrs, belnr, stblg FROM bkpf
      INTO TABLE @DATA(it_stblg_i)
      FOR ALL ENTRIES IN @it_096f
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @it_096f-doc_s_ini.

    SELECT bukrs, belnr, stblg FROM bkpf
      INTO TABLE @DATA(it_stblg_j)
      FOR ALL ENTRIES IN @it_096f
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @it_096f-doc_a_jur.

    SELECT bukrs, belnr, stblg FROM bkpf
      INTO TABLE @DATA(it_stblg_l)
      FOR ALL ENTRIES IN @it_096f
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @it_096f-doc_lpcp.

*----

    SELECT *
       FROM zglt035
     INTO TABLE it_035
      FOR ALL ENTRIES IN it_096f
      WHERE doc_lcto EQ it_096f-doc_lcti
        AND bukrs    EQ wa_092-bukrs
        AND tp_lcto  EQ '0000000000'.

    SELECT *
       FROM zglt035
     APPENDING TABLE it_035
      FOR ALL ENTRIES IN it_096f
      WHERE doc_lcto EQ it_096f-doc_lctj
        AND bukrs    EQ wa_092-bukrs
        AND tp_lcto  EQ '0000000000'.

    SELECT *
       FROM zglt035
     APPENDING TABLE it_035
      FOR ALL ENTRIES IN it_096f
      WHERE doc_lcto EQ it_096f-doc_lctl
        AND bukrs    EQ wa_092-bukrs
        AND tp_lcto  EQ '0000000000'.
  ENDIF.

  IF ( it_zib_contabil_chvi[] IS NOT INITIAL
    OR it_zib_contabil_chvj[] IS NOT INITIAL
    OR it_zib_contabil_chvl[] IS NOT INITIAL )
    AND it_096f[] IS NOT INITIAL. " Sucess

    LOOP AT it_saida_02 INTO DATA(wa_saida_02_tempupt).
      DATA(lv_tabix) = sy-tabix.

      READ TABLE it_096f INTO DATA(wa_096f) WITH KEY fl_venc_comp = wa_saida_02_tempupt-fl_venc_comp
                                                     obj_keyi     = wa_saida_02_tempupt-obj_keyi.
      DATA(lv_tabix96) = sy-tabix.
      IF ( sy-subrc IS INITIAL AND wa_096f-obj_keyi IS NOT INITIAL ) OR lv_tabix96 GT 0. " Maior / Modificado a condição / AOENNING

        READ TABLE it_zib_contabil_chvi INTO DATA(wa_zib_contabil_chvi) WITH KEY obj_key = wa_096f-obj_keyi.
        IF sy-subrc IS NOT INITIAL.
          CLEAR wa_zib_contabil_chvi.
        ENDIF.

        READ TABLE it_zib_contabil_chvj INTO DATA(wa_zib_contabil_chvj) WITH KEY obj_key = wa_096f-obj_keyj.
        IF sy-subrc IS NOT INITIAL.
          CLEAR wa_zib_contabil_chvj.
        ENDIF.

        READ TABLE it_zib_contabil_chvl INTO DATA(wa_zib_contabil_chvl) WITH KEY obj_key = wa_096f-obj_keyl.

        IF sy-subrc IS INITIAL AND ( wa_zib_contabil_chvi-belnr IS NOT INITIAL
                                  OR wa_zib_contabil_chvj-belnr IS NOT INITIAL
                                  OR wa_zib_contabil_chvl-belnr IS NOT INITIAL ).

          IF wa_zib_contabil_chvi-belnr IS NOT INITIAL." AND wa_zib_contabil_chvj-belnr IS NOT INITIAL AND wa_zib_contabil_chvl-belnr IS NOT INITIAL.
            wa_saida_02_tempupt-status = icon_led_green.
          ELSEIF wa_zib_contabil_chvj-belnr IS NOT INITIAL AND wa_zib_contabil_chvl-belnr IS NOT INITIAL.
            wa_saida_02_tempupt-status = icon_led_green.
          ELSEIF wa_zib_contabil_chvi-belnr IS INITIAL AND wa_zib_contabil_chvj-belnr IS INITIAL AND wa_zib_contabil_chvl-belnr IS INITIAL.
            wa_saida_02_tempupt-status = icon_led_yellow.
          ELSE.
            wa_saida_02_tempupt-status = icon_led_red.
          ENDIF.

*          IF wa_saida_02_tempupt-doc_lcti IS INITIAL.
*            wa_saida_02_tempupt-doc_lcti = wa_saida_02_tempupt-obj_keyi+5(10).
*            wa_saida_02_tempupt-doc_lctj = wa_saida_02_tempupt-obj_keyj+5(10).
*            wa_saida_02_tempupt-doc_lctl = wa_saida_02_tempupt-obj_keyl+5(10).
*            wa_096f-doc_lcti = wa_saida_02_tempupt-doc_lcti.
*            wa_096f-doc_lctj = wa_saida_02_tempupt-doc_lctj.
*            wa_096f-doc_lctl = wa_saida_02_tempupt-doc_lctl.
*          ENDIF.
*i
          IF wa_saida_02_tempupt-doc_s_ini IS INITIAL
            AND ( wa_saida_02_tempupt-doc_s_ini NE wa_saida_02_tempupt-est_s_ini
            OR  wa_saida_02_tempupt-est_s_ini IS INITIAL ).
            wa_saida_02_tempupt-doc_s_ini = wa_zib_contabil_chvi-belnr.
            wa_096f-doc_s_ini = wa_zib_contabil_chvi-belnr.

          ELSE.
            READ TABLE it_stblg_i INTO DATA(wa_stblgi) WITH KEY belnr = wa_096f-doc_s_ini.
            IF sy-subrc IS INITIAL AND wa_stblgi-stblg IS NOT INITIAL.

              wa_saida_02_tempupt-est_s_ini = wa_stblgi-stblg.
              wa_096f-est_s_ini = wa_stblgi-stblg.
            ELSE.
              CLEAR wa_stblgi.
            ENDIF.

          ENDIF.
*j
          IF wa_saida_02_tempupt-doc_a_jur IS INITIAL
            AND ( wa_saida_02_tempupt-doc_a_jur NE wa_saida_02_tempupt-est_a_jur
            OR  wa_saida_02_tempupt-est_a_jur IS INITIAL ).
            wa_saida_02_tempupt-doc_a_jur = wa_zib_contabil_chvj-belnr.
            wa_096f-doc_a_jur = wa_zib_contabil_chvj-belnr.
          ELSE.

            READ TABLE it_stblg_j INTO DATA(wa_stblgj) WITH KEY belnr = wa_096f-doc_a_jur.
            IF sy-subrc IS INITIAL AND wa_stblgj-stblg IS NOT INITIAL.

              wa_saida_02_tempupt-est_a_jur = wa_stblgj-stblg.
              wa_096f-est_a_jur = wa_stblgj-stblg.
            ELSE.
              CLEAR wa_stblgj.
            ENDIF.

          ENDIF.
*l
          IF wa_saida_02_tempupt-doc_lpcp IS INITIAL
            AND ( wa_saida_02_tempupt-doc_lpcp NE wa_saida_02_tempupt-est_lpcp
            OR  wa_saida_02_tempupt-est_lpcp IS INITIAL ).
            wa_saida_02_tempupt-doc_lpcp = wa_zib_contabil_chvl-belnr.
            wa_096f-doc_lpcp = wa_zib_contabil_chvl-belnr.
          ELSE.

            READ TABLE it_stblg_l INTO DATA(wa_stblgl) WITH KEY belnr = wa_096f-doc_lpcp.
            IF sy-subrc IS INITIAL AND wa_stblgl-stblg IS NOT INITIAL.

              wa_saida_02_tempupt-est_lpcp = wa_stblgl-stblg.
              wa_096f-est_lpcp = wa_stblgl-stblg.
            ELSE.
              CLEAR wa_stblgl.
            ENDIF.

          ENDIF.

* lote
          IF wa_saida_02_tempupt-doc_lcti IS NOT INITIAL.
            READ TABLE it_035 INTO DATA(wa_035) WITH KEY doc_lcto = wa_saida_02_tempupt-doc_lcti.
            IF sy-subrc IS INITIAL.
              wa_saida_02_tempupt-lt_i = wa_035-lote.
              wa_096f-lt_i = wa_035-lote.
            ENDIF.
          ENDIF.

          IF wa_saida_02_tempupt-doc_lctj IS NOT INITIAL.
            READ TABLE it_035 INTO DATA(wa_035j) WITH KEY doc_lcto = wa_saida_02_tempupt-doc_lctj.
            IF sy-subrc IS INITIAL.
              wa_saida_02_tempupt-lt_j = wa_035j-lote.
              wa_096f-lt_j = wa_035j-lote.
            ENDIF.
          ENDIF.

          IF wa_saida_02_tempupt-doc_lctl IS NOT INITIAL.
            READ TABLE it_035 INTO DATA(wa_035l) WITH KEY doc_lcto = wa_saida_02_tempupt-doc_lctl.
            IF sy-subrc IS INITIAL.
              wa_saida_02_tempupt-lt_l = wa_035l-lote.
              wa_096f-lt_l = wa_035l-lote.
            ENDIF.
          ENDIF.

          MODIFY it_saida_02 FROM wa_saida_02_tempupt INDEX lv_tabix. " WHERE doc_lcto = wa_saida_02_tempupt-doc_lcto.

*          IF wa_096f-ctime IS INITIAL.
*            wa_096f-ctime = sy-uzeit.
*          ENDIF.

          MODIFY it_096f FROM wa_096f INDEX lv_tabix96. " WHERE doc_lcto = wa_saida_02_tempupt-doc_lcto
*          MODIFY zglt096f FROM wa_096f. " WHERE doc_lcto = wa_saida_02_tempupt-doc_lcto
*          COMMIT WORK.
        ENDIF.
*
**      ELSE. " não encontrou verif err
**
**        READ TABLE it_zib_contabil_err INTO DATA(wa_zib_contabil_err) WITH KEY obj_key = wa_096e-obj_key.
**
**        IF sy-subrc IS NOT INITIAL.
**          CLEAR wa_zib_contabil_err.
**        ENDIF.
**
**        READ TABLE it_zib_contabil_errd INTO DATA(wa_zib_contabil_errd) WITH KEY obj_key = wa_096e-obj_keyd.
**
**        IF sy-subrc IS INITIAL OR wa_zib_contabil_err-message IS NOT INITIAL OR wa_zib_contabil_errd-message IS NOT INITIAL.
**
**          wa_saida_02_tempupt-status = icon_led_red.
**          MODIFY it_saida_02_temp FROM wa_saida_02_tempupt INDEX lv_tabix. " WHERE doc_lcto = wa_saida_02_tempupt-doc_lcto.
**
**        ELSE.
**          CLEAR wa_zib_contabil_errd.
**        ENDIF.
      ENDIF.
    ENDLOOP.

    IF it_096f[] IS NOT INITIAL.
      MODIFY zglt096f FROM TABLE it_096f.
      COMMIT WORK AND WAIT.
    ENDIF.

  ELSEIF ( it_zib_contabil_erri[] IS NOT INITIAL
    OR it_zib_contabil_errj[] IS NOT INITIAL
    OR it_zib_contabil_errl[] IS NOT INITIAL )
    AND it_096f[] IS NOT INITIAL. " err apenas

    LOOP AT it_saida_02 INTO DATA(wa_saida_02_tempuptf).
      DATA(lv_tabixf) = sy-tabix.

      READ TABLE it_096f INTO DATA(wa_096ff) WITH KEY fl_venc_comp = wa_saida_02_tempuptf-fl_venc_comp.
      IF ( sy-subrc IS INITIAL AND wa_096ff-obj_keyi IS NOT INITIAL ) OR lv_tabixf GT 1.

        READ TABLE it_zib_contabil_erri INTO DATA(wa_zib_contabil_erri) WITH KEY obj_key = wa_096ff-obj_keyi.
        IF wa_zib_contabil_erri IS INITIAL.
          CLEAR wa_zib_contabil_erri.
        ENDIF.

        READ TABLE it_zib_contabil_errj INTO DATA(wa_zib_contabil_errj) WITH KEY obj_key = wa_096ff-obj_keyj.
        IF wa_zib_contabil_errj IS INITIAL.
          CLEAR wa_zib_contabil_errj.
        ENDIF.

        READ TABLE it_zib_contabil_errl INTO DATA(wa_zib_contabil_errl) WITH KEY obj_key = wa_096ff-obj_keyl.

        IF sy-subrc IS INITIAL OR wa_zib_contabil_erri-message IS NOT INITIAL OR wa_zib_contabil_errj-message IS NOT INITIAL.

          wa_saida_02_tempuptf-status = icon_led_red.
          MODIFY it_saida_02 FROM wa_saida_02_tempuptf INDEX lv_tabixf.

        ENDIF.
      ELSE.
        CLEAR wa_096ff.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_DOCUMENTO_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5044   text
*----------------------------------------------------------------------*
FORM f_gerar_documento_03  USING  p_class TYPE zgle0029.

  DATA:
    dp_resp      TYPE char2,
    e_num_lote   TYPE zlote_num,
    wl_zglt035   TYPE zglt035,
    v_ultimo_dia TYPE sy-datum,
    gt_zglt032   TYPE TABLE OF zglt032,
    wa_zglt032   TYPE zglt032,
    gt_zglt036   TYPE TABLE OF zglt036,
    wl_zglt036   TYPE zglt036,
    v_objkey     TYPE char20,
    lv_dats      TYPE char10,
    lv_doc       TYPE zgle0004,
    lv_text      TYPE string,
    lv_cont      TYPE i,
    lv_int       TYPE zgle0004,
    lv_for       TYPE zgle0004,
    lv_comp      TYPE sy-datum,
    lv_ult_dia   TYPE sy-datum,
    lv_datum     TYPE sy-datum,
    v_qtdforn    TYPE i,
    v_data       TYPE sy-datum.


  MOVE: p_dtvenc TO v_data,
        v_data   TO v_ultimo_dia.

  READ TABLE it_saida_03 INTO wa_saida_03 WITH KEY dt_atualizacao = p_dtvenc.
  DATA(lv_tabix) = sy-tabix.

  IF p_class EQ '06'.

    CLEAR var_answer.

    IF sy-subrc IS INITIAL AND wa_saida_03-doc_contabil IS NOT INITIAL.
      MESSAGE 'Já existe documento gerado!' TYPE 'E'.
      EXIT.
    ELSEIF sy-subrc IS NOT INITIAL.
      MESSAGE 'Período informado inválido!' TYPE 'E'.
    ENDIF.

    CONCATENATE "v_ultimo_dia+6(2) '/'
                v_ultimo_dia+4(2) '/' v_ultimo_dia(4) INTO lv_dats.

    CONCATENATE 'Deseja realmente gerar o documento da competência'
    ' (' lv_dats ')?'
    INTO lv_text.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = lv_text
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
  ELSE.
    CHECK var_answer EQ '1'.
  ENDIF.

  IF wa_saida_03-doc_contabil IS NOT INITIAL.
    MESSAGE 'Já existe documento gerado!' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT *
    FROM zglt095
    INTO TABLE @DATA(it_zglt095)
    WHERE tp_arrendamento EQ @wa_092-tp_arrendamento
      AND classificacao EQ @p_class.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Parâmetros de Contabilização não encontrado para o tipo de contrato.' TYPE 'E'.
  ENDIF.

  "dp_resp = '83'. "Contabilidade
  dp_resp = '09'. "Contabilidade
  "PSA zcl_gerar_lote 3

  CALL METHOD zcl_gerar_lote=>create_lote
    EXPORTING
      i_bukrs      = wa_092-bukrs
      i_descr_lote = 'Arrendamento de Contratos'(002)
      i_user_resp  = sy-uname
      i_dep_resp   = dp_resp
    IMPORTING
      e_num_lote   = e_num_lote.

  MOVE:    e_num_lote                  TO wl_zglt035-lote,
           wa_092-bukrs                TO wl_zglt035-bukrs,
           ''                          TO wl_zglt035-tp_lcto,
           dp_resp                     TO wl_zglt035-dpto_resp,
           'BRL'                       TO wl_zglt035-moeda_doc,
           'LM'                        TO wl_zglt035-blart,
           TEXT-002                    TO wl_zglt035-bktxt,
           v_data                      TO wl_zglt035-budat,
           v_data                      TO wl_zglt035-bldat,
           sy-datum                    TO wl_zglt035-dt_lcto,
           ''                          TO wl_zglt035-prov_est,
           p_mes                       TO wl_zglt035-monat,
           p_ano                       TO wl_zglt035-gjahr,
           sy-uname                    TO wl_zglt035-usnam,
           sy-datum                    TO wl_zglt035-dt_entrada,
           sy-uzeit                    TO wl_zglt035-hr_entrada.

*  SELECT SINGLE * FROM zglt096c
*    INTO @DATA(wa_zglt096c)
*    WHERE cod_contrato EQ @wa_092-cod_contrato.

  SELECT * FROM zglt092a
    INTO TABLE @DATA(it_zglt092a)
    WHERE cod_contrato EQ @wa_092-cod_contrato
      AND ( tp_arrendamento EQ @wa_092-tp_arrendamento
      OR tp_arrendamento EQ @wa_092-tp_arrendamento+1(1) ).

  IF sy-subrc IS INITIAL.
    v_qtdforn = lines( it_zglt092a ).
  ENDIF.

  LOOP AT it_zglt092a ASSIGNING FIELD-SYMBOL(<f_zglt092a>). " Fornecedor/clientes

* Item 1 - forn/cliente bp circulante
    lv_cont = lv_cont + 1.

    IF it_zglt095[] IS NOT INITIAL.
      READ TABLE it_zglt095 INTO DATA(wa_zglt095f) INDEX lv_cont.
    ENDIF.

    wl_zglt036-seqitem = lv_cont.
    wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.

    IF <f_zglt092a>-fornecedor IS NOT INITIAL.
      wl_zglt036-hkont = <f_zglt092a>-fornecedor.
    ELSEIF <f_zglt092a>-cliente IS NOT INITIAL.
      wl_zglt036-hkont = <f_zglt092a>-cliente.
    ELSE.
      MESSAGE 'Cliente/Fornecedor não cadastrado!' TYPE 'E' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF <f_zglt092a>-cliente IS NOT INITIAL.
      wl_zglt036-bschl = '09'.
    ELSE.
      wl_zglt036-bschl = '39'.
    ENDIF.

**********************************************************************
    "122471 ZGL071- Arrendamento - PSA
    DATA dt_ultima TYPE zglt094-dt_vencimento.

    SELECT SINGLE dt_vencimento FROM zglt094
    INTO @dt_ultima
    WHERE cod_contrato = @wa_092-cod_contrato
    AND dt_vencimento >= @v_data.

    IF dt_ultima IS NOT INITIAL.
      CLEAR: v_data.
      v_data = dt_ultima.

    ENDIF.

**********************************************************************

    wl_zglt036-dt_vct = v_data.
    wl_zglt036-umskz = <f_zglt092a>-razao_especial.
    wl_zglt036-kostl =  ' '.
    wl_zglt036-anbwa = ' '.
    wl_zglt036-bewar = ' '.
    wl_zglt036-matnr = ' '.
    wl_zglt036-sgtxt = wa_zglt095f-texto_historico.
    wl_zglt036-gsber = wa_092-filial.
    wl_zglt036-zuonr = wa_092-atrib.

    lv_doc = abs( wa_saida_03-variacao_m_brl ).
    wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
    lv_int = abs( wa_saida_03-variacao_m_brl ).
    wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

*          lv_for = abs( wa_saida_03-vlr_atual_usd ).
    lv_for = abs( wa_saida_03-variacao_m_usd ).
    wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

*      IF wa_saida_02si-bp_cir_pri IS NOT INITIAL AND v_qtdforn IS NOT INITIAL.
*        lv_doc = abs( wa_saida_02si-bp_cir_pri / v_qtdforn ).
*        wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*      ENDIF.
*
*      lv_int = abs( wa_saida_02si-bp_cir_pri / v_qtdforn ).
*      wl_zglt036-vlr_moeda_int = CONV #( lv_int ).


*      IF wa_zglt095f-tp_taxa EQ '03'.
*        IF wa_saida_02si-bp_cir_pri IS NOT INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
** Taxa
*          lv_ukurs = abs( wl_tcurr-ukurs ).
*          lv_for = abs( ( wa_saida_02si-bp_cir_pri / v_qtdforn ) / lv_ukurs ).
*          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*        ELSE.
*          wl_zglt036-vlr_moeda_forte =  abs( ( wa_saida_02si-bp_cir_pri / v_qtdforn ) ).
*          IF lv_ukurs IS INITIAL.
*            lv_ukurs = 1.
*          ENDIF.
*          lv_for = abs( ( wa_saida_02si-bp_cir_pri / v_qtdforn ) / lv_ukurs ).
*          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*        ENDIF.
*      ELSEIF wa_zglt095f-tp_taxa EQ '04'.
** Taxa fut - wa_092-tx_usd_futuro
*        IF wa_saida_02si-bp_cir_pri IS NOT INITIAL AND wa_092-tx_usd_futuro IS NOT INITIAL.
** Taxa
*          lv_ukurs = abs( wa_092-tx_usd_futuro ).
*          lv_for = abs( ( wa_saida_02si-bp_cir_pri / v_qtdforn ) / lv_ukurs ).
*          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*        ELSE.
*          lv_for = abs( ( wa_saida_02si-bp_cir_pri / v_qtdforn ) ).
*          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*        ENDIF.
*      ENDIF.

    APPEND wl_zglt036 TO gt_zglt036.
    CLEAR: wl_zglt036.
  ENDLOOP.

  IF it_zglt095 IS NOT INITIAL.
    SORT it_zglt095 BY id_parametro.
  ENDIF.

  IF gt_zglt036 IS NOT INITIAL.
* Itens contabiliza
    LOOP AT it_zglt095 ASSIGNING FIELD-SYMBOL(<f_zglt095>).

      lv_cont = lv_cont + 1.

      wl_zglt036-seqitem = lv_cont .

      IF p_class EQ '06'.

        wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
        wl_zglt036-hkont = <f_zglt095>-conta.
        wl_zglt036-bschl = <f_zglt095>-chave_lcto.
        wl_zglt036-umskz = <f_zglt095>-razao_especial.

        wl_zglt036-anbwa = <f_zglt095>-tipo_mov_i.
        wl_zglt036-bewar = <f_zglt095>-bewar.
        wl_zglt036-matnr = <f_zglt095>-matnr.

        wl_zglt036-sgtxt = <f_zglt095>-texto_historico."wa_zglt032-sgtxt. 122421 Arrendamento - 03 - a receber - Texto contabilização PSA
        wl_zglt036-gsber = wa_092-filial.
        wl_zglt036-zuonr = wa_092-atrib.

        CASE lv_cont.

          WHEN '02'.

*      CASE sy-tabix.
*
*        WHEN '1'.
*          lv_doc = abs( wa_saida_03-variacao_m_brl ).
*          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*          lv_int = abs( wa_saida_03-variacao_m_brl ).
*          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
**          lv_for = abs( wa_saida_03-vlr_atual_usd ).
*          lv_for = abs( wa_saida_03-variacao_m_usd ).
*          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*
*        WHEN '2'.

            lv_doc = abs( wa_saida_03-variacao_m_brl ).
            wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
            lv_int = abs( wa_saida_03-variacao_m_brl ).
            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

*          lv_for = abs( wa_saida_03-vlr_atual_usd ).
            lv_for = abs( wa_saida_03-variacao_m_usd ).
            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*      ENDCASE.

          WHEN '03' OR '04'.
            lv_doc = abs( wa_saida_03-pis_brl ).
            wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
            lv_int = abs( wa_saida_03-pis_brl ).
            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

            lv_for = abs( wa_saida_03-pis_usd ).
            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

          WHEN '05' OR '06'.
            lv_doc = abs( wa_saida_03-cofins_brl ).
            wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
            lv_int = abs( wa_saida_03-cofins_brl ).
            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

            lv_for = abs( wa_saida_03-cofins_usd ).
            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

        ENDCASE.
      ENDIF.

      APPEND wl_zglt036 TO gt_zglt036.
      CLEAR: wl_zglt036.
    ENDLOOP.
  ELSE.

    LOOP AT it_zglt095 ASSIGNING <f_zglt095>.

      lv_cont = lv_cont + 1.

      wl_zglt036-seqitem = lv_cont .

      IF p_class EQ '06'.

        wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
        wl_zglt036-hkont = <f_zglt095>-conta.
        wl_zglt036-bschl = <f_zglt095>-chave_lcto.
        wl_zglt036-umskz = <f_zglt095>-razao_especial.

        wl_zglt036-anbwa = <f_zglt095>-tipo_mov_i.
        wl_zglt036-bewar = <f_zglt095>-bewar.
        wl_zglt036-matnr = <f_zglt095>-matnr.

        wl_zglt036-sgtxt = wa_zglt032-sgtxt.
        wl_zglt036-gsber = wa_092-filial.
        wl_zglt036-zuonr = wa_092-atrib.

        CASE lv_cont.

          WHEN '01'.
            lv_doc = abs( wa_saida_03-variacao_m_brl ).
            wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
            lv_int = abs( wa_saida_03-variacao_m_brl ).
            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

            lv_for = abs( wa_saida_03-variacao_m_usd ).
            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

          WHEN '02' OR '03'.
            lv_doc = abs( wa_saida_03-pis_brl ).
            wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
            lv_int = abs( wa_saida_03-pis_brl ).
            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

            lv_for = abs( wa_saida_03-pis_usd ).
            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

          WHEN '04' OR '05'.
            lv_doc = abs( wa_saida_03-cofins_brl ).
            wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
            lv_int = abs( wa_saida_03-cofins_brl ).
            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

            lv_for = abs( wa_saida_03-cofins_usd ).
            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

        ENDCASE.
      ENDIF.

      APPEND wl_zglt036 TO gt_zglt036.
      CLEAR: wl_zglt036.
    ENDLOOP.

  ENDIF.


  CLEAR wl_zglt036.
  LOOP AT gt_zglt036 INTO wl_zglt036.

    wa_zglt036_flg-doc_lcto        = wl_zglt036-doc_lcto.
    wa_zglt036_flg-seqitem         = wl_zglt036-seqitem.
    wa_zglt036_flg-seqsub          = wl_zglt036-seqsub.
    " wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
    " wa_zglt036_flg-fl_cv_moeda_int = abap_true.
    " wa_zglt036_flg-fl_cv_moeda_for = abap_true.
    wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
    APPEND  wa_zglt036_flg TO it_zglt036_flg.
  ENDLOOP.


  IF gt_zglt036[] IS NOT INITIAL.

    "PSA zcl_gerar_lote 4

    CALL METHOD zcl_gerar_lote=>contabilizar_lote(
      EXPORTING
        i_arredonda   = abap_true
        i_zglt036_flg = it_zglt036_flg
      CHANGING
        i_zglt036     = gt_zglt036
        i_zglt035     = wl_zglt035 ).

    COMMIT WORK AND WAIT.

    CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
      EXPORTING
        p_num_lote = e_num_lote.
    COMMIT WORK AND WAIT.
  ENDIF.


  "CHECK wl_zglt035-doc_lcto IS NOT INITIAL.
  CONCATENATE 'ZGL17' wl_zglt035-doc_lcto p_ano INTO v_objkey.


  SELECT *
    UP TO 1 ROWS
     FROM zib_contabil
     INTO @DATA(wa_zib_contabil)
     WHERE obj_key  = @v_objkey.
  ENDSELECT.

  SELECT *
    UP TO 1 ROWS
     FROM zib_contabil_chv
     INTO @DATA(wa_zib_contabilx)
     WHERE obj_key  = @v_objkey.
  ENDSELECT.

  IF p_class EQ '06'.
    IF wa_saida_03-doc_estorno IS INITIAL AND wa_zib_contabilx-belnr IS NOT INITIAL.
      MOVE:    wa_zib_contabilx-belnr TO wa_saida_03-doc_contabil,
               wl_zglt035-doc_lcto    TO wa_saida_03-doc_lcto,
               v_objkey               TO wa_saida_03-obj_key,
               e_num_lote             TO wa_saida_03-lt_doc,
               abap_false             TO wa_saida_03-doc_estorno.
    ENDIF.
  ENDIF.

  IF v_objkey IS NOT INITIAL.
    SELECT *
       FROM zib_contabil_err
       INTO TABLE @DATA(it_zib_contabil_err)
       WHERE obj_key  = @v_objkey.

    IF sy-subrc IS INITIAL AND it_zib_contabil_err[] IS NOT INITIAL.
      wa_saida_03-status = icon_led_red.

      IF p_class EQ '06'.
        MOVE:
               wl_zglt035-doc_lcto    TO wa_saida_03-doc_lcto,
               e_num_lote             TO wa_saida_03-lt_doc,
               v_objkey               TO wa_saida_03-obj_key.

      ENDIF.

    ELSE.
      MOVE:
             wl_zglt035-doc_lcto    TO wa_saida_03-doc_lcto,
             e_num_lote             TO wa_saida_03-lt_doc,
             v_objkey               TO wa_saida_03-obj_key.
      REFRESH it_zib_contabil_err.
    ENDIF.

  ENDIF.

  CHECK lv_tabix IS NOT INITIAL.
  MODIFY it_saida_03 FROM wa_saida_03 INDEX lv_tabix.

  wa_096c = CORRESPONDING #( wa_saida_03 ).
  wa_096c-mes = wa_saida_03-comp(2).
  wa_096c-ano = wa_saida_03-comp+3(4).
  wa_096c-qtd_sacas = wa_saida_03-saca_acumulada.
  wa_096c-preco_saca_brl = wa_saida_03-preco_saca.
  wa_096c-qtd_sacas_anual = wa_saida_03-preco_saca_a.
  wa_096c-vlr_parcmes_brl = wa_saida_03-vlr_p_m_sacas.
  wa_096c-vlr_atualizado_brl = wa_saida_03-vlr_atual_brl.
  wa_096c-variacao_mes_brl = wa_saida_03-variacao_m_brl.
  wa_096c-taxa_01 = wa_saida_03-ptax.
  wa_096c-variacao_mes_usd = wa_saida_03-variacao_m_usd.
  wa_096c-cod_contrato = wa_092-cod_contrato.
  wa_096c-usuario = sy-uname.
  wa_096c-data = sy-datum.
  wa_096c-hora = sy-uzeit.
  MODIFY zglt096c FROM wa_096c.
  COMMIT WORK AND WAIT.
  CLEAR: wa_096c, wa_saida_03, v_objkey.

  IF wa_saida_03-doc_contabil IS NOT INITIAL.
    MESSAGE 'Documentos gerados com sucesso!' TYPE 'S'.
  ELSEIF p_class NE '01' AND p_class NE '02'.
    MESSAGE 'Processo realizado!' TYPE 'S' DISPLAY LIKE 'S'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR_DOCUMENTO_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estornar_documento_03 .

  DATA: var_answer TYPE c.

  DATA: it_msg        TYPE TABLE OF bdcmsgcoll,
        lv_comp       TYPE sy-datum,
        lv_dat        TYPE char10,
        v_ultimo_dia  TYPE sy-datum,
        v_ultimo_diac TYPE char10,
        lv_text       TYPE string,
        wa_msg        TYPE bdcmsgcoll.

  READ TABLE it_saida_03 INTO wa_saida_03 WITH KEY dt_atualizacao = p_dtvenc.

  v_ultimo_dia = p_dtvenc.

  DATA(lv_tabix) = sy-tabix.
  IF wa_saida_03-doc_contabil IS INITIAL. " analisar
    MESSAGE 'Não existe documento a ser estornado!' TYPE 'E'.
    EXIT.
  ELSEIF wa_saida_03-doc_contabil IS NOT INITIAL.

    CONCATENATE v_ultimo_dia+4(2) '/' v_ultimo_dia(4) INTO lv_dat.

    CONCATENATE 'Deseja realmente estornar o documento da competência'
    ' (' lv_dat ')?'
    INTO lv_text.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = lv_text
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ELSE.
    MESSAGE 'Erro documentos a ser estornado!' TYPE 'E'.
    EXIT.
  ENDIF.

  CHECK var_answer EQ '1'.

  CHECK wa_saida_03-doc_contabil IS NOT INITIAL.

  CONCATENATE v_ultimo_dia+6(2) '.' v_ultimo_dia+4(2) '.' v_ultimo_dia(4) INTO v_ultimo_diac.

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
  ' '        ' '    ' '  'RF05A-BELNS' wa_saida_03-doc_contabil,
  ' '        ' '    ' '  'BKPF-BUKRS'  wa_092-bukrs,
  ' '        ' '    ' '  'RF05A-GJAHS' v_ultimo_diac(4),
  ' '        ' '    ' '  'BSIS-BUDAT'  v_ultimo_diac,
  ' '        ' '    ' '  'UF05A-STGRD' '01'.

  opt-dismode = 'N'.
  CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

  IF sy-subrc IS INITIAL.

* Selecionar campo BKPF-STBLG
    SELECT stblg FROM bkpf
      UP TO 1 ROWS
      INTO @DATA(lv_stblgd)
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @wa_saida_03-doc_contabil.
    ENDSELECT.

    IF sy-subrc IS INITIAL AND lv_stblgd IS NOT INITIAL.
      MOVE:    lv_stblgd        TO wa_saida_03-doc_estorno,
               abap_false       TO wa_saida_03-doc_contabil,
               icon_led_yellow  TO wa_saida_03-status.
    ELSE.
      MOVE:    abap_false  TO wa_saida_03-doc_estorno,
               icon_led_red TO wa_saida_03-status.
    ENDIF.


    MODIFY it_saida_03 FROM wa_saida_03 INDEX lv_tabix.

    wa_096c = CORRESPONDING #( wa_saida_03 ).
    wa_096c-cod_contrato = wa_092-cod_contrato.
    wa_096c-dt_atualizacao = sy-datum.
    wa_096c-usuario = sy-uname.
    MODIFY zglt096c FROM wa_096c.
    COMMIT WORK AND WAIT.
    CLEAR: wa_096c, wa_saida_03.

    IF wa_saida_03-doc_contabil IS INITIAL.
      MESSAGE 'Documento estornado com sucesso!' TYPE 'S'.
    ENDIF.

  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_DOCUMENTO_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atualizar_documento_03 .

  DATA: it_zglt036 TYPE TABLE OF zglt036.

* Seleções dados tab principais - PERFORM f_select_dados.
  SELECT *
    FROM zglt096c " zglt096c - fluxo arrendamento
    INTO TABLE it_096c
    WHERE cod_contrato EQ wa_092-cod_contrato
      AND mes NE space.

  IF it_096c[] IS NOT INITIAL.

    SELECT *
       FROM zib_contabil_err " Log de Erros de processamento contábeis
       INTO TABLE @DATA(it_zib_contabil_err)
      FOR ALL ENTRIES IN @it_096c
       WHERE obj_key  EQ @it_096c-obj_key.

    SELECT *
       FROM zib_contabil_chv  " Tabela de controle de Chave de referência
       INTO TABLE @DATA(it_zib_contabil_chv)
      FOR ALL ENTRIES IN @it_096c
       WHERE obj_key  EQ @it_096c-obj_key.

    SELECT bukrs, belnr, stblg
      FROM bkpf    " Cabeçalho do documento contábil
      INTO TABLE @DATA(it_stblg)
      FOR ALL ENTRIES IN @it_096c
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @it_096c-doc_contabil.

    SELECT *
       FROM zglt035 " Lançamentos Manuais - Cabeçalho
     INTO TABLE it_035
      FOR ALL ENTRIES IN it_096c
      WHERE doc_lcto EQ it_096c-doc_lcto
        AND bukrs    EQ wa_092-bukrs
        AND tp_lcto  EQ '0000000000'.

    SORT it_096c BY dt_atualizacao.
  ENDIF.

  IF it_zib_contabil_chv[] IS NOT INITIAL AND it_096c[] IS NOT INITIAL. " Sucess

    LOOP AT it_saida_03 INTO DATA(wa_saida_03).
      DATA(lv_tabix) = sy-tabix.

      READ TABLE it_096c INTO DATA(wa_096c) WITH KEY dt_atualizacao = wa_saida_03-dt_atualizacao.
      DATA(lv_tabix96) = sy-tabix.
      IF sy-subrc IS INITIAL AND wa_096c-obj_key IS NOT INITIAL.

        READ TABLE it_zib_contabil_chv INTO DATA(wa_zib_contabil_chv) WITH KEY obj_key = wa_096c-obj_key.

        IF sy-subrc IS INITIAL AND ( wa_zib_contabil_chv-belnr IS NOT INITIAL ).

          IF wa_zib_contabil_chv-belnr IS NOT INITIAL.
            wa_saida_03-status = icon_led_green.
          ELSEIF wa_zib_contabil_chv-belnr IS INITIAL.
            wa_saida_03-status = icon_led_yellow.
          ENDIF.

          IF wa_saida_03-doc_lcto IS INITIAL AND wa_saida_03-obj_key+5(10) IS NOT INITIAL.
            wa_saida_03-doc_lcto = wa_saida_03-obj_key+5(10).
            wa_096c-doc_lcto = wa_saida_03-doc_lcto.
          ENDIF.
* Doc contabil
          IF wa_saida_03-doc_contabil IS INITIAL
            AND ( wa_saida_03-doc_contabil NE wa_saida_03-doc_estorno
            OR  wa_saida_03-doc_estorno IS INITIAL ).
            wa_saida_03-doc_contabil = wa_zib_contabil_chv-belnr.
            wa_096c-doc_contabil = wa_zib_contabil_chv-belnr.

          ELSE.
            READ TABLE it_stblg INTO DATA(wa_stblg) WITH KEY belnr = wa_096c-doc_contabil.
            IF sy-subrc IS INITIAL AND wa_stblg-stblg IS NOT INITIAL.

              wa_saida_03-doc_estorno = wa_stblg-stblg.
              wa_096c-doc_estorno = wa_stblg-stblg.
            ELSE.
              CLEAR wa_stblg.
            ENDIF.
          ENDIF.

* Lote
          IF wa_saida_03-doc_lcto IS NOT INITIAL.
            READ TABLE it_035 INTO DATA(wa_035) WITH KEY doc_lcto = wa_saida_03-doc_lcto.
            IF sy-subrc IS INITIAL.
              wa_saida_03-lt_doc = wa_035-lote.
              wa_096c-lt_doc = wa_035-lote.
            ENDIF.
          ENDIF.

          IF p_mes IS NOT INITIAL.
            wa_096c-mes = p_mes.
          ENDIF.

          IF p_ano IS NOT INITIAL.
            wa_096c-ano = p_ano.
          ENDIF.

          MODIFY it_saida_03 FROM wa_saida_03 INDEX lv_tabix.
          MODIFY it_096c FROM wa_096c INDEX lv_tabix96. "Tabela Contratos de Arrendamento ( Cálculo ) Por Área
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF it_096c[] IS NOT INITIAL.
      MODIFY zglt096c FROM TABLE it_096c.
      COMMIT WORK AND WAIT.
    ENDIF.

  ELSEIF it_zib_contabil_err[] IS NOT INITIAL AND it_096c[] IS NOT INITIAL. " err apenas

    LOOP AT it_saida_03 INTO wa_saida_03.
      DATA(lv_tabixf) = sy-tabix.

      READ TABLE it_096c INTO DATA(wa_096cc) WITH KEY dt_atualizacao = wa_saida_03-dt_atualizacao.
      IF sy-subrc IS INITIAL AND wa_096cc-obj_key IS NOT INITIAL.

        READ TABLE it_zib_contabil_err INTO DATA(wa_zib_contabil_err) WITH KEY obj_key = wa_096cc-obj_key.

        IF sy-subrc IS INITIAL OR wa_zib_contabil_err-message IS NOT INITIAL.

          wa_saida_03-status = icon_led_red.
          MODIFY it_saida_03 FROM wa_saida_03 INDEX lv_tabixf.
        ENDIF.
      ELSE.
        CLEAR wa_096cc.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONSOLIDACAO_DE_DADOS_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consolidacao_de_dados_03 .

  DATA: tabix        TYPE sy-tabix,
        lv_cont      TYPE i,
        lv_comp      TYPE sy-datum,
        v_ultimo_dia TYPE sy-datum,
        lv_saldocp   TYPE zgle0004,
        lv_soma      TYPE zgle0004,
        vl_jirso     TYPE zgle0004.

  TRY .
      DATA(dt_vigencia) = it_096[ 1 ]-vigencia_de.
    CATCH cx_sy_itab_line_not_found.
      CLEAR dt_vigencia.
  ENDTRY.

  CHECK dt_vigencia IS NOT INITIAL.

  dt_vigencia = |{ dt_vigencia(6) }01|.

  PERFORM interval_date USING '-' CHANGING dt_vigencia.

  SORT it_094 BY dt_vencimento DESCENDING.

  TRY .
      DATA(dt_vencimento) = it_094[ 1 ]-dt_vencimento.
    CATCH cx_sy_itab_line_not_found.
      CLEAR dt_vencimento.
  ENDTRY.

  p_ano = p_dtvenc(4).
  p_mes = p_dtvenc+4(2).

  CONCATENATE p_ano p_mes '01' INTO lv_comp.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_comp
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  CHECK v_ultimo_dia EQ p_dtvenc.

  DO.

    IF dt_vigencia >= dt_vencimento.
      EXIT.
    ENDIF.

    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = dt_vigencia
      IMPORTING
        e_date = dt_vigencia.

    APPEND
    VALUE #(
              dt_atualizacao  = dt_vigencia
              comp            = |{ dt_vigencia+4(2) }/{ dt_vigencia(4) }|
           ) TO it_saida_03.

    dt_vigencia = |{ dt_vigencia(6) }01|.

    PERFORM interval_date USING '+' CHANGING dt_vigencia.

  ENDDO.

  DATA: wa_zglt094    TYPE zglt094,
        wa_094_mesfim TYPE zglt094,
        it_094_mesfim TYPE STANDARD TABLE OF zglt094 WITH HEADER LINE,
        dt_ano(4)     TYPE c,
        dt_mes(2)     TYPE c,
        dt_dia(2)     TYPE c,
        dt_fim(6)     TYPE c,
        vlr_acum      TYPE zglt094-saca_acumulada.

  "PSA - aqui trabalho mes acumulado, o primeiro mês traz o valor mas no proximeo venciomento é o mes + 1
  SORT it_094 BY dt_vencimento ASCENDING.
  MOVE-CORRESPONDING it_094[] TO it_094_mesfim[].

  SORT it_094 BY dt_vencimento ASCENDING.
  READ TABLE it_094[] INTO DATA(aux_094) INDEX 1.

  SORT it_096 BY vigencia_de ASCENDING.
  READ TABLE it_096 INTO DATA(aux_it_096) INDEX 1.
  "Aqui adiciona a linha de acumulado menor que declarado na tabela

  MOVE  aux_it_096-vigencia_de TO it_094_mesfim-dt_vencimento.
  MOVE aux_094-saca_acumulada TO it_094_mesfim-saca_acumulada.
  APPEND it_094_mesfim.


  SORT it_094_mesfim BY dt_vencimento ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_094_mesfim COMPARING dt_vencimento.

  LOOP AT it_094_mesfim[] ASSIGNING FIELD-SYMBOL(<it_094_mesfim>).

    tabix = sy-tabix.

    IF tabix = 1.
      dt_ano = <it_094_mesfim>-dt_vencimento+0(4).
      dt_mes = <it_094_mesfim>-dt_vencimento+4(2).
      dt_dia = <it_094_mesfim>-dt_vencimento+6(2).
    ELSE.

      IF <it_094_mesfim>-dt_vencimento+4(2) = 12.
        dt_ano = <it_094_mesfim>-dt_vencimento+0(4) + 1.
        dt_dia = <it_094_mesfim>-dt_vencimento+6(2).
        dt_mes = '01'.
      ELSE.
        dt_ano = <it_094_mesfim>-dt_vencimento+0(4).
        dt_mes = <it_094_mesfim>-dt_vencimento+4(2) + 1.
        dt_dia = <it_094_mesfim>-dt_vencimento+6(2).
        IF dt_mes <= 9.
          CONDENSE dt_mes NO-GAPS.
          UNPACK dt_mes TO dt_mes.
        ENDIF.
      ENDIF.


    ENDIF.


    dt_fim =  dt_ano && dt_mes && dt_dia.
    <it_094_mesfim>-dt_vencimento = dt_fim.
  ENDLOOP.

  CLEAR: dt_ano,dt_mes,dt_dia,dt_fim,tabix.

  "Remove datas que são menores que o inicio do contrato
  DELETE it_saida_03 WHERE dt_atualizacao(6) < aux_it_096-vigencia_de(6).

  LOOP AT it_saida_03 ASSIGNING FIELD-SYMBOL(<f_saida>).
    tabix = sy-tabix.

    READ TABLE it_096 INTO  wa_096 WITH KEY cod_contrato = p_cont
                                            vigencia_de(6)  = <f_saida>-dt_atualizacao(6).
    IF sy-subrc IS INITIAL AND wa_096-sacas_p_ano IS NOT INITIAL.
      <f_saida>-preco_saca_a  = wa_096-sacas_p_ano.  " Saca parcela anual

    ELSE.
      READ TABLE it_096 INTO  wa_096 WITH KEY cod_contrato = p_cont.
      IF sy-subrc IS INITIAL.
        <f_saida>-preco_saca_a  = wa_096-sacas_p_ano.  " Saca parcela anual
      ELSE.
        CLEAR wa_096.
      ENDIF.
    ENDIF.

    IF <f_saida>-preco_saca_a IS NOT INITIAL.
      <f_saida>-vlr_p_m_sacas  = <f_saida>-preco_saca_a  / 12.   " 'Vlr Parcela Mensal'
    ENDIF.

    READ TABLE it_zglt100 INTO DATA(wa_zglt100) WITH KEY
*                                                         cod_contrato    = p_cont
                                                         regiao          = wa_092-municipio
                                                         competencia(6)  = <f_saida>-dt_atualizacao(6).
    "131204 ZGL071 - Arrendamento - PSA TIPO 3
    CLEAR: wa_zglt100-ptax,vlr_out_taxa.
    PERFORM pega_taxa USING <f_saida>-dt_atualizacao CHANGING vlr_out_taxa.
    wa_zglt100-ptax = vlr_out_taxa.


    SORT it_094_mesfim BY dt_vencimento ASCENDING.
    READ TABLE it_094_mesfim INTO DATA(aux_094_mesfim) INDEX 1.

    IF <f_saida>-dt_atualizacao(6) < aux_094_mesfim-dt_vencimento.
      vlr_acum = aux_094_mesfim-saca_acumulada.
    ELSEIF aux_094_mesfim-dt_vencimento = <f_saida>-dt_atualizacao(6).
      vlr_acum = aux_094_mesfim-saca_acumulada.
    ELSE.
      READ TABLE it_094_mesfim INTO wa_094_mesfim WITH KEY dt_vencimento(6)  = <f_saida>-dt_atualizacao(6).
      vlr_acum = wa_094_mesfim-saca_acumulada.
    ENDIF.

    IF sy-subrc IS INITIAL.
      <f_saida>-saca_acumulada    =  vlr_acum."wa_zglt100-saca_acumulada.
      <f_saida>-preco_saca        =  wa_zglt100-preco_saca. " Preço saca mes

      IF <f_saida>-saca_acumulada IS INITIAL.
        READ TABLE it_saida_03 INTO DATA(wa_saida_03_ant) INDEX ( tabix - 1 ).
        IF sy-subrc IS INITIAL.
          <f_saida>-saca_acumulada    = <f_saida>-vlr_p_m_sacas  + wa_saida_03_ant-saca_acumulada.
        ELSE.
          CLEAR wa_saida_03_ant.
          <f_saida>-saca_acumulada    = <f_saida>-vlr_p_m_sacas .
        ENDIF.
      ENDIF.

    ELSE.

      READ TABLE it_saida_03 INTO wa_saida_03_ant INDEX ( tabix - 1 ).
      IF sy-subrc IS INITIAL.
        <f_saida>-saca_acumulada    = <f_saida>-vlr_p_m_sacas + wa_saida_03_ant-saca_acumulada.
      ELSE.
        CLEAR wa_saida_03_ant.
        <f_saida>-saca_acumulada    = <f_saida>-vlr_p_m_sacas .
      ENDIF.
    ENDIF.

    <f_saida>-vlr_atual_brl =  <f_saida>-saca_acumulada *  <f_saida>-preco_saca. " 'Vlr Atualizado BRL'

    READ TABLE it_saida_03 INTO wa_saida_03_ant INDEX ( tabix - 1 ).
    IF sy-subrc IS INITIAL AND wa_saida_03_ant-vlr_atual_brl IS NOT INITIAL.
      <f_saida>-variacao_m_brl  =  <f_saida>-vlr_atual_brl - wa_saida_03_ant-vlr_atual_brl.
    ELSE.
      <f_saida>-variacao_m_brl  = <f_saida>-vlr_atual_brl.
    ENDIF.

    IF wa_zglt100-ptax IS NOT INITIAL.
      ".  121595 Arrendamento - Erro Variação USD A receber e campo município - PSA
      "<f_saida>-variacao_m_usd  =  <f_saida>-vlr_atual_brl / wa_zglt100-ptax.
      <f_saida>-variacao_m_usd  =  <f_saida>-variacao_m_brl / wa_zglt100-ptax.
      <f_saida>-ptax  =  wa_zglt100-ptax.
    ELSE.
      <f_saida>-variacao_m_usd  = 0.
      "<f_saida>-variacao_m_usd  =  <f_saida>-vlr_atual_usd.
    ENDIF.

* Pis e Cofins - RJF
    <f_saida>-pis_brl    = ( <f_saida>-variacao_m_brl * ( wa_096-pis / 100 ) ).
    <f_saida>-pis_usd    = ( <f_saida>-variacao_m_usd * ( wa_096-pis / 100 ) ).
    <f_saida>-cofins_brl = ( <f_saida>-variacao_m_brl * ( wa_096-cofins / 100 ) ).
    <f_saida>-cofins_usd = ( <f_saida>-variacao_m_usd * ( wa_096-cofins / 100 ) ).

    IF <f_saida>-doc_contabil IS INITIAL.
      <f_saida>-status = icon_led_yellow.
    ELSEIF <f_saida>-doc_contabil IS NOT INITIAL.
      <f_saida>-status = icon_led_green.
    ENDIF.

    IF <f_saida>-dt_atualizacao(6) EQ v_ultimo_dia(6).
      <f_saida>-color = 'C300'.
    ENDIF.
  ENDLOOP.

* Lines uptate
  IF it_096c[] IS NOT INITIAL.

    SELECT *
    FROM zib_contabil_err
    INTO TABLE @DATA(it_ziberr)
    FOR ALL ENTRIES IN @it_096c
    WHERE obj_key EQ @it_096c-obj_key.

    LOOP AT it_saida_03 ASSIGNING <f_saida>.
      DATA(lv_tabix) = sy-tabix.
      READ TABLE it_096c INTO wa_096c WITH KEY dt_atualizacao(6) = <f_saida>-dt_atualizacao(6).

      IF sy-subrc IS INITIAL.
        IF wa_096c-obj_key IS NOT INITIAL.
          READ TABLE it_ziberr INTO DATA(wa_ziberr) WITH KEY obj_key = wa_096c-obj_key.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_ziberr.
          ENDIF.
        ENDIF.

      ELSE.
        CLEAR: wa_096c, wa_ziberr.
      ENDIF.

      IF wa_096c-doc_contabil IS NOT INITIAL
        AND wa_ziberr-message IS INITIAL.
*        <f_saida> = CORRESPONDING #( wa_096c ).
        <f_saida>-status = icon_led_green.
*        <f_saida>-status = icon_led_yellow.
        <f_saida>-doc_lcto = wa_096c-doc_lcto.
        <f_saida>-lt_doc  = wa_096c-lt_doc.
        <f_saida>-obj_key = wa_096c-obj_key.
        <f_saida>-saca_acumulada = wa_096c-qtd_sacas.
        <f_saida>-preco_saca = wa_096c-preco_saca_brl.
        <f_saida>-preco_saca_a = wa_096c-qtd_sacas_anual.
        <f_saida>-vlr_p_m_sacas = wa_096c-vlr_parcmes_brl.
        <f_saida>-vlr_atual_brl = wa_096c-vlr_atualizado_brl.
        <f_saida>-variacao_m_brl = wa_096c-variacao_mes_brl.
        <f_saida>-ptax = wa_096c-taxa_01.
        <f_saida>-variacao_m_usd = wa_096c-variacao_mes_usd.

      ELSEIF wa_096c-doc_contabil IS INITIAL AND wa_096c-obj_key IS NOT INITIAL.

        <f_saida>-status = icon_led_yellow.
        <f_saida>-doc_lcto = wa_096c-doc_lcto.
        <f_saida>-lt_doc  = wa_096c-lt_doc.
        <f_saida>-obj_key = wa_096c-obj_key.
*        <f_saida>-saca_acumulada = wa_096c-qtd_sacas.
*        <f_saida>-preco_saca = wa_096c-preco_saca_brl.
*        <f_saida>-preco_saca_a = wa_096c-qtd_sacas_anual.
*        <f_saida>-vlr_p_m_sacas = wa_096c-vlr_parcmes_brl.
*        <f_saida>-vlr_atual_brl = wa_096c-vlr_atualizado_brl.
*        <f_saida>-variacao_m_brl = wa_096c-variacao_mes_brl.
*        <f_saida>-ptax = wa_096c-taxa_01.
*        <f_saida>-variacao_m_usd = wa_096c-variacao_mes_usd.

      ELSEIF wa_096c-doc_contabil IS INITIAL
        AND ( wa_ziberr-message IS INITIAL ).
        <f_saida>-status = icon_led_yellow.
      ELSEIF ( wa_096c-doc_contabil IS NOT INITIAL OR wa_096c-doc_contabil IS INITIAL )
        AND wa_ziberr-message IS NOT INITIAL .
        <f_saida>-status = icon_led_red.
      ELSE.
        <f_saida>-status = icon_led_yellow.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_DOCUMENTO_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5710   text
*----------------------------------------------------------------------*
FORM f_gerar_documento_05 USING p_class TYPE zgle0029.

  DATA:
    dp_resp      TYPE char2,
    e_num_lote   TYPE zlote_num,
    wl_zglt035   TYPE zglt035,
    v_ultimo_dia TYPE sy-datum,
    gt_zglt032   TYPE TABLE OF zglt032,
    wa_zglt032   TYPE zglt032,
    gt_zglt036   TYPE TABLE OF zglt036,
    wl_zglt036   TYPE zglt036,
    v_objkey     TYPE char20,
    lv_dats      TYPE char10,
    lv_doc       TYPE zgle0004,
    lv_text      TYPE string,
    lv_tabix95   TYPE i,
    lv_cont      TYPE i,
    vl_data      TYPE c LENGTH 10,
    data         TYPE sy-datum,
    lv_vbund     TYPE rassc,
    wl_tcurr     TYPE tcurr,
    lv_ukurs     TYPE zgle0018,
    lv_datap     TYPE sy-datum,
    lv_int       TYPE zgle0004,
    lv_for       TYPE zgle0004,
    lv_comp      TYPE sy-datum,
    lv_bukrs     TYPE bukrs,
    lv_ult_dia   TYPE sy-datum,
    lv_datum     TYPE sy-datum,
    t_ska1       TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
    lv_saknr     TYPE saknr,
    v_qtdforn    TYPE i,
    v_data       TYPE sy-datum.

  MOVE: p_dtvenc TO v_data,
        v_data   TO v_ultimo_dia.

* Taxa
  lv_datap = v_data + 1.
  zcl_util=>conv_data_us_br( EXPORTING i_data = lv_datap
                             RECEIVING e_data = vl_data ).

  " Função standard para converter data em formato gravado na TCURR
  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = vl_data
    IMPORTING
      output = data.

  " Tabela responsavel por gravar a taxa do câmbio.
  SELECT SINGLE *
    FROM tcurr
     INTO wl_tcurr
   WHERE fcurr EQ 'BRL'
     AND tcurr EQ 'USD'
    AND kurst EQ 'B'
     AND gdatu EQ data.
  IF sy-subrc IS INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
    lv_ukurs = abs( wl_tcurr-ukurs ).
    IF lv_ukurs IS INITIAL.
      lv_ukurs = 1.
    ENDIF.
  ENDIF.

  READ TABLE it_saida_05 INTO wa_saida_05 WITH KEY dt_atualizacao = p_dtvenc.
  DATA(lv_tabix) = sy-tabix.

  IF p_class EQ '08'.

    CLEAR var_answer.

    IF sy-subrc IS INITIAL AND wa_saida_05-doc_contabil IS NOT INITIAL.
      MESSAGE 'Já existe documento gerado!' TYPE 'E'.
      EXIT.
    ELSEIF sy-subrc IS NOT INITIAL.
      MESSAGE 'Período informado inválido!' TYPE 'E'.
    ENDIF.

    CONCATENATE "v_ultimo_dia+6(2) '/'
                v_ultimo_dia+4(2) '/' v_ultimo_dia(4) INTO lv_dats.

    CONCATENATE 'Deseja realmente gerar o documento da competência'
    ' (' lv_dats ')?'
    INTO lv_text.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = lv_text
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
  ELSE.
    CHECK var_answer EQ '1'.
  ENDIF.

  IF wa_saida_05-doc_contabil IS NOT INITIAL.
    MESSAGE 'Já existe documento gerado!' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT *
    FROM zglt095
    INTO TABLE @DATA(it_zglt095)
    WHERE tp_arrendamento EQ @wa_092-tp_arrendamento.
*      AND classificacao EQ @p_class.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Parâmetros de Contabilização não encontrado para o tipo de contrato.' TYPE 'E'.
  ENDIF.


  IF it_zglt095[] IS NOT INITIAL.
    READ TABLE it_zglt095 INTO DATA(wa_zglt095x) WITH KEY classificacao = p_class.
    IF sy-subrc IS INITIAL.
      IF wa_zglt095x-classificacao EQ '07'.

        lv_bukrs = wa_092-bukrsp.


        IF lv_bukrs IS NOT INITIAL. " New RJF
          SELECT * INTO @DATA(wa_j_1bbranch)
            UP TO 1 ROWS
            FROM j_1bbranch
           WHERE branch NE '0001'
             AND bukrs  EQ @lv_bukrs
             AND cgc_branch EQ '0001'.
          ENDSELECT.
        ENDIF.

*        SELECT * FROM zglt092a
*          INTO TABLE @DATA(it_zglt092a)
*          WHERE cod_contrato EQ @wa_092-cod_contrato
*            AND ( tp_arrendamento EQ @wa_092-tp_arrendamento
*            OR tp_arrendamento EQ @wa_092-tp_arrendamento+1(1) ).

*        IF sy-subrc IS INITIAL.
*          v_qtdforn = lines( it_zglt092a ).
*          READ TABLE it_zglt092a INTO DATA(wa_zglt092a) INDEX 1.
*          IF sy-subrc IS INITIAL.
*            IF wa_zglt092a-fornecedor IS NOT INITIAL.
*              wa_092-bukrs = wa_zglt092a-fornecedor.
*            ELSE.
*              wa_092-bukrs = wa_zglt092a-cliente.
*            ENDIF.
*          ENDIF.
*        ENDIF.

      ELSE.
        lv_bukrs = wa_092-bukrs.
      ENDIF.
    ENDIF.
  ENDIF.

  "dp_resp = '83'. "Contabilidade
  dp_resp = '09'. "Contabilidade
  "PSA zcl_gerar_lote 5
  CALL METHOD zcl_gerar_lote=>create_lote
    EXPORTING
      i_bukrs      = lv_bukrs
      i_descr_lote = 'Arrendamento de Contratos'(002)
      i_user_resp  = sy-uname
      i_dep_resp   = dp_resp
    IMPORTING
      e_num_lote   = e_num_lote.

  MOVE:    e_num_lote                  TO wl_zglt035-lote,
*           wa_092-bukrs                TO wl_zglt035-bukrs,
           lv_bukrs                    TO wl_zglt035-bukrs,
           ''                          TO wl_zglt035-tp_lcto,
           dp_resp                     TO wl_zglt035-dpto_resp,
           'BRL'                       TO wl_zglt035-moeda_doc,
           'LM'                        TO wl_zglt035-blart,
           TEXT-002                    TO wl_zglt035-bktxt,
           v_data                      TO wl_zglt035-budat,
           v_data                      TO wl_zglt035-bldat,
           sy-datum                    TO wl_zglt035-dt_lcto,
           ''                          TO wl_zglt035-prov_est,
           p_mes                       TO wl_zglt035-monat,
           p_ano                       TO wl_zglt035-gjahr,
           sy-uname                    TO wl_zglt035-usnam,
           sy-datum                    TO wl_zglt035-dt_entrada,
           sy-uzeit                    TO wl_zglt035-hr_entrada.

* Itens contabiliza
*BUG 99658 - BG - INICIO

  DATA(li_arr) = wa_092-tp_arrendamento.
  SHIFT li_arr LEFT DELETING LEADING '0'.

  DATA(li_class) = p_class.
  SHIFT li_class LEFT DELETING LEADING '0'.


  SELECT SINGLE * FROM zglt092a
    INTO @DATA(w_zgl0t092a)
    WHERE cod_contrato EQ @wa_092-cod_contrato
    AND ( classificacao EQ @p_class
         OR classificacao EQ @li_class )
      AND ( tp_arrendamento EQ @wa_092-tp_arrendamento
             OR tp_arrendamento EQ @li_arr ).

  IF w_zgl0t092a IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_zgl0t092a-fornecedor
      IMPORTING
        output = w_zgl0t092a-fornecedor.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_zgl0t092a-cliente
      IMPORTING
        output = w_zgl0t092a-cliente.

  ENDIF.

  IF p_class EQ '07' OR p_class EQ '08'.

    lv_cont = 1.

    wl_zglt036-seqitem = lv_cont.

    IF w_zgl0t092a-fornecedor IS NOT INITIAL.
      wl_zglt036-hkont = w_zgl0t092a-fornecedor.
      wl_zglt036-bschl = '39'.

      SELECT SINGLE sortl "New-RJF
        FROM lfa1
        INTO @DATA(lv_sortl)
        WHERE lifnr EQ @w_zgl0t092a-fornecedor.
      IF sy-subrc IS INITIAL AND lv_sortl IS NOT INITIAL.
        wl_zglt036-vbund = lv_sortl(06).
        lv_vbund = wl_zglt036-vbund.
      ENDIF.
    ELSE.
      wl_zglt036-hkont = w_zgl0t092a-cliente.
      wl_zglt036-bschl = '09'.

      SELECT SINGLE vbund "New-RJF
        FROM kna1
        INTO  wl_zglt036-vbund
        WHERE kunnr EQ w_zgl0t092a-cliente.

      IF sy-subrc IS INITIAL AND wl_zglt036-vbund IS NOT INITIAL.
        lv_vbund = wl_zglt036-vbund.
      ENDIF.
    ENDIF.

    wl_zglt036-dt_vct  = v_data. "New-RJF

    wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
    wl_zglt036-umskz = w_zgl0t092a-razao_especial.  "zgl0t092A-razao_especial. <f_zglt095>-razao_especial.

    wl_zglt036-anbwa = ' '.
    wl_zglt036-bewar = ' '.
    wl_zglt036-matnr = ' '.

    IF it_zglt095[] IS NOT INITIAL.
      READ TABLE it_zglt095 INTO DATA(wa_zglt095_01) WITH KEY classificacao = p_class.
      IF sy-subrc IS INITIAL.
        wl_zglt036-sgtxt = wa_zglt095_01-texto_historico.
      ENDIF.
    ENDIF.

*    wl_zglt036-gsber = wa_092-filial. "New RJF

    IF wa_zglt095x-classificacao EQ '07' AND wa_j_1bbranch-branch IS NOT INITIAL. "New RJF
      wl_zglt036-gsber = wa_j_1bbranch-branch.
    ELSE.
      wl_zglt036-gsber = wa_092-filial.
    ENDIF.

    wl_zglt036-zuonr = wa_092-atrib.

    lv_doc = abs( wa_saida_05-valor_brl ).
    wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).

    lv_int = abs( wa_saida_05-valor_brl ). " fech...
    wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
    lv_for = abs( wa_saida_05-valor_usd ). " fech...
    wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

*    IF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*      lv_int = abs( wa_saida_05-valor_brl / lv_ukurs ). " fech...
*      wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*      lv_for = abs( wa_saida_05-valor_brl / lv_ukurs ). " fech...
*      wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*    ELSEIF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*      lv_int = abs( wa_saida_05-valor_brl ). " fech...
*      wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*      lv_for = abs( wa_saida_05-valor_brl ). " fech...
*      wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*    ENDIF.


    APPEND wl_zglt036 TO gt_zglt036.
    CLEAR: wl_zglt036.


  ENDIF.

  IF it_zglt095 IS NOT INITIAL.
    SORT it_zglt095 BY id_parametro.
  ENDIF.

  LOOP AT it_zglt095 ASSIGNING FIELD-SYMBOL(<f_zglt095>) WHERE classificacao EQ p_class.

    lv_cont = lv_cont + 1.

    wl_zglt036-seqitem = lv_cont .


    wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
    wl_zglt036-hkont = <f_zglt095>-conta.
    wl_zglt036-bschl = <f_zglt095>-chave_lcto.
    wl_zglt036-umskz = <f_zglt095>-razao_especial.

    wl_zglt036-anbwa = <f_zglt095>-tipo_mov_i.
    wl_zglt036-bewar = <f_zglt095>-bewar.
    wl_zglt036-matnr = <f_zglt095>-matnr.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class         = '0000'
        setnr         = 'CONTAS_EC-CS'
      TABLES
        set_values    = t_ska1
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      lv_saknr = wl_zglt036-hkont.
      READ TABLE t_ska1 INTO DATA(lv_conta) WITH KEY from = lv_saknr.
      IF sy-subrc IS INITIAL AND lv_conta-from IS NOT INITIAL.
        wl_zglt036-vbund   = lv_vbund. "New-RJF
      ENDIF.

    ENDIF.

    IF wa_092-tp_arrendamento = 5 AND p_class = '08'. "PSA OBJTO CENTRO CUSTO IGUAL A 5 E glaccount_type = 'P'

      SELECT SINGLE glaccount_type FROM ska1 WHERE saknr = @lv_saknr AND glaccount_type = 'P' INTO @aux_glaccount_type.

      IF aux_glaccount_type = 'P'.
        wl_zglt036-kostl = wa_092-kostl.
      ENDIF.

      CLEAR aux_glaccount_type.

    ENDIF.

    IF wa_092-tp_arrendamento = 5 AND p_class = '07'. "PSA OBJTO CENTRO LUCRO IGUAL A 5 E glaccount_type = 'P'

      SELECT SINGLE glaccount_type FROM ska1 WHERE saknr = @lv_saknr AND glaccount_type = 'P' INTO @aux_glaccount_type.

      IF aux_glaccount_type = 'P'.
        wl_zglt036-prctr = wa_092-prctr.
        wl_zglt036-matnr = wa_092-matnr.
      ENDIF.

      CLEAR aux_glaccount_type.

    ENDIF.

    wl_zglt036-sgtxt = <f_zglt095>-texto_historico."wa_zglt032-sgtxt.


*    wl_zglt036-gsber = wa_092-filial.

    IF wa_zglt095x-classificacao EQ '07' AND wa_j_1bbranch-branch IS NOT INITIAL.
      wl_zglt036-gsber = wa_j_1bbranch-branch.
    ELSE.
      wl_zglt036-gsber = wa_092-filial.
    ENDIF.


    wl_zglt036-zuonr = wa_092-atrib.

*    IF <f_zglt095>-classificacao EQ '08'.
    IF p_class EQ '08'.

      CASE lv_cont.

*        WHEN '1'.
        WHEN '2'.

** ----
          lv_doc = abs( ( wa_saida_05-valor_brl ) - ( wa_saida_05-pis_brl ) - ( wa_saida_05-cofins_brl ) ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
          lv_int = abs( ( wa_saida_05-valor_brl ) - ( wa_saida_05-pis_brl ) - ( wa_saida_05-cofins_brl ) ).
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

          lv_for = abs( ( wa_saida_05-valor_usd ) - ( wa_saida_05-pis_usd ) - ( wa_saida_05-cofins_usd ) ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
** ----

**          lv_doc = abs( ( ( wa_saida_05-valor_brl ) - ( wa_saida_05-pis_brl ) - ( wa_saida_05-cofins_brl ) ) ).
**          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
**
**          IF <f_zglt095>-tp_taxa EQ '03'.
**
**            IF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
**              lv_int = abs( ( ( wa_saida_05-valor_brl ) - ( wa_saida_05-pis_brl ) - ( wa_saida_05-cofins_brl ) ) / lv_ukurs ).
**              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
**              lv_for = abs( ( ( wa_saida_05-valor_brl ) - ( wa_saida_05-pis_brl ) - ( wa_saida_05-cofins_brl ) ) / lv_ukurs ).
**              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
**            ELSEIF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
**              lv_int = abs( ( ( wa_saida_05-valor_brl ) - ( wa_saida_05-pis_brl ) - ( wa_saida_05-cofins_brl ) ) ). " fech...
**              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
**              lv_for = abs( ( ( wa_saida_05-valor_brl ) - ( wa_saida_05-pis_brl ) - ( wa_saida_05-cofins_brl ) ) ). " fech...
**              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
**            ENDIF.
**          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
**
***            lv_doc = abs( ( ( wa_saida_05-valor_usd ) - ( wa_saida_05-pis_usd ) - ( wa_saida_05-cofins_usd ) ) ).
***            wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
**
**            lv_int = abs( ( ( wa_saida_05-valor_usd ) - ( wa_saida_05-pis_usd ) - ( wa_saida_05-cofins_usd ) ) ).
**            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
**            lv_for = abs( ( ( wa_saida_05-valor_usd ) - ( wa_saida_05-pis_usd ) - ( wa_saida_05-cofins_usd ) ) ).
**            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
**          ENDIF.


*          lv_doc = abs( wa_saida_05-valor_brl ).
*          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*
*          IF <f_zglt095>-tp_taxa EQ '03'.
*            IF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-valor_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-valor_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ELSEIF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-valor_brl ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-valor_brl ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
*            lv_int = abs( wa_saida_05-valor_usd ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*            lv_for = abs( wa_saida_05-valor_usd ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.

**          APPEND wl_zglt036 TO gt_zglt036.
**          CLEAR: wl_zglt036.
**
**          lv_cont = lv_cont + 1.
**          wl_zglt036-seqitem = lv_cont .
**
**          IF w_zgl0t092a-fornecedor IS NOT INITIAL.
**            wl_zglt036-hkont = w_zgl0t092a-fornecedor.
**            wl_zglt036-bschl = '39'.
**          ELSE.
**            wl_zglt036-hkont = w_zgl0t092a-cliente.
**            wl_zglt036-bschl = '09'.
**          ENDIF.
**
**
**
**          wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
**          wl_zglt036-umskz = w_zgl0t092a-razao_especial.  "zgl0t092A-razao_especial. <f_zglt095>-razao_especial.
**
**          wl_zglt036-anbwa = ' '.
**          wl_zglt036-bewar = ' '.
**          wl_zglt036-matnr = ' '.
**
**
**          wl_zglt036-sgtxt = <f_zglt095>-texto_historico.
**
**
**          wl_zglt036-gsber = wa_092-filial.
**
**          lv_doc = abs( wa_saida_05-valor_brl ).
**          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
**          lv_int = abs( wa_saida_05-valor_brl ).
**          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
**          lv_for = abs( wa_saida_05-valor_brl ).
**          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

*          lv_doc = abs( wa_saida_05-valor_brl ).
*          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*
*          IF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*            lv_int = abs( wa_saida_05-valor_brl / lv_ukurs ). " fech...
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*            lv_for = abs( wa_saida_05-valor_brl / lv_ukurs ). " fech...
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ELSEIF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*            lv_int = abs( wa_saida_05-valor_brl ). " fech...
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*            lv_for = abs( wa_saida_05-valor_brl ). " fech...
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.

        WHEN '3'.

          lv_doc = abs( wa_saida_05-pis_brl ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
          lv_int = abs( wa_saida_05-pis_brl ). " fech...
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
          lv_for = abs( wa_saida_05-pis_usd ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

*          lv_doc = abs( wa_saida_05-pis_brl ).
*          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*
*          IF <f_zglt095>-tp_taxa EQ '03'.
*
*            IF wa_saida_05-pis_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-pis_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-pis_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ELSEIF wa_saida_05-pis_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-pis_brl ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-pis_brl ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
*            lv_int = abs( wa_saida_05-pis_usd ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*            lv_for = abs( wa_saida_05-pis_usd ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.

        WHEN '4'.

          lv_doc = abs( wa_saida_05-cofins_brl ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
          lv_int = abs( wa_saida_05-cofins_brl  ).
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
          lv_for = abs( wa_saida_05-cofins_usd ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

*          lv_doc = abs( wa_saida_05-cofins_brl ).
*          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*
*          IF <f_zglt095>-tp_taxa EQ '03'.
*
*            IF wa_saida_05-cofins_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-cofins_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-cofins_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ELSEIF wa_saida_05-cofins_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-cofins_brl ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-cofins_brl ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
*            lv_int = abs( wa_saida_05-cofins_usd ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*            lv_for = abs( wa_saida_05-cofins_usd ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.

*          APPEND wl_zglt036 TO gt_zglt036.
*          CLEAR: wl_zglt036.
*
*          lv_cont = lv_cont + 1.
*          wl_zglt036-seqitem = lv_cont .
*
*          IF w_zgl0t092a-fornecedor IS NOT INITIAL.
*            wl_zglt036-hkont = w_zgl0t092a-fornecedor.
*            wl_zglt036-bschl = '39'.
*          ELSE.
*            wl_zglt036-hkont = w_zgl0t092a-cliente.
*            wl_zglt036-bschl = '09'.
*          ENDIF.
*
*          wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
*          wl_zglt036-umskz = w_zgl0t092a-razao_especial.  "zgl0t092A-razao_especial. <f_zglt095>-razao_especial.
*
*          wl_zglt036-anbwa = ' '.
*          wl_zglt036-bewar = ' '.
*          wl_zglt036-matnr = ' '.
*
*
*          wl_zglt036-sgtxt = <f_zglt095>-texto_historico.
*
*
*          wl_zglt036-gsber = wa_092-filial.
*
*          lv_doc = abs( wa_saida_05-pis_brl +  wa_saida_05-cofins_brl ).
*          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*          lv_int = abs( wa_saida_05-pis_usd +  wa_saida_05-cofins_usd ).
*          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*          lv_for = abs( wa_saida_05-pis_usd +  wa_saida_05-cofins_usd ).
*          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).


*        WHEN '4'.
*          lv_doc = abs( wa_saida_05-cofins_brl ).
*          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*
*          IF <f_zglt095>-tp_taxa EQ '03'.
*
*            IF wa_saida_05-cofins_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-cofins_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-cofins_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ELSEIF wa_saida_05-cofins_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-cofins_brl ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-cofins_brl ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
*            lv_int = abs( wa_saida_05-cofins_usd ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*            lv_for = abs( wa_saida_05-cofins_usd ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.
*
*        WHEN '5'.
*          lv_doc = abs( wa_saida_05-pis_brl +  wa_saida_05-cofins_brl ).
*          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*          lv_int = abs( wa_saida_05-pis_usd +  wa_saida_05-cofins_usd ).
*          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*          lv_for = abs( wa_saida_05-pis_usd +  wa_saida_05-cofins_usd ).
*          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
      ENDCASE.
    ENDIF.

*    IF <f_zglt095>-classificacao EQ '07'.
    IF p_class EQ '07'.

      CASE lv_cont.

        WHEN '2'.

*          lv_doc = abs( wa_saida_05-valor_brl ).
*          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*
*          IF <f_zglt095>-tp_taxa EQ '03'.
*            IF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-valor_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-valor_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ELSEIF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-valor_brl ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-valor_brl ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
*            lv_int = abs( wa_saida_05-valor_usd ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*            lv_for = abs( wa_saida_05-valor_usd ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.

          lv_doc = abs( wa_saida_05-valor_brl ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).

          lv_int = abs( wa_saida_05-valor_brl ). " fech...
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
          lv_for = abs( wa_saida_05-valor_usd ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).


*          IF <f_zglt095>-tp_taxa EQ '03'.
*            IF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-valor_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-valor_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ELSEIF wa_saida_05-valor_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-valor_brl ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-valor_brl ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
*            lv_int = abs( wa_saida_05-valor_usd ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*            lv_for = abs( wa_saida_05-valor_usd ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.

        WHEN '3'.
*          lv_doc = abs( wa_saida_05-pis_brl ).
          lv_doc = abs( wa_saida_05-pis_arrend ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).

          lv_int = abs( wa_saida_05-pis_arrend ). " fech...
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
          lv_for = abs( wa_saida_05-pis_u_arrend ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).


*          IF <f_zglt095>-tp_taxa EQ '03'.
**            IF wa_saida_05-pis_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
**              lv_int = abs( wa_saida_05-pis_brl / lv_ukurs ). " fech...
*            IF wa_saida_05-pis_arrend IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-pis_arrend / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
**              lv_for = abs( wa_saida_05-pis_brl / lv_ukurs ). " fech...
*              lv_for = abs( wa_saida_05-pis_arrend / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
**            ELSEIF wa_saida_05-pis_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
**              lv_int = abs( wa_saida_05-pis_brl ). " fech...
*            ELSEIF wa_saida_05-pis_arrend IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-pis_arrend ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
**              lv_for = abs( wa_saida_05-pis_brl ). " fech...
*              lv_for = abs( wa_saida_05-pis_arrend ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
**            lv_int = abs( wa_saida_05-pis_usd ).
*            lv_int = abs( wa_saida_05-pis_u_arrend ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
**            lv_for = abs( wa_saida_05-pis_usd ).
*            lv_for = abs( wa_saida_05-pis_u_arrend ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.

        WHEN '4'.

*          lv_doc = abs( wa_saida_05-pis_brl ).
*          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*
*          IF <f_zglt095>-tp_taxa EQ '03'.
*            IF wa_saida_05-pis_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-pis_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-pis_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ELSEIF wa_saida_05-pis_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-pis_brl ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-pis_brl ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
*            lv_int = abs( wa_saida_05-pis_usd ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*            lv_for = abs( wa_saida_05-pis_usd ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.

          lv_doc = abs( wa_saida_05-pis_arrend ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).

          lv_int = abs( wa_saida_05-pis_arrend ). " fech...
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
          lv_for = abs( wa_saida_05-pis_u_arrend ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

*          IF <f_zglt095>-tp_taxa EQ '03'.
*            IF wa_saida_05-pis_arrend IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-pis_arrend / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-pis_arrend / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ELSEIF wa_saida_05-pis_arrend IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-pis_arrend ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-pis_arrend ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
*            lv_int = abs( wa_saida_05-pis_u_arrend ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*            lv_for = abs( wa_saida_05-pis_u_arrend ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.

        WHEN '5'.
          lv_doc = abs( wa_saida_05-cofins_arrend ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).

          lv_int = abs( wa_saida_05-cofins_arrend ). " fech...
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
          lv_for = abs( wa_saida_05-cofins_u_arrend ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

*          IF <f_zglt095>-tp_taxa EQ '03'.
*            IF wa_saida_05-cofins_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-cofins_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-cofins_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ELSEIF wa_saida_05-cofins_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-cofins_brl ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-cofins_brl ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
*            lv_int = abs( wa_saida_05-cofins_usd ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*            lv_for = abs( wa_saida_05-cofins_usd ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.

        WHEN '6'.
          lv_doc = abs( wa_saida_05-cofins_arrend ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).

          lv_int = abs( wa_saida_05-cofins_arrend ). " fech...
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
          lv_for = abs( wa_saida_05-cofins_u_arrend ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

*          IF <f_zglt095>-tp_taxa EQ '03'.
*            IF wa_saida_05-cofins_brl IS NOT INITIAL AND lv_ukurs IS NOT INITIAL.
*              lv_int = abs( wa_saida_05-cofins_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-cofins_brl / lv_ukurs ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ELSEIF wa_saida_05-cofins_brl IS NOT INITIAL AND lv_ukurs IS INITIAL.
*              lv_int = abs( wa_saida_05-cofins_brl ). " fech...
*              wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*              lv_for = abs( wa_saida_05-cofins_brl ). " fech...
*              wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*            ENDIF.
*          ELSEIF <f_zglt095>-tp_taxa EQ '04'.
*            lv_int = abs( wa_saida_05-cofins_usd ).
*            wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*            lv_for = abs( wa_saida_05-cofins_usd ).
*            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*          ENDIF.
      ENDCASE.
    ENDIF.

    APPEND wl_zglt036 TO gt_zglt036.
    CLEAR: wl_zglt036.
  ENDLOOP.

  CLEAR wl_zglt036.

  DELETE gt_zglt036 WHERE vlr_moeda_doc = 0. "PSA Se o valor for igual a 0 não gerar linha na contabilidade

  LOOP AT gt_zglt036 INTO wl_zglt036.

    wa_zglt036_flg-doc_lcto        = wl_zglt036-doc_lcto.
    wa_zglt036_flg-seqitem         = wl_zglt036-seqitem.
    wa_zglt036_flg-seqsub          = wl_zglt036-seqsub.
    " wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
    " wa_zglt036_flg-fl_cv_moeda_int = abap_true.
    " wa_zglt036_flg-fl_cv_moeda_for = abap_true.
    wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
    APPEND  wa_zglt036_flg TO it_zglt036_flg.
  ENDLOOP.

  IF gt_zglt036[] IS NOT INITIAL.

    "PSA zcl_gerar_lote 6
    CALL METHOD zcl_gerar_lote=>contabilizar_lote(
      EXPORTING
        i_arredonda   = abap_true
        i_zglt036_flg = it_zglt036_flg
      CHANGING
        i_zglt036     = gt_zglt036
        i_zglt035     = wl_zglt035 ).

    COMMIT WORK AND WAIT.

    CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
      EXPORTING
        p_num_lote = e_num_lote.
    COMMIT WORK AND WAIT.

  ENDIF.

  "CHECK wl_zglt035-doc_lcto IS NOT INITIAL.
  CONCATENATE 'ZGL17' wl_zglt035-doc_lcto p_ano INTO v_objkey.

  SELECT SINGLE *
     FROM zib_contabil
     INTO @DATA(wa_zib_contabil)
     WHERE obj_key  = @v_objkey.

  SELECT SINGLE *
     FROM zib_contabil_chv
     INTO @DATA(wa_zib_contabilx)
     WHERE obj_key  = @v_objkey.

  IF p_class EQ '08'.
    IF wa_saida_05-doc_estorno IS INITIAL AND wa_zib_contabilx-belnr IS NOT INITIAL.
      MOVE:    wa_zib_contabilx-belnr TO wa_saida_05-doc_contabil,
               wl_zglt035-doc_lcto    TO wa_saida_05-doc_lcto,
               v_objkey               TO wa_saida_05-obj_key,
               e_num_lote             TO wa_saida_05-lt_doc,
               abap_false             TO wa_saida_05-doc_estorno.
*  ELSE.
*    MOVE:
*             wl_zglt035-doc_lcto    TO wa_saida_05-doc_lcto,
*             v_objkey               TO wa_saida_05-obj_key,
*             e_num_lote             TO wa_saida_05-lt_doc,
*             abap_false             TO wa_saida_05-doc_estorno.
    ENDIF.
  ENDIF.

  IF p_class EQ '07'.
    IF wa_saida_05-doc_estornor IS INITIAL AND wa_zib_contabilx-belnr IS NOT INITIAL.
      MOVE:    wa_zib_contabilx-belnr TO wa_saida_05-doc_contabilr,
               wl_zglt035-doc_lcto    TO wa_saida_05-doc_lctor,
               v_objkey               TO wa_saida_05-obj_keyr,
               e_num_lote             TO wa_saida_05-lt_docr,
               abap_false             TO wa_saida_05-doc_estornor.
*  ELSE.
*    MOVE:
*             wl_zglt035-doc_lcto    TO wa_saida_05-doc_lctor,
*             v_objkey               TO wa_saida_05-obj_keyr,
*             e_num_lote             TO wa_saida_05-lt_docr,
*             abap_false             TO wa_saida_05-doc_estornor.
    ENDIF.
  ENDIF.

  IF v_objkey IS NOT INITIAL.
    SELECT DISTINCT *
       FROM zib_contabil_err
       INTO TABLE @DATA(it_zib_contabil_err)
       WHERE obj_key  = @v_objkey.

    IF p_class EQ '08'.
      IF sy-subrc IS INITIAL AND it_zib_contabil_err[] IS NOT INITIAL.
        wa_saida_05-status = icon_led_red.
        MOVE:
               wl_zglt035-doc_lcto    TO wa_saida_05-doc_lcto,
               e_num_lote             TO wa_saida_05-lt_doc,
               v_objkey               TO wa_saida_05-obj_key.

      ELSE.
        MOVE:
               wl_zglt035-doc_lcto    TO wa_saida_05-doc_lcto,
               e_num_lote             TO wa_saida_05-lt_doc,
               v_objkey               TO wa_saida_05-obj_key.
*      REFRESH it_zib_contabil_err.
      ENDIF.

    ELSEIF p_class EQ '07'.
      IF sy-subrc IS INITIAL AND it_zib_contabil_err[] IS NOT INITIAL.
        wa_saida_05-status = icon_led_red.
        MOVE:
               wl_zglt035-doc_lcto    TO wa_saida_05-doc_lctor,
               e_num_lote             TO wa_saida_05-lt_docr,
               v_objkey               TO wa_saida_05-obj_keyr.
      ELSE.
        MOVE:
               wl_zglt035-doc_lcto    TO wa_saida_05-doc_lctor,
               e_num_lote             TO wa_saida_05-lt_docr,
               v_objkey               TO wa_saida_05-obj_keyr.
      ENDIF.
    ENDIF.
  ENDIF.
  REFRESH it_zib_contabil_err.
  CHECK lv_tabix IS NOT INITIAL.
  MODIFY it_saida_05 FROM wa_saida_05 INDEX lv_tabix.

  wa_096g = CORRESPONDING #( wa_saida_05 ).
  wa_096g-mes = wa_saida_05-comp(2).
  wa_096g-ano = wa_saida_05-comp+3(4).
  wa_096g-cod_contrato = wa_092-cod_contrato.
  wa_096g-usuario = sy-uname.
  wa_096g-data = sy-datum.
  wa_096g-hora = sy-uzeit.
  MODIFY zglt096g FROM wa_096g.
  COMMIT WORK AND WAIT.
  CLEAR: wa_096g, wa_saida_05, v_objkey, lv_cont.

  IF wa_saida_05-doc_contabil IS NOT INITIAL AND wa_saida_05-doc_contabilr IS NOT INITIAL.
    MESSAGE 'Documentos gerados com sucesso!' TYPE 'S'.
  ELSE.
    MESSAGE 'Processo realizado!' TYPE 'S' DISPLAY LIKE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR_DOCUMENTO_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estornar_documento_05 .

  DATA: var_answer TYPE c.

  DATA: it_msg        TYPE TABLE OF bdcmsgcoll,
        lv_comp       TYPE sy-datum,
        lv_dat        TYPE char10,
        v_ultimo_dia  TYPE sy-datum,
        v_ultimo_diac TYPE char10,
        lv_text       TYPE string,
        wa_msg        TYPE bdcmsgcoll.

  READ TABLE it_saida_05 INTO wa_saida_05 WITH KEY dt_atualizacao = p_dtvenc.

  v_ultimo_dia = p_dtvenc.

  DATA(lv_tabix) = sy-tabix.
  IF wa_saida_05-doc_contabil IS INITIAL. " analisar
    MESSAGE 'Não existe documento a ser estornado!' TYPE 'E'.
    EXIT.
  ELSEIF wa_saida_05-doc_contabil IS NOT INITIAL.

    CONCATENATE v_ultimo_dia+4(2) '/' v_ultimo_dia(4) INTO lv_dat.

    CONCATENATE 'Deseja realmente estornar o documento da competência'
    ' (' lv_dat ')?'
    INTO lv_text.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = lv_text
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ELSE.
    MESSAGE 'Erro documentos a ser estornado!' TYPE 'E'.
    EXIT.
  ENDIF.

  CHECK var_answer EQ '1'.

  CHECK wa_saida_05-doc_contabil IS NOT INITIAL.

  CONCATENATE v_ultimo_dia+6(2) '.' v_ultimo_dia+4(2) '.' v_ultimo_dia(4) INTO v_ultimo_diac.


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
  ' '        ' '    ' '  'RF05A-BELNS' wa_saida_05-doc_contabilr,
  ' '        ' '    ' '  'BKPF-BUKRS'  wa_092-bukrs,
  ' '        ' '    ' '  'RF05A-GJAHS' v_ultimo_diac(4),
  ' '        ' '    ' '  'BSIS-BUDAT' v_ultimo_diac,
  ' '        ' '    ' '  'UF05A-STGRD' '01'.

  opt-dismode = 'N'.
  CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

  IF sy-subrc IS INITIAL.

* Selecionar campo BKPF-STBLG
    SELECT stblg FROM bkpf
      UP TO 1 ROWS
      INTO @DATA(lv_stblg)
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @wa_saida_05-doc_lctor.
    ENDSELECT.

    IF sy-subrc IS INITIAL AND lv_stblg IS NOT INITIAL.

      MOVE:    lv_stblg                    TO wa_saida_05-doc_estorno,
               abap_false                  TO wa_saida_05-doc_contabil.

    ELSE.
      MOVE:    abap_false  TO wa_saida_05-doc_estorno,
               icon_led_red TO wa_saida_05-status.
    ENDIF.

    MODIFY it_saida_05 FROM wa_saida_05 INDEX lv_tabix.

  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*-----

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
  ' '        ' '    ' '  'RF05A-BELNS' wa_saida_05-doc_contabil,
  ' '        ' '    ' '  'BKPF-BUKRS'  wa_092-bukrs,
  ' '        ' '    ' '  'RF05A-GJAHS' v_ultimo_diac(4),
  ' '        ' '    ' '  'BSIS-BUDAT'  v_ultimo_diac,
  ' '        ' '    ' '  'UF05A-STGRD' '01'.

  opt-dismode = 'N'.
  CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

  IF sy-subrc IS INITIAL.

* Selecionar campo BKPF-STBLG
    SELECT stblg FROM bkpf
      UP TO 1 ROWS
      INTO @DATA(lv_stblgd)
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @wa_saida_05-doc_lcto.
    ENDSELECT.

    IF sy-subrc IS INITIAL AND lv_stblgd IS NOT INITIAL.
      MOVE:    lv_stblgd        TO wa_saida_05-doc_estorno,
               abap_false       TO wa_saida_05-doc_contabil,
               icon_led_yellow  TO wa_saida_05-status.
    ELSE.
      MOVE:    abap_false  TO wa_saida_05-doc_estorno,
               icon_led_red TO wa_saida_05-status.
    ENDIF.


    MODIFY it_saida_05 FROM wa_saida_05 INDEX lv_tabix.

    wa_096g = CORRESPONDING #( wa_saida_05 ).
    wa_096g-cod_contrato = wa_092-cod_contrato.
    wa_096g-dt_atualizacao = sy-datum.
    wa_096g-usuario = sy-uname.
    MODIFY zglt096g FROM wa_096g.
    COMMIT WORK AND WAIT.
    CLEAR: wa_096g, wa_saida_05.

    IF wa_saida_05-doc_contabil IS INITIAL.
      MESSAGE 'Documento estornado com sucesso!' TYPE 'S'.
    ENDIF.

  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_DOCUMENTO_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atualizar_documento_05 .

  DATA: it_zglt036 TYPE TABLE OF zglt036.

* Seleções dados tab principais
  SELECT DISTINCT *
    FROM zglt096g " zglt096g - fluxo arrendamento
    INTO TABLE it_096g
    WHERE cod_contrato EQ wa_092-cod_contrato
      AND mes NE space.

  IF it_096g[] IS NOT INITIAL.

    SELECT DISTINCT *
       FROM zib_contabil_err " Log de Erros de processamento contábeis
       INTO TABLE @DATA(it_zib_contabil_err)
      FOR ALL ENTRIES IN @it_096g
       WHERE obj_key  EQ @it_096g-obj_key.

    SELECT DISTINCT *
       FROM zib_contabil_err " Log de Erros de processamento contábeis
       APPENDING TABLE @it_zib_contabil_err
      FOR ALL ENTRIES IN @it_096g
       WHERE obj_key  EQ @it_096g-obj_keyr.

    SELECT DISTINCT *
       FROM zib_contabil_chv  " Tabela de controle de Chave de referência
       INTO TABLE @DATA(it_zib_contabil_chv)
      FOR ALL ENTRIES IN @it_096g
       WHERE obj_key  EQ @it_096g-obj_key.

    SELECT DISTINCT *
       FROM zib_contabil_chv  " Tabela de controle de Chave de referência
       APPENDING TABLE @it_zib_contabil_chv
      FOR ALL ENTRIES IN @it_096g
       WHERE obj_key  EQ @it_096g-obj_keyr.

    SELECT DISTINCT bukrs, belnr, stblg
      FROM bkpf    " Cabeçalho do documento contábil
      INTO TABLE @DATA(it_stblg)
      FOR ALL ENTRIES IN @it_096g
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @it_096g-doc_contabil.

    SELECT DISTINCT bukrs, belnr, stblg
      FROM bkpf    " Cabeçalho do documento contábil
      INTO TABLE @DATA(it_stblgr)
      FOR ALL ENTRIES IN @it_096g
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @it_096g-doc_estorno.

    SELECT *
       FROM zglt035 " Lançamentos Manuais - Cabeçalho
     INTO TABLE it_035
      FOR ALL ENTRIES IN it_096g
      WHERE doc_lcto EQ it_096g-doc_lcto
        AND bukrs    EQ wa_092-bukrs
        AND tp_lcto  EQ '0000000000'.

    SELECT *
       FROM zglt035
     APPENDING TABLE it_035
      FOR ALL ENTRIES IN it_096g
      WHERE doc_lcto EQ it_096g-doc_lctor
        AND bukrs    EQ wa_092-bukrs
        AND tp_lcto  EQ '0000000000'.

    SORT it_096g BY dt_atualizacao.
  ENDIF.

  IF it_zib_contabil_chv[] IS NOT INITIAL AND it_096g[] IS NOT INITIAL. " Sucess

    LOOP AT it_saida_05 INTO DATA(wa_saida_05).
      DATA(lv_tabix) = sy-tabix.

      READ TABLE it_096g INTO DATA(wa_096g) WITH KEY dt_atualizacao = wa_saida_05-dt_atualizacao.
      DATA(lv_tabix96) = sy-tabix.
      IF sy-subrc IS INITIAL AND wa_096g-obj_key IS NOT INITIAL.

        READ TABLE it_zib_contabil_chv INTO DATA(wa_zib_contabil_chvr) WITH KEY obj_key = wa_096g-obj_keyr.
        IF sy-subrc IS NOT INITIAL.
          CLEAR wa_zib_contabil_chvr.
        ENDIF.

        READ TABLE it_zib_contabil_chv INTO DATA(wa_zib_contabil_chv) WITH KEY obj_key = wa_096g-obj_key.

        IF sy-subrc IS INITIAL AND ( wa_zib_contabil_chv-belnr IS NOT INITIAL ).

          IF wa_zib_contabil_chv-belnr IS NOT INITIAL.
            wa_saida_05-status = icon_led_green.
          ELSEIF wa_zib_contabil_chv-belnr IS INITIAL.
            wa_saida_05-status = icon_led_yellow.
          ENDIF.

          IF wa_saida_05-doc_lcto IS INITIAL AND wa_saida_05-obj_key+5(10) IS NOT INITIAL.
            wa_saida_05-doc_lcto = wa_saida_05-obj_key+5(10).
            wa_096g-doc_lcto = wa_saida_05-doc_lcto.
          ENDIF.
* Doc contabil
          IF wa_saida_05-doc_contabil IS INITIAL
            AND ( wa_saida_05-doc_contabil NE wa_saida_05-doc_estorno
            OR  wa_saida_05-doc_estorno IS INITIAL ).
            wa_saida_05-doc_contabil = wa_zib_contabil_chv-belnr.
            wa_096g-doc_contabil = wa_zib_contabil_chv-belnr.

          ELSE.
            READ TABLE it_stblg INTO DATA(wa_stblg) WITH KEY belnr = wa_096g-doc_contabil.
            IF sy-subrc IS INITIAL AND wa_stblg-stblg IS NOT INITIAL.

              wa_saida_05-doc_estorno = wa_stblg-stblg.
              wa_096g-doc_estorno = wa_stblg-stblg.
            ELSE.
              CLEAR wa_stblg.
            ENDIF.
          ENDIF.

*-----
          IF wa_saida_05-doc_contabilr IS INITIAL
            AND ( wa_saida_05-doc_contabilr NE wa_saida_05-doc_estornor
            OR  wa_saida_05-doc_estornor IS INITIAL ).
            wa_saida_05-doc_contabilr = wa_zib_contabil_chvr-belnr.
            wa_096g-doc_contabilr = wa_zib_contabil_chvr-belnr.

          ELSE.
            READ TABLE it_stblg INTO DATA(wa_stblgr) WITH KEY belnr = wa_096g-doc_contabilr.
            IF sy-subrc IS INITIAL AND wa_stblgr-stblg IS NOT INITIAL.

              wa_saida_05-doc_estornor = wa_stblgr-stblg.
              wa_096g-doc_estornor = wa_stblgr-stblg.
            ELSE.
              CLEAR wa_stblg.
            ENDIF.
          ENDIF.

* Lote
          IF wa_saida_05-doc_lcto IS NOT INITIAL.
            READ TABLE it_035 INTO DATA(wa_035) WITH KEY doc_lcto = wa_saida_05-doc_lcto.
            IF sy-subrc IS INITIAL.
              wa_saida_05-lt_doc = wa_035-lote.
              wa_096g-lt_doc = wa_035-lote.
            ENDIF.
          ENDIF.

          IF wa_saida_05-doc_lctor IS NOT INITIAL.
            READ TABLE it_035 INTO DATA(wa_035r) WITH KEY doc_lcto = wa_saida_05-doc_lctor.
            IF sy-subrc IS INITIAL.
              wa_saida_05-lt_docr = wa_035r-lote.
              wa_096g-lt_docr = wa_035r-lote.
            ENDIF.
          ENDIF.

          IF p_mes IS NOT INITIAL.
            wa_096g-mes = p_mes.
          ENDIF.

          IF p_ano IS NOT INITIAL.
            wa_096g-ano = p_ano.
          ENDIF.

          MODIFY it_saida_05 FROM wa_saida_05 INDEX lv_tabix.
          MODIFY it_096g FROM wa_096g INDEX lv_tabix96. "Tabela Contratos de Arrendamento ( Cálculo ) Por Área
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF it_096g[] IS NOT INITIAL.
      MODIFY zglt096g FROM TABLE it_096g.
      COMMIT WORK AND WAIT.
    ENDIF.

  ELSEIF it_zib_contabil_err[] IS NOT INITIAL AND it_096g[] IS NOT INITIAL. " err apenas

    LOOP AT it_saida_05 INTO wa_saida_05.
      DATA(lv_tabixf) = sy-tabix.

      READ TABLE it_096g INTO DATA(wa_096gg) WITH KEY dt_atualizacao = wa_saida_05-dt_atualizacao.
      IF sy-subrc IS INITIAL AND wa_096gg-obj_key IS NOT INITIAL.

        READ TABLE it_zib_contabil_err INTO DATA(wa_zib_contabil_err) WITH KEY obj_key = wa_096gg-obj_key.

        IF sy-subrc IS INITIAL OR wa_zib_contabil_err-message IS NOT INITIAL.

          wa_saida_05-status = icon_led_red.
          MODIFY it_saida_05 FROM wa_saida_05 INDEX lv_tabixf.
        ENDIF.

        READ TABLE it_zib_contabil_err INTO wa_zib_contabil_err WITH KEY obj_key = wa_096gg-obj_keyr.

        IF sy-subrc IS INITIAL OR wa_zib_contabil_err-message IS NOT INITIAL.

          wa_saida_05-status = icon_led_red.
          MODIFY it_saida_05 FROM wa_saida_05 INDEX lv_tabixf.
        ENDIF.

      ELSE.
        CLEAR wa_096gg.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONSOLIDACAO_DE_DADOS_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consolidacao_de_dados_05 .

  DATA: tabix        TYPE sy-tabix,
        lv_cont      TYPE i,
        lv_comp      TYPE sy-datum,
        v_ultimo_dia TYPE sy-datum,
        lv_saldocp   TYPE zgle0004,
        lv_soma      TYPE zgle0004,
        vl_jirso     TYPE zgle0004.

  TRY .
      DATA(dt_vigencia) = it_096[ 1 ]-vigencia_de.
    CATCH cx_sy_itab_line_not_found.
      CLEAR dt_vigencia.
  ENDTRY.

  CHECK dt_vigencia IS NOT INITIAL.

  dt_vigencia = |{ dt_vigencia(6) }01|.

  PERFORM interval_date USING '-' CHANGING dt_vigencia.

  SORT it_094 BY dt_vencimento DESCENDING.

  TRY .
      DATA(dt_vencimento) = it_094[ 1 ]-dt_vencimento.
    CATCH cx_sy_itab_line_not_found.
      CLEAR dt_vencimento.
  ENDTRY.

  p_ano = p_dtvenc(4).
  p_mes = p_dtvenc+4(2).

  CONCATENATE p_ano p_mes '01' INTO lv_comp.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_comp
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  CHECK v_ultimo_dia EQ p_dtvenc.

  DO.

    IF dt_vigencia >= dt_vencimento.
      EXIT.
    ENDIF.

    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = dt_vigencia
      IMPORTING
        e_date = dt_vigencia.

    APPEND
    VALUE #(
              dt_atualizacao  = dt_vigencia
              comp            = |{ dt_vigencia+4(2) }/{ dt_vigencia(4) }|
           ) TO it_saida_05.

    dt_vigencia = |{ dt_vigencia(6) }01|.

    PERFORM interval_date USING '+' CHANGING dt_vigencia.
  ENDDO.

  IF it_saida_05[] IS NOT INITIAL.
    DELETE it_saida_05[] INDEX 1.
  ENDIF.

  LOOP AT it_saida_05 ASSIGNING FIELD-SYMBOL(<f_saida>).
    tabix = sy-tabix.

    CLEAR wa_096.
    LOOP AT it_096 INTO  wa_096 WHERE cod_contrato EQ p_cont
                                  AND vigencia_de(6) LE <f_saida>-dt_atualizacao(6)
                                  AND vigencia_ate(6) GE <f_saida>-dt_atualizacao(6).
*    READ TABLE it_096 INTO  wa_096 WITH KEY cod_contrato = p_cont
*                                            vigencia_de(6)  = <f_saida>-dt_atualizacao(6).
*    IF sy-subrc IS INITIAL AND wa_096-vlr_contrato IS NOT INITIAL.
      IF wa_096-vlr_contrato IS NOT INITIAL.
        <f_saida>-valor_brl  = ( ( wa_096-vlr_contrato / wa_096-prazo_safras ) / 12 ).  " Saca parcela anual
      ENDIF.
    ENDLOOP.

    READ TABLE it_zglt100 INTO DATA(wa_zglt100) WITH KEY
*                                                         cod_contrato    = p_cont
                                                         regiao          = wa_092-municipio
                                                         competencia(6)  = <f_saida>-dt_atualizacao(6).
    "131204 ZGL071 - Arrendamento - PSA TIPO 5
    CLEAR: wa_zglt100-ptax,vlr_out_taxa.
    PERFORM pega_taxa USING <f_saida>-dt_atualizacao CHANGING vlr_out_taxa.
    wa_zglt100-ptax = vlr_out_taxa.

    IF sy-subrc IS INITIAL AND wa_zglt100-ptax IS NOT INITIAL.
      <f_saida>-valor_usd  =  <f_saida>-valor_brl / wa_zglt100-ptax.
      <f_saida>-ptax       =  wa_zglt100-ptax.
    ELSE.
      CLEAR wa_zglt100.
    ENDIF.

* PIS BRL = ( Valor BRL * ZGLT096-PIS )
    <f_saida>-pis_brl  =  ( <f_saida>-valor_brl * wa_096-pis ) / 100.

* PIS USD = ( PIS BRL / PTAX )
    IF <f_saida>-pis_brl IS NOT INITIAL AND wa_zglt100-ptax IS NOT INITIAL.
      <f_saida>-pis_usd  =  <f_saida>-pis_brl / wa_zglt100-ptax.
    ENDIF.

*COFINS BRL = ( Valor BRL * ZGLT096-COFINS )
    <f_saida>-cofins_brl  =  ( <f_saida>-valor_brl * wa_096-cofins ) / 100.

*COFINS USD = ( COFINS BRL / PTAX )
    IF <f_saida>-cofins_brl IS NOT INITIAL AND wa_zglt100-ptax IS NOT INITIAL.
      <f_saida>-cofins_usd  =  ( <f_saida>-cofins_brl / wa_zglt100-ptax ).
    ENDIF.

*PIS BRL Arrendante = ( Valor BRL * ZGLT096-PIS_ARRENDANTE )
    <f_saida>-pis_arrend  =  ( <f_saida>-valor_brl * wa_096-pis_arrend ) / 100.

*PIS USD Arrendante = ( PIS BRL Arrendante / PTAX )
    IF <f_saida>-pis_arrend IS NOT INITIAL AND wa_zglt100-ptax IS NOT INITIAL.
      <f_saida>-pis_u_arrend  =  ( <f_saida>-pis_arrend / wa_zglt100-ptax ).
    ENDIF.

*COFINS BRL Arrendante = ( Valor BRL * ZGLT096-COFINS_ARRENDANTE )
    <f_saida>-cofins_arrend  =  ( <f_saida>-valor_brl * wa_096-cofins_arrend ) / 100.

*COFINS USD Arrendante = ( COFINS BRL Arrendante / PTAX )
    IF <f_saida>-cofins_arrend IS NOT INITIAL AND wa_zglt100-ptax IS NOT INITIAL.
      <f_saida>-cofins_u_arrend  = ( <f_saida>-cofins_arrend / wa_zglt100-ptax ).
    ENDIF.

    IF <f_saida>-doc_contabil IS INITIAL.
      <f_saida>-status = icon_led_yellow.
    ELSEIF <f_saida>-doc_contabil IS NOT INITIAL.
      <f_saida>-status = icon_led_green.
    ENDIF.

    IF <f_saida>-dt_atualizacao(6) EQ v_ultimo_dia(6).
      <f_saida>-color = 'C300'.
    ENDIF.
  ENDLOOP.

* Lines uptate
  IF it_096g[] IS NOT INITIAL.

    SELECT DISTINCT *
    FROM zib_contabil_err
    INTO TABLE @DATA(it_ziberr)
    FOR ALL ENTRIES IN @it_096g
    WHERE obj_key EQ @it_096g-obj_key.

    SELECT DISTINCT *
    FROM zib_contabil_err
    APPENDING TABLE @it_ziberr
    FOR ALL ENTRIES IN @it_096g
    WHERE obj_key EQ @it_096g-obj_keyr.

    LOOP AT it_saida_05 ASSIGNING <f_saida>.
      DATA(lv_tabix) = sy-tabix.
      READ TABLE it_096g INTO wa_096g WITH KEY dt_atualizacao(6) = <f_saida>-dt_atualizacao(6).

      IF sy-subrc IS INITIAL.
        IF wa_096g-obj_key IS NOT INITIAL.
          READ TABLE it_ziberr INTO DATA(wa_ziberr) WITH KEY obj_key = wa_096g-obj_key.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_ziberr.
          ENDIF.
        ENDIF.

        IF wa_096g-obj_keyr IS NOT INITIAL.
          READ TABLE it_ziberr INTO wa_ziberr WITH KEY obj_key = wa_096g-obj_keyr.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_ziberr.
          ENDIF.
        ENDIF.

      ELSE.
        CLEAR: wa_096g, wa_ziberr.
      ENDIF.

      IF wa_096g-doc_contabil IS NOT INITIAL AND wa_096g-doc_contabilr IS NOT INITIAL
        AND wa_ziberr-message IS INITIAL.
        <f_saida> = CORRESPONDING #( wa_096g ).
        <f_saida>-status = icon_led_green.
*        <f_saida>-status = icon_led_yellow.
*        <f_saida>-doc_lcto = wa_096c-doc_lcto.
*        <f_saida>-lt_doc  = wa_096c-lt_doc.
*        <f_saida>-obj_key = wa_096c-obj_key.
*        <f_saida>-saca_acumulada = wa_096c-qtd_sacas.
*        <f_saida>-preco_saca = wa_096c-preco_saca_brl.
*        <f_saida>-preco_saca_a = wa_096c-qtd_sacas_anual.
*        <f_saida>-vlr_p_m_sacas = wa_096c-vlr_parcmes_brl.
*        <f_saida>-vlr_atual_brl = wa_096c-vlr_atualizado_brl.
*        <f_saida>-variacao_m_brl = wa_096c-variacao_mes_brl.
*        <f_saida>-ptax = wa_096g-ptax.
*        <f_saida>-variacao_m_usd = wa_096c-variacao_mes_usd.

      ELSEIF wa_096g-doc_contabil IS INITIAL AND wa_096g-obj_key IS NOT INITIAL
          OR wa_096g-doc_contabilr IS INITIAL AND wa_096g-obj_keyr IS NOT INITIAL.

        <f_saida>-status = icon_led_yellow.
        <f_saida>-doc_lcto = wa_096g-doc_lcto.
        <f_saida>-lt_doc  = wa_096g-lt_doc.
        <f_saida>-obj_key = wa_096g-obj_key.
        <f_saida>-doc_lctor = wa_096g-doc_lctor.
        <f_saida>-lt_docr  = wa_096g-lt_docr.
        <f_saida>-obj_keyr = wa_096g-obj_keyr.
*        <f_saida>-saca_acumulada = wa_096c-qtd_sacas.
*        <f_saida>-preco_saca = wa_096c-preco_saca_brl.
*        <f_saida>-preco_saca_a = wa_096c-qtd_sacas_anual.
*        <f_saida>-vlr_p_m_sacas = wa_096c-vlr_parcmes_brl.
*        <f_saida>-vlr_atual_brl = wa_096c-vlr_atualizado_brl.
*        <f_saida>-variacao_m_brl = wa_096c-variacao_mes_brl.
*        <f_saida>-ptax = wa_096c-taxa_01.
*        <f_saida>-variacao_m_usd = wa_096c-variacao_mes_usd.

      ELSEIF wa_096g-doc_contabil IS INITIAL
        AND ( wa_ziberr-message IS INITIAL ).
        <f_saida>-status = icon_led_yellow.

      ELSEIF wa_096g-doc_contabilr IS INITIAL
        AND ( wa_ziberr-message IS INITIAL ).
        <f_saida>-status = icon_led_yellow.

      ELSEIF ( wa_096g-doc_contabil IS NOT INITIAL OR wa_096g-doc_contabil IS INITIAL )
        AND wa_ziberr-message IS NOT INITIAL .
        <f_saida>-status = icon_led_red.

      ELSEIF ( wa_096g-doc_contabilr IS NOT INITIAL OR wa_096g-doc_contabilr IS INITIAL )
        AND wa_ziberr-message IS NOT INITIAL .
        <f_saida>-status = icon_led_red.
      ELSE.
        <f_saida>-status = icon_led_yellow.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.


FORM pega_taxa USING in_dt CHANGING vlr_out_taxa TYPE tcurr-ukurs. "131204 ZGL071 - Arrendamento - PSA
  DATA: chdat(8)   TYPE c,
        datum      TYPE sy-datum,
        out_dt     TYPE tcurr-gdatu,
        houtput(8) TYPE n.

  datum = |{ in_dt+0(6) }01|.

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
    EXPORTING
      months  = 1
      olddate = datum
    IMPORTING
      newdate = datum.


  MOVE datum TO chdat.
  houtput = '99999999' - chdat.  "Zwischenspeichern in houtput, damit
  out_dt  = houtput.             "die führenden Nullen nicht verloren

  " Tabela responsavel por gravar a taxa do câmbio.
  SELECT SINGLE ukurs
    FROM tcurr
     INTO @DATA(vlr_tcurr)
   WHERE fcurr EQ 'BRL'
     AND tcurr EQ 'USD'
     AND kurst EQ 'B'
     AND gdatu EQ @out_dt.

  vlr_out_taxa = abs( vlr_tcurr ).

ENDFORM.

FORM estorno.

  CASE p_tp_arr.
    WHEN '02'.
      PERFORM f_estornar_documento.
    WHEN '03'.
      PERFORM f_estornar_documento_03.
    WHEN '05'.
      PERFORM f_estornar_documento_05.
  ENDCASE.

ENDFORM.
