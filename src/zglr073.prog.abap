REPORT zglr073.

TYPE-POOLS: slis, icon.

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
         tcolor         TYPE slis_t_specialcol_alv,
       END OF ty_saida_02.

TYPES: BEGIN OF ty_zib_contabil_err.
         INCLUDE STRUCTURE zib_contabil_err.
         TYPES:  mark TYPE c,
       END OF ty_zib_contabil_err.

TYPES: BEGIN OF ty_zib_contabil_chv.
         INCLUDE STRUCTURE zib_contabil_chv.
         TYPES:  bldat TYPE zib_contabil-bldat,
       END OF ty_zib_contabil_chv.

TYPES: BEGIN OF ty_saida_02_temp,
         parckey             TYPE zgle0043,
         tipo                TYPE c,
         parcela             TYPE i,
         compdats            TYPE sy-datum,
         competencia(7)      TYPE c,
         obj_key(20)         TYPE c,
         obj_keyd(20)        TYPE c,
         obj_keylpcp(20)     TYPE c,
         doc_lcto(10)        TYPE n,
         doc_lctod(10)       TYPE n,
         doc_lctolpcp(10)    TYPE n,
*         preco_saca_m        TYPE p DECIMALS 2,
         preco_saca_m        TYPE zgle0018,
         saca_clas_cp        TYPE zgle0018,
         atua_prec_cp        TYPE zgle0018,
         ativ_uso_d_cp       TYPE zgle0018,
         saca_clas_lp        TYPE zgle0018,
         atua_prec_lp        TYPE zgle0018,
         ativ_uso_d_lp       TYPE zgle0018,
         ativ_d_uso_tot      TYPE zgle0018,
         ativ_d_uso_totu     TYPE zgle0018,
         depreciacao         TYPE zgle0018,
         depreciacaou        TYPE zgle0018,
         doc_atua_preco(10)  TYPE c,
         est_atua_preco(10)  TYPE c,
         doc_depreciacao(10) TYPE c,
         est_depreciacao(10) TYPE c,
         doc_lpcp(10)        TYPE c,
         est_lpcp(10)        TYPE c,
         lt_doc              TYPE zlote_num,
         lt_depre            TYPE zlote_num,
         lt_lpcp             TYPE zlote_num,
         dolar               TYPE zgle0018,
         tcolor(4)           TYPE c,
         status(4),
       END OF ty_saida_02_temp.

TYPES: BEGIN OF ty_saida_03,
         num_parcela      TYPE zglt094-num_parcela,
         dt_atualizacao   TYPE zglt094-dt_vencimento,
         qnt_sacas        TYPE zgle0017,
         qnt_sacas_ms     TYPE zgle0017,
         vlr_p_m_sacas    TYPE zgle0017,
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
         vlr_atual_brl    TYPE zgle0017,
         variacao_m_brl   TYPE zgle0017,
         vlr_atual_usd    TYPE zgle0017,
         variacao_m_usd   TYPE zgle0017,
         celltab          TYPE lvc_t_styl,
       END OF ty_saida_03.

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

TYPES BEGIN OF ty_100.
        INCLUDE TYPE zglt100.
TYPES END OF ty_100.

TYPES BEGIN OF ty_096e.
        INCLUDE TYPE zglt096e.
TYPES END OF ty_096e.

TYPES: BEGIN OF ty_calc,
         num_parcela    TYPE zglt094-num_parcela,
         vlr_at_brl_ant TYPE zgle0017,
         vlr_at_usd_ant TYPE zgle0017,
       END OF ty_calc.

TYPES: BEGIN OF ty_zib_contabil.
         INCLUDE STRUCTURE zib_contabil.
         TYPES:  mark TYPE c,
       END OF ty_zib_contabil.

TYPES: BEGIN OF ty_parcela,
         parcela         TYPE zglt100-mes_ano_comp,
         pg_parcela      TYPE zglt100-mes_ano_comp,
         valor_pagamento TYPE zgle0026,
       END OF ty_parcela.

DATA: wa_092              TYPE zglt092,
      it_096              TYPE TABLE OF zglt096,
      wa_096              TYPE zglt096,
      it_096e             TYPE TABLE OF zglt096e,
      it_035              TYPE TABLE OF zglt035,
      it_096el            TYPE TABLE OF zglt096e,
      wa_096el            TYPE ty_096e,
      wa_096e             TYPE ty_096e,
      wa_zib_contabil     TYPE ty_zib_contabil,
      it_094              TYPE TABLE OF ty_0094,
      wa_094              TYPE  ty_0094,
      it_100              TYPE TABLE OF ty_100,
      wa_100              TYPE ty_100,
      it_092              TYPE TABLE OF zglt092,
      it_096c             TYPE TABLE OF zglt096c,
      wa_096c             TYPE zglt096c,
      it_saida_02         TYPE TABLE OF ty_saida_02,
      wa_saida_02         TYPE ty_saida_02,
      it_saida_02_dep     TYPE TABLE OF ty_saida_02_temp,
      it_saida_02_temp    TYPE TABLE OF ty_saida_02_temp,
      wa_saida_02_temp    TYPE ty_saida_02_temp,
      wa_zib_contabil_err TYPE ty_zib_contabil_err,
      wa_zib_contabil_chv TYPE ty_zib_contabil_chv,
      it_zib_contabil_err TYPE TABLE OF ty_zib_contabil_err,
      it_zib_contabil_chv TYPE TABLE OF ty_zib_contabil_chv,
      it_saida_03         TYPE TABLE OF ty_saida_03,
      wa_saida_03         TYPE ty_saida_03,
      it_saida_04         TYPE TABLE OF ty_saida_04,
      wa_saida_04         TYPE ty_saida_04,
      it_saida_04_1       TYPE TABLE OF ty_saida_04_1,
      wa_saida_04_1       TYPE ty_saida_04_1,
      it_calc             TYPE TABLE OF ty_calc,
      wa_calc             TYPE ty_calc,
      it_parcela          TYPE TABLE OF ty_parcela,
      wa_parcela          TYPE ty_parcela.

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
  lg_pt                   TYPE vtbbewe-atage,
  lg_txf                  TYPE zgle0018, "RJF
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
  lv_saca_clas_cp         TYPE p DECIMALS 2,
  lv_saca_clas_lp         TYPE p DECIMALS 2,
  lv_sacas_ano            TYPE p DECIMALS 2,
  p_text_table            TYPE sdydo_text_table.

DATA: gt_estilo01          TYPE lvc_t_styl.

DATA: taxa           TYPE zgle0004,
      tx(25)         TYPE c,
      lv_dperio      TYPE sy-datum,
      lv_udmes       TYPE sy-datum,
      lv_exit        TYPE c,
      lv_dperioc     TYPE sy-datum,
      vlr_at_brl_ant TYPE zgle0017,
      vlr_at_usd_ant TYPE zgle0017.

DATA : r_parcelas  TYPE RANGE OF zglt100-mes_ano_comp,
       rl_parcelas LIKE LINE OF r_parcelas
       .

DATA: it_zglt036_flg TYPE zde_zglt036_flg_t,
      wa_zglt036_flg TYPE zde_zglt036_flg.



START-OF-SELECTION.

  PERFORM select_dados.

  CALL SCREEN 0100.

CLASS cl_main DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS run.

    METHODS process_before_output.
    METHODS set_title_and_status.

ENDCLASS.

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

    READ TABLE it_saida_02_dep INTO wa_saida_02_temp INDEX e_row_id-index.
    IF sy-subrc = 0.

      CASE e_column_id .
        WHEN 'STATUS'.

          IF wa_saida_02_temp-status = icon_led_red .
            SELECT DISTINCT *
               FROM zib_contabil_err
               INTO TABLE it_zib_contabil_err
               WHERE obj_key  = wa_saida_02_temp-obj_key.

            SELECT DISTINCT *
               FROM zib_contabil_err
               APPENDING TABLE it_zib_contabil_err
               WHERE obj_key  = wa_saida_02_temp-obj_keyd.

            SELECT DISTINCT *
               FROM zib_contabil_err
               APPENDING TABLE it_zib_contabil_err
               WHERE obj_key  = wa_saida_02_temp-obj_keylpcp.

            wtab_msg-name    = 'MENSAGEM ERRO:'.
            APPEND wtab_msg TO itab_msg .
            CLEAR wtab_msg.
            LOOP AT it_zib_contabil_err INTO wa_zib_contabil_err.
              wtab_msg-name = wa_zib_contabil_err-message.
              APPEND wtab_msg TO itab_msg .
              CLEAR wtab_msg.
            ENDLOOP.

            IF wa_saida_02_temp-doc_atua_preco IS INITIAL.
              wtab_msg-name = 'Doc. ainda não gerado!'.
              APPEND wtab_msg TO itab_msg .
              CLEAR wtab_msg.
            ENDIF.

            CONCATENATE 'DOCUMENTO ' wa_saida_02_temp-doc_atua_preco INTO msg_alv SEPARATED BY space.
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
*---
            IF wa_saida_02_temp-parcela NE 0.
              IF wa_saida_02_temp-obj_key IS NOT INITIAL.
                SELECT DISTINCT *
                   FROM zib_contabil_err
                   INTO TABLE it_zib_contabil_err
                   WHERE obj_key  = wa_saida_02_temp-obj_key.
              ELSE.
                REFRESH it_zib_contabil_err.
              ENDIF.

              IF wa_saida_02_temp-obj_keyd IS NOT INITIAL.
                SELECT DISTINCT *
                   FROM zib_contabil_err
                   APPENDING TABLE it_zib_contabil_err
                   WHERE obj_key  = wa_saida_02_temp-obj_keyd.
              ELSE.
                REFRESH it_zib_contabil_err.
              ENDIF.

            ELSE.
              IF wa_saida_02_temp-obj_keylpcp IS NOT INITIAL.
                SELECT DISTINCT *
                   FROM zib_contabil_err
                   APPENDING TABLE it_zib_contabil_err
                   WHERE obj_key  = wa_saida_02_temp-obj_keylpcp.
              ELSE.
                REFRESH it_zib_contabil_err.
              ENDIF.
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
              MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX e_row_id-index.
            ENDIF.
*---

            IF wa_saida_02_temp-parcela NE 0.
              IF wa_saida_02_temp-doc_lcto IS NOT INITIAL AND itab_msg[] IS INITIAL.
                wtab_msg = 'AGUARDANDO PROCESSAMENTO DA ZIB_CONTABIL'.
                APPEND wtab_msg TO itab_msg .
              ELSE.
                wtab_msg = 'DOCUMENTO CONTABIL AINDA NÃO FOI GERADO'.
                APPEND wtab_msg TO itab_msg .
              ENDIF.
            ELSE.
              IF wa_saida_02_temp-doc_lctolpcp IS NOT INITIAL AND itab_msg[] IS INITIAL.
                wtab_msg = 'AGUARDANDO PROCESSAMENTO DA ZIB_CONTABIL'.
                APPEND wtab_msg TO itab_msg .
              ELSE.
                wtab_msg = 'DOCUMENTO CONTABIL AINDA NÃO FOI GERADO'.
                APPEND wtab_msg TO itab_msg .
              ENDIF.
            ENDIF.
            IF itab_msg IS NOT INITIAL.
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
          ELSEIF wa_saida_02_temp-status = icon_led_green.

            IF wa_saida_02_temp-parcela NE 0.
              wtab_msg-name    = 'MENSAGEM:'.
              APPEND wtab_msg TO itab_msg .
              CLEAR wtab_msg.

              IF wa_saida_02_temp-doc_atua_preco IS NOT INITIAL.
                wtab_msg = 'DOCUMENTO GERADO COM SUCESSO.'.
                APPEND wtab_msg TO itab_msg .
              ELSE.

                wtab_msg = 'DOCUMENTO CONTABIL AINDA NÃO FOI GERADO'.
                APPEND wtab_msg TO itab_msg .
              ENDIF.

            ELSE.
              wtab_msg-name    = 'MENSAGEM:'.
              APPEND wtab_msg TO itab_msg .
              CLEAR wtab_msg.

              IF wa_saida_02_temp-doc_lpcp IS NOT INITIAL.
                wtab_msg = 'DOCUMENTO GERADO COM SUCESSO.'.
                APPEND wtab_msg TO itab_msg .
              ELSE.

                wtab_msg = 'DOCUMENTO CONTABIL AINDA NÃO FOI GERADO'.
                APPEND wtab_msg TO itab_msg .
              ENDIF.
            ENDIF.
            IF itab_msg IS NOT INITIAL.
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
          ENDIF.

        WHEN 'DOC_ATUA_PRECO'.

          IF wa_saida_02_temp-doc_atua_preco IS NOT INITIAL.
            lv_belnr = wa_saida_02_temp-doc_atua_preco.
            SET PARAMETER ID 'BLN' FIELD lv_belnr.
            SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-compdats(4)."p_ano.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'EST_ATUA_PRECO'.

          IF wa_saida_02_temp-est_atua_preco IS NOT INITIAL.
            lv_belnr = wa_saida_02_temp-est_atua_preco.
            SET PARAMETER ID 'BLN' FIELD lv_belnr.
            SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-compdats(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'DOC_DEPRECIACAO'.

          IF wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.
            lv_belnr = wa_saida_02_temp-doc_depreciacao.
            SET PARAMETER ID 'BLN' FIELD lv_belnr.
            SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-compdats(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'EST_DEPRECIACAO'.

          IF wa_saida_02_temp-est_depreciacao IS NOT INITIAL.
            lv_belnr = wa_saida_02_temp-est_depreciacao.
            SET PARAMETER ID 'BLN' FIELD lv_belnr.
            SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-compdats(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'DOC_LPCP'.

          IF wa_saida_02_temp-doc_lpcp IS NOT INITIAL.
            lv_belnr = wa_saida_02_temp-doc_lpcp.
            SET PARAMETER ID 'BLN' FIELD lv_belnr.
            SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-compdats(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'EST_LPCP'.

          IF wa_saida_02_temp-est_lpcp IS NOT INITIAL.
            lv_belnr = wa_saida_02_temp-est_lpcp.
            SET PARAMETER ID 'BLN' FIELD lv_belnr.
            SET PARAMETER ID 'BUK' FIELD wa_092-bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida_02_temp-compdats(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'LT_DOC'.
          IF wa_saida_02_temp-lt_doc IS NOT INITIAL.
            lw_lote-sign = 'I'.
            lw_lote-option = 'EQ'.
            lw_lote-low = wa_saida_02_temp-lt_doc.
            APPEND lw_lote TO lr_lote.
            CLEAR lw_lote.

            SUBMIT zgl018   WITH p_bukrs    EQ wa_092-bukrs
                            WITH p_lote     IN lr_lote
            AND RETURN.

          ENDIF.

        WHEN 'LT_DEPRE'.
          IF wa_saida_02_temp-lt_depre IS NOT INITIAL.
            lw_lote-sign = 'I'.
            lw_lote-option = 'EQ'.
            lw_lote-low = wa_saida_02_temp-lt_depre.
            APPEND lw_lote TO lr_lote.
            CLEAR lw_lote.

            SUBMIT zgl018   WITH p_bukrs EQ wa_092-bukrs
                            WITH p_lote  IN lr_lote
            AND RETURN.

          ENDIF.

        WHEN 'LT_LPCP'.
          IF wa_saida_02_temp-lt_lpcp IS NOT INITIAL.
            lw_lote-sign = 'I'.
            lw_lote-option = 'EQ'.
            lw_lote-low = wa_saida_02_temp-lt_lpcp.
            APPEND lw_lote TO lr_lote.
            CLEAR lw_lote.

            SUBMIT zgl018   WITH p_bukrs EQ wa_092-bukrs
                            WITH p_lote  IN lr_lote
            AND RETURN.

          ENDIF.

      ENDCASE.
    ENDIF.

    CALL METHOD g_alv->refresh_table_display( is_stable = wa_stable ).

  ENDMETHOD.

  METHOD on_data_changed.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname EQ 'PRECO_SACA_M'.

      LOOP AT it_saida_02_dep INTO wa_saida_02_temp.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.
          WHEN 'PRECO_SACA_M'.
            wa_saida_02_temp-preco_saca_m = wa_good_cells-value.
            MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX wa_good_cells-row_id.

*          WHEN 'TAXA_02'.
*            wa_saida_03-taxa_02 = wa_good_cells-value.
*            MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_good_cells-row_id.

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

*    CLEAR: vlr_at_brl_ant,  vlr_at_usd_ant.


**    LOOP AT  et_good_cells INTO DATA(wa_cells)
**      WHERE fieldname EQ 'VLR_SACA_BRL' OR
**            fieldname EQ 'TAXA_02'.
**
**      LOOP AT it_saida_03 INTO wa_saida_03.
**
**        CHECK wa_cells-row_id EQ sy-tabix.
**
**        IF wa_saida_03-vlr_saca_brl IS NOT INITIAL AND wa_saida_03-taxa_02 IS NOT INITIAL.
**
**          wa_saida_03-vlr_saca_usd   = ( wa_saida_03-vlr_saca_brl / wa_saida_03-taxa_02  ).
**          wa_saida_03-vlr_atual_brl  = ( wa_saida_03-qnt_sacas_ms * wa_saida_03-vlr_saca_brl ).
**          wa_saida_03-vlr_atual_usd  = ( wa_saida_03-qnt_sacas_ms * wa_saida_03-vlr_saca_usd ).
**
**
**          CASE wa_saida_03-num_parcela.
**            WHEN 1.
**              vlr_at_brl_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_brl ).
**              vlr_at_usd_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_usd ).
**
**              wa_saida_03-variacao_m_brl  = vlr_at_brl_ant.
**              wa_saida_03-variacao_m_usd  = vlr_at_usd_ant.
**
**            WHEN OTHERS.
**
**              DATA(num_parc_ant) = wa_saida_03-num_parcela - 1.
**
**              READ TABLE it_calc INTO wa_calc WITH KEY num_parcela = num_parc_ant.
**              IF sy-subrc = 0.
**                vlr_at_brl_ant = wa_calc-vlr_at_brl_ant.
**                vlr_at_usd_ant = wa_calc-vlr_at_usd_ant.
**              ENDIF.
**
**              wa_saida_03-variacao_m_brl  = ( wa_saida_03-vlr_atual_brl - vlr_at_brl_ant ).
**              wa_saida_03-variacao_m_usd  = ( wa_saida_03-vlr_atual_usd - vlr_at_usd_ant ).
**
**              CLEAR: vlr_at_brl_ant,  vlr_at_usd_ant.
**              vlr_at_brl_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_brl ).
**              vlr_at_usd_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_usd ).
**          ENDCASE.
**
**          MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_cells-row_id.
**
**          wa_calc-num_parcela    = wa_saida_03-num_parcela.
**          wa_calc-vlr_at_brl_ant = vlr_at_brl_ant.
**          wa_calc-vlr_at_usd_ant = vlr_at_usd_ant.
**
**          APPEND wa_calc TO it_calc.
**
**        ENDIF.
**
**        CLEAR wa_saida_03.
**      ENDLOOP.
**    ENDLOOP.

*    LOOP AT et_good_cells INTO DATA(wa_good_cells)
*          WHERE fieldname EQ 'PRECO_SACA_M'.
*
*      LOOP AT it_saida_02_dep INTO wa_saida_02_temp.
*
*        CHECK wa_good_cells-row_id EQ sy-tabix.
*
*        CASE wa_good_cells-fieldname.
*          WHEN 'PRECO_SACA_M'.
*            wa_saida_02_temp-preco_saca_m = wa_good_cells-value.
*            MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX wa_good_cells-row_id.
*
**          WHEN 'TAXA_02'.
**            wa_saida_03-taxa_02 = wa_good_cells-value.
**            MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_good_cells-row_id.
*
*        ENDCASE.
*      ENDLOOP.
*    ENDLOOP.

    CALL METHOD g_alv->refresh_table_display( EXPORTING is_stable = wa_stable ).
  ENDMETHOD.

ENDCLASS.

DATA r_main TYPE REF TO cl_main.

CLASS cl_main IMPLEMENTATION.

  METHOD run.
    CREATE OBJECT r_main.
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD process_before_output.
    "//set title
    me->set_title_and_status( ).

*    "//set data
*    me->set_outtab_data( ).
*
*    "// set screen
*    me->set_screen( ).
*
*    "//display data
*    me->display( ).

  ENDMETHOD.

  METHOD set_title_and_status.
    DATA: it_tcode TYPE TABLE OF sy-ucomm.

    CLEAR: it_tcode.

*    IF r_main->at_antigo EQ gl_acao-exibir.
*      APPEND 'SAVE' TO it_tcode.
*    ENDIF.

    SET TITLEBAR 'TL_0100'.
    SET PF-STATUS 'ST_01000' EXCLUDING it_tcode.
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
      FROM zglt100
      INTO TABLE it_100
      WHERE regiao EQ wa_092-municipio.
*        AND tp_arrendamento EQ wa_092-tp_arrendamento.

    SELECT DISTINCT *
      FROM zglt096e
      INTO TABLE it_096e
      WHERE cod_contrato EQ wa_092-cod_contrato.

    IF it_096e[] IS NOT INITIAL.
      SELECT *
         FROM zglt035
        FOR ALL ENTRIES IN @it_096e
        WHERE doc_lcto EQ @it_096e-doc_lcto
          AND bukrs    EQ @wa_092-bukrs
          AND tp_lcto  EQ '0000000000'
          INTO TABLE @it_035.

      SELECT *
         FROM zglt035
       APPENDING TABLE it_035
        FOR ALL ENTRIES IN it_096e
        WHERE doc_lcto EQ it_096e-doc_lctod
          AND bukrs    EQ wa_092-bukrs
          AND tp_lcto  EQ '0000000000'.
    ENDIF.

    CASE p_tp_arr.
      WHEN '01' OR '02'.
*        PERFORM consolidacao_de_dados.
        PERFORM consolidacao_de_dadosd.
      WHEN '03'.

        SELECT DISTINCT *
          FROM zglt096c INTO TABLE it_096c
        WHERE  cod_contrato EQ wa_092-cod_contrato.

        PERFORM tratar_dados_03 USING sy-subrc.
      WHEN '04'.
        PERFORM tratar_dados_04.
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

    WHEN '03' OR '04'.

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
          ( fieldname = 'STATUS'              coltext = 'Status'                    outputlen = '05'     decimals_o  = ''   edit = ''  hotspot = 'X' )
          ( fieldname = 'PARCELA'             coltext = 'Parcela'                   outputlen = '10'     decimals_o  = ''   edit = ''  )
          ( fieldname = 'COMPETENCIA'         coltext = 'Competência'               outputlen = '15'     decimals_o  = ''   edit = ''  )
          ( fieldname = 'PRECO_SACA_M'        tabname = 'it_saida_02_dep'          coltext = 'Preço Saca Mensal'         outputlen = '20'  decimals_o  = 2   edit = '' )
          ( fieldname = 'SACA_CLAS_CP'        coltext = 'Sacas Classificadas CP'    outputlen = '20'     decimals_o  = 2    edit = ''  )
          ( fieldname = 'ATUA_PREC_CP'        coltext = 'Ativo Direito Uso CP'      outputlen = '20'     decimals_o  = 2    edit = ''  ) "'Atualização Preço CP' antigo
          ( fieldname = 'ATIV_USO_D_CP'       coltext = 'Atualização Preço CP'      outputlen = '20'     decimals_o  = 2    edit = '' do_sum = abap_true ) " 'Ativo Direito Uso CP' antigo
          ( fieldname = 'DOLAR'               coltext = 'TAXA'                      outputlen = '20'     decimals_o  = 5    edit = '' do_sum = abap_true )
          ( fieldname = 'SACA_CLAS_LP'        coltext = 'Sacas Classificadas LP'    outputlen = '20'     decimals_o  = 2    edit = ''  )
          ( fieldname = 'ATUA_PREC_LP'        coltext = 'Ativo Direito Uso LP'      outputlen = '20'     decimals_o  = 2    edit = ''  ) "'Atualização Preço LP'
          ( fieldname = 'ATIV_USO_D_LP'       coltext = 'Atualização Preço LP'      outputlen = '20'     decimals_o  = 2    edit = '' do_sum = abap_true ) " 'Ativo Uso Direito LP'
          ( fieldname = 'ATIV_D_USO_TOT'      coltext = 'Ativo Direito Uso Total BRL'   outputlen = '22'     decimals_o  = 2    edit = '' do_sum = abap_true )
          ( fieldname = 'ATIV_D_USO_TOTU'     coltext = 'Ativo Direito Uso Total USD'   outputlen = '22'     decimals_o  = 2    edit = '' do_sum = abap_true )
          ( fieldname = 'DEPRECIACAO'         coltext = 'Depreciação BRL'               outputlen = '20'     decimals_o  = 2    edit = '' do_sum = abap_true )
          ( fieldname = 'DEPRECIACAOU'        coltext = 'Depreciação USD'               outputlen = '20'     decimals_o  = 2    edit = '' do_sum = abap_true )
          ( fieldname = 'DOC_ATUA_PRECO'      coltext = 'Doc. Atual Preço'          outputlen = '20'     decimals_o  = ''   edit = '' hotspot = 'X' )
          ( fieldname = 'EST_ATUA_PRECO'      coltext = 'Est. Atual Preço'          outputlen = '20'     decimals_o  = ''   edit = '' hotspot = 'X' )
          ( fieldname = 'DOC_DEPRECIACAO'     coltext = 'Doc. Depreciação'          outputlen = '20'     decimals_o  = ''   edit = '' hotspot = 'X' )
          ( fieldname = 'EST_DEPRECIACAO'     coltext = 'Est. Depreciação'          outputlen = '20'     decimals_o  = ''   edit = '' hotspot = 'X' )
          ( fieldname = 'DOC_LPCP'            coltext = 'Doc. LPCP'                 outputlen = '20'     decimals_o  = ''   edit = '' hotspot = 'X' )
          ( fieldname = 'EST_LPCP'            coltext = 'Est. LPCP'                 outputlen = '20'     decimals_o  = ''   edit = '' hotspot = 'X' )
          ( fieldname = 'LT_DOC'              coltext = 'Lote Atual. Preço'         outputlen = '20'     decimals_o  = ''   edit = '' hotspot = 'X' )
          ( fieldname = 'LT_DEPRE'            coltext = 'Lote Depreciação'          outputlen = '20'     decimals_o  = ''   edit = '' hotspot = 'X' )
          ( fieldname = 'LT_LPCP'             coltext = 'Lote LPCP'                 outputlen = '20'     decimals_o  = ''   edit = '' hotspot = 'X' )
          ).

*      it_fieldcat =  VALUE lvc_t_fcat(
*          ( fieldname = 'FL_VENC_COMP'    coltext = 'Competência'                    outputlen = '10'       )
*          ( fieldname = 'T_PAGAR_BRL'     coltext = 'Total a Pagar BRL'              outputlen = '15'     decimals_o  = 2  )
*          ( fieldname = 'T_PAGAR_USD'     coltext = 'Total a Pagar USD'              outputlen = '15'     decimals_o  = 2  )
*          ( fieldname = 'TAXA_AA'         coltext = 'Taxa AA'                        outputlen = '08'     decimals_o  = 2  )
*          ( fieldname = 'TAXA_AM'         coltext = 'Taxa AM'                        outputlen = '08'     decimals_o  = 2  )
*          ( fieldname = 'VP_VL_PRESENTE'  coltext = 'Valor Presente'                 outputlen = '10'     decimals_o  = 2  )
*          ( fieldname = 'VP_VL_JUROS'     coltext = 'Valor Presente Juros'           outputlen = '12'     decimals_o  = 2  )
*          ( fieldname = 'VP_VL_TOTAL'     coltext = 'Valor Presente Total'           outputlen = '12'     decimals_o  = 2  )
*          ( fieldname = 'BP_CIR_PRI '     coltext = 'B.P. Circulante Principal'      outputlen = '20'     decimals_o  = 2  )
*          ( fieldname = 'BP_CIR_JUR '     coltext = 'B.P. Circulante Juros'          outputlen = '20'     decimals_o  = 2  )
*          ( fieldname = 'BP_NCIR_PRI'     coltext = 'B.P. Não Circulante Principal'  outputlen = '20'     decimals_o  = 2  )
*          ( fieldname = 'BP_NCIR_JUR'     coltext = 'B.P. Não Circulante Juros'      outputlen = '20'     decimals_o  = 2  )
*          ( fieldname = 'BP_TOT_PRI '     coltext = 'B.P. Total Principal'           outputlen = '15'     decimals_o  = 2  )
*          ( fieldname = 'BP_TOT_JUR '     coltext = 'B.P. Total Juros'               outputlen = '12'     decimals_o  = 2  )
*          ( fieldname = 'JIRCP'           coltext = 'J.Imp. Resultado CP'            outputlen = '12'     decimals_o  = 2  )
*          ( fieldname = 'JIRLP'           coltext = 'J.Imp. Resultado LP'            outputlen = '12'     decimals_o  = 2  )
*          ( fieldname = 'JIRSO'           coltext = 'J.Imp. Resultado Soma'          outputlen = '12'     decimals_o  = 2  ) ).



    WHEN '03'.
      it_fieldcat =  VALUE lvc_t_fcat(
          ( fieldname = 'NUM_PARCELA'      coltext = 'Parcela'                   outputlen = '07'                          edit = ''   )
          ( fieldname = 'DT_ATUALIZACAO'   coltext = 'Data Atualização'          outputlen = '16'                          edit = ''   )
          ( fieldname = 'QNT_SACAS'        coltext = 'Quant.Sacas'               outputlen = '10'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'QNT_SACAS_MS'     coltext = 'Quant.Sacas Mensal'        outputlen = '16'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VLR_P_M_SACAS'    coltext = 'Vlr Parcela Mensal Sacas'  outputlen = '25'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VLR_P_BRL'        coltext = 'Vlr Parcela BRL'           outputlen = '20'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VLR_P_M_BRL'      coltext = 'Vlr Parcela Mensal BRL'    outputlen = '25'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VLR_P_USD'        coltext = 'Vlr Parcela USD'           outputlen = '20'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VLR_P_M_USD'      coltext = 'Vlr Parcela Mensal USD'    outputlen = '25'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'PRECO_SACA_BRL'   coltext = 'Preço Saca BRL'            outputlen = '14'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'TAXA_01'          coltext = 'Taxa'                      outputlen = '10'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'PRECO_SACA_USD'   coltext = 'Preço Saca USD'            outputlen = '14'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VLR_SACA_BRL'     coltext = 'Vlr Saca BRL'              outputlen = '12'       decimals    = 2    edit = 'X'  inttype = 'F'  )
          ( fieldname = 'TAXA_02'          coltext = 'Taxa'                      outputlen = '10'       decimals    = 4    edit = 'X'  inttype = 'F'  )
          ( fieldname = 'VLR_SACA_USD'     coltext = 'Vlr Saca USD'              outputlen = '12'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VLR_ATUAL_BRL'    coltext = 'Vlr Atualizado BRL'        outputlen = '20'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VARIACAO_M_BRL'   coltext = 'Variação Mensal BRL'       outputlen = '20'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VLR_ATUAL_USD'    coltext = 'Vlr Atualizado USD'        outputlen = '20'       decimals_o  = 2    edit = ''   )
          ( fieldname = 'VARIACAO_M_USD'   coltext = 'Variação Mensal USD'       outputlen = '20'       decimals_o  = 2    edit = ''   ) ).

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

  DATA: tabix    TYPE sy-tabix,
        vl_jirso TYPE zgle0004.

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

    PERFORM cores USING tabix CHANGING <f_saida>.

  ENDLOOP.

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
      vp_vl_presente = vp_vl_presente + ( wa_saida-vl_parcela / ( 1 + taxa_am ) ** num_parcelas ).
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


        READ TABLE it_096 INTO  wa_096 WITH KEY cod_contrato = p_cont
                                                vigencia_de  = p_dtvenc.
        IF sy-subrc = 0.
          wa_saida_03-qnt_sacas       = wa_096-sacas_p_ano.
          wa_saida_03-qnt_sacas_ms    = ( ( wa_saida_03-qnt_sacas / 12 )    *  _num_parcela ).
          wa_saida_03-vlr_p_m_sacas   = ( ( wa_saida_03-qnt_sacas_ms / 12 ) *  _num_parcela ).
          wa_saida_03-vlr_p_brl       = ( wa_saida_03-qnt_sacas * wa_096-preco_sacas ).
          wa_saida_03-vlr_p_m_brl     = ( ( wa_saida_03-vlr_p_brl / 12 ) * _num_parcela ).
          wa_saida_03-vlr_p_usd       = ( wa_saida_03-vlr_p_brl /  wa_092-tx_contrato ).
          wa_saida_03-vlr_p_m_usd     = ( ( wa_saida_03-vlr_p_usd / 12 ) * _num_parcela ).
          wa_saida_03-preco_saca_brl  = wa_096-preco_sacas.
          wa_saida_03-taxa_01         = ( wa_saida_03-vlr_p_brl / wa_saida_03-vlr_p_usd  ).
          wa_saida_03-preco_saca_usd  = ( wa_saida_03-vlr_p_usd / wa_saida_03-qnt_sacas ).
          "WA_SAIDA_03-VLR_SACA_BRL    =  '62.00'.
          "WA_SAIDA_03-TAXA_02         =  '3.9407'.
          wa_saida_03-vlr_saca_usd    =  COND #( WHEN wa_saida_03-vlr_saca_brl / wa_saida_03-taxa_02 < 0 THEN  0 ELSE wa_saida_03-vlr_saca_brl / wa_saida_03-taxa_02  ).
          wa_saida_03-vlr_atual_brl   =  ( wa_saida_03-qnt_sacas_ms * wa_saida_03-vlr_saca_brl ).
          wa_saida_03-vlr_atual_usd   =  ( wa_saida_03-qnt_sacas_ms * wa_saida_03-vlr_saca_usd ).

          CASE _num_parcela.
            WHEN 1.
              vlr_at_brl_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_brl ).
              vlr_at_usd_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_usd ).

              wa_saida_03-variacao_m_brl  = wa_saida_03-vlr_atual_brl.
              wa_saida_03-variacao_m_usd  = wa_saida_03-vlr_atual_usd.

            WHEN OTHERS.
              wa_saida_03-variacao_m_brl  = ( wa_saida_03-vlr_atual_brl - vlr_at_brl_ant ).
              wa_saida_03-variacao_m_usd  = ( wa_saida_03-vlr_atual_usd - vlr_at_usd_ant ).

              CLEAR: vlr_at_brl_ant,  vlr_at_usd_ant.
              vlr_at_brl_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_brl ).
              vlr_at_usd_ant = ( wa_saida_03-qnt_sacas_ms  * wa_saida_03-vlr_saca_usd ).
          ENDCASE.
        ENDIF.

        FREE wa_saida_03-celltab.
        gt_estilo01 =  VALUE #( ( fieldname = 'VLR_SACA_BRL' style = cl_gui_alv_grid=>mc_style_disabled )
                                ( fieldname = 'TAXA_02'      style = cl_gui_alv_grid=>mc_style_disabled )  ).
        INSERT LINES OF gt_estilo01 INTO TABLE wa_saida_03-celltab.


        APPEND VALUE #(
                      num_parcela      = _num_parcela
                      dt_atualizacao   = _dt_atual
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
                      variacao_m_usd   = wa_saida_03-variacao_m_usd  ) TO it_saida_03.


        CLEAR _dt_vencimento.
        _dt_vencimento =  _dt_atual.

        CLEAR: _month, _year,  e_max_days, _dt_atual, _days, wa_saida_03.
      ENDDO.


    WHEN 0.
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
*  DATA: it_tcode TYPE TABLE OF sy-ucomm.
*
*  CLEAR: it_tcode.

  SET PF-STATUS 'ST_01000'." EXCLUDING it_tcode.
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
      PERFORM f_gerar_documento USING '04'. " Doc. Atualização de preço
      PERFORM f_gerar_documento USING '05'. " Doc. Depreciação Atualização de preço
    WHEN 'ESTOR'.
      PERFORM f_estornar_documento.
    WHEN 'REFRESH'.
      PERFORM f_atualizar_documento.
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

    SELECT SINGLE * INTO @DATA(wa_empresa)
      FROM t001
     WHERE bukrs EQ @wa_092-bukrs.

    IF ( zcl_string=>initialcap( i_str = CONV #( wa_empresa-butxt ) ) ) IS NOT INITIAL.
      CLEAR: filtros.
      filtros-parametro = 'Empresa'.
      filtros-valor     = zcl_string=>initialcap( i_str = CONV #( wa_empresa-butxt ) ).
    ENDIF.

    IF filtros IS NOT INITIAL.
      APPEND filtros TO i_filtros.
      CLEAR: filtros.
    ENDIF.


    IF p_mes IS NOT INITIAL.
      filtros-parametro = 'Mês'.
      filtros-valor     = p_mes.
    ENDIF.

    IF filtros IS NOT INITIAL.
      APPEND filtros TO i_filtros.
      CLEAR: filtros.
    ENDIF.

    IF p_ano IS NOT INITIAL.
      filtros-parametro = 'Ano'.
      filtros-valor     = p_ano.
    ENDIF.

    IF filtros IS NOT INITIAL.
      APPEND filtros TO i_filtros.
      CLEAR: filtros.
    ENDIF.

*---------

*
*    DATA: lv_datav  TYPE zgle0012,
*          lv_found  TYPE c,
*          lv_datcom TYPE sy-datum.
*
*    DATA:
*      vl_data  TYPE c LENGTH 10, "Variável de data
*      data     TYPE sy-datum,    "Variável de data
*      wl_tcurr TYPE tcurr,
*      lv_fech  TYPE ukurs_curr.
*
*    LOOP AT it_096 ASSIGNING FIELD-SYMBOL(<f_096>).
*
*      CONCATENATE  p_ano p_mes <f_096>-vigencia_de+6(2) INTO lv_datav.
*
*      IF lv_datav GE <f_096>-vigencia_de AND lv_datav LE <f_096>-vigencia_ate AND lv_found IS INITIAL.
*        lv_found = abap_true.
*      ELSE.
*        CONTINUE.
*      ENDIF.
*
**      <f_096>-vigencia_de = <f_096>-vigencia_de + 1.
*
*      CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
*        EXPORTING
*          i_date = <f_096>-vigencia_de
*        IMPORTING
*          e_date = <f_096>-vigencia_de.
*
**      CONCATENATE <f_096>-vigencia_de(4) <f_096>-vigencia_de+4(2) '01' INTO wa_saida_02_temp-compdats.
*
*      lv_datcom = <f_096>-vigencia_de + 1.
*
*      zcl_util=>conv_data_us_br( EXPORTING i_data = lv_datcom
*                                 RECEIVING e_data = vl_data ).
*
*      " Função standard para converter data em formato gravado na TCURR
*      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
*        EXPORTING
*          input  = vl_data
*        IMPORTING
*          output = data.
*
*      " Tabela responsavel por gravar a taxa do câmbio.
*      SELECT DISTINCT * UP TO 1 ROWS
*        FROM tcurr
*         INTO wl_tcurr
*       WHERE fcurr EQ 'BRL'
*         AND tcurr EQ 'USD'
*         AND gdatu EQ data.
*      ENDSELECT.
*      IF sy-subrc IS INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
*        lv_fech = abs( wl_tcurr-ukurs ).
*        lg_txf = lv_fech.
*      ENDIF.
*    ENDLOOP.
**---------

    IF lg_txf IS NOT INITIAL.
      filtros-parametro = 'Taxa Fechamento'.
      filtros-valor     = lg_txf.
    ENDIF.

    IF filtros IS NOT INITIAL.
      APPEND filtros TO i_filtros.
      CLEAR: filtros.
    ENDIF.

*
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
*
*    IF filtros IS NOT INITIAL.
*      APPEND filtros TO i_filtros.
*      CLEAR: filtros.
*    ENDIF.

    CASE wa_092-tp_arrendamento.
      WHEN '01' OR '02'.

      WHEN '03' OR '04'.

        IF wa_092-tp_arrendamento EQ '04'.
          lc_apropriacao_brl = 0.
          lc_apropriacao_usd = 0.
          lc_pis             = 0.
          lc_cofins          = 0.

          LOOP AT it_saida_04 INTO DATA(wa_saida_04).
            ADD wa_saida_04-apropriacao_brl TO lc_apropriacao_brl.
            ADD wa_saida_04-apropriacao_usd TO lc_apropriacao_usd.
            ADD wa_saida_04-pis TO lc_pis.
            ADD wa_saida_04-cofins TO lc_cofins.
          ENDLOOP.

          lv_valor = lc_apropriacao_brl.
          WRITE lv_valor TO tx_valor.
          CONDENSE tx_valor NO-GAPS.

          lv_valor = lc_apropriacao_usd.
          WRITE lv_valor TO tx_valor2.
          CONDENSE tx_valor2 NO-GAPS.
          APPEND VALUE #( parametro = 'Total Apropriar BRL' valor = tx_valor parametro2 = 'Total Apropriar USD' valor2 = tx_valor2 direita = abap_true ) TO i_filtros.

          lv_valor = lc_pis.
          WRITE lv_valor TO tx_valor.
          CONDENSE tx_valor NO-GAPS.

          lv_valor = lc_cofins.
          WRITE lv_valor TO tx_valor2.
          CONDENSE tx_valor2 NO-GAPS.
          APPEND VALUE #( parametro = 'Total PIS' valor = tx_valor parametro2 = 'Total COFINS' valor2 = tx_valor2 direita = abap_true ) TO i_filtros.

        ENDIF.

        WRITE wa_092-tx_pis TO tx_valor.
        CONDENSE tx_valor NO-GAPS.
        tx_valor = |{ tx_valor } %|.
        WRITE wa_092-tx_cofins TO tx_valor2.
        CONDENSE tx_valor2 NO-GAPS.
        tx_valor2 = |{ tx_valor2 } %|.
        APPEND VALUE #( parametro = 'PIS' valor = tx_valor parametro2 = 'COFINS' valor2 = tx_valor2  direita = abap_true ) TO i_filtros.

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

*        wa_layout-stylefname = 'CELLTAB'.
        wa_layout-sel_mode   = 'A'.
        wa_layout-stylefname = 'CELLSTYLES'.
*        wa_layout-ctab_fname = 'TCOLOR'.
        wa_layout-info_fname = 'TCOLOR'.

        SET HANDLER:  lcl_event_hander=>on_data_changed  FOR g_alv,
                      lcl_event_hander=>on_hotspot_click FOR g_alv,
                      lcl_event_hander=>on_data_changed_finished FOR g_alv.

        CALL METHOD g_alv->set_table_for_first_display
          EXPORTING
            is_layout       = wa_layout
            i_save          = 'A'
          CHANGING
            it_outtab       = it_saida_02_dep[]
            it_fieldcatalog = it_fieldcat.

      WHEN '03'.

        wa_layout-stylefname = 'CELLTAB'.

        SET HANDLER:  lcl_event_hander=>on_data_changed FOR g_alv,
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
*&      Form  CONSOLIDACAO_DE_DADOSD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consolidacao_de_dadosd .

  DATA: lv_meses       TYPE vtbbewe-atage,
        lv_sum         TYPE zgle0026,
        lv_compdats    TYPE sy-datum,
        lv_compdatsu   TYPE sy-datum,
        lv_ps          TYPE i,
        lv_cont        TYPE i,
        lv_contd       TYPE i,
        lv_contu       TYPE i,
        lv_contx       TYPE i,
        lv_contx2      TYPE i,
        lv_lin         TYPE i,
        lv_pa          TYPE zgle0017,
        lv_pa2         TYPE zgle0017,
        lv_pa_atu      TYPE zgle0017,
        lv_val_parcela TYPE zgle0017,

        lv_parc        TYPE i,
        lv_ini         TYPE c,
        lv_datcom      TYPE sy-datum.

  DATA:
    vl_data   TYPE c LENGTH 10, "Variável de data
    data      TYPE sy-datum,    "Variável de data
    wl_tcurr  TYPE tcurr,
    wl_tcurri TYPE tcurr,
    lv_rein   TYPE c,
    lv_vigf   TYPE sy-datum,
    lv_contf  TYPE i,
    lv_fechi  TYPE ukurs_curr,
    lv_fech   TYPE ukurs_curr.
  DATA: lv_datav TYPE zgle0012,
        lv_found TYPE c.
  DATA: wa_zibchv  TYPE zib_contabil_chv,
        wa_zibchvd TYPE zib_contabil_chv,
        wa_ziberr  TYPE zib_contabil_err,
        wa_ziberd  TYPE zib_contabil_err.

  DATA: lv_cont_adut  TYPE i,
        lv_cont_adutu TYPE i,
        lv_soma_adut  TYPE zgle0018,
        lv_soma_adutu TYPE zgle0018.

  FREE: lv_dperio, lv_vigf.

  SORT it_096 BY vigencia_de.
  lv_contx = 1.

  FREE lv_exit.
  IF sy-ucomm IS INITIAL AND p_mes IS INITIAL AND p_ano IS INITIAL.
    MOVE abap_true TO lv_exit.
    EXIT.
  ENDIF.

  CONCATENATE:  p_mes '/' p_ano  INTO DATA(lv_vig),
                p_ano p_mes '01' INTO lv_vigf.

  IF lv_vigf IS NOT INITIAL.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = lv_vigf
      IMPORTING
        e_date = lv_udmes.
  ENDIF.

**  IF it_saida_02_dep[] IS NOT INITIAL.
***Vai na tabela BKPFcom - BUKRS = empresa processamento - BELNR = em processamento
***selecionar campo BKPF-STBLG
**
**    SELECT bukrs, belnr, stblg FROM bkpf
**      INTO TABLE @DATA(it_stblg)
**      FOR ALL ENTRIES IN @it_saida_02_dep
**      WHERE bukrs EQ @wa_092-bukrs
**        AND belnr EQ @it_saida_02_dep-doc_atua_preco.
**
**    SELECT bukrs, belnr, stblg FROM bkpf
**      INTO TABLE @DATA(it_stblg_d)
**      FOR ALL ENTRIES IN @it_saida_02_dep
**      WHERE bukrs EQ @wa_092-bukrs
**        AND belnr EQ @it_saida_02_dep-doc_depreciacao.
**  ENDIF.
*
*    IF sy-subrc IS INITIAL AND lv_stblg IS NOT INITIAL.
**    MOVE:    wa_saida_02_temp-doc_atua_preco TO wa_saida_02_temp-est_atua_preco,
*      MOVE:    lv_stblg TO wa_saida_02_temp-est_atua_preco,
*               abap_false                  TO wa_saida_02_temp-doc_atua_preco.
*      MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX 1.
*    ENDIF.

  LOOP AT it_096 ASSIGNING FIELD-SYMBOL(<f_096>).

    IF <f_096>-vigencia_de LT <f_096>-vigencia_ate.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from = <f_096>-vigencia_de
*         I_KEY_DAY_FROM       =
          i_date_to   = <f_096>-vigencia_ate
*         I_KEY_DAY_TO         =
*         I_FLG_SEPARATE       = ' '
        IMPORTING
*         E_DAYS      =
          e_months    = lv_meses
*         E_YEARS     =
        .

    ENDIF.
    CLEAR lv_sum.

    IF lv_meses IS NOT INITIAL.
      MOVE lv_meses TO lg_pt.
    ENDIF.
*---- 100839 - CS2017000968 - Ajuste de regra de pagamento de parcela - BG - inicio
    SORT it_094 BY dt_vencimento.
    "READ TABLE it_094 INTO wa_094 INDEX 1."WITH KEY cod_contrato = <f_096>-cod_contrato.
*                                               num_parcela = wa_saida_02_temp-parcela.
    DATA: cont_parcelas TYPE i.
    REFRESH it_parcela.
    CLEAR: cont_parcelas.
    LOOP AT it_094 INTO DATA(wa_094).

      CONCATENATE wa_094-dt_vencimento+4(2) '/' wa_094-dt_vencimento(4) INTO wa_parcela-parcela.
      IF wa_094-dt_pagamneto IS NOT INITIAL.
        CONCATENATE wa_094-dt_pagamneto+4(2) '/' wa_094-dt_pagamneto(4) INTO wa_parcela-pg_parcela.
        MOVE wa_094-valor_pagamento TO wa_parcela-valor_pagamento.
      ENDIF.
      APPEND wa_parcela TO it_parcela.
      CLEAR wa_parcela.

      cont_parcelas = cont_parcelas + 1.
*      CLEAR:lv_val_parcela, lv_pa_atu.
    ENDLOOP.
*---- 100839 - CS2017000968 - Ajuste de regra de pagamento de parcela - BG - fim
* Lines
    DO lv_meses TIMES.

      lv_parc = lv_parc + 1.
      wa_saida_02_temp-parcela = lv_parc.

      <f_096>-vigencia_de = <f_096>-vigencia_de + 1.

      CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
        EXPORTING
          i_date = <f_096>-vigencia_de
        IMPORTING
          e_date = <f_096>-vigencia_de.

      CONCATENATE <f_096>-vigencia_de(4) <f_096>-vigencia_de+4(2) '01' INTO wa_saida_02_temp-compdats.

      lv_datcom = <f_096>-vigencia_de + 1.

      zcl_util=>conv_data_us_br( EXPORTING i_data = lv_datcom
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
         INTO @wl_tcurr
       WHERE fcurr EQ 'BRL'
         AND tcurr EQ 'USD'
         AND kurst EQ 'B'
         AND gdatu EQ @data.
      IF sy-subrc IS INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
        lv_fech = abs( wl_tcurr-ukurs ).

        CONCATENATE  p_ano p_mes <f_096>-vigencia_de+6(2) INTO lv_datav.

        DATA: lv_confcomp TYPE c.

        IF lv_datav(6) EQ wa_saida_02_temp-compdats(6).
          lv_confcomp = abap_true.
        ELSE.
          CLEAR lv_confcomp.
        ENDIF.

        lv_datav = lv_datav + 1.

        IF lv_datav GE <f_096>-vigencia_de AND lv_datav LE <f_096>-vigencia_ate AND lv_found IS INITIAL AND lv_confcomp IS NOT INITIAL.
          lv_found = abap_true.
* wa_saida_02_temp-compdats
          lg_txf = lv_fech.
        ENDIF.
      ELSE.
        CLEAR: wl_tcurr, lv_fech.
      ENDIF.

      IF lv_fech IS NOT INITIAL.
        wa_saida_02_temp-dolar = lv_fech.
      ENDIF.

      CONCATENATE <f_096>-vigencia_de+4(2) '/' <f_096>-vigencia_de(4) INTO wa_saida_02_temp-competencia.

      IF wa_saida_02_temp-parcela LE 1.
        SELECT SINGLE * FROM zglt096e
          INTO @DATA(wa_zglt096e)
          WHERE cod_contrato EQ @<f_096>-cod_contrato
            AND competencia EQ @wa_saida_02_temp-competencia.

        IF sy-subrc EQ 0 AND wa_zglt096e-doc_atua_preco IS INITIAL.

*          CONCATENATE 'ZGL17' i_doc_lcto i_ano_lcto INTO v_objkey.
          wa_saida_02_temp-obj_key  = wa_zglt096e-obj_key.
          wa_saida_02_temp-obj_keyd = wa_zglt096e-obj_keyd.

          SELECT SINGLE *
            FROM zib_contabil_chv
            INTO wa_zibchv
           WHERE obj_key = wa_zglt096e-obj_key.

          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_zibchv.
          ENDIF.

          SELECT SINGLE *
            FROM zib_contabil_chv
            INTO wa_zibchvd
           WHERE obj_key = wa_zglt096e-obj_keyd.

          IF ( sy-subrc IS NOT INITIAL ).
            CLEAR wa_zibchvd.
            SELECT SINGLE *
              FROM zib_contabil_err
              INTO wa_ziberr
             WHERE obj_key = wa_zglt096e-obj_key.

            IF sy-subrc IS NOT INITIAL.
              CLEAR wa_ziberr.
            ENDIF.

            SELECT SINGLE *
              FROM zib_contabil_err
              INTO wa_ziberd
             WHERE obj_key = wa_zglt096e-obj_keyd.

            IF sy-subrc IS NOT INITIAL.
              CLEAR wa_ziberd.
            ENDIF.

          ENDIF.

          IF wa_zibchv-belnr IS NOT INITIAL.

* Verificar se existe est
            SELECT stblg FROM bkpf
              UP TO 1 ROWS
              INTO @DATA(lv_stblg)
              WHERE bukrs EQ @wa_092-bukrs
                AND belnr EQ @wa_zibchv-belnr.
            ENDSELECT.

            IF sy-subrc IS INITIAL AND lv_stblg IS NOT INITIAL AND wa_zglt096e-est_atua_preco EQ lv_stblg.
              MOVE:
                    abap_false  TO wa_saida_02_temp-doc_atua_preco.
            ENDIF.

            IF lv_stblg IS NOT INITIAL.
              SELECT stblg FROM bkpf
                INTO @DATA(lv_stblg_d)
                WHERE bukrs EQ @wa_092-bukrs
                  AND belnr EQ @wa_zibchvd-belnr.
              ENDSELECT.
              IF sy-subrc IS INITIAL AND lv_stblg_d IS NOT INITIAL AND wa_zglt096e-est_depreciacao EQ lv_stblg_d.
                MOVE:
                      abap_false TO wa_saida_02_temp-doc_depreciacao.
              ENDIF.
            ENDIF.

            MOVE:
*                      wa_zibchv-belnr  TO wa_saida_02_temp-doc_atua_preco,
*                      wa_zibchvd-belnr TO wa_saida_02_temp-doc_depreciacao,
                  wa_zglt096e-est_atua_preco TO wa_saida_02_temp-est_atua_preco,
                  wa_zglt096e-est_depreciacao TO wa_saida_02_temp-est_depreciacao.
            CLEAR wa_zglt096e.
          ELSEIF wa_ziberr IS NOT INITIAL AND wa_zglt096e-obj_key IS NOT INITIAL.
            wa_saida_02_temp-status = icon_led_red.
          ELSEIF wa_ziberd IS NOT INITIAL AND wa_zglt096e-obj_keyd IS NOT INITIAL.
            wa_saida_02_temp-status = icon_led_red.
          ENDIF.

        ELSE.

          wa_saida_02_temp-obj_key  = wa_zglt096e-obj_key.
          wa_saida_02_temp-obj_keyd = wa_zglt096e-obj_keyd.

          MOVE: wa_zglt096e-doc_atua_preco TO wa_saida_02_temp-doc_atua_preco,
                wa_zglt096e-doc_depreciacao TO wa_saida_02_temp-doc_depreciacao,
                wa_zglt096e-est_atua_preco TO wa_saida_02_temp-est_atua_preco,
                wa_zglt096e-est_depreciacao TO wa_saida_02_temp-est_depreciacao.

        ENDIF.
      ENDIF.
*        MOVE abap_true TO lv_ini.
*      ELSE.
*        <f_096>-vigencia_de = <f_096>-vigencia_de + 27.
*        CONCATENATE <f_096>-vigencia_de+4(2) '/' <f_096>-vigencia_de(4) INTO wa_saida_02_temp-competencia.
*        if
*      ENDIF.
*       wa_saida_02_temp-competencia =

      READ TABLE it_100 INTO wa_100 WITH KEY
*                                             cod_contrato    = <f_096>-cod_contrato
                                             regiao          = wa_092-municipio
                                             competencia(6)  = wa_saida_02_temp-compdats(6).
*                                         BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_saida_02_temp-preco_saca_m = wa_100-preco_saca. " Criar tabela para inserção e busca?
      ENDIF.

      LOOP AT it_096 ASSIGNING FIELD-SYMBOL(<f_096x>).
        lv_ps = lv_ps + <f_096x>-prazo_safras.

        lv_sacas_ano = CONV #( <f_096x>-sacas_p_ano ). "RJF solicitação Amaury para teste arrendodamento alv
        <f_096x>-sacas_p_ano = CONV #( lv_sacas_ano ).

*Inicio Alteração - Leandro Valentim Ferreira - 26.09.23 - #122471
***        lv_pa = lv_pa + <f_096x>-sacas_p_ano.
        lv_pa = <f_096x>-total_sacas.
*Fim Alteração - Leandro Valentim Ferreira - 26.09.23 - #122471
      ENDLOOP.

      lv_val_parcela = lv_pa / lv_ps .
      IF  lv_pa_atu IS INITIAL AND lv_pa2 IS INITIAL.
        lv_pa_atu = lv_val_parcela.
        IF lv_pa2 < lv_pa.
          lv_pa2 = lv_val_parcela.
        ENDIF.
      ENDIF.

      DATA: v_comp_tabela    TYPE zgle0025,
            v_ultima_parcela TYPE abap_bool.

      READ TABLE it_parcela INTO DATA(wa_parcelas)  WITH KEY parcela = wa_saida_02_temp-competencia.
      IF sy-subrc EQ 0.

        IF sy-tabix = cont_parcelas.
          v_ultima_parcela = 'X'.
        ELSE.
          lv_contx = lv_contx + 1.
          lv_pa_atu = lv_pa_atu + lv_val_parcela .
          lv_pa2 =  lv_pa2 + lv_val_parcela .
        ENDIF.

      ENDIF.

      READ TABLE it_parcela INTO DATA(wa_pg_parcelas)  WITH KEY pg_parcela = wa_saida_02_temp-competencia.
      IF sy-subrc EQ 0.
        lv_contx2 = lv_contx2 + 1.

        lv_pa_atu = lv_pa_atu - lv_val_parcela.
      ENDIF.

      wa_saida_02_temp-saca_clas_cp = lv_pa_atu.

      lv_saca_clas_cp = CONV #( wa_saida_02_temp-saca_clas_cp ). "RJF solicitação Amaury para teste arrendodamento alv
      wa_saida_02_temp-saca_clas_cp = CONV #( lv_saca_clas_cp ).
      wa_saida_02_temp-atua_prec_cp = ( wa_saida_02_temp-preco_saca_m * wa_saida_02_temp-saca_clas_cp ). " ficou como ativ_uso_d_cp

      IF wa_saida_02_temp-parcela LE 1.

        SORT it_094 BY dt_vencimento.
        READ TABLE it_094 INTO wa_094 INDEX 1."WITH KEY cod_contrato = <f_096>-cod_contrato.
**                                               num_parcela = wa_saida_02_temp-parcela.
*        READ TABLE it_094 INTO wa_094 WITH KEY cod_contrato      = <f_096>-cod_contrato
*                                               dt_vencimento(06) = wa_saida_02_temp-compdats(6).
        IF sy-subrc IS INITIAL.
          wa_saida_02_temp-ativ_uso_d_cp = ( wa_saida_02_temp-atua_prec_cp - wa_094-valor_brl )."94      "atua_prec_cp
        ENDIF.

      ELSE.

*        READ TABLE it_094 INTO wa_094 WITH KEY cod_contrato      = <f_096>-cod_contrato
*                                               dt_pagamneto(06)  = wa_saida_02_temp-compdats(6).
*        IF sy-subrc IS INITIAL.
*
*          READ TABLE it_saida_02_dep INTO DATA(wa_saida_02_tempcp) WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
*          IF sy-subrc IS INITIAL.
**            wa_saida_02_temp-ativ_uso_d_cp = ( wa_saida_02_temp-atua_prec_cp - wa_saida_02_tempcp-atua_prec_cp + wa_094-valor_pagamento ). "atua_prec_cp
*            wa_saida_02_temp-ativ_uso_d_cp = ( wa_saida_02_temp-atua_prec_cp - wa_saida_02_tempcp-atua_prec_cp + wa_094-valor_pagamento ). "atua_prec_cp
*          ENDIF.
*        ELSE.
*          READ TABLE it_saida_02_dep INTO wa_saida_02_tempcp WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
*          IF sy-subrc IS INITIAL.
*            wa_saida_02_temp-ativ_uso_d_cp = ( wa_saida_02_temp-atua_prec_cp - wa_saida_02_tempcp-atua_prec_cp ). "atua_prec_cp
*          ENDIF.
*
*        ENDIF.
*      ENDIF.

*-----

        READ TABLE it_094 INTO wa_094 WITH KEY cod_contrato       = <f_096>-cod_contrato
                                               dt_vencimento(06)  = wa_saida_02_temp-compdats(6).
        IF sy-subrc IS INITIAL.

          READ TABLE it_094 INTO wa_094 WITH KEY cod_contrato       = <f_096>-cod_contrato
                                                 dt_pagamneto(06)   = wa_saida_02_temp-compdats(6).

          IF sy-subrc IS INITIAL.


            READ TABLE it_saida_02_dep INTO DATA(wa_saida_02_tempcp) WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
            IF sy-subrc IS INITIAL.
              wa_saida_02_temp-ativ_uso_d_cp = ( wa_saida_02_temp-atua_prec_cp - wa_saida_02_tempcp-atua_prec_lp + wa_094-valor_pagamento ). " atualização preço
            ENDIF.

          ELSE.

            READ TABLE it_saida_02_dep INTO wa_saida_02_tempcp WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
            IF sy-subrc IS INITIAL.
*            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-atua_prec_lp ). " atualização preço
*            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-ativ_uso_d_lp ). " atualização preço
              wa_saida_02_temp-ativ_uso_d_cp = ( wa_saida_02_temp-atua_prec_cp - wa_saida_02_tempcp-atua_prec_lp ). " atualização preço
            ENDIF.

          ENDIF.
        ELSE.

          READ TABLE it_094 INTO wa_094 WITH KEY cod_contrato       = <f_096>-cod_contrato
                                                 dt_pagamneto(06)   = wa_saida_02_temp-compdats(6).

          IF sy-subrc IS INITIAL.
            READ TABLE it_saida_02_dep INTO wa_saida_02_tempcp WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
            IF sy-subrc IS INITIAL.
              wa_saida_02_temp-ativ_uso_d_cp = ( wa_saida_02_temp-atua_prec_cp - wa_saida_02_tempcp-atua_prec_cp + wa_094-valor_pagamento ). " atualização preço
            ENDIF.
          ELSE.

            READ TABLE it_saida_02_dep INTO wa_saida_02_tempcp WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
            IF sy-subrc IS INITIAL.
*            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-atua_prec_lp ). " atualização preço
              wa_saida_02_temp-ativ_uso_d_cp = ( wa_saida_02_temp-atua_prec_cp - wa_saida_02_tempcp-atua_prec_cp ). " atualização preço
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

*-----

*      wa_saida_02_temp-saca_clas_lp = ( wa_saida_02_temp-saca_clas_cp * 2 ). " Mudou o calculo

      wa_saida_02_temp-saca_clas_lp = ( lv_pa - lv_pa2 ).
      IF wa_saida_02_temp-saca_clas_lp < 0 OR wa_saida_02_temp-saca_clas_cp = 0 .
        wa_saida_02_temp-saca_clas_lp = 0.
      ENDIF.

      lv_saca_clas_lp = CONV #( wa_saida_02_temp-saca_clas_lp ). "RJF solicitação Amaury para teste arrendodamento alv LP
      wa_saida_02_temp-saca_clas_lp = CONV #( lv_saca_clas_lp ).

      wa_saida_02_temp-atua_prec_lp = ( wa_saida_02_temp-preco_saca_m * wa_saida_02_temp-saca_clas_lp ).

      DATA(it_094d) = it_094.
      DELETE it_094d INDEX 1.
      IF lv_sum IS INITIAL.
        LOOP AT it_094d INTO wa_094.
          lv_sum = lv_sum + wa_094-valor_brl.
        ENDLOOP.
      ENDIF.

      IF wa_saida_02_temp-parcela LE 1.
*        READ TABLE it_094 INTO wa_094 WITH KEY cod_contrato      = <f_096>-cod_contrato
*                                               dt_pagamneto(06)  = wa_saida_02_temp-compdats(6).
*        IF sy-subrc IS INITIAL.
        wa_saida_02_temp-ativ_uso_d_lp = wa_saida_02_temp-atua_prec_lp - ( lv_sum ).
*        ENDIF.
      ELSE.

        READ TABLE it_094 INTO wa_094 WITH KEY cod_contrato       = <f_096>-cod_contrato
                                               dt_vencimento(06)  = wa_saida_02_temp-compdats(6).
        IF sy-subrc IS INITIAL.

          READ TABLE it_saida_02_dep INTO DATA(wa_saida_02_templp) WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
          IF sy-subrc IS INITIAL.
*            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-atua_prec_lp ). " atualização preço
*            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-ativ_uso_d_lp ). " atualização preço
            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-atua_prec_cp ). " atualização preço
          ENDIF.
        ELSE.
          READ TABLE it_saida_02_dep INTO wa_saida_02_templp WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
          IF sy-subrc IS INITIAL.
*            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-atua_prec_lp ). " atualização preço
            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-atua_prec_lp ). " atualização preço
          ENDIF.
        ENDIF.
      ENDIF.


**        READ TABLE it_094 INTO wa_094 WITH KEY cod_contrato       = <f_096>-cod_contrato
**                                               dt_vencimento(06)  = wa_saida_02_temp-compdats(6).
**        IF sy-subrc IS INITIAL.
**
**          READ TABLE it_094 INTO wa_094 WITH KEY cod_contrato       = <f_096>-cod_contrato
**                                                 dt_pagamneto(06)   = wa_saida_02_temp-compdats(6).
**
**          IF sy-subrc IS INITIAL.
**
**
**            READ TABLE it_saida_02_dep INTO DATA(wa_saida_02_templp) WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
**            IF sy-subrc IS INITIAL.
**              wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_cp - wa_saida_02_templp-atua_prec_lp + wa_094-valor_pagamento ). " atualização preço
**            ENDIF.
**
**          ELSE.
**
**            READ TABLE it_saida_02_dep INTO wa_saida_02_templp WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
**            IF sy-subrc IS INITIAL.
***            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-atua_prec_lp ). " atualização preço
***            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-ativ_uso_d_lp ). " atualização preço
**              wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-atua_prec_cp ). " atualização preço
**            ENDIF.
**
**          ENDIF.
**        ELSE.
**
**          READ TABLE it_094 INTO wa_094 WITH KEY cod_contrato       = <f_096>-cod_contrato
**                                                 dt_pagamneto(06)   = wa_saida_02_temp-compdats(6).
**
**          IF sy-subrc IS INITIAL.
**
**            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_cp - wa_saida_02_templp-atua_prec_lp ). " atualização preço
**
**          ELSE.
**
**            READ TABLE it_saida_02_dep INTO wa_saida_02_templp WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
**            IF sy-subrc IS INITIAL.
***            wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-atua_prec_lp ). " atualização preço
**              wa_saida_02_temp-ativ_uso_d_lp = ( wa_saida_02_temp-atua_prec_lp - wa_saida_02_templp-atua_prec_lp ). " atualização preço
**            ENDIF.
**          ENDIF.
**        ENDIF.

**      ENDIF.

      wa_saida_02_temp-ativ_d_uso_tot = ( wa_saida_02_temp-ativ_uso_d_cp + wa_saida_02_temp-ativ_uso_d_lp ).

      IF lv_fech IS NOT INITIAL.
        wa_saida_02_temp-ativ_d_uso_totu = ( wa_saida_02_temp-ativ_d_uso_tot / lv_fech ). "Tx. Câmbio
      ELSE.
        wa_saida_02_temp-ativ_d_uso_totu = wa_saida_02_temp-ativ_d_uso_tot.
      ENDIF.

      IF lv_fech IS NOT INITIAL.
        wa_saida_02_temp-dolar = lv_fech.
      ENDIF.

*      IF wa_saida_02_temp-obj_keyd IS NOT INITIAL.
*        APPEND VALUE #( fieldname = 'STATUS' color-col = 4 color-int = 1 color-inv = 0 ) TO wa_saida_02_temp-tcolor.
*      ENDIF.

**      IF wa_saida_02_temp-parcela LT 2.
**        wa_saida_02_temp-depreciacao = ( wa_saida_02_temp-ativ_d_uso_tot / lv_meses ).
**      ELSE.
**        READ TABLE it_saida_02_dep INTO DATA(wa_saida_02_tempd) WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
**        IF sy-subrc IS INITIAL.
**          wa_saida_02_temp-depreciacao = ( wa_saida_02_temp-ativ_d_uso_tot / ( lv_meses - ( wa_saida_02_temp-parcela - 1 ) ) +  wa_saida_02_tempd-depreciacao ).
**        ENDIF.
**      ENDIF.

* Geração e estornos
*       wa_saida_02_temp-doc_atua_preco
*       wa_saida_02_temp-est_atua_preco
*       wa_saida_02_temp-doc_depreciacao
*       wa_saida_02_temp-est_depreciacao
*endif.

      APPEND wa_saida_02_temp TO it_saida_02_dep.
      CLEAR: wa_saida_02_temp, lv_ps, lv_pa.
    ENDDO.
    CLEAR: wa_saida_02_temp, lv_ini, lv_cont.

  ENDLOOP.

  lv_meses = lines( it_saida_02_dep ).
  CLEAR wa_saida_02_temp.
  LOOP AT it_saida_02_dep INTO DATA(wa_saida_02_tempx).

    READ TABLE it_096e INTO wa_096e WITH KEY cod_contrato = <f_096>-cod_contrato
                                             parcela  = wa_saida_02_tempx-parcela.
    IF sy-subrc IS INITIAL.

      READ TABLE it_035 INTO DATA(wa_035) WITH KEY doc_lcto = wa_096e-doc_lcto.
      IF sy-subrc IS INITIAL.
        wa_saida_02_tempx-lt_doc = wa_035-lote.
      ENDIF.

      READ TABLE it_035 INTO DATA(wa_035d) WITH KEY doc_lcto = wa_096e-doc_lctod.
      IF sy-subrc IS INITIAL.
        wa_saida_02_tempx-lt_depre = wa_035d-lote.
      ENDIF.

      wa_saida_02_tempx-obj_key = wa_096e-obj_key.
      wa_saida_02_tempx-obj_keyd = wa_096e-obj_keyd.
      wa_saida_02_tempx-doc_lcto = wa_096e-doc_lcto.
      wa_saida_02_tempx-doc_lctod = wa_096e-doc_lctod.
      wa_saida_02_tempx-doc_atua_preco    = wa_096e-doc_atua_preco.
      wa_saida_02_tempx-est_atua_preco    = wa_096e-est_atua_preco.
      wa_saida_02_tempx-doc_depreciacao   = wa_096e-doc_depreciacao.
      wa_saida_02_tempx-est_depreciacao   = wa_096e-est_depreciacao.
    ENDIF.

    MOVE wa_saida_02_tempx TO wa_saida_02_temp.

    IF wa_saida_02_tempx-parcela LE 1.
      IF wa_saida_02_temp-doc_atua_preco IS INITIAL AND wa_saida_02_temp-doc_depreciacao IS INITIAL AND wa_ziberr-message IS INITIAL AND wa_ziberd-message IS INITIAL.
        wa_saida_02_temp-status = icon_led_yellow.
      ELSEIF wa_ziberr IS INITIAL AND wa_saida_02_temp-doc_atua_preco IS NOT INITIAL AND wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.
        wa_saida_02_temp-status = icon_led_green.
      ELSEIF wa_ziberr IS NOT INITIAL OR wa_ziberd IS NOT INITIAL.
        wa_saida_02_temp-status = icon_led_red.
      ELSE.

        IF wa_saida_02_temp-doc_atua_preco IS INITIAL AND wa_saida_02_temp-doc_depreciacao IS INITIAL.
          wa_saida_02_temp-status = icon_led_yellow.
        ELSEIF wa_saida_02_temp-doc_atua_preco IS INITIAL AND wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.
          wa_saida_02_temp-status = icon_led_red.
        ELSEIF wa_saida_02_temp-doc_atua_preco IS NOT INITIAL AND wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.
          wa_saida_02_temp-status = icon_led_green.
        ELSE.
          wa_saida_02_temp-status = icon_led_yellow.
        ENDIF.

      ENDIF.

    ELSE.
      IF wa_saida_02_temp-doc_atua_preco IS INITIAL AND wa_saida_02_temp-doc_depreciacao IS INITIAL AND wa_ziberr-message IS INITIAL AND wa_ziberd-message IS INITIAL.
        wa_saida_02_temp-status = icon_led_yellow.
      ELSEIF wa_saida_02_temp-doc_atua_preco IS INITIAL AND wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.
        wa_saida_02_temp-status = icon_led_red.
      ELSEIF wa_saida_02_temp-doc_atua_preco IS NOT INITIAL AND wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.
        wa_saida_02_temp-status = icon_led_green.
      ELSEIF wa_saida_02_temp-doc_atua_preco IS NOT INITIAL AND wa_saida_02_temp-doc_depreciacao IS INITIAL.
        wa_saida_02_temp-status = icon_led_red.
*      ELSEIF wa_saida_02_temp-doc_atua_preco IS INITIAL AND wa_saida_02_temp-doc_depreciacao IS INITIAL AND ( wa_ziberr-message IS NOT INITIAL OR wa_ziberd-message IS NOT INITIAL ).
*        wa_saida_02_temp-status = icon_led_red.
*      ELSEif wa_ziberr-message is initial and wa_ziberd-messagem is initial.
*        wa_saida_02_temp-status = icon_led_yellow.
      ENDIF.
    ENDIF.

** USD
**    MOVE wa_saida_02_temp-ativ_d_uso_tot TO wa_saida_02_tempx-ativ_d_uso_totu. " Verif...
*    IF wa_saida_02_tempx-parcela LE 1.
*      wa_saida_02_temp-depreciacaou = ( wa_saida_02_tempx-ativ_d_uso_totu / lv_meses ).
*    ELSE.
*      READ TABLE it_saida_02_temp INTO DATA(wa_saida_02_tempdu) WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
*      IF sy-subrc IS INITIAL." AND wa_saida_02_tempdu-compdats(4) NE wa_saida_02_tempx-compdats(4).
**        wa_saida_02_temp-depreciacaou = ( wa_saida_02_tempx-ativ_d_uso_totu / ( lv_meses - ( wa_saida_02_tempx-parcela - 1 ) ) ).
**      ELSE.
*        wa_saida_02_temp-depreciacaou = ( wa_saida_02_tempx-ativ_d_uso_totu / ( lv_meses - ( wa_saida_02_tempx-parcela - 1 ) ) +  wa_saida_02_tempdu-depreciacaou ).
*      ENDIF.
*    ENDIF.

*-----
    IF wa_saida_02_temp-competencia(7) EQ lv_vig(7).
      wa_saida_02_temp-tcolor = 'C300'.




    ENDIF.

    IF wa_saida_02_temp-compdats EQ lv_vigf.
      DATA(lv_start) = abap_on.
    ELSEIF lv_start IS NOT INITIAL.
      lv_contf = lv_contf + 1.
      IF lv_contf EQ '12'.
        lv_dperio  = wa_saida_02_temp-compdats.
      ELSEIF lv_contf LT '12'.
        lv_dperioc = wa_saida_02_temp-compdats.
      ENDIF.
    ENDIF.

*    IF lv_start IS NOT INITIAL.
    lv_cont_adut = lv_cont_adut + 1.
    IF lv_cont_adut LE '12'.
      lv_soma_adut = ( wa_saida_02_temp-ativ_d_uso_tot ) + ( lv_soma_adut ).
    ENDIF.
    IF lv_cont_adut EQ '13'.
      wa_saida_02_temp-ativ_d_uso_tot = ( ( wa_saida_02_temp-ativ_uso_d_cp ) + ( wa_saida_02_temp-ativ_uso_d_lp ) + ( lv_soma_adut ) ).
      FREE: lv_cont_adut, lv_soma_adut.
      lv_cont_adut = lv_cont_adut + 1.
*        lv_soma_adut = ( wa_saida_02_temp-ativ_d_uso_tot ) + ( lv_soma_adut ).
    ENDIF.

    lv_cont_adutu = lv_cont_adutu + 1.
    IF lv_cont_adutu LE '12' AND lv_rein IS INITIAL.
      lv_soma_adutu = ( wa_saida_02_temp-ativ_d_uso_totu ) + ( lv_soma_adutu ).
    ENDIF.
    IF lv_cont_adutu EQ '13' AND lv_rein IS INITIAL.

*      CLEAR lv_rein.
*---------------------------
      CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
        EXPORTING
          i_date = wa_saida_02_temp-compdats
        IMPORTING
          e_date = lv_compdatsu.

      lv_compdatsu = lv_compdatsu + 1.

      zcl_util=>conv_data_us_br( EXPORTING i_data = lv_compdatsu
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
         INTO wl_tcurri
       WHERE fcurr EQ 'BRL'
         AND tcurr EQ 'USD'
         AND kurst EQ 'B'
         AND gdatu EQ data.
      IF sy-subrc IS INITIAL AND wl_tcurri-ukurs IS NOT INITIAL.
        lv_fechi = abs( wl_tcurri-ukurs ).

*        CONCATENATE  p_ano p_mes <f_096>-vigencia_de+6(2) INTO lv_datav.
*
*        DATA: lv_confcomp TYPE c.
*
*        IF lv_datav(6) EQ wa_saida_02_temp-compdats(6).
*          lv_confcomp = abap_true.
*        ELSE.
*          CLEAR lv_confcomp.
*        ENDIF.

*        lv_datav = lv_datav + 1.

*        IF lv_datav GE <f_096>-vigencia_de AND lv_datav LE <f_096>-vigencia_ate AND lv_found IS INITIAL AND lv_confcomp IS NOT INITIAL.
*          lv_found = abap_true.
** wa_saida_02_temp-compdats
*          lg_txf = lv_fech.
*        ENDIF.
      ELSE.
        CLEAR: wl_tcurri, lv_fechi.
      ENDIF.
*----------------------------------

      IF lv_fechi IS NOT INITIAL.
        wa_saida_02_temp-ativ_d_uso_totu = ( ( ( ( wa_saida_02_temp-ativ_uso_d_cp ) + ( wa_saida_02_temp-ativ_uso_d_lp ) ) / ( lv_fechi ) ) + ( lv_soma_adutu ) ). "Tx. Câmbio
      ELSE.
        wa_saida_02_temp-ativ_d_uso_totu = ( ( ( ( wa_saida_02_temp-ativ_uso_d_cp ) + ( wa_saida_02_temp-ativ_uso_d_lp ) ) ) + ( lv_soma_adutu ) ).
      ENDIF.
      FREE: lv_cont_adutu, lv_soma_adutu.
      lv_cont_adutu = lv_cont_adutu + 1.
      lv_rein = abap_true.
    ELSEIF lv_rein IS NOT INITIAL AND lv_cont_adut LE '12'.


*---------------------------Buscar taxa do proximo mês
      CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
        EXPORTING
          i_date = wa_saida_02_temp-compdats
        IMPORTING
          e_date = lv_compdatsu.

      lv_compdatsu = lv_compdatsu + 1.

      zcl_util=>conv_data_us_br( EXPORTING i_data = lv_compdatsu
                                 RECEIVING e_data = vl_data ).

      "zcl_util=>conv_data_us_br( EXPORTING i_data = wa_saida_02_temp-compdats
      "                          RECEIVING e_data = vl_data ).

      " Função standard para converter data em formato gravado na TCURR
      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = vl_data
        IMPORTING
          output = data.

      " Tabela responsavel por gravar a taxa do câmbio.
      SELECT SINGLE *
        FROM tcurr
         INTO wl_tcurri
       WHERE fcurr EQ 'BRL'
         AND tcurr EQ 'USD'
        AND kurst EQ 'B'
         AND gdatu EQ data.
      IF sy-subrc IS INITIAL AND wl_tcurri-ukurs IS NOT INITIAL.
        lv_fechi = abs( wl_tcurri-ukurs ).

      ELSE.
        CLEAR: wl_tcurri, lv_fechi.
      ENDIF.
*----------------------------------

      IF lv_fechi IS NOT INITIAL.
        wa_saida_02_temp-ativ_d_uso_totu = ( wa_saida_02_tempx-ativ_d_uso_tot / lv_fechi ).
      ELSE.
        wa_saida_02_temp-ativ_d_uso_totu = ( wa_saida_02_tempx-ativ_d_uso_tot ).
      ENDIF.

    ENDIF.
*    ENDIF. "lv_start

* BRL
    lv_contd = lv_contd + 1.
    IF wa_saida_02_tempx-parcela LE 1.
      lv_contd = lv_contd + 1.
      wa_saida_02_temp-depreciacao = ( wa_saida_02_tempx-ativ_d_uso_tot / lv_meses ).
    ELSE.

*      READ TABLE it_saida_02_temp INTO DATA(wa_saida_02_tempd) WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
*      IF sy-subrc IS INITIAL AND wa_saida_02_tempd-compdats(4) NE wa_saida_02_temp-compdats(4).
*
*        wa_saida_02_temp-depreciacao = ( wa_saida_02_temp-ativ_d_uso_tot / ( lv_meses - ( wa_saida_02_temp-parcela - 1 ) ) ).

      IF lv_contd EQ '14'.
        wa_saida_02_temp-depreciacao = ( wa_saida_02_temp-ativ_d_uso_tot / ( lv_meses - ( wa_saida_02_temp-parcela - 1 ) ) ).
      ELSEIF lv_contd EQ '15'.
        READ TABLE it_saida_02_temp INTO DATA(wa_saida_02_tempd) WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
        IF sy-subrc IS INITIAL.
          wa_saida_02_temp-depreciacao = ( wa_saida_02_temp-ativ_d_uso_tot / ( lv_meses - ( wa_saida_02_temp-parcela - 1 ) ) +  wa_saida_02_tempd-depreciacao ).
        ENDIF.
        FREE lv_contd.
      ELSE."IF sy-subrc IS INITIAL.
        READ TABLE it_saida_02_temp INTO wa_saida_02_tempd WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
        IF sy-subrc IS INITIAL.
          wa_saida_02_temp-depreciacao = ( wa_saida_02_temp-ativ_d_uso_tot / ( lv_meses - ( wa_saida_02_temp-parcela - 1 ) ) +  wa_saida_02_tempd-depreciacao ).
        ENDIF.
      ENDIF.
    ENDIF.

* USD
    lv_contu = lv_contu + 1.
    IF wa_saida_02_tempx-parcela LE 1.
      lv_contu = lv_contu + 1.
      wa_saida_02_temp-depreciacaou = ( wa_saida_02_temp-ativ_d_uso_totu / lv_meses ).
    ELSE.
      IF lv_contu EQ '14'.
        wa_saida_02_temp-depreciacaou = ( ( wa_saida_02_temp-ativ_d_uso_totu ) / ( lv_meses - ( wa_saida_02_tempx-parcela - 1 ) ) ).
      ELSEIF lv_contu EQ '15'.
        READ TABLE it_saida_02_temp INTO DATA(wa_saida_02_tempdu) WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
        IF sy-subrc IS INITIAL.
          wa_saida_02_temp-depreciacaou = ( ( wa_saida_02_temp-ativ_d_uso_totu ) / ( ( lv_meses - ( wa_saida_02_tempx-parcela - 1 ) ) ) + ( wa_saida_02_tempdu-depreciacaou ) ).
        ENDIF.
        FREE lv_contu.
      ELSE.
        READ TABLE it_saida_02_temp INTO wa_saida_02_tempdu WITH KEY parcela = ( wa_saida_02_temp-parcela - 1 ).
        IF sy-subrc IS INITIAL.
          IF wa_saida_02_tempdu-depreciacaou LT 0.
            wa_saida_02_temp-depreciacaou = ( ( wa_saida_02_temp-ativ_d_uso_totu ) / ( lv_meses - ( wa_saida_02_tempx-parcela - 1 ) ) ).
            wa_saida_02_temp-depreciacaou =  ( ( wa_saida_02_temp-depreciacaou ) + ( wa_saida_02_tempdu-depreciacaou ) ).
          ELSE.
            wa_saida_02_temp-depreciacaou = ( ( wa_saida_02_temp-ativ_d_uso_totu ) / ( ( lv_meses - ( wa_saida_02_tempx-parcela - 1 ) ) ) + ( wa_saida_02_tempdu-depreciacaou ) ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_fechi IS NOT INITIAL.
      wa_saida_02_temp-dolar = lv_fechi.
    ENDIF.

    APPEND wa_saida_02_temp TO it_saida_02_temp.
    CLEAR: wa_saida_02_temp.
  ENDLOOP.

  IF lv_dperio IS NOT INITIAL.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = lv_dperio
      IMPORTING
        e_date = lv_dperio.
  ENDIF.

  IF lv_dperioc IS NOT INITIAL.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = lv_dperioc
      IMPORTING
        e_date = lv_dperioc.
  ENDIF.


* Melhorias - Arrend - Melhoria de calculo transfere parcela #112403 RJF - Ini
  DATA: lv_tabix       TYPE sy-tabix,
        wa_saida_02_tv TYPE ty_saida_02_temp,
        wa_saida_02_tp TYPE ty_saida_02_temp.

  FREE it_saida_02_dep.

  LOOP AT it_saida_02_temp INTO DATA(wa_saida_02_t).
    lv_tabix = lv_tabix + 1.

    MOVE wa_saida_02_t-parcela TO wa_saida_02_t-parckey.

* Venc
    READ TABLE it_parcela INTO DATA(wa_parc) WITH KEY parcela = wa_saida_02_t-competencia.
    IF sy-subrc IS INITIAL.
      wa_saida_02_tv-compdats = wa_saida_02_t-compdats.
      wa_saida_02_tv-parckey = wa_saida_02_t-parcela - '0.09'.
      wa_saida_02_tv-tipo = 'V'.
      wa_saida_02_tv-parcela = abap_off.
      wa_saida_02_tv-tcolor = 'C600'.

      READ TABLE it_saida_02_temp INTO DATA(wa_saida) WITH KEY parcela = ( wa_saida_02_t-parcela - 1 ).
      IF sy-subrc IS INITIAL.
        wa_saida_02_tv-preco_saca_m    = wa_saida-preco_saca_m.
      ENDIF.

      wa_saida_02_tv-status          = icon_led_yellow.
      wa_saida_02_tv-competencia     = wa_saida_02_t-competencia.

      READ TABLE it_saida_02_temp INTO DATA(wa_saida_02_tva) WITH KEY parcela = ( wa_saida_02_t-parcela - 1 ).

      wa_saida_02_tv-saca_clas_cp    = ( wa_saida_02_tva-saca_clas_lp ) - ( wa_saida_02_t-saca_clas_lp ).

      lv_saca_clas_cp = CONV #( wa_saida_02_tv-saca_clas_cp ). "RJF solicitação Amaury para teste arrendodamento alv
      wa_saida_02_tv-saca_clas_cp = CONV #( lv_saca_clas_cp ).
      wa_saida_02_tv-atua_prec_cp    = ( wa_saida_02_tv-preco_saca_m * wa_saida_02_tv-saca_clas_cp ). "adu

      IF wa_parc-pg_parcela IS NOT INITIAL. "apcp
*        READ TABLE it_saida_02_temp INTO DATA(wa_saida_02_tva) WITH KEY parcela = ( wa_saida_02_t-parcela - 1 ).
        wa_saida_02_t-ativ_uso_d_cp   = ( ( ( wa_saida_02_t-atua_prec_cp ) - ( wa_saida_02_tv-atua_prec_cp ) ) - ( wa_saida_02_tva-atua_prec_cp ) ).
      ELSE.
*        READ TABLE it_saida_02_temp INTO wa_saida_02_tva WITH KEY parcela = ( wa_saida_02_t-parcela - 1 ).
        wa_saida_02_t-ativ_uso_d_cp   = ( ( ( wa_saida_02_t-atua_prec_cp ) - ( wa_saida_02_tv-atua_prec_cp ) ) - ( wa_saida_02_tva-atua_prec_cp ) ).
      ENDIF.

      wa_saida_02_tv-ativ_uso_d_cp   = abap_off.
      wa_saida_02_tv-saca_clas_lp    = wa_saida_02_t-saca_clas_lp.

      lv_saca_clas_lp = CONV #( wa_saida_02_tv-saca_clas_lp ). "RJF solicitação Amaury para teste arrendodamento alv LP
      wa_saida_02_tv-saca_clas_lp = CONV #( lv_saca_clas_lp ).

      wa_saida_02_tv-atua_prec_lp    = ( wa_saida_02_tv-preco_saca_m * wa_saida_02_tv-saca_clas_lp ). "adu

      IF wa_parc-parcela IS NOT INITIAL.
        wa_saida_02_t-ativ_uso_d_lp   = ( wa_saida_02_t-atua_prec_lp ) - ( wa_saida_02_tv-atua_prec_lp ).
*        ELSE.
*          wa_saida_02_tv-ativ_uso_d_lp   = abap_off. "aplp
      ENDIF.
      wa_saida_02_tv-ativ_uso_d_lp   = abap_off. "aplp
      wa_saida_02_tv-ativ_d_uso_tot  = abap_off.
      wa_saida_02_tv-ativ_d_uso_totu = abap_off.
      wa_saida_02_tv-depreciacao     = abap_off.
      wa_saida_02_tv-depreciacaou    = abap_off.
      wa_saida_02_tv-doc_atua_preco  = abap_off.
      wa_saida_02_tv-est_atua_preco  = abap_off.
      wa_saida_02_tv-doc_depreciacao = abap_off.
      wa_saida_02_tv-est_depreciacao = abap_off.
      wa_saida_02_tv-lt_doc          = abap_off.
      wa_saida_02_tv-lt_depre        = abap_off.

      lv_lin = lv_lin + 1.
      APPEND wa_saida_02_tv TO it_saida_02_dep.
      FREE wa_saida_02_tv.
    ENDIF.

* Pag
    READ TABLE it_parcela INTO wa_parc WITH KEY pg_parcela = wa_saida_02_t-competencia.
    IF sy-subrc IS INITIAL.

      wa_saida_02_tp-compdats = wa_saida_02_t-compdats.
      wa_saida_02_tp-parckey  = wa_saida_02_t-parcela - '0.08'.
      wa_saida_02_tp-tipo = 'P'.
      wa_saida_02_tp-parcela = abap_off.
      wa_saida_02_tp-tcolor = 'C500'.

*      READ TABLE it_saida_02_temp INTO wa_saida WITH KEY parcela = ( wa_saida_02_t-parcela - 1 ).
*      IF sy-subrc IS INITIAL.

      wa_saida_02_tp-status          = icon_led_yellow.
      wa_saida_02_tp-competencia     = wa_saida_02_t-competencia.
      wa_saida_02_tp-preco_saca_m    = abap_off. "wa_saida-preco_saca_m.
      wa_saida_02_tp-saca_clas_cp    = abap_off. "wa_saida_02_t-saca_clas_lp.
*      wa_saida_02_tp-atua_prec_cp    = wa_saida_02_t-atua_prec_cp * ( -1 ) . "adu p
      wa_saida_02_tp-atua_prec_cp    = wa_parc-valor_pagamento * ( -1 ) . "adu p
      wa_saida_02_tp-ativ_uso_d_cp   = abap_off.
      wa_saida_02_tp-saca_clas_lp    = wa_saida_02_t-saca_clas_lp.

      lv_saca_clas_lp = CONV #( wa_saida_02_tp-saca_clas_lp ). "RJF solicitação Amaury para teste arrendodamento alv LP pagamento
      wa_saida_02_tp-saca_clas_lp = CONV #( lv_saca_clas_lp ).

      wa_saida_02_tp-atua_prec_lp    = ( wa_saida_02_tp-preco_saca_m * wa_saida_02_tp-saca_clas_lp ). "adu p

      READ TABLE it_saida_02_temp INTO DATA(wa_saida_02_tvp) WITH KEY parcela = ( wa_saida_02_t-parcela - 1 ).
      IF wa_parc-pg_parcela IS NOT INITIAL.
        wa_saida_02_t-ativ_uso_d_lp   = ( wa_saida_02_t-atua_prec_lp ) - ( wa_saida_02_tvp-atua_prec_lp ).
*        ELSE.
*          wa_saida_02_tv-ativ_uso_d_lp   = abap_off. "aplp
      ENDIF.

*      IF wa_parc-pg_parcela IS NOT INITIAL. "apcp
*        READ TABLE it_saida_02_temp INTO DATA(wa_saida_02_tva) WITH KEY parcela = ( wa_saida_02_t-parcela - 1 ).
*        wa_saida_02_t-ativ_uso_d_cp   = ( wa_saida_02_t-atua_prec_cp ) - ( wa_saida_02_tv-atua_prec_cp ) - ( wa_saida_02_tva-atua_prec_cp ).
*      ELSE.
*        READ TABLE it_saida_02_temp INTO wa_saida_02_tva WITH KEY parcela = ( wa_saida_02_t-parcela - 1 ).
*        wa_saida_02_t-ativ_uso_d_cp   = ( wa_saida_02_t-atua_prec_cp ) - ( wa_saida_02_tv-atua_prec_cp ) - ( wa_saida_02_tva-atua_prec_cp ).
*      ENDIF.


      wa_saida_02_tp-ativ_uso_d_lp   = abap_off.
      wa_saida_02_tp-ativ_d_uso_tot  = abap_off.
      wa_saida_02_tp-ativ_d_uso_totu = abap_off.
      wa_saida_02_tp-depreciacao     = abap_off.
      wa_saida_02_tp-depreciacaou    = abap_off.
      wa_saida_02_tp-doc_atua_preco  = abap_off.
      wa_saida_02_tp-est_atua_preco  = abap_off.
      wa_saida_02_tp-doc_depreciacao = abap_off.
      wa_saida_02_tp-est_depreciacao = abap_off.
      wa_saida_02_tp-lt_doc          = abap_off.
      wa_saida_02_tp-lt_depre        = abap_off.

*      ENDIF.
      lv_lin = lv_lin + 1.
      APPEND wa_saida_02_tp TO it_saida_02_dep.
      FREE wa_saida_02_tp.
    ENDIF.

    APPEND wa_saida_02_t TO it_saida_02_dep.

  ENDLOOP.

  IF it_saida_02_dep[] IS NOT INITIAL.
    SORT it_saida_02_dep BY parckey.
  ENDIF.
* Melhorias - Arrend - Melhoria de calculo transfere parcela #112403 RJF - Fim

*  IF it_saida_02_temp IS NOT INITIAL.
*    MOVE it_saida_02_temp[] TO it_saida_02_dep[].
*  ENDIF.

* Lines uptate
  SELECT DISTINCT *
    FROM zglt096e
    INTO TABLE it_096el
    FOR ALL ENTRIES IN it_saida_02_dep
    WHERE cod_contrato EQ p_cont
      AND competencia  EQ it_saida_02_dep-competencia
      AND parckey      EQ it_saida_02_dep-parckey.

  IF sy-subrc IS INITIAL AND it_096el[] IS NOT INITIAL.

    SELECT DISTINCT *
    FROM zib_contabil_err
    INTO TABLE @DATA(it_ziberr)
    FOR ALL ENTRIES IN @it_096el
    WHERE obj_key EQ @it_096el-obj_key.

    SELECT DISTINCT *
    FROM zib_contabil_err
    APPENDING TABLE @it_ziberr
    FOR ALL ENTRIES IN @it_096el
    WHERE obj_key EQ @it_096el-obj_keyd.

    SELECT DISTINCT *
    FROM zib_contabil_err
    APPENDING TABLE @it_ziberr
    FOR ALL ENTRIES IN @it_096el
    WHERE obj_key EQ @it_096el-obj_keylpcp.

    LOOP AT it_saida_02_dep INTO DATA(wa_saida_02_templ).
      lv_tabix = sy-tabix.
      READ TABLE it_096el INTO wa_096el WITH KEY competencia = wa_saida_02_templ-competencia
                                                 parckey     = wa_saida_02_templ-parckey. "RJF

      IF sy-subrc IS INITIAL.
        IF wa_096el-obj_key IS NOT INITIAL.
          READ TABLE it_ziberr INTO DATA(wa_ziberrn) WITH KEY obj_key = wa_096el-obj_key.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_ziberrn.
          ENDIF.
        ENDIF.

        IF wa_096el-obj_keyd IS NOT INITIAL.
          READ TABLE it_ziberr INTO DATA(wa_ziberrd) WITH KEY obj_key = wa_096el-obj_keyd.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_ziberrd.
          ENDIF.
        ENDIF.

        IF wa_096el-obj_keylpcp IS NOT INITIAL.
          READ TABLE it_ziberr INTO DATA(wa_ziberrlpcp) WITH KEY obj_key = wa_096el-obj_keylpcp.
          IF sy-subrc IS NOT INITIAL.
            CLEAR wa_ziberrd.
          ENDIF.

          IF wa_096el-lt_lpcp IS NOT INITIAL.
            wa_saida_02_templ-lt_lpcp = wa_096el-lt_lpcp.
          ENDIF.

        ENDIF.

      ELSE.
        CLEAR: wa_ziberrd, wa_ziberrn, wa_ziberrlpcp.
      ENDIF.

      IF wa_saida_02_templ-parcela NE 0.

        CLEAR: wa_saida_02_templ-status.
        DATA(lv_ldolar) = wa_saida_02_templ-dolar.
        IF wa_096el-doc_atua_preco IS NOT INITIAL AND wa_096el-doc_depreciacao IS NOT INITIAL
          AND ( wa_ziberrn-message IS INITIAL AND wa_ziberrd-message IS INITIAL ).
          wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-status = icon_led_green.
          " MODIFY it_saida_02_dep FROM wa_saida_02_templ INDEX lv_tabix.
        ELSEIF wa_096el-doc_atua_preco IS INITIAL AND wa_096el-doc_depreciacao IS NOT INITIAL
          AND ( wa_ziberrn-message IS INITIAL AND wa_ziberrd-message IS INITIAL ).
          wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-status = icon_led_yellow.
          " MODIFY it_saida_02_dep FROM wa_saida_02_templ INDEX lv_tabix.
        ELSEIF wa_096el-doc_atua_preco IS INITIAL AND wa_096el-doc_depreciacao IS NOT INITIAL
          AND ( wa_ziberrn-message IS NOT INITIAL OR wa_ziberrd-message IS NOT INITIAL ).
          wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-status = icon_led_red.
          " MODIFY it_saida_02_dep FROM wa_saida_02_templ INDEX lv_tabix.
        ELSEIF wa_096el-doc_atua_preco IS NOT INITIAL AND wa_096el-doc_depreciacao IS INITIAL
          AND ( wa_ziberrn-message IS NOT INITIAL OR wa_ziberrd-message IS NOT INITIAL ).
          wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-status = icon_led_red.
          " MODIFY it_saida_02_dep FROM wa_saida_02_templ INDEX lv_tabix.
        ELSEIF wa_096el-doc_atua_preco IS INITIAL AND wa_096el-doc_depreciacao IS INITIAL
          AND ( wa_ziberrn-message IS NOT INITIAL OR wa_ziberrd-message IS NOT INITIAL ).
*        wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-status = icon_led_red.
          "  MODIFY it_saida_02_dep FROM wa_saida_02_templ INDEX lv_tabix.
        ELSEIF ( wa_096el-est_atua_preco IS NOT INITIAL OR wa_096el-est_depreciacao IS NOT INITIAL )
          AND ( wa_ziberrn-message IS INITIAL AND wa_ziberrd-message IS INITIAL ).
*        wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-est_atua_preco = wa_096el-est_atua_preco.
          wa_saida_02_templ-est_depreciacao = wa_096el-est_depreciacao.
          wa_saida_02_templ-status = icon_led_yellow.
          "  MODIFY it_saida_02_dep FROM wa_saida_02_templ INDEX lv_tabix.
        ELSEIF wa_096el-doc_atua_preco IS INITIAL AND wa_096el-doc_depreciacao IS INITIAL
          AND ( wa_ziberrn-message IS INITIAL AND wa_ziberrd-message IS INITIAL ).
*        wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-status = icon_led_yellow.
          "  MODIFY it_saida_02_dep FROM wa_saida_02_templ INDEX lv_tabix.
*      ELSEIF wa_saida_02_templ-status NE '@5C@'.
*        wa_saida_02_templ-status = icon_led_yellow.
*        MODIFY it_saida_02_dep FROM wa_saida_02_templ INDEX lv_tabix.
        ENDIF.

      ELSE.

        CLEAR: wa_saida_02_templ-status.
        lv_ldolar = wa_saida_02_templ-dolar.
        IF wa_096el-doc_lpcp IS NOT INITIAL
          AND ( wa_ziberrlpcp-message IS INITIAL ).
          wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-status = icon_led_green.
        ELSEIF wa_096el-doc_lpcp IS INITIAL AND wa_096el-lt_lpcp IS NOT INITIAL
          AND ( wa_ziberrlpcp-message IS INITIAL ).
          wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-status = icon_led_yellow.
        ELSEIF wa_096el-doc_lpcp IS INITIAL
          AND ( wa_ziberrlpcp-message IS NOT INITIAL ).
          wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-status = icon_led_red.
        ELSEIF wa_096el-doc_lpcp IS NOT INITIAL
          AND ( wa_ziberrlpcp-message IS NOT INITIAL ).
          wa_saida_02_templ = CORRESPONDING #( wa_096el ).
          wa_saida_02_templ-status = icon_led_red.

        ELSEIF ( wa_096el-est_lpcp IS NOT INITIAL )
          AND ( wa_ziberrlpcp-message IS INITIAL ).
          wa_saida_02_templ-est_lpcp = wa_096el-est_lpcp.
          wa_saida_02_templ-status = icon_led_yellow.
        ELSEIF wa_096el-doc_lpcp IS INITIAL
          AND ( wa_ziberrlpcp-message IS INITIAL ).
          wa_saida_02_templ-status = icon_led_yellow.
        ENDIF.

      ENDIF.


      IF wa_saida_02_templ-competencia(7) EQ lv_vig(7)." AND wa_saida_02_templ-parcela NE 0.
        IF wa_saida_02_templ-parcela NE 0.
          wa_saida_02_templ-tcolor = 'C300'.
        ELSE.
          IF wa_saida_02_templ-tipo EQ 'V'.
            wa_saida_02_templ-tcolor = 'C600'.
            wa_saida_02_templ-lt_lpcp = wa_096el-lt_lpcp.
          ELSEIF wa_saida_02_templ-tipo EQ 'P'.
            wa_saida_02_templ-tcolor = 'C500'.
            wa_saida_02_templ-lt_lpcp = wa_096el-lt_lpcp.
          ENDIF.

        ENDIF.

        IF lg_txf IS NOT INITIAL.
          wa_saida_02_templ-dolar = lg_txf.
        ENDIF.
        wa_saida_02_templ-dolar = lv_ldolar.
        MODIFY it_saida_02_dep FROM wa_saida_02_templ INDEX lv_tabix.

      ELSEIF wa_saida_02_templ-parcela EQ 0 AND wa_saida_02_templ-tipo EQ 'V' AND wa_saida_02_templ-lt_lpcp IS NOT INITIAL.

*        wa_saida_02_templ-lt_lpcp = wa_096el-lt_lpcp.
        wa_saida_02_templ-tcolor = 'C600'.
*        wa_saida_02_templ-tipo = 'V'.

        IF lg_txf IS NOT INITIAL.
          wa_saida_02_templ-dolar = lg_txf.
        ENDIF.
        wa_saida_02_templ-dolar = lv_ldolar.
        MODIFY it_saida_02_dep FROM wa_saida_02_templ INDEX lv_tabix.

      ENDIF.

      CLEAR wa_096el.

    ENDLOOP.

  ENDIF.

  lg_pt = lines( it_saida_02_dep ).
  lg_pt = lg_pt - lv_lin.
  CLEAR lv_vig.
*  CLEAR lv_compdats.
*  CONCATENATE p_ano p_mes '01' INTO lv_compdats.

*  DELETE it_saida_02_dep WHERE compdats LT lv_compdats.

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

  TYPES:
    BEGIN OF ty_t001,
      bukrs TYPE t001-bukrs,
      land1 TYPE t001-land1,
    END OF   ty_t001,

    BEGIN OF ty_t005,
      land1 TYPE t005-land1,
      waers TYPE t005-waers,
      curin TYPE t005-curin,
      curha TYPE t005-curha,
    END OF   ty_t005.

  DATA:
    dp_resp        TYPE char2,
    e_num_lote     TYPE zlote_num,
    wl_zglt035     TYPE zglt035,
    v_ultimo_dia   TYPE sy-datum,
    gt_zglt032     TYPE TABLE OF zglt032,
    wa_zglt032     TYPE zglt032,
    lv_text        TYPE string,
    wa_t001        TYPE ty_t001,
    wa_t005        TYPE ty_t005,
    lv_cont        TYPE i,
    lv_tabixd      TYPE sy-tabix,
    lv_competencia TYPE char7,
    gt_zglt036     TYPE TABLE OF zglt036,
    wl_zglt036     TYPE zglt036,
    lv_datum       TYPE sy-datum,
    v_qtdforn      TYPE i,
    v_objkey       TYPE char20,
    v_data         TYPE sy-datum,
    lv_datap       TYPE sy-datum.

  DATA:
    vl_data  TYPE c LENGTH 10, "Variável de data
    data     TYPE sy-datum,    "Variável de data
    wl_tcurr TYPE tcurr,
    lv_ukurs TYPE zgle0018,
    lv_doc   TYPE zgle0018,
    lv_tabix TYPE i,
    lv_int   TYPE zgle0018,
    lv_for   TYPE zgle0018.


  CONCATENATE p_mes '/' p_ano INTO lv_competencia.

  IF p_class EQ '04'.

    CLEAR var_answer.

    READ TABLE it_saida_02_dep INTO wa_saida_02_temp WITH KEY competencia = lv_competencia
                                                              tipo = ''.
    IF sy-subrc IS INITIAL AND wa_saida_02_temp-doc_atua_preco IS NOT INITIAL.
      MESSAGE 'Já existe documento gerado!' TYPE 'E'.
      EXIT.
    ELSEIF sy-subrc IS NOT INITIAL.
      MESSAGE 'Período informado inválido!' TYPE 'E'.
    ENDIF.

    CONCATENATE 'Deseja realmente gerar os documentos da competência'
    ' (' lv_competencia ')?'
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

  READ TABLE it_saida_02_dep INTO wa_saida_02_temp WITH KEY competencia = lv_competencia
                                                            tipo = ''.
  lv_tabixd = sy-tabix.
  IF sy-subrc IS INITIAL AND wa_saida_02_temp-doc_atua_preco IS NOT INITIAL.
    MESSAGE 'Já existe documento gerado!' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT DISTINCT *
    UP TO 1 ROWS
    FROM zglt095
    INTO @DATA(wa_zglt095)
    WHERE tp_arrendamento EQ @wa_092-tp_arrendamento
      AND classificacao EQ @p_class.
  ENDSELECT.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Parâmetros de Contabilização não encontrado para o tipo de contrato.' TYPE 'E'.
  ENDIF.

  READ TABLE it_saida_02_dep INTO wa_saida_02_temp WITH KEY competencia = lv_competencia
                                                            tipo = ''.
  IF sy-subrc IS INITIAL AND wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.
    MESSAGE 'Já existe documento gerado!' TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT DISTINCT *
    FROM zglt095
    INTO TABLE @DATA(it_zglt095)
    WHERE tp_arrendamento EQ @wa_092-tp_arrendamento
      AND classificacao EQ @p_class.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Parâmetros de Contabilização não encontrado para o tipo de contrato.' TYPE 'E'.
  ELSE.
    SORT it_zglt095 BY id_parametro.
  ENDIF.

  dp_resp = '83'. "Contabilidade

  CALL METHOD zcl_gerar_lote=>create_lote
    EXPORTING
      i_bukrs      = wa_092-bukrs
      i_descr_lote = 'Arrendamento de Contratos'(002)
      i_user_resp  = sy-uname
      i_dep_resp   = dp_resp
    IMPORTING
      e_num_lote   = e_num_lote.

  READ TABLE it_saida_02_dep INTO wa_saida_02_temp WITH KEY competencia = lv_competencia
                                                            tipo = ''.
  IF sy-subrc IS INITIAL.
    lv_datum = wa_saida_02_temp-compdats.
  ELSE.
    lv_datum = sy-datum.
  ENDIF.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_datum
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  MOVE v_ultimo_dia TO v_data.

  MOVE:    e_num_lote                  TO wl_zglt035-lote,
           wa_092-bukrs                TO wl_zglt035-bukrs,
           ''                          TO wl_zglt035-tp_lcto,
           dp_resp                     TO wl_zglt035-dpto_resp,
           'BRL'                       TO wl_zglt035-moeda_doc,
           'LM'                        TO wl_zglt035-blart,
           text-002                    TO wl_zglt035-bktxt,
           v_data                      TO wl_zglt035-budat,
           v_data                      TO wl_zglt035-bldat,
           sy-datum                    TO wl_zglt035-dt_lcto,
           ''                          TO wl_zglt035-prov_est,
           p_mes                       TO wl_zglt035-monat,
           p_ano                       TO wl_zglt035-gjahr,
           sy-uname                    TO wl_zglt035-usnam,
           sy-datum                    TO wl_zglt035-dt_entrada,
           sy-uzeit                    TO wl_zglt035-hr_entrada.

  SELECT SINGLE * FROM zglt096c
    INTO @DATA(wa_zglt096c)
    WHERE cod_contrato EQ @wa_092-cod_contrato.

* Itens
  IF p_class EQ '04'.
    wl_zglt036-seqitem = 1.

    IF it_zglt095[] IS NOT INITIAL.
      READ TABLE it_zglt095 INTO DATA(wa_zglt095f) INDEX wl_zglt036-seqitem.
    ENDIF.

    DATA(it_094cp) = it_094[].
    DATA(it_094lp) = it_094[].

* Separação
    DATA(it_094cpz) = it_094[].
    DATA(it_094lpz) = it_094[].

    IF lv_dperio IS NOT INITIAL.
*      DELETE  it_094cp WHERE dt_vencimento    GT lv_dperio.
*      DELETE  it_094cp WHERE ( dt_pagamneto   LT lv_udmes
*                          AND  dt_pagamneto  IS NOT INITIAL
*                          AND  dt_pagamneto  NE '00000000' ).

*      DELETE  it_094cp WHERE ( dt_vencimento   GT lv_dperio
*                          AND ( dt_pagamneto   LT lv_udmes
*                          AND  ( dt_pagamneto   IS NOT INITIAL AND dt_vencimento LT lv_dperio )
*                          AND  ( dt_pagamneto   NE '00000000' AND dt_vencimento LT lv_dperio ) ) ).

* CP --------------------------
      DELETE it_094cpz WHERE dt_pagamneto   IS NOT INITIAL
                          AND dt_pagamneto   NE '00000000'.
      DELETE it_094cpz WHERE dt_vencimento    GT lv_dperio.

      IF it_094cpz IS NOT INITIAL.   "Não for vazia mesclar

        DELETE  it_094cp WHERE  dt_vencimento   GT lv_dperio
                            OR  dt_pagamneto    LT lv_udmes.

        IF it_094cpz IS NOT INITIAL OR it_094cp IS NOT INITIAL.
* Mesclar
          LOOP AT it_094cpz INTO DATA(wa_094cpz).

            APPEND wa_094cpz TO it_094cp.
            CLEAR wa_094cpz.

*            MODIFY it_094cp FROM wa_094cpz WHERE cod_contrato EQ wa_094cpz-cod_contrato "INDEX wa_094cpz-num_parcela.
*                                             AND num_parcela  EQ wa_094cpz-num_parcela.
          ENDLOOP.

          SORT it_094cp BY cod_contrato num_parcela.
          DELETE ADJACENT DUPLICATES FROM it_094cp COMPARING cod_contrato num_parcela.

        ENDIF.

      ELSE.

        DELETE  it_094cp WHERE  dt_vencimento   GT lv_dperio
                            OR  dt_pagamneto    LT lv_udmes.

      ENDIF.

* --------------------------
*
*      DELETE it_094lp WHERE ( dt_vencimento LE lv_dperio
*                          AND ( dt_pagamneto   LT lv_udmes
*                          AND  dt_pagamneto  IS NOT INITIAL
*                          AND  dt_pagamneto  NE '00000000' ) ).

* LP --------------------------
      DELETE it_094lpz WHERE dt_pagamneto   IS NOT INITIAL
                          AND dt_pagamneto   NE '00000000'.
      DELETE it_094lpz WHERE dt_vencimento    LE lv_dperio.

      IF it_094lpz IS NOT INITIAL.   "Não for vazia mesclar

        DELETE  it_094lp WHERE  dt_vencimento   LE lv_dperio
                            OR  dt_pagamneto    GT lv_udmes.

        IF it_094lpz IS NOT INITIAL OR it_094lp IS NOT INITIAL.
* Mesclar
          LOOP AT it_094lpz INTO DATA(wa_094lpz).
*            MODIFY it_094lp FROM wa_094lpz WHERE cod_contrato EQ wa_094lpz-cod_contrato "INDEX wa_094cpz-num_parcela.
*                                             AND num_parcela  EQ wa_094lpz-num_parcela.
            APPEND wa_094lpz TO it_094lp.
            CLEAR wa_094lpz.

          ENDLOOP.

          SORT it_094lp BY cod_contrato num_parcela.
          DELETE ADJACENT DUPLICATES FROM it_094lp COMPARING cod_contrato num_parcela.

        ENDIF.

      ELSE.

        DELETE  it_094lp WHERE  dt_vencimento   LE lv_dperio
                            OR  dt_pagamneto    LT lv_udmes.

      ENDIF.


    ELSE.
*      DELETE it_094cp WHERE ( dt_vencimento GT lv_dperioc
*                            AND ( dt_pagamneto  LT lv_udmes
*                            AND dt_pagamneto  IS NOT INITIAL
*                            AND dt_pagamneto  NE '00000000' ) ).

* CP --------------------------
      DELETE it_094cpz WHERE dt_pagamneto   IS NOT INITIAL
                          AND dt_pagamneto   NE '00000000'.
      DELETE it_094cpz WHERE dt_vencimento    GT lv_dperioc.

      IF it_094cpz IS NOT INITIAL.   "Não for vazia mesclar

        DELETE  it_094cp WHERE  dt_vencimento   GT lv_dperioc
                            OR  dt_pagamneto    LT lv_udmes.

        IF it_094cpz IS NOT INITIAL OR it_094cp IS NOT INITIAL.
* Mesclar
          LOOP AT it_094cpz INTO wa_094cpz.
*            MODIFY it_094cp FROM wa_094cpz WHERE cod_contrato EQ wa_094cpz-cod_contrato "INDEX wa_094cpz-num_parcela.
*                                             AND num_parcela  EQ wa_094cpz-num_parcela.

            APPEND wa_094cpz TO it_094cp.
            CLEAR wa_094cpz.

          ENDLOOP.

          SORT it_094cp BY cod_contrato num_parcela.
          DELETE ADJACENT DUPLICATES FROM it_094cp COMPARING cod_contrato num_parcela.

        ENDIF.

      ELSE.

        DELETE  it_094cp WHERE  dt_vencimento   GT lv_dperioc
                            OR  dt_pagamneto    LT lv_udmes.

      ENDIF.

      FREE: it_094lp.
    ENDIF.

    DATA(lv_qtdcp) = lines( it_094cp ).
    DATA(lv_qtdlp) = lines( it_094lp ).

    lv_cont = wl_zglt036-seqitem.
    wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
    wl_zglt036-hkont = wa_zglt095f-conta.

*BUG 99430 - BG - INICIO
*wl_zglt036-bschl = wa_zglt095f-chave_lcto.
    IF wa_saida_02_temp-ativ_d_uso_tot > 0.
      wl_zglt036-bschl = '40'.
    ELSE.
      wl_zglt036-bschl = '50'.
    ENDIF.

    wl_zglt036-umskz = wa_zglt095f-razao_especial.

*    wl_zglt036-dt_vct = v_data.

*    wl_zglt036-bewar = '100'. " Para testes mocado
    wl_zglt036-bewar = wa_zglt095f-bewar.
*      IF wa_zglt032-sgtxt IS INITIAL.
    wl_zglt036-sgtxt = wa_zglt095f-texto_historico.
*      ELSE.
*        wl_zglt036-sgtxt = wa_zglt032-sgtxt.
*      ENDIF.

*    wl_zglt036-sgtxt = wa_zglt032-sgtxt.
    wl_zglt036-gsber = wa_092-filial.
    wl_zglt036-zuonr = wa_092-atrib.


* Item 1compdats
    READ TABLE it_saida_02_dep INTO wa_saida_02_temp WITH KEY competencia = lv_competencia
                                                              tipo = ''.

    IF sy-subrc IS INITIAL.
*      wl_zglt036-vlr_moeda_doc =  abs( wa_saida_02_temp-ativ_d_uso_tot ).
      lv_doc = abs( wa_saida_02_temp-ativ_d_uso_tot ).
*      wl_zglt036-vlr_moeda_int = abs( wa_saida_02_temp-ativ_d_uso_tot ).
      lv_int = abs( wa_saida_02_temp-ativ_d_uso_tot ).

      wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
      wl_zglt036-vlr_moeda_int = CONV #( lv_int ).


      IF wa_saida_02_temp-ativ_d_uso_tot IS NOT INITIAL.
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
*          wl_zglt036-vlr_moeda_forte =  abs( wa_saida_02_temp-ativ_d_uso_tot / lv_ukurs ).
          lv_for =  abs( wa_saida_02_temp-ativ_d_uso_tot / lv_ukurs ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).

        ELSE.

*          wl_zglt036-vlr_moeda_forte =  abs( wa_saida_02_temp-ativ_d_uso_tot ).
          lv_for =  abs( wa_saida_02_temp-ativ_d_uso_tot ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ENDIF.
      ENDIF.

    ENDIF.

    APPEND wl_zglt036 TO gt_zglt036.
    CLEAR: wl_zglt036, wa_zglt032.


    DATA(li_arr) = wa_092-tp_arrendamento.
    SHIFT li_arr LEFT DELETING LEADING '0'.

    SELECT DISTINCT * FROM zglt092a
      INTO TABLE @DATA(it_zglt092a)
      WHERE cod_contrato EQ @wa_092-cod_contrato
        AND ( tp_arrendamento EQ @wa_092-tp_arrendamento
         OR tp_arrendamento EQ @li_arr ).

    IF sy-subrc IS INITIAL.
      v_qtdforn = lines( it_zglt092a ).
      IF v_qtdforn IS INITIAL.
        v_qtdforn = 1.
      ENDIF.
    ENDIF.

    CLEAR lv_tabix.

    LOOP AT it_zglt092a ASSIGNING FIELD-SYMBOL(<f_zglt092a>). " Fornecedor/clientes
*-----------------------------
* Quant. CP
      DO lv_qtdcp TIMES.
        lv_tabix = lv_tabix + 1.
        READ TABLE it_094cp INTO DATA(wa_094cp) INDEX lv_tabix.
        IF sy-subrc IS INITIAL.
          wl_zglt036-dt_vct = wa_094cp-dt_vencimento.
        ENDIF.

        lv_cont = lv_cont + 1.
        IF it_zglt095[] IS NOT INITIAL.
          READ TABLE it_zglt095 INTO wa_zglt095f INDEX lv_cont.
        ENDIF.

        wl_zglt036-seqitem = lv_cont.
        wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.

        IF <f_zglt092a>-fornecedor IS NOT INITIAL.
          wl_zglt036-hkont = <f_zglt092a>-fornecedor.
          IF wa_saida_02_temp-ativ_uso_d_cp GT 0.
            wl_zglt036-bschl = '39'.
          ELSE.
            wl_zglt036-bschl = '29'.
          ENDIF.

        ELSEIF <f_zglt092a>-cliente IS NOT INITIAL.
          wl_zglt036-hkont = <f_zglt092a>-cliente.
          IF wa_saida_02_temp-ativ_uso_d_cp > 0.
            wl_zglt036-bschl = '39'.
          ELSE.
            wl_zglt036-bschl = '29'.
          ENDIF.
        ELSE.
          MESSAGE 'Cliente/Fornecedor não cadastrado!' TYPE 'E' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF <f_zglt092a>-rcomp IS NOT INITIAL.
          wl_zglt036-vbund   = <f_zglt092a>-rcomp. "RJF
        ENDIF.

        IF <f_zglt092a>-cliente IS NOT INITIAL.
          wl_zglt036-bschl = '21'.
        ENDIF.

*        wl_zglt036-dt_vct = v_data.
        wl_zglt036-umskz = '='.
        wl_zglt036-kostl = ' '.
        wl_zglt036-anbwa = ' '.
        wl_zglt036-bewar = ' '.
        wl_zglt036-matnr = ' '. " MATKL?

        wl_zglt036-sgtxt = wa_zglt095f-texto_historico.
        wl_zglt036-gsber = wa_092-filial.
        wl_zglt036-zuonr = wa_092-atrib.

        IF wa_saida_02_temp-ativ_uso_d_cp IS NOT INITIAL AND v_qtdforn IS NOT INITIAL AND lv_qtdcp IS NOT INITIAL.
          wl_zglt036-vlr_moeda_doc =  abs( ( wa_saida_02_temp-ativ_uso_d_cp / lv_qtdcp ) / v_qtdforn ).
*          lv_doc = abs( wa_saida_02_temp-ativ_uso_d_cp / v_qtdforn ).
          lv_doc  = abs( ( wa_saida_02_temp-ativ_uso_d_cp / lv_qtdcp ) / v_qtdforn ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).

*          wl_zglt036-vlr_moeda_int = abs( ( wa_saida_02_temp-ativ_uso_d_cp / lv_qtdcp ) / v_qtdforn ).
          lv_int = abs( ( wa_saida_02_temp-ativ_uso_d_cp / lv_qtdcp ) / v_qtdforn ).
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
        ELSEIF wa_saida_02_temp-ativ_uso_d_cp IS NOT INITIAL AND v_qtdforn IS NOT INITIAL AND lv_qtdcp IS INITIAL.
*          wl_zglt036-vlr_moeda_doc =  abs( ( wa_saida_02_temp-ativ_uso_d_cp ) / v_qtdforn ).
          lv_doc = abs( wa_saida_02_temp-ativ_uso_d_cp / v_qtdforn ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).

*          wl_zglt036-vlr_moeda_int = abs( ( wa_saida_02_temp-ativ_uso_d_cp ) / v_qtdforn ).
          lv_int = abs( ( wa_saida_02_temp-ativ_uso_d_cp ) / v_qtdforn ).
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
        ENDIF.

        IF wa_saida_02_temp-ativ_uso_d_cp IS NOT INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
* Taxa
          lv_ukurs = abs( wl_tcurr-ukurs ).
*          wl_zglt036-vlr_moeda_forte = abs( ( ( wa_saida_02_temp-ativ_uso_d_cp / lv_qtdcp ) / v_qtdforn ) / lv_ukurs ).
          lv_for = abs( ( ( wa_saida_02_temp-ativ_uso_d_cp / lv_qtdcp ) / v_qtdforn ) / lv_ukurs ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ELSEIF wa_saida_02_temp-ativ_uso_d_cp IS NOT INITIAL AND wl_tcurr-ukurs IS INITIAL.
          lv_for = abs( ( ( wa_saida_02_temp-ativ_uso_d_cp / lv_qtdcp ) / v_qtdforn ) ).
*          wl_zglt036-vlr_moeda_forte = abs( ( ( wa_saida_02_temp-ativ_uso_d_cp / lv_qtdcp ) / v_qtdforn ) ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
          IF lv_ukurs IS INITIAL.
            lv_ukurs = 1.
          ENDIF.
          lv_for = abs( ( ( wa_saida_02_temp-ativ_uso_d_cp / lv_qtdcp ) / v_qtdforn ) / lv_ukurs ).
          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
        ENDIF.

        APPEND wl_zglt036 TO gt_zglt036.
        CLEAR: wl_zglt036.

      ENDDO.

      CLEAR lv_tabix.
*-----------------------------
* Quant. LP
      DO lv_qtdlp TIMES.

        lv_tabix = lv_tabix + 1.
        READ TABLE it_094lp INTO DATA(wa_094lp) INDEX lv_tabix.
        IF sy-subrc IS INITIAL.
          wl_zglt036-dt_vct = wa_094lp-dt_vencimento.
        ENDIF.

        lv_cont = lv_cont + 1.

        IF it_zglt095[] IS NOT INITIAL.
          READ TABLE it_zglt095 INTO wa_zglt095f INDEX lv_cont.
        ENDIF.

        wl_zglt036-seqitem = lv_cont.
        wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.

        IF <f_zglt092a>-rcomp IS NOT INITIAL.
          wl_zglt036-vbund   = <f_zglt092a>-rcomp.
        ENDIF.

        IF <f_zglt092a>-fornecedor IS NOT INITIAL.
          wl_zglt036-hkont = <f_zglt092a>-fornecedor.
          IF wa_saida_02_temp-ativ_uso_d_lp GT 0.
            wl_zglt036-bschl = '39' .
          ELSE.
            wl_zglt036-bschl = '29'.
          ENDIF.

        ELSEIF <f_zglt092a>-cliente IS NOT INITIAL.
          wl_zglt036-hkont = <f_zglt092a>-cliente.
        ELSE.
          MESSAGE 'Cliente/Fornecedor não cadastrado!' TYPE 'E' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF <f_zglt092a>-cliente IS NOT INITIAL.
          wl_zglt036-bschl = '21'.
        ENDIF.

*        wl_zglt036-dt_vct = v_data.

        wl_zglt036-umskz =  '%'."<f_zglt092a>-razao_especial.
        wl_zglt036-kostl = ' '.
        wl_zglt036-anbwa = ' '.
        wl_zglt036-bewar = ' '.
        wl_zglt036-matnr = ' '.

        wl_zglt036-sgtxt = wa_zglt095f-texto_historico.
        wl_zglt036-gsber = wa_092-filial.
        wl_zglt036-zuonr = wa_092-atrib.

        IF wa_saida_02_temp-ativ_uso_d_lp IS NOT INITIAL AND v_qtdforn IS NOT INITIAL.
*          wl_zglt036-vlr_moeda_doc =  abs( ( wa_saida_02_temp-ativ_uso_d_lp / lv_qtdlp ) / v_qtdforn ).
          lv_doc = abs( ( wa_saida_02_temp-ativ_uso_d_lp / lv_qtdlp ) / v_qtdforn ).
          wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).

*          wl_zglt036-vlr_moeda_int = abs( ( wa_saida_02_temp-ativ_uso_d_lp / lv_qtdlp ) / v_qtdforn ).
          lv_int = abs( ( wa_saida_02_temp-ativ_uso_d_lp / lv_qtdlp ) / v_qtdforn ).
          wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
        ENDIF.

        IF wa_saida_02_temp-ativ_uso_d_lp IS NOT INITIAL.
* Taxa
          lv_datap = v_data + 1.
          zcl_util=>conv_data_us_br( EXPORTING i_data = lv_datap
                                     RECEIVING e_data = vl_data ).

* Função standard para converter data em formato gravado na TCURR
          CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
            EXPORTING
              input  = vl_data
            IMPORTING
              output = data.

* Tabela responsavel por gravar a taxa do câmbio.
          SELECT SINGLE *
            FROM tcurr
             INTO wl_tcurr
           WHERE fcurr EQ 'BRL'
             AND tcurr EQ 'USD'
             AND kurst EQ 'B'
             AND gdatu EQ data.
          IF sy-subrc IS INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
            lv_ukurs = abs( wl_tcurr-ukurs ).
*            wl_zglt036-vlr_moeda_forte = abs( ( ( wa_saida_02_temp-ativ_uso_d_lp / lv_qtdlp ) / v_qtdforn ) / lv_ukurs ).
            lv_for = abs( ( ( wa_saida_02_temp-ativ_uso_d_lp / lv_qtdlp ) / v_qtdforn ) / lv_ukurs ).
            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
          ELSE.
*            wl_zglt036-vlr_moeda_forte = abs( ( ( wa_saida_02_temp-ativ_uso_d_lp / lv_qtdlp ) / v_qtdforn ) ).
            lv_for = abs( ( ( wa_saida_02_temp-ativ_uso_d_lp / lv_qtdlp ) / v_qtdforn ) ).
            wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
          ENDIF.
        ENDIF.

        APPEND wl_zglt036 TO gt_zglt036.
        CLEAR: wl_zglt036.

      ENDDO.

      CLEAR lv_tabix.
*-----------------------------
    ENDLOOP.


*    LOOP AT it_zglt092a ASSIGNING FIELD-SYMBOL(<f_zglt092a>). " Fornecedor/clientes
**Item 2
*      lv_cont = lv_cont + 1.
*
*      IF it_zglt095[] IS NOT INITIAL.
*        READ TABLE it_zglt095 INTO wa_zglt095f INDEX lv_cont.
*      ENDIF.
*
*      wl_zglt036-seqitem = lv_cont.
*      wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
**      wl_zglt036-hkont = wa_zglt095f-conta.
*
*      IF <f_zglt092a>-fornecedor IS NOT INITIAL.
*        wl_zglt036-hkont = <f_zglt092a>-fornecedor.
*        IF wa_saida_02_temp-ativ_uso_d_cp GT 0.
*          wl_zglt036-bschl = '39'.
*        ELSE.
*          wl_zglt036-bschl = '29'.
*        ENDIF.
*
*      ELSEIF <f_zglt092a>-cliente IS NOT INITIAL.
*        wl_zglt036-hkont = <f_zglt092a>-cliente.
*        IF wa_saida_02_temp-ativ_uso_d_cp > 0.
*          wl_zglt036-bschl = '39'.
*        ELSE.
*          wl_zglt036-bschl = '29'.
*        ENDIF.
*      ELSE.
*        MESSAGE 'Cliente/Fornecedor não cadastrado!' TYPE 'E' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
*
*      IF <f_zglt092a>-rcomp IS NOT INITIAL.
*        wl_zglt036-vbund   = <f_zglt092a>-rcomp. "RJF
*      ENDIF.
*
*      IF <f_zglt092a>-cliente IS NOT INITIAL.
*        wl_zglt036-bschl = '21'.
**      ELSE.
**        wl_zglt036-bschl = '39'.
*      ENDIF.
*
**      wl_zglt036-bschl = wa_zglt095f-chave_lcto.
*      wl_zglt036-dt_vct = v_data.
*
*      wl_zglt036-umskz = '='.  "<f_zglt092a>-razao_especial. "BUG 99430
*      wl_zglt036-kostl = ' '.
*      wl_zglt036-anbwa = ' '.
*      wl_zglt036-bewar = ' '.
*      wl_zglt036-matnr = ' '.
**MATKL
*
*      wl_zglt036-sgtxt = wa_zglt095f-texto_historico.
**      wl_zglt036-sgtxt = wa_zglt032-sgtxt.
*      wl_zglt036-gsber = wa_092-filial.
*
*      IF wa_saida_02_temp-ativ_uso_d_cp IS NOT INITIAL AND v_qtdforn IS NOT INITIAL.
*        wl_zglt036-vlr_moeda_doc =  abs( wa_saida_02_temp-ativ_uso_d_cp / v_qtdforn ).
*        lv_doc = abs( wa_saida_02_temp-ativ_uso_d_cp / v_qtdforn ).
*        wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*      ENDIF.
*
**      wl_zglt036-vlr_moeda_doc = abs( wa_saida_02_temp-ativ_uso_d_cp ).
*      wl_zglt036-vlr_moeda_int = abs( wa_saida_02_temp-ativ_uso_d_cp / v_qtdforn ).
*      lv_int = abs( wa_saida_02_temp-ativ_uso_d_cp / v_qtdforn ).
*      wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*      IF wa_saida_02_temp-ativ_uso_d_cp IS NOT INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
** taxa
*        lv_ukurs = abs( wl_tcurr-ukurs ).
*        wl_zglt036-vlr_moeda_forte = abs( ( wa_saida_02_temp-ativ_uso_d_cp / v_qtdforn ) / lv_ukurs ).
*        lv_for = abs( ( wa_saida_02_temp-ativ_uso_d_cp / v_qtdforn ) / lv_ukurs ).
*        wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*      ELSE.
*        wl_zglt036-vlr_moeda_forte = abs( ( wa_saida_02_temp-ativ_uso_d_cp / v_qtdforn ) ).
*        IF lv_ukurs IS INITIAL.
*          lv_ukurs = 1.
*        ENDIF.
*        lv_for = abs( ( wa_saida_02_temp-ativ_uso_d_cp / v_qtdforn ) / lv_ukurs ).
*        wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*      ENDIF.
*
*      APPEND wl_zglt036 TO gt_zglt036.
*      CLEAR: wl_zglt036.
*
**Item 3
*      lv_cont = lv_cont + 1.
*
*      IF it_zglt095[] IS NOT INITIAL.
*        READ TABLE it_zglt095 INTO wa_zglt095f INDEX lv_cont.
*      ENDIF.
*
*      wl_zglt036-seqitem = lv_cont.
*      wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
**      wl_zglt036-hkont = wa_zglt095f-conta.
*
*      IF <f_zglt092a>-rcomp IS NOT INITIAL.
*        wl_zglt036-vbund   = <f_zglt092a>-rcomp. "RJF
*      ENDIF.
*
*      IF <f_zglt092a>-fornecedor IS NOT INITIAL.
*        wl_zglt036-hkont = <f_zglt092a>-fornecedor.
*        IF wa_saida_02_temp-ativ_uso_d_lp GT 0.
*          wl_zglt036-bschl = '39' .
*        ELSE.
*          wl_zglt036-bschl = '29'.
*        ENDIF.
*
*      ELSEIF <f_zglt092a>-cliente IS NOT INITIAL.
*        wl_zglt036-hkont = <f_zglt092a>-cliente.
*      ELSE.
*        MESSAGE 'Cliente/Fornecedor não cadastrado!' TYPE 'E' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
*
*      IF <f_zglt092a>-cliente IS NOT INITIAL.
*        wl_zglt036-bschl = '21'.
**      ELSE.
**        wl_zglt036-bschl = '39'.
*      ENDIF.
*
**      wl_zglt036-bschl = wa_zglt095f-chave_lcto.
**      wl_zglt036-umskz = wa_zglt095f-razao_especial.
*      wl_zglt036-dt_vct = v_data.
*
*      wl_zglt036-umskz =  '%'."<f_zglt092a>-razao_especial. "BUG 99430
*      wl_zglt036-kostl = ' '.
*      wl_zglt036-anbwa = ' '.
*      wl_zglt036-bewar = ' '.
*      wl_zglt036-matnr = ' '.
*
*      wl_zglt036-sgtxt = wa_zglt095f-texto_historico.
**      wl_zglt036-sgtxt = wa_zglt032-sgtxt.
*      wl_zglt036-gsber = wa_092-filial.
*
*      IF wa_saida_02_temp-ativ_uso_d_lp IS NOT INITIAL AND v_qtdforn IS NOT INITIAL.
*        wl_zglt036-vlr_moeda_doc =  abs( wa_saida_02_temp-ativ_uso_d_lp / v_qtdforn ).
*        lv_doc = abs( wa_saida_02_temp-ativ_uso_d_lp / v_qtdforn ).
*        wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
*      ENDIF.
*
**      wl_zglt036-vlr_moeda_doc = abs( wa_saida_02_temp-ativ_uso_d_lp / v_qtdforn ).
*      wl_zglt036-vlr_moeda_int = abs( wa_saida_02_temp-ativ_uso_d_lp / v_qtdforn ).
*      lv_int = abs( wa_saida_02_temp-ativ_uso_d_lp / v_qtdforn ).
*      wl_zglt036-vlr_moeda_int = CONV #( lv_int ).
*
*      IF wa_saida_02_temp-ativ_uso_d_lp IS NOT INITIAL.
** taxa
*        lv_datap = v_data + 1.
*        zcl_util=>conv_data_us_br( EXPORTING i_data = lv_datap
*                                   RECEIVING e_data = vl_data ).
*
*        " Função standard para converter data em formato gravado na TCURR
*        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
*          EXPORTING
*            input  = vl_data
*          IMPORTING
*            output = data.
*
*        " Tabela responsavel por gravar a taxa do câmbio.
*        SELECT DISTINCT  * UP TO 1 ROWS
*          FROM tcurr
*           INTO wl_tcurr
*         WHERE fcurr EQ 'BRL'
*           AND tcurr EQ 'USD'
*           AND gdatu EQ data.
*        ENDSELECT.
*        IF sy-subrc IS INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
*          lv_ukurs = abs( wl_tcurr-ukurs ).
*          wl_zglt036-vlr_moeda_forte = abs( ( wa_saida_02_temp-ativ_uso_d_lp / v_qtdforn ) / lv_ukurs ).
*          lv_for = abs( ( wa_saida_02_temp-ativ_uso_d_lp / v_qtdforn ) / lv_ukurs ).
*          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*        ELSE.
*          wl_zglt036-vlr_moeda_forte = abs( ( wa_saida_02_temp-ativ_uso_d_lp / v_qtdforn ) ).
*          lv_for = abs( ( wa_saida_02_temp-ativ_uso_d_lp / v_qtdforn ) ).
*          wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
*        ENDIF.
*      ENDIF.
*
*      APPEND wl_zglt036 TO gt_zglt036.
*      CLEAR: wl_zglt036.
*    ENDLOOP.



    CLEAR lv_cont.
  ELSE. " Dep

    LOOP AT it_zglt095 ASSIGNING FIELD-SYMBOL(<f_zglt095>).

      wl_zglt036-seqitem = sy-tabix.
      wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
      wl_zglt036-hkont = <f_zglt095>-conta.
      clear : wl_zglt036-kostl.
      select single kostl from zglt092 into @wl_zglt036-kostl where bukrs = @wa_092-bukrs and filial = @wa_092-filial and cod_contrato = @wa_092-cod_contrato.
                                                            "bug 99430

      IF p_class EQ '05'.
        IF wa_saida_02_temp-depreciacao > 0.
          CASE sy-tabix.
            WHEN 1.
              wl_zglt036-bschl = '50' ."'40'. 122471 ZGL071- Arrendamento PSA
            WHEN 2.
              wl_zglt036-bschl = '40' . "'50'. 122471 ZGL071- Arrendamento PSA
          ENDCASE.
        ELSE.
          CASE sy-tabix.
            WHEN 1.
              wl_zglt036-bschl = '40'."'50'. 122471 ZGL071- Arrendamento PSA
            WHEN 2.
              wl_zglt036-bschl = '50'."'40'. 122471 ZGL071- Arrendamento PSA
          ENDCASE.
        ENDIF.
      ELSE.
        wl_zglt036-bschl = <f_zglt095>-chave_lcto.
      ENDIF.

      wl_zglt036-umskz = <f_zglt095>-razao_especial.
*      wl_zglt036-dt_vct = v_data.
      wl_zglt036-sgtxt = <f_zglt095>-texto_historico.
      wl_zglt036-gsber = wa_092-filial.
      wl_zglt036-zuonr = wa_092-atrib.

*      wl_zglt036-vlr_moeda_doc = CONV #( abs( wa_saida_02_temp-depreciacao ) ).
      lv_doc = abs( wa_saida_02_temp-depreciacao ).
      wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).
      lv_int =  abs( wa_saida_02_temp-depreciacao ).
      wl_zglt036-vlr_moeda_int = CONV #( lv_int ).

      IF wa_saida_02_temp-depreciacaou IS NOT INITIAL AND wl_tcurr-ukurs IS NOT INITIAL.
* taxa histórica
        lv_ukurs = abs( wl_tcurr-ukurs ).
        lv_for = abs( ( wa_saida_02_temp-depreciacaou / lv_ukurs ) ).
        wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
      ELSE.
        lv_for = abs( wa_saida_02_temp-depreciacaou ).
        wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
      ENDIF.

      APPEND wl_zglt036 TO gt_zglt036.

      CLEAR: wl_zglt036.
    ENDLOOP.

  ENDIF.
  """LP BUG ARREDONDAMENTO  #102071
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

*--------------------------------------------

  IF p_class EQ '04'.

    CONCATENATE 'ZGL17' wl_zglt035-doc_lcto p_ano INTO v_objkey.

    SELECT DISTINCT *
      UP TO 1 ROWS
       FROM zib_contabil
       INTO @DATA(wa_zib_contabil)
       WHERE obj_key  = @v_objkey.
    ENDSELECT.

    SELECT DISTINCT *
      UP TO 1 ROWS
       FROM zib_contabil_chv
       INTO @DATA(wa_zib_contabilx)
       WHERE obj_key  = @v_objkey.
    ENDSELECT.

    IF sy-subrc IS INITIAL AND wa_zib_contabilx-belnr IS NOT INITIAL.

      MOVE:    wa_zib_contabilx-belnr TO wa_saida_02_temp-doc_atua_preco,
               wl_zglt035-doc_lcto    TO wa_saida_02_temp-doc_lcto,
               v_objkey               TO wa_saida_02_temp-obj_key,
               e_num_lote             TO wa_saida_02_temp-lt_doc,
               abap_false             TO wa_saida_02_temp-est_atua_preco.

    ELSE.
      SELECT DISTINCT *
         FROM zib_contabil_err
         INTO TABLE @DATA(it_zib_contabil_err)
         WHERE obj_key  = @v_objkey.

      IF sy-subrc IS NOT INITIAL." AND it_zib_contabil_err[] IS NOT INITIAL.
        wa_saida_02_temp-status = icon_led_yellow.
        MOVE:
        wl_zglt035-doc_lcto    TO wa_saida_02_temp-doc_lcto,
        v_objkey               TO wa_saida_02_temp-obj_key,
        e_num_lote             TO wa_saida_02_temp-lt_doc.
*        abap_false             TO wa_saida_02_temp-est_atua_preco.
      ELSE.
        wa_saida_02_temp-status = icon_led_red.
        MOVE:
        wl_zglt035-doc_lcto    TO wa_saida_02_temp-doc_lcto,
        v_objkey               TO wa_saida_02_temp-obj_key,
        e_num_lote             TO wa_saida_02_temp-lt_doc.
      ENDIF.

    ENDIF.

  ELSE. " Dep..

    CONCATENATE 'ZGL17' wl_zglt035-doc_lcto p_ano INTO v_objkey.

    SELECT DISTINCT *
      UP TO 1 ROWS
       FROM zib_contabil
       INTO @DATA(wa_zib_contabilxc)
       WHERE obj_key  = @v_objkey.
    ENDSELECT.

    SELECT DISTINCT *
      UP TO 1 ROWS
       FROM zib_contabil_chv
       INTO @DATA(wa_zib_contabilxx)
       WHERE obj_key  = @v_objkey.
    ENDSELECT.

    IF wa_zib_contabilxx-belnr IS NOT INITIAL.
      MOVE:
               wa_zib_contabilxx-belnr TO wa_saida_02_temp-doc_depreciacao,
               wl_zglt035-doc_lcto     TO wa_saida_02_temp-doc_lctod,
               v_objkey                TO wa_saida_02_temp-obj_keyd,
               e_num_lote              TO wa_saida_02_temp-lt_depre,
               abap_false              TO wa_saida_02_temp-est_depreciacao.
    ELSE.
      MOVE:
*             wa_zib_contabilxx-belnr TO wa_saida_02_temp-doc_depreciacao,
               wl_zglt035-doc_lcto     TO wa_saida_02_temp-doc_lctod,
               v_objkey                TO wa_saida_02_temp-obj_keyd,
               e_num_lote              TO wa_saida_02_temp-lt_depre.
*             abap_false              TO wa_saida_02_temp-est_depreciacao.

    ENDIF.


    IF wa_saida_02_temp-doc_atua_preco IS NOT INITIAL AND wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.
      wa_saida_02_temp-status = icon_led_green.
    ENDIF.
  ENDIF.

  MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX lv_tabixd. "wa_saida_02_temp-parcela.

  CONCATENATE p_mes '/' p_ano INTO DATA(lv_comp).

  SELECT SINGLE * FROM zglt096e INTO @DATA(w_zglt096e)
                      WHERE cod_contrato EQ @wa_092-cod_contrato AND
                            competencia  EQ @lv_comp AND
                            parckey      EQ @wa_saida_02_temp-parckey.

  IF sy-subrc IS INITIAL.
    CONCATENATE 'ZGL17' wl_zglt035-doc_lcto p_ano INTO v_objkey.
    wa_096e = CORRESPONDING #( wa_saida_02_temp ).
    wa_096e-cod_contrato = wa_092-cod_contrato.
    wa_096e-parcelat = lg_pt.
    wa_096e-dt_atualizacao = sy-datum.

    IF p_class EQ '04'.
      wa_096e-obj_key  = v_objkey.
*      wa_096e-doc_lcto  = wl_zglt035-doc_lcto.
    ELSE.
*      READ TABLE it_096e INTO DATA(wa_096ex) WITH KEY cod_contrato = wa_092-cod_contrato.
*      IF sy-subrc IS INITIAL.
*        wa_096e-obj_key  = wa_096ex-obj_key.
*      ENDIF.
      wa_096e-obj_key  = wa_saida_02_temp-obj_key.
      wa_096e-obj_keyd = v_objkey.
*      wa_096e-doc_lctod  = wl_zglt035-doc_lcto.
    ENDIF.

    wa_096e-ctime = sy-uzeit.
    wa_096e-cmuser = sy-uname.
    MODIFY zglt096e FROM wa_096e.
    COMMIT WORK.
    CLEAR: wa_096e, wa_saida_02_temp.

  ELSE.
    wa_096e = CORRESPONDING #( wa_saida_02_temp ).
    wa_096e-cod_contrato = wa_092-cod_contrato.
    wa_096e-parcelat = lg_pt.
    wa_096e-dt_atualizacao = sy-datum.

    IF p_class EQ '04'.
      wa_096e-obj_key  = v_objkey.
    ELSE.
*      READ TABLE it_096e INTO DATA(wa_096exx) WITH KEY cod_contrato = wa_092-cod_contrato.
*      IF sy-subrc IS INITIAL.
*        wa_096e-obj_key  = wa_096exx-obj_key.
*      ENDIF.
      wa_096e-obj_key  = wa_saida_02_temp-obj_key.
      wa_096e-obj_keyd = v_objkey.
*      IF wa_096e-obj_keyd IS NOT INITIAL.
*        APPEND VALUE #( fieldname = 'STATUS' color-col = 4 color-int = 1 color-inv = 0 ) TO wa_saida_02_temp-tcolor.
*      ENDIF.

    ENDIF.
    wa_096e-ctime = sy-uzeit.
    wa_096e-cmuser = sy-uname.
    MODIFY zglt096e FROM wa_096e.
    COMMIT WORK.

  ENDIF.

* Melhoria - RJF - Ini
  IF p_class NE '04'.
    PERFORM f_gerar_documento_lpcp USING '09'. " Doc. Lp CP
  ENDIF.
* Melhoria - RJF - Fim

  IF p_class NE '04' AND wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.
    MESSAGE 'Documentos gerados com sucesso!' TYPE 'S' DISPLAY LIKE 'S'.
  ELSEIF p_class NE '04'.
    MESSAGE 'Processo realizado!' TYPE 'S' DISPLAY LIKE 'S'.
  ENDIF.

  CLEAR: wa_096e, wa_saida_02_temp.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estornar_documento .
  DATA: var_answer TYPE c.

  DATA: it_msg         TYPE TABLE OF bdcmsgcoll, " WITH HEADER LINE,
        lv_competencia TYPE char7,
        v_ultimo_dia   TYPE sy-datum,
        lv_text        TYPE string,
        v_ultimo_diac  TYPE char10,
        wa_msg         TYPE bdcmsgcoll.

  CONCATENATE p_mes '/' p_ano INTO lv_competencia.

  READ TABLE it_saida_02_dep INTO wa_saida_02_temp WITH KEY competencia = lv_competencia
                                                            tipo = ''.
  DATA(lv_tabix) = sy-tabix.
  IF wa_saida_02_temp-doc_atua_preco IS INITIAL AND wa_saida_02_temp-doc_depreciacao IS INITIAL.
    MESSAGE 'Não existe documento a ser estornado!' TYPE 'E'.
    EXIT.
  ELSEIF wa_saida_02_temp-doc_atua_preco IS NOT INITIAL AND wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.

    CONCATENATE 'Deseja realmente estornar os documentos da competência'
    ' (' lv_competencia ')?'
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
    MESSAGE 'Erro documentos a serem estornados!' TYPE 'E'.
    EXIT.
  ENDIF.

  CHECK var_answer EQ '1'.

  CHECK wa_saida_02_temp-doc_atua_preco IS NOT INITIAL AND wa_saida_02_temp-doc_depreciacao IS NOT INITIAL.

  FREE: it_dta.
  DEFINE shdb.
    CLEAR IT_DTA.
    WA_DTA-PROGRAM   = &1.
    WA_DTA-DYNPRO    = &2.
    WA_DTA-DYNBEGIN  = &3.
    WA_DTA-FNAM      = &4.
    WA_DTA-FVAL      = &5.
    APPEND WA_DTA TO IT_DTA.
  END-OF-DEFINITION.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = wa_saida_02_temp-compdats
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  CONCATENATE v_ultimo_dia+6(2) '.' v_ultimo_dia+4(2) '.' v_ultimo_dia(4) INTO v_ultimo_diac.
*  v_ultimo_diac = v_ultimo_dia.

  shdb:
  'SAPMF05A' '0105' 'X'  ' '           ' ',
  ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
  ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
  ' '        ' '    ' '  'RF05A-BELNS' wa_saida_02_temp-doc_atua_preco,
  ' '        ' '    ' '  'BKPF-BUKRS'  wa_092-bukrs,
  ' '        ' '    ' '  'RF05A-GJAHS' lv_competencia+3(4),
  ' '        ' '    ' '  'BSIS-BUDAT' v_ultimo_diac,
  ' '        ' '    ' '  'UF05A-STGRD' '01'.

  opt-dismode = 'N'.
  CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.
**
  IF sy-subrc IS INITIAL.

*Vai na tabela BKPFcom - BUKRS = empresa processamento - BELNR = em processamento
*selecionar campo BKPF-STBLG
    SELECT stblg FROM bkpf
      UP TO 1 ROWS
      INTO @DATA(lv_stblg)
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @wa_saida_02_temp-doc_atua_preco.
    ENDSELECT.

    IF sy-subrc IS INITIAL AND lv_stblg IS NOT INITIAL.
*    MOVE:    wa_saida_02_temp-doc_atua_preco TO wa_saida_02_temp-est_atua_preco,
      MOVE:    lv_stblg TO wa_saida_02_temp-est_atua_preco,
               abap_false                  TO wa_saida_02_temp-doc_atua_preco.
*      MODIFY it_saida_02_dep FROM wa_saida_02_temp index lv_tabix.
    ELSE.
      MOVE:    abap_false  TO wa_saida_02_temp-est_atua_preco,
*               abap_false  TO wa_saida_02_temp-doc_depreciacao.

               icon_led_red TO wa_saida_02_temp-status.
    ENDIF.


    MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX lv_tabix.
  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*--------------------------

  FREE: it_dta.
  DEFINE shdb.
    CLEAR IT_DTA.
    WA_DTA-PROGRAM   = &1.
    WA_DTA-DYNPRO    = &2.
    WA_DTA-DYNBEGIN  = &3.
    WA_DTA-FNAM      = &4.
    WA_DTA-FVAL      = &5.
    APPEND WA_DTA TO IT_DTA.
  END-OF-DEFINITION.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = wa_saida_02_temp-compdats
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  CONCATENATE v_ultimo_dia+6(2) '.' v_ultimo_dia+4(2) '.' v_ultimo_dia(4) INTO v_ultimo_diac.
*  v_ultimo_diac = v_ultimo_dia.

  shdb:
  'SAPMF05A' '0105' 'X'  ' '           ' ',
  ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
  ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
  ' '        ' '    ' '  'RF05A-BELNS' wa_saida_02_temp-doc_depreciacao,
  ' '        ' '    ' '  'BKPF-BUKRS'  wa_092-bukrs,
  ' '        ' '    ' '  'RF05A-GJAHS' lv_competencia+3(4),
  ' '        ' '    ' '  'BSIS-BUDAT' v_ultimo_diac,
  ' '        ' '    ' '  'UF05A-STGRD' '01'.

  opt-dismode = 'N'.
  CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.
**

  IF sy-subrc IS INITIAL.

*Vai na tabela BKPFcom - BUKRS = empresa processamento - BELNR = em processamento
*selecionar campo BKPF-STBLG
    SELECT stblg FROM bkpf
      UP TO 1 ROWS
      INTO @DATA(lv_stblgd)
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @wa_saida_02_temp-doc_depreciacao.
    ENDSELECT.

    IF sy-subrc IS INITIAL AND lv_stblgd IS NOT INITIAL.
*    MOVE:    wa_saida_02_temp-doc_atua_preco TO wa_saida_02_temp-est_atua_preco,
      MOVE:    lv_stblgd    TO wa_saida_02_temp-est_depreciacao,
               abap_false  TO wa_saida_02_temp-doc_depreciacao,
               icon_led_yellow TO wa_saida_02_temp-status.
*      MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX 1.

    ELSE.

      MOVE:    abap_false  TO wa_saida_02_temp-est_depreciacao,
*               abap_false  TO wa_saida_02_temp-doc_depreciacao.

               icon_led_red TO wa_saida_02_temp-status.
*      MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX 1.

    ENDIF.

*    MOVE:    wa_saida_02_temp-doc_depreciacao TO wa_saida_02_temp-est_depreciacao,
*             abap_false                   TO wa_saida_02_temp-doc_depreciacao.

    MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX lv_tabix.

    wa_096e = CORRESPONDING #( wa_saida_02_temp ).
    wa_096e-cod_contrato = wa_092-cod_contrato.
    wa_096e-parcelat = lg_pt.
    wa_096e-dt_atualizacao = sy-datum.
    wa_096e-cmuser = sy-uname.
    MODIFY zglt096e FROM wa_096e.
    COMMIT WORK AND WAIT.
    CLEAR: wa_096e, wa_saida_02_temp.

* Melhoria - RJF - Ini
    PERFORM f_estornar_documento_lpcp. " Estorno. Lp CP
* Melhoria - RJF - Fim

    IF wa_saida_02_temp-doc_depreciacao IS INITIAL.
      MESSAGE 'Documentos estornados com sucesso!' TYPE 'S'.
    ENDIF.

  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*  CALL METHOD grid1->refresh_table_display.

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
  SELECT DISTINCT *
    FROM zglt096e
    INTO TABLE it_096e
    WHERE cod_contrato EQ wa_092-cod_contrato.

  IF it_096e[] IS NOT INITIAL.
    SELECT DISTINCT *
       FROM zib_contabil_err
       INTO TABLE @DATA(it_zib_contabil_err)
      FOR ALL ENTRIES IN @it_096e
       WHERE obj_key  EQ @it_096e-obj_key.

    SELECT DISTINCT *
       FROM zib_contabil_err
       INTO TABLE @DATA(it_zib_contabil_errd)
      FOR ALL ENTRIES IN @it_096e
       WHERE obj_key  EQ @it_096e-obj_keyd.

    SELECT DISTINCT *
       FROM zib_contabil_err
       INTO TABLE @DATA(it_zib_contabil_errlpcp)
      FOR ALL ENTRIES IN @it_096e
       WHERE obj_key  EQ @it_096e-obj_keylpcp. "RJF

    SELECT DISTINCT *
       FROM zib_contabil_chv
       INTO TABLE @DATA(it_zib_contabil_chv)
      FOR ALL ENTRIES IN @it_096e
       WHERE obj_key  EQ @it_096e-obj_key.

    SELECT DISTINCT *
       FROM zib_contabil_chv
       INTO TABLE @DATA(it_zib_contabil_chvd)
      FOR ALL ENTRIES IN @it_096e
       WHERE obj_key  EQ @it_096e-obj_keyd.

    SELECT DISTINCT *
       FROM zib_contabil_chv
       INTO TABLE @DATA(it_zib_contabil_chvlpcp)
      FOR ALL ENTRIES IN @it_096e
       WHERE obj_key  EQ @it_096e-obj_keylpcp.     "RJF
*  ENDIF.

*  IF it_saida_02_dep[] IS NOT INITIAL.
*Vai na tabela BKPFcom - BUKRS = empresa processamento - BELNR = em processamento
*selecionar campo BKPF-STBLG

    SELECT DISTINCT bukrs, belnr, stblg FROM bkpf
      INTO TABLE @DATA(it_stblg)
      FOR ALL ENTRIES IN @it_096e
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @it_096e-doc_atua_preco.

    SELECT DISTINCT bukrs, belnr, stblg FROM bkpf
      INTO TABLE @DATA(it_stblg_d)
      FOR ALL ENTRIES IN @it_096e
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @it_096e-doc_depreciacao.

    SELECT DISTINCT bukrs, belnr, stblg FROM bkpf
      INTO TABLE @DATA(it_stblg_lpcp)
      FOR ALL ENTRIES IN @it_096e
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @it_096e-doc_lpcp.

    SELECT *
       FROM zglt035
     INTO TABLE it_035
      FOR ALL ENTRIES IN it_096e
      WHERE doc_lcto EQ it_096e-doc_lcto
        AND bukrs    EQ wa_092-bukrs
        AND tp_lcto  EQ '0000000000'.

    SELECT *
       FROM zglt035
     APPENDING TABLE it_035
      FOR ALL ENTRIES IN it_096e
      WHERE doc_lcto EQ it_096e-doc_lctod
        AND bukrs    EQ wa_092-bukrs
        AND tp_lcto  EQ '0000000000'.

    SELECT *
       FROM zglt035
     APPENDING TABLE it_035
      FOR ALL ENTRIES IN it_096e
      WHERE doc_lcto EQ it_096e-doc_lctolpcp
        AND bukrs    EQ wa_092-bukrs
        AND tp_lcto  EQ '0000000000'.

  ENDIF.

  IF it_096e[] IS NOT INITIAL AND it_saida_02_dep[] IS NOT INITIAL. "it_zib_contabil_chv[] IS NOT INITIAL " Sucess

    LOOP AT it_saida_02_dep INTO DATA(wa_saida_02_tempupt).
      DATA(lv_tabix) = sy-tabix.

      READ TABLE it_096e INTO DATA(wa_096e) WITH KEY competencia = wa_saida_02_tempupt-competencia
                                                     parckey     = wa_saida_02_tempupt-parckey.
      DATA(lv_tabix96) = sy-tabix.
      IF sy-subrc IS INITIAL AND wa_096e-obj_key IS NOT INITIAL.

        READ TABLE it_zib_contabil_chv INTO DATA(wa_zib_contabil_chv) WITH KEY obj_key = wa_096e-obj_key.
        IF sy-subrc IS NOT INITIAL.
          CLEAR wa_zib_contabil_chv.
        ELSEIF wa_zib_contabil_chv-belnr IS NOT INITIAL. " ver se est

          SELECT bukrs, belnr, stblg FROM bkpf
            UP TO 1 ROWS
            INTO @DATA(wa_stblg)
            WHERE bukrs EQ @wa_092-bukrs
              AND belnr EQ @wa_zib_contabil_chv-belnr.
          ENDSELECT.
*      if sy-subrc is initial and wa_stblg-belnr EQ .
*
*      endif.

        ENDIF.

        READ TABLE it_zib_contabil_chvd INTO DATA(wa_zib_contabil_chvd) WITH KEY obj_key = wa_096e-obj_keyd.

        IF sy-subrc IS NOT INITIAL.
          CLEAR wa_zib_contabil_chvd.
        ELSEIF wa_zib_contabil_chvd-belnr IS NOT INITIAL. " ver se est.
          SELECT bukrs, belnr, stblg FROM bkpf
            UP TO 1 ROWS
            INTO @DATA(wa_stblg_d)
            WHERE bukrs EQ @wa_092-bukrs
              AND belnr EQ @wa_zib_contabil_chvd-belnr.
          ENDSELECT.
*            IF sy-subrc IS INITIAL AND wa_stblg_d-belnr NE
        ENDIF.

        IF sy-subrc IS INITIAL AND wa_zib_contabil_chvd-belnr IS NOT INITIAL.

*          IF wa_zib_contabil_chv-belnr IS NOT INITIAL.
**            wa_saida_02_tempupt-status = icon_led_red.
**          ELSE.
*            wa_saida_02_tempupt-status = icon_led_green.
*          ELSE.
*            wa_saida_02_tempupt-status = icon_led_yellow.
*          ENDIF.

          IF wa_saida_02_tempupt-doc_lcto IS INITIAL.
            wa_saida_02_tempupt-doc_lcto = wa_saida_02_tempupt-obj_key+5(10).
            wa_saida_02_tempupt-doc_lctod = wa_saida_02_tempupt-obj_keyd+5(10).
            wa_096e-doc_lcto = wa_saida_02_tempupt-doc_lcto.
            wa_096e-doc_lctod = wa_saida_02_tempupt-doc_lctod.

*            IF wa_saida_02_tempupt-doc_lctod IS NOT INITIAL.
*              APPEND VALUE #( fieldname = 'STATUS' color-col = 4 color-int = 1 color-inv = 0 ) TO wa_saida_02_temp-tcolor.
*            ENDIF.
          ENDIF.

          IF wa_saida_02_tempupt-doc_atua_preco IS INITIAL
            AND ( wa_stblg-stblg IS INITIAL
            OR wa_stblg-stblg NE wa_saida_02_tempupt-est_atua_preco )
            AND wa_saida_02_tempupt-est_atua_preco IS INITIAL.
            wa_saida_02_tempupt-doc_atua_preco = wa_zib_contabil_chv-belnr.
            wa_saida_02_tempupt-est_atua_preco = abap_false.
            wa_096e-doc_atua_preco = wa_zib_contabil_chv-belnr.
            wa_096e-est_atua_preco = abap_false.

            IF wa_saida_02_tempupt-doc_atua_preco IS NOT INITIAL
            AND wa_saida_02_tempupt-doc_depreciacao IS NOT INITIAL.
              wa_saida_02_tempupt-status = icon_led_green.
            ENDIF.

          ELSE.
            READ TABLE it_stblg INTO wa_stblg WITH KEY belnr = wa_096e-doc_atua_preco.
            IF sy-subrc IS INITIAL AND wa_stblg-stblg IS NOT INITIAL.

              wa_saida_02_tempupt-est_atua_preco = wa_stblg-stblg.
              wa_saida_02_tempupt-doc_atua_preco = abap_false.
              wa_096e-est_atua_preco = wa_stblg-stblg.
              wa_096e-doc_atua_preco = abap_false.

              IF wa_saida_02_tempupt-est_atua_preco IS NOT INITIAL
              AND wa_saida_02_tempupt-est_depreciacao IS NOT INITIAL.
                wa_saida_02_tempupt-status = icon_led_yellow.
              ENDIF.

            ELSEIF wa_stblg-stblg IS INITIAL AND wa_zib_contabil_chv-belnr IS NOT INITIAL.

              wa_saida_02_tempupt-doc_atua_preco = wa_zib_contabil_chv-belnr.
              wa_saida_02_tempupt-est_atua_preco = abap_false.
              wa_096e-doc_atua_preco = wa_zib_contabil_chv-belnr.
              wa_096e-est_atua_preco = abap_false.

            ELSE.
              CLEAR wa_stblg.
            ENDIF.

          ENDIF.

* dep
          IF wa_saida_02_tempupt-doc_depreciacao IS INITIAL
            AND ( wa_stblg_d-stblg IS INITIAL
            OR wa_stblg_d-stblg NE wa_saida_02_tempupt-est_depreciacao )
            AND wa_saida_02_tempupt-est_atua_preco IS INITIAL.
            wa_saida_02_tempupt-doc_depreciacao = wa_zib_contabil_chvd-belnr.
            wa_saida_02_tempupt-est_depreciacao = abap_false.
            wa_096e-doc_depreciacao = wa_zib_contabil_chvd-belnr.
            wa_096e-est_depreciacao = abap_false.

            IF wa_saida_02_tempupt-doc_atua_preco IS NOT INITIAL
            AND wa_saida_02_tempupt-doc_depreciacao IS NOT INITIAL.
              wa_saida_02_tempupt-status = icon_led_green.
            ENDIF.

          ELSE.

            READ TABLE it_stblg_d INTO wa_stblg_d WITH KEY belnr = wa_096e-doc_depreciacao.
            IF sy-subrc IS INITIAL AND wa_stblg_d-stblg IS NOT INITIAL.

              wa_saida_02_tempupt-est_depreciacao = wa_stblg_d-stblg.
              wa_saida_02_tempupt-doc_depreciacao = abap_false.
              wa_096e-est_depreciacao = wa_stblg_d-stblg.
              wa_096e-doc_depreciacao = abap_false.

              IF wa_saida_02_tempupt-est_atua_preco IS NOT INITIAL
              AND wa_saida_02_tempupt-est_depreciacao IS NOT INITIAL.
                wa_saida_02_tempupt-status = icon_led_yellow.
              ENDIF.

            ELSEIF wa_stblg_d-stblg IS INITIAL AND wa_zib_contabil_chv-belnr IS NOT INITIAL.
              wa_saida_02_tempupt-doc_depreciacao = wa_zib_contabil_chvd-belnr.
              wa_saida_02_tempupt-est_depreciacao = abap_false.
              wa_096e-doc_depreciacao = wa_zib_contabil_chvd-belnr.
              wa_096e-est_depreciacao = abap_false.

            ELSE.
              CLEAR wa_stblg_d.
            ENDIF.
          ENDIF.

        ENDIF.

        IF wa_saida_02_tempupt-doc_lcto IS NOT INITIAL.
          READ TABLE it_035 INTO DATA(wa_035) WITH KEY doc_lcto = wa_saida_02_tempupt-doc_lcto.
          IF sy-subrc IS INITIAL.
            wa_saida_02_tempupt-lt_doc = wa_035-lote.
            wa_096e-lt_doc = wa_035-lote.
          ENDIF.
        ENDIF.

        IF wa_saida_02_tempupt-doc_lctod IS NOT INITIAL.
          READ TABLE it_035 INTO DATA(wa_035d) WITH KEY doc_lcto = wa_saida_02_tempupt-doc_lctod.
          IF sy-subrc IS INITIAL.
            wa_saida_02_tempupt-lt_depre = wa_035d-lote.
            wa_096e-lt_depre = wa_035d-lote.
          ENDIF.
        ENDIF.

        IF wa_096e-ctime IS INITIAL.
          wa_096e-ctime = sy-uzeit.
        ENDIF.

        MODIFY it_saida_02_dep FROM wa_saida_02_tempupt INDEX lv_tabix. " WHERE doc_lcto = wa_saida_02_tempupt-doc_lcto.

        MODIFY it_096e FROM wa_096e INDEX lv_tabix96. " WHERE doc_lcto = wa_saida_02_tempupt-doc_lcto


*      ELSE. " não encontrou verif err
*
*        READ TABLE it_zib_contabil_err INTO DATA(wa_zib_contabil_err) WITH KEY obj_key = wa_096e-obj_key.
*
*        IF sy-subrc IS NOT INITIAL.
*          CLEAR wa_zib_contabil_err.
*        ENDIF.
*
*        READ TABLE it_zib_contabil_errd INTO DATA(wa_zib_contabil_errd) WITH KEY obj_key = wa_096e-obj_keyd.
*
*        IF sy-subrc IS INITIAL OR wa_zib_contabil_err-message IS NOT INITIAL OR wa_zib_contabil_errd-message IS NOT INITIAL.
*
*          wa_saida_02_tempupt-status = icon_led_red.
*          MODIFY it_saida_02_dep FROM wa_saida_02_tempupt INDEX lv_tabix. " WHERE doc_lcto = wa_saida_02_tempupt-doc_lcto.
*
*        ELSE.
*          CLEAR wa_zib_contabil_errd.
*        ENDIF.
      ELSEIF sy-subrc IS INITIAL AND wa_096e-obj_keylpcp IS NOT INITIAL. "RJF


*        READ TABLE it_zib_contabil_chv INTO DATA(wa_zib_contabil_chv) WITH KEY obj_key = wa_096e-obj_key.
*        IF sy-subrc IS NOT INITIAL.
*          CLEAR wa_zib_contabil_chv.
*        ELSEIF wa_zib_contabil_chv-belnr IS NOT INITIAL. " ver se est
*
*          SELECT bukrs, belnr, stblg FROM bkpf
*            UP TO 1 ROWS
*            INTO @DATA(wa_stblg)
*            WHERE bukrs EQ @wa_092-bukrs
*              AND belnr EQ @wa_zib_contabil_chv-belnr.
*          ENDSELECT.
**      if sy-subrc is initial and wa_stblg-belnr EQ .
**
**      endif.
*
*        ENDIF.

        READ TABLE it_zib_contabil_chvlpcp INTO DATA(wa_zib_contabil_chvlpcp) WITH KEY obj_key = wa_096e-obj_keylpcp.

        IF sy-subrc IS NOT INITIAL.
          CLEAR wa_zib_contabil_chvlpcp.
        ELSEIF wa_zib_contabil_chvlpcp-belnr IS NOT INITIAL. " ver se est.
          SELECT bukrs, belnr, stblg FROM bkpf
            UP TO 1 ROWS
            INTO @DATA(wa_stblg_lpcp)
            WHERE bukrs EQ @wa_092-bukrs
              AND belnr EQ @wa_zib_contabil_chvlpcp-belnr.
          ENDSELECT.
*            IF sy-subrc IS INITIAL AND wa_stblg_d-belnr NE
        ENDIF.

        IF sy-subrc IS INITIAL AND wa_zib_contabil_chvlpcp-belnr IS NOT INITIAL.

*          IF wa_zib_contabil_chv-belnr IS NOT INITIAL.
**            wa_saida_02_tempupt-status = icon_led_red.
**          ELSE.
*            wa_saida_02_tempupt-status = icon_led_green.
*          ELSE.
*            wa_saida_02_tempupt-status = icon_led_yellow.
*          ENDIF.

          IF wa_saida_02_tempupt-doc_lctolpcp IS INITIAL.
*            wa_saida_02_tempupt-doc_lcto = wa_saida_02_tempupt-obj_key+5(10).
            wa_saida_02_tempupt-doc_lctolpcp = wa_saida_02_tempupt-obj_keylpcp+5(10).
*            wa_096e-doc_lcto = wa_saida_02_tempupt-doc_lcto.
            wa_096e-doc_lctolpcp = wa_saida_02_tempupt-doc_lctolpcp.

*            IF wa_saida_02_tempupt-doc_lctod IS NOT INITIAL.
*              APPEND VALUE #( fieldname = 'STATUS' color-col = 4 color-int = 1 color-inv = 0 ) TO wa_saida_02_temp-tcolor.
*            ENDIF.
          ENDIF.

*--
*          IF wa_saida_02_tempupt-doc_atua_preco IS INITIAL
*            AND ( wa_stblg-stblg IS INITIAL
*            OR wa_stblg-stblg NE wa_saida_02_tempupt-est_atua_preco )
*            AND wa_saida_02_tempupt-est_atua_preco IS INITIAL.
*            wa_saida_02_tempupt-doc_atua_preco = wa_zib_contabil_chv-belnr.
*            wa_saida_02_tempupt-est_atua_preco = abap_false.
*            wa_096e-doc_atua_preco = wa_zib_contabil_chv-belnr.
*            wa_096e-est_atua_preco = abap_false.
*
*            IF wa_saida_02_tempupt-doc_atua_preco IS NOT INITIAL
*            AND wa_saida_02_tempupt-doc_depreciacao IS NOT INITIAL.
*              wa_saida_02_tempupt-status = icon_led_green.
*            ENDIF.
*
*          ELSE.
*            READ TABLE it_stblg INTO wa_stblg WITH KEY belnr = wa_096e-doc_atua_preco.
*            IF sy-subrc IS INITIAL AND wa_stblg-stblg IS NOT INITIAL.
*
*              wa_saida_02_tempupt-est_atua_preco = wa_stblg-stblg.
*              wa_saida_02_tempupt-doc_atua_preco = abap_false.
*              wa_096e-est_atua_preco = wa_stblg-stblg.
*              wa_096e-doc_atua_preco = abap_false.
*
*              IF wa_saida_02_tempupt-est_atua_preco IS NOT INITIAL
*              AND wa_saida_02_tempupt-est_depreciacao IS NOT INITIAL.
*                wa_saida_02_tempupt-status = icon_led_yellow.
*              ENDIF.
*
*            ELSEIF wa_stblg-stblg IS INITIAL AND wa_zib_contabil_chv-belnr IS NOT INITIAL.
*
*              wa_saida_02_tempupt-doc_atua_preco = wa_zib_contabil_chv-belnr.
*              wa_saida_02_tempupt-est_atua_preco = abap_false.
*              wa_096e-doc_atua_preco = wa_zib_contabil_chv-belnr.
*              wa_096e-est_atua_preco = abap_false.
*
*            ELSE.
*              CLEAR wa_stblg.
*            ENDIF.
*
*          ENDIF.

* dep
*          IF wa_saida_02_tempupt-doc_depreciacao IS INITIAL
*            AND ( wa_stblg_d-stblg IS INITIAL
*            OR wa_stblg_d-stblg NE wa_saida_02_tempupt-est_depreciacao )
*            AND wa_saida_02_tempupt-est_atua_preco IS INITIAL.
*            wa_saida_02_tempupt-doc_depreciacao = wa_zib_contabil_chvd-belnr.
*            wa_saida_02_tempupt-est_depreciacao = abap_false.
*            wa_096e-doc_depreciacao = wa_zib_contabil_chvd-belnr.
*            wa_096e-est_depreciacao = abap_false.
*
*            IF wa_saida_02_tempupt-doc_atua_preco IS NOT INITIAL
*            AND wa_saida_02_tempupt-doc_depreciacao IS NOT INITIAL.
*              wa_saida_02_tempupt-status = icon_led_green.
*            ENDIF.
*
*          ELSE.

*            READ TABLE it_stblg_d INTO wa_stblg_d WITH KEY belnr = wa_096e-doc_depreciacao.
*            IF sy-subrc IS INITIAL AND wa_stblg_d-stblg IS NOT INITIAL.
*
*              wa_saida_02_tempupt-est_depreciacao = wa_stblg_d-stblg.
*              wa_saida_02_tempupt-doc_depreciacao = abap_false.
*              wa_096e-est_depreciacao = wa_stblg_d-stblg.
*              wa_096e-doc_depreciacao = abap_false.
*
*              IF wa_saida_02_tempupt-est_atua_preco IS NOT INITIAL
*              AND wa_saida_02_tempupt-est_depreciacao IS NOT INITIAL.
*                wa_saida_02_tempupt-status = icon_led_yellow.
*              ENDIF.
*
*            ELSEIF wa_stblg_d-stblg IS INITIAL AND wa_zib_contabil_chv-belnr IS NOT INITIAL.
*              wa_saida_02_tempupt-doc_depreciacao = wa_zib_contabil_chvd-belnr.
*              wa_saida_02_tempupt-est_depreciacao = abap_false.
*              wa_096e-doc_depreciacao = wa_zib_contabil_chvd-belnr.
*              wa_096e-est_depreciacao = abap_false.
*
*            ELSE.
*              CLEAR wa_stblg_d.
*            ENDIF.
*          ENDIF.

* RJF
          IF wa_saida_02_tempupt-doc_lpcp IS INITIAL
            AND ( wa_stblg_lpcp-stblg IS INITIAL
            OR wa_stblg_lpcp-stblg NE wa_saida_02_tempupt-est_lpcp ).
            wa_saida_02_tempupt-doc_lpcp = wa_zib_contabil_chvlpcp-belnr.
            wa_saida_02_tempupt-est_lpcp = abap_false.
            wa_096e-doc_lpcp = wa_zib_contabil_chvlpcp-belnr.
            wa_096e-est_lpcp = abap_false.

            IF wa_saida_02_tempupt-doc_lpcp IS NOT INITIAL.
              wa_saida_02_tempupt-status = icon_led_green.
            ENDIF.

          ELSE.

            READ TABLE it_stblg_lpcp INTO wa_stblg_lpcp WITH KEY belnr = wa_096e-doc_lpcp.
            IF sy-subrc IS INITIAL AND wa_stblg_lpcp-stblg IS NOT INITIAL.

              wa_saida_02_tempupt-est_lpcp = wa_stblg_lpcp-stblg.
              wa_saida_02_tempupt-doc_lpcp = abap_false.
              wa_096e-est_lpcp = wa_stblg_lpcp-stblg.
              wa_096e-doc_lpcp = abap_false.

              IF wa_saida_02_tempupt-est_lpcp IS NOT INITIAL.
                wa_saida_02_tempupt-status = icon_led_yellow.
              ENDIF.

            ELSEIF wa_stblg_lpcp-stblg IS INITIAL AND wa_zib_contabil_chv-belnr IS NOT INITIAL.
              wa_saida_02_tempupt-doc_lpcp = wa_zib_contabil_chvd-belnr.
              wa_saida_02_tempupt-est_lpcp = abap_false.
              wa_096e-doc_lpcp = wa_zib_contabil_chvlpcp-belnr.
              wa_096e-est_lpcp = abap_false.

            ELSE.
              CLEAR wa_stblg_lpcp.
            ENDIF.
          ENDIF.
* RJF

        ENDIF.

*        IF wa_saida_02_tempupt-doc_lcto IS NOT INITIAL.
*          READ TABLE it_035 INTO DATA(wa_035) WITH KEY doc_lcto = wa_saida_02_tempupt-doc_lcto.
*          IF sy-subrc IS INITIAL.
*            wa_saida_02_tempupt-lt_doc = wa_035-lote.
*            wa_096e-lt_doc = wa_035-lote.
*          ENDIF.
*        ENDIF.
*
*        IF wa_saida_02_tempupt-doc_lctod IS NOT INITIAL.
*          READ TABLE it_035 INTO DATA(wa_035d) WITH KEY doc_lcto = wa_saida_02_tempupt-doc_lctod.
*          IF sy-subrc IS INITIAL.
*            wa_saida_02_tempupt-lt_depre = wa_035d-lote.
*            wa_096e-lt_depre = wa_035d-lote.
*          ENDIF.
*        ENDIF.


        IF wa_saida_02_tempupt-doc_lctolpcp IS NOT INITIAL. "RJF
          READ TABLE it_035 INTO DATA(wa_035lpcp) WITH KEY doc_lcto = wa_saida_02_tempupt-doc_lctolpcp.
          IF sy-subrc IS INITIAL.
            wa_saida_02_tempupt-lt_lpcp = wa_035lpcp-lote.
            wa_096e-lt_lpcp = wa_035lpcp-lote.
          ENDIF.
        ENDIF.

        IF wa_096e-ctime IS INITIAL.
          wa_096e-ctime = sy-uzeit.
        ENDIF.

        MODIFY it_saida_02_dep FROM wa_saida_02_tempupt INDEX lv_tabix. " WHERE doc_lcto = wa_saida_02_tempupt-doc_lcto.

        MODIFY it_096e FROM wa_096e INDEX lv_tabix96. " WHERE doc_lcto = wa_saida_02_tempupt-doc_lcto


      ENDIF.
    ENDLOOP.

    IF it_096e[] IS NOT INITIAL.
      MODIFY zglt096e FROM TABLE it_096e. " WHERE doc_lcto = wa_saida_02_tempupt-doc_lcto
      COMMIT WORK AND WAIT.
    ENDIF.


  ELSEIF it_zib_contabil_err[] IS NOT INITIAL AND it_096e[] IS NOT INITIAL. " err apenas

    LOOP AT it_saida_02_dep INTO DATA(wa_saida_02_tempupte).
      DATA(lv_tabixe) = sy-tabix.

      READ TABLE it_096e INTO DATA(wa_096ee) WITH KEY competencia = wa_saida_02_tempupte-competencia
                                                      parckey     = wa_saida_02_tempupte-parckey.
      IF sy-subrc IS INITIAL AND wa_096ee-obj_key IS NOT INITIAL.

        READ TABLE it_zib_contabil_err INTO DATA(wa_zib_contabil_erre) WITH KEY obj_key = wa_096ee-obj_key.

        IF sy-subrc IS NOT INITIAL.
          CLEAR wa_zib_contabil_erre.
        ENDIF.

        READ TABLE it_zib_contabil_errd INTO DATA(wa_zib_contabil_errde) WITH KEY obj_key = wa_096ee-obj_keyd.

        IF sy-subrc IS INITIAL OR wa_zib_contabil_erre-message IS NOT INITIAL OR wa_zib_contabil_errde-message IS NOT INITIAL.

          wa_saida_02_tempupte-status = icon_led_red.
          MODIFY it_saida_02_dep FROM wa_saida_02_tempupte INDEX lv_tabixe.
        ELSE.
          CLEAR wa_zib_contabil_errde.
        ENDIF.
      ELSEIF sy-subrc IS INITIAL AND wa_096ee-obj_keylpcp IS NOT INITIAL. "RJF

        READ TABLE it_zib_contabil_errlpcp INTO DATA(wa_zib_contabil_errlpcp) WITH KEY obj_key = wa_096ee-obj_keylpcp.

        IF sy-subrc IS INITIAL OR wa_zib_contabil_errlpcp-message IS NOT INITIAL.

          wa_saida_02_tempupte-status = icon_led_red.
          MODIFY it_saida_02_dep FROM wa_saida_02_tempupte INDEX lv_tabixe.
        ELSE.
          CLEAR wa_zib_contabil_errlpcp.
        ENDIF.

      ELSE.
        CLEAR wa_096ee.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_DOCUMENTO_LPCP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0573   text
*----------------------------------------------------------------------*
FORM f_gerar_documento_lpcp  USING p_class TYPE zgle0029.

  TYPES:
    BEGIN OF ty_t001,
      bukrs TYPE t001-bukrs,
      land1 TYPE t001-land1,
    END OF   ty_t001,

    BEGIN OF ty_t005,
      land1 TYPE t005-land1,
      waers TYPE t005-waers,
      curin TYPE t005-curin,
      curha TYPE t005-curha,
    END OF   ty_t005.

  DATA:
    dp_resp        TYPE char2,
    e_num_lote     TYPE zlote_num,
    wl_zglt035     TYPE zglt035,
    v_ultimo_dia   TYPE sy-datum,
    gt_zglt032     TYPE TABLE OF zglt032,
    wa_zglt032     TYPE zglt032,
    lv_text        TYPE string,
    wa_t001        TYPE ty_t001,
    wa_t005        TYPE ty_t005,
    lv_cont        TYPE i,
    lv_competencia TYPE char7,
    gt_zglt036     TYPE TABLE OF zglt036,
    wl_zglt036     TYPE zglt036,
    lv_datum       TYPE sy-datum,
    v_qtdforn      TYPE i,
    v_objkey       TYPE char20,
    v_data         TYPE sy-datum,
    lv_datap       TYPE sy-datum.

  DATA:
    vl_data  TYPE c LENGTH 10, "Variável de data
    data     TYPE sy-datum,    "Variável de data
    wl_tcurr TYPE tcurr,
    lv_ukurs TYPE zgle0018,
    lv_doc   TYPE zgle0018,
    lv_tabix TYPE i,
    lv_int   TYPE zgle0018,
    lv_for   TYPE zgle0018.


  CONCATENATE p_mes '/' p_ano INTO lv_competencia.

  READ TABLE it_saida_02_dep INTO wa_saida_02_temp WITH KEY competencia = lv_competencia
                                                            tipo = 'V'.
  IF sy-subrc IS INITIAL AND wa_saida_02_temp-doc_lpcp IS NOT INITIAL.
    MESSAGE 'Já existe documento gerado!' TYPE 'E'.
    EXIT.
  ELSEIF sy-subrc IS NOT INITIAL.
    EXIT.
  ENDIF.

  SELECT DISTINCT *
    UP TO 1 ROWS
    FROM zglt095
    INTO @DATA(wa_zglt095)
    WHERE tp_arrendamento EQ @wa_092-tp_arrendamento
      AND classificacao EQ @p_class.
  ENDSELECT.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Parâmetros de Contabilização não encontrado para o tipo de contrato.' TYPE 'E'.
  ENDIF.

  SELECT DISTINCT *
    FROM zglt095
    INTO TABLE @DATA(it_zglt095)
    WHERE tp_arrendamento EQ @wa_092-tp_arrendamento
      AND classificacao EQ @p_class.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Parâmetros de Contabilização não encontrado para o tipo de contrato.' TYPE 'E'.
  ELSE.
    SORT it_zglt095 BY id_parametro.
  ENDIF.

  dp_resp = '83'. "Contabilidade

  CALL METHOD zcl_gerar_lote=>create_lote
    EXPORTING
      i_bukrs      = wa_092-bukrs
      i_descr_lote = 'Arrendamento de Contratos'(002)
      i_user_resp  = sy-uname
      i_dep_resp   = dp_resp
    IMPORTING
      e_num_lote   = e_num_lote.

  READ TABLE it_saida_02_dep INTO wa_saida_02_temp WITH KEY competencia = lv_competencia
                                                            tipo = 'V'.
  IF sy-subrc IS INITIAL.
    DATA(lv_tabparc) = sy-tabix.
    lv_datum = wa_saida_02_temp-compdats.
  ELSE.
    lv_datum = sy-datum.
  ENDIF.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_datum
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  MOVE v_ultimo_dia TO v_data.

  MOVE:    e_num_lote                  TO wl_zglt035-lote,
           wa_092-bukrs                TO wl_zglt035-bukrs,
           ''                          TO wl_zglt035-tp_lcto,
           dp_resp                     TO wl_zglt035-dpto_resp,
           'BRL'                       TO wl_zglt035-moeda_doc,
           'LM'                        TO wl_zglt035-blart,
           text-002                    TO wl_zglt035-bktxt,
           v_data                      TO wl_zglt035-budat,
           v_data                      TO wl_zglt035-bldat,
           sy-datum                    TO wl_zglt035-dt_lcto,
           ''                          TO wl_zglt035-prov_est,
           p_mes                       TO wl_zglt035-monat,
           p_ano                       TO wl_zglt035-gjahr,
           sy-uname                    TO wl_zglt035-usnam,
           sy-datum                    TO wl_zglt035-dt_entrada,
           sy-uzeit                    TO wl_zglt035-hr_entrada.


  DATA(li_arr) = wa_092-tp_arrendamento.
  SHIFT li_arr LEFT DELETING LEADING '0'.

  SELECT DISTINCT * FROM zglt092a
    INTO TABLE @DATA(it_zglt092a)
    WHERE cod_contrato EQ @wa_092-cod_contrato
      AND ( tp_arrendamento EQ @wa_092-tp_arrendamento
       OR tp_arrendamento EQ @li_arr ).

  IF sy-subrc IS INITIAL.
    CLEAR lv_tabix.
    LOOP AT it_zglt092a ASSIGNING FIELD-SYMBOL(<f_zglt092a>). " Fornecedor/clientes

      lv_tabix = lv_tabix + 1.
      wl_zglt036-dt_vct = v_ultimo_dia.

      lv_cont = lv_cont + 1.
      IF it_zglt095[] IS NOT INITIAL.
        READ TABLE it_zglt095 INTO wa_zglt095 INDEX 1.
      ENDIF.

      wl_zglt036-seqitem = lv_cont.
      wl_zglt036-tp_lcto = wl_zglt035-tp_lcto.
      wl_zglt036-bschl   = wa_zglt095-chave_lcto.
      wl_zglt036-umskz   = wa_zglt095-razao_especial.

      IF <f_zglt092a>-rcomp IS NOT INITIAL.
        wl_zglt036-vbund   = <f_zglt092a>-rcomp. "RJF
      ENDIF.

      IF <f_zglt092a>-fornecedor IS NOT INITIAL.
        wl_zglt036-hkont = <f_zglt092a>-fornecedor.
      ELSEIF <f_zglt092a>-cliente IS NOT INITIAL.
        wl_zglt036-hkont = <f_zglt092a>-cliente.
      ENDIF.

*        wl_zglt036-dt_vct = v_data.
*      IF wl_zglt036-bschl EQ '29'.
*        wl_zglt036-umskz = '%'.
*      ELSE.
*        wl_zglt036-umskz = '='.
*      ENDIF.
      wl_zglt036-kostl = ' '.
      wl_zglt036-anbwa = ' '.
      wl_zglt036-bewar = ' '.
      wl_zglt036-matnr = ' '. " MATKL?

      wl_zglt036-sgtxt = wa_zglt095-texto_historico.
      wl_zglt036-gsber = wa_092-filial.
      wl_zglt036-zuonr = wa_092-atrib.

      IF wa_saida_02_temp-atua_prec_cp IS NOT INITIAL." AND v_qtdforn IS NOT INITIAL." AND lv_qtdcp IS NOT INITIAL.
        lv_doc = abs( wa_saida_02_temp-atua_prec_cp ).
        wl_zglt036-vlr_moeda_doc = CONV #( lv_doc ).

        lv_int = abs( wa_saida_02_temp-atua_prec_cp ).
        wl_zglt036-vlr_moeda_int = CONV #( lv_int ).


        READ TABLE it_saida_02_dep INTO DATA(wa_saida_02_temp2) INDEX lv_tabparc + 1.
        IF sy-subrc IS INITIAL.
          DATA(lv_dolar) = wa_saida_02_temp2-dolar.
        ENDIF.

        lv_dolar = abs( lv_dolar ).
        IF lv_dolar IS INITIAL.
          lv_ukurs = 1.
        ENDIF.
        lv_for = abs( ( wa_saida_02_temp-atua_prec_cp ) / ( lv_dolar ) ).
        wl_zglt036-vlr_moeda_forte = CONV #( lv_for ).
      ENDIF.

      APPEND wl_zglt036 TO gt_zglt036.
*      CLEAR: wl_zglt036.

      IF wl_zglt036-bschl EQ '29'.
        IF it_zglt095[] IS NOT INITIAL.
          READ TABLE it_zglt095 INTO wa_zglt095 INDEX 2.
          IF sy-subrc IS INITIAL.
            lv_cont = lv_cont + 1.
            wl_zglt036-seqitem = lv_cont.
            wl_zglt036-bschl   = wa_zglt095-chave_lcto.
            wl_zglt036-umskz   = wa_zglt095-razao_especial.
          ENDIF.
        ENDIF.

      ENDIF.

      APPEND wl_zglt036 TO gt_zglt036.
      CLEAR: wl_zglt036.
      CLEAR lv_tabix.
    ENDLOOP.
  ELSE.
    MESSAGE 'Cliente/Fornecedor não cadastrado!' TYPE 'E' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR lv_cont.

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

*--------------------------------------------
  CONCATENATE 'ZGL17' wl_zglt035-doc_lcto p_ano INTO v_objkey.

  SELECT DISTINCT *
    UP TO 1 ROWS
     FROM zib_contabil
     INTO @DATA(wa_zib_contabil)
     WHERE obj_key  = @v_objkey.
  ENDSELECT.

  SELECT DISTINCT *
    UP TO 1 ROWS
     FROM zib_contabil_chv
     INTO @DATA(wa_zib_contabilx)
     WHERE obj_key  = @v_objkey.
  ENDSELECT.

  IF sy-subrc IS INITIAL AND wa_zib_contabilx-belnr IS NOT INITIAL.

    MOVE:    wa_zib_contabilx-belnr TO wa_saida_02_temp-doc_lpcp,
             wl_zglt035-doc_lcto    TO wa_saida_02_temp-doc_lctolpcp,
             v_objkey               TO wa_saida_02_temp-obj_keylpcp,
             e_num_lote             TO wa_saida_02_temp-lt_lpcp,
             abap_false             TO wa_saida_02_temp-est_lpcp.

  ELSE.
    SELECT DISTINCT *
       FROM zib_contabil_err
       INTO TABLE @DATA(it_zib_contabil_err)
       WHERE obj_key  = @v_objkey.

    IF sy-subrc IS NOT INITIAL." AND it_zib_contabil_err[] IS NOT INITIAL.
      wa_saida_02_temp-status = icon_led_yellow.
      MOVE:
      wl_zglt035-doc_lcto    TO wa_saida_02_temp-doc_lctolpcp,
      v_objkey               TO wa_saida_02_temp-obj_keylpcp,
      e_num_lote             TO wa_saida_02_temp-lt_lpcp.
*        abap_false             TO wa_saida_02_temp-est_atua_preco.
    ELSE.
      wa_saida_02_temp-status = icon_led_red.
      MOVE:
      wl_zglt035-doc_lcto    TO wa_saida_02_temp-doc_lctolpcp,
      v_objkey               TO wa_saida_02_temp-obj_keylpcp,
      e_num_lote             TO wa_saida_02_temp-lt_lpcp.
    ENDIF.

  ENDIF.

  IF wa_saida_02_temp-doc_lpcp IS NOT INITIAL.
    wa_saida_02_temp-status = icon_led_green.
  ENDIF.

  MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX lv_tabparc.

  CONCATENATE p_mes '/' p_ano INTO DATA(lv_comp).

  SELECT SINGLE * FROM zglt096e INTO @DATA(w_zglt096e)
                      WHERE cod_contrato EQ @wa_092-cod_contrato AND
                            competencia  EQ @lv_comp AND
                            parckey      EQ @wa_saida_02_temp-parckey.

  IF sy-subrc IS INITIAL.
    CONCATENATE 'ZGL17' wl_zglt035-doc_lcto p_ano INTO v_objkey.
    wa_096e = CORRESPONDING #( wa_saida_02_temp ).
    wa_096e-cod_contrato = wa_092-cod_contrato.
    wa_096e-parcelat = lg_pt.
    wa_096e-dt_atualizacao = sy-datum.
    wa_096e-obj_keylpcp  = v_objkey.
    wa_096e-ctime = sy-uzeit.
    wa_096e-cmuser = sy-uname.
    MODIFY zglt096e FROM wa_096e.
    COMMIT WORK.

  ELSE.
    wa_096e = CORRESPONDING #( wa_saida_02_temp ).
    wa_096e-cod_contrato = wa_092-cod_contrato.
    wa_096e-parcelat = lg_pt.
    wa_096e-dt_atualizacao = sy-datum.
    wa_096e-obj_keylpcp  = v_objkey.
*      READ TABLE it_096e INTO DATA(wa_096exx) WITH KEY cod_contrato = wa_092-cod_contrato.
*      IF sy-subrc IS INITIAL.
*        wa_096e-obj_key  = wa_096exx-obj_key.
*      ENDIF.
*      IF wa_096e-obj_keyd IS NOT INITIAL.
*        APPEND VALUE #( fieldname = 'STATUS' color-col = 4 color-int = 1 color-inv = 0 ) TO wa_saida_02_temp-tcolor.
*      ENDIF.

*    ENDIF.
    wa_096e-ctime = sy-uzeit.
    wa_096e-cmuser = sy-uname.
    MODIFY zglt096e FROM wa_096e.
    COMMIT WORK.

  ENDIF.
  CLEAR: wa_096e, wa_saida_02_temp.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR_DOCUMENTO_LPCP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estornar_documento_lpcp .

  DATA: var_answer TYPE c.

  DATA: it_msg         TYPE TABLE OF bdcmsgcoll, " WITH HEADER LINE,
        lv_competencia TYPE char7,
        v_ultimo_dia   TYPE sy-datum,
        lv_text        TYPE string,
        v_ultimo_diac  TYPE char10,
        wa_msg         TYPE bdcmsgcoll.

  CONCATENATE p_mes '/' p_ano INTO lv_competencia.

  READ TABLE it_saida_02_dep INTO wa_saida_02_temp WITH KEY competencia = lv_competencia
                                                            tipo = 'V'.
  DATA(lv_tabix) = sy-tabix.
  IF wa_saida_02_temp-doc_lpcp IS INITIAL.
    MESSAGE 'Não existe documento a ser estornado!' TYPE 'E'.
    EXIT.
  ENDIF.

  CHECK wa_saida_02_temp-doc_lpcp IS NOT INITIAL.

  FREE: it_dta.
  DEFINE shdb.
    CLEAR IT_DTA.
    WA_DTA-PROGRAM   = &1.
    WA_DTA-DYNPRO    = &2.
    WA_DTA-DYNBEGIN  = &3.
    WA_DTA-FNAM      = &4.
    WA_DTA-FVAL      = &5.
    APPEND WA_DTA TO IT_DTA.
  END-OF-DEFINITION.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = wa_saida_02_temp-compdats
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  CONCATENATE v_ultimo_dia+6(2) '.' v_ultimo_dia+4(2) '.' v_ultimo_dia(4) INTO v_ultimo_diac.
*  v_ultimo_diac = v_ultimo_dia.

  shdb:
  'SAPMF05A' '0105' 'X'  ' '           ' ',
  ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
  ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
  ' '        ' '    ' '  'RF05A-BELNS' wa_saida_02_temp-doc_lpcp,
  ' '        ' '    ' '  'BKPF-BUKRS'  wa_092-bukrs,
  ' '        ' '    ' '  'RF05A-GJAHS' lv_competencia+3(4),
  ' '        ' '    ' '  'BSIS-BUDAT' v_ultimo_diac,
  ' '        ' '    ' '  'UF05A-STGRD' '01'.

  opt-dismode = 'N'.
  CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.
**
  IF sy-subrc IS INITIAL.

*Vai na tabela BKPFcom - BUKRS = empresa processamento - BELNR = em processamento
*selecionar campo BKPF-STBLG
    SELECT stblg FROM bkpf
      UP TO 1 ROWS
      INTO @DATA(lv_stblg)
      WHERE bukrs EQ @wa_092-bukrs
        AND belnr EQ @wa_saida_02_temp-doc_lpcp.
    ENDSELECT.

    IF sy-subrc IS INITIAL AND lv_stblg IS NOT INITIAL.
*    MOVE:    wa_saida_02_temp-doc_atua_preco TO wa_saida_02_temp-est_atua_preco,
      MOVE:    lv_stblg TO wa_saida_02_temp-est_lpcp,
               abap_false                  TO wa_saida_02_temp-doc_lpcp.
*      MODIFY it_saida_02_dep FROM wa_saida_02_temp index lv_tabix.
    ELSE.
      MOVE:    abap_false  TO wa_saida_02_temp-est_lpcp,
*               abap_false  TO wa_saida_02_temp-doc_depreciacao.

               icon_led_red TO wa_saida_02_temp-status.
    ENDIF.


    MODIFY it_saida_02_dep FROM wa_saida_02_temp INDEX lv_tabix.

    wa_096e = CORRESPONDING #( wa_saida_02_temp ).
    wa_096e-cod_contrato = wa_092-cod_contrato.
    wa_096e-parcelat = lg_pt.
    wa_096e-dt_atualizacao = sy-datum.
    wa_096e-cmuser = sy-uname.
    MODIFY zglt096e FROM wa_096e.
    COMMIT WORK AND WAIT.
    CLEAR: wa_096e, wa_saida_02_temp.

  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
