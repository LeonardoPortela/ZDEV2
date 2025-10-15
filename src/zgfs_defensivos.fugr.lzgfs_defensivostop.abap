FUNCTION-POOL zgfs_defensivos.              "MESSAGE-ID ..

*******************************************************************************************
* includes
*******************************************************************************************
INCLUDE <icon>.

*******************************************************************************************
* classes / btree
*******************************************************************************************
CLASS cl_gui_column_tree     DEFINITION LOAD.
CLASS cl_gui_cfw             DEFINITION LOAD.

DATA: tree1               TYPE REF TO cl_hrpayna_gui_alv_tree, "cl_gui_alv_tree.
      mr_toolbar          TYPE REF TO cl_gui_toolbar,
      g_container         TYPE scrfname VALUE 'CONTAINER1',
      g_container2        TYPE scrfname VALUE 'CONTAINER2',
      g_container3        TYPE scrfname VALUE 'HEADER',
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      g_custom_container2 TYPE REF TO cl_gui_custom_container,
      g_grid              TYPE REF TO cl_gui_alv_grid,
      g_grid2             TYPE REF TO cl_gui_alv_grid,
      g_grid3             TYPE REF TO cl_gui_alv_grid,
      w_tool              TYPE stb_button.

*******************************************************************************************
* Types
*******************************************************************************************
TYPES: BEGIN OF ty_ordem_5820,
         nro_cgd     TYPE zsdt0139-nro_cgd,
         inco1       TYPE zsdt0140-inco1,
         matnr       TYPE makt-matnr,
         maktx       TYPE makt-maktx,
         qtd_vinc_lt TYPE zsdt0131-qtd_vinc,
         meins       TYPE vbap-meins,
         qtd_emkg    TYPE zsdt0131-qtd_emkg,
         cor(4)      TYPE c,
         lock        TYPE char1.  "*-CS2021000218-14.10.2022-#91701-JT-inicio
         INCLUDE STRUCTURE zsdt0082.
TYPES: END OF ty_ordem_5820.

TYPES: BEGIN OF ty_carga_5820,
         icone     TYPE char6,
         name1     TYPE lfa1-name1,
         name2     TYPE lfa1-name1,
         name4     TYPE lfa1-name1,
         nome_rtc  TYPE zsdt0259-nome,
         tipo_rtc2 TYPE char20, "*-CS2021000218-31.08.2022-#89492-JT-inicio
         cor(4)    TYPE c.
         INCLUDE STRUCTURE zsdt0139.
TYPES: END OF ty_carga_5820.

TYPES: BEGIN OF ty_lotes,
         charg     TYPE mseg-charg,
         ebeln     TYPE ekbe-ebeln,
         lgort     TYPE mseg-lgort,
         lifnr     TYPE mseg-lifnr,
         werks     TYPE mseg-werks,
         matnr     TYPE mseg-matnr,
         vfdat     TYPE mch1-vfdat,
         clabs     TYPE mchb-clabs,
         clabs_ori TYPE mchb-clabs,
         lfimg     TYPE zsdt0134-lfimg,
         lfimg_ori TYPE zsdt0134-lfimg,
         edit      TYPE char1,
         bloq_data TYPE char1,
         vbeln     TYPE zsdt0134-vbeln,
         posnr     TYPE zsdt0134-posnr,
         nro_cg    TYPE zsdt0134-nro_cg,
         gr_lot    TYPE char1,
         color     TYPE char4.
TYPES: END OF ty_lotes.

TYPES: BEGIN OF ty_hist,
         charg       TYPE mseg-charg,
         lifnr       TYPE zsdt0134-lifnr,
         nro_cg      TYPE zsdt0134-nro_cg,
         nr_romaneio TYPE zsdt0001-nr_romaneio,
         lfimg       TYPE zsdt0134-lfimg,
         doc_rem     TYPE zsdt0001-doc_rem,
         nfenum      TYPE j_1bnfdoc-nfenum,
         data_atual  TYPE zsdt0134-data_atual.
TYPES: END OF ty_hist.

TYPES: BEGIN OF ty_nlote_5820,
         edit       TYPE char1.
         INCLUDE STRUCTURE zsdt0134.
TYPES: cellstyles TYPE lvc_t_styl.
TYPES: END OF ty_nlote_5820.

*******************************************************************************************
* variaveis
*******************************************************************************************
DATA: zcl_romaneio           TYPE REF TO zcl_romaneio,
      zcx_cadastro           TYPE REF TO zcx_cadastro,
      vg_cg_para_rom         TYPE zsdt0133-nro_cg,
      it_carga_5820          TYPE TABLE OF ty_carga_5820,
      vg_tela_5821           TYPE char1,
*
      w_ordem_carga          TYPE zsdt0304,
      t_zsdt0134             TYPE TABLE OF zsdt0134,
      w_zsdt0134             TYPE zsdt0134,
      t_zsdt0001             TYPE TABLE OF zsdt0001,
      w_zsdt0001             TYPE zsdt0001,
*
      ok_code                TYPE sy-ucomm,
      g_quant_carga          TYPE char50,
      t_lotes                TYPE TABLE OF ty_lotes,
      t_lotes_tot            TYPE TABLE OF ty_lotes,
      t_lotes_aux            TYPE TABLE OF ty_lotes,
      t_lotes_gr             TYPE TABLE OF ty_lotes,
      t_lotes_saldo_date     TYPE TABLE OF ty_lotes,
      t_lotes_ori            TYPE TABLE OF ty_lotes,
      t_hist                 TYPE TABLE OF ty_hist,
      w_lotes                TYPE ty_lotes,
      w_lotes_gr             TYPE ty_lotes,
      w_lotes_tot            TYPE ty_lotes,
      w_hist                 TYPE ty_hist,
      w_retorno              TYPE zsdt0134,
      l_cont                 TYPE i,
      it_zsdt0062            TYPE TABLE OF zsdt0062,
      it_mchb                TYPE TABLE OF mchb,
      it_mch1                TYPE TABLE OF mch1,
      it_mslb                TYPE TABLE OF mslb,
      wa_0140                TYPE zsdt0140,
      wa_0134                TYPE zsdt0134,
      wa_mch1                TYPE mch1,
      l_qtd_soma             TYPE brgew,
      l_erro                 TYPE char1,
      gv_msg                 TYPE char50,
      gv_msg1                TYPE char50,
      gv_charg               TYPE mseg-charg,
      l_qtd_dias_vencimento  TYPE zsdt0301-qtd_dias_vencimento,
      l_bloq_lotes_vencidos  TYPE zsdt0301-bloq_lotes_vencidos,
      l_lote_vencmto_proximo TYPE zsdt0301-lote_vencmto_proximo,
      l_regra_vencto         TYPE char1,
      l_data_vencto          TYPE datum,
      l_data_prox            TYPE datum,
*
      wl_zsdt0001            TYPE zsdt0001,
      wa_ret                 TYPE ddshretval,
      wa_modi                TYPE lvc_s_modi,
      wa_mchb                TYPE mchb,
      wa_mslb                TYPE mslb,
*
      l_edit                 TYPE char1,
      l_row                  TYPE lvc_s_row,
      l_col                  TYPE lvc_s_col,
      t_fieldcatalog         TYPE lvc_t_fcat, "Fieldcatalog
      t_fieldcatalog2        TYPE lvc_t_fcat, "Fieldcatalog
      t_exctab               TYPE slis_t_extab,
      w_item_layout          TYPE lvc_s_laci,
      w_layout               TYPE lvc_s_layo,
      ls_fieldcatalog        TYPE lvc_s_fcat,
      obj_dyndoc_id          TYPE REF TO cl_dd_document,
      cl_container_95        TYPE REF TO cl_gui_docking_container,
      t_estilo               TYPE lvc_t_styl,
      ls_exclude             TYPE ui_func,
      pt_exclude             TYPE ui_functions,
      pt_exclude2            TYPE ui_functions,
      pt_extab               TYPE slis_t_extab,
      t_del_rows             TYPE lvc_t_row,
      w_del_rows             TYPE lvc_s_row,
      t_sel_cols             TYPE lvc_t_col,
      w_sel_cols             TYPE lvc_s_col,
      l_row_id               TYPE lvc_s_row,
      l_column_id            TYPE lvc_s_col,
      l_stable               TYPE lvc_s_stbl,
      l_tabix                TYPE sy-tabix,
*
      t_sel_rows             TYPE lvc_t_row,
      w_sel_rows             TYPE lvc_s_row,
      gv_qte_lib             TYPE zsdt0304-qte_lib,
      gv_qte_libc            TYPE zsdt0304-qte_lib,
      gv_valid               type zsdt0304-qte_lib,
      ls_sort                TYPE lvc_s_sort,
      lt_sort1               TYPE lvc_t_sort,
      lt_sort2               TYPE lvc_t_sort,
      t_fcat_lvc             TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE,
      t_fcat_kkb             TYPE kkblo_t_fieldcat,
      w_stable               TYPE lvc_s_stbl VALUE 'XX'.

*******************************************************************************************
*variaveis tela romaneios
*******************************************************************************************
TYPES: BEGIN OF ty_romaneios_5821,
         icone              TYPE char6,
         qtd_solicitacao_ra TYPE numc3,
         log_solrec         TYPE char6,
         color              TYPE char4.
         INCLUDE STRUCTURE zsdt0001.
TYPES: END OF ty_romaneios_5821.

DATA: g_custom_container_pop_5821 TYPE REF TO cl_gui_custom_container,
      ctl_alv1_pop_5821           TYPE REF TO cl_gui_alv_grid,
      gs_layout_pop_5821          TYPE lvc_s_layo,
      it_fieldcatalog_pop_5821    TYPE lvc_t_fcat.

DATA: it_zsdt0001_5821 TYPE TABLE OF ty_romaneios_5821,
      it_zsdt0134_5821 TYPE TABLE OF zsdt0134,
      t_zsdt0302       TYPE TABLE OF zsdt0302,
      t_zsdt0298       TYPE TABLE OF zsdt0298,
      w_zsdt0302       TYPE zsdt0302,
      w_zsdt0298       TYPE zsdt0298,
      l_excluiu_rom    TYPE char1.

DATA: rg_vfdat TYPE RANGE OF mch1-vfdat.

*******************************************************************************************
* Classe
*******************************************************************************************
CLASS lcl_event_handler_5821 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      toolbar_5821 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5821 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      on_hotspot     FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*******************************************************************************************
* classes / implementacao
*******************************************************************************************
CLASS lcl_event DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event2 DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed4 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.

    CLASS-METHODS:
      handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid.

ENDCLASS.

CLASS lcl_event_handler2 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed4 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.

ENDCLASS.

*----------------------------------------------------------------------*
*   INCLUDE BCALV_TOOLBAR_EVENT_RECEIVER                               *
*----------------------------------------------------------------------*

CLASS lcl_toolbar_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS: on_function_selected
      FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING fcode,

      on_toolbar_dropdown
        FOR EVENT dropdown_clicked OF cl_gui_toolbar
        IMPORTING fcode
                  posx
                  posy.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_toolbar_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_toolbar_event_receiver IMPLEMENTATION.

  METHOD on_function_selected.

  ENDMETHOD.

  METHOD on_toolbar_dropdown.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.

    LOOP AT t_lotes INTO w_lotes.
      CLEAR w_lotes-color.
      MODIFY t_lotes FROM w_lotes INDEX sy-tabix TRANSPORTING color.
    ENDLOOP.

    READ TABLE t_lotes INTO w_lotes INDEX e_row-index.

    PERFORM f_historico USING w_lotes-charg.

    w_lotes-color = 'C500'.
    MODIFY t_lotes FROM w_lotes INDEX e_row-index TRANSPORTING color.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

    CALL METHOD g_grid2->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD handle_top_of_page.
  ENDMETHOD.

  METHOD on_data_changed4.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          l_lfimg  TYPE zsdt0134-lfimg.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      CASE ls_good-fieldname.
        WHEN 'LFIMG'.
          READ TABLE t_lotes INTO w_lotes INDEX ls_good-row_id.

          lv_value          = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_lfimg           = lv_value.

*         READ TABLE t_lotes INTO w_lotes INDEX ls_good-row_id.

          w_lotes-lfimg     = l_lfimg.
          w_lotes-edit      = abap_true.
          MODIFY t_lotes FROM w_lotes INDEX ls_good-row_id.

*------------------------
*-------- analisa vencimentos proximos
*------------------------
          PERFORM f_analisa_vencimento.

          READ TABLE t_lotes INTO w_lotes INDEX ls_good-row_id.

          IF l_lfimg > w_lotes-clabs_ori AND
             l_lfimg IS NOT INITIAL.
            MESSAGE s024(sd) WITH 'Qtd Vinculada maior que utilização livre!' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          IF w_lotes-bloq_data = abap_true AND
             l_lfimg IS NOT INITIAL.
            LOOP AT t_lotes INTO DATA(w_lote_data) WHERE bloq_data = abap_off
                                                     AND lfimg     = 0.
              IF w_lotes-lifnr IS     INITIAL AND w_lote_data-lifnr IS     INITIAL.
                EXIT.
              ENDIF.
              IF w_lotes-lifnr IS NOT INITIAL AND w_lote_data-lifnr IS NOT INITIAL.
                EXIT.
              ENDIF.
            ENDLOOP.

            IF l_erro IS INITIAL.
*              MESSAGE s024(sd) WITH 'Deve ser vinculado o Saldo total do Lote '  w_lote_data-charg
              MESSAGE s024(sd) WITH 'Deve ser vinculado o saldo da carga ou a qtd total, do lote'(e05) w_lote_data-charg
                                    ' pois ele está com vencimento mais próximo.'  DISPLAY LIKE 'E'.
            ENDIF.
            EXIT.
          ENDIF.

*         w_lotes-clabs     = w_lotes-clabs_ori - ( l_lfimg - w_lotes-lfimg_ori ).
          w_lotes-clabs     = w_lotes-clabs_ori -   l_lfimg.
          w_lotes-lfimg     = l_lfimg.
          w_lotes-edit      = abap_true.
          MODIFY t_lotes FROM w_lotes INDEX ls_good-row_id.

          lv_value = w_lotes-clabs.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'CLABS'
              i_value     = lv_value.

      ENDCASE.
    ENDLOOP.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD handle_on_f1.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_handler2 IMPLEMENTATION.

  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_data_changed4.
  ENDMETHOD.

  METHOD handle_on_f1.
  ENDMETHOD.

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event IMPLEMENTATION.

  METHOD toolbar.
  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event2 IMPLEMENTATION.

  METHOD toolbar.
  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    CALL METHOD g_grid2->refresh_table_display.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5821 IMPLEMENTATION.

  METHOD toolbar_5821.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ESTORNAROM'.
    wa_tool-icon     = '@VI@'.
    wa_tool-quickinfo = 'Estornar Todos Romaneios'.
*   wa_tool-text      = 'Estornar Romaneios'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-butn_type = 3.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'GERAR_SRECEITA'.
    wa_tool-icon     = '@15@'.
    wa_tool-quickinfo = 'Gerar Solicitação Receita'.
*   wa_tool-text      = 'Gerar Sol.Receita'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'CANCEL_SRECEITA'.
    wa_tool-icon     = '@2O@'.
    wa_tool-quickinfo = 'Cancelar Solicitação Receita'.
*   wa_tool-text      = 'Cancelar Sol.Receita'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD user_command_5821.

    DATA: wa_zsdt0001   TYPE ty_romaneios_5821,
          wa_zsdt0139   TYPE zsdt0139,
          wa_zsdt0134   TYPE zsdt0134,
          wa_carga_5820 TYPE ty_carga_5820,
          vl_check      TYPE char1.

    FREE: l_erro.

    CASE e_ucomm.

      WHEN 'GERAR_SRECEITA'.
        CALL METHOD ctl_alv1_pop_5821->get_selected_rows
          IMPORTING
            et_index_rows = t_sel_rows.

        DESCRIBE TABLE t_sel_rows LINES DATA(l_lines).

        IF l_lines = 0 OR l_lines > 1.
          MESSAGE TEXT-022 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          READ TABLE t_sel_rows       INTO w_sel_rows  INDEX 1.
          READ TABLE it_zsdt0001_5821 INTO wa_zsdt0001 INDEX w_sel_rows-index.

          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = 50
              text       = 'Gerando Receituário Agronômico...'.

*----------------------------------
*-------- gerar solicitacao receita
*----------------------------------
          TRY.
              zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
                 )->set_gerar_sol_ra( EXPORTING i_nro_cgd       = wa_zsdt0001-nro_cg
                                                i_ch_referencia = wa_zsdt0001-ch_referencia
                                                i_exibe_popup   = abap_true ).

            CATCH zcx_integracao INTO DATA(ex_integra).
              l_erro = abap_true.
              ex_integra->zif_error~published_erro( i_msgty = 'S' ).
            CATCH zcx_error INTO DATA(ex_error).
              l_erro = abap_true.
              ex_error->zif_error~published_erro(   i_msgty = 'S' ).
          ENDTRY.

          IF l_erro = abap_false.
            MESSAGE s024(sd) WITH TEXT-100.
          ENDIF.

          PERFORM f_selecao_romaneios USING wa_zsdt0001-nro_cg.
        ENDIF.

      WHEN 'CANCEL_SRECEITA'.
        CALL METHOD ctl_alv1_pop_5821->get_selected_rows
          IMPORTING
            et_index_rows = t_sel_rows.

        DESCRIBE TABLE t_sel_rows LINES l_lines.

        IF l_lines = 0 OR l_lines > 1.
          MESSAGE TEXT-022 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          READ TABLE t_sel_rows       INTO w_sel_rows  INDEX 1.
          READ TABLE it_zsdt0001_5821 INTO wa_zsdt0001 INDEX w_sel_rows-index.

          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = 50
              text       = 'Cancelando Receituário Agronômico...'.

*----------------------------------
*-------- cancelar solicitacao receita
*----------------------------------
          TRY.
              zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
                 )->set_cancelar_sol_ra( EXPORTING i_nro_cgd       = wa_zsdt0001-nro_cg
                                                   i_ch_referencia = wa_zsdt0001-ch_referencia ).
*                                                  i_elimina_reg   = abap_true ).

            CATCH zcx_integracao INTO ex_integra.
              l_erro = abap_true.
              ex_integra->zif_error~published_erro( i_msgty = 'S' ).
            CATCH zcx_error      INTO ex_error.
              l_erro = abap_true.
              ex_error->zif_error~published_erro(   i_msgty = 'S' ).
          ENDTRY.

          IF l_erro = abap_false.
            MESSAGE s024(sd) WITH TEXT-101.
          ENDIF.

          PERFORM f_selecao_romaneios USING wa_zsdt0001-nro_cg.
        ENDIF.

      WHEN 'ESTORNAROM'.
        LOOP AT it_zsdt0001_5821 INTO wa_zsdt0001.
          IF wa_zsdt0001-doc_rem IS NOT INITIAL.
            vl_check = abap_true.
          ENDIF.
        ENDLOOP.

        IF sy-subrc = 0.
          IF vl_check IS INITIAL.
            CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
              EXPORTING
                percentage = 50
                text       = 'Cancelando Receituário Agronômico...'.

*----------------------------------
*---------- cancelar solicitacao receita
*----------------------------------
            CLEAR l_erro.
            LOOP AT it_zsdt0001_5821 INTO wa_zsdt0001.

*             SELECT id
*               INTO @DATA(l_id)
*               FROM zsdt0298
*                 UP TO 1 ROWS
*              WHERE nro_cgd       = @wa_zsdt0001-nro_cg
*                AND ch_referencia = @wa_zsdt0001-ch_referencia
*                AND cancelado     = @abap_false.
*             ENDSELECT.

*             CHECK sy-subrc = 0.

              TRY.
                  zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
                     )->set_cancelar_sol_ra( EXPORTING i_nro_cgd       = wa_zsdt0001-nro_cg
                                                       i_ch_referencia = wa_zsdt0001-ch_referencia ).

                CATCH zcx_integracao INTO ex_integra.
                  l_erro = COND #( WHEN ex_integra->zif_error~msgty = 'E' THEN abap_true
                                                                          ELSE abap_off ).
                  ex_integra->zif_error~published_erro( i_msgty = 'S' ).
                CATCH zcx_error      INTO ex_error.
                  l_erro = COND #( WHEN ex_error->zif_error~msgty   = 'E' THEN abap_true
                                                                          ELSE abap_off ).
                  ex_error->zif_error~published_erro(   i_msgty = 'S' ).
              ENDTRY.

              IF l_erro = abap_false.
                MESSAGE s024(sd) WITH TEXT-101.
              ENDIF.
            ENDLOOP.

            IF l_erro = abap_false.
              CREATE OBJECT zcl_romaneio.

              LOOP AT it_zsdt0001_5821 INTO wa_zsdt0001.
                TRY.
                    CALL METHOD zcl_romaneio->set_registro
                      EXPORTING
                        i_id_registro = wa_zsdt0001-ch_referencia.
                  CATCH zcx_cadastro INTO zcx_cadastro.
                    zcx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
                ENDTRY.

                CALL METHOD zcl_romaneio->excluir_registro.
              ENDLOOP.

              LOOP AT it_zsdt0134_5821 INTO wa_zsdt0134.
                CLEAR: wa_zsdt0134-ch_referencia.
                MODIFY zsdt0134 FROM wa_zsdt0134.
              ENDLOOP.

              SELECT SINGLE *
                FROM zsdt0139
                INTO wa_zsdt0139
               WHERE nro_cgd EQ vg_cg_para_rom.

              wa_zsdt0139-status = wa_zsdt0139-status - 1.
              MODIFY zsdt0139 FROM wa_zsdt0139 .

              l_excluiu_rom      = abap_true.

              MESSAGE TEXT-059 TYPE 'S'.
            ENDIF.
          ELSE.
            MESSAGE TEXT-060 TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.
          MESSAGE TEXT-061 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

        CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
          EXPORTING
            chave = vg_cg_para_rom.

        PERFORM f_selecao_romaneios USING vg_cg_para_rom.

*       CALL METHOD g_custom_container_pop_5821->free.
*       CALL METHOD g_custom_container2->free.
*       LEAVE TO SCREEN 0.

    ENDCASE.

    CALL METHOD ctl_alv1_pop_5821->refresh_table_display
      EXPORTING
        is_stable = w_stable.

    CALL METHOD g_grid2->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.                    "USER_COMMAND

  METHOD on_double_click.

    DATA: wa_zsdt0001 TYPE ty_romaneios_5821.

    LOOP AT it_zsdt0001_5821 INTO wa_zsdt0001.
      CLEAR wa_zsdt0001-color.
      MODIFY it_zsdt0001_5821 FROM wa_zsdt0001 INDEX sy-tabix TRANSPORTING color.
    ENDLOOP.

    READ TABLE it_zsdt0001_5821 INTO wa_zsdt0001 INDEX e_row-index.

    PERFORM f_lista_receitas USING wa_zsdt0001-nro_cg
                                   wa_zsdt0001-ch_referencia.

    wa_zsdt0001-color = 'C500'.
    MODIFY it_zsdt0001_5821 FROM wa_zsdt0001 INDEX e_row-index TRANSPORTING color.

    CALL METHOD ctl_alv1_pop_5821->refresh_table_display
      EXPORTING
        is_stable = w_stable.

    CALL METHOD g_grid2->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD on_hotspot.

    DATA: wa_zsdt0001 TYPE ty_romaneios_5821,
          r_sidinf    TYPE RANGE OF zde_id_integracao,
          r_sidref    TYPE RANGE OF zde_id_referencia,
          r_sdtreg    TYPE RANGE OF zde_dt_registro.

    READ TABLE it_zsdt0001_5821 INTO wa_zsdt0001 INDEX e_row_id-index.

    CASE e_column_id.

      WHEN 'LOG_SOLREC'.
        CLEAR w_zsdt0302.
        READ TABLE t_zsdt0302 INTO w_zsdt0302 WITH KEY nro_cgd       = wa_zsdt0001-nro_cg
                                                       ch_referencia = wa_zsdt0001-ch_referencia.
        IF sy-subrc = 0.
          r_sidinf = VALUE #( sign = 'I' option = 'EQ' ( low = '079'                     high = '079' ) ).
          r_sidref = VALUE #( sign = 'I' option = 'EQ' ( low = wa_zsdt0001-ch_referencia high = wa_zsdt0001-ch_referencia ) ).
          r_sdtreg = VALUE #( sign = 'I' option = 'EQ' ( low = '19000101'                high = '99991231' ) ).

*--------------------
*-------- Monitor Integracao
*--------------------
          SUBMIT zintegracao
            WITH sidinf IN r_sidinf
            WITH sidref IN r_sidref
            WITH sdtreg IN r_sdtreg
             AND RETURN.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*******************************************************************************************
* classes / btree
*******************************************************************************************
DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver,
      m_event_handler        TYPE REF TO lcl_event,
      m_event_handler2       TYPE REF TO lcl_event2.

*******************************************************************************************
*******************************************************************************************
