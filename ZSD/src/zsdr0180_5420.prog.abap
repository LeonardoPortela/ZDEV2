*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5420
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_sol_5420,
         bezei  TYPE tvkbt-bezei,
         name1  TYPE kna1-name1,
         matnr  TYPE makt-matnr,
         maktx  TYPE makt-maktx,
         kunnr  TYPE kna1-kunnr,
         qtdvc  TYPE zsdt0082-qte_lib,
         qtdeb  TYPE zsdt0082-qte_lib,
         meins  TYPE vbap-meins,
         inco1  TYPE vbkd-inco1,
         cor(4) TYPE c,
         editar TYPE char6.
         INCLUDE STRUCTURE zsdt0082.
TYPES: END OF ty_sol_5420.

TYPES: BEGIN OF ty_frete_5420,
         edit        TYPE char1,
         nro_sol     TYPE zsdt0082-nro_sol,
         seq         TYPE zsdt0082-seq,
         matnr       TYPE makt-matnr,
         filial_resp TYPE zsdt0137-filial_resp,
         bezei       TYPE tvkbt-bezei,
         qtd_vinc    TYPE zsdt0137-qtd_vinc,
         um          TYPE vbap-meins,
         inco1       TYPE vbkd-inco1,
         icone       TYPE char6,
         cor(4)      TYPE c.
TYPES: cellstyles TYPE lvc_t_styl.
TYPES: END OF ty_frete_5420.

TYPES: BEGIN OF ty_caminhao_5420,
         edit        TYPE char1,
         icone       TYPE char6,
         name1       TYPE lfa1-name1,
         name2       TYPE lfa1-name1,
         dpto_col    TYPE lfa1-name1,
         dpto_ent    TYPE kna1-name1,
         demiss_cte  TYPE lfa1-name1,
         nr_romaneio TYPE zsdt0001-nr_romaneio,

         cor(4)      TYPE c.
         INCLUDE    STRUCTURE zsdt0138.
TYPES:   cellstyles  TYPE lvc_t_styl.
TYPES: END OF ty_caminhao_5420.

*TYPES:
*  BEGIN OF ty_popup_vinculacao,
*    material TYPE mara-matnr,
*    pedido   TYPE ekpo-ebeln,
*    "IS_INVALID TYPE ABAP_BOOL,
*  END OF ty_popup_vinculacao.

DATA: it_sol_5420          TYPE STANDARD TABLE OF ty_sol_5420,
      it_frete_5420        TYPE STANDARD TABLE OF ty_frete_5420,
      it_caminhao_5420     TYPE STANDARD TABLE OF ty_caminhao_5420,
      it_caminhao_aux_5420 TYPE STANDARD TABLE OF ty_caminhao_5420.

DATA: g_editar       TYPE char1,
      g_click_editar TYPE char1,
      l_disabled     TYPE char1,
      l_tabix        TYPE sy-tabix,
      l_gname        TYPE seqg3-gname,
      l_garg         TYPE seqg3-garg,
      it_enq         TYPE TABLE OF seqg3.

DATA: g_custom_container_5420       TYPE REF TO cl_gui_custom_container,
      dg_splitter_1_5420            TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2_5420            TYPE REF TO cl_gui_splitter_container,
      dg_parent_1_5420              TYPE REF TO cl_gui_container,
      dg_parent_2_5420              TYPE REF TO cl_gui_container,
      dg_parent_3_5420              TYPE REF TO cl_gui_container,
      dg_parent_4_5420              TYPE REF TO cl_gui_container,
      ctl_alv1_5420                 TYPE REF TO cl_gui_alv_grid,
      ctl_alv2_5420                 TYPE REF TO cl_gui_alv_grid,
      ctl_alv3_5420                 TYPE REF TO cl_gui_alv_grid,
      gs_layout_5420_alv1           TYPE lvc_s_layo,
      gs_layout_5420_alv2           TYPE lvc_s_layo,
      gs_layout_5420_alv3           TYPE lvc_s_layo,
      it_fieldcatalog_sol_5420      TYPE lvc_t_fcat,
      it_fieldcatalog_frete_5420    TYPE lvc_t_fcat,
      it_fieldcatalog_caminhao_5420 TYPE lvc_t_fcat,
      it_exclude_5420               TYPE ui_functions,
      it_exclude_frete_5420         TYPE ui_functions,
      it_sort_sol_5420              TYPE lvc_t_sort,
      it_sort_frete_5420            TYPE lvc_t_sort,
      it_sort_caminhao_5420         TYPE lvc_t_sort.
DATA: wa_sol_5420      TYPE ty_sol_5420.

*DATA popup_vinculacao TYPE ty_popup_vinculacao.

DATA: vg_sol_5420   TYPE ty_sol_5420,
      vg_frete_5420 TYPE ty_frete_5420.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5420 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click_5420 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      data_changed_finished_5420 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      on_double_click_frete_5420 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      toolbar_5420 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      toolbar_frete_5420 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      toolbar_caminhao_5420 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5420 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_f4_5420 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display,

      on_f4_pedido_5420 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display,

*-----CS2019001891 - 08.03.2021 - JT - inicio
      handle_hotspot_click_5420 FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.
*-----CS2019001891 - 08.03.2021 - JT - fim

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5420 IMPLEMENTATION.

  METHOD on_double_click_5420.

*-CS2019001896 - 12.08.2021 - JT - inicio
    IF g_click_editar = abap_true.
      g_editar       = abap_true.
      g_click_editar = abap_false.
    ELSE.
      g_editar       = abap_false.
    ENDIF.
*-CS2019001896 - 12.08.2021 - JT - fim

    index_click = e_row-index.
    PERFORM click_em_solic USING e_row-index.
    PERFORM bloqueia_linhas_frete_5420.

    PERFORM refresh_ctl_alv1_5420.
    PERFORM refresh_ctl_alv2_5420.
    PERFORM refresh_ctl_alv3_5420.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD data_changed_finished_5420.

    DATA: wa_good_cells    TYPE lvc_s_modi,
          wa_caminhao_5420 TYPE ty_caminhao_5420,
          wa_lfa1          TYPE lfa1,
          wa_zlest0002     TYPE zlest0002,
          vl_renavam       TYPE zlest0002-cd_renavam.

    LOOP AT et_good_cells INTO wa_good_cells.
      IF e_modified EQ abap_true.
        IF wa_good_cells-fieldname EQ 'COD_TRANSPORTADORA'.

          READ TABLE it_caminhao_5420 INTO wa_caminhao_5420 INDEX wa_good_cells-row_id.

          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
            WHERE lifnr EQ wa_caminhao_5420-cod_transportadora.

          IF sy-subrc IS INITIAL.
            wa_caminhao_5420-name1 = wa_lfa1-name1.
            MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX wa_good_cells-row_id.
          ENDIF.

          PERFORM refresh_ctl_alv3_5420.

        ELSEIF wa_good_cells-fieldname EQ 'MOTORISTA'.

          READ TABLE it_caminhao_5420 INTO wa_caminhao_5420 INDEX wa_good_cells-row_id.

          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
            WHERE lifnr EQ wa_caminhao_5420-motorista.

          IF sy-subrc IS INITIAL.
            wa_caminhao_5420-name2 = wa_lfa1-name1.
            MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX wa_good_cells-row_id.
          ENDIF.

          PERFORM refresh_ctl_alv3_5420.

        ENDIF.

        IF ( vg_sol_5420-inco1 NE 'CPT' )  AND ( vg_sol_5420-inco1 NE 'CFR' ).

          IF wa_good_cells-fieldname EQ 'PLACA_CAV'.

            READ TABLE it_caminhao_5420 INTO wa_caminhao_5420 INDEX wa_good_cells-row_id.

            IF wa_caminhao_5420 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '0'
                                              CHANGING wa_caminhao_5420-placa_cav
                                                       vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX wa_good_cells-row_id.
                PERFORM refresh_ctl_alv3_5420.
              ENDIF.
            ENDIF.

          ELSEIF wa_good_cells-fieldname EQ 'PLACA_CAR1'.

            READ TABLE it_caminhao_5420 INTO wa_caminhao_5420 INDEX wa_good_cells-row_id.

            IF wa_caminhao_5420 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '1'
                                              CHANGING wa_caminhao_5420-placa_car1
                                                       vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX wa_good_cells-row_id.
*              CALL METHOD CTL_ALV3_5420->REFRESH_TABLE_DISPLAY.
                PERFORM refresh_ctl_alv3_5420.
              ENDIF.
            ENDIF.

          ELSEIF wa_good_cells-fieldname EQ 'PLACA_CAR2'.

            READ TABLE it_caminhao_5420 INTO wa_caminhao_5420 INDEX wa_good_cells-row_id.

            IF wa_caminhao_5420 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '1'
                                              CHANGING wa_caminhao_5420-placa_car2
                                                       vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX wa_good_cells-row_id.
*              CALL METHOD CTL_ALV3_5420->REFRESH_TABLE_DISPLAY.
                PERFORM refresh_ctl_alv3_5420.
              ENDIF.
            ENDIF.

          ELSEIF wa_good_cells-fieldname EQ 'PLACA_CAR3'.

            READ TABLE it_caminhao_5420 INTO wa_caminhao_5420 INDEX wa_good_cells-row_id.

            IF wa_caminhao_5420 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '1'
                                              CHANGING wa_caminhao_5420-placa_car3
                                                       vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX wa_good_cells-row_id.
*              CALL METHOD CTL_ALV3_5420->REFRESH_TABLE_DISPLAY.
                PERFORM refresh_ctl_alv3_5420.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_double_click_frete_5420.
    index_click_frete = e_row-index.
    PERFORM click_em_frete USING e_row-index.
    PERFORM bloqueia_linhas_caminhao_5420.

    PERFORM refresh_ctl_alv2_5420.
    PERFORM refresh_ctl_alv3_5420.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD toolbar_5420.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'VINCULAR'.
    wa_tool-icon     = '@EH@'.
    wa_tool-quickinfo = 'Vincular Pedido'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ATUALIZAR'.
    wa_tool-icon     = '@42@'.
    wa_tool-quickinfo = 'Atualizar'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

*-----CS2019001891 - 08.03.2021 - JT - inicio
  METHOD handle_hotspot_click_5420.

    DATA: wa_frete_5420_det TYPE ty_frete_5420,
          l_selecao         TYPE char1.

*-CS2019001896 - 12.08.2021 - JT - inicio
    IF e_column_id-fieldname EQ 'EDITAR'.
      index_click = e_row_id.

      READ TABLE it_sol_5420 INTO wa_sol_5420 INDEX e_row_id.
      IF wa_sol_5420-editar = '@0Z@'. "change
        g_editar = abap_false.
      ELSE.
        g_editar = abap_true.
      ENDIF.

      PERFORM click_em_solic USING e_row_id.
      PERFORM bloqueia_linhas_frete_5420.
      PERFORM refresh_ctl_alv1_5420.
      PERFORM refresh_ctl_alv2_5420.
      PERFORM refresh_ctl_alv3_5420.
    ENDIF.
*-CS2019001896 - 12.08.2021 - JT - fim

    IF e_column_id-fieldname EQ 'ICONE'.

      IF g_editar = abap_false.
        l_selecao = abap_true.
      ELSE.
        l_selecao = abap_false.
      ENDIF.

      READ TABLE it_frete_5420 INTO wa_frete_5420_det INDEX e_row_id.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'ZSD_PEDIDOS_IMPORTACAO'
          EXPORTING
            i_nro_sol     = wa_frete_5420_det-nro_sol
            i_seq         = wa_frete_5420_det-seq
            i_filial_resp = wa_frete_5420_det-filial_resp
            i_selecao     = l_selecao.
      ENDIF.
    ENDIF.

  ENDMETHOD.
*-----CS2019001891 - 08.03.2021 - JT - fim

  METHOD toolbar_frete_5420.

    DATA wa_tool TYPE stb_button.

*-CS2019001896 - 12.08.2021 - JT - inicio
    IF g_editar = abap_true.
      l_disabled = abap_false.
    ELSE.
      l_disabled = abap_true.
    ENDIF.
*-CS2019001896 - 12.08.2021 - JT - fim

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ADD_ROW'.
    wa_tool-icon     = '@17@'.
    wa_tool-disabled  = l_disabled.
    wa_tool-quickinfo = 'Adicionar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'DEL_ROW'.
    wa_tool-icon     = '@18@'.
    wa_tool-disabled  = l_disabled.
    wa_tool-quickinfo = 'Eliminar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'EDIT_ROW'.
    wa_tool-icon     = '@0Z@'.
    wa_tool-quickinfo = 'Editar Linha'.
    wa_tool-disabled  = l_disabled.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'SAVE_FRETE'.
    wa_tool-icon     = '@2L@'.
    wa_tool-quickinfo = 'Salvar'.
    wa_tool-disabled  = l_disabled.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD toolbar_caminhao_5420.

    DATA wa_tool TYPE stb_button.

*-CS2019001896 - 12.08.2021 - JT - inicio
    IF g_editar = abap_true.
      l_disabled = abap_false.
    ELSE.
      l_disabled = abap_true.
    ENDIF.
*-CS2019001896 - 12.08.2021 - JT - fim

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ADD_ROW_CAM'.
    wa_tool-icon     = '@17@'.
    wa_tool-disabled  = l_disabled.
    wa_tool-quickinfo = 'Adicionar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'DEL_ROW_CAM'.
    wa_tool-icon     = '@18@'.
    wa_tool-disabled  = l_disabled.
    wa_tool-quickinfo = 'Eliminar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'EDIT_ROW_CAM'.
    wa_tool-disabled  = l_disabled.
    wa_tool-icon     = '@0Z@'.
    wa_tool-quickinfo = 'Editar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'SAVE_FRETE_CAM'.
    wa_tool-icon     = '@2L@'.
    wa_tool-disabled  = l_disabled.
    wa_tool-quickinfo = 'Salvar'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ROMANEIO_CAM'.
    wa_tool-icon     = '@0Q@'.
    wa_tool-quickinfo = 'Gerar Romaneio'.
    wa_tool-disabled  = l_disabled.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'EXROMANEIO_CAM'.
    wa_tool-icon     = '@VI@'.
    wa_tool-quickinfo = 'Estornar Romaneio'.
    wa_tool-disabled  = l_disabled.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ATUALIZAR'.
    wa_tool-icon     = '@42@'.
    wa_tool-quickinfo = 'Atualizar'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.


  ENDMETHOD.             "DISPLAY

  METHOD user_command_5420.

    DATA: it_selected_rows TYPE lvc_t_row,
          t_row_no         TYPE lvc_t_roid,
          wa_selected_rows TYPE lvc_s_row,
          it_rsparams      TYPE TABLE OF rsparams,
          wa_rsparams      TYPE rsparams,
          it_zsdt0062      TYPE STANDARD TABLE OF zsdt0062,
          wa_zsdt0062      TYPE zsdt0062,
          "wa_sol_5420      TYPE ty_sol_5420,
          wa_frete_5420    TYPE ty_frete_5420,
          vl_lines         TYPE i,
          wa_caminhao_5420 TYPE ty_caminhao_5420,
          wa_zsdt0082      TYPE zsdt0082,
          vl_check_rom     TYPE char1,
          wa_row           TYPE lvc_s_row,
          vl_check         TYPE char1,
          chave            TYPE zde_chave_sol.

    IF e_ucomm = 'VINCULAR'.

      CALL METHOD ctl_alv1_5420->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows
          et_row_no     = t_row_no.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.
      IF vl_lines NE 1.
        MESSAGE TEXT-041 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        READ TABLE it_sol_5420 INTO wa_sol_5420 INDEX wa_selected_rows-index.

        READ TABLE t_row_no INTO DATA(w_row_no) INDEX 1.

        SELECT SINGLE mtart INTO @DATA(_mtart) FROM mara WHERE matnr EQ @wa_sol_5420-matnr.

        IF sy-subrc IS INITIAL.

          "//Call popup to inform material or purshasing number.


          SELECT SINGLE * FROM zsdt0281 INTO @DATA(it_zsdt0281)
            WHERE werks EQ @wa_sol_5420-werks AND
            spart EQ @wa_sol_5420-spart AND
            mtart EQ @_mtart. "wa_sol_5420-matnr.
          IF sy-subrc EQ 0.
            TRY.
                PERFORM validar_material
                  USING
                    wa_sol_5420-matnr
                    wa_sol_5420-spart   "//St. Atividade: Fertilizante
                    _mtart. "//Tipo Material: Importação

                TRY.
                    PERFORM call_popup_vinculacao.
                  CATCH cx_abap_util_exception.
                    EXIT.
                ENDTRY.

              CATCH cx_abap_util_exception.
            ENDTRY.
*          ELSE.
*
*            IF ( wa_sol_5420-werks = '0175' ).
*              TRY.
*                  PERFORM validar_material
*                    USING
*                       wa_sol_5420-matnr
*                    wa_sol_5420-spart   "//St. Atividade: Fertilizante
*                    _mtart. "//Tipo Material: Importação
*
*                  TRY.
*                      PERFORM call_popup_vinculacao.
*                    CATCH cx_abap_util_exception.
*                      EXIT.
*                  ENDTRY.
*
*                CATCH cx_abap_util_exception.
*              ENDTRY.
*            ENDIF.
          ENDIF.

          "//Enqueue register;
          CONCATENATE wa_sol_5420-nro_sol wa_sol_5420-seq wa_sol_5420-vbeln wa_sol_5420-posnr INTO chave.

          CALL FUNCTION 'ZENQUEUE_SD_SOL_INSUMOS'
            EXPORTING
              chave          = chave
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
          ELSE.

*            "Nr. Carga
*            WA_RSPARAMS-SELNAME = 'P_NROSL'.
*            WA_RSPARAMS-KIND    = 'S'.  "SELECT OPTIONS TO BE PASSED
*            WA_RSPARAMS-SIGN    = 'I'.
*            WA_RSPARAMS-OPTION  = 'EQ'.
*            WA_RSPARAMS-LOW     = WA_SOL_5420-NRO_SOL.
*            APPEND WA_RSPARAMS TO IT_RSPARAMS.
*            CLEAR WA_RSPARAMS.
*
*            "Nr. Seq
*            WA_RSPARAMS-SELNAME = 'P_SEQSL'.
*            WA_RSPARAMS-KIND    = 'S'.  "SELECT OPTIONS TO BE PASSED
*            WA_RSPARAMS-SIGN    = 'I'.
*            WA_RSPARAMS-OPTION  = 'EQ'.
*            WA_RSPARAMS-LOW     = WA_SOL_5420-SEQ.
*            APPEND WA_RSPARAMS TO IT_RSPARAMS.
*            CLEAR WA_RSPARAMS.
*
*            "Dt. Lançamento
*            WA_RSPARAMS-SELNAME = 'P_ERDAT'.
*            WA_RSPARAMS-KIND    = 'S'.  "SELECT OPTIONS TO BE PASSED
*            WA_RSPARAMS-SIGN    = 'I'.
*            WA_RSPARAMS-OPTION  = 'BT'.
*            WA_RSPARAMS-LOW     = 20150101.
*            WA_RSPARAMS-HIGH    = SY-DATUM.
*            APPEND WA_RSPARAMS TO IT_RSPARAMS.
*            CLEAR WA_RSPARAMS.
*
*            "Nr. Material
*            WA_RSPARAMS-SELNAME = 'P_MATNR'.
*            WA_RSPARAMS-KIND    = 'S'.  "SELECT OPTIONS TO BE PASSED
*            WA_RSPARAMS-SIGN    = 'I'.
*            WA_RSPARAMS-OPTION  = 'EQ'.
*            WA_RSPARAMS-LOW     = WA_SOL_5420-MATNR.
*            APPEND WA_RSPARAMS TO IT_RSPARAMS.
*            CLEAR WA_RSPARAMS.
*
*
*            "Nr. OrgaNização de Vendas
*            WA_RSPARAMS-SELNAME = 'P_VKORG'.
*            WA_RSPARAMS-KIND    = 'S'.  "SELECT OPTIONS TO BE PASSED
*            WA_RSPARAMS-SIGN    = 'I'.
*            WA_RSPARAMS-OPTION  = 'EQ'.
*            WA_RSPARAMS-LOW     = WA_SOL_5420-VKORG.
*            APPEND WA_RSPARAMS TO IT_RSPARAMS.
*            CLEAR WA_RSPARAMS.
*
*            "Nr. Setor Ativ.
*            WA_RSPARAMS-SELNAME = 'P_SPART'.
*            WA_RSPARAMS-KIND    = 'S'.  "SELECT OPTIONS TO BE PASSED
*            WA_RSPARAMS-SIGN    = 'I'.
*            WA_RSPARAMS-OPTION  = 'EQ'.
*            WA_RSPARAMS-LOW     = WA_SOL_5420-SPART.
*            APPEND WA_RSPARAMS TO IT_RSPARAMS.
*            CLEAR WA_RSPARAMS.

            it_rsparams = VALUE #(
              ( selname = 'P_NROSL' kind = 'S' sign = 'I' option = 'EQ' low = wa_sol_5420-nro_sol        )
              ( selname = 'P_SEQSL' kind = 'S' sign = 'I' option = 'EQ' low = wa_sol_5420-seq            )
              ( selname = 'P_ERDAT' kind = 'S' sign = 'I' option = 'BT' low = '20150101' high = sy-datum )
              ( selname = 'P_VKORG' kind = 'S' sign = 'I' option = 'EQ' low = wa_sol_5420-vkorg          )
              ( selname = 'P_SPART' kind = 'S' sign = 'I' option = 'EQ' low = wa_sol_5420-spart          )
              ( selname = 'P_EBELN' kind = 'S' sign = 'I' option = 'EQ' low = popup_vinculacao-pedido    )
              ( selname = 'P_MATNR' kind = 'S' sign = 'I' option = 'EQ' low = wa_sol_5420-matnr          )

              ( selname = 'P_MATNRP'
                kind    = 'S'
                sign    = 'I'
                option  = 'EQ'
                low     = COND #( WHEN popup_vinculacao-material IS NOT INITIAL THEN popup_vinculacao-material
                                  ELSE wa_sol_5420-matnr )
              ) ).

            IF ( wa_sol_5420-spart EQ '02' ).
              "Tipo pedido de Compra.
              APPEND VALUE #( selname = 'P_BSART' kind = 'S' sign = 'I' option = 'EQ' low = 'ZFTE' ) TO it_rsparams.
              APPEND VALUE #( selname = 'P_BSART' kind = 'S' sign = 'I' option = 'EQ' low = 'ZOFE' ) TO it_rsparams.
*Inicio AS - 02/10/2020 - CS2020001072
              APPEND VALUE #( selname = 'P_BSART' kind = 'S' sign = 'I' option = 'EQ' low = 'ZSON' ) TO it_rsparams.
              APPEND VALUE #( selname = 'P_BSART' kind = 'S' sign = 'I' option = 'EQ' low = 'ZEFI' ) TO it_rsparams.
              APPEND VALUE #( selname = 'P_BSART' kind = 'S' sign = 'I' option = 'EQ' low = 'ZIMP' ) TO it_rsparams.
*FIM AS - 02/10/2020 - CS2020001072
*              WA_RSPARAMS-SELNAME = 'P_BSART'.
*              WA_RSPARAMS-KIND    = 'S'.  "SELECT OPTIONS TO BE PASSED
*              WA_RSPARAMS-SIGN    = 'I'.
*              WA_RSPARAMS-OPTION  = 'EQ'.
*              WA_RSPARAMS-LOW     = 'ZFTE'.
*              APPEND WA_RSPARAMS TO IT_RSPARAMS.
*              CLEAR WA_RSPARAMS.
*              "Tipo pedido de Compra.
*              WA_RSPARAMS-SELNAME = 'P_BSART'.
*              WA_RSPARAMS-KIND    = 'S'.  "SELECT OPTIONS TO BE PASSED
*              WA_RSPARAMS-SIGN    = 'I'.
*              WA_RSPARAMS-OPTION  = 'EQ'.
*              WA_RSPARAMS-LOW     = 'ZOFE'.
*              APPEND WA_RSPARAMS TO IT_RSPARAMS.
*              CLEAR WA_RSPARAMS.
            ENDIF.

            SUBMIT zsdr0062 WITH SELECTION-TABLE it_rsparams AND RETURN.

            CONCATENATE wa_sol_5420-nro_sol wa_sol_5420-seq wa_sol_5420-vbeln wa_sol_5420-posnr INTO chave.

            CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
              EXPORTING
                chave = chave.

            SELECT *
              FROM zsdt0062
              INTO TABLE it_zsdt0062
             WHERE nro_sol EQ wa_sol_5420-nro_sol
               AND seq     EQ wa_sol_5420-seq
               AND status  NE 'E'.

            "Atualiza o status da solicitação de entrega
            "------------------------------------------------

            CLEAR: wa_zsdt0082.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE *
                FROM zsdt0082
                INTO wa_zsdt0082
                WHERE nro_sol = wa_sol_5420-nro_sol
                  AND seq     = wa_sol_5420-seq
                  AND vbeln   = wa_sol_5420-vbeln
                  AND posnr   = wa_sol_5420-posnr.

              IF sy-subrc IS INITIAL.
                wa_zsdt0082-status = '2'. "volta para "Liberada"
              ENDIF.
            ELSE.

              SELECT SINGLE *
                FROM zsdt0082
                INTO wa_zsdt0082
                WHERE nro_sol = wa_sol_5420-nro_sol
                  AND seq     = wa_sol_5420-seq
                  AND vbeln   = wa_sol_5420-vbeln
                  AND posnr   = wa_sol_5420-posnr.

              IF sy-subrc IS INITIAL.
                wa_zsdt0082-status = '5'.  "Define para "Planejado/Produzido"
              ENDIF.
            ENDIF.
            MODIFY zsdt0082 FROM wa_zsdt0082.
            "------------------------------------------------

            CLEAR: wa_sol_5420-qtdvc.

            LOOP AT it_zsdt0062 INTO wa_zsdt0062.
              wa_sol_5420-qtdvc = wa_sol_5420-qtdvc + wa_zsdt0062-qtd_vinc.
            ENDLOOP.

            MODIFY it_sol_5420 FROM wa_sol_5420 INDEX wa_selected_rows-index.
            PERFORM refresh_ctl_alv1_5420.

          ENDIF.

        ENDIF.
      ENDIF.

      CLEAR popup_vinculacao.

    ELSEIF e_ucomm = 'ADD_ROW'.

      IF vg_sol_5420 IS NOT INITIAL.

        CLEAR: wa_frete_5420.
        wa_frete_5420-edit    = abap_true.
        wa_frete_5420-nro_sol = vg_sol_5420-nro_sol.
        wa_frete_5420-seq     = vg_sol_5420-seq.
        wa_frete_5420-matnr   = vg_sol_5420-matnr.
        wa_frete_5420-um   = vg_sol_5420-meins.
        wa_frete_5420-icone  = '@CC@'. "'@PB@'.

        APPEND wa_frete_5420 TO it_frete_5420.
        PERFORM bloqueia_linhas_frete_5420.
        PERFORM refresh_ctl_alv2_5420.

      ENDIF.

    ELSEIF e_ucomm = 'ADD_ROW_CAM'.

      IF vg_frete_5420       IS NOT INITIAL AND
*         vg_frete_5420-inco1 EQ 'CFR' AND
*-CS2019001891 - 28.06.2021 - JT - inicio
         vg_frete_5420-filial_resp EQ g_filial_resp.
*        vg_frete_5420-filial_resp EQ 'TCOR'.
*-CS2019001891 - 28.06.2021 - JT - fim

        CLEAR: wa_caminhao_5420.
        wa_caminhao_5420-edit    = abap_true.
        wa_caminhao_5420-nro_sol = vg_frete_5420-nro_sol.
        wa_caminhao_5420-seq     = vg_frete_5420-seq.
        wa_caminhao_5420-um      = vg_frete_5420-um.

        APPEND wa_caminhao_5420 TO it_caminhao_5420.
        PERFORM bloqueia_linhas_caminhao_5420.
        PERFORM refresh_ctl_alv3_5420.

      ENDIF.

    ELSEIF e_ucomm = 'DEL_ROW'.

      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv2_5420->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE it_frete_5420 INTO wa_frete_5420 INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.
          "Verificar se existe vinculo, caso exista não pode deixar deletar.
          SELECT SINGLE * FROM zsdt0137
            INTO @DATA(ws_zsdt0137)
            WHERE nro_sol EQ  @wa_frete_5420-nro_sol
            AND seq EQ @wa_frete_5420-seq
            AND filial_resp EQ @wa_frete_5420-filial_resp
            AND ped_imp EQ 'X'.
          IF ws_zsdt0137 IS NOT INITIAL.
            MESSAGE ' Existem pedidos de importação associados, por favor removê-los!' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR: ws_zsdt0137.
            CONTINUE.
          ELSE.


            wa_frete_5420-bezei = 'DELETE'.
            MODIFY it_frete_5420 FROM wa_frete_5420 INDEX wa_selected_rows-index..
            CLEAR: wa_frete_5420, ws_zsdt0137.
          ENDIF.
        ENDIF.
      ENDLOOP.

      DELETE it_frete_5420 WHERE bezei EQ 'DELETE'.
      PERFORM refresh_ctl_alv2_5420.

    ELSEIF e_ucomm = 'DEL_ROW_CAM'.

      IF vg_frete_5420 IS NOT INITIAL." AND vg_frete_5420-inco1 EQ 'CFR'.

        CLEAR: it_selected_rows, wa_selected_rows.

        CALL METHOD ctl_alv3_5420->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        LOOP AT it_selected_rows INTO wa_selected_rows.
          READ TABLE it_caminhao_5420 INTO wa_caminhao_5420 INDEX wa_selected_rows-index.
          IF sy-subrc IS INITIAL.
            IF wa_caminhao_5420-status NE 2.
              wa_caminhao_5420-name1 = 'DELETE'.
              MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX wa_selected_rows-index..
              CLEAR: wa_caminhao_5420.
            ENDIF.
          ENDIF.
        ENDLOOP.

        DELETE it_caminhao_5420 WHERE name1 EQ 'DELETE'.
        PERFORM refresh_ctl_alv3_5420.

      ENDIF.

    ELSEIF e_ucomm = 'EDIT_ROW'.

      CLEAR: it_selected_rows, wa_selected_rows.

      CALL METHOD ctl_alv2_5420->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE it_frete_5420 INTO wa_frete_5420 INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.
          MOVE abap_true TO wa_frete_5420-edit.
          MODIFY it_frete_5420 FROM wa_frete_5420 INDEX wa_selected_rows-index..
          CLEAR: wa_frete_5420.
        ENDIF.
      ENDLOOP.

      PERFORM bloqueia_linhas_frete_5420.
      PERFORM refresh_ctl_alv2_5420.

    ELSEIF e_ucomm = 'EDIT_ROW_CAM'.

      IF vg_frete_5420 IS NOT INITIAL." AND vg_frete_5420-inco1 EQ 'CFR'.

        CLEAR: it_selected_rows, wa_selected_rows.

        CALL METHOD ctl_alv3_5420->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        LOOP AT it_selected_rows INTO wa_selected_rows.
          READ TABLE it_caminhao_5420 INTO wa_caminhao_5420 INDEX wa_selected_rows-index.
          IF sy-subrc IS INITIAL.
            IF wa_caminhao_5420-status NE 2.
              MOVE abap_true TO wa_caminhao_5420-edit.
              MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX wa_selected_rows-index..
              CLEAR: wa_caminhao_5420.
            ENDIF.
          ENDIF.
        ENDLOOP.

        PERFORM bloqueia_linhas_caminhao_5420.
        PERFORM refresh_ctl_alv3_5420.

      ENDIF.

    ELSEIF e_ucomm = 'SAVE_FRETE'.

      PERFORM salva_frete_5420.

    ELSEIF e_ucomm = 'SAVE_FRETE_CAM'.

      PERFORM salva_frete_cam_5420.

    ELSEIF e_ucomm = 'ROMANEIO_CAM'.

      CLEAR: it_selected_rows, wa_selected_rows, it_caminhao_aux_5420, vl_check.

      CALL METHOD ctl_alv3_5420->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE TEXT-103 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.

        LOOP AT it_caminhao_5420 INTO wa_caminhao_5420.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.
*            IF vg_frete_5420-inco1 EQ 'CFR'.
            IF wa_caminhao_5420-status EQ 1.
              IF wa_caminhao_5420-nfenum IS INITIAL OR wa_caminhao_5420-series IS INITIAL OR
                 wa_caminhao_5420-netwr IS INITIAL OR wa_caminhao_5420-docdat_nf IS INITIAL.
                MESSAGE TEXT-104 TYPE 'S' DISPLAY LIKE 'E'.
                vl_check = abap_true.
              ELSEIF wa_caminhao_5420-edit EQ abap_true.
                MOVE abap_true TO vl_check.
                MESSAGE TEXT-122 TYPE 'S' DISPLAY LIKE 'E'.
              ELSE.
                APPEND wa_caminhao_5420 TO it_caminhao_aux_5420.
              ENDIF.
            ELSE.
              vl_check = abap_true.
              MESSAGE TEXT-088 TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.
*            ELSE.
*              vl_check = abap_true.
*              MESSAGE text-087 TYPE 'S' DISPLAY LIKE 'E'.
*            ENDIF.
          ENDIF.
        ENDLOOP.

        IF vl_check IS INITIAL.
          IF it_caminhao_aux_5420 IS NOT INITIAL.
            PERFORM salva_romaneios_5420 CHANGING vl_check_rom.

            LOOP AT it_frete_5420 INTO wa_frete_5420 WHERE nro_sol     EQ vg_frete_5420-nro_sol
                                                   AND seq         EQ vg_frete_5420-seq
                                                   AND filial_resp EQ vg_frete_5420-filial_resp.
              wa_row-index = sy-tabix.
            ENDLOOP.

            CALL METHOD on_double_click_frete_5420
              EXPORTING
                e_row = wa_row.

          ELSE.
            MESSAGE TEXT-045 TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'EXROMANEIO_CAM'.

      CLEAR: it_selected_rows, wa_selected_rows, it_caminhao_aux_5420.

      CALL METHOD ctl_alv3_5420->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_caminhao_5420 INTO wa_caminhao_5420.
        READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
        IF sy-subrc IS INITIAL.
*          IF vg_frete_5420-inco1 EQ 'CFR'.
          IF wa_caminhao_5420-status EQ 2.
            APPEND wa_caminhao_5420 TO it_caminhao_aux_5420.
          ENDIF.
*          ENDIF.
        ENDIF.
      ENDLOOP.

      IF it_caminhao_aux_5420 IS NOT INITIAL.
        PERFORM delete_romaneios_5420.
      ELSE.
        MESSAGE TEXT-054 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

      LOOP AT it_frete_5420 INTO wa_frete_5420 WHERE nro_sol     EQ vg_frete_5420-nro_sol
                                                 AND seq         EQ vg_frete_5420-seq
                                                 AND filial_resp EQ vg_frete_5420-filial_resp.
        wa_row-index = sy-tabix.
      ENDLOOP.

      CALL METHOD on_double_click_frete_5420
        EXPORTING
          e_row = wa_row.
    ELSEIF e_ucomm = 'ATUALIZAR'.

      PERFORM seleciona_sol_5420.
      PERFORM alv_sol_5420.

      PERFORM click_em_solic USING index_click.
      PERFORM click_em_frete USING index_click_frete.
      PERFORM bloqueia_linhas_frete_5420.
      PERFORM bloqueia_linhas_caminhao_5420.

      PERFORM refresh_ctl_alv1_5420.
      PERFORM refresh_ctl_alv2_5420.
      PERFORM refresh_ctl_alv3_5420.

    ENDIF.

    PERFORM refresh_ctl_alv1_5420.

  ENDMETHOD.                    "USER_COMMAND

  METHOD on_f4_5420.
    PERFORM f4_filial_5420 USING er_event_data es_row_no.
  ENDMETHOD.

  METHOD on_f4_pedido_5420.
    PERFORM f4_pedido_5420 USING er_event_data es_row_no.
  ENDMETHOD.
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5420  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5420 OUTPUT.

  PERFORM seleciona_sol_5420.
  PERFORM alv_sol_5420.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5420  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5420 INPUT.

  CASE sy-ucomm.
    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 80 20.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5420_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5420_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SELECT_LOTE
*&---------------------------------------------------------------------*
FORM seleciona_sol_5420.

  DATA: it_makt     TYPE STANDARD TABLE OF makt,
        it_kna1     TYPE STANDARD TABLE OF kna1,
        it_tvkbt    TYPE STANDARD TABLE OF tvkbt,
        it_zsdt0062 TYPE STANDARD TABLE OF zsdt0062,
        it_zsdt0138 TYPE STANDARD TABLE OF zsdt0138,
        wa_makt     TYPE makt,
        wa_kna1     TYPE kna1,
        wa_tvkbt    TYPE tvkbt,
        wa_zsdt0062 TYPE zsdt0062,
        wa_zsdt0138 TYPE zsdt0138,
        wa_sol_5420 TYPE ty_sol_5420,
        vl_cont     TYPE i,
        l_chave     TYPE zde_chave_sol.

  SELECT zsdt0082~nro_sol
         zsdt0082~vkbur
         zsdt0082~vbeln
         zsdt0082~posnr
         zsdt0082~vkorg
         zsdt0082~spart
         zsdt0082~seq
         vbap~matnr
         zsdt0082~werks
         zsdt0082~qte_lib
         vbap~meins
         zsdt0082~nr_rot
         vbpa~kunnr
         vbkd~inco1
    FROM zsdt0082
    INNER JOIN vbap ON vbap~vbeln = zsdt0082~vbeln
                    AND vbap~posnr = zsdt0082~posnr
    INNER JOIN vbkd ON vbkd~vbeln = vbap~vbeln
                    "AND VBKD~POSNR = VBAP~POSNR
    INNER JOIN vbpa ON vbpa~vbeln = vbkd~vbeln
    INNER JOIN mara ON mara~matnr = vbap~matnr
    INTO CORRESPONDING FIELDS OF TABLE it_sol_5420
*-CS2019001891 - 28.06.2021 - JT - inicio
    WHERE zsdt0082~vkorg    IN x_vkorg
      AND zsdt0082~vkbur    IN x_vkbur
      AND zsdt0082~spart    EQ x_spart
      AND zsdt0082~nro_sol  IN x_nrsol
      AND zsdt0082~dt_liber IN x_datas
      AND zsdt0082~vbeln    IN x_ovcor
      AND zsdt0082~seq      NE 1
      AND vbkd~inco1        IN x_inco1
      AND vbkd~posnr        EQ 0
      AND vbpa~kunnr        IN x_kunnr
      AND vbpa~parvw        EQ 'AG'
      AND ( zsdt0082~status EQ 2 OR
            zsdt0082~status EQ 5 )
      AND mara~mtart NE 'ZFER'.
*   WHERE zsdt0082~vkorg    IN r_vkorg
*     AND zsdt0082~vkbur    IN r_vkbur
*     AND zsdt0082~spart    EQ r_spart
*     AND zsdt0082~nro_sol  IN r_nrsol
*     AND zsdt0082~dt_liber IN r_datas
*     AND zsdt0082~vbeln    IN r_ovcor
*     AND zsdt0082~seq      NE 1
*     AND vbkd~inco1        IN r_inco1
*     AND vbkd~posnr        EQ 0
*     AND vbpa~kunnr        IN r_kunnr
*     AND vbpa~parvw        EQ 'AG'
*     AND ( zsdt0082~status EQ 2 OR
*           zsdt0082~status EQ 5 )
*     AND mara~mtart NE 'ZFER'.
*-CS2019001891 - 28.06.2021 - JT - fim

*-CS2019001896 - 12.08.2021 - JT - inicio
  LOOP AT it_sol_5420 INTO wa_sol_5420.
    l_tabix = sy-tabix.

    FREE: it_enq.

    wa_sol_5420-editar = '@10@'.

    l_gname = 'IT_ZSDT0082'.
    CONCATENATE sy-mandt          wa_sol_5420-nro_sol wa_sol_5420-seq
                wa_sol_5420-vbeln wa_sol_5420-posnr
           INTO l_garg.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gname                 = l_gname
        garg                  = l_garg
      TABLES
        enq                   = it_enq
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF it_enq[] IS NOT INITIAL.
      wa_sol_5420-editar = '@06@'.
    ENDIF.
    MODIFY it_sol_5420 FROM wa_sol_5420 INDEX l_tabix.
  ENDLOOP.
*-CS2019001896 - 12.08.2021 - JT - fim

  IF it_sol_5420 IS NOT INITIAL.

    SELECT *
      FROM tvkbt
      INTO TABLE it_tvkbt
      FOR ALL ENTRIES IN it_sol_5420
      WHERE vkbur EQ it_sol_5420-vkbur
        AND spras EQ sy-langu.

    SELECT *
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_sol_5420
      WHERE kunnr EQ it_sol_5420-kunnr.

    SELECT *
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_sol_5420
      WHERE matnr EQ it_sol_5420-matnr.

    SELECT *
      FROM zsdt0062
      INTO TABLE it_zsdt0062
      FOR ALL ENTRIES IN it_sol_5420
      WHERE nro_sol EQ it_sol_5420-nro_sol
        AND seq     EQ it_sol_5420-seq
        AND status  NE 'E'.

    SELECT *
      FROM zsdt0138
      INTO TABLE it_zsdt0138
      FOR ALL ENTRIES IN it_sol_5420
      WHERE nro_sol EQ it_sol_5420-nro_sol
        AND seq     EQ it_sol_5420-seq
        AND status  NE 'X'.

  ENDIF.

  LOOP AT it_sol_5420 INTO wa_sol_5420.

    vl_cont = vl_cont + 1.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_sol_5420-matnr.
    IF sy-subrc IS INITIAL.
      wa_sol_5420-maktx = wa_makt-maktx.
    ENDIF.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_sol_5420-kunnr.
    IF sy-subrc IS INITIAL.
      wa_sol_5420-name1 = wa_kna1-name1.
    ENDIF.

    READ TABLE it_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_sol_5420-vkbur.
    IF sy-subrc IS INITIAL.
      wa_sol_5420-bezei = wa_tvkbt-bezei.
    ENDIF.

    LOOP AT it_zsdt0062 INTO wa_zsdt0062 WHERE nro_sol EQ wa_sol_5420-nro_sol
                                           AND seq     EQ wa_sol_5420-seq.
      wa_sol_5420-qtdvc = wa_sol_5420-qtdvc + wa_zsdt0062-qtd_vinc.
    ENDLOOP.

    LOOP AT it_zsdt0138 INTO wa_zsdt0138 WHERE nro_sol EQ wa_sol_5420-nro_sol
                                           AND seq     EQ wa_sol_5420-seq.
      wa_sol_5420-qtdeb = wa_sol_5420-qtdeb + wa_zsdt0138-qtd_embarq.
    ENDLOOP.

    MODIFY it_sol_5420 FROM wa_sol_5420 INDEX vl_cont.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_LOTE
*&---------------------------------------------------------------------*
FORM alv_sol_5420.

  IF g_custom_container_5420 IS INITIAL.

    CREATE OBJECT g_custom_container_5420
      EXPORTING
        container_name              = 'CONTAINER5420'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT dg_splitter_1_5420
      EXPORTING
        parent  = g_custom_container_5420
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1_5420->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_5420.

    CALL METHOD dg_splitter_1_5420->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_2_5420.

    CREATE OBJECT dg_splitter_2_5420
      EXPORTING
        parent  = dg_parent_2_5420
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2_5420->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_3_5420.

    CALL METHOD dg_splitter_2_5420->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_4_5420.

    CALL METHOD dg_splitter_1_5420->set_row_mode
      EXPORTING
        mode = dg_splitter_1_5420->mode_relative.

    CALL METHOD dg_splitter_1_5420->set_row_height
      EXPORTING
        id     = 1
        height = 50.

    CALL METHOD dg_splitter_1_5420->set_row_height
      EXPORTING
        id     = 2
        height = 50.

    CALL METHOD dg_splitter_2_5420->set_column_mode
      EXPORTING
        mode = dg_splitter_2_5420->mode_relative.

    CALL METHOD dg_splitter_2_5420->set_column_width
      EXPORTING
        id    = 1
        width = 30.

    CALL METHOD dg_splitter_2_5420->set_column_width
      EXPORTING
        id    = 2
        width = 70.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5420 USING:
*-CS2019001896 - 12.08.2021 - JT - inicio
          01 'EDITAR'         'ZSDT0082'      ' '  ' '  ' '  'X'   ' '   ' '   ' '   ' '   'Edição',
*-CS2019001896 - 12.08.2021 - JT - fim
          01 'NRO_SOL'        'ZSDT0082'      ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Sol.',
          02 'VKBUR'          'ZSDT0082'      ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Venda',
          03 'BEZEI'          ''              ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Esc. Venda',
          04 'KUNNR'          'VBPA'          ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Cliente',
          05 'NAME1'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cliente',
          06 'VBELN'          'ZSDT0082'      ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
          07 'POSNR'          ''              ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item',
          08 'MATNR'          'MAKT'          ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Produto',
          09 'MAKTX'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Produto',
          10 'WERKS'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Centro Forn.',
          11 'QTE_LIB'        'ZSDT0082'      ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. Solic.',
          12 'QTDVC'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. Vinc. Ped.',
          13 'QTDEB'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. Embarc.',
          14 'MEINS'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
          15 'INCO1'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp. Frete',
          16 'NR_ROT'         'ZSDT0082'      ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Roteiro'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_frete_5420 USING:
           01 'NRO_SOL'       ' '          ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
           02 'FILIAL_RESP'   ' '          ' '  'X'  ' '  ' '   'X'   ' '   ' '   'X'   'Filial Frete',
           03 'BEZEI'         ' '          ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Filial',
           04 'QTD_VINC'      'ZSDT0137'   ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. Vinc.',
           05 'UM'            ' '          ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
           06 'ICONE'         ' '          ' '  ' '  ' '  'X'   'X'   ' '   ' '   ' '   'P.Imp.'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_caminhao_5420 USING:
          01 'ICONE'                ' '         ' '     ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status',
          02 'NRO_SOL'              ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
          03 'EBELN'                ' '         'C310'  ' '  ' '  ' '   'X'   ' '   ' '   'X'   'Pedido Vinc.',
          04 'EBELP'                ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item Ped.',
          05 'COD_TRANSPORTADORA'   'ZSDT0138'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   'X'   'Prop. Veículo',
          06 'NAME1'                ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Propietário',
          07 'PRECO_FRETE'          'ZSDT0138'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Vlr. Frete',
          08 'PLACA_CAV'            ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Cavalo',
          09 'PLACA_CAR1'           ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Carreta I',
          10 'PLACA_CAR2'           ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Carreta II',
          10 'PLACA_CAR3'           ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Carreta III',
          11 'MOTORISTA'            'ZSDT0138'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   'X'   'Cód. Motorista',
          12 'NAME2'                ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Motorista',
          13 'QTD_EMBARQ'           ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd.',
          14 'UM'                   ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
          15 'NIFORN'               ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Intern. fornec',
          15 'NFENUM'               ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'NF Fornc.',
          16 'SERIES'               ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Série',
          17 'NETWR'                'ZSDT0001'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Valor NF',
          18 'DOCDAT_NF'            'ZSDT0138'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Data Emissão NF',
          19 'NR_ROMANEIO'          ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Romaneio'.


    gs_layout_5420_alv1-sel_mode   = 'A'.
    gs_layout_5420_alv1-stylefname = 'CELLSTYLES'.
    gs_layout_5420_alv1-cwidth_opt = 'X'.
    gs_layout_5420_alv1-info_fname = 'COR'.
    gs_layout_5420_alv1-smalltitle = 'X'.
    gs_layout_5420_alv1-grid_title = 'Solicitações de Ordem'.
    gs_layout_5420_alv2-sel_mode   = 'A'.
    gs_layout_5420_alv2-stylefname = 'CELLSTYLES'.
    gs_layout_5420_alv2-cwidth_opt = 'X'.
    gs_layout_5420_alv2-info_fname = 'COR'.
    gs_layout_5420_alv2-smalltitle = 'X'.
    gs_layout_5420_alv2-grid_title = 'Filiais de Frete'.
    gs_layout_5420_alv3-sel_mode   = 'A'.
    gs_layout_5420_alv3-stylefname = 'CELLSTYLES'.
    gs_layout_5420_alv3-cwidth_opt = 'X'.
    gs_layout_5420_alv3-info_fname = 'COR'.
    gs_layout_5420_alv3-smalltitle = 'X'.
    gs_layout_5420_alv3-grid_title = 'Embarques'.

    PERFORM sort USING 'NRO_SOL'      CHANGING it_sort_sol_5420.
    PERFORM sort USING 'FILIAL_RESP'  CHANGING it_sort_frete_5420.

    CREATE OBJECT ctl_alv1_5420
      EXPORTING
        i_parent = dg_parent_1_5420.           "ALV Solicitação

    CREATE OBJECT ctl_alv2_5420
      EXPORTING
        i_parent = dg_parent_3_5420.           "ALV Filial Frete

    CREATE OBJECT ctl_alv3_5420
      EXPORTING
        i_parent = dg_parent_4_5420.          "Alv Caminhões

    PERFORM excluir_botoes CHANGING it_exclude_5420.
    PERFORM excluir_botoes_frete_5420 CHANGING it_exclude_frete_5420.
    PERFORM registrar_f4_5420.
    PERFORM registrar_f4_pedido_5420.

    SET HANDLER:
      lcl_event_handler_5420=>on_double_click_5420 FOR ctl_alv1_5420,
      lcl_event_handler_5420=>user_command_5420 FOR ctl_alv1_5420,
      lcl_event_handler_5420=>toolbar_5420 FOR ctl_alv1_5420,
*-CS2019001896 - 12.08.2021 - JT - inicio
      lcl_event_handler_5420=>handle_hotspot_click_5420 FOR ctl_alv1_5420.
*-CS2019001896 - 12.08.2021 - JT - fim

    CALL METHOD ctl_alv1_5420->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5420_alv1
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_sol_5420
        it_outtab       = it_sol_5420
        it_sort         = it_sort_sol_5420.

    SET HANDLER:
      lcl_event_handler_5420=>toolbar_frete_5420 FOR ctl_alv2_5420,
      lcl_event_handler_5420=>user_command_5420 FOR ctl_alv2_5420,
      lcl_event_handler_5420=>on_f4_5420 FOR ctl_alv2_5420,
      lcl_event_handler_5420=>on_double_click_frete_5420 FOR ctl_alv2_5420,
*-----CS2019001891 - 08.03.2021 - JT - inicio
      lcl_event_handler_5420=>handle_hotspot_click_5420 FOR ctl_alv2_5420.
*-----CS2019001891 - 08.03.2021 - JT - fim

    CALL METHOD ctl_alv2_5420->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5420_alv2
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_frete_5420
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_frete_5420
        it_outtab            = it_frete_5420
        it_sort              = it_sort_frete_5420.

    SET HANDLER:
      lcl_event_handler_5420=>toolbar_caminhao_5420 FOR ctl_alv3_5420,
      lcl_event_handler_5420=>user_command_5420 FOR ctl_alv3_5420,
      lcl_event_handler_5420=>data_changed_finished_5420 FOR ctl_alv3_5420,
      lcl_event_handler_5420=>on_f4_pedido_5420 FOR ctl_alv3_5420.

    CALL METHOD ctl_alv3_5420->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5420_alv3
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5420
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_caminhao_5420
        it_outtab            = it_caminhao_5420
        it_sort              = it_sort_caminhao_5420.

    CALL METHOD ctl_alv3_5420->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    CALL METHOD ctl_alv1_5420->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv2_5420->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv3_5420->refresh_table_display
      EXPORTING
        is_stable = _stable.


  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLICK_EM_SOLIC
*&---------------------------------------------------------------------*
FORM click_em_solic  USING    p_e_row_index.

  DATA: it_tvkbt      TYPE STANDARD TABLE OF tvkbt,
        wa_tvkbt      TYPE tvkbt,
        wa_sol_5420   TYPE ty_sol_5420,
        wa_frete_5420 TYPE ty_frete_5420,
        vl_cont       TYPE i,
        l_tabix       TYPE sy-tabix,
        chave         TYPE zde_chave_sol.

  CLEAR: it_frete_5420, it_caminhao_5420.

  LOOP AT it_sol_5420 INTO wa_sol_5420 WHERE nro_sol EQ vg_sol_5420-nro_sol
                                         AND seq     EQ vg_sol_5420-seq
                                         AND vbeln   EQ vg_sol_5420-vbeln
                                         AND posnr   EQ vg_sol_5420-posnr.

    vl_cont = sy-tabix.

    CONCATENATE wa_sol_5420-nro_sol wa_sol_5420-seq wa_sol_5420-vbeln wa_sol_5420-posnr INTO chave.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = chave.

    CLEAR: wa_sol_5420-cor.
*-CS2019001896 - 12.08.2021 - JT - inicio
    wa_sol_5420-editar = '@10@'.
*-CS2019001896 - 12.08.2021 - JT - fim
    MODIFY it_sol_5420 FROM wa_sol_5420 INDEX vl_cont.

  ENDLOOP.

  CLEAR: vg_sol_5420, vl_cont.

  READ TABLE it_sol_5420 INTO wa_sol_5420 INDEX p_e_row_index.

*-CS2019001896 - 12.08.2021 - JT - inicio
  IF sy-subrc IS INITIAL.
    wa_sol_5420-cor = 'C300'.
    vg_sol_5420 = wa_sol_5420.

    IF g_editar = abap_false.
      FREE: it_enq.
      l_gname = 'IT_ZSDT0082'.
      CONCATENATE sy-mandt          wa_sol_5420-nro_sol wa_sol_5420-seq
                  wa_sol_5420-vbeln wa_sol_5420-posnr
             INTO l_garg.

      CALL FUNCTION 'ENQUEUE_READ'
        EXPORTING
          gname                 = l_gname
          garg                  = l_garg
        TABLES
          enq                   = it_enq
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2
          OTHERS                = 3.

      IF it_enq[] IS NOT INITIAL.
        wa_sol_5420-editar = '@06@'.
      ELSE.
        wa_sol_5420-editar = '@10@'.
      ENDIF.
      vg_sol_5420 = wa_sol_5420.
      sy-subrc = 0.
*-CS2019001896 - 12.08.2021 - JT - fim
    ELSE.
      wa_sol_5420-cor = 'C300'.
*-CS2019001896 - 12.08.2021 - JT - inicio
      wa_sol_5420-editar = '@0Z@'.
*-CS2019001896 - 12.08.2021 - JT - fim
      vg_sol_5420 = wa_sol_5420.

      CONCATENATE wa_sol_5420-nro_sol wa_sol_5420-seq wa_sol_5420-vbeln wa_sol_5420-posnr INTO chave.

      CALL FUNCTION 'ZENQUEUE_SD_SOL_INSUMOS'
        EXPORTING
          chave          = chave
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
    ENDIF.
*-CS2019001896 - 12.08.2021 - JT - fim

    IF sy-subrc <> 0.
*-CS2019001896 - 12.08.2021 - JT - inicio
      g_editar = abap_false.
      wa_sol_5420-editar = '@06@'.  "Locked
      MODIFY it_sol_5420 FROM wa_sol_5420 INDEX p_e_row_index.
*-CS2019001896 - 12.08.2021 - JT - fim
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ELSE.
      MODIFY it_sol_5420 FROM wa_sol_5420 INDEX p_e_row_index.

*-CS2019001896 - 12.08.2021 - JT - inicio
      LOOP AT it_sol_5420 INTO wa_sol_5420.
        l_tabix = sy-tabix.

        CHECK sy-tabix <> p_e_row_index.

        FREE: it_enq.
        l_gname = 'IT_ZSDT0082'.
        CONCATENATE sy-mandt          wa_sol_5420-nro_sol wa_sol_5420-seq
                    wa_sol_5420-vbeln wa_sol_5420-posnr
               INTO l_garg.

        CALL FUNCTION 'ENQUEUE_READ'
          EXPORTING
            gname                 = l_gname
            garg                  = l_garg
          TABLES
            enq                   = it_enq
          EXCEPTIONS
            communication_failure = 1
            system_failure        = 2
            OTHERS                = 3.

        IF it_enq[] IS NOT INITIAL.
          wa_sol_5420-editar = '@06@'.
        ELSE.
          wa_sol_5420-editar = '@10@'.
        ENDIF.
        MODIFY it_sol_5420 FROM wa_sol_5420 INDEX l_tabix.
      ENDLOOP.
*-CS2019001896 - 12.08.2021 - JT - fim

      SELECT *
      FROM zsdt0137
      INTO CORRESPONDING FIELDS OF TABLE it_frete_5420
      WHERE nro_sol EQ vg_sol_5420-nro_sol
        AND seq     EQ vg_sol_5420-seq
        AND status  NE 'X'.

      IF it_frete_5420 IS NOT INITIAL.

        SELECT *
          FROM tvkbt
          INTO TABLE it_tvkbt
          FOR ALL ENTRIES IN it_frete_5420
          WHERE vkbur EQ it_frete_5420-filial_resp
            AND spras EQ sy-langu.

      ENDIF.

      LOOP AT it_frete_5420 INTO wa_frete_5420.

        vl_cont = vl_cont + 1.


*-CS2019001891 - 28.06.2021 - JT - inicio
*           READ TABLE it_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_frete_5420-filial_resp.
*           IF sy-subrc IS INITIAL.
*             wa_frete_5420-bezei = wa_tvkbt-bezei.
*           ENDIF.
        READ TABLE t_zsdt0282_tot INTO DATA(w_0282) WITH KEY transp = wa_frete_5420-filial_resp
                                                             vkorg  = vg_sol_5420-vkorg.
        IF sy-subrc IS INITIAL.
          wa_frete_5420-bezei = w_0282-nome.
        ENDIF.
*-CS2019001891 - 28.06.2021 - JT - inicio

        wa_frete_5420-inco1 = vg_sol_5420-inco1.
        wa_frete_5420-icone = '@CC@'.

        MODIFY it_frete_5420 FROM wa_frete_5420 INDEX vl_cont.

      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLICK_EM_FRETE
*&---------------------------------------------------------------------*
FORM click_em_frete  USING    p_e_row_index.

  DATA: wa_frete_5420    TYPE ty_frete_5420,
        wa_caminhao_5420 TYPE ty_caminhao_5420,
        it_lfa1          TYPE STANDARD TABLE OF lfa1,
        wa_lfa1          TYPE lfa1,
        it_zsdt0001      TYPE STANDARD TABLE OF zsdt0001,
        wa_zsdt0001      TYPE zsdt0001,
        vl_cont          TYPE i.

  READ TABLE it_frete_5420 INTO wa_frete_5420 INDEX p_e_row_index.
  IF sy-subrc IS INITIAL.
    IF wa_frete_5420-edit NE abap_true.

      CLEAR: vg_frete_5420.

      LOOP AT it_frete_5420 INTO wa_frete_5420 WHERE cor IS NOT INITIAL.
        CLEAR: wa_frete_5420-cor.
        MODIFY it_frete_5420 FROM wa_frete_5420 INDEX sy-tabix.
      ENDLOOP.

      READ TABLE it_frete_5420 INTO wa_frete_5420 INDEX p_e_row_index.
      IF sy-subrc IS INITIAL.
        wa_frete_5420-cor = 'C300'.
        vg_frete_5420 = wa_frete_5420.
        MODIFY it_frete_5420 FROM wa_frete_5420 INDEX p_e_row_index.
      ENDIF.

      SELECT *
        FROM zsdt0138
        INTO CORRESPONDING FIELDS OF TABLE it_caminhao_5420
        WHERE nro_sol     EQ vg_frete_5420-nro_sol
          AND seq         EQ vg_frete_5420-seq
          AND filial_resp EQ vg_frete_5420-filial_resp
          AND status  NE 'X'.

      IF it_caminhao_5420 IS NOT INITIAL.

        SELECT *
          FROM lfa1
          INTO TABLE it_lfa1
          FOR ALL ENTRIES IN it_caminhao_5420
          WHERE lifnr EQ it_caminhao_5420-cod_transportadora.

        SELECT *
          FROM lfa1
          APPENDING TABLE it_lfa1
          FOR ALL ENTRIES IN it_caminhao_5420
          WHERE lifnr EQ it_caminhao_5420-motorista.

        SELECT *
          FROM zsdt0001
          INTO TABLE it_zsdt0001
          FOR ALL ENTRIES IN it_caminhao_5420
          WHERE ch_referencia EQ it_caminhao_5420-ch_referencia.

      ENDIF.

      LOOP AT it_caminhao_5420 INTO wa_caminhao_5420.

        vl_cont = vl_cont + 1.

        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_caminhao_5420-cod_transportadora.
        IF sy-subrc IS INITIAL.
          wa_caminhao_5420-name1 = wa_lfa1-name1.
        ENDIF.

        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_caminhao_5420-motorista.
        IF sy-subrc IS INITIAL.
          wa_caminhao_5420-name2 = wa_lfa1-name1.
        ENDIF.

        IF wa_caminhao_5420-status EQ 1.
          wa_caminhao_5420-icone = '@5B@'.
        ELSEIF wa_caminhao_5420-status EQ 2.
          wa_caminhao_5420-icone = '@0Q@'.
        ENDIF.

        READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY ch_referencia = wa_caminhao_5420-ch_referencia.
        IF sy-subrc IS INITIAL.
          wa_caminhao_5420-nr_romaneio = wa_zsdt0001-nr_romaneio.
          IF wa_zsdt0001-st_proc EQ '99'.
            wa_caminhao_5420-icone = '@01@'.
          ENDIF.
        ENDIF.



        MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX vl_cont.

      ENDLOOP.

    ELSE.
      MESSAGE TEXT-067 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BLOQUEIA_LINHAS_FRETE_5420
*&---------------------------------------------------------------------*
FORM bloqueia_linhas_frete_5420.
  DATA: vl_cont       TYPE i,
        it_celltab    TYPE lvc_t_styl,
        wa_frete_5420 TYPE ty_frete_5420.

  LOOP AT it_frete_5420 INTO wa_frete_5420.
    vl_cont = vl_cont + 1.
    CLEAR: it_celltab, wa_frete_5420-cellstyles.
    "REFRESH IT_CELLTAB.
    PERFORM fill_celltab_5420 USING wa_frete_5420-edit
                         CHANGING it_celltab.
    CLEAR wa_frete_5420-cellstyles.
    INSERT LINES OF it_celltab INTO TABLE wa_frete_5420-cellstyles.
    MODIFY it_frete_5420 FROM wa_frete_5420 INDEX vl_cont.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BLOQUEIA_LINHAS_FRETE_5420
*&---------------------------------------------------------------------*
FORM bloqueia_linhas_caminhao_5420.
  DATA: vl_cont          TYPE i,
        it_celltab       TYPE lvc_t_styl,
        wa_caminhao_5420 TYPE ty_caminhao_5420.

  LOOP AT it_caminhao_5420 INTO wa_caminhao_5420.
    vl_cont = vl_cont + 1.
    CLEAR: it_celltab, wa_caminhao_5420-cellstyles.
    PERFORM fill_celltab_5420 USING wa_caminhao_5420-edit
                         CHANGING it_celltab.
    CLEAR wa_caminhao_5420-cellstyles.
    INSERT LINES OF it_celltab INTO TABLE wa_caminhao_5420-cellstyles.
    MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX vl_cont.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB_5420
*&---------------------------------------------------------------------*
FORM fill_celltab_5420  USING    p_edit
                        CHANGING p_it_celltab TYPE lvc_t_styl.

  DATA: wa_celltab TYPE lvc_s_styl,
        status     TYPE raw4.

  IF p_edit EQ abap_true.
    status = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    status = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  wa_celltab-fieldname = 'FILIAL_RESP'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'QTD_VINC'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'COD_TRANSPORTADORA'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PRECO_FRETE'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PLACA_CAV'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PLACA_CAR1'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PLACA_CAR2'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PLACA_CAR3'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'MOTORISTA'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'QTD_EMBARQ'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'NFENUM'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'SERIES'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'NETWR'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'DOCDAT_NF'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.

ENDFORM.                               " FILL_CELLTAB

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR_F4_5420
*&---------------------------------------------------------------------*
FORM registrar_f4_5420 .

  DATA: gs_f4      TYPE lvc_s_f4,
        gt_f4_5420 TYPE lvc_t_f4.

  gs_f4-fieldname  = 'FILIAL_RESP'.
  gs_f4-register   = 'X'.
  gs_f4-getbefore  = space.
  gs_f4-chngeafter = space.
  APPEND gs_f4 TO gt_f4_5420.

  CALL METHOD ctl_alv2_5420->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_5420.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR_F4_5420
*&---------------------------------------------------------------------*
FORM registrar_f4_pedido_5420 .

  DATA: gs_f4      TYPE lvc_s_f4,
        gt_f4_5420 TYPE lvc_t_f4.

  gs_f4-fieldname  = 'EBELN'.
  gs_f4-register   = 'X'.
  gs_f4-getbefore  = space.
  gs_f4-chngeafter = space.
  APPEND gs_f4 TO gt_f4_5420.

  CALL METHOD ctl_alv3_5420->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_5420.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVA_ROMANEIOS
*&---------------------------------------------------------------------*
FORM salva_romaneios_5420 CHANGING vl_check_rom.

  DATA: it_zsdt0040      TYPE STANDARD TABLE OF zsdt0040,
        it_zsdt0041      TYPE STANDARD TABLE OF zsdt0041,
        it_zsdt0090      TYPE STANDARD TABLE OF zsdt0090,
        it_zsdt0136      TYPE STANDARD TABLE OF zsdt0136,
        it_zsdt0001      TYPE STANDARD TABLE OF zsdt0001,
        it_zsdt0082      TYPE STANDARD TABLE OF zsdt0082,
        it_vbpa          TYPE STANDARD TABLE OF vbpa,
        it_vbap          TYPE STANDARD TABLE OF vbap,
        it_vbkd          TYPE STANDARD TABLE OF vbkd,
        wa_caminhao_5420 TYPE ty_caminhao_5420,
        wa_zsdt0041      TYPE zsdt0041,
        wa_zsdt0040      TYPE zsdt0040,
        wa_zsdt0090      TYPE zsdt0090,
        wa_zsdt0001      TYPE zsdt0001,
        wa_zsdt0136      TYPE zsdt0136,
        wa_zsdt0082      TYPE zsdt0082,
        wa_vbkd          TYPE vbkd,
        wa_vbpa          TYPE vbpa,
        wa_vbap          TYPE vbap,
        chave            TYPE char8,
        vl_doc_simulacao TYPE zsdt0041-doc_simulacao,
        vl_safra         TYPE zsdt0040-safra,
        it_zsdt0138      TYPE STANDARD TABLE OF zsdt0138,
        wa_zsdt0138      TYPE zsdt0138,
        vl_gravou        TYPE char1.


  SELECT *
    FROM zsdt0082
    INTO CORRESPONDING FIELDS OF TABLE it_zsdt0082
    FOR ALL ENTRIES IN it_caminhao_aux_5420
    WHERE status NE 'X'
      AND nro_sol EQ it_caminhao_aux_5420-nro_sol
      AND seq     EQ it_caminhao_aux_5420-seq.

  IF it_zsdt0082 IS NOT INITIAL.

    SORT it_zsdt0082 BY nro_sol seq ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_zsdt0082 COMPARING nro_sol seq.

    SELECT *
      FROM zsdt0041
      INTO TABLE it_zsdt0041
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE vbeln EQ it_zsdt0082-vbeln.

    SELECT *
      FROM zsdt0090
      INTO TABLE it_zsdt0090
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE vbeln EQ it_zsdt0082-vbeln.

    IF it_zsdt0041 IS NOT INITIAL.

      SELECT *
        FROM zsdt0040
        INTO TABLE it_zsdt0040
        FOR ALL ENTRIES IN it_zsdt0041
        WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.

    ENDIF.

    IF it_zsdt0090 IS NOT INITIAL.

      SELECT *
        FROM zsdt0040
        APPENDING TABLE it_zsdt0040
        FOR ALL ENTRIES IN it_zsdt0090
        WHERE doc_simulacao EQ it_zsdt0090-doc_simulacao.

    ENDIF.

    SELECT *
      FROM zsdt0136
      INTO TABLE it_zsdt0136
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE werks EQ it_zsdt0082-werks.

  ENDIF.

  LOOP AT it_zsdt0082 INTO wa_zsdt0082.
    READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_zsdt0082-vbeln.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE it_zsdt0090 INTO wa_zsdt0090 WITH KEY vbeln = wa_zsdt0082-vbeln.
      IF sy-subrc IS INITIAL.
        vl_doc_simulacao = wa_zsdt0090-doc_simulacao.
      ENDIF.
    ELSE.
      vl_doc_simulacao = wa_zsdt0041-doc_simulacao.
    ENDIF.

    IF vl_doc_simulacao IS NOT INITIAL.
      READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = vl_doc_simulacao.
      IF sy-subrc IS INITIAL.
        vl_safra = wa_zsdt0040-safra.
      ENDIF.
    ENDIF.

    READ TABLE it_zsdt0136 WITH KEY werks = wa_zsdt0082-werks
                                    safra = vl_safra TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE TEXT-055 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check_rom = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF vl_check_rom IS INITIAL.

    CREATE OBJECT zcl_romaneio.

    SELECT *
       FROM vbpa
       INTO TABLE it_vbpa
       FOR ALL ENTRIES IN it_zsdt0082
       WHERE vbeln EQ it_zsdt0082-vbeln
         AND ( parvw EQ 'PC' OR
               parvw EQ 'AG' ).

    SELECT *
      FROM vbkd
      INTO TABLE it_vbkd
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE vbeln EQ it_zsdt0082-vbeln.

    SELECT *
      FROM vbap
      INTO TABLE it_vbap
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE vbeln EQ it_zsdt0082-vbeln
        AND posnr EQ it_zsdt0082-posnr.

    LOOP AT it_caminhao_aux_5420 INTO wa_caminhao_5420.

      wa_zsdt0001-tp_movimento = 'S'.

      READ TABLE it_zsdt0082 INTO wa_zsdt0082 WITH KEY nro_sol = wa_caminhao_5420-nro_sol
                                                       seq     = wa_caminhao_5420-seq.
      IF sy-subrc IS INITIAL.
        wa_zsdt0001-vbeln = wa_zsdt0082-vbeln.
        wa_zsdt0001-bukrs = wa_zsdt0082-vkorg.
        wa_zsdt0001-branch = wa_zsdt0082-werks.

        READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_zsdt0082-vbeln.
        IF sy-subrc IS INITIAL.
          IF wa_vbkd-inco1 EQ 'FOB'.
            wa_zsdt0001-tp_frete = 'F'.
          ELSE.
            wa_zsdt0001-tp_frete = 'C'.
          ENDIF.
        ENDIF.

        READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_zsdt0082-vbeln
                                                       parvw = 'PC'.
        IF sy-subrc IS INITIAL.
          wa_zsdt0001-parid = wa_vbpa-lifnr.
        ENDIF.

        READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_zsdt0082-vbeln
                                                 parvw = 'AG'.
        IF sy-subrc IS INITIAL.
          wa_zsdt0001-id_cli_dest = wa_vbpa-kunnr.
        ENDIF.

        READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_zsdt0082-vbeln
                                                 posnr = wa_zsdt0082-posnr.
        IF sy-subrc IS INITIAL.
          wa_zsdt0001-matnr = wa_vbap-matnr.
        ENDIF.

        READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_zsdt0082-vbeln.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE it_zsdt0090 INTO wa_zsdt0090 WITH KEY vbelv = wa_zsdt0082-vbeln.
          IF sy-subrc IS INITIAL.
            vl_doc_simulacao = wa_zsdt0090-doc_simulacao.
          ENDIF.
        ELSE.
          vl_doc_simulacao = wa_zsdt0041-doc_simulacao.
        ENDIF.

        IF vl_doc_simulacao IS NOT INITIAL.
          READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = vl_doc_simulacao.
          IF sy-subrc IS INITIAL.
            vl_safra = wa_zsdt0040-safra.
          ENDIF.
        ENDIF.

      ENDIF.

      wa_zsdt0001-dt_movimento = sy-datum.
      wa_zsdt0001-peso_liq = wa_caminhao_5420-qtd_embarq.
      wa_zsdt0001-peso_fiscal = wa_caminhao_5420-qtd_embarq.
      wa_zsdt0001-peso_subtotal = wa_caminhao_5420-qtd_embarq. "IR096917 - 01.06.2012 - RMNI
      wa_zsdt0001-placa_cav = wa_caminhao_5420-placa_cav.
      wa_zsdt0001-placa_car1 = wa_caminhao_5420-placa_car1.
      wa_zsdt0001-placa_car2 = wa_caminhao_5420-placa_car2.
      wa_zsdt0001-placa_car3 = wa_caminhao_5420-placa_car3.
      wa_zsdt0001-motorista = wa_caminhao_5420-motorista.
      wa_zsdt0001-id_interface = '51'.
      wa_zsdt0001-nr_safra = vl_safra.

      zcl_romaneio->zif_cadastro~novo_registro( ).
      zcl_romaneio->set_cabecalho_romaneio( i_romaneio = wa_zsdt0001 ).

      TRY.
          zcl_romaneio->zif_cadastro~gravar_registro( RECEIVING i_gravou = vl_gravou ).
        CATCH zcx_cadastro INTO zcx_cadastro.
          zcx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

      IF vl_gravou IS INITIAL.
        MOVE abap_true TO vl_check_rom.
      ELSE.
*-------CS2019001896 - 05.01.2021 - inicio
        SELECT SINGLE *
              FROM zsdt0138
              INTO wa_zsdt0138
              WHERE seq_cam     EQ wa_caminhao_5420-seq_cam
                AND nro_sol     EQ wa_caminhao_5420-nro_sol
                AND seq         EQ wa_caminhao_5420-seq
                AND filial_resp EQ wa_caminhao_5420-filial_resp.

        IF sy-subrc = 0.
          zcl_romaneio->get_registro( IMPORTING e_registro = wa_zsdt0001 ).
          wa_zsdt0138-ch_referencia = wa_zsdt0001-ch_referencia.
          wa_zsdt0138-status = wa_zsdt0138-status + 1.
          MODIFY zsdt0138 FROM wa_zsdt0138.
        ELSE.
          SELECT SINGLE *
                FROM zsdt0138
                INTO wa_zsdt0138
                WHERE seq_cam     EQ wa_caminhao_5420-seq_cam
                  AND nro_sol     EQ wa_caminhao_5420-nro_sol
                  AND seq         EQ wa_caminhao_5420-seq
*-CS2019001891 - 28.06.2021 - JT - inicio
*                 AND filial_resp EQ g_filial_resp.
                  AND filial_resp EQ wa_caminhao_5420-filial_resp. "'TCOR'.
*-CS2019001891 - 28.06.2021 - JT - fim

          IF sy-subrc = 0.
            zcl_romaneio->get_registro( IMPORTING e_registro = wa_zsdt0001 ).
            wa_zsdt0138-ch_referencia = wa_zsdt0001-ch_referencia.
            wa_zsdt0138-status = wa_zsdt0138-status + 1.
            MODIFY zsdt0138 FROM wa_zsdt0138.
          ENDIF.
        ENDIF.
*-------CS2019001896 - 05.01.2021 - fim
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.

*-CS2019001896 - 05.01.2021 - inicio
*&---------------------------------------------------------------------*
*&      Form  GRAVAR TABELA TRANSPORTE
** Este FOEM é chamado no metodo GRAVAR_ROMANEIO_FERTILIZANTE
*  da classe ZCL_NFE_INBOUND
*&---------------------------------------------------------------------*
FORM grava_tabela_caminhoes USING p_nro_sol
                                  p_seq
                                  p_seq_cam
                                  p_filial_resp
                                  p_qte_embarq
                                  p_placa_cav.
*                                 p_filial_resp2.

  DATA: wa_caminhao_aux_5420 TYPE ty_caminhao_5420.

* g_filial_resp                    = p_filial_resp2.

  FREE: it_caminhao_aux_5420.

  wa_caminhao_aux_5420-nro_sol     = p_nro_sol.
  wa_caminhao_aux_5420-seq         = p_seq.
  wa_caminhao_aux_5420-seq_cam     = p_seq_cam.
  wa_caminhao_aux_5420-filial_resp = p_filial_resp.
  wa_caminhao_aux_5420-qtd_embarq  = p_qte_embarq.
  wa_caminhao_aux_5420-placa_cav   = p_placa_cav.

  APPEND wa_caminhao_aux_5420     TO it_caminhao_aux_5420.

ENDFORM.
*-CS2019001896 - 05.01.2021 - fim
.
*&---------------------------------------------------------------------*
*&      Form  DELETE_ROMANEIOS_5620
*&---------------------------------------------------------------------*
FORM delete_romaneios_5420 .

  DATA: vl_check    TYPE char1,
        it_zsdt0138 TYPE STANDARD TABLE OF zsdt0138,
        wa_zsdt0138 TYPE zsdt0138,
        it_zsdt0001 TYPE STANDARD TABLE OF zsdt0001,
        wa_zsdt0001 TYPE zsdt0001.

  SELECT *
    FROM zsdt0001
    INTO CORRESPONDING FIELDS OF TABLE it_zsdt0001
    FOR ALL ENTRIES IN it_caminhao_aux_5420
    WHERE ch_referencia EQ it_caminhao_aux_5420-ch_referencia.

  "Check se há algum romaneio com
  LOOP AT it_zsdt0001 INTO wa_zsdt0001.
    IF wa_zsdt0001-doc_rem IS NOT INITIAL.
      vl_check = abap_true.
      MESSAGE s000(z_fi) WITH 'Romaneio' wa_zsdt0001-ch_referencia 'já possuí remessa' DISPLAY LIKE 'E'.
    ENDIF.
  ENDLOOP.

  IF vl_check IS INITIAL.

    LOOP AT it_zsdt0001 INTO wa_zsdt0001.

      SELECT SINGLE *
        FROM zsdt0138
        INTO wa_zsdt0138
        WHERE ch_referencia EQ wa_zsdt0001-ch_referencia.

      CLEAR: wa_zsdt0138-ch_referencia.
      wa_zsdt0138-status = wa_zsdt0138-status - 1.
      MODIFY zsdt0138 FROM wa_zsdt0138.
      DELETE zsdt0001 FROM wa_zsdt0001.
    ENDLOOP.

    MESSAGE TEXT-059 TYPE 'S'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_BOTOES_5820
*&---------------------------------------------------------------------*
FORM excluir_botoes_frete_5420 CHANGING p_it_exclude TYPE ui_functions.

  DATA: ls_exclude     TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_excl_all.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_FILIAL_5420
*&---------------------------------------------------------------------*
FORM f4_filial_5420 USING: er_event_data TYPE REF TO cl_alv_event_data
                              es_row_no TYPE lvc_s_roid.

  DATA: it_ret_5420 TYPE STANDARD TABLE OF ddshretval,
        it_f4_5420  TYPE STANDARD TABLE OF ty_f4_filial_frete,
        it_tvkbt    TYPE STANDARD TABLE OF tvkbt,
        wa_tvkbt    TYPE tvkbt.

  DATA: wa_f4_5420    TYPE ty_f4_filial_frete,
        wa_frete_5420 TYPE ty_frete_5420,
        wa_ret        TYPE ddshretval,
        wa_modi       TYPE lvc_s_modi.

  FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

  DATA : it_fmap TYPE STANDARD TABLE OF dselc,
         wa_fmap TYPE dselc.

  READ TABLE it_frete_5420 INTO wa_frete_5420 INDEX es_row_no-row_id.
  IF sy-subrc IS INITIAL AND wa_frete_5420-edit EQ abap_true.

    wa_fmap-fldname = 'F0001'.
    wa_fmap-dyfldname = 'FILIAL_RESP'.
    APPEND wa_fmap TO it_fmap.
    wa_fmap-fldname = 'F0002'.
    wa_fmap-dyfldname = 'BEZEI'.
    APPEND wa_fmap TO it_fmap.

*-CS2019001891 - 28.06.2021 - JT - inicio
    SELECT *
      FROM zsdt0282
      INTO TABLE @DATA(t_0282)
     WHERE vkorg IN @x_vkorg.

    LOOP AT t_0282        INTO DATA(w_0282).
      wa_f4_5420-filial_resp = w_0282-transp.
      wa_f4_5420-bezei       = w_0282-nome.
      APPEND wa_f4_5420     TO it_f4_5420.
      CLEAR: wa_f4_5420.
    ENDLOOP.

*   SELECT *
*     FROM tvkbt
*     INTO TABLE it_tvkbt.
*
*   LOOP AT it_tvkbt INTO wa_tvkbt.
*     IF wa_tvkbt-vkbur(1) = 'T'.
*       wa_f4_5420-filial_resp = wa_tvkbt-vkbur.
*       wa_f4_5420-bezei = wa_tvkbt-bezei.
*       APPEND wa_f4_5420 TO it_f4_5420.
*       CLEAR: wa_f4_5420.
*     ENDIF.
*   ENDLOOP.
*-CS2019001891 - 28.06.2021 - JT - fim

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'FILIAL_RESP'
        window_title    = 'Lista de Pedidos'(002)
        value_org       = 'S'
        dynprofield     = 'FILIAL_RESP'
      TABLES
        value_tab       = it_f4_5420
        return_tab      = it_ret_5420
        dynpfld_mapping = it_fmap
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc = 0.
      ASSIGN er_event_data->m_data->* TO <itab>.
      READ TABLE it_ret_5420 INTO wa_ret INDEX 1.
      wa_modi-row_id   = es_row_no-row_id.
      wa_modi-fieldname = 'FILIAL_RESP'.
      wa_modi-value     = wa_ret-fieldval.
      APPEND wa_modi TO <itab>.
      READ TABLE it_ret_5420 INTO wa_ret INDEX 2.
      wa_modi-fieldname = 'BEZEI'.
      wa_modi-value     = wa_ret-fieldval.
      APPEND wa_modi TO <itab>.
    ENDIF.

    er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_PEDIDO_5420
*&---------------------------------------------------------------------*
FORM f4_pedido_5420 USING: er_event_data TYPE REF TO cl_alv_event_data
                              es_row_no TYPE lvc_s_roid.

  DATA: it_ret_5420      TYPE STANDARD TABLE OF ddshretval,
        it_f4_5420       TYPE STANDARD TABLE OF ty_f4_pedido,
        wa_caminhao_5420 TYPE ty_caminhao_5420.

  DATA: wa_ret  TYPE ddshretval,
        wa_modi TYPE lvc_s_modi.

  FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

  DATA : it_fmap TYPE STANDARD TABLE OF dselc,
         wa_fmap TYPE dselc.

  DATA: lv_ebeln TYPE  ebeln,
        lv_ebelp TYPE  ebelp.

  READ TABLE it_caminhao_5420 INTO wa_caminhao_5420 INDEX es_row_no-row_id.
  IF sy-subrc IS INITIAL AND wa_caminhao_5420-edit EQ abap_true.

    SELECT COUNT(*)
      FROM zsdt0137
      INTO @DATA(lv_cont)
      WHERE nro_sol      EQ @vg_sol_5420-nro_sol
        AND matnr        EQ @vg_sol_5420-matnr
        AND seq          EQ @vg_sol_5420-seq
*-----CS2019001891 - 08.03.2021 - JT - inicio
        AND filial_resp  EQ @vg_frete_5420-filial_resp
        AND ped_imp      EQ 'X'.
*-----CS2019001891 - 08.03.2021 - JT - fim

    IF lv_cont = 0.
      wa_fmap-fldname = 'F0001'.
      wa_fmap-dyfldname = 'EBELN'.
      APPEND wa_fmap TO it_fmap.
      wa_fmap-fldname = 'F0002'.
      wa_fmap-dyfldname = 'EBELP'.
      APPEND wa_fmap TO it_fmap.

      SELECT *
        FROM zsdt0062
        INTO CORRESPONDING FIELDS OF TABLE it_f4_5420
        WHERE vbeln   EQ vg_sol_5420-vbeln
          AND matnr   EQ vg_sol_5420-matnr
          AND nro_sol EQ vg_sol_5420-nro_sol
          AND seq     EQ vg_sol_5420-seq
          AND status  NE 'E'.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'EBELN'
          window_title    = 'Lista de Filiais'(002)
          value_org       = 'S'
          dynprofield     = 'EBELN'
        TABLES
          value_tab       = it_f4_5420
          return_tab      = it_ret_5420
          dynpfld_mapping = it_fmap
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.

      IF sy-subrc = 0.
        ASSIGN er_event_data->m_data->* TO <itab>.
        READ TABLE it_ret_5420 INTO wa_ret INDEX 1.
        wa_modi-row_id   = es_row_no-row_id.
        wa_modi-fieldname = 'EBELN'.
        wa_modi-value     = wa_ret-fieldval.
        APPEND wa_modi TO <itab>.
        READ TABLE it_ret_5420 INTO wa_ret INDEX 2.
        wa_modi-fieldname = 'EBELP'.
        wa_modi-value     = wa_ret-fieldval.
        APPEND wa_modi TO <itab>.
      ENDIF.

      er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)

    ELSE.

      CALL FUNCTION 'ZSD_PEDIDOS_IMPORTACAO'
        EXPORTING
          i_nro_sol     = vg_frete_5420-nro_sol
          i_seq         = vg_frete_5420-seq
          i_filial_resp = vg_frete_5420-filial_resp
          i_selecao     = 'X'
        IMPORTING
          e_ebeln       = lv_ebeln
          e_ebelp       = lv_ebelp.

      IF lv_ebeln IS NOT INITIAL.
        ASSIGN er_event_data->m_data->* TO <itab>.
        wa_modi-row_id   = es_row_no-row_id.
        wa_modi-fieldname = 'EBELN'.
        wa_modi-value     = lv_ebeln.
        APPEND wa_modi TO <itab>.
        wa_modi-fieldname = 'EBELP'.
        wa_modi-value     = lv_ebelp.
        APPEND wa_modi TO <itab>.
      ENDIF.

      er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REFRESH_CTL_ALV2_5420
*&---------------------------------------------------------------------*
FORM refresh_ctl_alv2_5420 .

  DATA: wa_stable TYPE lvc_s_stbl.

  wa_stable-row = 'X'.
  wa_stable-col = 'X'.

  "gs_layout_5420_alv2-info_fname = 'C510'.

  CALL METHOD ctl_alv2_5420->get_frontend_layout
    IMPORTING
      es_layout = gs_layout_5420_alv2.

  gs_layout_5420_alv2-cwidth_opt = abap_true.

  CALL METHOD ctl_alv2_5420->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_5420_alv2.

  CALL METHOD ctl_alv2_5420->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REFRESH_CTL_ALV3_5420
*&---------------------------------------------------------------------*
FORM refresh_ctl_alv3_5420 .

  DATA: wa_stable TYPE lvc_s_stbl.

  wa_stable-row = 'X'.
  wa_stable-col = 'X'.

  CALL METHOD ctl_alv3_5420->get_frontend_layout
    IMPORTING
      es_layout = gs_layout_5420_alv3.

  gs_layout_5420_alv3-cwidth_opt = abap_true.

  CALL METHOD ctl_alv3_5420->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_5420_alv3.

  CALL METHOD ctl_alv3_5420->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REFRESH_CTL_ALV1_5420
*&---------------------------------------------------------------------*
FORM refresh_ctl_alv1_5420 .

  DATA: wa_stable TYPE lvc_s_stbl.

  wa_stable-row = 'X'.
  wa_stable-col = 'X'.

  CALL METHOD ctl_alv1_5420->get_frontend_layout
    IMPORTING
      es_layout = gs_layout_5420_alv1.

  gs_layout_5420_alv1-cwidth_opt = abap_true.

  CALL METHOD ctl_alv1_5420->set_frontend_layout
    EXPORTING
      is_layout = gs_layout_5420_alv1.

  CALL METHOD ctl_alv1_5420->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SALVA_FRETE_5420
*&---------------------------------------------------------------------*
FORM salva_frete_5420.

  DATA: it_zsdt0138       TYPE STANDARD TABLE OF zsdt0138,
        it_zsdt0138_aux   TYPE STANDARD TABLE OF zsdt0138,
        wa_zsdt0138       TYPE zsdt0138,
        wa_zsdt0138_aux   TYPE zsdt0138,
        it_zsdt0137       TYPE STANDARD TABLE OF zsdt0137,
        wa_zsdt0137       TYPE zsdt0137,
*        WA_ZSDT0082       TYPE ZSDT0082,
        vl_qtd_vinc_total TYPE zsdt0131-qtd_vinc,
        vl_qtd_vinc_camin TYPE zsdt0138-qtd_embarq,
        vl_qtd_vinc_frete TYPE zsdt0131-qtd_vinc,
        vl_filial_resp    TYPE zsdt0137-filial_resp,
        vl_check_initial  TYPE char1,
        wa_frete_5420     TYPE ty_frete_5420,
        wa_sol_5420       TYPE ty_sol_5420,
        wa_row            TYPE lvc_s_row.

  CLEAR: vl_check_initial.

  IF vg_sol_5420 IS NOT INITIAL.

    LOOP AT it_frete_5420 INTO wa_frete_5420.
      DATA(l_tabix1) = sy-tabix.
      READ TABLE t_zsdt0282_tot INTO DATA(w_282) WITH KEY transp = wa_frete_5420-filial_resp
                                                          vkorg  = vg_sol_5420-vkorg.
      IF sy-subrc = 0.
        wa_frete_5420-bezei     = w_282-nome.
        MODIFY it_frete_5420 FROM wa_frete_5420 INDEX l_tabix1.
      ENDIF.
    ENDLOOP.

    "Check se não está distribuindo mais que o permitido / Check se não está salvando linhas em branco
    LOOP AT it_frete_5420 INTO wa_frete_5420.
*-CS2019001891 - 28.06.2021 - JT - inicio
      IF wa_frete_5420-filial_resp IS INITIAL OR
         wa_frete_5420-qtd_vinc IS INITIAL.
        vl_check_initial = abap_true.
        MESSAGE TEXT-053 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        READ TABLE t_zsdt0282_tot INTO DATA(w_0282) WITH KEY transp = wa_frete_5420-filial_resp.
        IF sy-subrc <> 0.
          vl_check_initial = abap_true.
          MESSAGE TEXT-066 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.

*     IF   wa_frete_5420-filial_resp IS INITIAL OR
*        wa_frete_5420-qtd_vinc IS INITIAL.
*       vl_check_initial = abap_true.
*       MESSAGE text-053 TYPE 'S' DISPLAY LIKE 'E'.
*     ELSEIF wa_frete_5420-filial_resp NE 'TCOR' AND
*               wa_frete_5420-filial_resp NE 'TPGA' AND
*               wa_frete_5420-filial_resp NE 'TROO'.
*       vl_check_initial = abap_true.
*       MESSAGE text-066 TYPE 'S' DISPLAY LIKE 'E'.
*     ENDIF.
*-CS2019001891 - 28.06.2021 - JT - fim

*INICIO - AS -  02/10/2020 - CS2020001072
*      IF vg_sol_5420-inco1 NE 'CFR'.
*        IF wa_frete_5420-filial_resp EQ 'TCOR'.
*          vl_check_initial = abap_true.
*          MESSAGE text-066 TYPE 'S' DISPLAY LIKE 'E'.
*        ENDIF.
*      ELSEIF vg_sol_5420-inco1 EQ 'CFR'.
*        IF wa_frete_5420-filial_resp NE 'TCOR'.
*          vl_check_initial = abap_true.
*          MESSAGE text-066 TYPE 'S' DISPLAY LIKE 'E'.
*        ENDIF.
*      ENDIF.
*FIM - AS -  02/10/2020 - CS2020001072
      vl_qtd_vinc_total = vl_qtd_vinc_total + wa_frete_5420-qtd_vinc.
    ENDLOOP.

    IF vl_qtd_vinc_total GT vg_sol_5420-qte_lib.
      vl_check_initial = abap_true.
      MESSAGE TEXT-048 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    CLEAR: vl_filial_resp.
    "Check se há repetição de filiais frete
    SORT it_frete_5420 BY filial_resp.
    LOOP AT it_frete_5420 INTO wa_frete_5420.
      IF wa_frete_5420-filial_resp EQ vl_filial_resp.
        vl_check_initial = abap_true.
        MESSAGE TEXT-089 TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      vl_filial_resp = wa_frete_5420-filial_resp.
    ENDLOOP.

    "Check se tem caminhão e não tem distribuição para filial
    IF vl_check_initial IS INITIAL.

      SELECT *
        FROM zsdt0138
        INTO TABLE it_zsdt0138
       WHERE nro_sol EQ vg_sol_5420-nro_sol
         AND seq     EQ vg_sol_5420-seq
         AND status  NE 'X'.

      it_zsdt0138_aux = it_zsdt0138.

      SORT it_zsdt0138 BY filial_resp ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_zsdt0138 COMPARING filial_resp.

      LOOP AT it_zsdt0138 INTO wa_zsdt0138.

        "Quantidade de Caminhões
        LOOP AT  it_zsdt0138_aux INTO wa_zsdt0138_aux WHERE filial_resp EQ wa_zsdt0138-filial_resp.
          vl_qtd_vinc_camin = vl_qtd_vinc_camin + wa_zsdt0138_aux-qtd_embarq.
        ENDLOOP.

        "Quantidade distribuida para filial
        LOOP AT it_frete_5420 INTO wa_frete_5420 WHERE filial_resp EQ wa_zsdt0138-filial_resp.
          vl_qtd_vinc_frete = vl_qtd_vinc_frete + wa_frete_5420-qtd_vinc.
        ENDLOOP.

        IF vl_qtd_vinc_camin GT vl_qtd_vinc_frete.
          MESSAGE TEXT-085 TYPE 'S' DISPLAY LIKE 'E'.
          vl_check_initial = abap_true.
        ENDIF.

        CLEAR: vl_qtd_vinc_camin, vl_qtd_vinc_frete.

      ENDLOOP.

    ENDIF.

    IF vl_check_initial NE abap_true.

      "Atualizando a tabela de Solicitação
      CLEAR: wa_zsdt0137. "WA_ZSDT0082.
      FREE: it_zsdt0137.
      SELECT *
        FROM zsdt0137
        INTO TABLE it_zsdt0137
        WHERE nro_sol EQ vg_sol_5420-nro_sol
          AND seq     EQ vg_sol_5420-seq.

      LOOP AT it_frete_5420 INTO wa_frete_5420 WHERE edit EQ abap_true.

        DATA(l_tabix) = sy-tabix.

*-CS2019001896 - 17.08.2021 - JT - inicio
        READ TABLE it_zsdt0137 INTO DATA(w_137) WITH KEY nro_sol     = wa_frete_5420-nro_sol
                                                         seq         = wa_frete_5420-seq
                                                         filial_resp = wa_frete_5420-filial_resp.
        IF sy-subrc = 0.
          DATA(l_pedido_imp) = w_137-ped_imp.
        ENDIF.
*-CS2019001896 - 17.08.2021 - JT - fim

        wa_zsdt0137-nro_sol     = vg_sol_5420-nro_sol.
        wa_zsdt0137-seq         = vg_sol_5420-seq.
        wa_zsdt0137-matnr       = vg_sol_5420-matnr.
        wa_zsdt0137-qtd_vinc    = wa_frete_5420-qtd_vinc.
        wa_zsdt0137-um          = wa_frete_5420-um.
        wa_zsdt0137-filial_resp = wa_frete_5420-filial_resp.
        wa_zsdt0137-status      = '1'.
*-CS2019001896 - 17.08.2021 - JT - inicio
        wa_zsdt0137-ped_imp     = l_pedido_imp.
*       wa_zsdt0137-ped_imp     = abap_true. "BUG 60282 - IR064717 - AOENNING.
*-CS2019001896 - 17.08.2021 - JT - fim
        wa_zsdt0137-usnam       = sy-uname.
        wa_zsdt0137-data_atual  = sy-datum.
        wa_zsdt0137-hora_atual  = sy-uzeit.
        MODIFY zsdt0137 FROM wa_zsdt0137.
        COMMIT WORK.
        wa_frete_5420-edit = abap_false.
        MODIFY it_frete_5420 FROM wa_frete_5420 INDEX l_tabix. "sy-tabix.
        CLEAR: wa_zsdt0137.
      ENDLOOP.


      LOOP AT it_zsdt0137 INTO wa_zsdt0137.
        READ TABLE it_frete_5420 INTO wa_frete_5420 WITH KEY nro_sol         = wa_zsdt0137-nro_sol
                                                                 seq         = wa_zsdt0137-seq
                                                                 filial_resp = wa_zsdt0137-filial_resp.
        IF sy-subrc IS NOT INITIAL.
          wa_zsdt0137-status = 'X'.
          wa_zsdt0137-user_canc = sy-uname.
          wa_zsdt0137-dt_canc = sy-datum.
          wa_zsdt0137-hr_can = sy-uzeit.
          MODIFY zsdt0137 FROM wa_zsdt0137.
          COMMIT WORK.
        ENDIF.
*          "Atualiza o status da solicitação de entrega
*          "------------------------------------------------
*          SELECT SINGLE *
*            FROM ZSDT0082
*            INTO WA_ZSDT0082
*            WHERE NRO_SOL = VG_SOL_5420-NRO_SOL
*              AND SEQ     = VG_SOL_5420-SEQ
*              AND VBELN   = VG_SOL_5420-VBELN
*              AND POSNR   = VG_SOL_5420-POSNR.
*
*          IF SY-SUBRC IS INITIAL.
*            WA_ZSDT0082-STATUS = '2'. "volta para "Liberada"
*          ENDIF.
*        ELSE.
*
*          SELECT SINGLE *
*            FROM ZSDT0082
*            INTO WA_ZSDT0082
*            WHERE NRO_SOL = VG_SOL_5420-NRO_SOL
*              AND SEQ     = VG_SOL_5420-SEQ
*              AND VBELN   = VG_SOL_5420-VBELN
*              AND POSNR   = VG_SOL_5420-POSNR.
*
*          IF SY-SUBRC IS INITIAL.
*            WA_ZSDT0082-STATUS = '5'.  "Define para "Planejado/Produzido"
*          ENDIF.
*        ENDIF.
*        MODIFY ZSDT0082 FROM WA_ZSDT0082.
*        "------------------------------------------------
      ENDLOOP.

      CLEAR: vl_qtd_vinc_total.

      LOOP AT it_sol_5420 INTO wa_sol_5420 WHERE nro_sol EQ vg_sol_5420-nro_sol
                                                 AND seq     EQ vg_sol_5420-seq.
        wa_row-index = sy-tabix.
      ENDLOOP.

*-CS2019001896 - 12.08.2021 - JT - inicio
      g_click_editar = abap_true.
*-CS2019001896 - 12.08.2021 - JT - fim

      CALL METHOD lcl_event_handler_5420=>on_double_click_5420
        EXPORTING
          e_row = wa_row.

      MESSAGE TEXT-049 TYPE 'S'.
      PERFORM refresh_ctl_alv1_5420.

    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SALVA_FRETE_CAM_5420
*&---------------------------------------------------------------------*
FORM salva_frete_cam_5420.

  DATA: wa_caminhao_5420  TYPE ty_caminhao_5420,
        wa_sol_5420       TYPE ty_sol_5420,
        wa_frete_5420     TYPE ty_frete_5420,
        it_zsdt0137       TYPE STANDARD TABLE OF zsdt0137,
        wa_zsdt0137       TYPE zsdt0137,
        it_zsdt0137_aux   TYPE STANDARD TABLE OF zsdt0137,
        wa_zsdt0137_aux   TYPE zsdt0137,
        vl_qtd_vinc_frete TYPE zsdt0131-qtd_vinc,
        vl_qtd_vinc_camin TYPE zsdt0138-qtd_embarq,
        vl_qtd_vinc_total TYPE zsdt0131-qtd_vinc,
        vl_check_initial  TYPE char1,
        vl_seq_cam        TYPE zsdt0138-seq_cam,
        it_zsdt0138       TYPE STANDARD TABLE OF zsdt0138,
        wa_zsdt0138       TYPE zsdt0138,
        wa_row            TYPE lvc_s_row.

  CLEAR: vl_check_initial, wa_row.

  IF vg_frete_5420 IS NOT INITIAL. "AND vg_frete_5420-inco1 EQ 'CFR'.
    "Check se tem embarcado mais que o permitido / Check se não está salvando linhas em branco
    LOOP AT it_caminhao_5420 INTO wa_caminhao_5420.
      IF  wa_caminhao_5420-edit EQ 'X' AND ( wa_caminhao_5420-ebeln              IS INITIAL OR
         wa_caminhao_5420-cod_transportadora IS INITIAL OR  "comentando conforme informado nos testes do CS2018002183 -- descomentado conforme solicitado pela Arianne 25.06.2021
*         WA_CAMINHAO_5420-PRECO_FRETE        IS INITIAL OR
         wa_caminhao_5420-qtd_embarq         IS INITIAL ).
        vl_check_initial = abap_true.
        MESSAGE TEXT-053 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ENDLOOP.

    "Check se tem caminhão e não tem distribuição para filial
    IF vl_check_initial IS INITIAL.

      SELECT *
        FROM zsdt0137
        INTO TABLE it_zsdt0137
       WHERE nro_sol     EQ vg_frete_5420-nro_sol
         AND seq         EQ vg_frete_5420-seq
         AND filial_resp EQ vg_frete_5420-filial_resp
         AND status  NE 'X'.

      it_zsdt0137_aux = it_zsdt0137.
* ---> S4 Migration - 04/07/2023 - FTM - Início
      SORT: it_zsdt0137 BY filial_resp.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
      DELETE ADJACENT DUPLICATES FROM it_zsdt0137 COMPARING filial_resp.

      LOOP AT it_zsdt0137 INTO wa_zsdt0137.

        CLEAR: vl_qtd_vinc_camin, vl_qtd_vinc_frete.

        "Quantidade de Caminhões
        LOOP AT  it_zsdt0137_aux INTO wa_zsdt0137_aux.
          vl_qtd_vinc_frete = vl_qtd_vinc_frete + wa_zsdt0137_aux-qtd_vinc.
        ENDLOOP.

        "Quantidade caminhoes para filial
        LOOP AT it_caminhao_5420 INTO wa_caminhao_5420.
          vl_qtd_vinc_camin = vl_qtd_vinc_camin + wa_caminhao_5420-qtd_embarq.
        ENDLOOP.

        IF vl_qtd_vinc_camin GT vl_qtd_vinc_frete.
          vl_check_initial = abap_true.
          MESSAGE TEXT-048 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      ENDLOOP.

    ENDIF.

    IF vl_check_initial NE abap_true.

      "Atualizando a tabela de Solicitação
      LOOP AT it_sol_5420 INTO wa_sol_5420 WHERE nro_sol EQ vg_frete_5420-nro_sol AND
                                                     seq EQ vg_frete_5420-seq.
        wa_sol_5420-qtdeb = vl_qtd_vinc_camin.
        MODIFY it_sol_5420 FROM wa_sol_5420 INDEX sy-tabix.
      ENDLOOP.

      SELECT MAX( seq_cam )
        FROM zsdt0138
        INTO vl_seq_cam
        WHERE filial_resp EQ vg_frete_5420-filial_resp
          AND seq         EQ vg_frete_5420-seq
          AND nro_sol     EQ vg_frete_5420-nro_sol.

      CLEAR: wa_zsdt0138.

      LOOP AT it_caminhao_5420 INTO wa_caminhao_5420 WHERE edit EQ abap_true.

        IF wa_caminhao_5420-seq_cam IS INITIAL.
          wa_zsdt0138-seq_cam          = vl_seq_cam + 1.
          wa_caminhao_5420-seq_cam     = vl_seq_cam + 1.
          vl_seq_cam                   = vl_seq_cam + 1.
        ELSE.
          wa_zsdt0138-seq_cam          = wa_caminhao_5420-seq_cam.
        ENDIF.

        wa_zsdt0138-nro_sol            = vg_frete_5420-nro_sol.
        wa_zsdt0138-seq                = vg_frete_5420-seq.
        wa_zsdt0138-filial_resp        = vg_frete_5420-filial_resp.
        wa_zsdt0138-aviso_receb        = wa_frete_5420-qtd_vinc.
        wa_zsdt0138-ebeln              = wa_caminhao_5420-ebeln.
        wa_zsdt0138-ebelp              = wa_caminhao_5420-ebelp.
        wa_zsdt0138-cod_transportadora = wa_caminhao_5420-cod_transportadora.
        wa_zsdt0138-preco_frete        = wa_caminhao_5420-preco_frete.
        wa_zsdt0138-placa_cav          = wa_caminhao_5420-placa_cav.
        wa_zsdt0138-placa_car1         = wa_caminhao_5420-placa_car1.
        wa_zsdt0138-placa_car2         = wa_caminhao_5420-placa_car2.
        wa_zsdt0138-placa_car3         = wa_caminhao_5420-placa_car3.
        wa_zsdt0138-motorista          = wa_caminhao_5420-motorista.
        wa_zsdt0138-qtd_embarq         = wa_caminhao_5420-qtd_embarq.
        wa_zsdt0138-um                 = wa_caminhao_5420-um.
        wa_zsdt0138-nfenum             = wa_caminhao_5420-nfenum.
        wa_zsdt0138-series             = wa_caminhao_5420-series.
        wa_zsdt0138-netwr              = wa_caminhao_5420-netwr.
        wa_zsdt0138-docdat_nf          = wa_caminhao_5420-docdat_nf.
        wa_zsdt0138-usnam              = sy-uname.
        wa_zsdt0138-status             = '1'.
        wa_zsdt0138-data_atual         = sy-datum.
        wa_zsdt0138-hora_atual         = sy-uzeit.
        MODIFY zsdt0138 FROM wa_zsdt0138.
        wa_caminhao_5420-edit = abap_false.
        MODIFY it_caminhao_5420 FROM wa_caminhao_5420 INDEX sy-tabix.
      ENDLOOP.

      SELECT *
        FROM zsdt0138
        INTO TABLE it_zsdt0138
        WHERE nro_sol     EQ vg_frete_5420-nro_sol
          AND seq         EQ vg_sol_5420-seq
          AND filial_resp EQ vg_frete_5420-filial_resp.

      LOOP AT it_zsdt0138 INTO wa_zsdt0138.
        READ TABLE it_caminhao_5420 INTO wa_caminhao_5420 WITH KEY nro_sol = wa_zsdt0138-nro_sol
                                                                       seq = wa_zsdt0138-seq
                                                                   seq_cam = wa_zsdt0138-seq_cam.
        IF sy-subrc IS NOT INITIAL.
          wa_zsdt0138-status = 'X'.
          wa_zsdt0138-user_canc = sy-uname.
          wa_zsdt0138-dt_canc = sy-datum.
          wa_zsdt0138-hr_can = sy-uzeit.
          MODIFY zsdt0138 FROM wa_zsdt0138.
        ENDIF.

      ENDLOOP.

      CLEAR: vl_qtd_vinc_total.

      LOOP AT it_sol_5420 INTO wa_sol_5420 WHERE nro_sol EQ vg_sol_5420-nro_sol
                                             AND seq     EQ vg_sol_5420-seq.
        wa_row-index = sy-tabix.
      ENDLOOP.

*-CS2019001896 - 12.08.2021 - JT - inicio
      g_click_editar = abap_true.
*-CS2019001896 - 12.08.2021 - JT - fim

      CALL METHOD lcl_event_handler_5420=>on_double_click_5420
        EXPORTING
          e_row = wa_row.

      LOOP AT it_frete_5420 INTO wa_frete_5420 WHERE nro_sol     EQ vg_frete_5420-nro_sol
                                                 AND seq         EQ vg_frete_5420-seq
                                                 AND filial_resp EQ vg_frete_5420-filial_resp.
        wa_row-index = sy-tabix.
      ENDLOOP.

      CALL METHOD lcl_event_handler_5420=>on_double_click_frete_5420
        EXPORTING
          e_row = wa_row.

      MESSAGE TEXT-049 TYPE 'S'.

      CALL METHOD ctl_alv1_5420->refresh_table_display
        EXPORTING
          is_stable = _stable.

      CALL METHOD ctl_alv2_5420->refresh_table_display
        EXPORTING
          is_stable = _stable.

      CALL METHOD ctl_alv3_5420->refresh_table_display
        EXPORTING
          is_stable = _stable.

    ENDIF.

  ENDIF.

ENDFORM.

FORM validar_material USING
                        material
                        st_atividade
                        tipo
                      RAISING
                        cx_abap_util_exception.
  SELECT COUNT(*)
    FROM mara
   WHERE matnr EQ material
     AND spart EQ st_atividade
     AND mtart EQ tipo.

  IF NOT sy-subrc IS INITIAL.
    RAISE EXCEPTION TYPE cx_abap_util_exception.
  ENDIF.
ENDFORM.

FORM validar_dados_vinculacao RAISING cx_abap_util_exception.
  DATA: r_matnr   TYPE RANGE OF mara-matnr,
        w_matnr   LIKE LINE OF r_matnr,
        w_matnr18 TYPE matnr18.
  DATA categoria(1) VALUE 'F'.    "//pedido;
  DATA count          TYPE i.
  DATA it_selected_rows  TYPE lvc_t_row.
  CALL METHOD ctl_alv1_5420->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF ( popup_vinculacao-material IS NOT INITIAL ).

    DATA(_material) = popup_vinculacao-material.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = _material
      IMPORTING
        output = w_matnr18.
  ENDIF.

  _material = w_matnr18.

  IF ( popup_vinculacao-material IS NOT INITIAL AND  popup_vinculacao-pedido IS INITIAL ).

*    DATA(_material) = popup_vinculacao-material.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = _material
*        IMPORTING
*          output = _material.

    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
    READ TABLE it_sol_5420 INTO wa_sol_5420 INDEX wa_selected_rows-index.

    SELECT SINGLE spart INTO @DATA(spart_mat) FROM mara WHERE matnr EQ @_material.


    IF spart_mat EQ wa_sol_5420-spart.

      SELECT COUNT(*)
        FROM ekpo AS a
  INNER JOIN ekko AS b ON b~ebeln EQ a~ebeln
       WHERE a~matnr EQ _material
         AND b~bstyp EQ categoria.

      IF NOT sy-subrc IS INITIAL .
        MESSAGE TEXT-128 TYPE 'I' DISPLAY LIKE 'E'.
        RAISE EXCEPTION TYPE cx_abap_util_exception.
      ENDIF.

    ELSE.
      CONCATENATE 'Setor de atividade ' spart_mat 'do material ' _material 'é diferente do Setor de Atividade' wa_sol_5420-spart '' INTO DATA(mensagem) SEPARATED BY space.
      MESSAGE mensagem TYPE 'S' DISPLAY LIKE 'E'.
      RAISE EXCEPTION TYPE cx_abap_util_exception.
    ENDIF.

  ELSEIF
     ( popup_vinculacao-pedido   IS NOT INITIAL AND  popup_vinculacao-material IS INITIAL ).
    CLEAR: w_matnr.
    FREE r_matnr.
    SELECT 'I' AS sign, 'EQ' AS option, matnr AS low, matnr AS high
    INTO TABLE @DATA(t_mat)
    FROM ekpo WHERE ebeln EQ @popup_vinculacao-pedido.


    SELECT matnr, spart INTO TABLE @DATA(t_spart_mat) FROM mara WHERE matnr IN @t_mat.

    LOOP AT t_spart_mat INTO DATA(w_spart).
      IF w_spart-spart EQ wa_sol_5420-spart.
        w_matnr-sign = 'I'.
        w_matnr-option = 'EQ'.
        w_matnr-low = w_spart-matnr.
        APPEND w_matnr TO r_matnr.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    IF r_matnr[] IS NOT INITIAL.

      SELECT COUNT(*)
        FROM ekpo AS a
  INNER JOIN ekko AS b ON b~ebeln EQ a~ebeln
       WHERE a~matnr IN r_matnr
         AND b~bstyp EQ categoria.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE TEXT-129 TYPE 'S' DISPLAY LIKE 'E'.
        RAISE EXCEPTION TYPE cx_abap_util_exception.
      ENDIF.
    ELSE.
      CONCATENATE 'Setor de atividade dos materiais do pedido são diferentes do Setor de Atividade' wa_sol_5420-spart 'do material da OV'INTO mensagem SEPARATED BY space.
      MESSAGE mensagem TYPE 'S' DISPLAY LIKE 'E'.
      RAISE EXCEPTION TYPE cx_abap_util_exception.
    ENDIF.
  ELSEIF ( popup_vinculacao-material IS NOT INITIAL AND  popup_vinculacao-pedido IS NOT INITIAL ).

*DATA(_material2) = popup_vinculacao-material.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = _material2
*        IMPORTING
*          output = _material2.

    SELECT
      'I' AS sign,
      'EQ' AS option,
      matnr AS low,
      matnr AS high
    INTO TABLE
      @DATA(it_ekpo)
    FROM
      ekpo
   WHERE
      ebeln EQ @popup_vinculacao-pedido
      AND matnr = @_material.

    IF it_ekpo IS NOT INITIAL.
      SELECT matnr, spart INTO TABLE @DATA(t_mat_pedido) FROM mara WHERE matnr IN @it_ekpo.

      LOOP AT t_mat_pedido INTO DATA(w_spart_pedido).
        IF w_spart_pedido-spart EQ wa_sol_5420-spart.
          w_matnr-sign = 'I'.
          w_matnr-option = 'EQ'.
          w_matnr-low = w_spart_pedido-matnr.
          APPEND w_matnr TO r_matnr.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      IF r_matnr[] IS NOT INITIAL.

        SELECT COUNT(*)
          FROM ekpo AS a
    INNER JOIN ekko AS b ON b~ebeln EQ a~ebeln
         WHERE a~matnr IN r_matnr
           AND b~bstyp EQ categoria.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE TEXT-129 TYPE 'S' DISPLAY LIKE 'E'.
          RAISE EXCEPTION TYPE cx_abap_util_exception.
        ENDIF.

      ELSE.
        CONCATENATE 'Setor de atividade ' w_spart_pedido-spart 'do material ' w_spart_pedido-matnr 'é diferente do Setor de Atividade' wa_sol_5420-spart 'do material ' wa_sol_5420-matnr 'da OV.' INTO mensagem SEPARATED BY space..
        MESSAGE mensagem TYPE 'S' DISPLAY LIKE 'E'.
        RAISE EXCEPTION TYPE cx_abap_util_exception.
      ENDIF.
    ELSE.
      CONCATENATE 'Material' _material 'não existe no pedido' popup_vinculacao-pedido INTO mensagem SEPARATED BY space.
      MESSAGE mensagem TYPE 'S' DISPLAY LIKE 'E'.
      RAISE EXCEPTION TYPE cx_abap_util_exception.

    ENDIF.

  ENDIF.

ENDFORM.

FORM call_popup_vinculacao RAISING cx_abap_util_exception.
  "//Call popup to inform the purchasing or material number.
  CALL SCREEN 6002 STARTING AT 35 10 ENDING AT 90 10.

  IF ( sy-ucomm = 'BACK' )
  OR ( sy-ucomm = 'EXIT' ).
    CLEAR popup_vinculacao.
    RAISE EXCEPTION TYPE cx_abap_util_exception.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  PBO_POPUP_VINCULACAO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_popup_vinculacao OUTPUT.
  SET PF-STATUS '6002'.
  SET TITLEBAR '6002'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_POPUP_VINCULACAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_popup_vinculacao INPUT.
  CASE sy-ucomm.
    WHEN 'OK'.

      TRY.
          IF vg_subt_lote EQ '5420'.
            PERFORM validar_dados_vinculacao.
          ELSEIF vg_subt_lote EQ '5230'.
            PERFORM validar_dados_vinculacao_5230.
          ENDIF.
          LEAVE TO SCREEN 0.

        CATCH cx_abap_util_exception.
      ENDTRY.

    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
