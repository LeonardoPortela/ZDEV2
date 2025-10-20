*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5230
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_carga_5230,
         edit        TYPE char1,
         icone       TYPE char6,
         rot_desc    TYPE zsdt0132-rot_desc,
         tval        TYPE zsdt0133-preco_frete,
         desc_transp TYPE lfa1-name1,
         placa_cav   TYPE zsdt0129-placa_cav,
         placa_car1  TYPE zsdt0129-placa_car1,
         placa_car2  TYPE zsdt0129-placa_car2,
         placa_car3  TYPE zsdt0129-placa_car3,
         motorista   TYPE zsdt0129-motorista,
         desc_mot    TYPE lfa1-name1,
         dt_entrega  TYPE zsdt0129-dt_entrega,
         antig       TYPE char1,
         inco1       TYPE zsdt0129-inco1.
         INCLUDE STRUCTURE zsdt0133.
TYPES:   transp_resp TYPE vkbur,
         cellstyles  TYPE lvc_t_styl.
TYPES: END OF ty_carga_5230.

TYPES: BEGIN OF ty_sol_5230,
         nro_cg       TYPE zsdt0133-nro_cg,
         bezei        TYPE tvkbt-bezei,
         seq_ent_cg   TYPE zsdt0130-seq_ent_cg,
         rot_desc     TYPE zsdt0132-rot_desc,
         name1        TYPE kna1-name1,
         maktx        TYPE makt-maktx,
         antig        TYPE char1,
         texto        TYPE char5,
         nr_rot1      TYPE zsdt0130-nr_rot,
         tval         TYPE zsdt0133-preco_frete,
         cod_loc_emb1 TYPE zsdt0131-cod_loc_emb,
         lifnr        TYPE lfa1-lifnr.
         INCLUDE    STRUCTURE zsdt0131.
TYPES:   cellstyles   TYPE lvc_t_styl.
TYPES: END OF ty_sol_5230.

TYPES: BEGIN OF ty_sol_val_5230,
         vbeln TYPE zsdt0131-vbeln,
         posnr TYPE zsdt0131-posnr,
         knumv TYPE vbak-knumv.
TYPES: END OF ty_sol_val_5230.

TYPES: BEGIN OF ty_edit_cg_5230,
         nro_cg TYPE zsdt0129-nro_cg.
TYPES: END OF ty_edit_cg_5230.

DATA: g_custom_container_5230   TYPE REF TO cl_gui_custom_container,
      g_custom_container_5230_2 TYPE REF TO cl_gui_custom_container,
      dg_splitter_1_5230        TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2_5230        TYPE REF TO cl_gui_splitter_container,
      dg_parent_1_5230          TYPE REF TO cl_gui_container,
      dg_parent_2_5230          TYPE REF TO cl_gui_container,
      dg_parent_3_5230          TYPE REF TO cl_gui_container,
      dg_parent_4_5230          TYPE REF TO cl_gui_container,
      ctl_alv1_5230             TYPE REF TO cl_gui_alv_grid,
      ctl_alv2_5230             TYPE REF TO cl_gui_alv_grid,
      gs_layout_5230_alv1       TYPE lvc_s_layo,
      gs_layout_5230_alv2       TYPE lvc_s_layo,
      it_fieldcatalog_5230      TYPE lvc_t_fcat,
      it_fieldcatalog_sol_5230  TYPE lvc_t_fcat,
      it_sort_5230              TYPE lvc_t_sort,
      it_exclude_5230           TYPE ui_functions,
      gs_layout                 TYPE lvc_s_layo.

DATA: g_custom_container_5231 TYPE REF TO cl_gui_custom_container,
      ctl_alv1_5231           TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog_5231    TYPE lvc_t_fcat,
      it_exclude_5231         TYPE ui_functions.

DATA: it_carga_5230      TYPE STANDARD TABLE OF ty_carga_5230,
      it_carga_bkp_5230  TYPE STANDARD TABLE OF ty_carga_5230,
      it_carga_canc_5230 TYPE STANDARD TABLE OF ty_carga_5230,
      it_sol_5230        TYPE STANDARD TABLE OF ty_sol_5230,
      it_sol_click_5230  TYPE STANDARD TABLE OF ty_sol_5230.

DATA: it_sol_val_5230            TYPE STANDARD TABLE OF ty_sol_val_5230,
      it_edit_cg_5230            TYPE STANDARD TABLE OF ty_edit_cg_5230,
      it_solicitacoes_5230_check TYPE STANDARD TABLE OF ty_sol_5230.

DATA: wa_stable_5230 TYPE lvc_s_stbl .

DATA: _param TYPE  ustyp_t_parameters.

DATA r_trans TYPE RANGE OF vkbur.
DATA t_trans TYPE TABLE OF vkbur.
DATA p_antigo TYPE char1.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5230 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      toolbar_5230 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5230 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      data_changed_finished_5230 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      handle_hotspot_click_5230  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      on_double_click_5230 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      toolbar_5230_alv2 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      on_f4_5230 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5230 IMPLEMENTATION.

  METHOD toolbar_5230.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'EDITAR'.
    wa_tool-icon     = '@0Z@'.
    wa_tool-quickinfo = 'Editar Carga'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'REFRESH'.
    wa_tool-icon     = '@42@'.
    wa_tool-quickinfo = 'Cancela Edição'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

*    IF p_corplt IS NOT INITIAL OR
*       p_corpcg IS NOT INITIAL OR
*       p_corppt IS NOT INITIAL.
*
*      wa_tool-function = 'CANCELAR'.
*      wa_tool-icon     = '@2O@'.
*      wa_tool-quickinfo = 'Cancela Carga'.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      MOVE 3 TO wa_tool-butn_type.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      wa_tool-function  = 'RETURNCARGA'.
*      wa_tool-icon      = '@2W@'.
*      wa_tool-quickinfo = 'Retornar para Status "Sem Cotação"'.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      MOVE 3 TO wa_tool-butn_type.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*    ENDIF.

    wa_tool-function = 'VINCULAR'.
    wa_tool-icon     = '@EH@'.
    wa_tool-quickinfo = 'Vincular Pedido'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'AUTORIZACAO'.
    wa_tool-icon     = '@96@'.
    wa_tool-quickinfo = 'Aut. de Embarque'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function  = 'CANCAUTORIZ'.
    wa_tool-icon      = '@BA@'.
    wa_tool-quickinfo = 'Cancelar Aut. de Embarque'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'FRTCONTRATO'.
    wa_tool-icon     = '@4A@'.
    wa_tool-quickinfo = 'Frete Contratado Transportadora'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD user_command_5230.

    DATA: it_selected_rows  TYPE lvc_t_row,
          t_row_no          TYPE lvc_t_roid,
          wa_selected_rows  TYPE lvc_s_row,
          it_zsdt0129       TYPE STANDARD TABLE OF zsdt0129,
          wa_zsdt0129       TYPE zsdt0129,
          it_zsdt0133       TYPE STANDARD TABLE OF zsdt0133,
          wa_zsdt0133       TYPE zsdt0133,
          it_carga_aux_5230 TYPE STANDARD TABLE OF ty_carga_5230,
          wa_carga_5230     TYPE ty_carga_5230,
          it_sol_aux_5230   TYPE STANDARD TABLE OF ty_sol_5230,
          wa_sol_aux_5230   TYPE ty_sol_5230,
          wa_sol_5230       TYPE ty_sol_5230,
          it_rsparams       TYPE TABLE OF rsparams,
          wa_rsparams       TYPE rsparams,
          it_zsdt0062       TYPE STANDARD TABLE OF zsdt0062,
          wa_zsdt0062       TYPE zsdt0062,
          vl_lines          TYPE i,
          vl_spart          TYPE zsdt0131-spart,
          vl_vkorg          TYPE zsdt0131-vkorg,
          vl_check          TYPE char1,
          vl_check2         TYPE char1,
          vl_block          TYPE sy-tabix,
          vl_vinc1          TYPE zsdt0131-qtd_vinc,
          vl_vinc2          TYPE zsdt0131-qtd_vinc,
          wa_edit_cg_5230   TYPE ty_edit_cg_5230,
          vl_cont           TYPE i,
          vl_index          TYPE i,
          wa_sol_click_5230 TYPE ty_sol_5230,
          answer.

    wa_stable_5230-row = 'X'.
    wa_stable_5230-col = 'X'.


    CLEAR: it_selected_rows, wa_selected_rows, vl_lines.

    IF e_ucomm = 'EDITAR'.

      CALL METHOD ctl_alv1_5230->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.

          CLEAR: wa_zsdt0133.

          SELECT SINGLE *
            FROM zsdt0133
            INTO wa_zsdt0133
            WHERE nro_cg EQ wa_carga_5230-nro_cg.

          IF sy-subrc IS INITIAL.

            CLEAR: wa_zsdt0129.

            SELECT SINGLE *
              FROM zsdt0129
              INTO wa_zsdt0129
              WHERE nro_cg EQ wa_zsdt0133-nro_cg.

          ENDIF.

          IF wa_zsdt0133-status      NE wa_carga_5230-status      OR
             wa_zsdt0133-preco_frete NE wa_carga_5230-preco_frete OR
             wa_zsdt0129-placa_cav   NE wa_carga_5230-placa_cav   OR
             wa_zsdt0129-placa_car1  NE wa_carga_5230-placa_car1  OR
             wa_zsdt0129-placa_car2  NE wa_carga_5230-placa_car2  OR
             wa_zsdt0129-placa_car3  NE wa_carga_5230-placa_car3  OR
             wa_zsdt0129-motorista   NE wa_carga_5230-motorista.
            MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
          ELSEIF wa_carga_5230-status LE 6.

            CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave          = wa_carga_5230-nro_cg
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            ELSE.

              SELECT b~*
                FROM zsdt0131 AS a
                INNER JOIN zsdt0132 AS b ON b~nr_rot EQ a~cod_loc_emb
                INTO TABLE @DATA(t_0132)
                WHERE a~nro_lote EQ @wa_zsdt0129-nro_lote.
*WA_CARGA_5230-INCO1
              IF sy-subrc IS INITIAL.
                SORT t_0132 BY transportadora.
                DELETE ADJACENT DUPLICATES FROM t_0132 COMPARING transportadora.
                IF lines( t_0132 ) EQ 1.
                  IF NOT t_0132[ 1 ]-transportadora IS INITIAL AND wa_carga_5230-inco1 EQ 'CPT'.
                    MOVE abap_false TO wa_carga_5230-edit.
                  ELSE.
                    MOVE abap_true TO wa_carga_5230-edit.
                  ENDIF.
                ELSE.
                  MOVE abap_true TO wa_carga_5230-edit.
                ENDIF.
              ENDIF.

              MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_selected_rows-index.
              CLEAR: wa_carga_5230.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM bloqueia_linhas_5230.

      CALL METHOD ctl_alv1_5230->refresh_table_display
        EXPORTING
          is_stable = wa_stable_5230.

    ELSEIF e_ucomm = 'REFRESH'.

      CLEAR: wa_carga_5230.

      LOOP AT it_carga_5230 INTO wa_carga_5230 WHERE edit IS NOT INITIAL.

        CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
          EXPORTING
            chave = wa_carga_5230-nro_cg.

      ENDLOOP.

      it_carga_5230 = it_carga_bkp_5230.
      PERFORM bloqueia_linhas_5230.

      CALL METHOD ctl_alv1_5230->refresh_table_display
        EXPORTING
          is_stable = wa_stable_5230.

    ELSEIF e_ucomm = 'CANCELAR'.

      CLEAR: vl_check, vl_lines, wa_zsdt0129.

      CALL METHOD ctl_alv1_5230->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE TEXT-054 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      IF vl_check IS INITIAL.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Cancelar Lote(s)'
            text_question         = 'Prosseguir com o Cancelamento para a seleção?'
            text_button_1         = 'Sim'(023)
            text_button_2         = 'Não'(024)
            default_button        = '1'
            display_cancel_button = ' '
          IMPORTING
            answer                = answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF answer = '1'.

          it_sol_aux_5230 = it_sol_5230.

          LOOP AT it_carga_5230 INTO wa_carga_5230 WHERE status NE 6.
            READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
            IF sy-subrc IS INITIAL.
              LOOP AT it_sol_aux_5230 INTO wa_sol_aux_5230 WHERE nro_cg = wa_carga_5230-nro_cg.
                wa_sol_aux_5230-rot_desc = 'ABC'.
                MODIFY it_sol_aux_5230 FROM wa_sol_aux_5230 INDEX sy-tabix.
              ENDLOOP.
            ENDIF.
          ENDLOOP.

          DELETE it_sol_aux_5230 WHERE rot_desc NE 'ABC'.
          SORT it_sol_aux_5230 BY nro_cg vbeln posnr ASCENDING.
          DELETE ADJACENT DUPLICATES FROM it_sol_aux_5230 COMPARING nro_cg vbeln posnr.

          IF it_sol_aux_5230 IS NOT INITIAL.

            SELECT *
                        FROM zsdt0062
                        INTO TABLE it_zsdt0062
                        FOR ALL ENTRIES IN it_sol_aux_5230
                        WHERE nro_cg EQ it_sol_aux_5230-nro_cg
                          AND status EQ 'L'.

            "Check se há vinculação para o Lote
            IF it_zsdt0062 IS NOT INITIAL.
              READ TABLE it_zsdt0062 INTO wa_zsdt0062 INDEX 1.
              MESSAGE s000(z_fi) WITH TEXT-043 wa_zsdt0062-nro_cg DISPLAY LIKE 'E'.
              vl_check = abap_true.
            ENDIF.

          ELSE.
            vl_check = abap_true.
          ENDIF.

          IF vl_check IS INITIAL.
            it_carga_aux_5230 = it_carga_5230.

            LOOP AT it_carga_aux_5230 INTO wa_carga_5230.
              READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
              IF sy-subrc IS INITIAL.
                wa_carga_5230-rot_desc = 'ABC'.
                MODIFY it_carga_aux_5230 FROM wa_carga_5230 INDEX wa_selected_rows-index.
              ENDIF.
            ENDLOOP.

            DELETE it_carga_aux_5230 WHERE rot_desc NE 'ABC'.

            CLEAR: wa_carga_5230.
            READ TABLE it_carga_aux_5230 INTO wa_carga_5230 INDEX 1.

            CLEAR: wa_zsdt0133.

            SELECT SINGLE *
              FROM zsdt0133
              INTO wa_zsdt0133
              WHERE nro_cg EQ wa_carga_5230-nro_cg
                AND status NE 'X'.

            IF wa_carga_5230-status NE wa_zsdt0133-status.
              MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.

              CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave          = wa_carga_5230-nro_cg
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.

              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
                MOVE abap_true TO vl_check.
              ELSE.

                SELECT *
                  FROM zsdt0129
                  INTO TABLE it_zsdt0129
                  FOR ALL ENTRIES IN it_carga_aux_5230
                  WHERE nro_cg EQ it_carga_aux_5230-nro_cg
                    AND status NE 'X'.

                SELECT *
                  FROM zsdt0133
                  INTO TABLE it_zsdt0133
                  FOR ALL ENTRIES IN it_carga_aux_5230
                  WHERE nro_cg EQ it_carga_aux_5230-nro_cg.

                LOOP AT it_zsdt0129 INTO wa_zsdt0129.
                  CLEAR: wa_zsdt0129-nro_cg.
                  wa_zsdt0129-status = 1.
                  MODIFY zsdt0129 FROM wa_zsdt0129.
                ENDLOOP.

                LOOP AT it_zsdt0133 INTO wa_zsdt0133.
                  wa_zsdt0133-status = 'X'.
                  wa_zsdt0133-user_canc = sy-uname.
                  wa_zsdt0133-dt_canc = sy-datum.
                  wa_zsdt0133-hr_can = sy-uzeit.
                  MODIFY zsdt0133 FROM wa_zsdt0133.
                ENDLOOP.

                MESSAGE TEXT-044 TYPE 'S'.
                CLEAR: it_sol_click_5230.
                CALL METHOD ctl_alv1_5230->refresh_table_display
                  EXPORTING
                    is_stable = _stable.
                CALL METHOD ctl_alv2_5230->refresh_table_display
                  EXPORTING
                    is_stable = _stable.

                CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                  EXPORTING
                    chave = wa_carga_5230-nro_cg.

                LEAVE TO SCREEN 5000.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'VINCULAR'.

      CLEAR: wa_carga_5230, wa_zsdt0129.

      CALL METHOD ctl_alv1_5230->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows
          et_row_no     = t_row_no.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE TEXT-054 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      IF vl_check IS INITIAL.
        FREE it_sol_click_5230.
        it_sol_click_5230 = it_sol_5230.
        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_selected_rows-index.

        DELETE it_sol_click_5230 WHERE nro_cg NE wa_carga_5230-nro_cg.

        READ TABLE it_sol_click_5230 INTO DATA(w_sol_click_5230) INDEX 1.

        SELECT SINGLE mtart INTO @DATA(_mtart) FROM mara WHERE matnr EQ @w_sol_click_5230-matnr.


        IF sy-subrc IS INITIAL.
          SELECT SINGLE * FROM zsdt0281 INTO @DATA(it_zsdt0281)
            WHERE werks EQ @w_sol_click_5230-werks AND
            spart EQ @w_sol_click_5230-spart AND "@wa_carga_5230-spart and
            mtart EQ @_mtart.

          IF sy-subrc EQ 0.
            TRY.
                PERFORM validar_material
                  USING
                    w_sol_click_5230-matnr  "//Material
                    w_sol_click_5230-spart    "//St. Atividade: Sementes
                    _mtart. "//Tipo Material: Importação

                TRY.
                    PERFORM call_popup_vinculacao.
                  CATCH cx_abap_util_exception.
                    EXIT.
                ENDTRY.

              CATCH cx_abap_util_exception.
            ENDTRY.
*          ELSE.
*            "//Call popup to inform material or purshasing number.
*            "IF ( wa_carga_5230-werks = '0175' ).
*            TRY.
*                PERFORM validar_material
*                  USING
*                    w_sol_click_5230-matnr  "//Material
*                    w_sol_click_5230-spart    "//St. Atividade: Sementes
*                    _Mtart. "//Tipo Material: Importação
*
*                TRY.
*                    PERFORM call_popup_vinculacao.
*                  CATCH cx_abap_util_exception.
*                    EXIT.
*                ENDTRY.
*
*              CATCH cx_abap_util_exception.
*            ENDTRY.
*            "ENDIF.

          ENDIF.

          CLEAR: wa_zsdt0133.

          SELECT SINGLE *
            FROM zsdt0133
            INTO wa_zsdt0133
            WHERE nro_cg EQ wa_carga_5230-nro_cg.

          IF wa_zsdt0133-status NE wa_carga_5230-status.
            MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
          ELSEIF wa_carga_5230-status GE 5.
            MESSAGE TEXT-070 TYPE 'S' DISPLAY LIKE 'E'.
          ELSE.

            CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave          = wa_carga_5230-nro_cg
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
              MOVE abap_true TO vl_check.
            ELSE.

*             "// Busca dados de Local de Entrega para validação
              SELECT c~*
                FROM zsdt0129 AS a
                INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
                INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
                INTO TABLE @t_0132
                WHERE a~nro_cg EQ @wa_carga_5230-nro_cg.
*             "// Se o Local de Embarque for do Tipo Armazem não Imprimir Autorização de Embarque
              IF line_exists( t_0132[ armazem = abap_true ] ).
                MESSAGE TEXT-140 TYPE 'S' DISPLAY LIKE 'E'.
                vl_check = abap_true. EXIT.
              ENDIF.

              "Nr. Carga
              wa_rsparams-selname = 'P_NROCG'.
              wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
              wa_rsparams-sign = 'I'.
              wa_rsparams-option = 'EQ'.
              wa_rsparams-low    = wa_carga_5230-nro_cg.
              APPEND wa_rsparams TO it_rsparams.
              CLEAR wa_rsparams.

              "Dt. Lançamento
              wa_rsparams-selname = 'P_ERDAT'.
              wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
              wa_rsparams-sign = 'I'.
              wa_rsparams-option = 'BT'.
              wa_rsparams-low    = 20150101.
              wa_rsparams-high   = sy-datum.
              APPEND wa_rsparams TO it_rsparams.
              CLEAR wa_rsparams.

              LOOP AT it_sol_5230 INTO wa_sol_5230 WHERE nro_cg EQ wa_carga_5230-nro_cg.
                vl_spart = wa_sol_5230-spart.
                vl_vkorg = wa_sol_5230-vkorg.
                "Nr. Material
                wa_rsparams-selname = 'P_MATNR'.
                wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
                wa_rsparams-sign = 'I'.
                wa_rsparams-option = 'EQ'.
                wa_rsparams-low    = wa_sol_5230-matnr.
                APPEND wa_rsparams TO it_rsparams.
                CLEAR wa_rsparams.
              ENDLOOP.

              "Nr. OrgaNização de Vendas
              wa_rsparams-selname = 'P_VKORG'.
              wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
              wa_rsparams-sign = 'I'.
              wa_rsparams-option = 'EQ'.
              wa_rsparams-low    = vl_vkorg.
              APPEND wa_rsparams TO it_rsparams.
              CLEAR wa_rsparams.

              "Nr. Setor Ativ.
              wa_rsparams-selname = 'P_SPART'.
              wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
              wa_rsparams-sign = 'I'.
              wa_rsparams-option = 'EQ'.
              wa_rsparams-low    = vl_spart.
              APPEND wa_rsparams TO it_rsparams.
              CLEAR wa_rsparams.

              IF vl_spart EQ '04'.
                "Tipo pedido de Compra.
                wa_rsparams-selname = 'P_BSART'.
                wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
                wa_rsparams-sign = 'I'.
                wa_rsparams-option = 'EQ'.
                wa_rsparams-low    = 'ZSEM'.
                APPEND wa_rsparams TO it_rsparams.
                CLEAR wa_rsparams.
                "Tipo pedido de Compra.
                wa_rsparams-selname = 'P_BSART'.
                wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
                wa_rsparams-sign = 'I'.
                wa_rsparams-option = 'EQ'.
                wa_rsparams-low    = 'ZOSM'.
                APPEND wa_rsparams TO it_rsparams.
                CLEAR wa_rsparams.
*INICIO - AS - 02/10/2020 - CS2020001072
                "Tipo pedido de Compra.
                wa_rsparams-selname = 'P_BSART'.
                wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
                wa_rsparams-sign = 'I'.
                wa_rsparams-option = 'EQ'.
                wa_rsparams-low    = 'ZSON'.
                APPEND wa_rsparams TO it_rsparams.
                CLEAR wa_rsparams.
                "Tipo pedido de Compra.
                wa_rsparams-selname = 'P_BSART'.
                wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
                wa_rsparams-sign = 'I'.
                wa_rsparams-option = 'EQ'.
                wa_rsparams-low    = 'ZEFI'.
                APPEND wa_rsparams TO it_rsparams.
                CLEAR wa_rsparams.
                wa_rsparams-selname = 'P_BSART'.
                wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
                wa_rsparams-sign = 'I'.
                wa_rsparams-option = 'EQ'.
                wa_rsparams-low    = 'ZIMP'.
                APPEND wa_rsparams TO it_rsparams.
                CLEAR wa_rsparams.
              ENDIF.
*FIM - AS - 02/10/2020 - CS2020001072
              SUBMIT zsdr0062 WITH SELECTION-TABLE it_rsparams AND RETURN.

              CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave = wa_carga_5230-nro_cg.

            ENDIF.

          ENDIF.

        ELSE.
          MESSAGE TEXT-070 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'AUTORIZACAO'.

      CLEAR: vl_check, wa_zsdt0133, wa_zsdt0129, vl_check2, vl_block.

      CALL METHOD ctl_alv1_5230->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      "Consistência se há dados faltantes
      IF vl_check IS INITIAL.
        LOOP AT it_carga_5230 INTO wa_carga_5230.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.

            CLEAR: wa_zsdt0133.

            SELECT SINGLE *
              FROM zsdt0133
              INTO wa_zsdt0133
              WHERE nro_cg EQ wa_carga_5230-nro_cg.

            IF sy-subrc IS INITIAL.

              CLEAR: wa_zsdt0129.

              SELECT SINGLE *
                FROM zsdt0129
                INTO wa_zsdt0129
                WHERE nro_cg EQ wa_zsdt0133-nro_cg.

            ENDIF.

            IF wa_zsdt0133-status      NE wa_carga_5230-status      OR
               wa_zsdt0133-preco_frete NE wa_carga_5230-preco_frete OR
               wa_zsdt0129-placa_cav   NE wa_carga_5230-placa_cav   OR
               wa_zsdt0129-placa_car1  NE wa_carga_5230-placa_car1  OR
               wa_zsdt0129-placa_car2  NE wa_carga_5230-placa_car2  OR
               wa_zsdt0129-placa_car3  NE wa_carga_5230-placa_car3  OR
               wa_zsdt0129-motorista   NE wa_carga_5230-motorista.
              MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
              vl_check = abap_true.
              EXIT.
            ELSE.
              IF wa_carga_5230-status < 4.
                MESSAGE TEXT-071 TYPE 'S' DISPLAY LIKE 'E'.
                vl_check = abap_true.
                EXIT.
              ENDIF.

*             "// Busca dados de Local de Entrega para validação
              SELECT b~*
                FROM zsdt0131 AS a
                INNER JOIN zsdt0132 AS b ON b~nr_rot EQ a~cod_loc_emb
                INTO TABLE @t_0132
                WHERE a~nro_lote EQ @wa_zsdt0129-nro_lote.
*             "// Se o Local de Embarque for do Tipo Armazem não Imprimir Autorização de Embarque
              IF line_exists( t_0132[ armazem = abap_true ] ).
                MESSAGE TEXT-139 TYPE 'S' DISPLAY LIKE 'E'.
                vl_check = abap_true.
                EXIT.
              ENDIF.

*             "// Se for transportadora não Obriga a includão dos campos.
              IF NOT line_exists( t_0132[ transportadora = abap_false ] )." AND WA_CARGA_5230-INCO1 EQ 'CPT'.
              ELSE.
                IF ( wa_carga_5230-preco_frete        IS INITIAL AND wa_carga_5230-inco1 NE 'FOB' ) OR
                   ( wa_carga_5230-placa_cav          IS INITIAL OR
                     wa_carga_5230-motorista          IS INITIAL OR
                     wa_carga_5230-cod_transportadora IS INITIAL ).
*                IF ( ( WA_CARGA_5230-PRECO_FRETE        IS INITIAL OR
*                       WA_CARGA_5230-PLACA_CAV          IS INITIAL OR
*                       WA_CARGA_5230-MOTORISTA          IS INITIAL OR
*                       WA_CARGA_5230-COD_TRANSPORTADORA IS INITIAL ) AND WA_CARGA_5230-INCO1 NE 'FOB' ) OR
*                   ( ( WA_CARGA_5230-PLACA_CAV          IS INITIAL OR
*                       WA_CARGA_5230-MOTORISTA          IS INITIAL ) AND WA_CARGA_5230-INCO1 EQ 'FOB' ).
                  MESSAGE TEXT-042 TYPE 'S' DISPLAY LIKE 'E'.
                  vl_check = abap_true.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      "Consistência se há alguma ordem não vinculada
      IF vl_check IS INITIAL.
        it_sol_aux_5230 = it_sol_5230.

        LOOP AT it_carga_5230 INTO wa_carga_5230.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.
            LOOP AT it_sol_aux_5230 INTO wa_sol_aux_5230 WHERE nro_cg = wa_carga_5230-nro_cg.
              wa_sol_aux_5230-rot_desc = 'ABC'.
              MODIFY it_sol_aux_5230 FROM wa_sol_aux_5230 INDEX sy-tabix.
            ENDLOOP.
          ENDIF.
        ENDLOOP.

        DELETE it_sol_aux_5230 WHERE rot_desc NE 'ABC'.
        SORT it_sol_aux_5230 BY nro_cg vbeln posnr ASCENDING.
        DELETE ADJACENT DUPLICATES FROM it_sol_aux_5230 COMPARING nro_cg vbeln posnr.

        SELECT *
          FROM zsdt0062
          INTO TABLE it_zsdt0062
          FOR ALL ENTRIES IN it_sol_aux_5230
          WHERE nro_cg EQ it_sol_aux_5230-nro_cg
            AND status EQ 'L'.

        LOOP AT it_sol_aux_5230 INTO wa_sol_aux_5230.

          TRY .
              DATA(cod_trans) = it_carga_5230[ nro_cg = wa_sol_aux_5230-nro_cg ]-cod_transportadora.
            CATCH cx_sy_itab_line_not_found.
              CLEAR cod_trans.
          ENDTRY.

          SELECT COUNT(*)
            FROM zsdt0132
              WHERE armazem EQ abap_true
              AND ( lifnr EQ cod_trans OR
                    kunnr EQ cod_trans ).

          IF sy-subrc IS NOT INITIAL.
            LOOP AT it_sol_5230 INTO wa_sol_5230 WHERE nro_cg EQ wa_sol_aux_5230-nro_cg
                                                   AND vbeln  EQ wa_sol_aux_5230-vbeln
                                                   AND posnr  EQ wa_sol_aux_5230-posnr.
              vl_vinc1 = vl_vinc1 + wa_sol_5230-qtd_vinc.
            ENDLOOP.
            LOOP AT it_zsdt0062 INTO wa_zsdt0062 WHERE nro_cg EQ wa_sol_aux_5230-nro_cg
                                                   AND vbeln  EQ wa_sol_aux_5230-vbeln
                                                   AND posnr  EQ wa_sol_aux_5230-posnr.
              vl_vinc2 = vl_vinc2 + wa_zsdt0062-qtd_vinc.
            ENDLOOP.
            IF vl_vinc1 NE vl_vinc2.
              MESSAGE s000(z_fi) WITH 'Falta vinculação de Pedido na Carga' wa_sol_aux_5230-nro_cg DISPLAY LIKE 'E'.
              vl_check = abap_true.
              EXIT.
            ENDIF.
          ENDIF.

        ENDLOOP.

        CLEAR: vl_vinc1, vl_vinc2.
      ENDIF.

      "Gera Aut. de Embarque
      IF vl_check IS INITIAL.
        vl_check2 = abap_true.

        LOOP AT it_carga_5230 INTO wa_carga_5230.


          vl_block = sy-tabix.

          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.

            CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave          = wa_carga_5230-nro_cg
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
              CLEAR: vl_check2.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CLEAR: vl_cont.

      IF vl_check2 IS NOT INITIAL.

        LOOP AT it_carga_5230 INTO wa_carga_5230.

          vl_cont = sy-tabix.

          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.

            PERFORM gerar_pdf_embarque USING wa_carga_5230-nro_cg abap_false.
            IF wa_carga_5230-status EQ 4.
              SELECT SINGLE *
                            FROM zsdt0133
                            INTO CORRESPONDING FIELDS OF wa_zsdt0133
                            WHERE nro_cg EQ wa_carga_5230-nro_cg.

              wa_zsdt0133-status = 5.
              MODIFY zsdt0133 FROM wa_zsdt0133.

              wa_carga_5230-status = 5.
              wa_carga_5230-icone  = '@96@'.
              MODIFY it_carga_5230 FROM wa_carga_5230 INDEX vl_cont.

            ENDIF.

            CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave = wa_carga_5230-nro_cg.

          ENDIF.
        ENDLOOP.

      ELSE.

        LOOP AT it_carga_5230 INTO wa_carga_5230.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.
            IF wa_selected_rows-index LT vl_block.

              CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave = wa_carga_5230-nro_cg.

            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.

      CALL METHOD ctl_alv1_5230->refresh_table_display
        EXPORTING
          is_stable = wa_stable_5230.

    ELSEIF e_ucomm = 'CANCAUTORIZ'.

      CLEAR: it_carga_canc_5230, vl_check, vl_lines, wa_zsdt0133.

      CALL METHOD ctl_alv1_5230->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE TEXT-054 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      IF vl_check IS INITIAL.

        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_selected_rows-index.

*      LOOP AT IT_CARGA_5720 INTO WA_CARGA_5720.
*        READ TABLE IT_SEL_ROWS_CARGA_5720 INTO WA_SEL_ROWS_CARGA_5720 WITH KEY INDEX = SY-TABIX.
        IF sy-subrc IS INITIAL.

          "// Busca dados de Local de Entrega para validação
          SELECT c~*
            FROM zsdt0129 AS a
            INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
            INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
            INTO TABLE @t_0132
            WHERE a~nro_cg EQ @wa_carga_5230-nro_cg.
*             "// Se o Local de Embarque for do Tipo Armazem não Cancelar a Autorização de Embarque
          IF line_exists( t_0132[ armazem = abap_true ] ).
            MESSAGE TEXT-149 TYPE 'S' DISPLAY LIKE 'E'.
            vl_check = abap_true.
            EXIT.
          ENDIF.

          CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave          = wa_carga_5230-nro_cg
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
          ELSE.

            CLEAR: wa_zsdt0133.

            SELECT SINGLE *
              FROM zsdt0133
              INTO wa_zsdt0133
             WHERE nro_cg EQ wa_carga_5230-nro_cg.

            IF wa_zsdt0133-status NE wa_carga_5230-status.
              MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.

              APPEND wa_carga_5230 TO it_carga_canc_5230.
              PERFORM busca_lotes_produtos_5230 CHANGING vl_check.

              IF vl_check IS INITIAL.

                PERFORM cancela_autorizacao_5230.

                CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                  EXPORTING
                    chave = wa_carga_5230-nro_cg.

                LEAVE TO SCREEN 5000.
              ELSE.
                MESSAGE TEXT-093 TYPE 'S' DISPLAY LIKE 'E'.
              ENDIF.

            ENDIF.

            CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave = wa_carga_5230-nro_cg.

          ENDIF.

        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'RETURNCARGA'.

      CLEAR: it_carga_canc_5230, vl_check, vl_lines, wa_zsdt0133.

      CALL METHOD ctl_alv1_5230->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE TEXT-054 TYPE 'S' DISPLAY LIKE 'E'.
        MOVE abap_true TO vl_check.
      ENDIF.

      READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
      READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_selected_rows-index.

      IF sy-subrc IS INITIAL.
        "Verifica se a Carga está com Lote de Produtos vinculados
        APPEND wa_carga_5230 TO it_carga_canc_5230.
        PERFORM busca_lotes_produtos_5230 CHANGING vl_check.

        IF vl_check IS INITIAL.

*       "// Carga sem Cotação   2
*       "// Carga em Cotação    3
*       "// Frete Contratado    4
*       "// Embarque Autorizado 5

          CASE wa_carga_5230-status.
            WHEN 3 OR 4.
            WHEN 5.

              CALL FUNCTION 'POPUP_TO_CONFIRM'
                EXPORTING
                  titlebar              = 'Carga já Possui autorização de Embarque!'
                  text_question         = 'Quer Mesmo Alterar o Status para "Sem Cotação"?'
                  text_button_1         = 'Sim'(023)
                  text_button_2         = 'Não'(024)
                  default_button        = '1'
                  display_cancel_button = ' '
                IMPORTING
                  answer                = answer
                EXCEPTIONS
                  text_not_found        = 1
                  OTHERS                = 2.

              CHECK answer EQ '1'.

            WHEN OTHERS.
              MESSAGE TEXT-131 TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
          ENDCASE.

          CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave          = wa_carga_5230-nro_cg
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
          ELSE.

            CLEAR: wa_zsdt0133.

            SELECT SINGLE *
              FROM zsdt0133
              INTO wa_zsdt0133
             WHERE nro_cg EQ wa_carga_5230-nro_cg.

            IF wa_zsdt0133-status NE wa_carga_5230-status.
              MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.

              UPDATE zsdt0133 SET cod_transportadora = ''
                                  preco_frete = 0
                                  status = '2'
                                  user_edit = sy-uname
                                  data_edit = sy-datum
                                  hora_edit = sy-uzeit
              WHERE nro_cg EQ wa_zsdt0133-nro_cg.

              IF sy-subrc IS INITIAL.
                UPDATE zsdt0129 SET
                     placa_cav = ' '
                     placa_car1 = ' '
                     placa_car2 = ' '
                     placa_car3 = ' '
                     motorista = ' '
                WHERE nro_cg EQ wa_zsdt0133-nro_cg.
              ENDIF.

              IF sy-subrc IS INITIAL.
                CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                  EXPORTING
                    chave = wa_carga_5230-nro_cg.
                LEAVE TO SCREEN 5000.
              ENDIF.

            ENDIF.

            CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave = wa_carga_5230-nro_cg.

          ENDIF.
        ELSE.
          MESSAGE TEXT-154 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.

    ELSEIF e_ucomm = 'DESVINCULAR'.

      DATA: qtd_lotes TYPE i.

      CALL METHOD ctl_alv2_5230->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      IF lines( it_selected_rows ) NE 1.
        MESSAGE TEXT-142 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.

      CHECK vl_check IS INITIAL.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Desvincluar Cliente'
          text_question         = 'Prosseguir com a Desvinculação do Cliente desta Carga?'
          text_button_1         = 'Sim'(023)
          text_button_2         = 'Não'(024)
          default_button        = '1'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer = '1'.

*
      vl_index = it_selected_rows[ 1 ]-index.
      wa_sol_click_5230 = it_sol_click_5230[ vl_index ].

      SELECT COUNT(*)
        FROM zsdt0134
          WHERE nro_cg  EQ wa_sol_click_5230-nro_cg
            AND vbeln EQ wa_sol_click_5230-vbeln
            AND posnr EQ wa_sol_click_5230-posnr
            AND status NE 'X'.

      IF sy-subrc IS INITIAL.
        MESSAGE 'OV/item Contém lotes de Produtos vinculados!' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      SELECT COUNT(*)
        FROM zsdt0062
          WHERE nro_cg  EQ wa_sol_click_5230-nro_cg
            AND nro_sol EQ wa_sol_click_5230-nro_sol
            AND seq     EQ wa_sol_click_5230-seq
            AND status  EQ 'L'.

      IF sy-subrc IS INITIAL.
        MESSAGE s000(z_fi) WITH TEXT-150 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      SELECT COUNT(*) FROM zsdt0131
        INTO qtd_lotes
        WHERE nro_lote = wa_sol_click_5230-nro_lote
          AND status <> 'X'.

      IF sy-subrc EQ 0.

        IF qtd_lotes GT 1.

          UPDATE zsdt0131 SET
                  status = abap_true
                  user_canc = sy-uname
                  dt_canc = sy-datum
                  hr_can  = sy-uzeit
           WHERE nro_lote EQ wa_sol_click_5230-nro_lote
             AND nro_sol EQ wa_sol_click_5230-nro_sol
             AND seq EQ wa_sol_click_5230-seq
             AND kunnr EQ wa_sol_click_5230-kunnr
             AND vbeln EQ wa_sol_click_5230-vbeln
             AND posnr EQ wa_sol_click_5230-posnr.

          UPDATE zsdt0130 SET
                    status = abap_true
                    usnam  = sy-uname
                    data_atual = sy-datum
                    hora_atual = sy-uzeit
             WHERE nro_lote EQ wa_sol_click_5230-nro_lote
             AND nro_sol EQ wa_sol_click_5230-nro_sol
             AND seq EQ wa_sol_click_5230-seq
             AND kunnr EQ wa_sol_click_5230-kunnr.

          CLEAR: wa_zsdt0129.

          SELECT SINGLE *
            FROM zsdt0129
            INTO wa_zsdt0129
            WHERE nro_lote EQ wa_sol_click_5230-nro_lote.

          wa_zsdt0129-qtd_total_kg = wa_zsdt0129-qtd_total_kg - wa_sol_click_5230-qtd_emkg.

          UPDATE zsdt0129 SET
                  qtd_total_kg = wa_zsdt0129-qtd_total_kg
             WHERE nro_lote EQ wa_sol_click_5230-nro_lote.

          CLEAR: wa_zsdt0133.

          SELECT SINGLE *
            FROM zsdt0133
            INTO wa_zsdt0133
           WHERE nro_cg EQ wa_sol_click_5230-nro_cg.

          wa_zsdt0133-qtd_total_kg = wa_zsdt0133-qtd_total_kg - wa_sol_click_5230-qtd_emkg.

          IF wa_zsdt0133-qtd_total_kg LE 23000.
            wa_zsdt0133-ctg_transp = 'A'.
          ELSEIF wa_zsdt0133-qtd_total_kg LE 27000.
            wa_zsdt0133-ctg_transp = 'B'.
          ELSEIF wa_zsdt0133-qtd_total_kg LE 32000.
            wa_zsdt0133-ctg_transp = 'C'.
          ELSEIF wa_zsdt0133-qtd_total_kg LE 37000.
            wa_zsdt0133-ctg_transp = 'D'.
          ELSEIF wa_zsdt0133-qtd_total_kg LE 50000.
            wa_zsdt0133-ctg_transp = 'E'.
          ENDIF.

          UPDATE zsdt0133 SET
                    qtd_total_kg = wa_zsdt0133-qtd_total_kg
                    ctg_transp   = wa_zsdt0133-ctg_transp
              WHERE nro_cg EQ wa_sol_click_5230-nro_cg.

        ELSE.

          SELECT COUNT(*)
            FROM zsdt0129
            INTO @DATA(vl_qtd)
            WHERE nro_cg EQ @wa_sol_click_5230-nro_cg
              AND status <> 'X'.

          IF vl_qtd EQ 1.

            UPDATE zsdt0133
            SET status = abap_true
                user_canc = sy-uname
                dt_canc = sy-datum
                hr_can = sy-uzeit
               WHERE nro_cg EQ wa_sol_click_5230-nro_cg.
          ELSE.

            SELECT SINGLE *
              FROM zsdt0133
              INTO wa_zsdt0133
             WHERE nro_cg EQ wa_sol_click_5230-nro_cg.

            wa_zsdt0133-qtd_total_kg = wa_zsdt0133-qtd_total_kg - wa_sol_click_5230-qtd_emkg.

            IF wa_zsdt0133-qtd_total_kg LE 23000.
              wa_zsdt0133-ctg_transp = 'A'.
            ELSEIF wa_zsdt0133-qtd_total_kg LE 27000.
              wa_zsdt0133-ctg_transp = 'B'.
            ELSEIF wa_zsdt0133-qtd_total_kg LE 32000.
              wa_zsdt0133-ctg_transp = 'C'.
            ELSEIF wa_zsdt0133-qtd_total_kg LE 37000.
              wa_zsdt0133-ctg_transp = 'D'.
            ELSEIF wa_zsdt0133-qtd_total_kg LE 50000.
              wa_zsdt0133-ctg_transp = 'E'.
            ENDIF.

            UPDATE zsdt0133 SET
                      qtd_total_kg = wa_zsdt0133-qtd_total_kg
                      ctg_transp   = wa_zsdt0133-ctg_transp
                      user_edit    = sy-uname
                      data_edit    = sy-datum
                      hora_edit    = sy-uzeit
                WHERE nro_cg EQ wa_sol_click_5230-nro_cg.

          ENDIF.

          CLEAR: wa_zsdt0129.

          SELECT SINGLE *
            FROM zsdt0129
            INTO wa_zsdt0129
               WHERE nro_cg EQ wa_sol_click_5230-nro_cg
                 AND status <> 'X'.

          IF wa_zsdt0129-inco1 EQ 'FOB'.
            UPDATE zsdt0129 SET
                status = '1'
                nro_cg = ' '
              WHERE nro_lote EQ wa_sol_click_5230-nro_lote.
          ELSE.
            UPDATE zsdt0129 SET
                  status = '1'
                  nro_cg = ' '
                  placa_cav = ' '
                  placa_car1 = ' '
                  placa_car2 = ' '
                  placa_car3 = ' '
                  motorista = ' '
               WHERE nro_lote EQ wa_sol_click_5230-nro_lote.
          ENDIF.
        ENDIF.
      ENDIF.

      LEAVE TO SCREEN 5000.

    ELSEIF e_ucomm = 'REMOVECARGA'.

      CALL METHOD ctl_alv2_5230->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      IF lines( it_selected_rows ) NE 1.
        MESSAGE TEXT-142 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.

      CHECK vl_check IS INITIAL.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Desvincular Solicitação de Entrega do Lote de Carga'
          text_question         = 'Prosseguir com a Remoção da "Sol. de Entrega" do Lote de Carga?'
          text_button_1         = 'Sim'(023)
          text_button_2         = 'Não'(024)
          default_button        = '1'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer = '1'.

*     "// Cancela o item do Lote
      vl_index = it_selected_rows[ 1 ]-index.
      wa_sol_click_5230 = it_sol_click_5230[ vl_index ].

      SELECT COUNT(*)
        FROM zsdt0134
          WHERE nro_cg  EQ wa_sol_click_5230-nro_cg
            AND vbeln EQ wa_sol_click_5230-vbeln
            AND posnr EQ wa_sol_click_5230-posnr
            AND status NE 'X'.

      IF sy-subrc IS INITIAL.
        MESSAGE 'OV/item Contém lotes de Produtos vinculados!' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      SELECT COUNT(*)
        FROM zsdt0062
          WHERE nro_cg  EQ wa_sol_click_5230-nro_cg
            AND nro_sol EQ wa_sol_click_5230-nro_sol
            AND seq     EQ wa_sol_click_5230-seq
            AND status  EQ 'L'.

      IF sy-subrc IS INITIAL.
        MESSAGE s000(z_fi) WITH TEXT-150 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      UPDATE zsdt0131 SET status = abap_true
                          user_canc = sy-uname
                          dt_canc = sy-datum
             WHERE nro_lote EQ wa_sol_click_5230-nro_lote
               AND nro_sol EQ wa_sol_click_5230-nro_sol
               AND seq EQ wa_sol_click_5230-seq
               AND kunnr EQ wa_sol_click_5230-kunnr
               AND vbeln EQ wa_sol_click_5230-vbeln
               AND posnr EQ wa_sol_click_5230-posnr.

      UPDATE zsdt0130 SET status = abap_true
                          usnam  = sy-uname
                          data_atual = sy-datum
                          hora_atual = sy-uzeit
         WHERE nro_lote EQ wa_sol_click_5230-nro_lote
           AND nro_sol EQ wa_sol_click_5230-nro_sol
           AND seq EQ wa_sol_click_5230-seq
           AND kunnr EQ wa_sol_click_5230-kunnr.

*     "// Cancela ou Atualiza Lote
      SELECT SINGLE *
        FROM zsdt0129
        INTO @DATA(w_0129)
        WHERE nro_lote EQ @wa_sol_click_5230-nro_lote.

      IF sy-subrc IS INITIAL.

        w_0129-qtd_total_kg = w_0129-qtd_total_kg - wa_sol_click_5230-qtd_emkg.

        IF w_0129-qtd_total_kg EQ 0.
          UPDATE zsdt0129 SET status    = abap_true
                              user_canc = sy-uname
                              dt_canc   = sy-datum
                              hr_can    = sy-uzeit
                 WHERE nro_lote EQ wa_sol_click_5230-nro_lote.
        ELSE.
          UPDATE zsdt0129 SET qtd_total_kg = w_0129-qtd_total_kg
                 WHERE nro_lote EQ wa_sol_click_5230-nro_lote.
        ENDIF.

*      "// Cancela ou Atualiza a Carga
        SELECT SINGLE *
          FROM zsdt0133
          INTO @DATA(w_0133)
          WHERE nro_cg EQ @wa_sol_click_5230-nro_cg.

        IF sy-subrc IS INITIAL.

          w_0133-qtd_total_kg = w_0133-qtd_total_kg - wa_sol_click_5230-qtd_emkg.
          IF w_0133-qtd_total_kg EQ 0.
            UPDATE zsdt0133 SET status    = abap_true
                                user_canc = sy-uname
                                dt_canc   = sy-datum
                                hr_can    = sy-uzeit
                   WHERE nro_cg EQ w_0133-nro_cg.
          ELSE.

            IF w_0133-qtd_total_kg LE 23000.
              w_0133-ctg_transp = 'A'.
            ELSEIF w_0133-qtd_total_kg LE 27000.
              w_0133-ctg_transp = 'B'.
            ELSEIF w_0133-qtd_total_kg LE 32000.
              w_0133-ctg_transp = 'C'.
            ELSEIF w_0133-qtd_total_kg LE 37000.
              w_0133-ctg_transp = 'D'.
            ELSEIF w_0133-qtd_total_kg LE 50000.
              w_0133-ctg_transp = 'E'.
            ENDIF.

            UPDATE zsdt0133 SET qtd_total_kg = w_0133-qtd_total_kg
                                ctg_transp = w_0133-ctg_transp
                                user_edit = sy-uname
                                data_edit = sy-datum
                                hora_edit = sy-uzeit
                   WHERE nro_cg EQ w_0133-nro_cg.
          ENDIF.

        ENDIF.

*       "// Cancela ou Atualiza a Solicitação de entrega
        SELECT *
          FROM zsdt0082
          INTO TABLE @DATA(t_0082)
          WHERE nro_sol EQ @wa_sol_click_5230-nro_sol.

        IF sy-subrc IS INITIAL.
          DATA(w_0082) = t_0082[ seq = wa_sol_click_5230-seq ].
          w_0082-qte_lib = w_0082-qte_lib - wa_sol_click_5230-qtd_vinc.
          IF w_0082-qte_lib EQ 0.
            UPDATE zsdt0082 SET status = '4'
                                user_canc = sy-uname
                                dt_canc = sy-datum
                   WHERE nro_sol EQ w_0082-nro_sol
                     AND seq EQ w_0082-seq
                     AND vbeln EQ w_0082-vbeln
                     AND posnr EQ w_0082-posnr.
          ELSE.
            UPDATE zsdt0082 SET qte_lib = w_0082-qte_lib
                   WHERE nro_sol EQ w_0082-nro_sol
                     AND seq EQ w_0082-seq
                     AND vbeln EQ w_0082-vbeln
                     AND posnr EQ w_0082-posnr.
          ENDIF.

          CLEAR w_0082.
          w_0082 = t_0082[ seq = '001' ].
          w_0082-qte_sol = w_0082-qte_sol - wa_sol_click_5230-qtd_vinc.
          IF w_0082-qte_sol EQ 0.
            UPDATE zsdt0082 SET status = '3'
                                user_canc = sy-uname
                                dt_canc = sy-datum
                   WHERE nro_sol EQ w_0082-nro_sol
                     AND seq EQ w_0082-seq
                     AND vbeln EQ w_0082-vbeln
                     AND posnr EQ w_0082-posnr.
          ELSE.
            UPDATE zsdt0082 SET qte_sol = w_0082-qte_sol
                   WHERE nro_sol EQ w_0082-nro_sol
                     AND seq EQ w_0082-seq
                     AND vbeln EQ w_0082-vbeln
                     AND posnr EQ w_0082-posnr.
          ENDIF.
        ENDIF.
      ENDIF.

      LEAVE TO SCREEN 5000.

    ELSEIF e_ucomm = 'EDITSEQENTREGA'.

      p_antigo = abap_false.
      PERFORM bloqueia_linhas_5230_sol.

      LEAVE TO SCREEN 5000.

    ELSEIF e_ucomm = 'ADDLOTE'.

      PERFORM seleciona_carga_5231.
      CALL SCREEN 5231 STARTING AT 5 5 ENDING AT 160 30.

      LEAVE TO SCREEN 5000.

    ELSEIF e_ucomm = 'CHANGELOCAL'.

      CALL METHOD ctl_alv2_5230->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = it_fieldcatalog_sol_5230.

      IF line_exists( it_fieldcatalog_sol_5230[ fieldname  = 'LIFNR' ] ).
        p_ative = COND #( WHEN p_ative IS INITIAL THEN abap_true ELSE abap_false ).
        it_fieldcatalog_sol_5230[ fieldname  = 'LIFNR' ]-no_out = p_ative.
      ENDIF.

      CALL METHOD ctl_alv2_5230->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = it_fieldcatalog_sol_5230.

      CALL METHOD ctl_alv2_5230->refresh_table_display
        EXPORTING
          is_stable = wa_stable_5230.

    ELSEIF e_ucomm = 'FRTCONTRATO'.

      CALL METHOD ctl_alv1_5230->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      IF it_selected_rows IS NOT INITIAL.
        IF lines( it_selected_rows ) NE 1.
          MESSAGE 'Selecione somente uma Linha!' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE 'Selecione uma Linha!' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      wa_carga_5230 = it_carga_5230[ it_selected_rows[ 1 ]-index ].

      SELECT SINGLE *
        FROM zsdt0133
        INTO wa_zsdt0133
        WHERE nro_cg EQ wa_carga_5230-nro_cg.

      IF wa_zsdt0133-status NE wa_carga_5230-status.
        MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF wa_zsdt0133-status NOT BETWEEN 2 AND 4.
        MESSAGE TEXT-102 TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF wa_carga_5230-preco_frete IS INITIAL OR wa_carga_5230-cod_transportadora IS INITIAL.
        MESSAGE TEXT-047 TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
        EXPORTING
          chave          = wa_carga_5230-nro_cg
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0133
        INTO CORRESPONDING FIELDS OF wa_zsdt0133
        WHERE nro_cg EQ wa_carga_5230-nro_cg.

      wa_zsdt0133-status = 4.
      MODIFY zsdt0133 FROM wa_zsdt0133.

*      WA_CARGA_5230-STATUS = 4.
*      WA_CARGA_5230-ICONE  = '@4A@'.

*      MODIFY IT_CARGA_SCOT_5220 FROM WA_CARGA_5220 INDEX VL_CONT.

      CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
        EXPORTING
          chave = wa_carga_5230-nro_cg.

      LEAVE TO SCREEN 5000.

    ENDIF.

  ENDMETHOD.                    "USER_COMMAND

  METHOD data_changed_finished_5230.

    DATA: wa_good_cells TYPE lvc_s_modi,
          wa_carga_5230 TYPE ty_carga_5230,
          wa_lfa1       TYPE lfa1,
          vl_check      TYPE char1.

    wa_stable_5230-row = 'X'.
    wa_stable_5230-col = 'X'.

    LOOP AT et_good_cells INTO wa_good_cells.
      IF e_modified EQ abap_true.
        IF wa_good_cells-fieldname EQ 'COD_TRANSPORTADORA'.

          READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_good_cells-row_id.

          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
            WHERE lifnr EQ wa_carga_5230-cod_transportadora.

          IF sy-subrc IS INITIAL.
            wa_carga_5230-desc_transp = wa_lfa1-name1.
            MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_good_cells-row_id.
          ENDIF.

          "------------- Check das placas
          IF wa_carga_5230-cod_transportadora EQ 116 AND wa_carga_5230-inco1 EQ 'CIF'.
            IF wa_carga_5230-placa_cav IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '0'
                                                            CHANGING wa_carga_5230-placa_cav
                                                                     vl_check.

              IF vl_check IS NOT INITIAL.
                MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv1_5230->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable_5230.
              ENDIF.
            ENDIF.

            IF wa_carga_5230-placa_car1 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '1'
                                                          CHANGING wa_carga_5230-placa_car1
                                                                   vl_check.

              IF vl_check IS NOT INITIAL.
                MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv1_5230->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable_5230.
              ENDIF.
            ENDIF.

            IF wa_carga_5230-placa_car2 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '1'
                                                          CHANGING wa_carga_5230-placa_car2
                                                                   vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv1_5230->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable_5230.
              ENDIF.
            ENDIF.

*-CS2019001891 - JT - 04.02.2021 - inicio
            IF wa_carga_5230-placa_car3 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '1'
                                                          CHANGING wa_carga_5230-placa_car3
                                                                   vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv1_5230->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable_5230.
              ENDIF.
            ENDIF.
*-CS2019001891 - JT - 04.02.2021 - fim

          ENDIF.
          "----------------------------

          CALL METHOD ctl_alv1_5230->refresh_table_display
            EXPORTING
              is_stable = wa_stable_5230.

        ELSEIF wa_good_cells-fieldname EQ 'MOTORISTA'.

          READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_good_cells-row_id.

          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
            WHERE lifnr EQ wa_carga_5230-motorista.

          IF sy-subrc IS INITIAL.
            wa_carga_5230-desc_mot = wa_lfa1-name1.
            MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_good_cells-row_id.
          ENDIF.

          CALL METHOD ctl_alv1_5230->refresh_table_display
            EXPORTING
              is_stable = wa_stable_5230.

        ELSEIF wa_good_cells-fieldname EQ 'PLACA_CAV'.

          READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_good_cells-row_id.

          IF wa_carga_5230 IS NOT INITIAL.
            "Se não for 0116 Transp Amaggi faz o check
            IF wa_carga_5230-cod_transportadora EQ 116 AND wa_carga_5230-inco1 EQ 'CIF'.


              PERFORM valida_placa_sementes_2 USING '0'
                                              CHANGING wa_carga_5230-placa_cav
                                                       vl_check.

              IF vl_check IS NOT INITIAL.
                MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv1_5230->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable_5230.
              ENDIF.

            ENDIF.
          ENDIF.

        ELSEIF wa_good_cells-fieldname EQ 'PLACA_CAR1'.

          READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_good_cells-row_id.

          IF wa_carga_5230 IS NOT INITIAL.
            "Se não for 0116 Transp Amaggi faz o check
            IF wa_carga_5230-cod_transportadora EQ 116 AND wa_carga_5230-inco1 EQ 'CIF'.

              PERFORM valida_placa_sementes_2 USING '1'
                                              CHANGING wa_carga_5230-placa_car1
                                                       vl_check.

              IF vl_check IS NOT INITIAL.
                MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv1_5230->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable_5230.
              ENDIF.

            ENDIF.
          ENDIF.

        ELSEIF wa_good_cells-fieldname EQ 'PLACA_CAR2'.

          READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_good_cells-row_id.

          IF wa_carga_5230 IS NOT INITIAL.
            "Se não for 0116 Transp Amaggi faz o check
            IF wa_carga_5230-cod_transportadora EQ 116 AND wa_carga_5230-inco1 EQ 'CIF'.

              PERFORM valida_placa_sementes_2 USING '1'
                                              CHANGING wa_carga_5230-placa_car2
                                                       vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv1_5230->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable_5230.
              ENDIF.

            ENDIF.
          ENDIF.

*-CS2019001891 - JT - 04.02.2021 - inicio
        ELSEIF wa_good_cells-fieldname EQ 'PLACA_CAR3'.

          READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_good_cells-row_id.

          IF wa_carga_5230 IS NOT INITIAL.
            "Se não for 0116 Transp Amaggi faz o check
            IF wa_carga_5230-cod_transportadora EQ 116 AND wa_carga_5230-inco1 EQ 'CIF'.

              PERFORM valida_placa_sementes_2 USING '1'
                                              CHANGING wa_carga_5230-placa_car3
                                                       vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_carga_5230 FROM wa_carga_5230 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv1_5230->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable_5230.
              ENDIF.

            ENDIF.
          ENDIF.
*-CS2019001891 - JT - 04.02.2021 - fim

        ELSEIF wa_good_cells-fieldname EQ 'SEQ_ENT_CG'.
          PERFORM atualiza_seq_entrega_5230 USING wa_good_cells.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD handle_hotspot_click_5230.

    DATA: wa_sol_click_5230 TYPE ty_sol_5230,
          wl_name           TYPE thead-tdname.

    IF e_column_id-fieldname EQ 'COD_LOC_EMB'.

      READ TABLE it_sol_click_5230 INTO wa_sol_click_5230 INDEX e_row_id-index.
      IF sy-subrc IS INITIAL.
        wl_name = wa_sol_click_5230-cod_loc_emb.
        PERFORM chama_texto_rot USING wl_name.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD on_double_click_5230.

    DATA: vl_row TYPE i.

    PERFORM alv_sol_click_5230 USING e_row-index.

    CALL METHOD ctl_alv2_5230->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5230_alv2.

    gs_layout_5230_alv2-cwidth_opt = abap_true.

    CALL METHOD ctl_alv2_5230->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5230_alv2.

    CALL METHOD ctl_alv2_5230->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD toolbar_5230_alv2.

    DATA wa_tool TYPE stb_button.


*    IF p_corplt IS NOT INITIAL OR
*       p_corpcg IS NOT INITIAL.
*
*      MOVE 3 TO wa_tool-butn_type.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      wa_tool-function = 'DESVINCULAR'.
*      wa_tool-icon     = '@GC@'.
*      wa_tool-quickinfo = 'Desvincular Cliente'.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      APPEND:
*      VALUE #( butn_type = 3 ) TO e_object->mt_toolbar,
*      VALUE #( function = 'REMOVECARGA' icon     = '@VI@'  quickinfo = 'Desvincular Solicitação' ) TO e_object->mt_toolbar.
*
*      MOVE 3 TO wa_tool-butn_type.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      wa_tool-function = 'EDITSEQENTREGA'.
*      wa_tool-icon     = '@HL@'.
*      wa_tool-quickinfo = 'Editar Sequencia de Entrega'.
*      APPEND wa_tool TO e_object->mt_toolbar.
*      CLEAR wa_tool.
*
*      APPEND:
*      VALUE #( butn_type = 3 ) TO e_object->mt_toolbar,
*      VALUE #( function = 'ADDLOTE' icon = '@XR@'  quickinfo = 'Add Lote de Carga' ) TO e_object->mt_toolbar,
*
*      VALUE #( butn_type = 3 ) TO e_object->mt_toolbar,
*      VALUE #( function = 'CHANGELOCAL' icon = '@A4@'  quickinfo = 'Altera Local de Embarque' ) TO e_object->mt_toolbar.
*
*    ENDIF.

  ENDMETHOD.             "DISPLAY

  METHOD on_f4_5230.

    DATA: it_ret TYPE STANDARD TABLE OF ddshretval,
          it_f4  TYPE STANDARD TABLE OF ty_f4_loc_emb.

    DATA: it_zsdt0132_f4 TYPE STANDARD TABLE OF zsdt0132,
          wa_zsdt0132_f4 TYPE zsdt0132.

    DATA: wa_f4   TYPE ty_f4_loc_emb,
          wa_ret  TYPE ddshretval,
          wa_modi TYPE lvc_s_modi.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA : it_fmap TYPE STANDARD TABLE OF dselc,
           wa_fmap TYPE dselc.

    it_fmap =
    VALUE #(
             ( fldname = 'F0001' dyfldname = 'LIFNR' )
             ( fldname = 'F0002' dyfldname = 'COD_LOC_EMB' )
             ( fldname = 'F0003' dyfldname = 'LOCAL_EMBARQ' )
             ( fldname = 'F0004' dyfldname = 'ARMAZEM' )
             ( fldname = 'F0005' dyfldname = 'TRANSPORTADORA' )
             ( fldname = 'F0006' dyfldname = 'TRANSP_RESP' )
           ).
    DATA(wa_sol) = it_sol_click_5230[ es_row_no-row_id ].


    SELECT *
      FROM zsdt0132
      INTO TABLE it_zsdt0132_f4
      WHERE status NE 'I'
        AND marca  EQ ( SELECT a~marca
                            FROM zsdt0132 AS a
                              INNER JOIN zsdt0131 AS b ON b~cod_loc_emb EQ a~nr_rot
                               WHERE b~nro_lote EQ wa_sol-nro_lote
                                 AND b~nro_sol EQ wa_sol-nro_sol
                                 AND a~status NE 'I'
                       ).

    it_f4 = VALUE #( FOR ls IN it_zsdt0132_f4
    (
        lifnr          = ls-lifnr
        cod_loc_emb    = ls-nr_rot
        local_embarq   = ls-rot_desc
        armazem        = ls-armazem
        transportadora = ls-transportadora
        transp_resp    = ls-transp_resp
     ) ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'LIFNR'
        window_title    = 'Lista de Locais'(002)
        value_org       = 'S'
        dynprofield     = 'LIFNR'
      TABLES
        value_tab       = it_f4
        return_tab      = it_ret
        dynpfld_mapping = it_fmap
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc = 0.
      ASSIGN er_event_data->m_data->* TO <itab>.

      READ TABLE it_ret INTO wa_ret INDEX 1.
      wa_modi-row_id   = es_row_no-row_id.
      wa_modi-fieldname = 'LIFNR'.
      wa_modi-value     = wa_ret-fieldval.
      APPEND wa_modi TO <itab>.

      READ TABLE it_ret INTO wa_ret INDEX 2.
      wa_modi-fieldname = 'COD_LOC_EMB1'.
      wa_modi-value     = wa_ret-fieldval.
      APPEND wa_modi TO <itab>.
      it_sol_click_5230[ es_row_no-row_id ]-cod_loc_emb1 = wa_ret-fieldval.

      READ TABLE it_ret INTO wa_ret INDEX 3.
      wa_modi-fieldname = 'ROT_DESC'.
      wa_modi-value     = wa_ret-fieldval.
      APPEND wa_modi TO <itab>.

      READ TABLE it_ret INTO wa_ret INDEX 3.
      wa_modi-fieldname = 'LOCAL_EMBARQ'.
      wa_modi-value     = wa_ret-fieldval.
      APPEND wa_modi TO <itab>.
      it_sol_click_5230[ es_row_no-row_id ]-local_embarq = wa_ret-fieldval.

*      READ TABLE IT_RET INTO WA_RET INDEX 4.
*      WA_MODI-FIELDNAME = 'ARMAZEM'.
*      WA_MODI-VALUE     = WA_RET-FIELDVAL.
*      APPEND WA_MODI TO <ITAB>.
*      READ TABLE IT_RET INTO WA_RET INDEX 5.
*      WA_MODI-FIELDNAME = 'TRANSPORTADORA'.
*      WA_MODI-VALUE     = WA_RET-FIELDVAL.
*      APPEND WA_MODI TO <ITAB>.
*      READ TABLE IT_RET INTO WA_RET INDEX 6.
*      WA_MODI-FIELDNAME = 'TRANSP_RESP'.
*      WA_MODI-VALUE     = WA_RET-FIELDVAL.
*      APPEND WA_MODI TO <ITAB>.
    ENDIF.

    er_event_data->m_event_handled = 'X'. "(to inform grid that f4 was handled manually)


  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5230  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5230 OUTPUT.

  p_ative = abap_false.

  PERFORM seleciona_carga_5230.
  PERFORM completa_dados_carga_5230.
  PERFORM bloqueia_linhas_5230.
  PERFORM alv_tela_carga_5230.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5230  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5230 INPUT.

  DATA: wa_carga_5230 TYPE ty_carga_5230.

  ctl_alv1_5230->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'SAVE'.
      CLEAR: vl_check.
      PERFORM check_parametro_5230 CHANGING vl_check.
      IF vl_check IS INITIAL.
        PERFORM salvar_edicao_5230.
        MESSAGE TEXT-049 TYPE 'S'.
      ENDIF.
    WHEN 'TABSTRIP_FC1' OR 'TABSTRIP_FC3'.

      LOOP AT it_carga_5230 INTO wa_carga_5230.

        CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
          EXPORTING
            chave = wa_carga_5230-nro_cg.

      ENDLOOP.
    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 80 20.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ALV_TELA_CARGA_5230
*&---------------------------------------------------------------------*
FORM alv_tela_carga_5230.

  IF g_custom_container_5230 IS INITIAL.

    CREATE OBJECT g_custom_container_5230
      EXPORTING
        container_name              = 'CONTAINER5230'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT dg_splitter_1_5230
      EXPORTING
        parent  = g_custom_container_5230
        rows    = 3
        columns = 1.

    CALL METHOD dg_splitter_1_5230->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_5230.

    CALL METHOD dg_splitter_1_5230->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_2_5230.

    CREATE OBJECT dg_splitter_2_5230
      EXPORTING
        parent  = dg_parent_2_5230
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2_5230->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_3_5230.

    CALL METHOD dg_splitter_2_5230->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_4_5230.


    CALL METHOD dg_splitter_1_5230->set_row_mode
      EXPORTING
        mode = dg_splitter_1_5230->mode_relative.

    CALL METHOD dg_splitter_1_5230->set_row_height
      EXPORTING
        id     = 1
        height = 50.

    CALL METHOD dg_splitter_1_5230->set_row_height
      EXPORTING
        id     = 2
        height = 50.

    CALL METHOD dg_splitter_2_5230->set_column_width
      EXPORTING
        id    = 1
        width = 50.

    CALL METHOD dg_splitter_2_5230->set_column_width
      EXPORTING
        id    = 2
        width = 50.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_5230 USING:
          01 'ICONE'                ''             ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status',
          02 'NRO_CG'               ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Carga',
          03 'ROT_DESC'             'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Local Embarque',
          04 'QTD_TOTAL_KG'         'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd Total Kg',
          06 'TVAL'                 ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Valor Carga',
          07 'COD_TRANSPORTADORA'   'ZSDT0133'     ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Transp.',
          08 'DESC_TRANSP'          'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transportadora',
          09 'PRECO_FRETE'          ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Valor Frete',
          10 'PLACA_CAV'            ''             ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Cavalo',
          11 'PLACA_CAR1'           ''             ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta I',
          12 'PLACA_CAR2'           ''             ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta II',
          13 'PLACA_CAR3'           ''             ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Dolly',
          14 'MOTORISTA'            'ZSDT0129'     ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Motorista',
          15 'DESC_MOT'             'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Motorista',
          16 'DT_ENTREGA'           'ZSDT0129'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Prv. Dt. Entrega',
          17 'INCO1'                'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp. Frete',
          18 'DATA_ATUAL'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Data Carga'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5230 USING:
          01 'NRO_CG'             ''     ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Nr. Carga',
          02 'NRO_LOTE'           ''     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Lote',
          03 'VKBUR'              ''     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Vendas',
          04 'SEQ_ENT_CG'         ''         ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Seq. Entrega',
          05 'LIFNR'              'ZSDT0132' 'C310'  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Fornecedor',
          06 'ROT_DESC'           ''     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Local Embarque',
          07 'NAME1'              ''     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nome Cliente',
          08 'VBELN'              ''     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
          09 'MAKTX'              ''     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Produto',
          10 'QTD_VINC'           ''     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd.',
          11 'UM'                 ''     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
          12 'QTD_EMKG'           ''     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. em Kg',
          13 'COD_LOC_EMB'        ''     ' '  ' '  ' '  'X'   'X'   ' '   ' '   ' '   'Roteiro'.

    p_ative = abap_true.
    it_fieldcatalog_sol_5230[ fieldname  = 'LIFNR' ]-no_out = p_ative.

    gs_layout_5230_alv1 =
    VALUE #(
              sel_mode   = 'A'
              stylefname = 'CELLSTYLES'
              cwidth_opt = 'X'
              grid_title = 'Cargas'
              smalltitle = 'X'
           ).

    gs_layout_5230_alv2 =
    VALUE #(
              sel_mode   = 'A'
              stylefname = 'CELLSTYLES'
              cwidth_opt = 'X'
              grid_title = 'Solicitação da Carga'
              smalltitle = 'X'
           ).

    PERFORM sort USING 'NRO_CG' CHANGING it_sort_5230.
    PERFORM excluir_botoes CHANGING it_exclude_5230.

    CREATE OBJECT ctl_alv1_5230
      EXPORTING
        i_parent = dg_parent_1_5230.           "ALV Lote

    CREATE OBJECT ctl_alv2_5230
      EXPORTING
        i_parent = dg_parent_3_5230.           "ALV Oderm

    SET HANDLER:
      lcl_event_handler_5230=>toolbar_5230 FOR ctl_alv1_5230,
      lcl_event_handler_5230=>user_command_5230 FOR ctl_alv1_5230,
      lcl_event_handler_5230=>on_double_click_5230 FOR ctl_alv1_5230,
      lcl_event_handler_5230=>data_changed_finished_5230 FOR ctl_alv1_5230.

    CALL METHOD ctl_alv1_5230->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5230_alv1
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5230
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_5230
        it_outtab            = it_carga_5230
        it_sort              = it_sort_5230.

    CALL METHOD ctl_alv1_5230->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    SET HANDLER: lcl_event_handler_5230=>data_changed_finished_5230 FOR ctl_alv2_5230,
                 lcl_event_handler_5230=>toolbar_5230_alv2          FOR ctl_alv2_5230,
                 lcl_event_handler_5230=>user_command_5230          FOR ctl_alv2_5230,
                 lcl_event_handler_5230=>handle_hotspot_click_5230  FOR ctl_alv2_5230,
                 lcl_event_handler_5230=>on_f4_5230                 FOR ctl_alv2_5230.

    CALL METHOD ctl_alv2_5230->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5230_alv2
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5230
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_sol_5230
        it_outtab            = it_sol_click_5230
        it_sort              = it_sort_5230.

    DATA(gt_f4_5230)  =
    VALUE lvc_t_f4( (
                    fieldname  = 'LIFNR'
                    register   = abap_true
                  ) ).

    CALL METHOD ctl_alv2_5230->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4_5230.


    CALL METHOD ctl_alv2_5230->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    CALL METHOD ctl_alv1_5230->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv2_5230->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_CARGA_5230
*&---------------------------------------------------------------------*
FORM seleciona_carga_5230.

  IF p_transp IS NOT INITIAL.

    SELECT *
        FROM zsdt0133
        INTO CORRESPONDING FIELDS OF TABLE it_carga_5230
        WHERE data_atual IN t_datab
          AND nro_cg     IN t_numcg
          AND EXISTS ( SELECT *
                         FROM zsdt0129
                         INNER JOIN zsdt0131 ON zsdt0129~nro_lote EQ zsdt0131~nro_lote
                         WHERE zsdt0129~nro_cg EQ zsdt0133~nro_cg
                           AND zsdt0129~inco1  IN t_inco1
                           AND zsdt0129~inco1  NE 'FOB'
                           AND zsdt0129~status NE 'X'
                           AND zsdt0131~vkorg  IN t_vkorg
                           AND zsdt0131~spart  EQ t_spart
                           AND zsdt0131~kunnr  IN t_kunnr
                           AND zsdt0131~vbeln  IN t_ovcor
                           AND zsdt0131~status NE 'X'
*                           AND EXISTS ( SELECT *
*                                          FROM ZSDT0135
*                                         WHERE WRKST EQ ZSDT0129~MARCA )
                            ).

    PERFORM check_transp USING 'T'.

*  ELSEIF p_corpcg IS NOT INITIAL. "OR P_CORPLT IS NOT INITIAL.
*
*    SELECT *
*        FROM zsdt0133
*        INTO CORRESPONDING FIELDS OF TABLE it_carga_5230
*        WHERE data_atual IN c_datab
*          AND nro_cg     IN c_numcg
*        AND EXISTS ( SELECT *
*                     FROM zsdt0129
*                     INNER JOIN zsdt0131 ON zsdt0129~nro_lote EQ zsdt0131~nro_lote
*                     WHERE zsdt0129~nro_cg EQ zsdt0133~nro_cg
*                       AND zsdt0129~inco1  IN c_inco1
*                       AND zsdt0129~status NE 'X'
*                       AND zsdt0131~vkorg  IN c_vkorg
*                       AND zsdt0131~spart  EQ c_spart
*                       AND zsdt0131~kunnr  IN c_kunnr
*                       AND zsdt0131~status NE 'X' ).
*
*  ELSEIF p_corplt IS NOT INITIAL.
*
*    SELECT *
*        FROM zsdt0133
*        INTO CORRESPONDING FIELDS OF TABLE it_carga_5230
*        WHERE data_atual IN c_dataa
*          AND nro_cg     IN c_numcg
*        AND EXISTS ( SELECT *
*                     FROM zsdt0129
*                     INNER JOIN zsdt0131 ON zsdt0129~nro_lote EQ zsdt0131~nro_lote
*                     WHERE zsdt0129~nro_cg EQ zsdt0133~nro_cg
*                       AND zsdt0129~inco1  IN c_inco1
*                       AND zsdt0129~status NE 'X'
*                       AND zsdt0131~vkorg  IN c_vkorg
*                       AND zsdt0131~spart  EQ c_spart
*                       AND zsdt0131~kunnr  IN c_kunnr
*                       AND zsdt0131~status NE 'X' ).
*

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_CARGA_5230
*&---------------------------------------------------------------------*
FORM completa_dados_carga_5230 .

  DATA: it_zsdt0130     TYPE STANDARD TABLE OF zsdt0130,
        it_zsdt0129     TYPE STANDARD TABLE OF zsdt0129,
        it_zsdt0132     TYPE STANDARD TABLE OF zsdt0132,
        it_tvkbt        TYPE STANDARD TABLE OF tvkbt,
        it_kna1         TYPE STANDARD TABLE OF kna1,
        it_lfa1         TYPE STANDARD TABLE OF lfa1,
        it_makt         TYPE STANDARD TABLE OF makt,
        it_konv         TYPE STANDARD TABLE OF konv,
        wa_sol_val_5230 TYPE ty_sol_val_5230,
        wa_zsdt0132     TYPE zsdt0132,
        wa_zsdt0129     TYPE zsdt0129,
        wa_zsdt0130     TYPE zsdt0130,
        wa_tvkbt        TYPE tvkbt,
        wa_makt         TYPE makt,
        wa_kna1         TYPE kna1,
        wa_lfa1         TYPE lfa1,
        wa_konv         TYPE konv,
        wa_sol_5230     TYPE ty_sol_5230,
        wa_carga_5230   TYPE ty_carga_5230,
        vl_cont         TYPE i.

  IF it_carga_5230 IS NOT INITIAL.

    "Informações para completar table Carga
    SELECT *
      FROM zsdt0129
      INTO TABLE it_zsdt0129
      FOR ALL ENTRIES IN it_carga_5230
      WHERE nro_cg EQ it_carga_5230-nro_cg
        AND status NE 'X'.

    SELECT *
      FROM lfa1
      INTO TABLE it_lfa1
      FOR ALL ENTRIES IN it_carga_5230
      WHERE lifnr EQ it_carga_5230-cod_transportadora.

    IF it_zsdt0129 IS NOT INITIAL.

      "Informações para completar tabela Solicitação
      SELECT *
        FROM zsdt0131
        INTO CORRESPONDING FIELDS OF TABLE it_sol_5230
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE nro_lote EQ it_zsdt0129-nro_lote
          AND status NE 'X'.

      SELECT zsdt0131~vbeln
             zsdt0131~posnr
             vbak~knumv
        FROM zsdt0131
        INNER JOIN vbak ON zsdt0131~vbeln = vbak~vbeln
        INTO CORRESPONDING FIELDS OF TABLE it_sol_val_5230
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE nro_lote EQ it_zsdt0129-nro_lote
          AND status NE 'X'.

      SELECT *
        FROM lfa1
        APPENDING TABLE it_lfa1
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE lifnr EQ it_zsdt0129-motorista.

      SELECT *
        FROM zsdt0130
        INTO TABLE it_zsdt0130
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE nro_lote EQ it_zsdt0129-nro_lote
          AND status NE 'X'.

      IF it_sol_5230 IS NOT INITIAL.

        SELECT *
          FROM tvkbt
          INTO TABLE it_tvkbt
          FOR ALL ENTRIES IN it_sol_5230
          WHERE spras EQ sy-langu
            AND vkbur EQ it_sol_5230-vkbur.

        SELECT *
          FROM zsdt0132
          INTO TABLE it_zsdt0132
          FOR ALL ENTRIES IN it_sol_5230
          WHERE nr_rot EQ it_sol_5230-cod_loc_emb.

        SELECT *
          FROM kna1
          INTO TABLE it_kna1
          FOR ALL ENTRIES IN it_sol_5230
          WHERE kunnr EQ it_sol_5230-kunnr.

        SELECT *
          FROM makt
          INTO TABLE it_makt
          FOR ALL ENTRIES IN it_sol_5230
          WHERE matnr EQ it_sol_5230-matnr.

      ENDIF.

      IF it_sol_val_5230 IS NOT INITIAL.

        SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @it_sol_val_5230 WHERE knumv EQ @it_sol_val_5230-knumv AND kposn EQ @it_sol_val_5230-posnr AND kschl EQ 'PR00' INTO CORRESPONDING FIELDS OF TABLE @it_konv .







      ENDIF.

    ENDIF.

  ENDIF.

  LOOP AT it_sol_5230 INTO wa_sol_5230.

    vl_cont = vl_cont + 1.

    wa_sol_5230-cod_loc_emb1 = wa_sol_5230-cod_loc_emb.

    READ TABLE it_zsdt0132 INTO wa_zsdt0132 WITH KEY nr_rot = wa_sol_5230-cod_loc_emb.
    IF sy-subrc IS INITIAL.
      wa_sol_5230-rot_desc = wa_zsdt0132-rot_desc.
      wa_sol_5230-lifnr = wa_zsdt0132-lifnr.
    ENDIF.


    CLEAR: wa_sol_5230-cod_loc_emb.
    READ TABLE it_zsdt0130 INTO wa_zsdt0130 WITH KEY nro_lote = wa_sol_5230-nro_lote
                                                     nro_sol  = wa_sol_5230-nro_sol.
    IF sy-subrc IS INITIAL.
      wa_sol_5230-seq_ent_cg = wa_zsdt0130-seq_ent_cg.
      wa_sol_5230-cod_loc_emb = wa_zsdt0130-nr_rot.
    ENDIF.

    READ TABLE it_zsdt0129 INTO wa_zsdt0129 WITH KEY nro_lote = wa_sol_5230-nro_lote.
    IF sy-subrc IS INITIAL.
      wa_sol_5230-nro_cg = wa_zsdt0129-nro_cg.
    ENDIF.

    READ TABLE it_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_sol_5230-vkbur.
    IF sy-subrc IS INITIAL.
      wa_sol_5230-bezei = wa_tvkbt-bezei.
    ENDIF.

    READ TABLE it_zsdt0132 INTO wa_zsdt0132 WITH KEY nr_rot = wa_sol_5230-cod_loc_emb.
    IF sy-subrc IS INITIAL.
      wa_sol_5230-rot_desc = wa_zsdt0132-rot_desc.
    ENDIF.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_sol_5230-kunnr.
    IF sy-subrc IS INITIAL.
      wa_sol_5230-name1 = wa_kna1-name1.
    ENDIF.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_sol_5230-matnr.
    IF sy-subrc IS INITIAL.
      wa_sol_5230-maktx = wa_makt-maktx.
    ENDIF.

    READ TABLE it_sol_val_5230 INTO wa_sol_val_5230 WITH KEY vbeln = wa_sol_5230-vbeln
                                                             posnr = wa_sol_5230-posnr.
    IF sy-subrc IS INITIAL.
      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_sol_val_5230-knumv
                                               kposn = wa_sol_val_5230-posnr.
      IF sy-subrc IS INITIAL.
        IF wa_konv-kmein EQ 'TO'.
          wa_sol_5230-tval = wa_sol_5230-qtd_vinc * wa_sol_5230-brgew * wa_konv-kbetr / 1000 * wa_konv-kkurs.
        ELSEIF wa_konv-kmein EQ 'KG'.
          wa_sol_5230-tval = wa_sol_5230-qtd_vinc * wa_sol_5230-brgew * wa_konv-kbetr / 1 * wa_konv-kkurs.
        ELSE.
          wa_sol_5230-tval = wa_sol_5230-qtd_vinc * wa_konv-kbetr / 1 * wa_konv-kkurs.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY it_sol_5230 FROM wa_sol_5230 INDEX vl_cont.

  ENDLOOP.

  CLEAR: vl_cont.

  LOOP AT it_carga_5230 INTO wa_carga_5230.

    vl_cont = vl_cont + 1.

    IF wa_carga_5230-status EQ '2'.
      wa_carga_5230-icone = '@5D@'.
    ELSEIF wa_carga_5230-status EQ '3'.
      wa_carga_5230-icone =  '@FD@'.
    ELSEIF wa_carga_5230-status EQ '4'.
      wa_carga_5230-icone =  '@4A@'.
    ELSEIF wa_carga_5230-status EQ '5'.
      wa_carga_5230-icone = '@96@'.
    ELSEIF wa_carga_5230-status EQ '6'.
      wa_carga_5230-icone =  '@0Q@'.
    ENDIF.

    LOOP AT it_sol_5230 INTO wa_sol_5230 WHERE nro_cg EQ wa_carga_5230-nro_cg.
      wa_carga_5230-rot_desc = wa_sol_5230-rot_desc.
      wa_carga_5230-tval = wa_carga_5230-tval + wa_sol_5230-tval.
    ENDLOOP.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5230-cod_transportadora.
    IF sy-subrc IS INITIAL.
      wa_carga_5230-desc_transp = wa_lfa1-name1.
    ENDIF.

    READ TABLE it_zsdt0129 INTO wa_zsdt0129 WITH KEY nro_cg = wa_carga_5230-nro_cg.
    IF sy-subrc IS INITIAL.
      wa_carga_5230-dt_entrega = wa_zsdt0129-dt_entrega.
      wa_carga_5230-inco1 = wa_zsdt0129-inco1.
      wa_carga_5230-placa_cav = wa_zsdt0129-placa_cav.
      wa_carga_5230-placa_car1 = wa_zsdt0129-placa_car1.
      wa_carga_5230-placa_car2 = wa_zsdt0129-placa_car2.
      wa_carga_5230-placa_car3 = wa_zsdt0129-placa_car3.
      wa_carga_5230-motorista = wa_zsdt0129-motorista.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5230-motorista.
      IF sy-subrc IS INITIAL.
        wa_carga_5230-desc_mot = wa_lfa1-name1.
      ENDIF.
    ENDIF.

    MODIFY it_carga_5230 FROM wa_carga_5230 INDEX vl_cont.

  ENDLOOP.

  it_carga_bkp_5230 = it_carga_5230.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BLOQUEIA_LINHAS_5230
*&---------------------------------------------------------------------*
FORM bloqueia_linhas_5230 .
  DATA: vl_cont       TYPE i,
        it_celltab    TYPE lvc_t_styl,
        wa_carga_5230 TYPE ty_carga_5230.

  LOOP AT it_carga_5230 INTO wa_carga_5230.
    ADD 1 TO vl_cont.
    CLEAR: it_celltab, wa_carga_5230-cellstyles.

    PERFORM fill_celltab_5230 USING wa_carga_5230-edit
                                    wa_carga_5230-status
                         CHANGING it_celltab.

    INSERT LINES OF it_celltab INTO TABLE wa_carga_5230-cellstyles.
    MODIFY it_carga_5230 FROM wa_carga_5230 INDEX vl_cont.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_SOL_CLICK_5230
*&---------------------------------------------------------------------*
FORM alv_sol_click_5230  USING    p_e_row_index.

  DATA: wa_carga_5230 TYPE ty_carga_5230.

  CLEAR: it_sol_click_5230.

  it_sol_click_5230 = it_sol_5230.
  READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX p_e_row_index.

  DELETE it_sol_click_5230 WHERE nro_cg NE wa_carga_5230-nro_cg.
  p_antigo = abap_true.
  PERFORM bloqueia_linhas_5230_sol.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
FORM fill_celltab_5230  USING    p_edit p_status
                        CHANGING p_it_celltab TYPE lvc_t_styl.

  DATA: wa_celltab     TYPE lvc_s_styl,
        status         TYPE raw4,
        status_inativo TYPE raw4.

  IF p_edit EQ abap_true.
    status = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    status = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  p_it_celltab = VALUE #(
                         ( fieldname = 'COD_TRANSPORTADORA' style = SWITCH #( p_status WHEN 5 OR 6 THEN '00100000' ELSE status ) )
                         ( fieldname = 'PRECO_FRETE'        style = SWITCH #( p_status WHEN 5 OR 6 THEN status ELSE status ) )
                         ( fieldname = 'PLACA_CAV'          style = SWITCH #( p_status WHEN 5 OR 6 THEN '00100000' ELSE status ) )
                         ( fieldname = 'PLACA_CAR1'         style = SWITCH #( p_status WHEN 5 OR 6 THEN '00100000' ELSE status ) )
                         ( fieldname = 'PLACA_CAR2'         style = SWITCH #( p_status WHEN 5 OR 6 THEN '00100000' ELSE status ) )
                         ( fieldname = 'PLACA_CAR3'         style = SWITCH #( p_status WHEN 5 OR 6 THEN '00100000' ELSE status ) )
                         ( fieldname = 'MOTORISTA'          style = SWITCH #( p_status WHEN 5 OR 6 THEN '00100000' ELSE status ) )
                        ).

ENDFORM.                               " FILL_CELLTAB

*&---------------------------------------------------------------------*
*&      Form  GERAR_PDF_EMBARQUE
*&---------------------------------------------------------------------*
FORM gerar_pdf_embarque USING p_nro_cg p_check.

  TYPES: BEGIN OF ty_vbak,
           vbeln TYPE vbak-vbeln,
           posnr TYPE vbap-posnr,
           knumv TYPE vbak-knumv,
         END OF ty_vbak,

         BEGIN OF ty_seq,
           nr_cg TYPE zsdt0129-nro_cg,
           ini   TYPE n LENGTH 2,
           fim   TYPE n LENGTH 2,
         END OF ty_seq.

  DATA: it_zsdt0129 TYPE TABLE OF zsdt0129 WITH HEADER LINE,
        it_kna1     TYPE TABLE OF kna1 WITH HEADER LINE,
        it_zsdt0131 TYPE TABLE OF zsdt0131 WITH HEADER LINE,
        it_zsdt0132 TYPE TABLE OF zsdt0132 WITH HEADER LINE,
        it_zsdt0134 TYPE TABLE OF zsdt0134 WITH HEADER LINE,
        it_adr6     TYPE TABLE OF adr6 WITH HEADER LINE,
        it_vbak     TYPE TABLE OF ty_vbak WITH HEADER LINE,
        it_fat      TYPE TABLE OF zembarquefat WITH HEADER LINE,
        it_pro      TYPE TABLE OF zembarquepro WITH HEADER LINE,
        it_com      TYPE TABLE OF zembarquecom WITH HEADER LINE,
        wa_top      TYPE zembarquetopo,
        wa_tra      TYPE zembarquetrans,
        it_exc      TYPE TABLE OF zsdt0135 WITH HEADER LINE,
        nro_cg      TYPE zsdt0133-nro_cg.

  DATA: vl_formname TYPE tdsfname,
        vl_name     TYPE rs38l_fnam,
        wa_import   TYPE zcontrato_insumos,
        it_itens_im TYPE TABLE OF zcontrato_insumos_itens.
  DATA vl_kunnr TYPE kna1-kunnr.

  DATA r_seq TYPE TABLE OF ty_seq WITH HEADER LINE.

  nro_cg = p_nro_cg.

* Cabeçario de Carga
  SELECT * FROM zsdt0133
    INTO TABLE @DATA(it_zsdt0133)
    WHERE nro_cg EQ @nro_cg.

  CHECK NOT it_zsdt0133[] IS INITIAL.

* Cabeçario de Lote
  SELECT * FROM zsdt0129
    INTO TABLE it_zsdt0129
    FOR ALL ENTRIES IN it_zsdt0133
    WHERE nro_cg EQ it_zsdt0133-nro_cg.

  CHECK NOT it_zsdt0129[] IS INITIAL.

** Marca da Carga
*  SELECT * FROM ZSDT0135
*    INTO TABLE @DATA(IT_ZSDT0135)
*    FOR  ALL ENTRIES IN @IT_ZSDT0129
*    WHERE WRKST EQ @IT_ZSDT0129-MARCA.

* Cliente do Lote
  SELECT * FROM zsdt0130
     INTO TABLE @DATA(it_zsdt0130)
     FOR ALL ENTRIES IN @it_zsdt0129
     WHERE nro_lote EQ @it_zsdt0129-nro_lote
       AND status NE 'X'.

* Ordem do Lote
  SELECT * FROM zsdt0131
    INTO TABLE it_zsdt0131
    FOR ALL ENTRIES IN it_zsdt0129
    WHERE nro_lote EQ it_zsdt0129-nro_lote
      AND status NE 'X'.

  IF NOT it_zsdt0131[] IS INITIAL.

* Controle de Vinculação Pedido X Ordem
    SELECT * FROM zsdt0062
      INTO TABLE @DATA(it_zsdt0062)
      FOR ALL ENTRIES IN @it_zsdt0131
      WHERE nro_cg EQ @nro_cg
        AND vbeln  EQ @it_zsdt0131-vbeln
        AND status EQ 'L'.

    IF NOT it_zsdt0062[] IS INITIAL.
* Cabeçario do Pedido
      SELECT * FROM ekko
        INTO TABLE @DATA(it_ekko)
        FOR ALL ENTRIES IN @it_zsdt0062
        WHERE ebeln EQ @it_zsdt0062-ebeln.
    ENDIF.

    SELECT * FROM zsdt0132
      INTO TABLE it_zsdt0132
      FOR ALL ENTRIES IN it_zsdt0131
      WHERE nr_rot EQ it_zsdt0131-cod_loc_emb.

***Comentado porque esta duplicando as linhas para impressão. (AOENNING).
** Lotes de Produto
*    SELECT * FROM ZSDT0134 AS T0134
*    INNER JOIN ZSDT0129 AS T0129 ON T0134~NRO_CG EQ T0129~NRO_CG
*    INTO CORRESPONDING FIELDS OF TABLE IT_ZSDT0134
*    FOR ALL ENTRIES IN IT_ZSDT0131
*    WHERE T0134~VBELN EQ IT_ZSDT0131-VBELN " PEDIDO
*    AND T0134~POSNR EQ IT_ZSDT0131-POSNR " ITEM
*    AND T0134~NRO_CG EQ NRO_CG. " Carga

    SELECT * FROM zsdt0134
    INTO CORRESPONDING FIELDS OF TABLE it_zsdt0134
    FOR ALL ENTRIES IN it_zsdt0131
    WHERE vbeln EQ it_zsdt0131-vbeln " PEDIDO
    AND posnr EQ it_zsdt0131-posnr " ITEM
    AND nro_cg EQ nro_cg " Carga
* ----->  CS1059274 / IR124771 -->
    AND status NE 'X'.  " Somente não excluidos
* <-----  CS1059274 / IR124771 <--

  ENDIF.

* Dados do Fornecedor
*####################################################
  IF NOT it_ekko[] IS INITIAL.
    SELECT * FROM lfa1
      INTO TABLE @DATA(it_lfa1)
      FOR  ALL ENTRIES IN @it_ekko
      WHERE lifnr EQ @it_ekko-lifnr.
  ENDIF.

  IF NOT it_zsdt0132[] IS INITIAL.
    SELECT * FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_zsdt0132
      WHERE lifnr EQ it_zsdt0132-lifnr.
  ENDIF.

  IF NOT it_zsdt0133[] IS INITIAL.
    SELECT * FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_zsdt0133
      WHERE lifnr EQ it_zsdt0133-cod_transportadora.
  ENDIF.

  IF NOT it_zsdt0129[] IS  INITIAL.
    SELECT * FROM lfa1
      APPENDING TABLE it_lfa1
      FOR ALL ENTRIES IN it_zsdt0129
      WHERE lifnr EQ it_zsdt0129-motorista.

  ENDIF.

  IF NOT it_zsdt0131[] IS INITIAL.
    SELECT * FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_zsdt0131
      WHERE kunnr EQ it_zsdt0131-kunnr.

    SELECT * FROM makt
      INTO TABLE @DATA(it_makt)
      FOR ALL ENTRIES IN @it_zsdt0131
      WHERE matnr EQ @it_zsdt0131-matnr.
  ENDIF.
*####################################################

  IF NOT it_zsdt0129[] IS INITIAL.
    SELECT zsdt0131~vbeln
           zsdt0131~posnr
           vbak~knumv
        FROM zsdt0131
        INNER JOIN vbak ON zsdt0131~vbeln = vbak~vbeln
        INTO CORRESPONDING FIELDS OF TABLE it_vbak
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE nro_lote EQ it_zsdt0129-nro_lote
        AND status NE abap_true.

    IF NOT it_vbak[] IS INITIAL.
      SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @it_vbak WHERE knumv EQ @it_vbak-knumv AND kposn EQ @it_vbak-posnr AND kschl EQ 'PR00' INTO TABLE @DATA(it_konv) .





    ENDIF.
  ENDIF.

  IF NOT it_kna1[] IS INITIAL.

    SELECT * FROM adr6
      INTO TABLE it_adr6
      FOR ALL ENTRIES IN it_kna1
      WHERE addrnumber EQ it_kna1-adrnr.

    SELECT * FROM adr2
      INTO TABLE @DATA(it_adr2)
      FOR ALL ENTRIES IN @it_kna1
      WHERE addrnumber EQ @it_kna1-adrnr.

  ENDIF.

  DATA(it_0131) = it_zsdt0131[].
  SORT it_0131 BY kunnr.
  DELETE ADJACENT DUPLICATES FROM it_0131 COMPARING kunnr.



  LOOP AT it_0131 INTO DATA(wa_0131).

*    IF NOT LINE_EXISTS( IT_ZSDT0135[  WRKST = IT_ZSDT0129[ NRO_LOTE = WA_0131-NRO_LOTE  ]-MARCA ] ).

    DATA wkna1 TYPE kna1.

    vl_kunnr = |{ wa_0131-werks ALPHA = IN }|.

    SELECT SINGLE *
      FROM kna1
      INTO wkna1
      WHERE kunnr EQ vl_kunnr.

    it_fat-cliente  = wkna1-name1.
    it_fat-endereco = wkna1-stras.
    it_fat-cpf_cnpj = wkna1-stcd1.
    it_fat-ins_est  = wkna1-stcd3.
    it_fat-cidade   = wkna1-ort01.
    it_fat-uf       = wkna1-regio.

    SELECT SINGLE smtp_addr FROM adr6
      INTO it_fat-email
    WHERE addrnumber EQ wkna1-adrnr.

    SELECT SINGLE tel_number FROM adr2
      INTO it_fat-contato
      WHERE addrnumber EQ wkna1-adrnr.

    APPEND it_fat.
    CLEAR it_fat.

*      EXIT.
*    ENDIF.

*    TRY .
*        IT_ZSDT0132 = IT_ZSDT0132[ NR_ROT = WA_0131-COD_LOC_EMB ].
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*        CLEAR IT_ZSDT0132.
*    ENDTRY.
*
*    TRY .
*        IT_KNA1 = IT_KNA1[ KUNNR = WA_0131-KUNNR  ].
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*        CLEAR IT_KNA1.
*    ENDTRY.
*
*    TRY .
*        IT_ADR6 = IT_ADR6[ ADDRNUMBER = IT_KNA1-ADRNR ].
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*        CLEAR IT_ADR6.
*    ENDTRY.
*
*    IT_FAT-CLIENTE  = IT_KNA1-NAME1.
*    IT_FAT-ENDERECO = IT_ZSDT0132-ROT_DESC.
*    IT_FAT-CPF_CNPJ = IT_KNA1-STCD1.
*    IT_FAT-INS_EST  = IT_KNA1-STCD3.
*    IT_FAT-CIDADE   = IT_ZSDT0132-CITY1.
*    IT_FAT-UF       = IT_ZSDT0132-UF.
*    IT_FAT-EMAIL    = IT_ADR6-SMTP_ADDR.
*    IT_FAT-CONTATO  = IT_ZSDT0132-TEL_NUMBER.
*
*    IF IT_FAT-CLIENTE IS INITIAL .
*      IT_FAT-CLIENTE  = '_                          _'.
*    ENDIF.
*    IF IT_FAT-ENDERECO  IS INITIAL.
*      IT_FAT-ENDERECO = '_                          _'.
*    ENDIF.
*    IF IT_FAT-CPF_CNPJ IS INITIAL.
*      IT_FAT-CPF_CNPJ = '_                          _'.
*    ENDIF.
*    IF IT_FAT-INS_EST  IS INITIAL.
*      IT_FAT-INS_EST  = '_                          _'.
*    ENDIF.
*    IF IT_FAT-CIDADE   IS INITIAL.
*      IT_FAT-CIDADE   = '_                          _'.
*    ENDIF.
*    IF IT_FAT-UF       IS INITIAL.
*      IT_FAT-UF       = '_                          _'.
*    ENDIF.
*    IF IT_FAT-EMAIL    IS INITIAL.
*      IT_FAT-EMAIL    = '_                          _'.
*    ENDIF.
*    IF IT_FAT-CONTATO  IS INITIAL.
*      IT_FAT-CONTATO  = '_                          _'.
*    ENDIF.

*    APPEND IT_FAT.
*    CLEAR IT_FAT.

  ENDLOOP.

  SORT it_fat BY cpf_cnpj .
  DELETE ADJACENT DUPLICATES FROM it_fat COMPARING cpf_cnpj.

  LOOP AT it_zsdt0129.

    wa_top-nro_cg = it_zsdt0129-nro_cg.
    wa_top-dt_emissao = sy-datum.
    wa_top-icon = it_zsdt0129-inco1.



    LOOP AT it_zsdt0131 INTO DATA(wa_zsdt0131) WHERE nro_lote EQ it_zsdt0129-nro_lote.

      wa_top-spart = wa_zsdt0131-spart.

      CASE wa_top-spart.
        WHEN '04'.  wa_top-desc_spart = 'SEMENTES'.
        WHEN '02'.  wa_top-desc_spart = 'FERTILIZANTES'.
      ENDCASE.

      IF p_check EQ abap_true.

        IF it_zsdt0134[] IS NOT INITIAL.
          LOOP AT it_zsdt0134 INTO DATA(wa_zsdt0134) WHERE vbeln EQ wa_zsdt0131-vbeln " OV
                                                                    AND posnr EQ wa_zsdt0131-posnr. " ITEM
            it_pro-nro_cg     = it_zsdt0129-nro_cg.

            it_pro-qtd_vinc   = wa_zsdt0134-lfimg.
            it_pro-um         = wa_zsdt0131-um.
            it_pro-lote     = wa_zsdt0134-charg.

            TRY .
                DATA(wa_zsdt0132) = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
                it_pro-local      = wa_zsdt0132-rot_desc.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            TRY .
                DATA(wa_zsdt0130) = it_zsdt0130[ nro_lote = it_zsdt0129-nro_lote
                                                 nro_sol = wa_zsdt0131-nro_sol ].
                it_pro-seq_ent_cg = wa_zsdt0130-seq_ent_cg.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            TRY .
                it_pro-name1 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-name1.
                it_pro-mcod3 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-mcod3.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            TRY .
                it_pro-maktx = it_makt[ matnr = wa_zsdt0131-matnr ]-maktx.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            it_pro-nr_rot = wa_zsdt0130-nr_rot.

            TRY .
                DATA(wa_vbak) = it_vbak[ vbeln = wa_zsdt0131-vbeln
                                         posnr = wa_zsdt0131-posnr ].
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            TRY .
                DATA(wa_konv) = it_konv[ knumv = wa_vbak-knumv
                                         kposn = wa_vbak-posnr ].
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.


            CASE wa_konv-kmein.
              WHEN 'TO'.
                it_pro-preco_frete = ( ( ( ( wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew ) * wa_konv-kbetr ) / 1000 ) * wa_konv-kkurs ).
              WHEN OTHERS.
                it_pro-preco_frete = ( ( wa_zsdt0131-qtd_vinc * wa_konv-kbetr ) * wa_konv-kkurs ).
            ENDCASE.

            r_seq-ini    = it_pro-seq_ent_cg.
            r_seq-nr_cg  = it_pro-nro_cg.
            APPEND r_seq.
            CLEAR r_seq.

            APPEND it_pro .
            CLEAR it_pro .
          ENDLOOP.
        ELSE.
          "QUANDO NÃO POSSUI LOTE DE PRODUTO VINCULADO
          it_pro-nro_cg     = it_zsdt0129-nro_cg.

          it_pro-qtd_vinc   = wa_zsdt0131-qtd_vinc.
          it_pro-um         = wa_zsdt0131-um.
          it_pro-lote     = ' '.

          TRY .
              wa_zsdt0132 = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
              it_pro-local      = wa_zsdt0132-rot_desc.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY .
              wa_zsdt0130 = it_zsdt0130[ nro_lote = it_zsdt0129-nro_lote
                                               nro_sol = wa_zsdt0131-nro_sol ].
              it_pro-seq_ent_cg = wa_zsdt0130-seq_ent_cg.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY .
              it_pro-name1 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-name1.
              it_pro-mcod3 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-mcod3.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY .
              it_pro-maktx = it_makt[ matnr = wa_zsdt0131-matnr ]-maktx.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          it_pro-nr_rot = wa_zsdt0130-nr_rot.

          TRY .
              wa_vbak = it_vbak[ vbeln = wa_zsdt0131-vbeln
                                       posnr = wa_zsdt0131-posnr ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY .
              wa_konv = it_konv[ knumv = wa_vbak-knumv
                                       kposn = wa_vbak-posnr ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.


          CASE wa_konv-kmein.
            WHEN 'TO'.
              it_pro-preco_frete = ( ( ( ( wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew ) * wa_konv-kbetr ) / 1000 ) * wa_konv-kkurs ).
            WHEN OTHERS.
              it_pro-preco_frete = ( ( wa_zsdt0131-qtd_vinc * wa_konv-kbetr ) * wa_konv-kkurs ).
          ENDCASE.

          r_seq-ini    = it_pro-seq_ent_cg.
          r_seq-nr_cg  = it_pro-nro_cg.
          APPEND r_seq.
          CLEAR r_seq.

          APPEND it_pro .
          CLEAR it_pro .
        ENDIF.

        TRY.
            DATA(wa_0132) = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        it_com-local_embarque = wa_0132-rot_desc.
        it_com-municipio = wa_0132-city1.
        it_com-endereco = it_lfa1[ lifnr = wa_0132-lifnr ]-stras.
        it_com-uf = wa_0132-uf.

        APPEND it_com.

        CLEAR it_com.

      ELSE.

        it_pro-nro_cg     = it_zsdt0129-nro_cg.

        it_pro-vkbur      = wa_zsdt0131-vkbur.
        it_pro-vbeln      = wa_zsdt0131-vbeln.

        it_pro-qtd_vinc   = wa_zsdt0131-qtd_vinc.
        it_pro-um         = wa_zsdt0131-um.
        it_pro-qtd_emkg   = wa_zsdt0131-qtd_emkg.

        TRY .
            DATA(wa_zsdt0130_pro) = it_zsdt0130[ nro_lote = it_zsdt0129-nro_lote
                                             nro_sol = wa_zsdt0131-nro_sol ].
            it_pro-seq_ent_cg = wa_zsdt0130_pro-seq_ent_cg.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        TRY .
            it_pro-name1 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-name1.
            it_pro-mcod3 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-mcod3.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        TRY .
            it_pro-maktx = it_makt[ matnr = wa_zsdt0131-matnr ]-maktx.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        TRY .
            wa_zsdt0132 = it_zsdt0132[ nr_rot = wa_zsdt0130-nr_rot ].
            it_pro-fazenda    = wa_zsdt0132-rot_desc.
            it_pro-tel_number = wa_zsdt0132-tel_number.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        it_pro-nr_rot = wa_zsdt0130-nr_rot.

        TRY .
            DATA(wa_vbak_pro) = it_vbak[ vbeln = wa_zsdt0131-vbeln
                                     posnr = wa_zsdt0131-posnr ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        TRY .
            DATA(wa_konv_pro) = it_konv[ knumv = wa_vbak_pro-knumv
                                     kposn = wa_vbak_pro-posnr ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        CASE wa_konv-kmein.
          WHEN 'TO'.
            it_pro-preco_frete = ( ( ( ( wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew ) * wa_konv_pro-kbetr ) / 1000 ) * wa_konv_pro-kkurs ).
          WHEN OTHERS.
            it_pro-preco_frete = ( ( wa_zsdt0131-qtd_vinc * wa_konv_pro-kbetr ) * wa_konv_pro-kkurs ).
        ENDCASE.

        r_seq-ini    = it_pro-seq_ent_cg.
        r_seq-nr_cg  = it_pro-nro_cg.
        APPEND r_seq.
        CLEAR r_seq.

        TRY.
            DATA(wa_0132_pro) = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        LOOP AT it_zsdt0062 INTO DATA(wa_zsdt0062) WHERE vbeln EQ wa_zsdt0131-vbeln AND nro_cg EQ it_zsdt0129-nro_cg.

          it_com-local_embarque = wa_0132_pro-rot_desc.
          it_com-municipio = wa_0132_pro-city1.
          it_com-endereco = it_lfa1[ lifnr = wa_0132_pro-lifnr ]-stras.
          it_com-uf = wa_0132_pro-uf.

          TRY .
              it_com-fornecedor = it_lfa1[ lifnr = it_ekko[ lifnr = wa_zsdt0062-lifnr ]-lifnr ]-name1.
            CATCH cx_sy_itab_line_not_found.
              CLEAR it_com-fornecedor.
          ENDTRY.

          TRY .
              it_com-ihrez = it_ekko[ ebeln = wa_zsdt0062-ebeln ]-ihrez.
            CATCH cx_sy_itab_line_not_found.
              CLEAR it_com-ihrez.
          ENDTRY.

*          IF NOT LINE_EXISTS( IT_ZSDT0135[  WRKST = IT_ZSDT0129-MARCA ] ).
          it_pro-wrkst = it_com-ihrez.
*          ELSE.
*            IT_PRO-WRKST = WA_ZSDT0062-VBELN.
*          ENDIF.

          it_com-ebeln = wa_zsdt0062-ebeln.

          IF NOT line_exists( it_com[ ebeln = wa_zsdt0062-ebeln ] ).
            APPEND it_com.
          ENDIF.

          CLEAR it_com.

        ENDLOOP.
        APPEND it_pro .
        CLEAR it_pro .
      ENDIF.

    ENDLOOP.

  ENDLOOP.

  SORT r_seq BY nr_cg ini.
  DELETE ADJACENT DUPLICATES FROM r_seq COMPARING nr_cg ini.

  DATA cont TYPE n LENGTH 2.

  LOOP AT r_seq .

    cont = 0.

    LOOP AT r_seq WHERE nr_cg EQ r_seq-nr_cg.
      ADD 1 TO cont.
    ENDLOOP.

    LOOP AT r_seq ASSIGNING FIELD-SYMBOL(<seq>) WHERE nr_cg EQ r_seq-nr_cg.
      <seq>-fim    = cont.
      SUBTRACT 1 FROM cont.
    ENDLOOP.

  ENDLOOP.

  LOOP AT it_pro  ASSIGNING FIELD-SYMBOL(<exc>).

    r_seq = r_seq[ nr_cg = <exc>-nro_cg
                     ini = <exc>-seq_ent_cg ].
    IF r_seq-ini EQ <exc>-seq_ent_cg AND
       r_seq-nr_cg EQ <exc>-nro_cg.
      <exc>-seq_ent_cg = r_seq-fim.
    ENDIF.

  ENDLOOP.

  SORT it_pro[] BY seq_ent_cg.

  LOOP AT it_zsdt0133 INTO DATA(wa_0133).

    TRY .
        wa_tra-transportadora = it_lfa1[ lifnr = wa_0133-cod_transportadora ]-name1.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-transportadora.
    ENDTRY.

    TRY .
        wa_tra-cnpj  = it_lfa1[ lifnr = wa_0133-cod_transportadora ]-stcd1.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-cnpj.
    ENDTRY.

    TRY .
        wa_tra-nome_motorista = it_lfa1[ lifnr = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-motorista ]-name1.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-nome_motorista.
    ENDTRY.

    TRY .
        wa_tra-cpf = it_lfa1[ lifnr = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-motorista ]-stcd2.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-cpf.
    ENDTRY.

    TRY .
        wa_tra-contato = it_lfa1[ lifnr = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-motorista ]-telf1.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-contato.
    ENDTRY.

    TRY .
        wa_tra-placa_cavalo = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-placa_cav.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-placa_cavalo.
    ENDTRY.

    TRY .
        wa_tra-placa_carreta1 = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-placa_car1.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-placa_carreta1.
    ENDTRY.

    TRY .
        wa_tra-placa_carreta2 = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-placa_car2.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-placa_carreta2.
    ENDTRY.

*-CS2019001891 - JT - 04.02.2021 - inicio
    TRY .
        wa_tra-placa_carreta3 = it_zsdt0129[ nro_cg = wa_0133-nro_cg ]-placa_car3.
      CATCH cx_sy_itab_line_not_found.
        CLEAR wa_tra-placa_carreta3.
    ENDTRY.
*-CS2019001891 - JT - 04.02.2021 - fim

  ENDLOOP.
  IF p_check EQ abap_false.
    vl_formname = 'ZSDF0003'.
  ELSE.
    vl_formname = 'ZSDF0013'.
  ENDIF.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'. "SY-MSGTY
*    EXIT.
  ENDIF.

  CALL FUNCTION vl_name
    EXPORTING
      wa_top           = wa_top
      wa_tra           = wa_tra
    TABLES
      it_fat           = it_fat
      it_pro           = it_pro
      it_com           = it_com
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'. "SY-MSGTY
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVAR_EDICAO_5230
*&---------------------------------------------------------------------*
FORM salvar_edicao_5230 .

  DATA: it_zsdt0129       TYPE STANDARD TABLE OF zsdt0129,
        it_zsdt0133       TYPE STANDARD TABLE OF zsdt0133,
        it_carga_del_5230 TYPE STANDARD TABLE OF ty_carga_5230,
        wa_carga_5230     TYPE ty_carga_5230,
        wa_zsdt0129       TYPE zsdt0129,
        wa_zsdt0131       TYPE zsdt0131.

  it_carga_del_5230 = it_carga_5230.
  DELETE it_carga_del_5230 WHERE edit NE 'X'.



  "IF P_ANTIGO NE ABAP_TRUE.
  LOOP AT it_sol_click_5230 INTO DATA(wa_sol_click_5230).

    UPDATE zsdt0130 SET seq_ent_cg = wa_sol_click_5230-seq_ent_cg
                 WHERE nro_lote EQ wa_sol_click_5230-nro_lote
                   AND nro_sol EQ wa_sol_click_5230-nro_sol.
    CLEAR wa_sol_click_5230.
  ENDLOOP.
  "ENDIF.


  SELECT *
    FROM zsdt0133
    INTO TABLE it_zsdt0133
    FOR ALL ENTRIES IN it_carga_del_5230
    WHERE nro_cg EQ it_carga_del_5230-nro_cg.

  SELECT *
    FROM zsdt0129
    INTO TABLE it_zsdt0129
    FOR ALL ENTRIES IN it_carga_del_5230
    WHERE nro_cg EQ it_carga_del_5230-nro_cg.

  IF it_sol_click_5230 IS NOT INITIAL.
    SELECT *
        FROM zsdt0131
        INTO TABLE @DATA(t_0131)
         FOR ALL ENTRIES IN @it_sol_click_5230
            WHERE nro_lote EQ @it_sol_click_5230-nro_lote
              AND nro_sol EQ @it_sol_click_5230-nro_sol.

    LOOP AT t_0131 INTO DATA(wa_0131).
      READ TABLE it_sol_click_5230 INTO DATA(w_5230) WITH KEY nro_lote = wa_0131-nro_lote
                                                              nro_sol  = wa_0131-nro_sol.
      IF sy-subrc IS INITIAL.
        wa_0131-cod_loc_emb = w_5230-cod_loc_emb1.
        wa_0131-local_embarq = w_5230-rot_desc.
        MODIFY zsdt0131 FROM wa_0131.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT it_zsdt0133 INTO wa_zsdt0133.
    READ TABLE it_carga_del_5230 INTO wa_carga_5230 WITH KEY nro_cg = wa_zsdt0133-nro_cg.
    IF sy-subrc IS INITIAL.
      wa_zsdt0133-cod_transportadora = wa_carga_5230-cod_transportadora.
      wa_zsdt0133-preco_frete = wa_carga_5230-preco_frete.
      MODIFY zsdt0133 FROM wa_zsdt0133.
    ENDIF.
  ENDLOOP.

  LOOP AT it_zsdt0129 INTO wa_zsdt0129.
    READ TABLE it_carga_del_5230 INTO wa_carga_5230 WITH KEY nro_cg = wa_zsdt0129-nro_cg.
    IF sy-subrc IS INITIAL.
      wa_zsdt0129-placa_cav = wa_carga_5230-placa_cav.
      wa_zsdt0129-placa_car1 = wa_carga_5230-placa_car1.
      wa_zsdt0129-placa_car2 = wa_carga_5230-placa_car2.
      wa_zsdt0129-placa_car3 = wa_carga_5230-placa_car3.
      wa_zsdt0129-motorista = wa_carga_5230-motorista.

      MODIFY zsdt0129 FROM wa_zsdt0129.
    ENDIF.
  ENDLOOP.

  LOOP AT it_carga_del_5230 INTO wa_carga_5230.

    CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
      EXPORTING
        chave = wa_carga_5230-nro_cg.

  ENDLOOP.
  p_antigo = abap_true.
  PERFORM bloqueia_linhas_5230_sol.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_LOTES_PRODUTOS_5230
*&---------------------------------------------------------------------*
FORM busca_lotes_produtos_5230 CHANGING vl_check.

  DATA: it_zsdt0131 TYPE STANDARD TABLE OF zsdt0131,
        it_zsdt0134 TYPE STANDARD TABLE OF zsdt0134,
        it_zsdt0129 TYPE STANDARD TABLE OF zsdt0129,
        wa_zsdt0134 TYPE zsdt0134,
        vl_cont     TYPE i.

  SELECT *
    FROM zsdt0129
    INTO TABLE it_zsdt0129
    FOR ALL ENTRIES IN it_carga_canc_5230
    WHERE nro_cg EQ it_carga_canc_5230-nro_cg
      AND status NE 'X'.

  SELECT *
    FROM zsdt0131
    INTO TABLE it_zsdt0131
    FOR ALL ENTRIES IN it_zsdt0129
    WHERE nro_lote EQ it_zsdt0129-nro_lote
      AND status   NE 'X'.

  SELECT *
    FROM zsdt0134
    INTO TABLE it_zsdt0134
    FOR ALL ENTRIES IN it_zsdt0131
    WHERE vbeln  EQ it_zsdt0131-vbeln
      AND posnr  EQ it_zsdt0131-posnr
      AND status NE 'X'.

  LOOP AT it_zsdt0134 INTO wa_zsdt0134.
    vl_cont = vl_cont + 1.
    READ TABLE it_carga_canc_5230 WITH KEY nro_cg = wa_zsdt0134-nro_cg TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      wa_zsdt0134-status = 'K'.
    ENDIF.
    MODIFY it_zsdt0134 FROM wa_zsdt0134 INDEX vl_cont TRANSPORTING status.
  ENDLOOP.

  DELETE it_zsdt0134 WHERE status EQ 'K'.

  IF it_zsdt0134 IS NOT INITIAL.
    vl_check = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CANCELA_AUTORIZACAO_5230
*&---------------------------------------------------------------------*
FORM cancela_autorizacao_5230 .

  DATA: it_zsdt0133 TYPE STANDARD TABLE OF zsdt0133,
        wa_zsdt0133 TYPE zsdt0133.

  SELECT *
    FROM zsdt0133
    INTO TABLE it_zsdt0133
    FOR ALL ENTRIES IN it_carga_canc_5230
    WHERE nro_cg EQ it_carga_canc_5230-nro_cg
      AND status NE 'X'.

  LOOP AT it_zsdt0133 INTO wa_zsdt0133.
    IF wa_zsdt0133-status EQ 5.
      wa_zsdt0133-status = 4.
    ENDIF.
    MODIFY zsdt0133 FROM wa_zsdt0133.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BLOQUEIA_LINHAS
*&---------------------------------------------------------------------*
FORM bloqueia_linhas_5230_sol .
  DATA: it_celltab  TYPE lvc_t_styl,
        wa_zsdt0131 TYPE ty_sol_5230.
  vl_cont = 0.

  LOOP AT it_sol_click_5230  INTO wa_zsdt0131.
    vl_cont = vl_cont + 1.
    CLEAR: it_celltab, wa_zsdt0131-cellstyles.
    PERFORM fill_celltab USING p_antigo
                          CHANGING it_celltab.

    INSERT LINES OF it_celltab INTO TABLE wa_zsdt0131-cellstyles.
    MODIFY it_sol_click_5230 FROM wa_zsdt0131 INDEX vl_cont.
  ENDLOOP.

  CALL METHOD ctl_alv2_5230->refresh_table_display
    EXPORTING
      is_stable = wa_stable_5230.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_TRANSP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_CARGA_5230  text
*----------------------------------------------------------------------*
FORM check_transp USING  p_vl.

* // Pega os Parametros do Usuario
  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = _param
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.

* // Deleto todos os parametros diferente de ZTRANS
  DELETE _param WHERE parid NE 'ZTRANS'.

  IF _param[] IS NOT INITIAL.
*   "// Pega as informações do parametro
    DATA(_transportadora) = _param[ 1 ]-parva.

*   "// separo a quantidade de parametro no item
    SPLIT _transportadora AT space INTO TABLE t_trans.
*   "// Separa em uma Range de Parametros
    r_trans = VALUE #( FOR ls IN t_trans ( sign = 'I' option = 'EQ' low = ls ) ).
  ENDIF.

* "// caso seja * "Asterisco" pegar todos os Parametros cadastrados
  IF _transportadora EQ '*'.

    SELECT *
      FROM tvbur
      INTO TABLE @DATA(t_tvbur)
      WHERE vkbur LIKE 'T%'.

    r_trans = VALUE #( FOR ls3 IN t_tvbur ( sign = 'I' option = 'EQ' low = ls3-vkbur ) ).

  ENDIF.

*-IR108506-29.08.2022-Ativar versao-JT-inicio
* "// Exclui o parametro TCOR dependendo da Visão que estiver Visualizando
  IF p_vl EQ 'T'.
    DELETE r_trans WHERE low EQ 'TCOR'. "// Transportadora
  ELSE.
    DELETE r_trans WHERE low NE 'TCOR'. "// Corporativo
  ENDIF.
*-CS2019001891 - 28.06.2021 - JT - fim
*-IR108506-29.08.2022-Ativar versao-JT-fim

* // caso no usuario não tenha parametro sera excluido todos os item da carga
  IF r_trans IS INITIAL.
    IF p_vl EQ 'T'.
      FREE it_carga_5230. "// Transportadora
    ELSE.
      FREE: it_carga_scot_5220, it_carga_ccot_5220. "// Corporativo
    ENDIF.
    EXIT.
  ENDIF.

  IF p_vl EQ 'T'.

    LOOP AT it_carga_5230 ASSIGNING FIELD-SYMBOL(<f_carga>).

      SELECT *
        FROM zsdt0129 AS a
        INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
        INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
        INTO TABLE @DATA(w_0129)
        WHERE a~nro_cg EQ @<f_carga>-nro_cg
          AND c~transp_resp IN @r_trans.

      IF sy-subrc IS NOT INITIAL.
        <f_carga>-nro_cg = '9999999999'.
      ENDIF.

    ENDLOOP.

    DELETE it_carga_5230 WHERE nro_cg EQ '9999999999'.

  ELSE.

    LOOP AT it_carga_ccot_5220 ASSIGNING FIELD-SYMBOL(<f_cargac>).

      SELECT *
        FROM zsdt0129 AS a
        INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
        INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
        INTO TABLE w_0129
        WHERE a~nro_cg EQ <f_cargac>-nro_cg
          AND c~transp_resp IN r_trans.

      IF sy-subrc IS NOT INITIAL.
        <f_cargac>-nro_cg = '9999999999'.
      ENDIF.

    ENDLOOP.

    DELETE it_carga_ccot_5220 WHERE nro_cg EQ '9999999999'.

    LOOP AT it_carga_scot_5220 ASSIGNING FIELD-SYMBOL(<f_cargas>).

      SELECT *
        FROM zsdt0129 AS a
        INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
        INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
        INTO TABLE w_0129
        WHERE a~nro_cg EQ <f_cargas>-nro_cg
          AND c~transp_resp IN r_trans.

      IF sy-subrc IS NOT INITIAL.
        <f_cargas>-nro_cg = '9999999999'.
      ENDIF.

    ENDLOOP.

    DELETE it_carga_scot_5220 WHERE nro_cg EQ '9999999999'.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
FORM fill_celltab USING p_antig
                  CHANGING p_it_celltab TYPE lvc_t_styl.


  DATA: wa_celltab TYPE lvc_s_styl,
        status     TYPE raw4.

  IF p_antig EQ abap_true.
    status = cl_gui_alv_grid=>mc_style_disabled.
  ELSE.
    status = cl_gui_alv_grid=>mc_style_enabled.
  ENDIF.

  wa_celltab-fieldname = 'SEQ_ENT_CG'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.

ENDFORM.                               " FILL_CELLTAB


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_SEQ_ENTREGA_5140
*&---------------------------------------------------------------------*
FORM atualiza_seq_entrega_5230 USING wa_good_cells TYPE lvc_s_modi.

  DATA: vl_roteiro        TYPE zsdt0130-nr_rot,
        wa_sol_click_5230 TYPE ty_sol_5230,
        it_zsdt0130       TYPE STANDARD TABLE OF zsdt0130.

  READ TABLE it_sol_click_5230 INTO wa_sol_click_5230 INDEX wa_good_cells-row_id.

  vl_roteiro = wa_sol_click_5230-cod_loc_emb.

  LOOP AT it_sol_click_5230 ASSIGNING FIELD-SYMBOL(<wa_sol_click_5230>) WHERE cod_loc_emb EQ vl_roteiro.

    <wa_sol_click_5230>-seq_ent_cg = wa_good_cells-value.

  ENDLOOP.

  CALL METHOD ctl_alv2_5230->refresh_table_display
    EXPORTING
      is_stable = wa_stable_5230.

ENDFORM.



FORM check_parametro_5230 CHANGING vl_check.


  LOOP AT it_sol_click_5230 INTO DATA(wa_cliente).
    "Check solicitação sem sequência de entrega
    IF wa_cliente-seq_ent_cg IS INITIAL.
      MESSAGE TEXT-016 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF it_sol_click_5230 IS NOT INITIAL.
*    DATA(T_0132) = IT_SOL_CLICK_5230.

*    SELECT A~*
*      FROM ZSDT0132 AS A
*      INNER JOIN ZSDT0131 AS B ON B~COD_LOC_EMB EQ A~NR_ROT
*          INTO TABLE @DATA(T_0132)
*            FOR ALL ENTRIES IN @IT_SOL_CLICK_5230
*          WHERE B~NRO_LOTE EQ @IT_SOL_CLICK_5230-NRO_LOTE
*               AND B~NRO_SOL EQ @IT_SOL_CLICK_5230-NRO_SOL.

    SELECT *
      FROM zsdt0132
       INTO TABLE @DATA(t_0132)
         FOR ALL ENTRIES IN @it_sol_click_5230
       WHERE nr_rot EQ @it_sol_click_5230-cod_loc_emb1.

    SORT t_0132 BY armazem transportadora transp_resp.
    DELETE ADJACENT DUPLICATES FROM t_0132 COMPARING armazem transportadora transp_resp.

    IF lines( t_0132 ) NE 1.
      MESSAGE TEXT-144 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  "Check solicitação sem sequência correta
  it_solicitacoes_5230_check = it_sol_click_5230.
  SORT it_solicitacoes_5230_check BY seq_ent_cg ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_solicitacoes_5230_check COMPARING seq_ent_cg.
  LOOP AT it_solicitacoes_5230_check INTO DATA(wa_cliente_5140).
    IF wa_cliente_5140-seq_ent_cg NE sy-tabix.
      MESSAGE TEXT-143 TYPE 'S' DISPLAY LIKE 'W'.
*      VL_CHECK = ABAP_TRUE.
    ENDIF.
  ENDLOOP.


ENDFORM.

FORM validar_dados_vinculacao_5230 RAISING cx_abap_util_exception.
  DATA: r_matnr   TYPE RANGE OF mara-matnr,
        w_matnr   LIKE LINE OF r_matnr,
        w_matnr18 TYPE matnr.
  DATA categoria(1) VALUE 'F'.    "//pedido;
  DATA count          TYPE i.
  DATA it_selected_rows  TYPE lvc_t_row.
  CALL METHOD ctl_alv1_5230->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.


  IF ( popup_vinculacao-material IS NOT INITIAL ).

    DATA(_material) = popup_vinculacao-material.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = _material
      IMPORTING
        output = w_matnr18.
    _material = w_matnr18.
  ENDIF.



  IF ( popup_vinculacao-material IS NOT INITIAL AND  popup_vinculacao-pedido IS INITIAL ).

*    DATA(_material) = popup_vinculacao-material.
*
*     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = _material
*        IMPORTING
*          output = _material.

    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
    READ TABLE it_carga_5230 INTO wa_carga_5230 INDEX wa_selected_rows-index.

    DELETE it_sol_click_5230 WHERE nro_cg NE wa_carga_5230-nro_cg.

    READ TABLE it_sol_click_5230 INTO DATA(w_sol_click_5230) INDEX 1.

    SELECT SINGLE spart INTO @DATA(spart_mat) FROM mara WHERE matnr EQ @_material.

    IF spart_mat EQ w_sol_click_5230-spart.



      SELECT COUNT( * )
        FROM ekpo AS a
  INNER JOIN ekko AS b ON b~ebeln EQ a~ebeln
       WHERE a~matnr = _material
         AND b~bstyp = categoria.

      IF NOT sy-subrc IS INITIAL .
        MESSAGE TEXT-128 TYPE 'I' DISPLAY LIKE 'E'.
        RAISE EXCEPTION TYPE cx_abap_util_exception.
      ENDIF.

    ELSE.
      CONCATENATE 'Setor de atividade ' spart_mat 'do material ' _material 'é diferente do Setor de Atividade' w_sol_click_5230-spart '' INTO DATA(mensagem) SEPARATED BY space.
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
      IF w_spart-spart EQ w_sol_click_5230-spart.
        w_matnr-sign = 'I'.
        w_matnr-option = 'eq'.
        w_matnr-low = w_spart-matnr.
        APPEND w_matnr TO r_matnr.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    IF r_matnr[] IS NOT INITIAL.

      SELECT COUNT( * )
        FROM ekpo AS a
  INNER JOIN ekko AS b ON b~ebeln EQ a~ebeln
       WHERE a~matnr IN r_matnr
         AND b~bstyp = categoria.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE TEXT-129 TYPE 'S' DISPLAY LIKE 'E'.
        RAISE EXCEPTION TYPE cx_abap_util_exception.
      ENDIF.
    ELSE.
      CONCATENATE 'Setor de atividade dos materiais do pedido são diferentes do Setor de Atividade' w_sol_click_5230-spart 'do material da OV'INTO mensagem SEPARATED BY space.
      MESSAGE mensagem TYPE 'S' DISPLAY LIKE 'E'.
      RAISE EXCEPTION TYPE cx_abap_util_exception.
    ENDIF.
  ELSEIF ( popup_vinculacao-material IS NOT INITIAL AND  popup_vinculacao-pedido IS NOT INITIAL ).

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
        IF w_spart_pedido-spart EQ w_sol_click_5230-spart.
          w_matnr-sign = 'I'.
          w_matnr-option = 'eq'.
          w_matnr-low = w_spart_pedido-matnr.
          APPEND w_matnr TO r_matnr.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      IF r_matnr[] IS NOT INITIAL.

        SELECT COUNT( * )
          FROM ekpo AS a
    INNER JOIN ekko AS b ON b~ebeln EQ a~ebeln
         WHERE a~matnr IN r_matnr
           AND b~bstyp = categoria.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE TEXT-129 TYPE 'S' DISPLAY LIKE 'E'.
          RAISE EXCEPTION TYPE cx_abap_util_exception.
        ENDIF.

      ELSE.
        CONCATENATE 'Setor de atividade ' w_spart_pedido-spart 'do material ' w_spart_pedido-matnr 'é diferente do Setor de Atividade' w_sol_click_5230-spart 'do material ' w_sol_click_5230-matnr 'da OV.' INTO mensagem SEPARATED BY space.
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
