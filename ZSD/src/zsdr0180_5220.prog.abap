*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5220
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_carga_5220,
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
         inco1       TYPE zsdt0129-inco1.
         INCLUDE STRUCTURE zsdt0133.
TYPES:   cellstyles  TYPE lvc_t_styl.
TYPES: END OF ty_carga_5220.

"FF #154848 - inicio
TYPES: BEGIN OF ty_iti_carga_5220,
         edit             TYPE char1,
         nro_cg           TYPE zsdt0346-nro_carga,
         operacao         TYPE zsdt0345-operacao,
         tipo_transporte  TYPE zsdt0345-tipo_transporte,
         ponto_coleta     TYPE zsdt0346-ponto_coleta,
         local_entrega    TYPE zsdt0346-local_entrega,
         itinerario       TYPE zsdt0346-itinerario,
         distancia_km     TYPE tvro-distz,
         unidade_condicao TYPE zsdt0346-unidade_condicao,
         valor            TYPE zsdt0346-valor,
         unidade_medida   TYPE zsdt0346-unidade_medida.
TYPES:   cellstyles  TYPE lvc_t_styl.
TYPES: END OF ty_iti_carga_5220.
"FF #154848 - fim



TYPES: BEGIN OF ty_sol_val_5220,
         vbeln TYPE zsdt0131-vbeln,
         posnr TYPE zsdt0131-posnr,
         knumv TYPE vbak-knumv.
TYPES: END OF ty_sol_val_5220.

TYPES: BEGIN OF ty_email,
         local           TYPE zsdt0132-rot_desc,
         city1           TYPE zsdt0132-city1,
         uf              TYPE zsdt0132-uf,
         vkbur           TYPE zsdt0131-vkbur,
         nro_cg          TYPE zsdt0129-nro_cg,
         dt_entrega(10)  TYPE c,
         preco_frete(15) TYPE p DECIMALS 2,
         seq_desc_cg     TYPE zsdt0130-seq_ent_cg,
         vbeln           TYPE zsdt0131-vbeln,
         name1           TYPE kna1-name1,
         maktx           TYPE makt-maktx,
         qtd_vinc(15)    TYPE p DECIMALS 2,
         um              TYPE zsdt0131-um,
         qtd_emkg(15)    TYPE p DECIMALS 2,
         seq_ent_cg      TYPE zsdt0130-seq_ent_cg,
         mcod3           TYPE kna1-mcod3,
         fazenda         TYPE zsdt0132-rot_desc,
         tel_number      TYPE zsdt0132-tel_number,
         nr_rot          TYPE zsdt0130-nr_rot,
         roteiro         TYPE string,
       END OF ty_email,

       BEGIN OF ty_saida,
         local            TYPE zsdt0132-rot_desc,
         city1            TYPE zsdt0132-city1,
         uf               TYPE zsdt0132-uf,
         vkbur            TYPE zsdt0131-vkbur,
         nro_cg           TYPE zsdt0129-nro_cg,
         dt_entrega(10)   TYPE c,
         preco_frete(20)  TYPE c,
         seq_desc_cg      TYPE zsdt0130-seq_ent_cg,
         vbeln            TYPE zsdt0131-vbeln,
         name1            TYPE kna1-name1,
         maktx            TYPE makt-maktx,
         qtd_vinc(20)     TYPE c,
         um               TYPE zsdt0131-um,
         qtd_emkg(20)     TYPE c,
         seq_ent_cg       TYPE zsdt0130-seq_ent_cg,
         mcod3            TYPE kna1-mcod3,
         fazenda          TYPE zsdt0132-rot_desc,
         tel_number       TYPE zsdt0132-tel_number,
         nr_rot           TYPE zsdt0130-nr_rot,
         roteiro          TYPE string,
         preco_frete2(20) TYPE c,
       END OF ty_saida,

       BEGIN OF ty_lote,
         nro_lote   TYPE zsdt0129-nro_lote,
         nro_cg     TYPE zsdt0129-nro_cg,
         dt_entrega TYPE zsdt0129-dt_entrega,
       END OF ty_lote,

       BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,
         posnr TYPE vbap-posnr,
         knumv TYPE vbak-knumv,
       END OF ty_vbak,

       BEGIN OF ty_seq,
         nr_cg TYPE zsdt0129-nro_cg,
         ini   TYPE n LENGTH 10,
         fim   TYPE n LENGTH 10,
       END OF ty_seq.

DATA it_zsdt0129 TYPE TABLE OF ty_lote WITH HEADER LINE.
DATA: it_email TYPE TABLE OF ty_email WITH HEADER LINE,
      it_saida TYPE TABLE OF ty_saida,
      wa_saida TYPE ty_saida,
      it_vbak  TYPE TABLE OF ty_vbak WITH HEADER LINE,
      tg_texto TYPE TABLE OF tline WITH HEADER LINE,
      wl_name  TYPE thead-tdname,
      tabix_   TYPE sy-tabix,
      concat   TYPE c LENGTH 99999.

DATA r_seq TYPE TABLE OF ty_seq WITH HEADER LINE.

*
DATA: it_carga_5220          TYPE STANDARD TABLE OF ty_carga_5220,
      it_carga_scot_5220     TYPE STANDARD TABLE OF ty_carga_5220,
      it_carga_ccot_5220     TYPE STANDARD TABLE OF ty_carga_5220,
      it_carga_iti_5220      TYPE STANDARD TABLE OF ty_iti_carga_5220,
      it_carga_ccot_5220_aux TYPE STANDARD TABLE OF ty_carga_5220,
      carga                  TYPE STANDARD TABLE OF ty_carga_5220,
      it_carga_canc_5220     TYPE STANDARD TABLE OF ty_carga_5220.

DATA: g_custom_container_5220        TYPE REF TO cl_gui_custom_container,
      g_custom_container_5220_2      TYPE REF TO cl_gui_custom_container,
      g_custom_container_5220_3      TYPE REF TO cl_gui_custom_container, "FF #154848
      dg_splitter_1_5220             TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2_5220             TYPE REF TO cl_gui_splitter_container,
      dg_splitter_3_5220             TYPE REF TO cl_gui_splitter_container, "FF #154848
      dg_parent_1_5220               TYPE REF TO cl_gui_container,
      dg_parent_2_5220               TYPE REF TO cl_gui_container,
      dg_parent_3_5220               TYPE REF TO cl_gui_container,
      dg_parent_4_5220               TYPE REF TO cl_gui_container,
      ctl_alv1_5220                  TYPE REF TO cl_gui_alv_grid,
      ctl_alv2_5220                  TYPE REF TO cl_gui_alv_grid,
      ctl_alv3_5220                  TYPE REF TO cl_gui_alv_grid, "FF #154848
      gs_layout_5220_alv1            TYPE lvc_s_layo,
      gs_layout_5220_alv2            TYPE lvc_s_layo,
      gs_layout_5220_alv3            TYPE lvc_s_layo,       "FF #154848
      it_fieldcatalog_scot_5220      TYPE lvc_t_fcat,
      it_fieldcatalog_ccot_5220      TYPE lvc_t_fcat,
      it_fieldcatalog_iti_carga_5220 TYPE lvc_t_fcat,
      it_sort_5220                   TYPE lvc_t_sort,
      it_exclude_5220                TYPE ui_functions.

DATA: vg_hotspot_carga_5220 TYPE zsdt0133-nro_cg.

DATA: qntd(2)  TYPE c,
      email(2) TYPE c.

DATA: lt_mailsubject     TYPE sodocchgi1,
      lt_mailrecipientes TYPE STANDARD TABLE OF somlrec90 WITH HEADER LINE,
      lt_mailtxt         TYPE STANDARD TABLE OF soli WITH HEADER LINE.



DATA: it_selected_rows  TYPE lvc_t_row,
      wa_selected_rows  TYPE lvc_s_row,
      wa_carga_5220     TYPE ty_carga_5220,
      wa_carga_5220_aux TYPE ty_carga_5220,
      wa_zsdt0346       TYPE zsdt0346,
      wa_zsdt0133       TYPE zsdt0133.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5220 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      toolbar_5220 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      toolbar_editar_5220 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      "FF #154848 - inicio
      toolbar_iti_carga_5220 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,
      "FF #154848 - fim

      user_command_5220 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      data_changed_finished_5220 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      handle_hotspot_click_ccot_5220  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      handle_hotspot_click_scot_5220  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      on_data_changed_5222 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,

      on_data_changed_finished_5222 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      "FF #154848 - inicio
      on_double_click_5220 FOR EVENT double_click OF cl_gui_alv_grid "Itinerário da carga
        IMPORTING e_row e_column,

      on_data_changed_5223 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,

      on_data_changed_finished_5223 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
    "FF #154848 - fim


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5220 IMPLEMENTATION.

  METHOD toolbar_iti_carga_5220. "ALV3 - itinerario da carga

*    DATA wa_tool TYPE stb_button.
*
*    MOVE 3 TO wa_tool-butn_type.
*    APPEND wa_tool TO e_object->mt_toolbar.
*    CLEAR wa_tool.
*
*    wa_tool-function = 'EDITAR'.
*    wa_tool-icon     = '@0Z@'.
*    wa_tool-quickinfo = 'Editar Carga'.
*    APPEND wa_tool TO e_object->mt_toolbar.
*    CLEAR wa_tool.

  ENDMETHOD.

  METHOD toolbar_5220.

    e_object->mt_toolbar
    = VALUE #(
                ( butn_type = 3 )
                ( function = 'COTACAO1'      icon = '@FD@' quickinfo = 'Cotação' )
                ( butn_type = 3 )
                ( function = 'COTACAO_EMAIL' icon = '@H0@' quickinfo = 'Cotação via e-mail' )
    "FF #154847 - inicio
                ( butn_type = 3 )
                ( function = 'CARGACRIADA_S_COT'   icon = '@9K@' quickinfo = 'Retornar Status "Carga Criada"' )
    "FF #154847 - fim

              ).

  ENDMETHOD.             "DISPLAY

  METHOD toolbar_editar_5220.

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
    wa_tool-quickinfo = 'Cancelar Edição'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'COTACAO2'.
    wa_tool-icon     = '@FD@'.
    wa_tool-quickinfo = 'Cotação'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'FRCONTRATO'.
    wa_tool-icon     = '@4A@'.
    wa_tool-quickinfo = 'Frete Contratado'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.


    "FF #154847 - inicio

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function  = 'RETURNCARGA'.
    wa_tool-icon      = '@2W@'.
    wa_tool-quickinfo = 'Retornar Status "Sem Cotação"'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function  = 'CARGACRIADA_C_COT'.
    wa_tool-icon      = '@9K@'.
    wa_tool-quickinfo = 'Retornar Status "Carga Criada"'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.
    "FF #154847 - fim

  ENDMETHOD.             "DISPLAY

  METHOD user_command_5220.

    DATA: vl_check  TYPE char1,
          vl_check2 TYPE char1,
          vl_block  TYPE sy-tabix,
          vl_cont   TYPE i,
          vl_lines  TYPE i,
          vl_answer.
*
    DATA: r_cg     TYPE RANGE OF zsdt0129-nro_cg,
          w_cg     LIKE LINE OF r_cg,
          msg(200) TYPE c.

    CLEAR: it_selected_rows, wa_selected_rows, email.


    IF e_ucomm = 'EDITAR'.

      CALL METHOD ctl_alv2_5220->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DATA(lv_lines) = lines( it_selected_rows ).

      IF lv_lines > 1.

        MESSAGE 'Selecione apenas 1 linha.' TYPE 'S' DISPLAY LIKE 'E'.

      ELSE.

        LOOP AT it_selected_rows INTO wa_selected_rows.
          READ TABLE it_carga_ccot_5220 INTO wa_carga_5220 INDEX wa_selected_rows-index.
          IF sy-subrc IS INITIAL.
            IF wa_carga_5220-status LT 4.
              MOVE abap_true TO wa_carga_5220-edit.
              MODIFY it_carga_ccot_5220 FROM wa_carga_5220 INDEX wa_selected_rows-index..
              CLEAR: wa_carga_5220.

              "FF #154848 - inicio

              PERFORM alv_carga_double_click_5220 USING wa_selected_rows-index. "A função "EDITAR" também deve carregar o ALV da direita.

            ELSEIF wa_carga_5220-status > 4.

              MESSAGE 'Para editar a carga, solicitar a equipe de insumos que cancele a liberação de embarque.' TYPE 'S' DISPLAY LIKE 'E'.

              "FF #154848 - fim

            ENDIF.
          ENDIF.
        ENDLOOP.

        PERFORM bloqueia_linhas_5220.

        CALL METHOD ctl_alv2_5220->refresh_table_display
          EXPORTING
            is_stable = _stable.
      ENDIF.

    ELSEIF e_ucomm = 'REFRESH'.

      it_carga_ccot_5220 = it_carga_5220.
      DELETE it_carga_ccot_5220 WHERE status LE 3.
      PERFORM bloqueia_linhas_5220.
      CALL METHOD ctl_alv2_5220->refresh_table_display
        EXPORTING
          is_stable = _stable.

    ELSEIF e_ucomm = 'COTACAO1'." OR E_UCOMM = 'COTACAO2'.

      CLEAR: vl_check, vl_check, vl_check2, wa_zsdt0133, vl_block.

      CALL METHOD ctl_alv1_5220->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
        READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
        IF sy-subrc IS INITIAL.

          CLEAR: wa_zsdt0133.

          SELECT SINGLE *
                        FROM zsdt0133
                        INTO wa_zsdt0133
                        WHERE nro_cg EQ wa_carga_5220-nro_cg.

          IF wa_zsdt0133-status NE wa_carga_5220-status.
            MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
            EXIT.
          ELSEIF wa_zsdt0133-status NE 2.
            MESSAGE TEXT-102 TYPE 'S' DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
            EXIT.
          ENDIF.

        ENDIF.
      ENDLOOP.

      IF vl_check IS INITIAL.
        LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
          vl_check2 = abap_true.

          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.

            vl_block = sy-tabix.

            CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave          = wa_carga_5220-nro_cg
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

      IF vl_check2 IS NOT INITIAL.

        LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.

            vl_cont = sy-tabix.

            CLEAR: wa_zsdt0133.

            SELECT SINGLE *
              FROM zsdt0133
              INTO wa_zsdt0133
              WHERE nro_cg EQ wa_carga_5220-nro_cg.

            wa_zsdt0133-status = 3.
            MODIFY zsdt0133 FROM wa_zsdt0133.

            wa_carga_5220-status = 3.
            wa_carga_5220-icone  = '@FD@'.

            MODIFY it_carga_scot_5220 FROM wa_carga_5220 INDEX vl_cont.

          ENDIF.
        ENDLOOP.

        FREE r_cg[].

        LOOP AT it_selected_rows INTO wa_selected_rows.

          w_cg-sign   = 'I'.
          w_cg-option = 'EQ'.

          TRY .
              w_cg-low    = it_carga_scot_5220[ wa_selected_rows-index ]-nro_cg.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
          APPEND w_cg TO r_cg.
          CLEAR w_cg.
        ENDLOOP.

        IF NOT r_cg[] IS INITIAL.
          PERFORM gerar_excel_cotacao TABLES r_cg.
        ENDIF.


        LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.

            CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave = wa_carga_5220-nro_cg.

          ENDIF.
        ENDLOOP.

      ELSE.

        LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.
            IF wa_selected_rows-index LT vl_block.

              CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave = wa_carga_5220-nro_cg.

            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.

      LEAVE TO SCREEN 5000.

      CALL METHOD ctl_alv1_5220->refresh_table_display
        EXPORTING
          is_stable = _stable.
      CALL METHOD ctl_alv2_5220->refresh_table_display
        EXPORTING
          is_stable = _stable.

    ELSEIF e_ucomm = 'COTACAO2'.

      CLEAR: vl_check, vl_check, vl_check2, wa_zsdt0133, vl_block.

      CALL METHOD ctl_alv2_5220->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_carga_ccot_5220 INTO wa_carga_5220.
        READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
        IF sy-subrc IS INITIAL.

          CLEAR: wa_zsdt0133.

          SELECT SINGLE *
                        FROM zsdt0133
                        INTO wa_zsdt0133
                        WHERE nro_cg EQ wa_carga_5220-nro_cg.

          IF wa_zsdt0133-status NE wa_carga_5220-status.
            MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
            EXIT.
          ELSEIF wa_zsdt0133-status LT 3.
            MESSAGE TEXT-102 TYPE 'S' DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
            EXIT.
          ENDIF.

        ENDIF.
      ENDLOOP.

      IF vl_check IS INITIAL.
        LOOP AT it_carga_ccot_5220 INTO wa_carga_5220.
          vl_check2 = abap_true.

          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.

            vl_block = sy-tabix.

            CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave          = wa_carga_5220-nro_cg
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

      IF vl_check2 IS NOT INITIAL.

        FREE r_cg[].

        LOOP AT it_selected_rows INTO wa_selected_rows.

          w_cg-sign   = 'I'.
          w_cg-option = 'EQ'.

          TRY .
              w_cg-low    = it_carga_ccot_5220[ wa_selected_rows-index ]-nro_cg.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
          APPEND w_cg TO r_cg.
          CLEAR w_cg.
        ENDLOOP.

        IF NOT r_cg[] IS INITIAL.
          PERFORM gerar_excel_cotacao TABLES r_cg.
        ENDIF.


        LOOP AT it_carga_ccot_5220 INTO wa_carga_5220.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.

            CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave = wa_carga_5220-nro_cg.

          ENDIF.
        ENDLOOP.

      ELSE.

        LOOP AT it_carga_ccot_5220 INTO wa_carga_5220.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.
            IF wa_selected_rows-index LT vl_block.

              CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave = wa_carga_5220-nro_cg.

            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.

      LEAVE TO SCREEN 5000.

      ctl_alv1_5220->refresh_table_display( EXPORTING is_stable = _stable ).
      ctl_alv2_5220->refresh_table_display( EXPORTING is_stable = _stable ).

    ELSEIF e_ucomm = 'FRCONTRATO'.

      DATA:  cod_trans TYPE zsdt0133-cod_transportadora.

      CLEAR: vl_check, vl_check2, wa_zsdt0133, qntd.

      email = 1.

      CALL METHOD ctl_alv2_5220->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_carga_ccot_5220 INTO wa_carga_5220.
        READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
        IF sy-subrc IS INITIAL.

          CLEAR: wa_zsdt0133.

          SELECT SINGLE *
            FROM zsdt0133
            INTO wa_zsdt0133
            WHERE nro_cg EQ wa_carga_5220-nro_cg.

          IF wa_zsdt0133-status NE wa_carga_5220-status.
            MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
            EXIT.
          ELSEIF wa_zsdt0133-status NE 3 AND  wa_zsdt0133-status NE 4..
            MESSAGE TEXT-102 TYPE 'S' DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
            EXIT.
          ELSEIF wa_carga_5220-preco_frete IS INITIAL OR wa_carga_5220-cod_transportadora IS INITIAL.
            MESSAGE TEXT-047 TYPE 'S' DISPLAY LIKE 'E'.
            MOVE abap_true TO vl_check.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.

      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE  it_carga_ccot_5220 INTO wa_carga_5220 INDEX wa_selected_rows-index.
        IF sy-subrc = 0.
          IF cod_trans IS INITIAL.
            cod_trans = wa_carga_5220-cod_transportadora.
          ENDIF.
        ENDIF.
      ENDLOOP.


      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE  it_carga_ccot_5220 INTO wa_carga_5220 INDEX wa_selected_rows-index.
        IF sy-subrc = 0.
          IF wa_carga_5220-cod_transportadora = cod_trans.
            qntd = 0.
          ELSE.
            qntd = qntd + 1.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF qntd >= 1.
        MESSAGE TEXT-130 TYPE 'S' DISPLAY LIKE 'E'.
*        MESSAGE 'Transportadora deve ser a mesma entre as linhas Selecionadas' TYPE 'E'.
        MOVE abap_true TO vl_check.
        EXIT.
      ENDIF.

      SELECT b~lifnr b~adrnr c~smtp_addr
          FROM lfa1 AS b
          INNER JOIN adr6 AS c ON c~addrnumber EQ b~adrnr
          INTO TABLE it_enviar
      WHERE b~lifnr EQ cod_trans.

      IF it_enviar IS INITIAL.
        msg = |{ 'Nenhum e-mail cadastrado para a Transportadora:  ' } { cod_trans }|.
        MESSAGE msg TYPE 'I'.
        MOVE abap_true TO vl_check.
        EXIT.
      ENDIF.

      IF vl_check IS INITIAL.
        vl_check2 = abap_true.

        LOOP AT it_carga_ccot_5220 INTO wa_carga_5220.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.

            vl_block = sy-tabix.

            CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave          = wa_carga_5220-nro_cg
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

      IF vl_check2 IS NOT INITIAL.

        FREE r_cg[].

        LOOP AT it_selected_rows INTO wa_selected_rows.

          w_cg-sign   = 'I'.
          w_cg-option = 'EQ'.

          TRY .
              w_cg-low    = it_carga_ccot_5220[ wa_selected_rows-index ]-nro_cg.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
          APPEND w_cg TO r_cg.
          CLEAR w_cg.
        ENDLOOP.

        IF NOT r_cg[] IS INITIAL.
          PERFORM cotacao_email TABLES r_cg.
        ENDIF.


**        IF IT_ENVIAR IS INITIAL.
*          MSG = |{ 'Nenhum e-mail cadastrado para a Transportadora:  ' } { COD_TRANS }|.
*          MESSAGE MSG TYPE 'I'.
*          EXIT.
**        ELSE.
        IF it_enviar IS NOT INITIAL.
          LOOP AT it_enviar INTO wa_enviar.
            PERFORM monta_email.
            CLEAR wa_enviar.
          ENDLOOP.


          LOOP AT it_carga_ccot_5220 INTO wa_carga_5220.
            READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
            IF sy-subrc IS INITIAL.

              vl_cont = sy-tabix.

              CLEAR: wa_zsdt0133.

              SELECT SINGLE *
                FROM zsdt0133
                INTO CORRESPONDING FIELDS OF wa_zsdt0133
                WHERE nro_cg EQ wa_carga_5220-nro_cg.

              wa_zsdt0133-status = 4.
              MODIFY zsdt0133 FROM wa_zsdt0133.

              CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave = wa_carga_5220-nro_cg.

              wa_carga_5220-status = 4.
              wa_carga_5220-icone  = '@4A@'.

              MODIFY it_carga_scot_5220 FROM wa_carga_5220 INDEX vl_cont.

            ENDIF.
          ENDLOOP.
        ENDIF.





      ELSE.

        LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.
            IF wa_selected_rows-index LT vl_block.

              CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave = wa_carga_5220-nro_cg.

            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.

      LEAVE TO SCREEN 5000.

    ENDIF.

    CASE e_ucomm.

        "FF #154847 - inicio
      WHEN 'CARGACRIADA_S_COT'.
        PERFORM f_volta_status_carga_criada TABLES it_carga_scot_5220 "Sem cotação
                                            USING 'ALV1'.
      WHEN 'CARGACRIADA_C_COT'.
        PERFORM f_volta_status_carga_criada TABLES it_carga_ccot_5220 "Com cotação
                                            USING 'ALV2'.
      WHEN 'RETURNCARGA'.
        CLEAR: vl_lines,
               vl_answer.

        CLEAR: it_carga_canc_5220, vl_check, vl_lines, wa_zsdt0133.

        CALL METHOD ctl_alv2_5220->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        DESCRIBE TABLE it_selected_rows LINES vl_lines.

        IF vl_lines NE 1.
          MESSAGE TEXT-054 TYPE 'S' DISPLAY LIKE 'E'.
          MOVE abap_true TO vl_check.
        ENDIF.

        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
        READ TABLE  it_carga_ccot_5220 INTO wa_carga_5220 INDEX wa_selected_rows-index.

        IF sy-subrc IS INITIAL.
          "Verifica se a Carga está com Lote de Produtos vinculados
          APPEND wa_carga_5220 TO it_carga_canc_5220.
          PERFORM busca_lotes_produtos_5230 CHANGING vl_check.

          IF vl_check IS INITIAL.

*       "// Carga sem Cotação   2
*       "// Carga em Cotação    3
*       "// Frete Contratado    4
*       "// Embarque Autorizado 5

            CASE wa_carga_5220-status.
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
                    answer                = vl_answer
                  EXCEPTIONS
                    text_not_found        = 1
                    OTHERS                = 2.

                CHECK vl_answer EQ '1'.

              WHEN OTHERS.
                MESSAGE TEXT-131 TYPE 'S' DISPLAY LIKE 'E'.
                EXIT.
            ENDCASE.

            CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave          = wa_carga_5220-nro_cg
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
               WHERE nro_cg EQ wa_carga_5220-nro_cg.

              IF wa_zsdt0133-status NE wa_carga_5220-status.
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
                      chave = wa_carga_5220-nro_cg.
                  LEAVE TO SCREEN 5000.
                ENDIF.

              ENDIF.

              CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave = wa_carga_5220-nro_cg.

            ENDIF.
          ELSE.
            MESSAGE TEXT-154 TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.

        CALL METHOD ctl_alv1_5220->refresh_table_display
          EXPORTING
            is_stable = _stable.

        CALL METHOD ctl_alv2_5220->refresh_table_display
          EXPORTING
            is_stable = _stable.

        "FF #154847 - fim

      WHEN 'COTACAO_EMAIL'.
        CLEAR: vl_check, vl_check, vl_check2, wa_zsdt0133, vl_block.

        email = 2.

        CALL METHOD ctl_alv1_5220->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.


        LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.

            CLEAR: wa_zsdt0133.

            SELECT SINGLE *
                          FROM zsdt0133
                          INTO wa_zsdt0133
                          WHERE nro_cg EQ wa_carga_5220-nro_cg.

            IF wa_zsdt0133-status NE wa_carga_5220-status.
              MESSAGE TEXT-096 TYPE 'S' DISPLAY LIKE 'E'.
              MOVE abap_true TO vl_check.
              EXIT.
            ELSEIF wa_zsdt0133-status NE 2.
              MESSAGE TEXT-102 TYPE 'S' DISPLAY LIKE 'E'.
              MOVE abap_true TO vl_check.
              EXIT.
            ENDIF.

          ENDIF.
        ENDLOOP.

        IF vl_check IS INITIAL.
          LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
            vl_check2 = abap_true.

            READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
            IF sy-subrc IS INITIAL.

              vl_block = sy-tabix.

              CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave          = wa_carga_5220-nro_cg
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

        IF vl_check2 IS NOT INITIAL.

          FREE r_cg[].

          LOOP AT it_selected_rows INTO wa_selected_rows.

            w_cg-sign   = 'I'.
            w_cg-option = 'EQ'.

            TRY .
                w_cg-low    = it_carga_scot_5220[ wa_selected_rows-index ]-nro_cg.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.
            APPEND w_cg TO r_cg.
            CLEAR w_cg.
          ENDLOOP.

          IF NOT r_cg[] IS INITIAL.
            PERFORM cotacao_email TABLES r_cg.
          ENDIF.
        ENDIF.

        SELECT a~lifnr a~lifnr b~name1 b~adrnr
          FROM zsdt0163 AS a
          INNER JOIN lfa1 AS b ON b~lifnr EQ a~lifnr
          INTO TABLE it_0163
        WHERE bukrs EQ l_vkorg-low
          AND a~estorno EQ abap_false.

        LOOP AT it_0163 ASSIGNING FIELD-SYMBOL(<wa>).
          <wa>-check = abap_true.
        ENDLOOP.

        CALL SCREEN 5222 STARTING AT 030 6
                         ENDING   AT 90 21.

    ENDCASE.

  ENDMETHOD.                    "USER_COMMAND

  METHOD data_changed_finished_5220.

    DATA: wa_good_cells TYPE lvc_s_modi,
          wa_carga_5220 TYPE ty_carga_5220,
          wa_lfa1       TYPE lfa1.

    LOOP AT et_good_cells INTO wa_good_cells.
      IF e_modified EQ abap_true.
        IF wa_good_cells-fieldname EQ 'COD_TRANSPORTADORA'.

          READ TABLE it_carga_ccot_5220 INTO wa_carga_5220 INDEX wa_good_cells-row_id.

          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
            WHERE lifnr EQ wa_carga_5220-cod_transportadora.

          IF sy-subrc IS INITIAL.
            wa_carga_5220-desc_transp = wa_lfa1-name1.
            MODIFY it_carga_ccot_5220 FROM wa_carga_5220 INDEX wa_good_cells-row_id.
          ENDIF.

          CALL METHOD ctl_alv2_5220->refresh_table_display
            EXPORTING
              is_stable = _stable.

        ENDIF.

        "FF #154848 - inicio
        IF wa_good_cells-fieldname EQ 'VALOR'.


          DATA: lv_total_frete TYPE p DECIMALS 2.

          lv_total_frete = REDUCE #( INIT soma = 0
                               FOR wa IN it_carga_iti_5220
                               NEXT soma = soma + wa-valor ).


          READ TABLE it_carga_iti_5220 INTO DATA(wa_alv3) INDEX 1.
          IF sy-subrc = 0.

            READ TABLE it_carga_ccot_5220 ASSIGNING FIELD-SYMBOL(<fs_alv2>) WITH KEY nro_cg = wa_alv3-nro_cg.
            IF <fs_alv2> IS ASSIGNED.
              <fs_alv2>-preco_frete = <fs_alv2>-preco_frete + lv_total_frete.

              CALL METHOD ctl_alv3_5220->refresh_table_display
                EXPORTING
                  is_stable = _stable.

              CALL METHOD ctl_alv2_5220->refresh_table_display
                EXPORTING
                  is_stable = _stable.
            ENDIF.
          ENDIF.
          "FF #154848 - fim

        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD handle_hotspot_click_ccot_5220.

    DATA: wa_carga_5220 TYPE ty_carga_5220.

    READ TABLE it_carga_ccot_5220 INTO wa_carga_5220 INDEX e_row_id-index.
    CLEAR vg_hotspot_carga_5220.
    vg_hotspot_carga_5220 = wa_carga_5220-nro_cg.

    CALL SCREEN 5221 STARTING AT 5 5 ENDING AT 75 20.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD handle_hotspot_click_scot_5220.

    DATA: wa_carga_5220 TYPE ty_carga_5220.

    READ TABLE it_carga_scot_5220 INTO wa_carga_5220 INDEX e_row_id-index.
    CLEAR vg_hotspot_carga_5220.
    vg_hotspot_carga_5220 = wa_carga_5220-nro_cg.

    CALL SCREEN 5221 STARTING AT 5 5 ENDING AT 75 20.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD on_data_changed_5222.
  ENDMETHOD.

  METHOD on_data_changed_finished_5222.
  ENDMETHOD.

  "FF 154848 - inicio
  METHOD on_data_changed_5223.
  ENDMETHOD.

  METHOD on_data_changed_finished_5223.
  ENDMETHOD.

  METHOD on_double_click_5220.

    PERFORM alv_carga_double_click_5220 USING e_row-index. "ALV3

  ENDMETHOD.

  "FF 154848 - fim


ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*
*&---------------------------------------------------------------------*
*&      Module  STATUS_5220  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5220 OUTPUT.

  PERFORM seleciona_carga_5220.
  PERFORM completa_carga_5220.
  PERFORM bloqueia_linhas_5220.
  PERFORM alv_tela_carga_5220.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5120  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5220 INPUT.

  "DATA: WA_CARGA_5220 TYPE TY_CARGA_5220,
  "      WA_ZSDT0133   TYPE ZSDT0133.

  ctl_alv2_5220->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'SAVE'.

      "FF #154848 - inicio
      LOOP AT it_carga_iti_5220 ASSIGNING FIELD-SYMBOL(<fs_iti>).

        wa_zsdt0346-nro_carga        = <fs_iti>-nro_cg.
        wa_zsdt0346-tipo_transporte  = <fs_iti>-tipo_transporte.
        wa_zsdt0346-ponto_coleta     = <fs_iti>-ponto_coleta.
        wa_zsdt0346-local_entrega    = <fs_iti>-local_entrega.
        wa_zsdt0346-itinerario       = <fs_iti>-itinerario.
        wa_zsdt0346-unidade_condicao = <fs_iti>-unidade_condicao.
        wa_zsdt0346-valor            = <fs_iti>-valor.
        wa_zsdt0346-unidade_medida   = <fs_iti>-unidade_medida.

        MODIFY zsdt0346 FROM wa_zsdt0346.

      ENDLOOP..
      "FF #154848 - fim

      LOOP AT it_carga_ccot_5220 INTO wa_carga_5220 WHERE edit EQ abap_true.
        wa_zsdt0133-nro_cg             = wa_carga_5220-nro_cg.
        wa_zsdt0133-cod_transportadora = wa_carga_5220-cod_transportadora.

        "FF #154848 - inicio
        READ TABLE it_carga_iti_5220 WITH KEY nro_cg = wa_carga_5220-nro_cg INTO DATA(wa_carga_iti).
        IF sy-subrc = 0.
          wa_carga_5220-preco_frete  = wa_carga_iti-valor.
        ENDIF.
        "FF #154848 - fim

        wa_zsdt0133-preco_frete        = wa_carga_5220-preco_frete.
        wa_zsdt0133-ctg_transp         = wa_carga_5220-ctg_transp.
        wa_zsdt0133-qtd_total_kg       = wa_carga_5220-qtd_total_kg.
        wa_zsdt0133-status             = wa_carga_5220-status.
        wa_zsdt0133-usnam              = wa_carga_5220-usnam.
        wa_zsdt0133-data_atual         = wa_carga_5220-data_atual.
        wa_zsdt0133-hora_atual         = wa_carga_5220-hora_atual.
        wa_zsdt0133-user_edit          = sy-uname.
        wa_zsdt0133-data_edit          = sy-datum.
        wa_zsdt0133-hora_edit          = sy-uzeit.
        wa_zsdt0133-user_canc          = wa_carga_5220-user_canc.
        wa_zsdt0133-dt_canc            = wa_carga_5220-dt_canc.
        wa_zsdt0133-hr_can             = wa_carga_5220-hr_can.
        MODIFY zsdt0133 FROM wa_zsdt0133.
      ENDLOOP.

      "FF #154848 - inicio
      CALL METHOD ctl_alv2_5220->refresh_table_display
        EXPORTING
          is_stable = _stable.


      CALL METHOD ctl_alv3_5220->refresh_table_display
        EXPORTING
          is_stable = _stable.
      "FF #154848 - fim


    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 80 20.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_CARGA_5220
*&---------------------------------------------------------------------*
FORM seleciona_carga_5220.

  DATA: it_zsdt0131 TYPE STANDARD TABLE OF zsdt0131,
        it_zsdt0129 TYPE STANDARD TABLE OF zsdt0129.

  SELECT *
    FROM zsdt0131
    INTO TABLE it_zsdt0131
    WHERE vkorg IN l_vkorg
      AND spart EQ l_spart
      AND vbeln IN l_ovcor
      AND status NE 'X'.

  IF it_zsdt0131 IS NOT INITIAL.

    SELECT *
        FROM zsdt0129
        INTO TABLE it_zsdt0129
        FOR ALL ENTRIES IN it_zsdt0131
        WHERE nro_lote EQ it_zsdt0131-nro_lote
          AND inco1  IN l_inco1
          AND inco1  NE 'FOB'
          AND status NE 'X'.
*          AND NOT EXISTS ( SELECT *
*                             FROM ZSDT0135
*                             WHERE WRKST EQ ZSDT0129~MARCA ).

    IF it_zsdt0129 IS NOT INITIAL.

      SELECT *
          FROM zsdt0133
          INTO CORRESPONDING FIELDS OF TABLE it_carga_5220
          FOR ALL ENTRIES IN it_zsdt0129
          WHERE nro_cg     EQ it_zsdt0129-nro_cg
            AND nro_cg     IN l_numcg
            AND data_atual IN l_datab.

    ENDIF.

  ENDIF.
*wsb
ENDFORM.
*
*&---------------------------------------------------------------------*
*&      Form  ALV_TELA_CARGA_5220
*&---------------------------------------------------------------------*
FORM alv_tela_carga_5220.

  IF g_custom_container_5220 IS INITIAL.

    CREATE OBJECT g_custom_container_5220
      EXPORTING
        container_name              = 'CONTAINER5220'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT dg_splitter_1_5220
      EXPORTING
        parent  = g_custom_container_5220
        rows    = 3
        columns = 1.

    CALL METHOD dg_splitter_1_5220->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_5220.

    CALL METHOD dg_splitter_1_5220->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_2_5220.

    CREATE OBJECT dg_splitter_2_5220
      EXPORTING
        parent  = dg_parent_2_5220
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2_5220->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_3_5220.

    CALL METHOD dg_splitter_2_5220->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_4_5220.

    CALL METHOD dg_splitter_1_5220->set_row_mode
      EXPORTING
        mode = dg_splitter_1_5220->mode_relative.

    CALL METHOD dg_splitter_1_5220->set_row_height
      EXPORTING
        id     = 1
        height = 50.

    CALL METHOD dg_splitter_1_5220->set_row_height
      EXPORTING
        id     = 2
        height = 50.

    CALL METHOD dg_splitter_2_5220->set_column_width
      EXPORTING
        id    = 1
        width = 50.

    CALL METHOD dg_splitter_2_5220->set_column_width
      EXPORTING
        id    = 2
        width = 50.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_scot_5220 USING:
          01 'ICONE'                ''             ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status',
          02 'NRO_CG'               'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Carga',
          03 'ROT_DESC'             'ZSDT0133'     ' '  ' '  ' '  'X'   'X'   ' '   ' '   ' '   'Local Embarque',
          04 'QTD_TOTAL_KG'         'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd Total Kg',
          06 'TVAL'                 ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Valor Carga',
          07 'COD_TRANSPORTADORA'   'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Transp.',
          08 'DESC_TRANSP'          'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transportadora',
          09 'PRECO_FRETE'          'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Valor Frete',
          10 'PLACA_CAV'            'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Cavalo',
          11 'PLACA_CAR1'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta I',
          12 'PLACA_CAR2'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta II',
          13 'PLACA_CAR3'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Dolly',
          14 'MOTORISTA'            'ZSDT0129'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Motorista',
          15 'DESC_MOT'             'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Motorista',
          16 'DT_ENTREGA'           'ZSDT0129'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Prv. Dt. Entrega',
          17 'INCO1'                'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp. Frete',
          18 'DATA_ATUAL'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Data Carga'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_ccot_5220 USING:
          01 'ICONE'                ''             ' '  ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status',
          02 'NRO_CG'               'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Carga',
          03 'ROT_DESC'             'ZSDT0133'     ' '  ' '  ' '  'X'   'X'   ' '   ' '   ' '   'Local Embarque',
          04 'QTD_TOTAL_KG'         'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd Total Kg',
          06 'TVAL'                 ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Valor Carga',
          07 'COD_TRANSPORTADORA'   'ZSDT0133'     ' '  'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Transp.',
          08 'DESC_TRANSP'          'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transportadora',
          09 'PRECO_FRETE'          'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Valor Frete',
          10 'PLACA_CAV'            'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Cavalo',
          11 'PLACA_CAR1'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta I',
          12 'PLACA_CAR2'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Carreta II',
          13 'PLACA_CAR3'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Placa Dolly',
          14 'MOTORISTA'            'ZSDT0129'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Motorista',
          15 'DESC_MOT'             'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Motorista',
          16 'DT_ENTREGA'           'ZSDT0129'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Prv. Dt. Entrega',
          17 'INCO1'                'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp. Frete',
          18 'DATA_ATUAL'           'ZSDT0133'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Data Carga'.

    "FF #154848 - inicio aqui
    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_iti_carga_5220 USING:
          01 'NRO_CG'               'ZSDT0346'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Carga',
          02 'OPERACAO'             'ZSDT0345'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Operação',
          03 'TIPO_TRANSPORTE'      'ZSDT0346'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tipo de transporte',
          04 'PONTO_COLETA'         'ZSDT0346'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ponto de coleta',
          05 'LOCAL_ENTREGA'        'ZSDT0346'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Local de entrega',
          06 'ITINERARIO'           'ZSDT0346'     ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Itinerário',
          07 'DISTANCIA_KM'         ''             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Distância (KM)',
          08 'UNIDADE_CONDICAO'     'ZSDT0346'     ' '  'X'  ' '  ' '   'X'   ' '   ' '   'X'   'Unidade condição',
          09 'VALOR'                'ZSDT0346'     ' '  'X'  ' '  ' '   'X'   ' '   'X'   ' '   'Valor',
          10 'UNIDADE_MEDIDA'       'ZSDT0346'     ' '  'X'  ' '  ' '   'X'   ' '   ' '   'X'   'Unidade medida'.
    "FF #154848 - fim

    gs_layout_5220_alv1-sel_mode   = 'A'.
    gs_layout_5220_alv1-stylefname = 'CELLSTYLES'.
    gs_layout_5220_alv1-cwidth_opt = 'X'.
    gs_layout_5220_alv1-smalltitle = 'X'.
    gs_layout_5220_alv1-grid_title = 'Cargas sem cotação'.

    gs_layout_5220_alv2-sel_mode   = 'A'.
    gs_layout_5220_alv2-stylefname = 'CELLSTYLES'.
    gs_layout_5220_alv2-cwidth_opt = 'X'.
    gs_layout_5220_alv2-smalltitle = 'X'.
    gs_layout_5220_alv2-grid_title = 'Cargas com cotação'.

    "FF #154848 - inicio
    gs_layout_5220_alv3-sel_mode   = 'A'.
    gs_layout_5220_alv3-stylefname = 'CELLSTYLES'.
    gs_layout_5220_alv3-cwidth_opt = 'X'.
    gs_layout_5220_alv3-smalltitle = 'X'.
    gs_layout_5220_alv3-grid_title = 'Itinerários da carga'.
    "FF #154848 - fim

    PERFORM sort USING 'NRO_CG' CHANGING it_sort_5220.
    PERFORM excluir_botoes CHANGING it_exclude_5220.

    CREATE OBJECT ctl_alv1_5220
      EXPORTING
        i_parent = dg_parent_1_5220.           "ALV Lote
*        i_parent = g_custom_container_5220.           "ALV Lote

    CREATE OBJECT ctl_alv2_5220
      EXPORTING
        i_parent = dg_parent_3_5220.           "ALV Oderm

    "FF #154848 - inicio
    CREATE OBJECT ctl_alv3_5220
      EXPORTING
        i_parent = dg_parent_4_5220.           "ALV Itinerários da carga
    "FF #154848 - fim

    SET HANDLER: lcl_event_handler_5220=>toolbar_5220 FOR ctl_alv1_5220,
                 lcl_event_handler_5220=>user_command_5220 FOR ctl_alv1_5220,
                 lcl_event_handler_5220=>handle_hotspot_click_scot_5220 FOR ctl_alv1_5220.

    CALL METHOD ctl_alv1_5220->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5220_alv1
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_scot_5220
        it_outtab       = it_carga_scot_5220
        it_sort         = it_sort_5220.

    SET HANDLER:
          lcl_event_handler_5220=>on_double_click_5220  FOR ctl_alv2_5220, "FF 154848
          lcl_event_handler_5220=>toolbar_editar_5220 FOR ctl_alv2_5220,
          lcl_event_handler_5220=>user_command_5220 FOR ctl_alv2_5220,
          lcl_event_handler_5220=>handle_hotspot_click_ccot_5220 FOR ctl_alv2_5220,
          lcl_event_handler_5220=>data_changed_finished_5220 FOR ctl_alv2_5220.


    IF sy-sysid = 'DEV'."Apenas para testes no DEV
      "ALV2
      APPEND INITIAL LINE TO it_carga_ccot_5220 ASSIGNING FIELD-SYMBOL(<fs_alv2>).
      <fs_alv2>-nro_cg = '123456'.


      "ALV3
      APPEND INITIAL LINE TO it_carga_iti_5220 ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-nro_cg = '123456'.
      <fs>-ponto_coleta = '111111'.
      <fs>-valor = 200.

      APPEND INITIAL LINE TO it_carga_iti_5220 ASSIGNING <fs>.
      <fs>-nro_cg = '123456'.
      <fs>-ponto_coleta = '222222'.
      <fs>-valor = 500.

    ENDIF.




    CALL METHOD ctl_alv2_5220->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5220_alv2
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5220
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_ccot_5220
        it_outtab            = it_carga_ccot_5220
        it_sort              = it_sort_5220.


    CALL METHOD ctl_alv2_5220->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.


    "FF #154848 - inicio

    SET HANDLER:
          lcl_event_handler_5220=>toolbar_iti_carga_5220 FOR ctl_alv3_5220,
          lcl_event_handler_5220=>user_command_5220 FOR ctl_alv3_5220,
*          lcl_event_handler_5220=>handle_hotspot_click_iti_5220 FOR ctl_alv3_5220,
          lcl_event_handler_5220=>data_changed_finished_5220 FOR ctl_alv3_5220.

    CALL METHOD ctl_alv3_5220->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5220_alv3
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5220
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_iti_carga_5220
        it_outtab            = it_carga_iti_5220
        it_sort              = it_sort_5220.

    CALL METHOD ctl_alv3_5220->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    "FF #154848 - fim



  ELSE.

    CALL METHOD ctl_alv1_5220->refresh_table_display
      EXPORTING
        is_stable = _stable.

    CALL METHOD ctl_alv2_5220->refresh_table_display
      EXPORTING
        is_stable = _stable.

    "FF #154848 - inicio
    CALL METHOD ctl_alv3_5220->refresh_table_display
      EXPORTING
        is_stable = _stable.
    "FF #154848 - fim

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_CARGA_5220
*&---------------------------------------------------------------------*
FORM completa_carga_5220 .

  DATA: it_zsdt0131     TYPE STANDARD TABLE OF zsdt0131,
        it_zsdt0129     TYPE STANDARD TABLE OF zsdt0129,
        it_lfa1         TYPE STANDARD TABLE OF lfa1,
        it_sol_val_5220 TYPE STANDARD TABLE OF ty_sol_val_5220,
        wa_sol_val_5220 TYPE ty_sol_val_5220,
        it_konv         TYPE STANDARD TABLE OF konv,
        wa_zsdt0131     TYPE zsdt0131,
        wa_zsdt0129     TYPE zsdt0129,
        wa_lfa1         TYPE lfa1,
        wa_konv         TYPE konv,
        wa_carga_5220   TYPE ty_carga_5220,
        vl_cont         TYPE i.

  IF it_carga_5220 IS NOT INITIAL.

    SELECT *
      FROM lfa1
      INTO TABLE it_lfa1
      FOR ALL ENTRIES IN it_carga_5220
      WHERE lifnr EQ it_carga_5220-cod_transportadora.

    SELECT *
      FROM zsdt0129
      INTO TABLE it_zsdt0129
      FOR ALL ENTRIES IN it_carga_5220
      WHERE nro_cg EQ it_carga_5220-nro_cg.

    IF it_zsdt0129 IS NOT INITIAL.

      SELECT *
        FROM zsdt0131
        INTO TABLE it_zsdt0131
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE nro_lote EQ it_zsdt0129-nro_lote
          AND status NE 'X'.

      SELECT zsdt0131~vbeln
             zsdt0131~posnr
             vbak~knumv
        FROM zsdt0131
        INNER JOIN vbak ON zsdt0131~vbeln = vbak~vbeln
        INTO CORRESPONDING FIELDS OF TABLE it_sol_val_5220
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE nro_lote EQ it_zsdt0129-nro_lote
          AND status NE 'X'.

      IF it_sol_val_5220 IS NOT INITIAL.

* ---> S4 Migration - 07/07/2023 - JP
*        SELECT *
*          FROM konv
*          INTO TABLE it_konv
*          FOR ALL ENTRIES IN it_sol_val_5220
*          WHERE knumv EQ it_sol_val_5220-knumv
*            AND kposn EQ it_sol_val_5220-posnr
*            AND kschl EQ 'PR00'.

        SELECT *
          FROM v_konv
          INTO TABLE @DATA(it_konv_aux)
          FOR ALL ENTRIES IN @it_sol_val_5220
         WHERE knumv EQ @it_sol_val_5220-knumv
           AND kposn EQ @it_sol_val_5220-posnr
           AND kschl EQ 'PR00'.

        MOVE-CORRESPONDING it_konv_aux[] TO it_konv[].

* <--- S4 Migration - 07/07/2023 - JP

      ENDIF.

      SELECT *
        FROM lfa1
        APPENDING TABLE it_lfa1
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE lifnr EQ it_zsdt0129-motorista.

      SELECT *
        FROM lfa1
        APPENDING TABLE it_lfa1
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE lifnr EQ it_zsdt0129-motorista.

      SELECT *
        FROM lfa1
        APPENDING TABLE it_lfa1
        FOR ALL ENTRIES IN it_zsdt0129
        WHERE lifnr EQ it_zsdt0129-motorista.

    ENDIF.

  ENDIF.

  LOOP AT it_carga_5220 INTO wa_carga_5220.

    vl_cont = vl_cont + 1.

    READ TABLE it_zsdt0129 INTO wa_zsdt0129 WITH KEY nro_cg = wa_carga_5220-nro_cg.
    IF sy-subrc IS INITIAL.
      wa_carga_5220-placa_cav  = wa_zsdt0129-placa_cav.
      wa_carga_5220-placa_car1 = wa_zsdt0129-placa_car1.
      wa_carga_5220-placa_car2 = wa_zsdt0129-placa_car2.
      wa_carga_5220-placa_car3 = wa_zsdt0129-placa_car3.
      wa_carga_5220-motorista  = wa_zsdt0129-motorista.
      wa_carga_5220-dt_entrega = wa_zsdt0129-dt_entrega.
      wa_carga_5220-inco1      = wa_zsdt0129-inco1.

      IF wa_carga_5220-cod_transportadora IS NOT INITIAL.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5220-cod_transportadora.
        IF sy-subrc IS INITIAL.
          wa_carga_5220-desc_transp = wa_lfa1-name1.
        ENDIF.
      ENDIF.

      IF wa_carga_5220-motorista IS NOT INITIAL.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_carga_5220-motorista.
        IF sy-subrc IS INITIAL.
          wa_carga_5220-desc_mot = wa_lfa1-name1.
        ENDIF.
      ENDIF.

      LOOP AT it_zsdt0131 INTO wa_zsdt0131 WHERE nro_lote EQ wa_zsdt0129-nro_lote.

        READ TABLE it_sol_val_5220 INTO wa_sol_val_5220 WITH KEY vbeln = wa_zsdt0131-vbeln
                                                                 posnr = wa_zsdt0131-posnr.
        IF sy-subrc IS INITIAL.
          READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_sol_val_5220-knumv
                                                   kposn = wa_sol_val_5220-posnr.
          IF sy-subrc IS INITIAL.
            IF wa_konv-kmein EQ 'TO'.
              wa_carga_5220-tval = wa_carga_5220-tval + wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew * wa_konv-kbetr / 1000 * wa_konv-kkurs.
            ELSEIF wa_konv-kmein EQ 'KG'.
              wa_carga_5220-tval = wa_carga_5220-tval + wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew * wa_konv-kbetr / 1 * wa_konv-kkurs.
            ELSE.
              wa_carga_5220-tval = wa_carga_5220-tval + wa_zsdt0131-qtd_vinc * wa_konv-kbetr / 1 * wa_konv-kkurs.
            ENDIF.
          ENDIF.
        ENDIF.
        wa_carga_5220-rot_desc = wa_zsdt0131-local_embarq.
      ENDLOOP.

    ENDIF.

    IF wa_carga_5220-status EQ '2'.
      wa_carga_5220-icone = '@5D@'.
    ELSEIF wa_carga_5220-status EQ '3'.
      wa_carga_5220-icone = '@FD@'.
    ELSEIF wa_carga_5220-status EQ '4'.
      wa_carga_5220-icone = '@4A@'.
    ELSEIF wa_carga_5220-status EQ '5'.
      wa_carga_5220-icone = '@96@'.
    ELSEIF wa_carga_5220-status EQ '6'.
      wa_carga_5220-icone =  '@0Q@'.
    ENDIF.

    wa_carga_5220-edit = abap_false.

    MODIFY it_carga_5220 FROM wa_carga_5220 INDEX vl_cont.

  ENDLOOP.

  it_carga_scot_5220 = it_carga_5220.
  it_carga_ccot_5220 = it_carga_5220.

  DELETE it_carga_scot_5220 WHERE status NE 2.
  DELETE it_carga_ccot_5220 WHERE ( status LT 3 OR status EQ 'X' ).

  PERFORM check_transp USING 'C'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BLOQUEIA_LINHAS_5220
*&---------------------------------------------------------------------*
FORM bloqueia_linhas_5220.

  DATA: vl_cont       TYPE i,
        it_celltab    TYPE lvc_t_styl,
        wa_carga_5220 TYPE ty_carga_5220.

  LOOP AT it_carga_ccot_5220 INTO wa_carga_5220.

    vl_cont = vl_cont + 1.
    CLEAR: it_celltab, wa_carga_5220-cellstyles.
    "REFRESH IT_CELLTAB.
    PERFORM fill_celltab_5220 USING wa_carga_5220-edit
                                    wa_carga_5220-status
                              CHANGING it_celltab.
    "CLEAR WA_ZSDT0132-CELLSTYLES.
    INSERT LINES OF it_celltab INTO TABLE wa_carga_5220-cellstyles.
    MODIFY it_carga_ccot_5220 FROM wa_carga_5220 INDEX vl_cont.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
FORM fill_celltab_5220  USING    p_edit
                                 p_status
                        CHANGING p_it_celltab TYPE lvc_t_styl.

  DATA: wa_celltab TYPE lvc_s_styl,
        status     TYPE raw4.

  IF p_edit EQ abap_true.
    status = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    status = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  "FF #154848 - inicio
*  wa_celltab-fieldname = 'COD_TRANSPORTADORA'.
*  wa_celltab-style = status.
*  INSERT wa_celltab INTO TABLE p_it_celltab.

*  wa_celltab-fieldname = 'PRECO_FRETE'.
*  wa_celltab-style = status.
*  INSERT wa_celltab INTO TABLE p_it_celltab.

  CASE p_status.

    WHEN '3'.

      wa_celltab-fieldname = 'COD_TRANSPORTADORA'.
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

    WHEN '4'.

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

    WHEN 'X'. "Itinerário

      wa_celltab-fieldname = 'UNIDADE_CONDICAO'.
      wa_celltab-style = status.
      INSERT wa_celltab INTO TABLE p_it_celltab.

      wa_celltab-fieldname = 'VALOR'.
      wa_celltab-style = status.
      INSERT wa_celltab INTO TABLE p_it_celltab.

      wa_celltab-fieldname = 'UNIDADE_MEDIDA'.
      wa_celltab-style = status.
      INSERT wa_celltab INTO TABLE p_it_celltab.

  ENDCASE.
  "FF #154848 - fim
ENDFORM.                               " FILL_CELLTAB

*&---------------------------------------------------------------------*
*&      Form  GERAR_EXCEL_COTACAO
*&---------------------------------------------------------------------*
FORM gerar_excel_cotacao TABLES t_cg.

  TYPES: BEGIN OF ty_excel,
           local       TYPE zsdt0132-rot_desc,
           city1       TYPE zsdt0132-city1,
           uf          TYPE zsdt0132-uf,
           vkbur       TYPE zsdt0131-vkbur,
           nro_cg      TYPE zsdt0129-nro_cg,
           dt_entrega  TYPE zsdt0129-dt_entrega,
           preco_frete TYPE zsdt0133-preco_frete,
           seq_desc_cg TYPE zsdt0130-seq_ent_cg,

           vbeln       TYPE zsdt0131-vbeln,
           cpf_cnpj    TYPE kna1-stcd1,
           name1       TYPE kna1-name1,
           maktx       TYPE makt-maktx,
           qtd_vinc    TYPE zsdt0131-qtd_vinc,
           um          TYPE zsdt0131-um,
           qtd_emkg    TYPE zsdt0131-qtd_emkg,
           seq_ent_cg  TYPE zsdt0130-seq_ent_cg,
           mcod3       TYPE kna1-mcod3,
           uf_dest     TYPE kna1-regio,
           fazenda     TYPE zsdt0132-rot_desc,
           tel_number  TYPE zsdt0132-tel_number,
           nr_rot      TYPE zsdt0130-nr_rot,
           roteiro     TYPE c LENGTH 255,
         END OF ty_excel,

         BEGIN OF ty_lote,
           nro_lote   TYPE zsdt0129-nro_lote,
           nro_cg     TYPE zsdt0129-nro_cg,
           dt_entrega TYPE zsdt0129-dt_entrega,
         END OF ty_lote,

         BEGIN OF ty_vbak,
           vbeln TYPE vbak-vbeln,
           posnr TYPE vbap-posnr,
           knumv TYPE vbak-knumv,
         END OF ty_vbak,

         BEGIN OF ty_seq,
           nr_cg TYPE zsdt0129-nro_cg,
           ini   TYPE n LENGTH 10,
           fim   TYPE n LENGTH 10,
         END OF ty_seq.

  DATA it_zsdt0129 TYPE TABLE OF ty_lote WITH HEADER LINE.
  DATA: it_excel TYPE TABLE OF ty_excel WITH HEADER LINE,
        it_vbak  TYPE TABLE OF ty_vbak WITH HEADER LINE,
        tg_texto TYPE TABLE OF tline WITH HEADER LINE,
        wl_name  TYPE thead-tdname,
        tabix_   TYPE sy-tabix,
        concat   TYPE c LENGTH 99999.

  DATA r_seq TYPE TABLE OF ty_seq WITH HEADER LINE.

  SELECT nro_lote nro_cg dt_entrega FROM zsdt0129
   INTO TABLE it_zsdt0129
   WHERE nro_cg IN t_cg.

  CHECK NOT it_zsdt0129[] IS INITIAL.

  SELECT * FROM zsdt0130
    INTO TABLE @DATA(it_zsdt0130)
    FOR ALL ENTRIES IN @it_zsdt0129
    WHERE nro_lote EQ @it_zsdt0129-nro_lote.

  SELECT * FROM zsdt0131
    INTO TABLE @DATA(it_zsdt0131)
    FOR ALL ENTRIES IN @it_zsdt0129
    WHERE nro_lote EQ @it_zsdt0129-nro_lote
    AND status NE @abap_true.

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
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
*    SELECT * FROM konv
*      INTO TABLE @DATA(it_konv)
*      FOR ALL ENTRIES IN @it_vbak
*      WHERE knumv EQ @it_vbak-knumv
*       AND kposn EQ @it_vbak-posnr
*       AND kschl EQ 'PR00'.
    DATA: it_konv TYPE TABLE OF konv.
    TRY.
        cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
          EXPORTING it_selection_attribute = VALUE #( ( fieldname = 'KNUMV'
                                                        value     = it_vbak-knumv )
                                                      ( fieldname = 'KPOSN'
                                                        value     = it_vbak-posnr )
                                                      ( fieldname = 'KSCHL'
                                                        value     = 'PR00' ) )
          IMPORTING et_prc_element_classic_format = it_konv ).
      CATCH cx_prc_result ##NO_HANDLER.
    ENDTRY.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
  ENDIF.

  IF NOT it_zsdt0131 IS INITIAL.
    SELECT * FROM zsdt0132
      INTO TABLE @DATA(it_zsdt0132)
      FOR ALL ENTRIES IN @it_zsdt0131
       WHERE nr_rot EQ @it_zsdt0131-cod_loc_emb.

    SELECT * FROM makt
      INTO TABLE @DATA(it_makt)
      FOR ALL ENTRIES IN @it_zsdt0131
     WHERE matnr EQ @it_zsdt0131-matnr.
  ENDIF.

  IF NOT it_zsdt0130 IS INITIAL.
    SELECT * FROM zsdt0132
      APPENDING TABLE it_zsdt0132
      FOR ALL ENTRIES IN it_zsdt0130
       WHERE nr_rot EQ it_zsdt0130-nr_rot.

    SELECT * FROM kna1
      INTO TABLE @DATA(it_kna1)
      FOR ALL ENTRIES IN @it_zsdt0130
      WHERE kunnr EQ @it_zsdt0130-kunnr.
  ENDIF.


  LOOP AT it_zsdt0129.

    LOOP AT it_zsdt0131 INTO DATA(wa_zsdt0131) WHERE nro_lote EQ it_zsdt0129-nro_lote.

      it_excel-nro_cg     = it_zsdt0129-nro_cg.
      it_excel-dt_entrega = it_zsdt0129-dt_entrega.
      it_excel-vkbur      = wa_zsdt0131-vkbur.
      it_excel-vbeln      = wa_zsdt0131-vbeln.

      it_excel-qtd_vinc   = wa_zsdt0131-qtd_vinc.
      it_excel-um         = wa_zsdt0131-um.
      it_excel-qtd_emkg   = wa_zsdt0131-qtd_emkg.

      TRY .
          DATA(wa_zsdt0132) = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
          it_excel-local      = wa_zsdt0132-rot_desc.
          it_excel-city1      = wa_zsdt0132-city1.
          it_excel-uf         = wa_zsdt0132-uf.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          DATA(wa_zsdt0130) = it_zsdt0130[ nro_lote = it_zsdt0129-nro_lote
                                            nro_sol = wa_zsdt0131-nro_sol ].
          it_excel-seq_ent_cg = wa_zsdt0130-seq_ent_cg.
          it_excel-seq_desc_cg = wa_zsdt0130-seq_ent_cg.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          it_excel-name1 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-name1.
          it_excel-mcod3 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-mcod3.
*INICIO - AS - 02/10/2020 - CS2020001072
          it_excel-uf_dest = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-regio.
          it_excel-cpf_cnpj = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-stcd1.
          IF it_kna1[ kunnr = wa_zsdt0130-kunnr ]-stcd1 IS INITIAL.
            it_excel-cpf_cnpj = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-stcd2.
          ENDIF.
*FIM - AS - 02/10/2020 - CS2020001072
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          it_excel-maktx = it_makt[ matnr = wa_zsdt0131-matnr ]-maktx.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          wa_zsdt0132 = it_zsdt0132[ nr_rot = wa_zsdt0130-nr_rot ].
          it_excel-fazenda    = wa_zsdt0132-rot_desc.
          it_excel-tel_number = wa_zsdt0132-tel_number.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      it_excel-nr_rot = wa_zsdt0130-nr_rot.

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
          it_excel-preco_frete = ( ( ( ( wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew ) * wa_konv-kbetr ) / 1000 ) * wa_konv-kkurs ).
        WHEN OTHERS.
          it_excel-preco_frete = ( ( wa_zsdt0131-qtd_vinc * wa_konv-kbetr ) * wa_konv-kkurs ).
      ENDCASE.

      r_seq-ini  = it_excel-seq_ent_cg.
      r_seq-nr_cg = it_excel-nro_cg.
      APPEND r_seq.
      CLEAR r_seq.

      APPEND it_excel.

    ENDLOOP.
  ENDLOOP.

  SORT r_seq BY nr_cg ini.
  DELETE ADJACENT DUPLICATES FROM r_seq COMPARING nr_cg ini.

  DATA cont TYPE n LENGTH 10.

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

  LOOP AT it_excel ASSIGNING FIELD-SYMBOL(<exc>).

    r_seq = r_seq[ nr_cg = <exc>-nro_cg
                     ini = <exc>-seq_ent_cg ].
    IF r_seq-ini EQ <exc>-seq_ent_cg AND
       r_seq-nr_cg EQ <exc>-nro_cg.
      <exc>-seq_ent_cg = r_seq-fim.
    ENDIF.

  ENDLOOP.

  SORT it_excel BY nro_cg     ASCENDING
                   seq_ent_cg ASCENDING.

  DATA str          TYPE REF TO data.

  ASSIGN 'TY_EXCEL' TO FIELD-SYMBOL(<fs_str>).
  CREATE DATA str TYPE (<fs_str>).

  DATA(r_table) =
  CORRESPONDING lvc_t_fcat(
  cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr(
  cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

  LOOP AT r_table ASSIGNING FIELD-SYMBOL(<table>).

    CASE <table>-fieldname.
      WHEN 'LOCAL'.
        <table>-scrtext_l = 'LOCAL DE CARREGAMENTO'.
      WHEN 'CITY1'.
        <table>-scrtext_l = 'MUNICÍPIO'.
      WHEN 'UF'.
        <table>-scrtext_l = 'UF'.
      WHEN 'VKBUR'.
        <table>-scrtext_l = 'FILIAL'.
      WHEN 'NRO_CG'.
        <table>-scrtext_l = 'CARGA'.
      WHEN 'DT_ENTREGA'.
        <table>-scrtext_l = 'DATA ENTREGA'.
      WHEN 'SEQ_DESC_CG'.
        <table>-scrtext_l = 'SEQUENCIA DE DESCARGA'.
      WHEN 'SEQ_ENT_CG'.
        <table>-scrtext_l = 'SEQUENCIA DE CARREGAMENTO SEMENTEIRA'. "'SEQUENCIA DE DESCARGA'.
      WHEN 'VBELN'.
        <table>-scrtext_l = 'OV'.
      WHEN 'CPF_CNPJ'.
        <table>-scrtext_l = 'CPF_CNPJ CLIENTE'. "*INICIO - AS - 02/10/2020 - CS2020001072
      WHEN 'NAME1'.
        <table>-scrtext_l = 'CLIENTE'.
      WHEN 'MAKTX'.
        <table>-scrtext_l = 'CULTIVAR'.
      WHEN 'QTD_VINC'.
        <table>-scrtext_l = 'QUANTIDADE'.
      WHEN 'UM'.
        <table>-scrtext_l = 'UNIDADE'.
      WHEN 'QTD_EMKG'.
        <table>-scrtext_l = 'QTD/QUILOS'.
      WHEN 'PRECO_FRETE'.
        <table>-scrtext_l = 'VALOR TOTAL DA CARGA'."'PREÇO FRETE'.
      WHEN 'MCOD3'.
        <table>-scrtext_l = 'MUNICÍPIO DESTINO'.
      WHEN 'UF_DEST'.
        <table>-scrtext_l = 'UF DESTINO'. "*INICIO - AS - 02/10/2020 - CS2020001072
      WHEN 'FAZENDA'.
        <table>-scrtext_l = 'FAZENDA'.
      WHEN 'TEL_NUMBER'.
        <table>-scrtext_l = 'CONTATO'.
      WHEN 'NR_ROT'.
        <table>-scrtext_l = 'ID ROTEIRO'.
      WHEN 'ROTEIRO'.
        <table>-scrtext_l = 'ROTEIRO'.
    ENDCASE.
  ENDLOOP.

  DATA: w_excel     TYPE ole2_object,
        w_workbooks TYPE ole2_object,
        w_workbook  TYPE ole2_object,
        h_int       TYPE ole2_object,
        h_f         TYPE ole2_object,
        h_rows      TYPE ole2_object,
        h_font      TYPE ole2_object,
        h_columns   TYPE ole2_object,
        h_entirecol TYPE ole2_object,
        w_cell      TYPE ole2_object.

  DATA: w_line TYPE i.

  CREATE OBJECT w_excel 'Excel.Application'.

  CALL METHOD OF w_excel 'Workbooks' = w_workbooks.
  CALL METHOD OF w_workbooks 'Add' = w_workbook.

  SET PROPERTY OF w_excel 'Visible' = 1.

  LOOP AT r_table ASSIGNING <table>.
    DATA(tabix) = sy-tabix.
    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = 1 #2 = tabix.
    SET PROPERTY OF w_cell 'Value' = <table>-scrtext_l.
    GET PROPERTY OF w_cell 'Interior'   = h_int.
    GET PROPERTY OF w_cell 'Font'    = h_f.
    SET PROPERTY OF w_cell 'HorizontalAlignment' = -4108 .

  ENDLOOP.

  CALL METHOD OF w_excel 'Rows' = h_rows
    EXPORTING
      #1 = '1:1'.
  GET PROPERTY OF h_rows 'Font' = h_font.
  SET PROPERTY OF h_font 'Bold' = 1.

  FIELD-SYMBOLS: <fs_campo> TYPE any.

  w_line = 2.
  LOOP AT it_excel ASSIGNING FIELD-SYMBOL(<excel>).
    LOOP AT r_table ASSIGNING <table>.

      tabix = sy-tabix.
      ASSIGN COMPONENT <table>-fieldname OF STRUCTURE <excel> TO <fs_campo>.

      CASE <table>-fieldname.

        WHEN 'NR_ROT'.

          wl_name = <fs_campo>.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id        = 'ZROT'
              language  = sy-langu
              name      = wl_name
              object    = 'ZSDROTEIRO'
            TABLES
              lines     = tg_texto
            EXCEPTIONS
              id        = 1
              language  = 2
              name      = 3
              not_found = 4
              OTHERS    = 5.

          CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
          SET PROPERTY OF w_cell 'Value' = <fs_campo>.

        WHEN 'ROTEIRO'.

          IF NOT tg_texto[] IS INITIAL.

            LOOP AT tg_texto.
              concat = |{ concat } { tg_texto-tdline } |.
            ENDLOOP.

            CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
            SET PROPERTY OF w_cell 'Formula' = concat.
            SET PROPERTY OF w_cell 'Value' = concat.

            FREE tg_texto.
            CLEAR concat.

          ENDIF.

        WHEN OTHERS.
          CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
          SET PROPERTY OF w_cell 'Value' = <fs_campo>.

          CASE <table>-fieldname.
            WHEN 'QTD_VINC' OR 'QTD_EMKG'.
              SET PROPERTY OF w_cell 'NumberFormat' = '#,###'.
            WHEN 'PRECO_FRETE'.
              SET PROPERTY OF w_cell 'NumberFormat' = '#,###.##'.
          ENDCASE.

      ENDCASE.
    ENDLOOP.
    ADD 1 TO w_line.
  ENDLOOP.

  CALL METHOD OF w_excel 'Columns' = h_columns
    EXPORTING
      #1 = 'A:Q'.
  GET PROPERTY OF h_columns 'EntireColumn' = h_entirecol.
  SET PROPERTY OF h_entirecol 'Autofit' = 1.

  FREE OBJECT: w_excel,
               w_workbooks,
               w_workbook,
               w_cell.
ENDFORM. "FECHAR_EXCEL
*&---------------------------------------------------------------------*
*&      Module  STATUS_5222  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5222 OUTPUT.

  SET PF-STATUS 'PF5222'.
  SET TITLEBAR  'T5222'.

  ASSIGN 'TY_0163' TO FIELD-SYMBOL(<fs_str>).
  CREATE DATA _str TYPE (<fs_str>).

  it_fcat = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( _str ) ) ) ).

  LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>).

    CASE <fcat>-fieldname.
      WHEN 'CHECK'.
        <fcat>-checkbox = abap_true.
        <fcat>-edit = abap_true.
        <fcat>-scrtext_l = 'Check'.
        <fcat>-outputlen = 03.
        <fcat>-col_pos = 1.
      WHEN 'DESC'.
        <fcat>-scrtext_l = 'Descrição Transportadora'.
        <fcat>-outputlen = 35.
        <fcat>-col_pos = 3.
      WHEN 'LIFNR'.
        <fcat>-scrtext_l = 'Cod. Transportadora'.
        <fcat>-outputlen = 13.
        <fcat>-col_pos = 2.
      WHEN OTHERS.
        <fcat>-no_out = abap_true.
    ENDCASE.

    <fcat>-reptext = <fcat>-scrtext_s = <fcat>-scrtext_m = <fcat>-scrtext_l.

  ENDLOOP.

  _layout-zebra      = abap_true.
  _function = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

  IF _conteiner IS INITIAL.

    CREATE OBJECT _conteiner
      EXPORTING
        container_name = 'COTACAO_VIA_EMAIL'.

    CREATE OBJECT _grid
      EXPORTING
        i_parent = _conteiner.

    _grid->set_table_for_first_display(
      EXPORTING
        is_layout            = _layout
        it_toolbar_excluding = _function
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_0163 ).

    SET HANDLER: lcl_event_handler_5220=>on_data_changed_finished_5222 FOR _grid,
                 lcl_event_handler_5220=>on_data_changed_5222 FOR _grid.

    _grid->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
    _grid->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

  ELSE.
    _stable = abap_true.
    _grid->refresh_table_display( EXPORTING is_stable = _stable ).
  ENDIF.

ENDMODULE.
"FF #154848 - inicio
MODULE status_5223 OUTPUT.

*  PERFORM seleciona_carga_5220.
*  PERFORM completa_carga_5220.
*  PERFORM bloqueia_linhas_5220.
  PERFORM alv_tela_carga_5220.


*
*  SET PF-STATUS 'PF5223'.
*  SET TITLEBAR  'T5223'.
*
*  ASSIGN 'TY_0163' TO <fs_str>.
*  CREATE DATA _str TYPE (<fs_str>).
*
*  it_fcat = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( _str ) ) ) ).
*
*  LOOP AT it_fcat ASSIGNING <fcat>.
*
*    CASE <fcat>-fieldname.
*      WHEN 'CHECK'.
*        <fcat>-checkbox = abap_true.
*        <fcat>-edit = abap_true.
*        <fcat>-scrtext_l = 'Check'.
*        <fcat>-outputlen = 03.
*        <fcat>-col_pos = 1.
*      WHEN 'DESC'.
*        <fcat>-scrtext_l = 'Descrição Transportadora'.
*        <fcat>-outputlen = 35.
*        <fcat>-col_pos = 3.
*      WHEN 'LIFNR'.
*        <fcat>-scrtext_l = 'Cod. Transportadora'.
*        <fcat>-outputlen = 13.
*        <fcat>-col_pos = 2.
*      WHEN OTHERS.
*        <fcat>-no_out = abap_true.
*    ENDCASE.
*
*    <fcat>-reptext = <fcat>-scrtext_s = <fcat>-scrtext_m = <fcat>-scrtext_l.
*
*  ENDLOOP.
*
*  _layout-zebra      = abap_true.
*  _function = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).
*
*  IF _conteiner IS INITIAL.
*
*    CREATE OBJECT _conteiner
*      EXPORTING
*        container_name = 'COTACAO_VIA_EMAIL'.
*
*    CREATE OBJECT _grid
*      EXPORTING
*        i_parent = _conteiner.
*
*    _grid->set_table_for_first_display(
*      EXPORTING
*        is_layout            = _layout
*        it_toolbar_excluding = _function
*      CHANGING
*        it_fieldcatalog      = it_fcat
*        it_outtab            = it_0163 ).
*
*    SET HANDLER: lcl_event_handler_5220=>on_data_changed_finished_5222 FOR _grid,
*                 lcl_event_handler_5220=>on_data_changed_5222 FOR _grid.
*
*    _grid->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
*    _grid->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
*
*  ELSE.
*    _stable = abap_true.
*    _grid->refresh_table_display( EXPORTING is_stable = _stable ).
*  ENDIF.

ENDMODULE.
"FF #154848 - fim


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5222  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5222 INPUT.

  DATA: _seq      TYPE n LENGTH 10.
  DATA: it_rows   TYPE lvc_t_row.
  DATA: mensg     TYPE string.
  DATA: vl_block  TYPE sy-tabix.
  DATA: v_exit    TYPE char1.

  CLEAR: v_exit.

  CASE sy-ucomm.
    WHEN 'SEND'.
      CALL METHOD ctl_alv1_5220->get_selected_rows
        IMPORTING
          et_index_rows = it_rows.

      FREE: carga.
      DELETE it_0163 WHERE check IS INITIAL.

      SELECT MAX( seq )
        INTO _seq
        FROM zsdt0164.

      ADD 1 TO _seq.

      SELECT b~lifnr b~adrnr c~smtp_addr
          FROM lfa1 AS b
          INNER JOIN adr6 AS c ON c~addrnumber EQ b~adrnr
          INTO TABLE it_enviar
        FOR ALL ENTRIES IN  it_0163
      WHERE b~lifnr EQ it_0163-lifnr.

      IF it_0163 IS NOT INITIAL.
        LOOP AT it_0163 INTO DATA(wb_0163).
          READ TABLE it_enviar INTO wa_enviar WITH KEY lifnr = wb_0163-lifnr.
          IF wa_enviar-smtp_addr IS INITIAL.
            CONCATENATE 'Nenhum endereço de e-mail localizado para a transportadora:  '  wb_0163-lifnr INTO mensg.
            MESSAGE  mensg TYPE 'I'.
            v_exit = 'X'.
            EXIT.
          ENDIF.
          CLEAR wb_0163.
        ENDLOOP.

        IF v_exit NE 'X'.
          LOOP AT it_0163 INTO DATA(wa_0163).
            LOOP AT it_enviar INTO wa_enviar WHERE lifnr = wa_0163-lifnr.
              LOOP AT it_rows INTO DATA(wa_rows).
                LOOP AT it_carga_scot_5220 INTO DATA(wa_5220).
                  CHECK wa_rows-index EQ sy-tabix.
                  APPEND wa_5220 TO carga.
                ENDLOOP.
              ENDLOOP.

              LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
                READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
                IF sy-subrc IS INITIAL.

                  vl_cont = sy-tabix.

                  CLEAR: wa_zsdt0133.

                  SELECT SINGLE *
                    FROM zsdt0133
                    INTO wa_zsdt0133
                    WHERE nro_cg EQ wa_carga_5220-nro_cg.

                  wa_zsdt0133-status = 3.
                  MODIFY zsdt0133 FROM wa_zsdt0133.

                  wa_carga_5220-status = 3.
                  wa_carga_5220-icone  = '@FD@'.

                  MODIFY it_carga_scot_5220 FROM wa_carga_5220 INDEX vl_cont.
                ENDIF.
              ENDLOOP.

              it_0164 = VALUE #( FOR ls1 IN carga
                                          FOR ls2 IN it_0163
                                                      (
                                                         seq        = _seq
                                                         nro_cg     = ls1-nro_cg
                                                         lifnr      = ls2-lifnr
                                                         usnam      = sy-uname
                                                         data_atual = sy-datum
                                                         hora_atual = sy-uzeit
                                                       )
                                        ).

              MODIFY zsdt0164 FROM TABLE it_0164.

              PERFORM monta_email.
*            ENDIF.
              CLEAR wa_enviar.
            ENDLOOP.
            CLEAR wa_0163.
          ENDLOOP.
        ELSE.
          LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
            READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
            IF sy-subrc IS INITIAL.
              CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
                EXPORTING
                  chave = wa_carga_5220-nro_cg.
            ENDIF.
          ENDLOOP.
        ENDIF.

        PERFORM seleciona_carga_5220.
        PERFORM completa_carga_5220.
        PERFORM bloqueia_linhas_5220.
        PERFORM alv_tela_carga_5220.
        LEAVE TO SCREEN 0.

      ELSE.

        LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
              EXPORTING
                chave = wa_carga_5220-nro_cg.
          ENDIF.
        ENDLOOP.

        MESSAGE 'Selecione ao menos uma transportadora!'TYPE 'I'.
        CALL METHOD _grid->refresh_table_display
          EXPORTING
            is_stable = _stable.

        SELECT a~lifnr a~lifnr b~name1 b~adrnr
          FROM zsdt0163 AS a
          INNER JOIN lfa1 AS b ON b~lifnr EQ a~lifnr
          INTO TABLE it_0163
          WHERE bukrs EQ l_vkorg-low.

      ENDIF.

    WHEN 'EXIT'.

      LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
        READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave = wa_carga_5220-nro_cg.
        ENDIF.
      ENDLOOP.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

"FF #154848 - inicio
MODULE user_command_5223 INPUT.

  CLEAR: v_exit,
        _seq,
        it_rows,
        mensg ,
        vl_block.

  CASE sy-ucomm.
*    WHEN 'SEND'.
*      CALL METHOD ctl_alv1_5220->get_selected_rows
*        IMPORTING
*          et_index_rows = it_rows.
*
*      FREE: carga.
*      DELETE it_0163 WHERE check IS INITIAL.
*
*      SELECT MAX( seq )
*        INTO _seq
*        FROM zsdt0164.
*
*      ADD 1 TO _seq.
*
*      SELECT b~lifnr b~adrnr c~smtp_addr
*          FROM lfa1 AS b
*          INNER JOIN adr6 AS c ON c~addrnumber EQ b~adrnr
*          INTO TABLE it_enviar
*        FOR ALL ENTRIES IN  it_0163
*      WHERE b~lifnr EQ it_0163-lifnr.
*
*      IF it_0163 IS NOT INITIAL.
*        LOOP AT it_0163 INTO DATA(wb_0163).
*          READ TABLE it_enviar INTO wa_enviar WITH KEY lifnr = wb_0163-lifnr.
*          IF wa_enviar-smtp_addr IS INITIAL.
*            CONCATENATE 'Nenhum endereço de e-mail localizado para a transportadora:  '  wb_0163-lifnr INTO mensg.
*            MESSAGE  mensg TYPE 'I'.
*            v_exit = 'X'.
*            EXIT.
*          ENDIF.
*          CLEAR wb_0163.
*        ENDLOOP.
*
*        IF v_exit NE 'X'.
*          LOOP AT it_0163 INTO DATA(wa_0163).
*            LOOP AT it_enviar INTO wa_enviar WHERE lifnr = wa_0163-lifnr.
*              LOOP AT it_rows INTO DATA(wa_rows).
*                LOOP AT it_carga_scot_5220 INTO DATA(wa_5220).
*                  CHECK wa_rows-index EQ sy-tabix.
*                  APPEND wa_5220 TO carga.
*                ENDLOOP.
*              ENDLOOP.
*
*              LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
*                READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
*                IF sy-subrc IS INITIAL.
*
*                  vl_cont = sy-tabix.
*
*                  CLEAR: wa_zsdt0133.
*
*                  SELECT SINGLE *
*                    FROM zsdt0133
*                    INTO wa_zsdt0133
*                    WHERE nro_cg EQ wa_carga_5220-nro_cg.
*
*                  wa_zsdt0133-status = 3.
*                  MODIFY zsdt0133 FROM wa_zsdt0133.
*
*                  wa_carga_5220-status = 3.
*                  wa_carga_5220-icone  = '@FD@'.
*
*                  MODIFY it_carga_scot_5220 FROM wa_carga_5220 INDEX vl_cont.
*                ENDIF.
*              ENDLOOP.
*
*              it_0164 = VALUE #( FOR ls1 IN carga
*                                          FOR ls2 IN it_0163
*                                                      (
*                                                         seq        = _seq
*                                                         nro_cg     = ls1-nro_cg
*                                                         lifnr      = ls2-lifnr
*                                                         usnam      = sy-uname
*                                                         data_atual = sy-datum
*                                                         hora_atual = sy-uzeit
*                                                       )
*                                        ).
*
*              MODIFY zsdt0164 FROM TABLE it_0164.
*
*              PERFORM monta_email.
**            ENDIF.
*              CLEAR wa_enviar.
*            ENDLOOP.
*            CLEAR wa_0163.
*          ENDLOOP.
*        ELSE.
*          LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
*            READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
*            IF sy-subrc IS INITIAL.
*              CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
*                EXPORTING
*                  chave = wa_carga_5220-nro_cg.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*
*        PERFORM seleciona_carga_5220.
*        PERFORM completa_carga_5220.
*        PERFORM bloqueia_linhas_5220.
*        PERFORM alv_tela_carga_5220.
*        LEAVE TO SCREEN 0.
*
*      ELSE.
*
*        LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
*          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
*          IF sy-subrc IS INITIAL.
*            CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
*              EXPORTING
*                chave = wa_carga_5220-nro_cg.
*          ENDIF.
*        ENDLOOP.
*
*        MESSAGE 'Selecione ao menos uma transportadora!'TYPE 'I'.
*        CALL METHOD _grid->refresh_table_display
*          EXPORTING
*            is_stable = _stable.
*
*        SELECT a~lifnr a~lifnr b~name1 b~adrnr
*          FROM zsdt0163 AS a
*          INNER JOIN lfa1 AS b ON b~lifnr EQ a~lifnr
*          INTO TABLE it_0163
*          WHERE bukrs EQ l_vkorg-low.
*
*      ENDIF.
*
    WHEN 'EXIT'.

      LOOP AT it_carga_scot_5220 INTO wa_carga_5220.
        READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
            EXPORTING
              chave = wa_carga_5220-nro_cg.
        ENDIF.
      ENDLOOP.

*      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
"FF #154848 - fim

FORM cotacao_email TABLES t_cg.

  DATA:   wl_name  TYPE thead-tdname.

  CLEAR:  it_email.
  FREE:   it_email.

  SELECT nro_lote nro_cg dt_entrega FROM zsdt0129
   INTO TABLE it_zsdt0129
   WHERE nro_cg IN t_cg.

  CHECK NOT it_zsdt0129[] IS INITIAL.

  SELECT * FROM zsdt0130
    INTO TABLE @DATA(it_zsdt0130)
    FOR ALL ENTRIES IN @it_zsdt0129
    WHERE nro_lote EQ @it_zsdt0129-nro_lote.

  SELECT * FROM zsdt0131
    INTO TABLE @DATA(it_zsdt0131)
    FOR ALL ENTRIES IN @it_zsdt0129
    WHERE nro_lote EQ @it_zsdt0129-nro_lote
    AND status NE @abap_true.

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
* ---> S4 Migration - 04/07/2023 - FTM - Início
*    SELECT * FROM konv
*      INTO TABLE @DATA(it_konv)
*      FOR ALL ENTRIES IN @it_vbak
*      WHERE knumv EQ @it_vbak-knumv
*       AND kposn EQ @it_vbak-posnr
*       AND kschl EQ 'PR00'.
    DATA: it_konv TYPE TABLE OF konv.
    TRY.
        cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
          EXPORTING it_selection_attribute = VALUE #( ( fieldname = 'KNUMV'
                                                        value     = it_vbak-knumv )
                                                      ( fieldname = 'KPOSN'
                                                        value     = it_vbak-posnr )
                                                      ( fieldname = 'KSCHL'
                                                        value     = 'PR00' ) )
          IMPORTING et_prc_element_classic_format = it_konv ).
      CATCH cx_prc_result ##NO_HANDLER.
    ENDTRY.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
  ENDIF.

  IF NOT it_zsdt0131 IS INITIAL.
    SELECT * FROM zsdt0132
      INTO TABLE @DATA(it_zsdt0132)
      FOR ALL ENTRIES IN @it_zsdt0131
       WHERE nr_rot EQ @it_zsdt0131-cod_loc_emb.

    SELECT * FROM makt
      INTO TABLE @DATA(it_makt)
      FOR ALL ENTRIES IN @it_zsdt0131
     WHERE matnr EQ @it_zsdt0131-matnr.
  ENDIF.

  IF NOT it_zsdt0130 IS INITIAL.
    SELECT * FROM zsdt0132
      APPENDING TABLE it_zsdt0132
      FOR ALL ENTRIES IN it_zsdt0130
       WHERE nr_rot EQ it_zsdt0130-nr_rot.

    SELECT * FROM kna1
      INTO TABLE @DATA(it_kna1)
      FOR ALL ENTRIES IN @it_zsdt0130
      WHERE kunnr EQ @it_zsdt0130-kunnr.
  ENDIF.

  LOOP AT it_zsdt0129.

    LOOP AT it_zsdt0131 INTO DATA(wa_zsdt0131) WHERE nro_lote EQ it_zsdt0129-nro_lote.

      it_email-nro_cg     = it_zsdt0129-nro_cg.
      it_email-dt_entrega = it_zsdt0129-dt_entrega.
      it_email-vkbur      = wa_zsdt0131-vkbur.
      it_email-vbeln      = wa_zsdt0131-vbeln.

      it_email-qtd_vinc   = wa_zsdt0131-qtd_vinc.
      it_email-um         = wa_zsdt0131-um.
      it_email-qtd_emkg   = wa_zsdt0131-qtd_emkg.

      TRY .
          DATA(wa_zsdt0132) = it_zsdt0132[ nr_rot = wa_zsdt0131-cod_loc_emb ].
          it_email-local      = wa_zsdt0132-rot_desc.
          it_email-city1      = wa_zsdt0132-city1.
          it_email-uf         = wa_zsdt0132-uf.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          DATA(wa_zsdt0130) = it_zsdt0130[ nro_lote = it_zsdt0129-nro_lote
                                            nro_sol = wa_zsdt0131-nro_sol ].
          it_email-seq_ent_cg = wa_zsdt0130-seq_ent_cg.
          it_email-seq_desc_cg = wa_zsdt0130-seq_ent_cg.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          it_email-name1 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-name1.
          it_email-mcod3 = it_kna1[ kunnr = wa_zsdt0130-kunnr ]-mcod3.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          it_email-maktx = it_makt[ matnr = wa_zsdt0131-matnr ]-maktx.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY .
          wa_zsdt0132 = it_zsdt0132[ nr_rot = wa_zsdt0130-nr_rot ].
          it_email-fazenda    = wa_zsdt0132-rot_desc.
          it_email-tel_number = wa_zsdt0132-tel_number.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      it_email-nr_rot = wa_zsdt0130-nr_rot.

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
          SET COUNTRY 'BR'.
          it_email-preco_frete = ( ( ( ( wa_zsdt0131-qtd_vinc * wa_zsdt0131-brgew ) * wa_konv-kbetr ) / 1000 ) * wa_konv-kkurs ).
        WHEN OTHERS.
          SET COUNTRY 'BR'.
          it_email-preco_frete = ( ( wa_zsdt0131-qtd_vinc * wa_konv-kbetr ) * wa_konv-kkurs ).
      ENDCASE.

      wl_name = it_email-nr_rot.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id        = 'ZROT'
          language  = sy-langu
          name      = wl_name
          object    = 'ZSDROTEIRO'
        TABLES
          lines     = tg_texto
        EXCEPTIONS
          id        = 1
          language  = 2
          name      = 3
          not_found = 4
          OTHERS    = 5.

      IF NOT tg_texto[] IS INITIAL.

        LOOP AT tg_texto.
          concat = |{ concat } { tg_texto-tdline } |.
          it_email-roteiro = concat.
        ENDLOOP.

        FREE tg_texto.
        CLEAR concat.
      ENDIF.

      APPEND it_email.

    ENDLOOP.
  ENDLOOP.

ENDFORM.

FORM monta_email.

  DATA: wl_cont TYPE sy-tabix,
        wl_mod  TYPE sy-tabix,
        wl_pos  TYPE sy-tabix,
        wl_line TYPE sy-tabix.

  DATA: vlr_frete(15) TYPE p DECIMALS 2.

  CLEAR: lt_mailrecipientes[],
         lt_mailtxt[],
         lt_mailsubject.


  lt_mailrecipientes-rec_type = 'U'.
  lt_mailrecipientes-receiver = wa_enviar-smtp_addr.
  APPEND lt_mailrecipientes.

  IF email = 1.
    lt_mailsubject-obj_langu =  sy-langu.
    lt_mailsubject-obj_descr = 'Confirmação da Contratação de Frete'.
  ELSE.
    lt_mailsubject-obj_langu =  sy-langu.
    lt_mailsubject-obj_descr = 'Cotação de Frete'.
  ENDIF.

  lt_mailtxt = '<!DOCTYPE HTML><html><body><STYLE>'.
  APPEND lt_mailtxt.


  lt_mailtxt = 'TABLE,TH, TD{ BORDER:1PX SOLID BLACK; BORDER-COLLAPSE: COLLAPSE; }'.
  APPEND lt_mailtxt.

  lt_mailtxt = 'TH, TD { PADDING: 1PX; }'.
  APPEND lt_mailtxt.

  lt_mailtxt = 'TH { TEXT-ALIGN: LEFT; }'.
  APPEND lt_mailtxt.

  lt_mailtxt = ' TABLE#T01 TH { BACKGROUND-COLOR:#c9daf9; COLOR: black; } </STYLE>'.
  APPEND lt_mailtxt.

  IF email = 1.
    lt_mailtxt = 'Embarcador <br/ ><br>'.
    APPEND lt_mailtxt.
    lt_mailtxt = 'Estamos de acordo com o frete cotado.<br /><br>'.
    APPEND lt_mailtxt.

    CONCATENATE 'Favor programar o carregamento e nos enviar os seguintes documentos'
                ' abaixo no e-mail insumos.sementes@amaggi.com.br: <br /><br>'
                '<b>- Nome completo do motorista</b><br />'  INTO  lt_mailtxt.
    APPEND lt_mailtxt.
    CONCATENATE '<b>- CPF</b><br />'
                '<b>- Telefone Celular</b><br />'
                '<b>- Placas do Caminhão (Cavalo e carretas)</b><br /><br>' INTO  lt_mailtxt.
    APPEND lt_mailtxt.
    CONCATENATE '<b>Observação: Todas as notas deverão ser acompanhadas do boleto '
                'fase e termos de conformidade das sementes.</b><br /><br>'  INTO lt_mailtxt.
    APPEND lt_mailtxt.
  ELSE.
    lt_mailtxt = ' <H4 ALIGN=LEFT>Favor realizar a cotação abaixo com o FRETE FINAL + ICMS caso tenha.</H4>'.
    APPEND lt_mailtxt.
    lt_mailtxt = ' <H4 ALIGN=LEFT>Se a carga CONTEMPLAR LOTAÇÃO informar o frete final por ton. </H4>'.
    APPEND lt_mailtxt.
  ENDIF.

  lt_mailtxt = '<table border="1" style="width:200%" id="T01">'.
  APPEND lt_mailtxt.

  CONCATENATE '<tr><th align=center><font size="2">Local de Carregamento</font></th>'
              '<th align=center><font size="2">Municipio</font></th>' INTO lt_mailtxt.
  APPEND lt_mailtxt.

  CONCATENATE '<th><font size="2">UF</font></th>'
              '<th><font size="2">Filial</font></th>' INTO lt_mailtxt.
  APPEND lt_mailtxt.

  CONCATENATE '<th><font size="2">Carga</font></th>'
              ' <th><font size="2">Data Entrega</font></th>' INTO lt_mailtxt.
  APPEND lt_mailtxt.

  CONCATENATE  '<th><font size="2">Seq. Descarga</font></th>'
               '<th><font size="2">Seq.Carregamento Sementeira</font></th>' INTO  lt_mailtxt.
  APPEND lt_mailtxt.

  CONCATENATE   '<th><font size="2">Cliente</font></th>'
                '<th><font size="2">Cultivar</font></th>' INTO  lt_mailtxt.
  APPEND lt_mailtxt.

  CONCATENATE  '<th><font size="2">Quantidade</font></th>'
               '<th><font size="2">Unidade</font></th>'  INTO lt_mailtxt.
  APPEND lt_mailtxt.

  CONCATENATE  '<th><font size="2">Qtd/Quilos</font></th>'
               '<th><font size="2">Valor Total Carga</font></th>' INTO lt_mailtxt.
  APPEND lt_mailtxt.

  IF email = 1.
    lt_mailtxt = '<th><font size="2">Valor Frete</font></th>'.
    APPEND lt_mailtxt.
  ENDIF.

  CONCATENATE  '<th><font size="2">Municipio Destino</font></th>'
               '<th><font size="2">Fazenda</font></th>'     INTO lt_mailtxt.
  APPEND lt_mailtxt.

  CONCATENATE  '<th><font size="2">Contato</font></th>'
               '<th><font size="2">Roteiro</font></th></tr>'  INTO lt_mailtxt.
  APPEND lt_mailtxt.



  SORT it_email BY nro_cg ASCENDING.
  LOOP AT it_email.
    wa_saida-local        = it_email-local.
    wa_saida-city1        = it_email-city1.
    wa_saida-uf           = it_email-uf.
    wa_saida-vkbur        = it_email-vkbur.
    wa_saida-nro_cg       = it_email-nro_cg.
    wa_saida-seq_desc_cg  = it_email-seq_desc_cg.
    wa_saida-vbeln        = it_email-vbeln.
    wa_saida-name1        = it_email-name1.
    wa_saida-maktx        = it_email-maktx.
    wa_saida-um           = it_email-um.
    wa_saida-seq_ent_cg   = it_email-seq_ent_cg.
    wa_saida-mcod3        = it_email-mcod3.
    wa_saida-fazenda      = it_email-fazenda.
    wa_saida-tel_number   = it_email-tel_number.
    wa_saida-nr_rot       = it_email-nr_rot.
    wa_saida-roteiro      = it_email-roteiro.

    CLEAR vlr_frete.

    SELECT SINGLE * FROM zsdt0133
      INTO CORRESPONDING FIELDS OF wa_zsdt0133
    WHERE nro_cg EQ it_email-nro_cg.

    vlr_frete = wa_zsdt0133-preco_frete.
    WRITE vlr_frete TO  wa_saida-preco_frete2.
    CONDENSE wa_saida-preco_frete2.

    WRITE it_email-preco_frete TO wa_saida-preco_frete.
    CONDENSE wa_saida-preco_frete .

    WRITE it_email-qtd_vinc TO wa_saida-qtd_vinc.
    CONDENSE wa_saida-qtd_vinc.

    WRITE it_email-qtd_emkg TO wa_saida-qtd_emkg.
    CONDENSE wa_saida-qtd_emkg.

    CONCATENATE it_email-dt_entrega+6(2) '.' it_email-dt_entrega+4(2) '.' it_email-dt_entrega+0(4) INTO wa_saida-dt_entrega.

    CONCATENATE '<tr><td><font size="2">'wa_saida-local'</font></td>' INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE  '<td><font size="2">'wa_saida-city1'</font></td>'  INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-uf'</font></td>'   INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-vkbur'</font></td>'   INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-nro_cg'</font></td>'   INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-dt_entrega'</font></td>'  INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-seq_desc_cg'</font></td>'  INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-seq_ent_cg'</font></td>'  INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-name1'</font></td>'  INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-maktx'</font></td>'   INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">' wa_saida-qtd_vinc'</font></td>'  INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-um'</font></td>' INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-qtd_emkg'</font></td>' INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-preco_frete'</font></td>' INTO lt_mailtxt.
    APPEND lt_mailtxt.

    IF email = 1.
      CONCATENATE '<td><font size="2">'wa_saida-preco_frete2'</font></td>' INTO lt_mailtxt.
      APPEND lt_mailtxt.
    ENDIF.

    CONCATENATE  '<td><font size="2">'wa_saida-mcod3'</font></td>'  INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-fazenda'</font></td>' INTO lt_mailtxt.
    APPEND lt_mailtxt.

    CONCATENATE '<td><font size="2">'wa_saida-tel_number'</font></td>' INTO lt_mailtxt.
    APPEND lt_mailtxt.

    lt_mailtxt = '<td><font size="2">'.
    APPEND lt_mailtxt.

    wl_cont =  strlen( it_email-roteiro ).
    WHILE wl_pos < wl_cont.
      wl_line = wl_cont - wl_pos.

      IF wl_line >= 91.
        wl_line = 91.
      ENDIF.

      wa_saida-roteiro = it_email-roteiro+wl_pos(wl_line).
      ADD 91 TO wl_pos.

      IF wa_saida-roteiro IS NOT INITIAL.
        lt_mailtxt = wa_saida-roteiro.
        APPEND lt_mailtxt.
      ENDIF.

      CLEAR:  wa_saida-roteiro.
    ENDWHILE.
    CLEAR: wl_cont, wl_mod, wl_pos, wl_line.

    lt_mailtxt = '</font></td></tr>'.
    APPEND lt_mailtxt.

    APPEND wa_saida TO it_saida.
    CLEAR: wa_saida, wa_zsdt0133.
  ENDLOOP.

  lt_mailtxt = '</table></body></html>'.
  APPEND lt_mailtxt.

  PERFORM enviar.

ENDFORM.

FORM enviar.

  DATA: vuser         TYPE sy-uname.

  vuser = sy-uname.
  sy-uname = 'JOBADM'.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = lt_mailsubject
      document_type              = 'HTM'
    TABLES
      object_content             = lt_mailtxt
      receivers                  = lt_mailrecipientes
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  sy-uname = vuser.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

ENDFORM.


FORM f_volta_status_carga_criada TABLES p_it_carga_5220
                                  USING p_alv.

  DATA: vl_lines ,
        vl_answer.

  CLEAR: it_carga_canc_5220, vl_check, vl_lines, wa_zsdt0133.

  CASE p_alv.

    WHEN 'ALV1'.
      CALL METHOD ctl_alv1_5220->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

    WHEN 'ALV2'.
      CALL METHOD ctl_alv2_5220->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.
  ENDCASE.

  DESCRIBE TABLE it_selected_rows LINES vl_lines.

  IF vl_lines NE 1.
    MESSAGE TEXT-054 TYPE 'S' DISPLAY LIKE 'E'.
    MOVE abap_true TO vl_check.
  ENDIF.

  READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
  READ TABLE  p_it_carga_5220 INTO wa_carga_5220 INDEX wa_selected_rows-index.

  IF wa_carga_5220-status >= '4'.
    MESSAGE  'CARGA COM AUTORIZAÇÃO DE EMBARQUE GERADA, ENTRAR EM CONTATO COM A EQUIPE DE INSUMOS.' TYPE 'E'.

  ELSE.

    wa_carga_5220-status = '1'.

    MODIFY p_it_carga_5220 FROM wa_carga_5220 INDEX wa_selected_rows-index.

    SELECT * FROM zsdt0346
    WHERE nro_carga = @wa_carga_5220-nro_cg
    INTO TABLE @DATA(lt_346).

    IF sy-subrc = 0.

      " Atualização direta no banco
      UPDATE zsdt0346
        SET cancelado   = @abap_true,
            user_cancel = @sy-uname,
            data_cancel = @sy-datum,
            hora_cancel = @sy-uzeit
        WHERE nro_carga = @wa_carga_5220-nro_cg.

    ENDIF.


**********************************************************************
    CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
      EXPORTING
        chave          = wa_carga_5220-nro_cg
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
       WHERE nro_cg EQ wa_carga_5220-nro_cg.

      IF wa_zsdt0133-status NE wa_carga_5220-status.

        UPDATE zsdt0133 SET cod_transportadora = ''
                            preco_frete = 0
                            status = '1'
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
              chave = wa_carga_5220-nro_cg.
                  LEAVE TO SCREEN 5000.
        ENDIF.

      ENDIF.

      CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
        EXPORTING
          chave = wa_carga_5220-nro_cg.

    ENDIF.

**********************************************************************

  ENDIF.

  CLEAR: wa_carga_5220.

  CASE p_alv.
    WHEN 'ALV1'.

      CALL METHOD ctl_alv1_5220->refresh_table_display
        EXPORTING
          is_stable = _stable.

    WHEN 'ALV2'.

      CALL METHOD ctl_alv2_5220->refresh_table_display
        EXPORTING
          is_stable = _stable.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
FORM alv_carga_double_click_5220  USING    p_e_row_index.

  CLEAR it_carga_iti_5220[].

  READ TABLE it_carga_ccot_5220 INTO DATA(ls_carga_ccot) INDEX p_e_row_index.

  SELECT
    t346~nro_carga AS nro_cg,
    t345~operacao,
    t346~tipo_transporte,
    t346~ponto_coleta,
    t346~local_entrega,
    t346~itinerario,
    tvro~distz AS distancia_km,
    t346~unidade_condicao,
    t346~valor,
    t346~unidade_medida

    FROM zsdt0346 AS t346
    LEFT OUTER JOIN zsdt0345 AS t345 "A tabela principal zsdt0346 será selecionada mesmo que náo houver correspondencia na zsdt0345 ou tvro
      ON t346~id_operacao = t345~id
    LEFT OUTER JOIN tvro
      ON tvro~route = t346~itinerario
    WHERE t346~nro_carga = @ls_carga_ccot-nro_cg
      AND t346~cancelado IS INITIAL
    INTO TABLE @DATA(it_itinerario_carga).

  IF sy-subrc = 0 AND it_itinerario_carga[] IS NOT INITIAL.

    it_carga_iti_5220 = CORRESPONDING #( it_itinerario_carga ).

    LOOP AT it_carga_iti_5220 ASSIGNING FIELD-SYMBOL(<data>).

      SHIFT <data>-nro_cg LEFT DELETING LEADING '0'.
      CONDENSE <data>-nro_cg NO-GAPS.

    ENDLOOP.


    IF ls_carga_ccot-edit = abap_false.
      PERFORM f_editar_exibir_alv3. "Itinerários da carga
    ENDIF.

  ELSE.

    MESSAGE 'Os dados do itinerário da carga não foram encontrados.' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CALL METHOD ctl_alv3_5220->refresh_table_display
    EXPORTING
      is_stable = _stable.



ENDFORM.

FORM f_editar_exibir_alv3. "Itinerários da carga


  DATA: vl_cont       TYPE i,
        it_celltab    TYPE lvc_t_styl,
        wa_carga_5220 TYPE ty_carga_5220.

  LOOP AT it_carga_iti_5220 INTO DATA(wa_carga_iti_5220).

    vl_cont = vl_cont + 1.

    CLEAR: it_celltab, wa_carga_iti_5220-cellstyles.
    "REFRESH IT_CELLTAB.
    PERFORM fill_celltab_5220 USING abap_false
                                    abap_true
                              CHANGING it_celltab.
    "CLEAR WA_ZSDT0132-CELLSTYLES.
    INSERT LINES OF it_celltab INTO TABLE wa_carga_iti_5220-cellstyles.
    MODIFY it_carga_iti_5220 FROM wa_carga_iti_5220 INDEX vl_cont.

    CALL METHOD ctl_alv3_5220->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDLOOP.

ENDFORM.
