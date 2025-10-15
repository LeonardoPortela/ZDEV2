*&---------------------------------------------------------------------*
*&  Include           ZSDR0117_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD: on_finished.

    DATA: v_time_active TYPE c.

    PERFORM f_get_status_time CHANGING v_time_active.

    IF ( v_time_active EQ abap_true ).

      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = 'CLOCK'.

      go_clock->interval = c_time_interval.

      CALL METHOD go_clock->run.

      CHECK wg_cab_boletim_prod-id_boletim IS NOT INITIAL.

      PERFORM: f_gerar_documentos,
               f_load_notas_vinc,
               f_refresh_alv USING '0120_02',
               f_leave_to_screen.

    ENDIF.

  ENDMETHOD.                           " ON_FINISHED
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION


CLASS lcl_alv_toolbar_0100_01 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_handler_0100_01 IMPLEMENTATION.

  METHOD on_data_changed_finished.

    DATA: ls_good      TYPE lvc_s_modi.
    FREE: it_msg_return.
    CLEAR: wa_mensagem.
    CHECK et_good_cells[] IS NOT INITIAL.

    LOOP AT et_good_cells INTO ls_good.

      READ TABLE it_saida_0100_01 ASSIGNING FIELD-SYMBOL(<fs_saida_0100_01>) INDEX ls_good-row_id.
      CHECK sy-subrc = 0.

      CASE ls_good-fieldname.
        WHEN 'QTDE_CONSUMO'.
          LOOP AT it_saida_0100_02 ASSIGNING FIELD-SYMBOL(<fs_saida_0100_02>).
            PERFORM f_calc_perc_rendimento CHANGING <fs_saida_0100_02>.
          ENDLOOP.
      ENDCASE.

    ENDLOOP.

    PERFORM fm_check_erro.

    LEAVE TO SCREEN 0100.


  ENDMETHOD.                    "on_data_changed_finishe

  METHOD catch_hotspot.
  ENDMETHOD.

  METHOD on_onf4.
  ENDMETHOD.                    "ON_ONF4


ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

CLASS lcl_alv_toolbar_0100_02 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_handler_0100_02 IMPLEMENTATION.

  METHOD handle_data_changed. "Adicionado CS2020001194

    LOOP AT er_data_changed->mt_good_cells INTO DATA(w_row).

      IF w_row-fieldname = 'QTDE_MI' OR w_row-fieldname = 'QTDE'.

        READ TABLE it_saida_0100_02 ASSIGNING FIELD-SYMBOL(<fs_saida_0100_02>) INDEX w_row-row_id.

        CHECK sy-subrc = 0.

*---------------------------------------------------------------------
* Captura o valor da Célula  QTDE
*---------------------------------------------------------------------
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = w_row-row_id
            i_fieldname = 'QTDE'
          IMPORTING
            e_value     = <fs_saida_0100_02>-qtde.

*---------------------------------------------------------------------
* Captura o valor da Célula  QTDE_MI
*---------------------------------------------------------------------
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = w_row-row_id
            i_fieldname = 'QTDE_MI'
          IMPORTING
            e_value     = <fs_saida_0100_02>-qtde_mi.

*---------------------------------------------------------------------
* Efetua Calculo da quantidade externa
*---------------------------------------------------------------------
        <fs_saida_0100_02>-qtde_me = <fs_saida_0100_02>-qtde - <fs_saida_0100_02>-qtde_mi.

*---------------------------------------------------------------------
* Se a quantidade interna for maior que a quantidade definida
*apresenta erro na tela
*---------------------------------------------------------------------
*        IF <fs_saida_0100_02>-qtde < <fs_saida_0100_02>-QTDE_MI.
*
*          CALL METHOD er_data_changed->add_protocol_entry
*            EXPORTING
*              i_msgid     = 'SU'
*              i_msgty     = 'E'
*              i_msgno     = '000'
*              i_msgv1     = 'Valor maior que a quantidade disponível'
*              i_fieldname = 'QTDE_MI'
*              i_row_id    = w_row-row_id.
*
*        ENDIF.

      ENDIF.

    ENDLOOP.

    " NOTE: removes all protocol entries including the previously added one!!!
    er_data_changed->display_protocol( ).


  ENDMETHOD. "handle_data_changed

  METHOD on_data_changed_finished.

    DATA: ls_good      TYPE lvc_s_modi.

    CHECK et_good_cells[] IS NOT INITIAL.

    LOOP AT et_good_cells INTO ls_good.

      READ TABLE it_saida_0100_02 ASSIGNING FIELD-SYMBOL(<fs_saida_0100_02>) INDEX ls_good-row_id.
      CHECK sy-subrc = 0.

      CASE ls_good-fieldname.
        WHEN 'QTDE'.
          PERFORM f_calc_perc_rendimento   USING <fs_saida_0100_02>.
      ENDCASE.

    ENDLOOP.

    CALL METHOD obj_alv_0100_02->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "on_data_changed_finishe

  METHOD catch_hotspot.
  ENDMETHOD.

  METHOD on_onf4.
  ENDMETHOD.                    "ON_ONF4


ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

CLASS lcl_alv_toolbar_0120_01 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    IF ( wg_cab_boletim_prod-id_boletim IS NOT INITIAL ) AND ( wg_cab_boletim_prod-com_nf EQ abap_true ).
      CLEAR ty_toolbar.
      ty_toolbar-icon      = icon_column_right.
      ty_toolbar-function  = 'VINC_SALDO'.
      ty_toolbar-text      = 'Vincular Saldo'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

*    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.


  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'VINC_SALDO'.
        PERFORM f_vincular_saldo.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_handler_0120_01 IMPLEMENTATION.

  METHOD on_data_changed_finished.
  ENDMETHOD.                    "on_data_changed_finishe

  METHOD catch_hotspot.

    DATA: t_1bnfdoc TYPE TABLE OF j_1bnfdoc.

    CASE e_column_id.
      WHEN 'SALDO'.

        CLEAR: it_saida_0121[].

        READ TABLE it_saida_0120_01 INTO wa_saida_0120_01 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_01-saldo IS NOT INITIAL ).

        CLEAR: t_1bnfdoc[].

        SELECT *
          FROM j_1bnfdoc INTO TABLE t_1bnfdoc
           FOR ALL ENTRIES IN it_zsdt0251
         WHERE docnum EQ it_zsdt0251-docnum.

*-CS2021000386 - 28.04.2021 - JT - inicio
        SELECT SINGLE emissao_nf
                 INTO @DATA(l_emissao_nf)
                 FROM zsdt0253
                WHERE branch = @wg_cab_boletim_prod-branch.
*-CS2021000386 - 28.04.2021 - JT - fim

        LOOP AT it_zsdt0251 INTO DATA(wl_zsdt0251) WHERE branch EQ wa_saida_0120_01-branch
                              AND charg  EQ wa_saida_0120_01-charg.
          CLEAR: wa_saida_0121.

*-CS2021000386 - 28.04.2021 - JT - inicio
          IF l_emissao_nf = abap_true.
            IF wg_cab_boletim_prod-categ_soja = 'CO' AND
               wl_zsdt0251-lgort_v <> 'PO58'.
              CONTINUE.
            ENDIF.
            IF wg_cab_boletim_prod-categ_soja = 'RR' AND
               wl_zsdt0251-lgort_v <> 'PO17'.
              CONTINUE.
            ENDIF.

            "FF #191283 - inicio
            IF wg_cab_boletim_prod-categ_soja = 'RE' AND
               wl_zsdt0251-lgort_v <> 'POD2'.
              CONTINUE.
            ENDIF.

            IF wg_cab_boletim_prod-categ_soja = 'CE' AND
               wl_zsdt0251-lgort_v <> 'POD3'.
              CONTINUE.
            ENDIF.
            "FF #191283 - fim.

          ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim


          wa_saida_0121-docnum = wl_zsdt0251-docnum.
          wa_saida_0121-saldo  = wl_zsdt0251-qtde_saldo.

          READ TABLE t_1bnfdoc INTO DATA(wl_doc) WITH KEY docnum = wl_zsdt0251-docnum.
          IF sy-subrc EQ 0.
            wa_saida_0121-nfenum = wl_doc-nfenum.
            wa_saida_0121-docdat = wl_doc-docdat.
          ENDIF.

          APPEND wa_saida_0121 TO it_saida_0121.

        ENDLOOP.

        CHECK it_saida_0121[] IS NOT INITIAL.

        CALL SCREEN 0121 STARTING AT 05 05.

    ENDCASE.

  ENDMETHOD.

  METHOD on_onf4.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION


CLASS lcl_alv_toolbar_0120_02 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA(_boletim_com_nf) = abap_false.

    IF ( wg_cab_boletim_prod-id_boletim IS NOT INITIAL ) AND ( wg_cab_boletim_prod-com_nf EQ abap_true ).
      _boletim_com_nf = abap_true.
    ENDIF.

    IF ( _boletim_com_nf EQ abap_true ).
      CLEAR ty_toolbar.
      ty_toolbar-icon      = icon_column_left.
      ty_toolbar-function  = 'DESVINC_SALDO'.
      ty_toolbar-text      = 'Desvincular Saldo'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    "Gerar Documentos
    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_generate.
    ty_toolbar-function  = 'GENERATE_DOCS'.
    ty_toolbar-text      = 'Gerar Documentos'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_storno.
    ty_toolbar-function  = 'ESTORNO_DOCS'.
    ty_toolbar-text      = 'Estornar Documentos'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    IF ( _boletim_com_nf EQ abap_true ).

      CLEAR ty_toolbar.
      ty_toolbar-icon      = icon_viewer_optical_archive.
      ty_toolbar-function  = 'ZNFE_DEVOL'.
      ty_toolbar-text      = 'ZNFE Devolução'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      CLEAR ty_toolbar.
      ty_toolbar-icon      = icon_viewer_optical_archive.
      ty_toolbar-function  = 'ZNFE_IND'.
      ty_toolbar-text      = 'ZNFE Industr.'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      CLEAR ty_toolbar.
      ty_toolbar-icon      = icon_viewer_optical_archive.
      ty_toolbar-function  = 'ZNFE_RFL'.
      ty_toolbar-text      = 'ZNFE RFL'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      CLEAR ty_toolbar.
      ty_toolbar-icon      = icon_viewer_optical_archive.
      ty_toolbar-function  = 'ZNFE_RCO'.
      ty_toolbar-text      = 'ZNFE RCO'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

    ENDIF.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'GENERATE_DOCS'.
        PERFORM f_gerar_documentos.
      WHEN 'ESTORNO_DOCS'.
        PERFORM f_estornar_documentos.
      WHEN 'DESVINC_SALDO'.
        PERFORM f_desvincular_saldo.
      WHEN 'ZNFE_DEVOL'.
        PERFORM f_call_znfe_nf_devol.
      WHEN 'ZNFE_IND'.
        PERFORM f_call_znfe_nf_ind.
      WHEN 'ZNFE_RFL'.
        PERFORM f_call_znfe_nf_rfl.
      WHEN 'ZNFE_RCO'.
        PERFORM f_call_znfe_nf_rco.

        "WHEN 'GENERATE_DEVOL'.
        "  PERFORM F_GERAR_NF_DEVOLUCAO.
        "WHEN 'GENERATE_ENT_DEVOL'.
        "  PERFORM F_GERAR_NF_ENT_DEV.
        "WHEN 'GENERATE_DOC_PRODUCAO'.
        "  PERFORM F_GERAR_DOC_PRODUCAO.
        "WHEN 'ESTORNO_DOC_PROD'.
        "  PERFORM F_ESTORNO_DOC_PROD.
        "WHEN 'GENERATE_RFL'.
        "  PERFORM F_GERAR_NF_RFL.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_handler_0120_02 IMPLEMENTATION.

  METHOD on_data_changed_finished.

  ENDMETHOD.                    "on_data_changed_finishe

  METHOD catch_hotspot.

    DATA: t_1bnfdoc TYPE TABLE OF j_1bnfdoc.

    CASE e_column_id.
      WHEN 'SALDO_VINC'.

        CLEAR: it_saida_0121[].

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-saldo_vinc IS NOT INITIAL ).

        CLEAR: it_zsdt0249[], t_1bnfdoc[].

        SELECT *
          FROM zsdt0249 INTO TABLE it_zsdt0249
         WHERE id_boletim EQ wa_saida_0120_02-id_boletim
           AND branch     EQ wa_saida_0120_02-branch
           AND charg      EQ wa_saida_0120_02-charg
           AND id_agrp    EQ wa_saida_0120_02-id_agrp.

        CHECK it_zsdt0249[] IS NOT INITIAL.

        SELECT *
          FROM j_1bnfdoc INTO TABLE t_1bnfdoc
           FOR ALL ENTRIES IN it_zsdt0249
         WHERE docnum EQ it_zsdt0249-docnum.

        LOOP AT it_zsdt0249 INTO DATA(wl_zsdt0249).
          CLEAR: wa_saida_0121.

          wa_saida_0121-docnum = wl_zsdt0249-docnum.
          wa_saida_0121-saldo  = wl_zsdt0249-qtde_vinc.

          READ TABLE t_1bnfdoc INTO DATA(wl_doc) WITH KEY docnum = wl_zsdt0249-docnum.
          IF sy-subrc EQ 0.
            wa_saida_0121-nfenum = wl_doc-nfenum.
            wa_saida_0121-docdat = wl_doc-docdat.
          ENDIF.

          APPEND wa_saida_0121 TO it_saida_0121.

        ENDLOOP.

        CALL SCREEN 0121 STARTING AT 05 05.

*-----------------------------------------------------------------------------------------------------------------------*
*     NF Devolução - Saida e Entrada
*-----------------------------------------------------------------------------------------------------------------------*

      WHEN 'SEQLCTO_DEVOL'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_devol IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_devol.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_DEVOL'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_devol IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_devol.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_DEVOL'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_devol IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_devol,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.


      WHEN 'SEQLCTO_ENT_DEV'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ent_dev IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_ent_dev.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_ENT_DEV'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_ent_dev IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_ent_dev.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_ENT_DEV'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ent_dev IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_ent_dev,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.

*-----------------------------------------------------------------------------------------------------------------------*
*     NF Industrialização - Saida e Entrada
*-----------------------------------------------------------------------------------------------------------------------*

      WHEN 'SEQLCTO_IND'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ind IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_ind.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_IND'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_ind IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_ind.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_IND'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ind IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_ind,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.


      WHEN 'SEQLCTO_ENT_IND'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ent_ind IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_ent_ind.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_ENT_IND'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_ent_ind IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_ent_ind.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_ENT_IND'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ent_ind IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_ent_ind,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.


*-----------------------------------------------------------------------------------------------------------------------*
*     Documentos Produção
*-----------------------------------------------------------------------------------------------------------------------*

      WHEN 'DOC_PROD_01'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-doc_prod_01 IS NOT INITIAL ).

*---> 19.07.2023 19:11:35 - Migração S4 - DL
*        SET PARAMETER ID 'MBN'  FIELD wa_saida_0120_02-doc_prod_01.
*        SET PARAMETER ID 'MJA'  FIELD wa_saida_0120_02-ano_doc_prod_01.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = wa_saida_0120_02-doc_prod_01
            i_mjahr             = wa_saida_0120_02-ano_doc_prod_01.
*<--- 19.07.2023 19:11:35 - Migração S4 - DL

      WHEN 'DOC_PROD_02'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-doc_prod_02 IS NOT INITIAL ).

*---> 19.07.2023 19:11:35 - Migração S4 - DL
*        SET PARAMETER ID 'MBN'  FIELD wa_saida_0120_02-doc_prod_02.
*        SET PARAMETER ID 'MJA'  FIELD wa_saida_0120_02-ano_doc_prod_02.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = wa_saida_0120_02-doc_prod_02
            i_mjahr             = wa_saida_0120_02-ano_doc_prod_02.
*<--- 19.07.2023 19:11:35 - Migração S4 - DL

      WHEN 'DOC_PROD_03'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-doc_prod_03 IS NOT INITIAL ).

*---> 19.07.2023 19:11:35 - Migração S4 - DL
*        SET PARAMETER ID 'MBN'  FIELD wa_saida_0120_02-doc_prod_03.
*        SET PARAMETER ID 'MJA'  FIELD wa_saida_0120_02-ano_doc_prod_03.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = wa_saida_0120_02-doc_prod_03
            i_mjahr             = wa_saida_0120_02-ano_doc_prod_03.
*<--- 19.07.2023 19:11:35 - Migração S4 - DL

      WHEN 'DOC_PROD_04'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-doc_prod_04 IS NOT INITIAL ).


*---> 19.07.2023 19:10:15 - Migração S4 - DL
*        SET PARAMETER ID 'MBN'  FIELD wa_saida_0120_02-doc_prod_04.
*        SET PARAMETER ID 'MJA'  FIELD wa_saida_0120_02-ano_doc_prod_04.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = wa_saida_0120_02-doc_prod_04
            i_mjahr             = wa_saida_0120_02-ano_doc_prod_04.
*<--- 19.07.2023 19:10:15 - Migração S4 - DL

      WHEN 'DOC_PROD_05'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-doc_prod_05 IS NOT INITIAL ).

*---> 19.07.2023 19:11:35 - Migração S4 - DL
*        SET PARAMETER ID 'MBN'  FIELD wa_saida_0120_02-doc_prod_05.
*        SET PARAMETER ID 'MJA'  FIELD wa_saida_0120_02-ano_doc_prod_05.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = wa_saida_0120_02-doc_prod_05
            i_mjahr             = wa_saida_0120_02-ano_doc_prod_05.
*<--- 19.07.2023 19:11:35 - Migração S4 - DL

*-----------------------------------------------------------------------------------------------------------------------*
*     NF RFL - 01 02 03
*-----------------------------------------------------------------------------------------------------------------------*

      WHEN 'SEQLCTO_RFL_01'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rfl_01 IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_rfl_01.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_RFL_01'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_rfl_01 IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_rfl_01.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_RFL_01'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rfl_01 IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_rfl_01,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.

      WHEN 'SEQLCTO_RFL_02'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rfl_02 IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_rfl_02.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_RFL_02'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_rfl_02 IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_rfl_02.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_RFL_02'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rfl_02 IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_rfl_02,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.

      WHEN 'SEQLCTO_RFL_03'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rfl_03 IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_rfl_03.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_RFL_03'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_rfl_03 IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_rfl_03.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_RFL_03'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rfl_03 IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_rfl_03,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.

*-----------------------------------------------------------------------------------------------------------------------*
*     NF Rem. Conta e Ordem 01 - Saida e Entrada
*-----------------------------------------------------------------------------------------------------------------------*

      WHEN 'SEQLCTO_RCO_01'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rco_01 IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_rco_01.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_RCO_01'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_rco_01 IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_rco_01.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_RCO_01'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rco_01 IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_rco_01,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.


      WHEN 'SEQLCTO_ENT_RCO_01'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ent_rco_01 IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_ent_rco_01.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_ENT_RCO_01'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_ent_rco_01 IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_ent_rco_01.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_ENT_RCO_01'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ent_rco_01 IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_ent_rco_01,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.

*-----------------------------------------------------------------------------------------------------------------------*
*     NF Rem. Conta e Ordem 02 - Saida e Entrada
*-----------------------------------------------------------------------------------------------------------------------*

      WHEN 'SEQLCTO_RCO_02'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rco_02 IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_rco_02.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_RCO_02'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_rco_02 IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_rco_02.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_RCO_02'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rco_02 IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_rco_02,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.


      WHEN 'SEQLCTO_ENT_RCO_02'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ent_rco_02 IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_ent_rco_02.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_ENT_RCO_02'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_ent_rco_02 IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_ent_rco_02.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_ENT_RCO_02'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ent_rco_02 IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_ent_rco_02,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.

*-----------------------------------------------------------------------------------------------------------------------*
*     NF Rem. Conta e Ordem 03 - Saida e Entrada
*-----------------------------------------------------------------------------------------------------------------------*

      WHEN 'SEQLCTO_RCO_03'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rco_03 IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_rco_03.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_RCO_03'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_rco_03 IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_rco_03.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_RCO_03'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_rco_03 IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_rco_03,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.


      WHEN 'SEQLCTO_ENT_RCO_03'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ent_rco_03 IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0120_02-seqlcto_ent_rco_03.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_ENT_RCO_03'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-docnum_ent_rco_03 IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0120_02-docnum_ent_rco_03.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'NFENUM_ENT_RCO_03'.

        READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0120_02-seqlcto_ent_rco_03 IS NOT INITIAL ).

        REFRESH: tl_bdc.
        PERFORM f_preencher_dynpro USING:
                 'X' 'ZWRR0004'              '0100',
                 ' ' 'P_SEQ_LCTO'            wa_saida_0120_02-seqlcto_ent_rco_03,
                 ' ' 'BDC_OKCODE'            'SEARCH'.

        opt-dismode = 'E'.
        opt-defsize = ' '.
        opt-racommit = 'X'.

        CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.


    ENDCASE.


    CASE e_column_id.

      WHEN 'SEQLCTO_DEVOL'        OR
           'NFENUM_DEVOL'         OR
           'SEQLCTO_ENT_DEV'      OR
           'NFENUM_ENT_DEV'       OR

           'SEQLCTO_IND'          OR
           'NFENUM_IND'           OR
           'SEQLCTO_ENT_IND'      OR
           'NFENUM_ENT_IND'       OR

           'SEQLCTO_RFL_01'       OR
           'NFENUM_RFL_01'        OR

           'SEQLCTO_RFL_02'       OR
           'NFENUM_RFL_02'        OR

           'SEQLCTO_RFL_03'       OR
           'NFENUM_RFL_03'        OR

           'SEQLCTO_RCO_01'       OR
           'NFENUM_RCO_01'        OR
           'SEQLCTO_ENT_RCO_01'   OR
           'NFENUM_ENT_RCO_01'    OR

           'SEQLCTO_RCO_02'       OR
           'NFENUM_RCO_02'        OR
           'SEQLCTO_ENT_RCO_02'   OR
           'NFENUM_ENT_RCO_02'    OR

           'SEQLCTO_RCO_03'       OR
           'NFENUM_RCO_03'        OR
           'SEQLCTO_ENT_RCO_03'   OR
           'NFENUM_ENT_RCO_03'    .


        PERFORM: f_load_notas_vinc,
                 f_refresh_alv USING '0120_02'.

    ENDCASE.




  ENDMETHOD.

  METHOD on_onf4.
  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION


CLASS lcl_alv_toolbar_0121 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

*    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_handler_0121 IMPLEMENTATION.

  METHOD on_data_changed_finished.

  ENDMETHOD.                    "on_data_changed_finishe

  METHOD catch_hotspot.

    CASE e_column_id.

      WHEN 'DOCNUM'.

        READ TABLE it_saida_0121 INTO wa_saida_0121 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0121-docnum IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0121-docnum.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.


    ENDCASE.


  ENDMETHOD.

  METHOD on_onf4.
  ENDMETHOD.

ENDCLASS.
