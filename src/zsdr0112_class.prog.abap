*&---------------------------------------------------------------------*
*&  Include           ZSDR0112_CLASS
*&---------------------------------------------------------------------*



CLASS lcl_alv_toolbar_0100 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: tl_parametros TYPE ustyp_t_parameters.

    REFRESH: tl_parametros.

    CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
      EXPORTING
        user_name           = sy-uname
      TABLES
        user_parameters     = tl_parametros
      EXCEPTIONS
        user_name_not_exist = 1
        OTHERS              = 2.


    ty_toolbar-icon      = icon_icon_list.
    ty_toolbar-function  = c_nfs_sel.
    ty_toolbar-text      = 'NFs Selecionadas'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_set_b.
    ty_toolbar-function  = c_atrib_qtd.
    ty_toolbar-text      = 'Atribuir Quantidade'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_generate.
    ty_toolbar-function  = c_gerar_lcto.
    ty_toolbar-text      = 'Gerar Lançamento'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_reject.
    ty_toolbar-function  = c_gerar_estorno.
    ty_toolbar-text      = 'Estornar Lançamento'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_create_text.
    ty_toolbar-function  = c_gerar_sefaz.
    ty_toolbar-text      = 'Enviar SEFAZ'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer       TYPE c,
          vl_refresh_dados TYPE c.


    CASE e_ucomm.
      WHEN c_gerar_sefaz.
        PERFORM f_gerar_sefaz.

      WHEN c_gerar_lcto.
        PERFORM f_gerar_lcto.
        vl_refresh_dados = abap_true.

      WHEN c_gerar_estorno.
        PERFORM f_estornar_retorno.
        vl_refresh_dados = abap_true.

      WHEN c_nfs_sel.
        PERFORM f_count_nf_selecionadas.
      WHEN c_atrib_qtd.
        CALL METHOD obj_alv_0100->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        PERFORM f_verifica_linha TABLES it_sel_rows USING vl_status_line.

        IF it_sel_rows[] IS NOT INITIAL AND vl_status_line IS INITIAL.

          CALL SCREEN 0101 STARTING AT 05 05.

          CALL METHOD obj_alv_0100->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.

          IF lines( it_row ) > 0.
            CALL METHOD obj_alv_0100->set_selected_rows
              EXPORTING
                it_index_rows = it_row.
          ENDIF.

        ELSEIF it_sel_rows[] IS NOT INITIAL AND vl_status_line IS NOT INITIAL.
          MESSAGE |selecionar somente linhas sem Doc de Retorno!| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ELSE.
          MESSAGE | Favor selecione pelo menos uma linha!| TYPE 'S' DISPLAY LIKE 'E' .
          EXIT.
        ENDIF.
    ENDCASE.

    IF ( vl_refresh_dados EQ abap_true ).
      PERFORM: f_selecionar_dados USING abap_false,
               f_processa_dados,
               f_call_alv.
    ENDIF.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler_0100 IMPLEMENTATION.

  METHOD catch_hotspot.

    CASE e_column_id.
*      WHEN 'DOCNUM_RETORNO'. "docnum_retorno
      WHEN 'DOCNUM_RET_FLAG'. "docnum_retorno

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-docnum_retorno IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0100-docnum_retorno.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

*      WHEN 'NFERET_QUEBRA'.
      WHEN 'NFERET_FLAG'.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-docnum_retorno IS NOT INITIAL ).

        SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida_0100-docnum_retorno.
        SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_saida_0100-bukrs.

        CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.

*      WHEN 'NFENUM_ZNFW'.
      WHEN 'NFENUM_FLAG'.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-docnum_znfw IS NOT INITIAL ).

        SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida_0100-docnum_znfw.
        SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_saida_0100-bukrs.

        CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.

*      WHEN 'SEQ_LCTO_ZNFW'.
      WHEN 'SEQ_LCTO_FLAG'.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-seq_lcto_znfw IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD  wa_saida_0100-seq_lcto_znfw.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

*      WHEN 'DOCNUM_ZNFW'.
      WHEN 'DOCNUM_FLAG'.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.
        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-docnum_znfw IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD wa_saida_0100-docnum_znfw.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.


*      WHEN  'MBLNR_ZNFW'.
      WHEN  'MBLNR_ZNFW_FLAG'.
        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-mblnr_znfw IS NOT INITIAL ).


* ---> S4 Migration - 19/07/2023 - DG
*        SET PARAMETER ID 'MBN' FIELD wa_saida_0100-mblnr_znfw.
*        SET PARAMETER ID 'MJA' FIELD wa_saida_0100-docdat_znfw+0(4).
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
            i_mblnr             = wa_saida_0100-mblnr_znfw
            i_mjahr             = wa_saida_0100-docdat_znfw+0(4).
        "I_ZEILE = I_FINAL-ZEILE.
* <--- S4 Migration - 19/07/2023 - DG3 - DG

      WHEN  'MBLNR_CCE'.
        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-mblnr_cce IS NOT INITIAL ).


* ---> S4 Migration - 19/07/2023 - DG
*        SET PARAMETER ID 'MBN' FIELD wa_saida_0100-mblnr_cce.
*        SET PARAMETER ID 'MJA' FIELD wa_saida_0100-mjahr_cce.
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
            i_mblnr             = wa_saida_0100-mblnr_cce
            i_mjahr             = wa_saida_0100-mjahr_cce.
        "I_ZEILE = I_FINAL-ZEILE.
* <--- S4 Migration - 19/07/2023 - DG3 - DG

      WHEN  'MBLNR_FLAG'.
        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.

        CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-mblnr IS NOT INITIAL ).


* ---> S4 Migration - 19/07/2023 - DG
*        SET PARAMETER ID 'MBN' FIELD wa_saida_0100-mblnr.
*        SET PARAMETER ID 'MJA' FIELD wa_saida_0100-mjahr.
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
            i_mblnr             = wa_saida_0100-mblnr
            i_mjahr             = wa_saida_0100-mjahr.
        "I_ZEILE = I_FINAL-ZEILE.
* <--- S4 Migration - 19/07/2023 - DG3 - DG

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD: on_finished.

  ENDMETHOD.                           " ON_FINISHED
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
