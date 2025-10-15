*&---------------------------------------------------------------------*
*&  Include           ZMMR123_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0100 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    ty_toolbar-icon      = icon_create.
    ty_toolbar-function  = 'NOVO'.
    ty_toolbar-text      = 'Novo'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_change.
    ty_toolbar-function  = 'CHANGE'.
    ty_toolbar-text      = 'Modificar'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN c_novo.

        vg_operacao = e_ucomm.

        CLEAR: wa_saida_0110.

        wa_saida_0110-dt_registro = sy-datum.
        wa_saida_0110-hr_registro = sy-uzeit.
        wa_saida_0110-usuario     = sy-uname.

        CALL SCREEN 0110 STARTING AT 10 05 ENDING AT 130 11 .

        PERFORM: f_selecionar_dados,
                 f_processa_dados.

        LEAVE TO SCREEN 0100.

      WHEN c_change.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0100->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF lines( it_sel_rows ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.

        CHECK sy-subrc = 0.

        IF wa_saida_0100-st_envio_opus = '1'. "Enviado
          MESSAGE 'Registro em processamento' TYPE 'S'.
          EXIT.
        ENDIF.

        CLEAR: wa_saida_0110, wa_saida_0110_aux.

        MOVE-CORRESPONDING wa_saida_0100 TO wa_saida_0110.
        MOVE-CORRESPONDING wa_saida_0100 TO wa_saida_0110_aux.

        vg_operacao = e_ucomm.

        CALL SCREEN 0110 STARTING AT 10 05 ENDING AT 130 11 .

        PERFORM: f_selecionar_dados,
                 f_processa_dados.

        LEAVE TO SCREEN 0100.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_alv_toolbar_0110 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
*
*    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
*    TY_TOOLBAR-FUNCTION  = 'INSR_ROW'.
*    TY_TOOLBAR-TEXT      = ''.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
*
*    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
*    TY_TOOLBAR-FUNCTION  = 'DEL_ROW'.
*    TY_TOOLBAR-TEXT      = ''.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
*
*    TY_TOOLBAR-ICON      = ICON_SYSTEM_MARK.
*    TY_TOOLBAR-FUNCTION  = 'MARK'.
*    TY_TOOLBAR-TEXT      = 'Marcar Selecionados'.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
*
*    TY_TOOLBAR-ICON      = ICON_DESELECT_ALL.
*    TY_TOOLBAR-FUNCTION  = 'DESMARK'.
*    TY_TOOLBAR-TEXT      = 'Desmarcar Selecionados'.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.


*    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

*    CASE E_UCOMM.
*      WHEN 'INSR_ROW'.
*        CLEAR: WA_SAIDA_0110, GT_ESTILO[].
*        WA_SAIDA_0110-MANUAL = 'X'.
*        WA_SAIDA_0110-IC_MANUAL = ICON_CHECKED.
*        WA_SAIDA_0110-KURSF  = WA_SAIDA_0100-KURSF.
*        WA_SAIDA_0110-KOART  = WA_SAIDA_0100-KOART.
*
*        WL_ESTILO-FIELDNAME    = 'VLR_RSD'.
*        WL_ESTILO-STYLE        = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*        APPEND WL_ESTILO TO GT_ESTILO.
*
*        INSERT LINES OF GT_ESTILO INTO TABLE WA_SAIDA_0110-ESTILO.
*
*        APPEND WA_SAIDA_0110 TO IT_SAIDA_0110.
*        LEAVE TO SCREEN 0110.
*      WHEN 'DEL_ROW'.
*
*       CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.
*
*       CALL METHOD OBJ_ALV_0110->GET_SELECTED_ROWS
*         IMPORTING
*           ET_INDEX_ROWS = IT_SEL_ROWS.
*
*       CHECK IT_SEL_ROWS[] IS NOT INITIAL.
*       SORT IT_SEL_ROWS BY INDEX DESCENDING.
*
*       LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.
*         READ TABLE IT_SAIDA_0110 INTO WA_SAIDA_0110 INDEX WA_SEL_ROWS-INDEX.
*         IF SY-SUBRC NE 0 .
*           RETURN.
*         ENDIF.
*
*         IF WA_SAIDA_0110-MANUAL IS INITIAL.
*           MESSAGE 'Só é possivel deletar as partidas manuais!' TYPE 'S'.
*           RETURN.
*         ENDIF.
*       ENDLOOP.
*
*       LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.
*         DELETE IT_SAIDA_0110 INDEX WA_SEL_ROWS-INDEX.
*       ENDLOOP.
*
*       LEAVE TO SCREEN 0110.
*
*      WHEN 'DESMARK'.
*        PERFORM F_FLAG_DOCUMENTOS USING ''.
*      WHEN 'MARK'.
*        PERFORM F_FLAG_DOCUMENTOS USING 'X'.
*      WHEN 'REFRESH'.
*        PERFORM F_ATUALIZA_SALDO.
*        LEAVE TO SCREEN 0110.
*    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_handler_0100 IMPLEMENTATION.

  METHOD catch_hotspot.

    CASE e_column_id.
      WHEN 'EBELN'.
        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.
        CHECK sy-subrc = 0.

        IF wa_saida_0100-ebeln  IS NOT INITIAL.
          SET PARAMETER ID 'BES' FIELD wa_saida_0100-ebeln.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'ENVIAR_OPUS'.
        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX e_row_id-index.
        CHECK sy-subrc = 0.

        CHECK ( wa_saida_0100-ebeln IS NOT INITIAL ) AND
              ( wa_saida_0100-ebelp IS NOT INITIAL ).
* Ajuste US 103763 - Criar API para Envio Sobra Material para OPUS - BG - inicio

*        UPDATE ZMMT0086 SET ST_ENVIO_OPUS = '1'
*         WHERE EBELN = WA_SAIDA_0100-EBELN
*           AND EBELP = WA_SAIDA_0100-EBELP.


        SELECT SINGLE * FROM zmmt0086 INTO @DATA(w_zmmt0086)
        WHERE ebeln = @wa_saida_0100-ebeln
          AND ebelp = @wa_saida_0100-ebelp.

        TRY.
            zcl_int_ob_sobra_material_opus=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = w_zmmt0086 ).
          CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
            MESSAGE 'Houve erro ao enviar para o OPUS!' TYPE 'S'.
            RETURN.
          CATCH zcx_error INTO DATA(zcx_error).
            MESSAGE 'Houve erro ao enviar para o OPUS!' TYPE 'S'.
            RETURN.
        ENDTRY.
        IF sy-subrc = 0.
          MESSAGE 'Envio realizado com sucesso!' TYPE 'S'.

          " MESSAGE 'Registro processado com sucesso!' TYPE 'S'.
* Ajuste US 103763 - Criar API para Envio Sobra Material para OPUS - BG - inicio
          PERFORM: f_selecionar_dados,
                   f_processa_dados.

          LEAVE TO SCREEN 0100.
        ELSE.
          MESSAGE 'Houve um erro no processamento!' TYPE 'S'.
          EXIT.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
