*&---------------------------------------------------------------------*
*&  Include           ZLESR0107_CLASS
*&---------------------------------------------------------------------*

CLASS LCL_ALV_TOOLBAR_0100 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

*    TY_TOOLBAR-ICON      = ICON_CREATE.
*    TY_TOOLBAR-FUNCTION  = 'NOVO'.
*    TY_TOOLBAR-TEXT      = 'Novo'.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

    READ TABLE TG_PARAMETROS INTO DATA(WL_PARAMETROS) WITH KEY PARID = 'ZLES0144_CAD_LIMITE'.
    IF SY-SUBRC EQ 0.
      TY_TOOLBAR-ICON      = ICON_CHANGE.
      TY_TOOLBAR-FUNCTION  = 'CHANGE'.
      TY_TOOLBAR-TEXT      = 'Modificar'.
      TY_TOOLBAR-BUTN_TYPE = 0.
      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR TY_TOOLBAR.

      TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
      TY_TOOLBAR-FUNCTION  = 'DEL'.
      TY_TOOLBAR-TEXT      = 'Deletar'.
      TY_TOOLBAR-BUTN_TYPE = 0.
      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR TY_TOOLBAR.

    ENDIF.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    DATA: V_DATA_OUT TYPE C LENGTH 15.
    DATA: _ERROR TYPE CHAR1.

    CASE E_UCOMM.
      WHEN 'DEL'.
        CLEAR: WA_SAIDA_0100.

        CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

        CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = IT_SEL_ROWS.

        IF IT_SEL_ROWS[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TITLEBAR              = 'Confirmação'
            TEXT_QUESTION         = 'Deseja realmente deletar o(s) registro(s) selecionado(s)?'
            TEXT_BUTTON_1         = 'Sim'
            TEXT_BUTTON_2         = 'Não'
            DEFAULT_BUTTON        = '1'
            DISPLAY_CANCEL_BUTTON = ''
          IMPORTING
            ANSWER                = VAR_ANSWER
          EXCEPTIONS
            TEXT_NOT_FOUND        = 1
            OTHERS                = 2.

        CHECK VAR_ANSWER EQ '1'.

        LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

          READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.

          CHECK SY-SUBRC = 0.

          V_DATA_OUT = WA_SAIDA_0100-DATA+6(2) && '/' && WA_SAIDA_0100-DATA+4(2) && '/' && WA_SAIDA_0100-DATA(4).

          SELECT SINGLE *
            FROM ZLEST0141 INTO @DATA(_WL_0141_CTB)
           WHERE BUKRS EQ @WA_SAIDA_0100-BUKRS
             AND DATA  EQ @WA_SAIDA_0100-DATA.

          IF SY-SUBRC EQ 0.
            IF ( _WL_0141_CTB-LOTE IS NOT INITIAL ) OR ( _WL_0141_CTB-SEM_SLD_PGTO IS NOT INITIAL ).
              ROLLBACK WORK.
              MESSAGE | Processamento já realizado na data: { V_DATA_OUT } | TYPE 'I'.
              RETURN.
            ENDIF.
          ENDIF.

          "Para dias não uteis, checar se já houve processamento no ultimo dia anterior Útil.
          DATA(_PROC) = ABAP_FALSE.
          PERFORM F_CHECK_PROC_DIA_ANTERIOR_UTIL USING WA_SAIDA_0100-BUKRS
                                                       WA_SAIDA_0100-DATA
                                              CHANGING _PROC.
          IF _PROC EQ ABAP_TRUE.
            ROLLBACK WORK.
            RETURN.
          ENDIF.

***    Pegar o valor da saida e verificar se houve alteração valor adiantamento com base na data e empresa.
          READ TABLE T_TEMP INTO DATA(W_TEMP) WITH KEY BUKRS = WA_SAIDA_0100-BUKRS
                                                                      DATA  = WA_SAIDA_0100-DATA.

          CLEAR: W_ZLEST0140.
          W_ZLEST0140 = VALUE #( BUKRS      = WA_SAIDA_0100-BUKRS
                                 DATA       = WA_SAIDA_0100-DATA
                                 VALOR_DE   = W_TEMP-VALOR
                                 VALOR_PARA = WA_SAIDA_0100-VALOR
*                                   TIP_OPER   = 'Delete'
                                  ).

          TXTOPEN = ABAP_TRUE.
          CALL SCREEN 0300 STARTING AT 05 15 .

          CLEAR WA_EDITOR.
          CALL METHOD EDITOR->GET_TEXT_AS_STREAM( IMPORTING TEXT = IT_EDITOR ).
          IF IT_EDITOR IS INITIAL.

            _ERROR = ABAP_TRUE.
            CONTINUE.
          ELSE.

            CLEAR: W_ZLEST0140-OBS.
            LOOP AT IT_EDITOR INTO WA_EDITOR.
              W_ZLEST0140-OBS = |{ W_ZLEST0140-OBS } { WA_EDITOR-LINE }|.
            ENDLOOP.
          ENDIF.

          W_LOG = VALUE #( BUKRS      = WA_SAIDA_0100-BUKRS
                          TIPO_OPER   = 'Delete'
                          DATA_MODIF  = SY-DATUM
                          HORA_MODIF  = SY-UZEIT
                          USUARIO     = SY-UNAME
                          DATA        = WA_SAIDA_0100-DATA
                          VALOR_ANT   = W_TEMP-VALOR
                          VALOR_ATUAL = WA_SAIDA_0100-VALOR
                          MOTIVO = W_ZLEST0140-OBS ).

          APPEND W_LOG TO T_LOG.

          MODIFY ZLEST0198 FROM W_LOG.
          COMMIT WORK.
          CLEAR: W_LOG, W_TEMP.
*          ENDIF.

***************************************************************************************************
          IF _ERROR IS INITIAL.
            DELETE FROM ZLEST0140 WHERE BUKRS = WA_SAIDA_0100-BUKRS
                                    AND DATA  = WA_SAIDA_0100-DATA.
          ELSE.
            MESSAGE 'Preenchear a justificativa!' TYPE 'S'.
          ENDIF.
          CLEAR: WA_SAIDA_0100.
        ENDLOOP.

        MESSAGE 'Registros deletados com sucesso!' TYPE 'S'.

        PERFORM: F_SELECIONAR_DADOS,
                 F_PROCESSAR_DADOS.

        LEAVE TO SCREEN 0100.

      WHEN 'CHANGE'.
        IF ( OBJ_ALV_0100 IS NOT INITIAL ) AND ( VG_OPERACAO NE C_EDIT ).
          CALL METHOD OBJ_ALV_0100->FREE.
          CALL METHOD CL_GUI_CFW=>FLUSH.
          FREE: OBJ_ALV_0100.
        ENDIF.

        VG_OPERACAO = C_EDIT.

*****        Guardar as informações da saida numa tabela temporaria para comparar caso as inforamções sejam alteradas.
        FREE: T_TEMP.
        MOVE-CORRESPONDING IT_SAIDA_0100 TO T_TEMP.

        PERFORM: F_SELECIONAR_DADOS,
                 F_PROCESSAR_DADOS.

        LEAVE TO SCREEN 0100.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS LCL_EVENT_HANDLER_0100 IMPLEMENTATION.

  METHOD HANDLE_BUTTON_CLICK .
  ENDMETHOD.

  METHOD CATCH_HOTSPOT.

    DATA: VL_OBJ_KEY    TYPE SIBFLPORB-INSTID,
          VL_LINES      TYPE I,
          ANEXOS        TYPE TABLE OF BDN_CON,
          VL_IP_MODE    TYPE SGS_RWMOD,
          VL_IP_SERVICE TYPE SGS_SRVNAM,
          WA_BOR        TYPE BORIDENT,
          ANEXO_OBJ     TYPE REF TO CL_GOS_MANAGER.

    DATA: VL_DISPLAY_MODE TYPE XFELD,
          WL_HEADER       TYPE THEAD,
          WL_NAME         TYPE THEAD-TDNAME,
          IT_TEXTO        TYPE STANDARD TABLE OF TLINE,
          WA_TEXTO        TYPE TLINE,
          TL_TEXTO        TYPE CATSXT_LONGTEXT_ITAB,
          WL_TEXTO        TYPE LINE OF CATSXT_LONGTEXT_ITAB.

    READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX E_ROW_ID-INDEX.

    CHECK SY-SUBRC = 0.

    CASE E_COLUMN_ID.
      WHEN 'ANEXO' OR 'ANEXO_G'.

        IF VG_OPERACAO EQ C_EDIT.
          MESSAGE 'Vinculação não permitida em modo de edição!' TYPE 'S'.
          EXIT.
        ENDIF.

        CREATE OBJECT ANEXO_OBJ TYPE CL_GOS_MANAGER.

        IF E_COLUMN_ID = 'ANEXO_G'.
          VL_IP_MODE  = 'R'. "Ready.
          VL_IP_SERVICE = 'VIEW_ATTA'.
          WA_BOR-OBJTYPE = 'ZLES0144-G'.
          WA_BOR-OBJKEY  = WA_SAIDA_0100-ANEXO_LINK_G.
          VL_OBJ_KEY     = WA_SAIDA_0100-ANEXO_LINK_G.
        ELSE.
          VL_IP_MODE  = 'E'.
          IF WA_SAIDA_0100-ANEXO EQ '@1F@'.
            VL_IP_SERVICE = 'PCATTA_CREA'.
          ELSE.
            VL_IP_SERVICE = 'VIEW_ATTA'.
          ENDIF.
          WA_BOR-OBJTYPE = 'ZLES0144'.
          WA_BOR-OBJKEY  = WA_SAIDA_0100-ANEXO_LINK.
          VL_OBJ_KEY     = WA_SAIDA_0100-ANEXO_LINK.
        ENDIF.

        ANEXO_OBJ->SET_RW_MODE( IP_MODE = VL_IP_MODE ).

        ANEXO_OBJ->START_SERVICE_DIRECT(
          EXPORTING
            IP_SERVICE         = VL_IP_SERVICE
            IS_OBJECT          = WA_BOR
          EXCEPTIONS
            NO_OBJECT          = 1
            OBJECT_INVALID     = 2
            EXECUTION_FAILED   = 3
            OTHERS             = 4 ).

        COMMIT WORK.

*        "Contando o número de anexos p/ atualizar ícone
*        CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
*          EXPORTING
*            CLASSNAME          = 'ZLES0144'
*            OBJKEY             = VL_OBJ_KEY
*            CLIENT             = SY-MANDT
*          TABLES
*            GOS_CONNECTIONS    = ANEXOS
*          EXCEPTIONS
*            NO_OBJECTS_FOUND   = 1
*            INTERNAL_ERROR     = 2
*            INTERNAL_GOS_ERROR = 3
*            OTHERS             = 4.
*
*        IF LINES( ANEXOS[] ) > 0.
*          WA_SAIDA_0100-ANEXO = '@1E@'.
*        ELSE.
*          WA_SAIDA_0100-ANEXO = '@1F@'.
*        ENDIF.
*
*        MODIFY IT_SAIDA_0100 FROM WA_SAIDA_0100 INDEX ES_ROW_NO-ROW_ID TRANSPORTING ANEXO .
*        CLEAR: WA_BOR, VL_OBJ_KEY, VL_LINES, ANEXOS.

        PERFORM: F_SELECIONAR_DADOS,
                 F_PROCESSAR_DADOS.
        LEAVE TO SCREEN 0100.

    ENDCASE.

  ENDMETHOD.



ENDCLASS.
