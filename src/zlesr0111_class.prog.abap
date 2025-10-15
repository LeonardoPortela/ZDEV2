*&---------------------------------------------------------------------*
*&  Include           ZLESR0111_CLASS
*&---------------------------------------------------------------------*

CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
   METHOD: ON_FINISHED.

     PERFORM F_ATUALIZA_TIME.

     IF ( VG_TST_LIM > VG_TST_ATUAL ).
       CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
         EXPORTING
           NEW_CODE = 'CLOCK'.

       GO_CLOCK->INTERVAL = VG_TIME_INTERVAL.
       CALL METHOD GO_CLOCK->RUN.
     ELSE.
       PERFORM F_LOGOUT.
     ENDIF.

   ENDMETHOD.                           " ON_FINISHED
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

CLASS LCL_ALV_TOOLBAR_0100 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    DATA: TL_PARAMETROS TYPE USTYP_T_PARAMETERS,
          VL_TXT_BUTTON TYPE TEXT40,
          V_TIME        TYPE T.

    REFRESH: TL_PARAMETROS.

    CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
      EXPORTING
        USER_NAME           = SY-UNAME
      TABLES
        USER_PARAMETERS     = TL_PARAMETROS
      EXCEPTIONS
        USER_NAME_NOT_EXIST = 1
        OTHERS              = 2.

*    SELECT SINGLE *
*      FROM SETLEAF INTO @DATA(WL_0147)
*     WHERE SETNAME = 'ZLEST0147_US_WS'
*       AND VALFROM = @SY-UNAME.
*    IF SY-SUBRC = 0.
*      TY_TOOLBAR-ICON      = ICON_DISCONNECT.
*      TY_TOOLBAR-FUNCTION  = 'CALL_WEB'.
*      TY_TOOLBAR-TEXT      = 'Call WebService'.
*      TY_TOOLBAR-BUTN_TYPE = 0.
*      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*      CLEAR TY_TOOLBAR.
*    ENDIF.

*----------------------------------------------------------------------------------------------------------------*
*   Configurar Botão Autenticação
*----------------------------------------------------------------------------------------------------------------*
    DATA(_AUTENTICAR) = ABAP_TRUE.
    CASE ABAP_TRUE.
      WHEN P_TP_RCC. "Recepcionar Carga
        CASE ABAP_TRUE.
          WHEN P_VW_NF.  "Notas Fiscais
          WHEN P_RC_NF . "Recepções de Carga por NF
          WHEN P_VW_DUE. "DU-e's
          WHEN P_RC_DUE. "Recepções de Carga por DU-e
          WHEN P_EC_DUE. "Entregas de Carga por DU-e
        ENDCASE.
      WHEN P_TP_ECG. "Entregar Carga

        CASE ABAP_TRUE.
          WHEN P_VW_NF.  "Notas Fiscais
          WHEN P_RC_NF . "Recepções de Carga por NF
          WHEN P_VW_DUE. "DU-e's
            _AUTENTICAR = ABAP_FALSE.
          WHEN P_RC_DUE. "Recepções de Carga por DU-e
          WHEN P_EC_DUE. "Entregas de Carga por DU-e
        ENDCASE.

    ENDCASE.

    IF _AUTENTICAR EQ ABAP_TRUE.
      IF VG_ST_LOGON = C_DISCONNECTED.
        TY_TOOLBAR-ICON      = ICON_DISCONNECT.
        TY_TOOLBAR-FUNCTION  = C_LOGIN.
        TY_TOOLBAR-TEXT      = 'Conectar'.
        TY_TOOLBAR-BUTN_TYPE = 0.
        APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
        CLEAR TY_TOOLBAR.
      ELSE.
        TY_TOOLBAR-ICON      = ICON_CONNECT.
        TY_TOOLBAR-FUNCTION  = C_LOGOUT.

        IF VG_TIME_LIM(2) = '23'.
          VG_TIME_LIM = 1.
        ENDIF.

        CONCATENATE VG_TIME_LIM(2) ':' VG_TIME_LIM+2(2) ':' VG_TIME_LIM+4(2) INTO DATA(_TIME_DESC).
        CONCATENATE 'Desconectar em ' _TIME_DESC 'seg.'
               INTO VL_TXT_BUTTON SEPARATED BY SPACE.

        TY_TOOLBAR-TEXT      = VL_TXT_BUTTON.
        TY_TOOLBAR-BUTN_TYPE = 0.
        APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
        CLEAR TY_TOOLBAR.
      ENDIF.
    ENDIF.
*----------------------------------------------------------------------------------------------------------------*
*   Visões
*----------------------------------------------------------------------------------------------------------------*
    CASE ABAP_TRUE.
      WHEN P_TP_RCC. "Recepcionar Carga

        CASE ABAP_TRUE.
          WHEN P_VW_NF. "Notas Fiscais

            TY_TOOLBAR-ICON      = ICON_GENERATE.
            TY_TOOLBAR-FUNCTION  = C_RECEPCIONAR_CARGA.
            TY_TOOLBAR-TEXT      = 'Recepcionar Carga'.
            TY_TOOLBAR-BUTN_TYPE = 0.
            APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
            CLEAR TY_TOOLBAR.

            TY_TOOLBAR-ICON      = ICON_SYSTEM_UNDO.
            TY_TOOLBAR-FUNCTION  = C_DISP_NFE_AJUSTE.
            TY_TOOLBAR-TEXT      = 'Lib. NF-e p/ Ajuste'.
            TY_TOOLBAR-BUTN_TYPE = 0.
            APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
            CLEAR TY_TOOLBAR.

            TY_TOOLBAR-ICON      = ICON_SET_STATE.
            TY_TOOLBAR-FUNCTION  = C_DEF_CNPJ_CPF_TRANSP.
            TY_TOOLBAR-TEXT      = 'Definir CNPJ/CPF Transportador'.
            TY_TOOLBAR-BUTN_TYPE = 0.
            APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
            CLEAR TY_TOOLBAR.

            SELECT SINGLE *
              FROM SETLEAF INTO @DATA(WL_0147)
             WHERE SETNAME = 'ZLES0147_REPROC_ROM'
               AND VALFROM = @SY-UNAME.
            IF SY-SUBRC = 0.
              TY_TOOLBAR-ICON      = ICON_ACTIVITY.
              TY_TOOLBAR-FUNCTION  = C_REPROC_ROM.
              TY_TOOLBAR-TEXT      = 'Reprocessar Romaneios'.
              TY_TOOLBAR-BUTN_TYPE = 0.
              APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
              CLEAR TY_TOOLBAR.
            ENDIF.

            READ TABLE TL_PARAMETROS INTO DATA(WL_PARAMETROS) WITH KEY PARID = 'ZCCT_LIB_CARGA_PARC'.
            IF SY-SUBRC EQ 0.
              TY_TOOLBAR-ICON      = ICON_STATUS_BOOKED.
              TY_TOOLBAR-FUNCTION  = C_LIB_ENVIO_CARGA_PARC.
              TY_TOOLBAR-TEXT      = 'Lib.Carga.Parc.'.
              TY_TOOLBAR-BUTN_TYPE = 0.
              APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
              CLEAR TY_TOOLBAR.
            ENDIF.


          WHEN P_RC_NF. "Recepções de Carga por NF

            SELECT SINGLE *
              FROM SETLEAF INTO @DATA(WL_SETLEAF)
             WHERE SETNAME = 'ZLES0147_DOWN_XML_RC'
               AND VALFROM = @SY-UNAME.

            IF SY-SUBRC = 0.
              TY_TOOLBAR-ICON      = ICON_PAGE_DOWN.
              TY_TOOLBAR-FUNCTION  = C_DOWN_XML_RC_NFE.
              TY_TOOLBAR-TEXT      = 'Download XML Recepção Carga'.
              TY_TOOLBAR-BUTN_TYPE = 0.
              APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
              CLEAR TY_TOOLBAR.
            ENDIF.

*            TY_TOOLBAR-ICON      = ICON_OUTBOX.
*            TY_TOOLBAR-FUNCTION  = C_ENVIO_CARGA.
*            TY_TOOLBAR-TEXT      = 'Enviar Carga'.
*            TY_TOOLBAR-BUTN_TYPE = 0.
*            APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*            CLEAR TY_TOOLBAR.

            READ TABLE TL_PARAMETROS INTO WL_PARAMETROS WITH KEY PARID = 'ZCCT_CANCEL_CARGA'.
            IF SY-SUBRC EQ 0.
              TY_TOOLBAR-ICON      = ICON_STORNO.
              TY_TOOLBAR-FUNCTION  = C_CANCEL_RECEP_CARGA.
              TY_TOOLBAR-TEXT      = 'Cancelar Recepção Carga'.
              TY_TOOLBAR-BUTN_TYPE = 0.
              APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
              CLEAR TY_TOOLBAR.
            ENDIF.

            TY_TOOLBAR-ICON      = ICON_HISTORY.
            TY_TOOLBAR-FUNCTION  = C_LOG_CANCEL.
            TY_TOOLBAR-TEXT      = 'Log Cancelamento'.
            TY_TOOLBAR-BUTN_TYPE = 0.
            APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
            CLEAR TY_TOOLBAR.

            TY_TOOLBAR-ICON      = ICON_VIEWER_OPTICAL_ARCHIVE.
            TY_TOOLBAR-FUNCTION  = C_DOC_RAT_RECEPCAO.
            TY_TOOLBAR-TEXT      = 'Documentos Recepção'.
            TY_TOOLBAR-BUTN_TYPE = 0.
            APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
            CLEAR TY_TOOLBAR.

*            SELECT SINGLE *
*              FROM SETLEAF INTO WL_SETLEAF
*             WHERE SETNAME = 'ZLES0147_PROC_RCC'
*               AND VALFROM = SY-UNAME.
*            IF SY-SUBRC = 0.
              TY_TOOLBAR-ICON      = ICON_SUBMIT.
              TY_TOOLBAR-FUNCTION  = C_PROC_RCC.
              TY_TOOLBAR-TEXT      = 'Processa Recepção Carga'.
              TY_TOOLBAR-BUTN_TYPE = 0.
              APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
              CLEAR TY_TOOLBAR.
*            ENDIF.

        ENDCASE.


      WHEN P_TP_ECG. "Entregar Carga

        CASE ABAP_TRUE.
          WHEN P_VW_DUE. "DU-e's

            TY_TOOLBAR-ICON      = ICON_NEW_TASK.
            TY_TOOLBAR-FUNCTION  = C_NOVA_ENTREGA.
            TY_TOOLBAR-TEXT      = 'Nova Entrega'.
            TY_TOOLBAR-BUTN_TYPE = 0.
            APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
            CLEAR TY_TOOLBAR.

          WHEN P_EC_DUE. "Entregas de Carga por DU-e

            TY_TOOLBAR-ICON      = ICON_GENERATE.
            TY_TOOLBAR-FUNCTION  = C_ENTREGAR_CARGA.
            TY_TOOLBAR-TEXT      = 'Entregar Carga'.
            TY_TOOLBAR-BUTN_TYPE = 0.
            APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
            CLEAR TY_TOOLBAR.

            TY_TOOLBAR-ICON      = ICON_CHANGE.
            TY_TOOLBAR-FUNCTION  = C_CHANGE_ENTREGA.
            TY_TOOLBAR-TEXT      = 'Modificar'.
            TY_TOOLBAR-BUTN_TYPE = 0.
            APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
            CLEAR TY_TOOLBAR.

            TY_TOOLBAR-ICON      = ICON_DISPLAY.
            TY_TOOLBAR-FUNCTION  = C_VIEW_ENTREGA.
            TY_TOOLBAR-TEXT      = 'Visualizar'.
            TY_TOOLBAR-BUTN_TYPE = 0.
            APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
            CLEAR TY_TOOLBAR.

            READ TABLE TL_PARAMETROS INTO WL_PARAMETROS WITH KEY PARID = 'ZCCT_CANC_ENTREGA_CG'.
            IF SY-SUBRC EQ 0.
              TY_TOOLBAR-ICON      = ICON_STORNO.
              TY_TOOLBAR-FUNCTION  = C_CANCEL_ENTREGA_CARGA.
              TY_TOOLBAR-TEXT      = 'Cancelar Entrega Carga'.
              TY_TOOLBAR-BUTN_TYPE = 0.
              APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
              CLEAR TY_TOOLBAR.
            ENDIF.


        ENDCASE.


    ENDCASE.

    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN C_LIB_ENVIO_CARGA_PARC.
        PERFORM F_LIB_ENVIO_CARGA_PARC.
      WHEN C_LOG_CANCEL.
        PERFORM F_LOG_CANCEL_RECEPCAO.
      WHEN C_DOC_RAT_RECEPCAO.
        PERFORM F_DOC_RAT_RECEPCAO.
      WHEN C_DOWN_XML_RC_NFE.
        PERFORM F_DOWN_XML_RC_NFE.
      WHEN C_DEF_CNPJ_CPF_TRANSP.
        PERFORM F_DEFINE_CNPJ_CPF_TRANSP.
      WHEN C_REPROC_ROM.
        PERFORM F_REPROCESSAR_MOV_ROM.
      WHEN C_CALL_WEB.
        PERFORM F_CONECTAR_SERVER.
      WHEN C_LOGIN.
        PERFORM F_AUTENTICAR.
      WHEN C_LOGOUT.
        PERFORM F_LOGOUT.
      WHEN C_RECEPCIONAR_CARGA.
        PERFORM F_RECEPCIONAR_CARGA.
      WHEN C_CANCEL_RECEP_CARGA.
        PERFORM F_CANCEL_RECEP_CARGA.
      WHEN C_CANCEL_ENTREGA_CARGA.
        PERFORM F_CANCEL_ENTREGA_CARGA.
      WHEN C_ENVIO_CARGA.
        PERFORM F_ENVIO_CARGA.
      WHEN C_DISP_NFE_AJUSTE.
        PERFORM F_LIB_NFE_AJUSTE.
      WHEN C_NOVA_ENTREGA.
        PERFORM F_NOVA_ENTREGA.
      WHEN C_ENTREGAR_CARGA.
        PERFORM F_ENTREGAR_CARGA.
      WHEN C_CHANGE_ENTREGA.
        PERFORM F_CHANGE_ENTREGA.
      WHEN C_VIEW_ENTREGA.
        PERFORM F_VIEW_ENTREGA.
      WHEN C_PROC_RCC.
        PERFORM F_PROCESSA_RECEPCAO_SEL.
    ENDCASE.

    CASE E_UCOMM.
      WHEN C_ENVIO_CARGA             OR
           C_RECEPCIONAR_CARGA       OR
           C_CANCEL_RECEP_CARGA      OR
           C_CANCEL_ENTREGA_CARGA    OR
           C_DISP_NFE_AJUSTE         OR
           C_LIB_ENVIO_CARGA_PARC    OR
           C_NOVA_ENTREGA            OR
           C_ENTREGAR_CARGA          OR
           C_REPROC_ROM.

        PERFORM: F_SELECIONAR_DADOS,
                 F_PROCESSA_DADOS.

        LEAVE TO SCREEN 0100.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS LCL_EVENT_HANDLER_0100 IMPLEMENTATION.


  METHOD CATCH_HOTSPOT.

    DATA: IT_RSPARAMS   TYPE TABLE OF RSPARAMS,
          WA_RSPARAMS   TYPE RSPARAMS.

    DATA: OPT     TYPE CTU_PARAMS,
          VL_LOTE TYPE ZGLT034-LOTE.

    CASE ABAP_TRUE.
      WHEN P_TP_RCC. "Recepcionar Carga

        CASE ABAP_TRUE.
          WHEN P_VW_NF. "Notas Fiscais
          WHEN P_VW_DUE. "DU-e's
          WHEN P_RC_NF. "Recepções de Carga por NF

            CASE E_COLUMN_ID.
              WHEN 'DOCNUM'.

                READ TABLE IT_SAIDA_0100_02 INTO WA_SAIDA_0100_02 INDEX E_ROW_ID-INDEX.

                CHECK ( SY-SUBRC = 0 ) AND ( WA_SAIDA_0100_02-DOCNUM IS NOT INITIAL ).
                SET PARAMETER ID 'JEF'  FIELD WA_SAIDA_0100_02-DOCNUM.
                CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

            ENDCASE.

          WHEN P_RC_DUE. "Recepções de Carga por DU-e
          WHEN P_EC_DUE. "Entregas de Carga por DU-e
        ENDCASE.

      WHEN OTHERS.

    ENDCASE.




  ENDMETHOD.

ENDCLASS.

CLASS LCL_ALV_TOOLBAR_0110 IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

     CLEAR TY_TOOLBAR.
     TY_TOOLBAR-ICON      = ICON_SYSTEM_UNDO.
     TY_TOOLBAR-FUNCTION  = 'CLEAR_ST'.
     TY_TOOLBAR-TEXT      = 'Reiniciar Status.'.
     TY_TOOLBAR-BUTN_TYPE = 0.
     APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
     CLEAR TY_TOOLBAR.

     TY_TOOLBAR-FUNCTION     = 'ESTORNO_CTB'.
     TY_TOOLBAR-ICON         = ICON_STORNO.
     TY_TOOLBAR-BUTN_TYPE    = 0.
     TY_TOOLBAR-TEXT         = 'Estornar Ctb.'.
     APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
     CLEAR TY_TOOLBAR.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN 'ESTORNO_CTB'.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS LCL_EVENT_HANDLER_0110 IMPLEMENTATION.


  METHOD CATCH_HOTSPOT.

    DATA: IT_RSPARAMS   TYPE TABLE OF RSPARAMS,
          WA_RSPARAMS   TYPE RSPARAMS.

    DATA: OPT     TYPE CTU_PARAMS,
          VL_LOTE TYPE ZGLT034-LOTE.

    CASE E_COLUMN_ID.
      WHEN 'LOTE'.

*        READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX E_ROW_ID-INDEX.
*
*        CHECK WA_SAIDA_0100-LOTE IS NOT INITIAL.
*
*        REFRESH: IT_RSPARAMS.
*
*        WA_RSPARAMS-SELNAME = 'P_BUKRS'.
*        WA_RSPARAMS-KIND = 'P'.  "SELECT OPTIONS TO BE PASSED
*        WA_RSPARAMS-SIGN = 'I'.
*        WA_RSPARAMS-OPTION = 'EQ'.
*        WA_RSPARAMS-LOW    = WA_SAIDA_0100-BUKRS.
*        WA_RSPARAMS-HIGH   = WA_SAIDA_0100-BUKRS.
*        APPEND WA_RSPARAMS TO IT_RSPARAMS.
*
*        WA_RSPARAMS-SELNAME = 'P_LOTE'.
*        WA_RSPARAMS-KIND = 'S'.  "SELECT OPTIONS TO BE PASSED
*        WA_RSPARAMS-SIGN = 'I'.
*        WA_RSPARAMS-OPTION = 'EQ'.
*        WA_RSPARAMS-LOW    = WA_SAIDA_0100-LOTE.
*        WA_RSPARAMS-HIGH   = WA_SAIDA_0100-LOTE.
*        APPEND WA_RSPARAMS TO IT_RSPARAMS.
*
*        SUBMIT ZGL018 WITH SELECTION-TABLE IT_RSPARAMS
*                AND RETURN.



    ENDCASE.

  ENDMETHOD.

ENDCLASS.

FORM F_MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                              VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                              VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                              VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                              VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                              VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                              VALUE(P_OUTPUTLEN)
                              VALUE(P_EDIT).

  CLEAR: WA_ESTRUTURA.

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  WA_ESTRUTURA-OUTPUTLEN = P_OUTPUTLEN.


  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM F_MONTAR_LAYOUT_LOG_CANCEL.
  REFRESH ESTRUTURA.
  PERFORM F_MONTAR_ESTRUTURA USING:
     01  ''   ''            'TG_0146_CANCEL' 'ID_RECEPCAO'     'Id.Rec.'          '10' '',
     02  ''   ''            'TG_0146_CANCEL' 'DT_RECEPCAO'     'Dt.Recepção'      '11' '',
     03  ''   ''            'TG_0146_CANCEL' 'HR_RECEPCAO'     'Hr.Recepção'      '11' '',
     04  ''   ''            'TG_0146_CANCEL' 'US_RECEPCAO'     'Usuário'          '13' '',
     05  ''   ''            'TG_0146_CANCEL' 'DT_CANCEL'       'Dt.Cancel'        '10' '',
     06  ''   ''            'TG_0146_CANCEL' 'HR_CANCEL'       'Hr.Cancel'        '10' '',
     07  ''   ''            'TG_0146_CANCEL' 'US_CANCEL'       'Us.Cancel'        '13' '',
     08  ''   ''            'TG_0146_CANCEL' 'CHAVE_NFE'       'Chave NF-e'       '45' '',
     08  ''   ''            'TG_0146_CANCEL' 'CHAVE_NFF'       'Chave NF-f'       '45' '',
     09  ''   ''            'TG_0146_CANCEL' 'CNPJ_EMISSOR'    'CNPJ Fornecedor'  '16' '',
     10  ''   ''            'TG_0146_CANCEL' 'NUMERO'          'Núm. NF-e'        '10' '',
     11  ''   ''            'TG_0146_CANCEL' 'DT_EMISSAO'      'Dt.Emissão'       '11' ''.

ENDFORM.                    " MONTAR_LAYOUT
