*&---------------------------------------------------------------------*
*&  Include           ZMMR104_PAI
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0100 INPUT.
  IF ( R_UTILS IS INITIAL ).
    CREATE OBJECT R_UTILS.
  ENDIF.

  CREATE OBJECT R_OPERACAO.

  CASE SY-UCOMM.
    WHEN C_BACK.
      LEAVE TO SCREEN 0.

    WHEN C_CANC OR C_EXIT.
      LEAVE PROGRAM.

    WHEN C_SHOW_MSG.
      R_UTILS->SHOW_ERRORS( I_SHOW = 'X').

    WHEN C_SAVE.
      CLEAR GT_FIELDS.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          PERCENTAGE = 1
          TEXT       = TEXT-W02.

      CHECK R_UTILS->VALIDA_CABECALHO( ) = ABAP_TRUE.
      CHECK R_UTILS->VALIDA_ITENS( )     = ABAP_TRUE.

      VG_OP_MODE = C_SAVE.
      R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR1'
                              VALUE     = '0'
                              INVISIBLE = '0' ).

      R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR2'
                              VALUE     = '0'
                              INVISIBLE = '0' ).

      "// Get new values and check modifications;
      R_UTILS->SET_NEW_VALUES( I_HEADER = WL_SAIDA_CABECALHO
                               I_ITENS  = GT_SAIDA_ITENS ).

      "// Check modifications and save log;
      R_UTILS->VALIDATE_CHANGES( EXCEPTIONS RECORDS_ALREADY_SAVED = 4 ).

      "// Save data;
      IF SY-SUBRC IS INITIAL.
        R_OPERACAO->SALVAR_REGISTROS( ).

        "// After validated the data, set the new values how old;
        R_UTILS->SET_OLD_VALUES( I_HEADER = WL_SAIDA_CABECALHO
                                 I_ITENS  = GT_SAIDA_ITENS ).
      ENDIF.
    WHEN C_NOVO.
      R_OPERACAO->NOVO_REGISTRO( ).

    WHEN C_SEARCH.
      CLEAR GT_FIELDS.
      CLEAR: WL_SAIDA_CABECALHO, GT_SAIDA_ITENS, WL_MENSAGEM,
       GT_MSG_RETURN, R_WERKS, GT_FIELDS, R_WERKS[]. "GT_ITEM_0200.

      BTN_DISPLAY_WERKS = ICON_ENTER_MORE.

      VG_OP_MODE = C_SEARCH.
      R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR1'
                              VALUE     = '1'
                              INVISIBLE = '0' ).

      R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR2'
                              VALUE     = '0'
                              INVISIBLE = '0' ).

    WHEN C_EDIT.
      IF ( WL_SAIDA_CABECALHO-LOEKZ = ABAP_TRUE ).
        MESSAGE TEXT-E13 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        CLEAR GT_FIELDS.
        VG_OP_MODE = C_EDIT.

        R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR1'
                                VALUE     = '0'
                                INVISIBLE = '0' ).

        R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR2'
                                VALUE     = '1'
                                INVISIBLE = '0' ).

        LOOP AT GT_SAIDA_ITENS ASSIGNING <FS_SAIDA_ITENS>.
          CLEAR <FS_SAIDA_ITENS>-ESTILO.
        ENDLOOP.

        R_UTILS->SET_OLD_VALUES( I_HEADER = WL_SAIDA_CABECALHO
                                 I_ITENS  = GT_SAIDA_ITENS ).
      ENDIF.
    WHEN C_COPY.
      CLEAR: GT_FIELDS,
             WL_SAIDA_CABECALHO-EVRTN,
             WL_SAIDA_CABECALHO-LOEKZ,
             WL_SAIDA_CABECALHO-VEDAT,
             WL_SAIDA_CABECALHO-CTRA_SUPERIOR,
             R_UTILS->MT_OLD_ITENS,
             R_UTILS->MW_OLD_HEADER.

      LOOP AT GT_SAIDA_ITENS ASSIGNING <FS_SAIDA_ITENS>.
        CLEAR: <FS_SAIDA_ITENS>-ESTILO,
               <FS_SAIDA_ITENS>-EVRTN.

        <FS_SAIDA_ITENS>-STATUS   = ICON_LIGHT_OUT.
        <FS_SAIDA_ITENS>-CHANGEID = R_UTILS->C_CRUD-INSERT.
      ENDLOOP.

      UNASSIGN <FS_SAIDA_ITENS>.

      VG_OP_MODE = C_COPY.
      R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR1'
                              VALUE     = '0'
                              INVISIBLE = '0' ).

      R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR2'
                              VALUE     = '1'
                              INVISIBLE = '0' ).

    WHEN C_DELETE.
      VG_OP_MODE = C_DELETE.

      R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR1'
                              VALUE     = '0'
                              INVISIBLE = '0' ).

      R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR2'
                              VALUE     = '0'
                              INVISIBLE = '0' ).

      R_OPERACAO->SET_DELETION_FLAG( ).

    WHEN C_DB_CLICK.
      IF WL_SAIDA_CABECALHO-CTRA_SUPERIOR IS NOT INITIAL.
        SET PARAMETER ID 'CTR' FIELD WL_SAIDA_CABECALHO-CTRA_SUPERIOR.
        CALL TRANSACTION 'ME33K' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " PAI_0100  INPUT

MODULE PAI_0200 INPUT.
*  CASE SY-UCOMM.
*    WHEN 'EXIT'.
*      CLEAR: GT_ITEM_0200, WL_ITEM_0200, VG_SEQ.
*      LEAVE TO SCREEN 0.
*    WHEN 'OK'.
*      CLEAR <FS_SAIDA_ITENS>-ITENS_CLAS.
*
*      IF ( VG_SEQ > 1 ).
*        IF ( <FS_SAIDA_ITENS>-VRTKZ IS INITIAL ).
*          MESSAGE S836(SD) WITH TEXT-E09 DISPLAY LIKE 'E'. EXIT.
*        ELSEIF ( <FS_SAIDA_ITENS>-TWRKZ IS INITIAL ).
*          MESSAGE S836(SD) WITH TEXT-E10 DISPLAY LIKE 'E'. EXIT.
*        ENDIF.
*      ENDIF.
*
*      APPEND LINES OF GT_ITEM_0200 TO <FS_SAIDA_ITENS>-ITENS_CLAS.
*      MESSAGE TEXT-S01 TYPE 'S' DISPLAY LIKE 'S'.
*
*      LEAVE TO SCREEN 0.
*    WHEN OTHERS.
*  ENDCASE.
*
*  UNASSIGN <FS_SAIDA_ITENS>.
ENDMODULE.                 " PAI_0200  INPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0300 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT'.
      CLEAR: GT_SAIDA_SERVICOS, VG_ITEM_0400.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_COLLAPSE'.
      IF ( BTN_COLLAPSE EQ ICON_EXPAND ).
        BTN_COLLAPSE = ICON_COLLAPSE.
        VG_SCREEN_COLLAPSE = 0400.
      ELSE.
        BTN_COLLAPSE = ICON_EXPAND.
        VG_SCREEN_COLLAPSE = 0500.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " PAI_0300  INPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0600 INPUT.
  CASE SY-UCOMM.
    WHEN C_EXIT.
      LEAVE TO SCREEN 0.
    WHEN C_VIEW_ITEM.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " PAI_0600  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0600 OUTPUT.
  SET PF-STATUS '0600'.
  SET TITLEBAR '0600'.

ENDMODULE.                 " STATUS_0600  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0110 INPUT.
  IF ( R_UTILS    IS INITIAL ).
    CREATE OBJECT R_UTILS.
  ENDIF.

  CREATE OBJECT R_OPERACAO.

  CASE SY-UCOMM.
    WHEN C_ENTER.
      IF ( VG_OP_MODE = C_SEARCH ).
        IF WL_SAIDA_CABECALHO-EVRTN IS INITIAL.
          MESSAGE TEXT-W01 TYPE 'I' DISPLAY LIKE 'W'.
        ELSE.
          R_OPERACAO->SEARCH_REGISTROS( I_EVRTN = WL_SAIDA_CABECALHO-EVRTN ).
        ENDIF.
      ELSE.
        CHECK R_UTILS->VALIDA_CABECALHO( ) = ABAP_TRUE.
        CHECK ( GT_SAIDA_ITENS IS INITIAL ).
        LCL_EVENT_TOOLBAR=>GET_UCOMM( E_UCOMM = 'BTN_INSERT_ROW' ).
      ENDIF.


    WHEN C_BTN_DISPLAY_WERKS.
      DATA LV_ENABLED TYPE C.
      DATA LV_ERROR   TYPE ABAP_BOOL VALUE ABAP_TRUE.

      IF ( WL_SAIDA_CABECALHO-BUKRS IS NOT INITIAL ).
        IF ( VG_OP_MODE = C_SEARCH )
        OR ( VG_OP_MODE = C_SAVE   ).
          LV_ENABLED  = ABAP_ON.
        ELSE.
          LV_ENABLED = ABAP_OFF.
        ENDIF.

        DATA(OLD_WERKS) = R_WERKS[].
        PERFORM F_SELECTIONS_DIALOG TABLES R_WERKS
                                     USING 'Informar centro'
                                           LV_ENABLED
                                           'PC_WERKS'.

        IF SY-UCOMM = 'ACPT'.
          DATA CONDITIONS TYPE TABLE OF RSIS_S_CONDITION_ID.

          DO.
            TRY.
                LOOP AT R_WERKS[] INTO R_WERKS.
                  SELECT SINGLE BRANCH
                    FROM J_1BBRANCH
                    INTO @DATA(LV_WERKS)
                   WHERE BUKRS  = @WL_SAIDA_CABECALHO-BUKRS
                     AND BRANCH = @R_WERKS-LOW.

                  IF ( NOT SY-SUBRC IS INITIAL ).
                    DATA(_TXT_ERROR) = |Centro { R_WERKS-LOW } não pertence a empresa { WL_SAIDA_CABECALHO-BUKRS }.|.
                    MESSAGE _TXT_ERROR TYPE 'S' DISPLAY LIKE 'E'.

                    PERFORM F_SELECTIONS_DIALOG
                     TABLES R_WERKS
                      USING _TXT_ERROR LV_ENABLED 'PC_WERKS'.

                    RAISE EXCEPTION TYPE CX_ABAP_INVALID_PARAM_VALUE.
                  ENDIF.
                ENDLOOP.

                LV_ERROR = ABAP_FALSE.
              CATCH CX_ABAP_INVALID_PARAM_VALUE.
                LV_ERROR = ABAP_TRUE.
            ENDTRY.

            CHECK ( LV_ERROR = ABAP_FALSE ).
            EXIT.
          ENDDO.

          LOOP AT OLD_WERKS INTO DATA(_OLD_WERKS).
            READ TABLE R_WERKS TRANSPORTING NO FIELDS WITH KEY LOW = _OLD_WERKS-LOW.

            IF SY-SUBRC IS NOT INITIAL.
              LOOP AT GT_SAIDA_ITENS ASSIGNING <FS_SAIDA_ITENS>.
                TRY.
                    DATA(_SAIDA_ITEM) = <FS_SAIDA_ITENS>-T_WERKS[ LOW = _OLD_WERKS-LOW ].

                    SELECT SINGLE *
                      FROM ZMMT0064
                      INTO @DATA(_ITEM_CONTRATO)
                     WHERE EVRTN = @<FS_SAIDA_ITENS>-EVRTN
                       AND EVRTP = @<FS_SAIDA_ITENS>-EVRTP
                       AND WERKS = @_OLD_WERKS-LOW.

                    IF _ITEM_CONTRATO-NIVEL = R_UTILS->C_NIVEL-APROVADO.
                      APPEND VALUE RSIS_S_CONDITION_ID( NAME = _OLD_WERKS-LOW OBJECT = <FS_SAIDA_ITENS>-EVRTP ) TO CONDITIONS.
                    ELSE.
                      DELETE <FS_SAIDA_ITENS>-T_WERKS WHERE LOW = _OLD_WERKS-LOW.
                    ENDIF.
                  CATCH CX_SY_ITAB_LINE_NOT_FOUND.
                ENDTRY.
              ENDLOOP.
            ENDIF.
          ENDLOOP.

          IF CONDITIONS IS NOT INITIAL.
            CL_DEMO_OUTPUT=>NEW( ).
            CL_DEMO_OUTPUT=>WRITE_TEXT( TEXT-W06 ).
            CL_DEMO_OUTPUT=>LINE( ).

            LOOP AT CONDITIONS INTO DATA(_CONDITION).
              CL_DEMO_OUTPUT=>WRITE_TEXT( |Centro: { _CONDITION-NAME } -> Item: { _CONDITION-OBJECT }| ).
              APPEND VALUE #( OPTION = 'EQ' SIGN = 'I' LOW = _CONDITION-NAME ) TO R_WERKS[].
            ENDLOOP.

            CLEAR CONDITIONS.
            CL_DEMO_OUTPUT=>DISPLAY( ).
          ENDIF.
        ENDIF.

        IF R_WERKS[] IS NOT INITIAL.
          SORT R_WERKS BY LOW.
          READ TABLE R_WERKS INDEX 1.
          BTN_DISPLAY_WERKS = ICON_DISPLAY_MORE.
        ELSE.
          BTN_DISPLAY_WERKS = ICON_ENTER_MORE.
          CLEAR R_WERKS.
        ENDIF.
      ELSE.
        MESSAGE S836(SD) WITH TEXT-E02 'Empresa.' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " PAI_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0120 INPUT.
  CASE SY-UCOMM.
    WHEN 'BTN_DISPAY'.

*      LOOP AT KEYS_TABLE INTO DATA(_KEYS_TABLE).
*         DATA(_SELECTED_ITEM) =
*      ENDLOOP.

      "//Define a aba "Criar/visualizar contratos" como atual p/ visualizar o contrato
*        APPEND 1 TO LT_NODES_SELECT.
*        OBJ_ALV_TREE_0100->SET_SELECTED_NODES( LT_NODES_SELECT ).
*
*        R_UTILS->GET_HISTORY_CHANGE( EXPORTING
*                                     I_EVRTN   = R_UTILS->AT_EVRTN
*                                     I_EVRTP   = R_UTILS->AT_EVRTP
*                                     I_WERKS   = R_UTILS->AT_WERKS
*                                    ).
*
*        DELETE R_UTILS->MT_ZMMT0068 WHERE FNAME     EQ 'WERKS'
*                                      AND VALUE_NEW NE R_UTILS->AT_WERKS.
*
*        CLEAR R_UTILS->MT_HISTORY_OUT.
*        LOOP AT R_UTILS->MT_ZMMT0068 INTO R_UTILS->MW_ZMMT0068.
*          R_UTILS->MW_HISTORY_OUT-EVRTP      = R_UTILS->MW_ZMMT0068-EVRTP.
*          R_UTILS->MW_HISTORY_OUT-FTEXT      = R_UTILS->MW_ZMMT0068-FTEXT.
*          R_UTILS->MW_HISTORY_OUT-VALUE_OLD  = R_UTILS->MW_ZMMT0068-VALUE_OLD.
*          R_UTILS->MW_HISTORY_OUT-VALUE_NEW  = R_UTILS->MW_ZMMT0068-VALUE_NEW.
*          R_UTILS->MW_HISTORY_OUT-USERNAME   = R_UTILS->MW_ZMMT0068-USERNAME.
*          R_UTILS->MW_HISTORY_OUT-DATA       = R_UTILS->MW_ZMMT0068-DATA.
*          R_UTILS->MW_HISTORY_OUT-HORA       = R_UTILS->MW_ZMMT0068-HORA.
*
*          CASE R_UTILS->MW_ZMMT0068-CHANGEID.
*            WHEN R_UTILS->C_CRUD-DELETE.
*              R_UTILS->MW_HISTORY_OUT-ACTION = 'Delet.'.
*            WHEN R_UTILS->C_CRUD-INSERT.
*              R_UTILS->MW_HISTORY_OUT-ACTION = 'Inser.'.
*            WHEN R_UTILS->C_CRUD-UPDATE.
*              R_UTILS->MW_HISTORY_OUT-ACTION = 'Modif.'.
*            WHEN R_UTILS->C_NIVEL-APROVADO.
*              R_UTILS->MW_HISTORY_OUT-ACTION = 'Aprov.'.
*            WHEN R_UTILS->C_NIVEL-REJEITADO.
*              R_UTILS->MW_HISTORY_OUT-ACTION = 'Rejei.'.
*          ENDCASE.
*
*          APPEND R_UTILS->MW_HISTORY_OUT TO R_UTILS->MT_HISTORY_OUT.
*          CLEAR R_UTILS->MW_HISTORY_OUT.
*        ENDLOOP.
*
*        R_UTILS->SHOW_HISTORY_CHANGE( EXPORTING I_PFSTATUS = '0600'
*                                      CHANGING  C_TABLE    = R_UTILS->MT_HISTORY_OUT ).
*
*        R_UTILS->MO_ALV->DISPLAY( ).
*
*        IF ( SY-UCOMM = C_VIEW_ITEM ).
*          VG_OP_MODE  = C_SEARCH.
*          R_OPERACAO->SEARCH_REGISTROS( I_EVRTN = R_UTILS->AT_EVRTN
*                                        I_EVRTP = R_UTILS->AT_EVRTP
*                                        I_WERKS = R_UTILS->AT_WERKS ).
*          VG_SCREEN_PRINCIPAL = '0110'.
*        ENDIF.

    WHEN 'BTN_ALLOW_ALL'.
      DATA KEYS_TABLE TYPE TREEV_NKS.
      DATA(LT_KEYS) = GT_ITEM_TABLE.

      DELETE ADJACENT DUPLICATES FROM LT_KEYS COMPARING NODE_KEY.

      R_UTILS->POPUP_TO_CONFIRM( EXPORTING
                                 I_TITLE    = 'Aprovar todos os contratos'
                                 I_QUESTION = 'Tem certeza que deseja aprovar todos os contratos?'
                                 IMPORTING
                                 E_ANSWER   = R_UTILS->AT_ANSWER ).

      IF ( R_UTILS->AT_ANSWER = 1 ). "//Sim
        DATA COUNT TYPE I.

        COUNT = 1.
        VG_OP_MODE = C_ALLOW_ALL.

        WHILE COUNT <=  LINES( LT_KEYS ).
          LCL_EVENT_HANDLER=>HANDLE_BUTTON_CLICK(
              ITEM_NAME = 'Allow'
              NODE_KEY  = CONV #( COUNT ) ).

          COUNT = COUNT + 1.
        ENDWHILE.

        MESSAGE 'Contratos aprovados com sucesso!' TYPE 'S'.

        OBJ_ALV_TREE_0120->DELETE_ALL_NODES(
          EXCEPTIONS
            FAILED            = 1
            CNTL_SYSTEM_ERROR = 2
            OTHERS            = 3
        ).

      ENDIF.
  ENDCASE.
ENDMODULE.
