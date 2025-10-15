FUNCTION ZBSPL_TREE_CREATE .
*"----------------------------------------------------------------------
*"*"Interface global:
*"  IMPORTING
*"     VALUE(IS_SETTINGS) LIKE  RFBILA_ALV_SETTINGS STRUCTURE
*"        RFBILA_ALV_SETTINGS
*"     VALUE(IT_LIST_COMMENTARY) TYPE  SLIS_T_LISTHEADER OPTIONAL
*"     REFERENCE(P_BUKRS) TYPE  BUKRS
*"     REFERENCE(SD_CURT) TYPE  ALLGCRTP
*"     REFERENCE(SD_CURT2) TYPE  ALLGCRTP
*"----------------------------------------------------------------------
  CALL FUNCTION 'RGRE_ERGSL_TEXT_GET'
    EXPORTING
      LANGUAGE        = IS_SETTINGS-FS_LANGUAGE
      BALANCE_VERSION = IS_SETTINGS-FS_VERSION
      TEXT_TYPE       = 'K'
    TABLES
      TEXT_TAB        = GT_ERGSL_TEXT.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      I_BUKRS                = P_BUKRS
    IMPORTING
      E_X001                 = E_X001
    EXCEPTIONS
      CURRENCY_2_NOT_DEFINED = 1
      CURRENCY_3_NOT_DEFINED = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  SELECT SINGLE * INTO WA_T001 FROM T001 WHERE BUKRS EQ P_BUKRS.

  IF SD_CURT EQ '10'.
    MOEDA_01 = WA_T001-WAERS.
  ELSEIF SD_CURT EQ E_X001-CURT2.
    MOEDA_01 = E_X001-HWAE2.
  ELSEIF SD_CURT EQ E_X001-CURT3.
    MOEDA_01 = E_X001-HWAE3.
  ENDIF.

  IF SD_CURT2 EQ '10'.
    MOEDA_02 = WA_T001-WAERS.
  ELSEIF SD_CURT2 EQ E_X001-CURT2.
    MOEDA_02 = E_X001-HWAE2.
  ELSEIF SD_CURT2 EQ E_X001-CURT3.
    MOEDA_02 = E_X001-HWAE3.
  ENDIF.

  CALL SCREEN 100.

ENDFUNCTION.


*----------------------------------------------------------------------*
*  MODULE STATUS_0100 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'MAIN'.
  IF NOT ( IS_SETTINGS-TITLE IS INITIAL ).
    SET TITLEBAR 'MAIN' WITH IS_SETTINGS-TITLE.
  ENDIF.
  IF G_CUSTOM_CONTAINER2 IS INITIAL.
    IF SY-BATCH IS INITIAL.
      CREATE OBJECT G_CUSTOM_CONTAINER2
        EXPORTING
          CONTAINER_NAME              = 'ALV_TREE_CONTROL'
        EXCEPTIONS
          CNTL_ERROR                  = 1
          CNTL_SYSTEM_ERROR           = 2
          CREATE_ERROR                = 3
          LIFETIME_ERROR              = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5.

      IF SY-SUBRC <> 0.
        MESSAGE A000(TREE_CONTROL_MSG).
      ENDIF.
    ENDIF.
    CREATE OBJECT ALV_TREE_CONTROL
      EXPORTING
        PARENT         = G_CUSTOM_CONTAINER2
        NO_HTML_HEADER = ' '.

    PERFORM BSPL_TREE_COLUMN_AREA_ADJUST
                                CHANGING FIELDCATALOG.
    GS_DISVARIANT-REPORT   = IS_SETTINGS-REPID.
    GS_DISVARIANT-HANDLE   = CON_TREE.
    GS_DISVARIANT-USERNAME = SY-UNAME.
    GS_DISVARIANT-VARIANT  = IS_SETTINGS-TREE_VARI.

    PERFORM BSPL_TREE_HEADER_DEFINE
                                   CHANGING HIERARCHY_HEADER.

    DATA: L_EVENT_RECEIVER TYPE REF TO LCL_TREE_EVENT_RECEIVER.
*... create object for event receiver
    CREATE OBJECT L_EVENT_RECEIVER.
    SET HANDLER L_EVENT_RECEIVER->HANDLE_TOP_OF_PAGE
                                FOR ALV_TREE_CONTROL.
    SET HANDLER L_EVENT_RECEIVER->HANDLE_AFTER_USER_COMMAND "n1616718
                                FOR ALV_TREE_CONTROL.       "n1616718
    CALL METHOD ALV_TREE_CONTROL->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT          = GS_DISVARIANT
        I_SAVE              = 'A'
        IS_HIERARCHY_HEADER = HIERARCHY_HEADER
        IT_LIST_COMMENTARY  = IT_LIST_COMMENTARY
*       I_LOGO              = 'ENJOYSAP_LOGO'
      CHANGING
        IT_OUTTAB           = TABLE_POINTER
        IT_FIELDCATALOG     = FIELDCATALOG.

    DATA: LT_NKEY_EXP     TYPE LVC_T_NKEY .
    PERFORM BSPL_TREE_COLUMN_TREE_FILL TABLES LT_NKEY_EXP.

*    Update calculations which were initially defined by field DO_SUM
*    of the fieldcatalog. (see BSPL_TREE_COLUMN_AREA_ADJUST).
    CALL METHOD ALV_TREE_CONTROL->UPDATE_CALCULATIONS.

    CALL METHOD ALV_TREE_CONTROL->EXPAND_NODES
      EXPORTING
        IT_NODE_KEY             = LT_NKEY_EXP
      EXCEPTIONS
        FAILED                  = 1
        CNTL_SYSTEM_ERROR       = 2
        ERROR_IN_NODE_KEY_TABLE = 3
        DP_ERROR                = 4
        NODE_NOT_FOUND          = 5
        OTHERS                  = 6.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM CHANGE_TOOLBAR.                                 "n1616718

    CALL METHOD ALV_TREE_CONTROL->FRONTEND_UPDATE.

    CALL METHOD CL_GUI_CFW=>FLUSH.
  ENDIF.
ENDMODULE.                    "STATUS_0100 OUTPUT

*----------------------------------------------------------------------*
*  MODULE USER_COMMAND_0100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'BACK'
      OR 'EXIT'
      OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'SEL'.                                             "n1489749
      CALL FUNCTION 'FB_SELECTIONS_DISPLAY'                 "n1489749
        EXPORTING                                           "n1489749
          ED_PROGRAM = SY-CPROG                             "n1489749
          ED_MODE    = '1'.                                 "n1489749
    WHEN OTHERS.
      CALL METHOD CL_GUI_CFW=>DISPATCH.
  ENDCASE.
ENDMODULE.               "USER_COMMAND_0100 INPUT.

*&---------------------------------------------------------------------*
*&      Form  BSPL_TREE_COLUMN_AREA_ADJUST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDCATALOG  text
*----------------------------------------------------------------------*
FORM BSPL_TREE_COLUMN_AREA_ADJUST
                      CHANGING P_FIELDCATALOG TYPE LVC_T_FCAT.

  FIELD-SYMBOLS: <FIELD> TYPE LVC_S_FCAT.
  DATA: PCOL_POS LIKE SY-CUCOL.
* local data declaration
  DATA: LS_FIELDCATALOG TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZBSPL_TREE_FIELDCAT'
    CHANGING
      CT_FIELDCAT      = P_FIELDCATALOG.

  LOOP AT P_FIELDCATALOG ASSIGNING <FIELD>.

    IF <FIELD>-FIELDNAME EQ 'KTOPL' OR
       <FIELD>-FIELDNAME EQ 'KKTPL' OR
       <FIELD>-FIELDNAME EQ 'BILKT' OR
       <FIELD>-FIELDNAME EQ 'KTOP2' OR
       <FIELD>-FIELDNAME EQ 'ALTKT' OR
       <FIELD>-FIELDNAME EQ 'RBUKRS' OR
       <FIELD>-FIELDNAME EQ 'RBUSA' OR
       <FIELD>-FIELDNAME EQ 'RFAREA' OR
       <FIELD>-FIELDNAME EQ 'RYEAR' OR
       <FIELD>-FIELDNAME EQ 'POPER' OR
       <FIELD>-FIELDNAME EQ 'CURTP'.
      <FIELD>-NO_OUT = CON_X.
    ENDIF.

    IF <FIELD>-FIELDNAME EQ 'TEXT'.
      <FIELD>-OUTPUTLEN = 40.
    ENDIF.

    IF <FIELD>-FIELDNAME EQ 'KTOPL'.
      <FIELD>-OUTPUTLEN = 30.
    ENDIF.

    IF <FIELD>-FIELDNAME EQ 'REPVAL'   OR
       <FIELD>-FIELDNAME EQ 'COMPVAL'  OR
       <FIELD>-FIELDNAME EQ 'REPVAL2'  OR
       <FIELD>-FIELDNAME EQ 'COMPVAL2'.
      <FIELD>-OUTPUTLEN = 20.
    ENDIF.

    IF <FIELD>-FIELDNAME EQ 'ABSVAR'   OR
       <FIELD>-FIELDNAME EQ 'ABSVAR2'.
      <FIELD>-OUTPUTLEN = 15.
      "<FIELD>-NO_OUT = CON_X.
    ENDIF.

    IF <FIELD>-FIELDNAME EQ 'REPVALV'  OR
       <FIELD>-FIELDNAME EQ 'REPVAL2V' OR
       <FIELD>-FIELDNAME EQ 'REPVAL3V'.
      <FIELD>-OUTPUTLEN = 15.
    ENDIF.

    IF <FIELD>-FIELDNAME EQ 'RELVAR'   OR
       <FIELD>-FIELDNAME EQ 'RELVAR2'.
      <FIELD>-OUTPUTLEN = 7.
    ENDIF.

    IF <FIELD>-FIELDNAME EQ 'WAERS' OR
       <FIELD>-FIELDNAME EQ 'WAER2'.
      <FIELD>-OUTPUTLEN = 10.
      <FIELD>-JUST      = 'C'.
    ENDIF.

    IF <FIELD>-FIELDNAME EQ 'ERGSL'.
      <FIELD>-OUTPUTLEN = 6.
    ENDIF.

    IF <FIELD>-FIELDNAME CS 'PER0' OR <FIELD>-FIELDNAME CS 'PER1'.
      <FIELD>-NO_OUT = ABAP_TRUE.
    ENDIF.

  ENDLOOP.

  PCOL_POS = 15.

  LOOP AT P_FIELDCATALOG INTO LS_FIELDCATALOG.
    CASE LS_FIELDCATALOG-FIELDNAME.
      WHEN 'ERGSL'.
        LS_FIELDCATALOG-COL_POS = 1.
        LS_FIELDCATALOG-NO_OUT = 'X'.
      WHEN 'TEXT'.
        LS_FIELDCATALOG-COL_POS = 2.
      WHEN 'RACCT'.
        LS_FIELDCATALOG-COL_POS = 3.
      WHEN 'REPVAL'.

        CONCATENATE '(' MOEDA_01 ')' INTO LS_FIELDCATALOG-SCRTEXT_L.
        CONCATENATE TEXT-009 LS_FIELDCATALOG-SCRTEXT_L INTO LS_FIELDCATALOG-SCRTEXT_L SEPARATED BY SPACE. "'Total período relatório'
        CONCATENATE TEXT-010 MOEDA_01  INTO LS_FIELDCATALOG-SCRTEXT_S SEPARATED BY SPACE. "'PerRel'
        CONCATENATE TEXT-011 MOEDA_01  INTO LS_FIELDCATALOG-SCRTEXT_M SEPARATED BY SPACE. "'Per.Rel.'

        LS_FIELDCATALOG-COL_POS = 6.
        LS_FIELDCATALOG-DO_SUM  = 'X'.
      WHEN 'REPVAL2'.

        CONCATENATE '(' MOEDA_02 ')' INTO LS_FIELDCATALOG-SCRTEXT_L.
        CONCATENATE TEXT-009 LS_FIELDCATALOG-SCRTEXT_L INTO LS_FIELDCATALOG-SCRTEXT_L SEPARATED BY SPACE. "'Total período relatório'
        CONCATENATE TEXT-010 MOEDA_02  INTO LS_FIELDCATALOG-SCRTEXT_S SEPARATED BY SPACE. "'PerRel'
        CONCATENATE TEXT-011 MOEDA_02  INTO LS_FIELDCATALOG-SCRTEXT_M SEPARATED BY SPACE. "'Per.Rel.'

        LS_FIELDCATALOG-COL_POS = 7.
        LS_FIELDCATALOG-DO_SUM  = 'X'.
      WHEN 'REPVALV'.

        LS_FIELDCATALOG-SCRTEXT_L = TEXT-012."'Taxa Período Relatório'.
        LS_FIELDCATALOG-SCRTEXT_S = TEXT-013."'TxRel'.
        LS_FIELDCATALOG-SCRTEXT_M = TEXT-014."'Tx.Rel.'.

        LS_FIELDCATALOG-COL_POS = 8.
        LS_FIELDCATALOG-DO_SUM  = 'X'.
        LS_FIELDCATALOG-JUST    = 'R'.
      WHEN 'COMPVAL'.

        CONCATENATE '(' MOEDA_01 ')' INTO LS_FIELDCATALOG-SCRTEXT_L.
        CONCATENATE TEXT-015 LS_FIELDCATALOG-SCRTEXT_L INTO LS_FIELDCATALOG-SCRTEXT_L SEPARATED BY SPACE. "'Total período comparação'
        CONCATENATE TEXT-016 MOEDA_01  INTO LS_FIELDCATALOG-SCRTEXT_S. "'PerComp'
        CONCATENATE TEXT-017 MOEDA_01  INTO LS_FIELDCATALOG-SCRTEXT_M SEPARATED BY SPACE. "'Per.Comp.'

        LS_FIELDCATALOG-COL_POS = 9.
        LS_FIELDCATALOG-DO_SUM  = 'X'.
      WHEN 'COMPVAL2'.

        CONCATENATE '(' MOEDA_02 ')' INTO LS_FIELDCATALOG-SCRTEXT_L.
        CONCATENATE TEXT-015 LS_FIELDCATALOG-SCRTEXT_L INTO LS_FIELDCATALOG-SCRTEXT_L SEPARATED BY SPACE. "'Total período comparação'
        CONCATENATE TEXT-016 MOEDA_02  INTO LS_FIELDCATALOG-SCRTEXT_S. "'PerComp'
        CONCATENATE TEXT-017 MOEDA_02  INTO LS_FIELDCATALOG-SCRTEXT_M SEPARATED BY SPACE. "'Per.Comp.'

        LS_FIELDCATALOG-COL_POS = 10.
        LS_FIELDCATALOG-DO_SUM  = 'X'.
      WHEN 'REPVAL2V'.

        LS_FIELDCATALOG-SCRTEXT_L = TEXT-018."'Taxa Período Comparação'.
        LS_FIELDCATALOG-SCRTEXT_S = TEXT-019."'TxComp'.
        LS_FIELDCATALOG-SCRTEXT_M = TEXT-020."'Tx.Comp.'.

        LS_FIELDCATALOG-COL_POS = 11.
        LS_FIELDCATALOG-DO_SUM  = 'X'.
        LS_FIELDCATALOG-JUST    = 'R'.

      WHEN 'ABSVAR'.

        CONCATENATE '(' MOEDA_01 ')' INTO LS_FIELDCATALOG-SCRTEXT_L.
        CONCATENATE TEXT-021 LS_FIELDCATALOG-SCRTEXT_L INTO LS_FIELDCATALOG-SCRTEXT_L SEPARATED BY SPACE. "'Desvio Absoluto'
        CONCATENATE TEXT-022 MOEDA_01  INTO LS_FIELDCATALOG-SCRTEXT_S SEPARATED BY SPACE. "'Desvio'
        CONCATENATE TEXT-022 MOEDA_01  INTO LS_FIELDCATALOG-SCRTEXT_M SEPARATED BY SPACE. "'Desvio'

        LS_FIELDCATALOG-COL_POS = 12.
      WHEN 'ABSVAR2'.
        CONCATENATE '(' MOEDA_02 ')' INTO LS_FIELDCATALOG-SCRTEXT_L.
        CONCATENATE TEXT-021 LS_FIELDCATALOG-SCRTEXT_L INTO LS_FIELDCATALOG-SCRTEXT_L SEPARATED BY SPACE. "'Desvio Absoluto'
        CONCATENATE TEXT-022 MOEDA_02  INTO LS_FIELDCATALOG-SCRTEXT_S SEPARATED BY SPACE. "'Desvio'
        CONCATENATE TEXT-022 MOEDA_02  INTO LS_FIELDCATALOG-SCRTEXT_M SEPARATED BY SPACE. "'Desvio'

        LS_FIELDCATALOG-COL_POS = 13.
      WHEN 'REPVAL3V'.
        LS_FIELDCATALOG-COL_POS = 14.
        LS_FIELDCATALOG-DO_SUM  = 'X'.
        LS_FIELDCATALOG-JUST    = 'R'.
        LS_FIELDCATALOG-SCRTEXT_L = TEXT-024.
        LS_FIELDCATALOG-SCRTEXT_M = TEXT-023.
        LS_FIELDCATALOG-SCRTEXT_S = TEXT-023.

      WHEN 'WAERS'.
        LS_FIELDCATALOG-SCRTEXT_L = TEXT-007."'Moeda Per'.
        LS_FIELDCATALOG-SCRTEXT_M = TEXT-007."'Moeda Per'.
        LS_FIELDCATALOG-SCRTEXT_S = TEXT-007."'Moeda Per'.
        LS_FIELDCATALOG-COL_POS = 4.
      WHEN 'WAER2'.
        LS_FIELDCATALOG-SCRTEXT_L = TEXT-008."'Moeda Comp'.
        LS_FIELDCATALOG-SCRTEXT_M = TEXT-008."'Moeda Comp'.
        LS_FIELDCATALOG-SCRTEXT_M = TEXT-008."'Moeda Comp'.
        LS_FIELDCATALOG-COL_POS = 5.
      WHEN OTHERS.
        LS_FIELDCATALOG-COL_POS = PCOL_POS.
        ADD 1 TO PCOL_POS.
        CASE LS_FIELDCATALOG-FIELDNAME.
          WHEN 'ID'
            OR 'PARENT'
            OR 'RLDNR'
            OR 'RRCTY'
            OR 'RVERS'.
            LS_FIELDCATALOG-NO_OUT = 'X'.
          WHEN 'ABSVAR'  OR 'ABSVAR2'.
            LS_FIELDCATALOG-DO_SUM  = 'X'.
          WHEN 'RELVAR' OR 'RELVAR2'.
            LS_FIELDCATALOG-JUST   = 'R'.
            LS_FIELDCATALOG-NO_OUT = CON_X.
        ENDCASE.
    ENDCASE.
    MODIFY P_FIELDCATALOG FROM LS_FIELDCATALOG.
  ENDLOOP.

ENDFORM.          " BSPL_TREE_COLUMN_AREA_ADJUST

*&---------------------------------------------------------------------*
*&      Form  BSPL_TREE_HEADER_DEFINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
FORM BSPL_TREE_HEADER_DEFINE
            CHANGING P_HIERARCHY_HEADER TYPE TREEV_HHDR.
  P_HIERARCHY_HEADER-HEADING = 'Bil/GuV-Position/Konto'(001).
  P_HIERARCHY_HEADER-TOOLTIP = 'Bil/GuV-Position/Konto'(001).
  P_HIERARCHY_HEADER-WIDTH = 60.
  P_HIERARCHY_HEADER-WIDTH_PIX = SPACE.
ENDFORM.                    " BSPL_TREE_HEADER_DEFINE

*&---------------------------------------------------------------------*
*&      Form  BSPL_TREE_COLUMN_TREE_FILL
*&---------------------------------------------------------------------*
FORM BSPL_TREE_COLUMN_TREE_FILL
                      TABLES PT_EXPID TYPE LVC_T_NKEY.
* local data declaration
  DATA: L_BPOS_KEY     TYPE LVC_NKEY,
        L_LEAF_KEY     TYPE LVC_NKEY,
        L_PARENT_KEY   TYPE LVC_NKEY,
        L_NOTASS_KEY   TYPE LVC_NKEY,
        L_NODE_TEXT    TYPE LVC_VALUE,
        LS_NODE_LAYOUT TYPE LVC_S_LAYN,
        LT_ITEM_LAYOUT TYPE LVC_T_LAYI.

  DATA: LS_BSPLDATA      LIKE ZBSPL_TREE_FIELDCAT,
        LS_BSPLDATA_SAVE LIKE ZBSPL_TREE_FIELDCAT,
        LS_NODE_SAKNR    TYPE TS_NODE_SAKNR,
        LS_NODE_ERGSL    TYPE TS_NODE_ERGSL.


* save different fields like CURTP, WAERS ...
  READ TABLE GT_BSPLDATA INDEX 1 INTO LS_BSPLDATA_SAVE.

  LOOP AT GT_RSTHIE INTO WA_RSTHIE.
*.. prepare node ID
    WRITE: WA_RSTHIE-PARENT TO L_PARENT_KEY
                               NO-ZERO
                               RIGHT-JUSTIFIED.
    SHIFT L_PARENT_KEY LEFT.

    CLEAR: LS_NODE_LAYOUT.
    L_NODE_TEXT = WA_RSTHIE-NAME.

    CASE WA_RSTHIE-TYPE.
      WHEN CON_TOP.
*.... top node
        CLEAR: LS_BSPLDATA.
        LS_NODE_LAYOUT-HIDDEN = 'X'.
        L_NODE_TEXT = T011T-VSTXT.
        LS_BSPLDATA-CURTP  = LS_BSPLDATA_SAVE-CURTP.
        LS_BSPLDATA-WAERS  = LS_BSPLDATA_SAVE-WAERS.
        LS_BSPLDATA-WAER2  = LS_BSPLDATA_SAVE-WAER2.

        PERFORM SOMA_NIVEIS USING WA_RSTHIE
                         CHANGING LS_BSPLDATA-REPVALV
                                  LS_BSPLDATA-REPVAL2V
                                  LS_BSPLDATA-REPVAL3V
                                  LS_BSPLDATA-ABSVAR
                                  LS_BSPLDATA-ABSVAR2.

        PERFORM BSPL_TREE_NODE_ADD USING    LS_BSPLDATA
                                            ' '
                                            L_NODE_TEXT
                                            LS_NODE_LAYOUT
                                            LT_ITEM_LAYOUT
                                   CHANGING L_PARENT_KEY.

      WHEN CON_BPOS.
*.... bs/p+l item
        CLEAR: LS_BSPLDATA.
        LS_NODE_ERGSL     = WA_RSTHIE-NAME.
        LS_BSPLDATA-ERGSL = LS_NODE_ERGSL-ERGSL.
        LS_BSPLDATA-CURTP  = LS_BSPLDATA_SAVE-CURTP.
        LS_BSPLDATA-WAERS  = LS_BSPLDATA_SAVE-WAERS.
        LS_BSPLDATA-WAER2  = LS_BSPLDATA_SAVE-WAER2.
        READ TABLE GT_ERGSL_TEXT WITH KEY ERGSL = LS_NODE_ERGSL-ERGSL.
        IF SY-SUBRC = 0.
          L_NODE_TEXT = GT_ERGSL_TEXT-TXT45.
        ENDIF.

        PERFORM SOMA_NIVEIS USING WA_RSTHIE
                         CHANGING LS_BSPLDATA-REPVALV
                                  LS_BSPLDATA-REPVAL2V
                                  LS_BSPLDATA-REPVAL3V
                                  LS_BSPLDATA-ABSVAR
                                  LS_BSPLDATA-ABSVAR2.

        IF WA_RSTHIE-TLEVEL = '02'.
*...... start with first column
          PERFORM BSPL_TREE_NODE_ADD USING   LS_BSPLDATA
                                              ' '
                                             L_NODE_TEXT
                                             LS_NODE_LAYOUT
                                             LT_ITEM_LAYOUT
                                    CHANGING L_BPOS_KEY.
          IF LS_NODE_ERGSL-ERGSL <> T011-ZUORD
         AND L_NOTASS_KEY IS INITIAL.
            APPEND L_BPOS_KEY TO PT_EXPID.
          ELSE.
            L_NOTASS_KEY = L_BPOS_KEY.
          ENDIF.
        ELSE.
*       IF GT_RSTHIE-TLEVEL >= '05'.    " bewirkt Anzeige bis
*         LS_NODE_LAYOUT-HIDDEN = 'X'.  " Stufe vier
*       ENDIF.
          PERFORM BSPL_TREE_NODE_ADD USING   LS_BSPLDATA
                                             L_PARENT_KEY
                                             L_NODE_TEXT
                                             LS_NODE_LAYOUT
                                             LT_ITEM_LAYOUT
                                    CHANGING L_BPOS_KEY.
        ENDIF.

      WHEN CON_FBER.
*.... function area
        SELECT SINGLE FKBTX INTO L_NODE_TEXT
                            FROM TFKBT WHERE SPRAS = SY-LANGU
                                         AND FKBER = WA_RSTHIE-NAME.
        CONCATENATE WA_RSTHIE-NAME
                    L_NODE_TEXT
               INTO L_NODE_TEXT SEPARATED BY ' '.

        LS_BSPLDATA-RFAREA = WA_RSTHIE-NAME.
        LS_BSPLDATA-CURTP  = LS_BSPLDATA_SAVE-CURTP.
        LS_BSPLDATA-WAERS  = LS_BSPLDATA_SAVE-WAERS.
        LS_BSPLDATA-WAER2  = LS_BSPLDATA_SAVE-WAER2.
        LS_BSPLDATA-RYEAR  = LS_BSPLDATA_SAVE-RYEAR.
        LS_BSPLDATA-POPER  = LS_BSPLDATA_SAVE-POPER.
        LS_NODE_LAYOUT-N_IMAGE   =  ICON_CONTROLLING_AREA.
        LS_NODE_LAYOUT-EXP_IMAGE =  ICON_CONTROLLING_AREA.

        PERFORM SOMA_NIVEIS USING WA_RSTHIE
                         CHANGING LS_BSPLDATA-REPVALV
                                  LS_BSPLDATA-REPVAL2V
                                  LS_BSPLDATA-REPVAL3V
                                  LS_BSPLDATA-ABSVAR
                                  LS_BSPLDATA-ABSVAR2.

        PERFORM BSPL_TREE_NODE_ADD USING   LS_BSPLDATA
                                           L_PARENT_KEY
                                           L_NODE_TEXT
                                           LS_NODE_LAYOUT
                                           LT_ITEM_LAYOUT
                                  CHANGING L_BPOS_KEY.

      WHEN CON_ACCT.
*.... account number
        LS_NODE_SAKNR = WA_RSTHIE-NAME.
        IF NOT ( IS_SETTINGS-ALTACCT IS INITIAL ).
*...... alternative account number is used
          LS_BSPLDATA-KTOP2 = LS_NODE_SAKNR-KTOPL.
          LS_BSPLDATA-ALTKT = LS_NODE_SAKNR-SAKNR.
        ELSEIF NOT ( T011-XERGS IS INITIAL ).
*...... group account number is used
          LS_BSPLDATA-KKTPL = LS_NODE_SAKNR-KTOPL.
          LS_BSPLDATA-BILKT = LS_NODE_SAKNR-SAKNR.
        ELSE.
*...... default normal accounts used
          LS_BSPLDATA-KTOPL = LS_NODE_SAKNR-KTOPL.
          LS_BSPLDATA-RACCT = LS_NODE_SAKNR-SAKNR.
        ENDIF.
        LS_BSPLDATA-CURTP  = LS_BSPLDATA_SAVE-CURTP.
        LS_BSPLDATA-WAERS  = LS_BSPLDATA_SAVE-WAERS.
        LS_BSPLDATA-WAER2  = LS_BSPLDATA_SAVE-WAER2.
        LS_BSPLDATA-RYEAR  = LS_BSPLDATA_SAVE-RYEAR.
        LS_BSPLDATA-POPER  = LS_BSPLDATA_SAVE-POPER.
        CLEAR: LS_BSPLDATA-RBUKRS.
        CALL FUNCTION 'READ_HAUPTBUCH_TEXT'
          EXPORTING
            KONTENPLAN     = LS_NODE_SAKNR-KTOPL
            SACHKONTO      = LS_NODE_SAKNR-SAKNR
            SPRACHE        = IS_SETTINGS-FS_LANGUAGE
          IMPORTING
            TEXT_WA        = SKAT
          EXCEPTIONS
            TEXT_NOT_FOUND = 1
            OTHERS         = 2.
        IF SY-SUBRC = 0.
*...... fix account number to relevant length
          PERFORM BSPL_ACCOUNT_LENGTH_FIX
                                 CHANGING LS_NODE_SAKNR.
          CONCATENATE LS_NODE_SAKNR-SAKNR
                      SKAT-TXT50
                 INTO L_NODE_TEXT SEPARATED BY ' '.
        ELSE.
*...... fix account number to relevant length
          PERFORM BSPL_ACCOUNT_LENGTH_FIX
                                 CHANGING LS_NODE_SAKNR.
          CONCATENATE LS_NODE_SAKNR-KTOPL
                      LS_NODE_SAKNR-SAKNR
                 INTO L_NODE_TEXT SEPARATED BY ' '.
        ENDIF.

        LS_NODE_LAYOUT-N_IMAGE   =  ICON_ACCOUNT_ASSIGNMENT.
        LS_NODE_LAYOUT-EXP_IMAGE =  ICON_ACCOUNT_ASSIGNMENT.
        PERFORM SOMA_NIVEIS USING WA_RSTHIE
                         CHANGING LS_BSPLDATA-REPVALV
                                  LS_BSPLDATA-REPVAL2V
                                  LS_BSPLDATA-REPVAL3V
                                  LS_BSPLDATA-ABSVAR
                                  LS_BSPLDATA-ABSVAR2.

        PERFORM BSPL_TREE_NODE_ADD USING   LS_BSPLDATA
                                           L_PARENT_KEY
                                           L_NODE_TEXT
                                           LS_NODE_LAYOUT
                                           LT_ITEM_LAYOUT
                                  CHANGING L_BPOS_KEY.

      WHEN CON_CCOD.
*.... company code
        SELECT SINGLE BUTXT INTO L_NODE_TEXT
                            FROM T001 WHERE BUKRS = WA_RSTHIE-NAME.
        CONCATENATE WA_RSTHIE-NAME
                    L_NODE_TEXT
               INTO L_NODE_TEXT SEPARATED BY ' '.

        LS_BSPLDATA-RBUKRS = WA_RSTHIE-NAME.
        LS_BSPLDATA-CURTP  = LS_BSPLDATA_SAVE-CURTP.
        LS_BSPLDATA-WAERS  = LS_BSPLDATA_SAVE-WAERS.
        LS_BSPLDATA-WAER2  = LS_BSPLDATA_SAVE-WAER2.
        LS_BSPLDATA-RYEAR  = LS_BSPLDATA_SAVE-RYEAR.
        LS_BSPLDATA-POPER  = LS_BSPLDATA_SAVE-POPER.
        LS_NODE_LAYOUT-N_IMAGE   =  ICON_COMPANY_CODE.
        LS_NODE_LAYOUT-EXP_IMAGE =  ICON_COMPANY_CODE.
        PERFORM SOMA_NIVEIS USING WA_RSTHIE
              CHANGING LS_BSPLDATA-REPVALV
                       LS_BSPLDATA-REPVAL2V
                       LS_BSPLDATA-REPVAL3V
                       LS_BSPLDATA-ABSVAR
                       LS_BSPLDATA-ABSVAR2.

        PERFORM BSPL_TREE_NODE_ADD USING   LS_BSPLDATA
                                           L_PARENT_KEY
                                           L_NODE_TEXT
                                           LS_NODE_LAYOUT
                                           LT_ITEM_LAYOUT
                                  CHANGING L_BPOS_KEY.

      WHEN CON_LEAF.
*.... assigned accounts  (BUSA's)
        SELECT SINGLE GTEXT INTO L_NODE_TEXT
                            FROM TGSBT WHERE SPRAS = SY-LANGU
                                         AND GSBER = WA_RSTHIE-NAME.

        READ TABLE GT_BSPLDATA WITH KEY ID = WA_RSTHIE-ID
                                             BINARY SEARCH.
        IF SY-SUBRC <> 0.
          CLEAR: GT_BSPLDATA.
        ENDIF.

        IF L_NODE_TEXT = CON_RESULT.
          L_NODE_TEXT = TEXT-003.
          LS_NODE_LAYOUT-N_IMAGE   =  ICON_SPACE.
          LS_NODE_LAYOUT-EXP_IMAGE =  ICON_SPACE.
          CLEAR: GT_BSPLDATA-RACCT,
                 GT_BSPLDATA-BILKT,
                 GT_BSPLDATA-ALTKT.
        ELSE.
          IF L_NODE_TEXT IS INITIAL.
            L_NODE_TEXT = TEXT-002.
          ENDIF.
          CONCATENATE WA_RSTHIE-NAME
                      L_NODE_TEXT
                 INTO L_NODE_TEXT SEPARATED BY ' '.
          LS_NODE_LAYOUT-N_IMAGE   =  ICON_BUSINESS_AREA.
          LS_NODE_LAYOUT-EXP_IMAGE =  ICON_BUSINESS_AREA.
        ENDIF.
        PERFORM BSPL_TREE_NODE_ADD USING GT_BSPLDATA
                                         L_BPOS_KEY
                                         L_NODE_TEXT
                                         LS_NODE_LAYOUT
                                         LT_ITEM_LAYOUT
                                CHANGING L_LEAF_KEY.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " BSPL_TREE_COLUMN_TREE_FILL

*&---------------------------------------------------------------------*
*&      Form  BSPL_TREE_NODE_ADD
*&---------------------------------------------------------------------*
FORM BSPL_TREE_NODE_ADD USING P_BSPLDATA LIKE ZBSPL_TREE_FIELDCAT
                              P_RELAT_KEY     TYPE LVC_NKEY
                              P_NODE_TEXT     TYPE LVC_VALUE
                              PS_NODE_LAYOUT  TYPE LVC_S_LAYN
                              PT_ITEM_LAYOUT  TYPE LVC_T_LAYI
                     CHANGING P_NEW_KEY.

  CALL METHOD ALV_TREE_CONTROL->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = P_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = P_NODE_TEXT
      IS_OUTTAB_LINE   = P_BSPLDATA
      IS_NODE_LAYOUT   = PS_NODE_LAYOUT
      IT_ITEM_LAYOUT   = PT_ITEM_LAYOUT
    IMPORTING
      E_NEW_NODE_KEY   = P_NEW_KEY.

ENDFORM.                    " BSPL_TREE_NODE_ADD

*&---------------------------------------------------------------------*
*&      Form  SOMA_NIVEIS
*&---------------------------------------------------------------------*
*       Soma níveis para calculo e retornar taxa
*----------------------------------------------------------------------*
FORM SOMA_NIVEIS USING P_RSTHIE  TYPE RSTHIE
              CHANGING P_REPVALV  TYPE ZDE_VAR_REPVALF
                       P_REPVAL2V TYPE ZDE_VAR_COMPVALF
                       P_REPVAL3V TYPE ZDE_VAR_COMPVALF
                       P_ABSVAR   TYPE BSPL_VARIANCE_ABSOLUTE
                       P_ABSVAR2  TYPE BSPL_VARIANCE_ABSOLUTE.

  DATA: LC_RSTHIE   TYPE RSTHIE,
        "1ª Moeda
        LC_REPVAL	  TYPE BSPL_REPVAL,
        LC_COMPVAL  TYPE BSPL_COMPVAL,
        LC_ABSVAR	  TYPE BSPL_VARIANCE_ABSOLUTE,

        "2ª Moeda
        LC_REPVAL2  TYPE BSPL_REPVAL,
        LC_COMPVAL2	TYPE BSPL_COMPVAL,
        LC_ABSVAR2  TYPE BSPL_VARIANCE_ABSOLUTE.

  DATA: VP_REPVALV(15)  TYPE P DECIMALS 10,
        VP_REPVALV2(15) TYPE P DECIMALS 10,
        VP_REPVALV3(15) TYPE P DECIMALS 10.

  DATA: GVP_REPVALV(15)  TYPE P DECIMALS 4,
        GVP_REPVALV2(15) TYPE P DECIMALS 4,
        GVP_REPVALV3(15) TYPE P DECIMALS 4.

  LC_REPVAL   = 0.
  LC_COMPVAL  = 0.
  LC_ABSVAR   = 0.
  LC_REPVAL2  = 0.
  LC_COMPVAL2 = 0.
  LC_ABSVAR2  = 0.

  LOOP AT GT_RSTHIE INTO LC_RSTHIE WHERE PARENT EQ P_RSTHIE-ID.
    IF LC_RSTHIE-TYPE EQ CON_LEAF.
      READ TABLE GT_BSPLDATA WITH KEY ID = LC_RSTHIE-ID BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LC_REPVAL   = LC_REPVAL   + GT_BSPLDATA-REPVAL.
        LC_COMPVAL  = LC_COMPVAL  + GT_BSPLDATA-COMPVAL.
        LC_ABSVAR   = LC_ABSVAR   + GT_BSPLDATA-ABSVAR.
        LC_REPVAL2  = LC_REPVAL2  + GT_BSPLDATA-REPVAL2.
        LC_COMPVAL2 = LC_COMPVAL2 + GT_BSPLDATA-COMPVAL2.
        LC_ABSVAR2  = LC_ABSVAR2  + GT_BSPLDATA-ABSVAR2.
      ENDIF.
    ELSE.
      PERFORM SOMA_NIVEIS_2 USING LC_RSTHIE CHANGING LC_REPVAL LC_COMPVAL LC_ABSVAR LC_REPVAL2 LC_COMPVAL2 LC_ABSVAR2.
    ENDIF.
  ENDLOOP.

  P_ABSVAR  = LC_REPVAL  - LC_COMPVAL.
  P_ABSVAR2 = LC_REPVAL2 - LC_COMPVAL2.

  "P_ABSVAR  = ABS( P_ABSVAR  ).
  "P_ABSVAR2 = ABS( P_ABSVAR2 ).

  IF LC_REPVAL2 NE 0.
    VP_REPVALV = LC_REPVAL / LC_REPVAL2.
  ELSE.
    P_REPVALV = 0.
  ENDIF.

  IF LC_ABSVAR2 NE 0.
    VP_REPVALV3 = LC_ABSVAR / LC_ABSVAR2.
  ELSE.
    VP_REPVALV3 = 0.
  ENDIF.

  IF LC_COMPVAL2 NE 0.
    VP_REPVALV2 = LC_COMPVAL / LC_COMPVAL2.
  ELSE.
    VP_REPVALV2 = 0.
  ENDIF.

  CALL FUNCTION 'ROUND'
    EXPORTING
      DECIMALS      = 4
      INPUT         = VP_REPVALV
    IMPORTING
      OUTPUT        = GVP_REPVALV
    EXCEPTIONS
      INPUT_INVALID = 1
      OVERFLOW      = 2
      TYPE_INVALID  = 3
      OTHERS        = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'ROUND'
    EXPORTING
      DECIMALS      = 4
      INPUT         = VP_REPVALV2
    IMPORTING
      OUTPUT        = GVP_REPVALV2
    EXCEPTIONS
      INPUT_INVALID = 1
      OVERFLOW      = 2
      TYPE_INVALID  = 3
      OTHERS        = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'ROUND'
    EXPORTING
      DECIMALS      = 4
      INPUT         = VP_REPVALV3
    IMPORTING
      OUTPUT        = GVP_REPVALV3
    EXCEPTIONS
      INPUT_INVALID = 1
      OVERFLOW      = 2
      TYPE_INVALID  = 3
      OTHERS        = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  WRITE: GVP_REPVALV  TO P_REPVALV,
         GVP_REPVALV2 TO P_REPVAL2V,
         GVP_REPVALV3 TO P_REPVAL3V.

ENDFORM.                    "SOMA_NIVEIS


*&---------------------------------------------------------------------*
*&      Form  SOMA_NIVEIS_2
*&---------------------------------------------------------------------*
*       Somatória de Valores para calculo de taxa
*----------------------------------------------------------------------*
FORM SOMA_NIVEIS_2 USING P_RSTHIE   TYPE RSTHIE
                CHANGING P_REPVAL   TYPE BSPL_REPVAL
                         P_COMPVAL  TYPE BSPL_COMPVAL
                         P_ABSVAR   TYPE BSPL_VARIANCE_ABSOLUTE
                         P_REPVAL2  TYPE BSPL_REPVAL
                         P_COMPVAL2 TYPE BSPL_COMPVAL
                         P_ABSVAR2  TYPE BSPL_VARIANCE_ABSOLUTE.

  DATA: LC_RSTHIE TYPE RSTHIE.

  LOOP AT GT_RSTHIE INTO LC_RSTHIE WHERE PARENT EQ P_RSTHIE-ID.
    IF LC_RSTHIE-TYPE EQ CON_LEAF.
      READ TABLE GT_BSPLDATA WITH KEY ID = LC_RSTHIE-ID BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        P_REPVAL   = P_REPVAL   + GT_BSPLDATA-REPVAL.
        P_COMPVAL  = P_COMPVAL  + GT_BSPLDATA-COMPVAL.
        P_ABSVAR   = P_ABSVAR   + GT_BSPLDATA-ABSVAR.
        P_REPVAL2  = P_REPVAL2  + GT_BSPLDATA-REPVAL2.
        P_COMPVAL2 = P_COMPVAL2 + GT_BSPLDATA-COMPVAL2.
        P_ABSVAR2  = P_ABSVAR2  + GT_BSPLDATA-ABSVAR2.
      ENDIF.
    ELSE.
      PERFORM SOMA_NIVEIS_2 USING LC_RSTHIE CHANGING P_REPVAL P_COMPVAL P_ABSVAR P_REPVAL2 P_COMPVAL2 P_ABSVAR2.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "SOMA_NIVEIS
*&---------------------------------------------------------------------*
*&      Form  change_toolbar
*&---------------------------------------------------------------------*
FORM CHANGE_TOOLBAR.                        "new since "n1616718
*----------------------------------------------------------------------*
  DATA LR_TOOLBAR             TYPE REF TO CL_GUI_TOOLBAR.
  DATA LD_QUICKINFO           TYPE ICONQUICK.
*----------------------------------------------------------------------*
* Get toolbar
  CALL METHOD ALV_TREE_CONTROL->GET_TOOLBAR_OBJECT
    IMPORTING
      ER_TOOLBAR = LR_TOOLBAR.

* Add separator
  CALL METHOD LR_TOOLBAR->ADD_BUTTON
    EXPORTING
      FCODE     = SPACE
      ICON      = SPACE
      TEXT      = SPACE
      QUICKINFO = SPACE
      BUTN_TYPE = 3.

* Add function to expand all FSI nodes
  CALL METHOD LR_TOOLBAR->ADD_BUTTON
    EXPORTING
      FCODE     = 'EXP_FSI'
      ICON      = ICON_EXPAND_ALL
      TEXT      = 'Bilanz/GuV-Positionen'(006)
      QUICKINFO = LD_QUICKINFO
      BUTN_TYPE = 0.

ENDFORM.                    "change_toolbar

* local classes
*---------------------------------------------------------------------*
*       CLASS LCL_TREE_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_TREE_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_TOP_OF_PAGE.
* local adata declaration
    DATA: LT_LIST_COMMENTARY TYPE SLIS_T_LISTHEADER.
    DATA: LS_LIST_COMMENTARY TYPE SLIS_LISTHEADER.

    IF LT_LIST_COMMENTARY[] IS INITIAL.
*.. check if list commentary is filled by caller
      IF NOT IT_LIST_COMMENTARY[] IS INITIAL.
*.... yes, take it over
        LT_LIST_COMMENTARY[] = IT_LIST_COMMENTARY[].
      ELSE.
*.... no, then set default list commentary
        LS_LIST_COMMENTARY-TYP  = 'H'.
        LS_LIST_COMMENTARY-INFO = T011T-VSTXT.
        APPEND LS_LIST_COMMENTARY
            TO LT_LIST_COMMENTARY.
      ENDIF.
    ENDIF.

* fill standard page header
    BHDGD-INIFL = CON_0.
    BHDGD-LINES = SY-LINSZ.
    BHDGD-UNAME = SY-UNAME.
    BHDGD-REPID = IS_SETTINGS-REPID.
    READ TABLE LT_LIST_COMMENTARY INDEX 1
                                   INTO LS_LIST_COMMENTARY.
    BHDGD-LINE1 = LS_LIST_COMMENTARY-INFO.
    BHDGD-LINE2 = IS_SETTINGS-ALLGLINE.
    BHDGD-BUKRS = IS_SETTINGS-BUKRS.
    BHDGD-DOMAI = 'BUKRS'.
    BHDGD-START_PAGNO = IS_SETTINGS-PAGE_NO.

* write standard page header
    PERFORM BATCH-HEADING1 IN PROGRAM RSBTCHH0 USING BHDGD.

  ENDMETHOD.                    "HANDLE_TOP_OF_PAGE

*----------------------------------------------------------------------*
  METHOD HANDLE_AFTER_USER_COMMAND.         "new since "n1616718
*----------------------------------------------------------------------*
    DATA LT_NODES                   TYPE LVC_T_NKEY.
    DATA LD_NODE                    TYPE LVC_NKEY.
    DATA LS_RSTHIE                  TYPE RSTHIE.
    DATA LT_HELP_NODES              TYPE LVC_T_NKEY.
    DATA LT_EXPAND_NODES            TYPE LVC_T_NKEY.
*----------------------------------------------------------------------*
    CASE UCOMM.
* Expand all FS items
      WHEN 'EXP_FSI'.
* Get currently selected node
        CALL METHOD ALV_TREE_CONTROL->GET_SELECTED_NODES
          CHANGING
            CT_SELECTED_NODES = LT_NODES
          EXCEPTIONS
            OTHERS            = 0.

* No node selected
        IF LT_NODES IS INITIAL.
* Get selected item
          CALL METHOD ALV_TREE_CONTROL->GET_SELECTED_ITEM
            IMPORTING
              E_SELECTED_NODE = LD_NODE
            EXCEPTIONS
              OTHERS          = 0.
* Items was selected >> expand subtree
          IF LD_NODE IS NOT INITIAL.
            APPEND LD_NODE TO LT_NODES.
* No node was selected >> expand all subtrees
* (default logic expands first level nodes)
          ELSE.
            CALL METHOD ALV_TREE_CONTROL->GET_EXPANDED_NODES
              CHANGING
                CT_EXPANDED_NODES = LT_NODES
              EXCEPTIONS
                OTHERS            = 0.
          ENDIF.
        ENDIF.

* Go over selected nodes/items/expanded nodes
        LOOP AT LT_NODES
          INTO  LD_NODE.
* Get subtree for current node
          CALL METHOD ALV_TREE_CONTROL->GET_SUBTREE
            EXPORTING
              I_NODE_KEY       = LD_NODE
            IMPORTING
              ET_SUBTREE_NODES = LT_HELP_NODES.
* Remember all subtrees
          APPEND LINES OF LT_HELP_NODES
            TO LT_EXPAND_NODES.
        ENDLOOP.

* Free memory
        CLEAR LT_NODES.
        CLEAR LT_HELP_NODES.
* Sort nodes to be expanded in descending order to prepare for delete
        SORT LT_EXPAND_NODES DESCENDING.
* Delete adjacent duplicates to prepare for delete
        DELETE ADJACENT DUPLICATES FROM LT_NODES
          COMPARING ALL FIELDS.

* Go over nodes which may have to be expanded (bottom up)
        LOOP AT LT_EXPAND_NODES
          INTO  LD_NODE.
* Read corresponding detail info
          READ TABLE GT_RSTHIE
            INTO LS_RSTHIE
            INDEX LD_NODE.
* Node could not be found
          IF SY-SUBRC <> 0
* Node is not FS item
          OR LS_RSTHIE-TYPE <> CON_BPOS.
* >> Node should not be expanded
            DELETE LT_EXPAND_NODES.
          ENDIF.
        ENDLOOP.

* Expand all remaining nodes. These should be FS items.
        CALL METHOD ALV_TREE_CONTROL->EXPAND_NODES
          EXPORTING
            IT_NODE_KEY = LT_EXPAND_NODES
          EXCEPTIONS
            OTHERS      = 0.
    ENDCASE.
  ENDMETHOD.                    "handle_after_user_command
ENDCLASS.                    "LCL_TREE_EVENT_RECEIVER IMPLEMENTATION
