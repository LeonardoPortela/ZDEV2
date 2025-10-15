*----------------------------------------------------------------------*
***INCLUDE MZENTREMESSA0002 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_NOTAS_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
TYPE-POOLS ICON.

*&---------------------------------------------------------------------*
*&      Form  pesquisar_notas_remessa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PESQUISAR_NOTAS_REMESSA .

  DATA: BEGIN OF GS_CENTRO,
          SIGN      TYPE C,
          OPTION(2) TYPE C,
          LOW       TYPE C LENGTH 4,
          HIGH      TYPE C LENGTH 4,
        END OF GS_CENTRO.

  DATA: T_SADRVB  TYPE TABLE OF SADRVB INITIAL SIZE 0 WITH HEADER LINE,
        T_VBPAVB  TYPE TABLE OF VBPAVB INITIAL SIZE 0 WITH HEADER LINE,
        WA_VBPAVB TYPE VBPAVB,
        WA_LFA1   TYPE LFA1,
        VG_CANCEL TYPE CHAR1,
        WA_VBRP   TYPE VBRP,
        WA_VBKD   TYPE VBKD,
        WA_VBAK   TYPE VBAK,
        GT_CENTRO LIKE TABLE OF GS_CENTRO,
        VG_STATUS TYPE ZSTATUS.

  CLEAR: IT_NOTAS[], IT_ITENS[], IT_DISP[].

  IF P_CENTRO IS NOT INITIAL.
    GS_CENTRO-SIGN   = 'I'.
    GS_CENTRO-OPTION = 'EQ'.
    GS_CENTRO-LOW    = P_CENTRO.
    GS_CENTRO-HIGH   = P_CENTRO.
    "move-corresponding gs_centro to gt_centro.
    APPEND GS_CENTRO TO GT_CENTRO.
  ENDIF.

  IF NOT P_VBELN IS INITIAL.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_ITENS
      FROM J_1BNFLIN
     WHERE REFTYP EQ 'BI'
       AND REFKEY EQ P_VBELN.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_DISP
      FROM J_1BNFDOC AS NF
      INNER JOIN J_1BNFE_ACTIVE AS AC ON AC~DOCNUM EQ NF~DOCNUM
      FOR ALL ENTRIES IN IT_ITENS
     WHERE NF~DOCNUM EQ IT_ITENS-DOCNUM
       AND NF~CANCEL EQ VG_CANCEL
       AND NF~DIRECT EQ '2'
       AND AC~DOCSTA EQ '1'
       AND AC~CANCEL NE 'X'
       AND NOT EXISTS ( SELECT * FROM ZSDT_ENTRADA_REM AS EN WHERE EN~DOCNUM EQ NF~DOCNUM ).

    IF P_TODOS IS INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_VINC
        FROM J_1BNFDOC AS NF
         FOR ALL ENTRIES IN IT_ITENS
       WHERE NF~DOCNUM EQ IT_ITENS-DOCNUM
         AND EXISTS ( SELECT * FROM ZSDT_ENTRADA_REM AS EN WHERE EN~DOCNUM EQ NF~DOCNUM ).
    ELSE.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_VINC
        FROM J_1BNFDOC AS NF
         FOR ALL ENTRIES IN IT_ITENS
       WHERE NF~DOCNUM EQ IT_ITENS-DOCNUM
         AND EXISTS ( SELECT *
                        FROM ZSDT_ENTRADA_REM AS EN
                       WHERE EN~DOCNUM EQ NF~DOCNUM
                         AND EN~TP_STATUS EQ VG_STATUS ).
    ENDIF.

  ELSE.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_DISP
      FROM J_1BNFDOC AS NF
      INNER JOIN J_1BNFE_ACTIVE AS AC ON AC~DOCNUM EQ NF~DOCNUM
     WHERE NF~DOCNUM IN P_DOCNUM
       AND NF~DOCDAT IN P_DOCDAT
       AND NF~NFENUM IN P_NFENUM
       AND NF~MODEL  IN P_MODELO
       AND NF~BRANCH IN GT_CENTRO
       AND NF~CANCEL EQ VG_CANCEL
       AND NF~DIRECT EQ '2'
       AND AC~DOCSTA EQ '1'
       AND AC~CANCEL NE 'X'
       AND NOT EXISTS ( SELECT * FROM ZSDT_ENTRADA_REM AS EN WHERE EN~DOCNUM EQ NF~DOCNUM ).

    IF P_TODOS IS INITIAL.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_VINC
        FROM J_1BNFDOC AS NF
       WHERE NF~DOCNUM IN P_DOCNUM
         AND NF~DOCDAT IN P_DOCDAT
         AND NF~NFENUM IN P_NFENUM
         AND NF~MODEL  IN P_MODELO
         AND NF~BRANCH IN GT_CENTRO
         AND EXISTS ( SELECT * FROM ZSDT_ENTRADA_REM AS EN WHERE EN~DOCNUM EQ NF~DOCNUM ).

    ELSE.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_VINC
        FROM J_1BNFDOC AS NF
       WHERE NF~DOCNUM IN P_DOCNUM
         AND NF~DOCDAT IN P_DOCDAT
         AND NF~NFENUM IN P_NFENUM
         AND NF~MODEL  IN P_MODELO
         AND NF~BRANCH IN GT_CENTRO
         AND EXISTS ( SELECT *
                        FROM ZSDT_ENTRADA_REM AS EN
                       WHERE EN~DOCNUM    EQ NF~DOCNUM
                         AND EN~TP_STATUS EQ VG_STATUS ).

    ENDIF.

  ENDIF.

  LOOP AT IT_DISP INTO WA_NOTAS.

    SELECT SINGLE * INTO WA_ITENS
      FROM J_1BNFLIN
     WHERE DOCNUM EQ WA_NOTAS-DOCNUM
       AND REFTYP EQ 'BI'.

    IF SY-SUBRC EQ 0.

      WA_NOTAS-VBELN = WA_ITENS-REFKEY(10).

      "Fatura
      SELECT SINGLE * INTO WA_VBRP
        FROM VBRP
       WHERE VBELN EQ WA_NOTAS-VBELN.

      "Ordem de Venda: dados comerciais
      SELECT SINGLE * INTO WA_VBKD
        FROM VBKD
       WHERE VBELN EQ WA_VBRP-AUBEL.

      IF WA_VBKD-INCO1 EQ 'FOB'.
        WA_NOTAS-VBKD_INCO1 = WA_VBKD-INCO1.
      ENDIF.

      SELECT SINGLE * INTO WA_VBAK
        FROM VBAK
       WHERE VBELN EQ WA_VBRP-AUBEL.

      IF WA_VBAK-AUART NE 'ZRFL' AND WA_VBAK-AUART NE 'ZRDC' AND WA_VBAK-AUART NE 'ZIND'.
        CONTINUE.
      ENDIF.

      WA_NOTAS-VBELN_S = WA_VBRP-VGBEL.

      CLEAR: T_VBPAVB[], T_SADRVB[].

      CALL FUNCTION 'SD_PARTNER_READ'
        EXPORTING
          F_VBELN  = WA_NOTAS-VBELN
          OBJECT   = 'VBPA'
        TABLES
          I_XVBADR = T_SADRVB
          I_XVBPA  = T_VBPAVB.

      DELETE T_VBPAVB WHERE PARVW NE 'Z1'.

      IF NOT T_VBPAVB[] IS INITIAL.

        READ TABLE T_VBPAVB INTO WA_VBPAVB INDEX 1.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            P_PARCEIRO   = WA_VBPAVB-LIFNR
            P_PARTYPE    = 'V'
          CHANGING
            WA_INFO_PART = WA_LFA1.

        WA_NOTAS-LIFNR    = WA_VBPAVB-LIFNR.
        WA_NOTAS-TERMINAL = WA_LFA1-NAME1.

        CLEAR WA_LFA1.

        CLEAR: WA_DEPARA.

*        SELECT SINGLE * INTO wa_depara
*          FROM zsdt_depara_depo
*         WHERE werks EQ wa_notas-branch
*           AND lifnr EQ wa_notas-lifnr.

        DATA(_OPERA) = 'RF'.
        IF  WA_VBAK-AUART  EQ 'ZIND'.
          _OPERA = 'RI'.
        ENDIF.
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - INICIO
*        CALL FUNCTION 'Z_BUSCA_DEPARA'
*          EXPORTING
*            I_WERKS          = WA_NOTAS-BRANCH
*            I_LIFNR          = WA_NOTAS-LIFNR
*            I_OPERA          = _OPERA
*          IMPORTING
*            ZSDT_DEPARA_DEPO = WA_DEPARA.

        ZCL_DEPARA_CENTRO_FIXO_VIRTUAL=>GET_DADOS_DEPARA(
            EXPORTING
              I_WERKS       = WA_NOTAS-BRANCH
              I_LIFNR       = WA_NOTAS-LIFNR
              I_OPERACAO    = _OPERA
            IMPORTING
             E_SINGLE_DEPARA          = WA_DEPARA  ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM

        IF SY-SUBRC EQ 0.

          SELECT SINGLE * INTO WA_INFORV
            FROM ZSDT_DEPARA_CENV
           WHERE WERKS EQ WA_DEPARA-WERKS_V.

          IF SY-SUBRC EQ 0.
            WA_NOTAS-WERKS_V = WA_DEPARA-WERKS_V.
            WA_NOTAS-LGORT   = WA_DEPARA-LGORT.
            WA_NOTAS-VKORG   = WA_INFORV-VKORG.
            WA_NOTAS-VTWEG   = WA_INFORV-VTWEG.
            WA_NOTAS-SPART   = WA_INFORV-SPART.
            APPEND WA_NOTAS TO IT_NOTAS.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.

  ENDLOOP.

  CLEAR IT_DISP[].
  MOVE IT_VINC[] TO IT_DISP[].
  CLEAR IT_VINC[].

  LOOP AT IT_DISP INTO WA_NOTAS.

    SELECT SINGLE * INTO WA_ITENS
      FROM J_1BNFLIN
     WHERE DOCNUM EQ WA_NOTAS-DOCNUM
       AND REFTYP EQ 'BI'.

    IF SY-SUBRC EQ 0.

      WA_NOTAS-VBELN = WA_ITENS-REFKEY(10).

      CLEAR: T_VBPAVB[], T_SADRVB[].

      CALL FUNCTION 'SD_PARTNER_READ'
        EXPORTING
          F_VBELN  = WA_NOTAS-VBELN
          OBJECT   = 'VBPA'
        TABLES
          I_XVBADR = T_SADRVB
          I_XVBPA  = T_VBPAVB.

      DELETE T_VBPAVB WHERE PARVW NE 'Z1'.

      IF NOT T_VBPAVB[] IS INITIAL.

        READ TABLE T_VBPAVB INTO WA_VBPAVB INDEX 1.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            P_PARCEIRO   = WA_VBPAVB-LIFNR
            P_PARTYPE    = 'V'
          CHANGING
            WA_INFO_PART = WA_LFA1.

        WA_NOTAS-LIFNR    = WA_VBPAVB-LIFNR.
        WA_NOTAS-TERMINAL = WA_LFA1-NAME1.

        CLEAR WA_LFA1.

        SELECT SINGLE * INTO WA_ENTRADA
          FROM ZSDT_ENTRADA_REM
         WHERE DOCNUM EQ WA_NOTAS-DOCNUM.

        IF SY-SUBRC EQ 0.
          WA_NOTAS-STATUS      = WA_ENTRADA-TP_STATUS.
          WA_NOTAS-DT_CHEGADA  = WA_ENTRADA-DT_CHEGADA.
          WA_NOTAS-QT_CHEGADA  = WA_ENTRADA-QT_CHEGADA.
          WA_NOTAS-VBELN_ORD_E = WA_ENTRADA-VBELN_ORD_E.
          WA_NOTAS-VBELN_E     = WA_ENTRADA-VBELN_E.
          WA_NOTAS-TP_STATUS   = WA_ENTRADA-TP_STATUS.
        ENDIF.

        IF WA_NOTAS-STATUS IS INITIAL.
          WA_NOTAS-ICONE = ICON_UNLOCKED.
        ELSE.
          WA_NOTAS-ICONE = ICON_LOCKED.
        ENDIF.

        CLEAR: WA_DEPARA.
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255

*        SELECT SINGLE * INTO WA_DEPARA
*          FROM ZSDT_DEPARA_DEPO
*         WHERE WERKS EQ WA_NOTAS-BRANCH
*           AND LIFNR EQ WA_NOTAS-LIFNR.

         ZCL_DEPARA_CENTRO_FIXO_VIRTUAL=>GET_DADOS_DEPARA(
            EXPORTING
              I_WERKS       = WA_NOTAS-BRANCH
              I_LIFNR       = WA_NOTAS-LIFNR
            IMPORTING
             E_SINGLE_DEPARA          = WA_DEPARA  ).

**Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255

        IF SY-SUBRC EQ 0.
          WA_NOTAS-WERKS_V = WA_DEPARA-WERKS_V.
          WA_NOTAS-LGORT   = WA_DEPARA-LGORT.
        ENDIF.

        APPEND WA_NOTAS TO IT_VINC.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " PESQUISAR_NOTAS_REMESSA

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_NOTAS_LIVRE'. DO NOT CHANGE THIS L
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TAB_NOTAS_LIVRE_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_NOTAS LINES TAB_NOTAS_LIVRE-LINES.
ENDMODULE.                    "TAB_NOTAS_LIVRE_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_NOTAS_LIVRE'. DO NOT CHANGE THIS LIN
*&SPWIZARD: MARK TABLE
MODULE TAB_NOTAS_LIVRE_MARK INPUT.
  DATA: G_TAB_NOTAS_LIVRE_WA2 LIKE LINE OF IT_NOTAS.
  IF TAB_NOTAS_LIVRE-LINE_SEL_MODE = 1
  AND IT_NOTAS-MARC = 'X'.
    LOOP AT IT_NOTAS INTO G_TAB_NOTAS_LIVRE_WA2
      WHERE MARC = 'X'.
      G_TAB_NOTAS_LIVRE_WA2-MARC = ''.
      MODIFY IT_NOTAS
        FROM G_TAB_NOTAS_LIVRE_WA2
        TRANSPORTING MARC.
    ENDLOOP.
  ENDIF.
  MODIFY IT_NOTAS
    INDEX TAB_NOTAS_LIVRE-CURRENT_LINE
    TRANSPORTING MARC.
ENDMODULE.                    "TAB_NOTAS_LIVRE_MARK INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0002 INPUT.
  CASE OK_CODE.
    WHEN 'BTVINCU'.
      PERFORM DISP_SELECIONA USING 'X'.
    WHEN 'BTDESVI'.
      PERFORM DISP_SELECIONA USING ' '.
    WHEN 'BTVALL'.
      PERFORM VINC_SELECIONA USING 'X'.
    WHEN 'BTDALL'.
      PERFORM VINC_SELECIONA USING ' '.
    WHEN 'VINC'.
      PERFORM VINCULAR_NOTA.
      CLEAR: OK_CODE.
    WHEN 'DESV'.
      PERFORM DESVINCULAR_NOTA.
      CLEAR: OK_CODE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0002  INPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_NOTAS_VINC'. DO NOT CHANGE THIS LI
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TAB_NOTAS_VINC_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_VINC LINES TAB_NOTAS_VINC-LINES.
ENDMODULE.                    "TAB_NOTAS_VINC_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_NOTAS_VINC'. DO NOT CHANGE THIS LINE
*&SPWIZARD: MARK TABLE
MODULE TAB_NOTAS_VINC_MARK INPUT.
  DATA: G_TAB_NOTAS_VINC_WA2 LIKE LINE OF IT_VINC.
  IF TAB_NOTAS_VINC-LINE_SEL_MODE = 1
  AND IT_VINC-MARC = 'X'.
    LOOP AT IT_VINC INTO G_TAB_NOTAS_VINC_WA2
      WHERE MARC = 'X'.
      G_TAB_NOTAS_VINC_WA2-MARC = ''.
      MODIFY IT_VINC
        FROM G_TAB_NOTAS_VINC_WA2
        TRANSPORTING MARC.
    ENDLOOP.
  ENDIF.
  MODIFY IT_VINC
    INDEX TAB_NOTAS_VINC-CURRENT_LINE
    TRANSPORTING MARC.
ENDMODULE.                    "TAB_NOTAS_VINC_MARK INPUT

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VINCULAR_NOTA .

  READ TABLE IT_NOTAS WITH KEY MARC = 'X'.

  IF SY-SUBRC EQ 0.
    VG_ALTEROU = 'X'.
    LOOP AT IT_NOTAS INTO WA_NOTAS WHERE MARC EQ 'X'.
      WA_NOTAS-ICONE = ICON_UNLOCKED.
      CLEAR: WA_NOTAS-MARC .
      APPEND WA_NOTAS TO IT_VINC.
      DELETE IT_DESVIN WHERE DOCNUM EQ WA_NOTAS-DOCNUM.
    ENDLOOP.
    DELETE IT_NOTAS WHERE MARC EQ 'X'.
  ELSE.
    MESSAGE 'Favor selecionar notas para vinculação!' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " VINCULAR_NOTA

*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DESVINCULAR_NOTA .

  READ TABLE IT_VINC WITH KEY MARC = 'X'.
  IF SY-SUBRC EQ 0.
    LOOP AT IT_VINC INTO WA_NOTAS WHERE MARC EQ 'X'.
      IF WA_NOTAS-STATUS IS INITIAL.
        WA_NOTAS-ICONE = ICON_UNLOCKED.
        CLEAR: WA_NOTAS-MARC .
        APPEND WA_NOTAS TO IT_NOTAS.
        APPEND WA_NOTAS TO IT_DESVIN.
        VG_ALTEROU = 'X'.
      ENDIF.
    ENDLOOP.
    DELETE IT_VINC WHERE MARC EQ 'X'.
  ELSE.
    MESSAGE 'Favor selecionar notas para desvinculação!' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " DESVINCULAR_NOTA

*&---------------------------------------------------------------------*
*&      Module  ALTERA_IT_VIN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTERA_IT_VIN INPUT.

  MODIFY IT_VINC
    INDEX TAB_NOTAS_VINC-CURRENT_LINE
    TRANSPORTING DT_CHEGADA QT_CHEGADA.

  VG_ALTEROU = 'X'.

ENDMODULE.                 " ALTERA_IT_VIN  INPUT

*&---------------------------------------------------------------------*
*&      Form  DISP_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISP_SELECIONA  USING  TAG TYPE C.

  LOOP AT IT_NOTAS INTO WA_NOTAS WHERE MARC NE TAG.
    WA_NOTAS-MARC = TAG.
    MODIFY IT_NOTAS INDEX SY-TABIX FROM WA_NOTAS TRANSPORTING MARC.
  ENDLOOP.

ENDFORM.                    " DISP_SELECIONA

*&---------------------------------------------------------------------*
*&      Form  VINC_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VINC_SELECIONA  USING  TAG TYPE C.

  LOOP AT IT_VINC INTO WA_NOTAS WHERE MARC NE TAG.
    WA_NOTAS-MARC = TAG.
    MODIFY IT_VINC INDEX SY-TABIX FROM WA_NOTAS TRANSPORTING MARC.
  ENDLOOP.

ENDFORM.                    " VINC_SELECIONA
