*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDO_4000 .
*----------------------------------------------------------------------*

DATA: VG_NOME_EMPRESA TYPE BUTXT,
      VG_NOME_CENTRO  TYPE NAME1.

*&---------------------------------------------------------------------*
*&      Module  STATUS_4000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_4000 OUTPUT.

  DATA: WA_PROTOLO_MEMO TYPE ZDOC_MEMO_PRO_ME.

  SELECT SINGLE * INTO WA_PROTOLO_MEMO
    FROM ZDOC_MEMO_PRO_ME
   WHERE NR_MEMORANDO EQ WA_MEMORANDO-NR_MEMORANDO.

  IF SY-SUBRC EQ 0.
    MESSAGE S001 WITH WA_PROTOLO_MEMO-NR_PROTOCOLO DISPLAY LIKE C_S.
    VG_TEM_PROTOCOLO = C_X.
  ELSE.
    CLEAR: VG_TEM_PROTOCOLO.
  ENDIF.

ENDMODULE.                 " STATUS_4000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_4000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_4000 INPUT.

  CASE OK_CODE.
    WHEN C_BTPESQ.
      CLEAR OK_CODE.
      PERFORM POPULAR_NOTAS_LIVRES.
    WHEN C_BTVINF.
      CLEAR OK_CODE.
      PERFORM VINCULAR_NOTA.
    WHEN C_BTDENF.
      CLEAR OK_CODE.
      PERFORM DESVINCULAR_NOTA.
    WHEN C_SAVE.
      CLEAR OK_CODE.
      PERFORM SALVA_NOTAS.
    WHEN C_BACKV OR C_EXITV OR C_CANCELV.
      CLEAR OK_CODE.
      PERFORM SAIR_NOTAS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_4000  INPUT

*&---------------------------------------------------------------------*
*&      Module  LIVRES_CHANGE_FIELD_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LIVRES_CHANGE_FIELD_ATTR OUTPUT.

  IF VG_TEM_PROTOCOLO IS INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'IT_NF_LIVRE'.
        SCREEN-OUTPUT = '1'.
        SCREEN-INPUT  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " LIVRES_CHANGE_FIELD_ATTR  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VINCU_CHANGE_FIELD_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VINCU_CHANGE_FIELD_ATTR OUTPUT.

  IF VG_TEM_PROTOCOLO IS INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'IT_NF_VINCU'.
        SCREEN-OUTPUT = '1'.
        SCREEN-INPUT  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " VINCU_CHANGE_FIELD_ATTR  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  POPULAR_NOTAS_LIVRES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POPULAR_NOTAS_LIVRES .

  DATA: GS_BUKRS  TYPE ZMEMO_BUKRS,
        GS_BRANCH TYPE ZMEMO_BRANCH,
        GS_CFOP   TYPE ZMEMO_CFOP,
        GS_DOCNUM TYPE STR_SO_DOCNUM,
        GS_NFNUM  TYPE BAPI_J_1BNFNFNUM_RA,
        GS_NFENUM TYPE BAPI_J_1BNFNFENUM_RA,
        VG_PARID  TYPE J_1BPARID.

  DATA: GT_BUKRS  LIKE TABLE OF GS_BUKRS,
        GT_BRANCH LIKE TABLE OF GS_BRANCH,
        GT_CFOP   LIKE TABLE OF GS_CFOP,
        GT_DOCNUM LIKE TABLE OF STR_SO_DOCNUM,
        GT_NFNUM  LIKE TABLE OF BAPI_J_1BNFNFNUM_RA,
        GT_NFENUM LIKE TABLE OF BAPI_J_1BNFNFENUM_RA,
        VG_TABIX  TYPE SY-TABIX.

  DATA: VG_CANCEL    TYPE CANCEL.

  CLEAR: IT_NF_LIVRE[], IT_NF_DESV[].

  IF J_1BNFDOC-BUKRS IS NOT INITIAL.
    GS_BUKRS-SIGN   = 'I'.
    GS_BUKRS-OPTION = 'EQ'.
    GS_BUKRS-LOW    = J_1BNFDOC-BUKRS.
    GS_BUKRS-HIGH   = J_1BNFDOC-BUKRS.
    APPEND GS_BUKRS TO GT_BUKRS.
  ENDIF.

  IF J_1BNFDOC-BRANCH IS NOT INITIAL.
    GS_BRANCH-SIGN   = 'I'.
    GS_BRANCH-OPTION = 'EQ'.
    GS_BRANCH-LOW    = ZDOC_MEMORANDO-REPRESENTANTE+6(4).
    GS_BRANCH-HIGH   = ZDOC_MEMORANDO-REPRESENTANTE+6(4).
    APPEND GS_BRANCH TO GT_BRANCH.
  ENDIF.

  IF J_1BNFDOC-DOCNUM IS NOT INITIAL.
    GS_DOCNUM-SIGN   = 'I'.
    GS_DOCNUM-OPTION = 'EQ'.
    GS_DOCNUM-LOW    = J_1BNFDOC-DOCNUM.
    GS_DOCNUM-HIGH   = J_1BNFDOC-DOCNUM.
    APPEND GS_DOCNUM TO GT_DOCNUM.
  ENDIF.

  IF J_1BNFDOC-NFENUM IS NOT INITIAL.
    MOVE J_1BNFDOC-NFENUM TO J_1BNFDOC-NFNUM.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = J_1BNFDOC-NFENUM
      IMPORTING
        OUTPUT = J_1BNFDOC-NFENUM.

    GS_NFENUM-SIGN   = 'I'.
    GS_NFENUM-OPTION = 'EQ'.
    GS_NFENUM-LOW    = J_1BNFDOC-NFENUM.
    GS_NFENUM-HIGH   = J_1BNFDOC-NFENUM.
    APPEND GS_NFENUM TO GT_NFENUM.

    GS_NFNUM-SIGN   = 'I'.
    GS_NFNUM-OPTION = 'EQ'.
    GS_NFNUM-LOW    = J_1BNFDOC-NFNUM.
    GS_NFNUM-HIGH   = J_1BNFDOC-NFNUM.
    APPEND GS_NFNUM TO GT_NFNUM.
  ENDIF.

  CALL FUNCTION 'Z_MEMO_CFOP_ENTRADAS'
    TABLES
      CFOPS = GT_CFOP.

  IF GT_CFOP IS INITIAL.
    MESSAGE E040 DISPLAY LIKE C_S.
  ENDIF.

  IF ZDOC_MEMORANDO-TP_TRANSF EQ C_M.
    CONCATENATE ZDOC_MEMORANDO-BUKRS ZDOC_MEMORANDO-BRANCH INTO VG_PARID.
  ELSE.
    VG_PARID = ZDOC_MEMORANDO-REMETENTE.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_NF_LIVRE
    FROM J_1BNFDOC AS DC
   INNER JOIN J_1BNFLIN AS LI ON LI~DOCNUM EQ DC~DOCNUM
   WHERE DC~BUKRS  IN GT_BUKRS
     AND DC~BRANCH IN GT_BRANCH
     AND DC~PARID  EQ VG_PARID
     AND DC~DIRECT EQ C_1
     AND DC~CANCEL EQ VG_CANCEL
     AND DC~DOCTYP NE C_5
     AND DC~DOCDAT GE J_1BNFDOC-DOCDAT
     AND DC~DOCDAT LE J_1BNFDOC-PSTDAT
     AND LI~MATNR  EQ ZDOC_MEMO_NF_EXP-MATERIAL
     AND LI~CFOP   IN GT_CFOP
     AND ( DC~NFNUM  IN GT_NFNUM OR DC~NFENUM IN GT_NFENUM )
     AND DC~DOCNUM IN GT_DOCNUM.


  LOOP AT IT_NF_LIVRE INTO WA_NF_LIVRE.
    VG_TABIX = SY-TABIX.

    IF WA_NF_LIVRE-MEINS NE 'KG'.

      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          I_MATNR             = WA_NF_LIVRE-MATNR
          I_MEIN1             = WA_NF_LIVRE-MEINS
          I_MEINS             = 'KG'
          I_MENGE             = WA_NF_LIVRE-MENGE
        IMPORTING
          MENGE               = WA_NF_LIVRE-MENGE
        EXCEPTIONS
          ERROR_IN_CONVERSION = 1
          NO_SUCCESS          = 2
          OTHERS              = 3.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        WA_NF_LIVRE-MEINS = 'KG'.
        MODIFY IT_NF_LIVRE INDEX VG_TABIX FROM WA_NF_LIVRE TRANSPORTING MENGE MEINS.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM REAJUSTA_NOTAS USING SPACE.

ENDFORM.                    " POPULAR_NOTAS_LIVRES

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_VARIAVEIS_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM LIMPAR_VARIAVEIS_NOTAS .
  CLEAR: IT_NF_LIVRE[],
         IT_NF_VINCU[],
         IT_NF_VINCU_S[],
         ZDOC_MEMORANDO,
         VG_QUANTIDADE,
         VG_SALDO,
         VG_VINCULADO,
         VG_QTD_VINCU,
         J_1BNFDOC,
         VG_ALTEROU_NOTAS.
ENDFORM.                    " LIMPAR_VARIAVEIS_NOTAS

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VINCULAR_NOTA .

  DATA: VG_TABIX            TYPE SY-TABIX,
        VG_SALDO_A_VINCULAR TYPE J_1BNETQTY,
        VG_TOTAL_VINCULADO  TYPE J_1BNETQTY.

  CHECK VG_TEM_PROTOCOLO IS INITIAL.

  IF VG_QTD_VINCU GT 0.

    PERFORM REAJUSTA_NOTAS USING SPACE.

    VG_SALDO_A_VINCULAR = VG_QTD_VINCU - VG_VINCULADO.

    IF VG_SALDO_A_VINCULAR GT 0.

      LOOP AT IT_NF_LIVRE INTO WA_NF_LIVRE WHERE MARK EQ C_X.

        VG_TABIX = SY-TABIX.
        CLEAR: WA_NF_LIVRE-MARK.
        MOVE-CORRESPONDING WA_NF_LIVRE TO WA_NF_VINCU.

        IF VG_SALDO_A_VINCULAR GT WA_NF_LIVRE-SALDO.
          WA_NF_VINCU-MENGE = WA_NF_LIVRE-SALDO.
        ELSEIF VG_SALDO_A_VINCULAR GT 0.
          WA_NF_VINCU-MENGE = VG_SALDO_A_VINCULAR.
        ELSE.
          WA_NF_VINCU-MENGE = 0.
        ENDIF.

        IF WA_NF_VINCU-MENGE GT 0.

          VG_TOTAL_VINCULADO = VG_VINCULADO + WA_NF_VINCU-MENGE.

          IF VG_TOTAL_VINCULADO GE ZDOC_MEMORANDO-QUANTIDADE_MEMO.
            WA_NF_VINCU-MENGE = ZDOC_MEMORANDO-QUANTIDADE_MEMO - VG_VINCULADO.
          ELSEIF VG_TOTAL_VINCULADO EQ ZDOC_MEMORANDO-QUANTIDADE_MEMO.
            WA_NF_VINCU-MENGE = 0.
          ENDIF.

          IF WA_NF_VINCU-MENGE GT 0.

            WA_NF_LIVRE-SALDO   = WA_NF_LIVRE-SALDO - WA_NF_VINCU-MENGE.
            VG_SALDO            = VG_SALDO - WA_NF_VINCU-MENGE.
            VG_VINCULADO        = VG_VINCULADO + WA_NF_VINCU-MENGE.
            VG_SALDO_A_VINCULAR = VG_SALDO_A_VINCULAR - WA_NF_VINCU-MENGE.

            DELETE IT_NF_DESV WHERE DOCNUM EQ WA_NF_VINCU-DOCNUM AND ITMNUM EQ WA_NF_VINCU-ITMNUM.

            READ TABLE IT_NF_VINCU INTO WA_NF_AUX WITH KEY DOCNUM = WA_NF_VINCU-DOCNUM
                                                           ITMNUM = WA_NF_VINCU-ITMNUM.
            IF SY-SUBRC EQ 0.
              WA_NF_AUX-MENGE = WA_NF_AUX-MENGE + WA_NF_VINCU-MENGE.
              MODIFY IT_NF_VINCU INDEX SY-TABIX FROM WA_NF_AUX TRANSPORTING MENGE.
            ELSE.
              APPEND WA_NF_VINCU TO IT_NF_VINCU.
            ENDIF.

            MODIFY IT_NF_LIVRE INDEX VG_TABIX FROM WA_NF_LIVRE TRANSPORTING SALDO MARK.
            VG_ALTEROU_NOTAS = C_X.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ELSE.
      MESSAGE E026 DISPLAY LIKE C_S.
    ENDIF.

  ELSE.
    MESSAGE E025 DISPLAY LIKE C_S.
  ENDIF.

ENDFORM.                    " VINCULAR_NOTA

*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DESVINCULAR_NOTA .

  CHECK VG_TEM_PROTOCOLO IS INITIAL.

  LOOP AT IT_NF_VINCU INTO WA_NF_VINCU WHERE MARK EQ C_X.
    CLEAR: WA_NF_VINCU-MARK.
    APPEND WA_NF_VINCU TO IT_NF_DESV.
    VG_ALTEROU_NOTAS = C_X.
  ENDLOOP.

  DELETE IT_NF_VINCU WHERE MARK EQ C_X.

  PERFORM REAJUSTA_NOTAS USING C_X.

ENDFORM.                    " DESVINCULAR_NOTA

*&---------------------------------------------------------------------*
*&      Form  SALDO_DOC_NUM_ITM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SALDO_DOC_NUM_ITM  USING    P_DOCNUM   TYPE J_1BDOCNUM
                                 P_ITMNUM   TYPE J_1BITMNUM
                                 P_MENGE    TYPE J_1BNETQTY
                                 P_SALDO    TYPE J_1BNETQTY.

  DATA: VG_VINCU TYPE J_1BNETQTY.

  VG_VINCU = 0.


  READ TABLE IT_NF_DESV WITH KEY DOCNUM = P_DOCNUM
                                 ITMNUM = P_ITMNUM.

  IF SY-SUBRC NE 0.
    SELECT SUM( MENGE ) INTO VG_VINCU
      FROM ZDOC_MEMO_NOTA
     WHERE DOCNUM EQ P_DOCNUM
       AND ITMNUM EQ P_ITMNUM.
*       AND NR_MEMORANDO NE ZDOC_MEMORANDO-NR_MEMORANDO.
  ENDIF.

  IF VG_VINCU GT 0.
    P_SALDO = P_SALDO - VG_VINCU.
  ELSE.
    READ TABLE IT_NF_VINCU INTO WA_NF_VINCU WITH KEY DOCNUM = P_DOCNUM ITMNUM = P_ITMNUM.
    IF SY-SUBRC EQ 0.
      P_SALDO = P_SALDO - WA_NF_VINCU-MENGE.
    ENDIF.

    READ TABLE IT_NF_VINCU_S INTO WA_NF_VINCU WITH KEY DOCNUM = P_DOCNUM ITMNUM = P_ITMNUM.
    IF SY-SUBRC EQ 0.
      P_SALDO = P_SALDO - WA_NF_VINCU-MENGE.
    ENDIF.

  ENDIF.

  READ TABLE IT_NF_DESV INTO WA_NF_VINCU WITH KEY DOCNUM = P_DOCNUM ITMNUM = P_ITMNUM.
  IF SY-SUBRC EQ 0.
    P_SALDO = P_SALDO + WA_NF_VINCU-MENGE.
  ENDIF.

ENDFORM.                    " SALDO_DOC_NUM_ITM

*&---------------------------------------------------------------------*
*&      Form  REAJUSTA_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REAJUSTA_NOTAS USING OP TYPE C .

  DATA: VG_TABIX  TYPE SY-TABIX.

  VG_QUANTIDADE = 0.
  VG_SALDO      = 0.

  LOOP AT IT_NF_LIVRE INTO WA_NF_LIVRE.

    VG_TABIX = SY-TABIX.

    IF NOT OP IS INITIAL.
      READ TABLE IT_NF_DESV WITH KEY DOCNUM = WA_NF_LIVRE-DOCNUM ITMNUM = WA_NF_LIVRE-ITMNUM.
    ENDIF.

    IF ( SY-SUBRC EQ 0 ) OR ( OP IS INITIAL ).
      IF OP IS INITIAL.
        WA_NF_LIVRE-SALDO    = WA_NF_LIVRE-MENGE.
        WA_NF_LIVRE-VINCULAR = WA_NF_LIVRE-MENGE.
      ENDIF.
      PERFORM SALDO_DOC_NUM_ITM USING WA_NF_LIVRE-DOCNUM  WA_NF_LIVRE-ITMNUM WA_NF_LIVRE-MENGE WA_NF_LIVRE-SALDO.
      WA_NF_LIVRE-VINCULAR = WA_NF_LIVRE-SALDO.
      MODIFY IT_NF_LIVRE INDEX VG_TABIX FROM WA_NF_LIVRE TRANSPORTING SALDO VINCULAR.
    ENDIF.

*    VG_QUANTIDADE = VG_QUANTIDADE + WA_NF_LIVRE-MENGE.
*    VG_SALDO      = VG_SALDO      + WA_NF_LIVRE-SALDO.

  ENDLOOP.

  SORT IT_NF_LIVRE BY DOCDAT DOCNUM.

  VG_VINCULADO = 0.

*  DELETE IT_NF_LIVRE WHERE SALDO EQ 0. "WSB
  DELETE IT_NF_LIVRE WHERE SALDO LE 0. " LE <=

  LOOP AT IT_NF_LIVRE INTO WA_NF_LIVRE.
    VG_QUANTIDADE = VG_QUANTIDADE + WA_NF_LIVRE-MENGE.
    VG_SALDO      = VG_SALDO      + WA_NF_LIVRE-SALDO.
  ENDLOOP.

  LOOP AT IT_NF_VINCU INTO WA_NF_VINCU.
    VG_VINCULADO = VG_VINCULADO + WA_NF_VINCU-MENGE.
  ENDLOOP.

  CLEAR: IT_NF_DESV[].

ENDFORM.                    " REAJUSTA_NOTAS

*&---------------------------------------------------------------------*
*&      Form  SALVA_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SALVA_NOTAS .

  DATA VG_LINES TYPE I.

  VG_LINES = 0.

  IF NOT VG_ALTEROU_NOTAS IS INITIAL.
    DELETE FROM ZDOC_MEMO_NOTA WHERE NR_MEMORANDO EQ ZDOC_MEMORANDO-NR_MEMORANDO.

    LOOP AT IT_NF_VINCU INTO WA_NF_VINCU.
      WA_NF_VINCU-NR_MEMORANDO = ZDOC_MEMORANDO-NR_MEMORANDO.
      MODIFY ZDOC_MEMO_NOTA FROM WA_NF_VINCU.
    ENDLOOP.

    DESCRIBE TABLE IT_NF_VINCU LINES VG_LINES.

    IF VG_LINES GT 0.
      ZDOC_MEMORANDO-STATUS = C_P.
      MODIFY ZDOC_MEMORANDO.
    ELSE.
      CLEAR: ZDOC_MEMORANDO-STATUS.
      MODIFY ZDOC_MEMORANDO.
    ENDIF.

    COMMIT WORK.

    MESSAGE S027 WITH ZDOC_MEMORANDO-NR_MEMORANDO DISPLAY LIKE C_S.
    CLEAR VG_ALTEROU_NOTAS.
  ENDIF.

ENDFORM.                    " SALVA_NOTAS

*&---------------------------------------------------------------------*
*&      Form  SAIR_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAIR_NOTAS .

  DATA: ANSWER TYPE C LENGTH 1.

  IF NOT VG_ALTEROU_NOTAS IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        TITEL     = TEXT-001
        TEXTLINE1 = TEXT-002
        TEXTLINE2 = TEXT-003
      IMPORTING
        ANSWER    = ANSWER.

    CASE ANSWER.
      WHEN C_J.
        PERFORM SALVA_NOTAS.
        VG_DYNNR_000 = VG_DYNNR_ANT.
        PERFORM DESBLOQUEIA USING ZDOC_MEMORANDO-NR_MEMORANDO.
        PERFORM LIMPAR_VARIAVEIS_NOTAS.
      WHEN C_N.
        VG_DYNNR_000 = VG_DYNNR_ANT.
        PERFORM DESBLOQUEIA USING ZDOC_MEMORANDO-NR_MEMORANDO.
        PERFORM LIMPAR_VARIAVEIS_NOTAS.
      WHEN C_A.
        EXIT.
    ENDCASE.

  ELSE.
    VG_DYNNR_000 = VG_DYNNR_ANT.
    PERFORM DESBLOQUEIA USING ZDOC_MEMORANDO-NR_MEMORANDO.
    PERFORM LIMPAR_VARIAVEIS_NOTAS.
  ENDIF.

ENDFORM.                    " SAIR_NOTAS



*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_NF_LIVRES'. DO NOT CHANGE THIS LIN
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TAB_NF_LIVRES_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_NF_LIVRE LINES TAB_NF_LIVRES-LINES.
ENDMODULE.                    "TAB_NF_LIVRES_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_NF_LIVRES'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TAB_NF_LIVRES_MARK INPUT.
  DATA: G_TAB_NF_LIVRES_WA2 LIKE LINE OF IT_NF_LIVRE.
  IF TAB_NF_LIVRES-LINE_SEL_MODE = 1
  AND IT_NF_LIVRE-MARK = C_X.
    LOOP AT IT_NF_LIVRE INTO G_TAB_NF_LIVRES_WA2
      WHERE MARK = C_X.
      G_TAB_NF_LIVRES_WA2-MARK = ''.
      MODIFY IT_NF_LIVRE
        FROM G_TAB_NF_LIVRES_WA2
        TRANSPORTING MARK.
    ENDLOOP.
  ENDIF.
  MODIFY IT_NF_LIVRE
    INDEX TAB_NF_LIVRES-CURRENT_LINE
    TRANSPORTING MARK.
ENDMODULE.                    "TAB_NF_LIVRES_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_NF_LIVRES'. DO NOT CHANGE THIS LINE
*&SPWIZARD: PROCESS USER COMMAND
MODULE TAB_NF_LIVRES_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TAB_NF_LIVRES'
                              'IT_NF_LIVRE'
                              'MARK'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.                    "TAB_NF_LIVRES_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                         P_TABLE_NAME
                         P_MARK_NAME
                CHANGING P_OK      LIKE SY-UCOMM.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: L_OK              TYPE SY-UCOMM,
        L_OFFSET          TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH P_OK FOR P_TC_NAME.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  L_OFFSET = STRLEN( P_TC_NAME ) + 1.
  L_OK = P_OK+L_OFFSET.

  CASE L_OK.
    WHEN 'INSR'.                      "insert row
      PERFORM FCODE_INSERT_ROW USING    P_TC_NAME
                                        P_TABLE_NAME.
      CLEAR P_OK.

    WHEN 'DELE'.                      "delete row
      PERFORM FCODE_DELETE_ROW USING    P_TC_NAME
                                        P_TABLE_NAME
                                        P_MARK_NAME.
      CLEAR P_OK.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                            L_OK.
      CLEAR P_OK.
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                        P_TABLE_NAME
                                        P_MARK_NAME   .
      CLEAR P_OK.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                          P_TABLE_NAME
                                          P_MARK_NAME .
      CLEAR P_OK.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM FCODE_INSERT_ROW
              USING    P_TC_NAME           TYPE DYNFNAM
                       P_TABLE_NAME             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_LINES_NAME       LIKE FELD-NAME.
  DATA L_SELLINE          LIKE SY-STEPL.
  DATA L_LASTLINE         TYPE I.
  DATA L_LINE             TYPE I.
  DATA L_TABLE_NAME       LIKE FELD-NAME.
  FIELD-SYMBOLS <TC>                 TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <LINES>              TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_LINES_NAME.
  ASSIGN (L_LINES_NAME) TO <LINES>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE L_SELLINE.
  IF SY-SUBRC <> 0.                   " append line to table
    L_SELLINE = <TC>-LINES + 1.
*&SPWIZARD: set top line                                               *
    IF L_SELLINE > <LINES>.
      <TC>-TOP_LINE = L_SELLINE - <LINES> + 1 .
    ELSE.
      <TC>-TOP_LINE = 1.
    ENDIF.
  ELSE.                               " insert line into table
    L_SELLINE = <TC>-TOP_LINE + L_SELLINE - 1.
    L_LASTLINE = <TC>-TOP_LINE + <LINES> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  L_LINE = L_SELLINE - <TC>-TOP_LINE + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <TABLE> INDEX L_SELLINE.
  <TC>-LINES = <TC>-LINES + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE L_LINE.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM FCODE_DELETE_ROW
              USING    P_TC_NAME           TYPE DYNFNAM
                       P_TABLE_NAME
                       P_MARK_NAME   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <TABLE> LINES <TC>-LINES.

  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    IF <MARK_FIELD> = 'X'.
      DELETE <TABLE> INDEX SYST-TABIX.
      IF SY-SUBRC = 0.
        <TC>-LINES = <TC>-LINES - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME
                                      P_OK.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TC_NEW_TOP_LINE     TYPE I.
  DATA L_TC_NAME             LIKE FELD-NAME.
  DATA L_TC_LINES_NAME       LIKE FELD-NAME.
  DATA L_TC_FIELD_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <LINES>      TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
  ASSIGN (L_TC_LINES_NAME) TO <LINES>.


*&SPWIZARD: is no line filled?                                         *
  IF <TC>-LINES = 0.
*&SPWIZARD: yes, ...                                                   *
    L_TC_NEW_TOP_LINE = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
         EXPORTING
              ENTRY_ACT             = <TC>-TOP_LINE
              ENTRY_FROM            = 1
              ENTRY_TO              = <TC>-LINES
              LAST_PAGE_FULL        = 'X'
              LOOPS                 = <LINES>
              OK_CODE               = P_OK
              OVERLAPPING           = 'X'
         IMPORTING
              ENTRY_NEW             = L_TC_NEW_TOP_LINE
         EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
              OTHERS                = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD L_TC_FIELD_NAME
             AREA  L_TC_NAME.

  IF SYST-SUBRC = 0.
    IF L_TC_NAME = P_TC_NAME.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_MARK_LINES USING P_TC_NAME
                               P_TABLE_NAME
                               P_MARK_NAME.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    <MARK_FIELD> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                 P_TABLE_NAME
                                 P_MARK_NAME .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    <MARK_FIELD> = SPACE.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_NF_VINCU'. DO NOT CHANGE THIS LINE
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TAB_NF_VINCU_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_NF_VINCU LINES TAB_NF_VINCU-LINES.
ENDMODULE.                    "TAB_NF_VINCU_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_NF_VINCU'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TAB_NF_VINCU_MARK INPUT.
  DATA: G_TAB_NF_VINCU_WA2 LIKE LINE OF IT_NF_VINCU.
  IF TAB_NF_VINCU-LINE_SEL_MODE = 1
  AND IT_NF_VINCU-MARK = C_X.
    LOOP AT IT_NF_VINCU INTO G_TAB_NF_VINCU_WA2
      WHERE MARK = C_X.
      G_TAB_NF_VINCU_WA2-MARK = ''.
      MODIFY IT_NF_VINCU
        FROM G_TAB_NF_VINCU_WA2
        TRANSPORTING MARK.
    ENDLOOP.
  ENDIF.
  MODIFY IT_NF_VINCU
    INDEX TAB_NF_VINCU-CURRENT_LINE
    TRANSPORTING MARK.
ENDMODULE.                    "TAB_NF_VINCU_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_NF_VINCU'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TAB_NF_VINCU_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TAB_NF_VINCU'
                              'IT_NF_VINCU'
                              'MARK'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.                    "TAB_NF_VINCU_USER_COMMAND INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_4002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_4002 OUTPUT.

  CLEAR: VG_NOME_EMPRESA, VG_NOME_CENTRO.

  IF J_1BNFDOC-BUKRS IS INITIAL.
    SELECT SINGLE BUKRS INTO J_1BNFDOC-BUKRS
      FROM J_1BBRANCH
     WHERE BRANCH EQ ZDOC_MEMORANDO-REPRESENTANTE+6(4).

    J_1BNFDOC-BRANCH = ZDOC_MEMORANDO-REPRESENTANTE+6(4).
  ENDIF.

  SELECT SINGLE BUTXT INTO VG_NOME_EMPRESA
    FROM T001
   WHERE BUKRS EQ J_1BNFDOC-BUKRS.

  SELECT SINGLE NAME1 INTO VG_NOME_CENTRO
    FROM T001W
   WHERE WERKS EQ ZDOC_MEMORANDO-REPRESENTANTE+6(4).

ENDMODULE.                 " STATUS_4002  OUTPUT
