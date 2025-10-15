*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDO_6000 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_6000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_6000 INPUT.

  CASE OK_CODE.
    WHEN C_BTPESQ.
      CLEAR OK_CODE.
      PERFORM POPULAR_NOTAS_LIVRES_SAIDA.
    WHEN C_BTVINF.
      CLEAR OK_CODE.
      PERFORM VINCULAR_NOTA_S.
    WHEN C_BTDENF.
      CLEAR OK_CODE.
      PERFORM DESVINCULAR_NOTA_S.
    WHEN C_SAVE.
      CLEAR OK_CODE.
      PERFORM SALVA_NOTAS_S.
    WHEN C_BACKV OR C_EXITV OR C_CANCELV.
      CLEAR OK_CODE.
      PERFORM SAIR_NOTAS_SAIDA.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_6000  INPUT

*&---------------------------------------------------------------------*
*&      Form  POPULAR_NOTAS_LIVRES_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POPULAR_NOTAS_LIVRES_SAIDA .

  DATA: T_CFOP  TYPE TABLE OF ZMEMO_CFOP.

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
        GT_NFENUM LIKE TABLE OF BAPI_J_1BNFNFENUM_RA.

  DATA: VG_CANCEL TYPE CANCEL.

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
    GS_BRANCH-LOW    = ZDOC_MEMORANDO-REMETENTE+6(4).
    GS_BRANCH-HIGH   = ZDOC_MEMORANDO-REMETENTE+6(4).
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

  CALL FUNCTION 'Z_MEMO_CFOP_SAIDA'
    TABLES
      CFOPS = GT_CFOP.

  IF GT_CFOP IS INITIAL.
    MESSAGE E041 DISPLAY LIKE C_S.
  ENDIF.

  IF ZDOC_MEMORANDO-TP_TRANSF EQ C_R.
    CONCATENATE ZDOC_MEMORANDO-BUKRS ZDOC_MEMORANDO-BRANCH INTO VG_PARID.
  ELSE.
    VG_PARID = ZDOC_MEMORANDO-REPRESENTANTE.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_NF_LIVRE
    FROM J_1BNFDOC AS DC
   INNER JOIN J_1BNFLIN AS LI ON LI~DOCNUM EQ DC~DOCNUM
   WHERE DC~BUKRS  IN GT_BUKRS
     AND DC~BRANCH IN GT_BRANCH
     AND DC~PARID  EQ VG_PARID "WSB
     AND DC~DIRECT EQ C_2
     AND DC~CANCEL EQ VG_CANCEL
     AND DC~DOCTYP NE C_5
     AND DC~DOCDAT GE J_1BNFDOC-DOCDAT "WSB
     AND DC~DOCDAT LE J_1BNFDOC-PSTDAT " WSB
     AND LI~MATNR  EQ ZDOC_MEMO_NF_EXP-MATERIAL "WSB
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

  PERFORM REAJUSTA_NOTAS_S USING SPACE.

ENDFORM.                    " POPULAR_NOTAS_LIVRES_SAIDA

*&---------------------------------------------------------------------*
*&      Form  SAIR_NOTAS_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SAIR_NOTAS_SAIDA .

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
        "PERFORM salva_notas.
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

ENDFORM.                    " SAIR_NOTAS_SAIDA

*&---------------------------------------------------------------------*
*&      Form  REAJUSTA_NOTAS_S
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REAJUSTA_NOTAS_S  USING OP TYPE C .

  DATA: VG_TABIX  TYPE SY-TABIX.
  DATA: WA_NOTA_S TYPE ZDOC_MEMO_NOTA_S.

  "Retira notas de saída não salvas
  IF NOT ( IT_NF_VINCU_S[] IS INITIAL ) AND ( SY-UCOMM EQ 'BTPESQ' ).

    LOOP AT IT_NF_VINCU_S INTO WA_NF_VINCU_S.

      SELECT SINGLE * FROM ZDOC_MEMO_NOTA_S INTO WA_NOTA_S WHERE DOCNUM EQ WA_NF_VINCU_S-DOCNUM.

      IF ( SY-SUBRC EQ 0 ).
        CONTINUE.
      ELSE.
        DELETE IT_NF_VINCU_S WHERE DOCNUM EQ WA_NF_VINCU_S-DOCNUM.
      ENDIF.

      CLEAR: WA_NF_VINCU_S.
    ENDLOOP.
  ENDIF.

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
      PERFORM SALDO_DOC_NUM_ITM_S USING WA_NF_LIVRE-DOCNUM  WA_NF_LIVRE-ITMNUM WA_NF_LIVRE-MENGE WA_NF_LIVRE-SALDO.
      WA_NF_LIVRE-VINCULAR = WA_NF_LIVRE-SALDO.
      MODIFY IT_NF_LIVRE INDEX VG_TABIX FROM WA_NF_LIVRE TRANSPORTING SALDO VINCULAR.
    ENDIF.

*    VG_QUANTIDADE = VG_QUANTIDADE + WA_NF_LIVRE-MENGE.
    VG_SALDO      = VG_SALDO      + WA_NF_LIVRE-SALDO.

  ENDLOOP.

  SORT IT_NF_LIVRE BY DOCDAT.

  VG_VINCULADO = 0.

  DELETE IT_NF_LIVRE WHERE SALDO EQ 0.

  LOOP AT IT_NF_LIVRE.
    VG_QUANTIDADE = VG_QUANTIDADE + WA_NF_LIVRE-MENGE.
    IT_NF_LIVRE-VINCULAR = IT_NF_LIVRE-SALDO.
    MODIFY IT_NF_LIVRE INDEX SY-TABIX TRANSPORTING VINCULAR.
  ENDLOOP.

  IF NOT (   IT_NF_VINCU[] IS INITIAL ).
    LOOP AT IT_NF_VINCU INTO WA_NF_VINCU.
      VG_VINCULADO = VG_VINCULADO + WA_NF_VINCU-MENGE.
    ENDLOOP.
  ELSE.
    CLEAR: WA_NF_VINCU_S.
    LOOP AT IT_NF_VINCU_S INTO WA_NF_VINCU_S.
      VG_VINCULADO = VG_VINCULADO + WA_NF_VINCU_S-MENGE.
    ENDLOOP.
  ENDIF.

  CLEAR: IT_NF_DESV[].

ENDFORM.                    " REAJUSTA_NOTAS_S

*&---------------------------------------------------------------------*
*&      Form  SALDO_DOC_NUM_ITM_S
*&---------------------------------------------------------------------*
*       Busca Saldo utilizado de notas de saida
*----------------------------------------------------------------------*
FORM SALDO_DOC_NUM_ITM_S  USING  P_DOCNUM TYPE J_1BDOCNUM
                                 P_ITMNUM TYPE J_1BITMNUM
                                 P_MENGE  TYPE J_1BNETQTY
                                 P_SALDO  TYPE J_1BNETQTY.

  DATA: VG_VINCU TYPE J_1BNETQTY.

  VG_VINCU = 0.


  READ TABLE IT_NF_DESV WITH KEY DOCNUM = P_DOCNUM
                                 ITMNUM = P_ITMNUM.

  IF SY-SUBRC NE 0.
    SELECT SUM( MENGE ) INTO VG_VINCU
      FROM ZDOC_MEMO_NOTA_S
     WHERE DOCNUM EQ P_DOCNUM
       AND ITMNUM EQ P_ITMNUM
       AND NR_MEMORANDO NE ZDOC_MEMORANDO-NR_MEMORANDO.
  ENDIF.

  IF VG_VINCU GT 0.
    P_SALDO = P_SALDO - VG_VINCU.
  ELSE.
    READ TABLE IT_NF_VINCU INTO WA_NF_VINCU WITH KEY DOCNUM = P_DOCNUM ITMNUM = P_ITMNUM.
    IF SY-SUBRC EQ 0.
      P_SALDO = P_SALDO - WA_NF_VINCU-MENGE.
    ENDIF.

    READ TABLE IT_NF_VINCU_S INTO WA_NF_VINCU_S WITH KEY DOCNUM = P_DOCNUM ITMNUM = P_ITMNUM.
    IF SY-SUBRC EQ 0.
      P_SALDO = P_SALDO - WA_NF_VINCU_S-MENGE.
    ENDIF.

  ENDIF.

  READ TABLE IT_NF_DESV INTO WA_NF_VINCU WITH KEY DOCNUM = P_DOCNUM ITMNUM = P_ITMNUM.
  IF SY-SUBRC EQ 0.
    P_SALDO = P_SALDO + WA_NF_VINCU-MENGE.
  ENDIF.

ENDFORM.                    " SALDO_DOC_NUM_ITM_S

*&---------------------------------------------------------------------*
*&      Module  STATUS_6002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_6002 OUTPUT.

  CLEAR: VG_NOME_EMPRESA, VG_NOME_CENTRO.

  IF J_1BNFDOC-BUKRS IS INITIAL.
    SELECT SINGLE BUKRS INTO J_1BNFDOC-BUKRS
      FROM J_1BBRANCH
     WHERE BRANCH EQ ZDOC_MEMORANDO-REMETENTE+6(4).

    J_1BNFDOC-BRANCH = ZDOC_MEMORANDO-REMETENTE+6(4).
  ENDIF.

  SELECT SINGLE BUTXT INTO VG_NOME_EMPRESA
    FROM T001
   WHERE BUKRS EQ J_1BNFDOC-BUKRS.

  SELECT SINGLE NAME1 INTO VG_NOME_CENTRO
    FROM T001W
   WHERE WERKS EQ ZDOC_MEMORANDO-REMETENTE+6(4).

ENDMODULE.                 " STATUS_6002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TAB_NF_LIVRES_S_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TAB_NF_LIVRES_S_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_NF_LIVRE LINES TAB_NF_LIVRES_S-LINES.
ENDMODULE.                 " TAB_NF_LIVRES_S_CHANGE_TC_ATTR  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TAB_NF_LIVRES_S_MARK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TAB_NF_LIVRES_S_MARK INPUT.

  DATA: G_TAB_NF_LIVRES_S_WA2 LIKE LINE OF IT_NF_LIVRE.
  IF TAB_NF_LIVRES_S-LINE_SEL_MODE = 1
  AND IT_NF_LIVRE-MARK = C_X.
    LOOP AT IT_NF_LIVRE INTO G_TAB_NF_LIVRES_S_WA2
      WHERE MARK = C_X.
      G_TAB_NF_LIVRES_S_WA2-MARK = ''.
      MODIFY IT_NF_LIVRE
        FROM G_TAB_NF_LIVRES_S_WA2
        TRANSPORTING MARK.
    ENDLOOP.
  ENDIF.
  MODIFY IT_NF_LIVRE
    INDEX TAB_NF_LIVRES_S-CURRENT_LINE
    TRANSPORTING MARK.

ENDMODULE.                 " TAB_NF_LIVRES_S_MARK  INPUT

*&---------------------------------------------------------------------*
*&      Module  TAB_NF_LIVRES_S_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TAB_NF_LIVRES_S_USER_COMMAND INPUT.

  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TAB_NF_LIVRES_S'
                              'IT_NF_LIVRE'
                              'MARK'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.

ENDMODULE.                 " TAB_NF_LIVRES_S_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  TAB_NF_VINCU_S_MARK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TAB_NF_VINCU_S_MARK INPUT.

  DATA: G_TAB_NF_VINCU_S_WA2 LIKE LINE OF IT_NF_VINCU_S.
  IF TAB_NF_VINCU_S-LINE_SEL_MODE = 1
  AND IT_NF_VINCU_S-MARK = C_X.
    LOOP AT IT_NF_VINCU_S INTO G_TAB_NF_VINCU_S_WA2
      WHERE MARK = C_X.
      G_TAB_NF_VINCU_S_WA2-MARK = ''.
      MODIFY IT_NF_VINCU_S
        FROM G_TAB_NF_VINCU_S_WA2
        TRANSPORTING MARK.
    ENDLOOP.
  ENDIF.
  MODIFY IT_NF_VINCU_S
    INDEX TAB_NF_VINCU_S-CURRENT_LINE
    TRANSPORTING MARK.

ENDMODULE.                 " TAB_NF_VINCU_S_MARK  INPUT

*&---------------------------------------------------------------------*
*&      Module  TAB_NF_VINCU_S_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TAB_NF_VINCU_S_USER_COMMAND INPUT.

  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TAB_NF_LIVRES_S'
                              'IT_NF_LIVRE_S'
                              'MARK'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.

ENDMODULE.                 " TAB_NF_VINCU_S_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  TAB_NF_VINCU_S_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TAB_NF_VINCU_S_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_NF_VINCU_S LINES TAB_NF_VINCU_S-LINES.
ENDMODULE.                 " TAB_NF_VINCU_S_CHANGE_TC_ATTR  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_NOTA_S
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VINCULAR_NOTA_S .


  DATA: VG_TABIX            TYPE SY-TABIX,
        VG_SALDO_A_VINCULAR TYPE J_1BNETQTY,
        VG_TOTAL_VINCULADO  TYPE J_1BNETQTY.

  IF VG_QTD_VINCU GT 0.

    PERFORM REAJUSTA_NOTAS USING SPACE.

    VG_SALDO_A_VINCULAR = VG_QTD_VINCU - VG_VINCULADO.

*    IF ( ZDOC_MEMORANDO-QUANTIDADE_MEMO > VG_VINCULADO ).

*      IF ( ZDOC_MEMORANDO-QUANTIDADE_MEMO EQ VG_QTD_VINCU ).
*        VG_SALDO_A_VINCULAR = ZDOC_MEMORANDO-QUANTIDADE_MEMO.
*      ELSE.
*        VG_SALDO_A_VINCULAR = ZDOC_MEMORANDO-QUANTIDADE_MEMO - VG_QTD_VINCU.
*      ENDIF.


    IF VG_SALDO_A_VINCULAR GT 0.

      LOOP AT IT_NF_LIVRE INTO WA_NF_LIVRE WHERE MARK EQ C_X.

        VG_TABIX = SY-TABIX.
        CLEAR: WA_NF_LIVRE-MARK.
        MOVE-CORRESPONDING WA_NF_LIVRE TO WA_NF_VINCU.

        IF VG_SALDO_A_VINCULAR GT WA_NF_LIVRE-SALDO.
          WA_NF_VINCU-MENGE = WA_NF_LIVRE-SALDO.
          "WA_NF_VINCU-MENGE = VG_QTD_VINCU.
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

*              IF ( ZDOC_MEMORANDO-QUANTIDADE_MEMO EQ VG_QTD_VINCU ).
*                VG_VINCULADO        = ZDOC_MEMORANDO-QUANTIDADE_MEMO.
*              ELSE.
*                VG_VINCULADO        = VG_VINCULADO + WA_NF_VINCU-MENGE.
*              ENDIF.
            VG_VINCULADO        = VG_VINCULADO + WA_NF_VINCU-MENGE.
            VG_SALDO_A_VINCULAR = VG_SALDO_A_VINCULAR - WA_NF_VINCU-MENGE.

            DELETE IT_NF_DESV WHERE DOCNUM EQ WA_NF_VINCU-DOCNUM AND ITMNUM EQ WA_NF_VINCU-ITMNUM.

            READ TABLE IT_NF_VINCU_S INTO WA_NF_AUX WITH KEY DOCNUM = WA_NF_VINCU-DOCNUM
                                                           ITMNUM = WA_NF_VINCU-ITMNUM.
            IF SY-SUBRC EQ 0.
              WA_NF_AUX-MENGE = WA_NF_AUX-MENGE + WA_NF_VINCU-MENGE.
              MODIFY IT_NF_VINCU_S INDEX SY-TABIX FROM WA_NF_AUX TRANSPORTING MENGE.
            ELSE.
              APPEND WA_NF_VINCU TO IT_NF_VINCU_S.
            ENDIF.

            MODIFY IT_NF_LIVRE INDEX VG_TABIX FROM WA_NF_LIVRE TRANSPORTING SALDO MARK.
            VG_ALTEROU_NOTAS = C_X.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.
*    ELSE.
*      MESSAGE E026 DISPLAY LIKE C_S.
*    ENDIF.
  ELSE.
    MESSAGE E025 DISPLAY LIKE C_S.
  ENDIF.

ENDFORM.                    " VINCULAR_NOTA_S

*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_NOTA_S
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DESVINCULAR_NOTA_S .

  LOOP AT IT_NF_VINCU_S INTO WA_NF_VINCU WHERE MARK EQ C_X.
    CLEAR: WA_NF_VINCU-MARK.
    APPEND WA_NF_VINCU TO IT_NF_DESV.
    VG_ALTEROU_NOTAS = C_X.
  ENDLOOP.

  DELETE IT_NF_VINCU_S WHERE MARK EQ C_X.

  PERFORM REAJUSTA_NOTAS_S USING C_X.

ENDFORM.                    " DESVINCULAR_NOTA_S

*&---------------------------------------------------------------------*
*&      Form  SALVA_NOTAS_S
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SALVA_NOTAS_S .

  DATA VG_LINES TYPE I.

  VG_LINES = 0.

  IF NOT VG_ALTEROU_NOTAS IS INITIAL.
    DELETE FROM ZDOC_MEMO_NOTA_S WHERE NR_MEMORANDO EQ ZDOC_MEMORANDO-NR_MEMORANDO.

    LOOP AT IT_NF_VINCU_S INTO WA_NF_VINCU.
      WA_NF_VINCU-NR_MEMORANDO = ZDOC_MEMORANDO-NR_MEMORANDO.
      MODIFY ZDOC_MEMO_NOTA_S FROM WA_NF_VINCU.
    ENDLOOP.

    DESCRIBE TABLE IT_NF_VINCU_S LINES VG_LINES.

    IF VG_LINES GT 0.
      ZDOC_MEMORANDO-STATUS = C_F. " C_F PARA FINALIZADO.
      MODIFY ZDOC_MEMORANDO.
    ELSE.
      CLEAR: ZDOC_MEMORANDO-STATUS.
      MODIFY ZDOC_MEMORANDO.
    ENDIF.

    COMMIT WORK.

    MESSAGE S027 WITH ZDOC_MEMORANDO-NR_MEMORANDO DISPLAY LIKE C_S.
    CLEAR VG_ALTEROU_NOTAS.
  ENDIF.

ENDFORM.                    " SALVA_NOTAS_S
