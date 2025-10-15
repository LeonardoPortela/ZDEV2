*&---------------------------------------------------------------------*
*& Report  Z_EXEC_SHDB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_EXEC_SHDB.
TYPE-POOLS: PMST, SLIS.
DATA: TG_ZSHDB TYPE TABLE OF ZSHDBT0001 WITH HEADER LINE,
      TG_BDC   TYPE TABLE OF BDCDATA WITH HEADER LINE,
      TG_MSG   TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      TG_ZSHDB_MSG TYPE TABLE OF ZSHDBT0002 WITH HEADER LINE,
      WG_PAR   TYPE CTU_PARAMS.

selection-screen: begin of block b1 with frame title text-001.
SELECT-OPTIONS: S_SHDBNR FOR TG_ZSHDB-SHDBNR.
PARAMETERS: P_PROG  TYPE SY-REPID,
            P_FORM  TYPE SLIS_FORMNAME,
            P_DISM  TYPE CTU_PARAMS-DISMODE,
            P_UPDM  TYPE CTU_PARAMS-UPDMODE,
            P_DEFS  TYPE CTU_PARAMS-DEFSIZE,
            P_RACO  TYPE CTU_PARAMS-RACOMMIT,
            P_NOBI  TYPE CTU_PARAMS-NOBINPT.

selection-screen: end of block b1.

START-OF-SELECTION.
  PERFORM SELECIONA_DADOS.
  PERFORM ORGANIZA_DADOS.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  SELECT *
    FROM ZSHDBT0001
    INTO TABLE TG_ZSHDB
     WHERE SHDBNR IN S_SHDBNR.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZA_DADOS .
  SORT: TG_ZSHDB BY SHDBNR TCODE LINNUM.
  LOOP AT TG_ZSHDB.
    MOVE-CORRESPONDING: TG_ZSHDB TO TG_BDC.
     move: tg_zshdb-programa to tg_bdc-program.

    condense tg_bdc-dynbegin no-gaps.
    condense tg_bdc-program no-gaps.
    condense tg_bdc-dynpro no-gaps.
    condense tg_bdc-fnam no-gaps.
    condense tg_bdc-fval no-gaps.

    APPEND TG_BDC.

    AT END OF TCODE.
      PERFORM CALL_TRANSACTION.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION .
  DATA: WL_MESSAGE TYPE PMST_RAW_MESSAGE.

  WG_PAR-DISMODE =   P_DISM.           "'E'.
  WG_PAR-UPDMODE =   P_UPDM.           "'A'.
  WG_PAR-DEFSIZE =   P_DEFS.           "'X'.
  WG_PAR-RACOMMIT =  P_RACO.           "'X'.
  WG_PAR-NOBINPT =   P_NOBI.           "'X'.

  refresh: tg_zshdb_msg, tg_msg.

  CONDENSE TG_ZSHDB-TCODE NO-GAPS.

  CALL TRANSACTION TG_ZSHDB-TCODE USING TG_BDC
      MESSAGES INTO TG_MSG
        OPTIONS FROM WG_PAR.

  REFRESH: TG_BDC.

  READ TABLE TG_MSG
    WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC IS NOT INITIAL.
    DELETE FROM ZSHDBT0001 WHERE TCODE EQ TG_ZSHDB-TCODE
                             AND SHDBNR EQ TG_ZSHDB-SHDBNR.

    DELETE FROM ZSHDBT0002 WHERE TCODE EQ TG_ZSHDB-TCODE
                             AND SHDBNR EQ TG_ZSHDB-SHDBNR.

  ELSE.
    DELETE FROM ZSHDBT0002 WHERE TCODE EQ TG_ZSHDB-TCODE
                             AND SHDBNR EQ TG_ZSHDB-SHDBNR.

    LOOP AT TG_MSG WHERE MSGTYP EQ 'E'.
      CLEAR: WL_MESSAGE.
      MOVE-CORRESPONDING: TG_MSG TO TG_ZSHDB_MSG.
      MOVE: TG_ZSHDB-TCODE TO TG_ZSHDB_MSG-TCODE,
            TG_ZSHDB-SHDBNR TO TG_ZSHDB_MSG-SHDBNR.

      add 1 to tg_zshdb_msg-linnum.

      TG_ZSHDB_MSG-MSGNR = SY-MSGNO = TG_MSG-MSGNR.
      TG_ZSHDB_MSG-MSGV1 = SY-MSGV1 = TG_MSG-MSGV1.
      TG_ZSHDB_MSG-MSGV2 = SY-MSGV2 = TG_MSG-MSGV2.
      TG_ZSHDB_MSG-MSGV3 = SY-MSGV3 = TG_MSG-MSGV3.
      TG_ZSHDB_MSG-MSGV4 = SY-MSGV4 = TG_MSG-MSGV4.

      CALL FUNCTION 'CUTC_GET_MESSAGE'
        EXPORTING
          MSG_TYPE       = TG_MSG-MSGTYP
          MSG_ID         = TG_MSG-MSGID
          MSG_NO         = SY-MSGNO
          MSG_ARG1       = SY-MSGV1
          MSG_ARG2       = SY-MSGV2
          MSG_ARG3       = SY-MSGV3
          MSG_ARG4       = SY-MSGV4
        IMPORTING
          RAW_MESSAGE    = WL_MESSAGE
        EXCEPTIONS
          MSG_NOT_FOUND  = 1
          INTERNAL_ERROR = 2
          OTHERS         = 3.

      IF ( SY-SUBRC NE 0 ).
        WL_MESSAGE = 'Erro na mensagem do BATCH-INPUT'.
      ENDIF.

      TG_ZSHDB_MSG-msg = WL_MESSAGE.

      MODIFY ZSHDBT0002 FROM TG_ZSHDB_MSG.
    ENDLOOP.
  ENDIF.

  IF P_PROG IS NOT INITIAL
  AND P_FORM IS NOT INITIAL.
    PERFORM (P_FORM) IN PROGRAM (P_PROG) IF FOUND
                     TABLES TG_ZSHDB_MSG
                            TG_ZSHDB.

   ENDIF.
ENDFORM.                    " CALL_TRANSACTION
