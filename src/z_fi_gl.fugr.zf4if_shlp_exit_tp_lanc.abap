FUNCTION ZF4IF_SHLP_EXIT_TP_LANC.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA: RC            TYPE SY-SUBRC,
        LC_BUKRS      TYPE ZGLT031-BUKRS,
        IT_ZGLT031    TYPE TABLE OF ZGLT031 WITH HEADER LINE,
        IT_TBSL       TYPE TABLE OF TBSL WITH HEADER LINE,
        LC_TABIX      TYPE SY-TABIX,
        POSICAO_INI   TYPE I,
        POSICAO_FINAL TYPE I.

  RANGES: LC_SAKNR FOR ZGLT032-HKONT,
          LC_SHKZG FOR TBSL-SHKZG,
          LC_BSCHL FOR TBSL-BSCHL.

* EXIT immediately, if you do not want to handle this step
  IF CALLCONTROL-STEP <> 'SELONE' AND
     CALLCONTROL-STEP <> 'SELECT' AND
     " AND SO ON
     CALLCONTROL-STEP <> 'DISP'.
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP SELONE  (Select one of the elementary searchhelps)
*"----------------------------------------------------------------------
* This step is only called for collective searchhelps. It may be used
* to reduce the amount of elementary searchhelps given in SHLP_TAB.
* The compound searchhelp is given in SHLP.
* If you do not change CALLCONTROL-STEP, the next step is the
* dialog, to select one of the elementary searchhelps.
* If you want to skip this dialog, you have to return the selected
* elementary searchhelp in SHLP and to change CALLCONTROL-STEP to
* either to 'PRESEL' or to 'SELECT'.
  IF CALLCONTROL-STEP = 'SELONE'.
*   PERFORM SELONE .........
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
*"----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normaly only SHLP-SELOPT should be changed in this step.
  IF CALLCONTROL-STEP = 'PRESEL'.
*   PERFORM PRESEL ..........
    EXIT.
  ENDIF.
*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step.
* Standard function module F4UT_RESULTS_MAP may be very helpfull in this
* step.
  IF CALLCONTROL-STEP = 'SELECT'.

    DATA: LT_FIELDDESCR LIKE DFIES OCCURS 0 WITH HEADER LINE.

    SELECT * INTO TABLE IT_ZGLT031
      FROM ZGLT031 AS C.

    REFRESH RECORD_TAB.
    LT_FIELDDESCR[] = SHLP-FIELDDESCR[].
    DATA: LD_TP_LCTO   LIKE DFIES,
          LD_DESCRICAO LIKE DFIES,
          LD_DPTO_RESP LIKE DFIES,
          LD_MOEDA_DOC LIKE DFIES,
          LD_BUKRS     LIKE DFIES.
    READ TABLE LT_FIELDDESCR WITH KEY FIELDNAME = 'TP_LCTO'.
    LD_TP_LCTO = LT_FIELDDESCR.

    READ TABLE LT_FIELDDESCR WITH KEY FIELDNAME = 'DESCRICAO'.
    LD_DESCRICAO = LT_FIELDDESCR.

    READ TABLE LT_FIELDDESCR WITH KEY FIELDNAME = 'DPTO_RESP'.
    LD_DPTO_RESP = LT_FIELDDESCR.

    READ TABLE LT_FIELDDESCR WITH KEY FIELDNAME = 'MOEDA_DOC'.
    LD_MOEDA_DOC = LT_FIELDDESCR.

    READ TABLE LT_FIELDDESCR WITH KEY FIELDNAME = 'BUKRS'.
    LD_BUKRS = LT_FIELDDESCR.
    LOOP AT IT_ZGLT031.
      CLEAR RECORD_TAB.
      RECORD_TAB-STRING+03(10) = IT_ZGLT031-TP_LCTO.
      RECORD_TAB-STRING+13(52) = IT_ZGLT031-DESCRICAO.
      RECORD_TAB-STRING+65(04) = IT_ZGLT031-BUKRS.
      RECORD_TAB-STRING+70(05) = IT_ZGLT031-MOEDA_DOC.
      APPEND RECORD_TAB.
    ENDLOOP.
    SORT RECORD_TAB.
    CALLCONTROL-STEP = 'DISP'.

    "EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.
*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
  IF CALLCONTROL-STEP = 'DISP'.

    "modelo de linha
    "   0000000049LANÇAMENTO NORMAL                                   0001BRL
    "12345678901234567890123456789012345678901234567890123456789012345678901234567890
    "        0         1         2         3         4         5         6         7

    GET PARAMETER ID 'ZBANK_SAKNR' FIELD LC_SAKNR-LOW.

    IF LC_SAKNR-LOW IS NOT INITIAL.
      GET PARAMETER ID 'ZSHKZG'      FIELD LC_SHKZG-LOW.
      GET PARAMETER ID 'BUK'         FIELD LC_BUKRS.

      IF LC_SAKNR-LOW IS NOT INITIAL.
        LC_SAKNR-SIGN   = 'I'.
        LC_SAKNR-OPTION = 'EQ'.
        LC_SAKNR-HIGH   = LC_SAKNR-LOW.
        APPEND LC_SAKNR.
      ENDIF.

      IF LC_SHKZG-LOW IS NOT INITIAL.
        LC_SHKZG-SIGN   = 'I'.
        LC_SHKZG-OPTION = 'EQ'.
        LC_SHKZG-HIGH   = LC_SHKZG-LOW.
        APPEND LC_SHKZG.

        SELECT * INTO TABLE IT_TBSL
          FROM TBSL
         WHERE SHKZG IN LC_SHKZG.

        LOOP AT IT_TBSL.
          LC_BSCHL-SIGN   = 'I'.
          LC_BSCHL-OPTION = 'EQ'.
          LC_BSCHL-LOW    = IT_TBSL-BSCHL.
          LC_BSCHL-HIGH   = IT_TBSL-BSCHL.
          APPEND LC_BSCHL.
        ENDLOOP.
      ENDIF.

      IF LC_BUKRS IS NOT INITIAL.
        SELECT * INTO TABLE IT_ZGLT031
          FROM ZGLT031 AS C
         WHERE C~ST_CONC_BANC EQ 'X'
           AND EXISTS ( SELECT *
                          FROM ZGLT032 AS I
                         WHERE I~TP_LCTO EQ C~TP_LCTO
                           AND I~HKONT   IN LC_SAKNR
                           AND I~BSCHL   IN LC_BSCHL
                           AND EXISTS ( SELECT * FROM SKB1 AS B WHERE B~BUKRS EQ LC_BUKRS AND B~SAKNR EQ I~HKONT ) ).     "#EC CI_DB_OPERATION_OK[2431747]
      ELSE.
        SELECT * INTO TABLE IT_ZGLT031
          FROM ZGLT031 AS C
         WHERE C~ST_CONC_BANC EQ 'X'
           AND EXISTS ( SELECT * FROM ZGLT032 AS I
                         WHERE I~TP_LCTO EQ C~TP_LCTO
                           AND I~HKONT   IN LC_SAKNR
                           AND I~BSCHL   IN LC_BSCHL ).
      ENDIF.

      IF SY-SUBRC IS INITIAL.
        SORT IT_ZGLT031 BY TP_LCTO.
        LOOP AT RECORD_TAB.
          LC_TABIX = SY-TABIX.
          IF RECORD_TAB+65(04) IS NOT INITIAL AND RECORD_TAB+65(04) NE LC_BUKRS.
            DELETE RECORD_TAB INDEX LC_TABIX.
          ELSE.
            READ TABLE IT_ZGLT031 WITH KEY TP_LCTO = RECORD_TAB+3(10) BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL.
              DELETE RECORD_TAB INDEX LC_TABIX.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
        CLEAR: RECORD_TAB[].
      ENDIF.
    ENDIF.

  ENDIF.

ENDFUNCTION.
