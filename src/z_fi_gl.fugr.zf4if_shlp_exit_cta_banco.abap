FUNCTION ZF4IF_SHLP_EXIT_CTA_BANCO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA: RC       TYPE SY-SUBRC,
        LC_TABIX TYPE SY-TABIX.

  DATA: IT_TBSL TYPE TABLE OF TBSL WITH HEADER LINE,
        IT_CONT TYPE TABLE OF ZVW_CTA_BANCO WITH HEADER LINE.

  RANGES: LC_BUKRS     FOR ZVW_CTA_BANCO-BUKRS,  "Empresa
          LC_TP_LCTO   FOR ZGLT032-TP_LCTO,      "Tipo de Lançamento
          LC_DEB_CRED  FOR TBSL-SHKZG,           "Lançamento de Débito/Crédito
          LC_CTA_BANCO FOR ZVW_CTA_BANCO-SAKNR,  "Conta Bancária de Origem
          LC_BSCHL     FOR TBSL-BSCHL.           "Chaves Permitidas

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
    "PERFORM STEP_SELECT TABLES RECORD_TAB SHLP_TAB CHANGING SHLP CALLCONTROL RC.
*    IF RC = 0.
*      CALLCONTROL-STEP = 'DISP'.
*    ELSE.
*      CALLCONTROL-STEP = 'EXIT'.
*    ENDIF.
    EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.
*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
  IF CALLCONTROL-STEP = 'DISP'.

    "modelo de linha
    "   00010000111000BRL
    "12345678901234567890123456789012345678901234567890123456789012345678901234567890
    "        0         1         2         3         4         5         6         7

    GET PARAMETER ID 'ZBANK_SAKNR' FIELD LC_CTA_BANCO-LOW.

    IF LC_CTA_BANCO-LOW IS NOT INITIAL.
      GET PARAMETER ID 'ZDEB_CRED'   FIELD LC_DEB_CRED-LOW.
      GET PARAMETER ID 'ZTP_LCTO'    FIELD LC_TP_LCTO-LOW.
      GET PARAMETER ID 'BUK'         FIELD LC_BUKRS-LOW.

      "Conta Bancária de Origem
      IF LC_CTA_BANCO-LOW IS NOT INITIAL.
        LC_CTA_BANCO-SIGN   = 'I'.
        LC_CTA_BANCO-OPTION = 'EQ'.
        LC_CTA_BANCO-HIGH   = LC_CTA_BANCO-LOW.
        APPEND LC_CTA_BANCO.
      ENDIF.

      "Chaves Permitidas de lançamento
      IF LC_DEB_CRED-LOW IS NOT INITIAL.
        LC_DEB_CRED-SIGN   = 'I'.
        LC_DEB_CRED-OPTION = 'EQ'.
        LC_DEB_CRED-HIGH   = LC_DEB_CRED-LOW.
        APPEND LC_DEB_CRED.

        SELECT * INTO TABLE IT_TBSL
          FROM TBSL
         WHERE SHKZG IN LC_DEB_CRED.

        LOOP AT IT_TBSL.
          LC_BSCHL-SIGN   = 'I'.
          LC_BSCHL-OPTION = 'EQ'.
          LC_BSCHL-LOW    = IT_TBSL-BSCHL.
          LC_BSCHL-HIGH   = IT_TBSL-BSCHL.
          APPEND LC_BSCHL.
        ENDLOOP.
      ENDIF.

      "Empresas Permitidas
      IF LC_BUKRS-LOW IS NOT INITIAL.
        LC_BUKRS-SIGN   = 'I'.
        LC_BUKRS-OPTION = 'EQ'.
        LC_BUKRS-HIGH   = LC_BUKRS-LOW.
        APPEND LC_BUKRS.
      ENDIF.

      "Tipo de lançamentos
      IF LC_TP_LCTO-LOW IS NOT INITIAL.
        LC_TP_LCTO-SIGN   = 'I'.
        LC_TP_LCTO-OPTION = 'EQ'.
        LC_TP_LCTO-HIGH   = LC_TP_LCTO-LOW.
        APPEND LC_TP_LCTO.
      ENDIF.

      SELECT * INTO TABLE IT_CONT
        FROM ZVW_CTA_BANCO AS C
       WHERE C~BUKRS IN LC_BUKRS
         AND C~SPRAS EQ SY-LANGU
         AND EXISTS ( SELECT * FROM ZGLT032 AS Z
                       WHERE Z~HKONT   EQ C~SAKNR
                         AND Z~TP_LCTO IN LC_TP_LCTO
                         AND Z~BSCHL   IN LC_BSCHL
                         AND Z~HKONT   NOT IN LC_CTA_BANCO
                         AND EXISTS ( SELECT * FROM ZGLT031 AS E WHERE E~TP_LCTO EQ Z~TP_LCTO AND E~ST_CONC_BANC EQ 'X' ) ).

      IF SY-SUBRC IS INITIAL.
        SORT IT_CONT BY BUKRS SAKNR SPRAS.
        LOOP AT RECORD_TAB.
          LC_TABIX = SY-TABIX.
          READ TABLE IT_CONT WITH KEY BUKRS = RECORD_TAB+03(04)
                                      SAKNR = RECORD_TAB+07(10)
                                      SPRAS = RECORD_TAB+22(01) BINARY SEARCH.
          IF SY-SUBRC IS NOT INITIAL.
            DELETE RECORD_TAB INDEX LC_TABIX.
          ENDIF.
        ENDLOOP.
      ELSE.
        CLEAR: RECORD_TAB[].
      ENDIF.
    ENDIF.

  ENDIF.

ENDFUNCTION.
