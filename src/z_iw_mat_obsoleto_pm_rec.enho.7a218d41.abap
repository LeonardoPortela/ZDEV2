"Name: \PR:SAPFMMEX\FO:MT06E_STATUS\SE:BEGIN\EI
ENHANCEMENT 0 Z_IW_MAT_OBSOLETO_PM_REC.

DATA: WA_CAUFVD TYPE CAUFVD,
      CK_COND   TYPE C.

FIELD-SYMBOLS: <CAUFVD> TYPE ANY.

IF SY-TCODE EQ 'IW31' OR SY-TCODE EQ 'IW32' OR SY-TCODE EQ 'IW34' OR SY-TCODE EQ 'IW21' OR SY-TCODE EQ 'IW22'.
  ASSIGN ('(SAPLCOIH)CAUFVD') TO <CAUFVD>.
  IF <CAUFVD> IS ASSIGNED.
    WA_CAUFVD = <CAUFVD>.
    IF ( WA_CAUFVD-STTXT(3) EQ 'LIB' OR WA_CAUFVD-STTXT(3) EQ 'ABE' ) AND WA_CAUFVD-AUTYP EQ '30'.
      MOVE ABAP_TRUE TO CK_COND.
    ENDIF.
  ENDIF.
ENDIF.

IF CK_COND EQ ABAP_TRUE.

*------- Pruefen Loeschvormerkung -------------------------------------*
  MMPUR_METAFIELD MMMFD_MATNR.
  IF I_MTCOR-LVORM NE SPACE.
    IF I_MT06E-MPROF IS INITIAL OR                          "45A HTN
       I_MT06E-BMATN IS INITIAL.                            "45A HTN
      PERFORM ENACO USING 'ME' '051'.
      CASE SY-SUBRC.
        WHEN 0.
          MESSAGE E051 WITH I_MTCOM-MATNR.
        WHEN 1.
          MESSAGE W051 WITH I_MTCOM-MATNR INTO GL_DUMMY.
          MMPUR_MESSAGE 'W' 'ME' '051' I_MTCOM-MATNR '' '' ''.
        WHEN 2.
          MESSAGE E051 WITH I_MTCOM-MATNR.
      ENDCASE.
    ELSE.
*... Bestandsgeführtes Material mit  HTN ist zum Löschen vorgemerkt ..*
      PERFORM ENACO USING 'ME' '484'.
      CASE SY-SUBRC.
        WHEN 0.
          MESSAGE E484 WITH I_MT06E-BMATN I_MTCOM-MATNR.
        WHEN 1.
          MESSAGE W484 WITH I_MT06E-BMATN I_MTCOM-MATNR INTO GL_DUMMY.
          MMPUR_MESSAGE 'W' 'ME' '484' I_MT06E-BMATN I_MTCOM-MATNR ''
                        ''.
        WHEN 2.
          MESSAGE E484 WITH I_MT06E-BMATN I_MTCOM-MATNR.
      ENDCASE.
    ENDIF.
  ENDIF.
*
*... Löschvormerkung des HTN .........................................*
  IF I_MTCOR-MPN_LVORM NE SPACE AND
     NOT I_MT06E-MPROF IS INITIAL.
    PERFORM ENACO USING 'ME' '051'.
    CASE SY-SUBRC.
      WHEN 0.
        MESSAGE E051 WITH I_MTCOM-MATNR.
      WHEN 1.
        MESSAGE W051 WITH I_MTCOM-MATNR INTO GL_DUMMY.
        MMPUR_MESSAGE 'W' 'ME' '051' I_MTCOM-MATNR '' '' ''.
      WHEN 2.
        MESSAGE E051 WITH I_MTCOM-MATNR.
    ENDCASE.
  ENDIF.
*
*------- Pruefen Status ---- ( Werksebene ) ---------------------------*
  IF I_MT06E-MMSTA NE SPACE.
    IF I_MT06E-MMSTD IS INITIAL OR I_MT06E-MMSTD LE SY-DATLO.
      SELECT SINGLE * FROM T141 WHERE MMSTA EQ I_MT06E-MMSTA.
      IF SY-SUBRC NE 0.
        MESSAGE E052 WITH I_MT06E-MMSTA I_MTCOM-MATNR I_MTCOM-WERKS. "#EC *
      ENDIF.
      SELECT SINGLE * FROM T141T WHERE SPRAS EQ SY-LANGU
                                   AND MMSTA EQ T141-MMSTA.
*------- Modificação CS2016001207
      CLEAR T141-DEINK.
*------- Modificação CS2016001207
      CASE T141-DEINK.
        WHEN 'B'.                      "Error
          MESSAGE E053 WITH T141T-MTSTB I_MTCOM-MATNR I_MTCOM-WERKS. "#EC *
        WHEN 'A'.                      "Dialog
          MESSAGE W054 WITH I_MTCOM-MATNR I_MTCOM-WERKS T141T-MTSTB INTO
          GL_DUMMY.
          MMPUR_MESSAGE 'W' 'ME' '054' I_MTCOM-MATNR I_MTCOM-WERKS
                                       T141T-MTSTB ''.
      ENDCASE.
    ENDIF.
  ENDIF.

*------- Pruefen Status ---- ( Mandantenebene ) -----------------------*
  IF I_MT06E-MSTAE NE SPACE.
    IF I_MT06E-MSTDE IS INITIAL OR I_MT06E-MSTDE LE SY-DATLO.
      SELECT SINGLE * FROM T141 WHERE MMSTA EQ I_MT06E-MSTAE.
      IF SY-SUBRC NE 0.
        MESSAGE E052 WITH I_MT06E-MSTAE I_MTCOM-MATNR.
      ENDIF.
      SELECT SINGLE * FROM T141T WHERE SPRAS EQ SY-LANGU
                                   AND MMSTA EQ T141-MMSTA.
*------- Modificação CS2017000674
      CLEAR T141-DEINK.
*------- Modificação CS2017000674
      CASE T141-DEINK.
        WHEN 'B'.                      "Error
          MESSAGE E053 WITH T141T-MTSTB I_MTCOM-MATNR.
        WHEN 'A'.                      "Dialog
          MESSAGE W324 WITH I_MTCOM-MATNR T141T-MTSTB INTO GL_DUMMY.
          MMPUR_MESSAGE 'W' 'ME' '324' I_MTCOM-MATNR T141T-MTSTB '' ''.
      ENDCASE.
    ENDIF.
  ENDIF.

*-- Erweiterte Statusprüfungen für IS-R -------------------------------*
  IF NOT I_MT06E-DATAB IS INITIAL AND SY-DATLO LT I_MT06E-DATAB AND
     I_MT06E-DATAB NE SPACE.           "wegen alten Materialien
* Material noch nicht gültig
    MESSAGE E325 WITH I_MTCOM-MATNR I_MT06E-DATAB.
  ENDIF.
  IF NOT I_MT06E-LIQDT IS INITIAL AND SY-DATLO GT I_MT06E-LIQDT AND
     I_MT06E-LIQDT NE SPACE.           "wegen alten Materialien
* Material nicht mehr gültig (schon liquidiert)
    MESSAGE E326 WITH I_MTCOM-MATNR I_MT06E-LIQDT.
  ENDIF.

  CHECK 1 = 2.

ENDIF.

ENDENHANCEMENT.
