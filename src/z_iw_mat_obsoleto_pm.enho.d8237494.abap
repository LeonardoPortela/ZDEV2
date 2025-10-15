"Name: \PR:SAPLCOSD\FO:CHECK_MATSTAT_COMP\SE:BEGIN\EI
ENHANCEMENT 0 Z_IW_MAT_OBSOLETO_PM.

  DATA: WA_CAUFVD TYPE CAUFVD,
        CK_COND   TYPE C.

  FIELD-SYMBOLS: <CAUFVD> TYPE ANY.

  IF SY-TCODE EQ 'IW31' OR SY-TCODE EQ 'IW32' OR SY-TCODE EQ 'IW34' OR SY-TCODE EQ 'IW21' OR SY-TCODE EQ 'IW22'.
    IF I_AUTYP EQ '30'.

      ASSIGN ('(SAPLCOIH)CAUFVD') TO <CAUFVD>.

      IF <CAUFVD> IS ASSIGNED.
        WA_CAUFVD = <CAUFVD>.
        IF WA_CAUFVD-STTXT(3) EQ 'LIB' or WA_CAUFVD-STTXT(3) EQ 'ABE'.
          MOVE ABAP_TRUE TO CK_COND.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF CK_COND EQ ABAP_TRUE.

    DATA: TXT_TMP_2 LIKE T141T-MTSTB.
    DATA: T_KLVAR_2               LIKE TCK03-KLVAR,
          S_RELEVANT_TO_COSTING_2.
    DATA: T141_MARA_2 LIKE T141,
          T141_MARC_2 LIKE T141,
          T141_COMP_2 LIKE T141.
    DATA: PSINFO_CALL_TRANSACTION_2.
    IMPORT PSINFO_CALL_TRANSACTION_2 FROM MEMORY ID 'PSINFO_CALL_TRANS'.
    DATA: FLG_PS_IF_2.
    DATA: L_MMSTA_2 LIKE STB-MMSTA,
          L_MSTAE_2 LIKE STB-MMSTA.
    DATA: L_MSGTY_2 TYPE SY-MSGTY.
    DATA: L_NO_DIALOG_2 TYPE FLAG.

    L_MMSTA_2 = I_MMSTA.
    L_MSTAE_2 = I_MSTAE.

    CLEAR E_ANTWORT.
*       Status valid at explosion date?
    IF I_AUFLD LT I_MMSTD.
      CLEAR L_MMSTA_2.
    ENDIF.
    IF I_AUFLD LT I_MSTDE.
      CLEAR L_MSTAE_2.
    ENDIF.
*       Status angegeben????
    IF L_MMSTA_2 IS INITIAL AND
       L_MSTAE_2 IS INITIAL.
      EXIT.
    ENDIF.

*      ...Materialstatus lt. Tab. 141
*      ...Werksstatus lesen
    IF NOT L_MMSTA_2 IS INITIAL.
      IF T141-MMSTA NE L_MMSTA_2.
        CALL FUNCTION 'CO_TA_T141_READ'
          EXPORTING
            T141_MMSTA = L_MMSTA_2
          IMPORTING
            T141WA     = T141
          EXCEPTIONS
            NOT_FOUND  = 1.
        IF SY-SUBRC NE 0.
          IF I_PRODCOST IS INITIAL.
            MESSAGE A053 WITH L_MMSTA_2 I_MATNR I_WERKS.
          ELSE.
            CALL FUNCTION 'CM_F_MESSAGE'
              EXPORTING
                ARBGB            = 'CK'
                MSGNR            = '355'
                MSGTY            = 'E'
                MSGV1            = L_MMSTA_2
                MSGV2            = I_MATNR
                MSGV3            = I_WERKS
                OBJECT_DEPENDENT = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
      T141_MARC_2 = T141.
    ENDIF.
*      ...Materialstatus lt. Tab. 141
*      ...Konzernstatus lesen
    IF NOT L_MSTAE_2 IS INITIAL.
      IF T141-MMSTA NE L_MSTAE_2.
        CALL FUNCTION 'CO_TA_T141_READ'
          EXPORTING
            T141_MMSTA = L_MSTAE_2
          IMPORTING
            T141WA     = T141
          EXCEPTIONS
            NOT_FOUND  = 1.
        IF SY-SUBRC NE 0.
          IF I_PRODCOST IS INITIAL.
            MESSAGE A053 WITH L_MSTAE_2 I_MATNR I_WERKS.
          ELSE.
            CALL FUNCTION 'CM_F_MESSAGE'
              EXPORTING
                ARBGB            = 'CK'
                MSGNR            = '355'
                MSGTY            = 'E'
                MSGV1            = L_MSTAE_2
                MSGV2            = I_MATNR
                MSGV3            = I_WERKS
                OBJECT_DEPENDENT = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
      T141_MARA_2 = T141.
    ENDIF.

    CASE I_AUTYP.
*      --- Instandhaltung ---------------------------------------------------*
      WHEN AUFTRAGSTYP-INST.
*       If called by BAPI => send no messages!                         "626009
        IF G_IBAPI_ACTIVE = SPACE.
          CALL FUNCTION 'FUNCTION_EXISTS'
            EXPORTING
              FUNCNAME           = 'IBAPI_Z_GET_BAPI_FLAG'
            EXCEPTIONS
              FUNCTION_NOT_EXIST = 1
              OTHERS             = 2.
          IF SY-SUBRC = 0.
            CALL FUNCTION 'IBAPI_Z_GET_BAPI_FLAG'             "#EC *
              EXCEPTIONS
                BAPI_ACTIVE     = 1
                BAPI_NOT_ACTIVE = 2
                OTHERS          = 3.
            IF SY-SUBRC = 1.
              G_IBAPI_ACTIVE = YX.
            ELSE.
              G_IBAPI_ACTIVE = YNEIN.
            ENDIF.
          ELSE.
            G_IBAPI_ACTIVE = YNEIN.
          ENDIF.
        ENDIF.

        CLEAR: T141_COMP_2.
        IF     T141_MARC_2-DINST = CHARB.
          T141_COMP_2 = T141_MARC_2.
        ELSEIF T141_MARA_2-DINST = CHARB.
          T141_COMP_2 = T141_MARA_2.
        ELSEIF T141_MARC_2-DINST = CHARA.
          T141_COMP_2 = T141_MARC_2.
        ELSEIF T141_MARA_2-DINST = CHARA.
          T141_COMP_2 = T141_MARA_2.
        ENDIF.
*      ...Modificação CS201601217
        clear T141_COMP_2-DINST.
*      ...Materialstatus fuer Auftragsposition
        CASE T141_COMP_2-DINST.
          WHEN CHARB.
*      .......Fertigung nicht erlaubt
            IF SY-BINPT IS INITIAL AND
               G_IBAPI_ACTIVE <> YX.                            "626009
*      ...Text zum Materialstatus lt. Tab. 141T.
              CALL FUNCTION 'CO_TA_T141T_READ'
                EXPORTING
                  MMSTA     = T141_COMP_2-MMSTA
                  SPRAS     = SY-LANGU
                IMPORTING
                  TEXT      = TXT_TMP_2
                EXCEPTIONS
                  NOT_FOUND = 01.
              CALL FUNCTION 'POPUP_FOR_INFORMATION'
                EXPORTING
                  KOMPONENTE = I_MATNR
                  STATUS     = TXT_TMP_2.
            ELSEIF G_IBAPI_ACTIVE = YX.                               "note1144417
*      ...Text zum Materialstatus lt. Tab. 141T.
              CALL FUNCTION 'CO_TA_T141T_READ'                        "note1144417
                EXPORTING                                             "note1144417
                  MMSTA     = T141_COMP_2-MMSTA                       "note1144417
                  SPRAS     = SY-LANGU                                "note1144417
                IMPORTING                                             "note1144417
                  TEXT      = TXT_TMP_2                               "note1144417
                EXCEPTIONS                                            "note1144417
                  NOT_FOUND = 01.                                     "note1144417
              MESSAGE E054(CO) WITH I_MATNR TXT_TMP_2 INTO TXT_TMP_2. "note1144417
            ENDIF.
            E_ANTWORT = YNEIN.
          WHEN CHARA.
*      ..Fertigung erlaubt, aber Warnung
            IF SY-BINPT IS INITIAL AND
               G_IBAPI_ACTIVE <> YX.                            "626009
*      ...Text zum Materialstatus lt. Tab. 141T.
              CALL FUNCTION 'CO_TA_T141T_READ'
                EXPORTING
                  MMSTA     = T141_COMP_2-MMSTA
                  SPRAS     = SY-LANGU
                IMPORTING
                  TEXT      = TXT_TMP_2
                EXCEPTIONS
                  NOT_FOUND = 01.
              CALL FUNCTION 'POPUP_TO_DECIDE_WM'
                EXPORTING
                  KOMPONENTE     = I_MATNR
                  STATUS         = TXT_TMP_2
                IMPORTING
                  ANSWER         = RET
                EXCEPTIONS
                  TITEL_TOO_LONG = 01.
              IF     RET = YNEIN.
                E_ANTWORT = RET.
              ENDIF.
            ELSEIF G_IBAPI_ACTIVE = YX.                         "1144417
*       im BAPI bei Warnung Materialübernahme                    "1144417
              E_ANTWORT = YJA.                                  "1144417
            ELSE.
*       im B/I (Wartungsplanung) keine Materialübernahme in PM-Auftrag
              E_ANTWORT = YNEIN.
            ENDIF.
*      ..Fertigung grundsaetzlich erlaubt
          WHEN OTHERS.
            "check 1 = 2.
        ENDCASE.
*      --- Restliche Auftragstypen ------------------------------------------*
      WHEN OTHERS.
        IF I_AUTYP EQ AUFTRAGSTYP-NETW.
          CALL FUNCTION 'CO_MK_GET_FLG_INTERFACE'
            IMPORTING
              FLG_EXP = FLG_PS_IF_2.
        ENDIF.
        IF I_PRODCOST IS INITIAL.
          CLEAR: T141_COMP_2.
          IF     T141_MARC_2-DFAPO = CHARB.
            T141_COMP_2 = T141_MARC_2.
          ELSEIF T141_MARA_2-DFAPO = CHARB.
            T141_COMP_2 = T141_MARA_2.
          ELSEIF T141_MARC_2-DFAPO = CHARA.
            T141_COMP_2 = T141_MARC_2.
          ELSEIF T141_MARA_2-DFAPO = CHARA.
            T141_COMP_2 = T141_MARA_2.
          ENDIF.

*       Production and process orders: Check if dialog is possible
          IF ( I_AUTYP = AUFTRAGSTYP-FERT OR
               I_AUTYP = AUFTRAGSTYP-BORD    ) AND
             ( T141_COMP_2-DFAPO = CHARB    OR
               T141_COMP_2-DFAPO = CHARA       ).
            PERFORM FLG_NO_DIALOG_READ(SAPLCOZF) CHANGING L_NO_DIALOG_2.
          ENDIF.

*      ...Materialstatus fuer Komponenten
          CASE T141_COMP_2-DFAPO.
            WHEN CHARB.
*      .......Fertigung nicht erlaubt
*      ...Text zum Materialstatus lt. Tab. 141T.
              CALL FUNCTION 'CO_TA_T141T_READ'
                EXPORTING
                  MMSTA     = T141_COMP_2-MMSTA
                  SPRAS     = SY-LANGU
                IMPORTING
                  TEXT      = TXT_TMP_2
                EXCEPTIONS
                  NOT_FOUND = 01.
              IF    I_AUTYP EQ AUFTRAGSTYP-FERT
                 OR I_AUTYP EQ AUFTRAGSTYP-BORD.
*       Read customizing of message C2 257 in case of production
*       or process orders
                CALL FUNCTION 'CO_MES_GET_MESS_TYPE'
                  EXPORTING
                    I_MSGID      = CON_MSGID_C2
                    I_MSGNO      = '257'
                    SEND_MESSAGE = SPACE
                  IMPORTING
                    E_MSGTY      = L_MSGTY_2
                  EXCEPTIONS
                    OTHERS       = 1.
                IF SY-SUBRC <> 0.
*       By default the message should be an info
                  L_MSGTY_2 = CON_MSGTY_INFO.
                ENDIF.
*                   warnings or errors can lead to orders without any
*                   material components   > adapt message type
                IF L_MSGTY_2 = CON_MSGTY_WARNING.
*                     info instead of warning so that processing continues
                  L_MSGTY_2 = CON_MSGTY_INFO.
                ENDIF.
                IF L_MSGTY_2 = CON_MSGTY_ERROR.
*                     abort in case of error so that processing stops
                  L_MSGTY_2 = CON_MSGTY_ABORT.
                ENDIF.
                IF ( SY-BINPT    <> SPACE     OR
                     L_NO_DIALOG_2 <> SPACE       ) AND
                   ( L_MSGTY_2 = CON_MSGTY_INFO OR
                     L_MSGTY_2 = CON_MSGTY_POPUP  ).
*       In batch input instead of a popup or info message
*       a status message should be sent
                  L_MSGTY_2 = CON_MSGTY_SUCCESS.
                ENDIF.
                CASE L_MSGTY_2.
                  WHEN CON_MSGTY_NOTHING.
*       Do not send any message
                  WHEN CON_MSGTY_POPUP.
*       Process a popup
                    CALL FUNCTION 'POPUP_FOR_INFORMATION'
                      EXPORTING
                        KOMPONENTE  = I_MATNR
                        STATUS      = TXT_TMP_2
                        PLANAUFTRAG = AFPOD-PLNUM.
                  WHEN OTHERS.
*       Send message
                    CLEAR SY-MSGV3.
                    IF NOT AFPOD-PLNUM IS INITIAL.
                      CONCATENATE TEXT-PA9 AFPOD-PLNUM INTO SY-MSGV3
                                  SEPARATED BY SPACE.
                    ENDIF.
                    MESSAGE ID CON_MSGID_C2 TYPE L_MSGTY_2 NUMBER '257'
                            WITH I_MATNR TXT_TMP_2 SY-MSGV3.
                ENDCASE.
              ELSE.
                IF     SY-BINPT IS INITIAL OR
                   NOT PSINFO_CALL_TRANSACTION_2 IS INITIAL. "called from IS
                  CALL FUNCTION 'POPUP_FOR_INFORMATION'
                    EXPORTING
                      KOMPONENTE  = I_MATNR
                      STATUS      = TXT_TMP_2
                      PLANAUFTRAG = AFPOD-PLNUM.
                ENDIF.
              ENDIF.
              E_ANTWORT = YNEIN.
            WHEN CHARA.
*      ..Fertigung erlaubt, aber Warnung
*      ...Text zum Materialstatus lt. Tab. 141T.
              CALL FUNCTION 'CO_TA_T141T_READ'
                EXPORTING
                  MMSTA     = T141_COMP_2-MMSTA
                  SPRAS     = SY-LANGU
                IMPORTING
                  TEXT      = TXT_TMP_2
                EXCEPTIONS
                  NOT_FOUND = 01.


              IF ( SY-BINPT    = SPACE AND
                   L_NO_DIALOG_2 = SPACE     ) OR
                 NOT PSINFO_CALL_TRANSACTION_2 IS INITIAL. "called from IS
                CALL FUNCTION 'POPUP_TO_DECIDE_WM'
                  EXPORTING
                    KOMPONENTE        = I_MATNR
                    STATUS            = TXT_TMP_2
                    PLANAUFTRAG       = AFPOD-PLNUM
                    I_CANCEL_POSSIBLE = I_CANCEL
                  IMPORTING
                    ANSWER            = RET
                  EXCEPTIONS
                    TITEL_TOO_LONG    = 01.
              ELSE.
                ANTWORT = YJA.
              ENDIF.
              IF     RET = YNEIN.
                E_ANTWORT = RET.
              ELSEIF RET = 'C'.   "Cancel
                IF NOT CAUFVD-UMSKZ IS INITIAL.
                  MESSAGE ID 'CO' TYPE 'E' NUMBER 650.
                ELSE.
                  PERFORM LEAVE USING I_AUTYP.
                ENDIF.
              ENDIF.
*      ..Fertigung grundsaetzlich erlaubt
            WHEN OTHERS.
          ENDCASE.
        ELSE.
*      ...Materialstatus fuer Kalkulation
          IF I_SANKA IS INITIAL.
            EXIT.
          ELSE.
            CALL FUNCTION 'CK_F_PRODUCTCOSTING'
              IMPORTING
                COSTINGVARIANT = T_KLVAR_2.
            CALL FUNCTION 'CK_F_RELEVANT_TO_COSTING_CHECK'
              EXPORTING
                SELKZ               = I_SANKA
                KLVAR               = T_KLVAR_2
              IMPORTING
                RELEVANT_TO_COSTING = S_RELEVANT_TO_COSTING_2.
            IF S_RELEVANT_TO_COSTING_2 IS INITIAL.
              EXIT.
            ENDIF.
          ENDIF.
          CLEAR T141_COMP_2.
          IF     T141_MARC_2-DERZK = CHARB OR
                 T141_MARC_2-DERZK = 'D'.
            T141_COMP_2 = T141_MARC_2.
          ELSEIF T141_MARA_2-DERZK = CHARB OR
                 T141_MARA_2-DERZK = 'D'.
            T141_COMP_2 = T141_MARA_2.
          ELSEIF T141_MARC_2-DERZK = CHARA OR
                 T141_MARC_2-DERZK = 'C'.
            T141_COMP_2 = T141_MARC_2.
          ELSEIF T141_MARA_2-DERZK = CHARA OR
                 T141_MARA_2-DERZK = 'C'.
            T141_COMP_2 = T141_MARA_2.
          ENDIF.
          CASE T141_COMP_2-DERZK.
            WHEN CHARB.
*      .......Kalkulation mit Error durchführen
              CALL FUNCTION 'CO_TA_T141T_READ'
                EXPORTING
                  MMSTA     = T141_COMP_2-MMSTA
                  SPRAS     = SY-LANGU
                IMPORTING
                  TEXT      = TXT_TMP_2
                EXCEPTIONS
                  NOT_FOUND = 01.
              CALL FUNCTION 'CM_F_MESSAGE'
                EXPORTING
                  ARBGB            = 'CK'
                  MSGNR            = '354'
                  MSGTY            = 'E'
                  MSGV1            = I_MATNR
                  MSGV2            = I_WERKS
                  MSGV3            = T141_COMP_2-MMSTA
                  MSGV4            = TXT_TMP_2
                  OBJECT_DEPENDENT = 'X'.
            WHEN CHARA.
*      ..Kalkulation mit Warnung
              CALL FUNCTION 'CO_TA_T141T_READ'
                EXPORTING
                  MMSTA     = T141_COMP_2-MMSTA
                  SPRAS     = SY-LANGU
                IMPORTING
                  TEXT      = TXT_TMP_2
                EXCEPTIONS
                  NOT_FOUND = 01.
              CALL FUNCTION 'CM_F_MESSAGE'
                EXPORTING
                  ARBGB            = 'CK'
                  MSGNR            = '354'
                  MSGTY            = 'W'
                  MSGV1            = I_MATNR
                  MSGV2            = I_WERKS
                  MSGV3            = T141_COMP_2-MMSTA
                  MSGV4            = TXT_TMP_2
                  OBJECT_DEPENDENT = 'X'.
            WHEN 'D'.
              CALL FUNCTION 'CO_TA_T141T_READ'
                EXPORTING
                  MMSTA     = T141_COMP_2-MMSTA
                  SPRAS     = SY-LANGU
                IMPORTING
                  TEXT      = TXT_TMP_2
                EXCEPTIONS
                  NOT_FOUND = 01.
*      .......Kalkulation mit Error durchführen
              CALL FUNCTION 'CM_F_MESSAGE'
                EXPORTING
                  ARBGB            = 'CK'
                  MSGNR            = '354'
                  MSGTY            = 'E'
                  MSGV1            = I_MATNR
                  MSGV2            = I_WERKS
                  MSGV3            = T141_COMP_2-MMSTA
                  MSGV4            = TXT_TMP_2
                  OBJECT_DEPENDENT = 'X'.
              E_ANTWORT = YNEIN.
            WHEN 'C'.
              CALL FUNCTION 'CO_TA_T141T_READ'
                EXPORTING
                  MMSTA     = T141_COMP_2-MMSTA
                  SPRAS     = SY-LANGU
                IMPORTING
                  TEXT      = TXT_TMP_2
                EXCEPTIONS
                  NOT_FOUND = 01.
*      ..Kalkulation mit Warnung
              CALL FUNCTION 'CM_F_MESSAGE'
                EXPORTING
                  ARBGB            = 'CK'
                  MSGNR            = '354'
                  MSGTY            = 'W'
                  MSGV1            = I_MATNR
                  MSGV2            = I_WERKS
                  MSGV3            = T141_COMP_2-MMSTA
                  MSGV4            = TXT_TMP_2
                  OBJECT_DEPENDENT = 'X'.
              E_ANTWORT = YNEIN.
*      ..Kalkulation grundsaetzlich erlaubt
            WHEN OTHERS.
          ENDCASE.
        ENDIF.
    ENDCASE.

    check 1 = 2.

  ENDIF.

ENDENHANCEMENT.
