*----------------------------------------------------------------------*
*   INCLUDE BILA&I01                                                   *
*----------------------------------------------------------------------*

* AT SELECTION-SCREEN.      >>>>>> im Hauptreport >>>>>>>>>>>
  CHECK SCR_PRINT_TO_FORM = YES.
  IF SSCRFIELDS-UCOMM = 'PRIN'.
     SSCRFIELDS-UCOMM = 'ONLI'.
  ENDIF.

AT SELECTION-SCREEN ON SCR_FORM.
  IF NOT SCR_FORM IS INITIAL.
     SCR_PRINT_TO_FORM = YES.
     PERFORM CHECK_FORM USING SCR_FORM. "Formular aktiv
  ENDIF.

at selection-screen on scr_devi.
  IF NOT SCR_DEVI IS INITIAL.
    PERFORM SCR_CHECK_PRINTER USING SCR_DEVI.
  ENDIF.

* F4 Hilfe für SCR_FORM
* --------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR SCR_FORM.
* SUBMIT RSTXFCAT VIA SELECTION-SCREEN
*         WITH FORM   CP 'F_BILA_ONE*'
*         WITH SPRAS  EQ SY-LANGU
*         AND RETURN.
* GET PARAMETER ID 'TTX' FIELD RSTXD.
* IF NOT RSTXD-TDFORM IS INITIAL.
*   SCR_FORM = RSTXD-TDFORM.
* ENDIF.
  data: HP_FORM_NAME like THEAD-TDFORM.
          CALL FUNCTION 'DISPLAY_FORM_TREE_F4'
               EXPORTING
                    P_TREE_NAME     = 'FI-9'
*                   P_DISPLAY_MODE  = ' '
*                   I_FORM_NAME     =
               IMPORTING
                    P_FORM_NAME     = HP_FORM_NAME
*                   P_FORM_LANGUAGE =
               EXCEPTIONS
*                   CANCELLED       = 1
*                   PARAMETER_ERROR = 2
*                   NOT_FOUND       = 3
                    OTHERS          = 4
                    .
          IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.
   IF SY-SUBRC EQ 0 AND HP_FORM_NAME NE SPACE.
     scr_form = HP_FORM_NAME.
   ENDIF.
