*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F12
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  display_nfe
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_NFE USING P_MODE TYPE C.

  CASE P_MODE.
    WHEN C_100.

      IF IT_SELECTED_ROWS IS INITIAL.
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
        RETURN.
      ENDIF.

      READ TABLE IT_SELECTED_ROWS INDEX 2 TRANSPORTING NO FIELDS.

      IF SY-SUBRC = 0.
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '009'.
      ENDIF.

      READ TABLE IT_SELECTED_ROWS INDEX 1
            INTO WA_SELECTED_ROWS.
      READ TABLE IT_NFE_ALV INDEX WA_SELECTED_ROWS-INDEX
            INTO WA_NFE_ALV.

      PERFORM NF_WRITER USING WA_NFE_ALV-DOCNUM.

    WHEN C_102.

      IF IT_SELECTED_ROWS_0102 IS INITIAL.
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
        RETURN.
      ENDIF.

      READ TABLE IT_SELECTED_ROWS_0102 INDEX 2 TRANSPORTING NO FIELDS.

      IF SY-SUBRC = 0.
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '009'.
      ENDIF.

      READ TABLE IT_SELECTED_ROWS_0102 INDEX 1 INTO WA_SELECTED_ROWS_0102.
      READ TABLE IT_CANCEL_ALV INDEX WA_SELECTED_ROWS_0102-INDEX INTO WA_CANCEL_ALV.

      PERFORM NF_WRITER USING WA_CANCEL_ALV-DOCNUM.

  ENDCASE.

ENDFORM.                    " display_nf
