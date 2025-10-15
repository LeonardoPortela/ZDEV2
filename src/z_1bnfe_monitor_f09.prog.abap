*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F09
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_scroll_info_via_id
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCROLL_INFO_VIA_ID USING P_MODE TYPE C.

  CASE P_MODE.
    WHEN C_100.
      CALL METHOD CTL_ALV_NFE->SET_SCROLL_INFO_VIA_ID
        EXPORTING
          IS_COL_INFO = GS_SCROLL_COL
          IS_ROW_NO   = GS_SCROLL_ROW.

    WHEN C_102.
      CALL METHOD CTL_CANCEL_ALV->SET_SCROLL_INFO_VIA_ID
        EXPORTING
          IS_COL_INFO = GS_SCROLL_COL_102
          IS_ROW_NO   = GS_SCROLL_ROW_102.

  ENDCASE.

ENDFORM.                    " set_scroll_info_via_id
