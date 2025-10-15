*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  handle_hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_COLUMN_ID_FIELDNAME  text
*----------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK
       USING VALUE(ROW_ID)    LIKE LVC_S_ROID-ROW_ID
             VALUE(FIELDNAME) LIKE LVC_S_COL-FIELDNAME.


  READ TABLE IT_NFE_ALV    INDEX ROW_ID INTO WA_NFE_ALV.
  READ TABLE IT_NFE_ACTIVE INDEX ROW_ID INTO WA_NFE_ACTIVE.
  GF_DOCNUM = WA_NFE_ALV-DOCNUM.

  CASE FIELDNAME.
*   Diplay NF-e process history in ALV2
    WHEN 'DOCNUM'.
      PERFORM FILL_NFE_HISTORY USING GF_DOCNUM.
      PERFORM FILL_MDFE_REFERENCE USING GF_DOCNUM.
*   Display NF document using the NF Writer
    WHEN 'NFNUM9'.
*del  CHECK wa_nfe_alv-nfnum9 IS NOT INITIAL.        "1265172
      PERFORM NF_WRITER USING WA_NFE_ALV-DOCNUM.

*   Display NF reference document in the NF Writer
    WHEN 'REFNUM'.
      CHECK WA_NFE_ALV-REFNUM IS NOT INITIAL.
      PERFORM NF_WRITER USING WA_NFE_ALV-REFNUM.

*   Display application-log entries for selected NF-e
    WHEN 'ERRLOG'.
      CLEAR GS_LOG_FILTER.
      GS_EXTNUM-SIGN   = 'I'.
      GS_EXTNUM-OPTION = 'EQ'.
      GS_EXTNUM-LOW = GS_EXTNUM-HIGH = WA_NFE_ALV-DOCNUM.
      APPEND GS_EXTNUM TO GS_LOG_FILTER-EXTNUMBER.

      PERFORM DISPLAY_ERROR_LOG.   "transaction SLG1

*   Display application-log entries for selected NF-e
    WHEN 'REGIO'.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.


  ENDCASE.

ENDFORM.                    " handle_hotspot_click
