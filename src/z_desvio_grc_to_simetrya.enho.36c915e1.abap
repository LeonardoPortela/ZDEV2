"Name: \FU:J_1BNFE_CUST3_READ\SE:END\EI
ENHANCEMENT 0 Z_DESVIO_GRC_TO_SIMETRYA.

*  FIELD-SYMBOLS: <FS_CAB> TYPE J_1BNFDOC.
*
*  DATA: WA_URL TYPE ZIB_NFE.
*
*  CLEAR: WA_URL.
*
*  ASSIGN ('(SAPLJ_1B_NFE)WK_HEADER') TO <FS_CAB>.
*
*  CASE iv_model.
*     WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_NFE.
*      select SINGLE *
*        FROM setleaf INTO @data(wl_setleaf_model_xi)
*       WHERE setname EQ 'GRC_NFE_CALL_XI_BRANCH'
*         AND valfrom EQ @IV_BRANCH.
*
*      IF ( SY-SUBRC IS INITIAL ) AND ( <FS_CAB> IS ASSIGNED ).
*
*        SELECT SINGLE * INTO WA_URL
*          FROM ZIB_NFE
*         WHERE DOCNUM       EQ <FS_CAB>-DOCNUM
*           AND DS_URL_DANFE NE SPACE.
*
*        IF ( SY-SUBRC EQ 0 ) AND ( WA_URL-DS_URL_DANFE CS 'SIMETRYA' OR WA_URL-DS_URL_DANFE CS '172.12.12.139' ).
*          SY-SUBRC = 8.
*        ELSE.
*          SY-SUBRC = 0.
*        ENDIF.
*
*      ENDIF.
*
*     WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_CTE.
*
*      select SINGLE *
*        FROM setleaf INTO @wl_setleaf_model_xi
*       WHERE setname EQ 'GRC_CTE_CALL_XI_BRANCH'
*         AND valfrom EQ @IV_BRANCH.
*
*      IF ( SY-SUBRC IS INITIAL ) AND ( <FS_CAB> IS ASSIGNED ).
*        SELECT SINGLE * INTO WA_URL
*          FROM ZIB_NFE
*         WHERE DOCNUM       EQ <FS_CAB>-DOCNUM
*           AND DS_URL_DANFE NE SPACE.
*
*        IF ( SY-SUBRC EQ 0 ) AND ( WA_URL-DS_URL_DANFE CS 'SIMETRYA' OR WA_URL-DS_URL_DANFE CS '172.12.12.139' ).
*          SY-SUBRC = 8.
*        ELSE.
*          SY-SUBRC = 0.
*        ENDIF.
*      ENDIF.
*
*     WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_MDFE.
*
*      SELECT SINGLE *
*        FROM SETLEAF INTO @WL_SETLEAF_MODEL_XI
*       WHERE SETNAME EQ 'GRC_MDFE_CALL_XI_BRANCH'
*         AND VALFROM EQ @IV_BRANCH.
*
*      IF ( SY-SUBRC IS INITIAL ) AND ( <FS_CAB> IS ASSIGNED ).
*        SELECT SINGLE * INTO WA_URL
*          FROM ZIB_NFE
*         WHERE DOCNUM EQ <FS_CAB>-DOCNUM
*           AND DS_URL_DANFE NE SPACE.
*
*        IF ( SY-SUBRC EQ 0 ) AND ( WA_URL-DS_URL_DANFE CS 'SIMETRYA' OR WA_URL-DS_URL_DANFE CS '172.12.12.139' ).
*          SY-SUBRC = 8.
*        ELSE.
*          SY-SUBRC = 0.
*        ENDIF.
*      ENDIF.
*
* ENDCASE.
*
* IF SY-SUBRC IS NOT INITIAL.
*
*   es_cust3-version    = 1.
*   es_cust3-xnfeactive = abap_false.
*   es_cust3-autoserver = abap_false.
*
*   CLEAR: es_cust3-event_group.
*
*   ASSIGN ('(SAPLJ_1B_NFE)WK_HEADER') TO <fs_cab>.
*   IF <fs_cab> is ASSIGNED.
*     <fs_cab>-xmlvers = es_cust3-version.
*   ENDIF.
* ENDIF.

ENDENHANCEMENT.
