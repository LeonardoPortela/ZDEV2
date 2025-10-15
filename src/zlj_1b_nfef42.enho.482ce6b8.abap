"Name: \PR:SAPLJ_1B_NFE\FO:CALL_XI\SE:BEGIN\EI
ENHANCEMENT 0 ZLJ_1B_NFEF42.

* CASE p_model.
*   WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_NFE.
*    SELECT SINGLE *
*      FROM setleaf INTO @data(wl_setleaf_model_xi)
*     WHERE setname EQ 'GRC_NFE_CALL_XI_BRANCH'
*       AND valfrom EQ @p_branch.
*
*    IF SY-SUBRC IS INITIAL.
*
*       SELECT SINGLE * INTO @DATA(WA_URL)
*         FROM ZIB_NFE
*        WHERE DOCNUM EQ @xmlh-DOCNUM
*          AND DS_URL_DANFE NE @SPACE.
*
*       IF ( WA_URL-DS_URL_DANFE CS 'SIMETRYA' OR WA_URL-DS_URL_DANFE CS '172.12.12.139' ).
*         SY-SUBRC = 8.
*       else.
*         SY-SUBRC = 0.
*       ENDIF.
*    ENDIF.
*
*   WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_CTE.
*    SELECT SINGLE *
*      FROM setleaf INTO @wl_setleaf_model_xi
*     WHERE setname EQ 'GRC_CTE_CALL_XI_BRANCH'
*       AND valfrom EQ @p_branch.
*
*    IF SY-SUBRC IS INITIAL.
*       SELECT SINGLE * INTO @WA_URL
*         FROM ZIB_NFE
*        WHERE DOCNUM EQ @xmlh-DOCNUM
*          AND DS_URL_DANFE NE @SPACE.
*
*       IF ( WA_URL-DS_URL_DANFE CS 'SIMETRYA' OR WA_URL-DS_URL_DANFE CS '172.12.12.139' ) .
*         SY-SUBRC = 8.
*       else.
*         SY-SUBRC = 0.
*       ENDIF.
*    ENDIF.
*
*   WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_MDFE.
*    SELECT SINGLE *
*      FROM SETLEAF INTO @wl_setleaf_model_xi
*     WHERE SETNAME EQ 'GRC_MDFE_CALL_XI_BRANCH'
*       AND VALFROM EQ @p_branch.
* ENDCASE.
*
*  IF SY-SUBRC NE 0.
*
*    IF 1 = 1.
*
*    CALL FUNCTION 'Z_MONTA_XML'
*     EXPORTING
*       XML_IN                      = xmlh
*       XML_HEAD_TAB                = xmlh_tab
*       XML_ITEM_TAB                = xmli_tab
*       XML_BATCH                   = xmlb_tab
*       XML_REF                     = xmlr_tab
*       XML_DUP                     = xmld_tab
*       XML_VOL                     = xmlv_tab
*       XML_IMP                     = xml_import_tab
*       XML_EXT1                    = xml_ext1_tab
*       XML_EXT2                    = xml_ext2_tab
*     EXCEPTIONS
*       COMMUNICATION_FAILURE       = 1
*       SYSTEM_FAILURE              = 2
*       OTHERS                      = 3.
*
*    IF sy-subrc <> 0.
*      p_rfcerror = sy-subrc.
*      exit.
*    ENDIF.
*      exit.
*    ENDIF.
*
*  ENDIF.

  IF p_model EQ ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_MDFE.

    DATA: ZET_BAPIRET2 TYPE ZBAPIRETTAB.
    DATA: ZEV_ERROR_STATUS TYPE ZXNFE_ERRSTATUS.
    DATA: lv_dummy_mdfe TYPE c.
    data: LV_RFCDEST_MDFE TYPE  RFCDEST.

    CALL FUNCTION 'Z_J_1B_MDFE_XML_OUT'
      EXPORTING
        BUKRS                = p_bukrs
        BRANCH               = p_branch
        MODEL                = p_model
        XMLH                 = xmlh
        WK_HEADER            = WK_HEADER
        XMLH_310             = XMLH_310
        resend               = p_resend
     IMPORTING
       ZET_BAPIRET2          = ZET_BAPIRET2
       ZEV_ERROR_STATUS	     = ZEV_ERROR_STATUS
       LV_RFCDEST_MDFE       = LV_RFCDEST_MDFE
     EXCEPTIONS
       COMMUNICATION_FAILURE = 1
       SYSTEM_FAILURE        = 2
       RFC_ERROR             = 3
       OTHERS                = 4.

        p_rfcerror = SY-SUBRC.

        IF NOT ZET_BAPIRET2[] IS INITIAL.
          CALL FUNCTION 'J_1B_NFE_BAPIRET2_MAP_TO_LOG1'
            EXPORTING
              iv_docnum         = XMLH-DOCNUM
              it_bapiret2       = ZET_BAPIRET2.
        ENDIF.                                            "1933985

        IF ZEV_ERROR_STATUS IS NOT INITIAL.                "1933985
           move ZEV_ERROR_STATUS to p_msstat.              "1933985
        ELSE.                                             "1933985
           move c_a to p_msstat.                          "1933985
        ENDIF.                                            "1933985

        IF NOT p_rfcerror IS INITIAL.
          if p_rfcerror <> 2.
            IF p_rfcerror = '9'.
              lv_dummy_mdfe = abap_true.
            ELSE.
              MESSAGE e066 WITH LV_RFCDEST_MDFE INTO lv_dummy_mdfe.
            ENDIF.
          else.
            MESSAGE e554 WITH LV_RFCDEST_MDFE INTO lv_dummy_mdfe.
          endif.
        ENDIF.

    EXIT.

  ENDIF.

ENDENHANCEMENT.
