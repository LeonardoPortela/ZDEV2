FUNCTION ZBAPI_GOODSMVT_CANCEL.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(MATERIALDOCUMENT) TYPE  BAPI2017_GM_HEAD_02-MAT_DOC
*"     VALUE(MATDOCUMENTYEAR) TYPE  BAPI2017_GM_HEAD_02-DOC_YEAR
*"     VALUE(GOODSMVT_PSTNG_DATE) TYPE  BAPI2017_GM_HEAD_02-PSTNG_DATE
*"         OPTIONAL
*"     VALUE(GOODSMVT_PR_UNAME) TYPE  BAPI2017_GM_HEAD_01-PR_UNAME
*"         OPTIONAL
*"  EXPORTING
*"     VALUE(GOODSMVT_HEADRET) LIKE  BAPI2017_GM_HEAD_RET
*"  STRUCTURE  BAPI2017_GM_HEAD_RET
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2
*"      GOODSMVT_MATDOCITEM STRUCTURE  BAPI2017_GM_ITEM_04 OPTIONAL
*"--------------------------------------------------------------------
* Local Data Definition ***********************************************
DATA: BUDAT LIKE MKPF-BUDAT,
      MBLNR LIKE MKPF-MBLNR,
      MJAHR LIKE MKPF-MJAHR,
      L_BUFFER_EXPORT TYPE C VALUE FALSE.                       "321507

*--> Brazil - Trigger NF geneartion via special code            "980112
data: j1b_action type goaction value 'A03'.                     "980112
export lv_action from j1b_action to memory id 'J1B_ACTION'.     "980112

DO 1 TIMES.                                                     "321507
* Reset Data Definition ***********************************************
  CLEAR: RETURN,
         GLOBAL_ERROR,
         LOC_TAB,
         T_IMSEG,
         T_IMSEG_CANCEL.

  REFRESH: RETURN,
           T_IMSEG,
           T_IMSEG_CANCEL,
           T_EMSEG.

* Mapping of Import Parameters ****************************************
  MBLNR = MATERIALDOCUMENT.
  MJAHR = MATDOCUMENTYEAR.
  BUDAT = GOODSMVT_PSTNG_DATE.

  IF BUDAT IS INITIAL.
    BUDAT = SY-DATUM.
  ENDIF.

  LOOP AT GOODSMVT_MATDOCITEM.
    CLEAR T_IMSEG_CANCEL.
    CALL FUNCTION 'MAP2I_B2017_GM_ITEM_04_TO_CANC'
         EXPORTING
              BAPI2017_GM_ITEM_04 = GOODSMVT_MATDOCITEM
         CHANGING
              IMSEG_CANCEL        = T_IMSEG_CANCEL.
    APPEND T_IMSEG_CANCEL.
  ENDLOOP.

* Initialization of serialnumber tables *******************************
  CALL FUNCTION 'SERIALPROFILE_CHECK'
       EXPORTING
            OPERATION            = 'SNCL'
       EXCEPTIONS ERROR_MESSAGE.
  IF SY-SUBRC <> 0.
    GLOBAL_ERROR = TRUE.
    PERFORM SY_MSG_TO_BAPIRET2 TABLES RETURN
                               USING  0
                                      'MATERIALDOCUMENT'.
  ENDIF.

  CHECK GLOBAL_ERROR = FALSE.

* Set flag XBAPI FOR MB_CANCEL_GOODS_MOVEMENT *************************
  CALL FUNCTION 'MB_SET_BAPI_FLAG'
       EXPORTING ACTION = '1'.

* Move STATUS (used by serialno., prod.order,...) to memory ***********
  CALL FUNCTION 'STATUS_BUFFER_EXPORT_TO_MEMORY'                "321507
       EXPORTING                                                "321507
            i_memory_id = 'BAPI_GOODSMVT_CANCEL'.               "321507
  L_BUFFER_EXPORT = TRUE.                                       "321507

* Call function for to cancel the material document *******************
  PERFORM MB_CANCEL_GOODS_MOVEMENT USING BUDAT
                                         MBLNR
                                         MJAHR.

* Check the result ****************************************************
  PERFORM RETURN_HANDLING TABLES RETURN.

* Posting only possible, if there was no error until now **************
  CHECK GLOBAL_ERROR = FALSE.

* Call function MB_POST_GOODS_MOVEMENT ********************************
  PERFORM MB_POST_GOODS_MOVEMENT TABLES RETURN
                                 USING  GOODSMVT_HEADRET.
ENDDO.                                                           "321507

***********************************************************************
* Error Handling:                                                "321507
* Get STATUS back from memory if error                           "321507
* But only if status was filled during this BAPI.                "321507
CHECK L_BUFFER_EXPORT = TRUE.                                    "321507
IF GLOBAL_ERROR = TRUE.                                          "321507
  CALL FUNCTION 'STATUS_BUFFER_REFRESH'.                         "321507
  CALL FUNCTION 'STATUS_BUFFER_IMPORT_FROM_MEMO'                 "321507
       EXPORTING                                                 "321507
            i_memory_id = 'BAPI_GOODSMVT_CANCEL'                 "321507
            i_client    = sy-mandt.                              "321507
ENDIF.                                                          "321507

* Clear flag XBAPI *****************************************************
CALL FUNCTION 'MB_SET_BAPI_FLAG'                                 "386958
     EXPORTING ACTION = '4'.                                     "386958

ENDFUNCTION.
