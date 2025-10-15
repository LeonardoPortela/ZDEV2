FUNCTION ZALE_GOODSMVT_CREATE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(GOODSMVTHEADER) LIKE  BAPI2017_GM_HEAD_01
*"  STRUCTURE  BAPI2017_GM_HEAD_01
*"     VALUE(GOODSMVTCODE) LIKE  BAPI2017_GM_CODE
*"  STRUCTURE  BAPI2017_GM_CODE
*"     VALUE(TESTRUN) LIKE  BAPI2017_GM_GEN-TESTRUN DEFAULT SPACE
*"     VALUE(GOODSMVTREFEWM) LIKE  /SPE/BAPI2017_GM_REF_EWM
*"  STRUCTURE  /SPE/BAPI2017_GM_REF_EWM OPTIONAL
*"     VALUE(OBJ_TYPE) LIKE  SERIAL-OBJ_TYPE DEFAULT 'BUS2017'
*"     VALUE(SERIAL_ID) LIKE  SERIAL-CHNUM DEFAULT '0'
*"  TABLES
*"      GOODSMVTITEM STRUCTURE  BAPI2017_GM_ITEM_CREATE
*"      GOODSMVTSERIALNUMBER STRUCTURE  BAPI2017_GM_SERIALNUMBER
*"         OPTIONAL
*"      GOODSMVTSERVPARTDATA STRUCTURE  /SPE/BAPI2017_SERVICEPART_DATA
*"         OPTIONAL
*"      EXTENSIONIN STRUCTURE  BAPIPAREX OPTIONAL
*"      RECEIVERS STRUCTURE  BDI_LOGSYS
*"      COMMUNICATION_DOCUMENTS STRUCTURE  SWOTOBJID OPTIONAL
*"      APPLICATION_OBJECTS STRUCTURE  SWOTOBJID OPTIONAL
*"  EXCEPTIONS
*"      ERROR_CREATING_IDOCS
*"--------------------------------------------------------------------
*----------------------------------------------------------------------*
*  this function module is generated                                   *
*          never change it manually, please!        19.03.2015         *
*----------------------------------------------------------------------*

  DATA: IDOC_CONTROL  LIKE BDICONTROL,
        IDOC_DATA     LIKE EDIDD      OCCURS 0 WITH HEADER LINE,
        IDOC_RECEIVER LIKE BDI_LOGSYS OCCURS 0 WITH HEADER LINE,
        IDOC_COMM     LIKE EDIDC      OCCURS 0 WITH HEADER LINE,
        SYST_INFO     LIKE SYST.


* create IDoc control-record                                           *
  IDOC_CONTROL-MESTYP = 'MBGMCR'.
  IDOC_CONTROL-IDOCTP = 'MBGMCR03'.
  IDOC_CONTROL-SERIAL = SY-DATUM.
  IDOC_CONTROL-SERIAL+8 = SY-UZEIT.

  IDOC_RECEIVER[] = RECEIVERS[].

*   call subroutine to create IDoc data-record                         *
    clear: syst_info, IDOC_DATA.
    REFRESH IDOC_DATA.
    PERFORM IDOC_GOODSMVT_CREATE
            TABLES
                GOODSMVTITEM
                GOODSMVTSERIALNUMBER
                GOODSMVTSERVPARTDATA
                EXTENSIONIN
                IDOC_DATA
            USING
                GOODSMVTHEADER
                GOODSMVTCODE
                TESTRUN
                GOODSMVTREFEWM
                SYST_INFO
                .
    IF NOT SYST_INFO IS INITIAL.
      MESSAGE ID SYST_INFO-MSGID
            TYPE SYST_INFO-MSGTY
          NUMBER SYST_INFO-MSGNO
            WITH SYST_INFO-MSGV1 SYST_INFO-MSGV2
                 SYST_INFO-MSGV3 SYST_INFO-MSGV4
      RAISING ERROR_CREATING_IDOCS.
    ENDIF.

*   distribute idocs                                                   *
    CALL FUNCTION 'ALE_IDOCS_CREATE'
         EXPORTING
              IDOC_CONTROL                = IDOC_CONTROL
              OBJ_TYPE                    = OBJ_TYPE
              CHNUM                       = SERIAL_ID
         TABLES
              IDOC_DATA                   = IDOC_DATA
              RECEIVERS                   = IDOC_RECEIVER
*             CREATED_IDOCS               =                            *
              CREATED_IDOCS_ADDITIONAL    = IDOC_COMM
              APPLICATION_OBJECTS         = APPLICATION_OBJECTS
         EXCEPTIONS
              IDOC_INPUT_WAS_INCONSISTENT = 1
              OTHERS                      = 2
              .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
      RAISING ERROR_CREATING_IDOCS.
    ENDIF.

    IF COMMUNICATION_DOCUMENTS IS REQUESTED.
      LOOP AT IDOC_COMM.
        CLEAR COMMUNICATION_DOCUMENTS.
        COMMUNICATION_DOCUMENTS-OBJTYPE  = 'IDOC'.
        COMMUNICATION_DOCUMENTS-OBJKEY   = IDOC_COMM-DOCNUM.
        COMMUNICATION_DOCUMENTS-LOGSYS   = IDOC_COMM-RCVPRN.
        COMMUNICATION_DOCUMENTS-DESCRIBE = SPACE.
        APPEND COMMUNICATION_DOCUMENTS.
      ENDLOOP.
    ENDIF.

* applications do commit work to trigger communications                *





ENDFUNCTION.


* subroutine creating IDoc data-record                                 *
form IDOC_GOODSMVT_CREATE
     tables
         GOODSMVTITEM structure
           BAPI2017_GM_ITEM_CREATE
         GOODSMVTSERIALNUMBER structure
           BAPI2017_GM_SERIALNUMBER
         GOODSMVTSERVPARTDATA structure
           /SPE/BAPI2017_SERVICEPART_DATA
         EXTENSIONIN structure
           BAPIPAREX
         idoc_data structure edidd
     using
         GOODSMVTHEADER like
           BAPI2017_GM_HEAD_01
         GOODSMVTCODE like
           BAPI2017_GM_CODE
         TESTRUN like
           BAPI2017_GM_GEN-TESTRUN
         GOODSMVTREFEWM like
           /SPE/BAPI2017_GM_REF_EWM
         syst_info like syst
         ."#EC *

  data:  E1MBGMCR like E1MBGMCR.
  data:  E1BP2017_GM_HEAD_01 like E1BP2017_GM_HEAD_01.
  data:  E1BP2017_GM_CODE like E1BP2017_GM_CODE.
  data:  /SPE/E1BP2017_GM_REF_EWM like /SPE/E1BP2017_GM_REF_EWM.
  data:  E1BP2017_GM_ITEM_CREATE like E1BP2017_GM_ITEM_CREATE.
  data:  E1BP2017_GM_ITEM_CREATE1 like E1BP2017_GM_ITEM_CREATE1.
  data:  E1BP2017_GM_SERIALNUMBER like E1BP2017_GM_SERIALNUMBER.
  data:  /SPE/E1BP2017_SERVICEPART_D like /SPE/E1BP2017_SERVICEPART_D.
  data:  E1BPPAREX like E1BPPAREX.

* go through all IDoc-segments                                         *

* for segment 'E1MBGMCR'                                               *
    clear: E1MBGMCR,
           idoc_data.
    move TESTRUN
      to E1MBGMCR-TESTRUN.
    idoc_data-sdata  = E1MBGMCR.
    idoc_data-segnam = 'E1MBGMCR'.
    append idoc_data.


*   for segment 'E1BP2017_GM_HEAD_01'                                  *
    clear: E1BP2017_GM_HEAD_01,
           idoc_data.
    move-corresponding GOODSMVTHEADER
        to E1BP2017_GM_HEAD_01."#EC ENHOK
    if not E1BP2017_GM_HEAD_01 is initial.
    idoc_data-sdata = E1BP2017_GM_HEAD_01.
    idoc_data-segnam = 'E1BP2017_GM_HEAD_01'.
    append idoc_data.
    endif.

*   for segment 'E1BP2017_GM_CODE'                                     *
    clear: E1BP2017_GM_CODE,
           idoc_data.
    move-corresponding GOODSMVTCODE
        to E1BP2017_GM_CODE."#EC ENHOK
    if not E1BP2017_GM_CODE is initial.
    idoc_data-sdata = E1BP2017_GM_CODE.
    idoc_data-segnam = 'E1BP2017_GM_CODE'.
    append idoc_data.
    endif.

*   for segment '/SPE/E1BP2017_GM_REF_EWM'                             *
    clear: /SPE/E1BP2017_GM_REF_EWM,
           idoc_data.
    move-corresponding GOODSMVTREFEWM
        to /SPE/E1BP2017_GM_REF_EWM."#EC ENHOK
    if not /SPE/E1BP2017_GM_REF_EWM is initial.
    idoc_data-sdata = /SPE/E1BP2017_GM_REF_EWM.
    idoc_data-segnam = '/SPE/E1BP2017_GM_REF_EWM'.
    append idoc_data.
    endif.

*   for segment 'E1BP2017_GM_ITEM_CREATE'                              *
  loop at GOODSMVTITEM
               .
    clear: E1BP2017_GM_ITEM_CREATE,
           idoc_data.
    move-corresponding GOODSMVTITEM
        to E1BP2017_GM_ITEM_CREATE."#EC ENHOK
condense E1BP2017_GM_ITEM_CREATE-ENTRY_QNT.
condense E1BP2017_GM_ITEM_CREATE-PO_PR_QNT.
condense E1BP2017_GM_ITEM_CREATE-AMOUNT_LC.
condense E1BP2017_GM_ITEM_CREATE-AMOUNT_SV.
condense E1BP2017_GM_ITEM_CREATE-SU_PL_STCK_1.
condense E1BP2017_GM_ITEM_CREATE-ST_UN_QTYY_1.
condense E1BP2017_GM_ITEM_CREATE-SU_PL_STCK_2.
condense E1BP2017_GM_ITEM_CREATE-ST_UN_QTYY_2.
    idoc_data-sdata = E1BP2017_GM_ITEM_CREATE.
    idoc_data-segnam = 'E1BP2017_GM_ITEM_CREATE'.
    append idoc_data.

*     for segment 'E1BP2017_GM_ITEM_CREATE1'                           *
    clear: E1BP2017_GM_ITEM_CREATE1,
           idoc_data.
    move-corresponding GOODSMVTITEM
        to E1BP2017_GM_ITEM_CREATE1."#EC ENHOK
condense E1BP2017_GM_ITEM_CREATE1-QUANTITY.
    idoc_data-sdata = E1BP2017_GM_ITEM_CREATE1.
    idoc_data-segnam = 'E1BP2017_GM_ITEM_CREATE1'.
    append idoc_data.
  endloop.


*   for segment 'E1BP2017_GM_SERIALNUMBER'                             *
  loop at GOODSMVTSERIALNUMBER
               .
    clear: E1BP2017_GM_SERIALNUMBER,
           idoc_data.
    move-corresponding GOODSMVTSERIALNUMBER
        to E1BP2017_GM_SERIALNUMBER."#EC ENHOK
    idoc_data-sdata = E1BP2017_GM_SERIALNUMBER.
    idoc_data-segnam = 'E1BP2017_GM_SERIALNUMBER'.
    append idoc_data.
  endloop.


*   for segment '/SPE/E1BP2017_SERVICEPART_D'                          *
  loop at GOODSMVTSERVPARTDATA
               .
    clear: /SPE/E1BP2017_SERVICEPART_D,
           idoc_data.
    move-corresponding GOODSMVTSERVPARTDATA
        to /SPE/E1BP2017_SERVICEPART_D."#EC ENHOK
condense /SPE/E1BP2017_SERVICEPART_D-TIMESTAMP.
condense /SPE/E1BP2017_SERVICEPART_D-KEEP_QUANTITY.
condense /SPE/E1BP2017_SERVICEPART_D-NUMERATOR.
condense /SPE/E1BP2017_SERVICEPART_D-DENOMINATR.
    idoc_data-sdata = /SPE/E1BP2017_SERVICEPART_D.
    idoc_data-segnam = '/SPE/E1BP2017_SERVICEPART_D'.
    append idoc_data.
  endloop.


*   for segment 'E1BPPAREX'                                            *
  loop at EXTENSIONIN
               .
    clear: E1BPPAREX,
           idoc_data.
    move-corresponding EXTENSIONIN
        to E1BPPAREX."#EC ENHOK
    idoc_data-sdata = E1BPPAREX.
    idoc_data-segnam = 'E1BPPAREX'.
    append idoc_data.
  endloop.


* end of through all IDoc-segments                                     *

endform.                               " IDOC_GOODSMVT_CREATE
