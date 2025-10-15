FUNCTION ZALE_GOODSMVT_CANCEL.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(MATERIALDOCUMENT) TYPE  BAPI2017_GM_HEAD_02-MAT_DOC
*"     VALUE(MATDOCUMENTYEAR) TYPE  BAPI2017_GM_HEAD_02-DOC_YEAR
*"     VALUE(GOODSMVTPSTNGDATE) TYPE  BAPI2017_GM_HEAD_02-PSTNG_DATE
*"         OPTIONAL
*"     VALUE(GOODSMVTPRUNAME) TYPE  BAPI2017_GM_HEAD_01-PR_UNAME
*"         OPTIONAL
*"     VALUE(OBJ_TYPE) LIKE  SERIAL-OBJ_TYPE DEFAULT 'BUS2017'
*"     VALUE(SERIAL_ID) LIKE  SERIAL-CHNUM DEFAULT '0'
*"  TABLES
*"      GOODSMVTMATDOCITEM STRUCTURE  BAPI2017_GM_ITEM_04 OPTIONAL
*"      RECEIVERS STRUCTURE  BDI_LOGSYS
*"      COMMUNICATION_DOCUMENTS STRUCTURE  SWOTOBJID OPTIONAL
*"      APPLICATION_OBJECTS STRUCTURE  SWOTOBJID OPTIONAL
*"  EXCEPTIONS
*"      ERROR_CREATING_IDOCS
*"--------------------------------------------------------------------
*----------------------------------------------------------------------*
*  this function module is generated                                   *
*          never change it manually, please!        06.11.2001         *
*----------------------------------------------------------------------*

  DATA: IDOC_CONTROL  LIKE BDICONTROL,
        IDOC_DATA     LIKE EDIDD      OCCURS 0 WITH HEADER LINE,
        IDOC_RECEIVER LIKE BDI_LOGSYS OCCURS 0 WITH HEADER LINE,
        IDOC_COMM     LIKE EDIDC      OCCURS 0 WITH HEADER LINE,
        SYST_INFO     LIKE SYST.


* create IDoc control-record                                           *
  IDOC_CONTROL-MESTYP = 'MBGMCA'.
  IDOC_CONTROL-IDOCTP = 'MBGMCA01'.
  IDOC_CONTROL-SERIAL = SY-DATUM.
  IDOC_CONTROL-SERIAL+8 = SY-UZEIT.

  IDOC_RECEIVER[] = RECEIVERS[].

*   call subroutine to create IDoc data-record                         *
  clear: syst_info, IDOC_DATA.
  REFRESH IDOC_DATA.
  PERFORM IDOC_GOODSMVT_CANCEL
          TABLES
              GOODSMVTMATDOCITEM
              IDOC_DATA
          USING
              MATERIALDOCUMENT
              MATDOCUMENTYEAR
              GOODSMVTPSTNGDATE
              GOODSMVTPRUNAME
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
form IDOC_GOODSMVT_CANCEL
     tables
         GOODSMVTMATDOCITEM structure
           BAPI2017_GM_ITEM_04
         idoc_data structure edidd
     using
         MATERIALDOCUMENT like
           BAPI2017_GM_HEAD_02-MAT_DOC
         MATDOCUMENTYEAR like
           BAPI2017_GM_HEAD_02-DOC_YEAR
         GOODSMVTPSTNGDATE like
           BAPI2017_GM_HEAD_02-PSTNG_DATE
         GOODSMVTPRUNAME like
           BAPI2017_GM_HEAD_01-PR_UNAME
         syst_info like syst
         .

  data:  E1MBGMCA like E1MBGMCA.
  data:  E1BP2017_GM_ITEM_04 like E1BP2017_GM_ITEM_04.

* go through all IDoc-segments                                         *

* for segment 'E1MBGMCA'                                               *
  clear: E1MBGMCA,
         idoc_data.
  move MATERIALDOCUMENT
    to E1MBGMCA-MATERIALDOCUMENT.
  move MATDOCUMENTYEAR
    to E1MBGMCA-MATDOCUMENTYEAR.
  move GOODSMVTPSTNGDATE
    to E1MBGMCA-GOODSMVT_PSTNG_DATE.
  move GOODSMVTPRUNAME
    to E1MBGMCA-GOODSMVT_PR_UNAME.
  idoc_data-sdata  = E1MBGMCA.
  idoc_data-segnam = 'E1MBGMCA'.
  append idoc_data.


*   for segment 'E1BP2017_GM_ITEM_04'                                  *
  loop at GOODSMVTMATDOCITEM
               .
    clear: E1BP2017_GM_ITEM_04,
           idoc_data.
    move-corresponding GOODSMVTMATDOCITEM
                    to E1BP2017_GM_ITEM_04.
    idoc_data-sdata = E1BP2017_GM_ITEM_04.
    idoc_data-segnam = 'E1BP2017_GM_ITEM_04'.
    append idoc_data.
  endloop.


* end of through all IDoc-segments                                     *

endform.                               " IDOC_GOODSMVT_CANCEL
