*----------------------------------------------------------------------*
*   INCLUDE YHBRCVT06         " Batch input
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       Form  BATCH_INPUT_TABLE
*----------------------------------------------------------------------*
FORM BATCH_INPUT_TABLE.

  DATA: W_PERNR(8)  TYPE C,
        W_BETRG     LIKE BDCDATA-FVAL,
        W_DATE      LIKE BDCDATA-FVAL.

  PERFORM BATCH_INPUT_DYNPRO USING 'SAPMP50A'    '1000'.
  W_PERNR = INF0015-PERNR.
  PERFORM BATCH_INPUT_FIELD  USING 'RP50G-PERNR' W_PERNR.
  PERFORM BATCH_INPUT_FIELD  USING 'RP50G-CHOIC' '0015'.
  PERFORM BATCH_INPUT_FIELD  USING 'RP50G-SUBTY' '9015'.
  PERFORM BATCH_INPUT_FIELD  USING 'BDC_OKCODE'  '=INS'.

  PERFORM BATCH_INPUT_DYNPRO USING 'MP001500'    '2000'.
  WRITE INF0015-BETRG TO W_BETRG(13) CURRENCY INF0015-WAERS.
  PERFORM BATCH_INPUT_FIELD  USING 'Q0015-BETRG' W_BETRG.
  WRITE INF0015-BEGDA TO W_DATE DD/MM/YYYY.
  PERFORM BATCH_INPUT_FIELD  USING 'P0015-BEGDA' W_DATE.
  PERFORM BATCH_INPUT_FIELD  USING 'BDC_OKCODE'  'UPD'.

ENDFORM.             " BATCH_INPUT_TABLE

*----------------------------------------------------------------------*
*       Form  BATCH_INPUT_DYNPRO
*----------------------------------------------------------------------*
FORM BATCH_INPUT_DYNPRO USING PROGR LIKE BDCDATA-PROGRAM
                              DYNPR LIKE BDCDATA-DYNPRO.

   CLEAR BDCDATA.
   BDCDATA-PROGRAM  = PROGR.
   BDCDATA-DYNPRO   = DYNPR.
   BDCDATA-DYNBEGIN = 'X'.
   APPEND BDCDATA.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  BATCH_INPUT_FIELD
*----------------------------------------------------------------------*
FORM BATCH_INPUT_FIELD USING FLD      LIKE BDCDATA-FNAM
                             VAL      TYPE C.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FLD.
  BDCDATA-FVAL = VAL.
  APPEND BDCDATA.

ENDFORM.

*----------------------------------------------------------------------*
*       Form  OPEN_BATCH_INPUT
*----------------------------------------------------------------------*
FORM OPEN_BATCH_INPUT.

  DATA: MAPNAME LIKE APQI-GROUPID.

  MAPNAME   = BATNAME.

  CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
           CLIENT              = SY-MANDT
           GROUP               = MAPNAME
           USER                = SY-UNAME
      EXCEPTIONS
           CLIENT_INVALID      = 1
           DESTINATION_INVALID = 2
           GROUP_INVALID       = 3
           GROUP_IS_LOCKED     = 4
           HOLDDATE_INVALID    = 5
           INTERNAL_ERROR      = 6
           QUEUE_ERROR         = 7
           RUNNING             = 8
           SYSTEM_LOCK_ERROR   = 9
           USER_INVALID        = 10
           OTHERS              = 11.

   IF SY-SUBRC <> 0.
          PERFORM APPEND_ERROR USING SPACE
                                     'E'
                                     '002'
                                     SY-SUBRC
                                     SPACE
                                     SPACE
                                     SPACE.
   ENDIF.

ENDFORM.                    " OPEN_BATCH_INPUT

*----------------------------------------------------------------------*
*       FORM INSERT_BATCH_INPUT
*----------------------------------------------------------------------*
FORM INSERT_BATCH_INPUT.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
         TCODE            = 'PA30'
         POST_LOCAL       = 'E'
     TABLES
          DYNPROTAB        = BDCDATA
     EXCEPTIONS
          INTERNAL_ERROR   = 1
          NOT_OPEN         = 2
          QUEUE_ERROR      = 3
          TCODE_INVALID    = 4
          PRINTING_INVALID = 5
          POSTING_INVALID  = 6
          OTHERS           = 7.

  IF SY-SUBRC <> 0.
          PERFORM APPEND_ERROR USING SPACE
                                     'E'
                                     '004'
                                     SY-SUBRC
                                     SPACE
                                     SPACE
                                     SPACE.
  ENDIF.

ENDFORM.                         " INSERT_BATCH_INPUT

*----------------------------------------------------------------------*
*       Form  CLOSE_BATCH_INPUT
*----------------------------------------------------------------------*
FORM CLOSE_BATCH_INPUT.

  CALL FUNCTION 'BDC_CLOSE_GROUP'
     EXCEPTIONS
          NOT_OPEN    = 1
          QUEUE_ERROR = 2
          OTHERS      = 3.

   IF SY-SUBRC <> 0.
          PERFORM APPEND_ERROR USING SPACE
                                     'E'
                                     '003'
                                     SY-SUBRC
                                     SPACE
                                     SPACE
                                     SPACE.
   ENDIF.

ENDFORM.                        " CLOSE_BATCH_INPUT
