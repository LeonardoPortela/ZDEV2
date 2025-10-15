FUNCTION ZBAPI_GOODSMVT_GETDETAIL.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(MATERIALDOCUMENT) TYPE  BAPI2017_GM_HEAD_02-MAT_DOC
*"     VALUE(MATDOCUMENTYEAR) TYPE  BAPI2017_GM_HEAD_02-DOC_YEAR
*"  EXPORTING
*"     VALUE(GOODSMVT_HEADER) LIKE  BAPI2017_GM_HEAD_02
*"  STRUCTURE  BAPI2017_GM_HEAD_02
*"  TABLES
*"      GOODSMVT_ITEMS STRUCTURE  BAPI2017_GM_ITEM_SHOW
*"      RETURN STRUCTURE  BAPIRET2
*"--------------------------------------------------------------------
*ENHANCEMENT-POINT BAPI_GOODSMVT_GETDETAIL_G8 SPOTS ES_SAPLMB_BUS2017 STATIC.

*ENHANCEMENT-POINT BAPI_GOODSMVT_GETDETAIL_G6 SPOTS ES_SAPLMB_BUS2017.

DATA: I_MKPF LIKE MKPF.

DATA: BEGIN OF I_MSEG OCCURS 0.
        INCLUDE STRUCTURE MSEG.
DATA: END OF I_MSEG.

  CLEAR RETURN. REFRESH RETURN.

  CALL FUNCTION 'MAP2I_B2017_GM_HEAD_02_TO_MKPF'
       EXPORTING
            BAPI2017_GM_HEAD_02 = GOODSMVT_HEADER
       CHANGING
            MKPF                = I_MKPF.
  CALL FUNCTION 'MB_READ_MATERIAL_HEADER'
       EXPORTING
            MBLNR   = MATERIALDOCUMENT
            MJAHR   = MATDOCUMENTYEAR
            TRTYP   = 'A'
            VGART   = ' '
       IMPORTING
            KOPF    = I_MKPF
       EXCEPTIONS
            ERROR_MESSAGE.
  IF NOT SY-SUBRC IS INITIAL.
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
         EXPORTING
              TYPE       = SY-MSGTY
              CL         = SY-MSGID
              NUMBER     = SY-MSGNO
              PAR1       = SY-MSGV1
              PAR2       = SY-MSGV2
              PAR3       = SY-MSGV3
              PAR4       = SY-MSGV4
*             LOG_NO     = ' '
*             LOG_MSG_NO = ' '
              PARAMETER  = RET_PARAMETER
              ROW        = RET_ROW
              FIELD      = RET_FIELD
         IMPORTING
              RETURN     = RETURN.
    APPEND RETURN.
  ENDIF.
  CALL FUNCTION 'MAP2E_MKPF_TO_B2017_GM_HEAD_02'
       EXPORTING
            MKPF                = I_MKPF
       CHANGING
            BAPI2017_GM_HEAD_02 = GOODSMVT_HEADER.

  CALL FUNCTION 'MB_READ_MATERIAL_POSITION'
       EXPORTING
            MBLNR   = MATERIALDOCUMENT
            MJAHR   = MATDOCUMENTYEAR
            TRTYP   = 'A'
*           ZEILB   =
*           ZEILE   =
       TABLES
            SEQTAB  = I_MSEG
       EXCEPTIONS
            ERROR_MESSAGE.
  IF NOT SY-SUBRC IS INITIAL.
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
         EXPORTING
              TYPE       = SY-MSGTY
              CL         = SY-MSGID
              NUMBER     = SY-MSGNO
              PAR1       = SY-MSGV1
              PAR2       = SY-MSGV2
              PAR3       = SY-MSGV3
              PAR4       = SY-MSGV4
*             LOG_NO     = ' '
*             LOG_MSG_NO = ' '
              PARAMETER  = RET_PARAMETER
              ROW        = RET_ROW
              FIELD      = RET_FIELD
         IMPORTING
              RETURN     = RETURN.
    APPEND RETURN.
  ENDIF.
  LOOP AT I_MSEG.
    AUTHORITY-CHECK OBJECT 'M_MSEG_WMB'
             ID 'ACTVT' FIELD '03'
             ID 'WERKS' FIELD I_MSEG-WERKS.
    IF NOT SY-SUBRC IS INITIAL.
      MOVE I_MSEG-WERKS TO SY-MSGV1.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
           EXPORTING
                TYPE       = 'E'
                CL         = 'M7'
                NUMBER     = '120'
                PAR1       = SY-MSGV1
*               par2       = sy-msgv2
*               par3       = sy-msgv3
*               par4       = sy-msgv4
*               LOG_NO     = ' '
*               LOG_MSG_NO = ' '
                PARAMETER  = RET_PARAMETER
                ROW        = RET_ROW
                FIELD      = RET_FIELD
           IMPORTING
                RETURN     = RETURN.
      APPEND RETURN.
    ENDIF.
    AUTHORITY-CHECK OBJECT 'M_MSEG_BMB'
             ID 'ACTVT' FIELD '03'
             ID 'BWART' FIELD I_MSEG-BWART.
    IF NOT SY-SUBRC IS INITIAL.
      MOVE I_MSEG-BWART TO SY-MSGV1.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
           EXPORTING
                TYPE       = 'E'
                CL         = 'M7'
                NUMBER     = '121'
                PAR1       = SY-MSGV1
*               par2       = sy-msgv2
*               par3       = sy-msgv3
*               par4       = sy-msgv4
*               LOG_NO     = ' '
*               LOG_MSG_NO = ' '
                PARAMETER  = RET_PARAMETER
                ROW        = RET_ROW
                FIELD      = RET_FIELD
           IMPORTING
                RETURN     = RETURN.
      APPEND RETURN.
    ENDIF.
    CALL FUNCTION 'MP2E_MSEG_TO_2017_GM_ITEM_SHOW' "#EC CI_USAGE_OK[2438006]
         EXPORTING
              MSEG                         = I_MSEG
         CHANGING
              BAPI2017_GM_ITEM_SHOW        = GOODSMVT_ITEMS
         EXCEPTIONS
              ERROR_CONVERTING_CURR_AMOUNT = 1
              OTHERS                       = 2.
    APPEND GOODSMVT_ITEMS.

*ENHANCEMENT-POINT BAPI_GOODSMVT_GETDETAIL_01 SPOTS ES_SAPLMB_BUS2017.

  ENDLOOP.

*ENHANCEMENT-POINT BAPI_GOODSMVT_GETDETAIL_G7 SPOTS ES_SAPLMB_BUS2017.
ENDFUNCTION.
