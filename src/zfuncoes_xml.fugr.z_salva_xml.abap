FUNCTION Z_SALVA_XML.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_XML) TYPE  ZEXML-XML OPTIONAL
*"     REFERENCE(P_DOCNUM) TYPE  J_1BNFDOC-DOCNUM OPTIONAL
*"     REFERENCE(P_TIPO) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_NEUS) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_CNPJ) TYPE  STCD1 OPTIONAL
*"     REFERENCE(P_PROTO) TYPE  ZPROTOCOLO OPTIONAL
*"----------------------------------------------------------------------

  TYPES: BEGIN OF TY_XML.
  TYPES:   XML TYPE STRING.
  TYPES: END OF TY_XML.


  DATA: WA_ZAMB_HOMOLOG TYPE ZAMB_HOMOLOG.


  DATA: IT_XML          TYPE STANDARD TABLE OF TY_XML,
        WA_XML          TYPE TY_XML,
        IP_ADDRESS(69)  TYPE C,
        ARQUIVO         TYPE STRING.


  IF NOT ( P_XML IS INITIAL ).

    CLEAR: ARQUIVO.


    CALL FUNCTION 'ZGET_IP_ADDRESS'
      IMPORTING
        ZIP_ADDRESS = IP_ADDRESS.

    SELECT SINGLE *
      INTO WA_ZAMB_HOMOLOG
      FROM ZAMB_HOMOLOG
     WHERE IP_SERVIDOR = IP_ADDRESS.

    CASE WA_ZAMB_HOMOLOG-AMBIENTE.

      WHEN: 'PRD'.

        IF ( P_NEUS EQ 'X' ).

          IF ( P_TIPO EQ 1 ).
            CONCATENATE '\\sapdev\tmp\neus\request\prd\'  P_CNPJ SY-DATUM '.xml' INTO ARQUIVO.

          ELSE.
            CONCATENATE '\\sapdev\tmp\neus\response\prd\' P_PROTO SY-DATUM '.xml' INTO ARQUIVO.
          ENDIF.

        ELSE.

          IF ( P_TIPO EQ 1 ).
            CONCATENATE '\\sapdev' '\tmp\xmlprd\nfe\' P_DOCNUM '.xml' INTO ARQUIVO.
          ELSE.
            CONCATENATE '\\sapdev' '\tmp\xmlprd\cte\' P_DOCNUM '.xml' INTO ARQUIVO.
          ENDIF.


        ENDIF.

      WHEN: 'QAS'.

        IF ( P_NEUS EQ 'X' ).

          IF ( P_TIPO EQ 1 ).
            CONCATENATE '\\sapdev\tmp\neus\request\qas\' P_CNPJ SY-DATUM '.xml' INTO ARQUIVO.
          ELSE.

            CONCATENATE '\\sapdev\tmp\neus\response\qas\' P_PROTO SY-DATUM '.xml' INTO ARQUIVO.

          ENDIF.
        ELSE.

          IF ( P_TIPO EQ 1 ).
            CONCATENATE '\\sapdev' '\tmp\xmlqas\nfe\' P_DOCNUM '.xml' INTO ARQUIVO.
          ELSE.
            CONCATENATE '\\sapdev' '\tmp\xmlqas\cte\' P_DOCNUM '.xml' INTO ARQUIVO.
          ENDIF.

        ENDIF.

      WHEN: 'DEV'.

        IF ( P_NEUS EQ 'X' ).


          IF ( P_TIPO EQ 1 ).
            CONCATENATE '\\sapdev\tmp\neus\request\dev\' P_CNPJ SY-DATUM '.xml' INTO ARQUIVO.

          ELSE.
            CONCATENATE '\\sapdev\tmp\neus\response\dev\' P_PROTO SY-DATUM '.xml' INTO ARQUIVO.
          ENDIF.

        ELSE.
          IF ( P_TIPO EQ 1 ).
            CONCATENATE '\\sapdev' '\tmp\xmldev\nfe\' P_DOCNUM '.xml' INTO ARQUIVO.
          ELSE.
            CONCATENATE '\\sapdev' '\tmp\xmldev\cte\' P_DOCNUM '.xml' INTO ARQUIVO.
          ENDIF.
        ENDIF.
    ENDCASE.

    IF NOT ( ARQUIVO IS INITIAL ).

      WA_XML-XML = P_XML.

      APPEND WA_XML TO IT_XML.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          FILENAME                = ARQUIVO
        TABLES
          DATA_TAB                = IT_XML
        EXCEPTIONS
          FILE_WRITE_ERROR        = 1
          NO_BATCH                = 2
          GUI_REFUSE_FILETRANSFER = 3
          INVALID_TYPE            = 4
          NO_AUTHORITY            = 5
          UNKNOWN_ERROR           = 6
          HEADER_NOT_ALLOWED      = 7
          SEPARATOR_NOT_ALLOWED   = 8
          FILESIZE_NOT_ALLOWED    = 9
          HEADER_TOO_LONG         = 10
          DP_ERROR_CREATE         = 11
          DP_ERROR_SEND           = 12
          DP_ERROR_WRITE          = 13
          UNKNOWN_DP_ERROR        = 14
          ACCESS_DENIED           = 15
          DP_OUT_OF_MEMORY        = 16
          DISK_FULL               = 17
          DP_TIMEOUT              = 18
          FILE_NOT_FOUND          = 19
          DATAPROVIDER_EXCEPTION  = 20
          CONTROL_FLUSH_ERROR     = 21
          OTHERS                  = 22.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


    ENDIF.

  ENDIF.


ENDFUNCTION.
