class ZCL_ARQUIVO definition
  public
  create public .

public section.

  class-methods GET_FILE_NAME
    importing
      !I_TITULO type STRING
      !I_DEFAULT_EXTENSION type STRING optional
      !I_DEFAULT_FILENAME type STRING optional
      !I_FILE_FILTER type STRING optional
      !I_WITH_ENCODING type ABAP_BOOL optional
      !I_INITIAL_DIRECTORY type STRING optional
      !I_MULTISELECTION type ABAP_BOOL optional
    exporting
      value(E_ARQUIVO) type FILETABLE
    raising
      ZCX_ARQUIVO .
  class-methods SET_FILE
    importing
      !I_FILE_NAME type STRING
      !I_FILETYPE type CHAR10 default 'ASC'
      !I_APPEND type CHAR01 default SPACE
      !I_WRITE_FIELD_SEPARATOR type CHAR01 default SPACE
      !I_HEADER type XSTRING default '00'
      !I_TRUNC_TRAILING_BLANKS type CHAR01 default SPACE
      !I_WRITE_LF type CHAR01 default 'X'
      !I_COL_SELECT type CHAR01 default SPACE
      !I_COL_SELECT_MASK type CHAR255 default SPACE
      !I_DAT_MODE type CHAR01 default SPACE
      !I_CONFIRM_OVERWRITE type CHAR01 default SPACE
      !I_NO_AUTH_CHECK type CHAR01 default SPACE
      !I_CODEPAGE type ABAP_ENCODING default SPACE
      !I_IGNORE_CERR type ABAP_BOOL default ABAP_TRUE
      !I_REPLACEMENT type ABAP_REPL default '#'
      !I_WRITE_BOM type ABAP_BOOL default SPACE
      !I_TRUNC_TRAILING_BLANKS_EOL type CHAR01 default 'X'
      !I_WK1_N_FORMAT type C default SPACE
      !I_WK1_N_SIZE type C default SPACE
      !I_WK1_T_FORMAT type C default SPACE
      !I_WK1_T_SIZE type C default SPACE
      !I_SHOW_TRANSFER_STATUS type CHAR01 default 'X'
      !I_FIELDNAMES type STANDARD TABLE optional
      !I_WRITE_LF_AFTER_LAST_LINE type ABAP_BOOL default 'X'
      !I_VIRUS_SCAN_PROFILE type VSCAN_PROFILE default '/SCET/GUI_DOWNLOAD'
    returning
      value(R_ARQUIVO) type ZDE_TDLINE_1000_T
    raising
      ZCX_ARQUIVO .
  class-methods GET_FILE_XLS
    importing
      !I_FILE_NAME type STRING
    returning
      value(R_ARQUIVO) type ISSR_ALSMEX_TABLINE
    raising
      ZCX_ARQUIVO .
  class-methods GET_FILE
    importing
      !I_FILENAME type STRING default SPACE
      !I_FILETYPE type CHAR10 default 'ASC'
      !I_HAS_FIELD_SEPARATOR type CHAR01 default SPACE
      !I_HEADER_LENGTH type I default 0
      !I_READ_BY_LINE type CHAR01 default 'X'
      !I_DAT_MODE type CHAR01 default SPACE
      !I_CODEPAGE type ABAP_ENCODING default SPACE
      !I_IGNORE_CERR type ABAP_BOOL default ABAP_TRUE
      !I_REPLACEMENT type ABAP_REPL default '#'
      !I_VIRUS_SCAN_PROFILE type VSCAN_PROFILE optional
    exporting
      !E_FILELENGTH type I
      !E_HEADER type XSTRING
    changing
      !I_ISSCANPERFORMED type CHAR01 default SPACE
    returning
      value(R_ARQUIVO) type ZDE_WALINE_STRING_T
    raising
      ZCX_ARQUIVO .
  methods GET_FILE_URI_GET
    importing
      !I_URI type STRING
      !I_CONTENT_TYPE type STRING optional
    exporting
      !E_TEXTO type STRING
      !E_TEXTO_2 type XSTRING
    returning
      value(R_ARQUIVO) type ref to ZCL_ARQUIVO
    raising
      ZCX_ARQUIVO .
  class-methods GET_FILE_URI_GET_
    importing
      !I_URI type STRING
      !I_CONTENT_TYPE type STRING optional
      !I_VIEW_PDF type CHAR01 default ' '
    exporting
      !E_TEXTO type STRING
      !E_TEXTO_2 type XSTRING
      !E_CODE type I
      !E_REASON type STRING
    raising
      ZCX_ARQUIVO .
  methods GET_FILE_READ
    importing
      !I_NAME_ARQUIVO type STRING
    exporting
      !E_TEXTO_X type XSTRING
      !E_TEXTO type STRING
    returning
      value(R_ARQUIVO) type ref to ZCL_ARQUIVO .
  methods SET_FILE_READ
    importing
      !I_TEXTO type STRING optional
      !I_TEXTO_2 type XSTRING optional
      !I_NAME_ARQUIVO type STRING
    returning
      value(R_ARQUIVO) type ref to ZCL_ARQUIVO .
  class-methods SET_ENVIA_ARQUIVOS_E_MAIL
    importing
      !I_MAIL_ORIGEM type CRMS_EMAIL_RECIPIENT optional
      !I_MAIL_DESTINO type CRMT_EMAIL_RECIPIENTS
      !I_BODY type CRMS_EMAIL_MIME_STRUC optional
      !I_ASSUNTO type STRING
      !I_ANEXOS type CRMT_EMAIL_MIME_STRUC optional .
  class-methods GET_SMARTFORM_PROGRAM
    importing
      !I_INSTANCE_MEMORY_AREA type STRING
      !I_PROGRAM type PROGRAMM
      !I_PARAMETROS type RSPARAMS_TT
    returning
      value(R_OTF) type TT_ITCOO
    raising
      ZCX_ARQUIVO .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_ARQUIVO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ARQUIVO IMPLEMENTATION.


  METHOD GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_ARQUIVO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_ARQUIVO=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_ARQUIVO=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_ARQUIVO=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_ARQUIVO=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  method GET_FILE.

    CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD(
      EXPORTING
        FILENAME                =  I_FILENAME
        FILETYPE                =  I_FILETYPE
        HAS_FIELD_SEPARATOR     =  I_HAS_FIELD_SEPARATOR
        HEADER_LENGTH           =  I_HEADER_LENGTH
        READ_BY_LINE            =  I_READ_BY_LINE
        DAT_MODE                =  I_DAT_MODE
        CODEPAGE                =  I_CODEPAGE
        IGNORE_CERR             =  I_IGNORE_CERR
        REPLACEMENT             =  I_REPLACEMENT
        VIRUS_SCAN_PROFILE      =  I_VIRUS_SCAN_PROFILE
      IMPORTING
        FILELENGTH              = E_FILELENGTH
        HEADER                  = E_HEADER
      CHANGING
        DATA_TAB                = R_ARQUIVO
        ISSCANPERFORMED         = I_ISSCANPERFORMED
      EXCEPTIONS
        FILE_OPEN_ERROR         = 1
        FILE_READ_ERROR         = 2
        NO_BATCH                = 3
        GUI_REFUSE_FILETRANSFER = 4
        INVALID_TYPE            = 5
        NO_AUTHORITY            = 6
        UNKNOWN_ERROR           = 7
        BAD_DATA_FORMAT         = 8
        HEADER_NOT_ALLOWED      = 9
        SEPARATOR_NOT_ALLOWED   = 10
        HEADER_TOO_LONG         = 11
        UNKNOWN_DP_ERROR        = 12
        ACCESS_DENIED           = 13
        DP_OUT_OF_MEMORY        = 14
        DISK_FULL               = 15
        DP_TIMEOUT              = 16
        NOT_SUPPORTED_BY_GUI    = 17
        ERROR_NO_GUI            = 18
        OTHERS                  = 19 ).

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ARQUIVO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID  MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGNO  = SY-MSGNO
          MSGID  = SY-MSGID
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.


  endmethod.


  METHOD GET_FILE_NAME.

    DATA: I_RC  TYPE I.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
      EXPORTING
        WINDOW_TITLE            = I_TITULO
        DEFAULT_EXTENSION       = I_DEFAULT_EXTENSION
        DEFAULT_FILENAME        = I_DEFAULT_FILENAME
        FILE_FILTER             = I_FILE_FILTER
        WITH_ENCODING           = I_WITH_ENCODING
        INITIAL_DIRECTORY       = I_INITIAL_DIRECTORY
        MULTISELECTION          = I_MULTISELECTION
      CHANGING
        FILE_TABLE              = E_ARQUIVO
        RC                      = I_RC
*       USER_ACTION             =
*       FILE_ENCODING           =
      EXCEPTIONS
        FILE_OPEN_DIALOG_FAILED = 1
        CNTL_ERROR              = 2
        ERROR_NO_GUI            = 3
        NOT_SUPPORTED_BY_GUI    = 4
        OTHERS                  = 5.

    IF SY-SUBRC IS NOT  INITIAL.
      RAISE EXCEPTION TYPE ZCX_ARQUIVO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID  MSGNO = SY-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGNO  = SY-MSGNO
          MSGID  = SY-MSGID
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.


  ENDMETHOD.


  METHOD GET_FILE_READ.

    DATA: LT_PDF  TYPE TABLE OF CHAR80,
          WA_PDF  LIKE LINE OF LT_PDF,
          ARQUIVO TYPE FILEEXTERN,
          TAMANHO TYPE I.

    R_ARQUIVO = ME.

    ARQUIVO = I_NAME_ARQUIVO.

    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
        ACTIVITY         = 'READ'
        FILENAME         = ARQUIVO
      EXCEPTIONS
        NO_AUTHORITY     = 1
        ACTIVITY_UNKNOWN = 2
        OTHERS           = 3.

    OPEN DATASET ARQUIVO FOR INPUT IN BINARY MODE.
    READ DATASET ARQUIVO INTO E_TEXTO_X.
    CLOSE DATASET ARQUIVO.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        BUFFER        = E_TEXTO_X
      IMPORTING
        OUTPUT_LENGTH = TAMANHO
      TABLES
        BINARY_TAB    = LT_PDF.

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        INPUT_LENGTH = TAMANHO
      IMPORTING
        TEXT_BUFFER  = E_TEXTO
      TABLES
        BINARY_TAB   = LT_PDF
      EXCEPTIONS
        FAILED       = 1
        OTHERS       = 2.

  ENDMETHOD.


  METHOD GET_FILE_URI_GET.

    DATA: LC_URI        TYPE STRING.

    R_ARQUIVO = ME.

    LC_URI = I_URI.

    CL_HTTP_CLIENT=>CREATE_BY_URL(
      EXPORTING
        URL                = LC_URI
      IMPORTING
        CLIENT             = DATA(I_CLIENTE)
      EXCEPTIONS
        ARGUMENT_NOT_FOUND = 1
        PLUGIN_NOT_ACTIVE  = 2
        INTERNAL_ERROR     = 3
        OTHERS             = 4 ).

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ARQUIVO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    I_CLIENTE->REQUEST->SET_METHOD( METHOD = IF_HTTP_REQUEST=>CO_REQUEST_METHOD_GET ).

    IF I_CONTENT_TYPE IS NOT INITIAL.
      I_CLIENTE->REQUEST->SET_HEADER_FIELD( NAME  = 'Content-Type'  VALUE = I_CONTENT_TYPE ).
    ENDIF.

    I_CLIENTE->SEND(
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5
    ).

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ARQUIVO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    I_CLIENTE->RECEIVE(
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        OTHERS                     = 4
    ).
    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ARQUIVO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    E_TEXTO   = I_CLIENTE->RESPONSE->GET_CDATA( ).
    E_TEXTO_2 = I_CLIENTE->RESPONSE->GET_DATA( ).

    I_CLIENTE->CLOSE(
*      EXCEPTIONS
*        HTTP_INVALID_STATE = 1
*        OTHERS             = 2
    ).
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR: I_CLIENTE.


  ENDMETHOD.


  METHOD GET_FILE_URI_GET_.

    DATA: LC_URI   TYPE STRING,
          PDF_DATA TYPE XSTRING.

    LC_URI = I_URI.

    CL_HTTP_CLIENT=>CREATE_BY_URL(
      EXPORTING
        URL                = LC_URI
      IMPORTING
        CLIENT             = DATA(I_CLIENTE)
      EXCEPTIONS
        ARGUMENT_NOT_FOUND = 1
        PLUGIN_NOT_ACTIVE  = 2
        INTERNAL_ERROR     = 3
        OTHERS             = 4 ).

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ARQUIVO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    I_CLIENTE->REQUEST->SET_METHOD( METHOD = IF_HTTP_REQUEST=>CO_REQUEST_METHOD_GET ).

    IF I_CONTENT_TYPE IS NOT INITIAL.
      I_CLIENTE->REQUEST->SET_HEADER_FIELD( NAME  = 'Content-Type'  VALUE = I_CONTENT_TYPE ).
    ENDIF.

    I_CLIENTE->SEND(
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5
    ).

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ARQUIVO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    I_CLIENTE->RECEIVE(
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        OTHERS                     = 4
    ).
    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ARQUIVO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    E_TEXTO   = I_CLIENTE->RESPONSE->GET_CDATA( ).
    E_TEXTO_2 = I_CLIENTE->RESPONSE->GET_DATA( ).

    I_CLIENTE->RESPONSE->GET_STATUS(
      IMPORTING
        CODE   = E_CODE
        REASON = E_REASON
    ).

    I_CLIENTE->CLOSE( ).

    CLEAR: I_CLIENTE.

    IF I_VIEW_PDF EQ ABAP_TRUE.
      CALL FUNCTION 'ZSMARTFORMS_PDF_FILE_PREVIEW'
        EXPORTING
          PDF_DATA = E_TEXTO_2.
    ENDIF.

  ENDMETHOD.


  METHOD GET_FILE_XLS.

    DATA: LC_FILENAME TYPE RLGRAP-FILENAME.

    MOVE I_FILE_NAME TO LC_FILENAME.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        FILENAME                = LC_FILENAME
        I_BEGIN_COL             = 1
        I_BEGIN_ROW             = 1
        I_END_COL               = 100
        I_END_ROW               = 64000
      TABLES
        INTERN                  = R_ARQUIVO
      EXCEPTIONS
        INCONSISTENT_PARAMETERS = 1
        UPLOAD_OLE              = 2
        OTHERS                  = 3.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ARQUIVO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID  MSGNO = SY-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGNO  = SY-MSGNO
          MSGID  = SY-MSGID
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.


  ENDMETHOD.


  METHOD GET_SMARTFORM_PROGRAM.

    DATA: LC_NM_INSTANCE TYPE C LENGTH 80,
          ROOT           TYPE REF TO ZCL_MEMORY_VARIAVEIS,
          OREF           TYPE REF TO ZCL_MEMORY_VARIAVEIS.

    TRY .

        LC_NM_INSTANCE = I_INSTANCE_MEMORY_AREA.
        DATA(HANDLE) = ZCL_MEMORY_VARIAVEIS_AREA=>ATTACH_FOR_WRITE( INST_NAME = LC_NM_INSTANCE ).

        CREATE OBJECT ROOT AREA HANDLE HANDLE TYPE ZCL_MEMORY_VARIAVEIS.
        HANDLE->SET_ROOT( ROOT ).
        HANDLE->DETACH_COMMIT( ).

        SUBMIT (I_PROGRAM) WITH  SELECTION-TABLE I_PARAMETROS AND RETURN.

        HANDLE = ZCL_MEMORY_VARIAVEIS_AREA=>ATTACH_FOR_READ( INST_NAME = LC_NM_INSTANCE ).
        OREF ?= HANDLE->ROOT.
        OREF->GET_TEXTO_OTF( IMPORTING E_OTF = R_OTF ).
        CLEAR OREF.
        HANDLE->DETACH( ).

      CATCH CX_ROOT INTO DATA(OEXCP).
        ZCL_ARQUIVO=>GERA_ERRO_GERAL( I_TEXTO = OEXCP->IF_MESSAGE~GET_LONGTEXT( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD SET_ENVIA_ARQUIVOS_E_MAIL.

    DATA: IT_BAPIRET2   TYPE TABLE OF BAPIRET2,
          IT_BAPIADSMTP TYPE TABLE OF BAPIADSMTP.

    DATA: LO_CREATE_MAIL  TYPE REF TO CL_CRM_EMAIL_DATA.
    CREATE OBJECT LO_CREATE_MAIL.

    CLEAR: LO_CREATE_MAIL->SUBJECT.

    LO_CREATE_MAIL->SUBJECT = I_ASSUNTO.

    IF I_BODY IS NOT INITIAL.
      APPEND I_BODY TO LO_CREATE_MAIL->BODY.
    ENDIF.

    LOOP AT I_ANEXOS INTO DATA(WA_ANEXOS).
      APPEND WA_ANEXOS TO LO_CREATE_MAIL->BODY.
    ENDLOOP.

    IF I_MAIL_ORIGEM IS INITIAL.
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          USERNAME = SY-UNAME
        TABLES
          RETURN   = IT_BAPIRET2
          ADDSMTP  = IT_BAPIADSMTP.

      READ TABLE IT_BAPIADSMTP INDEX 1 INTO DATA(WA_BAPIADSMTP).
      IF SY-SUBRC IS INITIAL.
        LO_CREATE_MAIL->FROM = VALUE #( NAME = WA_BAPIADSMTP-E_MAIL ADDRESS = WA_BAPIADSMTP-E_MAIL ).
      ENDIF.
    ELSE.
      LO_CREATE_MAIL->FROM = I_MAIL_ORIGEM.
    ENDIF.
    LO_CREATE_MAIL->TO[] = I_MAIL_DESTINO[].

    CL_CRM_EMAIL_UTILITY_BASE=>SEND_EMAIL( IV_MAIL_DATA = LO_CREATE_MAIL ).

  ENDMETHOD.


  METHOD SET_FILE.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
      EXPORTING
*       BIN_FILESIZE              =
        FILENAME                  = I_FILE_NAME
        FILETYPE                  = I_FILETYPE
        APPEND                    = I_APPEND
        WRITE_FIELD_SEPARATOR     = I_WRITE_FIELD_SEPARATOR
        HEADER                    = I_HEADER
        TRUNC_TRAILING_BLANKS     = I_TRUNC_TRAILING_BLANKS
        WRITE_LF                  = I_WRITE_LF
        COL_SELECT                = I_COL_SELECT
        COL_SELECT_MASK           = I_COL_SELECT_MASK
        DAT_MODE                  = I_DAT_MODE
        CONFIRM_OVERWRITE         = I_CONFIRM_OVERWRITE
        NO_AUTH_CHECK             = I_NO_AUTH_CHECK
        CODEPAGE                  = I_CODEPAGE
        IGNORE_CERR               = I_IGNORE_CERR
        REPLACEMENT               = I_REPLACEMENT
        WRITE_BOM                 = I_WRITE_BOM
        TRUNC_TRAILING_BLANKS_EOL = I_TRUNC_TRAILING_BLANKS_EOL
        WK1_N_FORMAT              = I_WK1_N_FORMAT
        WK1_N_SIZE                = I_WK1_N_SIZE
        WK1_T_FORMAT              = I_WK1_T_FORMAT
        WK1_T_SIZE                = I_WK1_T_SIZE
        SHOW_TRANSFER_STATUS      = I_SHOW_TRANSFER_STATUS
        FIELDNAMES                = I_FIELDNAMES
        WRITE_LF_AFTER_LAST_LINE  = I_WRITE_LF_AFTER_LAST_LINE
        VIRUS_SCAN_PROFILE        = I_VIRUS_SCAN_PROFILE
*      IMPORTING
*       FILELENGTH                =
      CHANGING
        DATA_TAB                  = R_ARQUIVO
      EXCEPTIONS
        FILE_WRITE_ERROR          = 1
        NO_BATCH                  = 2
        GUI_REFUSE_FILETRANSFER   = 3
        INVALID_TYPE              = 4
        NO_AUTHORITY              = 5
        UNKNOWN_ERROR             = 6
        HEADER_NOT_ALLOWED        = 7
        SEPARATOR_NOT_ALLOWED     = 8
        FILESIZE_NOT_ALLOWED      = 9
        HEADER_TOO_LONG           = 10
        DP_ERROR_CREATE           = 11
        DP_ERROR_SEND             = 12
        DP_ERROR_WRITE            = 13
        UNKNOWN_DP_ERROR          = 14
        ACCESS_DENIED             = 15
        DP_OUT_OF_MEMORY          = 16
        DISK_FULL                 = 17
        DP_TIMEOUT                = 18
        FILE_NOT_FOUND            = 19
        DATAPROVIDER_EXCEPTION    = 20
        CONTROL_FLUSH_ERROR       = 21
        NOT_SUPPORTED_BY_GUI      = 22
        ERROR_NO_GUI              = 23
        OTHERS                    = 24.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_ARQUIVO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID  MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGNO  = SY-MSGNO
          MSGID  = SY-MSGID
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.


  ENDMETHOD.


  METHOD SET_FILE_READ.

    DATA: LT_PDF  TYPE TABLE OF CHAR80,
          ARQUIVO TYPE FILEEXTERN.

    R_ARQUIVO = ME.

    ARQUIVO = I_NAME_ARQUIVO.

    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
        ACTIVITY         = 'WRITE'
        FILENAME         = ARQUIVO
      EXCEPTIONS
        NO_AUTHORITY     = 1
        ACTIVITY_UNKNOWN = 2
        OTHERS           = 3.

    IF I_TEXTO_2 IS NOT INITIAL.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          BUFFER     = I_TEXTO_2
        TABLES
          BINARY_TAB = LT_PDF.

      OPEN DATASET ARQUIVO FOR OUTPUT IN BINARY MODE.
      IF SY-SUBRC IS INITIAL.
        LOOP AT LT_PDF INTO DATA(LS_PDF).
          TRANSFER LS_PDF TO ARQUIVO NO END OF LINE.
        ENDLOOP.
        CLOSE DATASET ARQUIVO.
      ENDIF.

    ENDIF.

    IF I_TEXTO IS NOT INITIAL.
      OPEN DATASET ARQUIVO FOR OUTPUT IN BINARY MODE.
      IF SY-SUBRC IS INITIAL.
        TRANSFER I_TEXTO TO ARQUIVO.
        CLOSE DATASET ARQUIVO.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
