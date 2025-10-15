class ZCL_BIOMETRY definition
  public
  final
  create public .

public section.

  class-data USERNAME type STRING .
  class-data COMPUTER type STRING .
  class-data:
    BEGIN OF THUMB,
        LEFT_THUMB  TYPE TDID VALUE 'ZBML',
        RIGHT_THUMB TYPE TDID VALUE 'ZBMR',
      END OF THUMB .
  constants OBJECT_TEXT type TDOBJECT value 'ZBIOMETRIA' ##NO_TEXT.
  constants RESULT_EMPTY type STRING value 'Empty' ##NO_TEXT.

  class-methods VALIDATE_DIGITAL
    importing
      !HASH_VALIDATION_R type STRING optional
      !HASH_VALIDATION_L type STRING optional
      !REGISTRATION type PERSNO optional
    returning
      value(RESULT) type ZMMT0081
    raising
      ZCX_BIOMETRY .
  class-methods REGISTER_DIGITAL
    importing
      !HASH_LEFT_DIGITAL type STRING
      !HASH_RIGHT_DIGITAL type STRING
      !HASH_LEFT_DIGITAL_IMG type XSTRING
      !HASH_RIGHT_DIGITAL_IMG type XSTRING
      !REGISTRATION type PERSNO
      !DT_VAL_DE type DATS
      !DT_VAL_ATE type DATS
      !DT_VAL_DEN type DATS optional
      !DT_VAL_ATEN type DATS optional
    exceptions
      REGISTRATION_FAILED .
  class-methods SPLIT_HASH
    importing
      !INPUT type STRING
    exporting
      !HASH_VALIDATION type STRING
      !BASE64_IMAGE type STRING .
  class-methods GET_HASH_DIGITAL
    exporting
      !IMAGE_XSTRING type XSTRING
      !HASH_DIGITAL type STRING
    returning
      value(RESULT) type STRING
    exceptions
      COMMUNICATION_FAILED
      HASH_DIGITAL_EMPTY .
  class-methods DELETE_DIGITAL
    importing
      !REGISTRATION type PERSNO
    returning
      value(RESULT) type ZMMT0088
    raising
      ZCX_BIOMETRY .
  class-methods READ_DIGITAL
    importing
      !REGISTRATION type PERSNO
    returning
      value(RESULT) type ZMMT0088
    raising
      ZCX_BIOMETRY .
  class-methods BUILD_HASH_VALIDATION
    importing
      !DIGITAL_LEFT_THUMB type STRING optional
      !DIGITAL_RIGHT_THUMB type STRING optional
      !REGISTRATION type PERSNO optional
    returning
      value(RESULT) type STRING
    raising
      ZCX_BIOMETRY .
  class-methods GET_DIGITAL_AS_URL_IMAGE
    importing
      !HASH_DIGITAL type STRING optional
    returning
      value(IMAGE_URL) type CHAR255 .
  class-methods GET_DIGITAL_AS_URL_IMAGE2
    importing
      !IMAGE_XSTRING type XSTRING optional
    returning
      value(IMAGE_URL) type CHAR255 .
  class-methods GET_SYSTEM_DATA
    exporting
      !USERNAME type STRING
      !COMPUTER_NAME type STRING
      !IP_ADRESS type STRING .
  class-methods REGISTER_PASSWORD
    importing
      !SENHA type CHAR10
      !REGISTRATION type PERSNO
      !DT_VAL_DE type DATS
      !DT_VAL_ATE type DATS
      !DT_VAL_DEN type DATS optional
      !DT_VAL_ATEN type DATS optional
      !NU_LIB type ZNU_LIB optional
    exceptions
      REGISTRATION_FAILED .
  class-methods READ_PASSWORD
    importing
      !REGISTRATION type PERSNO
    returning
      value(RESULT) type ZMMT0120
    raising
      ZCX_BIOMETRY .
  PROTECTED SECTION.
private section.

  constants:
    BEGIN OF PORT_BIOMETRY_READER,
        CAPTURE_DIGITAL  TYPE STRING VALUE '33001',
        VALIDATE_DIGITAL TYPE STRING VALUE '33002',
      END OF PORT_BIOMETRY_READER .
ENDCLASS.



CLASS ZCL_BIOMETRY IMPLEMENTATION.


  METHOD BUILD_HASH_VALIDATION.
*----------------------------------------------------------------------------
* Version 1.0 - Enio R. Jesus
* This method build the hash validation using ZCL_BIOMETRY=>SPLIT_HASH
*----------------------------------------------------------------------------
    DATA HASH_VALIDATION TYPE STRING.
    DATA HASH_COMPLEMENT TYPE STRING.
    DATA _DIGITAL_LEFT   LIKE DIGITAL_LEFT_THUMB.
    DATA _DIGITAL_RIGHT  LIKE DIGITAL_RIGHT_THUMB.
    DATA HASH_DIGITAL    TYPE STRING.

    IF ( DIGITAL_LEFT_THUMB  IS INITIAL )
    OR ( DIGITAL_RIGHT_THUMB IS INITIAL ).

      DATA(_USER_DIGITAL) =
        ZCL_BIOMETRY=>READ_DIGITAL( REGISTRATION = REGISTRATION ).

      _DIGITAL_LEFT  = _USER_DIGITAL-POLEGAR_ESQUERDO.
      _DIGITAL_RIGHT = _USER_DIGITAL-POLEGAR_DIREITO.
    ELSE.
      _DIGITAL_LEFT  = DIGITAL_LEFT_THUMB.
      _DIGITAL_RIGHT = DIGITAL_RIGHT_THUMB.
    ENDIF.

    CALL METHOD ZCL_BIOMETRY=>SPLIT_HASH
      EXPORTING
        INPUT           = _DIGITAL_LEFT
      IMPORTING
        HASH_VALIDATION = HASH_VALIDATION.

    HASH_DIGITAL = HASH_VALIDATION && 'AMAGGI'.

    CALL METHOD ZCL_BIOMETRY=>SPLIT_HASH
      EXPORTING
        INPUT           = _DIGITAL_RIGHT
      IMPORTING
        HASH_VALIDATION = HASH_VALIDATION.

    RESULT = HASH_DIGITAL && HASH_VALIDATION.
  ENDMETHOD.


  METHOD DELETE_DIGITAL.

    DELETE FROM ZMMT0088 WHERE MATRICULA EQ REGISTRATION.
    IF SY-SUBRC <> 0.
      RAISE EXCEPTION TYPE ZCX_BIOMETRY
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_BIOMETRY=>DIGITAL_NOT_FOUND-MSGID
                            MSGNO = ZCX_BIOMETRY=>DIGITAL_NOT_FOUND-MSGNO
                            ATTR1 = REGISTRATION
                          ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_DIGITAL_AS_URL_IMAGE.
    CALL METHOD ZCL_BIOMETRY=>SPLIT_HASH
      EXPORTING
        INPUT        = HASH_DIGITAL
      IMPORTING
        BASE64_IMAGE = DATA(_BASE64_IMAGE).

    IMAGE_URL = NEW ZCL_IMAGE_HELPER( )->CREATE_INTERNAL_URL( _BASE64_IMAGE ).
  ENDMETHOD.


  METHOD GET_DIGITAL_AS_URL_IMAGE2.
    IMAGE_URL = NEW ZCL_IMAGE_HELPER( )->CREATE_INTERNAL_URL2( IMAGE_XSTRING ).
  ENDMETHOD.


  METHOD GET_HASH_DIGITAL.
*----------------------------------------------------------------------------
* Version 1.0 - Enio R. Jesus
*
* This is the method that make things happen. Here a http requisition is made
* in the ip adress, where the biometric reader must be installed;
*
* If the reader is working perfect, you should receive a hash in RESULT
* Here go some explanation about this hash:
*
* - It contains two parts (strings) that are separeted with the string 'AMAGGI';
* - The first part before AMAGGI is the hash validation;
* - The second part after AMAGGI is the image;
*
* You can se more about this hash in the method ZCL_BIOMETRY=>split_hash( hash );
*
*-------------------------------------------------------------------------

*    CL_HTTP_CLIENT=>CREATE(
*      EXPORTING
*        HOST               = CL_GUI_FRONTEND_SERVICES=>GET_IP_ADDRESS( )
*        SERVICE            = PORT_BIOMETRY_READER-CAPTURE_DIGITAL
*      IMPORTING
*        CLIENT             = DATA(_CLIENT)
*      EXCEPTIONS
*        ARGUMENT_NOT_FOUND = 1
*        PLUGIN_NOT_ACTIVE  = 2
*        INTERNAL_ERROR     = 3
*        OTHERS             = 4
*    ).
*
*    CALL METHOD _CLIENT->SEND
*      EXCEPTIONS
*        HTTP_COMMUNICATION_FAILURE = 1
*        HTTP_INVALID_STATE         = 2
*        HTTP_PROCESSING_FAILED     = 3
*        HTTP_INVALID_TIMEOUT       = 4
*        OTHERS                     = 5.
*
*    CALL METHOD _CLIENT->RECEIVE
*      EXCEPTIONS
*        HTTP_COMMUNICATION_FAILURE = 1
*        HTTP_INVALID_STATE         = 2
*        HTTP_PROCESSING_FAILED     = 3
*        OTHERS                     = 4.
*
*    CALL METHOD _CLIENT->RESPONSE->GET_STATUS
*      IMPORTING
*        CODE   = DATA(_STATUS_CODE)
*        REASON = DATA(_STATUS_TEXT).
*
*    IF ( _STATUS_CODE = '200' ).
*      RESULT = _CLIENT->RESPONSE->GET_CDATA( ).
*
*      IF RESULT = RESULT_EMPTY.
*        MESSAGE S002(ZBIOMETRIA) RAISING HASH_DIGITAL_EMPTY.
*      ENDIF.
*
*    ELSE.
*      MESSAGE S001(ZBIOMETRIA) RAISING COMMUNICATION_FAILED.
*    ENDIF.

    DATA: LEITURA TYPE REF TO ZCL_NBIOBSP.

    TRY .
        CREATE OBJECT LEITURA.
        LEITURA->ZIF_NBIOBSP~DEVICE->OPEN( ).
      CATCH ZCX_NBIOBSP INTO DATA(EX_NBIOBSP).    "
        EX_NBIOBSP->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING COMMUNICATION_FAILED.
    ENDTRY.

    TRY .
        LEITURA->ZIF_NBIOBSP~EXTRACTION->CAPTURE( I_NPURPOSE = ZIF_NBIOBSP_EXTRACTION=>ST_FIR_PURPOSE_DENTIFY
          )->GET_TEXTENCODEFIR(
          IMPORTING
            E_TEXTENCODEFIR = RESULT
          ).

        LEITURA->ZIF_NBIOBSP~FPIMAGE->SAVE(
          EXPORTING
            I_BSZIMGFILEPATH = 'C:\SAPBiometria\Digital'
            I_NIMAGETYPE     = 3    " Format JPEG = 3
*            I_NFINGERID      = 0
*            I_NSAMPLENUMBER  = 0
          IMPORTING
            E_IMAGE          = DATA(E_IMAGE)
            E_IMAGE_STRING   = DATA(E_IMAGE_STRING)
        ).

        IMAGE_XSTRING = E_IMAGE.
        HASH_DIGITAL  = RESULT.

        E_IMAGE_STRING = CL_HTTP_UTILITY=>ENCODE_BASE64( UNENCODED = E_IMAGE_STRING ).

        RESULT = RESULT && 'AMAGGI' &&  E_IMAGE_STRING.

      CATCH ZCX_NBIOBSP INTO EX_NBIOBSP.
        TRY .
            LEITURA->ZIF_NBIOBSP~DEVICE->CLOSE( ).
            EX_NBIOBSP->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING COMMUNICATION_FAILED.
          CATCH ZCX_NBIOBSP INTO EX_NBIOBSP.
            EX_NBIOBSP->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING COMMUNICATION_FAILED.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.


  METHOD GET_SYSTEM_DATA.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_USER_NAME
      CHANGING
        USER_NAME    = USERNAME
      EXCEPTIONS
        CNTL_ERROR   = 1
        ERROR_NO_GUI = 2
        OTHERS       = 3.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_COMPUTER_NAME
      CHANGING
        COMPUTER_NAME        = COMPUTER
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        NOT_SUPPORTED_BY_GUI = 3
        OTHERS               = 4.

    CALL METHOD CL_GUI_CFW=>UPDATE_VIEW
      EXCEPTIONS
        CNTL_SYSTEM_ERROR = 1
        CNTL_ERROR        = 2
        OTHERS            = 3.

    USERNAME      = USERNAME.
    COMPUTER_NAME = COMPUTER.
    IP_ADRESS     = CL_GUI_FRONTEND_SERVICES=>GET_IP_ADDRESS( ).

  ENDMETHOD.


  METHOD READ_DIGITAL.
    SELECT SINGLE *
      FROM ZMMT0088
      INTO RESULT
     WHERE MATRICULA  EQ REGISTRATION
     AND   DT_VAL_DE  LE SY-DATUM
     AND   DT_VAL_ATE GE SY-DATUM.

    IF SY-SUBRC <> 0.
      SELECT SINGLE *
      FROM ZMMT0088
      INTO RESULT
      WHERE MATRICULA  EQ REGISTRATION
      AND   DT_VAL_DE  LT '19000101'.
      IF SY-SUBRC <> 0.
        RAISE EXCEPTION TYPE ZCX_BIOMETRY
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_BIOMETRY=>DIGITAL_NOT_FOUND-MSGID
                              MSGNO = ZCX_BIOMETRY=>DIGITAL_NOT_FOUND-MSGNO
                              ATTR1 = REGISTRATION
                            ).
      ELSE.
        RESULT-DT_VAL_DE  =  '19000101'.
        RESULT-DT_VAL_ATE =  '99991231'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD READ_PASSWORD.
    SELECT *
    FROM ZMMT0120
    INTO TABLE @DATA(T_RESULT)
     WHERE MATRICULA  EQ @REGISTRATION
     AND   DT_VAL_DE  LE @SY-DATUM
     AND   DT_VAL_ATE GE @SY-DATUM.

    IF SY-SUBRC <> 0.
      SELECT SINGLE *
      FROM ZMMT0120
      INTO RESULT
      WHERE MATRICULA  EQ REGISTRATION
      AND   DT_VAL_DE  LT '19000101'.
      IF SY-SUBRC <> 0.
        RAISE EXCEPTION TYPE ZCX_BIOMETRY
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_BIOMETRY=>SENHA_NOT_FOUND-MSGID
                              MSGNO = ZCX_BIOMETRY=>SENHA_NOT_FOUND-MSGNO
                              ATTR1 = REGISTRATION
                            ).
      ELSE.
        RESULT-DT_VAL_DE  =  '19000101'.
        RESULT-DT_VAL_ATE =  '99991231'.
      ENDIF.
    ELSE.
      SORT T_RESULT BY DT_VAL_DE DESCENDING.
      READ TABLE T_RESULT INTO RESULT INDEX 1.
    ENDIF.
  ENDMETHOD.


  METHOD REGISTER_DIGITAL.
* -------------------------------------------------------------------------------------------
* Version 1.0 - Enio R. Jesus
* The user's digital registration is made by ZMM0121 transaction;
* The HASH_LEFT_DIGITAL and HASH_RIGHT_DIGITAL is taken by ZCL_BIOMETRY=>get_hash_digital( ).
* -------------------------------------------------------------------------------------------

    DATA: LC_HASH_LEFT_DIGITAL_IMG  TYPE STRING,
          LC_HASH_RIGHT_DIGITAL_IMG TYPE STRING.

    DATA: WA_ZMMT0088 TYPE ZMMT0088,
          VDT_VAL_DE  TYPE ZMMT0088-DT_VAL_DE,
          VDT_VAL_ATE TYPE ZMMT0088-DT_VAL_ATE.

    VDT_VAL_DE  = DT_VAL_DE.
    VDT_VAL_ATE = DT_VAL_ATE.
    CALL METHOD ZCL_BIOMETRY=>GET_SYSTEM_DATA(
      IMPORTING
        USERNAME      = DATA(_USERNAME)
        COMPUTER_NAME = DATA(_COMPUTER_NAME)
        IP_ADRESS     = DATA(_IP_ADRESS) ).

    IF DT_VAL_DEN IS NOT INITIAL. "Nova digital
      CLEAR WA_ZMMT0088.
      SELECT SINGLE *
        FROM ZMMT0088
        INTO WA_ZMMT0088
        WHERE MATRICULA  EQ REGISTRATION
        AND   DT_VAL_DE  EQ DT_VAL_DE
        AND   DT_VAL_ATE EQ DT_VAL_ATE.

      IF SY-SUBRC NE 0.
        SELECT SINGLE *
           FROM ZMMT0088
           INTO WA_ZMMT0088
           WHERE MATRICULA  EQ REGISTRATION
           AND   DT_VAL_DE  LT '19000101'.
        IF SY-SUBRC = 0.
          DELETE FROM ZMMT0088
          WHERE MATRICULA  EQ REGISTRATION
          AND   DT_VAL_DE  LT '19000101'.
        ENDIF.
      ELSE.
        DELETE FROM ZMMT0088
        WHERE MATRICULA  EQ REGISTRATION
        AND   DT_VAL_DE  EQ DT_VAL_DE
        AND   DT_VAL_ATE EQ DT_VAL_ATE.
      ENDIF.
      IF WA_ZMMT0088 IS NOT INITIAL.
        IF WA_ZMMT0088-DT_VAL_DE IS INITIAL.
          WA_ZMMT0088-DT_VAL_DE = '19000101'.
        ENDIF.
        WA_ZMMT0088-DT_VAL_ATE = SY-DATUM - 1.
      ENDIF.
      MODIFY ZMMT0088 FROM WA_ZMMT0088.
      COMMIT WORK.
      VDT_VAL_DE  = DT_VAL_DEN.
      VDT_VAL_ATE = DT_VAL_ATEN.
    ENDIF.

    DATA(_ITEM)
      = VALUE ZMMT0088( MATRICULA           = REGISTRATION
                        DT_VAL_DE           = VDT_VAL_DE
                        DT_VAL_ATE          = VDT_VAL_ATE
                        POLEGAR_ESQUERDO    = HASH_LEFT_DIGITAL
                        POLEGAR_DIREITO     = HASH_RIGHT_DIGITAL
                        DATA_CRIACAO        = SY-DATUM
                        HORA_CRIACAO        = SY-UZEIT
                        CRIADO_POR          = SY-UNAME
                        NOME_COMPUTADOR     = _COMPUTER_NAME
                        USUARIO_LOGADO      = _USERNAME
                        IP                  = _IP_ADRESS
                        IM_POLEGAR_ESQUERDO = HASH_LEFT_DIGITAL_IMG
                        IM_POLEGAR_DIREITO  = HASH_RIGHT_DIGITAL_IMG
                      ).

    MODIFY ZMMT0088 FROM _ITEM.
    COMMIT WORK.
  ENDMETHOD.


  METHOD REGISTER_PASSWORD.
    DATA: LC_HASH_LEFT_DIGITAL_IMG  TYPE STRING,
          LC_HASH_RIGHT_DIGITAL_IMG TYPE STRING.

    DATA: WA_ZMMT0120 TYPE ZMMT0120,
          VDT_VAL_DE  TYPE ZMMT0120-DT_VAL_DE,
          VDT_VAL_ATE TYPE ZMMT0120-DT_VAL_ATE.

    VDT_VAL_DE  = DT_VAL_DE.
    VDT_VAL_ATE = DT_VAL_ATE.
    CALL METHOD ZCL_BIOMETRY=>GET_SYSTEM_DATA(
      IMPORTING
        USERNAME      = DATA(_USERNAME)
        COMPUTER_NAME = DATA(_COMPUTER_NAME)
        IP_ADRESS     = DATA(_IP_ADRESS) ).

    IF DT_VAL_DEN IS NOT INITIAL. "Nova digital
      CLEAR WA_ZMMT0120.
      SELECT SINGLE *
        FROM ZMMT0120
        INTO WA_ZMMT0120
        WHERE MATRICULA  EQ REGISTRATION
        AND   DT_VAL_DE  EQ DT_VAL_DE
        AND   DT_VAL_ATE EQ DT_VAL_ATE.

      IF SY-SUBRC NE 0.
        SELECT SINGLE *
           FROM ZMMT0120
           INTO WA_ZMMT0120
           WHERE MATRICULA  EQ REGISTRATION
           AND   DT_VAL_DE  LT '19000101'.
        IF SY-SUBRC = 0.
          DELETE FROM ZMMT0120
          WHERE MATRICULA  EQ REGISTRATION
          AND   DT_VAL_DE  LT '19000101'.
        ENDIF.
      ELSE.
        DELETE FROM ZMMT0120
        WHERE MATRICULA  EQ REGISTRATION
        AND   DT_VAL_DE  EQ DT_VAL_DE
        AND   DT_VAL_ATE EQ DT_VAL_ATE.
      ENDIF.
      IF WA_ZMMT0120 IS NOT INITIAL.
        IF WA_ZMMT0120-DT_VAL_DE IS INITIAL.
          WA_ZMMT0120-DT_VAL_DE = '19000101'.
        ENDIF.
        WA_ZMMT0120-DT_VAL_ATE = SY-DATUM - 1.
      ENDIF.
      MODIFY ZMMT0120 FROM WA_ZMMT0120.
      COMMIT WORK.
      VDT_VAL_DE  = DT_VAL_DEN.
      VDT_VAL_ATE = DT_VAL_ATEN.
    ENDIF.

    DATA(_ITEM)
      = VALUE ZMMT0120( MATRICULA           = REGISTRATION
                        DT_VAL_DE           = VDT_VAL_DE
                        DT_VAL_ATE          = VDT_VAL_ATE
                        SENHA               = SENHA
                        DATA_CRIACAO        = SY-DATUM
                        HORA_CRIACAO        = SY-UZEIT
                        CRIADO_POR          = SY-UNAME
                        NOME_COMPUTADOR     = _COMPUTER_NAME
                        USUARIO_LOGADO      = _USERNAME
                        IP                  = _IP_ADRESS
                        NU_LIB              = NU_LIB
                      ).

    MODIFY ZMMT0120 FROM _ITEM.
    COMMIT WORK.
  ENDMETHOD.


  METHOD SPLIT_HASH.
*--------------------------------------------------------------------------------------
* Version 1.0 - Enio R. Jesus
*
* This method separate the hash validation from digial image that are taken by
* ZCL_BIOMETRY=>get_hash_digital( );
*
* The hash validation can be used to validate the user's registered digital,
* the base64 image, could be displayed as follow:
*
*    ZCL_IMAGE_HELPER=>DISPLAY(
*      EXPORTING
*        CUSTOM_NAME      = 'CONTAINER_NAME'
*        URL              = ZCL_IMAGE_HELPER=>get_url_from_image( BASE64_IMAGE )
*      CHANGING
*        CUSTOM_INSTANCE  = CUSTOM_INSTANCE
*        PICTURE_INSTANCE = PICTURE_INSTANCE
*    ).

    SPLIT INPUT AT 'AMAGGI' INTO HASH_VALIDATION BASE64_IMAGE.
  ENDMETHOD.


  METHOD VALIDATE_DIGITAL.
*----------------------------------------------------------------------------------
* Version 1.0 - Enio R. Jesus
*
* This is the method is used to validate a registered digital.
*
* If the reader is working perfect, you should receive a hash in RESULT
* Here go some explanation about this hash:
*
* - It contains two parts (strings) that are separeted with the string 'AMAGGI';
* - The first part before AMAGGI is the hash validation;
* - The second part after AMAGGI is the image;
*
* You can se more about this hash in the method ZCL_BIOMETRY=>BUILD_HASH_VALIDATION;
*
*----------------------------------------------------------------------------------
*    CL_HTTP_CLIENT=>CREATE(
*       EXPORTING
*         HOST               = CL_GUI_FRONTEND_SERVICES=>GET_IP_ADDRESS( )
*         SERVICE            = ZCL_BIOMETRY=>PORT_BIOMETRY_READER-VALIDATE_DIGITAL
*       IMPORTING
*         CLIENT             = DATA(_CLIENT)
*       EXCEPTIONS
*         ARGUMENT_NOT_FOUND = 1
*         PLUGIN_NOT_ACTIVE  = 2
*         INTERNAL_ERROR     = 3
*         OTHERS             = 4
*     ).
*
*    DATA(_HASH_VALIDATION) =
*      COND #( WHEN HASH_VALIDATION IS INITIAL THEN ZCL_BIOMETRY=>BUILD_HASH_VALIDATION( REGISTRATION = REGISTRATION )
*              ELSE HASH_VALIDATION ).
*
*    CALL METHOD _CLIENT->REQUEST->SET_CDATA
*      EXPORTING
*        DATA   = _HASH_VALIDATION
*        OFFSET = 0
*        LENGTH = STRLEN( _HASH_VALIDATION ).
*
*    CALL METHOD _CLIENT->SEND
*      EXCEPTIONS
*        HTTP_COMMUNICATION_FAILURE = 1
*        HTTP_INVALID_STATE         = 2
*        HTTP_PROCESSING_FAILED     = 3
*        HTTP_INVALID_TIMEOUT       = 4
*        OTHERS                     = 5.
*
*    CL_PROGRESS_INDICATOR=>PROGRESS_INDICATE( I_MSGID = 'ZBIOMETRIA' I_MSGNO = 012 I_OUTPUT_IMMEDIATELY = ABAP_TRUE ).
*
*    CALL METHOD _CLIENT->RECEIVE
*      EXCEPTIONS
*        HTTP_COMMUNICATION_FAILURE = 1
*        HTTP_INVALID_STATE         = 2
*        HTTP_PROCESSING_FAILED     = 3
*        OTHERS                     = 4.
*
*    DATA(_RESULT) = _CLIENT->RESPONSE->GET_CDATA( ).
*
*    IF ( _RESULT = 'true' ).
*      MESSAGE S009(ZBIOMETRIA).
*      MOVE ABAP_TRUE TO STATUS.
*    ELSE.
*      MOVE ABAP_FALSE TO STATUS.
*      RAISE EXCEPTION TYPE ZCX_BIOMETRY
*        EXPORTING
*          TEXTID = ZCX_BIOMETRY=>VALIDATION_FAILED.
*    ENDIF.

    DATA: LEITURA TYPE REF TO ZCL_NBIOBSP.
    DATA  IMAGEM  TYPE XSTRING.
    TRY .
        CREATE OBJECT LEITURA.
        LEITURA->ZIF_NBIOBSP~DEVICE->OPEN( ).
      CATCH ZCX_NBIOBSP INTO DATA(EX_NBIOBSP).    "
        RAISE EXCEPTION TYPE ZCX_BIOMETRY
          EXPORTING
            TEXTID = ZCX_BIOMETRY=>VALIDATION_FAILED.
    ENDTRY.

    TRY .
        "Capturar Digital
        LEITURA->ZIF_NBIOBSP~EXTRACTION->CAPTURE( I_NPURPOSE = ZIF_NBIOBSP_EXTRACTION=>ST_FIR_PURPOSE_DENTIFY )->GET_TEXTENCODEFIR( IMPORTING E_TEXTENCODEFIR = DATA(E_TEXTENCODEFIR) ).
      CATCH ZCX_NBIOBSP INTO EX_NBIOBSP.    "
        RAISE EXCEPTION TYPE ZCX_BIOMETRY EXPORTING TEXTID = ZCX_BIOMETRY=>VALIDATION_FAILED.
    ENDTRY.

    TRY .
        "Verificar se é o Direito
        LEITURA->ZIF_NBIOBSP~MATCHING->VERIFYMATCH( EXPORTING I_PROCESSEDFIR = E_TEXTENCODEFIR I_STOREDFIR = HASH_VALIDATION_R ).
        "pega imagem
        LEITURA->ZIF_NBIOBSP~FPIMAGE->SAVE(
                EXPORTING
                  I_BSZIMGFILEPATH = 'C:\SAPBiometria\Digital'
                  I_NIMAGETYPE     = 3    " Format JPEG = 3
*                        I_NFINGERID      = 0
*                        I_NSAMPLENUMBER  = 0
                IMPORTING
                  E_IMAGE          = DATA(E_IMAGE)
                  E_IMAGE_STRING   = DATA(E_IMAGE_STRING)
              ).

        LEITURA->ZIF_NBIOBSP~DEVICE->CLOSE( ).
        RESULT-LADO = 'R'.
        RESULT-POLEGAR = E_TEXTENCODEFIR.
        RESULT-IM_POLEGAR =  E_IMAGE.
      CATCH ZCX_NBIOBSP.    "
        TRY .
            "Verificar se é o Esquerdo
            LEITURA->ZIF_NBIOBSP~MATCHING->VERIFYMATCH( EXPORTING I_PROCESSEDFIR = E_TEXTENCODEFIR I_STOREDFIR = HASH_VALIDATION_L ).
            "pega imagem
            LEITURA->ZIF_NBIOBSP~FPIMAGE->SAVE(
              EXPORTING
                I_BSZIMGFILEPATH = 'C:\SAPBiometria\Digital'
                I_NIMAGETYPE     = 3    " Format JPEG = 3
*                              I_NFINGERID      = 0
*                              I_NSAMPLENUMBER  = 0
              IMPORTING
                E_IMAGE          = DATA(E_IMAGEL)
                E_IMAGE_STRING   = DATA(E_IMAGE_STRINGL)
              ).

            LEITURA->ZIF_NBIOBSP~DEVICE->CLOSE( ).
            RESULT-LADO       = 'L'.
            RESULT-POLEGAR    = E_TEXTENCODEFIR.
            RESULT-IM_POLEGAR = E_IMAGEL.
          CATCH ZCX_NBIOBSP.
            TRY .
                LEITURA->ZIF_NBIOBSP~DEVICE->CLOSE( ).
                RAISE EXCEPTION TYPE ZCX_BIOMETRY EXPORTING TEXTID = ZCX_BIOMETRY=>VALIDATION_FAILED.
              CATCH ZCX_NBIOBSP INTO EX_NBIOBSP.
                RAISE EXCEPTION TYPE ZCX_BIOMETRY EXPORTING TEXTID = ZCX_BIOMETRY=>VALIDATION_FAILED.
            ENDTRY.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
