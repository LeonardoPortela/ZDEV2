class ZCL_HRST_PHOTO definition
  public
  final
  create public .

public section.

  methods SAVE_PHOTO
    importing
      !MATRICULA type PA0465-PERNR
      !IMAGE type CPT_X255 .
  methods GET_PHOTO_URL
    importing
      !MATRICULA type PA0465-PERNR
    returning
      value(RESULT) type CHAR255 .
  class-methods GET_URL_FROM_IMAGE
    importing
      !IMAGE_DATA type STRING optional
      !I_BASE_64 type CHAR01 default 'X'
      !IMAGE_DATA_XSTRING type XSTRING optional
    returning
      value(URL) type CHAR255 .
  PROTECTED SECTION.

private section.

  methods ENCODE_PHOTO
    importing
      !IMAGE type CPT_X255
    returning
      value(RESULT) type STRING .
ENDCLASS.



CLASS ZCL_HRST_PHOTO IMPLEMENTATION.


  METHOD encode_photo.
    DATA image_x TYPE xstring.

    LOOP AT image INTO DATA(_image_bin).
      image_x = image_x && _image_bin.
    ENDLOOP.

    DATA strbin TYPE string.

    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        bindata = image_x
      IMPORTING
        b64data = result
*       EXCEPTIONS
*       SSF_KRN_ERROR                  = 1
*       SSF_KRN_NOOP                   = 2
*       SSF_KRN_NOMEMORY               = 3
*       SSF_KRN_OPINV                  = 4
*       SSF_KRN_INPUT_DATA_ERROR       = 5
*       SSF_KRN_INVALID_PAR            = 6
*       SSF_KRN_INVALID_PARLEN         = 7
*       OTHERS  = 8
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.


  METHOD GET_PHOTO_URL.
    DATA IMAGE_DATA   TYPE TABLE OF TLINE.
    DATA IMAGE_BASE64 TYPE STRING.
    DATA OBJECT       TYPE TDOBJECT VALUE 'ZHR_PHOTO'.
    DATA ID           TYPE TDID     VALUE 'IMG'.

    DATA(_NAME) = CONV THEAD-TDNAME( MATRICULA ).

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        CLIENT                  = SY-MANDT
        ID                      = ID
        LANGUAGE                = SY-LANGU
        NAME                    = _NAME
        OBJECT                  = OBJECT
      TABLES
        LINES                   = IMAGE_DATA
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.

    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT IMAGE_DATA INTO DATA(_IMG_DATA).
      IMAGE_BASE64 = IMAGE_BASE64 && _IMG_DATA-TDLINE.
    ENDLOOP.

    RESULT = ME->GET_URL_FROM_IMAGE( IMAGE_DATA = IMAGE_BASE64 ).
  ENDMETHOD.


  METHOD GET_URL_FROM_IMAGE.

    DATA BINARY_DATA TYPE XSTRING.
    DATA STR_TABLE TYPE STANDARD TABLE OF X255.

    CASE I_BASE_64.
      WHEN ABAP_TRUE.
        CALL FUNCTION 'SSFC_BASE64_DECODE'
          EXPORTING
            B64DATA = IMAGE_DATA
          IMPORTING
            BINDATA = BINARY_DATA.
      WHEN ABAP_FALSE.
        IF IMAGE_DATA IS NOT INITIAL.
          CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
            EXPORTING
              TEXT   = IMAGE_DATA
            IMPORTING
              BUFFER = BINARY_DATA
            EXCEPTIONS
              FAILED = 1
              OTHERS = 2.
        ENDIF.

        IF IMAGE_DATA_XSTRING IS NOT INITIAL.
          BINARY_DATA = IMAGE_DATA_XSTRING.
        ENDIF.
    ENDCASE.

    DATA(GRAPHIC_SIZE) = XSTRLEN( BINARY_DATA ).
    CHECK GRAPHIC_SIZE > 0.

    DATA(GRAPHIC_CONV) = GRAPHIC_SIZE.
    DATA(GRAPHIC_OFFS) = 0.

    WHILE GRAPHIC_CONV > 255.
      APPEND BINARY_DATA+GRAPHIC_OFFS(255) TO STR_TABLE.

      GRAPHIC_OFFS = GRAPHIC_OFFS + 255.
      GRAPHIC_CONV = GRAPHIC_CONV - 255.
    ENDWHILE.

    APPEND BINARY_DATA+GRAPHIC_OFFS(GRAPHIC_CONV)
        TO STR_TABLE.

    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        TYPE                 = 'IMAGE'
        SUBTYPE              = 'X-UNKNOWN'
        SIZE                 = GRAPHIC_SIZE
      TABLES
        DATA                 = STR_TABLE
      CHANGING
        URL                  = URL
      EXCEPTIONS
        DP_INVALID_PARAMETER = 1
        DP_ERROR_PUT_TABLE   = 2
        DP_ERROR_GENERAL     = 3
        OTHERS               = 4.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      EXIT.
    ENDIF.
  ENDMETHOD.


  METHOD save_photo.
    DATA img_content  TYPE TABLE OF tline.
    DATA string_table TYPE STANDARD TABLE OF tdline.

    DATA(_result) = me->encode_photo( image ).

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = _result
        i_tabline_length = 132
      TABLES
        et_table         = string_table.

    LOOP AT string_table INTO DATA(_str_table).
      APPEND VALUE #( tdline = _str_table ) TO img_content.
    ENDLOOP.

    DATA(obj_data) =
        VALUE thead(  tdobject = 'ZHR_PHOTO'
                      tdid     = 'IMG'
                      tdname   = matricula
                      tdspras  = sy-langu ).

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client   = sy-mandt
        header   = obj_data
      TABLES
        lines    = img_content
      EXCEPTIONS
        id       = 1
        language = 2
        name     = 3
        object   = 4
        OTHERS   = 5.
  ENDMETHOD.
ENDCLASS.
