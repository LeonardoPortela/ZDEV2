class ZCL_IMAGE_HELPER definition
  public
  final
  create public .

public section.

  class-data:
    BEGIN OF ID_REPOSITORY,
        HR_PHOTO TYPE TDID VALUE 'HR',
        GENERAL  TYPE TDID VALUE 'IMG',
      END OF ID_REPOSITORY .
  class-data OBJECT_REPOSITORY type TDOBJECT value 'ZIMAGES' ##NO_TEXT.

  methods SAVE_PHOTO
    importing
      !OBJECT type TDOBJECT default OBJECT_REPOSITORY
      !ID type TDID default ID_REPOSITORY-GENERAL
      !IMAGE type CPT_X255
      !NAME type TDOBNAME .
  methods GET_PHOTO_URL
    importing
      !OBJECT type TDOBJECT default OBJECT_REPOSITORY
      !ID type TDID default ID_REPOSITORY-GENERAL
      !NAME type TDOBNAME
    returning
      value(RESULT) type CHAR255 .
  methods GET_PHOTO
    importing
      !OBJECT type TDOBJECT default OBJECT_REPOSITORY
      !ID type TDID default ID_REPOSITORY-GENERAL
      !NAME type TDOBNAME
    returning
      value(RESULT) type STRING .
  methods CREATE_INTERNAL_URL
    importing
      !IMAGE_DATA type STRING
    returning
      value(URL) type CHAR255 .
  methods CREATE_INTERNAL_URL2
    importing
      !IMAGE_DATA type XSTRING
    returning
      value(URL) type CHAR255 .
  methods DISPLAY
    importing
      !CUSTOM_NAME type CHAR50
      !URL type CHAR255
    changing
      !CUSTOM_INSTANCE type ref to CL_GUI_CUSTOM_CONTAINER
      !PICTURE_INSTANCE type ref to CL_GUI_PICTURE .
*        !P_RESIDENT   TYPE STXBITMAPS-RESIDENT OPTIONAL
*        !P_AUTOHEIGHT TYPE STXBITMAPS-AUTOHEIGHT OPTIONAL
*        !P_BMCOMP     TYPE STXBITMAPS-BMCOMP OPTIONAL
  methods SAVE_BITMAP_BDS
    importing
      !NAME type STXBITMAPS-TDNAME
      !OBJECT type STXBITMAPS-TDOBJECT default 'GRAPHICS'
      !ID type STXBITMAPS-TDID default 'BMAP'
      !TYPE type STXBITMAPS-TDBTYPE default 'BCOL'
      !TITLE type BAPISIGNAT-PROP_VALUE default ''
      !COLOR type C default ABAP_TRUE
    changing
      !IMAGE type W3MIMETABTYPE
      !DOCID type STXBITMAPS-DOCID optional .
  methods CONVERT_JPEG_TO_BMP
    importing
      !IMAGE_BIN_TABLE type CPT_X255 optional
      !IMAGE_BIN_STR type XSTRING optional
    returning
      value(BMP) type W3MIMETABTYPE .
  methods CHECK_GRAPHIC_EXIST
    importing
      !OBJECT type TDOBJECT default ZCL_IMAGE_HELPER=>OBJECT_REPOSITORY
      !ID type TDID default ZCL_IMAGE_HELPER=>ID_REPOSITORY-GENERAL
      !NAME type TDOBNAME
    returning
      value(VALUE) type ABAP_BOOL .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS BDS_MIMETYPE TYPE BDS_MIMETP VALUE 'application/octet-stream' ##NO_TEXT.
    CONSTANTS BDS_CLASSNAME TYPE SBDST_CLASSNAME VALUE 'DEVC_STXD_BITMAP' ##NO_TEXT.
    CONSTANTS BDS_CLASSTYPE TYPE SBDST_CLASSTYPE VALUE 'OT' ##NO_TEXT.
    DATA IMAGE_STRING TYPE XSTRING .

    METHODS ENCODE_PHOTO
      IMPORTING
        !IMAGE        TYPE CPT_X255
      RETURNING
        VALUE(RESULT) TYPE STRING .
ENDCLASS.



CLASS ZCL_IMAGE_HELPER IMPLEMENTATION.


  METHOD CHECK_GRAPHIC_EXIST.
    SELECT SINGLE *
      FROM STXL
      INTO @DATA(_GRAPHIC)
     WHERE TDOBJECT = @OBJECT
       AND TDID     = @ID
       AND TDNAME   = @NAME
       AND TDSPRAS  = @SY-LANGU(1).

    IF SY-SUBRC IS INITIAL.
      VALUE = ABAP_TRUE.
    ELSE.
      VALUE = ABAP_FALSE.
    ENDIF.
  ENDMETHOD.


  METHOD convert_jpeg_to_bmp.
    DATA image_data      TYPE w3mimetabtype.
    DATA binary_data     TYPE xstring.
    DATA image_converter TYPE REF TO cl_igs_image_converter.

    IF image_bin_str IS INITIAL.
      LOOP AT image_bin_table INTO DATA(_image_bin).
        binary_data = binary_data && _image_bin.
      ENDLOOP.
    ELSE.
      binary_data = image_bin_str.
    ENDIF.

*    CALL FUNCTION 'SSFC_BASE64_DECODE'
*      EXPORTING
*        B64DATA = IMAGE
*      IMPORTING
*        BINDATA = BINARY_DATA.

    CREATE OBJECT image_converter
      EXPORTING
        destination = 'IGS_RFC_DEST'.

    DATA(graphic_size) = xstrlen( binary_data ).
    CHECK graphic_size > 0.

    DATA(graphic_conv) = graphic_size.
    DATA(graphic_offs) = 0.

    WHILE graphic_conv > 255.
      APPEND VALUE #( line = binary_data+graphic_offs(255) ) TO image_data.

      graphic_offs = graphic_offs + 255.
      graphic_conv = graphic_conv - 255.
    ENDWHILE.

    APPEND VALUE #( line = binary_data+graphic_offs(graphic_conv) )
            TO image_data.

    CALL METHOD image_converter->set_image
      EXPORTING
        blob      = image_data
        blob_size = 0.

    image_converter->input  = 'image/jpeg'.
    image_converter->output = 'image/x-ms-bmp'.

    CALL METHOD image_converter->execute
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc IS INITIAL.
      CALL METHOD image_converter->get_image
        IMPORTING
          blob      = bmp
          blob_size = DATA(_size)
          blob_type = DATA(_type).
    ENDIF.
  ENDMETHOD.


  METHOD CREATE_INTERNAL_URL.
    DATA BINARY_DATA TYPE XSTRING.
    DATA STR_TABLE   TYPE STANDARD TABLE OF X255.

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        B64DATA = IMAGE_DATA
      IMPORTING
        BINDATA = BINARY_DATA.

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
    ENDIF.
  ENDMETHOD.


  METHOD CREATE_INTERNAL_URL2.
    DATA STR_TABLE   TYPE STANDARD TABLE OF X255.

    DATA(GRAPHIC_SIZE) = XSTRLEN( IMAGE_DATA ).
    CHECK GRAPHIC_SIZE > 0.

    DATA(GRAPHIC_CONV) = GRAPHIC_SIZE.
    DATA(GRAPHIC_OFFS) = 0.

    WHILE GRAPHIC_CONV > 255.
      APPEND IMAGE_DATA+GRAPHIC_OFFS(255) TO STR_TABLE.

      GRAPHIC_OFFS = GRAPHIC_OFFS + 255.
      GRAPHIC_CONV = GRAPHIC_CONV - 255.
    ENDWHILE.

    APPEND IMAGE_DATA+GRAPHIC_OFFS(GRAPHIC_CONV)
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
    ENDIF.
  ENDMETHOD.


  METHOD DISPLAY.
    IF CUSTOM_INSTANCE IS INITIAL.
      CREATE OBJECT CUSTOM_INSTANCE
        EXPORTING
          CONTAINER_NAME = CUSTOM_NAME.

      PICTURE_INSTANCE = NEW #( PARENT = CUSTOM_INSTANCE ).
    ENDIF.

    IF URL IS INITIAL.
      PICTURE_INSTANCE->CLEAR_PICTURE( ).
    ELSE.
      PICTURE_INSTANCE->LOAD_PICTURE_FROM_URL(
        URL = URL ).
    ENDIF.

    PICTURE_INSTANCE->SET_DISPLAY_MODE(
      DISPLAY_MODE = CL_GUI_PICTURE=>DISPLAY_MODE_FIT_CENTER ).
  ENDMETHOD.


  METHOD ENCODE_PHOTO.
    DATA IMAGE_X TYPE XSTRING.

    LOOP AT IMAGE INTO DATA(_IMAGE_BIN).
      IMAGE_X = IMAGE_X && _IMAGE_BIN.
    ENDLOOP.

    DATA STRBIN TYPE STRING.

    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        BINDATA = IMAGE_X
      IMPORTING
        B64DATA = RESULT
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
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.


  METHOD GET_PHOTO.
    DATA IMAGE_DATA TYPE TABLE OF TLINE.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        CLIENT                  = SY-MANDT
        ID                      = ID
        LANGUAGE                = SY-LANGU
        NAME                    = NAME
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
      RESULT = RESULT && _IMG_DATA-TDLINE.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_PHOTO_URL.
    RESULT = ME->CREATE_INTERNAL_URL( ME->GET_PHOTO( OBJECT = OBJECT ID = ID NAME = NAME ) ).
  ENDMETHOD.


  METHOD SAVE_BITMAP_BDS.
    DATA OBJECT_KEY    TYPE SBDST_OBJECT_KEY.
    DATA TAB           TYPE DDOBJNAME.
    DATA IMAGE_SIZE    TYPE I.
    DATA BDS_OBJECT    TYPE REF TO CL_BDS_DOCUMENT_SET.
    DATA BDS_CONTENT   TYPE SBDST_CONTENT.
    DATA DPI           TYPE  STXBITMAPS-RESOLUTION.
    DATA BDS_SIGNATURE TYPE SBDST_SIGNATURE.
    DATA WA_STXBITMAPS TYPE STXBITMAPS.

    DATA WIDTH_TW      TYPE  STXBITMAPS-WIDTHTW.
    DATA HEIGHT_TW     TYPE  STXBITMAPS-HEIGHTTW.
    DATA WIDTH_PIX     TYPE  STXBITMAPS-WIDTHPIX.
    DATA HEIGHT_PIX    TYPE  STXBITMAPS-HEIGHTPIX.
    DATA BDS_BYTECOUNT TYPE  I.

    CALL FUNCTION 'SAPSCRIPT_CONVERT_BITMAP_BDS'
      EXPORTING
        COLOR                     = COLOR
        FORMAT                    = 'BMP'
        BITMAP_BYTECOUNT          = IMAGE_SIZE
        COMPRESS_BITMAP           = 'X'
      IMPORTING
        WIDTH_TW                  = WIDTH_TW
        HEIGHT_TW                 = HEIGHT_TW
        WIDTH_PIX                 = WIDTH_PIX
        HEIGHT_PIX                = HEIGHT_PIX
        DPI                       = DPI
        BDS_BYTECOUNT             = BDS_BYTECOUNT
      TABLES
        BITMAP_FILE               = IMAGE
        BITMAP_FILE_BDS           = BDS_CONTENT
      EXCEPTIONS
        FORMAT_NOT_SUPPORTED      = 1
        NO_BMP_FILE               = 2
        BMPERR_INVALID_FORMAT     = 3
        BMPERR_NO_COLORTABLE      = 4
        BMPERR_UNSUP_COMPRESSION  = 5
        BMPERR_CORRUPT_RLE_DATA   = 6
        TIFFERR_INVALID_FORMAT    = 7
        TIFFERR_NO_COLORTABLE     = 8
        TIFFERR_UNSUP_COMPRESSION = 9
        BMPERR_EOF                = 10
        OTHERS                    = 11.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    SELECT SINGLE DOCID INTO DOCID
      FROM STXBITMAPS
      WHERE TDOBJECT  = OBJECT
        AND   TDNAME  = NAME
        AND   TDID    = ID
        AND   TDBTYPE = TYPE.

    IF SY-SUBRC NE 0.
      CLEAR DOCID.
    ENDIF.

    CREATE OBJECT BDS_OBJECT.

    DATA(_BDS_COMPONENTS) =
      VALUE SBDST_COMPONENTS( ( DOC_COUNT  = '1'
                                COMP_COUNT = '1'
                                MIMETYPE   = BDS_MIMETYPE
                                COMP_SIZE  = BDS_BYTECOUNT
                               )
                            ).

    IF ( DOCID IS INITIAL ).
      APPEND VALUE #( DOC_COUNT = '1' ) TO BDS_SIGNATURE.

      CALL METHOD BDS_OBJECT->CREATE_WITH_TABLE
        EXPORTING
          CLASSNAME  = BDS_CLASSNAME
          CLASSTYPE  = BDS_CLASSTYPE
          COMPONENTS = _BDS_COMPONENTS
          CONTENT    = BDS_CONTENT
        CHANGING
          SIGNATURE  = BDS_SIGNATURE
          OBJECT_KEY = OBJECT_KEY
        EXCEPTIONS
          OTHERS     = 1.

      TRY.
          DOCID = BDS_SIGNATURE[ 1 ]-DOC_ID.
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
      ENDTRY.

    ELSE.

      "//Read object_key for faster access
      CLEAR OBJECT_KEY.

      SELECT SINGLE *
        FROM STXBITMAPS INTO WA_STXBITMAPS
       WHERE TDOBJECT = OBJECT
         AND TDID     = ID
         AND TDNAME   = NAME
         AND TDBTYPE  = TYPE.

      SELECT SINGLE TABNAME
        FROM BDS_LOCL INTO TAB
       WHERE CLASSNAME = BDS_CLASSNAME
         AND CLASSTYPE = BDS_CLASSTYPE.

      IF SY-SUBRC = 0.
        SELECT SINGLE OBJECT_KEY
          FROM (TAB)
          INTO OBJECT_KEY
         WHERE LOIO_ID = WA_STXBITMAPS-DOCID+10(32)
           AND CLASSNAME = BDS_CLASSNAME
           AND CLASSTYPE = BDS_CLASSTYPE.
      ENDIF.

      "//Read object key
      CALL METHOD BDS_OBJECT->UPDATE_WITH_TABLE
        EXPORTING
          CLASSNAME     = BDS_CLASSNAME
          CLASSTYPE     = BDS_CLASSTYPE
          OBJECT_KEY    = OBJECT_KEY
          DOC_ID        = DOCID
          DOC_VER_NO    = '1'
          DOC_VAR_ID    = '1'
        CHANGING
          COMPONENTS    = _BDS_COMPONENTS
          CONTENT       = BDS_CONTENT
        EXCEPTIONS
          NOTHING_FOUND = 1
          OTHERS        = 2.

      IF SY-SUBRC = 1. "Inconsistency STXBITMAPS - BDS; repeat check in
        APPEND VALUE #( DOC_COUNT = '1' ) TO BDS_SIGNATURE.

        CALL METHOD BDS_OBJECT->CREATE_WITH_TABLE
          EXPORTING
            CLASSNAME  = BDS_CLASSNAME
            CLASSTYPE  = BDS_CLASSTYPE
            COMPONENTS = _BDS_COMPONENTS
            CONTENT    = BDS_CONTENT
          CHANGING
            SIGNATURE  = BDS_SIGNATURE
            OBJECT_KEY = OBJECT_KEY
          EXCEPTIONS
            OTHERS     = 1.

        TRY.
            DOCID = BDS_SIGNATURE[ 1 ]-DOC_ID.
          CATCH CX_SY_ITAB_LINE_NOT_FOUND.
        ENDTRY.
      ENDIF.
    ENDIF.

    "//Save bitmap header in STXBITPMAPS

    WA_STXBITMAPS-TDNAME     = NAME.
    WA_STXBITMAPS-TDOBJECT   = OBJECT.
    WA_STXBITMAPS-TDID       = ID.
    WA_STXBITMAPS-TDBTYPE    = TYPE.
    WA_STXBITMAPS-DOCID      = DOCID.
    WA_STXBITMAPS-WIDTHPIX   = WIDTH_PIX.
    WA_STXBITMAPS-HEIGHTPIX  = HEIGHT_PIX.
    WA_STXBITMAPS-WIDTHTW    = WIDTH_TW.
    WA_STXBITMAPS-HEIGHTTW   = HEIGHT_TW.
    WA_STXBITMAPS-RESOLUTION = DPI.

    INSERT INTO STXBITMAPS VALUES WA_STXBITMAPS.

    IF SY-SUBRC <> 0.
      UPDATE STXBITMAPS FROM WA_STXBITMAPS.
    ENDIF.

    DATA(_BDS_PROPERTIES) =
      VALUE SBDST_PROPERTIES( ( PROP_NAME = 'DESCRIPTION' PROP_VALUE = TITLE ) ).

    CALL METHOD BDS_OBJECT->CHANGE_PROPERTIES
      EXPORTING
        CLASSNAME  = BDS_CLASSNAME
        CLASSTYPE  = BDS_CLASSTYPE
        OBJECT_KEY = OBJECT_KEY
        DOC_ID     = DOCID
        DOC_VER_NO = '1'
        DOC_VAR_ID = '1'
      CHANGING
        PROPERTIES = _BDS_PROPERTIES
      EXCEPTIONS
        OTHERS     = 1.
  ENDMETHOD.


  METHOD SAVE_PHOTO.
    DATA IMG_CONTENT  TYPE TABLE OF TLINE.
    DATA STRING_TABLE TYPE STANDARD TABLE OF TDLINE.
    DATA IMAGE_STR    TYPE STRING.

    "//Encode image to make the process faster
    DATA(_ENCODED_STRING) = ME->ENCODE_PHOTO( IMAGE ).

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        I_STRING         = _ENCODED_STRING
        I_TABLINE_LENGTH = 132
      TABLES
        ET_TABLE         = STRING_TABLE.

    LOOP AT STRING_TABLE INTO DATA(_STR_TABLE).
      APPEND VALUE #( TDLINE = _STR_TABLE ) TO IMG_CONTENT.
    ENDLOOP.

    DATA(OBJ_DATA) =
        VALUE THEAD(  TDOBJECT = OBJECT
                      TDID     = ID
                      TDNAME   = NAME
                      TDSPRAS  = SY-LANGU ).

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        CLIENT   = SY-MANDT
        HEADER   = OBJ_DATA
      TABLES
        LINES    = IMG_CONTENT
      EXCEPTIONS
        ID       = 1
        LANGUAGE = 2
        NAME     = 3
        OBJECT   = 4
        OTHERS   = 5.
  ENDMETHOD.
ENDCLASS.
