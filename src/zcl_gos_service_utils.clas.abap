class ZCL_GOS_SERVICE_UTILS definition
  public
  final
  create public .

public section.

  class-methods GET_ANEXOS_OBJETO
    importing
      !I_CLASS_NAME type BAPIBDS01-CLASSNAME
      !I_OBJ_KEY type SWOTOBJID-OBJKEY
    returning
      value(R_CONNECTIONS) type ZBDN_CON_T .
  class-methods GET_XSTRING_ANEXO
    importing
      !I_DOCUMENT_ID type SOFOLENTI1-DOC_ID
    returning
      value(R_XSTRING) type XSTRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GOS_SERVICE_UTILS IMPLEMENTATION.


  METHOD get_anexos_objeto.

    CLEAR: r_connections[].

    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
      EXPORTING
        classname          = i_class_name
        objkey             = i_obj_key
      TABLES
        gos_connections    = r_connections
      EXCEPTIONS
        no_objects_found   = 1
        internal_error     = 2
        internal_gos_error = 3
        OTHERS             = 4.

  ENDMETHOD.


  METHOD get_xstring_anexo.

    DATA: ls_document_data   TYPE sofolenti1,
          lt_object_header   TYPE TABLE OF solisti1,
          lt_object_content  TYPE TABLE OF solisti1,
          lt_object_para     TYPE TABLE OF soparai1,
          lt_object_parb     TYPE TABLE OF soparbi1,
          lt_attachment_list TYPE TABLE OF soattlsti1,
          lt_receiver_list   TYPE TABLE OF soreclsti1,
          lt_contents_hex    TYPE TABLE OF solix,
          lv_document_id     TYPE so_entryid.

    CLEAR: r_xstring.

    lv_document_id = i_document_id.

    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
      EXPORTING
        document_id                = i_document_id
      IMPORTING
        document_data              = ls_document_data
      TABLES
        object_header              = lt_object_header
        object_content             = lt_object_content
        object_para                = lt_object_para
        object_parb                = lt_object_parb
        attachment_list            = lt_attachment_list
        receiver_list              = lt_receiver_list
        contents_hex               = lt_contents_hex
      EXCEPTIONS
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        OTHERS                     = 4.


    CHECK sy-subrc EQ 0.

    CHECK lt_contents_hex[] IS NOT INITIAL.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = CONV i( lines( lt_contents_hex ) * 255 ) " Tamanho total dos dados
      IMPORTING
        buffer       = r_xstring                          " Resultado em xstring
      TABLES
        binary_tab   = lt_contents_hex                        " Tabela de entrada
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

  ENDMETHOD.
ENDCLASS.
