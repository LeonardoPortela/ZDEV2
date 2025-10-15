class ZCL_SEND_EMAIL definition
  public
  create public .

public section.

  methods CONSTRUCTOR .
  methods SEND
    importing
      !I_RECEIVERS type RVARI_VNAM
      !I_DOC_TYPE type SO_OBJ_TP default 'HTM'
      !I_SUBJECT type STRING
      !I_BODY type HTML_TABLE .
  class-methods SEND_STATIC
    importing
      !I_RECEIVERS type RVARI_VNAM
      !I_DOC_TYPE type SO_OBJ_TP default 'HTM'
      !I_SUBJECT type STRING
      !I_BODY type HTML_TABLE .
protected section.
private section.

  data AT_RECEIVERS type SOMLRECI1_T .
  data AS_DOC_DAT type SODOCCHGI1 .
  data AT_LIST type SOPCKLSTI1_T .
  data AT_CONTENTS_TXT type HTML_TABLE .
ENDCLASS.



CLASS ZCL_SEND_EMAIL IMPLEMENTATION.


  method CONSTRUCTOR.
  endmethod.


  METHOD send.

    DATA lt_email TYPE TABLE OF msgtx.

    DATA lt_stvarv TYPE	tvarvc_t.

    CLEAR: at_receivers, as_doc_dat, at_list, at_contents_txt.

    SELECT * FROM tvarvc
      INTO TABLE lt_stvarv
        WHERE name = i_receivers.

    LOOP AT lt_stvarv ASSIGNING FIELD-SYMBOL(<fs_stvar>).

      APPEND INITIAL LINE TO at_receivers ASSIGNING FIELD-SYMBOL(<fs_rec>).

      <fs_rec>-receiver = <fs_stvar>-low.
      <fs_rec>-rec_type = 'U'.

    ENDLOOP.

    APPEND INITIAL LINE TO at_list ASSIGNING FIELD-SYMBOL(<fs_list>).

    <fs_list>-head_start = 1.
    <fs_list>-head_num = 0.
    <fs_list>-body_start = 1.
    <fs_list>-body_num = 99999.
    <fs_list>-doc_type = i_doc_type.

    as_doc_dat-obj_name = i_subject.
    as_doc_dat-obj_descr = i_subject.
    as_doc_dat-no_change = 'X'.

    at_contents_txt = i_body.

    "Enviar
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = as_doc_dat
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = at_list
        contents_txt               = at_contents_txt
        receivers                  = at_receivers
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        operation_no_authorization = 4
        OTHERS                     = 99.

    IF sy-subrc NE 0.

    ENDIF.

  ENDMETHOD.


  METHOD send_static.

    DATA lo_email TYPE REF TO zcl_send_email.

    lo_email = NEW zcl_send_email( ).

    CALL METHOD lo_email->send
      EXPORTING
        i_receivers = i_receivers
        i_doc_type  = i_doc_type
        i_subject   = i_subject
        i_body      = i_body.

  ENDMETHOD.
ENDCLASS.
