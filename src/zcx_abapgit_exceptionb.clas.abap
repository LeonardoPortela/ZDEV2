class ZCX_ABAPGIT_EXCEPTIONB definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF c_section_text,
        cause           TYPE string VALUE `Cause`,
        system_response TYPE string VALUE `System response`,
        what_to_do      TYPE string VALUE `Procedure`,
        sys_admin       TYPE string VALUE `System administration`,
      END OF c_section_text .
  constants:
    BEGIN OF c_section_token,
        cause           TYPE string VALUE `&CAUSE&`,
        system_response TYPE string VALUE `&SYSTEM_RESPONSE&`,
        what_to_do      TYPE string VALUE `&WHAT_TO_DO&`,
        sys_admin       TYPE string VALUE `&SYS_ADMIN&`,
      END OF c_section_token .
  data MSGV1 type SYMSGV read-only .
  data MSGV2 type SYMSGV read-only .
  data MSGV3 type SYMSGV read-only .
  data MSGV4 type SYMSGV read-only .
  data MV_LONGTEXT type STRING read-only .
  data MT_CALLSTACK type ABAP_CALLSTACK read-only .
  data MI_LOG type ref to ZIF_ABAPGIT_LOG read-only .

    "! Raise exception with text
    "! @parameter iv_text | Text
    "! @parameter ix_previous | Previous exception
    "! @parameter ii_log | Log
    "! @parameter iv_longtext | Longtext
    "! @raising zcx_abapgit_exception | Exception
  class-methods RAISE
    importing
      !IV_TEXT type CLIKE
      !IX_PREVIOUS type ref to CX_ROOT optional
      !II_LOG type ref to ZIF_ABAPGIT_LOG optional
      !IV_LONGTEXT type CSEQUENCE optional
    raising
      ZCX_ABAPGIT_EXCEPTION .
    "! Raise exception with T100 message
    "! <p>
    "! Will default to sy-msg* variables. These need to be set right before calling this method.
    "! </p>
    "! @parameter iv_msgid | Message ID
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @parameter ii_log | Log
    "! @parameter ix_previous | Previous exception
    "! @parameter iv_longtext | Longtext
    "! @raising zcx_abapgit_exception | Exception
  class-methods RAISE_T100
    importing
      value(IV_MSGID) type SYMSGID default SY-MSGID
      value(IV_MSGNO) type SYMSGNO default SY-MSGNO
      value(IV_MSGV1) type SYMSGV default SY-MSGV1
      value(IV_MSGV2) type SYMSGV default SY-MSGV2
      value(IV_MSGV3) type SYMSGV default SY-MSGV3
      value(IV_MSGV4) type SYMSGV default SY-MSGV4
      !II_LOG type ref to ZIF_ABAPGIT_LOG optional
      !IX_PREVIOUS type ref to CX_ROOT optional
      !IV_LONGTEXT type CSEQUENCE optional
    raising
      ZCX_ABAPGIT_EXCEPTION .
    "! Raise with text from previous exception
    "! @parameter ix_previous | Previous exception
    "! @parameter iv_longtext | Longtext
    "! @raising zcx_abapgit_exception | Exception
  class-methods RAISE_WITH_TEXT
    importing
      !IX_PREVIOUS type ref to CX_ROOT
      !IV_LONGTEXT type CSEQUENCE optional
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional
      !MV_LONGTEXT type STRING optional
      !MT_CALLSTACK type ABAP_CALLSTACK optional
      !MI_LOG type ref to ZIF_ABAPGIT_LOG optional .

  methods GET_SOURCE_POSITION
    redefinition .
  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
protected section.
private section.

  constants C_GENERIC_ERROR_MSG type STRING value `An error occured (ZCX_ABAPGIT_EXCEPTION)` ##NO_TEXT.

  class-methods SPLIT_TEXT_TO_SYMSG
    importing
      !IV_TEXT type STRING
    returning
      value(RS_MSG) type SYMSG .
  methods SAVE_CALLSTACK .
  methods ITF_TO_STRING
    importing
      !IT_ITF type TLINE_TAB
    returning
      value(RV_RESULT) type STRING .
  methods GET_T100_LONGTEXT_ITF
    returning
      value(RT_ITF) type TLINE_TAB .
  methods REMOVE_EMPTY_SECTION
    importing
      !IV_TABIX_FROM type I
      !IV_TABIX_TO type I
    changing
      !CT_ITF type TLINE_TAB .
  methods REPLACE_SECTION_HEAD_WITH_TEXT
    changing
      !CS_ITF type TLINE .
  class-methods REMOVE_NEWLINES_FROM_STRING
    importing
      !IV_STRING type STRING
    returning
      value(RV_RESULT) type STRING .
ENDCLASS.



CLASS ZCX_ABAPGIT_EXCEPTIONB IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->MV_LONGTEXT = MV_LONGTEXT .
me->MT_CALLSTACK = MT_CALLSTACK .
me->MI_LOG = MI_LOG .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  method GET_SOURCE_POSITION.
*CALL METHOD SUPER->GET_SOURCE_POSITION
**  IMPORTING
**    program_name =
**    include_name =
**    source_line  =
*    .
  endmethod.


  method GET_T100_LONGTEXT_ITF.
  endmethod.


  method IF_MESSAGE~GET_LONGTEXT.
*CALL METHOD SUPER->IF_MESSAGE~GET_LONGTEXT
**  EXPORTING
**    preserve_newlines =
*  RECEIVING
*    RESULT            =
*    .
  endmethod.


  method ITF_TO_STRING.
  endmethod.


  method RAISE.
  endmethod.


  method RAISE_T100.
  endmethod.


  method RAISE_WITH_TEXT.
  endmethod.


  method REMOVE_EMPTY_SECTION.
  endmethod.


  method REMOVE_NEWLINES_FROM_STRING.
  endmethod.


  method REPLACE_SECTION_HEAD_WITH_TEXT.
  endmethod.


  method SAVE_CALLSTACK.
  endmethod.


  METHOD SPLIT_TEXT_TO_SYMSG.

    CONSTANTS:
      lc_length_of_msgv           TYPE i VALUE 50,
      lc_offset_of_last_character TYPE i VALUE 49.

    DATA:
      lv_text    TYPE c LENGTH 200,
      lv_rest    TYPE c LENGTH 200,
      ls_msg     TYPE symsg,
      lv_msg_var TYPE c LENGTH lc_length_of_msgv,
      lv_index   TYPE sy-index.

    lv_text = iv_text.

    DO 4 TIMES.

      lv_index = sy-index.

      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = lc_length_of_msgv
          text   = lv_text
        IMPORTING
          line   = lv_msg_var
          rest   = lv_rest.

      IF lv_msg_var+lc_offset_of_last_character(1) = space OR
         lv_text+lc_length_of_msgv(1) = space.
        " keep the space at the beginning of the rest
        " because otherwise it's lost
        lv_rest = | { lv_rest }|.
      ENDIF.

      lv_text = lv_rest.

      CASE lv_index.
        WHEN 1.
          ls_msg-msgv1 = lv_msg_var.
        WHEN 2.
          ls_msg-msgv2 = lv_msg_var.
        WHEN 3.
          ls_msg-msgv3 = lv_msg_var.
        WHEN 4.
          ls_msg-msgv4 = lv_msg_var.
      ENDCASE.

    ENDDO.

    " Set syst using generic error message
    MESSAGE e001(00) WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO lv_rest.

    rs_msg = ls_msg.

  ENDMETHOD.
ENDCLASS.
