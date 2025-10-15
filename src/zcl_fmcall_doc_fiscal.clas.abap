class ZCL_FMCALL_DOC_FISCAL definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools JS .

*"* public components of class ZCL_FMCALL_DOC_FISCAL
*"* do not include other source files here!!!
  interfaces IF_HTTP_EXTENSION .

  constants XNL type ABAP_CHAR1 value %_NEWLINE ##NO_TEXT.
  constants XCRLF type ABAP_CR_LF value %_CR_LF ##NO_TEXT.
  data MY_SERVICE type STRING .
  data MY_URL type STRING .

  class-methods ABAP2JSON
    importing
      !ABAP_DATA type DATA
      !NAME type STRING optional
      !UPCASE type XFELD optional
      !CAMELCASE type XFELD optional
    returning
      value(JSON_STRING) type STRING
    exceptions
      ERROR_IN_DATA_DESCRIPTION .
  class-methods ABAP2PDF
    importing
      !ABAP_DATA type DATA
      !NAME type STRING optional
    returning
      value(PDF_XSTRING) type XSTRING
    exceptions
      ERROR_IN_DATA_DESCRIPTION .
  class-methods ABAP2PERL
    importing
      !ABAP_DATA type DATA
      !NAME type STRING optional
      !UPCASE type XFELD optional
    returning
      value(PERL_STRING) type STRING
    exceptions
      ERROR_IN_DATA_DESCRIPTION .
  class-methods ABAP2XML
    importing
      !ABAP_DATA type DATA
      !NAME type STRING optional
      !WITH_XML_HEADER type ABAP_BOOL default ABAP_FALSE
      !UPCASE type XFELD optional
      !NAME_ATR type STRING optional
    returning
      value(XML_STRING) type STRING .
  class-methods ABAP2YAML
    importing
      !ABAP_DATA type DATA
      !NAME type STRING optional
      !UPCASE type XFELD optional
      !Y_LEVEL type I default 0
      !S_INDEX type I default 0
      !FIRST_ROW type XFELD optional
      !DONT_INDENT type XFELD optional
    returning
      value(YAML_STRING) type STRING
    exceptions
      ERROR_IN_DATA_DESCRIPTION .
  class-methods BUILD_PARAMS
    importing
      !FUNCTION_NAME type RS38L_FNAM
    exporting
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB
      !PARAMS type ANY
    exceptions
      INVALID_FUNCTION
      UNSUPPORTED_PARAM_TYPE .
  class-methods JSON2ABAP
    importing
      !JSON_STRING type STRING optional
      !VAR_NAME type STRING optional
      !PROPERTY_PATH type STRING default 'json_obj'
    exporting
      value(PROPERTY_TABLE) type JS_PROPERTY_TAB
    changing
      !JS_OBJECT type ref to CL_JAVA_SCRIPT optional
      value(ABAP_DATA) type ANY optional
    raising
      ZCX_JSON .
  class-methods JSON_DESERIALIZE
    importing
      !JSON type STRING
    changing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
    raising
      ZCX_JSON .
  methods NOTES
    returning
      value(TEXT) type STRING .
  class-methods SERIALIZE_JSON
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !CAMELCASE type ABAP_BOOL default ABAP_FALSE
    exporting
      !O_STRING type STRING .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_FMCALL_APP_MOBILE .
  class-methods GERA_ERRO_GERAL_SYS
    raising
      ZCX_FMCALL_APP_MOBILE .
  class-methods SERIALIZE_PDF
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !CAMELCASE type ABAP_BOOL default ABAP_FALSE
    exporting
      !O_XSTRING type XSTRING
      !O_NAME type STRING .
  class-methods SERIALIZE_XML_DOC
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !CAMELCASE type ABAP_BOOL default ABAP_FALSE
    exporting
      !O_STRING type STRING
      !O_XSTRING type XSTRING
      !O_NAME type STRING .
  class-methods SERIALIZE_PERL
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !FUNCNAME type RS38L_FNAM
    exporting
      !PERL_STRING type STRING .
  class-methods SERIALIZE_XML
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !FUNCNAME type RS38L_FNAM
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !FORMAT type STRING optional
    exporting
      !O_STRING type STRING .
  class-methods SERIALIZE_YAML
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
    exporting
      !YAML_STRING type STRING .
  class-methods DESERIALIZE_ID
    importing
      !JSON type STRING
    changing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
    raising
      ZCX_JSON .
  class-methods SERIALIZE_ID
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !FORMAT type STRING default 'JSON'
      !FUNCNAME type RS38L_FNAM optional
      !CAMELCASE type ABAP_BOOL default ABAP_FALSE
    exporting
      !O_STRING type STRING
    raising
      ZCX_JSON .
  class-methods CONVERT_TO_UTF8
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO) type STRING .
  class-methods LOGIN
    importing
      value(I_SERVER) type ref to IF_HTTP_SERVER
    exporting
      !E_OUT_LOGIN type ZDE_OUT_LOGIN_APP_MOBILE_OUT
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZCL_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  class-methods LOGOFF
    importing
      value(I_SERVER) type ref to IF_HTTP_SERVER
    returning
      value(R_INSTANCE) type ref to ZCL_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  class-methods VALIDA_TOKEN
    importing
      value(I_SERVER) type ref to IF_HTTP_SERVER
    exporting
      !E_OUT_LOGIN type ZDE_OUT_LOGIN_APP_MOBILE_OUT
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZCL_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  class-methods CONVERT_TXT_JSON_TO_STRING
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO) type STRING .
  class-methods GET_HTML_ERRO
    importing
      !I_STATUS type STRING
      !I_MENSSAGEM type STRING
    returning
      value(R_HTML) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FMCALL_DOC_FISCAL IMPLEMENTATION.


METHOD ABAP2JSON.
*/**********************************************/*
*/ This method takes any ABAP data variable and /*
*/ returns a string representing its value in   /*
*/ JSON format.                                 /*
*/ ABAP references are always de-referenced and /*
*/ treated as normal variables.                 /*
*/**********************************************/*

  TYPE-POOLS: ABAP.

  CONSTANTS:
    C_COMMA TYPE C VALUE ',',
    C_COLON TYPE C VALUE ':',
    C_QUOTE TYPE C VALUE '"'.

  DATA:
    DONT_QUOTE      TYPE XFELD,
    JSON_FRAGMENTS  TYPE TABLE OF STRING,
    REC_JSON_STRING TYPE STRING,
    L_TYPE          TYPE C,
    S_TYPE          TYPE C,
    L_COMPS         TYPE I,
    L_LINES         TYPE I,
    L_INDEX         TYPE I,
    L_VALUE         TYPE STRING,
    L_NAME          TYPE STRING,
    L_STRUDESCR     TYPE REF TO CL_ABAP_STRUCTDESCR.

  FIELD-SYMBOLS:
    <ABAP_DATA> TYPE ANY,
    <ITAB>      TYPE ANY TABLE,
    <STRU>      TYPE ANY TABLE,
    <COMP>      TYPE ANY,
    <ABAPCOMP>  TYPE ABAP_COMPDESCR.

  DEFINE GET_SCALAR_VALUE.
    " &1 : assigned var
    " &2 : abap data
    " &3 : abap type
    &1 = &2.
****************************************************
* Adapt some basic ABAP types (pending inclusion of all basic abap types?)
* Feel free to customize this for your needs
    case &3.
*       1. ABAP numeric types
      when 'I'. " Integer
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.
        dont_quote = 'X'.

      when 'F'. " Float
        condense &1.
        dont_quote = 'X'.

      when 'P'. " Packed number (used in quantities or currency, for example)
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.
        dont_quote = 'X'.

      when 'X'. " Hexadecimal
        condense &1.
        concatenate '0x' &1 into &1.
*        dont_quote = 'X'.
*        "Quote it, as JSON doesn't support Hex or Octal as native types.

*       2. ABAP char types
      when 'D'. " Date type
        CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2) INTO &1.

      when 'T'. " Time representation
        CONCATENATE &1(2) ':' &1+2(2) ':' &1+4(2) INTO &1.

      when 'N'. " Numeric text field
*           condense &1.

      when 'C' or 'g'. " Char sequences and Strings
* Put safe chars
        replace all occurrences of '\' in &1 with '\\' .
        replace all occurrences of '"' in &1 with '\"' .
        replace all occurrences of cl_abap_char_utilities=>cr_lf in &1 with '\r\n' .
        replace all occurrences of cl_abap_char_utilities=>newline in &1 with '\n' .
        replace all occurrences of cl_abap_char_utilities=>horizontal_tab in &1 with '\t' .
        replace all occurrences of cl_abap_char_utilities=>backspace in &1 with '\b' .
        replace all occurrences of cl_abap_char_utilities=>form_feed in &1 with '\f' .

      when 'y'.  " XSTRING
* Put the XSTRING in Base64
        &1 = cl_http_utility=>ENCODE_X_BASE64( &2 ).

      when others.
* Don't hesitate to add and modify scalar abap types to suit your taste.

    endcase.
** End of scalar data preparing.

* Enclose value in quotes (or not)
    if dont_quote ne 'X'.
      concatenate c_quote &1 c_quote into &1.
    endif.
    clear dont_quote.
  END-OF-DEFINITION.

***************************************************
*  Prepare field names, JSON does quote names!!   *
*  You must be strict in what you produce.        *
***************************************************
  IF NAME IS NOT INITIAL.
    CONCATENATE C_QUOTE NAME C_QUOTE C_COLON INTO REC_JSON_STRING.
    APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
    CLEAR REC_JSON_STRING.
  ENDIF.

**
* Get ABAP data type
  DESCRIBE FIELD ABAP_DATA TYPE L_TYPE COMPONENTS L_COMPS.

***************************************************
*  Get rid of data references
***************************************************
  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF.
    ASSIGN ABAP_DATA->* TO <ABAP_DATA>.
    IF SY-SUBRC NE 0.
      APPEND '{}' TO JSON_FRAGMENTS.
      CONCATENATE LINES OF JSON_FRAGMENTS INTO JSON_STRING.
      EXIT.
    ENDIF.
  ELSE.
    ASSIGN ABAP_DATA TO <ABAP_DATA>.
  ENDIF.

* Get ABAP data type again and start
  DESCRIBE FIELD <ABAP_DATA> TYPE L_TYPE COMPONENTS L_COMPS.

***************************************************
*  Tables
***************************************************
  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE.
* '[' JSON table opening bracket
    APPEND '[' TO JSON_FRAGMENTS.
    ASSIGN <ABAP_DATA> TO <ITAB>.
    L_LINES = LINES( <ITAB> ).
    LOOP AT <ITAB> ASSIGNING <COMP>.
      ADD 1 TO L_INDEX.
*> Recursive call for each table row:
      REC_JSON_STRING = ABAP2JSON( ABAP_DATA = <COMP> UPCASE = UPCASE CAMELCASE = CAMELCASE ).
      APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
      CLEAR REC_JSON_STRING.
      IF L_INDEX < L_LINES.
        APPEND C_COMMA TO JSON_FRAGMENTS.
      ENDIF.
    ENDLOOP.
    APPEND ']' TO JSON_FRAGMENTS.
* ']' JSON table closing bracket


***************************************************
*  Structures
***************************************************
  ELSE.
    IF L_COMPS IS NOT INITIAL.
* '{' JSON object opening curly brace
      APPEND '{' TO JSON_FRAGMENTS.
      L_STRUDESCR ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( <ABAP_DATA> ).
      LOOP AT L_STRUDESCR->COMPONENTS ASSIGNING <ABAPCOMP>.
        L_INDEX = SY-TABIX .
        ASSIGN COMPONENT <ABAPCOMP>-NAME OF STRUCTURE <ABAP_DATA> TO <COMP>.
        L_NAME = <ABAPCOMP>-NAME.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
        IF UPCASE NE 'X'.
          " translate l_name to lower case.
          L_NAME = TO_LOWER( L_NAME ).
        ENDIF.
        IF CAMELCASE EQ 'X'.
          L_NAME = TO_MIXED( VAL = L_NAME  CASE = 'a' ).
        ENDIF.
        DESCRIBE FIELD <COMP> TYPE S_TYPE.
        IF S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF OR
           S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT1 OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT2.
*> Recursive call for non-scalars:
          REC_JSON_STRING = ABAP2JSON( ABAP_DATA = <COMP> NAME = L_NAME UPCASE = UPCASE CAMELCASE = CAMELCASE ).
        ELSE.
          IF S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_OREF OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_IREF.
            REC_JSON_STRING = '"REF UNSUPPORTED"'.
          ELSE.
            GET_SCALAR_VALUE REC_JSON_STRING <COMP> S_TYPE.
          ENDIF.
          CONCATENATE C_QUOTE L_NAME C_QUOTE C_COLON REC_JSON_STRING INTO REC_JSON_STRING.
        ENDIF.
        APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
        CLEAR REC_JSON_STRING. CLEAR L_NAME.
        IF L_INDEX < L_COMPS.
          APPEND C_COMMA TO JSON_FRAGMENTS.
        ENDIF.
      ENDLOOP.
      APPEND '}' TO JSON_FRAGMENTS.
* '}' JSON object closing curly brace


****************************************************
*                  - Scalars -                     *
****************************************************
    ELSE.
      GET_SCALAR_VALUE L_VALUE <ABAP_DATA> L_TYPE.
      APPEND L_VALUE TO JSON_FRAGMENTS.

    ENDIF.
* End of structure/scalar IF block.
***********************************

  ENDIF.
* End of main IF block.
**********************

* Use a loop in older releases that don't support concatenate lines.
  CONCATENATE LINES OF JSON_FRAGMENTS INTO JSON_STRING.

ENDMETHOD.


METHOD ABAP2PDF.
*/**********************************************/*
*/ This method takes any ABAP data variable and /*
*/ returns a string representing its value in   /*
*/ JSON format.                                 /*
*/ ABAP references are always de-referenced and /*
*/ treated as normal variables.                 /*
*/**********************************************/*

  TYPE-POOLS: ABAP.

  CONSTANTS:
    C_COMMA TYPE C VALUE ',',
    C_COLON TYPE C VALUE ':',
    C_QUOTE TYPE C VALUE '"'.

  DATA:
    PDF_FRAGMENTS TYPE TABLE OF XSTRING,
    L_TYPE        TYPE C,
    L_COMPS       TYPE I,
    L_VALUE       TYPE XSTRING.

  FIELD-SYMBOLS:
    <ABAP_DATA> TYPE ANY,
    <ITAB>      TYPE ANY TABLE.

* Get ABAP data type
  DESCRIBE FIELD ABAP_DATA TYPE L_TYPE COMPONENTS L_COMPS.

***************************************************
*  Get rid of data references
***************************************************
  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF.
    ASSIGN ABAP_DATA->* TO <ABAP_DATA>.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
  ELSE.
    ASSIGN ABAP_DATA TO <ABAP_DATA>.
  ENDIF.

* Get ABAP data type again and start
  DESCRIBE FIELD <ABAP_DATA> TYPE L_TYPE COMPONENTS L_COMPS.

  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE.

    ASSIGN <ABAP_DATA> TO <ITAB>.
    LOOP AT <ITAB> ASSIGNING FIELD-SYMBOL(<COMP>).
      L_VALUE = <COMP>.
      APPEND L_VALUE TO PDF_FRAGMENTS.
    ENDLOOP.

  ELSEIF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_XSTRING.

    L_VALUE = <ABAP_DATA>.
    PDF_XSTRING = L_VALUE.
    APPEND L_VALUE TO PDF_FRAGMENTS.

  ELSE.

    L_VALUE = <ABAP_DATA>.
    APPEND L_VALUE TO PDF_FRAGMENTS.

  ENDIF.

ENDMETHOD.


method ABAP2PERL.
*/**********************************************/*
*/ This method takes any ABAP data variable and /*
*/ returns a string representing its value in   /*
*/ Perl Data::Dumper format, ready to be evaled /*
*/ in a Perl program.                           /*
*/**********************************************/*

  type-pools: abap.

  constants:
    c_comma type c value ',',
    c_colon type c value ':',
    c_quote type c value ''''.

  data:
    perl_hash_assign type string,
    dont_quote type xfeld,
    perl_fragments type table of string,
    rec_perl_string type string,
    l_type  type c,
    s_type  type c,
    l_comps type i,
    l_lines type i,
    l_index type i,
    l_value type string,
    l_name  type string,
    l_typedescr type ref to cl_abap_structdescr.

  field-symbols:
    <abap_data> type any,
    <itab> type any table,
    <stru> type any table,
    <comp> type any,
    <abapcomp> type abap_compdescr.

  concatenate space '=>' space into perl_hash_assign respecting blanks.

  define get_scalar_value.
    " &1 : assigned var
    " &2 : abap data
    " &3 : abap type
    &1 = &2.
****************************************************
* Adapt some basic ABAP types (pending inclusion of all basic abap types?)
* Feel free to customize this for your needs
    case &3.
*       1. ABAP numeric types
      when 'I'. " Integer
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.
        dont_quote = 'X'.

      when 'F'. " Float
        condense &1.
        dont_quote = 'X'.

      when 'P'. " Packed number (used in quantities, for example)
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.
        dont_quote = 'X'.

      when 'X'. " Hexadecimal
        condense &1.
        concatenate '0x' &1 into &1.
        dont_quote = 'X'.

*       2. ABAP char types
      when 'D'. " Date type
        CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2) INTO &1.

      when 'T'. " Time representation
        CONCATENATE &1(2) ':' &1+2(2) ':' &1+4(2) INTO &1.

      when 'N'. " Numeric text field
*           condense &1.

      when 'C' or 'g'. " Char sequences and Strings
* Put safe chars
        replace all occurrences of '''' in &1 with '\''' .

      when 'y'.  " XSTRING
* Put the XSTRING in Base64
        &1 = cl_http_utility=>ENCODE_X_BASE64( &2 ).

      when others.
* Don't hesitate to add and modify abap types to suit your taste.

    endcase.
** End of scalar data preparing.

* Enclose value in quotes (or not)
    if dont_quote ne 'X'.
      concatenate c_quote &1 c_quote into &1.
    endif.
    clear dont_quote.

  end-of-definition.



***************************************************
*  Prepare field names, we use single quotes.     *
*  You must be strict in what you produce.        *
***************************************************
  if name is not initial.
    concatenate c_quote name c_quote perl_hash_assign into rec_perl_string respecting blanks.
    append rec_perl_string to perl_fragments.
    clear rec_perl_string.
  endif.

**
* Get ABAP data type
  describe field abap_data type l_type components l_comps.

***************************************************
*  Get rid of data references
***************************************************
  if l_type eq cl_abap_typedescr=>typekind_dref.
    assign abap_data->* to <abap_data>.
    if sy-subrc ne 0.
      append '{}' to perl_fragments.
      concatenate lines of perl_fragments into perl_string.
      exit.
    endif.
  else.
    assign abap_data to <abap_data>.
  endif.


* Get ABAP data type again and start
  describe field <abap_data> type l_type components l_comps.

***************************************************
*  Tables
***************************************************
  if l_type eq cl_abap_typedescr=>typekind_table.
* '[' Table opening bracket
    append '[' to perl_fragments.
    assign <abap_data> to <itab>.
    l_lines = lines( <itab> ).
    loop at <itab> assigning <comp>.
      add 1 to l_index.
*> Recursive call here
      rec_perl_string = abap2perl( abap_data = <comp> upcase = upcase ).
      append rec_perl_string to perl_fragments.
      clear rec_perl_string.
      if l_index < l_lines.
        append c_comma to perl_fragments.
      endif.
    endloop.
    append ']' to perl_fragments.
* ']' Table closing bracket


***************************************************
*  Structures
***************************************************
  else .
    if l_comps is not initial.
* '{' Object opening curly brace
      append '{' to perl_fragments .
      l_typedescr ?= cl_abap_typedescr=>describe_by_data( <abap_data> ) .
      loop at l_typedescr->components assigning <abapcomp> .
        l_index = sy-tabix .
        assign component <abapcomp>-name of structure <abap_data> to <comp>.
        l_name = <abapcomp>-name.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
        if upcase ne 'X'.
          translate l_name to lower case.
        endif.
        describe field <comp> type s_type.
        if s_type eq cl_abap_typedescr=>typekind_table or s_type eq cl_abap_typedescr=>typekind_dref or
           s_type eq cl_abap_typedescr=>typekind_struct1 or s_type eq cl_abap_typedescr=>typekind_struct2.
*> Recursive call for non-scalars:
          rec_perl_string = abap2perl( abap_data = <comp> name = l_name upcase = upcase ).
        else.
          if s_type eq cl_abap_typedescr=>TYPEKIND_OREF or s_type eq cl_abap_typedescr=>TYPEKIND_IREF.
            rec_perl_string = '"REF UNSUPPORTED"'.
          else.
            get_scalar_value rec_perl_string <comp> s_type.
          endif.
          concatenate c_quote l_name c_quote perl_hash_assign rec_perl_string into rec_perl_string.
        endif.

        append rec_perl_string to perl_fragments.
        clear rec_perl_string.
        if l_index < l_comps.
          append c_comma to perl_fragments.
        endif.
      endloop.
      append '}' to perl_fragments.
* '}' Object closing curly brace


****************************************************
*                  - Scalars -                     *
****************************************************
    else.

      get_scalar_value l_value <abap_data> l_type.
      append l_value to perl_fragments.

    endif.
* End of structure/scalar IF block.
***********************************


  endif.
* End of main IF block.
**********************


* Use a loop in older releases that don't support concatenate lines.
  concatenate lines of perl_fragments into perl_string.

endmethod.


METHOD ABAP2XML.
*
*/ Look at method serialize_id for a new way of doing XML.

  TYPE-POOLS: ABAP.

  CONSTANTS:
    XML_HEAD TYPE STRING VALUE '<?xml version="1.0" encoding="utf-8"?>',
    ITEM_ATR TYPE STRING VALUE 'idx="#"'.

  DATA:
    XML_FRAGMENTS  TYPE TABLE OF STRING,
    REC_XML_STRING TYPE STRING,
    L_TYPE         TYPE C,
    S_TYPE         TYPE C,
    L_COMPS        TYPE I,
    L_VALUE        TYPE STRING,
    T_STRING       TYPE STRING,
    L_ITEM_ATR     TYPE STRING,
    L_ITEM_STR     TYPE STRING,
    L_NAME         TYPE STRING,
    L_IDX          TYPE STRING,
    L_TYPEDESCR    TYPE REF TO CL_ABAP_STRUCTDESCR,
    L_LINEDESCR    TYPE REF TO CL_ABAP_DATADESCR,
    L_TABLEDESCR   TYPE REF TO CL_ABAP_TABLEDESCR.

  FIELD-SYMBOLS:
    <ABAP_DATA> TYPE ANY,
    <ITAB>      TYPE ANY TABLE,
    <STRU>      TYPE ANY TABLE,
    <COMP>      TYPE ANY,
    <ABAPCOMP>  TYPE ABAP_COMPDESCR.

  DEFINE GET_SCALAR_VALUE.
    " &1 : assigned var
    " &2 : abap data
    " &3 : abap type
    " &4 : scape
    &1 = &2.
****************************************************
* Adapt some basic ABAP types (pending inclusion of all basic abap types?)
* Feel free to customize this for your needs
    case &3.
*       1. ABAP numeric types
      when 'I'. " Integer
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.

      when 'F'. " Float
        condense &1.

      when 'P'. " Packed number (used in quantities, for example)
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.

      when 'X'. " Hexadecimal
        condense &1.
        concatenate '0x' &1 into &1.

*       2. ABAP char types
      when 'D'. " Date type
        CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2) INTO &1.

      when 'T'. " Time representation
        CONCATENATE &1(2) ':' &1+2(2) ':' &1+4(2) INTO &1.

      when 'N'. " Numeric text field
*           condense &1.

      when 'C' or 'g'. " Char sequences and Strings
* Put safe chars
        t_string = &2.

        if &4 = abap_true.
        &1 = cl_http_utility=>escape_html( t_string ).
        endif.

      when 'y'.  " XSTRING
* Put the XSTRING in Base64
        &1 = cl_http_utility=>ENCODE_X_BASE64( &2 ).

      when others.
* Don't hesitate to add and modify abap types to suit your taste.

    endcase.
** End of scalar data preparing.

  END-OF-DEFINITION.



*******************************
* Put XML header if requested *
*******************************
  IF WITH_XML_HEADER EQ ABAP_TRUE.
    APPEND XML_HEAD TO XML_FRAGMENTS.
  ENDIF.

***************************************************
*  Open XML tag                                   *
*  <          >                                   *
***************************************************
  IF NAME IS NOT INITIAL.
    L_NAME = NAME.
    IF NAME_ATR IS NOT INITIAL.
      CONCATENATE NAME NAME_ATR INTO L_NAME SEPARATED BY SPACE.
    ENDIF.
    CONCATENATE '<' L_NAME '>' INTO REC_XML_STRING.
    APPEND REC_XML_STRING TO XML_FRAGMENTS.
    CLEAR REC_XML_STRING.
  ENDIF.

**
* Get ABAP data type
  DESCRIBE FIELD ABAP_DATA TYPE L_TYPE COMPONENTS L_COMPS .

***************************************************
*  Get rid of data references
***************************************************
  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF.
    ASSIGN ABAP_DATA->* TO <ABAP_DATA>.
    IF SY-SUBRC NE 0.
      IF NAME IS NOT INITIAL.
        CONCATENATE '<' NAME '/>' INTO XML_STRING.
      ELSE.
        CLEAR XML_STRING.
      ENDIF.
      EXIT.
    ENDIF.
  ELSE.
    ASSIGN ABAP_DATA TO <ABAP_DATA>.
  ENDIF.


* Get ABAP data type again and start
  DESCRIBE FIELD <ABAP_DATA> TYPE L_TYPE COMPONENTS L_COMPS.


***************************************************
*  Tables
***************************************************
  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE.
    L_TABLEDESCR ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( <ABAP_DATA> ).
    L_LINEDESCR = L_TABLEDESCR->GET_TABLE_LINE_TYPE( ).
    L_ITEM_STR = L_LINEDESCR->GET_RELATIVE_NAME( ).
    ASSIGN <ABAP_DATA> TO <ITAB>.
    LOOP AT <ITAB> ASSIGNING <COMP>.
      L_IDX = SY-TABIX.
      CONDENSE L_IDX.
      L_ITEM_ATR = ITEM_ATR.
      REPLACE '#' IN L_ITEM_ATR WITH L_IDX.
      IF UPCASE NE 'X'.
        TRANSLATE L_ITEM_STR TO LOWER CASE.
      ENDIF.
*> Recursive call for line items here:
      REC_XML_STRING = ABAP2XML( ABAP_DATA = <COMP> UPCASE = UPCASE NAME = L_ITEM_STR NAME_ATR = L_ITEM_ATR ).
      APPEND REC_XML_STRING TO XML_FRAGMENTS.
      CLEAR REC_XML_STRING.
    ENDLOOP.


***************************************************
*  Structures
***************************************************
  ELSE .
    IF L_COMPS IS NOT INITIAL.
      L_TYPEDESCR ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( <ABAP_DATA> ).
      LOOP AT L_TYPEDESCR->COMPONENTS ASSIGNING <ABAPCOMP> .
        ASSIGN COMPONENT <ABAPCOMP>-NAME OF STRUCTURE <ABAP_DATA> TO <COMP>.
        L_NAME = <ABAPCOMP>-NAME. " l_value justs holds the name here.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
        IF UPCASE NE 'X'.
          TRANSLATE L_NAME TO LOWER CASE.
        ENDIF.
        DESCRIBE FIELD <COMP> TYPE S_TYPE.
        IF S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF OR
           S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT1 OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT2.
*> Recursive call for non-scalars:
          REC_XML_STRING = ABAP2XML( ABAP_DATA = <COMP> NAME = L_NAME UPCASE = UPCASE ).
        ELSE.
          IF S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_OREF OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_IREF.
            REC_XML_STRING = 'REF UNSUPPORTED'.
          ELSE.
            GET_SCALAR_VALUE REC_XML_STRING <COMP> S_TYPE ABAP_TRUE.
          ENDIF.
          CONCATENATE '<' L_NAME '>' REC_XML_STRING '</' L_NAME '>' INTO REC_XML_STRING.
        ENDIF.
        APPEND REC_XML_STRING TO XML_FRAGMENTS.
        CLEAR REC_XML_STRING.
      ENDLOOP.



****************************************************
*                  - Scalars -                     *
****************************************************
    ELSE.

      GET_SCALAR_VALUE L_VALUE <ABAP_DATA> L_TYPE ABAP_FALSE.
      APPEND L_VALUE TO XML_FRAGMENTS.

    ENDIF.
* End of structure/scalar IF block.
***********************************


  ENDIF.
* End of main IF block.
**********************


*****************
* Close XML tag *
*****************
  IF NAME IS NOT INITIAL.
    CONCATENATE '</' NAME '>' INTO REC_XML_STRING.
    APPEND REC_XML_STRING TO XML_FRAGMENTS.
    CLEAR REC_XML_STRING.
  ENDIF.

* Use a loop in older releases that don't support concatenate lines.
  CONCATENATE LINES OF XML_FRAGMENTS INTO XML_STRING.

ENDMETHOD.


method ABAP2YAML.
*********************
* ABAP goes to YAML *
*********************

  type-pools: abap.

  constants:
    c_comma     type c value ',',
    c_space     type c value ' ',
    c_colon     type c value ':',
    c_quote     type c value '"',
    c_squot     type c value '''',
    c_colo2(2)  type c value ': ',
    c_indt2     type i value 2,
    c_hyph      type c value '-'.

  data:
  ly_level type i,
  l_dont_indent type xfeld,
  dec_level type i value 0,
  dont_quote type xfeld,
  yaml_fragments type table of string,
  rec_yaml_string type string,
  l_type  type c ,
  l_comps type i ,
  l_lines type i ,
  l_index type i ,
  l_value type string,
  l_name type string.
  field-symbols:
    <abap_data> type any,
    <itab> type any table,
    <stru> type any table,
    <comp> type any.
  data l_typedescr type ref to cl_abap_structdescr .
  field-symbols <abapcomp> type abap_compdescr .

  ly_level = y_level.

**
* Get ABAP data type
  describe field abap_data type l_type components l_comps .

***************************************************
*  First of all, get rid of data references
***************************************************
  if l_type eq cl_abap_typedescr=>typekind_dref.
    assign abap_data->* to <abap_data>.
    if sy-subrc ne 0.
      yaml_string = space. " pasamos de poner nada si falla...
      exit.
    endif.
  else.
    assign abap_data to <abap_data>.
  endif.


* Get ABAP data type again and start
  describe field <abap_data> type l_type components l_comps.

***************************************************
*  Prepare field names, YAML does not quote names *
***************************************************
* Put hyphens...
  if name is initial and y_level gt 0.
    concatenate c_hyph space into rec_yaml_string respecting blanks.
    l_dont_indent = 'X'.
  endif.

  if name is not initial.
    concatenate name c_colon c_space into rec_yaml_string respecting blanks.
  endif.

* do indent
  if dont_indent ne 'X'.
    do  ly_level  times.
      shift rec_yaml_string right by c_indt2 places.
    enddo.
  endif.

  append rec_yaml_string to yaml_fragments.
  clear rec_yaml_string.




***************************************************
*  Tables
***************************************************
  if l_type eq cl_abap_typedescr=>TYPEKIND_TABLE.
    assign <abap_data> to <itab>.
    l_lines = lines( <itab> ).
    clear l_index.
    if l_lines eq 0.
      move '[]' to rec_yaml_string.
      append rec_yaml_string to yaml_fragments.
      clear rec_yaml_string.
      append xnl to yaml_fragments.
    else.
      if name is not initial.
        append xnl to yaml_fragments.
      endif.
      add 1 to ly_level.
      loop at <itab> assigning <comp>.
        add 1 to l_index.
*> Recursive call here
        rec_yaml_string = abap2yaml( abap_data = <comp> upcase = upcase y_level = ly_level s_index = l_index ).
        append rec_yaml_string to yaml_fragments.
        clear rec_yaml_string.
      endloop.
    endif.
* YAML table ends *
*******************


***************************************************
*  Structures
***************************************************
  else .
    if l_comps is not initial.
      if name is not initial.
        append xnl to yaml_fragments.
      endif.
      add 1 to ly_level.
* Loop for structure elements
      l_typedescr ?= cl_abap_typedescr=>describe_by_data( <abap_data> ) .
      clear l_index.
      loop at l_typedescr->components assigning <abapcomp>.
        add 1 to l_index.
        assign component <abapcomp>-name of structure <abap_data> to <comp>.
        l_name = <abapcomp>-name.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
        if upcase ne 'X'.
          translate l_name to lower case.
        endif.
*> Recursive call here
        rec_yaml_string = abap2yaml( abap_data = <comp> name = l_name upcase = upcase y_level = ly_level s_index = l_index dont_indent = l_dont_indent ).
        clear l_dont_indent. " it is only used once
        append rec_yaml_string to yaml_fragments.
        clear rec_yaml_string.
      endloop.

* YAML structure ends *
***********************


***************************************************
*  Scalars and others...
***************************************************
    else.
      if l_type eq cl_abap_typedescr=>TYPEKIND_OREF or l_type eq cl_abap_typedescr=>TYPEKIND_IREF.
        l_value = 'REF UNSUPPORTED'.
      else.
        l_value = <abap_data>.
      endif.

* Adapt some basic ABAP types (pending inclusion of all basic abap types)
* Feel free to customize this for your needs
      case l_type.
*       1. ABAP numeric types
        when 'I'. " Integer
          condense l_value.
          if sign( l_value ) < 0.
            shift l_value by 1 places right circular.
          endif.
          dont_quote = 'X'.

        when 'F'. " Float
          condense l_value.
          dont_quote = 'X'.

        when 'P'. " Packed number (used in quantities, for example)
          condense l_value.
          if sign( l_value ) < 0.
            shift l_value by 1 places right circular.
          endif.
          dont_quote = 'X'.

        when 'X'. " Hexadecimal
          condense l_value.
          concatenate '0x' l_value into l_value.
          dont_quote = 'X'.

*       2. ABAP char types
        when 'D'. " Date type
          CONCATENATE l_value(4) '-' l_value+4(2) '-' l_value+6(2) INTO l_value.

        when 'T'. " Time representation
          CONCATENATE l_value(2) ':' l_value+2(2) ':' l_value+4(2) INTO l_value.

        when 'N'. " Numeric text field
*           condense l_value.

        when 'C' or 'g'. " Chars and Strings
* Put safe chars
          replace all occurrences of '\' in l_value with '\\' .
          replace all occurrences of '"' in l_value with '\"' .
          replace all occurrences of cl_abap_char_utilities=>cr_lf in l_value with '\r\n' .
          replace all occurrences of cl_abap_char_utilities=>newline in l_value with '\n' .
          replace all occurrences of cl_abap_char_utilities=>horizontal_tab in l_value with '\t' .
          replace all occurrences of cl_abap_char_utilities=>backspace in l_value with '\b' .
          replace all occurrences of cl_abap_char_utilities=>form_feed in l_value with '\f' .

        when 'y'.  " XSTRING
* Put the XSTRING in Base64
*          l_value = cl_http_utility=>ENCODE_X_BASE64( <abap_data> ).
          l_value = 'XSTRING not supported in YAML yet!'.

        when others.
* Don't hesitate to add and modify abap types to suit your taste.

      endcase.

* We use YAML scalars double quoted
      if dont_quote ne 'X'.
        concatenate c_quote l_value c_quote into l_value.
      else.
        clear dont_quote.
      endif.

      append l_value to yaml_fragments.

      append xnl to yaml_fragments.

    endif. " is structure or scalar

  endif. " main typekind sentence



* Use a loop in older releases that don't support concatenate lines.
  concatenate lines of yaml_fragments into yaml_string respecting blanks.

endmethod.


method BUILD_PARAMS.

  type-pools: ABAP.

  data defval type RS38L_DEFO.
  data dataname type string.
  data waref type ref to data.

  field-symbols:
    <wa> type any,
    <temp> type any.

  data len type i.
  data excnt type i value 1.

  data paramline  type line  of ABAP_FUNC_PARMBIND_TAB.
  data exceptline type line  of ABAP_FUNC_EXCPBIND_TAB.
  data t_params_p type table of RFC_FINT_P.
  data params_p   type RFC_FINT_P.

  define remove_enclosing_quotes.
    " Remove enclosing single quotes
    if &2 gt 1.
      subtract 1 from &2.
      if &1+&2 eq ''''.
        &1+&2 = space.
      endif.
      if &1(1) eq ''''.
        shift &1 left.
      endif.
      &2 = strlen( &1 ).
    endif.
  end-of-definition.


* do we have the rfc name?
  call function 'RFC_GET_FUNCTION_INTERFACE_P'
    EXPORTING
      funcname      = function_name
      language      = 'E'       "'D'  "sy-langu
    TABLES
      params_p      = t_params_p
    EXCEPTIONS
      fu_not_found  = 1
      nametab_fault = 2
      others        = 3.

  if sy-subrc <> 0.
    raise INVALID_FUNCTION.
  endif.


* Build params table
  loop at t_params_p into params_p.

    unassign <wa>.
    unassign <temp>.
    clear paramline.

    case params_p-paramclass.

      when 'I' or 'E' or 'C'.

        paramline-name = params_p-parameter.

        if params_p-paramclass = 'E'.
          paramline-kind = ABAP_FUNC_IMPORTING.
        elseif params_p-paramclass = 'I'.
          paramline-kind = ABAP_FUNC_EXPORTING.
        else.
          paramline-kind = ABAP_FUNC_CHANGING.
        endif.

        if params_p-fieldname is initial.
          dataname = params_p-tabname.
        else.
          concatenate params_p-tabname params_p-fieldname into
              dataname separated by '-'.
        endif.

* Assign default values
        defval = params_p-default.
        if dataname is initial.
           dataname = 'STRING'.  " use a STRING for this cases (see CONVERT_DATE_TO_EXTERNAL).
        endif.
        create data waref type (dataname).
        assign waref->* to <wa>.
        len = strlen( defval ).
        remove_enclosing_quotes defval len.
        if defval = 'SPACE'.
          <wa> = space.
        elseif len > 3 and defval+0(3) = 'SY-'.
          assign (defval) to <temp>.
          <wa> = <temp>.
          unassign <temp>.
        else.
          if defval is not initial.
            <wa> = defval.
          endif.
        endif.
        unassign <wa>.
        paramline-value = waref.
        insert paramline into table paramtab.

      when 'T'.
        paramline-name = params_p-parameter.
        paramline-kind = ABAP_FUNC_TABLES.
        if params_p-exid eq 'h'.
          create data waref type (params_p-tabname).
        else.
          create data waref type standard table of (params_p-tabname).
        endif.
        paramline-value = waref.
        insert paramline into table paramtab.

      when 'X'.
        exceptline-name = params_p-parameter.
        exceptline-value = excnt.
        data messg type ref to data.
        create data messg type string.
        assign messg->* to <temp>.
        <temp> = params_p-paramtext.
        exceptline-message = messg.
        insert exceptline into table exceptab.
        add 1 to excnt.

      when others.
        raise UNSUPPORTED_PARAM_TYPE.

    endcase.

  endloop.


* add in the catch all exception
  exceptline-name = 'OTHERS'.
  exceptline-value = excnt.
  insert exceptline into table exceptab.


* return
  params = t_params_p.

*********************************
******* Remaining from 2006 *****
******* end of build_params *****
*********************************
endmethod.


  METHOD CONVERT_TO_UTF8.

    DATA: LC_LENGTH TYPE I.
    DATA: LC_BUFFER TYPE XSTRING.
    DATA: LT_TEXTO  TYPE TABLE OF CHAR80.
    DATA: TAMANHO   TYPE I.

    TRY .
        LC_LENGTH = STRLEN( I_TEXTO ).
        CL_ABAP_CONV_OUT_CE=>CREATE( ENCODING = 'UTF-8' )->CONVERT( EXPORTING DATA = I_TEXTO N = LC_LENGTH IMPORTING BUFFER = LC_BUFFER ).

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            BUFFER        = LC_BUFFER
          IMPORTING
            OUTPUT_LENGTH = TAMANHO
          TABLES
            BINARY_TAB    = LT_TEXTO.

        CALL FUNCTION 'SCMS_BINARY_TO_STRING'
          EXPORTING
            INPUT_LENGTH = TAMANHO
          IMPORTING
            TEXT_BUFFER  = R_TEXTO
          TABLES
            BINARY_TAB   = LT_TEXTO
          EXCEPTIONS
            FAILED       = 1
            OTHERS       = 2.

        IF SY-SUBRC IS NOT INITIAL.
          R_TEXTO = I_TEXTO.
        ENDIF.

      CATCH CX_PARAMETER_INVALID_RANGE.
        R_TEXTO = I_TEXTO.
      CATCH CX_SY_CODEPAGE_CONVERTER_INIT.
        R_TEXTO = I_TEXTO.
      CATCH CX_SY_CONVERSION_CODEPAGE.
        R_TEXTO = I_TEXTO.
      CATCH CX_PARAMETER_INVALID_TYPE.
        R_TEXTO = I_TEXTO.
    ENDTRY.

    REPLACE ALL OCCURRENCES OF '\' IN R_TEXTO WITH '\\' .
    REPLACE ALL OCCURRENCES OF '"' IN R_TEXTO WITH '\"' .
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF IN R_TEXTO WITH '\r\n' .
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>NEWLINE IN R_TEXTO WITH '\n' .
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB IN R_TEXTO WITH '\t' .
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>BACKSPACE IN R_TEXTO WITH '\b' .
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>FORM_FEED IN R_TEXTO WITH '\f' .

  ENDMETHOD.


  METHOD CONVERT_TXT_JSON_TO_STRING.

    R_TEXTO = I_TEXTO.
    REPLACE ALL OCCURRENCES OF '\u00e1' IN R_TEXTO WITH 'á' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e0' IN R_TEXTO WITH 'à' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e2' IN R_TEXTO WITH 'â' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e3' IN R_TEXTO WITH 'ã' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e4' IN R_TEXTO WITH 'ä' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c1' IN R_TEXTO WITH 'Á' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c0' IN R_TEXTO WITH 'À' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c2' IN R_TEXTO WITH 'Â' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c3' IN R_TEXTO WITH 'Ã' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c4' IN R_TEXTO WITH 'Ä' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e9' IN R_TEXTO WITH 'é' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e8' IN R_TEXTO WITH 'è' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ea' IN R_TEXTO WITH 'ê' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ea' IN R_TEXTO WITH 'ê' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c9' IN R_TEXTO WITH 'É' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c8' IN R_TEXTO WITH 'È' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ca' IN R_TEXTO WITH 'Ê' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00cb' IN R_TEXTO WITH 'Ë' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ed' IN R_TEXTO WITH 'í' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ec' IN R_TEXTO WITH 'ì' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ee' IN R_TEXTO WITH 'î' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ef' IN R_TEXTO WITH 'ï' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00cd' IN R_TEXTO WITH 'Í' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00cc' IN R_TEXTO WITH 'Ì' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ce' IN R_TEXTO WITH 'Î' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00cf' IN R_TEXTO WITH 'Ï' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f3' IN R_TEXTO WITH 'ó' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f2' IN R_TEXTO WITH 'ò' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f4' IN R_TEXTO WITH 'ô' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f5' IN R_TEXTO WITH 'õ' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f6' IN R_TEXTO WITH 'ö' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d3' IN R_TEXTO WITH 'Ó' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d2' IN R_TEXTO WITH 'Ò' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d4' IN R_TEXTO WITH 'Ô' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d5' IN R_TEXTO WITH 'Õ' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d6' IN R_TEXTO WITH 'Ö' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00fa' IN R_TEXTO WITH 'ú' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f9' IN R_TEXTO WITH 'ù' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00fb' IN R_TEXTO WITH 'û' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00fc' IN R_TEXTO WITH 'ü' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00da' IN R_TEXTO WITH 'Ú' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d9' IN R_TEXTO WITH 'Ù' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00db' IN R_TEXTO WITH 'Û' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e7' IN R_TEXTO WITH 'ç' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c7' IN R_TEXTO WITH 'Ç' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f1' IN R_TEXTO WITH 'ñ' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d1' IN R_TEXTO WITH 'Ñ' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u0026' IN R_TEXTO WITH '&' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u0027' IN R_TEXTO WITH '''' IGNORING CASE.

  ENDMETHOD.


METHOD DESERIALIZE_ID.
*/***********************************************************/*
*/ New method using the built-in transformation              /*
*/ included in releases 7.02 and 7.03/7.31 (Kernelpatch 116) /*
*/***********************************************************/*

  TYPE-POOLS: ABAP.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  DATA:
    RTAB       TYPE ABAP_TRANS_RESBIND_TAB,
    RLIN       TYPE ABAP_TRANS_RESBIND,
    OEXCP      TYPE REF TO CX_ROOT,
    ETEXT      TYPE STRING,
    JSON_XTEXT TYPE XSTRING.

  FIELD-SYMBOLS <PARM> TYPE ABAP_FUNC_PARMBIND.

  IF JSON IS INITIAL. EXIT. ENDIF.  " exit method if there is nothing to parse

  " build rtab table for transformation id

  LOOP AT PARAMTAB ASSIGNING <PARM>.
    IF <PARM>-KIND EQ ABAP_FUNC_IMPORTING. "" va al revés, cuidado!!!
      CONTINUE.
    ENDIF.
    RLIN-NAME  = <PARM>-NAME.
    RLIN-VALUE = <PARM>-VALUE.
    APPEND RLIN TO RTAB.
  ENDLOOP.

  " Convert input JSON variable names to uppercase

  JSON_XTEXT = CL_ABAP_CODEPAGE=>CONVERT_TO( JSON ).
  DATA(READER) = CL_SXML_STRING_READER=>CREATE( JSON_XTEXT ).
  DATA(WRITER) = CAST IF_SXML_WRITER( CL_SXML_STRING_WRITER=>CREATE( TYPE = IF_SXML=>CO_XT_JSON ) ).
  DO.
    DATA(NODE) = READER->READ_NEXT_NODE( ).
    IF NODE IS INITIAL.
      EXIT.
    ENDIF.
    IF NODE->TYPE = IF_SXML_NODE=>CO_NT_ELEMENT_OPEN.
      DATA(ATTRIBUTES)  = CAST IF_SXML_OPEN_ELEMENT( NODE )->GET_ATTRIBUTES( ).
      LOOP AT ATTRIBUTES ASSIGNING FIELD-SYMBOL(<ATTRIBUTE>).
        IF <ATTRIBUTE>->QNAME-NAME = 'name'.
          <ATTRIBUTE>->SET_VALUE(
            TO_UPPER( <ATTRIBUTE>->GET_VALUE( ) ) ).
        ENDIF.
      ENDLOOP.
    ENDIF.
    WRITER->WRITE_NODE( NODE ).
  ENDDO.
  JSON_XTEXT = CAST CL_SXML_STRING_WRITER( WRITER )->GET_OUTPUT( ) .

  TRY.
      CALL TRANSFORMATION ID SOURCE XML JSON_XTEXT RESULT (RTAB).
    CATCH CX_ROOT INTO OEXCP.

      ETEXT = OEXCP->IF_MESSAGE~GET_TEXT( ).
      RAISE EXCEPTION TYPE ZCX_JSON
        EXPORTING
          MESSAGE = ETEXT.

  ENDTRY.

ENDMETHOD.


  METHOD GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD GERA_ERRO_GERAL_SYS.

    RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD GET_HTML_ERRO.

    R_HTML =
     '<html>' &&
     '<head>' &&
     '<title>' && 'Amaggi - Erro de Serviço SAP' && '</title>' &&
     '<style>' &&
     'body { background: #ffffff; text-align: center; width:100%; height:100%; overflow:hidden; }' &&
     '.content { display: table; position:absolute; width:100%; height:80%; }' &&
     '.valigned { display: table-cell; vertical-align: middle; }' &&
     '.lowerCenter { display: table-cell; vertical-align: bottom; }' &&
     '.footer { position: absolute; bottom: 10px; left: 0; width: 100%; z-index: -1; }' &&
     '.footerLeft { float: left; margin-left: 20px; }' &&
     '.footerRight { float: right; margin-right: 20px; position: absolute; bottom: 0px; right: 0px; }' &&
     '.centerText { font-style: normal; font-family: Arial; font-size: 26px; color: #444444; z-index: 1; }' &&
     '.errorTextHeader { font-style: normal; font-family: Arial; font-size: 20px; color: #444444; }' &&
     '.bottomText { align: center; font-style: normal; font-family: Arial; font-size: 14px; color: #444444; }' &&
     '.biggerBottomText { align: center; font-style: normal; font-family: Arial; font-size: 16px; color: #444444; }' &&
     '.detailTable { align: bottom; vertical-align: middle; margin-left:auto; margin-right:auto; font-style: normal; font-family: Arial; font-size: 16px; color: #444444; }' &&
     '</style>' &&
     '<div class="content">' &&
     '<div class="valigned">' &&
     '<p class="centerText"><span class="errorTextHeader">' && I_STATUS && '-' && I_MENSSAGEM && '</span></p></div></div>' &&
     '<div class="content"><div class="lowerCenter">' &&
     '<p class="centerText">' &&
     '</p></div></div>' &&
     '<div class="footer"><div class="footerLeft">' &&
     '<img title="" alt="Amaggi logo" src="data:image/png;base64,' &&
     'iVBORw0KGgoAAAANSUhEUgAAANMAAAAXCAYAAACRfnp7AAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsQAAA7EAZUrDhsAAAAldEVYdFNvZnR' &&
     '3YXJlAEFkb2JlIFBob3Rvc2hvcCBDQyAoV2luZG93cymwFbrrAAAAB3RJTUUH3gUPDyoTlOqCEgAAACF0RVh0Q3JlYXRpb24gVGltZQAyMDE0OjA0OjIyIDE0OjQwOjU' &&
     'zV48ZXgAAGdRJREFUeF7tXAd4VNW2/ieTNukNQkApUgWES1GfqKBcEbFS5V4QG2ADr1xFFFQsWBEFOyII2EAUuChYaEHQS1UiJTQhFOnJpM1kSqa8f+1zTjKTOgn63' &&
     'vfed3++kzlnn3X22nvt1fY++2DyE/gP/oM/E4aKmUza7/9ThGxMGpWP8ghT1wZ8vKFK6iwoqTDwmYrX5wa/v3Jb6woRjd/nQ5jZrJdokDJTmLS17u31sV1hAe06nW/Dps' &&
     'OnkH2qGCetNhQ43Qhj3X6fH+Hk2zA2Co1SLGiZkoTWGXFo2yhNfzIQtcuOPeFg8yRgnHYfz8P2o2dx8HQRThQ64fR6VC1CZokIR3pcNBonxeCCRolon56MJilx6rlA+' &&
     'NhOTRTB/L1+D8ymcP2q7lD1qoprhrRVaM0h0Aqxj3/CQtHV2kVaCaFHJioBNQhZx07j2ldX4GyRHbf3aIv591zLezJUIs8QOk9aofP73Oj2/DJsP3Aaf2mZgV+evoXl9' &&
     'Re+AaN+wS1vrcBX246gaWoCMifejAsaJKjykKH3+Y1VOzB+4Ub4We/bwy7Ffb076wShIbBNgsy9R/HKt7/i+62/AU5KLpL9DtN4aYdIk4dPnpFf/lBh1G+pG0iMwyWtGq' &&
     'Fn23T0ad8Y13ZsxhvVyF6GN4D37B9246212dix4yh5kle48CaN8A8LkL/Xq1hr5zyRetyliG2cistbarxv6nIeOp3XkE0L0wyqAn4+fIK89mL++j3Y+FR//FfLJvqd' &&
     '0ND9ywk4UHAc0eZIvUSawXaEh+HSBm0x+6r7kW7RxrTdZ2NxuqQAkeYIdR0Im9eJ+y7si9cuv1NdN5l/L5yeElZTWd9sHhee7DoIE7sN0ktCR53SvCKnHYlD3wJSErWxs' &&
     '7lwa6+2+HxMP+VJNW9dM1Qk4+AmjJmH4oIiIJqdpzeGORz+jx4gRe111AzRuDA0f+QjHDluBejZUeoBSlzwfzleI6kDPt14ALe9vARIjecV23a2GEufuRn9u7Ur60t' &&
     't8El0o+L+cuQsrnxxGUpy2W96fVEKpYVOKm4pD0YGeNh+GRI/o2EYy6R6M+kkOgqvCDEA/irDIt3pQri+e5z2GBw9BX4h8pv4mAnvrNmJsbNWspR0FipRBH/FWF2lNBLhL' &&
     'Tz5K0YruYbwFWMTQ1P85RkWSnuljW4PwuIs8M6+V1gFYdGWfbhzzno4cu1ADPmUuJE1/Q50blpVVK0eLhp09HsDkGhJVV03IL7G5XOhxGHDvhHvoU1yE+S77EiZNQ' &&
     'wJsUlsPdsbAOmCtSQPPw99A10btsAxmxVN596B5Ni0Mn8RiAJ7LnJGzEbzpHS9JDSEZEwqNFK6DcZ+iFybQxsIkbZ47jwbMl8ajqvaNdaIQ4Dd5UTc396mgjJtEClJE0SZ' &&
     'OFD+uWJQ9YHRDRM6PPE5sg+f5kDSkARyq9CBlYx+fTq20MpCQCn7FzlkBiNBjKZEAhqGOBH/wgfJqrICB4H9EtaizE8t3Yjn522gI4rVFFM0Qoy8yI0rLmmBu69si' &&
     '15tmjB60lEFwO3xIudsEXadzMe2w7n4ad8JbMg+RiFSXtFsE9viXfpI5VSU1asW80+nyV9gZ/YRUNP0fvAQB+YpxfC/dsLfL22BS1o0RoN4izxRhiKHEwfO2rDrmBVbDp3ED' &&
     '8widgtv6Tf1NaFhIgrfuktnprgpmG6cCqTFwsSoR1um7O3ImnZ7nYzJcM5LcrZg0IoX0UApvkizHB54Uegsge/+Rer6w72ZGLnmLaTFJKu+B1J7OZYOrwOO0QvV' &&
     '9etZy/HIxrlIi05S1+XwqxTVx/4U3DVfGz/1R92sESFHpn8u2IAZS3+mYtGjBtYsnszupHKN0wtqh83pRDw9Cl0Oq9LrklZQucLo/byz7+dFCK2vAp2e+Aw7c3K1iBSIIgd' &&
     'WTx6Ev3Y4Xy+oGoF60XT8xzjG+QSiRPm1MgV65TYZSdj38nC9oBrolU35eismz14PToD0G4StFBecn4qsKQMQHxWsxKHAaivGgi0H8eo32dj9wmB2tzwVEhhzxs5' &&
     'PLMCOQ2e0SChC9tEK8u0YcX0HfDSKKXo9sOv4GSzcfBArd57AlqcHaQqn3VIwMQIgnvwMLayHMQnUPI//blnxElad3IFYc7CchGex14XL0lphbf9nVVnPfz2DX3IPMTUMD2q' &&
     'TwEbaazLa4+sbnlDXnb8Yj5yiM4iqKt1zl+DWVldi/l/HVPQV1SI4HlaAYWd7TuZixsJNKqyLJxWPVgapITwSFz+neQcZxNpQuWHSWv64ffA5fTCPfFcrVsNUOw' &&
     'yeXZ9dhJ2H8rTHpO16+w2EIhA/vZJgyrLNOHaUaaLI2c5oLBHJQKQZ+w/mYtp3dC6CCnzKQIZH84poSJnlhiTK7PKgc+uGODh1mDIkJefq6giARscqeJ4SF48xvf+Cw9OGVTI' &&
     'kgRjS9JVZ2LH/RLkhiZCL7Zhy15W6IVFdWWco/lQoNFofOjZpiOcHXqYZkqSGfyr8WHbDREYqEzyqnZK2Mk+SuSWVL84chcwTWZiX/YOiXt//GThLOV4kNeiMI' &&
     '560y3O2YvGhzYo2a9BUFDttJC2v0zjiomLxUfZKrDuZzWtFXitqNCajkvaPfS6jB9/ZEux/UwaPaY9XhlQbhLBoM7b9+js+3LBHNSSEsakAGWQHtrw0lOlHKXwuHxLvm62VhwD' &&
     'h2eOFL7A9+yQY9rDuuYH6nWCE0qwweqnf8/IxeR4jiaSJnB8ceW8kE2kZIKlBDgo/PhKPvp/JKZRNCao6hRw+aw0jMA1J5Ts8TJSbw4Os54bwWntGLU6EMGIa' &&
     'XS2DFoCHpQ+S2hk9Z8rYtFkqnrypu3Yt/WCdqt5aIBQabTB3E1O+2p+uH8hN/RX8NGgK5zJWXmkGrQ7lVjhbsKTgrszpzHid6nr1zc8gz55XTldGz+wzJgWDv31JTftMnBMuuW' &&
     'ESaXMr0fIPM/IU9GakCxW1jIsJN77xDV0hvTUnqvcO6YLW6WnY+UJ/zpWKWa7l6WrRKdmCkTO+gcPtpICrVqwaUerDxc3TkDdvNAfdg6LCEsTe94F+szpowry' &&
     'MhrTxV3pglxvfvTwUvVqfR4VlBK0rdINoM/FL9ocOo8iGNZNuQdOkeLw6qicNyqUbBQdUJuVJMWj5WM0R+cetnKtEktbQOM5VhvTuwBMRvVH4x+PbXYdVOlo215N2O0oxY+jl2' &&
     'vX/MXRNbYmHutwEq9uul5RDjC4hKgGdFz+irq9q0gF3dLgGxaWacQWBYoiLike3LzTaAc2746aWl8LuobOsAHmFERMehZ5LJ+slNaNGY/p29xGsyNzDVCcC0Q' &&
     'lRmHn7VcobtEhNwYQRPRhNSnRKKqFMSpknt5jwGa/roSR8pJgGmxIXh5K596mVrhKrAxnj5uoEwQaqKW8Yrp22DJuyTlFxnNj0+gj07dBUI6gCNbWK5kECE+6YsxaOYg6Cw4shf' &&
     'Tqi94VN1WR4fL/uaN2qAflIiqvXRIMqttpwz/x1jGiVFyPW7T2upYmBnt/rx40X0dj/ZHzLTEGtlCqQv1rwKMW1nf583ucClUryn4yuHDLqmpvyY8YVo9DIk' &&
     'ggXU3EjHTOOKE41DhedxlObFijqeb0fpCFEopT1ibEF0lpoIDusRzA161+K9qt+kxSPimmkRGJLRBQ2nNqFuXvWKtqaUuJKxlRO7Mf1zy7WVp/ybfj91RGq1KQ/8srgK9CwcQIHS' &&
     'HJmURY2JyIcp08WYtKSTYqGlWm/IUNTOouF84iPHwBiI3DqRAEaPDSv7J4B6WxfGtKqTTlk7cL+90fj0gsaqeXq6lDtHd4QgW88cBwfLf+FSsjUJTYSi+6/' &&
     'Tt02lvz3MeqpiKd4yEMsZ7r3wZJt2H7stKIJxM7juWolOgg0pmZ1fd9VD+w6wbmjEZWkrdLkyAjERlZYmKkvpL4/A8rxMJlzWeEr3AVf0W+q2KuP/8+Dp3HaV8BxDk7L5EhhxHl+' &&
     '62fYX0AnRmwd+BIKi1kPHUlF2gbRCXhs/RycKMlXtFsGvIwCG8erAq2sWKdGJ+LuNW/C5nYoA6sOlYzJUMaLnqKFR3PiWlSCV+7tjdR4pj0VcGTa7eq+9m5' &&
     'CmPCXdC/N/xGHc9nIGhjXDs695t6PmJQE5J6woukjRoTSMOCd77Hy3weUwpyc+wDTzxQ1vvQnGkEVqP6O5vt6PLuU6V2cch77nr9VlRnQemjG4sdvYoor6YPeX0FaLLpOYmooCDBm' &&
     'u7y/odFX5Czvnc4VvoCJ/4bfjmPUvB9w15x1eokMCyNomfz13zo7t8ooc7Z6lV9u3Y8bX/8a8/69Tys4B0iPvM5cuH8ZB99WzlN3T4F/10R4fhoIb85HkK' &&
     'Wh9JgETO95L4egmNKXRgQeYUiOTcXFSx7nOdAyKQPPXDFM0QbTycxL3n2noOvCR3kGdEprhnFd+8Pq5By4Aq2MYQKNr8sXD/Nal0EVsiw3Jt4UIjMnZW9n7sCu7LOqrFWrNEy4vju' &&
     '8ymDKIXVFh5sxe1w/9Q5HmyYJc54wmrV89HMp0K4NpasDjEGzzxyJRk2ScCzHitaPfYacs/no//Zy/Gv1biAmEiWfPIBGCTFUUG0rjPYnNEg6oTpCYfV8i' &&
     'VFYjMruwMThPdG6UUqQ0qtqKYOB3Vrjmh7N9AhlMKMYyf+66StIyDK97Wb1IzzUpQYW5sjLzHNEGNPqF77aBNOd76Dn4wsxZ8V27DlFz6rDLHO6Mr48kXa5fUyl9bmk3sa6Qjyzs9S' &&
     'F2z5YBdPg1zBk2jdYsX4/vXrlOUeokLGW1vgcnPduG43w0iKYolLhj4jnkYiw8FTg5HL4dz6pDG5c5xvQLa0FXN6AVWUd4dRfV6kHw1ZPV9dPdxuKdvGN' &&
     '4fJVpDUhwhSBfE8R7l2nzc2nX0Fdi06mmHRd0iG6HWWOwG/FuZi8ZaGSQdnQB6DMmLTlQZN6B/TgjO/ooSM5WXbiwIvau5SKe5+UzlD5RvbsiO4dMxiWOTk3boSbmHm5Mfw91sN66z' &&
     'NsgeH05Iy70b5DY/yWfQwXjPwAy77fi7CUaPhpSBamLjIYsgpXGyq2QzkA8vlk0x5skIUCsxnpGUl4cch/qfuyayEISgY+rHp0gNZPFR34K6/YLdH4nkr' &&
     '11XZGS73tzRomMRHnuazgKeb8Y45kJDmm7p8LRs/NxJNzf5KB0aJpTASz7PKtNBekygqiMFWdVGUyD92wjwpLyNao+iJ57Fx8ujYbSCKPBIvaUWGu8NK4TmA7xW35s5/lVIFOjHI3W' &&
     'i2Hn/I1RSQgvHAPfGcyWQJsHvwy0y67Sve0PmqHdDk+0oIFe9Zh5bFfhRQ/D52KYkeh0pNAWhnLhIg4zNq5DFtPH+I18OuQaShyWGm0wbTyL41p5JTNn+' &&
     'Bg/im2q7L8yrTFkO15j3zMyBLPVMaBJY/3J4VZb0RV0B7f+szftLmTeHJFy8pio/DZ9zux/sDvinH1dYQCPyP+37H46UG4b8ilmPlIX3jnyE6JcFVvoOHVhEpUfM7tcWEEvSuSqBTFb' &&
     'hyaWvOLWL/ukjbL0nYuU1yjX1JMA7/lZUYnpRpAnwsbq0m/cmPqMf6JNGHeOhocofnj+iHBQmfHyKy2/BgIqO7q9k3Iu0L90eF4ew0jOlE/F6chRd43y' &&
     'uLGORhkIPzsg892CCYHIysdlvFPE5p+RRl6IhOB35eo6GRmNvF1vydgLSng3fLFBeNIiUtFv6+eVzKRFbmP+4xnupevmlyJNrYheiybJE1hxh6Pt3qNYbpXTB9J2VagTY5NR5clWmoo' &&
     'C1OBUCNhKPpDC39EYR5TEIbJ3pe1woBuLVV5dcoaWLry6cFqa1EQUuPQa7KkT1JH/QdP48QUq3srvDeiF+696iKtmAjVkARVtaDN40xHY+RltAOzxvWh' &&
     'flZ+ARoIg98lLdIxckA3yHYeagNLWLsodlQE2j+hpbjJsdFIb9qAEwEOv85c3m3Iu7R31+5QSlJvHyPtqPhsgChG9bwQkGxB0eiEkeH49sc9yLVp84L6OjjtqdDlXhukvrDi/Spqa5C' &&
     'SCgf1R8nLeVoprcfrw40tumJgy8tRVGpjX4IXGITGEhWFy5ZO5BlwW9sr0fu8TrAxRa1ESwOOpGPuu3yKoh170XW4mGmkzSepfDCtOczD9NKD4Suncyx' &&
     'lDKR9GoSnUpC9J/Lw5oL10gKWmLFmws3sQjlhbehz4fkY0odKXhLwfkcUh/OqS6cskQutrN744wbPwHNfb8ORI1YlkEs6NcVopqx1UbDZd16N2CRGcZW7S/v4bEQ49jCVmr5yu6KZf4' &&
     '+8n3IqZZDbKr2Kj8KYd1bhSG6hZhP1VGr1cDVjJHOq2/p1Uu+WxKsrOvlJiEOL8dr+tLo4omAITy36/hHQWqenqDWKQii1FDBc0lticb/xavd3Vau40e' &&
     'GR2HRqL2bukukGsObmZ+HmWGmpYQD4bExEJFYe2YZF+5k6E1uGTEUJjbRSvX4zU0MLPtuXiTXHd6oxMMaPLdJOLpz4KeN3GqOLHT+/yPROULl9NWLRmOvYA+bO8nrZANOBLVmH8fFPe' &&
     '/QCVlrV7O1/AIFcj1oL8bSsfsXGsL0ebJ7MyFoP7HuF8yfZHaG6TA4yf0qMw8PvrYXVbkffDs3RrycjvGzDMuZO0pCkGDS/fw4+37w3SKllYMQDCqH6x' &&
     '2v5Xkc8ZCDi5LONiggaLx8+vudamDkefpdHv0c+HB5biQtRo2fiiLVAURoQxTF0R/ENaEsgImSjbpA0zw1Skz+pA50SI6lyOlISfKiVO4kUca201pT/wcaBL1DWeTwLTskkkqVFp+D+de' &&
     '8g36kt+vzQfwpTQ9lJUYGWR1pMGoaufIUGp8n6635Pqd3mFVM9OVLi0tBnmbY7whg/GpMJt7zJPN/LU7sTowd2Qdem2g7wOnkufRR+e4XzJ9kdoa61' &&
     'MlOiBbe//i1cDLHC73/LmBRvHW0mLtLmSfk2ZE7WnQdRlz6LwjVJTMaLo66mscjyq4DPK4OyoOUELQJ888/+uKrLeWoeqslFaCjvhGj8bepKNHjoQ7y7eiesNrviL4MlNOofr9VHcizLyc' &&
     'vHvB+zMWzWakz7jpEvSib9vKfqpAIENV3qANyz7kE0U08tHSWd9C8qnArjR/O75uDyFxdj6fbDvFfKaCb81GMaX3WttSXr6Em8vjoL1766HEfPFmk' &&
     'LH38QZJnfbGnEiN2Oeuihekh/9MPQI/76PTaEnR/4ykJr7EUpzfDPbgNgdRWTrDwlU8+yrviYZPxlkbbj4fJGbTG64w3Id1dODYVPAo2v86J/KNobm3fBwFY91UJHRVrpfQwjVI8l2qZZ' &&
     'genH/b/7r3h0ARN8C2KYY9pnjtJv1R8TFv0bry7eirC4aPhEsaTTbi8ap8Xj+GsjaLMuxI14V981rj0jO5mLPn2QGVDNc5ZQYRr+JlNW1mXUX+TEGs' &&
     '7rerc/D3d+sBrzM7NZ6MegK9rhywf6UkAkNWhDhOqZ3r1Wjy/AwRNMGeXzFEPPil148KbOeHP4Vepy9oZsjH57Fek5cNFMp+X7IHlYIrl8V+Ti3IqPJ6UncVJM2RB2pxunrSVsP+c5ETQ' &&
     'K+ZZJeEhkEt6y1UuyTLsHHdunY+cLf1fPCWSNzHjJfveHazF32S8S0mhMLFMf0bECeRcmvGUBiSl+RsNEii1MbRErYPsLxDFyjifpqzIg2Rol57LYJ' &&
     'N9AyXNMY18YczUm3XCx4hW8a5wodCBr2m3oLPPHqiCGQuH7PaUo/eVuhDMy+GSHuBoQxiQfeZRa4W88AOYL7lS+qkzwikKbTzX75B4UcJ4YWWFlV6jy3MV4+KKb8GoPbfNBxpxRKAlzc66' &&
     'kp5c6xJCtDhuev+RWTOqmZSsxc4bBQspKu1xYca6jEB/0ug+j2l/Dy8HT/LIvTvaMnfpiHNLjZWNk/eFnx01kmjx2NgqO5as5kwYOQqEN99x2Od6/' &&
     '80qYrnlFMyYDNKa85ROQYnyDdI4wXf8yFTbYmDLfGIESrxc3jP2IKS15J1jgnz+WN8sHpr7wUKkj+r9GheNAqiV0Qqpl2vzdO3egb0fZxqMp9sx1ezD1++3I2XNSU86yr111WcmCBr2fqk' &&
     'CsXGkP74lSyakYn4xZlAltWmfg6lZNcHW7hujLeV+SJUCmOgyj8tDrT1qyDW+v2gHHmUI+T1mHs0L1fRXbHLgaq355SLlRJgspXv7KVwOM6pe1ykD' &&
     'Pto3Qhw6qZ7vGiNCV2NSHE/k4ps8kVaAxbZk1Ehe3zNALqgZrVzbiO/g+fGfWIoxpHeMATBY+13wkwlK7UxSV/zsCyRAkkloddqTOHMg+UYcN3oEoPoM1t72L3ud3wtHis2g2azhlUPlTf' &&
     'IXiXGy960N0b9QKu/KO46L5t3Osgr81U5BxslmR848vYXKVlvrlBZ8sNap7VTS2fqCNUviBX6LKkBTJ/rtophlsgzngnqSpkeESQsOVYM4Vbs6DAt' &&
     '+NeSnwCPZTPnizMPqZ1HWwVzp3+KmwdCYB7Zfpjp3eMoGRqPL+PT8y9xzHlpwz2H0iH8esdpwpccLOOY2kdSK7ZDqElNhoZCRF4fzUBLRpGI92Gcno1DwFFnP9HU+J24nvdh1D1uF87Dtt' &&
     'xe+MfnkON5wezWCjGAGTLNFIjYtCk+Q4tEi1oE16Mto1TkLHJtVEGAUOZFloDoSYSsX+Vw2pQeAvtSOMqZTUJ7oTilb6yUcCZlUaJEvZDq8b8ZHab' &&
     'h7RiXJuwZBFipJSDxL1b83UuFbTAA+dkI/6FvxxIE9VwDx3XSak2jpWpJrCZ/4Q/lWhHm0KEVrNNdQvfQsQrOFNzwlqvIxqQ6tL6MWbnqvDVN2R2ir2QTVIOy0HC1V53fsrjxnP1yavUGWq' &&
     '0UnNockgtHqB/wYT3b8ZoO52KAAAAABJRU5ErkJggg==" width="250" height="20">' &&
     '</div>' &&
     '<div class="footerRight"><p class="bottomText"><span class="biggerBottomText">©</span>2019 Amaggi-TI, All rights reserved.</p></div>' &&
     '</div>' &&
     '</body></html>'.

  ENDMETHOD.


METHOD IF_HTTP_EXTENSION~HANDLE_REQUEST.

  DATA: PATH_INFO          TYPE STRING,
        CK_DOWNLOAD        TYPE STRING,
        P_INFO_TAB         TYPE TABLE OF STRING,
        "I_CONTENT_TYPE  TYPE STRING,
        I_CDATA            TYPE STRING,
        O_CDATA            TYPE STRING,
        O_NAME             TYPE STRING,
        O_AUX              TYPE STRING,
        O_DATA             TYPE XSTRING,
        JSONP_CALLBACK     TYPE STRING,
        SHOW_IMPORT_PARAMS TYPE ABAP_BOOL VALUE ABAP_FALSE,
        T_PARAMS_P         TYPE STANDARD TABLE OF RFC_FINT_P,
        PARAMTAB           TYPE ABAP_FUNC_PARMBIND_TAB,
        EXCEPTAB           TYPE ABAP_FUNC_EXCPBIND_TAB,
        EXCEPTION          TYPE LINE OF ABAP_FUNC_EXCPBIND_TAB,
        LOWERCASE          TYPE ABAP_BOOL VALUE ABAP_FALSE,
        CAMELCASE          TYPE ABAP_BOOL VALUE ABAP_FALSE,
        DABLIU             TYPE STRING,
        EXCEPTHEADER       TYPE STRING,
        ETEXT              TYPE STRING,
        OEXCP              TYPE REF TO CX_ROOT,
        ETEXT2             TYPE STRING,
        STR_ITEM           TYPE STRING,
        HTTP_CODE          TYPE I,
        HTTP_STATUS        TYPE STRING,
        FUNCNAME           TYPE RS38L_FNAM,
        FUNCNAME2          TYPE STRING,
        LC_TP_AMBIENTE     TYPE ZDE_TP_AMBIENTE,
        LC_AMBIENTE        TYPE STRING,
        QS_NVP             TYPE TIHTTPNVP,
        L_IDX              TYPE I,
        WA_ZWST0001        TYPE ZWST0001.

  DATA: LC_ERRO   TYPE STRING.

  DEFINE HTTP_ERROR.

    http_code = &1.
    lc_erro = &3.

    CASE WA_ZWST0001-CD_RECURSO.
      WHEN 'getnfepdf' OR 'getctepdf' OR 'getmdfepdf' OR 'getccenfepdf' OR 'getccectepdf'.
        server->response->set_header_field( name = 'Content-Type'  value = 'text/html; charset=UTF-8' ).
        lc_erro = ZCL_STRING=>CONVERT_ACENTO_HTML( I_TEXTO = lc_erro ).
        server->response->set_cdata( GET_HTML_ERRO( I_STATUS = &1  I_MENSSAGEM = lc_erro ) ).
      WHEN 'getnfexml' OR 'getctexml' OR 'getmdfexml' or 'getDocFiscalStatus'.
        server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
        lc_erro = zcl_string=>TIRA_ACENTOS( lc_erro ).
        lc_erro = zcl_string=>CONVERT_TO_UTF8( lc_erro ).
        concatenate '{"ERROR_CODE":"' &1 '","ERROR_MESSAGE":"' lc_erro '"}' into lc_erro.
        server->response->set_cdata( lc_erro ).
    ENDCASE.

    server->response->set_status( code = http_code  reason = &2 ).
    exit.

  END-OF-DEFINITION.

* Get Server Info:

  SERVER->GET_LOCATION( IMPORTING HOST = DATA(LC_HOST)  PORT = DATA(LC_PORT)  OUT_PROTOCOL = DATA(LC_PROTO) ).
  CONCATENATE LC_PROTO '://' LC_HOST ':' LC_PORT INTO ME->MY_URL.

  ME->MY_SERVICE = SERVER->REQUEST->GET_HEADER_FIELD( NAME = '~script_name' ).

* Get function name from PATH_INFO
  PATH_INFO = SERVER->REQUEST->GET_HEADER_FIELD( NAME = '~path_info' ).
  CK_DOWNLOAD = SERVER->REQUEST->GET_HEADER_FIELD( NAME = 'download' ).
  CAMELCASE = SERVER->REQUEST->GET_FORM_FIELD( 'camelcase' ).
  DABLIU  = SERVER->REQUEST->GET_FORM_FIELD( 'dabliu' ).
  SPLIT PATH_INFO AT '/' INTO TABLE P_INFO_TAB.
  READ TABLE P_INFO_TAB INDEX 2 INTO FUNCNAME.
  READ TABLE P_INFO_TAB INDEX 3 INTO FUNCNAME2.
  IF SY-SUBRC EQ 0.
    CONCATENATE '//' FUNCNAME '/' FUNCNAME2 INTO FUNCNAME.
    CONDENSE FUNCNAME.
  ENDIF.

  SELECT SINGLE * INTO @WA_ZWST0001
    FROM ZWST0001
   WHERE CD_RECURSO EQ @FUNCNAME.

  IF SY-SUBRC IS NOT INITIAL.
    CONCATENATE 'Recurso' FUNCNAME 'não existe' INTO DATA(LC_TEXTO_01) SEPARATED BY SPACE.
    HTTP_ERROR '404' 'Recurso' LC_TEXTO_01 .
  ENDIF.

  DATA: I_INFO TYPE ZDE_INTEGRACAO_HTTP_CONFIG.
  DATA: SERVER_BASE TYPE REF TO CL_HTTP_SERVER.

  SERVER_BASE = CAST #( SERVER ).

  I_INFO-DS_BODY = SERVER->REQUEST->GET_CDATA( ).
  I_INFO-DS_CONTENT_TYPE = SERVER->REQUEST->GET_CONTENT_TYPE( ).
  I_INFO-DS_FORMATO = ZCL_STRING=>UPPER( SERVER->REQUEST->GET_FORM_FIELD( 'format' ) ).
  I_INFO-DS_IP_ORIGEM = SERVER_BASE->C_CALLER_IP.
  I_INFO-DS_METODO = SERVER->REQUEST->GET_METHOD( ).
  I_INFO-DS_SERVER_PROTOCOLO = CONV #( SERVER_BASE->M_PROTOCOL_VERSION ).

* Verificar Acesso se o usuário possui permissão
  IF WA_ZWST0001-CK_NAO_VERIF_PERFIL EQ ABAP_FALSE.
    AUTHORITY-CHECK OBJECT 'ZWS0001' ID 'CD_RECURSO' FIELD FUNCNAME.
    IF SY-SUBRC IS NOT INITIAL.
      CONCATENATE 'Recurso' WA_ZWST0001-DS_RECURSO 'nao disponivel para usuario/servico' INTO DATA(LC_TEXTO_02) SEPARATED BY SPACE.
      HTTP_ERROR '404' 'Recurso' LC_TEXTO_02.
    ENDIF.
  ENDIF.

*01	DESENVOLVIMENTO
*02	Homologação
*03	Produção

  CASE SY-SYSID.
    WHEN 'DEV'.
      LC_TP_AMBIENTE = '01'.
      LC_AMBIENTE = 'Desenvolvimento'.
    WHEN 'QAS'.
      LC_TP_AMBIENTE = '02'.
      LC_AMBIENTE = 'Qualidade'.
    WHEN 'PRD'.
      LC_TP_AMBIENTE = '03'.
      LC_AMBIENTE = 'Produtivo'.
  ENDCASE.

* Verificar se o recurso está cadastrado para o ambiente selecionado
  SELECT SINGLE * INTO @DATA(WA_ZWST0002)
    FROM ZWST0002
   WHERE CD_RECURSO  EQ @FUNCNAME
     AND TP_AMBIENTE EQ @LC_TP_AMBIENTE.

  IF SY-SUBRC IS NOT INITIAL.
    CONCATENATE 'Recurso' WA_ZWST0001-DS_RECURSO 'não disponivel para ambiente de' LC_AMBIENTE INTO DATA(LC_TEXTO_03) SEPARATED BY SPACE.
    HTTP_ERROR '404' 'Recurso' LC_TEXTO_03.
  ENDIF.

* Altera Recurso para buscar o Metodo SAP (Call Function/Objeto etc....)
  CHECK WA_ZWST0002-NM_RECURSO IS NOT INITIAL.

  FUNCNAME = WA_ZWST0002-NM_RECURSO.

  CASE WA_ZWST0001-CD_RECURSO.
    WHEN 'getnfepdf' OR 'getctepdf' OR 'getmdfepdf' OR 'getccenfepdf' OR 'getccectepdf'.
      I_INFO-DS_FORMATO = 'PDF'.
    WHEN 'getnfexml' OR 'getctexml' OR 'getmdfexml'.
      I_INFO-DS_FORMATO = 'XML'.
    WHEN 'getDocFiscalStatus'.
      I_INFO-DS_FORMATO = 'JSON'.
  ENDCASE.

  IF I_INFO-DS_FORMATO NE 'PDF' AND I_INFO-DS_FORMATO NE 'XML' AND I_INFO-DS_FORMATO NE 'JSON'.
    ETEXT = 'Formato ' && I_INFO-DS_FORMATO && ' não previsto!'.
    HTTP_ERROR '500' 'Internal Server Error' ETEXT.
  ENDIF.

  FIELD-SYMBOLS <QS_NVP> TYPE IHTTPNVP.

  SERVER->REQUEST->GET_FORM_FIELDS_CS( CHANGING FIELDS = QS_NVP ).

  IF ( QS_NVP IS NOT INITIAL AND I_CDATA IS INITIAL ) OR
          I_INFO-DS_CONTENT_TYPE CS 'application/x-www-form-urlencoded'.
    DATA(L_LINES) = LINES( QS_NVP ).
    CLEAR L_IDX.
    MOVE '{' TO I_CDATA.
    LOOP AT QS_NVP ASSIGNING <QS_NVP>.
      ADD 1 TO L_IDX.
      TRANSLATE <QS_NVP>-NAME TO UPPER CASE. " ABAP is upper case internally anyway.
      CONCATENATE I_CDATA '"' <QS_NVP>-NAME '":"' <QS_NVP>-VALUE '"' INTO I_CDATA
        RESPECTING BLANKS.
      IF L_IDX < L_LINES.
        CONCATENATE I_CDATA ',' INTO I_CDATA RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.
    CONCATENATE I_CDATA '}' INTO I_CDATA.
  ELSEIF I_CDATA IS NOT INITIAL AND NOT I_INFO-DS_CONTENT_TYPE CS '/JSON'.
    ETEXT = 'Content-Type ' && I_INFO-DS_CONTENT_TYPE && ' não previsto!'.
    HTTP_ERROR '500' 'Internal Server Error' ETEXT.
  ENDIF.

* Prepare params to call function
  CALL METHOD ZCL_FMCALL_HANDLER=>BUILD_PARAMS
    EXPORTING
      FUNCTION_NAME    = FUNCNAME
    IMPORTING
      PARAMS           = T_PARAMS_P
      PARAMTAB         = PARAMTAB
      EXCEPTAB         = EXCEPTAB
    EXCEPTIONS
      INVALID_FUNCTION = 1
      OTHERS           = 2.

  IF SY-SUBRC <> 0.
    CONCATENATE 'Invalid Function. ' SY-MSGID SY-MSGTY SY-MSGNO ': '
            SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
            INTO ETEXT SEPARATED BY '-'.
    HTTP_ERROR '500' 'Server Error' ETEXT.
  ENDIF.

**********************
* Process input data *
**********************
  TRY.
      CALL METHOD ME->JSON_DESERIALIZE     " The classic method using JavaScript (JSON only)
        EXPORTING
          JSON     = I_CDATA
        CHANGING
          PARAMTAB = PARAMTAB.
    CATCH CX_ROOT INTO OEXCP.
      ETEXT = OEXCP->IF_MESSAGE~GET_TEXT( ).
      HTTP_ERROR '500' 'Internal Server Error' ETEXT.
  ENDTRY.

  TRY.
      CALL FUNCTION FUNCNAME
        PARAMETER-TABLE
        PARAMTAB
        EXCEPTION-TABLE
        EXCEPTAB.

    CATCH ZCX_DOC_ELETRONICO INTO DATA(EX_DOC_ELETRONICO).

      MESSAGE ID EX_DOC_ELETRONICO->MSGID TYPE EX_DOC_ELETRONICO->MSGTY
       NUMBER EX_DOC_ELETRONICO->MSGNO
         WITH EX_DOC_ELETRONICO->MSGV1 EX_DOC_ELETRONICO->MSGV2 EX_DOC_ELETRONICO->MSGV3 EX_DOC_ELETRONICO->MSGV4
         INTO ETEXT.

      HTTP_ERROR '500' 'Internal Server Error' ETEXT.

    CATCH CX_ROOT INTO OEXCP.
      ETEXT = OEXCP->IF_MESSAGE~GET_LONGTEXT(  PRESERVE_NEWLINES = ABAP_TRUE ).
      HTTP_ERROR '500' 'Internal Server Error' ETEXT.
  ENDTRY.

* Remove unused exceptions
  DATA(FUNCRC) = SY-SUBRC.
  DELETE EXCEPTAB WHERE VALUE NE FUNCRC.
  READ TABLE EXCEPTAB INTO EXCEPTION WITH KEY VALUE = FUNCRC.
  IF SY-SUBRC EQ 0.
    EXCEPTHEADER = EXCEPTION-NAME.
    CALL METHOD SERVER->RESPONSE->SET_HEADER_FIELD(
        NAME  = 'X-SAPRFC-Exception'
        VALUE = EXCEPTHEADER ).
  ENDIF.

  CASE I_INFO-DS_FORMATO.
    WHEN 'JSON'.

      CALL METHOD ME->SERIALIZE_JSON
        EXPORTING
          PARAMTAB  = PARAMTAB
          EXCEPTAB  = EXCEPTAB
          PARAMS    = T_PARAMS_P
          JSONP     = JSONP_CALLBACK
          SHOW_IMPP = SHOW_IMPORT_PARAMS
          LOWERCASE = COND #( WHEN DABLIU IS NOT INITIAL THEN ABAP_TRUE ELSE ABAP_FALSE )
          CAMELCASE = CAMELCASE
        IMPORTING
          O_STRING  = O_CDATA.

      SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/json; charset=UTF-8' ).
      IF JSONP_CALLBACK IS NOT INITIAL.
        SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/javascript' ).
      ENDIF.
      SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'X-Data-Format' VALUE = 'JSON' ).
      SERVER->RESPONSE->SET_COMPRESSION( ).
      SERVER->RESPONSE->SET_CDATA( DATA = O_CDATA ).

    WHEN 'PDF'.

      CALL METHOD ME->SERIALIZE_PDF
        EXPORTING
          PARAMTAB  = PARAMTAB
          EXCEPTAB  = EXCEPTAB
          PARAMS    = T_PARAMS_P
          JSONP     = JSONP_CALLBACK
          SHOW_IMPP = SHOW_IMPORT_PARAMS
          LOWERCASE = LOWERCASE
          CAMELCASE = CAMELCASE
        IMPORTING
          O_XSTRING = O_DATA
          O_NAME    = O_NAME.

      IF CK_DOWNLOAD IS INITIAL.
        DATA(L_PDF_LEN) = XSTRLEN( O_DATA ).
        SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/pdf' ).
        SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'X-Data-Format' VALUE = CONV #( I_INFO-DS_FORMATO ) ).
        SERVER->RESPONSE->SET_COMPRESSION( ).
        "O_CDATA = ZCL_STRING=>XSTRING_TO_STRING( O_DATA ).
        "SERVER->RESPONSE->SET_CDATA( DATA = O_CDATA ).
        SERVER->RESPONSE->SET_DATA( DATA = O_DATA LENGTH = L_PDF_LEN ).
      ELSE.
        L_PDF_LEN = XSTRLEN( O_DATA ).
        CONCATENATE '"' O_NAME '"' INTO O_NAME.
        CONCATENATE 'attachment; filename = ' O_NAME INTO O_AUX SEPARATED BY SPACE.
        SERVER->RESPONSE->SET_HEADER_FIELDS( FIELDS = VALUE #( ( NAME = IF_HTTP_HEADER_FIELDS=>CONTENT_TYPE  VALUE = 'application/pdf' )
                                                               ( NAME = IF_HTTP_HEADER_FIELDS=>CONTENT_TRANSFER_ENCODING VALUE = 'Binary' )
                                                               ( NAME = 'Content-ID' VALUE = O_NAME )
                                                               ( NAME = IF_HTTP_HEADER_FIELDS=>CONTENT_LENGTH VALUE = CONV #( L_PDF_LEN ) )
                                                               ( NAME = IF_HTTP_HEADER_FIELDS=>CONTENT_DISPOSITION  VALUE = O_AUX  ) ) ).
        SERVER->RESPONSE->DELETE_HEADER_FIELD( IF_HTTP_HEADER_FIELDS=>CACHE_CONTROL ).
        SERVER->RESPONSE->DELETE_HEADER_FIELD( IF_HTTP_HEADER_FIELDS=>EXPIRES ).
        SERVER->RESPONSE->DELETE_HEADER_FIELD( IF_HTTP_HEADER_FIELDS=>PRAGMA ).
        SERVER->RESPONSE->SET_DATA( DATA = O_DATA LENGTH = L_PDF_LEN ).
      ENDIF.

    WHEN 'XML'.

      CALL METHOD ME->SERIALIZE_XML_DOC
        EXPORTING
          PARAMTAB  = PARAMTAB
          EXCEPTAB  = EXCEPTAB
          PARAMS    = T_PARAMS_P
          JSONP     = JSONP_CALLBACK
          SHOW_IMPP = SHOW_IMPORT_PARAMS
          LOWERCASE = LOWERCASE
          CAMELCASE = CAMELCASE
        IMPORTING
          O_STRING  = O_CDATA
          O_XSTRING = O_DATA
          O_NAME    = O_NAME.

      IF CK_DOWNLOAD IS INITIAL.
        SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/xml' ).
        SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'X-Data-Format' VALUE = CONV #( I_INFO-DS_FORMATO ) ).
        SERVER->RESPONSE->SET_COMPRESSION( ).
        SERVER->RESPONSE->SET_CDATA( DATA = O_CDATA ).
      ELSE.
        L_PDF_LEN = XSTRLEN( O_DATA ).
        CONCATENATE '"' O_NAME '"' INTO O_NAME.
        CONCATENATE 'attachment; filename = ' O_NAME INTO O_AUX SEPARATED BY SPACE.
        SERVER->RESPONSE->SET_HEADER_FIELDS( FIELDS = VALUE #( ( NAME = IF_HTTP_HEADER_FIELDS=>CONTENT_TYPE  VALUE = 'application/xml' )
                                                               ( NAME = IF_HTTP_HEADER_FIELDS=>CONTENT_TRANSFER_ENCODING VALUE = 'Binary' )
                                                               ( NAME = 'Content-ID' VALUE = O_NAME )
                                                               ( NAME = IF_HTTP_HEADER_FIELDS=>CONTENT_LENGTH VALUE = CONV #( L_PDF_LEN ) )
                                                               ( NAME = IF_HTTP_HEADER_FIELDS=>CONTENT_DISPOSITION  VALUE = O_AUX  ) ) ).
        SERVER->RESPONSE->DELETE_HEADER_FIELD( IF_HTTP_HEADER_FIELDS=>CACHE_CONTROL ).
        SERVER->RESPONSE->DELETE_HEADER_FIELD( IF_HTTP_HEADER_FIELDS=>EXPIRES ).
        SERVER->RESPONSE->DELETE_HEADER_FIELD( IF_HTTP_HEADER_FIELDS=>PRAGMA ).
        SERVER->RESPONSE->SET_DATA( DATA = O_DATA LENGTH = L_PDF_LEN ).
      ENDIF.

  ENDCASE.

ENDMETHOD.


method JSON2ABAP.
*/************************************************/*
*/ Input any abap data and this method tries to   /*
*/ fill it with the data in the JSON string.      /*
*/  Thanks to Juan Diaz for helping here!!        /*
*/************************************************/*

  type-pools: abap, js.

  data:
    js_script         type string,
    js_started        type i value 0,
    l_json_string     type string,
    js_property_table type   js_property_tab,
    js_property       type line of js_property_tab,
    l_property_path   type string,
    compname          type string,
    item_path         type string.

  data:
    l_type   type c,
    l_value  type string,
    linetype type string,
    l_comp   type line of ABAP_COMPDESCR_TAB.

  data:
    datadesc type ref to CL_ABAP_TYPEDESCR,
    drefdesc type ref to CL_ABAP_TYPEDESCR,
    linedesc type ref to CL_ABAP_TYPEDESCR,
    strudesc type ref to CL_ABAP_STRUCTDESCR,
    tabldesc type ref to CL_ABAP_TABLEDESCR.

  data newline type ref to data.

  field-symbols:
    <abap_data> type any,
    <itab>      type any table,
    <comp>      type any,
    <jsprop>    type line of js_property_tab,
    <abapcomp>  type abap_compdescr.


  define assign_scalar_value.
    "   &1   <abap_data>
    "   &2   js_property-value
    describe field &1 type l_type.
    l_value = &2.
* convert or adapt scalar values to ABAP.
    case l_type.
      when 'D'. " date type
        if l_value cs '-'.
          replace all occurrences of '-' in l_value with space.
          condense l_value no-gaps.
        endif.
      when 'T'. " time type
        if l_value cs ':'.
          replace all occurrences of ':' in l_value with space.
          condense l_value no-gaps.
        endif.
      when others.
        " may be other conversions or checks could be implemented here.
    endcase.
    &1 = l_value.
  end-of-definition.


  if js_object is not bound.

    if json_string is initial. exit. endif. " exit method if there is nothing to parse

    l_json_string = json_string.
    " js_object = cl_java_script=>create( STACKSIZE = 16384 ).
    js_object = cl_java_script=>create( STACKSIZE = 16384 HEAPSIZE = 960000 ).

***************************************************
*  Parse JSON using JavaScript                    *
***************************************************
    js_object->bind( exporting name_obj = 'abap_data' name_prop = 'json_string'    changing data = l_json_string ).
    js_object->bind( exporting name_obj = 'abap_data' name_prop = 'script_started' changing data = js_started ).

* We use the JavaScript engine included in ABAP to read the JSON string.
* We simply use the recommended way to eval a JSON string as specified
* in RFC 4627 (http://www.ietf.org/rfc/rfc4627.txt).
*
* Security considerations:
*
*   Generally there are security issues with scripting languages.  JSON
*   is a subset of JavaScript, but it is a safe subset that excludes
*   assignment and invocation.
*
*   A JSON text can be safely passed into JavaScript's eval() function
*   (which compiles and executes a string) if all the characters not
*   enclosed in strings are in the set of characters that form JSON
*   tokens.  This can be quickly determined in JavaScript with two
*   regular expressions and calls to the test and replace methods.
*
*      var my_JSON_object = !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(
*             text.replace(/"(\\.|[^"\\])*"/g, ''))) &&
*         eval('(' + text + ')');

    concatenate

         'var json_obj; '
         'var json_text; '

         'function start() { '
         '  if(abap_data.script_started) { return; } '
         '  json_text = abap_data.json_string;'
         '  json_obj = !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test( '
         '      json_text.replace(/"(\\.|[^"\\])*"/g, ''''))) && '
         '    eval(''('' + json_text + '')''); '
         '  abap_data.script_started = 1; '
         '} '

         'if(!abap_data.script_started) start(); '


       into js_script respecting blanks separated by xnl.

    js_object->compile( script_name = 'json_parser'     script = js_script ).
    js_object->execute( script_name = 'json_parser' ).

    if js_object->last_error_message is not initial.
      RAISE EXCEPTION type ZCX_JSON
        EXPORTING
          message = js_object->last_error_message.
    endif.

  endif.
** End of JS processing.

**
  if var_name is not initial.
    concatenate property_path var_name into l_property_path separated by '.'.
  else.
    l_property_path = property_path.
  endif.
**
**
  js_property_table = js_object->get_properties_scope_global( property_path = l_property_path ).
  property_table = js_property_table.

* Exit if abap_data is not supplied, normally when called
* from json_deserialize to get top level properties
  if abap_data is not supplied.
    exit.
  endif. "***

*
* Get ABAP data type, dereference if necessary and start
  datadesc = cl_abap_typedescr=>DESCRIBE_BY_DATA( abap_data ).
  if datadesc->kind eq cl_abap_typedescr=>kind_ref.
    assign abap_data->* to <abap_data>.
  else.
    assign abap_data to <abap_data>.
  endif.
  datadesc = cl_abap_typedescr=>DESCRIBE_BY_DATA( <abap_data> ).


  case datadesc->kind.

    when cl_abap_typedescr=>kind_elem.
* Scalar: process ABAP elements. Assume no type conversions for the moment.
      if var_name is initial.
        RAISE EXCEPTION type ZCX_JSON
          EXPORTING
            message = 'VAR_NAME is required for scalar values.'.
      endif.
      js_property_table = js_object->get_properties_scope_global( property_path = property_path ).
      read table js_property_table with key name = var_name into js_property.
      if sy-subrc eq 0.
        assign_scalar_value <abap_data> js_property-value.
      endif.


    when cl_abap_typedescr=>kind_struct.
* Process ABAP structures
      strudesc ?= datadesc.
      loop at js_property_table assigning <jsprop>.
        compname = <jsprop>-name.
        translate compname to upper case.
        read table strudesc->COMPONENTS with key name = compname into l_comp.
        if sy-subrc eq 0.
          assign component l_comp-name of structure <abap_data> to <comp>.
          case l_comp-type_kind.
            when    cl_abap_typedescr=>TYPEKIND_STRUCT1  " 'v'
                 or cl_abap_typedescr=>TYPEKIND_STRUCT2  " 'u'
                 or cl_abap_typedescr=>TYPEKIND_TABLE.   " 'h' (may need a different treatment one day)
              concatenate l_property_path <jsprop>-name into item_path separated by '.'.
*> Recursive call here
              json2abap( exporting property_path = item_path changing abap_data = <comp> js_object = js_object ).

            when others.
* Process scalars in structures (same as the kind_elem above)
              assign_scalar_value <comp> <jsprop>-value.

          endcase.
        endif.
      endloop.

    when cl_abap_typedescr=>kind_table.
* Process ABAP tables
      if js_property_table is not initial.
        tabldesc ?= datadesc.
        linedesc = tabldesc->get_table_line_type( ).
        linetype = linedesc->get_relative_name( ).
        assign <abap_data> to <itab>.
        loop at js_property_table into js_property where name NE 'length'. " the JS object length
          create data newline type (linetype).
          assign newline->* to <comp>.
          case js_property-kind.
            when 'O'.
              concatenate l_property_path js_property-name into item_path separated by '.'.
              condense item_path.
*> Recursive call here
              json2abap( exporting property_path = item_path changing abap_data = newline js_object = js_object ).
            when others. " Assume scalars, 'S', 'I', or other JS types
              " Process scalars in plain table components(same as the kind_elem above)
              assign_scalar_value <comp> js_property-value.
          endcase.
          insert <comp> into table <itab>.
          free newline.
        endloop.
      endif.

    when others. " kind_class, kind_intf
      " forget it.

  endcase.


endmethod.


method JSON_DESERIALIZE.

  type-pools: ABAP, JS.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  data paramname   type string.
  data js_obj      type ref to cl_java_script.
  data js_prop_tab type js_property_tab.

  field-symbols <js_prop> type line of js_property_tab.
  field-symbols <parm>    type abap_func_parmbind.

  if json is initial. exit. endif.

  json2abap( exporting json_string = json  importing property_table = js_prop_tab  changing js_object = js_obj ).

  loop at js_prop_tab assigning <js_prop>.
    paramname = <js_prop>-name.
    translate paramname to upper case.
    read table paramtab with key name = paramname assigning <parm>.
    if sy-subrc eq 0.
      if <parm>-kind ne abap_func_importing. "" va al revés, cuidado!!!
        json2abap( exporting var_name = <js_prop>-name  changing abap_data = <parm>-value js_object = js_obj ).
      endif.
    endif.
  endloop.

endmethod.


  METHOD LOGIN.

    I_SERVER->GET_XSRF_TOKEN( IMPORTING TOKEN = E_OUT_LOGIN-TOKEN
      EXCEPTIONS
        INTERNAL_ERROR           = 1
        CALLED_BY_PUBLIC_SERVICE = 2
        OTHERS                   = 3 ).

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    E_OUT_LOGIN-SESSION_ID = I_SERVER->SESSION_ID.

    SELECT SINGLE * INTO @DATA(WA_ZHCMT0007)
      FROM ZHCMT0007
     WHERE BNAME EQ @SY-UNAME.

    IF SY-SUBRC IS INITIAL.
      E_OUT_LOGIN-CPF          = WA_ZHCMT0007-CPF_NR.
      E_OUT_LOGIN-DEPARTAMENTO = WA_ZHCMT0007-DEPARTAMENTO.
      E_OUT_LOGIN-FUNCAO       = WA_ZHCMT0007-FUNCAO.
      E_OUT_LOGIN-NOME         = WA_ZHCMT0007-CNAME.
    ENDIF.

    E_JSON = '{' &&
                 '"e_sucesso" : "SIM",' &&
                 '"e_token" : "' && E_OUT_LOGIN-TOKEN && '",' &&
                 '"e_nome" : "' && E_OUT_LOGIN-NOME && '",' &&
                 '"e_departamento" : "' && E_OUT_LOGIN-DEPARTAMENTO && '",' &&
                 '"e_funcao" : "' && E_OUT_LOGIN-FUNCAO && '",' &&
                 '"e_cpf" : "' && E_OUT_LOGIN-CPF && '"' &&
              '}'.

    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'x-csrf-token'  VALUE = E_OUT_LOGIN-TOKEN ).
    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Session_Id'    VALUE = E_OUT_LOGIN-SESSION_ID ).
    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/json; charset=UTF-8' ).

  ENDMETHOD.


  METHOD LOGOFF.

    I_SERVER->LOGOFF(
      EXCEPTIONS
        LOGOFF_NOT_POSSIBLE       = 1
        OTHERS                    = 2
    ).

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

  ENDMETHOD.


method NOTES.

data location type string.

concatenate me->my_url me->my_service '/RFC_SYSTEM_INFO' into location.

concatenate

'<html><head><title>JSON (NEW) handler notes</title></head><body>'

'<h4>About this service...</h4>'
'This is the ABAP implementation of a conversion program that'
' tranforms ABAP data into a <a href="http://www.json.org">JSON</a> representation.'
'<p>'
'It provides a user interface in the form of a ICF service that '
'allows web invocation of ABAP function modules. It doesn''t matter if they are RFC enabled or not.'
'<p>In this system this service has '
'been assigned to ICF service <a href="' me->my_url me->my_service '">' me->my_service '</a>.'
'<p>'
'In order to invoke a function module, just put its name in the PATH_INFO '
'of the service URL, as is shown in the following examples.'

'<p>Try the following link to do the default call in JSON format:<pre><a href="' location '?format=json">'
location
'?format=json</a></pre>'

'<p>A simple syntax allows to get the output in different formats.<p>'

'The following gets the output in <a href="http://yaml.org">YAML</a> format:'
'<pre><a href="' location '?format=yaml">'
location
'?format=yaml</a></pre>'
''
'<p>And this will get the output in a basic XML representation: <pre><a href="' location '?format=xml">'
location
'?format=xml</a></pre>'

'<p>And, just for fun, getting it into Perl format could be handy: <pre><a href="' location '?format=perl">'
location
'?format=perl</a></pre>'

'<p>Finnally, you can add a callback to get the JSON response enclosed in a javascript function call,'
' in order to allow a <a href="http://en.wikipedia.org/wiki/JSONP">JSONP</a> style response: '
'<pre><a href="'
location '?format=json&callback=callMe">'
location '?format=json&callback=callMe</a></pre>'

'<hr><h4>WARNING</h4>This is work in progress and may not be suitable for use in productive '
'systems. The interface is somewhat unstable. Please feel free to test it and report  '
'any bug and improvement you may find.'
'<p>Use it at your own risk!'
'<p>For more information: <a href="https://cw.sdn.sap.com/cw/groups/json-adapter-for-abap-function-modules">'
'https://cw.sdn.sap.com/cw/groups/json-adapter-for-abap-function-modules</a>'
'<p>'
'If you have any questions, please contact me at <a href="mailto:cesar.martin@sap.com">'
'cesar.martin@sap.com</a>'
'<p>'


'<hr></body></html>'


into text RESPECTING BLANKS.


endmethod.


method SERIALIZE_ID.
*/***********************************************************/*
*/ New method using the built-in transformation              /*
*/ included in releases 7.02 and 7.03/7.31 (Kernelpatch 116) /*
*/ Generates both JSON and XML formats!!
*/***********************************************************/*
*/
** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  type-pools: ABAP.

  data:
    stab type ABAP_TRANS_SRCBIND_TAB,
    slin type ABAP_TRANS_SRCBIND,
    oexcp type ref to cx_root,
    etext type string,
    adata type ref to data,
    json_writer type ref to cl_sxml_string_writer.

  field-symbols <parm> type abap_func_parmbind.
*  field-symbols <excep> type abap_func_excpbind.


  loop at paramtab assigning <parm>.
    if show_impp ne 'X'
          and <parm>-kind eq abap_func_exporting. "" va al revés, cuidado!!!
      continue.
    endif.
    slin-name  = <parm>-name.
    slin-value = <parm>-value.
    append slin to stab. clear slin.
  endloop.

  if exceptab is not initial.
    slin-name  = 'EXCEPTION'.
    get reference of exceptab into adata.
    slin-value = adata.
    append slin to stab. clear slin.
  endif.


  json_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

  try.

      case format.

        when 'XML'.

          call transformation id options data_refs = 'embedded'
                                         initial_components = 'include'
                                 source (stab)
                                 result xml o_string.


        when others.

          call transformation id options data_refs = 'embedded'
                                         initial_components = 'include'
                                 source (stab)
                                 result xml json_writer.

          o_string = cl_abap_codepage=>convert_from( json_writer->get_output( ) ).
*  json_string = json_writer->get_output( ).

          if jsonp is not initial.
            concatenate jsonp '(' o_string ');' into o_string.
          endif.

      endcase.


    catch cx_root into oexcp.

      etext = oexcp->if_message~get_text( ).
      RAISE EXCEPTION type ZCX_JSON
        EXPORTING
          message = etext.

  endtry.


endmethod.


method SERIALIZE_JSON.
* ABAP based JSON serializer for function modules (January 2013).
  type-pools: ABAP.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  data json_fragments type table of string.
  data rec_json_string type string.
  data paramname type string.
  data l_lines type i.
  data l_index type i.
  data upcase type xfeld value 'X'.
  field-symbols <parm> type abap_func_parmbind.
  field-symbols <excep> type abap_func_excpbind.

  if jsonp is not initial.
    append jsonp to json_fragments.
    append '(' to json_fragments.
  endif.

  rec_json_string = '{'.
  append rec_json_string to json_fragments.
  clear rec_json_string.

  clear l_index.
  l_lines = lines( paramtab ).

  loop at paramtab assigning <parm>.
    if show_impp ne 'X'
          and <parm>-kind eq abap_func_exporting. "" va al revés, cuidado!!!
      subtract 1 from l_lines.
      continue.
    endif.
    add 1 to l_index.
    paramname = <parm>-name.
    if lowercase eq abap_true.
      translate paramname to lower case.
      " paramname = to_lower( paramname ).
      upcase = space.
    endif.
    if camelcase eq abap_true.
      paramname = to_mixed( val = paramname  case = 'a').
    endif.
    rec_json_string = abap2json( abap_data = <parm>-value  name = paramname  upcase = upcase camelcase = camelcase ).
    append rec_json_string to json_fragments.
    clear rec_json_string.
    if l_index < l_lines.
      append ',' to json_fragments .
    endif .
  endloop.

  if exceptab is not initial.
    if l_lines gt 0.
      append ',' to json_fragments.
    endif.
    rec_json_string = abap2json( abap_data = exceptab upcase = 'X' name = 'EXCEPTION').
    append rec_json_string to json_fragments.
    clear rec_json_string.
  endif.

  rec_json_string = '}'.
  append rec_json_string to json_fragments.
  clear rec_json_string.

  if jsonp is not initial.
    append ');' to json_fragments.
  endif.

  concatenate lines of json_fragments into o_string.

endmethod.


METHOD SERIALIZE_PDF.

  TYPE-POOLS: ABAP.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  DATA REC_PDF_XSTRING TYPE XSTRING.
  DATA PARAMNAME TYPE STRING.
  DATA L_LINES TYPE I.
  DATA L_INDEX TYPE I.
  DATA UPCASE TYPE XFELD VALUE 'X'.

  FIELD-SYMBOLS: <ABAP_DATA> TYPE ANY.

  FIELD-SYMBOLS <PARM> TYPE ABAP_FUNC_PARMBIND.
  FIELD-SYMBOLS <EXCEP> TYPE ABAP_FUNC_EXCPBIND.

  DATA: L_TYPE  TYPE C,
        L_COMPS TYPE I.

  CLEAR: REC_PDF_XSTRING.

  CLEAR L_INDEX.
  L_LINES = LINES( PARAMTAB ).

  LOOP AT PARAMTAB ASSIGNING <PARM>.

    IF SHOW_IMPP NE 'X' AND <PARM>-KIND EQ ABAP_FUNC_EXPORTING. "" va al revés, cuidado!!!
      SUBTRACT 1 FROM L_LINES.
      CONTINUE.
    ENDIF.

    ADD 1 TO L_INDEX.
    PARAMNAME = <PARM>-NAME.

    IF LOWERCASE EQ ABAP_TRUE.
      TRANSLATE PARAMNAME TO LOWER CASE.
      UPCASE = SPACE.
    ENDIF.

    IF CAMELCASE EQ ABAP_TRUE.
      PARAMNAME = TO_MIXED( VAL = PARAMNAME  CASE = 'a').
    ENDIF.

    DESCRIBE FIELD <PARM>-VALUE TYPE L_TYPE COMPONENTS L_COMPS.

    CASE L_TYPE.
      WHEN CL_ABAP_TYPEDESCR=>TYPEKIND_XSTRING OR CL_ABAP_TYPEDESCR=>TYPEKIND_DREF.

        CASE <PARM>-NAME.
          WHEN 'E_NAME'.
            ASSIGN <PARM>-VALUE->* TO <ABAP_DATA>.
            O_NAME = <ABAP_DATA>.
          WHEN OTHERS.
            REC_PDF_XSTRING = ABAP2PDF( ABAP_DATA = <PARM>-VALUE  NAME = PARAMNAME ).
            O_XSTRING = REC_PDF_XSTRING.
        ENDCASE.

      WHEN CL_ABAP_TYPEDESCR=>TYPEKIND_CHAR.
        ASSIGN <PARM>-VALUE TO <ABAP_DATA>.
        O_NAME = <ABAP_DATA>.
    ENDCASE.

  ENDLOOP.


ENDMETHOD.


method SERIALIZE_PERL.
* Just for fun, generate data in Perl Data::Dumper format.

  type-pools: ABAP.

**Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  data perl_fragments type table of string.
  data rec_perl_string type string.
  data paramname type string.
  data l_lines type i.
  data l_index type i.
  data upcase type xfeld value 'X'.
  data perl_var type string.
  field-symbols <parm> type abap_func_parmbind.
  field-symbols <excep> type abap_func_excpbind.

  if jsonp is not initial.
    perl_var = jsonp.
  else.
    perl_var = funcname.
  endif.
  concatenate '$' perl_var ' = {' into rec_perl_string.
  append rec_perl_string to perl_fragments.
  clear rec_perl_string.

  clear l_index.
  l_lines = lines( paramtab ).

  loop at paramtab assigning <parm>.
    if show_impp ne 'X'
          and <parm>-kind eq abap_func_exporting. "" va al revés, cuidado!!!
      subtract 1 from l_lines.
      continue.
    endif.
    add 1 to l_index.
    paramname = <parm>-name.
    if lowercase eq abap_true.
      translate paramname to lower case.
      upcase = space.
    endif.
    rec_perl_string = abap2perl( abap_data = <parm>-value  name = paramname  upcase = upcase ).
    append rec_perl_string to perl_fragments.
    clear rec_perl_string.
    if l_index < l_lines.
      append ',' to perl_fragments .
    endif .
  endloop.

  if exceptab is not initial.
    if l_lines gt 0.
      append ',' to perl_fragments.
    endif.
    rec_perl_string = abap2perl( abap_data = exceptab upcase = 'X' name = 'EXCEPTION').
    append rec_perl_string to perl_fragments.
    clear rec_perl_string.
  endif.

  rec_perl_string = '};'.
  append rec_perl_string to perl_fragments.
  clear rec_perl_string.

  concatenate lines of perl_fragments into perl_string.

endmethod.


method SERIALIZE_XML.
* serialize function data into simple xml
*/ look at method serialize_id for a new way of doing xml.

  type-pools: abap.

** remember function parameter types
***constants:
***  abap_func_exporting type abap_func_parmbind-kind value 10,
***  abap_func_importing type abap_func_parmbind-kind value 20,
***  abap_func_tables    type abap_func_parmbind-kind value 30,
***  abap_func_changing  type abap_func_parmbind-kind value 40.

  data rec_xml_string type string.
  data xml_fragments type table of string.
  data l_funcname type string.
  data paramname type string.
  field-symbols <parm> type abap_func_parmbind.
  field-symbols <excep> type abap_func_excpbind.
  data upcase type xfeld value 'x'.

  constants:
     xml_head type string value '<?xml version="1.0" encoding="utf-8"?>'.

  append xml_head to xml_fragments.

  l_funcname = funcname.
  if lowercase eq abap_true.
    translate l_funcname to lower case.
    upcase = space.
  endif.

  concatenate '<' l_funcname '>' into rec_xml_string.
  append rec_xml_string to xml_fragments.

  loop at paramtab assigning <parm>.
    if show_impp ne 'x'
          and <parm>-kind eq abap_func_exporting. "" va al revés, cuidado!!!
      continue.
    endif.
    paramname = <parm>-name.
    if lowercase eq abap_true.
      translate paramname to lower case.
    endif.
    rec_xml_string = abap2xml( name = paramname abap_data = <parm>-value upcase = upcase ).
    append rec_xml_string to xml_fragments.
  endloop.

  if exceptab is not initial.
    rec_xml_string = abap2xml( name = 'exception' abap_data = exceptab  upcase = upcase ).
    append rec_xml_string to xml_fragments.
  endif.

  concatenate '</' l_funcname '>' into rec_xml_string.
  append rec_xml_string to xml_fragments.

  concatenate lines of xml_fragments into o_string.

endmethod.


METHOD SERIALIZE_XML_DOC.

  TYPE-POOLS: ABAP.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  DATA REC_XML_XSTRING TYPE XSTRING.
  DATA PARAMNAME TYPE STRING.
  DATA L_LINES TYPE I.
  DATA L_INDEX TYPE I.
  DATA UPCASE TYPE XFELD VALUE 'X'.

  FIELD-SYMBOLS: <ABAP_DATA> TYPE ANY.

  FIELD-SYMBOLS <PARM> TYPE ABAP_FUNC_PARMBIND.
  FIELD-SYMBOLS <EXCEP> TYPE ABAP_FUNC_EXCPBIND.

  DATA: L_TYPE  TYPE C,
        L_COMPS TYPE I.

  CLEAR: REC_XML_XSTRING.

  CLEAR L_INDEX.
  L_LINES = LINES( PARAMTAB ).

  LOOP AT PARAMTAB ASSIGNING <PARM>.

    IF SHOW_IMPP NE 'X' AND <PARM>-KIND EQ ABAP_FUNC_EXPORTING. "" va al revés, cuidado!!!
      SUBTRACT 1 FROM L_LINES.
      CONTINUE.
    ENDIF.

    ADD 1 TO L_INDEX.
    PARAMNAME = <PARM>-NAME.

    IF LOWERCASE EQ ABAP_TRUE.
      TRANSLATE PARAMNAME TO LOWER CASE.
      UPCASE = SPACE.
    ENDIF.

    IF CAMELCASE EQ ABAP_TRUE.
      PARAMNAME = TO_MIXED( VAL = PARAMNAME  CASE = 'a').
    ENDIF.

    DESCRIBE FIELD <PARM>-VALUE TYPE L_TYPE COMPONENTS L_COMPS.

    CASE L_TYPE.
      WHEN CL_ABAP_TYPEDESCR=>TYPEKIND_XSTRING OR CL_ABAP_TYPEDESCR=>TYPEKIND_DREF.

        CASE <PARM>-NAME.
          WHEN 'E_NAME'.
            ASSIGN <PARM>-VALUE->* TO <ABAP_DATA>.
            O_NAME = <ABAP_DATA>.
          WHEN OTHERS.
            REC_XML_XSTRING = ABAP2PDF( ABAP_DATA = <PARM>-VALUE  NAME = PARAMNAME ).
            O_STRING = ZCL_STRING=>XSTRING_TO_STRING( I_XSTRING = REC_XML_XSTRING ).
            O_XSTRING = REC_XML_XSTRING.
        ENDCASE.

      WHEN CL_ABAP_TYPEDESCR=>TYPEKIND_CHAR.
        ASSIGN <PARM>-VALUE TO <ABAP_DATA>.
        O_NAME = <ABAP_DATA>.
    ENDCASE.

  ENDLOOP.


ENDMETHOD.


method SERIALIZE_YAML.
* Now, go and represent function data in YAML (http://yaml.org)

  type-pools: ABAP.
** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  data yaml_fragments type table of string.
  data rec_yaml_string type string.
  data rec_yaml_table type table of string.
  data paramname type string.
  field-symbols <parm> type abap_func_parmbind.
  field-symbols <excep> type abap_func_excpbind.
  data upcase type xfeld value 'X'.
  data yaml_head type string value '--- #YAML:1.0'.

  concatenate yaml_head xnl into rec_yaml_string.
  append rec_yaml_string to yaml_fragments.
  clear rec_yaml_string.

  loop at paramtab assigning <parm>.
    if show_impp ne 'X'
          and <parm>-kind eq abap_func_exporting. "" va al revés, cuidado!!!
      continue.
    endif.
    paramname = <parm>-name.
    if lowercase eq abap_true.
       translate paramname to lower case.
       upcase = space.
    endif.
    rec_yaml_string = abap2yaml( abap_data = <parm>-value  name = paramname upcase = upcase ).
    append rec_yaml_string to yaml_fragments.
    clear rec_yaml_string.
  endloop.

  if exceptab is not initial.
    rec_yaml_string = abap2yaml( abap_data = exceptab name = 'EXCEPTION' upcase = 'X' ).
    append rec_yaml_string to yaml_fragments.
    clear rec_yaml_string.
  endif.

*  append xnl to yaml_fragments.

  concatenate lines of yaml_fragments into yaml_string.

*  if jsonp is not initial.
*     concatenate jsonp '(' yaml_string ');' into yaml_string.
*  endif.

endmethod.


  METHOD VALIDA_TOKEN.

    E_OUT_LOGIN-TOKEN = I_SERVER->REQUEST->GET_HEADER_FIELD( 'token' ).

    I_SERVER->VALIDATE_XSRF_TOKEN(
      EXPORTING
        TOKEN                    = E_OUT_LOGIN-TOKEN
      IMPORTING
        SUCCESSFUL               = DATA(LC_SUCCESSFUL)
      EXCEPTIONS
        TOKEN_NOT_FOUND          = 1
        COOKIE_NOT_FOUND         = 2
        INTERNAL_ERROR           = 3
        CALLED_BY_PUBLIC_SERVICE = 4
        OTHERS                   = 5 ).

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    IF LC_SUCCESSFUL EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_TOKEN-MSGID MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_TOKEN-MSGNO )
          MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_TOKEN-MSGID
          MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_TOKEN-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    E_OUT_LOGIN-SESSION_ID = I_SERVER->SESSION_ID.

    SELECT SINGLE * INTO @DATA(WA_ZHCMT0007)
      FROM ZHCMT0007
     WHERE BNAME EQ @SY-UNAME.

    IF SY-SUBRC IS INITIAL.
      E_OUT_LOGIN-CPF          = WA_ZHCMT0007-CPF_NR.
      E_OUT_LOGIN-DEPARTAMENTO = WA_ZHCMT0007-DEPARTAMENTO.
      E_OUT_LOGIN-FUNCAO       = WA_ZHCMT0007-FUNCAO.
      E_OUT_LOGIN-NOME         = WA_ZHCMT0007-CNAME.
    ENDIF.

    E_JSON = '{' &&
                 '"e_sucesso" : "SIM",' &&
                 '"e_token" : "' && E_OUT_LOGIN-TOKEN && '",' &&
                 '"e_nome" : "' && E_OUT_LOGIN-NOME && '",' &&
                 '"e_departamento" : "' && E_OUT_LOGIN-DEPARTAMENTO && '",' &&
                 '"e_funcao" : "' && E_OUT_LOGIN-FUNCAO && '",' &&
                 '"e_cpf" : "' && E_OUT_LOGIN-CPF && '"' &&
              '}'.

    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'x-csrf-token' VALUE = E_OUT_LOGIN-TOKEN ).
    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Session_Id'   VALUE = I_SERVER->SESSION_ID ).
    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/json; charset=UTF-8' ).

  ENDMETHOD.
ENDCLASS.
