class ZCL_FMCALL_PM_MOBILE definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools JS .

*"* public components of class ZCL_FMCALL_PM_MOBILE
*"* do not include other source files here!!!
  interfaces IF_HTTP_EXTENSION .
  interfaces ZIF_FMCALL_APP_MOBILE .

  constants XNL type ABAP_CHAR1 value %_NEWLINE ##NO_TEXT.
  constants XCRLF type ABAP_CR_LF value %_CR_LF ##NO_TEXT.
  data MY_SERVICE type STRING .
  data MY_URL type STRING .
  class-data ONLINE type CHAR1 .

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
protected section.
private section.
ENDCLASS.



CLASS ZCL_FMCALL_PM_MOBILE IMPLEMENTATION.


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


METHOD if_http_extension~handle_request.

  DATA: path_info          TYPE string,
        p_info_tab         TYPE TABLE OF string,
        action             TYPE string,
        "I_CONTENT_TYPE  TYPE STRING,
        i_cdata            TYPE string,
        o_cdata            TYPE string,
        o_name             TYPE string,
        o_aux              TYPE string,
        lc_format          TYPE string,
        o_data             TYPE xstring,
        jsonp_callback     TYPE string,
        show_import_params TYPE abap_bool VALUE abap_false,
        t_params_p         TYPE STANDARD TABLE OF rfc_fint_p,
        paramtab           TYPE abap_func_parmbind_tab,
        exceptab           TYPE abap_func_excpbind_tab,
        lowercase          TYPE abap_bool VALUE abap_false,
        camelcase          TYPE abap_bool VALUE abap_false,
        exceptheader       TYPE string,
        etext              TYPE string,
        oexcp              TYPE REF TO cx_root,
        etext2             TYPE string,
        str_item           TYPE string,
        http_code          TYPE i,
        http_status        TYPE string,
        funcname           TYPE rs38l_fnam,
        funcname2          TYPE string.

  DATA: lc_erro   TYPE string.

  DEFINE http_error.

    lc_erro = &3.
    IF lc_erro IS NOT INITIAL.
      lc_erro = me->convert_to_utf8( i_texto = lc_erro ).
    ENDIF.
    server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
    http_code = &1.
    server->response->set_status( code = http_code  reason = &2 ).
    CONCATENATE '{"ERROR_CODE":"' &1 '","ERROR_MESSAGE":"' lc_erro '"}' INTO etext.
    server->response->set_cdata( etext ).
    EXIT.

  END-OF-DEFINITION.

* Get Server Info:

  server->get_location( IMPORTING host = DATA(lc_host)  port = DATA(lc_port)  out_protocol = DATA(lc_proto) ).
  CONCATENATE lc_proto '://' lc_host ':' lc_port INTO me->my_url.

  me->my_service = server->request->get_header_field( name = '~script_name' ).
  action         = server->request->get_header_field( 'action' ).

  TRANSLATE action TO UPPER CASE.

  IF action EQ 'LOGIN'.
    TRY .
        me->login( EXPORTING i_server = server IMPORTING e_json = o_cdata ).
        server->response->set_compression( ).
        server->response->set_cdata( data = o_cdata ).
      CATCH zcx_fmcall_app_mobile INTO DATA(ex_app_mobile).
        ex_app_mobile->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO DATA(mtext) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        http_error '401' 'Server' mtext.
    ENDTRY.
    EXIT.
  ENDIF.

  IF action EQ 'LOGOFF'.
    TRY .
        me->logoff( EXPORTING i_server = server ).
      CATCH zcx_fmcall_app_mobile INTO ex_app_mobile.
        ex_app_mobile->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        http_error '401' 'Server' mtext.
    ENDTRY.
    EXIT.
  ENDIF.

*  TRY .
*      ME->VALIDA_TOKEN( EXPORTING I_SERVER = SERVER ).
*    CATCH ZCX_FMCALL_APP_MOBILE INTO EX_APP_MOBILE.
*      EX_APP_MOBILE->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      HTTP_ERROR '402' 'Server' MTEXT.
*  ENDTRY.

* Get function name from PATH_INFO
  path_info = server->request->get_header_field( name = '~path_info' ).
  SPLIT path_info AT '/' INTO TABLE p_info_tab.
  READ TABLE p_info_tab INDEX 2 INTO funcname.
  READ TABLE p_info_tab INDEX 3 INTO funcname2.
  IF sy-subrc EQ 0.
    CONCATENATE '//' funcname '/' funcname2 INTO funcname.
    CONDENSE funcname.
  ENDIF.

  CHECK funcname IS NOT INITIAL.
  TRANSLATE funcname TO UPPER CASE.

  "Funçãoes Gerais
  TRY .
      CASE funcname.
        WHEN zif_fmcall_app_mobile~st_get_qtd_tarefas_usuario.
          me->zif_fmcall_app_mobile~get_qtd_tarefas_usuario( IMPORTING e_json = o_cdata ).
      ENDCASE.
    CATCH zcx_fmcall_app_mobile INTO ex_app_mobile.
      ex_app_mobile->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      http_error '405' 'Server' mtext.
  ENDTRY.

  DATA filial TYPE werks_d.
  DATA status TYPE char1.
* ---> S4 Migration - 19/06/2023 - JS
*  DATA mes_inicio TYPE char02.
  DATA mes_inicio(02) TYPE c.
* <--- S4 Migration - 19/06/2023 - JS
*---> 20.06.2023 - Migração S4 - DG
  "  DATA mes_fim TYPE char02.
  DATA mes_fim TYPE zchar02.
*<--- 20.06.2023 - Migração S4 - DG
  DATA ano TYPE gjahr.
  DATA tp_object TYPE eqart.
  DATA ct_eqpto TYPE eqtyp.
  DATA eqpto TYPE equnr.
  DATA modelo TYPE typbz.
  DATA cpf_nr TYPE pbr_cpfnr.

  DATA user_ad TYPE zde_usuario.
  DATA update_at TYPE timestampl.
  DATA create_at TYPE timestampl.

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
  DATA body      TYPE string.
  DATA: usuario_orc_order TYPE usalias,
        matnr             TYPE mara-matnr,
        maktx             TYPE makt-maktx,
        werks             TYPE marc-werks,
        online            TYPE c,
        pernr             TYPE persno,
        ordem             TYPE aufnr.
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

  DATA: lc_tp_ambiente  TYPE zde_tp_ambiente,
        lc_ambiente     TYPE string,
        lc_content_type TYPE string.

  DATA _view TYPE string.
  DATA param TYPE string.
  DATA: i_info TYPE zde_integracao_http_config.
  DATA: server_base TYPE REF TO cl_http_server.

  server_base = CAST #( server ).

  status = server->request->get_form_field( 'status' ).
  filial = server->request->get_form_field( 'filial' ).

  mes_inicio = server->request->get_form_field( 'mes_inicio' ).
  mes_fim    = server->request->get_form_field( 'mes_fim' ).
  ano        = server->request->get_form_field( 'ano ' ).
  tp_object  = server->request->get_form_field( 'tp_object' ).
  ct_eqpto   = server->request->get_form_field( 'ct_eqpto' ).
  eqpto      = server->request->get_form_field( 'eqpto' ).
  modelo     = server->request->get_form_field( 'modelo' ).
  user_ad     = server->request->get_form_field( 'user_ad' ).
  create_at     = server->request->get_form_field( 'create_at' ).
  update_at     = server->request->get_form_field( 'update_at' ).

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
  usuario_orc_order  = server->request->get_form_field( 'usuario_orc_order' ).
  matnr  = server->request->get_form_field( 'matnr' ).
  maktx  = server->request->get_form_field( 'maktx' ).
  werks  = server->request->get_form_field( 'werks' ).
  online  = server->request->get_form_field( 'online' ).
  pernr  = server->request->get_form_field( 'pernr' ).
  ordem  = server->request->get_form_field( 'ordem' ).
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

  _view = funcname.

  lc_format = server->request->get_form_field( 'format' ).
  IF lc_format IS INITIAL.
    lc_format = server->request->get_form_field( '$format' ).
  ENDIF.

  i_info-ds_body = server->request->get_cdata( ).
  i_info-ds_content_type = server->request->get_content_type( ).
  i_info-ds_formato = CONV #( lc_format ).
  i_info-ds_ip_origem = server_base->c_caller_ip.
  i_info-ds_metodo = server->request->get_method( ).
  i_info-ds_server_protocolo = CONV #( server_base->m_protocol_version ).

  DATA(lc_url) = me->my_url && me->my_service && path_info.

  i_info-ds_url = CONV #( lc_url ).

  "========================================================
  CASE sy-sysid.
    WHEN 'DEV'.
      lc_tp_ambiente = '01'.
      lc_ambiente = 'Desenvolvimento'.
    WHEN 'QAS'.
      lc_tp_ambiente = '02'.
      lc_ambiente = 'Qualidade'.
    WHEN 'PRD'.
      lc_tp_ambiente = '03'.
      lc_ambiente = 'Produtivo'.
  ENDCASE.

* Verificar se o recurso está cadastrado para o ambiente selecionado
  IF funcname EQ 'PM_APONTAMENTO'.
    param = funcname.
    TRANSLATE param TO LOWER CASE.

    SELECT SINGLE * INTO @DATA(wa_zwst0002)
      FROM zwst0002
     WHERE cd_recurso  EQ @param
       AND tp_ambiente EQ @lc_tp_ambiente.
  ENDIF.

  "============================================================

  DATA(_ordem) = NEW zcl_pm_ordem( ).
  DATA(_fatura) = NEW zcl_webservic_protheus( ).

  TRY.
      CASE _view.
        WHEN 'CRIAR_ORDEM' OR 'CRIAR_NOTA'.

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
          IF online IS NOT INITIAL.
            me->online = online.
          ENDIF.
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

          zcl_integracao_mobile_energia=>zif_integracao_mobile_energia~get_instance(
             )->set_ds_data( EXPORTING i_info = i_info
             )->set_send_msg(
               IMPORTING
                 e_msg = o_cdata
                 e_zintegracao_log = DATA(e_zintegracao_log)
             ).

          CALL METHOD _ordem->server
            EXPORTING
              view     = funcname
            CHANGING
              request  = server->request
              response = server->response
            RECEIVING
              data     = o_cdata.

        WHEN 'PM_PROTHEUS'.

          CALL FUNCTION 'ZPM_EXP_FATURA_SAP'
            EXPORTING
              view   = _view
            IMPORTING
              result = o_cdata.

        WHEN 'PM_APONTAMENTO'.

          DATA: lob_integracao TYPE REF TO zif_integracao_inbound.

          FREE: lob_integracao.
          CREATE OBJECT lob_integracao TYPE (wa_zwst0002-nm_recurso).

          lob_integracao->set_data( i_info = i_info
                       )->processar_requisicao( IMPORTING e_msg              = o_cdata
                                                          e_zintegracao_log  = e_zintegracao_log
                       )->configure_server( i_http_server  = server ).


        WHEN OTHERS.

          CALL FUNCTION 'ZPM_EXPORT_DATA_TO_PM_MOBILE'
            EXPORTING
              view              = _view
              status            = status
              filial            = filial
              mes_inicio        = mes_inicio
              mes_fim           = mes_fim
              ano               = ano
              tp_object         = tp_object
              ct_eqpto          = ct_eqpto
              eqpto             = eqpto
              modelo            = modelo
*             cpf_nr            = cpf_nr
              user_ad           = user_ad
              update_at         = update_at
              create_at         = create_at
*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
              body              = i_info-ds_body
              usuario_orc_order = usuario_orc_order
              matnr             = matnr
              maktx             = maktx
              werks             = werks
              pernr             = pernr
              ordem             = ordem
*-Equalização RISE x PRD - 19.07.2023 - JT - fim
            IMPORTING
              result            = o_cdata.

      ENDCASE.

    CATCH zcx_fmcall_app_mobile INTO ex_app_mobile.
      ex_app_mobile->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      http_error '405' 'Server' mtext.
  ENDTRY.

  server->response->set_compression( ).
  server->response->set_cdata( data = o_cdata ).

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


METHOD json_deserialize.

  TYPE-POOLS: abap, js.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  DATA paramname   TYPE string.
  DATA js_obj      TYPE REF TO cl_java_script.
  DATA js_prop_tab TYPE js_property_tab.

  FIELD-SYMBOLS <js_prop> TYPE LINE OF js_property_tab.
  FIELD-SYMBOLS <parm>    TYPE abap_func_parmbind.

  IF json IS INITIAL. EXIT. ENDIF.

  json2abap( EXPORTING json_string = json  IMPORTING property_table = js_prop_tab  CHANGING js_object = js_obj ).

  LOOP AT js_prop_tab ASSIGNING <js_prop>.
    paramname = <js_prop>-name.
    TRANSLATE paramname TO UPPER CASE.
    READ TABLE paramtab WITH KEY name = paramname ASSIGNING <parm>.
    IF sy-subrc EQ 0.
      IF <parm>-kind NE abap_func_importing. "" va al revés, cuidado!!!
        json2abap( EXPORTING var_name = <js_prop>-name  CHANGING abap_data = <parm>-value js_object = js_obj ).
      ENDIF.
    ENDIF.
  ENDLOOP.

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
**** Inicio - Rubenilson Pereira - 25.07.2022 - US83600 - MOBMAN
  IF zcl_fmcall_pm_mobile=>online IS NOT INITIAL.

    REFRESH js_prop_tab.
    CLEAR js_obj.

    READ TABLE paramtab WITH KEY name = 'ONLINE' ASSIGNING FIELD-SYMBOL(<fs_parm>).
    IF sy-subrc IS INITIAL.

      DATA lv_json TYPE string.

      CONCATENATE '{ "ONLINE":' '"' zcl_fmcall_pm_mobile=>online '" }' INTO lv_json .

      json2abap( EXPORTING json_string =  lv_json  IMPORTING property_table = js_prop_tab  CHANGING js_object = js_obj ).

      LOOP AT js_prop_tab ASSIGNING <js_prop>.
        paramname = <js_prop>-name.
        TRANSLATE paramname TO UPPER CASE.
        READ TABLE paramtab WITH KEY name = paramname ASSIGNING <parm>.
        IF sy-subrc EQ 0.
          IF <parm>-kind NE abap_func_importing. "" va al revés, cuidado!!!
            json2abap( EXPORTING var_name = <js_prop>-name  CHANGING abap_data = <parm>-value js_object = js_obj ).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
**** Fim - Rubenilson Pereira - 25.07.2022 - US83600 - MOBMAN
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

ENDMETHOD.


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


  METHOD ZIF_FMCALL_APP_MOBILE~GET_FAT_LIM_CREDITO.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST10'
      IMPORTING
        OUT_JSON = E_JSON.

  ENDMETHOD.


  method ZIF_FMCALL_APP_MOBILE~GET_IM_INVESTIMENTO.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST13'
      IMPORTING
        OUT_JSON = E_JSON.

  endmethod.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_IM_INVESTIMENTO_ITENS.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 21.
    TYPES END OF TY_SOLICITA.

    DATA: I_KOSTL      TYPE TY_SOLICITA,
          VL_KOSTL     TYPE KOSTL,
          VL_LOTE      TYPE NUMC10,
          IT_LOTES     TYPE TABLE OF ZFI_GRU_INV,
          IT_ESTRA     TYPE TABLE OF ZFI_ESTRATEGIA_ZIM,
          IT_DOCS      TYPE TABLE OF ZIM01_SOL_AP_INV,
          WA_ITENS     TYPE ZDE_ORC_INVEST_APROVACAO,
          IT_ITENS     TYPE TABLE OF ZDE_ORC_INVEST_APROVACAO,
          LC_DATA      TYPE C LENGTH 10,
          LC_VALOR     TYPE C LENGTH 20,
          LC_VALOR_DEC TYPE ZDE_DECIMAL_20_2,
          E_JSON_INV   TYPE STRING.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = I_KOSTL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = I_KOSTL-LOTE(10)
      IMPORTING
        OUTPUT = VL_KOSTL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = I_KOSTL-LOTE+11(10)
      IMPORTING
        OUTPUT = VL_LOTE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = VL_KOSTL
      IMPORTING
        OUTPUT = VL_KOSTL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = VL_LOTE
      IMPORTING
        OUTPUT = VL_LOTE.

    CALL FUNCTION 'Z_IM_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
        V_KOSTL   = VL_KOSTL
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA
        T_DOCS    = IT_DOCS.

    DELETE IT_LOTES WHERE KOSTL NE VL_KOSTL.
    DELETE IT_ESTRA WHERE KOSTL NE VL_KOSTL.
    DELETE IT_DOCS  WHERE KOSTL NE VL_KOSTL.

    DELETE IT_LOTES WHERE LOTE NE VL_LOTE.
    DELETE IT_ESTRA WHERE LOTE NE VL_LOTE.
    DELETE IT_DOCS  WHERE LOTE NE VL_LOTE.

    DELETE IT_ESTRA WHERE OPCOES <> ICON_SET_STATE.
    SORT IT_ESTRA BY LOTE KOSTL.
    SORT IT_LOTES BY LOTE KOSTL.

    LOOP AT IT_DOCS INTO DATA(_ITEM).

      READ TABLE IT_LOTES WITH KEY LOTE = _ITEM-LOTE KOSTL = _ITEM-KOSTL BINARY SEARCH TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE IT_ESTRA WITH KEY LOTE = _ITEM-LOTE KOSTL = _ITEM-KOSTL BINARY SEARCH TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR: WA_ITENS.
      "WRITE PO_HEADER-DOC_DATE TO LC_DATA DD/MM/YYYY.
      "WA_PEDIDOS_ITENS-DT_PEDIDO = LC_DATA.

      WA_ITENS-MATERIAL_TXT  = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _ITEM-DESCR_ITEM ) ).
      WA_ITENS-QUANTIDADE    = _ITEM-MENGE.
      WA_ITENS-VALOR_UN      = _ITEM-VLR_UNITARIO.
      WA_ITENS-VALOR         = _ITEM-VLR_TOTAL.

      WRITE _ITEM-MENGE TO LC_VALOR.
      WA_ITENS-QUANTIDADE_TXT = LC_VALOR.
      CONDENSE WA_ITENS-QUANTIDADE_TXT NO-GAPS.

      LC_VALOR_DEC = WA_ITENS-VALOR_UN.
      WRITE LC_VALOR_DEC TO LC_VALOR.
      WA_ITENS-VALOR_UN_TXT = LC_VALOR.
      CONDENSE WA_ITENS-VALOR_UN_TXT NO-GAPS.

      LC_VALOR_DEC = WA_ITENS-VALOR.
      WRITE LC_VALOR_DEC TO LC_VALOR.
      WA_ITENS-VALOR_TXT = LC_VALOR.
      CONDENSE WA_ITENS-VALOR_TXT NO-GAPS.

      WA_ITENS-UNIDADE = 'UN'.

      SELECT SINGLE KOSTL, LTEXT
        INTO @DATA(_COST_CENTER_NAME)
        FROM CSKT
       WHERE KOSTL EQ @_ITEM-KOSTL
         AND SPRAS EQ @SY-LANGU.

      IF SY-SUBRC IS INITIAL.
        WA_ITENS-CENTRO_CUSTO     = _COST_CENTER_NAME-KOSTL.
        WA_ITENS-CENTRO_CUSTO_TXT = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _COST_CENTER_NAME-LTEXT ) ).
      ENDIF.

      SELECT SINGLE NAME1
        INTO @DATA(_BRANCH_NAME)
        FROM T001W
       WHERE WERKS EQ @_ITEM-GSBER
         AND SPRAS EQ @SY-LANGU.

      IF SY-SUBRC IS INITIAL.
        WA_ITENS-BRANCH_ID  = _ITEM-GSBER.
        WA_ITENS-BRANCH_TXT = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _BRANCH_NAME ) ).
      ENDIF.

      APPEND WA_ITENS TO IT_ITENS.
    ENDLOOP.

    E_JSON = '{ "itens" : [ '.
    CLEAR: E_JSON_INV.

    LOOP AT IT_ITENS INTO WA_ITENS.
      IF E_JSON_INV IS NOT INITIAL.
        E_JSON_INV = E_JSON_INV && ','.
      ENDIF.
      E_JSON_INV = E_JSON_INV &&
                     '{' &&
                     '"e_kostl" : ' && '"' && WA_ITENS-KOSTL && '",' &&
                     '"e_material_txt" : ' && '"' && ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( I_TEXTO = CONV #( WA_ITENS-MATERIAL_TXT ) ) && '",' &&
                     '"e_branch_id" : ' && '"' && WA_ITENS-BRANCH_ID && '",' &&
                     '"e_branch_txt" : ' && '"' && ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( I_TEXTO = CONV #( WA_ITENS-BRANCH_TXT ) ) && '",' &&
                     '"e_unidade" : ' && '"' && WA_ITENS-UNIDADE && '",' &&
                     '"e_quantidade" : ' && '"' && WA_ITENS-QUANTIDADE && '",' &&
                     '"e_valor_un" : ' && '"' && WA_ITENS-VALOR_UN && '",' &&
                     '"e_valor" : ' && '"' && WA_ITENS-VALOR && '",' &&
                     '"e_quantidade_txt" : ' && '"' && WA_ITENS-QUANTIDADE_TXT && '",' &&
                     '"e_valor_un_txt" : ' && '"' && WA_ITENS-VALOR_UN_TXT && '",' &&
                     '"e_valor_txt" : ' && '"' && WA_ITENS-VALOR_TXT && '",' &&
                     '"e_centro_custo" : ' && '"' && WA_ITENS-CENTRO_CUSTO && '",' &&
                     '"e_centro_custo_txt" : ' && '"' && ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( I_TEXTO = WA_ITENS-CENTRO_CUSTO_TXT ) && '" ' &&
                   '}'.
    ENDLOOP.
    E_JSON = E_JSON && E_JSON_INV && ']}'.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_LAC_MANUAIS.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST07'
      IMPORTING
        OUT_JSON = E_JSON.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_LIB_EMBARQUES_INSUMOS.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST11'
      IMPORTING
        OUT_JSON = E_JSON.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_MM_ADIANTAMENTO_APROVACAO.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST04'
      IMPORTING
        OUT_JSON = E_JSON.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_MM_PEDIDOS_APROVACAO.

    DATA: ITEMS        TYPE TABLE OF SWWWLHEAD,
          LEADING      TYPE SWR_OBJ_2,
          HEADER       TYPE BAPIEKKOL,
          TEXTS        TYPE TABLE OF BAPIEKKOTX,
          WA_PEDIDOS   TYPE ZDE_PEDIDOS_APROVACAO,
          LC_DATA      TYPE C LENGTH 10,
          LC_VALOR     TYPE C LENGTH 14,
          LC_VALOR_DEC TYPE ZDE_DECIMAL_20_2,
          E_JSON_PED   TYPE STRING,
          FILTRO       TYPE TABLE OF RHOBJECTS.

    FILTRO = VALUE #( ( OBJECT = 'TS20000166' ) ).

    CLEAR E_PEDIDOS.

    DATA(LC_USER) = SY-UNAME.

    CALL FUNCTION 'RH_INBOX_VIEW_CREATE'
      EXPORTING
        INBOX_USER        = LC_USER
      TABLES
        WI_HEAD           = ITEMS
        TASK_FILTER       = FILTRO
      EXCEPTIONS
        NO_ACTIVE_PLVAR   = 1
        NO_TASKS_FOUND    = 2
        USER_NOT_DEFINED  = 3
        NO_WORKITEM_FOUND = 4
        OTHERS            = 5.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    "//Delete what isn't purchase order
    DELETE ITEMS WHERE WI_RH_TASK NE 'TS20000166'.

    LOOP AT ITEMS INTO DATA(_ITEM).

      CLEAR WA_PEDIDOS.
      WA_PEDIDOS-WI_ID = _ITEM-WI_ID.
      WA_PEDIDOS-WI_CD = _ITEM-WI_CD.
      WA_PEDIDOS-WI_CT = _ITEM-WI_CT.

      "//Get purchase order number
      CALL FUNCTION 'SAP_WAPI_GET_OBJECTS'
        EXPORTING
          WORKITEM_ID      = _ITEM-WI_ID
        IMPORTING
          LEADING_OBJECT_2 = LEADING.

      CLEAR: TEXTS, TEXTS[].

*---> 01/07/2023 - Migração S4 - EJ
*      CALL FUNCTION 'BAPI_PO_GETDETAIL'                 "#EC CI_USAGE_OK[2438131] " >> ---> S4 Migration - 07/07/2023 - RZ
      CALL FUNCTION 'BAPI_PO_GETDETAIL1'                 "#EC CI_USAGE_OK[2438131] " >> ---> S4 Migration - 07/07/2023 - RZ
*<--- 01/07/2023 - Migração S4 - EJ
        EXPORTING
          PURCHASEORDER   = CONV CHAR10( LEADING-INSTID )
          HEADER_TEXTS    = ABAP_TRUE
        IMPORTING
          PO_HEADER       = HEADER
        TABLES
          PO_HEADER_TEXTS = TEXTS.

      LOOP AT TEXTS INTO DATA(_TEXT).
        IF WA_PEDIDOS-DESCRIPTION IS INITIAL.
          WA_PEDIDOS-DESCRIPTION = _TEXT-TEXT_LINE.
        ELSE.
          CONCATENATE WA_PEDIDOS-DESCRIPTION _TEXT-TEXT_LINE INTO WA_PEDIDOS-DESCRIPTION SEPARATED BY SPACE.
        ENDIF.
      ENDLOOP.
      WA_PEDIDOS-DESCRIPTION = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = WA_PEDIDOS-DESCRIPTION ).

      SHIFT LEADING-INSTID LEFT DELETING LEADING '0'.

      SELECT SUM( BRTWR )
        FROM EKPO
        INTO @WA_PEDIDOS-BRTWR
       WHERE EBELN EQ @LEADING-INSTID.

      WRITE _ITEM-WI_CD TO LC_DATA DD/MM/YYYY.
      WA_PEDIDOS-WI_CD_TXT = LC_DATA.

      LC_VALOR_DEC = WA_PEDIDOS-BRTWR.
      WRITE LC_VALOR_DEC TO LC_VALOR.
      CONDENSE LC_VALOR NO-GAPS.
      WA_PEDIDOS-BRTWR_TXT = LC_VALOR.
      WA_PEDIDOS-VENDOR_ID   = HEADER-VENDOR.
      WA_PEDIDOS-VENDOR_NAME = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( HEADER-VEND_NAME ) ).
      WA_PEDIDOS-EBELN       = HEADER-PO_NUMBER.
      WA_PEDIDOS-WAERS       = HEADER-CURRENCY.

      WRITE HEADER-DOC_DATE TO LC_DATA DD/MM/YYYY.
      WA_PEDIDOS-DT_PEDIDO = LC_DATA.

      APPEND WA_PEDIDOS TO E_PEDIDOS.

    ENDLOOP.

    SORT E_PEDIDOS BY EBELN.
    DELETE ADJACENT DUPLICATES FROM E_PEDIDOS COMPARING EBELN.

    E_JSON = '{ "pedidos" : [ '.
    CLEAR: E_JSON_PED.

    LOOP AT E_PEDIDOS INTO WA_PEDIDOS.
      IF E_JSON_PED IS NOT INITIAL.
        E_JSON_PED = E_JSON_PED && ','.
      ENDIF.
      E_JSON_PED = E_JSON_PED &&
                    '{' &&
                     '"e_ebeln" : ' && '"' && WA_PEDIDOS-EBELN && '",' &&
                     '"e_dt_pedido" : ' && '"' && WA_PEDIDOS-DT_PEDIDO && '",' &&
                     '"e_waers" : ' && '"' && WA_PEDIDOS-WAERS && '",' &&
                     '"e_brtwr" : ' && '"' && WA_PEDIDOS-BRTWR && '",' &&
                     '"e_brtwr_txt" : ' && '"' && WA_PEDIDOS-BRTWR_TXT && '",' &&
                     '"e_wi_id" : ' && '"' && WA_PEDIDOS-WI_ID && '",' &&
                     '"e_wi_cd" : ' && '"' && WA_PEDIDOS-WI_CD && '",' &&
                     '"e_wi_cd_txt" : ' && '"' && WA_PEDIDOS-WI_CD_TXT && '",' &&
                     '"e_wi_ct" : ' && '"' && WA_PEDIDOS-WI_CT && '",' &&
                     '"e_vendor_id" : ' && '"' && WA_PEDIDOS-VENDOR_ID && '",' &&
                     '"e_vendor_name" : ' && '"' && ME->CONVERT_TO_UTF8( I_TEXTO = CONV #( WA_PEDIDOS-VENDOR_NAME ) ) && '",' &&
                     '"e_description" : ' && '"' && ME->CONVERT_TO_UTF8( I_TEXTO = WA_PEDIDOS-DESCRIPTION ) && '"' &&
                   '}'.
    ENDLOOP.
    E_JSON = E_JSON && E_JSON_PED && ']}'.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_MM_PEDIDO_ITENS.

    DATA: PO_HEADER        TYPE BAPIEKKOL,
          ITEMS            TYPE TABLE OF BAPIEKPO,
          ITEMSCC          TYPE TABLE OF BAPIEKKN,
          WA_PEDIDOS_ITENS TYPE ZDE_PEDITENS_APROVACAO,
          LC_DATA          TYPE C LENGTH 10,
          LC_VALOR         TYPE C LENGTH 20,
          LC_VALOR_DEC     TYPE ZDE_DECIMAL_20_2,
          E_JSON_PED       TYPE STRING.

    DATA: I_PEDIDO TYPE ZDE_PEDIDOS_APROVACAO.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = I_PEDIDO.
*---> 01/07/2023 - Migração S4 - EJ
*    CALL FUNCTION 'BAPI_PO_GETDETAIL'               "#EC CI_USAGE_OK[2438131] " >> ---> S4 Migration - 07/07/2023 - RZ
    CALL FUNCTION 'BAPI_PO_GETDETAIL1'               "#EC CI_USAGE_OK[2438131] " >> ---> S4 Migration - 07/07/2023 - RZ
*<--- 01/07/2023 - Migração S4 - EJ
      EXPORTING
        PURCHASEORDER              = I_PEDIDO-EBELN
        ITEMS                      = ABAP_TRUE
        ACCOUNT_ASSIGNMENT         = ABAP_TRUE
      IMPORTING
        PO_HEADER                  = PO_HEADER
      TABLES
        PO_ITEMS                   = ITEMS
        PO_ITEM_ACCOUNT_ASSIGNMENT = ITEMSCC.

    LOOP AT ITEMS INTO DATA(_ITEM).

      TRY.
          DATA(_ACCOUNTING_ACCOUNT) = ITEMSCC[ PO_ITEM = _ITEM-PO_ITEM ].
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
      ENDTRY.

      SELECT SINGLE NAME1
        INTO @DATA(_BRANCH_NAME)
        FROM T001W
       WHERE WERKS EQ @_ITEM-PLANT
         AND SPRAS EQ @SY-LANGU.

      SELECT SINGLE TXT20
        INTO @DATA(_ACCOUNTING_ACCOUNT_NAME)
        FROM SKAT
       WHERE SAKNR EQ @_ACCOUNTING_ACCOUNT-G_L_ACCT
         AND SPRAS EQ @SY-LANGU.

      SELECT SINGLE LTEXT
        INTO @DATA(_COST_CENTER_NAME)
        FROM CSKT
       WHERE KOSTL EQ @_ACCOUNTING_ACCOUNT-COST_CTR
         AND SPRAS EQ @SY-LANGU.

      IF _ACCOUNTING_ACCOUNT-ORDER_NO IS NOT INITIAL.
        SELECT SINGLE KTEXT
          INTO @DATA(_ORDEM_TEXT)
          FROM AUFK
         WHERE AUFNR EQ @_ACCOUNTING_ACCOUNT-ORDER_NO.
        SHIFT _ACCOUNTING_ACCOUNT-ORDER_NO LEFT DELETING LEADING '0'.
      ENDIF.

      SHIFT _ITEM-MATERIAL LEFT DELETING LEADING '0'.
      SHIFT _ITEM-PO_ITEM  LEFT DELETING LEADING '0'.
      SHIFT _ACCOUNTING_ACCOUNT-G_L_ACCT LEFT DELETING LEADING '0'.
      SHIFT _ACCOUNTING_ACCOUNT-ASSET_NO LEFT DELETING LEADING '0'.
      SHIFT _ACCOUNTING_ACCOUNT-SUB_NUMBER LEFT DELETING LEADING '0'.
      SHIFT _ACCOUNTING_ACCOUNT-ORDER_NO LEFT DELETING LEADING '0'.

      WA_PEDIDOS_ITENS-EBELN = _ITEM-PO_NUMBER.
      WA_PEDIDOS_ITENS-EBELP = _ITEM-PO_ITEM.

      WRITE PO_HEADER-DOC_DATE TO LC_DATA DD/MM/YYYY.
      WA_PEDIDOS_ITENS-DT_PEDIDO = LC_DATA.

      WA_PEDIDOS_ITENS-MATERIAL_ID   = _ITEM-MATERIAL.
      WA_PEDIDOS_ITENS-MATERIAL_TXT  = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _ITEM-SHORT_TEXT ) ).
      WA_PEDIDOS_ITENS-BRANCH_ID     = _ITEM-PLANT.
      WA_PEDIDOS_ITENS-BRANCH_TXT    = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _BRANCH_NAME ) ).
      WA_PEDIDOS_ITENS-QUANTIDADE    = _ITEM-QUANTITY.
      WA_PEDIDOS_ITENS-VALOR_UN      = _ITEM-NET_PRICE.
      WA_PEDIDOS_ITENS-VALOR         = ( _ITEM-NET_PRICE / _ITEM-CONV_DEN1 ) * _ITEM-QUANTITY.

      WRITE _ITEM-QUANTITY TO LC_VALOR.
      WA_PEDIDOS_ITENS-QUANTIDADE_TXT = LC_VALOR.
      CONDENSE WA_PEDIDOS_ITENS-QUANTIDADE_TXT NO-GAPS.

      LC_VALOR_DEC = WA_PEDIDOS_ITENS-VALOR_UN.
      WRITE LC_VALOR_DEC TO LC_VALOR.
      WA_PEDIDOS_ITENS-VALOR_UN_TXT = LC_VALOR.
      CONDENSE WA_PEDIDOS_ITENS-VALOR_UN_TXT NO-GAPS.

      IF _ITEM-UNIT NE _ITEM-ORDERPR_UN.
        CONCATENATE WA_PEDIDOS_ITENS-VALOR_UN_TXT _ITEM-ORDERPR_UN INTO WA_PEDIDOS_ITENS-VALOR_UN_TXT SEPARATED BY SPACE.
      ENDIF.

      LC_VALOR_DEC = WA_PEDIDOS_ITENS-VALOR.
      WRITE LC_VALOR_DEC TO LC_VALOR.
      WA_PEDIDOS_ITENS-VALOR_TXT = LC_VALOR.
      CONDENSE WA_PEDIDOS_ITENS-VALOR_TXT NO-GAPS.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          INPUT          = _ITEM-UNIT
        IMPORTING
          OUTPUT         = WA_PEDIDOS_ITENS-UNIDADE
        EXCEPTIONS
          UNIT_NOT_FOUND = 1
          OTHERS         = 2.

      IF _ACCOUNTING_ACCOUNT-ORDER_NO IS NOT INITIAL.
        WA_PEDIDOS_ITENS-ORDEM = _ACCOUNTING_ACCOUNT-ORDER_NO.
        WA_PEDIDOS_ITENS-ORDEM_TXT = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _ORDEM_TEXT ) ).
      ENDIF.

      IF _ACCOUNTING_ACCOUNT-ASSET_NO IS NOT INITIAL AND _ACCOUNTING_ACCOUNT-SUB_NUMBER IS NOT INITIAL.
        WA_PEDIDOS_ITENS-IMOBILIZADO = _ACCOUNTING_ACCOUNT-ASSET_NO.
        WA_PEDIDOS_ITENS-SUB_NUMBER  = _ACCOUNTING_ACCOUNT-SUB_NUMBER.
      ELSEIF _ACCOUNTING_ACCOUNT-ASSET_NO IS NOT INITIAL AND _ACCOUNTING_ACCOUNT-SUB_NUMBER IS INITIAL.
        WA_PEDIDOS_ITENS-IMOBILIZADO = _ACCOUNTING_ACCOUNT-ASSET_NO.
      ENDIF.

      WA_PEDIDOS_ITENS-CONTA_RAZAO      = _ACCOUNTING_ACCOUNT-G_L_ACCT.
      WA_PEDIDOS_ITENS-CONTA_RAZAO_TXT  =  ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _ACCOUNTING_ACCOUNT_NAME ) ).

      WA_PEDIDOS_ITENS-CENTRO_CUSTO     = _ACCOUNTING_ACCOUNT-COST_CTR.
      WA_PEDIDOS_ITENS-CENTRO_CUSTO_TXT = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _COST_CENTER_NAME ) ).

      WA_PEDIDOS_ITENS-CL_CONTABIL     = _ITEM-ACCTASSCAT.

      CASE _ITEM-ACCTASSCAT.
        WHEN 'K'.
          WA_PEDIDOS_ITENS-CL_CONTABIL_TXT = 'Centro de Custo'.
        WHEN 'A'.
          WA_PEDIDOS_ITENS-CL_CONTABIL_TXT = 'Imobilizado'.
        WHEN 'F'.
          WA_PEDIDOS_ITENS-CL_CONTABIL_TXT = 'Ordem Interna'.
        WHEN OTHERS.
          WA_PEDIDOS_ITENS-CL_CONTABIL_TXT = 'Estoque'.
      ENDCASE.

      APPEND WA_PEDIDOS_ITENS TO E_PEDIDOS_ITENS.
    ENDLOOP.

    E_JSON = '{ "pedidos" : [ '.
    CLEAR: E_JSON_PED.

    LOOP AT E_PEDIDOS_ITENS INTO WA_PEDIDOS_ITENS.
      IF E_JSON_PED IS NOT INITIAL.
        E_JSON_PED = E_JSON_PED && ','.
      ENDIF.
      E_JSON_PED = E_JSON_PED &&
                     '{' &&
                     '"e_ebeln" : ' && '"' && WA_PEDIDOS_ITENS-EBELN && '",' &&
                     '"e_ebelp" : ' && '"' && WA_PEDIDOS_ITENS-EBELP && '",' &&
                     '"e_dt_pedido" : ' && '"' && WA_PEDIDOS_ITENS-DT_PEDIDO && '",' &&
                     '"e_material_id" : ' && '"' && WA_PEDIDOS_ITENS-MATERIAL_ID && '",' &&
                     '"e_material_txt" : ' && '"' && WA_PEDIDOS_ITENS-MATERIAL_TXT && '",' &&
                     '"e_branch_id" : ' && '"' && WA_PEDIDOS_ITENS-BRANCH_ID && '",' &&
                     '"e_branch_txt" : ' && '"' && WA_PEDIDOS_ITENS-BRANCH_TXT && '",' &&
                     '"e_unidade" : ' && '"' && WA_PEDIDOS_ITENS-UNIDADE && '",' &&
                     '"e_quantidade" : ' && '"' && WA_PEDIDOS_ITENS-QUANTIDADE && '",' &&
                     '"e_valor_un" : ' && '"' && WA_PEDIDOS_ITENS-VALOR_UN && '",' &&
                     '"e_valor" : ' && '"' && WA_PEDIDOS_ITENS-VALOR && '",' &&
                     '"e_quantidade_txt" : ' && '"' && WA_PEDIDOS_ITENS-QUANTIDADE_TXT && '",' &&
                     '"e_valor_un_txt" : ' && '"' && WA_PEDIDOS_ITENS-VALOR_UN_TXT && '",' &&
                     '"e_valor_txt" : ' && '"' && WA_PEDIDOS_ITENS-VALOR_TXT && '",' &&
                     '"e_ordem" : ' && '"' && WA_PEDIDOS_ITENS-ORDEM && '",' &&
                     '"e_ordem_txt" : ' && '"' && WA_PEDIDOS_ITENS-ORDEM_TXT && '",' &&
                     '"e_imobilizado" : ' && '"' && WA_PEDIDOS_ITENS-IMOBILIZADO && '",' &&
                     '"e_sub_number" : ' && '"' && WA_PEDIDOS_ITENS-SUB_NUMBER && '",' &&
                     '"e_conta_razao" : ' && '"' && WA_PEDIDOS_ITENS-CONTA_RAZAO && '",' &&
                     '"e_conta_razao_txt" : ' && '"' && WA_PEDIDOS_ITENS-CONTA_RAZAO_TXT && '",' &&
                     '"e_centro_custo" : ' && '"' && WA_PEDIDOS_ITENS-CENTRO_CUSTO && '",' &&
                     '"e_centro_custo_txt" : ' && '"' && WA_PEDIDOS_ITENS-CENTRO_CUSTO_TXT && '",' &&
                     '"e_cl_contabil" : ' && '"' && WA_PEDIDOS_ITENS-CL_CONTABIL && '",' &&
                     '"e_cl_contabil_txt" : ' && '"' && WA_PEDIDOS_ITENS-CL_CONTABIL_TXT && '"' &&
                   '}'.
    ENDLOOP.
    E_JSON = E_JSON && E_JSON_PED && ']}'.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_MM_QTD_TAREFAS.

    DATA: ITEMS    TYPE TABLE OF SWWWLHEAD,
          FILTRO   TYPE TABLE OF RHOBJECTS,
          IT_LOTES TYPE TABLE OF ZAD_LOTES_IMP.

    DATA(LC_USER) = SY-UNAME.
    E_QTD_PEDIDOS    = 0.
    E_QTD_REQUISICAO = 0.
    E_QTD_RESERVA    = 0.
    E_QTD_ADIANTAMENTOS = 0.

    FILTRO = VALUE #( ( OBJECT = 'TS20000166' )
                      ( OBJECT = 'TS00007986' )
                      ( OBJECT = 'TS99900017' ) ).

    CALL FUNCTION 'RH_INBOX_VIEW_CREATE'
      EXPORTING
        INBOX_USER        = LC_USER
      TABLES
        WI_HEAD           = ITEMS
        TASK_FILTER       = FILTRO
      EXCEPTIONS
        NO_ACTIVE_PLVAR   = 1
        NO_TASKS_FOUND    = 2
        USER_NOT_DEFINED  = 3
        NO_WORKITEM_FOUND = 4
        OTHERS            = 5.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    "Pedido de compra
    DATA(QTD_ITEMS) = ITEMS[].
    DELETE QTD_ITEMS WHERE WI_RH_TASK NE 'TS20000166'.
    SORT QTD_ITEMS BY WI_ID.
    DELETE ADJACENT DUPLICATES FROM QTD_ITEMS COMPARING WI_ID.
    DESCRIBE TABLE QTD_ITEMS LINES E_QTD_PEDIDOS.

    "Requisições
    QTD_ITEMS = ITEMS[].
    DELETE QTD_ITEMS WHERE WI_RH_TASK NE 'TS00007986'.
    SORT QTD_ITEMS BY WI_ID.
    DELETE ADJACENT DUPLICATES FROM QTD_ITEMS COMPARING WI_ID.
    DESCRIBE TABLE QTD_ITEMS LINES E_QTD_REQUISICAO.

    "Reservas
    QTD_ITEMS = ITEMS[].
    DELETE QTD_ITEMS WHERE WI_RH_TASK NE 'TS99900017'.
    SORT QTD_ITEMS BY WI_ID.
    DELETE ADJACENT DUPLICATES FROM QTD_ITEMS COMPARING WI_ID.
    DESCRIBE TABLE QTD_ITEMS LINES E_QTD_RESERVA.

    CALL FUNCTION 'Z_AD_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
      TABLES
        T_LOTES   = IT_LOTES.

    DESCRIBE TABLE IT_LOTES LINES E_QTD_ADIANTAMENTOS.

    E_JSON = '{' &&
                 '"e_qtd_pedido" : '     && '"' && E_QTD_PEDIDOS    && '",' &&
                 '"e_qtd_requisicao" : ' && '"' && E_QTD_REQUISICAO && '",' &&
                 '"e_qtd_reserva" : '    && '"' && E_QTD_RESERVA    && '",'  &&
                 '"e_qtd_adiantamentos" : '    && '"' && E_QTD_ADIANTAMENTOS && '"'  &&
             '}'.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_MM_REQUISICAO_APROVACAO.

    DATA: INBOX_ITEMS   TYPE TABLE OF SWWWLHEAD,
          LEADING       TYPE SWR_OBJ_2,
          LC_DATA       TYPE C LENGTH 10,
          LC_VALOR      TYPE C LENGTH 14,
          WA_REQUISICAO	TYPE ZDE_REQUISICAO_APROVACAO,
          LINES         TYPE TABLE OF TLINE,
          LC_VALOR_DEC  TYPE ZDE_DECIMAL_20_2,
          E_JSON_REQ    TYPE STRING,
          FILTRO        TYPE TABLE OF RHOBJECTS.

    FILTRO = VALUE #( ( OBJECT = 'TS00007986' ) ).

    CALL FUNCTION 'RH_INBOX_VIEW_CREATE'
      TABLES
        WI_HEAD           = INBOX_ITEMS
        TASK_FILTER       = FILTRO
      EXCEPTIONS
        NO_ACTIVE_PLVAR   = 1
        NO_TASKS_FOUND    = 2
        USER_NOT_DEFINED  = 3
        NO_WORKITEM_FOUND = 4
        OTHERS            = 5.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    "//Delete what isn't purchase requisition
    DELETE INBOX_ITEMS WHERE WI_RH_TASK NE 'TS00007986'.

    LOOP AT INBOX_ITEMS INTO DATA(_ITEM).

      CLEAR: WA_REQUISICAO.

      "//Get purchase order number
      CALL FUNCTION 'SAP_WAPI_GET_OBJECTS'
        EXPORTING
          WORKITEM_ID      = _ITEM-WI_ID
        IMPORTING
          LEADING_OBJECT_2 = LEADING.

      LEADING-INSTID = LEADING-INSTID(10).

      SELECT SUM( PREIS )
        FROM EBAN
        INTO @DATA(LC_PREIS)
       WHERE BANFN EQ @LEADING-INSTID.

      SELECT * INTO TABLE @DATA(IT_EBAN)
        FROM EBAN
       WHERE BANFN EQ @LEADING-INSTID.

      DATA(IT_EBAN_AUX) = IT_EBAN[].
      SORT IT_EBAN_AUX BY WAERS.
      DELETE ADJACENT DUPLICATES FROM IT_EBAN_AUX COMPARING WAERS.
      DESCRIBE TABLE IT_EBAN_AUX LINES DATA(QTD_LINHAS).

      IF QTD_LINHAS EQ 1.
        READ TABLE IT_EBAN_AUX INTO DATA(WA_EBAN_AUX) INDEX 1.
        WA_REQUISICAO-WAERS = WA_EBAN_AUX-WAERS.
      ENDIF.

      LC_VALOR_DEC = LC_PREIS.
      WRITE LC_VALOR_DEC TO LC_VALOR.
      WA_REQUISICAO-BRTWR_TXT = LC_VALOR.
      CONDENSE WA_REQUISICAO-BRTWR_TXT NO-GAPS.

      WA_REQUISICAO-BANFN = LEADING-INSTID.
      WA_REQUISICAO-BRTWR = LC_PREIS.

      "//Get header text
      DATA(_TEXT_OBJ) = VALUE THEAD( TDID     = 'B01'
                                     TDOBJECT = 'EBANH'
                                     TDSPRAS  = SY-LANGU
                                     TDNAME   = LEADING-INSTID ).

      CLEAR: LINES, LINES[].

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = _TEXT_OBJ-TDID
          LANGUAGE                = _TEXT_OBJ-TDSPRAS
          NAME                    = _TEXT_OBJ-TDNAME
          OBJECT                  = _TEXT_OBJ-TDOBJECT
        TABLES
          LINES                   = LINES
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.

      SHIFT LEADING-INSTID LEFT DELETING LEADING '0'.

      LOOP AT LINES INTO DATA(WA_LINE).
        IF WA_REQUISICAO-DESCRIPTION IS INITIAL.
          WA_REQUISICAO-DESCRIPTION = WA_LINE-TDLINE.
        ELSE.
          CONCATENATE WA_REQUISICAO-DESCRIPTION WA_LINE-TDLINE INTO WA_REQUISICAO-DESCRIPTION SEPARATED BY SPACE.
        ENDIF.
      ENDLOOP.
      WA_REQUISICAO-DESCRIPTION = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = WA_REQUISICAO-DESCRIPTION ).

      WRITE _ITEM-WI_CD TO LC_DATA DD/MM/YYYY.
      WA_REQUISICAO-WI_CD_TXT     = LC_DATA.
      WA_REQUISICAO-WI_CD         = _ITEM-WI_CD.
      WA_REQUISICAO-WI_ID         = _ITEM-WI_ID.
      WA_REQUISICAO-WI_CT         = _ITEM-WI_CT.
      WA_REQUISICAO-DT_REQUISICAO = LC_DATA.
      APPEND WA_REQUISICAO TO E_REQUISICAO.
    ENDLOOP.

    E_JSON = '{ "requisicoes" : [ '.
    CLEAR: E_JSON_REQ.

    LOOP AT E_REQUISICAO INTO WA_REQUISICAO.
      IF E_JSON_REQ IS NOT INITIAL.
        E_JSON_REQ = E_JSON_REQ && ','.
      ENDIF.
      E_JSON_REQ = E_JSON_REQ &&
                   '{' &&
                     '"e_banfn" : ' && '"' && WA_REQUISICAO-BANFN && '",' &&
                     '"e_dt_requisicao" : ' && '"' && WA_REQUISICAO-DT_REQUISICAO && '",' &&
                     '"e_waers" : ' && '"' && WA_REQUISICAO-WAERS && '",' &&
                     '"e_brtwr" : ' && '"' && WA_REQUISICAO-BRTWR && '",' &&
                     '"e_brtwr_txt" : ' && '"' && WA_REQUISICAO-BRTWR_TXT && '",' &&
                     '"e_wi_id" : ' && '"' && WA_REQUISICAO-WI_ID && '",' &&
                     '"e_wi_cd" : ' && '"' && WA_REQUISICAO-WI_CD && '",' &&
                     '"e_wi_cd_txt" : ' && '"' && WA_REQUISICAO-WI_CD_TXT && '",' &&
                     '"e_wi_ct" : ' && '"' && WA_REQUISICAO-WI_CT && '",' &&
                     '"e_description" : ' && '"' && ME->CONVERT_TO_UTF8( I_TEXTO = WA_REQUISICAO-DESCRIPTION ) && '"' &&
                   '}'.
    ENDLOOP.
    E_JSON = E_JSON && E_JSON_REQ && ']}'.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_MM_REQUISICAO_ITENS.

    DATA: ITEMS               TYPE TABLE OF BAPIEBAN,
          ITEMSCC             TYPE TABLE OF BAPIEBKN,
          WA_REQUISICAO_ITENS	TYPE ZDE_REQITENS_APROVACAO,
          LC_DATA             TYPE C LENGTH 10,
          LC_VALOR            TYPE C LENGTH 20,
          LC_VALOR_DEC        TYPE ZDE_DECIMAL_20_2,
          E_JSON_REQ          TYPE STRING.

    DATA: I_REQUISICAO TYPE ZDE_REQUISICAO_APROVACAO.

    IF I_JSON IS NOT INITIAL.

      CALL METHOD /UI2/CL_JSON=>DESERIALIZE
        EXPORTING
          JSON = I_JSON
        CHANGING
          DATA = I_REQUISICAO.
    ELSE.
      I_REQUISICAO-BANFN = I_BANFN.
    ENDIF.

*---> 01/07/2023 - Migração S4 - EJ
    CALL FUNCTION 'BAPI_REQUISITION_GETDETAIL'            "#EC CI_USAGE_OK[2438131]
*<--- 01/07/2023 - Migração S4 - EJ
      EXPORTING
        NUMBER                         = I_REQUISICAO-BANFN
        ACCOUNT_ASSIGNMENT             = 'X'
      TABLES
        REQUISITION_ITEMS              = ITEMS
        REQUISITION_ACCOUNT_ASSIGNMENT = ITEMSCC.

    LOOP AT ITEMS INTO DATA(_ITEM).

      READ TABLE ITEMSCC INTO DATA(_ACCOUNTING_ACCOUNT) WITH KEY PREQ_ITEM = _ITEM-PREQ_ITEM.

      SELECT SINGLE BANPR
        FROM EBAN
        INTO @DATA(_STATUS)
       WHERE BANFN = @_ITEM-PREQ_NO
         AND BNFPO = @_ITEM-PREQ_ITEM.

      "05	Liberação concluída
      "08	Liberação recusada
      CHECK NOT ( ( _STATUS = '05' ) OR ( _STATUS = '08' ) ).

      SELECT SINGLE NAME1
        INTO @DATA(_BRANCH_NAME)
        FROM T001W
       WHERE WERKS = @_ITEM-PLANT
         AND SPRAS = @SY-LANGU.

      SELECT SINGLE TXT20
        INTO @DATA(_ACCOUNTING_ACCOUNT_NAME)
        FROM SKAT
       WHERE SAKNR = @_ACCOUNTING_ACCOUNT-G_L_ACCT
         AND SPRAS = @SY-LANGU.

      SELECT SINGLE LTEXT
        INTO @DATA(_COST_CENTER_NAME)
        FROM CSKT
       WHERE KOSTL = @_ACCOUNTING_ACCOUNT-COST_CTR
         AND SPRAS = @SY-LANGU.

      IF _ACCOUNTING_ACCOUNT-ORDER_NO IS NOT INITIAL.
        SELECT SINGLE KTEXT
          INTO @DATA(_ORDEM_TEXT)
          FROM AUFK
         WHERE AUFNR EQ @_ACCOUNTING_ACCOUNT-ORDER_NO.

        SHIFT _ACCOUNTING_ACCOUNT-ORDER_NO LEFT DELETING LEADING '0'.
      ENDIF.

      SHIFT _ITEM-MATERIAL LEFT DELETING LEADING '0'.
      SHIFT _ITEM-PREQ_ITEM  LEFT DELETING LEADING '0'.
      SHIFT _ACCOUNTING_ACCOUNT-G_L_ACCT LEFT DELETING LEADING '0'.
      SHIFT _ACCOUNTING_ACCOUNT-ASSET_NO LEFT DELETING LEADING '0'.
      SHIFT _ACCOUNTING_ACCOUNT-SUB_NUMBER LEFT DELETING LEADING '0'.
      SHIFT _ACCOUNTING_ACCOUNT-ORDER_NO LEFT DELETING LEADING '0'.

      WRITE _ITEM-PREQ_DATE TO LC_DATA DD/MM/YYYY.
      WA_REQUISICAO_ITENS-DT_REQUISICAO = LC_DATA.

      WA_REQUISICAO_ITENS-BANFN = _ITEM-PREQ_NO.
      WA_REQUISICAO_ITENS-BNFPO = _ITEM-PREQ_ITEM.

      WA_REQUISICAO_ITENS-MATERIAL_ID   = _ITEM-MATERIAL.
      WA_REQUISICAO_ITENS-MATERIAL_TXT  = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _ITEM-SHORT_TEXT ) ).
      WA_REQUISICAO_ITENS-BRANCH_ID     = _ITEM-PLANT.
      WA_REQUISICAO_ITENS-BRANCH_TXT    = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _BRANCH_NAME ) ).
      WA_REQUISICAO_ITENS-QUANTIDADE    = _ITEM-QUANTITY.
      WA_REQUISICAO_ITENS-VALOR_UN      = _ITEM-C_AMT_BAPI.
      WA_REQUISICAO_ITENS-VALOR         = _ITEM-C_AMT_BAPI * _ITEM-QUANTITY.

      WRITE _ITEM-QUANTITY TO LC_VALOR.
      WA_REQUISICAO_ITENS-QUANTIDADE_TXT = LC_VALOR.
      CONDENSE WA_REQUISICAO_ITENS-QUANTIDADE_TXT NO-GAPS.

      LC_VALOR_DEC = _ITEM-C_AMT_BAPI.
      WRITE LC_VALOR_DEC TO LC_VALOR.
      WA_REQUISICAO_ITENS-VALOR_UN_TXT = LC_VALOR.
      CONDENSE WA_REQUISICAO_ITENS-VALOR_UN_TXT NO-GAPS.

      LC_VALOR_DEC = WA_REQUISICAO_ITENS-VALOR.
      WRITE LC_VALOR_DEC TO LC_VALOR.
      WA_REQUISICAO_ITENS-VALOR_TXT = LC_VALOR.
      CONDENSE WA_REQUISICAO_ITENS-VALOR_TXT NO-GAPS.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          INPUT          = _ITEM-UNIT
        IMPORTING
          OUTPUT         = WA_REQUISICAO_ITENS-UNIDADE
        EXCEPTIONS
          UNIT_NOT_FOUND = 1
          OTHERS         = 2.

      IF _ACCOUNTING_ACCOUNT-ORDER_NO IS NOT INITIAL.
        WA_REQUISICAO_ITENS-ORDEM = _ACCOUNTING_ACCOUNT-ORDER_NO.
        WA_REQUISICAO_ITENS-ORDEM_TXT = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _ORDEM_TEXT ) ).
      ENDIF.

      IF _ACCOUNTING_ACCOUNT-ASSET_NO IS NOT INITIAL AND _ACCOUNTING_ACCOUNT-SUB_NUMBER IS NOT INITIAL.
        WA_REQUISICAO_ITENS-IMOBILIZADO = _ACCOUNTING_ACCOUNT-ASSET_NO.
        WA_REQUISICAO_ITENS-SUB_NUMBER  = _ACCOUNTING_ACCOUNT-SUB_NUMBER.
      ELSEIF _ACCOUNTING_ACCOUNT-ASSET_NO IS NOT INITIAL AND _ACCOUNTING_ACCOUNT-SUB_NUMBER IS INITIAL.
        WA_REQUISICAO_ITENS-IMOBILIZADO = _ACCOUNTING_ACCOUNT-ASSET_NO.
      ENDIF.

      WA_REQUISICAO_ITENS-CONTA_RAZAO      = _ACCOUNTING_ACCOUNT-G_L_ACCT.
      WA_REQUISICAO_ITENS-CONTA_RAZAO_TXT  = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _ACCOUNTING_ACCOUNT_NAME ) ).

      WA_REQUISICAO_ITENS-CENTRO_CUSTO     = _ACCOUNTING_ACCOUNT-COST_CTR.
      WA_REQUISICAO_ITENS-CENTRO_CUSTO_TXT = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _COST_CENTER_NAME ) ).

      WA_REQUISICAO_ITENS-CL_CONTABIL     = _ITEM-ACCTASSCAT.

      CASE _ITEM-ACCTASSCAT.
        WHEN 'K'.
          WA_REQUISICAO_ITENS-CL_CONTABIL_TXT = 'Centro de Custo'.
        WHEN 'A'.
          WA_REQUISICAO_ITENS-CL_CONTABIL_TXT = 'Imobilizado'.
        WHEN 'F'.
          WA_REQUISICAO_ITENS-CL_CONTABIL_TXT = 'Ordem Interna'.
        WHEN OTHERS.
          WA_REQUISICAO_ITENS-CL_CONTABIL_TXT = 'Estoque'.
      ENDCASE.

      TRY .
          WA_REQUISICAO_ITENS-REQUISITANTE = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( ZCL_USER=>ZIF_USER~GET_NAME_USER( I_USER = _ITEM-CREATED_BY ) ) ).
        CATCH ZCX_USER.
          WA_REQUISICAO_ITENS-REQUISITANTE = _ITEM-CREATED_BY.
      ENDTRY.

      APPEND WA_REQUISICAO_ITENS TO E_REQUISICAO_ITENS.
    ENDLOOP.

    E_JSON = '{ "requisicoes" : [ '.
    CLEAR: E_JSON_REQ.

    LOOP AT E_REQUISICAO_ITENS INTO WA_REQUISICAO_ITENS.
      IF E_JSON_REQ IS NOT INITIAL.
        E_JSON_REQ = E_JSON_REQ && ','.
      ENDIF.
      E_JSON_REQ = E_JSON_REQ &&
                    '{' &&
                     '"e_banfn" : ' && '"' && WA_REQUISICAO_ITENS-BANFN && '",' &&
                     '"e_bnfpo" : ' && '"' && WA_REQUISICAO_ITENS-BNFPO && '",' &&
                     '"e_dt_requisicao" : ' && '"' && WA_REQUISICAO_ITENS-DT_REQUISICAO && '",' &&
                     '"e_material_id" : ' && '"' && WA_REQUISICAO_ITENS-MATERIAL_ID && '",' &&
                     '"e_material_txt" : ' && '"' && WA_REQUISICAO_ITENS-MATERIAL_TXT && '",' &&
                     '"e_branch_id" : ' && '"' && WA_REQUISICAO_ITENS-BRANCH_ID && '",' &&
                     '"e_branch_txt" : ' && '"' && WA_REQUISICAO_ITENS-BRANCH_TXT && '",' &&
                     '"e_unidade" : ' && '"' && WA_REQUISICAO_ITENS-UNIDADE && '",' &&
                     '"e_quantidade" : ' && '"' && WA_REQUISICAO_ITENS-QUANTIDADE && '",' &&
                     '"e_valor_un" : ' && '"' && WA_REQUISICAO_ITENS-VALOR_UN && '",' &&
                     '"e_valor" : ' && '"' && WA_REQUISICAO_ITENS-VALOR && '",' &&
                     '"e_quantidade_txt" : ' && '"' && WA_REQUISICAO_ITENS-QUANTIDADE_TXT && '",' &&
                     '"e_valor_un_txt" : ' && '"' && WA_REQUISICAO_ITENS-VALOR_UN_TXT && '",' &&
                     '"e_valor_txt" : ' && '"' && WA_REQUISICAO_ITENS-VALOR_TXT && '",' &&
                     '"e_ordem" : ' && '"' && WA_REQUISICAO_ITENS-ORDEM && '",' &&
                     '"e_ordem_txt" : ' && '"' && WA_REQUISICAO_ITENS-ORDEM_TXT && '",' &&
                     '"e_imobilizado" : ' && '"' && WA_REQUISICAO_ITENS-IMOBILIZADO && '",' &&
                     '"e_sub_number" : ' && '"' && WA_REQUISICAO_ITENS-SUB_NUMBER && '",' &&
                     '"e_conta_razao" : ' && '"' && WA_REQUISICAO_ITENS-CONTA_RAZAO && '",' &&
                     '"e_conta_razao_txt" : ' && '"' && WA_REQUISICAO_ITENS-CONTA_RAZAO_TXT && '",' &&
                     '"e_centro_custo" : ' && '"' && WA_REQUISICAO_ITENS-CENTRO_CUSTO && '",' &&
                     '"e_centro_custo_txt" : ' && '"' && WA_REQUISICAO_ITENS-CENTRO_CUSTO_TXT && '",' &&
                     '"e_cl_contabil" : ' && '"' && WA_REQUISICAO_ITENS-CL_CONTABIL && '",' &&
                     '"e_cl_contabil_txt" : ' && '"' && WA_REQUISICAO_ITENS-CL_CONTABIL_TXT && '",' &&
                     '"e_requisitante" : ' && '"' && WA_REQUISICAO_ITENS-REQUISITANTE && '"' &&
                   '}'.
    ENDLOOP.
    E_JSON = E_JSON && E_JSON_REQ && ']}'.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_MM_RESERVA_APROVACAO.

    DATA: INBOX_ITEMS    TYPE TABLE OF SWWWLHEAD,
          DECISION_TITLE TYPE  SWR_WIHDR-WI_TEXT,
          ALTERNATIVES   TYPE TABLE OF SWR_DECIALTS,
          RESERVATION    TYPE BAPI2093_RES_ITEM_DETAIL-RES_NO,
          RETURN_CODE    TYPE SY-SUBRC,
          LC_DATA        TYPE C LENGTH 10,
          LC_VALOR       TYPE C LENGTH 14,
          LC_VALOR_DEC   TYPE ZDE_DECIMAL_20_2,
          WA_RESERVAS	   TYPE ZDE_RESERVA_APROVACAO,
          E_JSON_RES     TYPE STRING,
          FILTRO         TYPE TABLE OF RHOBJECTS.

    FILTRO = VALUE #( ( OBJECT = 'TS99900017' ) ).

    CALL FUNCTION 'RH_INBOX_VIEW_CREATE'
*      EXPORTING
*       SEARCH_DATE       = SY-DATUM
*       READ_OBJECT_TEXT  =
*       NO_WI_SELECTION   =
*       NO_HEADER_SELECTION       =
*       INBOX_USER        = USERNAME
*       IV_DO_COMMIT      = 'X'
      TABLES
*       INBOX_VIEW        =
        WI_HEAD           = INBOX_ITEMS
*       WI_STATUS         =
        TASK_FILTER       = FILTRO
      EXCEPTIONS
        NO_ACTIVE_PLVAR   = 1
        NO_TASKS_FOUND    = 2
        USER_NOT_DEFINED  = 3
        NO_WORKITEM_FOUND = 4
        OTHERS            = 5.

    IF ( SY-SUBRC IS INITIAL ).

      DELETE INBOX_ITEMS WHERE WI_RH_TASK NE 'TS99900017'.

      LOOP AT INBOX_ITEMS INTO DATA(_ITEM).

        CLEAR WA_RESERVAS.

        "//Get purchase order number
        CALL FUNCTION 'SAP_WAPI_DECISION_READ'
          EXPORTING
            WORKITEM_ID    = _ITEM-WI_ID
          IMPORTING
            DECISION_TITLE = DECISION_TITLE
          TABLES
            ALTERNATIVES   = ALTERNATIVES.

        RESERVATION = DECISION_TITLE+28(10).

        SELECT SINGLE *
          FROM RESB
          INTO @DATA(_IS_AVAILABLE)
         WHERE RSNUM = @RESERVATION
           AND XLOEK = @ABAP_FALSE
           AND XWAOK = @ABAP_FALSE.

        IF SY-SUBRC IS NOT INITIAL.
          CALL FUNCTION 'SAP_WAPI_WORKITEM_DELETE'
            EXPORTING
              WORKITEM_ID       = _ITEM-WI_ID
              ACTUAL_AGENT      = SY-UNAME
              LANGUAGE          = SY-LANGU
              DO_COMMIT         = ABAP_TRUE
              CHECK_FINAL_STATE = ABAP_FALSE
            IMPORTING
              RETURN_CODE       = RETURN_CODE.
          CONTINUE.
        ENDIF.

        WA_RESERVAS-WI_ID = _ITEM-WI_ID.
        WA_RESERVAS-WI_CD = _ITEM-WI_CD.
        WA_RESERVAS-WI_CT = _ITEM-WI_CT.

        WRITE _ITEM-WI_CD TO LC_DATA DD/MM/YYYY.
        WA_RESERVAS-WI_CD_TXT = LC_DATA.

        "//Selects
        SELECT SINGLE *
          FROM RKPF
          INTO @DATA(_RESERVATION_HEADER)
         WHERE RSNUM EQ @RESERVATION.

        SELECT SINGLE LTEXT
          INTO @DATA(_COST_CENTER_NAME)
          FROM CSKT
         WHERE KOSTL EQ @_RESERVATION_HEADER-KOSTL
           AND SPRAS EQ @SY-LANGU.

        SELECT MATNR, BDMNG, WERKS, WAERS
          FROM RESB
          INTO TABLE @DATA(_RESERVATION_ITEMS)
         WHERE RSNUM = @RESERVATION.

        "//Calculate the material price
        LC_VALOR_DEC = 0.

        LOOP AT _RESERVATION_ITEMS INTO DATA(_RESERVATION_ITEM).

          WA_RESERVAS-WAERS = _RESERVATION_ITEM-WAERS.

          SELECT SINGLE VERPR
            FROM MBEW
            INTO @DATA(_MATERIAL_PRICE)
           WHERE MATNR = @_RESERVATION_ITEM-MATNR
             AND BWKEY = @_RESERVATION_ITEM-WERKS.

          LC_VALOR_DEC = LC_VALOR_DEC + ( _MATERIAL_PRICE * _RESERVATION_ITEM-BDMNG ).
        ENDLOOP.
        WRITE LC_VALOR_DEC TO LC_VALOR.
        CONDENSE LC_VALOR NO-GAPS.
        WA_RESERVAS-BRTWR_TXT = LC_VALOR.
        WA_RESERVAS-BRTWR = LC_VALOR_DEC.

        WRITE _RESERVATION_HEADER-RSDAT TO LC_DATA DD/MM/YYYY.
        WA_RESERVAS-DT_RESERVA = LC_DATA.
        WA_RESERVAS-RSNUM      = _RESERVATION_HEADER-RSNUM.

        TRY .
            WA_RESERVAS-DESCRIPTION = 'C.Custo: ' && _RESERVATION_HEADER-KOSTL && ' - ' && _COST_CENTER_NAME &&
                                      'Requisitor: ' && ZCL_USER=>ZIF_USER~GET_NAME_USER( I_USER = _RESERVATION_HEADER-USNAM ).
          CATCH ZCX_USER.
            WA_RESERVAS-DESCRIPTION =
                                      'C.Custo: ' && _RESERVATION_HEADER-KOSTL && ' - ' && _COST_CENTER_NAME &&
                                      'Requisitor: ' && _RESERVATION_HEADER-USNAM.
        ENDTRY.

        WA_RESERVAS-DESCRIPTION = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = WA_RESERVAS-DESCRIPTION ).

        APPEND WA_RESERVAS TO E_RESERVAS.

      ENDLOOP.
    ENDIF.

    SORT E_RESERVAS BY RSNUM.
    DELETE ADJACENT DUPLICATES FROM E_RESERVAS COMPARING RSNUM.

    E_JSON = '{ "reservas" : [ '.
    CLEAR: E_JSON_RES.

    LOOP AT E_RESERVAS INTO WA_RESERVAS.
      IF E_JSON_RES IS NOT INITIAL.
        E_JSON_RES = E_JSON_RES && ','.
      ENDIF.
      E_JSON_RES = E_JSON_RES &&
                    '{' &&
                     '"e_rsnum" : ' && '"' && WA_RESERVAS-RSNUM && '",' &&
                     '"e_dt_reserva" : ' && '"' && WA_RESERVAS-DT_RESERVA && '",' &&
                     '"e_waers" : ' && '"' && WA_RESERVAS-WAERS && '",' &&
                     '"e_brtwr" : ' && '"' && WA_RESERVAS-BRTWR && '",' &&
                     '"e_brtwr_txt" : ' && '"' && WA_RESERVAS-BRTWR_TXT && '",' &&
                     '"e_wi_id" : ' && '"' && WA_RESERVAS-WI_ID && '",' &&
                     '"e_wi_cd" : ' && '"' && WA_RESERVAS-WI_CD && '",' &&
                     '"e_wi_cd_txt" : ' && '"' && WA_RESERVAS-WI_CD_TXT && '",' &&
                     '"e_wi_ct" : ' && '"' && WA_RESERVAS-WI_CT && '",' &&
                     '"e_description" : ' && '"' && ME->CONVERT_TO_UTF8( I_TEXTO = WA_RESERVAS-DESCRIPTION ) && '"' &&
                   '}'.
    ENDLOOP.
    E_JSON = E_JSON && E_JSON_RES && ']}'.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_MM_RESERVA_ITENS.

    DATA: ITEMS        TYPE TABLE OF BAPI2093_RES_ITEM_DETAIL,
          RETURNS      TYPE TABLE OF BAPIRET2,
          WA_RESERVA   TYPE ZDE_RESITENS_APROVACAO,
          LC_DATA      TYPE C LENGTH 10,
          LC_VALOR     TYPE C LENGTH 20,
          LC_VALOR_DEC TYPE ZDE_DECIMAL_20_2,
          E_JSON_RES   TYPE STRING,
          I_RESERVA    TYPE ZDE_RESERVA_APROVACAO.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = I_RESERVA.

*---> 01/07/2023 - Migração S4 - EJ
    CALL FUNCTION 'BAPI_RESERVATION_GETDETAIL1'     "#EC CI_USAGE_OK[2438131]
*<--- 01/07/2023 - Migração S4 - EJ
      EXPORTING
        RESERVATION       = I_RESERVA-RSNUM
      TABLES
        RESERVATION_ITEMS = ITEMS
        RETURN            = RETURNS.

    LOOP AT ITEMS INTO DATA(_ITEM).

      CLEAR: WA_RESERVA.

      SELECT SINGLE XLOEK, XWAOK
        FROM RESB
        INTO @DATA(_RESERVATION_ITEM)
       WHERE RSNUM = @_ITEM-RES_NO
         AND RSPOS = @_ITEM-RES_ITEM.

      CHECK NOT ( _RESERVATION_ITEM-XLOEK = ABAP_TRUE OR _RESERVATION_ITEM-XWAOK = ABAP_TRUE ).

      SELECT SINGLE MATNR, MAKTX
        INTO @DATA(_MATERIAL)
        FROM MAKT
       WHERE MATNR EQ @_ITEM-MATERIAL
         AND SPRAS EQ @SY-LANGU.
*---> 01/07/2023 - Migração S4 - EJ
        IF sy-subrc IS NOT INITIAL.

          SELECT SINGLE matnr,
                        maktx
            INTO @_material
            FROM makt
           WHERE matnr EQ @_item-material_long
             AND spras EQ @sy-langu.

        ENDIF.
*<--- 01/07/2023 - Migração S4 - EJ

      SELECT SINGLE WERKS, NAME1
        INTO @DATA(_BRANCH)
        FROM T001W
       WHERE WERKS EQ @_ITEM-PLANT
         AND SPRAS EQ @SY-LANGU.

      SELECT SINGLE SAKNR, TXT20
        INTO @DATA(_ACCOUNT)
        FROM SKAT
       WHERE SAKNR EQ @_ITEM-G_L_ACCT
         AND SPRAS EQ @SY-LANGU.

      "//Select cost center
      SELECT SINGLE *
        FROM RKPF
        INTO @DATA(WA_RKPF)
       WHERE RSNUM EQ @_ITEM-RES_NO.

      SELECT SINGLE LTEXT
        INTO @DATA(_COST_CENTER_NAME)
        FROM CSKT
       WHERE KOSTL EQ @WA_RKPF-KOSTL
         AND SPRAS EQ @SY-LANGU.

      "//Get material price
      SELECT SINGLE VERPR
        FROM MBEW
        INTO @DATA(_MATERIAL_PRICE)
       WHERE MATNR EQ @_ITEM-MATERIAL
         AND BWKEY EQ @_ITEM-PLANT.
*---> 01/07/2023 - Migração S4 - EJ
        IF sy-subrc IS NOT INITIAL.

          SELECT SINGLE verpr
            INTO @_material_price
            FROM mbew
           WHERE matnr EQ @_item-material_long
             AND bwkey EQ @_item-plant.

        ENDIF.
*<--- 01/07/2023 - Migração S4 - EJ

      SHIFT _MATERIAL-MATNR LEFT DELETING LEADING '0'.
      SHIFT _ACCOUNT-SAKNR LEFT DELETING LEADING '0'.
      SHIFT _ITEM-RES_NO    LEFT DELETING LEADING '0'.
      SHIFT WA_RKPF-KOSTL LEFT DELETING LEADING '0'.
      SHIFT _ITEM-RES_ITEM  LEFT DELETING LEADING '0'.

      WRITE _ITEM-REQ_DATE TO LC_DATA DD/MM/YYYY.
      WA_RESERVA-DT_RESERVA = LC_DATA.

      WA_RESERVA-RSNUM = _ITEM-RES_NO.
      WA_RESERVA-RSPOS = _ITEM-RES_ITEM.

      WA_RESERVA-MATERIAL_ID   = _MATERIAL-MATNR.
      WA_RESERVA-MATERIAL_TXT  = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _MATERIAL-MAKTX ) ).
      WA_RESERVA-BRANCH_ID     = _ITEM-PLANT.
      WA_RESERVA-BRANCH_TXT    = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _BRANCH-NAME1 ) ).
      WA_RESERVA-QUANTIDADE    = _ITEM-QUANTITY.
      WA_RESERVA-VALOR_UN      = _MATERIAL_PRICE.
      WA_RESERVA-VALOR         = _MATERIAL_PRICE * _ITEM-QUANTITY.

      WRITE _ITEM-QUANTITY TO LC_VALOR.
      WA_RESERVA-QUANTIDADE_TXT = LC_VALOR.
      CONDENSE WA_RESERVA-QUANTIDADE_TXT NO-GAPS.

      LC_VALOR_DEC = _MATERIAL_PRICE.
      WRITE LC_VALOR_DEC TO LC_VALOR.
      WA_RESERVA-VALOR_UN_TXT = LC_VALOR.
      CONDENSE WA_RESERVA-VALOR_UN_TXT NO-GAPS.

      LC_VALOR_DEC = WA_RESERVA-VALOR.
      WRITE LC_VALOR_DEC TO LC_VALOR.
      WA_RESERVA-VALOR_TXT = LC_VALOR.
      CONDENSE WA_RESERVA-VALOR_TXT NO-GAPS.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          INPUT          = _ITEM-ENTRY_UOM
        IMPORTING
          OUTPUT         = WA_RESERVA-UNIDADE
        EXCEPTIONS
          UNIT_NOT_FOUND = 1
          OTHERS         = 2.

*      IF _ACCOUNTING_ACCOUNT-ORDER_NO IS NOT INITIAL.
*        WA_RESERVA-ORDEM = _ACCOUNTING_ACCOUNT-ORDER_NO.
*        WA_RESERVA-ORDEM_TXT = _ORDEM_TEXT.
*      ENDIF.

*      IF _ACCOUNTING_ACCOUNT-ASSET_NO IS NOT INITIAL AND _ACCOUNTING_ACCOUNT-SUB_NUMBER IS NOT INITIAL.
*        WA_RESERVA-IMOBILIZADO = _ACCOUNTING_ACCOUNT-ASSET_NO.
*        WA_RESERVA-SUB_NUMBER  = _ACCOUNTING_ACCOUNT-SUB_NUMBER.
*      ELSEIF _ACCOUNTING_ACCOUNT-ASSET_NO IS NOT INITIAL AND _ACCOUNTING_ACCOUNT-SUB_NUMBER IS INITIAL.
*        WA_RESERVA-IMOBILIZADO = _ACCOUNTING_ACCOUNT-ASSET_NO.
*      ENDIF.

      WA_RESERVA-CONTA_RAZAO      = _ACCOUNT-SAKNR.
      WA_RESERVA-CONTA_RAZAO_TXT  = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _ACCOUNT-TXT20 ) ).

      WA_RESERVA-CENTRO_CUSTO     = WA_RKPF-KOSTL.
      WA_RESERVA-CENTRO_CUSTO_TXT = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( _COST_CENTER_NAME ) ).

      WA_RESERVA-CL_CONTABIL     = _ITEM-ACCTASSCAT.

      CASE _ITEM-ACCTASSCAT.
        WHEN 'K'.
          WA_RESERVA-CL_CONTABIL_TXT = 'Centro de Custo'.
        WHEN 'A'.
          WA_RESERVA-CL_CONTABIL_TXT = 'Imobilizado'.
        WHEN 'F'.
          WA_RESERVA-CL_CONTABIL_TXT = 'Ordem Interna'.
        WHEN OTHERS.
          WA_RESERVA-CL_CONTABIL_TXT = 'Estoque'.
      ENDCASE.

      TRY .
          WA_RESERVA-REQUISITANTE = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( EXPORTING I_TEXTO = CONV #( ZCL_USER=>ZIF_USER~GET_NAME_USER( I_USER = WA_RKPF-USNAM ) ) ).
        CATCH ZCX_USER.
          WA_RESERVA-REQUISITANTE = WA_RKPF-USNAM.
      ENDTRY.

      APPEND WA_RESERVA TO E_RESERVA_ITENS.
    ENDLOOP.

    E_JSON = '{ "reservas" : [ '.
    CLEAR: E_JSON_RES.

    LOOP AT E_RESERVA_ITENS INTO WA_RESERVA.
      IF E_JSON_RES IS NOT INITIAL.
        E_JSON_RES = E_JSON_RES && ','.
      ENDIF.
      E_JSON_RES = E_JSON_RES &&
                    '{' &&
                     '"e_rsnum" : ' && '"' && WA_RESERVA-RSNUM && '",' &&
                     '"e_rspos" : ' && '"' && WA_RESERVA-RSPOS && '",' &&
                     '"e_dt_reserva" : ' && '"' && WA_RESERVA-DT_RESERVA && '",' &&
                     '"e_material_id" : ' && '"' && WA_RESERVA-MATERIAL_ID && '",' &&
                     '"e_material_txt" : ' && '"' && WA_RESERVA-MATERIAL_TXT && '",' &&
                     '"e_branch_id" : ' && '"' && WA_RESERVA-BRANCH_ID && '",' &&
                     '"e_branch_txt" : ' && '"' && WA_RESERVA-BRANCH_TXT && '",' &&
                     '"e_unidade" : ' && '"' && WA_RESERVA-UNIDADE && '",' &&
                     '"e_quantidade" : ' && '"' && WA_RESERVA-QUANTIDADE && '",' &&
                     '"e_valor_un" : ' && '"' && WA_RESERVA-VALOR_UN && '",' &&
                     '"e_valor" : ' && '"' && WA_RESERVA-VALOR && '",' &&
                     '"e_quantidade_txt" : ' && '"' && WA_RESERVA-QUANTIDADE_TXT && '",' &&
                     '"e_valor_un_txt" : ' && '"' && WA_RESERVA-VALOR_UN_TXT && '",' &&
                     '"e_valor_txt" : ' && '"' && WA_RESERVA-VALOR_TXT && '",' &&
                     '"e_ordem" : ' && '"' && WA_RESERVA-ORDEM && '",' &&
                     '"e_ordem_txt" : ' && '"' && WA_RESERVA-ORDEM_TXT && '",' &&
                     '"e_imobilizado" : ' && '"' && WA_RESERVA-IMOBILIZADO && '",' &&
                     '"e_sub_number" : ' && '"' && WA_RESERVA-SUB_NUMBER && '",' &&
                     '"e_conta_razao" : ' && '"' && WA_RESERVA-CONTA_RAZAO && '",' &&
                     '"e_conta_razao_txt" : ' && '"' && WA_RESERVA-CONTA_RAZAO_TXT && '",' &&
                     '"e_centro_custo" : ' && '"' && WA_RESERVA-CENTRO_CUSTO && '",' &&
                     '"e_centro_custo_txt" : ' && '"' && WA_RESERVA-CENTRO_CUSTO_TXT && '",' &&
                     '"e_cl_contabil" : ' && '"' && WA_RESERVA-CL_CONTABIL && '",' &&
                     '"e_cl_contabil_txt" : ' && '"' && WA_RESERVA-CL_CONTABIL_TXT && '",' &&
                     '"e_requisitante" : ' && '"' && WA_RESERVA-REQUISITANTE && '"' &&
                   '}'.
    ENDLOOP.
    E_JSON = E_JSON && E_JSON_RES && ']}'.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_PAG_IMPOSTOS.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST05'
      IMPORTING
        OUT_JSON = E_JSON.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_PAG_INVOICES.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST06'
      IMPORTING
        OUT_JSON = E_JSON.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_PM_ORC_ORDEM.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST08'
      IMPORTING
        OUT_JSON = E_JSON.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_PM_ORC_SUPLE_ORDEM.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST15'
      IMPORTING
        OUT_JSON = E_JSON.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_QTD_TAREFAS_MM.

    DATA: ITEMS TYPE TABLE OF SWWWLHEAD.

    DATA(LC_USER) = SY-UNAME.

    CALL FUNCTION 'RH_INBOX_VIEW_CREATE'
      EXPORTING
*       SEARCH_DATE       = SY-DATUM
*       READ_OBJECT_TEXT  =
*       NO_WI_SELECTION   =
*       NO_HEADER_SELECTION       =
        INBOX_USER        = LC_USER
*       IV_DO_COMMIT      = 'X'
      TABLES
*       INBOX_VIEW        =
        WI_HEAD           = ITEMS
*       WI_STATUS         =
*       TASK_FILTER       =
      EXCEPTIONS
        NO_ACTIVE_PLVAR   = 1
        NO_TASKS_FOUND    = 2
        USER_NOT_DEFINED  = 3
        NO_WORKITEM_FOUND = 4
        OTHERS            = 5.

    IF SY-SUBRC IS NOT INITIAL.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL_SYS( ).
    ENDIF.

    "Pedido de compra
    DELETE ITEMS WHERE WI_RH_TASK NE 'TS20000166'.
    "Reservas
    DELETE ITEMS WHERE WI_RH_TASK NE 'TS99900017'.
    "Requisições
    DELETE ITEMS WHERE WI_RH_TASK NE 'TS00007986'.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_QTD_TAREFAS_USUARIO.

    DATA: E_QTD_RH_MOV_PESSOAL TYPE I,
          E_QTD_PM_ORC_ORDEM   TYPE NUMC2,
          E_QTD_PM_ORC_ORDEM_I TYPE I.

    DATA: IT_LOTES_FI    TYPE TABLE OF ZFI_LOTES_IMP,
          IT_LOTES_IV    TYPE TABLE OF ZIV_LOTES_IMP,
          IT_EMB_INSUMOS TYPE TABLE OF ZSD_ORD_VENDAS_EST,
          IT_LIB_CREDITO TYPE TABLE OF ZSD_ROMA_IMP,
          IT_SOL_OV      TYPE TABLE OF ZSDS018,
          IT_INVEST      TYPE TABLE OF ZFI_GRU_INV,
          IT_PAG_SAL     TYPE TABLE OF ZFI_LOTES_FOL,
          IT_PM_SUPLE    TYPE TABLE OF ZPMR0006.

    DATA: IT_ESTRA_FI TYPE TABLE OF ZFI_ESTRATEGIA_IMP,
          IT_ESTRA_GL TYPE TABLE OF ZFI_ESTRATEGIA_ZGL,
          IT_ESTRA_IV TYPE TABLE OF ZFI_ESTRATEGIA_IMP,
          IT_ESTRA_IS TYPE TABLE OF ZSD_ESTRATEGIA_OV,
          IT_ESTRA_CD TYPE TABLE OF ZSD_ESTRATEGIA_SD,
          IT_ESTRA_SV TYPE TABLE OF ZSDS019,
          IT_ESTRA_SA TYPE TABLE OF ZFI_ESTRATEGIA_FOL,
          IT_ESTRA_IN TYPE TABLE OF ZFI_ESTRATEGIA_ZIM.

    ME->ZIF_FMCALL_APP_MOBILE~GET_MM_QTD_TAREFAS(
      IMPORTING
        E_QTD_PEDIDOS       = DATA(E_QTD_PEDIDOS)
        E_QTD_REQUISICAO    = DATA(E_QTD_REQUISICAO)
        E_QTD_RESERVA       = DATA(E_QTD_RESERVA)
        E_QTD_ADIANTAMENTOS = DATA(E_QTD_ADIANTAMENTOS) ).

    " Pagamentos de Impostos """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_FI_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
      TABLES
        T_LOTES   = IT_LOTES_FI
        T_ESTRA   = IT_ESTRA_FI.

    DELETE IT_ESTRA_FI WHERE OPCOES NE ICON_SET_STATE.
    SORT IT_ESTRA_FI BY LOTE.

    LOOP AT IT_LOTES_FI INTO DATA(WA_LOTES_FI).
      READ TABLE IT_ESTRA_FI WITH KEY LOTE = WA_LOTES_FI-LOTE TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        DELETE IT_LOTES_FI WHERE LOTE EQ WA_LOTES_FI-LOTE.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE IT_LOTES_FI LINES DATA(E_QTD_APG_IMPOSTOS).

    " ZGL - Lançamento Contábeis """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_GL_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
      TABLES
        T_LOTES   = IT_LOTES_FI
        T_ESTRA   = IT_ESTRA_GL.

    DELETE IT_ESTRA_GL WHERE OPCOES NE ICON_SET_STATE.
    SORT IT_ESTRA_GL BY LOTE.

    LOOP AT IT_LOTES_FI INTO WA_LOTES_FI.
      READ TABLE IT_ESTRA_GL WITH KEY LOTE = WA_LOTES_FI-LOTE TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        DELETE IT_LOTES_FI WHERE LOTE EQ WA_LOTES_FI-LOTE.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE IT_LOTES_FI LINES DATA(E_QTD_ZGL_CONTABIL).

    " Pagamento de Invoice """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_IV_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
      TABLES
        T_LOTES   = IT_LOTES_IV
        T_ESTRA   = IT_ESTRA_IV.

    DELETE IT_ESTRA_IV WHERE OPCOES NE ICON_SET_STATE.
    SORT IT_ESTRA_IV BY LOTE.

    LOOP AT IT_LOTES_IV INTO DATA(WA_LOTES_IV).
      READ TABLE IT_ESTRA_IV WITH KEY LOTE = WA_LOTES_IV-LOTE TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        DELETE IT_LOTES_IV WHERE LOTE EQ WA_LOTES_IV-LOTE.
      ENDIF.
    ENDLOOP.

    DELETE IT_LOTES_IV WHERE STATUS NE SPACE.
    DESCRIBE TABLE IT_LOTES_IV LINES DATA(E_QTD_PAG_INVOICES).

    " Liberação de Embarques Insumos """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_OV_ESTRATEGIA_LISTA'
      EXPORTING
        I_USUARIO = SY-UNAME
      TABLES
        T_ORDENS  = IT_EMB_INSUMOS
        T_ESTRA   = IT_ESTRA_IS.

    DELETE IT_ESTRA_IS WHERE OPCOES <> ICON_SET_STATE.
    SORT IT_ESTRA_IS BY VBELN.

    LOOP AT IT_EMB_INSUMOS INTO DATA(WA_EMB_INSUMOS).
      READ TABLE IT_ESTRA_IS WITH KEY VBELN = WA_EMB_INSUMOS-VBELN TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        DELETE IT_EMB_INSUMOS WHERE VBELN EQ WA_EMB_INSUMOS-VBELN.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE IT_EMB_INSUMOS LINES DATA(E_QTD_LIB_EMB_INSUMOS).

    " Faturamento sem Limite Credito """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_SD_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
      TABLES
        T_ORDENS  = IT_LIB_CREDITO
        T_ESTRA   = IT_ESTRA_CD.

    DELETE IT_ESTRA_CD WHERE OPCOES <> ICON_SET_STATE.
    SORT IT_ESTRA_CD BY LOTE.

    LOOP AT IT_LIB_CREDITO INTO DATA(WA_LIB_CREDITO).
      READ TABLE IT_ESTRA_CD WITH KEY LOTE = WA_LIB_CREDITO-LOTE TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        DELETE IT_LIB_CREDITO WHERE LOTE EQ WA_LIB_CREDITO-LOTE.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE IT_LIB_CREDITO LINES DATA(E_QTD_FAT_LIM_CREDITO).

    " Solicitação de Ordem de Venda """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_SOL_OV_ESTRAT_LISTA'
      EXPORTING
        V_USUARIO      = SY-UNAME
      TABLES
        T_SOLICITACOES = IT_SOL_OV
        T_ESTRA        = IT_ESTRA_SV.

    DELETE IT_ESTRA_SV WHERE OPCOES <> ICON_SET_STATE.
    SORT IT_ESTRA_SV BY NRO_SOL_OV.

    LOOP AT IT_SOL_OV INTO DATA(WA_SOL_OV).
      READ TABLE IT_ESTRA_SV WITH KEY NRO_SOL_OV = WA_SOL_OV-NRO_SOL_OV TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        DELETE IT_SOL_OV WHERE NRO_SOL_OV EQ WA_SOL_OV-NRO_SOL_OV.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE IT_SOL_OV LINES DATA(E_QTD_SOL_ORD_VENDA).

    " Pagamento de Salários """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_FL_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
      TABLES
        T_LOTES   = IT_PAG_SAL
        T_ESTRA   = IT_ESTRA_SA.

    DELETE IT_ESTRA_SA WHERE OPCOES <> ICON_SET_STATE.
    SORT IT_ESTRA_SA BY LOTE.

    LOOP AT IT_PAG_SAL INTO DATA(WA_PAG_SAL).
      READ TABLE IT_ESTRA_SA WITH KEY LOTE = WA_PAG_SAL-LOTE TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL.
        DELETE IT_PAG_SAL WHERE LOTE EQ WA_PAG_SAL-LOTE.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE IT_PAG_SAL LINES DATA(E_QTD_RH_PAG_SALARIO).

    " Movimentação Pessoal """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    " Orçamento Ordem de PM """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_LISTA_ORDEM_PERMITS'
      EXPORTING
        I_USER  = SY-UNAME
      IMPORTING
        E_LINES = E_QTD_PM_ORC_ORDEM.
    E_QTD_PM_ORC_ORDEM_I = E_QTD_PM_ORC_ORDEM.

    "Orçamento Ordem de PM (Suplemento) """"""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'ZPM_LISTAR_SUPLEMENTOS1'
      EXPORTING
        USERNAME      = SY-UNAME
      TABLES
        T_SUPLEMENTOS = IT_PM_SUPLE.
    DESCRIBE TABLE IT_PM_SUPLE LINES DATA(E_QTD_PM_ORC_SUPLE).

    " Orçamento Investimento """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'Z_IM_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
      TABLES
        T_LOTES   = IT_INVEST
        T_ESTRA   = IT_ESTRA_IN.

    DELETE IT_ESTRA_IN WHERE OPCOES <> ICON_SET_STATE.
    SORT IT_ESTRA_IN BY LOTE KOSTL.

    LOOP AT IT_INVEST INTO DATA(WA_INVEST).
      READ TABLE IT_ESTRA_IN WITH KEY LOTE = WA_INVEST-LOTE KOSTL = WA_INVEST-KOSTL TRANSPORTING NO FIELDS BINARY SEARCH.
      IF SY-SUBRC IS NOT INITIAL.
        DELETE IT_INVEST WHERE LOTE = WA_INVEST-LOTE AND KOSTL EQ WA_INVEST-KOSTL.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE IT_INVEST LINES DATA(E_QTD_IM_INVEST).

    DATA(E_QTD_COMPRAS) = E_QTD_PEDIDOS + E_QTD_REQUISICAO + E_QTD_RESERVA + E_QTD_ADIANTAMENTOS.
    DATA(E_QTD_CSC_FINAN) = E_QTD_APG_IMPOSTOS + E_QTD_ZGL_CONTABIL.
    DATA(E_QTD_SD_MERC_INTERNO) = E_QTD_FAT_LIM_CREDITO + E_QTD_SOL_ORD_VENDA.
    DATA(E_QTD_RH_MERC_INTERNO) = E_QTD_RH_PAG_SALARIO + E_QTD_RH_MOV_PESSOAL.
    DATA(E_QTD_PM) = E_QTD_PM_ORC_ORDEM_I + E_QTD_PM_ORC_SUPLE.

    E_JSON =
'{ "itens" : [' &&
'  { "id" : "1", ' &&
'    "area" : "Compras", ' &&
'    "msg_vazia" : "Nenhuma Atividade para Compras", ' &&
'    "qtd_tarefas" : "' && E_QTD_COMPRAS && '",' &&
'    "processos" : [ ' &&
'         { "nome" : "Pedido(s)", ' &&
'           "qtd"  : "' && E_QTD_PEDIDOS    && '" }, ' &&
'         { "nome" : "Requisição(ões)",' &&
'           "qtd"  : "' && E_QTD_REQUISICAO && '"  },' &&
'         { "nome" : "Reserva(s)",' &&
'           "qtd"  : "' && E_QTD_RESERVA    && '" },' &&
'         { "nome" : "Adiantamento(s) a Fornecedor",' &&
'           "qtd"  : "' && E_QTD_ADIANTAMENTOS && '" }' &&
'      ]' &&
'   },' &&
'  { "id" : "2", ' &&
'    "area" : "CSC – Financeiro",' &&
'    "msg_vazia" : "Nenhuma Atividade para CSC – Financeiro",' &&
'    "qtd_tarefas" : "' && E_QTD_CSC_FINAN && '",' &&
'    "processos" : [' &&
'         { "nome" : "Pagamento(s) de Impostos",' &&
'           "qtd"  : "' && E_QTD_APG_IMPOSTOS && '" },' &&
'         { "nome" : "ZGL - Lançamento(s) Manual(is)",' &&
'           "qtd"  : "' && E_QTD_ZGL_CONTABIL && '" }' &&
'      ]' &&
'   },' &&
'  { "id" : "3", ' &&
'    "area" : "Financeiro",' &&
'    "msg_vazia" : "Nenhuma Atividade para Financeiro",' &&
'    "qtd_tarefas" : "' && E_QTD_PAG_INVOICES && '",' &&
'    "processos" : [' &&
'         { "nome" : "Pagamento(s) de Invoice",' &&
'           "qtd"  : "' && E_QTD_PAG_INVOICES && '" }' &&
'      ]' &&
'  },' &&
'  { "id" : "4", ' &&
'    "area" : "Insumos",' &&
'    "msg_vazia" : "Nenhuma Atividade para Insumos",' &&
'    "qtd_tarefas" : "' && E_QTD_LIB_EMB_INSUMOS && '",' &&
'    "processos" : [' &&
'         { "nome" : "Lib.Embarque(s) de Insumo",' &&
'           "qtd"  : "' && E_QTD_LIB_EMB_INSUMOS && '" }' &&
'      ]' &&
'  },' &&
'  { "id" : "5", ' &&
'    "area" : "Venda Mercado Interno",' &&
'    "msg_vazia" : "Nenhuma Atividade para Venda Mercado Interno",' &&
'    "qtd_tarefas" : "' && E_QTD_SD_MERC_INTERNO && '",' &&
'    "processos" : [' &&
'         { "nome" : "Faturamento(s) sem Limite de Crédito",' &&
'           "qtd"  : "' && E_QTD_FAT_LIM_CREDITO && '" },' &&
'         { "nome" : "Solicitação(ões) de Ordem de Venda",' &&
'           "qtd"  : "' && E_QTD_SOL_ORD_VENDA && '" }' &&
'      ]' &&
'  },' &&
'  { "id" : "6", ' &&
'    "area" : "Recursos Humanos",' &&
'    "msg_vazia" : "Nenhuma Atividade para Recursos Humanos",' &&
'    "qtd_tarefas" : "' && E_QTD_RH_MERC_INTERNO && '",' &&
'    "processos" : [' &&
'         { "nome" : "Pagamento(s) de Salário",' &&
'           "qtd"  : "' && E_QTD_RH_PAG_SALARIO && '" },' &&
'         { "nome" : "Movimentação(ões) de Pessoal",' &&
'           "qtd"  : "' && E_QTD_RH_MOV_PESSOAL && '" }' &&
'      ]' &&
'  },' &&
'  { "id" : "7", ' &&
'    "area" : "Manutenção",' &&
'    "msg_vazia" : "Nenhuma Atividade para Manutenção",' &&
'    "qtd_tarefas" : "' && E_QTD_PM && '",' &&
'    "processos" : [' &&
'         { "nome" : "Ordem(ns) de Orçamento",' &&
'           "qtd"  : "' && E_QTD_PM_ORC_ORDEM_I && '" },' &&
'         { "nome" : "Suplementação(ões) de Ordem de Orçamento",' &&
'           "qtd"  : "' && E_QTD_PM_ORC_SUPLE && '" }' &&
'      ]' &&
'  },' &&
'  { "id" : "8", ' &&
'    "area" : "Orçamento",' &&
'    "msg_vazia" : "Nenhuma Atividade para Orçamento",' &&
'    "qtd_tarefas" : "' && E_QTD_IM_INVEST && '",' &&
'    "processos" : [' &&
'         { "nome" : "Investimento(s)",' &&
'           "qtd"  : "' && E_QTD_IM_INVEST && '" }' &&
'      ]' &&
'  }' &&
']}'.

*    E_JSON = '{' &&
*                 '"e_desc_cab_compras" : '         && '"' && 'Compras' && '",' &&
*                 '"e_desc_cab_csc_financeiro" : '  && '"' && 'CSC – Financeiro' && '",' &&
*                 '"e_desc_cab_financeiro" : '      && '"' && 'Financeiro' && '",' &&
*                 '"e_desc_cab_insumo" : '          && '"' && 'Insumos' && '",' &&
*                 '"e_desc_cab_sd_merc_interno" : ' && '"' && 'Venda Mercado Interno' && '",' &&
*                 '"e_desc_cab_rh_folha_pag" : '    && '"' && 'RH – Folha Pagamento' && '",' &&
*                 '"e_desc_cab_pm" : '              && '"' && 'Manutenção' && '",' &&
*                 '"e_desc_cab_im" : '              && '"' && 'Orçamento' && '",' &&
*
*                 '"e_desc_pedido" : '              && '"' && 'Pedido(s)' && '",' &&
*                 '"e_desc_requisicao" : '          && '"' && 'Requisição(ões)' && '",' &&
*                 '"e_desc_reserva" : '             && '"' && 'Reserva(s)' && '",' &&
*                 '"e_desc_adiantamento" : '        && '"' && 'Adiantamento(s) a Fornecedor' && '",' &&
*                 '"e_desc_pag_impostos" : '        && '"' && 'Pagamento(s) de Impostos' && '",' &&
*                 '"e_desc_zgl_contabil" : '        && '"' && 'ZGL - Lançamento(s) Manual(is)' && '",' &&
*                 '"e_desc_pag_invoices" : '        && '"' && 'Pagamento(s) de Invoice' && '",' &&
*                 '"e_desc_lib_enb_insumos" : '     && '"' && 'Lib.Embarque(s) de Insumo' && '",' &&
*                 '"e_desc_fat_lim_credito" : '     && '"' && 'Faturamento(s) sem Limite de Crédito' && '",' &&
*                 '"e_desc_sol_ord_venda" : '       && '"' && 'Solicitação(ões) de Ordem de Venda' && '",' &&
*                 '"e_desc_rh_pag_salario" : '      && '"' && 'Pagamento(s) de Salário' && '",' &&
*                 '"e_desc_rh_mov_pessoal" : '      && '"' && 'Movimentação(ões) de Pessoal' && '",' &&
*                 '"e_desc_pm_orc_ordem" : '        && '"' && 'Ordem(ns) de Orçamento' && '",' &&
*                 '"e_desc_im_investimento" : '     && '"' && 'Investimento(s)' && '",' &&
*
*                 '"e_qtd_pedido" : '           && '"' && E_QTD_PEDIDOS    && '",' &&
*                 '"e_qtd_requisicao" : '       && '"' && E_QTD_REQUISICAO && '",' &&
*                 '"e_qtd_reserva" : '          && '"' && E_QTD_RESERVA    && '",' &&
*                 '"e_qtd_adiantamento" : '     && '"' && E_QTD_ADIANTAMENTOS && '", ' &&
*                 '"e_qtd_pag_impostos" : '     && '"' && E_QTD_APG_IMPOSTOS && '", ' &&
*                 '"e_qtd_zgl_contabil" : '     && '"' && E_QTD_ZGL_CONTABIL && '", ' &&
*                 '"e_qtd_pag_invoices" : '     && '"' && E_QTD_PAG_INVOICES && '", ' &&
*                 '"e_qtd_lib_enb_insumos" : '  && '"' && E_QTD_LIB_EMB_INSUMOS && '", ' &&
*                 '"e_qtd_fat_lim_credito" : '  && '"' && E_QTD_FAT_LIM_CREDITO && '", ' &&
*                 '"e_qtd_sol_ord_venda" : '    && '"' && E_QTD_SOL_ORD_VENDA && '", ' &&
*                 '"e_qtd_rh_pag_salario" : '   && '"' && E_QTD_RH_PAG_SALARIO && '", ' &&
*                 '"e_qtd_rh_mov_pessoal" : '   && '"' && E_QTD_RH_MOV_PESSOAL && '", ' &&
*                 '"e_qtd_pm_orc_ordem" : '     && '"' && E_QTD_PM_ORC_ORDEM_I && '", ' &&
*                 '"e_qtd_im_investimento" : '  && '"' && E_QTD_IM_INVEST && '" ' &&
*             '}'.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_RH_MOV_PESSOAL.
  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_RH_PAG_SALARIO.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST14'
      IMPORTING
        OUT_JSON = E_JSON.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_SOL_ORD_VENDA.

    CALL FUNCTION 'ZSVPL_FCN_GETWORKLIST12'
      IMPORTING
        OUT_JSON = E_JSON.

  ENDMETHOD.


  method ZIF_FMCALL_APP_MOBILE~GET_SOL_ORD_VENDA_ITENS.

    data: V_LOTE       type ZSDED013,
          IT_LOTES     type table of ZSDS018,
          IT_ESTRA     type table of ZSDS019,
          IT_DOCS      type table of ZSDS020,
          WA_ITENS     type ZDE_SOL_ORD_VENDA_APROVACAO,
          IT_ITENS     type table of ZDE_SOL_ORD_VENDA_APROVACAO,
          LC_DATA      type C length 10,
          LC_VALOR     type C length 20,
          LC_VALOR_DEC type ZDE_DECIMAL_20_2,
          E_JSON_SV    type STRING.

    types begin of TY_SOLICITA.
    types: LOTE type C length 20.
    types end of TY_SOLICITA.

    data WA_SOLICITA type TY_SOLICITA.

    call method /UI2/CL_JSON=>DESERIALIZE
      exporting
        JSON = I_JSON
      changing
        DATA = WA_SOLICITA.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        INPUT  = WA_SOLICITA-LOTE
      importing
        OUTPUT = V_LOTE.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = V_LOTE
      importing
        OUTPUT = V_LOTE.

    call function 'Z_SOL_OV_ESTRAT_LISTA'
      exporting
        V_USUARIO      = SY-UNAME
        "V_NRO_SOL_OV   = V_LOTE
      tables
        T_SOLICITACOES = IT_LOTES
        T_ESTRA        = IT_ESTRA
        T_DOCS         = IT_DOCS.

    delete IT_LOTES where NRO_SOL_OV ne V_LOTE.
    delete IT_ESTRA where NRO_SOL_OV ne V_LOTE.
    delete IT_DOCS  where NRO_SOL_OV ne V_LOTE.

    loop at IT_DOCS into data(_ITEM).

      clear: WA_ITENS.
      "WRITE PO_HEADER-DOC_DATE TO LC_DATA DD/MM/YYYY.
      "WA_PEDIDOS_ITENS-DT_PEDIDO = LC_DATA.

      WA_ITENS-MATERIAL_TXT  = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( exporting I_TEXTO = conv #( _ITEM-TXT_MATERIAL ) ).
      WA_ITENS-QUANTIDADE    = _ITEM-QTD_PREVISTA.
      WA_ITENS-VALOR_UN      = _ITEM-PRECO.
      WA_ITENS-VALOR         = _ITEM-VALOR_TOTAL.

      write _ITEM-QTD_PREVISTA to LC_VALOR.
      WA_ITENS-QUANTIDADE_TXT = LC_VALOR.
      condense WA_ITENS-QUANTIDADE_TXT no-gaps.

*---> 07/06/2023 - Migração S4 - JS
*            LC_VALOR_DEC = _ITEM-PRECO.
      LC_VALOR_DEC = conv #( _ITEM-PRECO ).
*<--- 07/06/2023 - Migração S4 - JS

      write LC_VALOR_DEC to LC_VALOR.
      WA_ITENS-VALOR_UN_TXT = LC_VALOR.
      condense WA_ITENS-VALOR_UN_TXT no-gaps.
      concatenate WA_ITENS-VALOR_UN_TXT _ITEM-UM_PRECO into WA_ITENS-VALOR_UN_TXT separated by SPACE.

*---> 07/06/2023 - Migração S4 - JS
*            LC_VALOR_DEC = _ITEM-VALOR_TOTAL.
      LC_VALOR_DEC = conv #( _ITEM-VALOR_TOTAL ).
*<--- 07/06/2023 - Migração S4 - JS

      write LC_VALOR_DEC to LC_VALOR.
      WA_ITENS-VALOR_TXT = LC_VALOR.
      condense WA_ITENS-VALOR_TXT no-gaps.

      WA_ITENS-UNIDADE = _ITEM-UM.

      select single NAME1
        into @data(_BRANCH_NAME)
        from T001W
       where WERKS eq @_ITEM-CENTRO
         and SPRAS eq @SY-LANGU.

      if SY-SUBRC is initial.
        WA_ITENS-BRANCH_ID  = _ITEM-CENTRO.
        WA_ITENS-BRANCH_TXT = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( exporting I_TEXTO = conv #( _BRANCH_NAME ) ).
      endif.

      append WA_ITENS to IT_ITENS.
    endloop.

    E_JSON = '{ "itens" : [ '.
    clear: E_JSON_SV.

    loop at IT_ITENS into WA_ITENS.
      if E_JSON_SV is not initial.
        E_JSON_SV = E_JSON_SV && ','.
      endif.
      E_JSON_SV = E_JSON_SV &&
                     '{' &&
                     '"e_nro_sol_ov" : ' && '"' && WA_ITENS-NRO_SOL_OV && '",' &&
                     '"e_material_txt" : ' && '"' && ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( I_TEXTO = conv #( WA_ITENS-MATERIAL_TXT ) ) && '",' &&
                     '"e_branch_id" : ' && '"' && WA_ITENS-BRANCH_ID && '",' &&
                     '"e_branch_txt" : ' && '"' && ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( I_TEXTO = conv #( WA_ITENS-BRANCH_TXT ) ) && '",' &&
                     '"e_unidade" : ' && '"' && WA_ITENS-UNIDADE && '",' &&
                     '"e_quantidade" : ' && '"' && WA_ITENS-QUANTIDADE && '",' &&
                     '"e_valor_un" : ' && '"' && WA_ITENS-VALOR_UN && '",' &&
                     '"e_valor" : ' && '"' && WA_ITENS-VALOR && '",' &&
                     '"e_quantidade_txt" : ' && '"' && WA_ITENS-QUANTIDADE_TXT && '",' &&
                     '"e_valor_un_txt" : ' && '"' && WA_ITENS-VALOR_UN_TXT && '",' &&
                     '"e_valor_txt" : ' && '"' && WA_ITENS-VALOR_TXT && '" ' &&
                   '}'.
    endloop.
    E_JSON = E_JSON && E_JSON_SV && ']}'.

  endmethod.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_FAT_LIM_CREDITO_APROVACAO.

    DATA: V_LOTE   TYPE ZSDT0151-LOTE,
          IT_LOTES TYPE TABLE OF ZSD_ROMA_IMP,
          IT_ESTRA TYPE TABLE OF ZSD_ESTRATEGIA_SD.

    DATA: LC_MSG TYPE  CHAR50,
          LC_OK  TYPE  CHAR01.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 20,
           ACAO TYPE C LENGTH 1.
    TYPES END OF TY_SOLICITA.

    DATA WA_SOLICITA TYPE TY_SOLICITA.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = V_LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'Z_SD_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
        V_LOTE    = V_LOTE
      TABLES
        T_ORDENS  = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    DELETE IT_LOTES WHERE LOTE  NE V_LOTE.
    DELETE IT_ESTRA WHERE LOTE  NE V_LOTE.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.
        LOOP AT IT_ESTRA ASSIGNING FIELD-SYMBOL(<FS_ESTRA>).
          <FS_ESTRA>-OPCOES = '@8Y@'.
        ENDLOOP.
    ENDCASE.

    CALL FUNCTION 'Z_SD_ESTRATEGIA_EXECUTAR'
      EXPORTING
        V_USUARIO = SY-UNAME
      IMPORTING
        MSG       = LC_MSG
        OK        = LC_OK
      TABLES
        T_ORDENS  = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    CASE LC_OK.
      WHEN ABAP_FALSE.
        ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( LC_MSG ) ).
      WHEN ABAP_TRUE.
        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        COMMIT WORK.
    ENDCASE.


  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_IM_INVESTIMENTO_APROVACAO.

    DATA: V_KOSTL  TYPE KOSTL,
          V_LOTE   TYPE NUMC10,
          IT_LOTES TYPE TABLE OF ZFI_GRU_INV,
          IT_ESTRA TYPE TABLE OF ZFI_ESTRATEGIA_ZIM.

    DATA: LC_MSG TYPE  CHAR50,
          LC_OK  TYPE  CHAR01.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 21,
           ACAO TYPE C LENGTH 1.
    TYPES END OF TY_SOLICITA.

    DATA WA_SOLICITA TYPE TY_SOLICITA.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE(10)
      IMPORTING
        OUTPUT = V_KOSTL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE+11(10)
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = V_KOSTL
      IMPORTING
        OUTPUT = V_KOSTL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = V_LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'Z_IM_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
        V_KOSTL   = V_KOSTL
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    DELETE IT_LOTES WHERE KOSTL NE V_KOSTL.
    DELETE IT_ESTRA WHERE KOSTL NE V_KOSTL.

    DELETE IT_LOTES WHERE LOTE NE V_LOTE.
    DELETE IT_ESTRA WHERE LOTE NE V_LOTE.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.
        LOOP AT IT_ESTRA ASSIGNING FIELD-SYMBOL(<FS_ESTRA>).
          <FS_ESTRA>-OPCOES = '@8Y@'.
        ENDLOOP.
    ENDCASE.

    CALL FUNCTION 'Z_IM_ESTRATEGIA_EXECUTAR'
      EXPORTING
        V_USUARIO = SY-UNAME
      IMPORTING
        MSG       = LC_MSG
        OK        = LC_OK
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    CASE LC_OK.
      WHEN ABAP_FALSE.
        ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( LC_MSG ) ).
      WHEN ABAP_TRUE.
        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        COMMIT WORK.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_LAC_MANUAIS_APROVACAO.

    DATA: V_LOTE   TYPE ZGLT034-LOTE,
          IT_LOTES TYPE TABLE OF ZFI_LOTES_IMP,
          IT_ESTRA TYPE TABLE OF ZFI_ESTRATEGIA_ZGL.

    DATA: LC_MSG TYPE  CHAR50,
          LC_OK  TYPE  CHAR01.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 20,
           ACAO TYPE C LENGTH 1.
    TYPES END OF TY_SOLICITA.

    DATA WA_SOLICITA TYPE TY_SOLICITA.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = V_LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'Z_GL_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
        V_LOTE    = V_LOTE
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    DELETE IT_LOTES WHERE LOTE  NE V_LOTE.
    DELETE IT_ESTRA WHERE LOTE  NE V_LOTE.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.
        LOOP AT IT_ESTRA ASSIGNING FIELD-SYMBOL(<FS_ESTRA>).
          <FS_ESTRA>-OPCOES = '@8Y@'.
        ENDLOOP.
    ENDCASE.

    CALL FUNCTION 'Z_GL_ESTRATEGIA_EXECUTAR'
      EXPORTING
        V_USUARIO = SY-UNAME
      IMPORTING
        MSG       = LC_MSG
        OK        = LC_OK
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    CASE LC_OK.
      WHEN ABAP_FALSE.
        ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( LC_MSG ) ).
      WHEN ABAP_TRUE.
        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        COMMIT WORK.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_LIB_EMB_INSUMOS_APROVACAO.

    DATA: V_LOTE   TYPE VBELN,
          IT_LOTES TYPE TABLE OF ZSD_ORD_VENDAS_EST,
          IT_ESTRA TYPE TABLE OF ZSD_ESTRATEGIA_OV.

    DATA: LC_MSG TYPE  CHAR50,
          LC_OK  TYPE  CHAR01.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 20,
           ACAO TYPE C LENGTH 1.
    TYPES END OF TY_SOLICITA.

    DATA WA_SOLICITA TYPE TY_SOLICITA.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = V_LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'Z_OV_ESTRATEGIA_LISTA'
      EXPORTING
        I_USUARIO = SY-UNAME
        I_VBELN   = V_LOTE
      TABLES
        T_ORDENS  = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    DELETE IT_LOTES WHERE VBELN NE V_LOTE.
    DELETE IT_ESTRA WHERE VBELN NE V_LOTE.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.
        LOOP AT IT_ESTRA ASSIGNING FIELD-SYMBOL(<FS_ESTRA>).
          <FS_ESTRA>-OPCOES = '@8Y@'.
        ENDLOOP.
    ENDCASE.

    CALL FUNCTION 'Z_OV_ESTRATEGIA_EXECUTAR'
      EXPORTING
        I_USUARIO = SY-UNAME
      IMPORTING
        E_MSG     = LC_MSG
        E_OK      = LC_OK
      TABLES
        T_ORDENS  = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    DESCRIBE TABLE IT_LOTES LINES DATA(QTD_LOTES).

    CASE LC_OK.
      WHEN ABAP_FALSE.
        ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( LC_MSG && V_LOTE && QTD_LOTES  ) ).
      WHEN ABAP_TRUE.
        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        COMMIT WORK.
    ENDCASE.


  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_MM_ADIANTAMENTO_APROVACAO.

    DATA: LC_NRO_SOL TYPE ZFIT0045-NRO_SOL.
    DATA: LC_MSG TYPE  CHAR50,
          LC_OK  TYPE  CHAR01.

    DATA: TI_LOTES TYPE TABLE OF ZAD_LOTES_IMP.
    DATA: TI_ESTRA TYPE TABLE OF ZFI_ESTRATEGIA_IMP.
    DATA: TI_DOCS  TYPE TABLE OF ZAD_DOCS_IMP.

    DATA: I_PROVACAO_ADM TYPE ZDE_ADIANT_FORN_APROVAR.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = I_PROVACAO_ADM.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_PROVACAO_ADM-LOTE
      IMPORTING
        OUTPUT = LC_NRO_SOL.

    CALL FUNCTION 'Z_AD_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
        V_NRO_SOL = LC_NRO_SOL
      TABLES
        T_LOTES   = TI_LOTES
        T_ESTRA   = TI_ESTRA
        T_DOCS    = TI_DOCS.

    DELETE TI_LOTES WHERE NRO_SOL NE LC_NRO_SOL.
    DELETE TI_ESTRA WHERE LOTE    NE LC_NRO_SOL.
    DELETE TI_DOCS  WHERE NRO_SOL NE LC_NRO_SOL.

    IF TI_LOTES IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_ADIANT_FORN_NOT_FOUND-MSGID
                            MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_ADIANT_FORN_NOT_FOUND-MSGNO
                            ATTR1 = CONV #( I_PROVACAO_ADM-LOTE ) )
          MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_ADIANT_FORN_NOT_FOUND-MSGID
          MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_ADIANT_FORN_NOT_FOUND-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_PROVACAO_ADM-LOTE ).
    ENDIF.

    IF I_PROVACAO_ADM-ACAO EQ ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.
      LOOP AT TI_ESTRA ASSIGNING FIELD-SYMBOL(<FS_ESTRA>).
        <FS_ESTRA>-OPCOES = '@8Y@'.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'Z_AD_ESTRATEGIA_EXECUTAR'
      EXPORTING
        V_USUARIO = SY-UNAME
      IMPORTING
        MSG       = LC_MSG
        OK        = LC_OK
      TABLES
        T_LOTES   = TI_LOTES
        T_ESTRA   = TI_ESTRA.

    CASE LC_OK.
      WHEN ABAP_FALSE.
        ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( LC_MSG ) ).
      WHEN ABAP_TRUE.
        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        COMMIT WORK.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_MM_PEDIDO_APROVACAO.

    DATA: DETAILS        TYPE TABLE OF BAPIRETURN,
          PO             TYPE REF TO CL_PO_HEADER_HANDLE_MM,
          LC_IM_DOCUMENT TYPE MEPO_DOCUMENT.

    DATA: I_PROVACAO TYPE ZDE_PEDIDOS_APROVAR.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = I_PROVACAO.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_PROVACAO-EBELN
      IMPORTING
        OUTPUT = I_PROVACAO-EBELN.

    CASE I_PROVACAO-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.

        "//Get release code
        SELECT SINGLE FRGCO
          INTO @DATA(LC_FRGCO)
          FROM T16FW
         WHERE OBJID = @SY-UNAME
           AND FRGGR = '01'. "01 = Estratégia de Pedido de Compra

        IF SY-SUBRC IS NOT INITIAL.
          RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_FUNCAO_PEDIDO-MSGID
                                MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_FUNCAO_PEDIDO-MSGNO
                                ATTR1 = CONV #( SY-UNAME ) )
              MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_FUNCAO_PEDIDO-MSGID
              MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_FUNCAO_PEDIDO-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( SY-UNAME ).
        ENDIF.

        "//Get purchase order details
        CALL FUNCTION 'BAPI_PO_RELEASE'
          EXPORTING
            PURCHASEORDER          = I_PROVACAO-EBELN
            PO_REL_CODE            = LC_FRGCO
          TABLES
            RETURN                 = DETAILS
          EXCEPTIONS
            AUTHORITY_CHECK_FAIL   = 1
            DOCUMENT_NOT_FOUND     = 2
            ENQUEUE_FAIL           = 3
            PREREQUISITE_FAIL      = 4
            RELEASE_ALREADY_POSTED = 5
            RESPONSIBILITY_FAIL    = 6
            OTHERS                 = 7.

        IF SY-SUBRC IS NOT INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
        ELSE.
          READ TABLE DETAILS WITH KEY TYPE = 'E' INTO DATA(WA_DETAIL).
          IF SY-SUBRC IS INITIAL.
            ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( WA_DETAIL-MESSAGE ) ).
          ENDIF.
        ENDIF.

        E_JSON = '{' && '"Sucesso" : "1"' && '}'.

      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.

        LC_IM_DOCUMENT-DOC_TYPE = 'F'.
        LC_IM_DOCUMENT-PROCESS  = 'PO_PROCESS'.
        LC_IM_DOCUMENT-TRTYP    = 'V'.
        LC_IM_DOCUMENT-DOC_KEY  = I_PROVACAO-EBELN.
        LC_IM_DOCUMENT-INITIATOR-INITIATOR = 'RELEASE'.

        CREATE OBJECT PO.
        PO->FOR_BAPI = 'X'.
        PO->PO_INITIALIZE( IM_DOCUMENT = LC_IM_DOCUMENT ).
        PO->SET_PO_NUMBER( IM_PO_NUMBER = I_PROVACAO-EBELN ).
        PO->SET_STATE( CL_PO_HEADER_HANDLE_MM=>C_AVAILABLE ).

        "//Read PO
        PO->PO_READ(
          EXPORTING
            IM_TCODE     = 'ME29N'
            IM_TRTYP     = LC_IM_DOCUMENT-TRTYP
            IM_AKTYP     = LC_IM_DOCUMENT-TRTYP
            IM_PO_NUMBER = I_PROVACAO-EBELN
            IM_DOCUMENT  = LC_IM_DOCUMENT ).

        IF ( PO->IF_RELEASABLE_MM~IS_REJECTION_ALLOWED( ) = ABAP_TRUE ).

          PO->IF_RELEASABLE_MM~REJECT(
            EXPORTING
              IM_RESET = SPACE
            EXCEPTIONS
              FAILED   = 1
              OTHERS   = 2 ).

          IF SY-SUBRC IS NOT INITIAL.
            CLEAR: PO.
            ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL_SYS( ).
          ENDIF.

          CALL METHOD PO->PO_POST
            EXCEPTIONS
              FAILURE = 1
              OTHERS  = 2.

          IF SY-SUBRC IS NOT INITIAL.
            CLEAR: PO.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
          ENDIF.

          CLEAR: PO.

        ELSE.
          CLEAR: PO.
          RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REJECT_PEDIDO-MSGID
                                MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REJECT_PEDIDO-MSGNO )
              MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REJECT_PEDIDO-MSGID
              MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REJECT_PEDIDO-MSGNO
              MSGTY  = 'E'.
        ENDIF.

        E_JSON = '{' && '"Sucesso" : "1"' && '}'.

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_MM_REQUISICAO_APROVACAO.

    DATA: LC_ITEM      TYPE BAPI2009OB-PREQ_ITEM,
          LC_XEBAN     TYPE TABLE OF UEBAN,
          LC_XEBKN     TYPE TABLE OF UEBKN,
          LC_YEBAN     TYPE TABLE OF UEBAN,
          LC_YEBKN     TYPE TABLE OF UEBKN,
          IT_CONTAINER TYPE TABLE OF SWR_CONT,
          WA_CONTAINER TYPE SWR_CONT.

    DATA: I_PROVACAO_REQ TYPE ZDE_REQUISICOES_APROVAR.
    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = I_PROVACAO_REQ.

    SELECT SINGLE FRGCO INTO @DATA(RELEASE_CODE)
      FROM T16FW
     WHERE OBJID = @SY-UNAME
       AND FRGGR = '02'.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_FUNCAO_REQUISICAO-MSGID
                            MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_FUNCAO_REQUISICAO-MSGNO
                            ATTR1 = CONV #( SY-UNAME ) )
          MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_FUNCAO_REQUISICAO-MSGID
          MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_FUNCAO_REQUISICAO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( SY-UNAME ).
    ENDIF.

    CASE I_PROVACAO_REQ-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.

        IF I_PROVACAO_REQ-BNFPO IS NOT INITIAL.

          LC_ITEM = I_PROVACAO_REQ-BNFPO.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = LC_ITEM
            IMPORTING
              OUTPUT = LC_ITEM.

          CALL FUNCTION 'BAPI_REQUISITION_RELEASE'
            EXPORTING
              NUMBER                 = I_PROVACAO_REQ-BANFN
              REL_CODE               = RELEASE_CODE
              ITEM                   = LC_ITEM
            EXCEPTIONS
              AUTHORITY_CHECK_FAIL   = 1
              REQUISITION_NOT_FOUND  = 2
              ENQUEUE_FAIL           = 3
              PREREQUISITE_FAIL      = 4
              RELEASE_ALREADY_POSTED = 5
              RESPONSIBILITY_FAIL    = 6
              OTHERS                 = 7.

          IF SY-SUBRC IS NOT INITIAL.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
          ENDIF.

        ELSE.
          ME->ZIF_FMCALL_APP_MOBILE~GET_MM_REQUISICAO_ITENS(
            EXPORTING
              I_BANFN = I_PROVACAO_REQ-BANFN
            IMPORTING
             E_REQUISICAO_ITENS = DATA(E_REQUISICAO_ITENS) ).

          LOOP AT E_REQUISICAO_ITENS INTO DATA(WA_REQUISICAO_ITENS).

            LC_ITEM = WA_REQUISICAO_ITENS-BNFPO.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = LC_ITEM
              IMPORTING
                OUTPUT = LC_ITEM.

            CALL FUNCTION 'BAPI_REQUISITION_RELEASE'
              EXPORTING
                NUMBER                 = WA_REQUISICAO_ITENS-BANFN
                REL_CODE               = RELEASE_CODE
                ITEM                   = LC_ITEM
              EXCEPTIONS
                AUTHORITY_CHECK_FAIL   = 1
                REQUISITION_NOT_FOUND  = 2
                ENQUEUE_FAIL           = 3
                PREREQUISITE_FAIL      = 4
                RELEASE_ALREADY_POSTED = 5
                RESPONSIBILITY_FAIL    = 6
                OTHERS                 = 7.

            IF SY-SUBRC IS NOT INITIAL.
              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
              ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
            ENDIF.
          ENDLOOP.
        ENDIF.

        E_JSON = '{' && '"Sucesso" : "1"' && '}'.

      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.
        TRY .

            IF I_PROVACAO_REQ-BNFPO IS NOT INITIAL.

              LC_ITEM = I_PROVACAO_REQ-BNFPO.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  INPUT  = LC_ITEM
                IMPORTING
                  OUTPUT = LC_ITEM.

              SELECT *
                INTO CORRESPONDING FIELDS OF TABLE LC_XEBAN
                FROM EBAN
               WHERE BANFN = I_PROVACAO_REQ-BANFN
                 AND BNFPO = LC_ITEM.

              IF SY-SUBRC IS NOT INITIAL.
                RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
                  EXPORTING
                    TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REQUISICAO_NOT_FOUND-MSGID
                                      MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REQUISICAO_NOT_FOUND-MSGNO
                                      ATTR1 = CONV #( I_PROVACAO_REQ-BANFN )
                                      ATTR2 = CONV #( LC_ITEM ) )
                    MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REQUISICAO_NOT_FOUND-MSGID
                    MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REQUISICAO_NOT_FOUND-MSGNO
                    MSGTY  = 'E'
                    MSGV1  = CONV #( I_PROVACAO_REQ-BANFN )
                    MSGV2  = CONV #( LC_ITEM ).
              ENDIF.

              SELECT *
                INTO CORRESPONDING FIELDS OF TABLE LC_XEBKN
                FROM EBKN
               WHERE BANFN = I_PROVACAO_REQ-BANFN
                 AND BNFPO = LC_ITEM.

              LC_YEBAN[] = LC_XEBAN[].
              LC_YEBKN[] = LC_XEBKN[].

              LC_XEBAN[ 1 ]-KZ    = 'U'.
              LC_XEBAN[ 1 ]-BANPR = '08'.

              CALL FUNCTION 'ME_UPDATE_REQUISITION'
                TABLES
                  XEBAN = LC_XEBAN
                  XEBKN = LC_XEBKN
                  YEBAN = LC_YEBAN
                  YEBKN = LC_YEBKN.

              CALL FUNCTION 'ME_UPDATE_REQUISITION_CHNGDOC'
                TABLES
                  XEBAN = LC_XEBAN
                  YEBAN = LC_YEBAN
                  XEBKN = LC_XEBKN
                  YEBKN = LC_YEBKN.

              WA_CONTAINER-ELEMENT = 'RELEASECODE'.
              WA_CONTAINER-VALUE   = RELEASE_CODE.
              APPEND WA_CONTAINER TO IT_CONTAINER.

              DATA(LC_OBJECT_KEY) = CONV SWR_STRUCT-OBJECT_KEY( I_PROVACAO_REQ-BANFN && LC_ITEM ).

              CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
                EXPORTING
                  OBJECT_TYPE     = 'BUS2009'
                  OBJECT_KEY      = LC_OBJECT_KEY
                  EVENT           = 'REJECTED'
                TABLES
                  INPUT_CONTAINER = IT_CONTAINER.

            ELSE.

              ME->ZIF_FMCALL_APP_MOBILE~GET_MM_REQUISICAO_ITENS(
                EXPORTING
                  I_BANFN = I_PROVACAO_REQ-BANFN
                IMPORTING
                 E_REQUISICAO_ITENS = E_REQUISICAO_ITENS ).

              LOOP AT E_REQUISICAO_ITENS INTO WA_REQUISICAO_ITENS.

                LC_ITEM = WA_REQUISICAO_ITENS-BNFPO.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    INPUT  = LC_ITEM
                  IMPORTING
                    OUTPUT = LC_ITEM.

                SELECT *
                  INTO CORRESPONDING FIELDS OF TABLE LC_XEBAN
                  FROM EBAN
                 WHERE BANFN = WA_REQUISICAO_ITENS-BANFN
                   AND BNFPO = LC_ITEM.

                IF SY-SUBRC IS NOT INITIAL.
                  RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
                    EXPORTING
                      TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REQUISICAO_NOT_FOUND-MSGID
                                        MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REQUISICAO_NOT_FOUND-MSGNO
                                        ATTR1 = CONV #( I_PROVACAO_REQ-BANFN )
                                        ATTR2 = CONV #( LC_ITEM ) )
                      MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REQUISICAO_NOT_FOUND-MSGID
                      MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_REQUISICAO_NOT_FOUND-MSGNO
                      MSGTY  = 'E'
                      MSGV1  = CONV #( I_PROVACAO_REQ-BANFN )
                      MSGV2  = CONV #( LC_ITEM ).
                ENDIF.

                SELECT *
                  INTO CORRESPONDING FIELDS OF TABLE LC_XEBKN
                  FROM EBKN
                 WHERE BANFN = WA_REQUISICAO_ITENS-BANFN
                   AND BNFPO = LC_ITEM.

                LC_YEBAN[] = LC_XEBAN[].
                LC_YEBKN[] = LC_XEBKN[].

                LC_XEBAN[ 1 ]-KZ    = 'U'.
                LC_XEBAN[ 1 ]-BANPR = '08'.

                CALL FUNCTION 'ME_UPDATE_REQUISITION'
                  TABLES
                    XEBAN = LC_XEBAN
                    XEBKN = LC_XEBKN
                    YEBAN = LC_YEBAN
                    YEBKN = LC_YEBKN.

                CALL FUNCTION 'ME_UPDATE_REQUISITION_CHNGDOC'
                  TABLES
                    XEBAN = LC_XEBAN
                    YEBAN = LC_YEBAN
                    XEBKN = LC_XEBKN
                    YEBKN = LC_YEBKN.

                WA_CONTAINER-ELEMENT = 'RELEASECODE'.
                WA_CONTAINER-VALUE   = RELEASE_CODE.
                APPEND WA_CONTAINER TO IT_CONTAINER.

                LC_OBJECT_KEY = WA_REQUISICAO_ITENS-BANFN && LC_ITEM.

                CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
                  EXPORTING
                    OBJECT_TYPE     = 'BUS2009'
                    OBJECT_KEY      = LC_OBJECT_KEY
                    EVENT           = 'REJECTED'
                  TABLES
                    INPUT_CONTAINER = IT_CONTAINER.

              ENDLOOP.

            ENDIF.
            E_JSON = '{' && '"Sucesso" : "1"' && '}'.
          CATCH CX_ROOT INTO DATA(EX_ROOT).
            ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = EX_ROOT->GET_LONGTEXT( ) ).
        ENDTRY.

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_MM_RESERVA_APROVACAO.

    DATA: ITEMS          TYPE TABLE OF BAPI2093_RES_ITEM_CHANGE,
          RETURNS        TYPE TABLE OF BAPIRET2,
          ITEMSX         TYPE TABLE OF BAPI2093_RES_ITEM_CHANGEX,
          LC_RESERVATION TYPE BAPI2093_RES_KEY-RESERV_NO,
          LC_RES_ITEM	   TYPE RSPOS.

    DATA: I_PROVACAO_RES TYPE ZDE_RESERVAS_APROVAR.
    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = I_PROVACAO_RES.

    CASE I_PROVACAO_RES-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.

        IF I_PROVACAO_RES-RSPOS IS NOT INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = I_PROVACAO_RES-RSPOS
            IMPORTING
              OUTPUT = LC_RES_ITEM.

          SELECT SINGLE RSNUM, RSPOS, XLOEK, XWAOK
            INTO @DATA(WA_RESERVATION_AUX)
            FROM RESB
           WHERE RSNUM EQ @I_PROVACAO_RES-RSNUM
             AND RSPOS EQ @LC_RES_ITEM.

          CHECK NOT ( WA_RESERVATION_AUX-XLOEK = ABAP_TRUE OR WA_RESERVATION_AUX-XWAOK = ABAP_TRUE ).

          ITEMS  = VALUE #( ( RES_ITEM = LC_RES_ITEM MOVEMENT = ABAP_TRUE ) ).
          ITEMSX = VALUE #( ( RES_ITEM = LC_RES_ITEM MOVEMENT = ABAP_TRUE ) ).

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = I_PROVACAO_RES-RSNUM
            IMPORTING
              OUTPUT = LC_RESERVATION.

*---> 01/07/2023 - Migração S4 - EJ
          CALL FUNCTION 'BAPI_RESERVATION_CHANGE'         "#EC CI_USAGE_OK[2438131]
*<--- 01/07/2023 - Migração S4 - EJ
            EXPORTING
              RESERVATION               = LC_RESERVATION
            TABLES
              RESERVATIONITEMS_CHANGED  = ITEMS
              RESERVATIONITEMS_CHANGEDX = ITEMSX
              RETURN                    = RETURNS.

          DELETE RETURNS WHERE TYPE NE 'E'.

          IF RETURNS IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          ELSE.
            READ TABLE RETURNS INTO DATA(WA_RETURNS) INDEX 1.
            ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = CONV #( WA_RETURNS-MESSAGE ) ).
          ENDIF.

        ELSE.

          SELECT RSNUM, RSPOS, XLOEK, XWAOK
            INTO TABLE @DATA(_RESERVATION_ITEM)
            FROM RESB
           WHERE RSNUM = @I_PROVACAO_RES-RSNUM.

          LOOP AT _RESERVATION_ITEM INTO DATA(WA_RESERVATION_ITEM).
            IF ( WA_RESERVATION_ITEM-XLOEK = ABAP_TRUE OR WA_RESERVATION_ITEM-XWAOK = ABAP_TRUE ).
              CONTINUE.
            ENDIF.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WA_RESERVATION_ITEM-RSPOS
              IMPORTING
                OUTPUT = LC_RES_ITEM.

            ITEMS  = VALUE #( ( RES_ITEM = LC_RES_ITEM MOVEMENT = ABAP_TRUE ) ).
            ITEMSX = VALUE #( ( RES_ITEM = LC_RES_ITEM MOVEMENT = ABAP_TRUE ) ).

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WA_RESERVATION_ITEM-RSNUM
              IMPORTING
                OUTPUT = LC_RESERVATION.

*---> 01/07/2023 - Migração S4 - EJ
            CALL FUNCTION 'BAPI_RESERVATION_CHANGE'         "#EC CI_USAGE_OK[2438131]
*<--- 01/07/2023 - Migração S4 - EJ
              EXPORTING
                RESERVATION               = LC_RESERVATION
              TABLES
                RESERVATIONITEMS_CHANGED  = ITEMS
                RESERVATIONITEMS_CHANGEDX = ITEMSX
                RETURN                    = RETURNS.

            DELETE RETURNS WHERE TYPE NE 'E'.

            IF RETURNS IS INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
            ELSE.
              READ TABLE RETURNS INTO WA_RETURNS INDEX 1.
              ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = CONV #( WA_RETURNS-MESSAGE ) ).
            ENDIF.
          ENDLOOP.
        ENDIF.

        E_JSON = '{' && '"Sucesso" : "1"' && '}'.

      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.

        IF I_PROVACAO_RES-RSPOS IS NOT INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = I_PROVACAO_RES-RSPOS
            IMPORTING
              OUTPUT = LC_RES_ITEM.

          SELECT SINGLE RSNUM, RSPOS, XLOEK, XWAOK
            INTO @WA_RESERVATION_AUX
            FROM RESB
           WHERE RSNUM EQ @I_PROVACAO_RES-RSNUM
             AND RSPOS EQ @LC_RES_ITEM.

          CHECK NOT ( WA_RESERVATION_AUX-XLOEK = ABAP_TRUE OR WA_RESERVATION_AUX-XWAOK = ABAP_TRUE ).

          ITEMS  = VALUE #( ( RES_ITEM = LC_RES_ITEM DELETE_IND = ABAP_TRUE ) ).
          ITEMSX = VALUE #( ( RES_ITEM = LC_RES_ITEM DELETE_IND = ABAP_TRUE ) ).

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = I_PROVACAO_RES-RSNUM
            IMPORTING
              OUTPUT = LC_RESERVATION.

*---> 01/07/2023 - Migração S4 - EJ
          CALL FUNCTION 'BAPI_RESERVATION_CHANGE'         "#EC CI_USAGE_OK[2438131]
*<--- 01/07/2023 - Migração S4 - EJ
            EXPORTING
              RESERVATION               = LC_RESERVATION
            TABLES
              RESERVATIONITEMS_CHANGED  = ITEMS
              RESERVATIONITEMS_CHANGEDX = ITEMSX
              RETURN                    = RETURNS.

          DELETE RETURNS WHERE TYPE NE 'E'.

          IF RETURNS IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          ELSE.
            READ TABLE RETURNS INTO WA_RETURNS INDEX 1.
            ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = CONV #( WA_RETURNS-MESSAGE ) ).
          ENDIF.

        ELSE.

          SELECT RSNUM, RSPOS, XLOEK, XWAOK
            INTO TABLE @_RESERVATION_ITEM
            FROM RESB
           WHERE RSNUM = @I_PROVACAO_RES-RSNUM.

          LOOP AT _RESERVATION_ITEM INTO WA_RESERVATION_ITEM.
            IF ( WA_RESERVATION_ITEM-XLOEK = ABAP_TRUE OR WA_RESERVATION_ITEM-XWAOK = ABAP_TRUE ).
              CONTINUE.
            ENDIF.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WA_RESERVATION_ITEM-RSPOS
              IMPORTING
                OUTPUT = LC_RES_ITEM.

            ITEMS  = VALUE #( ( RES_ITEM = LC_RES_ITEM DELETE_IND = ABAP_TRUE ) ).
            ITEMSX = VALUE #( ( RES_ITEM = LC_RES_ITEM DELETE_IND = ABAP_TRUE ) ).

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WA_RESERVATION_ITEM-RSNUM
              IMPORTING
                OUTPUT = LC_RESERVATION.

*---> 01/07/2023 - Migração S4 - EJ
            CALL FUNCTION 'BAPI_RESERVATION_CHANGE'         "#EC CI_USAGE_OK[2438131]
*<--- 01/07/2023 - Migração S4 - EJ
              EXPORTING
                RESERVATION               = LC_RESERVATION
              TABLES
                RESERVATIONITEMS_CHANGED  = ITEMS
                RESERVATIONITEMS_CHANGEDX = ITEMSX
                RETURN                    = RETURNS.

            DELETE RETURNS WHERE TYPE NE 'E'.

            IF RETURNS IS INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
            ELSE.
              READ TABLE RETURNS INTO WA_RETURNS INDEX 1.
              ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = CONV #( WA_RETURNS-MESSAGE ) ).
            ENDIF.
          ENDLOOP.
        ENDIF.

        E_JSON = '{' && '"Sucesso" : "1"' && '}'.

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_PAG_IMPOSTOS_APROVACAO.

    DATA: V_LOTE   TYPE ZIMP_CAD_LOTE-LOTE,
          IT_LOTES TYPE TABLE OF ZFI_LOTES_IMP,
          IT_ESTRA TYPE TABLE OF ZFI_ESTRATEGIA_IMP.

    DATA: LC_MSG TYPE  CHAR50,
          LC_OK  TYPE  CHAR01.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 20,
           ACAO TYPE C LENGTH 1.
    TYPES END OF TY_SOLICITA.

    DATA WA_SOLICITA TYPE TY_SOLICITA.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = V_LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'Z_FI_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
        V_LOTE    = V_LOTE
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    DELETE IT_LOTES WHERE LOTE  NE V_LOTE.
    DELETE IT_ESTRA WHERE LOTE  NE V_LOTE.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.
        LOOP AT IT_ESTRA ASSIGNING FIELD-SYMBOL(<FS_ESTRA>).
          <FS_ESTRA>-OPCOES = '@8Y@'.
        ENDLOOP.
    ENDCASE.

    CALL FUNCTION 'Z_FI_ESTRATEGIA_EXECUTAR'
      EXPORTING
        V_USUARIO = SY-UNAME
      IMPORTING
        MSG       = LC_MSG
        OK        = LC_OK
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    CASE LC_OK.
      WHEN ABAP_FALSE.
        ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( LC_MSG ) ).
      WHEN ABAP_TRUE.
        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        COMMIT WORK.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_PAG_INVOICES_APROVACAO.

    DATA: V_LOTE   TYPE ZFIT0036-LOTE,
          IT_LOTES TYPE TABLE OF ZIV_LOTES_IMP,
          IT_ESTRA TYPE TABLE OF ZFI_ESTRATEGIA_IMP.

    DATA: LC_MSG TYPE  CHAR50,
          LC_OK  TYPE  CHAR01.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 20,
           ACAO TYPE C LENGTH 1.
    TYPES END OF TY_SOLICITA.

    DATA WA_SOLICITA TYPE TY_SOLICITA.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = V_LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'Z_IV_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
        LOTE      = V_LOTE
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    DELETE IT_LOTES WHERE LOTE  NE V_LOTE.
    DELETE IT_ESTRA WHERE LOTE  NE V_LOTE.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.
        LOOP AT IT_ESTRA ASSIGNING FIELD-SYMBOL(<FS_ESTRA>).
          <FS_ESTRA>-OPCOES = '@8Y@'.
        ENDLOOP.
    ENDCASE.

    CALL FUNCTION 'Z_IV_ESTRATEGIA_EXECUTAR'
      EXPORTING
        V_USUARIO = SY-UNAME
      IMPORTING
        MSG       = LC_MSG
        OK        = LC_OK
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    CASE LC_OK.
      WHEN ABAP_FALSE.
        ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( LC_MSG ) ).
      WHEN ABAP_TRUE.
        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        COMMIT WORK.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_PM_ORC_ORDEM_APROVACAO.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 20,
           ACAO TYPE C LENGTH 1,
           MSG  TYPE STRING.
    TYPES END OF TY_SOLICITA.

    DATA: WA_SOLICITA TYPE TY_SOLICITA,
          I_AUFNR     TYPE  AUFK-AUFNR,
          LC_OK       TYPE CHAR01,
          LC_MSG      TYPE STRING.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    DATA: BEGIN OF WA_RETORNO,
            MENSAGEM       TYPE STRING,
            CODIGO_RETORNO TYPE STRING,
          END OF WA_RETORNO.

    WA_SOLICITA-MSG = ZCL_FMCALL_APP_MOBILE=>CONVERT_TXT_JSON_TO_STRING( I_TEXTO = WA_SOLICITA-MSG ).

    FIELD-SYMBOLS: <FS_IHGNS> TYPE IHGNS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE
      IMPORTING
        OUTPUT = I_AUFNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_AUFNR
      IMPORTING
        OUTPUT = I_AUFNR.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.

        CALL FUNCTION 'Z_LIBERA_PERMITS_PM'
          EXPORTING
            I_AUFNR    = I_AUFNR
            I_USER     = SY-UNAME
            I_ACAO     = '0001'
          IMPORTING
            E_CODIGO   = WA_RETORNO-CODIGO_RETORNO
            E_MENSAGEM = WA_RETORNO-MENSAGEM.

        IF WA_RETORNO-CODIGO_RETORNO(1) = 'S'.
          E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        ELSE.
          ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = WA_RETORNO-MENSAGEM ).
        ENDIF.

      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.

        CALL FUNCTION 'ZPM_REPROVAR_PERMIT'
          EXPORTING
            ORDEM       = I_AUFNR
            MOTIVO      = WA_SOLICITA-MSG
          IMPORTING
            OK          = LC_OK
            MSG_RETORNO = LC_MSG.

        IF LC_OK EQ ABAP_TRUE.
          COMMIT WORK.
          E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        ELSE.
          ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = LC_MSG ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_PM_ORC_SUPLE_ORDEM_APROV.

    DATA: IT_RETURN TYPE TABLE OF BAPIRET2,
          LC_OK     TYPE CHAR01,
          LC_MSG    TYPE STRING.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 20,
           ACAO TYPE C LENGTH 1,
           MSG  TYPE STRING.
    TYPES END OF TY_SOLICITA.

    DATA: WA_SOLICITA TYPE TY_SOLICITA,
          I_AUFNR     TYPE  AUFK-AUFNR.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    DATA: BEGIN OF WA_RETORNO,
            MENSAGEM       TYPE STRING,
            CODIGO_RETORNO TYPE STRING,
          END OF WA_RETORNO.

    WA_SOLICITA-MSG = ZCL_FMCALL_APP_MOBILE=>CONVERT_TXT_JSON_TO_STRING( I_TEXTO = WA_SOLICITA-MSG ).

    FIELD-SYMBOLS: <FS_IHGNS> TYPE IHGNS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE
      IMPORTING
        OUTPUT = I_AUFNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_AUFNR
      IMPORTING
        OUTPUT = I_AUFNR.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.

        CALL FUNCTION 'ZPM_APROVAR_SUPLEMENTACAO'
          EXPORTING
            ORDEM  = I_AUFNR
          TABLES
            ERRORS = IT_RETURN.

        IF IT_RETURN IS INITIAL.
          E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        ELSE.
          LOOP AT IT_RETURN INTO DATA(WA_RETURN).
            IF WA_RETORNO-MENSAGEM IS INITIAL.
              WA_RETORNO-MENSAGEM = WA_RETURN-MESSAGE.
            ELSE.
              CONCATENATE WA_RETORNO-MENSAGEM WA_RETURN-MESSAGE INTO WA_RETORNO-MENSAGEM SEPARATED BY '/'.
            ENDIF.
          ENDLOOP.
          WA_RETORNO-MENSAGEM = ZCL_FMCALL_APP_MOBILE=>CONVERT_TO_UTF8( I_TEXTO = WA_RETORNO-MENSAGEM ).
          ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = WA_RETORNO-MENSAGEM ).
        ENDIF.

      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.

        CALL FUNCTION 'ZPM_REPROVAR_SUPLEMENTACAO'
          EXPORTING
            ORDEM       = I_AUFNR
            MOTIVO      = WA_SOLICITA-MSG
          IMPORTING
            OK          = LC_OK
            MSG_RETORNO = LC_MSG.

        IF LC_OK EQ ABAP_TRUE.
          COMMIT WORK.
          E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        ELSE.
          ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = LC_MSG ).
        ENDIF.

    ENDCASE.


  ENDMETHOD.


  method ZIF_FMCALL_APP_MOBILE~SET_RH_MOV_PESSOAL_APROVACAO.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 10,
           ACAO TYPE C LENGTH 1.
    TYPES END OF TY_SOLICITA.

    DATA WA_SOLICITA TYPE TY_SOLICITA.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.

        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.

        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
    ENDCASE.

  endmethod.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_RH_PAG_SALARIO_APROVACAO.

    DATA: V_LOTE   TYPE ZHCMT_PY_0004-LOTE,
          IT_LOTES TYPE TABLE OF ZFI_LOTES_FOL,
          IT_ESTRA TYPE TABLE OF ZFI_ESTRATEGIA_FOL.

    DATA: LC_MSG TYPE  CHAR50,
          LC_OK  TYPE  CHAR01.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 20,
           ACAO TYPE C LENGTH 1.
    TYPES END OF TY_SOLICITA.

    DATA WA_SOLICITA TYPE TY_SOLICITA.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = V_LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'Z_FL_ESTRATEGIA_LISTA'
      EXPORTING
        V_USUARIO = SY-UNAME
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    DELETE IT_LOTES WHERE LOTE NE V_LOTE.
    DELETE IT_ESTRA WHERE LOTE NE V_LOTE.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.
        LOOP AT IT_ESTRA ASSIGNING FIELD-SYMBOL(<FS_ESTRA>).
          <FS_ESTRA>-OPCOES = '@8Y@'.
        ENDLOOP.
    ENDCASE.

    CALL FUNCTION 'Z_FL_ESTRATEGIA_EXECUTAR'
      EXPORTING
        V_USUARIO = SY-UNAME
      IMPORTING
        MSG       = LC_MSG
        OK        = LC_OK
      TABLES
        T_LOTES   = IT_LOTES
        T_ESTRA   = IT_ESTRA.

    CASE LC_OK.
      WHEN ABAP_FALSE.
        ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( LC_MSG ) ).
      WHEN ABAP_TRUE.
        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        COMMIT WORK.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~SET_SOL_ORD_VENDA_APROVACAO.

    DATA: V_LOTE    TYPE ZSDED013,
          IT_LOTES  TYPE TABLE OF ZSDS018,
          IT_ESTRA  TYPE TABLE OF ZSDS019,
          LC_MOTIVO TYPE CHAR50.

    DATA: LC_MSG TYPE  CHAR50,
          LC_OK  TYPE  CHAR01.

    TYPES BEGIN OF TY_SOLICITA.
    TYPES: LOTE TYPE C LENGTH 20,
           ACAO TYPE C LENGTH 1,
           MSG  TYPE STRING.
    TYPES END OF TY_SOLICITA.

    DATA WA_SOLICITA TYPE TY_SOLICITA.

    CALL METHOD /UI2/CL_JSON=>DESERIALIZE
      EXPORTING
        JSON = I_JSON
      CHANGING
        DATA = WA_SOLICITA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_SOLICITA-LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = V_LOTE
      IMPORTING
        OUTPUT = V_LOTE.

    CALL FUNCTION 'Z_SOL_OV_ESTRAT_LISTA'
      EXPORTING
        V_USUARIO      = SY-UNAME
"       V_NRO_SOL_OV   = V_LOTE
      TABLES
        T_SOLICITACOES = IT_LOTES
        T_ESTRA        = IT_ESTRA.

    DELETE IT_LOTES WHERE NRO_SOL_OV NE V_LOTE.
    DELETE IT_ESTRA WHERE NRO_SOL_OV NE V_LOTE.

    CASE WA_SOLICITA-ACAO.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_APROVAR.
      WHEN ZIF_FMCALL_APP_MOBILE~ST_RECUSAR.
        LOOP AT IT_ESTRA ASSIGNING FIELD-SYMBOL(<FS_ESTRA>).
          <FS_ESTRA>-OPCOES = '@8Y@'.
        ENDLOOP.
    ENDCASE.

    LC_MOTIVO = WA_SOLICITA-MSG.

    CALL FUNCTION 'Z_SOL_OV_ESTRAT_EXECUTA'
      EXPORTING
        V_USUARIO      = SY-UNAME
        I_MOTIVO       = LC_MOTIVO
      IMPORTING
        MSG            = LC_MSG
        OK             = LC_OK
      TABLES
        T_SOLICITACOES = IT_LOTES
        T_ESTRA        = IT_ESTRA.

    CASE LC_OK.
      WHEN ABAP_FALSE.
        ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = CONV #( LC_MSG ) ).
      WHEN ABAP_TRUE.
        E_JSON = '{' && '"Sucesso" : "1"' && '}'.
        COMMIT WORK.
    ENDCASE.


  ENDMETHOD.
ENDCLASS.
