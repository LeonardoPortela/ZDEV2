CLASS ZCL_FMCALL_HANDLER DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS ABAP .
    TYPE-POOLS JS .

*"* public components of class ZCL_FMCALL_HANDLER
*"* do not include other source files here!!!
    INTERFACES IF_HTTP_EXTENSION .

    CONSTANTS XNL TYPE ABAP_CHAR1 VALUE %_NEWLINE ##NO_TEXT.
    CONSTANTS XCRLF TYPE ABAP_CR_LF VALUE %_CR_LF ##NO_TEXT.
    DATA MY_SERVICE TYPE STRING .
    DATA MY_URL TYPE STRING .

    CLASS-METHODS ABAP2JSON
      IMPORTING
        !ABAP_DATA         TYPE DATA
        !NAME              TYPE STRING OPTIONAL
        !UPCASE            TYPE XFELD OPTIONAL
        !CAMELCASE         TYPE XFELD OPTIONAL
      RETURNING
        VALUE(JSON_STRING) TYPE STRING
      EXCEPTIONS
        ERROR_IN_DATA_DESCRIPTION .
    CLASS-METHODS ABAP2PDF
      IMPORTING
        !ABAP_DATA         TYPE DATA
        !NAME              TYPE STRING OPTIONAL
      RETURNING
        VALUE(PDF_XSTRING) TYPE XSTRING
      EXCEPTIONS
        ERROR_IN_DATA_DESCRIPTION .
    CLASS-METHODS ABAP2PERL
      IMPORTING
        !ABAP_DATA         TYPE DATA
        !NAME              TYPE STRING OPTIONAL
        !UPCASE            TYPE XFELD OPTIONAL
      RETURNING
        VALUE(PERL_STRING) TYPE STRING
      EXCEPTIONS
        ERROR_IN_DATA_DESCRIPTION .
    CLASS-METHODS ABAP2XML
      IMPORTING
        !ABAP_DATA        TYPE DATA
        !NAME             TYPE STRING OPTIONAL
        !WITH_XML_HEADER  TYPE ABAP_BOOL DEFAULT ABAP_FALSE
        !UPCASE           TYPE XFELD OPTIONAL
        !NAME_ATR         TYPE STRING OPTIONAL
      RETURNING
        VALUE(XML_STRING) TYPE STRING .
    CLASS-METHODS ABAP2YAML
      IMPORTING
        !ABAP_DATA         TYPE DATA
        !NAME              TYPE STRING OPTIONAL
        !UPCASE            TYPE XFELD OPTIONAL
        !Y_LEVEL           TYPE I DEFAULT 0
        !S_INDEX           TYPE I DEFAULT 0
        !FIRST_ROW         TYPE XFELD OPTIONAL
        !DONT_INDENT       TYPE XFELD OPTIONAL
      RETURNING
        VALUE(YAML_STRING) TYPE STRING
      EXCEPTIONS
        ERROR_IN_DATA_DESCRIPTION .
    CLASS-METHODS BUILD_PARAMS
      IMPORTING
        !FUNCTION_NAME TYPE RS38L_FNAM
      EXPORTING
        !PARAMTAB      TYPE ABAP_FUNC_PARMBIND_TAB
        !EXCEPTAB      TYPE ABAP_FUNC_EXCPBIND_TAB
        !PARAMS        TYPE ANY
      EXCEPTIONS
        INVALID_FUNCTION
        UNSUPPORTED_PARAM_TYPE .
    CLASS-METHODS JSON2ABAP
      IMPORTING
        !JSON_STRING          TYPE STRING OPTIONAL
        !VAR_NAME             TYPE STRING OPTIONAL
        !PROPERTY_PATH        TYPE STRING DEFAULT 'json_obj'
      EXPORTING
        VALUE(PROPERTY_TABLE) TYPE JS_PROPERTY_TAB
      CHANGING
        !JS_OBJECT            TYPE REF TO CL_JAVA_SCRIPT OPTIONAL
        VALUE(ABAP_DATA)      TYPE ANY OPTIONAL
      RAISING
        ZCX_JSON .
    CLASS-METHODS JSON_DESERIALIZE
      IMPORTING
        !JSON     TYPE STRING
      CHANGING
        !PARAMTAB TYPE ABAP_FUNC_PARMBIND_TAB
      RAISING
        ZCX_JSON .
    METHODS NOTES
      RETURNING
        VALUE(TEXT) TYPE STRING .
    CLASS-METHODS SERIALIZE_JSON
      IMPORTING
        !PARAMTAB  TYPE ABAP_FUNC_PARMBIND_TAB
        !PARAMS    TYPE ANY OPTIONAL
        !EXCEPTAB  TYPE ABAP_FUNC_EXCPBIND_TAB OPTIONAL
        !SHOW_IMPP TYPE ABAP_BOOL OPTIONAL
        !JSONP     TYPE STRING OPTIONAL
        !LOWERCASE TYPE ABAP_BOOL DEFAULT ABAP_FALSE
        !CAMELCASE TYPE ABAP_BOOL DEFAULT ABAP_FALSE
        !ACTION    TYPE STRING OPTIONAL
      EXPORTING
        !O_STRING  TYPE STRING .
    CLASS-METHODS SERIALIZE_PDF
      IMPORTING
        !PARAMTAB  TYPE ABAP_FUNC_PARMBIND_TAB
        !PARAMS    TYPE ANY OPTIONAL
        !EXCEPTAB  TYPE ABAP_FUNC_EXCPBIND_TAB OPTIONAL
        !SHOW_IMPP TYPE ABAP_BOOL OPTIONAL
        !JSONP     TYPE STRING OPTIONAL
        !LOWERCASE TYPE ABAP_BOOL DEFAULT ABAP_FALSE
        !CAMELCASE TYPE ABAP_BOOL DEFAULT ABAP_FALSE
      EXPORTING
        !O_XSTRING TYPE XSTRING
        !O_NAME    TYPE STRING .
    CLASS-METHODS SERIALIZE_PERL
      IMPORTING
        !PARAMTAB    TYPE ABAP_FUNC_PARMBIND_TAB
        !PARAMS      TYPE ANY OPTIONAL
        !EXCEPTAB    TYPE ABAP_FUNC_EXCPBIND_TAB OPTIONAL
        !SHOW_IMPP   TYPE ABAP_BOOL OPTIONAL
        !JSONP       TYPE STRING OPTIONAL
        !LOWERCASE   TYPE ABAP_BOOL DEFAULT ABAP_FALSE
        !FUNCNAME    TYPE RS38L_FNAM
      EXPORTING
        !PERL_STRING TYPE STRING .
    CLASS-METHODS SERIALIZE_XML
      IMPORTING
        !PARAMTAB  TYPE ABAP_FUNC_PARMBIND_TAB
        !PARAMS    TYPE ANY OPTIONAL
        !EXCEPTAB  TYPE ABAP_FUNC_EXCPBIND_TAB OPTIONAL
        !SHOW_IMPP TYPE ABAP_BOOL OPTIONAL
        !JSONP     TYPE STRING OPTIONAL
        !FUNCNAME  TYPE RS38L_FNAM
        !LOWERCASE TYPE ABAP_BOOL DEFAULT ABAP_FALSE
        !FORMAT    TYPE STRING OPTIONAL
      EXPORTING
        !O_STRING  TYPE STRING .
    CLASS-METHODS SERIALIZE_YAML
      IMPORTING
        !PARAMTAB    TYPE ABAP_FUNC_PARMBIND_TAB
        !PARAMS      TYPE ANY OPTIONAL
        !EXCEPTAB    TYPE ABAP_FUNC_EXCPBIND_TAB OPTIONAL
        !SHOW_IMPP   TYPE ABAP_BOOL OPTIONAL
        !JSONP       TYPE STRING OPTIONAL
        !LOWERCASE   TYPE ABAP_BOOL DEFAULT ABAP_FALSE
      EXPORTING
        !YAML_STRING TYPE STRING .
    CLASS-METHODS DESERIALIZE_ID
      IMPORTING
        !JSON     TYPE STRING
      CHANGING
        !PARAMTAB TYPE ABAP_FUNC_PARMBIND_TAB
      RAISING
        ZCX_JSON .
    CLASS-METHODS SERIALIZE_ID
      IMPORTING
        !PARAMTAB  TYPE ABAP_FUNC_PARMBIND_TAB
        !PARAMS    TYPE ANY OPTIONAL
        !EXCEPTAB  TYPE ABAP_FUNC_EXCPBIND_TAB OPTIONAL
        !SHOW_IMPP TYPE ABAP_BOOL OPTIONAL
        !JSONP     TYPE STRING OPTIONAL
        !LOWERCASE TYPE ABAP_BOOL DEFAULT ABAP_FALSE
        !FORMAT    TYPE STRING DEFAULT 'JSON'
        !FUNCNAME  TYPE RS38L_FNAM OPTIONAL
        !CAMELCASE TYPE ABAP_BOOL DEFAULT ABAP_FALSE
      EXPORTING
        !O_STRING  TYPE STRING
      RAISING
        ZCX_JSON .
    CLASS-METHODS CONVERT_TO_UTF8
      IMPORTING
        !I_TEXTO       TYPE STRING
      RETURNING
        VALUE(R_TEXTO) TYPE STRING .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FMCALL_HANDLER IMPLEMENTATION.


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


  METHOD ABAP2PERL.
*/**********************************************/*
*/ This method takes any ABAP data variable and /*
*/ returns a string representing its value in   /*
*/ Perl Data::Dumper format, ready to be evaled /*
*/ in a Perl program.                           /*
*/**********************************************/*

    TYPE-POOLS: ABAP.

    CONSTANTS:
      C_COMMA TYPE C VALUE ',',
      C_COLON TYPE C VALUE ':',
      C_QUOTE TYPE C VALUE ''''.

    DATA:
      PERL_HASH_ASSIGN TYPE STRING,
      DONT_QUOTE       TYPE XFELD,
      PERL_FRAGMENTS   TYPE TABLE OF STRING,
      REC_PERL_STRING  TYPE STRING,
      L_TYPE           TYPE C,
      S_TYPE           TYPE C,
      L_COMPS          TYPE I,
      L_LINES          TYPE I,
      L_INDEX          TYPE I,
      L_VALUE          TYPE STRING,
      L_NAME           TYPE STRING,
      L_TYPEDESCR      TYPE REF TO CL_ABAP_STRUCTDESCR.

    FIELD-SYMBOLS:
      <ABAP_DATA> TYPE ANY,
      <ITAB>      TYPE ANY TABLE,
      <STRU>      TYPE ANY TABLE,
      <COMP>      TYPE ANY,
      <ABAPCOMP>  TYPE ABAP_COMPDESCR.

    CONCATENATE SPACE '=>' SPACE INTO PERL_HASH_ASSIGN RESPECTING BLANKS.

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

    END-OF-DEFINITION.



***************************************************
*  Prepare field names, we use single quotes.     *
*  You must be strict in what you produce.        *
***************************************************
    IF NAME IS NOT INITIAL.
      CONCATENATE C_QUOTE NAME C_QUOTE PERL_HASH_ASSIGN INTO REC_PERL_STRING RESPECTING BLANKS.
      APPEND REC_PERL_STRING TO PERL_FRAGMENTS.
      CLEAR REC_PERL_STRING.
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
        APPEND '{}' TO PERL_FRAGMENTS.
        CONCATENATE LINES OF PERL_FRAGMENTS INTO PERL_STRING.
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
* '[' Table opening bracket
      APPEND '[' TO PERL_FRAGMENTS.
      ASSIGN <ABAP_DATA> TO <ITAB>.
      L_LINES = LINES( <ITAB> ).
      LOOP AT <ITAB> ASSIGNING <COMP>.
        ADD 1 TO L_INDEX.
*> Recursive call here
        REC_PERL_STRING = ABAP2PERL( ABAP_DATA = <COMP> UPCASE = UPCASE ).
        APPEND REC_PERL_STRING TO PERL_FRAGMENTS.
        CLEAR REC_PERL_STRING.
        IF L_INDEX < L_LINES.
          APPEND C_COMMA TO PERL_FRAGMENTS.
        ENDIF.
      ENDLOOP.
      APPEND ']' TO PERL_FRAGMENTS.
* ']' Table closing bracket


***************************************************
*  Structures
***************************************************
    ELSE .
      IF L_COMPS IS NOT INITIAL.
* '{' Object opening curly brace
        APPEND '{' TO PERL_FRAGMENTS .
        L_TYPEDESCR ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( <ABAP_DATA> ) .
        LOOP AT L_TYPEDESCR->COMPONENTS ASSIGNING <ABAPCOMP> .
          L_INDEX = SY-TABIX .
          ASSIGN COMPONENT <ABAPCOMP>-NAME OF STRUCTURE <ABAP_DATA> TO <COMP>.
          L_NAME = <ABAPCOMP>-NAME.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
          IF UPCASE NE 'X'.
            TRANSLATE L_NAME TO LOWER CASE.
          ENDIF.
          DESCRIBE FIELD <COMP> TYPE S_TYPE.
          IF S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF OR
             S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT1 OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT2.
*> Recursive call for non-scalars:
            REC_PERL_STRING = ABAP2PERL( ABAP_DATA = <COMP> NAME = L_NAME UPCASE = UPCASE ).
          ELSE.
            IF S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_OREF OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_IREF.
              REC_PERL_STRING = '"REF UNSUPPORTED"'.
            ELSE.
              GET_SCALAR_VALUE REC_PERL_STRING <COMP> S_TYPE.
            ENDIF.
            CONCATENATE C_QUOTE L_NAME C_QUOTE PERL_HASH_ASSIGN REC_PERL_STRING INTO REC_PERL_STRING.
          ENDIF.

          APPEND REC_PERL_STRING TO PERL_FRAGMENTS.
          CLEAR REC_PERL_STRING.
          IF L_INDEX < L_COMPS.
            APPEND C_COMMA TO PERL_FRAGMENTS.
          ENDIF.
        ENDLOOP.
        APPEND '}' TO PERL_FRAGMENTS.
* '}' Object closing curly brace


****************************************************
*                  - Scalars -                     *
****************************************************
      ELSE.

        GET_SCALAR_VALUE L_VALUE <ABAP_DATA> L_TYPE.
        APPEND L_VALUE TO PERL_FRAGMENTS.

      ENDIF.
* End of structure/scalar IF block.
***********************************


    ENDIF.
* End of main IF block.
**********************


* Use a loop in older releases that don't support concatenate lines.
    CONCATENATE LINES OF PERL_FRAGMENTS INTO PERL_STRING.

  ENDMETHOD.


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


  METHOD ABAP2YAML.
*********************
* ABAP goes to YAML *
*********************

    TYPE-POOLS: ABAP.

    CONSTANTS:
      C_COMMA    TYPE C VALUE ',',
      C_SPACE    TYPE C VALUE ' ',
      C_COLON    TYPE C VALUE ':',
      C_QUOTE    TYPE C VALUE '"',
      C_SQUOT    TYPE C VALUE '''',
      C_COLO2(2) TYPE C VALUE ': ',
      C_INDT2    TYPE I VALUE 2,
      C_HYPH     TYPE C VALUE '-'.

    DATA:
      LY_LEVEL        TYPE I,
      L_DONT_INDENT   TYPE XFELD,
      DEC_LEVEL       TYPE I VALUE 0,
      DONT_QUOTE      TYPE XFELD,
      YAML_FRAGMENTS  TYPE TABLE OF STRING,
      REC_YAML_STRING TYPE STRING,
      L_TYPE          TYPE C,
      L_COMPS         TYPE I,
      L_LINES         TYPE I,
      L_INDEX         TYPE I,
      L_VALUE         TYPE STRING,
      L_NAME          TYPE STRING.
    FIELD-SYMBOLS:
      <ABAP_DATA> TYPE ANY,
      <ITAB>      TYPE ANY TABLE,
      <STRU>      TYPE ANY TABLE,
      <COMP>      TYPE ANY.
    DATA L_TYPEDESCR TYPE REF TO CL_ABAP_STRUCTDESCR .
    FIELD-SYMBOLS <ABAPCOMP> TYPE ABAP_COMPDESCR .

    LY_LEVEL = Y_LEVEL.

**
* Get ABAP data type
    DESCRIBE FIELD ABAP_DATA TYPE L_TYPE COMPONENTS L_COMPS .

***************************************************
*  First of all, get rid of data references
***************************************************
    IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF.
      ASSIGN ABAP_DATA->* TO <ABAP_DATA>.
      IF SY-SUBRC NE 0.
        YAML_STRING = SPACE. " pasamos de poner nada si falla...
        EXIT.
      ENDIF.
    ELSE.
      ASSIGN ABAP_DATA TO <ABAP_DATA>.
    ENDIF.


* Get ABAP data type again and start
    DESCRIBE FIELD <ABAP_DATA> TYPE L_TYPE COMPONENTS L_COMPS.

***************************************************
*  Prepare field names, YAML does not quote names *
***************************************************
* Put hyphens...
    IF NAME IS INITIAL AND Y_LEVEL GT 0.
      CONCATENATE C_HYPH SPACE INTO REC_YAML_STRING RESPECTING BLANKS.
      L_DONT_INDENT = 'X'.
    ENDIF.

    IF NAME IS NOT INITIAL.
      CONCATENATE NAME C_COLON C_SPACE INTO REC_YAML_STRING RESPECTING BLANKS.
    ENDIF.

* do indent
    IF DONT_INDENT NE 'X'.
      DO  LY_LEVEL  TIMES.
        SHIFT REC_YAML_STRING RIGHT BY C_INDT2 PLACES.
      ENDDO.
    ENDIF.

    APPEND REC_YAML_STRING TO YAML_FRAGMENTS.
    CLEAR REC_YAML_STRING.




***************************************************
*  Tables
***************************************************
    IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE.
      ASSIGN <ABAP_DATA> TO <ITAB>.
      L_LINES = LINES( <ITAB> ).
      CLEAR L_INDEX.
      IF L_LINES EQ 0.
        MOVE '[]' TO REC_YAML_STRING.
        APPEND REC_YAML_STRING TO YAML_FRAGMENTS.
        CLEAR REC_YAML_STRING.
        APPEND XNL TO YAML_FRAGMENTS.
      ELSE.
        IF NAME IS NOT INITIAL.
          APPEND XNL TO YAML_FRAGMENTS.
        ENDIF.
        ADD 1 TO LY_LEVEL.
        LOOP AT <ITAB> ASSIGNING <COMP>.
          ADD 1 TO L_INDEX.
*> Recursive call here
          REC_YAML_STRING = ABAP2YAML( ABAP_DATA = <COMP> UPCASE = UPCASE Y_LEVEL = LY_LEVEL S_INDEX = L_INDEX ).
          APPEND REC_YAML_STRING TO YAML_FRAGMENTS.
          CLEAR REC_YAML_STRING.
        ENDLOOP.
      ENDIF.
* YAML table ends *
*******************


***************************************************
*  Structures
***************************************************
    ELSE .
      IF L_COMPS IS NOT INITIAL.
        IF NAME IS NOT INITIAL.
          APPEND XNL TO YAML_FRAGMENTS.
        ENDIF.
        ADD 1 TO LY_LEVEL.
* Loop for structure elements
        L_TYPEDESCR ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( <ABAP_DATA> ) .
        CLEAR L_INDEX.
        LOOP AT L_TYPEDESCR->COMPONENTS ASSIGNING <ABAPCOMP>.
          ADD 1 TO L_INDEX.
          ASSIGN COMPONENT <ABAPCOMP>-NAME OF STRUCTURE <ABAP_DATA> TO <COMP>.
          L_NAME = <ABAPCOMP>-NAME.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
          IF UPCASE NE 'X'.
            TRANSLATE L_NAME TO LOWER CASE.
          ENDIF.
*> Recursive call here
          REC_YAML_STRING = ABAP2YAML( ABAP_DATA = <COMP> NAME = L_NAME UPCASE = UPCASE Y_LEVEL = LY_LEVEL S_INDEX = L_INDEX DONT_INDENT = L_DONT_INDENT ).
          CLEAR L_DONT_INDENT. " it is only used once
          APPEND REC_YAML_STRING TO YAML_FRAGMENTS.
          CLEAR REC_YAML_STRING.
        ENDLOOP.

* YAML structure ends *
***********************


***************************************************
*  Scalars and others...
***************************************************
      ELSE.
        IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_OREF OR L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_IREF.
          L_VALUE = 'REF UNSUPPORTED'.
        ELSE.
          L_VALUE = <ABAP_DATA>.
        ENDIF.

* Adapt some basic ABAP types (pending inclusion of all basic abap types)
* Feel free to customize this for your needs
        CASE L_TYPE.
*       1. ABAP numeric types
          WHEN 'I'. " Integer
            CONDENSE L_VALUE.
            IF SIGN( L_VALUE ) < 0.
              SHIFT L_VALUE BY 1 PLACES RIGHT CIRCULAR.
            ENDIF.
            DONT_QUOTE = 'X'.

          WHEN 'F'. " Float
            CONDENSE L_VALUE.
            DONT_QUOTE = 'X'.

          WHEN 'P'. " Packed number (used in quantities, for example)
            CONDENSE L_VALUE.
            IF SIGN( L_VALUE ) < 0.
              SHIFT L_VALUE BY 1 PLACES RIGHT CIRCULAR.
            ENDIF.
            DONT_QUOTE = 'X'.

          WHEN 'X'. " Hexadecimal
            CONDENSE L_VALUE.
            CONCATENATE '0x' L_VALUE INTO L_VALUE.
            DONT_QUOTE = 'X'.

*       2. ABAP char types
          WHEN 'D'. " Date type
            CONCATENATE L_VALUE(4) '-' L_VALUE+4(2) '-' L_VALUE+6(2) INTO L_VALUE.

          WHEN 'T'. " Time representation
            CONCATENATE L_VALUE(2) ':' L_VALUE+2(2) ':' L_VALUE+4(2) INTO L_VALUE.

          WHEN 'N'. " Numeric text field
*           condense l_value.

          WHEN 'C' OR 'g'. " Chars and Strings
* Put safe chars
            REPLACE ALL OCCURRENCES OF '\' IN L_VALUE WITH '\\' .
            REPLACE ALL OCCURRENCES OF '"' IN L_VALUE WITH '\"' .
            REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF IN L_VALUE WITH '\r\n' .
            REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>NEWLINE IN L_VALUE WITH '\n' .
            REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB IN L_VALUE WITH '\t' .
            REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>BACKSPACE IN L_VALUE WITH '\b' .
            REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>FORM_FEED IN L_VALUE WITH '\f' .

          WHEN 'y'.  " XSTRING
* Put the XSTRING in Base64
*          l_value = cl_http_utility=>ENCODE_X_BASE64( <abap_data> ).
            L_VALUE = 'XSTRING not supported in YAML yet!'.

          WHEN OTHERS.
* Don't hesitate to add and modify abap types to suit your taste.

        ENDCASE.

* We use YAML scalars double quoted
        IF DONT_QUOTE NE 'X'.
          CONCATENATE C_QUOTE L_VALUE C_QUOTE INTO L_VALUE.
        ELSE.
          CLEAR DONT_QUOTE.
        ENDIF.

        APPEND L_VALUE TO YAML_FRAGMENTS.

        APPEND XNL TO YAML_FRAGMENTS.

      ENDIF. " is structure or scalar

    ENDIF. " main typekind sentence



* Use a loop in older releases that don't support concatenate lines.
    CONCATENATE LINES OF YAML_FRAGMENTS INTO YAML_STRING RESPECTING BLANKS.

  ENDMETHOD.


  METHOD BUILD_PARAMS.

    TYPE-POOLS: ABAP.

    DATA DEFVAL TYPE RS38L_DEFO.
    DATA DATANAME TYPE STRING.
    DATA WAREF TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <WA>   TYPE ANY,
      <TEMP> TYPE ANY.

    DATA LEN TYPE I.
    DATA EXCNT TYPE I VALUE 1.

    DATA PARAMLINE  TYPE LINE  OF ABAP_FUNC_PARMBIND_TAB.
    DATA EXCEPTLINE TYPE LINE  OF ABAP_FUNC_EXCPBIND_TAB.
    DATA T_PARAMS_P TYPE TABLE OF RFC_FINT_P.
    DATA PARAMS_P   TYPE RFC_FINT_P.

    DEFINE REMOVE_ENCLOSING_QUOTES.
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
    END-OF-DEFINITION.


* do we have the rfc name?
    CALL FUNCTION 'RFC_GET_FUNCTION_INTERFACE_P'
      EXPORTING
        FUNCNAME      = FUNCTION_NAME
        LANGUAGE      = 'E'       "'D'  "sy-langu
      TABLES
        PARAMS_P      = T_PARAMS_P
      EXCEPTIONS
        FU_NOT_FOUND  = 1
        NAMETAB_FAULT = 2
        OTHERS        = 3.

    IF SY-SUBRC <> 0.
      RAISE INVALID_FUNCTION.
    ENDIF.


* Build params table
    LOOP AT T_PARAMS_P INTO PARAMS_P.

      UNASSIGN <WA>.
      UNASSIGN <TEMP>.
      CLEAR PARAMLINE.

      CASE PARAMS_P-PARAMCLASS.

        WHEN 'I' OR 'E' OR 'C'.

          PARAMLINE-NAME = PARAMS_P-PARAMETER.

          IF PARAMS_P-PARAMCLASS = 'E'.
            PARAMLINE-KIND = ABAP_FUNC_IMPORTING.
          ELSEIF PARAMS_P-PARAMCLASS = 'I'.
            PARAMLINE-KIND = ABAP_FUNC_EXPORTING.
          ELSE.
            PARAMLINE-KIND = ABAP_FUNC_CHANGING.
          ENDIF.

          IF PARAMS_P-FIELDNAME IS INITIAL.
            DATANAME = PARAMS_P-TABNAME.
          ELSE.
            CONCATENATE PARAMS_P-TABNAME PARAMS_P-FIELDNAME INTO
                DATANAME SEPARATED BY '-'.
          ENDIF.

* Assign default values
          DEFVAL = PARAMS_P-DEFAULT.
          IF DATANAME IS INITIAL.
            DATANAME = 'STRING'.  " use a STRING for this cases (see CONVERT_DATE_TO_EXTERNAL).
          ENDIF.
          CREATE DATA WAREF TYPE (DATANAME).
          ASSIGN WAREF->* TO <WA>.
          LEN = STRLEN( DEFVAL ).
          REMOVE_ENCLOSING_QUOTES DEFVAL LEN.
          IF DEFVAL = 'SPACE'.
            <WA> = SPACE.
          ELSEIF LEN > 3 AND DEFVAL+0(3) = 'SY-'.
            ASSIGN (DEFVAL) TO <TEMP>.
            <WA> = <TEMP>.
            UNASSIGN <TEMP>.
          ELSE.
            IF DEFVAL IS NOT INITIAL.
              <WA> = DEFVAL.
            ENDIF.
          ENDIF.
          UNASSIGN <WA>.
          PARAMLINE-VALUE = WAREF.
          INSERT PARAMLINE INTO TABLE PARAMTAB.

        WHEN 'T'.
          PARAMLINE-NAME = PARAMS_P-PARAMETER.
          PARAMLINE-KIND = ABAP_FUNC_TABLES.
          IF PARAMS_P-EXID EQ 'h'.
            CREATE DATA WAREF TYPE (PARAMS_P-TABNAME).
          ELSE.
            CREATE DATA WAREF TYPE STANDARD TABLE OF (PARAMS_P-TABNAME).
          ENDIF.
          PARAMLINE-VALUE = WAREF.
          INSERT PARAMLINE INTO TABLE PARAMTAB.

        WHEN 'X'.
          EXCEPTLINE-NAME = PARAMS_P-PARAMETER.
          EXCEPTLINE-VALUE = EXCNT.
          DATA MESSG TYPE REF TO DATA.
          CREATE DATA MESSG TYPE STRING.
          ASSIGN MESSG->* TO <TEMP>.
          <TEMP> = PARAMS_P-PARAMTEXT.
          EXCEPTLINE-MESSAGE = MESSG.
          INSERT EXCEPTLINE INTO TABLE EXCEPTAB.
          ADD 1 TO EXCNT.

        WHEN OTHERS.
          RAISE UNSUPPORTED_PARAM_TYPE.

      ENDCASE.

    ENDLOOP.


* add in the catch all exception
    EXCEPTLINE-NAME = 'OTHERS'.
    EXCEPTLINE-VALUE = EXCNT.
    INSERT EXCEPTLINE INTO TABLE EXCEPTAB.


* return
    PARAMS = T_PARAMS_P.

*********************************
******* Remaining from 2006 *****
******* end of build_params *****
*********************************
  ENDMETHOD.


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


  METHOD if_http_extension~handle_request.
*/*************************************************************************/
*/ Assign this handler to a service in ICF. This allows any ABAP function */
*/ module to be called directly by URL and exchange data in JSON format.  */
*/ --
*/ This code is distributed under the terms of Apache License Version 2.0 */
*/ (see http://www.apache.org/licenses/LICENSE-2.0.html)                  */
*/ --
*/ (C) César Martín <cesar.martin@sap.com>                                */
*/ Many thanks to Juan Díez for his ideas, help, and support.             */
*/ --
*/*************************************************************************/
*/ If you want to use the SAP standard provided transformation for JSON   */
*/ and XML, uncomment the lines calling serialize_id and deserialize_id   */
*/*************************************************************************/
    TYPE-POOLS abap.

    DATA: show_import_params TYPE abap_bool VALUE abap_false,
          lowercase          TYPE abap_bool VALUE abap_false,
          camelcase          TYPE abap_bool VALUE abap_false,
          path_info          TYPE string,
          p_info_tab         TYPE TABLE OF string,
          format             TYPE string,
          accept             TYPE string,
          action             TYPE string,
          dabliu             TYPE string,
          request_method     TYPE string,
          jsonp_callback     TYPE string,
          i_content_type     TYPE string,
          i_cdata            TYPE string,
          o_cdata            TYPE string,
          o_name             TYPE string,
          o_aux              TYPE string,
          o_data             TYPE xstring,
          exceptheader       TYPE string,
          etext              TYPE string,
          etext2             TYPE string,
          str_item           TYPE string,
          host               TYPE string,
          port               TYPE string,
          proto              TYPE string,
          http_code          TYPE i,
          http_status        TYPE string,
          funcname           TYPE rs38l_fnam,
          funcname2          TYPE string,
          dparam             TYPE abap_parmname,
          t_params_p         TYPE STANDARD TABLE OF rfc_fint_p,
          paramtab           TYPE abap_func_parmbind_tab,
          exceptab           TYPE abap_func_excpbind_tab,
          exception          TYPE LINE OF abap_func_excpbind_tab,
          funcrc             TYPE sy-subrc,
          oexcp              TYPE REF TO cx_root,
          qs_nvp             TYPE tihttpnvp,
          l_lines            TYPE i,
          l_idx              TYPE i,
          lc_tp_ambiente     TYPE zde_tp_ambiente,
          lc_ambiente        TYPE string,
          lc_content_type    TYPE string.

    DATA: i_info TYPE zde_integracao_http_config.

    DATA: lc_erro   TYPE string.
    CONSTANTS: embalagem TYPE rs38l_fnam VALUE 'ZPP_INTERFACE_EMBALAGEM',
               protheus  TYPE rs38l_fnam VALUE 'ZPM_IMP_FATURA_DO_PROTHEUS'.

    FIELD-SYMBOLS <qs_nvp> TYPE ihttpnvp.
    FIELD-SYMBOLS <fm_param> TYPE abap_func_parmbind.
    FIELD-SYMBOLS <fm_value_str> TYPE string.
    FIELD-SYMBOLS <fm_value_i> TYPE i.
    FIELD-SYMBOLS <fm_int_handler> TYPE zicf_handler_data.

    DEFINE http_sucesso.

      server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
      http_code = '200'.
      server->response->set_status( code = http_code  reason = 'Server' ).
      etext = '{"SUCESSO":"OK"}'.
      server->response->set_cdata( etext ).
      EXIT.

    END-OF-DEFINITION.

    DEFINE http_metodo_nao_aceito.

      server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
      http_code = '406'.
      server->response->set_status( code = http_code  reason = 'Client' ).
      etext = '{"ERROR_CODE":"406","ERROR_MESSAGE":"Mensagem não reconhecida, favor enviar requisição ao administrador do sistema de destino para validação"}'.
      server->response->set_cdata( etext ).
      EXIT.

    END-OF-DEFINITION.

    DEFINE http_error.
      "   &1   http status code
      "   &2   status text
      "   &3   error message
      lc_erro = &3.

      IF lc_erro IS NOT INITIAL.
        lc_erro = me->convert_to_utf8( i_texto = lc_erro ).
      ENDIF.

      CASE format.
        WHEN 'json'.
          server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
          http_code = &1.
          server->response->set_status( code = http_code  reason = &2 ).
          CONCATENATE '{"ERROR_CODE":"' &1 '","ERROR_MESSAGE":"' lc_erro '"}' INTO etext.
          server->response->set_cdata( etext ).
        WHEN 'xml'.
          server->response->set_header_field( name = 'Content-Type'  value = 'application/xml; charset=UTF-8' ).
          http_code = &1.
          server->response->set_status( code = http_code  reason = &2 ).
          CONCATENATE '<ERROR_CODE>' &1 '</ERROR_CODE><ERROR_MESSAGE>' lc_erro '</ERROR_MESSAGE>' INTO etext.
          server->response->set_cdata( etext ).
        WHEN OTHERS.
          server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
          http_code = &1.
          server->response->set_status( code = http_code  reason = &2 ).
          CONCATENATE '{"ERROR_CODE":"' &1 '","ERROR_MESSAGE":"' lc_erro '"}' INTO etext.
          server->response->set_cdata( etext ).
      ENDCASE.

      EXIT.

    END-OF-DEFINITION.

* Get Server Info:

    server->get_location( IMPORTING host = host  port = port  out_protocol = proto ).
    CONCATENATE proto '://' host ':' port INTO me->my_url.

** Get all client Info:
*data clnt_hfields type TIHTTPNVP.
*server->request->get_header_fields( changing fields = clnt_hfields ).


* GET and POST and other methods are allowed.
* Uncomment or extend this if you want alternative actions following
* request methods, in order to define a REST style behaviour
* or, better, check an alternative approach on a way to do that
* inside the FM (search for _ICF_DATA below).
*  if request_method <> 'POST'.
****    http_error 405 'Method not allowed' 'Method not allowed.'.
*  endif.

* Get form and header fields
    me->my_service       = server->request->get_header_field( name = '~script_name' ).
    request_method       = server->request->get_header_field( name = '~request_method' ).
    i_content_type       = server->request->get_header_field( name = 'content-type' ).
    show_import_params   = server->request->get_form_field( 'show_import_params'  ).
    action               = server->request->get_form_field( 'action' ).
    dabliu               = server->request->get_form_field( 'dabliu' ).
    jsonp_callback       = server->request->get_form_field( 'callback'  ).
    lowercase            = server->request->get_form_field( 'lowercase' ).
    camelcase            = server->request->get_form_field( 'camelcase' ).
    format               = server->request->get_form_field( 'format' ).
    accept               = server->request->get_header_field( name = 'Accept' ).

* Try "$" equivalents:
    IF format IS INITIAL.
      format = server->request->get_form_field( '$format' ).
    ENDIF.
    IF jsonp_callback IS INITIAL.
      jsonp_callback = server->request->get_form_field( '$callback' ).
    ENDIF.

* Get function name from PATH_INFO
    path_info = server->request->get_header_field( name = '~path_info' ).
    SPLIT path_info AT '/' INTO TABLE p_info_tab.
    READ TABLE p_info_tab INDEX 2 INTO funcname.
    READ TABLE p_info_tab INDEX 3 INTO funcname2.
    IF sy-subrc EQ 0.
      CONCATENATE '//' funcname '/' funcname2 INTO funcname.
      CONDENSE funcname.
    ENDIF.

    IF funcname EQ 'getnfepdf' OR
       funcname EQ 'getnfexml' OR
       funcname EQ 'getctepdf' OR
       funcname EQ 'getctexml' OR
       funcname EQ 'getmdfepdf' OR
       funcname EQ 'getmdfexml' OR
       funcname EQ 'getDocFiscalStatus'.
      DATA: obj_doc_fiscal TYPE REF TO zcl_fmcall_doc_fiscal.
      CREATE OBJECT obj_doc_fiscal.
      obj_doc_fiscal->if_http_extension~handle_request( server = server ).
      CLEAR: obj_doc_fiscal.
      EXIT.
    ELSEIF funcname EQ zcl_fmcall_base=>at_servico_ctb_new_doc OR
           funcname EQ zcl_fmcall_base=>at_servico_ctb_cns_doc OR
           funcname EQ zcl_fmcall_base=>at_servico_ctb_est_doc OR
           funcname EQ 'pedido_compra_cns'.
      DATA: obj_http_base TYPE REF TO zcl_fmcall_base.
      CREATE OBJECT obj_http_base.
      obj_http_base->if_http_extension~handle_request( server = server ).
      CLEAR: obj_http_base.
      EXIT.
    ENDIF.

    IF funcname IS INITIAL AND action IS INITIAL.
      http_error '404' 'Not Found' 'Empty request.' .
    ENDIF.

* Verificar Acesso se o Recurso Exists
    SELECT SINGLE * INTO @DATA(wa_zwst0001)
      FROM zwst0001
     WHERE cd_recurso EQ @funcname.

    IF sy-subrc IS NOT INITIAL.
      CONCATENATE 'Recurso' funcname 'não existe' INTO DATA(lc_texto_01) SEPARATED BY space.
      http_error '404' 'Recurso' lc_texto_01 .
    ENDIF.

* Verificar Acesso se o usuário possui permissão
    IF wa_zwst0001-ck_nao_verif_perfil EQ abap_true.
      AUTHORITY-CHECK OBJECT 'ZWS0001' ID 'CD_RECURSO' FIELD funcname.
      IF sy-subrc IS NOT INITIAL.
        CONCATENATE 'Recurso' wa_zwst0001-ds_recurso 'nao disponivel para usuario/servico' INTO DATA(lc_texto_02) SEPARATED BY space.
        http_error '404' 'Recurso' lc_texto_02.
      ENDIF.
    ENDIF.

*01	Desenvolvimento
*02	Homologação
*03	Produção

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
    SELECT SINGLE * INTO @DATA(wa_zwst0002)
      FROM zwst0002
     WHERE cd_recurso  EQ @funcname
       AND tp_ambiente EQ @lc_tp_ambiente.

    IF sy-subrc IS NOT INITIAL.
      CONCATENATE 'Recurso' wa_zwst0001-ds_recurso 'não disponivel para ambiente de' lc_ambiente INTO DATA(lc_texto_03) SEPARATED BY space.
      http_error '404' 'Recurso' lc_texto_03.
    ENDIF.

    DATA: lc_format    TYPE string.
    DATA: server_base TYPE REF TO cl_http_server.

    DATA(lc_url) = me->my_url && me->my_service && path_info.

    server_base = CAST #( server ).

* Altera Recurso para buscar o Metodo SAP (Call Function/Objeto etc....)
    IF ( wa_zwst0002-nm_recurso IS NOT INITIAL ) AND ( wa_zwst0002-nm_recurso(3) NE 'ZCL' ).

      funcname = wa_zwst0002-nm_recurso.

* Get the desired response format from "Accept" header (as in RFC 2616 sec 14.1)
* See http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html. Thanks Uwe!!
      IF accept IS NOT INITIAL AND format IS INITIAL.
        TRANSLATE accept TO LOWER CASE.
        IF accept CS '/json'.
          format = 'json'.
        ELSEIF accept CS '/yaml'.
          format = 'yaml'.
        ELSEIF accept CS '/xml'.
          format = 'xml'.
        ELSEIF accept CS '/pdf'.
          format = 'pdf'.
        ELSEIF accept CS '/perl'.
          format = 'perl'.
        ELSEIF accept CS '*/*'.
          CLEAR format.  " for the moment, ignore quality levels in Accept, send default format (json).
        ELSE.
          http_error '406' 'Not Acceptable' 'The server cannot send a response which is acceptable according to the combined Accept field value'.
        ENDIF.
      ENDIF.

      TRANSLATE format TO UPPER CASE.
      IF show_import_params IS NOT INITIAL.
        show_import_params = abap_true.
      ENDIF.

***************************
* Do alternative actions...
      CASE action.
        WHEN 'NOTES'.
          server->response->set_header_field( name = 'Content-Type'  value = 'text/html' ).
          server->response->set_status( code = 200 reason = 'OK' ).
          o_cdata = me->notes( ).
          server->response->set_cdata( o_cdata ).
          EXIT.
*    when 'TEST'.
****** TEST *****
*      etext = me->test( i_cdata ).
*      http_error '200' 'Ok' etext.
*      exit.
****** Investigate further... *****
        WHEN 'START_SESSION'.
**      server->stateful = 1.
          server->set_session_stateful( stateful = server->co_enabled ).
        WHEN 'END_SESSION'.
**      server->stateful = 0.
          server->set_session_stateful( stateful = server->co_disabled ).
        WHEN OTHERS.
          " just go on
      ENDCASE.

* Check Authorization. Create the relevant auth object in SU21 and assign
* the authorized functions to the user. Uncomment to implement security.
*  authority-check object 'Z_JSON'
*         id 'FMNAME' field funcname.
*  if sy-subrc ne 0.
*    http_error '403' 'Not authorized' 'You are not authorized to invoke this function module.'.
*  endif.

*****************************************************************************
* check CORS
* see https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS *
*****************************************************************************

******************
* get input data *
******************
      CLEAR i_cdata.
      i_cdata = server->request->get_cdata( ).
      server->request->get_form_fields_cs( CHANGING fields = qs_nvp ).

* We map the query string to a simple JSON input. Handy for REST style queries.
* The query string may come from GET requests in the url and content data in
* POST request in x-www-form-urlencoded. ICF handles this perfectly and mixes both!! Great!!

      lc_content_type = i_content_type.
      TRANSLATE lc_content_type TO UPPER CASE.

      IF ( qs_nvp IS NOT INITIAL AND i_cdata IS INITIAL ) OR
          i_content_type CS 'application/x-www-form-urlencoded'.
        l_lines = lines( qs_nvp ).
        CLEAR l_idx.
        MOVE '{' TO i_cdata.
        LOOP AT qs_nvp ASSIGNING <qs_nvp>.
          ADD 1 TO l_idx.
          TRANSLATE <qs_nvp>-name TO UPPER CASE. " ABAP is upper case internally anyway.
          CONCATENATE i_cdata '"' <qs_nvp>-name '":"' <qs_nvp>-value '"' INTO i_cdata
            RESPECTING BLANKS.
          IF l_idx < l_lines.
            CONCATENATE i_cdata ',' INTO i_cdata RESPECTING BLANKS.
          ENDIF.
        ENDLOOP.
        CONCATENATE i_cdata '}' INTO i_cdata.
      ELSEIF i_cdata IS NOT INITIAL AND NOT lc_content_type CS '/JSON'.
        etext = 'Content-Type ' && i_content_type && ' não previsto!'.
        http_error '500' 'Internal Server Error' etext.
      ENDIF.



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

      lc_url = me->my_url && me->my_service && path_info.

      i_info-ds_url = CONV #( lc_url ).

      CASE funcname.
        WHEN embalagem.

          zcl_integracao_mobile_emb=>zif_integracao_mobile_emb~get_instance(
             )->set_ds_data( EXPORTING i_info = i_info
             )->set_send_msg(
               IMPORTING
                 e_msg = o_cdata
                 e_zintegracao_log = DATA(e_zintegracao_log)
             ).

        WHEN zcl_fmcall_base=>at_info_nf_se.

          zcl_info_nf_se=>zif_info_nf_se~get_instance(
             )->set_ds_data( i_info = i_info
             )->set_send_msg( IMPORTING e_msg = o_cdata
                                        e_zintegracao_log  = e_zintegracao_log
          ).



        WHEN protheus.
*
          zcl_integracao_protheus=>zif_integracao_protheus~get_instance(
             )->set_ds_data( EXPORTING i_info = i_info
             )->set_send_msg(
               IMPORTING
                 e_msg = o_cdata
                 e_zintegracao_log = e_zintegracao_log
             ).

      ENDCASE.



* Prepare params to call function
      CALL METHOD zcl_fmcall_handler=>build_params
        EXPORTING
          function_name    = funcname
        IMPORTING
          params           = t_params_p
          paramtab         = paramtab
          exceptab         = exceptab
        EXCEPTIONS
          invalid_function = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        CONCATENATE 'Invalid Function. ' sy-msgid sy-msgty sy-msgno ': '
                sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO etext SEPARATED BY '-'.
        http_error '500' 'Server Error' etext.
      ENDIF.


**********************
* Process input data *
**********************
      TRY.
          CALL METHOD me->json_deserialize     " The classic method using JavaScript (JSON only)
            EXPORTING
              json     = i_cdata
            CHANGING
              paramtab = paramtab.

        CATCH cx_root INTO oexcp.

          etext = oexcp->if_message~get_text( ).

          http_error '500' 'Internal Server Error' etext.

      ENDTRY.

*/**********************************/*
*/**********************************/*



*/************************************************/*
*/ Pass some request info to function module      /*
*/ for FMs implementing a REST model or whatever  /*
*/************************************************/*
      READ TABLE paramtab WITH KEY name = '_ICF_DATA' ASSIGNING <fm_param>.
      IF sy-subrc EQ 0.
        CREATE DATA <fm_param>-value TYPE zicf_handler_data.
        ASSIGN <fm_param>-value->* TO <fm_int_handler>.
        <fm_int_handler>-request_method = request_method.
        <fm_int_handler>-icf_url = me->my_url.
        <fm_int_handler>-icf_service = me->my_service.
        <fm_int_handler>-path_info = path_info.
        <fm_int_handler>-qs_tab = qs_nvp.
        <fm_int_handler>-i_json_data = i_cdata.
        APPEND '_ICF_DATA' TO <fm_int_handler>-delete_params.
        <fm_int_handler>-server = server. " Beware!
      ENDIF.



****************************
* Call the function module *
****************************
      TRY.

          CALL FUNCTION funcname
            PARAMETER-TABLE
            paramtab
            EXCEPTION-TABLE
            exceptab.

        CATCH cx_root INTO oexcp.

          etext = oexcp->if_message~get_longtext(  preserve_newlines = abap_true ).

          http_error '500' 'Internal Server Error' etext.

      ENDTRY.


* Remove unused exceptions
      funcrc = sy-subrc.
      DELETE exceptab WHERE value NE funcrc.
      READ TABLE exceptab INTO exception WITH KEY value = funcrc.
      IF sy-subrc EQ 0.
        exceptheader = exception-name.
        CALL METHOD server->response->set_header_field(
            name  = 'X-SAPRFC-Exception'
            value = exceptheader ).
      ENDIF.


*/*******************************************************************/*
*/ Read specific FM parameters for REST type interfaces              /*
*/ I need to find a way on how to operate with some http codes:       /*
*/ 201 Created - URI of resource created is set in Location header   /*
*/ 202 Accepted - Response contains status information
*/ 203 Non-Authoritative Information
*/ 204 No Content - NO CONTENT is sent, nothing, nada
*/ 205 Reset Content - NO CONTENT is sent, nothing, nada
*/ 206 Partial Content - probably will not implement this
*/ Codes 3xx, 4xx should also be handled.
*/***********************************/*
      IF <fm_int_handler> IS ASSIGNED.
        IF <fm_int_handler>-http_code IS NOT INITIAL.
          server->response->set_status( code = <fm_int_handler>-http_code  reason = <fm_int_handler>-http_status ).
          CASE <fm_int_handler>-http_code.
            WHEN 204 OR 205.
              EXIT.
            WHEN OTHERS. " many to add?
          ENDCASE.
        ENDIF.
        IF <fm_int_handler>-error_message IS NOT INITIAL.
          str_item = <fm_int_handler>-http_code. CONDENSE str_item.
          http_error str_item <fm_int_handler>-http_status <fm_int_handler>-error_message.
        ENDIF.
* Delete indicated params for not showing them in the response
        LOOP AT <fm_int_handler>-delete_params INTO dparam.
          DELETE paramtab WHERE name EQ dparam.
        ENDLOOP.
      ENDIF.


* Prepare response. Serialize to output format stream.
      CASE format.

        WHEN 'YAML'.

          CALL METHOD me->serialize_yaml
            EXPORTING
              paramtab    = paramtab
              exceptab    = exceptab
              params      = t_params_p
              jsonp       = jsonp_callback
              show_impp   = show_import_params
              lowercase   = lowercase
            IMPORTING
              yaml_string = o_cdata.

          server->response->set_header_field( name = 'Content-Type' value = 'text/plain' ).

* Get CSRF Token
*  SERVER->GET_XSRF_TOKEN(
*    IMPORTING
*      TOKEN                    = DATA(_TOKEN)
*    EXCEPTIONS
*      INTERNAL_ERROR           = 1
*      CALLED_BY_PUBLIC_SERVICE = 2
*      OTHERS                   = 3
*  ).

* Set response:
          server->response->set_header_field( name = 'X-Data-Format' value = format ). "
*  SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'X-CSRF-Token' VALUE = _TOKEN ). "
* Activate compression (will compress when size>1kb if requested by client in Accept-Encoding: gzip. Very nice.).
          server->response->set_compression( ).
          server->response->set_cdata( data = o_cdata ).

        WHEN 'PERL'.

          CALL METHOD me->serialize_perl
            EXPORTING
              paramtab    = paramtab
              exceptab    = exceptab
              params      = t_params_p
              jsonp       = jsonp_callback
              show_impp   = show_import_params
              funcname    = CONV #( wa_zwst0002-nm_recurso )
              lowercase   = lowercase
            IMPORTING
              perl_string = o_cdata.

          server->response->set_header_field( name = 'Content-Type' value = 'text/plain' ).

* Get CSRF Token
*  SERVER->GET_XSRF_TOKEN(
*    IMPORTING
*      TOKEN                    = DATA(_TOKEN)
*    EXCEPTIONS
*      INTERNAL_ERROR           = 1
*      CALLED_BY_PUBLIC_SERVICE = 2
*      OTHERS                   = 3
*  ).

* Set response:
          server->response->set_header_field( name = 'X-Data-Format' value = format ). "
*  SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'X-CSRF-Token' VALUE = _TOKEN ). "
* Activate compression (will compress when size>1kb if requested by client in Accept-Encoding: gzip. Very nice.).
          server->response->set_compression( ).
          server->response->set_cdata( data = o_cdata ).

        WHEN 'XML'.

          CALL METHOD me->serialize_xml
*      CALL METHOD me->serialize_id
            EXPORTING
              paramtab  = paramtab
              exceptab  = exceptab
              params    = t_params_p
              jsonp     = jsonp_callback
              show_impp = show_import_params
              funcname  = wa_zwst0001-cd_recurso
              lowercase = lowercase
              format    = format
            IMPORTING
              o_string  = o_cdata.

          server->response->set_header_field( name = 'Content-Type' value = 'application/xml' ).

* Get CSRF Token
*  SERVER->GET_XSRF_TOKEN(
*    IMPORTING
*      TOKEN                    = DATA(_TOKEN)
*    EXCEPTIONS
*      INTERNAL_ERROR           = 1
*      CALLED_BY_PUBLIC_SERVICE = 2
*      OTHERS                   = 3
*  ).

* Set response:
          server->response->set_header_field( name = 'X-Data-Format' value = format ). "
*  SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'X-CSRF-Token' VALUE = _TOKEN ). "
* Activate compression (will compress when size>1kb if requested by client in Accept-Encoding: gzip. Very nice.).
          server->response->set_compression( ).
          server->response->set_cdata( data = o_cdata ).

        WHEN 'PDF'.

          CALL METHOD me->serialize_pdf
            EXPORTING
              paramtab  = paramtab
              exceptab  = exceptab
              params    = t_params_p
              jsonp     = jsonp_callback
              show_impp = show_import_params
              lowercase = lowercase
              camelcase = camelcase
            IMPORTING
              o_xstring = o_data
              o_name    = o_name.

          DATA(l_pdf_len) = xstrlen( o_data ).
          CONCATENATE '"' o_name '"' INTO o_name.
          CONCATENATE 'attachment; filename = ' o_name INTO o_aux SEPARATED BY space.
          server->response->set_header_fields( fields = VALUE #( ( name = if_http_header_fields=>content_type  value = 'application/pdf' )
                                                                 ( name = if_http_header_fields=>content_transfer_encoding value = 'Binary' )
                                                                 ( name = 'Content-ID' value = o_name )
                                                                 ( name = if_http_header_fields=>content_length value = CONV #( l_pdf_len ) )
                                                                 ( name = if_http_header_fields=>content_disposition  value = o_aux  ) ) ).
          server->response->delete_header_field( if_http_header_fields=>cache_control ).
          server->response->delete_header_field( if_http_header_fields=>expires ).
          server->response->delete_header_field( if_http_header_fields=>pragma ).
          server->response->set_data( data = o_data length = l_pdf_len ).

        WHEN OTHERS. " the others default to JSON.

          format = 'JSON'.

          CALL METHOD me->serialize_json
            EXPORTING
              paramtab  = paramtab
              exceptab  = exceptab
              params    = t_params_p
              action    = dabliu
              jsonp     = jsonp_callback
              show_impp = show_import_params
              lowercase = COND #( WHEN dabliu IS NOT INITIAL THEN abap_true ELSE abap_false )
              camelcase = camelcase
            IMPORTING
              o_string  = o_cdata.

          server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
          IF jsonp_callback IS NOT INITIAL.
            server->response->set_header_field( name = 'Content-Type' value = 'application/javascript' ).
          ENDIF.

*" Get CSRF Token
*  SERVER->GET_XSRF_TOKEN(
*    IMPORTING
*      TOKEN                    = DATA(_TOKEN)
*    EXCEPTIONS
*      INTERNAL_ERROR           = 1
*      CALLED_BY_PUBLIC_SERVICE = 2
*      OTHERS                   = 3
*  ).

* Set response:
          server->response->set_header_field( name = 'X-Data-Format' value = format ). "
*  SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'X-CSRF-Token' VALUE = _TOKEN ). "
* Activate compression (will compress when size>1kb if requested by client in Accept-Encoding: gzip. Very nice.).
          server->response->set_compression( ).
          server->response->set_cdata( data = o_cdata ).
      ENDCASE.

      IF funcname(5) EQ 'ZHCMF'.

        i_info-ds_body = server->request->get_cdata( ).
        i_info-ds_content_type = server->request->get_content_type( ).
        i_info-ds_formato = CONV #( lc_format ).
        i_info-ds_ip_origem = server_base->c_caller_ip.
        i_info-ds_metodo = server->request->get_method( ).
        i_info-ds_server_protocolo = CONV #( server_base->m_protocol_version ).
        i_info-ds_url = CONV #( |{ lc_url CASE = LOWER }| ).
        i_info-ds_funcao_processa = funcname.

        DATA: lc_header_fields TYPE tihttpnvp.

        server->request->get_header_fields( CHANGING fields = lc_header_fields ).

        IF lc_header_fields IS NOT INITIAL.
          i_info-ds_header = zcl_fmcall_base=>abap2json( abap_data = lc_header_fields ).
        ENDIF.

        IF i_info-ds_formato IS INITIAL.
          IF i_info-ds_content_type CS 'JSON'.
            i_info-ds_formato = 'JSON'.
          ENDIF.
          IF i_info-ds_content_type CS 'XML'.
            i_info-ds_formato = 'XML'.
          ENDIF.
        ENDIF.

        zcl_integracao_websempre=>zif_integracao_websempre~get_instance(
          )->set_ds_data( i_info = i_info
          )->set_send_msg( IMPORTING e_msg = o_cdata
        ).

      ENDIF.

*******************************************
*******************************************
**********      *      *          *********
*********        *      *        **********
********          *      *      ***********
*******************************************
*******************************************
    ELSE.


      lc_format = server->request->get_form_field( 'format' ).
      IF lc_format IS INITIAL.
        lc_format = server->request->get_form_field( '$format' ).
      ENDIF.

*    DATA: I_INFO TYPE ZDE_INTEGRACAO_HTTP_CONFIG.
      DATA(obj_consig) = NEW zcl_hr_consig( ).
      DATA(obj_vip) = NEW zcl_integra_vip( ).

*     "temporario
      i_info-ds_body = server->request->get_cdata( ).

*      DATA(_multipart01) = server->request->get_multipart( index =  1 ).
*      DATA(_multipart02) = server->request->get_multipart( index =  2 ).
*
*      IF _multipart01 IS NOT INITIAL.
*        DATA(_data_multipart1) = _multipart01->get_data( ).
*        DATA(_cdata_multipart1) = _multipart01->get_cdata( ).
*        DATA: header_multipart1  TYPE tihttpnvp.
*        _multipart01->get_header_fields( CHANGING fields = header_multipart1  ).
*      ENDIF.
*
*      IF _multipart02 IS NOT INITIAL.
*        DATA(_data_multipart2) = _multipart02->get_data( ).
*        DATA(_cdata_multipart2) = _multipart02->get_cdata( ).
*        DATA: header_multipart2  TYPE tihttpnvp.
*        _multipart02->get_header_fields( CHANGING fields = header_multipart2  ).
*      ENDIF.
*     "fim temporario

      i_info-ds_content_type = server->request->get_content_type( ).
      i_info-ds_formato = CONV #( lc_format ).
      i_info-ds_ip_origem = server_base->c_caller_ip.
      i_info-ds_metodo = server->request->get_method( ).
      i_info-ds_server_protocolo = CONV #( server_base->m_protocol_version ).
      i_info-ds_url = CONV #( lc_url ).

      IF i_info-ds_formato IS INITIAL.
        IF i_info-ds_content_type CS 'JSON'.
          i_info-ds_formato = 'JSON'.
        ENDIF.
        IF i_info-ds_content_type CS 'XML'.
          i_info-ds_formato = 'XML'.
        ENDIF.
      ENDIF.

      DATA: e_name TYPE string,
            e_tipo TYPE string.

      CHECK funcname IS NOT INITIAL.
      TRANSLATE funcname TO LOWER CASE.

      TRY.
          CASE funcname.
            WHEN zcl_fmcall_base=>at_servico_pdf_xml_nfe_cte.

              DATA(r_arquivo) =
              zcl_doc_eletronico=>get_arq_doc_fiscal(
                EXPORTING
                  i_json    = i_info-ds_body
                IMPORTING
                  e_name    = e_name
                  e_tipo    = e_tipo ).

              CASE e_tipo.
                WHEN 'XML'.

                  server->response->set_header_field( name = 'Content-Type' value = 'application/xml' ).
                  server->response->set_compression( ).
                  o_cdata = zcl_string=>xstring_to_string( i_xstring = r_arquivo ).
                  server->response->set_cdata( data = o_cdata ).

                WHEN 'PDF'.

                  l_pdf_len = xstrlen( r_arquivo ).
                  CONCATENATE '"' e_name '"' INTO o_name.
                  CONCATENATE 'attachment; filename = ' o_name INTO o_aux SEPARATED BY space.
                  DATA(content_type_ret) = COND string( WHEN e_tipo EQ 'PDF' THEN 'application/pdf' WHEN e_tipo EQ 'XML' THEN 'application/xml'  ).
                  server->response->set_header_fields( fields = VALUE #( ( name = if_http_header_fields=>content_type  value = content_type_ret )
                                                                         ( name = if_http_header_fields=>content_transfer_encoding value = 'Binary' )
                                                                         ( name = 'Content-ID' value = o_name )
                                                                         ( name = if_http_header_fields=>content_length value = CONV #( l_pdf_len ) )
                                                                         ( name = if_http_header_fields=>content_disposition  value = o_aux  ) ) ).
                  server->response->delete_header_field( if_http_header_fields=>cache_control ).
                  server->response->delete_header_field( if_http_header_fields=>expires ).
                  server->response->delete_header_field( if_http_header_fields=>pragma ).
                  server->response->set_data( data = r_arquivo length = l_pdf_len ).

              ENDCASE.

            WHEN zcl_fmcall_base=>at_servico_nfe_writer_grc.

              zcl_integracao_grc_new_nfe=>zif_integracao_grc_new_nfe~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->set_send_msg(
                      IMPORTING
                        e_msg = o_cdata
                        e_zintegracao_log	= e_zintegracao_log
                 ).

              server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

              IF e_zintegracao_log-nm_code IS NOT INITIAL AND
                 e_zintegracao_log-nm_code NE '200'.
                server->response->set_status(
                    code = CONV #( e_zintegracao_log-nm_code )
                    reason = CONV #( e_zintegracao_log-ds_erro ) ).
              ENDIF.

              " 07.03.2022 - RAMON LIMA - COLETA DE ASSINATURA ENVIO BRY -->
            WHEN zcl_fmcall_base=>at_servico_bry_finalizada.

              DATA lt_head_f TYPE tihttpnvp.

              CALL METHOD server->request->if_http_entity~get_header_fields
                CHANGING
                  fields = lt_head_f.

              zcl_integracao_bry_adigital_in=>zif_integracao_bry_adigital_in~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->set_send_msg(
                      IMPORTING
                        e_msg = o_cdata
                        e_zintegracao_log  = e_zintegracao_log
                 ).

              IF e_zintegracao_log-nm_code = 200.
                http_sucesso.
              ELSE.
                http_error e_zintegracao_log-nm_code 'Bad Request' o_cdata.
              ENDIF.

*Comentado 04/04/2023 para subir implementação melhorias API Inbound - Projeto SHHana US 101006 - Ini
*-CS2021000218-24.10.2022-#90707-JT-inicio - este processo foi comentado,pois o documento assinado nao virá
* ----mais da BRY, e sim da AGRIQ
*              zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
*                 )->set_ds_data_agriq( i_info            = i_info
*                                       i_funcao_processa = zcl_fmcall_base=>at_servico_bry_finalizada
*                 )->set_send_msg_agriq(
*                      IMPORTING
*                        e_msg                 = o_cdata
*                        e_zintegracao_log     = e_zintegracao_log
*                 ).
*
*              server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
*
*              IF e_zintegracao_log-nm_code IS NOT INITIAL AND
*                 e_zintegracao_log-nm_code NE '200'.
*                server->response->set_status(
*                    code = CONV #( e_zintegracao_log-nm_code )
*                    reason = CONV #( e_zintegracao_log-ds_erro ) ).
*              ENDIF.
*-CS2021000218-24.10.2022-#90707-JT-fim
*Comentado 04/04/2023 para subir implementação melhorias API Inbound - Projeto SHHana US 101006 - Fim

              " 07.03.2022 - RAMON LIMA - COLETA DE ASSINATURA ENVIO BRY --<

              " 22.03.2022 - Implementação coupa - Caio

            WHEN zcl_fmcall_base=>at_servico_coupa_requisicao.

              zcl_integracao_coupa_requisit=>zif_integracao_coupa_requisit~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->set_send_msg(
                      IMPORTING
                        e_msg = o_cdata
                        e_zintegracao_log	= e_zintegracao_log
                 ).

              IF e_zintegracao_log-nm_code = 200.
                http_sucesso.
              ELSE.
                http_error e_zintegracao_log-nm_code 'Bad Request' o_cdata.
              ENDIF.

            WHEN zcl_fmcall_base=>at_servico_coupa_pedido.

              zcl_integracao_coupa_order=>zif_integracao_coupa_order~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->set_send_msg(
                      IMPORTING
                        e_msg = o_cdata
                        e_zintegracao_log	= e_zintegracao_log
                 ).

              IF e_zintegracao_log-nm_code = 200.
                http_sucesso.
              ELSE.
                http_error e_zintegracao_log-nm_code 'Bad Request' o_cdata.
              ENDIF.

              " 22.03.2022 - Implementação coupa - Caio

*------------inicio alteração Bruna---------------------
            WHEN zcl_fmcall_base=>at_servico_new_romaneio.

              zcl_integracao_new_romaneio=>zif_integracao_new_romaneio~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->set_send_msg(
                      IMPORTING
                        e_msg = o_cdata
                        e_zintegracao_log	= e_zintegracao_log
                 ).

              server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

              IF e_zintegracao_log-nm_code IS NOT INITIAL AND
                 e_zintegracao_log-nm_code NE '200'.
                server->response->set_status(
                    code = CONV #( e_zintegracao_log-nm_code )
                    reason = CONV #( e_zintegracao_log-ds_erro ) ).
              ENDIF.

*-CS2021000286 - 01.10.2021 - JT - inicio
            WHEN zcl_fmcall_base=>at_servico_obs_nf_zles0050.

              zcl_integracao_obs_zles0050=>zif_integracao_obs_zles0050~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->set_send_msg(
                      IMPORTING
                        e_msg = o_cdata
                        e_zintegracao_log  = e_zintegracao_log
                 ).

              server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

              IF e_zintegracao_log-nm_code IS NOT INITIAL AND
                 e_zintegracao_log-nm_code NE '200'.
                server->response->set_status(
                    code = CONV #( e_zintegracao_log-nm_code )
                    reason = CONV #( e_zintegracao_log-ds_erro ) ).
              ENDIF.
*-CS2021000286 - 01.10.2021 - JT - fim

*-CS2021000207 - 03.03.2022 - JT - inicio
            WHEN zcl_fmcall_base=>at_servico_compensacao_scp.

              zcl_integracao_compensa_scp=>zif_integracao_compensa_scp~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->set_send_msg(
                      IMPORTING
                        e_msg = o_cdata
                        e_zintegracao_log  = e_zintegracao_log
                 ).

              server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

              IF e_zintegracao_log-nm_code IS NOT INITIAL AND
                 e_zintegracao_log-nm_code NE '200'.
                server->response->set_status(
                    code = CONV #( e_zintegracao_log-nm_code )
                    reason = CONV #( e_zintegracao_log-ds_erro ) ).
              ENDIF.
*-CS2021000207 - 03.03.2022 - JT - fim

*Comentado 04/04/2023 para subir implementação melhorias API Inbound - Projeto SHHana US 101006 - Ini
*-CS2021000218-24.10.2022-#90707-JT-inicio
            WHEN zcl_fmcall_base=>at_servico_receita_agriq.

              zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
                 )->set_ds_data_agriq( i_info            = i_info
                                       i_funcao_processa = zcl_fmcall_base=>at_servico_receita_agriq
                 )->set_send_msg_agriq(
                      IMPORTING
                        e_msg                 = o_cdata
                        e_zintegracao_log     = e_zintegracao_log
                 ).

              server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

              IF e_zintegracao_log-nm_code IS NOT INITIAL AND
                 e_zintegracao_log-nm_code NE '200'.
                server->response->set_status(
                    code = CONV #( e_zintegracao_log-nm_code )
                    reason = CONV #( e_zintegracao_log-ds_erro ) ).
              ENDIF.

            WHEN zcl_fmcall_base=>at_servico_receita_agriq_assin.

              zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
                 )->set_ds_data_agriq( i_info            = i_info
                                       i_funcao_processa = zcl_fmcall_base=>at_servico_receita_agriq_assin
                 )->set_send_msg_agriq(
                      IMPORTING
                        e_msg                 = o_cdata
                        e_zintegracao_log     = e_zintegracao_log
                 ).

              server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

              IF e_zintegracao_log-nm_code IS NOT INITIAL AND
                 e_zintegracao_log-nm_code NE '200'.
                server->response->set_status(
                    code = CONV #( e_zintegracao_log-nm_code )
                    reason = CONV #( e_zintegracao_log-ds_erro ) ).
              ENDIF.
*-CS2021000218-24.10.2022-#90707-JT-fim
*Comentado 04/04/2023 para subir implementação melhorias API Inbound - Projeto SHHana US 101006 - Fim

            WHEN zcl_fmcall_base=>at_servico_nfe_writer_grc_can.

              zcl_integracao_grc_new_nfe_ca=>zif_integracao_grc_new_nfe_ca~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->set_send_msg(
                      IMPORTING
                        e_msg = o_cdata
                        e_zintegracao_log = e_zintegracao_log
                 ).

              server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

              IF e_zintegracao_log-nm_code IS NOT INITIAL AND
                 e_zintegracao_log-nm_code NE '200'.
                server->response->set_status(
                    code = CONV #( e_zintegracao_log-nm_code )
                    reason = CONV #( e_zintegracao_log-ds_erro ) ).
              ENDIF.

            WHEN zcl_fmcall_base=>at_servico_nfe_writer_grc_est.

              zcl_integracao_grc_new_nfe_es=>zif_integracao_grc_new_nfe_es~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->set_send_msg(
                      IMPORTING
                        e_msg = o_cdata
                        e_zintegracao_log = e_zintegracao_log
                 ).

              server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

              IF e_zintegracao_log-nm_code IS NOT INITIAL AND
                 e_zintegracao_log-nm_code NE '200'.
                server->response->set_status(
                    code = CONV #( e_zintegracao_log-nm_code )
                    reason = CONV #( e_zintegracao_log-ds_erro ) ).
              ENDIF.

            WHEN zcl_fmcall_base=>at_servico_valida_xml_nfe.

              zcl_nfe_xml=>zif_nfe_xml~get_instance(
                )->get_web_service_validar(
                EXPORTING
                  i_json       = i_info-ds_body
                IMPORTING
                  e_json       = o_cdata
                ).

            WHEN zcl_fmcall_base=>at_servico_inbound_ord_cancela.

              "Assincrona
              zcl_integracao_viagem_cancela=>zif_integracao_viagem_cancela~get_instance(
                )->set_ds_data( EXPORTING i_info = i_info
                )->set_send_msg( IMPORTING e_msg = o_cdata
                ).

              http_sucesso.

            WHEN zcl_fmcall_base=>at_servico_inbound_ord_carrega.

              "Assincrona
              zcl_integracao_ord_carrega=>zif_integracao_ord_carrega~get_instance(
                )->set_ds_data( EXPORTING i_info = i_info
                )->set_send_msg( IMPORTING e_msg = o_cdata
                ).

              http_sucesso.

            WHEN zcl_hr_consig=>at_servico_lanca_emprestimo.

              o_cdata = obj_consig->set_data_emprestimo( i_info = i_info ).

            WHEN zcl_hr_consig=>at_consig_quita_parcela.

              o_cdata = obj_consig->set_data_quita_parcela( i_info = i_info ).

*            WHEN ZCL_HR_CONSIG=>AT_CONSIG_CONS_FOLHA_AB_FECHA.
*
*              O_CDATA = OBJ_CONSIG->SET_DATA_FOLHA_ABERTA_FECHADA( I_INFO = I_INFO ).
*              SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type'  VALUE = 'application/json; charset=UTF-8' ).

            WHEN zcl_hr_consig=>at_consig_quita_total.

              o_cdata = obj_consig->set_data_quita_total( i_info = i_info ).

            WHEN zcl_integra_vip=>at_api_invoice OR
                 zcl_integra_vip=>at_api_invoice_reversal OR
                 zcl_integra_vip=>at_api_invoice_cancel.

              o_cdata = obj_vip->set_invoice_xml_to_sap( i_xml = i_info-ds_body ).

            WHEN zcl_fmcall_base=>at_interface_app_produtor_so.

              zcl_integracao_app_produtor=>zif_integracao_app_produtor~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->get_solicitacoes(
                 )->set_send_msg( IMPORTING e_msg = o_cdata
              ).

            WHEN zcl_fmcall_base=>at_interface_app_produtor_51.

              zcl_integracao_app_produtor=>zif_integracao_app_produtor~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->get_ordem_vendas(
                 )->set_send_msg( IMPORTING e_msg = o_cdata
              ).

            WHEN zcl_fmcall_base=>at_interface_app_produtor_re.

              zcl_integracao_app_produtor=>zif_integracao_app_produtor~get_instance(
                  )->set_ds_data( i_info = i_info
                  )->get_remessas(
                  )->set_send_msg( IMPORTING e_msg = o_cdata
               ).

            WHEN zcl_fmcall_base=>at_interface_app_produtor_te.

              zcl_integracao_app_produtor=>zif_integracao_app_produtor~get_instance(
                  )->set_ds_data( i_info = i_info
                  )->set_termos_aceite(
                  )->set_send_msg( IMPORTING e_msg = o_cdata
               ).

            WHEN zcl_fmcall_base=>at_estorno_doc_contabil.

              zcl_estorno_doc_contabil=>zif_estorno_doc_contabil~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->set_send_msg( IMPORTING e_msg = o_cdata
                                            e_zintegracao_log  = e_zintegracao_log ).

            WHEN zcl_fmcall_base=>at_interface_app_itinerario.

              zcl_integracao_trocantrobo=>zif_integracao_trocantrobo~get_instance(
                 )->set_ds_data( i_info = i_info
                 )->get_itinerario(
                 )->set_send_msg( IMPORTING e_msg = o_cdata
                                            e_zintegracao_log  = e_zintegracao_log ).

*Comentado 04/04/2023 para subir implementação melhorias API Inbound - Projeto SHHana US 101006 - Ini

              "Inicio - falheiros - 12.12.2022 - Criar API para os estornos doc. fatura - #90561
*            WHEN zcl_fmcall_base=>at_servico_cancel_doc_fatura.
*
*              zcl_cancelar_doc_fatura=>zif_cancelar_doc_fatura~get_instance(
*               )->set_ds_data( i_info = i_info
*               )->set_send_msg( IMPORTING e_msg = o_cdata
*                                          e_zintegracao_log  = e_zintegracao_log ).

              "// #96169 Wsb d:18/11/22 Inicio
*            WHEN zcl_fmcall_base=>at_plan_produce_oc_cancel.
*
*              zcl_integracao_plan_produce=>zif_integracao_plan_produce~get_instance(
*                 )->set_ds_data( i_info = i_info
*                 )->set_oc_cancelar(
*                 )->set_send_msg( IMPORTING e_msg = o_cdata
*                                            e_zintegracao_log  = e_zintegracao_log ).
*
*            WHEN zcl_fmcall_base=>at_plan_produce_oc_peso_vazio.
*
*              zcl_integracao_plan_produce=>zif_integracao_plan_produce~get_instance(
*                 )->set_ds_data( i_info = i_info
*                 )->set_oc_pesovazio(
*                 )->set_send_msg( IMPORTING e_msg = o_cdata
*                                            e_zintegracao_log  = e_zintegracao_log ).
*
*            WHEN zcl_fmcall_base=>at_plan_produce_oc_mod.
*
*              zcl_integracao_plan_produce=>zif_integracao_plan_produce~get_instance(
*                 )->set_ds_data( i_info = i_info
*                 )->set_oc_modificar(
*                 )->set_send_msg( IMPORTING e_msg = o_cdata
*                                            e_zintegracao_log  = e_zintegracao_log ).
*           "// #96169 Wsb dt:18/11/22 Fim

*            WHEN zcl_fmcall_base=>at_info_nf_se.
*              zcl_info_nf_se=>zif_info_nf_se~get_instance(
*                 )->set_ds_data( i_info = i_info
*                 )->set_send_msg( IMPORTING e_msg = o_cdata
*                                            e_zintegracao_log  = e_zintegracao_log
*             ).

*            WHEN zcl_fmcall_base=>at_estrat_aprov.
*
*              zcl_integracao_estrat_aprov=>zif_integracao_estrat_aprov~get_instance(
*                 )->set_ds_data( i_info = i_info
*                 )->set_send_msg( IMPORTING e_msg = o_cdata
*                                            e_zintegracao_log  = e_zintegracao_log
*             ).

*Comentado 04/04/2023 para subir implementação melhorias API Inbound - Projeto SHHana US 101006 - Fim

            WHEN OTHERS.

              IF wa_zwst0002-nm_recurso(3) EQ 'ZCL' . "Adaptações para Chamada Classe Integração de forma dinaminca

                 TRY. "Conversao S4 Hana - Projeto Hana - Amaaggi - WPP
                    DATA: lob_integracao TYPE REF TO zif_integracao_inbound.

                    FREE: lob_integracao.
                    CREATE OBJECT lob_integracao TYPE (wa_zwst0002-nm_recurso).

                    lob_integracao->set_data( i_info = i_info
                                 )->processar_requisicao( IMPORTING e_msg              = o_cdata
                                                                    e_zintegracao_log  = e_zintegracao_log
                                 )->configure_server( i_http_server  = server ).
                 CATCH cx_root.
                   http_error '500' 'Server' 'Classe Not Found'.
                 ENDTRY.
              ELSE.

                DATA(obj_interface) = zcl_integracao_inject=>get_instance( ).
                obj_interface->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
                obj_interface->zif_integracao_inject~at_info_request_http = i_info.
                obj_interface->zif_integracao_inject~at_anonimo           = abap_true.

                zcl_integracao=>zif_integracao~get_instance(
                  )->set_msg_inject( i_msg = CAST #( obj_interface )
                  )->set_new_msg(
                  )->free(
                  ).

                http_metodo_nao_aceito.

              ENDIF.

          ENDCASE.

        CATCH zcx_integracao INTO DATA(ex_integracao).

          TRY .
              obj_interface = zcl_integracao_inject=>get_instance( ).
              obj_interface->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
              obj_interface->zif_integracao_inject~at_info_request_http = i_info.
              obj_interface->zif_integracao_inject~at_anonimo           = abap_true.

              zcl_integracao=>zif_integracao~get_instance(
                )->set_msg_inject( i_msg = CAST #( obj_interface )
                )->set_new_msg(
                )->free(
                ).
            CATCH cx_root.
          ENDTRY.

          DATA(ms_erro) = ex_integracao->zif_error~get_msg_erro( ).
          http_error '405' 'Server' ms_erro.

        CATCH zcx_nfe_xml INTO DATA(ex_nfe_xml).

          ms_erro = ex_nfe_xml->zif_error~get_msg_erro( ).
          http_error '405' 'Server' ms_erro.

        CATCH zcx_error INTO DATA(ex_error).

          TRY .
              obj_interface = zcl_integracao_inject=>get_instance( ).
              obj_interface->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
              obj_interface->zif_integracao_inject~at_info_request_http = i_info.
              obj_interface->zif_integracao_inject~at_anonimo           = abap_true.

              zcl_integracao=>zif_integracao~get_instance(
                )->set_msg_inject( i_msg = CAST #( obj_interface )
                )->set_new_msg(
                )->free(
                ).

            CATCH cx_root.
          ENDTRY.

          ms_erro = ex_error->zif_error~get_msg_erro( ).
          ms_erro = zcl_string=>convert_to_utf8( i_texto = zcl_string=>tira_acentos( i_texto = ms_erro ) ).
          http_error '405' 'Server' ms_erro.

      ENDTRY.

      server->response->set_compression( ).
      IF funcname NE zcl_fmcall_base=>at_servico_pdf_xml_nfe_cte.
        server->response->set_cdata( data = o_cdata ).
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD JSON2ABAP.
*/************************************************/*
*/ Input any abap data and this method tries to   /*
*/ fill it with the data in the JSON string.      /*
*/  Thanks to Juan Diaz for helping here!!        /*
*/************************************************/*

    TYPE-POOLS: ABAP, JS.

    DATA:
      JS_SCRIPT         TYPE STRING,
      JS_STARTED        TYPE I VALUE 0,
      L_JSON_STRING     TYPE STRING,
      JS_PROPERTY_TABLE TYPE   JS_PROPERTY_TAB,
      JS_PROPERTY       TYPE LINE OF JS_PROPERTY_TAB,
      L_PROPERTY_PATH   TYPE STRING,
      COMPNAME          TYPE STRING,
      ITEM_PATH         TYPE STRING.

    DATA:
      L_TYPE   TYPE C,
      L_VALUE  TYPE STRING,
      LINETYPE TYPE STRING,
      L_COMP   TYPE LINE OF ABAP_COMPDESCR_TAB.

    DATA:
      DATADESC TYPE REF TO CL_ABAP_TYPEDESCR,
      DREFDESC TYPE REF TO CL_ABAP_TYPEDESCR,
      LINEDESC TYPE REF TO CL_ABAP_TYPEDESCR,
      STRUDESC TYPE REF TO CL_ABAP_STRUCTDESCR,
      TABLDESC TYPE REF TO CL_ABAP_TABLEDESCR.

    DATA NEWLINE TYPE REF TO DATA.

    FIELD-SYMBOLS:
      <ABAP_DATA> TYPE ANY,
      <ITAB>      TYPE ANY TABLE,
      <COMP>      TYPE ANY,
      <JSPROP>    TYPE LINE OF JS_PROPERTY_TAB,
      <ABAPCOMP>  TYPE ABAP_COMPDESCR.


    DEFINE ASSIGN_SCALAR_VALUE.
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
    END-OF-DEFINITION.


    IF JS_OBJECT IS NOT BOUND.

      IF JSON_STRING IS INITIAL. EXIT. ENDIF. " exit method if there is nothing to parse

      L_JSON_STRING = JSON_STRING.
      " js_object = cl_java_script=>create( STACKSIZE = 16384 ).
      JS_OBJECT = CL_JAVA_SCRIPT=>CREATE( STACKSIZE = 16384 HEAPSIZE = 960000 ).

***************************************************
*  Parse JSON using JavaScript                    *
***************************************************
      JS_OBJECT->BIND( EXPORTING NAME_OBJ = 'abap_data' NAME_PROP = 'json_string'    CHANGING DATA = L_JSON_STRING ).
      JS_OBJECT->BIND( EXPORTING NAME_OBJ = 'abap_data' NAME_PROP = 'script_started' CHANGING DATA = JS_STARTED ).

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

      CONCATENATE

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


         INTO JS_SCRIPT RESPECTING BLANKS SEPARATED BY XNL.

      JS_OBJECT->COMPILE( SCRIPT_NAME = 'json_parser'     SCRIPT = JS_SCRIPT ).
      JS_OBJECT->EXECUTE( SCRIPT_NAME = 'json_parser' ).

      IF JS_OBJECT->LAST_ERROR_MESSAGE IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_JSON
          EXPORTING
            MESSAGE = JS_OBJECT->LAST_ERROR_MESSAGE.
      ENDIF.

    ENDIF.
** End of JS processing.

**
    IF VAR_NAME IS NOT INITIAL.
      CONCATENATE PROPERTY_PATH VAR_NAME INTO L_PROPERTY_PATH SEPARATED BY '.'.
    ELSE.
      L_PROPERTY_PATH = PROPERTY_PATH.
    ENDIF.
**
**
    JS_PROPERTY_TABLE = JS_OBJECT->GET_PROPERTIES_SCOPE_GLOBAL( PROPERTY_PATH = L_PROPERTY_PATH ).
    PROPERTY_TABLE = JS_PROPERTY_TABLE.

* Exit if abap_data is not supplied, normally when called
* from json_deserialize to get top level properties
    IF ABAP_DATA IS NOT SUPPLIED.
      EXIT.
    ENDIF. "***

*
* Get ABAP data type, dereference if necessary and start
    DATADESC = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( ABAP_DATA ).
    IF DATADESC->KIND EQ CL_ABAP_TYPEDESCR=>KIND_REF.
      ASSIGN ABAP_DATA->* TO <ABAP_DATA>.
    ELSE.
      ASSIGN ABAP_DATA TO <ABAP_DATA>.
    ENDIF.
    DATADESC = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( <ABAP_DATA> ).


    CASE DATADESC->KIND.

      WHEN CL_ABAP_TYPEDESCR=>KIND_ELEM.
* Scalar: process ABAP elements. Assume no type conversions for the moment.
        IF VAR_NAME IS INITIAL.
          RAISE EXCEPTION TYPE ZCX_JSON
            EXPORTING
              MESSAGE = 'VAR_NAME is required for scalar values.'.
        ENDIF.
        JS_PROPERTY_TABLE = JS_OBJECT->GET_PROPERTIES_SCOPE_GLOBAL( PROPERTY_PATH = PROPERTY_PATH ).
        READ TABLE JS_PROPERTY_TABLE WITH KEY NAME = VAR_NAME INTO JS_PROPERTY.
        IF SY-SUBRC EQ 0.
          ASSIGN_SCALAR_VALUE <ABAP_DATA> JS_PROPERTY-VALUE.
        ENDIF.


      WHEN CL_ABAP_TYPEDESCR=>KIND_STRUCT.
* Process ABAP structures
        STRUDESC ?= DATADESC.
        LOOP AT JS_PROPERTY_TABLE ASSIGNING <JSPROP>.
          COMPNAME = <JSPROP>-NAME.
          TRANSLATE COMPNAME TO UPPER CASE.
          READ TABLE STRUDESC->COMPONENTS WITH KEY NAME = COMPNAME INTO L_COMP.
          IF SY-SUBRC EQ 0.
            ASSIGN COMPONENT L_COMP-NAME OF STRUCTURE <ABAP_DATA> TO <COMP>.
            CASE L_COMP-TYPE_KIND.
              WHEN    CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT1  " 'v'
                   OR CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT2  " 'u'
                   OR CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE.   " 'h' (may need a different treatment one day)
                CONCATENATE L_PROPERTY_PATH <JSPROP>-NAME INTO ITEM_PATH SEPARATED BY '.'.
*> Recursive call here
                JSON2ABAP( EXPORTING PROPERTY_PATH = ITEM_PATH CHANGING ABAP_DATA = <COMP> JS_OBJECT = JS_OBJECT ).

              WHEN OTHERS.
* Process scalars in structures (same as the kind_elem above)
                ASSIGN_SCALAR_VALUE <COMP> <JSPROP>-VALUE.

            ENDCASE.
          ENDIF.
        ENDLOOP.

      WHEN CL_ABAP_TYPEDESCR=>KIND_TABLE.
* Process ABAP tables
        IF JS_PROPERTY_TABLE IS NOT INITIAL.
          TABLDESC ?= DATADESC.
          LINEDESC = TABLDESC->GET_TABLE_LINE_TYPE( ).
          LINETYPE = LINEDESC->GET_RELATIVE_NAME( ).
          ASSIGN <ABAP_DATA> TO <ITAB>.
          LOOP AT JS_PROPERTY_TABLE INTO JS_PROPERTY WHERE NAME NE 'length'. " the JS object length
            CREATE DATA NEWLINE TYPE (LINETYPE).
            ASSIGN NEWLINE->* TO <COMP>.
            CASE JS_PROPERTY-KIND.
              WHEN 'O'.
                CONCATENATE L_PROPERTY_PATH JS_PROPERTY-NAME INTO ITEM_PATH SEPARATED BY '.'.
                CONDENSE ITEM_PATH.
*> Recursive call here
                JSON2ABAP( EXPORTING PROPERTY_PATH = ITEM_PATH CHANGING ABAP_DATA = NEWLINE JS_OBJECT = JS_OBJECT ).
              WHEN OTHERS. " Assume scalars, 'S', 'I', or other JS types
                " Process scalars in plain table components(same as the kind_elem above)
                ASSIGN_SCALAR_VALUE <COMP> JS_PROPERTY-VALUE.
            ENDCASE.
            INSERT <COMP> INTO TABLE <ITAB>.
            FREE NEWLINE.
          ENDLOOP.
        ENDIF.

      WHEN OTHERS. " kind_class, kind_intf
        " forget it.

    ENDCASE.


  ENDMETHOD.


  METHOD JSON_DESERIALIZE.

    TYPE-POOLS: ABAP, JS.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

    DATA PARAMNAME   TYPE STRING.
    DATA JS_OBJ      TYPE REF TO CL_JAVA_SCRIPT.
    DATA JS_PROP_TAB TYPE JS_PROPERTY_TAB.

    FIELD-SYMBOLS <JS_PROP> TYPE LINE OF JS_PROPERTY_TAB.
    FIELD-SYMBOLS <PARM>    TYPE ABAP_FUNC_PARMBIND.

    IF JSON IS INITIAL. EXIT. ENDIF.

    JSON2ABAP( EXPORTING JSON_STRING = JSON  IMPORTING PROPERTY_TABLE = JS_PROP_TAB  CHANGING JS_OBJECT = JS_OBJ ).

    LOOP AT JS_PROP_TAB ASSIGNING <JS_PROP>.
      PARAMNAME = <JS_PROP>-NAME.
      TRANSLATE PARAMNAME TO UPPER CASE.
      READ TABLE PARAMTAB WITH KEY NAME = PARAMNAME ASSIGNING <PARM>.
      IF SY-SUBRC EQ 0.
        IF <PARM>-KIND NE ABAP_FUNC_IMPORTING. "" va al revés, cuidado!!!
          JSON2ABAP( EXPORTING VAR_NAME = <JS_PROP>-NAME  CHANGING ABAP_DATA = <PARM>-VALUE JS_OBJECT = JS_OBJ ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD NOTES.

    DATA LOCATION TYPE STRING.

    CONCATENATE ME->MY_URL ME->MY_SERVICE '/RFC_SYSTEM_INFO' INTO LOCATION.

    CONCATENATE

    '<html><head><title>JSON (NEW) handler notes</title></head><body>'

    '<h4>About this service...</h4>'
    'This is the ABAP implementation of a conversion program that'
    ' tranforms ABAP data into a <a href="http://www.json.org">JSON</a> representation.'
    '<p>'
    'It provides a user interface in the form of a ICF service that '
    'allows web invocation of ABAP function modules. It doesn''t matter if they are RFC enabled or not.'
    '<p>In this system this service has '
    'been assigned to ICF service <a href="' ME->MY_URL ME->MY_SERVICE '">' ME->MY_SERVICE '</a>.'
    '<p>'
    'In order to invoke a function module, just put its name in the PATH_INFO '
    'of the service URL, as is shown in the following examples.'

    '<p>Try the following link to do the default call in JSON format:<pre><a href="' LOCATION '?format=json">'
    LOCATION
    '?format=json</a></pre>'

    '<p>A simple syntax allows to get the output in different formats.<p>'

    'The following gets the output in <a href="http://yaml.org">YAML</a> format:'
    '<pre><a href="' LOCATION '?format=yaml">'
    LOCATION
    '?format=yaml</a></pre>'
    ''
    '<p>And this will get the output in a basic XML representation: <pre><a href="' LOCATION '?format=xml">'
    LOCATION
    '?format=xml</a></pre>'

    '<p>And, just for fun, getting it into Perl format could be handy: <pre><a href="' LOCATION '?format=perl">'
    LOCATION
    '?format=perl</a></pre>'

    '<p>Finnally, you can add a callback to get the JSON response enclosed in a javascript function call,'
    ' in order to allow a <a href="http://en.wikipedia.org/wiki/JSONP">JSONP</a> style response: '
    '<pre><a href="'
    LOCATION '?format=json&callback=callMe">'
    LOCATION '?format=json&callback=callMe</a></pre>'

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


    INTO TEXT RESPECTING BLANKS.


  ENDMETHOD.


  METHOD SERIALIZE_ID.
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

    TYPE-POOLS: ABAP.

    DATA:
      STAB        TYPE ABAP_TRANS_SRCBIND_TAB,
      SLIN        TYPE ABAP_TRANS_SRCBIND,
      OEXCP       TYPE REF TO CX_ROOT,
      ETEXT       TYPE STRING,
      ADATA       TYPE REF TO DATA,
      JSON_WRITER TYPE REF TO CL_SXML_STRING_WRITER.

    FIELD-SYMBOLS <PARM> TYPE ABAP_FUNC_PARMBIND.
*  field-symbols <excep> type abap_func_excpbind.


    LOOP AT PARAMTAB ASSIGNING <PARM>.
      IF SHOW_IMPP NE 'X'
            AND <PARM>-KIND EQ ABAP_FUNC_EXPORTING. "" va al revés, cuidado!!!
        CONTINUE.
      ENDIF.
      SLIN-NAME  = <PARM>-NAME.
      SLIN-VALUE = <PARM>-VALUE.
      APPEND SLIN TO STAB. CLEAR SLIN.
    ENDLOOP.

    IF EXCEPTAB IS NOT INITIAL.
      SLIN-NAME  = 'EXCEPTION'.
      GET REFERENCE OF EXCEPTAB INTO ADATA.
      SLIN-VALUE = ADATA.
      APPEND SLIN TO STAB. CLEAR SLIN.
    ENDIF.


    JSON_WRITER = CL_SXML_STRING_WRITER=>CREATE( TYPE = IF_SXML=>CO_XT_JSON ).

    TRY.

        CASE FORMAT.

          WHEN 'XML'.

            CALL TRANSFORMATION ID OPTIONS DATA_REFS = 'embedded'
                                           INITIAL_COMPONENTS = 'include'
                                   SOURCE (STAB)
                                   RESULT XML O_STRING.


          WHEN OTHERS.

            CALL TRANSFORMATION ID OPTIONS DATA_REFS = 'embedded'
                                           INITIAL_COMPONENTS = 'include'
                                   SOURCE (STAB)
                                   RESULT XML JSON_WRITER.

            O_STRING = CL_ABAP_CODEPAGE=>CONVERT_FROM( JSON_WRITER->GET_OUTPUT( ) ).
*  json_string = json_writer->get_output( ).

            IF JSONP IS NOT INITIAL.
              CONCATENATE JSONP '(' O_STRING ');' INTO O_STRING.
            ENDIF.

        ENDCASE.


      CATCH CX_ROOT INTO OEXCP.

        ETEXT = OEXCP->IF_MESSAGE~GET_TEXT( ).
        RAISE EXCEPTION TYPE ZCX_JSON
          EXPORTING
            MESSAGE = ETEXT.

    ENDTRY.


  ENDMETHOD.


  METHOD SERIALIZE_JSON.
* ABAP based JSON serializer for function modules (January 2013).
    TYPE-POOLS: ABAP.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

    DATA JSON_FRAGMENTS TYPE TABLE OF STRING.
    DATA REC_JSON_STRING TYPE STRING.
    DATA PARAMNAME TYPE STRING.
    DATA L_LINES TYPE I.
    DATA L_INDEX TYPE I.
    DATA UPCASE TYPE XFELD VALUE 'X'.
    FIELD-SYMBOLS <PARM> TYPE ABAP_FUNC_PARMBIND.
    FIELD-SYMBOLS <EXCEP> TYPE ABAP_FUNC_EXCPBIND.

    IF JSONP IS NOT INITIAL.
      APPEND JSONP TO JSON_FRAGMENTS.
      APPEND '(' TO JSON_FRAGMENTS.
    ENDIF.

    IF ACTION NE 'w'.
      REC_JSON_STRING = '{'.
      APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
      CLEAR REC_JSON_STRING.
    ENDIF.

    CLEAR L_INDEX.
    L_LINES = LINES( PARAMTAB ).

    LOOP AT PARAMTAB ASSIGNING <PARM>.
      IF SHOW_IMPP NE 'X'
            AND <PARM>-KIND EQ ABAP_FUNC_EXPORTING. "" va al revés, cuidado!!!
        SUBTRACT 1 FROM L_LINES.
        CONTINUE.
      ENDIF.
      ADD 1 TO L_INDEX.
      PARAMNAME = <PARM>-NAME.
      IF LOWERCASE EQ ABAP_TRUE.
        TRANSLATE PARAMNAME TO LOWER CASE.
        " paramname = to_lower( paramname ).
        UPCASE = SPACE.
      ENDIF.
      IF CAMELCASE EQ ABAP_TRUE.
        PARAMNAME = TO_MIXED( VAL = PARAMNAME  CASE = 'a').
      ENDIF.
      REC_JSON_STRING = ABAP2JSON( ABAP_DATA = <PARM>-VALUE  NAME = PARAMNAME  UPCASE = UPCASE CAMELCASE = CAMELCASE ).
      APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
      CLEAR REC_JSON_STRING.
      IF L_INDEX < L_LINES.
        APPEND ',' TO JSON_FRAGMENTS .
      ENDIF .
    ENDLOOP.

    IF EXCEPTAB IS NOT INITIAL.
      IF L_LINES GT 0.
        APPEND ',' TO JSON_FRAGMENTS.
      ENDIF.
      REC_JSON_STRING = ABAP2JSON( ABAP_DATA = EXCEPTAB UPCASE = 'X' NAME = 'EXCEPTION').
      APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
      CLEAR REC_JSON_STRING.
    ENDIF.

    IF ACTION NE 'w'.
      REC_JSON_STRING = '}'.
      APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
      CLEAR REC_JSON_STRING.
    ENDIF.

    IF JSONP IS NOT INITIAL.
      APPEND ');' TO JSON_FRAGMENTS.
    ENDIF.

    CONCATENATE LINES OF JSON_FRAGMENTS INTO O_STRING.

  ENDMETHOD.


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


  METHOD SERIALIZE_PERL.
* Just for fun, generate data in Perl Data::Dumper format.

    TYPE-POOLS: ABAP.

**Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

    DATA PERL_FRAGMENTS TYPE TABLE OF STRING.
    DATA REC_PERL_STRING TYPE STRING.
    DATA PARAMNAME TYPE STRING.
    DATA L_LINES TYPE I.
    DATA L_INDEX TYPE I.
    DATA UPCASE TYPE XFELD VALUE 'X'.
    DATA PERL_VAR TYPE STRING.
    FIELD-SYMBOLS <PARM> TYPE ABAP_FUNC_PARMBIND.
    FIELD-SYMBOLS <EXCEP> TYPE ABAP_FUNC_EXCPBIND.

    IF JSONP IS NOT INITIAL.
      PERL_VAR = JSONP.
    ELSE.
      PERL_VAR = FUNCNAME.
    ENDIF.
    CONCATENATE '$' PERL_VAR ' = {' INTO REC_PERL_STRING.
    APPEND REC_PERL_STRING TO PERL_FRAGMENTS.
    CLEAR REC_PERL_STRING.

    CLEAR L_INDEX.
    L_LINES = LINES( PARAMTAB ).

    LOOP AT PARAMTAB ASSIGNING <PARM>.
      IF SHOW_IMPP NE 'X'
            AND <PARM>-KIND EQ ABAP_FUNC_EXPORTING. "" va al revés, cuidado!!!
        SUBTRACT 1 FROM L_LINES.
        CONTINUE.
      ENDIF.
      ADD 1 TO L_INDEX.
      PARAMNAME = <PARM>-NAME.
      IF LOWERCASE EQ ABAP_TRUE.
        TRANSLATE PARAMNAME TO LOWER CASE.
        UPCASE = SPACE.
      ENDIF.
      REC_PERL_STRING = ABAP2PERL( ABAP_DATA = <PARM>-VALUE  NAME = PARAMNAME  UPCASE = UPCASE ).
      APPEND REC_PERL_STRING TO PERL_FRAGMENTS.
      CLEAR REC_PERL_STRING.
      IF L_INDEX < L_LINES.
        APPEND ',' TO PERL_FRAGMENTS .
      ENDIF .
    ENDLOOP.

    IF EXCEPTAB IS NOT INITIAL.
      IF L_LINES GT 0.
        APPEND ',' TO PERL_FRAGMENTS.
      ENDIF.
      REC_PERL_STRING = ABAP2PERL( ABAP_DATA = EXCEPTAB UPCASE = 'X' NAME = 'EXCEPTION').
      APPEND REC_PERL_STRING TO PERL_FRAGMENTS.
      CLEAR REC_PERL_STRING.
    ENDIF.

    REC_PERL_STRING = '};'.
    APPEND REC_PERL_STRING TO PERL_FRAGMENTS.
    CLEAR REC_PERL_STRING.

    CONCATENATE LINES OF PERL_FRAGMENTS INTO PERL_STRING.

  ENDMETHOD.


  METHOD SERIALIZE_XML.
* serialize function data into simple xml
*/ look at method serialize_id for a new way of doing xml.

    TYPE-POOLS: ABAP.

** remember function parameter types
***constants:
***  abap_func_exporting type abap_func_parmbind-kind value 10,
***  abap_func_importing type abap_func_parmbind-kind value 20,
***  abap_func_tables    type abap_func_parmbind-kind value 30,
***  abap_func_changing  type abap_func_parmbind-kind value 40.

    DATA REC_XML_STRING TYPE STRING.
    DATA XML_FRAGMENTS TYPE TABLE OF STRING.
    DATA L_FUNCNAME TYPE STRING.
    DATA PARAMNAME TYPE STRING.
    FIELD-SYMBOLS <PARM> TYPE ABAP_FUNC_PARMBIND.
    FIELD-SYMBOLS <EXCEP> TYPE ABAP_FUNC_EXCPBIND.
    DATA UPCASE TYPE XFELD VALUE 'x'.

    CONSTANTS:
       XML_HEAD TYPE STRING VALUE '<?xml version="1.0" encoding="utf-8"?>'.

    APPEND XML_HEAD TO XML_FRAGMENTS.

    L_FUNCNAME = FUNCNAME.
    IF LOWERCASE EQ ABAP_TRUE.
      TRANSLATE L_FUNCNAME TO LOWER CASE.
      UPCASE = SPACE.
    ENDIF.

    CONCATENATE '<' L_FUNCNAME '>' INTO REC_XML_STRING.
    APPEND REC_XML_STRING TO XML_FRAGMENTS.

    LOOP AT PARAMTAB ASSIGNING <PARM>.
      IF SHOW_IMPP NE 'x'
            AND <PARM>-KIND EQ ABAP_FUNC_EXPORTING. "" va al revés, cuidado!!!
        CONTINUE.
      ENDIF.
      PARAMNAME = <PARM>-NAME.
      IF LOWERCASE EQ ABAP_TRUE.
        TRANSLATE PARAMNAME TO LOWER CASE.
      ENDIF.
      REC_XML_STRING = ABAP2XML( NAME = PARAMNAME ABAP_DATA = <PARM>-VALUE UPCASE = UPCASE ).
      APPEND REC_XML_STRING TO XML_FRAGMENTS.
    ENDLOOP.

    IF EXCEPTAB IS NOT INITIAL.
      REC_XML_STRING = ABAP2XML( NAME = 'exception' ABAP_DATA = EXCEPTAB  UPCASE = UPCASE ).
      APPEND REC_XML_STRING TO XML_FRAGMENTS.
    ENDIF.

    CONCATENATE '</' L_FUNCNAME '>' INTO REC_XML_STRING.
    APPEND REC_XML_STRING TO XML_FRAGMENTS.

    CONCATENATE LINES OF XML_FRAGMENTS INTO O_STRING.

  ENDMETHOD.


  METHOD SERIALIZE_YAML.
* Now, go and represent function data in YAML (http://yaml.org)

    TYPE-POOLS: ABAP.
** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

    DATA YAML_FRAGMENTS TYPE TABLE OF STRING.
    DATA REC_YAML_STRING TYPE STRING.
    DATA REC_YAML_TABLE TYPE TABLE OF STRING.
    DATA PARAMNAME TYPE STRING.
    FIELD-SYMBOLS <PARM> TYPE ABAP_FUNC_PARMBIND.
    FIELD-SYMBOLS <EXCEP> TYPE ABAP_FUNC_EXCPBIND.
    DATA UPCASE TYPE XFELD VALUE 'X'.
    DATA YAML_HEAD TYPE STRING VALUE '--- #YAML:1.0'.

    CONCATENATE YAML_HEAD XNL INTO REC_YAML_STRING.
    APPEND REC_YAML_STRING TO YAML_FRAGMENTS.
    CLEAR REC_YAML_STRING.

    LOOP AT PARAMTAB ASSIGNING <PARM>.
      IF SHOW_IMPP NE 'X'
            AND <PARM>-KIND EQ ABAP_FUNC_EXPORTING. "" va al revés, cuidado!!!
        CONTINUE.
      ENDIF.
      PARAMNAME = <PARM>-NAME.
      IF LOWERCASE EQ ABAP_TRUE.
        TRANSLATE PARAMNAME TO LOWER CASE.
        UPCASE = SPACE.
      ENDIF.
      REC_YAML_STRING = ABAP2YAML( ABAP_DATA = <PARM>-VALUE  NAME = PARAMNAME UPCASE = UPCASE ).
      APPEND REC_YAML_STRING TO YAML_FRAGMENTS.
      CLEAR REC_YAML_STRING.
    ENDLOOP.

    IF EXCEPTAB IS NOT INITIAL.
      REC_YAML_STRING = ABAP2YAML( ABAP_DATA = EXCEPTAB NAME = 'EXCEPTION' UPCASE = 'X' ).
      APPEND REC_YAML_STRING TO YAML_FRAGMENTS.
      CLEAR REC_YAML_STRING.
    ENDIF.

*  append xnl to yaml_fragments.

    CONCATENATE LINES OF YAML_FRAGMENTS INTO YAML_STRING.

*  if jsonp is not initial.
*     concatenate jsonp '(' yaml_string ');' into yaml_string.
*  endif.

  ENDMETHOD.
ENDCLASS.
