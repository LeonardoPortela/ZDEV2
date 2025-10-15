class ZCL_FMCALL_BASE definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools JS .

*"* public components of class ZCL_FMCALL_BASE
*"* do not include other source files here!!!
  interfaces IF_HTTP_EXTENSION .

  constants AT_SERVICO_CANCEL_DOC_FATURA type STRING value 'cancelar_doc_fatura' ##NO_TEXT.
  constants XNL type ABAP_CHAR1 value %_NEWLINE ##NO_TEXT.
  constants XCRLF type ABAP_CR_LF value %_CR_LF ##NO_TEXT.
  data MY_SERVICE type STRING .
  data MY_URL type STRING .
  constants AT_SERVICO_INBOUND_ORD_CARREGA type STRING value 'carguero_viagem_criada' ##NO_TEXT.
  constants AT_SERVICO_INBOUND_ORD_CANCELA type STRING value 'carguero_viagem_cancelar' ##NO_TEXT.
  constants AT_SERVICO_VALIDA_XML_NFE type STRING value 'validarxmlnfe' ##NO_TEXT.
  constants AT_SERVICO_NFE_WRITER_GRC type STRING value 'grc_new_nfe' ##NO_TEXT.
  constants AT_SERVICO_CTB_NEW_DOC type STRING value 'ctb_new_doc' ##NO_TEXT.
  constants AT_SERVICO_CTB_CNS_DOC type STRING value 'ctb_cns_doc' ##NO_TEXT.
  constants AT_SERVICO_CTB_EST_DOC type STRING value 'ctb_est_doc' ##NO_TEXT.
  constants AT_SERVICO_CNS_PED_COMPRA type STRING value 'pedido_compra_cns' ##NO_TEXT.
  constants AT_SERVICO_PDF_XML_NFE_CTE type STRING value 'grc_arq_doc_eletronico' ##NO_TEXT.
  constants AT_SERVICO_NFE_WRITER_GRC_CAN type STRING value 'grc_new_nfe_cancel' ##NO_TEXT.
  constants AT_SERVICO_NFE_WRITER_GRC_EST type STRING value 'grc_new_nfe_estorno' ##NO_TEXT.
  constants AT_SERVICO_COTTON_DOUCLE_CHECK type STRING value 'doublecheck' ##NO_TEXT.
  constants AT_SERVICO_PROCESSA_COTTON type STRING value 'processafardo' ##NO_TEXT.
  constants AT_SERVICO_ESTORNO_COTTON type STRING value 'estornofardo' ##NO_TEXT.
  constants AT_SERVICO_NEW_ROMANEIO type STRING value 'sap_new_romaneio' ##NO_TEXT.
  constants AT_SERVICO_STATUS_JOB type STRING value 'statusjob' ##NO_TEXT.
  constants AT_SERVICO_OBS_NF_ZLES0050 type STRING value 'obs_nf_zles0050' ##NO_TEXT.
  constants AT_SERVICO_COMPENSACAO_SCP type STRING value 'compensacao_scp' ##NO_TEXT.
  constants AT_SERVICO_BRY_FINALIZADA type STRING value 'bry_set_coleta_finalizada' ##NO_TEXT.
  constants AT_SERVICO_COUPA_REQUISICAO type STRING value 'coupa_rem_approval_requisition' ##NO_TEXT.
  constants AT_SERVICO_COUPA_PEDIDO type STRING value 'coupa_rem_approval_order' ##NO_TEXT.
  constants AT_INTERFACE_APP_PRODUTOR_SO type STRING value 'solicitacao' ##NO_TEXT.
  constants AT_INTERFACE_APP_PRODUTOR_51 type STRING value 'ov' ##NO_TEXT.
  constants AT_INTERFACE_APP_PRODUTOR_RE type STRING value 'fatura' ##NO_TEXT.
  constants AT_ESTORNO_DOC_CONTABIL type STRING value 'estorno_doc_contabil' ##NO_TEXT.
  constants AT_INTERFACE_APP_PRODUTOR_TE type STRING value 'termo' ##NO_TEXT.
  constants AT_INFO_NF_SE type STRING value 'ZSD_INFO_NF_SE' ##NO_TEXT.
  constants AT_INTERFACE_APP_ITINERARIO type STRING value 'itinerario' ##NO_TEXT.
  constants AT_SERVICO_RECEITA_AGRIQ type STRING value 'servico_receita_agriq' ##NO_TEXT.
  constants AT_PLAN_PRODUCE_OC_CANCEL type STRING value '//plan_produce/cancelar' ##NO_TEXT.
  constants AT_PLAN_PRODUCE_OC_PESO_VAZIO type STRING value '//plan_produce/pesagemvazia' ##NO_TEXT.
  constants AT_PLAN_PRODUCE_OC_MOD type STRING value '//plan_produce/modordemcarreg' ##NO_TEXT.
  constants AT_ESTRAT_APROV type STRING value 'dados_estrategia' ##NO_TEXT.
  constants AT_SERVICO_RECEITA_AGRIQ_ASSIN type STRING value 'servico_receita_agriq_assinada' ##NO_TEXT.

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
  class-methods CONVERT_TXT_JSON_TO_STRING
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO) type STRING .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_FMCALL_BASE IMPLEMENTATION.


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
      C_QUOTE TYPE C VALUE '"',
      C_NLINE TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>NEWLINE.

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
      APPEND C_NLINE TO JSON_FRAGMENTS.
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
          APPEND C_NLINE TO JSON_FRAGMENTS.
        ENDIF.
      ENDLOOP.
      APPEND C_NLINE TO JSON_FRAGMENTS.
      APPEND ']' TO JSON_FRAGMENTS.
      APPEND C_NLINE TO JSON_FRAGMENTS.
* ']' JSON table closing bracket


***************************************************
*  Structures
***************************************************
    ELSE.
      IF L_COMPS IS NOT INITIAL.
* '{' JSON object opening curly brace
        APPEND '{' TO JSON_FRAGMENTS.
        APPEND C_NLINE TO JSON_FRAGMENTS.
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
            APPEND C_NLINE TO JSON_FRAGMENTS.
          ENDIF.
        ENDLOOP.

        APPEND C_NLINE TO JSON_FRAGMENTS.
        APPEND '}' TO JSON_FRAGMENTS.
        APPEND C_NLINE TO JSON_FRAGMENTS.
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


  METHOD IF_HTTP_EXTENSION~HANDLE_REQUEST.

    DATA: PATH_INFO    TYPE STRING,
          P_INFO_TAB   TYPE TABLE OF STRING,
          ACTION       TYPE STRING,
          "I_CONTENT_TYPE  TYPE STRING,
          I_CDATA      TYPE STRING,
          O_CDATA      TYPE STRING,
          O_NAME       TYPE STRING,
          O_AUX        TYPE STRING,
          O_DATA       TYPE XSTRING,
          EXCEPTHEADER TYPE STRING,
          ETEXT        TYPE STRING,
          ETEXT2       TYPE STRING,
          STR_ITEM     TYPE STRING,
          LC_FORMAT    TYPE STRING,
          HTTP_CODE    TYPE I,
          HTTP_STATUS  TYPE STRING,
          FUNCNAME     TYPE RS38L_FNAM,
          FUNCNAME2    TYPE STRING.

    DATA: LC_ERRO   TYPE STRING.

    DEFINE HTTP_ERROR.

      lc_erro = &3.
      IF lc_erro IS NOT INITIAL.
        LC_ERRO = ME->CONVERT_TO_UTF8( I_TEXTO = LC_ERRO ).
      ENDIF.
      server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
      http_code = &1.
      server->response->set_status( code = http_code  reason = &2 ).
      concatenate '{"ERROR_CODE":"' &1 '","ERROR_MESSAGE":"' lc_erro '"}' into etext.
      server->response->set_cdata( etext ).
      exit.

    END-OF-DEFINITION.

    DEFINE HTTP_SUCESSO.

      server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
      http_code = '200'.
      server->response->set_status( code = http_code  reason = 'Server' ).
      etext = '{"SUCESSO":"OK"}'.
      server->response->set_cdata( etext ).
      exit.

    END-OF-DEFINITION.

    DEFINE HTTP_METODO_NAO_ACEITO.

      server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
      http_code = '406'.
      server->response->set_status( code = http_code  reason = 'Client' ).
      etext = '{"ERROR_CODE":"406","ERROR_MESSAGE":"Mensagem não reconhecida, favor enviar requisição ao administrador do sistema de destino para validação"}'.
      server->response->set_cdata( etext ).
      exit.

    END-OF-DEFINITION.

* Get Server Info:

    SERVER->GET_LOCATION( IMPORTING HOST = DATA(LC_HOST) PORT = DATA(LC_PORT)  OUT_PROTOCOL = DATA(LC_PROTO) ).
    CONCATENATE LC_PROTO '://' LC_HOST ':' LC_PORT INTO ME->MY_URL.

    ME->MY_SERVICE = SERVER->REQUEST->GET_HEADER_FIELD( NAME = '~script_name' ).
    ACTION         = SERVER->REQUEST->GET_HEADER_FIELD( 'action' ).

    TRANSLATE ACTION TO UPPER CASE.

* Get function name from PATH_INFO
    PATH_INFO = SERVER->REQUEST->GET_HEADER_FIELD( NAME = '~path_info' ).
    SPLIT PATH_INFO AT '/' INTO TABLE P_INFO_TAB.
    READ TABLE P_INFO_TAB INDEX 2 INTO FUNCNAME.
    READ TABLE P_INFO_TAB INDEX 3 INTO FUNCNAME2.
    IF SY-SUBRC EQ 0.
      CONCATENATE '//' FUNCNAME '/' FUNCNAME2 INTO FUNCNAME.
      CONDENSE FUNCNAME.
    ENDIF.

    CHECK FUNCNAME IS NOT INITIAL.
    TRANSLATE FUNCNAME TO UPPER CASE.

* Verificar Acesso se o Recurso Exists
*  SELECT SINGLE * INTO @DATA(WA_ZWST0001)
*    FROM ZWST0001
*   WHERE CD_RECURSO EQ @FUNCNAME.
*
*  IF SY-SUBRC IS NOT INITIAL.
*    CONCATENATE 'Recurso' FUNCNAME 'não existe' INTO DATA(LC_TEXTO_01) SEPARATED BY SPACE.
*    HTTP_ERROR '404' 'Recurso' LC_TEXTO_01 .
*  ENDIF.
*
** Verificar Acesso se o usuário possui permissão
*  IF WA_ZWST0001-CK_NAO_VERIF_PERFIL EQ ABAP_TRUE.
*    AUTHORITY-CHECK OBJECT 'ZWS0001' ID 'CD_RECURSO' FIELD FUNCNAME.
*    IF SY-SUBRC IS NOT INITIAL.
*      CONCATENATE 'Recurso' WA_ZWST0001-DS_RECURSO 'nao disponivel para usuario/servico' INTO DATA(LC_TEXTO_02) SEPARATED BY SPACE.
*      HTTP_ERROR '404' 'Recurso' LC_TEXTO_02.
*    ENDIF.
*  ENDIF.

*  "Funçãoes Gerais
**  TRY .
**      CASE FUNCNAME.
**        WHEN ZIF_FMCALL_APP_MOBILE~ST_GET_QTD_TAREFAS_USUARIO.
**          ME->ZIF_FMCALL_APP_MOBILE~GET_QTD_TAREFAS_USUARIO( IMPORTING E_JSON = O_CDATA ).
**      ENDCASE.
**    CATCH ZCX_FMCALL_APP_MOBILE INTO EX_APP_MOBILE.
**      EX_APP_MOBILE->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
**      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**      HTTP_ERROR '405' 'Server' MTEXT.
**  ENDTRY.

    DATA(LC_URL) = ME->MY_URL && ME->MY_SERVICE && PATH_INFO.

    DATA: SERVER_BASE TYPE REF TO CL_HTTP_SERVER.

    SERVER_BASE = CAST #( SERVER ).

    LC_FORMAT = SERVER->REQUEST->GET_FORM_FIELD( 'format' ).
    IF LC_FORMAT IS INITIAL.
      LC_FORMAT = SERVER->REQUEST->GET_FORM_FIELD( '$format' ).
    ENDIF.

    DATA: I_INFO TYPE ZDE_INTEGRACAO_HTTP_CONFIG.
    DATA(OBJ_CONSIG) = NEW ZCL_HR_CONSIG( ).

    I_INFO-DS_BODY = SERVER->REQUEST->GET_CDATA( ).
    I_INFO-DS_CONTENT_TYPE = SERVER->REQUEST->GET_CONTENT_TYPE( ).
    I_INFO-DS_FORMATO = CONV #( LC_FORMAT ).
    I_INFO-DS_IP_ORIGEM = SERVER_BASE->C_CALLER_IP.
    I_INFO-DS_METODO = SERVER->REQUEST->GET_METHOD( ).
    I_INFO-DS_SERVER_PROTOCOLO = CONV #( SERVER_BASE->M_PROTOCOL_VERSION ).
    I_INFO-DS_URL = CONV #( LC_URL ).
    TRANSLATE FUNCNAME TO LOWER CASE.

    TRY.
        CASE FUNCNAME.
          WHEN ZCL_FMCALL_BASE=>AT_SERVICO_CTB_NEW_DOC.

            ZCL_INTEGRACAO_NEW_DOC_CTB=>ZIF_INTEGRACAO_NEW_DOC_CTB~GET_INSTANCE(
               )->SET_DS_DATA( I_INFO = I_INFO
               )->SET_SEND_MSG( IMPORTING E_MSG = O_CDATA
               ).

            SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/json; charset=UTF-8' ).

          WHEN ZCL_FMCALL_BASE=>AT_SERVICO_CTB_CNS_DOC.

            ZCL_INTEGRACAO_CNS_DOC_CTB=>ZIF_INTEGRACAO_CNS_DOC_CTB~GET_INSTANCE(
               )->SET_DS_DATA( I_INFO = I_INFO
               )->SET_SEND_MSG( IMPORTING E_MSG = O_CDATA
               ).

            SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/json; charset=UTF-8' ).

          WHEN ZCL_FMCALL_BASE=>AT_SERVICO_CTB_EST_DOC.

            ZCL_INTEGRACAO_EST_DOC_CTB=>ZIF_INTEGRACAO_EST_DOC_CTB~GET_INSTANCE(
               )->SET_DS_DATA( I_INFO = I_INFO
               )->SET_SEND_MSG( IMPORTING E_MSG = O_CDATA
               ).

            SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/json; charset=UTF-8' ).

          WHEN ZCL_FMCALL_BASE=>AT_SERVICO_NFE_WRITER_GRC.

            ZCL_INTEGRACAO_GRC_NEW_NFE=>ZIF_INTEGRACAO_GRC_NEW_NFE~GET_INSTANCE(
               )->SET_DS_DATA( I_INFO = I_INFO
               )->SET_SEND_MSG( IMPORTING E_MSG = O_CDATA
               ).
*-----------------alteração Bruna-----------------
             WHEN ZCL_FMCALL_BASE=>at_servico_new_romaneio.

            zcl_integracao_new_romaneio=>zif_integracao_new_romaneio~GET_INSTANCE(
               )->SET_DS_DATA( I_INFO = I_INFO
               )->SET_SEND_MSG( IMPORTING E_MSG = O_CDATA
               ).
*------------------fim alteração Bruna-----------------
          WHEN ZCL_FMCALL_BASE=>AT_SERVICO_NFE_WRITER_GRC_CAN.

            ZCL_INTEGRACAO_GRC_NEW_NFE_CA=>ZIF_INTEGRACAO_GRC_NEW_NFE_CA~GET_INSTANCE(
               )->SET_DS_DATA( I_INFO = I_INFO
               )->SET_SEND_MSG( IMPORTING E_MSG = O_CDATA
               ).

          WHEN ZCL_FMCALL_BASE=>AT_SERVICO_VALIDA_XML_NFE.

            ZCL_NFE_XML=>ZIF_NFE_XML~GET_INSTANCE(
              )->GET_WEB_SERVICE_VALIDAR(
              EXPORTING
                I_JSON       = I_INFO-DS_BODY
              IMPORTING
                E_JSON       = O_CDATA
              ).

          WHEN ZCL_FMCALL_BASE=>AT_SERVICO_INBOUND_ORD_CANCELA.

            "Assincrona
            ZCL_INTEGRACAO_VIAGEM_CANCELA=>ZIF_INTEGRACAO_VIAGEM_CANCELA~GET_INSTANCE(
              )->SET_DS_DATA( EXPORTING I_INFO = I_INFO
              )->SET_SEND_MSG( IMPORTING E_MSG = O_CDATA
              ).

            HTTP_SUCESSO.

          WHEN ZCL_FMCALL_BASE=>AT_SERVICO_INBOUND_ORD_CARREGA.

            "Assincrona
            ZCL_INTEGRACAO_ORD_CARREGA=>ZIF_INTEGRACAO_ORD_CARREGA~GET_INSTANCE(
              )->SET_DS_DATA( EXPORTING I_INFO = I_INFO
              )->SET_SEND_MSG( IMPORTING E_MSG = O_CDATA
              ).

            HTTP_SUCESSO.

          WHEN ZCL_FMCALL_BASE=>AT_SERVICO_CNS_PED_COMPRA.

            ZCL_INTEGRACAO_PEDIDO_CONSULTA=>ZIF_INTEGRACAO_PEDIDO_CONSULTA~GET_INSTANCE(
               )->SET_DS_DATA( I_INFO = I_INFO
               )->SET_SEND_MSG( IMPORTING E_MSG = O_CDATA
               ).

            SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/json; charset=UTF-8' ).

          WHEN ZCL_HR_CONSIG=>AT_SERVICO_LANCA_EMPRESTIMO.

            O_CDATA = OBJ_CONSIG->SET_DATA_EMPRESTIMO( I_INFO = I_INFO ).

          WHEN ZCL_HR_CONSIG=>AT_CONSIG_QUITA_PARCELA.

            O_CDATA = OBJ_CONSIG->SET_DATA_QUITA_PARCELA( I_INFO = I_INFO ).

          WHEN ZCL_HR_CONSIG=>AT_CONSIG_QUITA_TOTAL.

            O_CDATA = OBJ_CONSIG->SET_DATA_QUITA_TOTAL( I_INFO = I_INFO ).

          WHEN OTHERS.

            DATA(OBJ_INTERFACE) = ZCL_INTEGRACAO_INJECT=>GET_INSTANCE( ).
            OBJ_INTERFACE->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL          = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
            OBJ_INTERFACE->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP = I_INFO.
            OBJ_INTERFACE->ZIF_INTEGRACAO_INJECT~AT_ANONIMO           = ABAP_TRUE.

            ZCL_INTEGRACAO=>ZIF_INTEGRACAO~GET_INSTANCE(
              )->SET_MSG_INJECT( I_MSG = CAST #( OBJ_INTERFACE )
              )->SET_NEW_MSG(
              )->FREE(
              ).

            HTTP_METODO_NAO_ACEITO.

        ENDCASE.

      CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRACAO).

        TRY .
            OBJ_INTERFACE = ZCL_INTEGRACAO_INJECT=>GET_INSTANCE( ).
            OBJ_INTERFACE->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL          = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
            OBJ_INTERFACE->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP = I_INFO.
            OBJ_INTERFACE->ZIF_INTEGRACAO_INJECT~AT_ANONIMO           = ABAP_TRUE.

            ZCL_INTEGRACAO=>ZIF_INTEGRACAO~GET_INSTANCE(
              )->SET_MSG_INJECT( I_MSG = CAST #( OBJ_INTERFACE )
              )->SET_NEW_MSG(
              )->FREE(
              ).
          CATCH CX_ROOT.
        ENDTRY.

        DATA(MS_ERRO) = EX_INTEGRACAO->ZIF_ERROR~GET_MSG_ERRO( ).
        HTTP_ERROR '405' 'Server' MS_ERRO.

      CATCH ZCX_NFE_XML INTO DATA(EX_NFE_XML).

        MS_ERRO = EX_NFE_XML->ZIF_ERROR~GET_MSG_ERRO( ).
        HTTP_ERROR '405' 'Server' MS_ERRO.

      CATCH ZCX_ERROR INTO DATA(EX_ERROR).

        TRY .
            OBJ_INTERFACE = ZCL_INTEGRACAO_INJECT=>GET_INSTANCE( ).
            OBJ_INTERFACE->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL          = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
            OBJ_INTERFACE->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP = I_INFO.
            OBJ_INTERFACE->ZIF_INTEGRACAO_INJECT~AT_ANONIMO           = ABAP_TRUE.

            ZCL_INTEGRACAO=>ZIF_INTEGRACAO~GET_INSTANCE(
              )->SET_MSG_INJECT( I_MSG = CAST #( OBJ_INTERFACE )
              )->SET_NEW_MSG(
              )->FREE(
              ).
          CATCH CX_ROOT.
        ENDTRY.

        MS_ERRO = EX_ERROR->ZIF_ERROR~GET_MSG_ERRO( ).
        MS_ERRO =  ZCL_STRING=>CONVERT_TO_UTF8( ZCL_STRING=>TIRA_ACENTOS( MS_ERRO ) ).
        HTTP_ERROR '405' 'Server' MS_ERRO.

    ENDTRY.

    SERVER->RESPONSE->SET_COMPRESSION( ).
    SERVER->RESPONSE->SET_CDATA( DATA = O_CDATA ).

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

    REC_JSON_STRING = '{'.
    APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
    CLEAR REC_JSON_STRING.

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

    REC_JSON_STRING = '}'.
    APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
    CLEAR REC_JSON_STRING.

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
