class ZCL_STRING2 definition
  public
  final
  create public .

public section.

  constants AT_SEPARADOR_PADRAO type STRING value '*****' ##NO_TEXT.

  class-methods CONVERT_ACENTO_HTML
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO) type STRING .
  class-methods CONVERT_TXT_JSON_TO_STRING
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO) type STRING .
  class-methods CONVERT_TO_UTF8
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO) type STRING .
  class-methods CONVERT_TXT_STRING_TO_JSON
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO) type STRING .
  class-methods NVL
    importing
      !I_DATA_1 type STRING
      !I_DATA_2 type STRING
    exporting
      !E_DIFERENTES type CHAR01
    returning
      value(R_DATA) type STRING .
  class-methods CONCAT
    importing
      !S1 type STRING
      !S2 type STRING
      !SP type STRING optional
    returning
      value(S3) type STRING .
  class-methods LENGTH
    importing
      !TEXT type STRING
    returning
      value(R) type I .
  class-methods TIRA_ACENTOS
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO) type STRING
    raising
      ZCX_ERROR .
  class-methods SHOW_TEXTO
    importing
      !I_STRING type STRING
      !I_TITULO type STRING optional
      !I_HTML type CHAR01 default SPACE .
  class-methods UPPER
    importing
      !I_STR type STRING
    returning
      value(R_STR) type STRING .
  class-methods TO_FLOAT
    importing
      !I_STR type STRING
    returning
      value(R_VALOR) type F
    raising
      ZCX_ERROR .
  class-methods TRIM
    importing
      !I_STR type STRING
    returning
      value(R_STR) type STRING .
  class-methods TRIM_NO_SPACE
    importing
      !I_STR type STRING
    returning
      value(R_STR) type STRING .
  class-methods RPAD
    importing
      !I_STR type STRING
      !I_QTD type I
      !I_CHAR type CHAR01
    returning
      value(R_STR) type STRING .
  class-methods LPAD
    importing
      !I_STR type STRING
      !I_QTD type I
      !I_CHAR type CHAR01
    returning
      value(R_STR) type STRING .
  class-methods SPLIT
    importing
      !I_STR type STRING
      !I_QUEBRA type STRING default ZCL_STRING=>AT_SEPARADOR_PADRAO
    returning
      value(R_TABLE) type ZDE_STRING_T .
  class-methods HTML_STRING_TO_TABLE_LIST
    importing
      !I_TABLE_STRING type ZDE_STRING_T
      !I_TIPO_LISTA type ZDE_TIPO_LISTA_HTML default '01'
    returning
      value(R_STRING_HTML) type STRING .
  class-methods HTML_STRING_INTO_HTML
    importing
      !I_STR type STRING optional
      !I_TXT_HEADER_TITLE type STRING optional
      !I_TXT_HEADER_STYLE type STRING optional
      !I_TXT_SCRIPT type STRING optional
    returning
      value(R_STR_HTML) type STRING .
  class-methods REPLACE
    importing
      !I_STR type STRING
      !I_CHAR_OLD type CHAR01 optional
      !I_CHAR_NEW type CHAR01 optional
      !I_WITH_REGEX type CHAR01 optional
      !I_CHAR_REGEX type CHAR100 optional
    returning
      value(R_STR) type STRING .
  class-methods INITIALCAP
    importing
      !I_STR type STRING
    returning
      value(R_STR) type STRING .
  class-methods XML_TO_JSON
    importing
      !I_XML type STRING
      !I_ELEMENT_ARRAY type ZDE_ELEMENT_ARRAY_T optional
    returning
      value(R_JSON) type STRING .
  class-methods XSTRING_TO_STRING
    importing
      !I_XSTRING type XSTRING
    returning
      value(R_STRING) type STRING .
  class-methods STRING_TO_TABLE
    importing
      !I_STRING type STRING
      !I_LINE_LENGTH type I default 72
    exporting
      value(E_TABLE) type TABLE
    returning
      value(R_QTD_LINHAS) type I .
  class-methods BASE64_TO_STRING
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO_BASE64) type STRING .
  class-methods STRING_TO_BASE64
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO_BASE64) type STRING .
  class-methods XSTRING_TO_BASE64
    importing
      !I_XSTRING type XSTRING
    returning
      value(R_BASE) type STRING .
  class-methods REMOVE_SPEC_CHAR
    importing
      !I_STRING type STRING
    returning
      value(R_STRING) type STRING .
  class-methods XML_TO_TABLE
    importing
      !I_XML type STRING
      !I_ELEMENT_ARRAY type ZDE_ELEMENT_ARRAY_T optional
    exporting
      value(R_DATA) type DATA .
  class-methods FILE_PROPERTIES
    importing
      !I_FILE_PATH type STRING
    exporting
      !E_APPLICATION_TYPE type STRING
      !E_FILE_NAME type STRING .
  class-methods GUI_INDICATOR_URL
    importing
      !I_URL type ZDE_URL_ENVIO .
  class-methods XML_TO_JSON2
    importing
      !I_XML type STRING
      !I_ELEMENT_ARRAY type ZDE_ELEMENT_ARRAY_T optional
    returning
      value(R_JSON) type STRING .
protected section.
private section.

  methods REPLACE_STRANGE_CHARS
    importing
      !IV_STRING type STRING
    returning
      value(RV_STRING) type STRING .
ENDCLASS.



CLASS ZCL_STRING2 IMPLEMENTATION.


  METHOD BASE64_TO_STRING.

    DATA: LC_BUFFER TYPE  XSTRING.

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        B64DATA                  = I_TEXTO
      IMPORTING
        BINDATA                  = LC_BUFFER
      EXCEPTIONS
        SSF_KRN_ERROR            = 1
        SSF_KRN_NOOP             = 2
        SSF_KRN_NOMEMORY         = 3
        SSF_KRN_OPINV            = 4
        SSF_KRN_INPUT_DATA_ERROR = 5
        SSF_KRN_INVALID_PAR      = 6
        SSF_KRN_INVALID_PARLEN   = 7
        OTHERS                   = 8.

    R_TEXTO_BASE64 = ZCL_STRING=>XSTRING_TO_STRING( I_XSTRING = LC_BUFFER ).

  ENDMETHOD.


  METHOD CONCAT.

    IF SP IS NOT INITIAL.
      IF S1 IS NOT INITIAL AND S2 IS NOT INITIAL.
        CONCATENATE S1 S2 INTO S3 SEPARATED BY SP.
      ELSEIF S1 IS NOT INITIAL.
        S3 = S1.
      ELSEIF S2 IS NOT INITIAL.
        S3 = S2.
      ENDIF.
    ELSE.
      IF S1 IS NOT INITIAL AND S2 IS NOT INITIAL.
        CONCATENATE S1 S2 INTO S3.
      ELSEIF S1 IS NOT INITIAL.
        S3 = S1.
      ELSEIF S2 IS NOT INITIAL.
        S3 = S2.
      ENDIF.
    ENDIF.

    CONDENSE S3.

  ENDMETHOD.


  METHOD CONVERT_ACENTO_HTML.

    R_TEXTO = I_TEXTO.
    REPLACE ALL OCCURRENCES OF '&'  IN R_TEXTO WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF 'Á'  IN R_TEXTO WITH '&Aacute;'.
    REPLACE ALL OCCURRENCES OF 'É'  IN R_TEXTO WITH '&Eacute;'.
    REPLACE ALL OCCURRENCES OF 'Í'  IN R_TEXTO WITH '&Iacute;'.
    REPLACE ALL OCCURRENCES OF 'Ó'  IN R_TEXTO WITH '&Oacute;'.
    REPLACE ALL OCCURRENCES OF 'Ú'  IN R_TEXTO WITH '&Uacute;'.
    REPLACE ALL OCCURRENCES OF 'á'  IN R_TEXTO WITH '&aacute;'.
    REPLACE ALL OCCURRENCES OF 'é'  IN R_TEXTO WITH '&eacute;'.
    REPLACE ALL OCCURRENCES OF 'í'  IN R_TEXTO WITH '&iacute;'.
    REPLACE ALL OCCURRENCES OF 'ó'  IN R_TEXTO WITH '&oacute;'.
    REPLACE ALL OCCURRENCES OF 'ú'  IN R_TEXTO WITH '&uacute;'.
    REPLACE ALL OCCURRENCES OF 'Â'  IN R_TEXTO WITH '&Acirc;'.
    REPLACE ALL OCCURRENCES OF 'Ê'  IN R_TEXTO WITH '&Ecirc;'.
    REPLACE ALL OCCURRENCES OF 'Ô'  IN R_TEXTO WITH '&Ocirc;'.
    REPLACE ALL OCCURRENCES OF 'â'  IN R_TEXTO WITH '&acirc;'.
    REPLACE ALL OCCURRENCES OF 'ê'  IN R_TEXTO WITH '&ecirc;'.
    REPLACE ALL OCCURRENCES OF 'ô'  IN R_TEXTO WITH '&ocirc;'.
    REPLACE ALL OCCURRENCES OF 'À'  IN R_TEXTO WITH '&Agrave;'.
    REPLACE ALL OCCURRENCES OF 'à'  IN R_TEXTO WITH '&agrave;'.
    REPLACE ALL OCCURRENCES OF 'Ü'  IN R_TEXTO WITH '&Uuml;'.
    REPLACE ALL OCCURRENCES OF 'ü'  IN R_TEXTO WITH '&uuml;'.
    REPLACE ALL OCCURRENCES OF 'Ç'  IN R_TEXTO WITH '&Ccedil;'.
    REPLACE ALL OCCURRENCES OF 'ç'  IN R_TEXTO WITH '&ccedil;'.
    REPLACE ALL OCCURRENCES OF 'Ã'  IN R_TEXTO WITH '&Atilde;'.
    REPLACE ALL OCCURRENCES OF 'Õ'  IN R_TEXTO WITH '&Otilde;'.
    REPLACE ALL OCCURRENCES OF 'ã'  IN R_TEXTO WITH '&atilde;'.
    REPLACE ALL OCCURRENCES OF 'õ'  IN R_TEXTO WITH '&otilde;'.
    REPLACE ALL OCCURRENCES OF 'Ñ'  IN R_TEXTO WITH '&Ntilde;'.
    REPLACE ALL OCCURRENCES OF 'ñ'  IN R_TEXTO WITH '&ntilde;'.
    REPLACE ALL OCCURRENCES OF '<'  IN R_TEXTO WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>'  IN R_TEXTO WITH '&gt;'.
    REPLACE ALL OCCURRENCES OF '"'  IN R_TEXTO WITH '&quot;'.

  ENDMETHOD.


  METHOD CONVERT_TO_UTF8.

    DATA: LC_LENGTH TYPE I.
    DATA: LC_BUFFER TYPE XSTRING.
    DATA: LT_TEXTO  TYPE TABLE OF CHAR80.
    DATA: TAMANHO   TYPE I.

    CLEAR R_TEXTO.

    CHECK I_TEXTO IS NOT INITIAL.

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


  METHOD CONVERT_TXT_STRING_TO_JSON.

    DATA: LC_LENGTH TYPE I.
    DATA: LC_BUFFER TYPE XSTRING.
    DATA: LT_TEXTO  TYPE TABLE OF CHAR80.
    DATA: TAMANHO   TYPE I.

    R_TEXTO = I_TEXTO.

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
    REPLACE ALL OCCURRENCES OF '''' IN R_TEXTO WITH '\u0027'.

    REPLACE ALL OCCURRENCES OF '!'  IN R_TEXTO WITH '\u0021' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '#'  IN R_TEXTO WITH '\u0023' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '$'  IN R_TEXTO WITH '\u0024' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%'  IN R_TEXTO WITH '\u0025' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '&'  IN R_TEXTO WITH '\u0026' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '('  IN R_TEXTO WITH '\u0028' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF ')'  IN R_TEXTO WITH '\u0029' IGNORING CASE.

    REPLACE ALL OCCURRENCES OF '*'  IN R_TEXTO WITH '\u002a' IGNORING CASE.

    REPLACE ALL OCCURRENCES OF '+' IN R_TEXTO WITH '\u002b' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF ',' IN R_TEXTO WITH '\u002c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '-' IN R_TEXTO WITH '\u002d' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '.' IN R_TEXTO WITH '\u002e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '/' IN R_TEXTO WITH '\u002f' IGNORING CASE.

    REPLACE ALL OCCURRENCES OF '<' IN R_TEXTO WITH '\u003c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '=' IN R_TEXTO WITH '\u003d' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '>' IN R_TEXTO WITH '\u003e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '?' IN R_TEXTO WITH '\u003f' IGNORING CASE.

    REPLACE ALL OCCURRENCES OF '@' IN R_TEXTO WITH '\u0040' IGNORING CASE.

    REPLACE ALL OCCURRENCES OF 'Á' IN R_TEXTO WITH '\u00c1' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'À' IN R_TEXTO WITH '\u00c0' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Â' IN R_TEXTO WITH '\u00c2' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ã' IN R_TEXTO WITH '\u00c3' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ä' IN R_TEXTO WITH '\u00c4' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'É' IN R_TEXTO WITH '\u00c9' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'È' IN R_TEXTO WITH '\u00c8' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ê' IN R_TEXTO WITH '\u00ca' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ë' IN R_TEXTO WITH '\u00cb' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Í' IN R_TEXTO WITH '\u00cd' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ì' IN R_TEXTO WITH '\u00cc' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Î' IN R_TEXTO WITH '\u00ce' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ï' IN R_TEXTO WITH '\u00cf' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ç' IN R_TEXTO WITH '\u00c7' IGNORING CASE.

    REPLACE ALL OCCURRENCES OF 'Ó' IN R_TEXTO WITH '\u00d3' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ò' IN R_TEXTO WITH '\u00d2' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ô' IN R_TEXTO WITH '\u00d4' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Õ' IN R_TEXTO WITH '\u00d5' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ö' IN R_TEXTO WITH '\u00d6' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ú' IN R_TEXTO WITH '\u00da' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ù' IN R_TEXTO WITH '\u00d9' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Û' IN R_TEXTO WITH '\u00db' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'Ñ' IN R_TEXTO WITH '\u00d1' IGNORING CASE.

    REPLACE ALL OCCURRENCES OF 'á' IN R_TEXTO WITH '\u00e1' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'à' IN R_TEXTO WITH '\u00e0' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'â' IN R_TEXTO WITH '\u00e2' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ã' IN R_TEXTO WITH '\u00e3' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ä' IN R_TEXTO WITH '\u00e4' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'é' IN R_TEXTO WITH '\u00e9' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'è' IN R_TEXTO WITH '\u00e8' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ê' IN R_TEXTO WITH '\u00ea' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ê' IN R_TEXTO WITH '\u00ea' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'í' IN R_TEXTO WITH '\u00ed' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ì' IN R_TEXTO WITH '\u00ec' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'î' IN R_TEXTO WITH '\u00ee' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ï' IN R_TEXTO WITH '\u00ef' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ç' IN R_TEXTO WITH '\u00e7' IGNORING CASE.

    REPLACE ALL OCCURRENCES OF 'ó' IN R_TEXTO WITH '\u00f3' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ò' IN R_TEXTO WITH '\u00f2' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ô' IN R_TEXTO WITH '\u00f4' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'õ' IN R_TEXTO WITH '\u00f5' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ö' IN R_TEXTO WITH '\u00f6' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ú' IN R_TEXTO WITH '\u00fa' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ù' IN R_TEXTO WITH '\u00f9' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'û' IN R_TEXTO WITH '\u00fb' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ü' IN R_TEXTO WITH '\u00fc' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF 'ñ' IN R_TEXTO WITH '\u00f1' IGNORING CASE.

  ENDMETHOD.


  METHOD file_properties.

    DATA lv_count TYPE i.
    DATA lv_appli TYPE c LENGTH 5.
    DATA lv_file_path TYPE msgxx.
    DATA lv_reverse TYPE msgxx.
    DATA lv_file_name TYPE msgxx.
    DATA lv_appli_m TYPE msgxx.
    DATA lw_match TYPE match_result.

    lv_file_path = i_file_path.

    CALL FUNCTION 'STRING_REVERSE'
      EXPORTING
        string  = lv_file_path
        lang    = sy-langu
      IMPORTING
        rstring = lv_reverse.

    FIND FIRST OCCURRENCE OF '/' IN lv_reverse RESULTS lw_match.

    CHECK lw_match-offset IS NOT INITIAL.

    "SUBTRACT 1 FROM lw_match-offset.

    DO lw_match-offset TIMES.
      e_file_name = e_file_name && lv_reverse+lv_count(1).

      IF lv_appli NA '.'.
        lv_appli = lv_appli && lv_reverse+lv_count(1).
      ENDIF.

      ADD 1 TO lv_count.

    ENDDO.

    lv_file_name = e_file_name.

    REPLACE '.' IN lv_appli WITH space.

    CALL FUNCTION 'STRING_REVERSE'
      EXPORTING
        string  = lv_file_name
        lang    = sy-langu
      IMPORTING
        rstring = lv_file_name.

    e_file_name = lv_file_name.

    CALL FUNCTION 'STRING_REVERSE'
      EXPORTING
        string  = lv_appli
        lang    = sy-langu
      IMPORTING
        rstring = lv_appli_m.

    e_application_type = lv_appli_m.

    TRANSLATE lv_appli_m TO UPPER CASE.

    CASE lv_appli_m.
      WHEN 'PDF'.
        e_application_type = 'application/pdf'.
      WHEN 'CSV'.
        e_application_type = 'text/csv'.
      WHEN 'DOC'.
        e_application_type = 'application/msword'.
      WHEN 'DOCX'.
        e_application_type = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'.
    ENDCASE.

  ENDMETHOD.


  METHOD gui_indicator_url.

    DATA lv_len TYPE i.

    DATA lv_text TYPE string.

    DATA lw_match TYPE match_result.

    CHECK i_url IS NOT INITIAL.

    FIND '.com' IN i_url RESULTS lw_match.

    lv_len = strlen( i_url ) - lw_match-offset.
    DATA(lv_value) = substring( val = i_url off = lw_match-offset len = lv_len ).

    REPLACE FIRST OCCURRENCE OF '/' IN lv_value WITH space.
    REPLACE FIRST OCCURRENCE OF '.com' IN lv_value WITH space.

    CONDENSE lv_value NO-GAPS.

    FIND '?' IN lv_value RESULTS lw_match.

    IF sy-subrc EQ 0.

      lv_value = substring( val = lv_value off = 0 len = lw_match-offset ).

    ENDIF.

    CHECK lv_value IS NOT INITIAL.

    lv_text = `Executando ` && lv_value.

    IF sy-batch IS INITIAL.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 50
          text       = lv_text.

    ELSE.

      lv_len = strlen( lv_text ).

      IF lv_len > 50.

        sy-msgv1 = lv_text(50).
        sy-msgv2 = lv_text+50.

        MESSAGE s016(ds) WITH sy-msgv1 sy-msgv2 space space.

      ELSE.

        MESSAGE s016(ds) WITH lv_text space space space.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD HTML_STRING_INTO_HTML.

    R_STR_HTML = '<!DOCTYPE html>' && '<html>' &&
                 '<head>' &&
                 '<title>' && I_TXT_HEADER_TITLE && '</title>' &&
                 I_TXT_HEADER_STYLE &&
                 '</head>' &&
                 I_TXT_SCRIPT &&
                 '<body>'  && I_STR && '</body>' && '</html>'.

  ENDMETHOD.


  METHOD HTML_STRING_TO_TABLE_LIST.

    DATA: LC_LISTA TYPE STRING.

    LC_LISTA = ''.
    LOOP AT I_TABLE_STRING INTO DATA(LC_STRING).
      LC_LISTA = LC_LISTA && '<li>' && LC_STRING && '</li>'.
    ENDLOOP.

    CASE I_TIPO_LISTA.
      WHEN '01'.
        "01	Unordered HTML List
        R_STRING_HTML = '<ul>'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ul>'.
      WHEN '02'.
        "02	Unordered HTML List - Choose List Item Marker - Disc
        R_STRING_HTML = '<ul style="list-style-type:disc;">'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ul>'.
      WHEN '03'.
        "03	Unordered HTML List - Choose List Item Marker - Circle
        R_STRING_HTML = '<ul style="list-style-type:circle;">'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ul>'.
      WHEN '04'.
        "04	Unordered HTML List - Choose List Item Marker - Square
        R_STRING_HTML = '<ul style="list-style-type:square;">'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ul>'.
      WHEN '05'.
        "05	Unordered HTML List - Choose List Item Marker - None
        R_STRING_HTML = '<ul style="list-style-type:none;">'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ul>'.
      WHEN '06'.
        "06	Ordered HTML List
        R_STRING_HTML = '<ol>'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ol>'.
      WHEN '07'.
        "07	Ordered HTML List - The Type Attribute (1)
        R_STRING_HTML = '<ol type="1">'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ol>'.
      WHEN '08'.
        "08	Ordered HTML List - The Type Attribute (A)
        R_STRING_HTML = '<ol type="A">'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ol>'.
      WHEN '09'.
        "09	Ordered HTML List - The Type Attribute (a)
        R_STRING_HTML = '<ol type="a">'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ol>'.
      WHEN '10'.
        "10	Ordered HTML List - The Type Attribute (I)
        R_STRING_HTML = '<ol type="I">'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ol>'.
      WHEN '11'.
        "11	Ordered HTML List - The Type Attribute (i)
        R_STRING_HTML = '<ol type="i">'.
        R_STRING_HTML = R_STRING_HTML && LC_LISTA.
        R_STRING_HTML = R_STRING_HTML && '</ol>'.
    ENDCASE.

  ENDMETHOD.


  METHOD initialcap.

    DATA: i_str_c TYPE c LENGTH 200.

    i_str_c = i_str.

*---> Migração S4 - 19.07.2023 - MIGNOW
*	CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
    CALL FUNCTION 'FI_CONVERT_FIRSTCHARS_TOUPPER'
*<--- Migração S4 - 19.07.2023 - MIGNOW
      EXPORTING
        input_string  = i_str_c
*       SEPARATORS    = ' -.,;:'
      IMPORTING
        output_string = i_str_c.

    r_str = i_str_c.

  ENDMETHOD.


  METHOD LENGTH.

    R = STRLEN( TEXT ).

  ENDMETHOD.


  METHOD LPAD.

    DATA(LC_DO) = I_QTD - ZCL_STRING=>LENGTH( TEXT = I_STR ).

    R_STR = I_STR.

    CHECK LC_DO GT 0.

    DO LC_DO TIMES.
      CONCATENATE I_CHAR R_STR INTO R_STR.
    ENDDO.

  ENDMETHOD.


  METHOD NVL.
    R_DATA       = COND STRING( WHEN I_DATA_1 IS NOT INITIAL THEN I_DATA_1 ELSE I_DATA_2 ).
    CHECK I_DATA_1 IS NOT INITIAL.
    E_DIFERENTES = COND STRING( WHEN I_DATA_1 EQ I_DATA_2 THEN ABAP_FALSE ELSE ABAP_TRUE ).
  ENDMETHOD.


  METHOD remove_spec_char.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = i_string
      IMPORTING
        outtext           = r_string
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      r_string = i_string.
    ENDIF.

  ENDMETHOD.


  METHOD REPLACE.

    R_STR = I_STR.
    IF I_CHAR_NEW IS INITIAL.
      IF I_WITH_REGEX EQ ABAP_TRUE.
        REPLACE ALL OCCURRENCES OF REGEX I_CHAR_REGEX IN R_STR WITH ''.
      ELSE.
        REPLACE ALL OCCURRENCES OF I_CHAR_OLD IN R_STR WITH ''.
      ENDIF.
    ELSE.
      IF I_WITH_REGEX EQ ABAP_TRUE.
        REPLACE ALL OCCURRENCES OF REGEX I_CHAR_REGEX IN R_STR WITH I_CHAR_NEW.
      ELSE.
        REPLACE ALL OCCURRENCES OF I_CHAR_OLD IN R_STR WITH I_CHAR_NEW.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD replace_strange_chars.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = iv_string
      IMPORTING
        outtext           = rv_string
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      rv_string = iv_string.
      EXIT.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '.' IN rv_string WITH ` `.

    CONDENSE rv_string.

  ENDMETHOD.


  METHOD RPAD.

    DATA(LC_DO) = I_QTD - ZCL_STRING=>LENGTH( TEXT = I_STR ).

    R_STR = I_STR.

    CHECK LC_DO GT 0.

    DO LC_DO TIMES.
      CONCATENATE R_STR I_CHAR INTO R_STR.
    ENDDO.

  ENDMETHOD.


  METHOD SHOW_TEXTO.

    CALL FUNCTION 'Z_STRING_MOSTRA_TEXTO'
      EXPORTING
        I_STRING = I_STRING
        I_TITULO = I_TITULO
        I_HTML   = I_HTML.

  ENDMETHOD.


  METHOD SPLIT.

    SPLIT I_STR AT I_QUEBRA INTO TABLE R_TABLE.

  ENDMETHOD.


  METHOD STRING_TO_BASE64.

    DATA: LC_BUFFER TYPE  XSTRING.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        TEXT   = I_TEXTO
      IMPORTING
        BUFFER = LC_BUFFER
      EXCEPTIONS
        FAILED = 1
        OTHERS = 2.

    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        BINDATA                  = LC_BUFFER
      IMPORTING
        B64DATA                  = R_TEXTO_BASE64
      EXCEPTIONS
        SSF_KRN_ERROR            = 1
        SSF_KRN_NOOP             = 2
        SSF_KRN_NOMEMORY         = 3
        SSF_KRN_OPINV            = 4
        SSF_KRN_INPUT_DATA_ERROR = 5
        SSF_KRN_INVALID_PAR      = 6
        SSF_KRN_INVALID_PARLEN   = 7
        OTHERS                   = 8.

  ENDMETHOD.


  METHOD STRING_TO_TABLE.

    R_QTD_LINHAS = 0.

    CLEAR: E_TABLE, E_TABLE[].

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        I_STRING         = I_STRING
        I_TABLINE_LENGTH = I_LINE_LENGTH
*       I_UNICODE        =
      TABLES
        ET_TABLE         = E_TABLE.

    DESCRIBE TABLE E_TABLE LINES R_QTD_LINHAS.

  ENDMETHOD.


  METHOD TIRA_ACENTOS.

    TYPES BEGIN OF TY_LINHA.
    TYPES LINHA TYPE C LENGTH 4999.
    TYPES END OF TY_LINHA.

    DATA: IT_LINHAS TYPE TABLE OF TY_LINHA.

    CLEAR R_TEXTO.

    CHECK I_TEXTO IS NOT INITIAL.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        I_STRING         = I_TEXTO
        I_TABLINE_LENGTH = 4999
      TABLES
        ET_TABLE         = IT_LINHAS.

    LOOP AT IT_LINHAS ASSIGNING FIELD-SYMBOL(<FS_LINHA>).

      DATA(LC_LINHA) = <FS_LINHA>-LINHA.

      LC_LINHA =
      ZCL_STRING=>REPLACE(
        EXPORTING
          I_STR        = CONV #( LC_LINHA )
          I_CHAR_NEW   = '.'
          I_WITH_REGEX = 'X'
          I_CHAR_REGEX = CONV #('[^0-9a-zA-ZÁÉÍÓÚÀÈÌÒÙÂÊÎÔÛÃÕÇáéíóúàèìòùâêîôûãõç;\/ \-+:,.?(){}*\[\]\\#]')  ).

      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          INTEXT            = LC_LINHA
        IMPORTING
          OUTTEXT           = <FS_LINHA>-LINHA
        EXCEPTIONS
          INVALID_CODEPAGE  = 1
          CODEPAGE_MISMATCH = 2
          INTERNAL_ERROR    = 3
          CANNOT_CONVERT    = 4
          FIELDS_NOT_TYPE_C = 5
          OTHERS            = 6.

      IF SY-SUBRC IS NOT INITIAL.

        IF SY-MSGID IS INITIAL.
          <FS_LINHA>-LINHA = LC_LINHA.
        ELSE.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
        ENDIF.

      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'CONVERT_TABLE_TO_STRING'
      EXPORTING
        I_TABLINE_LENGTH = 4999
      IMPORTING
        E_STRING         = R_TEXTO
      TABLES
        IT_TABLE         = IT_LINHAS.

  ENDMETHOD.


  METHOD TO_FLOAT.

    DATA(LC_STR) = I_STR.
    REPLACE ALL OCCURRENCES OF ',' IN LC_STR WITH ''.

    TRY .
        MOVE LC_STR TO R_VALOR.
      CATCH CX_ROOT INTO DATA(ERRO).
        ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = ERRO->GET_LONGTEXT( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD TRIM.

    R_STR = I_STR.
    CONDENSE R_STR.

  ENDMETHOD.


  METHOD TRIM_NO_SPACE.

    R_STR = I_STR.
    CONDENSE R_STR NO-GAPS.

  ENDMETHOD.


  METHOD UPPER.
    R_STR = I_STR.
    TRANSLATE R_STR TO UPPER CASE.
  ENDMETHOD.


  METHOD xml_to_json.

    DATA: v_xml TYPE string.

    TYPES: BEGIN OF node,
             node_type TYPE if_sxml_node=>node_type,
             name      TYPE string,
             value     TYPE string,
             array     TYPE c LENGTH 1,
           END OF node.

    DATA: nodes       TYPE TABLE OF node WITH EMPTY KEY.


    DO.

      v_xml     = replace( i_str      = i_xml
                           i_char_old = |\n|
                           i_char_new = '' ).

      DATA(xml) = cl_abap_codepage=>convert_to( v_xml  ).

      DATA(out) = cl_demo_output=>new( ).

      "Parsing XML into an internal table
      DATA(reader) = cl_sxml_string_reader=>create( xml ).

      CLEAR nodes.
      TRY.
          DO.
            reader->next_node( ).

            IF reader->node_type = if_sxml_node=>co_nt_final.
              EXIT.
            ENDIF.

            DATA(lv_name) = reader->name.

            REPLACE ALL OCCURRENCES OF '-' IN lv_name WITH ''.

            APPEND VALUE #(
              node_type  = reader->node_type
              name       = reader->prefix &&
                           COND string(
                                  WHEN reader->prefix IS NOT INITIAL
                                    THEN ':' ) && lv_name
              value      = reader->value ) TO nodes.

            IF reader->node_type = if_sxml_node=>co_nt_element_open.
              DO.
                reader->next_attribute( ).
                IF reader->node_type <> if_sxml_node=>co_nt_attribute.
                  EXIT.
                ENDIF.

*                APPEND VALUE #(
*                  node_type = if_sxml_node=>co_nt_initial
*                  name       = reader->prefix &&
*                               COND string(
*                                 WHEN reader->prefix IS NOT INITIAL
*                                   THEN ':' ) && reader->name
*                  value      = reader->value ) TO nodes.

              ENDDO.

            ENDIF.

          ENDDO.

        CATCH cx_sxml_parse_error INTO DATA(parse_error).
          out->write_text( parse_error->get_text( ) ).
      ENDTRY.

      "Tratamento para Elementos Simples do XML
      LOOP AT nodes INTO DATA(wl_node).
        DATA(tabix) = sy-tabix.

        IF wl_node-node_type EQ if_sxml_node=>co_nt_element_open.

          DATA(tabix_value) = tabix + 1.

          READ TABLE nodes INTO DATA(wl_node_aux) INDEX tabix_value.

          IF ( sy-subrc EQ 0 ) AND ( wl_node_aux-node_type EQ if_sxml_node=>co_nt_value ).

            DATA(tabix_close) = tabix + 2.

            READ TABLE nodes INTO wl_node_aux INDEX tabix_close.
            IF ( sy-subrc EQ 0 ) AND ( wl_node_aux-node_type EQ if_sxml_node=>co_nt_element_close ).
              DELETE nodes INDEX tabix.
              DELETE nodes INDEX tabix_close - 1 .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      "Determine the array limits in the internal table
      LOOP AT nodes ASSIGNING FIELD-SYMBOL(<node_open>) WHERE node_type = if_sxml_node=>co_nt_element_open
                                                          AND array IS INITIAL.

        DATA(idx_open) = sy-tabix.

        LOOP AT nodes ASSIGNING FIELD-SYMBOL(<node_close>) FROM idx_open  + 1 WHERE node_type = if_sxml_node=>co_nt_element_close
                                                                                AND name = <node_open>-name.

          DATA(idx_close) = sy-tabix.

          IF idx_close < lines( nodes ).

            ASSIGN nodes[ idx_close + 1 ] TO FIELD-SYMBOL(<node>).

            READ TABLE i_element_array INTO DATA(wl_element_array) WITH KEY table_line  = <node_open>-name.
            IF sy-subrc EQ 0.
              <node_open>-array = 'O'.
            ENDIF.

            IF <node>-node_type = if_sxml_node=>co_nt_element_open AND <node>-name = <node_open>-name.

              <node_open>-array = 'O'.
              <node>-array = '_'.

            ELSEIF ( <node>-node_type = if_sxml_node=>co_nt_element_open AND <node>-name <> <node_open>-name ) OR
                   ( <node>-node_type = if_sxml_node=>co_nt_element_close ) OR
                   ( <node>-node_type = if_sxml_node=>co_nt_value    AND  <node>-name <> <node_open>-name ).

              <node_close>-array = COND #( WHEN <node_open>-array = 'O' THEN 'C' ).
              EXIT.
            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDLOOP.

      "Render the internal table to JSON-XML
      DATA(writer) = CAST if_sxml_writer(
       cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
      "create( type = if_sxml=>co_xt_xml10 ) ).

      TRY.
          writer->open_element( name = 'object' ).

          LOOP AT nodes ASSIGNING <node>.

            TRY.

                CASE <node>-node_type.

                  WHEN if_sxml_node=>co_nt_element_open.

                    IF <node>-array IS INITIAL.

                      writer->open_element( name = 'object' ).
                      writer->write_attribute( name = 'name' value = <node>-name ).

                    ELSEIF <node>-array = 'O'.

                      writer->open_element( name = 'array' ).

                      writer->write_attribute( name = 'name'

                                               value = <node>-name ).

                      writer->open_element( name = 'object' ).

                    ELSEIF <node>-array = '_'.

                      writer->open_element( name = 'object' ).

                    ENDIF.

                  WHEN if_sxml_node=>co_nt_element_close.

                    IF <node>-array <> 'C'.

                      writer->close_element( ).

                    ELSE.

                      writer->close_element( ).

                      writer->close_element( ).

                    ENDIF.

                  WHEN if_sxml_node=>co_nt_initial.

*                  writer->open_element( name = 'str' ).
*
*                  writer->write_attribute( name = 'name'
*
*                                           value = 'a_' && <node>-name ).
*
*                  writer->write_value( <node>-value ).
*
*                  writer->close_element( ).

                  WHEN if_sxml_node=>co_nt_value.

                    writer->open_element( name = 'str' ).

                    writer->write_attribute( name = 'name'
                                            value = <node>-name ).
                    " value = 'e_' && <node>-name ).

                    writer->write_value( <node>-value ).

                    writer->close_element( ).

                  WHEN OTHERS.

                    out->display( 'A node type is not yet supported' ).

                    RETURN.

                ENDCASE.

              CATCH cx_sxml_error INTO DATA(exc).

                out->write( exc->get_text( ) ).

            ENDTRY.

          ENDLOOP.

          writer->close_element( ).

          DATA(json) = CAST cl_sxml_string_writer( writer )->get_output( ).

          IF writer->if_sxml~type = if_sxml=>co_xt_json.

            out->write_json( json ).

          ELSEIF writer->if_sxml~type = if_sxml=>co_xt_xml10.

            out->write_xml( json ).

          ENDIF.

        CATCH cx_sxml_error INTO DATA(exc2).

          out->write( exc->get_text( ) ).

      ENDTRY.


      r_json = xstring_to_string( i_xstring =  json ).

      "out->display( ).
      "data(R_JSON) = out->get( ).

      EXIT.

    ENDDO.




  ENDMETHOD.


  METHOD xml_to_json2.

    DATA: v_xml TYPE string.

    TYPES: BEGIN OF node,
             node_type TYPE if_sxml_node=>node_type,
             name      TYPE string,
             value     TYPE string,
             array     TYPE c LENGTH 1,
           END OF node.

    DATA: nodes       TYPE TABLE OF node WITH EMPTY KEY.


    DO.

      v_xml     = replace( i_str      = i_xml
                           i_char_old = |\n|
                           i_char_new = '' ).

      DATA(xml) = cl_abap_codepage=>convert_to( v_xml  ).

      DATA(out) = cl_demo_output=>new( ).

      "Parsing XML into an internal table
      DATA(reader) = cl_sxml_string_reader=>create( xml ).

      CLEAR nodes.
      TRY.
          DO.
            reader->next_node( ).

            IF reader->node_type = if_sxml_node=>co_nt_final.
              EXIT.
            ENDIF.

            data(lv_name) = reader->name.

            replace all occurrences of '-' in lv_name with ''.


            APPEND VALUE #(
              node_type  = reader->node_type
              name       = reader->prefix &&
                           COND string(
                                  WHEN reader->prefix IS NOT INITIAL
                                    THEN ':' ) && lv_name
              value      = reader->value ) TO nodes.

            IF reader->node_type = if_sxml_node=>co_nt_element_open.
              DO.
                reader->next_attribute( ).
                IF reader->node_type <> if_sxml_node=>co_nt_attribute.
                  EXIT.
                ENDIF.

                APPEND VALUE #(
                  node_type = if_sxml_node=>co_nt_initial
                  name       = reader->prefix &&
                               COND string(
                                 WHEN reader->prefix IS NOT INITIAL
                                   THEN ':' ) && reader->name
                  value      = reader->value ) TO nodes.

              ENDDO.

            ENDIF.

          ENDDO.

        CATCH cx_sxml_parse_error INTO DATA(parse_error).
          out->write_text( parse_error->get_text( ) ).
      ENDTRY.

      "Tratamento para Elementos Simples do XML
      LOOP AT nodes INTO DATA(wl_node).
        DATA(tabix) = sy-tabix.

        IF wl_node-node_type EQ if_sxml_node=>co_nt_element_open.

          DATA(tabix_value) = tabix + 1.

          READ TABLE nodes INTO DATA(wl_node_aux) INDEX tabix_value.

          IF ( sy-subrc EQ 0 ) AND ( wl_node_aux-node_type EQ if_sxml_node=>co_nt_value ).

            DATA(tabix_close) = tabix + 2.

            READ TABLE nodes INTO wl_node_aux INDEX tabix_close.
            IF ( sy-subrc EQ 0 ) AND ( wl_node_aux-node_type EQ if_sxml_node=>co_nt_element_close ).
              DELETE nodes INDEX tabix.
              DELETE nodes INDEX tabix_close - 1 .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      "Determine the array limits in the internal table
      LOOP AT nodes ASSIGNING FIELD-SYMBOL(<node_open>) WHERE node_type = if_sxml_node=>co_nt_element_open
                                                          AND array IS INITIAL.

        DATA(idx_open) = sy-tabix.

        LOOP AT nodes ASSIGNING FIELD-SYMBOL(<node_close>) FROM idx_open  + 1 WHERE node_type = if_sxml_node=>co_nt_element_close
                                                                                AND name = <node_open>-name.

          DATA(idx_close) = sy-tabix.

          IF idx_close < lines( nodes ).

            ASSIGN nodes[ idx_close + 1 ] TO FIELD-SYMBOL(<node>).

            READ TABLE i_element_array INTO DATA(wl_element_array) WITH KEY table_line  = <node_open>-name.
            IF sy-subrc EQ 0.
              <node_open>-array = 'O'.
            ENDIF.

            IF <node>-node_type = if_sxml_node=>co_nt_element_open AND <node>-name = <node_open>-name.

              <node_open>-array = 'O'.
              <node>-array = '_'.

            ELSEIF ( <node>-node_type = if_sxml_node=>co_nt_element_open AND <node>-name <> <node_open>-name ) OR
                   ( <node>-node_type = if_sxml_node=>co_nt_element_close ) OR
                   ( <node>-node_type = if_sxml_node=>co_nt_value    AND  <node>-name <> <node_open>-name ).

              <node_close>-array = COND #( WHEN <node_open>-array = 'O' THEN 'C' ).
              EXIT.
            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDLOOP.

      "Render the internal table to JSON-XML
      DATA(writer) = CAST if_sxml_writer(
       cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
      "create( type = if_sxml=>co_xt_xml10 ) ).

      TRY.
          writer->open_element( name = 'object' ).

          LOOP AT nodes ASSIGNING <node>.

            TRY.

                CASE <node>-node_type.

                  WHEN if_sxml_node=>co_nt_element_open.

                    IF <node>-array IS INITIAL.

                      writer->open_element( name = 'object' ).
                      writer->write_attribute( name = 'name' value = <node>-name ).

                    ELSEIF <node>-array = 'O'.

                      writer->open_element( name = 'array' ).

                      writer->write_attribute( name = 'name'

                                               value = <node>-name ).

                      writer->open_element( name = 'object' ).

                    ELSEIF <node>-array = '_'.

                      writer->open_element( name = 'object' ).

                    ENDIF.

                  WHEN if_sxml_node=>co_nt_element_close.

                    IF <node>-array <> 'C'.

                      writer->close_element( ).

                    ELSE.

                      writer->close_element( ).

                      writer->close_element( ).

                    ENDIF.

                  WHEN if_sxml_node=>co_nt_initial.

                    writer->open_element( name = 'str' ).

                    writer->write_attribute( name = 'name'

                                             value = 'a_' && <node>-name ).

                    writer->write_value( <node>-value ).

                    writer->close_element( ).

                  WHEN if_sxml_node=>co_nt_value.

                    writer->open_element( name = 'str' ).

                    writer->write_attribute( name = 'name'
                                            value = <node>-name ).
                    " value = 'e_' && <node>-name ).

                    writer->write_value( <node>-value ).

                    writer->close_element( ).

                  WHEN OTHERS.

                    out->display( 'A node type is not yet supported' ).

                    RETURN.

                ENDCASE.

              CATCH cx_sxml_error INTO DATA(exc).

                out->write( exc->get_text( ) ).

            ENDTRY.

          ENDLOOP.

          writer->close_element( ).

          DATA(json) = CAST cl_sxml_string_writer( writer )->get_output( ).

          IF writer->if_sxml~type = if_sxml=>co_xt_json.

            out->write_json( json ).

          ELSEIF writer->if_sxml~type = if_sxml=>co_xt_xml10.

            out->write_xml( json ).

          ENDIF.

        CATCH cx_sxml_error INTO DATA(exc2).

          out->write( exc->get_text( ) ).

      ENDTRY.


      r_json = xstring_to_string( i_xstring =  json ).

      "out->display( ).
      "data(R_JSON) = out->get( ).

      EXIT.

    ENDDO.




  ENDMETHOD.


  METHOD xml_to_table.

    CHECK i_xml IS NOT INITIAL.

    DATA(lv_json) = zcl_string2=>xml_to_json( EXPORTING i_xml = i_xml i_element_array = i_element_array ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = r_data ).

  ENDMETHOD.


  METHOD XSTRING_TO_BASE64.

    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        BINDATA                  = I_XSTRING
*       BINLENG                  =
      IMPORTING
        B64DATA                  = R_BASE
      EXCEPTIONS
        SSF_KRN_ERROR            = 1
        SSF_KRN_NOOP             = 2
        SSF_KRN_NOMEMORY         = 3
        SSF_KRN_OPINV            = 4
        SSF_KRN_INPUT_DATA_ERROR = 5
        SSF_KRN_INVALID_PAR      = 6
        SSF_KRN_INVALID_PARLEN   = 7
        OTHERS                   = 8.

  ENDMETHOD.


  method XSTRING_TO_STRING.

   TYPES: BEGIN OF TY_TAB,
            LINE TYPE X LENGTH 255,
          END OF TY_TAB.


  DATA : ITAB TYPE TABLE OF TY_TAB.

  DATA: FILESIZE TYPE I.

  CLEAR: ITAB[].

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      BUFFER        = I_XSTRING
    IMPORTING
      OUTPUT_LENGTH = FILESIZE
    TABLES
      BINARY_TAB    = ITAB.

   CALL FUNCTION 'SCMS_BINARY_TO_STRING'
     EXPORTING
       INPUT_LENGTH  = FILESIZE
     IMPORTING
       TEXT_BUFFER   = R_STRING
     TABLES
       BINARY_TAB    = ITAB.


  endmethod.
ENDCLASS.
