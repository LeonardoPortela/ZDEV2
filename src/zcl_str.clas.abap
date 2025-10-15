class ZCL_STR definition
  public
  final
  create public .

public section.

  constants AT_REGEX_DATE type STRING value '[0-9]{4}[-]{1}[0-9]{2}[-]{1}[0-9]{2}' ##NO_TEXT.
  constants AT_REGEX_MAIL type STRING value '^([\w-]+(?:\.[\w-]+)*)@((?:[\w-]+\.)*\w[\w-]{0,66})\.([a-z]{2,6}(?:\.[a-z]{2})?)$' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !I_TEXTO type STRING .
    "! Metodo para Retornar a String da Instância
    "! @parameter result | Retorno String
  methods GET
    returning
      value(R_STRING) type STRING .
  class-methods SET
    importing
      !I_TEXTO type STRING
    returning
      value(R_STR) type ref to ZCL_STR .
  methods NEW_INITCAP
    returning
      value(R_STR) type ref to ZCL_STR .
  class-methods UPPER
    importing
      !I_TEXTO type STRING
    returning
      value(R_STR) type ref to ZCL_STR .
  class-methods FLOAT_TO_STRING
    importing
      !I_VALOR type F
    returning
      value(R_STR) type ref to ZCL_STR .
  class-methods QUANT_TO_STRING
    importing
      !I_VALOR type BSTMG
    returning
      value(R_STR) type ref to ZCL_STR .
  class-methods VALOR_TO_STRING
    importing
      !I_VALOR type BBWERT
    returning
      value(R_STR) type ref to ZCL_STR .
  class-methods INITCAP
    importing
      !I_TEXTO type STRING
    returning
      value(R_STR) type ref to ZCL_STR .
  class-methods FIRST_TEXT
    importing
      !I_TEXTO type STRING
    returning
      value(R_STR) type ref to ZCL_STR .
  class-methods NVL
    importing
      !I_DATA_1 type STRING
      !I_DATA_2 type STRING
    returning
      value(R_STR) type ref to ZCL_STR .
  class-methods LPAD
    importing
      !I_STR type STRING
      !I_QTD type I
      !I_CHAR type CHAR01
    returning
      value(R_STR) type ref to ZCL_STR .
  class-methods TRIM
    importing
      !I_STR type STRING
    returning
      value(R_STR) type ref to ZCL_STR .
  class-methods TRIM_ALL
    importing
      !I_STR type STRING
    returning
      value(R_STR) type ref to ZCL_STR .
    "! Metodo para retornar a quantidade de caracteres
    "! @parameter result | Retorno Integer
  methods LENGTH
    returning
      value(R) type I .
    "! Metodo para Retornar uma verificação Regex/Matcher
    "! @parameter PATTERN | Regra Regex
    "! @parameter result | Retorno abap_true se Match
  methods MATCHER
    importing
      !PATTERN type STRING
      !IGNORE_CASE type CHAR01 default ABAP_TRUE
    returning
      value(R_MATCH) type FLAG .
  class-methods MATCHER_EMAIL
    importing
      !I_MAIL type STRING
    returning
      value(R_MATCH) type FLAG .
  class-methods MATCHER_DATE
    importing
      !I_DATE type STRING
    returning
      value(R_MATCH) type FLAG .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA TEXTO TYPE STRING.

ENDCLASS.



CLASS ZCL_STR IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    ME->TEXTO = I_TEXTO.
  ENDMETHOD.


  METHOD FIRST_TEXT.
    R_STR = NEW #( I_TEXTO = I_TEXTO ).
    SPLIT I_TEXTO AT SPACE INTO TABLE DATA(LT_SPLIT).
    READ TABLE LT_SPLIT INTO DATA(WA_TEXTO) INDEX 1.
    IF SY-SUBRC IS INITIAL.
      R_STR->TEXTO = WA_TEXTO.
    ELSE.
      R_STR->TEXTO = SPACE.
    ENDIF.
  ENDMETHOD.


  METHOD FLOAT_TO_STRING.
    DATA: TEXTO TYPE C LENGTH 100.
    WRITE I_VALOR TO TEXTO.
    CONDENSE TEXTO NO-GAPS.
    R_STR = NEW #( I_TEXTO = CONV #( TEXTO ) ).
  ENDMETHOD.


  METHOD GET.
    R_STRING = ME->TEXTO.
  ENDMETHOD.


  METHOD INITCAP.
    R_STR = NEW #( I_TEXTO = I_TEXTO ).
    R_STR->TEXTO = ZCL_STRING=>INITIALCAP( I_STR = I_TEXTO ).
  ENDMETHOD.


  METHOD LENGTH.
    R = STRLEN( ME->TEXTO ).
  ENDMETHOD.


  METHOD LPAD.
    R_STR = NEW #( I_TEXTO = I_STR ).
    DATA(LC_DO) = I_QTD - ZCL_STR=>SET( R_STR->TEXTO )->LENGTH( ).
    CHECK LC_DO GT 0.
    DO LC_DO TIMES.
      CONCATENATE I_CHAR R_STR->TEXTO INTO R_STR->TEXTO.
    ENDDO.
  ENDMETHOD.


  METHOD MATCHER.
    DATA: REGEX TYPE REF TO CL_ABAP_REGEX.
    REGEX = NEW #( PATTERN = PATTERN IGNORE_CASE = IGNORE_CASE ).
    R_MATCH = REGEX->CREATE_MATCHER( TEXT = ME->TEXTO )->MATCH( ).
    CLEAR: REGEX.
  ENDMETHOD.


  METHOD MATCHER_DATE.
    R_MATCH = ZCL_STR=>SET( I_TEXTO = I_DATE )->MATCHER( PATTERN = AT_REGEX_DATE ).
  ENDMETHOD.


  METHOD MATCHER_EMAIL.
    R_MATCH = ZCL_STR=>SET( I_TEXTO = I_MAIL )->MATCHER( PATTERN = AT_REGEX_MAIL ).
  ENDMETHOD.


  METHOD NEW_INITCAP.
    R_STR = ZCL_STR=>INITCAP( ME->GET( ) ).
  ENDMETHOD.


  METHOD NVL.
    R_STR = NEW #( I_TEXTO = COND STRING( WHEN I_DATA_1 IS NOT INITIAL THEN I_DATA_1 ELSE I_DATA_2 ) ).
  ENDMETHOD.


  METHOD QUANT_TO_STRING.
    DATA: TEXTO TYPE C LENGTH 100.
    WRITE I_VALOR TO TEXTO.
    CONDENSE TEXTO NO-GAPS.
    R_STR = NEW #( I_TEXTO = CONV #( TEXTO ) ).
  ENDMETHOD.


  METHOD SET.
    R_STR = NEW #( I_TEXTO = I_TEXTO ).
  ENDMETHOD.


  METHOD TRIM.
    R_STR = NEW #( I_TEXTO = I_STR ).
    CONDENSE R_STR->TEXTO.
  ENDMETHOD.


  METHOD TRIM_ALL.
    R_STR = NEW #( I_TEXTO = I_STR ).
    CONDENSE R_STR->TEXTO NO-GAPS.
  ENDMETHOD.


  METHOD UPPER.
    R_STR = NEW #( I_TEXTO = I_TEXTO ).
    TRANSLATE R_STR->TEXTO TO UPPER CASE.
  ENDMETHOD.


  METHOD VALOR_TO_STRING.
    DATA: TEXTO TYPE C LENGTH 100.
    WRITE I_VALOR TO TEXTO.
    CONDENSE TEXTO NO-GAPS.
    R_STR = NEW #( I_TEXTO = CONV #( TEXTO ) ).
  ENDMETHOD.
ENDCLASS.
