class ZCL_RISK_OF_PESTICIDE definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TY_RISK,
        TITLE    TYPE STRING,
        CRITICAL TYPE STRING,
        WARNING  TYPE STRING,
        POSITIVE TYPE STRING,
      END OF TY_RISK .

  class-methods GET_VALIDITY_RISK
    importing
      !WITH_ICON type ABAP_BOOL default ABAP_TRUE
    returning
      value(RISK) type TY_RISK .
  class-methods CHECK_RISK_OF_VALIDITY_DATE
    importing
      !VALIDITY_DATE type SY-DATUM
    returning
      value(RISK) type STRING .
  PROTECTED SECTION.
private section.

  class-data:
    RISKS_OF_VALIDITY_DATE TYPE TABLE OF RGSB4 .
  class-data AT_RESULT type TFMATAGE .
ENDCLASS.



CLASS ZCL_RISK_OF_PESTICIDE IMPLEMENTATION.


  METHOD CHECK_RISK_OF_VALIDITY_DATE.
    "//Short description about this method:
    "//The variable _RISK-TITLE(1) can return tree values according with day's interval, it is:
    "// C-Critical;
    "// W-Warning;
    "// P-Positive;

    IF RISKS_OF_VALIDITY_DATE IS INITIAL.
      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          SETNR           = 'ZPP_VENCIMENTO_DEFENSIVO'
          NO_DESCRIPTIONS = ABAP_FALSE
        TABLES
          SET_VALUES      = RISKS_OF_VALIDITY_DATE
        EXCEPTIONS
          SET_NOT_FOUND   = 1
          OTHERS          = 2.
    ENDIF.

    AT_RESULT = VALIDITY_DATE - SY-DATUM.

    LOOP AT RISKS_OF_VALIDITY_DATE INTO DATA(_RISK).

      RISK = SWITCH #( _RISK-TITLE(1) WHEN 'C' THEN COND #( WHEN AT_RESULT LE _RISK-TO THEN _RISK-TITLE(1) ELSE RISK )
                                      WHEN 'W' THEN COND #( WHEN AT_RESULT BETWEEN _RISK-FROM AND _RISK-TO THEN _RISK-TITLE(1) ELSE RISK )
                                      WHEN 'P' THEN COND #( WHEN AT_RESULT GE _RISK-FROM THEN _RISK-TITLE(1) ELSE RISK )
                     ).
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_VALIDITY_RISK.
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        SETNR           = 'ZPP_VENCIMENTO_DEFENSIVO'
        NO_DESCRIPTIONS = ABAP_FALSE
      TABLES
        SET_VALUES      = RISKS_OF_VALIDITY_DATE
      EXCEPTIONS
        SET_NOT_FOUND   = 1
        OTHERS          = 2.

    RISK-TITLE = 'Legenda das cores:'.

    LOOP AT RISKS_OF_VALIDITY_DATE INTO DATA(_RISK).
      CASE _RISK-TITLE(1).
        WHEN 'C'.
          RISK-CRITICAL = |{ SWITCH #( WITH_ICON WHEN ABAP_TRUE THEN ICON_LED_RED ) } { _RISK-FROM } à { _RISK-TO } dias p/ vencimento|.
        WHEN 'W'.
          RISK-WARNING  = |{ SWITCH #( WITH_ICON WHEN ABAP_TRUE THEN ICON_LED_YELLOW )  } { _RISK-FROM } à { _RISK-TO } dias p/ vencimento|.
        WHEN 'P'.
          RISK-POSITIVE = |{ SWITCH #( WITH_ICON WHEN ABAP_TRUE THEN ICON_LED_GREEN ) } { _RISK-FROM } à { _RISK-TO } dias p/ vencimento|.
      ENDCASE.
    ENDLOOP.

    SHIFT RISK-CRITICAL LEFT DELETING LEADING SPACE.
    SHIFT RISK-WARNING  LEFT DELETING LEADING SPACE.
    SHIFT RISK-POSITIVE LEFT DELETING LEADING SPACE.
  ENDMETHOD.
ENDCLASS.
