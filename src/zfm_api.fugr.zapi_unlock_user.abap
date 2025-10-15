FUNCTION ZAPI_UNLOCK_USER.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CPF) TYPE  PBR_CPFNR
*"     REFERENCE(I_CK_SENHA) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     VALUE(E_USER) TYPE  XUBNAME
*"     VALUE(E_SENHA) TYPE  CHAR20
*"----------------------------------------------------------------------

  DATA: T_RETURN   TYPE TABLE OF BAPIRET2,
        R_PASSWORD TYPE BAPIPWD.

  DATA(COMMONS) = NEW ZCL_HRST_COMMONS( ).

  CHECK I_CPF IS NOT INITIAL.

  DATA(CPF) = I_CPF.

  IF CPF CS '.'.
    REPLACE ALL OCCURRENCES OF '.' IN CPF WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN CPF WITH ''.
  ENDIF.

  SELECT SINGLE
    US~BNAME
  FROM USR21 AS US
  INNER JOIN ADR3 AS AD
  ON US~PERSNUMBER = AD~PERSNUMBER  AND
     US~ADDRNUMBER = AD~ADDRNUMBER
  INTO @DATA(USER)
    WHERE AD~FAX_NUMBER = @CPF.

  IF SY-SUBRC = 0.

    CALL FUNCTION 'BAPI_USER_UNLOCK'
      EXPORTING
        USERNAME = USER
      TABLES
        RETURN   = T_RETURN.

    TRY.
        DATA(RETURN) = T_RETURN[ 1 ]-MESSAGE.
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
    ENDTRY.

    IF RETURN CS 'desbloqueado'.
      E_USER = USER.
    ENDIF.

    CLEAR: T_RETURN[], RETURN.

    IF I_CK_SENHA IS NOT INITIAL.

      DATA: V_INT  TYPE I,
            V_PWD  TYPE BAPIPWD,
            V_PWDX TYPE BAPIPWDX.

      CALL FUNCTION 'QF05_RANDOM_INTEGER'
        EXPORTING
          RAN_INT_MAX   = 9999    " Greatest required value
          RAN_INT_MIN   = 1000    " Smallest required value
        IMPORTING
          RAN_INT       = V_INT    " Random number
        EXCEPTIONS
          INVALID_INPUT = 1
          OTHERS        = 2.

      DATA(V_STRING) = |amaggi@{ V_INT }|.
      V_PWD = CONV #( V_STRING ).
      V_PWDX = 'X'.

      CALL FUNCTION 'BAPI_USER_CHANGE'
        EXPORTING
          USERNAME  = USER
          PASSWORD  = V_PWD
          PASSWORDX = V_PWDX
        TABLES
          RETURN    = T_RETURN.

      TRY.
          RETURN = T_RETURN[ 1 ]-MESSAGE.
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
      ENDTRY.

      IF RETURN CS 'modificado'.
        E_SENHA = V_STRING.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFUNCTION.
