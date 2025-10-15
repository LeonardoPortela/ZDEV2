FUNCTION Z_CONTROLE_FECHAMES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  ZFIT0033-BUKRS
*"     REFERENCE(I_DATA) TYPE  ZFIT0033-DATA_LIM
*"     REFERENCE(I_DEP_RESP) TYPE  ZFIT0033-DEP_RESP OPTIONAL
*"     REFERENCE(I_USER) TYPE  UNAME DEFAULT SY-UNAME
*"     REFERENCE(I_MONAT) TYPE  ZFIT0033-MONAT OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_STATUS) TYPE  CHAR0001
*"     REFERENCE(E_MESSA) TYPE  CHAR0064
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: WA_ZFIT0033 TYPE ZFIT0033,
        P_DATA_VAL  TYPE DATUM,
        VG_MONAT    TYPE ZFIT0033-MONAT,
        VG_GJAHR    TYPE ZFIT0033-GJAHR.

  CHECK SY-TCODE NE 'KO8G'.
  CHECK I_BUKRS  NE '0201'.



  E_STATUS = 'S'.

  IF SY-TCODE+0(2) = 'MB'.
    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        P_DATA_ENT     = I_DATA
        P_BUKRS        = I_BUKRS
        P_VAL_FI       = ''
        P_VAL_MM       = 'X'
      IMPORTING
        P_DATA_VAL     = P_DATA_VAL
      EXCEPTIONS
        DATA_FI_MM_NAO = 1
        OTHERS         = 2.
  ELSE.
    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        P_DATA_ENT     = I_DATA
        P_BUKRS        = I_BUKRS
        P_VAL_FI       = 'X'
        P_VAL_MM       = ''
      IMPORTING
        P_DATA_VAL     = P_DATA_VAL
      EXCEPTIONS
        DATA_FI_MM_NAO = 1
        OTHERS         = 2.
  ENDIF.

  IF ( SY-DATUM+0(6) GT P_DATA_VAL+0(6) ) OR I_MONAT GE 13.     "mas não é o do mês corrente
    IF I_MONAT IS NOT INITIAL AND I_DATA+4(2) = '12'.
      VG_MONAT = I_MONAT.
    ELSE.
      VG_MONAT = I_DATA+4(2).
    ENDIF.

    VG_GJAHR = I_DATA+0(4).
    SELECT SINGLE *
        FROM ZFIT0033
        INTO WA_ZFIT0033
        WHERE MONAT      = VG_MONAT
        AND   GJAHR      = VG_GJAHR
        AND   USNAM      = I_USER.


    IF SY-SUBRC  NE 0.
      E_STATUS = 'E'.
      E_MESSA  = 'Sem parâmetro fechamento mensal'.
      E_MESSA  = TEXT-M01.
      "MESSAGE E398(00) WITH E_MESSA  RAISING ERROR.
    ELSEIF WA_ZFIT0033-LIB_CONTAB IS INITIAL.
      E_STATUS = 'E'.
      E_MESSA  = 'Usuário bloqueado para lançamento'.
      E_MESSA  = TEXT-M02.
    ELSEIF    SY-DATUM GT WA_ZFIT0033-DATA_LIM.
      E_STATUS = 'E'.
      E_MESSA  = 'Data limite ultrapassada para lançamento'.
      E_MESSA  = TEXT-M03.
    ELSEIF ( SY-DATUM EQ WA_ZFIT0033-DATA_LIM AND SY-UZEIT GT WA_ZFIT0033-HORA_LIM ).
      E_STATUS = 'E'.
      E_MESSA  = 'Hora limite ultrapassada para lançamento'.
      E_MESSA  = TEXT-M04.
    ENDIF.
  ENDIF.

ENDFUNCTION.
