*&---------------------------------------------------------------------*
*& Report  ZFIJ002
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIJ002.

DATA: W_LFA1  TYPE LFA1,

      I_LFA1  TYPE LFA1,
      I_LFB1  TYPE LFB1,
      I_LFM1  TYPE LFM1,
      I_YLFA1 TYPE LFA1,
      I_YLFB1 TYPE LFB1,
      I_YLFM1 TYPE LFM1,

      T_XLFAS TYPE TABLE OF  FLFAS,
      T_XLFB5 TYPE TABLE OF  FLFB5,
      T_XLFBK TYPE TABLE OF  FLFBK,
      T_XLFZA TYPE TABLE OF  FLFZA,
      T_YLFAS TYPE TABLE OF  FLFAS,
      T_YLFB5 TYPE TABLE OF  FLFB5,
      T_YLFBK TYPE TABLE OF  FLFBK,
      T_YLFZA TYPE TABLE OF  FLFZA.

DATA: W_KNA1  TYPE KNA1,

      I_KNA1  TYPE KNA1,
      I_KNB1  TYPE  KNB1,
      I_KNVV  TYPE  KNVV,
      I_YKNA1 TYPE  KNA1,
      I_YKNB1 TYPE  KNB1,

      T_XKNAS TYPE TABLE OF  FKNAS,
      T_XKNB5 TYPE TABLE OF  FKNB5,
      T_XKNBK TYPE TABLE OF  FKNBK,
      T_XKNVA TYPE TABLE OF  FKNVA,
      T_XKNVD TYPE TABLE OF  FKNVD,
      T_XKNVI TYPE TABLE OF  FKNVI,
      T_XKNVK TYPE TABLE OF  FKNVK,
      T_XKNVL TYPE TABLE OF  FKNVL,
      T_XKNVP TYPE TABLE OF  FKNVP,
      T_XKNVS TYPE TABLE OF  FKNVS,
      T_XKNEX TYPE TABLE OF  FKNEX,
      T_XKNZA TYPE TABLE OF  FKNZA,
      T_YKNAS TYPE TABLE OF  FKNAS,
      T_YKNB5 TYPE TABLE OF  FKNB5,
      T_YKNBK TYPE TABLE OF  FKNBK,
      T_YKNVA TYPE TABLE OF  FKNVA,
      T_YKNVD TYPE TABLE OF  FKNVD,
      T_YKNVI TYPE TABLE OF  FKNVI,
      T_YKNVK TYPE TABLE OF  FKNVK,
      T_YKNVL TYPE TABLE OF  FKNVL,
      T_YKNVP TYPE TABLE OF  FKNVP,
      T_YKNVS TYPE TABLE OF  FKNVS,
      T_YKNEX TYPE TABLE OF  FKNEX,
      T_YKNZA TYPE TABLE OF  FKNZA.

SELECT SINGLE COUNT(*)
  INTO @DATA(VG_JOB)
    FROM TBTCO
   WHERE JOBNAME EQ 'BLOQUEIO_CLIENTE/FORNECEDOR_OPUS'
     AND STATUS EQ 'R'.

IF ( VG_JOB EQ 1 ).
  SELECT *
    FROM ZFIT0144
    INTO TABLE @DATA(_T_ZFITA144).

  SORT   _T_ZFITA144 BY ELIMINAR DESCENDING SEQUENCIA ASCENDING.

  LOOP AT _T_ZFITA144 INTO DATA(_W_ZFITA144).
    IF _W_ZFITA144-COD_FORN IS INITIAL.
      SELECT SINGLE *
        FROM LFA1
        INTO W_LFA1
        WHERE STCD1 = _W_ZFITA144-CNPJ_CPF.
      IF SY-SUBRC = 0.
        _W_ZFITA144-COD_FORN = W_LFA1-LIFNR.
      ENDIF.
    ENDIF.
    IF _W_ZFITA144-COD_FORN IS NOT INITIAL.
      CLEAR I_LFA1.
      SELECT SINGLE *
        FROM LFA1
        INTO I_LFA1
        WHERE LIFNR = _W_ZFITA144-COD_FORN.
      IF SY-SUBRC = 0.
        IF _W_ZFITA144-ELIMINAR IS INITIAL.
          I_LFA1-SPERR = 'X'.
          I_LFA1-SPERM = 'X'.
        ELSE.
          CLEAR: I_LFA1-SPERR, I_LFA1-SPERM.
        ENDIF.
        CALL FUNCTION 'VENDOR_UPDATE'
          EXPORTING
            I_LFA1  = I_LFA1
            I_LFB1  = I_LFB1
            I_LFM1  = I_LFM1
            I_YLFA1 = I_YLFA1
            I_YLFB1 = I_YLFB1
            I_YLFM1 = I_YLFM1
          TABLES
            T_XLFAS = T_XLFAS
            T_XLFB5 = T_XLFB5
            T_XLFBK = T_XLFBK
            T_XLFZA = T_XLFZA
            T_YLFAS = T_YLFAS
            T_YLFB5 = T_YLFB5
            T_YLFBK = T_YLFBK
            T_YLFZA = T_YLFZA.

        IF SY-SUBRC EQ 0.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDIF.


    IF _W_ZFITA144-COD_CLI IS INITIAL.
      SELECT SINGLE *
        FROM KNA1
        INTO W_KNA1
        WHERE STCD1 = _W_ZFITA144-CNPJ_CPF.
      IF SY-SUBRC = 0.
        _W_ZFITA144-COD_CLI = W_KNA1-KUNNR.
      ENDIF.
    ENDIF.
    IF _W_ZFITA144-COD_CLI IS NOT INITIAL.
      CLEAR I_KNA1.
      SELECT SINGLE *
        FROM KNA1
        INTO I_KNA1
        WHERE KUNNR =  _W_ZFITA144-COD_CLI.
      IF SY-SUBRC = 0.
        IF _W_ZFITA144-ELIMINAR IS INITIAL.
          I_KNA1-SPERR = 'X'.
          I_KNA1-SPERZ = 'X'.
        ELSE.
          CLEAR : I_KNA1-SPERR, I_KNA1-SPERZ.
        ENDIF.

        CALL FUNCTION 'CUSTOMER_UPDATE'
          EXPORTING
            I_KNA1  = I_KNA1
            I_KNB1  = I_KNB1
            I_KNVV  = I_KNVV
            I_YKNA1 = I_YKNA1
            I_YKNB1 = I_YKNB1
          TABLES
            T_XKNAS = T_XKNAS
            T_XKNB5 = T_XKNB5
            T_XKNBK = T_XKNBK
            T_XKNVA = T_XKNVA
            T_XKNVD = T_XKNVD
            T_XKNVI = T_XKNVI
            T_XKNVK = T_XKNVK
            T_XKNVL = T_XKNVL
            T_XKNVP = T_XKNVP
            T_XKNVS = T_XKNVS
            T_XKNZA = T_XKNZA
            T_YKNAS = T_YKNAS
            T_YKNB5 = T_YKNB5
            T_YKNBK = T_YKNBK
            T_YKNVA = T_YKNVA
            T_YKNVD = T_YKNVD
            T_YKNVI = T_YKNVI
            T_YKNVK = T_YKNVK
            T_YKNVL = T_YKNVL
            T_YKNVP = T_YKNVP
            T_YKNVS = T_YKNVS
            T_YKNZA = T_YKNZA.

        IF SY-SUBRC EQ 0.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE FROM ZFIT0144.
  COMMIT WORK.

ENDIF.
