FUNCTION Z_FI_GL_SALDO_FAGLFLEXT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(RYEAR) TYPE  GJAHR
*"     REFERENCE(WAERS) TYPE  WAERS OPTIONAL
*"     REFERENCE(CONTAS) TYPE  ZCT_EMP_CONTAS
*"     REFERENCE(P_GERAR_TODAS) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_GERAR_SOC_PARCEIRA) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(RLDNR) TYPE  FAGL_RLDNR OPTIONAL
*"     REFERENCE(P_GERAR_FILIAL) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      IT_SALDOS STRUCTURE  ZDE_FI_GL_SALDO_FAGLFLEXT
*"      IT_SALDOS_2 STRUCTURE  ZDE_FI_GL_SALDO_FAGLFLEXT OPTIONAL
*"      IT_SALDOS_3 STRUCTURE  ZDE_FI_GL_SALDO_FAGLFLEXT OPTIONAL
*"  EXCEPTIONS
*"      MOEDA_NAO_ADM
*"      ERRO_LEDGER
*"------------------------------------------------------------------------

  DATA: WA_CONTAS         TYPE ZLC_EMP_CONTAS,
        IT_EMPRESAS       TYPE TABLE OF T001 WITH HEADER LINE,
        WA_MOEDAS_EMPRESA TYPE X001,
        IT_MOEDA          TYPE TABLE OF TY_MD_FLEXT_EMP WITH HEADER LINE,
        IT_CONTAS         TYPE TABLE OF ZLC_EMP_CONTAS,
        IT_FAGLFLEXT      TYPE TABLE OF FAGLFLEXT,
        WA_FAGLFLEXT      TYPE FAGLFLEXT,
        IT_SALDO_FAGL     TYPE HASHED TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT
                               WITH UNIQUE KEY
                               RCLNT RYEAR RLDNR RBUKRS RBUSA RACCT RASSC WITH HEADER LINE,
        IT_SALDO_FAGL_2   TYPE HASHED TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT
                               WITH UNIQUE KEY
                               RCLNT RYEAR RLDNR RBUKRS RBUSA RACCT RASSC WITH HEADER LINE,
        IT_SALDO_FAGL_3   TYPE HASHED TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT
                               WITH UNIQUE KEY
                               RCLNT RYEAR RLDNR RBUKRS RBUSA RACCT RASSC WITH HEADER LINE,
        WA_RANGE_RYEAR    TYPE FAGL_RANGE_RYEAR,
        WA_RANGE_ACTIV    TYPE FAGL_RANGE_ACTIV,
        WA_RANGE_AWTYP    TYPE FAGL_RANGE_AWTYP,
        WA_RANGE_RLDNR    TYPE FAGL_RANGE_RLDNR,
        WA_RANGE_RBUKRS   TYPE FAGL_RANGE_BUKRS,
        WA_RANGE_RACCT    TYPE FAGL_RANGE_RACCT,
        WA_RANGE_RRCTY    TYPE FAGL_RANGE_RRCTY,
        WA_RANGE_RVERS    TYPE FAGL_RANGE_RVERS.

  DATA: I_RANGE_RYEAR  TYPE FAGL_RANGE_T_RYEAR,
        I_RANGE_ACTIV  TYPE FAGL_RANGE_T_ACTIV,
        I_RANGE_AWTYP  TYPE TABLE OF FAGL_RANGE_AWTYP,
        I_RANGE_RLDNR  TYPE FAGL_RANGE_T_RLDNR,
        I_RANGE_RBUKRS TYPE FAGL_RANGE_T_BUKRS,
        I_RANGE_RACCT  TYPE FAGL_RANGE_T_RACCT,
        I_RANGE_RRCTY  TYPE FAGL_RANGE_T_RRCTY,
        I_RANGE_RVERS  TYPE FAGL_RANGE_T_RVERS.


  "Seleciona Empresas que serão utilizadas na Seleção """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "****************************************************************************************************************
  SELECT *
    INTO TABLE IT_EMPRESAS
    FROM T001
     FOR ALL ENTRIES IN CONTAS
   WHERE BUKRS EQ CONTAS-BUKRS.
  "****************************************************************************************************************


  "Carrega Tipo da Moeda da Empresa com Relação a Moeda Solicitada """"""""""""""""""""""""""""""""""""""""""""""""
  "****************************************************************************************************************
  MOVE CONTAS TO IT_CONTAS.
  DELETE ADJACENT DUPLICATES FROM IT_CONTAS COMPARING BUKRS.

  LOOP AT IT_CONTAS INTO WA_CONTAS.
    CLEAR: IT_EMPRESAS.
    READ TABLE IT_EMPRESAS WITH KEY BUKRS = WA_CONTAS-BUKRS.
    IF ( WAERS EQ IT_EMPRESAS-WAERS ) OR ( WAERS IS INITIAL ).
      IT_MOEDA-TP_MOEDA = 'H'.
      IT_MOEDA-WAERS    = IT_EMPRESAS-WAERS.
    ELSE.
      CLEAR: WA_MOEDAS_EMPRESA.
      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          I_BUKRS = WA_CONTAS-BUKRS
        IMPORTING
          E_X001  = WA_MOEDAS_EMPRESA.
      IF WA_MOEDAS_EMPRESA-HWAE2 EQ WAERS.
        IT_MOEDA-TP_MOEDA = 'K'.
        IT_MOEDA-WAERS    = WA_MOEDAS_EMPRESA-HWAE2.
      ELSEIF WA_MOEDAS_EMPRESA-HWAE3 EQ WAERS.
        IT_MOEDA-TP_MOEDA = 'O'.
        IT_MOEDA-WAERS    = WA_MOEDAS_EMPRESA-HWAE3.
      ELSE.
        MESSAGE E822(FR) RAISING MOEDA_NAO_ADM WITH WAERS WA_CONTAS-BUKRS.
      ENDIF.
    ENDIF.

    IT_MOEDA-BUKRS = WA_CONTAS-BUKRS.
    APPEND IT_MOEDA.
  ENDLOOP.
  "****************************************************************************************************************

  CHECK IT_MOEDA[] IS NOT INITIAL.

  "Busca de Ledger do Cliente""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "****************************************************************************************************************
  IF RLDNR IS INITIAL.
    CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
      IMPORTING
        E_RLDNR       = WA_RANGE_RLDNR-LOW
      EXCEPTIONS
        NOT_FOUND     = 1
        MORE_THAN_ONE = 2
        OTHERS        = 3.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ERRO_LEDGER.
    ENDIF.

  ELSE.
    WA_RANGE_RLDNR-LOW = RLDNR.
  ENDIF.
  "****************************************************************************************************************


  "Carrega Estruturas para Consulta""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "****************************************************************************************************************
  WA_RANGE_RLDNR-SIGN   = 'I'.
  WA_RANGE_RLDNR-OPTION = 'EQ'.
  WA_RANGE_RLDNR-HIGH   = WA_RANGE_RLDNR-LOW.
  APPEND WA_RANGE_RLDNR TO I_RANGE_RLDNR.

  WA_RANGE_RYEAR-SIGN   = 'I'.
  WA_RANGE_RYEAR-OPTION = 'EQ'.
  WA_RANGE_RYEAR-LOW    = RYEAR.
  WA_RANGE_RYEAR-HIGH   = RYEAR.
  APPEND WA_RANGE_RYEAR TO I_RANGE_RYEAR.

*  WA_RANGE_ACTIV-SIGN   = 'I'.
*  WA_RANGE_ACTIV-OPTION = 'EQ'.
*  WA_RANGE_ACTIV-LOW    = ACTIV.
*  WA_RANGE_ACTIV-HIGH   = ACTIV.
*  APPEND WA_RANGE_ACTIV TO I_RANGE_ACTIV.

*  WA_RANGE_AWTYP-SIGN   = 'I'.
*  WA_RANGE_AWTYP-OPTION = 'EQ'.
*  WA_RANGE_AWTYP-LOW    = AWTYP.
*  WA_RANGE_AWTYP-HIGH   = AWTYP.
*  APPEND WA_RANGE_AWTYP TO I_RANGE_AWTYP.

  WA_RANGE_RRCTY-SIGN   = 'I'.
  WA_RANGE_RRCTY-OPTION = 'EQ'.
  WA_RANGE_RRCTY-LOW    = '0'.
  WA_RANGE_RRCTY-HIGH   = '0'.
  APPEND WA_RANGE_RRCTY TO I_RANGE_RRCTY.

  WA_RANGE_RVERS-SIGN   = 'I'.
  WA_RANGE_RVERS-OPTION = 'EQ'.
  WA_RANGE_RVERS-LOW    = '001'.
  WA_RANGE_RVERS-LOW    = '001'.
  APPEND WA_RANGE_RVERS TO I_RANGE_RVERS.

  CLEAR: IT_CONTAS.
  MOVE CONTAS TO IT_CONTAS.
  DELETE ADJACENT DUPLICATES FROM IT_CONTAS COMPARING BUKRS.
  LOOP AT IT_CONTAS INTO WA_CONTAS.
    IF WA_CONTAS-BUKRS IS NOT INITIAL.
      WA_RANGE_RBUKRS-SIGN   = 'I'.
      WA_RANGE_RBUKRS-OPTION = 'EQ'.
      WA_RANGE_RBUKRS-LOW    = WA_CONTAS-BUKRS.
      WA_RANGE_RBUKRS-HIGH   = WA_CONTAS-BUKRS.
      APPEND WA_RANGE_RBUKRS TO I_RANGE_RBUKRS.
    ENDIF.
  ENDLOOP.

  CLEAR: IT_CONTAS.
  MOVE CONTAS TO IT_CONTAS.
  DELETE ADJACENT DUPLICATES FROM IT_CONTAS COMPARING SAKNR.
  LOOP AT IT_CONTAS INTO WA_CONTAS WHERE SAKNR NE '*'.
    IF WA_CONTAS-SAKNR IS NOT INITIAL.
      WA_RANGE_RACCT-SIGN   = 'I'.
      WA_RANGE_RACCT-OPTION = 'EQ'.
      WA_RANGE_RACCT-LOW    = WA_CONTAS-SAKNR.
      WA_RANGE_RACCT-HIGH   = WA_CONTAS-SAKNR.
      APPEND WA_RANGE_RACCT TO I_RANGE_RACCT.
    ENDIF.
  ENDLOOP.
  "*****************************************************************************************************************


  "Carrega dados de Saldos Contábeis Débitos/Créditos""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "****************************************************************************************************************"
  SELECT *
    INTO TABLE IT_FAGLFLEXT
    FROM FAGLFLEXT
   WHERE RYEAR  IN I_RANGE_RYEAR
     AND ACTIV  IN I_RANGE_ACTIV
     AND AWTYP  IN I_RANGE_AWTYP
     AND RLDNR  IN I_RANGE_RLDNR
     AND RRCTY  IN I_RANGE_RRCTY
     AND RVERS  IN I_RANGE_RVERS
     AND RBUKRS IN I_RANGE_RBUKRS
     AND RACCT  IN I_RANGE_RACCT.
  "****************************************************************************************************************


  "Gera saldo de conta razão"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "****************************************************************************************************************
  IF P_GERAR_TODAS IS INITIAL.
    CLEAR: IT_MOEDA.
    LOOP AT IT_FAGLFLEXT INTO WA_FAGLFLEXT.
      CLEAR: IT_SALDO_FAGL.
      MOVE-CORRESPONDING WA_FAGLFLEXT TO IT_SALDO_FAGL.

      IF P_GERAR_SOC_PARCEIRA IS INITIAL.
        CLEAR: IT_SALDO_FAGL-RASSC.
      ENDIF.

      IF P_GERAR_FILIAL IS INITIAL.
        CLEAR: IT_SALDO_FAGL-RBUSA.
      ENDIF.

      IF IT_MOEDA-BUKRS NE IT_SALDO_FAGL-RBUKRS.
        READ TABLE IT_MOEDA WITH KEY BUKRS = IT_SALDO_FAGL-RBUKRS.
      ENDIF.

      PERFORM ORGANIZA_VALORES_FAGLFLEXT USING IT_MOEDA WA_FAGLFLEXT CHANGING IT_SALDO_FAGL .

      COLLECT IT_SALDO_FAGL.
    ENDLOOP.

    LOOP AT IT_SALDO_FAGL.
      APPEND IT_SALDO_FAGL TO IT_SALDOS.
    ENDLOOP.
  ELSE.

    CLEAR: IT_MOEDA.

    LOOP AT IT_FAGLFLEXT INTO WA_FAGLFLEXT.
      CLEAR: IT_SALDO_FAGL, IT_SALDO_FAGL_2, IT_SALDO_FAGL_3.
      MOVE-CORRESPONDING WA_FAGLFLEXT TO IT_SALDO_FAGL.
      MOVE-CORRESPONDING WA_FAGLFLEXT TO IT_SALDO_FAGL_2.
      MOVE-CORRESPONDING WA_FAGLFLEXT TO IT_SALDO_FAGL_3.

      IF P_GERAR_SOC_PARCEIRA IS INITIAL.
        CLEAR: IT_SALDO_FAGL-RASSC,
               IT_SALDO_FAGL_2-RASSC,
               IT_SALDO_FAGL_3-RASSC.
      ENDIF.

      IF P_GERAR_FILIAL IS INITIAL.
        CLEAR: IT_SALDO_FAGL-RBUSA,
               IT_SALDO_FAGL_2-RBUSA,
               IT_SALDO_FAGL_3-RBUSA.
      ENDIF.

      IF IT_MOEDA-BUKRS NE IT_SALDO_FAGL-RBUKRS.
        READ TABLE IT_MOEDA WITH KEY BUKRS = IT_SALDO_FAGL-RBUKRS.
      ENDIF.

      IT_MOEDA-TP_MOEDA = 'H'.
      PERFORM ORGANIZA_VALORES_FAGLFLEXT USING IT_MOEDA WA_FAGLFLEXT CHANGING IT_SALDO_FAGL.

      IT_MOEDA-TP_MOEDA = 'K'.
      PERFORM ORGANIZA_VALORES_FAGLFLEXT USING IT_MOEDA WA_FAGLFLEXT CHANGING IT_SALDO_FAGL_2.

      IT_MOEDA-TP_MOEDA = 'O'.
      PERFORM ORGANIZA_VALORES_FAGLFLEXT USING IT_MOEDA WA_FAGLFLEXT CHANGING IT_SALDO_FAGL_3.

      COLLECT IT_SALDO_FAGL.
      COLLECT IT_SALDO_FAGL_2.
      COLLECT IT_SALDO_FAGL_3.
    ENDLOOP.

    LOOP AT IT_SALDO_FAGL.
      APPEND IT_SALDO_FAGL TO IT_SALDOS.
    ENDLOOP.

    LOOP AT IT_SALDO_FAGL_2.
      APPEND IT_SALDO_FAGL_2 TO IT_SALDOS_2.
    ENDLOOP.

    LOOP AT IT_SALDO_FAGL_3.
      APPEND IT_SALDO_FAGL_3 TO IT_SALDOS_3.
    ENDLOOP.

  ENDIF.
  "****************************************************************************************************************

ENDFUNCTION.
