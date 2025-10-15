*&---------------------------------------------------------------------*
*&  Include           ZPM_APONT_MEDIC_EQUIPAM_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_CRIAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_CRIAR.
*

  CHECK NOT LV_OBJNR IS INITIAL AND
        NOT V_EQUNR IS INITIAL.
*
  PERFORM ZF_STATUS_NOTA.
  REFRESH: IT_REG, IT_DIMPT, IT_RET_DI.

  SELECT *"OBJNR EQUNR
  FROM EQUI
  INTO CORRESPONDING FIELDS OF TABLE IT_EQUI
  WHERE EQUNR EQ V_EQUNR.

  CHECK IT_EQUI[] IS NOT INITIAL.
  SELECT *
   FROM IMPTT
   INTO CORRESPONDING FIELDS OF TABLE IT_IMPTT
   FOR ALL ENTRIES IN IT_EQUI
    WHERE MPOBJ EQ IT_EQUI-OBJNR.

  SELECT *
  FROM CABN
  INTO CORRESPONDING FIELDS OF TABLE IT_CABN
  FOR ALL ENTRIES IN IT_IMPTT
  WHERE ATINN EQ IT_IMPTT-ATINN.

  SELECT *
  FROM T006A
  INTO CORRESPONDING FIELDS OF TABLE IT_T006A
  FOR ALL ENTRIES IN IT_IMPTT
  WHERE MSEHI EQ IT_IMPTT-MRNGU.


*
*  CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
*    EXPORTING
*      I_EQUNR    = V_EQUNR
*    TABLES
*      ET_RETURN1 = IT_RET_DI
*      ET_DIIMPT  = IT_DIMPT.

*  CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
*    EXPORTING
*      I_EQUNR    = V_EQUNR
*    TABLES
*      ET_RETURN1 = IT_RET_DI
*      ET_DIIMPT  = IT_IMPTT.


*
  LOOP AT IT_IMPTT.
    CLEAR IT_REG.


    IF IT_IMPTT-INACT = LC_X.
      CONTINUE.
    ENDIF.
*
    IT_REG-POINT = |{ IT_IMPTT-POINT ALPHA = OUT }|.
    IT_REG-PTTXT = IT_IMPTT-PTTXT.
    IT_REG-LOCAS = IT_IMPTT-LOCAS.

    READ TABLE IT_CABN WITH KEY ATINN = IT_IMPTT-ATINN.
    IF SY-SUBRC = 0.
      IT_REG-ATNAM = IT_CABN-ATNAM.
    ENDIF.

    IF IT_IMPTT-MPTYP = LC_P.
      CLEAR LV_VALOR.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          CHAR_UNIT       = IT_IMPTT-MRNGU
          DECIMALS        = IT_IMPTT-DECIM
          EXPONENT        = 0
          FLTP_VALUE_SI   = IT_IMPTT-MRMIN
          INDICATOR_VALUE = 'X'
          MASC_SYMBOL     = ' '
        IMPORTING
          CHAR_VALUE      = LV_VALOR.
      CONDENSE LV_VALOR.

      IT_REG-MRMIC = LV_VALOR.

      CLEAR LV_VALOR.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          CHAR_UNIT       = IT_IMPTT-MRNGU
          DECIMALS        = IT_IMPTT-DECIM "3
          EXPONENT        = 0
          FLTP_VALUE_SI   = IT_IMPTT-MRMAX
          INDICATOR_VALUE = 'X'
          MASC_SYMBOL     = ' '
        IMPORTING
          CHAR_VALUE      = LV_VALOR.
      CONDENSE LV_VALOR.
      IT_REG-MRMAC = LV_VALOR.


      READ TABLE IT_T006A WITH KEY MSEHI = IT_IMPTT-MRNGU.
      IF SY-SUBRC = 0.
        IT_REG-MRNGU = IT_T006A-MSEH3.
      ENDIF.
*
*  Verificar se existe alguma medição válida para o ponto a ser apontado
      CLEAR LV_CONT.
      CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST'
        EXPORTING
          BUFFER_BYPASS  = ' '
          DYFIELD        = ' '
          OFFSET_DATE    = SY-DATUM
          OFFSET_TIME    = SY-UZEIT
          POINT          = IT_IMPTT-POINT
        IMPORTING
          IMRG_WA        = WA_VALUE
        EXCEPTIONS
          IMRG_NOT_FOUND = 1
          OTHERS         = 2.
      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
          EXPORTING
            CHAR_UNIT       = WA_VALUE-RECDU
            DECIMALS        = IT_IMPTT-DECIM "3
            EXPONENT        = 0
            FLTP_VALUE_SI   = WA_VALUE-READG
            INDICATOR_VALUE = LC_X
          IMPORTING
            CHAR_VALUE      = LV_CONT
          EXCEPTIONS
            NO_UNIT_GIVEN   = 01.
*        REPLACE ALL OCCURRENCES OF ',' IN LV_CONT WITH '.' .
        CONDENSE LV_CONT.
        MOVE LV_CONT TO IT_REG-RDANT.
*
      ENDIF.
      APPEND IT_REG.
      CLEAR IT_CABN.
    ENDIF.
  ENDLOOP.

  SORT IT_REG ASCENDING BY POINT.
*
  IF IT_REG[] IS INITIAL.
    CLEAR SY-UCOMM.
    CLEAR: MHIO-WARPL, MHIO-QMNUM, V_QMNUM,
       V_EQUNR, V_DESCRSAP, V_TIPOBJ.

    MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
    WITH 'Pontos de medição não encotrados para esta Nota'.
  ENDIF.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_STATUS_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_STATUS_NOTA .
*
  SELECT OBJNR STAT INTO TABLE IT_JEST
    FROM JEST
    WHERE OBJNR EQ LV_OBJNR
      AND INACT EQ ''.
  SORT IT_JEST ASCENDING BY OBJNR.
*
  READ TABLE IT_JEST WITH KEY OBJNR = LV_OBJNR
    BINARY SEARCH.
  IF SY-SUBRC IS INITIAL.
    IF IT_JEST-STAT EQ LC_I0068 OR    "MSPN
       IT_JEST-STAT EQ LC_I0070.      "MSPR
    ELSE.
      CLEAR SY-UCOMM.
      MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
        WITH 'Status da Nota deve ser MSPN ou MSPR'.
    ENDIF.
  ENDIF.

*  CLEAR LV_STATUS.
*  CALL FUNCTION 'STATUS_TEXT_EDIT'
*    EXPORTING
*      CLIENT      = SY-MANDT
*      OBJNR       = LV_OBJNR
*      ONLY_ACTIVE = 'X'
*      SPRAS       = SY-LANGU
**     BYPASS_BUFFER           = ' '
*    IMPORTING
*      LINE        = LV_STATUS.
*  IF LV_STATUS CS LC_MSPN OR
*     LV_STATUS CS LC_MSPR.
*  ELSE.
*    MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
*      WITH 'Status da Nota deve ser MSPN ou MSPR'.
** Implement suitable error handling here
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SALVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_SALVAR .

  DATA: ORDEM_ TYPE AUFNR.

*  SELECT ZVAL CONST
*    FROM ZTPARAM
*    INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
*    WHERE PARAM EQ LC_OS_PREDIT. " 'OS_PREDIT'
**
*  READ TABLE IT_PARAM INDEX 1.
*  IF NOT SY-SUBRC IS INITIAL.
*    MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
*    WITH 'Parâmetro não encontrado na tab. ZTPARAM:'
*         LC_OS_PREDIT.
*  ENDIF.

  LOOP AT IT_REG.
    CONDENSE IT_REG-RDATU.
    CONDENSE LV_VALUE.
    IF NOT IT_REG-RDATU IS INITIAL.
      LV_VALUE =  IT_REG-RDATU.
      CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
        EXPORTING
          MEASUREMENT_POINT    = IT_REG-POINT
          SECONDARY_INDEX      = ' '
          READING_DATE         = MHIO-ADDAT
          READING_TIME         = MHIO-ADTIME
          SHORT_TEXT           = ' '
          READER               = SY-UNAME
          ORIGIN_INDICATOR     = 'A'
          READING_AFTER_ACTION = ' '
          RECORDED_VALUE       = LV_VALUE " <- VALOR DIGITADO PELO USUÁRIO
          RECORDED_UNIT        = ' '
          DIFFERENCE_READING   = ' '
          CODE_VERSION         = ' '
          "USER_DATA                   = ' ' " VALOR PADRÃO?
          "CHECK_CUSTOM_DUPREC         = ' ' " VALOR PADRÃO?
          WITH_DIALOG_SCREEN   = ' '
          PREPARE_UPDATE       = ABAP_TRUE
          COMMIT_WORK          = ABAP_TRUE
          WAIT_AFTER_COMMIT    = ABAP_TRUE
          CREATE_NOTIFICATION  = ' ' " ABAP_TRUE
          "NOTIFICATION_TYPE           = ' ' LANÇANDO EXCEPTION = 15
          NOTIFICATION_PRIO    = ' '
        IMPORTING
          MEASUREMENT_DOCUMENT = LV_IMRC_MDOCM
        EXCEPTIONS
          NO_AUTHORITY         = 1
          POINT_NOT_FOUND      = 2
          INDEX_NOT_UNIQUE     = 3
          TYPE_NOT_FOUND       = 4
          POINT_LOCKED         = 5
          POINT_INACTIVE       = 6
          TIMESTAMP_IN_FUTURE  = 7
          TIMESTAMP_DUPREC     = 8
          UNIT_UNFIT           = 9
          VALUE_NOT_FLTP       = 10
          VALUE_OVERFLOW       = 11
          VALUE_UNFIT          = 12
          VALUE_MISSING        = 13
          CODE_NOT_FOUND       = 14
          NOTIF_TYPE_NOT_FOUND = 15
          NOTIF_PRIO_NOT_FOUND = 16
          NOTIF_GENER_PROBLEM  = 17
          UPDATE_FAILED        = 18
          INVALID_TIME         = 19
          INVALID_DATE         = 20
          OTHERS               = 21.
      IF NOT SY-SUBRC IS INITIAL.
        MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
          WITH 'Erro ao gravar o Point ' IT_REG-POINT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR VL_VAPLZ.

*  SELECT SINGLE VAPLZ
*    FROM VIAUFKST
*    INTO VL_VAPLZ
*    WHERE EQUNR EQ V_EQUNR.
  DATA: MARK TYPE C.
  CLEAR MARK.

  CONT = 0.
  LOOP AT IT_REG.
* Se o Apontamento não estiver entre o Minino e o Maximo gerará uma Ordem

    CONDENSE IT_REG-RDATU.
    IF IT_REG-RDATU > '0' OR IT_REG-RDATU > ''.
      IF NOT IT_REG-RDATU BETWEEN IT_REG-MRMIC AND IT_REG-MRMAC.
        IF MARK IS INITIAL.
          MARK = ABAP_TRUE.
          PERFORM ZF_CRIA_ORDEM CHANGING ORDEM_.
        ENDIF.
        PERFORM ZBAPI_ALM_ORDER_MAINTAIN USING ORDEM_.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Limpar campos.
  REFRESH: IT_REG, IT_NOTAS.
  PERFORM ZF_LIMPAR.
*
  SET SCREEN 100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CRIA_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_CRIA_ORDEM CHANGING P_ORDEM.
* Gravar Ordem de Manutenção pela Nota
  CALL SCREEN 0400 STARTING AT 8 8 ENDING AT 55 12.

  PERFORM ZF_FILL_BDCDATA_IW34 USING TP_ORDEM.
  PERFORM ZF_CALL_TRANSACTION_IW34 CHANGING P_ORDEM.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_FILL_BDCDATA_IW34
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_FILL_BDCDATA_IW34 USING TP_ORDEM.
  REFRESH: T_BDCDATA.

* 1st. Screen
  PERFORM ZF_FILL_DYNPRO USING:
          'X' 'SAPLCOIH'         '0105',
          ' ' 'BDC_OKCODE'       '/00',
          ' ' 'AUFPAR-PM_AUFART' TP_ORDEM, "'ZPM3',
          ' ' 'CAUFVD-PRIOK'        '2',
          ' ' 'AFIH-QMNUM'       V_QMNUM,
          ' ' 'CAUFVD-IWERK'     V_IWERK.
* 2st. Screen
  PERFORM ZF_FILL_DYNPRO USING:
          'X' 'SAPLCOIH'         '3000',
          ' ' 'CAUFVD-VAPLZ'     IT_MPOS-ARBPL,
          ' ' 'CAUFVD-ILART'     'Z99', "IT_REG-LOCAS,
          ' ' 'AFVGD-STEUS'      'PM01',
          ' ' 'BDC_OKCODE'       '/00'.
*
  PERFORM ZF_FILL_DYNPRO USING:
          'X' 'SAPLCOIH'         '3000',
          ' ' 'BDC_OKCODE'       '=BU'.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION_IW34
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_CALL_TRANSACTION_IW34 CHANGING P_ORDEM.


  CLEAR T_MESSAGE.   REFRESH T_MESSAGE.
  CLEAR T_AC_MESSAGE.   REFRESH T_AC_MESSAGE.

  CALL TRANSACTION LC_IW34  USING    T_BDCDATA
                            MODE     LV_MODE
                            UPDATE   LC_S
                            MESSAGES INTO T_MESSAGE.
*
  READ TABLE T_MESSAGE WITH KEY MSGTYP = LC_S
                                MSGID  = LC_IW
                                MSGNR  = LC_085.
  IF NOT SY-SUBRC IS INITIAL.
    LOOP AT T_MESSAGE.
      CLEAR T_AC_MESSAGE.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = T_MESSAGE-MSGID
          MSGNR               = T_MESSAGE-MSGNR
          MSGV1               = T_MESSAGE-MSGV1
          MSGV2               = T_MESSAGE-MSGV2
          MSGV3               = T_MESSAGE-MSGV3
          MSGV4               = T_MESSAGE-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = T_AC_MESSAGE-TDLINE.
*
      MESSAGE I000(ZPPM001) WITH T_AC_MESSAGE-TDLINE.
    ENDLOOP.
    CLEAR SY-UCOMM.
    MESSAGE E000(ZPPM001) WITH 'Erro na criação da ordem'.
  ELSE.
    P_ORDEM = T_MESSAGE[
                        MSGTYP = LC_S
                        MSGID  = LC_IW
                        MSGNR  = LC_085
                      ]-MSGV1.

    MESSAGE I000(ZPPM001) WITH 'Ordem criada:'
            T_MESSAGE-MSGV1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_FILL_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0426   text
*      -->P_0427   text
*      -->P_0428   text
*----------------------------------------------------------------------*
FORM ZF_FILL_DYNPRO  USING    PL_DYNBEGIN PL_NOMBRE PL_VALOR.
  CLEAR T_BDCDATA.
  IF PL_DYNBEGIN = 'X'.
    T_BDCDATA-DYNBEGIN = PL_DYNBEGIN.
    T_BDCDATA-PROGRAM  = PL_NOMBRE.
    T_BDCDATA-DYNPRO   = PL_VALOR.
  ELSE.
    T_BDCDATA-FNAM = PL_NOMBRE.
    T_BDCDATA-FVAL = PL_VALOR.
  ENDIF.
  APPEND T_BDCDATA.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_LIMPAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_LIMPAR .
*
  REFRESH: IT_REG, IT_NOTAS.

  CLEAR: MHIO-WARPL, MHIO-QMNUM, V_QMNUM,
         V_EQUNR, V_DESCRSAP, V_TIPOBJ.

  SET PARAMETER ID: 'MPL' FIELD MHIO-WARPL,
                  'IQM' FIELD MHIO-QMNUM.

  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'MHIO-WARPL'.
        SCREEN-INPUT = 1.
      WHEN 'MHIO-QMNUM'.
        SCREEN-INPUT = 1.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
**
* SET SCREEN 100.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BAPI_ALM_ORDER_MAINTAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZBAPI_ALM_ORDER_MAINTAIN USING P_ORDEM.

  CLEAR: IT_METHODS, IT_HEADER, IT_OPERATION, IT_OPERATION_UP,
         IT_RETURN, WA_RETURN.

  DATA: LV_ORDERID      TYPE AUFNR,
        LV_REFNUM       TYPE IFREFNUM,
        LV_OPER_NO      TYPE OBJIDEXT,
        NUMERO_ORDEM_V2 TYPE CHAR10,
        TXT_OPER        TYPE CHAR40.

  DATA: LV_ORDERID_      TYPE AUFNR,
        LV_REFNUM_       TYPE IFREFNUM,
        LV_OPER_NO_      TYPE OBJIDEXT,
        NUMERO_ORDEM_V2_ TYPE CHAR10.

  LV_ORDERID = P_ORDEM.
  LV_REFNUM  = 1.
  ADD 10 TO CONT.

  SHIFT LV_ORDERID RIGHT DELETING TRAILING SPACE.
  TRANSLATE LV_ORDERID USING ' 0'.

  CLEAR TXT_OPER.
  LV_OPER_NO = LV_ORDERID.
  LV_OPER_NO+12(4) = CONT.
  TXT_OPER = |{ 'VERIF'} { IT_REG-ATNAM } { IT_REG-PTTXT }|.

  IT_METHODS = VALUE #(
  ( REFNUMBER  = LV_REFNUM
    OBJECTTYPE = 'OPERATION'
    METHOD = COND #( WHEN CONT EQ 0010 THEN 'CHANGE' ELSE 'CREATE' )
    OBJECTKEY  = COND #( WHEN CONT EQ 0010 THEN LV_OPER_NO ELSE LV_ORDERID )
  )
  ( REFNUMBER  = ABAP_FALSE
    OBJECTTYPE = ABAP_FALSE
    METHOD = 'SAVE'
    OBJECTKEY  = LV_ORDERID
  )
).

  IT_OPERATION    = VALUE #( ( ACTIVITY    = CONT
                               CONTROL_KEY = 'PM01'
                               DESCRIPTION =  TXT_OPER ) ).
  IT_OPERATION_UP = VALUE #( ( ACTIVITY    = COND #( WHEN CONT EQ 0010 THEN ABAP_TRUE ELSE ABAP_FALSE )
                               DESCRIPTION = COND #( WHEN CONT EQ 0010 THEN ABAP_TRUE ELSE ABAP_FALSE ) ) ).

  "Campos de número de material não utilizados. Pseudo comentário acrescentado.[2438131]  " >> ---> S4 Migration - 07/07/2023 - RZ
  "Campos de Lista de Objetos não utilizados  . Pseudo comentário acrescentado.[2669857]  " >> ---> S4 Migration - 07/07/2023 - RZ
  CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'                       "#EC CI_USAGE_OK[2438131] " >> ---> S4 Migration - 07/07/2023 - RZ
    TABLES                                                      "#EC CI_USAGE_OK[2669857] " >> ---> S4 Migration - 07/07/2023 - RZ
      IT_METHODS      = IT_METHODS
      IT_OPERATION    = IT_OPERATION
      IT_OPERATION_UP = IT_OPERATION_UP
      RETURN          = IT_RETURN_.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = ABAP_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0400 OUTPUT.
  SET PF-STATUS 'ZPP001'.
  SET TITLEBAR  'ZPP002'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0400 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

    WHEN 'V_CONFIRMA'.
      PERFORM VALIDA_ORDEM.


    WHEN 'V_CANCELAR'.
      CLEAR V_TIPO_ORDEM.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  VALIDA_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VALIDA_ORDEM .

  IF V_TIPO_ORDEM IS INITIAL.
    MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.

    FREE IT_ZTPARAM.
    SELECT *
    FROM ZTPARAM
    INTO CORRESPONDING FIELDS OF TABLE IT_ZTPARAM
    WHERE PARAM EQ 'OR_PREDIT'.

    FREE IT_ORDEM_PREDIT[].
    LOOP AT IT_ZTPARAM INTO WA_ZTPARAM.
      IT_ORDEM_PREDIT-PARAM = WA_ZTPARAM-PARAM.
      IT_ORDEM_PREDIT-WERKS = WA_ZTPARAM-CONST.
      IT_ORDEM_PREDIT-AUART = WA_ZTPARAM-ZVAL.
      APPEND IT_ORDEM_PREDIT.
      CLEAR IT_ORDEM_PREDIT.
      CLEAR WA_ZTPARAM.
    ENDLOOP.


    LOOP AT IT_ORDEM_PREDIT WHERE AUART = V_TIPO_ORDEM
                             AND  WERKS = V_IWERK.
      IF SY-SUBRC = 0.
        CLEAR TP_ORDEM.
        TP_ORDEM = V_TIPO_ORDEM.
        "Validar tipo de ordem.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDLOOP.

    IF TP_ORDEM IS INITIAL.
      MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_ZVAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUSCA_ZVAL INPUT.


  FREE IT_ZTPARAM.
  SELECT *
  FROM ZTPARAM
  INTO CORRESPONDING FIELDS OF TABLE IT_ZTPARAM
  WHERE PARAM EQ 'OR_PREDIT'.

  FREE IT_ORDEM_PREDIT[].
  LOOP AT IT_ZTPARAM INTO WA_ZTPARAM.
    IT_ORDEM_PREDIT-PARAM = WA_ZTPARAM-PARAM.
    IT_ORDEM_PREDIT-WERKS = WA_ZTPARAM-CONST.
    IT_ORDEM_PREDIT-AUART = WA_ZTPARAM-ZVAL.
    APPEND IT_ORDEM_PREDIT.
    CLEAR IT_ORDEM_PREDIT.
    CLEAR WA_ZTPARAM.
  ENDLOOP.

  FREE IT_TP_ORDER[].
  LOOP AT IT_ORDEM_PREDIT WHERE WERKS = V_IWERK.

    MOVE-CORRESPONDING IT_ORDEM_PREDIT TO IT_TP_ORDER.
    APPEND IT_TP_ORDER.
    CLEAR IT_TP_ORDER.
    CLEAR IT_ORDEM_PREDIT.
  ENDLOOP.

*  SELECT *
*  FROM V_AUART
*  INTO TABLE IT_V_AUART
*  FOR ALL ENTRIES IN IT_TP_ORDER
*  WHERE AUART EQ IT_TP_ORDER-AUART
*    AND AUTYP EQ '30'
*    AND SPRAS EQ 'P'.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'AUART'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'AUART'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_TP_ORDER
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

ENDMODULE.
