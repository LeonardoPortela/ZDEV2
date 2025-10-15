*&---------------------------------------------------------------------*
*& Report  ZPMR0010
*&
*&---------------------------------------------------------------------*
*&  Serviço de atualização da atividade manual
*&  Analista: Cleudo Ferreira
*&  ABAP    : Marcos Faneli
*&---------------------------------------------------------------------*

REPORT  ZPMR0010.

** Types
TYPES: BEGIN OF TY_EQUIPAMENTO,
         EQUNR TYPE EQUZ-EQUNR,
         EQTYP TYPE EQUI-EQTYP,
         EQART TYPE EQUI-EQART,
         HERST TYPE EQUI-HERST,
         TYPBZ TYPE EQUI-TYPBZ,
         OBJNR TYPE EQUI-OBJNR,
       END OF TY_EQUIPAMENTO.

** SHDB
DATA: GT_BDCDATA    TYPE STANDARD TABLE OF BDCDATA,
      GW_BDCDATA    LIKE LINE OF GT_BDCDATA.

DEFINE F_PREENCHE_SHDB.
  CLEAR GW_BDCDATA.
  GW_BDCDATA-PROGRAM   = &1.
  GW_BDCDATA-DYNPRO    = &2.
  GW_BDCDATA-DYNBEGIN  = &3.
  GW_BDCDATA-FNAM      = &4.
  GW_BDCDATA-FVAL      = &5.
  APPEND GW_BDCDATA TO GT_BDCDATA.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_equipamento DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EQUIPAMENTO DEFINITION.
  PUBLIC SECTION.
    METHODS:
      CONSTRUCTOR,
      CARREGA_EQUIPAMENTOS IMPORTING P_EQUNR TYPE EQUI-EQUNR OPTIONAL,
      BUSCAR_EQUIPAMENTOS  IMPORTING P_EQUNR TYPE EQUI-EQUNR,
      BUSCAR_PONTOS,
      ULTIMA_POSICAO,
      BUSCAR_APONTAMENTOS,
      CALCULAR_ATV_ANUAL,
      ATUALIZAR_ATV_ANUAL,
      GERA_DOC_MEDICAO.

** Tabelas
    DATA: GT_EQUIPAMENTOS TYPE TABLE OF TY_EQUIPAMENTO,
          GT_DIIMPT       TYPE TABLE OF DIIMPT.

** Work Áreas
    DATA: GW_EQUIPAMENTOS TYPE TY_EQUIPAMENTO,
          GW_IMRG         TYPE IMRG.

** Variáveis
    DATA: GV_EQUNR        TYPE EQUI-EQUNR,
          GV_POINT_PRINC  TYPE DIIMPT-POINT,
          GV_POINT_UTIL   TYPE DIIMPT-POINT,
          GV_ULT_MED_DAT  TYPE IMRG-IDATE,
          GV_ULT_MEDICAO  TYPE DEC_16_02_S,
          GV_DOCU         TYPE IMRG-MDOCM,
          GV_VALOR        TYPE DEC_16_02_S,
          GV_ATV_ANUAL    TYPE DEC_16_02_S,
**  Parâmetros
          GV_PER_ATIV     TYPE I,
          GV_PER_MEDI     TYPE I,

          GV_SUBRC        TYPE I.

ENDCLASS.                    "lcl_equipamento DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_equipamento IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EQUIPAMENTO IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    DATA: TL_PARAM TYPE TABLE OF ZTPARAM,
          WL_PARAM TYPE ZTPARAM.

    SELECT MANDT PARAM CONST ZVAL
      FROM ZTPARAM
      INTO TABLE TL_PARAM
     WHERE PARAM = 'PM_PERI_AT'.

    IF SY-SUBRC IS NOT INITIAL.
      WL_PARAM-MANDT = SY-MANDT.
      WL_PARAM-PARAM = 'PM_PERI_AT'.
      WL_PARAM-CONST = 'ATIVIDADE_ANUAL'.
      WL_PARAM-ZVAL  = '90'.
      APPEND WL_PARAM TO TL_PARAM.

      CLEAR WL_PARAM.

      WL_PARAM-MANDT = SY-MANDT.
      WL_PARAM-PARAM = 'PM_PERI_AT'.
      WL_PARAM-CONST = 'ULT_DOCUMENTO'.
      WL_PARAM-ZVAL  = '5'.
      APPEND WL_PARAM TO TL_PARAM.

      INSERT ZTPARAM FROM TABLE TL_PARAM.
    ENDIF.

    SORT TL_PARAM BY CONST.

    LOOP AT TL_PARAM INTO WL_PARAM.
      CASE WL_PARAM-CONST.
        WHEN 'ATIVIDADE_ANUAL'.
          MOVE WL_PARAM-ZVAL TO GV_PER_ATIV.
        WHEN 'ULT_DOCUMENTO'.
          MOVE WL_PARAM-ZVAL TO GV_PER_MEDI.
      ENDCASE.
    ENDLOOP.

    IF GV_PER_ATIV IS INITIAL
    OR GV_PER_MEDI IS INITIAL.
      WRITE: / 'Definir valores para os parâmetros "PM_PERI_AT" em ZPM0002.'.
      GV_SUBRC = 1.
    ENDIF.

  ENDMETHOD.                    "constructor

  METHOD CARREGA_EQUIPAMENTOS.
    DATA: TL_EQUIP_STATUS TYPE TABLE OF BAPI_ITOB_STATUS,
          WL_EQUIP_STATUS TYPE BAPI_ITOB_STATUS,

          TL_USER_STATUS  TYPE TABLE OF BAPI_ITOB_STATUS,

          WL_RETURN       TYPE BAPIRET2,

          LV_EQUIPAMENTO  TYPE EQUI-EQUNR.

    CHECK GV_SUBRC IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = P_EQUNR
      IMPORTING
        OUTPUT = LV_EQUIPAMENTO.

    FIELD-SYMBOLS: <FS_EQUIP> TYPE TY_EQUIPAMENTO.

    IF LV_EQUIPAMENTO IS INITIAL.
      SELECT EQUNR EQTYP EQART HERST TYPBZ OBJNR
        FROM EQUI
        INTO CORRESPONDING FIELDS OF TABLE GT_EQUIPAMENTOS
       WHERE EQTYP EQ 'V'.

    ELSE.
      SELECT EQUNR EQTYP EQART HERST TYPBZ OBJNR
        FROM EQUI
        INTO CORRESPONDING FIELDS OF TABLE GT_EQUIPAMENTOS
       WHERE EQTYP EQ 'V'
         AND EQUNR EQ LV_EQUIPAMENTO.

    ENDIF.

* Determina status do equipamento
    LOOP AT GT_EQUIPAMENTOS ASSIGNING <FS_EQUIP>.

      GV_EQUNR = <FS_EQUIP>-EQUNR.

      CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
        EXPORTING
          EQUIPMENT     = <FS_EQUIP>-EQUNR
        IMPORTING
          RETURN        = WL_RETURN
        TABLES
          SYSTEM_STATUS = TL_EQUIP_STATUS
          USER_STATUS   = TL_USER_STATUS.

** Remove status Eliminado
      READ TABLE TL_EQUIP_STATUS INTO WL_EQUIP_STATUS WITH KEY STATUS = 'I0076'.
      IF SY-SUBRC IS INITIAL.
        DELETE GT_EQUIPAMENTOS WHERE EQUNR = <FS_EQUIP>-EQUNR.
        CONTINUE.
      ENDIF.
** Remove status Inativo
      READ TABLE TL_EQUIP_STATUS INTO WL_EQUIP_STATUS WITH KEY STATUS = 'I0320'.
      IF SY-SUBRC IS INITIAL.
        DELETE GT_EQUIPAMENTOS WHERE EQUNR = <FS_EQUIP>-EQUNR.
        CONTINUE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "listar

  METHOD BUSCAR_PONTOS.
    DATA: WL_DIIMPT TYPE DIIMPT.

    FREE ME->GT_DIIMPT.

    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        I_EQUNR   = ME->GV_EQUNR
      TABLES
        ET_DIIMPT = GT_DIIMPT.

    SORT GT_DIIMPT BY: ATNAM, INACT, INDTR.

    DELETE GT_DIIMPT WHERE ATNAM NE 'ODOMETRO'
                      AND  ATNAM NE 'HORIMETRO'.

    READ TABLE ME->GT_DIIMPT INTO WL_DIIMPT WITH KEY INDTR = ''.
    ME->GV_POINT_PRINC = WL_DIIMPT-POINT.

    READ TABLE ME->GT_DIIMPT INTO WL_DIIMPT WITH KEY INDTR = 'X'.
    ME->GV_POINT_UTIL = WL_DIIMPT-POINT.

  ENDMETHOD.                    "buscar_pontos

  METHOD ULTIMA_POSICAO.
    DATA LV_TEMP TYPE IMRC_TOTAC.

    CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST_BUF'
      EXPORTING
        POINT          = ME->GV_POINT_PRINC
      IMPORTING
        IMRG_WA        = ME->GW_IMRG
      EXCEPTIONS
        IMRG_NOT_FOUND = 1
        OTHERS         = 2.

    IF  ME->GW_IMRG IS NOT INITIAL
    AND ME->GW_IMRG-CANCL IS INITIAL.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          CHAR_UNIT       = ME->GW_IMRG-RECDU
          DECIMALS        = 2
          EXPONENT        = 0
          FLTP_VALUE_SI   = ME->GW_IMRG-CNTRR
          INDICATOR_VALUE = 'X'
          MASC_SYMBOL     = ' '
        IMPORTING
          CHAR_VALUE      = LV_TEMP.

      TRANSLATE LV_TEMP USING ',.'.
      CONDENSE LV_TEMP NO-GAPS.

      MOVE: LV_TEMP       TO ME->GV_ULT_MEDICAO,
            GW_IMRG-IDATE TO ME->GV_ULT_MED_DAT.
    ENDIF.

  ENDMETHOD.                    "ULTIMA_POSICAO

  METHOD BUSCAR_APONTAMENTOS.
    DATA: WL_IMPT   TYPE IMPT,

          TL_IMRG   TYPE TABLE OF IMRG,
          WL_IMRG   TYPE IMRG,

          LV_DATA  TYPE SY-DATUM,
          LV_CDIFF TYPE IMRC_TOTAC,
          LV_TEMP  TYPE IMRC_TOTAC,
          LV_VALOR TYPE DEC_16_02_S,
          LV_UNID  TYPE RIMR0-UNITS.

    CHECK ME->GV_POINT_PRINC IS NOT INITIAL.

    LV_DATA = SY-DATUM.
    SUBTRACT GV_PER_ATIV FROM LV_DATA.

    CALL FUNCTION 'MEASUREM_DOCUM_READ_INTERVAL'
      EXPORTING
        POINT            = ME->GV_POINT_UTIL
        DATE_HIGH        = SY-DATUM
        TIME_HIGH        = SY-UZEIT
        DATE_LOW         = LV_DATA
*        TIME_LOW         = '00:00:00'
        LIMIT_FOR_NUMBER = 999999
      IMPORTING
        IMPT_WA          = WL_IMPT
        SI_UNIT          = LV_UNID
      TABLES
        IMRG_TAB         = TL_IMRG
      EXCEPTIONS
        IMPTT_NOT_FOUND  = 1.

    SORT TL_IMRG BY IDATE ITIME.

    LOOP AT TL_IMRG INTO WL_IMRG WHERE CANCL NE 'X'.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          CHAR_UNIT       = WL_IMRG-RECDU
          DECIMALS        = 2
          EXPONENT        = 0
          FLTP_VALUE_SI   = WL_IMRG-CDIFF
          INDICATOR_VALUE = 'X'
          MASC_SYMBOL     = ' '
        IMPORTING
          CHAR_VALUE      = LV_TEMP.

      TRANSLATE LV_TEMP USING ',.'.
      CONDENSE LV_TEMP NO-GAPS.

      ADD LV_TEMP TO LV_CDIFF.

    ENDLOOP.

    MOVE LV_CDIFF TO ME->GV_VALOR.

  ENDMETHOD.                    "BUSCAR_APONTAMENTOS

  METHOD CALCULAR_ATV_ANUAL.
    ME->GV_ATV_ANUAL = ME->GV_VALOR / ME->GV_PER_ATIV * 365.
  ENDMETHOD.                    "CALCULAR_ATV_ANUAL

  METHOD ATUALIZAR_ATV_ANUAL.
    DATA: LV_VALOR TYPE C LENGTH 18.

    IF ME->GV_ATV_ANUAL IS NOT INITIAL.
      MOVE ME->GV_ATV_ANUAL TO LV_VALOR.
      TRANSLATE LV_VALOR USING '.,'.
      CONDENSE LV_VALOR NO-GAPS.

      FREE GT_BDCDATA.

      F_PREENCHE_SHDB:
          '        ' '0000' 'T' 'IK02        ' '                                                 ',
          'SAPLIMR0' '1110' 'X' '            ' '                                                 ',
          '        ' '0000' ' ' 'BDC_CURSOR  ' 'IMPT-POINT                                       ',
          '        ' '0000' ' ' 'BDC_OKCODE  ' '/00                                              ',
          '        ' '0000' ' ' 'IMPT-POINT  ' ME->GV_POINT_PRINC                                 ,
          '        ' '0000' ' ' 'BDC_SUBSCR  ' 'SAPLIMR4                                7500MPOBJ',
          'SAPLIMR0' '5110' 'X' '            ' '                                                 ',
          '        ' '0000' ' ' 'BDC_CURSOR  ' 'RIMR0-PYEAC                                      ',
          '        ' '0000' ' ' 'BDC_OKCODE  ' '/00                                              ',
          '        ' '0000' ' ' 'RIMR0-PYEAC ' LV_VALOR                                           ,
          'SAPLIMR0' '5110' 'X' '            ' '                                                 ',
          '        ' '0000' ' ' 'BDC_CURSOR  ' 'IMPT-PSORT                                       ',
          '        ' '0000' ' ' 'BDC_OKCODE  ' '=BU                                              '.

      CALL TRANSACTION 'IK02' USING GT_BDCDATA
            MODE 'E'
            UPDATE 'S'.

      FREE GT_BDCDATA.

      F_PREENCHE_SHDB:
          '        ' '0000' 'T' 'IK02        ' '                                                 ',
          'SAPLIMR0' '1110' 'X' '            ' '                                                 ',
          '        ' '0000' ' ' 'BDC_CURSOR  ' 'IMPT-POINT                                       ',
          '        ' '0000' ' ' 'BDC_OKCODE  ' '/00                                              ',
          '        ' '0000' ' ' 'IMPT-POINT  ' ME->GV_POINT_UTIL                                  ,
          '        ' '0000' ' ' 'BDC_SUBSCR  ' 'SAPLIMR4                                7500MPOBJ',
          'SAPLIMR0' '5110' 'X' '            ' '                                                 ',
          '        ' '0000' ' ' 'BDC_CURSOR  ' 'RIMR0-PYEAC                                      ',
          '        ' '0000' ' ' 'BDC_OKCODE  ' '/00                                              ',
          '        ' '0000' ' ' 'RIMR0-PYEAC ' LV_VALOR                                           ,
          'SAPLIMR0' '5110' 'X' '            ' '                                                 ',
          '        ' '0000' ' ' 'BDC_CURSOR  ' 'IMPT-PSORT                                       ',
          '        ' '0000' ' ' 'BDC_OKCODE  ' '=BU                                              '.

      CALL TRANSACTION 'IK02' USING GT_BDCDATA
            MODE 'E'
            UPDATE 'S'.

    ENDIF.

  ENDMETHOD.                    "atualizar_atv_anual

  METHOD GERA_DOC_MEDICAO.
    DATA LV_VALOR TYPE RIMR0-RECDC.

    MOVE GV_ULT_MEDICAO TO LV_VALOR.
    TRANSLATE LV_VALOR USING '.,'.
    CONDENSE LV_VALOR NO-GAPS.

    CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
      EXPORTING
        MEASUREMENT_POINT    = ME->GV_POINT_PRINC
        SECONDARY_INDEX      = ' '
        READING_DATE         = SY-DATUM
        READING_TIME         = SY-UZEIT
        SHORT_TEXT           = ' '
        READER               = SY-UNAME
        ORIGIN_INDICATOR     = ' '
        READING_AFTER_ACTION = ' '
        RECORDED_VALUE       = LV_VALOR
        RECORDED_UNIT        = ' '
        DIFFERENCE_READING   = ' '
        CODE_VERSION         = ' '
        USER_DATA            = ' '
        CHECK_CUSTOM_DUPREC  = ' '
        WITH_DIALOG_SCREEN   = ' '
        PREPARE_UPDATE       = 'X'
        COMMIT_WORK          = 'X'
        WAIT_AFTER_COMMIT    = 'X'
        CREATE_NOTIFICATION  = ' '
        NOTIFICATION_TYPE    = ' '
        NOTIFICATION_PRIO    = ' '
      IMPORTING
        MEASUREMENT_DOCUMENT = GV_DOCU
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

  ENDMETHOD.                    "gera_doc_medicao

  METHOD BUSCAR_EQUIPAMENTOS.
    FREE: GT_DIIMPT, GW_IMRG.

    CLEAR: ME->GV_EQUNR,
           ME->GV_POINT_PRINC,
           ME->GV_POINT_UTIL,
           ME->GV_VALOR,
           ME->GV_ULT_MEDICAO,
           ME->GV_DOCU.

    READ TABLE GT_EQUIPAMENTOS INTO GW_EQUIPAMENTOS WITH KEY EQUNR = P_EQUNR.
    IF SY-SUBRC IS INITIAL.
      ME->GV_EQUNR = GW_EQUIPAMENTOS-EQUNR.
      ME->BUSCAR_PONTOS( ).
      ME->BUSCAR_APONTAMENTOS( ).
      ME->CALCULAR_ATV_ANUAL( ).
    ENDIF.

  ENDMETHOD.                    "buscar

ENDCLASS.                    "lcl_equipamento IMPLEMENTATION

DATA: OBJ_EQUIPAMENTO TYPE REF TO LCL_EQUIPAMENTO.

FIELD-SYMBOLS: <FS_EQUIP> TYPE TY_EQUIPAMENTO.

START-OF-SELECTION.
  SELECTION-SCREEN: BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
  PARAMETER: P_EQUI TYPE V_EQUI-EQUNR.
  SELECTION-SCREEN:END OF BLOCK A1.

  DATA: LV_DATA_LIMIT TYPE SY-DATUM.

  CREATE OBJECT OBJ_EQUIPAMENTO.
  OBJ_EQUIPAMENTO->CARREGA_EQUIPAMENTOS( P_EQUI ).

  LOOP AT OBJ_EQUIPAMENTO->GT_EQUIPAMENTOS ASSIGNING <FS_EQUIP>.
    OBJ_EQUIPAMENTO->BUSCAR_EQUIPAMENTOS( <FS_EQUIP>-EQUNR ).
    IF OBJ_EQUIPAMENTO->GV_ATV_ANUAL > 0.
      WRITE: / OBJ_EQUIPAMENTO->GV_EQUNR,
               OBJ_EQUIPAMENTO->GV_POINT_PRINC,
               OBJ_EQUIPAMENTO->GV_POINT_UTIL,
               OBJ_EQUIPAMENTO->GV_VALOR,
               OBJ_EQUIPAMENTO->GV_ATV_ANUAL.

      OBJ_EQUIPAMENTO->ATUALIZAR_ATV_ANUAL( ).
      WRITE: / 'Atividade anual atualizada para equipamento ', OBJ_EQUIPAMENTO->GV_EQUNR.
    ENDIF.
    OBJ_EQUIPAMENTO->ULTIMA_POSICAO( ).
    IF OBJ_EQUIPAMENTO->GW_IMRG IS INITIAL.
      WRITE: / 'Equipamento ', OBJ_EQUIPAMENTO->GV_EQUNR, ' não possui ponto de medição válido. '.
    ELSE.
      LV_DATA_LIMIT = SY-DATUM.
      SUBTRACT OBJ_EQUIPAMENTO->GV_PER_MEDI FROM LV_DATA_LIMIT.
      IF OBJ_EQUIPAMENTO->GW_IMRG-IDATE LT LV_DATA_LIMIT.
        OBJ_EQUIPAMENTO->GERA_DOC_MEDICAO( ).
        IF OBJ_EQUIPAMENTO->GV_DOCU IS INITIAL.
          WRITE: / 'Não foi possível atualizar posição do equipamento ', OBJ_EQUIPAMENTO->GV_EQUNR.
        ELSE.
          WRITE: / 'Última posição do equipamento ', OBJ_EQUIPAMENTO->GV_EQUNR, ':', OBJ_EQUIPAMENTO->GW_IMRG-MDOCM, '-> Doc. gerado:',OBJ_EQUIPAMENTO->GV_DOCU.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF SY-SUBRC IS NOT INITIAL.
    WRITE / 'Nenhum equipamento foi atualizado.'.
  ENDIF.
