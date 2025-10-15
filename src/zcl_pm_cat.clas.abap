class ZCL_PM_CAT definition
  public
  final
  create public .

*"* public components of class ZCL_PM_CAT
*"* do not include other source files here!!!
public section.

  class-methods APONTAMENTO_MEDICAO
    importing
      !IV_FLOAT_1 type F optional
      !IV_FLOAT_2 type F optional
    returning
      value(FLOAT_1_FLOAT_2) type CHAR1 .
  methods GET_NOTA
    returning
      value(E_NOTA) type VIQMEL-QMNUM .
  methods SET_NOTA
    importing
      !I_NOTA type VIQMEL-QMNUM .
  methods CONTADOR
    importing
      !I_ADD type INT1 optional
    returning
      value(I_CONT) type INT1 .
  methods GET_MEDIDOR
    exporting
      !E_MEDIDOR2 type F
      !E_MEDIDOR1 type F .
  methods SET_MEDIDOR
    importing
      !I_MEDIDOR1 type ANY
      !I_MEDIDOR2 type ANY .
  PROTECTED SECTION.
*"* protected components of class ZCL_PM_CAT
*"* do not include other source files here!!!
private section.


  class-data AT_NOTA type VIQMEL-QMNUM .
  class-data AT_CONT type INT1 .
  class-data AT_MEDIDOR1 type P LENGTH 13.
  class-data AT_MEDIDOR2 type P LENGTH 13.

  class-methods GERA_ORDEM .
*"* private components of class ZCL_PM_CAT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_PM_CAT IMPLEMENTATION.


  METHOD APONTAMENTO_MEDICAO.

        IF AT_MEDIDOR1 > AT_MEDIDOR2.
        GERA_ORDEM( ).

      ELSEIF
        FLOAT_1_FLOAT_2 = ''.
*         GERA_ORDEM( ).




*      IF LV_CHAR_1 EQ LV_CHAR_2.
*      FLOAT_1_FLOAT_2 = '0'.
**      GERA_ORDEM( ).
*
*     ELSE.
*        IF LV_CHAR_1 GT LV_CHAR_2.
*         FLOAT_1_FLOAT_2 = ''.
*         GERA_ORDEM( ).
*
*      ELSE.
*        FLOAT_1_FLOAT_2 = '1'.
*
*
*      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD CONTADOR.

    IF NOT I_ADD IS INITIAL.
      ADD I_ADD TO AT_CONT.
    ENDIF.

    I_CONT = AT_CONT.
  ENDMETHOD.


  METHOD GERA_ORDEM.

    DATA: PM_CAT TYPE REF TO ZCL_PM_CAT.
    CREATE OBJECT PM_CAT.

    DATA: LT_EQUZ   TYPE TABLE OF EQUZ,
          LT_VIQMEL TYPE TABLE OF DIQMEL,
          WOOBK     TYPE RIMR0-WOOBK,
          QMNUM     TYPE VIQMEL-QMNUM,
          GT_BDC    TYPE TABLE OF BDCDATA,
          GW_BDC    TYPE BDCDATA.

    DATA OPT TYPE CTU_PARAMS.

    QMNUM = PM_CAT->GET_NOTA( ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = QMNUM
      IMPORTING
        OUTPUT = QMNUM.

    SELECT * FROM VIQMEL
      INTO CORRESPONDING FIELDS OF TABLE LT_VIQMEL
      WHERE QMNUM EQ QMNUM.

    SELECT * FROM EQUZ
      INTO TABLE LT_EQUZ
      FOR ALL ENTRIES IN LT_VIQMEL
      WHERE EQUNR EQ LT_VIQMEL-EQUNR.

    CHECK NOT LT_EQUZ IS INITIAL.

    TRY .
        DATA(LS_EQUZ) = LT_EQUZ[ EQUNR = LT_VIQMEL[ 1 ]-EQUNR ].
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
    ENDTRY.

    FREE GT_BDC.

    DEFINE SHDB.

      GW_BDC-DYNBEGIN = &1.
      GW_BDC-PROGRAM  = &2.
      GW_BDC-DYNPRO   = &3.
      GW_BDC-FNAM     = &4.
      GW_BDC-FVAL     = &5.

      APPEND GW_BDC TO GT_BDC.
      CLEAR: GW_BDC.
    END-OF-DEFINITION.

    SHDB:
          'X' 'SAPLCOIH' '0105' ''                 '',
          ' ' ' '        ' '    'AUFPAR-PM_AUFART'  'ZPM3',
          ' ' ' '        ' '    'CAUFVD-PRIOK'      '3',
          ' ' ' '        ' '    'AFIH-QMNUM'        QMNUM,
          ' ' ' '        ' '    'CAUFVD-IWERK'      LS_EQUZ-IWERK,
          ' ' ' '        ' '    'BDC_OKCODE'        'IHKZ',
          'X' 'SAPLCOIH' '3000' ' '                 ' ',
          ' ' ' '        ' '    'CAUFVD-ILART'      'Z13',
          ' ' ' '        ' '    'BDC_OKCODE'        'BU'.

    OPT-DISMODE = 'E'.
    OPT-DEFSIZE = ' '.

    WAIT UP TO 2 SECONDS.

    CALL TRANSACTION 'IW34' USING GT_BDC OPTIONS FROM OPT.

    WAIT UP TO 2 SECONDS.

  ENDMETHOD.


  method GET_MEDIDOR.
*    me->at_medidor1 = i_medidor1.
*    me->at_medidor2 = i_medidor2.
  endmethod.


  METHOD GET_NOTA.
    E_NOTA = ME->AT_NOTA.
  ENDMETHOD.


  METHOD SET_MEDIDOR.
    ME->AT_MEDIDOR1 = I_MEDIDOR1.
    ME->AT_MEDIDOR2 = I_MEDIDOR2.
  ENDMETHOD.


  METHOD SET_NOTA.
    ME->AT_NOTA = I_NOTA.
  ENDMETHOD.
ENDCLASS.
