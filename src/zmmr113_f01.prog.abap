*&---------------------------------------------------------------------*
*&  Include           ZMMR113_F01
*&---------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .

  REFRESH IT_FIELDCAT.

  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MATNR'.
  WA_AFIELD-REF_TABLE     = 'MCHB'.
  WA_AFIELD-REF_FIELD     = 'MATNR'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CHARG'.
  WA_AFIELD-REF_TABLE     = 'MCHB'.
  WA_AFIELD-REF_FIELD     = 'CHARG'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'LGORT'.
  WA_AFIELD-REF_TABLE     = 'MCHB'.
  WA_AFIELD-REF_FIELD     = 'LGORT'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'OBJECTID'.
  WA_AFIELD-REF_TABLE     = 'CDHDR'.
  WA_AFIELD-REF_FIELD     = 'OBJECTID'.
  WA_AFIELD-OUTPUTLEN     = 20.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CHANGENR'.
  WA_AFIELD-REF_TABLE     = 'CDHDR'.
  WA_AFIELD-REF_FIELD     = 'CHANGENR'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'USERNAME'.
  WA_AFIELD-REF_TABLE     = 'CDHDR'.
  WA_AFIELD-REF_FIELD     = 'USERNAME'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'UDATE'.
  WA_AFIELD-REF_TABLE     = 'CDHDR'.
  WA_AFIELD-REF_FIELD     = 'UDATE'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'UTIME'.
  WA_AFIELD-REF_TABLE     = 'CDHDR'.
  WA_AFIELD-REF_FIELD     = 'UTIME'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ATINN'.
  WA_AFIELD-REF_TABLE     = 'CABN'.
  WA_AFIELD-REF_FIELD     = 'ATINN'.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

*  I = I + 1.
*  CLEAR WA_AFIELD.
*  WA_AFIELD-COL_POS       = I.
*  WA_AFIELD-FIELDNAME     = 'ATNAM'.
*  WA_AFIELD-REF_TABLE     = 'CABN'.
*  WA_AFIELD-REF_FIELD     = 'ATNAM'.
*  WA_AFIELD-EDIT          = ''.
*  WA_AFIELD-KEY           = ''.
*  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VALUE_NEW'.
  WA_AFIELD-REF_TABLE     = 'CDPOS'.
  WA_AFIELD-REF_FIELD     = 'VALUE_NEW'.
  WA_AFIELD-OUTPUTLEN     = 20.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VALUE_OLD'.
  WA_AFIELD-REF_TABLE     = 'CDPOS'.
  WA_AFIELD-REF_FIELD     = 'VALUE_OLD'.
  WA_AFIELD-OUTPUTLEN     = 20.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

ENDFORM.

FORM F_BDC_DATA  USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR WA_BDCDATA.
  WA_BDCDATA-PROGRAM   = P_PROGRAM.
  WA_BDCDATA-DYNPRO    = P_DYNPRO.
  WA_BDCDATA-DYNBEGIN  = P_START.
  WA_BDCDATA-FNAM      = P_FNAM.
  WA_BDCDATA-FVAL      = P_FVAL.
  APPEND WA_BDCDATA TO TI_BDCDATA.

ENDFORM.                    " F_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUSCA_DADOS .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALV_HEADER .
  DATA:   WL_DATA(10),
            WL_HORA(8),
            WL_LINHA(60),
            WL_TEXT TYPE SDYDO_TEXT_ELEMENT.

  CONCATENATE  'Centro:' V_WERKS
          INTO WL_LINHA SEPARATED BY SPACE.
  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.





  CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM+0(4) INTO  WL_DATA SEPARATED BY '.'.

  CONCATENATE 'Data :' WL_DATA
  INTO WL_LINHA SEPARATED BY SPACE.

  WL_TEXT = WL_LINHA.
  CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.



ENDFORM.
