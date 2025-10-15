*&---------------------------------------------------------------------*
*& Report  Z_ESTORA_ZGL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT Z_ESTORA_ZGL.

DATA: T_BDCDATA    TYPE BDCDATA    OCCURS 0 WITH HEADER LINE,
      T_BDCMSGCOLL TYPE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.


PARAMETERS: P_BURKS(4)  TYPE C NO-DISPLAY,
            P_BELNR(10) TYPE C NO-DISPLAY,
            P_GJAHS(10) TYPE C NO-DISPLAY,
            P_DATA(10)  TYPE C NO-DISPLAY.


START-OF-SELECTION.


  PERFORM     F_BDC_DATA  USING:
                'SAPMF05A'  '0105'  'X'  ''                 ' ',
              ''          ''      ''   'BDC_OKCODE'	      '/00',
              ''          ''      ''   'RF05A-BELNS'      P_BELNR,
              ''          ''      ''   'BKPF-BUKRS'       P_BURKS,
              ''          ''      ''   'RF05A-GJAHS'      P_GJAHS,
              ''          ''      ''   'UF05A-STGRD'      '02',
              ''          ''      ''   'BSIS-BUDAT'       P_DATA,
              'SAPMF05A'  '0105'  'X'  ''                 ' ',
              ''          ''      ''   'BDC_OKCODE'	      '=BU'.

  CALL TRANSACTION 'FB08' USING T_BDCDATA MODE 'E'. "MESSAGES INTO IT_MSG.


FORM F_BDC_DATA USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.

  T_BDCDATA-PROGRAM   = P_PROGRAM.
  T_BDCDATA-DYNPRO    = P_DYNPRO.
  T_BDCDATA-DYNBEGIN  = P_START.
  T_BDCDATA-FNAM      = P_FNAM.
  T_BDCDATA-FVAL      = P_FVAL.
  APPEND T_BDCDATA.

ENDFORM.
