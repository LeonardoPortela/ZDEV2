*&---------------------------------------------------------------------*
*&  Include           ZFIR0062_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  SELECT BUKRS AUGDT COD_FLX RMVCT SUM( DMBTR ) SUM( DMBE2 )
    FROM ZFIT0080
    INTO TABLE IT_ZFIT0080
   WHERE BUKRS IN P_BUKRS
     AND AUGDT IN R_MES_ANO
     AND SLD_CONTAS EQ ''
    GROUP BY BUKRS AUGDT COD_FLX RMVCT.

  IF ( IT_ZFIT0080[] IS NOT INITIAL ).

    IF S_C_SLD IS INITIAL. " Codigos Fluxo Sem Saldo

      SELECT *
        INTO TABLE IT_ZFIT0077
        FROM ZFIT0077.

      LOOP AT IT_ZFIT0077 INTO WA_ZFIT0077.

        READ TABLE IT_ZFIT0080 INTO WA_ZFIT0080 WITH KEY COD_FLX = WA_ZFIT0077-COD_FLX.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ZFIT0080.
          WA_ZFIT0080-COD_FLX = WA_ZFIT0077-COD_FLX.
          APPEND WA_ZFIT0080 TO IT_ZFIT0080.
        ENDIF.

      ENDLOOP.

    ENDIF.

    REFRESH: IT_0080_AUX, IT_0080_SAI.
    IT_0080_AUX[] = IT_ZFIT0080[].
    IT_0080_SAI[] = IT_ZFIT0080[].

    SORT IT_0080_AUX BY COD_FLX .
    SORT IT_0080_SAI BY COD_FLX RMVCT.

    DELETE ADJACENT DUPLICATES FROM IT_0080_AUX COMPARING COD_FLX.
    DELETE ADJACENT DUPLICATES FROM IT_0080_SAI COMPARING COD_FLX RMVCT.

    SELECT *
      INTO TABLE IT_ZFIT0077
      FROM ZFIT0077
      FOR ALL ENTRIES IN IT_0080_AUX
     WHERE COD_FLX = IT_0080_AUX-COD_FLX.

    IF IT_ZFIT0077[] IS NOT INITIAL.
      REFRESH IT_0077_AUX.
      IT_0077_AUX[] = IT_ZFIT0077[].

      SORT IT_0077_AUX BY CD_GRUPO.
      DELETE ADJACENT DUPLICATES FROM IT_0077_AUX COMPARING CD_GRUPO.

      SELECT *
        INTO TABLE IT_ZFIT0105
        FROM ZFIT0105
        FOR ALL ENTRIES IN IT_0077_AUX
       WHERE CD_GRUPO = IT_0077_AUX-CD_GRUPO.
    ENDIF.

  ENDIF.



  IF ( IT_ZFIT0080[] IS INITIAL ).
    MESSAGE 'Dados não encontrados!' TYPE 'S'.
    RETURN.
  ENDIF.


ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_DADOS .

  CLEAR:  WA_SAIDA,
          WA_ZFIT0080,
          WA_T247,
          DAY_01_MOV,
          DAY_02_MOV,
          DAY_03_MOV,
          DAY_04_MOV,
          DAY_05_MOV,
          DAY_06_MOV,
          DAY_07_MOV,
          DAY_08_MOV,
          DAY_09_MOV,
          DAY_10_MOV,
          DAY_11_MOV,
          DAY_12_MOV,
          DAY_13_MOV,
          DAY_14_MOV,
          DAY_15_MOV,
          DAY_16_MOV,
          DAY_17_MOV,
          DAY_18_MOV,
          DAY_19_MOV,
          DAY_20_MOV,
          DAY_21_MOV,
          DAY_22_MOV,
          DAY_23_MOV,
          DAY_24_MOV,
          DAY_25_MOV,
          DAY_26_MOV,
          DAY_27_MOV,
          DAY_28_MOV,
          DAY_29_MOV,
          DAY_30_MOV,
          DAY_31_MOV.

  REFRESH: IT_SAIDA,
           IT_ZFIT0080,
           IT_0080_AUX,
           IT_0080_SAI,
           R_MES_ANO,
           IT_ZFIT0077,
           IT_0077_AUX,
           IT_ZFIT0105.


ENDFORM.                    " LIMPA_DADOS
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSA_DADOS .

  DATA: VL_TOT_MONTH TYPE ZFIT0080-DMBTR,
        VL_VLR_DIA   TYPE ZFIT0080-DMBTR,
        VL_FIRST_SLD TYPE ZFIT0080-DMBTR,
        VL_LAST_SLD  TYPE ZFIT0080-DMBTR.

  DATA: TG_0124      TYPE TABLE OF ZFIT0124 WITH HEADER LINE,
        TG_0124_AGRP TYPE TABLE OF ZFIT0124 WITH HEADER LINE.

  CHECK IT_ZFIT0080[] IS NOT INITIAL.

  LOOP AT IT_0080_SAI INTO WA_0080_SAI.

    CLEAR: WA_SAIDA, VL_TOT_MONTH.

    WA_SAIDA-BUKRS     = WA_0080_SAI-BUKRS.
    WA_SAIDA-COD_FLX   = WA_0080_SAI-COD_FLX.
    READ TABLE IT_ZFIT0077 INTO WA_ZFIT0077 WITH KEY COD_FLX = WA_SAIDA-COD_FLX.

    IF ( SY-SUBRC = 0 ) AND ( WA_SAIDA-COD_FLX IS NOT INITIAL ).

      WA_SAIDA-DESC_FLX  = WA_ZFIT0077-DESC_FLX.
      WA_SAIDA-ENT_SAI   = WA_ZFIT0077-ENT_SAI.
      WA_SAIDA-CD_GRUPO  = WA_ZFIT0077-CD_GRUPO.

      READ TABLE IT_ZFIT0105 INTO WA_ZFIT0105 WITH KEY CD_GRUPO = WA_SAIDA-CD_GRUPO.

      IF ( SY-SUBRC = 0 ) AND ( WA_SAIDA-CD_GRUPO IS NOT INITIAL ).
        WA_SAIDA-DS_GRUPO  = WA_ZFIT0105-DESCRICAO.
      ENDIF.

    ENDIF.

    WA_SAIDA-RMVCT = WA_0080_SAI-RMVCT.

    LOOP AT IT_ZFIT0080 INTO WA_ZFIT0080 WHERE COD_FLX =  WA_SAIDA-COD_FLX
                                           AND RMVCT   =  WA_SAIDA-RMVCT.

      CLEAR: VL_VLR_DIA.

      IF 'BRL' IN P_WAERS.
        VL_VLR_DIA  = WA_ZFIT0080-DMBTR.
      ELSE.
        VL_VLR_DIA  = WA_ZFIT0080-DMBE2.
      ENDIF.

      PERFORM ATRIB_VLR_DIA USING WA_ZFIT0080-AUGDT
                                  VL_VLR_DIA
                                  WA_SAIDA.

      ADD VL_VLR_DIA TO VL_TOT_MONTH.

    ENDLOOP.

    WA_SAIDA-TOT_MONTH = VL_TOT_MONTH.

    APPEND WA_SAIDA TO IT_SAIDA.

  ENDLOOP.

  REFRESH: IT_SAIDA_AUX[].

  "Busca Saldo Inicial e Final
  SELECT *
    FROM ZFIT0124
    INTO TABLE TG_0124
   WHERE BUKRS IN P_BUKRS
     AND AUGDT IN R_MES_ANO.

  IF TG_0124[] IS NOT INITIAL.
    TG_0124_AGRP[] = TG_0124[].

    SORT TG_0124      BY AUGDT.
    SORT TG_0124_AGRP BY AUGDT.

    DELETE ADJACENT DUPLICATES FROM TG_0124_AGRP COMPARING AUGDT.

    "Saldo Inicial
    PERFORM ATRIB_SALDOS TABLES TG_0124_AGRP
                                TG_0124
                          USING 'I'           "Inicial
                                WA_SAIDA
                       CHANGING VL_FIRST_SLD
                                VL_LAST_SLD.

    WA_SAIDA-DESC_FLX = 'Saldo Inicial'.
    WA_SAIDA-SLD_INI_FIM = 'X'.
    WA_SAIDA-ROWCOLOR = 'C311'.
    WA_SAIDA-ENT_SAI  = 'A'. "Para Ordenações

    WA_SAIDA-TOT_MONTH = VL_FIRST_SLD.

    APPEND WA_SAIDA TO IT_SAIDA_AUX.

    "Adiciona Movimentação.
    LOOP AT IT_SAIDA INTO WA_SAIDA.

      IF ( WA_SAIDA-COD_FLX IS INITIAL ) AND ( WA_SAIDA-ENT_SAI IS INITIAL ).
        WA_SAIDA-ENT_SAI = 'Y'. "Para Ordenações
      ENDIF.

      APPEND WA_SAIDA TO IT_SAIDA_AUX.
    ENDLOOP.

    "Saldo Final
    PERFORM ATRIB_SALDOS TABLES TG_0124_AGRP
                                TG_0124
                          USING 'F'           "Final
                                WA_SAIDA
                       CHANGING VL_FIRST_SLD
                                VL_LAST_SLD.

    WA_SAIDA-DESC_FLX = 'Saldo Final'.
    WA_SAIDA-SLD_INI_FIM = 'X'.
    WA_SAIDA-ROWCOLOR = 'C311'.
    WA_SAIDA-ENT_SAI = 'Z'. "Para Ordenações

    WA_SAIDA-TOT_MONTH = VL_LAST_SLD.

    APPEND WA_SAIDA TO IT_SAIDA_AUX.

    IT_SAIDA[] = IT_SAIDA_AUX[].

  ENDIF.




ENDFORM.                    " PROCESSA_DADOS

FORM ATRIB_VLR_DIA USING P_AUGDT       TYPE ZFIT0080-AUGDT
                         P_VALOR_MOV   TYPE ZFIT0080-DMBTR
                         P_WA_SAIDA    TYPE TY_SAIDA.

  CASE P_AUGDT+6(2).
    WHEN '01'.
      P_WA_SAIDA-DAY_01    = P_VALOR_MOV.
      DAY_01_MOV = 'X'.
    WHEN '02'.
      P_WA_SAIDA-DAY_02    = P_VALOR_MOV.
      DAY_02_MOV = 'X'.
    WHEN '03'.
      P_WA_SAIDA-DAY_03    = P_VALOR_MOV.
      DAY_03_MOV = 'X'.
    WHEN '04'.
      P_WA_SAIDA-DAY_04    = P_VALOR_MOV.
      DAY_04_MOV = 'X'.
    WHEN '05'.
      P_WA_SAIDA-DAY_05    = P_VALOR_MOV.
      DAY_05_MOV = 'X'.
    WHEN '06'.
      P_WA_SAIDA-DAY_06    = P_VALOR_MOV.
      DAY_06_MOV = 'X'.
    WHEN '07'.
      P_WA_SAIDA-DAY_07    = P_VALOR_MOV.
      DAY_07_MOV = 'X'.
    WHEN '08'.
      P_WA_SAIDA-DAY_08    = P_VALOR_MOV.
      DAY_08_MOV = 'X'.
    WHEN '09'.
      P_WA_SAIDA-DAY_09    = P_VALOR_MOV.
      DAY_09_MOV = 'X'.
    WHEN '10'.
      P_WA_SAIDA-DAY_10    = P_VALOR_MOV.
      DAY_10_MOV = 'X'.
    WHEN '11'.
      P_WA_SAIDA-DAY_11    = P_VALOR_MOV.
      DAY_11_MOV = 'X'.
    WHEN '12'.
      P_WA_SAIDA-DAY_12    = P_VALOR_MOV.
      DAY_12_MOV = 'X'.
    WHEN '13'.
      P_WA_SAIDA-DAY_13    = P_VALOR_MOV.
      DAY_13_MOV = 'X'.
    WHEN '14'.
      P_WA_SAIDA-DAY_14    = P_VALOR_MOV.
      DAY_14_MOV = 'X'.
    WHEN '15'.
      P_WA_SAIDA-DAY_15    = P_VALOR_MOV.
      DAY_15_MOV = 'X'.
    WHEN '16'.
      P_WA_SAIDA-DAY_16    = P_VALOR_MOV.
      DAY_16_MOV = 'X'.
    WHEN '17'.
      P_WA_SAIDA-DAY_17    = P_VALOR_MOV.
      DAY_17_MOV = 'X'.
    WHEN '18'.
      P_WA_SAIDA-DAY_18    = P_VALOR_MOV.
      DAY_18_MOV = 'X'.
    WHEN '19'.
      P_WA_SAIDA-DAY_19    = P_VALOR_MOV.
      DAY_19_MOV = 'X'.
    WHEN '20'.
      P_WA_SAIDA-DAY_20    = P_VALOR_MOV.
      DAY_20_MOV = 'X'.
    WHEN '21'.
      P_WA_SAIDA-DAY_21    = P_VALOR_MOV.
      DAY_21_MOV = 'X'.
    WHEN '22'.
      P_WA_SAIDA-DAY_22    = P_VALOR_MOV.
      DAY_22_MOV = 'X'.
    WHEN '23'.
      P_WA_SAIDA-DAY_23    = P_VALOR_MOV.
      DAY_23_MOV = 'X'.
    WHEN '24'.
      P_WA_SAIDA-DAY_24    = P_VALOR_MOV.
      DAY_24_MOV = 'X'.
    WHEN '25'.
      P_WA_SAIDA-DAY_25    = P_VALOR_MOV.
      DAY_25_MOV = 'X'.
    WHEN '26'.
      P_WA_SAIDA-DAY_26    = P_VALOR_MOV.
      DAY_26_MOV = 'X'.
    WHEN '27'.
      P_WA_SAIDA-DAY_27    = P_VALOR_MOV.
      DAY_27_MOV = 'X'.
    WHEN '28'.
      P_WA_SAIDA-DAY_28    = P_VALOR_MOV.
      DAY_28_MOV = 'X'.
    WHEN '29'.
      P_WA_SAIDA-DAY_29    = P_VALOR_MOV.
      DAY_29_MOV = 'X'.
    WHEN '30'.
      P_WA_SAIDA-DAY_30    = P_VALOR_MOV.
      DAY_30_MOV = 'X'.
    WHEN '31'.
      P_WA_SAIDA-DAY_31    = P_VALOR_MOV.
      DAY_31_MOV = 'X'.
  ENDCASE.


ENDFORM.


FORM IMPRIMIR_DADOS .

  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.

  CHECK ( IT_ZFIT0080[] IS NOT INITIAL ).

  IF P_VARIA IS NOT INITIAL.
    MOVE P_VARIA TO GS_VARIANT_C-VARIANT.
  ENDIF.

  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  WL_LAYOUT-INFO_FIELDNAME = 'ROWCOLOR'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      IS_VARIANT              = GS_VARIANT_C
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IT_FIELDCAT             = ESTRUTURA[]
      IS_LAYOUT               = WL_LAYOUT
      I_SAVE                  = 'X'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
    TABLES
      T_OUTTAB                = IT_SAIDA.

ENDFORM.                    " IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS .
    PERFORM F_CARREGAR_EVENTOS USING: SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0328   text
*      -->P_ENDFORM  text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT .

  REFRESH:  ESTRUTURA[].



  PERFORM MONTAR_ESTRUTURA USING:

    01   'ZFIT0077'   'CD_GRUPO'  'IT_SAIDA' 'CD_GRUPO'  'Cod.Grupo'       '09' '',
    02   ''           ''          'IT_SAIDA' 'DS_GRUPO'  'Desc. Grupo'     '13' '',
    03   'ZFIT0077'   'COD_FLX'   'IT_SAIDA' 'COD_FLX'   'Cod.Flx.'        '08' '',
    04   ''           ''          'IT_SAIDA' 'DESC_FLX'  'Desc.Flx.'       '09' '',
    05   ''           ''          'IT_SAIDA' 'RMVCT'     'Tp.Mov.'         '07' '',
    06   ''           ''          'IT_SAIDA' 'ENT_SAI'   'E/S'             '03' '',
    07   ''           ''          'IT_SAIDA' 'DAY_01'    'Dia 01'          '15' 'X',
    08   ''           ''          'IT_SAIDA' 'DAY_02'    'Dia 02'          '15' 'X',
    09   ''           ''          'IT_SAIDA' 'DAY_03'    'Dia 03'          '15' 'X',
    10   ''           ''          'IT_SAIDA' 'DAY_04'    'Dia 04'          '15' 'X',
    11   ''           ''          'IT_SAIDA' 'DAY_05'    'Dia 05'          '15' 'X',
    12   ''           ''          'IT_SAIDA' 'DAY_06'    'Dia 06'          '15' 'X',
    13   ''           ''          'IT_SAIDA' 'DAY_07'    'Dia 07'          '15' 'X',
    14   ''           ''          'IT_SAIDA' 'DAY_08'    'Dia 08'          '15' 'X',
    15   ''           ''          'IT_SAIDA' 'DAY_09'    'Dia 09'          '15' 'X',
    16   ''           ''          'IT_SAIDA' 'DAY_10'    'Dia 10'          '15' 'X',
    17   ''           ''          'IT_SAIDA' 'DAY_11'    'Dia 11'          '15' 'X',
    18   ''           ''          'IT_SAIDA' 'DAY_12'    'Dia 12'          '15' 'X',
    19   ''           ''          'IT_SAIDA' 'DAY_13'    'Dia 13'          '15' 'X',
    20   ''           ''          'IT_SAIDA' 'DAY_14'    'Dia 14'          '15' 'X',
    21   ''           ''          'IT_SAIDA' 'DAY_15'    'Dia 15'          '15' 'X',
    22   ''           ''          'IT_SAIDA' 'DAY_16'    'Dia 16'          '15' 'X',
    23   ''           ''          'IT_SAIDA' 'DAY_17'    'Dia 17'          '15' 'X',
    24   ''           ''          'IT_SAIDA' 'DAY_18'    'Dia 18'          '15' 'X',
    25   ''           ''          'IT_SAIDA' 'DAY_19'    'Dia 19'          '15' 'X',
    26   ''           ''          'IT_SAIDA' 'DAY_20'    'Dia 20'          '15' 'X',
    27   ''           ''          'IT_SAIDA' 'DAY_21'    'Dia 21'          '15' 'X',
    28   ''           ''          'IT_SAIDA' 'DAY_22'    'Dia 22'          '15' 'X',
    29   ''           ''          'IT_SAIDA' 'DAY_23'    'Dia 23'          '15' 'X',
    30   ''           ''          'IT_SAIDA' 'DAY_24'    'Dia 24'          '15' 'X',
    31   ''           ''          'IT_SAIDA' 'DAY_25'    'Dia 25'          '15' 'X',
    32   ''           ''          'IT_SAIDA' 'DAY_26'    'Dia 26'          '15' 'X',
    33   ''           ''          'IT_SAIDA' 'DAY_27'    'Dia 27'          '15' 'X',
    34   ''           ''          'IT_SAIDA' 'DAY_28'    'Dia 28'          '15' 'X',
    35   ''           ''          'IT_SAIDA' 'DAY_29'    'Dia 29'          '15' 'X',
    36   ''           ''          'IT_SAIDA' 'DAY_30'    'Dia 30'          '15' 'X',
    37   ''           ''          'IT_SAIDA' 'DAY_31'    'Dia 31'          '15' 'X',
    38   ''           ''          'IT_SAIDA' 'TOT_MONTH' 'Total Mês'       '23' 'X'.

ENDFORM.                    " MONTAR_LAYOUT

FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_HOTSPOT).

  DATA: VL_SCRTEXT_AUX TYPE DD03P-SCRTEXT_L,
        VL_SCRTEXT     TYPE DD03P-SCRTEXT_L.

  CASE P_FIELD.
    WHEN 'DAY_01'.
      IF DAY_01_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '01 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_02'.
      IF DAY_02_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '02 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_03'.
      IF DAY_03_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '03 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_04'.
      IF DAY_04_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '04 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_05'.
      IF DAY_05_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '05 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_06'.
      IF DAY_06_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '06 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_07'.
      IF DAY_07_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '07 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_08'.
      IF DAY_08_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '08 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_09'.
      IF DAY_09_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '09 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_10'.
      IF DAY_10_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '10 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_11'.
      IF DAY_11_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '11 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_12'.
      IF DAY_12_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '12 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_13'.
      IF DAY_13_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '13 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_14'.
      IF DAY_14_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '14 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_15'.
      IF DAY_15_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '15 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_16'.
      IF DAY_16_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '16 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_17'.
      IF DAY_17_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '17 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_18'.
      IF DAY_18_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '18 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_19'.
      IF DAY_19_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '19 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_20'.
      IF DAY_20_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '20 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_21'.
      IF DAY_21_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '21 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_22'.
      IF DAY_22_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '22 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_23'.
      IF DAY_23_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '23 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_24'.
      IF DAY_24_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '24 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_25'.
      IF DAY_25_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '25 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_26'.
      IF DAY_26_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '26 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_27'.
      IF DAY_27_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '27 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_28'.
      IF DAY_28_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '28 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_29'.
      IF DAY_29_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '29 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_30'.
      IF DAY_30_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '30 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'DAY_31'.
      IF DAY_31_MOV IS INITIAL.
        EXIT.
      ENDIF.
      CONCATENATE '31 -' WA_T247-KTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.
    WHEN 'TOT_MONTH'.
      CONCATENATE 'Total -' WA_T247-LTX INTO VL_SCRTEXT_AUX SEPARATED BY SPACE.

  ENDCASE.

  IF VL_SCRTEXT_AUX IS NOT INITIAL.
    IF VL_SCRTEXT_AUX(5) = 'Total'.
      TRANSLATE VL_SCRTEXT_AUX+9 TO LOWER CASE.
    ELSE.
      TRANSLATE VL_SCRTEXT_AUX+6 TO LOWER CASE.
    ENDIF.

    VL_SCRTEXT = VL_SCRTEXT_AUX.
  ELSE.
    VL_SCRTEXT = P_SCRTEXT_L.
  ENDIF.

  CLEAR WA_ESTRUTURA.

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = VL_SCRTEXT.
  WA_ESTRUTURA-SELTEXT_M     = VL_SCRTEXT.
  WA_ESTRUTURA-SELTEXT_L     = VL_SCRTEXT.
  WA_ESTRUTURA-HOTSPOT       = P_HOTSPOT.

  IF VL_SCRTEXT IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = VL_SCRTEXT.
  ENDIF.


  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO


*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS .

  DATA: WL_LAYOUT2(50) TYPE C,
        WL_LAYOUT3(50) TYPE C,
        WL_EMP(8) VALUE 'Empresa:',
        WL_BUTXT TYPE BUTXT,
        WL_BUKRS(50) TYPE C.

  REFRESH: T_TOP.

  SELECT SINGLE BUTXT
    INTO WL_BUTXT
    FROM T001
   WHERE BUKRS IN P_BUKRS.

  CONCATENATE WL_EMP P_BUKRS+3(4) SPACE WL_BUTXT  INTO WL_BUKRS SEPARATED BY SPACE.

  CONCATENATE 'Mês/Ano:' P_MM_ANO(02) '-' P_MM_ANO+2(4) INTO  WL_LAYOUT2 SEPARATED BY SPACE.
  CONCATENATE 'Moeda:' P_WAERS-LOW INTO WL_LAYOUT3 SEPARATED BY SPACE.

  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-003.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_BUKRS.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_LAYOUT2.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_LAYOUT3.

  V_REPORT = SY-REPID.
  GS_VARIANT-REPORT      = SY-REPID.

ENDFORM.                    " INICIAR_VARIAVEIS



*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  VALIDA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VG_VALIDA_CAMPOS  text
*----------------------------------------------------------------------*
FORM VALIDA_CAMPOS  CHANGING P_VG_VALIDA_CAMPOS.

  DATA: VL_MES          TYPE I,
        VL_ANO          TYPE I,
        VL_DT_INI(8)    TYPE C,
        VL_DT_FIM(8)    TYPE C,
        VL_DT_LOW       TYPE SY-DATUM,
        VL_DT_HIGH_IN   TYPE SY-DATUM,
        VL_DT_HIGH_OUT  TYPE SY-DATUM.


  CLEAR: P_VG_VALIDA_CAMPOS.

  VL_MES = P_MM_ANO(02).
  VL_ANO = P_MM_ANO+2(04).

  IF ( VL_MES = 0 ) OR ( VL_MES > 12 ).
    P_VG_VALIDA_CAMPOS = 'X'.
    MESSAGE 'Informe um mês válido!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF ( VL_ANO = 0 ).
    P_VG_VALIDA_CAMPOS = 'X'.
    MESSAGE 'Informe um ano válido!' TYPE 'S'.
    EXIT.
  ENDIF.


  CONCATENATE P_MM_ANO+2(4) P_MM_ANO(2) '01' INTO VL_DT_INI.

  CONCATENATE P_MM_ANO+2(4) P_MM_ANO(2) '01' INTO VL_DT_FIM.

  VL_DT_LOW     = VL_DT_INI.
  VL_DT_HIGH_IN = VL_DT_FIM.


  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN = VL_DT_HIGH_IN
    IMPORTING
      LAST_DAY_OF_MONTH = VL_DT_HIGH_OUT.

  R_MES_ANO-SIGN   = 'I'.
  R_MES_ANO-OPTION = 'BT'.
  R_MES_ANO-LOW    = VL_DT_LOW.
  R_MES_ANO-HIGH   = VL_DT_HIGH_OUT.
  APPEND R_MES_ANO.

  SELECT SINGLE *
    INTO WA_T247
    FROM T247
   WHERE SPRAS = SY-LANGU
     AND MNR   = VL_MES.


ENDFORM.                    " VALIDA_CAMPOS


FORM USER_COMMAND  USING R_UCOMM      LIKE SY-UCOMM
                         RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA: IT_RSPARAMS   TYPE TABLE OF RSPARAMS,
        WA_RSPARAMS   TYPE RSPARAMS.

  DATA: VL_DT_C(8)    TYPE C,
        VL_BUDAT      TYPE SY-DATUM,
        VL_BUDAT_LOW  TYPE SY-DATUM,
        VL_BUDAT_HIGH TYPE SY-DATUM,
        VL_BUDAT_IN   TYPE SY-DATUM.

  CASE R_UCOMM.
    WHEN: '&IC1'.

      CASE RS_SELFIELD-FIELDNAME(04).
        WHEN 'DAY_'.
          CLEAR: VL_DT_C,VL_BUDAT.
          CONCATENATE P_MM_ANO+2(4) P_MM_ANO(2) RS_SELFIELD-FIELDNAME+04(2) INTO VL_DT_C.
          VL_BUDAT    = VL_DT_C.

          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.

          CHECK WA_SAIDA-SLD_INI_FIM IS INITIAL.

          SUBMIT ZFIR0055 WITH S_BUKRS         = WA_SAIDA-BUKRS
                          WITH S_COD_FL        = WA_SAIDA-COD_FLX
                          WITH S_AUGDT         = VL_BUDAT
                          WITH S_RMVCT         = WA_SAIDA-RMVCT
                          AND RETURN.
      ENDCASE.

      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'TOT_MONTH'.
          CLEAR: VL_DT_C.
          CONCATENATE P_MM_ANO+2(4) P_MM_ANO(2) '01' INTO VL_DT_C.
          VL_BUDAT_LOW  = VL_DT_C.

          VL_BUDAT_IN   = VL_DT_C.

          CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
            EXPORTING
              DAY_IN = VL_BUDAT_IN
            IMPORTING
              LAST_DAY_OF_MONTH = VL_BUDAT_HIGH.

          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.

          CHECK WA_SAIDA-SLD_INI_FIM IS INITIAL.

          WA_RSPARAMS-KIND    = 'S'.  "SELECT OPTIONS TO BE PASSED
          WA_RSPARAMS-SIGN    = 'I'.

          WA_RSPARAMS-SELNAME = 'S_BUKRS'.
          WA_RSPARAMS-OPTION  = 'EQ'.
          WA_RSPARAMS-LOW     = WA_SAIDA-BUKRS.
          WA_RSPARAMS-HIGH    = WA_SAIDA-BUKRS.
          APPEND WA_RSPARAMS TO IT_RSPARAMS.

          WA_RSPARAMS-SELNAME = 'S_COD_FL'.
          WA_RSPARAMS-OPTION  = 'EQ'.
          WA_RSPARAMS-LOW     = WA_SAIDA-COD_FLX.
          WA_RSPARAMS-HIGH    = WA_SAIDA-COD_FLX.
          APPEND WA_RSPARAMS TO IT_RSPARAMS.

          WA_RSPARAMS-SELNAME = 'S_RMVCT'.
          WA_RSPARAMS-OPTION  = 'EQ'.
          WA_RSPARAMS-LOW     = WA_SAIDA-RMVCT.
          WA_RSPARAMS-HIGH    = WA_SAIDA-RMVCT.
          APPEND WA_RSPARAMS TO IT_RSPARAMS.

          WA_RSPARAMS-SELNAME = 'S_AUGDT'.
          WA_RSPARAMS-OPTION  = 'BT'.
          WA_RSPARAMS-LOW     = VL_BUDAT_LOW.
          WA_RSPARAMS-HIGH    = VL_BUDAT_HIGH.
          APPEND WA_RSPARAMS TO IT_RSPARAMS.

          SUBMIT ZFIR0055 WITH SELECTION-TABLE IT_RSPARAMS
                      AND RETURN.

      ENDCASE.

  ENDCASE.

ENDFORM.                    "user_command

FORM ATRIB_SALDOS TABLES P_0124_AGRP STRUCTURE ZFIT0124
                         P_0124      STRUCTURE ZFIT0124
                   USING P_TIPO_SALDO  " I = Inicial / F = Final
                         P_SAIDA
                CHANGING C_FIRST_SLD TYPE ZFIT0080-DMBTR
                         C_LAST_SLD  TYPE ZFIT0080-DMBTR.

  DATA: VL_MOV     TYPE C,
        VL_VLR_DIA TYPE ZFIT0080-DMBTR.

  CLEAR: P_SAIDA, C_FIRST_SLD, C_LAST_SLD.

  LOOP AT P_0124_AGRP.

    CLEAR: VL_VLR_DIA.

    PERFORM GET_MOV_DIA USING P_0124_AGRP-AUGDT
                     CHANGING VL_MOV.

    CHECK VL_MOV IS NOT INITIAL.

    LOOP AT P_0124 WHERE AUGDT = P_0124_AGRP-AUGDT.
      IF 'BRL' IN P_WAERS.
        IF P_TIPO_SALDO = 'I'.
          ADD P_0124-SDO_INICIAL_R  TO VL_VLR_DIA.
        ELSEIF P_TIPO_SALDO = 'F'.
          ADD P_0124-SDO_FINAL_R    TO VL_VLR_DIA.
        ENDIF.
      ELSE.
        IF P_TIPO_SALDO = 'I'.
          ADD P_0124-SDO_INICIAL_US TO VL_VLR_DIA.
        ELSEIF P_TIPO_SALDO = 'F'.
          ADD P_0124-SDO_FINAL_US   TO VL_VLR_DIA.
        ENDIF.
      ENDIF.
    ENDLOOP.

    PERFORM ATRIB_VLR_DIA USING P_0124_AGRP-AUGDT
                                VL_VLR_DIA
                                P_SAIDA.

    IF C_FIRST_SLD IS INITIAL.
      C_FIRST_SLD = VL_VLR_DIA.
    ENDIF.

    C_LAST_SLD = VL_VLR_DIA.

  ENDLOOP.

ENDFORM.

FORM GET_MOV_DIA USING P_AUGDT TYPE ZFIT0080-AUGDT
              CHANGING P_MOV.

  CLEAR: P_MOV.

  CASE P_AUGDT+6(2)..
    WHEN '01'.
      CHECK DAY_01_MOV IS NOT INITIAL.
    WHEN '02'.
      CHECK DAY_02_MOV IS NOT INITIAL.
    WHEN '03'.
      CHECK DAY_03_MOV IS NOT INITIAL.
    WHEN '04'.
      CHECK DAY_04_MOV IS NOT INITIAL.
    WHEN '05'.
      CHECK DAY_05_MOV IS NOT INITIAL.
    WHEN '06'.
      CHECK DAY_06_MOV IS NOT INITIAL.
    WHEN '07'.
      CHECK DAY_07_MOV IS NOT INITIAL.
    WHEN '08'.
      CHECK DAY_08_MOV IS NOT INITIAL.
    WHEN '09'.
      CHECK DAY_09_MOV IS NOT INITIAL.
    WHEN '10'.
      CHECK DAY_10_MOV IS NOT INITIAL.
    WHEN '11'.
      CHECK DAY_11_MOV IS NOT INITIAL.
    WHEN '12'.
      CHECK DAY_12_MOV IS NOT INITIAL.
    WHEN '13'.
      CHECK DAY_13_MOV IS NOT INITIAL.
    WHEN '14'.
      CHECK DAY_14_MOV IS NOT INITIAL.
    WHEN '15'.
      CHECK DAY_15_MOV IS NOT INITIAL.
    WHEN '16'.
      CHECK DAY_16_MOV IS NOT INITIAL.
    WHEN '17'.
      CHECK DAY_17_MOV IS NOT INITIAL.
    WHEN '18'.
      CHECK DAY_18_MOV IS NOT INITIAL.
    WHEN '19'.
      CHECK DAY_19_MOV IS NOT INITIAL.
    WHEN '20'.
      CHECK DAY_20_MOV IS NOT INITIAL.
    WHEN '21'.
      CHECK DAY_21_MOV IS NOT INITIAL.
    WHEN '22'.
      CHECK DAY_22_MOV IS NOT INITIAL.
    WHEN '23'.
      CHECK DAY_23_MOV IS NOT INITIAL.
    WHEN '24'.
      CHECK DAY_24_MOV IS NOT INITIAL.
    WHEN '25'.
      CHECK DAY_25_MOV IS NOT INITIAL.
    WHEN '26'.
      CHECK DAY_26_MOV IS NOT INITIAL.
    WHEN '27'.
      CHECK DAY_27_MOV IS NOT INITIAL.
    WHEN '28'.
      CHECK DAY_28_MOV IS NOT INITIAL.
    WHEN '29'.
      CHECK DAY_29_MOV IS NOT INITIAL.
    WHEN '30'.
      CHECK DAY_30_MOV IS NOT INITIAL.
    WHEN '31'.
      CHECK DAY_31_MOV IS NOT INITIAL.
  ENDCASE.

  P_MOV = 'X'.

ENDFORM.
