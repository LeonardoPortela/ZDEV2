*----------------------------------------------------------------------*
* INCLUDE ZSDR0048_FORM .
*----------------------------------------------------------------------*

FORM CHANGE_CATALOGO USING VALUE(P_EDIT) TYPE C.

  DATA: VAR_TMN TYPE N LENGTH 2.
  REFRESH: IT_FCAT.


  IF VAR_DIR NE ABAP_TRUE.
    VAR_TMN = 10.
    PERFORM ESTRUTURA_ALV USING:
          0  'ZSDT0094'  ' ' 'IT_ZSDT0094' 'DATA_REGISTRO' 'Data Reg.'         '10' ' '    '' ' ' ' ',
          1  'ZSDT0094'  ' ' 'IT_ZSDT0094' 'HORA_REGISTRO' 'Hora Reg.'         '8'  ' '    '' ' ' ' '.
  ELSE.
    VAR_TMN = 28.
  ENDIF.



  PERFORM ESTRUTURA_ALV USING:
        2  'ZSDT0094'  ' ' 'IT_ZSDT0094' 'PROGRAMA'      'Programa'          VAR_TMN ' ' '' ' ' ' ',
        3  ' '         ' ' 'IT_ZSDT0094' 'NRO_SOL_OV'    'Nro. Sol.'         '15'  ' '    '' ' ' ' ',
        4  'ZSDT0094'  ' ' 'IT_ZSDT0094' 'FIXACAO'       'Fixação'           '10' ' '    '' ' ' 'C',
        5  'ZSDT0094'  ' ' 'IT_ZSDT0094' 'DATA_VENC'     'Data Venc.'        '15' P_EDIT '' ' ' ' ',
        6  'ZSDT0094'  ' ' 'IT_ZSDT0094' 'DATA_LIB'      'Data Lib.'         '15' P_EDIT '' ' ' ' ',
        7  'ZSDT0094'  ' ' 'IT_ZSDT0094' 'CADENCIA_QTE'  'Qtde. Diária'      '12' P_EDIT '' ' ' ' ',
        8  'ZSDT0094'  ' ' 'IT_ZSDT0094' 'ZIEME'         'UM Qtde. Prev.'    '12' ''     '' ' ' 'C',
        9  'ZSDT0094'  ' ' 'IT_ZSDT0094' 'TOTAL_PROPORC' 'Total Proporc.'    '11' P_EDIT '' ' ' ' ',
        10 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'TIPO_TAXA'     'Tipo Taxa'         '10' P_EDIT '' ' ' ' ',
        11 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'TAXA_CURVA'    'Taxa Curva'        '10' P_EDIT '' ' ' ' ',
        12 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'TAXA_CAMBIO'   'Taxa Câmbio'       '11' P_EDIT '' ' ' ' ',
        13 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'FRETE_CIF'     'Frete CIF'         '8'  P_EDIT '' ' ' ' ',
        14 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'FRETE_PORTO'   'Frete Porto'       '10' P_EDIT '' ' ' ' ',
        15 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'TIPO'          'Tipo'              '6'  ' '    '' ' ' ' ',
        16 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'BEZEI'         'Denominação'       '11' P_EDIT '' ' ' ' ',
        17 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'ESTORNO'       'Estorno'           '9'  P_EDIT '' ' ' ' ',
        18 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'EDICAO'        'Edição'            '8'  ' '    '' ' ' ' ',
        19 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'VBELN'         'Doc. Vendas'       '15' P_EDIT '' ' ' ' ',
        20 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'INCO1'         'Incoterms'         '4'  P_EDIT '' ' ' ' ',
        21 'ZSDT0094'  ' ' 'IT_ZSDT0094' 'SAFRA'         'Safra'             '6'  P_EDIT '' ' ' ' '.

ENDFORM.                    " CRIAR_CATALOGO

FORM ESTRUTURA_ALV USING VALUE(P_COL_POS)       TYPE I
                         VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                         VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                         VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                         VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                         VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                         VALUE(P_OUTPUTLEN)
                         VALUE(P_EDIT)
                         VALUE(P_SUM)
                         VALUE(P_EMPHASIZE)
                         VALUE(P_JUST).

  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME   = P_FIELD.
  WA_FCAT-TABNAME     = P_TABNAME.
  WA_FCAT-REF_TABLE   = P_REF_TABNAME.
  WA_FCAT-REF_FIELD   = P_REF_FIELDNAME.
  WA_FCAT-KEY         = ' '.
  WA_FCAT-EDIT        = P_EDIT.
  WA_FCAT-COL_POS     = P_COL_POS.
  WA_FCAT-OUTPUTLEN   = P_OUTPUTLEN.
  WA_FCAT-NO_OUT      = ' '.
  WA_FCAT-REPTEXT     = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_S   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_M   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_L   = P_SCRTEXT_L.
  WA_FCAT-EMPHASIZE   = P_EMPHASIZE.
  WA_FCAT-STYLE       =
  WA_FCAT-JUST        = P_JUST.

  IF P_FIELD EQ 'BEZEI'.
    WA_FCAT-F4AVAILABL = ABAP_TRUE.
  ENDIF.

  IF VAR_DIR EQ ABAP_TRUE.
    CASE P_FIELD.
      WHEN 'TIPO_TAXA'.WA_FCAT-DRDN_HNDL = 1.
      WHEN 'PROGRAMA'. WA_FCAT-DRDN_HNDL = 2.
      WHEN 'TIPO'.     WA_FCAT-DRDN_HNDL = 3.
    ENDCASE.
  ENDIF.


  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    " ESTRUTURA_ALV

FORM CHANGE_ROWS USING VALUE(P_ACTIVE) VALUE(P_DIR).

  IF P_ACTIVE = 'ACTIVE'.
    ROW_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  ELSE.
    ROW_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  ENDIF.

  IF P_DIR IS INITIAL.

    LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.

      READ TABLE IT_ZSDT0094 INTO SL_ZSDT0094 INDEX WA_SELECTED_ROWS-INDEX.

      CLEAR: SL_ZSDT0094-FIELD_STYLE, LS_EDIT, LT_EDIT.

      LOOP AT IT_FCAT INTO WA_FCAT.

        CASE WA_FCAT-FIELDNAME.
          WHEN 'DATA_VENC'     OR 'DATA_LIB'   OR 'CADENCIA_QTE' OR
               'TOTAL_PROPORC' OR 'TAXA_CURVA' OR 'TIPO_TAXA'    OR
               'TAXA_CAMBIO'   OR 'FRETE_CIF'  OR 'FRETE_PORTO'  OR
               'BEZEI'         OR 'ESTORNO'    OR 'VBELN'        OR
               'INCO1'         OR 'SAFRA'.

            LS_EDIT-FIELDNAME = WA_FCAT-FIELDNAME.
            LS_EDIT-STYLE = ROW_EDIT-STYLE.
            LS_EDIT-STYLE2 = SPACE.
            LS_EDIT-STYLE3 = SPACE.
            LS_EDIT-STYLE4 = SPACE.
            INSERT LS_EDIT INTO TABLE LT_EDIT.
        ENDCASE.

      ENDLOOP.

      INSERT LINES OF LT_EDIT INTO TABLE SL_ZSDT0094-FIELD_STYLE.

      MODIFY IT_ZSDT0094 FROM SL_ZSDT0094 INDEX WA_SELECTED_ROWS-INDEX
         TRANSPORTING FIELD_STYLE.

    ENDLOOP.

  ELSE.


    ADD 1 TO CONT.

    IF CONT EQ 1.

      FREE SL_ZSDT0094-FIELD_STYLE.

      LOOP AT IT_FCAT_0102 INTO WA_FCAT.

        CASE WA_FCAT-FIELDNAME.
          WHEN 'DATA_REGISTRO' OR 'HORA_REGISTRO'.
            ROW_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
          WHEN OTHERS.
            ROW_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ENDCASE.

        FREE:  LS_EDIT, LT_EDIT.

        LS_EDIT-FIELDNAME = WA_FCAT-FIELDNAME.
        LS_EDIT-STYLE = ROW_EDIT-STYLE.
        LS_EDIT-STYLE2 = SPACE.
        LS_EDIT-STYLE3 = SPACE.
        LS_EDIT-STYLE4 = SPACE.
*        LS_EDIT-MAXLEN = 30.
        INSERT LS_EDIT INTO TABLE LT_EDIT.

        INSERT LINES OF LT_EDIT INTO TABLE SL_ZSDT0094-FIELD_STYLE.

      ENDLOOP.

    ENDIF.

    LOOP AT IT_ADD INTO WA_ADD.
      MODIFY IT_ADD FROM SL_ZSDT0094 INDEX SY-TABIX
         TRANSPORTING FIELD_STYLE.
    ENDLOOP.


  ENDIF.

ENDFORM.                    " DISABLED_ROWS

FORM GRAVA_REGISTRO .

  REFRESH IT_ZSDT0094_INPUT.
  CLEAR SL_ZSDT0094.

  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.

    CLEAR WL_ZSDT0094.

    READ TABLE IT_ZSDT0094 INTO SL_ZSDT0094 INDEX WA_SELECTED_ROWS-INDEX.

    MOVE-CORRESPONDING SL_ZSDT0094 TO WL_ZSDT0094.

    APPEND WL_ZSDT0094 TO IT_ZSDT0094_INPUT.

  ENDLOOP.

  MODIFY ZSDT0094 FROM TABLE IT_ZSDT0094_INPUT.

  COMMIT WORK.

ENDFORM.                    " GRAVA_REGISTRO

FORM SELECIONAR_DADOS .

  SELECT *
    FROM ZSDT0094 INTO CORRESPONDING FIELDS OF TABLE IT_ZSDT0094
   WHERE NRO_SOL_OV IN P_SOL_OV
     AND FIXACAO    IN P_FIX
     AND DATA_REGISTRO IN P_DATA
     AND HORA_REGISTRO IN P_HORA.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_DROPDOWN .

*  DATA:   LS_DROPDOWN TYPE LVC_S_DROP,
*          LT_DROPDOWN TYPE LVC_T_DROP.

  LS_DROPDOWN-HANDLE = '1'.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'V'.
  GT_VALUES-DOMVALUE_L = 'V'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'C'.
  GT_VALUES-DOMVALUE_L = 'C'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-HANDLE = '2'.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'ZSDT0062'.
  GT_VALUES-DOMVALUE_L = '22'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'ZSDT0044'.
  GT_VALUES-DOMVALUE_L = '16'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'ZSDT0066'.
  GT_VALUES-DOMVALUE_L = '26'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'ZSDT0087'.
  GT_VALUES-DOMVALUE_L = '42'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'VF01'.
  GT_VALUES-DOMVALUE_L = '0A'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'ME22N'.
  GT_VALUES-DOMVALUE_L = 'UI'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'ZLES0077'.
  GT_VALUES-DOMVALUE_L = 'FA'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  SORT LT_DROPDOWN BY HANDLE VALUE.
  DELETE ADJACENT DUPLICATES FROM LT_DROPDOWN COMPARING HANDLE VALUE.

  SORT GT_VALUES BY DOMVALUE_L.
  DELETE ADJACENT DUPLICATES FROM GT_VALUES COMPARING DOMVALUE_L.


* Übergabe der Dropdown-Tabelle an ALV-Grid-Control
  CALL METHOD OBJ_GRID_0102->SET_DROP_DOWN_TABLE
    EXPORTING
      IT_DROP_DOWN = LT_DROPDOWN.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_DROPDOWN1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_DROPDOWN1 USING P_PROG P_FIELDNAME.

  LS_DROPDOWN-HANDLE = '3'.

  CASE P_PROG.
    WHEN 'FA'.
      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'AQV'.
      GT_VALUES-DOMVALUE_L = 'AQV'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

    WHEN '22' OR '26'.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'VDA'.
      GT_VALUES-DOMVALUE_L = 'VDA'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'FRE'.
      GT_VALUES-DOMVALUE_L = 'FRE'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

    WHEN '16' OR '42'.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'VDI'.
      GT_VALUES-DOMVALUE_L = 'VDI'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'FRI'.
      GT_VALUES-DOMVALUE_L = 'FRI'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

    WHEN '0A'.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'VDA'.
      GT_VALUES-DOMVALUE_L = 'VDA'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'FRE'.
      GT_VALUES-DOMVALUE_L = 'FRE'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'VDI'.
      GT_VALUES-DOMVALUE_L = 'VDI'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'AQV'.
      GT_VALUES-DOMVALUE_L = 'AQV'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.
      " NOVOS
      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'SPT'.
      GT_VALUES-DOMVALUE_L = 'SPT'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'TBO'.
      GT_VALUES-DOMVALUE_L = 'TBO'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'TBP'.
      GT_VALUES-DOMVALUE_L = 'TBP'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

    WHEN 'UI'.

      LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'PDI'.
      GT_VALUES-DOMVALUE_L = 'PDI'.
      APPEND: LS_DROPDOWN TO LT_DROPDOWN,
              GT_VALUES.

  ENDCASE.

  SORT LT_DROPDOWN BY HANDLE VALUE.
  DELETE ADJACENT DUPLICATES FROM LT_DROPDOWN COMPARING HANDLE VALUE.

  SORT GT_VALUES BY DOMVALUE_L.
  DELETE ADJACENT DUPLICATES FROM GT_VALUES COMPARING DOMVALUE_L.

  CALL METHOD OBJ_GRID_0102->SET_DROP_DOWN_TABLE
    EXPORTING
      IT_DROP_DOWN = LT_DROPDOWN.

  IF NOT OBJ_GRID_0102 IS INITIAL.
    CALL METHOD OBJ_GRID_0102->REFRESH_TABLE_DISPLAY.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA .

  PERFORM SELECIONAR_DADOS.
  CALL METHOD OBJ_GRID->REFRESH_TABLE_DISPLAY.
  CALL METHOD OBJ_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

  WL_DESACTIVE = ' '.
  PERFORM CHANGE_ROWS USING 'DESACTIVE' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REVERSAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REVERSAO TABLES GY_ZSDT0094.

  CHECK NOT GY_ZSDT0094[] IS INITIAL.

  DATA: OBJ_ZCL_TX_CURVA_DB  TYPE REF TO ZCL_TAXA_CURVA_DB, "Objeto da taxa curva.
        OBJ_ZCL_TX_CURVA     TYPE REF TO ZCL_TAXA_CURVA, "Objeto da taxa curva.
        OBJ_ZCL_WEB_TX_CURVA TYPE REF TO ZCL_WEBSERVICE_TX_CURVA.

  FREE: OBJ_ZCL_TX_CURVA, OBJ_ZCL_WEB_TX_CURVA, OBJ_ZCL_TX_CURVA_DB.
  CREATE OBJECT: OBJ_ZCL_TX_CURVA, OBJ_ZCL_TX_CURVA_DB, OBJ_ZCL_WEB_TX_CURVA.

  DATA: GW_ZSDT0094 TYPE ZSDT0094.
  DATA: VAR_ESTORNO TYPE NUM10.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '01'
      OBJECT                  = 'ZEST_0094'
    IMPORTING
      NUMBER                  = VAR_ESTORNO "Numeração para identificar o estorno.
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.


  LOOP AT GY_ZSDT0094 INTO GW_ZSDT0094.

    CHECK GW_ZSDT0094-ESTORNO IS INITIAL.

    OBJ_ZCL_TX_CURVA->SET_NUMERO( GW_ZSDT0094-NRO_SOL_OV ).
    OBJ_ZCL_TX_CURVA->SET_DATA_REGISTRO( GW_ZSDT0094-DATA_REGISTRO ).
    OBJ_ZCL_TX_CURVA->SET_HORA_REGISTRO( GW_ZSDT0094-HORA_REGISTRO ).



    OBJ_ZCL_TX_CURVA->SET_ESTORNO( VAR_ESTORNO ).
    GW_ZSDT0094-ESTORNO  = VAR_ESTORNO.


    GW_ZSDT0094-CADENCIA_QTE  = GW_ZSDT0094-CADENCIA_QTE * -1.
    GW_ZSDT0094-TOTAL_PROPORC = GW_ZSDT0094-TOTAL_PROPORC * -1.

    IF GW_ZSDT0094-TIPO_TAXA EQ 'V'.
      GW_ZSDT0094-TIPO_TAXA =  'C'.
    ELSE.
      GW_ZSDT0094-TIPO_TAXA =  'V'.
    ENDIF.

    IF GW_ZSDT0094-DATA_VENC < SY-DATUM.
      GW_ZSDT0094-DATA_VENC = ( GW_ZSDT0094-DATA_LIB + 30 ).
    ENDIF.

    IF GW_ZSDT0094-DATA_REGISTRO NE SY-DATUM.

      OBJ_ZCL_WEB_TX_CURVA->BUSCAR_TAXA(
                EXPORTING
                  I_DATA = GW_ZSDT0094-DATA_VENC
              I_DATA_LIB = GW_ZSDT0094-DATA_LIB
                  I_TIPO = GW_ZSDT0094-TIPO_TAXA
                  RECEIVING
                  E_COTACAO = GW_ZSDT0094-TAXA_CURVA ).
    ENDIF.

    GW_ZSDT0094-DATA_REGISTRO = SY-DATUM.
    GW_ZSDT0094-HORA_REGISTRO = SY-UZEIT.
    GW_ZSDT0094-DATA_LIB = SY-DATUM.

    OBJ_ZCL_TX_CURVA_DB->ZIF_TAXA_CURVA_DB~ATUALIZAR( OBJ_ZCL_TX_CURVA ).

    INSERT INTO ZSDT0094 VALUES GW_ZSDT0094.

    COMMIT WORK.

    CLEAR: GW_ZSDT0094.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_PROG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VALUES_DOMVALUE_L  text
*      <--P_PROGRAMA  text
*----------------------------------------------------------------------*
FORM GET_PROG  USING    P_VALUES_DOMVALUE_L
               CHANGING P_PROGRAMA.

  CASE P_VALUES_DOMVALUE_L.
    WHEN '22'. P_PROGRAMA = 'ZSDR0022'.
    WHEN '26'. P_PROGRAMA = 'ZSDR0026'.
    WHEN '16'. P_PROGRAMA = 'ZSDR016'.
    WHEN '42'. P_PROGRAMA = 'ZSDR0042'.
    WHEN '0A'. P_PROGRAMA = 'SAPMV60A'.
    WHEN 'UI'. P_PROGRAMA = 'RM_MEPO_GUI'.
  ENDCASE.

ENDFORM.
