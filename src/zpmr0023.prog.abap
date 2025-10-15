 REPORT ZPMR0023.

*&---------------------------------------------------------------------*
*&      TABELAS
*&---------------------------------------------------------------------*
 TABLES: VIQMEL, IFLO, ITOB, COSP, T247.
*&---------------------------------------------------------------------*
*&      TYPES
*&---------------------------------------------------------------------*
 TYPES: BEGIN OF TY_IFLO_LOC_CAT,
          TPLNR TYPE IFLO-TPLNR.
 TYPES:END OF TY_IFLO_LOC_CAT.

 TYPES: BEGIN OF TY_V_EQUI_EQUIP_OBJ,
          EQUNR TYPE V_EQUI-EQUNR.
 TYPES:END OF TY_V_EQUI_EQUIP_OBJ.

 TYPES: BEGIN OF TY_SAIDA,
          QMNUM TYPE VIQMEL-QMNUM,
          BUKRS TYPE VIQMEL-BUKRS,
          BUTXT TYPE T001-BUTXT,
          SWERK TYPE VIQMEL-SWERK,
          TPLNR TYPE IFLO-TPLNR,
          TPLMA TYPE IFLO-TPLMA,
          PLTX2 TYPE IFLO-PLTXT,
          PLTX1 TYPE IFLO-PLTXT,
          QMCOD TYPE VIQMEL-QMCOD,
          TXTQM TYPE RQM00-TXTQM,
          PAR01 TYPE P DECIMALS 2,
          PAR02 TYPE P DECIMALS 2,
          PAR03 TYPE P DECIMALS 2,
          PAR04 TYPE P DECIMALS 2,
          PAR05 TYPE P DECIMALS 2,
          PAR06 TYPE P DECIMALS 2,
          PAR07 TYPE P DECIMALS 2,
          PAR08 TYPE P DECIMALS 2,
          PAR09 TYPE P DECIMALS 2,
          PAR10 TYPE P DECIMALS 2,
          PAR11 TYPE P DECIMALS 2,
          PAR12 TYPE P DECIMALS 2,
          PAR13 TYPE P DECIMALS 2.
 TYPES:END OF TY_SAIDA.

 TYPES:  BEGIN OF TY_VIQMMA,
           QMNUM  TYPE  VIQMMA-QMNUM,
           AUFNR  TYPE  AUFK-AUFNR,
           QMANUM TYPE VIQMMA-QMANUM,
         END OF TY_VIQMMA,

         BEGIN OF TY_VIAUFKST,
           BUKRS TYPE VIAUFKST-BUKRS,
           AUFNR TYPE VIAUFKST-AUFNR,
           SWERK TYPE VIAUFKST-SWERK,
           TPLNR TYPE VIAUFKST-TPLNR,
           AUART TYPE VIAUFKST-AUART,
           PLTXT TYPE IFLO-PLTXT,
           VORNR TYPE AFVC-VORNR,
           LTXA1 TYPE AFVC-LTXA1,
           VAPLZ TYPE VIAUFKST-VAPLZ,
           EQUNR TYPE VIAUFKST-EQUNR,
           TXT   TYPE V_AUART-TXT,
           BUTXT TYPE T001-BUTXT,
         END OF TY_VIAUFKST,

         BEGIN OF TY_VIQMEL_,
           BUKRS TYPE VIQMEL-BUKRS,
           SWERK TYPE VIQMEL-SWERK,
           QMNUM TYPE VIQMEL-QMNUM,
           AUSVN TYPE VIQMEL-AUSVN,
           QMCOD TYPE VIQMEL-QMCOD,
           QMART TYPE VIQMEL-QMART,
           TPLNR TYPE VIQMEL-TPLNR,
           AUSBS TYPE VIQMEL-AUSBS,
           AUZTV TYPE VIQMEL-AUZTV,
           AUZTB TYPE VIQMEL-AUZTB,
           AUSZT TYPE P DECIMALS 2,
         END OF TY_VIQMEL_,

         BEGIN OF TY_V_AUART,
           AUART TYPE V_AUART-AUART,
           TXT   TYPE V_AUART-TXT,
           SPRAS TYPE V_AUART-SPRAS,
         END OF TY_V_AUART,

         BEGIN OF TY_IFLO,
           TPLNR TYPE IFLO-TPLNR,
           PLTXT TYPE IFLO-PLTXT,
         END OF TY_IFLO.

*&---------------------------------------------------------------------*
*&      TYPES
*&---------------------------------------------------------------------*
 DATA: IT_IFLO_LOC_CAT     TYPE STANDARD TABLE OF TY_IFLO_LOC_CAT,
       IT_IFLO             TYPE STANDARD TABLE OF TY_IFLO,
       WA_IFLO             TYPE TY_IFLO,
       IT_V_AUART          TYPE STANDARD TABLE OF TY_V_AUART WITH HEADER LINE,
       IT_VIQMMA           TYPE STANDARD TABLE OF TY_VIQMMA,
       IT_VIQMMA_          TYPE STANDARD TABLE OF VIQMMA WITH HEADER LINE,
       WA_VIQMMA           TYPE TY_VIQMMA,
       IT_VIQUEL           TYPE STANDARD TABLE OF VIQMEL,
*       WA_VIQMEL           TYPE VIQMEL,
       IT_VIAUFKST         TYPE STANDARD TABLE OF TY_VIAUFKST,
       WA_VIAUFKST         TYPE TY_VIAUFKST,
       IT_V_EQUI_EQUIP_OBJ TYPE STANDARD TABLE OF TY_V_EQUI_EQUIP_OBJ,
       IT_VIQMEL           TYPE STANDARD TABLE OF VIQMEL,
       IT_VIQMEL_          TYPE STANDARD TABLE OF TY_VIQMEL_ WITH HEADER LINE,
       IT_T001             TYPE STANDARD TABLE OF T001 WITH HEADER LINE,
       IT_NOTAS            TYPE TABLE OF BAPI2080_1,
       IT_SAIDA            TYPE STANDARD TABLE OF TY_SAIDA,
       WA_BDCDATA          LIKE BDCDATA,
       IT_BDCDATA          LIKE STANDARD TABLE OF WA_BDCDATA,
       R_0010              TYPE RANGE OF IFLO-TPLNR WITH HEADER LINE,
       R_0020              TYPE RANGE OF IFLO-TPLNR WITH HEADER LINE,
       R_TPLNR             TYPE RANGE OF IFLO-TPLNR WITH HEADER LINE,
       T_TPLNR             TYPE RANGE OF IFLO-TPLNR WITH HEADER LINE,
       IT_DETALHE          TYPE STANDARD TABLE OF ZPME_INDISPON,
       IT_SAIDA_SMARTFORM  TYPE STANDARD TABLE OF ZPME_INDISPON,
       IT_TEMPRESA         TYPE STANDARD TABLE OF ZPME_INDISPON WITH HEADER LINE,
       IT_TOT_EMPRESA      TYPE STANDARD TABLE OF ZPME_INDISPON WITH HEADER LINE,
       IT_TOT_EMPRESA_LOC  TYPE STANDARD TABLE OF ZPME_INDISPON WITH HEADER LINE,
       IT_TOT_EMPRESA_AUX  TYPE STANDARD TABLE OF ZPME_INDISPON WITH HEADER LINE,
       IT_TOT_CENTRO_AUX   TYPE STANDARD TABLE OF ZPME_INDISPON WITH HEADER LINE,
       IT_TOT_CENTRO       TYPE STANDARD TABLE OF ZPME_INDISPON WITH HEADER LINE,
       IT_SAIDA_SMARTAUX   TYPE STANDARD TABLE OF ZPME_INDISPON WITH HEADER LINE,
       IT_TOT_GERAL        TYPE STANDARD TABLE OF ZPME_INDISPON WITH HEADER LINE,
       IT_TOT_GERAL_SMART  TYPE STANDARD TABLE OF ZPME_INDISPON WITH HEADER LINE,
       IT_DETALHE_SMART    TYPE STANDARD TABLE OF ZPME_DETALHES WITH HEADER LINE,
       IT_VIQSAIDA         TYPE STANDARD TABLE OF ZPME_DETALHES,
       WA_VIQSAIDA         TYPE ZPME_DETALHES,
       IT_VIQSAIDA_DET     TYPE STANDARD TABLE OF ZPME_DETALHES_NOTA WITH HEADER LINE,
*       WA_VIQSAIDA         TYPE ZPME_DETALHES,
       IT_ORDERNA          TYPE STANDARD TABLE OF ZPME_DETALHES WITH HEADER LINE.

 DATA: IT_FIELDCATALOG_2   TYPE LVC_T_FCAT,
       WA_FIELDCATALOG_2   TYPE LVC_S_FCAT,
       G_CUSTOM_CONTAINER3 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_CONTAINER         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       CTL_ALV_2           TYPE REF TO CL_GUI_ALV_GRID,
       IT_SORT3            TYPE LVC_T_SORT.




 DATA: DTINI TYPE SY-DATUM,
       DTFIM TYPE SY-DATUM.

 DATA: DTINIS TYPE SY-DATUM,
       DTFIMS TYPE SY-DATUM.

 DATA: DAY   TYPE I,
       MONTH TYPE I,
       YEAR  TYPE I.

 DATA: FLTP_CHAR     TYPE IMRC_TOTAC.

 DATA: WA_IFLO_LOC_CAT     TYPE TY_IFLO_LOC_CAT,
       WA_V_EQUI_EQUIP_OBJ TYPE TY_V_EQUI_EQUIP_OBJ,
       WA_VIQMEL           TYPE VIQMEL,
       WA_NOTAS            TYPE BAPI2080_1,
       WA_SAIDA            TYPE TY_SAIDA.

 FIELD-SYMBOLS <WA_SAIDA_AUX> TYPE TY_SAIDA.

 DATA: CLICKS TYPE SY-TABIX.

 DATA: VL_MONTHS  TYPE VTBBEWE-ATAGE,
       VL_DATE_TO TYPE VTBBEWE-DBERBIS,
       CONT_MES   TYPE N LENGTH 2.

 DATA: EAUSZT TYPE RIWO00-EAUSZT.

 CONSTANTS: C_FORM TYPE RS38L_FNAM VALUE 'ZPMR0023'.

*&---------------------------------------------------------------------*
*&      TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
 SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
 SELECT-OPTIONS: P_BUKRS FOR VIQMEL-BUKRS OBLIGATORY,                  "Empresa
                 P_SWERK FOR VIQMEL-SWERK,                             "Centro
                 P_ANO   FOR COSP-GJAHR NO-EXTENSION NO INTERVALS OBLIGATORY DEFAULT SY-DATUM(4),
                 P_MES   FOR T247-MNR OBLIGATORY NO-EXTENSION,
                 P_FLTYP FOR IFLO-FLTYP NO-EXTENSION NO INTERVALS OBLIGATORY DEFAULT 'E',"AOENNING.
                 P_TPLNR FOR VIQMEL-TPLNR.                             "Local de Instalação
 SELECTION-SCREEN END OF BLOCK B1.

 CLASS EVENT DEFINITION.

   PUBLIC SECTION.
     METHODS:
       HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN SENDER,
       ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID.
 ENDCLASS.
*
* CLASS LCL_EVENTOS_3 DEFINITION.
*
*   PUBLIC SECTION.
*     CLASS-METHODS:
*       ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*         IMPORTING E_ROW_ID E_COLUMN_ID.
*
* ENDCLASS.

* CLASS LCL_EVENTOS DEFINITION.
*
*   PUBLIC SECTION.
*     CLASS-METHODS:
*       ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*         IMPORTING E_ROW_ID E_COLUMN_ID.
*
* ENDCLASS.                    "LCL_EVENT DEFINITION

 DATA(OBJ_EVEN) = NEW EVENT( ).

 CLASS EVENT IMPLEMENTATION.

   METHOD HANDLE_DOUBLE_CLICK.
     CHECK E_ROW-ROWTYPE IS INITIAL.
     PERFORM DETALHES_SAIDA USING E_ROW E_COLUMN-FIELDNAME.
   ENDMETHOD.

   METHOD ON_HOTSPOT_CLICK.

     CASE E_COLUMN_ID-FIELDNAME.
       WHEN: 'AUFNR'."Ordem de manutenção

         DATA(WA_DETALHE_SMART1) = IT_VIQSAIDA[ E_ROW_ID-INDEX ].
         IF WA_DETALHE_SMART1-AUFNR IS NOT INITIAL.
           SET PARAMETER ID 'ANR' FIELD WA_DETALHE_SMART1-AUFNR.
           CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN .
         ENDIF.

*         TRY .
*             DATA(WA_DETALHE_SMART) = IT_VIQSAIDA[ E_ROW_ID-INDEX ].
*           CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*         ENDTRY.
*
*         FREE: IT_BDCDATA.
*         PERFORM F_INSERT_SHDB USING:
*               ' '         ' '     'T'  'KOB1'                ' ',
*               'RKAEP000'  '0110'  'X'  ' '                   ' ',
*               ' '         ' '     ' '  'BDC_CURSOR'           'P_KOKRS',
*               ' '         ' '     ' '  'BDC_OKCODE'          '=%011',
*               ' '         ' '     ' '  'P_KOKRS'             'MAGI',
*               ' '         ' '     ' '  'AUFNR-LOW'           WA_DETALHE_SMART-AUFNR,
*               ' '         ' '     ' '  'KSTAR-LOW'           ' ',
*               ' '         ' '     ' '  'R_BUDAT-LOW'         DTINIS,
*               ' '         ' '     ' '  'R_BUDAT-HIGH'        DTFIMS,
*               'SAPLALDB'  '3000'  'X'  ' '                   ' ',
*               ' '         ' '     ' '  'BDC_OKCODE'          '/EDELA',
*               'SAPLALDB'   '3000'  'X'  ' '                   ' ',
*               ' '         ' '     ' '  'BDC_SUBSCR'          'SAPLALDB                   3010SCREEN_HEADER',
*               ' '         ' '     ' '  'BDC_CURSOR'          'RSCSEL_255-SLOW_I(01)',
*               ' '         ' '     ' '  'RSCSEL_255-SLOW_I(01)'  '',
*               ' '         ' '     ' '  'BDC_OKCODE'          '=ACPT',
*               'RKAEP000'  '0110'  'X'  ' '                   ' ',
*               ' '         ' '     ' '  'BDC_OKCODE'          '=ONLI'.
*
*         CALL TRANSACTION 'KOB1' USING IT_BDCDATA MODE 'E'.

       WHEN:'QMNUM'."Notas Manuteção
*
         DATA(WA_DETALHE_SMART2) = IT_VIQSAIDA_DET[ E_ROW_ID-INDEX ].
         IF WA_DETALHE_SMART2-QMNUM IS NOT INITIAL.
           SET PARAMETER ID 'IQM' FIELD WA_DETALHE_SMART2-QMNUM.
           CALL TRANSACTION 'IW23' AND SKIP FIRST SCREEN .
         ENDIF.

       WHEN: 'EXEC_AVAR'.

         FREE IT_VIQSAIDA.
         DATA: TPARADA TYPE P DECIMALS 2.

         DATA(WA_VIQSAIDA_DET) = IT_VIQSAIDA_DET[ E_ROW_ID-INDEX ].
         LOOP AT IT_VIQMMA INTO WA_VIQMMA WHERE QMNUM = WA_VIQSAIDA_DET-QMNUM.
           IF SY-SUBRC = 0.
             WA_VIQSAIDA-QMNUM  = WA_VIQMMA-QMNUM.
           ENDIF.

           READ TABLE IT_VIQSAIDA_DET INTO WA_VIQSAIDA_DET WITH KEY QMNUM = WA_VIQMMA-QMNUM.
           IF SY-SUBRC = 0.
             WA_VIQSAIDA-SWERK  = WA_VIQSAIDA_DET-SWERK.
             WA_VIQSAIDA-AUSVN  = WA_VIQSAIDA_DET-AUSVN.
             WA_VIQSAIDA-QMCOD  = WA_VIQSAIDA_DET-QMCOD.
             WA_VIQSAIDA-QMART  = WA_VIQSAIDA_DET-QMART.
             WA_VIQSAIDA-TPLNR_ = WA_VIQSAIDA_DET-TPLNR.
             WA_VIQSAIDA-AUSZT  = WA_VIQSAIDA_DET-AUSZT.
           ENDIF.

           READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
           IF SY-SUBRC = 0.
             MOVE-CORRESPONDING WA_VIAUFKST TO WA_VIQSAIDA.
           ENDIF.

           READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = WA_VIQSAIDA_DET-TPLNR.
           IF SY-SUBRC = 0.
             WA_VIQSAIDA-PLTXT_ = WA_IFLO-PLTXT.
           ENDIF.



           APPEND WA_VIQSAIDA TO IT_VIQSAIDA.
           CLEAR WA_VIQSAIDA.
           CLEAR WA_VIAUFKST.
           CLEAR WA_IFLO.
           CLEAR WA_VIQMMA.
           CLEAR WA_VIQSAIDA_DET.
         ENDLOOP.

         SORT IT_VIQSAIDA ASCENDING BY AUSVN QMNUM AUFNR VORNR AUSVN.

         CALL SCREEN 0200.

     ENDCASE.




   ENDMETHOD.

 ENDCLASS.

*&---------------------------------------------------------------------*
*&      SELEÇÂO
*&---------------------------------------------------------------------*
 START-OF-SELECTION.

   PERFORM INICIA.

 FORM INICIA.
*   IF P_TPLNR IS NOT INITIAL AND ( P_FLTYP IS NOT INITIAL ) OR
*        P_TPLNR IS INITIAL AND P_FLTYP IS INITIAL.
*     MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
*     STOP.
*   ENDIF.

   IF P_ANO-LOW IS INITIAL OR P_MES-HIGH IS INITIAL.
     MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
     STOP.
   ENDIF.

   DTINI  = |{ P_ANO-LOW }{ P_MES-LOW  }{ '01' }|.
   DTINIS = |{ '01' }{ P_MES-LOW }{ P_ANO-LOW }|.

   IF P_MES-HIGH IS NOT INITIAL.
     MONTH = P_MES-HIGH.
     YEAR  = P_ANO-LOW.

     CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
       EXPORTING
         I_DATE_MONTH = MONTH
         I_DATE_YEAR  = YEAR
       IMPORTING
         E_MAX_DAYS   = DAY.
   ELSE.

     MONTH = P_MES-LOW.
     YEAR  = P_ANO-LOW.

     CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
       EXPORTING
         I_DATE_MONTH = MONTH
         I_DATE_YEAR  = YEAR
       IMPORTING
         E_MAX_DAYS   = DAY.
   ENDIF.

   IF MONTH < '10'.
     DATA: ADIC_ZERO TYPE CHAR2.
     ADIC_ZERO = |{ 0 }{ MONTH }|.
*     MONTH = |{ '0' }{ MONTH }|.
     DTFIM  = |{ YEAR }{ ADIC_ZERO }{ DAY  }|.
     DTFIMS = |{ DAY  }{ ADIC_ZERO  }{ YEAR }|.
   ELSE.
     DTFIM  = |{ YEAR }{ MONTH }{ DAY  }|.
     DTFIMS = |{ DAY  }{ MONTH  }{ YEAR }|.
   ENDIF.


*   IF P_TPLNR IS NOT INITIAL.
*     PERFORM SELECIONA_DADOS_LOC_CAT.                "Grupo de Filtros de Localização/Categoria(Dominante sobre Equip/Obj)
*   ENDIF.

   PERFORM SELECIONA_NOTAS.
   PERFORM ORGANIZA_DADOS_ANUAL.
   CALL SCREEN 0100.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_LOC_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM SELECIONA_DADOS_LOC_CAT .

   IF P_FLTYP IS NOT INITIAL.

     SELECT IFLO~TPLNR
       INTO CORRESPONDING FIELDS OF TABLE IT_IFLO_LOC_CAT
       FROM IFLO
      INNER JOIN J_1BBRANCH ON ( J_1BBRANCH~BRANCH EQ IFLO~SWERK )
      WHERE IFLO~SWERK IN P_SWERK
        AND IFLO~FLTYP IN P_FLTYP
     AND J_1BBRANCH~BUKRS IN P_BUKRS.

   ELSE.

     SELECT IFLO~TPLNR
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO_LOC_CAT
         FROM IFLO
        INNER JOIN J_1BBRANCH ON ( J_1BBRANCH~BRANCH EQ IFLO~SWERK )
        WHERE IFLO~SWERK IN P_SWERK
          AND IFLO~TPLNR IN P_TPLNR
     AND J_1BBRANCH~BUKRS IN P_BUKRS.

   ENDIF.

   SORT IT_IFLO_LOC_CAT BY TPLNR.
   DELETE ADJACENT DUPLICATES FROM IT_IFLO_LOC_CAT COMPARING TPLNR.

   IF IT_IFLO_LOC_CAT IS INITIAL.
     MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
     STOP.
   ENDIF.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_EQUIP_OBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM SELECIONA_DADOS_EQUIP_OBJ .

   SELECT V_EQUI~EQUNR
       INTO CORRESPONDING FIELDS OF TABLE IT_V_EQUI_EQUIP_OBJ
       FROM V_EQUI
      INNER JOIN J_1BBRANCH ON ( J_1BBRANCH~BRANCH EQ V_EQUI~SWERK )
      WHERE V_EQUI~SWERK IN P_SWERK
   AND J_1BBRANCH~BUKRS IN P_BUKRS.

   SORT IT_V_EQUI_EQUIP_OBJ BY EQUNR.
   DELETE ADJACENT DUPLICATES FROM IT_V_EQUI_EQUIP_OBJ COMPARING EQUNR.

   IF IT_V_EQUI_EQUIP_OBJ IS INITIAL.
     MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
     STOP.
   ENDIF.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM SELECIONA_NOTAS .

   DATA: FUNCLOC_INT   TYPE BAPI_ITOB_PARMS-FUNCLOC_INT,
         EQUIPMENT     TYPE BAPI_ITOB_PARMS-EQUIPMENT,
         WA_BAPIRETURN TYPE BAPIRETURN,
         NOTIFICATION  TYPE TABLE OF BAPI2080_1.

*   IF IT_IFLO_LOC_CAT IS NOT INITIAL.
*
*     SELECT *
*       FROM VIQMEL
*       INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL
*       FOR ALL ENTRIES IN IT_IFLO_LOC_CAT
*       WHERE TPLNR IN IT_IFLO_LOC_CAT-TPLNR
*         AND SWERK IN P_SWERK
*         AND BUKRS IN P_BUKRS
*         AND AUSZT NE SPACE
*         AND AUSVN BETWEEN DTINI AND DTFIM
*         AND AUSBS BETWEEN DTINI AND DTFIM
*         AND QMGRP EQ 'F0000030'
*         AND ( QMCOD EQ '0010' OR
*               QMCOD EQ '0020' ).
*
*   ELSE.

   SELECT *
     FROM VIQMEL
     INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL
     WHERE SWERK IN P_SWERK
       AND TPLNR IN P_TPLNR
       AND BUKRS IN P_BUKRS
       AND AUSZT NE SPACE
       AND AUSVN BETWEEN DTINI AND DTFIM
       AND AUSBS BETWEEN DTINI AND DTFIM
       AND QMGRP EQ 'F0000030'
       AND ( QMCOD EQ '0010' OR
             QMCOD EQ '0020' ).

   SELECT *
   FROM T001
   INTO CORRESPONDING FIELDS OF TABLE IT_T001
   FOR ALL ENTRIES IN IT_VIQMEL
   WHERE BUKRS EQ IT_VIQMEL-BUKRS.

*   ENDIF.

   "Deleta os registros que não contém o período selecionado
*  DELETE IT_VIQMEL  WHERE ( AUSVN LT P_DATES-LOW AND AUSBS LT P_DATES-LOW )
*                       OR ( AUSVN GT P_DATES-HIGH AND AUSBS GT P_DATES-HIGH ).
*  DELETE IT_VIQMEL  WHERE ( AUSVN LT DTINI AND AUSBS LT DTINI )
*                       OR ( AUSVN GT DTFIM AND AUSBS GT DTFIM ).

*  DELETE IT_VIQMEL WHERE AUSVN IS INITIAL OR AUSBS IS INITIAL.

   SORT IT_VIQMEL BY QMNUM SWERK.



   IF IT_VIQMEL IS INITIAL.
     MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
     STOP.
   ENDIF.

   "Ajusta extremos fora da range do parâmetro p_dates
*  LOOP AT IT_VIQMEL INTO WA_VIQMEL.
*    IF WA_VIQMEL-AUSVN LT P_DATES-LOW.
*      MOVE P_DATES-LOW TO WA_VIQMEL-AUSVN.
*      MOVE '000000' TO WA_VIQMEL-AUZTV.
*    ENDIF.
*    IF WA_VIQMEL-AUSBS GT P_DATES-HIGH.
*      MOVE P_DATES-HIGH TO WA_VIQMEL-AUSBS.
*      MOVE '235959' TO WA_VIQMEL-AUZTB.
*    ENDIF.
*    MODIFY IT_VIQMEL FROM WA_VIQMEL INDEX SY-TABIX.
*  ENDLOOP.

 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS_SINTETICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM ORGANIZA_DADOS_ANUAL.

   DATA: CONT  TYPE P DECIMALS 2,
         VCONT TYPE P DECIMALS 2,
         PERIO TYPE P DECIMALS 2.

   DATA: VL_DURATION TYPE SYTABIX,
         VL_TPLMA    TYPE IFLO-TPLMA,
         VL_SHTXT    TYPE ITOB-SHTXT,
         VL_EQART    TYPE V_EQUI-EQART.

   DATA: IT_SAIDA_AUX TYPE STANDARD TABLE OF TY_SAIDA,
         WA_SAIDA_AUX TYPE TY_SAIDA.

   LOOP AT IT_VIQMEL INTO WA_VIQMEL.

*     WA_SAIDA_AUX-QMNUM = WA_VIQMEL-QMNUM.
     WA_SAIDA_AUX-BUKRS = WA_VIQMEL-BUKRS.                                     "Empresa
     WA_SAIDA_AUX-SWERK = WA_VIQMEL-SWERK.                                     "Centro
     WA_SAIDA_AUX-TPLNR = WA_VIQMEL-TPLNR.
     WA_SAIDA_AUX-QMCOD = WA_VIQMEL-QMCOD.

     READ TABLE IT_T001 WITH KEY BUKRS =  WA_VIQMEL-BUKRS.
     WA_SAIDA_AUX-BUTXT = IT_T001-BUTXT.

     IF P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL.

       SELECT SINGLE TPLMA PLTXT
         FROM IFLO
         INTO (VL_TPLMA, WA_SAIDA_AUX-PLTX1 )
       WHERE TPLNR EQ WA_VIQMEL-TPLNR.

       IF SY-SUBRC IS INITIAL.
         WA_SAIDA_AUX-TPLMA = VL_TPLMA.                                           "Nº do Local Superior

         SELECT SINGLE PLTXT
           FROM IFLO
           INTO WA_SAIDA_AUX-PLTX2                                                "Local Pai
         WHERE TPLNR EQ VL_TPLMA.

       ENDIF.
     ENDIF.

     CALL FUNCTION 'QPK1_CODE_TEXT'
       EXPORTING
         I_KATALOGART      = WA_VIQMEL-QMKAT
         I_CODEGRUPPE      = WA_VIQMEL-QMGRP
         I_CODE            = WA_VIQMEL-QMCOD
         I_SPRACHE         = SY-LANGU
       IMPORTING
         E_TEXT            = WA_SAIDA_AUX-TXTQM                                "Descrição da Parada
       EXCEPTIONS
         NO_MATCH_IN_RANGE = 1
         OTHERS            = 2.

     PERFORM CALCULAR_TEMPO_PARADA USING WA_VIQMEL
                                   CHANGING WA_SAIDA_AUX.

     APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
     CLEAR: WA_SAIDA_AUX.

     SORT IT_SAIDA_AUX ASCENDING BY SWERK.

   ENDLOOP.

   FREE: R_0010, R_0020.

   LOOP AT IT_SAIDA_AUX ASSIGNING <WA_SAIDA_AUX>.

     R_0010-SIGN = 'I'.
     R_0010-OPTION = 'EQ'.

     R_0020-SIGN = 'I'.
     R_0020-OPTION = 'EQ'.

     CASE WA_SAIDA_AUX-QMCOD.
       WHEN '0010'. R_0010-LOW = <WA_SAIDA_AUX>-TPLNR.    APPEND R_0010.
       WHEN '0020'. R_0020-LOW = <WA_SAIDA_AUX>-TPLNR.    APPEND R_0020.
     ENDCASE.

     CONT_MES = 0.
     DO .
       IF CONT_MES EQ 13.
         EXIT.
       ENDIF.

       ADD 1 TO CONT_MES.
       PERFORM CALC_MES USING CONT_MES.

     ENDDO.

     <WA_SAIDA_AUX>-PAR13 = <WA_SAIDA_AUX>-PAR01 + <WA_SAIDA_AUX>-PAR02 +
                            <WA_SAIDA_AUX>-PAR03 + <WA_SAIDA_AUX>-PAR04 +
                            <WA_SAIDA_AUX>-PAR05 + <WA_SAIDA_AUX>-PAR06 +
                            <WA_SAIDA_AUX>-PAR07 + <WA_SAIDA_AUX>-PAR08 +
                            <WA_SAIDA_AUX>-PAR09 + <WA_SAIDA_AUX>-PAR10 +
                            <WA_SAIDA_AUX>-PAR11 + <WA_SAIDA_AUX>-PAR12.


     COLLECT <WA_SAIDA_AUX> INTO IT_SAIDA.

     CLEAR: R_0010, R_0020.

   ENDLOOP.

   SORT IT_SAIDA BY BUKRS ASCENDING
                    SWERK ASCENDING
                    TPLMA ASCENDING
                    TPLNR ASCENDING
                    QMCOD ASCENDING.

   LOOP AT IT_SAIDA INTO WA_SAIDA .

     IF  DTFIMS+2(2) < SY-DATUM+4(2).
       PERIO = DTFIMS+2(2).
       CONT = DTINIS+2(2).
       VCONT =  ( PERIO - CONT ).
       VCONT = VCONT + 1.
       WA_SAIDA-PAR13 = WA_SAIDA-PAR13 / VCONT.

     ELSE.
       PERIO = SY-DATUM+4(2).
       CONT = DTINIS+2(2).
       VCONT =  ( PERIO - CONT ).
       VCONT = VCONT + 1.
       WA_SAIDA-PAR13 = WA_SAIDA-PAR13 / VCONT.
     ENDIF.



     MODIFY IT_SAIDA FROM WA_SAIDA .
     CLEAR WA_SAIDA.
     CLEAR CONT.
     CLEAR VCONT.
     CLEAR PERIO.
   ENDLOOP.

   SORT IT_SAIDA ASCENDING BY BUKRS TPLMA.

   PERFORM INFO_SMARTFORM.

 ENDFORM.


 INCLUDE ZPMR0023_0100.
*&---------------------------------------------------------------------*
*&      Form  CALCULAR_TEMPO_PARADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM CALCULAR_TEMPO_PARADA USING WA_VIQMEL TYPE VIQMEL
                            CHANGING WA_SAIDA_AUX TYPE TY_SAIDA.

   DATA: VL_DURATION   TYPE SYTABIX,
         VL_AUSVN      TYPE I, "MES
         VL_AUSBS      TYPE I, "MES
         VL_PARAD      TYPE I,
         VL_PARAD1     TYPE P DECIMALS 2,
         CONVERT       TYPE P DECIMALS 2,
         VL_END_DATE   TYPE SY-DATUM,
         VL_START_DATE TYPE SY-DATUM,
         VL_START_TIME TYPE SY-UZEIT.

*   CALL FUNCTION 'SWI_DURATION_DETERMINE'
*        EXPORTING
*          START_DATE = WA_VIQMEL-AUSVN
*          END_DATE   = WA_VIQMEL-AUSBS
*          START_TIME = WA_VIQMEL-AUZTV
*          END_TIME   = WA_VIQMEL-AUZTB
*        IMPORTING
*          DURATION   = VL_DURATION.

   VL_AUSVN = WA_VIQMEL-AUSVN+4(2).
   VL_AUSBS = WA_VIQMEL-AUSBS+4(2).
   VL_START_DATE = WA_VIQMEL-AUSVN.
   VL_START_TIME = WA_VIQMEL-AUZTV.

   IF VL_AUSVN EQ VL_AUSBS.

     PERFORM FLTP_CHAR_CONVERSION_PAK_F40
               USING FLTP_CHAR
                 WA_VIQMEL-MAUEH
                 WA_VIQMEL-AUSZT.

     CALL FUNCTION 'MOVE_CHAR_TO_NUM'
       EXPORTING
         CHR = FLTP_CHAR
       IMPORTING
         NUM = CONVERT.



     CASE WA_VIQMEL-AUSVN+4(2).
       WHEN 01. WA_SAIDA_AUX-PAR01 =  ( CONVERT * 60 ).
       WHEN 02. WA_SAIDA_AUX-PAR02 =  ( CONVERT * 60 ).
       WHEN 03. WA_SAIDA_AUX-PAR03 =  ( CONVERT * 60 ).
       WHEN 04. WA_SAIDA_AUX-PAR04 =  ( CONVERT * 60 ).
       WHEN 05. WA_SAIDA_AUX-PAR05 =  ( CONVERT * 60 ).
       WHEN 06. WA_SAIDA_AUX-PAR06 =  ( CONVERT * 60 ).
       WHEN 07. WA_SAIDA_AUX-PAR07 =  ( CONVERT * 60 ).
       WHEN 08. WA_SAIDA_AUX-PAR08 =  ( CONVERT * 60 ).
       WHEN 09. WA_SAIDA_AUX-PAR09 =  ( CONVERT * 60 ).
       WHEN 10. WA_SAIDA_AUX-PAR10 =  ( CONVERT * 60 ).
       WHEN 11. WA_SAIDA_AUX-PAR11 =  ( CONVERT * 60 ).
       WHEN 12. WA_SAIDA_AUX-PAR12 =  ( CONVERT * 60 ).
     ENDCASE.

     CLEAR: VL_PARAD.

   ELSE.

     WHILE VL_AUSVN NE VL_AUSBS + 1.

       IF VL_AUSVN NE VL_AUSBS.
         CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
           EXPORTING
             DAY_IN            = VL_START_DATE
           IMPORTING
             LAST_DAY_OF_MONTH = VL_END_DATE
           EXCEPTIONS
             DAY_IN_NOT_VALID  = 1
             OTHERS            = 2.

         CALL FUNCTION 'SWI_DURATION_DETERMINE'
           EXPORTING
             START_DATE = VL_START_DATE
             END_DATE   = VL_END_DATE
             START_TIME = VL_START_TIME
             END_TIME   = '235959'
           IMPORTING
             DURATION   = VL_PARAD.
       ELSE.
         CALL FUNCTION 'SWI_DURATION_DETERMINE'
           EXPORTING
             START_DATE = VL_START_DATE
             END_DATE   = WA_VIQMEL-AUSBS
             START_TIME = VL_START_TIME
             END_TIME   = WA_VIQMEL-AUZTB
           IMPORTING
             DURATION   = VL_PARAD.
       ENDIF.

       VL_PARAD1 = ( VL_PARAD / 60 ) / 60.

       CASE VL_START_DATE+4(2).
         WHEN 01. WA_SAIDA_AUX-PAR01 = ( VL_PARAD1 * 60 ).
         WHEN 02. WA_SAIDA_AUX-PAR02 = ( VL_PARAD1 * 60 ).
         WHEN 03. WA_SAIDA_AUX-PAR03 = ( VL_PARAD1 * 60 ).
         WHEN 04. WA_SAIDA_AUX-PAR04 = ( VL_PARAD1 * 60 ).
         WHEN 05. WA_SAIDA_AUX-PAR05 = ( VL_PARAD1 * 60 ).
         WHEN 06. WA_SAIDA_AUX-PAR06 = ( VL_PARAD1 * 60 ).
         WHEN 07. WA_SAIDA_AUX-PAR07 = ( VL_PARAD1 * 60 ).
         WHEN 08. WA_SAIDA_AUX-PAR08 = ( VL_PARAD1 * 60 ).
         WHEN 09. WA_SAIDA_AUX-PAR09 = ( VL_PARAD1 * 60 ).
         WHEN 10. WA_SAIDA_AUX-PAR10 = ( VL_PARAD1 * 60 ).
         WHEN 11. WA_SAIDA_AUX-PAR11 = ( VL_PARAD1 * 60 ).
         WHEN 12. WA_SAIDA_AUX-PAR12 = ( VL_PARAD1 * 60 ).
       ENDCASE.

       VL_AUSVN = VL_AUSVN + 1.
       VL_START_DATE = VL_END_DATE + 1.
       VL_START_TIME = '000000'.
       CLEAR: VL_PARAD.
     ENDWHILE.

   ENDIF.

 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALCULAR_INDISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM CALCULAR_INDISP .

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INFO_SMARTFORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM INFO_SMARTFORM .

   DATA: TPARADA TYPE P DECIMALS 2.

   APPEND LINES OF R_0010 TO T_TPLNR.
   APPEND LINES OF R_0020 TO T_TPLNR.

   IT_SAIDA_SMARTFORM = VALUE #( FOR LS IN IT_SAIDA ( CORRESPONDING #( LS ) ) ).


   SELECT *
        FROM VIQMEL
        INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
        FOR ALL ENTRIES IN IT_SAIDA_SMARTFORM
        WHERE TPLNR EQ IT_SAIDA_SMARTFORM-TPLNR
          AND SWERK EQ IT_SAIDA_SMARTFORM-SWERK
          AND QMGRP EQ 'F0000030'
          AND QMCOD IN ('0010', '0020').
*            AND TPLNR IN T_TPLNR.

   SORT IT_VIQMEL_ ASCENDING BY QMNUM.

   SELECT *
   FROM IFLO
   INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
   FOR ALL ENTRIES IN IT_VIQMEL_
   WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

   SELECT *
     FROM VIQMMA
     INTO TABLE IT_VIQMMA_
     FOR ALL ENTRIES IN IT_VIQMEL_
     WHERE QMNUM EQ IT_VIQMEL_-QMNUM.

   SORT IT_VIQMMA_ ASCENDING BY QMNUM.

   IF IT_VIQMMA_[] IS NOT INITIAL.
     LOOP AT IT_VIQMMA_.
       WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
       WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
       WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
       APPEND WA_VIQMMA TO IT_VIQMMA.
     ENDLOOP.
   ENDIF.

   CHECK IT_VIQMMA IS NOT INITIAL.

   SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT J~BUTXT
   FROM VIAUFKST AS B
   INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
   INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
   INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
   INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
   INNER JOIN T001 AS J ON J~BUKRS EQ B~BUKRS
   INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
    FOR ALL ENTRIES IN IT_VIQMMA
    WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
      AND I~SPRAS EQ 'P'.

   CHECK IT_VIAUFKST[] IS NOT INITIAL.

   LOOP AT IT_VIAUFKST INTO WA_VIAUFKST.
     READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY AUFNR = WA_VIAUFKST-AUFNR.
     IF SY-SUBRC = 0.
       MOVE-CORRESPONDING WA_VIAUFKST TO IT_DETALHE_SMART.
     ENDIF.

     READ TABLE IT_VIQMEL_ WITH KEY QMNUM = WA_VIQMMA-QMNUM.
     IF SY-SUBRC = 0.
       IT_DETALHE_SMART-QMNUM  = IT_VIQMEL_-QMNUM.
       IT_DETALHE_SMART-AUSVN  = IT_VIQMEL_-AUSVN.
       IT_DETALHE_SMART-QMCOD  = IT_VIQMEL_-QMCOD.
       IT_DETALHE_SMART-QMART  = IT_VIQMEL_-QMART.
       IT_DETALHE_SMART-TPLNR_ = IT_VIQMEL_-TPLNR.

       IF IT_VIQMEL_-AUSZT IS NOT INITIAL.
         TPARADA = ( IT_VIQMEL_-AUSZT / 60 ).
         TPARADA = ( TPARADA / 60 ).
         IT_DETALHE_SMART-AUSZT  = TPARADA.
       ENDIF.
     ENDIF.

     READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
     IF SY-SUBRC = 0.
       IT_DETALHE_SMART-PLTXT_ = WA_IFLO-PLTXT.
     ENDIF.

     APPEND IT_DETALHE_SMART.
     CLEAR IT_DETALHE_SMART.
     CLEAR WA_VIAUFKST.
     CLEAR IT_VIQMEL_.
     CLEAR WA_VIQMMA.
     CLEAR WA_IFLO.
   ENDLOOP.


   LOOP AT IT_DETALHE_SMART[] ASSIGNING FIELD-SYMBOL(<SAIDA>).



     IT_ORDERNA-BUKRS = <SAIDA>-BUKRS.

     SELECT SINGLE TPLNR
       FROM IFLO
       INTO @DATA(TPLNR)
     WHERE TPLNR EQ @<SAIDA>-TPLNR_.

     IF SY-SUBRC IS INITIAL.
       SELECT SINGLE PLTXT
         FROM IFLO
         INTO IT_ORDERNA-PLTXT1
         WHERE TPLNR EQ TPLNR.
     ENDIF.

     APPEND IT_ORDERNA.

   ENDLOOP.

   DELETE IT_DETALHE_SMART WHERE AUSVN NOT BETWEEN DTINI AND DTFIM.

   SORT IT_DETALHE_SMART BY AUSVN SWERK PLTXT1.
   SORT IT_ORDERNA BY BUKRS.
   DELETE ADJACENT DUPLICATES FROM IT_ORDERNA COMPARING BUKRS.


 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FLTP_CHAR_CONVERSION_PAK_F40
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FLTP_CHAR  text
*      -->P_WA_VIQMEL_AUSZT  text
*      -->P_WA_VIQMEL_MAUEH  text
*----------------------------------------------------------------------*
 FORM FLTP_CHAR_CONVERSION_PAK_F40  USING    CHAR_WERT
                                             EINHEIT
                                             FLTP_WERT.

   CLEAR CHAR_WERT.
   CHECK NOT EINHEIT IS INITIAL.

   CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
     EXPORTING
       CHAR_UNIT       = EINHEIT
       DECIMALS        = 2
       EXPONENT        = 0
       FLTP_VALUE_SI   = FLTP_WERT
       INDICATOR_VALUE = ABAP_TRUE
       MASC_SYMBOL     = ABAP_FALSE
     IMPORTING
       CHAR_VALUE      = CHAR_WERT.

 ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DETALHES_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW  text
*----------------------------------------------------------------------*
 FORM DETALHES_SAIDA USING P_ROW P_COLUMN.

   DATA: TPARA_TOTAL TYPE P DECIMALS 2,
         ZMINUTOS    TYPE P DECIMALS 2,
         TEMPO_OPER  TYPE P DECIMALS 2.

   DATA: ZCONT   TYPE I,
         ZVCONT  TYPE P DECIMALS 2,
         ZPERIO  TYPE P DECIMALS 2,
         DATAINI TYPE VIQMEL-AEDAT,
         DATAFIM TYPE VIQMEL-AEDAT.


   CLEAR WA_SAIDA.
   FREE IT_VIQSAIDA[].
   FREE IT_VIQMMA.
   FREE IT_VIQMEL_[].
   FREE IT_VIQMMA_.
   FREE IT_VIQMMA.
   FREE IT_VIAUFKST[].
   FREE IT_IFLO[].
   FREE IT_VIQSAIDA_DET[].
   CLEAR CLICKS.
   CLEAR DAY.

   CHECK P_COLUMN+3(2) BETWEEN '01' AND '13'.

   DATA: V_QMART TYPE QMART,
         V_TPLNR TYPE TPLNR,
         TPARADA TYPE P DECIMALS 2.

   ADD 1 TO CLICKS.

   TRY .
       DATA(WA_SAIDA) = IT_SAIDA[ P_ROW ].
     CATCH CX_SY_ITAB_LINE_NOT_FOUND.
   ENDTRY.

   PERFORM CONTAGEM_DIAS USING P_COLUMN.


*   PERFORM DETALHES_NOTAS.

   CASE P_COLUMN.
     WHEN 'PAR01'.
       IF WA_SAIDA-PAR01 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.
*
         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0010'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
             AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
            FROM VIAUFKST AS B
            INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
            INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
            INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
            INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
            INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
             FOR ALL ENTRIES IN IT_VIQMMA
             WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
               AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

         WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0020'
            AND AUSZT NE ''
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
             AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
            FROM VIAUFKST AS B
            INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
            INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
            INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
            INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
            INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
             FOR ALL ENTRIES IN IT_VIQMMA
             WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
               AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.

             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
             CLEAR WA_IFLO.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.
*_______________________________________________________________________________


     WHEN 'PAR02'.
       IF WA_SAIDA-PAR02 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0010'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
*
       ELSEIF
*
          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0020'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.


         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR03'.
       IF WA_SAIDA-PAR03 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.
*
         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0010'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0020'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR04'.
       IF WA_SAIDA-PAR04 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0010'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF
*
          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0020'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF  IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR05'.
       IF WA_SAIDA-PAR05 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0010'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0020'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR06'.
       IF WA_SAIDA-PAR06 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0010'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
           FROM IFLO
           INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
            FROM VIQMEL
            INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
            WHERE TPLNR EQ WA_SAIDA-TPLNR
              AND QMGRP EQ 'F0000030'
              AND QMCOD EQ '0020'
              AND AUSZT NE ' '
              AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR07'.
       IF WA_SAIDA-PAR07 = 0.
         EXIT.
       ENDIF.
       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
           FROM VIQMEL
           INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
           WHERE TPLNR EQ WA_SAIDA-TPLNR
             AND QMGRP EQ 'F0000030'
             AND QMCOD EQ '0010'
             AND AUSZT NE ' '
             AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0020'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR08'.
       IF WA_SAIDA-PAR08 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
           FROM VIQMEL
           INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
           WHERE TPLNR EQ WA_SAIDA-TPLNR
             AND QMGRP EQ 'F0000030'
             AND QMCOD EQ '0010'
             AND AUSZT NE ' '
             AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
         FROM VIQMEL
         INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
         WHERE TPLNR EQ WA_SAIDA-TPLNR
           AND QMGRP EQ 'F0000030'
           AND QMCOD EQ '0020'
           AND AUSZT NE ' '
           AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR09'.
       IF WA_SAIDA-PAR09 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
           FROM VIQMEL
           INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
           WHERE TPLNR EQ WA_SAIDA-TPLNR
             AND QMGRP EQ 'F0000030'
             AND QMCOD EQ '0010'
             AND AUSZT NE ' '
             AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
           FROM VIQMEL
           INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
           WHERE TPLNR EQ WA_SAIDA-TPLNR
             AND QMGRP EQ 'F0000030'
             AND QMCOD EQ '0020'
             AND AUSZT NE ' '
             AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR10'.
       IF WA_SAIDA-PAR10 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
           FROM VIQMEL
           INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
           WHERE TPLNR EQ WA_SAIDA-TPLNR
             AND QMGRP EQ 'F0000030'
             AND QMCOD EQ '0010'
             AND AUSZT NE ' '
             AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
           FROM VIQMEL
           INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
           WHERE TPLNR EQ WA_SAIDA-TPLNR
             AND QMGRP EQ 'F0000030'
             AND QMCOD EQ '0020'
             AND AUSZT NE ' '
             AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR11'.
       IF WA_SAIDA-PAR11 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
           FROM VIQMEL
           INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
           WHERE TPLNR EQ WA_SAIDA-TPLNR
             AND QMGRP EQ 'F0000030'
             AND QMCOD EQ '0010'
             AND AUSZT NE ' '
             AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
           FROM VIQMEL
           INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
           WHERE TPLNR EQ WA_SAIDA-TPLNR
             AND QMGRP EQ 'F0000030'
             AND QMCOD EQ '0020'
             AND AUSZT NE ' '
             AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR12'.
       IF WA_SAIDA-PAR12 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0010'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.


         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

          WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0020'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF P_COLUMN+3(2) NE '13'.
           DELETE IT_VIQMEL_ WHERE AUSVN+4(2) NE P_COLUMN+3(2).
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             TEMPO_OPER = ( DAY * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

     WHEN 'PAR13'.
       IF WA_SAIDA-PAR13 = 0.
         EXIT.
       ENDIF.

       IF WA_SAIDA-QMCOD = '0010' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
          FROM VIQMEL
          INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
          WHERE TPLNR EQ WA_SAIDA-TPLNR
            AND QMGRP EQ 'F0000030'
            AND QMCOD EQ '0010'
            AND AUSZT NE ' '
            AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

         IF IT_VIQMEL_[] IS NOT INITIAL.
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.

         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             PERFORM CONTAGEM_DIAS USING P_COLUMN.

             DTINI  = |{ P_ANO-LOW }{ P_MES-LOW  }{ '01' }|.
             DTINIS = |{ '01' }{ P_MES-LOW }{ P_ANO-LOW }|.

             IF MONTH < '10'.
               DATA: ADIC_ZERO TYPE CHAR2.
               ADIC_ZERO = |{ 0 }{ MONTH }|.
*              MONTH = |{ '0' }{ MONTH }|.
               DTFIM  = |{ YEAR }{ ADIC_ZERO }{ DAY  }|.
*               DTFIMS = |{ DAY  }{ ADIC_ZERO  }{ YEAR }|.
             ELSE.
               DTFIM  = |{ YEAR }{ MONTH }{ DAY  }|.
*               DTFIMS = |{ DAY  }{ MONTH  }{ YEAR }|.
             ENDIF.

             DATAINI  = DTINI.
             DATAFIM = DTFIM.

             ZCONT = ( DATAFIM - DATAINI ).

*             DAY = '365'.
             TEMPO_OPER = ( ZCONT * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

           SORT IT_VIQSAIDA_DET ASCENDING BY AUSVN QMNUM.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.

       ELSEIF

       WA_SAIDA-QMCOD = '0020' AND WA_SAIDA-TPLNR IS NOT INITIAL.

         SELECT *
         FROM VIQMEL
         INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL_
         WHERE TPLNR EQ WA_SAIDA-TPLNR
         AND QMGRP EQ 'F0000030'
         AND QMCOD EQ '0020'
         AND AUSZT NE ' '
         AND SWERK EQ WA_SAIDA-SWERK.

         SORT IT_VIQMEL_ ASCENDING BY QMNUM.

         SELECT *
         FROM IFLO
         INTO CORRESPONDING FIELDS OF TABLE IT_IFLO
         FOR ALL ENTRIES IN IT_VIQMEL_
         WHERE TPLNR EQ IT_VIQMEL_-TPLNR.

*         IF P_COLUMN+3(2) EQ '13'.

         IF IT_VIQMEL_[] IS NOT INITIAL.
           DELETE IT_VIQMEL_ WHERE AUSVN(4)   NE P_ANO-LOW.
         ENDIF.
*
         SELECT *
           FROM VIQMMA
           INTO TABLE IT_VIQMMA_
           FOR ALL ENTRIES IN IT_VIQMEL_
           WHERE QMNUM EQ IT_VIQMEL_-QMNUM
          AND KZLOESCH EQ ABAP_FALSE.

         SORT IT_VIQMMA_ ASCENDING BY QMNUM.

         IF IT_VIQMMA_[] IS NOT INITIAL.
           LOOP AT IT_VIQMMA_.
             WA_VIQMMA-QMNUM =  IT_VIQMMA_-QMNUM.
             WA_VIQMMA-AUFNR =  |{ IT_VIQMMA_-MATXT ALPHA = IN }|.
             WA_VIQMMA-QMANUM = IT_VIQMMA_-QMANUM.
             APPEND WA_VIQMMA TO IT_VIQMMA.
           ENDLOOP.
         ENDIF.

         IF IT_VIQMMA IS NOT INITIAL.

           SELECT B~BUKRS B~AUFNR B~SWERK B~AUART B~TPLNR H~PLTXT D~VORNR D~LTXA1 B~VAPLZ B~EQUNR I~TXT
           FROM VIAUFKST AS B
           INNER JOIN AFKO AS C ON C~AUFNR EQ B~AUFNR
           INNER JOIN AFVC AS D ON D~AUFPL EQ C~AUFPL
           INNER JOIN IFLO AS H ON H~TPLNR EQ B~TPLNR
           INNER JOIN V_AUART AS I ON I~AUART EQ B~AUART
           INTO CORRESPONDING FIELDS OF TABLE IT_VIAUFKST
            FOR ALL ENTRIES IN IT_VIQMMA
            WHERE B~AUFNR EQ IT_VIQMMA-AUFNR
              AND I~SPRAS EQ 'P'.
         ENDIF.

         IF IT_VIQMEL_[] IS NOT INITIAL.

           LOOP AT IT_VIQMEL_.
             MOVE-CORRESPONDING IT_VIQMEL_ TO IT_VIQSAIDA_DET.

             READ TABLE IT_IFLO INTO WA_IFLO WITH KEY TPLNR = IT_VIQMEL_-TPLNR.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-PLTXT = WA_IFLO-PLTXT.
             ENDIF.

             IF IT_VIQMEL_-AUSZT IS NOT INITIAL.

               TPARA_TOTAL = ( IT_VIQMEL_-AUSZT / 60 ).
               TPARA_TOTAL = ( TPARA_TOTAL / 60 ).
               IT_VIQSAIDA_DET-AUSZT = TPARA_TOTAL.
             ENDIF.

             READ TABLE IT_T001 WITH KEY BUKRS = IT_VIQMEL_-BUKRS.
             IF SY-SUBRC = 0.
               IT_VIQSAIDA_DET-BUTXT = IT_T001-BUTXT.
             ENDIF.

             PERFORM CONTAGEM_DIAS USING P_COLUMN.

             DTINI  = |{ P_ANO-LOW }{ P_MES-LOW  }{ '01' }|.
             DTINIS = |{ '01' }{ P_MES-LOW }{ P_ANO-LOW }|.

             IF MONTH < '10'.
               DATA: ZADIC_ZERO TYPE CHAR2.
               ZADIC_ZERO = |{ 0 }{ MONTH }|.
*              MONTH = |{ '0' }{ MONTH }|.
               DTFIM  = |{ YEAR }{ ZADIC_ZERO }{ DAY  }|.
*               DTFIMS = |{ DAY  }{ ADIC_ZERO  }{ YEAR }|.
             ELSE.
               DTFIM  = |{ YEAR }{ MONTH }{ DAY  }|.
*               DTFIMS = |{ DAY  }{ MONTH  }{ YEAR }|.
             ENDIF.

             DATAINI  = DTINI.
             DATAFIM = DTFIM.

             ZCONT = ( DATAFIM - DATAINI ).

*             DAY = '365'.
             TEMPO_OPER = ( ZCONT * 60 ) * 24.
             IT_VIQSAIDA_DET-TEMPO_OPER = TEMPO_OPER.

             ZMINUTOS = ( TPARA_TOTAL * 60 ).
             IT_VIQSAIDA_DET-CONV_MINUT = ZMINUTOS.

             IT_VIQSAIDA_DET-INDISP = ( ZMINUTOS / TEMPO_OPER ) * 100.

             READ TABLE IT_VIQMMA INTO WA_VIQMMA WITH KEY QMNUM = IT_VIQMEL_-QMNUM.
             IF SY-SUBRC = 0.
             ENDIF.

             READ TABLE IT_VIAUFKST INTO WA_VIAUFKST WITH KEY AUFNR = WA_VIQMMA-AUFNR.
             IF WA_VIAUFKST-AUFNR IS NOT INITIAL.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Sim'.
             ELSE.
               IT_VIQSAIDA_DET-EXEC_AVAR = 'Não'.
             ENDIF.

             APPEND IT_VIQSAIDA_DET.
             CLEAR IT_VIQSAIDA_DET.
             CLEAR TEMPO_OPER.
             CLEAR IT_VIQMEL_.
             CLEAR ZMINUTOS.
             CLEAR TPARA_TOTAL.
             CLEAR WA_IFLO.
             CLEAR WA_VIQMMA.
             CLEAR WA_VIAUFKST.
           ENDLOOP.

           SORT IT_VIQSAIDA_DET ASCENDING BY AUSVN QMNUM.

         ELSE.
           MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'.
           EXIT.
         ENDIF.
       ENDIF.

   ENDCASE.

   CALL SCREEN 0300.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0372   text
*      -->P_0373   text
*      -->P_0374   text
*      -->P_0375   text
*      -->P_0376   text
*----------------------------------------------------------------------*
 FORM F_INSERT_SHDB  USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.

   APPEND VALUE #(
                   PROGRAM   = P_PROGRAM
                   DYNPRO    = P_DYNPRO
                   DYNBEGIN  = P_START
                   FNAM      = P_FNAM
                   FVAL      = P_FVAL
                  ) TO IT_BDCDATA.

 ENDFORM.

* CLASS LCL_EVENTOS IMPLEMENTATION.
*   METHOD ON_HOTSPOT_CLICK.
*     CASE E_COLUMN_ID-FIELDNAME.
*       WHEN:'QMNUM'."Notas Manuteção
*
*         IF IT_VIQSAIDA-QMNUM IS NOT INITIAL.
*           SET PARAMETER ID 'IQM' FIELD IT_VIQSAIDA-QMNUM.
*           CALL TRANSACTION 'IW23' AND SKIP FIRST SCREEN .
*         ENDIF.
*
*     ENDCASE.
*   ENDMETHOD.
* ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  DETALHES_AVARIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM DETALHES_NOTAS.


 ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 MODULE STATUS_0300 OUTPUT.
   SET PF-STATUS 'PF0300'.
   SET TITLEBAR 'TL0300'.


   PERFORM FILL_IT_FIELDCATALOG_NOTA  USING:

          01 'BUKRS       '      'VIQMEL '    '10'  ' '     ' '    ' '   'Empresa               '  ' '  ' ' ,
          02 'BUTXT       '      'VIQMEL '    '10'  ' '     ' '    ' '   'Desc empresa          '  ' '  ' ' ,
          03 'SWERK       '      'VIQMEL '    '10'  ' '     ' '    ' '   'Centro                '  ' '  ' ' ,
          04 'QMNUM       '      'VIQMEL '    '20'  ' '     ' '    ' '   'Nº Nota               '  ' '  'X' ,
          05 'AUSVN       '      'VIQMEL '    '20'  ' '     ' '    ' '   'Data da avaria        '  ' '  ' ' ,
          06 'TPLNR       '      'VIQMEL '    '12'  ' '     ' '    ' '   'Local                 '  ' '  ' ' ,
          07 'PLTXT       '      'IFLO   '    '30'  ' '     ' '    ' '   'Desc Local            '  ' '  ' ' ,
          08 'QMART       '      'VIQMEL '    '20'  ' '     ' '    ' '   'Tipo nota             '  ' '  ' ' ,
          09 'AUSZT       '      'VIQMEL '    '25'  ' '     ' '    'X '   'Duração Parada       '  ' '  ' ' ,
          10 'TEMPO_OPER  '      '       '    '15'  ' '     ' '    ' '   'Tempo/Operação Min    '  ' '  ' ' ,
          11 'CONV_MINUT  '      '       '    '15'  ' '     ' '    'X '   'Duração parada Min   '  ' '  ' ' ,
          12 'INDISP      '      '       '    '15'  ' '     ' '    'X '   '% Indisponivel       '  ' '  ' ' ,
          13 'EXEC_AVAR   '      '       '    '15'  ' '     ' '    '  '   'Reg manut no periodo?'  ' '  'X' .


   IF G_CUSTOM_CONTAINER3 IS INITIAL.

     CREATE OBJECT G_CUSTOM_CONTAINER3
       EXPORTING
         CONTAINER_NAME              = 'CONTAINER3'
       EXCEPTIONS
         CNTL_ERROR                  = 1
         CNTL_SYSTEM_ERROR           = 2
         CREATE_ERROR                = 3
         LIFETIME_ERROR              = 4
         LIFETIME_DYNPRO_DYNPRO_LINK = 5.

     CREATE OBJECT CTL_ALV_2
       EXPORTING
         I_PARENT = G_CUSTOM_CONTAINER3.

*     WA_EXCLUDE_FCODE_2 = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*    APPEND WA_EXCLUDE_FCODE_2 TO IT_EXCLUDE_FCODE_2.

     CALL METHOD CTL_ALV_2->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING
         IS_LAYOUT            = GS_LAYOUT_3
         IS_VARIANT           = GS_VARIANT_3
         IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE_3
         I_SAVE               = 'A'
       CHANGING
         IT_FIELDCATALOG      = IT_FIELDCATALOG_2
         IT_OUTTAB            = IT_VIQSAIDA_DET[]
         IT_SORT              = IT_SORT3.

     CREATE OBJECT OBJ_EVEN.
     SET HANDLER: OBJ_EVEN->ON_HOTSPOT_CLICK  FOR CTL_ALV_2.

*     SET HANDLER: LCL_EVENTOS_3=>ON_HOTSPOT_CLICK FOR CTL_ALV_2.

   ELSE.
     CALL METHOD CTL_ALV_2->REFRESH_TABLE_DISPLAY.
   ENDIF.




 ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 MODULE USER_COMMAND_0300 INPUT.
   CASE SY-UCOMM.
     WHEN 'EXIT'.
       LEAVE TO SCREEN 0.
   ENDCASE.
 ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_01     text
*      -->P_1400   text
*      -->P_1401   text
*      -->P_1402   text
*      -->P_1403   text
*      -->P_1404   text
*      -->P_1405   text
*      -->P_1406   text
*      -->P_1407   text
*      -->P_1408   text
*----------------------------------------------------------------------*
 FORM FILL_IT_FIELDCATALOG_NOTA  USING VALUE(P_COLNUM)
                                 VALUE(P_FIELDNAME)
                                 VALUE(P_TABNAME)
                                 VALUE(P_LEN)
                                 VALUE(P_EDIT)
                                 VALUE(P_ICON)
                                 VALUE(P_DO_SUM)
                                 VALUE(P_HEADER)
                                 VALUE(P_EMPHASIZE)
                                 VALUE(P_HOTSPOT).


   WA_FIELDCATALOG_2-COL_POS     = P_COLNUM.
   WA_FIELDCATALOG_2-FIELDNAME   = P_FIELDNAME.
   WA_FIELDCATALOG_2-TABNAME     = P_TABNAME.
   WA_FIELDCATALOG_2-OUTPUTLEN   = P_LEN.
   WA_FIELDCATALOG_2-EDIT        = P_EDIT.
   WA_FIELDCATALOG_2-ICON        = P_ICON.
   WA_FIELDCATALOG_2-DO_SUM      = P_DO_SUM.
   WA_FIELDCATALOG_2-COLTEXT     = P_HEADER.
   WA_FIELDCATALOG_2-EMPHASIZE   = P_EMPHASIZE.
   WA_FIELDCATALOG_2-HOTSPOT     = P_HOTSPOT.
   WA_FIELDCATALOG_2-REF_TABLE   = P_TABNAME.
   WA_FIELDCATALOG_2-CHECKTABLE  = P_TABNAME.

*  WA_FIELDCATALOG-EXCP_CONDS  = P_EXCP_CONDS.


   GS_LAYOUT_3-EXCP_CONDS    = 'X'.
   GS_LAYOUT_3-ZEBRA         = 'X'.
   GS_LAYOUT_3-SEL_MODE      = 'A'.
   GS_LAYOUT_3-CWIDTH_OPT    = 'X'.     "  Otimizar colunas na tela
   GS_LAYOUT_3-TOTALS_BEF    = ' '.

   APPEND WA_FIELDCATALOG_2 TO IT_FIELDCATALOG_2.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONTAGEM_DIAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM CONTAGEM_DIAS USING P_COLUMN.

   DATA: ZCONT  TYPE P DECIMALS 2,
         ZVCONT TYPE P DECIMALS 2,
         ZPERIO TYPE P DECIMALS 2.

   CASE P_COLUMN.
     WHEN 'PAR01'.
       MONTH = '01'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia


     WHEN 'PAR02'.
       MONTH = '02'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia


     WHEN 'PAR03'.
       MONTH = '03'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia


     WHEN 'PAR04'.
       MONTH = '04'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia


     WHEN 'PAR05'.
       MONTH = '05'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia


     WHEN 'PAR06'.
       MONTH = '06'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia



     WHEN 'PAR07'.
       MONTH = '07'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia



     WHEN 'PAR08'.
       MONTH = '08'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia


     WHEN 'PAR09'.
       MONTH = '09'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia



     WHEN 'PAR10'.
       MONTH = '10'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia




     WHEN 'PAR11'.
       MONTH = '11'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia



     WHEN 'PAR12'.
       MONTH = '12'.
       YEAR  = P_ANO-LOW.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia


     WHEN 'PAR13'.

       IF  DTFIMS+2(2) < SY-DATUM+4(2).
         ZPERIO = DTFIMS+2(2).
         MONTH = ZPERIO.
         YEAR  = P_ANO-LOW.

       ELSE.
         ZPERIO = SY-DATUM+4(2).
         MONTH = ZPERIO.
         YEAR  = P_ANO-LOW.

       ENDIF.

       CALL FUNCTION 'RTP_US_API_MAX_DAYS_IN_MONTH'
         EXPORTING
           I_DATE_MONTH = MONTH "Mes
           I_DATE_YEAR  = YEAR "Ano
         IMPORTING
           E_MAX_DAYS   = DAY. "Dia
   ENDCASE.


 ENDFORM.
