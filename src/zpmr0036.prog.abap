*&---------------------------------------------------------------------*
*& Report  ZPMR0036                                                    *
*& Data           : 22/05/2018                                         *
*& Especificado   : Anderson Oenning                                   *
*& Desenvolvimento: Anderson Oenning                                   *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT ZPMR0036.

*Tabelas:
*======================================================================*
TABLES: MPOS, ILOA, MPLA, *MMPT, *MHIO, MMPT, MHIO.


*Tipos:
*==========================================================================

TYPES: BEGIN OF TY_MPOS_.
        INCLUDE STRUCTURE MPOS.
TYPES: END   OF TY_MPOS_.
DATA: T_MPOS  TYPE TABLE OF TY_MPOS_ WITH HEADER LINE.

TYPES:
* Estrutura dos item de manutenção / Plano
  BEGIN OF TY_MPOS,
    BUKRS  TYPE ILOA-BUKRS,
    IWERK  TYPE CHAR40,      "Centro
    WARPL  TYPE MPOS-WARPL,  "Plano
    WPTXT  TYPE MPLA-WPTXT,  "Descrição do plano
    PSTXT  TYPE MPOS-PSTXT,
    BAUTL  TYPE MPOS-BAUTL,
    ILOAN  TYPE MPOS-ILOAN,  "Nº Local
    PLTXT  TYPE IFLO-PLTXT,
    EQUNR  TYPE MPOS-EQUNR,  "Equipamento
    EQKTX  TYPE EQKT-EQKTX,
    PLNNR  TYPE MPOS-PLNNR,  "Grupo de Lista Tarefas
    PLNAL  TYPE MPOS-PLNAL,  "Numerador grupo de lista tarefas
    NAME1  TYPE CHAR40,       "Nome do centro
    INACT  TYPE MPOS-INACT,  "Campo plano inativo.
    ERSDT  TYPE MPOS-ERSDT,  "Data Criação
    STATUS TYPE MPOS-STATUS, "Status do item de manutenção
    MPTYP  TYPE MPLA-MPTYP,
    OBJNR  TYPE MPLA-OBJNR,
    WSTRA  TYPE MPOS-WSTRA,
  END OF TY_MPOS,

* Estrutura do processo de execução
  BEGIN OF TY_MHIS,
    WARPL TYPE MHIS-WARPL, "Plano.
    NPLDA TYPE MHIS-NPLDA, "Data planejada.
    LRMDT TYPE MHIS-LRMDT, "Data da ultima conclusão.
    HORDA TYPE MHIS-HORDA, "Data da solicitação.
    LINHA TYPE P,
  END OF TY_MHIS,

  BEGIN OF TY_JEST ,
    OBJNR TYPE JEST-OBJNR,
    STAT  TYPE JEST-STAT,
    INACT TYPE JEST-INACT,
  END OF TY_JEST,

  BEGIN OF TY_MMPT,
    WARPL    TYPE MMPT-WARPL,
    ZYKL1    TYPE MMPT-ZYKL1,
    ZEIEH    TYPE MMPT-ZEIEH,
    PAK_TEXT TYPE MMPT-PAK_TEXT,
    LANGU    TYPE MMPT-LANGU,
    POINT    TYPE MMPT-POINT,
    OFFSET   TYPE MMPT-OFFSET,
    NZAEH    TYPE MMPT-NZAEH,
  END OF TY_MMPT,

  BEGIN OF TY_SAIDA,
    BUKRS   TYPE ILOA-BUKRS,
    IWERK   TYPE CHAR40, "Centro
    WARPL   TYPE MPOS-WARPL, "Plano
    ILOAN   TYPE MPOS-ILOAN, "Nº Local
    EQUNR   TYPE MPOS-EQUNR, "Equipamento
    PLNNR   TYPE MPOS-PLNNR, "Grupo de Lista Tarefas
    PLNAL   TYPE MPOS-PLNAL, "Numerador grupo de lista tarefas
    TPLANO  TYPE P,
    TPVENC  TYPE P,
    TPCERT  TYPE P,
    %PVENC  TYPE I,
    ZVALOR1 TYPE I,
    ZVALOR2 TYPE I,
    ZVALOR3 TYPE I,
    ZVALOR4 TYPE I,
  END OF TY_SAIDA,

  BEGIN OF TY_SAIDA_REL,
    BUKRS      TYPE ILOA-BUKRS,
    IWERK      TYPE CHAR40, "Centro
    WARPL      TYPE MPOS-WARPL, "Plano
    ILOAN      TYPE MPOS-ILOAN, "Nº Local
    EQUNR      TYPE MPOS-EQUNR, "Equipamento
    PLNNR      TYPE MPOS-PLNNR, "Grupo de Lista Tarefas
    PLNAL      TYPE MPOS-PLNAL, "Numerador grupo de lista tarefas
    CELL_COLOR TYPE LVC_T_SCOL,
    TPLANO     TYPE P,
    TPVENC     TYPE P,
    TPCERT     TYPE P,
    %PVENC     TYPE I,
    ZVALOR1    TYPE I,
    ZVALOR2    TYPE I,
    ZVALOR3    TYPE I,
    ZVALOR4    TYPE I,
  END OF TY_SAIDA_REL,

  BEGIN OF TY_RELATORIO,           " Dados do relatorio
    IWERK      TYPE MPOS-IWERK, "Centro
    ILOAN      TYPE MPOS-ILOAN,
    PLTXT      TYPE IFLO-PLTXT,
    EQUNR      TYPE EQUZ-EQUNR ,   " Codigo do equipamento
    EQKTX      TYPE EQKT-EQKTX ,   " Denominação do objeto técnico
    EARTX      TYPE T370K_T-EARTX, " Tipo de veículo
    WARPL      TYPE MPOS-WARPL,    " Plano de manutenção
    ZYKL1(8)   TYPE C,             " Ciclo / offset de um pacote de manutenção
    PAK_TEXT   TYPE MMPT-PAK_TEXT, " Texto p/o pacote ou ciclo de manutenção (tempo/rendimento)
    WPTXT      TYPE MPLA-WPTXT ,   " Descrição
    SZAEH(8)   TYPE C,             " Posição Contador
    RZAEH(8)   TYPE C,             " Posição contador conclusão
    TOTAC(8)   TYPE C,             " Posição Total Contador
    NZAEH(8)   TYPE C,             " Próxima leitura planejada de contador
    USO(8)     TYPE C,             " Utilização
    DESVIO(8)  TYPE C,             " Desvio
    PER_DESV   TYPE I,             " Desvio
    IDAT2      TYPE CAUFV-IDAT2,   " Data Ultima_troca
    BAUTL      TYPE MPOS-BAUTL,    " Sistema
    CELL_COLOR TYPE LVC_T_SCOL,    " Cor da Célula
    ZEIEH(10)  TYPE C,             " Unidade  MMPT-ZEIEH
    ORDEM(4)   TYPE C,
  END OF TY_RELATORIO .

TYPES: BEGIN OF TY_CALC,
         ABNUM     TYPE MHIS-ABNUM,
         WARPL     TYPE MPLA-WARPL,
         NPLDA     TYPE MHIS-NPLDA,
         STADT     TYPE MHIS-STADT,
         LRMDT     TYPE MHIS-LRMDT,
         MPTYP     TYPE MPLA-MPTYP,
         ZEIEH     TYPE MMPT-ZEIEH,
         ZYKL1     TYPE MMPT-ZYKL1,
         ZYKL2(8)  TYPE C,
         USO(8)    TYPE C,
         DESVIO(8) TYPE C,
         PER_DESV  TYPE I,
       END OF TY_CALC.

TYPES: BEGIN OF TY_WMMPT.
        INCLUDE STRUCTURE MMPT.
        INCLUDE STRUCTURE MMPT_ADDITION.
TYPES: END   OF TY_WMMPT.
TYPES: WC_T_WMMPT TYPE TY_WMMPT OCCURS 0.
DATA:  MMPT_TAB TYPE TY_WMMPT OCCURS 0 WITH HEADER LINE.

*--- maintenance item (MPOS)
TYPES: BEGIN OF TY_WMPOS.
        INCLUDE STRUCTURE MPOS.
        INCLUDE STRUCTURE MPOS_ADDITION.
TYPES: END   OF TY_WMPOS.
TYPES: WC_T_WMPOS TYPE TY_WMPOS OCCURS 0.
DATA: IMPOS TYPE TY_WMPOS OCCURS 1 WITH HEADER LINE.    "table


DATA: BEGIN OF WA_IMPT.
        INCLUDE STRUCTURE IMPT.
DATA: END OF WA_IMPT.

DATA: BEGIN OF WA_IMRG.
        INCLUDE STRUCTURE IMRG.
DATA: END OF WA_IMRG.

TYPES: BEGIN OF TY_VIMHIS.
        INCLUDE STRUCTURE VIMHIS.
TYPES: READG   LIKE  IMRG-READG.
TYPES: END   OF TY_VIMHIS.

TYPES: BEGIN OF TY_VIAUFKST,
         BUKRS  TYPE VIAUFKST-BUKRS,
         WERKS  TYPE VIAUFKST-WERKS,
         WARPL  TYPE VIAUFKST-WARPL,
         AUFNR  TYPE VIAUFKST-AUFNR,
         QMNUM  TYPE VIAUFKST-QMNUM,
         ERDAT  TYPE VIAUFKST-ERDAT,
         OBJNR  TYPE CAUFV-OBJNR,
         STTXT  TYPE CAUFVD-STTXT,
         ASTTX  TYPE CAUFVD-ASTTX,
         STATUS TYPE CHAR10.
TYPES: END OF TY_VIAUFKST.


TYPES: BEGIN OF TY_VIQMEL.
        INCLUDE STRUCTURE VIQMEL.
TYPES: STTXT  TYPE CAUFVD-STTXT,
       ASTTX  TYPE CAUFVD-ASTTX,
       STATUS TYPE CHAR10.
TYPES: END OF TY_VIQMEL.

*Estruturas:
*==========================================================================
DATA:
  IT_SAIDA     TYPE TABLE OF TY_SAIDA,
  IT_SAIDA_REL TYPE TABLE OF TY_SAIDA_REL,
  IT_MPOS      TYPE TABLE OF TY_MPOS,
  T_EQKT       TYPE TABLE OF EQKT,
  IT_MHIS      TYPE TABLE OF TY_MHIS WITH HEADER LINE,
  IT_JEST      TYPE TABLE OF TY_JEST WITH HEADER LINE,
  T_VIMHIS     TYPE TABLE OF TY_VIMHIS WITH HEADER LINE,
  T_PLANOS     TYPE TABLE OF TY_RELATORIO,
  T_RELATORIO  TYPE TABLE OF TY_RELATORIO,
  T_RELATORIO_ TYPE TABLE OF TY_RELATORIO.

DATA IT_RELATORIO TYPE TABLE OF TY_RELATORIO.
*DATA T_ORDEM TYPE TABLE OF AFIH.
DATA: T_ORDEM  TYPE TABLE OF TY_VIAUFKST.
DATA: T_VIQMEL TYPE TABLE OF TY_VIQMEL.

DATA: LINHA_SELECIONADA TYPE SLIS_SELFIELD.
DATA: _EXIT             TYPE C.

CONSTANTS: YX  VALUE 'X'.              "Flag X



DATA:
  WA_SAIDA     TYPE TY_SAIDA,
  WA_SAIDA_REL TYPE TY_SAIDA_REL,
  WA_MPOS      TYPE TY_MPOS,
  WA_MHIS      TYPE TY_MHIS,
  W_PLANOS     TYPE TY_RELATORIO,
  W_RELATORIO  TYPE TY_RELATORIO,
  W_RELATORIO_ TYPE TY_RELATORIO.

DATA: IT_CALC  TYPE TABLE OF TY_CALC WITH HEADER LINE.


DATA: FLTP_CHAR     TYPE IMRC_TOTAC,
      POINT_TXT(40) TYPE C,
      ANZ_VERPAK    LIKE T351P-ZAEHL,
      STTAG         TYPE SY-DATUM,
      MMPT_MAX      LIKE SY-INDEX,
      GV_RC(2)      TYPE C,
      GV_USO        TYPE IMRC_READG,
      GV_DESVIO     TYPE IMRC_READG.

CONSTANTS: CC_A        TYPE C VALUE 'A',
           CC_X        TYPE C VALUE 'X',
           CC_I        TYPE C VALUE 'I',
           CC_1        TYPE C VALUE '1',
           CC_2        TYPE C VALUE '2',
           CC_SPRAS(2) TYPE C VALUE 'PT',
           CC_M        TYPE C VALUE 'M',
           WC_UPD      TYPE C VALUE 'U',    "indicator: Update
           WC_DELE     TYPE C VALUE 'L',    "indicator: delete
           ZVALOR      TYPE I VALUE '0',
           ZVALOR1     TYPE I VALUE '33',
           ZVALOR2     TYPE I VALUE '66',
           ZVALOR3     TYPE I VALUE '99',
           ZVALOR4     TYPE I VALUE '34',
           ZVALOR5     TYPE I VALUE '67',
           ZVALOR6     TYPE I VALUE '-33',
           ZVALOR7     TYPE I VALUE '-66',
           ZVALOR8     TYPE I VALUE '-99',
           ZVALOR9     TYPE I VALUE '-34',
           ZVALOR10    TYPE I VALUE '-67',
           ZVALOR11    TYPE I VALUE '100',
           ZVALOR12    TYPE I VALUE '-100',
           ZVALOR13    TYPE I VALUE '-1',
           ZVALOR14    TYPE I VALUE '1'.


DATA: CLICKS TYPE SY-TABIX.


*Estruturas para ALV:
*==========================================================================
DATA:
  "Informações para 1º ALV.
  G_CONTAINER      TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "Conteiner ALV
  CTL_ALV          TYPE REF TO CL_GUI_ALV_GRID,
  GS_VARIANT       TYPE DISVARIANT,
  IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
  WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE,
  GS_LAYOUT        TYPE LVC_S_LAYO, "Layout da ALV
  IT_FIELDCATALOG  TYPE LVC_T_FCAT, "Catálogo de campos para controle visor de listas
  WA_FIELDCATALOG  TYPE LVC_S_FCAT, "Controle VLA: catálogo de campos
  IT_SORT          TYPE LVC_T_SORT.

"Informações para 2º ALV.
DATA:
  G_CONTAINER2      TYPE REF TO CL_GUI_CUSTOM_CONTAINER, "Conteiner ALV
  CTL_ALV2          TYPE REF TO CL_GUI_ALV_GRID,
  GS_VARIANT2       TYPE DISVARIANT,
  IT_EXCLUDE_FCODE2 TYPE UI_FUNCTIONS,
  WA_EXCLUDE_FCODE2 LIKE LINE OF IT_EXCLUDE_FCODE,
  GS_LAYOUT2        TYPE LVC_S_LAYO, "Layout da ALV
  IT_FIELDCATALOG2  TYPE LVC_T_FCAT, "Catálogo de campos para controle visor de listas
  WA_FIELDCATALOG2  TYPE LVC_S_FCAT, "Controle VLA: catálogo de campos
  IT_SORT2          TYPE LVC_T_SORT.

DATA: OK_CODE     TYPE          SY-UCOMM,
      RETURN_CODE TYPE          SY-SUBRC,
      WA_COLOR    TYPE          LVC_S_SCOL,  " Cor para célula
      IT_COLOR    TYPE TABLE OF LVC_S_SCOL,  " Cor para célula
      PT_EXCLUDE  TYPE          UI_FUNCTIONS. " internal table declaration to be passed.

CLASS EVENT DEFINITION.
  PUBLIC SECTION.
    METHODS:
      HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN SENDER.
*      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID.

    METHODS:
      HANDLE_DOUBLE_CLICK_ORDEM FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN SENDER.
ENDCLASS.

DATA(OBJ_EVEN) = NEW EVENT( ).

CLASS EVENT IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK.
    CHECK E_ROW-ROWTYPE IS INITIAL.
    PERFORM SEL_PLANOS USING E_ROW E_COLUMN-FIELDNAME.
  ENDMETHOD.

  METHOD HANDLE_DOUBLE_CLICK_ORDEM.
    CHECK E_ROW-ROWTYPE IS INITIAL.
    PERFORM SEL_ORDEM  USING E_ROW E_COLUMN-FIELDNAME.
  ENDMETHOD.

ENDCLASS.



* Layout - Tela Inicio
*==========================================================================

SELECTION-SCREEN: BEGIN OF BLOCK A2 WITH FRAME TITLE TEXT-002.

*Parametro de seleção - Tela Inicio:
SELECT-OPTIONS: P_BUKRS FOR ILOA-BUKRS OBLIGATORY,
                P_IWERK FOR MPOS-IWERK.
SELECTION-SCREEN: END OF BLOCK A2.

SELECTION-SCREEN: BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
*Parametro de seleção - Tela Inicio:
SELECT-OPTIONS: P_MPTYP FOR MPLA-MPTYP,
                P_WARPL FOR MPOS-WARPL,
                P_AUART FOR MPOS-AUART,
                P_ILART FOR MPOS-ILART.
SELECTION-SCREEN: END OF BLOCK A1.

SELECTION-SCREEN: BEGIN OF BLOCK A3 WITH FRAME TITLE TEXT-003.
*Parametro de seleção - Tela Inicio:
SELECT-OPTIONS: P_KOSTL FOR ILOA-KOSTL.
SELECTION-SCREEN: END OF BLOCK A3.


*Inicio:
*==========================================================================
INITIALIZATION.

START-OF-SELECTION.
  PERFORM SELECAO_DADOS.
  PERFORM PREPARA_DADOS.
  PERFORM EXIBIR_ALV.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  SELECAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECAO_DADOS.


*  DATA: T_MPLA TYPE TABLE OF TY_MPOS.
*
*  SELECT *
*  FROM MPLA AS A
*  INNER JOIN MPOS AS B ON B~WARPL = A~WARPL
*  INTO CORRESPONDING FIELDS OF TABLE T_MPLA
*  WHERE A~WARPL IN P_WARPL
*    AND B~IWERK IN P_IWERK
*    AND B~INACT EQ ABAP_FALSE.

  SELECT C~BUKRS A~IWERK F~PLTXT A~WARPL D~WPTXT A~PSTXT A~BAUTL A~ILOAN A~EQUNR  A~PLNNR A~PLNAL A~INACT A~ERSDT A~STATUS D~MPTYP D~OBJNR A~WSTRA B~NAME1"E~EQKTX" B~NAME1
  FROM MPLA AS D
  INNER JOIN MPOS  AS A ON A~WARPL = D~WARPL
  INNER JOIN T001W AS B ON B~WERKS = A~IWERK
  INNER JOIN ILOA  AS C ON C~ILOAN = A~ILOAN
  INNER JOIN IFLO AS F ON F~TPLNR = C~TPLNR
*  INNER JOIN EQKT  AS E ON E~EQUNR = A~EQUNR
  INTO CORRESPONDING FIELDS OF TABLE IT_MPOS
*  FOR ALL ENTRIES IN T_MPLA
  WHERE A~IWERK IN P_IWERK
    AND D~WARPL IN P_WARPL
    AND C~BUKRS IN P_BUKRS
    AND D~MPTYP IN P_MPTYP
    AND C~KOSTL IN P_KOSTL
    AND A~AUART IN P_AUART
    AND A~ILART IN P_ILART
    AND A~INACT  EQ ABAP_FALSE.
  SORT IT_MPOS ASCENDING BY WARPL IWERK.

  SELECT *
  FROM EQKT
  INTO TABLE T_EQKT
  FOR ALL ENTRIES IN IT_MPOS
  WHERE EQUNR EQ IT_MPOS-EQUNR.

  LOOP AT IT_MPOS INTO WA_MPOS.
    IF WA_MPOS-WARPL IS NOT INITIAL AND WA_MPOS-WSTRA IS NOT INITIAL.
      DELETE IT_MPOS INDEX SY-TABIX.
    ENDIF.
  ENDLOOP.

  SELECT *
FROM MPLA AS A
INNER JOIN MPOS AS B ON A~WARPL  = B~WARPL
INNER JOIN MMPT AS M ON A~WARPL  = M~WARPL
INNER JOIN ILOA AS C ON C~ILOAN  = B~ILOAN
INNER JOIN MHIS AS D ON A~WARPL  = D~WARPL
INTO CORRESPONDING FIELDS OF TABLE IT_CALC
FOR ALL ENTRIES IN IT_MPOS
WHERE A~WARPL EQ IT_MPOS-WARPL
AND D~STADT EQ ( SELECT MAX( STADT ) FROM MHIS WHERE WARPL EQ D~WARPL ).

  SORT IT_CALC ASCENDING BY WARPL STADT.
  LOOP AT IT_CALC.
    IF IT_CALC-WARPL IS NOT INITIAL AND IT_CALC-STADT IS NOT INITIAL .
      DELETE ADJACENT DUPLICATES FROM IT_CALC COMPARING WARPL STADT.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREPARA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARA_DADOS .
  DATA: WA_EQKT TYPE EQKT.
  DATA: TOTAL_PLANO TYPE CHAR20.

  LOOP AT IT_MPOS INTO WA_MPOS.
    CLEAR: WA_EQKT.
    W_RELATORIO-IWERK  = WA_MPOS-IWERK.
    W_RELATORIO-ILOAN  = WA_MPOS-ILOAN.
    W_RELATORIO-PLTXT  = WA_MPOS-PLTXT.
    W_RELATORIO-EQUNR  = WA_MPOS-EQUNR.
    W_RELATORIO-WARPL  = WA_MPOS-WARPL.
    W_RELATORIO-WPTXT  = WA_MPOS-PSTXT.
    W_RELATORIO-BAUTL  = WA_MPOS-BAUTL.
*    W_RELATORIO-EQKTX  = WA_MPOS-EQKTX.

    IF WA_MPOS-EQUNR IS NOT INITIAL.
      READ TABLE T_EQKT INTO WA_EQKT WITH KEY EQUNR = WA_MPOS-EQUNR.
      IF SY-SUBRC = 0.
        W_RELATORIO-EQKTX = WA_EQKT-EQKTX.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM MPLA
      WHERE WARPL = WA_MPOS-WARPL.

    PERFORM STATUS_CHECK_PLANO USING MPLA-OBJNR RETURN_CODE.
    IF RETURN_CODE <> 0 AND  " ativos
       RETURN_CODE <> 3   .  " inativo MREL
      CONTINUE.
    ENDIF.

    SELECT SINGLE * FROM MMPT
      WHERE WARPL EQ WA_MPOS-WARPL.
*      ENDSELECT.
    IF SY-SUBRC EQ 0.
      W_RELATORIO-ZEIEH = MMPT-ZEIEH.

      PERFORM OBTEM_POS_TOT_CONTADOR USING MMPT-POINT.
      IF WA_MPOS-WSTRA IS INITIAL.
        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR MMPT-ZYKL1 MMPT-ZEIEH.

        W_RELATORIO-ZYKL1    =  FLTP_CHAR+14(8).
        CONDENSE W_RELATORIO-ZYKL1.

        W_RELATORIO-PAK_TEXT = MMPT-PAK_TEXT.
        PERFORM OBTEM_CONTADORES USING WA_MPOS-WARPL MMPT-ZEIEH.
      ENDIF.
    ELSE.
      W_RELATORIO-ZYKL1      = ' '.
      W_RELATORIO-PAK_TEXT   = ' '.
    ENDIF.
  ENDLOOP.

  SORT T_RELATORIO ASCENDING BY DESVIO.

  LOOP AT T_RELATORIO INTO W_RELATORIO.
    IF W_RELATORIO-WARPL IS NOT INITIAL AND W_RELATORIO-IWERK IS NOT INITIAL.
      IF W_RELATORIO-DESVIO IS NOT INITIAL.
        ADD 1 TO WA_SAIDA-TPLANO.

        IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
          ADD 1 TO WA_SAIDA-TPVENC.
        ELSE.
          ADD 1 TO WA_SAIDA-TPCERT.
        ENDIF.

        IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
          IF W_RELATORIO-PER_DESV BETWEEN ZVALOR  AND ZVALOR1
          OR W_RELATORIO-PER_DESV BETWEEN ZVALOR6 AND ZVALOR.
            ADD 1 TO WA_SAIDA-ZVALOR1.
          ENDIF.
        ENDIF.

        IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
          IF  W_RELATORIO-PER_DESV BETWEEN ZVALOR4 AND ZVALOR2
          OR  W_RELATORIO-PER_DESV BETWEEN ZVALOR7 AND ZVALOR9.
            ADD 1 TO WA_SAIDA-ZVALOR2.
          ENDIF.
        ENDIF.

        IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
          IF W_RELATORIO-PER_DESV  BETWEEN ZVALOR5 AND ZVALOR3
          OR W_RELATORIO-PER_DESV  BETWEEN ZVALOR8 AND ZVALOR10.
            ADD 1 TO WA_SAIDA-ZVALOR3.
          ENDIF.
        ENDIF.

        IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
          IF W_RELATORIO-PER_DESV > ZVALOR3
          OR W_RELATORIO-PER_DESV < ZVALOR8.
            ADD 1 TO WA_SAIDA-ZVALOR4.
          ENDIF.
        ENDIF.

        READ TABLE IT_MPOS INTO WA_MPOS WITH KEY WARPL = W_RELATORIO-WARPL
                                                 IWERK = W_RELATORIO-IWERK.
        IF SY-SUBRC = 0.
          WA_SAIDA-BUKRS = WA_MPOS-BUKRS.
          WA_SAIDA-IWERK = |{ WA_MPOS-IWERK }-{ WA_MPOS-NAME1 }|.
        ENDIF.

        COLLECT WA_SAIDA INTO IT_SAIDA.
        CLEAR WA_MPOS.
        CLEAR WA_SAIDA.
        CLEAR W_RELATORIO.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    MOVE-CORRESPONDING WA_SAIDA TO WA_SAIDA_REL.
    IF WA_SAIDA-BUKRS IS NOT INITIAL AND WA_SAIDA-IWERK IS NOT INITIAL AND WA_SAIDA-TPLANO IS NOT INITIAL.
      WA_SAIDA_REL-%PVENC = ( WA_SAIDA-TPVENC / WA_SAIDA-TPLANO ) * 100.
    ENDIF.

*        Mudando a cor % planos vencidos.
    IF WA_SAIDA_REL-%PVENC >= 10.
      CLEAR WA_COLOR.
      MOVE '%PVENC'   TO WA_COLOR-FNAME.
      MOVE '6'        TO WA_COLOR-COLOR-COL.
      MOVE '1'        TO WA_COLOR-COLOR-INT.
      MOVE '1'        TO WA_COLOR-COLOR-INV.
      APPEND WA_COLOR TO IT_COLOR.

    ELSEIF WA_SAIDA_REL-%PVENC > 5 AND WA_SAIDA_REL-%PVENC < 10.
      CLEAR WA_COLOR.
      MOVE '%PVENC'    TO WA_COLOR-FNAME.
      MOVE '3'         TO WA_COLOR-COLOR-COL.
      MOVE '1'         TO WA_COLOR-COLOR-INT.
      MOVE '1'         TO WA_COLOR-COLOR-INV.
      APPEND WA_COLOR  TO IT_COLOR.
    ELSE.
      CLEAR WA_COLOR.
      MOVE '%PVENC'    TO WA_COLOR-FNAME.
      MOVE '5'         TO WA_COLOR-COLOR-COL.
      MOVE '1'         TO WA_COLOR-COLOR-INT.
      MOVE '1'         TO WA_COLOR-COLOR-INV.
      APPEND WA_COLOR  TO IT_COLOR.
    ENDIF.

    WA_SAIDA_REL-CELL_COLOR[] = IT_COLOR[].

    APPEND WA_SAIDA_REL TO IT_SAIDA_REL.
    CLEAR WA_SAIDA_REL.
    CLEAR WA_SAIDA.
    CLEAR IT_COLOR[].
  ENDLOOP.

  SORT IT_SAIDA_REL ASCENDING BY BUKRS IWERK.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXIBIR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXIBIR_ALV .
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'T001'.
  SET TITLEBAR 'T002'.


  PERFORM FILL_IT_FIELDCATALOG USING:

        01 'BUKRS '      'ILOA   '    '25'  ' '     ' '    ' '   'Empresa        '  'C410' '' ,
        02 'IWERK '      'MPOS   '    '25'  ' '     ' '    ' '   'Centro         '  '' '' ,
*        02 'WARPL '      'MPOS   '    '10'  ' '     ' '    ' '   'Plano          '  '' '' ,
*        03 'ILOAN '      'MPOS   '    '30'  ' '     ' '    ' '   'Cod Local      '  '' '' ,
*        04 'EQUNR '      'MPOS   '    '30'  ' '     ' '    ' '   'Equipamento    '  '' '' ,
*        05 'PLNNR '      'MPOS   '    '30'  ' '     ' '    ' '   'GRP Lista      '  '' '' ,
*        06 'PLNAL '      'MPOS   '    '30'  ' '     ' '    ' '   'Numerador GRP  '  '' '' ,
        04 'TPLANO '     '       '    '30'  ' '     ' '    'X'   'Total Planos   '  '' '' ,
        05 'TPVENC '     '       '    '30'  ' '     ' '    'X'   'Planos Venc    '  '' '' ,
        06 'TPCERT '     '       '    '30'  ' '     ' '    'X'   'Planos em dias '  '' '' ,
        07 '%PVENC '     '       '    '30'  ' '     ' '    ' '   '% planos venc  '  '' '' ,
        08 'ZVALOR1'     '       '    '30'  ' '     ' '    'X'   '0 à 33%        '  '' '' ,
        10 'ZVALOR2 '     '       '    '30'  ' '     ' '   'X'   '33% à 66%      '  '' '' ,
        11 'ZVALOR3 '     '       '    '30'  ' '     ' '   'X'   '66% à 99%      '  '' '' ,
        12 'ZVALOR4 '     '       '    '30'  ' '     ' '   'X'   'Acima 100%     '  '' '' .

  PERFORM CRIA_CONTAINER.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_01     text
*      -->P_0124   text
*      -->P_0125   text
*      -->P_0126   text
*      -->P_0127   text
*      -->P_0128   text
*      -->P_0129   text
*      -->P_0130   text
*      -->P_0131   text
*      -->P_0132   text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG  USING    VALUE(P_COLNUM)
                                    VALUE(P_FIELDNAME)
                                    VALUE(P_TABNAME)
                                    VALUE(P_LEN)
                                    VALUE(P_EDIT)
                                    VALUE(P_ICON)
                                    VALUE(P_DO_SUM)
                                    VALUE(P_HEADER)
                                    VALUE(P_EMPHASIZE)
                                    VALUE(P_HOTSPOT).

  DATA:  WA_FIELDCATALOG  TYPE LVC_S_FCAT.

  WA_FIELDCATALOG-COL_POS     = P_COLNUM.
  WA_FIELDCATALOG-FIELDNAME   = P_FIELDNAME.
  WA_FIELDCATALOG-TABNAME     = P_TABNAME.
  WA_FIELDCATALOG-OUTPUTLEN   = P_LEN.
  WA_FIELDCATALOG-COLTEXT     = P_HEADER.
  WA_FIELDCATALOG-EDIT        = P_EDIT.
  WA_FIELDCATALOG-ICON        = P_ICON.
  WA_FIELDCATALOG-REF_TABLE   = P_TABNAME.
  WA_FIELDCATALOG-CHECKTABLE  = P_TABNAME.
  WA_FIELDCATALOG-DO_SUM      = P_DO_SUM.
  WA_FIELDCATALOG-EMPHASIZE   = P_EMPHASIZE.
  WA_FIELDCATALOG-HOTSPOT     = P_HOTSPOT.

  GS_LAYOUT-CTAB_FNAME    = 'CELL_COLOR'.
  GS_LAYOUT-EXCP_CONDS    = 'X'.
  GS_LAYOUT-ZEBRA         = 'X'.
  GS_LAYOUT-SEL_MODE      = 'A'.
  GS_LAYOUT-CWIDTH_OPT    = 'X'.     "  Otimizar colunas na tela
  GS_LAYOUT-TOTALS_BEF    = ''.

  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CRIA_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIA_CONTAINER.

  IF G_CONTAINER IS INITIAL.

    CREATE OBJECT G_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = G_CONTAINER.

    SET HANDLER: OBJ_EVEN->HANDLE_DOUBLE_CLICK FOR CTL_ALV.
*                OBJ_EVEN->ON_HOTSPOT_CLICK    FOR CTL_ALV.


    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_SAIDA_REL
        IT_SORT              = IT_SORT.

  ELSE.
    CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OBTEM_POS_TOT_CONTADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MMPT_POINT  text
*----------------------------------------------------------------------*
FORM OBTEM_POS_TOT_CONTADOR USING POINT.

  DATA : NO_BELEG LIKE SY-SUBRC,
         FLTP     TYPE IMRC_TOTAC.

  CALL FUNCTION 'MEASUREM_POINT_LAST_VALUE'
    EXPORTING
      I_POINT           = POINT
    IMPORTING
      E_WA_POINT        = WA_IMPT
      E_POINT_TXT       = POINT_TXT
      E_WA_VALUE        = WA_IMRG
      E_NO_VALUE        = NO_BELEG
    EXCEPTIONS
      POINTER_NOT_FOUND = 01.

  CASE SY-SUBRC.
    WHEN 0.
      IF NO_BELEG IS INITIAL.
        IF NOT WA_IMRG-READG IS INITIAL.
          PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR WA_IMRG-READG WA_IMPT-MSEHI.
          W_RELATORIO-TOTAC  =  FLTP_CHAR+14(8).
          CONDENSE W_RELATORIO-TOTAC.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
      CLEAR: W_RELATORIO-TOTAC.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FLTP_CHAR_CONVERSION_PAK_F40
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FLTP_CHAR  text
*      -->P_WA_IMRG_READG  text
*      -->P_WA_IMPT_MSEHI  text
*----------------------------------------------------------------------*
FORM FLTP_CHAR_CONVERSION_PAK_F40  USING  CHAR_WERT
                                          FLTP_WERT
                                            EINHEIT.

  CLEAR CHAR_WERT.
  CHECK NOT EINHEIT IS INITIAL.

  CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
    EXPORTING
      CHAR_UNIT       = EINHEIT
      DECIMALS        = 0
      EXPONENT        = 0
      FLTP_VALUE_SI   = FLTP_WERT
      INDICATOR_VALUE = CC_X
      MASC_SYMBOL     = ' '
    IMPORTING
      CHAR_VALUE      = CHAR_WERT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OBTEM_CONTADORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_MPOS_WARPL  text
*      -->P_MMPT_ZEIEH  text
*----------------------------------------------------------------------*
FORM OBTEM_CONTADORES  USING P_WARPL P_ZEIEH.

  IF P_ZEIEH EQ 'H' OR P_ZEIEH EQ 'KM'.
    CLEAR: T_VIMHIS, W_RELATORIO-IDAT2.

    SELECT * FROM VIMHIS
      INTO  TABLE T_VIMHIS
      WHERE WARPL = P_WARPL.

    IF SY-SUBRC NE 0.
      RETURN.
    ENDIF.


    LOOP AT T_VIMHIS.
      IF T_VIMHIS-TSABR NE SPACE AND T_VIMHIS-LRMDT NE 0.  " Concluido
        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR T_VIMHIS-RZAEH P_ZEIEH.
        W_RELATORIO-RZAEH = FLTP_CHAR+14(8).           " Posição da última execução.
        CONDENSE W_RELATORIO-RZAEH.
        W_RELATORIO-IDAT2 = T_VIMHIS-LRMDT.            " Data de Conclusão
        CONTINUE.
      ENDIF.

      SELECT SINGLE *
      FROM MHIO
      WHERE WARPL EQ T_VIMHIS-WARPL
       AND  ABNUM EQ T_VIMHIS-ABNUM.

      IF T_VIMHIS-TSABR NE SPACE AND MHIO-ADDAT NE 0.  " Concluido
        CONTINUE.
      ENDIF.

      IF T_VIMHIS-TSTAT = CC_X.                        " Ignorado.
        CONTINUE.
      ENDIF.

      IF T_VIMHIS-TSABR NE SPACE.                      " Solicitado.
        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR T_VIMHIS-SZAEH P_ZEIEH.
        W_RELATORIO-SZAEH  =  FLTP_CHAR+14(8).         " Posição do contador.
        CONDENSE W_RELATORIO-SZAEH.

        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR T_VIMHIS-NZAEH P_ZEIEH.
        W_RELATORIO-NZAEH  =  FLTP_CHAR+14(8).       " Proxima leitura programa
        CONDENSE W_RELATORIO-NZAEH.

        IF WA_MPOS-WSTRA IS INITIAL.
          GV_USO    = WA_IMRG-READG - T_VIMHIS-NZAEH + MMPT-ZYKL1.
          GV_DESVIO = GV_USO - MMPT-ZYKL1.
        ELSE.
          GV_USO    = WA_IMRG-READG - T_VIMHIS-NZAEH + MMPT_TAB-ZYKL1.
          GV_DESVIO = GV_USO - MMPT_TAB-ZYKL1.
        ENDIF.

        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR GV_USO P_ZEIEH.
        W_RELATORIO-USO    =  FLTP_CHAR+14(8).       " Uso
        CONDENSE W_RELATORIO-USO.

        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR GV_DESVIO P_ZEIEH.
        W_RELATORIO-DESVIO =  FLTP_CHAR+14(8).       " Desvio
        CONDENSE W_RELATORIO-DESVIO.

        IF W_RELATORIO-ZYKL1 > 0.
          W_RELATORIO-PER_DESV = ( W_RELATORIO-DESVIO / W_RELATORIO-ZYKL1 ) * 100.
        ELSE.
          W_RELATORIO-PER_DESV = 0.
        ENDIF.

        APPEND W_RELATORIO TO T_RELATORIO.
        CLEAR: "W_RELATORIO-TIDNR,
               W_RELATORIO-EQUNR,
               W_RELATORIO-EQKTX,
               W_RELATORIO-EARTX,
               W_RELATORIO-CELL_COLOR[],
               W_RELATORIO-RZAEH,
               IT_COLOR[],
               W_RELATORIO,
               WA_MPOS,
               T_VIMHIS,
               IT_CALC.
        EXIT.
      ENDIF.

      IF T_VIMHIS-TSVBT NE SPACE AND T_VIMHIS-TSABR EQ SPACE.
        IF T_VIMHIS-TSENQ NE SPACE.                      "Espera.
          PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR T_VIMHIS-SZAEH P_ZEIEH.
          W_RELATORIO-SZAEH  =  FLTP_CHAR+14(8).         " Posição do contador.
          CONDENSE W_RELATORIO-SZAEH.

          PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR T_VIMHIS-NZAEH P_ZEIEH.
          W_RELATORIO-NZAEH  =  FLTP_CHAR+14(8).       " Proxima leitura programa
          CONDENSE W_RELATORIO-NZAEH.

          IF WA_MPOS-WSTRA IS INITIAL.
            GV_USO    = WA_IMRG-READG - T_VIMHIS-NZAEH + MMPT-ZYKL1.
            GV_DESVIO = GV_USO - MMPT-ZYKL1.
          ELSE.
            GV_USO    = WA_IMRG-READG - T_VIMHIS-NZAEH + MMPT_TAB-ZYKL1.
            GV_DESVIO = GV_USO - MMPT_TAB-ZYKL1.
          ENDIF.

          PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR GV_USO P_ZEIEH.
          W_RELATORIO-USO    = FLTP_CHAR+14(8).       " Uso
          CONDENSE  W_RELATORIO-USO.

          PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR GV_DESVIO P_ZEIEH.
          W_RELATORIO-DESVIO = FLTP_CHAR+14(8).       " Uso
          CONDENSE W_RELATORIO-DESVIO.

          IF W_RELATORIO-ZYKL1 > 0.
            W_RELATORIO-PER_DESV = ( W_RELATORIO-DESVIO / W_RELATORIO-ZYKL1 ) * 100.
          ELSE.
            W_RELATORIO-PER_DESV = 0.
          ENDIF.

          APPEND W_RELATORIO TO T_RELATORIO.

          CLEAR: "W_RELATORIO-TIDNR,
                 W_RELATORIO-EQUNR,
                 W_RELATORIO-EQKTX,
                 W_RELATORIO-EARTX,
                 W_RELATORIO-CELL_COLOR[],
                 W_RELATORIO-RZAEH,
                 IT_COLOR[],
                 W_RELATORIO,
                 WA_MPOS,
                 T_VIMHIS,
                 IT_CALC.

        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSE.
    PERFORM AGRUPA_CALC USING P_WARPL W_RELATORIO-IDAT2.
    PERFORM CALC USING P_WARPL P_ZEIEH.
    APPEND W_RELATORIO TO T_RELATORIO.
    CLEAR: W_RELATORIO,
           WA_MPOS,
           T_VIMHIS,
           IT_CALC.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DT  text
*      -->P_P_WARPL  text
*----------------------------------------------------------------------*
FORM CALC  USING V_WARPL P_ZEIEH.

  IF P_ZEIEH = 'WCH' OR P_ZEIEH = 'MON' OR P_ZEIEH = 'TAG'.
    TRY .
        DATA(WA) = IT_CALC[ WARPL = V_WARPL ].
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
        CLEAR WA.
    ENDTRY.
    W_RELATORIO-USO      = WA-USO.
    CONDENSE W_RELATORIO-USO.
    W_RELATORIO-DESVIO   = WA-DESVIO.
    CONDENSE W_RELATORIO-DESVIO.
    W_RELATORIO-PER_DESV = WA-PER_DESV.
    W_RELATORIO-SZAEH    = |{ WA-STADT+6(2) }{ WA-STADT+4(2) }{ WA-STADT(4) }|.
    W_RELATORIO-NZAEH    = |{ WA-NPLDA+6(2) }{ WA-NPLDA+4(2) }{ WA-NPLDA(4) }|.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AGRUPA_CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_WARPL  text
*      -->P_W_RELATORIO_IDAT2  text
*----------------------------------------------------------------------*
FORM AGRUPA_CALC  USING  V_WARPL V_IDAT2.

*  SELECT *
*  FROM MPLA AS A
*  INNER JOIN MPOS AS B ON A~WARPL  = B~WARPL
*  INNER JOIN MMPT AS M ON A~WARPL  = M~WARPL
*  INNER JOIN ILOA AS C ON C~ILOAN  = B~ILOAN
*  INNER JOIN MHIS AS D ON A~WARPL  = D~WARPL
*  INTO CORRESPONDING FIELDS OF TABLE IT_CALC
*  FOR ALL ENTRIES IN IT_MPOS
*  WHERE A~WARPL EQ IT_MPOS-WARPL
*  AND D~STADT EQ ( SELECT MAX( STADT ) FROM MHIS WHERE WARPL EQ D~WARPL ).
*     AND NPLDA > SY-DATUM
*  AND D~ABNUM EQ ( SELECT MAX( ABNUM ) FROM MHIS WHERE WARPL EQ D~WARPL AND HORDA = '00000000' ).
  SORT IT_CALC BY STADT.

  DATA: CONVERT      TYPE I,
        DATA_PROXIMA TYPE MHIS-LRMDT.

  LOOP AT IT_CALC ASSIGNING FIELD-SYMBOL(<CALC>) WHERE WARPL EQ V_WARPL.

    <CALC>-LRMDT = V_IDAT2.

    PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR <CALC>-ZYKL1 <CALC>-ZEIEH.
    CONDENSE FLTP_CHAR NO-GAPS.
    <CALC>-ZYKL2 = FLTP_CHAR. "CICLO

    IF <CALC>-STADT IS NOT INITIAL AND <CALC>-LRMDT IS NOT INITIAL.
      DATA(DT_MENOR1) = SY-DATUM - <CALC>-STADT.
      DATA(DT_MENOR2) = SY-DATUM - <CALC>-LRMDT.
      IF DT_MENOR1 < DT_MENOR2.
        DATA_PROXIMA = <CALC>-STADT.
      ELSE.
        DATA_PROXIMA = <CALC>-LRMDT.
      ENDIF.
    ELSEIF <CALC>-STADT IS NOT INITIAL AND <CALC>-LRMDT IS INITIAL.
      DATA_PROXIMA = <CALC>-STADT.
    ELSEIF <CALC>-STADT IS INITIAL AND <CALC>-LRMDT IS NOT INITIAL.
      DATA_PROXIMA = <CALC>-LRMDT.
    ENDIF.

*     IF NOT  <CALC>-ZEIEH IS INITIAL.
*      CALL FUNCTION 'DIMENSIONCHECK_TIME'
*           EXPORTING
*                meinh              =  <CALC>-ZEIEH
*           EXCEPTIONS
*                dimension_not_time = 1.
*    ENDIF.

    CASE <CALC>-ZEIEH.
      WHEN 'WCH'. "Semanas
        CONVERT =  ( ( SY-DATUM - DATA_PROXIMA ) / 7 ).  "CALC USO SEMANAS
*        <CALC>-ZEIEH = 'SEMANA'.
      WHEN 'MON'. "Meses
        CONVERT =  ( ( SY-DATUM - DATA_PROXIMA ) / 30 ). "CALC USO MESES
*        <CALC>-ZEIEH = 'MES'.
      WHEN 'TAG'. "Dias
        CONVERT =  (   SY-DATUM - DATA_PROXIMA ).          "CALC USO DIAS
*        <CALC>-ZEIEH = 'DIA'.
    ENDCASE.

    <CALC>-USO =  CONVERT. "CONVERT USO

    CONVERT =  <CALC>-USO - <CALC>-ZYKL2. " CALC DESVIO
    <CALC>-DESVIO =  CONVERT. " CONVERT DESVIO

    CONVERT = CONVERT * -1.
    <CALC>-PER_DESV =  ( ( CONVERT / <CALC>-ZYKL2 ) * 100 ). " PORCENTAGEM DO DESVIO

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  STATUS_CHECK_PLANO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MPLA_OBJNR  text
*      -->P_RETURN_CODE  text
*----------------------------------------------------------------------*
FORM STATUS_CHECK_PLANO  USING MPLA_OBJNR RETURN_CODE.

  CONSTANTS: Y_I0013_LOKZ LIKE JEST-STAT  VALUE 'I0013',
             Y_I0076_LOVM LIKE JEST-STAT  VALUE 'I0076',
             Y_I0320_INAK LIKE JEST-STAT  VALUE 'I0320'.

  DATA: BEGIN OF IJSTAT OCCURS   0.
          INCLUDE STRUCTURE JSTAT.
  DATA: END OF IJSTAT.

  CALL FUNCTION 'STATUS_READ'
    EXPORTING
      CLIENT      = SY-MANDT
      OBJNR       = MPLA_OBJNR
      ONLY_ACTIVE = 'X'
    TABLES
      STATUS      = IJSTAT.

  LOOP AT IJSTAT.

    CASE IJSTAT-STAT.
      WHEN Y_I0013_LOKZ.
        RETURN_CODE = 1.
      WHEN Y_I0076_LOVM.
        RETURN_CODE = 2.
        EXIT.
      WHEN Y_I0320_INAK.               "Inativo
        RETURN_CODE = 3.
      WHEN OTHERS.
        RETURN_CODE = 0.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_PLANOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM SEL_PLANOS  USING P_ROW P_COLUMN.

  FREE CLICKS.
  ADD 1 TO CLICKS.

  TRY .
      DATA(WA_SAIDA) = IT_SAIDA_REL[ P_ROW ].
    CATCH CX_SY_ITAB_LINE_NOT_FOUND.
  ENDTRY.

  FREE T_PLANOS.
  FREE IT_RELATORIO.
  IT_RELATORIO = T_RELATORIO.

  CASE P_COLUMN.
      WA_SAIDA-IWERK = WA_SAIDA-IWERK(4).

      DELETE IT_RELATORIO WHERE IWERK NE WA_SAIDA-IWERK.

    WHEN 'TPLANO'.
      LOOP AT IT_RELATORIO INTO W_RELATORIO.
        IF W_RELATORIO-WARPL IS NOT INITIAL AND W_RELATORIO-IWERK IS NOT INITIAL.
          MOVE-CORRESPONDING W_RELATORIO TO W_PLANOS.
          W_PLANOS-WARPL = |{ W_PLANOS-WARPL ALPHA = OUT }|.
          W_PLANOS-EQUNR = |{ W_PLANOS-EQUNR ALPHA = OUT }|.
          W_PLANOS-ORDEM = ICON_ORDER.

          CASE W_PLANOS-ZEIEH.
            WHEN 'WCH'.
              W_PLANOS-ZEIEH = 'Semanas'.
            WHEN 'MON'.
              W_PLANOS-ZEIEH = 'Mes'.
            WHEN 'TAG'.
              W_PLANOS-ZEIEH = 'Dia'.
            WHEN OTHERS.
          ENDCASE.

          APPEND W_PLANOS TO T_PLANOS.
          CLEAR: W_PLANOS, W_RELATORIO, WA_SAIDA.
        ENDIF.
      ENDLOOP.

    WHEN 'TPVENC'.

      LOOP AT IT_RELATORIO INTO W_RELATORIO.

        IF W_RELATORIO-WARPL IS NOT INITIAL AND W_RELATORIO-IWERK IS NOT INITIAL.
          IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
            MOVE-CORRESPONDING W_RELATORIO TO W_PLANOS.
            W_PLANOS-WARPL = |{ W_PLANOS-WARPL ALPHA = OUT }|.
            W_PLANOS-EQUNR = |{ W_PLANOS-EQUNR ALPHA = OUT }|.
            W_PLANOS-ORDEM = ICON_ORDER.

            CASE W_PLANOS-ZEIEH.
              WHEN 'WCH'.
                W_PLANOS-ZEIEH = 'Semanas'.
              WHEN 'MON'.
                W_PLANOS-ZEIEH = 'Mes'.
              WHEN 'TAG'.
                W_PLANOS-ZEIEH = 'Dia'.
              WHEN OTHERS.
            ENDCASE.

            APPEND W_PLANOS TO T_PLANOS.
            CLEAR: W_PLANOS, W_RELATORIO, WA_SAIDA.
          ENDIF.
        ELSE.
        ENDIF.

      ENDLOOP.

    WHEN 'TPCERT'.
      LOOP AT IT_RELATORIO INTO W_RELATORIO.

        IF W_RELATORIO-WARPL IS NOT INITIAL AND W_RELATORIO-IWERK IS NOT INITIAL.
          IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
          ELSE.
            MOVE-CORRESPONDING W_RELATORIO TO W_PLANOS.
            W_PLANOS-WARPL = |{ W_PLANOS-WARPL ALPHA = OUT }|.
            W_PLANOS-EQUNR = |{ W_PLANOS-EQUNR ALPHA = OUT }|.
            W_PLANOS-ORDEM = ICON_ORDER.

            CASE W_PLANOS-ZEIEH.
              WHEN 'WCH'.
                W_PLANOS-ZEIEH = 'Semanas'.
              WHEN 'MON'.
                W_PLANOS-ZEIEH = 'Mes'.
              WHEN 'TAG'.
                W_PLANOS-ZEIEH = 'Dia'.
              WHEN OTHERS.
            ENDCASE.

            APPEND W_PLANOS TO T_PLANOS.
            CLEAR: W_PLANOS, W_RELATORIO, WA_SAIDA.
          ENDIF.
        ENDIF.

      ENDLOOP.

    WHEN 'ZVALOR1'. "0% A 33%

      LOOP AT IT_RELATORIO INTO W_RELATORIO.

        IF W_RELATORIO-WARPL IS NOT INITIAL AND W_RELATORIO-IWERK IS NOT INITIAL.
          IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
            IF W_RELATORIO-PER_DESV BETWEEN ZVALOR  AND ZVALOR1
            OR W_RELATORIO-PER_DESV BETWEEN ZVALOR6 AND ZVALOR.

              MOVE-CORRESPONDING W_RELATORIO TO W_PLANOS.
              W_PLANOS-WARPL = |{ W_PLANOS-WARPL ALPHA = OUT }|.
              W_PLANOS-EQUNR = |{ W_PLANOS-EQUNR ALPHA = OUT }|.
              W_PLANOS-ORDEM = ICON_ORDER.

              CASE W_PLANOS-ZEIEH.
                WHEN 'WCH'.
                  W_PLANOS-ZEIEH = 'Semanas'.
                WHEN 'MON'.
                  W_PLANOS-ZEIEH = 'Mes'.
                WHEN 'TAG'.
                  W_PLANOS-ZEIEH = 'Dia'.
                WHEN OTHERS.
              ENDCASE.

              APPEND W_PLANOS TO T_PLANOS.
              CLEAR: W_PLANOS, W_RELATORIO, WA_SAIDA.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

    WHEN 'ZVALOR2'. "34% A 66%

      LOOP AT IT_RELATORIO INTO W_RELATORIO.

        IF W_RELATORIO-WARPL IS NOT INITIAL AND W_RELATORIO-IWERK IS NOT INITIAL.
          IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
            IF  W_RELATORIO-PER_DESV BETWEEN ZVALOR4 AND ZVALOR2
            OR  W_RELATORIO-PER_DESV BETWEEN ZVALOR7 AND ZVALOR9.

              MOVE-CORRESPONDING W_RELATORIO TO W_PLANOS.
              W_PLANOS-WARPL = |{ W_PLANOS-WARPL ALPHA = OUT }|.
              W_PLANOS-EQUNR = |{ W_PLANOS-EQUNR ALPHA = OUT }|.
              W_PLANOS-ORDEM = ICON_ORDER.

              CASE W_PLANOS-ZEIEH.
                WHEN 'WCH'.
                  W_PLANOS-ZEIEH = 'Semanas'.
                WHEN 'MON'.
                  W_PLANOS-ZEIEH = 'Mes'.
                WHEN 'TAG'.
                  W_PLANOS-ZEIEH = 'Dia'.
                WHEN OTHERS.
              ENDCASE.

              APPEND W_PLANOS TO T_PLANOS.
              CLEAR: W_PLANOS, W_RELATORIO, WA_SAIDA.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

    WHEN 'ZVALOR3'. "67% A 99%

      LOOP AT IT_RELATORIO INTO W_RELATORIO.

        IF W_RELATORIO-WARPL IS NOT INITIAL AND W_RELATORIO-IWERK IS NOT INITIAL.
          IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
            IF W_RELATORIO-PER_DESV  BETWEEN ZVALOR5 AND ZVALOR3
            OR W_RELATORIO-PER_DESV  BETWEEN ZVALOR8 AND ZVALOR10.

              MOVE-CORRESPONDING W_RELATORIO TO W_PLANOS.
              W_PLANOS-WARPL = |{ W_PLANOS-WARPL ALPHA = OUT }|.
              W_PLANOS-EQUNR = |{ W_PLANOS-EQUNR ALPHA = OUT }|.
              W_PLANOS-ORDEM = ICON_ORDER.

              CASE W_PLANOS-ZEIEH.
                WHEN 'WCH'.
                  W_PLANOS-ZEIEH = 'Semanas'.
                WHEN 'MON'.
                  W_PLANOS-ZEIEH = 'Mes'.
                WHEN 'TAG'.
                  W_PLANOS-ZEIEH = 'Dia'.
                WHEN OTHERS.
              ENDCASE.

              APPEND W_PLANOS TO T_PLANOS.
              CLEAR: W_PLANOS, W_RELATORIO, WA_SAIDA.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

    WHEN 'ZVALOR4'. "Acima de 99%

      LOOP AT IT_RELATORIO INTO W_RELATORIO.

        IF W_RELATORIO-WARPL IS NOT INITIAL AND W_RELATORIO-IWERK IS NOT INITIAL.
          IF W_RELATORIO-DESVIO IS NOT INITIAL AND W_RELATORIO-DESVIO > 0.
            IF W_RELATORIO-PER_DESV > ZVALOR3
            OR W_RELATORIO-PER_DESV < ZVALOR8.

              MOVE-CORRESPONDING W_RELATORIO TO W_PLANOS.
              W_PLANOS-WARPL = |{ W_PLANOS-WARPL ALPHA = OUT }|.
              W_PLANOS-EQUNR = |{ W_PLANOS-EQUNR ALPHA = OUT }|.
              W_PLANOS-ORDEM = ICON_ORDER.

              CASE W_PLANOS-ZEIEH.
                WHEN 'WCH'.
                  W_PLANOS-ZEIEH = 'Semanas'.
                WHEN 'MON'.
                  W_PLANOS-ZEIEH = 'Mes'.
                WHEN 'TAG'.
                  W_PLANOS-ZEIEH = 'Dia'.
                WHEN OTHERS.
              ENDCASE.

              APPEND W_PLANOS TO T_PLANOS.
              CLEAR: W_PLANOS, W_RELATORIO, WA_SAIDA.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

  ENDCASE.

  SORT T_PLANOS ASCENDING BY IWERK WARPL.

  CALL SCREEN 0200.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'T2001'.
  SET TITLEBAR 'T2002'.

  PERFORM FILL_IT_FIELDCATALOG2 USING:

        01 'IWERK     '      ' '    '25'  ' '     ' '    ' '   'Centro                                      '  '' '' ,
        02 'PLTXT     '      ' '    '25'  ' '     ' '    ' '   'Local instalação                            '  '' '' ,
        03 'EQUNR     '      ' '    '10'  ' '     ' '    ' '   'Equipamento                                 '  '' '' ,
        04 'EQKTX     '      ' '    '30'  ' '     ' '    ' '   'Desc equipamento                            '  '' '' ,
*        04 'EARTX     '      ' '    '30'  ' '     ' '    ' '   'Tipo de veículo                             '  '' '' ,
        05 'WARPL     '      ' '    '30'  ' '     ' '    ' '   'Plano                                       '  '' '' ,
        06 'WPTXT     '      ' '    '30'  ' '     ' '    ' '   'Descrição plano                             '  '' '' ,
        07 'ZYKL1     '      ' '    '30'  ' '     ' '    ' '   'Ciclo / offset                              '  '' '' ,
        08 'ZEIEH     '      ' '    '30'  ' '     ' '    ' '   'Unidade medida                              '  '' '' ,
        09 'PAK_TEXT  '      ' '    '30'  ' '     ' '    ' '   'Texto p/o pacote ou ciclo                   '  '' '' ,
        10 'SZAEH     '      ' '    '30'  ' '     ' '    ' '   'Posição Contador                            '  '' '' ,
        11 'USO       '      ' '    '30'  ' '     ' '    ' '   'Utilização                                  '  '' '' ,
        12 'DESVIO    '      ' '    '30'  ' '     ' '    ' '   'Desvio                                      '  '' '' ,
        13 'PER_DESV  '      ' '    '25'  ' '     ' '    ' '   '% Desvio                                    '  '' '' ,
        14 'NZAEH     '      ' '    '30'  ' '     ' '    ' '   'Próx leitura                                '  '' '' ,
        15 'IDAT2     '      ' '    '25'  ' '     ' '    ' '   'Data Ultima_troca                           '  '' '' ,
        16 'RZAEH     '      ' '    '30'  ' '     ' '    ' '   'Posição contador conclusão                  '  '' '' ,
        17 'TOTAC     '      ' '    '30'  ' '     ' '    ' '   'Posição Total Contador                      '  '' '' ,
        18 'BAUTL     '      ' '    '10'  ' '     ' '    ' '   'Sistema                                     '  '' '' ,
        19 'ORDEM     '      ' '    '10'  ' '     ' '    ' '   'Ordem/Nota                                  '  '' '' .
*        03 'CELL_COLOR'      ' '    '30'  ' '     ' '    ' '   'Cor da Célula                               '  '' '' ,

  IF G_CONTAINER2 IS INITIAL.

    CREATE OBJECT G_CONTAINER2
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_PLAN'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    CREATE OBJECT CTL_ALV2
      EXPORTING
        I_PARENT = G_CONTAINER2.

    SET HANDLER: OBJ_EVEN->HANDLE_DOUBLE_CLICK_ORDEM FOR CTL_ALV2.

    CALL METHOD CTL_ALV2->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT2
        IS_VARIANT           = GS_VARIANT2
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE2
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG2
        IT_OUTTAB            = T_PLANOS
        IT_SORT              = IT_SORT2.

  ELSE.
    CALL METHOD CTL_ALV2->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_01     text
*      -->P_2897   text
*      -->P_2898   text
*      -->P_2899   text
*      -->P_2900   text
*      -->P_2901   text
*      -->P_2902   text
*      -->P_2903   text
*      -->P_2904   text
*      -->P_2905   text
*----------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG2  USING    VALUE(P_COLNUM)
                                    VALUE(P_FIELDNAME)
                                    VALUE(P_TABNAME)
                                    VALUE(P_LEN)
                                    VALUE(P_EDIT)
                                    VALUE(P_ICON)
                                    VALUE(P_DO_SUM)
                                    VALUE(P_HEADER)
                                    VALUE(P_EMPHASIZE)
                                    VALUE(P_HOTSPOT).

  DATA:  WA_FIELDCATALOG2  TYPE LVC_S_FCAT.

  WA_FIELDCATALOG2-COL_POS     = P_COLNUM.
  WA_FIELDCATALOG2-FIELDNAME   = P_FIELDNAME.
  WA_FIELDCATALOG2-TABNAME     = P_TABNAME.
  WA_FIELDCATALOG2-OUTPUTLEN   = P_LEN.
  WA_FIELDCATALOG2-COLTEXT     = P_HEADER.
  WA_FIELDCATALOG2-EDIT        = P_EDIT.
  WA_FIELDCATALOG2-ICON        = P_ICON.
  WA_FIELDCATALOG2-REF_TABLE   = P_TABNAME.
  WA_FIELDCATALOG2-CHECKTABLE  = P_TABNAME.
  WA_FIELDCATALOG2-DO_SUM      = P_DO_SUM.
  WA_FIELDCATALOG2-EMPHASIZE   = P_EMPHASIZE.
  WA_FIELDCATALOG2-HOTSPOT     = P_HOTSPOT.

*  GS_LAYOUT2-CTAB_FNAME    = 'CELL_COLOR'.
  GS_LAYOUT2-EXCP_CONDS    = 'X'.
  GS_LAYOUT2-ZEBRA         = 'X'.
  GS_LAYOUT2-SEL_MODE      = 'A'.
  GS_LAYOUT2-CWIDTH_OPT    = 'X'.     "  Otimizar colunas na tela
  GS_LAYOUT2-TOTALS_BEF    = ''.

  APPEND WA_FIELDCATALOG2 TO IT_FIELDCATALOG2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM SEL_ORDEM  USING P_ROW P_COLUMN_FIELDNAME.

  FREE CLICKS.
  ADD 1 TO CLICKS.

  TRY .
      DATA(WA_PLANOS) = T_PLANOS[ P_ROW ].
    CATCH CX_SY_ITAB_LINE_NOT_FOUND.
  ENDTRY.

  CASE P_COLUMN_FIELDNAME.

    WHEN 'ORDEM'.
      WA_PLANOS-WARPL = |{ WA_PLANOS-WARPL ALPHA = IN }|.

      SELECT *
      FROM VIAUFKST AS A
      INNER JOIN CAUFV AS B ON B~AUFNR = A~AUFNR
      INTO CORRESPONDING FIELDS OF TABLE T_ORDEM
      WHERE A~WERKS EQ WA_PLANOS-IWERK
        AND A~WARPL EQ WA_PLANOS-WARPL.
      SORT T_ORDEM ASCENDING BY ERDAT.

      SELECT *
      FROM VIQMEL
      INTO CORRESPONDING FIELDS OF TABLE T_VIQMEL
      WHERE IWERK EQ WA_PLANOS-IWERK
        AND WARPL EQ WA_PLANOS-WARPL.
      SORT T_VIQMEL ASCENDING BY QMDAT.

      IF NOT T_ORDEM IS INITIAL.
        FREE LINHA_SELECIONADA.
        FREE _EXIT.
        LOOP AT T_ORDEM ASSIGNING FIELD-SYMBOL(<W_ORDEM>).

          IF <W_ORDEM>-OBJNR IS NOT INITIAL.
            CALL FUNCTION 'STATUS_TEXT_EDIT'
              EXPORTING
                FLG_USER_STAT    = YX
                OBJNR            = <W_ORDEM>-OBJNR             "1695763
                SPRAS            = SY-LANGU
              IMPORTING
                LINE             = <W_ORDEM>-STTXT             "1695763
                USER_LINE        = <W_ORDEM>-ASTTX             "1695763
              EXCEPTIONS
                OBJECT_NOT_FOUND = 1
                OTHERS           = 2.

            IF SY-SUBRC = 0.
              <W_ORDEM>-STATUS = <W_ORDEM>-STTXT.
              <W_ORDEM>-STATUS = <W_ORDEM>-STATUS(4).
            ENDIF.
          ENDIF.
        ENDLOOP.

        CHECK ( T_ORDEM IS NOT INITIAL ).
*      DELETE T_ORDEM WHERE WARPL NE WA_PLANOS-WARPL.

        DATA(TL_FIELDCAT) = VALUE SLIS_T_FIELDCAT_ALV(

        ( FIELDNAME = 'BUKRS     '        SELTEXT_M = 'BUKRS  '  OUTPUTLEN = '07' )
        ( FIELDNAME = 'WERKS     '        SELTEXT_M = 'WERKS  '  OUTPUTLEN = '04' )
        ( FIELDNAME = 'WARPL     '        SELTEXT_M = 'WARPL  '  OUTPUTLEN = '10' )
        ( FIELDNAME = 'AUFNR     '        SELTEXT_M = 'AUFNR  '  OUTPUTLEN = '10' )
        ( FIELDNAME = 'STATUS    '        SELTEXT_M = 'STATUS '  OUTPUTLEN = '10' )
        ( FIELDNAME = 'ERDAT     '        SELTEXT_M = 'ERDAT  '  OUTPUTLEN = '10' ) ).

        CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
          EXPORTING
            I_TITLE     = 'Selecionar ordem de plano'
            I_SELECTION = 'X'
            I_TABNAME   = 'T_ORDEM'
            I_ZEBRA     = 'X'
            IT_FIELDCAT = TL_FIELDCAT
          IMPORTING
            ES_SELFIELD = LINHA_SELECIONADA
            E_EXIT      = _EXIT
          TABLES
            T_OUTTAB    = T_ORDEM.

      ELSE.
        FREE LINHA_SELECIONADA.
        FREE _EXIT.
        CHECK ( T_VIQMEL IS NOT INITIAL ).
        LOOP AT T_VIQMEL ASSIGNING FIELD-SYMBOL(<W_VIQMEL>).
          IF <W_VIQMEL>-OBJNR IS NOT INITIAL.
            CALL FUNCTION 'STATUS_TEXT_EDIT'
              EXPORTING
                FLG_USER_STAT    = YX
                OBJNR            = <W_VIQMEL>-OBJNR             "1695763
                SPRAS            = SY-LANGU
              IMPORTING
                LINE             = <W_VIQMEL>-STTXT             "1695763
                USER_LINE        = <W_VIQMEL>-ASTTX             "1695763
              EXCEPTIONS
                OBJECT_NOT_FOUND = 1
                OTHERS           = 2.

            IF SY-SUBRC = 0.
              <W_VIQMEL>-STATUS = <W_VIQMEL>-STTXT.
              <W_VIQMEL>-STATUS = <W_VIQMEL>-STATUS(4).
            ENDIF.
          ENDIF.
        ENDLOOP.

        DATA(_TL_FIELDCAT) = VALUE SLIS_T_FIELDCAT_ALV(

        ( FIELDNAME = 'BUKRS     '        SELTEXT_M = 'Empresa       '  OUTPUTLEN = '07' )
        ( FIELDNAME = 'IWERK     '        SELTEXT_M = 'Centro        '  OUTPUTLEN = '04' )
        ( FIELDNAME = 'WARPL     '        SELTEXT_M = 'Plano         '  OUTPUTLEN = '10' )
        ( FIELDNAME = 'QMNUM     '        SELTEXT_M = 'Nota          '  OUTPUTLEN = '10' )
        ( FIELDNAME = 'STATUS    '        SELTEXT_M = 'Status        '  OUTPUTLEN = '10' )
        ( FIELDNAME = 'QMDAT     '        SELTEXT_M = 'Data da nota  '  OUTPUTLEN = '10' ) ).

        CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
          EXPORTING
            I_TITLE     = 'Selecionar notas de plano'
            I_SELECTION = 'X'
            I_TABNAME   = 'T_VIQMEL'
            I_ZEBRA     = 'X'
            IT_FIELDCAT = _TL_FIELDCAT
          IMPORTING
            ES_SELFIELD = LINHA_SELECIONADA
            E_EXIT      = _EXIT
          TABLES
            T_OUTTAB    = T_VIQMEL.

      ENDIF.

      CASE LINHA_SELECIONADA-FIELDNAME.
        WHEN 'AUFNR'.
          SET PARAMETER ID 'ANR' FIELD LINHA_SELECIONADA-VALUE.
          CALL TRANSACTION 'IW32' AND SKIP FIRST SCREEN .
        WHEN 'WARPL'.
          SET PARAMETER ID 'MPL' FIELD LINHA_SELECIONADA-VALUE.
          CALL TRANSACTION 'IP10' AND SKIP FIRST SCREEN.
        WHEN 'QMNUM'.
          SET PARAMETER ID 'IQM' FIELD LINHA_SELECIONADA-VALUE.
          CALL TRANSACTION 'IW22' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.

    WHEN 'WARPL'.
      WA_PLANOS-WARPL = |{ WA_PLANOS-WARPL ALPHA = IN }|.
      SET PARAMETER ID 'MPL' FIELD WA_PLANOS-WARPL.
      CALL TRANSACTION 'IP10' AND SKIP FIRST SCREEN.

    WHEN 'EQUNR'.
      SET PARAMETER ID 'EQN' FIELD WA_PLANOS-EQUNR.
      CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.
  ENDCASE.











ENDFORM.
