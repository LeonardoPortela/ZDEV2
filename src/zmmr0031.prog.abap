*&--------------------------------------------------------------------&*
*& Report Name    : Take up por clientes                              *&
*& Author         : Victor Hugo                                       *&
*& Date           : 23.05.2012                                        *&
*& Funcional Area : MM                                                *&
*&                                                                    *&
*&--------------------------------------------------------------------&*
REPORT  ZMMR0031.

*&--------------------------------------------------------------------&*
*& Tables
*&--------------------------------------------------------------------&*
TABLES: MCHB, MARA, KNA1, MSEG, T001,  RBKP,MKPF.
*&--------------------------------------------------------------------&*
*& Types
*&--------------------------------------------------------------------&*
TYPES:
      BEGIN OF TY_MCHB,
        MATNR TYPE MCHB-MATNR,
        WERKS TYPE MCHB-WERKS,
        LGORT TYPE MCHB-LGORT,
        CHARG TYPE MCHB-CHARG,
        CSPEM TYPE MCHB-CSPEM,
       END OF TY_MCHB,

       BEGIN OF TY_KNA1,
         KUNNR TYPE KNA1-KUNNR,
         NAME1 TYPE KNA1-NAME1,
       END OF TY_KNA1,

       BEGIN OF TY_MSEG,
         MBLNR TYPE MSEG-MBLNR,
         MJAHR TYPE MSEG-MJAHR,
         ZEILE TYPE MSEG-ZEILE,
         MATNR TYPE MSEG-MATNR,
         WERKS TYPE MSEG-WERKS,
         LGORT TYPE MSEG-LGORT,
         CHARG TYPE MSEG-CHARG,
         KUNNR TYPE MSEG-KUNNR,
         SGTXT TYPE MSEG-SGTXT,
       END OF TY_MSEG,

      BEGIN OF TY_MKPF,
        MBLNR TYPE MKPF-MBLNR,
        MJAHR TYPE MKPF-MJAHR,
        BLDAT TYPE MKPF-BLDAT,
        BKTXT TYPE MKPF-BKTXT,
      END OF TY_MKPF,

      BEGIN OF TY_MAKT,
        MATNR TYPE MAKT-MATNR,
        MAKTX TYPE MAKT-MAKTX,
      END OF TY_MAKT,

      BEGIN OF TY_MARC,
        MATNR TYPE MARC-MATNR,
        WERKS TYPE MARC-WERKS,
      END OF TY_MARC,

      BEGIN OF TY_MARA,
        MATNR TYPE MARA-MATNR,
        MATKL TYPE MARA-MATKL,
      END OF TY_MARA,

      BEGIN OF TY_SAIDA,
        WERKS   TYPE MSEG-WERKS,
        LGORT   TYPE MSEG-LGORT,
        KUNNR   TYPE KNA1-KUNNR,
        NAME1   TYPE KNA1-NAME1,
        SGTXT   TYPE MKPF-BKTXT,
        MATNR   TYPE MCHB-MATNR,
        MAKTX   TYPE MAKT-MAKTX,
        FARDOS  TYPE SY-TABIX,
        CSPEM   TYPE MCHB-CSPEM,
        BLDAT   TYPE MKPF-BLDAT,
        TOTAL_F TYPE MCHB-CSPEM,
        TOTAL_P TYPE MCHB-CSPEM,
      END OF TY_SAIDA,

      BEGIN OF TY_DETALHADO,
        WERKSD TYPE MSEG-WERKS,
        KUNNR  TYPE KNA1-KUNNR,
        NAME_C TYPE KNA1-NAME1,
        BUTXT  TYPE T001-BUTXT,
        NAME1  TYPE T001W-NAME1,
        LGORT  TYPE MSEG-LGORT,
        CSPEM  TYPE MCHB-CSPEM,
        CHARG  TYPE MSEG-CHARG,
        BUKRS  TYPE T001-BUKRS,
        WERKS  TYPE T001W-WERKS,
        SGTXT  TYPE MSEG-SGTXT,
        BKTXT  TYPE MKPF-BKTXT,
        MATNR  TYPE MCHB-MATNR,
        BLDAT  TYPE MKPF-BLDAT,
      END OF TY_DETALHADO.
*&--------------------------------------------------------------------&*
*& Table Internal
*&--------------------------------------------------------------------&*
DATA: IT_MCHB      TYPE TABLE OF TY_MCHB,
      IT_MSEG      TYPE TABLE OF TY_MSEG,
      IT_MKPF      TYPE TABLE OF TY_MKPF,
      IT_MAKT      TYPE TABLE OF TY_MAKT,
      IT_MARC      TYPE TABLE OF TY_MARC,
      IT_KNA1      TYPE TABLE OF TY_KNA1,
      IT_MARA      TYPE TABLE OF TY_MARA,
      IT_SAIDA     TYPE TABLE OF TY_SAIDA,
      IT_SAIDA_AUX TYPE TABLE OF TY_SAIDA,
      IT_DISPLAY   TYPE TABLE OF TY_SAIDA,

      IT_DETALHADO TYPE TABLE OF TY_DETALHADO.
*&--------------------------------------------------------------------&*
*& Work Area
*&--------------------------------------------------------------------&*
DATA: WA_MCHB      TYPE TY_MCHB,
      WA_MSEG      TYPE TY_MSEG,
      WA_MKPF      TYPE TY_MKPF,
      WA_MAKT      TYPE TY_MAKT,
      WA_MARC      TYPE TY_MARC,
      WA_KNA1      TYPE TY_KNA1,
      WA_MARA      TYPE TY_MARA,
      WA_SAIDA     TYPE TY_SAIDA,
      WA_SAIDA_AUX TYPE TY_SAIDA,
      WA_DISPLAY   TYPE TY_SAIDA,
      WA_DETALHADO TYPE TY_DETALHADO.
*&--------------------------------------------------------------------&*
*& ALV
*&--------------------------------------------------------------------&*
DATA: CL_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_GRID      TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCAT      TYPE LVC_T_FCAT,
      WA_FCAT      TYPE LVC_S_FCAT,
      WA_LAYOUT    TYPE LVC_S_LAYO,
      WA_VARIANT   TYPE DISVARIANT.

*&---------------------------------------------------------------------*
*&      Form  ALV_DINAMICA
*&---------------------------------------------------------------------*
TYPES: BEGIN OF TY_BLOCO,
        CHECK01(1),
        CHARG01(100) TYPE C,
        CSPEM01 TYPE MCHB-CSPEM,
        SGTXT01 TYPE MSEG-SGTXT,

        CHECK02(1),
        CHARG02(100) TYPE C,
        CSPEM02 TYPE MCHB-CSPEM,
        SGTXT02 TYPE MSEG-SGTXT,

        CHECK03(1),
        CHARG03(100) TYPE C,
        CSPEM03 TYPE MCHB-CSPEM,
        SGTXT03 TYPE MSEG-SGTXT,

        CHECK04(1),
        CHARG04(100) TYPE C,
        CSPEM04 TYPE MCHB-CSPEM,
        SGTXT04 TYPE MSEG-SGTXT,

       END OF TY_BLOCO.

*&---------------------------------------------------------------------*
*&      Table Internal
*&---------------------------------------------------------------------*
DATA: IT_BLOCO TYPE TABLE OF TY_BLOCO,
      WA_BLOCO TYPE          TY_BLOCO.

DATA: I_MKPF TYPE STANDARD TABLE OF MKPF WITH HEADER LINE,
      I_MSEG TYPE STANDARD TABLE OF MSEG ,
      W_MSEG TYPE                   MSEG .

DATA: CONT  TYPE I,
      INDEX TYPE NUMC2,
      FLG_SEL(1),
      WG_INSTRUCAO TYPE ZSDT0045-INSTRUCAO.
*&---------------------------------------------------------------------*
*&      ALV Grid
*&---------------------------------------------------------------------*
DATA: CL_CONTAINER_DETALHADO TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_GRID_DETALHADO      TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCAT_DETALHADO      TYPE LVC_T_FCAT,

      WA_STABLE              TYPE LVC_S_STBL,
      WA_FCAT_DETALHADO      TYPE LVC_S_FCAT,
      WA_LAYOUT_DETALHADO    TYPE LVC_S_LAYO,
      WA_VARIANT_DETALHADO   TYPE DISVARIANT.

*&--------------------------------------------------------------------&*
*& Parameters
*&--------------------------------------------------------------------&*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
                 P_BUKRS FOR T001-BUKRS OBLIGATORY NO-EXTENSION NO INTERVALS,
                 P_WERKS FOR MCHB-WERKS OBLIGATORY,
                 P_MATNR FOR MCHB-MATNR,
                 P_MATKL FOR MARA-MATKL,
                 P_LGORT FOR MCHB-LGORT,
                 P_KUNNR FOR KNA1-KUNNR,
                 P_SGTXT FOR RBKP-BKTXT,
                 P_BLDAT FOR MKPF-BLDAT.
SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.


*&--------------------------------------------------------------------&*
*& SELECT
*&--------------------------------------------------------------------&*
  PERFORM: SELECIONA_DADOS.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS.


  IF NOT ( P_MATNR-LOW IS INITIAL ) AND NOT ( P_MATKL-LOW IS INITIAL ).
    MESSAGE S888(SABAPDOCU) WITH 'Preencha Material ou Grupo de mercadorias' DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    IF NOT ( P_MATNR-LOW IS INITIAL ).
      PERFORM: SELECIONA_MATNR.
    ELSEIF NOT ( P_MATKL-LOW IS INITIAL ).
      PERFORM: SELECIONA_MATKL.
    ENDIF.

    PERFORM: AGRUPAMENTO_DISPLAY.
  ENDIF.

  CALL SCREEN 0100.
ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_MATNR
*&---------------------------------------------------------------------*
FORM SELECIONA_MATNR .

  SELECT MATNR WERKS LGORT CHARG CSPEM
    FROM MCHB
    INTO TABLE IT_MCHB
  WHERE MATNR IN P_MATNR
    AND WERKS IN P_WERKS
    AND LGORT IN P_LGORT
    AND CSPEM > 0.

  CHECK NOT IT_MCHB[] IS INITIAL.

  SELECT MBLNR MJAHR ZEILE MATNR WERKS LGORT CHARG KUNNR SGTXT
    FROM MSEG
    INTO TABLE IT_MSEG
    FOR ALL ENTRIES IN IT_MCHB
  WHERE MATNR EQ IT_MCHB-MATNR
    AND WERKS EQ IT_MCHB-WERKS
    AND LGORT EQ IT_MCHB-LGORT
    AND CHARG EQ IT_MCHB-CHARG.

  DELETE IT_MSEG WHERE KUNNR EQ ''.

  CHECK NOT IT_MSEG[] IS INITIAL.

  SORT: IT_MSEG BY ZEILE DESCENDING MJAHR DESCENDING MBLNR DESCENDING.

  SELECT MBLNR MJAHR BLDAT BKTXT
    FROM MKPF
    INTO TABLE IT_MKPF
     FOR ALL ENTRIES IN IT_MSEG
      WHERE MBLNR EQ IT_MSEG-MBLNR
        AND MJAHR EQ IT_MSEG-MJAHR
        AND BLDAT IN P_BLDAT.

  DELETE IT_MKPF WHERE BKTXT NOT IN P_SGTXT.

  SELECT KUNNR NAME1
    FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_MSEG
  WHERE KUNNR EQ IT_MSEG-KUNNR.

  DELETE IT_KNA1 WHERE KUNNR NOT IN P_KUNNR.

  SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_MSEG
  WHERE MATNR EQ IT_MSEG-MATNR.


  PERFORM: SAIDA_DETALHADO,
           CLEAR_TABLES.

ENDFORM.                    " SELECIONA_MATNR
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_MATKL
*&---------------------------------------------------------------------*
FORM SELECIONA_MATKL .

  SELECT MATNR MATKL
    FROM MARA
    INTO TABLE IT_MARA
  WHERE MATKL IN P_MATKL.

  CHECK NOT IT_MARA[] IS INITIAL.

  SELECT MATNR WERKS
    FROM MARC
    INTO TABLE IT_MARC
    FOR ALL ENTRIES IN IT_MARA
  WHERE MATNR EQ IT_MARA-MATNR
    AND WERKS IN P_WERKS.

  CHECK NOT IT_MARC[] IS INITIAL.

  SELECT MATNR WERKS LGORT CHARG CSPEM
    FROM MCHB
    INTO TABLE IT_MCHB
  WHERE MATNR IN P_MATNR
    AND WERKS IN P_WERKS
    AND LGORT IN P_LGORT
    AND CSPEM > 0.

  CHECK NOT IT_MCHB[] IS INITIAL.

  SELECT MBLNR MJAHR ZEILE MATNR WERKS LGORT CHARG KUNNR SGTXT
    FROM MSEG
    INTO TABLE IT_MSEG
    FOR ALL ENTRIES IN IT_MCHB
  WHERE MATNR EQ IT_MCHB-MATNR
    AND WERKS EQ IT_MCHB-WERKS
    AND LGORT EQ IT_MCHB-LGORT
    AND CHARG EQ IT_MCHB-CHARG.

  DELETE IT_MSEG WHERE KUNNR EQ ''.

  CHECK NOT IT_MSEG[] IS INITIAL.
  SORT: IT_MSEG BY ZEILE DESCENDING MJAHR DESCENDING MBLNR DESCENDING.

  SELECT MBLNR MJAHR BLDAT BKTXT
    FROM MKPF
    INTO TABLE IT_MKPF
     FOR ALL ENTRIES IN IT_MSEG
      WHERE MBLNR EQ IT_MSEG-MBLNR
        AND MJAHR EQ IT_MSEG-MJAHR
        AND BLDAT IN P_BLDAT.

  DELETE IT_MKPF WHERE BKTXT NOT IN P_SGTXT.

  SELECT KUNNR NAME1
    FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_MSEG
  WHERE KUNNR EQ IT_MSEG-KUNNR.

  DELETE IT_KNA1 WHERE KUNNR NOT IN P_KUNNR.

  SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_MSEG
  WHERE MATNR EQ IT_MSEG-MATNR.

  PERFORM: SAIDA_DETALHADO,
           CLEAR_TABLES.

ENDFORM.                    " SELECIONA_LGORT
*&---------------------------------------------------------------------*
*&      Form  SAIDA_DETALHADO
*&---------------------------------------------------------------------*
FORM SAIDA_DETALHADO .

  DATA:   WA_T001  TYPE T001,
          WA_T001W TYPE T001W.

  LOOP AT IT_MCHB INTO WA_MCHB.

    WA_SAIDA-LGORT = WA_MCHB-LGORT.
    WA_SAIDA-CSPEM = WA_MCHB-CSPEM.
    WA_SAIDA-WERKS = WA_MCHB-WERKS.
    READ TABLE IT_MSEG INTO WA_MSEG WITH KEY MATNR = WA_MCHB-MATNR
                                             WERKS = WA_MCHB-WERKS
                                             LGORT = WA_MCHB-LGORT
                                             CHARG = WA_MCHB-CHARG.

    IF ( SY-SUBRC EQ 0 ).

      READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_MSEG-KUNNR.
      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-KUNNR = WA_KNA1-KUNNR.
        WA_SAIDA-NAME1 = WA_KNA1-NAME1.


        READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_MSEG-MATNR.
        WA_SAIDA-MATNR = WA_MAKT-MATNR.
        WA_SAIDA-MAKTX = WA_MAKT-MAKTX.

        READ TABLE IT_MKPF INTO WA_MKPF WITH KEY MBLNR = WA_MSEG-MBLNR
                                                 MJAHR = WA_MSEG-MJAHR.
        IF SY-SUBRC IS INITIAL.
          WA_SAIDA-SGTXT = WA_MKPF-BKTXT.
          WA_SAIDA-BLDAT = WA_MKPF-BLDAT.

          WA_SAIDA-FARDOS = WA_SAIDA-FARDOS + 1.

          APPEND WA_SAIDA TO IT_SAIDA.

          SELECT SINGLE * FROM T001  INTO WA_T001  WHERE BUKRS EQ P_BUKRS-LOW.
          SELECT SINGLE * FROM T001W INTO WA_T001W WHERE WERKS EQ P_WERKS-LOW.
          WA_DETALHADO-WERKSD  = WA_MCHB-WERKS.
          WA_DETALHADO-KUNNR   = WA_SAIDA-KUNNR.
          WA_DETALHADO-NAME_C  = WA_SAIDA-NAME1.
          WA_DETALHADO-BUTXT   = WA_T001-BUTXT.
          WA_DETALHADO-NAME1   = WA_T001W-NAME1.
          WA_DETALHADO-LGORT   = WA_SAIDA-LGORT.
          WA_DETALHADO-CSPEM   = WA_SAIDA-CSPEM.
          WA_DETALHADO-CHARG   = WA_MSEG-CHARG. "wa_mseg-charg+2(8).

          WA_DETALHADO-BUKRS   = WA_T001-BUKRS.
          WA_DETALHADO-WERKS   = WA_T001W-WERKS.
          WA_DETALHADO-SGTXT   = WA_MSEG-SGTXT. "Instrução
          WA_DETALHADO-NAME_C  = WA_KNA1-NAME1.
          WA_DETALHADO-BKTXT   = WA_MKPF-BKTXT. "Contrato
          WA_DETALHADO-MATNR   = WA_MAKT-MATNR.
          WA_DETALHADO-BLDAT   = WA_MKPF-BLDAT.


          APPEND WA_DETALHADO TO IT_DETALHADO.

        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: WA_SAIDA, WA_MCHB, WA_MSEG, WA_KNA1, WA_MAKT, WA_T001, WA_T001W, WA_DETALHADO, WA_MKPF.


  ENDLOOP.


  CLEAR: WA_T001, WA_T001W.


ENDFORM.                    " SAIDA_DETALHADO
*&---------------------------------------------------------------------*
*&      Form  agrupamento_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM AGRUPAMENTO_DISPLAY .


  DATA: BEGIN OF TL_SALDO OCCURS 0,
         WERKS  TYPE MCHB-WERKS,
         LGORT  TYPE MCHB-LGORT,
         KUNNR  TYPE KNA1-KUNNR,
         SGTXT  TYPE MSEG-SGTXT,
         MATNR  TYPE MSEG-MATNR,
         TOTFAR TYPE SY-TABIX,
         CSPEM  TYPE MCHB-CSPEM,
        END OF TL_SALDO.

  DATA: TOTAL           TYPE MCHB-CSPEM,
        TOTAL_IN        TYPE MCHB-CSPEM,
        TOTAL_FARDOS    TYPE SY-TABIX,
        TOTAL_FARDOS_IN TYPE SY-TABIX.


  FIELD-SYMBOLS: <FS_DISPLAY> TYPE TY_SAIDA.

  IT_SAIDA_AUX[] = IT_SAIDA[].

  LOOP AT IT_SAIDA INTO WA_SAIDA.

    WA_DISPLAY-WERKS  = WA_SAIDA-WERKS.
    WA_DISPLAY-LGORT  = WA_SAIDA-LGORT.
    WA_DISPLAY-KUNNR  = WA_SAIDA-KUNNR.
    WA_DISPLAY-NAME1  = WA_SAIDA-NAME1.
    WA_DISPLAY-SGTXT  = WA_SAIDA-SGTXT.
    WA_DISPLAY-MATNR  = WA_SAIDA-MATNR.
    WA_DISPLAY-MAKTX  = WA_SAIDA-MAKTX.
    WA_DISPLAY-FARDOS = WA_SAIDA-FARDOS.
    WA_DISPLAY-CSPEM  = WA_SAIDA-CSPEM.
    WA_DISPLAY-BLDAT  = WA_SAIDA-BLDAT.

    COLLECT WA_DISPLAY INTO IT_DISPLAY.
    CLEAR: WA_DISPLAY.
  ENDLOOP.

  IF NOT ( IT_DETALHADO[] IS INITIAL ).

    LOOP AT IT_DISPLAY ASSIGNING <FS_DISPLAY>.

      LOOP AT IT_DETALHADO INTO WA_DETALHADO WHERE  WERKSD  EQ <FS_DISPLAY>-WERKS
                                               AND   LGORT  EQ <FS_DISPLAY>-LGORT
                                               AND   KUNNR  EQ <FS_DISPLAY>-KUNNR
                                               AND   BKTXT  EQ <FS_DISPLAY>-SGTXT
                                               AND   MATNR  EQ <FS_DISPLAY>-MATNR
                                               AND   BLDAT  EQ <FS_DISPLAY>-BLDAT.

        TOTAL        = TOTAL        + WA_DETALHADO-CSPEM.
        TOTAL_FARDOS = TOTAL_FARDOS + 1.

        IF NOT ( WA_DETALHADO-SGTXT IS INITIAL ).
          TOTAL_IN         = TOTAL_IN + WA_DETALHADO-CSPEM.
          TOTAL_FARDOS_IN = TOTAL_FARDOS_IN + 1.
        ENDIF.
      ENDLOOP.

      <FS_DISPLAY>-TOTAL_P = TOTAL - TOTAL_IN.
      <FS_DISPLAY>-TOTAL_F = TOTAL_FARDOS - TOTAL_FARDOS_IN.

      CLEAR: WA_DETALHADO, TOTAL, TOTAL_IN, TOTAL_FARDOS, TOTAL_FARDOS_IN.
    ENDLOOP.
    UNASSIGN <FS_DISPLAY>.

  ENDIF.


*  total = total + wa_detalhado-cspem.
*      fardos = fardos + 1.
*
*      IF wa_detalhado-sgtxt IS NOT INITIAL.
*        total_in = total_in + wa_detalhado-cspem.
*        fardos_in = fardos_in + 1.
*      ENDIF.
*
*      wa_cabecalho-butxt  = wa_detalhado-butxt.
*      wa_cabecalho-name1  = wa_detalhado-name1.
*      wa_cabecalho-sgtxt  = wa_detalhado-sgtxt.
*      wa_cabecalho-name_c = wa_detalhado-name_c.
*
*      CLEAR: wa_detalhado.
*
*    ENDLOOP.
*
*
*    CLEAR: wa_dados.
*    APPEND wa_dados TO it_bloco.
*
*    wa_dados-charg01 = 'Total:'.
*    wa_dados-cspem01 = total.
*    wa_dados-charg02 = 'Fardos:'.
*    wa_dados-cspem02 = fardos.
*    APPEND  wa_dados TO it_bloco.
*
*    wa_dados-charg01 = 'Total Instr:'.
*    wa_dados-cspem01 = total_in.
*    wa_dados-charg02 = 'Fardos Instr:'.
*    wa_dados-cspem02 = fardos_in.
*    APPEND  wa_dados TO it_bloco.




*  LOOP AT it_saida_aux INTO wa_saida_aux.
*
*    CLEAR: total, total_fardos.
*
**    LOOP AT it_saida INTO wa_saida WHERE lgort EQ wa_saida_aux-lgort.
*    LOOP AT it_saida INTO wa_saida WHERE sgtxt EQ wa_saida_aux-sgtxt
*                                     AND kunnr EQ wa_saida_aux-kunnr.
*      total        = total + wa_saida-cspem.
*      total_fardos = total_fardos + wa_saida-fardos.
*      CLEAR: wa_saida.
*    ENDLOOP.
*
*    wa_display-werks  =  wa_saida_aux-werks.
*    wa_display-lgort  =  wa_saida_aux-lgort.
*    wa_display-kunnr  =  wa_saida_aux-kunnr.
*    wa_display-name1  =  wa_saida_aux-name1.
*    wa_display-sgtxt  =  wa_saida_aux-sgtxt.
*    wa_display-matnr  =  wa_saida_aux-matnr.
*    wa_display-maktx  =  wa_saida_aux-maktx.
*    wa_display-fardos =  total_fardos.
*    wa_display-cspem  =  total.
*
*    APPEND wa_display TO it_display.
*
**    DELETE it_saida_aux WHERE lgort EQ wa_saida_aux-lgort.
*    DELETE it_saida_aux WHERE sgtxt EQ wa_saida_aux-sgtxt
*                          AND kunnr EQ wa_saida_aux-kunnr
*                          AND werks EQ wa_saida_aux-werks.
*
*    CLEAR: wa_saida_aux, wa_saida, wa_display.
*
*
*  ENDLOOP.
ENDFORM.                    " AGRUPAMENTO_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  CLEAR_TABLES
*&---------------------------------------------------------------------*
FORM CLEAR_TABLES.
  CLEAR: IT_MCHB[], IT_MSEG[], IT_KNA1[], IT_MAKT[].
ENDFORM.                    " CLEAR_TABLES
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.

  IF ( CL_CONTAINER IS INITIAL ).
    PERFORM: CREATE_OBJECT.
  ENDIF.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.
  CASE SY-UCOMM.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  HANDLER
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
    METHODS:
        HANDLE_HOTSPOT_CLICK FOR EVENT  HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

    METHODS:
     ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                     IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .


    METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
                     IMPORTING E_MODIFIED ET_GOOD_CELLS.



    METHODS:  ZM_HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
                                IMPORTING E_OBJECT E_INTERACTIVE.

    METHODS:     ZM_HANDLE_USER_COMMAND FOR EVENT  USER_COMMAND OF CL_GUI_ALV_GRID
                                         IMPORTING E_UCOMM.




ENDCLASS.                    "LCL_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM: HANDLE_HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD ON_DATA_CHANGED.

    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_VALUE TYPE LVC_VALUE.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'CHECK01'.

    ENDLOOP.

  ENDMETHOD.                    "on_data_changed

  METHOD ON_DATA_CHANGED_FINISHED.

**** Método de atualização de dados na Tela
    WA_STABLE-ROW = 'X'.
    CALL METHOD CL_GRID_DETALHADO->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "on_data_changed_finisheD

  METHOD ZM_HANDLE_TOOLBAR.
*   Incluindo Botão ALV
    PERFORM Z_HANDLE_TOOLBAR USING E_OBJECT
                                   E_INTERACTIVE.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD ZM_HANDLE_USER_COMMAND.
*   User Command Botões Incluidos
    PERFORM Z_HANDLE_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM Z_HANDLE_TOOLBAR  USING P_OBJECT      TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                             P_INTERACTIVE TYPE CHAR1 .

  DATA SL_TOOLBAR TYPE STB_BUTTON.


  CLEAR SL_TOOLBAR.
  MOVE: 'SELECT_TODOS'         TO SL_TOOLBAR-FUNCTION ,
         ICON_SELECT_ALL       TO SL_TOOLBAR-ICON     ,
         "TEXT-039              TO SL_TOOLBAR-QUICKINFO,
         "TEXT-039              TO SL_TOOLBAR-TEXT     ,
         SPACE                 TO SL_TOOLBAR-DISABLED .

  APPEND SL_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

  CLEAR SL_TOOLBAR.
  MOVE: 'DESELECT_TODOS'       TO SL_TOOLBAR-FUNCTION ,
         ICON_DESELECT_ALL     TO SL_TOOLBAR-ICON     ,
         "TEXT-040              TO SL_TOOLBAR-QUICKINFO,
         "TEXT-039              TO SL_TOOLBAR-TEXT     ,
         SPACE                 TO SL_TOOLBAR-DISABLED .

  APPEND SL_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
FORM Z_HANDLE_COMMAND  USING P_UCOMM.

  FIELD-SYMBOLS: <FS_BLOCO> TYPE TY_BLOCO.

  CASE P_UCOMM.

    WHEN: 'SELECT_TODOS'.

      LOOP AT IT_BLOCO ASSIGNING <FS_BLOCO>.
        <FS_BLOCO>-CHECK01 = 'X'.
        <FS_BLOCO>-CHECK02 = 'X'.
        <FS_BLOCO>-CHECK03 = 'X'.
        <FS_BLOCO>-CHECK04 = 'X'.
      ENDLOOP.


    WHEN: 'DESELECT_TODOS'.

      LOOP AT IT_BLOCO ASSIGNING <FS_BLOCO>.

        CLEAR:
          <FS_BLOCO>-CHECK01,
          <FS_BLOCO>-CHECK02,
          <FS_BLOCO>-CHECK03,
          <FS_BLOCO>-CHECK04.
      ENDLOOP.

  ENDCASE.

  CALL METHOD CL_GRID_DETALHADO->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.

ENDFORM.                    " Z_HANDLE_COMMAND


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING    P_E_ROW_ID
                                    P_E_COLUMN_ID
                                    P_ES_ROW_NO.

  READ TABLE IT_DISPLAY INTO WA_DISPLAY INDEX P_E_ROW_ID.

  CLEAR: IT_BLOCO[].

  PERFORM: ALV_DINAMICA USING WA_DISPLAY.

  IF NOT ( IT_BLOCO[] IS INITIAL ).
    CALL SCREEN 0200.
  ENDIF.
ENDFORM.                    " HANDLE_HOTSPOT_CLICK

DATA: WA_EVENT     TYPE REF TO LCL_EVENT_HANDLER.

*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
FORM CREATE_OBJECT .


  IF ( CL_CONTAINER IS INITIAL ).
    CREATE OBJECT CL_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_PRINCIPAL'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.


    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT = CL_CONTAINER.

    PERFORM: FCAT.


    IF ( WA_EVENT IS INITIAL ).

      CREATE OBJECT WA_EVENT.
      SET HANDLER: WA_EVENT->HANDLE_HOTSPOT_CLICK FOR CL_GRID.

    ENDIF.

    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT
        IS_VARIANT                    = WA_VARIANT
        I_SAVE                        = 'A'
      CHANGING
        IT_OUTTAB                     = IT_DISPLAY
        IT_FIELDCATALOG               = IT_FCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

  ENDIF.

  CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " CREATE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  FCAT
*&---------------------------------------------------------------------*
FORM FCAT .

  PERFORM CATALOG USING:

    'WERKS'   'Centro'       '06'  '' ' ' '' '' '',
    'LGORT'   'Bloco'        '06'  '' 'X' '' '' '',
    'NAME1'   'Cliente'      '35' ''  ''  '' '' '',
    'SGTXT'   'Contrato'     '20' ''  ''  '' '' '',
    'MAKTX'   'Material'     '35' 'X'  '' '' '' '',
    'FARDOS'  'Fardos'       '07' ''  ''  '' '' '',
    'CSPEM'   'Peso'         '10' ''  ''  '' '' '',
    'BLDAT'   'Data TAKE-UP' '15' ''  ''  '' '' '',
    'TOTAL_P' 'Saldo Peso'   '10' ''  ''  '' '' '',
    'TOTAL_F' 'Saldo Fardos' '10' ''  ''  '' '' ''.

ENDFORM.                    " FCAT
*&---------------------------------------------------------------------*
*&      Form  CATALOG
*&---------------------------------------------------------------------*
FORM CATALOG  USING  VALUE(P_FIELDNAME)
                        VALUE(P_DESC)
                        VALUE(P_TAM)
                        VALUE(P_NO_ZERO)
                        VALUE(P_HOTSPOT)
                        VALUE(P_COR)
                        VALUE(P_JUST)
                        VALUE(P_SUM).
  CLEAR: WA_FCAT.

  WA_FCAT-FIELDNAME = P_FIELDNAME.
  WA_FCAT-SCRTEXT_L = P_DESC.
  WA_FCAT-SCRTEXT_M = P_DESC.
  WA_FCAT-SCRTEXT_S = P_DESC.
  WA_FCAT-OUTPUTLEN = P_TAM.
  WA_FCAT-NO_ZERO   = P_NO_ZERO.
  WA_FCAT-HOTSPOT   = P_HOTSPOT.
  WA_FCAT-EMPHASIZE = P_COR.
  WA_FCAT-JUST      = P_JUST.
  WA_FCAT-DO_SUM    = P_SUM.


  APPEND WA_FCAT TO IT_FCAT.



ENDFORM.                    " CATALOG


*&---------------------------------------------------------------------*
*&      Form  ALV_DINAMICA
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ALV_DINAMICA
*&---------------------------------------------------------------------*

START-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  ALV_DINAMICA
*&---------------------------------------------------------------------*
FORM ALV_DINAMICA  USING    P_WA_DISPLAY TYPE TY_SAIDA.

  DATA: TOTAL     TYPE MCHB-CSPEM,
        FARDOS    TYPE SY-TABIX,
        TOTAL_IN  TYPE MCHB-CSPEM,
        FARDOS_IN TYPE SY-TABIX.

  DATA: WA_DADOS     TYPE TY_BLOCO,
        WA_CABECALHO TYPE TY_DETALHADO.

  CLEAR: INDEX, CONT.

  INDEX = 1.

  CLEAR: TOTAL_IN, TOTAL,FARDOS,FARDOS_IN, WA_CABECALHO, IT_BLOCO[], WA_DETALHADO, IT_FCAT_DETALHADO[].

  IF NOT ( P_WA_DISPLAY-LGORT IS INITIAL ).



    LOOP AT IT_DETALHADO INTO WA_DETALHADO WHERE WERKSD EQ P_WA_DISPLAY-WERKS
                                           AND   LGORT EQ P_WA_DISPLAY-LGORT
                                           AND   KUNNR EQ P_WA_DISPLAY-KUNNR
                                           AND   BKTXT EQ P_WA_DISPLAY-SGTXT
                                           AND   MATNR EQ P_WA_DISPLAY-MATNR
                                           AND   BLDAT EQ P_WA_DISPLAY-BLDAT.

      ADD 1 TO: CONT.

      PERFORM: VERIFICA_INDEX CHANGING CONT
                                       INDEX.

      PERFORM: COLUNAS_DINAMICA CHANGING CONT
                                         INDEX
                                         WA_DETALHADO.


      TOTAL = TOTAL + WA_DETALHADO-CSPEM.
      FARDOS = FARDOS + 1.

      IF WA_DETALHADO-SGTXT IS NOT INITIAL.
        TOTAL_IN = TOTAL_IN + WA_DETALHADO-CSPEM.
        FARDOS_IN = FARDOS_IN + 1.
      ENDIF.

      WA_CABECALHO-BUTXT  = WA_DETALHADO-BUTXT.
      WA_CABECALHO-NAME1  = WA_DETALHADO-NAME1.
      WA_CABECALHO-SGTXT  = WA_DETALHADO-SGTXT.
      WA_CABECALHO-NAME_C = WA_DETALHADO-NAME_C.

      CLEAR: WA_DETALHADO.

    ENDLOOP.


    CLEAR: WA_DADOS.
    APPEND WA_DADOS TO IT_BLOCO.

    WA_DADOS-CHARG01 = 'Total:'.
    WA_DADOS-CSPEM01 = TOTAL.
    WA_DADOS-CHARG02 = 'Fardos:'.
    WA_DADOS-CSPEM02 = FARDOS.
    APPEND  WA_DADOS TO IT_BLOCO.

    WA_DADOS-CHARG01 = 'Total Instr:'.
    WA_DADOS-CSPEM01 = TOTAL_IN.
    WA_DADOS-CHARG02 = 'Fardos Instr:'.
    WA_DADOS-CSPEM02 = FARDOS_IN.
    APPEND  WA_DADOS TO IT_BLOCO.

    CLEAR: WA_DADOS.

    WA_DADOS-CHARG01 = WA_CABECALHO-NAME_C.
    APPEND WA_DADOS TO IT_BLOCO.

    CLEAR: WA_DADOS.
    WA_DADOS-CHARG01 = WA_CABECALHO-NAME1.
    APPEND WA_DADOS TO IT_BLOCO.

    CLEAR: WA_DADOS.
    WA_DADOS-CHARG01 = WA_CABECALHO-SGTXT.
    APPEND WA_DADOS TO IT_BLOCO.


    CLEAR: WA_DADOS, WA_DETALHADO.

  ENDIF.


ENDFORM.                    " ALV_DINAMICA
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_INDEX
*&---------------------------------------------------------------------*
FORM VERIFICA_INDEX  CHANGING P_CONT  TYPE I
                              P_INDEX TYPE NUMC2.

  IF ( P_CONT EQ 36 ).
    ADD 1 TO P_INDEX.
    P_CONT = 1.
  ENDIF.


ENDFORM.                    " VERIFICA_INDEX
*&---------------------------------------------------------------------*
*&      Form  COLUNAS_DINAMICA
*&---------------------------------------------------------------------*
FORM COLUNAS_DINAMICA  CHANGING P_CONT         TYPE I
                                P_INDEX        TYPE NUMC2
                                P_WA_DETALHADO TYPE TY_DETALHADO.


  DATA: LINES    TYPE I,
        WA_BLOCO TYPE TY_BLOCO.

  PERFORM CAMPOS USING 'CHARG'
                        P_INDEX
                        P_WA_DETALHADO-CHARG
                        'C'
                   CHANGING WA_BLOCO.


  PERFORM CAMPOS USING 'CSPEM'
                        P_INDEX
                        P_WA_DETALHADO-CSPEM
                        'Q'
                  CHANGING WA_BLOCO.

  PERFORM CAMPOS USING 'SGTXT'
                        P_INDEX
                        P_WA_DETALHADO-SGTXT
                        'C'
                  CHANGING WA_BLOCO.

  DESCRIBE TABLE IT_BLOCO LINES LINES.

  IF ( LINES <= 34 ).
    APPEND WA_BLOCO TO IT_BLOCO.
    EXIT.
  ENDIF.

  CASE P_INDEX.

    WHEN 2.
      MODIFY IT_BLOCO FROM WA_BLOCO INDEX P_CONT TRANSPORTING  CHARG02 CSPEM02 SGTXT02.
    WHEN 3.
      MODIFY IT_BLOCO FROM WA_BLOCO INDEX P_CONT TRANSPORTING  CHARG03 CSPEM03 SGTXT03.
    WHEN: 4.
      MODIFY IT_BLOCO FROM WA_BLOCO INDEX P_CONT TRANSPORTING  CHARG04 CSPEM04 SGTXT04.

  ENDCASE.




ENDFORM.                    " COLUNAS_DINAMICA
*&---------------------------------------------------------------------*
*&      Form  CAMPOS
*&---------------------------------------------------------------------*
FORM CAMPOS  USING    P_COLUNA   TYPE C
                      P_INDEX    TYPE NUMC2
                      P_VALOR    TYPE ANY
                      P_TIPO     TYPE C
             CHANGING P_BLOCO    TYPE TY_BLOCO.


  DATA: VL_COLUNA TYPE CHAR30.


  FIELD-SYMBOLS: <COLUNA>  TYPE ANY,
                 <COLUNA2> TYPE MCHB-CSPEM.

  CONCATENATE 'P_BLOCO-'
               P_COLUNA
               P_INDEX
  INTO VL_COLUNA.

  CASE P_TIPO.
    WHEN 'C'.
      ASSIGN (VL_COLUNA) TO <COLUNA>.
      CHECK <COLUNA> IS ASSIGNED.
      <COLUNA> = P_VALOR.
      CONDENSE <COLUNA> NO-GAPS.
      UNASSIGN <COLUNA>.
    WHEN 'Q'.
      ASSIGN (VL_COLUNA) TO <COLUNA2>.
      CHECK <COLUNA2> IS ASSIGNED.
      <COLUNA2> = P_VALOR.
      UNASSIGN <COLUNA2>.
  ENDCASE.


ENDFORM.                    " CAMPOS

*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT_DETALHADO
*&---------------------------------------------------------------------*
FORM CREATE_OBJECT_DETALHADO .

  IF ( CL_CONTAINER_DETALHADO IS INITIAL ).

    CLEAR: CL_CONTAINER_DETALHADO, CL_GRID_DETALHADO.

    CREATE OBJECT CL_CONTAINER_DETALHADO
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_DETALHADO'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT CL_GRID_DETALHADO
      EXPORTING
        I_PARENT = CL_CONTAINER_DETALHADO.

    PERFORM: CATALOG_DETALHADO.

    IF ( WA_EVENT IS INITIAL ).
      CREATE OBJECT WA_EVENT.
    ENDIF.

    WA_VARIANT_DETALHADO-REPORT = SY-REPID.
    CALL METHOD CL_GRID_DETALHADO->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT_DETALHADO
        IS_VARIANT                    = WA_VARIANT_DETALHADO
        I_SAVE                        = 'A'
        I_DEFAULT                     = 'X'
      CHANGING
        IT_OUTTAB                     = IT_BLOCO
        IT_FIELDCATALOG               = IT_FCAT_DETALHADO
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    CALL METHOD CL_GRID_DETALHADO->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD CL_GRID_DETALHADO->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER WA_EVENT->ON_DATA_CHANGED            FOR CL_GRID_DETALHADO.
    SET HANDLER WA_EVENT->ON_DATA_CHANGED_FINISHED   FOR CL_GRID_DETALHADO.
    SET HANDLER WA_EVENT->ZM_HANDLE_TOOLBAR          FOR CL_GRID_DETALHADO.
    SET HANDLER WA_EVENT->ZM_HANDLE_USER_COMMAND     FOR CL_GRID_DETALHADO.
  ENDIF.

  CALL METHOD CL_GRID_DETALHADO->REFRESH_TABLE_DISPLAY.

ENDFORM.                    " CREATE_OBJECT_DETALHADO
*&---------------------------------------------------------------------*
*&      Form  CATALOG_DETALHADO
*&---------------------------------------------------------------------*
FORM CATALOG_DETALHADO.

  PERFORM PREENCHE_CATALOG_DETALHADO USING:

    'CHECK01'   'CHK'       '' '' '' '' '' '' 'IT_BLOCO',
    'CHARG01'   'Fardos'    '' '' '' '' '' '' 'IT_BLOCO',
    'CSPEM01'   'Peso'      '' '' '' '' '' '' 'IT_BLOCO',
    'SGTXT01'   'Instrução' '' '' '' '' '' '' 'IT_BLOCO',

    'CHECK02'   'CHK'       '' '' '' '' '' '' 'IT_BLOCO',
    'CHARG02'   'Fardos'    '' '' '' '' '' '' 'IT_BLOCO',
    'CSPEM02'   'Peso'      '' '' '' '' '' '' 'IT_BLOCO',
    'SGTXT02'   'Instrução' '' '' '' '' '' '' 'IT_BLOCO',

    'CHECK03'   'CHK'       '' '' '' '' '' '' 'IT_BLOCO',
    'CHARG03'   'Fardos'    '' '' '' '' '' '' 'IT_BLOCO',
    'CSPEM03'   'Peso'      '' '' '' '' '' '' 'IT_BLOCO',
    'SGTXT03'   'Instrução' '' '' '' '' '' '' 'IT_BLOCO',

    'CHECK04'   'CHK'       '' '' '' '' '' '' 'IT_BLOCO',
    'CHARG04'   'Fardos'    '' '' '' '' '' '' 'IT_BLOCO',
    'CSPEM04'   'Peso'      '' '' '' '' '' '' 'IT_BLOCO',
    'SGTXT04'   'Instrução' '' '' '' '' '' '' 'IT_BLOCO'.


ENDFORM.                    " CATALOG_DETALHADO
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CATALOG_DETALHADO
*&---------------------------------------------------------------------*
FORM PREENCHE_CATALOG_DETALHADO USING   VALUE(P_FIELDNAME)
                                        VALUE(P_DESC)
                                        VALUE(P_TAM)
                                        VALUE(P_NO_ZERO)
                                        VALUE(P_HOTSPOT)
                                        VALUE(P_COR)
                                        VALUE(P_JUST)
                                        VALUE(P_SUM)
                                        VALUE(P_TABLE).
  CLEAR: WA_FCAT_DETALHADO.

  WA_FCAT_DETALHADO-TABNAME   = P_TABLE.
  WA_FCAT_DETALHADO-FIELDNAME = P_FIELDNAME.
  WA_FCAT_DETALHADO-SCRTEXT_L = P_DESC.
  WA_FCAT_DETALHADO-SCRTEXT_M = P_DESC.
  WA_FCAT_DETALHADO-SCRTEXT_S = P_DESC.
  WA_FCAT_DETALHADO-OUTPUTLEN = P_TAM.
  WA_FCAT_DETALHADO-NO_ZERO   = P_NO_ZERO.
  WA_FCAT_DETALHADO-HOTSPOT   = P_HOTSPOT.
  WA_FCAT_DETALHADO-EMPHASIZE = P_COR.
  WA_FCAT_DETALHADO-JUST      = P_JUST.
  WA_FCAT_DETALHADO-DO_SUM    = P_SUM.

  IF P_FIELDNAME+0(5) = 'CHECK'.
    WA_FCAT_DETALHADO-CHECKBOX      = 'X'.
    WA_FCAT_DETALHADO-EDIT          = 'X'.
    WA_FCAT_DETALHADO-KEY           = ''.
    WA_FCAT_DETALHADO-OUTPUTLEN     = 5.
  ENDIF.



  APPEND WA_FCAT_DETALHADO TO IT_FCAT_DETALHADO.

ENDFORM.                    " PREENCHE_CATALOG_DETALHADO
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR  'TB0200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0200 INPUT.

  IF ( SY-DYNNR EQ '0200' ).
    CASE SY-UCOMM.
      WHEN: 'BACK'.
        LEAVE TO SCREEN 0.
      WHEN: 'CANC'.
        LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
      WHEN: 'INSTR'.
        CLEAR FLG_SEL.
        LOOP AT IT_BLOCO INTO WA_BLOCO.
          IF WA_BLOCO-CHECK01 = 'X' OR
             WA_BLOCO-CHECK02 = 'X' OR
             WA_BLOCO-CHECK03 = 'X' OR
             WA_BLOCO-CHECK04 = 'X'.
            FLG_SEL = 'X'.
            EXIT.
          ENDIF.

        ENDLOOP.
        IF FLG_SEL IS INITIAL.
          MESSAGE 'Marque os fardos para a vinculação' TYPE 'I'.
          EXIT.
        ENDIF.
        CALL SCREEN 0300 STARTING AT 050 3
                         ENDING   AT 165 5.
      WHEN: 'EXIT'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*

MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TB0200'.

  PERFORM: CREATE_OBJECT_DETALHADO.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS 'PF0300'.
  SET TITLEBAR 'TB0300'.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  IF ( SY-DYNNR EQ '0300' ).
    CASE SY-UCOMM.
      WHEN: 'SAIR'.
        LEAVE TO SCREEN 0.
      WHEN: 'VINC'.
        CLEAR FLG_SEL.
        DATA WA_ZSDT0045 TYPE ZSDT0045.

        SELECT SINGLE *
          FROM ZSDT0045
          INTO WA_ZSDT0045
          WHERE BUKRS IN P_BUKRS
          AND   WERKS IN P_WERKS
          AND   CONTRATO = WA_DISPLAY-SGTXT
          AND   INSTRUCAO = WG_INSTRUCAO.

        IF SY-SUBRC NE 0.
          MESSAGE 'Instrução não existe para este contrato' TYPE 'I'.
          EXIT.
        ENDIF.
        LOOP AT IT_BLOCO INTO WA_BLOCO.
          IF WA_BLOCO-CHECK01 = 'X'.
            WA_BLOCO-SGTXT01 = WG_INSTRUCAO.
            WA_BLOCO-CHECK01 = ''.
            MODIFY IT_BLOCO FROM WA_BLOCO INDEX SY-TABIX TRANSPORTING CHECK01 SGTXT01.
            PERFORM F_BAPI  USING 1.
          ENDIF.
          "
          IF WA_BLOCO-CHECK02 = 'X'.
            WA_BLOCO-SGTXT02 = WG_INSTRUCAO.
            WA_BLOCO-CHECK02 = ''.
            MODIFY IT_BLOCO FROM WA_BLOCO INDEX SY-TABIX TRANSPORTING CHECK02 SGTXT02.
            PERFORM F_BAPI  USING 2.
          ENDIF.
          "
          IF WA_BLOCO-CHECK03 = 'X'.
            WA_BLOCO-SGTXT03 = WG_INSTRUCAO.
            WA_BLOCO-CHECK03 = ''.
            MODIFY IT_BLOCO FROM WA_BLOCO INDEX SY-TABIX TRANSPORTING CHECK03 SGTXT03.
            PERFORM F_BAPI  USING 3.
          ENDIF.
          "
          IF WA_BLOCO-CHECK04 = 'X'.
            WA_BLOCO-SGTXT04 = WG_INSTRUCAO.
            WA_BLOCO-CHECK04 = ''.
            MODIFY IT_BLOCO FROM WA_BLOCO INDEX SY-TABIX TRANSPORTING CHECK04 SGTXT04.
            PERFORM F_BAPI  USING 4.
          ENDIF.
          "
        ENDLOOP.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_INSTR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_INSTR INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_INSTR OCCURS 0,
        INSTRUCAO TYPE ZSDT0045-INSTRUCAO,
         END OF TL_INSTR.

  SELECT INSTRUCAO
    FROM ZSDT0045
    INTO TABLE TL_INSTR
    WHERE BUKRS IN P_BUKRS
    AND   WERKS IN P_WERKS
    AND   CONTRATO = WA_DISPLAY-SGTXT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'INSTRUCAO'
      DYNPPROG        = SY-REPID                            "'ZFINR018'
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZSDT0045-INSTRUCAO'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_INSTR
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_INSTR  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_WA_BLOCO  text
*----------------------------------------------------------------------*
FORM F_BAPI  USING    VALUE(P_1).
  REFRESH I_MSEG.
  DATA TABIX TYPE SY-TABIX.
  IF P_1 = 1.
    SELECT * FROM MSEG INTO TABLE I_MSEG WHERE CHARG = WA_BLOCO-CHARG01
                                          AND  LGORT = WA_DISPLAY-LGORT
                                          AND  WERKS = WA_DISPLAY-WERKS
                                          AND  BUKRS = P_BUKRS-LOW
                                          AND  BWART = 344.
  ENDIF.

  IF P_1 = 2.
    SELECT * FROM MSEG INTO TABLE I_MSEG WHERE CHARG = WA_BLOCO-CHARG02
                                         AND  LGORT = WA_DISPLAY-LGORT
                                         AND  WERKS = WA_DISPLAY-WERKS
                                         AND  BUKRS = P_BUKRS-LOW
                                         AND  BWART = 344.
  ENDIF.

  IF P_1 = 3.
    SELECT * FROM MSEG INTO TABLE I_MSEG WHERE CHARG = WA_BLOCO-CHARG03
                                         AND   LGORT = WA_DISPLAY-LGORT
                                         AND   WERKS = WA_DISPLAY-WERKS
                                         AND  BUKRS = P_BUKRS-LOW
                                         AND   BWART = 344.
  ENDIF.

  IF P_1 = 4.
    SELECT * FROM MSEG INTO TABLE I_MSEG WHERE CHARG = WA_BLOCO-CHARG04
                                         AND   LGORT = WA_DISPLAY-LGORT
                                         AND   WERKS = WA_DISPLAY-WERKS
                                         AND   BUKRS = P_BUKRS-LOW
                                         AND   BWART = 344.
  ENDIF.

  IF I_MSEG[] IS INITIAL.
    MESSAGE 'Não existe documento de material' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT *
    FROM MKPF
    INTO TABLE I_MKPF
    FOR ALL ENTRIES IN I_MSEG
    WHERE MBLNR = I_MSEG-MBLNR
    AND   MJAHR = I_MSEG-MJAHR
    AND   BLDAT = WA_DISPLAY-BLDAT.

  SORT I_MKPF BY MBLNR MJAHR.
  LOOP AT I_MSEG INTO W_MSEG.
    TABIX = SY-TABIX.
    READ TABLE I_MKPF WITH KEY MBLNR = W_MSEG-MBLNR  MJAHR = W_MSEG-MJAHR BINARY SEARCH.
    IF SY-SUBRC NE 0.
      DELETE I_MSEG WHERE MBLNR = W_MSEG-MBLNR AND  MJAHR = W_MSEG-MJAHR.
      CONTINUE.
    ENDIF.
    W_MSEG-SGTXT = WG_INSTRUCAO.
    MODIFY I_MSEG FROM W_MSEG INDEX TABIX TRANSPORTING SGTXT.
  ENDLOOP.


  CALL FUNCTION 'MB_CHANGE_DOCUMENT'
    TABLES
      ZMKPF = I_MKPF
      ZMSEG = I_MSEG.
ENDFORM.                    " F_BAPI
