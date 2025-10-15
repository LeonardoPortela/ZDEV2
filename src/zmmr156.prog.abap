*&---------------------------------------------------------------------*
*& Report  ZMMR156
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR156.

TABLES ZMMT0122.

TYPES: BEGIN OF  TY_SAIDA,
         WERKS         TYPE ZMMT0122-WERKS,
         LGORT         TYPE ZMMT0122-LGORT,
         TP_MOV_SD     TYPE ZMMT0122-TP_MOV_SD,
         TP_MOV_ET     TYPE ZMMT0122-TP_MOV_ET,
         TP_MOV_MAT    TYPE ZMMT0122-TP_MOV_MAT,
* Início - Sara - CS2020000884  - Agosto/2020
         TP_MOV_QB     TYPE ZMMT0122-TP_MOV_QB,
* Fim - Sara - CS2020000884  - Agosto/2020
         QTE_DIAS_PROC TYPE ZMMT0122-QTE_DIAS_PROC,
         PROC_JOB      TYPE ZMMT0122-PROC_JOB,
         TXT           TYPE ZMMT0122-TXT,
         USNAM         TYPE ZMMT0122-USNAM,
         ZDT_ATUAL     TYPE ZMMT0122-ZDT_ATUAL,
         ZHR_ATUAL     TYPE ZMMT0122-ZHR_ATUAL,
         OBS(4)        TYPE C,
         CELLTAB       TYPE LVC_T_STYL,
       END OF TY_SAIDA.

DATA: IT_SAIDA TYPE TABLE OF TY_SAIDA,
      WA_SAIDA TYPE TY_SAIDA.

DATA: IT_TEXTO TYPE STANDARD TABLE OF TLINE,
      WA_TEXTO TYPE TLINE,
      TL_TEXTO TYPE CATSXT_LONGTEXT_ITAB,
      WL_TEXTO TYPE LINE OF CATSXT_LONGTEXT_ITAB,
      WL_NAME  TYPE THEAD-TDNAME.

"DATA: TL_TLINES LIKE TLINE OCCURS 0 WITH HEADER LINE.
DATA: TL_TLINES TYPE TABLE OF TLINE,
      WL_TLINES TYPE TLINE.

DATA: G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_GRID             TYPE REF TO CL_GUI_ALV_GRID,
      IT_FIELDCAT        TYPE LVC_T_FCAT,
      WA_FIELDCAT        TYPE LVC_S_FCAT,
      TL_FUNCTION        TYPE UI_FUNCTIONS,
      WL_FUNCTION        LIKE TL_FUNCTION  WITH HEADER LINE,
      WA_LAYOUT          TYPE LVC_S_LAYO,
      WA_VARIANT         TYPE DISVARIANT,
      WA_ESTILO          TYPE LVC_T_STYL,
      WA_STABLE          TYPE LVC_S_STBL VALUE 'XX'.

START-OF-SELECTION.


  PERFORM  Z_SEACH.

  CALL SCREEN 0100.


CLASS LCL_HANDER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING
          E_ONF4 E_ONF4_AFTER E_ONF4_BEFORE E_UCOMM ER_DATA_CHANGED SENDER,

      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS SENDER,

      ON_BUTTON FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
        IMPORTING ES_COL_ID ES_ROW_NO  SENDER.
ENDCLASS.

CLASS LCL_HANDER IMPLEMENTATION.

  METHOD ON_DATA_CHANGED.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(WA_GOOD)
      WHERE FIELDNAME EQ 'PROC_JOB'.

      LOOP AT IT_SAIDA INTO WA_SAIDA.

        CHECK WA_GOOD-ROW_ID EQ SY-TABIX.

        CASE WA_GOOD-FIELDNAME.
          WHEN 'PROC_JOB'.
            WA_SAIDA-PROC_JOB = ABAP_TRUE.
            MODIFY IT_SAIDA FROM WA_SAIDA INDEX WA_GOOD-ROW_ID.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD ON_DATA_CHANGED_FINISHED.
  ENDMETHOD.

  METHOD ON_BUTTON.

    DATA: WL_HEADER TYPE THEAD,
          WL_TEXTO  LIKE LINE OF TL_TEXTO.

    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX ES_ROW_NO-ROW_ID.

    CASE ES_COL_ID.
      WHEN 'OBS'.
        REFRESH: IT_TEXTO, TL_TEXTO.
        CLEAR: WL_TEXTO, WL_NAME.

        IF WA_SAIDA-TXT IS NOT INITIAL.

          WA_TEXTO-TDLINE =  WA_SAIDA-TXT.

          MOVE: WA_TEXTO-TDLINE TO WL_TEXTO.
          APPEND WA_TEXTO TO  TL_TEXTO.
        ENDIF.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            IM_TITLE     = 'Observação'
            "IM_DISPLAY_MODE = 'X'
            IM_START_ROW = 30              "" Quantidade de Linhas
          CHANGING
            CH_TEXT      = TL_TEXTO.

        IF SY-UCOMM EQ 'CX_CONT'.

          IF TL_TEXTO[] IS NOT INITIAL.

            LOOP AT TL_TEXTO INTO WL_TEXTO.
              MOVE: '*' TO WL_TLINES-TDFORMAT,
               WL_TEXTO TO WL_TLINES-TDLINE.
              APPEND WL_TLINES TO TL_TLINES.
              CLEAR WL_TLINES.

              WA_SAIDA-TXT =   WL_TEXTO.
            ENDLOOP.

            WA_SAIDA-OBS = '@1E@'.
            MODIFY IT_SAIDA FROM WA_SAIDA INDEX ES_ROW_NO-ROW_ID TRANSPORTING OBS TXT.
          ELSE.
            WA_SAIDA-TXT = ''.
            WA_SAIDA-OBS = '@1F@'.
            MODIFY IT_SAIDA FROM WA_SAIDA INDEX ES_ROW_NO-ROW_ID TRANSPORTING OBS TXT.

          ENDIF.
        ENDIF.
        CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = WA_STABLE ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.

FORM Z_SEACH.

  REFRESH IT_SAIDA.

  SELECT *
    FROM ZMMT0122 INTO TABLE @DATA(IT_ZMMT0122).

  LOOP AT IT_ZMMT0122 INTO DATA(WA_ZMMT0122).

    WA_SAIDA-WERKS         = WA_ZMMT0122-WERKS.
    WA_SAIDA-LGORT         = WA_ZMMT0122-LGORT.
    WA_SAIDA-TP_MOV_SD     = WA_ZMMT0122-TP_MOV_SD.
    WA_SAIDA-TP_MOV_ET     = WA_ZMMT0122-TP_MOV_ET.
    WA_SAIDA-TP_MOV_MAT    = WA_ZMMT0122-TP_MOV_MAT.
* Início - Sara - CS2020000884  - Agosto/2020
    WA_SAIDA-TP_MOV_QB     = WA_ZMMT0122-TP_MOV_QB.
* Fim - Sara - CS2020000884  - Agosto/2020
    WA_SAIDA-QTE_DIAS_PROC = WA_ZMMT0122-QTE_DIAS_PROC.
    WA_SAIDA-PROC_JOB      = WA_ZMMT0122-PROC_JOB.
    WA_SAIDA-USNAM         = WA_ZMMT0122-USNAM.
    WA_SAIDA-ZDT_ATUAL     = WA_ZMMT0122-ZDT_ATUAL.
    WA_SAIDA-ZHR_ATUAL     = WA_ZMMT0122-ZHR_ATUAL.
    WA_SAIDA-TXT           = WA_ZMMT0122-TXT.

    IF WA_SAIDA-TXT IS INITIAL.
      WA_SAIDA-OBS    = '@1F@'.
    ELSE.
      WA_SAIDA-OBS    = '@1E@'.
    ENDIF.

    FREE WA_SAIDA-CELLTAB.
    WA_ESTILO =  VALUE #(  ( FIELDNAME = 'WERKS'           STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                           ( FIELDNAME = 'LGORT'           STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                           ( FIELDNAME = 'TP_MOV_SD'       STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                           ( FIELDNAME = 'TP_MOV_ET'       STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                           ( FIELDNAME = 'TP_MOV_MAT'      STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
* Início - Sara - CS2020000884  - Agosto/2020
                           ( FIELDNAME = 'TP_MOV_QB'       STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
* Fim - Sara - CS2020000884  - Agosto/2020
                           ( FIELDNAME = 'QTE_DIAS_PROC'   STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                           ( FIELDNAME = 'PROC_JOB'        STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )   ).
    INSERT LINES OF WA_ESTILO INTO TABLE WA_SAIDA-CELLTAB.
    APPEND WA_SAIDA TO IT_SAIDA.

    CLEAR: WA_SAIDA, WL_NAME.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  PERFORM Z_ALV.

  IF G_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = 'CONTAINER'.

    IF G_GRID IS INITIAL AND G_CUSTOM_CONTAINER IS NOT INITIAL.

      CREATE OBJECT G_GRID
        EXPORTING
          I_PARENT = G_CUSTOM_CONTAINER.
    ENDIF.

    WA_LAYOUT-STYLEFNAME = 'CELLTAB'.

    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_SAVE          = 'X'
        IS_LAYOUT       = WA_LAYOUT
      CHANGING
        IT_OUTTAB       = IT_SAIDA
        IT_FIELDCATALOG = IT_FIELDCAT.

    SET HANDLER: LCL_HANDER=>ON_DATA_CHANGED FOR G_GRID,
                 LCL_HANDER=>ON_BUTTON FOR G_GRID.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.
  ELSE.
    CALL METHOD G_GRID->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = IT_FIELDCAT.

    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = WA_STABLE ).
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM Z_SAVE.
    WHEN '&EDIT'.
      CALL METHOD G_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = DATA(IT_SELECTED).

      IF IT_SELECTED IS INITIAL.
        MESSAGE 'Favor selecionar uma linha!' TYPE 'I'.
        EXIT.
      ELSE.

        LOOP AT IT_SELECTED INTO DATA(WA_SELECTED).

          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WA_SELECTED-INDEX.
          FREE WA_SAIDA-CELLTAB.

          WA_ESTILO = VALUE #( ( FIELDNAME = 'TP_MOV_SD'      STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                               ( FIELDNAME = 'TP_MOV_ET'      STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                               ( FIELDNAME = 'TP_MOV_MAT'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
* Início - Sara - CS2020000884  - Agosto/2020
                               ( FIELDNAME = 'TP_MOV_QB'      STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
* Fim - Sara - CS2020000884  - Agosto/2020
                               ( FIELDNAME = 'QTE_DIAS_PROC'  STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )  ).
          INSERT LINES OF WA_ESTILO INTO TABLE WA_SAIDA-CELLTAB.

          MODIFY IT_SAIDA FROM WA_SAIDA INDEX WA_SELECTED-INDEX.
        ENDLOOP.
      ENDIF.

    WHEN '&INS'.

      CLEAR WA_SAIDA.

      WA_SAIDA-OBS    = '@1F@'.
      FREE WA_SAIDA-CELLTAB.
      WA_ESTILO =  VALUE #(  ( FIELDNAME = 'WERKS'           STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                             ( FIELDNAME = 'LGORT'           STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                             ( FIELDNAME = 'TP_MOV_SD'       STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                             ( FIELDNAME = 'TP_MOV_ET'       STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                             ( FIELDNAME = 'TP_MOV_MAT'      STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
* Início - Sara - CS2020000884  - Agosto/2020
                             ( FIELDNAME = 'TP_MOV_QB'       STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
* Fim - Sara - CS2020000884  - Agosto/2020
                             ( FIELDNAME = 'QTE_DIAS_PROC'   STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                             ( FIELDNAME = 'PROC_JOB'        STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )   ).
      INSERT LINES OF WA_ESTILO INTO TABLE WA_SAIDA-CELLTAB.
      APPEND WA_SAIDA TO IT_SAIDA.

    WHEN '&DEL'.

      CALL METHOD G_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECTED.

      IF IT_SELECTED[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'I'.
        EXIT.
      ELSE.
        LOOP AT IT_SELECTED INTO WA_SELECTED.

          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WA_SELECTED-INDEX.

          DELETE FROM ZMMT0122 WHERE WERKS = WA_SAIDA-WERKS
                                 AND LGORT = WA_SAIDA-LGORT.
        ENDLOOP.
      ENDIF.

      PERFORM Z_SEACH.

      CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY( IS_STABLE = WA_STABLE ).
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_ALV .

  IT_FIELDCAT =  VALUE LVC_T_FCAT(
  ( REF_TABLE = 'T001W'  REF_FIELD = 'WERKS'  FIELDNAME = 'WERKS'           COLTEXT =   'Centro'                  OUTPUTLEN = '06'  CHECKBOX = ''   EDIT = ' '   )
  ( REF_TABLE = 'T001L'  REF_FIELD = 'LGORT'  FIELDNAME = 'LGORT'           COLTEXT =   'Depósito Destino'        OUTPUTLEN = '16'  CHECKBOX = ''   EDIT = ' '   )
  ( REF_TABLE = 'T156T'  REF_FIELD = 'BWART'  FIELDNAME = 'TP_MOV_SD'       COLTEXT =   'Tp.Mov.Saída'            OUTPUTLEN = '12'  CHECKBOX = ''   EDIT = ' '   )
  ( REF_TABLE = 'T156T'  REF_FIELD = 'BWART'  FIELDNAME = 'TP_MOV_ET'       COLTEXT =   'Tp.Mov.Entrada'          OUTPUTLEN = '13'  CHECKBOX = ''   EDIT = ' '   )
  ( REF_TABLE = 'T156T'  REF_FIELD = 'BWART'  FIELDNAME = 'TP_MOV_MAT'      COLTEXT =   'Tp.Mov.Troca Material'   OUTPUTLEN = '20'  CHECKBOX = ''   EDIT = ''   )
* Início - Sara - CS2020000884  - Agosto/2020
  ( REF_TABLE = 'T156T'  REF_FIELD = 'BWART'  FIELDNAME = 'TP_MOV_QB'       COLTEXT =   'Tp.Mov.Quebra'           OUTPUTLEN = '20'  CHECKBOX = ''   EDIT = ''   )
* Fim - Sara - CS2020000884  - Agosto/2020
  (                                           FIELDNAME = 'QTE_DIAS_PROC'   COLTEXT =   'Qte Dias Process.'       OUTPUTLEN = '20'  CHECKBOX = ''   EDIT = ''   )
  (                                           FIELDNAME = 'PROC_JOB'        COLTEXT =   'Processar Via Job'       OUTPUTLEN = '16'  CHECKBOX = 'X'  EDIT = ''   )
  (                                           FIELDNAME = 'OBS'             COLTEXT =   'Comentário'              OUTPUTLEN = '09'  CHECKBOX = ''   EDIT = ''  STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON  ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_SAVE.

  DATA: IT_SAVE TYPE TABLE OF ZMMT0122,
        WA_SAVE TYPE ZMMT0122.

  LOOP AT IT_SAIDA INTO WA_SAIDA.

    IF WA_SAIDA-WERKS IS INITIAL.
      MESSAGE 'Campo Centro obrigatório!' TYPE 'E'.
      EXIT.
    ELSE.
      MOVE  WA_SAIDA-WERKS TO WA_SAVE-WERKS.
    ENDIF.

    IF WA_SAIDA-LGORT IS INITIAL.
      MESSAGE 'Campo Depósito Destino obrigatório!' TYPE 'E'.
      EXIT.
    ELSE.
      MOVE  WA_SAIDA-LGORT TO WA_SAVE-LGORT.
    ENDIF.

    IF WA_SAIDA-TP_MOV_SD  IS INITIAL.
      MESSAGE 'Campo Tp.Mov.Saída obrigatório!' TYPE 'E'.
      EXIT.
    ELSE.
      MOVE  WA_SAIDA-TP_MOV_SD  TO WA_SAVE-TP_MOV_SD.
    ENDIF.

    IF WA_SAIDA-TP_MOV_ET  IS INITIAL.
      MESSAGE 'Campo Tp.Mob.Entrada obrigatório!' TYPE 'E'.
      EXIT.
    ELSE.
      MOVE  WA_SAIDA-TP_MOV_ET  TO WA_SAVE-TP_MOV_ET.
    ENDIF.

    MOVE  WA_SAIDA-TP_MOV_MAT  TO WA_SAVE-TP_MOV_MAT.
* Início - Sara - CS2020000884  - Agosto/2020
    MOVE  WA_SAIDA-TP_MOV_QB   TO WA_SAVE-TP_MOV_QB.
* Fim - Sara - CS2020000884  - Agosto/2020
    MOVE  WA_SAIDA-QTE_DIAS_PROC  TO WA_SAVE-QTE_DIAS_PROC.
    MOVE WA_SAIDA-PROC_JOB  TO WA_SAVE-PROC_JOB.


    WA_SAVE-TXT         = WA_SAIDA-TXT.
    WA_SAVE-USNAM       = SY-UNAME.
    WA_SAVE-ZDT_ATUAL   = SY-DATUM.
    WA_SAVE-ZHR_ATUAL   = SY-UZEIT.

    APPEND WA_SAVE TO IT_SAVE.
    CLEAR:  WA_SAVE, WA_SAIDA.
    "ENDIF.
  ENDLOOP.

  IF IT_SAVE IS NOT INITIAL.
    MODIFY ZMMT0122 FROM TABLE IT_SAVE.
    MESSAGE 'Dados gravado com sucesso!' TYPE 'S'.

    LOOP AT IT_SAIDA INTO WA_SAIDA.
      FREE WA_SAIDA-CELLTAB.
      WA_ESTILO =  VALUE #(  ( FIELDNAME = 'WERKS'           STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                             ( FIELDNAME = 'LGORT'           STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                             ( FIELDNAME = 'TP_MOV_SD'       STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                             ( FIELDNAME = 'TP_MOV_ET'       STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                             ( FIELDNAME = 'TP_MOV_MAT'      STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
* Início - Sara - CS2020000884  - Agosto/2020
                             ( FIELDNAME = 'TP_MOV_QB'       STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
* Fim - Sara - CS2020000884  - Agosto/2020
                             ( FIELDNAME = 'QTE_DIAS_PROC'   STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                             ( FIELDNAME = 'PROC_JOB'        STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )   ).
      INSERT LINES OF WA_ESTILO INTO TABLE WA_SAIDA-CELLTAB.
      MODIFY IT_SAIDA FROM WA_SAIDA.
    ENDLOOP.
  ENDIF.

  REFRESH IT_SAVE.
ENDFORM.
