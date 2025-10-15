*&---------------------------------------------------------------------*
*& Report  ZLESR0028
*&---------------------------------------------------------------------*
*&TITULO: Aprovação de Cadastro Preço de Frete
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 13.12.2013
*TRANSACAO: ZLES0090
*&---------------------------------------------------------------------*
REPORT  ZLESR0028.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:  BEGIN OF TY_LFA1,
          LIFNR                     TYPE LFA1-LIFNR,
          NAME1                     TYPE LFA1-NAME1,
        END OF TY_LFA1,

        BEGIN OF TY_MAKT,
          MATNR                     TYPE MAKT-MATNR,
          MAKTX                     TYPE MAKT-MAKTX,
        END OF TY_MAKT,

        BEGIN OF TY_TVROT,
          ROUTE    TYPE  TVROT-ROUTE,
          BEZEI    TYPE  TVROT-BEZEI,
        END OF TY_TVROT,

        BEGIN OF TY_SAIDA,
          CHECK(1),
          ACAO(12),
          SHTYP                      TYPE ZLEST0071-SHTYP,
          FORNECEDOR(50),
          ITINERARIO(50),
          MATERIAL(50),
          LZONEA                     TYPE ZLEST0071-LZONEA,
          LZONEZ                     TYPE ZLEST0071-LZONEZ,
          VLR_ANT_FR                 TYPE ZLEST0071-VLR_ANT_FR,
          KBETR                      TYPE ZLEST0071-KBETR,
          KONWA                      TYPE ZLEST0071-KONWA,
          DATAB                      TYPE ZLEST0071-DATAB,
          DATBI                      TYPE ZLEST0071-DATBI,
          ERNAM                      TYPE ZLEST0071-ERNAM,
          DT_EVENTO                  TYPE ZLEST0071-DT_EVENTO,
          HR_EVENTO                  TYPE ZLEST0071-HR_EVENTO,
          KNUMH                      TYPE ZLEST0071-KNUMH,

       END OF TY_SAIDA.
*----------------------------------------------------------------------*
* TABELAS INTERNAS
*----------------------------------------------------------------------*
DATA: IT_ZLEST0071                  TYPE TABLE OF ZLEST0071,
      IT_ZLEST0071_ANT              TYPE TABLE OF ZLEST0071,
      IT_LFA1                       TYPE TABLE OF TY_LFA1,
      IT_MAKT                       TYPE TABLE OF TY_MAKT,
      IT_TVROT                      TYPE TABLE OF TY_TVROT,
      IT_TROLZ                      TYPE TABLE OF TROLZ,
      IT_SAIDA                      TYPE TABLE OF TY_SAIDA.
*----------------------------------------------------------------------*
* TABELAS WORKAREAS
*----------------------------------------------------------------------*

DATA: WA_ZLEST0071                  TYPE ZLEST0071,
      WA_ZLEST0071_ANT              TYPE ZLEST0071,
      WA_ZLEST0071_ANT2             TYPE ZLEST0071,
      WA_LFA1                       TYPE TY_LFA1,
      WA_MAKT                       TYPE TY_MAKT,
      WA_TVROT                      TYPE TY_TVROT,
      WA_SAIDA                      TYPE TY_SAIDA,
      WA_TROLZ                      TYPE TROLZ.


*----------------------------------------------------------------------*
* VARIÁVEIS
*----------------------------------------------------------------------*
DATA: TABIX    TYPE SY-TABIX.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: EDITCONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EDITOR           TYPE REF TO CL_GUI_TEXTEDIT,
      CL_CONTAINER_95  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      CL_CONTAINER_05  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID    TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      WA_STABLE         TYPE LVC_S_STBL,
      WA_AFIELD        TYPE LVC_S_FCAT,
      IT_FIELDCAT      TYPE LVC_T_FCAT,
      W_FIELDCAT       TYPE LVC_S_FCAT,
      I_SORT           TYPE LVC_T_SORT,
      WA_LAYOUT        TYPE LVC_S_LAYO,
      IS_STABLE        TYPE LVC_S_STBL VALUE 'XX',
      WG_REPNAME LIKE SY-REPID,
      WG_X_VARIANT LIKE DISVARIANT,
      WG_EXIT(1) TYPE C,
      WG_SAVE(1) TYPE C,
      WG_VARIANT LIKE DISVARIANT.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

INITIALIZATION.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
          F_SELECIONA_DADOS,
          F_SAIDA,
          F_IMPRIME_DADOS.


END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.

  RANGES: IT_CODE FOR ZLEST0071-TCODE.

  IT_CODE-SIGN   = 'I'.
  IT_CODE-OPTION = 'EQ'.
  IT_CODE-LOW    = 'TK11'.
  IT_CODE-HIGH   = 'TK11'.
  APPEND IT_CODE.

  IT_CODE-SIGN   = 'I'.
  IT_CODE-OPTION = 'EQ'.
  IT_CODE-LOW    = 'TK12'.
  IT_CODE-HIGH   = 'TK12'.
  APPEND IT_CODE.

  SELECT *
    FROM ZLEST0071
    INTO TABLE IT_ZLEST0071
    WHERE US_APROVADOR EQ ''
      AND TCODE IN IT_CODE
      ORDER BY KNUMH DT_EVENTO HR_EVENTO.

  CHECK SY-SUBRC = 0.

  SELECT *
    FROM ZLEST0071
    INTO TABLE IT_ZLEST0071_ANT
    FOR ALL ENTRIES IN IT_ZLEST0071
    WHERE KNUMH = IT_ZLEST0071-KNUMH.

  SELECT LIFNR NAME1
    FROM LFA1
    INTO TABLE IT_LFA1
    FOR ALL ENTRIES IN IT_ZLEST0071
    WHERE LIFNR = IT_ZLEST0071-TDLNR.

  SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_ZLEST0071
    WHERE MATNR EQ IT_ZLEST0071-MATNR
      AND SPRAS EQ SY-LANGU.

  IF WA_ZLEST0071-ROUTE IS NOT INITIAL.
    SELECT ROUTE BEZEI
     FROM TVROT
     INTO TABLE IT_TVROT
     FOR ALL ENTRIES IN IT_ZLEST0071
     WHERE ROUTE = IT_ZLEST0071-ROUTE
     AND  SPRAS = 'P'.
  ELSE.
    SELECT ROUTE BEZEI
         FROM TVROT
         INTO TABLE IT_TVROT
         FOR ALL ENTRIES IN IT_TROLZ
         WHERE SPRAS = 'P'
           AND ROUTE EQ IT_TROLZ-ROUTE.
  ENDIF.

  SELECT *
    FROM TROLZ
    INTO TABLE IT_TROLZ
    FOR ALL ENTRIES IN IT_ZLEST0071
      WHERE ALAND = 'BR'
      AND AZONE EQ IT_ZLEST0071-LZONEA
      AND LZONE EQ IT_ZLEST0071-LZONEZ.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .


  DATA: VDATAHORA1(14),
        VDATAHORA2(14).

  SORT: IT_LFA1           BY LIFNR,
        IT_MAKT           BY MATNR,
        IT_TVROT          BY ROUTE,
        IT_ZLEST0071_ANT  BY KNUMH DT_EVENTO HR_EVENTO.


  LOOP AT IT_ZLEST0071 INTO WA_ZLEST0071.
    CLEAR: WA_TVROT, WA_LFA1, WA_MAKT.

    IF WA_ZLEST0071-TCODE = 'TK11'.
      WA_SAIDA-ACAO = 'Inclusão'.
    ELSE.
      WA_ZLEST0071-TCODE = 'TK12'.
      WA_SAIDA-ACAO = 'Modificação'.
    ENDIF.

    WA_SAIDA-SHTYP                      =  WA_ZLEST0071-SHTYP.

    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_ZLEST0071-TDLNR BINARY SEARCH.
    CONCATENATE WA_ZLEST0071-TDLNR '-' WA_LFA1-NAME1 INTO WA_SAIDA-FORNECEDOR.

    IF WA_ZLEST0071-ROUTE IS NOT INITIAL.
      READ TABLE IT_TVROT INTO WA_TVROT WITH KEY ROUTE = WA_ZLEST0071-ROUTE BINARY SEARCH.
      CONCATENATE WA_ZLEST0071-ROUTE '-' WA_TVROT-BEZEI INTO WA_SAIDA-ITINERARIO.
    ELSE.
   READ TABLE IT_TROLZ INTO WA_TROLZ WITH KEY ROUTE = WA_TVROT-ROUTE.
    CONCATENATE WA_TROLZ-ROUTE '-' WA_TVROT-BEZEI INTO WA_SAIDA-ITINERARIO.

      ENDIF.

    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_ZLEST0071-MATNR BINARY SEARCH.
    CONCATENATE WA_MAKT-MATNR '-' WA_MAKT-MAKTX INTO WA_SAIDA-MATERIAL.

    WA_SAIDA-LZONEA                     = WA_ZLEST0071-LZONEA.
    WA_SAIDA-LZONEZ                     = WA_ZLEST0071-LZONEZ.

    CLEAR WA_ZLEST0071_ANT2.
    LOOP AT IT_ZLEST0071_ANT INTO WA_ZLEST0071_ANT WHERE KNUMH     = WA_ZLEST0071-KNUMH.
      CONCATENATE WA_ZLEST0071-DT_EVENTO     WA_ZLEST0071-HR_EVENTO     INTO VDATAHORA1.
      CONCATENATE WA_ZLEST0071_ANT-DT_EVENTO WA_ZLEST0071_ANT-HR_EVENTO INTO VDATAHORA2.
      IF VDATAHORA2 LT VDATAHORA1.
        MOVE-CORRESPONDING WA_ZLEST0071_ANT TO WA_ZLEST0071_ANT2.
        EXIT.
      ENDIF.
    ENDLOOP.
    "
    WA_SAIDA-VLR_ANT_FR                 = WA_ZLEST0071_ANT2-KBETR.
    WA_SAIDA-KBETR                      = WA_ZLEST0071-KBETR.
    WA_SAIDA-KONWA                      = WA_ZLEST0071-KONWA.
    WA_SAIDA-DATAB                      = WA_ZLEST0071-DATAB.
    WA_SAIDA-DATBI                      = WA_ZLEST0071-DATBI.
    WA_SAIDA-ERNAM                      = WA_ZLEST0071-ERNAM.
    WA_SAIDA-DT_EVENTO                  = WA_ZLEST0071-DT_EVENTO.
    WA_SAIDA-HR_EVENTO                  = WA_ZLEST0071-HR_EVENTO.
    WA_SAIDA-KNUMH                      = WA_ZLEST0071-KNUMH.

    APPEND WA_SAIDA TO IT_SAIDA.
  ENDLOOP.

ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .
  PERFORM F_ALV_FIELDCAT.

  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.
  WA_LAYOUT-GRID_TITLE = ''.
  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT   = 'X'.

  CALL SCREEN 0100.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .
  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CHECK'.
  WA_AFIELD-CHECKBOX      = 'X'.
  WA_AFIELD-SCRTEXT_S = 'Chk'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = 'X'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ACAO'.
  WA_AFIELD-SCRTEXT_S = 'Ação'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SHTYP'.
  WA_AFIELD-SCRTEXT_M = 'Tipo Transporte'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'FORNECEDOR'.
  WA_AFIELD-SCRTEXT_M = 'Fornecedor Serviço'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ITINERARIO'.
  WA_AFIELD-SCRTEXT_M = 'Itinerário'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MATERIAL'.
  WA_AFIELD-SCRTEXT_M = 'Material'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'LZONEA'.
  WA_AFIELD-SCRTEXT_M = 'Zona de Partida'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'LZONEZ'.
  WA_AFIELD-SCRTEXT_M = 'Zona de Chegada'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VLR_ANT_FR'.
  WA_AFIELD-SCRTEXT_M = 'Vlr.Anterior'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'KBETR'.
  WA_AFIELD-SCRTEXT_M = 'Vlr.Novo'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'KONWA'.
  WA_AFIELD-SCRTEXT_M = 'Unid.frete'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DATAB'.
  WA_AFIELD-SCRTEXT_M = 'Dt.Val.In.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DATBI'.
  WA_AFIELD-SCRTEXT_M = 'Dt.Val.Fim.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ERNAM'.
  WA_AFIELD-SCRTEXT_M = 'Usuário'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DT_EVENTO'.
  WA_AFIELD-SCRTEXT_M = 'Dt.Evento'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-SCRTEXT_S = WA_AFIELD-SCRTEXT_M.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.

  SET PF-STATUS 'F_SET_PF' EXCLUDING FCODE.
  SET TITLEBAR  'ZFTITLE'.


  IF CL_CONTAINER_95 IS INITIAL.
    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        SIDE  = '4'
        RATIO = '80'.
  ENDIF.

  IF NOT CL_GRID IS INITIAL.

    PERFORM ZF_ALV_HEADER.
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
*    STYLE  =
*    BACKGROUND_COLOR =
*    BDS_STYLESHEET =
        NO_MARGINS = 'X'.

    PERFORM ZF_ALV_HEADER .


    IF EDITCONTAINER IS INITIAL .
      CREATE OBJECT EDITCONTAINER
        EXPORTING
          CONTAINER_NAME = 'HEADER'.
    ENDIF .

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.


    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT = CL_CONTAINER_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    WG_SAVE = 'X'.
    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    WA_STABLE-ROW        = 'X'.
    WG_X_VARIANT-REPORT  = SY-REPID.
    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = WG_X_VARIANT
        IS_LAYOUT       = WA_LAYOUT
        I_SAVE          = WG_SAVE
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
        IT_SORT         = I_SORT[]
        IT_OUTTAB       = IT_SAIDA[].


*    CREATE OBJECT EVENT_RECEIVER.
*    SET HANDLER EVENT_RECEIVER->CATCH_HOTSPOT           FOR CL_GRID.

  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
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

  WL_TEXT = 'Lista para Aprovação de Preço de Fretes'.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT
      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH IT_SAIDA.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'APROV'.
      LOOP AT IT_SAIDA INTO WA_SAIDA WHERE CHECK = 'X'.
        TABIX = SY-TABIX.
        UPDATE ZLEST0071 SET  US_APROVADOR = SY-UNAME
                              DT_APROVACAO = SY-DATUM
                              HR_APROVACAO = SY-UZEIT
        WHERE KNUMH     = WA_SAIDA-KNUMH
        AND   DT_EVENTO = WA_SAIDA-DT_EVENTO
        AND   HR_EVENTO = WA_SAIDA-HR_EVENTO.

        IF SY-SUBRC = 0.
          DELETE IT_SAIDA INDEX TABIX.
        ENDIF.
      ENDLOOP.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
