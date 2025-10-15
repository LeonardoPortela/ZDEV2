*----------------------------------------------------------------------*
***INCLUDE ZMMR112_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

  SELECT SINGLE *
    FROM ZMMT0078
    INTO WA_ZMMT0078
    WHERE USNAM = SY-UNAME.

  IF SY-SUBRC = 0.
    IF WA_ZMMT0078-MTART = 'ZINF'.
      WG_CADMAT2-HOM_TI = 'X'.
    ELSEIF  WA_ZMMT0078-MTART = 'ZSSO' OR ( WA_ZMMT0078-MTART = 'ZEPI' AND WA_ZMMT0078-WERKS IS INITIAL ). "SSO MATRIZ
      WG_CADMAT2-HOM_SSO = 'X'.
    ELSEIF  WA_ZMMT0078-MTART = 'ZEPI' AND WA_ZMMT0078-WERKS IS NOT INITIAL. "SSO Filial
      WG_CADMAT2-HOM_EPI = 'X'.
    ELSEIF WA_ZMMT0078-MTART = 'SUPR'.
      WG_CADMAT2-HOM_SUP = 'X'.
    ELSE.
      LOOP AT SCREEN.
        SCREEN-INPUT     = 0.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.
    LOOP AT SCREEN.
      IF WA_ZMMT0078-MTART = 'SUPR'.
        IF    SCREEN-NAME = 'WG_CADMAT2-HOM_TI'  OR
              SCREEN-NAME = 'WG_CADMAT2-HOM_SSO' OR
              SCREEN-NAME = 'WG_CADMAT2-HOM_EPI'.
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        MODIFY SCREEN.
      ELSEIF WA_ZMMT0078-MTART = 'ZINF'.
        IF    SCREEN-NAME = 'WG_CADMAT2-HOM_SUP' OR
              SCREEN-NAME = 'WG_CADMAT2-HOM_SSO' OR
              SCREEN-NAME = 'WG_CADMAT2-HOM_EPI'.
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        MODIFY SCREEN.
      ELSEIF WA_ZMMT0078-MTART = 'ZSSO' OR ( WA_ZMMT0078-MTART = 'ZEPI' AND WA_ZMMT0078-WERKS IS INITIAL ).
        IF    SCREEN-NAME = 'WG_CADMAT2-HOM_SUP'  OR
              SCREEN-NAME = 'WG_CADMAT2-HOM_EPI' OR
              SCREEN-NAME = 'WG_CADMAT2-HOM_TI'.
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        MODIFY SCREEN.
      ELSEIF WA_ZMMT0078-MTART = 'ZEPI' AND WA_ZMMT0078-WERKS IS NOT INITIAL.
        IF    SCREEN-NAME = 'WG_CADMAT2-HOM_SUP' OR
              SCREEN-NAME = 'WG_CADMAT2-HOM_SSO' OR
              SCREEN-NAME = 'WG_CADMAT2-HOM_TI'.
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      SCREEN-INPUT     = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  "
  "Objetos
  IF CL_CONTAINER_95 IS INITIAL.
    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        SIDE  = '4'
        RATIO = '80'.
  ENDIF.

  IF NOT CL_GRID IS INITIAL.

    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    PERFORM F_ALV_FIELDCAT.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
        NO_MARGINS = 'X'.


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


    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.


    WA_STABLE-ROW        = 'X'.

    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
        IT_OUTTAB       = IT_SAIDA[].
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIALIZA OUTPUT.
  IF WG_ACAO IS INITIAL.
    CLEAR WG_GER_WF.
    REFRESH: TG_MSG_RET.
    CLEAR WG_MENSAGEM.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN    = '200'
        I_SHOW      = ''
        I_REPID     = SY-REPID
        I_POPUP     = 0
        I_SET_FIELD = 'X_FIELD'
      IMPORTING
        E_MESSAGEM  = WG_MENSAGEM
      TABLES
        IT_MSGS     = TG_MSG_RET.
    CLEAR WG_CADMAT.

    REFRESH: TG_FIELDS.
*    IF WG_CADMAT2-HOM_SUP EQ 'X' OR WG_CADMAT2-HOM_TI  EQ 'X'.
    IF WG_CADMAT2-HOM_EPI  EQ ' ' AND WG_CADMAT2-HOM_SSO EQ ' '.
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                      'EPI'
                                      C_0       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
    ENDIF.

    WG_ACAO  = C_SEARCH.
  ENDIF.

  IF WG_CADMAT-MATKL IS NOT INITIAL.
    SELECT SINGLE *
      FROM T023T
      INTO WA_T023T
      WHERE SPRAS EQ SY-LANGU
      AND   MATKL EQ WG_CADMAT-MATKL.

    WG_CADMAT-WGBEZ = WA_T023T-WGBEZ.

  ENDIF.
  IF WG_CADMAT-MTART IS NOT INITIAL.
    SELECT SINGLE *
      FROM T134T
      INTO WA_T134T
      WHERE SPRAS EQ SY-LANGU
      AND   MTART EQ WG_CADMAT-MTART.

    WG_CADMAT-MTBEZ = WA_T134T-MTBEZ.

  ENDIF.
  IF WG_CADMAT-MBRSH IS NOT INITIAL.
    SELECT SINGLE *
      FROM T137T
      INTO WA_T137T
      WHERE SPRAS EQ SY-LANGU
      AND   MBRSH EQ WG_CADMAT-MBRSH.

    WG_CADMAT-MBBEZ = WA_T137T-MBBEZ.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.

  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF    SCREEN-NAME   EQ TG_FIELDS-CAMPO
        OR  SCREEN-GROUP1 EQ TG_FIELDS-GROUP1.

        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  IF G_CUSTOM_CONT_DESC IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONT_DESC
      EXPORTING
        CONTAINER_NAME = G_DESCBOX.

    IF G_CUSTOM_CONT_DESC IS NOT INITIAL.
      CREATE OBJECT OBG_DESCBOX
        EXPORTING
          PARENT            = G_CUSTOM_CONT_DESC
          WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
          WORDWRAP_POSITION = 72
          MAX_NUMBER_CHARS  = 936.

      CALL METHOD OBG_DESCBOX->SET_TOOLBAR_MODE
        EXPORTING
          TOOLBAR_MODE = '0'.

      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.
    ENDIF.
  ENDIF.
  "
  IF G_CUSTOM_CONT_DESCR IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONT_DESCR
      EXPORTING
        CONTAINER_NAME = G_DESCBOXR.

    IF G_CUSTOM_CONT_DESCR IS NOT INITIAL.
      CREATE OBJECT OBG_DESCBOXR
        EXPORTING
          PARENT            = G_CUSTOM_CONT_DESCR
          WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
          WORDWRAP_POSITION = 72
          MAX_NUMBER_CHARS  = 936.

      CALL METHOD OBG_DESCBOXR->SET_TOOLBAR_MODE
        EXPORTING
          TOOLBAR_MODE = '0'.
*
      CALL METHOD OBG_DESCBOXR->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 0.
    ENDIF.
  ENDIF.

  IF  WG_ACAO  = C_SEARCH.
    PERFORM F_BUSCA_0200.
    WG_ACAO = C_MODIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.

  REFRESH: FCODE.
  IF WG_CADMAT2-HOM_SSO = 'X' OR WG_CADMAT2-HOM_TI = 'X' OR WG_CADMAT2-HOM_EPI = 'X'.
    APPEND 'WORKF'   TO FCODE.
  ELSE.
    APPEND 'HOMOLOG'   TO FCODE.
  ENDIF.
  SET PF-STATUS '0200' EXCLUDING FCODE.
  SET TITLEBAR '0200'.
ENDMODULE.
