*----------------------------------------------------------------------*
***INCLUDE MZDRE0001_9000 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.

  DATA: XNIVEL TYPE ZNIVEL_DRE.

  IF TL_9000 IS INITIAL.
    TL_9000 = TL_9001.
    PERFORM ATUALIZA_ALV_EST.
    XNIVEL = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
    QT_CHAR_NIVEL = STRLEN( XNIVEL ).
  ENDIF.

  IF TL_9000 EQ TL_9001.
    SET PF-STATUS 'PFEST'.
    SET TITLEBAR 'TLEST'.
  ELSEIF TL_9000 = TL_9003.
    SET PF-STATUS 'PF9003E'.
    SET TITLEBAR 'TL9003' WITH WA_ZGL015_DRE_EST01_ALV-VERSN.
  ENDIF.

ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000_EXIT INPUT.

  IF TL_9000 = TL_9001.
    LEAVE PROGRAM.
  ELSEIF TL_9000 = TL_9003.
    TL_9000 = TL_9001.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_9000_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  CASE OK_CODE_9000.
    WHEN OK_ATUALIZA.
      PERFORM ATUALIZA_ALV_EST.
    WHEN OK_NOVA_EST.
      PERFORM CRIA_EST_DRE.
    WHEN OK_COPI_EST.
      PERFORM COPIAR_EST_DRE.
    WHEN OK_DELE_EST.
      PERFORM APAGAR_EST_DRE.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_DRE_EST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_ALV_DRE_EST OUTPUT.

  CONSTANTS: TABELA_DRE_EST TYPE STRING VALUE 'IT_ZGL015_DRE_EST01_ALV'.

  DATA: TEXT_E001 TYPE C LENGTH 50 VALUE 'Empresa',
        TEXT_E002 TYPE C LENGTH 50 VALUE 'Nome Empresa',
        TEXT_E003 TYPE C LENGTH 50 VALUE 'Estrutura',
        TEXT_E004 TYPE C LENGTH 50 VALUE 'Nome Estrutura'.

  IF PRIM_DRE_EST IS INITIAL.

    CREATE OBJECT DRE_CONTAINER_EST
      EXPORTING
        CONTAINER_NAME = 'ALV'.

    CREATE OBJECT DRE_ALV_EST
      EXPORTING
        I_PARENT = DRE_CONTAINER_EST.

    PERFORM Z_ESTRUTURA_FIELDCAT TABLES DRE_CATALOGO_EST USING:
        TABELA_DRE_EST 'EDITAR' ' '       'X' 01 04 SPACE SPACE SPACE 'X'   SPACE SPACE SPACE,
        TABELA_DRE_EST 'ESTRUT' ' '       'X' 02 04 SPACE SPACE SPACE 'X'   SPACE SPACE SPACE,
        TABELA_DRE_EST 'BUKRS'  TEXT_E001 ' ' 03 05 SPACE SPACE SPACE SPACE SPACE SPACE SPACE,
        TABELA_DRE_EST 'BUTXT'  TEXT_E002 ' ' 04 40 SPACE SPACE SPACE SPACE SPACE SPACE SPACE,
        TABELA_DRE_EST 'VERSN'  TEXT_E003 ' ' 05 05 SPACE SPACE SPACE SPACE SPACE SPACE SPACE,
        TABELA_DRE_EST 'VSTXT'  TEXT_E004 ' ' 06 60 SPACE SPACE SPACE SPACE SPACE SPACE SPACE.

    CLEAR: DRE_GS_LAYOUT.
    DRE_GS_LAYOUT-ZEBRA         = C_X.
*    DRE_GS_LAYOUT-CWIDTH_OPT    = C_X.
    DRE_GS_LAYOUT-SEL_MODE      = 'A'..

    CALL METHOD DRE_ALV_EST->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_DEFAULT       = SPACE
        IS_LAYOUT       = DRE_GS_LAYOUT
      CHANGING
        IT_FIELDCATALOG = DRE_CATALOGO_EST
        IT_OUTTAB       = IT_ZGL015_DRE_EST01_ALV[].

*   Create Object for Event Handler
    CREATE OBJECT DRE_EVENT_DRE_EST.
    SET HANDLER DRE_EVENT_DRE_EST->HANDLE_HOTSPOT_DRE_EST FOR DRE_ALV_EST.

    PRIM_DRE_EST = C_X.
  ENDIF.

  CALL METHOD DRE_ALV_EST->REFRESH_TABLE_DISPLAY.

  CALL METHOD DRE_ALV_EST->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_COL_INFO = WA_SCROLL_COL
      IS_ROW_NO   = WA_SCROLL_ROW.

ENDMODULE.                 " CRIA_ALV_DRE_EST  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_ALV_EST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA_ALV_EST .

  DATA: IT_T001 TYPE TABLE OF T001 WITH HEADER LINE.

  CLEAR: IT_ZGL015_DRE_EST01[],
         IT_ZGL015_DRE_EST01_ALV[].

  SELECT * INTO TABLE IT_ZGL015_DRE_EST01
    FROM ZGL015_DRE_EST01.

  SELECT * INTO TABLE IT_T001
    FROM T001.

  LOOP AT IT_ZGL015_DRE_EST01.
    CLEAR: IT_ZGL015_DRE_EST01_ALV.
    MOVE-CORRESPONDING IT_ZGL015_DRE_EST01 TO IT_ZGL015_DRE_EST01_ALV.
    READ TABLE IT_T001 WITH KEY BUKRS = IT_ZGL015_DRE_EST01-BUKRS.
    IT_ZGL015_DRE_EST01_ALV-BUTXT  = IT_T001-BUTXT.
    IT_ZGL015_DRE_EST01_ALV-EDITAR = ICON_CHANGE.
    IT_ZGL015_DRE_EST01_ALV-ESTRUT = ICON_DISPLAY_TREE.
    APPEND IT_ZGL015_DRE_EST01_ALV.
  ENDLOOP.

ENDFORM.                    " ATUALIZA_ALV_EST

*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.

  SET PF-STATUS 'PF9003'.
  SET TITLEBAR 'TL9002'.

  IF VG_EDITAR IS NOT INITIAL.
    LOOP AT SCREEN.
      IF ( SCREEN-NAME EQ 'ZGL015_DRE_EST01-BUKRS' ) OR ( SCREEN-NAME EQ 'ZGL015_DRE_EST01-VERSN' ).
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CLEAR: VG_BUKRS_TXT.

  IF NOT ZGL015_DRE_EST01-BUKRS IS INITIAL.
    SELECT SINGLE BUTXT INTO VG_BUKRS_TXT
      FROM T001
     WHERE BUKRS EQ ZGL015_DRE_EST01-BUKRS.
  ENDIF.

ENDMODULE.                 " STATUS_9002  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_EST_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CRIA_EST_DRE .

  CLEAR: VG_EDITAR, ZGL015_DRE_EST01.

  CALL SCREEN 9002 STARTING AT 10 10.

ENDFORM.                    " CRIA_EST_DRE

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_9002_EXIT  INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.

  IF OK_CODE_9002 EQ C_CONF.

    SELECT SINGLE * INTO WA_ZGL015_DRE_EST01
      FROM ZGL015_DRE_EST01
     WHERE BUKRS EQ ZGL015_DRE_EST01-BUKRS
       AND VERSN EQ ZGL015_DRE_EST01-VERSN.

    IF ( SY-SUBRC IS INITIAL ) AND ( VG_EDITAR IS INITIAL ).
      MESSAGE 'Empresa e Estrutura já cadastrada!' TYPE 'S'.
    ELSE.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF WA_ZGL015_DRE_EST01
      FROM ZGL001_DRE_EST
     WHERE BUKRS EQ ZGL015_DRE_EST01-BUKRS
       AND VERSN EQ ZGL015_DRE_EST01-VERSN.

      IF ( SY-SUBRC IS INITIAL ).
        MESSAGE 'Empresa e Estrutura já cadastrada em modelo antigo!' TYPE 'S'.
      ELSE.
        MODIFY ZGL015_DRE_EST01.
        PERFORM ATUALIZA_ALV_EST.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_9002  INPUT

*&---------------------------------------------------------------------*
*&      Module  Z_NOME_EMPRESA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_NOME_EMPRESA INPUT.

  IF NOT ZGL015_DRE_EST01-BUKRS IS INITIAL.
    SELECT SINGLE BUTXT INTO VG_BUKRS_TXT
      FROM T001
     WHERE BUKRS EQ ZGL015_DRE_EST01-BUKRS.
  ENDIF.

ENDMODULE.                 " Z_NOME_EMPRESA  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9003 INPUT.

  TYPES: BEGIN OF TY_NIVEL.
  TYPES:   SIGN       TYPE CHAR01,
           OPTION(02) TYPE C,
           LOW        TYPE ZNIVEL_DRE,
           HIGH       TYPE ZNIVEL_DRE.
  TYPES: END OF TY_NIVEL.

  DATA: NODE     TYPE TV_NODEKEY,
        VG_NIVEL TYPE ZNIVEL_DRE,
        WA_EST02 TYPE ZGL015_DRE_EST02,
        NX       TYPE I,
        NV00(02) TYPE C,
        FTFILTRO TYPE TABLE OF TY_NIVEL WITH HEADER LINE,
        ANSWER   TYPE C LENGTH 1.

  CLEAR: NODE, VG_NIVEL, WA_DRE_EST02.
  CLEAR: LS_SAKNR_2, LS_SAKNR_3, LS_SAKNR_4, LS_SAKNR_5,
         LS_KOSAR_2, LS_KOSAR_3, LS_KOSAR_4, LS_KOSAR_5,
         LS_PRCTR_2, LS_PRCTR_3, LS_PRCTR_4, LS_PRCTR_5,
         LS_MATKL_2, LS_MATKL_3, LS_MATKL_4, LS_MATKL_5.


  CASE OK_CODE_9000.

    WHEN OK_AGP_NIVEL.

      CALL METHOD G_TREE->GET_SELECTED_NODE
        IMPORTING
          NODE_KEY                   = NODE
        EXCEPTIONS
          FAILED                     = 1
          SINGLE_NODE_SELECTION_ONLY = 2
          CNTL_SYSTEM_ERROR          = 3
          OTHERS                     = 4.

      IF ( SY-SUBRC IS INITIAL ) AND ( NOT NODE IS INITIAL ).

        IF ( NODE(2) EQ 'CR' ) OR ( NODE(2) EQ 'CC' ) OR ( NODE(2) EQ 'CL' ) OR ( NODE(2) EQ 'GM' ) OR ( NODE(2) EQ 'AG' ).
          MESSAGE 'Selecione um Nível para Inserção de Agrupamento de Nível!' TYPE 'S'.
          EXIT.
        ELSE.

          WHILE NODE IS NOT INITIAL.
            IF NOT VG_NIVEL IS INITIAL.
              CONCATENATE VG_NIVEL '.' INTO VG_NIVEL.
            ENDIF.
            CONCATENATE VG_NIVEL NODE(2) INTO VG_NIVEL.
            NX = STRLEN( NODE ).
            NX = NX - 2.
            IF NX EQ 0.
              CLEAR NODE.
            ELSE.
              NODE = NODE+2(NX).
            ENDIF.
          ENDWHILE.

          SELECT SINGLE * INTO ZGL015_DRE_EST02
            FROM ZGL015_DRE_EST02
           WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
             AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
             AND NIVEL EQ VG_NIVEL.

          IF NOT ZGL015_DRE_EST02-NIVEL_TOTAL IS INITIAL.
            MOVE-CORRESPONDING ZGL015_DRE_EST02 TO WA_ZGL015_DRE_EST02.
            CLEAR: WA_ZGL015_DRE_EST02-NIVEL,
                   WA_ZGL015_DRE_EST02-NIVEL_ANT,
                   WA_ZGL015_DRE_EST02-NIVEL_TOTAL.
            CALL SCREEN 9010 STARTING AT 10 10.
          ELSE.
            MESSAGE 'Selecione um Nível Totalizador para Inserção de Nível Agrupados!' TYPE 'S'.
          ENDIF.

        ENDIF.

      ELSE.
        MESSAGE 'Selecione um Nível para Inserção de Agrupamento de Nível!' TYPE 'S'.
      ENDIF.

    WHEN OK_GRP_MERCA.

      CALL METHOD G_TREE->GET_SELECTED_NODE
        IMPORTING
          NODE_KEY                   = NODE
        EXCEPTIONS
          FAILED                     = 1
          SINGLE_NODE_SELECTION_ONLY = 2
          CNTL_SYSTEM_ERROR          = 3
          OTHERS                     = 4.

      IF ( SY-SUBRC IS INITIAL ) AND ( NOT NODE IS INITIAL ).

        IF ( NODE(2) NE 'CR' ).
          MESSAGE 'Selecione uma Conta Razão!' TYPE 'S'.
          EXIT.
        ENDIF.
        CLEAR ZGL015_DRE_EST06.
        READ TABLE IT_ZGL015_DRE_EST03_ID INTO WA_ZGL015_DRE_EST03_ID WITH KEY ID = NODE.
        READ TABLE IT_ZGL015_DRE_EST02    WITH KEY NIVEL = WA_ZGL015_DRE_EST03_ID-NIVEL.

        IF NOT IT_ZGL015_DRE_EST02-NIVEL_TOTAL IS INITIAL.
          MESSAGE 'Não permitido níveis em nível de totalização!' TYPE 'S'.
        ELSE.
          MOVE-CORRESPONDING WA_ZGL015_DRE_EST03_ID TO ZGL015_DRE_EST06.
          CALL SCREEN 9009 STARTING AT 10 10.
        ENDIF.

      ELSE.
        MESSAGE 'Selecione uma Conta Razão para Inserção de Grupo de Mercadoria!' TYPE 'S'.
      ENDIF.

    WHEN OK_CTA_LUCRO.

      CALL METHOD G_TREE->GET_SELECTED_NODE
        IMPORTING
          NODE_KEY                   = NODE
        EXCEPTIONS
          FAILED                     = 1
          SINGLE_NODE_SELECTION_ONLY = 2
          CNTL_SYSTEM_ERROR          = 3
          OTHERS                     = 4.

      IF ( SY-SUBRC IS INITIAL ) AND ( NOT NODE IS INITIAL ).

        IF ( NODE(2) NE 'CR' ).
          MESSAGE 'Selecione uma Conta Razão!' TYPE 'S'.
          EXIT.
        ENDIF.
        CLEAR ZGL015_DRE_EST05.
        READ TABLE IT_ZGL015_DRE_EST03_ID INTO WA_ZGL015_DRE_EST03_ID WITH KEY ID = NODE.
        READ TABLE IT_ZGL015_DRE_EST02    WITH KEY NIVEL = WA_ZGL015_DRE_EST03_ID-NIVEL.

        IF NOT IT_ZGL015_DRE_EST02-NIVEL_TOTAL IS INITIAL.
          MESSAGE 'Não permitido níveis em nível de totalização!' TYPE 'S'.
        ELSE.
          MOVE-CORRESPONDING WA_ZGL015_DRE_EST03_ID TO ZGL015_DRE_EST05.

          SELECT SINGLE KOKRS
            INTO ZGL015_DRE_EST05-KOKRS
            FROM TKA02
           WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS.

          IF NOT SY-SUBRC IS INITIAL.
            MESSAGE 'Não encontrado Área de contabilidade de custos!' TYPE 'S'.
          ELSE.
            CALL SCREEN 9008 STARTING AT 10 10.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE 'Selecione uma Conta Razão para Inserção de Centro de Lucro!' TYPE 'S'.
      ENDIF.

    WHEN OK_CTA_CUSTO.

      CALL METHOD G_TREE->GET_SELECTED_NODE
        IMPORTING
          NODE_KEY                   = NODE
        EXCEPTIONS
          FAILED                     = 1
          SINGLE_NODE_SELECTION_ONLY = 2
          CNTL_SYSTEM_ERROR          = 3
          OTHERS                     = 4.

      IF ( SY-SUBRC IS INITIAL ) AND ( NOT NODE IS INITIAL ).

        IF ( NODE(2) NE 'CR' ).
          MESSAGE 'Selecione uma Conta Razão!' TYPE 'S'.
          EXIT.
        ENDIF.
        CLEAR ZGL015_DRE_EST04.
        READ TABLE IT_ZGL015_DRE_EST03_ID INTO WA_ZGL015_DRE_EST03_ID WITH KEY ID = NODE.
        READ TABLE IT_ZGL015_DRE_EST02    WITH KEY NIVEL = WA_ZGL015_DRE_EST03_ID-NIVEL.

        IF NOT IT_ZGL015_DRE_EST02-NIVEL_TOTAL IS INITIAL.
          MESSAGE 'Não permitido níveis em nível de totalização!' TYPE 'S'.
        ELSE.
          MOVE-CORRESPONDING WA_ZGL015_DRE_EST03_ID TO ZGL015_DRE_EST04.
          CALL SCREEN 9007 STARTING AT 10 10.
        ENDIF.

      ELSE.
        MESSAGE 'Selecione uma Conta Razão para Inserção de Centro de Custo!' TYPE 'S'.
      ENDIF.

    WHEN OK_CTA_RAZAO.

      CALL METHOD G_TREE->GET_SELECTED_NODE
        IMPORTING
          NODE_KEY                   = NODE
        EXCEPTIONS
          FAILED                     = 1
          SINGLE_NODE_SELECTION_ONLY = 2
          CNTL_SYSTEM_ERROR          = 3
          OTHERS                     = 4.

      IF ( SY-SUBRC IS INITIAL ) AND ( NOT NODE IS INITIAL ).

        IF ( NODE(2) EQ 'CR' ) OR ( NODE(2) EQ 'CC' ) OR ( NODE(2) EQ 'CL' ) OR ( NODE(2) EQ 'GM' ) OR ( NODE(2) EQ 'AG' ).
          MESSAGE 'Selecione um Nível para Inserção de Conta Razão!' TYPE 'S'.
          EXIT.
        ENDIF.

        WHILE NODE IS NOT INITIAL.
          IF NOT VG_NIVEL IS INITIAL.
            CONCATENATE VG_NIVEL '.' INTO VG_NIVEL.
          ENDIF.
          CONCATENATE VG_NIVEL NODE(2) INTO VG_NIVEL.
          NX = STRLEN( NODE ).
          NX = NX - 2.
          IF NX EQ 0.
            CLEAR NODE.
          ELSE.
            NODE = NODE+2(NX).
          ENDIF.
        ENDWHILE.

        SELECT SINGLE * INTO ZGL015_DRE_EST02
          FROM ZGL015_DRE_EST02
         WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
           AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
           AND NIVEL EQ VG_NIVEL.

        IF NOT ZGL015_DRE_EST02-NIVEL_TOTAL IS INITIAL.
          MESSAGE 'Não permitido níveis em nível de totalização!' TYPE 'S'.
        ELSE.
          CLEAR: ZGL015_DRE_EST03.
          ZGL015_DRE_EST03-KTOPL = '0050'.

          CALL SCREEN 9006 STARTING AT 10 10.
        ENDIF.

      ELSE.
        MESSAGE 'Selecione um Nível para Inserção de Conta Razão!' TYPE 'S'.
      ENDIF.

    WHEN C_EXCLUIR.

      CALL METHOD G_TREE->GET_SELECTED_NODE
        IMPORTING
          NODE_KEY                   = NODE
        EXCEPTIONS
          FAILED                     = 1
          SINGLE_NODE_SELECTION_ONLY = 2
          CNTL_SYSTEM_ERROR          = 3
          OTHERS                     = 4.

      IF ( SY-SUBRC IS INITIAL ) AND ( NOT NODE IS INITIAL ).

        IF NODE(2) EQ 'AG'.

          CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
            EXPORTING
              TITEL     = 'Atenção!'
              TEXTLINE1 = 'O Nível Agrupado será excluido!'
              TEXTLINE2 = 'Deseja realmente excluir?'
            IMPORTING
              ANSWER    = ANSWER.

          IF ANSWER = C_J.
            READ TABLE IT_ZGL015_DRE_EST07_ID WITH KEY ID = NODE.
            IF SY-SUBRC IS INITIAL.

              DELETE FROM ZGL015_DRE_EST07
               WHERE BUKRS      EQ IT_ZGL015_DRE_EST07_ID-BUKRS
                 AND VERSN      EQ IT_ZGL015_DRE_EST07_ID-VERSN
                 AND NIVEL      EQ IT_ZGL015_DRE_EST07_ID-NIVEL
                 AND NIVEL_AGPD EQ IT_ZGL015_DRE_EST07_ID-NIVEL_AGPD.

              MOVE-CORRESPONDING IT_ZGL015_DRE_EST07_ID TO ZGL015_DRE_EST02.
              CALL METHOD G_TREE->DELETE_ALL_NODES.
              PRIM_DRE_NIVEL_RE = C_X.
              PERFORM ATUALIZA_ALV_EST.
              MESSAGE 'Nível Agrupado Excluido!' TYPE 'S'.
            ENDIF.
          ENDIF.

        ELSEIF NODE(2) EQ 'GM'.

          CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
            EXPORTING
              TITEL     = 'Atenção!'
              TEXTLINE1 = 'O objeto de grupo de mercadoria será excluido!'
              TEXTLINE2 = 'Deseja realmente excluir?'
            IMPORTING
              ANSWER    = ANSWER.

          IF ANSWER = C_J.
            READ TABLE IT_ZGL015_DRE_EST06_ID WITH KEY ID = NODE.
            IF SY-SUBRC IS INITIAL.

              DELETE FROM ZGL015_DRE_EST06
               WHERE BUKRS EQ IT_ZGL015_DRE_EST06_ID-BUKRS
                 AND VERSN EQ IT_ZGL015_DRE_EST06_ID-VERSN
                 AND NIVEL EQ IT_ZGL015_DRE_EST06_ID-NIVEL
                 AND KTOPL EQ IT_ZGL015_DRE_EST06_ID-KTOPL
                 AND SAKNR EQ IT_ZGL015_DRE_EST06_ID-SAKNR
                 AND MATKL EQ IT_ZGL015_DRE_EST06_ID-MATKL.

              MOVE-CORRESPONDING IT_ZGL015_DRE_EST06_ID TO ZGL015_DRE_EST02.
              CALL METHOD G_TREE->DELETE_ALL_NODES.
              PRIM_DRE_NIVEL_RE = C_X.
              PERFORM ATUALIZA_ALV_EST.
              MESSAGE 'Grupo de Mercadoria Excluido!' TYPE 'S'.
            ENDIF.
          ENDIF.

        ELSEIF NODE(2) EQ 'CL'.

          CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
            EXPORTING
              TITEL     = 'Atenção!'
              TEXTLINE1 = 'O objeto de centro de lucro será excluido!'
              TEXTLINE2 = 'Deseja realmente excluir?'
            IMPORTING
              ANSWER    = ANSWER.

          IF ANSWER = C_J.
            READ TABLE IT_ZGL015_DRE_EST05_ID WITH KEY ID = NODE.
            IF SY-SUBRC IS INITIAL.

              DELETE FROM ZGL015_DRE_EST05
               WHERE BUKRS EQ IT_ZGL015_DRE_EST05_ID-BUKRS
                 AND VERSN EQ IT_ZGL015_DRE_EST05_ID-VERSN
                 AND NIVEL EQ IT_ZGL015_DRE_EST05_ID-NIVEL
                 AND KTOPL EQ IT_ZGL015_DRE_EST05_ID-KTOPL
                 AND SAKNR EQ IT_ZGL015_DRE_EST05_ID-SAKNR
                 AND KOKRS EQ IT_ZGL015_DRE_EST05_ID-KOKRS
                 AND PRCTR EQ IT_ZGL015_DRE_EST05_ID-PRCTR.

              MOVE-CORRESPONDING IT_ZGL015_DRE_EST05_ID TO ZGL015_DRE_EST02.
              CALL METHOD G_TREE->DELETE_ALL_NODES.
              PRIM_DRE_NIVEL_RE = C_X.
              PERFORM ATUALIZA_ALV_EST.
              MESSAGE 'Centro de Lucro Excluido!' TYPE 'S'.
            ENDIF.
          ENDIF.

        ELSEIF NODE(2) EQ 'CC'.

          CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
            EXPORTING
              TITEL     = 'Atenção!'
              TEXTLINE1 = 'O objeto de centro de custo será excluido!'
              TEXTLINE2 = 'Deseja realmente excluir?'
            IMPORTING
              ANSWER    = ANSWER.

          IF ANSWER = C_J.
            READ TABLE IT_ZGL015_DRE_EST04_ID WITH KEY ID = NODE.
            IF SY-SUBRC IS INITIAL.

              DELETE FROM ZGL015_DRE_EST04
               WHERE BUKRS EQ IT_ZGL015_DRE_EST04_ID-BUKRS
                 AND VERSN EQ IT_ZGL015_DRE_EST04_ID-VERSN
                 AND NIVEL EQ IT_ZGL015_DRE_EST04_ID-NIVEL
                 AND KTOPL EQ IT_ZGL015_DRE_EST04_ID-KTOPL
                 AND SAKNR EQ IT_ZGL015_DRE_EST04_ID-SAKNR
                 AND KOSAR EQ IT_ZGL015_DRE_EST04_ID-KOSAR.

              MOVE-CORRESPONDING IT_ZGL015_DRE_EST04_ID TO ZGL015_DRE_EST02.
              CALL METHOD G_TREE->DELETE_ALL_NODES.
              PRIM_DRE_NIVEL_RE = C_X.
              PERFORM ATUALIZA_ALV_EST.
              MESSAGE 'Centro de Custo Excluido!' TYPE 'S'.
            ENDIF.
          ENDIF.

        ELSEIF NODE(2) EQ 'CR'.

          CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
            EXPORTING
              TITEL     = 'Atenção!'
              TEXTLINE1 = 'Todos os objetos da conta razão serão excluidos!'
              TEXTLINE2 = 'Deseja realmente excluir?'
            IMPORTING
              ANSWER    = ANSWER.

          IF ANSWER = C_J.
            READ TABLE IT_ZGL015_DRE_EST03_ID WITH KEY ID = NODE.
            IF SY-SUBRC IS INITIAL.

              DELETE FROM ZGL015_DRE_EST06
               WHERE BUKRS EQ IT_ZGL015_DRE_EST03_ID-BUKRS
                 AND VERSN EQ IT_ZGL015_DRE_EST03_ID-VERSN
                 AND NIVEL EQ IT_ZGL015_DRE_EST03_ID-NIVEL
                 AND KTOPL EQ IT_ZGL015_DRE_EST03_ID-KTOPL
                 AND SAKNR EQ IT_ZGL015_DRE_EST03_ID-SAKNR.

              DELETE FROM ZGL015_DRE_EST05
               WHERE VERSN EQ IT_ZGL015_DRE_EST03_ID-VERSN
                 AND NIVEL EQ IT_ZGL015_DRE_EST03_ID-NIVEL
                 AND KTOPL EQ IT_ZGL015_DRE_EST03_ID-KTOPL
                 AND SAKNR EQ IT_ZGL015_DRE_EST03_ID-SAKNR.

              DELETE FROM ZGL015_DRE_EST04
               WHERE BUKRS EQ IT_ZGL015_DRE_EST03_ID-BUKRS
                 AND VERSN EQ IT_ZGL015_DRE_EST03_ID-VERSN
                 AND NIVEL EQ IT_ZGL015_DRE_EST03_ID-NIVEL
                 AND KTOPL EQ IT_ZGL015_DRE_EST03_ID-KTOPL
                 AND SAKNR EQ IT_ZGL015_DRE_EST03_ID-SAKNR.

              DELETE FROM ZGL015_DRE_EST03
               WHERE BUKRS EQ IT_ZGL015_DRE_EST03_ID-BUKRS
                 AND VERSN EQ IT_ZGL015_DRE_EST03_ID-VERSN
                 AND NIVEL EQ IT_ZGL015_DRE_EST03_ID-NIVEL
                 AND KTOPL EQ IT_ZGL015_DRE_EST03_ID-KTOPL
                 AND SAKNR EQ IT_ZGL015_DRE_EST03_ID-SAKNR.

              MOVE-CORRESPONDING IT_ZGL015_DRE_EST03_ID TO ZGL015_DRE_EST02.
              CALL METHOD G_TREE->DELETE_ALL_NODES.
              PRIM_DRE_NIVEL_RE = C_X.
              PERFORM ATUALIZA_ALV_EST.
              MESSAGE 'Conta Razão Excluida!' TYPE 'S'.
            ENDIF.
          ENDIF.

        ELSE.

          CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
            EXPORTING
              TITEL     = 'Atenção!'
              TEXTLINE1 = 'Todo o nível da estrutura será excluido!'
              TEXTLINE2 = 'Deseja realmente excluir?'
            IMPORTING
              ANSWER    = ANSWER.

          IF ANSWER = C_J.

            WHILE NODE IS NOT INITIAL.
              IF NOT VG_NIVEL IS INITIAL.
                CONCATENATE VG_NIVEL '.' INTO VG_NIVEL.
              ENDIF.
              CONCATENATE VG_NIVEL NODE(2) INTO VG_NIVEL.
              NX = STRLEN( NODE ).
              NX = NX - 2.
              IF NX EQ 0.
                CLEAR NODE.
              ELSE.
                NODE = NODE+2(NX).
              ENDIF.
            ENDWHILE.

            DELETE FROM ZGL015_DRE_EST07
             WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL EQ VG_NIVEL.

            DELETE FROM ZGL015_DRE_EST06
             WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL EQ VG_NIVEL.

            DELETE FROM ZGL015_DRE_EST05
             WHERE VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL EQ VG_NIVEL.

            DELETE FROM ZGL015_DRE_EST04
             WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL EQ VG_NIVEL.

            DELETE FROM ZGL015_DRE_EST03
             WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL EQ VG_NIVEL.

            DELETE FROM ZGL015_DRE_EST02
             WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL EQ VG_NIVEL.

            FTFILTRO-SIGN   = 'I'.
            IF QT_CHAR_NIVEL > STRLEN( VG_NIVEL ).
              CONCATENATE VG_NIVEL '*' INTO VG_NIVEL.
              FTFILTRO-OPTION = 'CP'.
            ELSE.
              FTFILTRO-OPTION = 'EQ'.
            ENDIF.
            FTFILTRO-LOW    = VG_NIVEL.
            APPEND FTFILTRO.

            DELETE FROM ZGL015_DRE_EST07
             WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL IN FTFILTRO.

            DELETE FROM ZGL015_DRE_EST06
             WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL IN FTFILTRO.

            DELETE FROM ZGL015_DRE_EST05
             WHERE VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL IN FTFILTRO.

            DELETE FROM ZGL015_DRE_EST04
             WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL IN FTFILTRO.

            DELETE FROM ZGL015_DRE_EST03
             WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL IN FTFILTRO.

            DELETE FROM ZGL015_DRE_EST02
             WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
               AND NIVEL IN FTFILTRO.

            CALL METHOD G_TREE->DELETE_ALL_NODES.
            PRIM_DRE_NIVEL_RE = C_X.
            PERFORM ATUALIZA_ALV_EST.
            MESSAGE 'Nível Excluido!' TYPE 'S'.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN C_INSERT OR C_EDITAR.

      CALL METHOD G_TREE->GET_SELECTED_NODE
        IMPORTING
          NODE_KEY                   = NODE
        EXCEPTIONS
          FAILED                     = 1
          SINGLE_NODE_SELECTION_ONLY = 2
          CNTL_SYSTEM_ERROR          = 3
          OTHERS                     = 4.

      CLEAR: VG_NIVEL, WA_DRE_EST02, VG_EDITAR.

      IF NOT SY-SUBRC IS INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSEIF NODE IS INITIAL.

        IF OK_CODE_9000 EQ C_INSERT.
          QT_NIVEIS = 0.
          WA_DRE_EST02-BUKRS      = WA_ZGL015_DRE_EST01-BUKRS.
          WA_DRE_EST02-VERSN      = WA_ZGL015_DRE_EST01-VERSN.
          MOVE-CORRESPONDING WA_DRE_EST02 TO ZGL015_DRE_EST02.
          CALL SCREEN 9004 STARTING AT 10 10.
        ELSE.
          MESSAGE 'Selecione um Nível para Inserção de SubNível!' TYPE 'S'.
        ENDIF.
      ELSE.

        IF ( NODE(2) EQ 'CR' ) OR ( NODE(2) EQ 'CC' ) OR ( NODE(2) EQ 'CL' ) OR ( NODE(2) EQ 'GM' ) OR ( NODE(2) EQ 'AG' ).
          MESSAGE 'Selecione um Nível para Inserção de SubNível!' TYPE 'S'.
          EXIT.
        ENDIF.

        QT_NIVEIS = 0.

        WHILE NODE IS NOT INITIAL.
          IF NOT VG_NIVEL IS INITIAL.
            CONCATENATE VG_NIVEL '.' INTO VG_NIVEL.
          ENDIF.
          NV00 = NODE(2).
          CONCATENATE VG_NIVEL NODE(2) INTO VG_NIVEL.
          NX = STRLEN( NODE ).
          NX = NX - 2.
          IF NX EQ 0.
            CLEAR NODE.
          ELSE.
            NODE = NODE+2(NX).
          ENDIF.
          QT_NIVEIS = QT_NIVEIS + 1.

          CASE QT_NIVEIS.
            WHEN 1.
              WA_DRE_EST02-NA01 = NV00.
              WA_DRE_EST02-NV01 = NV00.
            WHEN 2.
              WA_DRE_EST02-NA02 = NV00.
              WA_DRE_EST02-NV02 = NV00.
            WHEN 3.
              WA_DRE_EST02-NA03 = NV00.
              WA_DRE_EST02-NV03 = NV00.
            WHEN 4.
              WA_DRE_EST02-NA04 = NV00.
              WA_DRE_EST02-NV04 = NV00.
            WHEN 5.
              WA_DRE_EST02-NV05 = NV00.
          ENDCASE.
        ENDWHILE.

        SELECT SINGLE * INTO WA_EST02
          FROM ZGL015_DRE_EST02
         WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
           AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
           AND NIVEL EQ VG_NIVEL.

        IF SY-SUBRC IS INITIAL.
          IF OK_CODE_9000 EQ C_INSERT.
            IF WA_EST02-NIVEL_TOTAL IS INITIAL.
              WA_DRE_EST02-BUKRS     = WA_ZGL015_DRE_EST01-BUKRS.
              WA_DRE_EST02-VERSN     = WA_ZGL015_DRE_EST01-VERSN.
              WA_DRE_EST02-NITXT_ANT = WA_EST02-NITXT.
              WA_DRE_EST02-NIVEL_ANT = WA_EST02-NIVEL.
              CLEAR: WA_EST02-NIVEL.

              IF QT_CHAR_NIVEL EQ STRLEN( VG_NIVEL ).
                MESSAGE 'Não permitido mais níveis!' TYPE 'E'.
              ENDIF.
            ELSE.
              MESSAGE 'Não permitido níveis em nível de totalização!' TYPE 'E'.
            ENDIF.

          ELSEIF OK_CODE_9000 EQ C_EDITAR.
            WA_DRE_EST02-BUKRS       = WA_ZGL015_DRE_EST01-BUKRS.
            WA_DRE_EST02-VERSN       = WA_ZGL015_DRE_EST01-VERSN.
            WA_DRE_EST02-NITXT       = WA_EST02-NITXT.
            WA_DRE_EST02-NIVEL_ANT   = WA_EST02-NIVEL_ANT.
            WA_DRE_EST02-NIVEL_TOTAL = WA_EST02-NIVEL_TOTAL.
            WA_DRE_EST02-LEVAR_QTDE  = WA_EST02-LEVAR_QTDE.
            QT_NIVEIS = QT_NIVEIS - 1.

            IF NOT WA_EST02-NIVEL_ANT IS INITIAL.
              SELECT SINGLE * INTO WA_EST02
                FROM ZGL015_DRE_EST02
               WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
                 AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
                 AND NIVEL EQ WA_EST02-NIVEL_ANT.

              WA_DRE_EST02-NITXT_ANT = WA_EST02-NITXT.
            ENDIF.

            VG_EDITAR = C_X.
          ENDIF.

          MOVE-CORRESPONDING WA_DRE_EST02 TO ZGL015_DRE_EST02.
          CALL SCREEN 9004 STARTING AT 10 10.
        ENDIF.
      ENDIF.

    WHEN OK_ERRO_CL.
      CLEAR: IT_MOTRA_ERRO[].
      PERFORM MOSTRAR_ERROS_CL USING WA_ZGL015_DRE_EST01-VERSN.
      PERFORM MOSTRAR_ERROS_OBJETOS.
    WHEN OK_ERRO_CC.
      CLEAR: IT_MOTRA_ERRO[].
      PERFORM MOSTRAR_ERROS_CC USING WA_ZGL015_DRE_EST01-VERSN.
      PERFORM MOSTRAR_ERROS_OBJETOS.
    WHEN OK_ERRO_GM.
      CLEAR: IT_MOTRA_ERRO[].
      PERFORM MOSTRAR_ERROS_GM USING WA_ZGL015_DRE_EST01-VERSN.
      PERFORM MOSTRAR_ERROS_OBJETOS.
    WHEN OK_ERRO_ALL.
      CLEAR: IT_MOTRA_ERRO[].
      PERFORM MOSTRAR_ERROS_CL USING WA_ZGL015_DRE_EST01-VERSN.
      PERFORM MOSTRAR_ERROS_CC USING WA_ZGL015_DRE_EST01-VERSN.
      PERFORM MOSTRAR_ERROS_GM USING WA_ZGL015_DRE_EST01-VERSN.
      PERFORM MOSTRAR_ERROS_OBJETOS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9003  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9003 OUTPUT.
  PERFORM CRIA_ALV_DRE_EST_NIVEL.
ENDMODULE.                 " STATUS_9003  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_DRE_EST_NIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CRIA_ALV_DRE_EST_NIVEL .

  DATA: NODE_TABLE TYPE TREEV_NTAB,
        ITEM_TABLE TYPE ITEM_TABLE_TYPE,
        NODE_ESP   TYPE TV_NODEKEY.

  IF PRIM_DRE_NIVEL IS INITIAL.
    CREATE OBJECT G_APPLICATION.
    PERFORM CREATE_AND_INIT_TREE.
    PRIM_DRE_NIVEL    = C_X.
    PRIM_DRE_NIVEL_RE = C_X.
  ENDIF.

  IF NOT PRIM_DRE_NIVEL_RE IS INITIAL.

    PERFORM BUILD_NODE_AND_ITEM_TABLE USING NODE_TABLE ITEM_TABLE.

    CALL METHOD G_TREE->ADD_NODES_AND_ITEMS
      EXPORTING
        NODE_TABLE                     = NODE_TABLE
        ITEM_TABLE                     = ITEM_TABLE
        ITEM_TABLE_STRUCTURE_NAME      = 'MTREEITM'
      EXCEPTIONS
        FAILED                         = 1
        CNTL_SYSTEM_ERROR              = 3
        ERROR_IN_TABLES                = 4
        DP_ERROR                       = 5
        TABLE_STRUCTURE_NAME_NOT_FOUND = 6.

    IF NOT SY-SUBRC IS INITIAL.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    IF NOT ZGL015_DRE_EST02 IS INITIAL.

      REPLACE ALL OCCURRENCES OF REGEX '[.]' IN ZGL015_DRE_EST02-NIVEL WITH ''.
      NODE_ESP = ZGL015_DRE_EST02-NIVEL.

      CALL METHOD G_TREE->EXPAND_NODE
        EXPORTING
          NODE_KEY = NODE_ESP.

    ENDIF.

    CLEAR: PRIM_DRE_NIVEL_RE, ZGL015_DRE_EST02.
  ENDIF.

ENDFORM.                    " CRIA_ALV_DRE_EST_NIVEL

*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CREATE_AND_INIT_TREE .

  DATA: EVENTS TYPE CNTL_SIMPLE_EVENTS,
        EVENT  TYPE CNTL_SIMPLE_EVENT.

* create a container for the tree control
  CREATE OBJECT G_CUSTOM_CONTAINER
    EXPORTING
      CONTAINER_NAME              = 'TREE_CONTAINER'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.

  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

* create a list tree
  CREATE OBJECT G_TREE
    EXPORTING
      PARENT                      = G_CUSTOM_CONTAINER
      NODE_SELECTION_MODE         = CL_GUI_LIST_TREE=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION              = 'X'
      WITH_HEADERS                = ' '
    EXCEPTIONS
      CNTL_SYSTEM_ERROR           = 1
      CREATE_ERROR                = 2
      FAILED                      = 3
      ILLEGAL_NODE_SELECTION_MODE = 4
      LIFETIME_ERROR              = 5.

  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

* define the events which will be passed to the backend
  " node double click
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.                                   "
  APPEND EVENT TO EVENTS.

  " item double click
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " expand no children
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_EXPAND_NO_CHILDREN.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " link click
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_LINK_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " button click
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_BUTTON_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " checkbox change
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_CHECKBOX_CHANGE.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  CALL METHOD G_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS                    = EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.

  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

* assign event handlers in the application class to each desired event
  SET HANDLER G_APPLICATION->HANDLE_NODE_DOUBLE_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_ITEM_DOUBLE_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_EXPAND_NO_CHILDREN FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_LINK_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_BUTTON_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_CHECKBOX_CHANGE FOR G_TREE.

ENDFORM.                    " CREATE_AND_INIT_TREE

*&---------------------------------------------------------------------*
*&      Form  BUILD_NODE_AND_ITEM_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_TABLE  text
*      -->P_ITEM_TABLE  text
*----------------------------------------------------------------------*
FORM BUILD_NODE_AND_ITEM_TABLE  USING NODE_TABLE TYPE TREEV_NTAB
                                      ITEM_TABLE TYPE ITEM_TABLE_TYPE.

  DATA: NODE                      TYPE TREEV_NODE,
        ITEM                      TYPE MTREEITM,
        IT_ZGL015_DRE_EST02_CAB   TYPE TABLE OF ZGL015_DRE_EST02 WITH HEADER LINE,
        IT_ZGL015_DRE_EST02_DET02 TYPE TABLE OF ZGL015_DRE_EST02 WITH HEADER LINE,
        IT_ZGL015_DRE_EST02_DET03 TYPE TABLE OF ZGL015_DRE_EST02 WITH HEADER LINE,
        IT_ZGL015_DRE_EST02_DET04 TYPE TABLE OF ZGL015_DRE_EST02 WITH HEADER LINE,
        IT_ZGL015_DRE_EST02_DET05 TYPE TABLE OF ZGL015_DRE_EST02 WITH HEADER LINE,
        IT_ZGL015_DRE_EST02_DET06 TYPE TABLE OF ZGL015_DRE_EST02 WITH HEADER LINE,
        IT_ZGL015_DRE_EST03_AUX   TYPE TABLE OF ZGL015_DRE_EST03 WITH HEADER LINE,
        IT_ZGL015_DRE_EST04_AUX   TYPE TABLE OF ZGL015_DRE_EST04 WITH HEADER LINE,
        IT_ZGL015_DRE_EST05_AUX   TYPE TABLE OF ZGL015_DRE_EST05 WITH HEADER LINE,
        IT_ZGL015_DRE_EST06_AUX   TYPE TABLE OF ZGL015_DRE_EST06 WITH HEADER LINE,
        IT_ZGL015_DRE_EST07_AUX   TYPE TABLE OF ZGL015_DRE_EST07 WITH HEADER LINE,
        IT_SKAT                   TYPE TABLE OF SKAT WITH HEADER LINE,
        IT_TKT05                  TYPE TABLE OF TKT05 WITH HEADER LINE,
        IT_CEPCT                  TYPE TABLE OF CEPCT WITH HEADER LINE,
        IT_T023T                  TYPE TABLE OF T023T WITH HEADER LINE,
        IT_ZGL015_DRE_EST02_TXT   TYPE TABLE OF ZGL015_DRE_EST02 WITH HEADER LINE,
        XID                       TYPE STRING.

  CLEAR: NODE_TABLE,
         ITEM_TABLE,
         IT_ZGL015_DRE_EST02[],
         IT_ZGL015_DRE_EST02_CAB[],
         IT_ZGL015_DRE_EST03[],
         IT_ZGL015_DRE_EST03_AUX[],
         IT_ZGL015_DRE_EST03_ID[],
         IT_ZGL015_DRE_EST04[],
         IT_ZGL015_DRE_EST04_AUX[],
         IT_ZGL015_DRE_EST04_ID[],
         IT_ZGL015_DRE_EST05[],
         IT_ZGL015_DRE_EST05_AUX[],
         IT_ZGL015_DRE_EST05_ID[],
         IT_ZGL015_DRE_EST06[],
         IT_ZGL015_DRE_EST06_AUX[],
         IT_ZGL015_DRE_EST06_ID[],
         IT_ZGL015_DRE_EST07[],
         IT_ZGL015_DRE_EST07_AUX[],
         IT_ZGL015_DRE_EST07_ID[].

  "*************************************************************************************
  "*** Níveis de Totalização ***********************************************************
  SELECT * INTO TABLE IT_ZGL015_DRE_EST07
    FROM ZGL015_DRE_EST07
   WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
     AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN.

  MOVE IT_ZGL015_DRE_EST07[] TO IT_ZGL015_DRE_EST07_AUX[].
  SORT IT_ZGL015_DRE_EST07_AUX BY NIVEL_AGPD.
  DELETE ADJACENT DUPLICATES FROM IT_ZGL015_DRE_EST07_AUX COMPARING NIVEL_AGPD.

  IF NOT IT_ZGL015_DRE_EST07_AUX[] IS INITIAL.
    SELECT * INTO TABLE IT_ZGL015_DRE_EST02_TXT
      FROM ZGL015_DRE_EST02
       FOR ALL ENTRIES IN IT_ZGL015_DRE_EST07_AUX
     WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
       AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN
       AND NIVEL EQ IT_ZGL015_DRE_EST07_AUX-NIVEL_AGPD.
  ENDIF.

  SORT IT_ZGL015_DRE_EST07 BY NIVEL NIVEL_AGPD.

  LOOP AT IT_ZGL015_DRE_EST07.
    MOVE-CORRESPONDING IT_ZGL015_DRE_EST07 TO IT_ZGL015_DRE_EST07_ID.
    MOVE SY-TABIX TO XID.
    CONCATENATE 'AG' XID INTO IT_ZGL015_DRE_EST07_ID-ID.
    READ TABLE IT_ZGL015_DRE_EST02_TXT WITH KEY NIVEL = IT_ZGL015_DRE_EST07_ID-NIVEL_AGPD.
    IF SY-SUBRC IS INITIAL.
      IT_ZGL015_DRE_EST07_ID-TX = IT_ZGL015_DRE_EST02_TXT-NITXT.
    ENDIF.
    APPEND IT_ZGL015_DRE_EST07_ID.
  ENDLOOP.
  "*************************************************************************************

  "*************************************************************************************
  "*** Grupo de Material ***************************************************************
  SELECT * INTO TABLE IT_ZGL015_DRE_EST06
    FROM ZGL015_DRE_EST06
   WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
     AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN.

  MOVE IT_ZGL015_DRE_EST06[] TO IT_ZGL015_DRE_EST06_AUX[].
  SORT IT_ZGL015_DRE_EST06_AUX BY MATKL.
  DELETE ADJACENT DUPLICATES FROM IT_ZGL015_DRE_EST06_AUX COMPARING MATKL.

  IF NOT IT_ZGL015_DRE_EST06_AUX[] IS INITIAL.
    SELECT * INTO TABLE IT_T023T
      FROM T023T
       FOR ALL ENTRIES IN IT_ZGL015_DRE_EST06_AUX
     WHERE SPRAS EQ SY-LANGU
       AND MATKL EQ IT_ZGL015_DRE_EST06_AUX-MATKL.
  ENDIF.

  SORT IT_ZGL015_DRE_EST06 BY NIVEL KTOPL SAKNR MATKL.

  LOOP AT IT_ZGL015_DRE_EST06.
    MOVE-CORRESPONDING IT_ZGL015_DRE_EST06 TO IT_ZGL015_DRE_EST06_ID.
    MOVE SY-TABIX TO XID.
    CONCATENATE 'GM' XID INTO IT_ZGL015_DRE_EST06_ID-ID.
    READ TABLE IT_T023T WITH KEY MATKL = IT_ZGL015_DRE_EST06_ID-MATKL.
    IF SY-SUBRC IS INITIAL.
      IT_ZGL015_DRE_EST06_ID-TX = IT_T023T-WGBEZ60.
    ENDIF.
    APPEND IT_ZGL015_DRE_EST06_ID.
  ENDLOOP.
  "*************************************************************************************


  "*************************************************************************************
  "*** Centro de Lucro *****************************************************************
  SELECT * INTO TABLE IT_ZGL015_DRE_EST05
    FROM ZGL015_DRE_EST05
   WHERE VERSN EQ WA_ZGL015_DRE_EST01-VERSN.

  MOVE IT_ZGL015_DRE_EST05[] TO IT_ZGL015_DRE_EST05_AUX[].
  SORT IT_ZGL015_DRE_EST05_AUX BY KOKRS PRCTR.
  DELETE ADJACENT DUPLICATES FROM IT_ZGL015_DRE_EST05_AUX COMPARING KOKRS PRCTR.

  IF NOT IT_ZGL015_DRE_EST05_AUX[] IS INITIAL.
    SELECT * INTO TABLE IT_CEPCT
      FROM CEPCT
       FOR ALL ENTRIES IN IT_ZGL015_DRE_EST05_AUX
     WHERE SPRAS EQ SY-LANGU
       AND PRCTR EQ IT_ZGL015_DRE_EST05_AUX-PRCTR
       AND KOKRS EQ IT_ZGL015_DRE_EST05_AUX-KOKRS.
  ENDIF.

  SORT IT_ZGL015_DRE_EST05 BY NIVEL KTOPL SAKNR KOKRS PRCTR.

  LOOP AT IT_ZGL015_DRE_EST05.
    MOVE-CORRESPONDING IT_ZGL015_DRE_EST05 TO IT_ZGL015_DRE_EST05_ID.
    MOVE SY-TABIX TO XID.
    CONCATENATE 'CL' XID INTO IT_ZGL015_DRE_EST05_ID-ID.
    READ TABLE IT_CEPCT WITH KEY PRCTR = IT_ZGL015_DRE_EST05_ID-PRCTR
                                 KOKRS = IT_ZGL015_DRE_EST05_ID-KOKRS.
    IF SY-SUBRC IS INITIAL.
      IT_ZGL015_DRE_EST05_ID-TX = IT_CEPCT-LTEXT.
    ENDIF.
    APPEND IT_ZGL015_DRE_EST05_ID.
  ENDLOOP.
  "*************************************************************************************

  "*************************************************************************************
  "*** Centro de Custo *****************************************************************
  SELECT * INTO TABLE IT_ZGL015_DRE_EST04
    FROM ZGL015_DRE_EST04
   WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
     AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN.

  MOVE IT_ZGL015_DRE_EST04[] TO IT_ZGL015_DRE_EST04_AUX[].
  SORT IT_ZGL015_DRE_EST04_AUX BY KOSAR.
  DELETE ADJACENT DUPLICATES FROM IT_ZGL015_DRE_EST04_AUX COMPARING KOSAR.

  IF NOT IT_ZGL015_DRE_EST04_AUX[] IS INITIAL.
    SELECT * INTO TABLE IT_TKT05
      FROM TKT05
       FOR ALL ENTRIES IN IT_ZGL015_DRE_EST04_AUX
     WHERE SPRAS EQ SY-LANGU
       AND KOSAR EQ IT_ZGL015_DRE_EST04_AUX-KOSAR.
  ENDIF.

  SORT IT_ZGL015_DRE_EST04 BY NIVEL KTOPL SAKNR KOSAR.

  LOOP AT IT_ZGL015_DRE_EST04.
    MOVE-CORRESPONDING IT_ZGL015_DRE_EST04 TO IT_ZGL015_DRE_EST04_ID.
    MOVE SY-TABIX TO XID.
    CONCATENATE 'CC' XID INTO IT_ZGL015_DRE_EST04_ID-ID.
    READ TABLE IT_TKT05 WITH KEY KOSAR = IT_ZGL015_DRE_EST04_ID-KOSAR.
    IF SY-SUBRC IS INITIAL.
      IT_ZGL015_DRE_EST04_ID-TX = IT_TKT05-KTEXT.
    ENDIF.
    APPEND IT_ZGL015_DRE_EST04_ID.
  ENDLOOP.
  "*************************************************************************************


  "*************************************************************************************
  "*** Conta Razão *********************************************************************
  SELECT * INTO TABLE IT_ZGL015_DRE_EST03
    FROM ZGL015_DRE_EST03
   WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
     AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN.

  MOVE IT_ZGL015_DRE_EST03[] TO IT_ZGL015_DRE_EST03_AUX[].
  SORT IT_ZGL015_DRE_EST03_AUX BY KTOPL SAKNR.
  DELETE ADJACENT DUPLICATES FROM IT_ZGL015_DRE_EST03_AUX COMPARING KTOPL SAKNR.

  IF NOT IT_ZGL015_DRE_EST03_AUX[] IS INITIAL.
    SELECT * INTO TABLE IT_SKAT
      FROM SKAT
       FOR ALL ENTRIES IN IT_ZGL015_DRE_EST03_AUX
     WHERE SPRAS EQ SY-LANGU
       AND KTOPL EQ IT_ZGL015_DRE_EST03_AUX-KTOPL
       AND SAKNR EQ IT_ZGL015_DRE_EST03_AUX-SAKNR.
  ENDIF.

  SORT IT_ZGL015_DRE_EST03 BY NIVEL KTOPL SAKNR.

  LOOP AT IT_ZGL015_DRE_EST03.
    MOVE-CORRESPONDING IT_ZGL015_DRE_EST03 TO IT_ZGL015_DRE_EST03_ID.
    MOVE SY-TABIX TO XID.
    CONCATENATE 'CR' XID INTO IT_ZGL015_DRE_EST03_ID-ID.
    READ TABLE IT_SKAT WITH KEY KTOPL = IT_ZGL015_DRE_EST03_ID-KTOPL
                                SAKNR = IT_ZGL015_DRE_EST03_ID-SAKNR.
    IF SY-SUBRC IS INITIAL.
      IT_ZGL015_DRE_EST03_ID-TX = IT_SKAT-TXT50.
    ENDIF.
    APPEND IT_ZGL015_DRE_EST03_ID.
  ENDLOOP.
  "*************************************************************************************

  SELECT * INTO TABLE IT_ZGL015_DRE_EST02
    FROM ZGL015_DRE_EST02
   WHERE BUKRS EQ WA_ZGL015_DRE_EST01-BUKRS
     AND VERSN EQ WA_ZGL015_DRE_EST01-VERSN.

  IF SY-SUBRC IS INITIAL.

    MOVE IT_ZGL015_DRE_EST02[] TO IT_ZGL015_DRE_EST02_CAB[].
    DELETE IT_ZGL015_DRE_EST02_CAB WHERE NIVEL_ANT NE SPACE.
    SORT IT_ZGL015_DRE_EST02_CAB BY NIVEL.                      "MIGNOW - 21.07.2023 - MG5677 - Ajustes Ordenação Saida do Relatório.

    LOOP AT IT_ZGL015_DRE_EST02_CAB.

      CLEAR: NODE-RELATKEY,    "Special case: A root node has no parent
             NODE-RELATSHIP,   "node.
             NODE-EXPANDER.    " see below

      IF IT_ZGL015_DRE_EST02_CAB-NIVEL_TOTAL IS INITIAL.
        NODE-N_IMAGE   = ICON_NEXT_NODE.
        NODE-EXP_IMAGE = ICON_PREVIOUS_NODE.
      ELSE.
        NODE-N_IMAGE   = ICON_SUM.
        NODE-EXP_IMAGE = ICON_SUM.
      ENDIF.
      NODE-HIDDEN    = ' '.    " The node is visible,
      NODE-DISABLED  = ' '.    " selectable,
      NODE-ISFOLDER  = 'X'.    " a folder.
      NODE-NODE_KEY  = IT_ZGL015_DRE_EST02_CAB-NIVEL.
      REPLACE ALL OCCURRENCES OF REGEX '[.]' IN NODE-NODE_KEY WITH ''.
      APPEND NODE TO NODE_TABLE.

      CLEAR ITEM.
      ITEM-NODE_KEY   = NODE-NODE_KEY.
      ITEM-ITEM_NAME  = '1'."p_node-node_key. " Item with name '1'
      ITEM-CLASS      = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
      ITEM-ALIGNMENT  = CL_GUI_LIST_TREE=>ALIGN_AUTO.
      ITEM-FONT       = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
      ITEM-TEXT       = IT_ZGL015_DRE_EST02_CAB-NIVEL.
      "item-usebgcolor = 'X'.
      APPEND ITEM TO ITEM_TABLE.

      ITEM-ITEM_NAME = '2'."p_node-node_key. " Item with name '1'
      ITEM-TEXT      = IT_ZGL015_DRE_EST02_CAB-NIVEL_TOTAL.
      APPEND ITEM TO ITEM_TABLE.

      ITEM-ITEM_NAME = '3'."p_node-node_key. " Item with name '1'
      ITEM-TEXT      = IT_ZGL015_DRE_EST02_CAB-NITXT.
      APPEND ITEM TO ITEM_TABLE.

      IF IT_ZGL015_DRE_EST02_CAB-LEVAR_QTDE IS NOT INITIAL.
        ITEM-ITEM_NAME = '4'."p_node-node_key. " Item with name '1'
        ITEM-TEXT      = '(Soma Quantidades)'.
        APPEND ITEM TO ITEM_TABLE.
      ENDIF.

      LOOP AT IT_ZGL015_DRE_EST07_ID WHERE NIVEL EQ IT_ZGL015_DRE_EST02_CAB-NIVEL.
        NODE-N_IMAGE   = ICON_INTERMEDIATE_SUM.
        NODE-EXP_IMAGE = ICON_INTERMEDIATE_SUM.
        NODE-ISFOLDER  = ' '.    " a folder.
        NODE-HIDDEN    = ' '.    " The node is visible,
        NODE-DISABLED  = ' '.    " selectable,
        NODE-NODE_KEY  = IT_ZGL015_DRE_EST07_ID-ID.
        NODE-RELATKEY  = IT_ZGL015_DRE_EST02_CAB-NIVEL.
        NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
        APPEND NODE TO NODE_TABLE.

        CLEAR ITEM.
        ITEM-NODE_KEY   = NODE-NODE_KEY.
        ITEM-ITEM_NAME  = '1'."p_node-node_key. " Item with name '1'
        ITEM-CLASS      = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
        ITEM-ALIGNMENT  = CL_GUI_LIST_TREE=>ALIGN_AUTO.
        ITEM-FONT       = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
        ITEM-TEXT       = IT_ZGL015_DRE_EST07_ID-NIVEL_AGPD.
        APPEND ITEM TO ITEM_TABLE.

        ITEM-ITEM_NAME  = '2'."p_node-node_key. " Item with name '1'
        ITEM-TEXT       = IT_ZGL015_DRE_EST07_ID-TX.
        APPEND ITEM TO ITEM_TABLE.
      ENDLOOP.

      IF NOT IT_ZGL015_DRE_EST02_CAB-NIVEL_TOTAL IS INITIAL.
        CONTINUE.
      ENDIF.

      MOVE IT_ZGL015_DRE_EST02[] TO IT_ZGL015_DRE_EST02_DET02[].
      READ TABLE IT_ZGL015_DRE_EST02_DET02 WITH KEY NIVEL = IT_ZGL015_DRE_EST02_CAB-NIVEL
                                                    NIVEL_ANT = SPACE.
      IF SY-SUBRC IS INITIAL.
        PERFORM ADD_NODE USING NODE_TABLE ITEM_TABLE C_X IT_ZGL015_DRE_EST02_DET02 NODE ITEM C_X.
      ENDIF.

      DELETE IT_ZGL015_DRE_EST02_DET02 WHERE NIVEL_ANT NE IT_ZGL015_DRE_EST02_CAB-NIVEL.

      LOOP AT IT_ZGL015_DRE_EST02_DET02.

        CLEAR: IT_ZGL015_DRE_EST02_DET03[].
        MOVE IT_ZGL015_DRE_EST02[] TO IT_ZGL015_DRE_EST02_DET03[].
        DELETE IT_ZGL015_DRE_EST02_DET03 WHERE NIVEL_ANT NE IT_ZGL015_DRE_EST02_DET02-NIVEL.

        "Criar Filho do nó
        IF NOT IT_ZGL015_DRE_EST02_DET03[] IS INITIAL.
          PERFORM ADD_NODE USING NODE_TABLE ITEM_TABLE C_X IT_ZGL015_DRE_EST02_DET02 NODE ITEM SPACE.
        ELSE.
          PERFORM ADD_NODE USING NODE_TABLE ITEM_TABLE SPACE IT_ZGL015_DRE_EST02_DET02 NODE ITEM SPACE.
        ENDIF.

        LOOP AT IT_ZGL015_DRE_EST02_DET03.
          CLEAR: IT_ZGL015_DRE_EST02_DET04[].
          MOVE IT_ZGL015_DRE_EST02[] TO IT_ZGL015_DRE_EST02_DET04[].
          DELETE IT_ZGL015_DRE_EST02_DET04 WHERE NIVEL_ANT NE IT_ZGL015_DRE_EST02_DET03-NIVEL.

          "Criar Filho do nó
          IF NOT IT_ZGL015_DRE_EST02_DET04[] IS INITIAL.
            PERFORM ADD_NODE USING NODE_TABLE ITEM_TABLE C_X IT_ZGL015_DRE_EST02_DET03 NODE ITEM SPACE.
          ELSE.
            PERFORM ADD_NODE USING NODE_TABLE ITEM_TABLE SPACE IT_ZGL015_DRE_EST02_DET03 NODE ITEM SPACE.
          ENDIF.

          LOOP AT IT_ZGL015_DRE_EST02_DET04.
            CLEAR: IT_ZGL015_DRE_EST02_DET05[].
            MOVE IT_ZGL015_DRE_EST02[] TO IT_ZGL015_DRE_EST02_DET05[].
            DELETE IT_ZGL015_DRE_EST02_DET05 WHERE NIVEL_ANT NE IT_ZGL015_DRE_EST02_DET04-NIVEL.
            "Criar Filho do nó
            IF NOT IT_ZGL015_DRE_EST02_DET05[] IS INITIAL.
              PERFORM ADD_NODE USING NODE_TABLE ITEM_TABLE C_X IT_ZGL015_DRE_EST02_DET04 NODE ITEM SPACE.
            ELSE.
              PERFORM ADD_NODE USING NODE_TABLE ITEM_TABLE SPACE IT_ZGL015_DRE_EST02_DET04 NODE ITEM SPACE.
            ENDIF.

            LOOP AT IT_ZGL015_DRE_EST02_DET05.
              CLEAR: IT_ZGL015_DRE_EST02_DET06[].
              "Criar Filho do nó
              PERFORM ADD_NODE USING NODE_TABLE ITEM_TABLE SPACE IT_ZGL015_DRE_EST02_DET05 NODE ITEM SPACE.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " BUILD_NODE_AND_ITEM_TABLE


*&---------------------------------------------------------------------*
*&      Form  ADD_NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ADD_NODE  USING  NODE_TABLE                  TYPE TREEV_NTAB
                      ITEM_TABLE                  TYPE ITEM_TABLE_TYPE
                      P_EXISTE_SUB                TYPE C
                      P_IT_ZGL015_DRE_EST02_DETAT TYPE ZGL015_DRE_EST02
                      P_NODE                      TYPE TREEV_NODE
                      P_ITEM                      TYPE MTREEITM
                      P_OBJT                      TYPE C.

  DATA: VG_NIVEL01 TYPE ZNIVEL_DRE,
        VG_NIVEL02 TYPE ZNIVEL_DRE.

  CLEAR: P_NODE-RELATKEY,    "Special case: A root node has no parent
         P_NODE-RELATSHIP,   "node.
         P_NODE-EXPANDER.    " see below

  VG_NIVEL01 = P_IT_ZGL015_DRE_EST02_DETAT-NIVEL.
  VG_NIVEL02 = P_IT_ZGL015_DRE_EST02_DETAT-NIVEL_ANT.

  REPLACE ALL OCCURRENCES OF REGEX '[.]' IN VG_NIVEL01 WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '[.]' IN VG_NIVEL02 WITH ''.

  IF P_OBJT IS INITIAL.

    IF NOT P_EXISTE_SUB IS INITIAL.
      P_NODE-N_IMAGE   = ICON_NEXT_NODE.
      P_NODE-EXP_IMAGE = ICON_PREVIOUS_NODE.
    ELSE.
      READ TABLE IT_ZGL015_DRE_EST03_ID WITH KEY NIVEL = P_IT_ZGL015_DRE_EST02_DETAT-NIVEL.
      IF SY-SUBRC IS INITIAL.
        P_NODE-N_IMAGE   = ICON_NEXT_NODE.
        P_NODE-EXP_IMAGE = ICON_TREE.
      ELSE.
        P_NODE-N_IMAGE   = ICON_LED_RED.
        P_NODE-EXP_IMAGE = ICON_LED_RED.
      ENDIF.
    ENDIF.
    P_NODE-ISFOLDER  = 'X'.    " a folder.
    P_NODE-HIDDEN    = ' '.    " The node is visible,
    P_NODE-DISABLED  = ' '.    " selectable,
    P_NODE-NODE_KEY  = VG_NIVEL01.
    P_NODE-RELATKEY  = VG_NIVEL02.
    P_NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
    APPEND P_NODE TO NODE_TABLE.

    CLEAR P_ITEM.
    P_ITEM-NODE_KEY   = P_NODE-NODE_KEY.
    P_ITEM-ITEM_NAME  = '1'."p_node-node_key. " Item with name '1'
    P_ITEM-CLASS      = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
    P_ITEM-ALIGNMENT  = CL_GUI_LIST_TREE=>ALIGN_AUTO.
    P_ITEM-FONT       = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
    P_ITEM-TEXT       = P_IT_ZGL015_DRE_EST02_DETAT-NIVEL.
    "p_item-usebgcolor = 'X'.
    APPEND P_ITEM TO ITEM_TABLE.

    P_ITEM-ITEM_NAME  = '2'."p_node-node_key. " Item with name '1'
    P_ITEM-TEXT       = P_IT_ZGL015_DRE_EST02_DETAT-NIVEL_TOTAL.
    APPEND P_ITEM TO ITEM_TABLE.

    P_ITEM-ITEM_NAME  = '3'."p_node-node_key. " Item with name '1'
    P_ITEM-TEXT       = P_IT_ZGL015_DRE_EST02_DETAT-NITXT.
    APPEND P_ITEM TO ITEM_TABLE.

    IF P_IT_ZGL015_DRE_EST02_DETAT-LEVAR_QTDE IS NOT INITIAL.
      P_ITEM-ITEM_NAME = '4'."p_node-node_key. " Item with name '1'
      P_ITEM-TEXT      = '(Soma Quantidades)'.
      APPEND P_ITEM TO ITEM_TABLE.
    ENDIF.

    LOOP AT IT_ZGL015_DRE_EST07_ID WHERE NIVEL EQ P_IT_ZGL015_DRE_EST02_DETAT-NIVEL.
      P_NODE-N_IMAGE   = ICON_INTERMEDIATE_SUM.
      P_NODE-EXP_IMAGE = ICON_INTERMEDIATE_SUM.
      P_NODE-ISFOLDER  = ' '.    " a folder.
      P_NODE-HIDDEN    = ' '.    " The node is visible,
      P_NODE-DISABLED  = ' '.    " selectable,
      P_NODE-NODE_KEY  = IT_ZGL015_DRE_EST07_ID-ID.
      P_NODE-RELATKEY  = VG_NIVEL01.
      P_NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
      APPEND P_NODE TO NODE_TABLE.

      CLEAR P_ITEM.
      P_ITEM-NODE_KEY   = P_NODE-NODE_KEY.
      P_ITEM-ITEM_NAME  = '1'."p_node-node_key. " Item with name '1'
      P_ITEM-CLASS      = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
      P_ITEM-ALIGNMENT  = CL_GUI_LIST_TREE=>ALIGN_AUTO.
      P_ITEM-FONT       = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
      P_ITEM-TEXT       = IT_ZGL015_DRE_EST07_ID-NIVEL_AGPD.
      APPEND P_ITEM TO ITEM_TABLE.

      P_ITEM-ITEM_NAME  = '2'."p_node-node_key. " Item with name '1'
      P_ITEM-TEXT       = IT_ZGL015_DRE_EST07_ID-TX.
      APPEND P_ITEM TO ITEM_TABLE.
    ENDLOOP.

    IF NOT P_IT_ZGL015_DRE_EST02_DETAT-NIVEL_TOTAL IS INITIAL.
      EXIT.
    ENDIF.

  ENDIF.

  LOOP AT IT_ZGL015_DRE_EST03_ID WHERE NIVEL EQ P_IT_ZGL015_DRE_EST02_DETAT-NIVEL.

    P_NODE-N_IMAGE   = ICON_SUBSCRIPTION.
    P_NODE-EXP_IMAGE = ICON_OUTBOX.
    P_NODE-ISFOLDER  = ' '.    " a folder.

    READ TABLE IT_ZGL015_DRE_EST04_ID WITH KEY NIVEL = IT_ZGL015_DRE_EST03_ID-NIVEL
                                               SAKNR = IT_ZGL015_DRE_EST03_ID-SAKNR.
    IF SY-SUBRC IS INITIAL.
      P_NODE-ISFOLDER  = 'X'.    " a folder.
    ENDIF.
    READ TABLE IT_ZGL015_DRE_EST05_ID WITH KEY NIVEL = IT_ZGL015_DRE_EST03_ID-NIVEL
                                               SAKNR = IT_ZGL015_DRE_EST03_ID-SAKNR.
    IF SY-SUBRC IS INITIAL.
      P_NODE-ISFOLDER  = 'X'.    " a folder.
    ENDIF.
    READ TABLE IT_ZGL015_DRE_EST06_ID WITH KEY NIVEL = IT_ZGL015_DRE_EST03_ID-NIVEL
                                               SAKNR = IT_ZGL015_DRE_EST03_ID-SAKNR.
    IF SY-SUBRC IS INITIAL.
      P_NODE-ISFOLDER  = 'X'.    " a folder.
    ENDIF.

    P_NODE-HIDDEN    = ' '.    " The node is visible,
    P_NODE-DISABLED  = ' '.    " selectable,
    P_NODE-NODE_KEY  = IT_ZGL015_DRE_EST03_ID-ID.
    P_NODE-RELATKEY  = VG_NIVEL01.
    P_NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
    APPEND P_NODE TO NODE_TABLE.

    CLEAR P_ITEM.
    P_ITEM-NODE_KEY   = P_NODE-NODE_KEY.
    P_ITEM-ITEM_NAME  = '1'."p_node-node_key. " Item with name '1'
    P_ITEM-CLASS      = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
    P_ITEM-ALIGNMENT  = CL_GUI_LIST_TREE=>ALIGN_AUTO.
    P_ITEM-FONT       = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
    P_ITEM-TEXT       = IT_ZGL015_DRE_EST03_ID-KTOPL.
    APPEND P_ITEM TO ITEM_TABLE.

    P_ITEM-ITEM_NAME  = '2'."p_node-node_key. " Item with name '1'
    P_ITEM-TEXT       = IT_ZGL015_DRE_EST03_ID-SAKNR.
    APPEND P_ITEM TO ITEM_TABLE.

    P_ITEM-ITEM_NAME  = '3'."p_node-node_key. " Item with name '1'
    P_ITEM-TEXT       = IT_ZGL015_DRE_EST03_ID-TX.
    APPEND P_ITEM TO ITEM_TABLE.

    IF IT_ZGL015_DRE_EST03_ID-LEVAR_QTDE IS NOT INITIAL.
      P_ITEM-ITEM_NAME = '4'."p_node-node_key. " Item with name '1'
      P_ITEM-TEXT      = '(Soma Quantidades)'.
      APPEND P_ITEM TO ITEM_TABLE.
    ENDIF.

*ICON_BIW_APPLICATION
    LOOP AT IT_ZGL015_DRE_EST04_ID WHERE NIVEL EQ P_IT_ZGL015_DRE_EST02_DETAT-NIVEL
                                     AND SAKNR EQ IT_ZGL015_DRE_EST03_ID-SAKNR.

      P_NODE-N_IMAGE   = ICON_BIW_APPLICATION.
      P_NODE-EXP_IMAGE = ICON_BIW_APPLICATION.
      P_NODE-ISFOLDER  = ' '.    " a folder.
      P_NODE-HIDDEN    = ' '.    " The node is visible,
      P_NODE-DISABLED  = ' '.    " selectable,
      P_NODE-NODE_KEY  = IT_ZGL015_DRE_EST04_ID-ID.
      P_NODE-RELATKEY  = IT_ZGL015_DRE_EST03_ID-ID.
      P_NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
      APPEND P_NODE TO NODE_TABLE.

      CLEAR P_ITEM.
      P_ITEM-NODE_KEY   = P_NODE-NODE_KEY.
      P_ITEM-ITEM_NAME  = '1'."p_node-node_key. " Item with name '1'
      P_ITEM-CLASS      = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
      P_ITEM-ALIGNMENT  = CL_GUI_LIST_TREE=>ALIGN_AUTO.
      P_ITEM-FONT       = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
      P_ITEM-TEXT       = IT_ZGL015_DRE_EST04_ID-KOSAR.
      APPEND P_ITEM TO ITEM_TABLE.

      P_ITEM-ITEM_NAME  = '2'."p_node-node_key. " Item with name '1'
      P_ITEM-TEXT       = IT_ZGL015_DRE_EST04_ID-TX.
      APPEND P_ITEM TO ITEM_TABLE.

    ENDLOOP.

*ICON_BIW_INFO_AREA
    LOOP AT IT_ZGL015_DRE_EST05_ID WHERE NIVEL EQ P_IT_ZGL015_DRE_EST02_DETAT-NIVEL
                                     AND SAKNR EQ IT_ZGL015_DRE_EST03_ID-SAKNR.

      P_NODE-N_IMAGE   = ICON_BIW_INFO_AREA.
      P_NODE-EXP_IMAGE = ICON_BIW_INFO_AREA.
      P_NODE-ISFOLDER  = ' '.    " a folder.
      P_NODE-HIDDEN    = ' '.    " The node is visible,
      P_NODE-DISABLED  = ' '.    " selectable,
      P_NODE-NODE_KEY  = IT_ZGL015_DRE_EST05_ID-ID.
      P_NODE-RELATKEY  = IT_ZGL015_DRE_EST03_ID-ID.
      P_NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
      APPEND P_NODE TO NODE_TABLE.

*ICON_BIW_INFO_CUBE

      CLEAR P_ITEM.
      P_ITEM-NODE_KEY   = P_NODE-NODE_KEY.
      P_ITEM-ITEM_NAME  = '1'."p_node-node_key. " Item with name '1'
      P_ITEM-CLASS      = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
      P_ITEM-ALIGNMENT  = CL_GUI_LIST_TREE=>ALIGN_AUTO.
      P_ITEM-FONT       = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
      P_ITEM-TEXT       = IT_ZGL015_DRE_EST05_ID-PRCTR.
      APPEND P_ITEM TO ITEM_TABLE.

      P_ITEM-ITEM_NAME  = '2'."p_node-node_key. " Item with name '1'
      P_ITEM-TEXT       = IT_ZGL015_DRE_EST05_ID-TX.
      APPEND P_ITEM TO ITEM_TABLE.

    ENDLOOP.

*ICON_BIW_INFO_CUBE
    LOOP AT IT_ZGL015_DRE_EST06_ID WHERE NIVEL EQ P_IT_ZGL015_DRE_EST02_DETAT-NIVEL
                                     AND SAKNR EQ IT_ZGL015_DRE_EST03_ID-SAKNR.

      P_NODE-N_IMAGE   = ICON_BIW_INFO_CUBE.
      P_NODE-EXP_IMAGE = ICON_BIW_INFO_CUBE.
      P_NODE-ISFOLDER  = ' '.    " a folder.
      P_NODE-HIDDEN    = ' '.    " The node is visible,
      P_NODE-DISABLED  = ' '.    " selectable,
      P_NODE-NODE_KEY  = IT_ZGL015_DRE_EST06_ID-ID.
      P_NODE-RELATKEY  = IT_ZGL015_DRE_EST03_ID-ID.
      P_NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
      APPEND P_NODE TO NODE_TABLE.

      CLEAR P_ITEM.
      P_ITEM-NODE_KEY   = P_NODE-NODE_KEY.
      P_ITEM-ITEM_NAME  = '1'."p_node-node_key. " Item with name '1'
      P_ITEM-CLASS      = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
      P_ITEM-ALIGNMENT  = CL_GUI_LIST_TREE=>ALIGN_AUTO.
      P_ITEM-FONT       = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
      P_ITEM-TEXT       = IT_ZGL015_DRE_EST06_ID-MATKL.
      APPEND P_ITEM TO ITEM_TABLE.

      P_ITEM-ITEM_NAME  = '2'."p_node-node_key. " Item with name '1'
      P_ITEM-TEXT       = IT_ZGL015_DRE_EST06_ID-TX.
      APPEND P_ITEM TO ITEM_TABLE.

    ENDLOOP.

  ENDLOOP.

ENDFORM. " ADD_NODE

*&---------------------------------------------------------------------*
*&      Module  STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9004 OUTPUT.

  SET PF-STATUS 'PF9004'.
  SET TITLEBAR 'TL9004'.

  IF VG_EDITAR IS INITIAL.
    LOOP AT SCREEN.
      IF ( SCREEN-NAME EQ 'WA_DRE_EST02-NV01' ) AND ( QT_NIVEIS = 0 ).
        SCREEN-INPUT    = 1.
        SCREEN-REQUIRED = 1.
        MODIFY SCREEN.
      ELSEIF ( SCREEN-NAME EQ 'WA_DRE_EST02-NV02' ) AND ( QT_NIVEIS = 1 ).
        SCREEN-INPUT    = 1.
        SCREEN-REQUIRED = 1.
        MODIFY SCREEN.
      ELSEIF ( SCREEN-NAME EQ 'WA_DRE_EST02-NV03' ) AND ( QT_NIVEIS = 2 ).
        SCREEN-INPUT    = 1.
        SCREEN-REQUIRED = 1.
        MODIFY SCREEN.
      ELSEIF ( SCREEN-NAME EQ 'WA_DRE_EST02-NV04' ) AND ( QT_NIVEIS = 3 ).
        SCREEN-INPUT    = 1.
        SCREEN-REQUIRED = 1.
        MODIFY SCREEN.
      ELSEIF ( SCREEN-NAME EQ 'WA_DRE_EST02-NV05' ) AND ( QT_NIVEIS = 4 ).
        SCREEN-INPUT    = 1.
        SCREEN-REQUIRED = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF NOT VG_EDITAR IS INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'WA_DRE_EST02-NIVEL_TOTAL'.
        SCREEN-INPUT  = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_9004  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9004_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_9004_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9004 INPUT.

  DATA: VG_DUB TYPE ZGL015_DRE_EST02.

  IF OK_CODE_9004 EQ C_CONF.

    CASE QT_NIVEIS.
      WHEN 0.
        ZGL015_DRE_EST02-NIVEL = WA_DRE_EST02-NV01.
      WHEN 1.
        CONCATENATE ZGL015_DRE_EST02-NIVEL_ANT '.' WA_DRE_EST02-NV02 INTO ZGL015_DRE_EST02-NIVEL.
      WHEN 2.
        CONCATENATE ZGL015_DRE_EST02-NIVEL_ANT '.' WA_DRE_EST02-NV03 INTO ZGL015_DRE_EST02-NIVEL.
      WHEN 3.
        CONCATENATE ZGL015_DRE_EST02-NIVEL_ANT '.' WA_DRE_EST02-NV04 INTO ZGL015_DRE_EST02-NIVEL.
      WHEN 4.
        CONCATENATE ZGL015_DRE_EST02-NIVEL_ANT '.' WA_DRE_EST02-NV05 INTO ZGL015_DRE_EST02-NIVEL.
    ENDCASE.

    IF VG_EDITAR IS INITIAL.
      SELECT SINGLE * INTO VG_DUB
        FROM ZGL015_DRE_EST02
       WHERE BUKRS EQ ZGL015_DRE_EST02-BUKRS
         AND VERSN EQ ZGL015_DRE_EST02-VERSN
         AND NIVEL EQ ZGL015_DRE_EST02-NIVEL.
      IF SY-SUBRC IS INITIAL.
        MESSAGE 'Este nível já existe!' TYPE 'S'.
        EXIT.
      ENDIF.
    ENDIF.

    ZGL015_DRE_EST02-NITXT       = WA_DRE_EST02-NITXT.
    ZGL015_DRE_EST02-NIVEL_TOTAL = WA_DRE_EST02-NIVEL_TOTAL.
    ZGL015_DRE_EST02-LEVAR_QTDE  = WA_DRE_EST02-LEVAR_QTDE.
    MODIFY ZGL015_DRE_EST02.

    CALL METHOD G_TREE->DELETE_ALL_NODES.
    PRIM_DRE_NIVEL_RE = C_X.
    PERFORM ATUALIZA_ALV_EST.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_9004  INPUT

*&---------------------------------------------------------------------*
*&      Form  COPIAR_EST_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM COPIAR_EST_DRE .

  DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
        WA_SELECTED_ROWS TYPE LVC_S_ROW.

  CLEAR: WA_ZGL015_DRE_EST01_ALV.

  CALL METHOD DRE_ALV_EST->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  IF IT_SELECTED_ROWS IS INITIAL.
    MESSAGE 'Selecione uma estrutura!' TYPE 'S'.
  ELSE.
    READ TABLE IT_SELECTED_ROWS INDEX 1 INTO WA_SELECTED_ROWS.
    READ TABLE IT_ZGL015_DRE_EST01_ALV INTO WA_ZGL015_DRE_EST01_ALV INDEX WA_SELECTED_ROWS-INDEX.
    CALL SCREEN 9005 STARTING AT 10 10.
  ENDIF.

ENDFORM.                    " COPIAR_EST_DRE

*&---------------------------------------------------------------------*
*&      Form  APAGAR_EST_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APAGAR_EST_DRE .

  TABLES: ZGL021_DRE_DADOS, ZGL022_DRE_DADOS, ZGL023_DRE_DADOS, ZGL024_DRE_DADOS.
  DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
        WA_SELECTED_ROWS TYPE LVC_S_ROW,
        ANSWER           TYPE C LENGTH 1.

  CLEAR: WA_ZGL015_DRE_EST01_ALV.

  CALL METHOD DRE_ALV_EST->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECTED_ROWS.

  IF IT_SELECTED_ROWS IS INITIAL.
    MESSAGE 'Selecione uma estrutura!' TYPE 'S'.
  ELSE.
    READ TABLE IT_SELECTED_ROWS INDEX 1 INTO WA_SELECTED_ROWS.
    READ TABLE IT_ZGL015_DRE_EST01_ALV INTO WA_ZGL015_DRE_EST01_ALV INDEX WA_SELECTED_ROWS-INDEX.

    IF SY-SUBRC IS INITIAL.
      SELECT SINGLE *
        FROM ZGL021_DRE_DADOS
         WHERE BUKRS EQ WA_ZGL015_DRE_EST01_ALV-BUKRS
           AND VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.

      IF SY-SUBRC IS NOT INITIAL.
        SELECT SINGLE *
          FROM ZGL022_DRE_DADOS
          WHERE BUKRS EQ WA_ZGL015_DRE_EST01_ALV-BUKRS
            AND VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.

        IF SY-SUBRC IS NOT INITIAL.
          SELECT SINGLE *
            FROM ZGL023_DRE_DADOS
            WHERE BUKRS EQ WA_ZGL015_DRE_EST01_ALV-BUKRS
              AND VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.

          IF SY-SUBRC IS NOT INITIAL.
            SELECT SINGLE *
              FROM ZGL024_DRE_DADOS
              WHERE BUKRS EQ WA_ZGL015_DRE_EST01_ALV-BUKRS
               AND VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.


          ENDIF.
        ENDIF.
      ENDIF.


      IF SY-SUBRC IS NOT INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            TITEL     = 'Atenção!'
            TEXTLINE1 = 'A Estrutura será excluida!'
            TEXTLINE2 = 'Deseja realmente excluir?'
          IMPORTING
            ANSWER    = ANSWER.

        IF ANSWER EQ C_J.

          DELETE FROM ZGL015_DRE_EST01
           WHERE BUKRS EQ WA_ZGL015_DRE_EST01_ALV-BUKRS
             AND VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.

          DELETE FROM ZGL015_DRE_EST02
           WHERE BUKRS EQ WA_ZGL015_DRE_EST01_ALV-BUKRS
             AND VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.

          DELETE FROM ZGL015_DRE_EST03
           WHERE BUKRS EQ WA_ZGL015_DRE_EST01_ALV-BUKRS
             AND VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.

          DELETE FROM ZGL015_DRE_EST04
           WHERE BUKRS EQ WA_ZGL015_DRE_EST01_ALV-BUKRS
             AND VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.

          DELETE FROM ZGL015_DRE_EST05
           WHERE VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.

          DELETE FROM ZGL015_DRE_EST06
           WHERE BUKRS EQ WA_ZGL015_DRE_EST01_ALV-BUKRS
             AND VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.

          DELETE FROM ZGL015_DRE_EST07
           WHERE BUKRS EQ WA_ZGL015_DRE_EST01_ALV-BUKRS
             AND VERSN EQ WA_ZGL015_DRE_EST01_ALV-VERSN.

          PERFORM ATUALIZA_ALV_EST.

          MESSAGE 'Estrutura Excluida!' TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE 'Estrutura não pode ser Excluida!' TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " APAGAR_EST_DRE

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ERROS_CL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZGL015_DRE_EST01_VERSN  text
*----------------------------------------------------------------------*
FORM MOSTRAR_ERROS_CL  USING P_VERSN TYPE VERSN_011.

  DATA: ERROR_TEXT TYPE STRING,
        EXC_REF    TYPE REF TO CX_SY_NATIVE_SQL_ERROR,
        WA_ERRO    TYPE TY_MOTRA_ERRO.

  TRY.
      EXEC SQL.
        OPEN REGISTROS_CL FOR
          SELECT DR2.NIVEL, DR2.SAKNR, DR2.KOKRS, DR2.PRCTR
          FROM (SELECT DR2.VERSN, DR2.SAKNR, DR2.KOKRS, DR2.PRCTR
                   FROM SAPHANADB.ZGL015_DRE_EST05 DR2
                  WHERE VERSN = :P_VERSN
                  GROUP BY DR2.VERSN, DR2.SAKNR, DR2.KOKRS, DR2.PRCTR
                 HAVING COUNT(*) > 1 ) TT,
                 SAPHANADB.ZGL015_DRE_EST05 DR2
          WHERE TT.VERSN  = DR2.VERSN
             AND TT.SAKNR = DR2.SAKNR
             AND TT.KOKRS = DR2.KOKRS
             AND TT.PRCTR = DR2.PRCTR
      ENDEXEC.
    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
      ERROR_TEXT = EXC_REF->GET_TEXT( ).
      MESSAGE ERROR_TEXT TYPE 'E'.
  ENDTRY.

  DO.
    EXEC SQL.
      FETCH NEXT REGISTROS_CL INTO
      :WA_ERRO-NIVEL,
      :WA_ERRO-SAKNR,
      :WA_ERRO-KOKRS,
      :WA_ERRO-PRCTR.
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      APPEND WA_ERRO TO IT_MOTRA_ERRO.
    ENDIF.
  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ERROS_CC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZGL015_DRE_EST01_VERSN  text
*----------------------------------------------------------------------*
FORM MOSTRAR_ERROS_CC  USING P_VERSN TYPE VERSN_011.

  DATA: ERROR_TEXT TYPE STRING,
        EXC_REF    TYPE REF TO CX_SY_NATIVE_SQL_ERROR,
        WA_ERRO    TYPE TY_MOTRA_ERRO.

  TRY.
      EXEC SQL.
        OPEN REGISTROS_CC FOR
          SELECT DR2.NIVEL, DR2.SAKNR, DR2.KOSAR
          FROM (SELECT DR2.VERSN, DR2.SAKNR, DR2.KOSAR
                   FROM SAPHANADB.ZGL015_DRE_EST04 DR2
                  WHERE VERSN = :P_VERSN
                  GROUP BY DR2.VERSN, DR2.SAKNR, DR2.KOSAR
                 HAVING COUNT(*) > 1 ) TT,
                 SAPHANADB.ZGL015_DRE_EST04 DR2
          WHERE TT.VERSN  = DR2.VERSN
             AND TT.SAKNR = DR2.SAKNR
             AND TT.KOSAR = DR2.KOSAR
      ENDEXEC.
    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
      ERROR_TEXT = EXC_REF->GET_TEXT( ).
      MESSAGE ERROR_TEXT TYPE 'E'.
  ENDTRY.

  DO.
    EXEC SQL.
      FETCH NEXT REGISTROS_CC INTO
      :WA_ERRO-NIVEL,
      :WA_ERRO-SAKNR,
      :WA_ERRO-KOSAR.
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      APPEND WA_ERRO TO IT_MOTRA_ERRO.
    ENDIF.
  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ERROS_GM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZGL015_DRE_EST01_VERSN  text
*----------------------------------------------------------------------*
FORM MOSTRAR_ERROS_GM  USING P_VERSN TYPE VERSN_011.

  DATA: ERROR_TEXT TYPE STRING,
        EXC_REF    TYPE REF TO CX_SY_NATIVE_SQL_ERROR,
        WA_ERRO    TYPE TY_MOTRA_ERRO.

  TRY.
      EXEC SQL.
        OPEN REGISTROS_GM FOR
          SELECT DR2.NIVEL, DR2.SAKNR, DR2.MATKL
          FROM (SELECT DR2.VERSN, DR2.SAKNR, DR2.MATKL
                   FROM SAPHANADB.ZGL015_DRE_EST06 DR2
                  WHERE VERSN = :P_VERSN
                  GROUP BY DR2.VERSN, DR2.SAKNR, DR2.MATKL
                 HAVING COUNT(*) > 1 ) TT,
                 SAPHANADB.ZGL015_DRE_EST06 DR2
          WHERE TT.VERSN  = DR2.VERSN
             AND TT.SAKNR = DR2.SAKNR
             AND TT.MATKL = DR2.MATKL
      ENDEXEC.
    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
      ERROR_TEXT = EXC_REF->GET_TEXT( ).
      MESSAGE ERROR_TEXT TYPE 'E'.
  ENDTRY.

  DO.
    EXEC SQL.
      FETCH NEXT REGISTROS_GM INTO
      :WA_ERRO-NIVEL,
      :WA_ERRO-SAKNR,
      :WA_ERRO-MATKL.
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      APPEND WA_ERRO TO IT_MOTRA_ERRO.
    ENDIF.
  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ERROS_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOSTRAR_ERROS_OBJETOS .

  IF IT_MOTRA_ERRO[] IS INITIAL.
    MESSAGE S040.
  ENDIF.

  CHECK IT_MOTRA_ERRO[] IS NOT INITIAL.

  CALL SCREEN 9011 STARTING AT 05 05.

ENDFORM.
