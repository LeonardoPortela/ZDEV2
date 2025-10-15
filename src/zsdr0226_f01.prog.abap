*&---------------------------------------------------------------------*
*&  Include           ZFIR0065_FORM
*&---------------------------------------------------------------------*

FORM CRIAR_FIELD_CATALOG.

  FREE: WA_FCAT, IT_FCAT.

  PERFORM ESTRUTURA_ALV USING:

      0  '        '  'ICONE'            'IT_SAIDA' 'ICONE'            'Status'           '10' '' '' ' ' ' ' ' ' '' ' ' ' ',
      1  '        '  'BOTAO_MOTIVO'     'IT_SAIDA' 'BOTAO_MOTIVO'     'Motivo'           '10' '' '' ' ' ' ' ' ' '' ' ' ' ',
      2  'ZSDT0392'  'US_APROVADOR'     'IT_SAIDA' 'US_APROVADOR'     'Último Aprovador' '20' '' '' ' ' ' ' ' ' '' ' ' ' ',
      3  'ZSDT0392'  'DT_APROVACAO'     'IT_SAIDA' 'DT_APROVACAO'     'Data Aprovação'   '20' '' '' ' ' ' ' ' ' '' ' ' ' ',
      4  'ZSDT0392'  'HR_APROVACAO'     'IT_SAIDA' 'HR_APROVACAO'     'Hora Aprovação'   '20' '' '' ' ' ' ' ' ' '' ' ' ' ',
      5  '        '  'BOTAO_HISTORICO'  'IT_SAIDA' 'BOTAO_HISTORICO'  'Histórico'        '10' '' '' ' ' ' ' ' ' '' ' ' ' '.

ENDFORM.                    " CRIAR_FIELD_CATALOG_XML

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
                         VALUE(P_JUST)
                         VALUE(P_HOTSPOT)
                         VALUE(P_F4)
                         VALUE(P_CHECK)
                         VALUE(P_NO_OUT).

  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME   = P_FIELD.
  WA_FCAT-TABNAME     = P_TABNAME.
  WA_FCAT-REF_TABLE   = P_REF_TABNAME.
  WA_FCAT-REF_FIELD   = P_REF_FIELDNAME.
  WA_FCAT-KEY         = ' '.
  WA_FCAT-EDIT        = P_EDIT.
  WA_FCAT-COL_POS     = P_COL_POS.
  WA_FCAT-OUTPUTLEN   = P_OUTPUTLEN.
  WA_FCAT-NO_OUT      = P_NO_OUT.
  WA_FCAT-REPTEXT     = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_S   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_M   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_L   = P_SCRTEXT_L.
  WA_FCAT-EMPHASIZE   = P_EMPHASIZE.
  WA_FCAT-STYLE       =
  WA_FCAT-JUST        = P_JUST.
  WA_FCAT-HOTSPOT     = P_HOTSPOT.
  WA_FCAT-F4AVAILABL  = P_F4.
  WA_FCAT-CHECKBOX    = P_CHECK.

  IF WA_FCAT-FIELDNAME = 'ICONE'.
    WA_FCAT-HOTSPOT = 'X'.
  ENDIF.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    " ESTRUTURA_ALV

FORM BUILD_DROPDOWN .

ENDFORM.                    " BUILD_DROPDOWN

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .

  SELECT * FROM ZSDT0392 INTO TABLE IT_ZSDT0392.
  SORT IT_ZSDT0392 BY DT_APROVACAO HR_APROVACAO.
  IF IT_ZSDT0392 IS INITIAL.
    WA_STYLE-FIELDNAME = 'BOTAO_MOTIVO'.
    WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
    INSERT  WA_STYLE INTO TABLE STYLE.

    WA_STYLE-FIELDNAME = 'BOTAO_HISTORICO'.
    WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
    INSERT  WA_STYLE INTO TABLE STYLE.

    INSERT LINES OF STYLE INTO TABLE WA_SAIDA-STYLE.

    WA_SAIDA-ICONE  = '@06@'.
    WA_SAIDA-STATUS = 'D'.
    WA_SAIDA-EDIT   = 'X'.
    APPEND WA_SAIDA TO IT_SAIDA.
  ELSE.
    READ TABLE IT_ZSDT0392 INTO WA_ZSDT0392 INDEX 1.
    WA_STYLE-FIELDNAME = 'BOTAO_MOTIVO'.
    WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
    INSERT  WA_STYLE INTO TABLE STYLE.

    WA_STYLE-FIELDNAME = 'BOTAO_HISTORICO'.
    WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
    INSERT  WA_STYLE INTO TABLE STYLE.

    INSERT LINES OF STYLE INTO TABLE WA_SAIDA-STYLE.

    IF WA_ZSDT0392-STATUS = 'D'.
      WA_SAIDA-ICONE = '@06@'.
    ELSE.
      WA_SAIDA-ICONE = '@07@'.
    ENDIF.

    WA_SAIDA-US_APROVADOR = WA_ZSDT0392-US_APROVADOR.
    WA_SAIDA-DT_APROVACAO = WA_ZSDT0392-DT_APROVACAO.
    WA_SAIDA-HR_APROVACAO = WA_ZSDT0392-HR_APROVACAO.

    APPEND WA_SAIDA TO IT_SAIDA.
  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  DEL_TP_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEL_VALUE USING FIELD.

ENDFORM.                    " DEL_TP_DOC

FORM DEL_LAST_VALUE CHANGING P_VALUE.

ENDFORM.                    " DEL_TP_DOC
*&---------------------------------------------------------------------*
*&      Form  DELETAR_REG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETAR_REG.

ENDFORM.                    " DELETAR_REG
*&---------------------------------------------------------------------*
*&      Form  SALVAR_REG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALVAR_REG .

  IF IT_SAIDA[] IS NOT INITIAL.
    FREE: IT_ZSDT0393, IT_ZSDT0392, TL_TLINES.
    CLEAR: WA_ZSDT0393, TL_TLINES.
    SELECT SINGLE *
      FROM ZSDT0393
      WHERE DT_APROVACAO = ( SELECT MAX( DT_APROVACAO ) FROM ZSDT0393 )
      AND   HR_APROVACAO = ( SELECT MAX( HR_APROVACAO ) FROM ZSDT0393 WHERE DT_APROVACAO = ( SELECT MAX( DT_APROVACAO ) FROM ZSDT0393 ) )
      INTO @WA_ZSDT0393.

    SELECT SINGLE * FROM ZSDTVINC_APROV INTO @DATA(WA_ZSDTVINC_APROV) WHERE APROVADOR EQ @SY-UNAME.

    IF WA_ZSDTVINC_APROV IS INITIAL.
      MESSAGE sy-uname && ' não é um Usuário Aprovador' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    READ TABLE IT_SAIDA[] INTO IT_SAIDA INDEX 1.
    READ TABLE TG_TEXTO INTO WA_TEXTO INDEX 1.
    IF WA_TEXTO IS NOT INITIAL AND IT_SAIDA-EDIT IS NOT INITIAL AND ( IT_SAIDA-STATUS <> WA_ZSDT0393-STATUS ).

      IT_SAIDA-US_APROVADOR = SY-UNAME.
      IT_SAIDA-DT_APROVACAO = SY-DATUM.
      IT_SAIDA-HR_APROVACAO = SY-UZEIT.
      IT_SAIDA-ID_APROVACAO = WA_ZSDT0393-ID_APROVACAO + 1.
      IT_SAIDA-ID_SUBST     = '1'.
      MODIFY IT_SAIDA[] FROM IT_SAIDA INDEX 1.
      MOVE-CORRESPONDING IT_SAIDA[] TO IT_ZSDT0392.
      MOVE-CORRESPONDING IT_ZSDT0392 TO IT_ZSDT0393.
      LOOP AT IT_ZSDT0392 INTO WA_ZSDT0392.
        MODIFY ZSDT0392 FROM WA_ZSDT0392.
      ENDLOOP.
      LOOP AT IT_ZSDT0393 INTO DATA(LS_ZSDT0393).
        CLEAR: WA_MOTIVO.
        LOOP AT IT_MOTIVO INTO WA_MOTIVO.
          MOVE: '*'              TO TL_TLINES-TDFORMAT,
                WA_MOTIVO-TDLINE TO TL_TLINES-TDLINE.

          APPEND TL_TLINES.
          CLEAR TL_TLINES.

          WL_HEADER-TDNAME   = LS_ZSDT0393-MOTIVO.
          WL_HEADER-TDOBJECT = 'ZTEXTMOTI'.
          WL_HEADER-TDID     = 'MOTI'.
          WL_HEADER-TDSPRAS  = SY-LANGU.

          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              HEADER          = WL_HEADER
              SAVEMODE_DIRECT = 'X'
            TABLES
              LINES           = TL_TLINES
            EXCEPTIONS
              ID              = 1
              LANGUAGE        = 2
              NAME            = 3
              OBJECT          = 4
              OTHERS          = 5.

          IF SY-SUBRC IS INITIAL.
            COMMIT WORK.
          ELSE.
            MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            EXIT.
          ENDIF.
        ENDLOOP.


        INSERT ZSDT0393 FROM LS_ZSDT0393.
      ENDLOOP.
      IF SY-SUBRC IS INITIAL.
        COMMIT WORK.
        MESSAGE 'Informações atualizadas.' TYPE 'S'.
      ELSE.
        ROLLBACK WORK.
        MESSAGE 'Não foi possível realizar a alteração.' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.


      CLEAR: IT_SAIDA-EDIT.
      MODIFY IT_SAIDA[] FROM IT_SAIDA INDEX 1.

      OBJ_ALV->REFRESH_TABLE_DISPLAY( ).
    ELSEIF WA_TEXTO IS INITIAL.
      MESSAGE 'Obrigatório o preenchimento do motivo' TYPE 'S' DISPLAY LIKE 'E'.
    ELSEIF IT_SAIDA-EDIT IS INITIAL OR ( IT_SAIDA-STATUS = WA_ZSDT0393-STATUS ).
      MESSAGE 'Não houve alteração do status.' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.                    " SALVAR_REG

FORM F_EXIBIR_HIST.
  FREE: IT_ZSDT0393.
  SELECT * FROM ZSDT0393 INTO TABLE IT_ZSDT0393.
  MOVE-CORRESPONDING IT_ZSDT0393 TO IT_ZSDT0393_AUX.
  FREE: IT_ZSDT0393.
  SORT IT_ZSDT0393_AUX BY ID_APROVACAO DESCENDING.

  PERFORM CRIAR_BOTAO_HIST.

  PERFORM DISPLAY_TABLE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_TABLE
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM DISPLAY_TABLE .

  IF GR_DIALOG_CONTAINER IS INITIAL.

    " Criar janela modal pop-up
    CREATE OBJECT GR_DIALOG_CONTAINER
      EXPORTING
        CAPTION = 'Histórico de Aprovações'
        TOP     = 100
        LEFT    = 100
        WIDTH   = 700
        HEIGHT  = 300
      EXCEPTIONS
        OTHERS  = 1.
    IF SY-SUBRC <> 0.
      MESSAGE 'Erro ao criar container de diálogo' TYPE 'E'.
    ENDIF.

    " Criar ALV dentro do pop-up
    CREATE OBJECT GR_GRID
      EXPORTING
        I_PARENT = GR_DIALOG_CONTAINER.

    " Preparar estrutura e layout
    PERFORM PREPAR_CATALOG.
    PERFORM PREPAR_LAYOUT.

    CREATE OBJECT GR_HANDLER.
    SET HANDLER GR_HANDLER->ON_CLOSE FOR GR_DIALOG_CONTAINER.

    SET HANDLER: OBJ_TOOLBAR->ON_TOOLBAR          FOR GR_GRID,
                 OBJ_TOOLBAR->HANDLE_USER_COMMAND FOR GR_GRID,
                 OBJ_TOOLBAR->HANDLE_DATA_CHANGED FOR GR_GRID,
                 LCL_EVENT_HANDLER=>HANDLE_BUTTON_CLICK   FOR GR_GRID,
                 OBJ_TOOLBAR->ON_HOTSPOT_CLICK FOR GR_GRID.

    " Exibir dados
    CALL METHOD GR_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = LS_LAYOUT
        I_SAVE          = 'A'
        IS_VARIANT      = GS_VARIANT
        I_DEFAULT       = 'X'
      CHANGING
        IT_FIELDCATALOG = IT_ESTRUTURA
        IT_OUTTAB       = IT_ZSDT0393_AUX
      EXCEPTIONS
        OTHERS          = 1.

  ELSE.
    CALL METHOD GR_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.


*  IF CUSTOM_CONTAINER IS INITIAL.
** create container
*    CREATE OBJECT CUSTOM_CONTAINER
*      EXPORTING
*        CONTAINER_NAME              = MYCONTAINER
*      EXCEPTIONS
*        CNTL_ERROR                  = 1
*        CNTL_SYSTEM_ERROR           = 2
*        CREATE_ERROR                = 3
*        LIFETIME_ERROR              = 4
*        LIFETIME_DYNPRO_DYNPRO_LINK = 5.
*    IF SY-SUBRC <> 0.
*      MESSAGE ' erreur container' TYPE 'I'.
*    ENDIF.
** create alv grid
*    CREATE OBJECT GR_GRID
*      EXPORTING
*        I_PARENT          = CUSTOM_CONTAINER
*      EXCEPTIONS
*        ERROR_CNTL_CREATE = 1
*        ERROR_CNTL_INIT   = 2
*        ERROR_CNTL_LINK   = 3
*        ERROR_DP_CREATE   = 4
*        OTHERS            = 5.
*    IF SY-SUBRC <> 0.
*      MESSAGE ' erreur grid' TYPE 'I'.
*    ENDIF.
*
** prepare fielfcatalog
*    PERFORM PREPAR_CATALOG.
** prepare layout
*    PERFORM PREPAR_LAYOUT.
** fist display
*    PERFORM FIRST_DISPLAY.
*
*  ELSE .
*    CALL METHOD GR_GRID->REFRESH_TABLE_DISPLAY
*      EXCEPTIONS
*        FINISHED = 1
*        OTHERS   = 2.
*  ENDIF.


ENDFORM. " DISPLAY_TABLE
*&---------------------------------------------------------------------*
*& Form PREPAR_CATALOG
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM PREPAR_CATALOG .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-REPID
      I_INTERNAL_TABNAME     = 'ITAB'
      I_CLIENT_NEVER_DISPLAY = 'X'
      I_INCLNAME             = SY-REPID
    CHANGING
      CT_FIELDCAT            = LT_FIELDCAT_SLIS.

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      IT_FIELDCAT_ALV = LT_FIELDCAT_SLIS
    IMPORTING
      ET_FIELDCAT_LVC = IT_ESTRUTURA[]
    TABLES
      IT_DATA         = IT_ZSDT0393
    EXCEPTIONS
      IT_DATA_MISSING = 1
      OTHERS          = 2.


ENDFORM. " PREPAR_CATALOG
*&---------------------------------------------------------------------*
*& Form PREPAR_LAYOUT
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM PREPAR_LAYOUT .

  LS_LAYOUT-ZEBRA = 'X' .
  LS_LAYOUT-SMALLTITLE = 'X'.
  LS_LAYOUT-STYLEFNAME = 'STYLE'.

  PERFORM MONTAR_LAYOUT_LOG_HIST.

ENDFORM. " PREPAR_LAYOUT
*&---------------------------------------------------------------------*
*& Form FIRST_DISPLAY
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM FIRST_DISPLAY .

  CALL METHOD GR_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = LS_LAYOUT
      I_SAVE                        = 'A'
      IS_VARIANT                    = GS_VARIANT
      I_DEFAULT                     = 'X'
    CHANGING
      IT_FIELDCATALOG               = IT_ESTRUTURA[]
      IT_OUTTAB                     = IT_ZSDT0393_AUX[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

ENDFORM. " FIRST_DISPLAY

FORM MONTAR_LAYOUT_LOG_HIST.
  REFRESH IT_ESTRUTURA[].
  PERFORM MONTAR_ESTRUTURA USING:
     1  ''   ''            'IT_ZSDT0393_AUX' 'ID_APROVACAO'     'ID Aprovação'       '14' '' '' ' ' ' ' ' ' '' ' ' ' ',
     2  ''   ''            'IT_ZSDT0393_AUX' 'STATUS'           'Status'             '10' '' '' ' ' ' ' ' ' '' ' ' ' ',
     3  ''   ''            'IT_ZSDT0393_AUX' 'BOTAO_MOTIVO_HISTORICO'     'Motivo'             '10' '' '' ' ' ' ' ' ' '' ' ' ' ',
     4  ''   ''            'IT_ZSDT0393_AUX' 'US_APROVADOR'     'Usuário Aprovador'  '16' '' '' ' ' ' ' ' ' '' ' ' ' ',
     5  ''   ''            'IT_ZSDT0393_AUX' 'DT_APROVACAO'     'Data da Aprovação'  '16' '' '' ' ' ' ' ' ' '' ' ' ' ',
     6  ''   ''            'IT_ZSDT0393_AUX' 'HR_APROVACAO'     'Hora da Aprovação'  '12' '' '' ' ' ' ' ' ' '' ' ' ' '.
ENDFORM.

FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                         VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                         VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                         VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                         VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                         VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                         VALUE(P_OUTPUTLEN)
                         VALUE(P_EDIT)
                         VALUE(P_SUM)
                         VALUE(P_EMPHASIZE)
                         VALUE(P_JUST)
                         VALUE(P_HOTSPOT)
                         VALUE(P_F4)
                         VALUE(P_CHECK)
                         VALUE(P_NO_OUT).

  CLEAR WA_FCAT.

  WA_ESTRUTURA-FIELDNAME   = P_FIELD.
  WA_ESTRUTURA-TABNAME     = P_TABNAME.
  WA_ESTRUTURA-REF_TABLE   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELD   = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY         = ' '.
  WA_ESTRUTURA-EDIT        = P_EDIT.
  WA_ESTRUTURA-COL_POS     = P_COL_POS.
  WA_ESTRUTURA-OUTPUTLEN   = P_OUTPUTLEN.
  WA_ESTRUTURA-NO_OUT      = P_NO_OUT.
  WA_ESTRUTURA-REPTEXT     = P_SCRTEXT_L.
  WA_ESTRUTURA-SCRTEXT_S   = P_SCRTEXT_L.
  WA_ESTRUTURA-SCRTEXT_M   = P_SCRTEXT_L.
  WA_ESTRUTURA-SCRTEXT_L   = P_SCRTEXT_L.
  WA_ESTRUTURA-EMPHASIZE   = P_EMPHASIZE.
  WA_ESTRUTURA-STYLE       =
  WA_ESTRUTURA-JUST        = P_JUST.
  WA_ESTRUTURA-HOTSPOT     = P_HOTSPOT.
  WA_ESTRUTURA-F4AVAILABL  = P_F4.
  WA_ESTRUTURA-CHECKBOX    = P_CHECK.

  APPEND WA_ESTRUTURA TO IT_ESTRUTURA.

ENDFORM.                    " montar_estrutura

FORM CRIAR_BOTAO_HIST.
  LOOP AT IT_ZSDT0393_AUX INTO WA_ZSDT0393_AUX.
    CLEAR: WA_STYLE.
    FREE: STYLE.

    WA_STYLE-FIELDNAME = 'BOTAO_MOTIVO_HISTORICO'.
    WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
    INSERT  WA_STYLE INTO TABLE STYLE.

    INSERT LINES OF STYLE INTO TABLE WA_ZSDT0393_AUX-STYLE.

    MODIFY IT_ZSDT0393_AUX FROM WA_ZSDT0393_AUX.
  ENDLOOP.
ENDFORM.

FORM F_EXIBIR_MOTIVO USING P_ES_ROW_NO TYPE LVC_S_ROID
                           C_X         TYPE C.

  CLEAR: WL_NAME, WA_TEXTO, LT_LINES.
  IF <FS_ZSDT0393> IS ASSIGNED.
    CLEAR: <FS_ZSDT0393>.
  ENDIF.

  SELECT * FROM ZSDT0393 INTO TABLE IT_ZSDT0393.
  SORT IT_ZSDT0393 BY ID_APROVACAO DESCENDING.

  IF IT_ZSDT0393 IS NOT INITIAL.
    IF C_X IS INITIAL AND TG_TEXTO IS INITIAL.
      READ TABLE IT_ZSDT0393 ASSIGNING <FS_ZSDT0393> INDEX P_ES_ROW_NO-ROW_ID.
      WL_NAME = <FS_ZSDT0393>-MOTIVO.
    ELSEIF C_X IS NOT INITIAL.
      READ TABLE IT_ZSDT0393 ASSIGNING <FS_ZSDT0393> INDEX 1.
      WL_NAME = <FS_ZSDT0393>-MOTIVO.
    ELSEIF TG_TEXTO IS NOT INITIAL.
      READ TABLE IT_ZSDT0393 ASSIGNING <FS_ZSDT0393> INDEX P_ES_ROW_NO-ROW_ID.
      WL_NAME = <FS_ZSDT0393>-MOTIVO.
    ENDIF.
  ELSE.
    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX 1.
    WL_NAME = WA_SAIDA-MOTIVO.
  ENDIF.
  IF WL_NAME IS NOT INITIAL.

    PERFORM F_READ_TEXT USING WL_NAME CHANGING LT_LINES.

    IF LT_LINES IS NOT INITIAL.
      LOOP AT lt_LINES ASSIGNING FIELD-SYMBOL(<FS_LINES>).
        APPEND INITIAL LINE TO TG_TEXTO ASSIGNING FIELD-SYMBOL(<FS_TEXTO>).
        <FS_TEXTO> = <FS_LINES>-TDLINE.
      ENDLOOP.
    ENDIF.
  ENDIF.



  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      IM_TITLE        = 'Motivo'           "" Título
      IM_DISPLAY_MODE = WL_DISPLAY
    CHANGING
      CH_TEXT         = TG_TEXTO.

ENDFORM.

FORM F_FECHAR_HIST.
  IF GR_GRID IS BOUND.
    CALL METHOD GR_GRID->FREE.
    FREE GR_GRID.
  ENDIF.

  IF GR_DIALOG_CONTAINER IS BOUND.
    CALL METHOD GR_DIALOG_CONTAINER->FREE.
    FREE GR_DIALOG_CONTAINER.
  ENDIF.

  IF GR_HANDLER IS BOUND.
    FREE GR_HANDLER.
  ENDIF.

ENDFORM.

FORM F_READ_TEXT USING WL_NAME TYPE THEAD-TDNAME CHANGING LT_LINES TYPE TY_TLINE_TAB.
  FREE: TG_TEXTO_AUX.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = 'MOTI'
      LANGUAGE                = SY-LANGU
      NAME                    = WL_NAME
      OBJECT                  = 'ZTEXTMOTI'
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF LT_LINES IS NOT INITIAL.
    LOOP AT lt_LINES ASSIGNING FIELD-SYMBOL(<FS_LINES_AUX>).
      APPEND INITIAL LINE TO TG_TEXTO_AUX ASSIGNING FIELD-SYMBOL(<FS_TEXTO_AUX>).
      <FS_TEXTO_AUX> = <FS_LINES_AUX>-TDLINE.
    ENDLOOP.

    READ TABLE TG_TEXTO_AUX INTO DATA(WL_TEXTO_AUX) INDEX 1.
    READ TABLE TG_TEXTO INTO DATA(WL_TEXTO) INDEX 1.
    IF WL_TEXTO_AUX = WL_TEXTO AND WL_TEXTO IS NOT INITIAL AND WL_DISPLAY IS INITIAL.
      FREE: TG_TEXTO, LT_LINES.
    ELSEIF WL_TEXTO_AUX <> WL_TEXTO AND WL_TEXTO IS NOT INITIAL AND WL_DISPLAY IS INITIAL.
      FREE: LT_LINES.
    ELSEIF ( WL_TEXTO_AUX <> WL_TEXTO AND WL_TEXTO IS NOT INITIAL AND WL_DISPLAY IS NOT INITIAL ) OR ( WL_TEXTO_AUX = WL_TEXTO AND WL_TEXTO IS NOT INITIAL AND WL_DISPLAY IS NOT INITIAL ).
      FREE: TG_TEXTO.
    ENDIF.

    FREE: TG_TEXTO_AUX.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_6001
*&---------------------------------------------------------------------*
FORM MONTA_POP_6001.

  "pelo numero da tela colocar ícones e legendas

  DATA: WA_LEGENDA TYPE TY_LEGENDA.

  CLEAR: IT_LEGENDA.

  CLEAR WA_LEGENDA.
  WA_LEGENDA-ICONE = '@07@'.
  WA_LEGENDA-DESCR = 'Ativado para aprovação manual.'.
  APPEND WA_LEGENDA TO IT_LEGENDA.

  CLEAR WA_LEGENDA.
  WA_LEGENDA-ICONE = '@06@'.
  WA_LEGENDA-DESCR = 'Desativado para aprovação manual (aprovação automática).'.
  APPEND WA_LEGENDA TO IT_LEGENDA.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP_6001
*&---------------------------------------------------------------------*
FORM MOSTRA_POP_6001.

  IF G_CUSTOM_CONTAINER_POP_6001 IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER_POP_6001
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER6001'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    FREE: IT_FCAT.
    PERFORM ESTRUTURA_ALV USING:
          0  '' 'ICONE' 'IT_LEGENDA' 'ICONE' 'Icone'     '10' '' '' ' ' ' ' ' ' '' ' ' '',
          1  '' 'DESCR' 'IT_LEGENDA' 'DESCR' 'Descrição' '10' '' '' ' ' ' ' ' ' '' ' ' ''.

    GS_LAYOUT_POP_6001-CWIDTH_OPT = 'X'.

    CREATE OBJECT CTL_ALV1_POP_6001
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER_POP_6001.           "ALV Lote

    CALL METHOD CTL_ALV1_POP_6001->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT_POP_6001
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FCAT
        IT_OUTTAB       = IT_LEGENDA.

  ELSE.
    CALL METHOD CTL_ALV1_POP_6001->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = _STABLE.
  ENDIF.

ENDFORM.
