*&---------------------------------------------------------------------*
*& Report  ZMMR148
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR148.

TABLES: ZMMT0101.

TYPES: BEGIN OF TY_SAIDA,
         MATRICULA  TYPE CHAR8, "ZMMT0101-MATRICULA,
         CPF        TYPE ZMMT0101-CPF,
         NOME       TYPE ZMMT0101-NOME,
         FILIAL     TYPE ZMMT0101-FILIAL,
         NAME1      TYPE T001W-NAME1,
         EMPRESA    TYPE ZMMT0101-EMPRESA,
         CARGO      TYPE ZMMT0101-CARGO,
         DATA_MODIF TYPE ZMMT0101-DATA_MODIF,
         HORA_MODIF TYPE ZMMT0101-HORA_MODIF,
         USNAME     TYPE ZMMT0101-USNAME,
       END OF TY_SAIDA.

DATA: IT_SAIDA    TYPE TABLE OF TY_SAIDA,
      WA_SAIDA    TYPE TY_SAIDA,
      WA_TELA     TYPE TY_SAIDA,
      IT_ZMMT0101 TYPE TABLE OF ZMMT0101,
      WA_ZMMT0101 TYPE ZMMT0101,
      IT_GRAVAR   TYPE TABLE OF ZMMT0101,
      WA_GRAVAR   TYPE ZMMT0101.


DATA: IT_FCAT   TYPE TABLE OF LVC_S_FCAT,
      WL_FCAT   TYPE LVC_S_FCAT,
      WA_STABLE TYPE LVC_S_STBL VALUE 'XX',
      WA_LAYOUT TYPE LVC_S_LAYO.

DATA: G_GRID               TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      TL_FUNCTION          TYPE UI_FUNCTIONS,
      WL_FUNCTION          LIKE TL_FUNCTION WITH HEADER LINE,
      TG_SELECTEDROW       TYPE LVC_T_ROW,
      WG_SELECTEDROW       TYPE LVC_S_ROW.

DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
      TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.


DATA WG_FLAG.
DATA NOVO.
DATA EDITAR.
DATA CPF_FORMAT TYPE  PBR99_CPF.
DATA CPF_RAW    TYPE  PBR99_CPF.
DATA CPF_NUMBER TYPE  PBR99_CPF.




INITIALIZATION.

  NOVO   =  ABAP_FALSE.
  EDITAR =  ABAP_FALSE.


START-OF-SELECTION.
  PERFORM BUSCA_DADOS.
  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  PERFORM Z_ALV.
  PERFORM Z_CRIA_ALV.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: VMATRICULA TYPE CHAR10.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&NOVO'.
      NOVO =  ABAP_TRUE.
    WHEN '&EDIT'.

      EDITAR = ABAP_TRUE.
      CALL METHOD G_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = TG_SELECTEDROW.

      IF TG_SELECTEDROW IS INITIAL.
        MESSAGE 'Favor selecione uma linha !' TYPE 'S'.
        EXIT.
      ELSE.

        READ TABLE TG_SELECTEDROW INTO WG_SELECTEDROW INDEX 1.

        READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WG_SELECTEDROW-INDEX.

        WA_TELA-MATRICULA = WA_SAIDA-MATRICULA.
        WA_TELA-CPF       = WA_SAIDA-CPF.
        WA_TELA-NOME      = WA_SAIDA-NOME.
        WA_TELA-FILIAL    = WA_SAIDA-FILIAL.
        WA_TELA-NAME1     = WA_SAIDA-NAME1.
        WA_TELA-EMPRESA   = WA_SAIDA-EMPRESA.
      ENDIF.

    WHEN '&DELE'.
      CALL METHOD G_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = TG_SELECTEDROW.

      IF TG_SELECTEDROW IS INITIAL.
        MESSAGE 'Favor selecione uma linha !' TYPE 'S'.
        EXIT.
      ELSE.

        LOOP AT TG_SELECTEDROW INTO WG_SELECTEDROW.

          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WG_SELECTEDROW-INDEX.

          DELETE FROM ZMMT0101 WHERE MATRICULA EQ WA_SAIDA-MATRICULA.

          CLEAR WA_SAIDA.

        ENDLOOP.

        MESSAGE 'Registro excluido com sucesso!' TYPE 'IS'.

        CLEAR WA_TELA.

        REFRESH IT_SAIDA.

        PERFORM BUSCA_DADOS.

        CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.
      ENDIF.

    WHEN 'SALVAR'.
      CLEAR: CPF_FORMAT, CPF_NUMBER, CPF_RAW.

      CPF_NUMBER  = WA_TELA-CPF.

      CALL FUNCTION 'HR_BR_CHECK_CPF_FORMAT'
        EXPORTING
          CPF_NUMBER               = CPF_NUMBER
        IMPORTING
          CPF_NUMBER_FORMATTED     = CPF_FORMAT
          CPF_UNFORMATTED          = CPF_RAW
        EXCEPTIONS
          CPF_FORMAT_NOT_SUPPORTED = 1
          CPF_CHECK_DIGIT          = 2
          OTHERS                   = 3.

      IF CPF_FORMAT IS INITIAL.
        MESSAGE 'CPF Invalido!' TYPE 'S'.
      ELSE.
        IF WA_TELA-MATRICULA IS INITIAL.
          CLEAR VMATRICULA.
          PERFORM GET_NEXT_NUMBER IN PROGRAM ZMMR148  USING  'ZMM_MATRIC'
                                                             '01' CHANGING VMATRICULA.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_TELA-MATRICULA
            IMPORTING
              OUTPUT = VMATRICULA.

        ENDIF.

        WA_GRAVAR-MANDT       =  SY-MANDT.
        WA_GRAVAR-MATRICULA   =  VMATRICULA+2(8).
        WA_GRAVAR-CPF         =  CPF_FORMAT.
        WA_GRAVAR-NOME        =  WA_TELA-NOME.
        WA_GRAVAR-FILIAL      =  WA_TELA-FILIAL.
        WA_GRAVAR-EMPRESA     =  WA_TELA-EMPRESA.
        WA_GRAVAR-CARGO       =  WA_TELA-CARGO.
        WA_GRAVAR-DATA_MODIF  =  SY-DATUM.
        WA_GRAVAR-HORA_MODIF  =  SY-UZEIT.
        WA_GRAVAR-USNAME      =  SY-UNAME.

        APPEND WA_GRAVAR TO IT_GRAVAR.

        MODIFY  ZMMT0101 FROM TABLE IT_GRAVAR.
        MESSAGE 'Dados Gravado com Sucesso!' TYPE 'S'.

        CLEAR: WA_TELA, NOVO, EDITAR.

        REFRESH IT_SAIDA.

        PERFORM BUSCA_DADOS.

        CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.

      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIED  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIED OUTPUT.

  LOOP AT SCREEN.

    IF NOVO =  ABAP_TRUE.
      IF SCREEN-NAME  = 'WA_TELA-MATRICULA'.
        SCREEN-INPUT  = 0.
        SCREEN-ACTIVE = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      SCREEN-INPUT  = 0.
      SCREEN-ACTIVE = 1.
      MODIFY SCREEN.
    ENDIF.


    IF EDITAR =  ABAP_TRUE.
      IF SCREEN-NAME  = 'WA_TELA-MATRICULA'.
        SCREEN-INPUT  = 0.
        SCREEN-ACTIVE = 1.
        MODIFY SCREEN.
      ELSE.
        SCREEN-INPUT  = 1.
        SCREEN-ACTIVE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
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
  PERFORM Z_PREENCHE_CAT USING:
        'MATRICULA'           'Matricula'       '08'    ''    ''    ''   '',
        'CPF'                 'CPF'             '11'    ''    ''    ''   '',
        'NOME'                'Nome'            '25'    ''    ''    ''   '',
        'CARGO'               'Cargo'           '25'    ''    ''    ''   '',
        'FILIAL'              'Filial'          '07'    ''    ''    ''   '',
        'NAME1'               'Descrição'       '20'    ''    ''    ''   '',
        'EMPRESA'             'Empresa'         '40'    ''    ''    ''   '',
        'DATA_MODIF'          'DT. Modif.'      '10'    ''    ''    ''   '',
        'HORA_MODIF'          'Hora Modif.'     '10'    ''    ''    ''   '',
        'USNAME'              'Usuário'         '10'    ''    ''    ''   ''.
ENDFORM.

FORM Z_PREENCHE_CAT USING VALUE(P_CAMPO)
                          VALUE(P_DESC)
                          VALUE(P_TAM)
                          VALUE(P_ZERO)
                          VALUE(P_HOT)
                          VALUE(P_SUM)
                          VALUE(P_JUST).

  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SCRTEXT_L = P_DESC.
  WL_FCAT-SCRTEXT_M = P_DESC.
  WL_FCAT-SCRTEXT_S = P_DESC.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-DO_SUM    = P_SUM.
  WL_FCAT-JUST      = P_JUST.

  APPEND WL_FCAT TO  IT_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_CRIA_ALV .

  IF G_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.


    CREATE OBJECT G_GRID
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER.

    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT
        I_SAVE                        = 'A'
      CHANGING
        IT_OUTTAB                     = IT_SAIDA[]
        IT_FIELDCATALOG               = IT_FCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.


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
    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS.

  SELECT *
    FROM ZMMT0101 INTO TABLE IT_ZMMT0101.

  CHECK  IT_ZMMT0101 IS NOT INITIAL.

  LOOP AT IT_ZMMT0101 INTO WA_ZMMT0101.
    WA_SAIDA-MATRICULA   = WA_ZMMT0101-MATRICULA.
    WA_SAIDA-CPF         = WA_ZMMT0101-CPF.
    WA_SAIDA-NOME        = WA_ZMMT0101-NOME.
    WA_SAIDA-FILIAL      = WA_ZMMT0101-FILIAL.
    WA_SAIDA-EMPRESA     = WA_ZMMT0101-EMPRESA.
    WA_SAIDA-CARGO       = WA_ZMMT0101-CARGO.
    WA_SAIDA-HORA_MODIF  = WA_ZMMT0101-HORA_MODIF.
    WA_SAIDA-DATA_MODIF  = WA_ZMMT0101-DATA_MODIF.
    WA_SAIDA-USNAME      = WA_ZMMT0101-USNAME.

    SELECT SINGLE NAME1 FROM T001W
      INTO WA_SAIDA-NAME1
     WHERE WERKS EQ WA_ZMMT0101-FILIAL.


    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR: WA_SAIDA, WA_ZMMT0101.
  ENDLOOP.

ENDFORM.


FORM GET_NEXT_NUMBER USING P_OBJECT
                           P_NR_RANGE
                    CHANGING P_NUMBER.

  CLEAR P_NUMBER.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = P_NR_RANGE
      OBJECT                  = P_OBJECT
    IMPORTING
      NUMBER                  = P_NUMBER
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
    CLEAR P_NUMBER.
    MESSAGE E836(SD) WITH 'O Intervalo de numeação não foi encontrado!'.
  ELSE.
    WG_FLAG = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_FILIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUSCA_FILIAL INPUT.

  DATA: BEGIN OF TL_FILIAL OCCURS 0,
          FILIAL TYPE T001W-WERKS,
          NAME   TYPE T001W-NAME1,
        END OF TL_FILIAL.

  SELECT WERKS NAME1 FROM T001W  INTO TABLE TL_FILIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'FILIAL'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'WA_TELA-FILIAL'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_FILIAL
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PREENCHE_FILIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PREENCHE_FILIAL INPUT.
  IF WA_TELA-FILIAL IS NOT INITIAL.

    SELECT SINGLE NAME1 FROM T001W
      INTO WA_TELA-NAME1
      WHERE WERKS EQ WA_TELA-FILIAL.
  ELSE.
    CLEAR  WA_TELA-NAME1.
  ENDIF.

ENDMODULE.
