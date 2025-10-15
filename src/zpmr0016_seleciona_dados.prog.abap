*&----------------------------------------------------------------------------*
*& CLASS SELECIONA_DADOS DEFINITION                                           *
*& AUTOR: ENIO JESUS                                                          *
*& 15.07.2015                                                                 *
*&----------------------------------------------------------------------------*
CLASS Z_SELECIONA_DADOS DEFINITION.
  PUBLIC SECTION.
*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0110                                       *
*&----------------------------------------------------------------------------*
    METHODS: Z_SELECIONA_DADOS_TELA_0110.
    METHODS: TBX_BUSCA_EQUIPM_DISPONIVEIS.
    METHODS: TBX_BUSCA_CENTRO_DISPONIVEIS.

*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0120                                       *
*&----------------------------------------------------------------------------*
    METHODS: Z_SELECIONA_DADOS_TELA_0120.
    METHODS: TBX_BUSCA_CENTRO_EMPRESTADOS.

    METHODS: TBX_BUSCA_EQUIPM_EMPRESTADOS.

*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0130                                       *
*&----------------------------------------------------------------------------*
    METHODS  Z_SELECIONA_DADOS_TELA_0130.
    METHODS: TBX_BUSCA_CENTRO_RESPONSAVEL.

*&----------------------------------------------------------------------------*
*& METHOD Z_AUTHORITY_CHECK                                                   *
*&----------------------------------------------------------------------------*
    METHODS: Z_AUTHORITY_CHECK  IMPORTING
                                        OBJECT TYPE CHAR7
                                            ID TYPE CHAR5
                                         FIELD TYPE SWERK
                                        RETURN TYPE CHAR1.

*&----------------------------------------------------------------------------*
*& METHOD Z_STATUS_EQUIPAMENTO                                                *
*&----------------------------------------------------------------------------*
    METHODS: Z_STATUS_EQUIPAMENTO  IMPORTING
                                     EQUIPMENT TYPE EQUNR.

    METHODS: Z_DETALHES_EQUIPAMENTO IMPORTING
                                     EQUIPMENT TYPE EQUNR.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_EQUI_EMPRESTADO                                     *
*&---------------------------------------------------------------------*
    METHODS: Z_CHECAR_EQUI_EMPRESTADO IMPORTING
                                           MSG TYPE CHAR1
                                        RETURN TYPE CHAR1.

*&---------------------------------------------------------------------*
*& METHOD Z_ATUALIZA_TELA_POS_EMPRESTIMO                               *
*&---------------------------------------------------------------------*
    METHODS: Z_ATUALIZA_TELA_EMPRESTIMO.


  PRIVATE SECTION.
    DATA: WA_DATA_GENERAL         TYPE BAPI_ITOB,
          WA_DATA_SPECIFIC_EXP    TYPE BAPI_ITOB_EQ_ONLY,
          WA_RETURN               TYPE BAPIRET2,
          AT_RETURN               TYPE SY-SUBRC.

ENDCLASS.                    "Z_SELECIONA_DADOS DEFINITION

*&---------------------------------------------------------------------*
*& CLASS SELECIONA_DADOS IMPLEMENTATION                                *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

CLASS Z_SELECIONA_DADOS IMPLEMENTATION.

************************************************************************
*& Descrição: Método de seleção sub-tela 0110                         &*
*& Este é o método principal, pois chama-se a partir daqui os métodos &*
*& de pesquisa por nº de equipamento e centro.                        &*
**********************************************************************&*

  METHOD: Z_SELECIONA_DADOS_TELA_0110.
    CLEAR: IT_EQUI, IT_SAIDA_EQUI_DISPONIVEIS, IT_ZEQUI_EMPRESTIMO.

*   Busca pelo nº do equipamento

    IF TBX_BUSC_EQUIPAMENTO IS NOT INITIAL.
      TBX_BUSCA_EQUIPM_DISPONIVEIS( ).

*   Busca pelo nº do centro

    ELSEIF TBX_BUSC_CENTRO IS NOT INITIAL.
      TBX_BUSCA_CENTRO_DISPONIVEIS( ).
    ENDIF.

*   Pegar descrição do equipamento.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_EQKT
      FROM EQKT
   FOR ALL ENTRIES IN IT_EQUI
     WHERE EQUNR = IT_EQUI-EQUNR.

    LOOP AT IT_EQUI INTO WA_EQUI.

*    Pegar centro origem do eqpto.

      CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
        EXPORTING
          EQUIPMENT        = WA_EQUI-EQUNR
        IMPORTING
          DATA_GENERAL_EXP = WA_DATA_GENERAL.

*     Verifica se o usuário possuí permissão para a pesquisa informada.

      Z_AUTHORITY_CHECK( OBJECT = 'I_SWERK'
                         ID     = 'SWERK'
                         FIELD  = WA_DATA_GENERAL-MAINTPLANT
                         RETURN = RETURN_STATUS ).

      IF RETURN_STATUS IS INITIAL.

        READ TABLE IT_EQKT INTO WA_EQKT WITH KEY EQUNR = WA_EQUI-EQUNR.

        WA_SAIDA_EQUI_DISPONIVEIS-EQKTX = WA_EQKT-EQKTX.
        WA_SAIDA_EQUI_DISPONIVEIS-EQUNR = WA_EQUI-EQUNR.
        WA_SAIDA_EQUI_DISPONIVEIS-IWERK = WA_DATA_GENERAL-MAINTPLANT.
        APPEND WA_SAIDA_EQUI_DISPONIVEIS TO IT_SAIDA_EQUI_DISPONIVEIS.

      ELSE.
        MESSAGE S836(SD) WITH TEXT-031 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR: WA_SAIDA_EQUI_DISPONIVEIS, WA_EQKT.

  ENDMETHOD.                    "Z_BUSCA_EQUIPAMENTO

************************************************************************
*& Descrição: Text-box de pesquisa por nº de eqpto sub-tela 0110      &*
*& Atributos Globais                                                  &*
**********************************************************************&*
  METHOD: TBX_BUSCA_EQUIPM_DISPONIVEIS.
    CLEAR IT_SAIDA_EQUI_DISPONIVEIS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = TBX_BUSC_EQUIPAMENTO
      IMPORTING
        OUTPUT = TBX_BUSC_EQUIPAMENTO.

    SELECT *
      FROM ZEQUI_EMPRESTIMO
      INTO TABLE IT_ZEQUI_EMPRESTIMO.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_EQUI
      FROM EQUI AS A
     INNER JOIN JEST AS B ON A~OBJNR = B~OBJNR
     WHERE A~EQUNR EQ TBX_BUSC_EQUIPAMENTO
       AND A~EQTYP IN ('V','F','U')
       AND B~STAT  NE 'I0076'
       AND B~STAT  NE 'I0320'
       AND B~INACT NE 'X'.

*   No método seguinte, é passado dois parâmetros de mensagem de erro, em caso do equipamento
*   estiver emprestado, o mesmo retorna um atributo return_status como 'x' ou ''.
*   sendo 'x' que o eqpto está emprestado e '' que está disponível.

    Z_CHECAR_EQUI_EMPRESTADO(    MSG   = 'X'
                              RETURN   = RETURN_STATUS ).

    CHECK RETURN_STATUS IS INITIAL.
    Z_STATUS_EQUIPAMENTO( EQUIPMENT = TBX_BUSC_EQUIPAMENTO ).
    Z_DETALHES_EQUIPAMENTO( EQUIPMENT = TBX_BUSC_EQUIPAMENTO ).

  ENDMETHOD.                    "Z_TBX_BUSCA_POR_EQUIPAMENTO

************************************************************************
*& Descrição: Text-box de pesquisa por nº do centro sub-tela 0110     &*
*& Atributos Globais                                                  &*
**********************************************************************&*
  METHOD: TBX_BUSCA_CENTRO_DISPONIVEIS.
    CLEAR IT_SAIDA_EQUI_DISPONIVEIS.

    SELECT *
      FROM ZEQUI_EMPRESTIMO
      INTO TABLE IT_ZEQUI_EMPRESTIMO.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_EQUI
      FROM EQUI AS A
     INNER JOIN ITOB AS B ON A~EQUNR = B~EQUNR
     INNER JOIN JEST AS C ON A~OBJNR = C~OBJNR
     WHERE A~EQTYP IN ('V','F','U')
       AND B~DATBI EQ '99991231'
       AND B~SWERK EQ TBX_BUSC_CENTRO
       AND C~STAT  NE 'I0076'
       AND C~STAT  NE 'I0320'
       AND C~INACT NE 'X'.

    IF SY-SUBRC IS NOT INITIAL.
      CHECK ITABSTRIP-ACTIVETAB = 'TAB_DISPONIVEL'.
      MESSAGE S836(SD) WITH TEXT-040 TEXT-041 DISPLAY LIKE 'E'.

    ELSE.

      SORT IT_EQUI.
      DELETE ADJACENT DUPLICATES FROM IT_EQUI.

*   Deletar os equipamentos da tabela de saída que estão emprestados.

      Z_CHECAR_EQUI_EMPRESTADO(    MSG   = ' '
                                RETURN   = RETURN_STATUS ).

    ENDIF.

  ENDMETHOD.                    "TBX_BUSC_POR_CENTRO

************************************************************************
*& Descrição: Método de seleção sub-tela 0120                         &*
*& Este é o método principal, pois chama-se a partir daqui os métodos &*
*& de pesquisa por equipamento e centro de custo.                     &*
**********************************************************************&*
  METHOD Z_SELECIONA_DADOS_TELA_0120.

    CLEAR: IT_SAIDA_EQUI_EMPRESTADOS, IT_ZEQUI_EMPRESTIMO.

*   Busca pelo nº do equipamento

    IF TBX_BUSC_EQUIPAMENTO IS NOT INITIAL.
      TBX_BUSCA_EQUIPM_EMPRESTADOS( ).

*   Busca pelo nº do centro

    ELSEIF TBX_BUSC_CENTRO IS NOT INITIAL.
      TBX_BUSCA_CENTRO_EMPRESTADOS( ).
    ENDIF.

    LOOP AT IT_ZEQUI_EMPRESTIMO INTO WA_ZEQUI_EMPRESTIMO.

*   Verifica se o usuário possuí permissão para a pesquisa informada.

      Z_AUTHORITY_CHECK( OBJECT = 'I_SWERK'
                         ID     = 'SWERK'
                         FIELD  = WA_ZEQUI_EMPRESTIMO-SWERK
                         RETURN = RETURN_STATUS ).

      IF RETURN_STATUS IS INITIAL.

        WA_SAIDA_EQUI_EMPRESTADOS-EQUNR       = WA_ZEQUI_EMPRESTIMO-EQUNR.
        WA_SAIDA_EQUI_EMPRESTADOS-SWERK       = WA_ZEQUI_EMPRESTIMO-SWERK.
        WA_SAIDA_EQUI_EMPRESTADOS-IWERK       = WA_ZEQUI_EMPRESTIMO-IWERK.
        WA_SAIDA_EQUI_EMPRESTADOS-QT_DIAS     = WA_ZEQUI_EMPRESTIMO-QT_DIAS.
        WA_SAIDA_EQUI_EMPRESTADOS-UNAME       = WA_ZEQUI_EMPRESTIMO-UNAME.
        WA_SAIDA_EQUI_EMPRESTADOS-ERDAT       = WA_ZEQUI_EMPRESTIMO-ERDAT.
        WA_SAIDA_EQUI_EMPRESTADOS-EQKTX       = WA_ZEQUI_EMPRESTIMO-EQKTX.
        APPEND WA_SAIDA_EQUI_EMPRESTADOS TO IT_SAIDA_EQUI_EMPRESTADOS.

      ELSE.
        MESSAGE S836(SD) WITH TEXT-031 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR: WA_SAIDA_EQUI_EMPRESTADOS, WA_ZEQUI_EMPRESTIMO.

  ENDMETHOD.                    "Z_SELECIONA_DADOS_SCREEN_0120

************************************************************************
*& Descrição: Text-box de pesquisa por nº do eqpto sub-tela 0120      &*
*& Atributos Globais                                                  &*
**********************************************************************&*
  METHOD: TBX_BUSCA_EQUIPM_EMPRESTADOS.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = TBX_BUSC_EQUIPAMENTO
      IMPORTING
        OUTPUT = TBX_BUSC_EQUIPAMENTO.

    SELECT *
      FROM ZEQUI_EMPRESTIMO
      INTO TABLE IT_ZEQUI_EMPRESTIMO
     WHERE EQUNR EQ TBX_BUSC_EQUIPAMENTO.

    CHECK SY-SUBRC IS NOT INITIAL.

*   Emitir mensagem de erro apenas se estiver na aba referênte aos
*   equipamentos emprestados da sub-tela 0120.

    IF ITABSTRIP-ACTIVETAB     = 'TAB_EMPRESTADOS'.
      MESSAGE S836(SD) WITH TEXT-010 TEXT-034 DISPLAY LIKE 'W'.

*   Emitir mensagem de erro apenas se estiver na aba referênte aos
*   equipamentos emprestados da sub-tela 0130.

    ELSEIF ITABSTRIP-ACTIVETAB = 'TAB_RESPONSAVEL'.
      MESSAGE S836(SD) WITH TEXT-010 TEXT-035 DISPLAY LIKE 'W'.
    ENDIF.

*    ITABSTRIP-ACTIVETAB = 'TAB_DISPONIVEL'.

  ENDMETHOD.                    "Z_TBX_BUSCA_POR_EQUIPAMENTO

************************************************************************
*& Descrição: Text-box de pesquisa por nº do centro sub-tela 0120     &*
*& Atributos Globais                                                  &*
**********************************************************************&*
  METHOD: TBX_BUSCA_CENTRO_EMPRESTADOS.

    SELECT *
      FROM ZEQUI_EMPRESTIMO AS A
      INTO TABLE IT_ZEQUI_EMPRESTIMO
     WHERE SWERK EQ TBX_BUSC_CENTRO.

    CHECK SY-SUBRC IS NOT INITIAL.

*   Emitir mensagem de erro apenas se estiver na aba referênte aos
*   equipamentos emprestados da sub-tela 0120.

    IF ITABSTRIP-ACTIVETAB = 'TAB_EMPRESTADOS'.
      MESSAGE S836(SD) WITH TEXT-042 TEXT-041 DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.                    "TBX_BUSCA_POR_CENTRO_0120

************************************************************************
*& Descrição: Método de seleção sub-tela 0130                         &*
*& Este é o método principal, pois chama-se a partir daqui os métodos &*
*& de pesquisa por equipamento e centro de custo.                     &*
**********************************************************************&*
  METHOD Z_SELECIONA_DADOS_TELA_0130.

    CLEAR: IT_SAIDA_EQUI_RESPONSAVEL, IT_ZEQUI_EMPRESTIMO.

*    IF IT_SAIDA_EQUI_DISPONIVEIS IS INITIAL AND
*       IT_SAIDA_EQUI_EMPRESTADOS IS INITIAL.

*      ITABSTRIP-ACTIVETAB = 'TAB_RESPONSAVEL'.

*    ENDIF.


*    Obs: Na sub-tela 0130 utiliza-se o mesmo método de pesquisa por nº de eqpto
*    da sub-tela 0120, pois são os mesmos critérios.

    IF TBX_BUSC_EQUIPAMENTO IS NOT INITIAL.
      TBX_BUSCA_EQUIPM_EMPRESTADOS( ).

*   Busca pelo nº do centro

    ELSEIF TBX_BUSC_CENTRO IS NOT INITIAL.
      TBX_BUSCA_CENTRO_RESPONSAVEL( ).
    ENDIF.

    LOOP AT IT_ZEQUI_EMPRESTIMO INTO WA_ZEQUI_EMPRESTIMO.

*   Verifica se o usuário possuí permissão para a pesquisa informada.
      Z_AUTHORITY_CHECK( OBJECT = 'I_IWERK'
                         ID     = 'IWERK'
                         FIELD  = WA_ZEQUI_EMPRESTIMO-IWERK
                         RETURN = RETURN_STATUS ).

      IF RETURN_STATUS IS INITIAL.

        WA_SAIDA_EQUI_RESPONSAVEL-EQUNR       = WA_ZEQUI_EMPRESTIMO-EQUNR.
        WA_SAIDA_EQUI_RESPONSAVEL-SWERK       = WA_ZEQUI_EMPRESTIMO-SWERK.
        WA_SAIDA_EQUI_RESPONSAVEL-IWERK       = WA_ZEQUI_EMPRESTIMO-IWERK.
        WA_SAIDA_EQUI_RESPONSAVEL-QT_DIAS     = WA_ZEQUI_EMPRESTIMO-QT_DIAS.
        WA_SAIDA_EQUI_RESPONSAVEL-UNAME       = WA_ZEQUI_EMPRESTIMO-UNAME.
        WA_SAIDA_EQUI_RESPONSAVEL-EQKTX       = WA_ZEQUI_EMPRESTIMO-EQKTX.
        WA_SAIDA_EQUI_RESPONSAVEL-ERDAT       = WA_ZEQUI_EMPRESTIMO-ERDAT.
        WA_SAIDA_EQUI_RESPONSAVEL-NUMERO_NOTA = WA_ZEQUI_EMPRESTIMO-NOTIF_NO.
        WA_SAIDA_EQUI_RESPONSAVEL-ORDEM_ABAST = WA_ZEQUI_EMPRESTIMO-STANDORDER.
        WA_SAIDA_EQUI_RESPONSAVEL-ORDEM_REMON = WA_ZEQUI_EMPRESTIMO-SETTLORDER.
        APPEND WA_SAIDA_EQUI_RESPONSAVEL TO IT_SAIDA_EQUI_RESPONSAVEL.

      ELSE.
        MESSAGE S836(SD) WITH TEXT-031 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR: WA_SAIDA_EQUI_RESPONSAVEL, WA_ZEQUI_EMPRESTIMO, TBX_BUSC_EQUIPAMENTO,
           TBX_BUSC_CENTRO.

  ENDMETHOD.                    "Z_SELECIONA_DADOS_SCREEN_0130

************************************************************************
*& Descrição: Text-box de pesquisa por nº do centro sub-tela 0130     &*
*& Atributos Globais                                                  &*
**********************************************************************&*
  METHOD TBX_BUSCA_CENTRO_RESPONSAVEL.
    SELECT *
      FROM ZEQUI_EMPRESTIMO
      INTO TABLE IT_ZEQUI_EMPRESTIMO
     WHERE IWERK EQ TBX_BUSC_CENTRO.

    CHECK SY-SUBRC IS NOT INITIAL.

    IF ITABSTRIP-ACTIVETAB = 'TAB_RESPONSAVEL'.
      MESSAGE S836(SD) WITH TEXT-043 TEXT-041 DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.                    "TBX_BUSCA_POR_CENTRO_0130

************************************************************************
*& Descrição.: Verificar se o usuário possuí permissão ao objeto      &*
*& Atributos.: ME->AT_RETURN                                          &*
*& Parâmetros: OBJECT, ID, FIELD                                      &*
**********************************************************************&*
  METHOD Z_AUTHORITY_CHECK.
    CLEAR RETURN_STATUS.

    AUTHORITY-CHECK OBJECT OBJECT
    ID ID FIELD FIELD.

    CHECK SY-SUBRC IS NOT INITIAL.
    RETURN_STATUS = 'X'.

  ENDMETHOD.                    "Z_AUTHORITY_CHECK

************************************************************************
*& Descrição.: Obter status do equipamento                            &*
*& Atributos.: Globais                                                &*
*& Parâmetros: EQUIPMENT                                              &*
**********************************************************************&*
  METHOD Z_STATUS_EQUIPAMENTO.

    CLEAR: IT_SYSTEM_STATUS, WA_RETURN,
           IT_USER_STATUS.

    CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
      EXPORTING
        EQUIPMENT     = EQUIPMENT
        LANGUAGE      = SY-LANGU
      IMPORTING
        RETURN        = WA_RETURN
      TABLES
        SYSTEM_STATUS = IT_SYSTEM_STATUS
        USER_STATUS   = IT_USER_STATUS.

    LOOP AT IT_SYSTEM_STATUS INTO WA_SYSTEM_STATUS.

      IF WA_SYSTEM_STATUS-STATUS = 'I0076' OR
         WA_SYSTEM_STATUS-STATUS = 'I0320'.
        MESSAGE S836(SD) WITH TEXT-022 TBX_BUSC_EQUIPAMENTO TEXT-033 DISPLAY LIKE 'W'.

        RETURN_STATUS = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "Z_STATUS_EQUIPAMENTO

************************************************************************
*& Descrição.: Verificar se o eqpto está emprestado                   &*
*& Atributos.: Globais                                                &*
*& Parâmetros: MSG, RETURN                                            &*
**********************************************************************&*
  METHOD Z_CHECAR_EQUI_EMPRESTADO.
    CLEAR RETURN_STATUS.

    LOOP AT IT_ZEQUI_EMPRESTIMO INTO WA_ZEQUI_EMPRESTIMO.
      READ TABLE IT_EQUI INTO WA_EQUI WITH KEY EQUNR = WA_ZEQUI_EMPRESTIMO-EQUNR.

      CHECK SY-SUBRC IS INITIAL.
      DELETE IT_EQUI WHERE EQUNR = WA_ZEQUI_EMPRESTIMO-EQUNR.

      IF MSG EQ 'X'.
        CHECK ITABSTRIP-ACTIVETAB = 'TAB_DISPONIVEIS'.
        MESSAGE S836(SD) WITH TEXT-022 WA_ZEQUI_EMPRESTIMO-EQUNR TEXT-032 DISPLAY LIKE 'W'.
        ITABSTRIP-ACTIVETAB = 'TAB_EMPRESTADOS'.
      ENDIF.

      RETURN_STATUS = 'X'.
    ENDLOOP.
  ENDMETHOD.                    "Z_CHECAR_EQUI_EMPRESTADO

************************************************************************
*& Descrição.: Detalhes do equipamento                                &*
*& Atributos.: WA_DATA_SPECIFIC_EXP, WA_RETURN                        &*
*& Parâmetros:                                                        &*
**********************************************************************&*
  METHOD Z_DETALHES_EQUIPAMENTO.

    CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
      EXPORTING
        EQUIPMENT         = TBX_BUSC_EQUIPAMENTO
      IMPORTING
        DATA_SPECIFIC_EXP = WA_DATA_SPECIFIC_EXP
        RETURN            = WA_RETURN.

    IF WA_DATA_SPECIFIC_EXP-EQUICATGRY = 'V' OR
       WA_DATA_SPECIFIC_EXP-EQUICATGRY = 'F'.
      EXIT.
    ELSE.
      MESSAGE S836(SD) WITH TEXT-002 TEXT-003 DISPLAY LIKE 'W'.
      RETURN_STATUS = 'X'.
    ENDIF.
  ENDMETHOD.                    "Z_DETALHES_EQUIPAMENTO

************************************************************************
*& Descrição.: Atualizar tela após empréstimo de eqpto                &*
*& Atributos.:                                                        &*
*& Parâmetros:                                                        &*
**********************************************************************&*
  METHOD Z_ATUALIZA_TELA_EMPRESTIMO.

*   Deletar da tela de equipamentos disponíveis,
*   os equipamentos que foram emprestados.

    LOOP AT IT_SAIDA_EMPRESTIMO_EQUI INTO WA_SAIDA_EMPRESTIMO_EQUI.
      DELETE IT_SAIDA_EQUI_DISPONIVEIS
       WHERE EQUNR = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR.
    ENDLOOP.

    CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

*   Fazer a busca dos equipamentos que foram emprestados, e mudar para a
*   aba dos equipamentos emprestados.

    TBX_BUSC_CENTRO     = WA_SAIDA_EMPRESTIMO_EQUI-IWERK.
    ITABSTRIP-ACTIVETAB = 'TAB_EMPRESTADOS'.

    Z_SELECIONA_DADOS_TELA_0120( ).

    CALL METHOD OBJ_ALV_0120->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    CLEAR: TBX_BUSC_EQUIPAMENTO, TBX_BUSC_CENTRO,
           TBX_CENTRO_DESTINO, TBX_QT_DIAS, IT_SAIDA_EMPRESTIMO_EQUI.


  ENDMETHOD.                    "Z_ATUALIZA_TELA_POS_OPERACAO

ENDCLASS.                    "Z_SELECIONA_DADOS IMPLEMENTATION
