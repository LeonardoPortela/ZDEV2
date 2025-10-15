*&---------------------------------------------------------------------*
*&  Include           ZPMR0016_CLASS_UTILS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& CLASS ZUTEIS	DEFINITION                                             *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*
CLASS ZUTEIS DEFINITION.
  PUBLIC SECTION.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_EQUI_HIERARCHY                                      *
*&---------------------------------------------------------------------*
    METHODS: Z_CHECAR_EQUI_HIERARCHY EXPORTING
                                        RETURN TYPE CHAR1.

*&---------------------------------------------------------------------*
*& METHOD Z_EQUI_HIERARCHY_READ                                        *
*&---------------------------------------------------------------------*
    METHODS: Z_EQUI_HIERARCHY_READ IMPORTING
                                     EQUIPMENT TYPE EQUNR.

*&---------------------------------------------------------------------*
*& METHOD Z_ATUALIZA_STATUS_BAPIS                                      *
*&---------------------------------------------------------------------*
    METHODS: Z_ATUALIZA_STATUS_BAPIS IMPORTING
                                    TXT_STATUS TYPE ITEX132.

*&---------------------------------------------------------------------*
*& METHOD Z_CREATE_COSTCENTER                                          *
*&---------------------------------------------------------------------*
    METHODS: Z_CREATE_COSTCENTER IMPORTING
                                        SWERK1 TYPE IWERK
                                        SWERK2 TYPE IWERK
                                        CENTER TYPE KOSTL.

*&---------------------------------------------------------------------*
*& METHOD Z_DELETE_ZEROS                                               *
*&---------------------------------------------------------------------*
    METHODS: Z_DELETE_ZEROS CHANGING
                                         FIELD TYPE EQUNR.

*&---------------------------------------------------------------------*
*& METHOD Z_INSERT_DADOS_EMPRESTIMO                                    *
*&---------------------------------------------------------------------*
    METHODS: Z_INSERT_DADOS_EMPRESTIMO IMPORTING
                                         EQUNR TYPE EQUNR
                                         SWERK TYPE SWERK
                                         IWERK TYPE IWERK
                                       QT_DIAS TYPE NUMC3
                                         ERDAT TYPE SY-DATUM
                                         UNAME TYPE SY-UNAME
                                         EQKTX TYPE KTX01
                                   NUMERO_NOTA TYPE QMNUM
                                   ORDEM_ABAST TYPE DAUFN
                                   ORDEM_REMON TYPE ILOM_ORDST.

*&---------------------------------------------------------------------*
*& METHOD Z_DELETE_DADOS_EMPRESTIMO                                    *
*&---------------------------------------------------------------------*
    METHODS: Z_DELETE_DADOS_EMPRESTIMO IMPORTING
                                       EQUIPMENT TYPE EQUNR.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_DT_HR_DEVOLUCAO                                    *
*&---------------------------------------------------------------------*
    METHODS: Z_CHECAR_DT_HR_DEVOLUCAO EXPORTING
                                        RETURN TYPE CHAR1
                                      CHANGING
                                        IT_TAB TYPE ANY TABLE
                                        WA_TAB TYPE ANY.

    METHODS: Z_LIMPAR_TELA.

  PRIVATE SECTION.
    DATA: CONCAC_TEXT TYPE CHAR200.

ENDCLASS.                    "ZUTEIS DEFINITION

*&---------------------------------------------------------------------*
*& CLASS ZUTEIS	IMPLEMENTATION                                         *
*& AUTOR: ENIO JESUS                                                   *
*& 15.07.2015                                                          *
*&---------------------------------------------------------------------*

CLASS ZUTEIS IMPLEMENTATION.

**********************************************************************
*& Descrição: Checar hierarquia dos equipamentos.                   &*
*& Parâmetro: EQUIPMENT, RETURN                                     &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD: Z_CHECAR_EQUI_HIERARCHY.
    CLEAR RETURN_STATUS.

    LOOP AT IT_SAIDA_EMPRESTIMO_EQUI INTO WA_SAIDA_EMPRESTIMO_EQUI.

*   No caso de um equipamento superior, o método retorna em IT_HIERARCHY
*   todos que são inferiores a este.
      Z_EQUI_HIERARCHY_READ( EQUIPMENT = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR ).

      IF IT_HIERARCHY[] IS NOT INITIAL.

        LOOP AT IT_HIERARCHY[] INTO WA_HIERARCHY.

*      Se equipamento for do tipo 'T' ou 'F' não bloquear.
          IF WA_HIERARCHY-EQTYP = 'T' OR
             WA_HIERARCHY-EQTYP = 'F'.
            EXIT.
          ENDIF.

*       Faz a leitura para verificar se o equipamento inferior foi selecionado.
          READ TABLE IT_SAIDA_EMPRESTIMO_EQUI INTO WA_SAIDA_EMPRESTIMO_EQUI
            WITH KEY EQUNR = WA_HIERARCHY-EQUNR.

*       Deleta os '000000's das variáveis de equipamento.
          Z_DELETE_ZEROS( CHANGING FIELD = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR ).
          Z_DELETE_ZEROS( CHANGING FIELD = WA_HIERARCHY-HEQUI ).
          Z_DELETE_ZEROS( CHANGING FIELD = WA_HIERARCHY-EQUNR ).

*       Verifica se o equipamento inferior existe na tabela de empréstimo.
          IF WA_HIERARCHY-EQUNR NE WA_SAIDA_EMPRESTIMO_EQUI-EQUNR.

*         Concatena os textos em uma variável para conseguir exibir.
            CONCATENATE TEXT-022 WA_HIERARCHY-HEQUI TEXT-011 WA_HIERARCHY-EQUNR
                   INTO CONCAC_TEXT SEPARATED BY SPACE.

            MESSAGE S836(SD) WITH CONCAC_TEXT TEXT-012 TEXT-013 DISPLAY LIKE 'W'.

*         Se o eqpto não existir, retorna exibe a mensagem e retorna falso.
            RETURN_STATUS = 'X'.

          ENDIF.
        ENDLOOP.
      ELSE.

*     No caso de um equipamento inferior, caso IT_HIERARCHY retorne vazia na tabela V_EQUI
*     pegamos o equipamento superior deste, no campo "HEQUI".
        SELECT SINGLE *
          FROM V_EQUI
          INTO WA_VEQUI
         WHERE EQUNR = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR.

        CHECK WA_VEQUI-HEQUI IS NOT INITIAL.

*     Método que retorna hierarquia dos equipamentos.
        Z_EQUI_HIERARCHY_READ( EQUIPMENT = WA_VEQUI-HEQUI ).

        LOOP AT IT_HIERARCHY[] INTO WA_HIERARCHY.
          CHECK WA_HIERARCHY-EQUNR = WA_VEQUI-EQUNR.

          IF WA_HIERARCHY-EQTYP = 'T' OR
             WA_HIERARCHY-EQTYP = 'F'.
            EXIT.
          ENDIF.

*       Faz a leitura para verificar se o equipamento superior deste foi selecionado.
          READ TABLE IT_SAIDA_EMPRESTIMO_EQUI INTO WA_SAIDA_EMPRESTIMO_EQUI
            WITH KEY EQUNR = WA_HIERARCHY-HEQUI.

*       Deleta os '000000's das variáveis de equipamento.
          Z_DELETE_ZEROS( CHANGING FIELD = WA_HIERARCHY-HEQUI ).
          Z_DELETE_ZEROS( CHANGING FIELD = WA_HIERARCHY-EQUNR ).

*       Verifica se o equipamento superior existe na tabela de empréstimo.
          IF WA_HIERARCHY-HEQUI NE WA_SAIDA_EMPRESTIMO_EQUI-EQUNR.

*       Concatena os textos em uma variável para conseguir exibir.
            CONCATENATE TEXT-022 WA_HIERARCHY-EQUNR TEXT-011 WA_HIERARCHY-HEQUI
                   INTO CONCAC_TEXT SEPARATED BY SPACE.

            MESSAGE S836(SD) WITH CONCAC_TEXT TEXT-012 TEXT-013 DISPLAY LIKE 'W'.

*         Se o eqpto não existir, retorna exibe a mensagem e retorna falso.
            RETURN_STATUS = 'X'.

          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "Z_CHECAR_EQUI_HIERARCHY

**********************************************************************
*& Descrição: Atualizar status das BAPIS no alv da tela 0200        &*
*& Parâmetro: TXT_STATUS                                            &*
*& Atributos: ME->CONCAC_TEXT                                       &*
********************************************************************&*
  METHOD Z_ATUALIZA_STATUS_BAPIS.

* Atualizar status das BAPIS na ALV, conforme os dados passados para o método.
    INDICE = INDICE + 1.

    CONCATENATE INDICE TXT_STATUS INTO ME->CONCAC_TEXT SEPARATED BY SPACE.

    WA_STATUS_BAPIS-TXT_STATUS = ME->CONCAC_TEXT.
    APPEND WA_STATUS_BAPIS TO IT_STATUS_BAPIS.

    STATUS_BAPIS = 'X'.

    CALL METHOD OBJ_ALV_0200->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "Z_CHECAR_STATUS_BAPIS

**********************************************************************
*& Descrição: Retornar hierarquia dos equipamentos                  &*
*& Parâmetro: EQUIPMENT                                             &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD Z_EQUI_HIERARCHY_READ.

* Função retorna as hierarquias dos equipamentos.
* INFERIOR || SUPERIOR

    CALL FUNCTION 'EQUI_HIERARCHY_READ'
      EXPORTING
        EQUIPMENT  = EQUIPMENT
        LEVEL_DOWN = 01
      TABLES
        HIER_TAB   = IT_HIERARCHY.

    DELETE IT_HIERARCHY[] INDEX 1.

  ENDMETHOD.                    "Z_EQUI_HIERARCHY_READ

**********************************************************************
*& Descrição: Deletar 000000's das variáveis                        &*
*& Parâmetro: FIELD                                                 &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD Z_DELETE_ZEROS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = FIELD
      IMPORTING
        OUTPUT = FIELD.

  ENDMETHOD.                    "Z_DELETE_ZEROS_VAR

**********************************************************************
*& Descrição: Montar centro de custo                                &*
*& Parâmetro: IWERK1 IWERK2 CENTER                                  &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD Z_CREATE_COSTCENTER.

* Para criar o centro de custo, primeiramente concatenamos '0', em seguida
* os 2 primeiros dígitos do centro destino, '0' novamente, os 2 ultimos
* dígitos do centro destino, e os 4 ultimos dígitos do
* centro origem.

* Exemplo.: Centro atual: 0150210085 -> Centro destino: 0150070085.

    CONCATENATE '0' SWERK1(2) '0' SWERK2+2(2) CENTER+6(4) INTO AT_COSTCENTER_DESTINO.

  ENDMETHOD.                    "Z_CREATE_COSTCENTER

**********************************************************************
*& Descrição: Inserior dados dos eqptos emprestados na tabela z     &*
*& Parâmetro: EQUNR, SWERK, IWERK, QT_DIAS, ERDAT, UNAME, EQKTX     &*
*             NUMERO_NOTA, ORDEM_ABAST, ORDEM_REMON                 &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD Z_INSERT_DADOS_EMPRESTIMO.

* Inserir dados dos equipamentos que estão sendo emprestados na tabela ZEQUI_EMPRESTIMO,
* esse médoto é chamado no final da das BAPIS.

    WA_ZEQUI_EMPRESTIMO-EQUNR      = EQUNR.
    WA_ZEQUI_EMPRESTIMO-SWERK      = SWERK.
    WA_ZEQUI_EMPRESTIMO-IWERK      = IWERK.
    WA_ZEQUI_EMPRESTIMO-QT_DIAS    = QT_DIAS.
    WA_ZEQUI_EMPRESTIMO-ERDAT      = ERDAT.
    WA_ZEQUI_EMPRESTIMO-UNAME      = UNAME.
    WA_ZEQUI_EMPRESTIMO-EQKTX      = EQKTX.
    WA_ZEQUI_EMPRESTIMO-NOTIF_NO   = NUMERO_NOTA.
    WA_ZEQUI_EMPRESTIMO-STANDORDER = ORDEM_ABAST.
    WA_ZEQUI_EMPRESTIMO-SETTLORDER = ORDEM_REMON.

    MODIFY ZEQUI_EMPRESTIMO FROM WA_ZEQUI_EMPRESTIMO.
    COMMIT WORK.
  ENDMETHOD.                    "Z_INSERT_TABLE_EMPRESTIMO

**********************************************************************
*& Descrição: Deletar dados dos eqptos da tabela z                  &*
*  no momento da devolução                                          &*
*& Parâmetro: EQUNR                                                 &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD Z_DELETE_DADOS_EMPRESTIMO.

    DELETE IT_SAIDA_EQUI_RESPONSAVEL WHERE EQUNR = EQUIPMENT.

    DELETE FROM ZEQUI_EMPRESTIMO
           WHERE EQUNR = EQUIPMENT.
    COMMIT WORK.

    CALL METHOD OBJ_ALV_0130->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "Z_DELETE_DADOS_EMPRESTIMO

**********************************************************************
*& Descrição: Checar se parâmetros estão preenchidos para devolver  &*
*& Atributos Globais                                                &*
********************************************************************&*
  METHOD: Z_CHECAR_DT_HR_DEVOLUCAO.
    CLEAR: RETURN_STATUS, WA_SAIDA_EQUI_RESPONSAVEL, WA_ZEQUI_EMPRESTIMO,
           IT_ZEQUI_EMPRESTIMO.

* Verifica se se foi selecionado algum equipamento.

    LOOP AT IT_SAIDA_EQUI_RESPONSAVEL INTO WA_SAIDA_EQUI_RESPONSAVEL
      WHERE CBX_DEVOLVER = 'X'.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE I836(SD) WITH TEXT-001.
        RETURN_STATUS = 'X'.
      ELSE.

        SELECT SINGLE *
          FROM ZEQUI_EMPRESTIMO
          INTO WA_ZEQUI_EMPRESTIMO
         WHERE EQUNR = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR.

*     Verifica se foi informada uma data para o eqpto selecionado.

        IF WA_SAIDA_EQUI_RESPONSAVEL-DT_DEVOLUCAO IS INITIAL.
          MESSAGE I836(SD) WITH TEXT-036 DISPLAY LIKE 'W'.

          RETURN_STATUS = 'X'.

*     Verifica se a data de devolução é maior que a data de empréstimo.

        ELSEIF WA_SAIDA_EQUI_RESPONSAVEL-DT_DEVOLUCAO < WA_ZEQUI_EMPRESTIMO-ERDAT.
          MESSAGE I836(SD) WITH TEXT-044 TEXT-045 DISPLAY LIKE 'W'.

          RETURN_STATUS = 'X'.

*      Verifica se foi informada uma hora para eqpto selecionado.

        ELSEIF WA_SAIDA_EQUI_RESPONSAVEL-HR_DEVOLUCAO IS INITIAL.
          MESSAGE I836(SD) WITH TEXT-037 DISPLAY LIKE 'W'.

          RETURN_STATUS = 'X'.

        ENDIF.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.                    "Z_CHECAR_DT_HR_DEVOLUCAO

  METHOD Z_LIMPAR_TELA.

    CLEAR: TBX_BUSC_EQUIPAMENTO, TBX_BUSC_CENTRO, IT_STATUS_BAPIS.

    CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "Z_LIMPAR_TELA

ENDCLASS.                    "ZUTEIS IMPLEMENTATION
