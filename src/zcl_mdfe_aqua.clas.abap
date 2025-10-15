class ZCL_MDFE_AQUA definition
  public
  inheriting from ZCL_MDFE
  create public .

*"* public components of class ZCL_MDFE_AQUA
*"* do not include other source files here!!!
public section.

  methods SET_CNPJAGENAV
    importing
      !CNPJ type STCD1 .
  methods SET_TPEMB
    importing
      !TPEMB type ZDLES003 .
  methods SET_CEMBAR
    importing
      !CEMBAR type CHAR10 .
  methods SET_XEMBAR
    importing
      !XEMBAR type CHAR060 .
  methods SET_NVIAG
    importing
      !NVIAG type NUM10 .
  methods SET_CPRTEMB
    importing
      !CPRTEMB type J_1BPARID .
  methods SET_CPRTDEST
    importing
      !CPRTDEST type J_1BPARID .
  methods SET_CTERMCARREG
    importing
      !CTERMCARREG type CHAR08 .
  methods SET_XTERMCARREG
    importing
      !XTERMCARREG type CHAR060 .
  methods SET_CTERMDESCARREG
    importing
      !CTERMDESCARREG type CHAR08 .
  methods SET_XTERMDESCARREG
    importing
      !XTERMDESCARREG type CHAR060 .
  methods SET_CEMBCOMB
    importing
      !CEMBCOMB type CHAR10 .
  methods SET_IDUNIDCARGAVAZIA
    importing
      !IDUNIDCARGAVAZIA type CHAR20 .
  methods SET_TPUNIDCARGAVAZIA
    importing
      !TPUNIDCARGAVAZIA type NUM1 .
  methods GET_CNPJAGENAV
    returning
      value(CNPJAGENAV) type STCD1 .
  methods GET_TPEMB
    returning
      value(TPEMB) type ZDLES003 .
  methods GET_CEMBAR
    returning
      value(CEMBAR) type CHAR10 .
  methods GET_XEMBAR
    returning
      value(XEMBAR) type CHAR060 .
  methods GET_NVIAG
    returning
      value(NVIAG) type NUM10 .
  methods GET_CPRTEMB
    returning
      value(CPRTEMB) type J_1BPARID .
  methods GET_CPRTDEST
    returning
      value(CPRTDEST) type J_1BPARID .
  methods GET_CTERMCARREG
    returning
      value(CTERMCARREG) type CHAR08 .
  methods GET_XTERMCARREG
    returning
      value(XTERMCARREG) type CHAR060 .
  methods GET_CTERMDESCARREG
    returning
      value(CTERMDESCARREG) type CHAR08 .
  methods GET_XTERMDESCARREG
    returning
      value(XTERMDESCARREG) type CHAR060 .
  methods GET_CEMBCOMB
    returning
      value(CEMBCOMB) type CHAR10 .
  methods GET_IDUNIDCARGAVAZIA
    returning
      value(IDUNIDCARGAVAZIA) type CHAR20 .
  methods GET_TPUNIDCARGAVAZIA
    returning
      value(TPUNIDCARGAVAZIA) type NUM1 .
  methods MONTA_XML
    importing
      !I_OBJ type ref to ZCL_MDFE
    returning
      value(E_XML) type STRING .
protected section.
*"* protected components of class ZCL_MDFE_AQUA
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_MDFE_AQUA
*"* do not include other source files here!!!

  data AT_CNPJAGENAV type STCD1 .
  data AT_TPEMB type ZDLES003 .
  data AT_CEMBAR type CHAR10 .
  data AT_XEMBAR type CHAR060 .
  data AT_NVIAG type NUM10 .
  data AT_CPRTEMB type J_1BPARID .
  data AT_CPRTDEST type J_1BPARID .
  data AT_CTERMCARREG type CHAR08 .
  data AT_XTERMCARREG type CHAR060 .
  data AT_CTERMDESCARREG type CHAR08 .
  data AT_XTERMDESCARREG type CHAR060 .
  data AT_CEMBCOMB type CHAR10 .
  data AT_IDUNIDCARGAVAZIA type CHAR20 .
  data AT_TPUNIDCARGAVAZIA type NUM1 .
ENDCLASS.



CLASS ZCL_MDFE_AQUA IMPLEMENTATION.


METHOD GET_CEMBAR.
************************************
*  Método de Acesso
*  Atributo: AT_CEMBAR
*  Retorno: CEMBAR
*  Descrição: Método para capturar o código da embarcação.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:53:37
************************************
  CEMBAR = ME->AT_CEMBAR.
ENDMETHOD.


method GET_CEMBCOMB.
************************************
*  Método de Acesso
*  Atributo: AT_CEMBCOMB
*  Retorno: CEMBCOMB
*  Descrição: Método para capturar o código da embarcação do combio.
*  Developer: Victor Hugo Souza Nunes
*  01.12.2015 08:22:36
************************************
  CEMBCOMB = ME->AT_CEMBCOMB.
endmethod.


METHOD GET_CNPJAGENAV.
************************************
*  Método de Acesso
*  Atributo: AT_CNPJAGENAV
*  Retorno: CNPJAGENAV
*  Descrição: Método para capturar o CNPJ da Agência de Navegação.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:40:13
************************************
  CNPJAGENAV = ME->AT_CNPJAGENAV.
ENDMETHOD.


method GET_CPRTDEST.
************************************
*  Método de Acesso
*  Atributo: AT_CPRTDEST
*  Retorno: CPRTDEST
*  Descrição: Método para capturar o código do Porto de Destino.
*  Developer: Victor Hugo Souza Nunes
*  01.12.2015 08:17:25
************************************
  CPRTDEST = ME->AT_CPRTDEST.
endmethod.


method GET_CPRTEMB.
************************************
*  Método de Acesso
*  Atributo: AT_CPRTEMB
*  Retorno: CPRTEMB
*  Descrição: Método para capturar o código do porto de embarque.
*  Developer: Victor Hugo Souza Nunes
*  01.12.2015 08:07:55
************************************
  CPRTEMB = ME->AT_CPRTEMB.
endmethod.


method GET_CTERMCARREG.
************************************
*  Método de Acesso
*  Atributo: AT_CTERMCARREG
*  Retorno: CTERMCARREG
*  Descrição: Método para capturar o código do terminal de carregamento.
*  Developer: Victor Hugo Souza Nunes
*  01.12.2015 08:18:15
************************************
  CTERMCARREG = ME->AT_CTERMCARREG.
endmethod.


METHOD GET_CTERMDESCARREG.
************************************
*  Método de Acesso
*  Atributo: AT_CTERMDESCARREG
*  Retorno: CTERMDESCARREG
*  Descrição: Método para capturar o Código do terminal de descarregamento.
*  Developer: Victor Hugo Souza Nunes
*  01.12.2015 08:20:13
************************************
  CTERMDESCARREG = ME->AT_CTERMDESCARREG.
ENDMETHOD.


method GET_IDUNIDCARGAVAZIA.
************************************
*  Método de Acesso
*  Atributo: AT_IDUNIDCARGAVAZIA
*  Retorno: IDUNIDCARGAVAZIA
*  Descrição: Método para capturar a Identificação da unidade de Carga Vazia.
*  Developer: Victor Hugo Souza Nunes
*  01.12.2015 08:23:45
************************************
  IDUNIDCARGAVAZIA = ME->AT_IDUNIDCARGAVAZIA.
endmethod.


method GET_NVIAG.
************************************
*  Método de Acesso
*  Atributo: AT_NVIAG
*  Retorno: NVIAG
*  Descrição: Método para capturar o Número da Viagem.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 10:11:23
************************************
  NVIAG = ME->AT_NVIAG.
endmethod.


method GET_TPEMB.
************************************
*  Método de Acesso
*  Atributo: AT_TPEMB
*  Retorno: TPEMB
*  Descrição: Método para capturar o código  da embarcação.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:42:28
************************************
  TPEMB = ME->AT_TPEMB.
endmethod.


method GET_TPUNIDCARGAVAZIA.
************************************
*  Método de Acesso
*  Atributo: AT_TPUNIDCARGAVAZIA
*  Retorno: TPUNIDCARGAVAZIA
*  Descrição: Método para capturar o tipo da unidade de carga vazia.
*  Developer: Victor Hugo Souza Nunes
*  01.12.2015 08:24:40
************************************
  TPUNIDCARGAVAZIA = ME->AT_TPUNIDCARGAVAZIA.
endmethod.


method GET_XEMBAR.
************************************
*  Método de Acesso
*  Atributo: AT_XEMBAR
*  Retorno: XEMBAR
*  Descrição: Método para capturar o Nome da Embarcação.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 10:10:41
************************************
  XEMBAR = ME->AT_XEMBAR.
endmethod.


METHOD GET_XTERMCARREG.
************************************
*  Método de Acesso
*  Atributo: AT_XTERMCARREG
*  Retorno: XTERMCARREG
*  Descrição: Método para configurar o nome do terminal de carregamento.
*  Developer: Victor Hugo Souza Nunes
*  01.12.2015 08:19:02
************************************
  XTERMCARREG = ME->AT_XTERMCARREG.
ENDMETHOD.


method GET_XTERMDESCARREG.
************************************
*  Método de Acesso
*  Atributo: AT_XTERMDESCARREG
*  Retorno: XTERMDESCARREG.
*  Descrição: Método para capturar o Nome do Terminal de descarregamento.
*  Developer: Victor Hugo Souza Nunes
*  01.12.2015 08:21:07
************************************
  XTERMDESCARREG = ME->AT_XTERMDESCARREG.
endmethod.


METHOD MONTA_XML.

  DATA: WA_ZLEST0066  TYPE ZLEST0066,
        WA_ZLEST0061  TYPE ZLEST0061,
        IT_ZLEST0063  TYPE TABLE OF ZLEST0063,
        WA_ZLEST0063  TYPE ZLEST0063,
        IT_ZLEST0053  TYPE TABLE OF ZLEST0053,
        WA_ZLEST0053  TYPE ZLEST0053,
        IT_ZLEST0056  TYPE TABLE OF ZLEST0056,
        WA_ZLEST0056  TYPE ZLEST0056,
        IT_ZLEST0106  TYPE TABLE OF ZLEST0106,
        WA_ZLEST0106  TYPE ZLEST0106,
        IT_J_1BNFDOC  TYPE TABLE OF J_1BNFDOC,
        WA_J_1BNFDOC  TYPE J_1BNFDOC,
        IT_J_1BBRANCH TYPE TABLE OF J_1BBRANCH,
        WA_J_1BBRANCH TYPE J_1BBRANCH.

  DATA: VL_IDX_DOC TYPE I,
        VL_XVALOR  TYPE STRING,
        MSG_EXIBIR TYPE STRING,
        MSG_AUX01  TYPE STRING.

  "Váriaveis XML
  DATA: VL_TP_EMB         TYPE STRING,
        VL_TP_IRIN        TYPE STRING,
        VL_CEMBAR         TYPE STRING,
        VL_XEMBAR         TYPE STRING,
        VL_CPRTEMB        TYPE STRING,
        VL_CPRTDEST       TYPE STRING,
        VL_CTERMCARREG    TYPE STRING,
        VL_XTERMCARREG    TYPE STRING,
        VL_CTERMDESCARREG TYPE STRING,
        VL_XTERMDESCARREG TYPE STRING,
        VL_CNPJAGENAV     TYPE STRING.


  DEFINE CONC_XML.
    CONCATENATE E_XML &1 INTO E_XML.
  END-OF-DEFINITION.

  CLEAR: E_XML. "Limpar a variavel de retorno.


  CONC_XML '<aquav>'.

  READ TABLE I_OBJ->AT_IT_DOC_MDFE INTO I_OBJ->AT_WA_DOC_MDFE INDEX 1.

  "Frete Aquaviário - Ordem de Venda
  CLEAR: WA_ZLEST0061.
  SELECT SINGLE *
    INTO WA_ZLEST0061
    FROM ZLEST0061
   WHERE DOCNUM = I_OBJ->AT_WA_DOC_MDFE-DOCNUM.

  "Busca Dados Doc.
  CLEAR: WA_J_1BNFDOC.
  SELECT SINGLE *
    INTO WA_J_1BNFDOC
    FROM J_1BNFDOC
   WHERE DOCNUM = I_OBJ->AT_WA_DOC_MDFE-DOCNUM.

  IF SY-SUBRC NE 0.
    CLEAR: E_XML.
    ROLLBACK WORK.
    MESSAGE 'Não encontrado os dados do documento vinculado ao MDF-e!' TYPE 'E'.
    RETURN.
  ENDIF.

  "Busca CNPJ Matriz
  SELECT SINGLE *
    INTO WA_J_1BBRANCH
    FROM J_1BBRANCH
   WHERE BUKRS      EQ  WA_J_1BNFDOC-BUKRS
     AND BRANCH     NE '0001'
     AND CGC_BRANCH EQ '1'.


  IF ( SY-SUBRC NE 0 ) OR ( WA_J_1BBRANCH-STCD1 IS INITIAL ).
    CLEAR: E_XML.
    ROLLBACK WORK.
    MESSAGE 'Não encontrado os dados do documento vinculado ao MDF-e!' TYPE 'E'.
    RETURN.
  ENDIF.

  VL_CNPJAGENAV = WA_J_1BBRANCH-STCD1.

  "Frete Aquaviário - Viagem
  CLEAR: WA_ZLEST0056.
  SELECT SINGLE *
    INTO WA_ZLEST0056
    FROM ZLEST0056
   WHERE BUKRS       EQ WA_ZLEST0061-BUKRS
     AND WERKS       EQ WA_ZLEST0061-WERKS
     AND ANO_VIAGEM  EQ WA_ZLEST0061-ANO_VIAGEM
     AND NR_VIAGEM   EQ WA_ZLEST0061-NR_VIAGEM.

  IF SY-SUBRC NE 0.
    CLEAR: E_XML.
    ROLLBACK WORK.
    MESSAGE 'Não encontrado os dados da viagem!' TYPE 'E'.
    RETURN.
  ENDIF.

  "Cadastro Porto x Ministério Transporte

  "Porto Embarque
  CLEAR: WA_ZLEST0106.
  SELECT SINGLE *
    INTO WA_ZLEST0106
    FROM ZLEST0106
   WHERE COD_PORTO = WA_ZLEST0056-PO_EMBARQUE.

  IF SY-SUBRC NE 0.
    CLEAR: E_XML.
    ROLLBACK WORK.
    CLEAR: MSG_EXIBIR, MSG_AUX01.

    MSG_AUX01 = WA_ZLEST0056-PO_EMBARQUE.
    CONCATENATE 'Não cadastrado o Porto de Embarque:'  MSG_AUX01 ' na transação ZLES0112!'
                INTO MSG_EXIBIR SEPARATED BY SPACE.
    MESSAGE MSG_EXIBIR TYPE 'E'.
    RETURN.
  ELSE.

    VL_CPRTEMB     = WA_ZLEST0106-COD_PORTO_MIN.
    VL_CTERMCARREG = WA_ZLEST0106-COD_TERMINAL.
    VL_XTERMCARREG = WA_ZLEST0106-DESC_TERMINAL.

    IF VL_CPRTEMB IS INITIAL.
      CLEAR: E_XML.
      ROLLBACK WORK.
      CLEAR: MSG_EXIBIR, MSG_AUX01.

      MSG_AUX01 = WA_ZLEST0056-PO_EMBARQUE.
      CONCATENATE 'Não cadastrado para Porto de Embarque:'  MSG_AUX01 '(Transação ZLES0112), o Código Porto Ministério Transp.!'
                  INTO MSG_EXIBIR SEPARATED BY SPACE.
      MESSAGE MSG_EXIBIR TYPE 'E'.
      RETURN.
    ENDIF.

    "IF VL_CTERMCARREG IS INITIAL.
    IF ( 1 = 2 ).
      CLEAR: E_XML.
      ROLLBACK WORK.
      CLEAR: MSG_EXIBIR, MSG_AUX01.

      MSG_AUX01 = WA_ZLEST0056-PO_EMBARQUE.
      CONCATENATE 'Não cadastrado para Porto de Embarque:'  MSG_AUX01 '(Transação ZLES0112), o Código do Terminal!'
                  INTO MSG_EXIBIR SEPARATED BY SPACE.
      MESSAGE MSG_EXIBIR TYPE 'E'.
      RETURN.
    ENDIF.

    "IF VL_XTERMCARREG IS INITIAL.
    IF ( 1 = 2 ).
      CLEAR: E_XML.
      ROLLBACK WORK.
      CLEAR: MSG_EXIBIR, MSG_AUX01.

      MSG_AUX01 = WA_ZLEST0056-PO_EMBARQUE.
      CONCATENATE 'Não cadastrado para Porto de Embarque:'  MSG_AUX01 '(Transação ZLES0112), a Descrição do Terminal!'
                  INTO MSG_EXIBIR SEPARATED BY SPACE.
      MESSAGE MSG_EXIBIR TYPE 'E'.
      RETURN.
    ENDIF.

  ENDIF.

  "Porto Destino
  CLEAR: WA_ZLEST0106.
  SELECT SINGLE *
    INTO WA_ZLEST0106
    FROM ZLEST0106
   WHERE COD_PORTO = WA_ZLEST0056-PO_DESTINO.

  IF SY-SUBRC NE 0.

    CLEAR: E_XML.
    ROLLBACK WORK.
    CLEAR: MSG_EXIBIR, MSG_AUX01.

    MSG_AUX01 = WA_ZLEST0056-PO_DESTINO.
    CONCATENATE 'Não cadastrado o Porto de Destino:'  MSG_AUX01 ' na transação ZLES0112!'
                INTO MSG_EXIBIR SEPARATED BY SPACE.
    MESSAGE MSG_EXIBIR TYPE 'E'.
    RETURN.

  ELSE.

    VL_CPRTDEST       = WA_ZLEST0106-COD_PORTO_MIN.
    VL_CTERMDESCARREG = WA_ZLEST0106-COD_TERMINAL.
    VL_XTERMDESCARREG = WA_ZLEST0106-DESC_TERMINAL.

    IF VL_CPRTDEST IS INITIAL.
      CLEAR: E_XML.
      ROLLBACK WORK.
      CLEAR: MSG_EXIBIR, MSG_AUX01.

      MSG_AUX01 = WA_ZLEST0056-PO_DESTINO.
      CONCATENATE 'Não cadastrado para Porto de Embarque:'  MSG_AUX01 '(Transação ZLES0112), o Código Porto Ministério Transp.!'
                  INTO MSG_EXIBIR SEPARATED BY SPACE.
      MESSAGE MSG_EXIBIR TYPE 'E'.
      RETURN.
    ENDIF.

    "IF VL_CTERMDESCARREG IS INITIAL.
    IF ( 1 = 2 ).
      CLEAR: E_XML.
      ROLLBACK WORK.
      CLEAR: MSG_EXIBIR, MSG_AUX01.

      MSG_AUX01 = WA_ZLEST0056-PO_DESTINO.
      CONCATENATE 'Não cadastrado para Porto de Embarque:'  MSG_AUX01 '(Transação ZLES0112), o Código do Terminal!'
                  INTO MSG_EXIBIR SEPARATED BY SPACE.
      MESSAGE MSG_EXIBIR TYPE 'E'.
      RETURN.
    ENDIF.

    "IF VL_XTERMDESCARREG IS INITIAL.
    IF ( 1 = 2 ).
      CLEAR: E_XML.
      ROLLBACK WORK.
      CLEAR: MSG_EXIBIR, MSG_AUX01.

      MSG_AUX01 = WA_ZLEST0056-PO_DESTINO.
      CONCATENATE 'Não cadastrado para Porto de Embarque:'  MSG_AUX01 '(Transação ZLES0112), a Descrição do Terminal!'
                  INTO MSG_EXIBIR SEPARATED BY SPACE.
      MESSAGE MSG_EXIBIR TYPE 'E'.
      RETURN.
    ENDIF.


  ENDIF.

  CLEAR: WA_ZLEST0063.
  SELECT SINGLE *
    INTO WA_ZLEST0063
    FROM ZLEST0063
   WHERE BUKRS       EQ WA_ZLEST0061-BUKRS
     AND WERKS       EQ WA_ZLEST0061-WERKS
     AND ANO_VIAGEM  EQ WA_ZLEST0061-ANO_VIAGEM
     AND NR_VIAGEM   EQ WA_ZLEST0061-NR_VIAGEM
     AND ( ( EMBARCACAO  EQ 'E') OR
           ( EMBARCACAO  EQ 'R' ) ).

  IF SY-SUBRC NE 0.
    CLEAR: E_XML.
    ROLLBACK WORK.
    MESSAGE 'Não encontrado Empurrador/Rebocador do comboio!' TYPE 'E'.
    RETURN.
  ENDIF.

  CLEAR: WA_ZLEST0053.
  SELECT SINGLE *
    INTO WA_ZLEST0053
    FROM ZLEST0053
   WHERE BUKRS      EQ WA_ZLEST0063-BUKRS
     AND NOME       EQ WA_ZLEST0063-NOME_EMB
     AND EMBARCACAO EQ WA_ZLEST0063-EMBARCACAO.

  IF SY-SUBRC NE 0.
    CLEAR: E_XML.
    ROLLBACK WORK.
    MESSAGE 'Não encontrado cadastro do Empurrador/Rebocador do comboio!' TYPE 'E'.
    RETURN.
  ENDIF.

  "Atribui dados (Rebocador/Empurrador)

  VL_TP_EMB  = WA_ZLEST0053-COD_TP_EMB_MIN. "Cód.Tp Embarcação de acordo com o Ministério Transp.
  VL_TP_IRIN = WA_ZLEST0053-IRIN.
  VL_CEMBAR  = WA_ZLEST0053-APELIDO.
  VL_XEMBAR  = WA_ZLEST0053-NOME.

  IF VL_TP_IRIN IS INITIAL.
    CLEAR: E_XML.
    ROLLBACK WORK.
    CLEAR: MSG_EXIBIR, MSG_AUX01.

    MSG_AUX01 = WA_ZLEST0053-NOME.
    CONCATENATE 'Não cadastrado o IRIN do Empurrador/Rebocador:'
                MSG_AUX01 '!' INTO MSG_EXIBIR SEPARATED BY SPACE.
    MESSAGE MSG_EXIBIR TYPE 'E'.
    RETURN.
  ENDIF.

  IF VL_TP_EMB IS INITIAL.
    CLEAR: E_XML.
    ROLLBACK WORK.
    CLEAR: MSG_EXIBIR, MSG_AUX01.

    MSG_AUX01 = WA_ZLEST0053-NOME.
    CONCATENATE 'Não cadastrado o Código do Tipo da Embarcação(Ministério Transp.) do Empurrador/Rebocador:'
                MSG_AUX01 '!' INTO MSG_EXIBIR SEPARATED BY SPACE.
    MESSAGE MSG_EXIBIR TYPE 'E'.
    RETURN.
  ENDIF.

  IF VL_CEMBAR IS INITIAL.
    CLEAR: E_XML.
    ROLLBACK WORK.
    CLEAR: MSG_EXIBIR, MSG_AUX01.

    MSG_AUX01 = WA_ZLEST0053-NOME.
    CONCATENATE 'Não encontrado o Apelido da Embarcação(Empurrador/Rebocador):'
                MSG_AUX01 '!' INTO MSG_EXIBIR SEPARATED BY SPACE.
    MESSAGE MSG_EXIBIR TYPE 'E'.
    RETURN.
  ENDIF.

  IF VL_XEMBAR IS INITIAL.
    CLEAR: E_XML.
    ROLLBACK WORK.
    CLEAR: MSG_EXIBIR, MSG_AUX01.

    MSG_AUX01 = WA_ZLEST0053-NOME.
    CONCATENATE 'Não encontrado o nome da Embarcação(Empurrador/Rebocador):'
                MSG_AUX01 '!' INTO MSG_EXIBIR SEPARATED BY SPACE.
    MESSAGE MSG_EXIBIR TYPE 'E'.
    RETURN.
  ENDIF.

  "CNPJ da Agência de Navegação
  CONC_XML     '<CNPJAgeNav>'.
  CONC_XML        VL_CNPJAGENAV.
  CONC_XML     '</CNPJAgeNav>'.

  CONC_XML     '<irin>'.
  CONC_XML        VL_TP_IRIN.
  CONC_XML     '</irin>'.

  "Tipo de embarcação
  CONC_XML     '<tpEmb>'.
  CONC_XML        VL_TP_EMB.
  CONC_XML     '</tpEmb>'.

  "Código da embarcação
  CONC_XML     '<cEmbar>'.
  CONC_XML        VL_CEMBAR.
  CONC_XML     '</cEmbar>'.

  "Nome da embarcação
  CONC_XML     '<xEmbar>'.
  CONC_XML        VL_XEMBAR.
  CONC_XML     '</xEmbar>'.

  "Número da viagem
  CONC_XML     '<nViag>'.

  VL_XVALOR = WA_ZLEST0061-NR_VIAGEM.
  CONC_XML     VL_XVALOR.

  CONC_XML     '</nViag>'.

  "Porto de Embarque
  CONC_XML     '<cPrtEmb>'.
  CONC_XML        VL_CPRTEMB.
  CONC_XML     '</cPrtEmb>'.

  "Porto de Destino
  CONC_XML     '<cPrtDest>'.
  CONC_XML        VL_CPRTDEST.
  CONC_XML     '</cPrtDest>'.

  "Frete Aquáviário - Comboio
  "Seleção Barcaças
  REFRESH IT_ZLEST0063.

  SELECT *
    FROM ZLEST0063
    INTO TABLE IT_ZLEST0063
   WHERE BUKRS       EQ WA_ZLEST0061-BUKRS
     AND WERKS       EQ WA_ZLEST0061-WERKS
     AND ANO_VIAGEM  EQ WA_ZLEST0061-ANO_VIAGEM
     AND NR_VIAGEM   EQ WA_ZLEST0061-NR_VIAGEM
     AND EMBARCACAO  NE 'E'
     AND EMBARCACAO  NE 'R'.

  "Código da embarcação.
  LOOP AT IT_ZLEST0063 INTO WA_ZLEST0063.

    CLEAR: WA_ZLEST0053.
    SELECT SINGLE *
      INTO WA_ZLEST0053
      FROM ZLEST0053
     WHERE BUKRS      EQ WA_ZLEST0063-BUKRS
       AND NOME       EQ WA_ZLEST0063-NOME_EMB
       AND EMBARCACAO EQ WA_ZLEST0063-EMBARCACAO.

    IF ( SY-SUBRC NE 0 ) OR ( WA_ZLEST0053-APELIDO IS INITIAL ).
      CLEAR: E_XML.
      ROLLBACK WORK.
      CLEAR: MSG_EXIBIR, MSG_AUX01.

      MSG_AUX01 = WA_ZLEST0063-EMBARCACAO.
      CONCATENATE 'Não encontrado o cadastro do Apelido da Embarcação:' MSG_AUX01 '!'
                  INTO MSG_EXIBIR SEPARATED BY SPACE.
      MESSAGE MSG_EXIBIR TYPE 'E'.
      RETURN.

    ENDIF.

    VL_CEMBAR = WA_ZLEST0053-APELIDO.

    CONC_XML     '<cEmbComb>'.
    CONC_XML        VL_CEMBAR.
    CONC_XML     '</cEmbComb>'.
  ENDLOOP.



  IF ( VL_CTERMCARREG    IS NOT INITIAL ) AND
     ( VL_CTERMDESCARREG IS NOT INITIAL ).

    "----------------------------------------------------------------------
    "Grupo de informações dos terminais de carregamento.
    "----------------------------------------------------------------------
    CONC_XML     '<infTermCarreg>'.
    CONC_XML         '<cTermCarreg>'.
    CONC_XML            VL_CTERMCARREG.
    CONC_XML         '</cTermCarreg>'.

    CONC_XML         '<xTermCarreg>'.
    CONC_XML            VL_XTERMCARREG.
    CONC_XML         '</xTermCarreg>'.
    CONC_XML     '</infTermCarreg>'.

    "----------------------------------------------------------------------
    "Grupo de informações dos terminais de descarregamento
    "----------------------------------------------------------------------
    CONC_XML     '<infTermDescarreg>'.
    CONC_XML         '<cTermDescarreg>'.
    CONC_XML            VL_CTERMDESCARREG.
    CONC_XML         '</cTermDescarreg>'.

    CONC_XML         '<xTermDescarreg>'.
    CONC_XML            VL_XTERMDESCARREG.
    CONC_XML         '</xTermDescarreg>'.
    CONC_XML     '</infTermDescarreg>'.

  ENDIF.

  CONC_XML '</aquav>'.

ENDMETHOD.


METHOD SET_CEMBAR.
************************************
*  Método de Configuração
*  Atributo: AT_CEMBAR
*  Parâmetro: CEMBAR
*  Descrição: Método para configurar o código da embarcação.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:01:12
************************************
  ME->AT_CEMBAR = CEMBAR.
ENDMETHOD.


METHOD SET_CEMBCOMB.
************************************
*  Método de Configuração
*  Atributo: AT_CEMBCOMB
*  Parâmetro: CEMBCOMB
*  Descrição: Método para configurar o código da embarcação do comboio.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:35:22
************************************
  ME->AT_CEMBCOMB = CEMBCOMB.
ENDMETHOD.


method SET_CNPJAGENAV.
************************************
*  Método de Configuração
*  Atributo: AT_CNPJ
*  Parâmetro: CNPJ
*  Descrição: Método para configurar o CNPJ do Agente de Navegação
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 08:55:28
************************************
  ME->AT_CNPJAGENAV = CNPJ.

endmethod.


method SET_CPRTDEST.
************************************
*  Método de Configuração
*  Atributo: AT_CPRTDEST
*  Parâmetro: CPRTDEST
*  Descrição:  Código do porto de destino.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:17:23
************************************
  ME->AT_CPRTDEST = CPRTDEST.
endmethod.


METHOD SET_CPRTEMB.
************************************
*  Método de Configuração
*  Atributo: AT_CPRTEMB
*  Parâmetro: CPRTEMB
*  Descrição: Método para configurar o código do porto de embarque.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:04:36
************************************
  ME->AT_CPRTEMB = CPRTEMB.
ENDMETHOD.


METHOD SET_CTERMCARREG.
************************************
*  Método de Configuração
*  Atributo: AT_CTERMCARREG
*  Parâmetro: CTERMCARREG
*  Descrição: Método para configurar o códigod o terminal de carregamento
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:27:01
************************************
  ME->AT_CTERMCARREG = CTERMCARREG.
ENDMETHOD.


method SET_CTERMDESCARREG.
************************************
*  Método de Configuração
*  Atributo: AT_CTERMDESCARREG
*  Parâmetro: CTERMDESCARREG
*  Descrição: Método para configurar o Código do Terminal de descarregamento.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:30:21
************************************
  ME->AT_CTERMDESCARREG = CTERMDESCARREG.
endmethod.


method SET_IDUNIDCARGAVAZIA.
************************************
*  Método de Configuração
*  Atributo: AT_IDUNIDCARGAVAZIA
*  Parâmetro: IDUNIDCARGAVAZIA
*  Descrição: Método para configurar a identificação da unidade de carga vazia.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:37:55
************************************
  ME->AT_IDUNIDCARGAVAZIA = IDUNIDCARGAVAZIA.
endmethod.


method SET_NVIAG.
************************************
*  Método de Configuração
*  Atributo: AT_NVIAG
*  Parâmetro: NVIAG
*  Descrição: Método para configurar o número da viagem.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:03:53
************************************
  ME->AT_NVIAG = NVIAG.
endmethod.


method SET_TPEMB.
************************************
*  Método de Configuração
*  Atributo: AT_TPEMB
*  Parâmetro: TPEMB
*  Descrição: Método para atribuir o Tipo de Embarcação da Viagem.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:00:13
************************************
  ME->AT_TPEMB = TPEMB.
endmethod.


METHOD SET_TPUNIDCARGAVAZIA.
************************************
*  Método de Configuração
*  Atributo: AT_TPUNIDCARGAVAZIA
*  Parâmetro: TPUNIDCARGAVAZIA
*  Descrição: Método para configurar o tipo da unidade de carga vazia.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:38:39
************************************
  ME->AT_TPUNIDCARGAVAZIA = TPUNIDCARGAVAZIA.
ENDMETHOD.


method SET_XEMBAR.
************************************
*  Método de Configuração
*  Atributo: AT_XEMBAR
*  Parâmetro: XEMBAR
*  Descrição: Método para configurar o nome da embarcação.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:02:54
************************************
  ME->AT_XEMBAR = XEMBAR.
endmethod.


METHOD SET_XTERMCARREG.
************************************
*  Método de Configuração
*  Atributo: AT_XTERMCARREG
*  Parâmetro: XTERMCARREG
*  Descrição: Método para configurar o nome do terminal de carregamento.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:28:58
************************************
  ME->AT_XTERMCARREG = XTERMCARREG.
ENDMETHOD.


METHOD SET_XTERMDESCARREG.
************************************
*  Método de Configuração
*  Atributo: AT_XTERMDESCARREG
*  Parâmetro: XTERMDESCARREG
*  Descrição: Método para configurar nome do terminal de descarregamento.
*  Developer: Victor Hugo Souza Nunes
*  30.11.2015 09:32:36
************************************
  ME->AT_XTERMDESCARREG = XTERMDESCARREG.
ENDMETHOD.
ENDCLASS.
