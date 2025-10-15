class ZCL_UTIL definition
  public
  create public .

*"* public components of class ZCL_UTIL
*"* do not include other source files here!!!
public section.

  class-data AT_PARAM type STRING .

  methods CONSTRUCTOR .
  methods DESTRUCTOR .
  class-methods CONV_DATA_BR_US
    importing
      !I_DATA type CHAR10
      !I_OPCAO type CHAR1 optional
    returning
      value(E_DATA) type CHAR10 .
  class-methods CONV_DATA_US_BR
    importing
      !I_DATA type SY-DATUM
      !I_OPCAO type CHAR1 optional
    returning
      value(E_DATA) type CHAR10 .
  class-methods PING_TIPCARD
    importing
      !I_URL type STRING
      !I_COD type CHAR4
      !I_TIPO type CHAR4 .
  class-methods MONTA_CHAVE_NFE
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_VALIDA type CHAR01 optional
    returning
      value(E_CHAVE) type CHAR44
    exceptions
      ERRO_CHAVE .
  class-methods FIELD_STYLE_EDIT
    importing
      !I_FIELDNAMES type ANY TABLE
    changing
      value(E_STYLE) type LVC_T_STYL .
  class-methods GET_STRUCTURE_DESCRIPTION
    importing
      !STRUCTURE type STRING
    returning
      value(R_TABLE) type LVC_T_FCAT .
  class-methods CONVERT_ROMANOS
    importing
      !NUM_DEC type N
    returning
      value(VALUE) type CHAR50 .
  class-methods GET_SALDO_MB5B
    importing
      !DATUM type ERSDA
      !MATNR type MATNR
      !WERKS type WERKS_D
      !LGORT type LGORT_D
    returning
      value(SALDO) type MENGE_D .
  class-methods GET_HTML_FUNDO
    returning
      value(R_HTML) type STRING .
  class-methods GET_DESC_VALUE_DOMAIN
    importing
      !I_DOMNAME type DOMNAME
      !I_DOMVALUE type DOMVALUE_L
    returning
      value(R_TEXT) type VAL_TEXT .
  class-methods CONV_EXT_DATE
    importing
      !I_DATE type SY-DATUM
    returning
      value(E_DATE) type CHAR10 .
  methods GET_CHAVE_NFE
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(E_CHAVE_NFE) type ZDE_CHAVE_DOC_E .
  methods GET_ATRIBUTOS_NFE
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_NFE
    returning
      value(E_CAMPOS_NFE) type ZDE_CAMPOS_NFE .
  methods GET_DOCNUM
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_NFE
    returning
      value(T_DOCUMENTOS) type J_1B_TT_NFE_ACTIVE .
  class-methods GET_STRING_NUMERIC
    importing
      !I_STRING type STRING
    returning
      value(E_SAIDA) type STRING .
  class-methods GET_AJUDA_PESQUISA
    importing
      !I_AJUDA_PESQUISA type SHLPNAME .
  PROTECTED SECTION.
*"* protected components of class ZCL_UTIL
*"* do not include other source files here!!!

    DATA AT_FIELDNAME TYPE STRING .
    DATA AT_STATUS TYPE STRING .
private section.
*"* private components of class ZCL_UTIL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_UTIL IMPLEMENTATION.


  METHOD CONSTRUCTOR.
  ENDMETHOD.


  METHOD CONVERT_ROMANOS.

    IF NUM_DEC IS NOT INITIAL.

      IF '1234567890' CS NUM_DEC(1).
        CASE NUM_DEC.
          WHEN'1'. VALUE = 'I'.
          WHEN'2'. VALUE = 'II'.
          WHEN'3'. VALUE = 'III'.
          WHEN'4'. VALUE = 'IV'.
          WHEN'5'. VALUE = 'V'.
          WHEN'6'. VALUE = 'VI'.
          WHEN'7'. VALUE = 'VII'.
          WHEN'8'. VALUE = 'VIII'.
          WHEN'9'. VALUE = 'IX'.
          WHEN'10'. VALUE = 'X'.
          WHEN'11'. VALUE = 'XI'.
          WHEN'12'. VALUE = 'XII'.
          WHEN'13'. VALUE = 'XIII'.
          WHEN'14'. VALUE = 'XIV'.
          WHEN'15'. VALUE = 'XV'.
          WHEN'16'. VALUE = 'XVI'.
          WHEN'17'. VALUE = 'XVII'.
          WHEN'18'. VALUE = 'XVIII'.
          WHEN'19'. VALUE = 'XIX'.
          WHEN'20'. VALUE = 'XX'.
          WHEN'21'. VALUE = 'XXI'.
          WHEN'22'. VALUE = 'XXII'.
          WHEN'23'. VALUE = 'XXIII'.
          WHEN'24'. VALUE = 'XXIV'.
          WHEN'25'. VALUE = 'XXV'.
          WHEN'26'. VALUE = 'XXVI'.
          WHEN'27'. VALUE = 'XXVII'.
          WHEN'28'. VALUE = 'XXVIII'.
          WHEN'29'. VALUE = 'XXIX'.
          WHEN'30'. VALUE = 'XXX'.
          WHEN'31'. VALUE = 'XXXI'.
          WHEN'32'. VALUE = 'XXXII'.
          WHEN'33'. VALUE = 'XXXIII'.
          WHEN'34'. VALUE = 'XXXIV'.
          WHEN'35'. VALUE = 'XXXV'.
          WHEN'36'. VALUE = 'XXXVI'.
          WHEN'37'. VALUE = 'XXXVII'.
          WHEN'38'. VALUE = 'XXXVIII'.
          WHEN'39'. VALUE = 'XXXIX'.
          WHEN'40'. VALUE = 'XL'.
          WHEN'41'. VALUE = 'XLI'.
          WHEN'42'. VALUE = 'XLII'.
          WHEN'43'. VALUE = 'XLIII'.
          WHEN'44'. VALUE = 'XLIV'.
          WHEN'45'. VALUE = 'XLV'.
          WHEN'46'. VALUE = 'XLVI'.
          WHEN'47'. VALUE = 'XLVII'.
          WHEN'48'. VALUE = 'XLVIII'.
          WHEN'49'. VALUE = 'XLIX'.
          WHEN'50'. VALUE = 'L'.
          WHEN'51'. VALUE = 'LI'.
          WHEN'52'. VALUE = 'LII'.
          WHEN'53'. VALUE = 'LIII'.
          WHEN'54'. VALUE = 'LIV'.
          WHEN'55'. VALUE = 'LV'.
          WHEN'56'. VALUE = 'LVI'.
          WHEN'57'. VALUE = 'LVII'.
          WHEN'58'. VALUE = 'LVIII'.
          WHEN'59'. VALUE = 'LIX'.
          WHEN'60'. VALUE = 'LX'.
          WHEN'61'. VALUE = 'LXI'.
          WHEN'62'. VALUE = 'LXII'.
          WHEN'63'. VALUE = 'LXIII'.
          WHEN'64'. VALUE = 'LXIV'.
          WHEN'65'. VALUE = 'LXV'.
          WHEN'66'. VALUE = 'LXVI'.
          WHEN'67'. VALUE = 'LXVII'.
          WHEN'68'. VALUE = 'LXVIII'.
          WHEN'69'. VALUE = 'LXIX'.
          WHEN'70'. VALUE = 'LXX'.
          WHEN'71'. VALUE = 'LXXI'.
          WHEN'72'. VALUE = 'LXXII'.
          WHEN'73'. VALUE = 'LXXIII'.
          WHEN'74'. VALUE = 'LXXIV'.
          WHEN'75'. VALUE = 'LXXV'.
          WHEN'76'. VALUE = 'LXXVI'.
          WHEN'77'. VALUE = 'LXXVII'.
          WHEN'78'. VALUE = 'LXXVIII'.
          WHEN'79'. VALUE = 'LXXIX'.
          WHEN'80'. VALUE = 'LXXX'.
          WHEN'81'. VALUE = 'LXXXI'.
          WHEN'82'. VALUE = 'LXXXII'.
          WHEN'83'. VALUE = 'LXXXIII'.
          WHEN'84'. VALUE = 'LXXXIV'.
          WHEN'85'. VALUE = 'LXXXV'.
          WHEN'86'. VALUE = 'LXXXVI'.
          WHEN'87'. VALUE = 'LXXXVII'.
          WHEN'88'. VALUE = 'LXXXVIII'.
          WHEN'89'. VALUE = 'LXXXIX'.
          WHEN'90'. VALUE = 'XC'.
          WHEN'91'. VALUE = 'XCI'.
          WHEN'92'. VALUE = 'XCII'.
          WHEN'93'. VALUE = 'XCIII'.
          WHEN'94'. VALUE = 'XCIV'.
          WHEN'95'. VALUE = 'XCV'.
          WHEN'96'. VALUE = 'XCVI'.
          WHEN'97'. VALUE = 'XCVII'.
          WHEN'98'. VALUE = 'XCVIII'.
          WHEN'99'. VALUE = 'XCIX'.
          WHEN'100'. VALUE = 'C'.
          WHEN OTHERS. VALUE = 'Digite um Valor inferior a 100!'.
        ENDCASE.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD CONV_DATA_BR_US.
**********************************************************************
* Método para converter data brasileira para a americana.
*
* Parametros: I_DATA  (Data no Formato Brasileiro)
*             I_OPCAO (Opção para retornar a data com pontos ou barra).
*
* Retorno: E_DATA (Data US).
*
* Versão: 1.0
**********************************************************************


    DATA: DIA TYPE C LENGTH 2, "Variavel DIA
          MES TYPE C LENGTH 2, "Variavel MES
          ANO TYPE C LENGTH 4. "Variavel ANO

    DIA = I_DATA(2).   "Seleciona o dia da DATA BRASILEIRA
    MES = I_DATA+2(2). "Seleciona o mes da DATA BRASILEIRA
    ANO = I_DATA+4(4). "Seleciona o ano da DATA BRASILEIRA

**********************************************************************
* I_OPCAO.
* Caso o parametro seja '.' (ponto) formatar a data com ponto no intervalo de valores.
* Caso o parametro seja '/' (ponto) formatar a data com barra no intervalo de valores.
* outros ficar sem tratamento e montar a data somente no formato brasileiro.
*
**********************************************************************
    CASE I_OPCAO.

      WHEN: '.'.
        "************************
        "* Exemplo: I_PONTO = '.'
        "*      DIA = 01.
        "*      MES = 03.
        "*      ANO = 2013.
        "*      ANO '.' MES '.' DIA INTO E_DATA
        "*      SAIDA = 2013.03.01
        "************************
        CONCATENATE ANO '.' MES '.' DIA INTO E_DATA. "Concatena a data com ponto.

      WHEN: '/'.

        "************************
        "* Exemplo: I_PONTO = '/'
        "*      DIA = 01.
        "*      MES = 03.
        "*      ANO = 2013.
        "*      ANO '/' MES '/' DIA INTO E_DATA
        "*      SAIDA = 2013/03/01
        "************************
        CONCATENATE ANO '/' MES '/' DIA INTO E_DATA. "Concatena a data com barra.


      WHEN: '-'.

        "************************
        "* Exemplo: I_PONTO = '-'
        "*      DIA = 01.
        "*      MES = 03.
        "*      ANO = 2013.
        "*      ANO '-' MES '-' DIA INTO E_DATA
        "*      SAIDA = 2013-03-01
        "************************
        CONCATENATE ANO '-' MES '-' DIA INTO E_DATA. "Concatena a data com traço.


      WHEN OTHERS.

        "************************
        "* Exemplo: I_PONTO = space ou não esta parametrizado.
        "*      DIA = 01.
        "*      MES = 03.
        "*      ANO = 2013.
        "*      ANO MES DIA INTO E_DATA
        "*      SAIDA = 20130301
        "************************

        CONCATENATE ANO  MES  DIA INTO E_DATA.  "Concatena a data sem tratamento de separação.
    ENDCASE.



  ENDMETHOD.


  METHOD CONV_DATA_US_BR.
**********************************************************************
* Método para converter data americana para brasileira.
*
* Parametros: I_DATA  (Data no Formato Americano)
*             I_OPCAO (Opção para retornar a data com pontos ou barra).
*
* Retorno: E_DATA (Data BR).
*
* Versão: 1.0
**********************************************************************

    DATA: DIA TYPE C LENGTH 2, "Variavel DIA
          MES TYPE C LENGTH 2, "Variavel MES
          ANO TYPE C LENGTH 4. "Variavel ANO

    DIA = I_DATA+6(2). "Seleciona o dia da DATA AMERICANA
    MES = I_DATA+4(2). "Seleciona o mes da DATA AMERICANA
    ANO = I_DATA(4).   "Seleciona o ano da DATA AMERICANA

**********************************************************************
* I_OPCAO.
* Caso o parametro seja '.' (ponto) formatar a data com ponto no intervalo de valores.
* Caso o parametro seja '/' (ponto) formatar a data com barra no intervalo de valores.
* outros ficar sem tratamento e montar a data somente no formato brasileiro.
*
**********************************************************************
    CASE I_OPCAO.

      WHEN: '.'.
        "************************
        "* Exemplo: I_OPCAO = '.'
        "*      DIA = 01.
        "*      MES = 03.
        "*      ANO = 2013.
        "*      DIA '.' MES '.' ANO INTO E_DATA
        "*      SAIDA = 01.03.2013
        "************************
        CONCATENATE DIA '.' MES '.' ANO INTO E_DATA. "Concatena a data com ponto.

      WHEN: '/'.

        "************************
        "* Exemplo: I_OPCAO = '/'
        "*      DIA = 01.
        "*      MES = 03.
        "*      ANO = 2013.
        "*      DIA '/' MES '/' ANO INTO E_DATA
        "*      SAIDA = 01.03.2013
        "************************
        CONCATENATE DIA '/' MES '/' ANO INTO E_DATA. "Concatena a data com barra.

      WHEN: '-'.
        "************************
        "* Exemplo: I_OPCAO = '-'
        "*      DIA = 01.
        "*      MES = 03.
        "*      ANO = 2013.
        "*      DIA '-' MES '-' ANO INTO E_DATA
        "*      SAIDA = 01.03.2013
        "************************
        CONCATENATE DIA '-' MES '-' ANO INTO E_DATA. "Concatena a data com barra.


      WHEN OTHERS.

        "************************
        "* Exemplo: I_OPCAO = space ou não esta parametrizado.
        "*      DIA = 01.
        "*      MES = 03.
        "*      ANO = 2013.
        "*      DIA  MES  ANO INTO E_DATA
        "*      SAIDA = 01032013
        "************************

        CONCATENATE DIA  MES  ANO INTO E_DATA. "Concatena a data sem tratamento.
    ENDCASE.

  ENDMETHOD.


METHOD conv_ext_date.

  TRY.
      cl_abap_datfm=>conv_date_int_to_ext(
        EXPORTING
          im_datint    = i_date
*        im_datfmdes  =
        IMPORTING
          ex_datext    = e_date
*        ex_datfmused =
      ).
    CATCH cx_abap_datfm_format_unknown.    "
  ENDTRY.


ENDMETHOD.


  METHOD DESTRUCTOR.
  ENDMETHOD.


  METHOD FIELD_STYLE_EDIT.
****************************************************************************************************
*  Método...: Habilitar/desabilitar campos do ALV
*  Autor....: Enio Jesus
*  Data.....: 18.01.2015
*  Descrição: Método é responsável por habilitar/desabilitar campos do ALV, quando passado 'X' para
*  LW_FIELDNAMES-STYLE habilita o campo, quando passado ' ' desabilita o campo.
****************************************************************************************************

    DATA: LT_FIELDNAMES TYPE TABLE OF ZSTYLE_FIELDNAME,
          LW_FIELDNAMES TYPE ZSTYLE_FIELDNAME,
          LW_STYLE      TYPE LVC_S_STYL.

    LT_FIELDNAMES = I_FIELDNAMES.

    SORT LT_FIELDNAMES BY FIELD.

    LOOP AT LT_FIELDNAMES INTO LW_FIELDNAMES.
      LW_STYLE-FIELDNAME = LW_FIELDNAMES-FIELD.

      DELETE E_STYLE WHERE FIELDNAME
          EQ LW_STYLE-FIELDNAME.

      IF ( LW_FIELDNAMES-STYLE = 'X' ).
        LW_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LW_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.

      APPEND LW_STYLE TO E_STYLE.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_atributos_nfe.

    FREE: e_campos_nfe.

    CHECK i_chave_nfe IS NOT INITIAL.

    e_campos_nfe-regio    = i_chave_nfe+00(02).
    e_campos_nfe-nfyear   = i_chave_nfe+02(02).
    e_campos_nfe-nfmonth  = i_chave_nfe+04(02).
    e_campos_nfe-stcd1    = i_chave_nfe+06(14).
    e_campos_nfe-model    = i_chave_nfe+20(02).
    e_campos_nfe-serie    = i_chave_nfe+22(03).
    e_campos_nfe-nfnum9   = i_chave_nfe+25(09).
    e_campos_nfe-docnum9  = i_chave_nfe+34(09).
    e_campos_nfe-cdv      = i_chave_nfe+43(01).

  ENDMETHOD.


  METHOD get_chave_nfe.

    FREE: e_chave_nfe.

    SELECT *
      FROM j_1bnfe_active
        UP TO 1 ROWS
      INTO @DATA(w_active)
     WHERE docnum = @i_docnum.
    ENDSELECT.

    CHECK sy-subrc = 0.

    CONCATENATE w_active-regio   "Região do emissor NF-e
                w_active-nfyear  "Ano da data do documento da NF-e
                w_active-nfmonth "Mês da data do documento da NF-e
                w_active-stcd1   "Nº CNPJ do emissor da NF-e
                w_active-model   "Modelo da nota fiscal
                w_active-serie   "SERIE
                w_active-nfnum9  "Nº NF-e de nove posições
                w_active-docnum9 "NF-e: nº aleatório
                w_active-cdv     "Dígito controle p/chave de acesso NF-e
           INTO e_chave_nfe.

  ENDMETHOD.


  method GET_DESC_VALUE_DOMAIN.

  CLEAR: R_TEXT.

  CHECK ( I_DOMNAME   IS NOT INITIAL ) AND
        ( I_DOMVALUE  IS NOT INITIAL ).

  SELECT SINGLE DDTEXT
    FROM DD07T INTO R_TEXT
   WHERE DOMNAME    = I_DOMNAME
     AND DDLANGUAGE = SY-LANGU
     AND DOMVALUE_L = I_DOMVALUE.

  endmethod.


  METHOD GET_HTML_FUNDO.

    DATA(IMAGEM_AMAGGI) =
'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOAAAAAiCAYAAABGDdVeAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsQAAA7EAZUrDhsAACYxSURBVHhe7VwJfFXVmf/yspKVJATCIigoCEpVlCquaKeMiFupI67oiExB0CqCdBhaRBlaxCKtIq' &&
'jFWtx1BKFFGGfEBSUqiyiCsgjImhAe2ffkZf7/75zz3n0vLyGBznTmN/z18N6799xzvvPtZ7mJaQTkBE7gBP4m8NnPEziBE/gboI0RsNZ+OruNQeF3frYO9Shxwi7xTMDTtc/+5qc0mGsSaz+PEYGA+Wz0+JmYRgn4zPUA2o/Tb21DwNLnC9JJkNa20OvGHskH0hbpF+04IhGw9cAz0m' &&
'ToSQhvz/Xji2yTcPUCYT2Ydny4xnZDY4oLG69BfRgPKV3TUj3psDDy5nW0FUabQT3oj/M2DRkpVBeODvZKBPWKUF0yX7XrRk9bsaF6TZ4Nk4WnLUJ/e+5rG+Z3Pb4fiy61yQApZK9AiLZ2Gs0AnUFEwtcmhY4GGJtyP4SQGrpvlsltQqMyPBLHIoAgLC+ojNFwtLa9ihQ5ZiI6L9mnqc' &&
't/nUOhjCPlTIQU3H1aqLKb665vn/I3ko7oBliDZ9lbnG2nBt/1N2+2Ahw7KXcUOzojjcv9bg6mnjGmSPCeu+7qEbzmfEei/WwL2mSAxngsHNODzG8dTBuuS/eci6wUWUj4x6XQgFcwVK5gxArEQ1fQNz0t9IRKYxSm9eNw46ayOCRaQR8rSG+QRiD03YygCQJVuEwu4X6A/aKoMzPPOT' &&
'7yMzovjcGYlskI0M9swTwGsB2OyRkS+BYJ5QPr8Dvq++zDAXBGaSNMNG3eCdTpNyd7/mJPxyx/0kR4ZExDYbscqzEU1sF9HbOr19RoWS/M8MgKF011OKxH2GttRNtSUJfSBcm08DHtwb2oaU4kTHdeL2MG5hlI2ACPA05/rRCMsqBtx0R3XUXDzlrbIRqmwkNZ6/FIHB1IADzxUbRtJd' &&
'oImKTG419fUHGbpoCRIN9CEcdCDZEgfe47ELUZ9mp6DvKEiKEsnTwcYiOlrgg5U0OHGiFhCfLSH93J4fmg7Al7vw1OPfx5Cz5vSVE5Eyp7fA/K3vObAKlBA4y4HozKEddV790UJzTUVqONBmgiVQBerga9OU/VTo2PRFnCWoTpzikdxWb8n2lLPZ8b5DEMKAxOAEAN2iJzWZJQ2C/Zpk' &&
'qvFdlZ6zpkG6SZbfBJw4NKNMh5Txv8NscJImrAC7bFQrD9/eUVUlleK9v3+6W0xC/lleXmpkVqcqokJCbIyZ1zJS05SZJTE6RdSoqkgm3JuG/GBmocD6IODTdhMJRnna3g6MBopBA0FBwqld0H86W2pjYqDekZ2ZKbmaI0dM5Ol4T4WO3fgXQkWh7TGENRMFwPCHcnzjn6ozl0W68K9a' &&
'iZoTwqBCcRukbVK/Rbgfocn4ObrXqfT0dJRH1OCWh83vqR4HhTzNc2o3kDVOUINygqBolZv2OPLHzjL1JYWiXXDblY7rzifGWey+FZr3k1NN3Rc9OIj9Q1yNNvLJX1X38jfbp1lp/dMUJ6pKZIOxVaVK1pBm4YXpopdJFytLMtv0iefP5VKSgrklsuHShDhw5Rxmk/GnWaMx5vCkJV8U' &&
'kpfj/5b+/KJ+s2SZfcLjLmpivljNxMtBUN0egCrDKQXzTAavz6EnxdvfFbWf7ZFsnbdQQMT9WqRoucmlpQ+ZhK1zMNxb3GWhnUM1fap8fLmb1Okr49TpL+vU6WvqArRVWL5hhBA8eDsdf7ktWhUBbrv/pGlq3eJB9/vUO2FlvXGAf1jWYMjgaOJYARxFQpDf26dpJTumVL/1O6yLk/6C' &&
'tZMMqQgzI0cPGGI1J92nlQ/jMvT8qLi+WRcXdJGmWitY4ifzXAGHn367Xy7Orleikhne41HBf1/IFcecGlkhubpr9nvTxfthTnR61L1JZWy6wxEyUH9fP9B+WRRU9LoIPXrYTjzKxuMnrY7ZITxWaOhugGyIaCYZoX8F0bb5B1+WUycOwjIhkn4zqYX1Qgk0acK9NuuVZSlCH18DAJxu' &&
'Pw0TB45xwNUgLNunXaQlm+NR9hCcpWWSN90mvkpRnjZUBuOmoaAbAdGjUFpoaOEg4OgcqCu56VQYHQA0gLP9x5SK54aD5cdjaug0Z/sUy4+iyZMXq4VQy26PxgJGqlAvdYIxF1S6CsD859VRZ+vEekfa4IBCmNRbJ23mQ5D8pOPrmFFHp9H2kjX3zxasjOjBItr6rAq0+hgL/90zuyfP' &&
'NhQ3cyuJcIc2aVoiLQDUUpLdbnwpAAqmgYCVQOGhh5wE+gvExkz3bZ8NZsOaerkUaNGoDx7Aof6iN1LsSYFq/6TJ545V3ZegSdZlG2QCzNAyhC3zR0ooFEAbGWz3GgE5GwCRgtYfeDTkuQj2bfD+eMHzo/NBzYXxcrLyx9T15c+RkMHXRh3IN6JMhK1E1v1mFEAcZSiGd//uKvZZn/K0' &&
'nKSoTaxklCg3m2NrZRqooq5JykLvLC6IelC4zqy7Ld8ncL/zmsbk2ccTb8zfrXZv9A5t/+zyr3OR+8LrM2vi3tEOm97RL8XVJaLr8fNkZGnnaZSoAg9cZht+xEWjbAWDbgGomVXbh8w70Py4ZyKFo8mE7PW1chcrhAVs39uVzSszM6BfPoFQmriCG49ng/AGMulYF3PSrSsQcuUYkg1L' &&
'py6RNXqkZ4NhQ6lI4wTWt+scQsw1tgHlRv0wx61ysm/U4krbPtA4OogzKV7JZ9bzwuXZUUtBfNwytq0TZTJ+O1X/zkC7nrsbdBc3dzmzw4vFOGnpYjb/7mQTvB50g4Tn7iSXVeZLO5RrDNMpSnEEmnPr/S8sDGUNJX7kf1Mplw1SXSu1dX6X1yd4kN1EkDDDkhI0lqS6rlyKEy2V1wSA' &&
'4cOiR52/ZI3sEDzMcgqiwYLQZWUqCOwfCxUVcbzTyT/DPGz6g3+pdzZcWWQpEOnUCDTaasXPt17yK3DD5dep7SU1NNwtFBMD09cLBQdu3zS96OQtlyAA7DB4cRB5cJpzqsR6q8Pn2UKrLLjCjRn/7it7JiG4w0PQe/iDoZlNOoBpiqsmxZcb2gazgAXv1o9jip6GycDA3DayTxh2qkb1' &&
'onefqe2ZKF4b/0yXKZvPYlqeoUJ5lwBgSNj3D1J10yQm6/aBjkVCqTXpwnH9bt1DYj2yZY/53x86Ubsrf0oP2Y7Mm0Gh0tzwF1zlevKQpV54GXl8j8xZ+JZPbV2xJAmkSBNUDtqjfL5gXT5HR4a18DBuQmuWFgKywmGmxEWjjwZ3OhfF1xDc9YBkh9hfRLrZc/Tb3dpHY2WhgjIbMi23' &&
'XCxb+oWw/Fou/e6C+Si/9pljG+uPZaT2LYDmg49I0aYCekR2ZuADShl6BD4TOJxmGMfwKRr5e5xQhBRSXK6+TxWy+UMdddbNNnM+dhMaMibeSnQRV4OvW5xTJnxSaR7G7mYp2NOOX58vwDP5GrLzoHysg0GY8q38JBJeW4+RSdTTmGsXvXQdm2e48s/WSdrPjwQ1n70jNyXkeMXR2YoY' &&
'sWwHnft+XVcsYdcICJGZjEdGCTqIdG4FCYSv7r2BFyIZwqQQlwLhcJ55jo+TlDdPPGNRs2aSpdU1UsHz31sNYj2EYV6o9g5nMArTbCYGLIR6SvMMAVMEBQ0zZAfhUY3+vbP5T7li/QSBUJGgoj1T+eNVR+c/lIdZSjX50pf67bZioAzgAJZ4SvjXtELkjrJp+W7dOoGa1twkXN3yFq5l' &&
'Bf7QId0ZIBOr6EgUI1Aue3OKRgIovX75D5r8P4MuAlSwtwC1dpfG7VLZApc/6wTL26Gl+bYCklYqikdbLlSIXcNvUp2Qwj5SSbdHBV0M3HooORL0GVUY3vbqTKVC69hbGwENQmoDbO9svmoxofq6IOUtRSfN732z+ZNJYKU4+0rAa+lzxgyciWiQv/Ip8j4pbb8Tjmaq/BSA5+wvheQy' &&
'Sd8+fPYXxQ8KDg69TxbH5hstwI48vBk5oiwwkwlfUWg1pwpVbneDkw7lPgSy8/uZOMwpz8tV+Ok81v/1HngGw3CCUjXuU0ccYCsB7RMoE8ArU0Pv8+mXDlD2UpDIEZDVPlRLQdzfgIzuN5h/dzQEe/5BjQkCETbhiixrTsiV9ialKp42jHcWDOSeUP1IB/dRibk4kXdIjOKbYSdHrXIA' &&
'X8h0791RgiwYiVkZ4qf/xyhazauVGvTb3xXkmFE0istxmbRSNoYkpa1zFRJr38pHzXUCX90k7WNDNa2wQNkynwAqSrXOTRleRWwOlIs6BCc0XulhnzMDdA2lVZKWOvOw3pzXZwH8yjB2MkTOkoCz/YK69hLsFUR+daLSDoFRrpv61wqdgN5XLT4P5ou0S21qerETJNYkqpxtAMTMpn/u' &&
'WCixoftwWSkkx7DaWmMPoRuJdQbyMCFDRahCG4OliDOSAXXfK2mwg2qH2lplYKOiAWpM7kz/g5b2pEIi0OwbHC+KrQVjHGc9cTS1Afxsdq5B+fxzyPUZ+LUCmMljRa0GlOlbCVUNEUTVN93sOndkdDq9e0nWlQv+RkXZ1jtAuqOb9DPovhAFZs3Wfmm/FsB32V+WXoqRnyCObGTAzj1P' &&
'ir8UwCxGn4H1mY0rIY4Bp5DtDguMTRARkG6eV18pGfrO1LRFRmVuKJOscKd5CDiyCP3TxF53sOLkUkaIQ0lHvfmCPfI2XtgfngkzdO0MgYiaL4BjXCL6oPyNyPlqhM7zztYjVwGigL4b6zsO2Fny6XTZhjMmgwswpmV83AZ5uxBbAPkLlcsKcIJj7xAoQEZcHEftQFPWX6rT+RKTdcAo' &&
'XBvMESoukT5hBjnlopm2AATEtUqdXzMxemahihqbCDQiPcNaC4VK7t310+nHUP+vPrStyQB+Zpukpj0cm8hWnTFKY1VMAte5HWjn8WGtBRBnROko9n3yl9OyNqVUORNGKjmgcumhiFNnwwxmiuU7nWIKpNfSUPEkY7h76XP0wbLQ+PHQ6FPWjrQfs5f4tL0TnQb5BaVtFQoLSh9JZKzv' &&
'GLvPv5V6gLRXVOKpaL3iIThvaX/ohYNBpdsNBFC5fCGtpChR80fn6nksWjOxoAnwM9vMdi6zJS0plQBlSmN1etwVQCDpVzeQXqFRfKpH+6xfSvxo+eYXzKYxgt9/wii4nIqIb/nHw5ZaHjIPXGHOPVgPnbzD8tmEUpQtfMNzu+KKBsjHyoB/jE/9o/9x8xXsb72bfeq5HKO1fzGgrniT' &&
'PeeFJ1+7KeZ8uU066S6iPGbRJMRbNqDeVcqGHU/BRRk2NzUZN12BbhTV1d1OScNPJUk9NVb3RXv2nSOhR7w0QFfvHJq4hoKzbu1ZWuPu1r5V9+frPOS+658ToZ1AupCz13LEVm0xxMqn8193nBVFwFbro0kca2Gg6uoqlaElQeszR86andZdVjY8G5WtlyuEwj4UF0wYjsBkHlZrtqmL' &&
'i0Lr9CBtyPOV9CQPok18uLv5kkZ2ZnSgaitibkKnzS5AWvKRvw4RZ6SCk6g7Gzz7Ez/2BSxRK/LJg4QnojQtFQnp8wAga5H3VZH+1SIKg3Z8nHurLJeYlRJhbyl79FPln7rfJWEcBzFGDREblsQG+rsKxPelGHMlG58FlvATiUoJDNSRM1AP2JZ+xUQOWAeSyrE4zAKzbswPMwfFUeQ0' &&
'u/nidL75M64hueU/pM5GUNo2KRNLDwDls2iw2uMDUNIkwRDR2BOEZWp/SWF0FgBBHK2xRwaOQPUz3wlXrBhrlb0xfp4uSzr9fI5o2ADjScNws2yYcwKnJoxJXXa9R0dfnpfY5G6KImV1EZNV0q6jVEggbPqPnK6nd06qZDo3qg2A8SwH8VOnJ6MoUdNPN0KuE6RJ0xc5Eqcd5XVCDzp9' &&
'wtHVGF6QVTi98/eIfIEUYBjJwRgIQgnVmxu1qeRcrGvS1lDLwp2wulKs2BXtcR1yA/xByEq3h9UgOyFQP+0diZsg3pME3dpB1VltZGjboDx8H4QOuArBp5b/4UzKeQymlbR4eyUA0avICyGk+eINPmvSFby8GmCr8M65stN2F+xWjCuREXSUZdfjoiITIBprYuve3YQ1deqegmxyCtvF' &&
'evXperhTrvYkR2iG+ULl25GEWEBNRWOANQgJduzqxRCeC9776H0+DCGaHKA9rq6uWU7BRprynjsffvwK0O9sXWaWaMBsodjeoWakDK+VaDzoSyUZaqo/Br0XSZJaZSMvB51+ChcmGHU/QZB2+k4nyQRnUYRsX9QUZNLrq4qMnirX84qU6jJpPVSxE1aeAuarp6zhDZ9u/ff0s+goFXkJ' &&
'fWEVqOG39l4TMpkmW4ph38Eid+1NJFB85TkBY+fteP5QIYhNnUJYNrdYXy8VFXwzgPqwDNKh7aS8uVqS+vkA2IAkqi9fYmsjQHQ6SLwlSKFCnURYSFs8ZLH+TXTEdv/8Vs2QUjZMrJ+QQXNLg3OeiBh6DEaZj3xOqqW1fk8ExVgizknhf78DC1CdT4EQPRbjnaf2f917JwDaI/9yirDs' &&
'ljk27STWJdFW30aboz4e5r1UHoaqiuiKINzm0SM2TK7Gd0D9GkZmyf/4guMEk8xMHoR1BwiPQ8zdI2dYwG0kcZ1SnvyXE6Qjouw/1GKS7BXFgzDwtLV25msvt6nGBPVcp/fuOYyJnDnMtb7fPVR98EPxroxHVrCvIMVO2Q0oOfin/bEvHveElKC97DNcxrgVRJl8nDxwSNyhmJ+6SBMV' &&
'2kURVLqS6ycNuB9QlvVCM4v2PU/PP2D9VabrnkKrkkvltYJPS2ndAjQw08HwZe5YIKdNuM3um4Y71TelRkCsb9IUYwPY0BMNUcqcvrIIqaBAWlkfLh0bg+7IwOZuOVk3nuYbES5hdM3RgF6tUMmBoxpYmGaGLHtYA5ETMAaeTiuRNkQK5PNuwvUSM8BJLpJLjaOXDir5FhZsqwPrmS99' &&
'R9kmI0TaMjUa/RiX1YRjQL1KMtod19UNjrZiw0ixSI8kunjtLFEZ1rcHkBnOR8lOkoMwPuuYUhpYMs+nSPvPHJFxqxXRqoqCWPPKDgWCmIkICOCZhvFkKGnLuOm/uqnH/3o3LGmOnGGYIHFaQ/aICWWSAgv6jSutfj7B+yY8Rl+v4CpjD3zHxa2v/kAT3xFB3qnloF5T8iXrX/Mzm8+X' &&
'WRPR9IZtEXWur3rZTqL/8g/s1/kQw47/OSu8uvfjoqaIQONBQaGBdZaFTPf7CC0pEbLxomF3btG6zrjNDVpxFym2NL2W7pgKg5beQ46VAdH9a2g9fA88nOZnYGwjSfxsfw/u3eIpn64idwiTCsQ99rqpmtgiKjzGICVyXpjdIhMkYGRR04ztSwwcwLt1bGyZwXluqTQc+n/0aC7UYOwk' &&
'zmOXnnPmAvKDqXtAed2kM2HKzWAwGzX1kmoyY/hRSwnYw850J5efooSUf6UcKuGoyxG6UDkuhxj6ZYuI8UiWx/aPZrupAj1eUy4ZofyuBzz9TldI6/BmO3pq2pzwBkBly6l1KqL9pQwWGkWd11tZNbKU7FlKoEKL9mC1b5WT8hQffP6Kpa3mppGZx7LVu/Wa4c/7Bccf/vZOHGItkq3a' &&
'Vf+2529JYHNfDcLmUmILeD/r1y5GgsagV4tG7ay4ul240TZczTH8iinUi1GzvDl3GLPWTyBuG/jo56GN+3Ughjy/R9L+1iS/RqQ2ydpMQVSXzCEWlXtk6jIlNRbk1c3uP04EonjcUbEd3K5X7/QT2aeN/1N4etirKeq08jZHr5+KI/qqPqj6g56oJhYW07OAN///tv5Z01y00UtJGQVu' &&
'YALTEnJJguMGIUIGINmLxAJBtp1P5duuhwXk4mKpJR0GzbCMk361oJGgWWTr5K54niy9L70ojIkZItc95ZLa8gCvBpTpBpiK4oYrzRgAbgBhEw6aMS7tP8uSvmJ2/OvFdGntsd6W2pzH5tjWw9UKTHyp5+8HpVbqaPuvNHj4MoYHsxq6AaBKHspC3GGKhhhVlXYwpOuhb85T1Z/g3mFj' &&
'Bappfj7x6ubsesCgbU8SSqCPC7MVmX238xerj0y6VhYTxUbAqYaWZ6pqbyKiIfE1KRfll20YppO+txewQp6IZN3yklpIUmHOQFP5mlRBTWccWB433r3z+SDYegTp16gga0jzE3VJcoPyn807pyLxM0MAUmT5g6x6bLhr2NUlVRoXVAUKivFhDs21OXqebMJRsxGToZmRCcuDoYvaX0BW' &&
'VCrkb3yM0A7ccUSOl3H0hunck4YhtUMvrpa0jWz6TYMml3eIdUF62XHBjhjBsmBxdZ3PyO393+H1dFR735uKaLF8Convnxg8H5nTMkgvX5bF67A7rfR528b/AI3Zpge6znjZpEeRefnrjh8TfeiZyG+ahQZAhvcD7FRQddMYTCDh1wqoy84nyt6GpFgo1yHvljRAhdkOAmfSzmTNzTIy' &&
'CEu+a8rkIJxHJOxs1jY8AKawitRWfwbMGUe2Tt87+UpdNuk81vPKr7VqSMxUzQW1YaE6EMzIho4GAMUjdud0x89j9M6pm/V9NLHl3ybkQbXoTAXxQG9/CYMUQi77sSTelrJEf7u/Ise/Ilzi7CcBU5o5M89+f3Na2m+ut2i87lmgdP/rhiIokZV3wyZqek30VSd8IGoJz51oIkc7x2TL' &&
'qKDWRmyZKVn2gUVQfBeRb0g0+HWgiHkaPjNz9tm3Ao/x2oPmLmeM2BkZCFRlhbW4wxVEgaHPdDQ24Pi2zOEB3cymUhvl99xnlqVF7QsFjfffJs6IqdG9WvcGuCaa4zaGeEDoya0xfN01VUwkxjDMBeLh/XQnQJOl9Z+NE2PAEPCe8/b+o4Pb3AlSWjdFagEeAKF+tNH3ejHqZGs+AEJ6' &&
'eozwWJtO563lBPyRCqWJbIWn43ZtAaMEpzFZaHtf8eRs/oy01rDp2tB1d0j4aggqBvXe73gfkxut2hC09FR+Txnw3Xhac0KJWbT4aBe3s2t2f/3JpgxiD53Jqw4EpndjeZumiVzsk46puvuVzbDwGUox4XmXgo2vhb1uQxQK5kApoJeAuucTFCzZWFQrWRPIgQX2OTMoK/qJATLu+jR8' &&
'70NA/7tx576qurdJGLm+ZslsrCaN/8CjaoC5BiQzUpiQp1CH8dJDXCsCxobNHA63W6T4uuUa6xK5feNNGB12gkNCousmRAh2bcPL7JKRlnWPzk1sTNy2bJESmVXnZrgkboIh/r8DsLDXZ13T7d0Fcb4OEVC5UJmU3Pz0gl6TAY/z55Zdw10oW02oWDYMSKABXehdWu8Y1mQeLQHv2Nns' &&
'wHPP2KHSV6SoZvQOjWRPOiahk+pHlIM0m4i0psj/Tpim6rmvVWojL7dM+OG+j6NgAwtG+6XXiyTNKFJxuddFxWMHiOffMX35a4DRnDsLO7gyh6X9Cnp4VQA/NBtyjFlV3NFsrhzZkp8D7TQaSrY+YtkeXrd0gpshGm003Vi32zUDh8zvROB8kSchMhxXHQcUCeHIk6gXYYTS3ntQCVC+' &&
'6Y52bvnv6cWbFUOZk+zCH4ZsAFLjgjrguE7f8F4eV3BFq41RRNjceloC1B3+RC4cplz9r0qEbI9JHzwUfeWijfNZTD6abrOVBvZPOCJ2VohH9avhQmKHDUZ+t8MPKomuvLbei/BQOv8rBIE1AK7eH5izVS8ZWgKT/tL9efe6okgmgqAQ8Oq7eNmtrVQTQN1mPW6uHdGbdfpK/86HyIB5' &&
'ZZUnN1T3E7DJ0kRqZxQWM9CjjBN0ImcWa/UhGVtuaka+ryLChpIQPfW/+1BM9mwnM+9+j95v0ubZ79sVAN3VvoZGxIkPrqCeaIfCXrmal36tsWOrdimkkjw+fW0kRN8dkkswWJK0H7qMMTIS5NRCp6y68XyXNLPw6mowbuhI4DaNM3sc1eH1dZoyt/U3ALSQ8SjL0ZTsAsYtiBwm7bSV' &&
'5hjO65rtbDBD49VlXlxqpRNwRzPpdJsPmTD7xLvnLbSA1a0YLxthFJWd2kOgZBogXQKKsaMiSeh/AD5D/mtmCN2++L3KB3qSijFvf7xr76a/Hj0nmeRRYXzQh+ujcoZm5/R/4NRsXDKbddNiLsGJwXNFga+B/fW6anZJwOKmco7OWbTG7Nd7Luv5XvyZkUjYailXhQl6lPE/CuZTA8Jk' &&
'8ujr9hiIkC9K7B/TGIJzVDFyS40hY0wAQXDfkbpb5KUnTnKjpYy/ztFQAK746SNQFpRbOGZZEwffMsKO9zy+Gh+fZspv+gvIIozrckVIFMVSZaqmRU+KbqRBpioaQcS70++/zEkbDsIlTnk7aR9E6y8N21GuG44c23RwYkwwDKbMbABSyeToGzmvjyGrnk7kdlAWTD7GR/XUDTF+OATK' &&
'lAs/sbArrHx/R2ETKMCXxX8at8GBK9NuhXYk0cJVfpUAkeJOCB7wX3/0SkYLtxBEzFuUAVnyxbA6lyxYOPyT88Ok8PjjMt3Y+oSIOkNOkumLzSSXDhjjS8jXHNxVz31ofm6vOafus80+s4POAilW5bkUgTZZRT3s36SMSkSmz7TlJGOqOAxleOpJNGmt75AlxJomWphvGJc+wpGbc1ET' &&
'lfo5GsObxLVy4Jt98XGTWdMbqoyUWWbFShgXuPtbl5Iw2Wn5xrcmuCL3UTMau+O9B4xfjZ5n20cr+sfeoB8/6Y3j5WNMq6/GLz4q6eN4QgCHKhqEgmDe+rL/B+A8UayNeFOvfEPRIEQZTly9JJ18lVmN8dHw0AIhjnUL+DEk98fTUiPOa27ugX0r99L/5C39YeM/NpWfS1iQQjz8yQp6' &&
'fco8JidDUrva0D2c65EueLTCGn83Wj97fqQXUDMIBbNEV79FUoGiH3M+e/ukxmL14H24XStqfi2pFTOZGR6MuwSQkyqGs7aa/vaIaw83ClnhKSRiTLqGNO2FCZ0TCfpxPkviO8LvvkCSY6SToSGhxXwHmKiI4xjy9G88A9ozajsjsqVswcQaRPlk+yMtPCaChuiJW8nXiONp6chsJsCX' &&
'LkyShdEcaYuQJdUa2r1bNGD1dz/ClfR/qe7eNBnkdFBsA3QT5/6j7lPRFV/gwMMfvFv/0/JKbkS0lDdkGj884FiwI9JL3XYEnKvhC/rPw0csMQQQ+dhvcFXi9oWDQ2GihTUG418HD1TfN+paulbm7nwLqMkDx188ytM/Wk2Av2XUPOK50Bep9hmjpr4G36rmFs9pkXPpy3AwysrpPH7x' &&
'wi15/XR/e2JObouXXzwKQ2tZ2celInWbrqI7QNRteAAVUQCPznmg9WyPln9Ze09HR57rmlEADuVyFaVEM5kRINufBMOat7Z+O8jwcxjBQiq7/+VlZt+BJ9IPSzj0ooVEWRjL99qKxY+a5Mf/ZtGAkVt1yWzJkiGWAiYxnNry00ULHg58BszJ/AvzMG9JWVKz8QfxH5i36rDqOYxZfNmz' &&
'bIDX93qWSir8H9+8j1l54tPdoHZP/O7ah/CPUZZ4AkKHJqJsaSIPsqY2UHmtlRnogSr8UfgMLzlatEUMw0lgci+KoP+uqT0ih3XNxX7vj7c2XaPw6XkzKSodwNEkNlbIzVORtjdyfIaviQC2To+WcgKy6QL3bvBX9smsR5YCIMKzFF+zI0mL5Jy75K3kf/7VDioGRVoJtnb2vg0FAGdU' &&
'2TMT8+S+6+frAMHXy2pCPjoat97d/flx17wYsq0MqFINCdlVgnP7v2Mr1PvkflvTrQTEnO6S51viTxVxfDtmp0bZ2lJuNcye51NdJPzLFjPMYFXWCJgaNvFxOQbj27yzt5H0lpXS38G+TlCuygpqZWkutjJf+rnXLVoCskJ7G95HTKkrzP15n6qOMK63Izfk/BIUmtrJaBvc+S07v3lg' &&
'O7tsmGI/tMPbTbHsNMKG/QwrY//+JLGf6jIRKz9fCBxtI67mSJ9EbkS1dPwQk5VfDYwflKOdSRrwZ5UWPtOgcJ0UmYb3GTmnDXIQPzEq7R5uOESaO3+f1SqLt1BuyD4Hj5Nz84fvafCSU+JTUF6TcVz6w+RvXCzYD1TdQ0L+NyTsZXqQ76TQTxorK8TM49tXvw1A7fnuY8Czot34Mn/G' &&
'NMO3ft1DfNt+wvkCNFZSbSedCvcxc9v8kjZFkdU6RXbid9c55vzHfJytSUiCro2Gi2KyhfIkFlZFZ3OUouohiHxai897s9svbb7VJQWiFbthbKjv0HwvvHc32yszQiZiHy9emWie+p0qVzjv6hqE4d0yUHvOR7HuzfzdW1P0wd+CcpyBcev+Mfn3I4LxfRlwt/XGxTU4wE6gb/rCQDRY' &&
'EEKkPRz5cCZ4V5Xw0ykPDYFoLTzUJkBYWVfns1hIpYH+QSkMLCYhlwUn/pit/kzU7oypEEcig6+AZFb64hoP39CDR7y7wr3U3B+nDWFXSCyhSd7zX7NnvbQFE7G6JvMuArK3Garej2hoKWZ5SC/xqvhyf1x/E5AUNBUMPtJ8FNbM4frQJC2JxTGVoBPqI84I+28sGwM/w5XuPv0D3yhz' &&
'BncdkXB4yiysdxh3b3DHecOzEtkWv8TXpZjLLR8C3Z2pdtj4B87RcUtMCU3/tnMvRBG3Ps/J8rv+7ABGUWUnNnVOYxp+imB9OvocO17UZLoJb24yg1fGI/upClDgGIaoBsG1kEj1rYk05hCOpty38KgtQEt6vc3I4Dsgt5NDaCzzNn02BAeOoEEc1OWB+0sB2lwfuMrU9XxolZTJ39kx' &&
'TOc5vk6/jhVuz0DCTAf90f9uKKFFmvf5iIiqcnMsxpHMIt0DTHwNbCKbD5I0QoqnAomKMw/Qo6Al3KB2NIlBMIX3PR+oaWVoGKq0fZwumnSvGKU0iOz/2Zd/Meo+maiux44KBJMPfZuMxvFVJNU40kFNVDCCm7vh+I9tmidxSGLly1r0fxaB3NgUatMvIqj+fhkOKyNvnITwc7Wq7M4n' &&
'nOtZR28hO/3V+/ZnNsx/tnO7jXyD8YxfscW41tK2oEC8rSjlNlZxft1MlgFEqe6bfZQOIxNkdTCq/hueCfkuANC+osOU7rUD7ZusE+LF36NjwQzGwsLyPB29xlaNvfBf2/CsvsZoXxVwP7+e/uw6E1fR0jPeSXU6oWecb2if+pMXvh+nag1VH5j40W41wioAHCGBQRtc5x4v+HAZ7ACf' &&
'wvRci8T+AETuB/HCcM8ARO4G8Gkf8CO1XW6Qr6nvUAAAAASUVORK5CYII='.

    R_HTML =
  '<!DOCTYPE HTML>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '<HTML>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '<HEAD>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '<META NAME="escolha_processo" content="width=device-width, initial-scale=1">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '<script type="text/javascript" src="https://platform.linkedin.com/badges/js/profile.js" async defer></script>' &&

  '<style>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  body {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    font-family: Verdana;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    color: black;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    background-color: MintCream;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  .card {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    transition: transform 0.5s;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    width: 200px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  .card:hover {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    transform: scale(1.5); ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  a {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '     text-decoration:none;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  .container {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    color: black;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  .split {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    height: 80%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    width: 50%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    position: fixed;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    z-index: 4;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    top: 0;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    overflow-x: hidden;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    overflow-y: hidden;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    padding-top: 20px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  .centered-bottom {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    position: absolute;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    bottom: 80px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    left: 50%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    transform: translate(-50%, -50%);' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  .left {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    left: 0;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  .right {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    right: 0;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  .centered {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    position: absolute;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    top: 50%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    left: 50%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    transform: translate(-50%, -50%);' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    text-align: center;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  .centered img {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    width: 150px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    border-radius: 45%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '</style>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '</head>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '<body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  ' ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  '  <div class="centered-bottom container">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '     <img src="' && IMAGEM_AMAGGI && '" style="width:100%">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  "'      <div style="width:100%" class="LI-profile-badge"  data-version="v1" data-size="medium" data-locale="pt_BR" data-type="horizontal" ' &&
  "'      data-theme="light" data-vanity="marcus-bárbara-8379ab31"><a class="LI-simple-link"  ' &&
  "' href=https://br.linkedin.com/in/marcus-b%C3%A1rbara-8379ab31?trk=profile-badge>Marcus Bárbara</a></div> ' &&
  '  </div>     ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '    ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '</body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '</html>'.

  ENDMETHOD.


  METHOD GET_SALDO_MB5B.

    TYPES: BEGIN OF T_L,
             LIKE(200),
           END OF T_L.

    DATA: TLX      TYPE TABLE OF T_L,
          T_LST    TYPE TABLE OF ABAPLIST,
          V_MEINS  TYPE MEINS,
          V_STOCKX TYPE C LENGTH 22,
          RETURN   TYPE BAPIRET2.

    SUBMIT RM07MLBD  WITH DATUM    EQ DATUM
                     WITH MATNR    EQ MATNR
                     WITH WERKS    EQ WERKS
                     WITH LGORT    EQ LGORT
                     WITH XSUM     EQ ABAP_FALSE
                     WITH LGBST    EQ ABAP_TRUE
                     WITH BWBST    EQ ABAP_FALSE
                     WITH PA_WDZER EQ ABAP_TRUE
                     WITH PA_WDZEW EQ ABAP_TRUE
                     WITH PA_WDWIZ EQ ABAP_TRUE
                     WITH PA_WDWUW EQ ABAP_TRUE
                     WITH PA_WDWEW EQ ABAP_TRUE
                     WITH PA_NDZER EQ ABAP_TRUE
                     WITH PA_NDSTO EQ ABAP_TRUE

     EXPORTING LIST TO MEMORY AND RETURN.

    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        LISTOBJECT = T_LST
      EXCEPTIONS
        NOT_FOUND  = 1
        OTHERS     = 2.

    IF SY-SUBRC IS INITIAL.

      CALL FUNCTION 'LIST_TO_ASCI'
        TABLES
          LISTASCI           = TLX
          LISTOBJECT         = T_LST
        EXCEPTIONS
          EMPTY_LIST         = 1
          LIST_INDEX_INVALID = 2
          OTHERS             = 3.

*     "//Pega linha 8 (saldo e UMB)
      LOOP AT TLX INTO DATA(_LX) FROM 8 TO 8.

        SHIFT _LX-LIKE+23(25) LEFT DELETING LEADING ' '.
        SHIFT _LX-LIKE+48(5) LEFT DELETING LEADING ' '.
        MOVE  _LX-LIKE+23(25) TO V_STOCKX.

*        TRANSLATE V_STOCKX USING '. '.
        CONDENSE  V_STOCKX NO-GAPS.
*        TRANSLATE V_STOCKX USING ',.'.

*       "//Unidade de medida
        MOVE _LX-LIKE+48(5) TO V_MEINS.

*       "//Converte unidade de medida de M3 para L
        IF V_MEINS = 'M3'.
          V_STOCKX = V_STOCKX * 1000.
        ENDIF.

*       "//Saldo na data de lançamento
        MOVE V_STOCKX TO SALDO.

      ENDLOOP.

*     "//Eliminar dados da memória
      CALL FUNCTION 'LIST_FREE_MEMORY'
        TABLES
          LISTOBJECT = T_LST.

    ENDIF.

  ENDMETHOD.


  METHOD GET_STRUCTURE_DESCRIPTION.
    DATA STR TYPE REF TO DATA.

    ASSIGN STRUCTURE TO FIELD-SYMBOL(<FS_STR>).
    CREATE DATA STR TYPE (<FS_STR>).

    R_TABLE = CORRESPONDING #(
                              CL_SALV_DATA_DESCR=>READ_STRUCTDESCR(
                                CAST CL_ABAP_STRUCTDESCR(
                                     CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA_REF( STR )
                                                ) )
                             ).
  ENDMETHOD.


  METHOD MONTA_CHAVE_NFE.
************************************
*  Método de montagem da chave da nota fiscal.
*  Parâmetro: I_DOCNUM
*  Descrição: Este método é responsavel por montar a chave da nota fiscal eletronica, sendo ela de 44 caracteres.
*  18.02.2014 09:25:48
************************************

    DATA: TL_J_1BNFE_ACTIVE TYPE J_1BNFE_ACTIVE, "Tabela responsavel por guardar as informações da CHAVE ATIVA.
          TL_KNA1           TYPE KNA1,           "Tabela Mestre de clientes (parte geral)
          TL_LFA1           TYPE LFA1,           "Tabela Mestre de fornecedores (parte geral)
          VAR_CNPJ          TYPE STCD1,          " Nº ID fiscal 1 (CNPJ).
          VAR_CHAVE         TYPE CHAR44,         "Variavel para Validação da Chave.
          VAR_TAM           TYPE SY-TABIX,       "Tamanho da Chave.
          VAR_ERRO          TYPE C.              "Variavel de Erro C = Cliente / F = Fornecedor.


    "Verifica se o docnum esta realmente preenchido.
    IF NOT ( I_DOCNUM IS INITIAL ).

      "Selecionar Informações da nota.
      SELECT SINGLE * FROM J_1BNFE_ACTIVE INTO TL_J_1BNFE_ACTIVE WHERE DOCNUM EQ I_DOCNUM.

      IF TL_J_1BNFE_ACTIVE-SERIE GE '890' AND TL_J_1BNFE_ACTIVE-SERIE LE '899' AND TL_J_1BNFE_ACTIVE-MODEL EQ '55'.

        SELECT SINGLE * INTO @DATA(WA_ZIB_NFE_FORN)
          FROM ZIB_NFE_FORN
         WHERE NU_CHAVE_MODELO  EQ @TL_J_1BNFE_ACTIVE-MODEL
           AND NU_CHAVE_SERIE   EQ @TL_J_1BNFE_ACTIVE-SERIE
           AND NU_CHAVE_NUMERO  EQ @TL_J_1BNFE_ACTIVE-NFNUM9
           AND NU_CHAVE_REGIAO  EQ @TL_J_1BNFE_ACTIVE-REGIO
           AND NU_CHAVE_ANO     EQ @TL_J_1BNFE_ACTIVE-NFYEAR
           AND NU_CHAVE_MES     EQ @TL_J_1BNFE_ACTIVE-NFMONTH
           AND NU_CHAVE_ALEATOR EQ @TL_J_1BNFE_ACTIVE-DOCNUM9
           AND NU_CHAVE_DV      EQ @TL_J_1BNFE_ACTIVE-CDV.

        IF SY-SUBRC IS INITIAL.
          TL_J_1BNFE_ACTIVE-STCD1 =  WA_ZIB_NFE_FORN-NU_CHAVE+6(14).
        ELSE.
          MESSAGE E899(FI) WITH 'Chave NF-e Avulsa não encontrado (Séries 890 à 899)!' VAR_TAM DISPLAY LIKE 'W' RAISING ERRO_CHAVE.
        ENDIF.

      ENDIF.

      " sy-subrc  = 0 (Retorno de sucesso, documento existente)
      " sy-subrc != 0 (documento não existe).
      CASE SY-SUBRC.
        WHEN: 0.

          CLEAR: VAR_CHAVE, "Limpar a Variavel da Chave.
                 VAR_TAM, "Limpar Variavel do Tamanho da Chave.
                 VAR_CNPJ. "Limpar Variavel do CNPJ

          "Monta a CHAVE com 44 caracteres.
          CONCATENATE TL_J_1BNFE_ACTIVE-REGIO   "Região do emissor NF-e
                      TL_J_1BNFE_ACTIVE-NFYEAR  "Ano da data do documento da NF-e
                      TL_J_1BNFE_ACTIVE-NFMONTH "Mês da data do documento da NF-e
                      TL_J_1BNFE_ACTIVE-STCD1   "Nº CNPJ do emissor da NF-e
                      TL_J_1BNFE_ACTIVE-MODEL   "Modelo da nota fiscal
                      TL_J_1BNFE_ACTIVE-SERIE   "SERIE
                      TL_J_1BNFE_ACTIVE-NFNUM9  "Nº NF-e de nove posições
                      TL_J_1BNFE_ACTIVE-DOCNUM9 "NF-e: nº aleatório
                      TL_J_1BNFE_ACTIVE-CDV     "Dígito controle p/chave de acesso NF-e
          INTO VAR_CHAVE.

          VAR_TAM = STRLEN( VAR_CHAVE ). "Captura o Tamanho da Chave da Nota Fiscal.

          IF ( VAR_TAM < 44 ). "Verifica se a chave é menor que 44 caracteres.

            MESSAGE E899(FI) WITH 'Chave' VAR_CHAVE 'menor que 44 caracteres. TAM: ' VAR_TAM DISPLAY LIKE 'W' RAISING ERRO_CHAVE.

          ELSE. "Caso seja de 44 caracteres retornar para o usuário.


            IF ( I_VALIDA IS INITIAL ). "Caso esse parametro esteja com atribuição X não validar o CNPJ.
              "Validar se o CNPJ da CHAVE é intercompany.
              VAR_CNPJ = VAR_CHAVE+6(14).

              SELECT SINGLE * FROM KNA1 INTO TL_KNA1 WHERE STCD1 EQ VAR_CNPJ. "Seleciona o CNPJ da CHAVE na tabela de Cliente e verifica sua existencia.

              IF ( SY-SUBRC NE 0 ).
                MESSAGE E899(FI) WITH 'CNPJ' VAR_CNPJ 'não é um cliente Intercompany.' VAR_TAM DISPLAY LIKE 'W' RAISING ERRO_CHAVE.

              ELSE.

                SELECT SINGLE * FROM LFA1 INTO TL_LFA1 WHERE STCD1 EQ VAR_CNPJ. "Seleciona o CNPJ da CHAVE na tabela de Fornecedor e verifica sua existencia.

                IF ( SY-SUBRC NE 0 ).
                  MESSAGE E899(FI) WITH 'CNPJ' VAR_CNPJ 'não é um fornecedor Intercompany.' VAR_TAM DISPLAY LIKE 'W' RAISING ERRO_CHAVE.
                ELSE.
                  E_CHAVE = VAR_CHAVE. "Retorno do valor da chave já montada.
                ENDIF.

              ENDIF.
            ELSE.
              E_CHAVE = VAR_CHAVE. "Retorno do valor da chave já montada.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
          MESSAGE E899(FI) WITH 'Documento' I_DOCNUM 'não encontrado.' DISPLAY LIKE 'W' RAISING ERRO_CHAVE.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD PING_TIPCARD.
*&--------------------------------------------------------------------&*
*&                         Grupo André Maggi
*&--------------------------------------------------------------------&*
*& Projeto..: Tipcard/Neus
*& Data.....: 31.01.2014 14:43:16
*& Descrição: Monitoramento para Solicitação de Viagem TipCard.
*&--------------------------------------------------------------------&*
*
*
*  DATA: GW_ZPARAMETROS TYPE ZPARAMETROS. "Parametros para o caminho no servidor.
*
*  DATA: VL_CAMINHO TYPE STRING,
*        VL_ARQUIVO TYPE STRING.
*
*  DATA: VL_DATA     TYPE C LENGTH 10, "Variável de Data
*        VL_HORA     TYPE C LENGTH 8. "Variavel da Hora
*
*  TYPES: BEGIN OF TY_TEXT,
*           TEXTO TYPE ZXML,
*         END OF TY_TEXT.
*
*  DATA: GT_TEXT  TYPE TABLE OF TY_TEXT,
*        GW_TEXT  TYPE TY_TEXT,
*        GW_T100 TYPE T100.
*
*  DATA: GW_PARAMETROS      TYPE  BTCXPGSTIM-PARAMS,
*        GW_STRTSTAT        TYPE  BTCXPGSTAT,
*        GW_XPGID           TYPE  BTCXPGID,
*        GW_CONVID          TYPE  GWY_STRUCT-CONVID,
*        GW_EXITSTAT        TYPE  BTCXPGSTAT,
*        GW_EXITCODE        TYPE  BTCXPGEXIT,
*        GW_LAST_PROC       TYPE  WPINFO-WP_PID,
*        GW_LAST_HOST       TYPE  RFCHOST,
*        GW_LAST_PROC_NUM   TYPE  WPINFO-WP_NO,
*        GT_LOG             TYPE TABLE OF BTCXPGLOG,
*        GW_LOG             TYPE BTCXPGLOG.
*
*
*  TYPES: BEGIN OF TY_ZLOG0001,
*          MANDT      TYPE ZLOG0001-MANDT,
*          HORA       TYPE ZLOG0001-HORA,
*          ARQUIVO    TYPE ZLOG0001-ARQUIVO,
*          TIPO       TYPE ZLOG0001-TIPO,
*          USUARIO    TYPE ZLOG0001-USUARIO,
*          PROGRAMA   TYPE ZLOG0001-PROGRAMA,
*          DATA       TYPE ZLOG0001-DATA,
*          MSGNR      TYPE ZLOG0001-MSGNR,
*          IP_ADDRESS TYPE ZLOG0001-IP_ADDRESS,
*          DEPARTMENT TYPE ZLOG0001-DEPARTMENT,
*          URL        TYPE ZLOG0001-URL,
*         END OF TY_ZLOG0001.
*
*
*  DATA: GW_ZLOG0001   TYPE TY_ZLOG0001,
*        GW_IP_ADDRESS TYPE CHAR69,
*        GW_USER_ADDR  TYPE USER_ADDR.
*
*
*  REFRESH: GT_TEXT[], GT_LOG[].
*
*  CLEAR: GW_TEXT.
*
*  CLEAR: VL_DATA,
*         VL_HORA,
*         VL_CAMINHO,
*         VL_ARQUIVO.
*
*
*  TYPES: BEGIN OF KERNEL_VERSION,
*           KEY(21) TYPE C,
*           DATA(69) TYPE C,
*        END OF KERNEL_VERSION.
*
*  DATA : GT_KERNEL_VERSION TYPE STANDARD TABLE OF KERNEL_VERSION,
*         GW_KERNEL_VERSION TYPE KERNEL_VERSION.
*
*  "Hora do Arquivo
*  CONCATENATE SY-UZEIT(2) ':' SY-UZEIT+2(2) ':' SY-UZEIT+4(2) INTO VL_HORA. "Monta a Hora Formato 00:00:00
*
*  "Método para Converter a Data para BRL
*  ME->CONV_DATA_US_BR( EXPORTING I_DATA  = SY-DATUM
*                                 I_OPCAO = '-'
*                       RECEIVING E_DATA = VL_DATA ).
*
*  "Selecionar o caminho para criar o arquivo no servidor
*  "Arquivo de Solicitação
*  SELECT SINGLE * FROM ZPARAMETROS INTO GW_ZPARAMETROS WHERE NOME_PARAMETRO EQ 'INPUT_REQUEST_TIP'
*                                                         AND STATUS EQ 'A'.
*
*  IF ( SY-SUBRC EQ 0 ). "Se encontrar o caminho parametrizado.
*
*    GW_TEXT-TEXTO = '----------------------------------------------------------------'.
*    APPEND GW_TEXT TO GT_TEXT.
*
*    CONCATENATE 'ENDEREÇO: ' I_URL INTO GW_TEXT-TEXTO SEPARATED BY SPACE.
*    APPEND GW_TEXT TO GT_TEXT.
*    CONCATENATE 'TIPO: ' I_TIPO INTO GW_TEXT-TEXTO SEPARATED BY SPACE.
*    APPEND GW_TEXT TO GT_TEXT.
*
*    CONCATENATE 'CÓDIGO: ' I_COD INTO GW_TEXT-TEXTO SEPARATED BY SPACE.
*
*    SELECT SINGLE * FROM T100 INTO GW_T100 WHERE MSGNR EQ I_COD+1(3)
*                                             AND SPRSL  EQ 'PT'
*                                             AND ARBGB  EQ 'ZSIMETRYA'. "Seleciona o texto do código de erro
*    CONCATENATE 'MENSAGEM: ' GW_T100-TEXT INTO GW_TEXT-TEXTO SEPARATED BY SPACE.
*    APPEND GW_TEXT TO GT_TEXT.
*
*
*    CASE I_TIPO.
*      WHEN: 'REQU'.
*        CONCATENATE GW_ZPARAMETROS-VALOR 'REQU-DT' VL_DATA '-HR' SY-UZEIT '.log' INTO VL_CAMINHO.
*        CONCATENATE 'REQU-DT' VL_DATA '-HR' SY-UZEIT INTO VL_ARQUIVO.
*      WHEN: 'RESP'.
*        CONCATENATE GW_ZPARAMETROS-VALOR 'RESP-DT' VL_DATA '-HR' SY-UZEIT '.log' INTO VL_CAMINHO.
*        CONCATENATE 'RESP-DT' VL_DATA '-HR' SY-UZEIT INTO VL_ARQUIVO.
*    ENDCASE.
*
*
*    CONCATENATE 'DATA: ' VL_DATA INTO GW_TEXT-TEXTO SEPARATED BY SPACE.
*    APPEND GW_TEXT TO GT_TEXT.
*
*    CONCATENATE 'HORA: ' VL_HORA INTO GW_TEXT-TEXTO SEPARATED BY SPACE.
*    APPEND GW_TEXT TO GT_TEXT.
*
*    "Caso o Arquivo seja gravado executar o ping no servidor
*    IF ( SY-SUBRC EQ 0 ).
*
*      CONCATENATE '-c 5' I_URL+8(15) INTO GW_PARAMETROS SEPARATED BY SPACE.
*
*      CALL FUNCTION 'SXPG_STEP_XPG_START'
*        EXPORTING
*          COMMANDNAME     = 'ZPING_TIPCARD'
*          OPERATINGSYSTEM = 'Linux'
*          PARAMS          = GW_PARAMETROS
*          STDINCNTL       = 'R'
*          STDOUTCNTL      = 'M'
*          STDERRCNTL      = 'M'
*          TRACECNTL       = '0'
*          TERMCNTL        = 'C'
*          TRACELEVEL      = '0'
*          CONNCNTL        = 'H'
*        IMPORTING
*          STRTSTAT        = GW_STRTSTAT
*          XPGID           = GW_XPGID
*          CONVID          = GW_CONVID
*          EXITSTAT        = GW_EXITSTAT
*          EXITCODE        = GW_EXITCODE
*          LAST_PROC       = GW_LAST_PROC
*          LAST_HOST       = GW_LAST_HOST
*          LAST_PROC_NUM   = GW_LAST_PROC_NUM
*        TABLES
*          LOG             = GT_LOG.
*
*      IF NOT ( GT_LOG[] IS INITIAL ).
*
*        LOOP AT GT_LOG INTO GW_LOG.
*          GW_TEXT-TEXTO = GW_LOG-MESSAGE.
*          APPEND GW_TEXT TO GT_TEXT.
*
*          CLEAR: GW_TEXT, GW_LOG.
*        ENDLOOP.
*
*        GW_TEXT-TEXTO = '----------------------------------------------------------------'.
*        APPEND GW_TEXT TO GT_TEXT.
*
*        "Capturar Qual é o IP que esta enviando a solicitação
*        CALL 'SAPCORE' ID 'ID' FIELD 'VERSION'
*                       ID 'TABLE' FIELD GT_KERNEL_VERSION[].
*
*        READ TABLE GT_KERNEL_VERSION INTO GW_KERNEL_VERSION INDEX 11.
*        GW_IP_ADDRESS = GW_KERNEL_VERSION-DATA.
*
*        SELECT SINGLE * FROM USER_ADDR
*          INTO GW_USER_ADDR
*          WHERE BNAME EQ SY-UNAME.
*
*        GW_ZLOG0001-MANDT      = SY-MANDT.
*        GW_ZLOG0001-HORA       = SY-TIMLO.
*        GW_ZLOG0001-ARQUIVO    = VL_ARQUIVO.
*        GW_ZLOG0001-TIPO       = I_TIPO.
*        GW_ZLOG0001-USUARIO    = SY-UNAME.
*        GW_ZLOG0001-PROGRAMA   = SY-CPROG.
*        GW_ZLOG0001-DATA       = SY-DATLO.
*        GW_ZLOG0001-MSGNR      = I_COD.
*        GW_ZLOG0001-IP_ADDRESS = GW_IP_ADDRESS.
*        GW_ZLOG0001-DEPARTMENT = GW_USER_ADDR-DEPARTMENT.
*        GW_ZLOG0001-URL        = I_URL.
*
*        IF NOT ( GW_ZLOG0001 IS INITIAL ).
*
*          "Gravar na tabela de Log
*          INSERT INTO ZLOG0001 VALUES GW_ZLOG0001.
*          COMMIT WORK.
*
*          CALL FUNCTION 'GUI_DOWNLOAD'
*            EXPORTING
*              FILENAME = VL_CAMINHO
*            TABLES
*              DATA_TAB = GT_TEXT.
*
*
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  ENDMETHOD.


  METHOD get_docnum.

    DATA: l_campos     TYPE zde_campos_nfe.

    FREE: t_documentos.

    CHECK i_chave_nfe IS NOT INITIAL.

    l_campos = me->get_atributos_nfe( i_chave_nfe ).

    SELECT *
      INTO TABLE t_documentos
      FROM j_1bnfe_active
     WHERE regio    = l_campos-regio
       AND nfyear   = l_campos-nfyear
       AND nfmonth  = l_campos-nfmonth
       AND stcd1    = l_campos-stcd1
       AND model    = l_campos-model
       AND serie    = l_campos-serie
       AND nfnum9   = l_campos-nfnum9
       AND docnum9  = l_campos-docnum9
       AND cdv      = l_campos-cdv.

  ENDMETHOD.


  METHOD get_string_numeric.

    DATA: lv_text   TYPE string,
          lv_amount TYPE string,
          lv_num    TYPE i.

    FREE: e_saida.

    CHECK i_string IS NOT INITIAL.

    lv_num  = strlen( i_string ).
    lv_text = i_string.

    DO lv_num TIMES.
      IF lv_text(1) CA '0123456789.,'.
        CONCATENATE lv_amount lv_text(1) INTO lv_amount.
        CONDENSE lv_amount NO-GAPS.
      ENDIF.
      SHIFT lv_text LEFT CIRCULAR.
    ENDDO.

    REPLACE ',' IN lv_amount WITH '.'.

    DO.
      FIND ALL OCCURRENCES OF '.' IN lv_amount MATCH COUNT lv_num.
      IF lv_num > 1.
        REPLACE FIRST OCCURRENCE OF '.' IN lv_amount WITH ''.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    CONDENSE lv_amount.

    e_saida = lv_amount.

  ENDMETHOD.


  METHOD GET_AJUDA_PESQUISA.

    DATA: LT_RETVAL TYPE HRRETURN_TAB,
          LS_SHLP   TYPE SHLP_DESCR.

    FIELD-SYMBOLS: <FS_INTERF> TYPE DDSHIFACE.

    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        SHLPNAME = I_AJUDA_PESQUISA
      IMPORTING
        SHLP     = LS_SHLP.

    READ TABLE LS_SHLP-INTERFACE ASSIGNING <FS_INTERF> INDEX 1.

    CHECK SY-SUBRC IS INITIAL.

    <FS_INTERF>-VALFIELD = ABAP_TRUE.

    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        SHLP          = LS_SHLP
      TABLES
        RETURN_VALUES = LT_RETVAL.


  ENDMETHOD.
ENDCLASS.
